/*
 * M68332 TPU block emulation
 *
 */
#include "qemu/osdep.h"
#include "qemu/log.h"
#include "hw/irq.h"
#include "hw/sysbus.h"
#include "qemu/log.h"
#include "qemu/module.h"
#include "qapi/error.h"
#include "hw/m68k/m68332.h"
#include "hw/qdev-properties.h"
#include "hw/qdev-properties-system.h"
#include "chardev/char-fe.h"
#include "qom/object.h"
#include "mcf_tpu.h"
#include <byteswap.h>

#define TYPE_MCF_TPU "mcf-tpu"

OBJECT_DECLARE_SIMPLE_TYPE(mcf_tpu_state, MCF_TPU);

static uint32_t calculate_pwm_delay(mcf_tpu_state *s, tpu_t *tp);
static uint16_t memory_read_16     (uint32_t addr);
static void     neg_x_timer_cb     (void *opaque);
static void     ticr_write         (mcf_tpu_state *s, uint32_t val);
static void     tpu_assert_irq     (mcf_tpu_state *s, uint32_t channel);
static void     tpu_dump_timer_cb  (void *opaque);
static void     tpu_maybe_start    (mcf_tpu_state *s, tpu_t *tp);

static const char *function_strs[] = {
   "str_0",  //
   "str_1",  //
   "str_2",  //
   "str_3",  //
   "str_4",  //
   "str_5",  //
   "QDEC",   //  6 - quadrature decoder
   "SPWM",   //  7 - synchronized PWM
   "DIO",    //  8 - discrete I/O
   "PWM",    //  9 - PWM
   "ITC",    // 10 -
   "PMA",    // 11 -
   "PSP",    // 12 -
   "SM",     // 13 -
   "OC",     // 14 - output compare
   "str_15", //
};
/*-------------------------------------------------------------------------
 *
 * name:        mcf_tpu_read
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static uint64_t mcf_tpu_read(void *opaque, hwaddr addr, unsigned size)
{
    mcf_tpu_state *s = opaque;

    (void) s;

    switch (addr & 0x3f) {

    case CFSR0:  return s->cfsr0;
    case CFSR1:  return s->cfsr1;
    case CFSR2:  return s->cfsr2;
    case CFSR3:  return s->cfsr3;
    case CIER:   return s->cier;
    case CISR:   return s->cisr;
    case CPR0:   return s->cpr0;
    case CPR1:   return s->cpr1;
    case HSQR0:  return s->hsqr0;
    case HSQR1:  return s->hsqr1;
    case HSRR0:  return s->hsrr0;
    case HSRR1:  return s->hsrr1;
    case TICR:   return s->ticr;
    case TPUMCR: return s->tpumcr;

    default:
       qemu_log_start_line("ERR");
       qemu_log("%s %d Read from unhandled offset: %lx size: %d \n", __func__, __LINE__, addr, size);
       exit(1);
    }

    return 0;
}

/*-------------------------------------------------------------------------
 *
 * name:        cfsr_update
 *
 * description: Update the channel function register
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void cfsr_update (mcf_tpu_state *s, uint16_t *cfsr_p, uint32_t first_channel, uint32_t val)
{

   uint32_t
      channel,
      function,
      i;
   tpu_t
      *tp;

   /*
    * Update register itself
    */
   *cfsr_p = val;

   /*
    * Check which fields are different
    */
   for (i=0; i < 4; i++) {

      channel = first_channel + i;

      tp = &s->tpus[channel];

      function = val & 0xf;

      tp->function = function;

      /*
       * Ready for next iteration
       */
      val >>= 4;
      }
}



/*-------------------------------------------------------------------------
 *
 * name:        cpr_update
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void cpr_update (mcf_tpu_state *s, uint16_t *cipr_p, uint32_t first_channel, uint32_t val)
{
   uint32_t
      channel,
      i;
   uint16_t
      old,
      old_pri,
      priority;
   tpu_t
      *tp;

   old = *cipr_p;

   /*
    * store new value
    */
   *cipr_p = val;

   /*
    * Check which fields are different
    */
   for (i=0; i< 8; i++) {

      old_pri  = (old & 3);
      priority = (val & 3);

      if (old_pri != priority) {

         channel = first_channel + i;

         tp = &s->tpus[channel];

         tp->priority = priority;

         qemu_log_start_line("TPU");

         if (priority) {
            tpu_maybe_start(s, tp);
         }
         else {
            qemu_log("  | STOPPED | channel: %d ", channel);
            tp->state = STATE_IDLE;
         }
         qemu_log("\n");
      }

      old >>= 2;
      val >>= 2;
   }

}

/*-------------------------------------------------------------------------
 *
 * name:        hsrr_update
 *
 * description: Update the Host Service Request code
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void hsrr_update (mcf_tpu_state *s, uint16_t *hsrr_p, uint32_t first_channel, uint32_t val)
{
   uint32_t
      channel,
      host_service_request,
      i;
   tpu_t
      *tp;

   /*
    * Update register itself
    */
   *hsrr_p = val;

   /*
    * Check which fields are different
    */
   for (i=0; i < 8; i++) {

      channel = first_channel + i;

      tp = &s->tpus[channel];

      host_service_request = val & 0x3;

      tp->host_service_request = host_service_request;

      /*
       * If request is non-zero, then the TPU may to run.
       */
      if (host_service_request) {
         tpu_maybe_start(s, tp);
      }

      /*
       * Ready for next iteration
       */
      val >>= 2;
      }
}




/*-------------------------------------------------------------------------
 *
 * name:        cier_update
 *
 * description: Update the Host Service Request code
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void cier_update (mcf_tpu_state *s, uint32_t cier)
{
   uint32_t
      i;
   tpu_t
      *tp;

   /*
    * Update register
    */
   s->cier = cier;

   /*
    * Roll through each of the TPUs
    */
   tp   = s->tpus;

   for (i=0; i< NUMBER_TPUS; i++, tp++) {

      tp->ier = (cier & 1) ? 1 : 0;

      cier >>= 1;
   } // for NUMBER_TPUS


}

/*-------------------------------------------------------------------------
 *
 * name:        cisr_update
 *
 * description: Update the Interrupt Status register.  If an interrupt
 *              is currently pending, and the new CISR value does not
 *              have the bit set, then clear the interrupt request.
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void cisr_update (mcf_tpu_state *s, uint32_t val)
{
   uint32_t
      cisr,
      i,
      mask;
   tpu_t
      *tp;

   /*
    * Roll through each of the TPUs
    */
   tp   = s->tpus;
   cisr = s->cisr;

   for (i=0; i< NUMBER_TPUS; i++, tp++) {

      mask = (1 << i);

      if (cisr & mask) {
         if ( (val & mask ) == 0) {

            qemu_log("\n%s %d clearing IRQ TPU channel: %d \n", __func__, __LINE__, i);

            qemu_set_irq(tp->irq, 0);
         }
      }
   } // for NUMBER_TPUS

   /*
    * Update register with new value.
    */
   s->cisr = val;
}


/*-------------------------------------------------------------------------
 *
 * name:        hsqr_update
 *
 * description: Update the Host Service Request code
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void hsqr_update (mcf_tpu_state *s, uint16_t *hsqr_p, uint32_t first_channel, uint32_t val)
{
   uint32_t
      channel,
      i;
   tpu_t
      *tp;

   /*
    * Update register itself
    */
   *hsqr_p = val;

   /*
    * Check which fields are different
    */
   for (i=0; i < 8; i++) {

      channel = first_channel + i;

      tp = &s->tpus[channel];

      tp->host_sequence_code = (val & 0x3);

      /*
       * Ready for next iteration
       */
      val >>= 2;
      }
}

/*-------------------------------------------------------------------------
 *
 * name:        mcf_tpu_write
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void mcf_tpu_write(void *opaque, hwaddr addr, uint64_t val, unsigned size)
{
   mcf_tpu_state
      *s = opaque;

    switch (addr & 0x3f) {

    case CFSR0:
       cfsr_update(s, &s->cfsr0, 12, val);
       break;

    case CFSR1:
       cfsr_update(s, &s->cfsr1, 8, val);
       break;

    case CFSR2:
       cfsr_update(s, &s->cfsr2, 4, val);
       break;

    case CFSR3:
       cfsr_update(s, &s->cfsr3, 0, val);
       break;

    case CIER:
       cier_update(s, val);
       break;

    case CISR:
       cisr_update(s, val);
       break;

    case CPR0:
       cpr_update(s, &s->cpr0, 8, val);
       break;

    case CPR1:
       cpr_update(s, &s->cpr1, 0, val);
       break;

    case HSQR0:
       hsqr_update(s, &s->hsqr0, 8, val);
       break;
    case HSQR1:
       hsqr_update(s, &s->hsqr1, 0, val);
       break;

    case HSRR0:
       hsrr_update(s, &s->hsrr0, 8, val);
       break;
    case HSRR1:
       hsrr_update(s, &s->hsrr1, 0, val);
       break;

       /*
        * TPU Interrupt Control Register
        */
    case TICR:
       s->ticr = val;
       ticr_write(s, val);
       break;

    case TPUMCR: //
       s->tpumcr = val;
       break;

    default:
       qemu_log_start_line("ERR");
       qemu_log("%s %d Write to unhandled offset: %lx\n", __func__, __LINE__, addr);
       break;
    }
}

/*-------------------------------------------------------------------------
 *
 * name:        mcf_tpu_reset
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void mcf_tpu_reset(DeviceState *dev)
{
    mcf_tpu_state *s = MCF_TPU(dev);

    (void) s;

//    s->spcr1 = 0x0404;
}

/*-------------------------------------------------------------------------
 *
 * name:        tpu_maybe_start
 *
 * description: A write to the CPRx register is requesting that this TPU
 *              run.
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void tpu_maybe_start (mcf_tpu_state *s, tpu_t *tp)
{
   /*
    * Seems we can change the parms to a TPU after it is running.  For example,
    * a DIO can change its output state after running.  So removed a check for
    * IDLE at this point.
    */

#define TPU_TIMER_INTERVAL_MS(x) (x * 1000 * 1000)

//   qemu_log("%s %d TPU channel: %d f: %d req: %d pri: %d \n", __func__, __LINE__, tp->channel, tp->function, tp->host_service_request, tp->priority);

   /*
    * If not a valid function, then we're stopped
    */
   if (tp->function == FUNCTION_IDLE) {
      qemu_log("%s %d TPU - leaving idle - zero function - channel: %d \n", __func__, __LINE__, tp->channel);
      return;
   }

   /*
    * We have to have a non-zero priority
    */
   if (tp->priority == 0) {
//      qemu_log("%s %d TPU - leaving idle - zero priority - channel: %d \n", __func__, __LINE__, tp->channel);
      tp->state = STATE_IDLE;
      return;
   }

   qemu_log("  | RUNNING  | channel: %d function: %d (%s) request: %d sequence: %x priority: %d ier: %d\n\n",
            tp->channel,
            tp->function, function_strs[tp->function],
            tp->host_service_request,
            tp->host_sequence_code,
            tp->priority,
            tp->ier);

   /*
    * Need to set timer for 5 uSec to simulate initialization and clear the HSRRx bits
    */
   qemu_log("\n%s %d [ TPU %d ] set timer for: %ld (5 uSec) \n", __func__, __LINE__, tp->channel, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + 5000);

   timer_mod(tp->timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + 5000);

   tp->state = STATE_INITIALIZING;
}

/*-------------------------------------------------------------------------
 *
 * name:        calculate_pwm_delay
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static uint32_t calculate_pwm_delay(mcf_tpu_state *s, tpu_t *tp)
{
   uint32_t
      nSecs,
      nSecs_per_tick,
      psck,
      pwm_per,
      tcr1p,
      tpumcr,
      tpu_ram_addr;

   tpumcr = s->tpumcr;

   /*
    * Pre-scaler value is bits 14:13
    */
   psck  = ((tpumcr & PSCK_MASK)  >> PSCK_SHIFT);
   tcr1p = ((tpumcr & TCR1P_MASK) >> TCR1P_SHIFT);

   /*
    * first order is 250, 500, 1000, or 2000, based on tcr1p value.
    */
   nSecs_per_tick = (250 * (1 << tcr1p));

   /*
    * Secondly, if PSCK is 0, then multiply by factor of 8.  Becomes
    * 2000, 4000, 8000, or 16000.
    */
   if (psck == 0) {
      nSecs_per_tick *= 8;
   }

   /*
    * Need to read the PWM period.
    */
   tpu_ram_addr = 0xffff00 + (tp->channel * 0x10);

   pwm_per = memory_read_16(tpu_ram_addr + 6);

   nSecs = (pwm_per * nSecs_per_tick);

   return nSecs;
}

/*-------------------------------------------------------------------------
 *
 * name:        tcr1_read
 *
 * description: Read content of the TCR1 register
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
uint32_t mcf_tpu_tcr1_read(void *opaque)
{
   uint32_t
      nSecs_per_tick,
      psck,
      tcr1,
      tcr1p,
      ticks,
      tpumcr;
   uint64_t
      nSecs;
   mcf_tpu_state
      *s = opaque;

   tpumcr = s->tpumcr;

   /*
    * Pre-scaler value is bits 14:13
    */
   psck  = ((tpumcr & PSCK_MASK)  >> PSCK_SHIFT);
   tcr1p = ((tpumcr & TCR1P_MASK) >> TCR1P_SHIFT);

   /*
    * first order is 250, 500, 1000, or 2000, based on tcr1p value.
    */
   nSecs_per_tick = (250 * (1 << tcr1p));

   /*
    * Secondly, if PSCK is 0, then multiply by factor of 8.  Becomes
    * 2000, 4000, 8000, or 16000.
    */
   if (psck == 0) {
      nSecs_per_tick *= 8;
   }

   /*
    * Need to read the current emulation time
    */
   nSecs = qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL);

   /*
    * Convert to ticks
    */
   ticks = nSecs / nSecs_per_tick;

   /*
    * Get modulo 16 bits value.
    */
   tcr1 = ticks & 0xffff;

   return tcr1;
}

/*-------------------------------------------------------------------------
 *
 * name:        tpu_timer_cb
 *
 * description: A tpu timeout has occurred.  Clear its HSRR bits and
 *              maybe set an interrupt.
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void tpu_timer_cb(void *opaque)
{
   uint16_t
      mask,
      *reg_p;
   uint32_t
      bit,
      nSecs,
      wrd;
   mcf_tpu_state
      *s;
   tpu_t
      *tp = opaque;

   s = tp->s;

//   qemu_log_start_line("\nTPU");
//   qemu_log("TIMER EXPIRED: channel: %d state: %d \n", tp->channel, tp->state);

   /*
    * If TPU is marked as stopped, then just return.  No need to re-start the timer.
    */
   if (tp->state == STATE_IDLE) {
      return;
   }

   /*
    * check if we're initializing
    */
   if (tp->state == STATE_INITIALIZING) {

      /*
       * The HSSRn register field needs to get cleared.  The firmware is
       * polling HSRR1 at line 12260 during initialization.
       */
      wrd =  tp->channel / 8;
      bit = (tp->channel % 8) * 2;

      /*
       * Turn off our bits in the HSRRn register.  The register layout is 'backward'.  Move backward if needed.
       */
      reg_p = (&s->hsrr1 - wrd);
      mask = (3 << bit);
      *reg_p = *reg_p & ~mask;

      /*
       * If the host service request is non-zero and ier, then post an interrupt.
       */
      if (tp->host_service_request && tp->ier) {
         qemu_log("ASSERT INITIALIZING IRQ: channel: %d \n", tp->channel);
         qemu_set_irq(tp->irq, 1);
      }

      tp->state = STATE_RUNNING;
   }

   /*
    * Should be in STATE_RUNNING here.
    */
   else {

      /*
       * If interrupts enabled, assert it.
       */
      if (tp->ier) {
         qemu_log("ASSERT RUNNING IRQ: channel: %d \n", tp->channel);
         qemu_set_irq(tp->irq, 1);
      }
   }

   /*
    * If configured as a DIO, then we can set to IDLE.  There is no repeating
    * operation
    */
   if (tp->function == FUNCTION_DIO) {
      tp->state = STATE_IDLE;
   }

   /*
    * If configured as a PWM, find the timeout for the period.
    */
   if (tp->function == FUNCTION_PWM) {
      nSecs = calculate_pwm_delay(s, tp);

      qemu_log("\n%s %d [ TPU %d ] nSecs: %d IER: %d \n", __func__, __LINE__, tp->channel, nSecs, tp->ier);
      timer_mod(tp->timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + nSecs );
   }
}

static const MemoryRegionOps mcf_tpu_ops = {
    .read       = mcf_tpu_read,
    .write      = mcf_tpu_write,
    .endianness = DEVICE_NATIVE_ENDIAN,
};

/*-------------------------------------------------------------------------
 *
 * name:
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void mcf_tpu_instance_init(Object *obj)
{
    SysBusDevice *dev = SYS_BUS_DEVICE(obj);
    mcf_tpu_state *s = MCF_TPU(dev);

    memory_region_init_io(&s->iomem, obj, &mcf_tpu_ops, s, "tpu", 0x30);

    sysbus_init_mmio(dev, &s->iomem);
}

static void mcf_tpu_realize(DeviceState *dev, Error **errp)
{
   uint32_t
      i;
   mcf_tpu_state
      *s = MCF_TPU(dev);
   tpu_t
      *tp;

   /*
    * Debug timer to dump all registers
    */
   s->dump_timer   = timer_new_ns(QEMU_CLOCK_VIRTUAL, tpu_dump_timer_cb, s);
   s->dump_timer_count = 0;

   /*
    * Set timer for 100mSecs
    */
   timer_mod(s->dump_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + TPU_TIMER_INTERVAL_MS(100) );

   /*
    * Timer for channel 1 interrupts
    */
   s->neg_x_timer   = timer_new_ns(QEMU_CLOCK_VIRTUAL, neg_x_timer_cb, s);
   timer_mod(s->neg_x_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + TPU_TIMER_INTERVAL_MS(100) );

   s->neg_x_timer_count = 0;

   for (i=0, tp = s->tpus; i< NUMBER_TPUS; i++, tp++) {
      tp->s       = s;
      tp->timer   = timer_new_ns(QEMU_CLOCK_VIRTUAL, tpu_timer_cb, tp);
      tp->channel = i;

   }
}

static void mcf_tpu_class_init(ObjectClass *oc, const void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->realize = mcf_tpu_realize;
    device_class_set_legacy_reset(dc, mcf_tpu_reset);
    set_bit(DEVICE_CATEGORY_INPUT, dc->categories);
}

static const TypeInfo mcf_tpu_info = {
    .name          = TYPE_MCF_TPU,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(mcf_tpu_state),
    .instance_init = mcf_tpu_instance_init,
    .class_init    = mcf_tpu_class_init,
};

static void mcf_tpu_register(void)
{
    type_register_static(&mcf_tpu_info);
}

type_init(mcf_tpu_register);

/*-------------------------------------------------------------------------
 *
 * name:        mcf_tpu_create
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static DeviceState *mcf_tpu_create(DeviceState *intc_dev)
{
    DeviceState *dev;

    dev = qdev_new(TYPE_MCF_TPU);
    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);

    mcf_tpu_state *s = MCF_TPU(dev);

    s->intc_dev = intc_dev;

    return dev;
}


/*-------------------------------------------------------------------------
 *
 * name:        mcf_tpu_create_mmap
 *
 * description: Create the TPU device
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
DeviceState *mcf_tpu_create_mmap(hwaddr base, DeviceState *intc_dev)
{
    DeviceState *dev;

    dev = mcf_tpu_create(intc_dev);
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 0, base);

    return dev;
}

/*-------------------------------------------------------------------------
 *
 * name:
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void ticr_write(mcf_tpu_state *s, uint32_t val)
{
   uint32_t
      cibv,
      cirl,
      i,
      vector_number;
   tpu_t
      *tp;

   /*
    * The TPU uses 16 interrupts.  The cibv field is the top nibble of the first
    * interrupt.
    *
    * A cibv value of 8 yields interrupts on 0x80 through 0x8f.
    */
   cibv = (val >> 4) & 0xf;
   cirl = (val >> 8) & 7;

   /*
    * Convert register bits into the actual vector number to use.
    */
   vector_number = (cibv << 4);

   /*
    * Set the interrupt priority for each of the 16 TPU interrupts.
    */
   for (i=0; i<NUMBER_TPUS; i++) {
      mcf_intc_set_priority(s->intc_dev, vector_number + i, cirl);
   }


   /*
    * Can now get the ISR infrastructure setup.
    */
   tp = s->tpus;

   for (i=0; i< NUMBER_TPUS; i++,tp++) {
      sysbus_init_irq(SYS_BUS_DEVICE(s), &tp->irq);
      tp->irq = mcf_intc_get_qemu_irq(s->intc_dev, vector_number + i);
      sysbus_connect_irq(SYS_BUS_DEVICE(s), i, tp->irq);
   }

}


/*-------------------------------------------------------------------------
 *
 * name:        neg_x_timer_cb
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void neg_x_timer_cb(void *opaque)
{
   mcf_tpu_state
      *s = opaque;

   s->neg_x_timer_count++;


   /*
    * If more than 3 seconds, then simulate TPU channel 1 interrupts.
    */
   if (s->neg_x_timer_count == 30) {



      qemu_log_start_line("TPU");
      qemu_log("neg_x_timer set \n");

      tpu_assert_irq(s, 7);
   }

   /*
    * Set timer for 100 mSecs later.
    */
   timer_mod(s->neg_x_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + TPU_TIMER_INTERVAL_MS(100) );
}




/*-------------------------------------------------------------------------
 *
 * name:        tpu_dump_timer_cb
 *
 * description: Dump all TPUs
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void tpu_dump_timer_cb(void *opaque)
{
   uint32_t
      i,
      j,
      trp;
   mcf_tpu_state
      *s = opaque;
   tpu_t
      *tp;
   FILE
      *fp;

   if (s->dump_timer_count++ < 40) {

      /*
       * Set timer for 100 mSecs later.
       */
      timer_mod(s->dump_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + TPU_TIMER_INTERVAL_MS(100) );
      return;
   }

   if ((fp = fopen("tpu_dump","w")) == NULL) {
      printf("Error opening dump file \n");
      exit(1);
   }

   qemu_log_start_line("TPU");
   qemu_log("DUMP FILE WRITTEN\n");

   {
      uint64_t ts_usec = qemu_clock_get_us(QEMU_CLOCK_VIRTUAL);

      fprintf(fp, "\n\nDump occurs at: %5ld.%03d \n\n", (ts_usec / 1000), (int) (ts_usec % 1000) );
   }

   fprintf(fp, " # | PRI | func  | HSRQ | HSEQ |\n");

   tp = s->tpus;

   trp = 0xffff00;

   for (i=0; i<16; i++,tp++) {
      fprintf(fp, "%2d |",  i);
      fprintf(fp, "  %d  |",  tp->priority);
      fprintf(fp, " %5s |", function_strs[tp->function]);
      fprintf(fp, "  %2d  |", tp->host_service_request);
      fprintf(fp, "  %2d  | ", tp->host_sequence_code);
      fprintf(fp, "  %x: ", trp);

      /*
       * Read the TPU RAM for this TPU.
       */
      for(j=0; j < 8; j++) {
         fprintf(fp, "%04x ", memory_read_16(trp));
         trp += 2;
      }

      fprintf(fp, "\n");
   }

   fclose(fp);
}

static uint16_t memory_read_16 (uint32_t addr)
{
   uint16_t
      val;

   cpu_physical_memory_read(addr, &val, sizeof(val) );

   /*
    * Get to little endian
    */
   return bswap_16(val);
}

static void tpu_assert_irq(mcf_tpu_state *s, uint32_t channel)
{
   tpu_t
      *tp;

   tp = &s->tpus[channel];

   s->cisr |= (1 << channel);

   qemu_set_irq(tp->irq, 1);
}
