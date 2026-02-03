/*
 * M68332 QSM block emulation
 *
 */
#include "qemu/osdep.h"
#include "hw/irq.h"
#include "hw/sysbus.h"
#include "qemu/log.h"
#include "qemu/module.h"
#include "qapi/error.h"
#include "hw/m68k/mcf.h"
#include "hw/qdev-properties.h"
#include "hw/qdev-properties-system.h"
#include "chardev/char-fe.h"
#include "qom/object.h"

typedef struct serial_recv_event_s {
   uint32_t    mSecs;
   const char *str;
} serial_recv_event_t;

serial_recv_event_t serial_recv_events[] = {
/*
 * POR is taking about 368 mSec.  Check time stamp of when we get to 'command_loop_wait_for_serial'.
 */
   { 400, "IM\n" },     // cause machine to reboot
   { 300, "vb0\n" },    // turn on verbose
   { 300, "CT\n" },     // ask for copyright information
   { 300, "vj1000\n" }, // set joig speed to 1000
   { 300, "vm200\n" },  // set max motor speed to 200
   { 300, "rl\n" },     // read limit switches
   {   0, "" }          // end of table
};


struct mcf_qsm_state {
   SysBusDevice  parent_obj;
   MemoryRegion  iomem;
   DeviceState  *intc_dev;
   CharFrontend  chr;
   QEMUTimer    *ser_recv_timer;
   QEMUTimer    *ser_xmit_timer;
   QEMUTimer    *spi_timer;
   uint32_t      baud_rate;

   qemu_irq      ser_irq;
   qemu_irq      spi_irq;

   serial_recv_event_t *serial_recv_event;
   const char   *serial_recv_event_cp;

   //
   // Register storage
   //
   uint16_t qsmcr;  // c00
   uint8_t  qilr;   // c04
   uint8_t  qivr;   // c05
   uint16_t sccr0;  // c08
   uint16_t sccr1;  // c0a
#define SCCR1_RIE (1 << 5)
#define SCCR1_TIE (1 << 7)

   uint16_t scsr;   // c0c
#define SCSR_RDRF (1 << 6)
#define SCSR_TC   (1 << 7)
#define SCSR_TDRE (1 << 8)

   uint16_t scdr;   // c0e
   uint8_t  portqs; // c15
   uint16_t spcr1;  // c1a
#define SPCR1_SPE (1 << 15)

   uint8_t  spsr;  // c1f
#define SPSR_SPIF (1 << 7)

};

#define TYPE_MCF_QSM "mcf-qsm"

/*
 * The 'qivr' register value will set the base vector value for both the
 * serial and SPI interrupts.  SER is the first one.
 */
#define IRQ_SER (0)
#define IRQ_SPI (1)


OBJECT_DECLARE_SIMPLE_TYPE(mcf_qsm_state, MCF_QSM);

static void serial_log_entry( const char *read_write_str, uint8_t ch);

/*-------------------------------------------------------------------------
 *
 * name:        ser_recv_start_timer_character
 *
 * description: start the timer for a character time.
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void ser_recv_start_timer_character (mcf_qsm_state *s)
{
   uint32_t
      nSecs;
   float
      bit_time,
      byte_time;

   bit_time = 1.0 / s->baud_rate;
   byte_time = (bit_time * 10);

   /*
    * time timer has a nSec resolution.  Convert the timeout from seconds to nSec.
    */
   nSecs = (byte_time * 1000 * 1000 * 1000);

   timer_mod(s->ser_recv_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + nSecs);
}

/*-------------------------------------------------------------------------
 *
 * name:        ser_recv_start_timer
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void ser_recv_start_timer_string (mcf_qsm_state *s)
{
   uint64_t
      nSecs;

   /*
    * Only start timer if the delay is non-zero
    */
   if (s->serial_recv_event->mSecs) {

      /*
       * Convert mSecs to nSecs
       */
      nSecs = (s->serial_recv_event->mSecs * 1000 * 1000);

      timer_mod(s->ser_recv_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + nSecs);

      /*
       * Set our character pointer to first byte of string
       */
      s->serial_recv_event_cp = s->serial_recv_event->str;
   }
}

/*-------------------------------------------------------------------------
 *
 * name:        ser_recv_timer_cb
 *
 * description: Serial Recv timer has expired
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void ser_recv_timer_cb (void *opaque)
{
   uint8_t
      ch;
   mcf_qsm_state
      *s = opaque;

   /*
    * Get character from the string.
    */
   ch = *s->serial_recv_event_cp;

   /*
    * If we got a valid character, then we put it into the SCDR lower byte and signal an interrupt
    */
   if (!ch) {
      qemu_log_start_line("SER");
      qemu_log("ERROR no character in string\n");
      return;
   }

   serial_log_entry("R", ch);

   s->scdr = ch;
   s->scsr |= SCSR_RDRF;

   if (s->sccr1 & SCCR1_RIE) {
      qemu_set_irq(s->ser_irq, 1);
   }

   /*
    * Move to next character in the string.
    */
   s->serial_recv_event_cp++;

   /*
    * if we are at NULL, then attempt to move to the next string.
    */
   ch = *s->serial_recv_event_cp;

   /*
    * If we have another character in this string, then set timer for a
    * character delay time.
    */
   if (ch) {
      ser_recv_start_timer_character (s);
   }
   else {
      s->serial_recv_event++;

      /*
       * Attempt to start timer for next string.
       */
      ser_recv_start_timer_string(s);
   }

}

/*-------------------------------------------------------------------------
 *
 * name:        ser_xmit_start_timer_character
 *
 * description: start the timer for a character time.
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void ser_xmit_start_timer_character (mcf_qsm_state *s)
{
   uint32_t
      nSecs;
   float
      bit_time,
      byte_time;

   bit_time = 1.0 / s->baud_rate;
   byte_time = (bit_time * 10);

   /*
    * time timer has a nSec resolution.  Convert the timeout from seconds to nSec.
    */
   nSecs = (byte_time * 1000 * 1000 * 1000);

   timer_mod(s->ser_xmit_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + nSecs);
}

/*-------------------------------------------------------------------------
 *
 * name:        ser_xmit_timer_cb
 *
 * description: Serial Xmit timer has expired
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void ser_xmit_timer_cb(void *opaque)
{
    mcf_qsm_state *s = opaque;

    /*
     * Turn on TDRE bit
     */
    s->scsr |= SCSR_TDRE;

    /*
     * Trigger our interrupt if the interrupt is enabled.
     */
    if (s->sccr1 & SCCR1_TIE) {
       qemu_set_irq(s->ser_irq, 1);
    }
}

#define SPI_TIMER_INTERVAL_NS (10000)

/*-------------------------------------------------------------------------
 *
 * name:        spi_timer_cb
 *
 * description: Set the SLOCK bit in the SYNCR register
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void spi_timer_cb(void *opaque)
{
    mcf_qsm_state *s = opaque;

    /*
     * Turn on SPIF bit
     */
    s->spsr |= SPSR_SPIF;

    /*
     * Trigger our interrupt
     */
    qemu_set_irq(s->spi_irq, 1);

    /*
     * Turn off enabled bit
     */
    s->spcr1 &= ~SPCR1_SPE;
}


static uint64_t mcf_qsm_read(void *opaque, hwaddr addr, unsigned size)
{
    mcf_qsm_state *s = opaque;

    switch (addr & 0x3f) {

    case 1:
       return s->qsmcr & 0xff;

    case 4:                     // QILR
       return s->qilr;

    case 0xa:                   // SCCR1
       if (size == 2) {
          return s->sccr1;
       }
       if (size == 1) {
          return (s->sccr1 >> 8) & 0xff;
       }
       break;

    case 0xb:                   // SCCR1 lower byte
       return s->sccr1 & 0xff;

    case 0xc:                   // SCSR


//       qemu_log_start_line("SER");
//       qemu_log("  | SCSR: %04x \n", s->scsr);



       if (size == 2) {
          return s->scsr;
       }
       if (size == 1) {
          return (s->scsr >> 8) & 0xff;
       }
       break;

       /*
        * byte read of lower portion of SCSR
        */
    case 0xd:                   // SCSR.l
       return s->scsr & 0xff;

    case 0xe:                   // SCDR
       /*
        * Reading from the SCDR should clear any pending serial interrupt due to RDRF
        */
       s->scsr &= ~SCSR_RDRF;

       qemu_set_irq(s->ser_irq, 0);

       return s->scdr;

    case 0x1a:                  // SPCR1
//       printf("%s %d !!!!!!!!!!!!!!!!!!! s: %p addr: %lx syncr: %x\n", __func__, __LINE__, s, addr, s->syncr);
       return s->spcr1;

    case 0x15:                  // PORTQS
       return s->portqs;

    case 0x1f:                  // SPSR
//       printf("%s %d !!!!!!!!!!!!!!!!!!! s: %p addr: %lx spsr: %x\n", __func__, __LINE__, s, addr, s->spsr);
       return s->spsr;

    default:
           printf("%s %d Read from unhandled offset: %lx size: %d \n", __func__, __LINE__, addr, size);
       break;
    }

    return 0;
}

/*-------------------------------------------------------------------------
 *
 * name:        mcf_qsm_write
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void mcf_qsm_write(void *opaque, hwaddr addr, uint64_t val, unsigned size)
{
   uint32_t
      baud_rate;
   mcf_qsm_state
      *s = opaque;

    switch (addr & 0x3f) {

    case 0x01:
       break;

       /*
        * This sets the interrupt level, so must update the variables in the interrupt
        * support code.
        */
    case 0x04:
       s->qilr = val;
       break;

    case 0x05:
       s->qivr = val;

       /*
        * Now that we know the interrupt to use, we can set its priority in the controller
        */
       {
          mcf_intc_set_priority(s->intc_dev, (s->qivr + IRQ_SER), (s->qilr >> 0) & 0x7);
          mcf_intc_set_priority(s->intc_dev, (s->qivr + IRQ_SPI), (s->qilr >> 3) & 0x7);
       }

       /*
        * now that we know that we know what interrupt vector to use,
        * connect to it.
        *
        * NOTE:
        *
        * The order in which the sysbus_init_irq() calls are done determines which interrupt number we get in
        * the sysbus device 'dev'.  Since spi is first, it is entry 0, when we sysbus_connect_irq() during
        * mcf_qsm_create()
        */
       s->ser_irq = mcf_intc_get_qemu_irq(s->intc_dev, s->qivr + IRQ_SER);
       sysbus_init_irq(SYS_BUS_DEVICE(s), &s->spi_irq);
       sysbus_connect_irq(SYS_BUS_DEVICE(s), 0, s->spi_irq);

       s->spi_irq = mcf_intc_get_qemu_irq(s->intc_dev, s->qivr + IRQ_SPI);
       sysbus_init_irq(SYS_BUS_DEVICE(s), &s->ser_irq);
       sysbus_connect_irq(SYS_BUS_DEVICE(s), 1, s->ser_irq);

       break;

       /*
        * Write to SCCR0 is setting the baud rate
        */
    case 0x08:

       baud_rate = 0x01000000 / (32 *val);

       qemu_log_start_line("SER");
       qemu_log("  | Baud rate set: %d \n", baud_rate);

       s->baud_rate = baud_rate;
       break;

       /*
        * Write lower byte of the SCCR register
        */
    case 0x0b:
       s->sccr1 = (s->sccr1 & 0xff00) | (val & 0xff);

       if ( (s->sccr1 & SCCR1_TIE) && (s->scsr & SCSR_TDRE) ) {
          qemu_log_mask(CPU_LOG_PLUGIN, "%s %d Assert Serial Interrupt\n", __func__, __LINE__);

          qemu_set_irq(s->ser_irq, 1);
       }
       break;

       /*
        * SCDR - xmit data register when written.  Read will retrieve received chars, so don't
        *        modify the SCDR register content.  Just log the transaction.
        */
    case 0x0e:

       /*
        * Make serial log entry
        */
       serial_log_entry("W", val);

       /*
        * We are no longer TDRE
        */
       s->scsr &= ~SCSR_TDRE;

       /*
        * Remove the irq request
        */
       qemu_set_irq(s->ser_irq, 0);

       /*
        * Setup a timer for when the character transmit time will end.  We need to
        * transmit 10 bit times: START,8 data bits, STOP.  When the timer expires,
        * we need to set TDRE and maybe generate an interrupt
        */
       ser_xmit_start_timer_character(s);

       break;

    case 0x15:
//       printf("%s %d PORTQS\n", __func__, __LINE__);
       break;

    case 0x16:
//       printf("%s %d PQSPAR\n", __func__, __LINE__);
       break;

    case 0x17:
//       printf("%s %d DDRQS\n", __func__, __LINE__);
       break;

    case 0x18:
//       printf("%s %d SPCR0\n", __func__, __LINE__);
       break;


    case 0x1a:
//       printf("%s %d SPCR1\n", __func__, __LINE__);

       s->spcr1 = val;

       if (val & SPCR1_SPE) {
          timer_mod(s->spi_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + SPI_TIMER_INTERVAL_NS);
       }

       break;

    case 0x1c:
//       printf("%s %d SPCR2\n", __func__, __LINE__);
       break;

    case 0x1e:
//       printf("%s %d SPCR3\n", __func__, __LINE__);
       break;

    case 0x1f:
//       printf("%s %d SPSR\n", __func__, __LINE__);

       /*
        * If the SPIF bit is on, but writing it will clear it, then remove the interrupt
        */
       if ((s->spsr & SPSR_SPIF) && !(val & SPSR_SPIF)) {
          qemu_set_irq(s->spi_irq, 0);
       }

       s->spsr = val;
       break;

    default:
       printf("%s %d Write to unhandled offset: %lx\n", __func__, __LINE__, addr);
       break;
    }
}

/*-------------------------------------------------------------------------
 *
 * name:        mcf_qsm_reset
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void mcf_qsm_reset(DeviceState *dev)
{
    mcf_qsm_state *s = MCF_QSM(dev);

    s->spcr1 = 0x0404;
}

static const MemoryRegionOps mcf_qsm_ops = {
    .read  = mcf_qsm_read,
    .write = mcf_qsm_write,
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
static void mcf_qsm_instance_init(Object *obj)
{
    SysBusDevice *dev = SYS_BUS_DEVICE(obj);
    mcf_qsm_state *s = MCF_QSM(dev);

    memory_region_init_io(&s->iomem, obj, &mcf_qsm_ops, s, "uart", 0x40);

    sysbus_init_mmio(dev, &s->iomem);

    /*
     * NOTE:
     *
     * The order in which the sysbus_init_irq() calls are done determines which interrupt number we get in
     * the sysbus device 'dev'.  Since spi is first, it is entry 0, when we sysbus_connect_irq() during
     * mcf_qsm_create()
     */

    /*
     * Initialze registers
     */
    s->scsr = (SCSR_TDRE | SCSR_TC);
}

static void mcf_qsm_realize(DeviceState *dev, Error **errp)
{
    mcf_qsm_state *s = MCF_QSM(dev);

    s->ser_recv_timer = timer_new_ns(QEMU_CLOCK_VIRTUAL, ser_recv_timer_cb, s);
    s->ser_xmit_timer = timer_new_ns(QEMU_CLOCK_VIRTUAL, ser_xmit_timer_cb, s);
    s->spi_timer      = timer_new_ns(QEMU_CLOCK_VIRTUAL, spi_timer_cb,      s);

    /*
     * Queue up the first serial recv event.  The '_cp' being NULL will cause us to
     * start reading from the first character of the string.
     */
    s->serial_recv_event    = serial_recv_events;
    s->serial_recv_event_cp = NULL;

    /*
     * start the timer for the next string.
     */
    ser_recv_start_timer_string(s);

}

static const Property mcf_qsm_properties[] = {
    DEFINE_PROP_CHR("chardev", mcf_qsm_state, chr),
};

static void mcf_qsm_class_init(ObjectClass *oc, const void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->realize = mcf_qsm_realize;
    device_class_set_legacy_reset(dc, mcf_qsm_reset);
    device_class_set_props(dc, mcf_qsm_properties);
    set_bit(DEVICE_CATEGORY_INPUT, dc->categories);
}

static const TypeInfo mcf_qsm_info = {
    .name          = TYPE_MCF_QSM,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(mcf_qsm_state),
    .instance_init = mcf_qsm_instance_init,
    .class_init    = mcf_qsm_class_init,
};

static void mcf_qsm_register(void)
{
    type_register_static(&mcf_qsm_info);
}

type_init(mcf_qsm_register);

/*-------------------------------------------------------------------------
 *
 * name:        mcf_qsm_create
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static DeviceState *mcf_qsm_create(DeviceState *intc_dev)
{
    DeviceState *dev;

    dev = qdev_new(TYPE_MCF_QSM);
    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);

    mcf_qsm_state *s = MCF_QSM(dev);

    s->intc_dev = intc_dev;

    return dev;
}


/*-------------------------------------------------------------------------
 *
 * name:        mcf_qsm_create_mmap
 *
 * description: Create the QSM device
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
DeviceState *mcf_qsm_create_mmap(hwaddr base, DeviceState *intc_dev)
{
    DeviceState *dev;

    dev = mcf_qsm_create(intc_dev);
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 0, base);

    return dev;
}

/*-------------------------------------------------------------------------
 *
 * name:        serial_log_entry
 *
 * description: write a log entry with regards to a serial transaction
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void serial_log_entry( const char *read_write_str, uint8_t ch)
{
   qemu_log_start_line("SER");

   qemu_log("%s |          |          |       %02x |", read_write_str, ch);

   if (isprint(ch)) {
      qemu_log(" '%c'", ch);
   }

   qemu_log("\n");

}
