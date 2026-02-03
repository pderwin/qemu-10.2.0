/*
 * M68332 QSM block emulation
 *
 */
#include "qemu/osdep.h"
#include "qemu/log.h"
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

struct mcf_tpu_state {
   SysBusDevice  parent_obj;
   MemoryRegion  iomem;
   qemu_irq      tpu_irq;




   CharFrontend  chr;
   QEMUTimer    *ser_recv_timer;
   QEMUTimer    *ser_xmit_timer;
   QEMUTimer    *spi_timer;
   uint32_t      baud_rate;

   //
   // Register storage
   //
   uint16_t tpumcr; // e00
   uint16_t tcr;    // e02
   uint16_t dscr;   // e04
   uint16_t dssr;   // e06
   uint16_t ticr;   // e08
   uint16_t cier;   // e0a
   uint16_t cfsr0;  // e0c
   uint16_t cfsr1;  // e0e
   uint16_t cfsr2;  // e10
   uint16_t cfsr3;  // e12
   uint16_t hsrr0;  // e18
   uint16_t hsrr1;  // e1a
   uint16_t cpr0;   // e1c
   uint16_t cpr1;   // e1e
};

enum {
   TPUMCR = 0,
   TICR   = 8,
   CIER   = 0xa,
   CFSR0  = 0xc,
   CFSR1  = 0xe,
   CFSR2  = 0x10,
   CFSR3  = 0x12,
   HSRR0  = 0x18,
   HSRR1  = 0x1a,
   CPR0   = 0x1c,
   CPR1   = 0x1e,
};

#define TYPE_MCF_TPU "mcf-tpu"

OBJECT_DECLARE_SIMPLE_TYPE(mcf_tpu_state, MCF_TPU);

static DeviceState *my_intc_device;
static void ticr_write(uint32_t val);

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
void mcf_tpu_set_intc_device(DeviceState *dev)
{
   my_intc_device = dev;
}

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
    case CPR0:   return s->cpr0;
    case CPR1:   return s->cpr1;
    case HSRR0:  return s->hsrr0;
    case HSRR1:  return s->hsrr1;
    case TICR:   return s->ticr;
    case TPUMCR: return s->tpumcr;

    default:
       qemu_log_start_line("ERR");
       qemu_log("%s %d Read from unhandled offset: %lx size: %d \n", __func__, __LINE__, addr, size);
       break;
    }

    return 0;
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

    case CFSR0: s->cfsr0 = val;
       break;

    case CFSR1:
       s->cfsr1 = val;
       break;

    case CFSR2:
       s->cfsr2 = val;
       break;

    case CFSR3:
       s->cfsr3 = val;
       break;

    case CIER:  s->cier = val;  break;
    case CPR0:  s->cpr0 = val;  break;
    case CPR1:  s->cpr1 = val;  break;
    case HSRR0: s->hsrr0 = val; break;
    case HSRR1: s->hsrr1 = val; break;
       /*
        * TPU Interrupt Control Register
        */
    case TICR:
       s->ticr = val;
       ticr_write(val);
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

    memory_region_init_io(&s->iomem, obj, &mcf_tpu_ops, s, "tpu", 0x40);

    sysbus_init_mmio(dev, &s->iomem);

    /*
     * NOTE:
     *
     * The order in which the sysbus_init_irq() calls are done determines which interrupt number we get in
     * the sysbus device 'dev'.  Since spi is first, it is entry 0, when we sysbus_connect_irq() during
     * mcf_qsm_create()
     */
    sysbus_init_irq(dev, &s->tpu_irq);

    /*
     * Initialze registers
     */
//    s->scsr = (SCSR_TDRE | SCSR_TC);
}

static void mcf_tpu_realize(DeviceState *dev, Error **errp)
{
    mcf_tpu_state *s = MCF_TPU(dev);

    (void) s;

//    s->spi_timer      = timer_new_ns(QEMU_CLOCK_VIRTUAL, spi_timer_cb,      s);

//    ser_recv_start_timer_string(s);

}

static const Property mcf_tpu_properties[] = {
    DEFINE_PROP_CHR("chardev", mcf_tpu_state, chr),
};

static void mcf_tpu_class_init(ObjectClass *oc, const void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->realize = mcf_tpu_realize;
    device_class_set_legacy_reset(dc, mcf_tpu_reset);
    device_class_set_props(dc, mcf_tpu_properties);
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
 * name:        mcf_qsm_create
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static DeviceState *mcf_tpu_create(qemu_irq tpu_irq)
{
    DeviceState *dev;

    dev = qdev_new(TYPE_MCF_TPU);
    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);

    /*
     * Connect to irq's
     *
     * irq number is determined by the calls to sysbus_init_irq() above.  See notes there.
     */
    sysbus_connect_irq(SYS_BUS_DEVICE(dev), 0, tpu_irq);

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
DeviceState *mcf_tpu_create_mmap(hwaddr base, qemu_irq tpu_irq)
{
    DeviceState *dev;

    dev = mcf_tpu_create(tpu_irq);
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
static void ticr_write(uint32_t val)
{
   uint32_t
      cibv,
      cirl;

   /*
    * The TPU uses 16 interrupts.  The cibv field is the top nibble of the first
    * interrupt.
    *
    * A cibv value of 8 yields interrupts on 0x80 through 0x8f.
    */
   cibv = (val >> 4) & 0xf;
   cirl = (val >> 8) & 7;

   qemu_log("TICR: cirl: %x cibv: %x\n", cirl, cibv);
}
