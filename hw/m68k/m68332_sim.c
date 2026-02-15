/*
 * ColdFire UART emulation.
 *
 * Copyright (c) 2007 CodeSourcery.
 *
 * This code is licensed under the GPL
 */

#include "qemu/osdep.h"
#include "hw/sysbus.h"
#include "qemu/module.h"
#include "qemu/timer.h"
#include "qapi/error.h"
#include "hw/m68k/m68332.h"

struct mcf_sim_state {
   SysBusDevice parent_obj;
   MemoryRegion iomem;
   QEMUTimer *slock_timer;

   /*
    * Registers
    */
   uint32_t syncr;  // a04
#define SYNCR_SLOCK ( 1 << 3  )
#define SYNCR_X     ( 1 << 14 )

   uint32_t porte;  // a11 or a13
};

#define SYNCR  (0x04)
#define PORTE0 (0x11)
#define PORTE1 (0x13)

#define TYPE_MCF_SIM "mcf-sim"

OBJECT_DECLARE_SIMPLE_TYPE(mcf_sim_state, MCF_SIM)
#define SLOCK_TIMER_INTERVAL_NS (100)
/*-------------------------------------------------------------------------
 *
 * name:        slock_timer_cb
 *
 * description: Set the SLOCK bit in the SYNCR register
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void slock_timer_cb(void *opaque)
{
    mcf_sim_state *s = opaque;

    s->syncr |= SYNCR_SLOCK;
}


/*-------------------------------------------------------------------------
 *
 * name:        mcf_sim_read
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static uint64_t mcf_sim_read(void *opaque, hwaddr addr, unsigned size)
{
    mcf_sim_state *s = opaque;

    switch (addr & 0x3f) {

    case PORTE0:
    case PORTE1:
       return s->porte;
    case SYNCR:                  // SYNCR register
       return s->syncr;
    }

    return 0;
}


/*-------------------------------------------------------------------------
 *
 * name:        mcf_sim_write
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void mcf_sim_write(void *opaque, hwaddr addr, uint64_t val, unsigned size)
{
    mcf_sim_state *s = opaque;

    switch (addr & 0x3f) {

    case PORTE0:
    case PORTE1:
       s->porte = val;
       break;

    case SYNCR:

       s->syncr = val;

       if (val & SYNCR_X) {
          timer_mod(s->slock_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + SLOCK_TIMER_INTERVAL_NS);

          s->syncr &= ~SYNCR_SLOCK;
       }

       break;
    }
}

static void mcf_sim_reset(DeviceState *dev)
{
    mcf_sim_state *s = MCF_SIM(dev);

    s->syncr = 0x3f00;
}

static const MemoryRegionOps mcf_sim_ops = {
    .read  = mcf_sim_read,
    .write = mcf_sim_write,
    .endianness = DEVICE_NATIVE_ENDIAN,
};

static void mcf_sim_instance_init(Object *obj)
{
    SysBusDevice *dev = SYS_BUS_DEVICE(obj);
    mcf_sim_state *s = MCF_SIM(dev);

    memory_region_init_io(&s->iomem, obj, &mcf_sim_ops, s, "uart", 0x40);

    sysbus_init_mmio(dev, &s->iomem);
//    sysbus_init_irq(dev, &s->irq);
}

static void mcf_sim_realize(DeviceState *dev, Error **errp)
{
    mcf_sim_state *s = MCF_SIM(dev);

    s->slock_timer = timer_new_ns(QEMU_CLOCK_VIRTUAL, slock_timer_cb, s);
}

static void mcf_sim_class_init(ObjectClass *oc, const void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->realize = mcf_sim_realize;
    device_class_set_legacy_reset(dc, mcf_sim_reset);
    set_bit(DEVICE_CATEGORY_INPUT, dc->categories);
}

static const TypeInfo mcf_sim_info = {
    .name          = TYPE_MCF_SIM,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(mcf_sim_state),
    .instance_init = mcf_sim_instance_init,
    .class_init    = mcf_sim_class_init,
};

static void mcf_sim_register(void)
{
    type_register_static(&mcf_sim_info);
}

type_init(mcf_sim_register);

static DeviceState *mcf_sim_create(void)
{
   DeviceState *dev;

   dev = qdev_new(TYPE_MCF_SIM);

   sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);

   return dev;
}

DeviceState *mcf_sim_create_mmap(hwaddr base)
{
    DeviceState *dev;

    dev = mcf_sim_create();
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 0, base);

    return dev;
}
