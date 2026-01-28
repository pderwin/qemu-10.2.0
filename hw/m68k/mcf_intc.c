/*
 * ColdFire Interrupt Controller emulation.
 *
 * NOTE: only implements the first 64 of the user interrupts.  This
 *       maps to vectors 64 through 128 in the CPU vector table.
 *
 * Copyright (c) 2007 CodeSourcery.
 *
 * This code is licensed under the GPL
 */

#include "qemu/osdep.h"
#include "qapi/error.h"
#include "qemu/module.h"
#include "qemu/log.h"
#include "cpu.h"
#include "hw/irq.h"
#include "hw/sysbus.h"
#include "hw/m68k/mcf.h"
#include "hw/qdev-properties.h"
#include "qom/object.h"

#define TYPE_MCF_INTC "mcf-intc"
OBJECT_DECLARE_SIMPLE_TYPE(mcf_intc_state, MCF_INTC)

struct mcf_intc_state {
    SysBusDevice parent_obj;

    MemoryRegion iomem;
    uint64_t ipr;
    uint64_t ifr;
    uint64_t enabled;
    uint8_t icr[64];
    M68kCPU *cpu;
    int active_vector;
};

static void mcf_intc_update(mcf_intc_state *s)
{
    uint64_t active;
    int i;
    int best;
    int best_level;

    active = s->ipr;
    best_level = 1;
    best = 64;

    if (active) {
        for (i = 0; i < 64; i++) {
            if ((active & 1) != 0 && s->icr[i] >= best_level) {
                best_level = s->icr[i];
                best = i;
            }
            active >>= 1;
        }
    }

    /*
     * User vectors start at number 64
     */
    s->active_vector = ((best == 64) ? 24 : (best + 64));
//    printf("%s %d av: %d \n", __func__, __LINE__, s->active_vector );

    m68k_set_irq_level(s->cpu, best_level, s->active_vector);

//    printf("%s %d IIIIIIIIIIIIII done \n", __func__, __LINE__);
}

void mcf_intc_set_level(void *opaque, uint32_t irq, uint32_t level)
{
   mcf_intc_state *s = (mcf_intc_state *)opaque;

   s->icr[irq] = level;
}

static void mcf_intc_set_irq(void *opaque, int irq, int level)
{
    mcf_intc_state *s = (mcf_intc_state *)opaque;

//    printf("%s %d SSSSSSSSSSSSS opaque: %p i: %d l: %d\n", __func__, __LINE__, opaque, irq, level);

    if (irq >= 64)
        return;

    if (level)
        s->ipr |= 1ull << irq;
    else
        s->ipr &= ~(1ull << irq);

    mcf_intc_update(s);
}

static void mcf_intc_reset(DeviceState *dev)
{
    mcf_intc_state *s = MCF_INTC(dev);

    s->ipr = 0;
    s->ifr = 0;
    s->enabled = 0;
    memset(s->icr, 0, 64);
    s->active_vector = 24;
}

static const MemoryRegionOps mcf_intc_ops = {
//    .read = mcf_intc_read,
//    .write = mcf_intc_write,
    .endianness = DEVICE_BIG_ENDIAN,
};

static void mcf_intc_instance_init(Object *obj)
{
    mcf_intc_state *s = MCF_INTC(obj);

    memory_region_init_io(&s->iomem, obj, &mcf_intc_ops, s, "mcf", 0x100);
    sysbus_init_mmio(SYS_BUS_DEVICE(obj), &s->iomem);
}

static const Property mcf_intc_properties[] = {
    DEFINE_PROP_LINK("m68k-cpu", mcf_intc_state, cpu,
                     TYPE_M68K_CPU, M68kCPU *),
};

static void mcf_intc_class_init(ObjectClass *oc, const void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    device_class_set_props(dc, mcf_intc_properties);
    set_bit(DEVICE_CATEGORY_MISC, dc->categories);
    device_class_set_legacy_reset(dc, mcf_intc_reset);
}

static const TypeInfo mcf_intc_gate_info = {
    .name          = TYPE_MCF_INTC,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(mcf_intc_state),
    .instance_init = mcf_intc_instance_init,
    .class_init    = mcf_intc_class_init,
};

static void mcf_intc_register_types(void)
{
    type_register_static(&mcf_intc_gate_info);
}

type_init(mcf_intc_register_types)

qemu_irq *mcf_intc_init(MemoryRegion *sysmem,
                        hwaddr base,
                        M68kCPU *cpu)
{
    DeviceState  *dev;

    dev = qdev_new(TYPE_MCF_INTC);

    mcf_qsm_set_intc_device(dev);

    object_property_set_link(OBJECT(dev), "m68k-cpu",
                             OBJECT(cpu), &error_abort);

    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);
    memory_region_add_subregion(sysmem, base,
                                sysbus_mmio_get_region(SYS_BUS_DEVICE(dev), 0));

    return qemu_allocate_irqs(mcf_intc_set_irq, dev, 64);
}
