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

#define USER_VECTORS_SUPPORTED (192)
#define USER_VECTOR_FIRST      (64)
#define USER_VECTOR_WORDS      (USER_VECTORS_SUPPORTED / 64)

struct mcf_intc_state {
   SysBusDevice  parent_obj;
   MemoryRegion  iomem;
   qemu_irq     *qemu_irqs;
   uint64_t      pending[USER_VECTOR_WORDS];
   uint8_t       priority[USER_VECTORS_SUPPORTED];
   M68kCPU      *cpu;
   int           active_vector;
};

/*-------------------------------------------------------------------------
 *
 * name:        mcf_intc_update
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void mcf_intc_update(mcf_intc_state *s)
{
   uint32_t
      uvn,
      w;
    uint64_t
       pending;
    int i;
    int best;
    int best_level;

    best_level = 0;
    best = -1;

    /*
     * Process all words of the pending register
     */
    for (w=0; w < USER_VECTOR_WORDS; w++) {

       pending = s->pending[w];

       if (pending) {

          /*
           * Scan all 64 bits of the register
           */
          for (i = 0; i < 64; i++) {
             /*
              * If pending bit is set, and the priority is higher than what has been seen so far...
              */
             if ( (pending & (1 << i)) && s->priority[i] >= best_level) {

                uvn = (w * 64) + i;  // user vector number

                best_level = s->priority[uvn];
                best = uvn;
             }
          }
       }
    }

    /*
     * User vectors start at number 64
     */
    s->active_vector = ((best == -1) ? 24 : (best + 64));

    m68k_set_irq_level(s->cpu, best_level, s->active_vector);
}

/*-------------------------------------------------------------------------
 *
 * name:        mcf_intc_get_qemu_irq
 *
 * description: Return the qemu_irq ptr from this device's array
 *
 * input:       vector_number: The CPUs vector number entry.  This code
 *                             only works with the user vectors, so we
 *                             subtract 64 from the input.
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
qemu_irq mcf_intc_get_qemu_irq(void *opaque, uint32_t vector_number)
{
   mcf_intc_state
      *s = MCF_INTC(opaque);
   qemu_irq
      qemu_irq;

   qemu_irq = s->qemu_irqs[vector_number - 64];

   return qemu_irq;
}

/*-------------------------------------------------------------------------
 *
 * name:        mcf_intc_set_priority
 *
 * description: Set the priority level for a given interrupt vector.  This
 *              code only manages the user interrupts, so we subtract 64
 *              from the vector number.
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
void mcf_intc_set_priority(void *opaque, uint32_t vector_number, uint32_t priority)
{
   uint32_t
      irq;
   mcf_intc_state
      *s = (mcf_intc_state *)opaque;

   irq = (vector_number - 64);

   s->priority[irq] = priority;
}

/*-------------------------------------------------------------------------
 *
 * name:        mcf_intc_set_irq
 *
 * description: This is a callback routine, invoked with driver code calls
 *              qemu_set_irq()
 *
 * input:       irq: user interrupt vector number
 *                   i.e. an input value of 0 results in vector number 64 being
 *                   asserted.
 *
 * output:
 *
 * NOTE:        The function is provided as a callback to the qemu_allocate_irqs()
 *              call below and therefore the parms are fixed.  Cannot include
 *              a parm for setting the interrupt priority on this call.
 *-------------------------------------------------------------------------*/
static void mcf_intc_set_irq_cb(void *opaque, int irq, int state)
{
   uint32_t
      bit,
      w;
   mcf_intc_state
      *s = opaque;

   if (irq >= USER_VECTORS_SUPPORTED) {
      qemu_log("%s: invalid irq number: %d\n", __func__, irq);
      return;
   }

   /*
    * Find which bit to set in the array of 64-bit variables
    */
   w   = irq / 64;
   bit = irq % 64;

   if (state)
      s->pending[w] |= (1ull << bit);
   else
      s->pending[w] &= ~(1ull << bit);

   mcf_intc_update(s);
}

static void mcf_intc_reset(DeviceState *dev)
{
    mcf_intc_state *s = MCF_INTC(dev);

    bzero(s->pending, sizeof(s->pending));
    bzero(s->priority, sizeof(s->priority));
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

type_init(mcf_intc_register_types);

DeviceState *mcf_intc_init(M68kCPU *cpu)
{
   DeviceState
      *dev;

   dev = qdev_new(TYPE_MCF_INTC);

   mcf_intc_state *s = MCF_INTC(dev);

   object_property_set_link(OBJECT(dev), "m68k-cpu",
                            OBJECT(cpu), &error_abort);

   sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);

   s->qemu_irqs = qemu_allocate_irqs(mcf_intc_set_irq_cb, dev, USER_VECTORS_SUPPORTED);

   return dev;
}
