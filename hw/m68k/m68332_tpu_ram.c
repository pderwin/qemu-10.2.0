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
#include "hw/m68k/m68332.h"
#include "hw/qdev-properties.h"
#include "hw/qdev-properties-system.h"
#include "chardev/char-fe.h"
#include "qom/object.h"

#define NUMBER_TPUS (16)

struct mcf_tpu_ram_state {
   SysBusDevice  parent_obj;
   MemoryRegion  iomem;

   void         *tpu_dev;

   //
   // Register storage
   //
   uint16_t parm[ NUMBER_TPUS ][8]; // f00
};

#define TYPE_MCF_TPU_RAM "mcf-tpu-ram"

OBJECT_DECLARE_SIMPLE_TYPE(mcf_tpu_ram_state, MCF_TPU_RAM);

/*-------------------------------------------------------------------------
 *
 * name:        mcf_tpu_ram_read
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static uint64_t mcf_tpu_ram_read(void *opaque, hwaddr addr, unsigned size)
{
   uint32_t
      chan,
      ret,
      wrd;

    mcf_tpu_ram_state *s = opaque;

    /*
     * The TCR1 register is mapped onto memory at channel 14 RAM at offset
     * 0xc.
     */
    if (addr == 0xec) {
       ret = mcf_tpu_tcr1_read(s->tpu_dev);
    }
    else {
       chan = (addr >> 4);
       wrd  = (addr & 0xf) >> 1;

       ret = s->parm[chan][wrd];
    }

    return ret;
}

/*-------------------------------------------------------------------------
 *
 * name:        mcf_tpu_ram_write
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void mcf_tpu_ram_write(void *opaque, hwaddr addr, uint64_t _val, unsigned size)
{
   mcf_tpu_ram_state
      *s = opaque;
   uint32_t
      chan,
      wrd;
   uint16_t
      val = _val;   // truncate 64 bit value to 16 bits

    chan = (addr >> 4);
    wrd  = (addr & 0xf) >> 1;

    s->parm[chan][wrd] = val;

    qemu_log("%s %d addr: %lx val: %x \n", __func__, __LINE__, addr, val);
}


/*-------------------------------------------------------------------------
 *
 * name:        mcf_tpu_ram_reset
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void mcf_tpu_ram_reset(DeviceState *dev)
{
   uint32_t
      i;
   mcf_tpu_ram_state
      *s = MCF_TPU_RAM(dev);

   /*
    * Mark all sections of RAM with their respective GPIOs high
    */
   for (i=0; i< NUMBER_TPUS; i++) {
      s->parm[i][1]= 0x8000;
   }

}

static const MemoryRegionOps mcf_tpu_ops = {
    .read       = mcf_tpu_ram_read,
    .write      = mcf_tpu_ram_write,
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
    mcf_tpu_ram_state *s = MCF_TPU_RAM(dev);

    memory_region_init_io(&s->iomem, obj, &mcf_tpu_ops, s, "tpu-ram", 0x100);

    sysbus_init_mmio(dev, &s->iomem);
}

static void mcf_tpu_realize(DeviceState *dev, Error **errp)
{
    mcf_tpu_ram_state *s = MCF_TPU_RAM(dev);

    (void) s;
}

#if 0
static const Property mcf_tpu_ram_properties[] = {
//    DEFINE_PROP_CHR("chardev", mcf_tpu_ram_state, chr),
};
#endif

static void mcf_tpu_class_init(ObjectClass *oc, const void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->realize = mcf_tpu_realize;
    device_class_set_legacy_reset(dc, mcf_tpu_ram_reset);
//    device_class_set_props(dc, mcf_tpu_properties);
    set_bit(DEVICE_CATEGORY_INPUT, dc->categories);
}

static const TypeInfo mcf_tpu_ram_info = {
    .name          = TYPE_MCF_TPU_RAM,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(mcf_tpu_ram_state),
    .instance_init = mcf_tpu_instance_init,
    .class_init    = mcf_tpu_class_init,
};

static void mcf_tpu_register(void)
{
    type_register_static(&mcf_tpu_ram_info);
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
static DeviceState *mcf_tpu_ram_create( DeviceState *tpu_dev)
{
    mcf_tpu_ram_state
       *s;
    DeviceState *dev;

    dev = qdev_new(TYPE_MCF_TPU_RAM);
    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);

    s = MCF_TPU_RAM(dev);

    s->tpu_dev = tpu_dev;

    return dev;
}


/*-------------------------------------------------------------------------
 *
 * name:        mcf_tpu_ram_create_mmap
 *
 * description: Create the QSM device
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
DeviceState *mcf_tpu_ram_create_mmap(hwaddr base, DeviceState *tpu_dev)
{
    DeviceState *dev;

    dev = mcf_tpu_ram_create( tpu_dev );
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 0, base);

    return dev;
}
