/*
 * M68332 QSM block emulation
 *
 */
#include "qemu/osdep.h"
#include "hw/irq.h"
#include "hw/sysbus.h"
#include "qemu/module.h"
#include "qapi/error.h"
#include "hw/m68k/mcf.h"
#include "hw/qdev-properties.h"
#include "hw/qdev-properties-system.h"
#include "chardev/char-fe.h"
#include "qom/object.h"

struct mcf_qsm_state {
   SysBusDevice  parent_obj;
   MemoryRegion  iomem;
   CharFrontend  chr;
   QEMUTimer    *spi_timer;
   qemu_irq      spi_irq;

   uint16_t qsmcr;  // c00
   uint8_t  qilr;   // c04
   uint8_t  qivr;   // c05
   uint16_t sccr0;  // c08
   uint16_t sccr1;  // c0a
   uint16_t scsr;   // c0c
   uint16_t scdr;   // c0e
   uint8_t  portqs; // c15
   uint16_t spcr1;  // c1a
#define SPCR1_SPE (1 << 15)

   uint8_t  spsr;  // c1f
#define SPSR_SPIF (1 << 7)
};

#define TYPE_MCF_QSM "mcf-qsm"

OBJECT_DECLARE_SIMPLE_TYPE(mcf_qsm_state, MCF_QSM);


static DeviceState *my_intc_device;

void mcf_qsm_set_intc_device(DeviceState *dev)
{
   my_intc_device = dev;
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
       return s->scsr & 0xffff;

    case 0xc:                   // SCSR
       return s->scsr;

    case 0xe:                   // SCDR
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

#define SYNCR_SLOCK ( 1 << 3  )
#define SYNCR_X     ( 1 << 14 )

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
    mcf_qsm_state *s = opaque;

//    printf("%s %d !!!!!!!!!!!!!!!!!!! s: %p a: %lx val: %lx siz: %d\n", __func__, __LINE__, s, addr, val, size);

    switch (addr & 0x3f) {

    case 0x01:
//       printf("%s %d QSMCR.l\n", __func__, __LINE__);
       break;

       /*
        * This sets the interrupt level, so must update the variables in the interrupt
        * support code.
        */
    case 0x04:
       s->qilr = val;

#define IRQ_SER (0)
#define IRQ_SPI (1)

       mcf_intc_set_level(my_intc_device, IRQ_SER, val >> 0);
       mcf_intc_set_level(my_intc_device, IRQ_SPI, val >> 3);
       break;

    case 0x05:
//       printf("%s %d QIVR intv: 0x%lx\n", __func__, __LINE__, val);
       s->qivr = val;
       break;
    case 0x08:
//       printf("%s %d SCCR0\n", __func__, __LINE__);
       break;

    case 0x0b:
//       printf("%s %d SCCR1.l\n", __func__, __LINE__);
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
//          printf("SPI enabled!\n");
          timer_mod(s->spi_timer, qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) + SPI_TIMER_INTERVAL_NS);

          /*
           * Need to make sure status is not showing complete.
           */
//          s->syncr &= ~SYNCR_SLOCK;
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
//          printf("CLEARING SPI INTERRUPT\n");
          qemu_set_irq(s->spi_irq, 0);
       }

       s->spsr = val;
       break;

    default:
       printf("%s %d Write to unhandled offset: %lx\n", __func__, __LINE__, addr);
       break;
    }
}

static void mcf_qsm_reset(DeviceState *dev)
{
    mcf_qsm_state *s = MCF_QSM(dev);

    s->spcr1 = 0x0404;
}

#if 0
static void mcf_uart_push_byte(mcf_uart_state *s, uint8_t data)
{
    /* Break events overwrite the last byte if the fifo is full.  */
    if (s->fifo_len == FIFO_DEPTH) {
        s->fifo_len--;
    }

    s->fifo[s->fifo_len] = data;
    s->fifo_len++;
    s->sr |= MCF_UART_RxRDY;
    if (s->fifo_len == FIFO_DEPTH) {
        s->sr |= MCF_UART_FFULL;
    }

    mcf_uart_update(s);
}

static void mcf_uart_event(void *opaque, QEMUChrEvent event)
{
    mcf_uart_state *s = (mcf_uart_state *)opaque;

    switch (event) {
    case CHR_EVENT_BREAK:
        s->isr |= MCF_UART_DBINT;
        mcf_uart_push_byte(s, 0);
        break;
    default:
        break;
    }
}

static int mcf_uart_can_receive(void *opaque)
{
    mcf_uart_state *s = (mcf_uart_state *)opaque;

    return s->rx_enabled ? FIFO_DEPTH - s->fifo_len : 0;
}

static void mcf_uart_receive(void *opaque, const uint8_t *buf, int size)
{
    mcf_uart_state *s = (mcf_uart_state *)opaque;

    for (int i = 0; i < size; i++) {
        mcf_uart_push_byte(s, buf[i]);
    }
}
#endif

static const MemoryRegionOps mcf_qsm_ops = {
    .read  = mcf_qsm_read,
    .write = mcf_qsm_write,
    .endianness = DEVICE_NATIVE_ENDIAN,
};

static void mcf_qsm_instance_init(Object *obj)
{
    SysBusDevice *dev = SYS_BUS_DEVICE(obj);
    mcf_qsm_state *s = MCF_QSM(dev);

    memory_region_init_io(&s->iomem, obj, &mcf_qsm_ops, s, "uart", 0x40);

    sysbus_init_mmio(dev, &s->iomem);
    sysbus_init_irq(dev, &s->spi_irq);
}

static void mcf_qsm_realize(DeviceState *dev, Error **errp)
{
    mcf_qsm_state *s = MCF_QSM(dev);

    s->spi_timer = timer_new_ns(QEMU_CLOCK_VIRTUAL, spi_timer_cb, s);
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


static DeviceState *mcf_qsm_create(qemu_irq irq, Chardev *chrdrv)
{
    DeviceState *dev;

    dev = qdev_new(TYPE_MCF_QSM);
    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);

    /*
     * Connect to irq
     */
    sysbus_connect_irq(SYS_BUS_DEVICE(dev), 0, irq);

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
DeviceState *mcf_qsm_create_mmap(hwaddr base, qemu_irq irq, Chardev *chrdrv)
{
    DeviceState *dev;

    dev = mcf_qsm_create(irq, chrdrv);
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 0, base);

    return dev;
}
