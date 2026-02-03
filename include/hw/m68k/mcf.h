#ifndef HW_MCF_H
#define HW_MCF_H
/* Motorola ColdFire device prototypes.  */

#include "exec/hwaddr.h"
#include "target/m68k/cpu-qom.h"

/* mcf_uart.c */
uint64_t mcf_uart_read(void *opaque, hwaddr addr,
                       unsigned size);
void mcf_uart_write(void *opaque, hwaddr addr,
                    uint64_t val, unsigned size);
DeviceState *mcf_uart_create(qemu_irq irq, Chardev *chr);
DeviceState *mcf_uart_create_mmap(hwaddr base, qemu_irq irq, Chardev *chr);

DeviceState *mcf_qsm_create_mmap(hwaddr base, DeviceState *intc_dev);
DeviceState *mcf_sim_create_mmap(hwaddr base);
DeviceState *mcf_tpu_create_mmap(hwaddr base, DeviceState *intc_dev);

/* mcf_intc.c */
DeviceState *mcf_intc_init(M68kCPU *cpu);

qemu_irq mcf_intc_get_qemu_irq(void *intc_dev, uint32_t vector_number);
/*
 * Change the interrupt level for a device's interrupt request.
 */
void mcf_intc_set_priority       (void *opaque, uint32_t vector_number, uint32_t priority);



/* mcf5206.c */
#define TYPE_MCF5206_MBAR "mcf5206-mbar"

#endif
