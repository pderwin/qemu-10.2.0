#pragma once

#include "target/m68k/cpu-qom.h"

DeviceState *mcf_qsm_create_mmap    (hwaddr base, DeviceState *intc_dev);
DeviceState *mcf_sim_create_mmap    (hwaddr base);
DeviceState *mcf_tpu_create_mmap    (hwaddr base, DeviceState *intc_dev);
DeviceState *mcf_tpu_ram_create_mmap(hwaddr base, DeviceState *tpu_dev);
uint32_t     mcf_tpu_tcr1_read      (void *opaque);

/* mcf_intc.c */
DeviceState *m68332_intc_init(M68kCPU *cpu);

qemu_irq mcf_intc_get_qemu_irq(void *intc_dev, uint32_t vector_number);

/*
 * Change the interrupt level for a device's interrupt request.
 */
void mcf_intc_set_priority       (void *opaque, uint32_t vector_number, uint32_t priority);

void load_line_number_info (const char *filename);
