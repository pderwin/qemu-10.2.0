/*
 * Motorola ColdFire MCF5208 SoC emulation.
 *
 * Copyright (c) 2007 CodeSourcery.
 *
 * This code is licensed under the GPL
 *
 * This file models both the MCF5208 SoC, and the
 * MCF5208EVB evaluation board. For details see
 *
 * "MCF5208 Reference Manual"
 * https://www.nxp.com/docs/en/reference-manual/MCF5208RM.pdf
 * "M5208EVB-RevB 32-bit Microcontroller User Manual"
 * https://www.nxp.com/docs/en/reference-manual/M5208EVBUM.pdf
 */
#include "qemu/osdep.h"
#include "qemu/units.h"
#include "qemu/error-report.h"
#include "qapi/error.h"
#include "qemu/datadir.h"
#include "cpu.h"
#include "hw/m68k/m68332.h"
#include "hw/boards.h"
#include "hw/loader.h"
#include "elf.h"

#define SYS_FREQ 166666666

#define ROM_SIZE 0x40000

/*-------------------------------------------------------------------------
 *
 * name:        m68332_init
 *
 * description:
 *
 * input:
 *
 * output:
 *
 *-------------------------------------------------------------------------*/
static void m68332_init(MachineState *machine)
{
    ram_addr_t
       ram_size = machine->ram_size;
    const char
       *kernel_filename = machine->kernel_filename;
    M68kCPU
       *cpu;
    DeviceState
       *intc_dev,
       *tpu_dev;
    CPUM68KState
       *env;
    int
       kernel_size;
    uint64_t
       elf_entry;
    hwaddr
       entry;
    MemoryRegion
       *address_space_mem = get_system_memory(),
       *rom = g_new(MemoryRegion, 1),
       *sram = g_new(MemoryRegion, 1);

    cpu = M68K_CPU(cpu_create(machine->cpu_type));
    env = &cpu->env;

    /* Initialize CPU registers.  */
    env->vbr = 0;

    /* RAM at 0x00000000 */
    memory_region_init_ram(rom, NULL, "firmware.rom", ROM_SIZE, &error_fatal);
    memory_region_add_subregion(address_space_mem, 0x00000000, rom);

    /* DRAM at 0x40000000 */
    memory_region_add_subregion(address_space_mem, 0x40000000, machine->ram);

    /* Internal SRAM.  */
    memory_region_init_ram(sram, NULL, "firmware.bin" , 128 * KiB, &error_fatal);
    memory_region_add_subregion(address_space_mem, 0x40000, sram);

    /* Internal peripherals.  */
    intc_dev = m68332_intc_init(cpu);

    mcf_sim_create_mmap(0xfffffa00);

    mcf_qsm_create_mmap(  0xfffc00, intc_dev);

    /*
     * TPU block
     */
    tpu_dev = mcf_tpu_create_mmap(  0xfffe00, intc_dev);

    /*
     * TPU RAM block.  Passing ptr to the TPU so that it can read the TCR1 register value when
     * a read to TPU_RAM[14][6] occurs.
     */
    mcf_tpu_ram_create_mmap( 0xffff00, tpu_dev );


    /* Load firmware */
    if (machine->firmware) {
        char *fn;
        uint8_t *ptr;

        fn = qemu_find_file(QEMU_FILE_TYPE_BIOS, machine->firmware);
        if (!fn) {
            error_report("Could not find ROM image '%s'", machine->firmware);
            exit(1);
        }
        if (load_image_targphys(fn, 0x0, ROM_SIZE, NULL) < 8) {
            error_report("Could not load ROM image '%s'", machine->firmware);
            exit(1);
        }
        g_free(fn);
        /* Initial PC is always at offset 4 in firmware binaries */
        ptr = rom_ptr(0x4, 4);
        assert(ptr != NULL);
        env->pc = ldl_be_p(ptr);
    }

    /* Load kernel.  */
    if (!kernel_filename) {
        if (machine->firmware) {
            return;
        }
        error_report("Kernel image must be specified");
        exit(1);
    }

    kernel_size = load_elf(kernel_filename, NULL, NULL, NULL, &elf_entry,
                           NULL, NULL, NULL, ELFDATA2MSB, EM_68K, 0, 0);
    entry = elf_entry;
    if (kernel_size < 0) {
        kernel_size = load_uimage(kernel_filename, &entry, NULL, NULL,
                                  NULL, NULL);
    }
    if (kernel_size < 0) {
        kernel_size = load_image_targphys(kernel_filename, 0x40000000,
                                          ram_size, NULL);
        entry = 0x40000000;
    }
    if (kernel_size < 0) {
        error_report("Could not load kernel '%s'", kernel_filename);
        exit(1);
    }

    env->pc = entry;
    env->aregs[7] = 0x3fffc;
}


static void m68332_machine_init(MachineClass *mc)
{
    mc->desc = "M68332";
    mc->init = m68332_init;
    mc->is_default = true;
    mc->default_cpu_type = M68K_CPU_TYPE_NAME("m68332");
    mc->default_ram_id = "m68332.ram";
}

DEFINE_MACHINE("m68332", m68332_machine_init)
