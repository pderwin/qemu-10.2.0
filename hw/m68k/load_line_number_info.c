#include <fcntl.h>
#include <stdio.h>
#include <elf.h>
#include <libelf.h>
#include <libdwarf/libdwarf.h>
#include <stdlib.h>
#include <unistd.h>
#include "hw/m68k/mcf.h"


void load_line_number_info(const char* elf_name) {
    int fd;
    Elf* elf = NULL;
    Dwarf_Debug dbg = NULL;
    Dwarf_Error err;
    int res;


    // 1. Open the ELF file
    if ((fd = open(elf_name, O_RDONLY)) < 0) {
        perror("Oh no! The file failed on open!");
        return;
    }

    // 2. Initialize libelf
    if (elf_version(EV_CURRENT) == EV_NONE) {
        fprintf(stderr, "ELF library initialization failed: %s\n", elf_errmsg(-1));
        goto cleanup;
    }
    if ((elf = elf_begin(fd, ELF_C_READ, (Elf*)0)) == 0) {
        fprintf(stderr, "elf_begin failed: %s\n", elf_errmsg(-1));
        goto cleanup;
    }

    // 3. Initialize libdwarf
    // The error object returned by dwarf_elf_init must be freed with free(3), not dwarf_dealloc()
    res = dwarf_elf_init(elf, DW_DLC_READ, NULL, NULL, &dbg, &err);
    if (res == DW_DLV_ERROR) {
        fprintf(stderr, "dwarf_init returned DW_DLV_ERROR\n");
        free(err); // Free error object from init
        goto cleanup;
    } else if (res == DW_DLV_NO_ENTRY) {
        printf("No DWARF debugging information found in the file.\n");
        goto cleanup;
    }

    printf("DWARF initialized successfully. Ready to parse line info.\n");

    //
    // Further steps involve calling libdwarf functions like dwarf_get_globals,
    // dwarf_next_cu_header, and iterating through line programs and their entries.
    // This part of the code is a starting point.
    //

cleanup:
    // 4. Clean up
    if (dbg != NULL) {
        dwarf_finish(dbg, &err); // Use dwarf_dealloc for subsequent errors
    }
    if (elf != NULL) {
        elf_end(elf);
    }
    if (fd >= 0) {
        close(fd);
    }
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_filename>\n", argv[0]);
        return 1;
    }

    parse_dwarf_line_info(argv[1]);
    return 0;
}
