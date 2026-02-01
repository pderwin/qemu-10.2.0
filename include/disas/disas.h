#ifndef QEMU_DISAS_H
#define QEMU_DISAS_H

/* Disassemble this for me please... (debugging). */
#ifdef CONFIG_TCG
void disas(FILE *out, const void *code, size_t size);
void target_disas(FILE *out, CPUState *cpu, const DisasContextBase *db);
#endif

void monitor_disas(Monitor *mon, CPUState *cpu, uint64_t pc,
                   int nb_insn, bool is_physical);

#ifdef CONFIG_PLUGIN
char *plugin_disas(CPUState *cpu, const DisasContextBase *db,
                   uint64_t addr, size_t size);
#endif

/* Look up symbol for debugging purpose.  Returns "" if unknown. */
const char *lookup_symbol  (uint64_t orig_addr);
uint32_t    lookup_symbol_r(uint64_t orig_addr, char *buf, uint32_t buf_siz);
uint32_t    symbol_address (const char *str, uint32_t *addr);


struct syminfo;
struct elf32_sym;
struct elf64_sym;

typedef const char *(*lookup_symbol_t)  (struct syminfo *s, uint64_t orig_addr);
typedef uint32_t    (*lookup_symbol_r_t)(struct syminfo *s, uint64_t orig_addr, char *buf, uint32_t buf_siz);
typedef uint32_t    (*symbol_address_t) (struct syminfo *s, const char *name, uint64_t *addr_ptr);

struct syminfo {
    lookup_symbol_t   lookup_symbol;
    lookup_symbol_r_t lookup_symbol_r;
    symbol_address_t  symbol_address;

    unsigned int disas_num_syms;
    union {
      struct elf32_sym *elf32;
      struct elf64_sym *elf64;
    } disas_symtab;
    const char *disas_strtab;
    struct syminfo *next;
};

/* Filled in by elfload.c.  Simplistic, but will do for now. */
extern struct syminfo *syminfos;

#endif /* QEMU_DISAS_H */
