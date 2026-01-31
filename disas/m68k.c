/* This file is composed of several different files from the upstream
   sourceware.org CVS.  Original file boundaries marked with **** */

#include "qemu/osdep.h"
#include <math.h>

#include "disas/dis-asm.h"

/* **** floatformat.h from sourceware.org CVS 2005-08-14.  */
/* IEEE floating point support declarations, for GDB, the GNU Debugger.
   Copyright 1991, 1994, 1995, 1997, 2000, 2003 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses/>.  */

#if !defined (FLOATFORMAT_H)
#define FLOATFORMAT_H 1

/*#include "ansidecl.h" */

/* A floatformat consists of a sign bit, an exponent and a mantissa.  Once the
   bytes are concatenated according to the byteorder flag, then each of those
   fields is contiguous.  We number the bits with 0 being the most significant
   (i.e. BITS_BIG_ENDIAN type numbering), and specify which bits each field
   contains with the *_start and *_len fields.  */

/* What is the order of the bytes. */

enum floatformat_byteorders {

  /* Standard little endian byte order.
     EX: 1.2345678e10 => 00 00 80 c5 e0 fe 06 42 */

  floatformat_little,

  /* Standard big endian byte order.
     EX: 1.2345678e10 => 42 06 fe e0 c5 80 00 00 */

  floatformat_big,

  /* Little endian byte order but big endian word order.
     EX: 1.2345678e10 => e0 fe 06 42 00 00 80 c5 */

  floatformat_littlebyte_bigword

};

enum floatformat_intbit { floatformat_intbit_yes, floatformat_intbit_no };

struct floatformat
{
  enum floatformat_byteorders byteorder;
  unsigned int totalsize;	/* Total size of number in bits */

  /* Sign bit is always one bit long.  1 means negative, 0 means positive.  */
  unsigned int sign_start;

  unsigned int exp_start;
  unsigned int exp_len;
  /* Bias added to a "true" exponent to form the biased exponent.  It
     is intentionally signed as, otherwise, -exp_bias can turn into a
     very large number (e.g., given the exp_bias of 0x3fff and a 64
     bit long, the equation (long)(1 - exp_bias) evaluates to
     4294950914) instead of -16382).  */
  int exp_bias;
  /* Exponent value which indicates NaN.  This is the actual value stored in
     the float, not adjusted by the exp_bias.  This usually consists of all
     one bits.  */
  unsigned int exp_nan;

  unsigned int man_start;
  unsigned int man_len;

  /* Is the integer bit explicit or implicit?  */
  enum floatformat_intbit intbit;

  /* Internal name for debugging. */
  const char *name;

  /* Validator method.  */
  int (*is_valid) (const struct floatformat *fmt, const char *from);
};

/* floatformats for IEEE single and double, big and little endian.  */

extern const struct floatformat floatformat_ieee_single_big;
extern const struct floatformat floatformat_ieee_single_little;
extern const struct floatformat floatformat_ieee_double_big;
extern const struct floatformat floatformat_ieee_double_little;

/* floatformat for ARM IEEE double, little endian bytes and big endian words */

extern const struct floatformat floatformat_ieee_double_littlebyte_bigword;

/* floatformats for various extendeds.  */

extern const struct floatformat floatformat_i387_ext;
extern const struct floatformat floatformat_m68881_ext;
extern const struct floatformat floatformat_i960_ext;
extern const struct floatformat floatformat_m88110_ext;
extern const struct floatformat floatformat_m88110_harris_ext;
extern const struct floatformat floatformat_arm_ext_big;
extern const struct floatformat floatformat_arm_ext_littlebyte_bigword;
/* IA-64 Floating Point register spilt into memory.  */
extern const struct floatformat floatformat_ia64_spill_big;
extern const struct floatformat floatformat_ia64_spill_little;
extern const struct floatformat floatformat_ia64_quad_big;
extern const struct floatformat floatformat_ia64_quad_little;

/* Convert from FMT to a double.
   FROM is the address of the extended float.
   Store the double in *TO.  */

extern void
floatformat_to_double (const struct floatformat *, const char *, double *);

/* The converse: convert the double *FROM to FMT
   and store where TO points.  */

extern void
floatformat_from_double (const struct floatformat *, const double *, char *);

/* Return non-zero iff the data at FROM is a valid number in format FMT.  */

extern int
floatformat_is_valid (const struct floatformat *fmt, const char *from);

#endif	/* defined (FLOATFORMAT_H) */
/* **** End of floatformat.h */
/* **** m68k-dis.h from sourceware.org CVS 2005-08-14.  */
/* Opcode table header for m680[01234]0/m6888[12]/m68851.
   Copyright 1989, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1999, 2001,
   2003, 2004 Free Software Foundation, Inc.

   This file is part of GDB, GAS, and the GNU binutils.

   GDB, GAS, and the GNU binutils are free software; you can redistribute
   them and/or modify them under the terms of the GNU General Public
   License as published by the Free Software Foundation; either version
   1, or (at your option) any later version.

   GDB, GAS, and the GNU binutils are distributed in the hope that they
   will be useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
   the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not,
   see <http://www.gnu.org/licenses/>.  */

/* These are used as bit flags for the arch field in the m68k_opcode
   structure.  */
#define	_m68k_undef  0
#define	m68000   0x001
#define	m68008   m68000 /* Synonym for -m68000.  otherwise unused.  */
#define	m68010   0x002
#define	m68020   0x004
#define	m68030   0x008
#define m68ec030 m68030 /* Similar enough to -m68030 to ignore differences;
                           gas will deal with the few differences.  */
#define	m68040   0x010
/* There is no 68050.  */
#define m68060   0x020
#define	m68881   0x040
#define	m68882   m68881 /* Synonym for -m68881.  otherwise unused.  */
#define	m68851   0x080
#define cpu32	 0x100		/* e.g., 68332 */

#define mcfmac   0x200		/* ColdFire MAC. */
#define mcfemac  0x400		/* ColdFire EMAC. */
#define cfloat   0x800		/* ColdFire FPU.  */
#define mcfhwdiv 0x1000		/* ColdFire hardware divide.  */

#define mcfisa_a 0x2000		/* ColdFire ISA_A.  */
#define mcfisa_aa 0x4000	/* ColdFire ISA_A+.  */
#define mcfisa_b 0x8000		/* ColdFire ISA_B.  */
#define mcfusp   0x10000	/* ColdFire USP instructions.  */

#define mcf5200  0x20000
#define mcf5206e 0x40000
#define mcf521x  0x80000
#define mcf5249  0x100000
#define mcf528x  0x200000
#define mcf5307  0x400000
#define mcf5407  0x800000
#define mcf5470  0x1000000
#define mcf5480  0x2000000

 /* Handy aliases.  */
#define	m68040up   (m68040 | m68060)
#define	m68030up   (m68030 | m68040up)
#define	m68020up   (m68020 | m68030up)
#define	m68010up   (m68010 | cpu32 | m68020up)
#define	m68000up   (m68000 | m68010up)

#define	mfloat  (m68881 | m68882 | m68040 | m68060)
#define	mmmu    (m68851 | m68030 | m68040 | m68060)

/* The structure used to hold information for an opcode.  */

struct m68k_opcode
{
  /* The opcode name.  */
  const char *name;
  /* The pseudo-size of the instruction(in bytes).  Used to determine
     number of bytes necessary to disassemble the instruction.  */
  unsigned int size;
  /* The opcode itself.  */
  unsigned long opcode;
  /* The mask used by the disassembler.  */
  unsigned long match;
  /* The arguments.  */
  const char *args;
  /* The architectures which support this opcode.  */
  unsigned int arch;
};

/* The structure used to hold information for an opcode alias.  */

struct m68k_opcode_alias
{
  /* The alias name.  */
  const char *alias;
  /* The instruction for which this is an alias.  */
  const char *primary;
};

/* We store four bytes of opcode for all opcodes because that is the
   most any of them need.  The actual length of an instruction is
   always at least 2 bytes, and is as much longer as necessary to hold
   the operands it has.

   The match field is a mask saying which bits must match particular
   opcode in order for an instruction to be an instance of that
   opcode.

   The args field is a string containing two characters for each
   operand of the instruction.  The first specifies the kind of
   operand; the second, the place it is stored.  */

/* Kinds of operands:
   Characters used: AaBbCcDdEeFfGgHIiJkLlMmnOopQqRrSsTtU VvWwXxYyZz01234|*~%;@!&$?/<>#^+-

   D  data register only.  Stored as 3 bits.
   A  address register only.  Stored as 3 bits.
   a  address register indirect only.  Stored as 3 bits.
   R  either kind of register.  Stored as 4 bits.
   r  either kind of register indirect only.  Stored as 4 bits.
      At the moment, used only for cas2 instruction.
   F  floating point coprocessor register only.   Stored as 3 bits.
   O  an offset (or width): immediate data 0-31 or data register.
      Stored as 6 bits in special format for BF... insns.
   +  autoincrement only.  Stored as 3 bits (number of the address register).
   -  autodecrement only.  Stored as 3 bits (number of the address register).
   Q  quick immediate data.  Stored as 3 bits.
      This matches an immediate operand only when value is in range 1 .. 8.
   M  moveq immediate data.  Stored as 8 bits.
      This matches an immediate operand only when value is in range -128..127
   T  trap vector immediate data.  Stored as 4 bits.

   k  K-factor for fmove.p instruction.   Stored as a 7-bit constant or
      a three bit register offset, depending on the field type.

   #  immediate data.  Stored in special places (b, w or l)
      which say how many bits to store.
   ^  immediate data for floating point instructions.   Special places
      are offset by 2 bytes from '#'...
   B  pc-relative address, converted to an offset
      that is treated as immediate data.
   d  displacement and register.  Stores the register as 3 bits
      and stores the displacement in the entire second word.

   C  the CCR.  No need to store it; this is just for filtering validity.
   S  the SR.  No need to store, just as with CCR.
   U  the USP.  No need to store, just as with CCR.
   E  the MAC ACC.  No need to store, just as with CCR.
   e  the EMAC ACC[0123].
   G  the MAC/EMAC MACSR.  No need to store, just as with CCR.
   g  the EMAC ACCEXT{01,23}.
   H  the MASK.  No need to store, just as with CCR.
   i  the MAC/EMAC scale factor.

   I  Coprocessor ID.   Not printed if 1.   The Coprocessor ID is always
      extracted from the 'd' field of word one, which means that an extended
      coprocessor opcode can be skipped using the 'i' place, if needed.

   s  System Control register for the floating point coprocessor.

   J  Misc register for movec instruction, stored in 'j' format.
        Possible values:
        0x000	SFC	Source Function Code reg	[60, 40, 30, 20, 10]
        0x001	DFC	Data Function Code reg		[60, 40, 30, 20, 10]
        0x002   CACR    Cache Control Register          [60, 40, 30, 20, mcf]
        0x003	TC	MMU Translation Control		[60, 40]
        0x004	ITT0	Instruction Transparent
                                Translation reg 0	[60, 40]
        0x005	ITT1	Instruction Transparent
                                Translation reg 1	[60, 40]
        0x006	DTT0	Data Transparent
                                Translation reg 0	[60, 40]
        0x007	DTT1	Data Transparent
                                Translation reg 1	[60, 40]
        0x008	BUSCR	Bus Control Register		[60]
        0x800	USP	User Stack Pointer		[60, 40, 30, 20, 10]
        0x801   VBR     Vector Base reg                 [60, 40, 30, 20, 10, mcf]
        0x802	CAAR	Cache Address Register		[        30, 20]
        0x803	MSP	Master Stack Pointer		[    40, 30, 20]
        0x804	ISP	Interrupt Stack Pointer		[    40, 30, 20]
        0x805	MMUSR	MMU Status reg			[    40]
        0x806	URP	User Root Pointer		[60, 40]
        0x807	SRP	Supervisor Root Pointer		[60, 40]
        0x808	PCR	Processor Configuration reg	[60]
        0xC00	ROMBAR	ROM Base Address Register	[520X]
        0xC04	RAMBAR0	RAM Base Address Register 0	[520X]
        0xC05	RAMBAR1	RAM Base Address Register 0	[520X]
        0xC0F	MBAR0	RAM Base Address Register 0	[520X]
        0xC04   FLASHBAR FLASH Base Address Register    [mcf528x]
        0xC05   RAMBAR  Static RAM Base Address Register [mcf528x]

    L  Register list of the type d0-d7/a0-a7 etc.
       (New!  Improved!  Can also hold fp0-fp7, as well!)
       The assembler tries to see if the registers match the insn by
       looking at where the insn wants them stored.

    l  Register list like L, but with all the bits reversed.
       Used for going the other way. . .

    c  cache identifier which may be "nc" for no cache, "ic"
       for instruction cache, "dc" for data cache, or "bc"
       for both caches.  Used in cinv and cpush.  Always
       stored in position "d".

    u  Any register, with ``upper'' or ``lower'' specification.  Used
       in the mac instructions with size word.

 The remainder are all stored as 6 bits using an address mode and a
 register number; they differ in which addressing modes they match.

   *  all					(modes 0-6,7.0-4)
   ~  alterable memory				(modes 2-6,7.0,7.1)
                                                (not 0,1,7.2-4)
   %  alterable					(modes 0-6,7.0,7.1)
                                                (not 7.2-4)
   ;  data					(modes 0,2-6,7.0-4)
                                                (not 1)
   @  data, but not immediate			(modes 0,2-6,7.0-3)
                                                (not 1,7.4)
   !  control					(modes 2,5,6,7.0-3)
                                                (not 0,1,3,4,7.4)
   &  alterable control				(modes 2,5,6,7.0,7.1)
                                                (not 0,1,3,4,7.2-4)
   $  alterable data				(modes 0,2-6,7.0,7.1)
                                                (not 1,7.2-4)
   ?  alterable control, or data register	(modes 0,2,5,6,7.0,7.1)
                                                (not 1,3,4,7.2-4)
   /  control, or data register			(modes 0,2,5,6,7.0-3)
                                                (not 1,3,4,7.4)
   >  *save operands				(modes 2,4,5,6,7.0,7.1)
                                                (not 0,1,3,7.2-4)
   <  *restore operands				(modes 2,3,5,6,7.0-3)
                                                (not 0,1,4,7.4)

   coldfire move operands:
   m                                            (modes 0-4)
   n						(modes 5,7.2)
   o						(modes 6,7.0,7.1,7.3,7.4)
   p						(modes 0-5)

   coldfire bset/bclr/btst/mulsl/mulul operands:
   q						(modes 0,2-5)
   v						(modes 0,2-5,7.0,7.1)
   b                                            (modes 0,2-5,7.2)
   w                                            (modes 2-5,7.2)
   y						(modes 2,5)
   z						(modes 2,5,7.2)
   x  mov3q immediate operand.
   4						(modes 2,3,4,5)
  */

/* For the 68851:  */
/* I didn't use much imagination in choosing the
   following codes, so many of them aren't very
   mnemonic. -rab

   0  32 bit pmmu register
        Possible values:
        000	TC	Translation Control Register (68030, 68851)

   1  16 bit pmmu register
        111	AC	Access Control (68851)

   2  8 bit pmmu register
        100	CAL	Current Access Level (68851)
        101	VAL	Validate Access Level (68851)
        110	SCC	Stack Change Control (68851)

   3  68030-only pmmu registers (32 bit)
        010	TT0	Transparent Translation reg 0
                        (aka Access Control reg 0 -- AC0 -- on 68ec030)
        011	TT1	Transparent Translation reg 1
                        (aka Access Control reg 1 -- AC1 -- on 68ec030)

   W  wide pmmu registers
        Possible values:
        001	DRP	Dma Root Pointer (68851)
        010	SRP	Supervisor Root Pointer (68030, 68851)
        011	CRP	Cpu Root Pointer (68030, 68851)

   f	function code register (68030, 68851)
        0	SFC
        1	DFC

   V	VAL register only (68851)

   X	BADx, BACx (16 bit)
        100	BAD	Breakpoint Acknowledge Data (68851)
        101	BAC	Breakpoint Acknowledge Control (68851)

   Y	PSR (68851) (MMUSR on 68030) (ACUSR on 68ec030)
   Z	PCSR (68851)

   |	memory          (modes 2-6, 7.*)

   t  address test level (68030 only)
      Stored as 3 bits, range 0-7.
      Also used for breakpoint instruction now.

*/

/* Places to put an operand, for non-general operands:
   Characters used: BbCcDdFfGgHhIijkLlMmNnostWw123456789/

   s  source, low bits of first word.
   d  dest, shifted 9 in first word
   1  second word, shifted 12
   2  second word, shifted 6
   3  second word, shifted 0
   4  third word, shifted 12
   5  third word, shifted 6
   6  third word, shifted 0
   7  second word, shifted 7
   8  second word, shifted 10
   9  second word, shifted 5
   D  store in both place 1 and place 3; for divul and divsl.
   B  first word, low byte, for branch displacements
   W  second word (entire), for branch displacements
   L  second and third words (entire), for branch displacements
      (also overloaded for move16)
   b  second word, low byte
   w  second word (entire) [variable word/long branch offset for dbra]
   W  second word (entire) (must be signed 16 bit value)
   l  second and third word (entire)
   g  variable branch offset for bra and similar instructions.
      The place to store depends on the magnitude of offset.
   t  store in both place 7 and place 8; for floating point operations
   c  branch offset for cpBcc operations.
      The place to store is word two if bit six of word one is zero,
      and words two and three if bit six of word one is one.
   i  Increment by two, to skip over coprocessor extended operands.   Only
      works with the 'I' format.
   k  Dynamic K-factor field.   Bits 6-4 of word 2, used as a register number.
      Also used for dynamic fmovem instruction.
   C  floating point coprocessor constant - 7 bits.  Also used for static
      K-factors...
   j  Movec register #, stored in 12 low bits of second word.
   m  For M[S]ACx; 4 bits split with MSB shifted 6 bits in first word
      and remaining 3 bits of register shifted 9 bits in first word.
      Indicate upper/lower in 1 bit shifted 7 bits in second word.
      Use with `R' or `u' format.
   n  `m' without upper/lower indication. (For M[S]ACx; 4 bits split
      with MSB shifted 6 bits in first word and remaining 3 bits of
      register shifted 9 bits in first word.  No upper/lower
      indication is done.)  Use with `R' or `u' format.
   o  For M[S]ACw; 4 bits shifted 12 in second word (like `1').
      Indicate upper/lower in 1 bit shifted 7 bits in second word.
      Use with `R' or `u' format.
   M  For M[S]ACw; 4 bits in low bits of first word.  Indicate
      upper/lower in 1 bit shifted 6 bits in second word.  Use with
      `R' or `u' format.
   N  For M[S]ACw; 4 bits in low bits of second word.  Indicate
      upper/lower in 1 bit shifted 6 bits in second word.  Use with
      `R' or `u' format.
   h  shift indicator (scale factor), 1 bit shifted 10 in second word

 Places to put operand, for general operands:
   d  destination, shifted 6 bits in first word
   b  source, at low bit of first word, and immediate uses one byte
   w  source, at low bit of first word, and immediate uses two bytes
   l  source, at low bit of first word, and immediate uses four bytes
   s  source, at low bit of first word.
      Used sometimes in contexts where immediate is not allowed anyway.
   f  single precision float, low bit of 1st word, immediate uses 4 bytes
   F  double precision float, low bit of 1st word, immediate uses 8 bytes
   x  extended precision float, low bit of 1st word, immediate uses 12 bytes
   p  packed float, low bit of 1st word, immediate uses 12 bytes
   G  EMAC accumulator, load  (bit 4 2nd word, !bit8 first word)
   H  EMAC accumulator, non load  (bit 4 2nd word, bit 8 first word)
   F  EMAC ACCx
   f  EMAC ACCy
   I  MAC/EMAC scale factor
   /  Like 's', but set 2nd word, bit 5 if trailing_ampersand set
   ]  first word, bit 10
*/

extern const struct m68k_opcode m68k_opcodes[];
extern const struct m68k_opcode_alias m68k_opcode_aliases[];

extern const int m68k_numopcodes, m68k_numaliases;

/* **** End of m68k-opcode.h */
/* **** m68k-dis.c from sourceware.org CVS 2005-08-14.  */
/* Print Motorola 68k instructions.
   Copyright 1986, 1987, 1989, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.  */

/* Local function prototypes.  */

static const char * const fpcr_names[] =
{
  "", "%fpiar", "%fpsr", "%fpiar/%fpsr", "%fpcr",
  "%fpiar/%fpcr", "%fpsr/%fpcr", "%fpiar/%fpsr/%fpcr"
};

static const char *const reg_names[] =
{
  "%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",
  "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%fp", "%sp",
  "%ps", "%pc"
};

/* Name of register halves for MAC/EMAC.
   Separate from reg_names since 'spu', 'fpl' look weird.  */
static const char *const reg_half_names[] =
{
  "%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",
  "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%a7",
  "%ps", "%pc"
};

/* Sign-extend an (unsigned char).  */
#if __STDC__ == 1
#define COERCE_SIGNED_CHAR(ch) ((signed char) (ch))
#else
#define COERCE_SIGNED_CHAR(ch) ((int) (((ch) ^ 0x80) & 0xFF) - 128)
#endif

/* Get a 1 byte signed integer.  */
#define NEXTBYTE(p)  (p += 2, fetch_data(info, p), COERCE_SIGNED_CHAR(p[-1]))

/* Get a 2 byte signed integer.  */
#define COERCE16(x) ((int) (((x) ^ 0x8000) - 0x8000))
#define NEXTWORD(p)  \
  (p += 2, fetch_data(info, p), \
   COERCE16 ((p[-2] << 8) + p[-1]))

/* Get a 4 byte signed integer.  */
#define COERCE32(x) ((bfd_signed_vma) ((x) ^ 0x80000000) - 0x80000000)
#define NEXTLONG(p)  \
  (p += 4, fetch_data(info, p), \
   (COERCE32 ((((((p[-4] << 8) + p[-3]) << 8) + p[-2]) << 8) + p[-1])))

/* Get a 4 byte unsigned integer.  */
#define NEXTULONG(p)  \
  (p += 4, fetch_data(info, p), \
   (unsigned int) ((((((p[-4] << 8) + p[-3]) << 8) + p[-2]) << 8) + p[-1]))

/* Get a single precision float.  */
#define NEXTSINGLE(val, p) \
  (p += 4, fetch_data(info, p), \
   floatformat_to_double (&floatformat_ieee_single_big, (char *) p - 4, &val))

/* Get a double precision float.  */
#define NEXTDOUBLE(val, p) \
  (p += 8, fetch_data(info, p), \
   floatformat_to_double (&floatformat_ieee_double_big, (char *) p - 8, &val))

/* Get an extended precision float.  */
#define NEXTEXTEND(val, p) \
  (p += 12, fetch_data(info, p), \
   floatformat_to_double (&floatformat_m68881_ext, (char *) p - 12, &val))

/* Need a function to convert from packed to double
   precision.   Actually, it's easier to print a
   packed number than a double anyway, so maybe
   there should be a special case to handle this... */
#define NEXTPACKED(p) \
  (p += 12, fetch_data(info, p), 0.0)

/* Maximum length of an instruction.  */
#define MAXLEN 22

struct private
{
  /* Points to first byte not fetched.  */
  bfd_byte *max_fetched;
  bfd_byte the_buffer[MAXLEN];
  bfd_vma insn_start;
  sigjmp_buf bailout;
};

/* Make sure that bytes from INFO->PRIVATE_DATA->BUFFER (inclusive)
   to ADDR (exclusive) are valid.  Returns 1 for success, longjmps
   on error.  */
static int
fetch_data2(struct disassemble_info *info, bfd_byte *addr)
{
  int status;
  struct private *priv = (struct private *)info->private_data;
  bfd_vma start = priv->insn_start + (priv->max_fetched - priv->the_buffer);

  status = (*info->read_memory_func) (start,
                                      priv->max_fetched,
                                      addr - priv->max_fetched,
                                      info);
  if (status != 0)
    {
      (*info->memory_error_func) (status, start, info);
      siglongjmp(priv->bailout, 1);
    }
  else
    priv->max_fetched = addr;
  return 1;
}

static int
fetch_data(struct disassemble_info *info, bfd_byte *addr)
{
    if (addr <= ((struct private *) (info->private_data))->max_fetched) {
        return 1;
    } else {
        return fetch_data2(info, addr);
    }
}

/* This function is used to print to the bit-bucket.  */
static int
dummy_printer (FILE *file ATTRIBUTE_UNUSED,
               const char *format ATTRIBUTE_UNUSED,
               ...)
{
  return 0;
}

static void
dummy_print_address (bfd_vma vma ATTRIBUTE_UNUSED,
                     struct disassemble_info *info ATTRIBUTE_UNUSED)
{
}

/* Fetch BITS bits from a position in the instruction specified by CODE.
   CODE is a "place to put an argument", or 'x' for a destination
   that is a general address (mode and register).
   BUFFER contains the instruction.  */

static int
fetch_arg (unsigned char *buffer,
           int code,
           int bits,
           disassemble_info *info)
{
  int val = 0;

  switch (code)
    {
    case '/': /* MAC/EMAC mask bit.  */
      val = buffer[3] >> 5;
      break;

    case 'G': /* EMAC ACC load.  */
      val = ((buffer[3] >> 3) & 0x2) | ((~buffer[1] >> 7) & 0x1);
      break;

    case 'H': /* EMAC ACC !load.  */
      val = ((buffer[3] >> 3) & 0x2) | ((buffer[1] >> 7) & 0x1);
      break;

    case ']': /* EMAC ACCEXT bit.  */
      val = buffer[0] >> 2;
      break;

    case 'I': /* MAC/EMAC scale factor.  */
      val = buffer[2] >> 1;
      break;

    case 'F': /* EMAC ACCx.  */
      val = buffer[0] >> 1;
      break;

    case 'f':
      val = buffer[1];
      break;

    case 's':
      val = buffer[1];
      break;

    case 'd':			/* Destination, for register or quick.  */
      val = (buffer[0] << 8) + buffer[1];
      val >>= 9;
      break;

    case 'x':			/* Destination, for general arg.  */
      val = (buffer[0] << 8) + buffer[1];
      val >>= 6;
      break;

    case 'k':
      fetch_data(info, buffer + 3);
      val = (buffer[3] >> 4);
      break;

    case 'C':
      fetch_data(info, buffer + 3);
      val = buffer[3];
      break;

    case '1':
      fetch_data(info, buffer + 3);
      val = (buffer[2] << 8) + buffer[3];
      val >>= 12;
      break;

    case '2':
      fetch_data(info, buffer + 3);
      val = (buffer[2] << 8) + buffer[3];
      val >>= 6;
      break;

    case '3':
    case 'j':
      fetch_data(info, buffer + 3);
      val = (buffer[2] << 8) + buffer[3];
      break;

    case '4':
      fetch_data(info, buffer + 5);
      val = (buffer[4] << 8) + buffer[5];
      val >>= 12;
      break;

    case '5':
      fetch_data(info, buffer + 5);
      val = (buffer[4] << 8) + buffer[5];
      val >>= 6;
      break;

    case '6':
      fetch_data(info, buffer + 5);
      val = (buffer[4] << 8) + buffer[5];
      break;

    case '7':
      fetch_data(info, buffer + 3);
      val = (buffer[2] << 8) + buffer[3];
      val >>= 7;
      break;

    case '8':
      fetch_data(info, buffer + 3);
      val = (buffer[2] << 8) + buffer[3];
      val >>= 10;
      break;

    case '9':
      fetch_data(info, buffer + 3);
      val = (buffer[2] << 8) + buffer[3];
      val >>= 5;
      break;

    case 'e':
      val = (buffer[1] >> 6);
      break;

    case 'm':
      val = (buffer[1] & 0x40 ? 0x8 : 0)
        | ((buffer[0] >> 1) & 0x7)
        | (buffer[3] & 0x80 ? 0x10 : 0);
      break;

    case 'n':
      val = (buffer[1] & 0x40 ? 0x8 : 0) | ((buffer[0] >> 1) & 0x7);
      break;

    case 'o':
      val = (buffer[2] >> 4) | (buffer[3] & 0x80 ? 0x10 : 0);
      break;

    case 'M':
      val = (buffer[1] & 0xf) | (buffer[3] & 0x40 ? 0x10 : 0);
      break;

    case 'N':
      val = (buffer[3] & 0xf) | (buffer[3] & 0x40 ? 0x10 : 0);
      break;

    case 'h':
      val = buffer[2] >> 2;
      break;

    default:
      abort ();
    }

  switch (bits)
    {
    case 1:
      return val & 1;
    case 2:
      return val & 3;
    case 3:
      return val & 7;
    case 4:
      return val & 017;
    case 5:
      return val & 037;
    case 6:
      return val & 077;
    case 7:
      return val & 0177;
    case 8:
      return val & 0377;
    case 12:
      return val & 07777;
    default:
      abort ();
    }
}

/* Check if an EA is valid for a particular code.  This is required
   for the EMAC instructions since the type of source address determines
   if it is a EMAC-load instruction if the EA is mode 2-5, otherwise it
   is a non-load EMAC instruction and the bits mean register Ry.
   A similar case exists for the movem instructions where the register
   mask is interpreted differently for different EAs.  */

static bfd_boolean
m68k_valid_ea (char code, int val)
{
  int mode, mask;
#define M(n0,n1,n2,n3,n4,n5,n6,n70,n71,n72,n73,n74) \
  (n0 | n1 << 1 | n2 << 2 | n3 << 3 | n4 << 4 | n5 << 5 | n6 << 6 \
   | n70 << 7 | n71 << 8 | n72 << 9 | n73 << 10 | n74 << 11)

  switch (code)
    {
    case '*':
      mask = M (1,1,1,1,1,1,1,1,1,1,1,1);
      break;
    case '~':
      mask = M (0,0,1,1,1,1,1,1,1,0,0,0);
      break;
    case '%':
      mask = M (1,1,1,1,1,1,1,1,1,0,0,0);
      break;
    case ';':
      mask = M (1,0,1,1,1,1,1,1,1,1,1,1);
      break;
    case '@':
      mask = M (1,0,1,1,1,1,1,1,1,1,1,0);
      break;
    case '!':
      mask = M (0,0,1,0,0,1,1,1,1,1,1,0);
      break;
    case '&':
      mask = M (0,0,1,0,0,1,1,1,1,0,0,0);
      break;
    case '$':
      mask = M (1,0,1,1,1,1,1,1,1,0,0,0);
      break;
    case '?':
      mask = M (1,0,1,0,0,1,1,1,1,0,0,0);
      break;
    case '/':
      mask = M (1,0,1,0,0,1,1,1,1,1,1,0);
      break;
    case '|':
      mask = M (0,0,1,0,0,1,1,1,1,1,1,0);
      break;
    case '>':
      mask = M (0,0,1,0,1,1,1,1,1,0,0,0);
      break;
    case '<':
      mask = M (0,0,1,1,0,1,1,1,1,1,1,0);
      break;
    case 'm':
      mask = M (1,1,1,1,1,0,0,0,0,0,0,0);
      break;
    case 'n':
      mask = M (0,0,0,0,0,1,0,0,0,1,0,0);
      break;
    case 'o':
      mask = M (0,0,0,0,0,0,1,1,1,0,1,1);
      break;
    case 'p':
      mask = M (1,1,1,1,1,1,0,0,0,0,0,0);
      break;
    case 'q':
      mask = M (1,0,1,1,1,1,0,0,0,0,0,0);
      break;
    case 'v':
      mask = M (1,0,1,1,1,1,0,1,1,0,0,0);
      break;
    case 'b':
      mask = M (1,0,1,1,1,1,0,0,0,1,0,0);
      break;
    case 'w':
      mask = M (0,0,1,1,1,1,0,0,0,1,0,0);
      break;
    case 'y':
      mask = M (0,0,1,0,0,1,0,0,0,0,0,0);
      break;
    case 'z':
      mask = M (0,0,1,0,0,1,0,0,0,1,0,0);
      break;
    case '4':
      mask = M (0,0,1,1,1,1,0,0,0,0,0,0);
      break;
    default:
      abort ();
    }
#undef M

  mode = (val >> 3) & 7;
  if (mode == 7)
    mode += val & 7;
  return (mask & (1 << mode)) != 0;
}

/* Print a base register REGNO and displacement DISP, on INFO->STREAM.
   REGNO = -1 for pc, -2 for none (suppressed).  */

static void
print_base (int regno, bfd_vma disp, disassemble_info *info)
{
  if (regno == -1)
    {
      (*info->fprintf_func) (info->stream, "%%pc@(");
      (*info->print_address_func) (disp, info);
    }
  else
    {
      char buf[50];

      if (regno == -2)
        (*info->fprintf_func) (info->stream, "@(");
      else if (regno == -3)
        (*info->fprintf_func) (info->stream, "%%zpc@(");
      else
        (*info->fprintf_func) (info->stream, "%s@(", reg_names[regno]);

      sprintf_vma (buf, disp);
      (*info->fprintf_func) (info->stream, "%s", buf);
    }
}

/* Print an indexed argument.  The base register is BASEREG (-1 for pc).
   P points to extension word, in buffer.
   ADDR is the nominal core address of that extension word.  */

static unsigned char *
print_indexed (int basereg,
               unsigned char *p,
               bfd_vma addr,
               disassemble_info *info)
{
  int word;
  static const char *const scales[] = { "", ":2", ":4", ":8" };
  bfd_vma base_disp;
  bfd_vma outer_disp;
  char buf[40];
  char vmabuf[50];

  word = NEXTWORD (p);

  /* Generate the text for the index register.
     Where this will be output is not yet determined.  */
  snprintf(buf, sizeof(buf), "%s:%c%s",
           reg_names[(word >> 12) & 0xf],
           (word & 0x800) ? 'l' : 'w',
           scales[(word >> 9) & 3]);

  /* Handle the 68000 style of indexing.  */

  if ((word & 0x100) == 0)
    {
      base_disp = word & 0xff;
      if ((base_disp & 0x80) != 0)
        base_disp -= 0x100;
      if (basereg == -1)
        base_disp += addr;
      print_base (basereg, base_disp, info);
      (*info->fprintf_func) (info->stream, ",%s)", buf);
      return p;
    }

  /* Handle the generalized kind.  */
  /* First, compute the displacement to add to the base register.  */
  if (word & 0200)
    {
      if (basereg == -1)
        basereg = -3;
      else
        basereg = -2;
    }
  if (word & 0100)
    buf[0] = '\0';
  base_disp = 0;
  switch ((word >> 4) & 3)
    {
    case 2:
      base_disp = NEXTWORD (p);
      break;
    case 3:
      base_disp = NEXTLONG (p);
    }
  if (basereg == -1)
    base_disp += addr;

  /* Handle single-level case (not indirect).  */
  if ((word & 7) == 0)
    {
      print_base (basereg, base_disp, info);
      if (buf[0] != '\0')
        (*info->fprintf_func) (info->stream, ",%s", buf);
      (*info->fprintf_func) (info->stream, ")");
      return p;
    }

  /* Two level.  Compute displacement to add after indirection.  */
  outer_disp = 0;
  switch (word & 3)
    {
    case 2:
      outer_disp = NEXTWORD (p);
      break;
    case 3:
      outer_disp = NEXTLONG (p);
    }

  print_base (basereg, base_disp, info);
  if ((word & 4) == 0 && buf[0] != '\0')
    {
      (*info->fprintf_func) (info->stream, ",%s", buf);
      buf[0] = '\0';
    }
  sprintf_vma (vmabuf, outer_disp);
  (*info->fprintf_func) (info->stream, ")@(%s", vmabuf);
  if (buf[0] != '\0')
    (*info->fprintf_func) (info->stream, ",%s", buf);
  (*info->fprintf_func) (info->stream, ")");

  return p;
}

/* Returns number of bytes "eaten" by the operand, or
   return -1 if an invalid operand was found, or -2 if
   an opcode table error was found.
   ADDR is the pc for this arg to be relative to.  */

static int
print_insn_arg (const char *d,
                unsigned char *buffer,
                unsigned char *p0,
                bfd_vma addr,
                disassemble_info *info)
{
  int val = 0;
  int place = d[1];
  unsigned char *p = p0;
  int regno;
  const char *regname;
  unsigned char *p1;
  double flval;
  int flt_p;
  bfd_signed_vma disp;
  unsigned int uval;

  switch (*d)
    {
    case 'c':		/* Cache identifier.  */
      {
        static const char *const cacheFieldName[] = { "nc", "dc", "ic", "bc" };
        val = fetch_arg (buffer, place, 2, info);
        (*info->fprintf_func) (info->stream, "%s", cacheFieldName[val]);
        break;
      }

    case 'a':		/* Address register indirect only. Cf. case '+'.  */
      {
        (*info->fprintf_func)
          (info->stream,
           "%s@",
           reg_names[fetch_arg (buffer, place, 3, info) + 8]);
        break;
      }

    case '_':		/* 32-bit absolute address for move16.  */
      {
        uval = NEXTULONG (p);
        (*info->print_address_func) (uval, info);
        break;
      }

    case 'C':
      (*info->fprintf_func) (info->stream, "%%ccr");
      break;

    case 'S':
      (*info->fprintf_func) (info->stream, "%%sr");
      break;

    case 'U':
      (*info->fprintf_func) (info->stream, "%%usp");
      break;

    case 'E':
      (*info->fprintf_func) (info->stream, "%%acc");
      break;

    case 'G':
      (*info->fprintf_func) (info->stream, "%%macsr");
      break;

    case 'H':
      (*info->fprintf_func) (info->stream, "%%mask");
      break;

    case 'J':
      {
        /* FIXME: There's a problem here, different m68k processors call the
           same address different names. This table can't get it right
           because it doesn't know which processor it's disassembling for.  */
        static const struct { const char *name; int value; } names[]
          = {{"%sfc", 0x000}, {"%dfc", 0x001}, {"%cacr", 0x002},
             {"%tc",  0x003}, {"%itt0",0x004}, {"%itt1", 0x005},
             {"%dtt0",0x006}, {"%dtt1",0x007}, {"%buscr",0x008},
             {"%usp", 0x800}, {"%vbr", 0x801}, {"%caar", 0x802},
             {"%msp", 0x803}, {"%isp", 0x804},
             {"%flashbar", 0xc04}, {"%rambar", 0xc05}, /* mcf528x added these.  */

             /* Should we be calling this psr like we do in case 'Y'?  */
             {"%mmusr",0x805},

             {"%urp", 0x806}, {"%srp", 0x807}, {"%pcr", 0x808}};

        val = fetch_arg (buffer, place, 12, info);
        for (regno = sizeof names / sizeof names[0] - 1; regno >= 0; regno--)
          if (names[regno].value == val)
            {
              (*info->fprintf_func) (info->stream, "%s", names[regno].name);
              break;
            }
        if (regno < 0)
          (*info->fprintf_func) (info->stream, "%d", val);
      }
      break;

    case 'Q':
      val = fetch_arg (buffer, place, 3, info);
      /* 0 means 8, except for the bkpt instruction... */
      if (val == 0 && d[1] != 's')
        val = 8;
      (*info->fprintf_func) (info->stream, "#%d", val);
      break;

    case 'x':
      val = fetch_arg (buffer, place, 3, info);
      /* 0 means -1.  */
      if (val == 0)
        val = -1;
      (*info->fprintf_func) (info->stream, "#%d", val);
      break;

    case 'M':
      if (place == 'h')
        {
          static const char *const scalefactor_name[] = { "<<", ">>" };
          val = fetch_arg (buffer, place, 1, info);
          (*info->fprintf_func) (info->stream, "%s", scalefactor_name[val]);
        }
      else
        {
          val = fetch_arg (buffer, place, 8, info);
          if (val & 0x80)
            val = val - 0x100;
          (*info->fprintf_func) (info->stream, "#%d", val);
        }
      break;

    case 'T':
      val = fetch_arg (buffer, place, 4, info);
      (*info->fprintf_func) (info->stream, "#%d", val);
      break;

    case 'D':
      (*info->fprintf_func) (info->stream, "%s",
                             reg_names[fetch_arg (buffer, place, 3, info)]);
      break;

    case 'A':
      (*info->fprintf_func)
        (info->stream, "%s",
         reg_names[fetch_arg (buffer, place, 3, info) + 010]);
      break;

    case 'R':
      (*info->fprintf_func)
        (info->stream, "%s",
         reg_names[fetch_arg (buffer, place, 4, info)]);
      break;

    case 'r':
      regno = fetch_arg (buffer, place, 4, info);
      if (regno > 7)
        (*info->fprintf_func) (info->stream, "%s@", reg_names[regno]);
      else
        (*info->fprintf_func) (info->stream, "@(%s)", reg_names[regno]);
      break;

    case 'F':
      (*info->fprintf_func)
        (info->stream, "%%fp%d",
         fetch_arg (buffer, place, 3, info));
      break;

    case 'O':
      val = fetch_arg (buffer, place, 6, info);
      if (val & 0x20)
        (*info->fprintf_func) (info->stream, "%s", reg_names[val & 7]);
      else
        (*info->fprintf_func) (info->stream, "%d", val);
      break;

    case '+':
      (*info->fprintf_func)
        (info->stream, "%s@+",
         reg_names[fetch_arg (buffer, place, 3, info) + 8]);
      break;

    case '-':
      (*info->fprintf_func)
        (info->stream, "%s@-",
         reg_names[fetch_arg (buffer, place, 3, info) + 8]);
      break;

    case 'k':
      if (place == 'k')
        (*info->fprintf_func)
          (info->stream, "{%s}",
           reg_names[fetch_arg (buffer, place, 3, info)]);
      else if (place == 'C')
        {
          val = fetch_arg (buffer, place, 7, info);
          if (val > 63)		/* This is a signed constant.  */
            val -= 128;
          (*info->fprintf_func) (info->stream, "{#%d}", val);
        }
      else
        return -2;
      break;

    case '#':
    case '^':
      p1 = buffer + (*d == '#' ? 2 : 4);
      if (place == 's')
        val = fetch_arg (buffer, place, 4, info);
      else if (place == 'C')
        val = fetch_arg (buffer, place, 7, info);
      else if (place == '8')
        val = fetch_arg (buffer, place, 3, info);
      else if (place == '3')
        val = fetch_arg (buffer, place, 8, info);
      else if (place == 'b')
        val = NEXTBYTE (p1);
      else if (place == 'w' || place == 'W')
        val = NEXTWORD (p1);
      else if (place == 'l')
        val = NEXTLONG (p1);
      else
        return -2;
      (*info->fprintf_func) (info->stream, "#%d", val);
      break;

    case 'B':
      if (place == 'b')
        disp = NEXTBYTE (p);
      else if (place == 'B')
        disp = COERCE_SIGNED_CHAR (buffer[1]);
      else if (place == 'w' || place == 'W')
        disp = NEXTWORD (p);
      else if (place == 'l' || place == 'L' || place == 'C')
        disp = NEXTLONG (p);
      else if (place == 'g')
        {
          disp = NEXTBYTE (buffer);
          if (disp == 0)
            disp = NEXTWORD (p);
          else if (disp == -1)
            disp = NEXTLONG (p);
        }
      else if (place == 'c')
        {
          if (buffer[1] & 0x40)		/* If bit six is one, long offset.  */
            disp = NEXTLONG (p);
          else
            disp = NEXTWORD (p);
        }
      else
        return -2;

      (*info->print_address_func) (addr + disp, info);
      break;

    case 'd':
      val = NEXTWORD (p);
      (*info->fprintf_func)
        (info->stream, "%s@(%d)",
         reg_names[fetch_arg (buffer, place, 3, info) + 8], val);
      break;

    case 's':
      (*info->fprintf_func) (info->stream, "%s",
                             fpcr_names[fetch_arg (buffer, place, 3, info)]);
      break;

    case 'e':
      val = fetch_arg(buffer, place, 2, info);
      (*info->fprintf_func) (info->stream, "%%acc%d", val);
      break;

    case 'g':
      val = fetch_arg(buffer, place, 1, info);
      (*info->fprintf_func) (info->stream, "%%accext%s", val==0 ? "01" : "23");
      break;

    case 'i':
      val = fetch_arg(buffer, place, 2, info);
      if (val == 1)
        (*info->fprintf_func) (info->stream, "<<");
      else if (val == 3)
        (*info->fprintf_func) (info->stream, ">>");
      else
        return -1;
      break;

    case 'I':
      /* Get coprocessor ID... */
      val = fetch_arg (buffer, 'd', 3, info);

      if (val != 1)				/* Unusual coprocessor ID?  */
        (*info->fprintf_func) (info->stream, "(cpid=%d) ", val);
      break;

    case '4':
    case '*':
    case '~':
    case '%':
    case ';':
    case '@':
    case '!':
    case '$':
    case '?':
    case '/':
    case '&':
    case '|':
    case '<':
    case '>':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'v':
    case 'b':
    case 'w':
    case 'y':
    case 'z':
      if (place == 'd')
        {
          val = fetch_arg (buffer, 'x', 6, info);
          val = ((val & 7) << 3) + ((val >> 3) & 7);
        }
      else
        val = fetch_arg (buffer, 's', 6, info);

      /* If the <ea> is invalid for *d, then reject this match.  */
      if (!m68k_valid_ea (*d, val))
        return -1;

      /* Get register number assuming address register.  */
      regno = (val & 7) + 8;
      regname = reg_names[regno];
      switch (val >> 3)
      {
        case 0:
          (*info->fprintf_func) (info->stream, "%s", reg_names[val]);
          break;

        case 1:
          (*info->fprintf_func) (info->stream, "%s", regname);
          break;

        case 2:
          (*info->fprintf_func) (info->stream, "%s@", regname);
          break;

        case 3:
          (*info->fprintf_func) (info->stream, "%s@+", regname);
          break;

        case 4:
          (*info->fprintf_func) (info->stream, "%s@-", regname);
          break;

        case 5:
          val = NEXTWORD (p);
          (*info->fprintf_func) (info->stream, "%s@(%d)", regname, val);
          break;

        case 6:
          p = print_indexed (regno, p, addr, info);
          break;

        case 7:
          switch (val & 7)
            {
            case 0:
              val = NEXTWORD (p);
              (*info->print_address_func) (val,info);
              break;

            case 1:
              uval = NEXTULONG (p);
              (*info->print_address_func) (uval, info);
              break;

            case 2:
              val = NEXTWORD (p);
              (*info->fprintf_func) (info->stream, "%%pc@(");
              (*info->print_address_func) (addr + val, info);
              (*info->fprintf_func) (info->stream, ")");
              break;

            case 3:
              p = print_indexed (-1, p, addr, info);
              break;

            case 4:
              flt_p = 1;	/* Assume it's a float... */
              switch (place)
              {
                case 'b':
                  val = NEXTBYTE (p);
                  flt_p = 0;
                  break;

                case 'w':
                  val = NEXTWORD (p);
                  flt_p = 0;
                  break;

                case 'l':
                  val = NEXTLONG (p);
                  flt_p = 0;
                  break;

                case 'f':
                  NEXTSINGLE (flval, p);
                  break;

                case 'F':
                  NEXTDOUBLE (flval, p);
                  break;

                case 'x':
                  NEXTEXTEND (flval, p);
                  break;

                case 'p':
                  flval = NEXTPACKED (p);
                  break;

                default:
                  return -1;
              }
              if (flt_p)	/* Print a float? */
                (*info->fprintf_func) (info->stream, "#%g", flval);
              else
                (*info->fprintf_func) (info->stream, "#%d", val);
              break;

            default:
              return -1;
            }
        }

      /* If place is '/', then this is the case of the mask bit for
         mac/emac loads. Now that the arg has been printed, grab the
         mask bit and if set, add a '&' to the arg.  */
      if (place == '/')
        {
          val = fetch_arg (buffer, place, 1, info);
          if (val)
            info->fprintf_func (info->stream, "&");
        }
      break;

    case 'L':
    case 'l':
        if (place == 'w')
          {
            char doneany;
            p1 = buffer + 2;
            val = NEXTWORD (p1);
            /* Move the pointer ahead if this point is farther ahead
               than the last.  */
            p = p1 > p ? p1 : p;
            if (val == 0)
              {
                (*info->fprintf_func) (info->stream, "#0");
                break;
              }
            if (*d == 'l')
              {
                int newval = 0;

                for (regno = 0; regno < 16; ++regno)
                  if (val & (0x8000 >> regno))
                    newval |= 1 << regno;
                val = newval;
              }
            val &= 0xffff;
            doneany = 0;
            for (regno = 0; regno < 16; ++regno)
              if (val & (1 << regno))
                {
                  int first_regno;

                  if (doneany)
                    (*info->fprintf_func) (info->stream, "/");
                  doneany = 1;
                  (*info->fprintf_func) (info->stream, "%s", reg_names[regno]);
                  first_regno = regno;
                  while (val & (1 << (regno + 1)))
                    ++regno;
                  if (regno > first_regno)
                    (*info->fprintf_func) (info->stream, "-%s",
                                           reg_names[regno]);
                }
          }
        else if (place == '3')
          {
            /* `fmovem' insn.  */
            char doneany;
            val = fetch_arg (buffer, place, 8, info);
            if (val == 0)
              {
                (*info->fprintf_func) (info->stream, "#0");
                break;
              }
            if (*d == 'l')
              {
                int newval = 0;

                for (regno = 0; regno < 8; ++regno)
                  if (val & (0x80 >> regno))
                    newval |= 1 << regno;
                val = newval;
              }
            val &= 0xff;
            doneany = 0;
            for (regno = 0; regno < 8; ++regno)
              if (val & (1 << regno))
                {
                  int first_regno;
                  if (doneany)
                    (*info->fprintf_func) (info->stream, "/");
                  doneany = 1;
                  (*info->fprintf_func) (info->stream, "%%fp%d", regno);
                  first_regno = regno;
                  while (val & (1 << (regno + 1)))
                    ++regno;
                  if (regno > first_regno)
                    (*info->fprintf_func) (info->stream, "-%%fp%d", regno);
                }
          }
        else if (place == '8')
          {
            /* fmoveml for FP status registers.  */
            (*info->fprintf_func) (info->stream, "%s",
                                   fpcr_names[fetch_arg (buffer, place, 3,
                                                         info)]);
          }
        else
          return -2;
      break;

    case 'X':
      place = '8';
      /* fall through */
    case 'Y':
    case 'Z':
    case 'W':
    case '0':
    case '1':
    case '2':
    case '3':
      {
        int reg = fetch_arg (buffer, place, 5, info);
        const char *name = 0;

        switch (reg)
          {
          case 2: name = "%tt0"; break;
          case 3: name = "%tt1"; break;
          case 0x10: name = "%tc"; break;
          case 0x11: name = "%drp"; break;
          case 0x12: name = "%srp"; break;
          case 0x13: name = "%crp"; break;
          case 0x14: name = "%cal"; break;
          case 0x15: name = "%val"; break;
          case 0x16: name = "%scc"; break;
          case 0x17: name = "%ac"; break;
          case 0x18: name = "%psr"; break;
          case 0x19: name = "%pcsr"; break;
          case 0x1c:
          case 0x1d:
            {
              int break_reg = ((buffer[3] >> 2) & 7);

              (*info->fprintf_func)
                (info->stream, reg == 0x1c ? "%%bad%d" : "%%bac%d",
                 break_reg);
            }
            break;
          default:
            (*info->fprintf_func) (info->stream, "<mmu register %d>", reg);
          }
        if (name)
          (*info->fprintf_func) (info->stream, "%s", name);
      }
      break;

    case 'f':
      {
        int fc = fetch_arg (buffer, place, 5, info);

        if (fc == 1)
          (*info->fprintf_func) (info->stream, "%%dfc");
        else if (fc == 0)
          (*info->fprintf_func) (info->stream, "%%sfc");
        else
          /* xgettext:c-format */
          (*info->fprintf_func) (info->stream, "<function code %d>", fc);
      }
      break;

    case 'V':
      (*info->fprintf_func) (info->stream, "%%val");
      break;

    case 't':
      {
        int level = fetch_arg (buffer, place, 3, info);

        (*info->fprintf_func) (info->stream, "%d", level);
      }
      break;

    case 'u':
      {
        short is_upper = 0;
        int reg = fetch_arg (buffer, place, 5, info);

        if (reg & 0x10)
          {
            is_upper = 1;
            reg &= 0xf;
          }
        (*info->fprintf_func) (info->stream, "%s%s",
                               reg_half_names[reg],
                               is_upper ? "u" : "l");
      }
      break;

    default:
      return -2;
    }

  return p - p0;
}

/* Try to match the current instruction to best and if so, return the
   number of bytes consumed from the instruction stream, else zero.  */

static int
match_insn_m68k (bfd_vma memaddr,
                 disassemble_info * info,
                 const struct m68k_opcode * best,
                 struct private * priv)
{
  unsigned char *save_p;
  unsigned char *p;
  const char *d;

  bfd_byte *buffer = priv->the_buffer;
  fprintf_function save_printer = info->fprintf_func;
  void (* save_print_address) (bfd_vma, struct disassemble_info *)
    = info->print_address_func;

  /* Point at first word of argument data,
     and at descriptor for first argument.  */
  p = buffer + 2;

  /* Figure out how long the fixed-size portion of the instruction is.
     The only place this is stored in the opcode table is
     in the arguments--look for arguments which specify fields in the 2nd
     or 3rd words of the instruction.  */
  for (d = best->args; *d; d += 2)
  {
     /* I don't think it is necessary to be checking d[0] here;
        I suspect all this could be moved to the case statement below.  */
     if (d[0] == '#')
     {
        if (d[1] == 'l' && p - buffer < 6)
           p = buffer + 6;
        else if (p - buffer < 4 && d[1] != 'C' && d[1] != '8')
           p = buffer + 4;
     }

     if ((d[0] == 'L' || d[0] == 'l') && d[1] == 'w' && p - buffer < 4)
        p = buffer + 4;

     switch (d[1])
     {
     case '1':
     case '2':
     case '3':
     case '7':
     case '8':
     case '9':
     case 'i':
        if (p - buffer < 4)
           p = buffer + 4;
        break;
     case '4':
     case '5':
     case '6':
        if (p - buffer < 6)
           p = buffer + 6;
        break;
     default:
        break;
     }
  }

  /* pflusha is an exceptions.  It takes no arguments but is two words
     long.  Recognize it by looking at the lower 16 bits of the mask.  */
  if (p - buffer < 4 && (best->match & 0xFFFF) != 0)
     p = buffer + 4;

  /* lpstop is another exception.  It takes a one word argument but is
     three words long.  */
  if (p - buffer < 6
      && (best->match & 0xffff) == 0xffff
      && best->args[0] == '#'
      && best->args[1] == 'w')
  {
     /* Copy the one word argument into the usual location for a one
        word argument, to simplify printing it.  We can get away with
        this because we know exactly what the second word is, and we
        aren't going to print anything based on it.  */
     p = buffer + 6;
     fetch_data(info, p);
     buffer[2] = buffer[4];
     buffer[3] = buffer[5];
  }

  fetch_data(info, p);

  d = best->args;

  save_p = p;
  info->print_address_func = dummy_print_address;
  info->fprintf_func = dummy_printer;

  /* We scan the operands twice.  The first time we don't print anything,
     but look for errors.  */
  for (; *d; d += 2)
  {
      int eaten = print_insn_arg (d, buffer, p, memaddr + (p - buffer), info);

      if (eaten >= 0)
        p += eaten;
      else if (eaten == -1)
        {
          info->fprintf_func = save_printer;
          info->print_address_func = save_print_address;
          return 0;
        }
      else
        {
          info->fprintf_func (info->stream,
                              /* xgettext:c-format */
                              "<internal error in opcode table: %s %s>\n",
                              best->name,  best->args);
          info->fprintf_func = save_printer;
          info->print_address_func = save_print_address;
          return 2;
        }
  }

  p = save_p;
  info->fprintf_func = save_printer;
  info->print_address_func = save_print_address;

  d = best->args;

  info->fprintf_func (info->stream, "%s", best->name);

  if (*d)
    info->fprintf_func (info->stream, " ");

  while (*d)
  {
      p += print_insn_arg (d, buffer, p, memaddr + (p - buffer), info);
      d += 2;

      if (*d && *(d - 2) != 'I' && *d != 'k')
        info->fprintf_func (info->stream, ",");
    }

  return p - buffer;
}

/* Print the m68k instruction at address MEMADDR in debugged memory,
   on INFO->STREAM.  Returns length of the instruction, in bytes.  */

int
print_insn_m68k (bfd_vma memaddr, disassemble_info *info)
{
  int i;
  const char *d;
  unsigned int arch_mask;
  struct private priv;
  bfd_byte *buffer = priv.the_buffer;
  int major_opcode;
  static int numopcodes[16];
  static const struct m68k_opcode **opcodes[16];
  int val;

  if (!opcodes[0])
  {
      /* Speed up the matching by sorting the opcode
         table on the upper four bits of the opcode.  */
      const struct m68k_opcode **opc_pointer[16];

      /* First count how many opcodes are in each of the sixteen buckets.  */
      for (i = 0; i < m68k_numopcodes; i++)
        numopcodes[(m68k_opcodes[i].opcode >> 28) & 15]++;

      /* Then create a sorted table of pointers
         that point into the unsorted table.  */
      opc_pointer[0] = malloc (sizeof (struct m68k_opcode *)
                               * m68k_numopcodes);
      opcodes[0] = opc_pointer[0];

      for (i = 1; i < 16; i++)
        {
          opc_pointer[i] = opc_pointer[i - 1] + numopcodes[i - 1];
          opcodes[i] = opc_pointer[i];
        }

      for (i = 0; i < m68k_numopcodes; i++)
        *opc_pointer[(m68k_opcodes[i].opcode >> 28) & 15]++ = &m68k_opcodes[i];
    }

  info->private_data = (PTR) &priv;
  /* Tell objdump to use two bytes per chunk
     and six bytes per line for displaying raw data.  */
  info->bytes_per_chunk = 2;
  info->bytes_per_line = 6;
  info->display_endian = BFD_ENDIAN_BIG;
  priv.max_fetched = priv.the_buffer;
  priv.insn_start = memaddr;

  if (sigsetjmp(priv.bailout, 0) != 0) {
      /* Error return.  */
      return -1;
  }

  switch (info->mach)
    {
    default:
    case 0:
      arch_mask = (unsigned int) -1;
      break;
    case bfd_mach_m68000:
      arch_mask = m68000|m68881|m68851;
      break;
    case bfd_mach_m68008:
      arch_mask = m68008|m68881|m68851;
      break;
    case bfd_mach_m68010:
      arch_mask = m68010|m68881|m68851;
      break;
    case bfd_mach_m68020:
      arch_mask = m68020|m68881|m68851;
      break;
    case bfd_mach_m68030:
      arch_mask = m68030|m68881|m68851;
      break;
    case bfd_mach_m68040:
      arch_mask = m68040|m68881|m68851;
      break;
    case bfd_mach_m68060:
      arch_mask = m68060|m68881|m68851;
      break;
    case bfd_mach_mcf5200:
      arch_mask = mcfisa_a;
      break;
    case bfd_mach_mcf521x:
    case bfd_mach_mcf528x:
      arch_mask = mcfisa_a|mcfhwdiv|mcfisa_aa|mcfusp|mcfemac;
      break;
    case bfd_mach_mcf5206e:
      arch_mask = mcfisa_a|mcfhwdiv|mcfmac;
      break;
    case bfd_mach_mcf5249:
      arch_mask = mcfisa_a|mcfhwdiv|mcfemac;
      break;
    case bfd_mach_mcf5307:
      arch_mask = mcfisa_a|mcfhwdiv|mcfmac;
      break;
    case bfd_mach_mcf5407:
      arch_mask = mcfisa_a|mcfhwdiv|mcfisa_b|mcfmac;
      break;
    case bfd_mach_mcf547x:
    case bfd_mach_mcf548x:
    case bfd_mach_mcfv4e:
      arch_mask = mcfisa_a|mcfhwdiv|mcfisa_b|mcfusp|cfloat|mcfemac;
      break;
    }

  fetch_data(info, buffer + 2);
  major_opcode = (buffer[0] >> 4) & 15;

  for (i = 0; i < numopcodes[major_opcode]; i++)
  {
      const struct m68k_opcode *opc = opcodes[major_opcode][i];
      unsigned long opcode = opc->opcode;
      unsigned long match = opc->match;

      /*
       * Check whether the 16bits of opcode match the pattern, when AND'ed with the
       * match bits.  Seems like a much more readable solution is possible....
       */
      if (((0xff & buffer[0] & (match >> 24)) == (0xff & (opcode >> 24)))
          && ((0xff & buffer[1] & (match >> 16)) == (0xff & (opcode >> 16)))
          /* Only fetch the next two bytes if we need to.  */
          && (((0xffff & match) == 0)
              ||
              (fetch_data(info, buffer + 4)
               && ((0xff & buffer[2] & (match >> 8)) == (0xff & (opcode >> 8)))
               && ((0xff & buffer[3] & match) == (0xff & opcode)))
              )
          && (opc->arch & arch_mask) != 0)
        {
          /* Don't use for printout the variants of divul and divsl
             that have the same register number in two places.
             The more general variants will match instead.  */
          for (d = opc->args; *d; d += 2)
            if (d[1] == 'D')
              break;

          /* Don't use for printout the variants of most floating
             point coprocessor instructions which use the same
             register number in two places, as above.  */
          if (*d == '\0')
            for (d = opc->args; *d; d += 2)
              if (d[1] == 't')
                break;

          /* Don't match fmovel with more than one register;
             wait for fmoveml.  */
          if (*d == '\0')
            {
              for (d = opc->args; *d; d += 2)
                {
                  if (d[0] == 's' && d[1] == '8')
                    {
                      val = fetch_arg (buffer, d[1], 3, info);
                      if ((val & (val - 1)) != 0)
                        break;
                    }
                }
            }

          /* Don't match FPU insns with non-default coprocessor ID.  */
          if (*d == '\0')
            {
              for (d = opc->args; *d; d += 2)
                {
                  if (d[0] == 'I')
                    {
                      val = fetch_arg (buffer, 'd', 3, info);
                      if (val != 1)
                        break;
                    }
                }
            }

          if (*d == '\0')
             if ((val = match_insn_m68k (memaddr, info, opc, & priv)))
                return val;
        }
  }

  /* Handle undefined instructions.  */
  info->fprintf_func (info->stream, "0%o", (buffer[0] << 8) + buffer[1]);
  return 2;
}


/* **** End of m68k-dis.c */

#include "m68k-opc.h"

/* **** floatformat.c from sourceware.org CVS 2005-08-14.  */
/* IEEE floating point support routines, for GDB, the GNU Debugger.
   Copyright (C) 1991, 1994, 1999, 2000, 2003 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses/>.  */

/* This is needed to pick up the NAN macro on some systems.  */
//#define _GNU_SOURCE

#ifndef INFINITY
#ifdef HUGE_VAL
#define INFINITY HUGE_VAL
#else
#define INFINITY (1.0 / 0.0)
#endif
#endif

#ifndef NAN
#define NAN (0.0 / 0.0)
#endif

static unsigned long get_field (const unsigned char *,
                                enum floatformat_byteorders,
                                unsigned int,
                                unsigned int,
                                unsigned int);
static int floatformat_always_valid (const struct floatformat *fmt,
                                     const char *from);

static int
floatformat_always_valid (const struct floatformat *fmt ATTRIBUTE_UNUSED,
                          const char *from ATTRIBUTE_UNUSED)
{
  return 1;
}

/* The odds that CHAR_BIT will be anything but 8 are low enough that I'm not
   going to bother with trying to muck around with whether it is defined in
   a system header, what we do if not, etc.  */
#define FLOATFORMAT_CHAR_BIT 8

/* floatformats for IEEE single and double, big and little endian.  */
const struct floatformat floatformat_ieee_single_big =
{
  floatformat_big, 32, 0, 1, 8, 127, 255, 9, 23,
  floatformat_intbit_no,
  "floatformat_ieee_single_big",
  floatformat_always_valid
};
const struct floatformat floatformat_ieee_single_little =
{
  floatformat_little, 32, 0, 1, 8, 127, 255, 9, 23,
  floatformat_intbit_no,
  "floatformat_ieee_single_little",
  floatformat_always_valid
};
const struct floatformat floatformat_ieee_double_big =
{
  floatformat_big, 64, 0, 1, 11, 1023, 2047, 12, 52,
  floatformat_intbit_no,
  "floatformat_ieee_double_big",
  floatformat_always_valid
};
const struct floatformat floatformat_ieee_double_little =
{
  floatformat_little, 64, 0, 1, 11, 1023, 2047, 12, 52,
  floatformat_intbit_no,
  "floatformat_ieee_double_little",
  floatformat_always_valid
};

/* floatformat for IEEE double, little endian byte order, with big endian word
   ordering, as on the ARM.  */

const struct floatformat floatformat_ieee_double_littlebyte_bigword =
{
  floatformat_littlebyte_bigword, 64, 0, 1, 11, 1023, 2047, 12, 52,
  floatformat_intbit_no,
  "floatformat_ieee_double_littlebyte_bigword",
  floatformat_always_valid
};

static int floatformat_i387_ext_is_valid (const struct floatformat *fmt, const char *from);

static int
floatformat_i387_ext_is_valid (const struct floatformat *fmt, const char *from)
{
  /* In the i387 double-extended format, if the exponent is all ones,
     then the integer bit must be set.  If the exponent is neither 0
     nor ~0, the intbit must also be set.  Only if the exponent is
     zero can it be zero, and then it must be zero.  */
  unsigned long exponent, int_bit;
  const unsigned char *ufrom = (const unsigned char *) from;

  exponent = get_field (ufrom, fmt->byteorder, fmt->totalsize,
                        fmt->exp_start, fmt->exp_len);
  int_bit = get_field (ufrom, fmt->byteorder, fmt->totalsize,
                       fmt->man_start, 1);

  if ((exponent == 0) != (int_bit == 0))
    return 0;
  else
    return 1;
}

const struct floatformat floatformat_i387_ext =
{
  floatformat_little, 80, 0, 1, 15, 0x3fff, 0x7fff, 16, 64,
  floatformat_intbit_yes,
  "floatformat_i387_ext",
  floatformat_i387_ext_is_valid
};
const struct floatformat floatformat_m68881_ext =
{
  /* Note that the bits from 16 to 31 are unused.  */
  floatformat_big, 96, 0, 1, 15, 0x3fff, 0x7fff, 32, 64,
  floatformat_intbit_yes,
  "floatformat_m68881_ext",
  floatformat_always_valid
};
const struct floatformat floatformat_i960_ext =
{
  /* Note that the bits from 0 to 15 are unused.  */
  floatformat_little, 96, 16, 17, 15, 0x3fff, 0x7fff, 32, 64,
  floatformat_intbit_yes,
  "floatformat_i960_ext",
  floatformat_always_valid
};
const struct floatformat floatformat_m88110_ext =
{
  floatformat_big, 80, 0, 1, 15, 0x3fff, 0x7fff, 16, 64,
  floatformat_intbit_yes,
  "floatformat_m88110_ext",
  floatformat_always_valid
};
const struct floatformat floatformat_m88110_harris_ext =
{
  /* Harris uses raw format 128 bytes long, but the number is just an ieee
     double, and the last 64 bits are wasted. */
  floatformat_big,128, 0, 1, 11,  0x3ff,  0x7ff, 12, 52,
  floatformat_intbit_no,
  "floatformat_m88110_ext_harris",
  floatformat_always_valid
};
const struct floatformat floatformat_arm_ext_big =
{
  /* Bits 1 to 16 are unused.  */
  floatformat_big, 96, 0, 17, 15, 0x3fff, 0x7fff, 32, 64,
  floatformat_intbit_yes,
  "floatformat_arm_ext_big",
  floatformat_always_valid
};
const struct floatformat floatformat_arm_ext_littlebyte_bigword =
{
  /* Bits 1 to 16 are unused.  */
  floatformat_littlebyte_bigword, 96, 0, 17, 15, 0x3fff, 0x7fff, 32, 64,
  floatformat_intbit_yes,
  "floatformat_arm_ext_littlebyte_bigword",
  floatformat_always_valid
};
const struct floatformat floatformat_ia64_spill_big =
{
  floatformat_big, 128, 0, 1, 17, 65535, 0x1ffff, 18, 64,
  floatformat_intbit_yes,
  "floatformat_ia64_spill_big",
  floatformat_always_valid
};
const struct floatformat floatformat_ia64_spill_little =
{
  floatformat_little, 128, 0, 1, 17, 65535, 0x1ffff, 18, 64,
  floatformat_intbit_yes,
  "floatformat_ia64_spill_little",
  floatformat_always_valid
};
const struct floatformat floatformat_ia64_quad_big =
{
  floatformat_big, 128, 0, 1, 15, 16383, 0x7fff, 16, 112,
  floatformat_intbit_no,
  "floatformat_ia64_quad_big",
  floatformat_always_valid
};
const struct floatformat floatformat_ia64_quad_little =
{
  floatformat_little, 128, 0, 1, 15, 16383, 0x7fff, 16, 112,
  floatformat_intbit_no,
  "floatformat_ia64_quad_little",
  floatformat_always_valid
};

/* Extract a field which starts at START and is LEN bits long.  DATA and
   TOTAL_LEN are the thing we are extracting it from, in byteorder ORDER.  */
static unsigned long
get_field (const unsigned char *data, enum floatformat_byteorders order,
           unsigned int total_len, unsigned int start, unsigned int len)
{
  unsigned long result;
  unsigned int cur_byte;
  int cur_bitshift;

  /* Start at the least significant part of the field.  */
  cur_byte = (start + len) / FLOATFORMAT_CHAR_BIT;
  if (order == floatformat_little)
    cur_byte = (total_len / FLOATFORMAT_CHAR_BIT) - cur_byte - 1;
  cur_bitshift =
    ((start + len) % FLOATFORMAT_CHAR_BIT) - FLOATFORMAT_CHAR_BIT;
  result = *(data + cur_byte) >> (-cur_bitshift);
  cur_bitshift += FLOATFORMAT_CHAR_BIT;
  if (order == floatformat_little)
    ++cur_byte;
  else
    --cur_byte;

  /* Move towards the most significant part of the field.  */
  while ((unsigned int) cur_bitshift < len)
    {
      if (len - cur_bitshift < FLOATFORMAT_CHAR_BIT)
        /* This is the last byte; zero out the bits which are not part of
           this field.  */
        result |=
          (unsigned long)(*(data + cur_byte)
                          & ((1 << (len - cur_bitshift)) - 1))
            << cur_bitshift;
      else
        result |= (unsigned long)*(data + cur_byte) << cur_bitshift;
      cur_bitshift += FLOATFORMAT_CHAR_BIT;
      if (order == floatformat_little)
        ++cur_byte;
      else
        --cur_byte;
    }
  return result;
}

/* Convert from FMT to a double.
   FROM is the address of the extended float.
   Store the double in *TO.  */

void
floatformat_to_double (const struct floatformat *fmt,
                       const char *from, double *to)
{
  const unsigned char *ufrom = (const unsigned char *)from;
  double dto;
  long exponent;
  unsigned long mant;
  unsigned int mant_bits, mant_off;
  int mant_bits_left;
  int special_exponent;		/* It's a NaN, denorm or zero */

  exponent = get_field (ufrom, fmt->byteorder, fmt->totalsize,
                        fmt->exp_start, fmt->exp_len);

  /* If the exponent indicates a NaN, we don't have information to
     decide what to do.  So we handle it like IEEE, except that we
     don't try to preserve the type of NaN.  FIXME.  */
  if ((unsigned long) exponent == fmt->exp_nan)
    {
      int nan;

      mant_off = fmt->man_start;
      mant_bits_left = fmt->man_len;
      nan = 0;
      while (mant_bits_left > 0)
        {
          mant_bits = MIN(mant_bits_left, 32);

          if (get_field (ufrom, fmt->byteorder, fmt->totalsize,
                         mant_off, mant_bits) != 0)
            {
              /* This is a NaN.  */
              nan = 1;
              break;
            }

          mant_off += mant_bits;
          mant_bits_left -= mant_bits;
        }

      /* On certain systems (such as GNU/Linux), the use of the
         INFINITY macro below may generate a warning that can not be
         silenced due to a bug in GCC (PR preprocessor/11931).  The
         preprocessor fails to recognise the __extension__ keyword in
         conjunction with the GNU/C99 extension for hexadecimal
         floating point constants and will issue a warning when
         compiling with -pedantic.  */
      if (nan)
        dto = NAN;
      else
        dto = INFINITY;

      if (get_field (ufrom, fmt->byteorder, fmt->totalsize, fmt->sign_start, 1))
        dto = -dto;

      *to = dto;

      return;
    }

  mant_bits_left = fmt->man_len;
  mant_off = fmt->man_start;
  dto = 0.0;

  special_exponent = exponent == 0 || (unsigned long) exponent == fmt->exp_nan;

  /* Don't bias zero's, denorms or NaNs.  */
  if (!special_exponent)
    exponent -= fmt->exp_bias;

  /* Build the result algebraically.  Might go infinite, underflow, etc;
     who cares. */

  /* If this format uses a hidden bit, explicitly add it in now.  Otherwise,
     increment the exponent by one to account for the integer bit.  */

  if (!special_exponent)
    {
      if (fmt->intbit == floatformat_intbit_no)
        dto = ldexp (1.0, exponent);
      else
        exponent++;
    }

  while (mant_bits_left > 0)
    {
      mant_bits = MIN(mant_bits_left, 32);

      mant = get_field (ufrom, fmt->byteorder, fmt->totalsize,
                         mant_off, mant_bits);

      /* Handle denormalized numbers.  FIXME: What should we do for
         non-IEEE formats?  */
      if (exponent == 0 && mant != 0)
        dto += ldexp ((double)mant,
                      (- fmt->exp_bias
                       - mant_bits
                       - (mant_off - fmt->man_start)
                       + 1));
      else
        dto += ldexp ((double)mant, exponent - mant_bits);
      if (exponent != 0)
        exponent -= mant_bits;
      mant_off += mant_bits;
      mant_bits_left -= mant_bits;
    }

  /* Negate it if negative.  */
  if (get_field (ufrom, fmt->byteorder, fmt->totalsize, fmt->sign_start, 1))
    dto = -dto;
  *to = dto;
}

static void put_field (unsigned char *, enum floatformat_byteorders,
                       unsigned int,
                       unsigned int,
                       unsigned int,
                       unsigned long);

/* Set a field which starts at START and is LEN bits long.  DATA and
   TOTAL_LEN are the thing we are extracting it from, in byteorder ORDER.  */
static void
put_field (unsigned char *data, enum floatformat_byteorders order,
           unsigned int total_len, unsigned int start, unsigned int len,
           unsigned long stuff_to_put)
{
  unsigned int cur_byte;
  int cur_bitshift;

  /* Start at the least significant part of the field.  */
  cur_byte = (start + len) / FLOATFORMAT_CHAR_BIT;
  if (order == floatformat_little)
    cur_byte = (total_len / FLOATFORMAT_CHAR_BIT) - cur_byte - 1;
  cur_bitshift =
    ((start + len) % FLOATFORMAT_CHAR_BIT) - FLOATFORMAT_CHAR_BIT;
  *(data + cur_byte) &=
    ~(((1 << ((start + len) % FLOATFORMAT_CHAR_BIT)) - 1) << (-cur_bitshift));
  *(data + cur_byte) |=
    (stuff_to_put & ((1 << FLOATFORMAT_CHAR_BIT) - 1)) << (-cur_bitshift);
  cur_bitshift += FLOATFORMAT_CHAR_BIT;
  if (order == floatformat_little)
    ++cur_byte;
  else
    --cur_byte;

  /* Move towards the most significant part of the field.  */
  while ((unsigned int) cur_bitshift < len)
    {
      if (len - cur_bitshift < FLOATFORMAT_CHAR_BIT)
        {
          /* This is the last byte.  */
          *(data + cur_byte) &=
            ~((1 << (len - cur_bitshift)) - 1);
          *(data + cur_byte) |= (stuff_to_put >> cur_bitshift);
        }
      else
        *(data + cur_byte) = ((stuff_to_put >> cur_bitshift)
                              & ((1 << FLOATFORMAT_CHAR_BIT) - 1));
      cur_bitshift += FLOATFORMAT_CHAR_BIT;
      if (order == floatformat_little)
        ++cur_byte;
      else
        --cur_byte;
    }
}

/* The converse: convert the double *FROM to an extended float
   and store where TO points.  Neither FROM nor TO have any alignment
   restrictions.  */

void
floatformat_from_double (const struct floatformat *fmt,
                         const double *from, char *to)
{
  double dfrom;
  int exponent;
  double mant;
  unsigned int mant_bits, mant_off;
  int mant_bits_left;
  unsigned char *uto = (unsigned char *)to;

  dfrom = *from;
  memset (uto, 0, fmt->totalsize / FLOATFORMAT_CHAR_BIT);

  /* If negative, set the sign bit.  */
  if (dfrom < 0)
    {
      put_field (uto, fmt->byteorder, fmt->totalsize, fmt->sign_start, 1, 1);
      dfrom = -dfrom;
    }

  if (dfrom == 0)
    {
      /* 0.0.  */
      return;
    }

  if (dfrom != dfrom)
    {
      /* NaN.  */
      put_field (uto, fmt->byteorder, fmt->totalsize, fmt->exp_start,
                 fmt->exp_len, fmt->exp_nan);
      /* Be sure it's not infinity, but NaN value is irrelevant.  */
      put_field (uto, fmt->byteorder, fmt->totalsize, fmt->man_start,
                 32, 1);
      return;
    }

  if (dfrom + dfrom == dfrom)
    {
      /* This can only happen for an infinite value (or zero, which we
         already handled above).  */
      put_field (uto, fmt->byteorder, fmt->totalsize, fmt->exp_start,
                 fmt->exp_len, fmt->exp_nan);
      return;
    }

  mant = frexp (dfrom, &exponent);
  if (exponent + fmt->exp_bias - 1 > 0)
    put_field (uto, fmt->byteorder, fmt->totalsize, fmt->exp_start,
               fmt->exp_len, exponent + fmt->exp_bias - 1);
  else
    {
      /* Handle a denormalized number.  FIXME: What should we do for
         non-IEEE formats?  */
      put_field (uto, fmt->byteorder, fmt->totalsize, fmt->exp_start,
                 fmt->exp_len, 0);
      mant = ldexp (mant, exponent + fmt->exp_bias - 1);
    }

  mant_bits_left = fmt->man_len;
  mant_off = fmt->man_start;
  while (mant_bits_left > 0)
    {
      unsigned long mant_long;
      mant_bits = mant_bits_left < 32 ? mant_bits_left : 32;

      mant *= 4294967296.0;
      mant_long = (unsigned long)mant;
      mant -= mant_long;

      /* If the integer bit is implicit, and we are not creating a
         denormalized number, then we need to discard it.  */
      if ((unsigned int) mant_bits_left == fmt->man_len
          && fmt->intbit == floatformat_intbit_no
          && exponent + fmt->exp_bias - 1 > 0)
        {
          mant_long &= 0x7fffffff;
          mant_bits -= 1;
        }
      else if (mant_bits < 32)
        {
          /* The bits we want are in the most significant MANT_BITS bits of
             mant_long.  Move them to the least significant.  */
          mant_long >>= 32 - mant_bits;
        }

      put_field (uto, fmt->byteorder, fmt->totalsize,
                 mant_off, mant_bits, mant_long);
      mant_off += mant_bits;
      mant_bits_left -= mant_bits;
    }
}

/* Return non-zero iff the data at FROM is a valid number in format FMT.  */

int
floatformat_is_valid (const struct floatformat *fmt, const char *from)
{
  return fmt->is_valid (fmt, from);
}


#ifdef IEEE_DEBUG

/* This is to be run on a host which uses IEEE floating point.  */

void
ieee_test (double n)
{
  double result;

  floatformat_to_double (&floatformat_ieee_double_little, (char *) &n,
                         &result);
  if ((n != result && (! isnan (n) || ! isnan (result)))
      || (n < 0 && result >= 0)
      || (n >= 0 && result < 0))
    printf ("Differ(to): %.20g -> %.20g\n", n, result);

  floatformat_from_double (&floatformat_ieee_double_little, &n,
                           (char *) &result);
  if ((n != result && (! isnan (n) || ! isnan (result)))
      || (n < 0 && result >= 0)
      || (n >= 0 && result < 0))
    printf ("Differ(from): %.20g -> %.20g\n", n, result);

#if 0
  {
    char exten[16];

    floatformat_from_double (&floatformat_m68881_ext, &n, exten);
    floatformat_to_double (&floatformat_m68881_ext, exten, &result);
    if (n != result)
      printf ("Differ(to+from): %.20g -> %.20g\n", n, result);
  }
#endif

#if IEEE_DEBUG > 1
  /* This is to be run on a host which uses 68881 format.  */
  {
    long double ex = *(long double *)exten;
    if (ex != n)
      printf ("Differ(from vs. extended): %.20g\n", n);
  }
#endif
}

int
main (void)
{
  ieee_test (0.0);
  ieee_test (0.5);
  ieee_test (256.0);
  ieee_test (0.12345);
  ieee_test (234235.78907234);
  ieee_test (-512.0);
  ieee_test (-0.004321);
  ieee_test (1.2E-70);
  ieee_test (1.2E-316);
  ieee_test (4.9406564584124654E-324);
  ieee_test (- 4.9406564584124654E-324);
  ieee_test (- 0.0);
  ieee_test (- INFINITY);
  ieee_test (- NAN);
  ieee_test (INFINITY);
  ieee_test (NAN);
  return 0;
}
#endif
/* **** End of floatformat.c  */
