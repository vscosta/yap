/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapMirror.c						 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Term conversion C implemented support			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#if SIZEOF_WCHAR_T == 2
#define CHARCODE_MAX 0xffff
#else
#define CHARCODE_MAX 0x10ffff
#endif

/*
 * This file defines main data-structure for text conversion and
 * mirroring
 */

#include "pl-utf8.h"

// standard strings

typedef enum {
  YAP_STRING_STRING = 0x1,
  YAP_STRING_CODES = 0x2,
  YAP_STRING_ATOMS = 0x4,
  YAP_STRING_ATOMS_CODES = 0x6,
  YAP_STRING_CHARS = 0x8,
  YAP_STRING_WCHARS = 0x10,
  YAP_STRING_ATOM = 0x20,
  YAP_STRING_INT = 0x40,
  YAP_STRING_FLOAT = 0x80,
  YAP_STRING_BIG = 0x100,
  YAP_STRING_LITERAL = 0x200,
  YAP_STRING_LENGTH = 0x400,
  YAP_STRING_NTH = 0x800,
} enum_seq_type_t;


#define  YAP_STRING_TERM 0x1000 // joint with other flags that define possible values 
#define  YAP_STRING_DIFF  0x2000  // difference list
#define  YAP_STRING_NCHARS 0x4000  // size of input/result
#define  YAP_STRING_TRUNC 0x8000  // truncate on maximum size of input/result

typedef UInt seq_type_t;

#define YAP_TYPE_MASK 0x1FFF

typedef union {
  Float  f;
  Int  i;
  MP_INT *b;
  const char *c;
  const wchar_t *w;
  Atom   a;
  size_t l;
  int d;
  Term   t;// depends on other flags
}
seq_val_t;

typedef struct text_cvt {
  seq_type_t type;
  seq_val_t val;
  Term      mod; // optional
  size_t     sz; // fixed sz, or -1
  Term      dif; // diff-list, usually TermNil
  size_t    max; // max_size
} seq_tv_t;

typedef enum internal_encoding {
  YAP_CHAR,
  YAP_UTF8,
  YAP_WCHAR
} encoding_t;

// string construction
#ifdef HR
static inline Term
init_tstring( USES_REGS1 ) { 
  Term t = AbsAppl(HR);

  HR[0] = (CELL)FunctorString;
  return t;
}

static inline char *
buf_from_tstring( CELL *p ) { 
  char *out = (char *)(p + 2);
  return out;
}

static inline void
close_tstring( char *p USES_REGS ) { 
  CELL *szp = HR+1;
  HR = (CELL *)ALIGN_YAPTYPE( p ,CELL);
  *szp = (HR - szp)-1;
  *HR++ = EndSpecials;
}
#endif

// string type depends on current module
static inline seq_type_t
mod_to_type( Term mod USES_REGS )
{
  unsigned int flags = Yap_GetModuleEntry(mod)->flags;
  if (!(flags & DBLQ_MASK)) {
    return YAP_STRING_CODES;
  } else if (flags & DBLQ_STRING) {
    return YAP_STRING_STRING;
  } else if (flags &  DBLQ_CHARS) {
    return YAP_STRING_CHARS;
  }
  return YAP_STRING_ATOM;
}

// the routines

extern int write_Text( void *inp, seq_tv_t *out, encoding_t enc, int minimal, size_t leng  USES_REGS);
extern int   Yap_CVT_Text( seq_tv_t *inp, seq_tv_t *out USES_REGS);
extern void *Yap_Concat_Text( int n,  seq_tv_t inp[], seq_tv_t *out USES_REGS);
extern void *Yap_Splice_Text( int n,  size_t cuts[], seq_tv_t *inp, encoding_t encv[], seq_tv_t outv[] USES_REGS);

// user friendly interface

static inline size_t
Yap_AtomicToLength(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_CODES|YAP_STRING_ATOMS|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_LENGTH;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_AtomicToListOfAtoms(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_AtomicToListOfCodes(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_AtomicToAtom(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline size_t
Yap_AtomToLength(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM;
  out.type = YAP_STRING_LENGTH;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return (size_t)(-1L);
  return out.val.l;
}

static inline Term
Yap_AtomToListOfAtoms(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_AtomSWIToListOfAtoms(Term t0 USES_REGS)
{
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM|YAP_STRING_STRING|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = YAP_STRING_ATOMS;
  
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}


static inline Term
Yap_AtomToListOfCodes(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_AtomToNumber(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM;
  out.type = YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_AtomToString(Term t0 USES_REGS)
{
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM;
  out.type = YAP_STRING_STRING;
  
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_AtomSWIToString(Term t0 USES_REGS)
{
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM|YAP_STRING_STRING|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = YAP_STRING_STRING;
  
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_AtomicToString(Term t0 USES_REGS)
{
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_STRING;
  
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_AtomicToTDQ(Term t0, Term mod USES_REGS)
{
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = mod_to_type(mod PASS_REGS);
  
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_CharsToAtom( const char *s USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.sz = 0;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term
Yap_CharsToListOfAtoms( const char *s USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.sz = 0;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_CharsToListOfCodes( const char *s USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.sz = 0;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_CharsToString( const char *s USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.sz = 0;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_STRING;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_CharsToTDQ( const char *s, Term mod USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.sz = 0;
  inp.type = YAP_STRING_CHARS;
  inp.mod = mod;
  out.type = mod_to_type(mod PASS_REGS);
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_ListOfAtomsToAtom(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOMS;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return (Atom)NULL;
  return out.val.a;
}

static inline Term
Yap_ListOfAtomsToNumber(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOMS;
  out.type = YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_ListOfAtomsToString(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOMS;
  out.type = YAP_STRING_STRING;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_ListOfCodesToAtom(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_CODES;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return (Atom)NULL;
  return out.val.a;
}

static inline Term
Yap_ListOfCodesToNumber(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_CODES;
  out.type = YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_ListOfCodesToString(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_CODES;
  out.type = YAP_STRING_STRING;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_ListToAtom(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term
Yap_ListToAtomic(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_ListToNumber(Term t0 USES_REGS)
{
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}


static inline Term
Yap_ListToString(Term t0 USES_REGS)
{
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = YAP_STRING_STRING;
  
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}


static inline Term
Yap_ListSWIToString(Term t0 USES_REGS)
{
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_ATOMS_CODES|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_STRING;
  
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}


static inline Term
YapListToTDQ(Term t0, Term mod USES_REGS)
{
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = mod_to_type(mod PASS_REGS);
  
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_NCharsToAtom( const char *s, size_t len USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.sz = len;
  inp.type = YAP_STRING_CHARS|YAP_STRING_NCHARS;
  out.type = YAP_STRING_ATOM;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term
Yap_NCharsToListOfAtoms( const char *s, size_t len USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.sz = len;
  inp.type = YAP_STRING_CHARS|YAP_STRING_NCHARS;
  out.type = YAP_STRING_ATOMS;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_NCharsToListOfCodes( const char *s, size_t len USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.sz = len;
  inp.type = YAP_STRING_CHARS|YAP_STRING_NCHARS;
  out.type = YAP_STRING_CODES;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_NCharsToString( const char *s, size_t len USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.sz = len;
  inp.type = YAP_STRING_CHARS|YAP_STRING_NCHARS;
  out.type = YAP_STRING_STRING;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_NCharsToTDQ( const char *s, size_t len, Term mod USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.c = s;
  inp.type = YAP_STRING_CHARS|YAP_STRING_NCHARS;
  inp.sz = len;
  inp.mod = mod;
  out.type = mod_to_type(mod PASS_REGS);
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_NumberToAtom(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term
Yap_NumberToListOfAtoms(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_NumberToListOfCodes(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_NumberToString(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_STRING;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_NWCharsToAtom( const wchar_t *s, size_t len USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.w = s;
  inp.sz = len;
  inp.type = YAP_STRING_WCHARS|YAP_STRING_NCHARS;
  out.type = YAP_STRING_ATOM;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term
Yap_NWCharsToListOfAtoms( const wchar_t *s, size_t len USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.w = s;
  inp.sz = len;
  inp.type = YAP_STRING_WCHARS|YAP_STRING_NCHARS;
  out.type = YAP_STRING_ATOMS;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_NWCharsToListOfCodes( const wchar_t *s, size_t len USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.w = s;
  inp.sz = len;
  inp.type = YAP_STRING_WCHARS|YAP_STRING_NCHARS;
  out.type = YAP_STRING_CODES;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_NWCharsToString( const wchar_t *s, size_t len USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.w = s;
  inp.sz = len;
  inp.type = YAP_STRING_WCHARS|YAP_STRING_NCHARS;
  out.type = YAP_STRING_STRING;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_StringToAtom(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.sz = 0;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Atom
Yap_StringSWIToAtom(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.sz = 0;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline size_t
Yap_StringToAtomic(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.sz = 0;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING;
  out.type = YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline size_t
Yap_StringToLength(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.sz = 0;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING;
  out.type = YAP_STRING_LENGTH;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return (size_t)(-1L);
  return out.val.l;
}

static inline size_t
Yap_StringToListOfAtoms(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.sz = 0;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline size_t
Yap_StringSWIToListOfAtoms(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.sz = 0;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline size_t
Yap_StringToListOfCodes(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.sz = 0;
  inp.type = YAP_STRING_STRING;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline size_t
Yap_StringSWIToListOfCodes(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_ATOMS_CODES|YAP_STRING_TERM;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_StringToNumber(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.sz = 0;
  inp.type = YAP_STRING_STRING;
  out.type = YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_WCharsToListOfCodes(const wchar_t *s USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.sz = 0;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_WCharsToTDQ( wchar_t *s, Term mod USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  inp.sz = 0;
  inp.mod = mod;
  out.type = mod_to_type(mod PASS_REGS);
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_WCharsToString(const wchar_t *s USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.sz = 0;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_STRING;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom
Yap_ConcatAtoms(Term t1, Term t2 USES_REGS)
{
  seq_tv_t inpv[2], out;
  inpv[0].val.t = t1;
  inpv[0].type = YAP_STRING_ATOM;
  inpv[0].sz = 0;
  inpv[1].val.t = t2;
  inpv[1].type = YAP_STRING_ATOM;
  inpv[1].sz = 0;
  out.type = YAP_STRING_ATOM;
  if (!Yap_Concat_Text(2, inpv, &out PASS_REGS))
    return (Atom)NULL;
  return out.val.a;
}

static inline Atom
Yap_ConcatAtomics(Term t1, Term t2 USES_REGS)
{
  seq_tv_t inpv[2], out;
  inpv[0].val.t = t1;
  inpv[0].type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  inpv[0].sz = 0;
  inpv[1].val.t = t2;
  inpv[1].type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  inpv[1].sz = 0;
  out.type = YAP_STRING_ATOM;
  if (!Yap_Concat_Text(2, inpv, &out PASS_REGS))
    return (Atom)NULL;
  return out.val.a;
}

static inline Term
Yap_ConcatStrings(Term t1, Term t2 USES_REGS)
{
  seq_tv_t inpv[2], out;
  inpv[0].val.t = t1;
  inpv[0].type = YAP_STRING_STRING;
  inpv[0].sz = 0;
  inpv[1].val.t = t2;
  inpv[1].type = YAP_STRING_STRING;
  inpv[1].sz = 0;
  out.type = YAP_STRING_STRING;
  if (!Yap_Concat_Text(2, inpv, &out PASS_REGS))
    return 0L;
  return out.val.t;
}


static inline Atom
Yap_SpliceAtom(Term t1, Atom ats[], size_t cut, size_t max USES_REGS)
{
  seq_tv_t outv[2], inp;
  size_t cuts[2];
  cuts[0] = cut;
  cuts[1] = max;
  inp.type = YAP_STRING_ATOM;
  inp.val.t = t1;
  inp.sz = 0;
  outv[0].type = YAP_STRING_ATOM;
  outv[0].sz = 0;
  outv[1].type = YAP_STRING_ATOM;
  outv[1].sz = 0;
  if (!Yap_Splice_Text(2, cuts, &inp, (encoding_t *)NULL, outv PASS_REGS))
    return (Atom)NULL;
  ats[0] = outv[0].val.a;
  ats[1] = outv[1].val.a;
  return ats[0];
}

static inline Atom
Yap_SubtractHeadAtom(Term t1, Term th USES_REGS)
{
  seq_tv_t outv[2], inp;
  inp.type = YAP_STRING_ATOM;
  inp.val.t = t1;
  inp.sz = 0;
  outv[0].type = YAP_STRING_ATOM;
  outv[0].val.t = th;
  outv[0].sz = 0;
  outv[1].type = YAP_STRING_ATOM;
  outv[1].val.t = 0;
  outv[1].sz = 0;
  if (!Yap_Splice_Text(2, (size_t *)NULL, &inp, (encoding_t *)NULL, outv PASS_REGS))
    return (Atom)NULL;
  return outv[1].val.a;
}


static inline Atom
Yap_SubtractTailAtom(Term t1, Term th USES_REGS)
{
  seq_tv_t outv[2], inp;
  inp.type = YAP_STRING_ATOM;
  inp.val.t = t1;
  inp.sz = 0;
  outv[0].type = YAP_STRING_ATOM;
  outv[0].val.t = 0;
  outv[0].sz = 0;
  outv[1].type = YAP_STRING_ATOM;
  outv[1].val.t = th;
  if (!Yap_Splice_Text(2, (size_t *)NULL, &inp, (encoding_t *)NULL, outv PASS_REGS))
    return (Atom)NULL;
  return outv[0].val.a;
}

static inline Term
Yap_SpliceString(Term t1, Term ts[], size_t cut, size_t max USES_REGS)
{
  seq_tv_t outv[2], inp;
  size_t cuts[2];
  inp.type = YAP_STRING_STRING;
  inp.val.t = t1;
  inp.sz = 0;
  outv[0].type = YAP_STRING_STRING;
  outv[1].type = YAP_STRING_STRING;
  outv[1].sz = 0;
  cuts[0] = cut;
  cuts[1] = max;
  if (!Yap_Splice_Text(2, cuts, &inp, (encoding_t *)NULL, outv PASS_REGS))
    return 0L;
  ts[0] = outv[0].val.t;
  ts[1] = outv[1].val.t;
  return ts[0];
}

static inline Term
Yap_SubtractHeadString(Term t1, Term th USES_REGS)
{
  seq_tv_t outv[2], inp;
  inp.type = YAP_STRING_STRING;
  inp.val.t = t1;
  inp.sz = 0;
  outv[0].type = YAP_STRING_STRING;
  outv[0].val.t = th;
  outv[0].sz = 0;
  outv[1].type = YAP_STRING_STRING;
  outv[1].val.t = 0;
  outv[1].sz = 0;
  if (!Yap_Splice_Text(2, (size_t *)NULL, &inp, (encoding_t *)NULL, outv PASS_REGS))
    return 0L;
  return outv[1].val.t;
}

static inline Term
Yap_SubtractTailString(Term t1, Term th USES_REGS)
{
  seq_tv_t outv[2], inp;
  inp.type = YAP_STRING_STRING;
  inp.val.t = t1;
  inp.sz = 0;
  outv[0].type = YAP_STRING_STRING;
  outv[0].val.t = 0;
  outv[0].sz = 0;
  outv[1].type = YAP_STRING_STRING;
  outv[1].val.t = th;
  if (!Yap_Splice_Text(2, (size_t *)NULL, &inp, (encoding_t *)NULL, outv PASS_REGS))
    return 0L;
  return outv[0].val.t;
}


