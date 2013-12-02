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

/*
 * This file defines main data-structure for term conversion 
 *
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
  YAP_STRING_TERM = 0x1000, // joint with other flags that define possible values 
  YAP_STRING_DIFF = 0x2000,  // difference list
  YAP_STRING_NCHARS= 0x4000,  // size of input/result
  YAP_STRING_TRUNC= 0x8000  // truncate on maximum size of input/result
}
seq_type_t;

#define YAP_TYPE_MASK 0x1FFF

typedef union {
  Float  f;
  Int  i;
  MP_INT *b;
  const char *c;
  const wchar_t *w;
  Atom   a;
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

int Yap_CVT_Text( seq_tv_t *inp, seq_tv_t *out USES_REGS);

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
  inp.type = YAP_STRING_CHARS;
  inp.mod = mod;
  out.type = mod_to_type(mod PASS_REGS);
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

static inline Atom
Yap_NWCharsToAtom( const wchar_t *s, size_t len USES_REGS )
{
  seq_tv_t inp, out;

  inp.val.w = s;
  inp.sz = len;
  inp.type = YAP_STRING_WCHARS|YAP_STRING_NCHARS;
  out.type = YAP_STRING_ATOM;
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
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}


Yap_TextToUTF8(Term t0 USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING|YAP_STRING_CODES|YAP_STRING_ATOMS|YAP_STRING_ATOM|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
  out.type = YAP_STRING_CODES;
  out.encoding = YAP_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term
Yap_WCharsToListOfCodes(const wchar_t *s USES_REGS)
{
  seq_tv_t inp, out;
  inp.val.w = s;
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
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_STRING;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

