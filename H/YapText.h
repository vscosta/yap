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
 * File:		YapMirror.c *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Term conversion C implemented support			 *
 *									 *
 *************************************************************************/
#ifndef YAP_TEXT_H
#define YAP_TEXT_H
#include "Yap.h"



#include "YapUTF8.h"

#ifndef Yap_Min
#define Yap_Min(x, y) (x < y ? x : y)

#endif

/****************** character definition table **************************/

#define NUMBER_OF_CHARS 256
extern char *Yap_chtype;

#define Yap_strlen(s) strlen((void *)(s))

typedef enum {
  BG = 0,  /* initial state */
  UC = 1,  /* Upper case */
  UL = 2,  /* Underline */
  LC = 3,  /* Lower case */
  NU = 4,  /* digit */
  QT = 5,  /* single quote */
  DC = 6,  /* double quote */
  SY = 7,  /* Symbol character */
  SL = 8,  /* Solo character */
  BK = 9,  /* Brackets & friends */
  BS = 10, /* Blank */
  EF = 11, /* End of File marker */
  CC = 12  /* comment,char %	*/
} char_kind_t;

extern char_kind_t Yap_chtype0[];

#define Yap_chtype (Yap_chtype0 + 1)

char_kind_t Yap_wide_chtype(int ch);

INLINE_ONLY char_kind_t Yap_wide_chtype(int ch) {
  if (ch < 256)
    return Yap_chtype[ch];
  switch (utf8proc_category(ch)) {
  case UTF8PROC_CATEGORY_CN: /**< Other, not assigned */
    return BG;
  case UTF8PROC_CATEGORY_LU: /**< Letter, uppercase */
    return UC;
  case UTF8PROC_CATEGORY_LL: /**< Letter, lowercase */
    return LC;
  case UTF8PROC_CATEGORY_LT: /**< Letter, titlecase */
    return UC;
  case UTF8PROC_CATEGORY_LM: /**< Letter, modifier */
    return LC;
  case UTF8PROC_CATEGORY_LO: /**< Letter, other */
    return LC;
  case UTF8PROC_CATEGORY_MN: /**< Mark, nonspacing */
    return BG;
  case UTF8PROC_CATEGORY_MC: /**< Mark, spacing combining */
    return BK;
  case UTF8PROC_CATEGORY_ME: /**< Mark, enclosing */
    return BK;
  case UTF8PROC_CATEGORY_ND: /**< Number, decimal digit */
    return NU;
  case UTF8PROC_CATEGORY_NL: /**< Number, letter */
    return NU;
  case UTF8PROC_CATEGORY_NO: /**< Number, other */
    return NU;
  case UTF8PROC_CATEGORY_PC: /**< Punctuation, connector */
    return SL;
  case UTF8PROC_CATEGORY_PD: /**< Punctuation, dash */
    return SY;
  case UTF8PROC_CATEGORY_PS: /**< Punctuation, open */
    return BK;
  case UTF8PROC_CATEGORY_PE: /**< Punctuation, close */
    return BK;
  case UTF8PROC_CATEGORY_PI: /**< Punctuation, initial quote */
    return QT;
  case UTF8PROC_CATEGORY_PF: /**< Punctuation, final quote */
    return QT;
  case UTF8PROC_CATEGORY_PO: /**< Punctuation, other */
    return SL;
  case UTF8PROC_CATEGORY_SM: /**< Symbol, math */
    return SY;
  case UTF8PROC_CATEGORY_SC: /**< Symbol, currency */
    return SY;
  case UTF8PROC_CATEGORY_SK: /**< Symbol, modifier

   unsure in YAP, let's assume a,c us treated as aç
 */
    return LC;
  case UTF8PROC_CATEGORY_SO: /**< Symbol, other */
    return SL;
  case UTF8PROC_CATEGORY_ZS: /**< Separator, space */
    return BS;
  case UTF8PROC_CATEGORY_ZL: /**< Separator, line */
    return BS;
  case UTF8PROC_CATEGORY_ZP: /**< Separator, paragraph */
    return BS;
  case UTF8PROC_CATEGORY_CC: /**< Other, control */
    return BG;
  case UTF8PROC_CATEGORY_CF: /**< Other, format */
    return BG;
  case UTF8PROC_CATEGORY_CS: /**< Other, surrogate */
    return BG;
  case UTF8PROC_CATEGORY_CO: /**< Other, private use */
    return BG;
  }
  return BS;
}

INLINE_ONLY char_kind_t chtype(Int ch) {
  if (ch < NUMBER_OF_CHARS)
    return Yap_chtype[ch];
  return Yap_wide_chtype(ch);
}

#ifndef __ANDROID__
#define __android_log_print(...)
#endif

extern const char *Yap_tokText(void *tokptr);
/// represent  token *_tokptr_ in string s, maxlength is sz-1
///
/// conversion is based on token type.
extern Term Yap_tokRep(void *tokptrXS);

// standard strings

typedef enum
{
  YAP_STRING_STRING = 0x1,          /// target is a string term
  YAP_STRING_CODES = 0x2,           /// target is a list of integer codes
  YAP_STRING_ATOMS = 0x4,           /// target is a list of kength-1 atom
  YAP_STRING_ATOMS_CODES = 0x6,     /// targt is list of atoms or codes
  YAP_STRING_CHARS = 0x8,           /// target is a buffer, with byte-sized units
  YAP_STRING_WCHARS = 0x10,         /// target is a buffer of wide chars
  YAP_STRING_ATOM = 0x20,           /// tarfet is an ayom
  YAP_STRING_INT = 0x40,            /// target is an integer term
  YAP_STRING_FLOAT = 0x80,          /// target is a floar term
  YAP_STRING_BIG = 0x100,           /// target is an big num term
  YAP_STRING_DATUM = 0x200,         /// associated with previous 3, use actual object if type, not tern
  YAP_STRING_LENGTH = 0x400,        /// input: length is fixed; output: return integer with length
  YAP_STRING_NTH = 0x800,           ///  input: ignored; output: nth char
  YAP_STRING_TERM = 0x1000,         // Generic term, if nothing else given
  YAP_STRING_DIFF = 0x2000,         // difference list
  YAP_STRING_NCHARS = 0x4000,       // size of input/result
  YAP_STRING_TRUNC = 0x8000,        // truncate on maximum size of input/result
  YAP_STRING_WQ = 0x10000,          // output with write_quote
  YAP_STRING_WC = 0x20000,          // output with write_canonical
  YAP_STRING_WITH_BUFFER = 0x40000, // output on existing buffer
  YAP_STRING_MALLOC = 0x80000,      // output on malloced buffer
  YAP_STRING_UPCASE = 0x100000,     // output on malloced buffer
  YAP_STRING_DOWNCASE = 0x200000,   // output on malloced buffer
  YAP_STRING_IN_TMP = 0x400000,     // temporary space has been allocated
  YAP_STRING_OUTPUT_TERM = 0x800000, // when we're not sure
  YAP_STRING_PREFER_LIST = 0x1000000 // when we're not sure
} enum_seq_type_t;

typedef UInt seq_type_t;

#define YAP_TYPE_MASK 0x0FFF

typedef union {
  Float f;
  Int i;
  MP_INT *b;
  const char *c0;
  const wchar_t *w0;
  char *c;
  unsigned char *uc;
  const unsigned char *uc0;
  wchar_t *w;
  Atom a;
  size_t l;
  int d;
  Term t; // depends on other flags
} seq_val_t;

typedef struct text_cvt {
  seq_type_t type;
  seq_val_t val;
  Term mod;   // optional
  Term dif;   // diff-list, usually TermNil
  size_t max; // max_size
  encoding_t enc;
} seq_tv_t;

// string construction
#ifdef HR
static inline Term init_tstring(USES_REGS1) {
  Term t = AbsAppl(HR);

  HR[0] = (CELL)FunctorString;
  return t;
}

static inline unsigned char *buf_from_tstring(CELL *p) {
  unsigned char *out = (unsigned char *)(p + 2);
  return out;
}

static inline void close_tstring(unsigned char *p USES_REGS) {
  CELL *szp = HR + 1;
  HR = (CELL *)ALIGN_BY_TYPE(p, CELL);
  *szp = (HR - szp) - 1;
  *HR++ = EndSpecials;
}
#endif

// string type depends on current module
static inline seq_type_t mod_to_type(Term mod USES_REGS) {

  // see pl-incl.h
  unsigned int flags = Yap_GetModuleEntry(mod)->flags;
  if (flags & DBLQ_ATOM) {
    return YAP_STRING_ATOM | YAP_STRING_OUTPUT_TERM;
  } else if (flags & DBLQ_STRING) {
    return YAP_STRING_STRING;
  } else if (flags & DBLQ_CHARS) {
    return YAP_STRING_ATOMS;
  }
  return YAP_STRING_CODES;
}

// string type depends on current module
static inline seq_type_t mod_to_atype(Term mod USES_REGS) {

  // see pl-incl.h
  unsigned int flags = Yap_GetModuleEntry(mod)->flags;
  if (flags & SNGQ_ATOM) {
    return YAP_STRING_ATOM | YAP_STRING_OUTPUT_TERM;
  } else if (flags & SNGQ_STRING) {
    return YAP_STRING_STRING;
  } else if (flags & SNGQ_CHARS) {
    return YAP_STRING_ATOMS;
  }
  return YAP_STRING_CODES;
}

// string type depends on current module
static inline seq_type_t mod_to_bqtype(Term mod USES_REGS) {
  Term t2;
  if ((t2 = GLOBAL_Flags[BACK_QUOTES_FLAG].at)) {
    if (t2 == TermString) {
    return YAP_STRING_STRING;
    } else if (t2 == TermAtom) {
      return YAP_STRING_ATOM | YAP_STRING_OUTPUT_TERM;
    } else if (t2 == TermCodes) {
       return YAP_STRING_CODES;
    }
    return YAP_STRING_ATOMS;
  }    
  // see pl-incl.h
  unsigned int flags = Yap_GetModuleEntry(mod)->flags;
  if (flags & BCKQ_ATOM) {
    return YAP_STRING_ATOM | YAP_STRING_OUTPUT_TERM;
  } else if (flags & BCKQ_STRING) {
    return YAP_STRING_STRING;
  } else if (flags & BCKQ_CHARS) {
    return YAP_STRING_ATOMS;
  }
  return YAP_STRING_CODES;
}

static inline seq_type_t Yap_TextType(Term t) {
  if (IsVarTerm(t = Deref(t))) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "expected text");
  }
  if (IsAtomTerm(t)) {
    return YAP_STRING_ATOM;
  }
  if (IsStringTerm(t)) {
    return YAP_STRING_STRING;
  }
  if (IsPairTerm(t)) {
    Term hd = HeadOfTerm(t);
    if (IsVarTerm(hd)) {
      Yap_ThrowError(INSTANTIATION_ERROR, t, "expected text");
    }
    if (IsIntegerTerm(hd)) {
      return YAP_STRING_CODES;
    }
    if (IsAtomTerm(hd)) {
      return YAP_STRING_ATOMS;
    }
  }
  Yap_ThrowError(TYPE_ERROR_TEXT, t, "expected text");
}

// the routines

extern unsigned char *Yap_readText(seq_tv_t *inp USES_REGS);
extern bool write_Text(unsigned char *inp, seq_tv_t *out USES_REGS);
extern bool Yap_CVT_Text(seq_tv_t *inp, seq_tv_t *out USES_REGS);
extern bool Yap_Concat_Text(int n, seq_tv_t inp[], seq_tv_t *out USES_REGS);
extern bool Yap_Splice_Text(int n, size_t cuts[], seq_tv_t *inp,
			    seq_tv_t outv[] USES_REGS);

// user friendly interface

static inline Atom Yap_AtomicToLowAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_CODES | YAP_STRING_ATOMS |
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM | YAP_STRING_DOWNCASE;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Atom Yap_AtomicToUpAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_CODES | YAP_STRING_ATOMS |
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM | YAP_STRING_UPCASE;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term Yap_AtomicToLowString(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_CODES | YAP_STRING_ATOMS |
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_STRING | YAP_STRING_DOWNCASE;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomicToUpString(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_CODES | YAP_STRING_ATOMS |
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_STRING | YAP_STRING_UPCASE;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomicToLowListOfCodes(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_CODES | YAP_STRING_ATOMS |
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_CODES | YAP_STRING_DOWNCASE;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomicToUpListOfCodes(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_CODES | YAP_STRING_ATOMS |
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_CODES | YAP_STRING_UPCASE;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomicToLowListOfAtoms(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_CODES | YAP_STRING_ATOMS |
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_ATOMS | YAP_STRING_DOWNCASE;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomicToUpListOfAtoms(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_CODES | YAP_STRING_ATOMS |
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_ATOMS | YAP_STRING_UPCASE;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline size_t Yap_AtomicToUnicodeLength(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_CODES | YAP_STRING_ATOMS |
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_CHARS | YAP_STRING_OUTPUT_TERM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return strlen_utf8(out.val.uc0);
}

static inline Term Yap_AtomicToListOfAtoms(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomicToListOfCodes(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG ;
  out.val.uc = NULL;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_AtomicToAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline size_t Yap_AtomToLength(Term t0 USES_REGS) {
  if (!IsAtomTerm(t0)) {
    return -TYPE_ERROR_ATOM;
  }
  return strlen(RepAtom(AtomOfTerm(t0))->StrOfAE);
}

static inline size_t Yap_AtomToUnicodeLength(Term t0 USES_REGS) {
  if (!IsAtomTerm(t0)) {
    return -TYPE_ERROR_ATOM;
  }
  return strlen_utf8(RepAtom(AtomOfTerm(t0))->UStrOfAE);
}

static inline Term Yap_AtomToListOfAtoms(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomSWIToListOfAtoms(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM | YAP_STRING_STRING | YAP_STRING_INT |
             YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_ATOMS_CODES;
  out.val.uc = NULL;
  out.type = YAP_STRING_ATOMS;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomToListOfCodes(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomSWIToListOfCodes(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM | YAP_STRING_STRING | YAP_STRING_INT |
             YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_ATOMS_CODES | YAP_STRING_ATOMS_CODES | YAP_STRING_ATOMS_CODES |
             YAP_STRING_TERM ;
  out.val.uc = NULL;
  out.type = YAP_STRING_CODES;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}


static inline Term Yap_AtomToNumber(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  out.enc = ENC_ISO_UTF8;
  out.type = YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomToString(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  out.type = YAP_STRING_STRING;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomSWIToString(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM | YAP_STRING_STRING | YAP_STRING_INT |
             YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_ATOMS_CODES ;
  out.val.uc = NULL;
  out.type = YAP_STRING_STRING;
  out.enc = ENC_ISO_UTF8;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomicToString(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.type = YAP_STRING_STRING;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_AtomicToTDQ(Term t0, Term mod USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.type = mod_to_type(mod PASS_REGS);
  out.enc = ENC_ISO_UTF8;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline wchar_t *Yap_AtomToWide(Atom at USES_REGS) {
  seq_tv_t inp, out;
  inp.val.a = at;
  inp.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  out.type = YAP_STRING_WCHARS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return NIL;
  return out.val.w;
}

static inline Term Yap_AtomicToTBQ(Term t0, Term mod USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.type = mod_to_bqtype(mod PASS_REGS);

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_CharsToAtom(const char *s, encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.enc = enc;
  inp.type = YAP_STRING_CHARS;
  out.val.uc = NULL;
  out.type = YAP_STRING_ATOM;
  out.enc = ENC_ISO_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term Yap_CharsToListOfAtoms(const char *s,
					  encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.enc = enc;
  inp.type = YAP_STRING_CHARS;
  out.val.uc = NULL;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_CharsToListOfCodes(const char *s,
					  encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.enc = enc;
  inp.type = YAP_STRING_CHARS;
  out.val.uc = NULL;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_UTF8ToListOfCodes(const char *s USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  inp.enc = ENC_ISO_UTF8;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_UTF8ToAtom(const unsigned char *s USES_REGS) {
  seq_tv_t inp, out;

  inp.val.uc0 = s;
  inp.type = YAP_STRING_CHARS;
  inp.enc = ENC_ISO_UTF8;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term Yap_CharsToDiffListOfCodes(const char *s, Term tail,
					      encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.enc = enc;
  inp.type = YAP_STRING_CHARS;
  out.val.uc = NULL;
  out.type = YAP_STRING_DIFF | YAP_STRING_CODES;
  out.dif = tail;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_UTF8ToDiffListOfCodes(const unsigned char *s,
					     Term tail USES_REGS) {
  seq_tv_t inp, out;

  inp.val.uc0 = s;
  inp.type = YAP_STRING_CHARS;
  inp.enc = ENC_ISO_UTF8;
  out.type = YAP_STRING_DIFF | YAP_STRING_CODES;
  out.val.uc = NULL;
  out.dif = tail;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_WCharsToDiffListOfCodes(const wchar_t *s,
					       Term tail USES_REGS) {
  seq_tv_t inp, out;

  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_DIFF | YAP_STRING_CODES;
  out.val.uc = NULL;
  out.dif = tail;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;

  return out.val.t;
}

static inline Term Yap_CharsToString(const char *s, encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.enc = enc;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_STRING;
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline char *Yap_AtomToUTF8Text(Atom at USES_REGS) {
  return RepAtom(at)->StrOfAE;
}

static inline Term Yap_CharsToTAQ(const char *s, Term mod,
				  encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  inp.mod = mod;
  inp.enc = enc;
  out.type = mod_to_atype(mod PASS_REGS);
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_CharsToTDQ(const char *s, Term mod,
				  encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  inp.mod = mod;
  inp.enc = enc;
  out.type = mod_to_type(mod PASS_REGS);
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_CharsToTBQ(const char *s, Term mod,
				  encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  inp.mod = mod;
  inp.enc = enc;
  out.type = mod_to_bqtype(mod PASS_REGS);
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_ListOfAtomsToAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOMS|YAP_STRING_CODES;
  out.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  out.enc = ENC_ISO_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return (Atom)NULL;
  return out.val.a;
}

static inline Term Yap_ListOfAtomsToNumber(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOMS|YAP_STRING_CODES;
  out.type =
    YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.enc = ENC_ISO_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_ListOfAtomsToString(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOMS|YAP_STRING_CODES;
  out.type = YAP_STRING_STRING;
  out.val.uc = NULL;
  out.enc = ENC_ISO_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_ListOfCodesToAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_CODES|YAP_STRING_ATOMS;
  out.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  out.enc = ENC_ISO_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return (Atom)NULL;
  return out.val.a;
}

static inline Term Yap_ListOfCodesToNumber(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_CODES|YAP_STRING_ATOMS;
  out.type = YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG;
  out.enc = ENC_ISO_UTF8;
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_ListOfCodesToString(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_CODES|YAP_STRING_ATOMS;
  out.val.uc = NULL;
  out.type = YAP_STRING_STRING;
  out.enc = ENC_ISO_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_ListToAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_ATOMS_CODES;
  out.val.uc = NULL;
  out.type = YAP_STRING_ATOM;
  out.enc = ENC_ISO_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term Yap_ListToAtomic(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOMS_CODES | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.enc = ENC_ISO_UTF8;
  out.type = YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
    YAP_STRING_BIG | YAP_STRING_OUTPUT_TERM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_ListToNumber(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOMS_CODES;
  out.val.uc = NULL;
  out.enc = ENC_ISO_UTF8;
  out.type = YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_ListToString(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOMS_CODES | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.type = YAP_STRING_STRING;
  out.enc = ENC_ISO_UTF8;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_ListSWIToString(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_ATOMS_CODES |
    YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG |
    YAP_STRING_OUTPUT_TERM | YAP_STRING_PREFER_LIST;
  out.val.uc = NULL;
  out.type = YAP_STRING_STRING;
  out.enc = ENC_ISO_UTF8;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term YapListToTDQ(Term t0, Term mod USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOMS_CODES | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.type = mod_to_type(mod PASS_REGS);
  out.enc = ENC_ISO_UTF8;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term YapListToTBQ(Term t0, Term mod USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOMS_CODES | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.type = mod_to_bqtype(mod PASS_REGS);
  out.enc = ENC_ISO_UTF8;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_NCharsToAtom(const char *s, size_t len,
				    encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS | YAP_STRING_NCHARS;
  inp.enc = enc;
  out.enc = ENC_ISO_UTF8;
  out.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term Yap_CharsToDiffListOfAtoms(const char *s, encoding_t enc,
					      Term tail USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  inp.enc = enc;
  out.type = YAP_STRING_ATOMS | YAP_STRING_DIFF;
  out.dif = tail;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_NCharsToListOfCodes(const char *s, size_t len,
					   encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS | YAP_STRING_NCHARS;
  inp.enc = enc;
  out.type = YAP_STRING_CODES;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_NCharsToString(const char *s, size_t len,
				      encoding_t enc USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.enc = enc;
  inp.type = YAP_STRING_CHARS | YAP_STRING_NCHARS;
  out.type = YAP_STRING_STRING;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_NCharsToTDQ(const char *s, size_t len, encoding_t enc,
				   Term mod USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS | YAP_STRING_NCHARS;
  inp.enc = enc;
  inp.mod = mod;
  out.type = mod_to_type(mod PASS_REGS);
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_NCharsToTBQ(const char *s, size_t len, encoding_t enc,
				   Term mod USES_REGS) {
  seq_tv_t inp, out;

  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS | YAP_STRING_NCHARS;
  inp.enc = enc;

  out.type = mod_to_bqtype(mod PASS_REGS);
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_NumberToAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type =
    YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term Yap_NumberToListOfAtoms(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type =
    YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_NumberToListOfCodes(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type =
    YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_NumberToString(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type =
    YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_STRING;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_NWCharsToAtom(const wchar_t *s, size_t len USES_REGS) {
  seq_tv_t inp, out;

  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS | YAP_STRING_NCHARS;
  out.type = YAP_STRING_ATOM;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term Yap_NWCharsToListOfAtoms(const wchar_t *s,
					    size_t len USES_REGS) {
  seq_tv_t inp, out;

  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS | YAP_STRING_NCHARS;
  out.type = YAP_STRING_ATOMS;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_NWCharsToListOfCodes(const wchar_t *s,
					    size_t len USES_REGS) {
  seq_tv_t inp, out;

  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS | YAP_STRING_NCHARS;
  out.type = YAP_STRING_CODES;
  out.val.uc = NULL;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_NWCharsToString(const wchar_t *s, size_t len USES_REGS) {
  seq_tv_t inp, out;

  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS | YAP_STRING_NCHARS;
  out.val.uc = NULL;
  out.type = YAP_STRING_STRING;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_StringToAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING;
  out.val.uc = NULL;
  out.type = YAP_STRING_ATOM;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Atom Yap_StringSWIToAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_ATOMS_CODES |
    YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline size_t Yap_StringToAtomic(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING;
  out.type =
    YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG;
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline size_t Yap_StringToUnicodeLength(Term t0 USES_REGS) {
  if (!IsStringTerm(t0)) {
    return -TYPE_ERROR_STRING;
  }
  return strlen_utf8(UStringOfTerm(t0));
}

static inline size_t Yap_StringToListOfAtoms(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING;
  out.type = YAP_STRING_ATOMS;
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline size_t Yap_StringSWIToListOfAtoms(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_ATOMS_CODES |
    YAP_STRING_TERM;
  out.type = YAP_STRING_ATOMS;
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline size_t Yap_StringToListOfCodes(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING;
  out.type = YAP_STRING_CODES;
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline size_t Yap_StringSWIToListOfCodes(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_ATOMS_CODES |
    YAP_STRING_TERM;
  out.type = YAP_STRING_CODES;
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_StringToNumber(Term t0 USES_REGS) {
  seq_tv_t inp, out;
  inp.val.t = t0;
  inp.type = YAP_STRING_STRING;
  out.type =
    YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.val.uc = NULL;
  out.enc = ENC_ISO_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Atom Yap_TextToAtom(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM | YAP_STRING_STRING | YAP_STRING_CODES |
    YAP_STRING_ATOMS_CODES;
  out.val.uc = NULL;
  out.type = YAP_STRING_ATOM;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.a;
}

static inline Term Yap_TextToString(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM | YAP_STRING_STRING | YAP_STRING_CODES |
    YAP_STRING_ATOMS_CODES;
  out.val.uc = NULL;
  out.type = YAP_STRING_STRING;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline void Yap_OverwriteUTF8BufferToLowCase(void *buf USES_REGS) {
  unsigned char *s = (unsigned char *)buf;
  while (*s) {
    // assumes the two code have always the same size;
    utf8proc_int32_t chr;
    get_utf8(s, -1, &chr);
    chr = utf8proc_tolower(chr);
    s += put_utf8(s, chr);
  }
}

/**
 * Function to convert a generic text term (string, atom, list of codes, list
 of<
 atoms)  into a buff
 er.
 *
 * @param t     the term
 * @param buf   the buffer, if NULL a buffer is malloced, and the user should
 reclai it
 * @param len   buffer size
 * @param enc   encoding (UTF-8 is strongly recommended)
 *
 * @return the buffer, or NULL in case of failure. If so, Yap_Error may be
 called.
 *
 * notice that it must be called from a push memory.
*/
static inline const char *Yap_TextTermToText(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM | YAP_STRING_STRING | YAP_STRING_CODES |
             YAP_STRING_ATOMS_CODES | YAP_STRING_MALLOC;
  out.val.uc = NULL;
  out.type = YAP_STRING_CHARS;
  out.enc = ENC_ISO_UTF8;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return NULL;
  return out.val.c0;
}

static inline const unsigned char *Yap_TextToUTF8Buffer(Term t0 USES_REGS) {
  seq_tv_t inp, out;

  inp.val.t = t0;
  inp.type = YAP_STRING_ATOM | YAP_STRING_STRING | YAP_STRING_CODES |
    YAP_STRING_ATOMS_CODES;
  out.val.uc = NULL;
  out.type = YAP_STRING_CHARS | YAP_STRING_MALLOC;
  out.enc = ENC_ISO_UTF8;

  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return NULL;
  return out.val.uc0;
}

static inline Term Yap_UTF8ToString(const char *s USES_REGS) {
  return MkStringTerm(s);
}

static inline Atom UTF32ToAtom(const wchar_t *s USES_REGS) {
  seq_tv_t inp, out;

  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOM;
  out.max = -1;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS)) {
    LOCAL_Error_TYPE  = (LOCAL_Error_TYPE  == TYPE_ERROR_TEXT ? TYPE_ERROR_ATOM : LOCAL_Error_TYPE  );
    return NULL;
  }
  return out.val.a;
}

static inline Term Yap_WCharsToListOfCodes(const wchar_t *s USES_REGS) {
  seq_tv_t inp, out;
  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.val.uc = NULL;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_WCharsToTDQ(wchar_t *s, Term mod USES_REGS) {
  seq_tv_t inp, out;

  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;

  inp.mod = mod;
  out.type = mod_to_type(mod PASS_REGS);
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_WCharsToTBQ(wchar_t *s, Term mod USES_REGS) {
  seq_tv_t inp, out;

  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  inp.mod = mod;
  out.type = mod_to_bqtype(mod PASS_REGS);
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  return out.val.t;
}

static inline Term Yap_WCharsToString(const wchar_t *s USES_REGS) {
  seq_tv_t inp, out;
  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_STRING;
  out.val.uc = NULL;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS)) {
    LOCAL_Error_TYPE   = (LOCAL_Error_TYPE  == TYPE_ERROR_TEXT ? TYPE_ERROR_STRING : LOCAL_Error_TYPE  );
    return 0L;
  }
  return out.val.t;
}

static inline Atom Yap_ConcatAtoms(Term t1, Term t2 USES_REGS) {
  seq_tv_t inpv[2], out;
  inpv[0].val.t = t1;
  inpv[0].type = YAP_STRING_ATOM ;
  inpv[1].val.t = t2;
  inpv[1].type = YAP_STRING_ATOM;
  out.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  if (!Yap_Concat_Text(2, inpv, &out PASS_REGS)) {
    LOCAL_Error_TYPE   = (LOCAL_Error_TYPE  == TYPE_ERROR_TEXT ? TYPE_ERROR_ATOM : LOCAL_Error_TYPE  );
    return NULL;
  }
  return out.val.a;
}

static inline Atom Yap_ConcatAtomics(Term t1, Term t2 USES_REGS) {
  seq_tv_t inpv[2], out;
  inpv[0].val.t = t1;
  inpv[0].type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  inpv[1].val.t = t2;
  inpv[1].type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
    YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
  out.type = YAP_STRING_ATOM;
  out.val.uc = NULL;
  if (!Yap_Concat_Text(2, inpv, &out PASS_REGS))
    return (Atom)NULL;
  return out.val.a;
}

static inline Term Yap_ConcatStrings(Term t1, Term t2 USES_REGS) {
  seq_tv_t inpv[2], out;
  inpv[0].val.t = t1;
  inpv[0].type = YAP_STRING_STRING;
  inpv[1].val.t = t2;
  inpv[1].type = YAP_STRING_STRING;
  out.type = YAP_STRING_STRING;

  if (!Yap_Concat_Text(2, inpv, &out PASS_REGS)){
    LOCAL_Error_TYPE   = (LOCAL_Error_TYPE  == TYPE_ERROR_TEXT ? TYPE_ERROR_STRING : LOCAL_Error_TYPE  );
    return 0L;
  }
  return out.val.t;
}

static inline Atom Yap_SpliceAtom(Term t1, Atom ats[], size_t cut,
				  size_t max USES_REGS) {
  seq_tv_t outv[2], inp;
  size_t cuts[2];
  cuts[0] = cut;
  cuts[1] = max;
  inp.type = YAP_STRING_ATOM;
  inp.val.t = t1;
  outv[0].type = YAP_STRING_ATOM;
  outv[1].type = YAP_STRING_ATOM;
  if (!Yap_Splice_Text(2, cuts, &inp, outv PASS_REGS)) {
    LOCAL_Error_TYPE   = (LOCAL_Error_TYPE  == TYPE_ERROR_TEXT ? TYPE_ERROR_ATOM : LOCAL_Error_TYPE  );
    return NULL;
  }
  ats[0] = outv[0].val.a;
  ats[1] = outv[1].val.a;
  return ats[0];
}

static inline Atom Yap_SubtractHeadAtom(Term t1, Term th USES_REGS) {
  seq_tv_t outv[2], inp;
  inp.type = YAP_STRING_ATOM;
  inp.val.t = t1;
  outv[0].type = YAP_STRING_ATOM;
  outv[0].val.t = th;
  outv[1].type = YAP_STRING_ATOM;
  outv[1].val.t = 0;
  if (!Yap_Splice_Text(2, (size_t *)NULL, &inp, outv PASS_REGS)) {
    LOCAL_Error_TYPE   = (LOCAL_Error_TYPE  == TYPE_ERROR_TEXT ? TYPE_ERROR_ATOM : LOCAL_Error_TYPE  );
    return NULL;
  }
  return outv[1].val.a;
}

static inline Atom Yap_SubtractTailAtom(Term t1, Term th USES_REGS) {
  seq_tv_t outv[2], inp;
  inp.type = YAP_STRING_ATOM;
  inp.val.t = t1;
  outv[0].type = YAP_STRING_ATOM;
  outv[0].val.t = 0;
  outv[1].type = YAP_STRING_ATOM;
  outv[1].val.t = th;
  if (!Yap_Splice_Text(2, (size_t *)NULL, &inp, outv PASS_REGS)) {
    LOCAL_Error_TYPE   = (LOCAL_Error_TYPE  == TYPE_ERROR_TEXT ? TYPE_ERROR_ATOM : LOCAL_Error_TYPE  );
    return 0L;
  }
  return outv[0].val.a;
}

static inline Term Yap_SpliceString(Term t1, Term ts[], size_t cut,
				    size_t max USES_REGS) {
  seq_tv_t outv[2], inp;
  size_t cuts[2];
  inp.type = YAP_STRING_STRING;
  inp.val.t = t1;
  outv[0].type = YAP_STRING_STRING;
  outv[1].type = YAP_STRING_STRING;
  cuts[0] = cut;
  cuts[1] = max;
  if (!Yap_Splice_Text(2, cuts, &inp, outv PASS_REGS)){
    LOCAL_Error_TYPE   = (LOCAL_Error_TYPE  == TYPE_ERROR_TEXT ? TYPE_ERROR_STRING : LOCAL_Error_TYPE  );
    return 0L;
  }
  ts[0] = outv[0].val.t;
  ts[1] = outv[1].val.t;
  return ts[0];
}

static inline Term Yap_SubtractHeadString(Term t1, Term th USES_REGS) {
  seq_tv_t outv[2], inp;
  inp.type = YAP_STRING_STRING;
  inp.val.t = t1;
  outv[0].type = YAP_STRING_STRING;
  outv[0].val.t = th;
  outv[1].type = YAP_STRING_STRING;
  outv[1].val.t = 0;
  if (!Yap_Splice_Text(2, (size_t *)NULL, &inp, outv PASS_REGS)){
    LOCAL_Error_TYPE   = (LOCAL_Error_TYPE == TYPE_ERROR_TEXT ? TYPE_ERROR_STRING : LOCAL_Error_TYPE );
    return 0L;
  }
  return outv[1].val.t;
}

static inline Term Yap_SubtractTailString(Term t1, Term th USES_REGS) {
  seq_tv_t outv[2], inp;
  inp.type = YAP_STRING_STRING;
  inp.val.t = t1;
  outv[0].type = YAP_STRING_STRING;
  outv[0].val.t = 0;
  outv[1].type = YAP_STRING_STRING;
  outv[1].val.t = th;
  if (!Yap_Splice_Text(2, (size_t *)NULL, &inp, outv PASS_REGS)){
    LOCAL_Error_TYPE   = (LOCAL_Error_TYPE  == TYPE_ERROR_TEXT ? TYPE_ERROR_STRING : LOCAL_Error_TYPE  );
    return 0L;
  }
  return outv[0].val.t;
}

#endif // ≈YAP_TEXT_H

extern Term Yap_MkTextTerm(const char *s, int guide USES_REGS);
