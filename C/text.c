/*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V. Santos Costa and Universidade do Porto 1985--	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		strings.c *
 * comments:	General-conversion of character sequences.		 *
 *									 *
 * Last rev:     $Date: 2008-07-24 16:02:00 $,$Author: vsc $	     	 *
 *									 *
 *************************************************************************/

/**
   @file text.c
   @brief Support routines for text processing
*/

/**
@defgroup TextSup Text  Processing Support Routines
@ingroup Text_Processing
@brief generic text processing engine.

@{
  Support for text processing:
  - converting to UTF-8
  - converting from UTF-8
  - striping
  - splitting
 -- concatenating
*/
#include "Yap.h"
#include "YapCompoundTerm.h"
#include "YapEval.h"
#include "YapHeap.h"
#include "YapStreams.h"
#include "YapTags.h"
#include "YapText.h"
#include "Yatom.h"
#include "alloc.h"
#include "yapio.h"

#include <YapText.h>
#include <string.h>
#include <wchar.h>

#ifndef HAVE_WCSNLEN
inline static ssize_t min_size(ssize_t i, ssize_t j) { return (i < j ? i : j); }
#define wcsnlen(S, N) min_size(N, wcslen(S))
#endif

#if !defined(HAVE_STPCPY) && !defined(__APPLE__)
inline static void *__stpcpy(void *i, const void *j) {
  return strcpy(i, j) + strlen(j);
}
#define stpcpy __stpcpy
#endif

#ifndef NAN
#define NAN (0.0 / 0.0)
#endif

inline char_kind_t Yap_wide_chtype(int ch) {
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

static Term Globalize(Term v USES_REGS) {
  if (!IsVarTerm(v = Deref(v))) {
    return v;
  }
  if (VarOfTerm(v) > HR && VarOfTerm(v) < LCL0) {
    Bind_Local(VarOfTerm(v), MkVarTerm());
    v = Deref(v);
  }
  return v;
}

static unsigned char *latin2utf8(seq_tv_t *inp) {
   CACHE_REGS
  unsigned char *b0 = inp->val.uc;
  ssize_t sz = strlen(inp->val.c);
  sz *= 2;
  int ch;
  unsigned char *buf = malloc(sz + 1), *pt = buf;
  if (!buf)
    return NULL;
  while ((ch = *b0++)) {
    int off = put_utf8(pt, ch);
    if (off < 0) {
      continue;
    }
    pt += off;
  }
  *pt++ = '\0';
  return buf;
}

static unsigned char *wchar2utf8(seq_tv_t *inp, size_t *szp) {
   CACHE_REGS
  ssize_t sz = wcslen(inp->val.w) * 4;
  wchar_t *b0 = inp->val.w;
  unsigned char *buf = malloc(sz + 1), *pt = buf;
  int ch;
  if (!buf)
    return NULL;
  while ((ch = *b0++))
    pt += put_utf8(pt, ch);
  *pt++ = '\0';
  return buf;
}


static void *slice(ssize_t min, ssize_t max, const unsigned char *buf USES_REGS) {
size_t sz =  strlen((char*)buf)+1;
if (max >sz) max = sz;
 if (max==min) {
   unsigned char *nbuf = malloc(1);
   nbuf[0] = '\0';
   return nbuf;
 }

 const unsigned char *ptr = skip_utf8(buf, min), *end = skip_utf8(ptr,max-min);
 unsigned char *nbuf = malloc((end-ptr) + 1);
  memmove(nbuf,ptr,(end-ptr));
  nbuf[end-ptr] = '\0';
  return nbuf;
}


static yap_error_number gen_type_error(int flags) {
  if ((flags & (YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
                YAP_STRING_FLOAT | YAP_STRING_ATOMS_CODES | YAP_STRING_BIG)) ==
      (YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
       YAP_STRING_ATOMS_CODES | YAP_STRING_BIG))
    return TYPE_ERROR_TEXT;
  if ((flags & (YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
                YAP_STRING_FLOAT | YAP_STRING_BIG)) ==
      (YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT | YAP_STRING_FLOAT |
       YAP_STRING_BIG))
    return TYPE_ERROR_ATOMIC;
  if ((flags & (YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG)) ==
      (YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG))
    return TYPE_ERROR_NUMBER;
  if (flags & YAP_STRING_ATOM)
    return TYPE_ERROR_ATOM;
  if (flags & YAP_STRING_STRING)
    return TYPE_ERROR_STRING;
  if (flags & (YAP_STRING_CODES | YAP_STRING_ATOMS))
    return TYPE_ERROR_LIST;
  return TYPE_ERROR_NUMBER;
}


//  static int cnt;
/**
   read any kind of input: numbers. atoms, strings, sequences of characters or
   codes, even Prolog terms.
   The output is a descriptor with:
   + the buffer data, has a pointer to a UTF-8 sequence of characters.
   + the output buffer data source (current frame, ancestor frame, malloc,
prolog database)
+ the buffer size
 */


unsigned char *Yap_readText(seq_tv_t *inp, buf_info *buf  USES_REGS) {
  yap_error_number err = YAP_NO_ERROR;
  /* we know what the term is */
  if (inp->type & YAP_STRING_CHARS) {
     if (inp->enc == ENC_ISO_ASCII ||
	 inp->enc == ENC_ISO_UTF8)
       {
      buf->sz = 0;
      buf->status = 0;
      buf->buf = (unsigned char *)inp->val.t   ;
      return buf->buf;
       }
     if (inp->enc == ENC_ISO_LATIN1 ) {
      buf->buf = latin2utf8(inp);
      buf->sz=0;
      buf->status = FROM_MALLOC;
      return buf->buf;
     }
  }
     if (inp->type & YAP_STRING_WCHARS) {
    // printf("%S\n",inp->val.w);
    char *out =
      ( char *)wchar2utf8(inp, &buf->sz);
      buf->status = FROM_MALLOC;
      buf->buf = (unsigned char *)out;
      return buf->buf;
     }

    enum_seq_type_t inpt = inp->type & (YAP_STRING_TERM|YAP_STRING_ATOM|YAP_STRING_ATOMS_CODES);
    
    if (IsVarTerm(inp->val.t)) {
	err = INSTANTIATION_ERROR;
      } else if (!IsAtomTerm(inp->val.t) && inpt == YAP_STRING_ATOM) {
        err = TYPE_ERROR_ATOM;
      } else if (!IsStringTerm(inp->val.t) && inpt == YAP_STRING_STRING) {
        err = TYPE_ERROR_STRING;
      } else if (!IsPairOrNilTerm(inp->val.t) && !IsStringTerm(inp->val.t)
                 && inpt == (YAP_STRING_ATOMS_CODES | YAP_STRING_STRING)) {
        err = TYPE_ERROR_LIST;
      } 
      if (err) {
	Yap_ThrowError(err,
		       inp->val.t, "while converting term %s", Yap_TermToBuffer(
										inp->val.t, YAP_WRITE_HANDLE_CYCLES|YAP_WRITE_QUOTED | Number_vars_f));
      }
      if (inp->val.t == TermNil) {
        if (inp->type & YAP_STRING_PREFER_LIST )
      {
	buf->buf = RepAtom(AtomOfTerm(TermEmpty))->UStrOfAE;
	buf->sz = 0;
	buf->status = FROM_CODE_SPACE;
	return buf->buf;
      }
        // fall to atom
      }
        
    if (IsAtomTerm(inp->val.t) && inp->type & YAP_STRING_ATOM)
      {
      // this is a term, extract to a buffer, and representation is wide
      // Yap_DebugPlWriteln(inp->val.t);
      Atom at = AtomOfTerm(inp->val.t);
      buf->buf = RepAtom(at)->UStrOfAE;
      buf->sz = 0;
      buf->status = FROM_CODE_SPACE;
      return buf->buf;
      }
     if (IsStringTerm(inp->val.t) && inp->type & YAP_STRING_STRING)
      {
      // this is a term, extract to a buffer, and representation is wide
      // Yap_DebugPlWriteln(inp->val.t);
      buf->buf = (unsigned char *)UStringOfTerm(inp->val.t);
      buf->sz = 0;
      buf->status = FROM_STACK_SPACE;
      return buf->buf;
      }
     if (IsPairTerm(inp->val.t)) 
      {
	if (IsIntTerm(HeadOfTerm(inp->val.t)) && inp->type & YAP_STRING_CODES) {        // Yap_DebugPlWriteln(inp->val.t);
	  buf->buf =
            (unsigned char *)Yap_CodesToBuffer(inp->val.t PASS_REGS);
          buf->sz = 0;
          buf->status = FROM_MALLOC;
          return buf->buf;
        }
        if ((IsAtomTerm(HeadOfTerm(inp->val.t)) && inp->type & YAP_STRING_ATOMS)) {
	  buf->buf =
            (unsigned char *)Yap_CharsToBuffer(inp->val.t PASS_REGS);
          buf->sz = 0;
          buf->status = FROM_MALLOC;
          return buf->buf;
	}
      }
   if (inp->type & YAP_STRING_INT && IsIntegerTerm(inp->val.t))
      {
      // ASCII, so both LATIN1 and UTF-8
      // Yap_DebugPlWriteln(inp->val.t);
      unsigned char * out = buf->buf = malloc(65);
      snprintf((char*)out, 64, Int_FORMAT,
               IntegerOfTerm(inp->val.t));
        buf->sz = 0;
        buf->status = FROM_MALLOC;
          return buf->buf;
      }
   if (inp->type & YAP_STRING_FLOAT && IsFloatTerm(inp->val.t))
      {
      char * out =   malloc(65);
      Yap_FormatFloat(FloatOfTerm(inp->val.t), &out, 64);
      buf->sz = 0;
      buf->status = FROM_MALLOC;
      buf->buf = (unsigned char *)out;
      return buf->buf;
      }
   if (inp->type & YAP_STRING_BIG && IsBigIntTerm(inp->val.t)) {
      char *out;
      // Yap_DebugPlWriteln(inp->val.t);
      if ((out=Yap_mpz_to_string(Yap_BigIntOfTerm(inp->val.t),10)) == NULL) 
	AUX_ERROR(inp->val.t, MaxTmp(PASS_REGS1), out, char);
      buf->sz = 0;
      buf->status = FROM_MALLOC;
      buf->buf = (unsigned char *)out;
      return buf->buf;
    }
   if (inp->type & YAP_STRING_TERM)
     {
    char *out =
      Yap_TermToBuffer(inp->val.t, 0);
      buf->sz = 0;
      buf->status = FROM_MALLOC;
      buf->buf = (unsigned char *)out;
      return buf->buf;
     }

   Yap_ThrowError(TYPE_ERROR_TEXT,
		 TermNil, "Bad text input ");
  return NULL;

  } 

static Term write_strings(unsigned char *s0, seq_tv_t *out USES_REGS) {
  ssize_t max;

  if (s0 && s0[0]) max = strlen((char *)s0);
  else max = 0;

  if (out->type & (YAP_STRING_NCHARS | YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS)
    if (out->type & YAP_STRING_TRUNC && out->max < max) {
      max = out->max;
      s0[max] = '\0';
    }
  }
  size_t sz = 1+strlen((char*)s0)/sizeof(CELL);
  if (HR+sz > ASP-64*1024)
    out->val.t = 0;
  else
    out->val.t = MkUStringTerm(s0);

  return out->val.t;
}

static Term write_atoms(void *s0, seq_tv_t *out USES_REGS) {
  Term t = AbsPair(HR);
  char *s1 = (char *)s0;
  ssize_t sz = 0;
  size_t max = strlen(s1);
  if (s1[0] == '\0') {
    out->val.t = TermNil;
    return TermNil;
  }
  if (out->type & (YAP_STRING_NCHARS | YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_TRUNC && out->max < max)
      max = out->max;
  }

  unsigned char *s = s0, *lim = s + strnlen((char *)s, max);
  unsigned char *cp = s;
  unsigned char w[10];
  int wp = 0;
  LOCAL_TERM_ERROR(t, 2 * (lim - s));
  while (cp < lim && *cp) {
    utf8proc_int32_t chr;
    CELL *cl;
    s += get_utf8(s, -1, &chr);
    if (chr == '\0') {
      w[0] = '\0';
      break;
    }
    wp = put_utf8(w, chr);
    w[wp] = '\0';
    cl = HR;
    HR += 2;
    cl[0] = MkAtomTerm(Yap_ULookupAtom(w));
    cl[1] = AbsPair(HR);
    sz++;
    if (sz == max)
      break;
  }
  if (out->type & YAP_STRING_DIFF) {
    if (sz == 0)
      t = out->dif;
    else
      HR[-1] = Globalize(out->dif PASS_REGS);
  } else {
    if (sz == 0)
      t = TermNil;
    else
      HR[-1] = TermNil;
  }
  out->val.t = t;
  return (t);
}

static Term write_codes(void *s0, seq_tv_t *out USES_REGS) {
  Term t;
  size_t sz = strlen(s0);
  if (sz == 0) {
    if (out->type & YAP_STRING_DIFF) {
      out->val.t = Globalize(out->dif PASS_REGS);
    } else {
      out->val.t = TermNil;
    }
    return out->val.t;
  }
  unsigned char *s = s0, *lim = s + sz;
  unsigned char *cp = s;

  t = AbsPair(HR);
  LOCAL_TERM_ERROR(t, 2 * (lim - s));
  t = AbsPair(HR);
  while (*cp) {
    utf8proc_int32_t chr;
    CELL *cl;
    cp += get_utf8(cp, -1, &chr);
    if (chr == '\0')
      break;
    cl = HR;
    HR += 2;
    cl[0] = MkIntegerTerm(chr);
    cl[1] = AbsPair(HR);
  }
  if (sz == 0) {
    HR[-1] = Globalize(out->dif PASS_REGS);
  } else {
    HR[-1] = TermNil;
  }
  out->val.t = t;
  return (t);
}

static Atom write_atom(void *s0, seq_tv_t *out USES_REGS) {
  unsigned char *s = s0;
  int32_t ch;
  if (s[0] == '\0') {
    return Yap_LookupAtom("");
  }
  size_t leng = strlen(s0);
  if (strlen_utf8(s0) <= leng) {
    return Yap_LookupAtom(s0);
  } else {
    size_t n = get_utf8(s, -1, &ch);
    unsigned char *buf = malloc(n + 1);
    memmove(buf, s0, n + 1);
    Atom rc = Yap_ULookupAtom(buf);
    return rc;
  }
}

void *write_buffer(unsigned char *s0, seq_tv_t *out USES_REGS) {
  size_t leng = strlen((char *)s0);
  size_t min = 0, max = leng;
  if (out->enc == ENC_ISO_UTF8) {
    if (out->val.uc == NULL) { // this should always be the case
      out->val.uc = malloc(leng + 1);
      strcpy(out->val.c, (char *)s0);
    } else if (out->val.uc != s0) {
      out->val.c = malloc(leng + 1);
      strcpy(out->val.c, (char *)s0);
    }
    out->type |= YAP_STRING_MALLOC;
  } else if (out->enc == ENC_ISO_LATIN1) {

    unsigned char *s = s0;
    unsigned char *cp = s;
    unsigned char *buf = out->val.uc;
    if (!buf) {
      return NULL;
    }
    while (*cp) {
      utf8proc_int32_t chr;
      int off = get_utf8(cp, -1, &chr);
      if (off <= 0 || chr > 255) {
        return NULL;
      }
      if (off == max)
        break;
      cp += off;
      *buf++ = chr;
    }
    if (max >= min)
      *buf++ = '\0';
    else
      while (max < min) {
        utf8proc_int32_t chr;
        max++;
        cp += get_utf8(cp, -1, &chr);
        *buf++ = chr;
      }
  } else if (out->enc == ENC_WCHAR) {
    unsigned char *s = s0, *lim = s + (max = strnlen((char *)s0, max));
    unsigned char *cp = s;
    wchar_t *buf0, *buf;

    buf = buf0 = out->val.w;
    if (!buf) {
      return NULL;
    }
    while (*cp && cp < lim) {
      utf8proc_int32_t chr;
      cp += get_utf8(cp, -1, &chr);
      *buf++ = chr;
    }
    if (max >= min)
      *buf++ = '\0';
    else
      while (max < min) {
        utf8proc_int32_t chr;
        max++;
        cp += get_utf8(cp, -1, &chr);
        *buf++ = chr;
      }
    *buf = '\0';
  } else {
    // no other encodings are supported.
    return NULL;
  }
  return out->val.c;
}

static size_t write_length(const unsigned char *s0, seq_tv_t *out USES_REGS) {
  return strlen_utf8(s0);
}

static Term write_number(unsigned char *s, seq_tv_t *out                       USES_REGS) {
  Term t;

  t = Yap_StringToNumberTerm((char *)s, &out->enc,false);
  LOCAL_delay = false;
  return t;
}

static Term string_to_term(void *s, seq_tv_t *out USES_REGS) {
  Term o;
  yap_error_descriptor_t *new_error = malloc(sizeof(yap_error_descriptor_t));
  yap_error_descriptor_t *old = Yap_pushErrorContext(true, new_error, LOCAL_ActiveError);
  o = out->val.t = Yap_BufferToTerm(s, TermNil);
  LOCAL_ActiveError = Yap_popErrorContext(true, false, old);
  free(new_error);
  return o;
}

Term Yap_StringToNumberTerm(const char *s, encoding_t *encp, bool error_on) {
  CACHE_REGS
  int sno;
  Atom nat = AtomEmptyBrackets;
  sno = Yap_open_buf_read_stream(NULL, s, strlen(s), encp, MEM_BUF_USER, nat,
                                 TermEvaluable);
  if (sno < 0)
    return TermNil;
  if (encp)
    GLOBAL_Stream[sno].encoding = *encp;
  else
    GLOBAL_Stream[sno].encoding = LOCAL_encoding;
#ifdef __ANDROID__

  while (*s && isblank(*s) && Yap_wide_chtype(*s) == BS)
    s++;
#endif
  GLOBAL_Stream[sno].status |= CloseOnException_Stream_f;
  if (!error_on) {
   GLOBAL_Stream[sno].status |= FailOnScanError_Stream_f;
  }
  int i = push_text_stack();
  Term t = Yap_scan_num(GLOBAL_Stream + sno
			);
  Yap_CloseStream(sno);
  pop_text_stack(i);
  return t;
}


bool write_Text(unsigned char *inp, seq_tv_t *out USES_REGS) {
  /* we know what the term is */
  out->val.t = 0;
  if (out->type == 0) {
    return true;
  }
  if ((out->type &(YAP_STRING_INT |
			 YAP_STRING_FLOAT |
		   YAP_STRING_BIG))!=0) {
    if ((out->val.t = write_number(
				   inp, out PASS_REGS)) !=0&&
	(out->val.t != TermNil)) {
      // Yap_DebugPlWriteln(out->val.t);

      return true;
    } else {

      if (!(out->type & (YAP_STRING_ATOM |
			 YAP_STRING_STRING |
			YAP_STRING_CODES))) {
	return false;
      }
      Yap_ResetException(NULL);


  }
  }
  if (out->type & (YAP_STRING_ATOM)) {


    if ((out->val.a = write_atom(inp, out PASS_REGS)) != NIL) {
      Atom at = out->val.a;
      if (at && (out->type & YAP_STRING_OUTPUT_TERM))
        out->val.t = MkAtomTerm(at);
      // Yap_DebugPlWriteln(out->val.t);
      return at != NIL;
    }
  }
  if (out->type & YAP_STRING_DATUM) {
    if ((out->val.t = string_to_term(inp, out PASS_REGS)) != 0L)
      return out->val.t != 0;
  }

  switch (out->type & YAP_TYPE_MASK) {
  case YAP_STRING_CHARS: {
    void *room = write_buffer(inp, out PASS_REGS);
    // printf("%s\n", out->val.c);
    return room != NULL;
  }
  case YAP_STRING_WCHARS: {
    void *room = write_buffer(inp, out PASS_REGS);
    // printf("%S\n", out->val.w);
    return room != NULL;
  }
  case YAP_STRING_STRING:
    out->val.t = write_strings(inp, out PASS_REGS);
    // Yap_DebugPlWriteln(out->val.t);
    return out->val.t != 0;
  case YAP_STRING_ATOMS:
    out->val.t = write_atoms(inp, out PASS_REGS);
    // Yap_DebugPlWriteln(out->val.t);
    return out->val.t != 0;
  case YAP_STRING_CODES:
    out->val.t = write_codes(inp, out PASS_REGS);
    // Yap_DebugPlWriteln(out->val.t);
    return out->val.t != 0;
  case YAP_STRING_LENGTH:
    out->val.l = write_length(inp, out PASS_REGS);
    // printf("s\n",out->val.l);
    return out->val.l != (size_t)(-1);
  case YAP_STRING_ATOM:
    out->val.a = write_atom(inp, out PASS_REGS);
    // Yap_DebugPlWriteln(out->val.t);
    return out->val.a != NULL;
  case YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG:
    out->val.t = write_number(inp, out PASS_REGS);
    // Yap_DebugPlWriteln(out->val.t);
    if (out->val.t==TermNil &&(out->type & (YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG |YAP_STRING_ATOM | YAP_STRING_STRING| YAP_STRING_TERM)) ==
	(YAP_STRING_INT | YAP_STRING_FLOAT | YAP_STRING_BIG))
      Yap_ThrowError(TYPE_ERROR_NUMBER, MkUStringTerm(inp), NULL);
    return out->val.t != 0;
  default: { return true; }
  }
  return false;
}

static size_t upcase(void *s0, seq_tv_t *out USES_REGS) {

  unsigned char *s = s0;
  while (*s) {
    // assumes the two code have always the same size;
    utf8proc_int32_t chr;
    get_utf8(s, -1, &chr);
    chr = utf8proc_toupper(chr);
    s += put_utf8(s, chr);
  }
  return true;
}

static size_t downcase(void *s0, seq_tv_t *out USES_REGS) {

  unsigned char *s = s0;
  while (*s) {
    // assumes the two code have always the same size;
    utf8proc_int32_t chr;
    get_utf8(s, -1, &chr);
    chr = utf8proc_tolower(chr);
    s += put_utf8(s, chr);
  }
  return true;
}

bool Yap_CVT_Text(seq_tv_t *inp, seq_tv_t *out USES_REGS) {
  unsigned char *buf;
  bool rc;
  buf_info buf_i, *obp = &buf_i;
  /*
  //printf(stderr, "[ %d ", n++)    ;
  if (inp->type & (YAP_STRING_TERM|YAP_STRING_ATOM|YAP_STRING_ATOMS_CODES
  |YAP_STRING_STRING))
  //Yap_DebugPlWriteln(inp->val.t);
  else if (inp->type & YAP_STRING_WCHARS) fprintf(stderr,"S %S\n", inp->val
  .w);
  else  fprintf(stderr,"s %s\n", inp->val.c);
  */
  //  cnt++;
  buf = Yap_readText(inp, obp PASS_REGS);
  if (!buf) {
    yap_error_number err;
    if (!(err=gen_type_error(inp->type)))
	  err = TYPE_ERROR_TEXT;
    return 0L;
  }
  bool oldm  = inp->type &YAP_STRING_MALLOC;
  if (buf[0]) {
    size_t leng = strlen_utf8(buf);
    if (out->type & (YAP_STRING_NCHARS | YAP_STRING_TRUNC)) {
      if (out->max < leng) {
        const unsigned char *ptr = skip_utf8(buf, out->max);
        size_t diff = (ptr - buf);
        if (obp->status & FROM_MALLOC) {
          buf = realloc(buf, diff + 1);
        } else {
          char *nbuf = malloc(diff+1);
          memmove(nbuf, buf, diff);
          nbuf[diff] = '\0';
          buf = (unsigned char *)nbuf;
        }
        leng = diff;
      }
      // else if (out->type & YAP_STRING_NCHARS &&
      // const unsigned char *ptr = skip_utf8(buf)
    }
    if (out->type & (YAP_STRING_UPCASE | YAP_STRING_DOWNCASE)) {
      if (out->type & YAP_STRING_UPCASE) {
        if (!upcase(buf, out PASS_REGS)) {
          return false;
        }
      }
      if (out->type & YAP_STRING_DOWNCASE) {
        if (!downcase(buf, out PASS_REGS)) {
          return false;
        }
      }
    }
  }
  rc = write_Text(buf, out PASS_REGS);
  /*    fprintf(stderr, " -> ");
        if (!rc) fprintf(stderr, "NULL");
        else if (out->type &
        (YAP_STRING_TERM|YAP_STRING_ATOMS_CODES
        |YAP_STRING_STRING)) //Yap_DebugPlWrite(out->val.t);
        else if (out->type &
        YAP_STRING_ATOM) //Yap_DebugPlWriteln(MkAtomTerm(out->val.a));
        else if (out->type & YAP_STRING_WCHARS) fprintf(stderr, "%S",
        out->val.w);
        else
        fprintf(stderr, "%s", out->val.c);
        fprintf(stderr, "\n]\n"); */
  if (!(out->type & (YAP_STRING_CHARS | YAP_STRING_WCHARS))) {
    if (obp->status == FROM_MALLOC && !oldm) {
      free(obp->buf);
    }
  }
  return rc;
}


//
// Out must be an atom or a string

//
// Out must be an atom or a string
bool Yap_Concat_Text(int tot, seq_tv_t inp[], seq_tv_t *out USES_REGS) {
  unsigned char *buf;
  int i;
  size_t avai;
    unsigned char const *nbuf=NULL;
    size_t bsz = tot * 256;
  buf = calloc(1,bsz);
  buf[0] = '\0';
  if (!buf) {
    return NULL;
  }
  for (i = 0; i < tot; i++) {
    Term t = inp[i].val.t;
    if (IsAtomTerm(t)) {
      nbuf = RepAtom(AtomOfTerm(t))->UStrOfAE;
    } else    if (IsStringTerm(t)) {
      nbuf = UStringOfTerm(t);
    } else {
  buf_info buf_i, *obp = &buf_i;
  nbuf = Yap_readText(inp + i, obp PASS_REGS);
    }
    if (!nbuf) {
      //     pop_text_stack(lvl);
      //return NULL;
      continue;
    }
    //      if (!nbuf[0])
    //	continue;
    if (nbuf && nbuf[0]) {
      size_t sz = strlen((char*)nbuf);
      avai = (strlen((char *)buf)+sz +1);
      size_t x = 256*(tot-i-1);
      if (avai > bsz-x) {
	buf = realloc(buf, avai+x);
      }
      strcat((char*)buf,(char*)nbuf);
    }
  }

  bool rc = write_Text(buf, out PASS_REGS);
  free(buf);

  return rc;
}

//
bool Yap_Splice_Text(int n, ssize_t cuts[], seq_tv_t *inp,
                     seq_tv_t outv[] USES_REGS) {
  const unsigned char *buf;
  ssize_t b_l, u_l, i;

  buf_info buf_i, *obp = &buf_i;
  inp->type |= YAP_STRING_IN_TMP;
  buf = Yap_readText(inp, obp PASS_REGS);
  if (!buf) {
    return false;
  }
  b_l = strlen((char *)buf);
  if (b_l == 0) {
    return false;
  }
  u_l = strlen_utf8(buf);
  if (!cuts) {
    if (n == 2) {
      size_t b_l0, b_l1, u_l0, u_l1;
      unsigned char *buf0, *buf1;

      if (outv[0].val.t) {
        buf_info buf_i, *obp = &buf_i;
        buf0 = Yap_readText(outv, obp PASS_REGS);
        if (!buf0) {
          return false;
        }
        b_l0 = strlen((const char *)buf0);
        if (memcmp(buf, buf0, b_l0) != 0) {
          return false;
        }
        u_l0 = strlen_utf8(buf0);
        u_l1 = u_l - u_l0;

        b_l1 = b_l - b_l0;
        buf1 = slice(u_l0, u_l, buf PASS_REGS);
        b_l1 = strlen((const char *)buf1);
        bool rc = write_Text(buf1, outv + 1 PASS_REGS);
        if (!rc) {
          return false;
        }
        return rc;
      } else /* if (outv[1].val.t) */ {
        buf1 = Yap_readText(outv + 1, obp PASS_REGS);
        if (!buf1) {
          return false;
        }
        b_l1 = strlen((char *)buf1);
        u_l1 = strlen_utf8(buf1);
        b_l0 = b_l - b_l1;
        u_l0 = u_l - u_l1;
        if (memcmp(skip_utf8((const unsigned char *)buf, b_l0), buf1, b_l1) !=
            0) {
          return false;
        }
        buf0 = slice(0, u_l0, buf PASS_REGS);
        bool rc = write_Text(buf0, outv PASS_REGS);
        return rc;
      }
    }
  }
  for (i = 0; i < n; i++) {
  ssize_t from,next;
    if (i == 0)
      from = 0;
    else
      from = cuts[i - 1];
    next = cuts[i];
    void *bufi = slice(from, next, buf PASS_REGS);
    if (!write_Text(bufi, outv + i PASS_REGS)) {
      return false;
    }
     }

  return true;
}

/**
 * Convert from a predicate structure to an UTF-8 string of the form
 *
 * module:name/arity.
 *
 * The result is in very volatile memory.
 *
 * @param s        the buffer
 *
 * @return the temporary string
 */
const char *Yap_PredIndicatorToUTF8String(PredEntry *ap, char *s0, size_t sz) {
  CACHE_REGS
  Atom at;
  arity_t arity = 0;
  Functor f;
  char *s, *smax;
  s = s0;
  smax = s + sz;
  Term tmod = ap->ModuleOfPred;
  if (tmod) {
    char *sn = Yap_AtomToUTF8Text(AtomOfTerm(tmod) PASS_REGS);
    stpcpy(s, sn);
    if (smax - s > 1) {
      strcat(s, ":");
    } else {
      return NULL;
    }
    s++;
  } else {
    if (smax - s > strlen("prolog:")) {
      s = strcpy(s, "prolog:");
    } else {
      return NULL;
    }
  }
  // follows the actual functor
  if (ap->ModuleOfPred == IDB_MODULE) {
    if (ap->PredFlags & NumberDBPredFlag) {
      Int key = ap->src.IndxId;
      snprintf(s, smax - s, "%" PRIdPTR, key);
      return s0;
    } else if (ap->PredFlags & AtomDBPredFlag) {
      at = (Atom)(ap->FunctorOfPred);
      if (!stpcpy(s, Yap_AtomToUTF8Text(at PASS_REGS)))
        return NULL;
    } else {
      f = ap->FunctorOfPred;
      at = NameOfFunctor(f);
      arity = ArityOfFunctor(f);
    }
  } else {
    arity = ap->ArityOfPE;
    if (arity) {
      at = NameOfFunctor(ap->FunctorOfPred);
    } else {
      at = (Atom)(ap->FunctorOfPred);
    }
  }
  if (!stpcpy(s, Yap_AtomToUTF8Text(at PASS_REGS))) {
    return NULL;
  }
  s += strlen(s);
  snprintf(s, smax - s, "/%" PRIdPTR, arity);
  return s0;
}


/// @}
