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
* File:		strings.c						 *
* comments:	General-conversion of character sequences.		 *
*									 *
* Last rev:     $Date: 2008-07-24 16:02:00 $,$Author: vsc $	     	 *
*									 *
*************************************************************************/

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "eval.h"
#include "yapio.h"
#include "YapText.h"

#if defined(__BIG_ENDIAN__)
#define ENC_WCHAR ENC_ISO_UTF32_BE
#else
#define ENC_WCHAR ENC_ISO_UTF32_LE
#endif

#include <string.h>
#include <wchar.h>

#ifndef HAVE_WCSNLEN
inline static size_t
min_size(size_t i, size_t j) {
  return ( i < j ?  i :  j );
}
#define wcsnlen(S, N) min_size(N, wcslen(S))
#endif

static inline unsigned char *get_char(unsigned char *p, int *c) { *c = *p; return p+1; }

static inline wchar_t *get_wchar(wchar_t *p, int *c) { *c = *p; return p+1; }

#ifndef NAN
#define NAN      (0.0/0.0)
#endif

static Term
Globalize(Term v USES_REGS)
{
  if (!IsVarTerm(v = Deref(v))) {
    return v;
  }
  if (VarOfTerm(v) > HR && VarOfTerm(v) < LCL0) {
    Bind_Local(VarOfTerm(v), MkVarTerm());
    v = Deref(v);
  }
  return v;
}

static char *
get_string_from_list( Term t, seq_tv_t *inp, char *s, int atoms USES_REGS)
{
  char *s0 = s;
  size_t max = -1;
  if (inp->type & YAP_STRING_TRUNC) {
    max = inp->max;
  }

  if (TRUE /* atoms == -1 */) {
    while (t != TermNil) {
      Term h = HeadOfTerm(t);
      if (IsAtomTerm(h)) {
	Atom at;
	if (IsWideAtom(at = AtomOfTerm(h)))
	  *s++ = RepAtom(at)->WStrOfAE[0];
	else
	  *s++ = (unsigned char)(RepAtom(at)->StrOfAE[0]);
      } else {
	*s++  = IntOfTerm(h);
      }
      if (--max == 0) {
	*s++ = 0;
	return s0;
      }
      t = TailOfTerm(t);
    }
  } else if (atoms) {
    while (t != TermNil) {
      Atom at;
      if (IsWideAtom(at = AtomOfTerm(HeadOfTerm(t)))) {
	int i = RepAtom(at)->WStrOfAE[0];
	if (i <= 0) {
	  LOCAL_Error_TYPE = REPRESENTATION_ERROR_CHARACTER_CODE;
	  return NULL;
	}
	*s++ = i;
      } else
	*s++ = RepAtom(at)->StrOfAE[0];
      if (--max == 0) {
	*s++ = 0;
	return s0;
      }
      t = TailOfTerm(t);
    }
  } else {
    while (t != TermNil) {
      Int i = IntOfTerm(HeadOfTerm(t));
      if (i <= 0 || i > 255) {
	LOCAL_Error_TYPE = REPRESENTATION_ERROR_CHARACTER_CODE;
	return NULL;
      }
      *s++ = i;
      if (--max == 0) {
	*s++ = '\0';
	return s0;
      }
      t = TailOfTerm(t);
    }
  }
  *s++ = '\0';
  return s0;
}

static wchar_t *
get_wide_from_list( Term t, seq_tv_t *inp, wchar_t *s, int atoms USES_REGS)
{
  wchar_t *s0 = s;
  size_t max = -1;
  if (inp->type & YAP_STRING_TRUNC) {
    max = inp->max;
  }

  if (TRUE /* atoms == -1*/) {
    while (t != TermNil) {
      Term h = HeadOfTerm(t);
      if (IsAtomTerm(h)) {
	Atom at;
	if (IsWideAtom(at = AtomOfTerm(h)))
	  *s++ = RepAtom(at)->WStrOfAE[0];
	else
	  *s++ = (unsigned char)(RepAtom(at)->StrOfAE[0]);
      } else {
	*s++ = IntOfTerm(h);
      }
      if (--max == 0) {
	*s++ = 0;
	return s0;
      }
      t = TailOfTerm(t);
    }
  } else if (atoms) {
    while (t != TermNil) {
      Atom at;
      if (IsWideAtom(at = AtomOfTerm(HeadOfTerm(t))))
	*s++ = RepAtom(at)->WStrOfAE[0];
      else
	*s++ = (unsigned char)(RepAtom(at)->StrOfAE[0]);
      if (--max == 0) {
	*s++ = 0;
	return s0;
      }
      t = TailOfTerm(t);
    }
  } else {
    while (t != TermNil) {
      int code;
      *s++ = code = IntOfTerm(HeadOfTerm(t));
      if (code <= 0) {
	LOCAL_Error_TYPE = REPRESENTATION_ERROR_CHARACTER_CODE;
	return NULL;
      }
      if (--max == 0) {
	*s++ = 0;
	return s0;
      }
      t = TailOfTerm(t);
    }
  }
  *s++ = '\0';
  return s0;
}


static Int
SkipListCodes(Term *l, Term **tailp, Int *atoms, bool *wide)
{
  Int length = 0;
  Term *s; /* slow */
  Term v; /* temporary */

  do_derefa(v,l,derefa_unk,derefa_nonvar);
  s = l;
  *wide = false;


  if (*l == TermNil) {
    *tailp = l;
    *atoms = 0;
    *wide = FALSE;
    return 0;
  }
  if ( IsPairTerm(*l) )
  { intptr_t power = 1, lam = 0;
    do
    { if ( power == lam )
      { s = l;
	power *= 2;
	lam = 0;
      }
      lam++;
      length++;
      { Term hd = Deref(RepPair(*l)[0]);
	if (IsVarTerm(hd)) {
	  length =  -INSTANTIATION_ERROR;
	} else if (IsAtomTerm(hd)) {
	  (*atoms)++;
	  /* if (*atoms < length)
	     { *tailp = l; return -TYPE_ERROR_STRING; } */
	  if (IsWideAtom(AtomOfTerm(hd))) {
	    if ((RepAtom(AtomOfTerm(hd))->WStrOfAE)[1] != '\0') { length = -REPRESENTATION_ERROR_CHARACTER; }
	    *wide = TRUE;
	  } else {
	    AtomEntry *ae = RepAtom(AtomOfTerm(hd));
	    if ((ae->StrOfAE)[1] != '\0') { length = -REPRESENTATION_ERROR_CHARACTER_CODE; }
	  }
	} else if (IsIntegerTerm(hd)) {
	  Int ch = IntegerOfTerm(hd);
	  if (/* *atoms|| */ch < 0) { *tailp = l; /*if (*atoms) length = -TYPE_ERROR_STRING;*/ length = -DOMAIN_ERROR_NOT_LESS_THAN_ZERO; }
	  else if (ch > 0x80) { *wide = TRUE; }
	} else {
	  length = -TYPE_ERROR_INTEGER;
	}
	if (length < 0) {
	  *tailp = l;
	  return length;
	}
      }
      l = RepPair(*l)+1;
      do_derefa(v,l,derefa2_unk,derefa2_nonvar);
    } while ( *l != *s && IsPairTerm(*l) );
  }
  *tailp = l;

  return length;
}


static void *
Yap_ListOfAtomsToBuffer(void *buf, Term t, seq_tv_t *inp, bool *widep, size_t *lenp USES_REGS)
{
  Int atoms = 0;
  CELL *r = NULL;
  Int n;

  *widep = false;
  n = SkipListCodes(&t, &r, &atoms, widep);
  if (n < 0) {
    LOCAL_Error_TYPE = -n;
    LOCAL_Error_Term = *r;
    return NULL;
  }
  if (*r != TermNil) {
    if (IsVarTerm(*r))
      LOCAL_Error_TYPE = INSTANTIATION_ERROR;
    else
      LOCAL_Error_TYPE = TYPE_ERROR_LIST;
    LOCAL_Error_Term = *r;
    return NULL;
  }
  /*  if (n && !atoms) {
    LOCAL_Error_Term = t;
    LOCAL_Error_TYPE = TYPE_ERROR_CHARACTER;
    return NULL;
  }
  */
  *lenp = n;
  if (*widep) {
    wchar_t *s;
    if (buf) s = buf;
    else s = ((AtomEntry *)Yap_PreAllocCodeSpace())->WStrOfAE;
    AUX_ERROR( t, 2*(n+1), s, wchar_t);
    s = get_wide_from_list( t, inp, s, atoms PASS_REGS);
    return s;
  } else {
    char *s;
    if (buf) s = buf;
    else s = (char *)((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
    AUX_ERROR( t, 2*(n+1), s, char);
    s = get_string_from_list( t, inp, s, atoms PASS_REGS);
    return s;
  }
}

static void *
Yap_ListOfCodesToBuffer(void *buf, Term t, seq_tv_t *inp, bool *widep, size_t *lenp USES_REGS)
{
  Int atoms = 0;
  CELL *r = NULL;
  Int n;

  *widep = false;
  n = SkipListCodes(&t, &r, &atoms, widep);
  if (n < 0) {
    LOCAL_Error_TYPE = -n;
    LOCAL_Error_Term = *r;
    return NULL;
  }
  if (*r != TermNil) {
    if (IsVarTerm(*r))
      LOCAL_Error_TYPE = INSTANTIATION_ERROR;
    else
      LOCAL_Error_TYPE = TYPE_ERROR_LIST;
    LOCAL_Error_Term = *r;
    return NULL;
  }
  if (n && atoms)
    return NULL;
  *lenp = n;
  if (*widep) {
    wchar_t *s;
    if (buf) s = buf;
    else s = ((AtomEntry *)Yap_PreAllocCodeSpace())->WStrOfAE;
    AUX_ERROR( t, 2*(n+1), s, wchar_t);
    s = get_wide_from_list( t, inp, s, atoms  PASS_REGS);
    return s;
  } else {
     char *s;
    if (buf) s = buf;
    else s = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
    AUX_ERROR( t, 2*(n+1), (char *)s, char);
    s = ( char *)get_string_from_list( t, inp, (char *)s, atoms PASS_REGS);
    return s;
  }
}

#if USE_GEN_TYPE_ERROR
static yap_error_number
gen_type_error(int flags) {
  if ((flags & (YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_ATOMS_CODES|YAP_STRING_BIG)) ==
      (YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_ATOMS_CODES|YAP_STRING_BIG))
    return TYPE_ERROR_TEXT;
  if ((flags & (YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG)) ==
      (YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG))
    return TYPE_ERROR_ATOMIC;
  if ((flags & (YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG)) ==
      (YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG))
    return TYPE_ERROR_NUMBER;
  if (flags & YAP_STRING_ATOM )
    return TYPE_ERROR_ATOM;
  if (flags & YAP_STRING_STRING)
    return TYPE_ERROR_STRING;
  if (flags & (YAP_STRING_CODES|YAP_STRING_ATOMS))
    return TYPE_ERROR_LIST;
  return TYPE_ERROR_NUMBER;
}
#endif

 void *
Yap_readText( void *buf, seq_tv_t *inp, encoding_t *enc, int *minimal, size_t *lengp USES_REGS)
{
  char *s;
  wchar_t *ws;
  bool wide;

  /* we know what the term is */
  if (inp->type & YAP_STRING_STRING && !IsVarTerm(inp->val.t) && IsStringTerm(inp->val.t)) { const char *s;
    s = (char *)StringOfTerm( inp->val.t );
    if ( s == NULL ) {
      return 0L;
    }
    // this is a term, extract the UTF8 representation
    *enc = ENC_ISO_UTF8;
    *minimal = FALSE;
    if (lengp)
      *lengp = strlen(s);
    return (void *)s;
  }
  if (inp->type & YAP_STRING_ATOM  && !IsVarTerm(inp->val.t) && IsAtomTerm(inp->val.t)) {
    // this is a term, extract to a buffer, and representation is wide
    *minimal = TRUE;
    Atom at = AtomOfTerm(inp->val.t);
    if (IsWideAtom(at)) {
      ws = at->WStrOfAE;
      *lengp = wcslen(ws);
      *enc = ENC_WCHAR;
      return ws;
    } else {
      s = (char *)at->StrOfAE;
      *lengp = strlen(s);
      *enc = ENC_ISO_LATIN1;
      return s;
    }
  }
  if (inp->type & YAP_STRING_CODES  && !IsVarTerm(inp->val.t) && (s = Yap_ListOfCodesToBuffer( buf, inp->val.t, inp, &wide, lengp PASS_REGS))) {
    // this is a term, extract to a sfer, and representation is wide
    *minimal = TRUE;
    *enc = ( wide ? ENC_WCHAR : ENC_ISO_LATIN1 );
    return s;
  }
  if (inp->type & YAP_STRING_ATOMS  && !IsVarTerm(inp->val.t) && (s = Yap_ListOfAtomsToBuffer( buf, inp->val.t, inp, &wide, lengp PASS_REGS))) {
    // this is a term, extract to a buffer, and representation is wide
    *minimal = TRUE;
    s = Yap_ListOfAtomsToBuffer( buf, inp->val.t, inp, &wide, lengp PASS_REGS);
    if (!s) return NULL;
    if (wide) { *enc = ENC_ISO_UTF8; }
    else { *enc = ENC_ISO_LATIN1; }
    return s;
  }
  if (inp->type & YAP_STRING_INT && IsIntegerTerm(inp->val.t)) {
      if (buf) s = buf;
      else s = Yap_PreAllocCodeSpace();
      AUX_ERROR( inp->val.t, LOCAL_MAX_SIZE, s, char);
      if (snprintf(s, LOCAL_MAX_SIZE-1, Int_FORMAT, IntegerOfTerm(inp->val.t)) < 0) {
	AUX_ERROR( inp->val.t, 2*LOCAL_MAX_SIZE, s, char);
      }
      *enc = ENC_ISO_LATIN1;
      *lengp = strlen(s);
      return s;
    }
  if (inp->type & YAP_STRING_FLOAT && IsFloatTerm(inp->val.t)) {
      if (buf) s = buf;
      else s = Yap_PreAllocCodeSpace();
      AUX_ERROR( inp->val.t, LOCAL_MAX_SIZE, s, char);
      if ( !Yap_FormatFloat( FloatOfTerm(inp->val.t), &s, LOCAL_MAX_SIZE-1 ) ) {
	AUX_ERROR( inp->val.t, 2*LOCAL_MAX_SIZE, s, char);
      }
      *lengp = strlen(s);
      *enc = ENC_ISO_LATIN1;
      return s;
    }
#if USE_GMP
  if (inp->type & YAP_STRING_BIG && IsBigIntTerm(inp->val.t)) {
      if (buf) s = buf;
      else s = Yap_PreAllocCodeSpace();
      if ( !Yap_mpz_to_string( Yap_BigIntOfTerm(inp->val.t), s, LOCAL_MAX_SIZE-1 , 10 ) ) {
	AUX_ERROR( inp->val.t, LOCAL_MAX_SIZE, s, char);
      }
      *enc = ENC_ISO_LATIN1;
      *lengp = strlen(s);
      return s;
    }
#endif
   if (inp->type & YAP_STRING_TERM)
    {
      char *s, *o;
      if (buf) s = buf;
      else s = Yap_PreAllocCodeSpace();
      size_t sz = LOCAL_MAX_SIZE-1;
      o = Yap_TermToString(inp->val.t, s, sz, lengp, ENC_ISO_UTF8, 0);
      return s;
    }
   if (inp->type & YAP_STRING_CHARS) {
      *enc = inp->enc;
      if (inp->type & YAP_STRING_NCHARS)
	*lengp = inp->sz;
      else
	*lengp = strlen(inp->val.c);
      return (void *)inp->val.c;
    }
    if (inp->type & YAP_STRING_WCHARS) {
      *enc = ENC_WCHAR;
      if (inp->type & YAP_STRING_NCHARS)
	*lengp = inp->sz;
      else
	*lengp = wcslen(inp->val.w);
      return (void *)inp->val.w;
    }
    return NULL;
}

static Term
write_strings( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  size_t min = 0, max = leng;
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC && out->max < max) max = out->max;
  }

  switch (enc) {
  case ENC_ISO_UTF8:
    { unsigned char *s = s0, *lim = s + (max = strlen_utf8(s));
      Term t = init_tstring( PASS_REGS1  );
      unsigned char *cp = s, *buf;

      LOCAL_TERM_ERROR( t, 2*(lim-s) );
      buf = buf_from_tstring(HR);
      while (*cp && cp < lim) {
	utf8proc_int32_t chr;
	cp +=  get_utf8(cp, &chr);
	buf += put_utf8(buf, chr);
      }
      if (max >= min) *buf++ = '\0';
      else while (max < min) {
	max++;
	buf += put_utf8(buf, '\0');
      }

      close_tstring( buf  PASS_REGS );
      out->val.t = t;
    }
    break;
  case ENC_ISO_LATIN1:
    { unsigned char *s = s0, *lim = s + (max = strlen_latin_utf8(s0));
      Term t = init_tstring( PASS_REGS1  );
      unsigned char *cp = s;
      unsigned char *buf;
      utf8proc_int32_t chr;

      LOCAL_TERM_ERROR( t, 2*(lim-s) );
      buf = buf_from_tstring(HR);
      while (cp < lim) {
	cp = get_char(cp, &chr);
	buf += put_utf8(buf, chr);
      }
      if (max >= min) *buf++ = '\0';
      else while (max < min) {
	  max++;
	  buf += put_utf8(buf, chr);
	}
      close_tstring( buf  PASS_REGS );
      out->val.t = t;
    }
    break;
  case ENC_WCHAR:
    { wchar_t *s = s0, *lim = s + (max = strlen_ucs2_utf8(s0));
      Term t = init_tstring( PASS_REGS1  );
      wchar_t *wp = s;
      unsigned char *buf;

      LOCAL_TERM_ERROR( t, 2*(lim-s) );
      buf = buf_from_tstring(HR);
      while (wp < lim) {
	utf8proc_int32_t chr;
	wp = get_wchar(wp, &chr);
	buf += put_utf8(buf, chr);
      }
      if (max >= min) *buf++ = '\0';
      else while (max < min) {
	max++;
	buf += put_utf8(buf,  '\0');
      }
      close_tstring( buf  PASS_REGS );
      out->val.t = t;
    }
    break;
  default:
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc), __FUNCTION__);
  }

  return out->val.t;
}

static Term
write_atoms( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  Term t = AbsPair(HR);
  size_t sz = 0;
  size_t max = leng;
  if (leng == 0) {
    out->val.t = t;
    return TermNil;
  }
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_TRUNC && out->max < max) max = out->max;
  }

  switch (enc) {
  case ENC_ISO_UTF8:
    { unsigned char *s = s0, *lim = s + strnlen((char*)s, max);
      unsigned char *cp = s;
      wchar_t w[2];
      w[1] = '\0';
     LOCAL_TERM_ERROR( t, 2*(lim-s) );
      while (cp < lim && *cp) {
	utf8proc_int32_t chr;
	CELL *cl;
	cp +=  get_utf8(cp, &chr);
	if (chr == '\0') break;
	w[0] = chr;
	cl = HR;
        HR += 2;
	cl[0] = MkAtomTerm(Yap_LookupMaybeWideAtom(w));
	cl[1] = AbsPair(HR);
	sz++;
	if (sz == max) break;
      }
      break;
    }
  case ENC_ISO_LATIN1:
    { unsigned char *s = s0, *lim = s + strnlen(s0, max);
      unsigned char *cp = s;
      char w[2];
      w[1] = '\0';

      LOCAL_TERM_ERROR( t, 2*(lim-s) );
      while (cp < lim) {
	utf8proc_int32_t chr;
	cp = get_char(cp, &chr);
	if (chr == '\0') break;
	w[0] = chr;
	HR[0] = MkAtomTerm(Yap_LookupAtom(w));
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
  case ENC_WCHAR:
    { wchar_t *s = s0, *lim = s + wcsnlen(s, max);
      wchar_t *cp = s;
      wchar_t w[2];
      w[1] = '\0';

      LOCAL_TERM_ERROR( t, 2*(lim-s) );
      while (*cp && cp < lim) {
	utf8proc_int32_t chr;
	cp = get_wchar(cp, &chr);
        if (chr == '\0') break;
	w[0] = chr;
	HR[0] = MkAtomTerm(Yap_LookupMaybeWideAtom(w));
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
  default:
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc), __FUNCTION__);
  }
  if (out->type & YAP_STRING_DIFF) {
    if (sz == 0) t = out->dif;
    else HR[-1] = Globalize(out->dif PASS_REGS);
  } else {
    if (sz == 0) t = TermNil;
    else HR[-1] = TermNil;
  }
  out->val.t = t;
  return (t);
}

static Term
write_codes( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  Term t = AbsPair(HR);
  size_t min = 0, max = leng;
  size_t sz = 0;

  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC && out->max < max) max = out->max;
  }

  switch (enc) {
  case ENC_ISO_UTF8:
    { unsigned char *s = s0, *lim = s + strnlen(s0, max);
      unsigned char *cp = s;
      LOCAL_TERM_ERROR( t, 2*(lim-s) );
      while (*cp && cp < lim) {
	utf8proc_int32_t chr;
	cp +=  get_utf8(cp, &chr);
	HR[0] = MkIntTerm(chr);
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
  case ENC_ISO_LATIN1:
    { unsigned char *s = s0, *lim = s + strnlen(s0, max);
      unsigned char *cp = s;

      LOCAL_TERM_ERROR( t, 2*(lim-s) );
      while (cp < lim) {
	utf8proc_int32_t chr;
	cp = get_char(cp, &chr);
	HR[0] = MkIntTerm(chr);
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
 case ENC_WCHAR:
    { wchar_t *s = s0, *lim = s + wcsnlen(s, max);
      wchar_t *cp = s;

      LOCAL_TERM_ERROR( t, 2*(lim-s) );
      while (cp < lim) {
	utf8proc_int32_t chr;
	cp = get_wchar(cp, &chr);
	HR[0] = MkIntTerm(chr);
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
  default:
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc), __FUNCTION__);
  }
  while (sz < min) {
    HR[0] = MkIntTerm(MkIntTerm(0));
    HR[1] = AbsPair(HR+2);
    HR += 2;
    sz++;
  }
  if (out->type & YAP_STRING_DIFF) {
    if (sz == 0) t = out->dif;
    else HR[-1] = Globalize(out->dif PASS_REGS);
  } else {
    if (sz == 0) t = TermNil;
    else HR[-1] = TermNil;
  }
  out->val.t = t;
  return (t);
}


static Atom
write_atom( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  size_t max = leng;
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
     if (out->type & YAP_STRING_TRUNC && out->max < max) max = out->max;
  }

  switch (enc) {
  case ENC_ISO_UTF8:
    { unsigned char *s = s0, *lim = s + strnlen(s0, max);
      wchar_t *buf = malloc(sizeof(wchar_t)*((lim+1)-s)), *ptr = buf;
      Atom at;

      while (*s && s < lim) {
	utf8proc_int32_t chr;
	s +=  get_utf8(s, &chr);
	*ptr++ = chr;
      }
      *ptr++ = '\0';
      at = Yap_LookupMaybeWideAtomWithLength( buf, max );
      free( buf );
      out->val.a = at;
      return at;
    }
  case ENC_ISO_LATIN1:
    { char *s = s0;
      Atom at;

      max = strnlen(s, max);
      at = Yap_LookupAtomWithLength(s, max);
      out->val.a = at;
      return at;
    }
 case ENC_WCHAR:
    { wchar_t *s = s0;
      Atom at;

      max = wcsnlen(s, max);
      out->val.a = at = Yap_LookupMaybeWideAtomWithLength(s, max);
      return at;
    }
   default:
     Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc));
  }
  return NULL;
}


static size_t
write_wbuffer( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  size_t min = 0, max = leng, sz_end, sz;
  out->enc = ENC_WCHAR;
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC && out->max < max) max = out->max;
  }
  if (out->enc != enc || out->type & (YAP_STRING_WITH_BUFFER|YAP_STRING_MALLOC)) {
    if (enc != ENC_WCHAR) {
      sz = strlen((char *)s0)+1;
    } else {
      sz = wcslen((wchar_t *)s0)+1;
    }
    if (sz < min) sz = min;
    sz *= sizeof(wchar_t);
    if (out->type & (YAP_STRING_MALLOC)) {
      out->val.w = malloc(sz);
    } else if (!(out->type & (YAP_STRING_WITH_BUFFER))) {
      if (ASP-(sz/sizeof(CELL)+1) > HR+1024) {
	out->val.w = (wchar_t *)(ASP-((sz*sizeof(wchar_t *)/sizeof(CELL)+1)));
      } else
	return -1;
    }
  } else {
    out->val.w = s0;
    sz_end = (wcslen( s0 )+1)*sizeof(wchar_t);
  }
  if (out->enc == ENC_WCHAR) {
    switch (enc) {
    case ENC_WCHAR:
	if (out->type & (YAP_STRING_WITH_BUFFER|YAP_STRING_MALLOC) ) {
	 wchar_t *s = s0;
	 size_t n = wcslen( s )+1;
	 if (n < min) n = min;
	 memcpy( out->val.c, s0, n*sizeof(wchar_t));
   	 out->val.w[n] = '\0';
	 sz_end = n+1;
	}
     case ENC_ISO_UTF8:
      {
	unsigned char *s = s0, *lim = s + (max = strnlen(s0, max));
	unsigned char *cp = s;
	wchar_t *buf0, *buf;
      
	buf = buf0 = out->val.w;
	if (!buf)
	  return -1;
	while (*cp && cp < lim) {
	  utf8proc_int32_t chr;
	  cp +=  get_utf8(cp, &chr);
	  *buf++ = chr;
	}
	if (max >= min) *buf++ = '\0';
	else while (max < min) {
	  utf8proc_int32_t chr;
	  max++;
	  cp +=  get_utf8(cp,  &chr);
	  *buf++ = chr;
	}
	*buf = '\0';
	sz_end = (buf-buf0)+1;
     }
      break;
   case ENC_ISO_LATIN1:
      {
	 char *s = s0;
	 size_t n = strlen( s ), i;
	 if (n < min) n = min;
	 for (i = 0; i < n; i++)
	   out->val.w[i] = s[i];
	 out->val.w[n] = '\0';
         sz_end = n+1;
      }
      break;
   default:
     sz_end = -1;
     Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc), __FUNCTION__);
    }
  }
  sz_end *= sizeof( wchar_t );
   if (out->type & (YAP_STRING_MALLOC)) {
     out->val.c = realloc(out->val.c,sz_end);
   }
   out->sz = sz_end;
  return sz_end;
 }


static size_t
write_buffer( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  size_t min = 0, max = leng, sz_end;
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC && out->max < max) max = out->max;
  }
  if (out->enc != enc || out->type & (YAP_STRING_WITH_BUFFER|YAP_STRING_MALLOC)) {
    size_t sz;
    if (enc != ENC_WCHAR)
      sz = strlen((char *)s0)+1;
    else
      sz = wcslen((wchar_t *)s0)+1;
    if (sz < min) sz = min;
    if (!minimal) sz *=  4;
    if (out->type & (YAP_STRING_MALLOC)) {
      out->val.c = malloc(sz);
    } else if (!(out->type & (YAP_STRING_WITH_BUFFER))) {
      if (ASP-(sz/sizeof(CELL)+1) > HR+1024) {
	out->val.c = (char *)(ASP-(sz/sizeof(CELL)+1));
      }
    }
  } else {
    out->val.c = s0;
  }
  if (out->enc == ENC_ISO_UTF8) {
    switch (enc) {
      case ENC_ISO_UTF8:
	if (out->type & (YAP_STRING_WITH_BUFFER|YAP_STRING_MALLOC) ) {
	 char *s = s0;
	 size_t n = strlen( s )+1;
	 memcpy( out->val.c, s0, n*sizeof(wchar_t));
   	 out->val.c[n] = '\0';
	 sz_end = n+1;
	} else {
	  sz_end = strlen(out->val.c)+1;
	}
	break;
   case ENC_ISO_LATIN1:
      {
	unsigned char *s = s0, *lim = s + (max = strnlen(s0, max));
	unsigned char *cp = s, *buf0, *buf;
        

	buf = buf0 = out->val.uc;
	if (!buf)
	  return -1;
	while (*cp && cp < lim) {
	  utf8proc_int32_t chr;
	  chr = *cp++;
	  buf += put_utf8(buf, chr);
	}
	if (max >= min) *buf++ = '\0';
	else while (max < min) {
	    max++;
	    utf8proc_int32_t chr;
	    chr = *cp++;
	    buf += put_utf8(buf, chr);
	  }
        buf[0] = '\0';
	sz_end = (buf+1)-buf0;
      } 
      break;
    case ENC_WCHAR:
      {
	wchar_t *s = s0;
	unsigned char *buf =  out->val.uc;
	size_t n = wcslen( s ), i;
	if (n < min) n = min;
	for (i = 0; i < n; i++) {
	  utf8proc_int32_t chr = s[i];
	  buf += put_utf8(buf, chr);
	}
	*buf++ = '\0';
	sz_end = (buf+1)-out->val.uc;
      }
      break;
   default:
        sz_end = -1;
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc), __FUNCTION__);
   }
  }else if (out->enc ==  ENC_ISO_LATIN1) {
   switch (enc) {
     case ENC_ISO_LATIN1:
       if (out->type & YAP_STRING_WITH_BUFFER) {
	 char *s = s0;
	 size_t n = strlen( s ), i;
	 if (n < min) n = min;
	 memcpy( out->val.c, s0, n);
	 for (i = 0; i < n; i++)
	   out->val.w[i] = s[i];
	 out->val.w[n] = '\0';
	 sz_end = (n+1)*sizeof(wchar_t);
       } else {
	 sz_end = strlen( out->val.c ) + 1;
       }
       break;
    case ENC_ISO_UTF8:
      {
	unsigned char *s = s0, *lim = s + (max = strnlen(s0, max));
	unsigned char *cp = s;
	unsigned char *buf0, *buf;
      
	buf = buf0 = out->val.uc;
	if (!buf)
	  return -1;
	while (*cp && cp < lim) {
	  utf8proc_int32_t chr;
	  cp +=  get_utf8(cp, &chr);
	  *buf++ = chr;
	}
	if (max >= min) *buf++ = '\0';
	else while (max < min) {
	  utf8proc_int32_t chr;
	  max++;
	  cp +=  get_utf8(cp, &chr);
	  *buf++ = chr;
	}
 	sz_end = buf-out->val.uc;
     }
      break;
   case ENC_WCHAR:
      {
	 wchar_t *s = s0;
	 size_t n = wcslen( s ), i;
	 if (n < min) n = min;
	 for (i = 0; i < n; i++)
	   out->val.c[i] = s[i];
	 out->val.c[n] = '\0';
         sz_end = n+1;
      }
      break;
   default:
       sz_end = -1;
       Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc), __FUNCTION__);
      }
  } else {
    // no other encodings are supported.
    sz_end = -1;
  }
   if (out->type & (YAP_STRING_MALLOC)) {
     out->val.c = realloc(out->val.c,sz_end);
   }
   out->sz = sz_end;
  return sz_end;
}



static ssize_t
write_length( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  size_t max = -1;

  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS && out->sz != (size_t)-1) return out->sz;
    if (out->type & YAP_STRING_TRUNC) max = out->max;
  }

  switch (enc) {
  case ENC_ISO_UTF8:
    {
      const unsigned char *s = s0;
      return strlen_utf8(s);
    }
  case ENC_ISO_LATIN1:
    {
      const char *s = s0;
      return strnlen(s, max);
    }
  case ENC_WCHAR:
    {
      const wchar_t *s = s0;
      return wcsnlen(s, max);
    }
  default:
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc), __FUNCTION__);
  }
  return (size_t)-1;
}

static Term
write_number( void *s0, seq_tv_t *out, encoding_t enc, int minimal, int size USES_REGS)
{
  return Yap_StringToNumberTerm(s0, enc);
}

static Term
string_to_term( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  return Yap_StringToTerm(s0, strlen(s0)+1, enc, 1200, NULL);
}


int
write_Text( void *inp, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  /* we know what the term is */
  switch (out->type &  YAP_TYPE_MASK) {
  case YAP_STRING_STRING:
    out->val.t =
      write_strings( inp, out, enc, minimal, leng PASS_REGS);
    return out->val.t != 0;
  case YAP_STRING_ATOMS:
    out->val.t =
      write_atoms( inp, out, enc, minimal, leng PASS_REGS);
    return out->val.t != 0;
  case YAP_STRING_CODES:
    out->val.t =
      write_codes( inp, out, enc, minimal, leng PASS_REGS);
    return out->val.t != 0;
  case YAP_STRING_LENGTH:
    out->val.l =
      write_length( inp, out, enc, minimal, leng PASS_REGS);
    return out->val.l != (ssize_t)(-1);
  case YAP_STRING_ATOM:
    out->val.a =
      write_atom( inp, out, enc, minimal, leng PASS_REGS);
    return out->val.a != NULL;
  case YAP_STRING_INT:
  case YAP_STRING_FLOAT:
  case YAP_STRING_BIG:
    out->val.t =
      write_number( inp, out, enc, minimal, leng PASS_REGS);
    return out->val.t != 0;
  case YAP_STRING_CHARS:
    {
      size_t sz = write_buffer( inp, out, enc, minimal, leng PASS_REGS);
      return((Int)sz > 0);
    }
  case YAP_STRING_WCHARS:
    {
      size_t sz = write_wbuffer( inp, out, enc, minimal, leng PASS_REGS);
      return((Int)sz > 0);
    }
   default:
    if (!(out->type & YAP_STRING_TERM))
      return 0;
    if (out->type & (YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG))
      if ((out->val.t =
	   write_number( inp, out, enc, minimal, leng PASS_REGS)) != 0L)
	return out->val.t != 0;
    if (out->type & (YAP_STRING_ATOM))
      if (write_atom( inp, out, enc, minimal, leng PASS_REGS) != NIL) {
	Atom at = out->val.a;
	if (at != NIL)
	  out->val.t = MkAtomTerm(at);
	return at != NIL;
      }
      if ((out->val.t =
	   string_to_term( inp, out, enc, minimal, leng PASS_REGS)) != 0L)
	return out->val.t != 0;
    }
  return FALSE;
}

 int
Yap_CVT_Text( seq_tv_t *inp, seq_tv_t *out USES_REGS)
{
  encoding_t enc;
  int minimal = FALSE;
  char *buf;
  size_t leng;

  buf = Yap_readText( NULL, inp, &enc, &minimal, &leng PASS_REGS );
  if (!buf)
    return 0L;
  return write_Text( buf, out, enc, minimal, leng PASS_REGS );
}

static void *
compute_end( void *s0, encoding_t enc )
{
  switch (enc) {
  case ENC_ISO_LATIN1:
  case ENC_ISO_UTF8:
    {
      char *s = (char *)s0;
      return s+(1+strlen(s));
    }
  case ENC_WCHAR:
    {
      wchar_t *s = (wchar_t *)s0;
      return s + (1+wcslen(s));
    }
 default:
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc), __FUNCTION__);
  }
  return NULL;
}

static void *
advance_Text( void *s, int l, encoding_t enc )
{
  switch (enc) {
  case ENC_ISO_LATIN1:
    return ((char *)s)+l;
  case ENC_ISO_UTF8:   
    return (char *)skip_utf8(s,l);
  case ENC_WCHAR:
    return ((wchar_t *)s)+l;
  default:
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc), __FUNCTION__);
  }
  return s;
}

static int
cmp_Text( void *s1, void *s2, int l, encoding_t enc1, encoding_t enc2 )
{
  Int i;
  switch (enc1) {
  case ENC_ISO_LATIN1:
    {
      char *w1 = (char *)s1;
      switch (enc2) {
      case ENC_ISO_LATIN1:
	return strncmp(s1, s2, l);
      case ENC_ISO_UTF8:
	{
	  utf8proc_int32_t chr1, chr2;
	  unsigned char *w2 = s2;
	  for (i = 0; i < l; i++) { chr1 = *w1++; w2 +=  get_utf8(w2, &chr2); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
    case ENC_WCHAR:
	{
	  utf8proc_int32_t chr1, chr2;
	  wchar_t *w2 = s2;
	  for (i = 0; i < l; i++) { chr1 = *w1++; chr2 = *w2++; if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
      default:
	Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc2), __FUNCTION__);
      }
     }
  case ENC_ISO_UTF8:
    {
      unsigned char *w1 = s1;
      switch (enc2) {
      case ENC_ISO_LATIN1:
	{
	  utf8proc_int32_t chr1, chr2;
	  unsigned char *w2 = s2;
	  for (i = 0; i < l; i++) { chr2 = *w2++; w1 += get_utf8(w1, &chr1); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
      case ENC_ISO_UTF8:
	{
	  utf8proc_int32_t chr1, chr2;
	  unsigned char *w2 = s2;
	  for (i = 0; i < l; i++) { w2 +=  get_utf8(w2, &chr2); w1 +=  get_utf8(w1, &chr1); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
    case ENC_WCHAR:
	{
	  utf8proc_int32_t chr1, chr2;
	  wchar_t *w2 = s2;
	  for (i = 0; i < l; i++) { chr2 = *w2++; w1 +=  get_utf8(w1, &chr1); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
      default:
	Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc2), __FUNCTION__);
     }
    }
  case ENC_WCHAR:
    {
      wchar_t *w1 = (wchar_t *)s1;
      switch (enc2) {
      case ENC_ISO_LATIN1:
	{
	  utf8proc_int32_t chr1, chr2;
	  char *w2 = s2;
	  for (i = 0; i < l; i++) { chr1 = *w1++; chr2 = *w2++; if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
      case ENC_ISO_UTF8:
	{
	  utf8proc_int32_t chr1, chr2;
	  unsigned char *w2 = s2;
	  for (i = 0; i < l; i++) { chr1 = *w1++; w2 +=  get_utf8(w2, &chr2); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
    case ENC_WCHAR:
	return wcsncmp(s1, s2, l);
      default:
	Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc2), __FUNCTION__);
      }
    }
    default:
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Unsupported Encoding ~s in %s", enc_name(enc1), __FUNCTION__);
  }
  return 0;
}

static void *
concat( int n, seq_tv_t *out, void *sv[], encoding_t encv[], size_t lengv[] USES_REGS )
{
  if (out->type == YAP_STRING_STRING) {
    /* we assume we concatenate strings only, or ASCII stuff like numbers */
    Term t = init_tstring( PASS_REGS1  );
    unsigned char *buf = buf_from_tstring(HR);
    int i;
    for (i = 0; i < n; i++) {
      if (encv[i] == ENC_WCHAR) {
	wchar_t *ptr = sv[i];
	utf8proc_int32_t chr;
	while ( (chr = *ptr++) ) buf += put_utf8(buf,  chr);
      } else if (encv[i] == ENC_ISO_LATIN1) {
	char *ptr = sv[i];
	utf8proc_int32_t chr;
	while ( (chr = *ptr++) ) buf += put_utf8(buf,  chr);
      } else {
	char *ptr = sv[i];
	utf8proc_int32_t chr;
	while ( (chr = *ptr++) ) *buf++ = chr;
      }
    }
    *buf ++ = '\0';
    close_tstring( buf  PASS_REGS );
    out->val.t = t;
    return HR;
  } else {
    encoding_t enc = ENC_ISO_LATIN1;
    size_t sz = 0;

    int i;
    for (i = 0; i < n; i++) {
      if (encv[i] != ENC_ISO_LATIN1) {
	enc = ENC_WCHAR;
      }
      sz += write_length(sv[i], out, encv[i], FALSE, lengv[i] PASS_REGS);
    }
    if (enc == ENC_WCHAR) {
      /* wide atom */
      wchar_t *buf = (wchar_t *)HR;
      Atom at;
      LOCAL_ERROR( MkAtomTerm(Yap_LookupWideAtom(buf)), sz+3 );
      for (i = 0; i < n ; i ++) {
	if (encv[i] == ENC_WCHAR) {
	  wchar_t *ptr = sv[i];
	  utf8proc_int32_t chr;
	  while ( (chr = *ptr++) != '\0' ) *buf++ = chr;
	} else if (encv[i] == ENC_ISO_LATIN1) {
	  char *ptr = sv[i];
	  utf8proc_int32_t chr;
	  while ( (chr = *ptr++) != '\0' ) *buf++ = (unsigned char)chr;
	} else {
	  unsigned char *ptr = sv[i];
	  utf8proc_int32_t chr;
	  while ( (ptr +=  get_utf8( ptr, &chr )) != NULL ) { if (chr == '\0') break; else *buf++ = chr; }
	}
      }
      *buf++ = '\0';
      at = out->val.a = Yap_LookupWideAtom((wchar_t *)HR);
      return at;
    } else {
      /* atom */
      char *buf = (char *)HR;
      Atom at;

      LOCAL_ERROR( MkAtomTerm(Yap_LookupAtom(buf)), sz/sizeof(CELL)+3 );
      for (i = 0; i < n ; i ++) {
	char *ptr = sv[i];
	utf8proc_int32_t chr;
	while ( (chr = *ptr++) != '\0' ) *buf++ = chr;
      }
      *buf++ = '\0';
      at = out->val.a = Yap_LookupAtom((const char *)HR);
      return at;
    }
  }
  return NULL;
}

static void *
slice( size_t min, size_t max, void *buf, seq_tv_t *out, encoding_t enc USES_REGS )
{
  if (out->type == YAP_STRING_STRING) {
    /* we assume we concatenate strings only, or ASCII stuff like numbers */
    Term t = init_tstring( PASS_REGS1  );
    unsigned char *nbuf = buf_from_tstring(HR);
    if (enc == ENC_WCHAR) {
      wchar_t *ptr = (wchar_t *)buf + min;
      utf8proc_int32_t chr;
      while ( min++ < max ) { chr = *ptr++; nbuf += put_utf8(nbuf, chr); }
    } else if (enc == ENC_ISO_LATIN1) {
      unsigned char *ptr = (unsigned char *)buf + min;
      utf8proc_int32_t chr;
      while ( min++ < max ) { chr = *ptr++; nbuf += put_utf8(nbuf, chr); }
    } else {
       unsigned char *ptr = skip_utf8 (buf, min );
      utf8proc_int32_t chr;
      if (!ptr) return NULL;
      while ( min++ < max ) { ptr +=  get_utf8(ptr, & chr); nbuf += put_utf8(nbuf, chr); }
    }
    *nbuf ++ = '\0';
    close_tstring( nbuf  PASS_REGS );
    out->val.t = t;
    return (void *)StringOfTerm(t);
  } else {
    Atom at;
     /* atom */
    if (enc == ENC_WCHAR) {
      /* wide atom */
      wchar_t *nbuf = (wchar_t *)HR;
      wchar_t *ptr = (wchar_t *)buf + min;
      if (max>min) {
	LOCAL_ERROR( MkAtomTerm(Yap_LookupWideAtom(buf)), (max-min)*sizeof(wchar_t) );
	memcpy( nbuf, ptr, (max - min)*sizeof(wchar_t));
      }
      nbuf[max-min] = '\0';
      at = Yap_LookupMaybeWideAtom( nbuf );
    } else if (enc == ENC_ISO_LATIN1) {
      /*  atom */
      char *nbuf = (char *)HR;

      if (max>min) {
	char *ptr = (char *)buf + min;
	LOCAL_ERROR( MkAtomTerm(Yap_LookupAtom(buf)), max-min );
	memcpy( nbuf, ptr, (max - min));
      }
      nbuf[max-min] = '\0';
      at = Yap_LookupAtom( nbuf );
    } else {
      /*  atom */
      wchar_t *nbuf = (wchar_t *)HR;
       unsigned char *ptr = skip_utf8 ( ( unsigned char *)buf, min );
      utf8proc_int32_t chr;

      LOCAL_ERROR( MkAtomTerm(Yap_LookupAtom(buf)), max-min );
      while ( min++ < max ) { ptr +=  get_utf8(ptr, & chr); *nbuf++ = chr; }
      nbuf[0] = '\0';
      at = Yap_LookupMaybeWideAtom( (wchar_t*)HR );
    }
    out->val.a = at;
    return at->StrOfAE;
  }
  return NULL;
}


//
// Out must be an atom or a string
void *
Yap_Concat_Text( int n,  seq_tv_t inp[], seq_tv_t *out USES_REGS)
{
  encoding_t * encv;
  void **bufv;
  int minimal = FALSE;
  void *buf;
  size_t leng, *lengv;
  int i;
  Term t = ARG1;
  bufv = (void **)malloc(n*sizeof(void *));
  HEAP_TERM_ERROR(bufv, void *);
  encv = (encoding_t *)malloc(n*sizeof(encoding_t));
  HEAP_ERROR(encv, encoding_t);
  buf = NULL;
  for (i = 0 ; i < n ; i++) {
    void *nbuf = Yap_readText( buf, inp+i, encv+i, &minimal, &leng PASS_REGS );

    if (!nbuf)
      return 0L;
    bufv[i] = nbuf;
    if ((char *)nbuf >= AuxBase &&  (char *)nbuf < AuxTop) {
      buf = compute_end( nbuf, encv[i] );
    }
  }
  lengv = (size_t *)malloc(n*sizeof(size_t));
  HEAP_ERROR(lengv, size_t);
  buf = concat(n, out, bufv, encv, lengv PASS_REGS);
  return buf;
}

//
// out must be an atom or a string
void *
Yap_Splice_Text( int n,  size_t cuts[], seq_tv_t *inp, encoding_t encv[], seq_tv_t outv[] USES_REGS)
{
  encoding_t enc;
  int minimal = FALSE;
  void *buf, *store;
  size_t l, leng;
  int i, min;

  buf = Yap_readText( NULL, inp, &enc, &minimal, &leng PASS_REGS );
  if (!buf)
    return NULL;
  l = write_length( buf, inp, enc, minimal, leng PASS_REGS);
  /* where to allocate next is the most complicated part */
  if ((char *)buf >= AuxBase &&  (char *)buf < AuxTop) {
    store = compute_end( buf, enc );
  } else {
    store = NULL;
  }


  if (!cuts) {
    if (n == 2) {
      size_t l0, l1;
      size_t leng0, leng1;
      encoding_t enc0, enc1;
      int minimal0, minimal1;
      void *buf0, *buf1;

      if (outv[0].val.t) {
	buf0 = Yap_readText( store, outv, &enc0, &minimal0, &leng0 PASS_REGS );
	if (!buf0)
	  return NULL;
	l0 = write_length( buf0, outv, enc, minimal0, leng0 PASS_REGS);
	if (cmp_Text( buf, buf0, l0, enc, enc0) != 0)
	  return NULL;

	l1 = l-l0;

	buf1 = slice(l0, l, buf, outv+1, enc PASS_REGS);
	if (encv)
	  encv[1] = enc;
	return buf1;
      } else /* if (outv[1].val.t) */ {
	buf1 = Yap_readText( store, outv+1, &enc1, &minimal1, &leng1 PASS_REGS );
	if (!buf1)
	  return NULL;
	l1 = write_length( buf1, outv+1, enc1, minimal1, leng1 PASS_REGS);
	if (l < l1) return NULL;
	l0 = l-l1;
	if (cmp_Text( advance_Text(buf, l0, enc), buf1, l1, enc, enc1) != 0)
	  return NULL;
	buf0 = slice(0, l0, buf, outv, enc PASS_REGS);
	if (encv)
	  encv[0] = enc;
	return buf0;
      }
    }
  }
  for (i = 0; i < n; i++) {
    if (i == 0) min = 0;
    else min = cuts[i-1];
    slice(min, cuts[i], buf, outv+i, enc PASS_REGS);
    if (!(outv[i].val.a))
      return NULL;
    if (encv)
      encv[i] = enc;
  }
  return (void *)outv;;
}
