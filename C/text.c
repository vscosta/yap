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

#include <string.h>
#include <wchar.h>

#ifndef HAVE_WCSNLEN
inline static size_t
min_size(size_t i, size_t j) {
  return ( i < j ?  i :  j );
}
#define wcsnlen(S, N) min_size(N, wcslen(S))
#endif

static inline unsigned char *getChar(unsigned char *p, int *c) { *c = *p; return p+1; }

static inline wchar_t *getWchar(wchar_t *p, int *c) { *c = *p; return p+1; }

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

static Int
SkipListCodes(unsigned char **bufp, Term *l, Term **tailp, Int *atoms, bool *wide, seq_tv_t *inp USES_REGS)
{
  Int length = 0;
  Term *s; /* slow */
  Term v; /* temporary */
  *wide = false;
  size_t max = 1;
  unsigned char *st0 = *bufp, *st;
  unsigned char *smax = NULL;

  do_derefa(v,l,derefa_unk,derefa_nonvar);
  *tailp = l;
  s = l;

  if (inp->type & YAP_STRING_TRUNC) {
    max = inp->max;
  } else {
    max = 0; // basically, this will never be reached;
  }

  if (!st0) {
    if (inp->type & YAP_STRING_MALLOC) {
      *bufp = st0 = (unsigned char *)malloc(MAXPATHLEN+1);
      smax = st0+(MAXPATHLEN-8); // give 8 bytes for max UTF-8 size + '\0';
    } else {
      *bufp = st0 = (unsigned char *)Yap_PreAllocCodeSpace();
      smax = (unsigned char *)AuxTop-8; // give 8 bytes for max UTF-8 size + '\0';
    }
  }  else if (inp->sz > 0) {
     smax = st0+(inp->sz-8); // give 8 bytes for max UTF-8 size + '\0';
  } else {
    // AUX_ERROR( *l, 2*(length+1), st0, unsigned char);
    return 0;
  }
  *bufp = st = st0;
  
  if (*l == TermNil) {
    return 0;
  }
  if ( IsPairTerm(*l) )
  {
    Term hd0 = HeadOfTerm(*l);
    if (IsVarTerm(hd0)) {
      return -INSTANTIATION_ERROR;
    }
    //are we looking for atoms/codes?
    // whatever the case, we should be consistent throughout,
    // so we should be consistent with the first arg.
    if (*atoms == 1) {
      if ( !IsIntegerTerm(hd0) ) {
	return -INSTANTIATION_ERROR;
      }
    } else if (*atoms == 2) {
      if ( !IsAtomTerm(hd0) ) {
	return -TYPE_ERROR_ATOM;
      }
    }
 
    do { 
      int ch;
      length++;
      if (length == max) {
        *st++ = '\0';
      }
      { Term hd = Deref(RepPair(*l)[0]);
	if (IsVarTerm(hd)) {
	  return  -INSTANTIATION_ERROR;
	} else if (IsAtomTerm(hd)) {
	  (*atoms)++;
	  if (*atoms < length)
	     { *tailp = l; return -TYPE_ERROR_NUMBER; }
	  if (IsWideAtom(AtomOfTerm(hd))) {
	    int ch;
	    if ((RepAtom(AtomOfTerm(hd))->WStrOfAE)[1] != '\0') {
	      length = -REPRESENTATION_ERROR_CHARACTER;
	    }
	    ch = RepAtom(AtomOfTerm(hd))->WStrOfAE[0];
	    *wide = true;
	  } else {
	    AtomEntry *ae = RepAtom(AtomOfTerm(hd));
	    if ((ae->StrOfAE)[1] != '\0') {
	      length = -REPRESENTATION_ERROR_CHARACTER;
	    } else {
	      ch = RepAtom(AtomOfTerm(hd))->StrOfAE[0];
	      *wide |= ch > 0x80;
	    }
	  }
	} else if (IsIntegerTerm(hd)) {
	   ch = IntegerOfTerm(hd);
	  if (*atoms) length = -TYPE_ERROR_ATOM;
	  else if (ch < 0) {
	    *tailp = l;
	    length = -DOMAIN_ERROR_NOT_LESS_THAN_ZERO;
	  } else {
	    *wide |= ch > 0x80;
	  }
	} else {
	  length = -TYPE_ERROR_INTEGER;
	}
	if (length < 0) {
	  *tailp = l;
	  return length;
	}
      }
      // now copy char to buffer
      size_t chsz = put_utf8( st, ch );
      if (smax <= st+chsz) {
        *st++ = '\0';
        *tailp = l;
        return length;
      } else {
        st += chsz;
      }
      l = RepPair(*l)+1;
      do_derefa(v,l,derefa2_unk,derefa2_nonvar);
    } while ( *l != *s && IsPairTerm(*l) );
  }
  if (IsVarTerm(*l)) {
	  return  -INSTANTIATION_ERROR;
  }
  if (   *l != TermNil) {
	  return  -TYPE_ERROR_LIST;
  }
  st[0] = '\0';
  *tailp = l;

  return length;
}

static void *
to_buffer(void *buf, Term t, seq_tv_t *inp, bool *widep, Int *atoms, size_t *lenp USES_REGS)
{
  CELL *r = NULL;
  Int n;

  if (!buf) {
    inp->sz = *lenp;
  }
  unsigned char *bufc = buf;
  n = SkipListCodes(&bufc, &t, &r, atoms, widep, inp PASS_REGS);
  if (n < 0) {
    LOCAL_Error_TYPE = -n;
    LOCAL_Error_Term = *r;
    return NULL;
  }
  *lenp = n;
  return bufc;  
}

static void *
Yap_ListOfCodesToBuffer(void *buf, Term t, seq_tv_t *inp, bool *widep, size_t *lenp USES_REGS)
{
  Int atoms = 1; // we only want lists of atoms
  return to_buffer( buf, t, inp, widep, &atoms, lenp PASS_REGS);
}

static void *
Yap_ListOfAtomsToBuffer(void *buf, Term t, seq_tv_t *inp, bool *widep, size_t *lenp USES_REGS)
{
  Int atoms = 2; // we only want lists of integer codes
  return to_buffer( buf, t, inp, widep, &atoms, lenp PASS_REGS);
}

static void *
Yap_ListToBuffer(void *buf, Term t, seq_tv_t *inp, bool *widep, size_t *lenp USES_REGS)
{
  Int atoms = 0; // we accept both types of lists.
  return to_buffer( buf, t, inp, widep, &atoms, lenp PASS_REGS);
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
  char  *s, *s0 = buf;
  wchar_t *ws;
  bool wide;

  /* we know what the term is */
  if ( !(inp->type & (YAP_STRING_CHARS|YAP_STRING_WCHARS)))
    {
      if ( !(inp->type & YAP_STRING_TERM)) {
        if (IsVarTerm(inp->val.t)) {
          LOCAL_Error_TYPE = INSTANTIATION_ERROR;
        } else if (!IsAtomTerm(inp->val.t) && inp->type == YAP_STRING_ATOM) {
          LOCAL_Error_TYPE = TYPE_ERROR_ATOM;
        } else if (!IsStringTerm(inp->val.t) && inp->type == YAP_STRING_STRING) {
          LOCAL_Error_TYPE = TYPE_ERROR_STRING;
        }else if (!IsPairTerm(inp->val.t) &&
                  !IsStringTerm(inp->val.t) &&
                  inp->type == (YAP_STRING_ATOMS_CODES|YAP_STRING_STRING)) {
          LOCAL_Error_TYPE = TYPE_ERROR_LIST;
        } else if (!IsNumTerm(inp->val.t) && (inp->type & ( YAP_STRING_INT|YAP_STRING_FLOAT| YAP_STRING_BIG)) == inp->type) {
          LOCAL_Error_TYPE = TYPE_ERROR_NUMBER;
        }
        LOCAL_Error_Term = inp->val.t;
      }
    }
  if (LOCAL_Error_TYPE != YAP_NO_ERROR)
    return NULL;
  
    // this is a term, extract the UTF8 representation
  if ( IsStringTerm(inp->val.t) &&
       inp->type & YAP_STRING_STRING) {
    const char *s = StringOfTerm(inp->val.t);
    *enc = ENC_ISO_UTF8;
    *minimal = FALSE;
    if (lengp)
      *lengp = strlen(s);
    return (void *)s;
  }
   if ( IsAtomTerm(inp->val.t)
	&& inp->type & YAP_STRING_ATOM) {
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
   if (((inp->type &(YAP_STRING_CODES|YAP_STRING_ATOMS)) ==
	(YAP_STRING_CODES|YAP_STRING_ATOMS))) {
	 s = Yap_ListToBuffer( s0, inp->val.t, inp, &wide, lengp PASS_REGS);
    // this is a term, extract to a sfer, and representation is wide
    *minimal = true;
    *enc = ENC_ISO_UTF8;
    return s;
  }
       if (inp->type == YAP_STRING_CODES) {
	 s = Yap_ListOfCodesToBuffer( s0, inp->val.t, inp, &wide, lengp PASS_REGS);
    // this is a term, extract to a sfer, and representation is wide
    *minimal = true;
    *enc = ENC_ISO_UTF8;
    return s;
       }
       if (inp->type == YAP_STRING_ATOMS) {
	 s = Yap_ListOfAtomsToBuffer( s0, inp->val.t, inp, &wide, lengp PASS_REGS);
    // this is a term, extract to a buffer, and representation is wide
    *minimal = true;
    *enc = ENC_ISO_UTF8;
    return s;
  }
  if (inp->type & YAP_STRING_INT && IsIntegerTerm(inp->val.t)) {
    if (s0) s = s0;
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
      if (s0) s = s0;
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
      if (s0) s = s0;
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
      if (s0) s = s0;
      else s = Yap_PreAllocCodeSpace();
      size_t sz = LOCAL_MAX_SIZE-1;
      encoding_t enc = ENC_ISO_UTF8;
      o = Yap_TermToString(inp->val.t, s, sz, lengp, &enc, 0);
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
	cp +=  get_utf8(cp, -1, &chr);
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
	cp = getChar(cp, &chr);
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
	wp = getWchar(wp, &chr);
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
	cp +=  get_utf8(cp, -1, &chr);
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
	cp = getChar(cp, &chr);
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
	cp = getWchar(cp, &chr);
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
	cp +=  get_utf8(cp, -1, &chr);
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
	cp = getChar(cp, &chr);
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
	cp = getWchar(cp, &chr);
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
      wchar_t *buf = malloc(sizeof(wchar_t)*((lim+2)-s)), *ptr = buf;
      Atom at;

      while (*s && s < lim) {
	utf8proc_int32_t chr;
	s +=  get_utf8(s,-1, &chr);
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
	  cp +=  get_utf8(cp, -1, &chr);
	  *buf++ = chr;
	}
	if (max >= min) *buf++ = '\0';
	else while (max < min) {
	  utf8proc_int32_t chr;
	  max++;
	  cp +=  get_utf8(cp, -1, &chr);
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


 size_t
write_buffer( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  size_t min = 0, max = leng, sz_end;
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC && out->max < max) max = out->max;
  }
  if (out->enc != enc) {
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
	out->val.c = Yap_PreAllocCodeSpace();
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

	buf = buf0 = s0;
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
	  cp +=  get_utf8(cp, -1, &chr);
	  *buf++ = chr;
	}
	if (max >= min) *buf++ = '\0';
	else while (max < min) {
	  utf8proc_int32_t chr;
	  max++;
	  cp +=  get_utf8(cp, -1, &chr);
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



static size_t
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
  return
    Yap_StringToNumberTerm(s0, &enc);
}

static Term
string_to_term( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
 Term o = out->val.t = Yap_StringToTerm(s0, strlen(s0)+1, &enc, GLOBAL_MaxPriority, NULL);
 return o;
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
    return out->val.l != (size_t)(-1);
  case YAP_STRING_ATOM:
    out->val.a =
      write_atom( inp, out, enc, minimal, leng PASS_REGS);
    return out->val.a != NULL;
  case YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG:
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
	  for (i = 0; i < l; i++) { chr1 = *w1++; w2 +=  get_utf8(w2, -1, &chr2); if (chr1-chr2) return chr1-chr2; }
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
	  for (i = 0; i < l; i++) { chr2 = *w2++; w1 += get_utf8(w1, -1, &chr1); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
      case ENC_ISO_UTF8:
	{
	  utf8proc_int32_t chr1, chr2;
	  unsigned char *w2 = s2;
	  for (i = 0; i < l; i++) { w2 +=  get_utf8(w2, -1, &chr2); w1 +=  get_utf8(w1,-1, &chr1); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
    case ENC_WCHAR:
	{
	  utf8proc_int32_t chr1, chr2;
	  wchar_t *w2 = s2;
	  for (i = 0; i < l; i++) { chr2 = *w2++; w1 +=  get_utf8(w1, -1, &chr1); if (chr1-chr2) return chr1-chr2; }
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
	  for (i = 0; i < l; i++) { chr1 = *w1++; w2 +=  get_utf8(w2, -1, &chr2); if (chr1-chr2) return chr1-chr2; }
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
	  while ( (ptr +=  get_utf8( ptr, -1, &chr )) != NULL ) { if (chr == '\0') break; else *buf++ = chr; }
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
      while ( min++ < max ) { ptr +=  get_utf8(ptr, -1, & chr); nbuf += put_utf8(nbuf, chr); }
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
      while ( min++ < max ) { ptr +=  get_utf8(ptr, -1, & chr); *nbuf++ = chr; }
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

/** 
 * Function to convert a generic text term (string, atom, list of codes, list of atoms)  into a buff
er.
 * 
 * @param t     the term
 * @param buf   the buffer, if NULL a buffer is malloced, and the user should reclai it 
 * @param len   buffer size
 * @param enc   encoding (UTF-8 is strongly recommended)
 * 
 * @return the buffer, or NULL in case of failure. If so, Yap_Error may be called.
 */
const char *
Yap_TextTermToText(Term t, char *buf, size_t len)
{ CACHE_REGS
  seq_tv_t inp, out;
  encoding_t enc = LOCAL_encoding;
  
  inp.val.t = t;
  if (IsAtomTerm(t))
    inp.type = YAP_STRING_ATOM;
  else if (IsStringTerm(t))
    inp.type = YAP_STRING_STRING;
  else if (IsPairTerm(t) )
    inp.type = (YAP_STRING_CODES|YAP_STRING_ATOMS);
  else {
    Yap_Error(TYPE_ERROR_TEXT, t,  NULL);
    return false;
  }
  out.enc = enc;
  out.type = YAP_STRING_CHARS;
  if (!buf) {
    inp.type |= YAP_STRING_MALLOC;
    out.type |= YAP_STRING_MALLOC;
  }
  out.val.c = buf;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return NULL;
  return out.val.c;
}

  
/** 
 * Convert from a text buffer (8-bit) to a term that has the same type as _Tguide_
 * 
 * @param s        the buffer
 * @param tguide   the guide
 * 
 * @return the term
 */
Term Yap_MkTextTerm(const char *s,
                                            Term tguide ) {
CACHE_REGS
  if (IsAtomTerm(tguide))
    return MkAtomTerm(Yap_LookupAtom(s));
 if (IsStringTerm(tguide))
    return MkStringTerm(s);
 if (IsPairTerm(tguide) && IsAtomTerm(HeadOfTerm(tguide))) {
   return Yap_CharsToListOfAtoms( s, LOCAL_encoding  PASS_REGS );
 }
 return Yap_CharsToListOfCodes( s, LOCAL_encoding  PASS_REGS );
}
