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
#include "pl-shared.h"
#include "YapMirror.h"

#include <string.h>

static inline char *get_char(char *p, int *c) { *c = *p; return p+1; }

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
  if (VarOfTerm(v) > H && VarOfTerm(v) < LCL0) {
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

  if (atoms) {
    while (t != TermNil) {
      Atom at;
      if (IsWideAtom(at = AtomOfTerm(HeadOfTerm(t)))) {
	int i = RepAtom(at)->WStrOfAE[0];
	if (i <= 0 || i > 255) {
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

  if (atoms) {
    while (t != TermNil) {
      Atom at;
      if (IsWideAtom(at = AtomOfTerm(HeadOfTerm(t))))
	*s++ = RepAtom(at)->WStrOfAE[0];
      else
	*s++ = RepAtom(at)->StrOfAE[0];
      if (--max == 0) {
	*s++ = 0;
	return s0;
      }
      t = TailOfTerm(t);
    }
  } else {
    while (t != TermNil) {
      *s++ = IntOfTerm(HeadOfTerm(t));
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


static inline Term
init_tstring( USES_REGS1 ) { 
  Term t = AbsAppl(H);

  H[0] = (CELL)FunctorString;
  return t;
}

static inline char *
buf_from_tstring( CELL *p ) { 
  char *out = (char *)(p + 2);
  return out;
}

static inline void
close_tstring( char *p USES_REGS ) { 
  CELL *szp = H+1;
  H = (CELL *)ALIGN_YAPTYPE( p ,CELL);
  *szp = (H - szp)-1;
  *H++ = EndSpecials;
}


static Int
SkipListCodes(Term *l, Term **tailp, Int *atoms, int *wide)
{
  Int length = 0;
  Term *s; /* slow */
  Term v; /* temporary */

  do_derefa(v,l,derefa_unk,derefa_nonvar);
  s = l;

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
	  if (*atoms < length)
	    { *tailp = l; return -TYPE_ERROR_STRING; }
	  if (IsWideAtom(AtomOfTerm(hd))) {
	    if ((RepAtom(AtomOfTerm(hd))->WStrOfAE)[1] != '\0') { length = -REPRESENTATION_ERROR_CHARACTER; }
	    *wide = TRUE;
	  } else {
	    if ((RepAtom(AtomOfTerm(hd))->StrOfAE)[1] != '\0') { length = -REPRESENTATION_ERROR_CHARACTER_CODE; }
	  }
	} else if (IsIntTerm(hd)) {
	  Int ch = IntOfTerm(hd);
	  if ( *atoms || ch < 0) { *tailp = l; if (*atoms) length = -TYPE_ERROR_STRING; length = -DOMAIN_ERROR_NOT_LESS_THAN_ZERO; }
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
Yap_ListOfAtomsToBuffer(Term t, seq_tv_t *inp, int *widep USES_REGS)
{
  Int atoms = 0;
  CELL *r = NULL;
  Int n;

  widep = FALSE;
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
  if (n && !atoms)
    return NULL;
  if (*widep) {
    wchar_t *s = ((AtomEntry *)Yap_PreAllocCodeSpace())->WStrOfAE;
    AUX_ERROR( t, 2*(n+1), s, wchar_t);
    s = get_wide_from_list( t, inp, s, atoms PASS_REGS);
    return s;
  } else {
    char *s = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
    AUX_ERROR( t, 2*(n+1), s, char);
    s = get_string_from_list( t, inp, s, atoms PASS_REGS);
    return s;
  }
}

static void *
Yap_ListOfCodesToBuffer(Term t, seq_tv_t *inp, int *widep USES_REGS)
{
  Int atoms = 0;
  CELL *r = NULL;
  Int n;

  widep = FALSE;
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
  if (n && !atoms)
    return NULL;
  if (*widep) {
    wchar_t *s = ((AtomEntry *)Yap_PreAllocCodeSpace())->WStrOfAE;
    AUX_ERROR( t, 2*(n+1), s, wchar_t);
    s = get_wide_from_list( t, inp, s, atoms  PASS_REGS);
    return s;
  } else {
    char *s = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
    AUX_ERROR( t, 2*(n+1), s, char);
    s = get_string_from_list( t, inp, s, atoms PASS_REGS);
    return s;
  }
}

static void *
Yap_ListToBuffer(Term t, seq_tv_t *inp, int *widep USES_REGS)
{
  Int atoms = 0;
  CELL *r = NULL;
  Int n;

  widep = FALSE;
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
  if (*widep) {
    wchar_t *s = ((AtomEntry *)Yap_PreAllocCodeSpace())->WStrOfAE;
    AUX_ERROR( t, 2*(n+1), s, wchar_t);
    s = get_wide_from_list( t, inp, s, atoms  PASS_REGS);
    return s;
  } else {
    char *s = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
    AUX_ERROR( t, 2*(n+1), s, char);
    s = get_string_from_list( t, inp, s, atoms  PASS_REGS);
    return s;
  }
}

static void *
read_Text( seq_tv_t *inp, encoding_t *enc, int *minimal USES_REGS)
{
  char *s;
  wchar_t *ws;

  /* we know what the term is */
  switch (inp->type &  YAP_TYPE_MASK) {
  case YAP_STRING_STRING:
    { const char *s = StringOfTerm( inp->val.t );
      if ( s == NULL )
	return 0L;
      // this is a term, extract the UTF8 representation
      *enc = YAP_UTF8;
      *minimal = FALSE;
      return (void *)s;
    }
  case YAP_STRING_CODES:
    // this is a term, extract to a sfer, and representation is wide
    *minimal = TRUE;
    {
      int wide = FALSE;
      s = Yap_ListOfCodesToBuffer(inp->val.t, inp, &wide PASS_REGS);
      if (!s) return NULL;
      *enc = ( wide ? YAP_WCHAR : YAP_CHAR );
    }
    return s;
  case YAP_STRING_ATOMS:
    // this is a term, extract to a buffer, and representation is wide
    *minimal = TRUE;
    {
      int wide = FALSE;
      s = Yap_ListOfAtomsToBuffer(inp->val.t, inp, &wide PASS_REGS);
      if (!s) return NULL;
      if (wide) { *enc = YAP_WCHAR; }
      else { *enc = YAP_CHAR; }
    }
    return s;
  case YAP_STRING_ATOMS_CODES:
    // this is a term, extract to a buffer, and representation is wide
    *minimal = TRUE;
    {
      int wide = FALSE;
      s = Yap_ListToBuffer(inp->val.t, inp, &wide PASS_REGS);
      if (!s) return NULL;
      *enc = ( wide ? YAP_WCHAR : YAP_CHAR );
    }
    return s;
  case YAP_STRING_ATOM:
    // this is a term, extract to a buffer, and representation is wide
    *minimal = TRUE;
    {
      if (IsWideAtom(inp->val.a)) {
	ws = inp->val.a->WStrOfAE;
	*enc = YAP_WCHAR;
	return ws;
      } else {
	s = inp->val.a->StrOfAE;
	*enc = YAP_CHAR;
	return s;
      }
    }
    break;
 case YAP_STRING_INT:
    s = Yap_PreAllocCodeSpace();
    AUX_ERROR( MkIntTerm(inp->val.i), LOCAL_MAX_SIZE, s, char);
    if (snprintf(s, LOCAL_MAX_SIZE-1, Int_FORMAT, inp->val.i) < 0) {
      AUX_ERROR( MkIntTerm(inp->val.i), 2*LOCAL_MAX_SIZE, s, char);
    }
    *enc = YAP_CHAR;
    return s;
 case YAP_STRING_FLOAT:
    s = Yap_PreAllocCodeSpace();
    AUX_ERROR( MkFloatTerm(inp->val.f), LOCAL_MAX_SIZE, s, char);
    if ( !Yap_FormatFloat( inp->val.f, s, LOCAL_MAX_SIZE-1 ) ) {
      AUX_ERROR( MkFloatTerm(inp->val.f), 2*LOCAL_MAX_SIZE, s, char);
    }
    *enc = YAP_CHAR;
    return s;
 case YAP_STRING_BIG:
    s = Yap_PreAllocCodeSpace();
    if ( !Yap_mpz_to_string( inp->val.b, s, LOCAL_MAX_SIZE-1 , 10 ) ) {
      AUX_ERROR( MkIntTerm(0), LOCAL_MAX_SIZE, s, char);
    }
    *enc = YAP_CHAR;
    return s;
 case YAP_STRING_CHARS:
   *enc = YAP_CHAR;
   return (void *)inp->val.c;
 case YAP_STRING_WCHARS:
   *enc = YAP_WCHAR;
   return (void *)inp->val.w;
 case YAP_STRING_LITERAL:
   { 
     Int CurSlot = Yap_StartSlots( PASS_REGS1 );
     s = Yap_PreAllocCodeSpace();
     size_t sz = LOCAL_MAX_SIZE-1;
     IOSTREAM *fd;
     AUX_ERROR( inp->val.t, LOCAL_MAX_SIZE, s, char);
     fd = Sopenmem(&s, &sz, "w");
     fd->encoding = ENC_UTF8;
     if ( ! PL_write_term(fd, Yap_InitSlot(inp->val.t PASS_REGS), 1200, 0) ||
	  Sputcode(EOS, fd) < 0 ||
	  Sflush(fd) < 0 ) {
       LOCAL_CurSlot = CurSlot;
       AUX_ERROR( inp->val.t, LOCAL_MAX_SIZE, s, char);
     } else {
       LOCAL_CurSlot = CurSlot;
     }
     *enc = YAP_UTF8;
     return s;
   }
  default:
    if (!(inp->type & YAP_STRING_TERM)) {
      return NULL;
    } else {
      Term t = inp->val.t;
      if (IsStringTerm(t)) {
	if (inp->type & (YAP_STRING_STRING)) {
	  inp->type &= (YAP_STRING_STRING);
	  return read_Text( inp, enc, minimal PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = TYPE_ERROR_STRING;
	  LOCAL_Error_Term = t;
	}	 
      } else if (IsPairTerm(t)) {
	if (inp->type & (YAP_STRING_CODES|YAP_STRING_ATOMS)) {
	  inp->type &= (YAP_STRING_CODES|YAP_STRING_ATOMS);
	  return read_Text( inp, enc, minimal PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = TYPE_ERROR_LIST;
	  LOCAL_Error_Term = t;
	}
      } else if (IsAtomTerm(t)) {
	if (inp->type & (YAP_STRING_ATOM)) {
	  inp->type &= (YAP_STRING_ATOM);
	  inp->val.a = AtomOfTerm(t);
	  return read_Text( inp, enc, minimal PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = TYPE_ERROR_ATOM;
	  LOCAL_Error_Term = t;
	}	 
      } else if (IsIntegerTerm(t)) {
	if (inp->type & (YAP_STRING_INT)) {
	  inp->type &= (YAP_STRING_INT);
	  inp->val.i = IntegerOfTerm(t);
	  return read_Text( inp, enc, minimal PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = TYPE_ERROR_INTEGER;
	  LOCAL_Error_Term = t;
	}	 
      } else if (IsFloatTerm(t)) {
	if (inp->type & (YAP_STRING_FLOAT)) {
	  inp->type &= (YAP_STRING_FLOAT);
	  inp->val.f = FloatOfTerm(t);
	  return read_Text( inp, enc, minimal PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = TYPE_ERROR_FLOAT;
	  LOCAL_Error_Term = t;
	}	 
      } else if (IsBigIntTerm(t)) {
	if (inp->type & (YAP_STRING_BIG)) {
	  inp->type &= (YAP_STRING_BIG);
	  inp->val.b = Yap_BigIntOfTerm(t);
	  return read_Text( inp, enc, minimal PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = TYPE_ERROR_BIGNUM;
	  LOCAL_Error_Term = t;
	}	 
      }
      return NULL;
    }
  }
}

static Term
write_strings( void *s0, seq_tv_t *out, encoding_t enc, int minimal USES_REGS)
{
  size_t min = 0, max = -1;
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC) max = out->max;
  }
  switch (enc) {
  case YAP_UTF8:
    { char *s = s0, *lim = s + (max = strnlen(s, max));
      Term t = init_tstring( PASS_REGS1  );
      char *cp = s, *buf;

      LOCAL_ERROR( lim-s );
      buf = buf_from_tstring(H);
      while (cp < lim) {
	int chr;
	cp = utf8_get_char(cp, &chr);
	buf = utf8_put_char(buf, chr);
      }
      if (max >= min) *buf++ = '\0';
      else while (max < min) {
	max++;
	buf = utf8_put_char(buf, '\0');
      }
      
      close_tstring( buf  PASS_REGS );
      out->val.t = t;
    }
    break;
  case YAP_CHAR:
    { char *s = s0, *lim = s + (max = strnlen(s, max));
      Term t = init_tstring( PASS_REGS1  );
      char *cp = s, *buf;

      LOCAL_ERROR( lim-s );
      buf = buf_from_tstring(H);
      while (cp < lim) {
	int chr;
	cp = get_char(cp, &chr);
	buf = utf8_put_char(buf, chr);
      }
      if (max >= min) *buf++ = '\0';
      else while (max < min) {
	max++;
	buf = utf8_put_char(buf, '\0');
      }
      close_tstring( buf  PASS_REGS );
      out->val.t = t;
    }
    break;
  case YAP_WCHAR:
    { wchar_t *s = s0, *lim = s + (max = wcsnlen(s, max));
      Term t = init_tstring( PASS_REGS1  );
      wchar_t *wp = s;
      char *buf;

      LOCAL_ERROR( lim-s );
      buf = buf_from_tstring(H);
      while (wp < lim) {
	int chr;
	wp = get_wchar(wp, &chr);
	buf = utf8_put_char(buf, chr);
      }
      if (max >= min) *buf++ = '\0';
      else while (max < min) {
	max++;
	buf = utf8_put_char(buf, '\0');
      }
      close_tstring( buf  PASS_REGS );
      out->val.t = t;
    }
  }
  return out->val.t;
}

static Term
write_atoms( void *s0, seq_tv_t *out, encoding_t enc, int minimal USES_REGS)
{
  size_t min = 0, max = -1, sz = 0;
  Term t = AbsPair(H);

  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC) max = out->max;
  }
  switch (enc) {
  case YAP_UTF8:
    { char *s = s0, *lim = s + strnlen(s, max);
      char *cp = s;
      wchar_t w[2];
      w[1] = '\0';
      LOCAL_ERROR( lim-s );
      while (cp < lim) {
	int chr;
	cp = utf8_get_char(cp, &chr);
	w[0] = chr;
	H[0] = MkAtomTerm(Yap_LookupMaybeWideAtom(w));
	H[1] = AbsPair(H+2);
	H += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
  case YAP_CHAR:
    { char *s = s0, *lim = s + strnlen(s, max);
      char *cp = s;
      char w[2];
      w[1] = '\0';

      LOCAL_ERROR( lim-s );
      while (cp < lim) {
	int chr;
	cp = get_char(cp, &chr);
	w[0] = chr;
	H[0] = MkAtomTerm(Yap_LookupAtom(w));
	H[1] = AbsPair(H+2);
	H += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
 case YAP_WCHAR:
   { wchar_t *s = s0, *lim = s + wcsnlen(s, max);
      wchar_t *cp = s;
      wchar_t w[2];
      w[1] = '\0';

      LOCAL_ERROR( lim-s );
      while (cp < lim) {
	int chr;
	cp = get_wchar(cp, &chr);
	w[0] = chr;
	H[0] = MkAtomTerm(Yap_LookupMaybeWideAtom(w));
	H[1] = AbsPair(H+2);
	H += 2;
	sz++;
	if (sz == max) break;
      }
    }
  }
  while (sz < min) {
    H[0] = MkAtomTerm(AtomEmptyAtom);
    H[1] = AbsPair(H+2);
    H += 2;
    sz++;
  }
  if (out->type & YAP_STRING_DIFF) {
    if (sz == 0) t = out->dif;
    else H[-1] = Globalize(out->dif PASS_REGS);
  } else {
    if (sz == 0) t = TermNil;
    else H[-1] = TermNil;
  } 
  out->val.t = t;
  return (t);
}

static Term
write_codes( void *s0, seq_tv_t *out, encoding_t enc, int minimal USES_REGS)
{
  Term t = AbsPair(H);
  size_t min = 0, max = -1, sz = 0;
  
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC) max = out->max;
  }
  switch (enc) {
  case YAP_UTF8:
    { char *s = s0, *lim = s + strnlen(s, max);
      char *cp = s;
      LOCAL_ERROR( lim-s );
      while (cp < lim) {
	int chr;
	cp = utf8_get_char(cp, &chr);
	H[0] = MkIntTerm(chr);
	H[1] = AbsPair(H+2);
	H += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
  case YAP_CHAR:
    { char *s = s0, *lim = s + strnlen(s, max);
      char *cp = s;

      LOCAL_ERROR( lim-s );
      while (cp < lim) {
	int chr;
	cp = get_char(cp, &chr);
	H[0] = MkIntTerm(chr);
	H[1] = AbsPair(H+2);
	H += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
 case YAP_WCHAR:
    { wchar_t *s = s0, *lim = s + wcsnlen(s, max);
      wchar_t *cp = s;

      LOCAL_ERROR( lim-s );
      while (cp < lim) {
	int chr;
	cp = get_wchar(cp, &chr);
	H[0] = MkIntTerm(chr);
	H[1] = AbsPair(H+2);
	H += 2;
	sz++;
	if (sz == max) break;
      }
    }
  }
  while (sz < min) {
    H[0] = MkIntTerm(MkIntTerm(0));
    H[1] = AbsPair(H+2);
    H += 2;
    sz++;
  }
  if (out->type & YAP_STRING_DIFF) {
    if (sz == 0) t = out->dif;
    else H[-1] = Globalize(out->dif PASS_REGS);
  } else {
    if (sz == 0) t = TermNil;
    else H[-1] = TermNil;
  } 
  out->val.t = t;
  return (t);
}


static Atom
write_atom( void *s0, seq_tv_t *out, encoding_t enc, int minimal USES_REGS)
{
  size_t min = 0, max = -1;
  
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC) max = out->max;
  }

  switch (enc) {
  case YAP_UTF8:
    { char *s = s0, *lim = s + strnlen(s, max);
      wchar_t *buf = malloc(sizeof(wchar_t)*((lim+1)-s)), *ptr = buf;
      Atom at;

      while (s < lim) {
	int chr;
	s = utf8_get_char(s, &chr);
	*ptr++ = chr;
      }
      if (min > max) max = min;
      at = Yap_LookupMaybeWideAtomWithLength( buf, max );
      free( buf );
      out->val.a = at;
      return at;
    }
  case YAP_CHAR:
    { char *s = s0;
      Atom at;

      max = strnlen(s, max);
      if (min > max) {
	max = min;
      }
      at = Yap_LookupAtomWithLength(s, max);
      out->val.a = at;
      return at;
    }
 case YAP_WCHAR:
    { wchar_t *s = s0;
      Atom at;

      max = wcsnlen(s, max);
      if (min > max) {
	max = min;
      }
      out->val.a = at = Yap_LookupMaybeWideAtomWithLength(s, max);
      return at;
    }
  }
}

static Term
write_number( void *s0, seq_tv_t *out, encoding_t enc, int minimal USES_REGS)
{
  // call the scanner
  IOSTREAM *st;
  char *s = s0;
  Term t = 0L;
  fprintf(stderr,"s=%s\n",s);
  if ( (st=Sopenmem( &s, NULL, "r")) != NULL )
    { 
      if (enc == YAP_UTF8)
	st->encoding = ENC_UTF8;
      else if (enc == YAP_WCHAR)
	st->encoding = ENC_WCHAR;
      else
	st->encoding = ENC_OCTET;
      t = Yap_scan_num(st);
      Sclose(st);
        /* not ever iso */
      if (t == TermNil && yap_flags[LANGUAGE_MODE_FLAG] != 1) {
	s = s0;
	int sign = 1;
	if (s[0] == '+') {
	  s++;
	}
	if (s[0] == '-') {
	  s++;
	  sign = -1;
	}
	if(strcmp(s,"inf") == 0) {
	  if (sign > 0) {
	    return MkFloatTerm(INFINITY);
	  } else {
	    return MkFloatTerm(-INFINITY);
	  }
	}
	if(strcmp(s,"nan") == 0) {
	  if (sign > 0) {
	    return MkFloatTerm(NAN);
	  } else {
	    return MkFloatTerm(-NAN);
	  }
	}
      }
      if (t == TermNil)
	return 0;
      return t;
    }
  return 0L;
}

static Term
write_term( void *s0, seq_tv_t *out, encoding_t enc, int minimal USES_REGS)
{
  // call the scanner
  IOSTREAM *st;
  size_t len = out->sz;
  Term t = 0L;
  if ( (st=Sopenmem( s0, &len, "r")) )
    { 
      if (enc == YAP_UTF8)
	st->encoding = ENC_UTF8;
      else if (enc == YAP_WCHAR)
	st->encoding = ENC_WCHAR;
      else
	st->encoding = ENC_OCTET;
      return t;
    }
  return 0L;
}

static int
write_Text( void *inp, seq_tv_t *out, encoding_t enc, int minimal USES_REGS)
{

  /* we know what the term is */
  switch (out->type &  YAP_TYPE_MASK) {
  case YAP_STRING_STRING:
    out->val.t =
      write_strings( inp, out, enc, minimal PASS_REGS);
    return out->val.t != 0;
  case YAP_STRING_ATOMS:
    out->val.t =
      write_atoms( inp, out, enc, minimal PASS_REGS);
    return out->val.t != 0;    
  case YAP_STRING_CODES:
    out->val.t =
      write_codes( inp, out, enc, minimal PASS_REGS);
    return out->val.t != 0;
  case YAP_STRING_ATOM:
    out->val.a =
      write_atom( inp, out, enc, minimal PASS_REGS);
    return out->val.a != NULL;    
  case YAP_STRING_INT:
  case YAP_STRING_FLOAT:
  case YAP_STRING_BIG:
    out->val.t =
      write_number( inp, out, enc, minimal PASS_REGS);
    return out->val.t != 0;    
  case YAP_STRING_CHARS:
    out->val.c = inp;
    return 1;
  case YAP_STRING_WCHARS:
    out->val.w = inp;
    return MkIntTerm(0);
  case YAP_STRING_LITERAL:
    return 0;
  default:
    if (!(out->type & YAP_STRING_TERM))
      return 0;
    if (out->type & (YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG))
      if ((out->val.t =
	   write_number( inp, out, enc, minimal PASS_REGS)) != 0L)
	return out->val.t != 0;
    if (out->type & (YAP_STRING_ATOM))
      if (write_atom( inp, out, enc, minimal PASS_REGS) != NIL) {
	Atom at = out->val.a;
	if (at != NIL)
	  out->val.t = MkAtomTerm(at);
	return at != NIL;
      }
    if (out->type & (YAP_STRING_LITERAL))
      if ((out->val.t =
	   write_term( inp, out, enc, minimal PASS_REGS)) != 0L)
	return out->val.t != 0;
    return FALSE;
  }
}

int
Yap_CVT_Text( seq_tv_t *inp, seq_tv_t *out USES_REGS)
{
  encoding_t enc;
  int minimal = FALSE;
  char *buf;

  buf = read_Text( inp, &enc, &minimal PASS_REGS );
  if (!buf)
    return 0L;
  return write_Text( buf, out, enc, minimal PASS_REGS );
}

