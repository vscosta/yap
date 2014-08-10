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
SkipListCodes(Term *l, Term **tailp, Int *atoms, int *wide)
{
  Int length = 0;
  Term *s; /* slow */
  Term v; /* temporary */

  do_derefa(v,l,derefa_unk,derefa_nonvar);
  s = l;


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
	} else if (IsIntTerm(hd)) {
	  Int ch = IntOfTerm(hd);
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
Yap_ListOfAtomsToBuffer(void *buf, Term t, seq_tv_t *inp, int *widep, size_t *lenp USES_REGS)
{
  Int atoms = 0;
  CELL *r = NULL;
  Int n;

  *widep = FALSE;
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
    else s = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
    AUX_ERROR( t, 2*(n+1), s, char);
    s = get_string_from_list( t, inp, s, atoms PASS_REGS);
    return s;
  }
}

static void *
Yap_ListOfCodesToBuffer(void *buf, Term t, seq_tv_t *inp, int *widep, size_t *lenp USES_REGS)
{
  Int atoms = 0;
  CELL *r = NULL;
  Int n;

  *widep = FALSE;
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
    AUX_ERROR( t, 2*(n+1), s, char);
    s = get_string_from_list( t, inp, s, atoms PASS_REGS);
    return s;
  }
}

static void *
Yap_ListToBuffer(void *buf, Term t, seq_tv_t *inp, int *widep, size_t *lenp USES_REGS)
{
  Int atoms = 0;
  CELL *r = NULL;
  Int n;

  *widep = FALSE;
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
    AUX_ERROR( t, 2*(n+1), s, char);
    s = get_string_from_list( t, inp, s, atoms  PASS_REGS);
    return s;
  }
}

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

static void *
read_Text( void *buf, seq_tv_t *inp, encoding_t *enc, int *minimal, size_t *lengp USES_REGS)
{
  char *s;
  wchar_t *ws;

  /* we know what the term is */
  switch (inp->type &  YAP_TYPE_MASK) {
  case YAP_STRING_STRING:
    { const char *s;
      if (IsVarTerm(inp->val.t)) {
	LOCAL_Error_TYPE = INSTANTIATION_ERROR;
	LOCAL_Error_Term = inp->val.t;
	return 0L;     	
      }
      if (!IsStringTerm(inp->val.t)) {
	LOCAL_Error_TYPE = TYPE_ERROR_STRING;
	LOCAL_Error_Term = inp->val.t;
	return 0L;     	
      }
      s = StringOfTerm( inp->val.t );
      if ( s == NULL )
	return 0L;
      // this is a term, extract the UTF8 representation
      *enc = YAP_UTF8;
      *minimal = FALSE;
      *lengp = strlen(s);
      return (void *)s;
    }
  case YAP_STRING_CODES:
    // this is a term, extract to a sfer, and representation is wide
    *minimal = TRUE;
    {
      int wide = FALSE;
      s = Yap_ListOfCodesToBuffer( buf, inp->val.t, inp, &wide, lengp PASS_REGS);
      if (!s) return NULL;
      *enc = ( wide ? YAP_WCHAR : YAP_CHAR );
    }
    return s;
  case YAP_STRING_ATOMS:
    // this is a term, extract to a buffer, and representation is wide
    *minimal = TRUE;
    {
      int wide = FALSE;
      s = Yap_ListOfAtomsToBuffer( buf, inp->val.t, inp, &wide, lengp PASS_REGS);
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
      s = Yap_ListToBuffer( buf, inp->val.t, inp, &wide, lengp PASS_REGS);
      if (!s) return NULL;
      *enc = ( wide ? YAP_WCHAR : YAP_CHAR );
    }
    return s;
  case YAP_STRING_ATOM:
    // this is a term, extract to a buffer, and representation is wide
    *minimal = TRUE;
    if (IsVarTerm(inp->val.t)) {
      LOCAL_Error_TYPE = INSTANTIATION_ERROR;
      LOCAL_Error_Term = inp->val.t;
      return 0L;     	
    } else if (!IsAtomTerm(inp->val.t)) {
      LOCAL_Error_TYPE = TYPE_ERROR_ATOM;
      LOCAL_Error_Term = inp->val.t;
      return NULL;     
    } else {
      Atom at = AtomOfTerm(inp->val.t);
      if (IsWideAtom(at)) {
	ws = at->WStrOfAE;
	*lengp = wcslen(ws);
	*enc = YAP_WCHAR;
	return ws;
      } else {
	s = at->StrOfAE;
	*lengp = strlen(s);
	*enc = YAP_CHAR;
	return s;
      }
    }
    break;
  case YAP_STRING_INT:
    if (buf) s = buf;
    else s = Yap_PreAllocCodeSpace();
    AUX_ERROR( MkIntTerm(inp->val.i), LOCAL_MAX_SIZE, s, char);
    if (snprintf(s, LOCAL_MAX_SIZE-1, Int_FORMAT, inp->val.i) < 0) {
      AUX_ERROR( MkIntTerm(inp->val.i), 2*LOCAL_MAX_SIZE, s, char);
    }
    *enc = YAP_CHAR;
    *lengp = strlen(s);
    return s;
  case YAP_STRING_FLOAT:
    if (buf) s = buf;
    else s = Yap_PreAllocCodeSpace();
    AUX_ERROR( MkFloatTerm(inp->val.f), LOCAL_MAX_SIZE, s, char);
    if ( !Yap_FormatFloat( inp->val.f, s, LOCAL_MAX_SIZE-1 ) ) {
      AUX_ERROR( MkFloatTerm(inp->val.f), 2*LOCAL_MAX_SIZE, s, char);
    }
    *lengp = strlen(s);
    *enc = YAP_CHAR;
    return s;
#if USE_GMP
  case YAP_STRING_BIG:
    if (buf) s = buf;
    else s = Yap_PreAllocCodeSpace();
    if ( !Yap_mpz_to_string( inp->val.b, s, LOCAL_MAX_SIZE-1 , 10 ) ) {
      AUX_ERROR( MkIntTerm(0), LOCAL_MAX_SIZE, s, char);
    }
    *enc = YAP_CHAR;
    *lengp = strlen(s);
    return s;
#endif
  case YAP_STRING_CHARS:
    *enc = YAP_CHAR;
    if (inp->type & YAP_STRING_NCHARS)
      *lengp = inp->sz;
    else
      *lengp = strlen(inp->val.c);
    return (void *)inp->val.c;
  case YAP_STRING_WCHARS:
    *enc = YAP_WCHAR;
    if (inp->type & YAP_STRING_NCHARS)
      *lengp = inp->sz;
    else
      *lengp = wcslen(inp->val.w);
    return (void *)inp->val.w;
  case YAP_STRING_LITERAL:
    { 
      Int CurSlot = Yap_StartSlots( PASS_REGS1 );
      if (buf) s = buf;
      else s = Yap_PreAllocCodeSpace();
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
      *lengp = strlen(s);
      return s;
    }
  default:
    if (!(inp->type & YAP_STRING_TERM)) {
      return NULL;
    } else {
      Term t = inp->val.t;
      if (IsVarTerm(t)) {
	LOCAL_Error_TYPE = INSTANTIATION_ERROR;
	LOCAL_Error_Term = t;
	return NULL;
      } else if (IsStringTerm(t)) {
	if (inp->type & (YAP_STRING_STRING)) {
	  inp->type &= (YAP_STRING_STRING);
	  return read_Text( buf, inp, enc, minimal, lengp PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = gen_type_error( inp->type );
	  LOCAL_Error_Term = t;
	}	 
      } else if (IsPairTerm(t) ) {
	if (inp->type & (YAP_STRING_CODES|YAP_STRING_ATOMS)) {
	  inp->type &= (YAP_STRING_CODES|YAP_STRING_ATOMS);
	  return read_Text( buf, inp, enc, minimal, lengp PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = gen_type_error( inp->type );
	  LOCAL_Error_Term = t;
	}
      } else if (IsAtomTerm(t)) {
	if (t == TermNil && inp->type & (YAP_STRING_CODES|YAP_STRING_ATOMS)) {
	  inp->type &= (YAP_STRING_CODES|YAP_STRING_ATOMS);
	  return read_Text( buf, inp, enc, minimal, lengp PASS_REGS);
	} else if (inp->type & (YAP_STRING_ATOM)) {
	  inp->type &= (YAP_STRING_ATOM);
	  inp->val.t = t;
	  return read_Text( buf, inp, enc, minimal, lengp PASS_REGS);
	  // [] is special...
	} else {
	  LOCAL_Error_TYPE = gen_type_error( inp->type );
	  LOCAL_Error_Term = t;
	}	 
      } else if (IsIntegerTerm(t)) {
	if (inp->type & (YAP_STRING_INT)) {
	  inp->type &= (YAP_STRING_INT);
	  inp->val.i = IntegerOfTerm(t);
	  return read_Text( buf, inp, enc, minimal, lengp PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = gen_type_error( inp->type );
	  LOCAL_Error_Term = t;
	}	 
      } else if (IsFloatTerm(t)) {
	if (inp->type & (YAP_STRING_FLOAT)) {
	  inp->type &= (YAP_STRING_FLOAT);
	  inp->val.f = FloatOfTerm(t);
	  return read_Text( buf, inp, enc, minimal, lengp PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = gen_type_error( inp->type );
	  LOCAL_Error_Term = t;
	}	 
#if USE_GMP
      } else if (IsBigIntTerm(t)) {
	if (inp->type & (YAP_STRING_BIG)) {
	  inp->type &= (YAP_STRING_BIG);
	  inp->val.b = Yap_BigIntOfTerm(t);
	  return read_Text( buf, inp, enc, minimal, lengp PASS_REGS);
	} else {
	  LOCAL_Error_TYPE = gen_type_error( inp->type );
	  LOCAL_Error_Term = t;
	}	 
#endif
      } else {
	if (!Yap_IsGroundTerm(t)) {
	  LOCAL_Error_TYPE = INSTANTIATION_ERROR;
	  LOCAL_Error_Term = t;
	}
      }
      return NULL;
    }
  }
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
  case YAP_UTF8:
    { char *s = s0, *lim = s + (max = strnlen(s, max));
      Term t = init_tstring( PASS_REGS1  );
      char *cp = s, *buf;

      LOCAL_TERM_ERROR( 2*(lim-s) );
      buf = buf_from_tstring(HR);
      while (*cp && cp < lim) {
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
    { unsigned char *s = s0, *lim = s + (max = strnlen(s0, max));
      Term t = init_tstring( PASS_REGS1  );
      unsigned char *cp = s;
      char *buf;

      LOCAL_TERM_ERROR( 2*(lim-s) );
      buf = buf_from_tstring(HR);
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

      LOCAL_TERM_ERROR( 2*(lim-s) );
      buf = buf_from_tstring(HR);
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
write_atoms( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
{
  Term t = AbsPair(HR);
  size_t sz = 0;
  size_t min = 0, max = leng;
  if (out->type & (YAP_STRING_NCHARS|YAP_STRING_TRUNC)) {
    if (out->type & YAP_STRING_NCHARS) min = out->sz;
    if (out->type & YAP_STRING_TRUNC && out->max < max) max = out->max;
  }

  switch (enc) {
  case YAP_UTF8:
    { char *s = s0, *lim = s + strnlen(s, max);
      char *cp = s;
      wchar_t w[2];
      w[1] = '\0';
      LOCAL_TERM_ERROR( 2*(lim-s) );
      while (cp < lim) {
	int chr;
	cp = utf8_get_char(cp, &chr);
	w[0] = chr;
	HR[0] = MkAtomTerm(Yap_LookupMaybeWideAtom(w));
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
  case YAP_CHAR:
    { unsigned char *s = s0, *lim = s + strnlen(s0, max);
      unsigned char *cp = s;
      char w[2];
      w[1] = '\0';

      LOCAL_TERM_ERROR( 2*(lim-s) );
      while (cp < lim) {
	int chr;
	cp = get_char(cp, &chr);
	w[0] = chr;
	HR[0] = MkAtomTerm(Yap_LookupAtom(w));
	HR[1] = AbsPair(HR+2);
	HR += 2;
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

      LOCAL_TERM_ERROR( 2*(lim-s) );
      while (*cp && cp < lim) {
	int chr;
	cp = get_wchar(cp, &chr);
	w[0] = chr;
	HR[0] = MkAtomTerm(Yap_LookupMaybeWideAtom(w));
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
    }
  }
  while (sz < min) {
    HR[0] = MkAtomTerm(AtomEmptyAtom);
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
  case YAP_UTF8:
    { char *s = s0, *lim = s + strnlen(s, max);
      char *cp = s;
      LOCAL_TERM_ERROR( 2*(lim-s) );
      while (*cp && cp < lim) {
	int chr;
	cp = utf8_get_char(cp, &chr);
	HR[0] = MkIntTerm(chr);
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
  case YAP_CHAR:
    { unsigned char *s = s0, *lim = s + strnlen(s0, max);
      unsigned char *cp = s;

      LOCAL_TERM_ERROR( 2*(lim-s) );
      while (cp < lim) {
	int chr;
	cp = get_char(cp, &chr);
	HR[0] = MkIntTerm(chr);
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
      break;
    }
 case YAP_WCHAR:
    { wchar_t *s = s0, *lim = s + wcsnlen(s, max);
      wchar_t *cp = s;

      LOCAL_TERM_ERROR( 2*(lim-s) );
      while (cp < lim) {
	int chr;
	cp = get_wchar(cp, &chr);
	HR[0] = MkIntTerm(chr);
	HR[1] = AbsPair(HR+2);
	HR += 2;
	sz++;
	if (sz == max) break;
      }
    }
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
  case YAP_UTF8:
    { char *s = s0, *lim = s + strnlen(s, max);
      wchar_t *buf = malloc(sizeof(wchar_t)*((lim+1)-s)), *ptr = buf;
      Atom at;

      while (*s && s < lim) {
	int chr;
	s = utf8_get_char(s, &chr);
	*ptr++ = chr;
      }
      *ptr++ = '\0';
      at = Yap_LookupMaybeWideAtomWithLength( buf, max );
      free( buf );
      out->val.a = at;
      return at;
    }
  case YAP_CHAR:
    { char *s = s0;
      Atom at;

      max = strnlen(s, max);
      at = Yap_LookupAtomWithLength(s, max);
      out->val.a = at;
      return at;
    }
 case YAP_WCHAR:
    { wchar_t *s = s0;
      Atom at;

      max = wcsnlen(s, max);
      out->val.a = at = Yap_LookupMaybeWideAtomWithLength(s, max);
      return at;
    }
  }
  return NULL;
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
  case YAP_UTF8:
    {
      const char *s = s0;
      return utf8_strlen1(s);
    }
  case YAP_CHAR:
    {
      const char *s = s0;
      return strnlen(s, max);
    }
  case YAP_WCHAR:
    {
      const wchar_t *s = s0;
      return wcsnlen(s, max);
    }
  }
  return (size_t)-1;
}

static Term
write_number( void *s0, seq_tv_t *out, encoding_t enc, int minimal, int size USES_REGS)
{
  // call the scanner
  IOSTREAM *st;
  char *s = s0;
  Term t = 0L;
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
write_term( void *s0, seq_tv_t *out, encoding_t enc, int minimal, size_t leng USES_REGS)
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
	   write_number( inp, out, enc, minimal, leng PASS_REGS)) != 0L)
	return out->val.t != 0;
    if (out->type & (YAP_STRING_ATOM))
      if (write_atom( inp, out, enc, minimal, leng PASS_REGS) != NIL) {
	Atom at = out->val.a;
	if (at != NIL)
	  out->val.t = MkAtomTerm(at);
	return at != NIL;
      }
    if (out->type & (YAP_STRING_LITERAL))
      if ((out->val.t =
	   write_term( inp, out, enc, minimal, leng PASS_REGS)) != 0L)
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
  size_t leng;

  buf = read_Text( NULL, inp, &enc, &minimal, &leng PASS_REGS );
  if (!buf)
    return 0L;
  return write_Text( buf, out, enc, minimal, leng PASS_REGS );
}

static void *
compute_end( void *s0, encoding_t enc )
{
  switch (enc) {
  case YAP_CHAR:
  case YAP_UTF8:
    {
      char *s = (char *)s0;
      return s+(1+strlen(s));
    }
  case YAP_WCHAR:
    {
      wchar_t *s = (wchar_t *)s0;
      return s + (1+wcslen(s));
    }
  }
  return NULL;
}

static void *
advance_Text( void *s, int l, encoding_t enc )
{
  switch (enc) {
  case YAP_CHAR:
    return ((char *)s)+l;
  case YAP_UTF8:
    return (char *)utf8_skip((const char *)s,l);
  case YAP_WCHAR:
    return ((wchar_t *)s)+l;
  }
  return s;
}

static int
cmp_Text( void *s1, void *s2, int l, encoding_t enc1, encoding_t enc2 )
{
  Int i;
  switch (enc1) {
  case YAP_CHAR:
    {
      char *w1 = (char *)s1;
      switch (enc2) {
      case YAP_CHAR:
	return strncmp(s1, s2, l);
      case YAP_UTF8:
	{
	  int chr1, chr2;
	  char *w2 = s2;
	  for (i = 0; i < l; i++) { chr1 = *w1++; w2 = utf8_get_char(w2, &chr2); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
    case YAP_WCHAR:
	{
	  int chr1, chr2;
	  wchar_t *w2 = s2;
	  for (i = 0; i < l; i++) { chr1 = *w1++; chr2 = *w2++; if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
      }
    }
  case YAP_UTF8:
    {
      char *w1 = (char *)s1;
      switch (enc2) {
      case YAP_CHAR:
	{
	  int chr1, chr2;
	  char *w2 = s2;
	  for (i = 0; i < l; i++) { chr2 = *w2++; w1 = utf8_get_char(w1, &chr1); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;	
      case YAP_UTF8:
	{
	  int chr1, chr2;
	  char *w2 = s2;
	  for (i = 0; i < l; i++) { w2 = utf8_get_char(w2, &chr2); w1 = utf8_get_char(w1, &chr1); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;	
    case YAP_WCHAR:
	{
	  int chr1, chr2;
	  wchar_t *w2 = s2;
	  for (i = 0; i < l; i++) { chr2 = *w2++; w1 = utf8_get_char(w1, &chr1); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
      }
    }	
  case YAP_WCHAR:
    {
      wchar_t *w1 = (wchar_t *)s1;
      switch (enc2) {
      case YAP_CHAR:
	{
	  int chr1, chr2;
	  char *w2 = s2;
	  for (i = 0; i < l; i++) { chr1 = *w1++; chr2 = *w2++; if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
      case YAP_UTF8:
	{
	  int chr1, chr2;
	  char *w2 = s2;
	  for (i = 0; i < l; i++) { chr1 = *w1++; w2 = utf8_get_char(w2, &chr2); if (chr1-chr2) return chr1-chr2; }
	}
	return 0;
    case YAP_WCHAR:
	return wcsncmp(s1, s2, l);
      }
    }
  }
  return 0;
}

static void *
concat( int n, seq_tv_t *out, void *sv[], encoding_t encv[], size_t lengv[] USES_REGS )
{
  if (out->type == YAP_STRING_STRING) {
    /* we assume we concatenate strings only, or ASCII stuff like numbers */
    Term t = init_tstring( PASS_REGS1  );
    char *buf = buf_from_tstring(HR);
    int i;
    for (i = 0; i < n; i++) {
      if (encv[i] == YAP_WCHAR) {
	wchar_t *ptr = sv[i];
	int chr;
	while ( (chr = *ptr++) ) buf = utf8_put_char(buf, chr);
      } else if (encv[i] == YAP_CHAR) {
	char *ptr = sv[i];
	int chr;
	while ( (chr = *ptr++) ) buf = utf8_put_char(buf, chr);
      } else {
	char *ptr = sv[i];
	int chr;
	while ( (chr = *ptr++) ) *buf++ = chr;
      }
    }
    *buf ++ = '\0';
    close_tstring( buf  PASS_REGS );
    out->val.t = t;
    return HR;
  } else {
    encoding_t enc = YAP_CHAR;
    size_t sz = 0;

    int i;
    for (i = 0; i < n; i++) {
      if (encv[i] != YAP_CHAR) {
	enc = YAP_WCHAR;
      }
      sz += write_length(sv[i], out, encv[i], FALSE, lengv[i] PASS_REGS);
    }
    if (enc == YAP_WCHAR) {
      /* wide atom */
      wchar_t *buf = (wchar_t *)HR;
      Atom at;
      Term t = ARG1;
      LOCAL_ERROR( sz+3 );
      for (i = 0; i < n ; i ++) {
	if (encv[i] == YAP_WCHAR) {
	  wchar_t *ptr = sv[i];
	  int chr;
	  while ( (chr = *ptr++) != '\0' ) *buf++ = chr;
	} else if (encv[i] == YAP_CHAR) {
	  char *ptr = sv[i];
	  int chr;
	  while ( (chr = *ptr++) != '\0' ) *buf++ = (unsigned char)chr;
	} else {
	  char *ptr = sv[i];
	  int chr;
	  while ( (ptr = utf8_get_char( ptr, &chr )) != NULL ) { if (chr == '\0') break; else *buf++ = chr; }
	}
      }
      *buf++ = '\0';
      at = out->val.a = Yap_LookupWideAtom((wchar_t *)HR);
      return at;
    } else {
      /* atom */
      char *buf = (char *)HR;
      Atom at;
      Term t = ARG1;

      LOCAL_TERM_ERROR( sz/sizeof(CELL)+3 );
      for (i = 0; i < n ; i ++) {
	char *ptr = sv[i];
	int chr;
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
    char *nbuf = buf_from_tstring(HR);
    if (enc == YAP_WCHAR) {
      wchar_t *ptr = (wchar_t *)buf + min;
      int chr;
      while ( min++ < max ) { chr = *ptr++; nbuf = utf8_put_char(nbuf, chr); }
    } else if (enc == YAP_CHAR) {
      char *ptr = (char *)buf + min;
      int chr;
      while ( min++ < max ) { chr = *ptr++; nbuf = utf8_put_char(nbuf, chr); }
    } else {
      const char *ptr = utf8_skip ( (const char *)buf, min );
      int chr;
      if (!ptr) return NULL;
      while ( min++ < max ) { ptr = utf8_get_char(ptr, & chr); nbuf = utf8_put_char(nbuf, chr); }
    }
    *nbuf ++ = '\0';
    close_tstring( nbuf  PASS_REGS );
    out->val.t = t;
    return (void *)StringOfTerm(t);
  } else {
    Atom at;
     /* atom */
    if (enc == YAP_WCHAR) {
      /* wide atom */
      wchar_t *nbuf = (wchar_t *)HR;
      Term t = TermNil;
      wchar_t *ptr = (wchar_t *)buf + min;
      if (max>min) {
	LOCAL_ERROR( (max-min)*sizeof(wchar_t) );
	memcpy( nbuf, ptr, (max - min)*sizeof(wchar_t));
      }
      nbuf[max-min] = '\0';
      at = Yap_LookupMaybeWideAtom( nbuf );
    } else if (enc == YAP_CHAR) {
      /*  atom */
      char *nbuf = (char *)HR;
      
      if (max>min) {
	Term t = TermNil;
	char *ptr = (char *)buf + min;
	LOCAL_ERROR( max-min );
	memcpy( nbuf, ptr, (max - min));
      }
      nbuf[max-min] = '\0';
      at = Yap_LookupAtom( nbuf );
    } else {
      /*  atom */
      wchar_t *nbuf = (wchar_t *)HR;
      Term t = ARG1;
      const char *ptr = utf8_skip ( (const char *)buf, min );
      int chr;

      LOCAL_ERROR( max-min );
      while ( min++ < max ) { ptr = utf8_get_char(ptr, & chr); *nbuf++ = chr; }
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
    void *nbuf = read_Text( buf, inp+i, encv+i, &minimal, &leng PASS_REGS );
  
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

  buf = read_Text( NULL, inp, &enc, &minimal, &leng PASS_REGS );
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
	buf0 = read_Text( store, outv, &enc0, &minimal0, &leng0 PASS_REGS );
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
	buf1 = read_Text( store, outv+1, &enc1, &minimal1, &leng1 PASS_REGS );
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

