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
* File:		write.c							 *
* Last rev:								 *
* mods:									 *
* comments:	Writing a Prolog Term					 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#include <stdlib.h>
#include <math.h>
#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "clause.h"
#if COROUTINING
#include "attvar.h"
#endif

#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif

/* describe the type of the previous term to have been written */
typedef enum {
  separator,	/* the previous term was a separator like ',', ')', ... */
  alphanum,	/* the previous term was an atom or number */
  symbol        /* the previous term was a symbol like +, -, *, .... */
} wtype;

static wtype lastw;

typedef  int      (*wrf) (int, wchar_t);

typedef struct union_slots {
  Int old;
  Int ptr;
} uslots;

typedef struct union_direct {
  Term old;
  CELL *ptr;
} udirect;


typedef  struct  rewind_term {
  struct rewind_term *parent;
  union {
    struct union_slots s;
    struct union_direct d;
  } u;
} rwts;

typedef struct write_globs {
  wrf      writewch;
  int      Quote_illegal, Ignore_ops, Handle_vars, Use_portray;
  int      keep_terms;
  int      Write_Loops;
  UInt     MaxDepth, MaxArgs;
} wglbs;

STATIC_PROTO(void wrputn, (Int, wrf));
STATIC_PROTO(void wrputs, (char *, wrf));
STATIC_PROTO(void wrputf, (Float, wrf));
STATIC_PROTO(void wrputref, (CODEADDR, int, wrf));
STATIC_PROTO(int legalAtom, (unsigned char *));
/*STATIC_PROTO(int LeftOpToProtect, (Atom, int));
  STATIC_PROTO(int RightOpToProtect, (Atom, int));*/
STATIC_PROTO(wtype AtomIsSymbols, (unsigned char *));
STATIC_PROTO(void putAtom, (Atom, int, wrf));
STATIC_PROTO(void writeTerm, (Term, int, int, int, struct write_globs *, struct rewind_term *));

#define wrputc(X,WF)	((*WF)(LOCAL_c_output_stream,X))	/* writes a character */

static void 
wrputn(Int n, wrf writewch)	/* writes an integer	 */
	                  
{
  CACHE_REGS
  char s[256], *s1=s; /* that should be enough for most integers */
  if (n < 0) {
    if (lastw == symbol)
      wrputc(' ', writewch);  
  } else {
    if (lastw == alphanum)
      wrputc(' ', writewch);
  }
#if HAVE_SNPRINTF
  snprintf(s, 256, Int_FORMAT, n);
#else
  sprintf(s, Int_FORMAT, n);
#endif
  while (*s1)
    wrputc(*s1++, writewch);
  lastw = alphanum;
}

static void 
wrputs(char *s, wrf writewch)		/* writes a string	 */
{
  CACHE_REGS
  while (*s) {
    wrputc((unsigned char)(*s++), writewch);
  }
}

static void 
wrputws(wchar_t *s, wrf writewch)		/* writes a string	 */
{
  CACHE_REGS
  while (*s)
    wrputc(*s++, writewch);
}

#ifdef USE_GMP

static char *
ensure_space(size_t sz) {
  CACHE_REGS
  char *s;

  s = (char *) Yap_PreAllocCodeSpace();
  while (s+sz >= (char *)AuxSp) {
#if USE_SYSTEM_MALLOC
    /* may require stack expansion */
    if (!Yap_ExpandPreAllocCodeSpace(sz, NULL, TRUE)) {
      s = NULL;
      break;
    }
    s = (char *) Yap_PreAllocCodeSpace();
#else
    s = NULL;
#endif
  }
  if (!s) {
    s = (char *)TR;
    while (s+sz >= LOCAL_TrailTop) {
      if (!Yap_growtrail(sz/sizeof(CELL), FALSE)) {
	s = NULL;
	break;
      }
      s = (char *)TR;
    }
  }
  if (!s) {
    s = (char *)H;
    if (s+sz >= (char *)ASP) {
      Yap_Error(OUT_OF_STACK_ERROR,TermNil,"not enough space to write bignum: it requires %d bytes", sz);
      s = NULL;
    }
  }
  return s;
}

static void
write_mpint(MP_INT *big, wrf writewch) {
  CACHE_REGS
  char *s;

  s = ensure_space(3+mpz_sizeinbase(big, 10));
  if (mpz_sgn(big) < 0) {
    if (lastw == symbol)
      wrputc(' ', writewch);  
  } else {
    if (lastw == alphanum)
      wrputc(' ', writewch);
  }
  if (!s) {
    s = mpz_get_str(NULL, 10, big);
    if (!s)
      return;
    wrputs(s,writewch);
    free(s);
  } else {
    mpz_get_str(s, 10, big);
    wrputs(s,writewch);
  }
}
#endif

	/* writes a bignum	 */
static void 
writebig(Term t, int p, int depth, int rinfixarg, struct write_globs *wglb, struct rewind_term *rwt)
{
  CACHE_REGS
  CELL *pt = RepAppl(t)+1;
#ifdef USE_GMP
  if (pt[0] == BIG_INT) 
  {
    MP_INT *big = Yap_BigIntOfTerm(t);
    write_mpint(big, wglb->writewch);
    return;
  } else if (pt[0] == BIG_RATIONAL) {
    Term trat = Yap_RatTermToApplTerm(t);
    writeTerm(trat, p, depth, rinfixarg, wglb, rwt);
    return;
  }
#endif
  if (pt[0] == BLOB_STRING) {
    wrputc('"',wglb->writewch);
    wrputs(Yap_BlobStringOfTerm(t),wglb->writewch);
    wrputc('"',wglb->writewch);
    return;
  } else if  (pt[0] == BLOB_STRING) {
    wchar_t *s = Yap_BlobWideStringOfTerm(t);
    wrputc('"', wglb->writewch);
    while (*s) {
      wrputc(*s++, wglb->writewch);
    }
    wrputc('"',wglb->writewch);
    return;
  }
  wrputs("0",wglb->writewch);
}	                  

static void 
wrputf(Float f, wrf writewch)		/* writes a float	 */
	                  
{
  CACHE_REGS
  char            s[256], *pt = s, ch;
  int found_dot = FALSE, found_exp = FALSE;

#if HAVE_ISNAN || defined(__WIN32)
  if (isnan(f)) {
    wrputs("(nan)", writewch);
    lastw = separator;
    return;
  }
#endif
  if (f < 0) {
#if HAVE_ISINF || defined(_WIN32)
    if (isinf(f)) {
      wrputs("(-inf)", writewch);
      lastw = separator;
      return;
    }
#endif
    if (lastw == symbol)
      wrputc(' ', writewch);  
  } else {
#if HAVE_ISINF || defined(_WIN32)
    if (isinf(f)) {
      wrputs("(+inf)", writewch);
      lastw = separator;
      return;
    }
#endif
    if (lastw == alphanum)
      wrputc(' ', writewch);
  }
  lastw = alphanum;
  //  sprintf(s, "%.15g", f);
  sprintf(s, RepAtom(AtomFloatFormat)->StrOfAE, f);
  while (*pt == ' ')
    pt++;
  if (*pt == '-') {
    wrputc('-', writewch);
    pt++;
  }
  while ((ch = *pt) != '\0') {
    switch (ch) {
    case '.':
      found_dot = TRUE;
      wrputc('.', writewch);    
      break;
    case 'e':
    case 'E':
      if (!found_dot) {
	found_dot = TRUE;
	wrputs(".0", writewch);
      }
      found_exp = TRUE;
    default:
      wrputc(ch, writewch);
    }
    pt++;
  }
  if (!found_dot) {
    wrputs(".0", writewch);    
  }
}

static void 
wrputref(CODEADDR ref, int Quote_illegal, wrf writewch)			/* writes a data base reference */
	                    
{
  char            s[256];

  putAtom(AtomDBref, Quote_illegal, writewch);
#if defined(__linux__) || defined(__APPLE__)
  sprintf(s, "(%p," UInt_FORMAT ")", ref, ((LogUpdClause*)ref)->ClRefCount);
#else
  sprintf(s, "(0x%p," UInt_FORMAT ")", ref, ((LogUpdClause*)ref)->ClRefCount);
#endif
  wrputs(s, writewch);
  lastw = alphanum;
}

static int 
legalAtom(unsigned char *s)			/* Is this a legal atom ? */
{
  wchar_t ch = *s;

  if (ch == '\0')
    return FALSE;
  if (Yap_chtype[ch] != LC) {
    if (ch == '[') {
      return (s[1] == ']' && !s[2]);
    } else if (ch == '{') {
	return (s[1] == '}' && !s[2]);
    } else if (Yap_chtype[ch] == SL) {
      return (!s[1]);
    } else if ((ch == ',' || ch == '.') && !s[1]) {
      return FALSE;
    } else {
      if (ch == '/') {
	if (s[1] == '*') return FALSE;
      }
      while (ch) {
	if (Yap_chtype[ch] != SY) {
	  return FALSE;
	}
	ch = *++s;
      }
    }
    return TRUE;
  } else
    while ((ch = *++s) != 0)
      if (Yap_chtype[ch] > NU)
	return FALSE;
  return (TRUE);
}

static wtype 
AtomIsSymbols(unsigned char *s)		/* Is this atom just formed by symbols ? */
{
  int ch;
  if (Yap_chtype[(int)s[0]] == SL && s[1] == '\0')
    return(separator);
  while ((ch = *s++) != '\0') {
    if (Yap_chtype[ch] != SY)
      return(alphanum);
  }
  return(symbol);
}

static void
write_quoted(int ch, int quote, wrf writewch)
{
  CACHE_REGS
  if (yap_flags[CHARACTER_ESCAPE_FLAG] == CPROLOG_CHARACTER_ESCAPES) {
    wrputc(ch, writewch);
    if (ch == '\'')
      wrputc('\'', writewch);	/* be careful about quotes */
    return;
  }
  if (!(ch < 0xff  && chtype(ch) == BS) && ch != '\'' && ch != '\\') {
    wrputc(ch, writewch);
  } else {
    switch (ch) {
    case '\\':
    case '\'':
      wrputc('\\', writewch);	
      wrputc(ch, writewch);	
      break;
    case 7:
      wrputc('\\', writewch);	
      wrputc('a', writewch);	
      break;
    case '\b':
      wrputc('\\', writewch);
      wrputc('b', writewch);	
      break;
    case '\t':
      wrputc('\\', writewch);
      wrputc('t', writewch);	
      break;
    case ' ':
    case 160:
      wrputc(' ', writewch);
      break;
    case '\n':
      wrputc('\\', writewch);
      wrputc('n', writewch);	
      break;
    case 11:
      wrputc('\\', writewch);
      wrputc('v', writewch);	
      break;
    case '\r':
      wrputc('\\', writewch);
      wrputc('r', writewch);	
      break;
    case '\f':
      wrputc('\\', writewch);
      wrputc('f', writewch);	
      break;
    default:
      if ( ch <= 0xff ) {
	char esc[8];
	
	if (yap_flags[CHARACTER_ESCAPE_FLAG] == SICSTUS_CHARACTER_ESCAPES) {
	  sprintf(esc, "\\%03o", ch);
	} else {
	  /* last backslash in ISO mode */
	  sprintf(esc, "\\%03o\\", ch);
	}
	wrputs(esc, writewch);
      }
    }
  }
}


static void 
putAtom(Atom atom, int Quote_illegal, wrf writewch)			/* writes an atom	 */
	                     
{
  CACHE_REGS
  unsigned char           *s = (unsigned char *)RepAtom(atom)->StrOfAE;
  wtype          atom_or_symbol = AtomIsSymbols(s);

  /* #define CRYPT_FOR_STEVE 1*/
#ifdef CRYPT_FOR_STEVE
  if (Yap_GetValue(AtomCryptAtoms) != TermNil && Yap_GetAProp(atom, OpProperty) == NIL) {
    char s[16];
    sprintf(s,"x%x", (CELL)s);
    wrputs(s, writewch);
    return;
  }
#endif
  if (IsBlob(atom)) {
    wrputref((CODEADDR)RepAtom(atom),1,writewch);
    return;
  }
  if (IsWideAtom(atom)) {
    wchar_t *ws = (wchar_t *)s;

    if (Quote_illegal) {
      wrputc('\'', writewch);
      while (*ws) {
	wchar_t ch = *ws++;
	write_quoted(ch, '\'', writewch);
      }
      wrputc('\'', writewch);
    } else {
      wrputws(ws, writewch);
    }
    return;
  }
  if (lastw == atom_or_symbol && atom_or_symbol != separator /* solo */)
    wrputc(' ', writewch);
  lastw = atom_or_symbol;
  if (Quote_illegal && !legalAtom(s)) {
    wrputc('\'', writewch);
    while (*s) {
      wchar_t ch = *s++;
      write_quoted(ch, '\'', writewch);
    }
    wrputc('\'', writewch);
  } else {
    wrputs((char *)s, writewch);
  }
}

static int 
IsStringTerm(Term string)	/* checks whether this is a string */
{
  if (IsVarTerm(string))
    return FALSE;
  do {
    Term hd;
    int ch;

    if (!IsPairTerm(string)) return(FALSE);
    hd = HeadOfTerm(string);
    if (IsVarTerm(hd)) return(FALSE);
    if (!IsIntTerm(hd)) return(FALSE);
    ch = IntOfTerm(HeadOfTerm(string));
    if ((ch < ' ' || ch > MAX_ISO_LATIN1) && ch != '\n' && ch != '\t')
      return(FALSE);
    string = TailOfTerm(string);
    if (IsVarTerm(string)) return(FALSE);
  } while (string != TermNil);
  return(TRUE);
}

static void 
putString(Term string, wrf writewch)		/* writes a string	 */
	                     
{
  CACHE_REGS
  wrputc('"', writewch);
  while (string != TermNil) {
    int ch = IntOfTerm(HeadOfTerm(string));
    write_quoted(ch, '"', writewch);
    string = TailOfTerm(string);
  }
  wrputc('"', writewch);
  lastw = alphanum;
}

static void 
putUnquotedString(Term string, wrf writewch)	/* writes a string	 */
	                     
{
  CACHE_REGS
  while (string != TermNil) {
    int ch = IntOfTerm(HeadOfTerm(string));
    wrputc(ch, writewch);
    string = TailOfTerm(string);
  }
  lastw = alphanum;
}


static void
write_var(CELL *t,  struct write_globs *wglb, struct rewind_term *rwt) 
{
  CACHE_REGS
  if (lastw == alphanum) {
    wrputc(' ', wglb->writewch);
  }
  wrputc('_', wglb->writewch);
  /* make sure we don't get no creepy spaces where they shouldn't be */
  lastw = separator;
  if (IsAttVar(t)) {
    Int vcount = (t-H0);
#if COROUTINING
#if DEBUG
    if (Yap_Portray_delays) {
      exts ext = ExtFromCell(t);

      Yap_Portray_delays = FALSE;
      if (ext == attvars_ext) {
	attvar_record *attv = RepAttVar(t);
	Int sl = 0;
	Term l = attv->Atts;

	wrputs("$AT(",wglb->writewch);
	write_var(t, wglb, rwt);
	wrputc(',', wglb->writewch);      
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot((CELL)attv PASS_REGS);
	}
	writeTerm((Term)&(attv->Value), 999, 1, FALSE, wglb, rwt);
	wrputc(',', wglb->writewch);
	writeTerm(l, 999, 1, FALSE, wglb, rwt);
	if (wglb->keep_terms) {
	  attv = (attvar_record *)Yap_GetFromSlot(sl PASS_REGS);
	  Yap_RecoverSlots(1 PASS_REGS);
	}
	wrputc(')', wglb->writewch);
      }
      Yap_Portray_delays = TRUE;
      return;
    }
#endif
    wrputc('D', wglb->writewch);
    wrputn(vcount,wglb->writewch);
#endif
  } else {
    wrputn(((Int) (t- H0)),wglb->writewch);
  }
}

static Term
from_pointer(CELL *ptr, struct rewind_term *rwt, struct write_globs *wglb)
{
  CACHE_REGS
  Term t;

  while (IsVarTerm(*ptr) && !IsUnboundVar(ptr))
    ptr = (CELL *)*ptr;
  t = *ptr;
  if (!IsVarTerm(t) && !IsAtomOrIntTerm(t)) {
    struct rewind_term *x = rwt->parent;
    if (wglb->keep_terms) {
      rwt->u.s.old = Yap_InitSlot(t PASS_REGS);
      rwt->u.s.ptr = Yap_InitSlot((CELL)ptr PASS_REGS);
      while (x) {
	if (Yap_GetFromSlot(x->u.s.old PASS_REGS) == t)
	  return TermFoundVar;
	x = x->parent;
      }
    } else {
      rwt->u.d.old = t;
      rwt->u.d.ptr = ptr;
      while (x) {
	if (x->u.d.old == t)
	  return TermFoundVar;
	x = x->parent;
      }
    }
  } else {
    rwt->u.s.ptr = 0;
  }
  return t;
}

static Term
check_infinite_loop(Term t, struct rewind_term *x, struct write_globs *wglb)
{
  CACHE_REGS
  if (wglb->keep_terms) {
    while (x) {
      if (Yap_GetFromSlot(x->u.s.old PASS_REGS) == t)
	return TermFoundVar;
      x = x->parent;
    }
  } else {
    while (x) {
      if (x->u.d.old == t)
	return TermFoundVar;
	x = x->parent;
    }
  }
  return t;
}

static void
restore_from_write(struct rewind_term *rwt, struct write_globs *wglb)
{
  CACHE_REGS
  Term t;
  if (rwt->u.s.ptr) {
    CELL *ptr;
    if (wglb->keep_terms) {
      ptr = (CELL *)Yap_GetPtrFromSlot(rwt->u.s.ptr PASS_REGS);
      t = Yap_GetPtrFromSlot(rwt->u.s.old PASS_REGS);
      Yap_RecoverSlots(2 PASS_REGS);
    } else {
      ptr = rwt->u.d.ptr;
      t = rwt->u.d.old;
    }
  }
  rwt->u.s.ptr = 0;
}

static void
write_list(Term t, int direction, int depth, struct write_globs *wglb, struct rewind_term *rwt)
{
  CACHE_REGS
  Term ti;
  struct rewind_term nrwt;
  nrwt.parent = rwt;
  nrwt.u.s.ptr = 0;

  while (1) {
    Int            sl= 0;
    int ndirection;
    int do_jump;

    if (wglb->keep_terms) {
      /* garbage collection may be called */
      sl = Yap_InitSlot(t PASS_REGS);
    }
    writeTerm(from_pointer(RepPair(t), &nrwt, wglb), 999, depth+1, FALSE, wglb, &nrwt);
    restore_from_write(&nrwt, wglb);
    if (wglb->keep_terms) {
      t = Yap_GetFromSlot(sl PASS_REGS);
      Yap_RecoverSlots(1 PASS_REGS);
    }
    ti = TailOfTerm(t);
    if (IsVarTerm(ti))
      break;
    if (!IsPairTerm(ti) ||
	!IsPairTerm((ti = check_infinite_loop(ti, rwt, wglb))))
      break;
    ndirection = RepPair(ti)-RepPair(t);
    /* make sure we're not trapped in loops */
    if (ndirection > 0) {
      do_jump = (direction <= 0);
    } else if (ndirection == 0) {
      wrputc(',', wglb->writewch);
      putAtom(AtomFoundVar, wglb->Quote_illegal, wglb->writewch);
      lastw = separator;
      return;
    } else {
      do_jump = (direction >= 0);
    }
    if (wglb->MaxDepth != 0 && depth > wglb->MaxDepth) {
      wrputc('|', wglb->writewch);
      putAtom(Atom3Dots, wglb->Quote_illegal, wglb->writewch);
      return;
    }
    lastw = separator;
    direction = ndirection;
    depth++;
    if (do_jump)
      break;
    wrputc(',', wglb->writewch);
    t = ti;
  }
  if (IsPairTerm(ti)) {
    Term nt = from_pointer(RepPair(t)+1, &nrwt, wglb);
    /* we found an infinite loop */
    if (IsAtomTerm(nt)) {
      wrputc('|', wglb->writewch);      
      writeTerm(nt, 999, depth, FALSE, wglb, rwt);
    } else {
      /* keep going on the list */
      wrputc(',', wglb->writewch);      
      write_list(nt, direction, depth, wglb, &nrwt);
    }
    restore_from_write(&nrwt, wglb);
  } else if (ti != MkAtomTerm(AtomNil)) {
    wrputc('|', wglb->writewch);
    lastw = separator;
    writeTerm(from_pointer(RepPair(t)+1, &nrwt, wglb), 999, depth, FALSE, wglb, &nrwt);
    restore_from_write(&nrwt, wglb);
  }
 }

static void 
writeTerm(Term t, int p, int depth, int rinfixarg, struct write_globs *wglb, struct rewind_term *rwt)
/* term to write			 */
/* context priority			 */
                     
{
  CACHE_REGS
  struct rewind_term nrwt;
  nrwt.parent = rwt;
  nrwt.u.s.ptr = 0;

  if (wglb->MaxDepth != 0 && depth > wglb->MaxDepth) {
    putAtom(Atom3Dots, wglb->Quote_illegal, wglb->writewch);
    return;
  }
  if (EX)
    return;
  t = Deref(t);
  if (IsVarTerm(t)) {
    write_var((CELL *)t, wglb, &nrwt);
  } else if (IsIntTerm(t)) {
    wrputn((Int) IntOfTerm(t),wglb->writewch);
  } else if (IsAtomTerm(t)) {
    putAtom(AtomOfTerm(t), wglb->Quote_illegal, wglb->writewch);
  } else if (IsPairTerm(t)) {
    if (wglb->Ignore_ops) {
      Int sl = 0;

      wrputs("'.'(",wglb->writewch);
      lastw = separator;
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	sl = Yap_InitSlot(t PASS_REGS);      
      }
      writeTerm(HeadOfTerm(t), 999, depth + 1, FALSE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	t = Yap_GetFromSlot(sl PASS_REGS);
	Yap_RecoverSlots(1 PASS_REGS);
      }
      wrputs(",",wglb->writewch);	
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	sl = Yap_InitSlot(t PASS_REGS);      
      }
      writeTerm(TailOfTerm(t), 999, depth + 1, FALSE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	t = Yap_GetFromSlot(sl PASS_REGS);
	Yap_RecoverSlots(1 PASS_REGS);
      }
      wrputc(')', wglb->writewch);
      lastw = separator;
      return;
    } 
    if (wglb->Use_portray) {
      Term targs[1];
      struct DB_TERM *old_EX = NULL;
      Int sl = 0;

      targs[0] = t;
      Yap_PutValue(AtomPortray, MkAtomTerm(AtomNil));
      if (EX) old_EX = EX;
      sl = Yap_InitSlot(t PASS_REGS);      
      Yap_execute_goal(Yap_MkApplTerm(FunctorPortray, 1, targs), 0, 1);
      t = Yap_GetFromSlot(sl PASS_REGS);
      Yap_RecoverSlots(1 PASS_REGS);
      if (old_EX != NULL) EX = old_EX;
      if (Yap_GetValue(AtomPortray) == MkAtomTerm(AtomTrue))
	return;
    }
    if (yap_flags[WRITE_QUOTED_STRING_FLAG] && IsStringTerm(t)) {
      putString(t, wglb->writewch);
    } else {
      wrputc('[', wglb->writewch);
      lastw = separator;
      /* we assume t was already saved in the stack */ 
      write_list(t, 0, depth, wglb, rwt);
      wrputc(']', wglb->writewch);
      lastw = separator;
    }
  } else {		/* compound term */
    Functor         functor = FunctorOfTerm(t);
    int             Arity;
    Atom            atom;
    int             op, lp, rp;

    if (IsExtensionFunctor(functor)) {
      switch((CELL)functor) {
      case (CELL)FunctorDouble:
	wrputf(FloatOfTerm(t),wglb->writewch);
	return;
      case (CELL)FunctorAttVar:	
	write_var(RepAppl(t)+1, wglb, &nrwt);
	return;
      case (CELL)FunctorDBRef:
	wrputref(RefOfTerm(t), wglb->Quote_illegal, wglb->writewch);
	return;
      case (CELL)FunctorLongInt:
	wrputn(LongIntOfTerm(t),wglb->writewch);
	return;
	/* case (CELL)FunctorBigInt: */
      default:
	writebig(t, p, depth, rinfixarg, wglb, rwt);
	return;
      }
    }
    Arity = ArityOfFunctor(functor);
    atom = NameOfFunctor(functor);
#ifdef SFUNC
    if (Arity == SFArity) {
      int             argno = 1;
      CELL           *p = ArgsOfSFTerm(t);
      putAtom(atom, wglb->Quote_illegal, wglb->writewch);
      wrputc('(', wglb->writewch);
      lastw = separator;
      while (*p) {
	Int sl = 0;

	while (argno < *p) {
	  wrputc('_', wglb->writewch), wrputc(',', wglb->writewch);
	  ++argno;
	}
	*p++;
	lastw = separator;
	/* cannot use the term directly with the SBA */
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot((CELL)p);
	}
	writeTerm(from_pointer(p++, &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
	restore_from_write(&nrwt, wglb);
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  p = (CELL *)Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	if (*p)
	  wrputc(',', wglb->writewch);
	argno++;
      }
      wrputc(')', wglb->writewch);
      lastw = separator;
      return;
    }
#endif
    if (wglb->Use_portray) {
      Term targs[1];
      struct DB_TERM *old_EX = NULL;
      Int sl = 0;

      targs[0] = t;
      Yap_PutValue(AtomPortray, MkAtomTerm(AtomNil));
      if (EX) old_EX = EX;
      sl = Yap_InitSlot(t PASS_REGS);      
      Yap_execute_goal(Yap_MkApplTerm(FunctorPortray, 1, targs),0, 1);
      t = Yap_GetFromSlot(sl PASS_REGS);
      Yap_RecoverSlots(1 PASS_REGS);
      if (old_EX) EX = old_EX;
      if (Yap_GetValue(AtomPortray) == MkAtomTerm(AtomTrue) || EX)
	return;
    }
    if (!wglb->Ignore_ops &&
	Arity == 1 &&  Yap_IsPrefixOp(atom, &op, &rp)
#ifdef DO_NOT_WRITE_PLUS_AND_MINUS_AS_PREFIX
	&&
	/* never write '+' and '-' as infix
	   operators */
	( (RepAtom(atom)->StrOfAE[0] != '+' &&
	   RepAtom(atom)->StrOfAE[0] != '-') ||
	  RepAtom(atom)->StrOfAE[1] )
#endif /* DO_NOT_WRITE_PLUS_AND_MINUS_AS_PREFIX */
	) {
      Term  tright = ArgOfTerm(1, t);
      int            bracket_right =
	!IsVarTerm(tright) && IsAtomTerm(tright) &&
	Yap_IsOp(AtomOfTerm(tright));
      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ', wglb->writewch);
	wrputc('(', wglb->writewch);
	lastw = separator;
      }
      putAtom(atom, wglb->Quote_illegal, wglb->writewch);
      if (bracket_right) {
	wrputc('(', wglb->writewch);
	lastw = separator;
      }
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), rp, depth + 1, FALSE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (bracket_right) {
	wrputc(')', wglb->writewch);
	lastw = separator;
      }
      if (op > p) {
	wrputc(')', wglb->writewch);
	lastw = separator;
      }
    } else if (!wglb->Ignore_ops &&
	       Arity == 1 &&
	       Yap_IsPosfixOp(atom, &op, &lp)) {
      Term  tleft = ArgOfTerm(1, t);
      Int sl = 0;
      int            bracket_left =
	!IsVarTerm(tleft) && IsAtomTerm(tleft) &&
	Yap_IsOp(AtomOfTerm(tleft)); 
      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ', wglb->writewch);
	wrputc('(', wglb->writewch);
	lastw = separator;
      }
      if (bracket_left) {
	wrputc('(', wglb->writewch);
	lastw = separator;
      }
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	sl = Yap_InitSlot(t PASS_REGS);      
      }
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), lp, depth + 1, rinfixarg, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	t = Yap_GetFromSlot(sl PASS_REGS);
	Yap_RecoverSlots(1 PASS_REGS);
      }
      if (bracket_left) {
	wrputc(')', wglb->writewch);
	lastw = separator;
      }
      putAtom(atom, wglb->Quote_illegal, wglb->writewch);
      if (op > p) {
	wrputc(')', wglb->writewch);
	lastw = separator;
      }
    } else if (!wglb->Ignore_ops &&
	       Arity == 2  && Yap_IsInfixOp(atom, &op, &lp,
						 &rp) ) {
      Term  tleft = ArgOfTerm(1, t);
      Term  tright = ArgOfTerm(2, t);
      Int sl = 0;
      int   bracket_left =
	!IsVarTerm(tleft) && IsAtomTerm(tleft) &&
	Yap_IsOp(AtomOfTerm(tleft));
      int   bracket_right =
	!IsVarTerm(tright) && IsAtomTerm(tright) &&
	Yap_IsOp(AtomOfTerm(tright));

      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ', wglb->writewch);
	wrputc('(', wglb->writewch);
	lastw = separator;
      }
      if (bracket_left) {
	wrputc('(', wglb->writewch);
	lastw = separator;
      }
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	sl = Yap_InitSlot(t PASS_REGS);      
      }
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), lp, depth + 1, rinfixarg, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	t = Yap_GetFromSlot(sl PASS_REGS);
	Yap_RecoverSlots(1 PASS_REGS);
      }
      if (bracket_left) {
	wrputc(')', wglb->writewch);
	lastw = separator;
      }
      /* avoid quoting commas */
      if (strcmp(RepAtom(atom)->StrOfAE,","))
	putAtom(atom, wglb->Quote_illegal, wglb->writewch);
      else {
	wrputc(',', wglb->writewch);
	lastw = separator;
      }
      if (bracket_right) {
	wrputc('(', wglb->writewch);
	lastw = separator;
      }
      writeTerm(from_pointer(RepAppl(t)+2, &nrwt, wglb), rp, depth + 1, TRUE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (bracket_right) {
	wrputc(')', wglb->writewch);
	lastw = separator;
      }
      if (op > p) {
	wrputc(')', wglb->writewch);
	lastw = separator;
      }
    } else if (wglb->Handle_vars && functor == FunctorVar) {
      Term ti = ArgOfTerm(1, t);
      if (lastw == alphanum) {
	wrputc(' ', wglb->writewch);
      }
      if (!IsVarTerm(ti) && (IsIntTerm(ti) || IsStringTerm(ti))) {
	if (IsIntTerm(ti)) {
	  Int k = IntOfTerm(ti);
	  if (k == -1)  {
	    wrputc('_', wglb->writewch);
	    lastw = alphanum;
	    return;
	  } else {
	    wrputc((k % 26) + 'A', wglb->writewch);
	    if (k >= 26) {
	      /* make sure we don't get confused about our context */
	      lastw = separator;
	      wrputn( k / 26 ,wglb->writewch);
	    } else
	      lastw = alphanum;
	  }
	} else {
	  putUnquotedString(ti, wglb->writewch);
	}
      } else {
	Int sl = 0;

	wrputs("'$VAR'(",wglb->writewch);
	lastw = separator;
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t PASS_REGS);      
	}
	writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
	restore_from_write(&nrwt, wglb);
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  t = Yap_GetFromSlot(sl PASS_REGS);
	  Yap_RecoverSlots(1 PASS_REGS);
	}
	wrputc(')', wglb->writewch);
	lastw = separator;
      }
    } else if (!wglb->Ignore_ops && functor == FunctorBraces) {
      wrputc('{', wglb->writewch);
      lastw = separator;
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), 1200, depth + 1, FALSE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      wrputc('}', wglb->writewch);
      lastw = separator;
    } else  if (atom == AtomArray) {
      Int sl = 0;

      wrputc('{', wglb->writewch);
      lastw = separator;
      for (op = 1; op <= Arity; ++op) {
	if (op == wglb->MaxArgs) {
	  wrputc('.', wglb->writewch);
	  wrputc('.', wglb->writewch);
	  wrputc('.', wglb->writewch);
	  break;
	}
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t PASS_REGS);      
	}
	writeTerm(from_pointer(RepAppl(t)+op, &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
	restore_from_write(&nrwt, wglb);
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  t = Yap_GetFromSlot(sl PASS_REGS);
	  Yap_RecoverSlots(1 PASS_REGS);
	}
	if (op != Arity) {
	  wrputc(',', wglb->writewch);
	  lastw = separator;
	}
      }
      wrputc('}', wglb->writewch);
      lastw = separator;
    } else {
      putAtom(atom, wglb->Quote_illegal, wglb->writewch);
      lastw = separator;
      wrputc('(', wglb->writewch);
      for (op = 1; op <= Arity; ++op) {
	Int sl = 0;

	if (op == wglb->MaxArgs) {
	  wrputc('.', wglb->writewch);
	  wrputc('.', wglb->writewch);
	  wrputc('.', wglb->writewch);
	  break;
	}
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t PASS_REGS);      
	}
	writeTerm(from_pointer(RepAppl(t)+op, &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
	restore_from_write(&nrwt, wglb);
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  t = Yap_GetFromSlot(sl PASS_REGS);
	  Yap_RecoverSlots(1 PASS_REGS);
	}
	if (op != Arity) {
	  wrputc(',', wglb->writewch);
	  lastw = separator;
	}
      }
      wrputc(')', wglb->writewch);
      lastw = separator;
    }
  }
}

void 
Yap_plwrite(Term t, int (*mywrite) (int, wchar_t), int flags, int priority)
     /* term to be written			 */
     /* consumer				 */
     /* write options			 */
{
  struct write_globs wglb;
  struct rewind_term rwt;

  wglb.writewch = mywrite;
  lastw = separator;
  wglb.Quote_illegal = flags & Quote_illegal_f;
  wglb.Handle_vars = flags & Handle_vars_f;
  wglb.Use_portray = flags & Use_portray_f;
  wglb.MaxDepth = 15L;
  wglb.MaxArgs = 15L;
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  wglb.keep_terms = (flags & (Use_portray_f|To_heap_f)); 
  /* initialise wglb */
  rwt.parent = NULL;
  wglb.Ignore_ops = flags & Ignore_ops_f;
  /* protect slots for portray */
  writeTerm(from_pointer(&t, &rwt, &wglb), priority, 1, FALSE, &wglb, &rwt);
  restore_from_write(&rwt, &wglb);
}

