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

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#if DEBUG
#if COROUTINING
#include "attvar.h"
#endif
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

typedef  int      (*wrf) (int, int);

typedef struct write_globs {
  wrf      writech;
  int      Quote_illegal, Ignore_ops, Handle_vars, Use_portray;
  int      keep_terms;
  UInt     MaxDepth, MaxList, MaxArgs;
} wglbs;

STATIC_PROTO(void wrputn, (Int, wrf));
STATIC_PROTO(void wrputs, (char *, wrf));
STATIC_PROTO(void wrputf, (Float, wrf));
STATIC_PROTO(void wrputref, (CODEADDR, int, wrf));
STATIC_PROTO(int legalAtom, (char *));
STATIC_PROTO(int LeftOpToProtect, (Atom, int));
STATIC_PROTO(int RightOpToProtect, (Atom, int));
STATIC_PROTO(wtype AtomIsSymbols, (char *));
STATIC_PROTO(void putAtom, (Atom, int, wrf));
STATIC_PROTO(void writeTerm, (Term, int, int, int, struct write_globs *));

#define wrputc(X,WF)	((*WF)(Yap_c_output_stream,X))	/* writes a character */

static void 
wrputn(Int n, wrf writech)	/* writes an integer	 */
	                  
{
  char s[256], *s1=s; /* that should be enough for most integers */
  if (n < 0) {
    if (lastw == symbol)
      wrputc(' ', writech);  
  } else {
    if (lastw == alphanum)
      wrputc(' ', writech);
  }
#if HAVE_SNPRINTF
#if SHORT_INTS
  snprintf(s, 256, "%ld", n);
#else
  snprintf(s, 256, "%d", n);
#endif
#else
#if SHORT_INTS
  sprintf(s, "%ld", n);
#else
  sprintf(s, "%d", n);
#endif
#endif
  while (*s1)
    wrputc(*s1++, writech);
  lastw = alphanum;
}

static void 
wrputs(char *s, wrf writech)		/* writes a string	 */
{
  while (*s)
    wrputc(*s++, writech);
}

static void 
wrputf(Float f, wrf writech)		/* writes a float	 */
	                  
{
  char            s[255], *pt = s, ch;

  if (f < 0) {
    if (lastw == symbol)
      wrputc(' ', writech);  
  } else {
    if (lastw == alphanum)
      wrputc(' ', writech);
  }
  lastw = alphanum;
  //  sprintf(s, "%.15g", f);
  sprintf(s, RepAtom(FloatFormat)->StrOfAE, f);
  while (*pt == ' ')
    pt++;
  if (*pt == 'i' || *pt == 'n')  /* inf or nan */ {
    wrputc('(', writech);    
    wrputc('+', writech);    
    wrputs(pt, writech);
    wrputc(')', writech);    
  } else {    
    wrputs(pt, writech);
  }
  if (*pt == '-') pt++;
  while ((ch = *pt) != '\0') {
    if (ch < '0' || ch > '9')
      return;
    pt++;
  }
  wrputs(".0", writech);    
}

static void 
wrputref(CODEADDR ref, int Quote_illegal, wrf writech)			/* writes a data base reference */
	                    
{
  char            s[256];

  putAtom(AtomDBRef, Quote_illegal, writech);
#if SHORT_INTS
  sprintf(s, "(0x%p,0)", ref);
#elif __linux__
  sprintf(s, "(%p,0)", ref);
#else
  sprintf(s, "(0x%p,0)", ref);
#endif
  wrputs(s, writech);
  lastw = alphanum;
}

static int 
legalAtom(char *s)			/* Is this a legal atom ? */
	                  
{
  register int ch = *s;
  if (ch == '\0')
    return(FALSE);
  if (Yap_chtype[ch] != LC) {
    if (ch == '[')
      return (*++s == ']' && !(*++s));
    else if (ch == '{')
      return (*++s == '}' && !(*++s));
    else if (Yap_chtype[ch] == SL)
      return (!*++s);
    else if ((ch == ',' || ch == '.') && !s[1])
      return (FALSE);
    else
      while (ch) {
	if (Yap_chtype[ch] != SY) return (FALSE);
	ch = *++s;
      }
    return (TRUE);
  } else
    while ((ch = *++s) != 0)
      if (Yap_chtype[ch] > NU)
	return (FALSE);
  return (TRUE);
}

static int LeftOpToProtect(Atom at, int p)
{
  int op, rp;
  OpEntry            *opinfo = Yap_GetOpProp(at);
  return(opinfo && Yap_IsPrefixOp(opinfo, &op, &rp) );
}

static int RightOpToProtect(Atom at, int p)
{
  int op, lp;
  OpEntry            *opinfo = Yap_GetOpProp(at);
  return(opinfo && Yap_IsPosfixOp(opinfo, &op, &lp) );
}

static wtype 
AtomIsSymbols(char *s)		/* Is this atom just formed by symbols ? */
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
putAtom(Atom atom, int Quote_illegal, wrf writech)			/* writes an atom	 */
	                     
{
  char           *s = RepAtom(atom)->StrOfAE;
  wtype          atom_or_symbol = AtomIsSymbols(s);

  /* #define CRYPT_FOR_STEVE 1*/
#ifdef CRYPT_FOR_STEVE
  if (Yap_GetValue(Yap_LookupAtom("crypt_atoms")) != TermNil && Yap_GetAProp(atom, OpProperty) == NIL) {
    char s[16];
    sprintf(s,"x%x", (CELL)s);
    wrputs(s, writech);
    return;
  }
#endif
  if (lastw == atom_or_symbol && atom_or_symbol != separator /* solo */)
    wrputc(' ', writech);
  lastw = atom_or_symbol;
  if (!legalAtom(s) && Quote_illegal) {
    wrputc('\'', writech);
    while (*s) {
      int ch = *s++;
      wrputc(ch, writech);
      if (ch == '\\' && yap_flags[CHARACTER_ESCAPE_FLAG] != CPROLOG_CHARACTER_ESCAPES)
	wrputc('\\', writech);	/* be careful about backslashes */
      else if (ch == '\'')
	wrputc('\'', writech);	/* be careful about quotes */
    }
    wrputc('\'', writech);
  } else {
    wrputs(s, writech);
  }
}

static int 
IsStringTerm(Term string)	/* checks whether this is a string */
{
  if (IsVarTerm(string)) return(FALSE);
  do {
    Term hd;
    int ch;

    if (!IsPairTerm(string)) return(FALSE);
    hd = HeadOfTerm(string);
    if (IsVarTerm(hd)) return(FALSE);
    if (!IsIntTerm(hd)) return(FALSE);
    ch = IntOfTerm(HeadOfTerm(string));
    if ((ch < ' ' || ch > 255) && ch != '\n' && ch != '\t')
      return(FALSE);
    string = TailOfTerm(string);
    if (IsVarTerm(string)) return(FALSE);
  } while (string != TermNil);
  return(TRUE);
}

static void 
putString(Term string, wrf writech)		/* writes a string	 */
	                     
{
  wrputc('"', writech);
  while (string != TermNil) {
    int ch = IntOfTerm(HeadOfTerm(string));
    wrputc(ch, writech);
    if (ch == '\\' && yap_flags[CHARACTER_ESCAPE_FLAG] != CPROLOG_CHARACTER_ESCAPES)
      wrputc('\\', writech);	/* be careful about backslashes */
    else if (ch == '"')
      wrputc('"', writech);	/* be careful about quotes */
    string = TailOfTerm(string);
  }
  wrputc('"', writech);
  lastw = alphanum;
}

static void 
putUnquotedString(Term string, wrf writech)	/* writes a string	 */
	                     
{
  while (string != TermNil) {
    int ch = IntOfTerm(HeadOfTerm(string));
    wrputc(ch, writech);
    string = TailOfTerm(string);
  }
  lastw = alphanum;
}


static void
write_var(CELL *t,  struct write_globs *wglb) 
{
  if (lastw == alphanum) {
    wrputc(' ', wglb->writech);
  }
  wrputc('_', wglb->writech);
  /* make sure we don't get no creepy spaces where they shouldn't be */
  lastw = separator;
  if (CellPtr(t) < H0) {
#if COROUTINING
#if DEBUG
    if (Yap_Portray_delays) {
      exts ext = ExtFromCell(t);

      Yap_Portray_delays = FALSE;
      if (ext == attvars_ext) {
	attvar_record *attv = (attvar_record *)t;
	long sl = 0;
	Term l = attv->Atts;

	wrputs("$AT(",wglb->writech);
	write_var(t, wglb);
	wrputc(',', wglb->writech);      
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot((CELL)attv);
	}
	writeTerm((Term)&(attv->Value), 999, 1, FALSE, wglb);
	wrputc(',', wglb->writech);
	writeTerm(l, 999, 1, FALSE, wglb);
	if (wglb->keep_terms) {
	  attv = (attvar_record *)Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	wrputc(')', wglb->writech);
      }
      Yap_Portray_delays = TRUE;
      return;
    }
#endif
#endif
    wrputc('D', wglb->writech);
    wrputn(((Int) (t- CellPtr(Yap_GlobalBase))),wglb->writech);
  } else {
    wrputn(((Int) (t- H0)),wglb->writech);
  }
}

static void 
writeTerm(Term t, int p, int depth, int rinfixarg, struct write_globs *wglb)
	                  	/* term to write			 */
	                  	/* context priority			 */
	                      
{
  if (wglb->MaxDepth != 0 && depth > wglb->MaxDepth) {
    putAtom(Yap_LookupAtom("..."), wglb->Quote_illegal, wglb->writech);
    return;
  }
  if (EX != 0)
    return;
  t = Deref(t);
  if (IsVarTerm(t)) {
    write_var((CELL *)t, wglb);
  } else if (IsIntTerm(t)) {
    wrputn((Int) IntOfTerm(t),wglb->writech);
  } else if (IsAtomTerm(t)) {
    putAtom(AtomOfTerm(t), wglb->Quote_illegal, wglb->writech);
  } else if (IsPairTerm(t)) {
    int             eldepth = 1;
    Term ti;

    if (wglb->Use_portray) {
      Term targs[1];
      Term old_EX = 0L;
      long sl = 0;

      targs[0] = t;
      Yap_PutValue(AtomPortray, MkAtomTerm(AtomNil));
      if (EX != 0L) old_EX = EX;
      sl = Yap_InitSlot(t);      
      Yap_execute_goal(Yap_MkApplTerm(FunctorPortray, 1, targs), 0, 1);
      t = Yap_GetFromSlot(sl);
      Yap_RecoverSlots(1);
      if (old_EX != 0L) EX = old_EX;
      if (Yap_GetValue(AtomPortray) == MkAtomTerm(AtomTrue))
	return;
    }
    if (yap_flags[WRITE_QUOTED_STRING_FLAG] && IsStringTerm(t)) {
      putString(t, wglb->writech);
    } else {
      wrputc('[', wglb->writech);
      lastw = separator;
      while (1) {
	int             new_depth = depth + 1;
	long            sl= 0;

	if (wglb->MaxList && eldepth > wglb->MaxList) {
	  putAtom(Yap_LookupAtom("..."), wglb->Quote_illegal, wglb->writech);
	  wrputc(']', wglb->writech);
	  lastw = separator;
	  return;
	} else {
	  eldepth++;
	}
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t);
	}
	writeTerm(HeadOfTermCell(t), 999, new_depth, FALSE, wglb);
	if (wglb->keep_terms) {
	  t = Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	ti = TailOfTerm(t);
	if (IsVarTerm(ti))
	  break;
	if (!IsPairTerm(ti))
	  break;
	t = ti;
	wrputc(',', wglb->writech);
	lastw = separator;
      }
      if (ti != MkAtomTerm(AtomNil)) {
	wrputc('|', wglb->writech);
	lastw = separator;
	writeTerm(TailOfTermCell(t), 999, depth + 1, FALSE, wglb);
      }
      wrputc(']', wglb->writech);
      lastw = separator;
    }
  } else {		/* compound term */
    Functor         functor = FunctorOfTerm(t);
    int             Arity;
    Atom            atom;
    OpEntry        *opinfo;
    int             op, lp, rp;

    if (IsExtensionFunctor(functor)) {
      switch((CELL)functor) {
      case (CELL)FunctorDouble:
	wrputf(FloatOfTerm(t),wglb->writech);
	return;
      case (CELL)FunctorIntArray:
      case (CELL)FunctorDoubleArray:
      case (CELL)FunctorDBRef:
	wrputref(RefOfTerm(t), wglb->Quote_illegal, wglb->writech);
	return;
      case (CELL)FunctorLongInt:
	wrputn(LongIntOfTerm(t),wglb->writech);
	return;
#ifdef USE_GMP
      case (CELL)FunctorBigInt:
	{
	  MP_INT *big = Yap_BigIntOfTerm(t);
	  char *s = (char *)TR;
	  if (s+2+mpz_sizeinbase(big, 10) >= Yap_TrailTop) {
	    s = (char *)H;
	    if (s+2+mpz_sizeinbase(big, 10) >= (char *)ASP) {
	      return;
	    }
	  }
	  if (!s)
	    return;
	  if (mpz_sgn(big) < 0) {
	    if (lastw == symbol)
	      wrputc(' ', wglb->writech);  
	  } else {
	    if (lastw == alphanum)
	      wrputc(' ', wglb->writech);
	  }
	  mpz_get_str(s, 10, big);
	  wrputs(s,wglb->writech);
	}
	return;
#endif
      }
    }
    Arity = ArityOfFunctor(functor);
    atom = NameOfFunctor(functor);
    opinfo = Yap_GetOpProp(atom);
#ifdef SFUNC
    if (Arity == SFArity) {
      int             argno = 1;
      CELL           *p = ArgsOfSFTerm(t);
      putAtom(atom, wglb->Quote_illegal, wglb->writech);
      wrputc('(', wglb->writech);
      lastw = separator;
      while (*p) {
	long sl = 0;

	while (argno < *p) {
	  wrputc('_', wglb->writech), wrputc(',', wglb->writech);
	  ++argno;
	}
	*p++;
	lastw = separator;
	/* cannot use the term directly with the SBA */
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot((CELL)p);
	}
	writeTerm(Deref(p++), 999, depth + 1, FALSE, wglb);
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  p = (CELL *)Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	if (*p)
	  wrputc(',', wglb->writech);
	argno++;
      }
      wrputc(')', wglb->writech);
      lastw = separator;
      return;
    }
#endif
    if (wglb->Use_portray) {
      Term targs[1];
      Term old_EX = 0L;
      long sl = 0;

      targs[0] = t;
      Yap_PutValue(AtomPortray, MkAtomTerm(AtomNil));
      if (EX != 0L) old_EX = EX;
      sl = Yap_InitSlot(t);      
      Yap_execute_goal(Yap_MkApplTerm(FunctorPortray, 1, targs),0, 1);
      t = Yap_GetFromSlot(sl);
      Yap_RecoverSlots(1);
      if (old_EX != 0L) EX = old_EX;
      if (Yap_GetValue(AtomPortray) == MkAtomTerm(AtomTrue) || EX != 0L)
	return;
    }
    if (!wglb->Ignore_ops &&
	Arity == 1 && opinfo && Yap_IsPrefixOp(opinfo, &op,
					   &rp)
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
	RightOpToProtect(AtomOfTerm(tright), rp);
      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ', wglb->writech);
	wrputc('(', wglb->writech);
	lastw = separator;
      }
      putAtom(atom, wglb->Quote_illegal, wglb->writech);
      if (bracket_right) {
	wrputc('(', wglb->writech);
	lastw = separator;
      }
      writeTerm(ArgOfTermCell(1,t), rp, depth + 1, FALSE, wglb);
      if (bracket_right) {
	wrputc(')', wglb->writech);
	lastw = separator;
      }
      if (op > p) {
	wrputc(')', wglb->writech);
	lastw = separator;
      }
    } else if (!wglb->Ignore_ops &&
	       Arity == 1 && opinfo && Yap_IsPosfixOp(opinfo, &op, &lp)) {
      Term  tleft = ArgOfTerm(1, t);
      long sl = 0;
      int            bracket_left =
	!IsVarTerm(tleft) && IsAtomTerm(tleft) &&
	LeftOpToProtect(AtomOfTerm(tleft), lp); 
      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ', wglb->writech);
	wrputc('(', wglb->writech);
	lastw = separator;
      }
      if (bracket_left) {
	wrputc('(', wglb->writech);
	lastw = separator;
      }
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	sl = Yap_InitSlot(t);      
      }
      writeTerm(ArgOfTermCell(1,t), lp, depth + 1, rinfixarg, wglb);
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	t = Yap_GetFromSlot(sl);
	Yap_RecoverSlots(1);
      }
      if (bracket_left) {
	wrputc(')', wglb->writech);
	lastw = separator;
      }
      putAtom(atom, wglb->Quote_illegal, wglb->writech);
      if (op > p) {
	wrputc(')', wglb->writech);
	lastw = separator;
      }
    } else if (!wglb->Ignore_ops &&
	       Arity == 2 && opinfo && Yap_IsInfixOp(opinfo, &op, &lp,
						 &rp) ) {
      Term  tleft = ArgOfTerm(1, t);
      Term  tright = ArgOfTerm(2, t);
      long sl = 0;
      int   bracket_left =
	!IsVarTerm(tleft) && IsAtomTerm(tleft) &&
	LeftOpToProtect(AtomOfTerm(tleft), lp);
      int   bracket_right =
	!IsVarTerm(tright) && IsAtomTerm(tright) &&
	RightOpToProtect(AtomOfTerm(tright), rp);

      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ', wglb->writech);
	wrputc('(', wglb->writech);
	lastw = separator;
      }
      if (bracket_left) {
	wrputc('(', wglb->writech);
	lastw = separator;
      }
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	sl = Yap_InitSlot(t);      
      }
      writeTerm(ArgOfTermCell(1, t), lp, depth + 1, rinfixarg, wglb);
      if (wglb->keep_terms) {
	/* garbage collection may be called */
	t = Yap_GetFromSlot(sl);
	Yap_RecoverSlots(1);
      }
      if (bracket_left) {
	wrputc(')', wglb->writech);
	lastw = separator;
      }
      /* avoid quoting commas */
      if (strcmp(RepAtom(atom)->StrOfAE,","))
	putAtom(atom, wglb->Quote_illegal, wglb->writech);
      else {
	wrputc(',', wglb->writech);
	lastw = separator;
      }
      if (bracket_right) {
	wrputc('(', wglb->writech);
	lastw = separator;
      }
      writeTerm(ArgOfTermCell(2, t), rp, depth + 1, TRUE, wglb);
      if (bracket_right) {
	wrputc(')', wglb->writech);
	lastw = separator;
      }
      if (op > p) {
	wrputc(')', wglb->writech);
	lastw = separator;
      }
    } else if (wglb->Handle_vars && functor == FunctorVar) {
      Term ti = ArgOfTerm(1, t);
      if (lastw == alphanum) {
	wrputc(' ', wglb->writech);
      }
      if (!IsVarTerm(ti) && (IsIntTerm(ti) || IsStringTerm(ti))) {
	if (IsIntTerm(ti)) {
	  Int k = IntOfTerm(ti);
	  if (k == -1)  {
	    wrputc('_', wglb->writech);
	    lastw = alphanum;
	    return;
	  } else {
	    wrputc((k % 26) + 'A', wglb->writech);
	    if (k >= 26) {
	      /* make sure we don't get confused about our context */
	      lastw = separator;
	      wrputn( k / 26 ,wglb->writech);
	    } else
	      lastw = alphanum;
	  }
	} else {
	  putUnquotedString(ti, wglb->writech);
	}
      } else {
	long sl = 0;

	wrputs("'$VAR'(",wglb->writech);
	lastw = separator;
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t);      
	}
	writeTerm(ArgOfTermCell(1,t), 999, depth + 1, FALSE, wglb);
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  t = Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	wrputc(')', wglb->writech);
	lastw = separator;
      }
    } else if (functor == FunctorBraces) {
      wrputc('{', wglb->writech);
      lastw = separator;
      writeTerm(ArgOfTermCell(1, t), 1200, depth + 1, FALSE, wglb);
      wrputc('}', wglb->writech);
      lastw = separator;
    } else  if (atom == AtomArray) {
      long sl = 0;

      wrputc('{', wglb->writech);
      lastw = separator;
      for (op = 1; op <= Arity; ++op) {
	if (op == wglb->MaxArgs) {
	  wrputc('.', wglb->writech);
	  wrputc('.', wglb->writech);
	  wrputc('.', wglb->writech);
	  break;
	}
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t);      
	}
	writeTerm(ArgOfTermCell(op, t), 999, depth + 1, FALSE, wglb);
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  t = Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	if (op != Arity) {
	  wrputc(',', wglb->writech);
	  lastw = separator;
	}
      }
      wrputc('}', wglb->writech);
      lastw = separator;
    } else {
      putAtom(atom, wglb->Quote_illegal, wglb->writech);
      lastw = separator;
      wrputc('(', wglb->writech);
      for (op = 1; op <= Arity; ++op) {
	long sl = 0;

	if (op == wglb->MaxArgs) {
	  wrputc('.', wglb->writech);
	  wrputc('.', wglb->writech);
	  wrputc('.', wglb->writech);
	  break;
	}
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t);      
	}
	writeTerm(ArgOfTermCell(op, t), 999, depth + 1, FALSE, wglb);
	if (wglb->keep_terms) {
	  /* garbage collection may be called */
	  t = Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	if (op != Arity) {
	  wrputc(',', wglb->writech);
	  lastw = separator;
	}
      }
      wrputc(')', wglb->writech);
      lastw = separator;
    }
  }
}

void 
Yap_plwrite(Term t, int (*mywrite) (int, int), int flags)
     /* term to be written			 */
     /* consumer				 */
     /* write options			 */
{
  struct write_globs wglb;

  wglb.writech = mywrite;
  lastw = separator;
  wglb.Quote_illegal = flags & Quote_illegal_f;
  wglb.Handle_vars = flags & Handle_vars_f;
  wglb.Use_portray = flags & Use_portray_f;
  wglb.MaxDepth = max_depth;
  wglb.MaxList = max_list;
  wglb.MaxArgs = max_write_args;
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  wglb.keep_terms = (flags & (Use_portray_f|To_heap_f)); 
  wglb.Ignore_ops = flags & Ignore_ops_f;
  /* protect slots for portray */
  writeTerm(t, 1200, 1, FALSE, &wglb);
}

