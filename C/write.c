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

STATIC_PROTO(void wrputn, (Int));
STATIC_PROTO(void wrputs, (char *));
STATIC_PROTO(void wrputf, (Float));
STATIC_PROTO(void wrputref, (CODEADDR));
STATIC_PROTO(int legalAtom, (char *));
STATIC_PROTO(int LeftOpToProtect, (Atom, int));
STATIC_PROTO(int RightOpToProtect, (Atom, int));
STATIC_PROTO(wtype AtomIsSymbols, (char *));
STATIC_PROTO(void putAtom, (Atom));
STATIC_PROTO(void writeTerm, (Term, int, int, int));

static int      (*writech) (int, int);
static int      Quote_illegal, Ignore_ops, Handle_vars, Use_portray;
static int      keep_terms;


#define wrputc(X)	((*writech)(Yap_c_output_stream,X))	/* writes a character */

static void 
wrputn(Int n)			/* writes an integer	 */
	                  
{
  char s[256], *s1=s; /* that should be enough for most integers */
  if (n < 0) {
    if (lastw == symbol)
      wrputc(' ');  
  } else {
    if (lastw == alphanum)
      wrputc(' ');
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
    wrputc(*s1++);
  lastw = alphanum;
}

static void 
wrputs(char *s)			/* writes a string	 */
{
  while (*s)
    wrputc(*s++);
}

static void 
wrputf(Float f)			/* writes a float	 */
	                  
{
  char            s[255], *pt = s, ch;

  if (f < 0) {
    if (lastw == symbol)
      wrputc(' ');  
  } else {
    if (lastw == alphanum)
      wrputc(' ');
  }
  lastw = alphanum;
  sprintf(s, "%.15g", f);
  while (*pt == ' ')
    pt++;
  if (*pt == 'i' || *pt == 'n')  /* inf or nan */
    wrputc('+');    
  wrputs(pt);
  if (*pt == '-') pt++;
  while ((ch = *pt) != '\0') {
    if (ch < '0' || ch > '9')
      return;
    pt++;
  }
  wrputs(".0");    
}

static void 
wrputref(CODEADDR ref)			/* writes a data base reference */
	                    
{
  char            s[256];

#if SHORT_INTS
  sprintf(s, "0x%p", ref);
#else
#ifdef linux
  sprintf(s, "%p", ref);
#else
  sprintf(s, "0x%p", ref);
#endif
#endif
  wrputs(s);
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
  Prop            opinfo = Yap_GetAProp(at, OpProperty);
  return(opinfo && Yap_IsPrefixOp(opinfo, &op, &rp) );
}

static int RightOpToProtect(Atom at, int p)
{
  int op, lp;
  Prop            opinfo = Yap_GetAProp(at, OpProperty);
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
putAtom(Atom atom)			/* writes an atom	 */
	                     
{
  char           *s = RepAtom(atom)->StrOfAE;
  wtype          atom_or_symbol = AtomIsSymbols(s);

  /* #define CRYPT_FOR_STEVE 1*/
#ifdef CRYPT_FOR_STEVE
  if (Yap_GetValue(Yap_LookupAtom("crypt_atoms")) != TermNil && Yap_GetAProp(atom, OpProperty) == NIL) {
    char s[16];
    sprintf(s,"x%x", (CELL)s);
    wrputs(s);
    return;
  }
#endif
  if (lastw == atom_or_symbol && atom_or_symbol != separator /* solo */)
    wrputc(' ');
  lastw = atom_or_symbol;
  if (!legalAtom(s) && Quote_illegal) {
    wrputc('\'');
    while (*s) {
      int ch = *s++;
      wrputc(ch);
      if (ch == '\\' && yap_flags[CHARACTER_ESCAPE_FLAG] != CPROLOG_CHARACTER_ESCAPES)
	wrputc('\\');	/* be careful about backslashes */
      else if (ch == '\'')
	wrputc('\'');	/* be careful about quotes */
    }
    wrputc('\'');
  } else {
    wrputs(s);
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
putString(Term string)			/* writes a string	 */
	                     
{
  wrputc('"');
  while (string != TermNil) {
    int ch = IntOfTerm(HeadOfTerm(string));
    wrputc(ch);
    if (ch == '\\' && yap_flags[CHARACTER_ESCAPE_FLAG] != CPROLOG_CHARACTER_ESCAPES)
      wrputc('\\');	/* be careful about backslashes */
    else if (ch == '"')
      wrputc('"');	/* be careful about quotes */
    string = TailOfTerm(string);
  }
  wrputc('"');
  lastw = alphanum;
}

static void 
putUnquotedString(Term string)		/* writes a string	 */
	                     
{
  while (string != TermNil) {
    int ch = IntOfTerm(HeadOfTerm(string));
    wrputc(ch);
    string = TailOfTerm(string);
  }
  lastw = alphanum;
}


static void
write_var(CELL *t) 
{
  if (lastw == alphanum) {
    wrputc(' ');
  }
  wrputc('_');
  /* make sure we don't get no creepy spaces where they shouldn't be */
  lastw = separator;
  if (CellPtr(t) < H0) {
#if COROUTINING
#if DEBUG
    if (Yap_Portray_delays) {
      exts ext = ExtFromCell(t);

      Yap_Portray_delays = FALSE;
      if (ext == susp_ext) {
	wrputs("$DL(");
	write_var(t);
	wrputc(')');
	lastw = separator;
      } else if (ext == attvars_ext) {
	attvar_record *attv = (attvar_record *)t;
	int i;
	long sl = 0;

	wrputs("$AT(");
	write_var(t);
	wrputc(',');      
	if (keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot((CELL)attv);
	}
	writeTerm((Term)&(attv->Value), 999, 1, FALSE);
	if (keep_terms) {
	  attv = (attvar_record *)Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	for (i = 0; i < NUM_OF_ATTS; i ++) {
	  if (!IsVarTerm(attv->Atts[2*i+1])) {
	    long sl = 0;

	    wrputc(',');
	    if (keep_terms) {
	      /* garbage collection may be called */
	      sl = Yap_InitSlot((CELL)attv);
	    }
	    writeTerm((Term)&(attv->Atts[2*i+1]), 999, 1, FALSE);
	    if (keep_terms) {
	      attv = (attvar_record *)Yap_GetFromSlot(sl);
	      Yap_RecoverSlots(1);
	    }
	  }
	}
	wrputc(')');
      }
      Yap_Portray_delays = TRUE;
      return;
    }
#endif
#endif
    wrputc('D');
    wrputn(((Int) (t- CellPtr(Yap_GlobalBase))));
  } else {
    wrputn(((Int) (t- H0)));
  }
}

static void 
writeTerm(Term t, int p, int depth, int rinfixarg)
	                  	/* term to write			 */
	                  	/* context priority			 */
	                      
{
  if (*max_depth != 0 && depth > *max_depth) {
    putAtom(Yap_LookupAtom("..."));
    return;
  }
  if (EX != 0)
    return;
  t = Deref(t);
  if (IsVarTerm(t)) {
    write_var((CELL *)t);
  } else if (IsIntTerm(t)) {
    wrputn((Int) IntOfTerm(t));
  } else if (IsAtomTerm(t)) {
    putAtom(AtomOfTerm(t));
  } else if (IsFloatTerm(t)) {
    wrputf(FloatOfTerm(t));
  } else if (IsRefTerm(t)) {
    wrputref(RefOfTerm(t));
  } else if (IsLongIntTerm(t)) {
    wrputn(LongIntOfTerm(t));
#ifdef USE_GMP
  } else if (IsBigIntTerm(t)) {
    char *s = (char *)TR;
    while (s+2+mpz_sizeinbase(Yap_BigIntOfTerm(t), 10) > (char *)Yap_TrailTop)
      Yap_growtrail(64*1024);
    mpz_get_str(s, 10, Yap_BigIntOfTerm(t));
    wrputs(s);
#endif
  } else if (IsPairTerm(t)) {
    int             eldepth = 1;
    Term ti;

    if (Use_portray) {
      Term targs[1];
      Term old_EX = 0L;
      long sl = 0;

      targs[0] = t;
      Yap_PutValue(AtomPortray, MkAtomTerm(AtomNil));
      if (EX != 0L) old_EX = EX;
      /* *--ASP = MkIntTerm(0); */
      sl = Yap_InitSlot(t);      
      Yap_execute_goal(Yap_MkApplTerm(FunctorPortray, 1, targs), 0, 1);
      t = Yap_GetFromSlot(sl);
      Yap_RecoverSlots(1);
      if (old_EX != 0L) EX = old_EX;
      Use_portray = TRUE;
      Use_portray = TRUE;
      if (Yap_GetValue(AtomPortray) == MkAtomTerm(AtomTrue))
	return;
    }
    if (yap_flags[WRITE_QUOTED_STRING_FLAG] && IsStringTerm(t)) {
      putString(t);
    } else {
      wrputc('[');
      lastw = separator;
      while (1) {
	int             new_depth = depth + 1;
	long            sl= 0;

	if (*max_list && eldepth > *max_list) {
	  putAtom(Yap_LookupAtom("..."));
	  wrputc(']');
	  lastw = separator;
	  return;
	} else
	  eldepth++;
	if (keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t);
	}
	writeTerm(HeadOfTermCell(t), 999, new_depth, FALSE);
	if (keep_terms) {
	  t = Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	ti = TailOfTerm(t);
	if (IsVarTerm(ti))
	  break;
	if (!IsPairTerm(ti))
	  break;
	t = ti;
	wrputc(',');
	lastw = separator;
      }
      if (ti != MkAtomTerm(AtomNil)) {
	wrputc('|');
	lastw = separator;
	writeTerm(TailOfTermCell(t), 999, depth + 1, FALSE);
      }
      wrputc(']');
      lastw = separator;
    }
  } else {		/* compound term */
    Functor         functor = FunctorOfTerm(t);
    int             Arity;
    Atom            atom;
    Prop            opinfo;
    int             op, lp, rp;

    Arity = ArityOfFunctor(functor);
    atom = NameOfFunctor(functor);
    opinfo = Yap_GetAProp(atom, OpProperty);
#ifdef SFUNC
    if (Arity == SFArity) {
      int             argno = 1;
      CELL           *p = ArgsOfSFTerm(t);
      putAtom(atom);
      wrputc('(');
      lastw = separator;
      while (*p) {
	long sl = 0;

	while (argno < *p) {
	  wrputc('_'), wrputc(',');
	  ++argno;
	}
	*p++;
	lastw = separator;
	/* cannot use the term directly with the SBA */
	if (keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot((CELL)p);
	}
	writeTerm(Deref(p++), 999, depth + 1, FALSE);
	if (keep_terms) {
	  /* garbage collection may be called */
	  p = (CELL *)Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	if (*p)
	  wrputc(',');
	argno++;
      }
      wrputc(')');
      lastw = separator;
      return;
    }
#endif
    if (Use_portray) {
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
      Use_portray = TRUE;
      if (Yap_GetValue(AtomPortray) == MkAtomTerm(AtomTrue) || EX != 0L)
	return;
    }
    if (!Ignore_ops &&
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
	  wrputc(' ');
	wrputc('(');
	lastw = separator;
      }
      putAtom(atom);
      if (bracket_right) {
	wrputc('(');
	lastw = separator;
      }
      writeTerm(ArgOfTermCell(1,t), rp, depth + 1, FALSE);
      if (bracket_right) {
	wrputc(')');
	lastw = separator;
      }
      if (op > p) {
	wrputc(')');
	lastw = separator;
      }
    } else if (!Ignore_ops &&
	       Arity == 1 && opinfo && Yap_IsPosfixOp(opinfo, &op, &lp)) {
      Term  tleft = ArgOfTerm(1, t);
      long sl = 0;
      int            bracket_left =
	!IsVarTerm(tleft) && IsAtomTerm(tleft) &&
	LeftOpToProtect(AtomOfTerm(tleft), lp); 
      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ');
	wrputc('(');
	lastw = separator;
      }
      if (bracket_left) {
	wrputc('(');
	lastw = separator;
      }
      if (keep_terms) {
	/* garbage collection may be called */
	sl = Yap_InitSlot(t);      
      }
      writeTerm(ArgOfTermCell(1,t), lp, depth + 1, rinfixarg);
      if (keep_terms) {
	/* garbage collection may be called */
	t = Yap_GetFromSlot(sl);
	Yap_RecoverSlots(1);
      }
      if (bracket_left) {
	wrputc(')');
	lastw = separator;
      }
      putAtom(atom);
      if (op > p) {
	wrputc(')');
	lastw = separator;
      }
    } else if (!Ignore_ops &&
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
	  wrputc(' ');
	wrputc('(');
	lastw = separator;
      }
      if (bracket_left) {
	wrputc('(');
	lastw = separator;
      }
      if (keep_terms) {
	/* garbage collection may be called */
	sl = Yap_InitSlot(t);      
      }
      writeTerm(ArgOfTermCell(1, t), lp, depth + 1, rinfixarg);
      if (keep_terms) {
	/* garbage collection may be called */
	t = Yap_GetFromSlot(sl);
	Yap_RecoverSlots(1);
      }
      if (bracket_left) {
	wrputc(')');
	lastw = separator;
      }
      /* avoid quoting commas */
      if (strcmp(RepAtom(atom)->StrOfAE,","))
	putAtom(atom);
      else {
	wrputc(',');
	lastw = separator;
      }
      if (bracket_right) {
	wrputc('(');
	lastw = separator;
      }
      writeTerm(ArgOfTermCell(2, t), rp, depth + 1, TRUE);
      if (bracket_right) {
	wrputc(')');
	lastw = separator;
      }
      if (op > p) {
	wrputc(')');
	lastw = separator;
      }
    } else if (Handle_vars && functor == FunctorVar) {
      Term ti = ArgOfTerm(1, t);
      if (lastw == alphanum) {
	wrputc(' ');
      }
      if (!IsVarTerm(ti) && (IsIntTerm(ti) || IsStringTerm(ti))) {
	if (IsIntTerm(ti)) {
	  Int k = IntOfTerm(ti);
	  if (k == -1)  {
	    wrputc('_');
	    lastw = alphanum;
	    return;
	  } else {
	    wrputc((k % 26) + 'A');
	    if (k >= 26) {
	      /* make sure we don't get confused about our context */
	      lastw = separator;
	      wrputn( k / 26 );
	    } else
	      lastw = alphanum;
	  }
	} else {
	  putUnquotedString(ti);
	}
      } else {
	long sl = 0;

	wrputs("'$VAR'(");
	lastw = separator;
	if (keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t);      
	}
	writeTerm(ArgOfTermCell(1,t), 999, depth + 1, FALSE);
	if (keep_terms) {
	  /* garbage collection may be called */
	  t = Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	wrputc(')');
	lastw = separator;
      }
    } else if (functor == FunctorBraces) {
      wrputc('{');
      lastw = separator;
      writeTerm(ArgOfTermCell(1, t), 1200, depth + 1, FALSE);
      wrputc('}');
      lastw = separator;
    } else  if (atom == AtomArray) {
      long sl = 0;

      wrputc('{');
      lastw = separator;
      for (op = 1; op <= Arity; ++op) {
	if (keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t);      
	}
	writeTerm(ArgOfTermCell(op, t), 999, depth + 1, FALSE);
	if (keep_terms) {
	  /* garbage collection may be called */
	  t = Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	if (op != Arity) {
	  wrputc(',');
	  lastw = separator;
	}
      }
      wrputc('}');
      lastw = separator;
    } else {
      putAtom(atom);
      lastw = separator;
      wrputc('(');
      for (op = 1; op <= Arity; ++op) {
	long sl = 0;

	if (keep_terms) {
	  /* garbage collection may be called */
	  sl = Yap_InitSlot(t);      
	}
	writeTerm(ArgOfTermCell(op, t), 999, depth + 1, FALSE);
	if (keep_terms) {
	  /* garbage collection may be called */
	  t = Yap_GetFromSlot(sl);
	  Yap_RecoverSlots(1);
	}
	if (op != Arity) {
	  wrputc(',');
	  lastw = separator;
	}
      }
      wrputc(')');
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
  writech = mywrite;
  lastw = separator;
  Quote_illegal = flags & Quote_illegal_f;
  Handle_vars = flags & Handle_vars_f;
  Use_portray = flags & Use_portray_f;
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  keep_terms = Use_portray;
  Ignore_ops = flags & Ignore_ops_f;
  writeTerm(t, 1200, 1, FALSE);
}

