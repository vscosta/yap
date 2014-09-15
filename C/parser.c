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
* File:		parser.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Prolog's parser						 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**

@defgroup Syntax YAP Syntax
@ingroup YAPProgramming
@{

We will describe the syntax of YAP at two levels. We first will
describe the syntax for Prolog terms. In a second level we describe
the \a tokens from which Prolog \a terms are
built.

@defgroup Formal_Syntax Syntax of Terms
@ingroup Syntax
@{

Below, we describe the syntax of YAP terms from the different
classes of tokens defined above. The formalism used will be <em>BNF</em>,
extended where necessary with attributes denoting integer precedence or
operator type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 term       ---->     subterm(1200)   end_of_term_marker

 subterm(N) ---->     term(M)         [M <= N]

 term(N)    ---->     op(N, fx) subterm(N-1)
             |        op(N, fy) subterm(N)
             |        subterm(N-1) op(N, xfx) subterm(N-1)
             |        subterm(N-1) op(N, xfy) subterm(N)
             |        subterm(N) op(N, yfx) subterm(N-1)
             |        subterm(N-1) op(N, xf)
             |        subterm(N) op(N, yf)

 term(0)   ---->      atom '(' arguments ')'
             |        '(' subterm(1200)  ')'
             |        '{' subterm(1200)  '}'
             |        list
             |        string
             |        number
             |        atom
             |        variable

 arguments ---->      subterm(999)
             |        subterm(999) ',' arguments

 list      ---->      '[]'
             |        '[' list_expr ']'

 list_expr ---->      subterm(999)
             |        subterm(999) list_tail

 list_tail ---->      ',' list_expr
             |        ',..' subterm(999)
             |        '|' subterm(999)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Notes:

   + \a op(N,T) denotes an atom which has been previously declared with type
      \a T and base precedence \a N.

  + Since ',' is itself a pre-declared operator with type \a xfy and
       precedence 1000, is \a subterm starts with a '(', \a op must be
       followed by a space to avoid ambiguity with the case of a functor
       followed by arguments, e.g.:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
+ (a,b)        [the same as '+'(','(a,b)) of arity one]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      versus

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
+(a,b)         [the same as '+'(a,b) of arity two]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  + 
In the first rule for term(0) no blank space should exist between
\a atom and '('.

  + 
Each term to be read by the YAP parser must end with a single
dot, followed by a blank (in the sense mentioned in the previous
paragraph). When a name consisting of a single dot could be taken for
the end of term marker, the ambiguity should be avoided by surrounding the
dot with single quotes.

@}

*/

/*
 * Description: 
 *
 * parser:     produces a prolog term from an array of tokens 
 *
 * parser usage: the parser takes its input from an array of token descriptions
 * addressed by the global variable 'tokptr' and produces a Term as result. A
 * macro 'NextToken' should be defined in 'yap.h' for advancing 'tokptr' from
 * one token to the next. In the distributed version this macro also updates
 * a variable named 'toktide' for keeping track of how far the parser went
 * before failling with a syntax error. The parser should be invoked with
 * 'tokptr' pointing to the first token. The last token should have type
 * 'eot_tok'. The parser return either a Term. Syntactic errors are signaled
 * by a return value 0. The parser builds new terms on the 'global stack' and
 * also uses an auxiliary stack pointed to by 'AuxSp'. In the distributed
 * version this auxiliary stack is assumed to grow downwards. This
 * assumption, however, is only relevant to routine 'ParseArgs', and to the
 * variable toktide. conclusion: set tokptr pointing to first token set AuxSp
 * Call Parse 
 *
 * VSC: Working whithout known bugs in 87/4/6 
 *
 * LD: -I or +I evaluated by parser 87/4/28 
 *
 * LD: parser extended 87/4/28 
 *
 */


#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "eval.h"
/* stuff we want to use in standard YAP code */
#include "pl-shared.h"
#include "YapText.h"
#include "pl-read.h"
#include "pl-text.h"
#if HAVE_STRING_H
#include <string.h>
#endif

#ifdef __STDC__XXX 
#define Volatile volatile
#else
#define Volatile
#endif


/* weak backtraking mechanism based on long_jump */

typedef struct jmp_buff_struct {
	sigjmp_buf JmpBuff;
} JMPBUFF;

static void GNextToken( CACHE_TYPE1 );
static void checkfor(wchar_t, JMPBUFF * CACHE_TYPE);
static Term ParseArgs(read_data *, Atom, wchar_t, JMPBUFF *, Term CACHE_TYPE);
static Term ParseList(read_data *, JMPBUFF * CACHE_TYPE);
static Term ParseTerm(read_data *, int, JMPBUFF * CACHE_TYPE);


#define TRY(S,P)                               \
  {	Volatile JMPBUFF *saveenv, newenv;     \
	Volatile TokEntry *saveT=LOCAL_tokptr;   \
        Volatile CELL *saveH=HR;                \
        Volatile int savecurprio=curprio;      \
        saveenv=FailBuff;                      \
        if(!sigsetjmp(newenv.JmpBuff, 0)) {      \
                FailBuff = &newenv;            \
		S;                             \
		FailBuff=saveenv;              \
		P;                             \
	  }                                    \
	else { FailBuff=saveenv;               \
		HR=saveH;                       \
		curprio = savecurprio;         \
                LOCAL_tokptr=saveT;              \
	}                                      \
   }

#define TRY3(S,P,F)                            \
  {	Volatile JMPBUFF *saveenv, newenv;     \
	Volatile TokEntry *saveT=LOCAL_tokptr;   \
        Volatile CELL *saveH=HR;                \
        saveenv=FailBuff;                      \
        if(!sigsetjmp(newenv.JmpBuff, 0)) {      \
                FailBuff = &newenv;            \
		S;                             \
		FailBuff=saveenv;              \
		P;                             \
	  }                                    \
	else {                                 \
                FailBuff=saveenv;              \
                HR=saveH;                       \
                LOCAL_tokptr=saveT;              \
                F }                            \
   }


#define FAIL  siglongjmp(FailBuff->JmpBuff,1)

VarEntry *
Yap_LookupVar(char *var)	/* lookup variable in variables table   */
{
  CACHE_REGS
  VarEntry *p;

#if DEBUG
  if (GLOBAL_Option[4])
    fprintf(stderr,"[LookupVar %s]", var);
#endif
  if (var[0] != '_' || var[1] != '\0') {
    VarEntry **op = &LOCAL_VarTable;
    unsigned char *vp = (unsigned char *)var;
    UInt hv;

    p = LOCAL_VarTable;
    hv = HashFunction(vp) % AtomHashTableSize;
    while (p != NULL) {
      CELL hpv = p->hv;
      if (hv == hpv) {
	Int scmp;
	if ((scmp = strcmp(var, p->VarRep)) == 0) {
	  p->refs++;
	  return(p);
	} else if (scmp < 0) {
	  op = &(p->VarLeft);
	  p = p->VarLeft;
	} else {
	  op = &(p->VarRight);
	  p = p->VarRight;
	}
      } else if (hv < hpv) {
	op = &(p->VarLeft);
	p = p->VarLeft;
      } else {
	op = &(p->VarRight);
	p = p->VarRight;
      }
    }
    p = (VarEntry *) Yap_AllocScannerMemory(strlen(var) + sizeof(VarEntry));
    *op = p;
    p->VarLeft = p->VarRight = NULL;
    p->hv = hv;
    p->refs = 1L;
    strcpy(p->VarRep, var);
  } else {
    /* anon var */
    p = (VarEntry *) Yap_AllocScannerMemory(sizeof(VarEntry) + 2);
    p->VarLeft = LOCAL_AnonVarTable;
    LOCAL_AnonVarTable = p;
    p->VarRight = NULL;    
    p->refs = 0L;
    p->hv = 1L;
    p->VarRep[0] = '_';
    p->VarRep[1] = '\0';
  }
  p->VarAdr = TermNil;
  return (p);
}

static Term
VarNames(VarEntry *p,Term l USES_REGS)
{
  if (p != NULL) {
    if (strcmp(p->VarRep, "_") != 0) {
      Term t[2];
      Term o;
      
      t[0] = MkAtomTerm(Yap_LookupAtom(p->VarRep));
      t[1] = p->VarAdr;
      o = Yap_MkApplTerm(FunctorEq, 2, t);
      o = MkPairTerm(o, VarNames(p->VarRight,
				 VarNames(p->VarLeft,l PASS_REGS) PASS_REGS));
      if (HR > ASP-4096) {
	save_machine_regs();
	siglongjmp(LOCAL_IOBotch,1);
      }  
      return(o);
    } else {
      return VarNames(p->VarRight,VarNames(p->VarLeft,l PASS_REGS) PASS_REGS);
    }
  } else {
    return (l);
  }
}

Term
Yap_VarNames(VarEntry *p,Term l)
{
  CACHE_REGS
  return VarNames(p,l PASS_REGS);
}

static Term
Singletons(VarEntry *p,Term l USES_REGS)
{
  if (p != NULL) {
    if (p->VarRep && p->VarRep[0] !=  '_' && p->refs == 1) {
      Term t[2];
      Term o;
      
      t[0] = MkAtomTerm(Yap_LookupAtom(p->VarRep));
      t[1] = p->VarAdr;
      o = Yap_MkApplTerm(FunctorEq, 2, t);
      o = MkPairTerm(o, Singletons(p->VarRight,
				 Singletons(p->VarLeft,l PASS_REGS) PASS_REGS));
      if (HR > ASP-4096) {
	save_machine_regs();
	siglongjmp(LOCAL_IOBotch,1);
      }  
      return(o);
    } else {
      return Singletons(p->VarRight,Singletons(p->VarLeft,l PASS_REGS) PASS_REGS);
    }
  } else {
    return (l);
  }
}

Term
Yap_Singletons(VarEntry *p,Term l)
{
  CACHE_REGS
  return Singletons(p,l PASS_REGS);
}


static Term
Variables(VarEntry *p,Term l USES_REGS)
{
  if (p != NULL) {
    Term o;
    o = MkPairTerm(p->VarAdr, Variables(p->VarRight,Variables(p->VarLeft,l PASS_REGS) PASS_REGS));
    if (HR > ASP-4096) {
      save_machine_regs();
      siglongjmp(LOCAL_IOBotch,1);
    }  
    return(o);
  } else {
    return (l);
  }
}

Term
Yap_Variables(VarEntry *p,Term l)
{
  CACHE_REGS
  return Variables(p,l PASS_REGS);
}

static int
IsPrefixOp(Atom op,int  *pptr, int *rpptr USES_REGS)
{
  int p;

  OpEntry *opp = Yap_GetOpProp(op, PREFIX_OP PASS_REGS);
  if (!opp)
    return FALSE;
  if (opp->OpModule &&
      opp->OpModule != CurrentModule) {
    READ_UNLOCK(opp->OpRWLock);
    return FALSE;
  }
  if ((p = opp->Prefix) != 0) {
    READ_UNLOCK(opp->OpRWLock);
    *pptr = *rpptr = p & MaskPrio;
    if (p & DcrrpFlag)
      --* rpptr;
    return TRUE;
  } else {
    READ_UNLOCK(opp->OpRWLock);
    return FALSE;
  }
}

int
Yap_IsPrefixOp(Atom op,int  *pptr, int *rpptr)
{
  CACHE_REGS
  return IsPrefixOp(op,pptr,rpptr PASS_REGS);
}

static int
IsInfixOp(Atom op, int *pptr, int *lpptr, int *rpptr USES_REGS)
{
  int p;

  OpEntry *opp = Yap_GetOpProp(op, INFIX_OP PASS_REGS);
  if (!opp)
    return FALSE;
  if (opp->OpModule &&
      opp->OpModule != CurrentModule) {
    READ_UNLOCK(opp->OpRWLock);
    return FALSE;
  }
  if ((p = opp->Infix) != 0) {
    READ_UNLOCK(opp->OpRWLock);
    *pptr = *rpptr = *lpptr = p & MaskPrio;
    if (p & DcrrpFlag)
      --* rpptr;
    if (p & DcrlpFlag)
      --* lpptr;
    return TRUE;
  } else {
    READ_UNLOCK(opp->OpRWLock);
    return FALSE;
  }
}

int
Yap_IsInfixOp(Atom op, int *pptr, int *lpptr, int *rpptr)
{
  CACHE_REGS
  return IsInfixOp(op, pptr, lpptr, rpptr PASS_REGS);
}

static int
IsPosfixOp(Atom op, int *pptr, int *lpptr USES_REGS)
{
  int p;

  OpEntry *opp = Yap_GetOpProp(op, POSFIX_OP PASS_REGS);
  if (!opp)
    return FALSE;
  if (opp->OpModule &&
      opp->OpModule != CurrentModule) {
    READ_UNLOCK(opp->OpRWLock);
    return FALSE;
  }
  if ((p = opp->Posfix) != 0) {
    READ_UNLOCK(opp->OpRWLock);
    *pptr = *lpptr = p & MaskPrio;
    if (p & DcrlpFlag)
      --* lpptr;
    return (TRUE);
  } else {
    READ_UNLOCK(opp->OpRWLock);
    return (FALSE);
  }
}

int
Yap_IsPosfixOp(Atom op, int *pptr, int *lpptr)
{
  CACHE_REGS
  return IsPosfixOp(op, pptr, lpptr PASS_REGS);
}

inline static void
GNextToken( USES_REGS1 )
{
	if (LOCAL_tokptr->Tok == Ord(eot_tok))
		return;
	if (LOCAL_tokptr == LOCAL_toktide)
	  LOCAL_toktide = LOCAL_tokptr = LOCAL_tokptr->TokNext;
	else
	  LOCAL_tokptr = LOCAL_tokptr->TokNext;
}

inline static void
checkfor(wchar_t c, JMPBUFF *FailBuff USES_REGS)
{
  if (LOCAL_tokptr->Tok != Ord(Ponctuation_tok)
      || LOCAL_tokptr->TokInfo != (Term)c)
    FAIL;
  NextToken;
}

#ifdef O_QUASIQUOTATIONS


static int
is_quasi_quotation_syntax(Term goal, ReadData _PL_rd, Atom *pat)
{ CACHE_REGS
  Term m = CurrentModule, t;
  Atom at;
  UInt arity;
  Functor f;

  t = Yap_StripModule(goal, &m);
  f = FunctorOfTerm( t );
  *pat = at = NameOfFunctor( f );
  arity = ArityOfFunctor( f );
  if ( arity > 0 )
    return TRUE;
  return FALSE;
}

static int
get_quasi_quotation(term_t t, unsigned char **here, unsigned char *ein,
		    ReadData _PL_rd)
{ unsigned char *in, *start = *here;

  for(in=start; in <= ein; in++)
    { if ( in[0] == '}' &&
	   in[-1] == '|' )
	{ *here = in+1;			/* after } */
	  in--;				/* Before | */

	  if ( _PL_rd->quasi_quotations )	/* option; must return strings */
	    { PL_chars_t txt;
	      int rc;

	      txt.text.t    = (char*)start;
	      txt.length    = in-start;
	      txt.storage   = PL_CHARS_HEAP;
	      txt.encoding  = ENC_UTF8;
	      txt.canonical = FALSE;

	      rc = PL_unify_text(t, 0, &txt, PL_CODE_LIST);
	      PL_free_text(&txt);

	      return rc;
	    } else
	    { return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_dquasi_quotation3,
				   PL_POINTER, _PL_rd,
				   PL_INTPTR, (intptr_t)(start),
				   PL_INTPTR, (intptr_t)(in-start));
	    }
	}
    }

  return FALSE; //errorWarning("end_of_file_in_quasi_quotation", 0, _PL_rd);
}
#endif /*O_QUASIQUOTATIONS*/


static Term
ParseArgs(read_data *rd, Atom a, wchar_t close, JMPBUFF *FailBuff, Term arg1 USES_REGS)
{
  int nargs = 0;
  Term *p, t;
  Functor func;
#ifdef SFUNC
  SFEntry *pe = (SFEntry *) Yap_GetAProp(a, SFProperty);
#endif
  
  NextToken;
  p = (Term *) ParserAuxSp;
  if (arg1) {
    *p = arg1;
    nargs++;
    ParserAuxSp = (char *)(p+1);
    if (LOCAL_tokptr->Tok == Ord(Ponctuation_tok)
	&& LOCAL_tokptr->TokInfo == close) {

      func = Yap_MkFunctor(a, 1);
      if (func == NULL) {
	LOCAL_ErrorMessage = "Heap Overflow";
	FAIL;
      }
      t = Yap_MkApplTerm(func, nargs, p);
      if (HR > ASP-4096) {
	LOCAL_ErrorMessage = "Stack Overflow";
	return TermNil;
      }  
      NextToken;
      return t;
    }
  }
  while (1) {
    Term *tp = (Term *)ParserAuxSp;
    if (ParserAuxSp+1 > LOCAL_TrailTop) {
      LOCAL_ErrorMessage = "Trail Overflow";
      FAIL;
    }
    *tp++ = Unsigned(ParseTerm(rd, 999, FailBuff PASS_REGS));
    ParserAuxSp = (char *)tp;
    ++nargs;
    if (LOCAL_tokptr->Tok != Ord(Ponctuation_tok))
      break;
    if (((int) LOCAL_tokptr->TokInfo) != ',')
      break;
    NextToken;
  }
  ParserAuxSp = (char *)p;
  /*
   * Needed because the arguments for the functor are placed in reverse
   * order 
   */
  if (HR > ASP-(nargs+1)) {
    LOCAL_ErrorMessage = "Stack Overflow";
    FAIL;
  }  
  func = Yap_MkFunctor(a, nargs);
  if (func == NULL) {
    LOCAL_ErrorMessage = "Heap Overflow";
    FAIL;
  }
#ifdef SFUNC
  if (pe)
    t = MkSFTerm(Yap_MkFunctor(a, SFArity), nargs, p, pe->NilValue);
  else
    t = Yap_MkApplTerm(Yap_MkFunctor(a, nargs), nargs, p);
#else
  if (a == AtomDBref && nargs == 2) 
    t = MkDBRefTerm((DBRef)IntegerOfTerm(p[0]));
  else
    t = Yap_MkApplTerm(func, nargs, p);
#endif
  if (HR > ASP-4096) {
    LOCAL_ErrorMessage = "Stack Overflow";
    return TermNil;
  }  
  /* check for possible overflow against local stack */
  checkfor(close, FailBuff PASS_REGS);
  return t;
}

static Term MakeAccessor( Term t, Functor f USES_REGS )
{
  UInt arity = ArityOfFunctor(FunctorOfTerm(t)), i;
  Term tf[2], tl= TermNil;
  
  tf[1] = ArgOfTerm(1, t);
  for (i = arity; i > 1; i--) {
    tl = MkPairTerm(ArgOfTerm(i, t), tl);
  }
  tf[0] = tl;
  return Yap_MkApplTerm( f, 2, tf );
}

static Term
ParseList(read_data *rd, JMPBUFF *FailBuff USES_REGS)
{
  Term o;
  CELL *to_store;
  o = AbsPair(HR);
 loop:
  to_store = HR;
  HR+=2;
  to_store[0] = ParseTerm(rd, 999, FailBuff PASS_REGS);
  if (LOCAL_tokptr->Tok == Ord(Ponctuation_tok)) {
    if (((int) LOCAL_tokptr->TokInfo) == ',') {
      NextToken;
      if (LOCAL_tokptr->Tok == Ord(Name_tok)
	  && strcmp(RepAtom((Atom)(LOCAL_tokptr->TokInfo))->StrOfAE, "..") == 0) {
	NextToken;
	to_store[1] = ParseTerm(rd, 999, FailBuff PASS_REGS);
      } else {
	/* check for possible overflow against local stack */
	if (HR > ASP-4096) {
	  to_store[1] = TermNil;
	  LOCAL_ErrorMessage = "Stack Overflow";
	  FAIL;
	} else {
	  to_store[1] = AbsPair(HR);
	  goto loop;
	}
      }
    } else if (((int) LOCAL_tokptr->TokInfo) == '|') {
      NextToken;
      to_store[1] = ParseTerm(rd, 999, FailBuff PASS_REGS);
    } else {
      to_store[1] = MkAtomTerm(AtomNil);
    }
  } else
    FAIL;
  return (o);
}

#ifndef INFINITY
#define INFINITY (1.0/0.0)
#endif

#ifndef NAN
#define NAN      (0.0/0.0)
#endif

static Term
ParseTerm(read_data *rd, int prio, JMPBUFF *FailBuff USES_REGS)
{
  /* parse term with priority prio */
  Volatile Term t;
  Volatile Functor func;
  Volatile VarEntry *varinfo;
  Volatile int curprio = 0, opprio, oplprio, oprprio;
  Volatile Atom opinfo;

  switch (LOCAL_tokptr->Tok) {
  case Name_tok:
    t = LOCAL_tokptr->TokInfo;
    NextToken;
    /* special rules apply for +1, -2.3, etc... */
    if (LOCAL_tokptr->Tok == Number_tok) {
      if ((Atom)t == AtomMinus) {
	t = LOCAL_tokptr->TokInfo;
	if (IsIntTerm(t))
	  t = MkIntTerm(-IntOfTerm(t));
	else if (IsFloatTerm(t))
	  t = MkFloatTerm(-FloatOfTerm(t));
#ifdef USE_GMP
	else if (IsBigIntTerm(t)) {
	  t = Yap_gmp_neg_big(t);
	}
#endif
	else
	  t = MkLongIntTerm(-LongIntOfTerm(t));
	NextToken;
	break;
      }
    }
    if ((LOCAL_tokptr->Tok != Ord(Ponctuation_tok)
	 || Unsigned(LOCAL_tokptr->TokInfo) != 'l')
	&& IsPrefixOp((Atom)t, &opprio, &oprprio PASS_REGS)
	) {
	if (LOCAL_tokptr->Tok == Name_tok) {
	  Atom at = (Atom)LOCAL_tokptr->TokInfo;
#ifndef _MSC_VER
	  if ((Atom)t == AtomPlus) {
	    if (at == AtomInf) {
	      t = MkFloatTerm(INFINITY);
	      NextToken;
	    break;
	    } else if (at == AtomNan) {
	      t = MkFloatTerm(NAN);
	      NextToken;
	      break;
	    }
	  } else if ((Atom)t == AtomMinus) {
	    if (at == AtomInf) {
	      t = MkFloatTerm(-INFINITY);
	      NextToken;
	      break;
	    } else if (at == AtomNan) {
	      t = MkFloatTerm(NAN);
	      NextToken;
	      break;
	    }
	  }
#endif
	}
      if (opprio <= prio) {
      /* try to parse as a prefix operator */
	TRY(
	  /* build appl on the heap */
	  func = Yap_MkFunctor((Atom) t, 1);
	  if (func == NULL) {
	    LOCAL_ErrorMessage = "Heap Overflow";
	    FAIL;
	  }
	  t = ParseTerm(rd, oprprio, FailBuff PASS_REGS);
	  t = Yap_MkApplTerm(func, 1, &t);
	  /* check for possible overflow against local stack */
	  if (HR > ASP-4096) {
	    LOCAL_ErrorMessage = "Stack Overflow";
	    FAIL;
	  }  
	  curprio = opprio;
	  ,
	  break;
	  )
	}
    }
    if (LOCAL_tokptr->Tok == Ord(Ponctuation_tok)
	&& Unsigned(LOCAL_tokptr->TokInfo) == 'l')
      t = ParseArgs(rd, (Atom) t, ')', FailBuff, 0L PASS_REGS);
    else
      t = MkAtomTerm((Atom)t);
    break;

  case Number_tok:
    t = LOCAL_tokptr->TokInfo;
    NextToken;
    break;

  case String_tok:	/* build list on the heap */
    {
      Volatile char *p = (char *) LOCAL_tokptr->TokInfo;
      t = Yap_CharsToTDQ(p, CurrentModule PASS_REGS);
      if (!t) {
	FAIL;
      }
      NextToken;
    }
  break;

  case WString_tok:	/* build list on the heap */
    {
      Volatile wchar_t *p = (wchar_t *) LOCAL_tokptr->TokInfo;
      t = Yap_WCharsToTDQ(p, CurrentModule PASS_REGS);
      if (!t) {
	FAIL;
      }
      NextToken;
    }
  break;

  case Var_tok:
    varinfo = (VarEntry *) (LOCAL_tokptr->TokInfo);
    if ((t = varinfo->VarAdr) == TermNil) {
      t = varinfo->VarAdr = MkVarTerm();
    }
    NextToken;
    break;

  case Error_tok:
    FAIL;

  case Ponctuation_tok:
    switch ((int) LOCAL_tokptr->TokInfo) {
    case '(':
    case 'l':	/* non solo ( */
      NextToken;
      t = ParseTerm(rd, 1200, FailBuff PASS_REGS);
      checkfor(')', FailBuff PASS_REGS);
      break;
    case '[':
      NextToken;
      if (LOCAL_tokptr->Tok == Ponctuation_tok &&
	  (int) LOCAL_tokptr->TokInfo == ']') {
	t = TermNil;
	NextToken;
	break;
      }
      t = ParseList(rd, FailBuff PASS_REGS);
      checkfor(']', FailBuff PASS_REGS);
      break;
    case '{':
      NextToken;
      if (LOCAL_tokptr->Tok == Ponctuation_tok &&
	  (int) LOCAL_tokptr->TokInfo == '}') {
	t = MkAtomTerm(AtomBraces);
	NextToken;
	break;
      }
      t = ParseTerm(rd, 1200, FailBuff PASS_REGS);
      t = Yap_MkApplTerm(FunctorBraces, 1, &t);
      /* check for possible overflow against local stack */
      if (HR > ASP-4096) {
	LOCAL_ErrorMessage = "Stack Overflow";
	FAIL;
      }  
      checkfor('}', FailBuff PASS_REGS);
      break;
    default:
      FAIL;
    }
    break;
    
  case QuasiQuotes_tok:
    {
      qq_t *qq = (qq_t *)(LOCAL_tokptr->TokInfo);
      term_t pv, positions = rd->subtpos, to;
      Atom at;
      Term tn;
      CELL *tnp;

      // from SWI, enter the list
      /* prepare (if we are the first in term) */
      if ( !rd->varnames )
	rd->varnames = PL_new_term_ref();
      if ( !rd->qq )
      { if ( rd->quasi_quotations )
	{ rd->qq = rd->quasi_quotations;
	} else
	{ if ( !(rd->qq = PL_new_term_ref()) )
	    return FALSE;
	}
	//  create positions term
      if ( positions )
      { if ( !(pv = PL_new_term_refs(3)) ||
	     !PL_unify_term(positions,
			    PL_FUNCTOR, FUNCTOR_quasi_quotation_position5,
			    PL_INTPTR, qq->start.charno,
			    PL_VARIABLE,
			    PL_TERM, pv+0, // leave three open slots
			    PL_TERM, pv+1,
			    PL_TERM, pv+2) )
	  return FALSE;
      } else
	pv = 0;
						/* push type */
      
      if ( !(rd->qq_tail = PL_copy_term_ref(rd->qq)) )
	return FALSE;
      }

      NextToken;
      t = ParseTerm(rd, 1200, FailBuff PASS_REGS);
      if (LOCAL_tokptr->Tok != QuasiQuotes_tok) {
	FAIL;
      }
      if ( !( is_quasi_quotation_syntax(t, rd, &at))  )
	FAIL;
						/* Arg 2: the content */
      tn = Yap_MkNewApplTerm( SWIFunctorToFunctor(FUNCTOR_quasi_quotation4), 4 );
      tnp = RepAppl(tn)+1;
      tnp[0] = MkAtomTerm(at);
      if ( !get_quasi_quotation(Yap_InitSlot( ArgOfTerm(2, tn) PASS_REGS), &qq->text, qq->text+strlen((const char *)qq->text), rd) )
	FAIL;

      if ( positions )
      { intptr_t qqend = qq->end.charno;

	// set_range_position(positions, -1, qqend PASS_LD);
	if ( !PL_unify_term( Yap_InitSlot( ArgOfTerm(2, t) PASS_REGS),
			    PL_FUNCTOR, FUNCTOR_minus2,
			      PL_INTPTR, qq->mid.charno+2,	/* end of | token */
			      PL_INTPTR, qqend-2) )     /* end minus "|}" */
	  FAIL;
      }

      tnp[2] = Yap_GetFromSlot(rd->varnames PASS_REGS);	/* Arg 3: the var dictionary */
      /* Arg 4: the result */
      t = ArgOfTerm(4, tn);
      if ( !(to = PL_new_term_ref()) ||
	   !PL_unify_list(rd->qq_tail, to, rd->qq_tail) ||
	   !PL_unify(to, Yap_InitSlot(tn PASS_REGS)) )
	FAIL;
    }
    NextToken;
    break;
  default:
    
    FAIL;
  }

  /* main loop to parse infix and posfix operators starts here */
  while (TRUE) {
    if (LOCAL_tokptr->Tok == Ord(Name_tok)
	&& Yap_HasOp((Atom)(LOCAL_tokptr->TokInfo))) {
      Atom save_opinfo = opinfo = (Atom)(LOCAL_tokptr->TokInfo);
      if (IsInfixOp(save_opinfo, &opprio, &oplprio, &oprprio PASS_REGS) 
	  && opprio <= prio && oplprio >= curprio) {
	/* try parsing as infix operator */
	Volatile int oldprio = curprio;
	TRY3(
	     func = Yap_MkFunctor((Atom) LOCAL_tokptr->TokInfo, 2);
	     if (func == NULL) {
	       LOCAL_ErrorMessage = "Heap Overflow";
	       FAIL;
	     }
	     NextToken;
	     {
	       Term args[2];
	       args[0] = t;
	       args[1] = ParseTerm(rd, oprprio, FailBuff PASS_REGS);
	       t = Yap_MkApplTerm(func, 2, args);
	       /* check for possible overflow against local stack */
	       if (HR > ASP-4096) {
		 LOCAL_ErrorMessage = "Stack Overflow";
		 FAIL;
	       }  
	     },
	       curprio = opprio;
	       opinfo = save_opinfo;
	       continue;
	       ,
	       opinfo = save_opinfo;
	       curprio = oldprio;
	     )
      }
      if (IsPosfixOp(opinfo, &opprio, &oplprio PASS_REGS)
	  && opprio <= prio && oplprio >= curprio) {
	/* parse as posfix operator */
	Functor func = Yap_MkFunctor((Atom) LOCAL_tokptr->TokInfo, 1);
	if (func == NULL) {
	  LOCAL_ErrorMessage = "Heap Overflow";
	  FAIL;
	}
	t = Yap_MkApplTerm(func, 1, &t);
	/* check for possible overflow against local stack */
	if (HR > ASP-4096) {
	  LOCAL_ErrorMessage = "Stack Overflow";
	  FAIL;
	}  
	curprio = opprio;
	NextToken;
	continue;
      }
      break;
    }
    if (LOCAL_tokptr->Tok == Ord(Ponctuation_tok)) {
      if (Unsigned(LOCAL_tokptr->TokInfo) == ',' &&
	  prio >= 1000 && curprio <= 999) {
	Volatile Term args[2];
	NextToken;
	args[0] = t;
	args[1] = ParseTerm(rd, 1000, FailBuff PASS_REGS);
	t = Yap_MkApplTerm(FunctorComma, 2, args);
	/* check for possible overflow against local stack */
	if (HR > ASP-4096) {
	  LOCAL_ErrorMessage = "Stack Overflow";
	  FAIL;
	}  
	curprio = 1000;
	continue;
      } else if (Unsigned(LOCAL_tokptr->TokInfo) == '|' &&
                IsInfixOp(AtomVBar, &opprio, &oplprio, &oprprio PASS_REGS)
                && opprio <= prio && oplprio >= curprio) {
	Volatile Term args[2];
	NextToken;
	args[0] = t;
	args[1] = ParseTerm(rd, oprprio, FailBuff PASS_REGS);
	t = Yap_MkApplTerm(FunctorVBar, 2, args);
	/* check for possible overflow against local stack */
	if (HR > ASP-4096) {
	  LOCAL_ErrorMessage = "Stack Overflow";
	  FAIL;
	}  
	curprio = opprio;
	continue;
      } else if (Unsigned(LOCAL_tokptr->TokInfo) == '(' &&
                IsPosfixOp(AtomEmptyBrackets, &opprio, &oplprio PASS_REGS)
                && opprio <= prio && oplprio >= curprio) {
	t = ParseArgs(rd, AtomEmptyBrackets, ')', FailBuff, t PASS_REGS);
	curprio = opprio;
	continue;
       } else if (Unsigned(LOCAL_tokptr->TokInfo) == '[' &&
                IsPosfixOp(AtomEmptySquareBrackets, &opprio, &oplprio PASS_REGS)
                && opprio <= prio && oplprio >= curprio) {
	t = ParseArgs(rd, AtomEmptySquareBrackets, ']', FailBuff, t PASS_REGS);
	t = MakeAccessor(t, FunctorEmptySquareBrackets PASS_REGS);
	curprio = opprio;
	continue;
      } else if (Unsigned(LOCAL_tokptr->TokInfo) == '{' &&
                IsPosfixOp(AtomEmptyCurlyBrackets, &opprio, &oplprio PASS_REGS)
                && opprio <= prio && oplprio >= curprio) {
	t = ParseArgs(rd, AtomEmptyCurlyBrackets, '}', FailBuff, t PASS_REGS);
	t = MakeAccessor(t, FunctorEmptyCurlyBrackets PASS_REGS);
	curprio = opprio;
	continue;
      }
    }
    if (LOCAL_tokptr->Tok <= Ord(WString_tok))
      FAIL;
    break;
  }
#if DEBUG
  if (GLOBAL_Option['p' - 'a' + 1]) {
    Yap_DebugPutc(LOCAL_c_error_stream,'[');
    Yap_DebugPlWrite(t);
    Yap_DebugPutc(LOCAL_c_error_stream,']');
    Yap_DebugPutc(LOCAL_c_error_stream,'\n');
  }
#endif
  return t;
}


Term
Yap_Parse(read_data *rd)
{
  CACHE_REGS
  Volatile Term t;
  JMPBUFF FailBuff;

  if (!sigsetjmp(FailBuff.JmpBuff, 0)) {
    t = ParseTerm(rd, 1200, &FailBuff PASS_REGS);
    if (LOCAL_tokptr->Tok != Ord(eot_tok))
      return (0L);
    return (t);
  } else
    return (0);
}

//! @}

