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
#include "Heap.h"
#include "yapio.h"
#if HAVE_STRING_H
#include <string.h>
#endif

#ifdef __STDC__XXX 
#define Volatile volatile
#else
#define Volatile
#endif


STATIC_PROTO(void GNextToken, (void));
STATIC_PROTO(void checkfor, (Term));
STATIC_PROTO(Term ParseArgs, (Atom));
STATIC_PROTO(Term ParseList, (void));
STATIC_PROTO(Term ParseTerm, (int));


/* weak backtraking mechanism based on long_jump */

typedef struct {
	jmp_buf JmpBuff;
} JMPBUFF;

static JMPBUFF FailBuff;


#define TRY(S,P) \
  {	Volatile JMPBUFF saveenv;\
	Volatile TokEntry *saveT=tokptr; \
        Volatile CELL *saveH=H;\
        Volatile int savecurprio=curprio;\
        saveenv=FailBuff;\
        if(!setjmp(FailBuff.JmpBuff)) {\
		S;\
		FailBuff=saveenv;\
		P;\
	  }\
	else { FailBuff=saveenv; \
		H=saveH; \
		curprio = savecurprio; \
                tokptr=saveT; \
	}\
   }\

#define TRY3(S,P,F) \
  {	Volatile JMPBUFF saveenv;\
	Volatile TokEntry *saveT=tokptr; Volatile CELL *saveH=H;\
    saveenv=FailBuff;\
    if(!setjmp(FailBuff.JmpBuff)) {\
		S;\
		FailBuff=saveenv;\
		P;\
	  }\
	else { FailBuff=saveenv; H=saveH; tokptr=saveT; F }\
   }\

#define FAIL  longjmp(FailBuff.JmpBuff,1)

TokEntry *tokptr, *toktide;
VarEntry *VarTable, *AnonVarTable;

VarEntry *
LookupVar(char *var)	/* lookup variable in variables table   */
{
  VarEntry *p;

#ifdef DEBUG
  if (Option[4])
    YP_fprintf(YP_stderr,"[LookupVar %s]", var);
#endif
  if (var[0] != '_' || var[1] != '\0') {
    VarEntry **op = &VarTable;
    unsigned char *vp = (unsigned char *)var;
    CELL hv;

    p = VarTable;
    HashFunction(vp, hv);
    while (p != NULL) {
      CELL hpv = p->hv;
      if (hv == hpv) {
	Int scmp;
	if ((scmp = strcmp(var, p->VarRep)) == 0) {
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
    p = (VarEntry *) AllocScannerMemory(strlen(var) + sizeof(VarEntry));
    *op = p;
    p->VarLeft = p->VarRight = NULL;
    p->hv = hv;
    strcpy(p->VarRep, var);
  } else {
    /* anon var */
    p = (VarEntry *) AllocScannerMemory(sizeof(VarEntry) + 2);
    p->VarLeft = AnonVarTable;
    AnonVarTable = p;
    p->VarRight = NULL;    
    p->hv = 0L;
    p->VarRep[0] = '_';
    p->VarRep[1] = '\0';
  }
  p->VarAdr = TermNil;
  return (p);
}

Term
VarNames(VarEntry *p,Term l)
{
  if (p != NULL) {
    if (strcmp(p->VarRep, "_") != 0) {
      Term o = MkPairTerm(MkPairTerm(StringToList(p->VarRep), p->VarAdr),
			  VarNames(p->VarRight,
				   VarNames(p->VarLeft,l)));
      if (H > ASP-4096) {
	longjmp(IOBotch,1);
      }  
      return(o);
    } else {
      return(VarNames(p->VarRight,VarNames(p->VarLeft,l)));
    }
  } else {
    return (l);
  }
}

int
IsPrefixOp(Prop opinfo,int  *pptr, int *rpptr)
{
  int p;

  READ_LOCK(RepOpProp(opinfo)->OpRWLock);
  if ((p = RepOpProp(opinfo)->Prefix) != 0) {
    READ_UNLOCK(RepOpProp(opinfo)->OpRWLock);
    *pptr = *rpptr = p & MaskPrio;
    if (p & DcrrpFlag)
      --* rpptr;
    return (TRUE);
  } else {
    READ_UNLOCK(RepOpProp(opinfo)->OpRWLock);
    return (FALSE);
  }
}

int
IsInfixOp(Prop opinfo, int *pptr, int *lpptr, int *rpptr)
{
  int p;

  READ_LOCK(RepOpProp(opinfo)->OpRWLock);
  if ((p = RepOpProp(opinfo)->Infix) != 0) {
    READ_UNLOCK(RepOpProp(opinfo)->OpRWLock);
    *pptr = *rpptr = *lpptr = p & MaskPrio;
    if (p & DcrrpFlag)
      --* rpptr;
    if (p & DcrlpFlag)
      --* lpptr;
    return (TRUE);
  } else {
    READ_UNLOCK(RepOpProp(opinfo)->OpRWLock);
    return (FALSE);
  }
}

int
IsPosfixOp(Prop opinfo, int *pptr, int *lpptr)
{
  int p;
  READ_LOCK(RepOpProp(opinfo)->OpRWLock);
  if ((p = RepOpProp(opinfo)->Posfix) != 0) {
    READ_UNLOCK(RepOpProp(opinfo)->OpRWLock);
    *pptr = *lpptr = p & MaskPrio;
    if (p & DcrlpFlag)
      --* lpptr;
    return (TRUE);
  } else {
    READ_UNLOCK(RepOpProp(opinfo)->OpRWLock);
    return (FALSE);
  }
}

inline static void
GNextToken(void)
{
	if (tokptr->Tok == Ord(eot_tok))
		return;
#ifdef EMACS
	if ((tokptr = tokptr->TokNext)->TokPos > toktide->TokPos)
		toktide = tokptr;
#else
	if (tokptr == toktide)
	  toktide = tokptr = tokptr->TokNext;
	else
	  tokptr = tokptr->TokNext;
#endif
}

inline static void
checkfor(Term c)
{
  if (tokptr->Tok != Ord(Ponctuation_tok)
      || tokptr->TokInfo != c)
    FAIL;
  NextToken;
}

static Term
ParseArgs(Atom a)
{
  int nargs = 0;
  Term *p, t;
#ifdef SFUNC
  SFEntry *pe = (SFEntry *) GetAProp(a, SFProperty);
#endif
  
  NextToken;
  p = (Term *) ParserAuxSp;
  while (1) {
    Term *tp = (Term *)ParserAuxSp;
    *tp++ = Unsigned(ParseTerm(999));
    ParserAuxSp = (tr_fr_ptr)tp;
    ++nargs;
    if (tokptr->Tok != Ord(Ponctuation_tok))
      break;
    if (((int) tokptr->TokInfo) != ',')
      break;
    NextToken;
  }
  ParserAuxSp = (tr_fr_ptr)p;
  /*
   * Needed because the arguments for the functor are placed in reverse
   * order 
   */
#ifdef SFUNC
  if (pe)
    t = MkSFTerm(MkFunctor(a, SFArity), nargs, p, pe->NilValue);
  else
    t = MkApplTerm(MkFunctor(a, nargs), nargs, p);
#else
  t = MkApplTerm(MkFunctor(a, nargs), nargs, p);
#endif
  /* check for possible overflow against local stack */
  if (H > ASP-4096) {
    ErrorMessage = "Stack Overflow";
    FAIL;
  }  
  checkfor((Term) ')');
  return (t);
}


static Term
ParseList(void)
{
  Term t, s, o;
  CELL *to_store;
  o = AbsPair(H);
 loop:
  to_store = H;
  H+=2;
  to_store[0] = ParseTerm(999);
  if (tokptr->Tok == Ord(Ponctuation_tok)) {
    if (((int) tokptr->TokInfo) == ',') {
      NextToken;
      if (tokptr->Tok == Ord(Name_tok)
	  && strcmp(RepAtom((Atom)(tokptr->TokInfo))->StrOfAE, "..") == 0) {
	NextToken;
	to_store[1] = ParseTerm(999);
      } else {
	/* check for possible overflow against local stack */
	if (H > ASP-4096) {
	  to_store[1] = TermNil;
	  ErrorMessage = "Stack Overflow";
	  FAIL;
	} else {
	  to_store[1] = AbsPair(H);
	  goto loop;
	}
      }
    } else if (((int) tokptr->TokInfo) == '|') {
      NextToken;
      to_store[1] = ParseTerm(999);
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
ParseTerm(int prio)
{
  /* parse term with priority prio */
  Volatile Prop opinfo;
  Volatile Term t;
  Volatile Functor func;
  Volatile VarEntry *varinfo;
  Volatile int curprio = 0, opprio, oplprio, oprprio;

  switch (tokptr->Tok) {
  case Name_tok:
    t = tokptr->TokInfo;
    NextToken;
    if ((tokptr->Tok != Ord(Ponctuation_tok)
	 || Unsigned(tokptr->TokInfo) != 'l')
	&& (opinfo = GetAProp((Atom) t, OpProperty))
	&& IsPrefixOp(opinfo, &opprio, &oprprio)
	) {
      /* special rules apply for +1, -2.3, etc... */
      if (tokptr->Tok == Number_tok) {
	if ((Atom)t == AtomMinus) {
	  t = tokptr->TokInfo;
	  if (IsIntTerm(t))
	    t = MkIntTerm(-IntOfTerm(t));
	  else if (IsFloatTerm(t))
	    t = MkFloatTerm(-FloatOfTerm(t));
#ifdef USE_GMP
	  else if (IsBigIntTerm(t)) {
	    MP_INT *new = PreAllocBigNum();

	    mpz_neg(new, BigIntOfTerm(t));
	    t = MkBigIntTerm(new);
	  }
#endif
	  else
	    t = MkLongIntTerm(-LongIntOfTerm(t));
	  NextToken;
	  break;
	} else if ((Atom)t == AtomPlus) {
	  t = tokptr->TokInfo;
	  NextToken;
	  break;
	}
      } else if (tokptr->Tok == Name_tok) {
	Atom at = (Atom)tokptr->TokInfo;
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
	  func = MkFunctor((Atom) t, 1);
	  t = ParseTerm(oprprio);
	  t = MkApplTerm(func, 1, &t);
	  /* check for possible overflow against local stack */
	  if (H > ASP-4096) {
	    ErrorMessage = "Stack Overflow";
	    FAIL;
	  }  
	  curprio = opprio;
	  ,
	  break;
	  )
	}
    }
    if (tokptr->Tok == Ord(Ponctuation_tok)
	&& Unsigned(tokptr->TokInfo) == 'l')
      t = ParseArgs((Atom) t);
    else
      t = MkAtomTerm((Atom)t);
    break;

  case Number_tok:
    t = tokptr->TokInfo;
    NextToken;
    break;

  case String_tok:	/* build list on the heap */
    {
      Volatile char *p = (char *) tokptr->TokInfo;
      if (*p == 0)
	t = MkAtomTerm(AtomNil);
      else if (yap_flags[YAP_DOUBLE_QUOTES_FLAG] == STRING_AS_CHARS)
	t = StringToListOfAtoms(p);
      else if (yap_flags[YAP_DOUBLE_QUOTES_FLAG] == STRING_AS_ATOM)
	t = MkAtomTerm(LookupAtom(p));
      else
	t = StringToList(p);
      NextToken;
    }
  break;

  case Var_tok:
    varinfo = (VarEntry *) (tokptr->TokInfo);
    if ((t = varinfo->VarAdr) == TermNil) {
      t = varinfo->VarAdr = MkVarTerm();
    }
    NextToken;
    break;

  case Ponctuation_tok:
    switch ((int) tokptr->TokInfo) {
    case '(':
    case 'l':	/* non solo ( */
      NextToken;
      t = ParseTerm(1200);
      checkfor((Term) ')');
      break;
    case '[':
      NextToken;
      t = ParseList();
      checkfor((Term) ']');
      break;
    case '{':
      NextToken;
      if (tokptr->Tok == Ord(Ponctuation_tok) &&
	  Unsigned(tokptr->TokInfo) == '}') {
	t = MkAtomTerm(NameOfFunctor(FunctorBraces));
	NextToken;
      } else {
	t = ParseTerm(1200);
	t = MkApplTerm(FunctorBraces, 1, &t);
	/* check for possible overflow against local stack */
	if (H > ASP-4096) {
	  ErrorMessage = "Stack Overflow";
	  FAIL;
	}  
	checkfor((Term) '}');
      }
      break;
    default:
      FAIL;
    }
    break;
    
  default:
    
    FAIL;
  }

  /* main loop to parse infix and posfix operators starts here */
  while (TRUE) {
    if (tokptr->Tok == Ord(Name_tok)
	&& (opinfo = GetAProp((Atom)(tokptr->TokInfo), OpProperty))) {
      Prop save_opinfo = opinfo;
      if (IsInfixOp(opinfo, &opprio, &oplprio, &oprprio)
	  && opprio <= prio && oplprio >= curprio) {
	/* try parsing as infix operator */
	Volatile int oldprio = curprio;
	TRY3(
	     func = MkFunctor((Atom) tokptr->TokInfo, 2);
	     NextToken;
	     {
	       Term args[2];
	       args[0] = t;
	       args[1] = ParseTerm(oprprio);
	       t = MkApplTerm(func, 2, args);
	       /* check for possible overflow against local stack */
	       if (H > ASP-4096) {
		 ErrorMessage = "Stack Overflow";
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
      if (IsPosfixOp(opinfo, &opprio, &oplprio)
	  && opprio <= prio && oplprio >= curprio) {
	/* parse as posfix operator */
	t = MkApplTerm(MkFunctor((Atom) tokptr->TokInfo, 1), 1, &t);
	/* check for possible overflow against local stack */
	if (H > ASP-4096) {
	  ErrorMessage = "Stack Overflow";
	  FAIL;
	}  
	curprio = opprio;
	NextToken;
	continue;
      }
      break;
    }
    if (tokptr->Tok == Ord(Ponctuation_tok)) {
      if (Unsigned(tokptr->TokInfo) == ',' &&
	  prio >= 1000 && curprio <= 999) {
	Volatile Term args[2];
	NextToken;
	args[0] = t;
	args[1] = ParseTerm(1000);
	t = MkApplTerm(MkFunctor(AtomComma, 2), 2, args);
	/* check for possible overflow against local stack */
	if (H > ASP-4096) {
	  ErrorMessage = "Stack Overflow";
	  FAIL;
	}  
	curprio = 1000;
	continue;
      } else if (Unsigned(tokptr->TokInfo) == '|' && prio >= 1100 &&
		 curprio <= 1099) {
	Volatile Term args[2];
	NextToken;
	args[0] = t;
	args[1] = ParseTerm(1100);
	t = MkApplTerm(FunctorVBar, 2, args);
	/* check for possible overflow against local stack */
	if (H > ASP-4096) {
	  ErrorMessage = "Stack Overflow";
	  FAIL;
	}  
	curprio = 1100;
	continue;
      }
    }
    if (tokptr->Tok <= Ord(String_tok))
      FAIL;
    break;
  }
  return (t);
}


Term
Parse(void)
{
  Volatile Term t;
  if (!setjmp(FailBuff.JmpBuff)) {
    t = ParseTerm(1200);
    if (tokptr->Tok != Ord(eot_tok))
      return (0L);
    return (t);
  } else
    return (0);
}
