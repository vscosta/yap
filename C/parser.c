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
#include "YapHeap.h"
#include "yapio.h"
#include "eval.h"
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

STATIC_PROTO(void GNextToken, ( CACHE_TYPE1 ));
STATIC_PROTO(void checkfor, (Term, JMPBUFF * CACHE_TYPE));
STATIC_PROTO(Term ParseArgs, (Atom, JMPBUFF * CACHE_TYPE));
STATIC_PROTO(Term ParseList, (JMPBUFF * CACHE_TYPE));
STATIC_PROTO(Term ParseTerm, (int, JMPBUFF * CACHE_TYPE));


#define TRY(S,P)                               \
  {	Volatile JMPBUFF *saveenv, newenv;     \
	Volatile TokEntry *saveT=LOCAL_tokptr;   \
        Volatile CELL *saveH=H;                \
        Volatile int savecurprio=curprio;      \
        saveenv=FailBuff;                      \
        if(!sigsetjmp(newenv.JmpBuff, 0)) {      \
                FailBuff = &newenv;            \
		S;                             \
		FailBuff=saveenv;              \
		P;                             \
	  }                                    \
	else { FailBuff=saveenv;               \
		H=saveH;                       \
		curprio = savecurprio;         \
                LOCAL_tokptr=saveT;              \
	}                                      \
   }

#define TRY3(S,P,F)                            \
  {	Volatile JMPBUFF *saveenv, newenv;     \
	Volatile TokEntry *saveT=LOCAL_tokptr;   \
        Volatile CELL *saveH=H;                \
        saveenv=FailBuff;                      \
        if(!sigsetjmp(newenv.JmpBuff, 0)) {      \
                FailBuff = &newenv;            \
		S;                             \
		FailBuff=saveenv;              \
		P;                             \
	  }                                    \
	else {                                 \
                FailBuff=saveenv;              \
                H=saveH;                       \
                LOCAL_tokptr=saveT;              \
                F }                            \
   }


#define FAIL  siglongjmp(FailBuff->JmpBuff,1)

VarEntry *
Yap_LookupVar(char *var)	/* lookup variable in variables table   */
{
  CACHE_REGS
  VarEntry *p;

#ifdef DEBUG
  if (GLOBAL_Option[4])
    fprintf(GLOBAL_stderr,"[LookupVar %s]", var);
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
    strcpy(p->VarRep, var);
  } else {
    /* anon var */
    p = (VarEntry *) Yap_AllocScannerMemory(sizeof(VarEntry) + 2);
    p->VarLeft = LOCAL_AnonVarTable;
    LOCAL_AnonVarTable = p;
    p->VarRight = NULL;    
    p->hv = 0L;
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
      Term o = MkPairTerm(MkPairTerm(Yap_StringToList(p->VarRep), p->VarAdr),
			  VarNames(p->VarRight,
				   VarNames(p->VarLeft,l PASS_REGS) PASS_REGS));
      if (H > ASP-4096) {
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
checkfor(Term c, JMPBUFF *FailBuff USES_REGS)
{
  if (LOCAL_tokptr->Tok != Ord(Ponctuation_tok)
      || LOCAL_tokptr->TokInfo != c)
    FAIL;
  NextToken;
}

static Term
ParseArgs(Atom a, JMPBUFF *FailBuff USES_REGS)
{
  int nargs = 0;
  Term *p, t;
  Functor func;
#ifdef SFUNC
  SFEntry *pe = (SFEntry *) Yap_GetAProp(a, SFProperty);
#endif
  
  NextToken;
  p = (Term *) ParserAuxSp;
  while (1) {
    Term *tp = (Term *)ParserAuxSp;
    if (ParserAuxSp+1 > LOCAL_TrailTop) {
      LOCAL_ErrorMessage = "Trail Overflow";
      FAIL;
    }
    *tp++ = Unsigned(ParseTerm(999, FailBuff PASS_REGS));
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
  if (H > ASP-(nargs+1)) {
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
  if (H > ASP-4096) {
    LOCAL_ErrorMessage = "Stack Overflow";
    return TermNil;
  }  
  /* check for possible overflow against local stack */
  checkfor((Term) ')', FailBuff PASS_REGS);
  return t;
}


static Term
ParseList(JMPBUFF *FailBuff USES_REGS)
{
  Term o;
  CELL *to_store;
  o = AbsPair(H);
 loop:
  to_store = H;
  H+=2;
  to_store[0] = ParseTerm(999, FailBuff PASS_REGS);
  if (LOCAL_tokptr->Tok == Ord(Ponctuation_tok)) {
    if (((int) LOCAL_tokptr->TokInfo) == ',') {
      NextToken;
      if (LOCAL_tokptr->Tok == Ord(Name_tok)
	  && strcmp(RepAtom((Atom)(LOCAL_tokptr->TokInfo))->StrOfAE, "..") == 0) {
	NextToken;
	to_store[1] = ParseTerm(999, FailBuff PASS_REGS);
      } else {
	/* check for possible overflow against local stack */
	if (H > ASP-4096) {
	  to_store[1] = TermNil;
	  LOCAL_ErrorMessage = "Stack Overflow";
	  FAIL;
	} else {
	  to_store[1] = AbsPair(H);
	  goto loop;
	}
      }
    } else if (((int) LOCAL_tokptr->TokInfo) == '|') {
      NextToken;
      to_store[1] = ParseTerm(999, FailBuff PASS_REGS);
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
ParseTerm(int prio, JMPBUFF *FailBuff USES_REGS)
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
	  t = ParseTerm(oprprio, FailBuff PASS_REGS);
	  t = Yap_MkApplTerm(func, 1, &t);
	  /* check for possible overflow against local stack */
	  if (H > ASP-4096) {
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
      t = ParseArgs((Atom) t, FailBuff PASS_REGS);
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
      if (*p == 0)
	t = MkAtomTerm(AtomNil);
      else if (yap_flags[YAP_DOUBLE_QUOTES_FLAG] == STRING_AS_CHARS)
	t = Yap_StringToListOfAtoms(p);
      else if (yap_flags[YAP_DOUBLE_QUOTES_FLAG] == STRING_AS_ATOM) {
	Atom at = Yap_LookupAtom(p);
	if (at == NIL) {
	  LOCAL_ErrorMessage = "Heap Overflow";
	  FAIL;	  
	}
	t = MkAtomTerm(at);
      } else
	t = Yap_StringToList(p);
      NextToken;
    }
  break;

  case WString_tok:	/* build list on the heap */
    {
      Volatile wchar_t *p = (wchar_t *) LOCAL_tokptr->TokInfo;
      if (*p == 0)
	t = MkAtomTerm(AtomNil);
      else if (yap_flags[YAP_DOUBLE_QUOTES_FLAG] == STRING_AS_CHARS)
	t = Yap_WideStringToListOfAtoms(p);
      else if (yap_flags[YAP_DOUBLE_QUOTES_FLAG] == STRING_AS_ATOM)
	t = MkAtomTerm(Yap_LookupWideAtom(p));
      else
	t = Yap_WideStringToList(p);
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
      t = ParseTerm(1200, FailBuff PASS_REGS);
      checkfor((Term) ')', FailBuff PASS_REGS);
      break;
    case '[':
      NextToken;
      t = ParseList(FailBuff PASS_REGS);
      checkfor((Term) ']', FailBuff PASS_REGS);
      break;
    case '{':
      NextToken;
      t = ParseTerm(1200, FailBuff PASS_REGS);
      t = Yap_MkApplTerm(FunctorBraces, 1, &t);
      /* check for possible overflow against local stack */
      if (H > ASP-4096) {
	LOCAL_ErrorMessage = "Stack Overflow";
	FAIL;
      }  
      checkfor((Term) '}', FailBuff PASS_REGS);
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
	       args[1] = ParseTerm(oprprio, FailBuff PASS_REGS);
	       t = Yap_MkApplTerm(func, 2, args);
	       /* check for possible overflow against local stack */
	       if (H > ASP-4096) {
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
	if (H > ASP-4096) {
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
	args[1] = ParseTerm(1000, FailBuff PASS_REGS);
	t = Yap_MkApplTerm(FunctorComma, 2, args);
	/* check for possible overflow against local stack */
	if (H > ASP-4096) {
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
	args[1] = ParseTerm(oprprio, FailBuff PASS_REGS);
	t = Yap_MkApplTerm(FunctorVBar, 2, args);
	/* check for possible overflow against local stack */
	if (H > ASP-4096) {
	  LOCAL_ErrorMessage = "Stack Overflow";
	  FAIL;
	}  
	curprio = opprio;
	continue;
      }
    }
    if (LOCAL_tokptr->Tok <= Ord(WString_tok))
      FAIL;
    break;
  }
#ifdef DEBUG
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
Yap_Parse(void)
{
  CACHE_REGS
  Volatile Term t;
  JMPBUFF FailBuff;

  if (!sigsetjmp(FailBuff.JmpBuff, 0)) {
    t = ParseTerm(1200, &FailBuff PASS_REGS);
    if (LOCAL_tokptr->Tok != Ord(eot_tok))
      return (0L);
    return (t);
  } else
    return (0);
}
