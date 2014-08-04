/*************************************************************************
*									 *
*	 Yap Prolog 							 *
*									 *
*	Yap Prolog Was Developed At Nccup - Universidade Do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa And Universidade Do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		errors.c						 *
* Last Rev:								 *
* Mods:									 *
* Comments:	Yap'S error handlers					 *
*									 *
*************************************************************************/

#include "absmi.h"
#include "yapio.h"
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#include "Foreign.h"

int Yap_HandleError( const char *s, ... ) {
  CACHE_REGS
    yap_error_number err = LOCAL_Error_TYPE;
  char *serr;

  LOCAL_Error_TYPE = YAP_NO_ERROR;
  if (LOCAL_ErrorMessage) {
    serr = LOCAL_ErrorMessage;
  } else {
    serr = (char *)s;
  }
  switch (err) {
  case OUT_OF_STACK_ERROR:
    if (!Yap_gc(2, ENV, gc_P(P,CP))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, serr);
      return(FALSE);
    }
    return TRUE;
  case OUT_OF_AUXSPACE_ERROR:
    if (LOCAL_MAX_SIZE < (char *)AuxSp-AuxBase) {
      LOCAL_MAX_SIZE += 1024;
    }
    if (!Yap_ExpandPreAllocCodeSpace(0,NULL, TRUE)) {
      /* crash in flames */
      Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, serr);
      return FALSE;
    }
    return TRUE;
  case OUT_OF_HEAP_ERROR:
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, ARG2, serr);
      return FALSE;
    }
  default:
    Yap_Error(err, LOCAL_Error_Term, serr);
    return(FALSE);
  }
}
  
int Yap_SWIHandleError( const char *s, ... )
{
  CACHE_REGS
    yap_error_number err = LOCAL_Error_TYPE;
  char *serr;

  LOCAL_Error_TYPE = YAP_NO_ERROR;
  if (LOCAL_ErrorMessage) {
    serr = LOCAL_ErrorMessage;
  } else {
    serr = (char *)s;
  }
  switch (err) {
  case OUT_OF_STACK_ERROR:
    if (!Yap_gc(2, ENV, gc_P(P,CP))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, serr);
      return(FALSE);
    }
    return TRUE;
  case OUT_OF_AUXSPACE_ERROR:
    if (LOCAL_MAX_SIZE < (char *)AuxSp-AuxBase) {
      LOCAL_MAX_SIZE += 1024;
    }
    if (!Yap_ExpandPreAllocCodeSpace(0,NULL, TRUE)) {
      /* crash in flames */
      Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, serr);
      return FALSE;
    }
    return TRUE;
  case OUT_OF_HEAP_ERROR:
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, ARG2, serr);
      return FALSE;
    }
  default:
    Yap_Error(err, LOCAL_Error_Term, serr);
    return(FALSE);
  }
}

void
Yap_RestartYap ( int flag )
{
  CACHE_REGS
#if PUSH_REGS
      restore_absmi_regs(&Yap_standard_regs);
#endif
      siglongjmp(LOCAL_RestartEnv,1);      
}

void DumpActiveGoals( CACHE_TYPE1 );
static int hidden(Atom);
static int legal_env(CELL * CACHE_TYPE);
static void detect_bug_location(yamop *,find_pred_type,char *, int);

#define ONHEAP(ptr) (CellPtr(ptr) >= CellPtr(Yap_HeapBase)  && CellPtr(ptr) < CellPtr(HeapTop))

#define ONLOCAL(ptr) (CellPtr(ptr) > CellPtr(HR)  && CellPtr(ptr) < CellPtr(LOCAL_LocalBase))

static int
hidden (Atom at)
{
  AtomEntry *chain;
  
  READ_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  while (!EndOfPAEntr (chain) && AbsAtom (chain) != at)
    chain = RepAtom(chain->NextOfAE);
  READ_UNLOCK(INVISIBLECHAIN.AERWLock);
  if (EndOfPAEntr (chain))
    return (FALSE);
  return (TRUE);
}

static int
legal_env (CELL *ep USES_REGS)
{
  CELL cp, ps;
  PredEntry *pe;
  if (!ONLOCAL (ep) || Unsigned (ep) & 3)
    return (FALSE);
  cp = ep[E_CP];
  if (!ONHEAP (cp))
    return (FALSE);
  ps = *((CELL *) (Addr (cp) - CellSize));
  pe = (PredEntry *) (ps - sizeof (OPREG) - sizeof (Prop));
  PELOCK(70,pe);
  if (!ONHEAP (pe) || Unsigned (pe) & 3 || pe->KindOfPE & 0xff00) {
    UNLOCK(pe->PELock);
    return (FALSE);
  }
  UNLOCK(pe->PELock);
  return (TRUE);
}

static int 
YapPutc(int sno, wchar_t ch)
{
  return (putc(ch, stderr));
}

static void
YapPlWrite(Term t)
{
  Yap_plwrite(t, NULL, 15, 0, 1200);
}


void
DumpActiveGoals ( USES_REGS1 )
{
  /* try to dump active goals */
  CELL *ep = YENV;		/* and current environment		  */
  choiceptr b_ptr = B;
  CELL cp;
  PredEntry *pe;
  int first = 1;

  if (legal_env (YENV PASS_REGS) && YENV < ENV)
    ep = YENV;
  else if (legal_env (ENV PASS_REGS))
    ep = ENV;
  while (TRUE)
    {
      if (!ONLOCAL (ep) || (Unsigned (ep) & (sizeof(CELL)-1)))
	break;
      cp = ep[E_CP];
      if (!ONHEAP (cp) || (Unsigned (cp) & (sizeof(CELL)-1)))
	break;
      pe = EnvPreg((yamop *)cp);
      if (!ONHEAP (pe) || Unsigned (pe) & (sizeof(CELL)-1))
	break;
      PELOCK(71,pe);
      if (pe->KindOfPE & 0xff00) {
	UNLOCK(pe->PELock);
	break;
      }
      if (pe->PredFlags & (CompiledPredFlag | DynamicPredFlag))
	{
	  Functor f;
	  Term mod = TermProlog;

	  UNLOCK(pe->PELock);
	  f = pe->FunctorOfPred;
	  if (pe->KindOfPE && hidden (NameOfFunctor (f)))
	    goto next;
	  if (first++ == 1)
	    fprintf(stderr,"Active ancestors:\n");
	  if (pe->ModuleOfPred) mod = pe->ModuleOfPred;
	  YapPlWrite (mod);
	  YapPutc (LOCAL_c_error_stream,':');
	  if (pe->ArityOfPE == 0) {
	    YapPlWrite (MkAtomTerm ((Atom)f));
	  } else {
	    YapPlWrite (MkAtomTerm (NameOfFunctor (f)));
	    YapPutc (LOCAL_c_error_stream,'/');
	    YapPlWrite (MkIntTerm (ArityOfFunctor (f)));
	  }
	  YapPutc (LOCAL_c_error_stream,'\n');
	} else {
	  UNLOCK(pe->PELock);
	}
    next:
      ep = (CELL *) ep[E_E];
    }
  first = 1;
  fprintf(stderr,"Active Choice-Points:\n");
  while (TRUE)
    {
      PredEntry *pe;
      
      if (!ONLOCAL (b_ptr) || b_ptr->cp_b == NULL)
	break;
      pe = Yap_PredForChoicePt(b_ptr);
      if (!pe)
	break;
      PELOCK(72,pe);
      {
	Functor f;
	Term mod = PROLOG_MODULE;

	f = pe->FunctorOfPred;
	if (pe->ModuleOfPred)
	  mod = pe->ModuleOfPred;
	else mod = TermProlog;
	if (mod != TermProlog && 
	    mod != MkAtomTerm(AtomUser) ) {
	  YapPlWrite (mod);
	  YapPutc (LOCAL_c_error_stream,':');
	}
	if (pe->ArityOfPE == 0) {
	  YapPlWrite (MkAtomTerm ((Atom)f));
	} else {
	  Int i = 0, arity = pe->ArityOfPE;
	  Term *args = &(b_ptr->cp_a1);
	  YapPlWrite (MkAtomTerm (NameOfFunctor (f)));
	  YapPutc (LOCAL_c_error_stream,'(');
	  for (i= 0; i < arity; i++) {
	    if (i > 0) YapPutc (LOCAL_c_error_stream,',');
	    YapPlWrite(args[i]);
	  }
	  YapPutc (LOCAL_c_error_stream,')');
	}
	YapPutc (LOCAL_c_error_stream,'\n');
      }
      UNLOCK(pe->PELock);
      b_ptr = b_ptr->cp_b;
    }
}


static void
detect_bug_location(yamop *yap_pc, find_pred_type where_from, char *tp, int psize)
{
  Atom pred_name;
  UInt pred_arity;
  Term pred_module;
  Int cl;

  tp[0] = '\0';
  if ((cl = Yap_PredForCode(yap_pc, where_from, &pred_name, &pred_arity, &pred_module))
      == 0) {
    /* system predicate */
#if   HAVE_SNPRINTF
    snprintf(tp, psize, "%s",
	     "meta-call");
#else
    sprintf(tp, "%s",
	    "meta-call");
#endif	
  } else if (pred_module == 0) {
    /* don't give info on system predicates */
#if   HAVE_SNPRINTF
    snprintf(tp, psize, "prolog:%s/%lu",
	     RepAtom(pred_name)->StrOfAE, (unsigned long int)pred_arity);
#else
    sprintf(tp, "in prolog:%s/%lu",
	    RepAtom(pred_name)->StrOfAE, (unsigned long int)pred_arity);
#endif
  } else if (cl < 0) {
#if   HAVE_SNPRINTF
    snprintf(tp, psize, "%s:%s/%lu",
	     RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, (unsigned long int)pred_arity);
#else
    sprintf(tp, "%s:%s/%lu",
	    RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, (unsigned long int)pred_arity);
#endif
  } else {
#if   HAVE_SNPRINTF
    snprintf(tp, psize, "%s:%s/%lu at clause %lu ",
	     RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, (unsigned long int)pred_arity, (unsigned long int)cl);
#else
    sprintf(tp, "%s:%s/%lu at clause %lu",
	    RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, (unsigned long int)pred_arity, (unsigned long int)cl);
#endif
  }
}

static int
handled_exception( USES_REGS1 )
{
  yamop *pos = NEXTOP(PredDollarCatch->cs.p_code.TrueCodeOfPred,l);
  int found_handler = FALSE;
  choiceptr gc_b;

  gc_b = B;
  while (gc_b) {
    yamop *ap = gc_b->cp_ap;
    if (ap == NOCODE) {
      /* C-code: let they deal with that */
      return FALSE;
    } else if (ap == pos) {
      if (found_handler)
	return TRUE; /* we have two handlers */
      found_handler = TRUE;
    }
    gc_b = gc_b->cp_b;
  }
  /* handled by Top c-code? */
  return !found_handler;
}


static void
dump_stack( USES_REGS1 )
{
  choiceptr b_ptr = B;
  CELL *env_ptr = ENV;
  char tp[256];
  yamop *ipc = CP;
  int max_count = 200;
  
  /* check if handled */
  if (handled_exception( PASS_REGS1 ))
    return;
#if DEBUG
  fprintf(stderr,"%% YAP regs: P=%p, CP=%p, ASP=%p, H=%p, TR=%p, HeapTop=%p\n",P,CP,ASP,HR,TR,HeapTop);
  fprintf(stderr,"%% YAP mode: %ux\n",(unsigned int)LOCAL_PrologMode);
  if (LOCAL_ErrorMessage)
    fprintf(stderr,"%% LOCAL_ErrorMessage: %s\n",LOCAL_ErrorMessage);
#endif
  if (HR > ASP || HR > LCL0) {
    fprintf(stderr,"%% YAP ERROR: Global Collided against Local (%p--%p)\n",HR,ASP);
  } else   if (HeapTop > (ADDR)LOCAL_GlobalBase) {
    fprintf(stderr,"%% YAP ERROR: Code Space Collided against Global (%p--%p)\n", HeapTop, LOCAL_GlobalBase);
  } else {
#if !USE_SYSTEM_MALLOC
    fprintf (stderr,"%ldKB of Code Space (%p--%p)\n",(long int)((CELL)HeapTop-(CELL)Yap_HeapBase)/1024,Yap_HeapBase,HeapTop); 
#if USE_DL_MALLOC 
    if (Yap_NOfMemoryHoles) {
      UInt i;

      for (i=0; i < Yap_NOfMemoryHoles; i++)
	fprintf(stderr,"  Current hole: %p--%p\n", 
		Yap_MemoryHoles[i].start,
		Yap_MemoryHoles[i].end);
    }
#endif
#endif
    detect_bug_location(P, FIND_PRED_FROM_ANYWHERE, (char *)HR, 256);
    fprintf (stderr,"%%\n%% PC: %s\n",(char *)HR); 
    detect_bug_location(CP, FIND_PRED_FROM_ANYWHERE, (char *)HR, 256);
    fprintf (stderr,"%%   Continuation: %s\n",(char *)HR); 
    fprintf (stderr,"%%    %luKB of Global Stack (%p--%p)\n",(unsigned long int)(sizeof(CELL)*(HR-H0))/1024,H0,HR); 
    fprintf (stderr,"%%    %luKB of Local Stack (%p--%p)\n",(unsigned long int)(sizeof(CELL)*(LCL0-ASP))/1024,ASP,LCL0); 
    fprintf (stderr,"%%    %luKB of Trail (%p--%p)\n",(unsigned long int)((ADDR)TR-LOCAL_TrailBase)/1024,LOCAL_TrailBase,TR); 
    fprintf (stderr,"%%    Performed %ld garbage collections\n", (unsigned long int)LOCAL_GcCalls);
#if LOW_LEVEL_TRACER
    {
      extern long long vsc_count;

      if (vsc_count) {
#if _WIN32
	fprintf(stderr,"Trace Counter at %I64d\n",vsc_count);
#else
	fprintf(stderr,"Trace Counter at %lld\n",vsc_count);
#endif
      }
    }
#endif
    fprintf (stderr,"%% All Active Calls and\n"); 
    fprintf (stderr,"%%         Goals With Alternatives Open  (Global In Use--Local In Use)\n%%\n");
    while (b_ptr != NULL) {
      while (env_ptr && env_ptr <= (CELL *)b_ptr) {
	detect_bug_location(ipc, FIND_PRED_FROM_ENV, tp, 256);
	if (env_ptr == (CELL *)b_ptr &&
	    (choiceptr)env_ptr[E_CB] > b_ptr) {
	  b_ptr = b_ptr->cp_b;
	  fprintf(stderr,"%%  %s\n", tp);
	} else {
	  fprintf(stderr,"%%  %s\n", tp);
	}
	if (!max_count--) {
	  fprintf(stderr,"%%  .....\n");
	  return;
	}
	ipc = (yamop *)(env_ptr[E_CP]);
	env_ptr = (CELL *)(env_ptr[E_E]);
      }
      if (b_ptr) {
	if (!max_count--) {
	  fprintf(stderr,"%%  .....\n");
	  return;
	}
	if (b_ptr->cp_ap && /* tabling */
	    b_ptr->cp_ap->opc != Yap_opcode(_or_else) &&
	    b_ptr->cp_ap->opc != Yap_opcode(_or_last) &&
	    b_ptr->cp_ap->opc != Yap_opcode(_Nstop)) {
	  /* we can safely ignore ; because there is always an upper env */
	  detect_bug_location(b_ptr->cp_ap, FIND_PRED_FROM_CP, tp, 256);
	  fprintf(stderr,"%%         %s (%luKB--%luKB)\n", tp,
		  (unsigned long int)((b_ptr->cp_h-H0)*sizeof(CELL)/1024), 
		  (unsigned long int)((ADDR)LCL0-(ADDR)b_ptr)/1024);
	}
	b_ptr = b_ptr->cp_b;
      }
    }
  }
}


static void
error_exit_yap (int value)
{
  CACHE_REGS
  if (!(LOCAL_PrologMode & BootMode)) {
    dump_stack( PASS_REGS1 );
#if DEBUG
#endif
  }
  fprintf(stderr, "\n   Exiting ....\n");
  Yap_exit(value);
}


#ifdef DEBUG

void
Yap_bug_location(yamop *pc)
{
  CACHE_REGS
  detect_bug_location(pc, FIND_PRED_FROM_ANYWHERE, (char *)HR, 256);
  fprintf(stderr,"%s\n",(char *)HR);
  dump_stack( PASS_REGS1 );
}

#endif

/* This needs to be a static because I can't trust the stack (WIN32), and
   I can't trust the Yap stacks  (error) */
#define YAP_BUF_SIZE 512

static char tmpbuf[YAP_BUF_SIZE];

yamop *
Yap_NilError(yap_error_number type, char *format,...)
{
  va_list ap;
  yamop *res;

  va_start (ap, format);
  /* now build the error string */
  res = Yap_Error(type, TermNil, format, ap);
  va_end (ap);

  return res;
}

yamop *
Yap_Error(yap_error_number type, Term where, char *format,...)
{
  CACHE_REGS
  va_list ap;
  CELL nt[3];
  Functor fun;
  int serious;
  int psize = YAP_BUF_SIZE;

  LOCAL_Error_TYPE = YAP_NO_ERROR;
  if (where == 0L)
    where = TermNil;
#if DEBUG_STRICT
  if (Yap_heap_regs && !(LOCAL_PrologMode & BootMode)) 
    fprintf(stderr,"***** Processing Error %d (%lx,%x) %s***\n", type, (unsigned long int)LOCAL_ActiveSignals,LOCAL_PrologMode,format);
  else
    fprintf(stderr,"***** Processing Error %d (%x) %s***\n", type,LOCAL_PrologMode,format);
#endif
  if (type == INTERRUPT_ERROR) {
    fprintf(stderr,"%% YAP exiting: cannot handle signal %d\n",
	    (int)IntOfTerm(where));
    Yap_exit(1);
  }
  /* disallow recursive error handling */ 
  if (LOCAL_PrologMode & InErrorMode) {
    /* error within error */
    va_start (ap, format);
    /* now build the error string */
    if (format != NULL) {
#if   HAVE_VSNPRINTF
      (void) vsnprintf(tmpbuf, YAP_BUF_SIZE, format, ap);
#else
      (void) vsprintf(tmpbuf, format, ap);
#endif
    } else {
      tmpbuf[0] = '\0';
    }
    va_end (ap);
    fprintf(stderr,"%% ERROR WITHIN ERROR %d: %s\n", LOCAL_CurrentError, tmpbuf);
    exit(1);
  }
  /* must do this here */
  if (type == FATAL_ERROR
      || type == INTERNAL_ERROR
#if USE_SYSTEM_MALLOC 
      || !Yap_heap_regs
#else
      || !Yap_HeapBase
#endif
      ) {
    if (format != NULL) {
      va_start (ap, format);
      /* now build the error string */
#if   HAVE_VSNPRINTF
      (void) vsnprintf(tmpbuf, YAP_BUF_SIZE, format, ap);
#else
      (void) vsprintf(tmpbuf, format, ap);
#endif
      va_end (ap);
    }  else {
      tmpbuf[0] = '\0';
    }
    if (LOCAL_PrologMode == UserCCallMode) {
      fprintf(stderr,"%%\n%%\n");
      fprintf(stderr,"%% YAP OOOPS in USER C-CODE: %s.\n",tmpbuf);
      fprintf(stderr,"%%\n%%\n");
    } else {
      fprintf(stderr,"%%\n%%\n");
      fprintf(stderr,"%% YAP OOOPS: %s.\n",tmpbuf);
      fprintf(stderr,"%%\n%%\n");
    }
    detect_bug_location(P, FIND_PRED_FROM_ANYWHERE, (char *)HR, 256);
    fprintf (stderr,"%%\n%% PC: %s\n",(char *)HR); 
    detect_bug_location(CP, FIND_PRED_FROM_ANYWHERE, (char *)HR, 256);
    fprintf (stderr,"%%   Continuation: %s\n",(char *)HR); 
    DumpActiveGoals( PASS_REGS1 );
    error_exit_yap (1);
  }
  if (P == (yamop *)(FAILCODE))
   return P;
  /* PURE_ABORT may not have set where correctly, BootMode may not have the data terms ready */
  if (type == PURE_ABORT || LOCAL_PrologMode & BootMode) {
    where = TermNil;
    LOCAL_PrologMode &= ~AbortMode;
    LOCAL_CurrentError = type;
    LOCAL_PrologMode |= InErrorMode;
    /* make sure failure will be seen at next port */
    LOCK(LOCAL_SignalLock);
    if (LOCAL_PrologMode & AsyncIntMode)
      Yap_signal(YAP_FAIL_SIGNAL);
    UNLOCK(LOCAL_SignalLock);
    P = FAILCODE;
  } else {
    if (IsVarTerm(where)) {
      /* we must be careful someone gave us a copy to a local variable */
      Term t = MkVarTerm();
      Yap_unify(t, where);
      where = Deref(where);
    }
    /* Exit Abort Mode, if we were there */
    LOCAL_PrologMode &= ~AbortMode;
    LOCAL_CurrentError = type;
    LOCAL_PrologMode |= InErrorMode;
    if (!(where = Yap_CopyTerm(where))) {
      where = TermNil;
    }
  }
  va_start (ap, format);
  /* now build the error string */
  if (format != NULL)
    {
#if   HAVE_VSNPRINTF
      (void) vsnprintf(tmpbuf, YAP_BUF_SIZE, format, ap);
#else
      (void) vsprintf(tmpbuf, format, ap);
#endif
    }
  else
    tmpbuf[0] = '\0';
  va_end (ap);
  if (LOCAL_PrologMode & BootMode) {
    /* crash in flames! */
    fprintf(stderr,"%% YAP Fatal Error: %s exiting....\n",tmpbuf);
    error_exit_yap (1);
  }
#ifdef DEBUGX
  DumpActiveGoals( USES_REGS1 );
#endif /* DEBUG */
  switch (type) {
  case INTERNAL_ERROR:
    {
      fprintf(stderr,"%% Internal YAP Error: %s exiting....\n",tmpbuf);
      serious = TRUE;
      if (LOCAL_PrologMode & BootMode) {
	fprintf(stderr,"%% YAP crashed while booting %s\n",tmpbuf);
      } else {
	detect_bug_location(P, FIND_PRED_FROM_ANYWHERE, tmpbuf, YAP_BUF_SIZE);
	fprintf(stderr,"%% Bug found while executing %s\n",tmpbuf);
      }
      error_exit_yap (1);
    }
  case FATAL_ERROR:
    {
      fprintf(stderr,"%% Fatal YAP Error: %s exiting....\n",tmpbuf);
      error_exit_yap (1);
    }
  case INTERRUPT_ERROR:
    {
      error_exit_yap (1);
    }
  case PURE_ABORT:
    nt[0] = MkAtomTerm(AtomDAbort);
    fun = FunctorVar;
    serious = TRUE;
    break;
  case CALL_COUNTER_UNDERFLOW:
    /* Do a long jump */
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    Yap_JumpToEnv(MkAtomTerm(AtomCallCounter));
    P = (yamop *)FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return(P);
  case PRED_ENTRY_COUNTER_UNDERFLOW:
    /* Do a long jump */
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    Yap_JumpToEnv(MkAtomTerm(AtomCallAndRetryCounter));
    P = (yamop *)FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return(P);
  case RETRY_COUNTER_UNDERFLOW:
    /* Do a long jump */
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    Yap_JumpToEnv(MkAtomTerm(AtomRetryCounter));
    P = (yamop *)FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return(P);
  case CONSISTENCY_ERROR:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = where;
      nt[0] = Yap_MkApplTerm(FunctorConsistencyError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_ARRAY_OVERFLOW:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomArrayOverflow);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_ARRAY_TYPE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomArrayType);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_IO_MODE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomIOMode);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_MUTABLE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomMutable);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NON_EMPTY_LIST:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomNonEmptyList);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NOT_LESS_THAN_ZERO:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomNotLessThanZero);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NOT_NL:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomNotNewline);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NOT_ZERO:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomNotZero);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_OUT_OF_RANGE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomOutOfRange);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_OPERATOR_PRIORITY:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomOperatorPriority);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_OPERATOR_SPECIFIER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomOperatorSpecifier);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_RADIX:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomRadix);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomShiftCountOverflow);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_SOURCE_SINK:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomSourceSink);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomVStream);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM_OR_ALIAS:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomStreamOrAlias);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM_ENCODING:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomEncoding);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM_POSITION:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomStreamPosition);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_SYNTAX_ERROR_HANDLER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomSyntaxErrorHandler);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_TIMEOUT_SPEC:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomTimeOutSpec);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorDomainError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_SOURCE_SINK:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomSourceSink);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorExistenceError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_ARRAY:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomArray);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorExistenceError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_KEY:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomKey);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorExistenceError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_STREAM:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomVStream);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorExistenceError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_VARIABLE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomVariable);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorExistenceError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_FLOAT_OVERFLOW:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomFloatOverflow);
      nt[0] = Yap_MkApplTerm(FunctorEvaluationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_INT_OVERFLOW:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomIntOverflow);
      nt[0] = Yap_MkApplTerm(FunctorEvaluationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_UNDEFINED:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomUndefined);
      nt[0] = Yap_MkApplTerm(FunctorEvaluationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_FLOAT_UNDERFLOW:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomFloatUnderflow);
      nt[0] = Yap_MkApplTerm(FunctorEvaluationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_UNDERFLOW:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomUnderflow);
      nt[0] = Yap_MkApplTerm(FunctorEvaluationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break; 
 case EVALUATION_ERROR_ZERO_DIVISOR:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomZeroDivisor);
      nt[0] = Yap_MkApplTerm(FunctorEvaluationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case INSTANTIATION_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(AtomInstantiationError);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case OPERATING_SYSTEM_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(AtomOperatingSystemError);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case OUT_OF_HEAP_ERROR:
    {
      int i;
      Term ti[1];

      dump_stack( PASS_REGS1 );
      ti[0] = MkAtomTerm(AtomCodeSpace);
      i = strlen(tmpbuf);
      nt[0] = Yap_MkApplTerm(FunctorResourceError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case OUT_OF_STACK_ERROR:
    {
      int i;
      Term ti[1];

      dump_stack( PASS_REGS1 );
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomStack);
      nt[0] = Yap_MkApplTerm(FunctorResourceError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case OUT_OF_ATTVARS_ERROR:
    {
      int i;
      Term ti[1];

      dump_stack( PASS_REGS1 );
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomAttributes);
      nt[0] = Yap_MkApplTerm(FunctorResourceError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case OUT_OF_AUXSPACE_ERROR:
    {
      int i;
      Term ti[1];

      dump_stack( PASS_REGS1 );
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomUnificationStack);
      nt[0] = Yap_MkApplTerm(FunctorResourceError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case OUT_OF_TRAIL_ERROR:
    {
      int i;
      Term ti[1];

      dump_stack( PASS_REGS1 );
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomTrail);
      nt[0] = Yap_MkApplTerm(FunctorResourceError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_ACCESS_PRIVATE_PROCEDURE:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomAccess);
      ti[1] = MkAtomTerm(AtomPrivateProcedure);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_CREATE_ARRAY:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomCreate);
      ti[1] = MkAtomTerm(AtomArray);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_CREATE_OPERATOR:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomCreate);
      ti[1] = MkAtomTerm(AtomOperator);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_BINARY_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomInput);
      ti[1] = MkAtomTerm(AtomBinaryStream);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomInput);
      ti[1] = MkAtomTerm(AtomPastEndOfStream);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomInput);
      ti[1] = MkAtomTerm(AtomVStream);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_TEXT_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomInput);
      ti[1] = MkAtomTerm(AtomTextStream);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomModify);
      ti[1] = MkAtomTerm(AtomStaticProcedure);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_NEW_ALIAS_FOR_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomNew);
      ti[1] = MkAtomTerm(AtomAlias);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OPEN_SOURCE_SINK:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomOpen);
      ti[1] = MkAtomTerm(AtomSourceSink);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OUTPUT_BINARY_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomOutput);
      ti[1] = MkAtomTerm(AtomBinaryStream);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OUTPUT_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomOutput);
      ti[1] = MkAtomTerm(AtomVStream);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OUTPUT_TEXT_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomOutput);
      ti[1] = MkAtomTerm(AtomTextStream);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_REPOSITION_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomReposition);
      ti[1] = MkAtomTerm(AtomVStream);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_RESIZE_ARRAY:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomResize);
      ti[1] = MkAtomTerm(AtomArray);
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(FunctorPermissionError, 3, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_CHARACTER:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomCharacter);
      nt[0] = Yap_MkApplTerm(FunctorRepresentationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_CHARACTER_CODE:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomCharacterCode);
      nt[0] = Yap_MkApplTerm(FunctorRepresentationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_INT:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomInt);
      nt[0] = Yap_MkApplTerm(FunctorRepresentationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_MAX_ARITY:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomMaxArity);
      nt[0] = Yap_MkApplTerm(FunctorRepresentationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_VARIABLE:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomVariable);
      nt[0] = Yap_MkApplTerm(FunctorRepresentationError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case RESOURCE_ERROR_MAX_STREAMS:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomStreams);
      nt[0] = Yap_MkApplTerm(FunctorResourceError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case RESOURCE_ERROR_STACK:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomStack);
      nt[0] = Yap_MkApplTerm(FunctorResourceError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case RESOURCE_ERROR_HUGE_INT:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomHugeInt);
      nt[0] = Yap_MkApplTerm(FunctorResourceError, 1, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case SYNTAX_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case SAVED_STATE_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(AtomSystemError);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case SYSTEM_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(AtomSystemError);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case INTERNAL_COMPILER_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(AtomInternalCompilerError);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_ARRAY:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomArray);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_ATOM:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomAtom);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_ATOMIC:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomAtomic);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_BIGNUM:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomBigNum);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_BYTE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomByte);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_CALLABLE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomCallable);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_CHAR:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomChar);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_CHARACTER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomCharacter);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_COMPOUND:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomCompound);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_DBREF:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomDBReference);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_DBTERM:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomDBTerm);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_EVALUABLE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomEvaluable);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_FLOAT:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomFloat);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_INTEGER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomInteger);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_KEY:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomKey);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_LIST:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomList);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_NUMBER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomNumber);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_PREDICATE_INDICATOR:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomPredicateIndicator);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_PTR:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomPointer);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_REFERENCE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomDBReference);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_STRING:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomString);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_TEXT:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomText);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_UBYTE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomUnsignedByte);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_UCHAR:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomUnsignedChar);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_VARIABLE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(AtomVariable);
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(FunctorTypeError, 2, ti);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
    break;
  default:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(AtomSystemError);
      psize -= i;
      fun = FunctorError;
      serious = TRUE;
    }
  }
  if (type != PURE_ABORT) {
    /* This is used by some complex procedures to detect there was an error */
    if (IsAtomTerm(nt[0])) {
      strncpy(LOCAL_ErrorSay, RepAtom(AtomOfTerm(nt[0]))->StrOfAE, MAX_ERROR_MSG_SIZ\
E);
      LOCAL_ErrorMessage = LOCAL_ErrorSay;
    } else {
      strncpy(LOCAL_ErrorSay, RepAtom(NameOfFunctor(FunctorOfTerm(nt[0])))->StrOfAE,\
 MAX_ERROR_MSG_SIZE);
      LOCAL_ErrorMessage = LOCAL_ErrorSay;
    }
  }
  switch (type) {
  case OUT_OF_HEAP_ERROR:
  case OUT_OF_STACK_ERROR:
  case OUT_OF_TRAIL_ERROR:
    nt[1] = MkAtomTerm(Yap_LookupAtom(tmpbuf));
    break;
  default:
    {
      Term stack_dump;

      if ((stack_dump = Yap_all_calls()) == 0L) {
	stack_dump = TermNil;
	LOCAL_Error_Size = 0L;
      }
      nt[1] = MkPairTerm(MkAtomTerm(Yap_LookupAtom(tmpbuf)), stack_dump);
      if (type == SYNTAX_ERROR) {
	nt[1] = MkPairTerm(where, nt[1]);
      }
    }
  }
  if (serious) {
    /* disable active signals at this point */
    LOCAL_FirstActiveSignal = LOCAL_LastActiveSignal  = 0;
    CalculateStackGap( PASS_REGS1 );
    LOCAL_PrologMode &= ~InErrorMode;
    LOCK(LOCAL_SignalLock);
    /* we might be in the middle of a critical region */
    if (LOCAL_InterruptsDisabled) {
      LOCAL_InterruptsDisabled = 0;
      LOCAL_UncaughtThrow = TRUE;
      UNLOCK(LOCAL_SignalLock);
      Yap_RestartYap( 1 );
    }
    UNLOCK(LOCAL_SignalLock);
#if DEBUG
    DumpActiveGoals( PASS_REGS1 );
#endif
    /* wait if we we are in user code,
       it's up to her to decide */

    if (LOCAL_PrologMode & UserCCallMode) {
      if (!(EX = Yap_StoreTermInDB(Yap_MkApplTerm(fun, 2, nt), 0))) {
	/* fat chance */
	Yap_RestartYap( 1 );
      }
    } else {
      if (type == PURE_ABORT) {
	Yap_JumpToEnv(MkAtomTerm(AtomDAbort));
      } else
	Yap_JumpToEnv(Yap_MkApplTerm(fun, 2, nt));
      P = (yamop *)FAILCODE;
    }
  } else {
    LOCAL_PrologMode &= ~InErrorMode;
  }
  return P;
}

