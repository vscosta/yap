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


#ifdef DEBUG
STATIC_PROTO (int hidden, (Atom));
STATIC_PROTO (int legal_env, (CELL *));
void STD_PROTO (DumpActiveGoals, (void));
STATIC_PROTO (void detect_bug_location, (yamop *,find_pred_type,char *, int));

#define ONHEAP(ptr) (CellPtr(ptr) >= CellPtr(Yap_HeapBase)  && CellPtr(ptr) < CellPtr(HeapTop))

#define ONLOCAL(ptr) (CellPtr(ptr) > CellPtr(H)  && CellPtr(ptr) < CellPtr(Yap_LocalBase))

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
legal_env (CELL *ep)
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
  READ_LOCK(pe->PRWLock);
  if (!ONHEAP (pe) || Unsigned (pe) & 3 || pe->KindOfPE & 0xff00) {
    READ_UNLOCK(pe->PRWLock);
    return (FALSE);
  }
  READ_UNLOCK(pe->PRWLock);
  return (TRUE);
}

void
DumpActiveGoals (void)
{
  /* try to dump active goals */
  CELL *ep = YENV;		/* and current environment		  */
  choiceptr b_ptr = B;
  CELL cp;
  PredEntry *pe;
  int first = 1;

  if (legal_env (YENV) && YENV < ENV)
    ep = YENV;
  else if (legal_env (ENV))
    ep = ENV;
  while (TRUE)
    {
      if (!ONLOCAL (ep) || (Unsigned (ep) & (sizeof(CELL)-1)))
	break;
      cp = ep[E_CP];
      if (!ONHEAP (cp) || (Unsigned (cp) & (sizeof(CELL)-1)))
	break;
      pe = EnvPreg(cp);
      if (!ONHEAP (pe) || Unsigned (pe) & (sizeof(CELL)-1))
	break;
      READ_LOCK(pe->PRWLock);
      if (pe->KindOfPE & 0xff00) {
	READ_UNLOCK(pe->PRWLock);
	break;
      }
      if (pe->PredFlags & (CompiledPredFlag | DynamicPredFlag))
	{
	  Functor f;
	  Term mod = TermProlog;

	  READ_UNLOCK(pe->PRWLock);
	  f = pe->FunctorOfPred;
	  if (pe->KindOfPE && hidden (NameOfFunctor (f)))
	    goto next;
	  if (first++ == 1)
	    fprintf(stderr,"Active ancestors:\n");
	  if (pe->ModuleOfPred) mod = pe->ModuleOfPred;
	  Yap_plwrite (mod, Yap_DebugPutc, 0);
	  Yap_DebugPutc (Yap_c_error_stream,':');
	  if (pe->ArityOfPE == 0) {
	    Yap_plwrite (MkAtomTerm ((Atom)f), Yap_DebugPutc, 0);
	  } else {
	    Yap_plwrite (MkAtomTerm (NameOfFunctor (f)), Yap_DebugPutc, 0);
	    Yap_DebugPutc (Yap_c_error_stream,'/');
	    Yap_plwrite (MkIntTerm (ArityOfFunctor (f)), Yap_DebugPutc, 0);
	  }
	  Yap_DebugPutc (Yap_c_error_stream,'\n');
	} else {
	  READ_UNLOCK(pe->PRWLock);
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
      READ_LOCK(pe->PRWLock);
      {
	Functor f;
	Term mod = PROLOG_MODULE;

	f = pe->FunctorOfPred;
	if (pe->ModuleOfPred)
	  mod = pe->ModuleOfPred;
	else mod = TermProlog;
	Yap_plwrite (mod, Yap_DebugPutc, 0);
	Yap_DebugPutc (Yap_c_error_stream,':');
	if (pe->ArityOfPE == 0) {
	  Yap_plwrite (MkAtomTerm (NameOfFunctor(f)), Yap_DebugPutc, 0);
	} else {
	  Int i = 0, arity = pe->ArityOfPE;
	  Term *args = &(b_ptr->cp_a1);
	  Yap_plwrite (MkAtomTerm (NameOfFunctor (f)), Yap_DebugPutc, 0);
	  Yap_DebugPutc (Yap_c_error_stream,'(');
	  for (i= 0; i < arity; i++) {
	    if (i > 0) Yap_DebugPutc (Yap_c_error_stream,',');
	    Yap_plwrite(args[i], Yap_DebugPutc, Handle_vars_f);
	  }
	  Yap_DebugPutc (Yap_c_error_stream,')');
	}
	Yap_DebugPutc (Yap_c_error_stream,'\n');
      }
      READ_UNLOCK(pe->PRWLock);
      b_ptr = b_ptr->cp_b;
    }
}
#endif /* DEBUG */

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
#if SHORT_INTS
    snprintf(tp, psize, "prolog:%s/%ld",
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    snprintf(tp, psize, "prolog:%s/%d",
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#else
#if SHORT_INTS
    sprintf(tp, "in prolog:%s/%ld",
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    sprintf(tp, "in prolog:%s/%d",
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#endif
  } else if (cl < 0) {
#if   HAVE_SNPRINTF
#if SHORT_INTS
    snprintf(tp, psize, "indexing code of %s:%s/%ld",
	     RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    snprintf(tp, psize, "indexing code of %s:%s/%d",
	     RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#else
#if SHORT_INTS
    sprintf(tp, "indexing code of %s:%s/%ld",
	    RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#else 
    sprintf(tp, "indexing code of %s:%s/%d",
	    RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#endif
  } else {
#if   HAVE_SNPRINTF
#if SHORT_INTS
    snprintf(tp, psize, "clause %ld of %s:%s/%ld", cl,
	     RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    snprintf(tp, psize, "clause %d of %s:%s/%d", cl,
	     RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#else
#if SHORT_INTS
    sprintf(tp, "clause %ld of %s:%s/%ld", cl,
	    RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    sprintf(tp, "clause %d of %s:%s/%d", cl,
	    RepAtom(AtomOfTerm(pred_module))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#endif
  }
}

static void
dump_stack(void)
{
  choiceptr b_ptr = B;
  CELL *env_ptr = ENV;
  char tp[256];
  yamop *ipc = CP;
  
#if DEBUG
  fprintf(stderr,"%% YAP regs: P=%p, CP=%p, ASP=%p, H=%p, TR=%p, HeapTop=%p\n",P,CP,ASP,H,TR,HeapTop);
  fprintf(stderr,"%% YAP mode: %x\n",Yap_PrologMode);
  if (Yap_ErrorMessage)
    fprintf(stderr,"%% YAP_ErrorMessage: %s\n",Yap_ErrorMessage);
#endif
  if (H > ASP || H > LCL0) {
    fprintf(stderr,"%% YAP ERROR: Global Collided against Local (%p--%p)\n",H,ASP);
  } else   if (HeapTop > (ADDR)Yap_GlobalBase) {
    fprintf(stderr,"%% YAP ERROR: Code Space Collided against Global (%p--%p)\n", HeapTop, Yap_GlobalBase);
  } else {
#if !USE_SYSTEM_MALLOC
    fprintf (stderr,"%ldKB of Code Space (%p--%p)\n",(long int)((CELL)HeapTop-(CELL)Yap_HeapBase)/1024,Yap_HeapBase,HeapTop); 
#if USE_DL_MALLOC 
    if (Yap_hole_start) {
      fprintf (stderr,"  Last hole: %p--%p\n", Yap_hole_start, Yap_hole_end);
    }
#endif
#endif
    fprintf (stderr,"%ldKB of Global Stack (%p--%p)\n",(long int)(sizeof(CELL)*(H-H0))/1024,H0,H); 
    fprintf (stderr,"%ldKB of Local Stack (%p--%p)\n",(long int)(sizeof(CELL)*(LCL0-ASP))/1024,ASP,LCL0); 
    fprintf (stderr,"%ldKB of Trail (%p--%p)\n",(long int)((ADDR)TR-Yap_TrailBase)/1024,Yap_TrailBase,TR); 
    fprintf (stderr,"Performed %d garbage collections\n", GcCalls);
#if LOW_LEVEL_TRACER
    {
      extern long long vsc_count;

      if (vsc_count) {
	fprintf(stderr,"Trace Counter at %lld\n",vsc_count);
      }
    }
#endif
    detect_bug_location(P, FIND_PRED_FROM_ANYWHERE, (char *)H, 256);
    fprintf (stderr,"Running code at %s\n",(char *)H); 
    detect_bug_location(CP, FIND_PRED_FROM_ANYWHERE, (char *)H, 256);
    fprintf (stderr,"Continuation is at %s\n",(char *)H); 
    while (b_ptr != NULL) {
      while (env_ptr && env_ptr <= (CELL *)b_ptr) {
	detect_bug_location(ipc, FIND_PRED_FROM_ENV, tp, 256);
	if (env_ptr == (CELL *)b_ptr &&
	    (choiceptr)env_ptr[E_CB] > b_ptr) {
	  b_ptr = b_ptr->cp_b;
	  fprintf(stderr,"  %s (*)\n", tp);
	} else {
	  fprintf(stderr,"  %s\n", tp);
	}
	ipc = (yamop *)(env_ptr[E_CP]);
	env_ptr = (CELL *)(env_ptr[E_E]);
      }
      if (b_ptr) {
	if (b_ptr->cp_ap->opc != Yap_opcode(_or_else) &&
	    b_ptr->cp_ap->opc != Yap_opcode(_or_last) &&
	    b_ptr->cp_ap->opc != Yap_opcode(_Nstop)) {
	  /* we can safely ignore ; because there is always an upper env */
	  detect_bug_location(b_ptr->cp_ap, FIND_PRED_FROM_CP, tp, 256);
	  fprintf(stderr,"     %s (*)\n", tp);
	}
	b_ptr = b_ptr->cp_b;
      }
    }
  }
}


static void
error_exit_yap (int value)
{
  if (!(Yap_PrologMode & BootMode)) {
    dump_stack();
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
  detect_bug_location(pc, FIND_PRED_FROM_ANYWHERE, (char *)H, 256);
  fprintf(stderr,"%s\n",(char *)H);
}

#endif

/* This needs to be a static because I can't trust the stack (WIN32), and
   I can't trust the Yap stacks  (error) */
#define YAP_BUF_SIZE 512

static char tmpbuf[YAP_BUF_SIZE];

yamop *
Yap_Error(yap_error_number type, Term where, char *format,...)
{
  va_list ap;
  CELL nt[3];
  Functor fun;
  int serious;
  char *tp = tmpbuf;
  int psize = YAP_BUF_SIZE;

#if DEBUG_STRICT
  if (Yap_heap_regs && !(Yap_PrologMode & BootMode)) 
    fprintf(stderr,"***** Processing Error %d (%lx,%x) %s***\n", type, (unsigned long int)ActiveSignals,Yap_PrologMode,format);
  else
    fprintf(stderr,"***** Processing Error %d (%x) %s***\n", type,Yap_PrologMode,format);
#endif
  if (type == INTERRUPT_ERROR) {
    fprintf(stderr,"%% YAP exiting: cannot handle signal %d\n",
	    (int)IntOfTerm(where));
    Yap_exit(1);
  }
  /* disallow recursive error handling */ 
  if (Yap_PrologMode & InErrorMode) {
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
    fprintf(stderr,"%% ERROR WITHIN ERROR: %s\n", tmpbuf);
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
    if (Yap_PrologMode == UserCCallMode) {
      fprintf(stderr,"%% YAP OOOPS in USER C-CODE: %s.\n",tmpbuf);
    } else {
      fprintf(stderr,"%% YAP OOOPS: %s.\n",tmpbuf);
    }
    error_exit_yap (1);
  }
  if (P == (yamop *)(FAILCODE))
   return P;
  /* PURE_ABORT may not have set where correctly, BootMode may not have the data terms ready */
  if (type == PURE_ABORT || Yap_PrologMode & BootMode) {
    where = TermNil;
    Yap_PrologMode &= ~AbortMode;
    Yap_PrologMode |= InErrorMode;
  } else {
    if (IsVarTerm(where)) {
      /* we must be careful someone gave us a copy to a local variable */
      Term t = MkVarTerm();
      Yap_unify(t, where);
      where = Deref(where);
    }
    /* Exit Abort Mode, if we were there */
    Yap_PrologMode &= ~AbortMode;
    Yap_PrologMode |= InErrorMode;
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
  if (Yap_PrologMode & BootMode) {
    /* crash in flames! */
    fprintf(stderr,"%% YAP Fatal Error: %s exiting....\n",tmpbuf);
    error_exit_yap (1);
  }
#ifdef DEBUGX
  DumpActiveGoals();
#endif /* DEBUG */
  switch (type) {
  case INTERNAL_ERROR:
    {
      fprintf(stderr,"%% Internal YAP Error: %s exiting....\n",tmpbuf);
      serious = TRUE;
      if (Yap_PrologMode & BootMode) {
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
    fprintf(stderr,"%% YAP execution aborted.\n");
    fun = FunctorVar;
    serious = TRUE;
    break;
  case CALL_COUNTER_UNDERFLOW:
    /* Do a long jump */
    PredEntriesCounter--;
    Yap_JumpToEnv(MkAtomTerm(Yap_LookupAtom("call_counter")));
    P = (yamop *)FAILCODE;
    Yap_PrologMode &= ~InErrorMode;
    return(P);
  case PRED_ENTRY_COUNTER_UNDERFLOW:
    /* Do a long jump */
    Yap_JumpToEnv(MkAtomTerm(Yap_LookupAtom("call_and_retry_counter")));
    P = (yamop *)FAILCODE;
    Yap_PrologMode &= ~InErrorMode;
    return(P);
  case RETRY_COUNTER_UNDERFLOW:
    /* Do a long jump */
    PredEntriesCounter--;
    Yap_JumpToEnv(MkAtomTerm(Yap_LookupAtom("retry_counter")));
    P = (yamop *)FAILCODE;
    Yap_PrologMode &= ~InErrorMode;
    return(P);
  case CONSISTENCY_ERROR:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("consistency_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_ARRAY_OVERFLOW:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("array_overflow"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_ARRAY_TYPE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("array_type"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_IO_MODE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("io_mode"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_MUTABLE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("mutable"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NON_EMPTY_LIST:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("non_empty_list"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NOT_LESS_THAN_ZERO:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("not_less_than_zero"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NOT_NL:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("not_newline"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NOT_ZERO:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("not_zero"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_OUT_OF_RANGE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("out_of_range"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_OPERATOR_PRIORITY:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("operator_priority"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_OPERATOR_SPECIFIER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("operator_specifier"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_RADIX:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("radix"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("shift_count_overflow"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_SOURCE_SINK:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("source_sink"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("stream"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM_OR_ALIAS:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("stream_or_alias"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM_POSITION:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("stream_position"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_SYNTAX_ERROR_HANDLER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("syntax_error_handler"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_TIMEOUT_SPEC:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("time_out_spec"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_SOURCE_SINK:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("source_sink"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("existence_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_ARRAY:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("array"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("existence_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_STREAM:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("stream"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("existence_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_FLOAT_OVERFLOW:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("float_overflow"));
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_INT_OVERFLOW:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("int_overflow"));
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_UNDEFINED:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("undefined"));
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_FLOAT_UNDERFLOW:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("float_underflow"));
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_UNDERFLOW:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("underflow"));
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break; 
 case EVALUATION_ERROR_ZERO_DIVISOR:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("zero_divisor"));
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case INSTANTIATION_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(Yap_LookupAtom("instantiation_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case OUT_OF_HEAP_ERROR:
    {
      int i;

      dump_stack();
      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(Yap_LookupAtom("out_of_heap_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case OUT_OF_STACK_ERROR:
    {
      int i;

      dump_stack();
      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(Yap_LookupAtom("out_of_stack_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case OUT_OF_ATTVARS_ERROR:
    {
      int i;

      dump_stack();
      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(Yap_LookupAtom("out_of_attvars_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case OUT_OF_AUXSPACE_ERROR:
    {
      int i;

      dump_stack();
      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(Yap_LookupAtom("out_of_auxspace_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case OUT_OF_TRAIL_ERROR:
    {
      int i;

      dump_stack();
      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(Yap_LookupAtom("out_of_trail_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_ACCESS_PRIVATE_PROCEDURE:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("access"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("private_procedure"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_CREATE_ARRAY:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("create"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("array"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_CREATE_OPERATOR:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("create"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("operator"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_BINARY_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("input"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("binary_stream"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("input"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("past_end_of_stream"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("input"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("stream"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_TEXT_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("input"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("text_stream"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("modify"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("static_procedure"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_NEW_ALIAS_FOR_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("new"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("alias"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OPEN_SOURCE_SINK:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("open"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("source_sink"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OUTPUT_BINARY_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("output"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("binary_stream"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OUTPUT_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("output"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("stream"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OUTPUT_TEXT_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("output"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("text_stream"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_REPOSITION_STREAM:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("reposition"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("stream"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_RESIZE_ARRAY:
    {
      int i;
      Term ti[3];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("resize"));
      ti[1] = MkAtomTerm(Yap_LookupAtom("array"));
      ti[2] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_CHARACTER:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("character"));
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("representation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_CHARACTER_CODE:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("character_code"));
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("representation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_MAX_ARITY:
    {
      int i;
      Term ti[1];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("max_arity"));
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("representation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case SYNTAX_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = where;
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case SYSTEM_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(Yap_LookupAtom("system_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case INTERNAL_COMPILER_ERROR:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(Yap_LookupAtom("internal_compiler_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_ARRAY:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("array"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_ATOM:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("atom"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_ATOMIC:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("atomic"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_BYTE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("byte"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_CALLABLE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("callable"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_CHAR:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("char"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_CHARACTER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("character"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_COMPOUND:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("compound"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_DBREF:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("db_reference"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_DBTERM:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("db_term"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_EVALUABLE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("evaluable"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_FLOAT:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("float"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_INTEGER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("integer"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_KEY:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("key"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_LIST:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("list"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_NUMBER:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("number"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_PREDICATE_INDICATOR:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("predicate_indicator"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_PTR:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("pointer"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_UBYTE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("unsigned_byte"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_UCHAR:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("unsigned_char"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_VARIABLE:
    {
      int i;
      Term ti[2];

      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(Yap_LookupAtom("variable"));
      ti[1] = where;
      nt[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  default:
    {
      int i;

      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(Yap_LookupAtom("system_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = Yap_MkFunctor(Yap_LookupAtom("error"),2);
      serious = TRUE;
    }
  }
  if (type != PURE_ABORT) {
    /* This is used by some complex procedures to detect there was an error */
    if (IsAtomTerm(nt[0])) {
      Yap_ErrorMessage = RepAtom(AtomOfTerm(nt[0]))->StrOfAE;
    } else {
      Yap_ErrorMessage = RepAtom(NameOfFunctor(FunctorOfTerm(nt[0])))->StrOfAE;
    }
  }
  switch (type) {
  case OUT_OF_HEAP_ERROR:
  case OUT_OF_STACK_ERROR:
  case OUT_OF_TRAIL_ERROR:
    nt[1] = MkAtomTerm(Yap_LookupAtom(tmpbuf));
    break;
  default:
    nt[1] = MkPairTerm(MkAtomTerm(Yap_LookupAtom(tmpbuf)), Yap_all_calls());
  }
  if (serious) {
    /* disable active signals at this point */
    ActiveSignals = 0;
    CreepFlag = CalculateStackGap();
    if (type == PURE_ABORT)
      Yap_JumpToEnv(MkAtomTerm(Yap_LookupAtom("$abort")));
    else
      Yap_JumpToEnv(Yap_MkApplTerm(fun, 2, nt));
    P = (yamop *)FAILCODE;
  }
  Yap_PrologMode &= ~InErrorMode;
  return P;
}

