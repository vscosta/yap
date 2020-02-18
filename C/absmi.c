/*************************************************************************
 *									 *
 *	 Yap Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *       								 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		absmi.c							 *
 * comments:	Portable abstract machine interpreter                    *
 * Last rev:     $Date: 2008-08-13 01:16:26 $,$Author: vsc $
 **
 * $Log: not supported by cvs2svn $
 * Revision 1.246  2008/08/12 01:27:22  vsc
 *									 *
 *									 *
 *************************************************************************/

/**

   @file absmi.c

   @{

   We next discuss several issues on trying to make Prolog programs run
   fast in YAP. We assume two different programming styles:

   + Exwaution of <em>deterministic</em> programs often
   boils down to a recursive loop of the form:

   ~~~~~
   loop(Done).
   loop(Env) :-
   do_something(Env,NewEnv),
   loop(NewEnv).
   ~~~~~

   or to the repeat-fail loop:

   ~~~~~
   loop(Inp) :-
   do_something(Inp,Out),
   out_and_fail(Out).
   ~~~~~


   @}

   @defgroup Implementation Implementation Considerations
   @ingroup YAPProgramming

   @{

   This section is about the YAP implementation, and is mostly of
   interest to hackers.
   @}
   @defgroup Emulator The Abstract Machine Emulator
   @ingroup Implementation


   @{

*/

/// code belongs to the emulator
#define IN_ABSMI_C 1
#define _INATIV

/// use tmp variables that are placed in registers
#define HAS_CACHE_REGS 1
#include "absmi.h"
#include "heapgc.h"

#include "cut_c.h"

#if YAP_JIT
#include "IsGround.h"

TraceContext **curtrace;
yamop *curpreg;
BlocksContext **globalcurblock;
COUNT ineedredefinedest;
yamop *headoftrace;

NativeContext *NativeArea;
IntermediatecodeContext *IntermediatecodeArea;

CELL l;

CELL nnexec;

Environment *Yap_ExpEnvP, Yap_ExpEnv;

void **Yap_ABSMI_ControlLabels;

static Int traced_absmi(void) { return Yap_traced_absmi(); }

#endif

#ifndef YREG
#define YREG YENV
#endif

void **Yap_ABSMI_OPCODES;

#ifdef PUSH_X
#else

/* keep X as a global variable */

Term Yap_XREGS[MaxTemps]; /* 29                                     */

#endif

#include "arith2.h"

// #include "print_preg.h"
//#include "sprint_op.hpp"
//#include "print_op.hpp"


static Term save_goal(PredEntry *pe USES_REGS) {
    BEGD(rc);
  CELL *S_PT;
  //  printf("D %lx %p\n", LOCAL_ActiveSignals, P);
  /* tell whether we can creep or not, this is hard because we will
     lose the info RSN
  */
  arity_t arity;
  /* if (pe->ModuleOfPred == PROLOG_MODULE) { */
  /*       if (CurrentModule == PROLOG_MODULE) */
  /*           HR[0] = TermProlog; */
  /*       else */
  /*           HR[0] = CurrentModule; */
  /*   } else { */
  /*       HR[0] = Yap_Module_Name(pe); */
  /*   } */
  S_PT = HR;
  HR += 3;
  rc = AbsAppl(S_PT);
  S_PT[0] = (CELL)FunctorModule;
  S_PT[1] = Yap_Module_Name(pe);
  arity = pe->ArityOfPE;
  if (arity == 0) {
    S_PT[2] = MkAtomTerm((Atom)pe->FunctorOfPred);
  } else {
    S_PT[2] = AbsAppl(HR);
    S_PT = HR;
    HR += arity+1;
    S_PT[0] = (CELL)pe->FunctorOfPred;
    BEGP(pt1);
    S_PT++;
    pt1 = XREGS;
    for (; arity > 0; --arity) {
      BEGD(d1);
      BEGP(pt0);
      pt0 = pt1++;
      d1 = *pt0;
      deref_head(d1, creep_unk);
    creep_nonvar:
      /* just copy it to the heap */
       *S_PT++ = d1;
      continue;

      derefa_body(d1, pt0, creep_unk, creep_nonvar);
      if (pt0 <= HR) {
	/* variable is safe */
	*S_PT++ = (CELL)pt0;
      } else {
	/* bind it, in case it is a local variable */
	d1 = Unsigned(S_PT);
	RESET_VARIABLE(S_PT);
	pt1++;
	S_PT += 1;
	Bind_Local(pt0, d1);
      }
      ENDP(pt0);
      ENDD(d1);
    }
    ENDP(pt1);
  }
  return rc;
      ENDD(rc);
}

#ifdef COROUTINING
/*
  this one's called before gc or stack expansion. It generates a consistent
  set of registers, so that they can be marked and assigned by the g collector.
*/
static arity_t live_regs( yamop *pco, PredEntry *pe) {
  CACHE_REGS
    if (pco->opc != Yap_opcode(_skip) &&
        pco->opc != Yap_opcode(_move_back))
      return pe->ArityOfPE;
  CELL *lab = (CELL *)(pco->y_u.l.l);
  arity_t max =       lab[0];   // largest live register
  CELL curr = lab[1];  // bitmap for 0-63 or 0-31
  arity_t i;
  lab += 2;
  for (i = 0; i <= max; i++) {
    //Process a group of N registers                                   
    if (i == 8 * CellSize) {
      lab++;
    }
    if (curr & 1) {
      continue;
    } else {
      /* dead register but let's store safe contents*/
      XREGS[i] = MkIntegerTerm(i);
    }
        
  }
  // this will be the new arity
  return max;
}
#endif

#if USE_THREADED_CODE && (defined(ANALYST) || defined(DEBUG))

char *Yap_op_names[] = {
#define OPCODE(OP, TYPE) #OP
#include "YapOpcodes.h"
#undef OPCODE
};

#endif

static int check_alarm_fail_int(int CONT USES_REGS) {
#if defined(_MSC_VER) || defined(__MINGW32__)
  /* I need this for Windows and any system where SIGINT
     is not proceesed by same thread as absmi */
  if (LOCAL_PrologMode & (AbortMode | InterruptMode)) {
    CalculateStackGap(PASS_REGS1);
  }
#endif
  if (Yap_get_signal(YAP_FAIL_SIGNAL)) {
    return INT_HANDLER_FAIL;
  }
  // fail even if there are more signals, they will have to be dealt later.
  return INT_HANDLER_GO_ON;
}

static int stack_overflow(PredEntry *pe, CELL *env, yamop *cp,
                          arity_t nargs USES_REGS) {
  if (Unsigned(YREG) - Unsigned(HR) < StackGap(PASS_REGS1) ||
      Yap_get_signal(YAP_STOVF_SIGNAL)) {
    S = (CELL *)pe;
    // p should be past the enbironment mang Obpp
    if (!Yap_dogc(P, live_regs(P, pe), NULL PASS_REGS)) {
      Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, "stack overflow: gc failed");
    }
    return INT_HANDLER_RET_JMP;
  }
  return INT_HANDLER_GO_ON;
}

static int code_overflow(CELL *yenv USES_REGS) {
  if (Yap_get_signal(YAP_CDOVF_SIGNAL)) {
    CELL cut_b = LCL0 - (CELL *)(yenv[E_CB]);

    /* do a garbage collection first to check if we can recover memory */
    if (!Yap_locked_growheap(false, 0, NULL)) {
      Yap_ThrowError(RESOURCE_ERROR_HEAP, TermNil, "YAP failed to grow heap: %s",
		   "malloc/mmap failed");
      return INT_HANDLER_FAIL;
    }
    CACHE_A1();
    if (yenv == ASP) {
      yenv[E_CB] = (CELL)(LCL0 - cut_b);
    }
  return INT_HANDLER_RET_JMP;
  }
  return INT_HANDLER_GO_ON;
}

/*
  Imagine we are interrupting the execution, say, because we have a spy
   point or because we have goals to wake up. This routine saves the current
   live temporary registers into a structure pointed to by register ARG1.
   The registers are then recovered by a nasty builtin
   called
*/
static Term save_xregs(yamop *pco) {
  CACHE_REGS
    CELL *lab = (CELL *)(pco->y_u.l.l),  *lab0 = lab;
  CELL max = lab[0];
  CELL curr = lab[1];
  Term tp = MkIntegerTerm((Int)pco);
  Term tcp = MkIntegerTerm((Int)CP);
  Term tenv = MkIntegerTerm((Int)(LCL0 - ENV));
  Term tyenv = MkIntegerTerm((Int)(LCL0 - YENV));
  arity_t  tot = 4;
  {
    CELL i;

    lab += 2;
    for (i = 0; i <= max; i++) {
      if (i == 8 * CellSize) {
        curr = lab[0];
        lab++;
      }
      if (curr & 1) {
        CELL d1;

        tot += 2;
       d1 = XREGS[i];
        deref_head(d1, mkglobal_unk);
      mkglobal_nonvar:
	continue;
        {
          CELL *pt0;
          deref_body(d1, pt0, mkglobal_unk, mkglobal_nonvar);
          /* bind it, in case it is a local variable */
          if (pt0 > ASP) {            /* variable is safe */
            d1 = Unsigned(HR + 1);
            RESET_VARIABLE(HR + 1);
            Bind_Local(pt0, d1);
          }
        }
      }
      curr >>= 1;
    }
    if (tot == 4)
      return TermTrue;
    lab = lab0;
      CELL *start = HR;

  *HR++ =  (CELL)Yap_MkFunctor(AtomTrue, tot);
  *HR++ = tp;
  *HR++ = tcp;
  *HR++ = tenv;
  *HR++ = tyenv;

    lab += 2;
    for (i = 0; i <= max; i++) {
      if (i == 8 * CellSize) {
        curr = lab[0];
        lab++;
      }
      if (curr & 1) {
        CELL d1;

        HR[0] = MkIntTerm(i);
        d1 = XREGS[i];
        deref_head(d1, wake_up_unk);
      wake_up_nonvar:
        /* just copy it to the heap */
        HR[1] = d1;
        HR += 2;
        continue;

        {
          CELL *pt0;
          deref_body(d1, pt0, wake_up_unk, wake_up_nonvar);
            /* variable is safe */
            HR[1] = (CELL)pt0;
        }
        HR += 2;
      }
      curr >>= 1;
    }
    return (AbsAppl(start));
  }
}
static Term addg(bool *goalp, Term g, Term tg)
{
    if (*goalp && *goalp != TermTrue) {
  Term ts[2];
    ts[0] =  g;
    ts[1] = tg;
    return Yap_MkApplTerm(FunctorComma,2,ts);
  } else {
    *goalp = true;
    return g;
  }
}

/** interrupt handling code

 It creates a conjunction with:
 + wake_up_goal, if exists;
 + generic signal processing;
 + cut
 + continuation goal
 + register recovery
*/
static bool interrupt_wake_up(Term  continuation, yamop *plab, Term cut_t USES_REGS) {
  //  printf("D %lx %p\n", LOCAL_ActiveSignals, P);
  /* tell whether we can creep or not, this is hard because we will
     lose the info RSN
  */
  bool goal = false;
  bool wk = Yap_get_signal(YAP_WAKEUP_SIGNAL);
  bool creep = Yap_get_signal(YAP_CREEP_SIGNAL);
  yamop *op = P, *ocp = CP;
  Term tg;
  
  if (plab) {
    Term g = save_xregs(plab PASS_REGS);
    if (g != TermTrue) {
    tg = Yap_MkApplTerm(FunctorRestoreRegs1, 1, &g);
    goal = true;
    }
  }
  if (continuation && continuation!=TermTrue) {
    Term g;
    if  (creep) {
      PredEntry *pe = CreepCode;
      g = Yap_MkApplTerm(pe->FunctorOfPred,1,&continuation);
  } else {
    g = continuation;
  }
    tg = addg(&goal, g, tg);
  }

  if (cut_t && IsIntegerTerm(cut_t)) {
    Term g = Yap_MkApplTerm(FunctorCutBy,1, &cut_t);
    tg = addg(&goal, g, tg);
  }


  if (wk) {
  Term ws = Yap_ListOfWokenGoals();
    while (IsPairTerm(ws)) {
      Term g = HeadOfTerm(ws);
      tg = addg(&goal, g, tg);
      ws = TailOfTerm(ws);
    }
  }
  yap_signals sig;	
  while ((sig = Yap_get_signal(LOCAL_Signals))) {
      Term g = MkIntegerTerm(sig);
      g = Yap_MkApplTerm(FunctorSig,1,&g);
            tg = addg(&goal, g, tg);
  }
  if (!goal || tg == TermTrue)
    return true;
  if (tg == TermFalse || tg == TermFail)
    return false;
  PredEntry *pe;
  if (IsApplTerm(tg)) {
    Functor f = FunctorOfTerm(tg);
    arity_t i, n = ArityOfFunctor(f);
    CELL *p = RepAppl(tg)+1;
    for (i = 0; i < n; i++) {
      XREGS[i+1] = p[i];
  }
    pe =  RepPredProp(Yap_GetPredPropByFunc(f, n));
  } else {
    pe = NULL;
    return false;
  }
  CACHE_A1();
  return  Yap_execute_pred(pe, NULL,false PASS_REGS);}

#if 0
#define DEBUG_INTERRUPTS()
#else
/* to trace interrupt calls */
#define DEBUG_INTERRUPTS()					\
    fprintf(stderr, "[%d] %" PRId64 " %s:%d:  (B=%p ENV=%p ASP=%p)\n",\
      worker_id, LOCAL_Signals,\
            __FUNCTION__, __LINE__, B, ENV, ASP)
#endif

static int interrupt_fail(USES_REGS1) {
  DEBUG_INTERRUPTS();
  check_alarm_fail_int(false PASS_REGS);
  /* don't do debugging and stack expansion here: space will
     be recovered. automatically by fail, so
     better wait.
  */
  if (Yap_has_signal(YAP_CREEP_SIGNAL)) {
    return INT_HANDLER_FAIL;
  }
  if (Yap_has_signal(YAP_CDOVF_SIGNAL)) {
    return INT_HANDLER_FAIL;
  }
  /* make sure we have the correct environment for continuation */
  ENV = B->cp_env;
  YENV = (CELL *)B;
    return interrupt_wake_up(TermFail, NULL, 0 PASS_REGS) ?
	  INT_HANDLER_RET_NEXT :
	  INT_HANDLER_FAIL;
}

static int interrupt_execute(USES_REGS1) {
  int v;

DEBUG_INTERRUPTS();
 if ( (v=check_alarm_fail_int(true PASS_REGS)) != INT_HANDLER_GO_ON) {
    return INT_HANDLER_FAIL;
  }
  if (PP)
    UNLOCKPE(1, PP);
  PP = P->y_u.Osbpp.p0;
  if ((P->y_u.Osbpp.p->PredFlags & (NoTracePredFlag | HiddenPredFlag)) &&
      Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return INT_HANDLER_RET_JMP;
  }
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  if ((v = stack_overflow(P->y_u.Osbpp.p, ENV, NEXTOP(P,Osbpp),
			  P->y_u.Osbpp.p->ArityOfPE PASS_REGS)) != INT_HANDLER_GO_ON) {
    return INT_HANDLER_RET_NEXT; // restartx
  }
    return interrupt_wake_up(save_goal(P->y_u.Osbpp.p), NULL, 0 PASS_REGS) ?
	  INT_HANDLER_RET_NEXT :
	  INT_HANDLER_FAIL;
}

static int interrupt_call(USES_REGS1) {
  int v;
  PredEntry *pe;

DEBUG_INTERRUPTS();
 if ( (v=check_alarm_fail_int(true PASS_REGS)) != INT_HANDLER_GO_ON) {
    return INT_HANDLER_FAIL;
  }
  if (PP)
    UNLOCKPE(1, PP);
  PP = P->y_u.Osbpp.p0;
  pe = P->y_u.Osbpp.p;
   if (Yap_only_has_signal(YAP_CREEP_SIGNAL) &&
      (pe->PredFlags & (NoTracePredFlag | HiddenPredFlag))) {
    return INT_HANDLER_RET_JMP;
  }
  if ((v = code_overflow(YENV PASS_REGS))  != INT_HANDLER_GO_ON) {
    return INT_HANDLER_RET_JMP;
  }
  // at this point P is already at the end of the instruction.
  if ((v = stack_overflow(pe, YENV, NEXTOP(P, Osbpp),
			  pe->ArityOfPE PASS_REGS)) != INT_HANDLER_GO_ON ) {
    return INT_HANDLER_RET_JMP;
  } else {
    return interrupt_wake_up(save_goal(pe), NULL, 0 PASS_REGS) ?
	  INT_HANDLER_RET_NEXT :
	  INT_HANDLER_FAIL;
 }
}

static int interrupt_pexecute(PredEntry *pen USES_REGS) {
  int v;

  DEBUG_INTERRUPTS();
 if ( (v=check_alarm_fail_int(true PASS_REGS)) != INT_HANDLER_GO_ON) {
    return INT_HANDLER_FAIL;
  }
  if (PP)
    UNLOCKPE(1, PP);
  PP = NULL;
  if (Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return INT_HANDLER_RET_JMP; /* keep on creeping */
  }
  if ((v = code_overflow(YENV PASS_REGS)) != INT_HANDLER_GO_ON) {
    return v;
  }
  if ((v = stack_overflow(pen, YENV, NEXTOP(P, Osbmp),pen->ArityOfPE PASS_REGS))) {
    return v;
  }
    return interrupt_wake_up(save_goal(pen), NULL, 0 PASS_REGS) ?
	  INT_HANDLER_RET_NEXT :
	  INT_HANDLER_FAIL;

}

static void execute_dealloc(USES_REGS1) {
  /* other instructions do depend on S being set by deallocate
   */
  CELL *ENVYREG = YENV;
  S = ENVYREG;
  CP = (yamop *)ENVYREG[E_CP];
  ENV = ENVYREG = (CELL *)ENVYREG[E_E];
#ifdef DEPTH_LIMIT
  DEPTH = ENVYREG[E_DEPTH];
#endif /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
  {
    choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
    if (ENVYREG > (CELL *)top_b || ENVYREG < HR)
      ENVYREG = (CELL *)top_b;
#else
    if (ENVYREG > (CELL *)top_b)
      ENVYREG = (CELL *)top_b;
#endif /* YAPOR_SBA */
    else
      ENVYREG = (CELL *)((CELL)ENVYREG + ENV_Size(CP));
  }
#else
  if (ENVYREG > (CELL *)B)
    ENVYREG = (CELL *)B;
  else
    YENV = (CELL *)((CELL)YENV + ENV_Size(CP));
#endif /* FROZEN_STACKS
 * */
}

/* don't forget I cannot creep at deallocate (where to?) */
/* also, this is unusual in that I have already done deallocate,
   so I don't need to redo it.
*/
static int interrupt_deallocate(USES_REGS1) {
  int v;
      Term cut;
      int rc;

  DEBUG_INTERRUPTS();
 if ( (v=check_alarm_fail_int(true PASS_REGS)) != INT_HANDLER_GO_ON) {
    return INT_HANDLER_FAIL;
  }
  /*
    don't do a creep here; also, if our instruction is followed by
    a execute_c, just wait a bit more */
  if (Yap_only_has_signals(YAP_CREEP_SIGNAL, YAP_WAKEUP_SIGNAL)) {
    /* keep on going if there is something else */
    execute_dealloc(PASS_REGS1);
    P = NEXTOP(NEXTOP(P, Osbpp), p);
    return INT_HANDLER_RET_NEXT;
  } else {
    if (PP)
      UNLOCKPE(1, PP);
    if ((v = code_overflow(YENV PASS_REGS)) != INT_HANDLER_GO_ON ) {
      return v;
    }

      if (NEXTOP(NEXTOP(P, Osbpp), p)->opc == Yap_opcode(_cut_e)) {
	/* followed by a cut */
	cut = MkIntegerTerm(LCL0 - (CELL *) S[E_CB]);
      } else {
	cut = 0;
        }
      // deallocate moves P one step forward.

  }
      rc = interrupt_wake_up(TermTrue, NULL, cut PASS_REGS) ?
	  INT_HANDLER_RET_NEXT :
	  INT_HANDLER_FAIL;
  YENV[E_CB] = (CELL)B;
  return rc;
}

static int interrupt_prune(CELL *upto, yamop *p USES_REGS) {
  Term cut_t = MkIntegerTerm(LCL0 - upto);
    
  int v;
  DEBUG_INTERRUPTS();
 if ( (v=check_alarm_fail_int(true PASS_REGS)) != INT_HANDLER_GO_ON) {
    return INT_HANDLER_FAIL;
  }

 p = NEXTOP(p, Osblp) ;
    v =   interrupt_wake_up(0, p, cut_t PASS_REGS) ?
	  INT_HANDLER_RET_NEXT :
	  INT_HANDLER_FAIL;
 P = NEXTOP(p,l);
  return v;
}

static int interrupt_cut(USES_REGS1) {
   return interrupt_prune((CELL  *)YENV[E_CB], NEXTOP(P,s) PASS_REGS);
}


static int interrupt_cut_t(USES_REGS1) {
  return interrupt_prune((CELL  *)YENV[E_CB], NEXTOP(P,s) PASS_REGS);
}


static int interrupt_cut_e(USES_REGS1) {
  return interrupt_prune((CELL  *)S[E_CB], NEXTOP(P,s) PASS_REGS);
}


static Int interrupt_commit_y(USES_REGS1) {
  return interrupt_prune((CELL  *)IntegerOfTerm(YENV[P->y_u.yps.y]), NEXTOP(P,yps) PASS_REGS);
}

static Int interrupt_commit_x(USES_REGS1) {
  return interrupt_prune((CELL  *)IntegerOfTerm(XREG(P->y_u.xps.x)), NEXTOP(P,xps) PASS_REGS);
}



static int interrupt_either(USES_REGS1) {
  int v;

 DEBUG_INTERRUPTS();
 if ( (v=check_alarm_fail_int(true PASS_REGS)) != INT_HANDLER_GO_ON) {
    return INT_HANDLER_FAIL;
  }
  if (Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return INT_HANDLER_RET_JMP;
  }
  if (PP)
    UNLOCKPE(1, PP);
  PP = P->y_u.Osblp.p0;
  /* find something to fool S */
  if ((v = code_overflow(YENV PASS_REGS)) != INT_HANDLER_GO_ON) {
    return v;
  }
  /* if ((v = stack_overflow(  PredRestoreRegs, YENV,
				   NEXTOP(P,Osblp), 0 PASS_REGS)) != INT_HANDLER_GO_ON) {
    return v;
    }*/

       return
         interrupt_wake_up(TermTrue,  NEXTOP(P,Osblp), 0 PASS_REGS) ?
         INT_HANDLER_RET_JMP:
       INT_HANDLER_FAIL;

}


static int interrupt_dexecute(USES_REGS1) {
  int v;
  PredEntry *pe;

  DEBUG_INTERRUPTS();
  if (PP)
    UNLOCKPE(1, PP);
  PP = P->y_u.Osbpp.p0;
  pe = P->y_u.Osbpp.p;
  if ((pe->PredFlags & (NoTracePredFlag | HiddenPredFlag)) &&
      Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return 2;
  }
  /* set S for next instructions */
  if ((v = code_overflow(YENV PASS_REGS)) != INT_HANDLER_GO_ON) {
    return v;
  }
  if ((v = stack_overflow(P->y_u.Osbpp.p, (CELL *)YENV[E_E], (yamop *)YENV[E_CP],
				  P->y_u.Osbpp.p->ArityOfPE PASS_REGS))  != INT_HANDLER_GO_ON) {
    return v;
  }
 /* first, deallocate */
  /* and now CREEP */
  YENV[E_CB] = (CELL)B;
  if (interrupt_wake_up(save_goal(pe), NULL, 0 PASS_REGS)) {
      execute_dealloc(PASS_REGS1);
      return 	 INT_HANDLER_RET_NEXT;
    }
    return
       INT_HANDLER_FAIL;

}

static void undef_goal(PredEntry *pe USES_REGS) {
  /* avoid trouble with undefined dynamic procedures */
  /* I assume they were not locked beforehand */
 if (pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag | MultiFileFlag) ) {
    /*   fprintf(stderr,"call to undefined Predicates %s ->",
IndicatorOfPred(pe)); Yap_DebugPlWriteln(ARG1); fputc(':', stderr);
    Yap_DebugPlWriteln(ARG2);
    fprintf(stderr,"  error handler not available, failing\n");
    */
#if defined(YAPOR) || defined(THREADS)
    UNLOCKPE(19, PP);
    PP = NULL;
#endif
    CalculateStackGap(PASS_REGS1);
        LOCAL_DoingUndefp = false;    P = FAILCODE;
    return;
  }
  if (UndefCode == NULL || UndefCode->OpcodeOfPred == UNDEF_OPCODE
      ) {
#if defined(YAPOR) || defined(THREADS)
    UNLOCKPE(19, PP);
    PP = NULL;
#endif
    CalculateStackGap(PASS_REGS1);
    if (Yap_UnknownFlag(CurrentModule?CurrentModule:TermProlog) == TermFail)
    P = FAILCODE;
    //else
      
      //Yap_RestartYap(1);
    //Yap_ThrowError( EVALUATION_ERROR_UNDEFINED, save_goal(pe PASS_REGS), NULL);
    return;
  }
 #if defined(YAPOR) || defined(THREADS)
  if (!PP) {
    PELOCK(19, pe);
    PP = pe;
  }
#endif
#if defined(YAPOR) || defined(THREADS)
  UNLOCKPE(19, PP);
  PP = NULL;
#endif
  ARG1 = save_goal(pe PASS_REGS);
// save_xregs(P PASS_REGS);
  ARG2 = MkVarTerm(); //Yap_getUnknownModule(Yap_GetModuleEntry(HR[0]));
#ifdef LOW_LEVEL_TRACER
  if (Yap_do_low_level_trace)
    low_level_trace(enter_pred, UndefCode, XREGS + 1);
#endif /* LOW_LEVEL_TRACE */
  P = UndefCode->CodeOfPred;
}

static void spy_goal(USES_REGS1) {
  PredEntry *pe = PredFromDefCode(P);

#if defined(YAPOR) || defined(THREADS)
  if (!PP) {
    PELOCK(14, pe);
    PP = pe;
  }
#endif
  if (!(pe->PredFlags & IndexedPredFlag) && pe->NOfClauses > 1) {
    /* update ASP before calling IPred */
    SET_ASP(YREG, E_CB * sizeof(CELL));
    Yap_IPred(pe, 0, CP);
    /* IPred can generate errors, it thus must get rid of the lock itself */
    if (P == FAILCODE) {
#if defined(YAPOR) || defined(THREADS)
      if (PP && !(PP->PredFlags & LogUpdatePredFlag)) {
        UNLOCKPE(20, pe);
        PP = NULL;
      }
#endif
      return;
    }
  }
  /* first check if we need to increase the counter */
  if ((pe->PredFlags & CountPredFlag)) {
    LOCK(pe->StatisticsForPred->lock);
    pe->StatisticsForPred->NOfEntries++;
    UNLOCK(pe->StatisticsForPred->lock);
    LOCAL_ReductionsCounter--;
    if (LOCAL_ReductionsCounter == 0 && LOCAL_ReductionsCounterOn) {
#if defined(YAPOR) || defined(THREADS)
      if (PP) {
        UNLOCKPE(20, pe);
        PP = NULL;
      }
#endif
      Yap_NilError(CALL_COUNTER_UNDERFLOW_EVENT, "");
      return;
    }
    LOCAL_PredEntriesCounter--;
    if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
#if defined(YAPOR) || defined(THREADS)
      if (PP) {
        UNLOCKPE(21, pe);
        PP = NULL;
      }
#endif
      Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT, "");
      return;
    }
    if ((pe->PredFlags & (CountPredFlag | ProfiledPredFlag | SpiedPredFlag)) ==
	CountPredFlag) {
#if defined(YAPOR) || defined(THREADS)
      if (PP) {
        UNLOCKPE(22, pe);
        PP = NULL;
      }
#endif
      P = pe->TrueCodeOfPred;
      return;
    }
  }
  /* standard profiler */
  if ((pe->PredFlags & ProfiledPredFlag)) {
    if (!pe->StatisticsForPred)
      Yap_initProfiler(pe);
    LOCK(pe->StatisticsForPred->lock);
    pe->StatisticsForPred->NOfEntries++;
    UNLOCK(pe->StatisticsForPred->lock);
    if (!(pe->PredFlags & SpiedPredFlag)) {
      P = pe->TrueCodeOfPred;
#if defined(YAPOR) || defined(THREADS)
      if (PP) {
        UNLOCKPE(23, pe);
        PP = NULL;
      }
#endif
      return;
    }
  }
#if defined(YAPOR) || defined(THREADS)
  if (PP) {
    UNLOCKPE(25, pe);
    PP = NULL;
  }
#endif
  ARG1 = save_goal(pe PASS_REGS);

  {
    PredEntry *pt0;
#if THREADS
    LOCK(GLOBAL_ThreadHandlesLock);
#endif
    pt0 = SpyCode;
    P_before_spy = P;
    P = pt0->CodeOfPred;
    /* for profiler */
#if THREADS
    UNLOCK(GLOBAL_ThreadHandlesLock);
#endif
#ifdef LOW_LEVEL_TRACER
    if (Yap_do_low_level_trace)
      low_level_trace(enter_pred, pt0, XREGS + 1);
#endif /* LOW_LEVEL_TRACE */
  }
}

Int Yap_absmi(int inp) {
  CACHE_REGS
#if BP_FREE
    /* some function might be using bp for an internal variable, it is the
       callee's responsability to save it */
    yamop *PCBACKUP = P1REG;
#endif

#ifdef LONG_LIVED_REGISTERS
  register CELL d0, d1;
  register CELL *pt0, *pt1;

#endif	/* LONG_LIVED_REGISTERS */

#ifdef SHADOW_P
  register yamop *PREG = P;
#endif /* SHADOW_P */

#ifdef SHADOW_CP
  register yamop *CPREG = CP;
#endif /* SHADOW_CP */

#ifdef SHADOW_HB
  register CELL *HBREG = HB;
#endif /* SHADOW_HB */

#ifdef SHADOW_Y
  register CELL *YREG = Yap_REGS.YENV_;
#endif /* SHADOW_Y */

#ifdef SHADOW_S
  register CELL *SREG = Yap_REGS.S_;
#else
#endif /* SHADOW_S */

  /* The indexing register so that we will not destroy ARG1 without
   * reason */
#define I_R (XREGS[0])

#if YAP_JIT
  Yap_ExpEnvP = &Yap_ExpEnv;
  static void *control_labels[] = {
				   &&fail,        &&NoStackCut,      &&NoStackCommitY,
				   &&NoStackCutT, &&NoStackEithcer,   &&NoStackExecute,
				   &&NoStackCall, &&NoStackDExecute, &&NoStackDeallocate,
				   &&notrailleft, &&NoStackFail,     &&NoStackCommitX};
  curtrace = NULL;
  curpreg = NULL;
  globalcurblock = NULL;
  ineedredefinedest = 0;
  NativeArea = (NativeContext *)malloc(sizeof(NativeContext));
  NativeArea->area.p = NULL;
  NativeArea->area.ok = NULL;
  NativeArea->area.pc = NULL;
#if YAP_STAT_PREDS
  NativeArea->area.nrecomp = NULL;
  NativeArea->area.compilation_time = NULL;
  NativeArea->area.native_size_bytes = NULL;
  NativeArea->area.trace_size_bytes = NULL;
  NativeArea->success = NULL;
  NativeArea->runs = NULL;
  NativeArea->t_runs = NULL;
#endif
  NativeArea->n = 0;
  IntermediatecodeArea =
    (IntermediatecodeContext *)malloc(sizeof(IntermediatecodeContext));
  IntermediatecodeArea->area.t = NULL;
  IntermediatecodeArea->area.ok = NULL;
  IntermediatecodeArea->area.isactive = NULL;
  IntermediatecodeArea->area.lastblock = NULL;
#if YAP_STAT_PREDS
  IntermediatecodeArea->area.profiling_time = NULL;
#endif
  IntermediatecodeArea->n = 0;
  nnexec = 0;
  l = 0;
#endif /* YAP_JIT */

#if USE_THREADED_CODE
  /************************************************************************/
  /*     Abstract Machine Instruction Address Table                       */
  /*  This must be declared inside the function. We use the asm directive */
  /* to make it available outside this function                           */
  /************************************************************************/
  static void *OpAddress[] = {
#define OPCODE(OP, TYPE) &&_##OP
#include "YapOpcodes.h"
#undef OPCODE
  };

#if YAP_JIT
  ExpEnv.config_struc.TOTAL_OF_OPCODES =
    sizeof(OpAddress) / (2 * sizeof(void *));
#endif

#endif /* USE_THREADED_CODE */

  /*static void* (*nat_glist_valx)(yamop**,yamop**,CELL**,void**,int*);

    if (nat_glist_valx == NULL) {
    nat_glist_valx =
    (void*(*)(yamop**,yamop**,CELL**,void**,int*))call_JIT_Compiler(J,
    _glist_valx);
    }*/

#ifdef SHADOW_REGS

  /* work with a local pointer to the registers */
  register REGSTORE *regp = &Yap_REGS;

#endif /* SHADOW_REGS */

#if PUSH_REGS

  /* useful on a X86 with -fomit-frame-pointer optimisation */
  /* The idea is to push REGS onto the X86 stack frame */

  /* first allocate local space */
  REGSTORE absmi_regs;
  REGSTORE *old_regs = Yap_regp;

#endif /* PUSH_REGS */

#ifdef BEAM
  CELL OLD_B = B;
  extern PredEntry *bpEntry;
  if (inp == -9000) {
#if PUSH_REGS
    old_regs = &Yap_REGS;
    init_absmi_regs(&absmi_regs);
#if THREADS
    regcache = Yap_regp LOCAL_PL_local_data_p->reg_cache = regcache;
#else
    Yap_regp = &absmi_regs;
#endif
#endif
    CACHE_A1();
    PREG = bpEntry->CodeOfPred;
    JMPNext(); /* go execute instruction at PREG */
  }

#endif

#if USE_THREADED_CODE
  /* absmadr */
  if (inp > 0) {
    Yap_ABSMI_OPCODES = OpAddress;
#if YAP_JIT
    Yap_ABSMI_ControlLabels = control_labels;
#endif
#if BP_FREE
    P1REG = PCBACKUP;
#endif
    return (0);
  }
#endif /* USE_THREADED_CODE */

#if PUSH_REGS			/*  */
  old_regs = &Yap_REGS;	/*  */

  /* done, let us now initialize this space */
  init_absmi_regs(&absmi_regs);

  /* the registers are all set up, let's swap */
#ifdef THREADS
  pthread_setspecific(Yap_yaamregs_key, (const void *)&absmi_regs);
  LOCAL_ThreadHandle.current_yaam_regs = &absmi_regs;
  regcache = &absmi_regs;
  // LOCAL_PL_local_data_p->reg_cache = regcache;
#else
  Yap_regp = &absmi_regs;
#endif
#undef Yap_REGS
#define Yap_REGS absmi_regs

#endif /* PUSH_REGS */

#ifdef SHADOW_REGS

  /* use regp as a copy of REGS */
  regp = &Yap_REGS;

#ifdef REGS
#undef REGS
#endif
#define REGS (*regp)

#endif /* SHADOW_REGS */

  setregs();

  CACHE_A1();

 reset_absmi:

  SP = SP0;

#if USE_THREADED_CODE
  //___androidlog_print(ANDROID_LOG_INFO, "YAP ", "%s",
  // Yap_op_names[Yap_op_from_opcode(PREG->opc)]);

  JMPNext(); /* go execute instruction at P          */

#else
  /* when we start we are not in write mode */

  {
    op_numbers opcode = _Ystop;
    op_numbers old_op;
#ifdef DEBUG_XX
    unsigned long ops_done;
#endif

    goto nextop;

  nextop_write:

    old_op = opcode;
    opcode = PREG->y_u.o.opcw;
    goto op_switch;

  nextop:

    old_op = opcode;
    opcode = PREG->opc;

  op_switch:

#ifdef ANALYST
    GLOBAL_opcount[opcode]++;
    GLOBAL_2opcount[old_op][opcode]++;
#ifdef DEBUG_XX
    ops_done++;
    /*    if (B->cp_b > 0x103fff90)
	  fprintf(stderr,"(%ld) doing %s, done %s, B is %p, HB is %p, H is %p\n",
	  ops_done,Yap_op_names[opcode],Yap_op_names[old_op],B,B->cp_h,HR);*/
#endif
#endif /* ANALYST */

    switch (opcode) {
#endif /* USE_THREADED_CODE */

#if !OS_HANDLES_TR_OVERFLOW
    notrailleft:
      /* if we are within indexing code, the system may have to
       * update a S */
      {
	CELL cut_b;

#ifdef SHADOW_S
	S = SREG;
#endif
	/* YREG was pointing to where we were going to build the
	 * next choice-point. The stack shifter will need to know this
	 * to move the local stack */
	SET_ASP(YREG, E_CB * sizeof(CELL));
	cut_b = LCL0 - (CELL *)(ASP[E_CB]);
	saveregs();
	if (!Yap_growtrail(0, false)) {
	  Yap_ThrowError(RESOURCE_ERROR_TRAIL,
		       TermNil,
		       "YAP failed to reserve %ld bytes in growtrail",
		       sizeof(CELL) * K16);
	  setregs();
	  FAIL();
	}
	setregs();
#ifdef SHADOW_S
	SREG = S;
#endif
	if (SREG == ASP) {
	  SREG[E_CB] = (CELL)(LCL0 - cut_b);
	}
      }
      goto reset_absmi;

#endif /* OS_HANDLES_TR_OVERFLOW */

      // move instructions to separate file
      // so that they are easier to analyse.

#include "absmi_insts.h"

#if !USE_THREADED_CODE
    default:
      saveregs();
      Yap_Error(SYSTEM_ERROR_INTERNAL, MkIntegerTerm(opcode),
		"trying to execute invalid YAAM instruction %d", opcode);
      setregs();
      FAIL();
    }
  }
#else

#if PUSH_REGS
  restore_absmi_regs(old_regs);
#endif

#if BP_FREE
  P1REG = PCBACKUP;
#endif

  return (0);
#endif
}

/* dummy function that is needed for profiler */
int Yap_absmiEND(void) { return 1; }

/// @}

