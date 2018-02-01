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

+ Execution of <em>deterministic</em> programs often
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

This section is about the YAP implementation, and is mostly of
interest to hackers.

@{

@defgroup Emulator The Abstract Machine Emulator
@ingroup Implementation

 */

/// code belongs to the emulator
#define IN_ABSMI_C 1
#define _INATIVE 1
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

#ifdef COROUTINING
/*
  Imagine we are interrupting the execution, say, because we have a spy
   point or because we have goals to wake up. This routine saves the current
   live temporary registers into a structure pointed to by register ARG1.
   The registers are then recovered by a nasty builtin
   called
*/
static Term push_live_regs(yamop *pco) {
  CACHE_REGS
  CELL *lab = (CELL *)(pco->y_u.l.l);
  CELL max = lab[0];
  CELL curr = lab[1];
  Term tp = MkIntegerTerm((Int)pco);
  Term tcp = MkIntegerTerm((Int)CP);
  Term tenv = MkIntegerTerm((Int)(LCL0 - ENV));
  Term tyenv = MkIntegerTerm((Int)(LCL0 - YENV));
  CELL *start = HR;
  Int tot = 0;

  HR++;
  *HR++ = tp;
  *HR++ = tcp;
  *HR++ = tenv;
  *HR++ = tyenv;
  tot += 4;
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
          /* bind it, in case it is a local variable */
          if (pt0 <= HR) {
            /* variable is safe */
            HR[1] = (CELL)pt0;
          } else {
            d1 = Unsigned(HR + 1);
            RESET_VARIABLE(HR + 1);
            Bind_Local(pt0, d1);
          }
        }
        HR += 2;
      }
      curr >>= 1;
    }
    start[0] = (CELL)Yap_MkFunctor(AtomTrue, tot);
    return (AbsAppl(start));
  }
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
    return CONT;
  }
#endif
  if (Yap_get_signal(YAP_FAIL_SIGNAL)) {
    return false;
  }
  if (!Yap_has_a_signal()) {
    /* no need to look into GC */
    CalculateStackGap(PASS_REGS1);
  }
  // fail even if there are more signals, they will have to be dealt later.
  return -1;
}

static int stack_overflow(PredEntry *pe, CELL *env, yamop *cp,
                          arity_t nargs USES_REGS) {
  if (Unsigned(YREG) - Unsigned(HR) < StackGap(PASS_REGS1) ||
      Yap_get_signal(YAP_STOVF_SIGNAL)) {
    S = (CELL *)pe;
    if (!Yap_locked_gc(nargs, env, cp)) {
      Yap_NilError(RESOURCE_ERROR_STACK, LOCAL_ErrorMessage);
      return 0;
    }
    return 1;
  }
  return -1;
}

static int code_overflow(CELL *yenv USES_REGS) {
  if (Yap_get_signal(YAP_CDOVF_SIGNAL)) {
    CELL cut_b = LCL0 - (CELL *)(yenv[E_CB]);

    /* do a garbage collection first to check if we can recover memory */
    if (!Yap_locked_growheap(false, 0, NULL)) {
      Yap_NilError(RESOURCE_ERROR_HEAP, "YAP failed to grow heap: %s",
                   LOCAL_ErrorMessage);
      return 0;
    }
    CACHE_A1();
    if (yenv == ASP) {
      yenv[E_CB] = (CELL)(LCL0 - cut_b);
    }
    return 1;
  }
  return -1;
}

static int interrupt_handler(PredEntry *pe USES_REGS) {

  //  printf("D %lx %p\n", LOCAL_ActiveSignals, P);
  /* tell whether we can creep or not, this is hard because we will
     lose the info RSN
  */
  BEGD(d0);
  d0 = pe->ArityOfPE;
  if (d0 == 0) {
    HR[1] = MkAtomTerm((Atom)pe->FunctorOfPred);
  } else {
    HR[d0 + 2] = AbsAppl(HR);
    *HR = (CELL)pe->FunctorOfPred;
    HR++;
    BEGP(pt1);
    pt1 = XREGS + 1;
    for (; d0 > 0; --d0) {
      BEGD(d1);
      BEGP(pt0);
      pt0 = pt1;
      d1 = *pt0;
      deref_head(d1, creep_unk);
    creep_nonvar:
      /* just copy it to the heap */
      pt1++;
      *HR++ = d1;
      continue;

      derefa_body(d1, pt0, creep_unk, creep_nonvar);
      if (pt0 <= HR) {
        /* variable is safe */
        *HR++ = (CELL)pt0;
        pt1++;
      } else {
        /* bind it, in case it is a local variable */
        d1 = Unsigned(HR);
        RESET_VARIABLE(HR);
        pt1++;
        HR += 1;
        Bind_Local(pt0, d1);
      }
      ENDP(pt0);
      ENDD(d1);
    }
    ENDP(pt1);
  }
  ENDD(d0);
  HR[0] = Yap_Module_Name(pe);
  ARG1 = (Term)AbsPair(HR);

  HR += 2;
#ifdef COROUTINING
  if (Yap_get_signal(YAP_WAKEUP_SIGNAL)) {
    CalculateStackGap(PASS_REGS1);
    ARG2 = Yap_ListOfWokenGoals();
    pe = WakeUpCode;
    /* no more goals to wake up */
    Yap_UpdateTimedVar(LOCAL_WokenGoals, TermNil);
  } else
#endif
  {
    CalculateStackGap(PASS_REGS1);
    pe = CreepCode;
  }
  P = pe->CodeOfPred;
#ifdef LOW_LEVEL_TRACER
  if (Yap_do_low_level_trace)
    low_level_trace(enter_pred, pe, XREGS + 1);
#endif /* LOW_LEVEL_TRACE */
  /* for profiler */
  CACHE_A1();
  return true;
}

// interrupt handling code that sets up the case when we do not have
// a guaranteed environment.
static int safe_interrupt_handler(PredEntry *pe USES_REGS) {
  CELL *npt = HR;

  //  printf("D %lx %p\n", LOCAL_ActiveSignals, P);
  /* tell whether we can creep or not, this is hard because we will
     lose the info RSN
  */
  BEGD(d0);
  S = (CELL *)pe;
  d0 = pe->ArityOfPE;
  if (d0 == 0) {
    HR[1] = MkAtomTerm((Atom)pe->FunctorOfPred);
  } else {
    HR[d0 + 2] = AbsAppl(HR);
    HR += d0 + 1 + 2;
    *npt++ = (CELL)pe->FunctorOfPred;
    BEGP(pt1);
    pt1 = XREGS + 1;
    for (; d0 > 0; --d0) {
      BEGD(d1);
      d1 = *pt1;
    loop:
      if (!IsVarTerm(d1)) {
        /* just copy it to the heap */
        pt1++;
        *npt++ = d1;
      } else {
        if (VarOfTerm(d1) < H0 || VarOfTerm(d1) > HR) {
          d1 = Deref(d1);
          if (VarOfTerm(d1) < H0 || VarOfTerm(d1) > HR) {
            Term v = MkVarTerm();
            YapBind(VarOfTerm(d1), v);
          } else {
            goto loop;
          }
        } else {
          *npt++ = d1;
        }
      }
      ENDD(d1);
    }
    ENDP(pt1);
  }
  ENDD(d0);
  npt[0] = Yap_Module_Name(pe);
  ARG1 = AbsPair(npt);

  HR += 2;
#ifdef COROUTINING
  if (Yap_get_signal(YAP_WAKEUP_SIGNAL)) {
    CalculateStackGap(PASS_REGS1);
    ARG2 = Yap_ListOfWokenGoals();
    pe = WakeUpCode;
    /* no more goals to wake up */
    Yap_UpdateTimedVar(LOCAL_WokenGoals, TermNil);
  } else
#endif
  {
    CalculateStackGap(PASS_REGS1);
    pe = CreepCode;
  }
  // allocate and fill out an environment
  YENV = ASP;
  CACHE_Y_AS_ENV(YREG);
  ENV_YREG[E_CP] = (CELL)CP;
  ENV_YREG[E_E] = (CELL)ENV;
#ifdef DEPTH_LIMIT
  ENV_YREG[E_DEPTH] = DEPTH;
#endif /* DEPTH_LIMIT */
  ENV = ENV_YREG;
  ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size(CP));
  WRITEBACK_Y_AS_ENV();
  ENDCACHE_Y_AS_ENV();
  CP = P;
  P = pe->CodeOfPred;
#ifdef DEPTH_LIMIT
  if (DEPTH <= MkIntTerm(1)) { /* I assume Module==0 is primitives */
    if (pe->ModuleOfPred) {
      if (DEPTH == MkIntTerm(0))
        return false;
      else
        DEPTH = RESET_DEPTH();
    }
  } else if (pe->ModuleOfPred) {
    DEPTH -= MkIntConstant(2);
  }
#endif /* DEPTH_LIMIT */
  return true;
}

static int interrupt_handlerc(PredEntry *pe USES_REGS) {
  /* do creep in call                                     */
  ENV = YENV;
  CP = NEXTOP(P, Osbpp);
  YENV = (CELL *)(((char *)YENV) + P->y_u.Osbpp.s);
#ifdef FROZEN_STACKS
  {
    choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
    if (YENV > (CELL *)top_b || YENV < HR)
      YENV = (CELL *)top_b;
#else
    if (YENV > (CELL *)top_b)
      YENV = (CELL *)top_b;
#endif /* YAPOR_SBA */
    else
      YENV = YENV + ENV_Size(CP);
  }
#else
  if (YENV > (CELL *)B)
    YENV = (CELL *)B;
  else
    /* I am not sure about this */
    YENV = YENV + ENV_Size(CP);
#endif /* FROZEN_STACKS */
  /* setup GB */
  YENV[E_CB] = (CELL)B;
  return interrupt_handler(pe PASS_REGS);
}

static int interrupt_handler_either(Term t_cut, PredEntry *pe USES_REGS) {
  int rc;

  ARG1 = push_live_regs(NEXTOP(P, Osbpp));
#ifdef FROZEN_STACKS
  {
    choiceptr top_b = PROTECT_FROZEN_B(B);
// protect registers before we mess about.
// recompute YENV and get ASP
#ifdef YAPOR_SBA
    if (YENV > (CELL *)top_b || YENV < HR)
      YENV = (CELL *)top_b;
#else
    if (YENV > (CELL *)top_b)
      YENV = (CELL *)top_b;
#endif /* YAPOR_SBA */
    else
      YENV = YENV + ENV_Size(CP);
  }
#else
  if (YENV > (CELL *)B)
    YENV = (CELL *)B;
#endif /* FROZEN_STACKS */
  P = NEXTOP(P, Osbpp);
  // should we cut? If t_cut == INT(0) no
  ARG2 = t_cut;
  // ASP
  SET_ASP(YENV, E_CB * sizeof(CELL));
  // do the work.
  rc = safe_interrupt_handler(pe PASS_REGS);
  return rc;
}

/* to trace interrupt calls */
// #define DEBUG_INTERRUPTS 1

#ifdef DEBUG_INTERRUPTS
static int trace_interrupts = true;
#endif

static int interrupt_fail(USES_REGS1) {
#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d:  (YENV=%p ENV=%p ASP=%p)\n",
            worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal,
            __FUNCTION__, __LINE__, YENV, ENV, ASP);
#endif
  check_alarm_fail_int(false PASS_REGS);
  /* don't do debugging and stack expansion here: space will
     be recovered. automatically by fail, so
     better wait.
  */
  if (Yap_has_signal(YAP_CREEP_SIGNAL)) {
    return false;
  }
  if (Yap_has_signal(YAP_CDOVF_SIGNAL)) {
    return false;
  }
  /* make sure we have the correct environment for continuation */
  ENV = B->cp_env;
  YENV = (CELL *)B;
  return interrupt_handler(RepPredProp(Yap_GetPredPropByAtom(AtomFail, 0))
                               PASS_REGS);
}

static int interrupt_execute(USES_REGS1) {
  int v;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d: (YENV=%p ENV=%p ASP=%p)\n", worker_id,
            LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, __FUNCTION__,
            __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(true PASS_REGS)) >= 0) {
    return v;
  }
  if (PP)
    UNLOCKPE(1, PP);
  PP = P->y_u.Osbpp.p0;
  if ((P->y_u.Osbpp.p->PredFlags & (NoTracePredFlag | HiddenPredFlag)) &&
      Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return 2;
  }
  SET_ASP(YENV, E_CB * sizeof(CELL));
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  if ((v = stack_overflow(P->y_u.Osbpp.p, ENV, CP,
                          P->y_u.Osbpp.p->ArityOfPE PASS_REGS)) >= 0) {
    return v;
  }
  return interrupt_handler(P->y_u.Osbpp.p PASS_REGS);
}

static int interrupt_call(USES_REGS1) {
  int v;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d:  (YENV=%p ENV=%p ASP=%p)\n",
            worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal,
            __FUNCTION__, __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(true PASS_REGS)) >= 0) {
    return v;
  }
  if (PP)
    UNLOCKPE(1, PP);
  PP = P->y_u.Osbpp.p0;
  if (Yap_only_has_signal(YAP_CREEP_SIGNAL) &&
      (P->y_u.Osbpp.p->PredFlags & (NoTracePredFlag | HiddenPredFlag))) {
    return 2;
  }
  SET_ASP(YENV, P->y_u.Osbpp.s);
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  if ((v = stack_overflow(P->y_u.Osbpp.p, YENV, NEXTOP(P, Osbpp),
                          P->y_u.Osbpp.p->ArityOfPE PASS_REGS)) >= 0) {
    return v;
  }
  return interrupt_handlerc(P->y_u.Osbpp.p PASS_REGS);
}

static int interrupt_pexecute(PredEntry *pen USES_REGS) {
  int v;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d:  (YENV=%p ENV=%p ASP=%p)\n",
            worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal,
            __FUNCTION__, __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(2 PASS_REGS)) >= 0) {
    return v;
  }
  if (PP)
    UNLOCKPE(1, PP);
  PP = NULL;
  if (Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return 2; /* keep on creeping */
  }
  SET_ASP(YENV, E_CB * sizeof(CELL));
  /* setup GB */
  YENV[E_CB] = (CELL)B;
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  if ((v = stack_overflow(pen, ENV, NEXTOP(P, Osbmp),
                          pen->ArityOfPE PASS_REGS)) >= 0) {
    return v;
  }
  CP = NEXTOP(P, Osbmp);
  return interrupt_handler(pen PASS_REGS);
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
    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size(CP));
#endif /* FROZEN_STACKS */
  YENV = ENVYREG;
  P = NEXTOP(P, p);
}

/* don't forget I cannot creep at deallocate (where to?) */
/* also, this is unusual in that I have already done deallocate,
   so I don't need to redo it.
 */
static int interrupt_deallocate(USES_REGS1) {
  int v;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d (YENV=%p ENV=%p ASP=%p)\n", worker_id,
            LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, __FUNCTION__,
            __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(true PASS_REGS)) >= 0) {
    return v;
  }
  /*
     don't do a creep here; also, if our instruction is followed by
     a execute_c, just wait a bit more */
  if (Yap_only_has_signals(YAP_CREEP_SIGNAL, YAP_WAKEUP_SIGNAL) ||
      /* keep on going if there is something else */
      (P->opc != Yap_opcode(_procceed) && P->opc != Yap_opcode(_cut_e))) {
    execute_dealloc(PASS_REGS1);
    return 1;
  } else {
    CELL cut_b = LCL0 - (CELL *)(S[E_CB]);

    if (PP)
      UNLOCKPE(1, PP);
    PP = PREVOP(P, p)->y_u.p.p;
    ASP = YENV + E_CB;
    /* cut_e */
    SET_ASP(YENV, E_CB * sizeof(CELL));
    if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
      return v;
    }
    if (Yap_has_a_signal()) {
      PredEntry *pe;

      if (Yap_op_from_opcode(P->opc) == _cut_e) {
        /* followed by a cut */
        ARG1 = MkIntegerTerm(LCL0 - (CELL *)S[E_CB]);
        pe = RepPredProp(Yap_GetPredPropByFunc(FunctorCutBy, 1));
      } else {
        pe = RepPredProp(Yap_GetPredPropByAtom(AtomTrue, 0));
      }
      // deallocate moves P one step forward.
      bool rc = interrupt_handler(pe PASS_REGS);
      P = NEXTOP(P,p);
      return rc;
    }
    if (!Yap_locked_gc(0, ENV, YESCODE)) {
      Yap_NilError(RESOURCE_ERROR_STACK, LOCAL_ErrorMessage);
    }
    S = ASP;
    S[E_CB] = (CELL)(LCL0 - cut_b);
  }
  return 1;
}

static int interrupt_cut(USES_REGS1) {
  Term t_cut = MkIntegerTerm(LCL0 - (CELL *)YENV[E_CB]);
  int v;
#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d (YENV=%p ENV=%p ASP=%p)\n", worker_id,
            LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, __FUNCTION__,
            __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(2 PASS_REGS)) >= 0) {
    return v;
  }
  if (!Yap_has_a_signal() ||
      Yap_only_has_signals(YAP_CDOVF_SIGNAL, YAP_CREEP_SIGNAL)) {
    return 2;
  }
  /* find something to fool S */
  P = NEXTOP(P, s);
  return interrupt_handler_either(t_cut, PredRestoreRegs PASS_REGS);
}

static int interrupt_cut_t(USES_REGS1) {
  Term t_cut = MkIntegerTerm(LCL0 - (CELL *)YENV[E_CB]);
  int v;
#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d: (YENV=%p ENV=%p ASP=%p)\n", worker_id,
            LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, __FUNCTION__,
            __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(2 PASS_REGS)) >= 0) {
    return v;
  }
  if (!Yap_has_a_signal() ||
      Yap_only_has_signals(YAP_CDOVF_SIGNAL, YAP_CREEP_SIGNAL)) {
    return 2;
  }
  /* find something to fool S */
  P = NEXTOP(P, s);
  return interrupt_handler_either(t_cut, PredRestoreRegs PASS_REGS);
}

static int interrupt_cut_e(USES_REGS1) {
  Term t_cut = MkIntegerTerm(LCL0 - (CELL *)S[E_CB]);
  int v;
#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d: (YENV=%p ENV=%p ASP=%p)\n", worker_id,
            LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, __FUNCTION__,
            __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(2 PASS_REGS)) >= 0) {
    return v;
  }
  if (!Yap_only_has_signals(YAP_CDOVF_SIGNAL, YAP_CREEP_SIGNAL)) {
    return 2;
  }
  /* find something to fool S */
  P = NEXTOP(P, s);
  return interrupt_handler_either(t_cut, PredRestoreRegs PASS_REGS);
}

static int interrupt_commit_y(USES_REGS1) {
  int v;
  Term t_cut = YENV[P->y_u.yps.y];

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d: (YENV=%p ENV=%p ASP=%p)\n", worker_id,
            LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, __FUNCTION__,
            __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(2 PASS_REGS)) >= 0) {
    return v;
  }
  if (!Yap_has_a_signal() ||
      Yap_only_has_signals(YAP_CDOVF_SIGNAL, YAP_CREEP_SIGNAL)) {
    return 2;
  }
  /* find something to fool S */
  P = NEXTOP(P, yps);
  return interrupt_handler_either(t_cut, PredRestoreRegs PASS_REGS);
}

static int interrupt_commit_x(USES_REGS1) {
  int v;
  Term t_cut = XREG(P->y_u.xps.x);

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s:%d (YENV=%p ENV=%p ASP=%p)\n", worker_id,
            LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, __FUNCTION__,
            __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(2 PASS_REGS)) >= 0) {
    return v;
  }
  if (Yap_only_has_signals(YAP_CDOVF_SIGNAL, YAP_CREEP_SIGNAL)) {
    return 2;
  }
  if (PP)
    UNLOCKPE(1, PP);
  PP = P->y_u.xps.p0;
  /* find something to fool S */
  if (P->opc == Yap_opcode(_fcall)) {
    /* fill it up */
    CACHE_Y_AS_ENV(YREG);
    ENV_YREG[E_CP] = (CELL)CP;
    ENV_YREG[E_E] = (CELL)ENV;
#ifdef DEPTH_LIMIT
    ENV_YREG[E_DEPTH] = DEPTH;
#endif /* DEPTH_LIMIT */
    ENDCACHE_Y_AS_ENV();
  }
  P = NEXTOP(P, xps);
  return interrupt_handler_either(t_cut, PredRestoreRegs PASS_REGS);
}

static int interrupt_either(USES_REGS1) {
  int v;

#ifdef DEBUGX
  // if (trace_interrupts)
  fprintf(stderr, "[%d] %s:%d:  (YENV=%p ENV=%p ASP=%p)\n", worker_id,
          __FUNCTION__, __LINE__, YENV, ENV, ASP);
#endif
  if ((v = check_alarm_fail_int(2 PASS_REGS)) >= 0) {
    return v;
  }
  if (Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return 2;
  }
  if (PP)
    UNLOCKPE(1, PP);
  PP = P->y_u.Osblp.p0;
  /* find something to fool S */
  SET_ASP(YENV, P->y_u.Osbpp.s);
  if (ASP > (CELL *)PROTECT_FROZEN_B(B))
    ASP = (CELL *)PROTECT_FROZEN_B(B);
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  // P = NEXTOP(P, Osblp);
  if ((v = stack_overflow(
           RepPredProp(Yap_GetPredPropByFunc(FunctorRestoreRegs1, 0)), YENV,
           NEXTOP(P, Osblp), 0 PASS_REGS)) >= 0) {
    // P = PREVOP(P, Osblp);
    return v;
  }
  // P = PREVOP(P, Osblp);
  return interrupt_handler_either(
      MkIntTerm(0),
      RepPredProp(Yap_GetPredPropByFunc(FunctorRestoreRegs1, 0)) PASS_REGS);
}

static int interrupt_dexecute(USES_REGS1) {
  int v;
  PredEntry *pe;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts)
    fprintf(stderr, "[%d] %lu--%lu %s/%d (YENV=%p ENV=%p ASP=%p)\n", worker_id,
            LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, __FUNCTION__,
            __LINE__, YENV, ENV, ASP);
#endif
  if (PP)
    UNLOCKPE(1, PP);
  PP = P->y_u.Osbpp.p0;
  pe = P->y_u.Osbpp.p;
  if ((pe->PredFlags & (NoTracePredFlag | HiddenPredFlag)) &&
      Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return 2;
  }
  /* set S for next instructions */
  ASP = YENV + E_CB;
  if (ASP > (CELL *)PROTECT_FROZEN_B(B))
    ASP = (CELL *)PROTECT_FROZEN_B(B);
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  if ((v = stack_overflow(P->y_u.Osbpp.p, (CELL *)YENV[E_E], (yamop *)YENV[E_CP],
                          P->y_u.Osbpp.p->ArityOfPE PASS_REGS)) >= 0) {
    return v;
  }
  /* first, deallocate */
  CP = (yamop *)YENV[E_CP];
  ENV = YENV = (CELL *)YENV[E_E];
#ifdef DEPTH_LIMIT
  YENV[E_DEPTH] = DEPTH;
#endif /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
  {
    choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
    if (YENV > (CELL *)top_b || YENV < HR)
      YENV = (CELL *)top_b;
#else
    if (YENV > (CELL *)top_b)
      YENV = (CELL *)top_b;
#endif /* YAPOR_SBA */
    else
      YENV = (CELL *)((CELL)YENV + ENV_Size(CPREG));
  }
#else
  if (YENV > (CELL *)B) {
    YENV = (CELL *)B;
  } else {
    YENV = (CELL *)((CELL)YENV + ENV_Size(CPREG));
  }
#endif /* FROZEN_STACKS */
  /* setup GB */
  YENV[E_CB] = (CELL)B;

  /* and now CREEP */
  return interrupt_handler(pe PASS_REGS);
}

static void undef_goal(USES_REGS1) {
  PredEntry *pe = PredFromDefCode(P);
    CELL *b;
    CELL *b0;
    
  BEGD(d0);
/* avoid trouble with undefined dynamic procedures */
/* I assume they were not locked beforehand */
#if defined(YAPOR) || defined(THREADS)
  if (!PP) {
    PELOCK(19, pe);
    PP = pe;
  }
#endif
  if (UndefCode == NULL || UndefCode->OpcodeOfPred == UNDEF_OPCODE) {
    fprintf(stderr,"call to undefined Predicates %s ->", IndicatorOfPred(pe));
    Yap_DebugPlWriteln(ARG1);
    fputc(':', stderr);
    Yap_DebugPlWriteln(ARG2);
    fprintf(stderr,"  error handler not available, failing\n");
#if defined(YAPOR) || defined(THREADS)
    UNLOCKPE(19, PP);
    PP = NULL;
#endif
    CalculateStackGap(PASS_REGS1);
    P = FAILCODE;
    return;
  }
  if (pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag | MultiFileFlag) ) {
#if defined(YAPOR) || defined(THREADS)
    UNLOCKPE(19, PP);
    PP = NULL;
#endif
    CalculateStackGap(PASS_REGS1);
    P = FAILCODE;
    return;
  }
#if defined(YAPOR) || defined(THREADS)
  UNLOCKPE(19, PP);
  PP = NULL;
#endif
  d0 = pe->ArityOfPE;
  if (pe->ModuleOfPred == PROLOG_MODULE) {
    if (CurrentModule == PROLOG_MODULE)
      HR[0] = MkAtomTerm(Yap_LookupAtom("prolog"));
    else
      HR[0] = CurrentModule;
  } else {
    HR[0] = Yap_Module_Name(pe);
  }
  b = b0 = HR;
  HR += 2;
  if (d0 == 0) {
    b[1] = MkAtomTerm((Atom)(pe->FunctorOfPred));
  } else {
    b[1] = AbsAppl(b+2);
    *HR++ = (CELL)pe->FunctorOfPred;
    b += 3;
    HR += d0;
    BEGP(pt1);
    pt1 = XREGS + 1;
    for (; d0 > 0; --d0) {
      BEGD(d1);
      BEGP(pt0);
      pt0 = pt1++;
      d1 = *pt0;
      deref_head(d1, undef_unk);
    undef_nonvar:
      /* just copy it to the heap */
      *b++ = d1;
      continue;

      derefa_body(d1, pt0, undef_unk, undef_nonvar);
      if (pt0 <= HR) {
        /* variable is safe */
        *b++ = (CELL)pt0;
      } else {
        /* bind it, in case it is a local variable */
        d1 = Unsigned(HR);
        RESET_VARIABLE(HR);
        HR += 1;
        Bind_Local(pt0, d1);
      }
      ENDP(pt0);
      ENDD(d1);
    }
    ENDP(pt1);
  }
  ENDD(d0);
  ARG1 = AbsPair(b0);
  ARG2 = Yap_getUnknownModule(Yap_GetModuleEntry(b0[0]));
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
  BEGD(d0);
  if (!(pe->PredFlags & IndexedPredFlag) && pe->cs.p_code.NOfClauses > 1) {
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
      P = pe->cs.p_code.TrueCodeOfPred;
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
      P = pe->cs.p_code.TrueCodeOfPred;
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

  d0 = pe->ArityOfPE;
  /* save S for ModuleName */
  if (d0 == 0) {
    HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred));
  } else {
    *HR = (CELL)pe->FunctorOfPred;
    HR[d0 + 2] = AbsAppl(HR);
    HR++;
    BEGP(pt1);
    pt1 = XREGS + 1;
    for (; d0 > 0; --d0) {
      BEGD(d1);
      BEGP(pt0);
      pt0 = pt1++;
      d1 = *pt0;
      deref_head(d1, dospy_unk);
    dospy_nonvar:
      /* just copy it to the heap */
      *HR++ = d1;
      continue;

      derefa_body(d1, pt0, dospy_unk, dospy_nonvar);
      if (pt0 <= HR) {
        /* variable is safe */
        *HR++ = (CELL)pt0;
      } else {
        /* bind it, in case it is a local variable */
        d1 = Unsigned(HR);
        RESET_VARIABLE(HR);
        HR += 1;
        Bind_Local(pt0, d1);
      }
      ENDP(pt0);
      ENDD(d1);
    }
    ENDP(pt1);
  }
  ENDD(d0);
  HR[0] = Yap_Module_Name(pe);

  ARG1 = (Term)AbsPair(HR);
  HR += 2;
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

#endif /* LONG_LIVED_REGISTERS */

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
#define SREG S
#endif /* SHADOW_S */

/* The indexing register so that we will not destroy ARG1 without
 * reason */
#define I_R (XREGS[0])

#if YAP_JIT
  Yap_ExpEnvP = &Yap_ExpEnv;
  static void *control_labels[] = {
      &&fail,        &&NoStackCut,      &&NoStackCommitY,
      &&NoStackCutT, &&NoStackEither,   &&NoStackExecute,
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

#if PUSH_REGS
  old_regs = &Yap_REGS;

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
      Yap_NilError(RESOURCE_ERROR_TRAIL,
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

/// @}
