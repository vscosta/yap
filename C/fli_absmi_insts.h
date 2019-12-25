/************************************************************************\
 *    Call C predicates instructions                                   *
\************************************************************************/

#ifdef INDENT_CODE
{
  {
    {
#endif /* INDENT_CODE */

      BOp(call_cpred, Osbpp);

      check_trail(TR);
      if (!(PREG->y_u.Osbpp.p->PredFlags &
            (SafePredFlag | NoTracePredFlag | HiddenPredFlag))) {
        CACHE_Y_AS_ENV(YREG);
        check_stack(NoStackCCall, HR);
        ENDCACHE_Y_AS_ENV();
      }
    do_c_call :
#ifdef FROZEN_STACKS
    {
      choiceptr top_b = PROTECT_FROZEN_B(B);

#ifdef YAPOR_SBA
      if (YREG > (CELL *)top_b || YREG < HR)
        ASP = (CELL *)top_b;
#else
      if (YREG > (CELL *)top_b)
        ASP = (CELL *)top_b;
#endif /* YAPOR_SBA */
      else
        ASP = (CELL *)(((char *)YREG) + PREG->y_u.Osbpp.s);
    }
#else
      SET_ASP(YREG, PREG->y_u.Osbpp.s);
/* for slots to work */
#endif /* FROZEN_STACKS */
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace)
        low_level_trace(enter_pred, PREG->y_u.Osbpp.p, XREGS + 1);
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      CPredicate f = PREG->y_u.Osbpp.p->cs.f_code;
      PREG = NEXTOP(PREG, Osbpp);
      saveregs();
      d0 = f(PASS_REGS1);
      setregs();
#ifdef SHADOW_S
      SREG = Yap_REGS.S_;
#endif
      if (!d0) {
        FAIL();
      }
      CACHE_A1();
      ENDD(d0);
      JMPNext();

    NoStackCCall:
      PROCESS_INTERRUPT(interrupt_call, do_c_call, PREG->y_u.Osbpp.s);

      ENDBOp();

      /* execute     Label               */
      BOp(execute_cpred, Osbpp);
      check_trail(TR);
      {
        PredEntry *pt0;

        BEGD(d0);
        CACHE_Y_AS_ENV(YREG);
#ifndef NO_CHECKING
        check_stack(NoStackExecuteC, HR);
      do_executec :
#endif
#ifdef FROZEN_STACKS
      {
        choiceptr top_b = PROTECT_FROZEN_B(B);

#ifdef YAPOR_SBA
        if (YREG > (CELL *)top_b || YREG < HR)
          ASP = (CELL *)top_b;
#else
        if (YREG > (CELL *)top_b)
          ASP = (CELL *)top_b;
#endif /* YAPOR_SBA */
        else
          ASP = YREG + E_CB;
      }
#else
        SET_ASP(YREG, E_CB * sizeof(CELL));
/* for slots to work */
#endif /* FROZEN_STACKS */
        pt0 = PREG->y_u.Osbpp.p;
#ifdef LOW_LEVEL_TRACER
        if (Yap_do_low_level_trace) {
          low_level_trace(enter_pred, pt0, XREGS + 1);
        }
#endif /* LOW_LEVEL_TRACE */
        CACHE_A1();
        BEGD(d0);
        d0 = (CELL)B;
        /* for profiler */
        save_pc();
        ENV_YREG[E_CB] = d0;
        ENDD(d0);
#ifdef DEPTH_LIMIT
        if (DEPTH <= MkIntTerm(1)) { /* I assume Module==0 is prolog */
          if (pt0->ModuleOfPred) {
            if (DEPTH == MkIntTerm(0)) {
              FAIL();
            } else {
              DEPTH = RESET_DEPTH();
            }
          }
        } else if (pt0->ModuleOfPred) {
          DEPTH -= MkIntConstant(2);
        }
#endif /* DEPTH_LIMIT */
        /* now call C-Code */
        {
          CPredicate f = PREG->y_u.Osbpp.p->cs.f_code;
          yamop *oldPREG = PREG;
          saveregs();
                      d0 = f(PASS_REGS1);
          setregs();
#ifdef SHADOW_S
          SREG = Yap_REGS.S_;
#endif
          if (!d0) {
            FAIL();
          }
          if (oldPREG == PREG) {
            /* we did not update PREG */
            /* we can proceed */
            PREG = CPREG;
            ENV_YREG = ENV;
#ifdef DEPTH_LIMIT
            DEPTH = ENV_YREG[E_DEPTH];
#endif
            WRITEBACK_Y_AS_ENV();
          } else {
            /* call the new code  */
            CACHE_A1();
          }
        }
        JMPNext();
        ENDCACHE_Y_AS_ENV();
        ENDD(d0);
      }

    NoStackExecuteC:
      PROCESS_INTERRUPT(interrupt_execute, do_executec,  E_CB*sizeof(CELL));
      ENDBOp();

      /* Like previous, the only difference is that we do not */
      /* trust the C-function we are calling and hence we must */
      /* guarantee that *all* machine registers are saved and */
      /* restored */

      BOp(call_usercpred, Osbpp);
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackUserCall, HR);
      ENDCACHE_Y_AS_ENV();
    do_user_call:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        low_level_trace(enter_pred, PREG->y_u.Osbpp.p, XREGS + 1);
      }
#endif /* LOW_LEVEL_TRACE */
#ifdef FROZEN_STACKS
      {
        choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
        if (YREG > (CELL *)top_b || YREG < HR)
          ASP = (CELL *)top_b;
#else
        if (YREG > (CELL *)top_b)
          ASP = (CELL *)top_b;
#endif /* YAPOR_SBA */
        else
          ASP = (CELL *)(((char *)YREG) + PREG->y_u.Osbpp.s);
      }
#else
      SET_ASP(YREG, PREG->y_u.Osbpp.s);
/* for slots to work */
#endif /* FROZEN_STACKS */
      {
        /* make sure that we can still have access to our old PREG after calling
         * user defined goals and backtracking or failing */
        yamop *savedP;

        LOCAL_PrologMode |= UserCCallMode;
        {
          PredEntry *p = PREG->y_u.Osbpp.p;

          PREG = NEXTOP(PREG, Osbpp);
          savedP = PREG;
          saveregs();
          save_machine_regs();
          SREG = (CELL *)YAP_Execute(p, p->cs.f_code);
        setregs();
        }
        LOCAL_PrologMode &= ~UserCCallMode;
        restore_machine_regs();
        PREG = savedP;
      }
      if (Yap_HasException()) {
        Yap_RaiseException();
        SREG = NULL;
      }
      if (!SREG) {
        FAIL();
      }
      /* in case we call Execute */
      YENV = ENV;
      YREG = ENV;
      JMPNext();

    NoStackUserCall:
      PROCESS_INTERRUPT(interrupt_call, do_user_call, PREG->y_u.Osbpp.s);

      ENDBOp();

      BOp(call_c_wfail, slpp);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        low_level_trace(enter_pred, PREG->y_u.slpp.p, XREGS + 1);
      }
#endif /* LOW_LEVEL_TRACE */
#ifdef FROZEN_STACKS
      {
        choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
        if (YREG > (CELL *)top_b || YREG < HR)
          ASP = (CELL *)top_b;
#else
        if (YREG > (CELL *)top_b)
          ASP = (CELL *)top_b;
#endif /* YAPOR_SBA */
        else {
          BEGD(d0);
          d0 = PREG->y_u.slpp.s;
          ASP = ((CELL *)YREG) + d0;
          ENDD(d0);
        }
      }
#else
      if (YREG > (CELL *)B)
        ASP = (CELL *)B;
      else {
        BEGD(d0);
        d0 = PREG->y_u.slpp.s;
        ASP = ((CELL *)YREG) + d0;
        ENDD(d0);
      }
#endif /* FROZEN_STACKS */
      {
        CPredicate f = PREG->y_u.slpp.p->cs.f_code;
        saveregs();
        SREG = (CELL *)((f)(PASS_REGS1));
        setregs();
      }
      if (!SREG) {
        /* be careful about error handling */
        if (PREG != FAILCODE)
          PREG = PREG->y_u.slpp.l;
      } else {
        PREG = NEXTOP(PREG, slpp);
      }
      CACHE_A1();
      JMPNext();
      ENDBOp();

      BOp(try_c, OtapFs);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(YREG);
      /* Alocate space for the cut_c structure*/
      CUT_C_PUSH(NEXTOP(NEXTOP(PREG, OtapFs), OtapFs), S_YREG);
      S_YREG = S_YREG - PREG->y_u.OtapFs.extra;
      store_args(PREG->y_u.OtapFs.s);
      store_yaam_regs(NEXTOP(P, OtapFs), 0);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      SET_BB(B_YREG);
      ENDCACHE_Y();

    TRYCC:
      ASP = (CELL *)B;
      {
        CPredicate f = (CPredicate)(PREG->y_u.OtapFs.f);
        saveregs();
        SREG = (CELL *)((f)(PASS_REGS1));
        /* This last instruction changes B B*/
        while (POP_CHOICE_POINT(B)) {
          cut_c_pop();
        }
        setregs();
      }
      if (!SREG) {
        /* Removes the cut functions from the stack
           without executing them because we have fail
           and not cuted the predicate*/
        while (POP_CHOICE_POINT(B))
          cut_c_pop();
        FAIL();
      }
      if ((CELL *)B == YREG && ASP != (CELL *)B) {
        /* as Luis says, the predicate that did the try C might
         * have left some data on the stack. We should preserve
         * it, unless the builtin also did cut */
        YREG = ASP;
        HBREG = PROTECT_FROZEN_H(B);
        SET_BB(B);
      }
      PREG = CPREG;
      YREG = ENV;
      JMPNext();
      ENDBOp();

      BOp(retry_c, OtapFs);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(B);
      CPREG = B_YREG->cp_cp;
      ENV = B_YREG->cp_env;
      HR = PROTECT_FROZEN_H(B);
#ifdef DEPTH_LIMIT
      DEPTH = B->cp_depth;
#endif
      HBREG = HR;
      restore_args(PREG->y_u.OtapFs.s);
      ENDCACHE_Y();
      goto TRYCC;
      ENDBOp();

      BOp(cut_c, OtapFs);
/*This is a phantom instruction. This is not executed by the WAM*/
#ifdef DEBUG
      /*If WAM executes this instruction, probably there's an error
        when we put this instruction, cut_c, after retry_c*/
      printf("ERROR: Should not print this message FILE: absmi.c %d\n",
             __LINE__);
#endif /*DEBUG*/
      ENDBOp();

      BOp(try_userc, OtapFs);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(YREG);
      /* Alocate space for the cut_c structure*/
      CUT_C_PUSH(NEXTOP(NEXTOP(PREG, OtapFs), OtapFs), S_YREG);
      S_YREG = S_YREG - PREG->y_u.OtapFs.extra;
      store_args(PREG->y_u.OtapFs.s);
      store_yaam_regs(NEXTOP(PREG, OtapFs), 0);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif
      SET_BB(B_YREG);
      ENDCACHE_Y();
      LOCAL_PrologMode = UserCCallMode;
      ASP = YREG;
      saveregs();
      save_machine_regs();
      SREG = (CELL *)YAP_ExecuteFirst(PREG->y_u.OtapFs.p,
                                      (CPredicate)(PREG->y_u.OtapFs.f));
      Yap_ResetException( worker_id );
      restore_machine_regs();
      setregs();
      LOCAL_PrologMode &= UserMode;
      if (!SREG) {
        FAIL();
      }
      if ((CELL *)B == YREG && ASP != (CELL *)B) {
        /* as Luis says, the predicate that did the try C might
         * have left some data on the stack. We should preserve
         * it, unless the builtin also did cut */
        YREG = ASP;
        HBREG = PROTECT_FROZEN_H(B);
      }
      PREG = CPREG;
      YREG = ENV;
      CACHE_A1();
      JMPNext();
      ENDBOp();

      BOp(retry_userc, OtapFs);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(B);
      CPREG = B_YREG->cp_cp;
      ENV = B_YREG->cp_env;
      HR = PROTECT_FROZEN_H(B);
#ifdef DEPTH_LIMIT
      DEPTH = B->cp_depth;
#endif
      HBREG = HR;
      restore_args(PREG->y_u.OtapFs.s);
      ENDCACHE_Y();

      LOCAL_PrologMode |= UserCCallMode;
      SET_ASP(YREG, E_CB * sizeof(CELL));
      saveregs();
      save_machine_regs();
      SREG = (CELL *)YAP_ExecuteNext(PREG->y_u.OtapFs.p,
                                     (CPredicate)(PREG->y_u.OtapFs.f));
      Yap_ResetException( worker_id);
      restore_machine_regs();
      setregs();
      LOCAL_PrologMode &= ~UserCCallMode;
      if (!SREG) {
        /* Removes the cut functions from the stack
           without executing them because we have fail
           and not cuted the predicate*/
        while (POP_CHOICE_POINT(B))
          cut_c_pop();
        FAIL();
      }
      if ((CELL *)B == YREG && ASP != (CELL *)B) {
        /* as Luis says, the predicate that did the try C might
         * have left some data on the stack. We should preserve
         * it, unless the builtin also did cut */
        YREG = ASP;
        HBREG = PROTECT_FROZEN_H(B);
      }
      PREG = CPREG;
      YREG = ENV;
      CACHE_A1();
      JMPNext();
      ENDBOp();

      BOp(cut_userc, OtapFs);
/*This is a phantom instruction. This is not executed by the WAM*/
#ifdef DEBUG
      /*If WAMexec utes this instruction, probably there's an error
        when we put this instruction, cut_userc, after retry_userc*/
      printf("ERROR: Should not print this message FILE: absmi.c %d\n",
             __LINE__);
#endif /*DEBUG*/
      CACHE_A1();
      JMPNext();
      ENDBOp();

      /************************************************************************\
       *    support instructions                                             *
\************************************************************************/

      BOp(lock_pred, e);
      {
        PredEntry *ap = PredFromDefCode(PREG);
        PELOCK(10, ap);
        PP = ap;
        if (!ap->NOfClauses) {
          UNLOCKPE(11, ap);
          FAIL();
        }
        /*
          we do not lock access to the predicate,
          we must take extra care here
        */
        if (ap->NOfClauses > 1 &&
            !(ap->PredFlags & IndexedPredFlag)) {
          /* update ASP before calling IPred */
          SET_ASP(YREG, E_CB * sizeof(CELL));
          saveregs();
          Yap_IPred(ap, 0, CP);
          /* IPred can generate errors, it thus must get rid of the lock itself
           */
          setregs();
          CACHE_A1();
          /* for profiler */
          save_pc();
        }
        PREG = ap->TrueCodeOfPred;
      }
      JMPNext();
      ENDBOp();

      BOp(index_pred, e);
      {
        PredEntry *ap = PredFromDefCode(PREG);
#if defined(YAPOR) || defined(THREADS)
        /*
          we do not lock access to the predicate,
          we must take extra care here
        */
        if (!PP) {
          PELOCK(11, ap);
        }
        if (ap->OpcodeOfPred != INDEX_OPCODE) {
          /* someone was here before we were */
          if (!PP) {
            UNLOCKPE(11, ap);
          }
          PREG = ap->CodeOfPred;
          /* for profiler */
          save_pc();
          JMPNext();
        }
#endif
        /* update ASP before calling IPred */
        SET_ASP(YREG, E_CB * sizeof(CELL));
        saveregs();
        Yap_IPred(ap, 0, CP);
        /* IPred can generate errors, it thus must get rid of the lock itself */
        setregs();
        CACHE_A1();
        PREG = ap->CodeOfPred;
        /* for profiler */
        save_pc();
#if defined(YAPOR) || defined(THREADS)
        if (!PP)
#endif
          UNLOCKPE(14, ap);
      }
      JMPNext();
      ENDBOp();

#if THREADS
      BOp(thread_local, e);
      {
        PredEntry *ap = PredFromDefCode(PREG);
        ap = Yap_GetThreadPred(ap PASS_REGS);
        PREG = ap->CodeOfPred;
        /* for profiler */
        save_pc();
      }
      JMPNext();
      ENDBOp();
#endif

      BOp(expand_index, e);
      {
        PredEntry *pe = PredFromExpandCode(PREG);
        yamop *pt0;

        /* update ASP before calling IPred */
        SET_ASP(YREG, E_CB * sizeof(CELL));
#if defined(YAPOR) || defined(THREADS)
        if (!PP) {
          PELOCK(12, pe);
        }
        if (!same_lu_block(PREG_ADDR, PREG)) {
          PREG = *PREG_ADDR;
          if (!PP) {
            UNLOCKPE(15, pe);
          }
          JMPNext();
        }
#endif
#ifdef SHADOW_S
        S = SREG;
#endif /* SHADOW_S */
        saveregs();
        pt0 = Yap_ExpandIndex(pe, 0);
        /* restart index */
        setregs();
#ifdef SHADOW_S
        SREG = S;
#endif /* SHADOW_S */
        PREG = pt0;
#if defined(YAPOR) || defined(THREADS)
        if (!PP) {
          UNLOCKPE(12, pe);
        }
#endif
        JMPNext();
      }
      ENDBOp();

      BOp(expand_clauses, sssllp);
      {
        PredEntry *pe = PREG->y_u.sssllp.p;
        yamop *pt0;

        /* update ASP before calling IPred */
        SET_ASP(YREG, E_CB * sizeof(CELL));
#if defined(YAPOR) || defined(THREADS)
        if (PP == NULL) {
          PELOCK(13, pe);
        }
        if (!same_lu_block(PREG_ADDR, PREG)) {
          PREG = *PREG_ADDR;
          if (!PP) {
            UNLOCKPE(16, pe);
          }
          JMPNext();
        }
#endif
        saveregs();
        pt0 = Yap_ExpandIndex(pe, 0);
        /* restart index */
        setregs();
        PREG = pt0;
#if defined(YAPOR) || defined(THREADS)
        if (!PP) {
          UNLOCKPE(18, pe);
        }
#endif
        JMPNext();
      }
      ENDBOp();

      BOp(undef_p, e);
      /* save S for module name */
      if (LOCAL_DoingUndefp) {
	PREG=FAILCODE;
	JMPNext();
      }
        LOCAL_DoingUndefp = true;
	saveregs();
      undef_goal(PASS_REGS1);
      setregs();
      /* for profiler */
        LOCAL_DoingUndefp = false;
      CACHE_A1();
      JMPNext();
      ENDBOp();

      BOp(spy_pred, e);
      saveregs();
      spy_goal(PASS_REGS1);
      setregs();
      CACHE_A1();
      JMPNext();
      ENDBOp();
