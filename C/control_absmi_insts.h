/************************************************************************\
 *      Cut & Commit Inst

ructions                                       *
\************************************************************************/

#ifdef INDENT_CODE
{
  {
    {
#endif /* INDENT_CODE */

      /* cut                              */
      Op(cut, s);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
	  check_stack(NoStackCut, HR);
      ENDCACHE_Y_AS_ENV();
#endif
      SET_ASP(YREG, PREG->y_u.s.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, s),Osbpp),l);
      /* assume cut is always in stack */
      saveregs();
      prune((choiceptr)YREG[E_CB] PASS_REGS);
      setregs();
      GONext();

#ifdef COROUTINING
    NoStackCut:
      PROCESS_INTERRUPT(interrupt_cut, do_cut, PREG->y_u.s.s );
    do_cut:
      set_pc();
      JMPNext();
#endif

      ENDOp();

      /* cut_t                            */
      /* cut_t does the same as cut */
      Op(cut_t, s);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackCutT, HR);
      ENDCACHE_Y_AS_ENV();
#endif
      SET_ASP(YREG, PREG->y_u.s.s);
      /* assume cut is always in stack */
      saveregs();
      prune((choiceptr)YREG[E_CB] PASS_REGS);
      setregs();
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, s),Osbpp),l);
        GONext();

        NoStackCutT:
      PROCESS_INTERRUPT(interrupt_cut_t, do_cut_t,  PREG->y_u.s.s);
    do_cut_t:
      set_pc();
      JMPNext();
      ENDOp();

      /* cut_e                            */
      Op(cut_e, s);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackCutE, HR);
      ENDCACHE_Y_AS_ENV();
#endif
      SET_ASP(YREG, PREG->y_u.s.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, s),Osbpp),l);
      saveregs();
      prune((choiceptr)SREG[E_CB] PASS_REGS);
      setregs();
      GONext();

#ifdef COROUTINING
    NoStackCutE:
      PROCESS_INTERRUPT(interrupt_cut_e, do_cut_e, PREG->y_u.s.s);
    do_cut_e:
      set_pc();
      JMPNext();
#endif

ENDOp();

      /* save_b_x      Xi                 */
      Op(save_b_x, x);
      BEGD(d0);
      d0 = PREG->y_u.x.x;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
      XREG(d0) = MkIntegerTerm((Int)B);
#else
      XREG(d0) = MkIntegerTerm(LCL0-(CELL *) (B));
#endif /* YAPOR_SBA && FROZEN_STACKS */
      PREG = NEXTOP(PREG, x);
      ENDD(d0);
      GONext();
      ENDOp();

      /* save_b_y      Yi                 */
      Op(save_b_y, y);
#if defined(YAPOR_SBA)
      INITIALIZE_PERMVAR(YREG+PREG->y_u.y.y,MkIntegerTerm((Int)B));
#else
      INITIALIZE_PERMVAR(YREG+PREG->y_u.y.y,MkIntegerTerm(LCL0-(CELL *)(B)));
#endif /* YAPOR_SBA*/
      PREG = NEXTOP(PREG, y);
      GONext();
      ENDOp();

      /* commit_b_x    Xi                 */
      Op(commit_b_x, xps);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackCommitX, HR);
      ENDCACHE_Y_AS_ENV();
#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xps.x);
      deref_head(d0, commit_b_x_unk);
    commit_b_x_nvar:
      /* skip a void call and a label */
      //   SET_ASP(ENV, PREG->y_u.xps.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xps),Osbpp),l);
      {
        choiceptr pt0;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
        pt0 = (choiceptr)IntegerOfTerm(d0);
#else
        pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0));
#endif /* YAPOR_SBA && FROZEN_STACKS */
        saveregs();
        prune(pt0 PASS_REGS);
        setregs();
      }
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, commit_b_x_unk, commit_b_x_nvar);
      ENDP(pt1);
      /* never cut to a variable */
      /* Abort */
      FAIL();
      ENDD(d0);

#ifdef COROUTINING
      /* Problem: have I got an environment or not? */
    NoStackCommitX:
      PROCESS_INTERRUPT(interrupt_commit_x, do_commit_b_x, PREG->y_u.xps.s);
      set_pc();
     do_commit_b_x:
     JMPNext();
      #endif
      ENDOp();

      /* commit_b_y    Yi                 */
      Op(commit_b_y, yps);
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackCommitY, HR);
      ENDCACHE_Y_AS_ENV();
      BEGD(d0);
      d0 = YREG[PREG->y_u.yps.y];
      deref_head(d0, commit_b_y_unk);
    commit_b_y_nvar:
      SET_ASP(YREG, PREG->y_u.yps.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yps),Osbpp),l);
      {
        choiceptr pt0;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
        pt0 = (choiceptr)IntegerOfTerm(d0);
#else
        pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0));
#endif
        saveregs();
        prune(pt0 PASS_REGS);
        setregs();
      }
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, commit_b_y_unk, commit_b_y_nvar);
      ENDP(pt1);
      /* never cut to a variable */
      /* Abort */
      FAIL();
      ENDD(d0);

      /* This is easier: I know there is an environment so I cannot do allocate */
    NoStackCommitY:
      PROCESS_INTERRUPT(interrupt_commit_y, after_commit_b_y, PREG->y_u.yps.s);
after_commit_b_y:
      set_pc();
      ENDOp();

      /*************************************************************************
       *      Call / Proceed instructions                                      *
       *************************************************************************/

      /* Macros for stack trimming                                            */

      /* execute     Label               */
      BOp(execute, Osbpp);
      {
        PredEntry *pt0;
        CACHE_Y_AS_ENV(YREG);
        pt0 = PREG->y_u.Osbpp.p;
#ifndef NO_CHECKING
        /* check stacks */
        check_stack(NoStackExecute, HR);
	goto do_execute;
     restart_execute:	
	pt0 = PP;
        FETCH_Y_FROM_ENV(YREG);
      do_execute:
#endif
#ifdef LOW_LEVEL_TRACER
        if (Yap_do_low_level_trace) {
          low_level_trace(enter_pred,pt0,XREGS+1);
        }
#endif  /* LOW_LEVEL_TRACE */
        CACHE_A1();
        ALWAYS_LOOKAHEAD(pt0->OpcodeOfPred);
        BEGD(d0);
        d0 = (CELL)B;
        PREG = pt0->CodeOfPred;
        /* for profiler */
        save_pc();
        ENV_YREG[E_CB] = d0;
        ENDD(d0);
#ifdef DEPTH_LIMIT
        if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
          if (pt0->ModuleOfPred) {
            if (DEPTH == MkIntTerm(0)) {
              FAIL();
            } else { DEPTH = RESET_DEPTH(); }
          }
        } else if (pt0->ModuleOfPred)
          DEPTH -= MkIntConstant(2);
#endif  /* DEPTH_LIMIT */
        /* this is the equivalent to setting up the stack */
        ALWAYS_GONext();
        ALWAYS_END_PREFETCH();
        ENDCACHE_Y_AS_ENV();
      }

    NoStackExecute:
      PROCESS_INTERRUPT(interrupt_execute, do_execute, PREG->y_u.Osbpp.s);

      ENDBOp();

      /* dexecute    Label               */
      /* joint deallocate and execute */
      BOp(dexecute, Osbpp);
      if (Yap_do_low_level_trace)
        low_level_trace(enter_pred,PREG->y_u.Osbpp.p,XREGS+1);
      CACHE_Y_AS_ENV(YREG);
      {
        PredEntry *pt0;

        pt0 = PREG->y_u.Osbpp.p;
        /* check stacks */
        check_stack(NoStackDExecute, HR);
      continue_dexecute:
	pt0 =PREG->y_u.Osbpp.p;
        CACHE_A1();
#ifdef DEPTH_LIMIT
        if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is primitives */
          if (pt0->ModuleOfPred) {
            if (DEPTH == MkIntTerm(0)) {
              FAIL();
            } else {
	      DEPTH = RESET_DEPTH();
	    }
          }
        } else if (pt0->ModuleOfPred)
          DEPTH -= MkIntConstant(2);
#endif  /* DEPTH_LIMIT */
        PREG = pt0->CodeOfPred;
        /* for profiler */
        save_pc();
        ALWAYS_LOOKAHEAD(pt0->OpcodeOfPred);
        /* do deallocate */
        CPREG = (yamop *) ENV_YREG[E_CP];
        ENV_YREG = ENV = (CELL *) ENV_YREG[E_E];
#ifdef FROZEN_STACKS
        {
          choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
          if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
          if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
          else ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size(CPREG));
        }
#else
        if (ENV_YREG > (CELL *)B) {
          ENV_YREG = (CELL *)B;
        }
        else {
          ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size(CPREG));
        }
#endif /* FROZEN_STACKS */
        WRITEBACK_Y_AS_ENV();
        /* setup GB */
        ENV_YREG[E_CB] = (CELL) B;
ALWAYS_GONext();
        ALWAYS_END_PREFETCH();
      }
      ENDCACHE_Y_AS_ENV();

    NoStackDExecute:
      PROCESS_INTERRUPT(interrupt_dexecute, continue_dexecute, ENV_Size(CPREG)*CellSize);
        JMPNext();
       ENDBOp();

      BOp(fcall, Osbpp);
      CACHE_Y_AS_ENV(YREG);
      ENV_YREG[E_CP] = (CELL) CPREG;
      ENV_YREG[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
      ENV_YREG[E_DEPTH] = DEPTH;
#endif  /* DEPTH_LIMIT */
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      BOp(call, Osbpp);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        low_level_trace(enter_pred,PREG->y_u.Osbpp.p,XREGS+1);
      }
#endif  /* LOW_LEVEL_TRACER */
      CACHE_Y_AS_ENV(YREG);
      {
        PredEntry *pt;
        CACHE_A1();
        pt = PREG->y_u.Osbpp.p;
#ifndef NO_CHECKING
        /* check stacks */
        check_stack(NoStackCall, HR);
	goto do_call;
     restart_call:	
	 CACHE_A1();
        FETCH_Y_FROM_ENV(YREG);
      do_call:
#endif
	//skip_call:
        ENV = ENV_YREG;
        /* Try to preserve the environment */
        ENV_YREG = (CELL *) (((char *) ENV_YREG) + PREG->y_u.Osbpp.s);
        CPREG = NEXTOP(PREG, Osbpp);
        ALWAYS_LOOKAHEAD(pt->OpcodeOfPred);
        PREG = pt->CodeOfPred;
        /* for profiler */
        save_pc();
#ifdef DEPTH_LIMIT
        if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is primitives */
          if (pt->ModuleOfPred) {
            if (DEPTH == MkIntTerm(0)) {
              FAIL();
            } else {
	      DEPTH = RESET_DEPTH();
	    }
          }
        } else if (pt->ModuleOfPred)
          DEPTH -= MkIntConstant(2);
#endif  /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
        {
          choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
          if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
          if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
        }
#else
        if (ENV_YREG > (CELL *) B) {
          ENV_YREG = (CELL *) B;
        }
#endif /* FROZEN_STACKS */
        WRITEBACK_Y_AS_ENV();
        /* setup GB */
        ENV_YREG[E_CB] = (CELL) B;
#ifdef YAPOR
        SCH_check_requests();
#endif  /* YAPOR */
        ALWAYS_GONext();
        ALWAYS_END_PREFETCH();
      }
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      BOp(procceed, p);
      CACHE_Y_AS_ENV(YREG);
      ALWAYS_LOOKAHEAD(CPREG->opc);
      PREG = CPREG;
      /* for profiler */
      save_pc();
      ENV_YREG = ENV;
#ifdef DEPTH_LIMIT
      DEPTH = ENV_YREG[E_DEPTH];
#endif
      WRITEBACK_Y_AS_ENV();
      ALWAYS_GONext();

    NoStackCall:
      PROCESS_INTERRUPT(interrupt_call, restart_call, PREG->y_u.Osbpp.s);

      ALWAYS_END_PREFETCH();
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      Op(allocate, e);
      CACHE_Y_AS_ENV(YREG);
      PREG = NEXTOP(PREG, e);
      ENV_YREG[E_CP] = (CELL) CPREG;
      ENV_YREG[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
      ENV_YREG[E_DEPTH] = DEPTH;
#endif  /* DEPTH_LIMIT */
      ENV = ENV_YREG;
      ENDCACHE_Y_AS_ENV();
      GONext();
      ENDOp();

      Op(deallocate, p);
      CACHE_Y_AS_ENV(YREG);
      // do this before checking
      SREG = YREG;
      check_trail(TR);
      PREG = NEXTOP(PREG, p);
#ifndef NO_CHECKING
      /* check stacks */
      //      check_stack(NoStackDeallocate, HR);
#endif
      /* other instructions do depend on S being set by deallocate
         :-( */
      CPREG = (yamop *) ENV_YREG[E_CP];
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E];
#ifdef DEPTH_LIMIT
      DEPTH = ENV_YREG[E_DEPTH];
#endif  /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
      {
        choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
        if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
        if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
        else ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size(CPREG));
      }
#else
      if (ENV_YREG > (CELL *) B)
        ENV_YREG = (CELL *) B;
      else
        ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size(CPREG));
#endif /* FROZEN_STACKS */
      WRITEBACK_Y_AS_ENV();
      ENDCACHE_Y_AS_ENV();
      GONext();

    NoStackDeallocate:
      BEGD(d0);
#ifdef SHADOW_S
      Yap_REGS.S_ = YREG;
#endif
      saveregs();
      d0 = interrupt_deallocate(  PASS_REGS1 );
       SREG = YREG;
     setregs();
#ifdef SHADOW_S
      SREG = Yap_REGS.S_;
#endif
      // return to original deallocate
      if (!d0) FAIL();
      		JMPNext();
      ENDD(d0);
      ENDOp();

      /**********************************************
       *        OPTYap instructions                  *
       **********************************************/

#ifdef YAPOR
#include "or.insts.h"
#endif /* YAPOR */
#ifdef TABLING
#include "tab.insts.h"
#include "tab.tries.insts.h"
#endif /* TABLING */



#ifdef BEAM
      extern int eam_am(PredEntry *);

      Op(retry_eam, e);
      printf("Aqui estou eu..................\n");
      if (!eam_am(2)) {
        abort_eam("Falhei\n");
        FAIL();
      }

      goto procceed;
      PREG = NEXTOP(PREG, e);
      GONext();
      ENDOp();

      Op(run_eam, os);
      if (inp==-9000) { /* use indexing to find out valid alternatives */
        extern CELL *beam_ALTERNATIVES;
        *beam_ALTERNATIVES= (CELL *) PREG->y_u.os.opcw;
        beam_ALTERNATIVES++;
        if (OLD_B!=B) goto fail;
#if PUSH_REGS
        Yap_regp=old_regs;
#endif
        return(0);
      }

      saveregs();
      if (!eam_am((PredEntry *) PREG->y_u.os.s)) FAIL();
      setregs();

      /* cut */
      BACKUP_B();
      while (POP_CHOICE_POINT(B->cp_b)) {
        POP_EXECUTE();
      }
      B = B->cp_b;  /* cut_fail */
      HB = B->cp_h; /* cut_fail */
      RECOVER_B();

      if (0) { register choiceptr ccp;
        /* initialize ccp */
#define NORM_CP(CP)            ((choiceptr)(CP))

        YREG = (CELL *) (NORM_CP(YREG) - 1);
        ccp = NORM_CP(YREG);
        store_yaam_reg_cpdepth(ccp);
        ccp->cp_tr = TR;
        ccp->cp_ap = BEAM_RETRY_CODE;
        ccp->cp_h  = HR;
        ccp->cp_b  = B;
        ccp->cp_env= ENV;
        ccp->cp_cp = CPREG;
        B = ccp;
        SET_BB(B);
      }
      goto procceed;
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();
#endif



