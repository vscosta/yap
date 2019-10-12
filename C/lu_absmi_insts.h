/************************************************************************\
 *    Logical Updates                                                 *
\************************************************************************/

#ifdef INDENT_CODE
{
  {
#endif /* INDENT_CODE */

      /************************************************************************\
       *        Logical Updates                                                 *
\************************************************************************/

      /* enter logical pred               */
      BOp(enter_lu_pred, Illss);
      check_trail(TR);
      /* mark the indexing code */
      {
        LogUpdIndex *cl = PREG->y_u.Illss.I;
        PredEntry *ap = cl->ClPred;

        if (!cl) { FAIL(); } /* in case the index is empty */
        if (ap->LastCallOfPred != LUCALL_EXEC) {
          /*
            only increment time stamp if we are working on current time
            stamp
          */
          if (ap->TimeStampOfPred >= TIMESTAMP_RESET)
            Yap_UpdateTimestamps(ap);
          ap->TimeStampOfPred++;
          ap->LastCallOfPred = LUCALL_EXEC;
          /*      fprintf(stderr,"R %x--%d--%ul\n",ap,ap->TimeStampOfPred,ap->ArityOfPE);*/
        }
        *--YREG = MkIntegerTerm(ap->TimeStampOfPred);
        /* fprintf(stderr,"> %p/%p %d %d\n",cl,ap,ap->TimeStampOfPred,PREG->y_u.Illss.s);*/
        PREG = PREG->y_u.Illss.l1;
        /* indicate the indexing code is being used */
#if MULTIPLE_STACKS
        /* just store a reference */
        INC_CLREF_COUNT(cl);
        TRAIL_CLREF(cl);
#else
        if (!(cl->ClFlags & InUseMask)) {
          cl->ClFlags |= InUseMask;
          TRAIL_CLREF(cl);
        }
#endif
      }
      JMPNext();
      ENDBOp();

      BOp(profiled_retry_logical, OtaLl);
      check_trail(TR);
      {
        UInt timestamp;
        CACHE_Y(B);

#if defined(YAPOR) || defined(THREADS)
        if (PP != PREG->y_u.OtaLl.d->ClPred) {
          if (PP) UNLOCKPE(15,PP);
          PP = PREG->y_u.OtaLl.d->ClPred;
          PELOCK(15,PP);
        }
#endif
        timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[PREG->y_u.OtaLl.s]);
        if (!VALID_TIMESTAMP(timestamp, PREG->y_u.OtaLl.d)) {
          /* jump to next instruction */
          PREG=PREG->y_u.OtaLl.n;
          JMPNext();
        }
        restore_yaam_regs(PREG->y_u.OtaLl.n);
        restore_args(PREG->y_u.OtaLl.s);
        LOCK(PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->lock);
        PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->NOfRetries++;
        UNLOCK(PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->lock);
#ifdef THREADS
        PP = PREG->y_u.OtaLl.d->ClPred;
#endif
        PREG = PREG->y_u.OtaLl.d->ClCode;
#ifdef FROZEN_STACKS
        S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
        set_cut(S_YREG, B->cp_b);
#else
        set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
        SET_BB(B_YREG);
        ENDCACHE_Y();
      }
      JMPNext();
      ENDBOp();

      BOp(profiled_trust_logical, OtILl);
      CACHE_Y(B);
      {
        LogUpdIndex *cl = PREG->y_u.OtILl.block;
        PredEntry *ap = cl->ClPred;
        LogUpdClause *lcl = PREG->y_u.OtILl.d;
        UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]);

#if defined(YAPOR) || defined(THREADS)
        if (PP != ap) {
          if (PP) UNLOCKPE(16,PP);
          PP = ap;
          PELOCK(16,PP);
        }
#endif
        if (!VALID_TIMESTAMP(timestamp, lcl)) {
          /* jump to next alternative */
          PREG = FAILCODE;
        } else {
          LOCK(ap->StatisticsForPred->lock);
          ap->StatisticsForPred->NOfRetries++;
          UNLOCK(ap->StatisticsForPred->lock);
          PREG = lcl->ClCode;
        }
        /* HEY, leave indexing block alone!! */
        /* check if we are the ones using this code */
#if MULTIPLE_STACKS
        DEC_CLREF_COUNT(cl);
        /* clear the entry from the trail */
        B->cp_tr--;
        TR = B->cp_tr;
        /* actually get rid of the code */
        if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) {
          if (PREG != FAILCODE) {
            /* I am the last one using this clause, hence I don't need a lock
               to dispose of it
            */
            if (lcl->ClRefCount == 1) {
              /* make sure the clause isn't destroyed */
              /* always add an extra reference */
              INC_CLREF_COUNT(lcl);
              TRAIL_CLREF(lcl);
            }
          }
          if (cl->ClFlags & ErasedMask) {
            saveregs();
            Yap_ErLogUpdIndex(cl);
            setregs();
          } else {
            saveregs();
            Yap_CleanUpIndex(cl);
            setregs();
          }
          save_pc();
        }
#else
        if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) &&
            B->cp_tr != B->cp_b->cp_tr) {
          cl->ClFlags &= ~InUseMask;
          --B->cp_tr;
#if FROZEN_STACKS
          if (B->cp_tr > TR_FZ)
#endif
            {
              TR = B->cp_tr;
            }
          /* next, recover space for the indexing code if it was erased */
          if (cl->ClFlags & (ErasedMask|DirtyMask)) {
            if (PREG != FAILCODE) {
              /* make sure we don't erase the clause we are jumping to,
                 notice that we can erase a number of refs in one go. */
              if (!(lcl->ClFlags & InUseMask)) {
                lcl->ClFlags |= InUseMask;
                TRAIL_CLREF(lcl);
              }
            }
            if (cl->ClFlags & ErasedMask) {
              saveregs();
              Yap_ErLogUpdIndex(cl);
              setregs();
            } else {
              saveregs();
              Yap_CleanUpIndex(cl);
              setregs();
            }
            save_pc();
          }
        }
#endif
#ifdef YAPOR
        if (SCH_top_shared_cp(B)) {
          SCH_last_alternative(PREG, B_YREG);
          restore_args(ap->ArityOfPE);
#ifdef FROZEN_STACKS
          S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#else
          S_YREG++;
#endif /* FROZEN_STACKS */
          set_cut(S_YREG, B->cp_b);
        } else
#endif  /* YAPOR */
          {
            pop_yaam_regs();
            pop_args(ap->ArityOfPE);
            S_YREG--;
#ifdef FROZEN_STACKS
            S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
            set_cut(S_YREG, B);
          }
        SET_BB(B_YREG);
        ENDCACHE_Y();
        JMPNext();
      }
      ENDBOp();

      BOp(try_logical, OtaLl);
  check_trail(TR);
  {
    UInt timestamp;

    CACHE_Y(YREG);
    timestamp = IntegerOfTerm(S_YREG[0]);
    /* fprintf(stderr,"+ %p/%p %d %d %d--%u\n",PREG,PREG->y_u.OtaLl.d->ClPred,timestamp,PREG->y_u.OtaLl.d->ClPred->TimeStampOfPred,PREG->y_u.OtaLl.d->ClTimeStart,PREG->y_u.OtaLl.d->ClTimeEnd);*/
    /* Point AP to the code that follows this instruction */
    /* always do this, even if we are not going to use it */
    store_args(PREG->y_u.OtaLl.s);
    store_yaam_regs(PREG->y_u.OtaLl.n, 0);
    set_cut(S_YREG, B);
    B = B_YREG;
#ifdef YAPOR
    SCH_set_load(B_YREG);
#endif  /* YAPOR */
#ifdef YAPOR
    PP = PREG->y_u.OtaLl.d->ClPred;
#endif  /* YAPOR */
    if (!VALID_TIMESTAMP(timestamp, PREG->y_u.OtaLl.d)) {
      /* jump to next alternative */
      PREG=PREG->y_u.OtaLl.n;
    } else {
      PREG = PREG->y_u.OtaLl.d->ClCode;
    }
    SET_BB(B_YREG);
    ENDCACHE_Y();
  }
  JMPNext();
  ENDBOp();

  BOp(count_retry_logical, OtaLl);
  check_trail(TR);
  {
    UInt timestamp;
    CACHE_Y(B);

#if defined(YAPOR) || defined(THREADS)
    if (PP != PREG->y_u.OtaLl.d->ClPred) {
      if (PP) UNLOCKPE(15,PP);
      PP = PREG->y_u.OtaLl.d->ClPred;
      PELOCK(15,PP);
    }
#endif
    timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[PREG->y_u.OtaLl.s]);
    if (!VALID_TIMESTAMP(timestamp, PREG->y_u.OtaLl.d)) {
      /* jump to next instruction */
      PREG=PREG->y_u.OtaLl.n;
      JMPNext();
    }
    restore_yaam_regs(PREG->y_u.OtaLl.n);
    restore_args(PREG->y_u.OtaLl.s);
    LOCAL_RetriesCounter--;
    if (LOCAL_RetriesCounter == 0) {
      saveregs();
      Yap_NilError(RETRY_COUNTER_UNDERFLOW_EVENT,"");
      setregs();
      JMPNext();
    }
    LOCAL_PredEntriesCounter--;
    if (LOCAL_PredEntriesCounter == 0) {
      saveregs();
      Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
      setregs();
      JMPNext();
    }
    LOCK(PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->lock);
    PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->NOfRetries++;
    UNLOCK(PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->lock);
#ifdef THREADS
    PP = PREG->y_u.OtaLl.d->ClPred;
#endif
    PREG = PREG->y_u.OtaLl.d->ClCode;
#ifdef FROZEN_STACKS
    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
    set_cut(S_YREG, B->cp_b);
#else
    set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
    SET_BB(B_YREG);
    ENDCACHE_Y();
  }
  JMPNext();
  ENDBOp();

  BOp(count_trust_logical, OtILl);
  CACHE_Y(B);
  {
    LogUpdIndex *cl = PREG->y_u.OtILl.block;
    PredEntry *ap = cl->ClPred;
    LogUpdClause *lcl = PREG->y_u.OtILl.d;
    UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]);

#if defined(YAPOR) || defined(THREADS)
    if (PP != ap) {
      if (PP) UNLOCKPE(16,PP);
      PP = ap;
      PELOCK(16,PP);
    }
#endif
    if (!VALID_TIMESTAMP(timestamp, lcl)) {
      /* jump to next alternative */
      PREG = FAILCODE;
    } else {
      LOCAL_RetriesCounter--;
      if (LOCAL_RetriesCounter == 0) {
        saveregs();
        Yap_NilError(RETRY_COUNTER_UNDERFLOW_EVENT,"");
        setregs();
        JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0) {
        saveregs();
        Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
        setregs();
        JMPNext();
      }
      LOCK(ap->StatisticsForPred->lock);
      ap->StatisticsForPred->NOfRetries++;
      UNLOCK(ap->StatisticsForPred->lock);
      PREG = lcl->ClCode;
    }
    /* HEY, leave indexing block alone!! */
    /* check if we are the ones using this code */
#if MULTIPLE_STACKS
    PELOCK(2, ap);
    PP = ap;
    DEC_CLREF_COUNT(cl);
    /* clear the entry from the trail */
    --B->cp_tr;
    TR = B->cp_tr;
    /* actually get rid of the code */
    if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) {
      if (PREG != FAILCODE) {
        /* I am the last one using this clause, hence I don't need a lock
           to dispose of it
        */
        if (lcl->ClRefCount == 1) {
          /* make sure the clause isn't destroyed */
          /* always add an extra reference */
          INC_CLREF_COUNT(lcl);
          TRAIL_CLREF(lcl);
        }
      }
      if (cl->ClFlags & ErasedMask) {
        saveregs();
        Yap_ErLogUpdIndex(cl);
        setregs();
      } else {
        saveregs();
        Yap_CleanUpIndex(cl);
        setregs();
      }
      save_pc();
    }
#else
    if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) &&
        B->cp_tr != B->cp_b->cp_tr) {
      cl->ClFlags &= ~InUseMask;
      --B->cp_tr;
#if FROZEN_STACKS
      if (B->cp_tr > TR_FZ)
#endif
        {
          TR = B->cp_tr;
        }
      /* next, recover space for the indexing code if it was erased */
      if (cl->ClFlags & (ErasedMask|DirtyMask)) {
        if (PREG != FAILCODE) {
          /* make sure we don't erase the clause we are jumping too */
          if (!(lcl->ClFlags & InUseMask)) {
            lcl->ClFlags |= InUseMask;
            TRAIL_CLREF(lcl);
          }
        }
        if (cl->ClFlags & ErasedMask) {
          saveregs();
          Yap_ErLogUpdIndex(cl);
          setregs();
        } else {
          saveregs();
          Yap_CleanUpIndex(cl);
          setregs();
        }
        save_pc();
      }
    }
#endif
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      SCH_last_alternative(PREG, B_YREG);
      restore_args(ap->ArityOfPE);
#ifdef FROZEN_STACKS
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#else
      S_YREG++;
#endif /* FROZEN_STACKS */
      set_cut(S_YREG, B->cp_b);
    } else
#endif  /* YAPOR */
      {
        pop_yaam_regs();
        pop_args(ap->ArityOfPE);
        S_YREG--;
#ifdef FROZEN_STACKS
        S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B);
      }
    SET_BB(B_YREG);
    ENDCACHE_Y();
    JMPNext();
  }
  ENDBOp();



  BOp(retry_logical, OtaLl);
  check_trail(TR);
  {
    UInt timestamp;
    CACHE_Y(B);

#if defined(YAPOR) || defined(THREADS)
    if (PP != PREG->y_u.OtaLl.d->ClPred) {
      if (PP) UNLOCKPE(15,PP);
      PP = PREG->y_u.OtaLl.d->ClPred;
      PELOCK(15,PP);
    }
#endif
    timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[PREG->y_u.OtaLl.s]);
    /* fprintf(stderr,"^ %p/%p %d %d %d--%u\n",PREG,PREG->y_u.OtaLl.d->ClPred,timestamp,PREG->y_u.OtaLl.d->ClPred->TimeStampOfPred,PREG->y_u.OtaLl.d->ClTimeStart,PREG->y_u.OtaLl.d->ClTimeEnd);*/
    if (!VALID_TIMESTAMP(timestamp, PREG->y_u.OtaLl.d)) {
      /* jump to next instruction */
      PREG=PREG->y_u.OtaLl.n;
      JMPNext();
    }
    restore_yaam_regs(PREG->y_u.OtaLl.n);
    restore_at_least_one_arg(PREG->y_u.OtaLl.s);
#ifdef THREADS
    PP = PREG->y_u.OtaLl.d->ClPred;
#endif
    PREG = PREG->y_u.OtaLl.d->ClCode;
#ifdef FROZEN_STACKS
    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
    set_cut(S_YREG, B->cp_b);
#else
    set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
    SET_BB(B_YREG);
    ENDCACHE_Y();
  }
  JMPNext();
  ENDBOp();

  BOp(trust_logical, OtILl);
  CACHE_Y(B);
  {
    LogUpdIndex *cl = PREG->y_u.OtILl.block;
    PredEntry *ap = cl->ClPred;
    LogUpdClause *lcl = PREG->y_u.OtILl.d;
    UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]);

    /* fprintf(stderr,"- %p/%p %d %d %p\n",PREG,ap,timestamp,ap->TimeStampOfPred,PREG->y_u.OtILl.d->ClCode);*/
#if defined(YAPOR) || defined(THREADS)
    if (PP != ap) {
      if (PP) UNLOCKPE(16,PP);
      PP = ap;
      PELOCK(16,PP);
    }
#endif
    if (!VALID_TIMESTAMP(timestamp, lcl)) {
      /* jump to next alternative */
      PREG = FAILCODE;
    } else {
      PREG = lcl->ClCode;
    }
    /* HEY, leave indexing block alone!! */
    /* check if we are the ones using this code */
#if MULTIPLE_STACKS
    DEC_CLREF_COUNT(cl);
    /* clear the entry from the trail */
    B->cp_tr--;
    TR = B->cp_tr;
    /* actually get rid of the code */
    if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) {
      if (PREG != FAILCODE) {
        if (lcl->ClRefCount == 1) {
          /* make sure the clause isn't destroyed */
          /* always add an extra reference */
          INC_CLREF_COUNT(lcl);
          TRAIL_CLREF(lcl);
          B->cp_tr = TR;
        }
      }
      if (cl->ClFlags & ErasedMask) {
        saveregs();
        Yap_ErLogUpdIndex(cl);
        setregs();
      } else {
        saveregs();
        Yap_CleanUpIndex(cl);
        setregs();
      }
      save_pc();
    }
#else
    if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) &&
        B->cp_tr != B->cp_b->cp_tr) {
      cl->ClFlags &= ~InUseMask;
      B->cp_tr--;
#if FROZEN_STACKS
      if (B->cp_tr > TR_FZ)
#endif
        {
          TR = B->cp_tr;
        }
      /* next, recover space for the indexing code if it was erased */
      if (cl->ClFlags & (ErasedMask|DirtyMask)) {
        if (PREG != FAILCODE) {
          /* make sure we don't erase the clause we are jumping too */
          if (!(lcl->ClFlags & InUseMask)) {
            lcl->ClFlags |= InUseMask;
            TRAIL_CLREF(lcl);
            B->cp_tr = TR;
          }
        }
        if (cl->ClFlags & ErasedMask) {
          saveregs();
          Yap_ErLogUpdIndex(cl);
          setregs();
        } else {
          saveregs();
          Yap_CleanUpIndex(cl);
          setregs();
        }
      }
    }
#endif
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      SCH_last_alternative(PREG, B_YREG);
      restore_args(ap->ArityOfPE);
#ifdef FROZEN_STACKS
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#else
      S_YREG++;
#endif /* FROZEN_STACKS */
      set_cut(S_YREG, B->cp_b);
    } else
#endif  /* YAPOR */
      {
        pop_yaam_regs();
        pop_args(ap->ArityOfPE);
        S_YREG--;
#ifdef FROZEN_STACKS
        S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B);
      }
    SET_BB(B_YREG);
    ENDCACHE_Y();
#if defined(YAPOR) || defined(THREADS)
    if (PREG == FAILCODE) {
      UNLOCKPE(26,PP);
      PP = NULL;
    }
#endif
    JMPNext();
  }
  ENDBOp();



  /*****************************************************************
   *        enter a logical semantics dynamic predicate             *
   *****************************************************************/

  /* only meaningful with THREADS on! */
  /* lock logical updates predicate.  */
  Op(lock_lu, p);
#if PARALLEL_YAP
  if (PP) {
    GONext();
  }
  PP = PREG->y_u.p.p;
  PELOCK(3, PP);
#endif
  PREG = NEXTOP(PREG, p);
  GONext();
  ENDOp();

  /* only meaningful with THREADS on! */
  /* lock logical updates predicate.  */
  Op(unlock_lu, e);
#if defined(YAPOR) || defined(THREADS)
  if (PP) {
    UNLOCKPE(1,PP);
    PP = NULL;
  }
#endif
  PREG = NEXTOP(PREG, e);
  GONext();
  ENDOp();


  /* enter logical pred               */
  BOp(alloc_for_logical_pred, L);
  check_trail(TR);
  /* say that an environment is using this clause */
  /* we have our own copy for the clause */
#if MULTIPLE_STACKS
  {
    LogUpdClause *cl = PREG->y_u.L.ClBase;
#if PARALLEL_YAP
    PredEntry *ap = cl->ClPred;
#endif

    /* always add an extra reference */
    INC_CLREF_COUNT(cl);
    TRAIL_CLREF(cl);
    UNLOCKPE(2,ap);
    PP = NULL;
  }
#else
  {
    LogUpdClause *cl = (LogUpdClause *)PREG->y_u.L.ClBase;
    if (!(cl->ClFlags & InUseMask)) {
      cl->ClFlags |= InUseMask;
      TRAIL_CLREF(cl);
    }
  }
#endif
  PREG = NEXTOP(PREG, L);
  JMPNext();
  ENDBOp();

  /* copy database term               */
  BOp(copy_idb_term, e);
  {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(PREG);
    Term t;

    SET_ASP(YREG, E_CB*sizeof(CELL));
    saveregs();
    while ((t = Yap_FetchTermFromDB(cl->lusl.ClSource)) == 0L) {
      if (PP) UNLOCKPE(3,PP);
#if defined(YAPOR) || defined(THREADS)
      PP = NULL;
#endif
      if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growglobal(NULL)) {
	  Yap_NilError(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, LOCAL_ErrorMessage);
	  FAIL();
	}
      } else {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_gcl(0,3, ENV, CP)) {
	  Yap_NilError(RESOURCE_ERROR_STACK, LOCAL_ErrorMessage);
	  FAIL();
	}
      }
#if defined(YAPOR) || defined(THREADS)
      PELOCK(5,ClauseCodeToLogUpdClause(PREG)->ClPred);
      PP = ClauseCodeToLogUpdClause(PREG)->ClPred;
#endif
    }
    if (!Yap_IUnify(ARG2, t)) {
      setregs();
#if defined(YAPOR) || defined(THREADS)
      if (PP) UNLOCKPE(6,PP);
      PP = NULL;
#endif
      FAIL();
    }
    if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) {
      setregs();
#if defined(YAPOR) || defined(THREADS)
      if (PP) UNLOCKPE(5,PP);
      PP = NULL;
#endif
      FAIL();
    }
    setregs();

#if MULTIPLE_STACKS
    /* always add an extra reference */
    INC_CLREF_COUNT(cl);
    TRAIL_CLREF(cl);
    if (PP) UNLOCKPE(7,PP);
    PP = NULL;
#else
    if (!(cl->ClFlags & InUseMask)) {
      /* Clause *cl = (Clause *)PREG->y_u.EC.ClBase;

	 PREG->y_u.EC.ClTrail = TR-(tr_fr_ptr)LOCAL_TrailBase;
	 PREG->y_u.EC.ClENV = LCL0-YREG;*/
      cl->ClFlags |= InUseMask;
      TRAIL_CLREF(cl);
    }
#endif
  }
  PREG = CPREG;
  YREG = ENV;
#ifdef DEPTH_LIMIT
  DEPTH = YREG[E_DEPTH];
#endif
  JMPNext();
  ENDBOp();


  /* unify with database term               */
  BOp(unify_idb_term, e);
  {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(PREG);

    saveregs();
    if (!Yap_IUnify(ARG2, cl->lusl.ClSource->Entry)) {
      setregs();
      UNLOCKPE(8,PP);
#if defined(YAPOR) || defined(THREADS)
      PP = NULL;
#endif
      FAIL();
    }
    if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) {
      setregs();
      UNLOCKPE(9,PP);
#if defined(YAPOR) || defined(THREADS)
      PP = NULL;
#endif
      FAIL();
    }
    setregs();

    /* say that an environment is using this clause */
    /* we have our own copy for the clause */
#if MULTIPLE_STACKS
    /* always add an extra reference */
    INC_CLREF_COUNT(cl);
    TRAIL_CLREF(cl);
    UNLOCKPE(10,PP);
    PP = NULL;
#else
    if (!(cl->ClFlags & InUseMask)) {
      /* Clause *cl = (Clause *)PREG->y_u.EC.ClBase;

	 PREG->y_u.EC.ClTrail = TR-(tr_fr_ptr)LOCAL_TrailBase;
	 PREG->y_u.EC.ClENV = LCL0-YREG;*/
      cl->ClFlags |= InUseMask;
      TRAIL_CLREF(cl);
    }
#endif
  }
  PREG = CPREG;
  YREG = ENV;
#ifdef DEPTH_LIMIT
  DEPTH = YREG[E_DEPTH];
#endif
  JMPNext();
  ENDBOp();
