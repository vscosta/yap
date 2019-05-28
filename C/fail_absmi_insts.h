/*****************************************************************
 *        Failure                                                 *
 *****************************************************************/

#ifdef INDENT_CODE
{
#endif /* INDENT_CODE */

  /* trust_fail                       */
  BOp(trust_fail, e);
  {
    while (POP_CHOICE_POINT(B->cp_b)) {
      POP_EXECUTE();
    }
  }
#ifdef YAPOR
  {
    choiceptr cut_pt;
    cut_pt = B->cp_b;
    CUT_prune_to(cut_pt);
    B = cut_pt;
  }
#else
  B = B->cp_b;
#endif /* YAPOR */
  goto fail;
  ENDBOp();

#ifdef YAPOR
shared_fail:
  B = Get_LOCAL_top_cp();
  SET_BB(PROTECT_FROZEN_B(B));
  goto fail;
#endif /* YAPOR */

  /* fail                             */
  PBOp(op_fail, e);

  if (PP) {
    UNLOCK(PP->PELock);
    PP = NULL;
  }
#ifdef COROUTINING
  CACHE_Y_AS_ENV(YREG);
  check_stack(NoStackFail, HR);
  ENDCACHE_Y_AS_ENV();
#endif

fail : {
  register tr_fr_ptr pt0 = TR;
#if defined(YAPOR) || defined(THREADS)
  if (PP) {
    UNLOCK(PP->PELock);
    PP = NULL;
  }
#endif
  PREG = B->cp_ap;
  save_pc();
  CACHE_TR(B->cp_tr);
  PREFETCH_OP(PREG);
failloop:
  if (pt0 == S_TR) {
    SP = SP0;
#ifdef LOW_LEVEL_TRACER
    if (Yap_do_low_level_trace) {
      int go_on = true;
      yamop *ipc = PREG;

      while (go_on) {
        op_numbers opnum = Yap_op_from_opcode(ipc->opc);

        go_on = false;
        switch (opnum) {
#ifdef TABLING
        case _table_load_answer:
          low_level_trace(retry_table_loader, LOAD_CP(B)->cp_pred_entry, NULL);
          break;
        case _table_try_answer:
        case _table_retry_me:
        case _table_trust_me:
        case _table_retry:
        case _table_trust:
        case _table_completion:
#ifdef THREADS_CONSUMER_SHARING
        case _table_answer_resolution_completion:
#endif /* THREADS_CONSUMER_SHARING */
#ifdef DETERMINISTIC_TABLING
          if (IS_DET_GEN_CP(B))
            low_level_trace(retry_table_generator, DET_GEN_CP(B)->cp_pred_entry,
                            NULL);
          else
#endif /* DETERMINISTIC_TABLING */
            low_level_trace(retry_table_generator, GEN_CP(B)->cp_pred_entry,
                            (CELL *)(GEN_CP(B) + 1));
          break;
        case _table_answer_resolution:
          low_level_trace(retry_table_consumer, CONS_CP(B)->cp_pred_entry,
                          NULL);
          break;
        case _trie_trust_var:
        case _trie_retry_var:
        case _trie_trust_var_in_pair:
        case _trie_retry_var_in_pair:
        case _trie_trust_val:
        case _trie_retry_val:
        case _trie_trust_val_in_pair:
        case _trie_retry_val_in_pair:
        case _trie_trust_atom:
        case _trie_retry_atom:
        case _trie_trust_atom_in_pair:
        case _trie_retry_atom_in_pair:
        case _trie_trust_null:
        case _trie_retry_null:
        case _trie_trust_null_in_pair:
        case _trie_retry_null_in_pair:
        case _trie_trust_pair:
        case _trie_retry_pair:
        case _trie_trust_appl:
        case _trie_retry_appl:
        case _trie_trust_appl_in_pair:
        case _trie_retry_appl_in_pair:
        case _trie_trust_extension:
        case _trie_retry_extension:
        case _trie_trust_double:
        case _trie_retry_double:
        case _trie_trust_longint:
        case _trie_retry_longint:
        case _trie_trust_gterm:
        case _trie_retry_gterm:
          low_level_trace(retry_table_loader, UndefCode, NULL);
          break;
#endif /* TABLING */
        case _or_else:
        case _or_last:
          low_level_trace(retry_or, NULL, NULL);
          break;
        case _retry2:
        case _retry3:
        case _retry4:
          ipc = NEXTOP(ipc, l);
          go_on = true;
          break;
        case _jump:
          ipc = ipc->y_u.l.l;
          go_on = true;
          break;
        case _retry_c:
        case _retry_userc:
          low_level_trace(retry_pred, ipc->y_u.OtapFs.p, B->cp_args);
          break;
        case _retry_profiled:
        case _count_retry:
          ipc = NEXTOP(ipc, p);
          go_on = true;
          break;
        case _retry_me:
        case _trust_me:
        case _count_retry_me:
        case _count_trust_me:
        case _profiled_retry_me:
        case _profiled_trust_me:
        case _retry_and_mark:
        case _profiled_retry_and_mark:
        case _retry:
        case _trust:
          low_level_trace(retry_pred, ipc->y_u.Otapl.p, B->cp_args);
          break;
        case _try_logical:
        case _retry_logical:
        case _profiled_retry_logical:
        case _count_retry_logical:
        case _trust_logical:
        case _profiled_trust_logical:
        case _count_trust_logical:
          low_level_trace(retry_pred, ipc->y_u.OtILl.d->ClPred, B->cp_args);
          break;
        case _Nstop:
        case _Ystop:
          low_level_trace(retry_pred, NULL, B->cp_args);
          break;
        default:
          break;
        }
      }
    }
#endif /* LOW_LEVEL_TRACER */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
    if (pt0 < TR_FZ || pt0 > (ADDR)CurrentTrailTop + MinTrailGap)
#else
    if (pt0 < TR_FZ)
#endif /* YAPOR_SBA */
    {
      TR = TR_FZ;
      TRAIL_LINK(pt0);
    } else
#endif /* FROZEN_STACKS */
      RESTORE_TR();
    GONext();
  }
  BEGD(d1);
  d1 = TrailTerm(pt0 - 1);
  pt0--;
  if (IsVarTerm(d1)) {
#if defined(YAPOR_SBA) && defined(YAPOR)
    /* clean up the trail when we backtrack */
    if (Unsigned((Int)(d1) - (Int)(H_FZ)) >
        Unsigned((Int)(B_FZ) - (Int)(H_FZ))) {
      RESET_VARIABLE(STACK_TO_SBA(d1));
    } else
#endif
      /* normal variable */
      RESET_VARIABLE(d1);
    goto failloop;
  }
/* pointer to code space */
/* or updatable variable */
  if (IsPairTerm(d1))
  {
    register CELL flags;
    CELL *pt1 = RepPair(d1);
#ifdef LIMIT_TABLING
    if ((ADDR)pt1 == LOCAL_TrailBase) {
      sg_fr_ptr sg_fr = (sg_fr_ptr)TrailVal(pt0);
      TrailTerm(pt0) = AbsPair((CELL *)(pt0 - 1));
      SgFr_state(sg_fr)--; /* complete_in_use --> complete : compiled_in_use -->
                              compiled */
      insert_into_global_sg_fr_list(sg_fr);
      goto failloop;
    }
#endif               /* LIMIT_TABLING */
#ifdef FROZEN_STACKS /* TRAIL */
    /* avoid frozen segments */
    if (
#ifdef YAPOR_SBA
        (ADDR)pt1 >= HeapTop
#else
        IN_BETWEEN(LOCAL_TrailBase, pt1, (ADDR)CurrentTrailTop + MinTrailGap)
#endif /* YAPOR_SBA */
    ) {
      pt0 = (tr_fr_ptr)pt1;
      goto failloop;
    } else
#endif /* FROZEN_STACKS */
        if (IN_BETWEEN(H0, pt1, LCL0)) {
      if (IsAttVar(pt1)) {
        goto failloop;
      } else {
        TR = pt0;

Yap_CleanOpaqueVariable(d1);

        goto failloop;
      }
    }
#ifdef FROZEN_STACKS /* TRAIL */
    /* don't reset frozen variables */
    else if (pt0 < TR_FZ)
      goto failloop;
#endif
    flags = *pt1;
#if MULTIPLE_STACKS
    if (FlagOn(DBClMask, flags)) {
      DBRef dbr = DBStructFlagsToDBStruct(pt1);
      int erase;

      LOCK(dbr->lock);
      DEC_DBREF_COUNT(dbr);
      erase = (dbr->Flags & ErasedMask) && (dbr->ref_count == 0);
      UNLOCK(dbr->lock);
      if (erase) {
        saveregs();
        Yap_ErDBE(dbr);
        setregs();
      }
    } else {
      if (flags & LogUpdMask) {
        if (flags & IndexMask) {
          LogUpdIndex *cl = ClauseFlagsToLogUpdIndex(pt1);
          int erase;
#if PARALLEL_YAP
          PredEntry *ap = cl->ClPred;
#endif

          PELOCK(8, ap);
          DEC_CLREF_COUNT(cl);
          erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount);
          if (erase) {
            saveregs();
            /* at this point,
we are the only ones accessing the clause,
hence we don't need to have a lock it */
            Yap_ErLogUpdIndex(cl);
            setregs();
          } else if (cl->ClFlags & DirtyMask) {
            saveregs();
            /* at this point,
we are the only ones accessing the clause,
hence we don't need to have a lock it */
            Yap_CleanUpIndex(cl);
            setregs();
          }
          UNLOCK(ap->PELock);
        } else {
          LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt1);
          int erase;
          PredEntry *ap = cl->ClPred;
          /* BB support */
          if (ap) {

            PELOCK(9, ap);
            DEC_CLREF_COUNT(cl);
            erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount);
            if (erase) {
              saveregs();
              /* at this point,
we are the only ones accessing the clause,
hence we don't need to have a lock it */
              Yap_ErLogUpdCl(cl);
              setregs();
            }
            UNLOCK(ap->PELock);
          }
        }
      } else {
        DynamicClause *cl = ClauseFlagsToDynamicClause(pt1);
        int erase;

        LOCK(cl->ClLock);
        DEC_CLREF_COUNT(cl);
        erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount);
        UNLOCK(cl->ClLock);
        if (erase) {
          saveregs();
          /* at this point,
we are the only ones accessing the clause,
hence we don't need to have a lock it */
          Yap_ErCl(cl);
          setregs();
        }
      }
    }
#else
    ResetFlag(InUseMask, flags);
    *pt1 = flags;
    if (FlagOn((ErasedMask | DirtyMask), flags)) {
      if (FlagOn(DBClMask, flags)) {
        saveregs();
        Yap_ErDBE(DBStructFlagsToDBStruct(pt1));
        setregs();
      } else {
        saveregs();
        if (flags & LogUpdMask) {
          if (flags & IndexMask) {
            if (FlagOn(ErasedMask, flags)) {
              Yap_ErLogUpdIndex(ClauseFlagsToLogUpdIndex(pt1));
            } else {
              Yap_CleanUpIndex(ClauseFlagsToLogUpdIndex(pt1));
            }
          } else {
            Yap_ErLogUpdCl(ClauseFlagsToLogUpdClause(pt1));
          }
        } else {
          Yap_ErCl(ClauseFlagsToDynamicClause(pt1));
        }
        setregs();
      }
    }
#endif
    goto failloop;
  }
#ifdef MULTI_ASSIGNMENT_VARIABLES
  else /* if (IsApplTerm(d1)) */
  {
    CELL *pt = RepAppl(d1);
/* AbsAppl means */
/* multi-assignment variable */
/* so the next cell is the old value */
#ifdef FROZEN_STACKS
    --pt0;
    pt[0] = TrailVal(pt0);
#else
    pt[0] = TrailTerm(pt0 - 1);
    pt0 -= 2;
#endif /* FROZEN_STACKS */
    goto failloop;
  }
#endif
  ENDD(d1);
  ENDCACHE_TR();
}

#ifdef COROUTINING
NoStackFail:
  BEGD(d0);
#ifdef SHADOW_S
  Yap_REGS.S_ = SREG;
#endif
  saveregs();
  d0 = interrupt_fail(PASS_REGS1);
  setregs();
#ifdef SHADOW_S
  SREG = Yap_REGS.S_;
#endif
  if (!d0)
    FAIL();
  JMPNext();
  ENDD(d0);

#endif /* COROUTINING */
  ENDPBOp();
#ifdef INDENT_CODE
}
#endif /* INDENT_CODE */
