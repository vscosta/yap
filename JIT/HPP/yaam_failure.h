#define TRUST_FAIL_INSTINIT

#ifdef CUT_C
#define TRUST_FAIL_CUT_C \
	while (POP_CHOICE_POINT(B->cp_b)) \
	    POP_EXECUTE();
#endif

#ifdef YAPOR
#define TRUST_FAIL_YAPOR \
    choiceptr cut_pt; \
    cut_pt = B->cp_b; \
    CUT_prune_to(cut_pt); \
    B = cut_pt;
#else
#define TRUST_FAIL_NOYAPOR \
      B = B->cp_b; \
	  return external_labels[0];
#endif

#ifdef YAPOR
#define LBL_SHARED_FAIL \
      B = Get_LOCAL_top_cp(); \
      SET_BB(PROTECT_FROZEN_B(B));
#endif

#define OP_FAIL_INSTINIT \
      if (PP) { \
	UNLOCK(PP->PELock); \
	PP = NULL; \
      }

#ifdef LOW_LEVEL_TRACER
#if defined(YAPOR) || defined(THREADS)
#define LBL_FAIL_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      { \
	register tr_fr_ptr pt0 = TR; \
	if (PP) { \
	  UNLOCK(PP->PELock); \
	  PP = NULL; \
	} \
	(*_PREG) = B->cp_ap; \
	save_pc(); \
	CACHE_TR(B->cp_tr); \
	PREFETCH_OP((*_PREG)); \
      while (1) { \
	if (pt0 == S_TR) { \
	  SP = SP0; \
    LBL_FAIL_LOW_LEVEL_TRACER
#else
#define LBL_FAIL_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      { \
	register tr_fr_ptr pt0 = TR; \
	(*_PREG) = B->cp_ap; \
	save_pc(); \
	CACHE_TR(B->cp_tr); \
	PREFETCH_OP((*_PREG)); \
      while (1) { \
	if (pt0 == S_TR) { \
	  SP = SP0; \
    LBL_FAIL_LOW_LEVEL_TRACER
#endif
#else /* LOW_LEVEL_TRACER */
#if defined(YAPOR) || defined(THREADS)
#define LBL_FAIL_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      { \
	register tr_fr_ptr pt0 = TR; \
	if (PP) { \
	  UNLOCK(PP->PELock); \
	  PP = NULL; \
	} \
	(*_PREG) = B->cp_ap; \
	save_pc(); \
	CACHE_TR(B->cp_tr); \
	PREFETCH_OP((*_PREG)); \
      while (1) { \
	if (pt0 == S_TR) { \
	  SP = SP0; \
    LBL_FAIL_POST_LOW_LEVEL_TRACER
#else
#define LBL_FAIL_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      { \
	register tr_fr_ptr pt0 = TR; \
	(*_PREG) = B->cp_ap; \
	save_pc(); \
	CACHE_TR(B->cp_tr); \
	PREFETCH_OP((*_PREG)); \
      while (1) { \
	if (pt0 == S_TR) { \
	  SP = SP0; \
    LBL_FAIL_POST_LOW_LEVEL_TRACER
#endif
#endif /* LOW_LEVEL_TRACER */

#ifdef LOW_LEVEL_TRACER
#ifdef TABLING
#ifdef THREADS_CONSUMER_SHARING
#ifdef DETERMINISTIC_TABLING
#define LBL_FAIL_LOW_LEVEL_TRACER \
	  if (Yap_do_low_level_trace) { \
	    int go_on = TRUE; \
	    yamop *ipc = (*_(*_PREG)); \
	    while (go_on) { \
	      op_numbers opnum = Yap_op_from_opcode(ipc->opc); \
	      go_on = FALSE; \
	      switch (opnum) { \
	      case _table_load_answer: \
		  { \
		low_level_trace(retry_table_loader, LOAD_CP(B)->cp_pred_entry, NULL); \
		break; \
		  } \
	      case _table_try_answer: \
	      case _table_retry_me: \
	      case _table_trust_me: \
	      case _table_retry: \
	      case _table_trust: \
	      case _table_completion: \
	      case _table_answer_resolution_completion: \
		if (IS_DET_GEN_CP(B)) { \
		  low_level_trace(retry_table_generator, DET_GEN_CP(B)->cp_pred_entry, NULL); \
		} else \
        { \
		  low_level_trace(retry_table_generator, GEN_CP(B)->cp_pred_entry, (CELL *)(GEN_CP(B) + 1)); \
		} \
		break; \
	      case _table_answer_resolution: \
		{ \
		low_level_trace(retry_table_consumer, CONS_CP(B)->cp_pred_entry, NULL); \
		break; \
		} \
              case _trie_trust_var: \
              case _trie_retry_var: \
              case _trie_trust_var_in_pair: \
              case _trie_retry_var_in_pair: \
              case _trie_trust_val: \
              case _trie_retry_val: \
              case _trie_trust_val_in_pair: \
              case _trie_retry_val_in_pair: \
              case _trie_trust_atom: \
              case _trie_retry_atom: \
              case _trie_trust_atom_in_pair: \
              case _trie_retry_atom_in_pair: \
              case _trie_trust_null: \
              case _trie_retry_null: \
              case _trie_trust_null_in_pair: \
              case _trie_retry_null_in_pair: \
              case _trie_trust_pair: \
              case _trie_retry_pair: \
              case _trie_trust_appl: \
              case _trie_retry_appl: \
              case _trie_trust_appl_in_pair: \
              case _trie_retry_appl_in_pair: \
              case _trie_trust_extension: \
              case _trie_retry_extension: \
              case _trie_trust_double: \
              case _trie_retry_double: \
              case _trie_trust_longint: \
              case _trie_retry_longint: \
              case _trie_trust_gterm: \
              case _trie_retry_gterm: \
	    { \
		low_level_trace(retry_table_loader, UndefCode, NULL); \
		break; \
		} \
    LBL_FAIL_POST_LOW_LEVEL_TRACER
#else /* DETERMINISTIC_TABLING */
#define LBL_FAIL_LOW_LEVEL_TRACER \
	  if (Yap_do_low_level_trace) { \
	    int go_on = TRUE; \
	    yamop *ipc = (*_PREG); \
	    while (go_on) { \
	      op_numbers opnum = Yap_op_from_opcode(ipc->opc); \
	      go_on = FALSE; \
	      switch (opnum) { \
	      case _table_load_answer: \
		  { \
		low_level_trace(retry_table_loader, LOAD_CP(B)->cp_pred_entry, NULL); \
		break; \
		  } \
	      case _table_try_answer: \
	      case _table_retry_me: \
	      case _table_trust_me: \
	      case _table_retry: \
	      case _table_trust: \
	      case _table_completion: \
	      case _table_answer_resolution_completion: \
        { \
		  low_level_trace(retry_table_generator, GEN_CP(B)->cp_pred_entry, (CELL *)(GEN_CP(B) + 1)); \
		} \
		break; \
	      case _table_answer_resolution: \
		{ \
		low_level_trace(retry_table_consumer, CONS_CP(B)->cp_pred_entry, NULL); \
		break; \
		} \
              case _trie_trust_var: \
              case _trie_retry_var: \
              case _trie_trust_var_in_pair: \
              case _trie_retry_var_in_pair: \
              case _trie_trust_val: \
              case _trie_retry_val: \
              case _trie_trust_val_in_pair: \
              case _trie_retry_val_in_pair: \
              case _trie_trust_atom: \
              case _trie_retry_atom: \
              case _trie_trust_atom_in_pair: \
              case _trie_retry_atom_in_pair: \
              case _trie_trust_null: \
              case _trie_retry_null: \
              case _trie_trust_null_in_pair: \
              case _trie_retry_null_in_pair: \
              case _trie_trust_pair: \
              case _trie_retry_pair: \
              case _trie_trust_appl: \
              case _trie_retry_appl: \
              case _trie_trust_appl_in_pair: \
              case _trie_retry_appl_in_pair: \
              case _trie_trust_extension: \
              case _trie_retry_extension: \
              case _trie_trust_double: \
              case _trie_retry_double: \
              case _trie_trust_longint: \
              case _trie_retry_longint: \
              case _trie_trust_gterm: \
              case _trie_retry_gterm: \
	    { \
		low_level_trace(retry_table_loader, UndefCode, NULL); \
		break; \
		} \
    LBL_FAIL_POST_LOW_LEVEL_TRACER
#endif /* DETERMINISTIC_TABLING */
#else /* THREADS_CONSUMER_SHARING */
#ifdef DETERMINISTIC_TABLING
#define LBL_FAIL_LOW_LEVEL_TRACER \
	  if (Yap_do_low_level_trace) { \
	    int go_on = TRUE; \
	    yamop *ipc = (*_PREG); \
	    while (go_on) { \
	      op_numbers opnum = Yap_op_from_opcode(ipc->opc); \
	      go_on = FALSE; \
	      switch (opnum) { \
	      case _table_load_answer: \
		  { \
		low_level_trace(retry_table_loader, LOAD_CP(B)->cp_pred_entry, NULL); \
		break; \
		  } \
	      case _table_try_answer: \
	      case _table_retry_me: \
	      case _table_trust_me: \
	      case _table_retry: \
	      case _table_trust: \
	      case _table_completion: \
		if (IS_DET_GEN_CP(B)) { \
		  low_level_trace(retry_table_generator, DET_GEN_CP(B)->cp_pred_entry, NULL); \
		} else \
        { \
		  low_level_trace(retry_table_generator, GEN_CP(B)->cp_pred_entry, (CELL *)(GEN_CP(B) + 1)); \
		} \
		break; \
	      case _table_answer_resolution: \
		{ \
		low_level_trace(retry_table_consumer, CONS_CP(B)->cp_pred_entry, NULL); \
		break; \
		} \
              case _trie_trust_var: \
              case _trie_retry_var: \
              case _trie_trust_var_in_pair: \
              case _trie_retry_var_in_pair: \
              case _trie_trust_val: \
              case _trie_retry_val: \
              case _trie_trust_val_in_pair: \
              case _trie_retry_val_in_pair: \
              case _trie_trust_atom: \
              case _trie_retry_atom: \
              case _trie_trust_atom_in_pair: \
              case _trie_retry_atom_in_pair: \
              case _trie_trust_null: \
              case _trie_retry_null: \
              case _trie_trust_null_in_pair: \
              case _trie_retry_null_in_pair: \
              case _trie_trust_pair: \
              case _trie_retry_pair: \
              case _trie_trust_appl: \
              case _trie_retry_appl: \
              case _trie_trust_appl_in_pair: \
              case _trie_retry_appl_in_pair: \
              case _trie_trust_extension: \
              case _trie_retry_extension: \
              case _trie_trust_double: \
              case _trie_retry_double: \
              case _trie_trust_longint: \
              case _trie_retry_longint: \
              case _trie_trust_gterm: \
              case _trie_retry_gterm: \
	    { \
		low_level_trace(retry_table_loader, UndefCode, NULL); \
		break; \
		} \
    LBL_FAIL_POST_LOW_LEVEL_TRACER
#else /* DETERMINISTIC_TABLING */
#define LBL_FAIL_LOW_LEVEL_TRACER \
	  if (Yap_do_low_level_trace) { \
	    int go_on = TRUE; \
	    yamop *ipc = (*_PREG); \
	    while (go_on) { \
	      op_numbers opnum = Yap_op_from_opcode(ipc->opc); \
	      go_on = FALSE; \
	      switch (opnum) { \
	      case _table_load_answer: \
		  { \
		low_level_trace(retry_table_loader, LOAD_CP(B)->cp_pred_entry, NULL); \
		break; \
		  } \
	      case _table_try_answer: \
	      case _table_retry_me: \
	      case _table_trust_me: \
	      case _table_retry: \
	      case _table_trust: \
	      case _table_completion: \
        { \
		  low_level_trace(retry_table_generator, GEN_CP(B)->cp_pred_entry, (CELL *)(GEN_CP(B) + 1)); \
		} \
		break; \
	      case _table_answer_resolution: \
		{ \
		low_level_trace(retry_table_consumer, CONS_CP(B)->cp_pred_entry, NULL); \
		break; \
		} \
              case _trie_trust_var: \
              case _trie_retry_var: \
              case _trie_trust_var_in_pair: \
              case _trie_retry_var_in_pair: \
              case _trie_trust_val: \
              case _trie_retry_val: \
              case _trie_trust_val_in_pair: \
              case _trie_retry_val_in_pair: \
              case _trie_trust_atom: \
              case _trie_retry_atom: \
              case _trie_trust_atom_in_pair: \
              case _trie_retry_atom_in_pair: \
              case _trie_trust_null: \
              case _trie_retry_null: \
              case _trie_trust_null_in_pair: \
              case _trie_retry_null_in_pair: \
              case _trie_trust_pair: \
              case _trie_retry_pair: \
              case _trie_trust_appl: \
              case _trie_retry_appl: \
              case _trie_trust_appl_in_pair: \
              case _trie_retry_appl_in_pair: \
              case _trie_trust_extension: \
              case _trie_retry_extension: \
              case _trie_trust_double: \
              case _trie_retry_double: \
              case _trie_trust_longint: \
              case _trie_retry_longint: \
              case _trie_trust_gterm: \
              case _trie_retry_gterm: \
	    { \
		low_level_trace(retry_table_loader, UndefCode, NULL); \
		break; \
		} \
    LBL_FAIL_POST_LOW_LEVEL_TRACER
#endif /* DETERMINISTIC_TABLING */
#endif /* THREADS_CONSUMER_SHARING */
#endif /* TABLING */
#define LBL_FAIL_LOW_LEVEL_TRACER \
	  if (Yap_do_low_level_trace) { \
	    int go_on = TRUE; \
	    yamop *ipc = (*_PREG); \
	    while (go_on) { \
	      op_numbers opnum = Yap_op_from_opcode(ipc->opc); \
	      go_on = FALSE; \
	      switch (opnum) { \
	      case _or_else: \
	      case _or_last: \
		{ \
		low_level_trace(retry_or, (PredEntry *)ipc, &(B->cp_a1)); \
		break; \
		} \
	      case _retry2: \
	      case _retry3: \
	      case _retry4: \
		{ \
		ipc = NEXTOP(ipc,l); \
		go_on = TRUE; \
		break; \
		} \
	      case _jump: \
		{ \
		ipc = ipc->u.l.l; \
		go_on = TRUE; \
		break; \
		} \
	      case _retry_c: \
	      case _retry_userc: \
		{ \
		low_level_trace(retry_pred, ipc->u.OtapFs.p, B->cp_args); \
		break; \
		} \
	      case _retry_profiled: \
	      case _count_retry: \
		{ \
		ipc = NEXTOP(ipc,p); \
		go_on = TRUE; \
		break; \
		} \
	      case _retry_me: \
	      case _trust_me: \
	      case _count_retry_me: \
	      case _count_trust_me: \
	      case _profiled_retry_me: \
	      case _profiled_trust_me: \
	      case _retry_and_mark: \
	      case _profiled_retry_and_mark: \
	      case _retry: \
	      case _trust: \
		{ \
		low_level_trace(retry_pred, ipc->u.Otapl.p, B->cp_args); \
		break; \
		} \
	      case _try_logical: \
	      case _retry_logical: \
	      case _profiled_retry_logical: \
	      case _count_retry_logical: \
	      case _trust_logical: \
	      case _profiled_trust_logical: \
	      case _count_trust_logical: \
		{ \
		low_level_trace(retry_pred, ipc->u.OtILl.d->ClPred, B->cp_args); \
		break; \
		} \
	      case _Nstop: \
	      case _Ystop: \
		{ \
		low_level_trace(retry_pred, NULL, B->cp_args); \
		break; \
		} \
	      default: \
		break; \
	      } \
	    } \
	  } \
    LBL_FAIL_POST_LOW_LEVEL_TRACER
#endif	/* LOW_LEVEL_TRACER */

#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define LBL_FAIL_POST_LOW_LEVEL_TRACER \
	  if (pt0 < TR_FZ || pt0 > (ADDR)CurrentTrailTop+MinTrailGap) \
	    { \
	      TR = TR_FZ; \
	      TRAIL_LINK(pt0); \
	    } else \
        { \
      RESTORE_TR(); \
        } \
      GONext(); \
	  break; \
	} \
	register CELL d1; \
	d1 = TrailTerm(pt0-1); \
	pt0--; \
    LBL_FAIL_VARTERM
#else /* YAPOR_SBA */
#define LBL_FAIL_POST_LOW_LEVEL_TRACER \
	  if (pt0 < TR_FZ) \
	    { \
	      TR = TR_FZ; \
	      TRAIL_LINK(pt0); \
	    } else \
        { \
      RESTORE_TR(); \
        } \
	  break; \
	} \
	register CELL d1; \
	d1 = TrailTerm(pt0-1); \
	pt0--; \
    LBL_FAIL_VARTERM
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define LBL_FAIL_POST_LOW_LEVEL_TRACER \
        { \
      RESTORE_TR(); \
        } \
	  break; \
	} \
	register CELL d1; \
	d1 = TrailTerm(pt0-1); \
	pt0--; \
    LBL_FAIL_VARTERM
#endif /* FROZEN_STACKS */

#if defined(YAPOR_SBA) && defined(YAPOR)
#define LBL_FAIL_VARTERM \
	if (IsVarTerm(d1)) { \
	  if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
	      Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	    RESET_VARIABLE(STACK_TO_SBA(d1)); \
	  } else \
      { \
	    RESET_VARIABLE(d1); \
      } \
	  continue; \
	} \
    LBL_FAIL_PAIRTERM_INIT
#else /* defined(YAPOR_SBA) && defined(YAPOR) */
#define LBL_FAIL_VARTERM \
	if (IsVarTerm(d1)) { \
      { \
	    RESET_VARIABLE(d1); \
      } \
	  continue; \
	} \
    LBL_FAIL_PAIRTERM_INIT
#endif /* defined(YAPOR_SBA) && defined(YAPOR) */

#if defined(TERM_EXTENSIONS) || defined(FROZEN_STACKS) || defined(MULTI_ASSIGNMENT_VARIABLES)
#ifdef LIMIT_TABLING
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define LBL_FAIL_PAIRTERM_INIT \
	if (IsPairTerm(d1)) \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ((ADDR) pt1 == LOCAL_TrailBase) { \
	      sg_fr_ptr sg_fr = (sg_fr_ptr) TrailVal(pt0); \
	      TrailTerm(pt0) = AbsPair((CELL *)(pt0 - 1)); \
	      SgFr_state(sg_fr)--; \
	      insert_into_global_sg_fr_list(sg_fr); \
	      continue; \
	    } \
	    if ( \
		(ADDR) pt1 >= HeapTop \
		) \
            { \
	      pt0 = (tr_fr_ptr) pt1; \
	      continue; \
	    } else \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
	    if (pt0 < TR_FZ) { \
		  continue; \
	    } \
    LBL_FAIL_PAIRTERM_END_APPL
#else /* YAPOR_SBA */
#define LBL_FAIL_PAIRTERM_INIT \
	if (IsPairTerm(d1)) \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ((ADDR) pt1 == LOCAL_TrailBase) { \
	      sg_fr_ptr sg_fr = (sg_fr_ptr) TrailVal(pt0); \
	      TrailTerm(pt0) = AbsPair((CELL *)(pt0 - 1)); \
	      SgFr_state(sg_fr)--; \
	      insert_into_global_sg_fr_list(sg_fr); \
	      continue; \
	    } \
	    if ( \
		IN_BETWEEN(LOCAL_TrailBase, pt1, (ADDR)CurrentTrailTop+MinTrailGap) \
		) \
            { \
	      pt0 = (tr_fr_ptr) pt1; \
	      continue; \
	    } else \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
	    if (pt0 < TR_FZ) { \
		  continue; \
	    } \
    LBL_FAIL_PAIRTERM_END_APPL
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define LBL_FAIL_PAIRTERM_INIT \
	if (IsPairTerm(d1)) \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ((ADDR) pt1 == LOCAL_TrailBase) { \
	      sg_fr_ptr sg_fr = (sg_fr_ptr) TrailVal(pt0); \
	      TrailTerm(pt0) = AbsPair((CELL *)(pt0 - 1)); \
	      SgFr_state(sg_fr)--; \
	      insert_into_global_sg_fr_list(sg_fr); \
	      continue; \
	    } \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
    LBL_FAIL_PAIRTERM_END_APPL
#endif /* FROZEN_STACKS */
#else /* LIMIT_TABLING */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define LBL_FAIL_PAIRTERM_INIT \
	if (IsPairTerm(d1)) \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ( \
		(ADDR) pt1 >= HeapTop \
		) \
            { \
	      pt0 = (tr_fr_ptr) pt1; \
	      continue; \
	    } else \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
	    if (pt0 < TR_FZ) { \
		  continue; \
	    } \
    LBL_FAIL_PAIRTERM_END_APPL
#else /* YAPOR_SBA */
#define LBL_FAIL_PAIRTERM_INIT \
	if (IsPairTerm(d1)) \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ( \
		IN_BETWEEN(LOCAL_TrailBase, pt1, (ADDR)CurrentTrailTop+MinTrailGap) \
		) \
            { \
	      pt0 = (tr_fr_ptr) pt1; \
	      continue; \
	    } else \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
	    if (pt0 < TR_FZ) { \
		  continue; \
	    } \
    LBL_FAIL_PAIRTERM_END_APPL
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define LBL_FAIL_PAIRTERM_INIT \
	if (IsPairTerm(d1)) \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
    LBL_FAIL_PAIRTERM_END_APPL
#endif /* FROZEN_STACKS */
#endif /* LIMIT_TABLING */
#else /* defined(TERM_EXTENSIONS) || defined(FROZEN_STACKS) || defined(MULTI_ASSIGNMENT_VARIABLES) */
#ifdef LIMIT_TABLING
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define LBL_FAIL_PAIRTERM_INIT \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ((ADDR) pt1 == LOCAL_TrailBase) { \
	      sg_fr_ptr sg_fr = (sg_fr_ptr) TrailVal(pt0); \
	      TrailTerm(pt0) = AbsPair((CELL *)(pt0 - 1)); \
	      SgFr_state(sg_fr)--; \
	      insert_into_global_sg_fr_list(sg_fr); \
	      continue; \
	    } \
	    if ( \
		(ADDR) pt1 >= HeapTop \
		) \
            { \
	      pt0 = (tr_fr_ptr) pt1; \
	      continue; \
	    } else \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
	    if (pt0 < TR_FZ) { \
		  continue; \
	    } \
    LBL_FAIL_PAIRTERM_END_APPL
#else /* YAPOR_SBA */
#define LBL_FAIL_PAIRTERM_INIT \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ((ADDR) pt1 == LOCAL_TrailBase) { \
	      sg_fr_ptr sg_fr = (sg_fr_ptr) TrailVal(pt0); \
	      TrailTerm(pt0) = AbsPair((CELL *)(pt0 - 1)); \
	      SgFr_state(sg_fr)--; \
	      insert_into_global_sg_fr_list(sg_fr); \
	      continue; \
	    } \
	    if ( \
		IN_BETWEEN(LOCAL_TrailBase, pt1, (ADDR)CurrentTrailTop+MinTrailGap) \
		) \
            { \
	      pt0 = (tr_fr_ptr) pt1; \
	      continue; \
	    } else \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
	    if (pt0 < TR_FZ) { \
		  continue; \
	    } \
    LBL_FAIL_PAIRTERM_END_APPL
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define LBL_FAIL_PAIRTERM_INIT \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ((ADDR) pt1 == LOCAL_TrailBase) { \
	      sg_fr_ptr sg_fr = (sg_fr_ptr) TrailVal(pt0); \
	      TrailTerm(pt0) = AbsPair((CELL *)(pt0 - 1)); \
	      SgFr_state(sg_fr)--; \
	      insert_into_global_sg_fr_list(sg_fr); \
	      continue; \
	    } \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
    LBL_FAIL_PAIRTERM_END_APPL
#endif /* FROZEN_STACKS */
#else /* LIMIT_TABLING */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define LBL_FAIL_PAIRTERM_INIT \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ( \
		(ADDR) pt1 >= HeapTop \
		) \
            { \
	      pt0 = (tr_fr_ptr) pt1; \
	      continue; \
	    } else \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
	    if (pt0 < TR_FZ) { \
		  continue; \
	    } \
    LBL_FAIL_PAIRTERM_END_APPL
#else /* YAPOR_SBA */
#define LBL_FAIL_PAIRTERM_INIT \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	    if ( \
		IN_BETWEEN(LOCAL_TrailBase, pt1, (ADDR)CurrentTrailTop+MinTrailGap) \
		) \
            { \
	      pt0 = (tr_fr_ptr) pt1; \
	      continue; \
	    } else \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
	    if (pt0 < TR_FZ) { \
		  continue; \
	    } \
    LBL_FAIL_PAIRTERM_END_APPL
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define LBL_FAIL_PAIRTERM_INIT \
	  { \
	    register CELL flags; \
	    CELL *pt1 = RepPair(d1); \
	      if (IN_BETWEEN(H0,pt1,H)) { \
		if (IsAttVar(pt1)) { \
		  continue; \
		} else if (*pt1 == (CELL)FunctorBigInt) { \
		  Yap_CleanOpaqueVariable(pt1); \
		} \
	      } \
    LBL_FAIL_PAIRTERM_END_APPL
#endif /* FROZEN_STACKS */
#endif /* LIMIT_TABLING */
#endif /* defined(TERM_EXTENSIONS) || defined(FROZEN_STACKS) || defined(MULTI_ASSIGNMENT_VARIABLES) */

#if MULTIPLE_STACKS
#if PARALLEL_YAP
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define LBL_FAIL_PAIRTERM_END_APPL \
	    flags = *pt1; \
	    if (FlagOn(DBClMask, flags)) { \
	      DBRef dbr = DBStructFlagsToDBStruct(pt1); \
	      int erase; \
	      LOCK(dbr->lock); \
	      DEC_DBREF_COUNT(dbr); \
	      erase = (dbr->Flags & ErasedMask) && (dbr->ref_count == 0); \
	      UNLOCK(dbr->lock); \
	      if (erase) { \
		saveregs(); \
		Yap_ErDBE(dbr); \
		setregs(); \
	      } \
	    } else { \
	      if (flags & LogUpdMask) { \
		if (flags & IndexMask) { \
		  LogUpdIndex *cl = ClauseFlagsToLogUpdIndex(pt1); \
		  int erase; \
		  PredEntry *ap = cl->ClPred; \
		  PELOCK(8,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdIndex(cl); \
		    setregs(); \
		  } else if (cl->ClFlags & DirtyMask) { \
		    saveregs(); \
		    Yap_CleanUpIndex(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} else { \
		  LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt1); \
		  int erase; \
		  PredEntry *ap = cl->ClPred; \
		  PELOCK(9,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdCl(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} \
	      } else { \
		DynamicClause *cl = ClauseFlagsToDynamicClause(pt1); \
		int erase; \
		LOCK(cl->ClLock); \
		DEC_CLREF_COUNT(cl); \
		erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		UNLOCK(cl->ClLock); \
		if (erase) { \
		  saveregs(); \
		  Yap_ErCl(cl); \
		  setregs(); \
		} \
	      } \
	    } \
	    continue; \
	  } \
	else { \
	  CELL *pt = RepAppl(d1); \
	  --pt0; \
	  pt[0] = TrailVal(pt0); \
	  pt[0] = TrailTerm(pt0-1); \
	  pt0 -= 2; \
	  continue; \
	} \
	ENDCACHE_TR(); \
      } \
    }
#else /* FROZEN_STACKS */
#define LBL_FAIL_PAIRTERM_END_APPL \
	    flags = *pt1; \
	    if (FlagOn(DBClMask, flags)) { \
	      DBRef dbr = DBStructFlagsToDBStruct(pt1); \
	      int erase; \
	      LOCK(dbr->lock); \
	      DEC_DBREF_COUNT(dbr); \
	      erase = (dbr->Flags & ErasedMask) && (dbr->ref_count == 0); \
	      UNLOCK(dbr->lock); \
	      if (erase) { \
		saveregs(); \
		Yap_ErDBE(dbr); \
		setregs(); \
	      } \
	    } else { \
	      if (flags & LogUpdMask) { \
		if (flags & IndexMask) { \
		  LogUpdIndex *cl = ClauseFlagsToLogUpdIndex(pt1); \
		  int erase; \
		  PredEntry *ap = cl->ClPred; \
		  PELOCK(8,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdIndex(cl); \
		    setregs(); \
		  } else if (cl->ClFlags & DirtyMask) { \
		    saveregs(); \
		    Yap_CleanUpIndex(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} else { \
		  LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt1); \
		  int erase; \
		  PredEntry *ap = cl->ClPred; \
		  PELOCK(9,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdCl(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} \
	      } else { \
		DynamicClause *cl = ClauseFlagsToDynamicClause(pt1); \
		int erase; \
		LOCK(cl->ClLock); \
		DEC_CLREF_COUNT(cl); \
		erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		UNLOCK(cl->ClLock); \
		if (erase) { \
		  saveregs(); \
		  Yap_ErCl(cl); \
		  setregs(); \
		} \
	      } \
	    } \
	    continue; \
	  } \
	else { \
	  CELL *pt = RepAppl(d1); \
	  pt[0] = TrailTerm(pt0-1); \
	  pt0 -= 2; \
	  continue; \
	} \
	ENDCACHE_TR(); \
      } \
    }
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define LBL_FAIL_PAIRTERM_END_APPL \
	    flags = *pt1; \
	    if (FlagOn(DBClMask, flags)) { \
	      DBRef dbr = DBStructFlagsToDBStruct(pt1); \
	      int erase; \
	      LOCK(dbr->lock); \
	      DEC_DBREF_COUNT(dbr); \
	      erase = (dbr->Flags & ErasedMask) && (dbr->ref_count == 0); \
	      UNLOCK(dbr->lock); \
	      if (erase) { \
		saveregs(); \
		Yap_ErDBE(dbr); \
		setregs(); \
	      } \
	    } else { \
	      if (flags & LogUpdMask) { \
		if (flags & IndexMask) { \
		  LogUpdIndex *cl = ClauseFlagsToLogUpdIndex(pt1); \
		  int erase; \
		  PredEntry *ap = cl->ClPred; \
		  PELOCK(8,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdIndex(cl); \
		    setregs(); \
		  } else if (cl->ClFlags & DirtyMask) { \
		    saveregs(); \
		    Yap_CleanUpIndex(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} else { \
		  LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt1); \
		  int erase; \
		  PredEntry *ap = cl->ClPred; \
		  PELOCK(9,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdCl(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} \
	      } else { \
		DynamicClause *cl = ClauseFlagsToDynamicClause(pt1); \
		int erase; \
		LOCK(cl->ClLock); \
		DEC_CLREF_COUNT(cl); \
		erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		UNLOCK(cl->ClLock); \
		if (erase) { \
		  saveregs(); \
		  Yap_ErCl(cl); \
		  setregs(); \
		} \
	      } \
	    } \
	    continue; \
	  } \
	ENDCACHE_TR(); \
      } \
    }
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#else /* PARALLEL_YAP */
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define LBL_FAIL_PAIRTERM_END_APPL \
	    flags = *pt1; \
	    if (FlagOn(DBClMask, flags)) { \
	      DBRef dbr = DBStructFlagsToDBStruct(pt1); \
	      int erase; \
	      LOCK(dbr->lock); \
	      DEC_DBREF_COUNT(dbr); \
	      erase = (dbr->Flags & ErasedMask) && (dbr->ref_count == 0); \
	      UNLOCK(dbr->lock); \
	      if (erase) { \
		saveregs(); \
		Yap_ErDBE(dbr); \
		setregs(); \
	      } \
	    } else { \
	      if (flags & LogUpdMask) { \
		if (flags & IndexMask) { \
		  LogUpdIndex *cl = ClauseFlagsToLogUpdIndex(pt1); \
		  int erase; \
		  PELOCK(8,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdIndex(cl); \
		    setregs(); \
		  } else if (cl->ClFlags & DirtyMask) { \
		    saveregs(); \
		    Yap_CleanUpIndex(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} else { \
		  LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt1); \
		  int erase; \
		  PELOCK(9,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdCl(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} \
	      } else { \
		DynamicClause *cl = ClauseFlagsToDynamicClause(pt1); \
		int erase; \
		LOCK(cl->ClLock); \
		DEC_CLREF_COUNT(cl); \
		erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		UNLOCK(cl->ClLock); \
		if (erase) { \
		  saveregs(); \
		  Yap_ErCl(cl); \
		  setregs(); \
		} \
	      } \
	    } \
	    continue; \
	  } \
	else { \
	  CELL *pt = RepAppl(d1); \
	  --pt0; \
	  pt[0] = TrailVal(pt0); \
	  continue; \
	} \
	ENDCACHE_TR(); \
      } \
    }
#else /* FROZEN_STACKS */
#define LBL_FAIL_PAIRTERM_END_APPL \
	    flags = *pt1; \
	    if (FlagOn(DBClMask, flags)) { \
	      DBRef dbr = DBStructFlagsToDBStruct(pt1); \
	      int erase; \
	      LOCK(dbr->lock); \
	      DEC_DBREF_COUNT(dbr); \
	      erase = (dbr->Flags & ErasedMask) && (dbr->ref_count == 0); \
	      UNLOCK(dbr->lock); \
	      if (erase) { \
		saveregs(); \
		Yap_ErDBE(dbr); \
		setregs(); \
	      } \
	    } else { \
	      if (flags & LogUpdMask) { \
		if (flags & IndexMask) { \
		  LogUpdIndex *cl = ClauseFlagsToLogUpdIndex(pt1); \
		  int erase; \
		  PELOCK(8,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdIndex(cl); \
		    setregs(); \
		  } else if (cl->ClFlags & DirtyMask) { \
		    saveregs(); \
		    Yap_CleanUpIndex(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} else { \
		  LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt1); \
		  int erase; \
		  PELOCK(9,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdCl(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} \
	      } else { \
		DynamicClause *cl = ClauseFlagsToDynamicClause(pt1); \
		int erase; \
		LOCK(cl->ClLock); \
		DEC_CLREF_COUNT(cl); \
		erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		UNLOCK(cl->ClLock); \
		if (erase) { \
		  saveregs(); \
		  Yap_ErCl(cl); \
		  setregs(); \
		} \
	      } \
	    } \
	    continue; \
	  } \
	else { \
	  CELL *pt = RepAppl(d1); \
	  pt[0] = TrailTerm(pt0-1); \
	  pt0 -= 2; \
	  continue; \
	} \
	ENDCACHE_TR(); \
      } \
    }
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define LBL_FAIL_PAIRTERM_END_APPL \
	    flags = *pt1; \
	    if (FlagOn(DBClMask, flags)) { \
	      DBRef dbr = DBStructFlagsToDBStruct(pt1); \
	      int erase; \
	      LOCK(dbr->lock); \
	      DEC_DBREF_COUNT(dbr); \
	      erase = (dbr->Flags & ErasedMask) && (dbr->ref_count == 0); \
	      UNLOCK(dbr->lock); \
	      if (erase) { \
		saveregs(); \
		Yap_ErDBE(dbr); \
		setregs(); \
	      } \
	    } else { \
	      if (flags & LogUpdMask) { \
		if (flags & IndexMask) { \
		  LogUpdIndex *cl = ClauseFlagsToLogUpdIndex(pt1); \
		  int erase; \
		  PELOCK(8,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdIndex(cl); \
		    setregs(); \
		  } else if (cl->ClFlags & DirtyMask) { \
		    saveregs(); \
		    Yap_CleanUpIndex(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} else { \
		  LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt1); \
		  int erase; \
		  PELOCK(9,ap); \
		  DEC_CLREF_COUNT(cl); \
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		  if (erase) { \
		    saveregs(); \
		    Yap_ErLogUpdCl(cl); \
		    setregs(); \
		  } \
		  UNLOCK(ap->PELock); \
		} \
	      } else { \
		DynamicClause *cl = ClauseFlagsToDynamicClause(pt1); \
		int erase; \
		LOCK(cl->ClLock); \
		DEC_CLREF_COUNT(cl); \
		erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount); \
		UNLOCK(cl->ClLock); \
		if (erase) { \
		  saveregs(); \
		  Yap_ErCl(cl); \
		  setregs(); \
		} \
	      } \
	    } \
	    continue; \
	  } \
	ENDCACHE_TR(); \
      } \
    }
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#endif /* PARALLEL_YAP */
#else /* MULTIPLE_STACKS */
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define LBL_FAIL_PAIRTERM_END_APPL \
	    flags = *pt1; \
	    ResetFlag(InUseMask, flags); \
	    *pt1 = flags; \
	    if (FlagOn((ErasedMask|DirtyMask), flags)) { \
	      if (FlagOn(DBClMask, flags)) { \
		saveregs(); \
		Yap_ErDBE(DBStructFlagsToDBStruct(pt1)); \
		setregs(); \
	      } else { \
		saveregs(); \
		if (flags & LogUpdMask) { \
		  if (flags & IndexMask) { \
		    if (FlagOn(ErasedMask, flags)) { \
		      Yap_ErLogUpdIndex(ClauseFlagsToLogUpdIndex(pt1)); \
		    } else { \
		      Yap_CleanUpIndex(ClauseFlagsToLogUpdIndex(pt1)); \
		    } \
		  } else { \
		    Yap_ErLogUpdCl(ClauseFlagsToLogUpdClause(pt1)); \
		  } \
		} else { \
		  Yap_ErCl(ClauseFlagsToDynamicClause(pt1)); \
		} \
		setregs(); \
	      } \
	    } \
	    continue; \
	  } \
	else { \
	  CELL *pt = RepAppl(d1); \
	  --pt0; \
	  pt[0] = TrailVal(pt0); \
	  continue; \
	} \
	ENDCACHE_TR(); \
      } \
    }
#else /* FROZEN_STACKS */
#define LBL_FAIL_PAIRTERM_END_APPL \
	    flags = *pt1; \
	    ResetFlag(InUseMask, flags); \
	    *pt1 = flags; \
	    if (FlagOn((ErasedMask|DirtyMask), flags)) { \
	      if (FlagOn(DBClMask, flags)) { \
		saveregs(); \
		Yap_ErDBE(DBStructFlagsToDBStruct(pt1)); \
		setregs(); \
	      } else { \
		saveregs(); \
		if (flags & LogUpdMask) { \
		  if (flags & IndexMask) { \
		    if (FlagOn(ErasedMask, flags)) { \
		      Yap_ErLogUpdIndex(ClauseFlagsToLogUpdIndex(pt1)); \
		    } else { \
		      Yap_CleanUpIndex(ClauseFlagsToLogUpdIndex(pt1)); \
		    } \
		  } else { \
		    Yap_ErLogUpdCl(ClauseFlagsToLogUpdClause(pt1)); \
		  } \
		} else { \
		  Yap_ErCl(ClauseFlagsToDynamicClause(pt1)); \
		} \
		setregs(); \
	      } \
	    } \
	    continue; \
	  } \
	else { \
	  CELL *pt = RepAppl(d1); \
	  pt[0] = TrailTerm(pt0-1); \
	  pt0 -= 2; \
	  continue; \
	} \
	ENDCACHE_TR(); \
      } \
    }
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define LBL_FAIL_PAIRTERM_END_APPL \
	    flags = *pt1; \
	    ResetFlag(InUseMask, flags); \
	    *pt1 = flags; \
	    if (FlagOn((ErasedMask|DirtyMask), flags)) { \
	      if (FlagOn(DBClMask, flags)) { \
		saveregs(); \
		Yap_ErDBE(DBStructFlagsToDBStruct(pt1)); \
		setregs(); \
	      } else { \
		saveregs(); \
		if (flags & LogUpdMask) { \
		  if (flags & IndexMask) { \
		    if (FlagOn(ErasedMask, flags)) { \
		      Yap_ErLogUpdIndex(ClauseFlagsToLogUpdIndex(pt1)); \
		    } else { \
		      Yap_CleanUpIndex(ClauseFlagsToLogUpdIndex(pt1)); \
		    } \
		  } else { \
		    Yap_ErLogUpdCl(ClauseFlagsToLogUpdClause(pt1)); \
		  } \
		} else { \
		  Yap_ErCl(ClauseFlagsToDynamicClause(pt1)); \
		} \
		setregs(); \
	      } \
	    } \
	    continue; \
	  } \
	ENDCACHE_TR(); \
      } \
    }
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#endif /* MULTIPLE_STACKS */

#define LBL_FAIL_END \
      BLOCK = (CELL)LBL_FAIL_END;
