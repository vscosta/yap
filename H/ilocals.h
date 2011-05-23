
  /* This file, ilocals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update misc/LOCALS instead */


static void InitWorker(int wid) {

  REMOTE_c_input_stream(wid) = 0;
  REMOTE_c_output_stream(wid) = 1;
  REMOTE_c_error_stream(wid) = 2;

  REMOTE_OldASP(wid) = NULL;
  REMOTE_OldLCL0(wid) = NULL;
  REMOTE_OldTR(wid) = NULL;
  REMOTE_OldGlobalBase(wid) = NULL;
  REMOTE_OldH(wid) = NULL;
  REMOTE_OldH0(wid) = NULL;
  REMOTE_OldTrailBase(wid) = NULL;
  REMOTE_OldTrailTop(wid) = NULL;
  REMOTE_OldHeapBase(wid) = NULL;
  REMOTE_OldHeapTop(wid) = NULL;
  REMOTE_ClDiff(wid) = 0L;
  REMOTE_GDiff(wid) = 0L;
  REMOTE_HDiff(wid) = 0L;
  REMOTE_GDiff0(wid) = 0L;
  REMOTE_GSplit(wid) = NULL;
  REMOTE_LDiff(wid) = 0L;
  REMOTE_TrDiff(wid) = 0L;
  REMOTE_XDiff(wid) = 0L;
  REMOTE_DelayDiff(wid) = 0L;
  REMOTE_BaseDiff(wid) = 0L;

  REMOTE_ReductionsCounter(wid) = 0L;
  REMOTE_PredEntriesCounter(wid) = 0L;
  REMOTE_RetriesCounter(wid) = 0L;
  REMOTE_ReductionsCounterOn(wid) = 0L;
  REMOTE_PredEntriesCounterOn(wid) = 0L;
  REMOTE_RetriesCounterOn(wid) = 0L;


  REMOTE_ConsultSp(wid) = NULL;



  REMOTE_ConsultBase(wid) = NULL;

  REMOTE_ConsultLow(wid) = NULL;

  REMOTE_GlobalArena(wid) = 0L;
  REMOTE_GlobalArenaOverflows(wid) = 0L;
  REMOTE_ArenaOverflows(wid) = 0L;
  REMOTE_DepthArenas(wid) = 0;
  REMOTE_ArithError(wid) = FALSE;
  REMOTE_LastAssertedPred(wid) = NULL;
  REMOTE_DebugOn(wid) = FALSE;
  REMOTE_ScannerStack(wid) = NULL;
  REMOTE_ScannerExtraBlocks(wid) = NULL;
  REMOTE_BallTerm(wid) = NULL;
  REMOTE_ActiveSignals(wid) = 0L;
  REMOTE_IPredArity(wid) = 0L;
  REMOTE_ProfEnd(wid) = NULL;
  REMOTE_UncaughtThrow(wid) = FALSE;
  REMOTE_DoingUndefp(wid) = FALSE;
  REMOTE_StartLine(wid) = 0L;
  InitScratchPad(wid);
#ifdef  COROUTINING
  REMOTE_WokenGoals(wid) = 0L;
  REMOTE_AttsMutableList(wid) = 0L;
#endif

  REMOTE_GcGeneration(wid) = 0L;
  REMOTE_GcPhase(wid) = 0L;
  REMOTE_GcCurrentPhase(wid) = 0L;
  REMOTE_GcCalls(wid) = 0L;
  REMOTE_TotGcTime(wid) = 0L;
  REMOTE_TotGcRecovered(wid) = 0L;
  REMOTE_LastGcTime(wid) = 0L;
  REMOTE_LastSSTime(wid) = 0L;

  REMOTE_total_marked(wid) = 0L;
  REMOTE_total_oldies(wid) = 0L;
  REMOTE_current_B(wid) = NULL;
  REMOTE_prev_HB(wid) = NULL;
  REMOTE_HGEN(wid) = NULL;
  REMOTE_iptop(wid) = NULL;
#if defined(GC_NO_TAGS)
  REMOTE_bp(wid) = NULL;
#endif
  REMOTE_sTR(wid) = NULL;
  REMOTE_sTR0(wid) = NULL;
  REMOTE_new_TR(wid) = NULL;
  REMOTE_cont_top0(wid) = NULL;
  REMOTE_cont_top(wid) = NULL;
  REMOTE_discard_trail_entries(wid) = 0;

  REMOTE_gc_ma_h_top(wid) = NULL;
  REMOTE_gc_ma_h_list(wid) = NULL;
  REMOTE_gc_timestamp(wid) = 0L;
  REMOTE_db_vec(wid) = NULL;
  REMOTE_db_vec0(wid) = NULL;
  REMOTE_db_root(wid) = NULL;
  REMOTE_db_nil(wid) = NULL;

  REMOTE_DynamicArrays(wid) = NULL;
  REMOTE_StaticArrays(wid) = NULL;
  REMOTE_GlobalVariables(wid) = NULL;
  REMOTE_AllowRestart(wid) = FALSE;

  REMOTE_CMemFirstBlock(wid) = NULL;
  REMOTE_CMemFirstBlockSz(wid) = 0L;

  REMOTE_LabelFirstArray(wid) = NULL;
  REMOTE_LabelFirstArraySz(wid) = 0L;

  REMOTE_PL_local_data_p(wid) = Yap_InitThreadIO(wid);
#ifdef THREADS
  InitThreadHandle(wid);
#endif /* THREADS */
#if defined(YAPOR) || defined(TABLING)
  Yap_init_local_optyap_data(wid);
#endif /* YAPOR || TABLING */
  REMOTE_InterruptsDisabled(wid) = FALSE;
  REMOTE_execution(wid) = NULL;
#if LOW_LEVEL_TRACER
  REMOTE_total_choicepoints(wid) = 0;
#endif
  REMOTE_consult_level(wid) = 0;
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(REMOTE_SignalLock(wid));
#endif

  REMOTE_LocalBase(wid) = REMOTE_LocalBase(0);
  REMOTE_GlobalBase(wid) = REMOTE_GlobalBase(0);
  REMOTE_TrailBase(wid) = REMOTE_TrailBase(0);
  REMOTE_TrailTop(wid) = REMOTE_TrailTop(0);
  REMOTE_ErrorMessage(wid) = REMOTE_ErrorMessage(0);
  REMOTE_Error_Term(wid) = REMOTE_Error_Term(0);
#ifdef THREADS
  REMOTE_Error_TYPE(wid) = REMOTE_Error_TYPE(0);
#else
  REMOTE_Error_TYPE(wid) = REMOTE_Error_TYPE(0);
#endif	
  REMOTE_Error_Size(wid) = REMOTE_Error_Size(0);


  REMOTE_tokptr(wid) = REMOTE_tokptr(0);
  REMOTE_toktide(wid) = REMOTE_toktide(0);
  REMOTE_VarTable(wid) = REMOTE_VarTable(0);
  REMOTE_AnonVarTable(wid) = REMOTE_AnonVarTable(0);



}
