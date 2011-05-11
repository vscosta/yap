
  /* This file, hlocals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update misc/LOCALS instead */


typedef struct worker_local {

  int  c_input_stream_;
  int  c_output_stream_;
  int  c_error_stream_;

  CELL*  OldASP_;
  CELL*  OldLCL0_;
  tr_fr_ptr  OldTR_;
  CELL*  OldGlobalBase_;
  CELL*  OldH_;
  CELL*  OldH0_;
  ADDR  OldTrailBase_;
  ADDR  OldTrailTop_;
  ADDR  OldHeapBase_;
  ADDR  OldHeapTop_;
  Int  ClDiff_;
  Int  GDiff_;
  Int  HDiff_;
  Int  GDiff0_;
  Int  GSplit_;
  Int  LDiff_;
  Int  TrDiff_;
  Int  XDiff_;
  Int  DelayDiff_;
  Int  BaseDiff_;

  YAP_ULONG_LONG  ReductionsCounter_;
  YAP_ULONG_LONG  PredEntriesCounter_;
  YAP_ULONG_LONG  RetriesCounter_;
  int  ReductionsCounterOn_;
  int  PredEntriesCounterOn_;
  int  RetriesCounterOn_;


  union CONSULT_OBJ*  ConsultSp_;

  UInt  ConsultCapacity_;

  union CONSULT_OBJ*  ConsultBase_;

  union CONSULT_OBJ*  ConsultLow_;

  Term  GlobalArena_;
  UInt  GlobalArenaOverflows_;
  Int  ArenaOverflows_;
  Int  DepthArenas_;
  int  ArithError_;
  struct pred_entry*  LastAssertedPred_;
  int  DebugOn_;
  char*  ScannerStack_;
  struct scanner_extra_alloc*  ScannerExtraBlocks_;
  struct DB_TERM*  BallTerm_;
  UInt  ActiveSignals_;
  UInt  IPredArity_;
  yamop*  ProfEnd_;
  int  UncaughtThrow_;
  int  DoingUndefp_;
  Int  StartLine_;
  scratch_block  ScratchPad_;
#ifdef  COROUTINING
  Term  WokenGoals_;
  Term  AttsMutableList_;
#endif

  Term  GcGeneration_;
  Term  GcPhase_;
  UInt  GcCurrentPhase_;
  UInt  GcCalls_;
  Int  TotGcTime_;
  YAP_ULONG_LONG  TotGcRecovered_;
  Int  LastGcTime_;
  Int  LastSSTime_;

  Int  total_marked_;
  Int  total_oldies_;
  struct choicept*  current_B_;
  CELL*  prev_HB_;
  CELL*  HGEN_;
  CELL**  iptop_;
#if defined(GC_NO_TAGS)
  char*  bp_;
#endif
  tr_fr_ptr  sTR_;
  tr_fr_ptr  sTR0_;
  tr_fr_ptr  new_TR_;
  struct gc_mark_continuation*  cont_top0_;
  struct gc_mark_continuation*  cont_top_;
  int  discard_trail_entries_;
  gc_ma_hash_entry  gc_ma_hash_table_[GC_MAVARS_HASH_SIZE];
  gc_ma_hash_entry*  gc_ma_h_top_;
  gc_ma_hash_entry*  gc_ma_h_list_;
  UInt  gc_timestamp_;
  ADDR  db_vec_;
  ADDR  db_vec0_;
  struct RB_red_blk_node*  db_root_;
  struct RB_red_blk_node*  db_nil_;
  sigjmp_buf  gc_restore_;
  struct array_entry*  DynamicArrays_;
  struct static_array_entry*  StaticArrays_;
  struct global_entry*  GlobalVariables_;
  int  AllowRestart_;

  struct mem_blk*  CMemFirstBlock_;
  UInt  CMemFirstBlockSz_;

  Int*  LabelFirstArray_;
  UInt  LabelFirstArraySz_;

  struct PL_local_data*  PL_local_data_p_;
#ifdef THREADS
  struct thandle  ThreadHandle_;
#endif /* THREADS */
#if defined(YAPOR) || defined(TABLING)
  struct local_optyap_data  optyap_data_;
#endif /* YAPOR || TABLING */
  int  InterruptsDisabled_;
  struct open_query_struct*  execution_;
#if LOW_LEVEL_TRACER
  Int  total_choicepoints_;
#endif
  int  consult_level_;
#if defined(YAPOR) || defined(THREADS)
  lockvar  SignalLock_;
#endif
} w_local;
