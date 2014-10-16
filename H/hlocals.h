
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
  CELL*  GSplit_;
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
  char*  ScannerStack_;
  struct scanner_extra_alloc*  ScannerExtraBlocks_;
  struct DB_TERM*  BallTerm_;
  UInt  MaxActiveSignals_;
  uint64_t  Signals_;
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
  CELL*  OpenArray_;

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
  CELL*  extra_gc_cells_;
  CELL*  extra_gc_cells_base_;
  CELL*  extra_gc_cells_top_;
  UInt  extra_gc_cells_size_;
  struct array_entry*  DynamicArrays_;
  struct static_array_entry*  StaticArrays_;
  struct global_entry*  GlobalVariables_;
  int  AllowRestart_;

  struct mem_blk*  CMemFirstBlock_;
  UInt  CMemFirstBlockSz_;

  int  nperm_;

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

  ADDR  LocalBase_;
  ADDR  GlobalBase_;
  ADDR  TrailBase_;
  ADDR  TrailTop_;
  char*  ErrorMessage_;
  Term  Error_Term_;
#ifdef THREADS
  Term  Error_TYPE_;
#else
  yap_error_number  Error_TYPE_;
#endif	
  UInt  Error_Size_;
  char  ErrorSay_[MAX_ERROR_MSG_SIZE];
  jmp_buf  IOBotch_;
  TokEntry*  tokptr_;
  TokEntry*  toktide_;
  VarEntry*  VarTable_;
  VarEntry*  AnonVarTable_;
  Term  Comments_;
  CELL*  CommentsTail_;
  CELL*  CommentsNextChar_;
  wchar_t*  CommentsBuff_;
  size_t  CommentsBuffPos_;
  size_t  CommentsBuffLim_;
  sigjmp_buf  RestartEnv_;
  char  FileNameBuf_[YAP_FILENAME_MAX];
  char  FileNameBuf2_[YAP_FILENAME_MAX];

  Int  PrologMode_;
  int  CritLocks_;


#ifdef ANALYST
  YAP_ULONG_LONG  opcount_[_std_top+1];
  YAP_ULONG_LONG  2opcount[_std_top+1][_std_top+1]_;
#endif /* ANALYST */

  struct db_globs*  s_dbg_;

  yap_error_number  matherror_;
  Term  mathtt_;
  char*  mathstring_;
  yap_error_number  CurrentError_;

  int  heap_overflows_;
  Int  total_heap_overflow_time_;
  int  stack_overflows_;
  Int  total_stack_overflow_time_;
  int  delay_overflows_;
  Int  total_delay_overflow_time_;
  int  trail_overflows_;
  Int  total_trail_overflow_time_;
  int  atom_table_overflows_;
  Int  total_atom_table_overflow_time_;

#ifdef LOAD_DYLD
  int  dl_errno_;
#endif

#ifdef LOW_LEVEL_TRACER
  int  do_trace_primitives_;
#endif

  struct export_atom_hash_entry_struct  *ExportAtomHashChain_;
  UInt  ExportAtomHashTableSize_;
  UInt  ExportAtomHashTableNum_;
  struct export_functor_hash_entry_struct  *ExportFunctorHashChain_;
  UInt  ExportFunctorHashTableSize_;
  UInt  ExportFunctorHashTableNum_;
  struct export_pred_entry_hash_entry_struct  *ExportPredEntryHashChain_;
  UInt  ExportPredEntryHashTableSize_;
  UInt  ExportPredEntryHashTableNum_;
  struct export_dbref_hash_entry_struct  *ExportDBRefHashChain_;
  UInt  ExportDBRefHashTableSize_;
  UInt  ExportDBRefHashTableNum_;
  struct import_atom_hash_entry_struct  **ImportAtomHashChain_;
  UInt  ImportAtomHashTableSize_;
  UInt  ImportAtomHashTableNum_;
  struct import_functor_hash_entry_struct  **ImportFunctorHashChain_;
  UInt  ImportFunctorHashTableSize_;
  UInt  ImportFunctorHashTableNum_;
  struct import_opcode_hash_entry_struct  **ImportOPCODEHashChain_;
  UInt  ImportOPCODEHashTableSize_;
  struct import_pred_entry_hash_entry_struct  **ImportPredEntryHashChain_;
  UInt  ImportPredEntryHashTableSize_;
  UInt  ImportPredEntryHashTableNum_;
  struct import_dbref_hash_entry_struct  **ImportDBRefHashChain_;
  UInt  ImportDBRefHashTableSize_;
  UInt  ImportDBRefHashTableNum_;
  yamop  *ImportFAILCODE_;
  Functor  FunctorVar_;
#if __ANDROID__

  struct AAssetManager*  assetManager_;
  char*  InAssetDir_;
#endif

  UInt  ibnds_[256];
  struct index_t*  exo_it_;
  CELL*  exo_base_;
  UInt  exo_arity_;
  UInt  exo_arg_;

  struct scan_atoms*  search_atoms_;

  yhandle_t  CurSlot_;
  Term  SourceModule_;
  size_t  MAX_SIZE_;
} w_local;
