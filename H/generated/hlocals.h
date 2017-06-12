
  /* This file, hlocals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update H/LOCALS instead */

// Stuff that must be considered local to a thread or worker
typedef struct worker_local {
// Streams
  int  c_input_stream_;
  int  c_output_stream_;
  int  c_error_stream_;
  bool  sockets_io_;
  bool  within_print_message_;
//
// Used by the prompts to check if they are after a newline, and then a
// prompt should be output, or if we are in the middle of a line.
//
  bool  newline_;
  Atom  AtPrompt_;
  char  Prompt_[MAX_PROMPT+1];
  encoding_t  encoding_;
  bool  quasi_quotations_;
  UInt  default_priority_;
  bool  eot_before_eof_;
  UInt  max_depth_;
  UInt  max_list_;
  UInt  max_write_args_;
// Restore info
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
// Reduction counters
  YAP_ULONG_LONG  ReductionsCounter_;
  YAP_ULONG_LONG  PredEntriesCounter_;
  YAP_ULONG_LONG  RetriesCounter_;
  int  ReductionsCounterOn_;
  int  PredEntriesCounterOn_;
  int  RetriesCounterOn_;
// support for consulting files
/* current consult stack */
  union CONSULT_OBJ*  ConsultSp_;
/* current maximum number of cells in consult stack */
  UInt  ConsultCapacity_;
/* top of consult stack  */
  union CONSULT_OBJ*  ConsultBase_;
/* low-water mark for consult  */
  union CONSULT_OBJ*  ConsultLow_;
  Term  VarNames_;
  Atom  SourceFileName_;
  UInt  SourceFileLineno_;
//global variables
  Term  GlobalArena_;
  UInt  GlobalArenaOverflows_;
  Int  ArenaOverflows_;
  Int  DepthArenas_;
  struct pred_entry*  LastAssertedPred_;
  struct pred_entry*  TmpPred_;
  char*  ScannerStack_;
  struct scanner_extra_alloc*  ScannerExtraBlocks_;
/// worker control information
/// stack limit after which the stack is managed by C-code.
  Int  CBorder_;
/// max number of signals (uint64_t)
  UInt  MaxActiveSignals_;
/// actual life signals
  uint64_t  Signals_;
/// indexing help data?
  UInt  IPredArity_;
  yamop*  ProfEnd_;
  int  DoingUndefp_;
  Int  StartCharCount_;
  Int  StartLineCount_;
  Int  StartLinePos_;
  scratch_block  ScratchPad_;
#ifdef  COROUTINING
  Term  WokenGoals_;
  Term  AttsMutableList_;
#endif
// gc_stuff
  Term  GcGeneration_;
  Term  GcPhase_;
  UInt  GcCurrentPhase_;
  UInt  GcCalls_;
  Int  TotGcTime_;
  YAP_ULONG_LONG  TotGcRecovered_;
  Int  LastGcTime_;
  Int  LastSSTime_;
  CELL*  OpenArray_;
/* in a single gc */
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
  sigjmp_buf*  gc_restore_;
  CELL*  extra_gc_cells_;
  CELL*  extra_gc_cells_base_;
  CELL*  extra_gc_cells_top_;
  UInt  extra_gc_cells_size_;
  struct array_entry*  DynamicArrays_;
  struct static_array_entry*  StaticArrays_;
  struct global_entry*  GlobalVariables_;
  int  AllowRestart_;
// Thread Local Area for Fast Storage of Intermediate Compiled Code
  struct mem_blk*  CMemFirstBlock_;
  UInt  CMemFirstBlockSz_;
// Variable used by the compiler to store number of permanent vars in a clause
  int  nperm_;
  int  jMP_;
// Thread Local Area for Labels
  Int*  LabelFirstArray_;
  UInt  LabelFirstArraySz_;
// Thread Local Area for SWI-Prolog emulation routines.
// struct PL_local_data*		PL_local_data_p				=Yap_InitThreadIO(wid)
#ifdef THREADS
  struct thandle  ThreadHandle_;
#endif /* THREADS */
#if defined(YAPOR) || defined(TABLING)
  struct local_optyap_data  optyap_data_;
  UInt  TabMode_;
#endif /* YAPOR || TABLING */
  int  InterruptsDisabled_;
  struct open_query_struct*  execution_;
#if LOW_LEVEL_TRACER
  Int  total_choicepoints_;
#endif
  int  consult_level_;
// Variables related to memory allocation
  ADDR  LocalBase_;
  ADDR  GlobalBase_;
  ADDR  TrailBase_;
  ADDR  TrailTop_;
/* error handling info, designed to be easy to pass to the foreign world */
  yap_error_descriptor_t*  ActiveError_;
/// pointer to an exception term, from throw
  jmp_buf*  IOBotch_;
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
  sigjmp_buf*  RestartEnv_;
  char  FileNameBuf_[YAP_FILENAME_MAX+1];
  char  FileNameBuf2_[YAP_FILENAME_MAX+1];
  struct TextBuffer_manager*  TextBuffer_;
// Prolog State
  UInt  BreakLevel_;
  Int  PrologMode_;
  int  CritLocks_;
// Prolog execution and state flags
  union flagTerm*  Flags_;
  UInt  flagCount_;
//analyst.c
/* used to find out how many instructions of each kind are executed */
#ifdef ANALYST
  YAP_ULONG_LONG  opcount_[_std_top+1];
  YAP_ULONG_LONG  2opcount[_std_top+1][_std_top+1]_;
#endif /* ANALYST */
//dbase.c
  struct db_globs*  s_dbg_;
//eval.c
  Term  mathtt_;
  char*  mathstring_;
//grow.c
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
//load_dyld
#ifdef LOAD_DYLD
  int  dl_errno_;
#endif
//tracer.c
#ifdef LOW_LEVEL_TRACER
  int  do_trace_primitives_;
#endif
//quick loader
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
// exo indexing
  UInt  ibnds_[256];
  struct index_t*  exo_it_;
  CELL*  exo_base_;
  UInt  exo_arity_;
  UInt  exo_arg_;
// atom completion
  struct scan_atoms*  search_atoms_;
  struct pred_entry*  SearchPreds_;
/// Slots Status
  yhandle_t  CurSlot_;
  yhandle_t  FrozenHandles_;
  yhandle_t  NSlots_;
  CELL*  SlotBase_;
// Mutexes
  struct swi_mutex*  Mutexes_;
  Term  SourceModule_;
  Term  Including_;
  size_t  MAX_SIZE_;
/* last call to walltime. */
  uint64_t  LastWTime_;
  void*  shared_;
} w_local;
