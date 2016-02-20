// Stuff that must be considered local to a thread or worker
// START_WORKER_LOCL

// Streams
LOCL(struct AliasDescS *, FileAliases, Yap_InitStandardAliases())
LOC(int, NOfFileAliases)
LOCL(int, SzOfFileAliases, void)

LOCL(int, c_input_stream, 0)
LOCL(int, c_output_stream, 1)
LOCL(int, c_error_stream, 2)

LOCL(bool, sockets_io, false)

LOCL(bool, within_print_message, false)

//
// Used by the prompts to check if they are after a newline, and then a
// prompt should be output, or if we are in the middle of a line.
//
LOCL(bool, newline, true)

LOCL(Atom, AtPrompt, AtomNil)
LOCN(char, MAX_PROMPT + 1, Prompt)

LOCL(encoding_t, encoding, Yap_DefaultEncoding())
LOCL(bool, quasi_quotations, false)
LOCL(UInt, default_priority, 1200)

LOCL(bool, eot_before_eof, false)
LOCL(UInt, max_depth, 0)
LOCL(UInt, max_list, 0)
LOCL(UInt, max_write_args, 0)

// Restore info
LOCL(CELL *, OldASP, NULL)
LOCL(CELL *, OldLCL0, NULL)
LOCL(tr_fr_ptr, OldTR, NULL)
LOCL(CELL *, OldGlobalBase, NULL)
LOCL(CELL *, OldH, NULL)
LOCL(CELL *, OldH0, NULL)
LOCL(ADDR, OldTrailBase, NULL)
LOCL(ADDR, OldTrailTop, NULL)
LOCL(ADDR, OldHeapBase, NULL)
LOCL(ADDR, OldHeapTop, NULL)
LOCL(Int, ClDiff, 0L)
LOCL(Int, GDiff, 0L)
LOCL(Int, HDiff, 0L)
LOCL(Int, GDiff0, 0L)
LOCL(CELL *, GSplit, NULL)
LOCL(Int, LDiff, 0L)
LOCL(Int, TrDiff, 0L)
LOCL(Int, XDiff, 0L)
LOCL(Int, DelayDiff, 0L)
LOCL(Int, BaseDiff, 0L)

// Reduction counters
LOCL(YAP_ULONG_LONG, ReductionsCounter, 0L)
LOCL(YAP_ULONG_LONG, PredEntriesCounter, 0L)
LOCL(YAP_ULONG_LONG, RetriesCounter, 0L)
LOCL(int, ReductionsCounterOn, 0L)
LOCL(int, PredEntriesCounterOn, 0L)
LOCL(int, RetriesCounterOn, 0L)

// support for consulting files
/* current consult stack */
LOCL(union CONSULT_OBJ *, ConsultSp, NULL)
/* current maximum number of cells in consult stack */
LOC(UInt, ConsultCapacity)
/* top of consult stack  */
LOCL(union CONSULT_OBJ *, ConsultBase, NULL)
/* low-water mark for consult  */
LOCL(union CONSULT_OBJ *, ConsultLow, NULL)
LOCL(Term, VarNames, ((Term)0))
LOCL(Atom, SourceFileName, NULL)
LOCL(UInt, SourceFileLineno, 0)

// global variables
     LOCLR(Term, GlobalArena, 0L, TermToGlobalOrAtomAdjust() )
LOCL(UInt, GlobalArenaOverflows, 0L)
LOCL(Int, ArenaOverflows, 0L)
LOCL(Int, DepthArenas, 0)

LOCL(int, ArithError, FALSE)
LOCL(struct pred_entry *, LastAssertedPred, NULL)
LOCL(struct pred_entry *, TmpPred, NULL)
LOCL(char *, ScannerStack, NULL)
LOCL(struct scanner_extra_alloc *, ScannerExtraBlocks, NULL)
LOCLR(struct DB_TERM *, BallTerm, NULL, RestoreBallTerm(wid))
LOCL(UInt, MaxActiveSignals, 64L)
LOCL(uint64_t, Signals, 0L)
LOCL(UInt, IPredArity, 0L)
LOCL(yamop *, ProfEnd, NULL)
LOCL(int, UncaughtThrow, FALSE)
LOCL(int, DoingUndefp, FALSE)
LOCL(Int, StartCharCount, 0L)
LOCL(Int, StartLineCount, 0L)
LOCL(Int, StartLinePos, 0L)
LOCL(scratch_block, ScratchPad, InitScratchPad(wid))
#ifdef COROUTINING
     LOCLR(Term, WokenGoals, 0L, TermToGlobalAdjust() )
     LOCLR(Term, AttsMutableList, 0L, TermToGlobalAdjust() )
#endif

// gc_stuff
LOCLR(Term, GcGeneration, 0L, TermToGlobalAdjust() )
LOCLR(Term, GcPhase, 0L, TermToGlobalAdjust() )
LOCL(UInt, GcCurrentPhase, 0L)
LOCL(UInt, GcCalls, 0L)
LOCL(Int, TotGcTime, 0L)
LOCL(YAP_ULONG_LONG, TotGcRecovered, 0L)
LOCL(Int, LastGcTime, 0L)
LOCL(Int, LastSSTime, 0L)
LOCL(CELL *, OpenArray, NULL)

/* in a single gc */
LOCL(Int, total_marked, 0L)
LOCL(Int, total_oldies, 0L)
LOCL(struct choicept *, current_B, NULL)
LOCL(CELL *, prev_HB, NULL)
LOCL(CELL *, HGEN, NULL)
LOCL(CELL **, iptop, NULL)

#if defined(GC_NO_TAGS)
LOCL(char *, bp, NULL)
#endif
LOCL(tr_fr_ptr, sTR, NULL)
LOCL(tr_fr_ptr, sTR0, NULL)
LOCL(tr_fr_ptr, new_TR, NULL)
LOCL(struct gc_mark_continuation *, cont_top0, NULL)
LOCL(struct gc_mark_continuation *, cont_top, NULL)
LOCL(int, discard_trail_entries, 0)
LOCN(gc_ma_hash_entry, GC_MAVARS_HASH_SIZE, gc_ma_hash_table)
LOCL(gc_ma_hash_entry *, gc_ma_h_top, NULL)
LOCL(gc_ma_hash_entry *, gc_ma_h_list, NULL)
LOCL(UInt, gc_timestamp, 0L)
LOCL(ADDR, db_vec, NULL)
LOCL(ADDR, db_vec0, NULL)
LOCL(struct RB_red_blk_node *, db_root, NULL)
LOCL(struct RB_red_blk_node *, db_nil, NULL)

LOC(sigjmp_buf, gc_restore)
LOC(CELL *, extra_gc_cells)
LOC(CELL *, extra_gc_cells_base)
LOC(CELL *, extra_gc_cells_top)
LOCN(UInt, 256, extra_gc_cells_size)
LOCLR(struct array_entry *, DynamicArrays, NULL, PtoArrayEAdjust)
LOCLR(struct static_array_entry *, StaticArrays, NULL, PtoArraySAdjust)
LOCLR(struct global_entry *, GlobalVariables, NULL, PtoGlobalEAdjust)
LOCL(int, AllowRestart, FALSE)

// Thread Local Area for Fast Storage of Intermediate Compiled Code
LOCL(struct mem_blk *, CMemFirstBlock, NULL)
LOCL(UInt, CMemFirstBlockSz, 0L)

// Variable used by the compiler to store number of permanent vars in a clause
LOCL(int, nperm, 0L)

// Thread Local Area for Labels
LOCL(Int *, LabelFirstArray, NULL)
LOCL(UInt, LabelFirstArraySz, 0L)

// Thread Local Area for SWI-Prolog emulation routines.
// struct PL_local_data*, PL_local_data_p, Yap_InitThreadIO(wid)

#ifdef THREADS
LOCL(struct thandle, ThreadHandle, InitThreadHandle(wid))
#endif /* THREADS */

#if defined(YAPOR) || defined(TABLING)
LOCL(struct local_optyap_data, optyap_data, Yap_init_local_optyap_data(wid))
LOCL(UInt, TabMode, 0L)
#endif /* YAPOR || TABLING */

LOCL(int, InterruptsDisabled, FALSE)

LOCL(struct open_query_struct *, execution, NULL)

#if LOW_LEVEL_TRACER
LOCL(Int, total_choicepoints, 0)
#endif

LOCL(int, consult_level, 0)

// Variables related to memory allocation
LOC(ADDR, LocalBase)
LOC(ADDR, GlobalBase)
LOC(ADDR, TrailBase)
LOC(ADDR, TrailTop)
LOC(char *, ErrorMessage)
LOC(Term, Error_Term)
LOC(yap_error_number, Error_TYPE)
LOC(const char *, Error_File)
LOC(const char *, Error_Function)
LOC(int, Error_Lineno)
LOC(size_t, Error_Size)
LOCN(char, MAX_ERROR_MSG_SIZE, ErrorSay)
LOC(jmp_buf, IOBotch)
LOC(TokEntry *, tokptr)
LOC(TokEntry *, toktide)
LOC(VarEntry *, VarTable)
LOC(VarEntry *, AnonVarTable)
LOC(Term, Comments)
LOC(CELL *, CommentsTail)
LOC(CELL *, CommentsNextChar)
LOC(wchar_t *, CommentsBuff)
LOC(size_t, CommentsBuffPos)
LOC(size_t, CommentsBuffLim)
LOC(sigjmp_buf, RestartEnv)
LOCN(char, YAP_FILENAME_MAX, FileNameBuf)
LOCN(char, YAP_FILENAME_MAX, FileNameBuf2)

// Prolog State
LOCL(UInt, BreakLevel, 0)
LOCL(Int, PrologMode, BootMode)
LOCL(int, CritLocks, 0)

// Prolog execution and state flags
LOC(union flagTerm *, Flags)
LOC(UInt, flagCount)

// analyst.c
/* used to find out how many instructions of each kind are executed */
#ifdef ANALYST
LOC(YAP_ULONG_LONG, opcount[_std_top + 1])
LOC(YAP_ULONG_LONG, 2opcount [_std_top + 1][_std_top + 1])
#endif /* ANALYST */

// dbase.c
LOC(struct db_globs *, s_dbg)

// eval.c
LOCL(yap_error_number, matherror, YAP_NO_ERROR)
LOCL(Term, mathtt, NULL)
LOCL(char *, mathstring, NULL)
LOCL(yap_error_number, CurrentError, YAP_NO_ERROR)

// grow.c
LOCL(int, heap_overflows, 0)
LOCL(Int, total_heap_overflow_time, 0)
LOCL(int, stack_overflows, 0)
LOCL(Int, total_stack_overflow_time, 0)
LOCL(int, delay_overflows, 0)
LOCL(Int, total_delay_overflow_time, 0)
LOCL(int, trail_overflows, 0)
LOCL(Int, total_trail_overflow_time, 0)
LOCL(int, atom_table_overflows, 0)
LOCL(Int, total_atom_table_overflow_time, 0)

// load_dyld
#ifdef LOAD_DYLD
LOCL(int, dl_errno, 0)
#endif

// tracer.c
#ifdef LOW_LEVEL_TRACER
LOCL(int, do_trace_primitives, TRUE)
#endif

// quick loader
LOCL(struct export_atom_hash_entry_struct *, ExportAtomHashChain, NULL)
LOCL(UInt, ExportAtomHashTableSize, 0)
LOCL(UInt, ExportAtomHashTableNum, 0)
LOCL(struct export_functor_hash_entry_struct *, ExportFunctorHashChain, NULL)
LOCL(UInt, ExportFunctorHashTableSize, 0)
LOCL(UInt, ExportFunctorHashTableNum, 0)
LOCL(struct export_pred_entry_hash_entry_struct *, ExportPredEntryHashChain,
      NULL)
LOCL(UInt, ExportPredEntryHashTableSize, 0)
LOCL(UInt, ExportPredEntryHashTableNum, 0)
LOCL(struct export_dbref_hash_entry_struct *, ExportDBRefHashChain, NULL)
LOCL(UInt, ExportDBRefHashTableSize, 0)
LOCL(UInt, ExportDBRefHashTableNum, 0)
LOCL(struct import_atom_hash_entry_struct **, ImportAtomHashChain, NULL)
LOCL(UInt, ImportAtomHashTableSize, 0)
LOCL(UInt, ImportAtomHashTableNum, 0)
LOCL(struct import_functor_hash_entry_struct **, ImportFunctorHashChain, NULL)
LOCL(UInt, ImportFunctorHashTableSize, 0)
LOCL(UInt, ImportFunctorHashTableNum, 0)
LOCL(struct import_opcode_hash_entry_struct **, ImportOPCODEHashChain, NULL)
LOCL(UInt, ImportOPCODEHashTableSize, 0)
LOCL(struct import_pred_entry_hash_entry_struct **, ImportPredEntryHashChain,
      NULL)
LOCL(UInt, ImportPredEntryHashTableSize, 0)
LOCL(UInt, ImportPredEntryHashTableNum, 0)
LOCL(struct import_dbref_hash_entry_struct **, ImportDBRefHashChain, NULL)
LOCL(UInt, ImportDBRefHashTableSize, 0)
LOCL(UInt, ImportDBRefHashTableNum, 0)
LOCL(yamop *, ImportFAILCODE, NULL)

#if __ANDROID__
// current virtual directory.
LOCL(struct AAssetManager *assetManager, GLOBAL_assetManager)
LOCL(char *, InAssetDir, NULL)
#endif

// exo indexing

LOCN(UInt, 256, ibnds)
LOCL(struct index_t *, exo_it, NULL)
LOCL(CELL *, exo_base, NULL)
LOCL(UInt, exo_arity, 0)
LOCL(UInt, exo_arg, 0)

// atom completion
LOC(struct scan_atoms *, search_atoms)

// Slots
LOCL(yhandle_t, CurSlot, 0)
LOCL(yhandle_t, NSlots, 0)
LOCL(CELL *, SlotBase, InitHandles(wid))

// Mutexes
LOCL(struct swi_mutex *, Mutexes, NULL)

LOCL(Term, SourceModule, 0)
LOCL(Term, Including, TermNil)

LOCL(size_t, MAX_SIZE, 1024L)
