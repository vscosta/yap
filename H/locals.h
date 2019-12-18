/// Thread Local Variables. This file now follows C syntax. 

// Macro support
#ifndef LOCAL
#include "Yap.h"
#include "heap.h"
#define LOCAL(A, B) A B
#define LOCAL_INIT(A, B, C)                                                    \
  A B;                                                                         \
  B = C
#define LOCAL_ARRAY(A, B, C) A B[C]
#define LOCAL_ARRAY_ARRAY(A, B, C,D) A B[C][D]
#define LOCAL_INIT(A, B, C, D) A B[C][D]
#define LOCAL_INITF(A, B, C)                                                   \
  A B;                                                                         \
  C
  #define LOCAL_INIT_RESTORE(A,B,C,D) A B; C; D;
#endif

/// Current bindings for std streams, includes default s
LOCAL_INIT(int, c_input_stream, 0);
LOCAL_INIT(int, c_output_stream, 1);
LOCAL_INIT(int, c_error_stream, 2);
LOCAL_INIT(bool, sockets_io, false);
LOCAL_INIT(bool, within_print_message, false);
//

// Used by the prompts to check if they are after a newline, and then a
// prompt should be output, or if we are in the middle of a line.
//
LOCAL_INIT(bool, newline, true);

LOCAL_INIT(Atom, AtPrompt, AtomNil);
LOCAL_ARRAY(char, Prompt, MAX_PROMPT + 1);

LOCAL_ARRAY(Term, debugger_state, DEBUG_NUMBER_OF_OPTS);

LOCAL_INITF(encoding_t, encoding, Yap_DefaultEncoding());
LOCAL_INIT(bool, quasi_quotations, false);
LOCAL_INIT(UInt, default_priority, 1200);
LOCAL_INIT(bool, eot_before_eof, false);
LOCAL_INIT(UInt, max_depth, 0);
LOCAL_INIT(UInt, max_list, 0);
LOCAL_INIT(UInt, max_write_args, 0);
// Restore info
LOCAL_INIT(CELL *, OldASP, NULL);
LOCAL_INIT(CELL *, OldLCL0, NULL);
LOCAL_INIT(tr_fr_ptr, OldTR, NULL);
LOCAL_INIT(CELL *, OldGlobalBase, NULL);
LOCAL_INIT(CELL *, OldH, NULL);
LOCAL_INIT(CELL *, OldH0, NULL);
LOCAL_INIT(ADDR, OldTrailBase, NULL);
LOCAL_INIT(ADDR, OldTrailTop, NULL);
LOCAL_INIT(ADDR, OldHeapBase, NULL);
LOCAL_INIT(ADDR, OldHeapTop, NULL);
LOCAL_INIT(Int, ClDiff, 0L);
LOCAL_INIT(Int, GDiff, 0L);
LOCAL_INIT(Int, HDiff, 0L);
LOCAL_INIT(Int, GDiff0, 0L);
LOCAL_INIT(CELL *, GSplit, NULL);
LOCAL_INIT(Int, LDiff, 0L);
LOCAL_INIT(Int, TrDiff, 0L);
LOCAL_INIT(Int, XDiff, 0L);
LOCAL_INIT(Int, DelayDiff, 0L);
LOCAL_INIT(Int, BaseDiff, 0L);
// A term containing a copy with all current live registrt
// Reduction counters
LOCAL_INIT(YAP_ULONG_LONG, ReductionsCounter, 0L);                                                
LOCAL_INIT(YAP_ULONG_LONG, PredEntriesCounter, 0L);
LOCAL_INIT(YAP_ULONG_LONG, RetriesCounter, 0L);
LOCAL_INIT(int, ReductionsCounterOn, 0L);
LOCAL_INIT(int, PredEntriesCounterOn, 0L);
LOCAL_INIT(int, RetriesCounterOn, 0L);
// support for consulting files
/* current consult stack */
LOCAL_INIT(union CONSULT_OBJ *, ConsultSp, NULL);
/* current maximum number of cells in consult stack */
LOCAL(UInt, ConsultCapacity);
/* top of consult stack  */
LOCAL_INIT(union CONSULT_OBJ *, ConsultBase, NULL);
/* low-water mark for consult  */
LOCAL_INIT(union CONSULT_OBJ *, ConsultLow, NULL);
LOCAL_INIT(Term, VarNames, ((Term)0));
LOCAL_INIT(Atom, SourceFileName, NULL);
LOCAL_INIT(UInt, SourceFileLineno, 0);
// global variables
LOCAL_INIT_RESTORE(Term, GlobalArena, 0L, TermToGlobalOrAtomAdjust);
LOCAL_INIT(UInt, GlobalArenaOverflows, 0L);
LOCAL_INIT(Int, ArenaOverflows, 0L);
LOCAL_INIT(Int, DepthArenas, 0);
LOCAL_INIT(struct pred_entry *, LastAssertedPred, NULL);
LOCAL_INIT(struct pred_entry *, TmpPred, NULL);
LOCAL_INIT(char *, ScannerStack, NULL);
LOCAL_INIT(struct scanner_extra_alloc *, ScannerExtraBlocks, NULL);

/// worker control information
/// stack limit after which the stack is managed by C-code.
LOCAL_INIT(Int, CBorder, 0);
LOCAL_INIT(yhandle_t , HandleBorder, 1);
/// max number of signals (uint64_t);
LOCAL_INIT(UInt, MaxActiveSignals, 64L);
/// actual life signals
LOCAL_INIT(uint64_t, Signals, 0L);
/// indexing help data?
LOCAL_INIT(UInt, IPredArity, 0L);
LOCAL_INIT(yamop *, ProfEnd, NULL);
LOCAL_INIT(int, DoingUndefp, FALSE);
LOCAL_INIT(Int, StartCharCount, 0L);
LOCAL_INIT(Int, StartLineCount, 0L);
LOCAL_INIT(Int, StartLinePos, 0L);
LOCAL_INITF(scratch_block, ScratchPad, InitScratchPad(wid));
#ifdef COROUTINING
LOCAL_INIT_RESTORE(Term, WokenGoals, 0L, TermToGlobalAdjust);
LOCAL_INIT_RESTORE(Term, AttsMutableList, 0L, TermToGlobalAdjust);
#endif

// gc_stuff
LOCAL_INIT(Term, GcGeneration, 0);
LOCAL_INIT(Term, GcPhase, 0L);
LOCAL_INIT(UInt, GcCurrentPhase, 0L);
LOCAL_INIT(UInt, GcCalls, 0L);
LOCAL_INIT(Int, TotGcTime, 0L);
LOCAL_INIT(YAP_ULONG_LONG, TotGcRecovered, 0L);
LOCAL_INIT(Int, LastGcTime, 0L);
LOCAL_INIT(Int, LastSSTime, 0L);
LOCAL_INIT(CELL *, OpenArray, NULL);
/* in a single gc */
LOCAL_INIT(int, MallocDepth, 0L);
LOCAL_INIT(Int, total_marked, 0L);
LOCAL_INIT(Int, total_oldies, 0L);
LOCAL_INIT(struct choicept *, current_B, NULL);
LOCAL_INIT(CELL *, prev_HB, NULL);
LOCAL_INIT(CELL *, HGEN, NULL);
LOCAL_INIT(CELL **, iptop, NULL);
#if defined(GC_NO_TAGS)
LOCAL_INIT(char *, bp, NULL);
#endif
LOCAL_INIT(tr_fr_ptr, sTR, NULL);
LOCAL_INIT(tr_fr_ptr, sTR0, NULL);
LOCAL_INIT(tr_fr_ptr, new_TR, NULL);
LOCAL_INIT(struct gc_mark_continuation *, cont_top0, NULL);
LOCAL_INIT(struct gc_mark_continuation *, cont_top, NULL);
LOCAL_INIT(int, discard_trail_entries, 0);
LOCAL_ARRAY(gc_ma_hash_entry, gc_ma_hash_table, GC_MAVARS_HASH_SIZE);
LOCAL_INIT(gc_ma_hash_entry *, gc_ma_h_top, NULL);
LOCAL_INIT(gc_ma_hash_entry *, gc_ma_h_list, NULL);
LOCAL_INIT(UInt, gc_timestamp, 0L);
LOCAL_INIT(ADDR, db_vec, NULL);
LOCAL_INIT(ADDR, db_vec0, NULL);
LOCAL_INIT(struct RB_red_blk_node *, db_root, NULL);
LOCAL_INIT(struct RB_red_blk_node *, db_nil, NULL);


/* parser stack, used to be AuxSp, now is Malloc */
LOCAL( CELL *, ParserAuxSp);
LOCAL( CELL *, ParserAuxMax);
LOCAL( CELL *, ParserAuxBase);

LOCAL(sigjmp_buf , gc_restore);
LOCAL(CELL *, extra_gc_cells);
LOCAL(CELL *, extra_gc_cells_base);
LOCAL(CELL *, extra_gc_cells_top);
LOCAL_INIT(UInt, extra_gc_cells_size, 256);
LOCAL_INIT_RESTORE(struct array_entry *, DynamicArrays, NULL, PtoArrayEAdjust);
LOCAL_INIT_RESTORE(struct static_array_entry *, StaticArrays, NULL, PtoArraySAdjust);
LOCAL_INIT_RESTORE(struct global_entry *, GlobalVariables, NULL, PtoGlobalEAdjust);
LOCAL_INIT(int, AllowRestart, FALSE);

// Thread Local Area for Fast Storage of Intermediate Compiled Code
LOCAL_INIT(struct mem_blk *, CMemFirstBlock, NULL);
LOCAL_INIT(UInt, CMemFirstBlockSz, 0L);

// Variable used by the compiler to store number of permanent vars in a clause
LOCAL_INIT(int, nperm, 0);
LOCAL_INIT(int, jMP, 0);
// Thread Local Area for Labels
LOCAL_INIT(Int *, LabelFirstArray, NULL);
LOCAL_INIT(UInt, LabelFirstArraySz, 0L);

// Thread Local Area for SWI-Prolog emulation routines.
// struct LOCAL_INIT( PL_local_data*, PL_local_data_p,  Yap_InitThreadIO(wid));
#ifdef THREADS
LOCAL_INITF(struct thandle, ThreadHandle, InitThreadHandle(wid));
#endif /* THREADS */

#if defined(YAPOR) || defined(TABLING)
LOCAL_INITF(struct local_optyap_data, optyap_data,Yap_init_local_optyap_data(wid));
LOCAL_INIT(UInt, TabMode, 0L);
#endif /* YAPOR || TABLING */

LOCAL_INIT(int, InterruptsDisabled, FALSE);

LOCAL_INIT(struct open_query_struct *, execution, NULL);

#if LOW_LEVEL_TRACER
LOCAL_INIT(Int, total_choicepoints, 0);
#endif

LOCAL_INIT(int, consult_level, 0);

// Variables related to memory allocation
LOCAL(ADDR, LocalBase);
LOCAL(ADDR, GlobalBase);
LOCAL(ADDR, TrailBase);
LOCAL(ADDR, TrailTop);

/* error handling info, designed to be easy to pass to the foreign world */
LOCAL_INIT(yap_error_descriptor_t *, ActiveError, calloc(sizeof(yap_error_descriptor_t), 1));
LOCAL_INIT(yap_error_descriptor_t *, CommittedError, NULL);
LOCAL_INIT(bool, delay, false);
/// pointer to an exception term, from throw
LOCAL(jmp_buf, IOBotch);

/// tokenizer support (should be private to the tokenizer).
LOCAL(TokEntry *, tokptr);
LOCAL(TokEntry *, toktide);
LOCAL(VarEntry *, VarTable);
LOCAL(VarEntry *, AnonVarTable);
LOCAL(VarEntry *, VarList);
LOCAL(VarEntry *, VarTail);
LOCAL(Term, Comments);
LOCAL(CELL *, CommentsTail);
LOCAL(CELL *, CommentsNextChar);
LOCAL(wchar_t *, CommentsBuff);
LOCAL(size_t, CommentsBuffPos);
LOCAL(size_t, CommentsBuffLim);
LOCAL_INIT(sigjmp_buf *, RestartEnv, NULL);
LOCAL_ARRAY(char, FileNameBuf, YAP_FILENAME_MAX + 1);
LOCAL_ARRAY(char, FileNameBuf2, YAP_FILENAME_MAX + 1);
LOCAL_INIT(struct TextBuffer_manager *, TextBuffer, Yap_InitTextAllocator());

// Prolog State
LOCAL_INIT(UInt, BreakLevel, 0);
LOCAL_INIT(Int, PrologMode, BootMode);
LOCAL_INIT(int, CritLocks, 0);

// Prolog execution and state flags
LOCAL(union flagTerm *, Flags);
LOCAL(UInt, flagCount);
// analyst.c
/* used to find out how many instructions of each kind are executed */
#ifdef ANALYST
LOCAL_ARRAY(YAP_ULONG_LONG, opcount, _std_top + 1);
LOCAL_ARRAY_ARRAY(YAP_ULONG_LONG, 2opcount, _std_top + 1, _std_top + 1);
#endif /* ANALYST */

// dbase.c
LOCAL(struct db_globs *, s_dbg);

// eval.c
LOCAL(Term, mathtt);
LOCAL_INIT(char *, mathstring, NULL);
LOCAL_INIT(struct eval_context *, ctx, NULL);


// grow.c
LOCAL_INIT(int, heap_overflows, 0);
LOCAL_INIT(Int, total_heap_overflow_time, 0);
LOCAL_INIT(int, stack_overflows, 0);
LOCAL_INIT(Int, total_stack_overflow_time, 0);
LOCAL_INIT(int, delay_overflows, 0);
LOCAL_INIT(Int, total_delay_overflow_time, 0);
LOCAL_INIT(int, trail_overflows, 0);
LOCAL_INIT(Int, total_trail_overflow_time, 0);
LOCAL_INIT(int, atom_table_overflows, 0);
LOCAL_INIT(Int, total_atom_table_overflow_time, 0);

// load_dyld
#ifdef LOAD_DYLD
LOCAL_INIT(int, dl_errno, 0);
#endif

// tracer.c
#ifdef LOW_LEVEL_TRACER
LOCAL_INIT(int, do_trace_primitives, TRUE);
#endif

// quick loader
LOCAL_INIT(struct export_atom_hash_entry_struct *, ExportAtomHashChain, NULL);
LOCAL_INIT(UInt, ExportAtomHashTableSize, 0);
LOCAL_INIT(UInt, ExportAtomHashTableNum, 0);
LOCAL_INIT(struct export_functor_hash_entry_struct *, ExportFunctorHashChain,NULL);
LOCAL_INIT(UInt, ExportFunctorHashTableSize, 0);
LOCAL_INIT(UInt, ExportFunctorHashTableNum, 0);
LOCAL_INIT(struct export_pred_entry_hash_entry_struct *,ExportPredEntryHashChain, NULL);
LOCAL_INIT(UInt, ExportPredEntryHashTableSize, 0);
LOCAL_INIT(UInt, ExportPredEntryHashTableNum, 0);
LOCAL_INIT(struct export_dbref_hash_entry_struct *, ExportDBRefHashChain, NULL);
LOCAL_INIT(UInt, ExportDBRefHashTableSize, 0);
LOCAL_INIT(UInt, ExportDBRefHashTableNum, 0);
LOCAL_INIT(struct import_atom_hash_entry_struct **, ImportAtomHashChain, NULL);
LOCAL_INIT(UInt, ImportAtomHashTableSize, 0);
LOCAL_INIT(UInt, ImportAtomHashTableNum, 0);
LOCAL_INIT(struct import_functor_hash_entry_struct **, ImportFunctorHashChain, NULL);
LOCAL_INIT(UInt, ImportFunctorHashTableSize, 0);
LOCAL_INIT(UInt, ImportFunctorHashTableNum, 0);
LOCAL_INIT(struct import_opcode_hash_entry_struct **, ImportOPCODEHashChain, NULL);
LOCAL_INIT(UInt, ImportOPCODEHashTableSize, 0);
LOCAL_INIT(struct import_pred_entry_hash_entry_struct **, ImportPredEntryHashChain, NULL);
LOCAL_INIT(UInt, ImportPredEntryHashTableSize, 0);
LOCAL_INIT(UInt, ImportPredEntryHashTableNum, 0);
LOCAL_INIT(struct import_dbref_hash_entry_struct **, ImportDBRefHashChain,   NULL);
LOCAL_INIT(UInt, ImportDBRefHashTableSize, 0);
LOCAL_INIT(UInt, ImportDBRefHashTableNum, 0);
LOCAL_INIT(yamop *, ImportFAILCODE, NULL);

// exo indexing

LOCAL_ARRAY(UInt, ibnds, 256);
LOCAL_INIT(struct index_t *, exo_it, NULL);
LOCAL_INIT(CELL *, exo_base, NULL);
LOCAL_INIT(UInt, exo_arity, 0);
LOCAL_INIT(UInt, exo_arg, 0);

// atom completion
LOCAL(struct scan_atoms *, search_atoms);
LOCAL(struct pred_entry *, SearchPreds);

/// Slots Status
LOCAL_INIT(yhandle_t, CurSlot, 0);
LOCAL_INIT(yhandle_t, FrozenHandles, 0);
LOCAL_INIT(yhandle_t, NSlots, 0);
LOCAL_INIT(CELL *, SlotBase, InitHandles(wid));

// Mutexes
LOCAL_INIT(struct swi_mutex *, Mutexes, NULL);

LOCAL_INIT(Term, SourceModule, 0);
LOCAL_INIT(Term, Including, TermNil);

LOCAL_INIT(size_t, MAX_SIZE, 1024L);

/* last call to walltime. */
LOCAL_INIT(uint64_t, LastWTime, 0);

LOCAL(scratch_sys_struct_t, WorkerBuffer);

