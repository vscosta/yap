/// Thread Local Variables. This file now follows C syntax. 
// Macro support
#ifndef LOCAL
#include "Yap.h"
#include "heap.h"
#define LOCAL(A, B) A B
#define LOCAL_INIT(A, B, C)                                                      A B                                                                           B = C
#define LOCAL_ARRAY(A, B, C) A B[C]
#define LOCAL_ARRAY_ARRAY(A, B, C,D) A B[C][D]
#define LOCAL_INIT(A, B, C, D) A B[C][D]
#define LOCAL_INITF(A, B, C)                                                     A B                                                                           C
  #define LOCAL_INIT_RESTORE(A,B,C,D) A B C D
#endif
/// Current bindings for std streams, includes default s
#define LOCAL_c_input_stream (Yap_local.c_input_stream)
#define REMOTE_c_input_stream(wid) (REMOTE(wid)->c_input_stream)

#define LOCAL_c_output_stream (Yap_local.c_output_stream)
#define REMOTE_c_output_stream(wid) (REMOTE(wid)->c_output_stream)

#define LOCAL_c_error_stream (Yap_local.c_error_stream)
#define REMOTE_c_error_stream(wid) (REMOTE(wid)->c_error_stream)

#define LOCAL_sockets_io (Yap_local.sockets_io)
#define REMOTE_sockets_io(wid) (REMOTE(wid)->sockets_io)

#define LOCAL_within_print_message (Yap_local.within_print_message)
#define REMOTE_within_print_message(wid) (REMOTE(wid)->within_print_message)

//
// Used by the prompts to check if they are after a newline, and then a
// prompt should be output, or if we are in the middle of a line.
//
#define LOCAL_newline (Yap_local.newline)
#define REMOTE_newline(wid) (REMOTE(wid)->newline)

#define LOCAL_AtPrompt (Yap_local.AtPrompt)
#define REMOTE_AtPrompt(wid) (REMOTE(wid)->AtPrompt)

#define LOCAL_Prompt (Yap_local.Prompt)
#define REMOTE_Prompt(wid) (REMOTE(wid)->Prompt)

#define LOCAL_encoding (Yap_local.encoding)
#define REMOTE_encoding(wid) (REMOTE(wid)->encoding)

#define LOCAL_quasi_quotations (Yap_local.quasi_quotations)
#define REMOTE_quasi_quotations(wid) (REMOTE(wid)->quasi_quotations)

#define LOCAL_default_priority (Yap_local.default_priority)
#define REMOTE_default_priority(wid) (REMOTE(wid)->default_priority)

#define LOCAL_eot_before_eof (Yap_local.eot_before_eof)
#define REMOTE_eot_before_eof(wid) (REMOTE(wid)->eot_before_eof)

#define LOCAL_max_depth (Yap_local.max_depth)
#define REMOTE_max_depth(wid) (REMOTE(wid)->max_depth)

#define LOCAL_max_list (Yap_local.max_list)
#define REMOTE_max_list(wid) (REMOTE(wid)->max_list)

#define LOCAL_max_write_args (Yap_local.max_write_args)
#define REMOTE_max_write_args(wid) (REMOTE(wid)->max_write_args)

// Restore info
#define LOCAL_OldASP (Yap_local.OldASP)
#define REMOTE_OldASP(wid) (REMOTE(wid)->OldASP)

#define LOCAL_OldLCL0 (Yap_local.OldLCL0)
#define REMOTE_OldLCL0(wid) (REMOTE(wid)->OldLCL0)

#define LOCAL_OldTR (Yap_local.OldTR)
#define REMOTE_OldTR(wid) (REMOTE(wid)->OldTR)

#define LOCAL_OldGlobalBase (Yap_local.OldGlobalBase)
#define REMOTE_OldGlobalBase(wid) (REMOTE(wid)->OldGlobalBase)

#define LOCAL_OldH (Yap_local.OldH)
#define REMOTE_OldH(wid) (REMOTE(wid)->OldH)

#define LOCAL_OldH0 (Yap_local.OldH0)
#define REMOTE_OldH0(wid) (REMOTE(wid)->OldH0)

#define LOCAL_OldTrailBase (Yap_local.OldTrailBase)
#define REMOTE_OldTrailBase(wid) (REMOTE(wid)->OldTrailBase)

#define LOCAL_OldTrailTop (Yap_local.OldTrailTop)
#define REMOTE_OldTrailTop(wid) (REMOTE(wid)->OldTrailTop)

#define LOCAL_OldHeapBase (Yap_local.OldHeapBase)
#define REMOTE_OldHeapBase(wid) (REMOTE(wid)->OldHeapBase)

#define LOCAL_OldHeapTop (Yap_local.OldHeapTop)
#define REMOTE_OldHeapTop(wid) (REMOTE(wid)->OldHeapTop)

#define LOCAL_ClDiff (Yap_local.ClDiff)
#define REMOTE_ClDiff(wid) (REMOTE(wid)->ClDiff)

#define LOCAL_GDiff (Yap_local.GDiff)
#define REMOTE_GDiff(wid) (REMOTE(wid)->GDiff)

#define LOCAL_HDiff (Yap_local.HDiff)
#define REMOTE_HDiff(wid) (REMOTE(wid)->HDiff)

#define LOCAL_GDiff0 (Yap_local.GDiff0)
#define REMOTE_GDiff0(wid) (REMOTE(wid)->GDiff0)

#define LOCAL_GSplit (Yap_local.GSplit)
#define REMOTE_GSplit(wid) (REMOTE(wid)->GSplit)

#define LOCAL_LDiff (Yap_local.LDiff)
#define REMOTE_LDiff(wid) (REMOTE(wid)->LDiff)

#define LOCAL_TrDiff (Yap_local.TrDiff)
#define REMOTE_TrDiff(wid) (REMOTE(wid)->TrDiff)

#define LOCAL_XDiff (Yap_local.XDiff)
#define REMOTE_XDiff(wid) (REMOTE(wid)->XDiff)

#define LOCAL_DelayDiff (Yap_local.DelayDiff)
#define REMOTE_DelayDiff(wid) (REMOTE(wid)->DelayDiff)

#define LOCAL_BaseDiff (Yap_local.BaseDiff)
#define REMOTE_BaseDiff(wid) (REMOTE(wid)->BaseDiff)

// Reduction counters
#define LOCAL_ReductionsCounter (Yap_local.ReductionsCounter)
#define REMOTE_ReductionsCounter(wid) (REMOTE(wid)->ReductionsCounter)

#define LOCAL_PredEntriesCounter (Yap_local.PredEntriesCounter)
#define REMOTE_PredEntriesCounter(wid) (REMOTE(wid)->PredEntriesCounter)

#define LOCAL_RetriesCounter (Yap_local.RetriesCounter)
#define REMOTE_RetriesCounter(wid) (REMOTE(wid)->RetriesCounter)

#define LOCAL_ReductionsCounterOn (Yap_local.ReductionsCounterOn)
#define REMOTE_ReductionsCounterOn(wid) (REMOTE(wid)->ReductionsCounterOn)

#define LOCAL_PredEntriesCounterOn (Yap_local.PredEntriesCounterOn)
#define REMOTE_PredEntriesCounterOn(wid) (REMOTE(wid)->PredEntriesCounterOn)

#define LOCAL_RetriesCounterOn (Yap_local.RetriesCounterOn)
#define REMOTE_RetriesCounterOn(wid) (REMOTE(wid)->RetriesCounterOn)

// support for consulting files
/* current consult stack */
#define LOCAL_ConsultSp (Yap_local.ConsultSp)
#define REMOTE_ConsultSp(wid) (REMOTE(wid)->ConsultSp)

/* current maximum number of cells in consult stack */
#define LOCAL_ConsultCapacity (Yap_local.ConsultCapacity)
#define REMOTE_ConsultCapacity(wid) (REMOTE(wid)->ConsultCapacity)

/* top of consult stack  */
#define LOCAL_ConsultBase (Yap_local.ConsultBase)
#define REMOTE_ConsultBase(wid) (REMOTE(wid)->ConsultBase)

/* low-water mark for consult  */
#define LOCAL_ConsultLow (Yap_local.ConsultLow)
#define REMOTE_ConsultLow(wid) (REMOTE(wid)->ConsultLow)

#define LOCAL_VarNames (Yap_local.VarNames)
#define REMOTE_VarNames(wid) (REMOTE(wid)->VarNames)

#define LOCAL_SourceFileName (Yap_local.SourceFileName)
#define REMOTE_SourceFileName(wid) (REMOTE(wid)->SourceFileName)

#define LOCAL_SourceFileLineno (Yap_local.SourceFileLineno)
#define REMOTE_SourceFileLineno(wid) (REMOTE(wid)->SourceFileLineno)

// global variables
#define LOCAL_GlobalArena (Yap_local.GlobalArena)
#define REMOTE_GlobalArena(wid) (REMOTE(wid)->GlobalArena)

#define LOCAL_GlobalArenaOverflows (Yap_local.GlobalArenaOverflows)
#define REMOTE_GlobalArenaOverflows(wid) (REMOTE(wid)->GlobalArenaOverflows)

#define LOCAL_ArenaOverflows (Yap_local.ArenaOverflows)
#define REMOTE_ArenaOverflows(wid) (REMOTE(wid)->ArenaOverflows)

#define LOCAL_DepthArenas (Yap_local.DepthArenas)
#define REMOTE_DepthArenas(wid) (REMOTE(wid)->DepthArenas)

#define LOCAL_LastAssertedPred (Yap_local.LastAssertedPred)
#define REMOTE_LastAssertedPred(wid) (REMOTE(wid)->LastAssertedPred)

#define LOCAL_TmpPred (Yap_local.TmpPred)
#define REMOTE_TmpPred(wid) (REMOTE(wid)->TmpPred)

#define LOCAL_ScannerStack (Yap_local.ScannerStack)
#define REMOTE_ScannerStack(wid) (REMOTE(wid)->ScannerStack)

#define LOCAL_ScannerExtraBlocks (Yap_local.ScannerExtraBlocks)
#define REMOTE_ScannerExtraBlocks(wid) (REMOTE(wid)->ScannerExtraBlocks)

/// worker control information
/// stack limit after which the stack is managed by C-code.
#define LOCAL_CBorder (Yap_local.CBorder)
#define REMOTE_CBorder(wid) (REMOTE(wid)->CBorder)

/// max number of signals (uint64_t)
#define LOCAL_MaxActiveSignals (Yap_local.MaxActiveSignals)
#define REMOTE_MaxActiveSignals(wid) (REMOTE(wid)->MaxActiveSignals)

/// actual life signals
#define LOCAL_Signals (Yap_local.Signals)
#define REMOTE_Signals(wid) (REMOTE(wid)->Signals)

/// indexing help data?
#define LOCAL_IPredArity (Yap_local.IPredArity)
#define REMOTE_IPredArity(wid) (REMOTE(wid)->IPredArity)

#define LOCAL_ProfEnd (Yap_local.ProfEnd)
#define REMOTE_ProfEnd(wid) (REMOTE(wid)->ProfEnd)

#define LOCAL_DoingUndefp (Yap_local.DoingUndefp)
#define REMOTE_DoingUndefp(wid) (REMOTE(wid)->DoingUndefp)

#define LOCAL_StartCharCount (Yap_local.StartCharCount)
#define REMOTE_StartCharCount(wid) (REMOTE(wid)->StartCharCount)

#define LOCAL_StartLineCount (Yap_local.StartLineCount)
#define REMOTE_StartLineCount(wid) (REMOTE(wid)->StartLineCount)

#define LOCAL_StartLinePos (Yap_local.StartLinePos)
#define REMOTE_StartLinePos(wid) (REMOTE(wid)->StartLinePos)

#define LOCAL_ScratchPad (Yap_local.ScratchPad)
#define REMOTE_ScratchPad(wid) (REMOTE(wid)->ScratchPad)

#ifdef COROUTINING
#define LOCAL_WokenGoals (Yap_local.WokenGoals)
#define REMOTE_WokenGoals(wid) (REMOTE(wid)->WokenGoals)

#define LOCAL_AttsMutableList (Yap_local.AttsMutableList)
#define REMOTE_AttsMutableList(wid) (REMOTE(wid)->AttsMutableList)

#endif
// gc_stuff
#define LOCAL_GcGeneration (Yap_local.GcGeneration)
#define REMOTE_GcGeneration(wid) (REMOTE(wid)->GcGeneration)

#define LOCAL_GcPhase (Yap_local.GcPhase)
#define REMOTE_GcPhase(wid) (REMOTE(wid)->GcPhase)

#define LOCAL_GcCurrentPhase (Yap_local.GcCurrentPhase)
#define REMOTE_GcCurrentPhase(wid) (REMOTE(wid)->GcCurrentPhase)

#define LOCAL_GcCalls (Yap_local.GcCalls)
#define REMOTE_GcCalls(wid) (REMOTE(wid)->GcCalls)

#define LOCAL_TotGcTime (Yap_local.TotGcTime)
#define REMOTE_TotGcTime(wid) (REMOTE(wid)->TotGcTime)

#define LOCAL_TotGcRecovered (Yap_local.TotGcRecovered)
#define REMOTE_TotGcRecovered(wid) (REMOTE(wid)->TotGcRecovered)

#define LOCAL_LastGcTime (Yap_local.LastGcTime)
#define REMOTE_LastGcTime(wid) (REMOTE(wid)->LastGcTime)

#define LOCAL_LastSSTime (Yap_local.LastSSTime)
#define REMOTE_LastSSTime(wid) (REMOTE(wid)->LastSSTime)

#define LOCAL_OpenArray (Yap_local.OpenArray)
#define REMOTE_OpenArray(wid) (REMOTE(wid)->OpenArray)

/* in a single gc */
#define LOCAL_total_marked (Yap_local.total_marked)
#define REMOTE_total_marked(wid) (REMOTE(wid)->total_marked)

#define LOCAL_total_oldies (Yap_local.total_oldies)
#define REMOTE_total_oldies(wid) (REMOTE(wid)->total_oldies)

#define LOCAL_current_B (Yap_local.current_B)
#define REMOTE_current_B(wid) (REMOTE(wid)->current_B)

#define LOCAL_prev_HB (Yap_local.prev_HB)
#define REMOTE_prev_HB(wid) (REMOTE(wid)->prev_HB)

#define LOCAL_HGEN (Yap_local.HGEN)
#define REMOTE_HGEN(wid) (REMOTE(wid)->HGEN)

#define LOCAL_iptop (Yap_local.iptop)
#define REMOTE_iptop(wid) (REMOTE(wid)->iptop)

#if defined(GC_NO_TAGS)
#define LOCAL_bp (Yap_local.bp)
#define REMOTE_bp(wid) (REMOTE(wid)->bp)

#endif
#define LOCAL_sTR (Yap_local.sTR)
#define REMOTE_sTR(wid) (REMOTE(wid)->sTR)

#define LOCAL_sTR0 (Yap_local.sTR0)
#define REMOTE_sTR0(wid) (REMOTE(wid)->sTR0)

#define LOCAL_new_TR (Yap_local.new_TR)
#define REMOTE_new_TR(wid) (REMOTE(wid)->new_TR)

#define LOCAL_cont_top0 (Yap_local.cont_top0)
#define REMOTE_cont_top0(wid) (REMOTE(wid)->cont_top0)

#define LOCAL_cont_top (Yap_local.cont_top)
#define REMOTE_cont_top(wid) (REMOTE(wid)->cont_top)

#define LOCAL_discard_trail_entries (Yap_local.discard_trail_entries)
#define REMOTE_discard_trail_entries(wid) (REMOTE(wid)->discard_trail_entries)

#define LOCAL_gc_ma_hash_table (Yap_local.gc_ma_hash_table)
#define REMOTE_gc_ma_hash_table(wid) (REMOTE(wid)->gc_ma_hash_table)

#define LOCAL_gc_ma_h_top (Yap_local.gc_ma_h_top)
#define REMOTE_gc_ma_h_top(wid) (REMOTE(wid)->gc_ma_h_top)

#define LOCAL_gc_ma_h_list (Yap_local.gc_ma_h_list)
#define REMOTE_gc_ma_h_list(wid) (REMOTE(wid)->gc_ma_h_list)

#define LOCAL_gc_timestamp (Yap_local.gc_timestamp)
#define REMOTE_gc_timestamp(wid) (REMOTE(wid)->gc_timestamp)

#define LOCAL_db_vec (Yap_local.db_vec)
#define REMOTE_db_vec(wid) (REMOTE(wid)->db_vec)

#define LOCAL_db_vec0 (Yap_local.db_vec0)
#define REMOTE_db_vec0(wid) (REMOTE(wid)->db_vec0)

#define LOCAL_db_root (Yap_local.db_root)
#define REMOTE_db_root(wid) (REMOTE(wid)->db_root)

#define LOCAL_db_nil (Yap_local.db_nil)
#define REMOTE_db_nil(wid) (REMOTE(wid)->db_nil)

#define LOCAL_gc_restore (Yap_local.gc_restore)
#define REMOTE_gc_restore(wid) (REMOTE(wid)->gc_restore)

#define LOCAL_extra_gc_cells (Yap_local.extra_gc_cells)
#define REMOTE_extra_gc_cells(wid) (REMOTE(wid)->extra_gc_cells)

#define LOCAL_extra_gc_cells_base (Yap_local.extra_gc_cells_base)
#define REMOTE_extra_gc_cells_base(wid) (REMOTE(wid)->extra_gc_cells_base)

#define LOCAL_extra_gc_cells_top (Yap_local.extra_gc_cells_top)
#define REMOTE_extra_gc_cells_top(wid) (REMOTE(wid)->extra_gc_cells_top)

#define LOCAL_extra_gc_cells_size (Yap_local.extra_gc_cells_size)
#define REMOTE_extra_gc_cells_size(wid) (REMOTE(wid)->extra_gc_cells_size)

#define LOCAL_DynamicArrays (Yap_local.DynamicArrays)
#define REMOTE_DynamicArrays(wid) (REMOTE(wid)->DynamicArrays)

#define LOCAL_StaticArrays (Yap_local.StaticArrays)
#define REMOTE_StaticArrays(wid) (REMOTE(wid)->StaticArrays)

#define LOCAL_GlobalVariables (Yap_local.GlobalVariables)
#define REMOTE_GlobalVariables(wid) (REMOTE(wid)->GlobalVariables)

#define LOCAL_AllowRestart (Yap_local.AllowRestart)
#define REMOTE_AllowRestart(wid) (REMOTE(wid)->AllowRestart)

// Thread Local Area for Fast Storage of Intermediate Compiled Code
#define LOCAL_CMemFirstBlock (Yap_local.CMemFirstBlock)
#define REMOTE_CMemFirstBlock(wid) (REMOTE(wid)->CMemFirstBlock)

#define LOCAL_CMemFirstBlockSz (Yap_local.CMemFirstBlockSz)
#define REMOTE_CMemFirstBlockSz(wid) (REMOTE(wid)->CMemFirstBlockSz)

// Variable used by the compiler to store number of permanent vars in a clause
#define LOCAL_nperm (Yap_local.nperm)
#define REMOTE_nperm(wid) (REMOTE(wid)->nperm)

#define LOCAL_jMP (Yap_local.jMP)
#define REMOTE_jMP(wid) (REMOTE(wid)->jMP)

// Thread Local Area for Labels
#define LOCAL_LabelFirstArray (Yap_local.LabelFirstArray)
#define REMOTE_LabelFirstArray(wid) (REMOTE(wid)->LabelFirstArray)

#define LOCAL_LabelFirstArraySz (Yap_local.LabelFirstArraySz)
#define REMOTE_LabelFirstArraySz(wid) (REMOTE(wid)->LabelFirstArraySz)

// Thread Local Area for SWI-Prolog emulation routines.
// struct LOCAL_INIT( PL_local_data*, PL_local_data_p,  Yap_InitThreadIO(wid))
#ifdef THREADS
#define LOCAL_ThreadHandle (Yap_local.ThreadHandle)
#define REMOTE_ThreadHandle(wid) (REMOTE(wid)->ThreadHandle)

#endif /* THREADS */
#if defined(YAPOR) || defined(TABLING)
#define LOCAL_optyap_data (Yap_local.optyap_data)
#define REMOTE_optyap_data(wid) (REMOTE(wid)->optyap_data)

#define LOCAL_TabMode (Yap_local.TabMode)
#define REMOTE_TabMode(wid) (REMOTE(wid)->TabMode)

#endif /* YAPOR || TABLING */
#define LOCAL_InterruptsDisabled (Yap_local.InterruptsDisabled)
#define REMOTE_InterruptsDisabled(wid) (REMOTE(wid)->InterruptsDisabled)

#define LOCAL_execution (Yap_local.execution)
#define REMOTE_execution(wid) (REMOTE(wid)->execution)

#if LOW_LEVEL_TRACER
#define LOCAL_total_choicepoints (Yap_local.total_choicepoints)
#define REMOTE_total_choicepoints(wid) (REMOTE(wid)->total_choicepoints)

#endif
#define LOCAL_consult_level (Yap_local.consult_level)
#define REMOTE_consult_level(wid) (REMOTE(wid)->consult_level)

// Variables related to memory allocation
#define LOCAL_LocalBase (Yap_local.LocalBase)
#define REMOTE_LocalBase(wid) (REMOTE(wid)->LocalBase)

#define LOCAL_GlobalBase (Yap_local.GlobalBase)
#define REMOTE_GlobalBase(wid) (REMOTE(wid)->GlobalBase)

#define LOCAL_TrailBase (Yap_local.TrailBase)
#define REMOTE_TrailBase(wid) (REMOTE(wid)->TrailBase)

#define LOCAL_TrailTop (Yap_local.TrailTop)
#define REMOTE_TrailTop(wid) (REMOTE(wid)->TrailTop)

/* error handling info, designed to be easy to pass to the foreign world */
#define LOCAL_ActiveError (Yap_local.ActiveError)
#define REMOTE_ActiveError(wid) (REMOTE(wid)->ActiveError)

#define LOCAL_CommittedError (Yap_local.CommittedError)
#define REMOTE_CommittedError(wid) (REMOTE(wid)->CommittedError)

#define LOCAL_delay (Yap_local.delay)
#define REMOTE_delay(wid) (REMOTE(wid)->delay)

/// pointer to an exception term, from throw
#define LOCAL_IOBotch (Yap_local.IOBotch)
#define REMOTE_IOBotch(wid) (REMOTE(wid)->IOBotch)

/// tokenizer support (should be private to the tokenizer).
#define LOCAL_tokptr (Yap_local.tokptr)
#define REMOTE_tokptr(wid) (REMOTE(wid)->tokptr)

#define LOCAL_toktide (Yap_local.toktide)
#define REMOTE_toktide(wid) (REMOTE(wid)->toktide)

#define LOCAL_VarTable (Yap_local.VarTable)
#define REMOTE_VarTable(wid) (REMOTE(wid)->VarTable)

#define LOCAL_AnonVarTable (Yap_local.AnonVarTable)
#define REMOTE_AnonVarTable(wid) (REMOTE(wid)->AnonVarTable)

#define LOCAL_Comments (Yap_local.Comments)
#define REMOTE_Comments(wid) (REMOTE(wid)->Comments)

#define LOCAL_CommentsTail (Yap_local.CommentsTail)
#define REMOTE_CommentsTail(wid) (REMOTE(wid)->CommentsTail)

#define LOCAL_CommentsNextChar (Yap_local.CommentsNextChar)
#define REMOTE_CommentsNextChar(wid) (REMOTE(wid)->CommentsNextChar)

#define LOCAL_CommentsBuff (Yap_local.CommentsBuff)
#define REMOTE_CommentsBuff(wid) (REMOTE(wid)->CommentsBuff)

#define LOCAL_CommentsBuffPos (Yap_local.CommentsBuffPos)
#define REMOTE_CommentsBuffPos(wid) (REMOTE(wid)->CommentsBuffPos)

#define LOCAL_CommentsBuffLim (Yap_local.CommentsBuffLim)
#define REMOTE_CommentsBuffLim(wid) (REMOTE(wid)->CommentsBuffLim)

#define LOCAL_RestartEnv (Yap_local.RestartEnv)
#define REMOTE_RestartEnv(wid) (REMOTE(wid)->RestartEnv)

#define LOCAL_FileNameBuf (Yap_local.FileNameBuf)
#define REMOTE_FileNameBuf(wid) (REMOTE(wid)->FileNameBuf)

#define LOCAL_FileNameBuf2 (Yap_local.FileNameBuf2)
#define REMOTE_FileNameBuf2(wid) (REMOTE(wid)->FileNameBuf2)

#define LOCAL_TextBuffer (Yap_local.TextBuffer)
#define REMOTE_TextBuffer(wid) (REMOTE(wid)->TextBuffer)

// Prolog State
#define LOCAL_BreakLevel (Yap_local.BreakLevel)
#define REMOTE_BreakLevel(wid) (REMOTE(wid)->BreakLevel)

#define LOCAL_PrologMode (Yap_local.PrologMode)
#define REMOTE_PrologMode(wid) (REMOTE(wid)->PrologMode)

#define LOCAL_CritLocks (Yap_local.CritLocks)
#define REMOTE_CritLocks(wid) (REMOTE(wid)->CritLocks)

// Prolog execution and state flags
#define LOCAL_Flags (Yap_local.Flags)
#define REMOTE_Flags(wid) (REMOTE(wid)->Flags)

#define LOCAL_flagCount (Yap_local.flagCount)
#define REMOTE_flagCount(wid) (REMOTE(wid)->flagCount)

// analyst.c
/* used to find out how many instructions of each kind are executed */
#ifdef ANALYST
#define LOCAL_opcount (Yap_local.opcount)
#define REMOTE_opcount(wid) (REMOTE(wid)->opcount)

#define LOCAL_2opcount (Yap_local.2opcount)
#define REMOTE_2opcount(wid) (REMOTE(wid)->2opcount)

#endif /* ANALYST */
// dbase.c
#define LOCAL_s_dbg (Yap_local.s_dbg)
#define REMOTE_s_dbg(wid) (REMOTE(wid)->s_dbg)

// eval.c
#define LOCAL_mathtt (Yap_local.mathtt)
#define REMOTE_mathtt(wid) (REMOTE(wid)->mathtt)

#define LOCAL_mathstring (Yap_local.mathstring)
#define REMOTE_mathstring(wid) (REMOTE(wid)->mathstring)

#define LOCAL_ctx (Yap_local.ctx)
#define REMOTE_ctx(wid) (REMOTE(wid)->ctx)

// grow.c
#define LOCAL_heap_overflows (Yap_local.heap_overflows)
#define REMOTE_heap_overflows(wid) (REMOTE(wid)->heap_overflows)

#define LOCAL_total_heap_overflow_time (Yap_local.total_heap_overflow_time)
#define REMOTE_total_heap_overflow_time(wid) (REMOTE(wid)->total_heap_overflow_time)

#define LOCAL_stack_overflows (Yap_local.stack_overflows)
#define REMOTE_stack_overflows(wid) (REMOTE(wid)->stack_overflows)

#define LOCAL_total_stack_overflow_time (Yap_local.total_stack_overflow_time)
#define REMOTE_total_stack_overflow_time(wid) (REMOTE(wid)->total_stack_overflow_time)

#define LOCAL_delay_overflows (Yap_local.delay_overflows)
#define REMOTE_delay_overflows(wid) (REMOTE(wid)->delay_overflows)

#define LOCAL_total_delay_overflow_time (Yap_local.total_delay_overflow_time)
#define REMOTE_total_delay_overflow_time(wid) (REMOTE(wid)->total_delay_overflow_time)

#define LOCAL_trail_overflows (Yap_local.trail_overflows)
#define REMOTE_trail_overflows(wid) (REMOTE(wid)->trail_overflows)

#define LOCAL_total_trail_overflow_time (Yap_local.total_trail_overflow_time)
#define REMOTE_total_trail_overflow_time(wid) (REMOTE(wid)->total_trail_overflow_time)

#define LOCAL_atom_table_overflows (Yap_local.atom_table_overflows)
#define REMOTE_atom_table_overflows(wid) (REMOTE(wid)->atom_table_overflows)

#define LOCAL_total_atom_table_overflow_time (Yap_local.total_atom_table_overflow_time)
#define REMOTE_total_atom_table_overflow_time(wid) (REMOTE(wid)->total_atom_table_overflow_time)

// load_dyld
#ifdef LOAD_DYLD
#define LOCAL_dl_errno (Yap_local.dl_errno)
#define REMOTE_dl_errno(wid) (REMOTE(wid)->dl_errno)

#endif
// tracer.c
#ifdef LOW_LEVEL_TRACER
#define LOCAL_do_trace_primitives (Yap_local.do_trace_primitives)
#define REMOTE_do_trace_primitives(wid) (REMOTE(wid)->do_trace_primitives)

#endif
// quick loader
#define LOCAL_ExportAtomHashChain (Yap_local.ExportAtomHashChain)
#define REMOTE_ExportAtomHashChain(wid) (REMOTE(wid)->ExportAtomHashChain)

#define LOCAL_ExportAtomHashTableSize (Yap_local.ExportAtomHashTableSize)
#define REMOTE_ExportAtomHashTableSize(wid) (REMOTE(wid)->ExportAtomHashTableSize)

#define LOCAL_ExportAtomHashTableNum (Yap_local.ExportAtomHashTableNum)
#define REMOTE_ExportAtomHashTableNum(wid) (REMOTE(wid)->ExportAtomHashTableNum)

#define LOCAL_ExportFunctorHashChain (Yap_local.ExportFunctorHashChain)
#define REMOTE_ExportFunctorHashChain(wid) (REMOTE(wid)->ExportFunctorHashChain)

#define LOCAL_ExportFunctorHashTableSize (Yap_local.ExportFunctorHashTableSize)
#define REMOTE_ExportFunctorHashTableSize(wid) (REMOTE(wid)->ExportFunctorHashTableSize)

#define LOCAL_ExportFunctorHashTableNum (Yap_local.ExportFunctorHashTableNum)
#define REMOTE_ExportFunctorHashTableNum(wid) (REMOTE(wid)->ExportFunctorHashTableNum)

#define LOCAL_ExportPredEntryHashChain (Yap_local.ExportPredEntryHashChain)
#define REMOTE_ExportPredEntryHashChain(wid) (REMOTE(wid)->ExportPredEntryHashChain)

#define LOCAL_ExportPredEntryHashTableSize (Yap_local.ExportPredEntryHashTableSize)
#define REMOTE_ExportPredEntryHashTableSize(wid) (REMOTE(wid)->ExportPredEntryHashTableSize)

#define LOCAL_ExportPredEntryHashTableNum (Yap_local.ExportPredEntryHashTableNum)
#define REMOTE_ExportPredEntryHashTableNum(wid) (REMOTE(wid)->ExportPredEntryHashTableNum)

#define LOCAL_ExportDBRefHashChain (Yap_local.ExportDBRefHashChain)
#define REMOTE_ExportDBRefHashChain(wid) (REMOTE(wid)->ExportDBRefHashChain)

#define LOCAL_ExportDBRefHashTableSize (Yap_local.ExportDBRefHashTableSize)
#define REMOTE_ExportDBRefHashTableSize(wid) (REMOTE(wid)->ExportDBRefHashTableSize)

#define LOCAL_ExportDBRefHashTableNum (Yap_local.ExportDBRefHashTableNum)
#define REMOTE_ExportDBRefHashTableNum(wid) (REMOTE(wid)->ExportDBRefHashTableNum)

#define LOCAL_ImportAtomHashChain (Yap_local.ImportAtomHashChain)
#define REMOTE_ImportAtomHashChain(wid) (REMOTE(wid)->ImportAtomHashChain)

#define LOCAL_ImportAtomHashTableSize (Yap_local.ImportAtomHashTableSize)
#define REMOTE_ImportAtomHashTableSize(wid) (REMOTE(wid)->ImportAtomHashTableSize)

#define LOCAL_ImportAtomHashTableNum (Yap_local.ImportAtomHashTableNum)
#define REMOTE_ImportAtomHashTableNum(wid) (REMOTE(wid)->ImportAtomHashTableNum)

#define LOCAL_ImportFunctorHashChain (Yap_local.ImportFunctorHashChain)
#define REMOTE_ImportFunctorHashChain(wid) (REMOTE(wid)->ImportFunctorHashChain)

#define LOCAL_ImportFunctorHashTableSize (Yap_local.ImportFunctorHashTableSize)
#define REMOTE_ImportFunctorHashTableSize(wid) (REMOTE(wid)->ImportFunctorHashTableSize)

#define LOCAL_ImportFunctorHashTableNum (Yap_local.ImportFunctorHashTableNum)
#define REMOTE_ImportFunctorHashTableNum(wid) (REMOTE(wid)->ImportFunctorHashTableNum)

#define LOCAL_ImportOPCODEHashChain (Yap_local.ImportOPCODEHashChain)
#define REMOTE_ImportOPCODEHashChain(wid) (REMOTE(wid)->ImportOPCODEHashChain)

#define LOCAL_ImportOPCODEHashTableSize (Yap_local.ImportOPCODEHashTableSize)
#define REMOTE_ImportOPCODEHashTableSize(wid) (REMOTE(wid)->ImportOPCODEHashTableSize)

#define LOCAL_ImportPredEntryHashChain (Yap_local.ImportPredEntryHashChain)
#define REMOTE_ImportPredEntryHashChain(wid) (REMOTE(wid)->ImportPredEntryHashChain)

#define LOCAL_ImportPredEntryHashTableSize (Yap_local.ImportPredEntryHashTableSize)
#define REMOTE_ImportPredEntryHashTableSize(wid) (REMOTE(wid)->ImportPredEntryHashTableSize)

#define LOCAL_ImportPredEntryHashTableNum (Yap_local.ImportPredEntryHashTableNum)
#define REMOTE_ImportPredEntryHashTableNum(wid) (REMOTE(wid)->ImportPredEntryHashTableNum)

#define LOCAL_ImportDBRefHashChain (Yap_local.ImportDBRefHashChain)
#define REMOTE_ImportDBRefHashChain(wid) (REMOTE(wid)->ImportDBRefHashChain)

#define LOCAL_ImportDBRefHashTableSize (Yap_local.ImportDBRefHashTableSize)
#define REMOTE_ImportDBRefHashTableSize(wid) (REMOTE(wid)->ImportDBRefHashTableSize)

#define LOCAL_ImportDBRefHashTableNum (Yap_local.ImportDBRefHashTableNum)
#define REMOTE_ImportDBRefHashTableNum(wid) (REMOTE(wid)->ImportDBRefHashTableNum)

#define LOCAL_ImportFAILCODE (Yap_local.ImportFAILCODE)
#define REMOTE_ImportFAILCODE(wid) (REMOTE(wid)->ImportFAILCODE)

// exo indexing
#define LOCAL_ibnds (Yap_local.ibnds)
#define REMOTE_ibnds(wid) (REMOTE(wid)->ibnds)

#define LOCAL_exo_it (Yap_local.exo_it)
#define REMOTE_exo_it(wid) (REMOTE(wid)->exo_it)

#define LOCAL_exo_base (Yap_local.exo_base)
#define REMOTE_exo_base(wid) (REMOTE(wid)->exo_base)

#define LOCAL_exo_arity (Yap_local.exo_arity)
#define REMOTE_exo_arity(wid) (REMOTE(wid)->exo_arity)

#define LOCAL_exo_arg (Yap_local.exo_arg)
#define REMOTE_exo_arg(wid) (REMOTE(wid)->exo_arg)

// atom completion
#define LOCAL_search_atoms (Yap_local.search_atoms)
#define REMOTE_search_atoms(wid) (REMOTE(wid)->search_atoms)

#define LOCAL_SearchPreds (Yap_local.SearchPreds)
#define REMOTE_SearchPreds(wid) (REMOTE(wid)->SearchPreds)

/// Slots Status
#define LOCAL_CurSlot (Yap_local.CurSlot)
#define REMOTE_CurSlot(wid) (REMOTE(wid)->CurSlot)

#define LOCAL_FrozenHandles (Yap_local.FrozenHandles)
#define REMOTE_FrozenHandles(wid) (REMOTE(wid)->FrozenHandles)

#define LOCAL_NSlots (Yap_local.NSlots)
#define REMOTE_NSlots(wid) (REMOTE(wid)->NSlots)

#define LOCAL_SlotBase (Yap_local.SlotBase)
#define REMOTE_SlotBase(wid) (REMOTE(wid)->SlotBase)

// Mutexes
#define LOCAL_Mutexes (Yap_local.Mutexes)
#define REMOTE_Mutexes(wid) (REMOTE(wid)->Mutexes)

#define LOCAL_SourceModule (Yap_local.SourceModule)
#define REMOTE_SourceModule(wid) (REMOTE(wid)->SourceModule)

#define LOCAL_Including (Yap_local.Including)
#define REMOTE_Including(wid) (REMOTE(wid)->Including)

#define LOCAL_MAX_SIZE (Yap_local.MAX_SIZE)
#define REMOTE_MAX_SIZE(wid) (REMOTE(wid)->MAX_SIZE)

/* last call to walltime. */
#define LOCAL_LastWTime (Yap_local.LastWTime)
#define REMOTE_LastWTime(wid) (REMOTE(wid)->LastWTime)

#define LOCAL_shared (Yap_local.shared)
#define REMOTE_shared(wid) (REMOTE(wid)->shared)

