/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Yap.proto						 *
* mods:									 *
* comments:	Function declarations for YAP				 *
* version:      $Id: Yapproto.h,v 1.90 2008-08-07 20:51:23 vsc Exp $	 *
*************************************************************************/

/* prototype file for Yap */
/// @file Yapproto.h
///
/// @brief Prototype Declarations
///
/// @{

#ifndef YAP_PROTOS_H
#define YAP_PROTOS_H 1

#include "YapDefs.h"

/* absmi.c */
extern Int Yap_absmi(int);
extern  int Yap_absmiEND(void);

/* adtdefs.c */
extern Term Yap_ArrayToList(Term *, size_t);
extern int Yap_GetName(char *, UInt, Term);
extern Term Yap_GetValue(Atom);
extern int Yap_HasOp(Atom);
extern struct operator_entry *
     Yap_GetOpPropForAModuleHavingALock(struct AtomEntryStruct *, Term);
extern Atom Yap_LookupAtom(const char *);
extern Atom Yap_AtomInUse(const char *atom);
extern Atom Yap_ULookupAtom(const unsigned char *);
extern Atom Yap_LookupAtomWithLength(const char *, size_t);
extern Atom Yap_FullLookupAtom(const char *);
extern void Yap_LookupAtomWithAddress(const char *, struct AtomEntryStruct *);
extern Prop Yap_NewPredPropByFunctor(struct FunctorEntryStruct *, Term);
extern Prop Yap_NewPredPropByAtom(struct AtomEntryStruct *, Term);
extern Prop Yap_PredPropByFunctorNonThreadLocal(struct FunctorEntryStruct *, Term);
extern Prop Yap_PredPropByAtomNonThreadLocal(struct AtomEntryStruct *, Term);
extern Functor Yap_UnlockedMkFunctor(struct AtomEntryStruct *, arity_t);
extern Functor Yap_MkFunctor(Atom, arity_t);
extern void Yap_MkFunctorWithAddress(Atom, unsigned int, FunctorEntry *);
extern void Yap_PutValue(Atom, Term);
extern void Yap_ReleaseAtom(Atom);
extern int Yap_AtomIncreaseHold(Atom);
extern int Yap_AtomDecreaseHold(Atom);
extern struct operator_entry *Yap_OpPropForModule(Atom, Term);

#ifdef SFUNC
extern Term MkSFTerm(Functor, int, Term *, Term);
extern CELL *ArgsOfSFTerm(Term);
#endif

extern Prop Yap_GetPredPropByAtom(Atom, Term);
extern Prop Yap_GetPredPropByFunc(Functor, Term);
extern Prop Yap_GetPredPropByAtomInThisModule(Atom, Term);
extern Prop Yap_GetPredPropByFuncInThisModule(Functor, Term);
extern Prop Yap_GetPredPropHavingLock(Atom, unsigned int, Term);
extern Prop Yap_GetExpProp(Atom, unsigned int);
extern Prop Yap_GetExpPropHavingLock(struct AtomEntryStruct *, unsigned int);

/* agc.c */
extern void Yap_atom_gc(CACHE_TYPE1);
extern void Yap_init_agc(void);

/* alloc.c */
extern void Yap_FreeCodeSpace(void *);
extern void *Yap_AllocAtomSpace(size_t);
extern void *Yap_AllocCodeSpace(size_t);
extern void *Yap_ReallocCodeSpace(void *, size_t);
extern ADDR Yap_AllocFromForeignArea(size_t);
extern int Yap_ExtendWorkSpace(Int);
extern void Yap_FreeAtomSpace(void *);
extern int Yap_FreeWorkSpace(void);
extern void Yap_InitMemory(size_t, size_t, size_t);
extern void Yap_InitExStacks(int, size_t, size_t);

/* amasm.c */
extern OPCODE Yap_opcode(op_numbers);

/* analyst.c */
#ifdef ANALYST
extern void Yap_InitAnalystPreds(void);
#endif /* ANALYST */

/* arrays.c */
extern void Yap_InitArrayPreds(void);

/* atoms.c */
extern void Yap_InitBackAtoms(void);
extern void Yap_InitAtomPreds(void);

/* attvar.c */
extern void Yap_InitAttVarPreds(void);
extern void Yap_MkEmptyWakeUp(Atom mod);

/* bb.c */
extern void Yap_InitBBPreds(void);

/* bignum.c */
extern Term Yap_MkULLIntTerm(YAP_ULONG_LONG);
extern int Yap_IsStringTerm(Term);
extern int Yap_IsWideStringTerm(Term);
extern Term Yap_RatTermToApplTerm(Term);
extern void Yap_InitBigNums(void);
extern Term Yap_AllocExternalDataInStack(CELL, size_t, void *);
extern int Yap_CleanOpaqueVariable(Term t);
extern CELL *Yap_HeapStoreOpaqueTerm(Term t);
extern size_t Yap_OpaqueTermToString(Term t, char *str, size_t max);
extern Int Yap_blob_tag(Term t);

/* c_interface.c */
#ifndef YAP_CPP_INTERFACE
extern X_API Int YAP_Execute(struct pred_entry *, CPredicate);
extern X_API Int YAP_ExecuteFirst(struct pred_entry *, CPredicate);
extern X_API Int YAP_ExecuteNext(struct pred_entry *, CPredicate);
extern X_API Int YAP_ExecuteOnCut(struct pred_entry *, CPredicate, struct cut_c_str *);
extern X_API Int YAP_RunGoalOnce(Term);
#endif

/* cdmgr.c */
extern Term Yap_all_calls(void);
extern Atom Yap_ConsultingFile(USES_REGS1);
extern bool Yap_Consulting(USES_REGS1);
extern struct pred_entry *Yap_PredForChoicePt(choiceptr bptr, op_numbers *op);
extern void Yap_InitCdMgr(void);
extern struct pred_entry *Yap_PredFromClause(Term t USES_REGS);
extern bool Yap_discontiguous(struct pred_entry *ap, Term mode USES_REGS);
extern bool Yap_multiple(struct pred_entry *ap, Term mode USES_REGS);
extern void Yap_init_consult(int, const char *);
extern void Yap_end_consult(void);
extern void Yap_Abolish(struct pred_entry *);
extern void Yap_BuildMegaClause(struct pred_entry *);
extern void Yap_EraseMegaClause(yamop *, struct pred_entry *);
extern void Yap_ResetConsultStack(void);
extern void Yap_AssertzClause(struct pred_entry *, yamop *);
extern void Yap_HidePred(struct pred_entry *pe);
extern int Yap_SetNoTrace(char *name, UInt arity, Term tmod);
extern bool Yap_unknown(Term tflagvalue);
extern struct pred_entry *Yap_MkLogPred(struct pred_entry *pe);

/* cmppreds.c */
extern Int Yap_compare_terms(Term, Term);
extern Int Yap_acmp(Term, Term USES_REGS);
extern void Yap_InitCmpPreds(void);

/* compiler.c */
extern yamop *Yap_cclause(Term, Int, Term, Term);

/* computils.c */
extern int Yap_DebugPutc(FILE *, wchar_t);
extern int Yap_DebugPuts(FILE *, const char *);
extern void Yap_DebugSetIFile(char *);
extern void Yap_DebugEndline(void);
extern void Yap_DebugPlWrite(Term t);
extern void Yap_DebugPlWriteln(Term t);

/* corout.c */
extern void Yap_InitCoroutPreds(void);
#ifdef COROUTINING
extern Term Yap_ListOfWokenGoals(void);
extern void Yap_WakeUp(CELL *);
extern bool Yap_WakeUpUnsafe(CELL *);
#endif

/* dbase.c */
extern struct pred_entry *Yap_FindLUIntKey(Int);
extern int Yap_DBTrailOverflow(void);
extern CELL Yap_EvalMasks(Term, CELL *);
extern void Yap_InitBackDB(void);
extern void Yap_InitDBPreds(void);
extern void Yap_InitDBLoadPreds(void);

/* errors.c */
#if DEBUG
extern const char *Yap_PrintPredName(struct pred_entry *ap);
#endif
extern void Yap_RestartYap(int);
extern void Yap_exit(int)
#ifndef MSC_VER
__attribute__((noreturn))
#endif
;
extern bool Yap_Warning(const char *s, ...);
extern bool Yap_PrintWarning(Term t);
extern bool Yap_HandleError__(const char *file, const char *function, int lineno,
                       const char *s, ...);
#define Yap_HandleError(...)                                                   \
  Yap_HandleError__(__FILE__, __FUNCTION__, __LINE__, __VA_ARGS__)
extern int Yap_SWIHandleError(const char *, ...);
extern void Yap_InitErrorPreds(void);

/* eval.c */
extern void Yap_InitEval(void);

/* exec.c */
extern void Yap_fail_all(choiceptr bb USES_REGS);
extern Term Yap_ExecuteCallMetaCall(Term,Term);
extern void Yap_InitExecFs(void);
extern bool Yap_JumpToEnv(void);
extern Term Yap_RunTopGoal(Term, bool);
extern bool Yap_execute_goal(Term, int, Term, bool);
extern bool Yap_exec_absmi(bool, yap_reset_t);
extern void Yap_trust_last(void);
extern void  Yap_closeGoal(bool out, yamop *saved_p, yamop * saved_cp, Int saved_e, Int saved_b, yhandle_t hdl, bool pass_ex);
extern void Yap_PrepGoal(UInt, CELL *, choiceptr USES_REGS);
extern bool Yap_execute_pred(struct pred_entry *ppe, CELL *pt,
                      bool pass_exception USES_REGS);
extern int Yap_dogc(arity_t extra_args, Term *tp USES_REGS);
extern Term Yap_PredicateToIndicator(struct pred_entry *pe);
extern Term Yap_TermToIndicator(Term t, Term mod);
extern bool Yap_Execute(Term t USES_REGS);

/* exo.c */
extern void Yap_InitExoPreds(void);
extern void Yap_udi_Interval_init(void);
extern bool Yap_Reset(yap_reset_t mode, bool hard);

/* flagss.c */
X_API extern bool Yap_create_prolog_flag(const char *name, bool writable,  Term ttype, Term v);

/* foreign.c */
extern char *Yap_FindExecutable(void);

/* gprof.c */
extern void Yap_InitLowProf(void);
#if LOW_PROF
extern void Yap_inform_profiler_of_clause__(void *, void *, struct pred_entry *,
                                     gprof_info);
#define Yap_inform_profiler_of_clause(CODE0, CODEF, AP, MODE)                  \
  {                                                                            \
    if (GLOBAL_FPreds)                                                         \
      Yap_inform_profiler_of_clause__(CODE0, CODEF, AP, MODE);                 \
  }
#else
#define Yap_inform_profiler_of_clause(CODE0, CODEF, AP, MODE)
#endif
extern void Yap_tell_gprof(yamop *);

/* globals.c */
extern Term Yap_NewArena(size_t, CELL *);
extern CELL *Yap_GetFromArena(Term *, size_t, UInt);
extern void Yap_InitGlobals(void);
extern Term Yap_SaveTerm(Term);
extern Term Yap_SetGlobalVal(Atom, Term);
extern Term Yap_GetGlobal(Atom);
extern  Int Yap_DeleteGlobal(Atom);
extern void Yap_AllocateDefaultArena(size_t gsize, int wid, void *cs);
extern CELL *Yap_ArenaLimit(Term arena);

/* grow.c */
extern Int Yap_total_stack_shift_time(void);
extern void Yap_InitGrowPreds(void);
extern size_t Yap_InsertInGlobal(CELL *, size_t);
extern int Yap_growheap(bool, size_t, void *);
extern int Yap_growstack(size_t);
extern int Yap_growtrail(size_t, bool);
extern int Yap_growglobal(CELL **);
extern int Yap_locked_growheap(bool, size_t, void *);
extern int Yap_locked_growstack(size_t);
extern int Yap_locked_growtrail(size_t, bool);
extern int Yap_locked_growglobal(CELL **);
extern CELL **Yap_shift_visit(CELL **, CELL ***, CELL ***);
#ifdef THREADS
extern void Yap_CopyThreadStacks(int, int, int);
#endif

/* heapgc.c */
extern Int Yap_total_gc_time(void);
extern void Yap_init_gc(void);
extern bool Yap_is_gc_verbose(void);
extern int Yap_gc(void *);
extern int Yap_locked_gc(Int, CELL *, yamop *);
extern int Yap_gcl(UInt, Int, CELL *, yamop *);
extern int Yap_locked_gcl(UInt, Int, CELL *, yamop *);
extern bool Yap_expand(size_t sz USES_REGS);

/* init.c */
extern int Yap_IsOpType(char *);
extern void Yap_InitWorkspace(struct yap_boot_params *, UInt, UInt, UInt, UInt, UInt, int, int, int);
extern bool Yap_AddCallToFli(struct pred_entry *pe, CPredicate call);
extern bool Yap_AddRetryToFli(struct pred_entry *pe, CPredicate re);
extern bool Yap_AddCutToFli(struct pred_entry *pe, CPredicate cut);
extern const char *Yap_version(void);

#ifdef YAPOR
extern void Yap_init_yapor_workers(void);
#endif /* YAPOR */
#if defined(YAPOR) || defined(THREADS)
extern void Yap_KillStacks(int);
#else
extern void Yap_KillStacks(int);
#endif
extern void Yap_InitYaamRegs(int, bool full_reset);
extern void Yap_ReInitWTime(void);
extern int Yap_OpDec(int, char *, Atom, Term);
extern void Yap_CloseScratchPad(void);

/* inlines.c */
extern void Yap_InitInlines(void);
extern int Yap_eq(Term, Term);

/* iopreds.c */
extern bool Yap_IsAbsolutePath(const char *p, bool);
extern Atom Yap_TemporaryFile(const char *prefix, int *fd);
extern void Yap_InitPlIO( struct yap_boot_params *ts );
extern void Yap_InitBackIO(void);
extern void Yap_InitIOPreds(void);
extern void Yap_DebugPlWrite(Term t);
extern void Yap_DebugPlWriteln(Term t);
extern void Yap_DebugErrorPutc(int n);
extern void Yap_DebugErrorPuts(const char *s);
extern void Yap_DebugWriteIndicator(struct pred_entry *ap);
extern void Yap_CloseReadline(void);
/* depth_lim.c */
extern bool Yap_InitReadline(Term t);
extern void Yap_InitItDeepenPreds(void);
extern struct AliasDescS *Yap_InitStandardAliases(void);

/* load_foreign.c */
extern void Yap_InitLoadForeign(void);
extern bool Yap_LateInit(const char s[]);

/* mavar.c */
extern void Yap_InitMaVarCPreds(void);
extern Term Yap_NewTimedVar(Term);
extern Term Yap_NewEmptyTimedVar(void);
extern Term Yap_ReadTimedVar(Term);
extern Term Yap_UpdateTimedVar(Term, Term);

/* modules.c */
extern Term Yap_Module(Term);
extern Term Yap_Module_Name(struct pred_entry *);
extern struct pred_entry *Yap_ModulePred(Term);
extern void Yap_NewModulePred(Term, struct pred_entry *);
extern Term Yap_StripModule(Term, Term *);
extern Term Yap_YapStripModule(Term, Term *);
extern void Yap_InitModules(void);
extern void Yap_InitModulesC(void);
extern struct mod_entry *Yap_GetModuleEntry(Term tmod);
extern Term Yap_GetModuleFromEntry(struct mod_entry *me);
extern bool Yap_CharacterEscapes(Term mt);
extern bool Yap_constPred(struct pred_entry *pt);
extern bool Yap_isSystemModule(Term mod);

#if HAVE_MPI
/* mpi.c */
extern void Yap_InitMPI(void);
#endif

#if HAVE_MPE
/* mpe.c */
extern void Yap_InitMPE(void);
#endif

/* other.c */
extern Term Yap_MkApplTerm(Functor, arity_t, const Term *);
extern Term Yap_MkNewApplTerm(Functor, arity_t);
extern Term Yap_MkNewPairTerm(void);
extern Term Yap_Globalise(Term);

/* readutil.c */
extern void Yap_InitReadUtil(void);

/* qly.c */
extern void Yap_InitQLY(void);
extern YAP_file_type_t Yap_Restore(const char *);
extern void Yap_InitQLYR(void);

/* range.c */
extern void Yap_InitRange(void);

/* save.c */
extern int Yap_SavedInfo(const char *, CELL *, CELL *, CELL *);
extern int Yap_SavedStateRestore(char *);
extern FILE *Yap_OpenRestore(const char *);
extern void Yap_InitSavePreds(void);

/* scanner.c */

/* signals.c */
extern void Yap_InitSignalCPreds(void);
extern void *Yap_InitSignals(int wid);
extern bool Yap_DisableInterrupts(int wid);
extern bool Yap_EnableInterrupts(int wid);
extern bool Yap_InitSIGSEGV(Term inp);

extern void Yap_InitSockets(void);

/* sort.c */
extern void Yap_InitSortPreds(void);

/* stack.c */
extern void Yap_InitStInfo(void);
extern char *Yap_output_bug_location(yamop *yap_pc, int where_from, int psize);

#if !defined(YAPOR) && !defined(THREADS)
extern bool Yap_search_for_static_predicate_in_use(struct pred_entry *, bool);
#endif

/* stdpreds.c */
extern void Yap_InitBackCPreds(void);
extern void Yap_InitCPreds(void);
extern void Yap_show_statistics(void);
extern int Yap_IsOpMaxPrio(Atom);

extern bool Yap_SetInputStream( Term sd );
extern bool Yap_SetOutputStream( Term sd );
extern bool Yap_SetErrorStream( Term sd );

/* sysbits.c */
extern size_t Yap_InitPageSize(void);
extern bool Yap_set_fpu_exceptions(Term);
extern UInt Yap_cputime(void);
extern uint64_t Yap_walltime(void);
extern int Yap_dir_separator(int);
extern int Yap_volume_header(char *);
extern int Yap_signal_index(const char *);
#ifdef MAC
extern void Yap_SetTextFile(char *);
#endif
extern const char *Yap_AbsoluteFile(const char *spec, bool expand);
#if __ANDROID__
#include <android/asset_manager.h>

extern void *Yap_openAssetFile(const char *path);
extern bool Yap_isAsset(const char *path);
#endif
extern const char *Yap_getcwd( char *, size_t);
extern void Yap_cputime_interval(Int *, Int *);
extern void Yap_systime_interval(Int *, Int *);
extern void Yap_InitSysbits(int wid);
extern void Yap_InitSysPreds(void);
extern void Yap_InitcTime(int);
extern void Yap_InitTime(int);
extern double Yap_random(void);
#ifdef _WIN32
extern char *Yap_RegistryGetString(char *);
extern void Yap_WinError(char *);
#endif

extern const char *Yap_AbsoluteFileInBuffer(const char *spec, char *outp, size_t sz,
                                     bool ok);
extern bool Yap_ChDir(const char *path);
bool Yap_isDirectory(const char *FileName);
extern bool Yap_Exists(const char *f);

/* terms.c */
extern Term Yap_BreakCycles(Term t, Term tail USES_REGS);
extern bool Yap_IsCyclicTerm(Term inp USES_REGS);
extern Term Yap_HackCycles(Term t  USES_REGS);
extern Term Yap_NonSingletons(Term inp, Term tail  USES_REGS);
extern void  Yap_InitTermCPreds(void);

/* threads.c */
extern void Yap_InitThreadPreds(void);
extern void Yap_InitFirstWorkerThreadHandle(void);
extern int Yap_ThreadID(void);
extern int Yap_NOfThreads(void);
#if THREADS
extern int Yap_InitThread(int);
#endif
extern intptr_t system_thread_id(void);
/* tracer.c */
#ifdef LOW_LEVEL_TRACER
extern void Yap_InitLowLevelTrace(void);
#endif

extern void *Yap_InitTextAllocator( void );

/* udi.c */
extern void Yap_udi_init(void);
extern void Yap_udi_abolish(struct pred_entry *);

/* unify.c */
extern int Yap_rational_tree_loop(CELL *, CELL *, CELL **, CELL **);
extern void Yap_InitAbsmi(void);
extern void Yap_InitUnify(void);
extern void Yap_TrimTrail(void);
extern int Yap_Unifiable(Term d0, Term d1);
extern int Yap_IUnify( CELL d0,  CELL d1);

/* userpreds.c */
extern void Yap_InitUserCPreds(void);
extern void Yap_InitUserBacks(void);

/* utilpreds.c */
int Yap_copy_complex_term(CELL *pt0, CELL *pt0_end,
			  bool share, bool copy_att_vars, CELL *ptf,
			  CELL *HLow USES_REGS);
extern Term Yap_CopyTerm(Term);
extern bool Yap_Variant(Term, Term);
extern size_t Yap_ExportTerm(Term, char *, size_t, UInt);
extern size_t Yap_SizeOfExportedTerm(char *);
extern Term Yap_ImportTerm(char *);
extern bool Yap_IsListTerm(Term);
extern bool Yap_IsListOrPartialListTerm(Term);
extern Term Yap_CopyTermNoShare(Term);
extern int Yap_SizeGroundTerm(Term, int);
extern bool Yap_IsGroundTerm(Term);
extern bool Yap_IsAcyclicTerm(Term);
extern void Yap_InitUtilCPreds(void);
extern Int Yap_TermHash(Term, Int, Int, int);
extern Int Yap_NumberVars(Term, Int, bool, Int * USES_REGS);
extern Term Yap_TermVariables(Term t, UInt arity USES_REGS);
extern Term Yap_UnNumberTerm(Term, int);
extern  Int Yap_SkipList(Term *, Term **);
extern Term Yap_BreakRational(Term inp, UInt arity, Term *of, Term oi USES_REGS);
extern Term Yap_BreakTerml(Term inp, UInt arity, Term *of, Term oi USES_REGS);

/* yap.c */

/* write.c */

/* yap2swi.c */
extern void Yap_swi_install(void);
extern void Yap_InitSWIHash(void);
extern int Yap_get_stream_handle(Term, int, int, void *);
extern Term Yap_get_stream_position(void *);
extern struct AtomEntryStruct *Yap_lookupBlob(void *blob, size_t len, void *type,
                                       int *newp);
extern void *Yap_RepStreamFromId(int sno);

/* opt.preds.c */
extern void Yap_init_optyap_preds(void);

/* pl-file.c */
//  struct PL_local_data *Yap_InitThreadIO(int wid);
extern void Yap_flush_all(void);

extern X_API YAP_opaque_tag_t
YAP_NewOpaqueType(struct YAP_opaque_handler_struct *f);

extern void pp(Term, int);

/* pl-yap.c */
extern Int Yap_source_line_no(void);
extern Atom Yap_source_file_name(void);

extern void Yap_install_blobs(void);

extern yamop *Yap_gcP(void);

#if USE_MYDDAS
extern  void init_myddas(void);
  #endif

#if !HAVE_STRNCAT
#define strncat(X, Y, Z) strcat(X, Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X, Y, Z) strcpy(X, Y)
#endif

#endif /* YAP_PROTOS_H */

/// @}
