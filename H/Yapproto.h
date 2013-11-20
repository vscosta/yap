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

/* absmi.c */
Int	     Yap_absmi(int);
int	     Yap_absmiEND(void);

/* adtdefs.c */
Term	Yap_ArrayToList(Term *,int);
int	Yap_GetName(char *,UInt,Term);
Term	Yap_GetValue(Atom);
int     Yap_HasOp(Atom);
struct operator_entry *Yap_GetOpPropForAModuleHavingALock(AtomEntry *, Term);
Atom	Yap_LookupAtom(char *);
Atom	Yap_LookupMaybeWideAtom(wchar_t *);
Atom	Yap_LookupMaybeWideAtomWithLength(wchar_t *, size_t);
Atom	Yap_FullLookupAtom(char *);
void	Yap_LookupAtomWithAddress(char *,AtomEntry *);
Prop	Yap_NewPredPropByFunctor(struct FunctorEntryStruct *, Term);
Prop	Yap_NewPredPropByAtom(struct AtomEntryStruct *, Term);
Prop	Yap_PredPropByFunctorNonThreadLocal(struct FunctorEntryStruct *, Term);
Prop	Yap_PredPropByAtomNonThreadLocal(struct AtomEntryStruct *, Term);
Functor	Yap_UnlockedMkFunctor(AtomEntry *,unsigned int);
Functor	Yap_MkFunctor(Atom,unsigned int);
void	Yap_MkFunctorWithAddress(Atom,unsigned int,FunctorEntry *);
void	Yap_PutValue(Atom,Term);
void	Yap_ReleaseAtom(Atom);
Term	Yap_StringToList(char *);
Term	Yap_NStringToList(char *, size_t);
Term	Yap_WideStringToList(wchar_t *);
Term	Yap_NWideStringToList(wchar_t *, size_t);
Term	Yap_StringToDiffList(char *,Term CACHE_TYPE);
Term	Yap_NStringToDiffList(char *,Term, size_t);
Term	Yap_WideStringToDiffList(wchar_t *,Term);
Term	Yap_NWideStringToDiffList(wchar_t *,Term, size_t);
Term	Yap_StringToListOfAtoms(char *);
Term	Yap_NStringToListOfAtoms(char *, size_t);
Term	Yap_WideStringToListOfAtoms(wchar_t *);
Term	Yap_NWideStringToListOfAtoms(wchar_t *, size_t);
Term	Yap_NWideStringToDiffListOfAtoms(wchar_t *, Term, size_t);
int     Yap_AtomIncreaseHold(Atom);
int     Yap_AtomDecreaseHold(Atom);
struct operator_entry *Yap_OpPropForModule(Atom, Term);


#ifdef SFUNC
Term	MkSFTerm(Functor,int,Term *,Term);
CELL   *ArgsOfSFTerm(Term);
#endif

Prop	Yap_GetPredPropByAtom(Atom, Term);
Prop	Yap_GetPredPropByFunc(Functor, Term);
Prop	Yap_GetPredPropByAtomInThisModule(Atom, Term);
Prop	Yap_GetPredPropByFuncInThisModule(Functor, Term);
Prop	Yap_GetPredPropHavingLock(Atom,unsigned int, Term);
Prop	Yap_GetExpProp(Atom,unsigned int);
Prop	Yap_GetExpPropHavingLock(AtomEntry *,unsigned int);

/* agc.c */
void    Yap_atom_gc( CACHE_TYPE1 );
void    Yap_init_agc( void );

/* alloc.c */
void	Yap_FreeCodeSpace(char *);
char   *Yap_AllocAtomSpace(size_t);
char   *Yap_AllocCodeSpace(size_t);
char   *Yap_ReallocCodeSpace(char *,size_t);
ADDR	Yap_AllocFromForeignArea(Int);
int     Yap_ExtendWorkSpace(Int);
void	Yap_FreeAtomSpace(char *);
int     Yap_FreeWorkSpace(void);
void	Yap_InitMemory(UInt,UInt,UInt);
void	Yap_InitExStacks(int,int,int);

/* amasm.c */
OPCODE	Yap_opcode(op_numbers);

/* analyst.c */
#ifdef ANALYST
void   Yap_InitAnalystPreds(void);
#endif /* ANALYST */

/* arrays.c */
void   Yap_InitArrayPreds(void);

/* atoms.c */
void Yap_InitBackAtoms(void);
void Yap_InitAtomPreds(void);


/* attvar.c */
void   Yap_InitAttVarPreds(void);
void   Yap_MkEmptyWakeUp(Atom mod);

/* bb.c */
void   Yap_InitBBPreds(void);

/* bignum.c */
Term   Yap_MkULLIntTerm(YAP_ULONG_LONG);
int    Yap_IsStringTerm(Term);
int    Yap_IsWideStringTerm(Term);
Term   Yap_RatTermToApplTerm(Term);
void   Yap_InitBigNums(void);
Term   Yap_AllocExternalDataInStack(CELL, size_t);
int    Yap_CleanOpaqueVariable(CELL *);

/* c_interface.c */
Int    YAP_Execute(struct pred_entry *, CPredicate);
Int    YAP_ExecuteFirst(struct pred_entry *, CPredicate);
Int    YAP_ExecuteNext(struct pred_entry *, CPredicate);
Int    YAP_ExecuteOnCut(struct pred_entry *, CPredicate, struct cut_c_str *);
Int    YAP_RunGoalOnce(Term);

/* cdmgr.c */
Term	Yap_all_calls(void);
Atom	Yap_ConsultingFile(void);
struct pred_entry *Yap_PredForChoicePt(choiceptr);
void	Yap_InitCdMgr(void);
void	Yap_init_consult(int, char *);
void	Yap_end_consult(void);
void	Yap_Abolish(struct pred_entry *);
void	Yap_BuildMegaClause(struct pred_entry *);
void	Yap_EraseMegaClause(yamop *,struct pred_entry *);
void	Yap_ResetConsultStack(void);
void	Yap_AssertzClause(struct pred_entry *, yamop *);
void    Yap_HidePred(struct pred_entry *pe);


/* cmppreds.c */
Int	Yap_compare_terms(Term,Term);
Int	Yap_acmp(Term, Term USES_REGS);
void	Yap_InitCmpPreds(void);

/* compiler.c */
yamop  *Yap_cclause(Term, Int, Term, Term);

/* computils.c */

/* corout.c */
void	Yap_InitCoroutPreds(void);
#ifdef COROUTINING
Term	Yap_ListOfWokenGoals(void);
void	Yap_WakeUp(CELL *);
#endif

/* dbase.c */
struct pred_entry  *Yap_FindLUIntKey(Int);
int     Yap_DBTrailOverflow(void);
CELL	Yap_EvalMasks(Term,CELL *);
void	Yap_InitBackDB(void);
void	Yap_InitDBPreds(void);

/* errors.c */
void	Yap_RestartYap(int);
void	Yap_exit(int);
yamop  *Yap_Error(yap_error_number,Term,char *msg, ...);
yamop  *Yap_NilError(yap_error_number,char *msg, ...);

/* eval.c */
void	Yap_InitEval(void);

/* exec.c */
Term	Yap_ExecuteCallMetaCall(Term);
void	Yap_InitExecFs(void);
Int	Yap_JumpToEnv(Term);
Term	Yap_RunTopGoal(Term);
void	Yap_ResetExceptionTerm(int);
Int	Yap_execute_goal(Term, int, Term);
Int	Yap_exec_absmi(int);
void	Yap_trust_last(void);
Term	Yap_GetException(void);
void	Yap_PrepGoal(UInt, CELL *, choiceptr USES_REGS);
int     Yap_dogc( int extra_args, Term *tp USES_REGS );

/* exo.c */
void	Yap_InitExoPreds(void);
void    Yap_udi_Interval_init(void);

/* foreign.c */
char   *Yap_FindExecutable(void);

/* gprof.c */
void	Yap_InitLowProf(void);
#if  LOW_PROF
void	Yap_inform_profiler_of_clause__(void *,void *,struct pred_entry *, gprof_info);
#define Yap_inform_profiler_of_clause(CODE0,CODEF,AP,MODE) {if (GLOBAL_FPreds) Yap_inform_profiler_of_clause__(CODE0,CODEF,AP,MODE);}
#else
#define	Yap_inform_profiler_of_clause(CODE0,CODEF,AP,MODE)
#endif
void    Yap_tell_gprof(yamop *);

/* globals.c */
Term	Yap_NewArena(UInt,CELL *);
CELL   *Yap_GetFromArena(Term *,UInt,UInt);
void	Yap_InitGlobals(void);
Term	Yap_SaveTerm(Term);
Term	Yap_SetGlobalVal(Atom, Term);
Int	Yap_DeleteGlobal(Atom);
void	Yap_AllocateDefaultArena(Int, Int, int);

/* grow.c */
Int     Yap_total_stack_shift_time(void);
void    Yap_InitGrowPreds(void);
UInt    Yap_InsertInGlobal(CELL *, UInt);
int     Yap_growheap(int, size_t, void *);
int     Yap_growstack( size_t );
int     Yap_growtrail(long, int);
int     Yap_growglobal(CELL **);
CELL  **Yap_shift_visit(CELL **, CELL ***, CELL ***);
#ifdef THREADS
void   Yap_CopyThreadStacks(int, int, int);
#endif

/* heapgc.c */
Int  Yap_total_gc_time(void);
void Yap_init_gc(void);
int  Yap_is_gc_verbose(void);
int  Yap_gc(Int, CELL *, yamop *);
int  Yap_gcl(UInt, Int, CELL *, yamop *);

/* init.c */
#ifdef DEBUG
int	Yap_DebugPutc(int,wchar_t);
void	Yap_DebugSetIFile(char *);
void	Yap_DebugEndline(void);
int	Yap_DebugGetc(void);
#endif
int	Yap_IsOpType(char *);
void	Yap_InitCPred(char *, unsigned long int, CPredicate, UInt);
void	Yap_InitAsmPred(char *, unsigned long int, int, CPredicate, UInt);
void	Yap_InitCmpPred(char *, unsigned long int, CmpPredicate, UInt);
void	Yap_InitCPredBack(char *, unsigned long int, unsigned int, CPredicate,CPredicate,UInt);
void	Yap_InitCPredBackCut(char *, unsigned long int, unsigned int, CPredicate,CPredicate,CPredicate,UInt);
#ifdef CUT_C
void    Yap_InitCPredBack_(char *, unsigned long int, unsigned int, CPredicate,CPredicate,CPredicate,UInt);
#endif
void	Yap_InitWorkspace(UInt,UInt,UInt,UInt,UInt,int,int,int);

#ifdef YAPOR
void    Yap_init_yapor_workers(void);
#endif /* YAPOR */
#if defined(YAPOR) || defined(THREADS)
void	Yap_KillStacks(int);
#else
void	Yap_KillStacks(int);
#endif
void	Yap_InitYaamRegs(int);
void    Yap_ReInitWallTime(void);
int	Yap_OpDec(int,char *,Atom,Term);
void    Yap_CloseScratchPad(void);

/* inlines.c */
void    Yap_InitInlines(void);
int      Yap_eq(Term, Term);

/* iopreds.c */
void	Yap_InitBackIO(void);
void	Yap_InitIOPreds(void);
void   *Yap_GetStreamHandle(Atom at);
void   *Yap_GetInputStream(Atom at);
void   *Yap_GetOutputStream(Atom at);
#ifdef DEBUG
extern void Yap_DebugPlWrite (Term t);
extern void Yap_DebugErrorPutc (int n);
#endif
void    Yap_PlWriteToStream(Term, int, int);
/* depth_lim.c */
void	Yap_InitItDeepenPreds(void);

/* load_foreign.c */
void	Yap_InitLoadForeign(void);

/* mavar.c */
void	Yap_InitMaVarCPreds(void);
Term    Yap_NewTimedVar(Term);
Term    Yap_NewEmptyTimedVar(void);
Term	Yap_ReadTimedVar(Term);
Term    Yap_UpdateTimedVar(Term, Term);

/* modules.c */
Term    Yap_Module(Term);
Term    Yap_Module_Name(struct pred_entry *);
struct pred_entry *Yap_ModulePred(Term);
void    Yap_NewModulePred(Term, struct pred_entry *);
Term    Yap_StripModule(Term, Term *);
void    Yap_InitModules(void);
void    Yap_InitModulesC(void);
struct mod_entry *Yap_GetModuleEntry(Term tmod);
Term Yap_GetModuleFromEntry(struct mod_entry *me);


#if HAVE_MPI
/* mpi.c */
void    Yap_InitMPI(void);
#endif

#if HAVE_MPE
/* mpe.c */
void    Yap_InitMPE(void);
#endif


/* other.c */
Term	Yap_MkApplTerm(Functor,unsigned int,Term *);
Term	Yap_MkNewApplTerm(Functor,unsigned int);
Term	Yap_MkNewPairTerm(void);
Term	Yap_Globalise(Term);


/* parser.c */
Term	Yap_Parse(void);

/* readutil.c */
void	Yap_InitReadUtil(void);

/* qly.c */
void	Yap_InitQLY(void);
int 	Yap_Restore(char *, char *);
void	Yap_InitQLYR(void);

/* range.c */
void Yap_InitRange(void);

/* save.c */
int	Yap_SavedInfo(char *,char *,CELL *,CELL *,CELL *);
int 	Yap_SavedStateRestore(char *, char *);
struct io_stream *Yap_OpenRestore(char *, char *);
void	Yap_InitSavePreds(void);

/* scanner.c */

/* signals.c */
void	Yap_signal(yap_signals);
void	Yap_undo_signal(yap_signals);
void	Yap_InitSignalCPreds(void);

/* sort.c */
void    Yap_InitSortPreds(void);

/* stdpreds.c */
void	Yap_InitBackCPreds(void);
void	Yap_InitCPreds(void);
void	Yap_show_statistics(void);
int	Yap_IsOpMaxPrio(Atom);

/* sysbits.c */
void    Yap_InitPageSize(void);
void	Yap_set_fpu_exceptions(int);
UInt	Yap_cputime(void);
Int	Yap_walltime(void);
int	Yap_dir_separator(int);
int	Yap_volume_header(char *);
void	Yap_InitSysPath(void);
int	Yap_signal_index(const char *);
#ifdef MAC
void	Yap_SetTextFile(char *);
#endif
int     Yap_getcwd(const char *, int);
void    Yap_cputime_interval(Int *,Int *);
void    Yap_systime_interval(Int *,Int *);
void    Yap_walltime_interval(Int *,Int *);
void	Yap_InitSysbits(void);
void	Yap_InitSysPreds(void);
void	Yap_InitTime(int);
int     Yap_TrueFileName(char *, char *, int);
double  Yap_random(void);
#ifdef _WIN32
char	*Yap_RegistryGetString(char *);
void	Yap_WinError(char *);
#endif

/* threads.c */
void   Yap_InitThreadPreds(void);
void   Yap_InitFirstWorkerThreadHandle(void);
#if THREADS
int    Yap_InitThread(int);
#endif

/* tracer.c */
#ifdef LOW_LEVEL_TRACER
void	Yap_InitLowLevelTrace(void);
#endif

/* udi.c */
void	Yap_udi_init(void);
void	Yap_udi_abolish(struct pred_entry *);

/* unify.c */
int          Yap_rational_tree_loop(CELL *, CELL *, CELL **, CELL **);
void         Yap_InitAbsmi(void);
void         Yap_InitUnify(void);
void         Yap_TrimTrail(void);
int          Yap_Unifiable(Term d0, Term d1);
int          Yap_IUnify(register CELL d0,register CELL d1);

/* userpreds.c */
void	Yap_InitUserCPreds(void);
void	Yap_InitUserBacks(void);

/* utilpreds.c */
Term	Yap_CopyTerm(Term);
int	Yap_Variant(Term, Term);
size_t	Yap_ExportTerm(Term, char *, size_t, UInt);
size_t	Yap_SizeOfExportedTerm(char *);
Term	Yap_ImportTerm(char *);
int	Yap_IsListTerm(Term);
int	Yap_IsListOrPartialListTerm(Term);
Term	Yap_CopyTermNoShare(Term);
int	Yap_SizeGroundTerm(Term, int);
int	Yap_IsGroundTerm(Term);
int	Yap_IsAcyclicTerm(Term);
void	Yap_InitUtilCPreds(void);
Int     Yap_TermHash(Term, Int, Int, int);
Int     Yap_NumberVars(Term, Int, int);
Term    Yap_UnNumberTerm(Term, int);
Int     Yap_SkipList(Term *, Term **);
/* yap.c */


/* write.c */
void	Yap_plwrite(Term, void *, int, int, int);


/* MYDDAS */

#if defined MYDDAS_MYSQL || defined MYDDAS_ODBC

/* myddas_initialization.c */
MYDDAS_GLOBAL          myddas_init_initialize_myddas(void);
MYDDAS_UTIL_CONNECTION myddas_init_initialize_connection(void *,void *,MYDDAS_UTIL_CONNECTION);
MYDDAS_UTIL_PREDICATE  myddas_init_initialize_predicate(char *, int, char *,MYDDAS_UTIL_PREDICATE);

#ifdef MYDDAS_STATS
/* myddas_statistics.c */
MYDDAS_GLOBAL          myddas_stats_initialize_global_stats(MYDDAS_GLOBAL);
MYDDAS_STATS_STRUCT    myddas_stats_initialize_connection_stats(void);
void                   myddas_stats_delete_stats_list(MYDDAS_STATS_STRUCT);
#endif /* MYDDAS_STATS */

#ifdef MYDDAS_MYSQL
/* myddas_util.c */
void                   myddas_util_table_write(MYSQL_RES *);
#endif
Short                  myddas_util_connection_type(void *);
MYDDAS_UTIL_CONNECTION myddas_util_add_connection(void *,void *);
MYDDAS_UTIL_CONNECTION myddas_util_search_connection(void *);
void                   myddas_util_delete_connection(void *);
MYDDAS_UTIL_CONNECTION myddas_util_add_predicate(char * ,Int , char *,void *);
MYDDAS_UTIL_PREDICATE  myddas_util_search_predicate(char * ,Int , char *);
void                   myddas_util_delete_predicate(MYDDAS_UTIL_PREDICATE);

/* Get's the number of queries to save */
UInt                   myddas_util_get_total_multi_queries_number(MYDDAS_UTIL_CONNECTION);
void                   myddas_util_set_total_multi_queries_number(MYDDAS_UTIL_CONNECTION,UInt);
#ifdef MYDDAS_ODBC
/* Return enviromment identifier*/
SQLHENV                myddas_util_get_odbc_enviromment(SQLHDBC);
#endif

void *                 myddas_util_get_list_pred(MYDDAS_UTIL_CONNECTION);
void *                 myddas_util_get_pred_next(void *);
char *                 myddas_util_get_pred_module(void *);
char *                 myddas_util_get_pred_name(void *);
MyddasInt              myddas_util_get_pred_arity(void *);
//DELETE THIS WHEN DB_STATS  IS COMPLETED
MyddasInt              get_myddas_top(void);

#ifdef DEBUG
void check_int(void);
#endif

#endif /* MYDDAS_MYSQL || MYDDAS_ODBC */

/* myddas_mysql.c */
#if defined MYDDAS_MYSQL
void    Yap_InitMYDDAS_MySQLPreds(void);
void    Yap_InitBackMYDDAS_MySQLPreds(void);
#endif

/* myddas_odbc.c */
#if defined MYDDAS_ODBC
void    Yap_InitMYDDAS_ODBCPreds(void);
void    Yap_InitBackMYDDAS_ODBCPreds(void);
#endif

/* myddas_shared.c */
#if defined MYDDAS_ODBC || defined MYDDAS_MYSQL
void    Yap_MYDDAS_delete_all_myddas_structs(void);
void    Yap_InitMYDDAS_SharedPreds(void);
void    Yap_InitBackMYDDAS_SharedPreds(void);
#endif

/* myddas_top_level.c */
#if defined MYDDAS_TOP_LEVEL && defined MYDDAS_MYSQL //&& defined HAVE_LIBREADLINE
void    Yap_InitMYDDAS_TopLevelPreds(void);
#endif

/* yap2swi.c */
void	Yap_swi_install(void);
void    Yap_InitSWIHash(void);
int     Yap_get_stream_handle(Term, int, int, void *);
Term    Yap_get_stream_position(void *);

/* opt.preds.c */
void    Yap_init_optyap_preds(void);

/* pl-file.c */
struct PL_local_data *Yap_InitThreadIO(int wid); 
void Yap_flush(void);

/* pl-yap.c */
Int Yap_source_line_no( void );
Atom Yap_source_file_name( void );

static inline
yamop *
gc_P(yamop *p, yamop *cp)
{
  return (p->opc == Yap_opcode(_execute_cpred) ? cp : p);
}


