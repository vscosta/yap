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

#define STD_PROTO(F,A)  F A
#define STATIC_PROTO(F,A) static F A


/* absmi.c */
Int	     STD_PROTO(Yap_absmi,(int));
int	     STD_PROTO(Yap_absmiEND,(void));

/* adtdefs.c */
Term	STD_PROTO(Yap_ArrayToList,(Term *,int));
int	STD_PROTO(Yap_GetName,(char *,UInt,Term));
Term	STD_PROTO(Yap_GetValue,(Atom));
int     STD_PROTO(Yap_HasOp,(Atom));
struct operator_entry *STD_PROTO(Yap_GetOpPropForAModuleHavingALock,(AtomEntry *, Term));
Atom	STD_PROTO(Yap_LookupAtom,(char *));
Atom	STD_PROTO(Yap_LookupMaybeWideAtom,(wchar_t *));
Atom	STD_PROTO(Yap_LookupMaybeWideAtomWithLength,(wchar_t *, size_t));
Atom	STD_PROTO(Yap_FullLookupAtom,(char *));
void	STD_PROTO(Yap_LookupAtomWithAddress,(char *,AtomEntry *));
Prop	STD_PROTO(Yap_NewPredPropByFunctor,(struct FunctorEntryStruct *, Term));
Prop	STD_PROTO(Yap_NewPredPropByAtom,(struct AtomEntryStruct *, Term));
Prop	STD_PROTO(Yap_PredPropByFunctorNonThreadLocal,(struct FunctorEntryStruct *, Term));
Prop	STD_PROTO(Yap_PredPropByAtomNonThreadLocal,(struct AtomEntryStruct *, Term));
Functor	STD_PROTO(Yap_UnlockedMkFunctor,(AtomEntry *,unsigned int));
Functor	STD_PROTO(Yap_MkFunctor,(Atom,unsigned int));
void	STD_PROTO(Yap_MkFunctorWithAddress,(Atom,unsigned int,FunctorEntry *));
void	STD_PROTO(Yap_PutValue,(Atom,Term));
void	STD_PROTO(Yap_ReleaseAtom,(Atom));
Term	STD_PROTO(Yap_StringToList,(char *));
Term	STD_PROTO(Yap_NStringToList,(char *, size_t));
Term	STD_PROTO(Yap_WideStringToList,(wchar_t *));
Term	STD_PROTO(Yap_NWideStringToList,(wchar_t *, size_t));
Term	STD_PROTO(Yap_StringToDiffList,(char *,Term CACHE_TYPE));
Term	STD_PROTO(Yap_NStringToDiffList,(char *,Term, size_t));
Term	STD_PROTO(Yap_WideStringToDiffList,(wchar_t *,Term));
Term	STD_PROTO(Yap_NWideStringToDiffList,(wchar_t *,Term, size_t));
Term	STD_PROTO(Yap_StringToListOfAtoms,(char *));
Term	STD_PROTO(Yap_NStringToListOfAtoms,(char *, size_t));
Term	STD_PROTO(Yap_WideStringToListOfAtoms,(wchar_t *));
Term	STD_PROTO(Yap_NWideStringToListOfAtoms,(wchar_t *, size_t));
Term	STD_PROTO(Yap_NWideStringToDiffListOfAtoms,(wchar_t *, Term, size_t));
int     STD_PROTO(Yap_AtomIncreaseHold,(Atom));
int     STD_PROTO(Yap_AtomDecreaseHold,(Atom));
struct operator_entry *STD_PROTO(Yap_OpPropForModule,(Atom, Term));
Int	STD_PROTO(Yap_InitSlot,(Term CACHE_TYPE));
Int     STD_PROTO(Yap_NewSlots,(int CACHE_TYPE));
int     STD_PROTO(Yap_RecoverSlots,(int CACHE_TYPE));


#ifdef SFUNC
Term	STD_PROTO(MkSFTerm,(Functor,int,Term *,Term));
CELL	STD_PROTO(*ArgsOfSFTerm,(Term));
#endif

Prop	STD_PROTO(Yap_GetPredPropByAtom,(Atom, Term));
Prop	STD_PROTO(Yap_GetPredPropByFunc,(Functor, Term));
Prop	STD_PROTO(Yap_GetPredPropByAtomInThisModule,(Atom, Term));
Prop	STD_PROTO(Yap_GetPredPropByFuncInThisModule,(Functor, Term));
Prop	STD_PROTO(Yap_GetPredPropHavingLock,(Atom,unsigned int, Term));
Prop	STD_PROTO(Yap_GetExpProp,(Atom,unsigned int));
Prop	STD_PROTO(Yap_GetExpPropHavingLock,(AtomEntry *,unsigned int));

/* agc.c */
void    STD_PROTO(Yap_atom_gc, (CACHE_TYPE1));
void    STD_PROTO(Yap_init_agc, (void));

/* alloc.c */
void	STD_PROTO(Yap_FreeCodeSpace,(char *));
char   *STD_PROTO(Yap_AllocAtomSpace,(unsigned long int));
char   *STD_PROTO(Yap_AllocCodeSpace,(unsigned long int));
char   *STD_PROTO(Yap_ReallocCodeSpace,(char *,unsigned long int));
ADDR	STD_PROTO(Yap_AllocFromForeignArea,(Int));
int     STD_PROTO(Yap_ExtendWorkSpace,(Int));
void	STD_PROTO(Yap_FreeAtomSpace,(char *));
int     STD_PROTO(Yap_FreeWorkSpace, (void));
void	STD_PROTO(Yap_InitMemory,(UInt,UInt,UInt));
void	STD_PROTO(Yap_InitExStacks,(int,int));

/* amasm.c */
OPCODE	STD_PROTO(Yap_opcode,(op_numbers));

/* analyst.c */
#ifdef ANALYST
void   STD_PROTO(Yap_InitAnalystPreds,(void));
#endif /* ANALYST */

/* arrays.c */
void   STD_PROTO(Yap_InitArrayPreds,(void));

/* attvar.c */
void   STD_PROTO(Yap_InitAttVarPreds,(void));

/* bb.c */
void   STD_PROTO(Yap_InitBBPreds,(void));

/* bignum.c */
Term   STD_PROTO(Yap_MkULLIntTerm, (YAP_ULONG_LONG));
int    STD_PROTO(Yap_IsStringTerm, (Term));
int    STD_PROTO(Yap_IsWideStringTerm, (Term));
Term   STD_PROTO(Yap_RatTermToApplTerm, (Term));
void   STD_PROTO(Yap_InitBigNums, (void));
Term   STD_PROTO(Yap_AllocExternalDataInStack, (CELL, size_t));
int    STD_PROTO(Yap_CleanOpaqueVariable, (CELL *));

/* c_interface.c */
Int    STD_PROTO(YAP_Execute,(struct pred_entry *, CPredicate));
Int    STD_PROTO(YAP_ExecuteFirst,(struct pred_entry *, CPredicate));
Int    STD_PROTO(YAP_ExecuteNext,(struct pred_entry *, CPredicate));
Int    STD_PROTO(YAP_ExecuteOnCut,(struct pred_entry *, CPredicate, struct cut_c_str *));

/* cdmgr.c */
Term	STD_PROTO(Yap_all_calls,(void));
Atom	STD_PROTO(Yap_ConsultingFile,(void));
struct pred_entry *STD_PROTO(Yap_PredForChoicePt,(choiceptr));
void	STD_PROTO(Yap_InitCdMgr,(void));
void	STD_PROTO(Yap_init_consult,(int, char *));
void	STD_PROTO(Yap_end_consult,(void));
void	STD_PROTO(Yap_Abolish,(struct pred_entry *));
void	STD_PROTO(Yap_BuildMegaClause,(struct pred_entry *));
void	STD_PROTO(Yap_EraseMegaClause,(yamop *,struct pred_entry *));
void	STD_PROTO(Yap_ResetConsultStack,(void));
void	STD_PROTO(Yap_AssertzClause,(struct pred_entry *, yamop *));


/* cmppreds.c */
Int	STD_PROTO(Yap_compare_terms,(Term,Term));
void	STD_PROTO(Yap_InitCmpPreds,(void));

/* compiler.c */
yamop  *STD_PROTO(Yap_cclause,(Term, Int, Term, Term));

/* computils.c */

/* corout.c */
void	STD_PROTO(Yap_InitCoroutPreds,(void));
#ifdef COROUTINING
Term	STD_PROTO(Yap_ListOfWokenGoals,(void));
void	STD_PROTO(Yap_WakeUp,(CELL *));
#endif

/* dbase.c */
struct pred_entry  *STD_PROTO(Yap_FindLUIntKey,(Int));
int     STD_PROTO(Yap_DBTrailOverflow,(void));
CELL	STD_PROTO(Yap_EvalMasks,(Term,CELL *));
void	STD_PROTO(Yap_InitBackDB,(void));
void	STD_PROTO(Yap_InitDBPreds,(void));

/* errors.c */
void	STD_PROTO(Yap_exit,(int));
yamop  *STD_PROTO(Yap_Error,(yap_error_number,Term,char *msg, ...));
yamop  *STD_PROTO(Yap_NilError,(yap_error_number,char *msg, ...));

/* eval.c */
void	STD_PROTO(Yap_InitEval,(void));

/* exec.c */
Term	STD_PROTO(Yap_ExecuteCallMetaCall,(Term));
void	STD_PROTO(Yap_InitExecFs,(void));
Int	STD_PROTO(Yap_JumpToEnv,(Term));
Term	STD_PROTO(Yap_RunTopGoal,(Term));
void	STD_PROTO(Yap_ResetExceptionTerm,(void));
Int	STD_PROTO(Yap_execute_goal,(Term, int, Term));
Int	STD_PROTO(Yap_exec_absmi,(int));
void	STD_PROTO(Yap_trust_last,(void));
Term	STD_PROTO(Yap_GetException,(void));

/* gprof.c */
void	STD_PROTO(Yap_InitLowProf,(void));
#if  LOW_PROF
void	STD_PROTO(Yap_inform_profiler_of_clause,(struct yami *,struct yami *,struct pred_entry *,int));
#else
#define	Yap_inform_profiler_of_clause(A,B,C,D)
#endif

/* globals.c */
Term	STD_PROTO(Yap_NewArena,(UInt,CELL *));
CELL   *STD_PROTO(Yap_GetFromArena,(Term *,UInt,UInt));
void	STD_PROTO(Yap_InitGlobals,(void));
Term	STD_PROTO(Yap_SaveTerm, (Term));
Term	STD_PROTO(Yap_SetGlobalVal, (Atom, Term));
Int	STD_PROTO(Yap_DeleteGlobal, (Atom));
void	STD_PROTO(Yap_AllocateDefaultArena, (Int, Int));

/* grow.c */
Int     STD_PROTO(Yap_total_stack_shift_time,(void));
void    STD_PROTO(Yap_InitGrowPreds, (void));
UInt    STD_PROTO(Yap_InsertInGlobal, (CELL *, UInt));
int     STD_PROTO(Yap_growheap,      (int, UInt, void *));
int     STD_PROTO(Yap_growstack,     (long));
int     STD_PROTO(Yap_growtrail,     (long, int));
int     STD_PROTO(Yap_growglobal,    (CELL **));
CELL  **STD_PROTO(Yap_shift_visit,   (CELL **, CELL ***));
#ifdef THREADS
void   STD_PROTO(Yap_CopyThreadStacks, (int, int, int));
#endif

/* heapgc.c */
Int  STD_PROTO(Yap_total_gc_time,(void));
void STD_PROTO(Yap_init_gc,(void));
int  STD_PROTO(Yap_is_gc_verbose, (void));
int  STD_PROTO(Yap_gc, (Int, CELL *, yamop *));
int  STD_PROTO(Yap_gcl, (UInt, Int, CELL *, yamop *));

/* init.c */
#ifdef DEBUG
int	STD_PROTO(Yap_DebugPutc,(int,wchar_t));
void	STD_PROTO(Yap_DebugSetIFile,(char *));
void	STD_PROTO(Yap_DebugEndline,(void));
int	STD_PROTO(Yap_DebugGetc,(void));
#endif
int	STD_PROTO(Yap_IsOpType,(char *));
void	STD_PROTO(Yap_InitCPred,(char *, unsigned long int, CPredicate, UInt));
void	STD_PROTO(Yap_InitAsmPred,(char *, unsigned long int, int, CPredicate, UInt));
void	STD_PROTO(Yap_InitCmpPred,(char *, unsigned long int, CmpPredicate, UInt));
void	STD_PROTO(Yap_InitCPredBack,(char *, unsigned long int, unsigned int, CPredicate,CPredicate,UInt));
void	STD_PROTO(Yap_InitCPredBackCut,(char *, unsigned long int, unsigned int, CPredicate,CPredicate,CPredicate,UInt));
#ifdef CUT_C
void    STD_PROTO(Yap_InitCPredBack_,(char *, unsigned long int, unsigned int, CPredicate,CPredicate,CPredicate,UInt));
#endif
void	STD_PROTO(Yap_InitWorkspace,(UInt,UInt,UInt,UInt,UInt,int,int,int));

#ifdef YAPOR
void    STD_PROTO(Yap_init_yapor_workers, (void));
#endif /* YAPOR */
#if defined(YAPOR) || defined(THREADS)
void	STD_PROTO(Yap_KillStacks,(int));
#else
void	STD_PROTO(Yap_KillStacks,(int));
#endif
void	STD_PROTO(Yap_InitYaamRegs,(void));
void    STD_PROTO(Yap_ReInitWallTime, (void));
int	STD_PROTO(Yap_OpDec,(int,char *,Atom,Term));
void    STD_PROTO(Yap_CloseScratchPad,(void));

/* inlines.c */
void    STD_PROTO(Yap_InitInlines,(void));
int      STD_PROTO(Yap_eq,(Term, Term));

/* iopreds.c */
void	STD_PROTO(Yap_InitBackIO,(void));
void	STD_PROTO(Yap_InitIOPreds,(void));
#ifdef DEBUG
extern void Yap_DebugPlWrite (Term t);
extern void Yap_DebugErrorPutc (int n);
#endif
int	STD_PROTO(Yap_LookupSWIStream,(void *));
int     STD_PROTO(Yap_readTerm, (void *, Term *, Term *, Term *, Term *));
void    STD_PROTO(Yap_PlWriteToStream, (Term, int, int));
/* depth_lim.c */
void	STD_PROTO(Yap_InitItDeepenPreds,(void));

/* load_foreign.c */
void	STD_PROTO(Yap_InitLoadForeign,(void));

/* mavar.c */
void	STD_PROTO(Yap_InitMaVarCPreds,(void));
Term    STD_PROTO(Yap_NewTimedVar,(Term));
Term    STD_PROTO(Yap_NewEmptyTimedVar,(void));
Term	STD_PROTO(Yap_ReadTimedVar,(Term));
Term    STD_PROTO(Yap_UpdateTimedVar,(Term, Term));

/* modules.c */
Term    STD_PROTO(Yap_Module, (Term));
Term    STD_PROTO(Yap_Module_Name, (struct pred_entry *));
struct pred_entry *STD_PROTO(Yap_ModulePred, (Term));
void    STD_PROTO(Yap_NewModulePred, (Term, struct pred_entry *));
Term    STD_PROTO(Yap_StripModule, (Term, Term *));
void    STD_PROTO(Yap_InitModules, (void));
void    STD_PROTO(Yap_InitModulesC, (void));

#if HAVE_MPI
/* mpi.c */
void    STD_PROTO(Yap_InitMPI,(void));
#endif

#if HAVE_MPE
/* mpe.c */
void    STD_PROTO(Yap_InitMPE,(void));
#endif


/* other.c */
Term	STD_PROTO(Yap_MkApplTerm,(Functor,unsigned int,Term *));
Term	STD_PROTO(Yap_MkNewApplTerm,(Functor,unsigned int));
Term	STD_PROTO(Yap_MkNewPairTerm,(void));
Term	STD_PROTO(Yap_Globalise,(Term));


/* parser.c */
Term	STD_PROTO(Yap_Parse,(void));

/* readutil.c */
void	STD_PROTO(Yap_InitReadUtil,(void));

/* qly.c */
void	STD_PROTO(Yap_InitQLY,(void));
int 	STD_PROTO(Yap_Restore,(char *, char *));
void	STD_PROTO(Yap_InitQLYR,(void));

/* save.c */
int	STD_PROTO(Yap_SavedInfo,(char *,char *,CELL *,CELL *,CELL *));
int 	STD_PROTO(Yap_SavedStateRestore,(char *, char *));
struct io_stream *STD_PROTO(Yap_OpenRestore,(char *, char *));
void	STD_PROTO(Yap_InitSavePreds,(void));

/* scanner.c */

/* sort.c */
void    STD_PROTO(Yap_InitSortPreds,(void));

/* stdpreds.c */
void	STD_PROTO(Yap_InitBackCPreds,(void));
void	STD_PROTO(Yap_InitCPreds,(void));
void	STD_PROTO(Yap_show_statistics,(void));
void	STD_PROTO(Yap_signal,(yap_signals));
void	STD_PROTO(Yap_undo_signal,(yap_signals));
int	STD_PROTO(Yap_IsOpMaxPrio,(Atom));

/* sysbits.c */
void    STD_PROTO(Yap_InitPageSize, (void));
void	STD_PROTO(Yap_set_fpu_exceptions,(int));
UInt	STD_PROTO(Yap_cputime,(void));
Int	STD_PROTO(Yap_walltime,(void));
int	STD_PROTO(Yap_dir_separator,(int));
int	STD_PROTO(Yap_volume_header,(char *));
void	STD_PROTO(Yap_InitSysPath,(void));
int	STD_PROTO(Yap_signal_index,(const char *));
#ifdef MAC
void	STD_PROTO(Yap_SetTextFile,(char *));
#endif
int     STD_PROTO(Yap_getcwd,(const char *, int));
void    STD_PROTO(Yap_cputime_interval,(Int *,Int *));
void    STD_PROTO(Yap_systime_interval,(Int *,Int *));
void    STD_PROTO(Yap_walltime_interval,(Int *,Int *));
void	STD_PROTO(Yap_InitSysbits,(void));
void	STD_PROTO(Yap_InitSysPreds,(void));
void	STD_PROTO(Yap_InitTime,(void));
int     STD_PROTO(Yap_TrueFileName, (char *, char *, int));
int	STD_PROTO(Yap_ProcessSIGINT,(void));
double  STD_PROTO(Yap_random, (void));
#ifdef _WIN32
char	*STD_PROTO(Yap_RegistryGetString,(char *));
void	STD_PROTO(Yap_WinError,(char *));
#endif

/* threads.c */
void   STD_PROTO(Yap_InitThreadPreds,(void));
#if THREADS
int    STD_PROTO(Yap_InitThread,(int));
#endif

/* tracer.c */
#ifdef LOW_LEVEL_TRACER
void	STD_PROTO(Yap_InitLowLevelTrace,(void));
#endif

/* udi.c */
void	STD_PROTO(Yap_udi_init,(void));

/* unify.c */
int          STD_PROTO(Yap_rational_tree_loop, (CELL *, CELL *, CELL **, CELL **));
void         STD_PROTO(Yap_InitAbsmi,(void));
void         STD_PROTO(Yap_InitUnify,(void));
void         STD_PROTO(Yap_TrimTrail,(void));
int          STD_PROTO(Yap_Unifiable,(Term d0, Term d1));
int          STD_PROTO(Yap_IUnify,(register CELL d0,register CELL d1));

/* userpreds.c */
void	STD_PROTO(Yap_InitUserCPreds,(void));
void	STD_PROTO(Yap_InitUserBacks,(void));

/* utilpreds.c */
Term	STD_PROTO(Yap_CopyTerm,(Term));
int	STD_PROTO(Yap_Variant,(Term, Term));
size_t	STD_PROTO(Yap_ExportTerm,(Term, char *, size_t, UInt));
size_t	STD_PROTO(Yap_SizeOfExportedTerm,(char *));
Term	STD_PROTO(Yap_ImportTerm,(char *));
int	STD_PROTO(Yap_IsListTerm,(Term));
Term	STD_PROTO(Yap_CopyTermNoShare,(Term));
int	STD_PROTO(Yap_SizeGroundTerm,(Term, int));
int	STD_PROTO(Yap_IsGroundTerm,(Term));
void	STD_PROTO(Yap_InitUtilCPreds,(void));
Int     STD_PROTO(Yap_TermHash,(Term, Int, Int, int));
Int     STD_PROTO(Yap_NumberVars,(Term, Int));
Term    STD_PROTO(Yap_UnNumberTerm,(Term, int));
Int     STD_PROTO(Yap_SkipList,(Term *, Term **));
/* yap.c */


/* write.c */
void	STD_PROTO(Yap_plwrite,(Term,int (*)(int, wchar_t), int, int));


/* MYDDAS */

#if defined MYDDAS_MYSQL || defined MYDDAS_ODBC

/* myddas_initialization.c */
MYDDAS_GLOBAL          STD_PROTO(myddas_init_initialize_myddas,(void));
MYDDAS_UTIL_CONNECTION STD_PROTO(myddas_init_initialize_connection,(void *,void *,MYDDAS_UTIL_CONNECTION));
MYDDAS_UTIL_PREDICATE  STD_PROTO(myddas_init_initialize_predicate,(char *, int, char *,MYDDAS_UTIL_PREDICATE));

#ifdef MYDDAS_STATS
/* myddas_statistics.c */
MYDDAS_GLOBAL          STD_PROTO(myddas_stats_initialize_global_stats,(MYDDAS_GLOBAL));
MYDDAS_STATS_STRUCT    STD_PROTO(myddas_stats_initialize_connection_stats,(void));
void                   STD_PROTO(myddas_stats_delete_stats_list,(MYDDAS_STATS_STRUCT));
#endif /* MYDDAS_STATS */

#ifdef MYDDAS_MYSQL
/* myddas_util.c */
void                   STD_PROTO(myddas_util_table_write,(MYSQL_RES *));
#endif
Short                  STD_PROTO(myddas_util_connection_type,(void *));
MYDDAS_UTIL_CONNECTION STD_PROTO(myddas_util_add_connection,(void *,void *));
MYDDAS_UTIL_CONNECTION STD_PROTO(myddas_util_search_connection,(void *));
void                   STD_PROTO(myddas_util_delete_connection,(void *));
MYDDAS_UTIL_CONNECTION STD_PROTO(myddas_util_add_predicate,(char * ,Int , char *,void *));
MYDDAS_UTIL_PREDICATE  STD_PROTO(myddas_util_search_predicate,(char * ,Int , char *));
void                   STD_PROTO(myddas_util_delete_predicate,(MYDDAS_UTIL_PREDICATE));

/* Get's the number of queries to save */
UInt                   STD_PROTO(myddas_util_get_total_multi_queries_number,(MYDDAS_UTIL_CONNECTION));
void                   STD_PROTO(myddas_util_set_total_multi_queries_number,(MYDDAS_UTIL_CONNECTION,UInt));
#ifdef MYDDAS_ODBC
/* Return enviromment identifier*/
SQLHENV                STD_PROTO(myddas_util_get_odbc_enviromment,(SQLHDBC));
#endif

void *                 STD_PROTO(myddas_util_get_list_pred,(MYDDAS_UTIL_CONNECTION));
void *                 STD_PROTO(myddas_util_get_pred_next,(void *));
char *                 STD_PROTO(myddas_util_get_pred_module,(void *));
char *                 STD_PROTO(myddas_util_get_pred_name,(void *));
MyddasInt              STD_PROTO(myddas_util_get_pred_arity,(void *));
//DELETE THIS WHEN DB_STATS  IS COMPLETED
MyddasInt              STD_PROTO(get_myddas_top,(void));

#ifdef DEBUG
void check_int(void);
#endif

#endif /* MYDDAS_MYSQL || MYDDAS_ODBC */

/* myddas_mysql.c */
#if defined MYDDAS_MYSQL
void    STD_PROTO(Yap_InitMYDDAS_MySQLPreds,(void));
void    STD_PROTO(Yap_InitBackMYDDAS_MySQLPreds,(void));
#endif

/* myddas_odbc.c */
#if defined MYDDAS_ODBC
void    STD_PROTO(Yap_InitMYDDAS_ODBCPreds,(void));
void    STD_PROTO(Yap_InitBackMYDDAS_ODBCPreds,(void));
#endif

/* myddas_shared.c */
#if defined MYDDAS_ODBC || defined MYDDAS_MYSQL
void    STD_PROTO(Yap_MYDDAS_delete_all_myddas_structs,(void));
void    STD_PROTO(Yap_InitMYDDAS_SharedPreds,(void));
void    STD_PROTO(Yap_InitBackMYDDAS_SharedPreds,(void));
#endif

/* myddas_top_level.c */
#if defined MYDDAS_TOP_LEVEL && defined MYDDAS_MYSQL //&& defined HAVE_LIBREADLINE
void    STD_PROTO(Yap_InitMYDDAS_TopLevelPreds,(void));
#endif

/* yap2swi.c */
void	STD_PROTO(Yap_swi_install,(void));
void    STD_PROTO(Yap_InitSWIHash,(void));
int     STD_PROTO(Yap_get_stream_handle,(Term, int, int, void *));
Term    STD_PROTO(Yap_get_stream_position,(void *));

/* opt.preds.c */
void    STD_PROTO(Yap_init_optyap_preds,(void));

/* pl-file.c */
struct PL_local_data *Yap_InitThreadIO(int wid); 

static inline
yamop *
gc_P(yamop *p, yamop *cp)
{
  return (p->opc == Yap_opcode(_execute_cpred) ? cp : p);
}

#ifdef _PL_STREAM_H
extern int Yap_getInputStream(Int t, IOSTREAM **s);
extern int Yap_getOutputStream(Int t, IOSTREAM **s);
#endif

