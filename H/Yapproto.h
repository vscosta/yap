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
* version:      $Id: Yapproto.h,v 1.53 2004-07-22 21:32:21 vsc Exp $	 *
*************************************************************************/

/* prototype file for Yap */

#define STD_PROTO(F,A)  F A
#define STATIC_PROTO(F,A) static F A


/* absmi.c */
Int	     STD_PROTO(Yap_absmi,(int));

/* adtdefs.c */
Term	STD_PROTO(Yap_ArrayToList,(Term *,int));
int	STD_PROTO(Yap_GetName,(char *,UInt,Term));
Term	STD_PROTO(Yap_GetValue,(Atom));
Atom	STD_PROTO(Yap_LookupAtom,(char *));
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
Term	STD_PROTO(Yap_StringToListOfAtoms,(char *));

long    STD_PROTO(Yap_InitSlot,(Term));
long    STD_PROTO(Yap_NewSlots,(int));
Term    STD_PROTO(Yap_GetFromSlot,(long));
void    STD_PROTO(Yap_RecoverSlots,(int));
Term    STD_PROTO(Yap_GetFromSlot,(long));
Term   *STD_PROTO(Yap_AddressFromSlot,(long));
void    STD_PROTO(Yap_PutInSlot,(long, Term));


#if SFUNC
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
void    STD_PROTO(Yap_atom_gc, (void));
void    STD_PROTO(Yap_init_agc, (void));

/* alloc.c */
void	STD_PROTO(Yap_FreeCodeSpace,(char *));
char   *STD_PROTO(Yap_AllocAtomSpace,(unsigned int));
char   *STD_PROTO(Yap_AllocCodeSpace,(unsigned int));
ADDR	STD_PROTO(Yap_AllocFromForeignArea,(Int));
int     STD_PROTO(Yap_ExtendWorkSpace,(Int));
void	STD_PROTO(Yap_FreeAtomSpace,(char *));
int     STD_PROTO(Yap_FreeWorkSpace, (void));
void	STD_PROTO(Yap_InitMemory,(int,int,int));
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
Term   STD_PROTO(Yap_CurrentAttVars,(void));
void   STD_PROTO(Yap_InitAttVarPreds,(void));

/* bb.c */
void   STD_PROTO(Yap_InitBBPreds,(void));

/* bignum.c */
Term   STD_PROTO(Yap_MkULLIntTerm,(YAP_ULONG_LONG));
void   STD_PROTO(Yap_InitBigNums,(void));

/* c_interface.c */
Int    STD_PROTO(YAP_Execute,(struct pred_entry *, CPredicate));

/* cdmgr.c */
Term	STD_PROTO(Yap_all_calls,(void));
Atom	STD_PROTO(Yap_ConsultingFile,(void));
Int	STD_PROTO(Yap_PredForCode,(yamop *, Atom *, UInt *, Term *));
void	STD_PROTO(Yap_InitCdMgr,(void));
#if     EMACS
int     STD_PROTO(where_new_clause, (Prop, int));
#endif
void	STD_PROTO(Yap_init_consult,(int, char *));
void	STD_PROTO(Yap_end_consult,(void));
void	STD_PROTO(Yap_Abolish,(struct pred_entry *));


/* cmppreds.c */
int	STD_PROTO(Yap_compare_terms,(Term,Term));
void	STD_PROTO(Yap_InitCmpPreds,(void));

/* compiler.c */
yamop  *STD_PROTO(Yap_cclause,(Term, int, int, Term));

/* computils.c */

/* corout.c */
void	STD_PROTO(Yap_InitCoroutPreds,(void));
#ifdef COROUTINING
Term	STD_PROTO(Yap_ListOfWokenGoals,(void));
void	STD_PROTO(Yap_WakeUp,(CELL *));
#endif

/* dbase.c */
int     STD_PROTO(Yap_DBTrailOverflow,(void));
CELL	STD_PROTO(Yap_EvalMasks,(Term,CELL *));
void	STD_PROTO(Yap_InitBackDB,(void));
void	STD_PROTO(Yap_InitDBPreds,(void));

/* errors.c */
void	STD_PROTO(Yap_exit,(int));
yamop  *STD_PROTO(Yap_Error,(yap_error_number,Term,char *msg, ...));

/* eval.c */
void	STD_PROTO(Yap_InitEval,(void));

/* exec.c */
Term	STD_PROTO(Yap_ExecuteCallMetaCall,(Term));
void	STD_PROTO(Yap_InitExecFs,(void));
Int	STD_PROTO(Yap_JumpToEnv,(Term));
Term	STD_PROTO(Yap_RunTopGoal,(Term));
Int	STD_PROTO(Yap_execute_goal,(Term, int, Term));
int	STD_PROTO(Yap_exec_absmi,(int));
void	STD_PROTO(Yap_trust_last,(void));


/* grow.c */
Int     STD_PROTO(Yap_total_stack_shift_time,(void));
void    STD_PROTO(Yap_InitGrowPreds, (void));
int     STD_PROTO(Yap_growheap,      (int, UInt, void *));
int     STD_PROTO(Yap_growstack,     (long));
int     STD_PROTO(Yap_growtrail,     (long));
int     STD_PROTO(Yap_growglobal,    (CELL **));
CELL  **STD_PROTO(Yap_shift_visit,   (CELL **, CELL ***));

/* heapgc.c */
Int  STD_PROTO(Yap_total_gc_time,(void));
void STD_PROTO(Yap_init_gc,(void));
int  STD_PROTO(Yap_is_gc_verbose, (void));
int  STD_PROTO(Yap_gc, (Int, CELL *, yamop *));
int  STD_PROTO(Yap_gcl, (UInt, Int, CELL *, yamop *));

/* init.c */
#ifdef DEBUG
int	STD_PROTO(Yap_DebugPutc,(int,int));
void	STD_PROTO(Yap_DebugSetIFile,(char *));
void	STD_PROTO(Yap_DebugEndline,(void));
int	STD_PROTO(Yap_DebugGetc,(void));
#endif
int	STD_PROTO(Yap_IsOpType,(char *));
void	STD_PROTO(Yap_InitCPred,(char *, unsigned long int, CPredicate, int));
void	STD_PROTO(Yap_InitAsmPred,(char *, unsigned long int, int, CPredicate, int));
void	STD_PROTO(Yap_InitCmpPred,(char *, unsigned long int, CmpPredicate, int));
void	STD_PROTO(Yap_InitCPredBack,(char *, unsigned long int, unsigned int, CPredicate,CPredicate,int));
void	STD_PROTO(Yap_InitWorkspace,(int,int,int,int,int,int));

#if defined(YAPOR) || defined(THREADS)
void	STD_PROTO(Yap_KillStacks,(int));
#else
void	STD_PROTO(Yap_KillStacks,(void));
#endif
void	STD_PROTO(Yap_InitYaamRegs,(void));
void    STD_PROTO(Yap_ReInitWallTime, (void));
int	STD_PROTO(Yap_OpDec,(int,char *,Atom));

/* inlines.c */
void         STD_PROTO(Yap_InitInlines,(void));

/* iopreds.c */
void	STD_PROTO(Yap_InitPlIO,(void));
void	STD_PROTO(Yap_InitBackIO,(void));
void	STD_PROTO(Yap_InitIOPreds,(void));

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
Term    STD_PROTO(Yap_Module_Name, (struct pred_entry *));
struct pred_entry *STD_PROTO(Yap_ModulePred, (Term));
void    STD_PROTO(Yap_NewModulePred, (Term, struct pred_entry *));
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


/* parser.c */
int	STD_PROTO(Yap_IsPrefixOp,(Prop,int *,int *));
int	STD_PROTO(Yap_IsInfixOp,(Prop,int *,int *,int *));
int	STD_PROTO(Yap_IsPosfixOp,(Prop,int *,int *));
Term	STD_PROTO(Yap_Parse,(void));

/* save.c */
int	STD_PROTO(Yap_SavedInfo,(char *,char *,CELL *,CELL *,CELL *));
int 	STD_PROTO(Yap_Restore,(char *, char *));
void	STD_PROTO(Yap_InitSavePreds,(void));

/* scanner.c */

/* sort.c */
void    STD_PROTO(Yap_InitSortPreds,(void));

/* stdpreds.c */
void	STD_PROTO(Yap_InitBackCPreds,(void));
void	STD_PROTO(Yap_InitCPreds,(void));
void	STD_PROTO(Yap_show_statistics,(void));
void	STD_PROTO(Yap_signal,(yap_signals));

/* sysbits.c */
void	STD_PROTO(Yap_set_fpu_exceptions,(int));
UInt	STD_PROTO(Yap_cputime,(void));
Int	STD_PROTO(Yap_walltime,(void));
int	STD_PROTO(Yap_dir_separator,(int));
int	STD_PROTO(Yap_volume_header,(char *));
void	STD_PROTO(Yap_InitSysPath,(void));
#if MAC
void	STD_PROTO(Yap_SetTextFile,(char *));
#endif
void    STD_PROTO(Yap_cputime_interval,(Int *,Int *));
void    STD_PROTO(Yap_walltime_interval,(Int *,Int *));
void	STD_PROTO(Yap_InitSysbits,(void));
void	STD_PROTO(Yap_InitSysPreds,(void));
void	STD_PROTO(Yap_InitTime,(void));
int     STD_PROTO(Yap_TrueFileName, (char *, char *, int));
int	STD_PROTO(Yap_ProcessSIGINT,(void));
double  STD_PROTO(Yap_random, (void));

/* threads.c */
void   STD_PROTO(Yap_InitThreadPreds,(void));

/* tracer.c */
#ifdef LOW_LEVEL_TRACER
void	STD_PROTO(Yap_InitLowLevelTrace,(void));
#endif

/* unify.c */
void         STD_PROTO(Yap_InitAbsmi,(void));
void         STD_PROTO(Yap_InitUnify,(void));
int          STD_PROTO(Yap_IUnify,(register CELL d0,register CELL d1));

/* userpreds.c */
void	STD_PROTO(Yap_InitUserCPreds,(void));
void	STD_PROTO(Yap_InitUserBacks,(void));

/* utilpreds.c */
Term	STD_PROTO(Yap_CopyTerm,(Term));
void	STD_PROTO(Yap_InitUtilCPreds,(void));

/* yap.c */

/* ypsocks.c */
void	STD_PROTO(Yap_InitSockets,(void));
#ifdef USE_SOCKET
void	STD_PROTO(Yap_init_socks,(char *, long));
#endif

/* opt.preds.c */
void    STD_PROTO(Yap_init_optyap_preds,(void));


#if LOW_PROF
void STD_PROTO(Yap_dump_code_area_for_profiler,(void));
void STD_PROTO(Yap_inform_profiler_of_clause,(yamop *,yamop *, struct pred_entry *,int index_code));
#endif /* LOW_PROF */
