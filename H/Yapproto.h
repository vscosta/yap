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
* version:      $Id: Yapproto.h,v 1.28 2002-11-11 17:37:58 vsc Exp $	 *
*************************************************************************/

/* prototype file for Yap */

#define STD_PROTO(F,A)  F A
#define STATIC_PROTO(F,A) static F A


typedef Int (*CPredicate)(void);

typedef Int (*CmpPredicate)(Term, Term);


/* absmi.c */
Int	     STD_PROTO(_YAP_absmi,(int));

/* adtdefs.c */
Term	STD_PROTO(_YAP_ArrayToList,(Term *,int));
int	STD_PROTO(_YAP_GetName,(char *,UInt,Term));
Term	STD_PROTO(_YAP_GetValue,(Atom));
Atom	STD_PROTO(_YAP_LookupAtom,(char *));
Atom	STD_PROTO(_YAP_FullLookupAtom,(char *));
void	STD_PROTO(_YAP_LookupAtomWithAddress,(char *,AtomEntry *));
Prop	STD_PROTO(_YAP_NewPredPropByFunctor,(struct FunctorEntryStruct *, SMALLUNSGN));
Prop	STD_PROTO(_YAP_NewPredPropByAtom,(struct AtomEntryStruct *, SMALLUNSGN));
Functor	STD_PROTO(_YAP_UnlockedMkFunctor,(AtomEntry *,unsigned int));
Functor	STD_PROTO(_YAP_MkFunctor,(Atom,unsigned int));
void	STD_PROTO(_YAP_MkFunctorWithAddress,(Atom,unsigned int,FunctorEntry *));
void	STD_PROTO(_YAP_PutValue,(Atom,Term));
void	STD_PROTO(_YAP_ReleaseAtom,(Atom));
Term	STD_PROTO(_YAP_StringToList,(char *));
Term	STD_PROTO(_YAP_StringToListOfAtoms,(char *));

long    STD_PROTO(_YAP_InitSlot,(Term));
long    STD_PROTO(_YAP_NewSlots,(int));
Term    STD_PROTO(_YAP_GetFromSlot,(long));
void    STD_PROTO(_YAP_RecoverSlots,(int));
Term    STD_PROTO(_YAP_GetFromSlot,(long));
Term   *STD_PROTO(_YAP_AddressFromSlot,(long));
void    STD_PROTO(_YAP_PutInSlot,(long, Term));


#if SFUNC
Term	STD_PROTO(MkSFTerm,(Functor,int,Term *,Term));
CELL	STD_PROTO(*ArgsOfSFTerm,(Term));
#endif

Prop	STD_PROTO(_YAP_GetPredPropByAtom,(Atom, SMALLUNSGN));
Prop	STD_PROTO(_YAP_GetPredPropByFunc,(Functor, SMALLUNSGN));
Prop	STD_PROTO(_YAP_GetPredPropHavingLock,(Atom,unsigned int,SMALLUNSGN));
Prop	STD_PROTO(_YAP_GetExpProp,(Atom,unsigned int));
Prop	STD_PROTO(_YAP_GetExpPropHavingLock,(AtomEntry *,unsigned int));

/* agc.c */
void    STD_PROTO(_YAP_atom_gc, (void));
void    STD_PROTO(_YAP_init_agc, (void));

/* alloc.c */
int	STD_PROTO(_YAP_SizeOfBlock,(CODEADDR));
void	STD_PROTO(_YAP_FreeCodeSpace,(char *));
char   *STD_PROTO(_YAP_AllocAtomSpace,(unsigned int));
char   *STD_PROTO(_YAP_AllocCodeSpace,(unsigned int));
ADDR	STD_PROTO(_YAP_AllocFromForeignArea,(Int));
int     STD_PROTO(_YAP_ExtendWorkSpace,(Int));
void	STD_PROTO(_YAP_FreeAtomSpace,(char *));
int     STD_PROTO(_YAP_FreeWorkSpace, (void));
void	STD_PROTO(_YAP_InitMemory,(int,int,int));

/* amasm.c */
OPCODE	STD_PROTO(_YAP_opcode,(op_numbers));

/* analyst.c */
#ifdef ANALYST
void   STD_PROTO(_YAP_InitAnalystPreds,(void));
#endif /* ANALYST */

/* arrays.c */
void   STD_PROTO(_YAP_InitArrayPreds,(void));

/* attvar.c */
Term   STD_PROTO(_YAP_CurrentAttVars,(void));
void   STD_PROTO(_YAP_InitAttVarPreds,(void));

/* bb.c */
void   STD_PROTO(_YAP_InitBBPreds,(void));

/* bignum.c */
void   STD_PROTO(_YAP_InitBigNums,(void));

/* c_interface.c */
Int    STD_PROTO(YAP_Execute,(struct pred_entry *, CPredicate));

/* cdmgr.c */
void	STD_PROTO(_YAP_addclause,(Term,CODEADDR,int,int));
Term	STD_PROTO(_YAP_all_calls,(void));
Atom	STD_PROTO(_YAP_ConsultingFile,(void));
Int	STD_PROTO(_YAP_PredForCode,(CODEADDR, Atom *, Int *, SMALLUNSGN *));
void	STD_PROTO(_YAP_InitCdMgr,(void));
#if     EMACS
int     STD_PROTO(where_new_clause, (Prop, int));
#endif
void	STD_PROTO(_YAP_init_consult,(int, char *));
void	STD_PROTO(_YAP_end_consult,(void));


/* cmppreds.c */
int	STD_PROTO(_YAP_compare_terms,(Term,Term));
void	STD_PROTO(_YAP_InitCmpPreds,(void));

/* compiler.c */
CODEADDR	STD_PROTO(_YAP_cclause,(Term, int, int));

/* computils.c */

/* corout.c */
void	STD_PROTO(_YAP_InitCoroutPreds,(void));
#ifdef COROUTINING
Term	STD_PROTO(_YAP_ListOfWokenGoals,(void));
void	STD_PROTO(_YAP_WakeUp,(CELL *));
void	STD_PROTO(_YAP_mark_all_suspended_goals,(void));
#endif

/* dbase.c */
int     STD_PROTO(_YAP_DBTrailOverflow,(void));
CELL	STD_PROTO(_YAP_EvalMasks,(Term,CELL *));
void	STD_PROTO(_YAP_InitBackDB,(void));
void	STD_PROTO(_YAP_InitDBPreds,(void));

/* errors.c */
void	STD_PROTO(_YAP_exit,(int));
yamop  *STD_PROTO(_YAP_Error,(yap_error_number,Term,char *msg, ...));

/* eval.c */
void	STD_PROTO(_YAP_InitEval,(void));

/* exec.c */
Term	STD_PROTO(_YAP_ExecuteCallMetaCall,(SMALLUNSGN mod));
void	STD_PROTO(_YAP_InitExecFs,(void));
Int	STD_PROTO(_YAP_JumpToEnv,(Term));
int	STD_PROTO(_YAP_RunTopGoal,(Term));
Int	STD_PROTO(_YAP_execute_goal,(Term, int, SMALLUNSGN));
int	STD_PROTO(_YAP_exec_absmi,(int));
void	STD_PROTO(_YAP_trust_last,(void));


/* grow.c */
Int     STD_PROTO(_YAP_total_stack_shift_time,(void));
void    STD_PROTO(_YAP_InitGrowPreds, (void));
int     STD_PROTO(_YAP_growheap,      (int));
int     STD_PROTO(_YAP_growstack,     (long));
int     STD_PROTO(_YAP_growtrail,     (long));
int     STD_PROTO(_YAP_growglobal,    (CELL **));

/* heapgc.c */
Int  STD_PROTO(_YAP_total_gc_time,(void));
void STD_PROTO(_YAP_init_gc,(void));
int  STD_PROTO(_YAP_is_gc_verbose, (void));
int  STD_PROTO(_YAP_gc, (Int, CELL *, yamop *));

/* init.c */
#ifdef DEBUG
int	STD_PROTO(_YAP_DebugPutc,(int,int));
void	STD_PROTO(_YAP_DebugSetIFile,(char *));
void	STD_PROTO(_YAP_DebugEndline,(void));
int	STD_PROTO(_YAP_DebugGetc,(void));
#endif
int	STD_PROTO(_YAP_IsOpType,(char *));
void	STD_PROTO(_YAP_InitStacks,(int,int,int,int,int,int));
void	STD_PROTO(_YAP_InitCPred,(char *, unsigned long int, CPredicate, int));
void	STD_PROTO(_YAP_InitAsmPred,(char *, unsigned long int, int, CPredicate, int));
void	STD_PROTO(_YAP_InitCmpPred,(char *, unsigned long int, CmpPredicate, CPredicate, int));
void	STD_PROTO(_YAP_InitCPredBack,(char *, unsigned long int, unsigned int, CPredicate,CPredicate,int));
void	STD_PROTO(_YAP_InitYaamRegs,(void));
void    STD_PROTO(_YAP_ReInitWallTime, (void));
int	STD_PROTO(_YAP_OpDec,(int,char *,Atom));

/* inlines.c */
void         STD_PROTO(_YAP_InitInlines,(void));

/* iopreds.c */
void	STD_PROTO(_YAP_InitPlIO,(void));
void	STD_PROTO(_YAP_InitBackIO,(void));
void	STD_PROTO(_YAP_InitIOPreds,(void));

/* depth_lim.c */
void	STD_PROTO(_YAP_InitItDeepenPreds,(void));

/* load_foreign.c */
void	STD_PROTO(_YAP_InitLoadForeign,(void));

/* mavar.c */
void	STD_PROTO(_YAP_InitMaVarCPreds,(void));
Term    STD_PROTO(_YAP_NewTimedVar,(Term));
Term    STD_PROTO(_YAP_NewEmptyTimedVar,(void));
Term	STD_PROTO(_YAP_ReadTimedVar,(Term));
Term    STD_PROTO(_YAP_UpdateTimedVar,(Term, Term));

/* modules.c */
SMALLUNSGN	STD_PROTO(_YAP_LookupModule,(Term));
Term    STD_PROTO(_YAP_Module_Name, (CODEADDR));
void    STD_PROTO(_YAP_InitModules, (void));

#if HAVE_MPI
/* mpi.c */
void    STD_PROTO(_YAP_InitMPI,(void));
#endif

#if HAVE_MPE
/* mpe.c */
void    STD_PROTO(_YAP_InitMPE,(void));
#endif


/* other.c */
Term	STD_PROTO(_YAP_MkApplTerm,(Functor,unsigned int,Term *));
Term	STD_PROTO(_YAP_MkNewApplTerm,(Functor,unsigned int));
Term	STD_PROTO(_YAP_MkNewPairTerm,(void));


/* parser.c */
int	STD_PROTO(_YAP_IsPrefixOp,(Prop,int *,int *));
int	STD_PROTO(_YAP_IsInfixOp,(Prop,int *,int *,int *));
int	STD_PROTO(_YAP_IsPosfixOp,(Prop,int *,int *));
Term	STD_PROTO(_YAP_Parse,(void));

/* save.c */
int	STD_PROTO(_YAP_SavedInfo,(char *,char *,CELL *,CELL *,CELL *));
int 	STD_PROTO(_YAP_Restore,(char *, char *));
void	STD_PROTO(_YAP_InitSavePreds,(void));

/* scanner.c */

/* sort.c */
void    STD_PROTO(_YAP_InitSortPreds,(void));

/* stdpreds.c */
void	STD_PROTO(_YAP_InitBackCPreds,(void));
void	STD_PROTO(_YAP_InitCPreds,(void));
void	STD_PROTO(_YAP_show_statistics,(void));
Int	STD_PROTO(_YAP_creep,(void));

/* sysbits.c */
void	STD_PROTO(_YAP_set_fpu_exceptions,(int));
Int	STD_PROTO(_YAP_cputime,(void));
Int	STD_PROTO(_YAP_walltime,(void));
int	STD_PROTO(_YAP_dir_separator,(int));
int	STD_PROTO(_YAP_volume_header,(char *));
void	STD_PROTO(_YAP_InitSysPath,(void));
#if MAC
void	STD_PROTO(_YAP_SetTextFile,(char *));
#endif
void    STD_PROTO(_YAP_cputime_interval,(Int *,Int *));
void    STD_PROTO(_YAP_walltime_interval,(Int *,Int *));
void	STD_PROTO(_YAP_InitSysbits,(void));
void	STD_PROTO(_YAP_InitSysPreds,(void));
int     STD_PROTO(_YAP_TrueFileName, (char *, char *, int));
int	STD_PROTO(_YAP_ProcessSIGINT,(void));
double  STD_PROTO(_YAP_random, (void));

/* tracer.c */
#ifdef LOW_LEVEL_TRACER
void	STD_PROTO(_YAP_InitLowLevelTrace,(void));
#endif

/* unify.c */
void         STD_PROTO(_YAP_InitAbsmi,(void));
void         STD_PROTO(_YAP_InitUnify,(void));
int          STD_PROTO(_YAP_IUnify,(register CELL d0,register CELL d1));
op_numbers   STD_PROTO(_YAP_op_from_opcode,(OPCODE));

/* userpreds.c */
void	STD_PROTO(_YAP_InitUserCPreds,(void));
void	STD_PROTO(_YAP_InitUserBacks,(void));

/* utilpreds.c */
Term	STD_PROTO(_YAP_CopyTerm,(Term));
void	STD_PROTO(_YAP_InitUtilCPreds,(void));

/* yap.c */

/* ypsocks.c */
void	STD_PROTO(_YAP_InitSockets,(void));
#ifdef USE_SOCKET
void	STD_PROTO(_YAP_init_socks,(char *, long));
#endif

/* opt.preds.c */
void    STD_PROTO(_YAP_init_optyap_preds,(void));


