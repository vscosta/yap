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
* version:      $Id: Yapproto.h,v 1.12 2002-02-22 06:12:18 vsc Exp $	 *
*************************************************************************/

/* prototype file for Yap */

#define STD_PROTO(F,A)  F A
#define STATIC_PROTO(F,A) static F A


typedef Int (*CPredicate)(void);

typedef Int (*CmpPredicate)(Term, Term);


/* absmi.c */
Int	     STD_PROTO(absmi,(int));


/* adtdefs.c */
Term	STD_PROTO(ArrayToList,(Term *,int));
int	STD_PROTO(GetName,(char *,UInt,Term));
Term	STD_PROTO(GetValue,(Atom));
Atom	STD_PROTO(LookupAtom,(char *));
Atom	STD_PROTO(FullLookupAtom,(char *));
void	STD_PROTO(LookupAtomWithAddress,(char *,AtomEntry *));
Term	STD_PROTO(MkApplTerm,(Functor,unsigned int,Term *));
Prop	STD_PROTO(NewPredPropByFunctor,(struct FunctorEntryStruct *, SMALLUNSGN));
Prop	STD_PROTO(NewPredPropByAtom,(struct AtomEntryStruct *, SMALLUNSGN));
Functor	STD_PROTO(UnlockedMkFunctor,(AtomEntry *,unsigned int));
Functor	STD_PROTO(MkFunctor,(Atom,unsigned int));
void	STD_PROTO(MkFunctorWithAddress,(Atom,unsigned int,FunctorEntry *));
Term	STD_PROTO(MkPairTerm,(Term,Term));
void	STD_PROTO(PutValue,(Atom,Term));
void	STD_PROTO(ReleaseAtom,(Atom));
Term	STD_PROTO(StringToList,(char *));
Term	STD_PROTO(StringToListOfAtoms,(char *));

#if SFUNC
Term	STD_PROTO(MkSFTerm,(Functor,int,Term *,Term));
CELL	STD_PROTO(*ArgsOfSFTerm,(Term));
#endif

SMALLUNSGN	STD_PROTO(LookupModule,(Term));
Prop	STD_PROTO(GetPredPropByAtom,(Atom, SMALLUNSGN));
Prop	STD_PROTO(GetPredPropByFunc,(Functor, SMALLUNSGN));
Prop	STD_PROTO(GetPredPropHavingLock,(Atom,unsigned int,SMALLUNSGN));
Prop	STD_PROTO(GetExpProp,(Atom,unsigned int));
Prop	STD_PROTO(GetExpPropHavingLock,(AtomEntry *,unsigned int));
Term    STD_PROTO(Module_Name, (CODEADDR));


/* alloc.c */
int	STD_PROTO(SizeOfBlock,(CODEADDR));
void	STD_PROTO(FreeCodeSpace,(char *));
ADDR    STD_PROTO(PreAllocCodeSpace, (void));
char   *STD_PROTO(AllocAtomSpace,(unsigned int));
char	STD_PROTO(*AllocScannerMemory,(unsigned int));
char	STD_PROTO(*AllocCodeSpace,(unsigned int));
ADDR	STD_PROTO(AllocFromForeignArea,(Int));
int     STD_PROTO(ExtendWorkSpace,(Int));
void	STD_PROTO(FreeAtomSpace,(char *));
int     STD_PROTO(FreeWorkSpace, (void));
void	STD_PROTO(InitMemory,(int,int,int));
MALLOC_T STD_PROTO(InitWorkSpace, (Int));

/* amasm.c */
OPCODE	STD_PROTO(opcode,(op_numbers));
CODEADDR	STD_PROTO(assemble,(int));

/* analyst.c */
#ifdef ANALYST
void   STD_PROTO(InitAnalystPreds,(void));
#endif /* ANALYST */

/* arrays.c */
void   STD_PROTO(InitArrayPreds,(void));
CELL  *STD_PROTO(ClearNamedArray,(CELL *));

/* attvar.c */
Term	STD_PROTO(CurrentAttVars,(void));
void   STD_PROTO(InitAttVarPreds,(void));

/* bb.c */
void   STD_PROTO(InitBBPreds,(void));

/* bignum.c */
void   STD_PROTO(InitBigNums,(void));

/* c_interface.c */
Int    STD_PROTO(YapExecute,(CPredicate));

/* cdmgr.c */
void	STD_PROTO(mark_as_fast,(Term));
void	STD_PROTO(IPred,(CODEADDR sp));
Int	STD_PROTO(PredForCode,(CODEADDR, Atom *, Int *, SMALLUNSGN *));
void	STD_PROTO(InitCdMgr,(void));
#if     EMACS
int     STD_PROTO(where_new_clause, (Prop, int));
#endif
void	STD_PROTO(init_consult,(int, char *));
void	STD_PROTO(end_consult,(void));


/* cmppreds.c */
int	STD_PROTO(compare_terms,(Term,Term));
int	STD_PROTO(iequ,(Term,Term));
void	STD_PROTO(InitCmpPreds,(void));

/* compiler.c */
CODEADDR	STD_PROTO(cclause,(Term, int, int));

/* computils.c */

/* corout.c */
void	STD_PROTO(InitCoroutPreds,(void));
#ifdef COROUTINING
Term	STD_PROTO(ListOfWokenGoals,(void));
void	STD_PROTO(WakeUp,(CELL *));
#endif

/* dbase.c */
int     STD_PROTO(DBTrailOverflow,(void));
CELL	STD_PROTO(EvalMasks,(Term,CELL *));
void	STD_PROTO(InitBackDB,(void));
void	STD_PROTO(InitDBPreds,(void));

/* errors.c */
void	STD_PROTO(exit_yap,(int));
yamop  *STD_PROTO(Error,(yap_error_number,Term,char *msg, ...));


/* eval.c */
void	STD_PROTO(InitEval,(void));
Int	STD_PROTO(EvFArt,(Term));

/* exec.c */
Term	STD_PROTO(ExecuteCallMetaCall,(SMALLUNSGN mod));
void	STD_PROTO(InitExecFs,(void));
Int	STD_PROTO(JumpToEnv,(Term));
int	STD_PROTO(RunTopGoal,(Term));
Int	STD_PROTO(execute_goal,(Term, int, SMALLUNSGN));
int	STD_PROTO(exec_absmi,(int));


/* grow.c */
Int     STD_PROTO(total_stack_shift_time,(void));
void    STD_PROTO(InitGrowPreds, (void));
int     STD_PROTO(growheap,      (int));
int     STD_PROTO(growstack,     (long));
int     STD_PROTO(growtrail,     (long));
int     STD_PROTO(growglobal,    (void));

/* heapgc.c */
Int  STD_PROTO(total_gc_time,(void));
void STD_PROTO(init_gc,(void));
int  STD_PROTO(is_gc_verbose, (void));
int  STD_PROTO(gc, (Int, CELL *, yamop *));



/* init.c */
#ifdef DEBUG
int	STD_PROTO(DebugPutc,(int,int));
void	STD_PROTO(DebugSetIFile,(char *));
void	STD_PROTO(DebugEndline,(void));
int	STD_PROTO(DebugGetc,(void));
#endif
int	STD_PROTO(IsOpType,(char *));
void	STD_PROTO(InitStacks,(int,int,int,int,int,int));
void	STD_PROTO(InitCPred,(char *, int, CPredicate, int));
void	STD_PROTO(InitAsmPred,(char *, int, int, CPredicate, int));
void	STD_PROTO(InitCmpPred,(char *, int, CmpPredicate, CPredicate, int));
void	STD_PROTO(InitCPredBack,(char *, int, int, CPredicate,CPredicate,int));
void	STD_PROTO(InitYaamRegs,(void));
void    STD_PROTO(ReInitWallTime, (void));
int	STD_PROTO(OpDec,(int,char *,Atom));
void	STD_PROTO(UserCPredicate,(char *,CPredicate,unsigned int));
void	STD_PROTO(UserBackCPredicate,(char*,CPredicate,CPredicate,unsigned int,int));

/* iopreds.c */
void	STD_PROTO(CloseStreams,(int));
void	STD_PROTO(InitPlIO,(void));
void	STD_PROTO(InitBackIO,(void));
void	STD_PROTO(InitIOPreds,(void));
Atom	STD_PROTO(YapConsultingFile,(void));

/* depth_lim.c */
void	STD_PROTO(InitItDeepenPreds,(void));

/* load_foreign.c */
void	STD_PROTO(InitLoadForeign,(void));

/* mavar.c */
void	STD_PROTO(InitMaVarCPreds,(void));
Term    STD_PROTO(NewTimedVar,(Term));
Term    STD_PROTO(NewEmptyTimedVar,(void));
Term	STD_PROTO(ReadTimedVar,(Term));
Term    STD_PROTO(UpdateTimedVar,(Term, Term));

#if HAVE_MPI
/* mpi.c */
void    STD_PROTO(InitMPI,(void));
#endif

/* parser.c */
int	STD_PROTO(IsPrefixOp,(Prop,int *,int *));
int	STD_PROTO(IsInfixOp,(Prop,int *,int *,int *));
int	STD_PROTO(IsPosfixOp,(Prop,int *,int *));
Term	STD_PROTO(Parse,(void));

/* save.c */
int	STD_PROTO(SavedInfo,(char *,int *,int *,int *,char *));
int 	STD_PROTO(Restore,(char *));
void	STD_PROTO(InitSavePreds,(void));

/* scanner.c */

/* sort.c */
void    STD_PROTO(InitSortPreds,(void));

/* stdpreds.c */
#ifdef undefined
CELL	STD_PROTO(FindWhatCreep,(CELL));
#endif /* undefined */
void	STD_PROTO(InitBackCPreds,(void));
void	STD_PROTO(InitCPreds,(void));
Int	STD_PROTO(p_creep,(void));

/* sysbits.c */
void	STD_PROTO(set_fpu_exceptions,(int));
Int	STD_PROTO(cputime,(void));
Int	STD_PROTO(runtime,(void));
Int	STD_PROTO(walltime,(void));
int	STD_PROTO(dir_separator,(int));
int	STD_PROTO(volume_header,(char *));
void	STD_PROTO(InitSysPath,(void));
void	STD_PROTO(SetTextFile,(char *));
void    STD_PROTO(cputime_interval,(Int *,Int *));
void    STD_PROTO(walltime_interval,(Int *,Int *));
void	STD_PROTO(InitSysbits,(void));
void	STD_PROTO(InitSysPreds,(void));
int     STD_PROTO(TrueFileName, (char *, char *, int));
int	STD_PROTO(ProcessSIGINT,(void));
double  STD_PROTO(yap_random, (void));
void    STD_PROTO(set_fpu_exceptions, (int));

/* tracer.c */
#ifdef LOW_LEVEL_TRACER
void	STD_PROTO(InitLowLevelTrace,(void));
#endif

/* unify.c */
void         STD_PROTO(InitAbsmi,(void));
void         STD_PROTO(InitUnify,(void));
int          STD_PROTO(IUnify,(register CELL d0,register CELL d1));
EXTERN Term  STD_PROTO(Deref,(Term));
EXTERN Term  STD_PROTO(Derefa,(CELL *));
EXTERN Int   STD_PROTO(unify,(Term, Term));
EXTERN Int   STD_PROTO(unify_constant,(Term,Term));
op_numbers   STD_PROTO(op_from_opcode,(OPCODE));

/* userpreds.c */
void	STD_PROTO(InitUserCPreds,(void));
void	STD_PROTO(InitUserBacks,(void));

/* utilpreds.c */
Term	STD_PROTO(CopyTerm,(Term));
Int     STD_PROTO(var_in_term, (Term, Term));
void	STD_PROTO(InitUtilCPreds,(void));

/* yap.c */

void	STD_PROTO(addclause,(Term,CODEADDR,int,int));

/* ypsocks.c */
void	STD_PROTO(InitSockets,(void));
#ifdef USE_SOCKET
void	STD_PROTO(init_socks,(char *, long));
#endif

/* opt.preds.c */
void    STD_PROTO(init_optyap_preds,(void));


