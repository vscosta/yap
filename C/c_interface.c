/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%						 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		c_interface.c						 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	c_interface primitives definition 			 *
*									 *
*************************************************************************/

#define Bool int
#define flt double
#define C_INTERFACE

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#define HAS_YAP_H 1
#include "yap_structs.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */

#define YAP_BOOT_FROM_PROLOG       0
#define YAP_BOOT_FROM_SAVED_CODE   1
#define YAP_BOOT_FROM_SAVED_STACKS 2
#define YAP_BOOT_FROM_SAVED_ERROR  -1

#if defined(_MSC_VER) && defined(YAP_EXPORTS)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif

X_API Term    STD_PROTO(YapA,(int));
X_API Term    STD_PROTO(YapMkVarTerm,(void));
X_API Bool    STD_PROTO(YapIsVarTerm,(Term));
X_API Bool    STD_PROTO(YapIsNonVarTerm,(Term));
X_API Bool    STD_PROTO(YapIsIntTerm,(Term));
X_API Bool    STD_PROTO(YapIsFloatTerm,(Term));
X_API Bool    STD_PROTO(YapIsDbRefTerm,(Term));
X_API Bool    STD_PROTO(YapIsAtomTerm,(Term));
X_API Bool    STD_PROTO(YapIsPairTerm,(Term));
X_API Bool    STD_PROTO(YapIsApplTerm,(Term));
X_API Term    STD_PROTO(YapMkIntTerm,(Int));
X_API Int     STD_PROTO(YapIntOfTerm,(Term));
X_API Term    STD_PROTO(YapMkFloatTerm,(flt));
X_API flt     STD_PROTO(YapFloatOfTerm,(Term));
X_API Term    STD_PROTO(YapMkAtomTerm,(Atom));
X_API Atom    STD_PROTO(YapAtomOfTerm,(Term));
X_API Atom    STD_PROTO(YapLookupAtom,(char *));
X_API Atom    STD_PROTO(YapFullLookupAtom,(char *));
X_API char   *STD_PROTO(YapAtomName,(Atom));
X_API Term    STD_PROTO(YapMkPairTerm,(Term,Term));
X_API Term    STD_PROTO(YapMkNewPairTerm,(void));
X_API Term    STD_PROTO(YapHeadOfTerm,(Term));
X_API Term    STD_PROTO(YapTailOfTerm,(Term));
X_API Term    STD_PROTO(YapMkApplTerm,(Functor,unsigned int,Term *));
X_API Term    STD_PROTO(YapMkNewApplTerm,(Functor,unsigned int));
X_API Functor STD_PROTO(YapFunctorOfTerm,(Term));
X_API Term    STD_PROTO(YapArgOfTerm,(Int,Term));
X_API Functor STD_PROTO(YapMkFunctor,(Atom,Int));
X_API Atom    STD_PROTO(YapNameOfFunctor,(Functor));
X_API Int     STD_PROTO(YapArityOfFunctor,(Functor));
X_API void   *STD_PROTO(YapExtraSpace,(void));
X_API Int     STD_PROTO(Yapcut_fail,(void));
X_API Int     STD_PROTO(Yapcut_succeed,(void));
X_API Int     STD_PROTO(YapUnify,(Term,Term));
X_API Int     STD_PROTO(YapUnify,(Term,Term));
X_API int     STD_PROTO(YapReset,(void));
X_API Int     STD_PROTO(YapInit,(yap_init_args *));
X_API Int     STD_PROTO(YapFastInit,(char *));
X_API Int     STD_PROTO(YapCallProlog,(Term));
X_API void   *STD_PROTO(YapAllocSpaceFromYap,(unsigned int));
X_API void    STD_PROTO(YapFreeSpaceFromYap,(void *));
X_API void    STD_PROTO(YapFreeSpaceFromYap,(void *));
X_API int     STD_PROTO(YapStringToBuffer, (Term, char *, unsigned int));
X_API Term    STD_PROTO(YapBufferToString, (char *));
X_API Term    STD_PROTO(YapBufferToAtomList, (char *));
X_API void    STD_PROTO(YapError,(char *));
X_API int     STD_PROTO(YapRunGoal,(Term));
X_API int     STD_PROTO(YapRestartGoal,(void));
X_API int     STD_PROTO(YapGoalHasException,(Term *));
X_API int     STD_PROTO(YapContinueGoal,(void));
X_API void    STD_PROTO(YapPruneGoal,(void));
X_API void    STD_PROTO(YapInitConsult,(int, char *));
X_API void    STD_PROTO(YapEndConsult,(void));
X_API Term    STD_PROTO(YapRead, (int (*)(void)));
X_API void    STD_PROTO(YapWrite, (Term, void (*)(int), int));
X_API char   *STD_PROTO(YapCompileClause, (Term));
X_API void    STD_PROTO(YapPutValue, (Atom,Term));
X_API Term    STD_PROTO(YapGetValue, (Atom));
X_API int     STD_PROTO(YapReset, (void));
X_API void    STD_PROTO(YapExit, (int));
X_API void    STD_PROTO(YapInitSocks, (char *, long));
X_API void    STD_PROTO(YapSetOutputMessage, (void));
X_API int     STD_PROTO(YapStreamToFileNo, (Term));
X_API void    STD_PROTO(YapCloseAllOpenStreams,(void));
X_API Term    STD_PROTO(YapOpenStream,(void *, char *, Term, int));
X_API long    STD_PROTO(YapNewSlots,(int));
X_API long    STD_PROTO(YapInitSlot,(Term));
X_API Term    STD_PROTO(YapGetFromSlot,(long));
X_API Term   *STD_PROTO(YapAddressFromSlot,(long));
X_API void    STD_PROTO(YapPutInSlot,(long, Term));
X_API void    STD_PROTO(YapRecoverSlots,(int));
X_API void    STD_PROTO(YapThrow,(Term));
X_API int     STD_PROTO(YapLookupModule,(Term));
X_API Term    STD_PROTO(YapModuleName,(int));
X_API void    STD_PROTO(YapHalt,(int));
X_API Term   *STD_PROTO(YapTopOfLocalStack,(void));
X_API void   *STD_PROTO(YapPredicate,(Atom,Int,Int));
X_API void    STD_PROTO(YapPredicateInfo,(void *,Atom *,Int *,Int *));
X_API void    STD_PROTO(YapUserCPredicateWithArgs,(char *,CPredicate,Int,Int));
X_API Int     STD_PROTO(YapCurrentModule,(void));

static int (*do_getf)(void);

static int do_yap_getc(int streamno) {
  return(do_getf());
}

static void (*do_putcf)(int);

static int do_yap_putc(int streamno,int ch) {
  do_putcf(ch);
  return(ch);
}

X_API Term
YapA(int i)
{

  return(Deref(XREGS[i]));
}

X_API Bool 
YapIsIntTerm(Term t)
{
  return (IsIntegerTerm(t));
}

X_API Bool 
YapIsVarTerm(Term t)
{
  return (IsVarTerm(t));
}

X_API Bool 
YapIsNonVarTerm(Term t)
{
  return (IsNonVarTerm(t));
}

X_API Bool 
YapIsFloatTerm(Term t)
{
  return (IsFloatTerm(t));
}

X_API Bool 
YapIsDbRefTerm(Term t)
{
  return (IsDBRefTerm(t));
}

X_API Bool 
YapIsAtomTerm(Term t)
{
  return (IsAtomTerm(t));
}

X_API Bool 
YapIsPairTerm(Term t)
{
  return (IsPairTerm(t));
}

X_API Bool 
YapIsApplTerm(Term t)
{
  return (IsApplTerm(t) && !IsExtensionFunctor(FunctorOfTerm(t)));
}


X_API Term 
YapMkIntTerm(Int n)
{
  Term I;
  BACKUP_H();

  I = MkIntegerTerm(n);
  RECOVER_H();
  return(I);
}

X_API Int 
YapIntOfTerm(Term t)
{
  if (!IsApplTerm(t))
    return (IntOfTerm(t));
  else
    return(LongIntOfTerm(t));
}

X_API Term 
YapMkFloatTerm(double n)
{
  Term t;
  BACKUP_H();

  t = MkFloatTerm(n);

  RECOVER_H();
  return(t);
}

X_API flt 
YapFloatOfTerm(Term t)
{
  return (FloatOfTerm(t));
}

X_API Term 
YapMkAtomTerm(Atom n)
{
  Term t;

  t = MkAtomTerm(n);
  return(t);
}

X_API Atom 
YapAtomOfTerm(Term t)
{
  return (AtomOfTerm(t));
}


X_API char           *
YapAtomName(Atom a)
{
  char *o;

  o = AtomName(a);
  return(o);
}

X_API Atom
YapLookupAtom(char *c)
{
  return(LookupAtom(c));
}

X_API Atom
YapFullLookupAtom(char *c)
{
  Atom at;

  at = FullLookupAtom(c);
  return(at);
}

X_API Term
YapMkVarTerm(void)
{
  CELL t; 
  BACKUP_H();

  t = MkVarTerm();

  RECOVER_H();
  return(t);
}

X_API Term
YapMkPairTerm(Term t1, Term t2)
{
  Term t; 
  BACKUP_H();

  t = MkPairTerm(t1, t2);
  
  RECOVER_H();
  return(t);
}

X_API Term
YapMkNewPairTerm()
{
  Term t; 
  BACKUP_H();

  t = MkNewPairTerm();
  
  RECOVER_H();
  return(t);
}

X_API Term 
YapHeadOfTerm(Term t)
{
  return (HeadOfTerm(t));
}

X_API Term 
YapTailOfTerm(Term t)
{
  return (TailOfTerm(t));
}

X_API Term
YapMkApplTerm(Functor f,unsigned int arity, Term args[])
{
  Term t; 
  BACKUP_H();

  t = MkApplTerm(f, arity, args);

  RECOVER_H();
  return(t);
}

X_API Term
YapMkNewApplTerm(Functor f,unsigned int arity)
{
  Term t; 
  BACKUP_H();

  t = MkNewApplTerm(f, arity);

  RECOVER_H();
  return(t);
}

X_API Functor 
YapFunctorOfTerm(Term t)
{
  return (FunctorOfTerm(t));
}


X_API Term 
YapArgOfTerm(Int n, Term t)
{
  return (ArgOfTerm(n, t));
}



X_API Functor 
YapMkFunctor(Atom a, Int n)
{
  return (MkFunctor(a, n));
 }

X_API Atom 
YapNameOfFunctor(Functor f)
{
  return (NameOfFunctor(f));
}

X_API Int 
YapArityOfFunctor(Functor f)
{
  return (ArityOfFunctor(f));
}

X_API void *
YapExtraSpace(void)
{
  void *ptr;
  BACKUP_B();

  /* find a pointer to extra space allocable */
  ptr = (void *)((CELL *)(B+1)+P->u.lds.s);

  RECOVER_B();
  return(ptr);
}

X_API Int
Yapcut_fail(void)
{
  BACKUP_B();

  B = B->cp_b;  /* cut_fail */
  HB = B->cp_h; /* cut_fail */

  RECOVER_B();
  return(FALSE);
}

X_API Int
Yapcut_succeed(void)
{
  BACKUP_B();

  B = B->cp_b;
  HB = B->cp_h;

  RECOVER_B();
  return(TRUE);
}

X_API Int
YapUnify(Term t1, Term t2)
{
  Int out;
  BACKUP_MACHINE_REGS();

  out = unify(t1, t2);

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API long
YapNewSlots(int n)
{
  Int old_slots = IntOfTerm(ASP[0]), oldn = n;
  while (n > 0) {
    RESET_VARIABLE(ASP);
    ASP--;
    n--;
  }
  ASP[0] = MkIntTerm(old_slots+oldn);
  return((ASP+1)-LCL0);
}

X_API long
YapInitSlot(Term t)
{
  Int old_slots = IntOfTerm(ASP[0]);
  *ASP = t;
  ASP--;
  ASP[0] = MkIntTerm(old_slots+1);
  return((ASP+1)-LCL0);
}

X_API void
YapRecoverSlots(int n)
{
  Int old_slots = IntOfTerm(ASP[0]);
  ASP += n;
  ASP[0] = MkIntTerm(old_slots-n);
}

X_API Term
YapGetFromSlot(long slot)
{
  return(Deref(LCL0[slot]));
}

X_API Term *
YapAddressFromSlot(long slot)
{
  return(LCL0+slot);
}

X_API void
YapPutInSlot(long slot, Term t)
{
  LCL0[slot] = t;
}


typedef Int (*CPredicate1)(long);
typedef Int (*CPredicate2)(long,long);
typedef Int (*CPredicate3)(long,long,long);
typedef Int (*CPredicate4)(long,long,long,long);
typedef Int (*CPredicate5)(long,long,long,long,long);
typedef Int (*CPredicate6)(long,long,long,long,long,long);
typedef Int (*CPredicate7)(long,long,long,long,long,long,long);
typedef Int (*CPredicate8)(long,long,long,long,long,long,long,long);

Int
YapExecute(PredEntry *pe, CPredicate exec_code)
{
  if (pe->PredFlags & CArgsPredFlag) {
    switch (pe->ArityOfPE) {
    case 0:
      {
	CPredicate code0 = exec_code;
	return ((code0)());
      }
    case 1:
      {
	CPredicate1 code1 = (CPredicate1)exec_code;
	return ((code1)(YapInitSlot(Deref(ARG1))));
      }
    case 2:
      {
	CPredicate2 code2 = (CPredicate2)exec_code;
	return ((code2)(YapInitSlot(Deref(ARG1)),
			YapInitSlot(Deref(ARG2))));
      }
    case 3:
      {
	CPredicate3 code3 = (CPredicate3)exec_code;
	return ((code3)(YapInitSlot(Deref(ARG1)),
			YapInitSlot(Deref(ARG2)),
			YapInitSlot(Deref(ARG3))));
      }
    case 4:
      {
	CPredicate4 code4 = (CPredicate4)exec_code;
	return ((code4)(YapInitSlot(Deref(ARG1)),
			YapInitSlot(Deref(ARG2)),
			YapInitSlot(Deref(ARG3)),
			YapInitSlot(Deref(ARG4))));
      }
    case 5:
      {
	CPredicate5 code5 = (CPredicate5)exec_code;
	return ((code5)(YapInitSlot(Deref(ARG1)),
			YapInitSlot(Deref(ARG2)),
			YapInitSlot(Deref(ARG3)),
			YapInitSlot(Deref(ARG4)),YapInitSlot(Deref(ARG5))));
      }
    case 6:
      {
	CPredicate6 code6 = (CPredicate6)exec_code;
	return ((code6)(YapInitSlot(Deref(ARG1)),
			YapInitSlot(Deref(ARG2)),
			YapInitSlot(Deref(ARG3)),
			YapInitSlot(Deref(ARG4)),
			YapInitSlot(Deref(ARG5)),
			YapInitSlot(Deref(ARG6))));
      }
    case 7:
      {
	CPredicate7 code7 = (CPredicate7)exec_code;
	return ((code7)(YapInitSlot(Deref(ARG1)),
			YapInitSlot(Deref(ARG2)),
			YapInitSlot(Deref(ARG3)),
			YapInitSlot(Deref(ARG4)),
			YapInitSlot(Deref(ARG5)),
			YapInitSlot(Deref(ARG6)),
			YapInitSlot(Deref(ARG7))));
      }
    case 8:
      {
	CPredicate8 code8 = (CPredicate8)exec_code;
	return ((code8)(YapInitSlot(Deref(ARG1)),
			YapInitSlot(Deref(ARG2)),
			YapInitSlot(Deref(ARG3)),
			YapInitSlot(Deref(ARG4)),
			YapInitSlot(Deref(ARG5)),
			YapInitSlot(Deref(ARG6)),
			YapInitSlot(Deref(ARG7)),
			YapInitSlot(Deref(ARG8))));
      }
    default:
      return(FALSE);
    }
  } else {
    return((exec_code)());
  }
}

X_API Int
YapCallProlog(Term t)
{
  Int out;
  SMALLUNSGN mod = CurrentModule;
  BACKUP_MACHINE_REGS();

  while (!IsVarTerm(t) &&
      IsApplTerm(t) &&
      FunctorOfTerm(t) == FunctorModule) {
    Term tmod = ArgOfTerm(1,t);
    if (IsVarTerm(tmod)) return(FALSE);
    if (!IsAtomTerm(tmod)) return(FALSE);
    mod = LookupModule(tmod);
    t = ArgOfTerm(2,t);
  }
  out = execute_goal(t, 0, mod);
  RECOVER_MACHINE_REGS();
  return(out);
}

X_API void *
YapAllocSpaceFromYap(unsigned int size)
{
  void *ptr;
  BACKUP_MACHINE_REGS();

  if ((ptr = AllocCodeSpace(size)) == NULL) {
    if (!growheap(FALSE)) {
      Error(SYSTEM_ERROR, TermNil, "YAP failed to reserve space in growheap");
      return(NULL);
    }
  }

  RECOVER_MACHINE_REGS();
  return(ptr);
}

X_API void
YapFreeSpaceFromYap(void *ptr)
{
  FreeCodeSpace(ptr);
}

/* copy a string to a buffer */
X_API int
YapStringToBuffer(Term t, char *buf, unsigned int bufsize)
{
  unsigned int j = 0;

  while (t != TermNil) {
    register Term   Head;
    register Int    i;

    Head = HeadOfTerm(t);
    if (IsVarTerm(Head)) {
      Error(INSTANTIATION_ERROR,Head,"user defined procedure");
      return(FALSE);
    } else if (!IsIntTerm(Head)) {
      Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"user defined procedure");
      return(FALSE);		
    }
    i = IntOfTerm(Head);
    if (i < 0 || i > 255) {
      Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"user defined procedure");
      return(FALSE);		
    }
    buf[j++] = i;
    if (j > bufsize) {
      buf[j-1] = '\0';
      return(FALSE);
    }
    t = TailOfTerm(t);
    if (IsVarTerm(t)) {
      Error(INSTANTIATION_ERROR,t,"user defined procedure");
      return(FALSE);
    } else if (!IsPairTerm(t) && t != TermNil) {
      Error(TYPE_ERROR_LIST, t, "user defined procedure");
      return(FALSE);
    }
  }
  buf[j] = '\0';
  return(TRUE);
}


/* copy a string to a buffer */
X_API Term
YapBufferToString(char *s)
{
  Term t; 
  BACKUP_H();

  t = StringToList(s);

  RECOVER_H();
  return(t);
}

/* copy a string to a buffer */
X_API Term
YapBufferToAtomList(char *s)
{
  Term t; 
  BACKUP_H();

  t = StringToListOfAtoms(s);

  RECOVER_H();
  return(t);
}


X_API void
YapError(char *buf)
{
  Error(SYSTEM_ERROR,TermNil,buf);
}

static void myputc (int ch)
{
  putc(ch,stderr);
}

X_API int
YapRunGoal(Term t)
{
  int out;
  yamop *old_CP = CP;
  BACKUP_MACHINE_REGS();

  out = RunTopGoal(t);
  if (out) {
    P = (yamop *)ENV[E_CP];
    ENV = (CELL *)ENV[E_E];
    CP = old_CP;
  } else {
    B = B->cp_b;
  }

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API int
YapRestartGoal(void)
{
  int out;
  BACKUP_MACHINE_REGS();

  P = (yamop *)FAILCODE;
  do_putcf = myputc;
  out = exec_absmi(TRUE);
  if (out == FALSE) {
    /* cleanup */
    trust_last();
  }

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API int
YapContinueGoal(void)
{
  int out;
  BACKUP_MACHINE_REGS();

  out = exec_absmi(TRUE);

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API void
YapPruneGoal(void)
{
  BACKUP_B();

  while (B->cp_ap != NOCODE) {
    B = B->cp_b;
  }
  B = B->cp_b;

  RECOVER_B();
}

X_API int
YapGoalHasException(Term *t)
{
  int out = FALSE;
  BACKUP_MACHINE_REGS();
  if (EX) {
    *t = EX;
    out = TRUE;
  }
  RECOVER_MACHINE_REGS();
  return(out);
}

X_API void
YapInitConsult(int mode, char *filename)
{
  BACKUP_MACHINE_REGS();

  if (mode == YAP_CONSULT_MODE)
    init_consult(FALSE, filename);
  else
    init_consult(TRUE, filename);

  RECOVER_MACHINE_REGS();
}

X_API void
YapEndConsult(void)
{
  BACKUP_MACHINE_REGS();

  end_consult();

  RECOVER_MACHINE_REGS();
}

X_API Term
YapRead(int (*mygetc)(void))
{
  Term t;
  tr_fr_ptr old_TR;
  
  BACKUP_MACHINE_REGS();

  do_getf = mygetc;
  old_TR = TR;
  tokptr = toktide = tokenizer(do_yap_getc, do_yap_getc);
  if (ErrorMessage)
    {
      TR = old_TR;
      save_machine_regs();
      return(0);
    }
  t = Parse();
  TR = old_TR;

  RECOVER_MACHINE_REGS();
  return(t);
}

X_API void
YapWrite(Term t, void (*myputc)(int), int flags)
{
  BACKUP_MACHINE_REGS();

  do_putcf = myputc;
  plwrite (t, do_yap_putc, flags);

  RECOVER_MACHINE_REGS();
}

X_API char *
YapCompileClause(Term t)
{
  char *ErrorMessage;
  CODEADDR codeaddr;
  int mod = CurrentModule;

  BACKUP_MACHINE_REGS();

  ErrorMessage = NULL;
  ARG1 = t;
  codeaddr = cclause (t,0, mod);
  if (codeaddr != NULL) {
    t = Deref(ARG1); /* just in case there was an heap overflow */
    addclause (t, codeaddr, TRUE, mod);
  }

  RECOVER_MACHINE_REGS();
  return(ErrorMessage);
}

/* this routine is supposed to be called from an external program
   that wants to control Yap */

X_API Int
YapInit(yap_init_args *yap_init)
{
  int restore_result;
  int Trail = 0, Stack = 0, Heap = 0;
  BACKUP_MACHINE_REGS();

  yap_args = yap_init->Argv;
  yap_argc = yap_init->Argc;
  if (yap_init->SavedState != NULL) {
    if (SavedInfo (yap_init->SavedState, &Trail, &Stack, &Heap, yap_init->YapLibDir) != 1) {
      return(YAP_BOOT_FROM_SAVED_ERROR);
    }
  }
  if (yap_init->TrailSize == 0) {
    if (Trail == 0)
      Trail = DefTrailSpace;
  } else {
    Trail = yap_init->TrailSize;
  }
  if (yap_init->StackSize == 0) {
    if (Stack == 0)
      Stack = DefStackSpace;
  } else {
    Stack = yap_init->StackSize;
  }
  if (yap_init->HeapSize == 0) {
    if (Heap == 0)
      Heap = DefHeapSpace;  
  } else {
    Heap = yap_init->HeapSize;
  }
  InitStacks (Heap, Stack, Trail,
	      yap_init->NumberWorkers,
	      yap_init->SchedulerLoop,
	      yap_init->DelayedReleaseLoad
	      );
  InitYaamRegs();

#if HAVE_MPI
  InitMPI ();
#endif
#if HAVE_MPE
  InitMPE ();
#endif

  if (yap_init->YapPrologBootFile != NULL) {
    /*
      This must be done before restore, otherwise
      restore will print out messages ....
    */
    yap_flags[HALT_AFTER_CONSULT_FLAG] = yap_init->HaltAfterConsult;
  }
  if (yap_init->SavedState != NULL) {
    restore_result = Restore(yap_init->SavedState);
  } else {
    restore_result = FAIL_RESTORE;
  }
  yap_flags[FAST_BOOT_FLAG] = yap_init->FastBoot;
#if defined(YAPOR) || defined(TABLING)
  make_root_frames();
#ifdef YAPOR
  init_workers();
#endif /* YAPOR */
  init_local();
#ifdef YAPOR
  if (worker_id != 0) {
#if SBA
    /*
      In the SBA we cannot just happily inherit registers
      from the other workers
    */
    InitYaamRegs();
#endif
    /* slaves, waiting for work */
    CurrentModule = 1;
    P = GETWORK_FIRST_TIME;
    exec_absmi(FALSE);
    abort_optyap("abstract machine unexpected exit");
  }
#endif /* YAPOR */
#endif /* YAPOR || TABLING */
  RECOVER_MACHINE_REGS();

  if (yap_init->YapPrologBootFile != NULL) {
    PutValue(FullLookupAtom("$consult_on_boot"), MkAtomTerm(LookupAtom(yap_init->YapPrologBootFile)));
    /*
      This must be done again after restore, as yap_flags
      has been overwritten ....
    */
    yap_flags[HALT_AFTER_CONSULT_FLAG] = yap_init->HaltAfterConsult;
  }
  if (yap_init->SavedState != NULL) {

    if (restore_result == FAIL_RESTORE)
      return(YAP_BOOT_FROM_SAVED_ERROR);
    if (restore_result == DO_ONLY_CODE) {
      return(YAP_BOOT_FROM_SAVED_CODE);
    } else {
      return(YAP_BOOT_FROM_SAVED_STACKS);
    }
  }
  return(YAP_BOOT_FROM_PROLOG);
}

X_API Int
YapFastInit(char saved_state[])
{
  yap_init_args init_args;

  init_args.SavedState = saved_state;
  init_args.HeapSize = 0;
  init_args.StackSize = 0;
  init_args.TrailSize = 0;
  init_args.YapLibDir = NULL;
  init_args.YapPrologBootFile = NULL;
  init_args.HaltAfterConsult = FALSE;
  init_args.FastBoot = FALSE;
  init_args.NumberWorkers = 1;
  init_args.SchedulerLoop = 10;
  init_args.DelayedReleaseLoad = 3;
  init_args.Argc = 0;
  init_args.Argv = NULL;

  return(YapInit(&init_args));
}

X_API void
YapPutValue(Atom at, Term t)
{
  PutValue(at, t);
}

X_API Term
YapGetValue(Atom at)
{
  return(GetValue(at));
}

X_API int
YapReset(void)
{
  BACKUP_MACHINE_REGS();

  /* first, backtrack to the root */
  if (B != NULL) {
    while (B->cp_b != NULL)
      B = B->cp_b;
    P = (yamop *)FAILCODE;
    if (exec_absmi(0) != 0)
      return(FALSE);
  }
  /* reinitialise the engine */
  InitYaamRegs();

  RECOVER_MACHINE_REGS();
  return(TRUE);
}

X_API void
YapExit(int retval)
{
  exit_yap(retval);
}

X_API void
YapInitSocks(char *host, long port)
{
#if USE_SOCKET
  init_socks(host, port);
#endif
}

X_API void
YapSetOutputMessage(void)
{
#if DEBUG
  output_msg = TRUE;
#endif
}

X_API int
YapStreamToFileNo(Term t)
{
  return(StreamToFileNo(t));
}

X_API void
YapCloseAllOpenStreams(void)
{
  BACKUP_H();

  CloseStreams(FALSE);

  RECOVER_H();
}

X_API Term
YapOpenStream(void *fh, char *name, Term nm, int flags)
{
  Term retv;

  BACKUP_H();

  retv = OpenStream((FILE *)fh, name, nm, flags);

  RECOVER_H();
  return retv;
}

X_API void
YapThrow(Term t)
{
  BACKUP_MACHINE_REGS();
  JumpToEnv(t);
  RECOVER_MACHINE_REGS();
}

X_API int
YapLookupModule(Term t)
{
  return(LookupModule(t));
}

X_API Term
YapModuleName(int i)
{
  return(ModuleName[i]);
}

X_API void
YapHalt(int i)
{
  exit_yap(i);
}

X_API CELL *
YapTopOfLocalStack(void)
{
  return(ASP);
}

X_API void *
YapPredicate(Atom a, Int arity, Int m)
{
  if (arity == 0) {
    return((void *)RepPredProp(PredPropByAtom(a,m)));
  } else {
    Functor f = MkFunctor(a, arity);
    return((void *)RepPredProp(PredPropByFunc(f,m)));
  }
} 

X_API void
YapPredicateInfo(void *p, Atom* a, Int* arity, Int* m)
{
  PredEntry *pd = (PredEntry *)p;
  if (pd->ArityOfPE) {
    *arity = pd->ArityOfPE;
    *a = NameOfFunctor(pd->FunctorOfPred);
  } else {
    *arity = 0;
    *a = (Atom)(pd->FunctorOfPred);
  }
  *m = pd->ModuleOfPred;
} 

X_API void
YapUserCPredicateWithArgs(char *a, CPredicate f, Int arity, Int mod)
{
  PredEntry *pe;
  SMALLUNSGN cm = CurrentModule;
  CurrentModule = mod;
  UserCPredicate(a,f,arity);
  if (arity == 0) {
    pe = RepPredProp(PredPropByAtom(LookupAtom(a),mod));
  } else {
    Functor f = MkFunctor(LookupAtom(a), arity);
    pe = RepPredProp(PredPropByFunc(f,mod));
  }
  pe->PredFlags |= CArgsPredFlag;
  CurrentModule = cm;
} 

X_API Int
YapCurrentModule(void)
{
  return(CurrentModule);
} 
