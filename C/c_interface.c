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

#if defined(_MSC_VER) && defined(YAPDLL_EXPORTS)
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
X_API Term    STD_PROTO(YapHeadOfTerm,(Term));
X_API Term    STD_PROTO(YapTailOfTerm,(Term));
X_API Term    STD_PROTO(YapMkApplTerm,(Functor,unsigned int,Term *));
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
      Int     STD_PROTO(YapExecute,(CPredicate));
X_API int     STD_PROTO(YapReset,(void));
X_API Int     STD_PROTO(YapInit,(yap_init_args *));
X_API Int     STD_PROTO(YapFastInit,(char *));
X_API Int     STD_PROTO(YapCallProlog,(Term));
X_API void   *STD_PROTO(YapAllocSpaceFromYap,(unsigned int));
X_API void    STD_PROTO(YapFreeSpaceFromYap,(void *));
X_API void    STD_PROTO(YapFreeSpaceFromYap,(void *));
X_API int     STD_PROTO(YapStringToBuffer, (Term, char *, unsigned int));
X_API void    STD_PROTO(YapError,(char *));
X_API int     STD_PROTO(YapRunGoal,(Term));
X_API int     STD_PROTO(YapRestartGoal,(void));
X_API int     STD_PROTO(YapContinueGoal,(void));
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

X_API Term
YapA(int i)
{

  return(Deref(XREGS[i]));
}

X_API Bool 
YapIsIntTerm(Term t)
{
  return (IsIntTerm(t) || IsLongIntTerm(t));
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

  B = B->cp_b; /* cut_fail */

  RECOVER_B();
  return(FALSE);
}

X_API Int
Yapcut_succeed(void)
{
  BACKUP_B();

  B = B->cp_b;

  RECOVER_B();
  return(TRUE);
}

X_API Int
YapUnify(Term pt1, Term pt2)
{
  Int out;
  BACKUP_MACHINE_REGS();

  out = unify(pt1, pt2);

  RECOVER_MACHINE_REGS();
  return(out);
}

Int YapExecute(CPredicate code)
{
  return((code)());
}

X_API Int YapCallProlog(Term t)
{
  Int out;
  BACKUP_MACHINE_REGS();

  out = execute_goal(t,0);

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API void *YapAllocSpaceFromYap(unsigned int size)
{
  void *ptr;
  BACKUP_MACHINE_REGS();

  if ((ptr = AllocCodeSpace(size)) == NULL) {
    if (!growheap(FALSE)) {
      Abort("[ SYSTEM ERROR: YAP failed to reserve space in growheap ]\n");
      return(NULL);
    }
  }

  RECOVER_MACHINE_REGS();
  return(ptr);
}

X_API void YapFreeSpaceFromYap(void *ptr)
{
  FreeCodeSpace(ptr);
}

/* copy a string to a buffer */
X_API int YapStringToBuffer(Term t, char *buf, unsigned int bufsize)
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
    if (j > bufsize) return(FALSE);
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


X_API void
YapError(char *buf)
{
  Error(SYSTEM_ERROR,TermNil,buf);
}

X_API int
YapRunGoal(Term t)
{
  int out;
  BACKUP_MACHINE_REGS();

  InitYaamRegs();
  out = RunTopGoal(t);

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API int
YapRestartGoal(void)
{
  int out;
  BACKUP_MACHINE_REGS();

  P = (yamop *)FAILCODE;
  out = exec_absmi(TRUE);

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

static int (*do_getf)(void);

static int do_yap_getc(int streamno) {
  return(do_getf());
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

static void (*do_putcf)(int);

static int do_yap_putc(int streamno,int ch) {
  do_putcf(ch);
  return(ch);
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
  BACKUP_MACHINE_REGS();

  ErrorMessage = NULL;
  ARG1 = t;
  codeaddr = cclause (t,0);
  if (codeaddr != NULL) {
    t = Deref(ARG1); /* just in case there was an heap overflow */
    addclause (t, codeaddr, TRUE);
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
  exit_yap(retval, NULL);
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

