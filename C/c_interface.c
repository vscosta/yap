/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.Santos Costa and Universidade do Porto 1985--	 *
*									 *
**************************************************************************
*									 *
* File:		c_interface.c						 *
* comments:	c_interface primitives definition 			 *
*									 *
* Last rev:	$Date: 2005-03-04 20:30:10 $,$Author: ricroc $						 *
* $Log: not supported by cvs2svn $
* Revision 1.62  2005/03/02 18:35:44  vsc
* try to make initialisation process more robust
* try to make name more robust (in case Lookup new atom fails)
*
* Revision 1.61  2005/03/01 22:25:08  vsc
* fix pruning bug
* make DL_MALLOC less enthusiastic about walking through buckets.
*
* Revision 1.60  2005/02/08 18:04:47  vsc
* library_directory may not be deterministic (usually it isn't).
*
* Revision 1.59  2004/12/08 00:56:35  vsc
* missing ;
*
* Revision 1.58  2004/11/19 22:08:41  vsc
* replace SYSTEM_ERROR by out OUT_OF_WHATEVER_ERROR whenever appropriate.
*
* Revision 1.57  2004/11/18 22:32:31  vsc
* fix situation where we might assume nonextsing double initialisation of C predicates (use
* Hidden Pred Flag).
* $host_type was double initialised.
*
* Revision 1.56  2004/10/31 02:18:03  vsc
* fix bug in handling Yap heap overflow while adding new clause.
*
* Revision 1.55  2004/10/28 20:12:20  vsc
* Use Doug Lea's malloc as an alternative to YAP's standard malloc
* don't use TR directly in scanner/parser, this avoids trouble with ^C while
* consulting large files.
* pass gcc -mno-cygwin to library compilation in cygwin environment (cygwin should
* compile out of the box now).
*
* Revision 1.54  2004/10/06 16:55:46  vsc
* change configure to support big mem configs
* get rid of extra globals
* fix trouble with multifile preds
*
* Revision 1.53  2004/08/11 16:14:51  vsc
* whole lot of fixes:
*   - memory leak in indexing
*   - memory management in WIN32 now supports holes
*   - extend Yap interface, more support for SWI-Interface
*   - new predicate mktime in system
*   - buffer console I/O in WIN32
*
* Revision 1.52  2004/07/23 03:37:16  vsc
* fix heap overflow in YAP_LookupAtom
*
* Revision 1.51  2004/07/22 21:32:20  vsc
* debugger fixes
* initial support for JPL
* bad calls to garbage collector and gc
* debugger fixes
*
* Revision 1.50  2004/06/29 19:04:41  vsc
* fix multithreaded version
* include new version of Ricardo's profiler
* new predicat atomic_concat
* allow multithreaded-debugging
* small fixes
*
* Revision 1.49  2004/06/09 03:32:02  vsc
* fix bugs
*
* Revision 1.48  2004/06/05 03:36:59  vsc
* coroutining is now a part of attvars.
* some more fixes.
*
* Revision 1.47  2004/05/17 21:42:08  vsc
* misc fixes
*
* Revision 1.46  2004/05/14 17:56:45  vsc
* Yap_WriteBuffer
*
* Revision 1.45  2004/05/14 17:11:30  vsc
* support BigNums in interface
*
* Revision 1.44  2004/05/14 16:33:44  vsc
* add Yap_ReadBuffer
*									 *
*									 *
*************************************************************************/

#define Bool int
#define flt double
#define YAP_Term Term
#define C_INTERFACE

#include "Yap.h"
#include "clause.h"
#include "yapio.h"
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if _MSC_VER || defined(__MINGW32__) 
#include <windows.h>
#endif
#include "iopreds.h"
#define HAS_YAP_H 1
#include "yap_structs.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#include "threads.h"

#define YAP_BOOT_FROM_PROLOG       0
#define YAP_BOOT_FROM_SAVED_CODE   1
#define YAP_BOOT_FROM_SAVED_STACKS 2
#define YAP_BOOT_FROM_SAVED_ERROR  -1

#if defined(_MSC_VER) && defined(YAP_EXPORTS)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif

X_API Term    STD_PROTO(YAP_A,(int));
X_API Term    STD_PROTO(YAP_Deref,(Term));
X_API Term    STD_PROTO(YAP_MkVarTerm,(void));
X_API Bool    STD_PROTO(YAP_IsVarTerm,(Term));
X_API Bool    STD_PROTO(YAP_IsNonVarTerm,(Term));
X_API Bool    STD_PROTO(YAP_IsIntTerm,(Term));
X_API Bool    STD_PROTO(YAP_IsBigNumTerm,(Term));
X_API Bool    STD_PROTO(YAP_IsFloatTerm,(Term));
X_API Bool    STD_PROTO(YAP_IsDbRefTerm,(Term));
X_API Bool    STD_PROTO(YAP_IsAtomTerm,(Term));
X_API Bool    STD_PROTO(YAP_IsPairTerm,(Term));
X_API Bool    STD_PROTO(YAP_IsApplTerm,(Term));
X_API Term    STD_PROTO(YAP_MkIntTerm,(Int));
X_API Term    STD_PROTO(YAP_MkBigNumTerm,(void *));
X_API Int     STD_PROTO(YAP_IntOfTerm,(Term));
X_API void   *STD_PROTO(YAP_BigNumOfTerm,(Term));
X_API Term    STD_PROTO(YAP_MkFloatTerm,(flt));
X_API flt     STD_PROTO(YAP_FloatOfTerm,(Term));
X_API Term    STD_PROTO(YAP_MkAtomTerm,(Atom));
X_API Atom    STD_PROTO(YAP_AtomOfTerm,(Term));
X_API Atom    STD_PROTO(YAP_LookupAtom,(char *));
X_API Atom    STD_PROTO(YAP_FullLookupAtom,(char *));
X_API char   *STD_PROTO(YAP_AtomName,(Atom));
X_API Term    STD_PROTO(YAP_MkPairTerm,(Term,Term));
X_API Term    STD_PROTO(YAP_MkNewPairTerm,(void));
X_API Term    STD_PROTO(YAP_HeadOfTerm,(Term));
X_API Term    STD_PROTO(YAP_TailOfTerm,(Term));
X_API Term    STD_PROTO(YAP_MkApplTerm,(Functor,unsigned long int,Term *));
X_API Term    STD_PROTO(YAP_MkNewApplTerm,(Functor,unsigned long int));
X_API Functor STD_PROTO(YAP_FunctorOfTerm,(Term));
X_API Term    STD_PROTO(YAP_ArgOfTerm,(Int,Term));
X_API Functor STD_PROTO(YAP_MkFunctor,(Atom,Int));
X_API Atom    STD_PROTO(YAP_NameOfFunctor,(Functor));
X_API Int     STD_PROTO(YAP_ArityOfFunctor,(Functor));
X_API void   *STD_PROTO(YAP_ExtraSpace,(void));
X_API Int     STD_PROTO(YAP_cut_fail,(void));
X_API Int     STD_PROTO(YAP_cut_succeed,(void));
X_API Int     STD_PROTO(YAP_Unify,(Term,Term));
X_API int     STD_PROTO(YAP_Reset,(void));
X_API Int     STD_PROTO(YAP_Init,(YAP_init_args *));
X_API Int     STD_PROTO(YAP_FastInit,(char *));
X_API Int     STD_PROTO(YAP_CallProlog,(Term));
X_API void   *STD_PROTO(YAP_AllocSpaceFromYap,(unsigned int));
X_API void    STD_PROTO(YAP_FreeSpaceFromYap,(void *));
X_API int     STD_PROTO(YAP_StringToBuffer, (Term, char *, unsigned int));
X_API Term    STD_PROTO(YAP_ReadBuffer, (char *,Term *));
X_API Term    STD_PROTO(YAP_BufferToString, (char *));
X_API Term    STD_PROTO(YAP_BufferToAtomList, (char *));
X_API void    STD_PROTO(YAP_Error,(int, Term, char *, ...));
X_API Term    STD_PROTO(YAP_RunGoal,(Term));
X_API int     STD_PROTO(YAP_RestartGoal,(void));
X_API int     STD_PROTO(YAP_GoalHasException,(Term *));
X_API int     STD_PROTO(YAP_ContinueGoal,(void));
X_API void    STD_PROTO(YAP_PruneGoal,(void));
X_API void    STD_PROTO(YAP_InitConsult,(int, char *));
X_API void    STD_PROTO(YAP_EndConsult,(void));
X_API Term    STD_PROTO(YAP_Read, (int (*)(void)));
X_API void    STD_PROTO(YAP_Write, (Term, void (*)(int), int));
X_API Term    STD_PROTO(YAP_WriteBuffer, (Term, char *, unsigned int, int));
X_API char   *STD_PROTO(YAP_CompileClause, (Term));
X_API void    STD_PROTO(YAP_PutValue, (Atom,Term));
X_API Term    STD_PROTO(YAP_GetValue, (Atom));
X_API int     STD_PROTO(YAP_CompareTerms, (Term,Term));
X_API int     STD_PROTO(YAP_Reset, (void));
X_API void    STD_PROTO(YAP_Exit, (int));
X_API void    STD_PROTO(YAP_InitSocks, (char *, long));
X_API void    STD_PROTO(YAP_SetOutputMessage, (void));
X_API int     STD_PROTO(YAP_StreamToFileNo, (Term));
X_API void    STD_PROTO(YAP_CloseAllOpenStreams,(void));
X_API Term    STD_PROTO(YAP_OpenStream,(void *, char *, Term, int));
X_API long    STD_PROTO(YAP_NewSlots,(int));
X_API long    STD_PROTO(YAP_InitSlot,(Term));
X_API Term    STD_PROTO(YAP_GetFromSlot,(long));
X_API Term   *STD_PROTO(YAP_AddressFromSlot,(long));
X_API void    STD_PROTO(YAP_PutInSlot,(long, Term));
X_API void    STD_PROTO(YAP_RecoverSlots,(int));
X_API void    STD_PROTO(YAP_Throw,(Term));
X_API void    STD_PROTO(YAP_Halt,(int));
X_API Term   *STD_PROTO(YAP_TopOfLocalStack,(void));
X_API void   *STD_PROTO(YAP_Predicate,(Atom,unsigned long int,int));
X_API void    STD_PROTO(YAP_PredicateInfo,(void *,Atom *,unsigned long int *,Term *));
X_API void    STD_PROTO(YAP_UserCPredicate,(char *,CPredicate,unsigned long int));
X_API void    STD_PROTO(YAP_UserBackCPredicate,(char *,CPredicate,CPredicate,unsigned long int,unsigned int));
X_API void    STD_PROTO(YAP_UserCPredicateWithArgs,(char *,CPredicate,unsigned long int,Term));
X_API Term     STD_PROTO(YAP_CurrentModule,(void));
X_API Term     STD_PROTO(YAP_CreateModule,(Atom));
X_API int     STD_PROTO(YAP_ThreadSelf,(void));
X_API int     STD_PROTO(YAP_GetThreadRefCount,(int));
X_API void    STD_PROTO(YAP_SetThreadRefCount,(int,int));
X_API int     STD_PROTO(YAP_ThreadCreateEngine,(thread_attr *));
X_API int     STD_PROTO(YAP_ThreadAttachEngine,(int));
X_API int     STD_PROTO(YAP_ThreadDetachEngine,(int));
X_API int     STD_PROTO(YAP_ThreadDestroyEngine,(int));

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
YAP_A(int i)
{
  return(Deref(XREGS[i]));
}

X_API Term
YAP_Deref(Term t)
{
  return(Deref(t));
}

X_API Bool 
YAP_IsIntTerm(Term t)
{
  return IsIntegerTerm(t);
}

X_API Bool 
YAP_IsBigNumTerm(Term t)
{
#if USE_GMP
  return IsIntegerTerm(t);
#else
  return FALSE;
#endif
}

X_API Bool 
YAP_IsVarTerm(Term t)
{
  return (IsVarTerm(t));
}

X_API Bool 
YAP_IsNonVarTerm(Term t)
{
  return (IsNonVarTerm(t));
}

X_API Bool 
YAP_IsFloatTerm(Term t)
{
  return (IsFloatTerm(t));
}

X_API Bool 
YAP_IsDbRefTerm(Term t)
{
  return (IsDBRefTerm(t));
}

X_API Bool 
YAP_IsAtomTerm(Term t)
{
  return (IsAtomTerm(t));
}

X_API Bool 
YAP_IsPairTerm(Term t)
{
  return (IsPairTerm(t));
}

X_API Bool 
YAP_IsApplTerm(Term t)
{
  return (IsApplTerm(t) && !IsExtensionFunctor(FunctorOfTerm(t)));
}


X_API Term 
YAP_MkIntTerm(Int n)
{
  Term I;
  BACKUP_H();

  I = MkIntegerTerm(n);
  RECOVER_H();
  return(I);
}

X_API Int 
YAP_IntOfTerm(Term t)
{
  if (!IsApplTerm(t))
    return IntOfTerm(t);
  else {
    return LongIntOfTerm(t);
  }
}

X_API Term 
YAP_MkBigNumTerm(void *big)
{
#if USE_GMP
  Term I;
  BACKUP_H();

  I = Yap_MkBigIntTerm((MP_INT *)big);
  RECOVER_H();
  return I;
#else
  return TermNil;
#endif /* USE_GMP */
}

X_API void * 
YAP_BigNumOfTerm(Term t)
{
#if USE_GMP
  if (IsVarTerm(t))
    return NULL;
  if (!IsBigIntTerm(t))
    return NULL;
  return (void *)Yap_BigIntOfTerm(t); 
#else
  return NULL;
#endif /* USE_GMP */
}

X_API Term 
YAP_MkFloatTerm(double n)
{
  Term t;
  BACKUP_H();

  t = MkFloatTerm(n);

  RECOVER_H();
  return t;
}

X_API flt 
YAP_FloatOfTerm(Term t)
{
  return (FloatOfTerm(t));
}

X_API Term 
YAP_MkAtomTerm(Atom n)
{
  Term t;

  t = MkAtomTerm(n);
  return t;
}

X_API Atom 
YAP_AtomOfTerm(Term t)
{
  return (AtomOfTerm(t));
}


X_API char           *
YAP_AtomName(Atom a)
{
  char *o;

  o = AtomName(a);
  return(o);
}

X_API Atom
YAP_LookupAtom(char *c)
{
  Atom a;

  while (TRUE) {
    a = Yap_LookupAtom(c);
    if (a == NIL || (ActiveSignals & YAP_CDOVF_SIGNAL)) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap: %s", Yap_ErrorMessage);
      }
    } else {
      return a;
    }
  }
}

X_API Atom
YAP_FullLookupAtom(char *c)
{
  Atom at;

  while (TRUE) {
    at = Yap_FullLookupAtom(c);
    if (at == NIL || (ActiveSignals & YAP_CDOVF_SIGNAL)) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap: %s", Yap_ErrorMessage);
      }
    } else {
      return at;
    }
  }
}

X_API Term
YAP_MkVarTerm(void)
{
  CELL t; 
  BACKUP_H();

  t = MkVarTerm();

  RECOVER_H();
  return t;
}

X_API Term
YAP_MkPairTerm(Term t1, Term t2)
{
  Term t; 
  BACKUP_H();

  t = MkPairTerm(t1, t2);
  
  RECOVER_H();
  return t;
}

X_API Term
YAP_MkNewPairTerm()
{
  Term t; 
  BACKUP_H();

  t = Yap_MkNewPairTerm();
  
  RECOVER_H();
  return t;
}

X_API Term 
YAP_HeadOfTerm(Term t)
{
  return (HeadOfTerm(t));
}

X_API Term 
YAP_TailOfTerm(Term t)
{
  return (TailOfTerm(t));
}

X_API Term
YAP_MkApplTerm(Functor f,unsigned long int arity, Term args[])
{
  Term t; 
  BACKUP_H();

  t = Yap_MkApplTerm(f, arity, args);

  RECOVER_H();
  return t;
}

X_API Term
YAP_MkNewApplTerm(Functor f,unsigned long int arity)
{
  Term t; 
  BACKUP_H();

  t = Yap_MkNewApplTerm(f, arity);

  RECOVER_H();
  return t;
}

X_API Functor 
YAP_FunctorOfTerm(Term t)
{
  return (FunctorOfTerm(t));
}


X_API Term 
YAP_ArgOfTerm(Int n, Term t)
{
  return (ArgOfTerm(n, t));
}



X_API Functor 
YAP_MkFunctor(Atom a, Int n)
{
  return (Yap_MkFunctor(a, n));
 }

X_API Atom 
YAP_NameOfFunctor(Functor f)
{
  return (NameOfFunctor(f));
}

X_API Int 
YAP_ArityOfFunctor(Functor f)
{
  return (ArityOfFunctor(f));
}

X_API void *
YAP_ExtraSpace(void)
{
  void *ptr;
  BACKUP_B();

  /* find a pointer to extra space allocable */
  ptr = (void *)((CELL *)(B+1)+P->u.lds.s);

  RECOVER_B();
  return(ptr);
}

X_API Int
YAP_cut_fail(void)
{
  BACKUP_B();

  B = B->cp_b;  /* cut_fail */
  HB = B->cp_h; /* cut_fail */

  RECOVER_B();
  return(FALSE);
}

X_API Int
YAP_cut_succeed(void)
{
  BACKUP_B();

  B = B->cp_b;
  HB = B->cp_h;

  RECOVER_B();
  return(TRUE);
}

X_API Int
YAP_Unify(Term t1, Term t2)
{
  Int out;
  BACKUP_MACHINE_REGS();

  out = Yap_unify(t1, t2);

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API long
YAP_NewSlots(int n)
{
  return Yap_NewSlots(n);
}

X_API long
YAP_InitSlot(Term t)
{
  return Yap_InitSlot(t);
}

X_API void
YAP_RecoverSlots(int n)
{
 Yap_RecoverSlots(n);
}

X_API Term
YAP_GetFromSlot(long slot)
{
  return Yap_GetFromSlot(slot);
}

X_API Term *
YAP_AddressFromSlot(long slot)
{
  return Yap_AddressFromSlot(slot);
}

X_API void
YAP_PutInSlot(long slot, Term t)
{
  Yap_PutInSlot(slot, t);
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
YAP_Execute(PredEntry *pe, CPredicate exec_code)
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
	return ((code1)(YAP_InitSlot(Deref(ARG1))));
      }
    case 2:
      {
	CPredicate2 code2 = (CPredicate2)exec_code;
	return ((code2)(YAP_InitSlot(Deref(ARG1)),
			YAP_InitSlot(Deref(ARG2))));
      }
    case 3:
      {
	CPredicate3 code3 = (CPredicate3)exec_code;
	return ((code3)(YAP_InitSlot(Deref(ARG1)),
			YAP_InitSlot(Deref(ARG2)),
			YAP_InitSlot(Deref(ARG3))));
      }
    case 4:
      {
	CPredicate4 code4 = (CPredicate4)exec_code;
	return ((code4)(YAP_InitSlot(Deref(ARG1)),
			YAP_InitSlot(Deref(ARG2)),
			YAP_InitSlot(Deref(ARG3)),
			YAP_InitSlot(Deref(ARG4))));
      }
    case 5:
      {
	CPredicate5 code5 = (CPredicate5)exec_code;
	return ((code5)(YAP_InitSlot(Deref(ARG1)),
			YAP_InitSlot(Deref(ARG2)),
			YAP_InitSlot(Deref(ARG3)),
			YAP_InitSlot(Deref(ARG4)),
			YAP_InitSlot(Deref(ARG5))));
      }
    case 6:
      {
	CPredicate6 code6 = (CPredicate6)exec_code;
	return ((code6)(YAP_InitSlot(Deref(ARG1)),
			YAP_InitSlot(Deref(ARG2)),
			YAP_InitSlot(Deref(ARG3)),
			YAP_InitSlot(Deref(ARG4)),
			YAP_InitSlot(Deref(ARG5)),
			YAP_InitSlot(Deref(ARG6))));
      }
    case 7:
      {
	CPredicate7 code7 = (CPredicate7)exec_code;
	return ((code7)(YAP_InitSlot(Deref(ARG1)),
			YAP_InitSlot(Deref(ARG2)),
			YAP_InitSlot(Deref(ARG3)),
			YAP_InitSlot(Deref(ARG4)),
			YAP_InitSlot(Deref(ARG5)),
			YAP_InitSlot(Deref(ARG6)),
			YAP_InitSlot(Deref(ARG7))));
      }
    case 8:
      {
	CPredicate8 code8 = (CPredicate8)exec_code;
	return ((code8)(YAP_InitSlot(Deref(ARG1)),
			YAP_InitSlot(Deref(ARG2)),
			YAP_InitSlot(Deref(ARG3)),
			YAP_InitSlot(Deref(ARG4)),
			YAP_InitSlot(Deref(ARG5)),
			YAP_InitSlot(Deref(ARG6)),
			YAP_InitSlot(Deref(ARG7)),
			YAP_InitSlot(Deref(ARG8))));
      }
    default:
      return(FALSE);
    }
  } else {
    return((exec_code)());
  }
}

X_API Int
YAP_CallProlog(Term t)
{
  Int out;
  Term mod = CurrentModule;
  BACKUP_MACHINE_REGS();

  while (!IsVarTerm(t) &&
      IsApplTerm(t) &&
      FunctorOfTerm(t) == FunctorModule) {
    Term tmod = ArgOfTerm(1,t);
    if (IsVarTerm(tmod)) return(FALSE);
    if (!IsAtomTerm(tmod)) return(FALSE);
    mod = tmod;
    t = ArgOfTerm(2,t);
  }
  out = Yap_execute_goal(t, 0, mod);
  RECOVER_MACHINE_REGS();
  return(out);
}

X_API void *
YAP_AllocSpaceFromYap(unsigned int size)
{
  void *ptr;
  BACKUP_MACHINE_REGS();

  if ((ptr = Yap_AllocCodeSpace(size)) == NULL) {
    if (!Yap_growheap(FALSE, size, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return(NULL);
    }
  }

  RECOVER_MACHINE_REGS();
  return(ptr);
}

X_API void
YAP_FreeSpaceFromYap(void *ptr)
{
  Yap_FreeCodeSpace(ptr);
}

/* copy a string to a buffer */
X_API int
YAP_StringToBuffer(Term t, char *buf, unsigned int bufsize)
{
  unsigned int j = 0;

  while (t != TermNil) {
    register Term   Head;
    register Int    i;

    Head = HeadOfTerm(t);
    if (IsVarTerm(Head)) {
      Yap_Error(INSTANTIATION_ERROR,Head,"user defined procedure");
      return(FALSE);
    } else if (!IsIntTerm(Head)) {
      Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"user defined procedure");
      return(FALSE);		
    }
    i = IntOfTerm(Head);
    if (i < 0 || i > 255) {
      Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"user defined procedure");
      return(FALSE);		
    }
    buf[j++] = i;
    if (j > bufsize) {
      buf[j-1] = '\0';
      return(FALSE);
    }
    t = TailOfTerm(t);
    if (IsVarTerm(t)) {
      Yap_Error(INSTANTIATION_ERROR,t,"user defined procedure");
      return(FALSE);
    } else if (!IsPairTerm(t) && t != TermNil) {
      Yap_Error(TYPE_ERROR_LIST, t, "user defined procedure");
      return(FALSE);
    }
  }
  buf[j] = '\0';
  return(TRUE);
}


/* copy a string to a buffer */
X_API Term
YAP_BufferToString(char *s)
{
  Term t; 
  BACKUP_H();

  t = Yap_StringToList(s);

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_ReadBuffer(char *s, Term *tp)
{
  Term t; 
  BACKUP_H();

  t = Yap_StringToTerm(s,tp);

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_BufferToAtomList(char *s)
{
  Term t; 
  BACKUP_H();

  t = Yap_StringToListOfAtoms(s);

  RECOVER_H();
  return t;
}


X_API void
YAP_Error(int myerrno, Term t, char *buf,...)
{
#define YAP_BUF_SIZE 512
  va_list ap;
  char tmpbuf[YAP_BUF_SIZE];

  if (!myerrno)
    myerrno = SYSTEM_ERROR;
  if (t == 0L)
    t = TermNil;
  if (buf != NULL) {
    va_start (ap, buf);
#if   HAVE_VSNPRINTF
    (void) vsnprintf(tmpbuf, YAP_BUF_SIZE, buf, ap);
#else
    (void) vsprintf(tmpbuf, buf, ap);
#endif
    va_end (ap);
  } else {
    tmpbuf[0] = '\0';
  }
  Yap_Error(myerrno,t,tmpbuf);
}

static void myputc (int ch)
{
  putc(ch,stderr);
}

X_API Term
YAP_RunGoal(Term t)
{
  Term out;
  yamop *old_CP = CP;
  BACKUP_MACHINE_REGS();

  Yap_PrologMode = UserMode;
  out = Yap_RunTopGoal(t);
  Yap_PrologMode = UserCCallMode;
  if (out) {
    P = (yamop *)ENV[E_CP];
    ENV = (CELL *)ENV[E_E];
    CP = old_CP;
  } else {
    if (B != NULL) /* restore might have destroyed B */
      B = B->cp_b;
  }

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API int
YAP_RestartGoal(void)
{
  int out;
  BACKUP_MACHINE_REGS();

  P = (yamop *)FAILCODE;
  do_putcf = myputc;
  Yap_PrologMode = UserMode;
  out = Yap_exec_absmi(TRUE);
  Yap_PrologMode = UserCCallMode;
  if (out == FALSE) {
    /* cleanup */
    Yap_trust_last();
  }

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API int
YAP_ContinueGoal(void)
{
  int out;
  BACKUP_MACHINE_REGS();

  Yap_PrologMode = UserMode;
  out = Yap_exec_absmi(TRUE);
  Yap_PrologMode = UserCCallMode;

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API void
YAP_PruneGoal(void)
{
  BACKUP_B();

  while (B->cp_ap != NOCODE) {
    B = B->cp_b;
  }
  B = B->cp_b;

  RECOVER_B();
}

X_API int
YAP_GoalHasException(Term *t)
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
YAP_InitConsult(int mode, char *filename)
{
  BACKUP_MACHINE_REGS();

  if (mode == YAP_CONSULT_MODE)
    Yap_init_consult(FALSE, filename);
  else
    Yap_init_consult(TRUE, filename);

  RECOVER_MACHINE_REGS();
}

X_API void
YAP_EndConsult(void)
{
  BACKUP_MACHINE_REGS();

  Yap_end_consult();

  RECOVER_MACHINE_REGS();
}

X_API Term
YAP_Read(int (*mygetc)(void))
{
  Term t;
  int sno;
  TokEntry *tokstart;
  
  BACKUP_MACHINE_REGS();

  do_getf = mygetc;
  sno = Yap_GetFreeStreamD();
  if (sno < 0) {
    Yap_Error(SYSTEM_ERROR,TermNil, "new stream not available for YAP_Read");
    return TermNil;
  }
  Stream[sno].stream_getc_for_read = Stream[sno].stream_getc = do_yap_getc;
  tokstart = Yap_tokptr = Yap_toktide = Yap_tokenizer(sno);
  Stream[sno].status = Free_Stream_f;
  if (Yap_ErrorMessage)
    {
      save_machine_regs();
      return(0);
    }
  t = Yap_Parse();
  Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);

  RECOVER_MACHINE_REGS();
  return t;
}

X_API void
YAP_Write(Term t, void (*myputc)(int), int flags)
{
  BACKUP_MACHINE_REGS();

  do_putcf = myputc;
  Yap_plwrite (t, do_yap_putc, flags);

  RECOVER_MACHINE_REGS();
}

X_API Term
YAP_WriteBuffer(Term t, char *buf, unsigned int sze, int flags)
{
  BACKUP_MACHINE_REGS();
  t = Yap_TermToString(t, buf, sze, flags);
  RECOVER_MACHINE_REGS();
  return t;
}

X_API char *
YAP_CompileClause(Term t)
{
  yamop *codeaddr;
  int mod = CurrentModule;
  Term tn = TermNil;

  BACKUP_MACHINE_REGS();

  Yap_ErrorMessage = NULL;
  ARG1 = t;
  YAPEnterCriticalSection();
  codeaddr = Yap_cclause (t,0, mod, t);
  if (codeaddr != NULL) {
    t = Deref(ARG1); /* just in case there was an heap overflow */
    if (!Yap_addclause (t, codeaddr, TRUE, mod, &tn)) {
      YAPLeaveCriticalSection();
      return Yap_ErrorMessage;
    }
  }
  YAPLeaveCriticalSection();

  if (ActiveSignals & YAP_CDOVF_SIGNAL) {
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap: %s", Yap_ErrorMessage);
    }
  }
  RECOVER_MACHINE_REGS();
  return(Yap_ErrorMessage);
}

/* this routine is supposed to be called from an external program
   that wants to control Yap */

X_API Int
YAP_Init(YAP_init_args *yap_init)
{
  int restore_result;
  CELL Trail = 0, Stack = 0, Heap = 0;

  Yap_argv = yap_init->Argv;
  Yap_argc = yap_init->Argc;
  if (yap_init->SavedState != NULL ||
      yap_init->YapPrologBootFile == NULL) {
    if (Yap_SavedInfo (yap_init->SavedState, yap_init->YapLibDir, &Trail, &Stack, &Heap) != 1) {
      yap_init->ErrorNo = Yap_Error_TYPE;
      yap_init->ErrorCause = Yap_ErrorMessage;
      return YAP_BOOT_FROM_SAVED_ERROR;
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
  /* tell the system who should cope with interruptions */
  Yap_PrologShouldHandleInterrupts = yap_init->PrologShouldHandleInterrupts;
  Yap_InitWorkspace(Heap, Stack, Trail,
	      yap_init->NumberWorkers,
	      yap_init->SchedulerLoop,
	      yap_init->DelayedReleaseLoad
	      );
  Yap_InitExStacks (Trail, Stack);
  BACKUP_MACHINE_REGS();
  Yap_InitYaamRegs();

#if HAVE_MPI
  Yap_InitMPI ();
#endif
#if HAVE_MPE
  Yap_InitMPE ();
#endif

  if (yap_init->YapPrologRCFile != NULL) {
    /*
      This must be done before restore, otherwise
      restore will print out messages ....
    */
    yap_flags[HALT_AFTER_CONSULT_FLAG] = yap_init->HaltAfterConsult;
  }
  if (yap_init->SavedState != NULL ||
      yap_init->YapPrologBootFile == NULL) {
    restore_result = Yap_Restore(yap_init->SavedState, yap_init->YapLibDir);
    if (restore_result == FAIL_RESTORE) {
      yap_init->ErrorNo = Yap_Error_TYPE;
      yap_init->ErrorCause = Yap_ErrorMessage;
      return YAP_BOOT_FROM_SAVED_ERROR;
    }
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
    Yap_InitYaamRegs();
#endif /* SBA */
    /* slaves, waiting for work */
    CurrentModule = USER_MODULE;
    P = GETWORK_FIRST_TIME;
    Yap_exec_absmi(FALSE);
    abort_yapor("abstract machine unexpected exit");
  }
#endif /* YAPOR */
#endif /* YAPOR || TABLING */
  RECOVER_MACHINE_REGS();

  if (yap_init->YapPrologRCFile != NULL) {
    Yap_PutValue(Yap_FullLookupAtom("$consult_on_boot"), MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologRCFile)));
    /*
      This must be done again after restore, as yap_flags
      has been overwritten ....
    */
    yap_flags[HALT_AFTER_CONSULT_FLAG] = yap_init->HaltAfterConsult;
  }
  if (yap_init->SavedState != NULL ||
      yap_init->YapPrologBootFile == NULL) {
    if (restore_result == FAIL_RESTORE) {
      yap_init->ErrorNo = Yap_Error_TYPE;
      yap_init->ErrorCause = Yap_ErrorMessage;
      return YAP_BOOT_FROM_SAVED_ERROR;
    }
    if (restore_result == DO_ONLY_CODE) {
      return YAP_BOOT_FROM_SAVED_CODE;
    } else {
      return YAP_BOOT_FROM_SAVED_STACKS;
    }
  }
  return YAP_BOOT_FROM_PROLOG;
}

X_API Int
YAP_FastInit(char saved_state[])
{
  YAP_init_args init_args;
  Int out;

  init_args.SavedState = saved_state;
  init_args.HeapSize = 0;
  init_args.StackSize = 0;
  init_args.TrailSize = 0;
  init_args.YapLibDir = NULL;
  init_args.YapPrologBootFile = NULL;
  init_args.YapPrologInitFile = NULL;
  init_args.YapPrologRCFile = NULL;
  init_args.HaltAfterConsult = FALSE;
  init_args.FastBoot = FALSE;
  init_args.NumberWorkers = 1;
  init_args.SchedulerLoop = 10;
  init_args.DelayedReleaseLoad = 3;
  init_args.PrologShouldHandleInterrupts = FALSE;
  init_args.Argc = 0;
  init_args.Argv = NULL;
  init_args.ErrorNo = 0;
  init_args.ErrorCause = NULL;
  out = YAP_Init(&init_args);
  if (out == YAP_BOOT_FROM_SAVED_ERROR) {
    Yap_Error(init_args.ErrorNo,TermNil,init_args.ErrorCause);
  }
  return out;
}

X_API void
YAP_PutValue(Atom at, Term t)
{
  Yap_PutValue(at, t);
}

X_API Term
YAP_GetValue(Atom at)
{
  return(Yap_GetValue(at));
}

X_API int
YAP_CompareTerms(Term t1, Term t2)
{
  return Yap_compare_terms(t1, t2);
} 

X_API int
YAP_Reset(void)
{
  BACKUP_MACHINE_REGS();

  /* first, backtrack to the root */
  if (B != NULL) {
    while (B->cp_b != NULL)
      B = B->cp_b;
    P = (yamop *)FAILCODE;
    if (Yap_exec_absmi(0) != 0)
      return(FALSE);
  }
  /* reinitialise the engine */
  Yap_InitYaamRegs();

  RECOVER_MACHINE_REGS();
  return(TRUE);
}

X_API void
YAP_Exit(int retval)
{
  Yap_exit(retval);
}

X_API void
YAP_InitSocks(char *host, long port)
{
#if USE_SOCKET
  Yap_init_socks(host, port);
#endif
}

X_API void
YAP_SetOutputMessage(void)
{
#if DEBUG
  Yap_output_msg = TRUE;
#endif
}

X_API int
YAP_StreamToFileNo(Term t)
{
  return(Yap_StreamToFileNo(t));
}

X_API void
YAP_CloseAllOpenStreams(void)
{
  BACKUP_H();

  Yap_CloseStreams(FALSE);

  RECOVER_H();
}

X_API Term
YAP_OpenStream(void *fh, char *name, Term nm, int flags)
{
  Term retv;

  BACKUP_H();

  retv = Yap_OpenStream((FILE *)fh, name, nm, flags);

  RECOVER_H();
  return retv;
}

X_API void
YAP_Throw(Term t)
{
  BACKUP_MACHINE_REGS();
  Yap_JumpToEnv(t);
  RECOVER_MACHINE_REGS();
}

X_API void
YAP_Halt(int i)
{
  Yap_exit(i);
}

X_API CELL *
YAP_TopOfLocalStack(void)
{
  return(ASP);
}

X_API void *
YAP_Predicate(Atom a, unsigned long int arity, int m)
{
  if (arity == 0) {
    return((void *)RepPredProp(PredPropByAtom(a,m)));
  } else {
    Functor f = Yap_MkFunctor(a, arity);
    return((void *)RepPredProp(PredPropByFunc(f,m)));
  }
} 

X_API void
YAP_PredicateInfo(void *p, Atom* a, unsigned long int* arity, Term* m)
{
  PredEntry *pd = (PredEntry *)p;
  if (pd->ArityOfPE) {
    *arity = pd->ArityOfPE;
    *a = NameOfFunctor(pd->FunctorOfPred);
  } else {
    *arity = 0;
    *a = (Atom)(pd->FunctorOfPred);
  }
  if (pd->ModuleOfPred)
    *m = pd->ModuleOfPred;
  else
    *m = TermProlog;
} 

X_API void 
YAP_UserCPredicate(char *name, CPredicate def, unsigned long int arity)
{
  Yap_InitCPred(name, arity, def, UserCPredFlag);
}

X_API void 
YAP_UserBackCPredicate(char *name, CPredicate init, CPredicate cont,
		   unsigned long int arity, unsigned int extra)
{
  Yap_InitCPredBack(name, arity, extra, init, cont, UserCPredFlag);
}

X_API void
YAP_UserCPredicateWithArgs(char *a, CPredicate f, unsigned long int arity, Term mod)
{
  PredEntry *pe;
  Term cm = CurrentModule;
  CurrentModule = mod;
  YAP_UserCPredicate(a,f,arity);
  if (arity == 0) {
    pe = RepPredProp(PredPropByAtom(Yap_LookupAtom(a),mod));
  } else {
    Functor f = Yap_MkFunctor(Yap_LookupAtom(a), arity);
    pe = RepPredProp(PredPropByFunc(f,mod));
  }
  pe->PredFlags |= CArgsPredFlag;
  CurrentModule = cm;
} 

X_API Term
YAP_CurrentModule(void)
{
  return(CurrentModule);
} 

X_API Term
YAP_CreateModule(Atom at)
{
  return Yap_Module(MkAtomTerm(at));
} 

X_API int
YAP_ThreadSelf(void)
{
#if USE_THREADS
  return Yap_thread_self();
#else
  return 0;
#endif
} 

X_API int
YAP_ThreadCreateEngine(thread_attr *attr)
{
#if USE_THREADS
  return Yap_thread_create_engine(attr);
#else
  return FALSE;
#endif
} 

X_API int
YAP_ThreadAttachEngine(int wid)
{
#if USE_THREADS
  return Yap_thread_attach_engine(wid);
#else
  return FALSE;
#endif
} 

X_API int
YAP_ThreadDetachEngine(int wid)
{
#if USE_THREADS
  return Yap_thread_detach_engine(wid);
#else
  return FALSE;
#endif
} 

X_API int
YAP_ThreadDestroyEngine(int wid)
{
#if USE_THREADS
  return Yap_thread_destroy_engine(wid);
#else
  return FALSE;
#endif
} 

