/*************************************************************************
*									 *
*	 YAP Prolog 	@(#)c_interface.h	2.2			 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		c_interface.h						 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	c_interface header file for YAP				 *
*									 *
*************************************************************************/
/**
 * @file c_interface.h
 *
 * @addtogroup ChYInterface
 */
#if  !defined(_c_interface_h) && !defined(_YAP_NOT_INSTALLED_)

#define _c_interface_h 1

#include "YapInterface.h"

#define CELL YAP_CELL

#ifndef Bool
#define Bool YAP_Bool
#endif

#define Int long int

#define flt double

#define Term YAP_Term

#define Functor YAP_Functor

#define Atom YAP_Atom

#define yap_init_args YAP_init_args

#define A(X) YAP_A(X)
#define ARG1 YAP_ARG1
#define ARG2 YAP_ARG2
#define ARG3 YAP_ARG3
#define ARG4 YAP_ARG4
#define ARG5 YAP_ARG5
#define ARG6 YAP_ARG6
#define ARG7 YAP_ARG7
#define ARG8 YAP_ARG8
#define ARG9 YAP_ARG9
#define ARG10 YAP_ARG10
#define ARG11 YAP_ARG11
#define ARG12 YAP_ARG12
#define ARG13 YAP_ARG13
#define ARG14 YAP_ARG14
#define ARG15 YAP_ARG15
#define ARG16 YAP_ARG16

/*  YAP_Term Deref(YAP_Term)  */
#define Deref(t)  YAP_Deref(t)
#define YapDeref(t)  YAP_Deref(t)

/*  YAP_Bool IsVarTerm(YAP_Term) */
#define IsVarTerm(t)  YAP_IsVarTerm(t)
#define YapIsVarTerm(t)  YAP_IsVarTerm(t)

/*  YAP_Bool IsNonVarTerm(YAP_Term) */
#define IsNonVarTerm(t)  YAP_IsNonVarTerm(t)
#define YapIsNonVarTerm(t)  YAP_IsNonVarTerm(t)

/*  YAP_Term  MkVarTerm()  */
#define MkVarTerm()  YAP_MkVarTerm()
#define YapMkVarTerm()  YAP_MkVarTerm()

/*  YAP_Bool IsIntTerm(YAP_Term)  */
#define IsIntTerm(t)  YAP_IsIntTerm(t)
#define YapIsIntTerm(t)  YAP_IsIntTerm(t)

/*  YAP_Bool IsFloatTerm(YAP_Term)  */
#define IsFloatTerm(t)  YAP_IsFloatTerm(t)
#define YapIsFloatTerm(t)  YAP_IsFloatTerm(t)

/*  YAP_Bool IsDbRefTerm(YAP_Term)  */
#define IsDbRefTerm(t)  YAP_IsDbRefTerm(t)
#define YapIsDbRefTerm(t)  YAP_IsDbRefTerm(t)

/*  YAP_Bool IsAtomTerm(YAP_Term)  */
#define IsAtomTerm(t)  YAP_IsAtomTerm(t)
#define YapIsAtomTerm(t)  YAP_IsAtomTerm(t)

/*  YAP_Bool IsPairTerm(YAP_Term)  */
#define IsPairTerm(t)  YAP_IsPairTerm(t)
#define YapIsPairTerm(t)  YAP_IsPairTerm(t)

/*  YAP_Bool IsApplTerm(YAP_Term)  */
#define IsApplTerm(t)  YAP_IsApplTerm(t)
#define YapIsApplTerm(t)  YAP_IsApplTerm(t)

/*    Term MkIntTerm(YAP_Int)  */
#define MkIntTerm(t)  YAP_MkIntTerm(t)
#define YapMkIntTerm(t)  YAP_MkIntTerm(t)

/*    YAP_Int  IntOfTerm(Term) */
#define IntOfTerm(t)  YAP_IntOfTerm(t)
#define YapIntOfTerm(t)  YAP_IntOfTerm(t)

/*    Term MkFloatTerm(YAP_flt)  */
#define MkFloatTerm(f)  YAP_MkFloatTerm(f)
#define YapMkFloatTerm(f)  YAP_MkFloatTerm(f)

/*    YAP_flt  FloatOfTerm(YAP_Term) */
#define FloatOfTerm(t)  YAP_FloatOfTerm(t)
#define YapFloatOfTerm(t)  YAP_FloatOfTerm(t)

/*    Term MkAtomTerm(Atom)  */
#define MkAtomTerm(a)  YAP_MkAtomTerm(a)
#define YapMkAtomTerm(a)  YAP_MkAtomTerm(a)

/*    YAP_Atom  AtomOfTerm(Term) */
#define AtomOfTerm(t)  YAP_AtomOfTerm(t)
#define YapAtomOfTerm(t)  YAP_AtomOfTerm(t)

/*    YAP_Atom  LookupAtom(char *) */
#define LookupAtom(s)  YAP_LookupAtom(s)
#define YapLookupAtom(s)  YAP_LookupAtom(s)

/*    YAP_Atom  FullLookupAtom(char *) */
#define FullLookupAtom(s)  YAP_FullLookupAtom(s)
#define YapFullLookupAtom(s)  YAP_FullLookupAtom(s)

/*    char* AtomName(YAP_Atom) */
#define AtomName(a)  YAP_AtomName(a)
#define YapAtomName(a)  YAP_AtomName(a)

/*    YAP_Term  MkPairTerm(YAP_Term Head, YAP_Term Tail) */
#define MkPairTerm(h,t)  YAP_MkPairTerm(h,t)
#define YapMkPairTerm(h,t)  YAP_MkPairTerm(h,t)

/*    YAP_Term  MkNewPairTerm(void) */
#define MkNewPairTerm()  YAP_MkNewPairTerm()
#define YapMkNewPairTerm()  YAP_MkNewPairTerm()

/*    Term  HeadOfTerm(Term)  */
#define HeadOfTerm(t)  YAP_HeadOfTerm(t)
#define YapHeadOfTerm(t)  YAP_HeadOfTerm(t)

/*    Term  TailOfTerm(Term)  */
#define TailOfTerm(t)  YAP_TailOfTerm(t)
#define YapTailOfTerm(t)  YAP_TailOfTerm(t)

/*    YAP_Term     MkApplTerm(YAP_Functor f, int n, YAP_Term[] args) */
#define MkApplTerm(f,i,ts)  YAP_MkApplTerm(f,i,ts)
#define YapMkApplTerm(f,i,ts)  YAP_MkApplTerm(f,i,ts)

/*    YAP_Term     MkNewApplTerm(YAP_Functor f, int n) */
#define MkNewApplTerm(f,i)  YAP_MkNewApplTerm(f,i)
#define YapMkNewApplTerm(f,i)  YAP_MkNewApplTerm(f,i)

/*    YAP_Functor  YAP_FunctorOfTerm(Term)  */
#define FunctorOfTerm(t)  YAP_FunctorOfTerm(t)
#define YapFunctorOfTerm(t)  YAP_FunctorOfTerm(t)

/*    YAP_Term     ArgOfTerm(int argno,YAP_Term t) */
#define ArgOfTerm(i,t)  YAP_ArgOfTerm(i,t)
#define YapArgOfTerm(i,t)  YAP_ArgOfTerm(i,t)

/*    YAP_Functor  MkFunctor(YAP_Atom a,int arity) */
#define MkFunctor(a,i)  YAP_MkFunctor(a,i)
#define YapMkFunctor(a,i)  YAP_MkFunctor(a,i)

/*    YAP_Atom     NameOfFunctor(Functor) */
#define NameOfFunctor(f)  YAP_NameOfFunctor(f)
#define YapNameOfFunctor(f)  YAP_NameOfFunctor(f)

/*    YAP_Int    YAP_ArityOfFunctor(Functor) */
#define ArityOfFunctor(f)  YAP_ArityOfFunctor(f)
#define YapArityOfFunctor(f)  YAP_ArityOfFunctor(f)

#define PRESERVE_DATA(ptr, type) (ptr = (type *)YAP_ExtraSpace())
#define PRESERVED_DATA(ptr, type) (ptr = (type *)YAP_ExtraSpace())
#define PRESERVED_DATA_CUT(ptr,type) (ptr = (type *)YAP_ExtraSpaceCut())

/*   YAP_Int      unify(YAP_Term a, YAP_Term b) */
#define unify(t1,t2)  YAP_Unify(t1, t2)
#define YapUnify(t1,t2)  YAP_Unify(t1, t2)

/*  void UserCPredicate(char *name, int *fn(), int arity) */
#define UserCPredicate(s,f,i)  YAP_UserCPredicate(s,f,i);

/*  void UserBackCPredicate(char *name, int *init(), int *cont(), int
    arity, int extra) */
#define UserBackCPredicate(s,f1,f2,i,i2)  YAP_UserBackCPredicate(s,f1,f2,i,i2)

/*  void UserCPredicate(char *name, int *fn(), int arity) */
#define UserCPredicateWithArgs(s,f,i1,i2)  YAP_UserCPredicateWithArgs(s,f,i1,i2)
/*  void CallProlog(YAP_Term t) */
#define CallProlog(t)  YAP_CallProlog(t)
#define YapCallProlog(t)  YAP_CallProlog(t)

/*  void cut_fail(void) */
#define cut_fail()  YAP_cut_fail()

/*  void cut_succeed(void) */
#define cut_succeed()  YAP_cut_succeed()

/*  void *AllocSpaceFromYap(int) */
#define AllocSpaceFromYap(s)  YAP_AllocSpaceFromYap(s)

/*  void FreeSpaceFromYap(void *) */
#define FreeSpaceFromYap(s)  YAP_FreeSpaceFromYap(s)

/*  int YAP_RunGoal(YAP_Term) */
#define RunGoal(t)  YAP_RunGoal(t)
#define YapRunGoal(t)  YAP_RunGoal(t)

/*  int YAP_RestartGoal(void) */
#define RestartGoal()  YAP_RestartGoal()
#define YapRestartGoal()  YAP_RestartGoal()

/*  int YAP_ContinueGoal(void) */
#define ContinueGoal()  YAP_ContinueGoal()
#define YapContinueGoal()  YAP_ContinueGoal()

/*  void YAP_PruneGoal(void) */
#define PruneGoal()  YAP_PruneGoal()
#define YapPruneGoal()  YAP_PruneGoal()

/*  int YAP_GoalHasException(void) */
#define GoalHasException(tp)  YAP_GoalHasException(tp)
#define YapGoalHasException(tp)  YAP_GoalHasException(tp)

/*  int YAP_Reset(void) */
#define YapReset()  YAP_Reset()

/*  void YAP_Error(char *) */
#define YapError(s)  YAP_Error(s)

/*  YAP_Term YAP_Read(int (*)(void)) */
#define YapRead(f)  YAP_Read(f);

/*  void YAP_Write(YAP_Term,void (*)(int),int) */
#define YapWrite(t,f)  YAP_Write(t,f);

/*  char *YAP_CompileClause(YAP_Term) */
#define CompileClause(t)  YAP_CompileClause(t)
#define YapCompileClause(t)  YAP_CompileClause(t)

/*  int YAP_Init(YAP_init_args *) */
#define YapInit(as)  YAP_Init(as)

/*  int YAP_FastInit(char *) */
#define YapFastInit(s)  YAP_FastInit(s)

/*  int YAP_InitConsult(int, char *) */
#define YapInitConsult(i,s)  YAP_InitConsult(i,s)

/*  int YAP_StartConsult(int, char *) */
#define YapEndConsult()  YAP_EndConsult()

/*  void YAP_Exit(int) */
#define YapExit(code)  YAP_Exit(code)

/*  void YAP_PutValue(YAP_Atom, YAP_Term) */
#define PutValue()  YAP_PutValue(a, t)
#define YapPutValue()  YAP_PutValue(a, t)

/*  YAP_Term YAP_GetValue(YAP_Atom) */
#define GetValue(a)  YAP_GetValue(a)
#define YapGetValue(a)  YAP_GetValue(a)

/*  int StringToBuffer(YAP_Term,char *,unsigned int) */
#define StringToBuffer(t,s,l)  YAP_StringToBuffer(t,s,l)
#define YapStringToBuffer(t,s,l)  YAP_StringToBuffer(t,s,l)

/*  int BufferToString(char *) */
#define BufferToString(s)  YAP_BufferToString(s)
#define YapBufferToString(s)  YAP_BufferToString(s)

/*  int BufferToAtomList(char *) */
#define BufferToAtomList(s)  YAP_BufferToAtomList(s)
#define YapBufferToAtomList(s)  YAP_BufferToAtomList(s)

/*  void YAP_InitSocks(char *,long) */
#define InitSocks(s,l)  YAP_InitSocks(s,l)
#define YapInitSocks(s,l)  YAP_InitSocks(s,l)

#ifdef  SFUNC

#define SFArity  0
#define ArgsOfSFTerm(s,t) YAP_ArgsOfSFTerm(s,t)

extern  MkSFTerm(t) YAP_MkSFTerm(t)

#endif /* SFUNC */

/*  YAP_Term  YAP_SetOutputMessage()  */
#define YapSetOutputMessage(s)  YAP_SetOutputMessage(s)

/*  YAP_Term  YAP_SetOutputMessage()  */
#define YapStreamToFileNo(st)  YAP_StreamToFileNo(st)

/*  YAP_Term  YAP_SetOutputMessage()  */
#define YapCloseAllOpenStreams()  YAP_CloseAllOpenStreams()

/*  YAP_Term  YAP_OpenStream()  */
#define YapOpenStream(st, s, t, i)  YAP_OpenStream(st, s, t, i)

#endif
