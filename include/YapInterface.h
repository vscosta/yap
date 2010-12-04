/*************************************************************************
*									 *
*	 YAP Prolog 	@(#)c_interface.h	2.2			 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapInterface.h						 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	c_interface header file for YAP				 *
*									 *
*************************************************************************/

/*******************  IMPORTANT ********************
   Due to a limitation of the DecStation loader any function (including
   library functions) which is linked to yap can not be called directly
   from C code loaded dynamically.
      To go around this problem we adopted the solution of calling such
   functions indirectly
****************************************************/

#ifndef _yap_c_interface_h

#define _yap_c_interface_h 1

#define __YAP_PROLOG__ 1

#ifndef YAPVERSION
#define YAPVERSION 60000
#endif

#include "yap_structs.h"

#if HAVE_STDARG_H
#include <stdarg.h>
#endif

#include <wchar.h>

/*
   __BEGIN_DECLS should be used at the beginning of the C declarations,
   so that C++ compilers don't mangle their names.  __END_DECLS is used
   at the end of C declarations.
*/
#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS /* empty */
# define __END_DECLS /* empty */
#endif /* _cplusplus */

__BEGIN_DECLS

#if defined(_MSC_VER) && defined(YAP_EXPORTS)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif

/* Primitive Functions */

#define YAP_Deref(t)  (t)
extern X_API YAP_Term PROTO(YAP_A,(int));
#define YAP_ARG1	YAP_A(1)
#define YAP_ARG2	YAP_A(2)
#define YAP_ARG3	YAP_A(3)
#define YAP_ARG4	YAP_A(4)
#define YAP_ARG5	YAP_A(5)
#define YAP_ARG6	YAP_A(6)
#define YAP_ARG7	YAP_A(7)
#define YAP_ARG8	YAP_A(8)
#define YAP_ARG9	YAP_A(9)
#define YAP_ARG10	YAP_A(10)
#define YAP_ARG11	YAP_A(11)
#define YAP_ARG12	YAP_A(12)
#define YAP_ARG13	YAP_A(13)
#define YAP_ARG14	YAP_A(14)
#define YAP_ARG15	YAP_A(15)
#define YAP_ARG16	YAP_A(16)

/*  YAP_Bool IsVarTerm(YAP_Term) */
extern X_API YAP_Bool PROTO(YAP_IsVarTerm,(YAP_Term));

/*  YAP_Bool IsNonVarTerm(YAP_Term) */
extern X_API YAP_Bool PROTO(YAP_IsNonVarTerm,(YAP_Term));

/*  YAP_Term  MkVarTerm()  */
extern X_API YAP_Term PROTO(YAP_MkVarTerm,(void));

/*  YAP_Bool IsIntTerm(YAP_Term)  */
extern X_API YAP_Bool PROTO(YAP_IsIntTerm,(YAP_Term));

/*  YAP_Bool IsLongIntTerm(YAP_Term)  */
extern X_API YAP_Bool PROTO(YAP_IsLongIntTerm,(YAP_Term));

/*  YAP_Bool IsBigNumTerm(YAP_Term)  */
extern X_API YAP_Bool PROTO(YAP_IsBigNumTerm,(YAP_Term));

/*  YAP_Bool IsFloatTerm(YAP_Term)  */
extern X_API YAP_Bool PROTO(YAP_IsFloatTerm,(YAP_Term));

/*  YAP_Bool IsDbRefTerm(YAP_Term)  */
extern X_API YAP_Bool PROTO(YAP_IsDbRefTerm,(YAP_Term));

/*  YAP_Bool IsAtomTerm(YAP_Term)  */
extern X_API YAP_Bool PROTO(YAP_IsAtomTerm,(YAP_Term));

/*  YAP_Bool IsPairTerm(YAP_Term)  */
extern X_API YAP_Bool PROTO(YAP_IsPairTerm,(YAP_Term));

/*  YAP_Bool IsApplTerm(YAP_Term)  */
extern X_API YAP_Bool PROTO(YAP_IsApplTerm,(YAP_Term));

/*    Term MkIntTerm(YAP_Int)  */
extern X_API YAP_Term PROTO(YAP_MkIntTerm,(YAP_Int));

/*    Term MkBigNumTerm(void *)  */
extern X_API YAP_Term PROTO(YAP_MkBigNumTerm,(void *));

/*    YAP_Int  IntOfTerm(Term) */
extern X_API YAP_Int PROTO(YAP_IntOfTerm,(YAP_Term));

/*    void *  BigNumOfTerm(Term) */
extern X_API void *PROTO(YAP_BigNumOfTerm,(YAP_Term, void *));

/*    Term MkFloatTerm(YAP_Float)  */
extern X_API YAP_Term PROTO(YAP_MkFloatTerm,(YAP_Float));

/*    YAP_Float  FloatOfTerm(YAP_Term) */
extern X_API YAP_Float PROTO(YAP_FloatOfTerm,(YAP_Term));

/*    Term MkAtomTerm(Atom)  */
extern X_API YAP_Term PROTO(YAP_MkAtomTerm,(YAP_Atom));

/*    YAP_Atom  AtomOfTerm(Term) */
extern X_API YAP_Atom PROTO(YAP_AtomOfTerm,(YAP_Term));

/*    YAP_Atom  LookupAtom(const char *) */
extern X_API YAP_Atom PROTO(YAP_LookupAtom,(CONST char *));

/*    YAP_Atom  LookupWideAtom(const wchar_t *) */
extern X_API YAP_Atom PROTO(YAP_LookupWideAtom,(CONST wchar_t *));

/*    YAP_Atom  FullLookupAtom(const char *) */
extern X_API YAP_Atom PROTO(YAP_FullLookupAtom,(CONST char *));

/*    int  AtomNameLength(Atom) */
extern X_API int PROTO(YAP_AtomNameLength,(YAP_Atom));

/*    const char* IsWideAtom(YAP_Atom) */
extern X_API int *PROTO(YAP_IsWideAtom,(YAP_Atom));

/*    const char* AtomName(YAP_Atom) */
extern X_API CONST char *PROTO(YAP_AtomName,(YAP_Atom));

/*    const wchar_t* AtomWideName(YAP_Atom) */
extern X_API CONST wchar_t *PROTO(YAP_WideAtomName,(YAP_Atom));

/*    YAP_Term  MkPairTerm(YAP_Term Head, YAP_Term Tail) */
extern X_API YAP_Term PROTO(YAP_MkPairTerm,(YAP_Term,YAP_Term));

/*    YAP_Term  MkNewPairTerm(void) */
extern X_API YAP_Term PROTO(YAP_MkNewPairTerm,(void));

/*    Term  HeadOfTerm(Term)  */
extern X_API YAP_Term PROTO(YAP_HeadOfTerm,(YAP_Term));

/*    Term  TailOfTerm(Term)  */
extern X_API YAP_Term PROTO(YAP_TailOfTerm,(YAP_Term));

/*    Term  TailOfTerm(Term)  */
extern X_API YAP_Term PROTO(YAP_TermNil,(void));

/*    YAP_Term     MkApplTerm(YAP_Functor f, unsigned int n, YAP_Term[] args) */
extern X_API YAP_Term PROTO(YAP_MkApplTerm,(YAP_Functor,unsigned int,YAP_Term *));

/*    YAP_Term     MkNewApplTerm(YAP_Functor f, unsigned int n) */
extern X_API YAP_Term PROTO(YAP_MkNewApplTerm,(YAP_Functor,unsigned int));

/*    YAP_Functor  YAP_FunctorOfTerm(Term)  */
extern X_API YAP_Functor PROTO(YAP_FunctorOfTerm,(YAP_Term));

/*    YAP_Term     ArgOfTerm(unsigned int argno,YAP_Term t) */
extern X_API YAP_Term PROTO(YAP_ArgOfTerm,(unsigned int,YAP_Term));

/*    YAP_Term    *ArgsOfTerm(YAP_Term t) */
extern X_API YAP_Term *PROTO(YAP_ArgsOfTerm,(YAP_Term));

/*    YAP_Functor  MkFunctor(YAP_Atom a,int arity) */
extern X_API YAP_Functor PROTO(YAP_MkFunctor,(YAP_Atom,unsigned int));

/*    YAP_Atom     NameOfFunctor(Functor) */
extern X_API YAP_Atom PROTO(YAP_NameOfFunctor,(YAP_Functor));

/*    unsigned unsigned int     YAP_ArityOfFunctor(Functor) */
extern X_API unsigned int PROTO(YAP_ArityOfFunctor,(YAP_Functor));

/*  void ExtraSpace(void) */
extern X_API void *PROTO(YAP_ExtraSpace,(void));
extern X_API void *PROTO(YAP_ExtraSpaceCut,(void));

#define YAP_PRESERVE_DATA(ptr, type) (ptr = (type *)YAP_ExtraSpace())
#define YAP_PRESERVED_DATA(ptr, type) (ptr = (type *)YAP_ExtraSpace())
#define YAP_PRESERVED_DATA_CUT(ptr,type) (ptr = (type *)YAP_ExtraSpaceCut())

/*   YAP_Bool      unify(YAP_Term a, YAP_Term b) */
extern X_API YAP_Bool PROTO(YAP_Unify,(YAP_Term, YAP_Term));

/*  void UserCPredicate(const char *name, int *fn(), int arity) */
extern X_API void PROTO(YAP_UserCPredicate,(CONST char *, YAP_Bool (*)(void), unsigned int));

/*  void UserCPredicateWithArgs(const char *name, int *fn(), unsigned int arity) */
extern X_API void PROTO(YAP_UserCPredicateWithArgs,(CONST char *, YAP_Bool (*)(void), YAP_Arity, YAP_Term));

/*  void UserBackCPredicate(const char *name, int *init(), int *cont(), int
    arity, int extra) */
extern X_API void PROTO(YAP_UserBackCPredicate,(CONST char *, YAP_Bool (*)(void), YAP_Bool (*)(void), YAP_Arity, unsigned int));

/*  void UserBackCPredicate(char *name, int *init(), int *cont(), int *cut(), int
    arity, int extra) */
extern X_API void PROTO(YAP_UserBackCutCPredicate,(char *, YAP_Bool (*)(void), YAP_Bool (*)(void), YAP_Bool (*)(void), YAP_Arity, unsigned int));

/*  void CallProlog(YAP_Term t) */
extern X_API YAP_Bool PROTO(YAP_CallProlog,(YAP_Term t));

/*  void cut_fail(void) */
extern X_API void PROTO(YAP_cut_up,(void));

#define YAP_cut_succeed() { YAP_cut_up(); return TRUE; }

#define YAP_cut_fail() { YAP_cut_up(); return FALSE; }

/*  void *AllocSpaceFromYAP_(int) */
extern X_API void *PROTO(YAP_AllocSpaceFromYap,(unsigned int));

/*  void *ReallocSpaceFromYAP_(void*,int) */
extern X_API void *PROTO(YAP_ReallocSpaceFromYap,(void*,unsigned int));

/*  void FreeSpaceFromYAP_(void *) */
extern X_API void PROTO(YAP_FreeSpaceFromYap,(void *));

/*  int YAP_RunGoal(YAP_Term) */
extern X_API YAP_Term PROTO(YAP_RunGoal,(YAP_Term));

/*  int YAP_RunGoalOnce(YAP_Term) */
extern X_API YAP_Term PROTO(YAP_RunGoalOnce,(YAP_Term));

/*  int YAP_RestartGoal(void) */
extern X_API YAP_Bool PROTO(YAP_RestartGoal,(void));

/*  int YAP_ShutdownGoal(int) */
extern X_API YAP_Bool PROTO(YAP_ShutdownGoal,(int));

/*  int YAP_ContinueGoal(void) */
extern X_API YAP_Bool PROTO(YAP_ContinueGoal,(void));


/*  void YAP_PruneGoal(void) */
extern X_API void PROTO(YAP_PruneGoal,(void));

/*  int YAP_FunctorToPred(struct pred_entry *, YAP_Term *) */
extern X_API YAP_PredEntryPtr PROTO(YAP_FunctorToPred,(YAP_Functor));

/*  int YAP_AtomToPred(struct pred_entry *, YAP_Term *) */
extern X_API YAP_PredEntryPtr PROTO(YAP_AtomToPred,(YAP_Atom));

/*  int YAP_EnterGoal(void) */
extern X_API YAP_Bool PROTO(YAP_EnterGoal,(YAP_PredEntryPtr, YAP_Term *, YAP_dogoalinfo *));

/*  int YAP_RetryGoal(void) */
extern X_API YAP_Bool PROTO(YAP_RetryGoal,(YAP_dogoalinfo *));

/*  int YAP_LeaveGoal(void) */
extern X_API YAP_Bool PROTO(YAP_LeaveGoal,(int, YAP_dogoalinfo *));

/*  int YAP_GoalHasException(YAP_Term *) */
extern X_API YAP_Bool PROTO(YAP_GoalHasException,(YAP_Term *));

/*  void YAP_ClearExceptions(void) */
extern X_API void PROTO(YAP_ClearExceptions,(void));

/*  int YAP_Reset(void) */
extern X_API void PROTO(YAP_Reset,(void));

/*  void YAP_Error(int, YAP_Term, const char *,...) */
extern X_API void PROTO(YAP_Error,(int, YAP_Term, CONST char *, ...));

/*  YAP_Term YAP_Read(int (*)(void)) */
extern X_API YAP_Term PROTO(YAP_Read,(int (*)(void)));

/*  void YAP_Write(YAP_Term,void (*)(int),int) */
extern X_API void PROTO(YAP_Write,(YAP_Term,void (*)(int),int));

/*  void YAP_WriteBufffer(YAP_Term,char *,unsgined int,int) */
extern X_API void PROTO(YAP_WriteBuffer,(YAP_Term,char *,unsigned int,int));

/*  void YAP_Term(YAP_Term) */
extern X_API YAP_Term PROTO(YAP_CopyTerm,(YAP_Term));

/*  char *YAP_CompileClause(YAP_Term) */
extern X_API char *PROTO(YAP_CompileClause,(YAP_Term));

/*  int YAP_Init(YAP_init_args *) */
extern X_API int PROTO(YAP_Init,(YAP_init_args *));

/*  int YAP_FastInit(const char *) */
extern X_API int PROTO(YAP_FastInit,(CONST char *));

/*  int YAP_InitConsult(int, const char *) */
extern X_API int PROTO(YAP_InitConsult,(int, CONST char *));

/*  int YAP_EndConsult(void) */
extern X_API int PROTO(YAP_EndConsult,(void));

/*  void YAP_Exit(int) */
extern X_API void PROTO(YAP_Exit,(int));

/*  void YAP_PutValue(YAP_Atom, YAP_Term) */
extern X_API void PROTO(YAP_PutValue,(YAP_Atom, YAP_Term));

/*  YAP_Term YAP_GetValue(YAP_Atom) */
extern X_API YAP_Term PROTO(YAP_GetValue,(YAP_Atom));

/*  int StringToBuffer(YAP_Term,char *,unsigned int) */
extern X_API int PROTO(YAP_StringToBuffer,(YAP_Term,char *,unsigned int));

/*  int BufferToString(const char *) */
extern X_API YAP_Term PROTO(YAP_BufferToString,(CONST char *));

/*  int BufferToString(const char *) */
extern X_API YAP_Term PROTO(YAP_NBufferToString,(CONST char *, size_t len));

/*  int BufferToString(const char *) */
extern X_API YAP_Term PROTO(YAP_WideBufferToString,(CONST wchar_t *));

/*  int BufferToString(const char *) */
extern X_API YAP_Term PROTO(YAP_NWideBufferToString,(CONST wchar_t *, size_t len));

/*  int BufferToAtomList(const char *) */
extern X_API YAP_Term PROTO(YAP_BufferToAtomList,(CONST char *));

/*  int BufferToAtomList(const char *) */
extern X_API YAP_Term PROTO(YAP_NBufferToAtomList,(CONST char *, size_t len));

/*  int BufferToAtomList(const char *) */
extern X_API YAP_Term PROTO(YAP_WideBufferToAtomList,(CONST wchar_t *));

/*  int BufferToAtomList(const char *) */
extern X_API YAP_Term PROTO(YAP_NWideBufferToAtomList,(CONST wchar_t *, size_t len));

/*  int BufferToDiffList(const char *) */
extern X_API YAP_Term PROTO(YAP_NWideBufferToAtomDiffList,(CONST wchar_t *, YAP_Term, size_t len));

/*  int BufferToDiffList(const char *) */
extern X_API YAP_Term PROTO(YAP_BufferToDiffList,(CONST char *));

/*  int BufferToDiffList(const char *) */
extern X_API YAP_Term PROTO(YAP_NBufferToDiffList,(CONST char *, size_t len));

/*  int BufferToDiffList(const char *) */
extern X_API YAP_Term PROTO(YAP_WideBufferToDiffList,(CONST wchar_t *));

/*  int BufferToDiffList(const char *) */
extern X_API YAP_Term PROTO(YAP_NWideBufferToDiffList,(CONST wchar_t *, YAP_Term, size_t len));

/* YAP_Term BufferToTerm(const char *) */
extern X_API YAP_Term PROTO(YAP_ReadBuffer,(CONST char *,YAP_Term *));

/*  void YAP_InitSocks(const char *,long) */
extern X_API int PROTO(YAP_InitSocks,(CONST char *,long));

#ifdef  SFUNC

#define SFArity  0
extern X_API YAP_Term *ArgsOfSFTerm();

extern X_API YAP_Term MkSFTerm();

#endif /* SFUNC */


extern X_API void PROTO(YAP_SetOutputMessage,(void));

extern X_API int PROTO(YAP_StreamToFileNo,(YAP_Term));

extern X_API void PROTO(YAP_CloseAllOpenStreams,(void));

extern X_API void PROTO(YAP_FlushAllStreams,(void));

#define YAP_INPUT_STREAM	0x01
#define YAP_OUTPUT_STREAM	0x02
#define YAP_APPEND_STREAM	0x04
#define YAP_PIPE_STREAM 	0x08
#define YAP_TTY_STREAM	 	0x10
#define YAP_POPEN_STREAM	0x20
#define YAP_BINARY_STREAM	0x40
#define YAP_SEEKABLE_STREAM	0x80

/*  YAP_Term  YAP_OpenStream()  */
extern X_API YAP_Term PROTO(YAP_OpenStream,(void *, CONST char *, YAP_Term, int));

/*  YAP_Term  *YAP_NewSlots()  */
extern X_API YAP_Int PROTO(YAP_NewSlots,(int));

/*  YAP_Int  YAP_CurrentSlot()  */
extern X_API YAP_Int PROTO(YAP_CurrentSlot,(void));

/*  YAP_Term  *YAP_InitSlot()  */
extern X_API YAP_Int PROTO(YAP_InitSlot,(YAP_Term));

/*  YAP_Term  YAP_GetFromSlots(t)  */
extern X_API YAP_Term PROTO(YAP_GetFromSlot,(YAP_Int));

/*  YAP_Term  YAP_AddressFromSlots(t)  */
extern X_API YAP_Term *PROTO(YAP_AddressFromSlot,(YAP_Int));

/*  YAP_Term  YAP_PutInSlots(t)  */
extern X_API void PROTO(YAP_PutInSlot,(YAP_Int, YAP_Term));

/*  void  YAP_RecoverSlots()  */
extern X_API int PROTO(YAP_RecoverSlots,(int));

/*  void  YAP_RecoverSlots()  */
extern X_API YAP_Int PROTO(YAP_ArgsToSlots,(int));

/*  void  YAP_RecoverSlots()  */
extern X_API void PROTO(YAP_SlotsToArgs,(int, YAP_Int));

/*  void  YAP_Throw()  */
extern X_API void PROTO(YAP_Throw,(YAP_Term));

/*  void  YAP_AsyncThrow()  */
extern X_API void PROTO(YAP_AsyncThrow,(YAP_Term));

/*  int  YAP_LookupModule()  */
#define YAP_LookupModule(T)  (T)

#define YAP_ModuleName(mod) (mod)

/*  int  YAP_Halt()  */
extern X_API int  PROTO(YAP_Halt,(int));

/*  int  YAP_TopOfLocalStack()  */
extern X_API YAP_Term  *PROTO(YAP_TopOfLocalStack,(void));

/*  int  YAP_Predicate()  */
extern X_API void  *PROTO(YAP_Predicate,(YAP_Atom,YAP_Arity,YAP_Term));

/*  int  YAP_Predicate()  */
extern X_API void  PROTO(YAP_PredicateInfo,(void *,YAP_Atom *,YAP_Arity*,YAP_Module*));

/*  int  YAP_CurrentModule()  */
extern X_API YAP_Module  PROTO(YAP_CurrentModule,(void));

/*  int  YAP_CurrentModule()  */
extern X_API YAP_Module  PROTO(YAP_CreateModule,(YAP_Atom));

/*  int  YAP_StripModule()  */
extern X_API YAP_Term  PROTO(YAP_StripModule,(YAP_Term, YAP_Module *));

/*  int  YAP_AtomGetHold(YAP_Atom)  */
extern X_API int  PROTO(YAP_AtomGetHold,(YAP_Atom));

/*  int  YAP_AtomReleaseHold(YAP_Atom)  */
extern X_API int  PROTO(YAP_AtomReleaseHold,(YAP_Atom));

/*  void  YAP_AtomReleaseHold(YAP_Atom)  */
extern X_API YAP_agc_hook  PROTO(YAP_AGCRegisterHook,(YAP_agc_hook));

/*  void  YAP_AtomReleaseHold(YAP_Atom)  */
extern X_API int  PROTO(YAP_HaltRegisterHook,(YAP_halt_hook, void *));

/*  char *YAP_cwd(void)  */
extern X_API char *  PROTO(YAP_cwd,(void));

/* thread stuff */
extern X_API int PROTO(YAP_ThreadSelf,(void));
extern X_API int PROTO(YAP_ThreadCreateEngine,(YAP_thread_attr *));
extern X_API int PROTO(YAP_ThreadAttachEngine,(int));
extern X_API int PROTO(YAP_ThreadDetachEngine,(int));
extern X_API int PROTO(YAP_ThreadDestroyEngine,(int));

/* blob stuff */
extern X_API YAP_Term PROTO(YAP_MkBlobTerm,(unsigned int));
extern X_API void    *PROTO(YAP_BlobOfTerm,(YAP_Term));

/*  term comparison */
extern X_API int  PROTO(YAP_CompareTerms,(YAP_Term, YAP_Term));

/*  list construction */
extern X_API YAP_Term     PROTO(YAP_OpenList,(int));
extern X_API YAP_Term     PROTO(YAP_ExtendList,(YAP_Term, YAP_Term));
extern X_API int          PROTO(YAP_CloseList,(YAP_Term, YAP_Term));

/*  attributed variables */
extern X_API int	PROTO(YAP_IsAttVar,(YAP_Term));
extern X_API YAP_Term	PROTO(YAP_AttsOfVar,(YAP_Term));

/*  stream info */
extern X_API void      *PROTO(YAP_FileDescriptorFromStream,(YAP_Term));
extern X_API int        PROTO(YAP_FileNoFromStream,(YAP_Term));

/*  store and recover terms */
extern X_API void      *PROTO(YAP_Record,(YAP_Term));
extern X_API YAP_Term   PROTO(YAP_Recorded,(void *));
extern X_API int        PROTO(YAP_Erase,(void *));

/*  term utilities */
extern X_API int        PROTO(YAP_Variant,(YAP_Term,YAP_Term));
extern X_API int        PROTO(YAP_ExactlyEqual,(YAP_Term,YAP_Term));
extern X_API YAP_Int    PROTO(YAP_TermHash,(YAP_Term, YAP_Int, YAP_Int, int));

/*  stack expansion control */
extern X_API int        PROTO(YAP_SetYAPFlag,(yap_flag_t,int));

#define YAP_InitCPred(N,A,F)  YAP_UserCPredicate(N,F,A)

__END_DECLS

#endif

