/*************************************************************************
 *									 *
 *	 YAP Prolog 	@(#)c_interface.h	2.2			 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		YapInterface.h *
 * Last rev:	19/2/88							 *
 * mods: *
 * comments:	c_interface header file for YAP				 *
 *									 *
 *************************************************************************/

/**

   @file YapInterface.h

@addtogroup ChYInterface
   @{

   @brief C-Interface to YAP.

The following routines export the YAP internals and architecture.

*/

#ifndef _yap_c_interface_h

#define _yap_c_interface_h 1

#include "YapConfig.h"

#define __YAP_PROLOG__ 1

#ifndef YAPVERSION
#define YAPVERSION YAP_NUMERIC_VERSION
#endif

#include "YapDefs.h"

#if HAVE_STDARG_H
#include <stdarg.h>
#endif

#if HAVE_STDBOOL_H
#include <stdbool.h>
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
#define __BEGIN_DECLS extern "C" {
#define __END_DECLS }
#else
#define __BEGIN_DECLS /* empty */
#define __END_DECLS   /* empty */
#endif                /* _cplusplus */

__BEGIN_DECLS

/**
 * X_API macro
 *
 * @brief declares the symbol as to be exported/imported from a
 * DLL. It is mostly ignored in Linux, but honored in WIN32.
 *

 * @return
 */

#include "YapFormat.h"

/* Primitive Functions */

// Basic operation that follows a pointer chain.
#define YAP_Deref(t) (t)

X_API
extern YAP_Term YAP_A(int);
#define YAP_ARG1 YAP_A(1)
#define YAP_ARG2 YAP_A(2)
#define YAP_ARG3 YAP_A(3)
#define YAP_ARG4 YAP_A(4)
#define YAP_ARG5 YAP_A(5)
#define YAP_ARG6 YAP_A(6)
#define YAP_ARG7 YAP_A(7)
#define YAP_ARG8 YAP_A(8)
#define YAP_ARG9 YAP_A(9)
#define YAP_ARG10 YAP_A(10)
#define YAP_ARG11 YAP_A(11)
#define YAP_ARG12 YAP_A(12)
#define YAP_ARG13 YAP_A(13)
#define YAP_ARG14 YAP_A(14)
#define YAP_ARG15 YAP_A(15)
#define YAP_ARG16 YAP_A(16)

X_API
extern YAP_Term YAP_SetA(int, YAP_Term);

/*  YAP_Bool IsVarTerm(YAP_Term) */
extern X_API YAP_Bool YAP_IsVarTerm(YAP_Term);

/*  YAP_Bool IsNonVarTerm(YAP_Term) */
extern X_API YAP_Bool YAP_IsNonVarTerm(YAP_Term);

/*  YAP_Term  MkVarTerm()  */
extern X_API YAP_Term YAP_MkVarTerm(void);

/*  YAP_Bool IsIntTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsIntTerm(YAP_Term);

/*  YAP_Bool IsLongIntTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsLongIntTerm(YAP_Term);

/*  YAP_Bool IsBigNumTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsBigNumTerm(YAP_Term);

/*  YAP_Bool IsRationalTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsRationalTerm(YAP_Term);

/*  YAP_Bool IsFloatTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsFloatTerm(YAP_Term);

/*  YAP_Bool IsNumberTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsNumberTerm(YAP_Term);

/*  YAP_Bool IsDbRefTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsDbRefTerm(YAP_Term);

/*  YAP_Bool IsAtomTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsAtomTerm(YAP_Term);

/*  YAP_Bool IsAtomTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsStringTerm(YAP_Term);

/*  YAP_Bool IsPairTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsPairTerm(YAP_Term);

/*  YAP_Bool IsApplTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsApplTerm(YAP_Term);

/*  YAP_Bool IsCompoundTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsCompoundTerm(YAP_Term);

/*    Term MkIntTerm(YAP_Int)  */
extern X_API YAP_Term YAP_MkIntTerm(YAP_Int);

/*    Term MkIntTerm(YAP_Int)  */
extern X_API YAP_Term YAP_MkStringTerm(const char *);

/*    Term MkIntTerm(YAP_Int)  */
extern X_API YAP_Term YAP_MkUnsignedStringTerm(const unsigned char *);

/*    Term MkBigNumTerm(void *)  */
extern X_API YAP_Term YAP_MkBigNumTerm(void *);

/*    Term MkRationalTerm(void *)  */
extern X_API YAP_Term YAP_MkRationalTerm(void *);

/*    YAP_Int  IntOfTerm(Term) */
extern X_API const unsigned char *YAP_UnsignedStringOfTerm(YAP_Term);

/*    YAP_Int  StribgOfTerm(Term) */
extern X_API const char *YAP_StringOfTerm(YAP_Term);

/*    YAP_Int  IntOfTerm(Term) */
extern X_API YAP_Int YAP_IntOfTerm(YAP_Term);

/*    void *  BigNumOfTerm(Term) */
extern X_API YAP_Bool YAP_BigNumOfTerm(YAP_Term t, void *b);

/*    void *  RationalOfTerm(Term) */
extern X_API YAP_Bool YAP_RationalOfTerm(YAP_Term, void *);

/*    Term MkFloatTerm(YAP_Float)  */
extern X_API YAP_Term YAP_MkFloatTerm(YAP_Float);

/*    YAP_Float  FloatOfTerm(YAP_Term) */
// extract a floating point number for a term t. The type `flt` is a typedef for
// the appropriate C floating point type,
extern X_API YAP_Float YAP_FloatOfTerm(YAP_Term);

/*    Term MkAtomTerm(Atom)  */
extern X_API YAP_Term YAP_MkAtomTerm(YAP_Atom);

/*    YAP_Atom  AtomOfTerm(Term) */
extern X_API YAP_Atom YAP_AtomOfTerm(YAP_Term);

extern X_API YAP_Atom YAP_LookupAtom(const char *c);

extern X_API YAP_Atom YAP_LookupWideAtom(const wchar_t *c);

extern X_API YAP_Atom YAP_FullLookupAtom(const char *c);

/*    int  AtomNameLength(Atom) */
extern X_API size_t YAP_AtomNameLength(YAP_Atom);

extern X_API YAP_Bool YAP_IsWideAtom(YAP_Atom a);

extern X_API const char *YAP_AtomName(YAP_Atom a);

extern X_API const wchar_t *YAP_WideAtomName(YAP_Atom a);

/*    YAP_Term  MkPairTerm(YAP_Term Head, YAP_Term Tail) */
extern X_API YAP_Term YAP_MkPairTerm(YAP_Term, YAP_Term);

extern X_API YAP_Term YAP_MkListFromTerms(YAP_Term *, YAP_Int);

/*    YAP_Term  MkNewPairTerm(void) */
extern X_API YAP_Term YAP_MkNewPairTerm(void);

/*    Term  HeadOfTerm(Term)  */
extern X_API YAP_Term YAP_HeadOfTerm(YAP_Term);

/*    Term  TailOfTerm(Term)  */
extern X_API YAP_Term YAP_TailOfTerm(YAP_Term);

/*    Int  AddressOfTailOfTerm(Term *, Term **)  */
extern X_API YAP_Int YAP_SkipList(YAP_Term *, YAP_Term **);

/*    Term  TailOfTerm(Term)  */
extern X_API YAP_Term YAP_TermNil(void);

extern X_API int YAP_IsTermNil(YAP_Term);

extern X_API YAP_Term YAP_MkApplTerm(YAP_Functor functor, YAP_UInt arity,
                                     YAP_Term args[]);

extern X_API YAP_Term YAP_MkNewApplTerm(YAP_Functor f, YAP_UInt arity);

extern X_API YAP_Functor YAP_FunctorOfTerm(YAP_Term t);

extern X_API YAP_Term YAP_ArgOfTerm(YAP_UInt n, YAP_Term t);

extern X_API YAP_Term *YAP_ArgsOfTerm(YAP_Term t);

extern X_API YAP_Functor YAP_MkFunctor(YAP_Atom a, YAP_UInt n);

extern X_API YAP_Atom YAP_NameOfFunctor(YAP_Functor g);

extern X_API YAP_UInt YAP_ArityOfFunctor(YAP_Functor f);

/*  void ExtraSpace(void) */
extern X_API void *YAP_ExtraSpace(void);
extern X_API void *YAP_ExtraSpaceCut(void);

#define YAP_PRESERVE_DATA(ptr, type) (ptr = (type *)YAP_ExtraSpace())
#define YAP_PRESERVED_DATA(ptr, type) (ptr = (type *)YAP_ExtraSpace())
#define YAP_PRESERVED_DATA_CUT(ptr, type) (ptr = (type *)YAP_ExtraSpaceCut())

extern X_API YAP_Bool YAP_Unify(YAP_Term t1, YAP_Term t2);

extern X_API void YAP_UserCPredicate(const char *, YAP_UserCPred,
                                     YAP_Arity arity);

/*  void UserCPredicateWithArgs(const char *name, int *fn(), unsigned int arity)
 */
extern X_API void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred,
                                             YAP_Arity, YAP_Term);

/*  void UserBackCPredicate(const char *name, int *init(), int *cont(), int
    arity, int extra) */
extern X_API void YAP_UserBackCPredicate(const char *, YAP_UserCPred,
                                         YAP_UserCPred, YAP_Arity, YAP_Arity);

/*  void UserBackCPredicate(char *name, int *init(), int *cont(), int *cut(),
   int
    arity, int extra) */
extern X_API void YAP_UserBackCutCPredicate(const char *name, YAP_UserCPred,
                                            YAP_UserCPred, YAP_UserCPred,
                                            YAP_Arity, YAP_Arity);

/*   YAP_Int      YAP_ListLength(YAP_Term t) */
extern X_API YAP_Int YAP_ListLength(YAP_Term);

extern X_API size_t YAP_UTF8_TextLength(YAP_Term t);

/*  void CallProlog(YAP_Term t) */
extern X_API YAP_Int YAP_CallProlog(YAP_Term t);

/*  void cut_fail(void) */
extern X_API void YAP_cut_up(void);

#define YAP_cut_succeed()                                                      \
  do {                                                                         \
    YAP_cut_up();                                                              \
    return TRUE;                                                               \
  } while (0)

#define YAP_cut_fail()                                                         \
  do {                                                                         \
    YAP_cut_up();                                                              \
    return FALSE;                                                              \
  } while (0)

/*  void *AllocSpaceFromYAP_(int) */
extern X_API void *YAP_AllocSpaceFromYap(size_t);

/*  void *ReallocSpaceFromYAP_(void*,int) */
extern X_API void *YAP_ReallocSpaceFromYap(void *, size_t);

/*  void FreeSpaceFromYAP_(void *) */
extern X_API void YAP_FreeSpaceFromYap(void *);

/*  int YAP_RunGoal(YAP_Term) */
extern X_API YAP_Int YAP_RunGoal(YAP_Term);

// extern X_API YAP_Int YAP_RunPredicate(YAP_PredEntryPtr, YAP_Term *);

/*  int YAP_RunGoalOnce(YAP_Term) */
extern X_API YAP_Int YAP_RunGoalOnce(YAP_Term);

/*  int YAP_RestartGoal(void) */
extern X_API YAP_Bool YAP_RestartGoal(void);

/*  int YAP_ShutdownGoal(int) */
extern X_API YAP_Bool YAP_ShutdownGoal(int);

/*  int YAP_ContinueGoal(void) */
extern X_API YAP_Bool YAP_ContinueGoal(void);

/*  void YAP_PruneGoal(void) */
extern X_API void YAP_PruneGoal(YAP_dogoalinfo *);

/*  int YAP_FunctorToPred(struct pred_entry *, YAP_Term *) */
extern X_API YAP_PredEntryPtr YAP_FunctorToPred(YAP_Functor);

/*  int YAP_AtomToPred(struct pred_entry *, YAP_Term *) */
extern X_API YAP_PredEntryPtr YAP_AtomToPred(YAP_Atom);

extern X_API YAP_PredEntryPtr YAP_FunctorToPredInModule(YAP_Functor,
                                                        YAP_Module);

/*  int YAP_AtomToPred(struct pred_entry *, YAP_Term *) */
extern X_API YAP_PredEntryPtr YAP_AtomToPredInModule(YAP_Atom, YAP_Module);

/*  int YAP_EnterGoal(void) */
extern X_API YAP_Bool YAP_EnterGoal(YAP_PredEntryPtr, YAP_Term *,
                                    YAP_dogoalinfo *);

/*  int YAP_RetryGoal(void) */
extern X_API YAP_Bool YAP_RetryGoal(YAP_dogoalinfo *);

/*  int YAP_LeaveGoal(void) */
extern X_API YAP_Bool YAP_LeaveGoal(bool, YAP_dogoalinfo *);

/*  int YAP_GoalHasException(YAP_Term *) */
extern X_API YAP_Bool YAP_GoalHasException(YAP_Term *);

/*  void YAP_ClearExceptions(void) */
extern X_API void YAP_ClearExceptions(void);

extern X_API int YAP_Reset(yap_reset_t reset, bool reset_global);

#define YAP_Error(id, inp, ...)                                                \
  YAP_Error__( __FILE__, __FUNCTION__, __LINE__, id, inp, __VA_ARGS__)
extern X_API void YAP_Error__(const char *f,   const char *fn, int pos, int myerrno, YAP_Term t, const char *buf, ...);

extern X_API char *YAP_WriteBuffer(YAP_Term, char *, size_t, int);

extern X_API int YAP_WriteDynamicBuffer(YAP_Term t, char *buf, size_t sze,
                                        size_t *lengthp, YAP_encoding_t enc,
                                        int flags);

/*  void YAP_Term(YAP_Term) */
extern X_API YAP_Term YAP_CopyTerm(YAP_Term t);

/* bool YAP_CompileClause(YAP_Term)

@short compile the clause _Cl_; on failure it may call the exception handler. */
extern X_API bool YAP_CompileClause(YAP_Term Cl);

extern X_API int YAP_NewExo(YAP_PredEntryPtr ap, size_t data, void *user_di);

extern X_API int YAP_AssertTuples(YAP_PredEntryPtr pred, const YAP_Term *ts,
                                  size_t offset, size_t sz);

/*  int YAP_Init(YAP_init_args *) */
extern X_API void YAP_Init(YAP_init_args *);

/*  int YAP_FastInit(const char *) */
extern X_API void YAP_FastInit(char saved_state[], int argc, char *argv[]);

#ifndef _PL_STREAM_H
// if we don't know what a stream is, just don't assume nothing about the
// pointer
#define IOSTREAM void
#endif /* FPL_STREAM_H */

/// read a Prolog term from an operating system stream $s$.
extern X_API YAP_Term YAP_Read(FILE *s);

/// read a Prolog term from a Prolog opened stream $s$. Check YAP_OpenStream()
/// for how to open
/// Prolog streams in `C`.
extern X_API YAP_Term YAP_ReadFromStream(int s);

/// read a Prolog clause from a Prolog opened stream $s$. Similar to
/// YAP_ReadFromStream() but takes /// default options from read_clause/3.
extern X_API YAP_Term YAP_ReadClauseFromStream(int s, YAP_Term varNames,
                                               YAP_Term);

extern X_API void YAP_Write(YAP_Term t, FILE *s, int);

extern X_API FILE *YAP_TermToStream(YAP_Term t);

extern X_API int YAP_InitConsult(int mode, const char *filename, char **buf,
                                 int *previous_sno);

extern X_API void YAP_EndConsult(int s, int *previous_sno,
                                 const char *previous_cwd);

extern X_API void YAP_Exit(int);

/*  void YAP_PutValue(YAP_Atom, YAP_Term) */
extern X_API void YAP_PutValue(YAP_Atom, YAP_Term);

/*  YAP_Term YAP_GetValue(YAP_Atom) */
extern X_API YAP_Term YAP_GetValue(YAP_Atom);

extern X_API YAP_Term YAP_FloatsToList(YAP_Float *, size_t);
extern X_API YAP_Int YAP_ListToFloats(YAP_Term, YAP_Float *, size_t);

extern X_API YAP_Term YAP_IntsToList(YAP_Int *, size_t);
extern X_API YAP_Int YAP_ListToInts(YAP_Term, YAP_Int *, size_t);

/*  int StringToBuffer(YAP_Term,char *,unsigned int) */
extern X_API char *YAP_StringToBuffer(YAP_Term, char *, unsigned int);

extern X_API YAP_Term YAP_BufferToString(const char *s);

extern X_API YAP_Term YAP_NBufferToString(const char *s, size_t len);

/*  int BufferToString(const char *) */
extern X_API YAP_Term YAP_WideBufferToString(const wchar_t *);

extern X_API YAP_Term YAP_NWideBufferToString(const wchar_t *s, size_t len);

extern X_API YAP_Term YAP_BufferToAtomList(const char *s);

extern X_API YAP_Term YAP_NBufferToAtomList(const char *s, size_t len);

extern X_API YAP_Term YAP_WideBufferToAtomList(const wchar_t *s);

extern X_API YAP_Term YAP_NWideBufferToAtomList(const wchar_t *s, size_t len);

extern X_API YAP_Term YAP_NWideBufferToAtomDiffList(const wchar_t *s,
                                                    YAP_Term t0, size_t len);

extern X_API YAP_Term YAP_BufferToDiffList(const char *s, YAP_Term t0);

extern X_API YAP_Term YAP_NBufferToDiffList(const char *s, YAP_Term t0,
                                            size_t len);

extern X_API YAP_Term YAP_WideBufferToDiffList(const wchar_t *s, YAP_Term t0);

extern X_API YAP_Term YAP_NWideBufferToDiffList(const wchar_t *s, YAP_Term t0,
                                                size_t len);

extern X_API YAP_Term YAP_ReadBuffer(const char *s, YAP_Term *tp);

extern X_API int YAP_InitSocks(const char *host, long port);

#ifdef SFUNC

#define SFArity 0
extern X_API YAP_Term *ArgsOfSFTerm();

extern X_API YAP_Term MkSFTerm();

#endif /* SFUNC */

extern X_API void YAP_SetOutputMessage(void);

extern X_API int YAP_StreamToFileNo(YAP_Term);

/**
 * Utility routine to Obtain a pointer to the YAP representation of a stream.
 *
 * @param sno Stream Id
 * @return data structure for stream
 */
extern X_API void *YAP_RepStreamFromId(int sno);

extern X_API void YAP_CloseAllOpenStreams(void);

extern X_API void YAP_FlushAllStreams(void);

/*  YAP_Term  *YAP_StartSlots()  */
extern X_API void YAP_StartSlots(void);

/*  YAP_Term  *YAP_EndSlots()  */
extern X_API void YAP_EndSlots(void);

/*  YAP_Term  *YAP_NewSlots()  */
extern X_API YAP_handle_t YAP_NewSlots(int);

/*  YAP_Int  YAP_CurrentSlot()  */
extern X_API YAP_handle_t YAP_CurrentSlot(void);

/*  YAP_Term  *YAP_InitSlot()  */
extern X_API YAP_handle_t YAP_InitSlot(YAP_Term);

/*  YAP_Term  YAP_GetFromSlots(t)  */
extern X_API YAP_Term YAP_GetFromSlot(YAP_handle_t);

/*  YAP_Term  *YAP_AddressFromSlots(t)  */
extern X_API YAP_Term *YAP_AddressFromSlot(YAP_handle_t);

/*  YAP_Term  *YAP_AddressOfTermInSlot(t)  */
extern X_API YAP_Term *YAP_AddressOfTermInSlot(YAP_handle_t);

/*  YAP_Term  YAP_PutInSlots(t)  */
extern X_API void YAP_PutInSlot(YAP_handle_t, YAP_Term);

extern X_API int YAP_RecoverSlots(int n, YAP_handle_t top_slot);

extern X_API YAP_handle_t YAP_ArgsToSlots(int);

extern X_API void YAP_SlotsToArgs(int, YAP_handle_t);

/*  void  YAP_Throw()  */
extern X_API void YAP_Throw(YAP_Term);

/*  void  YAP_AsyncThrow()  */
extern X_API void YAP_AsyncThrow(YAP_Term);

/*  int  YAP_LookupModule()  */
#define YAP_LookupModule(T) (T)

#define YAP_ModuleName(mod) (mod)

/*  int  YAP_Halt()  */
extern X_API void YAP_Halt(int);

/*  int  YAP_TopOfLocalStack()  */
extern X_API YAP_Term *YAP_TopOfLocalStack(void);

/*  int  YAP_Predicate()  */
extern X_API void *YAP_Predicate(YAP_Atom, YAP_Arity, YAP_Term);

/*  int  YAP_Predicate()  */
extern X_API void YAP_PredicateInfo(void *, YAP_Atom *, YAP_Arity *,
                                    YAP_Module *);

/*  int  YAP_CurrentModule()  */
extern X_API YAP_Module YAP_CurrentModule(void);

/*  int  YAP_SetCurrentModule()  */
extern X_API YAP_Module YAP_SetCurrentModule(YAP_Module);

/*  int  YAP_CreateModule()  */
extern X_API YAP_Module YAP_CreateModule(YAP_Atom);

/*  int  YAP_StripModule()  */
extern X_API YAP_Term YAP_StripModule(YAP_Term, YAP_Module *);

/*  int  YAP_AtomGetHold(YAP_Atom)  */
extern X_API int YAP_AtomGetHold(YAP_Atom);

/*  int  YAP_AtomReleaseHold(YAP_Atom)  */
extern X_API int YAP_AtomReleaseHold(YAP_Atom);

/*  void  YAP_AtomReleaseHold(YAP_Atom)  */
extern X_API YAP_agc_hook YAP_AGCRegisterHook(YAP_agc_hook hook);

/*  void  YAP_AtomReleaseHold(YAP_Atom)  */
extern X_API int YAP_HaltRegisterHook(YAP_halt_hook, void *);

/*  char *YAP_cwd(void)  */
extern X_API char *YAP_cwd(void);

/* thread stuff */
extern X_API int YAP_ThreadSelf(void);
extern X_API int YAP_ThreadCreateEngine(YAP_thread_attr *attr);
extern X_API int YAP_ThreadAttachEngine(int);
extern X_API int YAP_ThreadDetachEngine(int);
extern X_API int YAP_ThreadDestroyEngine(int);

/* blob stuff */
extern X_API YAP_Term YAP_MkBlobTerm(unsigned int);
extern X_API void *YAP_BlobOfTerm(YAP_Term);

/*  term comparison */
extern X_API int YAP_CompareTerms(YAP_Term, YAP_Term);

/*  list construction */
extern X_API YAP_Term YAP_OpenList(int);
extern X_API YAP_Term YAP_ExtendList(YAP_Term, YAP_Term);
extern X_API int YAP_CloseList(YAP_Term, YAP_Term);

/*  attributed variables */
extern X_API int YAP_IsAttVar(YAP_Term);
extern X_API YAP_Term YAP_AttsOfVar(YAP_Term);

/*  stream info */
extern X_API void *YAP_FileDescriptorFromStream(YAP_Term);
extern X_API int YAP_FileNoFromStream(YAP_Term);

/*  store and recover terms */
extern X_API void *YAP_Record(YAP_Term);
extern X_API YAP_Term YAP_Recorded(void *);
extern X_API int YAP_Erase(void *);

/*  term utilities */
extern X_API int YAP_Variant(YAP_Term, YAP_Term);
extern X_API YAP_Int YAP_NumberVars(YAP_Term, YAP_Int);
extern X_API YAP_Term YAP_UnNumberVars(YAP_Term);
extern X_API int YAP_IsNumberedVariable(YAP_Term);
extern X_API int YAP_Unifiable(YAP_Term, YAP_Term);
extern X_API int YAP_ExactlyEqual(YAP_Term, YAP_Term);
extern X_API YAP_Int YAP_TermHash(YAP_Term, YAP_Int, YAP_Int, int);

extern X_API void YAP_signal(int);

/*  stack expansion control */
extern X_API YAP_Term YAP_GetYAPFlag(YAP_Term flag);
extern X_API int YAP_SetYAPFlag(YAP_Term flag, YAP_Term set);

/*    void  *YAP_GlobalBase(Term)  */
extern X_API YAP_Int YAP_VarSlotToNumber(YAP_Int);

/*    Term  YAP_ModuleUser()  */
extern X_API YAP_Term YAP_ModuleUser(void);

/*    Int  YAP_NumberOfClausesForPredicate()  */
extern X_API YAP_Int YAP_NumberOfClausesForPredicate(YAP_PredEntryPtr);

/*    int  YAP_MaxOpPriority(Atom, Term)  */
extern X_API int YAP_MaxOpPriority(YAP_Atom, YAP_Term);

/*    int  YAP_OpInfo(Atom, Term, int, int *, int *)  */
extern X_API int YAP_OpInfo(YAP_Atom, YAP_Term, int, int *, int *);

extern X_API YAP_Bool YAP_IsExternalDataInStackTerm(YAP_Term);

extern X_API YAP_Term YAP_AllocExternalDataInStack(size_t bytes);

extern X_API void *YAP_ExternalDataInStackFromTerm(YAP_Term t);

extern X_API YAP_Bool YAP_IsExternalDataInStackTerm(YAP_Term t);

extern X_API YAP_opaque_tag_t
YAP_NewOpaqueType(struct YAP_opaque_handler_struct *f);

extern X_API YAP_Bool YAP_IsOpaqueObjectTerm(YAP_Term t, YAP_opaque_tag_t tag);

extern X_API YAP_Term YAP_NewOpaqueObject(YAP_opaque_tag_t tag, size_t bytes);

extern X_API void *YAP_OpaqueObjectFromTerm(YAP_Term t);

extern X_API YAP_CELL *YAP_HeapStoreOpaqueTerm(YAP_Term t);

extern X_API int YAP_Argv(char ***);

extern X_API bool YAP_DelayInit(YAP_ModInit_t f, const char s[]);

extern X_API YAP_tag_t YAP_TagOfTerm(YAP_Term);

extern X_API size_t YAP_ExportTerm(YAP_Term, char *, size_t);

extern X_API size_t YAP_SizeOfExportedTerm(char *);

extern X_API YAP_Term YAP_ImportTerm(char *);

extern X_API int YAP_RequiresExtraStack(size_t);

/**
 * YAP_parse_yap_arguments(int argc, char *argv[], YAP_init_args *iap)
 *
 * @param [in] argc the number of arguments to YAP
 * @param [in] argv the array of arguments to YAP
 * @param [in,out] argc the array with processed settings YAP
 *
 * @return
 */ /*
     * proccess command line arguments: valid switches are:
     *  -b    boot file
     *  -l    load file
     *  -L    load file, followed by exit.
     *  -s    stack area size (K)
     *  -h    heap area size
     *  -a    aux stack size
     *  -e    emacs_mode -m
     *  -DVar=Value
     *  reserved memory for alloc IF DEBUG
     *  -P    only in development versions
     */
extern X_API YAP_file_type_t YAP_parse_yap_arguments(int argc, char *argv[],
                                                     YAP_init_args *iap);

extern X_API void *YAP_foreign_stream(int sno);

extern X_API YAP_Int YAP_AtomToInt(YAP_Atom At);

extern X_API YAP_Atom YAP_IntToAtom(YAP_Int i);

extern X_API YAP_Int YAP_FunctorToInt(YAP_Functor At);

extern X_API YAP_Functor YAP_IntToFunctor(YAP_Int i);

extern X_API YAP_PredEntryPtr YAP_TopGoal(void);

extern X_API void *YAP_GetStreamFromId(int no);

#define YAP_InitCPred(N, A, F) YAP_UserCPredicate(N, F, A)

__END_DECLS

#endif

/// @}
