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
* Last rev:	$Date: 2008-08-07 20:51:21 $,$Author: vsc $						 *
* $Log: not supported by cvs2svn $
* Revision 1.122  2008/08/01 21:44:24  vsc
* swi compatibility support
*
* Revision 1.121  2008/07/24 16:02:00  vsc
* improve C-interface and SWI comptaibility a bit.
*
* Revision 1.120  2008/07/11 17:02:07  vsc
* fixes by Bart and Tom: mostly libraries but nasty one in indexing
* compilation.
*
* Revision 1.119  2008/06/17 13:37:48  vsc
* fix c_interface not to crash when people try to recover slots that are
* not there.
* fix try_logical and friends to handle case where predicate has arity 0.
*
* Revision 1.118  2008/06/04 14:47:18  vsc
* make sure we do trim_trail whenever we mess with B!
*
* Revision 1.117  2008/06/04 13:58:36  vsc
* more fixes to C-interface
*
* Revision 1.116  2008/04/28 23:02:32  vsc
* fix bug in current_predicate/2
* fix bug in c_interface.
*
* Revision 1.115  2008/04/11 16:30:27  ricroc
* *** empty log message ***
*
* Revision 1.114  2008/04/04 13:35:41  vsc
* fix duplicate dependency frame at entry
*
* Revision 1.113  2008/04/04 09:10:02  vsc
* restore was restoring twice
*
* Revision 1.112  2008/04/03 13:26:38  vsc
* protect signal handling with locks for threaded version.
* fix close/1 entry in manual (obs from Nicos).
* fix -f option in chr Makefile.
*
* Revision 1.111  2008/04/02 21:44:07  vsc
* threaded version should ignore saved states (for now).
*
* Revision 1.110  2008/04/02 17:37:06  vsc
* handle out of memory error at thread creation (obs from Paulo Moura).
*
* Revision 1.109  2008/04/01 15:31:41  vsc
* more saved state fixes
*
* Revision 1.108  2008/03/22 23:35:00  vsc
* fix bug in all_calls
*
* Revision 1.107  2008/03/13 18:41:50  vsc
* -q flag
*
* Revision 1.106  2008/02/12 17:03:50  vsc
* SWI-portability changes
*
* Revision 1.105  2008/01/28 10:42:19  vsc
* fix BOM trouble
*
* Revision 1.104  2007/12/05 12:17:23  vsc
* improve JT
* fix graph compatibility with SICStus
* re-export declaration.
*
* Revision 1.103  2007/11/16 14:58:40  vsc
* implement sophisticated operations with matrices.
*
* Revision 1.102  2007/11/01 20:50:31  vsc
* fix YAP_LeaveGoal (again)
*
* Revision 1.101  2007/10/29 22:48:54  vsc
* small fixes
*
* Revision 1.100  2007/10/28 00:54:09  vsc
* new version of viterbi implementation
* fix all:atvars reporting bad info
* fix bad S info in x86_64
*
* Revision 1.99  2007/10/16 18:57:17  vsc
* get rid of debug statement.
*
* Revision 1.98  2007/10/15 23:48:46  vsc
* unset var
*
* Revision 1.97  2007/10/05 18:24:30  vsc
* fix garbage collector and fix LeaveGoal
*
* Revision 1.96  2007/09/04 10:34:54  vsc
* Improve SWI interface emulation.
*
* Revision 1.95  2007/06/04 12:28:01  vsc
* interface speedups
* bad error message in X is foo>>2.
*
* Revision 1.94  2007/05/15 11:33:51  vsc
* fix min list
*
* Revision 1.93  2007/05/14 16:44:11  vsc
* improve external interface
*
* Revision 1.92  2007/04/18 23:01:16  vsc
* fix deadlock when trying to create a module with the same name as a
* predicate (for now, just don't lock modules). obs Paulo Moura.
*
* Revision 1.91  2007/03/30 16:47:22  vsc
* fix gmpless blob handling
*
* Revision 1.90  2007/03/22 11:12:20  vsc
* make sure that YAP_Restart does not restart a failed goal.
*
* Revision 1.89  2007/01/28 14:26:36  vsc
* WIN32 support
*
* Revision 1.88  2007/01/08 08:27:19  vsc
* fix restore (Trevor)
* make indexing a bit faster on IDB
*
* Revision 1.87  2006/12/13 16:10:14  vsc
* several debugger and CLP(BN) improvements.
*
* Revision 1.86  2006/11/27 17:42:02  vsc
* support for UNICODE, and other bug fixes.
*
* Revision 1.85  2006/05/16 18:37:30  vsc
* WIN32 fixes
* compiler bug fixes
* extend interface
*
* Revision 1.84  2006/03/09 15:52:04  tiagosoares
* CUT_C and MYDDAS support for 64 bits architectures
*
* Revision 1.83  2006/02/08 17:29:54  tiagosoares
* MYDDAS: Myddas Top Level for MySQL and Datalog
*
* Revision 1.82  2006/01/18 15:34:53  vsc
* avoid sideffects from MkBigInt
*
* Revision 1.81  2006/01/16 02:57:51  vsc
* fix bug with very large integers
* fix bug where indexing code was looking at code after a cut.
*
* Revision 1.80  2006/01/02 03:35:44  vsc
* fix interface and docs
*
* Revision 1.79  2006/01/02 02:25:44  vsc
* cannot release space from external GMPs.
*
* Revision 1.78  2006/01/02 02:16:18  vsc
* support new interface between YAP and GMP, so that we don't rely on our own
* allocation routines.
* Several big fixes.
*
* Revision 1.77  2005/11/18 18:48:51  tiagosoares
* support for executing c code when a cut occurs
*
* Revision 1.76  2005/11/03 18:49:26  vsc
* fix bignum conversion
*
* Revision 1.75  2005/10/28 17:38:49  vsc
* sveral updates
*
* Revision 1.74  2005/10/21 16:07:07  vsc
* fix tabling
*
* Revision 1.73  2005/10/18 17:04:43  vsc
* 5.1:
* - improvements to GC
*    2 generations
*    generic speedups
* - new scheme for attvars
*    - hProlog like interface also supported
* - SWI compatibility layer
*    - extra predicates
*    - global variables
*    - moved to Prolog module
* - CLP(R) by Leslie De Koninck, Tom Schrijvers, Cristian Holzbaur, Bart
* Demoen and Jan Wielemacker
* - load_files/2
*
* from 5.0.1
*
* - WIN32 missing include files (untested)
* - -L trouble (my thanks to Takeyuchi Shiramoto-san)!
* - debugging of backtrable user-C preds would core dump.
* - redeclaring a C-predicate as Prolog core dumps.
* - badly protected  YapInterface.h.
* - break/0 was failing at exit.
* - YAP_cut_fail and YAP_cut_succeed were different from manual.
* - tracing through data-bases could core dump.
* - cut could break on very large computations.
* - first pass at BigNum issues (reported by Roberto).
* - debugger could get go awol after fail port.
* - weird message on wrong debugger option.
*
* Revision 1.72  2005/10/15 02:42:57  vsc
* fix interface
*
* Revision 1.71  2005/08/17 13:35:51  vsc
* YPP would leave exceptions on the system, disabling Yap-4.5.7
* message.
*
* Revision 1.70  2005/08/04 15:45:51  ricroc
* TABLING NEW: support to limit the table space size
*
* Revision 1.69  2005/07/19 17:12:18  rslopes
* fix for older compilers that do not support declaration of vars
* in the middle of the function code.
*
* Revision 1.68  2005/05/31 00:23:47  ricroc
* remove abort_yapor function
*
* Revision 1.67  2005/04/10 04:35:19  vsc
* AllocMemoryFromYap should now handle large requests the right way.
*
* Revision 1.66  2005/04/10 04:01:10  vsc
* bug fixes, I hope!
*
* Revision 1.65  2005/03/15 18:29:23  vsc
* fix GPL
* fix idb: stuff in coroutines.
*
* Revision 1.64  2005/03/13 06:26:10  vsc
* fix excessive pruning in meta-calls
* fix Term->int breakage in compiler
* improve JPL (at least it does something now for amd64).
*
* Revision 1.63  2005/03/04 20:30:10  ricroc
* bug fixes for YapTab support
*
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
#define C_INTERFACE

#include <stdlib.h>
#include "Yap.h"
#include "clause.h"
#include "yapio.h"
#include "Foreign.h"
#include "attvar.h"
#include "SWI-Stream.h"
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if HAVE_STDINT_H
#include <stdint.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if _MSC_VER || defined(__MINGW32__) 
#include <windows.h>
#endif
#include "iopreds.h"
// we cannot consult YapInterface.h, that conflicts with what we declare, though
// it shouldn't
#include "yap_structs.h"
#define _yap_c_interface_h 1
#include "pl-shared.h"
#include "YapText.h"
#include "pl-read.h"
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#include "threads.h"
#include "cut_c.h"
#if HAVE_MALLOC_H
#include <malloc.h>
#endif

#if !HAVE_STRNCPY
#define strncpy(X,Y,Z) strcpy(X,Y)
#endif
#if !HAVE_STRNCAT
#define strncat(X,Y,Z) strcat(X,Y)
#endif

#if defined(_MSC_VER) && defined(YAP_EXPORTS)
#define X_API __declspec(dllexport)
#endif

X_API Term    YAP_A(int);
X_API Term    YAP_Deref(Term);
X_API Term    YAP_MkVarTerm(void);
X_API Bool    YAP_IsVarTerm(Term);
X_API Bool    YAP_IsNonVarTerm(Term);
X_API Bool    YAP_IsIntTerm(Term);
X_API Bool    YAP_IsLongIntTerm(Term);
X_API Bool    YAP_IsBigNumTerm(Term);
X_API Bool    YAP_IsNumberTerm(Term);
X_API Bool    YAP_IsRationalTerm(Term);
X_API Bool    YAP_IsFloatTerm(Term);
X_API Bool    YAP_IsDbRefTerm(Term);
X_API Bool    YAP_IsAtomTerm(Term);
X_API Bool    YAP_IsPairTerm(Term);
X_API Bool    YAP_IsApplTerm(Term);
X_API Bool    YAP_IsCompoundTerm(Term);
X_API Bool    YAP_IsExternalDataInStackTerm(Term);
X_API Bool    YAP_IsOpaqueObjectTerm(Term, int);
X_API Term    YAP_MkIntTerm(Int);
X_API Term    YAP_MkBigNumTerm(void *);
X_API Term    YAP_MkRationalTerm(void *);
X_API Int     YAP_IntOfTerm(Term);
X_API void    YAP_BigNumOfTerm(Term, void *);
X_API void    YAP_RationalOfTerm(Term, void *);
X_API Term    YAP_MkFloatTerm(flt);
X_API flt     YAP_FloatOfTerm(Term);
X_API Term    YAP_MkAtomTerm(Atom);
X_API Atom    YAP_AtomOfTerm(Term);
X_API Atom    YAP_LookupAtom(char *);
X_API Atom    YAP_LookupWideAtom(wchar_t *);
X_API size_t  YAP_AtomNameLength(Atom);
X_API Atom    YAP_FullLookupAtom(char *);
X_API int     YAP_IsWideAtom(Atom);
X_API char   *YAP_AtomName(Atom);
X_API wchar_t *YAP_WideAtomName(Atom);
X_API Term    YAP_MkPairTerm(Term,Term);
X_API Term    YAP_MkListFromTerms(Term *,Int);
X_API Term    YAP_MkNewPairTerm(void);
X_API Term    YAP_HeadOfTerm(Term);
X_API Term    YAP_TailOfTerm(Term);
X_API Int     YAP_SkipList(Term *, Term **);
X_API Term    YAP_MkApplTerm(Functor,UInt,Term *);
X_API Term    YAP_MkNewApplTerm(Functor,UInt);
X_API Functor YAP_FunctorOfTerm(Term);
X_API Term    YAP_ArgOfTerm(Int,Term);
X_API Term   *YAP_ArgsOfTerm(Term);
X_API Functor YAP_MkFunctor(Atom,Int);
X_API Atom    YAP_NameOfFunctor(Functor);
X_API Int     YAP_ArityOfFunctor(Functor);
X_API void   *YAP_ExtraSpace(void);
X_API void    YAP_cut_up(void);
X_API Int     YAP_Unify(Term,Term);
X_API int     YAP_Unifiable(Term,Term);
X_API int     YAP_Reset(void);
X_API Int     YAP_ListLength(Term);
X_API Int     YAP_Init(YAP_init_args *);
X_API Int     YAP_FastInit(char *);
X_API PredEntry *YAP_FunctorToPred(Functor);
X_API PredEntry *YAP_AtomToPred(Atom);
X_API PredEntry *YAP_FunctorToPredInModule(Functor, Term);
X_API PredEntry *YAP_AtomToPredInModule(Atom, Term);
X_API Int     YAP_CallProlog(Term);
X_API void   *YAP_AllocSpaceFromYap(size_t);
X_API void   *YAP_ReallocSpaceFromYap(void*,size_t);
X_API void    YAP_FreeSpaceFromYap(void *);
X_API int     YAP_StringToBuffer(Term, char *, unsigned int);
X_API Term    YAP_ReadBuffer(char *,Term *);
X_API Term    YAP_FloatsToList(double *, size_t);
X_API Int     YAP_ListToFloats(Term, double *, size_t);
X_API Term    YAP_IntsToList(Int *, size_t);
X_API Int     YAP_ListToInts(Term, Int *, size_t);
X_API Term    YAP_BufferToString(char *);
X_API Term    YAP_NBufferToString(char *, size_t);
X_API Term    YAP_WideBufferToString(wchar_t *);
X_API Term    YAP_NWideBufferToString(wchar_t *, size_t);
X_API Term    YAP_BufferToAtomList(char *);
X_API Term    YAP_NBufferToAtomList(char *,size_t);
X_API Term    YAP_WideBufferToAtomList(wchar_t *);
X_API Term    YAP_NWideBufferToAtomList(wchar_t *, size_t);
X_API Term    YAP_NWideBufferToAtomDiffList(wchar_t *, Term, size_t);
X_API Term    YAP_BufferToDiffList(char *, Term);
X_API Term    YAP_NBufferToDiffList(char *, Term, size_t);
X_API Term    YAP_WideBufferToDiffList(wchar_t *, Term);
X_API Term    YAP_NWideBufferToDiffList(wchar_t *, Term, size_t);
X_API void    YAP_Error(int, Term, char *, ...);
X_API Int     YAP_RunPredicate(PredEntry *, Term *);
X_API Int     YAP_RunGoal(Term);
X_API Int     YAP_RunGoalOnce(Term);
X_API int     YAP_RestartGoal(void);
X_API int     YAP_ShutdownGoal(int);
X_API int     YAP_EnterGoal(PredEntry *, Term *, YAP_dogoalinfo *);
X_API int     YAP_RetryGoal(YAP_dogoalinfo *);
X_API int     YAP_LeaveGoal(int, YAP_dogoalinfo *);
X_API int     YAP_GoalHasException(Term *);
X_API void    YAP_ClearExceptions(void);
X_API int     YAP_ContinueGoal(void);
X_API void    YAP_PruneGoal(YAP_dogoalinfo *);
X_API IOSTREAM   *YAP_TermToStream(Term);
X_API IOSTREAM   *YAP_InitConsult(int, char *);
X_API void    YAP_EndConsult(IOSTREAM *);
X_API Term    YAP_Read(IOSTREAM *);
X_API void    YAP_Write(Term, IOSTREAM *, int);
X_API Term    YAP_CopyTerm(Term);
X_API int     YAP_WriteBuffer(Term, char *, size_t, int);
X_API char   *YAP_WriteDynamicBuffer(Term, char *, size_t, size_t *, int *, int);
X_API char   *YAP_CompileClause(Term);
X_API void    YAP_PutValue(Atom,Term);
X_API Term    YAP_GetValue(Atom);
X_API int     YAP_CompareTerms(Term,Term);
X_API void    YAP_Exit(int);
X_API void    YAP_InitSocks(char *, long);
X_API void    YAP_SetOutputMessage(void);
X_API int     YAP_StreamToFileNo(Term);
X_API void    YAP_CloseAllOpenStreams(void);
X_API void    YAP_FlushAllStreams(void);
X_API Int     YAP_CurrentSlot(void);
X_API Int     YAP_NewSlots(int);
X_API Int     YAP_InitSlot(Term);
X_API Term    YAP_GetFromSlot(Int);
X_API Term   *YAP_AddressFromSlot(Int);
X_API Term   *YAP_AddressOfTermInSlot(Int);
X_API void    YAP_PutInSlot(Int, Term);
X_API int     YAP_RecoverSlots(int);
X_API Int     YAP_ArgsToSlots(int);
X_API void    YAP_SlotsToArgs(int, Int);
X_API void    YAP_Throw(Term);
X_API void    YAP_AsyncThrow(Term);
X_API void    YAP_Halt(int);
X_API Term   *YAP_TopOfLocalStack(void);
X_API void   *YAP_Predicate(Atom,UInt,Term);
X_API void    YAP_PredicateInfo(void *,Atom *,UInt *,Term *);
X_API void    YAP_UserCPredicate(char *,CPredicate,UInt);
X_API void    YAP_UserBackCPredicate(char *,CPredicate,CPredicate,UInt,unsigned int);
X_API void    YAP_UserCPredicateWithArgs(char *,CPredicate,UInt,Term);
X_API void    YAP_UserBackCutCPredicate(char *,CPredicate,CPredicate,CPredicate,UInt,unsigned int);
X_API void   *YAP_ExtraSpaceCut(void);
X_API Term     YAP_SetCurrentModule(Term);
X_API Term     YAP_CurrentModule(void);
X_API Term     YAP_CreateModule(Atom);
X_API Term     YAP_StripModule(Term, Term *);
X_API int      YAP_ThreadSelf(void);
X_API int      YAP_ThreadCreateEngine(struct thread_attr_struct *);
X_API int      YAP_ThreadAttachEngine(int);
X_API int      YAP_ThreadDetachEngine(int);
X_API int      YAP_ThreadDestroyEngine(int);
X_API Term     YAP_MkBlobTerm(unsigned int);
X_API void    *YAP_BlobOfTerm(Term);
X_API Term     YAP_TermNil(void);
X_API int      YAP_IsTermNil(Term);
X_API int      YAP_AtomGetHold(Atom);
X_API int      YAP_AtomReleaseHold(Atom);
X_API Agc_hook YAP_AGCRegisterHook(Agc_hook);
X_API int      YAP_HaltRegisterHook(HaltHookFunc, void *);
X_API char    *YAP_cwd(void);
X_API Term     YAP_OpenList(int);
X_API Term     YAP_ExtendList(Term, Term);
X_API int      YAP_CloseList(Term, Term);
X_API int      YAP_IsAttVar(Term);
X_API Term     YAP_AttsOfVar(Term);
X_API int      YAP_FileNoFromStream(Term);
X_API void    *YAP_FileDescriptorFromStream(Term);
X_API void    *YAP_Record(Term);
X_API Term     YAP_Recorded(void *);
X_API int      YAP_Erase(void *);
X_API int      YAP_Variant(Term, Term);
X_API Int      YAP_NumberVars(Term, Int);
X_API Term     YAP_UnNumberVars(Term);
X_API int      YAP_IsNumberedVariable(Term);
X_API int      YAP_ExactlyEqual(Term, Term);
X_API Int      YAP_TermHash(Term, Int, Int, int);
X_API void     YAP_signal(int);
X_API int      YAP_SetYAPFlag(yap_flag_t, int);
X_API Int      YAP_VarSlotToNumber(Int);
X_API Term     YAP_ModuleUser(void);
X_API Int      YAP_NumberOfClausesForPredicate(PredEntry *);
X_API int      YAP_MaxOpPriority(Atom, Term);
X_API int      YAP_OpInfo(Atom, Term, int, int *, int *);
X_API Term     YAP_AllocExternalDataInStack(size_t);
X_API void    *YAP_ExternalDataInStackFromTerm(Term);
X_API int      YAP_NewOpaqueType(void *);
X_API Term     YAP_NewOpaqueObject(int, size_t);
X_API void    *YAP_OpaqueObjectFromTerm(Term);
X_API CELL    *YAP_HeapStoreOpaqueTerm(Term t);
X_API int      YAP_Argv(char *** argvp);
X_API YAP_tag_t YAP_TagOfTerm(Term);
X_API size_t   YAP_ExportTerm(Term, char *, size_t);
X_API size_t   YAP_SizeOfExportedTerm(char *);
X_API Term     YAP_ImportTerm(char *);
X_API int      YAP_RequiresExtraStack(size_t);
X_API Int      YAP_AtomToInt(Atom At);
X_API Atom     YAP_IntToAtom(Int i);

static UInt
current_arity(void)
{
  CACHE_REGS
 if (P && PREVOP(P,Osbpp)->opc == Yap_opcode(_call_usercpred)) {
    return PREVOP(P,Osbpp)->u.Osbpp.p->ArityOfPE;
  } else {
    return 0;
  }
}

static int
doexpand(UInt sz)
{
  CACHE_REGS
  UInt arity;

  if (P && PREVOP(P,Osbpp)->opc == Yap_opcode(_call_usercpred)) {
    arity = PREVOP(P,Osbpp)->u.Osbpp.p->ArityOfPE;
  } else {
    arity = 0;
  }
  if (!Yap_gcl(sz, arity, ENV, gc_P(P,CP))) {
    return FALSE;
  }
  return TRUE;
}

X_API Term
YAP_A(int i)
{
  CACHE_REGS
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
YAP_IsNumberTerm(Term t)
{
  return IsIntegerTerm(t) || IsIntTerm(t) || IsFloatTerm(t) || IsBigIntTerm(t);
}

X_API Bool 
YAP_IsLongIntTerm(Term t)
{
  return IsLongIntTerm(t);
}

X_API Bool 
YAP_IsBigNumTerm(Term t)
{
#if USE_GMP
  CELL *pt;
  if (IsVarTerm(t))
    return FALSE;
  if (!IsBigIntTerm(t))
    return FALSE;
  pt = RepAppl(t);
  return pt[1] == BIG_INT;
#else
  return FALSE;
#endif
}

X_API Bool 
YAP_IsRationalTerm(Term t)
{
#if USE_GMP
  CELL *pt;
  if (IsVarTerm(t))
    return FALSE;
  if (!IsBigIntTerm(t))
    return FALSE;
  pt = RepAppl(t);
  return pt[1] == BIG_RATIONAL;
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

X_API Bool 
YAP_IsCompoundTerm(Term t)
{
  return (IsApplTerm(t) && !IsExtensionFunctor(FunctorOfTerm(t))) ||
    IsPairTerm(t);
}


X_API Term 
YAP_MkIntTerm(Int n)
{
  CACHE_REGS
  Term I;
  BACKUP_H();

  I = MkIntegerTerm(n);
  RECOVER_H();
  return I;
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

X_API void
YAP_BigNumOfTerm(Term t, void *b)
{
#if USE_GMP
  MP_INT *bz = (MP_INT *)b;
  if (IsVarTerm(t))
    return;
  if (!IsBigIntTerm(t))
    return;
  mpz_set(bz,Yap_BigIntOfTerm(t));
#endif /* USE_GMP */
}

X_API Term 
YAP_MkRationalTerm(void *big)
{
#if USE_GMP
  Term I;
  BACKUP_H();
  I = Yap_MkBigRatTerm((MP_RAT *)big);
  RECOVER_H();
  return I;
#else
  return TermNil;
#endif /* USE_GMP */
}

X_API void
YAP_RationalOfTerm(Term t, void *b)
{
#if USE_GMP
  MP_RAT *br = (MP_RAT *)b;
  if (IsVarTerm(t))
    return;
  if (!IsBigIntTerm(t))
    return;
  mpq_set(br,Yap_BigRatOfTerm(t));
#endif /* USE_GMP */
}

X_API Term 
YAP_MkBlobTerm(unsigned int sz)
{
  CACHE_REGS
  Term I;
  MP_INT *dst;
  BACKUP_H();

  while (HR+(sz+sizeof(MP_INT)/sizeof(CELL)+2) > ASP-1024) {
    if (!doexpand((sz+sizeof(MP_INT)/sizeof(CELL)+2)*sizeof(CELL))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, "YAP failed to grow the stack while constructing a blob: %s", LOCAL_ErrorMessage);
      return TermNil;
    }
  }
  I = AbsAppl(HR);
  HR[0] = (CELL)FunctorBigInt;
  HR[1] = ARRAY_INT;
  dst = (MP_INT *)(HR+2);
  dst->_mp_size = 0L;
  dst->_mp_alloc = sz;
  HR += (2+sizeof(MP_INT)/sizeof(CELL));
  HR[sz] = EndSpecials;
  HR += sz+1;
  RECOVER_H();

  return I;
}

X_API void *
YAP_BlobOfTerm(Term t)
{
  MP_INT *src;

  if (IsVarTerm(t))
    return NULL;
  if (!IsBigIntTerm(t))
    return NULL;
  src = (MP_INT *)(RepAppl(t)+2);
  return (void *)(src+1);
}

X_API Term 
YAP_MkFloatTerm(double n)
{
  CACHE_REGS
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


X_API int
YAP_IsWideAtom(Atom a)
{
  return IsWideAtom(a);
}

X_API char           *
YAP_AtomName(Atom a)
{
  char *o;

  o = AtomName(a);
  return(o);
}

X_API wchar_t           *
YAP_WideAtomName(Atom a)
{
  return RepAtom(a)->WStrOfAE;
}

X_API Atom
YAP_LookupAtom(char *c)
{
  CACHE_REGS
  Atom a;

  while (TRUE) {
    a = Yap_LookupAtom(c);
    if (a == NIL || Yap_has_signal(YAP_CDOVF_SIGNAL)) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap: %s", LOCAL_ErrorMessage);
      }
    } else {
      return a;
    }
  }
}

X_API Atom
YAP_LookupWideAtom(wchar_t *c)
{
  CACHE_REGS
  Atom a;

  while (TRUE) {
    a = Yap_LookupWideAtom(c);
    if (a == NIL || Yap_has_signal(YAP_CDOVF_SIGNAL)) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap: %s", LOCAL_ErrorMessage);
      }
    } else {
      return a;
    }
  }
}

X_API Atom
YAP_FullLookupAtom(char *c)
{
  CACHE_REGS
  Atom at;

  while (TRUE) {
    at = Yap_FullLookupAtom(c);
    if (at == NIL || Yap_has_signal(YAP_CDOVF_SIGNAL)) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap: %s", LOCAL_ErrorMessage);
      }
    } else {
      return at;
    }
  }
}

X_API size_t
YAP_AtomNameLength(Atom at)
{
  if (IsBlob(at)) {
    return RepAtom(at)->rep.blob->length;
  }
  if (IsWideAtom(at)) {
    wchar_t *c = RepAtom(at)->WStrOfAE;

    return wcslen(c);
  } else {
    char *c = RepAtom(at)->StrOfAE;

    return strlen(c);
  }
}

X_API Term
YAP_MkVarTerm(void)
{
  CACHE_REGS
  CELL t; 
  BACKUP_H();

  t = MkVarTerm();

  RECOVER_H();
  return t;
}

X_API Term
YAP_MkPairTerm(Term t1, Term t2)
{
  CACHE_REGS
  Term t; 
  BACKUP_H();

  while (HR > ASP-1024) {
    Int sl1 = Yap_InitSlot(t1 PASS_REGS);
    Int sl2 = Yap_InitSlot(t2 PASS_REGS);
    RECOVER_H();
    if (!Yap_dogc( 0, NULL PASS_REGS )) {
      return TermNil;
    }
    BACKUP_H();
    t1 =  Yap_GetFromSlot(sl1 PASS_REGS);
    t2 =  Yap_GetFromSlot(sl2 PASS_REGS);
    Yap_RecoverSlots(2 PASS_REGS);
  }
  t = MkPairTerm(t1, t2);
  RECOVER_H();
  return t;
}

X_API Term
YAP_MkListFromTerms(Term *ta, Int sz)
{
  CACHE_REGS
  Term t; 
  CELL *h;
  if (sz == 0)
    return TermNil;
  BACKUP_H();
  while (HR+sz*2 > ASP-1024) {
    Int sl1 = Yap_InitSlot((CELL)ta PASS_REGS);
    RECOVER_H();
    if (!Yap_dogc( 0, NULL PASS_REGS )) {
      return TermNil;
    }
    BACKUP_H();
    ta =  (CELL *)Yap_GetFromSlot(sl1 PASS_REGS);
    Yap_RecoverSlots(1 PASS_REGS);
  }
  h = HR;
  t = AbsPair(h);
  while (sz--) {
    Term ti = *ta++;
    if (IsVarTerm(ti)) {
      RESET_VARIABLE(h);
      Yap_unify(ti, h[0]);
    } else {
      h[0] = ti;
    }
    h[1] = AbsPair(h+2);
    h += 2;
  }
  h[-1] = TermNil;
  HR = h;
  RECOVER_H();
  return t;
}

X_API Term
YAP_MkNewPairTerm()
{
  CACHE_REGS
  Term t; 
  BACKUP_H();

  if (HR > ASP-1024)
    t = TermNil;
  else
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

X_API Int
YAP_SkipList(Term *l, Term **tailp)
{
  return Yap_SkipList(l, tailp);
  Int length = 0;
  Term *s; /* slow */
  Term v; /* temporary */

  do_derefa(v,l,derefa_unk,derefa_nonvar);
  s = l;

  if ( IsPairTerm(*l) )
  { intptr_t power = 1, lam = 0;
    do
    { if ( power == lam )
      { s = l;
	power *= 2;
	lam = 0;
      }
      lam++;
      length++;
      l = RepPair(*l)+1; 
      do_derefa(v,l,derefa2_unk,derefa2_nonvar);
    } while ( *l != *s && IsPairTerm(*l) );
  }
  *tailp = l;

  return length;
}

X_API Term
YAP_MkApplTerm(Functor f,UInt arity, Term args[])
{
  CACHE_REGS
  Term t; 
  BACKUP_H();

  if (HR+arity > ASP-1024)
    t = TermNil;
  else
    t = Yap_MkApplTerm(f, arity, args);

  RECOVER_H();
  return t;
}

X_API Term
YAP_MkNewApplTerm(Functor f,UInt arity)
{
  CACHE_REGS
  Term t; 
  BACKUP_H();

  if (HR+arity > ASP-1024)
    t = TermNil;
  else
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

X_API Term *
YAP_ArgsOfTerm(Term t)
{
  if (IsApplTerm(t))
    return RepAppl(t)+1;
  else if (IsPairTerm(t))
    return RepPair(t);
  return NULL;
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
YAP_ExtraSpaceCut(void)
{
  CACHE_REGS
  void *ptr;
  BACKUP_B();

  ptr = (void *)(((CELL *)(Yap_REGS.CUT_C_TOP))-(((yamop *)Yap_REGS.CUT_C_TOP->try_userc_cut_yamop)->u.OtapFs.extra));

  RECOVER_B();
  return(ptr);
}

X_API void *
YAP_ExtraSpace(void)
{
  CACHE_REGS
  void *ptr;
  BACKUP_B();
  BACKUP_H();

  /* find a pointer to extra space allocable */
  ptr = (void *)((CELL *)(B+1)+P->u.OtapFs.s);
  B->cp_h = HR;

  RECOVER_H();
  RECOVER_B();
  return(ptr);
}

X_API void
YAP_cut_up(void)
{
  CACHE_REGS
  BACKUP_B();
      {
	while (POP_CHOICE_POINT(B->cp_b))
	  { 
	    POP_EXECUTE();
	  }
      }
      /* This is complicated: make sure we can restore the ASP
	 pointer back to where cut_up called it. Slots depend on it. */
  if (ENV > B->cp_env) {
    ASP = B->cp_env;
  }
#ifdef YAPOR
  {
    choiceptr cut_pt;

    cut_pt = B->cp_b;
    /* make sure we prune C-choicepoints */
    if (POP_CHOICE_POINT(B->cp_b))
      {
	POP_EXECUTE();
      }
    CUT_prune_to(cut_pt);
    Yap_TrimTrail();
    B = cut_pt;
  }
#else
  /* make sure we prune C-choicepoints */
  if (POP_CHOICE_POINT(B->cp_b))
    {
      POP_EXECUTE();
    }
  Yap_TrimTrail();
  B = B->cp_b;  /* cut_fail */
#endif
  HB = B->cp_h; /* cut_fail */
  RECOVER_B();
}

X_API Int
YAP_Unify(Term t1, Term t2)
{
  Int out;
  BACKUP_MACHINE_REGS();

  out = Yap_unify(t1, t2);

  RECOVER_MACHINE_REGS();
  return out;
}

X_API int
YAP_Unifiable(Term t1, Term t2)
{
  int out;
  BACKUP_MACHINE_REGS();

  out = Yap_Unifiable(t1, t2);

  RECOVER_MACHINE_REGS();
  return out;
}

/* == */
X_API int
YAP_ExactlyEqual(Term t1, Term t2)
{
  int out;
  BACKUP_MACHINE_REGS();

  out = Yap_eq(t1, t2);

  RECOVER_MACHINE_REGS();
  return out;
}

/* =@= */
X_API int
YAP_Variant(Term t1, Term t2)
{
  int out;
  BACKUP_MACHINE_REGS();

  out = Yap_Variant(Deref(t1), Deref(t2));

  RECOVER_MACHINE_REGS();
  return out;
}

/* =@= */
X_API Int
YAP_TermHash(Term t, Int sz, Int depth, int variant)
{
  Int out;

  BACKUP_MACHINE_REGS();

  out = Yap_TermHash(t, sz, depth, variant);

  RECOVER_MACHINE_REGS();
  return out;
}

X_API Int
YAP_CurrentSlot(void)
{
  CACHE_REGS
  return Yap_CurrentSlot( PASS_REGS1 );
}

X_API Int
YAP_NewSlots(int n)
{
  CACHE_REGS
  return Yap_NewSlots(n PASS_REGS);
}

X_API Int
YAP_InitSlot(Term t)
{
  CACHE_REGS
  return Yap_InitSlot(t PASS_REGS);
}

X_API int
YAP_RecoverSlots(int n)
{
  CACHE_REGS
  return Yap_RecoverSlots(n PASS_REGS);
}

X_API Term
YAP_GetFromSlot(Int slot)
{
  CACHE_REGS
  return Yap_GetFromSlot(slot PASS_REGS);
}

X_API Term *
YAP_AddressFromSlot(Int slot)
{
  CACHE_REGS
  return Yap_AddressFromSlot(slot PASS_REGS);
}

X_API Term *
YAP_AddressOfTermInSlot(Int slot)
{
  CACHE_REGS
  Term *b = Yap_AddressFromSlot(slot PASS_REGS);
  Term a = *b;
 restart:
  if (!IsVarTerm(a)) {
    return(b);
  } else if (a == (CELL)b) {
    return(b);
  } else {
    b = (CELL *)a;
    a = *b;
    goto restart;
  }
}

X_API void
YAP_PutInSlot(Int slot, Term t)
{
  CACHE_REGS
  Yap_PutInSlot(slot, t PASS_REGS);
}


typedef Int (*CPredicate0)(void);
typedef Int (*CPredicate1)(Int);
typedef Int (*CPredicate2)(Int,Int);
typedef Int (*CPredicate3)(Int,Int,Int);
typedef Int (*CPredicate4)(Int,Int,Int,Int);
typedef Int (*CPredicate5)(Int,Int,Int,Int,Int);
typedef Int (*CPredicate6)(Int,Int,Int,Int,Int,Int);
typedef Int (*CPredicate7)(Int,Int,Int,Int,Int,Int,Int);
typedef Int (*CPredicate8)(Int,Int,Int,Int,Int,Int,Int,Int);
typedef Int (*CPredicate9)(Int,Int,Int,Int,Int,Int,Int,Int,Int);
typedef Int (*CPredicate10)(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int);
typedef Int (*CPredicateV)(Int,Int,struct foreign_context *);

static Int
execute_cargs(PredEntry *pe, CPredicate exec_code USES_REGS)
{
  switch (pe->ArityOfPE) {
  case 0:
    {
      CPredicate0 code0 = (CPredicate0)exec_code;
      return ((code0)());
    }
  case 1:
    {
      CPredicate1 code1 = (CPredicate1)exec_code;
      return ((code1)(Yap_InitSlot(Deref(ARG1) PASS_REGS)));
    }
  case 2:
    {
      CPredicate2 code2 = (CPredicate2)exec_code;
      return ((code2)(Yap_InitSlot(Deref(ARG1) PASS_REGS),
		      Yap_InitSlot(Deref(ARG2) PASS_REGS)));
    }
  case 3:
    {
      CPredicate3 code3 = (CPredicate3)exec_code;
      return ((code3)(Yap_InitSlot(Deref(ARG1) PASS_REGS),
		      Yap_InitSlot(Deref(ARG2) PASS_REGS),
		      Yap_InitSlot(Deref(ARG3) PASS_REGS)));
    }
  case 4:
    {
      CPredicate4 code4 = (CPredicate4)exec_code;
      return ((code4)(Yap_InitSlot(Deref(ARG1) PASS_REGS),
		      Yap_InitSlot(Deref(ARG2) PASS_REGS),
		      Yap_InitSlot(Deref(ARG3) PASS_REGS),
		      Yap_InitSlot(Deref(ARG4) PASS_REGS)));
    }
  case 5:
    {
      CPredicate5 code5 = (CPredicate5)exec_code;
      return ((code5)(Yap_InitSlot(Deref(ARG1) PASS_REGS),
		      Yap_InitSlot(Deref(ARG2) PASS_REGS),
		      Yap_InitSlot(Deref(ARG3) PASS_REGS),
		      Yap_InitSlot(Deref(ARG4) PASS_REGS),
		      Yap_InitSlot(Deref(ARG5) PASS_REGS)));
    }
  case 6:
    {
      CPredicate6 code6 = (CPredicate6)exec_code;
      return ((code6)(Yap_InitSlot(Deref(ARG1) PASS_REGS),
		      Yap_InitSlot(Deref(ARG2) PASS_REGS),
		      Yap_InitSlot(Deref(ARG3) PASS_REGS),
		      Yap_InitSlot(Deref(ARG4) PASS_REGS),
		      Yap_InitSlot(Deref(ARG5) PASS_REGS),
		      Yap_InitSlot(Deref(ARG6) PASS_REGS)));
    }
  case 7:
    {
      CPredicate7 code7 = (CPredicate7)exec_code;
      return ((code7)(Yap_InitSlot(Deref(ARG1) PASS_REGS),
		      Yap_InitSlot(Deref(ARG2) PASS_REGS),
		      Yap_InitSlot(Deref(ARG3) PASS_REGS),
		      Yap_InitSlot(Deref(ARG4) PASS_REGS),
		      Yap_InitSlot(Deref(ARG5) PASS_REGS),
		      Yap_InitSlot(Deref(ARG6) PASS_REGS),
		      Yap_InitSlot(Deref(ARG7) PASS_REGS)));
    }
  case 8:
    {
      CPredicate8 code8 = (CPredicate8)exec_code;
      return ((code8)(Yap_InitSlot(Deref(ARG1) PASS_REGS),
		      Yap_InitSlot(Deref(ARG2) PASS_REGS),
		      Yap_InitSlot(Deref(ARG3) PASS_REGS),
		      Yap_InitSlot(Deref(ARG4) PASS_REGS),
		      Yap_InitSlot(Deref(ARG5) PASS_REGS),
		      Yap_InitSlot(Deref(ARG6) PASS_REGS),
		      Yap_InitSlot(Deref(ARG7) PASS_REGS),
		      Yap_InitSlot(Deref(ARG8) PASS_REGS)));
    }
  case 9:
    {
      CPredicate9 code9 = (CPredicate9)exec_code;
      return ((code9)(Yap_InitSlot(Deref(ARG1) PASS_REGS),
		      Yap_InitSlot(Deref(ARG2) PASS_REGS),
		      Yap_InitSlot(Deref(ARG3) PASS_REGS),
		      Yap_InitSlot(Deref(ARG4) PASS_REGS),
		      Yap_InitSlot(Deref(ARG5) PASS_REGS),
		      Yap_InitSlot(Deref(ARG6) PASS_REGS),
		      Yap_InitSlot(Deref(ARG7) PASS_REGS),
		      Yap_InitSlot(Deref(ARG8) PASS_REGS),
		      Yap_InitSlot(Deref(ARG9) PASS_REGS)));
    }
  case 10:
    {
      CPredicate10 code10 = (CPredicate10)exec_code;
      return ((code10)(Yap_InitSlot(Deref(ARG1) PASS_REGS),
		      Yap_InitSlot(Deref(ARG2) PASS_REGS),
		      Yap_InitSlot(Deref(ARG3) PASS_REGS),
		      Yap_InitSlot(Deref(ARG4) PASS_REGS),
		      Yap_InitSlot(Deref(ARG5) PASS_REGS),
		      Yap_InitSlot(Deref(ARG6) PASS_REGS),
		      Yap_InitSlot(Deref(ARG7) PASS_REGS),
		      Yap_InitSlot(Deref(ARG8) PASS_REGS),
		      Yap_InitSlot(Deref(ARG9) PASS_REGS),
		      Yap_InitSlot(Deref(ARG10) PASS_REGS)));
    }
  default:
    return(FALSE);
  }
}

typedef Int (*CBPredicate)(struct foreign_context *);
typedef Int (*CBPredicate1)(Int,struct foreign_context *);
typedef Int (*CBPredicate2)(Int,Int,struct foreign_context *);
typedef Int (*CBPredicate3)(Int,Int,Int,struct foreign_context *);
typedef Int (*CBPredicate4)(Int,Int,Int,Int,struct foreign_context *);
typedef Int (*CBPredicate5)(Int,Int,Int,Int,Int,struct foreign_context *);
typedef Int (*CBPredicate6)(Int,Int,Int,Int,Int,Int,struct foreign_context *);
typedef Int (*CBPredicate7)(Int,Int,Int,Int,Int,Int,Int,struct foreign_context *);
typedef Int (*CBPredicate8)(Int,Int,Int,Int,Int,Int,Int,Int,struct foreign_context *);
typedef Int (*CBPredicate9)(Int,Int,Int,Int,Int,Int,Int,Int,Int,struct foreign_context *);
typedef Int (*CBPredicate10)(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,struct foreign_context *);

static Int
execute_cargs_back(PredEntry *pe, CPredicate exec_code, struct foreign_context *ctx USES_REGS)
{
  switch (pe->ArityOfPE) {
  case 0:
    {
      CBPredicate code0 = (CBPredicate)exec_code;
      return ((code0)(ctx));
    }
  case 1:
    {
      CBPredicate1 code1 = (CBPredicate1)exec_code;
      return ((code1)(&B->cp_a1-LCL0,
		      ctx));
    }
  case 2:
    {
      CBPredicate2 code2 = (CBPredicate2)exec_code;
      return ((code2)(&B->cp_a1-LCL0,
		      &B->cp_a2-LCL0,
		      ctx));
    }
  case 3:
    {
      CBPredicate3 code3 = (CBPredicate3)exec_code;
      return ((code3)(&B->cp_a1-LCL0,
		      &B->cp_a2-LCL0,
		      &B->cp_a3-LCL0,
		      ctx));
    }
  case 4:
    {
      CBPredicate4 code4 = (CBPredicate4)exec_code;
      return ((code4)(&B->cp_a1-LCL0,
		      &B->cp_a2-LCL0,
		      &B->cp_a3-LCL0,
		      &B->cp_a4-LCL0,
		      ctx));
    }
  case 5:
    {
      CBPredicate5 code5 = (CBPredicate5)exec_code;
      return ((code5)(&B->cp_a1-LCL0,
		      &B->cp_a2-LCL0,
		      &B->cp_a3-LCL0,
		      &B->cp_a4-LCL0,
		      &B->cp_a5-LCL0,
		      ctx));
    }
  case 6:
    {
      CBPredicate6 code6 = (CBPredicate6)exec_code;
      return ((code6)(&B->cp_a1-LCL0,
		      &B->cp_a2-LCL0,
		      &B->cp_a3-LCL0,
		      &B->cp_a4-LCL0,
		      &B->cp_a5-LCL0,
		      &B->cp_a6-LCL0,
		      ctx));
    }
  case 7:
    {
      CBPredicate7 code7 = (CBPredicate7)exec_code;
      return ((code7)(&B->cp_a1-LCL0,
		      &B->cp_a2-LCL0,
		      &B->cp_a3-LCL0,
		      &B->cp_a4-LCL0,
		      &B->cp_a5-LCL0,
		      &B->cp_a6-LCL0,
		      &B->cp_a7-LCL0,
		      ctx));
    }
  case 8:
    {
      CBPredicate8 code8 = (CBPredicate8)exec_code;
      return ((code8)(&B->cp_a1-LCL0,
		      &B->cp_a2-LCL0,
		      &B->cp_a3-LCL0,
		      &B->cp_a4-LCL0,
		      &B->cp_a5-LCL0,
		      &B->cp_a6-LCL0,
		      &B->cp_a7-LCL0,
		      &B->cp_a8-LCL0,
		      ctx));
    }
  case 9:
    {
      CBPredicate9 code9 = (CBPredicate9)exec_code;
      return ((code9)(&B->cp_a1-LCL0,
		      &B->cp_a2-LCL0,
		      &B->cp_a3-LCL0,
		      &B->cp_a4-LCL0,
		      &B->cp_a5-LCL0,
		      &B->cp_a6-LCL0,
		      &B->cp_a7-LCL0,
		      &B->cp_a8-LCL0,
		      &B->cp_a9-LCL0,
		      ctx));
    }
  case 10:
    {
      CBPredicate10 code10 = (CBPredicate10)exec_code;
      return ((code10)(&B->cp_a1-LCL0,
		      &B->cp_a2-LCL0,
		      &B->cp_a3-LCL0,
		      &B->cp_a4-LCL0,
		      &B->cp_a5-LCL0,
		      &B->cp_a6-LCL0,
		      &B->cp_a7-LCL0,
		      &B->cp_a8-LCL0,
		      &B->cp_a9-LCL0,
		      &B->cp_a10-LCL0,
		      ctx));
    }
  default:
    return(FALSE);
  }
}

static Int
complete_fail(choiceptr ptr, int has_cp USES_REGS)
{
  // this case is easy, jut be sure to throw everything 
  // after the old B;
  while (B != ptr) {
    B = B->cp_b;
  }
  if (has_cp)
    return do_cut( FALSE );
  return FALSE;
}

static int
complete_exit(choiceptr ptr, int has_cp, int cut_all USES_REGS)
{
  // the user often leaves open frames, especially in forward execution
  while (B && (!ptr ||  B < ptr)) {
    if (cut_all || B->cp_ap == NOCODE) {/* separator */
      do_cut( TRUE ); // pushes B up
      continue;
    } else if (B->cp_ap->opc == RETRY_USERC_OPCODE && B->cp_b == ptr) { 
      // started the current choicepoint, I hope
      return do_cut( TRUE );
    } else 
      break; // oops, there is something else
  }
  if (!ptr ||  B < ptr) {
    // we're still not there yet
    choiceptr new = B;
    while (new  && new < ptr) {
      if (new->cp_ap == NOCODE) /* separator */
	new->cp_ap = FAILCODE; // there are choice-points above but at least, these won't harm innocent code
      else if (new->cp_ap->opc == RETRY_USERC_OPCODE && new->cp_b == ptr) { 
	// I can't cut, but I can tag it as done
	new->cp_ap = FAILCODE; // there are choice-points above but at least, these won't harm innocent code
      }
      new = new->cp_b;
    }
  }
  if (has_cp) {
    if (B == ptr) {
      return do_cut( TRUE );
    } else {
      ptr->cp_ap = FAILCODE;
    }
  }
  return TRUE;
}

Int
YAP_Execute(PredEntry *pe, CPredicate exec_code)
{
  CACHE_REGS
  Int ret;
  Int OASP = LCL0-(CELL *)B;
  //  Term omod = CurrentModule;
  //if (pe->PredFlags & CArgsPredFlag) {
  //  CurrentModule = pe->ModuleOfPred;
  //}
  Int CurSlot = Yap_StartSlots( PASS_REGS1 );
  if (pe->PredFlags & SWIEnvPredFlag) {
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context ctx;
    UInt i;
    Int sl = 0;

    ctx.engine = NULL;
    for (i=pe->ArityOfPE; i > 0; i--) {
      sl = Yap_InitSlot(XREGS[i] PASS_REGS);
    }
    PP = pe;
    ret = ((codev)(sl,0,&ctx));
  } else if (pe->PredFlags & CArgsPredFlag) {
    PP = pe;
    ret =  execute_cargs(pe, exec_code PASS_REGS);
  } else {
    PP = pe;
    ret = (exec_code)( PASS_REGS1 );
  }
  PP = NULL;
  // check for junk: open frames, etc */
  LOCAL_CurSlot = CurSlot;
  if (ret)
    complete_exit(((choiceptr)(LCL0-OASP)), FALSE, FALSE PASS_REGS);
  else
    complete_fail(((choiceptr)(LCL0-OASP)), FALSE PASS_REGS);
  //CurrentModule = omod;
  if (!ret) {
    Term t;

    LOCAL_BallTerm = EX;
    EX = NULL;
    if ((t = Yap_GetException())) {
      Yap_JumpToEnv(t);
      return FALSE;
    }
  }
  return ret;
}

#define FRG_REDO_MASK	0x00000003L
#define FRG_REDO_BITS	2
#define REDO_INT	0x02		/* Returned an integer */
#define REDO_PTR	0x03		/* returned a pointer */

Int
YAP_ExecuteFirst(PredEntry *pe, CPredicate exec_code)
{
  CACHE_REGS
  CELL ocp = LCL0-(CELL *)B;
  /* for slots to work */
  Int CurSlot = Yap_StartSlots( PASS_REGS1 );
  if (pe->PredFlags & (SWIEnvPredFlag|CArgsPredFlag)) {
    Int val;
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context *ctx = (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE,1));

    PP = pe;
    ctx->control = FRG_FIRST_CALL;
    ctx->engine = NULL; //(PL_local_data *)Yap_regp;
    ctx->context = (uintptr_t)NULL;
    if (pe->PredFlags & CArgsPredFlag) {
      val = execute_cargs_back(pe, exec_code, ctx PASS_REGS);
    } else {
      val = ((codev)(B->cp_args-LCL0,0,ctx));
    }
    LOCAL_CurSlot = CurSlot;
    PP = NULL;
    if (val == 0) {
      Term t;

      LOCAL_BallTerm = EX;
      EX = NULL;
      if ((t = Yap_GetException())) {
	cut_c_pop();
	B = B->cp_b;
	Yap_JumpToEnv(t);
	return FALSE;
      }
      return complete_fail(((choiceptr)(LCL0-ocp)), TRUE PASS_REGS);
    } else if (val == 1) { /* TRUE */
      return complete_exit(((choiceptr)(LCL0-ocp)), TRUE, FALSE PASS_REGS);
    } else {
      if ((val & REDO_PTR) == REDO_PTR)
	ctx->context = (uintptr_t)(val & ~REDO_PTR);
      else
	ctx->context = (uintptr_t)((val & ~REDO_PTR)>>FRG_REDO_BITS);
      return TRUE;
    }
  } else {
    Int ret = (exec_code)( PASS_REGS1 );
    LOCAL_CurSlot = CurSlot;
    if (!ret) {
      Term t;

      LOCAL_BallTerm = EX;
      EX = NULL;
      if ((t = Yap_GetException())) {
	  Yap_JumpToEnv(t);
	  return FALSE;
      }
    }
    return ret;
  }
}

Int
YAP_ExecuteOnCut(PredEntry *pe, CPredicate exec_code, struct cut_c_str *top)
{
  CACHE_REGS
  choiceptr  oB = B;
  /* find out where we belong */
  while (B->cp_b < (choiceptr)top)
    B = B->cp_b;
  if (pe->PredFlags & (SWIEnvPredFlag|CArgsPredFlag)) {
    Int val;
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context *ctx = (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE,1));
    Int CurSlot;
    CELL *args = B->cp_args;

    B = oB;
    PP = pe;
    ctx->control = FRG_CUTTED;
    ctx->engine = NULL; //(PL_local_data *)Yap_regp;
    /* for slots to work */
    CurSlot = Yap_StartSlots( PASS_REGS1 );
    if (pe->PredFlags & CArgsPredFlag) {
      val = execute_cargs_back(pe, exec_code, ctx PASS_REGS);
    } else {
      val = ((codev)(args-LCL0,0,ctx));
    }
    LOCAL_CurSlot = CurSlot;

    PP = NULL;
    //    B = LCL0-(CELL*)oB;
    if (val == 0) {
      Term t;

      LOCAL_BallTerm = EX;
      EX = NULL;
      if ((t = Yap_GetException())) {
	cut_c_pop();
	Yap_JumpToEnv(t);
	return FALSE;
      }
      return FALSE;
    } else { /* TRUE */
      return TRUE;
    } 
  } else {
    Int ret, CurSlot;
    B = oB;
    /* for slots to work */
    CurSlot = Yap_StartSlots( PASS_REGS1 );
    ret = (exec_code)( PASS_REGS1 );
    LOCAL_CurSlot = CurSlot;
    if (!ret) {
      Term t;

      LOCAL_BallTerm = EX;
      EX = NULL;
      if ((t = Yap_GetException())) {
	  Yap_JumpToEnv(t);
	  return FALSE;
      }
    }
    return ret;
  }
}


Int
YAP_ExecuteNext(PredEntry *pe, CPredicate exec_code)
{
  CACHE_REGS
    /* for slots to work */
  Int CurSlot = Yap_StartSlots( PASS_REGS1 );
  UInt ocp = LCL0-(CELL *)B;
  if (pe->PredFlags & (SWIEnvPredFlag|CArgsPredFlag)) {
    Int val;
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context *ctx = (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE,1));
    
    PP = pe;
    ctx->control = FRG_REDO;
    if (pe->PredFlags & CArgsPredFlag) {
      val = execute_cargs_back(pe, exec_code, ctx PASS_REGS);
    } else {
      val = ((codev)(B->cp_args-LCL0,0,ctx));
    }
    LOCAL_CurSlot = CurSlot;
    /* we are below the original choice point ?? */
    /* make sure we clean up the frames left by the user */
    PP = NULL;
    if (val == 0) {
      Term t;

      LOCAL_BallTerm = EX;
      EX = NULL;
      if ((t = Yap_GetException())) {
	cut_c_pop();
	B = B->cp_b;
	Yap_JumpToEnv(t);
	return FALSE;
      } else {
	return complete_fail(((choiceptr)(LCL0-ocp)), TRUE PASS_REGS);
      }
    } else if (val == 1) { /* TRUE */
      return complete_exit(((choiceptr)(LCL0-ocp)), TRUE, FALSE PASS_REGS);
    } else {
      if ((val & REDO_PTR) == REDO_PTR)
	ctx->context = (uintptr_t)(val & ~REDO_PTR);
      else
	ctx->context = (uintptr_t)((val & ~REDO_PTR)>>FRG_REDO_BITS);
    }
    return TRUE;
  } else {
    Int ret = (exec_code)( PASS_REGS1 );
    LOCAL_CurSlot = CurSlot;
    if (!ret) {
      Term t;

      LOCAL_BallTerm = EX;
      EX = NULL;
      if ((t = Yap_GetException())) {
	  Yap_JumpToEnv(t);
	  return FALSE;
      }
    }
    return ret;
  }
}

X_API Int
YAP_CallProlog(Term t)
{
  CACHE_REGS
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
YAP_ReallocSpaceFromYap(void *ptr,size_t size) {
  CACHE_REGS
  void *new_ptr;
  BACKUP_MACHINE_REGS();
  while ((new_ptr = Yap_ReallocCodeSpace(ptr,size)) == NULL) {
    if (!Yap_growheap(FALSE, size, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
      return NULL;
    }
  }
  RECOVER_MACHINE_REGS();
  return new_ptr;
}
X_API void *
YAP_AllocSpaceFromYap(size_t size)
{
  CACHE_REGS 
    void *ptr;
  BACKUP_MACHINE_REGS();

  while ((ptr = Yap_AllocCodeSpace(size)) == NULL) {
    if (!Yap_growheap(FALSE, size, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
      return NULL;
    }
  }
  RECOVER_MACHINE_REGS();
  return ptr;
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
  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.t = t;
  inp.type = YAP_STRING_CODES|YAP_STRING_TRUNC;
  inp.max = bufsize;
  out.type = YAP_STRING_CHARS;
  out.val.c = buf;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return FALSE;
  return TRUE;
}


/* copy a string to a buffer */
X_API Term
YAP_BufferToString(char *s)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_NBufferToString(char *s, size_t len)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_CODES|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
  out.sz = len;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_WideBufferToString(wchar_t *s)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_NWideBufferToString(wchar_t *s, size_t len)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_CODES|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
  out.sz = len;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_ReadBuffer(char *s, Term *tp)
{
  CACHE_REGS
  Int sl;
  BACKUP_H();

  sl = Yap_NewSlots(1 PASS_REGS);
  LOCAL_ErrorMessage=NULL;
  while (!PL_chars_to_term(s,sl)) {
    if (LOCAL_ErrorMessage) {
      if (!strcmp(LOCAL_ErrorMessage,"Stack Overflow")) {
	if (!Yap_dogc( 0, NULL PASS_REGS )) {
	  *tp = MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage));
	  LOCAL_ErrorMessage = NULL;
	  RECOVER_H();
	  return 0L;
	} 
      } else if (!strcmp(LOCAL_ErrorMessage,"Heap Overflow")) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  *tp = MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage));
	  LOCAL_ErrorMessage = NULL;
	  RECOVER_H();
	  return 0L;
	}
      } else if (!strcmp(LOCAL_ErrorMessage,"Trail Overflow")) {
	if (!Yap_growtrail (0, FALSE)) {
	  *tp = MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage));
	  LOCAL_ErrorMessage = NULL;
	  RECOVER_H();
	  return 0L;
	}
      } else {
	// get from slot has an exception
	*tp = Yap_GetFromSlot(sl PASS_REGS);
	RECOVER_H();
	return 0L;
      }
      LOCAL_ErrorMessage = NULL;
      continue;
    } else {
      break;
    }
  }
  RECOVER_H();
  return Yap_GetFromSlot(sl PASS_REGS);
}

/* copy a string to a buffer */
X_API Term
YAP_BufferToAtomList(char *s)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term
YAP_NBufferToAtomList(char *s, size_t len)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOMS|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
  out.sz = len;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_WideBufferToAtomList(wchar_t *s)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term
YAP_NWideBufferToAtomList(wchar_t *s, size_t len)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOMS|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
  out.sz = len;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term
YAP_NWideBufferToAtomDiffList(wchar_t *s, Term t0, size_t len)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOMS|YAP_STRING_NCHARS|YAP_STRING_TRUNC|YAP_STRING_DIFF;
  out.sz = len;
  out.max = len;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_BufferToDiffList(char *s, Term t0)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_CODES|YAP_STRING_DIFF;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term
YAP_NBufferToDiffList(char *s, Term t0, size_t len)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_CODES|YAP_STRING_NCHARS|YAP_STRING_TRUNC|YAP_STRING_DIFF;
  out.sz = len;
  out.max = len;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_WideBufferToDiffList(wchar_t *s, Term t0)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_CODES|YAP_STRING_DIFF;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term
YAP_NWideBufferToDiffList(wchar_t *s, Term t0, size_t len)
{
  Term t; 
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_CODES|YAP_STRING_NCHARS|YAP_STRING_TRUNC|YAP_STRING_DIFF;
  out.sz = len;
  out.max = len;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

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

X_API PredEntry *
YAP_FunctorToPred(Functor func)
{
  CACHE_REGS
  return RepPredProp(PredPropByFunc(func, CurrentModule));
}

X_API PredEntry *
YAP_AtomToPred(Atom at)
{
  CACHE_REGS
  return RepPredProp(PredPropByAtom(at, CurrentModule));  
}

X_API PredEntry *
YAP_FunctorToPredInModule(Functor func, Term mod)
{
  return RepPredProp(PredPropByFunc(func, mod));
}

X_API PredEntry *
YAP_AtomToPredInModule(Atom at, Term mod)
{
  return RepPredProp(PredPropByAtom(at, mod));  
}


static int
run_emulator(YAP_dogoalinfo *dgi USES_REGS)
{
  int out;

  LOCAL_PrologMode &= ~(UserCCallMode|CCallMode);
  out = Yap_absmi(0);
  LOCAL_PrologMode |= UserCCallMode;
  return out;
}

X_API int
YAP_EnterGoal(PredEntry *pe, Term *ptr, YAP_dogoalinfo *dgi)
{
  CACHE_REGS
  int out;

  BACKUP_MACHINE_REGS();
  LOCAL_PrologMode = UserMode;
  dgi->p = P;
  dgi->cp = CP;
  dgi->CurSlot = LOCAL_CurSlot;
  // ensure our current ENV receives current P.
  Yap_PrepGoal(pe->ArityOfPE, ptr, B PASS_REGS);
  P = pe->CodeOfPred;
  dgi->b = LCL0-(CELL*)B;
  out = run_emulator(dgi PASS_REGS);
  RECOVER_MACHINE_REGS();
  if (out) {
    LOCAL_CurSlot = dgi->CurSlot; // ignore any slots created within the called goal
    Yap_StartSlots( PASS_REGS1 );
  }
  return out;
}

X_API int
YAP_RetryGoal(YAP_dogoalinfo *dgi)
{
  CACHE_REGS
  choiceptr myB;
  int out;

  BACKUP_MACHINE_REGS();
  myB = (choiceptr)(LCL0-dgi->b);
  CP = myB->cp_cp;
  /* sanity check */
  if (B >= myB) {
    return FALSE;
  }
  P = FAILCODE;
  /* make sure we didn't leave live slots when we backtrack */
  ASP = (CELL *)B;
  LOCAL_CurSlot = dgi->CurSlot;
  out = run_emulator(dgi PASS_REGS);
  RECOVER_MACHINE_REGS();
  if (out) {
    LOCAL_CurSlot = dgi->CurSlot;
    Yap_StartSlots( PASS_REGS1 );
  }
  return out;
}

X_API int
YAP_LeaveGoal(int backtrack, YAP_dogoalinfo *dgi)
{
  CACHE_REGS
  choiceptr myB;

  BACKUP_MACHINE_REGS();
  myB = (choiceptr)(LCL0-dgi->b);
  if (B > myB) {
    /* someone cut us */
    return FALSE;
  }
  /* prune away choicepoints */
  if (B != myB) {
#ifdef YAPOR
    CUT_prune_to(myB);
#endif
    B = myB;
  }
  /* if backtracking asked for, recover space and bindings */
  if (backtrack) {
    P = FAILCODE;
    Yap_exec_absmi(TRUE);
    /* recover stack space */
    HR = B->cp_h;
    TR = B->cp_tr;
#ifdef DEPTH_LIMIT
    DEPTH = B->cp_depth;
#endif /* DEPTH_LIMIT */
    YENV = ENV = B->cp_env;
  } else {
    Yap_TrimTrail();
  }
  /* recover local stack */
#ifdef DEPTH_LIMIT
  DEPTH= ENV[E_DEPTH];
#endif
  /* make sure we prune C-choicepoints */
  if (POP_CHOICE_POINT(B->cp_b))
    {
      POP_EXECUTE();
    }
  ENV  = (CELL *)(ENV[E_E]);
  /* ASP should be set to the top of the local stack when we
     did the call */
  ASP = B->cp_env;
  /* YENV should be set to the current environment */
  YENV = ENV  = (CELL *)((B->cp_env)[E_E]);
  B    = B->cp_b;
  //SET_BB(B);
  HB = PROTECT_FROZEN_H(B);
  CP = dgi->cp;
  P = dgi->p;
  LOCAL_CurSlot = dgi->CurSlot;
  RECOVER_MACHINE_REGS();
  return TRUE;
}

X_API Int
YAP_RunGoal(Term t)
{
  CACHE_REGS
  Term out;
  yamop *old_CP = CP;
  Int CurSlot = LOCAL_CurSlot;
  BACKUP_MACHINE_REGS();

  LOCAL_AllowRestart = FALSE;
  LOCAL_PrologMode = UserMode;
  out = Yap_RunTopGoal(t);
  LOCAL_PrologMode = UserCCallMode;
  if (out) {
    P = (yamop *)ENV[E_CP];
    ENV = (CELL *)ENV[E_E];
    CP = old_CP;
    LOCAL_AllowRestart = TRUE;
    // we are back to user code again, need slots */
    Yap_StartSlots( PASS_REGS1 );
  } else {
    ENV = B->cp_env;
    ENV = (CELL *)ENV[E_E];
    CP = old_CP;
    B = B->cp_b;
    LOCAL_AllowRestart = FALSE;
    ASP = ENV;
    LOCAL_CurSlot = CurSlot;
  }
  RECOVER_MACHINE_REGS();
  return out;
}

X_API Term
YAP_AllocExternalDataInStack(size_t bytes)
{
  Term t = Yap_AllocExternalDataInStack(EXTERNAL_BLOB, bytes);
  if (t == TermNil)
    return 0L;
  return t;
}

X_API Bool
YAP_IsExternalDataInStackTerm(Term t)
{
  return IsExternalBlobTerm(t, EXTERNAL_BLOB);
}

X_API void *
YAP_ExternalDataInStackFromTerm(Term t)
{
  return ExternalBlobFromTerm (t);
}

int YAP_NewOpaqueType(void *f)
{
  int i;
  if (!GLOBAL_OpaqueHandlers) {
    GLOBAL_OpaqueHandlers = malloc(sizeof(opaque_handler_t)*(USER_BLOB_END-USER_BLOB_START));
    if (!GLOBAL_OpaqueHandlers) {
      /* no room */
      return -1;
    }
  } else if (GLOBAL_OpaqueHandlersCount == USER_BLOB_END-USER_BLOB_START) {
    /* all types used */
    return -1;
  }
  i = GLOBAL_OpaqueHandlersCount++;
  memcpy(GLOBAL_OpaqueHandlers+i,f,sizeof(opaque_handler_t));
  return i+USER_BLOB_START;
}

Term YAP_NewOpaqueObject(int tag, size_t bytes)
{
  Term t = Yap_AllocExternalDataInStack((CELL)tag, bytes);
  if (t == TermNil)
    return 0L;
  return t;
}

X_API Bool
YAP_IsOpaqueObjectTerm(Term t, int tag)
{
  return IsExternalBlobTerm(t, (CELL)tag);
}

X_API void *
YAP_OpaqueObjectFromTerm(Term t)
{
  return ExternalBlobFromTerm (t);
}

X_API CELL *
YAP_HeapStoreOpaqueTerm(Term t)
{
  return Yap_HeapStoreOpaqueTerm(t);
}

X_API Int
YAP_RunGoalOnce(Term t)
{
  CACHE_REGS
  Term out;
  yamop *old_CP = CP;
  Int oldPrologMode = LOCAL_PrologMode;

  BACKUP_MACHINE_REGS();
  LOCAL_PrologMode = UserMode;
  out = Yap_RunTopGoal(t);
  LOCAL_PrologMode = oldPrologMode;
  if (!(oldPrologMode & UserCCallMode)) {
    /* called from top-level */
    LOCAL_AllowRestart = FALSE;
    RECOVER_MACHINE_REGS();
    return out;
  }
  if (out) {
    choiceptr cut_pt, ob;

    ob = NULL;
    cut_pt = B;
    while (cut_pt-> cp_ap != NOCODE) {
      /* make sure we prune C-choicepoints */
      if (POP_CHOICE_POINT(cut_pt->cp_b))
	{
	  POP_EXECUTE();
	}
      ob = cut_pt;
      cut_pt = cut_pt->cp_b;
    }
#ifdef YAPOR
    CUT_prune_to(cut_pt);
#endif
    if (ob) {
      B = ob;
      Yap_TrimTrail();
    }
    B = cut_pt;
  }
  ASP = B->cp_env;
  ENV = (CELL *)ASP[E_E];
  B = (choiceptr)ASP[E_CB];
#ifdef  DEPTH_LIMITxs
  DEPTH = ASP[E_DEPTH];
#endif
  P = (yamop *)ASP[E_CP];
  CP = old_CP;
  LOCAL_AllowRestart = FALSE;
  RECOVER_MACHINE_REGS();
  return out;
}

X_API int
YAP_RestartGoal(void)
{
  CACHE_REGS
  int out;
  BACKUP_MACHINE_REGS();
  if (LOCAL_AllowRestart) {
    P = (yamop *)FAILCODE;
    LOCAL_PrologMode = UserMode;
    out = Yap_exec_absmi(TRUE);
    LOCAL_PrologMode = UserCCallMode;
    if (out == FALSE) {
      /* cleanup */
      Yap_trust_last();
      LOCAL_AllowRestart = FALSE;
    }
  } else { 
    out = FALSE;
  }
  RECOVER_MACHINE_REGS();
  return(out);
}

X_API int
YAP_ShutdownGoal(int backtrack)
{
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  
  if (LOCAL_AllowRestart) {
    choiceptr cut_pt;

    cut_pt = B;
    while (cut_pt-> cp_ap != NOCODE) {
      /* make sure we prune C-choicepoints */
      if (POP_CHOICE_POINT(cut_pt->cp_b))
	{
	  POP_EXECUTE();
	}
      cut_pt = cut_pt->cp_b;
    }
#ifdef YAPOR
    CUT_prune_to(cut_pt);
#endif
    /* just force backtrack */
    B = cut_pt;
    if (backtrack) {
      P = FAILCODE;
      Yap_exec_absmi(TRUE);
      /* recover stack space */
      HR = cut_pt->cp_h;
      TR = cut_pt->cp_tr;
    }
    /* we can always recover the stack */
    ASP = cut_pt->cp_env;
    ENV = (CELL *)ASP[E_E];
    B = (choiceptr)ASP[E_CB];
    Yap_TrimTrail();
#ifdef  DEPTH_LIMIT
    DEPTH = ASP[E_DEPTH];
#endif
    LOCAL_AllowRestart = FALSE;
  }
  RECOVER_MACHINE_REGS();
  return TRUE;
}

X_API int
YAP_ContinueGoal(void)
{
  CACHE_REGS
  int out;
  BACKUP_MACHINE_REGS();

  LOCAL_PrologMode = UserMode;
  out = Yap_exec_absmi(TRUE);
  LOCAL_PrologMode = UserCCallMode;

  RECOVER_MACHINE_REGS();
  return(out);
}

X_API void
YAP_PruneGoal(YAP_dogoalinfo *gi)
{
  CACHE_REGS 
  BACKUP_B();

  choiceptr myB = (choiceptr)(LCL0-gi->b);
  while (B != myB) {
    /* make sure we prune C-choicepoints */
    if (POP_CHOICE_POINT(B->cp_b))
      {
	POP_EXECUTE();
      }
    if (!B->cp_b)
      break;
    B = B->cp_b;
  }
  Yap_TrimTrail();

  RECOVER_B();
}

X_API int
YAP_GoalHasException(Term *t)
{
  CACHE_REGS
  int out = FALSE;
  BACKUP_MACHINE_REGS();
  if (EX) {
    do {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      *t = Yap_FetchTermFromDB(EX);
      if (LOCAL_Error_TYPE == YAP_NO_ERROR) {
	RECOVER_MACHINE_REGS();
	return TRUE;
      } else if (LOCAL_Error_TYPE == OUT_OF_ATTVARS_ERROR) {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growglobal(NULL)) {
	  Yap_Error(OUT_OF_ATTVARS_ERROR, TermNil, LOCAL_ErrorMessage);
	  RECOVER_MACHINE_REGS();
	  return FALSE;
	}
      } else {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growstack(EX->NOfCells*CellSize)) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  RECOVER_MACHINE_REGS();
	  return FALSE;
	}
      }
    } while (*t == (CELL)0);
    out = TRUE;
  }
  RECOVER_MACHINE_REGS();
  return out;
}

X_API void
YAP_ClearExceptions(void)
{
  CACHE_REGS

  if (EX) {
    LOCAL_BallTerm = EX;
  }
  EX = NULL;
  Yap_ResetExceptionTerm( 0 );    
  LOCAL_UncaughtThrow = FALSE;
}

X_API IOSTREAM *
YAP_InitConsult(int mode, char *filename)
{
  IOSTREAM *st;
  BACKUP_MACHINE_REGS();

  if (mode == YAP_CONSULT_MODE)
    Yap_init_consult(FALSE, filename);
  else
    Yap_init_consult(TRUE, filename);
  st = Sopen_file(filename, "r");
  RECOVER_MACHINE_REGS();
  return st;
}

X_API IOSTREAM *
YAP_TermToStream(Term t)
{
  IOSTREAM *s;
  BACKUP_MACHINE_REGS();

  if (IsVarTerm(t) || !IsAtomTerm(t))
    return NULL;
  if ( (s=Yap_GetStreamHandle(AtomOfTerm(t))) ) {
    RECOVER_MACHINE_REGS();
    return s;
  }
  RECOVER_MACHINE_REGS();
  return NULL;
}

X_API void
YAP_EndConsult(IOSTREAM *s)
{
  BACKUP_MACHINE_REGS();

  Yap_end_consult();
  Sclose(s);

  RECOVER_MACHINE_REGS();
}

X_API Term
YAP_Read(IOSTREAM *inp)
{
  GET_LD
  Term t, tpos = TermNil;
  TokEntry *tokstart;
  read_data rd;

  init_read_data(&rd, inp PASS_LD);

  BACKUP_MACHINE_REGS();


  tokstart = LOCAL_tokptr = LOCAL_toktide = Yap_tokenizer(inp, FALSE, &tpos, &rd);
  if (LOCAL_ErrorMessage)
    {
      Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable, LOCAL_Comments);
      free_read_data(&rd);
      RECOVER_MACHINE_REGS();
      return 0;
    }
  if (inp->flags & (SIO_FEOF|SIO_FEOF2)) {
      Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable, LOCAL_Comments);
      RECOVER_MACHINE_REGS();
      free_read_data(&rd);
      return MkAtomTerm (AtomEof);
  }
  t = Yap_Parse( &rd );
  Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable, LOCAL_Comments);

  free_read_data(&rd);
  RECOVER_MACHINE_REGS();
  return t;
}

X_API void
YAP_Write(Term t, IOSTREAM *stream, int flags)
{
  BACKUP_MACHINE_REGS();

  Yap_plwrite (t, stream, 0, flags, 1200);

  RECOVER_MACHINE_REGS();
}


X_API Term
YAP_CopyTerm(Term t)
{
  Term tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm(t);

  RECOVER_MACHINE_REGS();

  return tn;
}

X_API int
YAP_WriteBuffer(Term t, char *buf, size_t sze, int flags)
{
  int enc;
  size_t length;
  char *b;

  BACKUP_MACHINE_REGS();
  if ((b = Yap_TermToString(t, buf, sze, &length, &enc, flags)) != buf) {
    if (b) free(b);
    RECOVER_MACHINE_REGS();
    return FALSE;
  }
  RECOVER_MACHINE_REGS();
  return TRUE;
}

X_API char *
YAP_WriteDynamicBuffer(Term t, char *buf, size_t sze, size_t *lengthp, int *encp, int flags)
{
  char *b;

  BACKUP_MACHINE_REGS();
  b = Yap_TermToString(t, buf, sze, lengthp, encp, flags);
  RECOVER_MACHINE_REGS();
  return b;
}

X_API char *
YAP_CompileClause(Term t)
{
  CACHE_REGS
  yamop *codeaddr;
  Term mod = CurrentModule;
  Term tn = TermNil;

  BACKUP_MACHINE_REGS();

  /* allow expansion during stack initialization */
  LOCAL_ErrorMessage = NULL;
  ARG1 = t;
  YAPEnterCriticalSection();
  codeaddr = Yap_cclause (t,0, mod, t);
  if (codeaddr != NULL) {
    t = Deref(ARG1); /* just in case there was an heap overflow */
    if (!Yap_addclause (t, codeaddr, TRUE, mod, &tn)) {
      YAPLeaveCriticalSection();
      return LOCAL_ErrorMessage;
    }
  }
  YAPLeaveCriticalSection();

  if (Yap_has_signal( YAP_CDOVF_SIGNAL ) ) {
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap: %s", LOCAL_ErrorMessage);
    }
  }
  RECOVER_MACHINE_REGS();
  return(LOCAL_ErrorMessage);
}

static int eof_found = FALSE;
static int yap_lineno = 0;

static IOSTREAM *bootfile;

static char InitFile[] = "init.yap";
static char BootFile[] = "boot.yap";

/* do initial boot by consulting the file boot.yap */
static void
do_bootfile (char *bootfilename)
{
  Term t;
  Term term_end_of_file = MkAtomTerm(AtomEof);
  Term term_true = YAP_MkAtomTerm(AtomTrue);
  Functor functor_query = Yap_MkFunctor(Yap_LookupAtom("?-"),1);

  /* consult boot.pl */
  /* the consult mode does not matter here, really */
  /*
    To be honest, YAP_InitConsult does not really do much,
    it's here for the future. It also makes what we want to do clearer.
  */
  bootfile = YAP_InitConsult(YAP_CONSULT_MODE,bootfilename);
  if (bootfile == NULL)
    {
      fprintf(stderr, "[ FATAL ERROR: could not open bootfile %s ]\n", bootfilename);
      exit(1);
    }
  while (!eof_found)
    {
      CACHE_REGS
      Int CurSlot = Yap_StartSlots( PASS_REGS1 );
      t = YAP_Read(bootfile);
      LOCAL_CurSlot = CurSlot;
      if (eof_found) {
	break;
      }
      if (t == 0)
        {
	  fprintf(stderr, "[ SYNTAX ERROR: while parsing bootfile %s at line %d ]\n", bootfilename, yap_lineno);
	  exit(1);
        }
      if (YAP_IsVarTerm (t) || t == TermNil)
	{
	  continue;
	}
      else if (t == term_true)
	{
	  YAP_Exit(0);
	}
      else if (t == term_end_of_file)
	{
	  break;
	}
      else if (YAP_IsPairTerm (t))
        {
	  fprintf(stderr, "[ SYSTEM ERROR: consult not allowed in boot file ]\n");
	  fprintf(stderr, "error found at line %d and pos %d", yap_lineno, Sseek(bootfile,0L,SEEK_CUR));
	}
      else if (YAP_IsApplTerm (t) && FunctorOfTerm (t) == functor_query) 
	{ 
	  YAP_RunGoalOnce(ArgOfTerm (1, t));
        }
      else
	{
	  char *ErrorMessage = YAP_CompileClause(t);
	  if (ErrorMessage)
	    fprintf(stderr, "%s", ErrorMessage);
	}
      /* do backtrack */
      YAP_Reset();
    }
  YAP_EndConsult(bootfile);
#if DEBUG
  if (Yap_output_msg)
    fprintf(stderr,"Boot loaded\n");
#endif
}

static void
construct_init_file(char *boot_file, char *BootFile)
{
  /* trust YAPSHAREDIR over YAP_PL_SRCDIR, and notice that the code is / dependent. */
#if HAVE_GETENV
  if (getenv("YAPSHAREDIR")) {
    strncpy(boot_file, getenv("YAPSHAREDIR"), 256);
    strncat(boot_file, "/pl/", 255);
  } else {
#endif
    strncpy(boot_file, YAP_PL_SRCDIR, 256);
    strncat(boot_file, "/", 255);
#if HAVE_GETENV
  }
#endif
  strncat(boot_file, BootFile, 255);
}


/* this routine is supposed to be called from an external program
   that wants to control Yap */

#if defined(USE_SYSTEM_MALLOC) && FALSE
#define  BOOT_FROM_SAVED_STATE FALSE
#else
#define  BOOT_FROM_SAVED_STATE TRUE
#endif

X_API Int
YAP_Init(YAP_init_args *yap_init)
{
  CACHE_REGS
  int restore_result;
  int do_bootstrap = (yap_init->YapPrologBootFile != NULL);
  CELL Trail = 0, Stack = 0, Heap = 0, Atts = 0;
  char boot_file[256];
  static int initialised = FALSE;

  /* ignore repeated calls to YAP_Init */
  if (initialised)
    return YAP_BOOT_DONE_BEFOREHAND;
  initialised = TRUE;

  Yap_InitPageSize();  /* init memory page size, required by later functions */
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
  Yap_init_yapor_global_local_memory();
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA */
  GLOBAL_PrologShouldHandleInterrupts = yap_init->PrologShouldHandleInterrupts;
  Yap_InitSysbits();  /* init signal handling and time, required by later functions */
  GLOBAL_argv = yap_init->Argv;
  GLOBAL_argc = yap_init->Argc;
#if !BOOT_FROM_SAVED_STATE
  if (yap_init->SavedState) {
    fprintf(stderr,"[ WARNING: threaded YAP will ignore saved state %s ]\n",yap_init->SavedState);
    yap_init->SavedState = NULL;
  }    
#endif
  if (FALSE && BOOT_FROM_SAVED_STATE && !do_bootstrap) {
    if (Yap_SavedInfo (yap_init->SavedState, yap_init->YapLibDir, &Trail, &Stack, &Heap)) {
      yap_init->ErrorNo = LOCAL_Error_TYPE;
      yap_init->ErrorCause = LOCAL_ErrorMessage;
      return YAP_BOOT_ERROR;
    }
  }
  if (yap_init->TrailSize == 0) {
    if (yap_init->MaxTrailSize) {
      Trail = yap_init->MaxTrailSize;
    } else if (Trail == 0)
      Trail = DefTrailSpace;
  } else {
    Trail = yap_init->TrailSize;
  }
  Atts = yap_init->AttsSize;
  if (yap_init->StackSize == 0) {
    if (yap_init->MaxStackSize || yap_init->MaxGlobalSize) {
      if (yap_init->MaxStackSize) {
	if (yap_init->MaxGlobalSize) {
	  Stack = yap_init->MaxStackSize+yap_init->MaxGlobalSize;
	} else {
	  Stack = yap_init->MaxStackSize+DefStackSpace/2;
	}
      } else {
	Stack = yap_init->MaxGlobalSize+DefStackSpace/2;
      }
    } else if (Stack == 0)
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
  Yap_InitWorkspace(Heap, Stack, Trail, Atts,
	      yap_init->MaxTableSpaceSize,
	      yap_init->NumberWorkers,
	      yap_init->SchedulerLoop,
	      yap_init->DelayedReleaseLoad
	      );
  if (yap_init->QuietMode) {
    yap_flags[QUIET_MODE_FLAG] = TRUE;
  }

  { if (yap_init->YapPrologRCFile != NULL) {
      /*
	This must be done before restore, otherwise
	restore will print out messages ....
      */
      yap_flags[HALT_AFTER_CONSULT_FLAG] = yap_init->HaltAfterConsult;
    }
    /* tell the system who should cope with interruptions */
    Yap_ExecutionMode = yap_init->ExecutionMode;
    if (do_bootstrap) {
      restore_result = YAP_BOOT_FROM_PROLOG;
    } else if (BOOT_FROM_SAVED_STATE) {
      restore_result = Yap_Restore(yap_init->SavedState, yap_init->YapLibDir);
      if (restore_result == FAIL_RESTORE) {
	yap_init->ErrorNo = LOCAL_Error_TYPE;
	yap_init->ErrorCause = LOCAL_ErrorMessage;
	/* shouldn't RECOVER_MACHINE_REGS();  be here ??? */
	return YAP_BOOT_ERROR;
      }
    } else {
      restore_result = YAP_BOOT_FROM_PROLOG;
    }
    yap_flags[FAST_BOOT_FLAG] = yap_init->FastBoot;
#if defined(YAPOR) || defined(TABLING)
    Yap_init_root_frames();
#endif /* YAPOR || TABLING */
#ifdef YAPOR
    Yap_init_yapor_workers();
#if YAPOR_THREADS
    if (Yap_thread_self() != 0) {
#else
    if (worker_id != 0) {
#endif
#if defined(YAPOR_COPY) || defined(YAPOR_SBA)
      /*
	In the SBA we cannot just happily inherit registers
	from the other workers
      */
      Yap_InitYaamRegs( worker_id );
#endif /* YAPOR_COPY || YAPOR_SBA */
#ifndef YAPOR_THREADS
      Yap_InitPreAllocCodeSpace( 0 );
#endif /* YAPOR_THREADS */
      /* slaves, waiting for work */
      CurrentModule = USER_MODULE;
      P = GETWORK_FIRST_TIME;
      Yap_exec_absmi(FALSE);
      Yap_Error(INTERNAL_ERROR, TermNil, "abstract machine unexpected exit (YAP_Init)");
    }
#endif /* YAPOR */
    RECOVER_MACHINE_REGS();
  }
  /* make sure we do this after restore */
  if (yap_init->MaxStackSize) {
    GLOBAL_AllowLocalExpansion = FALSE;
  } else {
    GLOBAL_AllowLocalExpansion = TRUE;
  }
  if (yap_init->MaxGlobalSize) {
    GLOBAL_AllowGlobalExpansion = FALSE;
  } else {
    GLOBAL_AllowGlobalExpansion = TRUE;
  }
  if (yap_init->MaxTrailSize) {
    GLOBAL_AllowTrailExpansion = FALSE;
  } else {
    GLOBAL_AllowTrailExpansion = TRUE;
  }
  if (yap_init->YapPrologRCFile) {
    Yap_PutValue(AtomConsultOnBoot, MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologRCFile)));
    /*
      This must be done again after restore, as yap_flags
      has been overwritten ....
    */
    yap_flags[HALT_AFTER_CONSULT_FLAG] = yap_init->HaltAfterConsult;
  }
  if (yap_init->YapPrologTopLevelGoal) {
    Yap_PutValue(AtomTopLevelGoal, MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologTopLevelGoal)));
  }
  if (yap_init->YapPrologGoal) {
    Yap_PutValue(AtomInitGoal, MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologGoal)));
  }
  if (yap_init->YapPrologAddPath) {
    Yap_PutValue(AtomExtendFileSearchPath, MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologAddPath)));
  }
  if (yap_init->QuietMode) {
    yap_flags[QUIET_MODE_FLAG] = TRUE;
  }
  if (BOOT_FROM_SAVED_STATE && !do_bootstrap) {
    if (restore_result == FAIL_RESTORE) {
      yap_init->ErrorNo = LOCAL_Error_TYPE;
      yap_init->ErrorCause = LOCAL_ErrorMessage;
      return YAP_BOOT_ERROR;
    }
    if (Atts && Atts*1024 > 2048*sizeof(CELL))
      Yap_AttsSize = Atts*1024;
    else
      Yap_AttsSize = 2048*sizeof(CELL);
      /* reset stacks */
    //    Yap_StartSlots( PASS_REGS1 );
    if (restore_result == DO_ONLY_CODE) {
      /* first, initialise the saved state */
      Term t_goal = MkAtomTerm(AtomInitProlog);
      YAP_RunGoalOnce(t_goal);
      Yap_InitYaamRegs( 0 );
      return YAP_BOOT_FROM_SAVED_CODE;
    } else {
      return YAP_BOOT_FROM_SAVED_STACKS;
    }
  } else {

    /* read the bootfile */
    if (!do_bootstrap) {
      construct_init_file(boot_file, BootFile);
      yap_init->YapPrologBootFile = boot_file;
    }
    do_bootfile (yap_init->YapPrologBootFile ? yap_init->YapPrologBootFile : BootFile);
    /* initialise the top-level */
    if (!do_bootstrap) {
      char init_file[256];
      Atom atfile;
      Functor fgoal;
      YAP_Term goal, as[2];
      construct_init_file(init_file, InitFile);
      /* consult init file */
      atfile = Yap_LookupAtom(init_file);
      as[0] = MkAtomTerm(atfile);
      fgoal = Yap_MkFunctor(Yap_FullLookupAtom("$silent_bootstrap"), 1);
      goal = Yap_MkApplTerm(fgoal, 1, as);
      /* launch consult */
      YAP_RunGoalOnce(goal);
      /* set default module to user */
      as[0] = MkAtomTerm(AtomUser);
      fgoal = Yap_MkFunctor(Yap_LookupAtom("module"), 1);
      goal = Yap_MkApplTerm(fgoal, 1, as);
      YAP_RunGoalOnce(goal);
      /* reset stacks */
      Yap_InitYaamRegs( 0 );
    }
    Yap_PutValue(Yap_FullLookupAtom("$live"), MkAtomTerm (Yap_FullLookupAtom("$true")));
  }
  return YAP_BOOT_FROM_PROLOG;
}

X_API Int
YAP_FastInit(char saved_state[])
{
  YAP_init_args init_args;
  Int out;

  init_args.SavedState = saved_state;
  init_args.AttsSize = 0;
  init_args.HeapSize = 0;
  init_args.StackSize = 0;
  init_args.TrailSize = 0;
  init_args.MaxAttsSize = 0;
  init_args.MaxHeapSize = 0;
  init_args.MaxStackSize = 0;
  init_args.MaxGlobalSize = 0;
  init_args.MaxTrailSize = 0;
  init_args.YapLibDir = NULL;
  init_args.YapPrologBootFile = NULL;
  init_args.YapPrologInitFile = NULL;
  init_args.YapPrologRCFile = NULL;
  init_args.YapPrologGoal = NULL;
  init_args.YapPrologTopLevelGoal = NULL;
  init_args.YapPrologAddPath = NULL;
  init_args.HaltAfterConsult = FALSE;
  init_args.FastBoot = FALSE;
  init_args.NumberWorkers = 1;
  init_args.SchedulerLoop = 10;
  init_args.DelayedReleaseLoad = 3;
  init_args.PrologShouldHandleInterrupts = FALSE;
  init_args.ExecutionMode = INTERPRETED;
  init_args.Argc = 1;
  {
    size_t l1 = 2*sizeof(char *);
    if (!(init_args.Argv = (char **)malloc(l1)))
      return YAP_BOOT_ERROR;
    init_args.Argv[0] = Yap_FindExecutable ();
    init_args.Argv[1] = NULL;
  }
  init_args.ErrorNo = 0;
  init_args.ErrorCause = NULL;
  init_args.QuietMode = FALSE;
  out = YAP_Init(&init_args);
  if (out == YAP_BOOT_ERROR) {
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
  CACHE_REGS
  int res = TRUE;
  BACKUP_MACHINE_REGS();

  YAP_ClearExceptions();
  /* first, backtrack to the root */
  while (B->cp_b) {
    B = B->cp_b;
    P = FAILCODE;
    res = Yap_exec_absmi(0);
  }
  /* reinitialise the engine */
  //  Yap_InitYaamRegs( worker_id );
  GLOBAL_Initialised = TRUE;
  ENV = LCL0;
  ASP = (CELL *)B;
  /* the first real choice-point will also have AP=FAIL */ 
  /* always have an empty slots for people to use */
  P = CP = YESCODE;
  // ensure that we have slots where we need them
  LOCAL_CurSlot = 0;
  Yap_StartSlots( PASS_REGS1 );
  RECOVER_MACHINE_REGS();
  return res;
}

X_API void
YAP_Exit(int retval)
{
  Yap_exit(retval);
}

X_API void
YAP_InitSocks(char *host, long port)
{
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

X_API void
YAP_FlushAllStreams(void)
{
  BACKUP_H();

  // VSC??  Yap_FlushStreams();

  RECOVER_H();
}

X_API void
YAP_Throw(Term t)
{
  BACKUP_MACHINE_REGS();
  Yap_JumpToEnv(t);
  RECOVER_MACHINE_REGS();
}

X_API void
YAP_AsyncThrow(Term t)
{ 
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  LOCAL_PrologMode |= AsyncIntMode;
  Yap_JumpToEnv(t);
  LOCAL_PrologMode &= ~AsyncIntMode;
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
  CACHE_REGS
  return(ASP);
}

X_API void *
YAP_Predicate(Atom a, UInt arity, Term m)
{
  if (arity == 0) {
    return((void *)RepPredProp(PredPropByAtom(a,m)));
  } else {
    Functor f = Yap_MkFunctor(a, arity);
    return((void *)RepPredProp(PredPropByFunc(f,m)));
  }
} 

X_API void
YAP_PredicateInfo(void *p, Atom* a, UInt* arity, Term* m)
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
YAP_UserCPredicate(char *name, CPredicate def, UInt arity)
{
  Yap_InitCPred(name, arity, def, UserCPredFlag);
}

X_API void 
YAP_UserBackCPredicate(char *name, CPredicate init, CPredicate cont,
		   UInt arity, unsigned int extra)
{
  Yap_InitCPredBackCut(name, arity, extra, init, cont, NULL ,UserCPredFlag);

}

X_API void 
YAP_UserBackCutCPredicate(char *name, CPredicate init, CPredicate cont, CPredicate cut,
			  UInt arity, unsigned int extra)
{
  Yap_InitCPredBackCut(name, arity, extra, init, cont, cut, UserCPredFlag);
}

X_API void
YAP_UserCPredicateWithArgs(char *a, CPredicate f, UInt arity, Term mod)
{
  CACHE_REGS
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
  CACHE_REGS
  return(CurrentModule);
} 

X_API Term
YAP_SetCurrentModule(Term new)
{
  CACHE_REGS
  Term omod = CurrentModule;
  LOCAL_SourceModule = CurrentModule = new;
  return omod;
} 

X_API Term
YAP_CreateModule(Atom at)
{
  Term t;
  WRITE_LOCK(RepAtom(at)->ARWLock);  
  t = Yap_Module(MkAtomTerm(at));
  WRITE_UNLOCK(RepAtom(at)->ARWLock);  
  return t;

} 

X_API Term
YAP_StripModule(Term t,  Term *modp)
{
  return Yap_StripModule(t, modp);
}


X_API int
YAP_ThreadSelf(void)
{
#if THREADS
  return Yap_thread_self();
#else
  return -2;
#endif
} 

X_API int
YAP_ThreadCreateEngine(struct thread_attr_struct * attr)
{
#if THREADS
  return Yap_thread_create_engine(attr);
#else
  return -1;
#endif
} 

X_API int
YAP_ThreadAttachEngine( int wid)
{
#if THREADS
  return Yap_thread_attach_engine(wid);
#else
  return FALSE;
#endif
}

X_API int
YAP_ThreadDetachEngine(int wid)
{
#if THREADS
  return Yap_thread_detach_engine(wid);
#else
  return FALSE;
#endif
} 

X_API int
YAP_ThreadDestroyEngine(int wid)
{
#if THREADS
  return Yap_thread_destroy_engine(wid);
#else
  return FALSE;
#endif
} 

X_API Term
YAP_TermNil(void)
{
  return TermNil;
} 

X_API int
YAP_IsTermNil(Term t)
{
  return t == TermNil;
} 

X_API int
YAP_AtomGetHold(Atom at)
{
  return Yap_AtomIncreaseHold(at);
} 

X_API int
YAP_AtomReleaseHold(Atom at)
{
  return Yap_AtomDecreaseHold(at);
} 

X_API Agc_hook
YAP_AGCRegisterHook(Agc_hook hook)
{
  Agc_hook old = GLOBAL_AGCHook;
  GLOBAL_AGCHook = hook;
  return old;
} 

X_API int
YAP_HaltRegisterHook(HaltHookFunc hook, void * closure)
{
  return Yap_HaltRegisterHook(hook, closure);
} 

X_API char *
YAP_cwd(void)
{
  CACHE_REGS
  char *buf;
  int len;
  if (!Yap_getcwd(LOCAL_FileNameBuf, YAP_FILENAME_MAX))
    return FALSE;
  len = strlen(LOCAL_FileNameBuf);
  buf = Yap_AllocCodeSpace(len+1);
  if (!buf)
    return NULL;
  strncpy(buf, LOCAL_FileNameBuf, len);
  return buf;
}

X_API Term
YAP_FloatsToList(double *dblp, size_t sz)
{
  CACHE_REGS
  Term t;
  CELL *oldH;
  BACKUP_H();

  if (!sz)
    return TermNil;
  while (ASP-1024 < HR + sz*(2+2+SIZEOF_DOUBLE/SIZEOF_INT_P)) {
    if ((CELL *)dblp > H0 && (CELL *)dblp < HR) {
      /* we are in trouble */
      LOCAL_OpenArray =  (CELL *)dblp;
    }
    if (!Yap_dogc( 0, NULL PASS_REGS )) {
      RECOVER_H();
      return 0L;
    }
    dblp = (double *)LOCAL_OpenArray;
    LOCAL_OpenArray = NULL;
  }
  t = AbsPair(HR);
  while (sz) {
    oldH = HR;
    HR +=2;
    oldH[0] = MkFloatTerm(*dblp++);
    oldH[1] = AbsPair(HR);
    sz--;
  }
  oldH[1] = TermNil;
  RECOVER_H();
  return t;
}

X_API Int
YAP_ListToFloats(Term t, double *dblp, size_t sz)
{
  size_t i = 0;

  t = Deref(t);
  do {
    Term hd;
    if (IsVarTerm(t))
	return -1;
    if (t == TermNil)
      return i;
    if (!IsPairTerm(t))
      return -1;
    hd = HeadOfTerm(t);
    if (IsFloatTerm(hd)) {
      dblp[i++] = FloatOfTerm(hd);
    } else {
      extern double Yap_gmp_to_float(Term hd);

      if (IsIntTerm(hd))
	dblp[i++] = IntOfTerm(hd);
      else if (IsLongIntTerm(hd))
	dblp[i++] = LongIntOfTerm(hd);
#if USE_GMP
      else if (IsBigIntTerm(hd))
	dblp[i++] = Yap_gmp_to_float(hd);
#endif
      else
	return -1;
    }
    if (i == sz)
      return sz;
    t = TailOfTerm(t);
  } while (TRUE);
}

X_API Term
YAP_IntsToList(Int *dblp, size_t sz)
{
  CACHE_REGS
  Term t;
  CELL *oldH;
  BACKUP_H();

  if (!sz)
    return TermNil;
  while (ASP-1024 < HR + sz*3) {
    if ((CELL *)dblp > H0 && (CELL *)dblp < HR) {
      /* we are in trouble */
      LOCAL_OpenArray =  (CELL *)dblp;
    }
    if (!Yap_dogc( 0, NULL PASS_REGS )) {
      RECOVER_H();
      return 0L;
    }
    dblp = (Int *)LOCAL_OpenArray;
    LOCAL_OpenArray = NULL;
  }
  t = AbsPair(HR);
  while (sz) {
    oldH = HR;
    HR +=2;
    oldH[0] = MkIntegerTerm(*dblp++);
    oldH[1] = AbsPair(HR);
    sz--;
  }
  oldH[1] = TermNil;
  RECOVER_H();
  return t;
}

X_API Int
YAP_ListToInts(Term t, Int *dblp, size_t sz)
{
  size_t i = 0;

  t = Deref(t);
  do {
    Term hd;
    if (IsVarTerm(t))
	return -1;
    if (t == TermNil)
      return i;
    if (!IsPairTerm(t))
      return -1;
    hd = HeadOfTerm(t);
    if (!IsIntTerm(hd))
      return -1;
    dblp[i++] = IntOfTerm(hd);
    if (i == sz)
      return sz;
    t = TailOfTerm(t);
  } while (TRUE);
}

X_API Term
YAP_OpenList(int n)
{
  CACHE_REGS
  Term t;
  BACKUP_H();

  while (HR+2*n > ASP-1024) {
    if (!Yap_dogc( 0, NULL PASS_REGS )) {
      RECOVER_H();
      return FALSE;
    }
  }
  t = AbsPair(HR);
  HR += 2*n;

  RECOVER_H();
  return t;
}

X_API Term
YAP_ExtendList(Term t0, Term inp)
{
  Term t;
  CELL *ptr = RepPair(t0);
  BACKUP_H();

  ptr[0] = inp;
  ptr[1] = AbsPair(ptr+2);
  t = AbsPair(ptr+2);

  RECOVER_H();
  return t;
}

X_API int
YAP_CloseList(Term t0, Term tail)
{
  CELL *ptr = RepPair(t0);

  RESET_VARIABLE(ptr-1);
  if (!Yap_unify((Term)(ptr-1), tail))
    return FALSE;
  return TRUE;
}

X_API int
YAP_IsAttVar(Term t)
{
  CACHE_REGS
  t = Deref(t);
  if (!IsVarTerm(t))
    return FALSE;
  return IsAttVar(VarOfTerm(t));
}

X_API Term
YAP_AttsOfVar(Term t)
{
  CACHE_REGS
  attvar_record *attv;
  
  t = Deref(t);
  if (!IsVarTerm(t))
    return TermNil;
  if(!IsAttVar(VarOfTerm(t)))
    return TermNil;
  attv = RepAttVar(VarOfTerm(t));
  return attv->Atts;
}

X_API int
YAP_FileNoFromStream(Term t)
{

 t = Deref(t);
 if (IsVarTerm(t))
   return -1;
 return Yap_StreamToFileNo(t);
 return -1;
}

X_API void *
YAP_FileDescriptorFromStream(Term t)
{
 
  t = Deref(t);
  if (IsVarTerm(t))
    return NULL;
  return Yap_FileDescriptorFromStream(t);
  return NULL;
}

X_API void *
YAP_Record(Term t)
{
  DBTerm *dbterm;
  DBRecordList *dbt;

  dbterm = Yap_StoreTermInDB(Deref(t), 0);
  if (dbterm == NULL)
    return NULL;
  dbt = (struct record_list *)Yap_AllocCodeSpace(sizeof(struct record_list));
  while (dbt == NULL) {
    if (!Yap_growheap(FALSE, sizeof(struct record_list), NULL)) {
      /* be a good neighbor */
      Yap_FreeCodeSpace((void *)dbterm);
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "using YAP_Record");
      return NULL;
    }
  }
  if (Yap_Records) {
    Yap_Records->prev_rec = dbt;
  }
  dbt->next_rec = Yap_Records;
  dbt->prev_rec = NULL;
  dbt->dbrecord = dbterm;
  Yap_Records = dbt;
  return dbt;
}

X_API Term
YAP_Recorded(void *handle)
{
  CACHE_REGS
  Term t;
  DBTerm *dbterm = ((DBRecordList *)handle)->dbrecord;

  BACKUP_MACHINE_REGS();
  do {
    LOCAL_Error_TYPE = YAP_NO_ERROR;
    t = Yap_FetchTermFromDB(dbterm);
    if (LOCAL_Error_TYPE == YAP_NO_ERROR) {
      RECOVER_MACHINE_REGS();
      return t;
    } else if (LOCAL_Error_TYPE == OUT_OF_ATTVARS_ERROR) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_growglobal(NULL)) {
	Yap_Error(OUT_OF_ATTVARS_ERROR, TermNil, LOCAL_ErrorMessage);
	RECOVER_MACHINE_REGS();
	return FALSE;
      }
    } else {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_growstack(dbterm->NOfCells*CellSize)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	RECOVER_MACHINE_REGS();
	return FALSE;
      }
    }
  } while (t == (CELL)0);
  RECOVER_MACHINE_REGS();
  return t;
}

X_API int
YAP_Erase(void *handle)
{
  DBRecordList *dbr = (DBRecordList *)handle;
  if (dbr->next_rec) 
    dbr->next_rec->prev_rec = dbr->prev_rec;
  if (dbr->prev_rec) 
    dbr->prev_rec->next_rec = dbr->next_rec;
  else if (Yap_Records == dbr) {
    Yap_Records = dbr->next_rec;
  }
  Yap_ReleaseTermFromDB(dbr->dbrecord);
  Yap_FreeCodeSpace(handle);
  return 1;
}

X_API Int
YAP_ArgsToSlots(int n)
{
  CACHE_REGS
  Int slot = Yap_NewSlots(n PASS_REGS);
  CELL *ptr0 = LCL0+slot, *ptr1=&ARG1;
  while (n--) {
    *ptr0++ = *ptr1++;
  }
  return slot;
}

X_API void
YAP_SlotsToArgs(int n, Int slot)
{
  CACHE_REGS
  CELL *ptr0 = LCL0+slot, *ptr1=&ARG1;
  while (n--) {
    *ptr1++ = *ptr0++;
  }
}

X_API void 
YAP_signal(int sig)
{
  Yap_signal(sig);
}

X_API int
YAP_SetYAPFlag(yap_flag_t flag, int val)
{
  switch (flag) {
  case YAPC_ENABLE_GC:
    if (val) {
      Yap_PutValue(AtomGc, MkAtomTerm(AtomTrue));
    } else {
      Yap_PutValue(AtomGc, TermNil);
    }
    return TRUE;
  case YAPC_ENABLE_AGC:
    if (val) {
      GLOBAL_AGcThreshold = 10000;
    } else {
      GLOBAL_AGcThreshold = 0;
    }
    return TRUE;
  default:
    return FALSE;
  }
}


/*    Int  YAP_VarSlotToNumber(Int)  */
Int YAP_VarSlotToNumber(Int s) {
  CACHE_REGS
  Term *t = (CELL *)Deref(Yap_GetFromSlot(s PASS_REGS));
  if (t < HR)
    return t-H0;
  return t-LCL0;
}

/*    Term  YAP_ModuleUser()  */
Term YAP_ModuleUser(void) {
  return MkAtomTerm(AtomUser);
}

/*    int  YAP_PredicateHasClauses()  */
Int YAP_NumberOfClausesForPredicate(PredEntry *pe) {
  return pe->cs.p_code.NOfClauses;
}

int YAP_MaxOpPriority(Atom at, Term module)
{
  AtomEntry      *ae = RepAtom(at);
  OpEntry        *info;
  WRITE_LOCK(ae->ARWLock);
  info = Yap_GetOpPropForAModuleHavingALock(ae, module);
  if (!info) {
    WRITE_UNLOCK(ae->ARWLock);
    return 0;
  }
  int ret = info->Prefix;
  if (info->Infix > ret)
    ret = info->Infix;
  if (info->Posfix > ret)
    ret = info->Posfix;
  WRITE_UNLOCK(ae->ARWLock);
  return ret;
}

int
YAP_OpInfo(Atom at, Term module, int opkind, int *yap_type, int *prio)
{
  AtomEntry      *ae = RepAtom(at);
  OpEntry        *info;
  int n;

  WRITE_LOCK(ae->ARWLock);
  info = Yap_GetOpPropForAModuleHavingALock(ae, module);
  if (!info) {
    /* try system operators */
    info = Yap_GetOpPropForAModuleHavingALock(ae, PROLOG_MODULE);
    if (!info) {
      WRITE_UNLOCK(ae->ARWLock);
      return 0;
    }
  }
  if (opkind == PREFIX_OP) {
    SMALLUNSGN p = info->Prefix;
    if (!p) {
      WRITE_UNLOCK(ae->ARWLock);
      return FALSE;
    }
    if (p & DcrrpFlag) {
      n = 6;
      *prio = (p ^ DcrrpFlag);
    } else {
      n = 7;
      *prio = p;
    }
  } else if (opkind == INFIX_OP) {
    SMALLUNSGN p = info->Infix;
    if (!p) {
      WRITE_UNLOCK(ae->ARWLock);
      return FALSE;
    }
    if ((p & DcrrpFlag) && (p & DcrlpFlag)) {
      n = 1;
      *prio = (p ^ (DcrrpFlag | DcrlpFlag));
    } else if (p & DcrrpFlag) {
      n = 3;
      *prio = (p ^ DcrrpFlag);
    } else if (p & DcrlpFlag) {
      n = 2;
      *prio = (p ^ DcrlpFlag);
    } else {
      n = 4;
      *prio = p;
    }
  } else {
    SMALLUNSGN p = info->Posfix;
    if (p & DcrlpFlag) {
      n = 4;
      *prio = (p ^ DcrlpFlag);
    } else {
      n = 5;
      *prio = p;
    }
  }
  *yap_type = n;
  WRITE_UNLOCK(ae->ARWLock);
  return 1;
}

int
YAP_Argv(char ***argvp)
{
  if (argvp) {
    *argvp = GLOBAL_argv;
  }
  return GLOBAL_argc;
}

YAP_tag_t
YAP_TagOfTerm(Term t)
{
  if (IsVarTerm(t)) {
    CELL *pt = VarOfTerm(t);
    if (IsUnboundVar(pt)) {
      CACHE_REGS
      if (IsAttVar(pt))
	return YAP_TAG_ATT;
      return YAP_TAG_UNBOUND;
    }
    return YAP_TAG_REF;
  }
  if (IsPairTerm(t))
    return YAP_TAG_PAIR;
  if (IsAtomOrIntTerm(t)) {
    if (IsAtomTerm(t))
      return YAP_TAG_ATOM;
    return YAP_TAG_INT;
  } else {
    Functor f = FunctorOfTerm(t);
    
    if (IsExtensionFunctor(f)) {
      if (f == FunctorDBRef) {
	return YAP_TAG_DBREF;
      }
      if (f == FunctorLongInt) {
	return YAP_TAG_LONG_INT;
      }
      if (f == FunctorBigInt) {
	big_blob_type bt = RepAppl(t)[1];
	switch (bt) {
	case BIG_INT:
	  return YAP_TAG_BIG_INT;
	case BIG_RATIONAL:
	  return YAP_TAG_RATIONAL;
	default:
	  return YAP_TAG_OPAQUE;
	}
      }
    }
    return YAP_TAG_APPL;
  }
}

int YAP_BPROLOG_exception;
Term YAP_BPROLOG_curr_toam_status;

Int
YAP_ListLength(Term t) {
  Term *aux;

  Int n = Yap_SkipList(&t, &aux);
  if (IsVarTerm(*aux))
    return -1;
  if (*aux == TermNil)
    return n;
  return -1;
}

Int
YAP_NumberVars(Term t, Int nbv) {
  return Yap_NumberVars(t, nbv, FALSE);
}

Term
YAP_UnNumberVars(Term t) {
  /* don't allow sharing of ground terms */
  return Yap_UnNumberTerm(t, FALSE);
}

int
YAP_IsNumberedVariable(Term t) {
  return IsApplTerm(t) &&
    FunctorOfTerm(t) == FunctorVar &&
    IsIntegerTerm(ArgOfTerm(1,t));
}

X_API size_t
YAP_ExportTerm(Term inp, char * buf, size_t len) {
  if (!len)
    return 0;
  return Yap_ExportTerm(inp, buf, len, current_arity());
}

X_API size_t
YAP_SizeOfExportedTerm(char * buf) {
  if (!buf)
    return 0;
  return Yap_SizeOfExportedTerm(buf);
}

X_API Term
YAP_ImportTerm(char * buf) {
  return Yap_ImportTerm(buf);
}

X_API int
YAP_RequiresExtraStack(size_t sz) {
  CACHE_REGS

  if (sz < 16*1024) 
    sz = 16*1024;
  if (HR <= ASP-sz) {
    return FALSE;
  }
  BACKUP_H();
  while (HR > ASP-sz) {
    CACHE_REGS
    RECOVER_H();
    if (!Yap_dogc( 0, NULL PASS_REGS )) {
      return -1;
    }
    BACKUP_H();
  }
  RECOVER_H();
  return TRUE;
}

X_API Int
YAP_AtomToInt(Atom At)
{
  TranslationEntry *te = Yap_GetTranslationProp(At);
  if (te != NIL) return te->Translation;
  SWI_Atoms[AtomTranslations] = At;
  Yap_PutAtomTranslation(At, AtomTranslations);
  AtomTranslations++;
  if (AtomTranslations == MaxAtomTranslations) {
    Atom * nt = (Atom *)malloc(sizeof(Atom)*2*MaxAtomTranslations), *ot = SWI_Atoms;
    if (nt == NULL) {
      Yap_Error(SYSTEM_ERROR,MkAtomTerm(At),"No more room for translations");
      return -1;
    }
    memcpy(nt, ot, sizeof(Atom)*MaxAtomTranslations);
    SWI_Atoms = nt;
    free( ot );
    MaxAtomTranslations *= 2;
  }
  return AtomTranslations-1;
}

X_API Atom
YAP_IntToAtom(Int i)
{
  return SWI_Atoms[i];
}

