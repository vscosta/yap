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
#define YAP_Term Term
#define C_INTERFACE

#include <stdlib.h>
#include "Yap.h"
#include "clause.h"
#include "yapio.h"
#include "attvar.h"
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if _MSC_VER || defined(__MINGW32__) 
#include <windows.h>
#endif
#include "iopreds.h"
#define HAS_YAP_H 1
#include "yap_structs.h"
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#include "threads.h"
#ifdef CUT_C
#include "cut_c.h"
#endif /* CUT_C */
#if HAVE_MALLOC_H
#include <malloc.h>
#endif

#if !HAVE_STRNCPY
#define strncpy(X,Y,Z) strcpy(X,Y)
#endif

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
X_API void    STD_PROTO(YAP_BigNumOfTerm,(Term, void *));
X_API Term    STD_PROTO(YAP_MkFloatTerm,(flt));
X_API flt     STD_PROTO(YAP_FloatOfTerm,(Term));
X_API Term    STD_PROTO(YAP_MkAtomTerm,(Atom));
X_API Atom    STD_PROTO(YAP_AtomOfTerm,(Term));
X_API Atom    STD_PROTO(YAP_LookupAtom,(char *));
X_API Atom    STD_PROTO(YAP_LookupWideAtom,(wchar_t *));
X_API int     STD_PROTO(YAP_AtomNameLength,(Atom));
X_API Atom    STD_PROTO(YAP_FullLookupAtom,(char *));
X_API int     STD_PROTO(YAP_IsWideAtom,(Atom));
X_API char   *STD_PROTO(YAP_AtomName,(Atom));
X_API wchar_t *STD_PROTO(YAP_WideAtomName,(Atom));
X_API Term    STD_PROTO(YAP_MkPairTerm,(Term,Term));
X_API Term    STD_PROTO(YAP_MkNewPairTerm,(void));
X_API Term    STD_PROTO(YAP_HeadOfTerm,(Term));
X_API Term    STD_PROTO(YAP_TailOfTerm,(Term));
X_API Term    STD_PROTO(YAP_MkApplTerm,(Functor,unsigned long int,Term *));
X_API Term    STD_PROTO(YAP_MkNewApplTerm,(Functor,unsigned long int));
X_API Functor STD_PROTO(YAP_FunctorOfTerm,(Term));
X_API Term    STD_PROTO(YAP_ArgOfTerm,(Int,Term));
X_API Term   *STD_PROTO(YAP_ArgsOfTerm,(Term));
X_API Functor STD_PROTO(YAP_MkFunctor,(Atom,Int));
X_API Atom    STD_PROTO(YAP_NameOfFunctor,(Functor));
X_API Int     STD_PROTO(YAP_ArityOfFunctor,(Functor));
X_API void   *STD_PROTO(YAP_ExtraSpace,(void));
X_API void    STD_PROTO(YAP_cut_up,(void));
X_API Int     STD_PROTO(YAP_Unify,(Term,Term));
X_API int     STD_PROTO(YAP_Reset,(void));
X_API Int     STD_PROTO(YAP_Init,(YAP_init_args *));
X_API Int     STD_PROTO(YAP_FastInit,(char *));
X_API PredEntry *STD_PROTO(YAP_FunctorToPred,(Functor));
X_API PredEntry *STD_PROTO(YAP_AtomToPred,(Atom));
X_API Int     STD_PROTO(YAP_CallProlog,(Term));
X_API void   *STD_PROTO(YAP_AllocSpaceFromYap,(unsigned int));
X_API void   *STD_PROTO(YAP_ReallocSpaceFromYap,(void*,unsigned int));
X_API void    STD_PROTO(YAP_FreeSpaceFromYap,(void *));
X_API int     STD_PROTO(YAP_StringToBuffer, (Term, char *, unsigned int));
X_API Term    STD_PROTO(YAP_ReadBuffer, (char *,Term *));
X_API Term    STD_PROTO(YAP_BufferToString, (char *));
X_API Term    STD_PROTO(YAP_NBufferToString, (char *, size_t));
X_API Term    STD_PROTO(YAP_WideBufferToString, (wchar_t *));
X_API Term    STD_PROTO(YAP_NWideBufferToString, (wchar_t *, size_t));
X_API Term    STD_PROTO(YAP_BufferToAtomList, (char *));
X_API Term    STD_PROTO(YAP_NBufferToAtomList, (char *,size_t));
X_API Term    STD_PROTO(YAP_WideBufferToAtomList, (wchar_t *));
X_API Term    STD_PROTO(YAP_NWideBufferToAtomList, (wchar_t *, size_t));
X_API Term    STD_PROTO(YAP_NWideBufferToAtomDiffList, (wchar_t *, Term, size_t));
X_API Term    STD_PROTO(YAP_BufferToDiffList, (char *, Term));
X_API Term    STD_PROTO(YAP_NBufferToDiffList, (char *, Term, size_t));
X_API Term    STD_PROTO(YAP_WideBufferToDiffList, (wchar_t *, Term));
X_API Term    STD_PROTO(YAP_NWideBufferToDiffList, (wchar_t *, Term, size_t));
X_API void    STD_PROTO(YAP_Error,(int, Term, char *, ...));
X_API Term    STD_PROTO(YAP_RunGoal,(Term));
X_API Term    STD_PROTO(YAP_RunGoalOnce,(Term));
X_API int     STD_PROTO(YAP_RestartGoal,(void));
X_API int     STD_PROTO(YAP_ShutdownGoal,(int));
X_API int     STD_PROTO(YAP_EnterGoal,(PredEntry *, Term *, YAP_dogoalinfo *));
X_API int     STD_PROTO(YAP_RetryGoal,(YAP_dogoalinfo *));
X_API int     STD_PROTO(YAP_LeaveGoal,(int, YAP_dogoalinfo *));
X_API int     STD_PROTO(YAP_GoalHasException,(Term *));
X_API void    STD_PROTO(YAP_ClearExceptions,(void));
X_API int     STD_PROTO(YAP_ContinueGoal,(void));
X_API void    STD_PROTO(YAP_PruneGoal,(void));
X_API void    STD_PROTO(YAP_InitConsult,(int, char *));
X_API void    STD_PROTO(YAP_EndConsult,(void));
X_API Term    STD_PROTO(YAP_Read, (int (*)(void)));
X_API void    STD_PROTO(YAP_Write, (Term, int (*)(wchar_t), int));
X_API Term    STD_PROTO(YAP_CopyTerm, (Term));
X_API Term    STD_PROTO(YAP_WriteBuffer, (Term, char *, unsigned int, int));
X_API char   *STD_PROTO(YAP_CompileClause, (Term));
X_API void    STD_PROTO(YAP_PutValue, (Atom,Term));
X_API Term    STD_PROTO(YAP_GetValue, (Atom));
X_API int     STD_PROTO(YAP_CompareTerms, (Term,Term));
X_API void    STD_PROTO(YAP_Exit, (int));
X_API void    STD_PROTO(YAP_InitSocks, (char *, long));
X_API void    STD_PROTO(YAP_SetOutputMessage, (void));
X_API int     STD_PROTO(YAP_StreamToFileNo, (Term));
X_API void    STD_PROTO(YAP_CloseAllOpenStreams,(void));
X_API void    STD_PROTO(YAP_FlushAllStreams,(void));
X_API Term    STD_PROTO(YAP_OpenStream,(void *, char *, Term, int));
X_API long    STD_PROTO(YAP_CurrentSlot,(void));
X_API long    STD_PROTO(YAP_NewSlots,(int));
X_API long    STD_PROTO(YAP_InitSlot,(Term));
X_API Term    STD_PROTO(YAP_GetFromSlot,(long));
X_API Term   *STD_PROTO(YAP_AddressFromSlot,(long));
X_API void    STD_PROTO(YAP_PutInSlot,(long, Term));
X_API int     STD_PROTO(YAP_RecoverSlots,(int));
X_API void    STD_PROTO(YAP_Throw,(Term));
X_API void    STD_PROTO(YAP_Halt,(int));
X_API Term   *STD_PROTO(YAP_TopOfLocalStack,(void));
X_API void   *STD_PROTO(YAP_Predicate,(Atom,unsigned long int,int));
X_API void    STD_PROTO(YAP_PredicateInfo,(void *,Atom *,unsigned long int *,Term *));
X_API void    STD_PROTO(YAP_UserCPredicate,(char *,CPredicate,unsigned long int));
X_API void    STD_PROTO(YAP_UserBackCPredicate,(char *,CPredicate,CPredicate,unsigned long int,unsigned int));
X_API void    STD_PROTO(YAP_UserCPredicateWithArgs,(char *,CPredicate,unsigned long int,Term));
#ifdef CUT_C
X_API void    STD_PROTO(YAP_UserBackCutCPredicate,(char *,CPredicate,CPredicate,CPredicate,unsigned long int,unsigned int));
X_API void   *STD_PROTO(YAP_ExtraSpaceCut,(void));
#endif
X_API Term     STD_PROTO(YAP_CurrentModule,(void));
X_API Term     STD_PROTO(YAP_CreateModule,(Atom));
X_API Term     STD_PROTO(YAP_StripModule,(Term, Term *));
X_API int      STD_PROTO(YAP_ThreadSelf,(void));
X_API int      STD_PROTO(YAP_GetThreadRefCount,(int));
X_API void     STD_PROTO(YAP_SetThreadRefCount,(int,int));
X_API CELL     STD_PROTO(YAP_ThreadCreateEngine,(struct thread_attr_struct *));
X_API int      STD_PROTO(YAP_ThreadAttachEngine,(int));
X_API int      STD_PROTO(YAP_ThreadDetachEngine,(int));
X_API int      STD_PROTO(YAP_ThreadDestroyEngine,(int));
X_API Term     STD_PROTO(YAP_MkBlobTerm,(unsigned int));
X_API void    *STD_PROTO(YAP_BlobOfTerm,(Term));
X_API Term     STD_PROTO(YAP_TermNil,(void));
X_API int      STD_PROTO(YAP_AtomGetHold,(Atom));
X_API int      STD_PROTO(YAP_AtomReleaseHold,(Atom));
X_API Agc_hook STD_PROTO(YAP_AGCRegisterHook,(Agc_hook));
X_API char    *STD_PROTO(YAP_cwd,(void));
X_API Term     STD_PROTO(YAP_OpenList,(int));
X_API Term     STD_PROTO(YAP_ExtendList,(Term, Term));
X_API int      STD_PROTO(YAP_CloseList,(Term, Term));
X_API int      STD_PROTO(YAP_IsAttVar,(Term));
X_API Term     STD_PROTO(YAP_AttsOfVar,(Term));
X_API Term     STD_PROTO(YAP_TermHash,(Term));
X_API int      STD_PROTO(YAP_FileNoFromStream,(Term));
X_API void    *STD_PROTO(YAP_FileDescriptorFromStream,(Term));

static int (*do_getf)(void);

static int do_yap_getc(int streamno) {
  return(do_getf());
}

static int (*do_putcf)(wchar_t);

static int do_yap_putc(int streamno,wchar_t ch) {
  do_putcf(ch);
  return(ch);
}

static int
dogc(void)
{
  UInt arity;

  if (P && PREVOP(P,Osbpp)->opc == Yap_opcode(_call_usercpred)) {
    arity = PREVOP(P,Osbpp)->u.Osbpp.p->ArityOfPE;
  } else {
    arity = 0;
  }
  if (!Yap_gc(arity, ENV, CP)) {
    return FALSE;
  }
  return TRUE;
}

static int
doexpand(UInt sz)
{
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
  return IsBigIntTerm(t);
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
YAP_MkBlobTerm(unsigned int sz)
{
  Term I;
  MP_INT *dst;
  BACKUP_H();

  while (H+(sz+sizeof(MP_INT)/sizeof(CELL)+2) > ASP-1024) {
    if (!doexpand((sz+sizeof(MP_INT)/sizeof(CELL)+2)*sizeof(CELL))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, "YAP failed to grow the stack while constructing a blob: %s", Yap_ErrorMessage);
      return TermNil;
    }
  }
  I = AbsAppl(H);
  H[0] = (CELL)FunctorBigInt;
  H[1] = BIG_INT;
  dst = (MP_INT *)(H+2);
  dst->_mp_size = 0L;
  dst->_mp_alloc = sz;
  H += (2+sizeof(MP_INT)/sizeof(CELL));
  H[sz] = EndSpecials;
  H += sz+1;
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
YAP_LookupWideAtom(wchar_t *c)
{
  Atom a;

  while (TRUE) {
    a = Yap_LookupWideAtom(c);
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

X_API int
YAP_AtomNameLength(Atom at)
{
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

  if (H > ASP-1024) {
    long sl1 = Yap_InitSlot(t1);
    long sl2 = Yap_InitSlot(t2);
    if (!dogc()) {
      RECOVER_H();
      return TermNil;
    }
    t1 =  Yap_GetFromSlot(sl1);
    t2 =  Yap_GetFromSlot(sl2);
    Yap_RecoverSlots(2);
  }
  t = MkPairTerm(t1, t2);
  RECOVER_H();
  return t;
}

X_API Term
YAP_MkNewPairTerm()
{
  Term t; 
  BACKUP_H();

  if (H > ASP-1024)
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

X_API Term
YAP_MkApplTerm(Functor f,unsigned long int arity, Term args[])
{
  Term t; 
  BACKUP_H();

  if (H+arity > ASP-1024)
    t = TermNil;
  else
    t = Yap_MkApplTerm(f, arity, args);

  RECOVER_H();
  return t;
}

X_API Term
YAP_MkNewApplTerm(Functor f,unsigned long int arity)
{
  Term t; 
  BACKUP_H();

  if (H+arity > ASP-1024)
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

#ifdef CUT_C
X_API void *
YAP_ExtraSpaceCut(void)
{
  void *ptr;
  BACKUP_B();

  ptr = (void *)(((CELL *)(Yap_REGS.CUT_C_TOP))-(((yamop *)Yap_REGS.CUT_C_TOP->try_userc_cut_yamop)->u.OtapFs.extra));

  RECOVER_B();
  return(ptr);
}
#endif /*CUT_C*/

X_API void *
YAP_ExtraSpace(void)
{
  void *ptr;
  BACKUP_B();
  BACKUP_H();

  /* find a pointer to extra space allocable */
  ptr = (void *)((CELL *)(B+1)+P->u.OtapFs.s);
  B->cp_h = H;

  RECOVER_H();
  RECOVER_B();
  return(ptr);
}

X_API void
YAP_cut_up(void)
{
  BACKUP_B();
#ifdef CUT_C
      {
	while (POP_CHOICE_POINT(B->cp_b))
	  { 
	    POP_EXECUTE();
	  }
      }
#endif /* CUT_C */
#ifdef YAPOR
  {
    choiceptr cut_pt;

    cut_pt = B->cp_b;
    CUT_prune_to(cut_pt);
    B = cut_pt;
  }
#else	/* YAPOR */
  B = B->cp_b;  /* cut_fail */
#endif
  HB = B->cp_h; /* cut_fail */
  Yap_TrimTrail();

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

X_API long
YAP_CurrentSlot(void)
{
  return Yap_CurrentSlot();
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

X_API int
YAP_RecoverSlots(int n)
{
 return Yap_RecoverSlots(n);
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


typedef enum
{ FRG_FIRST_CALL = 0,		/* Initial call */
  FRG_CUTTED     = 1,		/* Context was cutted */
  FRG_REDO	 = 2		/* Normal redo */
} frg_code;

typedef  struct foreign_context
    { int *		context;	/* context value */
      frg_code		control;	/* FRG_* action */
      struct PL_local_data *engine;		/* invoking engine */
} scontext ;

typedef Int (*CPredicate1)(long);
typedef Int (*CPredicate2)(long,long);
typedef Int (*CPredicate3)(long,long,long);
typedef Int (*CPredicate4)(long,long,long,long);
typedef Int (*CPredicate5)(long,long,long,long,long);
typedef Int (*CPredicate6)(long,long,long,long,long,long);
typedef Int (*CPredicate7)(long,long,long,long,long,long,long);
typedef Int (*CPredicate8)(long,long,long,long,long,long,long,long);
typedef Int (*CPredicateV)(Int,Int,struct foreign_context *);

static Int
execute_cargs(PredEntry *pe, CPredicate exec_code)
{
  switch (pe->ArityOfPE) {
  case 0:
    {
      CPredicate code0 = exec_code;
      return ((code0)());
    }
  case 1:
    {
      CPredicate1 code1 = (CPredicate1)exec_code;
      return ((code1)(Yap_InitSlot(Deref(ARG1))));
    }
  case 2:
    {
      CPredicate2 code2 = (CPredicate2)exec_code;
      return ((code2)(Yap_InitSlot(Deref(ARG1)),
		      Yap_InitSlot(Deref(ARG2))));
    }
  case 3:
    {
      CPredicate3 code3 = (CPredicate3)exec_code;
      return ((code3)(Yap_InitSlot(Deref(ARG1)),
		      Yap_InitSlot(Deref(ARG2)),
		      Yap_InitSlot(Deref(ARG3))));
    }
  case 4:
    {
      CPredicate4 code4 = (CPredicate4)exec_code;
      return ((code4)(Yap_InitSlot(Deref(ARG1)),
		      Yap_InitSlot(Deref(ARG2)),
		      Yap_InitSlot(Deref(ARG3)),
		      Yap_InitSlot(Deref(ARG4))));
    }
  case 5:
    {
      CPredicate5 code5 = (CPredicate5)exec_code;
      return ((code5)(Yap_InitSlot(Deref(ARG1)),
		      Yap_InitSlot(Deref(ARG2)),
		      Yap_InitSlot(Deref(ARG3)),
		      Yap_InitSlot(Deref(ARG4)),
		      Yap_InitSlot(Deref(ARG5))));
    }
  case 6:
    {
      CPredicate6 code6 = (CPredicate6)exec_code;
      return ((code6)(Yap_InitSlot(Deref(ARG1)),
		      Yap_InitSlot(Deref(ARG2)),
		      Yap_InitSlot(Deref(ARG3)),
		      Yap_InitSlot(Deref(ARG4)),
		      Yap_InitSlot(Deref(ARG5)),
		      Yap_InitSlot(Deref(ARG6))));
    }
  case 7:
    {
      CPredicate7 code7 = (CPredicate7)exec_code;
      return ((code7)(Yap_InitSlot(Deref(ARG1)),
		      Yap_InitSlot(Deref(ARG2)),
		      Yap_InitSlot(Deref(ARG3)),
		      Yap_InitSlot(Deref(ARG4)),
		      Yap_InitSlot(Deref(ARG5)),
		      Yap_InitSlot(Deref(ARG6)),
		      Yap_InitSlot(Deref(ARG7))));
    }
  case 8:
    {
      CPredicate8 code8 = (CPredicate8)exec_code;
      return ((code8)(Yap_InitSlot(Deref(ARG1)),
		      Yap_InitSlot(Deref(ARG2)),
		      Yap_InitSlot(Deref(ARG3)),
		      Yap_InitSlot(Deref(ARG4)),
		      Yap_InitSlot(Deref(ARG5)),
		      Yap_InitSlot(Deref(ARG6)),
		      Yap_InitSlot(Deref(ARG7)),
		      Yap_InitSlot(Deref(ARG8))));
    }
  default:
    return(FALSE);
  }
}


Int
YAP_Execute(PredEntry *pe, CPredicate exec_code)
{
  if (pe->PredFlags & SWIEnvPredFlag) {
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context ctx;
    ctx.engine = NULL;

    return ((codev)((&ARG1)-LCL0,0,&ctx));
  }
  if (pe->PredFlags & CArgsPredFlag) {
    return execute_cargs(pe, exec_code);
  } else {
    return((exec_code)());
  }
}

#define FRG_REDO_MASK	0x00000003L
#define FRG_REDO_BITS	2
#define REDO_INT	0x02		/* Returned an integer */
#define REDO_PTR	0x03		/* returned a pointer */

Int
YAP_ExecuteFirst(PredEntry *pe, CPredicate exec_code)
{
  if (pe->PredFlags & SWIEnvPredFlag) {
    Int val;
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context *ctx = (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE,1));
    
    ctx->control = FRG_FIRST_CALL;
    ctx->engine = NULL; //(PL_local_data *)Yap_regp;
    ctx->context = NULL;
    val = ((codev)((&ARG1)-LCL0,0,ctx));
    if (val == 0) {
      cut_fail();
    } else if (val == 1) { /* TRUE */
      cut_succeed();
    } else {
      if ((val & REDO_PTR) == REDO_PTR)
	ctx->context = (int *)(val & ~REDO_PTR);
      else
	ctx->context = (int *)((val & ~REDO_PTR)>>FRG_REDO_BITS);
    }
  }
  if (pe->PredFlags & CArgsPredFlag) {
    return execute_cargs(pe, exec_code);
  } else {
    return (exec_code)();
  }
}


Int
YAP_ExecuteNext(PredEntry *pe, CPredicate exec_code)
{
  if (pe->PredFlags & SWIEnvPredFlag) {
    Int val;
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context *ctx = (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE,1));
    
    ctx->control = FRG_REDO;
    val = ((codev)((&ARG1)-LCL0,0,ctx));
    if (val == 0) {
      cut_fail();
    } else if (val == 1) { /* TRUE */
      cut_succeed();
    } else {
      if ((val & REDO_PTR) == REDO_PTR)
	ctx->context = (int *)(val & ~REDO_PTR);
      else
	ctx->context = (int *)((val & ~REDO_PTR)>>FRG_REDO_BITS);
    }
  }
  if (pe->PredFlags & CArgsPredFlag) {
    return execute_cargs(pe, exec_code);
  } else {
    return (exec_code)();
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
YAP_ReallocSpaceFromYap(void *ptr,unsigned int size) {
  void *new_ptr;
  BACKUP_MACHINE_REGS();
  while ((new_ptr = Yap_ExpandPreAllocCodeSpace(size,ptr,TRUE)) == NULL) {
    if (!Yap_growheap(FALSE, size, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return NULL;
    }
  }
  RECOVER_MACHINE_REGS();
  return new_ptr;
}
X_API void *
YAP_AllocSpaceFromYap(unsigned int size)
{
  void *ptr;
  BACKUP_MACHINE_REGS();

  while ((ptr = Yap_AllocCodeSpace(size)) == NULL) {
    if (!Yap_growheap(FALSE, size, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
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
YAP_NBufferToString(char *s, size_t len)
{
  Term t; 
  BACKUP_H();

  t = Yap_NStringToList(s, len);

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_WideBufferToString(wchar_t *s)
{
  Term t; 
  BACKUP_H();

  t = Yap_WideStringToList(s);

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_NWideBufferToString(wchar_t *s, size_t len)
{
  Term t; 
  BACKUP_H();

  t = Yap_NWideStringToList(s, len);

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_ReadBuffer(char *s, Term *tp)
{
  Term t; 
  BACKUP_H();

  while ((t = Yap_StringToTerm(s,tp)) == 0L) {
    if (Yap_ErrorMessage) {
      if (!strcmp(Yap_ErrorMessage,"Stack Overflow")) {
	if (!dogc()) {
	  *tp = MkAtomTerm(Yap_LookupAtom(Yap_ErrorMessage));
	  Yap_ErrorMessage = NULL;
	  RECOVER_H();
	  return 0L;
	} 
      } else if (!strcmp(Yap_ErrorMessage,"Heap Overflow")) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  *tp = MkAtomTerm(Yap_LookupAtom(Yap_ErrorMessage));
	  Yap_ErrorMessage = NULL;
	  RECOVER_H();
	  return 0L;
	}
      } else if (!strcmp(Yap_ErrorMessage,"Trail Overflow")) {
	if (!Yap_growtrail (0, FALSE)) {
	  *tp = MkAtomTerm(Yap_LookupAtom(Yap_ErrorMessage));
	  Yap_ErrorMessage = NULL;
	  RECOVER_H();
	  return 0L;
	}
      } else {
	*tp = MkAtomTerm(Yap_LookupAtom(Yap_ErrorMessage));
	Yap_ErrorMessage = NULL;
	RECOVER_H();
	return 0L;
      }
      Yap_ErrorMessage = NULL;
      continue;
    } else {
      break;
    }
  }
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

/* copy a string of size len to a buffer */
X_API Term
YAP_NBufferToAtomList(char *s, size_t len)
{
  Term t; 
  BACKUP_H();

  t = Yap_NStringToListOfAtoms(s, len);

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_WideBufferToAtomList(wchar_t *s)
{
  Term t; 
  BACKUP_H();

  t = Yap_WideStringToListOfAtoms(s);

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term
YAP_NWideBufferToAtomList(wchar_t *s, size_t len)
{
  Term t; 
  BACKUP_H();

  t = Yap_NWideStringToListOfAtoms(s, len);

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term
YAP_NWideBufferToAtomDiffList(wchar_t *s, Term t0, size_t len)
{
  Term t; 
  BACKUP_H();

  t = Yap_NWideStringToDiffListOfAtoms(s, t0, len);

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_BufferToDiffList(char *s, Term t0)
{
  Term t; 
  BACKUP_H();

  t = Yap_StringToDiffList(s, t0);

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term
YAP_NBufferToDiffList(char *s, Term t0, size_t len)
{
  Term t; 
  BACKUP_H();

  t = Yap_NStringToDiffList(s, t0, len);

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term
YAP_WideBufferToDiffList(wchar_t *s, Term t0)
{
  Term t; 
  BACKUP_H();

  t = Yap_WideStringToDiffList(s, t0);

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term
YAP_NWideBufferToDiffList(wchar_t *s, Term t0, size_t len)
{
  Term t; 
  BACKUP_H();

  t = Yap_NWideStringToDiffList(s, t0, len);

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

static int myputc (wchar_t ch)
{
  putc(ch,stderr);
  return ch;
}

X_API PredEntry *
YAP_FunctorToPred(Functor func)
{
  return RepPredProp(PredPropByFunc(func, CurrentModule));
}

X_API PredEntry *
YAP_AtomToPred(Atom at)
{
  return RepPredProp(PredPropByAtom(at, CurrentModule));  
}


static int
run_emulator(YAP_dogoalinfo *dgi)
{
  choiceptr myB;
  int out;
  BACKUP_MACHINE_REGS();

  Yap_PrologMode = UserMode;
  out = Yap_absmi(0);
  Yap_PrologMode = UserCCallMode;
  myB = (choiceptr)(LCL0-dgi->b);
  CP = myB->cp_cp;
  if (!out ) {
    /* recover stack */
    /* on failed computations */
    TR = B->cp_tr;
    H = B->cp_h;
#ifdef DEPTH_LIMIT
    DEPTH = B->cp_depth = DEPTH;
#endif /* DEPTH_LIMIT */
    YENV = ENV = B->cp_env;
    ASP = (CELL *)(B+1);
    B = B->cp_b;
    HB = B->cp_h;
  } else {
    Yap_StartSlots();
  }
  P = dgi->p;
  RECOVER_MACHINE_REGS();
  return out;
}

X_API int
YAP_EnterGoal(PredEntry *pe, Term *ptr, YAP_dogoalinfo *dgi)
{
  UInt i;
  choiceptr myB;
  int out;

  BACKUP_MACHINE_REGS();
  dgi->p = P;
  ptr--;
  i = pe->ArityOfPE;
  while (i>0) {
    XREGS[i] = ptr[i];
    i--;
  }
  P = pe->CodeOfPred;
  /* create a choice-point to be tag new goal */
  myB = (choiceptr)ASP;
  myB--;
  dgi->b = LCL0-(CELL *)myB;
  myB->cp_tr = TR;
  myB->cp_h = HB = H;
  myB->cp_b = B;
#ifdef DEPTH_LIMIT
  myB->cp_depth = DEPTH;
#endif /* DEPTH_LIMIT */
  myB->cp_cp = CP;
  myB->cp_ap = NOCODE;
  myB->cp_env = ENV;
  CP = YESCODE;
  B = myB;
  HB = H;
#if defined(YAPOR) || defined(THREADS)
  WPP = NULL;
#endif
  ASP = YENV = (CELL *)B;
  YENV[E_CB] = Unsigned (B);
  out = run_emulator(dgi);
  RECOVER_MACHINE_REGS();
  return out;
}

X_API int
YAP_RetryGoal(YAP_dogoalinfo *dgi)
{
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
  out = run_emulator(dgi);
  RECOVER_MACHINE_REGS();
  return out;
}

X_API int
YAP_LeaveGoal(int backtrack, YAP_dogoalinfo *dgi)
{
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
    H = B->cp_h;
    TR = B->cp_tr;
#ifdef DEPTH_LIMIT
    DEPTH = B->cp_depth;
#endif /* DEPTH_LIMIT */
    YENV = ENV = B->cp_env;
  } else {
    Yap_TrimTrail();
  }
  /* recover local stack */
  ASP = (CELL *)(B+1);
  B = B->cp_b;
  HB = B->cp_h;
  P = dgi->p;
  RECOVER_MACHINE_REGS();
  return TRUE;
}

X_API Term
YAP_RunGoal(Term t)
{
  Term out;
  yamop *old_CP = CP;
  BACKUP_MACHINE_REGS();

  Yap_AllowRestart = FALSE;
  Yap_PrologMode = UserMode;
  out = Yap_RunTopGoal(t);
  Yap_PrologMode = UserCCallMode;
  if (out) {
    P = (yamop *)ENV[E_CP];
    ENV = (CELL *)ENV[E_E];
    CP = old_CP;
    Yap_AllowRestart = TRUE;
  } else {
    ENV = B->cp_env;
    B = B->cp_b;
    Yap_AllowRestart = FALSE;
  }
  
  RECOVER_MACHINE_REGS();
  return(out);
}

X_API Term
YAP_RunGoalOnce(Term t)
{
  Term out;
  yamop *old_CP = CP;
  BACKUP_MACHINE_REGS();

  Yap_PrologMode = UserMode;
  out = Yap_RunTopGoal(t);
  Yap_PrologMode = UserCCallMode;
  if (out) {
    choiceptr cut_pt;

    cut_pt = B;
    while (cut_pt-> cp_ap != NOCODE) {
      cut_pt = cut_pt->cp_b;
    }
#ifdef YAPOR
    CUT_prune_to(cut_pt);
#endif
    B = cut_pt;
    Yap_TrimTrail();
  }
  ASP = B->cp_env;
  ENV = (CELL *)ASP[E_E];
  B = (choiceptr)ASP[E_CB];
#ifdef  DEPTH_LIMIT
  DEPTH = ASP[E_DEPTH];
#endif
  P = (yamop *)ASP[E_CP];
  CP = old_CP;
  Yap_AllowRestart = FALSE;
  RECOVER_MACHINE_REGS();
  return(out);
}

X_API int
YAP_RestartGoal(void)
{
  int out;
  BACKUP_MACHINE_REGS();
  
  if (Yap_AllowRestart) {
    P = (yamop *)FAILCODE;
    do_putcf = myputc;
    Yap_PrologMode = UserMode;
    out = Yap_exec_absmi(TRUE);
    Yap_PrologMode = UserCCallMode;
    if (out == FALSE) {
      /* cleanup */
      Yap_trust_last();
      Yap_AllowRestart = FALSE;
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
  BACKUP_MACHINE_REGS();
  
  if (Yap_AllowRestart) {
    choiceptr cut_pt;

    cut_pt = B;
    while (cut_pt-> cp_ap != NOCODE) {
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
      H = cut_pt->cp_h;
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
    Yap_AllowRestart = FALSE;
  }
  RECOVER_MACHINE_REGS();
  return TRUE;
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

  Yap_TrimTrail();
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
  return out;
}

X_API void
YAP_ClearExceptions(void)
{
  EX = 0L;
  UncaughtThrow = FALSE;
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
  Term t, tpos = TermNil;
  int sno;
  TokEntry *tokstart;
  
  BACKUP_MACHINE_REGS();

  do_getf = mygetc;
  sno = Yap_GetFreeStreamDForReading();
  if (sno < 0) {
    Yap_Error(SYSTEM_ERROR,TermNil, "new stream not available for YAP_Read");
    return TermNil;
  }
  Stream[sno].stream_getc = do_yap_getc;
  Stream[sno].status |= Tty_Stream_f;
  tokstart = Yap_tokptr = Yap_toktide = Yap_tokenizer(sno, &tpos);
  Stream[sno].status = Free_Stream_f;
  if (Yap_ErrorMessage)
    {
      Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
      RECOVER_MACHINE_REGS();
      return 0;
    }
  t = Yap_Parse();
  Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);

  RECOVER_MACHINE_REGS();
  return t;
}

X_API void
YAP_Write(Term t, int (*myputc)(wchar_t), int flags)
{
  BACKUP_MACHINE_REGS();

  do_putcf = myputc;		/*  */
  Yap_plwrite (t, do_yap_putc, flags, 1200);

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

  /* allow expansion during stack initialization */
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
  CELL Trail = 0, Stack = 0, Heap = 0, Atts = 0;

  Yap_argv = yap_init->Argv;
  Yap_argc = yap_init->Argc;
#ifdef USE_SYSTEM_MALLOC
  if (yap_init->SavedState) {
    fprintf(stderr,"[ WARNING: threaded Yap will ignore saved state %s ]\n",yap_init->SavedState);
    yap_init->SavedState = NULL;
  }    
#endif
  if (yap_init->SavedState != NULL ||
      yap_init->YapPrologBootFile == NULL) {
    if (Yap_SavedInfo (yap_init->SavedState, yap_init->YapLibDir, &Trail, &Stack, &Heap) != 1) {
      yap_init->ErrorNo = Yap_Error_TYPE;
      yap_init->ErrorCause = Yap_ErrorMessage;
      return YAP_BOOT_FROM_SAVED_ERROR;
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
  Yap_PrologShouldHandleInterrupts = yap_init->PrologShouldHandleInterrupts;
  Yap_InitWorkspace(Heap, Stack, Trail, Atts,
	      yap_init->MaxTableSpaceSize,
	      yap_init->NumberWorkers,
	      yap_init->SchedulerLoop,
	      yap_init->DelayedReleaseLoad
	      );
#if USE_SYSTEM_MALLOC
  if (Trail < MinTrailSpace)
    Trail = MinTrailSpace;
  if (Stack < MinStackSpace)
    Stack = MinStackSpace;
  if (!(Yap_GlobalBase = (ADDR)malloc((Trail+Stack)*1024))) {
    yap_init->ErrorNo = RESOURCE_ERROR_MEMORY;
    yap_init->ErrorCause = "could not allocate stack space for main thread";
    return YAP_BOOT_FROM_SAVED_ERROR;;
  }
#endif
  Yap_AllowGlobalExpansion = TRUE;
  Yap_AllowLocalExpansion = TRUE;
  Yap_AllowTrailExpansion = TRUE;
  Yap_InitExStacks (Trail, Stack);
  if (yap_init->QuietMode) {
    yap_flags[QUIET_MODE_FLAG] = TRUE;
  }

{ BACKUP_MACHINE_REGS();
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
  /* tell the system who should cope with interruptions */
  Yap_ExecutionMode = yap_init->ExecutionMode;
  if (yap_init->SavedState != NULL ||
      yap_init->YapPrologBootFile == NULL) {
#if SUPPORT_CONDOR || SUPPORT_THREADS
    restore_result = YAP_FULL_BOOT_FROM_PROLOG;
#else
    restore_result = Yap_Restore(yap_init->SavedState, yap_init->YapLibDir);
    if (restore_result == FAIL_RESTORE) {
      yap_init->ErrorNo = Yap_Error_TYPE;
      yap_init->ErrorCause = Yap_ErrorMessage;
      /* shouldn't RECOVER_MACHINE_REGS();  be here ??? */
      return YAP_BOOT_FROM_SAVED_ERROR;
    }
#endif
  } else {
    restore_result = FAIL_RESTORE;
  }
  yap_flags[FAST_BOOT_FLAG] = yap_init->FastBoot;
#if defined(YAPOR) || defined(TABLING)
  make_root_frames();
#ifdef YAPOR
  init_workers();
#endif /* YAPOR */
  Yap_init_local();
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
    Yap_Error(INTERNAL_ERROR, TermNil, "abstract machine unexpected exit (YAP_Init)");
  }
#endif /* YAPOR */
#endif /* YAPOR || TABLING */
  RECOVER_MACHINE_REGS();
 }
  /* make sure we do this after restore */
  if (yap_init->MaxStackSize) {
    Yap_AllowLocalExpansion = FALSE;
  } else {
    Yap_AllowLocalExpansion = TRUE;
  }
  if (yap_init->MaxGlobalSize) {
    Yap_AllowGlobalExpansion = FALSE;
  } else {
    Yap_AllowGlobalExpansion = TRUE;
  }
  if (yap_init->MaxTrailSize) {
    Yap_AllowTrailExpansion = FALSE;
  } else {
    Yap_AllowTrailExpansion = TRUE;
  }
  if (yap_init->YapPrologRCFile) {
    Yap_PutValue(AtomConsultOnBoot, MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologRCFile)));
    /*
      This must be done again after restore, as yap_flags
      has been overwritten ....
    */
    yap_flags[HALT_AFTER_CONSULT_FLAG] = yap_init->HaltAfterConsult;
  }
#ifdef MYDDAS_MYSQL
  if (yap_init->myddas) {
    Yap_PutValue(AtomMyddasGoal,MkIntegerTerm(yap_init->myddas));
    
    /* Mandatory Fields */
    Yap_PutValue(AtomMyddasUser,MkAtomTerm(Yap_LookupAtom(yap_init->myddas_user)));
    Yap_PutValue(AtomMyddasDB,MkAtomTerm(Yap_LookupAtom(yap_init->myddas_db)));
    
    /* Non-Mandatory Fields */
    if (yap_init->myddas_pass != NULL)
      Yap_PutValue(AtomMyddasPass,MkAtomTerm(Yap_LookupAtom(yap_init->myddas_pass)));
    if (yap_init->myddas_host != NULL)
      Yap_PutValue(AtomMyddasHost,MkAtomTerm(Yap_LookupAtom(yap_init->myddas_host)));
  }
#endif
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
  if (yap_init->SavedState != NULL ||
      yap_init->YapPrologBootFile == NULL) {
    if (restore_result == FAIL_RESTORE) {
      yap_init->ErrorNo = Yap_Error_TYPE;
      yap_init->ErrorCause = Yap_ErrorMessage;
      return YAP_BOOT_FROM_SAVED_ERROR;
    }
    if (Atts && Atts*1024 > 2048*sizeof(CELL))
      Yap_AttsSize = Atts*1024;
    else
      Yap_AttsSize = 2048*sizeof(CELL);
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
  init_args.Argc = 0;
  init_args.Argv = NULL;
  init_args.ErrorNo = 0;
  init_args.ErrorCause = NULL;
  init_args.QuietMode = FALSE;
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

X_API void
YAP_FlushAllStreams(void)
{
  BACKUP_H();

  Yap_FlushStreams();

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
#ifdef CUT_C
  Yap_InitCPredBackCut(name, arity, extra, init, cont, NULL ,UserCPredFlag);
#else
  Yap_InitCPredBack(name, arity, extra, init, cont, UserCPredFlag);
#endif

}

#ifdef CUT_C
X_API void 
YAP_UserBackCutCPredicate(char *name, CPredicate init, CPredicate cont, CPredicate cut,
			  unsigned long int arity, unsigned int extra)
{
  Yap_InitCPredBackCut(name, arity, extra, init, cont, cut, UserCPredFlag);
}
#endif


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
#if USE_THREADS
  return Yap_thread_self();
#else
  return 0;
#endif
} 

X_API CELL
YAP_ThreadCreateEngine(struct thread_attr_struct *attr)
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

X_API Term
YAP_TermNil(void)
{
  return TermNil;
} 

X_API int
YAP_AtomGetHold(Atom at)
{
  return Yap_AtomGetHold(at);
} 

X_API int
YAP_AtomReleaseHold(Atom at)
{
  return Yap_AtomReleaseHold(at);
} 

X_API Agc_hook
YAP_AGCRegisterHook(Agc_hook hook)
{
  Agc_hook old = AGCHook;
  AGCHook = hook;
  return old;
} 

X_API char *
YAP_cwd(void)
{
  char *buf;
  int len;
  if (!Yap_getcwd(Yap_FileNameBuf, YAP_FILENAME_MAX))
    return FALSE;
  len = strlen(Yap_FileNameBuf);
  buf = Yap_AllocCodeSpace(len+1);
  if (!buf)
    return NULL;
  strncpy(buf, Yap_FileNameBuf, len);
  return buf;
}

X_API Term
YAP_OpenList(int n)
{
  Term t;
  BACKUP_H();

  if (H+2*n > ASP-1024) {
    if (!dogc()) {
      RECOVER_H();
      return FALSE;
    }
  }
  t = AbsPair(H);
  H += 2*n;

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
  t = Deref(t);
  if (!IsVarTerm(t))
    return FALSE;
  return (VarOfTerm(t) < H0);
}

X_API Term
YAP_AttsOfVar(Term t)
{
  attvar_record *attv;
  
  t = Deref(t);
  if (!IsVarTerm(t))
    return TermNil;
  if (VarOfTerm(t) >= H0)
    return TermNil;
  attv = (attvar_record *)VarOfTerm(t);
  return attv->Atts;
}

X_API Term
YAP_TermHash(Term t)
{
  attvar_record *attv;
  
  t = Deref(t);
  if (!IsVarTerm(t))
    return TermNil;
  if (VarOfTerm(t) >= H0)
    return TermNil;
  attv = (attvar_record *)VarOfTerm(t);
  return attv->Atts;
}

X_API int
YAP_FileNoFromStream(Term t)
{

 t = Deref(t);
 if (IsVarTerm(t))
   return -1;
 return Yap_StreamToFileNo(t);
}

X_API void *
YAP_FileDescriptorFromStream(Term t)
{
 
  t = Deref(t);
  if (IsVarTerm(t))
    return NULL;
  return Yap_FileDescriptorFromStream(t);
}

