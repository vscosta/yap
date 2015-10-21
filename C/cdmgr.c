/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		cdmgr.c							 *
* comments:	Code manager						 *
*									 *
* Last rev:     $Date: 2008-07-22 23:34:44 $,$Author: vsc $
**
* $Log: not supported by cvs2svn $
* Revision 1.230  2008/06/02 17:20:28  vsc
* fix abolish bug
*
* Revision 1.229  2008/05/28 17:18:35  vsc
* thread fixes
*
* Revision 1.228  2008/04/28 23:02:32  vsc
* fix bug in current_predicate/2
* fix bug in c_interface.
*
* Revision 1.227  2008/04/11 16:30:27  ricroc
* *** empty log message ***
*
* Revision 1.226  2008/04/01 22:28:41  vsc
* put YAPOR back to life.
*
* Revision 1.225  2008/04/01 08:42:45  vsc
* fix restore and small VISTA thingies
*
* Revision 1.224  2008/03/31 22:56:21  vsc
* more fixes
*
* Revision 1.223  2008/03/25 16:45:53  vsc
* make or-parallelism compile again
*
* Revision 1.222  2008/03/24 23:48:47  vsc
* fix maximum number of threads open error
*
* Revision 1.221  2008/03/22 23:35:00  vsc
* fix bug in all_calls
*
* Revision 1.220  2008/03/17 18:31:16  vsc
* fix breakage in module system
* disable stack writing in error for now
*
* Revision 1.219  2008/02/22 15:08:33  vsc
* Big update to support more SICStus/SWI like message handling
* fix YAPSHAREDIR
* fix yap.tex (Bernd)
*
* Revision 1.218  2008/01/23 17:57:44  vsc
* valgrind it!
* enable atom garbage collection.
*
* Revision 1.217  2007/12/26 19:50:40  vsc
* new version of clp(fd)
* fix deadlock with empty args facts in clause/2.
*
* Revision 1.216  2007/12/23 22:48:44  vsc
* recover stack space
*
* Revision 1.215  2007/12/18 17:46:58  vsc
* purge_clauses does not need to do anything if there are no clauses
* fix gprof bugs.
*
* Revision 1.214  2007/11/28 23:52:14  vsc
* junction tree algorithm
*
* Revision 1.213  2007/11/26 23:43:07  vsc
* fixes to support threads and assert correctly, even if inefficiently.
*
* Revision 1.212  2007/11/16 14:58:40  vsc
* implement sophisticated operations with matrices.
*
* Revision 1.211  2007/11/08 09:53:01  vsc
* YAP would always say the system has tabling!
*
* Revision 1.210  2007/11/07 09:25:27  vsc
* speedup meta-calls
*
* Revision 1.209  2007/11/06 17:02:11  vsc
* compile ground terms away.
*
* Revision 1.208  2007/11/01 10:01:35  vsc
* fix uninitalised lock and reconsult test.
*
* Revision 1.207  2007/10/29 22:48:54  vsc
* small fixes
*
* Revision 1.206  2007/04/10 22:13:20  vsc
* fix max modules limitation
*
* Revision 1.205  2007/03/26 15:18:43  vsc
* debugging and clause/3 over tabled predicates would kill YAP.
*
* Revision 1.204  2007/01/25 22:11:55  vsc
* all/3 should fail on no solutions.
* get rid of annoying gcc complaints.
*
* Revision 1.203  2007/01/24 10:01:38  vsc
* fix matrix mess
*
* Revision 1.202  2006/12/27 01:32:37  vsc
* diverse fixes
*
* Revision 1.201  2006/12/13 16:10:14  vsc
* several debugger and CLP(BN) improvements.
*
* Revision 1.200  2006/11/27 17:42:02  vsc
* support for UNICODE, and other bug fixes.
*
* Revision 1.199  2006/11/15 00:13:36  vsc
* fixes for indexing code.
*
* Revision 1.198  2006/11/14 11:42:25  vsc
* fix bug in growstack
*
* Revision 1.197  2006/11/06 18:35:03  vsc
* 1estranha
*
* Revision 1.196  2006/10/16 17:12:48  vsc
* fixes for threaded version.
*
* Revision 1.195  2006/10/11 17:24:36  vsc
* make sure we only follow pointers *before* we removed the respective code
*block,
* ie don't kill the child before checking pointers from parent!
*
* Revision 1.194  2006/10/11 15:08:03  vsc
* fix bb entries
* comment development code for timestamp overflow.
*
* Revision 1.193  2006/10/11 14:53:57  vsc
* fix memory leak
* fix overflow handling
* VS: ----------------------------------------------------------------------
*
* Revision 1.192  2006/10/10 14:08:16  vsc
* small fixes on threaded implementation.
*
* Revision 1.191  2006/09/20 20:03:51  vsc
* improve indexing on floats
* fix sending large lists to DB
*
* Revision 1.190  2006/08/07 18:51:44  vsc
* fix garbage collector not to try to garbage collect when we ask for large
* chunks of stack in a single go.
*
* Revision 1.189  2006/05/24 02:35:39  vsc
* make chr work and other minor fixes.
*
* Revision 1.188  2006/05/18 16:33:04  vsc
* fix info reported by memory manager under DL_MALLOC and SYSTEM_MALLOC
*
* Revision 1.187  2006/04/29 01:15:18  vsc
* fix expand_consult patch
*
* Revision 1.186  2006/04/28 17:53:44  vsc
* fix the expand_consult patch
*
* Revision 1.185  2006/04/28 13:23:22  vsc
* fix number of overflow bugs affecting threaded version
* make current_op faster.
*
* Revision 1.184  2006/04/27 14:11:57  rslopes
* *** empty log message ***
*
* Revision 1.183  2006/03/29 16:00:10  vsc
* make tabling compile
*
* Revision 1.182  2006/03/24 16:26:26  vsc
* code review
*
* Revision 1.181  2006/03/22 20:07:28  vsc
* take better care of zombies
*
* Revision 1.180  2006/03/22 16:14:20  vsc
* don't be too eager at throwing indexing code for static predicates away.
*
* Revision 1.179  2006/03/21 17:11:39  vsc
* prevent breakage
*
* Revision 1.178  2006/03/20 19:51:43  vsc
* fix indexing and tabling bugs
*
* Revision 1.177  2006/03/06 14:04:56  vsc
* fixes to garbage collector
* fixes to debugger
*
* Revision 1.176  2006/02/01 13:28:56  vsc
* bignum support fixes
*
* Revision 1.175  2006/01/08 03:12:00  vsc
* fix small bug in attvar handling.
*
* Revision 1.174  2005/12/23 00:20:13  vsc
* updates to gprof
* support for __POWER__
* Try to saveregs before _longjmp.
*
* Revision 1.173  2005/12/17 03:25:39  vsc
* major changes to support online event-based profiling
* improve error discovery and restart on scanner.
*
* Revision 1.172  2005/11/23 03:01:33  vsc
* fix several bugs in save/restore.b
*
* Revision 1.171  2005/10/29 01:28:37  vsc
* make undefined more ISO compatible.
*
* Revision 1.170  2005/10/18 17:04:43  vsc
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
* Revision 1.169  2005/10/15 02:05:57  vsc
* fix for trying to add clauses to a C pred.
*
* Revision 1.168  2005/08/05 14:55:02  vsc
* first steps to allow mavars with tabling
* fix trailing for tabling with multiple get_cons
*
* Revision 1.167  2005/08/02 03:09:49  vsc
* fix debugger to do well nonsource predicates.
*
* Revision 1.166  2005/08/01 15:40:37  ricroc
* TABLING NEW: better support for incomplete tabling
*
* Revision 1.165  2005/07/06 19:33:52  ricroc
* TABLING: answers for completed calls can now be obtained by loading (new
*option) or executing (default) them from the trie data structure.
*
* Revision 1.164  2005/07/06 15:10:03  vsc
* improvements to compiler: merged instructions and fixes for ->
*
* Revision 1.163  2005/06/08 00:35:27  vsc
* fix silly calls such as 0.15 ( bug reported by Jude Shavlik)
*
* Revision 1.162  2005/06/04 07:27:33  ricroc
* long int support for tabling
*
* Revision 1.161  2005/06/03 08:26:32  ricroc
* float support for tabling
*
* Revision 1.160  2005/06/01 14:02:47  vsc
* get_rid of try_me?, retry_me? and trust_me? instructions: they are not
* significantly used nowadays.
*
* Revision 1.159  2005/05/31 19:42:27  vsc
* insert some more slack for indices in LU
* Use doubly linked list for LU indices so that updating is less cumbersome.
*
* Revision 1.158  2005/05/31 00:30:23  ricroc
* remove abort_yapor function
*
* Revision 1.157  2005/05/12 03:36:32  vsc
* debugger was making predicates meta instead of testing
* fix handling of dbrefs in facts and in subarguments.
*
* Revision 1.156  2005/04/20 04:02:15  vsc
* fix a few variable warnings
* fix erase clause to pass a pointer to clause, not code
* get rid of Yap4.4 code in Yap_EraseStaticClause
*
* Revision 1.155  2005/04/10 04:01:10  vsc
* bug fixes, I hope!
*
* Revision 1.154  2005/03/04 20:30:11  ricroc
* bug fixes for YapTab support
*
* Revision 1.153  2005/02/25 03:39:44  vsc
* fix fixes to undefp
* fix bug where clause mistook cp for ap
*
* Revision 1.152  2005/02/08 18:04:57  vsc
* library_directory may not be deterministic (usually it isn't).
*
* Revision 1.151  2005/02/08 04:05:23  vsc
* fix mess with add clause
* improves on sigsegv handling
*
* Revision 1.150  2005/01/28 23:14:34  vsc
* move to Yap-4.5.7
* Fix clause size
*
* Revision 1.149  2005/01/05 05:35:01  vsc
* get rid of debugging stub.
*
* Revision 1.148  2005/01/04 02:50:21  vsc
* - allow MegaClauses with blobs
* - change Diffs to be thread specific
* - include Christian's updates
*
* Revision 1.147  2004/12/28 22:20:35  vsc
* some extra bug fixes for trail overflows: some cannot be recovered that
*easily,
* some can.
*
* Revision 1.146  2004/12/20 21:44:57  vsc
* more fixes to CLPBN
* fix some Yap overflows.
*
* Revision 1.145  2004/12/16 05:57:23  vsc
* fix overflows
*
* Revision 1.144  2004/12/08 00:10:48  vsc
* more grow fixes
*
* Revision 1.143  2004/12/05 05:01:23  vsc
* try to reduce overheads when running with goal expansion enabled.
* CLPBN fixes
* Handle overflows when allocating big clauses properly.
*
* Revision 1.142  2004/11/18 22:32:31  vsc
* fix situation where we might assume nonextsing double initialization of C
*predicates (use
* Hidden Pred Flag).
* $host_type was double initialised.
*
* Revision 1.141  2004/11/04 18:22:31  vsc
* don't ever use memory that has been freed (that was done by LU).
* generic fixes for WIN32 libraries
*
* Revision 1.140  2004/10/31 02:18:03  vsc
* fix bug in handling Yap heap overflow while adding new clause.
*
* Revision 1.139  2004/10/28 20:12:21  vsc
* Use Doug Lea's malloc as an alternative to YAP's standard malloc
* don't use TR directly in scanner/parser, this avoids trouble with ^C while
* consulting large files.
* pass gcc -mno-cygwin to library compilation in cygwin environment (cygwin
*should
* compile out of the box now).
*
* Revision 1.138  2004/10/26 20:15:51  vsc
* More bug fixes for overflow handling
*
* Revision 1.137  2004/10/22 16:53:19  vsc
* bug fixes
*
* Revision 1.136  2004/10/06 16:55:46  vsc
* change configure to support big mem configs
* get rid of extra globals
* fix trouble with multifile preds
*
* Revision 1.135  2004/09/30 21:37:40  vsc
* fixes for thread support
*
* Revision 1.134  2004/09/30 19:51:53  vsc
* fix overflow from within clause/2
*
* Revision 1.133  2004/09/27 20:45:02  vsc
* Mega clauses
* Fixes to sizeof(expand_clauses) which was being overestimated
* Fixes to profiling+indexing
* Fixes to reallocation of memory after restoring
* Make sure all clauses, even for C, end in _Ystop
* Don't reuse space for Streams
* Fix Stream_F on StreaNo+1
*
* Revision 1.132  2004/09/17 19:34:51  vsc
* simplify frozen/2
*
* Revision 1.131  2004/09/08 17:56:45  vsc
* source: a(X) :- true is a fact!
* fix use of value after possible overflow in IPred
*
* Revision 1.130  2004/09/07 16:48:04  vsc
* fix bug in unwinding trail at amiops.h
*
* Revision 1.129  2004/09/07 16:25:22  vsc
* memory management bug fixes
*
* Revision 1.128  2004/09/03 03:11:07  vsc
* memory management fixes
*
* Revision 1.127  2004/08/16 21:02:04  vsc
* more fixes for !
*
* Revision 1.126  2004/07/22 21:32:20  vsc
* debugger fixes
* initial support for JPL
* bad calls to garbage collector and gc
* debugger fixes
*
* Revision 1.125  2004/06/29 19:04:41  vsc
* fix multithreaded version
* include new version of Ricardo's profiler
* new predicat atomic_concat
* allow multithreaded-debugging
* small fixes
*
* Revision 1.124  2004/06/05 03:36:59  vsc
* coroutining is now a part of attvars.
* some more fixes.
*
* Revision 1.123  2004/05/17 21:42:09  vsc
* misc fixes
*
* Revision 1.122  2004/05/13 21:36:45  vsc
* get rid of pesky debugging prints
*
* Revision 1.121  2004/05/13 20:54:57  vsc
* debugger fixes
* make sure we always go back to current module, even during initizlization.
*
* Revision 1.120  2004/04/27 16:21:16  vsc
* stupid bug
*
* Revision 1.119  2004/04/27 15:03:43  vsc
* more fixes for expand_clauses
*
* Revision 1.118  2004/04/14 19:10:23  vsc
* expand_clauses: keep a list of clauses to expand
* fix new trail scheme for multi-assignment variables
*
* Revision 1.117  2004/04/07 22:04:03  vsc
* fix memory leaks
*
* Revision 1.116  2004/03/31 01:03:09  vsc
* support expand group of clauses
*
* Revision 1.115  2004/03/19 11:35:42  vsc
* trim_trail for default machine
* be more aggressive about try-retry-trust chains.
*    - handle cases where block starts with a wait
*    - don't use _killed instructions, just let the thing rot by itself.
*
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "@(#)cdmgr.c	1.1 05/02/98";
#endif

#include "Yap.h"
#include "clause.h"
#include "yapio.h"
#include "eval.h"
#include "tracer.h"
#ifdef YAPOR
#include "or.macros.h"
#endif /* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#if HAVE_STRING_H
#include <string.h>
#endif
#include <heapgc.h>
#include <iopreds.h>

static void retract_all(PredEntry *, int);
static void add_first_static(PredEntry *, yamop *, int);
static void add_first_dynamic(PredEntry *, yamop *, int);
static void asserta_stat_clause(PredEntry *, yamop *, int);
static void asserta_dynam_clause(PredEntry *, yamop *);
static void assertz_stat_clause(PredEntry *, yamop *, int);
static void assertz_dynam_clause(PredEntry *, yamop *);
static void expand_consult(void);
static int not_was_reconsulted(PredEntry *, Term, int);
static int RemoveIndexation(PredEntry *);
static Int p_number_of_clauses(USES_REGS1);
static Int p_compile(USES_REGS1);
static Int p_compile_dynamic(USES_REGS1);
static Int p_purge_clauses(USES_REGS1);
static Int p_setspy(USES_REGS1);
static Int p_rmspy(USES_REGS1);
static Int p_startconsult(USES_REGS1);
static Int p_showconslultlev(USES_REGS1);
static Int p_endconsult(USES_REGS1);
static Int p_undefined(USES_REGS1);
static Int p_new_multifile(USES_REGS1);
static Int p_is_multifile(USES_REGS1);
static Int p_optimizer_on(USES_REGS1);
static Int p_optimizer_off(USES_REGS1);
static Int p_is_dynamic(USES_REGS1);
static Int p_kill_dynamic(USES_REGS1);
static Int p_compile_mode(USES_REGS1);
static Int p_is_profiled(USES_REGS1);
static Int p_profile_info(USES_REGS1);
static Int p_profile_reset(USES_REGS1);
static Int p_is_call_counted(USES_REGS1);
static Int p_call_count_info(USES_REGS1);
static Int p_call_count_set(USES_REGS1);
static Int p_call_count_reset(USES_REGS1);
static void kill_first_log_iblock(LogUpdIndex *, LogUpdIndex *, PredEntry *);

#define PredArity(p) (p->ArityOfPE)
#define TRYCODE(G, F, N) ((N) < 5 ? (op_numbers)((int)F + (N)*3) : G)

static void InitConsultStack(void) {
  CACHE_REGS
  LOCAL_ConsultLow = (consult_obj *)Yap_AllocCodeSpace(sizeof(consult_obj) *
                                                       InitialConsultCapacity);
  if (LOCAL_ConsultLow == NULL) {
    Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "No Heap Space in InitCodes");
    return;
  }
  LOCAL_ConsultCapacity = InitialConsultCapacity;
  LOCAL_ConsultBase = LOCAL_ConsultSp =
      LOCAL_ConsultLow + LOCAL_ConsultCapacity;
}

void Yap_ResetConsultStack(void) {
  CACHE_REGS
  Yap_FreeCodeSpace((char *)LOCAL_ConsultLow);
  LOCAL_ConsultBase = LOCAL_ConsultSp = LOCAL_ConsultLow = NULL;
  LOCAL_ConsultCapacity = InitialConsultCapacity;
}

/******************************************************************

                ADDING AND REMOVE INFO TO A PROCEDURE

******************************************************************/

/*
 * we have three kinds of predicates: dynamic		DynamicPredFlag
 * static 		CompiledPredFlag fast		FastPredFlag all the
 * database predicates are supported for dynamic predicates only abolish and
 * assertz are supported for static predicates no database predicates are
 * supportted for fast predicates
 */

#define is_dynamic(pe) (pe->PredFlags & DynamicPredFlag)
#define is_static(pe) (pe->PredFlags & CompiledPredFlag)
#define is_logupd(pe) (pe->PredFlags & LogUpdatePredFlag)
#ifdef TABLING
#define is_tabled(pe) (pe->PredFlags & TabledPredFlag)
#endif /* TABLING */

static PredEntry *get_pred(Term t, Term tmod, char *pname) {
  Term t0 = t;

restart:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t0, pname);
    return NULL;
  } else if (IsAtomTerm(t)) {
    return RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t), tmod));
  } else if (IsIntegerTerm(t) && tmod == IDB_MODULE) {
    return Yap_FindLUIntKey(IntegerOfTerm(t));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    if (IsExtensionFunctor(fun)) {
      Yap_Error(TYPE_ERROR_CALLABLE, Yap_PredicateIndicator(t, tmod), pname);
      return NULL;
    }
    if (fun == FunctorModule) {
      Term tmod = ArgOfTerm(1, t);
      if (IsVarTerm(tmod)) {
        Yap_Error(INSTANTIATION_ERROR, t0, pname);
        return NULL;
      }
      if (!IsAtomTerm(tmod)) {
        Yap_Error(TYPE_ERROR_ATOM, t0, pname);
        return NULL;
      }
      t = ArgOfTerm(2, t);
      goto restart;
    }
    return RepPredProp(Yap_GetPredPropByFunc(fun, tmod));
  } else
    return NULL;
}

/******************************************************************

                Mega Clauses

******************************************************************/

#define OrArgAdjust(P)
#define TabEntryAdjust(P)
#define DoubleInCodeAdjust(D)
#define IntegerInCodeAdjust(D)
#define IntegerAdjust(D) (D)
#define PtoPredAdjust(X) (X)
#define PtoOpAdjust(X) (X)
#define PtoLUClauseAdjust(P) (P)
#define PtoLUIndexAdjust(P) (P)
#define XAdjust(X) (X)
#define YAdjust(X) (X)
#define AtomTermAdjust(X) (X)
#define CellPtoHeapAdjust(X) (X)
#define FuncAdjust(X) (X)
#define CodeAddrAdjust(X) (X)
#define CodeComposedTermAdjust(X) (X)
#define ConstantAdjust(X) (X)
#define ArityAdjust(X) (X)
#define OpcodeAdjust(X) (X)
#define ModuleAdjust(X) (X)
#define ExternalFunctionAdjust(X) (X)
#define AdjustSwitchTable(X, Y, Z)
#define DBGroundTermAdjust(X) (X)
#define rehash(A, B, C)

static Term BlobTermInCodeAdjust(Term t) {
  CACHE_REGS
#if TAGS_FAST_OPS
  return t - LOCAL_ClDiff;
#else
  return t + LOCAL_ClDiff;
#endif
}

static Term ConstantTermAdjust(Term t) {
  if (IsAtomTerm(t))
    return AtomTermAdjust(t);
  return t;
}

#include "rclause.h"

#ifdef DEBUG
static UInt total_megaclause, total_released, nof_megaclauses;
#endif

void Yap_BuildMegaClause(PredEntry *ap) {
  CACHE_REGS
  StaticClause *cl;
  UInt sz;
  MegaClause *mcl;
  yamop *ptr;
  size_t required;
  UInt has_blobs = 0;

  if (ap->PredFlags & (DynamicPredFlag | LogUpdatePredFlag | MegaClausePredFlag
#ifdef TABLING
                       | TabledPredFlag
#endif /* TABLING */
                       | UDIPredFlag) ||
      ap->cs.p_code.FirstClause == NULL || ap->cs.p_code.NOfClauses < 16) {
    return;
  }
  cl = ClauseCodeToStaticClause(ap->cs.p_code.FirstClause);
  sz = cl->ClSize;
  while (TRUE) {
    if (!(cl->ClFlags & FactMask))
      return; /* no mega clause, sorry */
    if (cl->ClSize != sz)
      return; /* no mega clause, sorry */
    if (cl->ClCode == ap->cs.p_code.LastClause)
      break;
    has_blobs |= (cl->ClFlags & HasBlobsMask);
    cl = cl->ClNext;
  }
  /* ok, we got the chance for a mega clause */
  if (has_blobs) {
    sz -= sizeof(StaticClause);
  } else {
    sz -= (UInt)NEXTOP((yamop *)NULL, p) + sizeof(StaticClause);
  }
  required = sz * ap->cs.p_code.NOfClauses + sizeof(MegaClause) +
             (UInt)NEXTOP((yamop *)NULL, l);
  while (!(mcl = (MegaClause *)Yap_AllocCodeSpace(required))) {
    if (!Yap_growheap(FALSE, required, NULL)) {
      /* just fail, the system will keep on going */
      return;
    }
  }
#ifdef DEBUG
  total_megaclause += required;
  cl = ClauseCodeToStaticClause(ap->cs.p_code.FirstClause);
  total_released += ap->cs.p_code.NOfClauses * cl->ClSize;
  nof_megaclauses++;
#endif
  Yap_ClauseSpace += required;
  /* cool, it's our turn to do the conversion */
  mcl->ClFlags = MegaMask | has_blobs;
  mcl->ClSize = required;
  mcl->ClPred = ap;
  mcl->ClItemSize = sz;
  mcl->ClNext = NULL;
  cl = ClauseCodeToStaticClause(ap->cs.p_code.FirstClause);
  mcl->ClLine = cl->usc.ClLine;
  ptr = mcl->ClCode;
  while (TRUE) {
    memcpy((void *)ptr, (void *)cl->ClCode, sz);
    if (has_blobs) {
      LOCAL_ClDiff = (char *)(ptr) - (char *)cl->ClCode;
      restore_opcodes(ptr, NULL PASS_REGS);
    }
    ptr = (yamop *)((char *)ptr + sz);
    if (cl->ClCode == ap->cs.p_code.LastClause)
      break;
    cl = cl->ClNext;
  }
  ptr->opc = Yap_opcode(_Ystop);
  cl = ClauseCodeToStaticClause(ap->cs.p_code.FirstClause);
  /* recover the space spent on the original clauses */
  while (TRUE) {
    StaticClause *ncl, *curcl = cl;

    ncl = cl->ClNext;
    Yap_InformOfRemoval(cl);
    Yap_ClauseSpace -= cl->ClSize;
    Yap_FreeCodeSpace((ADDR)cl);
    if (curcl->ClCode == ap->cs.p_code.LastClause)
      break;
    cl = ncl;
  }
  ap->cs.p_code.FirstClause = ap->cs.p_code.LastClause = mcl->ClCode;
  ap->PredFlags |= MegaClausePredFlag;
  Yap_inform_profiler_of_clause(mcl, (char *)mcl + required, ap, GPROF_MEGA);
}

static void split_megaclause(PredEntry *ap) {
  StaticClause *start = NULL, *prev = NULL;
  MegaClause *mcl;
  yamop *ptr;
  UInt ncls = ap->cs.p_code.NOfClauses, i;

  mcl = ClauseCodeToMegaClause(ap->cs.p_code.FirstClause);
  if (mcl->ClFlags & ExoMask) {
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, TermNil,
              "while deleting clause from exo predicate %s/%d\n",
              RepAtom(NameOfFunctor(ap->FunctorOfPred))->StrOfAE,
              ap->ArityOfPE);
    return;
  }
  RemoveIndexation(ap);
  for (i = 0, ptr = mcl->ClCode; i < ncls; i++) {
    StaticClause *new = (StaticClause *)Yap_AllocCodeSpace(
        sizeof(StaticClause) + mcl->ClItemSize +
        (UInt)NEXTOP((yamop *)NULL, p));
    if (new == NULL) {
      if (!Yap_growheap(FALSE,
                        (sizeof(StaticClause) + mcl->ClItemSize) * (ncls - i),
                        NULL)) {
        while (start) {
          StaticClause *cl = start;
          start = cl->ClNext;
          Yap_InformOfRemoval(cl);
          Yap_ClauseSpace -= cl->ClSize;
          Yap_FreeCodeSpace((char *)cl);
        }
        if (ap->ArityOfPE) {
          Yap_Error(RESOURCE_ERROR_HEAP, TermNil,
                    "while breaking up mega clause for %s/%d\n",
                    RepAtom(NameOfFunctor(ap->FunctorOfPred))->StrOfAE,
                    ap->ArityOfPE);
        } else {
          Yap_Error(RESOURCE_ERROR_HEAP, TermNil,
                    "while breaking up mega clause for %s\n",
                    RepAtom((Atom)ap->FunctorOfPred)->StrOfAE);
        }
        return;
      }
      break;
    }
    Yap_ClauseSpace +=
        sizeof(StaticClause) + mcl->ClItemSize + (UInt)NEXTOP((yamop *)NULL, p);
    new->ClFlags = StaticMask | FactMask;
    new->ClSize = mcl->ClItemSize;
    new->usc.ClLine = Yap_source_line_no();
    new->ClNext = NULL;
    memcpy((void *)new->ClCode, (void *)ptr, mcl->ClItemSize);
    if (prev) {
      prev->ClNext = new;
    } else {
      start = new;
    }
    ptr = (yamop *)((char *)ptr + mcl->ClItemSize);
    prev = new;
  }
  ap->PredFlags &= ~MegaClausePredFlag;
  ap->cs.p_code.FirstClause = start->ClCode;
  ap->cs.p_code.LastClause = prev->ClCode;
}

/******************************************************************

                Indexation Info

******************************************************************/
#define ByteAdr(X) ((Int) & (X))

/* Index a prolog pred, given its predicate entry */
/* ap is already locked. */
static void IPred(PredEntry *ap, UInt NSlots, yamop *next_pc) {
  yamop *BaseAddr;

#ifdef DEBUG
  CACHE_REGS
  if (GLOBAL_Option['i' - 'a' + 1]) {
    Term tmod = ap->ModuleOfPred;
    if (!tmod)
      tmod = TermProlog;
    Yap_DebugPutc(stderr, '\t');
    Yap_DebugPlWrite(tmod);
    Yap_DebugPutc(stderr, ':');
    if (ap->ModuleOfPred == IDB_MODULE) {
      Term t = Deref(ARG1);
      if (IsAtomTerm(t)) {
        Yap_DebugPlWrite(t);
      } else if (IsIntegerTerm(t)) {
        Yap_DebugPlWrite(t);
      } else {
        Functor f = FunctorOfTerm(t);
        Atom At = NameOfFunctor(f);
        Yap_DebugPlWrite(MkAtomTerm(At));
        Yap_DebugPutc(stderr, '/');
        Yap_DebugPlWrite(MkIntTerm(ArityOfFunctor(f)));
      }
    } else {
      if (ap->ArityOfPE == 0) {
        Atom At = (Atom)ap->FunctorOfPred;
        Yap_DebugPlWrite(MkAtomTerm(At));
      } else {
        Functor f = ap->FunctorOfPred;
        Atom At = NameOfFunctor(f);
        Yap_DebugPlWrite(MkAtomTerm(At));
        Yap_DebugPutc(stderr, '/');
        Yap_DebugPlWrite(MkIntTerm(ArityOfFunctor(f)));
      }
    }
    Yap_DebugPutc(stderr, '\n');
  }
#endif
  /* Do not try to index a dynamic predicate  or one whithout args */
  if (is_dynamic(ap)) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "trying to index a dynamic predicate");
    return;
  }
  if ((BaseAddr = Yap_PredIsIndexable(ap, NSlots, next_pc)) != NULL) {
    ap->cs.p_code.TrueCodeOfPred = BaseAddr;
    ap->PredFlags |= IndexedPredFlag;
  }
  if (ap->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
#if defined(YAPOR) || defined(THREADS)
  } else if (ap->PredFlags & LogUpdatePredFlag &&
             !(ap->PredFlags & ThreadLocalPredFlag) &&
             ap->ModuleOfPred != IDB_MODULE) {
    ap->OpcodeOfPred = LOCKPRED_OPCODE;
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
#endif
  } else {
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred;
    ap->OpcodeOfPred = ap->CodeOfPred->opc;
  }
#ifdef DEBUG
  if (GLOBAL_Option['i' - 'a' + 1])
    Yap_DebugPutc(stderr, '\n');
#endif
}

void Yap_IPred(PredEntry *p, UInt NSlots, yamop *next_pc) {
  IPred(p, NSlots, next_pc);
}

#define GONEXT(TYPE) code_p = ((yamop *)(&(code_p->y_u.TYPE.next)))

static void RemoveMainIndex(PredEntry *ap) {
  yamop *First = ap->cs.p_code.FirstClause;
  int spied =
      ap->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag);

  ap->PredFlags &= ~IndexedPredFlag;
  if (First == NULL) {
    ap->cs.p_code.TrueCodeOfPred = FAILCODE;
  } else {
    ap->cs.p_code.TrueCodeOfPred = First;
  }
  if (First != NULL && spied) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
  } else if (ap->cs.p_code.NOfClauses > 1
#ifdef TABLING
             || ap->PredFlags & TabledPredFlag
#endif /* TABLING */
             ) {
    ap->OpcodeOfPred = INDEX_OPCODE;
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred =
        (yamop *)(&(ap->OpcodeOfPred));
  } else {
    ap->OpcodeOfPred = ap->cs.p_code.TrueCodeOfPred->opc;
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred;
  }
#if defined(YAPOR) || defined(THREADS)
  if (ap->PredFlags & LogUpdatePredFlag &&
      !(ap->PredFlags & ThreadLocalPredFlag) &&
      ap->ModuleOfPred != IDB_MODULE) {
    ap->OpcodeOfPred = LOCKPRED_OPCODE;
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
  }
#endif
}

static void decrease_ref_counter(yamop *ptr, yamop *b, yamop *e, yamop *sc) {
  if (ptr != FAILCODE && ptr != sc && (ptr < b || ptr > e)) {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(ptr);
    cl->ClRefCount--;
    if (cl->ClFlags & ErasedMask && !(cl->ClRefCount) &&
        !(cl->ClFlags & InUseMask)) {
      /* last ref to the clause */
      Yap_ErLogUpdCl(cl);
    }
  }
}

static yamop *release_wcls(yamop *cop, OPCODE ecs) {
  if (cop->opc == ecs) {
    cop->y_u.sssllp.s3--;
    if (!cop->y_u.sssllp.s3) {
      UInt sz = (UInt)NEXTOP((yamop *)NULL, sssllp) +
                cop->y_u.sssllp.s1 * sizeof(yamop *);
      LOCK(ExpandClausesListLock);
#ifdef DEBUG
      Yap_expand_clauses_sz -= sz;
      Yap_ExpandClauses--;
#endif
      if (cop->y_u.sssllp.p->PredFlags & LogUpdatePredFlag) {
        Yap_LUIndexSpace_EXT -= sz;
      } else {
        Yap_IndexSpace_EXT -= sz;
      }
      if (ExpandClausesFirst == cop)
        ExpandClausesFirst = cop->y_u.sssllp.snext;
      if (ExpandClausesLast == cop) {
        ExpandClausesLast = cop->y_u.sssllp.sprev;
      }
      if (cop->y_u.sssllp.sprev) {
        cop->y_u.sssllp.sprev->y_u.sssllp.snext = cop->y_u.sssllp.snext;
      }
      if (cop->y_u.sssllp.snext) {
        cop->y_u.sssllp.snext->y_u.sssllp.sprev = cop->y_u.sssllp.sprev;
      }
      UNLOCK(ExpandClausesListLock);
      Yap_InformOfRemoval(cop);
      Yap_FreeCodeSpace((char *)cop);
    }
  }
  return FAILCODE;
}

static void cleanup_dangling_indices(yamop *ipc, yamop *beg, yamop *end,
                                     yamop *suspend_code) {
  OPCODE ecs = Yap_opcode(_expand_clauses);

  while (ipc) {
    op_numbers op = Yap_op_from_opcode(ipc->opc);
    /*    fprintf(stderr,"op: %d %p->%p\n", op, ipc, end);*/
    switch (op) {
    case _Ystop:
      /* end of clause, for now */
      return;
    case _index_dbref:
    case _index_blob:
    case _index_long:
      ipc = NEXTOP(ipc, e);
      break;
    case _lock_lu:
    case _unlock_lu:
      /* locking should be done already */
      ipc = NEXTOP(ipc, e);
    case _retry_profiled:
    case _count_retry:
      ipc = NEXTOP(ipc, p);
      break;
    case _try_clause2:
    case _try_clause3:
    case _try_clause4:
      ipc = NEXTOP(ipc, l);
      break;
    case _retry2:
    case _retry3:
    case _retry4:
      decrease_ref_counter(ipc->y_u.l.l, beg, end, suspend_code);
      ipc = NEXTOP(ipc, l);
      break;
    case _retry:
    case _trust:
      decrease_ref_counter(ipc->y_u.Otapl.d, beg, end, suspend_code);
      ipc = NEXTOP(ipc, Otapl);
      break;
    case _try_clause:
    case _try_me:
    case _retry_me:
    case _profiled_trust_me:
    case _trust_me:
    case _count_trust_me:
      ipc = NEXTOP(ipc, Otapl);
      break;
    case _try_logical:
    case _retry_logical:
    case _count_retry_logical:
    case _profiled_retry_logical: {
      yamop *oipc = ipc;
      decrease_ref_counter(ipc->y_u.OtaLl.d->ClCode, beg, end, suspend_code);
      ipc = ipc->y_u.OtaLl.n;
      Yap_LUIndexSpace_CP -= (UInt)NEXTOP((yamop *)NULL, OtaLl);
      Yap_FreeCodeSpace((ADDR)oipc);
#ifdef DEBUG
      Yap_DirtyCps--;
      Yap_FreedCps++;
#endif
    } break;
    case _trust_logical:
    case _count_trust_logical:
    case _profiled_trust_logical:
#ifdef DEBUG
      Yap_DirtyCps--;
      Yap_FreedCps++;
#endif
      decrease_ref_counter(ipc->y_u.OtILl.d->ClCode, beg, end, suspend_code);
      Yap_LUIndexSpace_CP -= (UInt)NEXTOP((yamop *)NULL, OtILl);
      Yap_FreeCodeSpace((ADDR)ipc);
      return;
    case _enter_lu_pred: {
      yamop *oipc = ipc;
      if (ipc->y_u.Illss.I->ClFlags & InUseMask || ipc->y_u.Illss.I->ClRefCount)
        return;
#ifdef DEBUG
      Yap_DirtyCps += ipc->y_u.Illss.s;
      Yap_LiveCps -= ipc->y_u.Illss.s;
#endif
      ipc = ipc->y_u.Illss.l1;
      /* in case we visit again */
      oipc->y_u.Illss.l1 = FAILCODE;
      oipc->y_u.Illss.s = 0;
      oipc->y_u.Illss.e = 0;
    } break;
    case _try_in:
    case _jump:
    case _jump_if_var:
      ipc->y_u.l.l = release_wcls(ipc->y_u.l.l, ecs);
      ipc = NEXTOP(ipc, l);
      break;
    /* instructions type xl */
    case _jump_if_nonvar:
      ipc->y_u.xll.l1 = release_wcls(ipc->y_u.xll.l1, ecs);
      ipc = NEXTOP(ipc, xll);
      break;
    /* instructions type p */
    case _user_switch:
      ipc = NEXTOP(ipc, lp);
      break;
    /* instructions type e */
    case _switch_on_type:
      ipc->y_u.llll.l1 = release_wcls(ipc->y_u.llll.l1, ecs);
      ipc->y_u.llll.l2 = release_wcls(ipc->y_u.llll.l2, ecs);
      ipc->y_u.llll.l3 = release_wcls(ipc->y_u.llll.l3, ecs);
      ipc->y_u.llll.l4 = release_wcls(ipc->y_u.llll.l4, ecs);
      ipc = NEXTOP(ipc, llll);
      break;
    case _switch_list_nl:
      ipc->y_u.ollll.l1 = release_wcls(ipc->y_u.ollll.l1, ecs);
      ipc->y_u.ollll.l2 = release_wcls(ipc->y_u.ollll.l2, ecs);
      ipc->y_u.ollll.l3 = release_wcls(ipc->y_u.ollll.l3, ecs);
      ipc->y_u.ollll.l4 = release_wcls(ipc->y_u.ollll.l4, ecs);
      ipc = NEXTOP(ipc, ollll);
      break;
    case _switch_on_arg_type:
      ipc->y_u.xllll.l1 = release_wcls(ipc->y_u.xllll.l1, ecs);
      ipc->y_u.xllll.l2 = release_wcls(ipc->y_u.xllll.l2, ecs);
      ipc->y_u.xllll.l3 = release_wcls(ipc->y_u.xllll.l3, ecs);
      ipc->y_u.xllll.l4 = release_wcls(ipc->y_u.xllll.l4, ecs);
      ipc = NEXTOP(ipc, xllll);
      break;
    case _switch_on_sub_arg_type:
      ipc->y_u.sllll.l1 = release_wcls(ipc->y_u.sllll.l1, ecs);
      ipc->y_u.sllll.l2 = release_wcls(ipc->y_u.sllll.l2, ecs);
      ipc->y_u.sllll.l3 = release_wcls(ipc->y_u.sllll.l3, ecs);
      ipc->y_u.sllll.l4 = release_wcls(ipc->y_u.sllll.l4, ecs);
      ipc = NEXTOP(ipc, sllll);
      break;
    case _if_not_then:
      ipc = NEXTOP(ipc, clll);
      break;
    case _switch_on_func:
    case _if_func:
    case _go_on_func:
    case _switch_on_cons:
    case _if_cons:
    case _go_on_cons:
      /* make sure we don't leave dangling references to memory that is going to
       * be removed */
      ipc->y_u.sssl.l = NULL;
      ipc = NEXTOP(ipc, sssl);
      break;
    case _op_fail:
      return;
    default:
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "Bug in Indexing Code: opcode %d", op);
      return;
    }
#if defined(YAPOR) || defined(THREADS)
    ipc = (yamop *)((CELL)ipc & ~1);
#endif
  }
}

static void decrease_log_indices(LogUpdIndex *c, yamop *suspend_code) {
  /* decrease all reference counters */
  yamop *beg = c->ClCode, *end, *ipc;

  if (c->ClFlags & SwitchTableMask) {
    CELL *end = (CELL *)((char *)c + c->ClSize);
    CELL *beg = (CELL *)(c->ClCode);
    OPCODE ecs = Yap_opcode(_expand_clauses);

    while (beg < end) {
      yamop **x = (yamop **)(beg + 1);
      beg += 2;
      *x = release_wcls(*x, ecs);
    }
    return;
  }
  end = (yamop *)((CODEADDR)c + c->ClSize);
  ipc = beg;
  cleanup_dangling_indices(ipc, beg, end, suspend_code);
}

static void kill_static_child_indxs(StaticIndex *indx, int in_use) {
  StaticIndex *cl = indx->ChildIndex;
  while (cl != NULL) {
    StaticIndex *next = cl->SiblingIndex;
    kill_static_child_indxs(cl, in_use);
    cl = next;
  }
  if (in_use) {
    LOCK(DeadStaticIndicesLock);
    indx->SiblingIndex = DeadStaticIndices;
    indx->ChildIndex = NULL;
    DeadStaticIndices = indx;
    UNLOCK(DeadStaticIndicesLock);
  } else {
    Yap_InformOfRemoval(indx);
    if (indx->ClFlags & SwitchTableMask)
      Yap_IndexSpace_SW -= indx->ClSize;
    else
      Yap_IndexSpace_Tree -= indx->ClSize;
    Yap_FreeCodeSpace((char *)indx);
  }
}

static void kill_children(LogUpdIndex *c, PredEntry *ap) {
  LogUpdIndex *ncl;

  c->ClRefCount++;
  ncl = c->ChildIndex;
  /* kill children */
  while (ncl) {
    kill_first_log_iblock(ncl, c, ap);
    ncl = c->ChildIndex;
  }
  c->ClRefCount--;
}

/* assumes c is already locked */
static void kill_off_lu_block(LogUpdIndex *c, LogUpdIndex *parent,
                              PredEntry *ap) {
  /* first, make sure that I killed off all my children, some children may
     remain in case I have tables as children */
  if (parent != NULL) {
    /* sat bye bye */
    /* decrease refs */
    parent->ClRefCount--;
    if (parent->ClFlags & ErasedMask && !(parent->ClFlags & InUseMask) &&
        parent->ClRefCount == 0) {
      /* cool, I can erase the father too. */
      if (parent->ClFlags & SwitchRootMask) {
        kill_off_lu_block(parent, NULL, ap);
      } else {
        kill_off_lu_block(parent, parent->ParentIndex, ap);
      }
    }
  }
  decrease_log_indices(c, (yamop *)&(ap->cs.p_code.ExpandCode));
  /* remove from list */
  if (c->SiblingIndex)
    c->SiblingIndex->PrevSiblingIndex = c->PrevSiblingIndex;
  if (c->PrevSiblingIndex) {
    c->PrevSiblingIndex->SiblingIndex = c->SiblingIndex;
  } else {
    DBErasedIList = c->SiblingIndex;
  }
  Yap_InformOfRemoval(c);
  if (c->ClFlags & SwitchTableMask)
    Yap_LUIndexSpace_SW -= c->ClSize;
  else {
    Yap_LUIndexSpace_Tree -= c->ClSize;
  }
  Yap_FreeCodeSpace((char *)c);
}

static void kill_first_log_iblock(LogUpdIndex *c, LogUpdIndex *parent,
                                  PredEntry *ap) {
  /* parent is always locked, now I lock myself */
  if (parent != NULL) {
    /* remove myself from parent */
    if (c == parent->ChildIndex) {
      parent->ChildIndex = c->SiblingIndex;
      if (parent->ChildIndex) {
        parent->ChildIndex->PrevSiblingIndex = NULL;
      }
    } else {
      c->PrevSiblingIndex->SiblingIndex = c->SiblingIndex;
      if (c->SiblingIndex) {
        c->SiblingIndex->PrevSiblingIndex = c->PrevSiblingIndex;
      }
    }
  } else {
    /* I am  top node */
    if (ap->cs.p_code.TrueCodeOfPred == c->ClCode) {
      RemoveMainIndex(ap);
    }
  }
  decrease_log_indices(c, (yamop *)&(ap->cs.p_code.ExpandCode));
  /* make sure that a child cannot remove us */
  kill_children(c, ap);
  /* check if we are still the main index */
  /* always add to erased list */
  c->SiblingIndex = DBErasedIList;
  c->PrevSiblingIndex = NULL;
  if (DBErasedIList)
    DBErasedIList->PrevSiblingIndex = c;
  DBErasedIList = c;
  if (!((c->ClFlags & InUseMask) || c->ClRefCount)) {
    kill_off_lu_block(c, parent, ap);
  } else {
    if (c->ClFlags & ErasedMask)
      return;
    c->ClFlags |= ErasedMask;
    /* try to move up, so that we don't hold a switch table */
    if (parent != NULL && parent->ClFlags & SwitchTableMask) {

      c->ParentIndex = parent->ParentIndex;
      parent->ParentIndex->ClRefCount++;
      parent->ClRefCount--;
    }
  }
}

static void kill_top_static_iblock(StaticIndex *c, PredEntry *ap) {
  kill_static_child_indxs(c, Yap_static_in_use(ap, TRUE));
  RemoveMainIndex(ap);
}

void Yap_kill_iblock(ClauseUnion *blk, ClauseUnion *parent_blk, PredEntry *ap) {
  if (ap->PredFlags & LogUpdatePredFlag) {
    LogUpdIndex *c = (LogUpdIndex *)blk;
    if (parent_blk != NULL) {
      LogUpdIndex *cl = (LogUpdIndex *)parent_blk;
#if MULTIPLE_STACKS
      /* protect against attempts at erasing */
      cl->ClRefCount++;
#endif
      kill_first_log_iblock(c, cl, ap);
#if MULTIPLE_STACKS
      cl->ClRefCount--;
#endif
    } else {
      kill_first_log_iblock(c, NULL, ap);
    }
  } else {
    StaticIndex *c = (StaticIndex *)blk;
    if (parent_blk != NULL) {
      StaticIndex *cl = parent_blk->si.ChildIndex;
      if (cl == c) {
        parent_blk->si.ChildIndex = c->SiblingIndex;
      } else {
        while (cl->SiblingIndex != c) {
          cl = cl->SiblingIndex;
        }
        cl->SiblingIndex = c->SiblingIndex;
      }
    }
    kill_static_child_indxs(c, Yap_static_in_use(ap, TRUE));
  }
}

/*
  This predicate is supposed to be called with a
  lock on the current predicate
*/
void Yap_ErLogUpdIndex(LogUpdIndex *clau) {
  if (clau->ClFlags & ErasedMask) {
    if (!clau->ClRefCount) {
      decrease_log_indices(clau,
                           (yamop *)&(clau->ClPred->cs.p_code.ExpandCode));
      if (clau->ClFlags & SwitchRootMask) {
        kill_off_lu_block(clau, NULL, clau->ClPred);
      } else {
        kill_off_lu_block(clau, clau->ParentIndex, clau->ClPred);
      }
    }
    /* otherwise, nothing I can do, I have been erased already */
    return;
  }
  if (clau->ClFlags & SwitchRootMask) {
    kill_first_log_iblock(clau, NULL, clau->ClPred);
  } else {
#if MULTIPLE_STACKS
    /* protect against attempts at erasing */
    clau->ClRefCount++;
#endif
    kill_first_log_iblock(clau, clau->ParentIndex, clau->ClPred);
#if MULTIPLE_STACKS
    /* protect against attempts at erasing */
    clau->ClRefCount--;
#endif
  }
}

/* Routine used when wanting to remove the indexation */
/* ap is known to already have been locked for WRITING */
static int RemoveIndexation(PredEntry *ap) {
  if (ap->OpcodeOfPred == INDEX_OPCODE) {
    return TRUE;
  }
  if (ap->PredFlags & LogUpdatePredFlag) {
    kill_first_log_iblock(ClauseCodeToLogUpdIndex(ap->cs.p_code.TrueCodeOfPred),
                          NULL, ap);
  } else {
    StaticIndex *cl;

    cl = ClauseCodeToStaticIndex(ap->cs.p_code.TrueCodeOfPred);

    kill_top_static_iblock(cl, ap);
  }
  return TRUE;
}

int Yap_RemoveIndexation(PredEntry *ap) { return RemoveIndexation(ap); }
/******************************************************************

                        Adding clauses

******************************************************************/

#define assertz 0
#define consult 1
#define asserta 2

/* p is already locked */
static void retract_all(PredEntry *p, int in_use) {
  yamop *q;

  q = p->cs.p_code.FirstClause;
  if (q != NULL) {
    if (p->PredFlags & LogUpdatePredFlag) {
      LogUpdClause *cl = ClauseCodeToLogUpdClause(q);
      do {
        LogUpdClause *ncl = cl->ClNext;
        Yap_ErLogUpdCl(cl);
        cl = ncl;
      } while (cl != NULL);
    } else if (p->PredFlags & MegaClausePredFlag) {
      MegaClause *cl = ClauseCodeToMegaClause(q);

      if (in_use || cl->ClFlags & HasBlobsMask) {
        LOCK(DeadMegaClausesLock);
        cl->ClNext = DeadMegaClauses;
        DeadMegaClauses = cl;
        UNLOCK(DeadMegaClausesLock);
      } else {
        Yap_InformOfRemoval(cl);
        Yap_ClauseSpace -= cl->ClSize;
        Yap_FreeCodeSpace((char *)cl);
      }
      /* make sure this is not a MegaClause */
      p->PredFlags &= ~MegaClausePredFlag;
      p->cs.p_code.NOfClauses = 0;
    } else {
      StaticClause *cl = ClauseCodeToStaticClause(q);

      while (cl) {
        StaticClause *ncl = cl->ClNext;

        if (in_use || cl->ClFlags & HasBlobsMask) {
          LOCK(DeadStaticClausesLock);
          cl->ClNext = DeadStaticClauses;
          DeadStaticClauses = cl;
          UNLOCK(DeadStaticClausesLock);
        } else {
          Yap_InformOfRemoval(cl);
          Yap_ClauseSpace -= cl->ClSize;
          Yap_FreeCodeSpace((char *)cl);
        }
        p->cs.p_code.NOfClauses--;
        if (!ncl)
          break;
        cl = ncl;
      }
    }
  }
  p->cs.p_code.FirstClause = NULL;
  p->cs.p_code.LastClause = NULL;
  if (p->PredFlags & (DynamicPredFlag | LogUpdatePredFlag | MultiFileFlag)) {
    p->OpcodeOfPred = FAIL_OPCODE;
  } else {
    p->OpcodeOfPred = UNDEF_OPCODE;
  }
  p->cs.p_code.TrueCodeOfPred = p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  if (PROFILING) {
    p->PredFlags |= ProfiledPredFlag;
  } else
    p->PredFlags &= ~ProfiledPredFlag;
  if (CALL_COUNTING) {
    p->PredFlags |= CountPredFlag;
  } else
    p->PredFlags &= ~CountPredFlag;
  Yap_PutValue(AtomAbol, MkAtomTerm(AtomTrue));
}

static int source_pred(PredEntry *p, yamop *q) {
  if (p->PredFlags & (DynamicPredFlag | LogUpdatePredFlag))
    return FALSE;
  if (p->PredFlags & MultiFileFlag)
    return TRUE;
  if (trueGlobalPrologFlag(SOURCE_FLAG)) {
    return TRUE;
  }
  return FALSE;
}

/* p is already locked */
static void add_first_static(PredEntry *p, yamop *cp, int spy_flag) {
  CACHE_REGS
  yamop *pt = cp;

  if (is_logupd(p)) {
    if (p == PredGoalExpansion || p->FunctorOfPred == FunctorGoalExpansion2) {
      PRED_GOAL_EXPANSION_ON = TRUE;
      Yap_InitComma();
    }
  } else {
#ifdef TABLING
    if (is_tabled(p)) {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->cs.p_code.TrueCodeOfPred = p->CodeOfPred =
          (yamop *)(&(p->OpcodeOfPred));
    }
#endif /* TABLING */
  }
  p->cs.p_code.TrueCodeOfPred = pt;
  p->cs.p_code.FirstClause = p->cs.p_code.LastClause = cp;
  p->OpcodeOfPred = pt->opc;
#if defined(YAPOR) || defined(THREADS)
  if (p->PredFlags & LogUpdatePredFlag &&
      !(p->PredFlags & ThreadLocalPredFlag) && p->ModuleOfPred != IDB_MODULE) {
    p->OpcodeOfPred = LOCKPRED_OPCODE;
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  } else
#endif
    p->CodeOfPred = pt;
  p->cs.p_code.NOfClauses = 1;
  if (PROFILING) {
    p->PredFlags |= ProfiledPredFlag;
    spy_flag = TRUE;
  } else {
    p->PredFlags &= ~ProfiledPredFlag;
  }
  if (CALL_COUNTING) {
    p->PredFlags |= CountPredFlag;
    spy_flag = TRUE;
  } else {
    p->PredFlags &= ~CountPredFlag;
  }
  if (spy_flag) {
    p->OpcodeOfPred = Yap_opcode(_spy_pred);
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  }
  if (source_pred(p, cp)) {
    p->PredFlags |= SourcePredFlag;
  }
  if (!(p->PredFlags & MultiFileFlag) && p->src.OwnerFile == AtomNil)
    p->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
}

/* p is already locked */
static void add_first_dynamic(PredEntry *p, yamop *cp, int spy_flag) {
  CACHE_REGS
  yamop *ncp = ((DynamicClause *)NULL)->ClCode;
  DynamicClause *cl;
  if (p == PredGoalExpansion || p->FunctorOfPred == FunctorGoalExpansion2) {
    PRED_GOAL_EXPANSION_ON = TRUE;
    Yap_InitComma();
  }
  if (PROFILING) {
    p->PredFlags |= ProfiledPredFlag;
    spy_flag = TRUE;
  } else {
    p->PredFlags &= ~ProfiledPredFlag;
  }
  if (CALL_COUNTING) {
    p->PredFlags |= CountPredFlag;
    spy_flag = TRUE;
  } else {
    p->PredFlags &= ~CountPredFlag;
  }
#ifdef YAPOR
  p->PredFlags |= SequentialPredFlag;
#endif /* YAPOR */
       /* allocate starter block, containing info needed to start execution,
        * that is a try_mark to start the code and a fail to finish things up */
  cl = (DynamicClause *)Yap_AllocCodeSpace(
      (Int)NEXTOP(NEXTOP(NEXTOP(ncp, Otapl), e), l));
  if (cl == NIL) {
    Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "Heap crashed against Stacks");
    return;
  }
  Yap_ClauseSpace += (Int)NEXTOP(NEXTOP(NEXTOP(ncp, Otapl), e), l);
  /* skip the first entry, this contains the back link and will always be
     empty for this entry */
  ncp = (yamop *)(((CELL *)ncp) + 1);
  /* next we have the flags. For this block mainly say whether we are
   *  being spied */
  cl->ClFlags = DynamicMask;
  ncp = cl->ClCode;
  INIT_LOCK(cl->ClLock);
  INIT_CLREF_COUNT(cl);
  /* next, set the first instruction to execute in the dyamic
   *  predicate */
  if (spy_flag)
    p->OpcodeOfPred = ncp->opc = Yap_opcode(_spy_or_trymark);
  else
    p->OpcodeOfPred = ncp->opc = Yap_opcode(_try_and_mark);
  ncp->y_u.Otapl.s = p->ArityOfPE;
  ncp->y_u.Otapl.p = p;
  ncp->y_u.Otapl.d = cp;
  /* This is the point we enter the code */
  p->cs.p_code.TrueCodeOfPred = p->CodeOfPred = ncp;
  p->cs.p_code.NOfClauses = 1;
#if defined(YAPOR) || defined(THREADS)
  if (p->PredFlags & LogUpdatePredFlag &&
      !(p->PredFlags & ThreadLocalPredFlag) && p->ModuleOfPred != IDB_MODULE) {
    p->OpcodeOfPred = LOCKPRED_OPCODE;
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  }
#endif
  /* set the first clause to have a retry and mark which will
   *  backtrack to the previous block */
  if (p->PredFlags & ProfiledPredFlag)
    cp->opc = Yap_opcode(_profiled_retry_and_mark);
  else if (p->PredFlags & CountPredFlag)
    cp->opc = Yap_opcode(_count_retry_and_mark);
  else
    cp->opc = Yap_opcode(_retry_and_mark);
  cp->y_u.Otapl.s = p->ArityOfPE;
  cp->y_u.Otapl.p = p;
  cp->y_u.Otapl.d = ncp;
  /* also, keep a backpointer for the days you delete the clause */
  ClauseCodeToDynamicClause(cp)->ClPrevious = ncp;
  /* Don't forget to say who is the only clause for the predicate so
     far */
  p->cs.p_code.LastClause = p->cs.p_code.FirstClause = cp;
  /* we're only missing what to do when we actually exit the procedure
   */
  ncp = NEXTOP(ncp, Otapl);
  /* and the last instruction to execute to exit the predicate, note
     the retry is pointing to this pseudo clause */
  ncp->opc = Yap_opcode(_trust_fail);
  /* we're only missing what to do when we actually exit the procedure
   */
  /* and close the code */
  ncp = NEXTOP(ncp, e);
  ncp->opc = Yap_opcode(_Ystop);
  ncp->y_u.l.l = cl->ClCode;
  if (!(p->PredFlags & MultiFileFlag) && p->src.OwnerFile == AtomNil)
    p->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
}

/* p is already locked */
static void asserta_stat_clause(PredEntry *p, yamop *q, int spy_flag) {
  StaticClause *cl = ClauseCodeToStaticClause(q);

  p->cs.p_code.NOfClauses++;
  if (is_logupd(p)) {
    LogUpdClause *clp = ClauseCodeToLogUpdClause(p->cs.p_code.FirstClause),
                 *clq = ClauseCodeToLogUpdClause(q);
    clq->ClPrev = NULL;
    clq->ClNext = clp;
    clp->ClPrev = clq;
    p->cs.p_code.FirstClause = q;
    if (p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
      p->OpcodeOfPred = Yap_opcode(_spy_pred);
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    } else if (!(p->PredFlags & IndexedPredFlag)) {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
#if defined(YAPOR) || defined(THREADS)
    if (p->ModuleOfPred != IDB_MODULE &&
        !(p->PredFlags & ThreadLocalPredFlag)) {
      p->OpcodeOfPred = LOCKPRED_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
#endif
    return;
  }
  cl->ClNext = ClauseCodeToStaticClause(p->cs.p_code.FirstClause);
  p->cs.p_code.FirstClause = q;
  p->cs.p_code.TrueCodeOfPred = q;
  if (p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
    p->OpcodeOfPred = Yap_opcode(_spy_pred);
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  } else if (!(p->PredFlags & IndexedPredFlag)) {
    p->OpcodeOfPred = INDEX_OPCODE;
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  }
}

/* p is already locked */
static void asserta_dynam_clause(PredEntry *p, yamop *cp) {
  yamop *q;
  DynamicClause *cl = ClauseCodeToDynamicClause(cp);
  q = cp;
  LOCK(ClauseCodeToDynamicClause(p->cs.p_code.FirstClause)->ClLock);
  /* also, keep backpointers for the days we'll delete all the clause */
  ClauseCodeToDynamicClause(p->cs.p_code.FirstClause)->ClPrevious = q;
  cl->ClPrevious = (yamop *)(p->CodeOfPred);
  cl->ClFlags |= DynamicMask;
  UNLOCK(ClauseCodeToDynamicClause(p->cs.p_code.FirstClause)->ClLock);
  q->y_u.Otapl.d = p->cs.p_code.FirstClause;
  q->y_u.Otapl.s = p->ArityOfPE;
  q->y_u.Otapl.p = p;
  if (p->PredFlags & ProfiledPredFlag)
    cp->opc = Yap_opcode(_profiled_retry_and_mark);
  else if (p->PredFlags & CountPredFlag)
    cp->opc = Yap_opcode(_count_retry_and_mark);
  else
    cp->opc = Yap_opcode(_retry_and_mark);
  cp->y_u.Otapl.s = p->ArityOfPE;
  cp->y_u.Otapl.p = p;
  p->cs.p_code.FirstClause = cp;
  q = p->CodeOfPred;
  q->y_u.Otapl.d = cp;
  q->y_u.Otapl.s = p->ArityOfPE;
  q->y_u.Otapl.p = p;
}

/* p is already locked */
static void assertz_stat_clause(PredEntry *p, yamop *cp, int spy_flag) {
  yamop *pt;

  p->cs.p_code.NOfClauses++;
  pt = p->cs.p_code.LastClause;
  if (is_logupd(p)) {
    LogUpdClause *clp = ClauseCodeToLogUpdClause(cp),
                 *clq = ClauseCodeToLogUpdClause(pt);

    clq->ClNext = clp;
    clp->ClPrev = clq;
    clp->ClNext = NULL;
    p->cs.p_code.LastClause = cp;
    if (!(p->PredFlags & IndexedPredFlag)) {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->cs.p_code.TrueCodeOfPred = p->CodeOfPred =
          (yamop *)(&(p->OpcodeOfPred));
    }
#if defined(YAPOR) || defined(THREADS)
    if (p->ModuleOfPred != IDB_MODULE &&
        !(p->PredFlags & ThreadLocalPredFlag)) {
      p->OpcodeOfPred = LOCKPRED_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
#endif
    if (p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
      p->OpcodeOfPred = Yap_opcode(_spy_pred);
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
    return;
  } else {
    StaticClause *cl = ClauseCodeToStaticClause(pt);

    cl->ClNext = ClauseCodeToStaticClause(cp);
  }
  if (p->cs.p_code.FirstClause == p->cs.p_code.LastClause) {
    if (!(p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag))) {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
  }
  p->cs.p_code.LastClause = cp;
}

/* p is already locked */
static void assertz_dynam_clause(PredEntry *p, yamop *cp) {
  yamop *q;
  DynamicClause *cl = ClauseCodeToDynamicClause(cp);

  q = p->cs.p_code.LastClause;
  LOCK(ClauseCodeToDynamicClause(q)->ClLock);
  q->y_u.Otapl.d = cp;
  p->cs.p_code.LastClause = cp;
  /* also, keep backpointers for the days we'll delete all the clause */
  cl->ClPrevious = q;
  cl->ClFlags |= DynamicMask;
  UNLOCK(ClauseCodeToDynamicClause(q)->ClLock);
  q = (yamop *)cp;
  if (p->PredFlags & ProfiledPredFlag)
    q->opc = Yap_opcode(_profiled_retry_and_mark);
  else if (p->PredFlags & CountPredFlag)
    q->opc = Yap_opcode(_count_retry_and_mark);
  else
    q->opc = Yap_opcode(_retry_and_mark);
  q->y_u.Otapl.d = p->CodeOfPred;
  q->y_u.Otapl.s = p->ArityOfPE;
  q->y_u.Otapl.p = p;
  p->cs.p_code.NOfClauses++;
}

void Yap_AssertzClause(PredEntry *p, yamop *cp) {
  if (p->PredFlags & DynamicPredFlag) {
    if (p->cs.p_code.FirstClause == NULL) {
      add_first_dynamic(p, cp, FALSE);
    } else {
      assertz_dynam_clause(p, cp);
    }
  } else {
    if (p->cs.p_code.FirstClause == NULL) {
      add_first_static(p, cp, FALSE);
    } else {
      assertz_stat_clause(p, cp, FALSE);
    }
  }
}

static void expand_consult(void) {
  CACHE_REGS
  consult_obj *new_cl, *new_cs;
  UInt OldConsultCapacity = LOCAL_ConsultCapacity;

  /* now double consult capacity */
  LOCAL_ConsultCapacity += InitialConsultCapacity;
  /* I assume it always works ;-) */
  while ((new_cl = (consult_obj *)Yap_AllocCodeSpace(
              sizeof(consult_obj) * LOCAL_ConsultCapacity)) == NULL) {
    if (!Yap_growheap(FALSE, sizeof(consult_obj) * LOCAL_ConsultCapacity,
                      NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
      return;
    }
  }
  new_cs = new_cl + InitialConsultCapacity;
  /* start copying */
  memcpy((void *)new_cs, (void *)LOCAL_ConsultLow,
         OldConsultCapacity * sizeof(consult_obj));
  /* copying done, release old space */
  Yap_FreeCodeSpace((char *)LOCAL_ConsultLow);
  /* next, set up pointers correctly */
  new_cs += (LOCAL_ConsultSp - LOCAL_ConsultLow);
  /* put LOCAL_ConsultBase at same offset as before move */
  LOCAL_ConsultBase = LOCAL_ConsultBase + (new_cs - LOCAL_ConsultSp);
  /* new consult pointer */
  LOCAL_ConsultSp = new_cs;
  /* new end of memory */
  LOCAL_ConsultLow = new_cl;
}

static int not_was_reconsulted(PredEntry *p, Term t, int mode) {
  CACHE_REGS
  register consult_obj *fp;
  Prop p0 = AbsProp((PropEntry *)p);

  if (p == LOCAL_LastAssertedPred)
    return FALSE;
  if (!LOCAL_ConsultSp) {
    InitConsultStack();
  }
  if (p->cs.p_code.NOfClauses) {
    for (fp = LOCAL_ConsultSp; fp < LOCAL_ConsultBase; ++fp)
      if (fp->p == p0)
        break;
  } else {
    fp = LOCAL_ConsultBase;
  }
  if (fp != LOCAL_ConsultBase) {
    LOCAL_LastAssertedPred = p;
    return false;    /* careful */
  } else if (mode) { // consulting again a predicate in the original file.
    if ((p->cs.p_code.NOfClauses &&
         p->src.OwnerFile == Yap_ConsultingFile(PASS_REGS1) &&
         p->src.OwnerFile != AtomNil && !(p->PredFlags & MultiFileFlag) &&
         p->src.OwnerFile != AtomUserIn)) {
      // if (p->ArityOfPE)
      //	printf("+ %s %s
      //%d\n",NameOfFunctor(p->FunctorOfPred)->StrOfAE,p->src.OwnerFile->StrOfAE,
      //p->cs.p_code.NOfClauses);
      retract_all(p, Yap_static_in_use(p, TRUE));
    }
    //	printf("- %s
    //%s\n",NameOfFunctor(p->FunctorOfPred)->StrOfAE,p->src.OwnerFile->StrOfAE);
  }
  if (mode) {
    if (LOCAL_ConsultSp == LOCAL_ConsultLow + 1) {
      expand_consult();
    }
    --LOCAL_ConsultSp;
    LOCAL_ConsultSp->p = p0;
    if (LOCAL_ConsultBase[1].mode &&
        !(p->PredFlags & MultiFileFlag)) /* we are in reconsult mode */ {
      retract_all(p, Yap_static_in_use(p, TRUE));
    }
    p->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  }
  LOCAL_LastAssertedPred = p;
  return TRUE; /* careful */
}

static void addcl_permission_error(AtomEntry *ap, Int Arity, int in_use) {
  CACHE_REGS
  Term t, ti[2];

  ti[0] = MkAtomTerm(AbsAtom(ap));
  ti[1] = MkIntegerTerm(Arity);
  t = Yap_MkApplTerm(FunctorSlash, 2, ti);
  LOCAL_ErrorMessage = LOCAL_ErrorSay;
  LOCAL_Error_Term = t;
  LOCAL_Error_TYPE = PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE;
  if (in_use) {
    if (Arity == 0)
      sprintf(LOCAL_ErrorMessage, "static predicate %s is in use", ap->StrOfAE);
    else
      sprintf(LOCAL_ErrorMessage,
              "static predicate %s/" Int_FORMAT " is in use", ap->StrOfAE,
              Arity);
  } else {
    if (Arity == 0)
      sprintf(LOCAL_ErrorMessage, "system predicate %s", ap->StrOfAE);
    else
      sprintf(LOCAL_ErrorMessage, "system predicate %s/" Int_FORMAT,
              ap->StrOfAE, Arity);
  }
}

PredEntry *Yap_PredFromClause(Term t USES_REGS) {
  Term cmod = LOCAL_SourceModule;
  arity_t extra_arity = 0;

  if (IsVarTerm(t))
    return NULL;
  while (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorModule) {
      // module
      cmod = ArgOfTerm(1, t);
      if (!IsAtomTerm(cmod))
        return NULL;
      t = ArgOfTerm(2, t);
    } else if (f == FunctorAssert) {
      t = ArgOfTerm(1, t);
    } else if (f == FunctorComma && extra_arity == 2) {
      t = ArgOfTerm(1, t);
    } else if (f == FunctorDoubleArrow) {
      extra_arity = 2;
      t = ArgOfTerm(1, t);
    } else if (f == FunctorQuery || f == FunctorAssert1) {
      // directives
      return NULL;
    } else {
      if (extra_arity) {
        f = Yap_MkFunctor(NameOfFunctor(f), ArityOfFunctor(f) + 2);
      }
      return RepPredProp(Yap_GetPredPropByFunc(f, cmod));
    }
  }
  if (IsAtomTerm(t)) {
    if (extra_arity) {
      Functor f = Yap_MkFunctor(AtomOfTerm(t), 2);
      return RepPredProp(Yap_GetPredPropByFunc(f, cmod));
    }
    return RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t), cmod));
  }
  // ints, lists

  return NULL;
}

bool Yap_discontiguous(PredEntry *ap USES_REGS) {
  register consult_obj *fp;

  if (ap->PredFlags & (DiscontiguousPredFlag | MultiFileFlag))
    return false;
  if (!LOCAL_ConsultSp) {
    return false;
  }
  if (ap == LOCAL_LastAssertedPred)
    return FALSE;
  if (ap->cs.p_code.NOfClauses) {
    for (fp = LOCAL_ConsultSp; fp < LOCAL_ConsultBase; ++fp)
      if (fp->p == AbsPredProp(ap))
        return true;
  }
  return false;
}

bool Yap_multiple(PredEntry *ap, int mode USES_REGS) {
  register consult_obj *fp;

  if ((ap->PredFlags & (MultiFileFlag | LogUpdatePredFlag | DynamicPredFlag)) ||
      mode == consult)
    return false;
  if (LOCAL_consult_level == 0)
    return false;
  for (fp = LOCAL_ConsultSp; fp < LOCAL_ConsultBase; ++fp)
    if (fp->p == AbsPredProp(ap)) {
      return false;
    }
  return ap->cs.p_code.NOfClauses > 0 && ap->src.OwnerFile != AtomNil &&
         Yap_ConsultingFile(PASS_REGS1) != ap->src.OwnerFile &&
         LOCAL_Including != MkAtomTerm(ap->src.OwnerFile);
}

static int is_fact(Term t) {
  Term a1;

  if (IsAtomTerm(t))
    return TRUE;
  if (FunctorOfTerm(t) != FunctorAssert)
    return TRUE;
  a1 = ArgOfTerm(2, t);
  if (a1 == MkAtomTerm(AtomTrue))
    return TRUE;
  return FALSE;
}

static void mark_preds_with_this_func(Functor f, Prop p0) {
  PredEntry *pe = RepPredProp(p0);

  pe->PredFlags |= GoalExPredFlag;
  Prop p = f->PropsOfFE;

  while (p) {
    Prop nextp = p->NextOfPE;
    pe = RepPredProp(p);

    pe->PredFlags |= GoalExPredFlag;
    p = nextp;
  }
}

static void mark_preds_with_this_atom(Prop p) {
  while (p) {
    Prop nextp = p->NextOfPE;
    if (p->KindOfPE == PEProp)
      RepPredProp(p)->PredFlags |= GoalExPredFlag;
    p = nextp;
  }
}

static void goal_expansion_support(PredEntry *p, Term tf) {
  if (p == PredGoalExpansion) {
    Term tg = ArgOfTerm(1, tf);
    Term tm = ArgOfTerm(2, tf);

    if (IsVarTerm(tg) || IsVarTerm(tm)) {
      if (!IsVarTerm(tg)) {
        /* this is the complicated case, first I need to inform
           predicates for this functor */
        PRED_GOAL_EXPANSION_FUNC = TRUE;
        if (IsAtomTerm(tg)) {
          AtomEntry *ae = RepAtom(AtomOfTerm(tg));
          Prop p0 = ae->PropsOfAE;
          int found = FALSE;

          while (p0) {
            PredEntry *pe = RepPredProp(p0);
            if (pe->KindOfPE == PEProp) {
              pe->PredFlags |= GoalExPredFlag;
              found = TRUE;
            }
            p0 = pe->NextOfPE;
          }
          if (!found) {
            PredEntry *npe =
                RepPredProp(PredPropByAtom(AtomOfTerm(tg), IDB_MODULE));
            npe->PredFlags |= GoalExPredFlag;
          }
        } else if (IsApplTerm(tg)) {
          FunctorEntry *fe = (FunctorEntry *)FunctorOfTerm(tg);
          Prop p0;

          p0 = fe->PropsOfFE;
          if (p0) {
            mark_preds_with_this_func(FunctorOfTerm(tg), p0);
          } else {
            CACHE_REGS
            Term mod = CurrentModule;
            PredEntry *npe;
            if (CurrentModule == PROLOG_MODULE)
              mod = IDB_MODULE;
            npe = RepPredProp(PredPropByFunc(fe, mod));
            npe->PredFlags |= GoalExPredFlag;
          }
        }
      } else {
        PRED_GOAL_EXPANSION_ALL = TRUE;
      }
    } else {
      if (IsAtomTerm(tm)) {
        if (IsAtomTerm(tg)) {
          PredEntry *p = RepPredProp(PredPropByAtom(AtomOfTerm(tg), tm));
          p->PredFlags |= GoalExPredFlag;
        } else if (IsApplTerm(tg)) {
          PredEntry *p = RepPredProp(PredPropByFunc(FunctorOfTerm(tg), tm));
          p->PredFlags |= GoalExPredFlag;
        }
      }
    }
  } else if (p->FunctorOfPred == FunctorGoalExpansion2) {
    Term tg = ArgOfTerm(1, tf);

    if (IsVarTerm(tg)) {
      PRED_GOAL_EXPANSION_ALL = TRUE;
    } else if (IsApplTerm(tg)) {
      FunctorEntry *fe = (FunctorEntry *)FunctorOfTerm(tg);
      Prop p0;
      PredEntry *npe;

      p0 = fe->PropsOfFE;
      if (p0 && (p->ModuleOfPred == PROLOG_MODULE ||
                 p->ModuleOfPred == SYSTEM_MODULE ||
                 p->ModuleOfPred == USER_MODULE)) {
        mark_preds_with_this_func(fe, p0);
        PRED_GOAL_EXPANSION_FUNC = TRUE;
      }
      npe = RepPredProp(PredPropByFunc(fe, p->ModuleOfPred));
      npe->PredFlags |= GoalExPredFlag;
    } else if (IsAtomTerm(tg)) {
      Atom at = AtomOfTerm(tg);
      Prop p0;
      PredEntry *npe;

      p0 = RepAtom(at)->PropsOfAE;
      if (p0 && (p->ModuleOfPred == PROLOG_MODULE ||
                 p->ModuleOfPred == SYSTEM_MODULE ||
                 p->ModuleOfPred == USER_MODULE)) {
        mark_preds_with_this_atom(p0);
        PRED_GOAL_EXPANSION_FUNC = TRUE;
      }
      npe = RepPredProp(PredPropByAtom(at, p->ModuleOfPred));
      npe->PredFlags |= GoalExPredFlag;
    }
  }
}

Int Yap_source_line_no(void) {
  CACHE_REGS
  return LOCAL_SourceFileLineno;
}

Atom Yap_source_file_name(void) {
  CACHE_REGS
  return LOCAL_SourceFileName;
}

static int addclause(Term t, yamop *cp, int mode, Term mod, Term *t4ref)
/*
 *
 mode
   0  assertz
   1  consult
   2  asserta
*/
{
  CACHE_REGS
  PredEntry *p;
  int spy_flag = FALSE;
  Atom at;
  arity_t Arity;
  pred_flags_t pflags;
  Term tf;

  if (IsApplTerm(t) && FunctorOfTerm(t) == FunctorAssert)
    tf = ArgOfTerm(1, t);
  else
    tf = t;
  if (IsAtomTerm(tf)) {
    at = AtomOfTerm(tf);
    p = RepPredProp(PredPropByAtom(at, mod));
    Arity = 0;
  } else {
    Functor f = FunctorOfTerm(tf);
    Arity = ArityOfFunctor(f);
    at = NameOfFunctor(f);
    p = RepPredProp(PredPropByFunc(f, mod));
  }
  Yap_PutValue(AtomAbol, TermNil);
  PELOCK(20, p);
  pflags = p->PredFlags;
  /* we are redefining a prolog module predicate */
  if (!(p->PredFlags & SysExportPredFlag) &&
      ((pflags &
        (UserCPredFlag | CArgsPredFlag | NumberDBPredFlag | AtomDBPredFlag |
         TestPredFlag | AsmPredFlag | CPredFlag | BinaryPredFlag)) ||
       (p->ModuleOfPred == PROLOG_MODULE && mod != TermProlog && mod))) {
    // printf("p=%p p->PredFlags=%lx %lx p->cs.p_code.NOfClauses=%ld\n", p,
    // p->PredFlags,SysExportPredFlag  , p->cs.p_code.NOfClauses);
    addcl_permission_error(RepAtom(at), Arity, FALSE);
    UNLOCKPE(30, p);
    return TermNil;
  }
  /* we are redefining a prolog module predicate */
  if (pflags & MegaClausePredFlag) {
    split_megaclause(p);
  }
  /* The only problem we have now is when we need to throw away
     Indexing blocks
  */
  if (pflags & IndexedPredFlag) {
    Yap_AddClauseToIndex(p, cp, mode == asserta);
  }
  if (pflags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag))
    spy_flag = TRUE;
  goal_expansion_support(p, tf);
  if (Yap_discontiguous(p PASS_REGS)) {
    Term disc[3], sc[4];
    if (p->ArityOfPE) {
      disc[0] = MkAtomTerm(NameOfFunctor(p->FunctorOfPred));
    } else {
      disc[1] = MkAtomTerm((Atom)(p->FunctorOfPred));
    }
    disc[1] = MkIntTerm(p->ArityOfPE);
    disc[2] = Yap_Module_Name(p);
    sc[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomDiscontiguous, 3), 3, disc);
    sc[1] = MkIntegerTerm(Yap_source_line_no());
    sc[2] = MkAtomTerm(LOCAL_SourceFileName);
    sc[3] = t;
    Yap_PrintWarning(Yap_MkApplTerm(Yap_MkFunctor(AtomStyleCheck, 4), 4, sc));
  } else if (Yap_multiple(p, mode PASS_REGS)) {
    Term disc[4], sc[4];
    if (p->ArityOfPE) {
      disc[0] = MkAtomTerm(NameOfFunctor(p->FunctorOfPred));
    } else {
      disc[1] = MkAtomTerm((Atom)(p->FunctorOfPred));
    }
    disc[1] = MkIntTerm(p->ArityOfPE);
    disc[2] = Yap_Module_Name(p);
    disc[3] = MkAtomTerm(p->src.OwnerFile);
    sc[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomMultiple, 4), 4, disc);
    sc[1] = MkIntegerTerm(Yap_source_line_no());
    sc[2] = MkAtomTerm(LOCAL_SourceFileName);
    sc[3] = t;
    Yap_PrintWarning(Yap_MkApplTerm(Yap_MkFunctor(AtomStyleCheck, 4), 4, sc));
  }
  if (mode == consult)
    not_was_reconsulted(p, t, TRUE);
  /* always check if we have a valid error first */
  if (LOCAL_ErrorMessage &&
      LOCAL_Error_TYPE == PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE) {
    UNLOCKPE(31, p);
    return TermNil;
  }
  if (pflags & UDIPredFlag) {
    Yap_new_udi_clause(p, cp, t);
  }
  if (!is_dynamic(p)) {
    if (pflags & LogUpdatePredFlag) {
      LogUpdClause *clp = ClauseCodeToLogUpdClause(cp);
      clp->ClFlags |= LogUpdMask;
      if (is_fact(t)) {
        clp->ClFlags |= FactMask;
        clp->lusl.ClLine = Yap_source_line_no();
      }
    } else {
      StaticClause *clp = ClauseCodeToStaticClause(cp);
      clp->ClFlags |= StaticMask;
      if (is_fact(t) && !(p->PredFlags & TabledPredFlag)) {
        clp->ClFlags |= FactMask;
        clp->usc.ClLine = Yap_source_line_no();
      }
    }
    if (compile_mode)
      p->PredFlags = p->PredFlags | CompiledPredFlag;
    else
      p->PredFlags = p->PredFlags | CompiledPredFlag;
  }
  if (p->cs.p_code.FirstClause == NULL) {
    if (!(pflags & DynamicPredFlag)) {
      add_first_static(p, cp, spy_flag);
      /* make sure we have a place to jump to */
      if (p->OpcodeOfPred == UNDEF_OPCODE ||
          p->OpcodeOfPred == FAIL_OPCODE) { /* log updates */
        p->CodeOfPred = p->cs.p_code.TrueCodeOfPred;
        p->OpcodeOfPred = ((yamop *)(p->CodeOfPred))->opc;
      }
#if defined(YAPOR) || defined(THREADS)
      if (p->PredFlags & LogUpdatePredFlag &&
          !(p->PredFlags & ThreadLocalPredFlag) &&
          p->ModuleOfPred != IDB_MODULE) {
        p->OpcodeOfPred = LOCKPRED_OPCODE;
        p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
      }
#endif
    } else {
      add_first_dynamic(p, cp, spy_flag);
    }
  } else if (mode == asserta) {
    if (pflags & DynamicPredFlag)
      asserta_dynam_clause(p, cp);
    else
      asserta_stat_clause(p, cp, spy_flag);
  } else if (pflags & DynamicPredFlag)
    assertz_dynam_clause(p, cp);
  else {
    assertz_stat_clause(p, cp, spy_flag);
    if (p->OpcodeOfPred != INDEX_OPCODE &&
        p->OpcodeOfPred != Yap_opcode(_spy_pred)) {
      p->CodeOfPred = p->cs.p_code.TrueCodeOfPred;
      p->OpcodeOfPred = ((yamop *)(p->CodeOfPred))->opc;
    }
#if defined(YAPOR) || defined(THREADS)
    if (p->PredFlags & LogUpdatePredFlag &&
        !(p->PredFlags & ThreadLocalPredFlag) &&
        p->ModuleOfPred != IDB_MODULE) {
      p->OpcodeOfPred = LOCKPRED_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
#endif
  }
  UNLOCKPE(32, p);
  if (pflags & LogUpdatePredFlag) {
    LogUpdClause *cl = (LogUpdClause *)ClauseCodeToLogUpdClause(cp);
    tf = MkDBRefTerm((DBRef)cl);
#if MULTIPLE_STACKS
    TRAIL_CLREF(cl); /* So that fail will erase it */
    INC_CLREF_COUNT(cl);
#else
    if (!(cl->ClFlags & InUseMask)) {
      cl->ClFlags |= InUseMask;
      TRAIL_CLREF(cl); /* So that fail will erase it */
    }
#endif
  } else {
    tf = Yap_MkStaticRefTerm(ClauseCodeToStaticClause(cp), p);
  }
  if (*t4ref != TermNil) {
    if (!Yap_unify(*t4ref, tf)) {
      return FALSE;
    }
  }
  if (pflags & MultiFileFlag) {
    /* add Info on new clause for multifile predicates to the DB */
    Term t[5], tn;
    t[0] = MkAtomTerm(Yap_ConsultingFile(PASS_REGS1));
    t[1] = MkAtomTerm(at);
    t[2] = MkIntegerTerm(Arity);
    t[3] = mod;
    t[4] = tf;
    tn = Yap_MkApplTerm(FunctorMultiFileClause, 5, t);
    Yap_Recordz(AtomMultiFile, tn);
  }
  return TRUE;
}

int Yap_addclause(Term t, yamop *cp, int mode, Term mod, Term *t4ref) {
  return addclause(t, cp, mode, mod, t4ref);
}

void Yap_EraseMegaClause(yamop *cl, PredEntry *ap) {
  /* just make it fail */
  cl->opc = Yap_opcode(_op_fail);
}

void Yap_EraseStaticClause(StaticClause *cl, PredEntry *ap, Term mod) {

  /* ok, first I need to find out the parent predicate */
  if (ap->PredFlags & MegaClausePredFlag) {
    split_megaclause(ap);
  }
  if (ap->PredFlags & IndexedPredFlag)
    RemoveIndexation(ap);
  ap->cs.p_code.NOfClauses--;
  if (ap->cs.p_code.FirstClause == cl->ClCode) {
    /* got rid of first clause */
    if (ap->cs.p_code.LastClause == cl->ClCode) {
      /* got rid of all clauses */
      ap->cs.p_code.LastClause = ap->cs.p_code.FirstClause = NULL;
      ap->OpcodeOfPred = UNDEF_OPCODE;
      ap->cs.p_code.TrueCodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
    } else {
      yamop *ncl = cl->ClNext->ClCode;
      ap->cs.p_code.FirstClause = ncl;
      ap->cs.p_code.TrueCodeOfPred = ncl;
      ap->OpcodeOfPred = ncl->opc;
    }
  } else {
    StaticClause *pcl = ClauseCodeToStaticClause(ap->cs.p_code.FirstClause),
                 *ocl = NULL;

    while (pcl != cl) {
      ocl = pcl;
      pcl = pcl->ClNext;
    }
    ocl->ClNext = cl->ClNext;
    if (cl->ClCode == ap->cs.p_code.LastClause) {
      ap->cs.p_code.LastClause = ocl->ClCode;
    }
  }
  if (ap->cs.p_code.NOfClauses == 1) {
    ap->cs.p_code.TrueCodeOfPred = ap->cs.p_code.FirstClause;
    ap->OpcodeOfPred = ap->cs.p_code.TrueCodeOfPred->opc;
  }
  if (cl->ClFlags & HasBlobsMask || Yap_static_in_use(ap, TRUE)) {
    LOCK(DeadStaticClausesLock);
    cl->ClNext = DeadStaticClauses;
    DeadStaticClauses = cl;
    UNLOCK(DeadStaticClausesLock);
  } else {
    Yap_InformOfRemoval(cl);
    Yap_ClauseSpace -= cl->ClSize;
    Yap_FreeCodeSpace((char *)cl);
  }
  if (ap->cs.p_code.NOfClauses == 0) {
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred;
  } else if (ap->cs.p_code.NOfClauses > 1) {
    ap->OpcodeOfPred = INDEX_OPCODE;
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred =
        (yamop *)(&(ap->OpcodeOfPred));
  } else if (ap->PredFlags &
             (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred =
        (yamop *)(&(ap->OpcodeOfPred));
  } else {
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred;
  }
#if defined(YAPOR) || defined(THREADS)
  if (ap->PredFlags & LogUpdatePredFlag &&
      !(ap->PredFlags & ThreadLocalPredFlag) &&
      ap->ModuleOfPred != IDB_MODULE) {
    ap->OpcodeOfPred = LOCKPRED_OPCODE;
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
  }
#endif
}

void Yap_add_logupd_clause(PredEntry *pe, LogUpdClause *cl, int mode) {
  yamop *cp = cl->ClCode;

  if (pe->PredFlags & IndexedPredFlag) {
    Yap_AddClauseToIndex(pe, cp, mode == asserta);
  }
  if (pe->cs.p_code.FirstClause == NULL) {
    add_first_static(pe, cp, FALSE);
    /* make sure we have a place to jump to */
    if (pe->OpcodeOfPred == UNDEF_OPCODE ||
        pe->OpcodeOfPred == FAIL_OPCODE) { /* log updates */
#if defined(YAPOR) || defined(THREADS)
      if (pe->PredFlags & LogUpdatePredFlag &&
          !(pe->PredFlags & ThreadLocalPredFlag) &&
          pe->ModuleOfPred != IDB_MODULE) {
        pe->OpcodeOfPred = LOCKPRED_OPCODE;
        pe->CodeOfPred = (yamop *)(&(pe->OpcodeOfPred));
      } else {
#endif
        pe->CodeOfPred = pe->cs.p_code.TrueCodeOfPred;
        pe->OpcodeOfPred = ((yamop *)(pe->CodeOfPred))->opc;
#if defined(YAPOR) || defined(THREADS)
      }
#endif
    }
  } else if (mode == asserta) {
    asserta_stat_clause(pe, cp, FALSE);
  } else {
    assertz_stat_clause(pe, cp, FALSE);
  }
}

static Int p_compile(USES_REGS1) { /* '$compile'(+C,+Flags, Mod) */
  Term t = Deref(ARG1);
  Term t1 = Deref(ARG2);
  Term mod = Deref(ARG4);
  Term tn = TermNil;
  yamop *codeadr;

  if (IsVarTerm(t1) || !IsIntTerm(t1))
    return (FALSE);
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);

  YAPEnterCriticalSection();
  codeadr =
      Yap_cclause(t, 4, mod, Deref(ARG3)); /* vsc: give the number of arguments
                        to cclause in case there is overflow */
  t = Deref(ARG1); /* just in case there was an heap overflow */
  if (!LOCAL_ErrorMessage)
    addclause(t, codeadr, (int)(IntOfTerm(t1) & 3), mod, &tn);
  YAPLeaveCriticalSection();
  if (LOCAL_ErrorMessage) {
    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);

    return FALSE;
  }
  return TRUE;
}

static Int
    p_compile_dynamic(USES_REGS1) { /* '$compile_dynamic'(+C,+Flags,Mod,-Ref) */
  Term t = Deref(ARG1);
  Term t1 = Deref(ARG2);
  Term mod = Deref(ARG4);
  yamop *code_adr;
  int old_optimize, mode;

  if (IsVarTerm(t1) || !IsAtomicTerm(t1))
    return FALSE;
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return FALSE;
  if (IsAtomTerm(t1)) {
    if (RepAtom(AtomOfTerm(t1))->StrOfAE[0] == 'f')
      mode = asserta;
    else
      mode = assertz;
  } else
    mode = IntegerOfTerm(t1);
  /* separate assert in current file from reconsult
    if (mode == assertz && LOCAL_consult_level && mod == CurrentModule)
      mode = consult;
  */
  old_optimize = optimizer_on;
  optimizer_on = FALSE;
  YAPEnterCriticalSection();
  code_adr = Yap_cclause(t, 5, mod,
                         Deref(ARG3)); /* vsc: give the number of arguments to
                    cclause() in case there is a overflow */
  t = Deref(ARG1); /* just in case there was an heap overflow */
  if (!LOCAL_ErrorMessage) {

    optimizer_on = old_optimize;
    addclause(t, code_adr, mode, mod, &ARG5);
  }
  if (LOCAL_ErrorMessage) {
    if (!LOCAL_Error_Term)
      LOCAL_Error_Term = TermNil;
    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
    YAPLeaveCriticalSection();
    return FALSE;
  }
  YAPLeaveCriticalSection();
  return TRUE;
}

Atom Yap_ConsultingFile(USES_REGS1) {
  int sno;
  if ((sno = Yap_CheckAlias(AtomLoopStream)) >= 0) {
    //    if(sno ==0)
    //  return(AtomUserIn);
    return StreamFullName(sno);
  }
  if (LOCAL_consult_level == 0) {
    return (AtomUser);
  } else {
    return (Yap_ULookupAtom(LOCAL_ConsultBase[2].filename));
  }
}

/* consult file *file*, *mode* may be one of either consult or reconsult */
static void init_consult(int mode, const unsigned char *file) {
  CACHE_REGS
  if (!LOCAL_ConsultSp) {
    InitConsultStack();
  }
  LOCAL_ConsultSp--;
  LOCAL_ConsultSp->filename = file;
  LOCAL_ConsultSp--;
  LOCAL_ConsultSp->mode = mode;
  LOCAL_ConsultSp--;
  LOCAL_ConsultSp->c = (LOCAL_ConsultBase - LOCAL_ConsultSp);
  LOCAL_ConsultBase = LOCAL_ConsultSp;
#if !defined(YAPOR) && !defined(YAPOR_SBA)
/*  if (LOCAL_consult_level == 0)
    do_toggle_static_predicates_in_use(TRUE); */
#endif
  LOCAL_consult_level++;
  LOCAL_LastAssertedPred = NULL;
}

void Yap_init_consult(int mode, const char *file) {
  init_consult(mode, (const unsigned char *)file);
}

static Int p_startconsult(USES_REGS1) { /* '$start_consult'(+Mode)	 */
  Term t;
  char *smode = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
  int mode;

  mode = strcmp("consult", (char *)smode);
  init_consult(mode, RepAtom(AtomOfTerm(Deref(ARG2)))->UStrOfAE);
  t = MkIntTerm(LOCAL_consult_level);
  return (Yap_unify_constant(ARG3, t));
}

static Int p_showconslultlev(USES_REGS1) {
  Term t;

  t = MkIntTerm(LOCAL_consult_level);
  return (Yap_unify_constant(ARG1, t));
}

static void end_consult(USES_REGS1) {
  LOCAL_ConsultSp = LOCAL_ConsultBase;
  LOCAL_ConsultBase = LOCAL_ConsultSp + LOCAL_ConsultSp->c;
  LOCAL_ConsultSp += 3;
  LOCAL_consult_level--;
  LOCAL_LastAssertedPred = NULL;
#if !defined(YAPOR) && !defined(YAPOR_SBA)
/*  if (LOCAL_consult_level == 0)
    do_toggle_static_predicates_in_use(FALSE);*/
#endif
}

void Yap_end_consult(void) {
  CACHE_REGS
  end_consult(PASS_REGS1);
}

static Int p_endconsult(USES_REGS1) { /* '$end_consult'		 */
  end_consult(PASS_REGS1);
  return (TRUE);
}

static void purge_clauses(PredEntry *pred) {
  if (pred->PredFlags & UDIPredFlag) {
    Yap_udi_abolish(pred);
  }
  if (pred->cs.p_code.NOfClauses) {
    if (pred->PredFlags & IndexedPredFlag)
      RemoveIndexation(pred);
    Yap_PutValue(AtomAbol, MkAtomTerm(AtomTrue));
    retract_all(pred, Yap_static_in_use(pred, TRUE));
  }
}

void Yap_Abolish(PredEntry *pred) {
  purge_clauses(pred);
  pred->src.OwnerFile = AtomNil;
}

static Int p_purge_clauses(USES_REGS1) { /* '$purge_clauses'(+Func) */
  PredEntry *pred;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  MegaClause *before = DeadMegaClauses;

  Yap_PutValue(AtomAbol, MkAtomTerm(AtomNil));
  if (IsVarTerm(t))
    return FALSE;
  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return FALSE;
  }
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(PredPropByFunc(fun, mod));
  } else
    return (FALSE);
  PELOCK(21, pred);
  if (pred->PredFlags & StandardPredFlag) {
    UNLOCKPE(33, pred);
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, t, "assert/1");
    return (FALSE);
  }
  purge_clauses(pred);
  UNLOCKPE(34, pred);
  /* try to use the garbage collector to recover the mega clause,
     in case the objs pointing to it are dead themselves */
  if (DeadMegaClauses != before) {
    if (!Yap_gc(2, ENV, gc_P(P, CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  return TRUE;
}

static Int p_sys_export(USES_REGS1) { /* '$set_spy'(+Fun,+M)	 */
  PredEntry *pred;
  Term t, mod;

  t = Deref(ARG1);
  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(Yap_PredPropByAtomNonThreadLocal(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(Yap_PredPropByFunctorNonThreadLocal(fun, mod));
  } else {
    return (FALSE);
  }
  PELOCK(100, pred);
  pred->PredFlags |= SysExportPredFlag;
  UNLOCKPE(100, pred);
  return TRUE;
}

/******************************************************************

                MANAGING SPY-POINTS

******************************************************************/

static Int p_is_no_trace(USES_REGS1) { /* '$undefined'(P,Mod)	 */
  PredEntry *pe;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "undefined/1");
  if (EndOfPAEntr(pe))
    return TRUE;
  PELOCK(36, pe);
  if (pe->PredFlags & NoTracePredFlag) {
    UNLOCKPE(57, pe);
    return TRUE;
  }
  UNLOCKPE(59, pe);
  return FALSE;
}

static Int p_set_no_trace(USES_REGS1) { /* '$set_no_trace'(+Fun,+M)	 */
  PredEntry *pe;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "undefined/1");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(36, pe);
  pe->PredFlags |= NoTracePredFlag;
  UNLOCKPE(57, pe);
  return TRUE;
}

int Yap_SetNoTrace(char *name, arity_t arity, Term tmod) {
  PredEntry *pe;

  if (arity == 0) {
    pe = get_pred(MkAtomTerm(Yap_LookupAtom(name)), tmod, "no_trace");
  } else {
    pe = RepPredProp(
        PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom(name), arity), tmod));
  }
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(36, pe);
  pe->PredFlags |= NoTracePredFlag;
  UNLOCKPE(57, pe);
  return TRUE;
}

static Int p_setspy(USES_REGS1) { /* '$set_spy'(+Fun,+M)	 */
  Atom at;
  PredEntry *pred;
  pred_flags_t fg;
  Term t, mod;

  at = AtomSpy;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1), 0));
  SpyCode = pred;
  t = Deref(ARG1);
  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(Yap_PredPropByAtomNonThreadLocal(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(Yap_PredPropByFunctorNonThreadLocal(fun, mod));
  } else {
    return (FALSE);
  }
  PELOCK(22, pred);
restart_spy:
  if (pred->PredFlags & (CPredFlag | SafePredFlag)) {
    UNLOCKPE(35, pred);
    return FALSE;
  }
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    UNLOCKPE(36, pred);
    return FALSE;
  }
  if (pred->OpcodeOfPred == INDEX_OPCODE) {
    int i = 0;
    for (i = 0; i < pred->ArityOfPE; i++) {
      XREGS[i + 1] = MkVarTerm();
    }
    IPred(pred, 0, CP);
    goto restart_spy;
  }
  fg = pred->PredFlags;
  if (fg & DynamicPredFlag) {
    pred->OpcodeOfPred = ((yamop *)(pred->CodeOfPred))->opc =
        Yap_opcode(_spy_or_trymark);
  } else {
    pred->OpcodeOfPred = Yap_opcode(_spy_pred);
    pred->CodeOfPred = (yamop *)(&(pred->OpcodeOfPred));
  }
  pred->PredFlags |= SpiedPredFlag;
  UNLOCKPE(37, pred);
  return TRUE;
}

static Int p_rmspy(USES_REGS1) { /* '$rm_spy'(+T,+Mod)	 */
  Atom at;
  PredEntry *pred;
  Term t;
  Term mod;

  t = Deref(ARG1);
  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t)) {
    at = AtomOfTerm(t);
    pred = RepPredProp(Yap_PredPropByAtomNonThreadLocal(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(Yap_PredPropByFunctorNonThreadLocal(fun, mod));
  } else
    return FALSE;
  PELOCK(23, pred);
  if (!(pred->PredFlags & SpiedPredFlag)) {
    UNLOCKPE(38, pred);
    return FALSE;
  }
#if THREADS
  if (pred->PredFlags & ThreadLocalPredFlag) {
    pred->OpcodeOfPred = Yap_opcode(_thread_local);
    pred->PredFlags ^= SpiedPredFlag;
    UNLOCKPE(39, pred);
    return TRUE;
  }
#endif
  if (!(pred->PredFlags & (CountPredFlag | ProfiledPredFlag))) {
    if (!(pred->PredFlags & DynamicPredFlag)) {
#if defined(YAPOR) || defined(THREADS)
      if (pred->PredFlags & LogUpdatePredFlag &&
          !(pred->PredFlags & ThreadLocalPredFlag) &&
          pred->ModuleOfPred != IDB_MODULE) {
        pred->OpcodeOfPred = LOCKPRED_OPCODE;
        pred->CodeOfPred = (yamop *)(&(pred->OpcodeOfPred));
      } else {
#endif
        pred->CodeOfPred = pred->cs.p_code.TrueCodeOfPred;
        pred->OpcodeOfPred = pred->CodeOfPred->opc;
#if defined(YAPOR) || defined(THREADS)
      }
#endif
    } else if (pred->OpcodeOfPred == Yap_opcode(_spy_or_trymark)) {
      pred->OpcodeOfPred = Yap_opcode(_try_and_mark);
    } else {
      UNLOCKPE(39, pred);
      return FALSE;
    }
  }
  pred->PredFlags ^= SpiedPredFlag;
  UNLOCKPE(40, pred);
  return (TRUE);
}

/******************************************************************

                INFO ABOUT PREDICATES

******************************************************************/

static Int
    p_number_of_clauses(USES_REGS1) { /* '$number_of_clauses'(Predicate,M,N) */
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  int ncl = 0;
  Prop pe;

  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  }
  if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pe = Yap_GetPredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    pe = Yap_GetPredPropByFunc(f, mod);
  } else {
    return (FALSE);
  }
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(24, RepPredProp(pe));
  ncl = RepPredProp(pe)->cs.p_code.NOfClauses;
  UNLOCKPE(41, RepPredProp(pe));
  return (Yap_unify_constant(ARG3, MkIntegerTerm(ncl)));
}

static Int p_new_multifile(USES_REGS1) { /* '$new_multifile'(+N,+Ar,+Mod)  */
  Atom at;
  arity_t arity;
  PredEntry *pe;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG3);

  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t))
    at = AtomOfTerm(t);
  else
    return (FALSE);
  t = Deref(ARG2);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsIntTerm(t))
    arity = IntOfTerm(t);
  else
    return FALSE;
  if (arity == 0)
    pe = RepPredProp(PredPropByAtom(at, mod));
  else
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, arity), mod));
  PELOCK(26, pe);
  pe->PredFlags |= MultiFileFlag;
  /* mutifile-predicates are weird, they do not seat really on the default
   * module */
  if (pe->ModuleOfPred == PROLOG_MODULE)
    pe->ModuleOfPred = TermProlog;
  if (!(pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag))) {
    /* static */
    pe->PredFlags |= (SourcePredFlag | CompiledPredFlag);
  }
  pe->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  UNLOCKPE(43, pe);
  return (TRUE);
}

static Int p_is_multifile(USES_REGS1) { /* '$is_multifile'(+S,+Mod)	 */
  PredEntry *pe;
  bool out;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_multifile");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);
  out = (pe->PredFlags & MultiFileFlag);
  UNLOCKPE(44, pe);
  return (out);
}

static Int
    p_new_discontiguous(USES_REGS1) { /* '$new_discontiguous'(+N,+Ar,+Mod)  */
  Atom at;
  arity_t arity;
  PredEntry *pe;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG3);

  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t))
    at = AtomOfTerm(t);
  else
    return (FALSE);
  t = Deref(ARG2);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsIntTerm(t))
    arity = IntOfTerm(t);
  else
    return FALSE;
  if (arity == 0)
    pe = RepPredProp(PredPropByAtom(at, mod));
  else
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, arity), mod));
  PELOCK(26, pe);
  pe->PredFlags |= DiscontiguousPredFlag;
  /* mutifile-predicates are weird, they do not seat really on the default
   * module */
  if (pe->ModuleOfPred == PROLOG_MODULE)
    pe->ModuleOfPred = TermProlog;
  UNLOCKPE(43, pe);
  return (TRUE);
}

static Int p_is_discontiguous(USES_REGS1) { /* '$is_multifile'(+S,+Mod)	 */
  PredEntry *pe;
  bool out;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "discontigus");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);
  out = (pe->PredFlags & DiscontiguousPredFlag);
  UNLOCKPE(44, pe);
  return (out);
}

static Int p_is_thread_local(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_log_updatable");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);
  out = (pe->PredFlags & ThreadLocalPredFlag);
  UNLOCKPE(45, pe);
  return (out);
}

static Int p_is_log_updatable(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_log_updatable");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);
  out = (pe->PredFlags & LogUpdatePredFlag);
  UNLOCKPE(45, pe);
  return (out);
}

static Int p_is_source(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_source");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(28, pe);
  out = (pe->PredFlags & SourcePredFlag);
  UNLOCKPE(46, pe);
  return (out);
}

static Int p_is_exo(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;
  MegaClause *mcl;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_exo");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(28, pe);
  out = (pe->PredFlags & MegaClausePredFlag);
  if (out) {
    mcl = ClauseCodeToMegaClause(pe->cs.p_code.FirstClause);
    out = mcl->ClFlags & ExoMask;
  }
  UNLOCKPE(46, pe);
  return (out);
}

static Int p_owner_file(USES_REGS1) { /* '$owner_file'(+P,M,F)	 */
  PredEntry *pe;
  Atom owner;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_source");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(29, pe);
  if (pe->ModuleOfPred == IDB_MODULE) {
    UNLOCKPE(47, pe);
    return FALSE;
  }
  if (pe->PredFlags & MultiFileFlag) {
    UNLOCKPE(48, pe);
    return FALSE;
  }
  owner = pe->src.OwnerFile;
  UNLOCKPE(49, pe);
  if (owner == AtomNil)
    return FALSE;
  return Yap_unify(ARG3, MkAtomTerm(owner));
}

static Int p_set_owner_file(USES_REGS1) { /* '$owner_file'(+P,M,F)	 */
  PredEntry *pe;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_source");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(29, pe);
  if (pe->ModuleOfPred == IDB_MODULE) {
    UNLOCKPE(47, pe);
    return FALSE;
  }
  if (pe->PredFlags & MultiFileFlag) {
    UNLOCKPE(48, pe);
    return FALSE;
  }
  pe->src.OwnerFile = AtomOfTerm(Deref(ARG3));
  UNLOCKPE(49, pe);
  return TRUE;
}

static Int p_mk_d(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_source");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(30, pe);
  if (pe->OpcodeOfPred == UNDEF_OPCODE) {
    pe->OpcodeOfPred = FAIL_OPCODE;
  }
  pe->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  UNLOCKPE(50, pe);
  return TRUE;
}

static Int p_is_dynamic(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_dynamic");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(31, pe);
  out = (pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag));
  UNLOCKPE(51, pe);
  return (out);
}

static Int p_is_metapredicate(USES_REGS1) { /* '$is_metapredicate'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$is_meta");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(32, pe);
  out = (pe->PredFlags & MetaPredFlag);
  UNLOCKPE(52, pe);
  return out;
}

static Int p_is_expandgoalormetapredicate(
    USES_REGS1) { /* '$is_expand_goal_predicate'(+P)	 */
  PredEntry *pe;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  bool out;

  if (PRED_GOAL_EXPANSION_ALL)
    return TRUE;
  if (IsVarTerm(t)) {
    return (FALSE);
  } else if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(Yap_GetPredPropByAtom(at, mod));
    if (EndOfPAEntr(pe)) {
      if (PRED_GOAL_EXPANSION_FUNC) {
        Prop p1 = RepAtom(at)->PropsOfAE;

        while (p1) {
          PredEntry *pe = RepPredProp(p1);

          if (pe->KindOfPE == PEProp) {
            if (pe->PredFlags & GoalExPredFlag) {
              PredPropByAtom(at, mod);
              return TRUE;
            } else {
              return FALSE;
            }
          }
          p1 = pe->NextOfPE;
        }
      }
      return FALSE;
    }
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);

    if (IsExtensionFunctor(fun)) {
      return FALSE;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(fun, mod));
    if (EndOfPAEntr(pe)) {
      if (PRED_GOAL_EXPANSION_FUNC) {
        FunctorEntry *fe = (FunctorEntry *)fun;
        if (fe->PropsOfFE &&
            (RepPredProp(fe->PropsOfFE)->PredFlags & GoalExPredFlag)) {
          PredPropByFunc(fun, mod);
          return TRUE;
        }
      }
      return FALSE;
    }
  } else {
    return FALSE;
  }

  PELOCK(33, pe);
  out = (pe->PredFlags & (GoalExPredFlag | MetaPredFlag));
  UNLOCKPE(53, pe);
  return (out);
}

static Int p_pred_exists(USES_REGS1) { /* '$pred_exists'(+P,+M)	 */
  PredEntry *pe;
  bool out;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$exists");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(34, pe);
  if (pe->PredFlags & HiddenPredFlag) {
    UNLOCKPE(54, pe);
    return FALSE;
  }
  out = (pe->OpcodeOfPred != UNDEF_OPCODE);
  UNLOCKPE(55, pe);
  return out;
}

static Int p_set_pred_module(USES_REGS1) { /* '$set_pred_module'(+P,+Mod)
                                              */
  PredEntry *pe;

  pe = get_pred(Deref(ARG1), CurrentModule, "set_pred_module/1");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(35, pe);
  pe->ModuleOfPred = Deref(ARG2);
  UNLOCKPE(56, pe);
  return (TRUE);
}

static Int p_set_pred_owner(USES_REGS1) { /* '$set_pred_module'(+P,+File)
                                             */
  PredEntry *pe;
  Term a2 = Deref(ARG2);

  pe = get_pred(Deref(ARG1), CurrentModule, "set_pred_module/1");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(35, pe);
  if (pe->PredFlags &
      (UserCPredFlag | CArgsPredFlag | NumberDBPredFlag | AtomDBPredFlag |
       TestPredFlag | AsmPredFlag | CPredFlag | BinaryPredFlag)) {
    UNLOCKPE(56, pe);
    return FALSE;
  }
  if (IsVarTerm(a2)) {
    Yap_Error(INSTANTIATION_ERROR, a2, "load_files/2");
    UNLOCKPE(56, pe);
    return FALSE;
  }
  if (!IsAtomTerm(a2)) {
    Yap_Error(TYPE_ERROR_ATOM, a2, "load_files/2");
    UNLOCKPE(56, pe);
    return FALSE;
  }
  pe->src.OwnerFile = AtomOfTerm(a2);
  UNLOCKPE(56, pe);
  return (TRUE);
}

static Int p_undefined(USES_REGS1) { /* '$undefined'(P,Mod)	 */
  PredEntry *pe;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "undefined/1");
  if (EndOfPAEntr(pe))
    return TRUE;
  PELOCK(36, pe);
  if (pe->PredFlags & (CPredFlag | UserCPredFlag | TestPredFlag | AsmPredFlag |
                       DynamicPredFlag | LogUpdatePredFlag | TabledPredFlag)) {
    UNLOCKPE(57, pe);
    return FALSE;
  }
  if (pe->OpcodeOfPred == UNDEF_OPCODE) {
    UNLOCKPE(58, pe);
    return TRUE;
  }
  UNLOCKPE(59, pe);
  return FALSE;
}

/*
 * this predicate should only be called when all clauses for the dynamic
 * predicate were remove, otherwise chaos will follow!!
 */

static Int p_kill_dynamic(USES_REGS1) { /* '$kill_dynamic'(P,M)       */
  PredEntry *pe;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "kill_dynamic/1");
  if (EndOfPAEntr(pe))
    return TRUE;
  PELOCK(37, pe);
  if (!(pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag))) {
    UNLOCKPE(60, pe);
    return FALSE;
  }
  if (pe->cs.p_code.LastClause != pe->cs.p_code.FirstClause) {
    UNLOCKPE(61, pe);
    return (FALSE);
  }
  pe->cs.p_code.LastClause = pe->cs.p_code.FirstClause = NULL;
  pe->OpcodeOfPred = UNDEF_OPCODE;
  pe->cs.p_code.TrueCodeOfPred = pe->CodeOfPred =
      (yamop *)(&(pe->OpcodeOfPred));
  pe->PredFlags = pe->PredFlags & GoalExPredFlag;
  UNLOCKPE(62, pe);
  return (TRUE);
}

static Int p_optimizer_on(USES_REGS1) { /* '$optimizer_on'		 */
  optimizer_on = TRUE;
  return (TRUE);
}

static Int p_optimizer_off(USES_REGS1) { /* '$optimizer_off'		 */
  optimizer_on = FALSE;
  return (TRUE);
}

static Int p_compile_mode(USES_REGS1) { /* $compile_mode(Old,New)	 */
  Term t2, t3 = MkIntTerm(compile_mode);
  if (!Yap_unify_constant(ARG1, t3))
    return (FALSE);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2) || !IsIntTerm(t2))
    return (FALSE);
  compile_mode = IntOfTerm(t2) & 1;
  return (TRUE);
}

static Int p_is_profiled(USES_REGS1) {
  Term t = Deref(ARG1);
  char *s;

  if (IsVarTerm(t)) {
    Term ta;

    if (PROFILING)
      ta = MkAtomTerm(AtomOn);
    else
      ta = MkAtomTerm(AtomOff);
    YapBind((CELL *)t, ta);
    return (TRUE);
  } else if (!IsAtomTerm(t))
    return (FALSE);
  s = (char *)RepAtom(AtomOfTerm(t))->StrOfAE;
  if (strcmp(s, "on") == 0) {
    PROFILING = TRUE;
    Yap_InitComma();
    return (TRUE);
  } else if (strcmp(s, "off") == 0) {
    PROFILING = FALSE;
    Yap_InitComma();
    return (TRUE);
  }
  return (FALSE);
}

static Int p_profile_info(USES_REGS1) {
  Term mod = Deref(ARG1);
  Term tfun = Deref(ARG2);
  Term out;
  PredEntry *pe;
  Term p[3];

  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(tfun)) {
    return (FALSE);
  } else if (IsApplTerm(tfun)) {
    Functor f = FunctorOfTerm(tfun);
    if (IsExtensionFunctor(f)) {
      return (FALSE);
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(f, mod));
  } else if (IsAtomTerm(tfun)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(tfun), mod));
  } else {
    return (FALSE);
  }
  if (EndOfPAEntr(pe))
    return (FALSE);
  LOCK(pe->StatisticsForPred->lock);
  if (!(pe->StatisticsForPred->NOfEntries)) {
    UNLOCK(pe->StatisticsForPred->lock);
    return (FALSE);
  }
  p[0] = Yap_MkULLIntTerm(pe->StatisticsForPred->NOfEntries);
  p[1] = Yap_MkULLIntTerm(pe->StatisticsForPred->NOfHeadSuccesses);
  p[2] = Yap_MkULLIntTerm(pe->StatisticsForPred->NOfRetries);
  UNLOCK(pe->StatisticsForPred->lock);
  out = Yap_MkApplTerm(Yap_MkFunctor(AtomProfile, 3), 3, p);
  return (Yap_unify(ARG3, out));
}

static Int p_profile_reset(USES_REGS1) {
  Term mod = Deref(ARG1);
  Term tfun = Deref(ARG2);
  PredEntry *pe;

  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(tfun)) {
    return (FALSE);
  } else if (IsApplTerm(tfun)) {
    Functor f = FunctorOfTerm(tfun);
    if (IsExtensionFunctor(f)) {
      return (FALSE);
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(f, mod));
  } else if (IsAtomTerm(tfun)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(tfun), mod));
  } else {
    return (FALSE);
  }
  if (EndOfPAEntr(pe))
    return (FALSE);
  LOCK(pe->StatisticsForPred->lock);
  pe->StatisticsForPred->NOfEntries = 0;
  pe->StatisticsForPred->NOfHeadSuccesses = 0;
  pe->StatisticsForPred->NOfRetries = 0;
  UNLOCK(pe->StatisticsForPred->lock);
  return (TRUE);
}

static Int p_is_call_counted(USES_REGS1) {
  Term t = Deref(ARG1);
  char *s;

  if (IsVarTerm(t)) {
    Term ta;

    if (CALL_COUNTING)
      ta = MkAtomTerm(AtomOn);
    else
      ta = MkAtomTerm(AtomOff);
    YapBind((CELL *)t, ta);
    return (TRUE);
  } else if (!IsAtomTerm(t))
    return (FALSE);
  s = (char *)RepAtom(AtomOfTerm(t))->StrOfAE;
  if (strcmp(s, "on") == 0) {
    CALL_COUNTING = TRUE;
    Yap_InitComma();
    return (TRUE);
  } else if (strcmp(s, "off") == 0) {
    CALL_COUNTING = FALSE;
    Yap_InitComma();
    return (TRUE);
  }
  return (FALSE);
}

static Int p_call_count_info(USES_REGS1) {
  return (Yap_unify(MkIntegerTerm(LOCAL_ReductionsCounter), ARG1) &&
          Yap_unify(MkIntegerTerm(LOCAL_PredEntriesCounter), ARG2) &&
          Yap_unify(MkIntegerTerm(LOCAL_PredEntriesCounter), ARG3));
}

static Int p_call_count_reset(USES_REGS1) {
  LOCAL_ReductionsCounter = 0;
  LOCAL_ReductionsCounterOn = FALSE;
  LOCAL_PredEntriesCounter = 0;
  LOCAL_PredEntriesCounterOn = FALSE;
  LOCAL_RetriesCounter = 0;
  LOCAL_RetriesCounterOn = FALSE;
  return (TRUE);
}

static Int p_call_count_set(USES_REGS1) {
  int do_calls = IntOfTerm(ARG2);
  int do_retries = IntOfTerm(ARG4);
  int do_entries = IntOfTerm(ARG6);

  if (do_calls)
    LOCAL_ReductionsCounter = IntegerOfTerm(Deref(ARG1));
  LOCAL_ReductionsCounterOn = do_calls;
  if (do_retries)
    LOCAL_RetriesCounter = IntegerOfTerm(Deref(ARG3));
  LOCAL_RetriesCounterOn = do_retries;
  if (do_entries)
    LOCAL_PredEntriesCounter = IntegerOfTerm(Deref(ARG5));
  LOCAL_PredEntriesCounterOn = do_entries;
  return (TRUE);
}

static Int p_clean_up_dead_clauses(USES_REGS1) {
  while (DeadStaticClauses != NULL) {
    char *pt = (char *)DeadStaticClauses;
    Yap_ClauseSpace -= DeadStaticClauses->ClSize;
    DeadStaticClauses = DeadStaticClauses->ClNext;
    Yap_InformOfRemoval(pt);
    Yap_FreeCodeSpace(pt);
  }
  while (DeadStaticIndices != NULL) {
    char *pt = (char *)DeadStaticIndices;
    if (DeadStaticIndices->ClFlags & SwitchTableMask)
      Yap_IndexSpace_SW -= DeadStaticIndices->ClSize;
    else
      Yap_IndexSpace_Tree -= DeadStaticIndices->ClSize;
    DeadStaticIndices = DeadStaticIndices->SiblingIndex;
    Yap_InformOfRemoval(pt);
    Yap_FreeCodeSpace(pt);
  }
  while (DeadMegaClauses != NULL) {
    char *pt = (char *)DeadMegaClauses;
    Yap_ClauseSpace -= DeadMegaClauses->ClSize;
    DeadMegaClauses = DeadMegaClauses->ClNext;
    Yap_InformOfRemoval(pt);
    Yap_FreeCodeSpace(pt);
  }
  return TRUE;
}

static Int /* $system_predicate(P) */
    p_system_pred(USES_REGS1) {
  PredEntry *pe;

  Term t1 = Deref(ARG1);
  Term mod = Deref(ARG2);

restart_system_pred:
  if (IsVarTerm(t1))
    return FALSE;
  if (IsAtomTerm(t1)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t1), mod));
  } else if (IsApplTerm(t1)) {
    Functor funt = FunctorOfTerm(t1);
    if (IsExtensionFunctor(funt)) {
      return FALSE;
    }
    if (funt == FunctorModule) {
      Term nmod = ArgOfTerm(1, t1);
      if (IsVarTerm(nmod)) {
        Yap_Error(INSTANTIATION_ERROR, ARG1, "system_predicate/1");
        return FALSE;
      }
      if (!IsAtomTerm(nmod)) {
        Yap_Error(TYPE_ERROR_ATOM, ARG1, "system_predicate/1");
        return FALSE;
      }
      t1 = ArgOfTerm(2, t1);
      goto restart_system_pred;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(funt, mod));
  } else if (IsPairTerm(t1)) {
    return TRUE;
  } else
    return FALSE;
  if (EndOfPAEntr(pe))
    return FALSE;
  return (!pe->ModuleOfPred || /* any predicate in prolog module */
          /* any C-pred */
          pe->PredFlags & (UserCPredFlag | CPredFlag | BinaryPredFlag |
                           AsmPredFlag | TestPredFlag) ||
          /* any weird user built-in */
          pe->OpcodeOfPred == Yap_opcode(_try_userc));
}

static Int /* $system_predicate(P) */
    p_all_system_pred(USES_REGS1) {
  PredEntry *pe;

  Term t1 = Deref(ARG1);
  Term mod = Deref(ARG2);

restart_system_pred:
  if (IsVarTerm(t1))
    return TRUE;
  if (IsAtomTerm(t1)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t1), mod));
  } else if (IsApplTerm(t1)) {
    Functor funt = FunctorOfTerm(t1);
    if (IsExtensionFunctor(funt)) {
      return FALSE;
    }
    if (funt == FunctorModule) {
      Term nmod = ArgOfTerm(1, t1);
      if (IsVarTerm(nmod)) {
        Yap_Error(INSTANTIATION_ERROR, ARG1, "system_predicate/1");
        return FALSE;
      }
      if (!IsAtomTerm(nmod)) {
        Yap_Error(TYPE_ERROR_ATOM, ARG1, "system_predicate/1");
        return FALSE;
      }
      t1 = ArgOfTerm(2, t1);
      goto restart_system_pred;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(funt, mod));
  } else if (IsPairTerm(t1)) {
    return TRUE;
  } else
    return FALSE;
  if (EndOfPAEntr(pe))
    return FALSE;
  if (pe->ModuleOfPred) {
    if (!Yap_unify(ARG3, pe->ModuleOfPred))
      return FALSE;
  } else {
    if (!Yap_unify(ARG3, TermProlog))
      return FALSE;
  }
  return (!pe->ModuleOfPred || /* any predicate in prolog module */
          /* any C-pred */
          pe->PredFlags & (UserCPredFlag | CPredFlag | BinaryPredFlag |
                           AsmPredFlag | TestPredFlag) ||
          /* any weird user built-in */
          pe->OpcodeOfPred == Yap_opcode(_try_userc));
}

void Yap_HidePred(PredEntry *pe) {
  Prop p0 = AbsPredProp(pe);
  if (pe->ArityOfPE == 0) {
    Atom a = (Atom)pe->FunctorOfPred;

    p0 = RepAtom(a)->PropsOfAE;
    if (p0 == AbsPredProp(pe)) {
      RepAtom(a)->PropsOfAE = pe->NextOfPE;
    } else {
      while (p0->NextOfPE != AbsPredProp(pe))
        p0 = p0->NextOfPE;
      if (p0 == NIL)
        return;
      p0->NextOfPE = pe->NextOfPE;
    }
  } else {
    Functor funt = pe->FunctorOfPred;

    p0 = funt->PropsOfFE;
    if (p0 == AbsPredProp(pe)) {
      funt->PropsOfFE = pe->NextOfPE;
    } else {
      while (p0->NextOfPE != AbsPredProp(pe))
        p0 = p0->NextOfPE;
      if (p0 == NIL)
        return;
      p0->NextOfPE = pe->NextOfPE;
    }
  }
  pe->NextOfPE = HIDDEN_PREDICATES;
  HIDDEN_PREDICATES = AbsPredProp(pe);
  pe->PredFlags |= HiddenPredFlag;
}

static Int /* $system_predicate(P) */
    p_stash_predicate(USES_REGS1) {
  PredEntry *pe;

  Term t1 = Deref(ARG1);
  Term mod = Deref(ARG2);

restart_system_pred:
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    Atom a = AtomOfTerm(t1);

    pe = RepPredProp(Yap_GetPredPropByAtom(a, mod));
  } else if (IsApplTerm(t1)) {
    Functor funt = FunctorOfTerm(t1);
    if (IsExtensionFunctor(funt)) {
      return (FALSE);
    }
    if (funt == FunctorModule) {
      Term nmod = ArgOfTerm(1, t1);
      if (IsVarTerm(nmod)) {
        Yap_Error(INSTANTIATION_ERROR, ARG1, "hide_predicate/1");
        return (FALSE);
      }
      if (!IsAtomTerm(nmod)) {
        Yap_Error(TYPE_ERROR_ATOM, ARG1, "hide_predicate/1");
        return (FALSE);
      }
      t1 = ArgOfTerm(2, t1);
      goto restart_system_pred;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(funt, mod));
  } else if (IsPairTerm(t1)) {
    return TRUE;
  } else
    return FALSE;
  if (EndOfPAEntr(pe))
    return FALSE;
  Yap_HidePred(pe);
  return TRUE;
}

static Int /* $system_predicate(P) */
    p_hide_predicate(USES_REGS1) {
  PredEntry *pe;

  Term t1 = Deref(ARG1);
  Term mod = Deref(ARG2);

restart_system_pred:
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    Atom a = AtomOfTerm(t1);

    pe = RepPredProp(Yap_GetPredPropByAtom(a, mod));
  } else if (IsApplTerm(t1)) {
    Functor funt = FunctorOfTerm(t1);
    if (IsExtensionFunctor(funt)) {
      return (FALSE);
    }
    if (funt == FunctorModule) {
      Term nmod = ArgOfTerm(1, t1);
      if (IsVarTerm(nmod)) {
        Yap_Error(INSTANTIATION_ERROR, ARG1, "hide_predicate/1");
        return (FALSE);
      }
      if (!IsAtomTerm(nmod)) {
        Yap_Error(TYPE_ERROR_ATOM, ARG1, "hide_predicate/1");
        return (FALSE);
      }
      t1 = ArgOfTerm(2, t1);
      goto restart_system_pred;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(funt, mod));
  } else if (IsPairTerm(t1)) {
    return TRUE;
  } else
    return FALSE;
  if (EndOfPAEntr(pe))
    return FALSE;
  pe->PredFlags |= (HiddenPredFlag | NoTracePredFlag);
  return TRUE;
}

static Int /* $hidden_predicate(P) */
    p_hidden_predicate(USES_REGS1) {
  PredEntry *pe;

  Term t1 = Deref(ARG1);
  Term mod = Deref(ARG2);

restart_system_pred:
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t1), mod));
  } else if (IsApplTerm(t1)) {
    Functor funt = FunctorOfTerm(t1);
    if (IsExtensionFunctor(funt)) {
      return (FALSE);
    }
    if (funt == FunctorModule) {
      Term nmod = ArgOfTerm(1, t1);
      if (IsVarTerm(nmod)) {
        Yap_Error(INSTANTIATION_ERROR, ARG1, "hide_predicate/1");
        return (FALSE);
      }
      if (!IsAtomTerm(nmod)) {
        Yap_Error(TYPE_ERROR_ATOM, ARG1, "hide_predicate/1");
        return (FALSE);
      }
      t1 = ArgOfTerm(2, t1);
      goto restart_system_pred;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(funt, mod));
  } else if (IsPairTerm(t1)) {
    return (TRUE);
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return (FALSE);
  return (pe->PredFlags & HiddenPredFlag);
}

static Int fetch_next_lu_clause(PredEntry *pe, yamop *i_code, Term th, Term tb,
                                Term tr, yamop *cp_ptr, int first_time) {
  CACHE_REGS
  LogUpdClause *cl;
  Term rtn;
  Term Terms[3];

  Terms[0] = th;
  Terms[1] = tb;
  Terms[2] = tr;
  cl = Yap_FollowIndexingCode(
      pe, i_code, Terms, NEXTOP(PredLogUpdClause->CodeOfPred, Otapl), cp_ptr);
  th = Terms[0];
  tb = Terms[1];
  tr = Terms[2];
  if (cl == NULL) {
    UNLOCK(pe->PELock);
    return FALSE;
  }
  rtn = MkDBRefTerm((DBRef)cl);
#if MULTIPLE_STACKS
  TRAIL_CLREF(cl); /* So that fail will erase it */
  INC_CLREF_COUNT(cl);
#else
  if (!(cl->ClFlags & InUseMask)) {
    cl->ClFlags |= InUseMask;
    TRAIL_CLREF(cl); /* So that fail will erase it */
  }
#endif
  if (cl->ClFlags & FactMask) {
    if (!Yap_unify_constant(tb, MkAtomTerm(AtomTrue)) || !Yap_unify(tr, rtn)) {
      UNLOCK(pe->PELock);
      return FALSE;
    }
    if (pe->ArityOfPE) {
      Functor f = FunctorOfTerm(th);
      arity_t arity = ArityOfFunctor(f), i;
      CELL *pt = RepAppl(th) + 1;

      for (i = 0; i < arity; i++) {
        XREGS[i + 1] = pt[i];
      }
      /* don't need no ENV */
      if (first_time && P->opc != EXECUTE_CPRED_OP_CODE) {
        CP = P;
        ENV = YENV;
        YENV = ASP;
        YENV[E_CB] = (CELL)B;
      }
      P = cl->ClCode;
#if defined(YAPOR) || defined(THREADS)
      if (pe->PredFlags & ThreadLocalPredFlag) {
        /* we don't actually need to execute code */
        UNLOCK(pe->PELock);
      } else {
        PP = pe;
      }
#endif
    } else {
      /* we don't actually need to execute code */
      UNLOCK(pe->PELock);
    }
    return TRUE;
  } else {
    Term t;

    while ((t = Yap_FetchClauseTermFromDB(cl->lusl.ClSource)) == 0L) {

      if (first_time) {
        ARG5 = th;
        ARG6 = tb;
        ARG7 = tr;
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_growglobal(NULL)) {
            UNLOCK(pe->PELock);
            Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                      LOCAL_ErrorMessage);
            return FALSE;
          }
        } else {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_gcl(LOCAL_Error_Size, 7, ENV, gc_P(P, CP))) {
            UNLOCK(pe->PELock);
            Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            return FALSE;
          }
        }
        th = ARG5;
        tb = ARG6;
        tr = ARG7;
      } else {
        ARG6 = th;
        ARG7 = tb;
        ARG8 = tr;
        if (!Yap_gcl(LOCAL_Error_Size, 8, ENV, gc_P(P, CP))) {
          UNLOCK(pe->PELock);
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
        th = ARG6;
        tb = ARG7;
        tr = ARG8;
      }
    }
    UNLOCK(pe->PELock);
    return (Yap_unify(th, ArgOfTerm(1, t)) && Yap_unify(tb, ArgOfTerm(2, t)) &&
            Yap_unify(tr, rtn));
  }
}

static Int /* $hidden_predicate(P) */
    p_log_update_clause(USES_REGS1) {
  PredEntry *pe;
  Term t1 = Deref(ARG1);
  Int ret;
  yamop *new_cp;

  if (P->opc == EXECUTE_CPRED_OP_CODE) {
    new_cp = CP;
  } else {
    new_cp = P;
  }
  pe = get_pred(t1, Deref(ARG2), "clause/3");
  if (pe == NULL || EndOfPAEntr(pe))
    return FALSE;
  PELOCK(41, pe);
  ret = fetch_next_lu_clause(pe, pe->CodeOfPred, t1, ARG3, ARG4, new_cp, TRUE);
  return ret;
}

static Int /* $hidden_predicate(P) */
    p_continue_log_update_clause(USES_REGS1) {
  PredEntry *pe = (PredEntry *)IntegerOfTerm(Deref(ARG1));
  yamop *ipc = (yamop *)IntegerOfTerm(ARG2);

  PELOCK(42, pe);
  return fetch_next_lu_clause(pe, ipc, Deref(ARG3), ARG4, ARG5, B->cp_cp,
                              FALSE);
}

static Int fetch_next_lu_clause_erase(PredEntry *pe, yamop *i_code, Term th,
                                      Term tb, Term tr, yamop *cp_ptr,
                                      int first_time) {
  CACHE_REGS
  LogUpdClause *cl;
  Term rtn;
  Term Terms[3];

  Terms[0] = th;
  Terms[1] = tb;
  Terms[2] = tr;
  cl = Yap_FollowIndexingCode(pe, i_code, Terms,
                              NEXTOP(PredLogUpdClauseErase->CodeOfPred, Otapl),
                              cp_ptr);
  th = Terms[0];
  tb = Terms[1];
  tr = Terms[2];
  /* don't do this!! I might have stored a choice-point and changed ASP
     Yap_RecoverSlots(3);
  */
  if (cl == NULL) {
    UNLOCK(pe->PELock);
    return FALSE;
  }
  rtn = MkDBRefTerm((DBRef)cl);
#if MULTIPLE_STACKS
  TRAIL_CLREF(cl); /* So that fail will erase it */
  INC_CLREF_COUNT(cl);
#else
  if (!(cl->ClFlags & InUseMask)) {
    cl->ClFlags |= InUseMask;
    TRAIL_CLREF(cl); /* So that fail will erase it */
  }
#endif
  if (cl->ClFlags & FactMask) {
    if (!Yap_unify_constant(tb, MkAtomTerm(AtomTrue)) || !Yap_unify(tr, rtn)) {
      UNLOCK(pe->PELock);
      return FALSE;
    }
    if (pe->ArityOfPE) {
      Functor f = FunctorOfTerm(th);
      arity_t arity = ArityOfFunctor(f), i;
      CELL *pt = RepAppl(th) + 1;

      for (i = 0; i < arity; i++) {
        XREGS[i + 1] = pt[i];
      }
      /* don't need no ENV */
      if (first_time && P->opc != EXECUTE_CPRED_OP_CODE) {
        CP = P;
        ENV = YENV;
        YENV = ASP;
        YENV[E_CB] = (CELL)B;
      }
      P = cl->ClCode;
#if defined(YAPOR) || defined(THREADS)
      if (pe->PredFlags & ThreadLocalPredFlag) {
        /* we don't actually need to execute code */
        UNLOCK(pe->PELock);
      } else {
        PP = pe;
      }
#endif
    } else {
      /* we don't actually need to execute code */
      UNLOCK(pe->PELock);
    }
    Yap_ErLogUpdCl(cl);
    return TRUE;
  } else {
    Term t;
    Int res;

    while ((t = Yap_FetchClauseTermFromDB(cl->lusl.ClSource)) == 0L) {
      if (first_time) {
        ARG5 = th;
        ARG6 = tb;
        ARG7 = tr;
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_locked_growglobal(NULL)) {
            UNLOCK(pe->PELock);
            Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                      LOCAL_ErrorMessage);
            return FALSE;
          }
        } else {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_locked_gcl(LOCAL_Error_Size, 7, ENV, gc_P(P, CP))) {
            UNLOCK(pe->PELock);
            Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            return FALSE;
          }
        }
        th = ARG5;
        tb = ARG6;
        tr = ARG7;
      } else {
        ARG6 = th;
        ARG7 = tb;
        ARG8 = tr;
        if (!Yap_gcl(LOCAL_Error_Size, 8, ENV, CP)) {
          UNLOCK(pe->PELock);
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
        th = ARG6;
        tb = ARG7;
        tr = ARG8;
      }
    }
    res = Yap_unify(th, ArgOfTerm(1, t)) && Yap_unify(tb, ArgOfTerm(2, t)) &&
          Yap_unify(tr, rtn);
    if (res)
      Yap_ErLogUpdCl(cl);
    UNLOCK(pe->PELock);
    return res;
  }
}

static Int /* $hidden_predicate(P) */
    p_log_update_clause_erase(USES_REGS1) {
  PredEntry *pe;
  Term t1 = Deref(ARG1);
  Int ret;
  yamop *new_cp;

  if (P->opc == EXECUTE_CPRED_OP_CODE) {
    new_cp = CP;
  } else {
    new_cp = P;
  }
  pe = get_pred(t1, Deref(ARG2), "clause/3");
  if (pe == NULL || EndOfPAEntr(pe))
    return FALSE;
  PELOCK(43, pe);
  ret = fetch_next_lu_clause_erase(pe, pe->CodeOfPred, t1, ARG3, ARG4, new_cp,
                                   TRUE);
  return ret;
}

static Int /* $hidden_predicate(P) */
    p_continue_log_update_clause_erase(USES_REGS1) {
  PredEntry *pe = (PredEntry *)IntegerOfTerm(Deref(ARG1));
  yamop *ipc = (yamop *)IntegerOfTerm(ARG2);

  PELOCK(44, pe);
  return fetch_next_lu_clause_erase(pe, ipc, Deref(ARG3), ARG4, ARG5, B->cp_cp,
                                    FALSE);
}

static void adjust_cl_timestamp(LogUpdClause *cl, UInt *arp, UInt *base) {
  UInt clstamp = cl->ClTimeEnd;
  if (cl->ClTimeEnd != TIMESTAMP_EOT) {
    while (arp[0] > clstamp)
      arp--;
    if (arp[0] == clstamp) {
      cl->ClTimeEnd = (arp - base);
    } else {
      cl->ClTimeEnd = (arp - base) + 1;
    }
  }
  clstamp = cl->ClTimeStart;
  while (arp[0] > clstamp)
    arp--;
  if (arp[0] == clstamp) {
    cl->ClTimeStart = (arp - base);
  } else {
    cl->ClTimeStart = (arp - base) + 1;
  }
  clstamp = cl->ClTimeEnd;
}

static Term replace_integer(Term orig, UInt new) {
  CELL *pt;

  if (IntInBnd((Int) new))
    return MkIntTerm(new);
  /* should create an old integer */
  if (!IsApplTerm(orig)) {
    CACHE_REGS
    Yap_Error(SYSTEM_ERROR_INTERNAL, orig,
              "%uld-->%uld  where it should increase",
              (unsigned long int)IntegerOfTerm(orig), (unsigned long int)new);
    return MkIntegerTerm(new);
  }
  /* appl->appl */
  /* replace integer in situ */
  pt = RepAppl(orig) + 1;
  *pt = new;
  return orig;
}

static UInt tree_index_ssz(StaticIndex *x) {
  UInt sz = x->ClSize;
  x = x->ChildIndex;
  while (x != NULL) {
    sz += tree_index_ssz(x);
    x = x->SiblingIndex;
  }
  return sz;
}

static UInt index_ssz(StaticIndex *x, PredEntry *pe) {
  UInt sz = 0;
  yamop *ep = ExpandClausesFirst;
  if (pe->PredFlags & MegaClausePredFlag) {
    MegaClause *mcl = ClauseCodeToMegaClause(pe->cs.p_code.FirstClause);
    if (mcl->ClFlags & ExoMask) {
      struct index_t *i = ((struct index_t **)(pe->cs.p_code.FirstClause))[0];
      sz = 0;

      while (i) {
        sz = i->size + sz;
        i = i->next;
      }
      return sz;
    }
  }
  /* expand clause blocks */
  while (ep) {
    if (ep->y_u.sssllp.p == pe)
      sz += (UInt)NEXTOP((yamop *)NULL, sssllp) +
            ep->y_u.sssllp.s1 * sizeof(yamop *);
    ep = ep->y_u.sssllp.snext;
  }
  /* main indexing tree */
  sz += tree_index_ssz(x);
  return sz;
}

#ifdef DEBUG
static Int p_predicate_lu_cps(USES_REGS1) {
  return Yap_unify(ARG1, MkIntegerTerm(Yap_LiveCps)) &&
         Yap_unify(ARG2, MkIntegerTerm(Yap_FreedCps)) &&
         Yap_unify(ARG3, MkIntegerTerm(Yap_DirtyCps)) &&
         Yap_unify(ARG4, MkIntegerTerm(Yap_NewCps));
}
#endif

static Int static_statistics(PredEntry *pe) {
  CACHE_REGS
  UInt sz = sizeof(PredEntry), cls = 0, isz = 0;
  StaticClause *cl = ClauseCodeToStaticClause(pe->cs.p_code.FirstClause);

  if (pe->cs.p_code.NOfClauses > 1 &&
      pe->cs.p_code.TrueCodeOfPred != pe->cs.p_code.FirstClause) {
    isz = index_ssz(ClauseCodeToStaticIndex(pe->cs.p_code.TrueCodeOfPred), pe);
  }
  if (pe->PredFlags & MegaClausePredFlag) {
    MegaClause *mcl = ClauseCodeToMegaClause(pe->cs.p_code.FirstClause);
    return Yap_unify(ARG3, MkIntegerTerm(mcl->ClSize / mcl->ClItemSize)) &&
           Yap_unify(ARG4, MkIntegerTerm(mcl->ClSize)) &&
           Yap_unify(ARG5, MkIntegerTerm(isz));
  }
  if (pe->cs.p_code.NOfClauses) {
    do {
      cls++;
      sz += cl->ClSize;
      if (cl->ClCode == pe->cs.p_code.LastClause)
        break;
      cl = cl->ClNext;
    } while (TRUE);
  }
  return Yap_unify(ARG3, MkIntegerTerm(cls)) &&
         Yap_unify(ARG4, MkIntegerTerm(sz)) &&
         Yap_unify(ARG5, MkIntegerTerm(isz));
}

static Int p_static_pred_statistics(USES_REGS1) {
  Int out;
  PredEntry *pe;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "predicate_statistics");
  if (pe == NIL)
    return (FALSE);
  PELOCK(50, pe);
  if (pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag | UserCPredFlag |
                       AsmPredFlag | CPredFlag | BinaryPredFlag)) {
    /* should use '$recordedp' in this case */
    UNLOCK(pe->PELock);
    return FALSE;
  }
  out = static_statistics(pe);
  UNLOCK(pe->PELock);
  return out;
}

static Int p_predicate_erased_statistics(USES_REGS1) {
  UInt sz = 0, cls = 0;
  UInt isz = 0, icls = 0;
  PredEntry *pe;
  LogUpdClause *cl = DBErasedList;
  LogUpdIndex *icl = DBErasedIList;
  Term tpred = ArgOfTerm(2, Deref(ARG1));
  Term tmod = ArgOfTerm(1, Deref(ARG1));

  if (EndOfPAEntr(pe = get_pred(tpred, tmod, "predicate_erased_statistics")))
    return FALSE;
  while (cl) {
    if (cl->ClPred == pe) {
      cls++;
      sz += cl->ClSize;
    }
    cl = cl->ClNext;
  }
  while (icl) {
    if (pe == icl->ClPred) {
      icls++;
      isz += icl->ClSize;
    }
    icl = icl->SiblingIndex;
  }
  return Yap_unify(ARG2, MkIntegerTerm(cls)) &&
         Yap_unify(ARG3, MkIntegerTerm(sz)) &&
         Yap_unify(ARG4, MkIntegerTerm(icls)) &&
         Yap_unify(ARG5, MkIntegerTerm(isz));
}

void /* $hidden_predicate(P) */
    Yap_UpdateTimestamps(PredEntry *ap) {
  CACHE_REGS
  choiceptr bptr = B;
  yamop *cl0 = NEXTOP(PredLogUpdClause0->CodeOfPred, Otapl);
  yamop *cl = NEXTOP(PredLogUpdClause->CodeOfPred, Otapl);
  yamop *cle = NEXTOP(PredLogUpdClauseErase->CodeOfPred, Otapl);
  arity_t ar = ap->ArityOfPE;
  UInt *arp, *top, *base;
  LogUpdClause *lcl;

#if THREADS
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Timestamp overflow %p", ap);
  return;
#endif
  if (!ap->cs.p_code.NOfClauses)
    return;
restart:
  *--ASP = TIMESTAMP_EOT;
  top = arp = (UInt *)ASP;
  while (bptr) {
    op_numbers opnum = Yap_op_from_opcode(bptr->cp_ap->opc);

    switch (opnum) {
    case _retry_logical:
    case _count_retry_logical:
    case _profiled_retry_logical:
    case _trust_logical:
    case _count_trust_logical:
    case _profiled_trust_logical:
      if (bptr->cp_ap->y_u.OtaLl.d->ClPred == ap) {
        UInt ts = IntegerOfTerm(bptr->cp_args[ar]);
        if (ts != arp[0]) {
          if (arp - HR < 1024) {
            goto overflow;
          }
          /* be thrifty, have this in case there is a hole */
          if (ts != arp[0] - 1) {
            UInt x = arp[0];
            *--arp = x;
          }
          *--arp = ts;
        }
      }
      bptr = bptr->cp_b;
      break;
    case _retry:
      if ((bptr->cp_ap == cl0 || bptr->cp_ap == cl || bptr->cp_ap == cle) &&
          ((PredEntry *)IntegerOfTerm(bptr->cp_args[0]) == ap)) {
        UInt ts = IntegerOfTerm(bptr->cp_args[5]);
        if (ts != arp[0]) {
          if (arp - HR < 1024) {
            goto overflow;
          }
          if (ts != arp[0] - 1) {
            UInt x = arp[0];
            *--arp = x;
          }
          *--arp = ts;
        }
      }
      bptr = bptr->cp_b;
      break;
    default:
      bptr = bptr->cp_b;
      continue;
    }
  }
  if (*arp)
    *--arp = 0L;
  base = arp;
  lcl = ClauseCodeToLogUpdClause(ap->cs.p_code.FirstClause);
  while (lcl) {
    adjust_cl_timestamp(lcl, top - 1, base);
    lcl = lcl->ClNext;
  }
  lcl = DBErasedList;
  while (lcl) {
    if (lcl->ClPred == ap)
      adjust_cl_timestamp(lcl, top - 1, base);
    lcl = lcl->ClNext;
  }
  arp = top - 1;
  bptr = B;
  while (bptr) {
    op_numbers opnum = Yap_op_from_opcode(bptr->cp_ap->opc);

    switch (opnum) {
    case _retry_logical:
    case _count_retry_logical:
    case _profiled_retry_logical:
    case _trust_logical:
    case _count_trust_logical:
    case _profiled_trust_logical:
      if (bptr->cp_ap->y_u.OtaLl.d->ClPred == ap) {
        UInt ts = IntegerOfTerm(bptr->cp_args[ar]);
        while (ts != arp[0])
          arp--;
        bptr->cp_args[ar] = replace_integer(bptr->cp_args[ar], arp - base);
      }
      bptr = bptr->cp_b;
      break;
    case _retry:
      if ((bptr->cp_ap == cl0 || bptr->cp_ap == cl || bptr->cp_ap == cle) &&
          ((PredEntry *)IntegerOfTerm(bptr->cp_args[0]) == ap)) {
        UInt ts = IntegerOfTerm(bptr->cp_args[5]);
        while (ts != arp[0])
          arp--;
        bptr->cp_args[5] = replace_integer(bptr->cp_args[5], arp - base);
      }
      bptr = bptr->cp_b;
      break;
    default:
      bptr = bptr->cp_b;
      continue;
    }
  }
  return;
overflow:
  if (!Yap_growstack(64 * 1024)) {
    Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
    return;
  }
  goto restart;
}

static Int fetch_next_static_clause(PredEntry *pe, yamop *i_code, Term th,
                                    Term tb, Term tr, yamop *cp_ptr,
                                    int first_time) {
  CACHE_REGS
  StaticClause *cl;
  Term rtn;
  Term Terms[3];

  Terms[0] = th;
  Terms[1] = tb;
  Terms[2] = tr;
  cl = (StaticClause *)Yap_FollowIndexingCode(
      pe, i_code, Terms, NEXTOP(PredStaticClause->CodeOfPred, Otapl), cp_ptr);
  th = Deref(Terms[0]);
  tb = Deref(Terms[1]);
  tr = Deref(Terms[2]);
  /* don't do this!! I might have stored a choice-point and changed ASP
     Yap_RecoverSlots(3);
  */
  if (cl == NULL) {
    UNLOCKPE(45, pe);
    return FALSE;
  }
  if (pe->PredFlags & MegaClausePredFlag) {
    yamop *code = (yamop *)cl;
    rtn = Yap_MkMegaRefTerm(pe, code);
    if (!Yap_unify(tb, MkAtomTerm(AtomTrue)) || !Yap_unify(tr, rtn)) {
      UNLOCKPE(45, pe);
      return FALSE;
    }
    if (pe->ArityOfPE) {
      Functor f = FunctorOfTerm(th);
      arity_t arity = ArityOfFunctor(f), i;
      CELL *pt = RepAppl(th) + 1;

      for (i = 0; i < arity; i++) {
        XREGS[i + 1] = pt[i];
      }
      /* don't need no ENV */
      if (first_time && P->opc != EXECUTE_CPRED_OP_CODE) {
        CP = P;
        ENV = YENV;
        YENV = ASP;
        YENV[E_CB] = (CELL)B;
      }
      P = code;
    }
    UNLOCKPE(45, pe);
    return TRUE;
  }
  rtn = Yap_MkStaticRefTerm(cl, pe);
  if (cl->ClFlags & FactMask) {
    if (!Yap_unify(tb, MkAtomTerm(AtomTrue)) || !Yap_unify(tr, rtn)) {
      UNLOCKPE(45, pe);
      return FALSE;
    }

    if (pe->ArityOfPE) {
      Functor f = FunctorOfTerm(th);
      arity_t arity = ArityOfFunctor(f), i;
      CELL *pt = RepAppl(th) + 1;

      for (i = 0; i < arity; i++) {
        XREGS[i + 1] = pt[i];
      }
      /* don't need no ENV */
      if (first_time && P->opc != EXECUTE_CPRED_OP_CODE) {
        CP = P;
        ENV = YENV;
        YENV = ASP;
        YENV[E_CB] = (CELL)B;
      }
      P = cl->ClCode;
    }
    UNLOCKPE(45, pe);
    return TRUE;
  } else {
    Term t;

    if (!(pe->PredFlags & SourcePredFlag)) {
      /* no source */
      rtn = Yap_MkStaticRefTerm(cl, pe);
      UNLOCKPE(45, pe);
      return Yap_unify(tr, rtn);
    }

    if (!(pe->PredFlags & SourcePredFlag)) {
      rtn = Yap_MkStaticRefTerm(cl, pe);
      UNLOCKPE(45, pe);
      return Yap_unify(tr, rtn);
    }
    while ((t = Yap_FetchClauseTermFromDB(cl->usc.ClSource)) == 0L) {
      if (first_time) {
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_growglobal(NULL)) {
            Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                      LOCAL_ErrorMessage);
            UNLOCKPE(45, pe);
            return FALSE;
          }
        } else {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          ARG5 = th;
          ARG6 = tb;
          ARG7 = tr;
          if (!Yap_gc(7, ENV, gc_P(P, CP))) {
            Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            UNLOCKPE(45, pe);
            return FALSE;
          }
          th = ARG5;
          tb = ARG6;
          tr = ARG7;
        }
      } else {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        ARG6 = th;
        ARG7 = tb;
        ARG8 = tr;
        if (!Yap_gcl(LOCAL_Error_Size, 8, ENV, CP)) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          UNLOCKPE(45, pe);
          return FALSE;
        }
        th = ARG6;
        tb = ARG7;
        tr = ARG8;
      }
    }
    rtn = Yap_MkStaticRefTerm(cl, pe);
    UNLOCKPE(45, pe);
    if (!IsApplTerm(t) || FunctorOfTerm(t) != FunctorAssert) {
      return (Yap_unify(th, t) && Yap_unify(tb, MkAtomTerm(AtomTrue)) &&
              Yap_unify(tr, rtn));
    } else {
      return (Yap_unify(th, ArgOfTerm(1, t)) &&
              Yap_unify(tb, ArgOfTerm(2, t)) && Yap_unify(tr, rtn));
    }
  }
}

static Int /* $hidden_predicate(P) */
    p_static_clause(USES_REGS1) {
  PredEntry *pe;
  Term t1 = Deref(ARG1);
  yamop *new_cp;

  if (P->opc == EXECUTE_CPRED_OP_CODE) {
    new_cp = CP;
  } else {
    new_cp = P;
  }
  pe = get_pred(t1, Deref(ARG2), "clause/3");
  if (pe == NULL || EndOfPAEntr(pe))
    return FALSE;
  PELOCK(46, pe);
  return fetch_next_static_clause(pe, pe->CodeOfPred, ARG1, ARG3, ARG4, new_cp,
                                  TRUE);
}

static Int /* $hidden_predicate(P) */
    p_continue_static_clause(USES_REGS1) {
  PredEntry *pe = (PredEntry *)IntegerOfTerm(Deref(ARG1));
  yamop *ipc = (yamop *)IntegerOfTerm(ARG2);

  PELOCK(48, pe);
  return fetch_next_static_clause(pe, ipc, Deref(ARG3), ARG4, ARG5, B->cp_ap,
                                  FALSE);
}

static UInt compute_dbcl_size(arity_t arity) {
  UInt sz;
  switch (arity) {
  case 2:
    sz = (UInt)NEXTOP((yamop *)NULL, cc);
    break;
  case 3:
    sz = (UInt)NEXTOP((yamop *)NULL, ccc);
    break;
  case 4:
    sz = (UInt)NEXTOP((yamop *)NULL, cccc);
    break;
  case 5:
    sz = (UInt)NEXTOP((yamop *)NULL, ccccc);
    break;
  case 6:
    sz = (UInt)NEXTOP((yamop *)NULL, cccccc);
    break;
  default:
    sz = arity * (UInt)NEXTOP((yamop *)NULL, xc);
    break;
  }
  return (UInt)NEXTOP((yamop *)sz, p);
}

#define DerefAndCheck(t, V)                                                    \
  t = Deref(V);                                                                \
  if (IsVarTerm(t) || !(IsAtomOrIntTerm(t)))                                   \
    Yap_Error(TYPE_ERROR_ATOM, t0, "load_db");

static int store_dbcl_size(yamop *pc, arity_t arity, Term t0, PredEntry *pe) {
  Term t;
  CELL *tp = RepAppl(t0) + 1;
  switch (arity) {
  case 2:
    pc->opc = Yap_opcode(_get_2atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.cc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.cc.c2 = t;
    pc = NEXTOP(pc, cc);
    break;
  case 3:
    pc->opc = Yap_opcode(_get_3atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.ccc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.ccc.c2 = t;
    DerefAndCheck(t, tp[2]);
    pc->y_u.ccc.c3 = t;
    pc = NEXTOP(pc, ccc);
    break;
  case 4:
    pc->opc = Yap_opcode(_get_4atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.cccc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.cccc.c2 = t;
    DerefAndCheck(t, tp[2]);
    pc->y_u.cccc.c3 = t;
    DerefAndCheck(t, tp[3]);
    pc->y_u.cccc.c4 = t;
    pc = NEXTOP(pc, cccc);
    break;
  case 5:
    pc->opc = Yap_opcode(_get_5atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.ccccc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.ccccc.c2 = t;
    DerefAndCheck(t, tp[2]);
    pc->y_u.ccccc.c3 = t;
    DerefAndCheck(t, tp[3]);
    pc->y_u.ccccc.c4 = t;
    DerefAndCheck(t, tp[4]);
    pc->y_u.ccccc.c5 = t;
    pc = NEXTOP(pc, ccccc);
    break;
  case 6:
    pc->opc = Yap_opcode(_get_6atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.cccccc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.cccccc.c2 = t;
    DerefAndCheck(t, tp[2]);
    pc->y_u.cccccc.c3 = t;
    DerefAndCheck(t, tp[3]);
    pc->y_u.cccccc.c4 = t;
    DerefAndCheck(t, tp[4]);
    pc->y_u.cccccc.c5 = t;
    DerefAndCheck(t, tp[5]);
    pc->y_u.cccccc.c6 = t;
    pc = NEXTOP(pc, cccccc);
    break;
  default: {
    arity_t i;
    for (i = 0; i < arity; i++) {
      pc->opc = Yap_opcode(_get_atom);
#if PRECOMPUTE_REGADDRESS
      pc->y_u.xc.x = (CELL)(XREGS + (i + 1));
#else
      pc->y_u.xc.x = i + 1;
#endif
      DerefAndCheck(t, tp[0]);
      pc->y_u.xc.c = t;
      tp++;
      pc = NEXTOP(pc, xc);
    }
  } break;
  }
  pc->opc = Yap_opcode(_procceed);
  pc->y_u.p.p = pe;
  return TRUE;
}

static Int
    p_dbload_get_space(USES_REGS1) { /* '$number_of_clauses'(Predicate,M,N) */
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  Term tn = Deref(ARG3);
  arity_t arity;
  Prop pe;
  PredEntry *ap;
  UInt sz;
  MegaClause *mcl;
  yamop *ptr;
  UInt ncls;
  UInt required;

  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  }
  if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    arity = 0;
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    arity = ArityOfFunctor(f);
    pe = PredPropByFunc(f, mod);
  } else {
    return FALSE;
  }
  if (EndOfPAEntr(pe))
    return FALSE;
  ap = RepPredProp(pe);
  if (ap->PredFlags & (DynamicPredFlag | LogUpdatePredFlag
#ifdef TABLING
                       | TabledPredFlag
#endif /* TABLING */
                       )) {
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, t,
              "dbload_get_space/4");
    return FALSE;
  }
  if (IsVarTerm(tn) || !IsIntegerTerm(tn)) {
    return FALSE;
  }
  ncls = IntegerOfTerm(tn);
  if (ncls <= 1) {
    return FALSE;
  }

  sz = compute_dbcl_size(arity);
  required = sz * ncls + sizeof(MegaClause) + (UInt)NEXTOP((yamop *)NULL, l);
#ifdef DEBUG
  total_megaclause += required;
  nof_megaclauses++;
#endif
  while (!(mcl = (MegaClause *)Yap_AllocCodeSpace(required))) {
    if (!Yap_growheap(FALSE, required, NULL)) {
      /* just fail, the system will keep on going */
      return FALSE;
    }
  }
  Yap_ClauseSpace += required;
  /* cool, it's our turn to do the conversion */
  mcl->ClFlags = MegaMask;
  mcl->ClSize = sz * ncls;
  mcl->ClPred = ap;
  mcl->ClItemSize = sz;
  mcl->ClNext = NULL;
  ap->cs.p_code.FirstClause = ap->cs.p_code.LastClause = mcl->ClCode;
  ap->PredFlags |= (MegaClausePredFlag);
  ap->cs.p_code.NOfClauses = ncls;
  if (ap->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
  } else {
    ap->OpcodeOfPred = INDEX_OPCODE;
  }
  ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred =
      (yamop *)(&(ap->OpcodeOfPred));
  ptr = (yamop *)((ADDR)mcl->ClCode + ncls * sz);
  ptr->opc = Yap_opcode(_Ystop);
  return Yap_unify(ARG4, MkIntegerTerm((Int)mcl));
}

static Int p_dbassert(USES_REGS1) { /* '$number_of_clauses'(Predicate,M,N) */
  Term thandle = Deref(ARG2);
  Term tn = Deref(ARG3);
  PredEntry *pe;
  MegaClause *mcl;
  Int n;

  if (IsVarTerm(thandle) || !IsIntegerTerm(thandle)) {
    return FALSE;
  }
  mcl = (MegaClause *)IntegerOfTerm(thandle);
  if (IsVarTerm(tn) || !IsIntegerTerm(tn)) {
    return FALSE;
  }
  n = IntegerOfTerm(tn);
  pe = mcl->ClPred;
  return store_dbcl_size((yamop *)((ADDR)mcl->ClCode + n * (mcl->ClItemSize)),
                         pe->ArityOfPE, Deref(ARG1), pe);
}

#define CL_PROP_ERASED 0
#define CL_PROP_PRED 1
#define CL_PROP_FILE 2
#define CL_PROP_FACT 3
#define CL_PROP_LINE 4
#define CL_PROP_STREAM 5

/* instance(+Ref,?Term) */
static Int instance_property(USES_REGS1) {
  Term t1 = Deref(ARG1);
  DBRef dbr;

  Int op = IntOfTerm(Deref(ARG2));

  if (IsVarTerm(t1) || !IsDBRefTerm(t1)) {
    if (IsApplTerm(t1)) {
      if (FunctorOfTerm(t1) == FunctorStaticClause) {
        StaticClause *cl = Yap_ClauseFromTerm(t1);

        if (op == CL_PROP_ERASED) {
          if (cl->ClFlags & ErasedMask) {
            if (!Yap_unify(ARG3, MkAtomTerm(AtomTrue)))
              return FALSE;
          } else {
            if (!Yap_unify(ARG3, MkAtomTerm(AtomFalse)))
              return FALSE;
          }
        }
        if (op == CL_PROP_PRED || op == CL_PROP_FILE || op == CL_PROP_STREAM) {
          PredEntry *ap = (PredEntry *)IntegerOfTerm(ArgOfTerm(2, t1));
          if (!ap) {
            return FALSE;
          }
          if (op == CL_PROP_FILE) {
            if (ap->src.OwnerFile)
              return Yap_unify(ARG3, MkAtomTerm(ap->src.OwnerFile));
            else
              return FALSE;
          } else {
            Term t[2];

            if (ap->ArityOfPE == 0) {
              t[1] = MkAtomTerm((Atom)ap->FunctorOfPred);
            } else {
              Functor nf = ap->FunctorOfPred;
              arity_t arity = ArityOfFunctor(nf);
              Atom name = NameOfFunctor(nf);

              t[0] = MkAtomTerm(name);
              t[1] = MkIntegerTerm(arity);
              t[1] = Yap_MkApplTerm(FunctorSlash, 2, t);
            }
            if (ap->ModuleOfPred == PROLOG_MODULE) {
              t[0] = MkAtomTerm(AtomProlog);
            } else {
              t[0] = ap->ModuleOfPred;
            }
            return Yap_unify(ARG3, Yap_MkApplTerm(FunctorModule, 2, t));
          }
        }
        if (op == CL_PROP_FACT) {
          if (cl->ClFlags & FactMask) {
            return Yap_unify(ARG3, MkAtomTerm(AtomTrue));
          } else {
            return Yap_unify(ARG3, MkAtomTerm(AtomFalse));
          }
        }
        if (op == CL_PROP_LINE) {
          if (cl->ClFlags & FactMask) {
            return Yap_unify(ARG3, MkIntTerm(cl->usc.ClLine));
          } else if (cl->ClFlags & SrcMask) {
            return Yap_unify(ARG3, MkIntTerm(cl->usc.ClSource->ag.line_number));
          } else
            return Yap_unify(ARG3, MkIntTerm(0));
        }
      } else if (FunctorOfTerm(t1) == FunctorMegaClause) {
        PredEntry *ap = (PredEntry *)IntegerOfTerm(ArgOfTerm(1, t1));
        MegaClause *mcl = ClauseCodeToMegaClause(ap->cs.p_code.FirstClause);

        if (op == CL_PROP_ERASED) {
          return FALSE;
        }
        if (op == CL_PROP_PRED || op == CL_PROP_FILE || op == CL_PROP_STREAM) {
          if (op == CL_PROP_FILE) {
            if (ap->src.OwnerFile)
              return Yap_unify(ARG3, MkAtomTerm(ap->src.OwnerFile));
            else
              return FALSE;
          } else {
            Functor nf = ap->FunctorOfPred;
            arity_t arity = ArityOfFunctor(nf);
            Atom name = NameOfFunctor(nf);
            Term t[2];

            t[0] = MkAtomTerm(name);
            t[1] = MkIntegerTerm(arity);
            t[1] = Yap_MkApplTerm(FunctorSlash, 2, t);
            if (ap->ModuleOfPred == PROLOG_MODULE) {
              t[0] = MkAtomTerm(AtomProlog);
            } else {
              t[0] = ap->ModuleOfPred;
            }
            return Yap_unify(ARG3, Yap_MkApplTerm(FunctorModule, 2, t));
          }
        }
        if (op == CL_PROP_FACT) {
          return Yap_unify(ARG3, MkAtomTerm(AtomTrue));
        }
        if (op == CL_PROP_LINE) {
          return Yap_unify(ARG3, MkIntTerm(mcl->ClLine));
        }
      }
    }
  } else if ((dbr = DBRefOfTerm(t1))->Flags & LogUpdMask) {
    LogUpdClause *cl = (LogUpdClause *)dbr;

    if (op == CL_PROP_ERASED) {
      if (cl->ClFlags & ErasedMask) {
        if (!Yap_unify(ARG3, MkAtomTerm(AtomTrue)))
          return FALSE;
      } else {
        if (!Yap_unify(ARG3, MkAtomTerm(AtomFalse)))
          return FALSE;
      }
    }
    if (op == CL_PROP_PRED || op == CL_PROP_FILE) {
      PredEntry *ap = cl->ClPred;
      Term t[2];

      if (op == CL_PROP_FILE) {
        if (ap->src.OwnerFile)
          return Yap_unify(ARG3, MkAtomTerm(ap->src.OwnerFile));
        else
          return FALSE;
      }
      if (ap->ArityOfPE == 0) {
        t[1] = MkAtomTerm((Atom)ap->FunctorOfPred);
      } else {
        Functor nf = ap->FunctorOfPred;
        arity_t arity = ArityOfFunctor(nf);
        Atom name = NameOfFunctor(nf);

        t[0] = MkAtomTerm(name);
        t[1] = MkIntegerTerm(arity);
        t[1] = Yap_MkApplTerm(FunctorSlash, 2, t);
      }
      if (ap->ModuleOfPred == PROLOG_MODULE) {
        t[0] = MkAtomTerm(AtomProlog);
      } else {
        t[0] = ap->ModuleOfPred;
      }
      return Yap_unify(ARG3, Yap_MkApplTerm(FunctorModule, 2, t));
    }
    if (op == CL_PROP_FACT) {
      if (cl->ClFlags & FactMask) {
        return Yap_unify(ARG3, MkAtomTerm(AtomTrue));
      } else {
        return Yap_unify(ARG3, MkAtomTerm(AtomFalse));
      }
    }
    if (op == CL_PROP_LINE) {
      if (cl->ClFlags & FactMask) {
        return Yap_unify(ARG3, MkIntTerm(cl->lusl.ClLine));
      } else if (cl->ClFlags & SrcMask) {
        return Yap_unify(ARG3, MkIntTerm(cl->lusl.ClSource->ag.line_number));
      } else
        return Yap_unify(ARG3, MkIntTerm(0));
    }
  }
  return FALSE;
}

static Int p_nth_instance(USES_REGS1) {
  PredEntry *pe;
  arity_t pred_arity;
  Functor pred_f;
  Term pred_module;
  Term t4 = Deref(ARG4);

  if (IsVarTerm(t4)) {
    // we must know I or count;
    Term TCount;
    Int Count;

    TCount = Deref(ARG3);
    if (IsVarTerm(TCount)) {
      return FALSE; // backtrack?
    }
    if (!IsIntegerTerm(TCount)) {
      Yap_Error(TYPE_ERROR_INTEGER, TCount, "nth_instance/3");
      return FALSE;
    }
    Count = IntegerOfTerm(TCount);
    if (Count <= 0) {
      if (Count)
        Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, TCount, "nth_clause/3");
      else
        Yap_Error(DOMAIN_ERROR_NOT_ZERO, TCount, "nth_clause/3");
      return FALSE;
    }
    pe = get_pred(Deref(ARG1), Deref(ARG2), "nth_clause/3");
    if (pe) {
      PELOCK(47, pe);
    }
    if (Deref(ARG2) == IDB_MODULE) {
      return Yap_db_nth_recorded(pe, Count PASS_REGS);
    } else {
      Int CurSlot, sl4;
      arity_t i;
      void *cl0;

      if (!pe)
        return FALSE;
      if (!(pe->PredFlags & (SourcePredFlag | LogUpdatePredFlag))) {
        UNLOCK(pe->PELock);
        return FALSE;
      }
      CurSlot = Yap_StartSlots();
      /* I have pe and n */
      sl4 = Yap_InitSlot(ARG4);
      /* in case we have to index or to expand code */
      for (i = 1; i <= pe->ArityOfPE; i++) {
        XREGS[i] = MkVarTerm();
      }
      if (pe->OpcodeOfPred == INDEX_OPCODE) {
        IPred(pe, 0, CP);
      }
      cl0 = Yap_NthClause(pe, Count);
      ARG4 = Yap_GetFromSlot(sl4);
      LOCAL_CurSlot = CurSlot;
      if (cl0 == NULL) {
        UNLOCK(pe->PELock);
        return FALSE;
      }
      if (pe->PredFlags & LogUpdatePredFlag) {
        LogUpdClause *cl = cl0;

#if MULTIPLE_STACKS
        TRAIL_CLREF(cl); /* So that fail will erase it */
        INC_CLREF_COUNT(cl);
#else
        if (!(cl->ClFlags & InUseMask)) {
          cl->ClFlags |= InUseMask;
          TRAIL_CLREF(cl); /* So that fail will erase it */
        }
#endif
        UNLOCK(pe->PELock);
        return Yap_unify(MkDBRefTerm((DBRef)cl), ARG4);
      } else if (pe->PredFlags & MegaClausePredFlag) {
        MegaClause *mcl = ClauseCodeToMegaClause(pe->cs.p_code.FirstClause);
        if (mcl->ClFlags & ExoMask) {
          UNLOCK(pe->PELock);
          return Yap_unify(Yap_MkExoRefTerm(pe, Count - 1), ARG4);
        }
        /* fast access to nth element, all have same size */
        UNLOCK(pe->PELock);
        return Yap_unify(Yap_MkMegaRefTerm(pe, cl0), ARG4);
      } else {
        UNLOCK(pe->PELock);
        return Yap_unify(Yap_MkStaticRefTerm(cl0, pe), ARG4);
      }
    }
  }
  /* t4 is bound, we have a reference */
  if (IsDBRefTerm(t4)) {
    DBRef ref = DBRefOfTerm(t4);
    if (ref->Flags & LogUpdMask) {
      LogUpdClause *cl = (LogUpdClause *)ref;
      LogUpdClause *ocl;
      UInt icl = 0;

      pe = cl->ClPred;
      PELOCK(66, pe);
      if (cl->ClFlags & ErasedMask) {
        UNLOCK(pe->PELock);
        return FALSE;
      }
      ocl = ClauseCodeToLogUpdClause(pe->cs.p_code.FirstClause);
      do {
        icl++;
        if (cl == ocl)
          break;
        ocl = ocl->ClNext;
      } while (ocl != NULL);
      UNLOCK(pe->PELock);
      if (ocl == NULL) {
        return FALSE;
      }
      if (!Yap_unify(ARG3, MkIntegerTerm(icl))) {
        return FALSE;
      }
    } else {
      return Yap_unify_immediate_ref(ref PASS_REGS);
    }
  } else if (IsApplTerm(t4)) {
    Functor f = FunctorOfTerm(t4);

    if (f == FunctorStaticClause) {
      StaticClause *cl = Yap_ClauseFromTerm(t4), *cl0;
      pe = (PredEntry *)IntegerOfTerm(ArgOfTerm(2, t4));
      Int i;

      if (!pe) {
        return FALSE;
      }
      if (!pe->cs.p_code.NOfClauses)
        return FALSE;
      cl0 = ClauseCodeToStaticClause(pe->cs.p_code.FirstClause);
      // linear scan
      for (i = 1; i < pe->cs.p_code.NOfClauses; i++) {
        if (cl0 == cl) {
          if (!Yap_unify(MkIntTerm(i), ARG3))
            return FALSE;
          break;
        }
      }
    } else if (f == FunctorMegaClause) {
      MegaClause *mcl;
      yamop *cl = Yap_MegaClauseFromTerm(t4);
      Int i;

      pe = Yap_MegaClausePredicateFromTerm(t4);
      mcl = ClauseCodeToMegaClause(pe->cs.p_code.FirstClause);
      i = ((char *)cl - (char *)mcl->ClCode) / mcl->ClItemSize;
      if (!Yap_unify(MkIntTerm(i), ARG3))
        return FALSE;
    } else if (f == FunctorExoClause) {
      Int i;

      pe = Yap_ExoClausePredicateFromTerm(t4);
      i = Yap_ExoClauseFromTerm(t4);
      if (!Yap_unify(MkIntTerm(i + 1), ARG3)) {
        return FALSE;
      }
    } else {
      Yap_Error(TYPE_ERROR_REFERENCE, t4, "nth_clause/3");
      return FALSE;
    }
  } else {
    Yap_Error(TYPE_ERROR_REFERENCE, t4, "nth_clause/3");
    return FALSE;
  }
  pred_module = pe->ModuleOfPred;
  if (pred_module != IDB_MODULE) {
    pred_f = pe->FunctorOfPred;
    pred_arity = pe->ArityOfPE;
  } else {
    if (pe->PredFlags & NumberDBPredFlag) {
      pred_f = (Functor)MkIntegerTerm(pe->src.IndxId);
      pred_arity = 0;
    } else {
      pred_f = pe->FunctorOfPred;
      if (pe->PredFlags & AtomDBPredFlag) {
        pred_arity = 0;
      } else {
        pred_arity = ArityOfFunctor(pred_f);
      }
    }
  }
  if (pred_arity) {
    if (!Yap_unify(ARG1, Yap_MkNewApplTerm(pred_f, pred_arity)))
      return FALSE;
  } else {
    if (!Yap_unify(ARG1, MkAtomTerm((Atom)pred_f)))
      return FALSE;
  }
  if (pred_module == PROLOG_MODULE) {
    if (!Yap_unify(ARG2, TermProlog))
      return FALSE;
  } else {
    if (!Yap_unify(ARG2, pred_module))
      return FALSE;
  }
  return TRUE;
}

static Int including(USES_REGS1) {
  bool rc = Yap_unify(ARG1, LOCAL_Including);
  if (!rc)
    return FALSE;
  LOCAL_Including = Deref(ARG2);
  return true;
}

static Int predicate_flags(
    USES_REGS1) { /* $predicate_flags(+Functor,+Mod,?OldFlags,?NewFlags) */
  PredEntry *pe;
  pred_flags_t newFl;
  Term t1 = Deref(ARG1);
  Term mod = Deref(ARG2);

  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  }
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    while ((pe = RepPredProp(PredPropByAtom(AtomOfTerm(t1), mod))) == NULL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, ARG1, "while generating new predicate");
        return FALSE;
      }
      t1 = Deref(ARG1);
      mod = Deref(ARG2);
    }
  } else if (IsApplTerm(t1)) {
    Functor funt = FunctorOfTerm(t1);
    while ((pe = RepPredProp(PredPropByFunc(funt, mod))) == NULL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, ARG1, "while generating new predicate");
        return FALSE;
      }
      t1 = Deref(ARG1);
      mod = Deref(ARG2);
    }
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return (FALSE);
  PELOCK(92, pe);
  if (!Yap_unify_constant(ARG3, MkIntegerTerm(pe->PredFlags))) {
    UNLOCK(pe->PELock);
    return (FALSE);
  }
  ARG4 = Deref(ARG4);
  if (IsVarTerm(ARG4)) {
    UNLOCK(pe->PELock);
    return (TRUE);
  } else if (!IsIntegerTerm(ARG4)) {
    Term te = Yap_Eval(ARG4);

    if (IsIntegerTerm(te)) {
      newFl = IntegerOfTerm(te);
    } else {
      UNLOCK(pe->PELock);
      Yap_Error(TYPE_ERROR_INTEGER, ARG4, "flags");
      return (FALSE);
    }
  } else
    newFl = IntegerOfTerm(ARG4);
  pe->PredFlags = newFl;
  UNLOCK(pe->PELock);
  return TRUE;
}

void Yap_InitCdMgr(void) {
  CACHE_REGS
  Term cm = CurrentModule;

  Yap_InitCPred("$compile_mode", 2, p_compile_mode,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$start_consult", 3, p_startconsult,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$show_consult_level", 1, p_showconslultlev, SafePredFlag);
  Yap_InitCPred("$end_consult", 0, p_endconsult, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$set_spy", 2, p_setspy, SyncPredFlag);
  Yap_InitCPred("$rm_spy", 2, p_rmspy, SafePredFlag | SyncPredFlag);
  /* gc() may happen during compilation, hence these predicates are
        now unsafe */
  Yap_InitCPred("$predicate_flags", 4, predicate_flags, SyncPredFlag);
  Yap_InitCPred("$compile", 4, p_compile, SyncPredFlag);
  Yap_InitCPred("$compile_dynamic", 5, p_compile_dynamic, SyncPredFlag);
  Yap_InitCPred("$purge_clauses", 2, p_purge_clauses,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$is_dynamic", 2, p_is_dynamic, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$is_metapredicate", 2, p_is_metapredicate,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("$is_expand_goal_or_meta_predicate", 2,
                p_is_expandgoalormetapredicate, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$is_log_updatable", 2, p_is_log_updatable,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("$is_thread_local", 2, p_is_thread_local,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("$is_source", 2, p_is_source, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$is_exo", 2, p_is_exo, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$owner_file", 3, p_owner_file, SafePredFlag);
  Yap_InitCPred("$set_owner_file", 3, p_set_owner_file, SafePredFlag);
  Yap_InitCPred("$mk_d", 2, p_mk_d, SafePredFlag);
  Yap_InitCPred("$sys_export", 2, p_sys_export, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$pred_exists", 2, p_pred_exists, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$number_of_clauses", 3, p_number_of_clauses,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$undefined", 2, p_undefined, SafePredFlag | TestPredFlag);
  Yap_InitCPred("$optimizer_on", 0, p_optimizer_on,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$clean_up_dead_clauses", 0, p_clean_up_dead_clauses,
                SyncPredFlag);
  Yap_InitCPred("$optimizer_off", 0, p_optimizer_off,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$kill_dynamic", 2, p_kill_dynamic,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$new_multifile", 3, p_new_multifile,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$is_multifile", 2, p_is_multifile,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("$new_discontiguous", 3, p_new_discontiguous,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$is_discontiguous", 2, p_is_discontiguous,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("$is_no_trace", 2, p_is_no_trace, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$set_no_trace", 2, p_set_no_trace,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("$is_profiled", 1, p_is_profiled, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$profile_info", 3, p_profile_info,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$profile_reset", 2, p_profile_reset,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$is_call_counted", 1, p_is_call_counted,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$call_count_info", 3, p_call_count_info,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$call_count_set", 6, p_call_count_set,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$call_count_reset", 0, p_call_count_reset,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$set_pred_module", 2, p_set_pred_module, SafePredFlag);
  Yap_InitCPred("$set_pred_owner", 2, p_set_pred_owner, SafePredFlag);
  Yap_InitCPred("$system_predicate", 2, p_system_pred, SafePredFlag);
  Yap_InitCPred("$all_system_predicate", 3, p_all_system_pred, SafePredFlag);
  Yap_InitCPred("$hide_predicate", 2, p_hide_predicate, SafePredFlag);
  Yap_InitCPred("$stash_predicate", 2, p_stash_predicate, SafePredFlag);
  Yap_InitCPred("$hidden_predicate", 2, p_hidden_predicate, SafePredFlag);
  Yap_InitCPred("$log_update_clause", 4, p_log_update_clause, SyncPredFlag);
  Yap_InitCPred("$continue_log_update_clause", 5, p_continue_log_update_clause,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$log_update_clause_erase", 4, p_log_update_clause_erase,
                SyncPredFlag);
  Yap_InitCPred("$continue_log_update_clause_erase", 5,
                p_continue_log_update_clause_erase,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$static_clause", 4, p_static_clause, SyncPredFlag);
  Yap_InitCPred("$continue_static_clause", 5, p_continue_static_clause,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$static_pred_statistics", 5, p_static_pred_statistics,
                SyncPredFlag);
  Yap_InitCPred("instance_property", 3, instance_property,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$fetch_nth_clause", 4, p_nth_instance, SyncPredFlag);
  CurrentModule = DBLOAD_MODULE;
  Yap_InitCPred("dbload_get_space", 4, p_dbload_get_space, 0L);
  Yap_InitCPred("dbassert", 3, p_dbassert, 0L);
  CurrentModule = cm;
  Yap_InitCPred("$predicate_erased_statistics", 5,
                p_predicate_erased_statistics, SyncPredFlag);
  Yap_InitCPred("$including", 2, including, SyncPredFlag | HiddenPredFlag);

#ifdef DEBUG
  Yap_InitCPred("$predicate_lu_cps", 4, p_predicate_lu_cps, 0L);
#endif
}
