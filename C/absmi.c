/*************************************************************************
*									 *
*	 Yap Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*	\z								 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		absmi.c							 *
* comments:	Portable abstract machine interpreter                    *
* Last rev:     $Date: 2008-08-13 01:16:26 $,$Author: vsc $						 *
* $Log: not supported by cvs2svn $
* Revision 1.246  2008/08/12 01:27:22  vsc
* MaxOS fixes
* Avoid a thread deadlock
* improvements to SWI predicates.
* make variables_in_term system builtin.
*
* Revision 1.245  2008/08/07 20:51:15  vsc
* more threadin  fixes
*
* Revision 1.244  2008/08/06 23:05:49  vsc
* fix debugging info
*
* Revision 1.243  2008/08/06 17:32:18  vsc
* more thread fixes
*
* Revision 1.242  2008/06/17 13:37:48  vsc
* fix c_interface not to crash when people try to recover slots that are
* not there.
* fix try_logical and friends to handle case where predicate has arity 0.
*
* Revision 1.241  2008/06/04 14:47:18  vsc
* make sure we do trim_trail whenever we mess with B!
*
* Revision 1.240  2008/04/04 16:11:40  vsc
* yapor had gotten broken with recent thread changes
*
* Revision 1.239  2008/04/03 13:26:37  vsc
* protect signal handling with locks for threaded version.
* fix close/1 entry in manual (obs from Nicos).
* fix -f option in chr Makefile.
*
* Revision 1.238  2008/04/03 10:50:23  vsc
* term_variables could store local variable in global.
*
* Revision 1.237  2008/03/26 14:37:07  vsc
* more icc fixes
*
* Revision 1.236  2008/03/25 16:45:52  vsc
* make or-parallelism compile again
*
* Revision 1.235  2008/02/12 17:03:50  vsc
* SWI-portability changes
*
* Revision 1.234  2008/01/27 11:01:06  vsc
* make thread code more stable
*
* Revision 1.233  2008/01/23 17:57:44  vsc
* valgrind it!
* enable atom garbage collection.
*
* Revision 1.232  2007/11/28 23:52:14  vsc
* junction tree algorithm
*
* Revision 1.231  2007/11/26 23:43:07  vsc
* fixes to support threads and assert correctly, even if inefficiently.
*
* Revision 1.230  2007/11/08 15:52:15  vsc
* fix some bugs in new dbterm code.
*
* Revision 1.229  2007/11/07 09:25:27  vsc
* speedup meta-calls
*
* Revision 1.228  2007/11/06 17:02:08  vsc
* compile ground terms away.
*
* Revision 1.227  2007/10/28 11:23:39  vsc
* fix overflow
*
* Revision 1.226  2007/10/28 00:54:09  vsc
* new version of viterbi implementation
* fix all:atvars reporting bad info
* fix bad S info in x86_64
*
* Revision 1.225  2007/10/17 09:18:26  vsc
* growtrail assumed SREG meant ASP?
*
* Revision 1.224  2007/09/24 09:02:31  vsc
* minor bug fixes
*
* Revision 1.223  2007/06/04 12:28:01  vsc
* interface speedups
* bad error message in X is foo>>2.
*
* Revision 1.222  2007/05/01 21:18:19  vsc
* fix bug in saving P at p_eq (obs from Frabrizio Riguzzi)
*
* Revision 1.221  2007/04/10 22:13:20  vsc
* fix max modules limitation
*
* Revision 1.220  2007/03/21 18:32:49  vsc
* fix memory expansion bugs.
*
* Revision 1.219  2007/01/24 09:57:25  vsc
* fix glist_void_varx
*
* Revision 1.218  2006/12/31 01:50:34  vsc
* fix some bugs in call_cleanup: the result of action should not matter,
* and !,fail would not wakeup the delayed goal.
*
* Revision 1.217  2006/12/30 03:25:44  vsc
* call_cleanup/2 and 3
*
* Revision 1.216  2006/12/29 01:57:50  vsc
* allow coroutining plus tabling, this means fixing some trouble with the
* gc and a bug in global variable handling.
*
* Revision 1.215  2006/12/27 01:32:37  vsc
* diverse fixes
*
* Revision 1.214  2006/11/28 00:46:28  vsc
* fix bug in threaded implementation
*
* Revision 1.213  2006/11/27 17:42:02  vsc
* support for UNICODE, and other bug fixes.
*
* Revision 1.212  2006/11/21 16:21:30  vsc
* fix I/O mess
* fix spy/reconsult mess
*
* Revision 1.211  2006/11/15 00:13:36  vsc
* fixes for indexing code.
*
* Revision 1.210  2006/10/25 02:31:07  vsc
* fix emulation of trust_logical
*
* Revision 1.209  2006/10/18 13:47:31  vsc
* index.c implementation of trust_logical was decrementing the wrong
* cp_tr
*
* Revision 1.208  2006/10/11 14:53:57  vsc
* fix memory leak
* fix overflow handling
* VS: ----------------------------------------------------------------------
*
* Revision 1.207  2006/10/10 20:21:42  vsc
* fix new indexing code to actually recover space
* fix predicate info to work for LUs
*
* Revision 1.206  2006/10/10 14:08:15  vsc
* small fixes on threaded implementation.
*
* Revision 1.205  2006/09/28 16:15:54  vsc
* make GMPless version compile.
*
* Revision 1.204  2006/09/20 20:03:51  vsc
* improve indexing on floats
* fix sending large lists to DB
*
* Revision 1.203  2006/08/07 18:51:44  vsc
* fix garbage collector not to try to garbage collect when we ask for large
* chunks of stack in a single go.
*
* Revision 1.202  2006/05/24 02:35:39  vsc
* make chr work and other minor fixes.
*
* Revision 1.201  2006/04/27 14:11:57  rslopes
* *** empty log message ***
*
* Revision 1.200  2006/04/12 17:14:58  rslopes
* fix needed by the EAM engine
*
* Revision 1.199  2006/04/12 15:51:23  rslopes
* small fixes
*
* Revision 1.198  2006/03/30 01:11:09  vsc
* fix nasty variable shunting bug in garbage collector :-(:wq
*
* Revision 1.197  2006/03/24 17:13:41  rslopes
* New update to BEAM engine.
* BEAM now uses YAP Indexing (JITI)
*
* Revision 1.196  2006/03/03 23:10:47  vsc
* fix MacOSX interrupt handling
* fix using Yap files as Yap scripts.
*
* Revision 1.195  2006/02/01 13:28:56  vsc
* bignum support fixes
*
* Revision 1.194  2006/01/26 19:13:24  vsc
* avoid compilation issues with lack of gmp (Remko Troncon)
*
* Revision 1.193  2006/01/18 15:34:53  vsc
* avoid sideffects from MkBigInt
*
* Revision 1.192  2006/01/17 14:10:40  vsc
* YENV may be an HW register (breaks some tabling code)
* All YAAM instructions are now brackedted, so Op introduced an { and EndOp introduces an }. This is because Ricardo assumes that.
* Fix attvars when COROUTING is undefined.
*
* Revision 1.191  2006/01/02 02:16:17  vsc
* support new interface between YAP and GMP, so that we don't rely on our own
* allocation routines.
* Several big fixes.
*
* Revision 1.190  2005/12/23 00:20:13  vsc
* updates to gprof
* support for __POWER__
* Try to saveregs before longjmp.
*
* Revision 1.189  2005/12/17 03:25:38  vsc
* major changes to support online event-based profiling
* improve error discovery and restart on scanner.
*
* Revision 1.188  2005/12/05 17:16:10  vsc
* write_depth/3
* overflow handlings and garbage collection
* Several ipdates to CLPBN
* dif/2 could be broken in the presence of attributed variables.
*
* Revision 1.187  2005/11/26 02:57:25  vsc
* improvements to debugger
* overflow fixes
* reading attvars from DB was broken.
*
* Revision 1.186  2005/11/23 03:01:32  vsc
* fix several bugs in save/restore.b
*
* Revision 1.185  2005/11/18 18:48:51  tiagosoares
* support for executing c code when a cut occurs
*
* Revision 1.184  2005/11/15 00:50:49  vsc
* fixes for stack expansion and garbage collection under tabling.
*
* Revision 1.183  2005/11/07 15:35:47  vsc
* fix bugs in garbage collection of tabling.
*
* Revision 1.182  2005/11/05 03:02:33  vsc
* get rid of unnecessary ^ in setof
* Found bug in comparisons
*
* Revision 1.181  2005/11/04 15:39:14  vsc
* absmi should PREG, never P!!
*
* Revision 1.180  2005/10/28 17:38:49  vsc
* sveral updates
*
* Revision 1.179  2005/10/18 17:04:43  vsc
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
* Revision 1.178  2005/10/15 17:05:23  rslopes
* enable profiling on amd64
*
* Revision 1.177  2005/09/09 17:24:37  vsc
* a new and hopefully much better implementation of atts.
*
* Revision 1.176  2005/09/08 22:06:44  rslopes
* BEAM for YAP update...
*
* Revision 1.175  2005/08/12 17:00:00  ricroc
* TABLING FIX: support for incomplete tables
*
* Revision 1.174  2005/08/05 14:55:02  vsc
* first steps to allow mavars with tabling
* fix trailing for tabling with multiple get_cons
*
* Revision 1.173  2005/08/04 15:45:49  ricroc
* TABLING NEW: support to limit the table space size
*
* Revision 1.172  2005/08/02 03:09:48  vsc
* fix debugger to do well nonsource predicates.
*
* Revision 1.171  2005/08/01 15:40:36  ricroc
* TABLING NEW: better support for incomplete tabling
*
* Revision 1.170  2005/07/06 19:33:51  ricroc
* TABLING: answers for completed calls can now be obtained by loading (new option) or executing (default) them from the trie data structure.
*
* Revision 1.169  2005/07/06 15:10:01  vsc
* improvements to compiler: merged instructions and fixes for ->
*
* Revision 1.168  2005/06/04 07:27:33  ricroc
* long int support for tabling
*
* Revision 1.167  2005/06/03 08:26:31  ricroc
* float support for tabling
*
* Revision 1.166  2005/06/01 20:25:22  vsc
* == and \= should not need a choice-point in ->
*
* Revision 1.165  2005/06/01 14:02:45  vsc
* get_rid of try_me?, retry_me? and trust_me? instructions: they are not
* significantly used nowadays.
*
* Revision 1.164  2005/05/26 18:07:32  vsc
* fix warning
*
* Revision 1.163  2005/04/10 04:01:07  vsc
* bug fixes, I hope!
*
* Revision 1.162  2005/04/07 17:48:53  ricroc
* Adding tabling support for mixed strategy evaluation (batched and local scheduling)
*   UPDATE: compilation flags -DTABLING_BATCHED_SCHEDULING and -DTABLING_LOCAL_SCHEDULING removed. To support tabling use -DTABLING in the Makefile or --enable-tabling in configure.
*   NEW: yap_flag(tabling_mode,MODE) changes the tabling execution mode of all tabled predicates to MODE (batched, local or default).
*   NEW: tabling_mode(PRED,MODE) changes the default tabling execution mode of predicate PRED to MODE (batched or local).
*
* Revision 1.161  2005/03/13 06:26:09  vsc
* fix excessive pruning in meta-calls
* fix Term->int breakage in compiler
* improve JPL (at least it does something now for amd64).
*
* Revision 1.160  2005/03/07 17:49:14  vsc
* small fixes
*
* Revision 1.159  2005/03/04 20:29:55  ricroc
* bug fixes for YapTab support
*
* Revision 1.158  2005/03/01 22:25:07  vsc
* fix pruning bug
* make DL_MALLOC less enthusiastic about walking through buckets.
*
* Revision 1.157  2005/02/08 18:04:17  vsc
* library_directory may not be deterministic (usually it isn't).
*
* Revision 1.156  2005/01/13 05:47:25  vsc
* lgamma broke arithmetic optimisation
* integer_y has type y
* pass original source to checker (and maybe even use option in parser)
* use warning mechanism for checker messages.
*
* Revision 1.155  2004/12/28 22:20:34  vsc
* some extra bug fixes for trail overflows: some cannot be recovered that easily,
* some can.
*
* Revision 1.154  2004/12/05 05:01:21  vsc
* try to reduce overheads when running with goal expansion enabled.
* CLPBN fixes
* Handle overflows when allocating big clauses properly.
*
* Revision 1.153  2004/11/19 22:08:35  vsc
* replace SYSTEM_ERROR_INTERNAL by out OUT_OF_WHATEVER_ERROR whenever appropriate.
*
* Revision 1.152  2004/11/19 17:14:12  vsc
* a few fixes for 64 bit compiling.
*
* Revision 1.151  2004/11/04 18:22:28  vsc
* don't ever use memory that has been freed (that was done by LU).
* generic fixes for WIN32 libraries
*
* Revision 1.150  2004/10/26 20:15:36  vsc
* More bug fixes for overflow handling
*
* Revision 1.149  2004/10/14 22:14:52  vsc
* don't use a cached version of ARG1 in choice-points
*
* Revision 1.148  2004/09/30 21:37:40  vsc
* fixes for thread support
*
* Revision 1.147  2004/09/30 19:51:53  vsc
* fix overflow from within clause/2
*
* Revision 1.146  2004/09/27 20:45:02  vsc
* Mega clauses
* Fixes to sizeof(expand_clauses) which was being overestimated
* Fixes to profiling+indexing
* Fixes to reallocation of memory after restoring
* Make sure all clauses, even for C, end in _Ystop
* Don't reuse space for Streams
* Fix Stream_F on StreaNo+1
*
* Revision 1.145  2004/09/17 20:47:35  vsc
* fix some overflows recorded.
*
* Revision 1.144  2004/09/17 19:34:49  vsc
* simplify frozen/2
*
* Revision 1.143  2004/08/16 21:02:04  vsc
* more fixes for !
*
* Revision 1.142  2004/08/11 16:14:51  vsc
* whole lot of fixes:
*   - memory leak in indexing
*   - memory management in WIN32 now supports holes
*   - extend Yap interface, more support for SWI-Interface
*   - new predicate mktime in system
*   - buffer console I/O in WIN32
*
* Revision 1.141  2004/07/23 21:08:44  vsc
* windows fixes
*
* Revision 1.140  2004/07/22 21:32:20  vsc
* debugger fixes
* initial support for JPL
* bad calls to garbage collector and gc
* debugger fixes
*
* Revision 1.139  2004/07/03 03:29:24  vsc
* make it compile again on non-linux machines
*
* Revision 1.138  2004/06/29 19:04:40  vsc
* fix multithreaded version
* include new version of Ricardo's profiler
* new predicat atomic_concat
* allow multithreaded-debugging
* small fixes
*
* Revision 1.137  2004/06/23 17:24:19  vsc
* New comment-based message style
* Fix thread support (at least don't deadlock with oneself)
* small fixes for coroutining predicates
* force Yap to recover space in arrays of dbrefs
* use private predicates in debugger.
*
* Revision 1.136  2004/06/17 22:07:22  vsc
* bad bug in indexing code.
*
* Revision 1.135  2004/06/09 03:32:02  vsc
* fix bugs
*
* Revision 1.134  2004/06/05 03:36:59  vsc
* coroutining is now a part of attvars.
* some more fixes.
*
* Revision 1.133  2004/05/13 20:54:57  vsc
* debugger fixes
* make sure we always go back to current module, even during initizlization.
*
* Revision 1.132  2004/04/29 03:45:49  vsc
* fix garbage collection in execute_tail
*
* Revision 1.131  2004/04/22 20:07:02  vsc
* more fixes for USE_SYSTEM_MEMORY
*
* Revision 1.130  2004/04/22 03:24:17  vsc
* trust_logical should protect the last clause, otherwise it cannot
* jump there.
*
* Revision 1.129  2004/04/16 19:27:30  vsc
* more bug fixes
*
* Revision 1.128  2004/04/14 19:10:22  vsc
* expand_clauses: keep a list of clauses to expand
* fix new trail scheme for multi-assignment variables
*
* Revision 1.127  2004/03/31 01:03:09  vsc
* support expand group of clauses
*
* Revision 1.126  2004/03/19 11:35:42  vsc
* trim_trail for default machine
* be more aggressive about try-retry-trust chains.
*    - handle cases where block starts with a wait
*    - don't use _killed instructions, just let the thing rot by itself.
*
* Revision 1.125  2004/03/10 14:59:54  vsc
* optimise -> for type tests
*
* Revision 1.124  2004/03/08 19:31:01  vsc
* move to 4.5.3
*									 *
*									 *
*************************************************************************/


/**

@file absmi.c

@defgroup Efficiency Efficiency Considerations
@ingroup YAPProgramming

We next discuss several issues on trying to make Prolog programs run
fast in YAP. We assume two different programming styles:

+ Execution of <em>deterministic</em> programs ofte
n
boils down to a recursive loop of the form:

~~~~~
loop(Env) :-
        do_something(Env,NewEnv),
        loop(NewEnv).
~~~~~
 */





#define IN_ABSMI_C 1
#define _INATIVE 1
#define HAS_CACHE_REGS 1


#include "absmi.h"
#include "heapgc.h"

#include "cut_c.h"

#if YAP_JIT
#include "IsGround.h"

TraceContext **curtrace;
yamop *curpreg;
BlocksContext **globalcurblock;
COUNT ineedredefinedest;
yamop* headoftrace;

NativeContext *NativeArea;
IntermediatecodeContext *IntermediatecodeArea;

CELL l;

CELL nnexec;

Environment *Yap_ExpEnvP, Yap_ExpEnv;

void **Yap_ABSMI_ControlLabels;

static Int traced_absmi(void)
{
  return Yap_traced_absmi();
}

#endif

void **Yap_ABSMI_OPCODES;

#ifdef PUSH_X
#else

/* keep X as a global variable */

Term Yap_XREGS[MaxTemps];	/* 29                                     */

#endif

#include "arith2.h"


// #include "print_preg.h"
//#include "sprint_op.hpp"
//#include "print_op.hpp"

#ifdef COROUTINING
/*
  Imagine we are interrupting the execution, say, because we have a spy
   point or because we have goals to wake up. This routine saves the current
   live temporary registers into a structure pointed to by register ARG1.
   The registers are then recovered by a nasty builtin
   called
*/
static Term
push_live_regs(yamop *pco)
{
  CACHE_REGS
  CELL *lab = (CELL *)(pco->y_u.l.l);
  CELL max = lab[0];
  CELL curr = lab[1];
  Term tp = MkIntegerTerm((Int)pco);
  Term tcp = MkIntegerTerm((Int)CP);
  Term tenv = MkIntegerTerm((Int)(LCL0-ENV));
  Term tyenv = MkIntegerTerm((Int)(LCL0-YENV));
  CELL *start = HR;
  Int tot = 0;

  HR++;
  *HR++ = tp;
  *HR++ = tcp;
  *HR++ = tenv;
  *HR++ = tyenv;
  tot += 4;
  {
	  CELL i;

    lab += 2;
    for (i=0; i <= max; i++) {
      if (i == 8*CellSize) {
	curr = lab[0];
	lab++;
      }
      if (curr & 1) {
	CELL d1;

	tot+=2;
	HR[0] = MkIntTerm(i);
	d1 = XREGS[i];
	deref_head(d1, wake_up_unk);
      wake_up_nonvar:
	/* just copy it to the heap */
	HR[1] = d1;
	HR += 2;
	continue;

	{
	  CELL *pt0;
	  deref_body(d1, pt0, wake_up_unk, wake_up_nonvar);
	  /* bind it, in case it is a local variable */
	  if (pt0 <= HR) {
	    /* variable is safe */
	    HR[1] = (CELL)pt0;
	  } else {
	    d1 = Unsigned(HR+1);
	    RESET_VARIABLE(HR+1);
	    Bind_Local(pt0, d1);
	  }
	}
	HR += 2;
      }
      curr >>= 1;
    }
    start[0] = (CELL)Yap_MkFunctor(AtomTrue, tot);
    return(AbsAppl(start));
  }
}
#endif

#if defined(ANALYST) || defined(DEBUG)

char *Yap_op_names[] =
{
#define OPCODE(OP,TYPE) #OP
#include "YapOpcodes.h"
#undef  OPCODE
};

#endif

static int
check_alarm_fail_int(int CONT USES_REGS)
{
#if  defined(_MSC_VER) || defined(__MINGW32__)
  /* I need this for Windows and any system where SIGINT
     is not proceesed by same thread as absmi */
  if (LOCAL_PrologMode & (AbortMode|InterruptMode))
    {
      CalculateStackGap( PASS_REGS1 );
      return CONT;
    }
#endif
  if (Yap_get_signal( YAP_FAIL_SIGNAL )) {
      return false;
  }
  if (!Yap_has_a_signal()) {
      /* no need to look into GC */
      CalculateStackGap( PASS_REGS1 );
  }
  // fail even if there are more signals, they will have to be dealt later.
  return -1;
}

static int
stack_overflow( PredEntry *pe, CELL *env, yamop *cp, arity_t nargs  USES_REGS)
{
  if ((Int)(Unsigned(YREG) - Unsigned(HR)) < StackGap( PASS_REGS1 ) ||
      Yap_get_signal( YAP_STOVF_SIGNAL )) {
    S = (CELL *)pe;
    if (!Yap_locked_gc(nargs, env, cp)) {
      Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
      return 0;
    }
    return 1;
  }
  return -1;
}

static int
code_overflow( CELL *yenv USES_REGS )
{
  if (Yap_get_signal( YAP_CDOVF_SIGNAL )) {
    CELL cut_b = LCL0-(CELL *)(yenv[E_CB]);

    /* do a garbage collection first to check if we can recover memory */
    if (!Yap_locked_growheap(false, 0, NULL)) {
      Yap_NilError(RESOURCE_ERROR_HEAP, "YAP failed to grow heap: %s", LOCAL_ErrorMessage);
      return 0;
    }
    CACHE_A1();
    if (yenv == ASP) {
      yenv[E_CB] = (CELL)(LCL0-cut_b);
    }
    return 1;
  }
  return -1;
}

static int
interrupt_handler( PredEntry *pe USES_REGS )
{

  //  printf("D %lx %p\n", LOCAL_ActiveSignals, P);
  /* tell whether we can creep or not, this is hard because we will
     lose the info RSN
  */
  BEGD(d0);
  d0 = pe->ArityOfPE;
  if (d0 == 0) {
    HR[1] = MkAtomTerm((Atom) pe->FunctorOfPred);
  }
  else {
    HR[d0 + 2] = AbsAppl(HR);
    *HR = (CELL) pe->FunctorOfPred;
    HR++;
    BEGP(pt1);
    pt1 = XREGS + 1;
    for (; d0 > 0; --d0) {
      BEGD(d1);
      BEGP(pt0);
      pt0 = pt1;
      d1 = *pt0;
      deref_head(d1, creep_unk);
    creep_nonvar:
      /* just copy it to the heap */
      pt1++;
      *HR++ = d1;
      continue;

      derefa_body(d1, pt0, creep_unk, creep_nonvar);
      if (pt0 <= HR) {
	/* variable is safe */
	*HR++ = (CELL)pt0;
	pt1++;
      } else {
	/* bind it, in case it is a local variable */
	d1 = Unsigned(HR);
	RESET_VARIABLE(HR);
	pt1++;
	HR += 1;
	Bind_Local(pt0, d1);
      }
      ENDP(pt0);
      ENDD(d1);
    }
    ENDP(pt1);
  }
  ENDD(d0);
  HR[0] = Yap_Module_Name(pe);
  ARG1 = (Term) AbsPair(HR);

  HR += 2;
#ifdef COROUTINING
  if (Yap_get_signal( YAP_WAKEUP_SIGNAL )) {
    CalculateStackGap( PASS_REGS1 );
    ARG2 = Yap_ListOfWokenGoals();
    pe = WakeUpCode;
    /* no more goals to wake up */
    Yap_UpdateTimedVar(LOCAL_WokenGoals,TermNil);
  } else
#endif
    {
      CalculateStackGap( PASS_REGS1 );
      pe = CreepCode;
    }
  P = pe->CodeOfPred;
#ifdef LOW_LEVEL_TRACER
  if (Yap_do_low_level_trace)
    low_level_trace(enter_pred,pe,XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
  /* for profiler */
  CACHE_A1();
  return true;
}


// interrupt handling code that sets up the case when we do not have
// a guaranteed environment.
static int
safe_interrupt_handler( PredEntry *pe USES_REGS )
{
  CELL *npt = HR;

  //  printf("D %lx %p\n", LOCAL_ActiveSignals, P);
  /* tell whether we can creep or not, this is hard because we will
     lose the info RSN
  */
  BEGD(d0);
  S = (CELL *)pe;
  d0 = pe->ArityOfPE;
  if (d0 == 0) {
    HR[1] = MkAtomTerm((Atom) pe->FunctorOfPred);
  }
  else {
    HR[d0 + 2] = AbsAppl(HR);
    HR += d0+1+2;
    *npt++ = (CELL) pe->FunctorOfPred;
    BEGP(pt1);
    pt1 = XREGS + 1;
    for (; d0 > 0; --d0) {
      BEGD(d1);
      d1 = *pt1;
    loop:
      if (!IsVarTerm(d1)) {
	/* just copy it to the heap */
	pt1++;
	*npt++ = d1;
      } else {
	if (VarOfTerm(d1) < H0 || VarOfTerm(d1) > HR) {
	  d1 = Deref(d1);
	  if (VarOfTerm(d1) < H0 || VarOfTerm(d1) > HR) {
	    Term v = MkVarTerm();
	    YapBind( VarOfTerm(d1),v );
	  } else {
	    goto loop;
	  }
	} else {
	  *npt++ = d1;
	}
      }
      ENDD(d1);
    }
    ENDP(pt1);
  }
  ENDD(d0);
  npt[0] = Yap_Module_Name(pe);
  ARG1 = AbsPair(npt);

  HR += 2;
#ifdef COROUTINING
  if (Yap_get_signal( YAP_WAKEUP_SIGNAL )) {
    CalculateStackGap( PASS_REGS1 );
    ARG2 = Yap_ListOfWokenGoals();
    pe = WakeUpCode;
    /* no more goals to wake up */
    Yap_UpdateTimedVar(LOCAL_WokenGoals,TermNil);
  } else
#endif
    {
      CalculateStackGap( PASS_REGS1 );
      pe = CreepCode;
    }
  // allocate and fill out an environment
  YENV = ASP;
  CACHE_Y_AS_ENV(YREG);
  ENV_YREG[E_CP] = (CELL) CP;
  ENV_YREG[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
  ENV_YREG[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
  ENV = ENV_YREG;
  ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size(CP));
  WRITEBACK_Y_AS_ENV();
  ENDCACHE_Y_AS_ENV();
  CP = P;
  P = pe->CodeOfPred;
#ifdef DEPTH_LIMIT
  if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is primitives */
    if (pe->ModuleOfPred) {
      if (DEPTH == MkIntTerm(0))
		return false;
      else DEPTH = RESET_DEPTH();
    }
  } else if (pe->ModuleOfPred) {
    DEPTH -= MkIntConstant(2);
  }
#endif	/* DEPTH_LIMIT */
  return true;
}

static int
interrupt_handlerc( PredEntry *pe USES_REGS )
{
  /* do creep in call                                     */
  ENV = YENV;
  CP = NEXTOP(P, Osbpp);
  YENV = (CELL *) (((char *) YENV) + P->y_u.Osbpp.s);
#ifdef FROZEN_STACKS
  {
    choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
    if (YENV > (CELL *) top_b || YENV < HR) YENV = (CELL *) top_b;
#else
    if (YENV > (CELL *) top_b) YENV = (CELL *) top_b;
#endif /* YAPOR_SBA */
    else YENV = YENV + ENV_Size(CP);
  }
#else
  if (YENV > (CELL *) B)
    YENV = (CELL *) B;
  else
    /* I am not sure about this */
    YENV = YENV + ENV_Size(CP);
#endif /* FROZEN_STACKS */
  /* setup GB */
  YENV[E_CB] = (CELL) B;
  return interrupt_handler( pe PASS_REGS );
}

static int
interrupt_handler_either( Term t_cut, PredEntry *pe USES_REGS )
{
	int rc;

    ARG1 = push_live_regs(NEXTOP(P, Osbpp));
#ifdef FROZEN_STACKS
  {
    choiceptr top_b = PROTECT_FROZEN_B(B);
    // protect registers before we mess about.
    // recompute YENV and get ASP
#ifdef YAPOR_SBA
    if (YENV > (CELL *) top_b || YENV < HR) YENV = (CELL *) top_b;
#else
    if (YENV > (CELL *) top_b) YENV = (CELL *) top_b;
#endif /* YAPOR_SBA */
	else YENV = YENV + ENV_Size(CP);
  }
#else
  if (YENV > (CELL *) B)
    YENV = (CELL *) B;
#endif /* FROZEN_STACKS */
  P = NEXTOP(P, Osbpp);
  // should we cut? If t_cut == INT(0) no
  ARG2 = t_cut;
  // ASP
 SET_ASP(YENV, E_CB*sizeof(CELL));
 // do the work.
  rc = safe_interrupt_handler( pe PASS_REGS );
  return rc;
}

/* to trace interrupt calls */
// #define DEBUG_INTERRUPTS 1

#ifdef DEBUG_INTERRUPTS
static int trace_interrupts = true;
#endif

static int
interrupt_fail( USES_REGS1 )
{
#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d:  (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  check_alarm_fail_int( false PASS_REGS );
  /* don't do debugging and stack expansion here: space will
     be recovered. automatically by fail, so
     better wait.
  */
  if (Yap_has_signal( YAP_CREEP_SIGNAL ) ) {
    return false;
  }
  if (Yap_has_signal( YAP_CDOVF_SIGNAL ) ) {
    return false;
  }
  /* make sure we have the correct environment for continuation */
  ENV = B->cp_env;
  YENV  = (CELL *)B;
  return interrupt_handler( RepPredProp(Yap_GetPredPropByAtom(AtomFail,0)) PASS_REGS );
}

static int
interrupt_execute( USES_REGS1 )
{
  int v;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d: (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int(  true PASS_REGS )) >= 0) {
    return v;
  }
  if (PP) UNLOCKPE(1,PP);
  PP  = P->y_u.pp.p0;
  if ((P->y_u.pp.p->PredFlags & (NoTracePredFlag|HiddenPredFlag)) && Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return 2;
  }
  SET_ASP(YENV, E_CB*sizeof(CELL));
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  if ((v = stack_overflow(P->y_u.pp.p, ENV, CP, P->y_u.pp.p->ArityOfPE PASS_REGS )) >= 0) {
    return v;
  }
  return interrupt_handler( P->y_u.pp.p PASS_REGS );
}

static int
interrupt_call( USES_REGS1 )
{
  int v;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d:  (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int( true PASS_REGS )) >= 0) {
    return v;
  }
  if (PP) UNLOCKPE(1,PP);
  PP = P->y_u.Osbpp.p0;
  if (Yap_only_has_signal(YAP_CREEP_SIGNAL) &&
      (P->y_u.Osbpp.p->PredFlags & (NoTracePredFlag|HiddenPredFlag)) ) {
    return 2;
  }
  SET_ASP(YENV, P->y_u.Osbpp.s);
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  if ((v = stack_overflow( P->y_u.Osbpp.p, YENV, NEXTOP(P, Osbpp), P->y_u.Osbpp.p->ArityOfPE PASS_REGS )) >= 0) {
    return v;
  }
  return interrupt_handlerc( P->y_u.Osbpp.p PASS_REGS );
}

static int
interrupt_pexecute( PredEntry *pen USES_REGS )
{
  int v;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d:  (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int( 2 PASS_REGS )) >= 0) {
    return v;
  }
  if (PP) UNLOCKPE(1,PP);
  PP = NULL;
  if (Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return 2; /* keep on creeping */
  }
  SET_ASP(YENV, E_CB*sizeof(CELL));
  /* setup GB */
  YENV[E_CB] = (CELL) B;
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  if ((v = stack_overflow( pen, ENV, NEXTOP(P, Osbmp), pen->ArityOfPE PASS_REGS )) >= 0) {
	return v;
  }
  CP = NEXTOP(P, Osbmp);
  return interrupt_handler( pen PASS_REGS );
}

static void
execute_dealloc( USES_REGS1 )
{
     /* other instructions do depend on S being set by deallocate
         :-( */
  CELL *ENV_YREG = YENV;
  S = ENV_YREG;
  CP = (yamop *) ENV_YREG[E_CP];
  ENV = ENV_YREG = (CELL *) ENV_YREG[E_E];
#ifdef DEPTH_LIMIT
  DEPTH = ENV_YREG[E_DEPTH];
#endif  /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
  {
    choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
    if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
    if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
    else ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size(CP));
  }
#else
  if (ENV_YREG > (CELL *) B)
    ENV_YREG = (CELL *) B;
      else
        ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size(CP));
#endif /* FROZEN_STACKS */
  YENV = ENV_YREG;
  P = NEXTOP(P,p);
      
}

      /* don't forget I cannot creep at deallocate (where to?) */
      /* also, this is unusual in that I have already done deallocate,
	 so I don't need to redo it.
       */
static int
interrupt_deallocate( USES_REGS1 )
{
  int v;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int( true PASS_REGS )) >= 0) {
    return v;
  }
  /*
     don't do a creep here; also, if our instruction is followed by
     a execute_c, just wait a bit more */
  if ( Yap_only_has_signal( YAP_CREEP_SIGNAL ) ||
	/* keep on going if there is something else */
       (P->opc != Yap_opcode(_procceed) &&
	P->opc != Yap_opcode(_cut_e))) {
    execute_dealloc( PASS_REGS1 );
    return 1;
  } else {
    CELL cut_b = LCL0-(CELL *)(S[E_CB]);

    if (PP) UNLOCKPE(1,PP);
    PP = PREVOP(P,p)->y_u.p.p;
    ASP = YENV+E_CB;
    /* cut_e */
    SET_ASP(YENV, E_CB*sizeof(CELL));
    if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
      return v;
    }
    if (Yap_has_a_signal()) {
        PredEntry *pe;

      if (Yap_op_from_opcode(P->opc) == _cut_e) {
	       /* followed by a cut */
	        ARG1 = MkIntegerTerm(LCL0-(CELL *)S[E_CB]);
	         pe = RepPredProp(Yap_GetPredPropByFunc(FunctorCutBy,1));
      } else {
	       pe = RepPredProp(Yap_GetPredPropByAtom(AtomTrue,0));
      }
      return interrupt_handler( pe PASS_REGS );
    }
    if (!Yap_locked_gc(0, ENV, YESCODE)) {
      Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
    }
    S = ASP;
    S[E_CB] = (CELL)(LCL0-cut_b);
  }
  return 1;
}

static int
interrupt_cut( USES_REGS1 )
{
  Term t_cut = MkIntegerTerm(LCL0-(CELL *)YENV[E_CB]);
  int v;
#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int( 2 PASS_REGS )) >= 0) {
    return v;
  }
  if (!Yap_has_a_signal()
      || Yap_only_has_signals(YAP_CDOVF_SIGNAL , YAP_CREEP_SIGNAL )) {
    return 2;
  }
  /* find something to fool S */
  P = NEXTOP(P,s);
  return interrupt_handler_either( t_cut, PredRestoreRegs PASS_REGS );
}

static int
interrupt_cut_t( USES_REGS1 )
{
  Term t_cut = MkIntegerTerm(LCL0-(CELL *)YENV[E_CB]);
  int v;
#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d: (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int( 2 PASS_REGS )) >= 0) {
    return v;
  }
  if (!Yap_has_a_signal()
      || Yap_only_has_signals(YAP_CDOVF_SIGNAL , YAP_CREEP_SIGNAL )) {
    return 2;
  }
  /* find something to fool S */
  P = NEXTOP(P,s);
  return interrupt_handler_either( t_cut, PredRestoreRegs PASS_REGS );
}

static int
interrupt_cut_e( USES_REGS1 )
{
  Term t_cut = MkIntegerTerm(LCL0-(CELL *)S[E_CB]);
  int v;
#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d: (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int( 2 PASS_REGS )) >= 0) {
    return v;
  }
  if (!Yap_only_has_signals(YAP_CDOVF_SIGNAL , YAP_CREEP_SIGNAL )) {
    return 2;
  }
  /* find something to fool S */
  P = NEXTOP(P,s);
  return interrupt_handler_either( t_cut, PredRestoreRegs PASS_REGS );
}

static int
interrupt_commit_y( USES_REGS1 )
{
  int v;
  Term t_cut = YENV[P->y_u.yps.y];

  #ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d: (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int( 2 PASS_REGS )) >= 0) {
    return v;
  }
  if (!Yap_has_a_signal()
      || Yap_only_has_signals(YAP_CDOVF_SIGNAL , YAP_CREEP_SIGNAL )) {
    return 2;
  }
  /* find something to fool S */
  P = NEXTOP(P,yps);
  return interrupt_handler_either( t_cut, PredRestoreRegs PASS_REGS );
}

static int
interrupt_commit_x( USES_REGS1 )
{
  int v;
  Term t_cut = XREG(P->y_u.xps.x);

  #ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s:%d (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int( 2 PASS_REGS )) >= 0) {
    return v;
  }
  if (Yap_only_has_signals(YAP_CDOVF_SIGNAL , YAP_CREEP_SIGNAL )) {
    return 2;
  }
  if (PP) UNLOCKPE(1,PP);
  PP = P->y_u.xps.p0;
  /* find something to fool S */
  if (P->opc == Yap_opcode(_fcall)) {
    /* fill it up */
    CACHE_Y_AS_ENV(YREG);
    ENV_YREG[E_CP] = (CELL) CP;
    ENV_YREG[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
    ENV_YREG[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
    ENDCACHE_Y_AS_ENV();
  }
  P = NEXTOP(P,xps);
  return interrupt_handler_either( t_cut, PredRestoreRegs PASS_REGS );
}

static int
interrupt_either( USES_REGS1 )
{
  int v;

#ifdef DEBUGX
  //if (trace_interrupts)
  fprintf(stderr,"[%d] %s:%d:  (YENV=%p ENV=%p ASP=%p)\n",  worker_id, \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if ((v = check_alarm_fail_int( 2 PASS_REGS )) >= 0) {
    return v;
  }
  if (Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return 2;
  }
  if (PP) UNLOCKPE(1,PP);
  PP = P->y_u.Osblp.p0;
  /* find something to fool S */
  SET_ASP(YENV, P->y_u.Osbpp.s);
  if (ASP > (CELL *)PROTECT_FROZEN_B(B))
    ASP = (CELL *)PROTECT_FROZEN_B(B);
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  //P = NEXTOP(P, Osblp);
   if ((v = stack_overflow(RepPredProp(Yap_GetPredPropByFunc(FunctorRestoreRegs1,0)), YENV, NEXTOP(P,Osblp), 0  PASS_REGS )) >= 0) {
    //P = PREVOP(P, Osblp);
    return v;
  }
  // P = PREVOP(P, Osblp);
  return interrupt_handler_either( MkIntTerm(0), RepPredProp(Yap_GetPredPropByFunc(FunctorRestoreRegs1,0)) PASS_REGS );
}

static int
interrupt_dexecute( USES_REGS1 )
{
  int v;
  PredEntry *pe;

#ifdef DEBUG_INTERRUPTS
  if (trace_interrupts) fprintf(stderr,"[%d] %lu--%lu %s/%d (YENV=%p ENV=%p ASP=%p)\n",  worker_id, LOCAL_FirstActiveSignal, LOCAL_LastActiveSignal,  \
	  __FUNCTION__, __LINE__,YENV,ENV,ASP);
#endif
  if (PP) UNLOCKPE(1,PP);
  PP = P->y_u.pp.p0;
  pe = P->y_u.pp.p;
  if ((pe->PredFlags & (NoTracePredFlag|HiddenPredFlag)) && Yap_only_has_signal(YAP_CREEP_SIGNAL)) {
    return 2;
  }
  /* set S for next instructions */
  ASP = YENV+E_CB;
  if (ASP > (CELL *)PROTECT_FROZEN_B(B))
    ASP = (CELL *)PROTECT_FROZEN_B(B);
  if ((v = code_overflow(YENV PASS_REGS)) >= 0) {
    return v;
  }
  if ((v = stack_overflow( P->y_u.pp.p, (CELL *)YENV[E_E], (yamop *)YENV[E_CP], P->y_u.pp.p->ArityOfPE PASS_REGS )) >= 0) {
      return v;
  }
/* first, deallocate */
  CP = (yamop *) YENV[E_CP];
  ENV = YENV = (CELL *) YENV[E_E];
#ifdef DEPTH_LIMIT
  YENV[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
  {
    choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
    if (YENV > (CELL *) top_b || YENV < HR) YENV = (CELL *) top_b;
#else
    if (YENV > (CELL *) top_b) YENV = (CELL *) top_b;
#endif /* YAPOR_SBA */
    else YENV = (CELL *) ((CELL)YENV + ENV_Size(CPREG));
  }
#else
  if (YENV > (CELL *) B) {
    YENV = (CELL *) B;
  }
  else {
    YENV = (CELL *) ((CELL) YENV + ENV_Size(CPREG));
  }
#endif /* FROZEN_STACKS */
  /* setup GB */
  YENV[E_CB] = (CELL) B;

  /* and now CREEP */
  return interrupt_handler( pe PASS_REGS );
}

static void
undef_goal( USES_REGS1 )
{
  PredEntry *pe = PredFromDefCode(P);
  if (Yap_UnknownFlag(CurrentModule) == TermFail) {
    P = FAILCODE;
    return;      
  }
  BEGD(d0);
  /* avoid trouble with undefined dynamic procedures */
  /* I assume they were not locked beforehand */
#if defined(YAPOR) || defined(THREADS)
  if (!PP) {
    PELOCK(19,pe);
    PP = pe;
  }
#endif
  if (pe->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|MultiFileFlag)) {
#if defined(YAPOR) || defined(THREADS)
    UNLOCKPE(19,PP);
    PP = NULL;
#endif
    P = FAILCODE;
    return;
  }
#if defined(YAPOR) || defined(THREADS)
  UNLOCKPE(19,PP);
  PP = NULL;
#endif
  d0 = pe->ArityOfPE;
  if (d0 == 0) {
    HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred));
  }
  else {
    HR[d0 + 2] = AbsAppl(HR);
    *HR = (CELL) pe->FunctorOfPred;
    HR++;
    BEGP(pt1);
    pt1 = XREGS + 1;
    for (; d0 > 0; --d0) {
      BEGD(d1);
      BEGP(pt0);
      pt0 = pt1++;
      d1 = *pt0;
      deref_head(d1, undef_unk);
    undef_nonvar:
      /* just copy it to the heap */
      *HR++ = d1;
      continue;

      derefa_body(d1, pt0, undef_unk, undef_nonvar);
      if (pt0 <= HR) {
	/* variable is safe */
	*HR++ = (CELL)pt0;
      } else {
	/* bind it, in case it is a local variable */
	d1 = Unsigned(HR);
	RESET_VARIABLE(HR);
	HR += 1;
	Bind_Local(pt0, d1);
      }
      ENDP(pt0);
      ENDD(d1);
    }
    ENDP(pt1);
  }
  ENDD(d0);
  HR[0] = Yap_Module_Name(pe);
  ARG1 = (Term) AbsPair(HR);
  ARG2 = Yap_getUnknownModule(Yap_GetModuleEntry(HR[0]));
  HR += 2;
#ifdef LOW_LEVEL_TRACER
  if (Yap_do_low_level_trace)
    low_level_trace(enter_pred,UndefCode,XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
  P = UndefCode->CodeOfPred;
}


static void
spy_goal( USES_REGS1 )
{
  PredEntry *pe = PredFromDefCode(P);

#if defined(YAPOR) || defined(THREADS)
  if (!PP) {
    PELOCK(14,pe);
    PP = pe;
  }
#endif
  BEGD(d0);
  if (!(pe->PredFlags & IndexedPredFlag) &&
      pe->cs.p_code.NOfClauses > 1) {
    /* update ASP before calling IPred */
    SET_ASP(YREG, E_CB*sizeof(CELL));
    Yap_IPred(pe, 0, CP);
    /* IPred can generate errors, it thus must get rid of the lock itself */
    if (P == FAILCODE) {
#if defined(YAPOR) || defined(THREADS)
      if (PP && !(PP->PredFlags & LogUpdatePredFlag)){
	UNLOCKPE(20,pe);
	PP = NULL;
      }
#endif
      return;
    }
  }
  /* first check if we need to increase the counter */
  if ((pe->PredFlags & CountPredFlag)) {
    LOCK(pe->StatisticsForPred->lock);
    pe->StatisticsForPred->NOfEntries++;
    UNLOCK(pe->StatisticsForPred->lock);
    LOCAL_ReductionsCounter--;
    if (LOCAL_ReductionsCounter == 0 && LOCAL_ReductionsCounterOn) {
#if defined(YAPOR) || defined(THREADS)
      if (PP) {
	UNLOCKPE(20,pe);
	PP = NULL;
      }
#endif
      Yap_NilError(CALL_COUNTER_UNDERFLOW_EVENT,"");
      return;
    }
    LOCAL_PredEntriesCounter--;
    if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
#if defined(YAPOR) || defined(THREADS)
      if (PP) {
	UNLOCKPE(21,pe);
	PP = NULL;
      }
#endif
      Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
      return;
    }
    if ((pe->PredFlags & (CountPredFlag|ProfiledPredFlag|SpiedPredFlag)) ==
	CountPredFlag) {
#if defined(YAPOR) || defined(THREADS)
      if (PP) {
	UNLOCKPE(22,pe);
	PP = NULL;
      }
#endif
      P = pe->cs.p_code.TrueCodeOfPred;
      return;
    }
  }
  /* standard profiler */
  if ((pe->PredFlags & ProfiledPredFlag)) {
    LOCK(pe->StatisticsForPred->lock);
    pe->StatisticsForPred->NOfEntries++;
    UNLOCK(pe->StatisticsForPred->lock);
    if (!(pe->PredFlags & SpiedPredFlag)) {
      P = pe->cs.p_code.TrueCodeOfPred;
#if defined(YAPOR) || defined(THREADS)
      if (PP) {
	UNLOCKPE(23,pe);
	PP = NULL;
      }
#endif
      return;
    }
  }
#if defined(YAPOR) || defined(THREADS)
  if (PP) {
    UNLOCKPE(25,pe);
    PP = NULL;
  }
#endif

  d0 = pe->ArityOfPE;
  /* save S for ModuleName */
  if (d0 == 0) {
    HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred));
  } else {
    *HR = (CELL) pe->FunctorOfPred;
    HR[d0 + 2] = AbsAppl(HR);
    HR++;
    BEGP(pt1);
    pt1 = XREGS + 1;
    for (; d0 > 0; --d0) {
      BEGD(d1);
      BEGP(pt0);
      pt0 = pt1++;
      d1 = *pt0;
      deref_head(d1, dospy_unk);
    dospy_nonvar:
      /* just copy it to the heap */
      *HR++ = d1;
      continue;

      derefa_body(d1, pt0, dospy_unk, dospy_nonvar);
      if (pt0 <= HR) {
	/* variable is safe */
	*HR++ = (CELL)pt0;
      } else {
	/* bind it, in case it is a local variable */
	d1 = Unsigned(HR);
	RESET_VARIABLE(HR);
	HR += 1;
	Bind_Local(pt0, d1);
      }
      ENDP(pt0);
      ENDD(d1);
    }
    ENDP(pt1);
  }
  ENDD(d0);
  HR[0] = Yap_Module_Name(pe);

  ARG1 = (Term) AbsPair(HR);
  HR += 2;
  {
    PredEntry *pt0;
#if THREADS
    LOCK(GLOBAL_ThreadHandlesLock);
#endif
    pt0 = SpyCode;
    P_before_spy = P;
    P = pt0->CodeOfPred;
    /* for profiler */
#if THREADS
    UNLOCK(GLOBAL_ThreadHandlesLock);
#endif
#ifdef LOW_LEVEL_TRACER
    if (Yap_do_low_level_trace)
      low_level_trace(enter_pred,pt0,XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
  }
}

Int
Yap_absmi(int inp)
{
  CACHE_REGS
#if BP_FREE
  /* some function might be using bp for an internal variable, it is the
     callee's responsability to save it */
  yamop* PCBACKUP = P1REG;
#endif

#ifdef LONG_LIVED_REGISTERS
  register CELL d0, d1;
  register CELL *pt0, *pt1;

#endif /* LONG_LIVED_REGISTERS */

#ifdef SHADOW_P
  register yamop *PREG = P;
#endif /* SHADOW_P */

#ifdef SHADOW_CP
  register yamop *CPREG = CP;
#endif /* SHADOW_CP */

#ifdef SHADOW_HB
  register CELL *HBREG = HB;
#endif /* SHADOW_HB */

#ifdef SHADOW_Y
  register CELL *YREG = Yap_REGS.YENV_;
#endif /* SHADOW_Y */

#ifdef SHADOW_S
  register CELL *SREG = Yap_REGS.S_;
#else
#define SREG S
#endif /* SHADOW_S */

  /* The indexing register so that we will not destroy ARG1 without
   * reason */
#define I_R (XREGS[0])

#if YAP_JIT
  Yap_ExpEnvP = & Yap_ExpEnv;
  static void *control_labels[] = { &&fail, &&NoStackCut, &&NoStackCommitY, &&NoStackCutT, &&NoStackEither, &&NoStackExecute, &&NoStackCall, &&NoStackDExecute, &&NoStackDeallocate, &&notrailleft, &&NoStackFail, &&NoStackCommitX };
  curtrace = NULL;
  curpreg = NULL;
  globalcurblock = NULL;
  ineedredefinedest = 0;
  NativeArea = (NativeContext*)malloc(sizeof(NativeContext));
  NativeArea->area.p = NULL;
  NativeArea->area.ok = NULL;
  NativeArea->area.pc = NULL;
#if YAP_STAT_PREDS
  NativeArea->area.nrecomp = NULL;
  NativeArea->area.compilation_time = NULL;
  NativeArea->area.native_size_bytes = NULL;
  NativeArea->area.trace_size_bytes = NULL;
  NativeArea->success = NULL;
  NativeArea->runs = NULL;
  NativeArea->t_runs = NULL;
#endif
  NativeArea->n = 0;
  IntermediatecodeArea = (IntermediatecodeContext*)malloc(sizeof(IntermediatecodeContext));
  IntermediatecodeArea->area.t = NULL;
  IntermediatecodeArea->area.ok = NULL;
  IntermediatecodeArea->area.isactive = NULL;
  IntermediatecodeArea->area.lastblock = NULL;
#if YAP_STAT_PREDS
  IntermediatecodeArea->area.profiling_time = NULL;
#endif
  IntermediatecodeArea->n = 0;
  nnexec = 0;
  l = 0;
#endif /* YAP_JIT */

#if USE_THREADED_CODE
/************************************************************************/
/*     Abstract Machine Instruction Address Table                       */
/*  This must be declared inside the function. We use the asm directive */
/* to make it available outside this function                           */
/************************************************************************/
  static void *OpAddress[] =
  {
#define OPCODE(OP,TYPE) && _##OP
#include "YapOpcodes.h"
#undef  OPCODE
  };

#if YAP_JIT
  ExpEnv.config_struc.TOTAL_OF_OPCODES =
    sizeof(OpAddress)/(2*sizeof(void*));
#endif

#endif /* USE_THREADED_CODE */

  /*static void* (*nat_glist_valx)(yamop**,yamop**,CELL**,void**,int*);

    if (nat_glist_valx == NULL) {
    nat_glist_valx = (void*(*)(yamop**,yamop**,CELL**,void**,int*))call_JIT_Compiler(J, _glist_valx);
    }*/

#ifdef SHADOW_REGS

  /* work with a local pointer to the registers */
  register REGSTORE *regp = &Yap_REGS;

#endif /* SHADOW_REGS */

#if PUSH_REGS

/* useful on a X86 with -fomit-frame-pointer optimisation */
  /* The idea is to push REGS onto the X86 stack frame */

  /* first allocate local space */
  REGSTORE absmi_regs;
  REGSTORE *old_regs = Yap_regp;

#endif /* PUSH_REGS */

#ifdef BEAM
  CELL OLD_B=B;
  extern PredEntry *bpEntry;
  if (inp==-9000) {
#if PUSH_REGS
    old_regs = &Yap_REGS;
    init_absmi_regs(&absmi_regs);
#if THREADS
    regcache = Yap_regp
    LOCAL_PL_local_data_p->reg_cache = regcache;
#else
    Yap_regp = &absmi_regs;
#endif
#endif
    CACHE_A1();
    PREG=bpEntry->CodeOfPred;
    JMPNext();			/* go execute instruction at PREG */
  }

#endif


#if USE_THREADED_CODE
  /* absmadr */
  if (inp > 0) {
    Yap_ABSMI_OPCODES = OpAddress;
#if YAP_JIT
    Yap_ABSMI_ControlLabels = control_labels;
#endif
#if BP_FREE
    P1REG = PCBACKUP;
#endif
    return(0);
  }
#endif /* USE_THREADED_CODE */

#if PUSH_REGS
  old_regs = &Yap_REGS;

  /* done, let us now initialize this space */
  init_absmi_regs(&absmi_regs);

  /* the registers are all set up, let's swap */
#ifdef THREADS
  pthread_setspecific(Yap_yaamregs_key, (const void *)&absmi_regs);
  LOCAL_ThreadHandle.current_yaam_regs = &absmi_regs;
  regcache = &absmi_regs;
  // LOCAL_PL_local_data_p->reg_cache = regcache;
#else
  Yap_regp = &absmi_regs;
#endif
#undef Yap_REGS
#define Yap_REGS absmi_regs

#endif /* PUSH_REGS */

#ifdef SHADOW_REGS

  /* use regp as a copy of REGS */
  regp = &Yap_REGS;

#ifdef REGS
#undef REGS
#endif
#define REGS (*regp)

#endif /* SHADOW_REGS */

  setregs();

  CACHE_A1();

 reset_absmi:

  SP = SP0;

#if USE_THREADED_CODE
//___androidlog_print(ANDROID_LOG_INFO, "YAP ", "%s", Yap_op_names[Yap_op_from_opcode(PREG->opc)]);

  JMPNext();			/* go execute instruction at P          */

#else
  /* when we start we are not in write mode */

  {
    op_numbers opcode = _Ystop;
    op_numbers old_op;
#ifdef DEBUG_XX
    unsigned long ops_done;
#endif

    goto nextop;

  nextop_write:

    old_op = opcode;
    opcode = PREG->y_u.o.opcw;
    goto op_switch;

  nextop:

    old_op = opcode;
    opcode = PREG->opc;

  op_switch:

#ifdef ANALYST
    GLOBAL_opcount[opcode]++;
    GLOBAL_2opcount[old_op][opcode]++;
#ifdef DEBUG_XX
    ops_done++;
    /*    if (B->cp_b > 0x103fff90)
      fprintf(stderr,"(%ld) doing %s, done %s, B is %p, HB is %p, H is %p\n",
      ops_done,Yap_op_names[opcode],Yap_op_names[old_op],B,B->cp_h,HR);*/
#endif
#endif /* ANALYST */

    switch (opcode) {
#endif /* USE_THREADED_CODE */

#if !OS_HANDLES_TR_OVERFLOW
    notrailleft:
      /* if we are within indexing code, the system may have to
       * update a S */
      {
	CELL cut_b;

#ifdef SHADOW_S
	S = SREG;
#endif
	/* YREG was pointing to where we were going to build the
	 * next choice-point. The stack shifter will need to know this
	 * to move the local stack */
	SET_ASP(YREG, E_CB*sizeof(CELL));
	cut_b = LCL0-(CELL *)(ASP[E_CB]);
	saveregs();
	if(!Yap_growtrail (0, false)) {
	  Yap_NilError(RESOURCE_ERROR_TRAIL,"YAP failed to reserve %ld bytes in growtrail",sizeof(CELL) * K16);
	  setregs();
	  FAIL();
	}
	setregs();
#ifdef SHADOW_S
	SREG = S;
#endif
	if (SREG == ASP) {
	  SREG[E_CB] = (CELL)(LCL0-cut_b);
	}
      }
      goto reset_absmi;

#endif /* OS_HANDLES_TR_OVERFLOW */

// move instructions to separate file
// so that they are easier to analyse.
#include "absmi_insts.h"


#if !USE_THREADED_CODE
    default:
      saveregs();
      Yap_Error(SYSTEM_ERROR_INTERNAL, MkIntegerTerm(opcode), "trying to execute invalid YAAM instruction %d", opcode);
      setregs();
      FAIL();
    }
  }
#else

#if PUSH_REGS
  restore_absmi_regs(old_regs);
#endif

#if BP_FREE
  P1REG = PCBACKUP;
#endif

  return (0);
#endif
}

/* dummy function that is needed for profiler */
int Yap_absmiEND(void)
{
  return 1;
}
