/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V. Santos Costa and Universidade do Porto 1985--	 *
*									 *
**************************************************************************
*									 *
* File:		stdpreds.c						 *
* comments:	General-purpose C implemented system predicates		 *
*									 *
* Last rev:     $Date: 2008-07-24 16:02:00 $,$Author: vsc $						 *
* $Log: not supported by cvs2svn $
* Revision 1.131  2008/06/12 10:55:52  vsc
* fix syntax error messages
*
* Revision 1.130  2008/04/06 11:53:02  vsc
*  fix some restore bugs
*
* Revision 1.129  2008/03/15 12:19:33  vsc
* fix flags
*
* Revision 1.128  2008/02/15 12:41:33  vsc
* more fixes to modules
*
* Revision 1.127  2008/02/13 10:15:35  vsc
* fix some bugs from yesterday plus improve support for modules in
* operators.
*
* Revision 1.126  2008/02/07 23:09:13  vsc
* don't break ISO standard in current_predicate/1.
* Include Nicos flag.
*
* Revision 1.125  2008/01/23 17:57:53  vsc
* valgrind it!
* enable atom garbage collection.
*
* Revision 1.124  2007/11/26 23:43:08  vsc
* fixes to support threads and assert correctly, even if inefficiently.
*
* Revision 1.123  2007/11/06 17:02:12  vsc
* compile ground terms away.
*
* Revision 1.122  2007/10/18 08:24:16  vsc
* fix global variables
*
* Revision 1.121  2007/10/10 09:44:24  vsc
* some more fixes to make YAP swi compatible
* fix absolute_file_name (again)
* fix setarg
*
* Revision 1.120  2007/10/08 23:02:15  vsc
* minor fixes
*
* Revision 1.119  2007/04/18 23:01:16  vsc
* fix deadlock when trying to create a module with the same name as a
* predicate (for now, just don't lock modules). obs Paulo Moura.
*
* Revision 1.118  2007/02/26 10:41:40  vsc
* fix prolog_flags for chr.
*
* Revision 1.117  2007/01/28 14:26:37  vsc
* WIN32 support
*
* Revision 1.116  2006/12/13 16:10:23  vsc
* several debugger and CLP(BN) improvements.
*
* Revision 1.115  2006/11/28 13:46:41  vsc
* fix wide_char support for name/2.
*
* Revision 1.114  2006/11/27 17:42:03  vsc
* support for UNICODE, and other bug fixes.
*
* Revision 1.113  2006/11/16 14:26:00  vsc
* fix handling of infinity in name/2 and friends.
*
* Revision 1.112  2006/11/08 01:56:47  vsc
* fix argument order in db statistics.
*
* Revision 1.111  2006/11/06 18:35:04  vsc
* 1estranha
*
* Revision 1.110  2006/10/10 14:08:17  vsc
* small fixes on threaded implementation.
*
* Revision 1.109  2006/09/15 19:32:47  vsc
* ichanges for QSAR
*
* Revision 1.108  2006/09/01 20:14:42  vsc
* more fixes for global data-structures.
* statistics on atom space.
*
* Revision 1.107  2006/08/22 16:12:46  vsc
* global variables
*
* Revision 1.106  2006/08/07 18:51:44  vsc
* fix garbage collector not to try to garbage collect when we ask for large
* chunks of stack in a single go.
*
* Revision 1.105  2006/06/05 19:36:00  vsc
* hacks
*
* Revision 1.104  2006/05/19 14:31:32  vsc
* get rid of IntArrays and FloatArray code.
* include holes when calculating memory usage.
*
* Revision 1.103  2006/05/18 16:33:05  vsc
* fix info reported by memory manager under DL_MALLOC and SYSTEM_MALLOC
*
* Revision 1.102  2006/04/28 17:53:44  vsc
* fix the expand_consult patch
*
* Revision 1.101  2006/04/28 13:23:23  vsc
* fix number of overflow bugs affecting threaded version
* make current_op faster.
*
* Revision 1.100  2006/02/05 02:26:35  tiagosoares
* MYDDAS: Top Level Functionality
*
* Revision 1.99  2006/02/05 02:17:54  tiagosoares
* MYDDAS: Top Level Functionality
*
* Revision 1.98  2005/12/17 03:25:39  vsc
* major changes to support online event-based profiling
* improve error discovery and restart on scanner.
*
* Revision 1.97  2005/11/22 11:25:59  tiagosoares
* support for the MyDDAS interface library
*
* Revision 1.96  2005/10/28 17:38:49  vsc
* sveral updates
*
* Revision 1.95  2005/10/21 16:09:02  vsc
* SWI compatible module only operators
*
* Revision 1.94  2005/09/08 22:06:45  rslopes
* BEAM for YAP update...
*
* Revision 1.93  2005/08/04 15:45:53  ricroc
* TABLING NEW: support to limit the table space size
*
* Revision 1.92  2005/07/20 13:54:27  rslopes
* solved warning: cast from pointer to integer of different size
*
* Revision 1.91  2005/07/06 19:33:54  ricroc
* TABLING: answers for completed calls can now be obtained by loading (new option) or executing (default) them from the trie data structure.
*
* Revision 1.90  2005/07/06 15:10:14  vsc
* improvements to compiler: merged instructions and fixes for ->
*
* Revision 1.89  2005/05/26 18:01:11  rslopes
* *** empty log message ***
*
* Revision 1.88  2005/04/27 20:09:25  vsc
* indexing code could get confused with suspension points
* some further improvements on oveflow handling
* fix paths in Java makefile
* changs to support gibbs sampling in CLP(BN)
*
* Revision 1.87  2005/04/07 17:48:55  ricroc
* Adding tabling support for mixed strategy evaluation (batched and local scheduling)
*   UPDATE: compilation flags -DTABLING_BATCHED_SCHEDULING and -DTABLING_LOCAL_SCHEDULING removed. To support tabling use -DTABLING in the Makefile or --enable-tabling in configure.
*   NEW: yap_flag(tabling_mode,MODE) changes the tabling execution mode of all tabled predicates to MODE (batched, local or default).
*   NEW: tabling_mode(PRED,MODE) changes the default tabling execution mode of predicate PRED to MODE (batched or local).
*
* Revision 1.86  2005/03/13 06:26:11  vsc
* fix excessive pruning in meta-calls
* fix Term->int breakage in compiler
* improve JPL (at least it does something now for amd64).
*
* Revision 1.85  2005/03/02 19:48:02  vsc
* Fix some possible errors in name/2 and friends, and cleanup code a bit
* YAP_Error changed.
*
* Revision 1.84  2005/03/02 18:35:46  vsc
* try to make initialisation process more robust
* try to make name more robust (in case Lookup new atom fails)
*
* Revision 1.83  2005/03/01 22:25:09  vsc
* fix pruning bug
* make DL_MALLOC less enthusiastic about walking through buckets.
*
* Revision 1.82  2005/02/21 16:50:04  vsc
* amd64 fixes
* library fixes
*
* Revision 1.81  2005/02/08 04:05:35  vsc
* fix mess with add clause
* improves on sigsegv handling
*
* Revision 1.80  2005/01/05 05:32:37  vsc
* Ricardo's latest version of profiler.
*
* Revision 1.79  2004/12/28 22:20:36  vsc
* some extra bug fixes for trail overflows: some cannot be recovered that easily,
* some can.
*
* Revision 1.78  2004/12/08 04:45:03  vsc
* polish changes to undefp
* get rid of a few warnings
*
* Revision 1.77  2004/12/05 05:07:26  vsc
* name/2 should accept [] as a valid list (string)
*
* Revision 1.76  2004/12/05 05:01:25  vsc
* try to reduce overheads when running with goal expansion enabled.
* CLPBN fixes
* Handle overflows when allocating big clauses properly.
*
* Revision 1.75  2004/12/02 06:06:46  vsc
* fix threads so that they at least start
* allow error handling to work with threads
* replace heap_base by Yap_heap_base, according to Yap's convention for globals.
*
* Revision 1.74  2004/11/19 22:08:43  vsc
* replace SYSTEM_ERROR by out OUT_OF_WHATEVER_ERROR whenever appropriate.
*
* Revision 1.73  2004/11/19 17:14:14  vsc
* a few fixes for 64 bit compiling.
*
* Revision 1.72  2004/11/18 22:32:37  vsc
* fix situation where we might assume nonextsing double initialisation of C predicates (use
* Hidden Pred Flag).
* $host_type was double initialised.
*
* Revision 1.71  2004/07/23 21:08:44  vsc
* windows fixes
*
* Revision 1.70  2004/06/29 19:04:42  vsc
* fix multithreaded version
* include new version of Ricardo's profiler
* new predicat atomic_concat
* allow multithreaded-debugging
* small fixes
*
* Revision 1.69  2004/06/16 14:12:53  vsc
* miscellaneous fixes
*
* Revision 1.68  2004/05/14 17:11:30  vsc
* support BigNums in interface
*
* Revision 1.67  2004/05/14 16:33:45  vsc
* add Yap_ReadBuffer
*
* Revision 1.66  2004/05/13 20:54:58  vsc
* debugger fixes
* make sure we always go back to current module, even during initizlization.
*
* Revision 1.65  2004/04/27 15:14:36  vsc
* fix halt/0 and halt/1
*									 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#define HAS_CACHE_REGS 1
/*
 * This file includes the definition of a miscellania of standard predicates
 * for yap refering to: Consulting, Executing a C predicate from call,
 * Comparisons (both general and numeric), Structure manipulation, Direct
 * access to atoms and predicates, Basic support for the debugger 
 *
 * It also includes a table where all C-predicates are initializated 
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "eval.h"
#include "yapio.h"
#include "pl-shared.h"
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <wchar.h>

static Int p_setval( USES_REGS1 );
static Int p_value( USES_REGS1 );
static Int p_values( USES_REGS1 );
#ifdef undefined
static CODEADDR *FindAtom(CODEADDR, int *);
#endif /* undefined */
static Int p_opdec( USES_REGS1 );
static Int p_univ( USES_REGS1 );
static Int p_abort( USES_REGS1 );
#ifdef BEAM
Int p_halt( USES_REGS1 );
#else
static Int p_halt( USES_REGS1 );
#endif
static Int init_current_predicate( USES_REGS1 );
static Int cont_current_predicate( USES_REGS1 );
static Int init_current_predicate_for_atom( USES_REGS1 );
static Int cont_current_predicate_for_atom( USES_REGS1 );
static OpEntry *NextOp(OpEntry * CACHE_TYPE);
static Int init_current_op( USES_REGS1 );
static Int cont_current_op( USES_REGS1 );
static Int init_current_atom_op( USES_REGS1 );
static Int cont_current_atom_op( USES_REGS1 );
static Int p_flags( USES_REGS1 );
static Int TrailMax(void);
static Int GlobalMax(void);
static Int LocalMax(void);
static Int p_statistics_heap_max( USES_REGS1 );
static Int p_statistics_global_max( USES_REGS1 );
static Int p_statistics_local_max( USES_REGS1 );
static Int p_statistics_heap_info( USES_REGS1 );
static Int p_statistics_stacks_info( USES_REGS1 );
static Int p_statistics_trail_info( USES_REGS1 );
static Term mk_argc_list( USES_REGS1 );
static Int p_argv( USES_REGS1 );
static Int p_cputime( USES_REGS1 );
static Int p_systime( USES_REGS1 );
static Int p_runtime( USES_REGS1 );
static Int p_walltime( USES_REGS1 );
static Int p_access_yap_flags( USES_REGS1 );
static Int p_set_yap_flags( USES_REGS1 );
static Int p_break( USES_REGS1 );

#ifdef BEAM
Int use_eam( USES_REGS1 );
Int eager_split( USES_REGS1 );
Int force_wait( USES_REGS1 );
Int commit( USES_REGS1 );
Int skip_while_var( USES_REGS1 );
Int wait_while_var( USES_REGS1 );
Int show_time( USES_REGS1 );
Int start_eam( USES_REGS1 );
Int cont_eam( USES_REGS1 );

extern int EAM;
extern int eam_am(PredEntry*);
extern int showTime(void); 

Int start_eam( USES_REGS1 ) {
  if (eam_am((PredEntry *) 0x1)) return (TRUE); 
  else { cut_fail(); return (FALSE); }
}

Int cont_eam( USES_REGS1 ) {
  if (eam_am((PredEntry *) 0x2)) return (TRUE); 
  else { cut_fail(); return (FALSE); }
}

Int use_eam( USES_REGS1 ) { 
  if (EAM)  EAM=0;
    else { Yap_PutValue(AtomCArith,0); EAM=1; }
  return(TRUE);
}

Int commit( USES_REGS1 ) { 
  if (EAM) {
  printf("Nao deveria ter sido chamado commit do stdpreds\n");
  exit(1);
  }
  return(TRUE);
}

Int skip_while_var( USES_REGS1 ) { 
  if (EAM) {
  printf("Nao deveria ter sido chamado skip_while_var do stdpreds\n");
  exit(1);
  }
  return(TRUE);
}

Int wait_while_var( USES_REGS1 ) { 
  if (EAM) {
  printf("Nao deveria ter sido chamado wait_while_var do stdpreds\n");
  exit(1);
  }
  return(TRUE);
}

Int force_wait( USES_REGS1 ) {
  if (EAM) {
  printf("Nao deveria ter sido chamado force_wait do stdpreds\n");
  exit(1);
  }
  return(TRUE);
}

Int eager_split( USES_REGS1 ) {
  if (EAM) {
  printf("Nao deveria ter sido chamado eager_split do stdpreds\n");
  exit(1);
  }
  return(TRUE);
}

Int show_time( USES_REGS1 )  /* MORE PRECISION */
{
  return (showTime());
}

#endif /* BEAM */ 

static Int 
p_setval( USES_REGS1 )
{				/* '$set_value'(+Atom,+Atomic) */
	Term            t1 = Deref(ARG1), t2 = Deref(ARG2);
	if (!IsVarTerm(t1) && IsAtomTerm(t1) &&
	    (!IsVarTerm(t2) && (IsAtomTerm(t2) || IsNumTerm(t2)))) {
		Yap_PutValue(AtomOfTerm(t1), t2);
		return (TRUE);
	}
	return (FALSE);
}

static Int 
p_value( USES_REGS1 )
{				/* '$get_value'(+Atom,?Val) */
  Term t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR,t1,"get_value/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM,t1,"get_value/2");
    return (FALSE);
  }
  return (Yap_unify_constant(ARG2, Yap_GetValue(AtomOfTerm(t1))));
}


static Int 
p_values( USES_REGS1 )
{				/* '$values'(Atom,Old,New) */
  Term            t1 = Deref(ARG1), t3 = Deref(ARG3);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR,t1,"set_value/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM,t1,"set_value/2");
    return (FALSE);
  }
  if (!Yap_unify_constant(ARG2, Yap_GetValue(AtomOfTerm(t1))))
    return (FALSE);
  if (!IsVarTerm(t3)) {
    if (IsAtomTerm(t3) || IsNumTerm(t3)) {
      Yap_PutValue(AtomOfTerm(t1), t3);
    } else
      return (FALSE);
  }
  return (TRUE);
}

static Int 
p_opdec( USES_REGS1 )
{				/* '$opdec'(p,type,atom)		 */
  /* we know the arguments are integer, atom, atom */
  Term            p = Deref(ARG1), t = Deref(ARG2), at = Deref(ARG3);
  Term tmod = Deref(ARG4);
  if (tmod == TermProlog) {
    tmod = PROLOG_MODULE;
  }
  return Yap_OpDec((int) IntOfTerm(p), RepAtom(AtomOfTerm(t))->StrOfAE,
		   AtomOfTerm(at), tmod);
}


#ifdef NO_STRTOD

#if HAVE_CTYPE_H
#include <ctype.h>
#endif

double 
strtod(s, pe)
	char           *s, **pe;
{
	double          r = atof(s);
	*pe = s;
	while (*s == ' ')
		++s;
	if (*s == '+' || *s == '-')
		++s;
	if (!isdigit(*s))
		return (r);
	while (isdigit(*s))
		++s;
	if (*s == '.')
		++s;
	while (isdigit(*s))
		++s;
	if (*s == 'e' || *s == 'E')
		++s;
	if (*s == '+' || *s == '-')
		++s;
	while (isdigit(*s))
		++s;
	*pe = s;
	return (r);
}

#else

#include <stdlib.h>

#endif

#ifndef INFINITY
#define INFINITY (1.0/0.0)
#endif


static UInt 
runtime( USES_REGS1 )
{
  return(Yap_cputime()-Yap_total_gc_time()-Yap_total_stack_shift_time());
}

/* $runtime(-SinceInterval,-SinceStart)	 */
static Int 
p_runtime( USES_REGS1 )
{
  Int now, interval,
    gc_time,
    ss_time;
  Term tnow, tinterval;

  Yap_cputime_interval(&now, &interval);
  gc_time = Yap_total_gc_time();
  now -= gc_time;
  ss_time = Yap_total_stack_shift_time();
  now -= ss_time;
  interval -= (gc_time-LOCAL_LastGcTime)+(ss_time-LOCAL_LastSSTime);
  LOCAL_LastGcTime = gc_time;
  LOCAL_LastSSTime = ss_time;
  tnow = MkIntegerTerm(now);
  tinterval = MkIntegerTerm(interval);
  return( Yap_unify_constant(ARG1, tnow) && 
	 Yap_unify_constant(ARG2, tinterval) );
}

/* $cputime(-SinceInterval,-SinceStart)	 */
static Int 
p_cputime( USES_REGS1 )
{
  Int now, interval;
  Yap_cputime_interval(&now, &interval);
  return( Yap_unify_constant(ARG1, MkIntegerTerm(now)) && 
	 Yap_unify_constant(ARG2, MkIntegerTerm(interval)) );
}

static Int 
p_systime( USES_REGS1 )
{
  Int now, interval;
  Yap_systime_interval(&now, &interval);
  return( Yap_unify_constant(ARG1, MkIntegerTerm(now)) && 
	 Yap_unify_constant(ARG2, MkIntegerTerm(interval)) );
}

static Int 
p_walltime( USES_REGS1 )
{
  Int now, interval;
  Yap_walltime_interval(&now, &interval);
  return( Yap_unify_constant(ARG1, MkIntegerTerm(now)) && 
	 Yap_unify_constant(ARG2, MkIntegerTerm(interval)) );
}

static Int 
p_univ( USES_REGS1 )
{				/* A =.. L			 */
  unsigned int    arity;
  register Term   tin;
  Term            twork, t2;
  Atom            at;

  tin = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(tin)) {
    /* we need to have a list */
    Term           *Ar;
    if (IsVarTerm(t2)) {
      Yap_Error(INSTANTIATION_ERROR, t2, "(=..)/2");
      return(FALSE);
    }
    if (!IsPairTerm(t2)) {
      if (t2 == TermNil)
	Yap_Error(DOMAIN_ERROR_NON_EMPTY_LIST, t2, "(=..)/2");
      else
	Yap_Error(TYPE_ERROR_LIST, ARG2, "(=..)/2");
      return (FALSE);
    }
    twork = HeadOfTerm(t2);
    if (IsVarTerm(twork)) {
      Yap_Error(INSTANTIATION_ERROR, twork, "(=..)/2");
      return(FALSE);
    }
    if (IsNumTerm(twork)) {
      Term tt = TailOfTerm(t2);
      if (IsVarTerm(tt)) {
	Yap_Error(INSTANTIATION_ERROR, tt, "(=..)/2");
	return (FALSE);
      }
      if ( tt != MkAtomTerm(AtomNil)) {
	Yap_Error(TYPE_ERROR_ATOMIC, twork, "(=..)/2");
	return (FALSE);
      }
      return (Yap_unify_constant(ARG1, twork));
    }
    if (!IsAtomTerm(twork)) {
      Term tt = TailOfTerm(t2);
      if (IsVarTerm(tt)) {
	Yap_Error(INSTANTIATION_ERROR, twork, "(=..)/2");
	return(FALSE);
      } else if (tt == MkAtomTerm(AtomNil)) {
	Yap_Error(TYPE_ERROR_ATOMIC, twork, "(=..)/2");
	return (FALSE);
      } else {
	Yap_Error(TYPE_ERROR_ATOM, twork, "(=..)/2");
	return (FALSE);
      }
    }      
    at = AtomOfTerm(twork);
    twork = TailOfTerm(t2);
    if (IsVarTerm(twork)) {
      Yap_Error(INSTANTIATION_ERROR, twork, "(=..)/2");
      return(FALSE);
    } else  if (!IsPairTerm(twork)) {
      if (twork != TermNil) {
	Yap_Error(TYPE_ERROR_LIST, ARG2, "(=..)/2");
	return(FALSE);
      }
      return (Yap_unify_constant(ARG1, MkAtomTerm(at)));
    }
  build_compound:
    /* build the term directly on the heap */
    Ar = HR;
    HR++;
    
    while (!IsVarTerm(twork) && IsPairTerm(twork)) {
      *HR++ = HeadOfTerm(twork);
      if (HR > ASP - 1024) {
	/* restore space */
	HR = Ar;
	if (!Yap_gcl((ASP-HR)*sizeof(CELL), 2, ENV, gc_P(P,CP))) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
	twork = TailOfTerm(Deref(ARG2));
	goto build_compound;
      }
      twork = TailOfTerm(twork);
    }
    if (IsVarTerm(twork)) {
      Yap_Error(INSTANTIATION_ERROR, twork, "(=..)/2");
      return(FALSE);
    }
    if (twork != TermNil) {
      Yap_Error(TYPE_ERROR_LIST, ARG2, "(=..)/2");
      return (FALSE);
    }
#ifdef SFUNC
    DOES_NOT_WORK();
    {
      SFEntry        *pe = (SFEntry *) Yap_GetAProp(at, SFProperty);
      if (pe)
	twork = MkSFTerm(Yap_MkFunctor(at, SFArity),
			 arity, CellPtr(TR), pe->NilValue);
      else
	twork = Yap_MkApplTerm(Yap_MkFunctor(at, arity),
			   arity, CellPtr(TR));
    }
#else
    arity = HR-Ar-1;
    if (at == AtomDot && arity == 2) {
      Ar[0] = Ar[1];
      Ar[1] = Ar[2];
      HR --;
      twork = AbsPair(Ar);
    } else {      
      *Ar = (CELL)(Yap_MkFunctor(at, arity));
      twork = AbsAppl(Ar);
    }
#endif
    return (Yap_unify(ARG1, twork));
  }
  if (IsAtomicTerm(tin)) {
    twork = MkPairTerm(tin, MkAtomTerm(AtomNil));
    return (Yap_unify(twork, ARG2));
  }
  if (IsRefTerm(tin))
    return (FALSE);
  if (IsApplTerm(tin)) {
    Functor         fun = FunctorOfTerm(tin);
    if (IsExtensionFunctor ( fun ) ) {
      twork = MkPairTerm(tin, MkAtomTerm(AtomNil));
      return (Yap_unify(twork, ARG2));
    }
    arity = ArityOfFunctor(fun);
    at = NameOfFunctor(fun);
#ifdef SFUNC
    if (arity == SFArity) {
      CELL           *p = CellPtr(TR);
      CELL           *q = ArgsOfSFTerm(tin);
      int             argno = 1;
      while (*q) {
	while (*q > argno++)
	  *p++ = MkVarTerm();
	++q;
	*p++ = Deref(*q++);
      }
      twork = Yap_ArrayToList(CellPtr(TR), argno - 1);
      while (IsIntTerm(twork)) {
	if (!Yap_gc(2, ENV, gc_P(P,CP))) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  return(FALSE);
	}    
	twork = Yap_ArrayToList(CellPtr(TR), argno - 1);
      }
    } else
#endif
      {
	while (HR+arity*2 > ASP-1024) {
	  if (!Yap_gcl((arity*2)*sizeof(CELL), 2, ENV, gc_P(P,CP))) {
	    Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	    return(FALSE);
	  }
	  tin = Deref(ARG1);
	}
	twork = Yap_ArrayToList(RepAppl(tin) + 1, arity);
      }
  } else {
    /* We found a list */
    at = AtomDot;
    twork = Yap_ArrayToList(RepPair(tin), 2);
  }
  twork = MkPairTerm(MkAtomTerm(at), twork);
  return (Yap_unify(ARG2, twork));
}

static Int 
p_abort( USES_REGS1 )
{				/* abort			 */
  /* make sure we won't go creeping around */
  Yap_Error(PURE_ABORT, TermNil, "");
  return(FALSE);
}

#ifdef BEAM
extern void exit_eam(char *s); 

Int 
#else
static Int 
#endif
p_halt( USES_REGS1 )
{				/* halt				 */
  Term t = Deref(ARG1);
  Int out;

#ifdef BEAM
  if (EAM) exit_eam("\n\n[ Prolog execution halted ]\n");
#endif

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"halt/1");
    return(FALSE);
  }
  if (!IsIntegerTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER,t,"halt/1");
    return(FALSE);
  }
  out = IntegerOfTerm(t);
  Yap_exit(out);
  return TRUE;
}


static Int 
cont_current_predicate( USES_REGS1 )
{
  PredEntry      *pp = (PredEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(3,1));
  UInt Arity;
  Term name;

  while (pp != NULL) {
    if (pp->PredFlags & HiddenPredFlag) {
      pp = pp->NextPredOfModule;
    } else
      break;
  }
  if (pp == NULL)
    cut_fail();
  EXTRA_CBACK_ARG(3,1) = (CELL)MkIntegerTerm((Int)(pp->NextPredOfModule));
  if (pp->FunctorOfPred == FunctorModule)
    return FALSE;
  if (pp->ModuleOfPred != IDB_MODULE) {
    Arity = pp->ArityOfPE;
    if (Arity)
      name = MkAtomTerm(NameOfFunctor(pp->FunctorOfPred));
    else
      name = MkAtomTerm((Atom)pp->FunctorOfPred);
  } else {
    if (pp->PredFlags & NumberDBPredFlag) {
      name = MkIntegerTerm(pp->src.IndxId);
      Arity = 0;
    } else if (pp->PredFlags & AtomDBPredFlag) {
      name = MkAtomTerm((Atom)pp->FunctorOfPred);
      Arity = 0;
    } else {
      Functor f = pp->FunctorOfPred;
      name = MkAtomTerm(NameOfFunctor(f));
      Arity = ArityOfFunctor(f);
    }
  }
  if (pp->PredFlags & HiddenPredFlag)
    return FALSE;
  return
    Yap_unify(ARG2,name) &&
    Yap_unify(ARG3, MkIntegerTerm((Int)Arity));
}

static Int 
init_current_predicate( USES_REGS1 )
{
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1) || !IsAtomTerm(t1)) cut_fail();
  EXTRA_CBACK_ARG(3,1) = MkIntegerTerm((Int)Yap_ModulePred(t1));
  return cont_current_predicate( PASS_REGS1 );
}

static Int 
cont_current_predicate_for_atom( USES_REGS1 )
{
  Prop pf = (Prop)IntegerOfTerm(EXTRA_CBACK_ARG(3,1));
  Term mod = Deref(ARG2);

  while (pf != NIL) {
    FunctorEntry *pp = RepFunctorProp(pf);
    if (IsFunctorProperty(pp->KindOfPE)) {
      Prop p0;
      FUNC_READ_LOCK(pp);
      p0 = pp->PropsOfFE;
      if (p0) {
	PredEntry *p = RepPredProp(p0);
	if (p->ModuleOfPred == mod ||
	    p->ModuleOfPred == 0) {
	  UInt ar = p->ArityOfPE;
	  /* we found the predicate */
	  EXTRA_CBACK_ARG(3,1) = MkIntegerTerm((Int)(pp->NextOfPE));
	  FUNC_READ_UNLOCK(pp);
	  return 
	    Yap_unify(ARG3,MkIntegerTerm(ar));
	} else if (p->NextOfPE) {
	  UInt hash = PRED_HASH(pp,mod,PredHashTableSize);
	  READ_LOCK(PredHashRWLock);
	  PredEntry *p = PredHash[hash];
    
	  while (p) {
	    if (p->FunctorOfPred == pp &&
		p->ModuleOfPred == mod)
	      {
		READ_UNLOCK(PredHashRWLock);
        FUNC_READ_UNLOCK(pp);
		/* we found the predicate */
		EXTRA_CBACK_ARG(3,1) = MkIntegerTerm((Int)(p->NextOfPE));
		return Yap_unify(ARG3,MkIntegerTerm(p->ArityOfPE));
	      }
	    p = RepPredProp(p->NextOfPE);
	  }
	}
      }
      FUNC_READ_UNLOCK(pp);
    } else if (pp->KindOfPE == PEProp) {
      PredEntry *pe = RepPredProp(pf);
      PELOCK(31,pe);
      if (pe->PredFlags & HiddenPredFlag)
	return FALSE;
      if (pe->ModuleOfPred == mod ||
	  pe->ModuleOfPred == 0) {
	/* we found the predicate */
	EXTRA_CBACK_ARG(3,1) = MkIntegerTerm((Int)(pp->NextOfPE));
	UNLOCKPE(31,pe);
	return Yap_unify(ARG3,MkIntTerm(0));
      }
      UNLOCKPE(31,pe);
    }
    pf = pp->NextOfPE;
  }
  cut_fail();
}

static Int 
init_current_predicate_for_atom( USES_REGS1 )
{
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1) || !IsAtomTerm(t1)) cut_fail();
  EXTRA_CBACK_ARG(3,1) = MkIntegerTerm((Int)RepAtom(AtomOfTerm(t1))->PropsOfAE);
  return (cont_current_predicate_for_atom( PASS_REGS1 ));
}

static OpEntry *
NextOp(OpEntry *pp USES_REGS)
{
  while (!EndOfPAEntr(pp) && pp->KindOfPE != OpProperty &&
	 (pp->OpModule != PROLOG_MODULE || pp->OpModule != CurrentModule))
    pp = RepOpProp(pp->NextOfPE);
  return (pp);
}

int
Yap_IsOp(Atom at)
{
  CACHE_REGS
  OpEntry *op = NextOp(RepOpProp((Prop)(RepAtom(at)->PropsOfAE)) PASS_REGS);
  return (!EndOfPAEntr(op));
}

int
Yap_IsOpMaxPrio(Atom at)
{
  CACHE_REGS
  OpEntry *op = NextOp(RepOpProp((Prop)(RepAtom(at)->PropsOfAE)) PASS_REGS);
  int max;

  if (EndOfPAEntr(op))
    return 0;
  max = (op->Prefix & 0xfff);
  if ((op->Infix & 0xfff) > max)
    max = op->Infix & 0xfff;
  if ((op->Posfix & 0xfff) > max)
    max = op->Posfix & 0xfff;
  return max;
}

static Int
unify_op(OpEntry         *op USES_REGS)
{
  Term tmod = op->OpModule;

  if (tmod == PROLOG_MODULE)
    tmod = TermProlog;
  return 
    Yap_unify_constant(ARG2,tmod) &&
    Yap_unify_constant(ARG3,MkIntegerTerm(op->Prefix)) &&
    Yap_unify_constant(ARG4,MkIntegerTerm(op->Infix)) &&
    Yap_unify_constant(ARG5,MkIntegerTerm(op->Posfix));
}

static Int 
cont_current_op( USES_REGS1 )
{
  OpEntry         *op = (OpEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(5,1)), *next;
  
  READ_LOCK(op->OpRWLock);
  next = op->OpNext;
  if (Yap_unify_constant(ARG1,MkAtomTerm(op->OpName)) &&
      unify_op(op PASS_REGS)) {
    READ_UNLOCK(op->OpRWLock);
    if (next) {
      EXTRA_CBACK_ARG(5,1) = (CELL) MkIntegerTerm((CELL)next);
      return TRUE;
    } else {
      cut_succeed();
    }
  } else {
    READ_UNLOCK(op->OpRWLock);
    if (next) {
      EXTRA_CBACK_ARG(5,1) = (CELL) MkIntegerTerm((CELL)next);
      return FALSE;
    } else {
      cut_fail();
    }
  }
}

static Int 
init_current_op( USES_REGS1 )
{				/* current_op(-Precedence,-Type,-Atom)		 */
  EXTRA_CBACK_ARG(5,1) = (CELL) MkIntegerTerm((CELL)OpList);
  return cont_current_op( PASS_REGS1 );
}

static Int 
cont_current_atom_op( USES_REGS1 )
{
  OpEntry         *op = (OpEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(5,1)), *next;
  
  READ_LOCK(op->OpRWLock);
  next = NextOp(RepOpProp(op->NextOfPE) PASS_REGS);
  if (unify_op(op PASS_REGS)) {
    READ_UNLOCK(op->OpRWLock);
    if (next) {
      EXTRA_CBACK_ARG(5,1) = (CELL) MkIntegerTerm((CELL)next);
      return TRUE;
    } else {
      cut_succeed();
    }
  } else {
    READ_UNLOCK(op->OpRWLock);
    if (next) {
      EXTRA_CBACK_ARG(5,1) = (CELL) MkIntegerTerm((CELL)next);
      return FALSE;
    } else {
      cut_fail();
    }
  }
}

static Int 
init_current_atom_op( USES_REGS1 )
{				/* current_op(-Precedence,-Type,-Atom)		 */
  Term t = Deref(ARG1);
  AtomEntry *ae;
  OpEntry *ope;

  if (IsVarTerm(t) || !IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM,t,"current_op/3");
    cut_fail();
  }
  ae = RepAtom(AtomOfTerm(t));
  if (EndOfPAEntr((ope = NextOp(RepOpProp(ae->PropsOfAE) PASS_REGS)))) {
    cut_fail();
  }
  EXTRA_CBACK_ARG(5,1) = (CELL) MkIntegerTerm((Int)ope);
  return cont_current_atom_op( PASS_REGS1 );
}

static Int 
p_flags( USES_REGS1 )
{				/* $flags(+Functor,+Mod,?OldFlags,?NewFlags) */
  PredEntry      *pe;
  Int             newFl;
  Term t1 = Deref(ARG1);
  Term mod = Deref(ARG2);

  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return(FALSE);
  }
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    while ((pe = RepPredProp(PredPropByAtom(AtomOfTerm(t1), mod)))== NULL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, ARG1, "while generating new predicate");
	return FALSE;
      }
      t1 = Deref(ARG1);
      mod = Deref(ARG2);
    }
  } else if (IsApplTerm(t1)) {
    Functor         funt = FunctorOfTerm(t1);
    while ((pe = RepPredProp(PredPropByFunc(funt, mod)))== NULL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, ARG1, "while generating new predicate");
	return FALSE;
      }
      t1 = Deref(ARG1);
      mod = Deref(ARG2);
    }
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return (FALSE);
  PELOCK(92,pe);
  if (!Yap_unify_constant(ARG3, MkIntegerTerm(pe->PredFlags))) {
    UNLOCK(pe->PELock);
    return(FALSE);
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
      return(FALSE);
    }
  } else
    newFl = IntegerOfTerm(ARG4);
  pe->PredFlags = (CELL)newFl;
  UNLOCK(pe->PELock);
  return TRUE;
}

static Int 
p_set_flag( USES_REGS1 )
{				/* $flags(+Functor,+Mod,?OldFlags,?NewFlags) */
  PredEntry      *pe;
  Term t1 = Deref(ARG1);
  Term mod = Deref(ARG2);
  Term v = Deref(ARG4);
  char *s;

  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return(FALSE);
  }
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    while ((pe = RepPredProp(PredPropByAtom(AtomOfTerm(t1), mod)))== NULL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, ARG1, "while generating new predicate");
	return FALSE;
      }
      t1 = Deref(ARG1);
      mod = Deref(ARG2);
    }
  } else if (IsApplTerm(t1)) {
    Functor         funt = FunctorOfTerm(t1);
    while ((pe = RepPredProp(PredPropByFunc(funt, mod)))== NULL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, ARG1, "while generating new predicate");
	return FALSE;
      }
      t1 = Deref(ARG1);
      mod = Deref(ARG2);
    }
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return (FALSE);
  ARG3 = Deref(ARG3);
  if (IsVarTerm(ARG3)) {
    UNLOCK(pe->PELock);
    return (FALSE);
  } else if (!IsAtomTerm(ARG3)) {
    Yap_Error(TYPE_ERROR_ATOM,ARG3,"set_property/1");
    return(FALSE);
  }
  v = Deref(ARG4);
  if (IsVarTerm(ARG4)) {
    UNLOCK(pe->PELock);
    return (FALSE);
  } else if (!IsIntTerm(v)) {
    Yap_Error(TYPE_ERROR_ATOM,v,"set_property/1");
    return(FALSE);
  }
  s = RepAtom(AtomOfTerm(ARG3))->StrOfAE;
  if (v == MkIntTerm(1)) {
    if (!strcmp(s, "quasi_quotation_syntax")) {
      pe->ExtraPredFlags |= QuasiQuotationPredFlag;
    } else if (!strcmp(s, "trace")) {
      // proc->ExtraPredFlags |= QuasiQuotationPredFlag;
    } else {
      fprintf( stderr, "not implemented");
      UNLOCK(pe->PELock);
      return FALSE;
    }
  } else if (v == MkIntTerm(0)) {
    if (!strcmp(s, "quasi_quotation_syntax")) {
      pe->ExtraPredFlags &= ~QuasiQuotationPredFlag;
    } else if (!strcmp(s, "trace")) {
      // proc->ExtraPredFlags |= QuasiQuotationPredFlag;
    } else {
      fprintf( stderr, "not implemented");
      UNLOCK(pe->PELock);
      return FALSE;
    }
  }
  
  UNLOCK(pe->PELock);
  return TRUE;
}

void
Yap_show_statistics(void)
{
  CACHE_REGS
  unsigned long int heap_space_taken;
  double frag;

#if USE_SYSTEM_MALLOC && HAVE_MALLINFO
  struct mallinfo mi = mallinfo();

  heap_space_taken = (mi.arena+mi.hblkhd)-Yap_HoleSize;
#else
  heap_space_taken = 
    (unsigned long int)(Unsigned(HeapTop)-Unsigned(Yap_HeapBase))-Yap_HoleSize;
#endif
  frag  = (100.0*(heap_space_taken-HeapUsed))/heap_space_taken;

  Sfprintf(GLOBAL_stderr, "Code Space:  %ld (%ld bytes needed, %ld bytes used, fragmentation %.3f%%).\n", 
	     (unsigned long int)(Unsigned (H0) - Unsigned (Yap_HeapBase)),
	     (unsigned long int)(Unsigned(HeapTop)-Unsigned(Yap_HeapBase)),
	     (unsigned long int)(HeapUsed),
	     frag);
  Sfprintf(GLOBAL_stderr, "Stack Space: %ld (%ld for Global, %ld for local).\n", 
	     (unsigned long int)(sizeof(CELL)*(LCL0-H0)),
	     (unsigned long int)(sizeof(CELL)*(HR-H0)),
	     (unsigned long int)(sizeof(CELL)*(LCL0-ASP)));
  Sfprintf(GLOBAL_stderr, "Trail Space: %ld (%ld used).\n", 
	     (unsigned long int)(sizeof(tr_fr_ptr)*(Unsigned(LOCAL_TrailTop)-Unsigned(LOCAL_TrailBase))),
	     (unsigned long int)(sizeof(tr_fr_ptr)*(Unsigned(TR)-Unsigned(LOCAL_TrailBase))));
  Sfprintf(GLOBAL_stderr, "Runtime: %lds.\n", (unsigned long int)(runtime ( PASS_REGS1 )));
  Sfprintf(GLOBAL_stderr, "Cputime: %lds.\n", (unsigned long int)(Yap_cputime ()));
  Sfprintf(GLOBAL_stderr, "Walltime: %lds.\n", (unsigned long int)(Yap_walltime ()));
}

static Int
p_statistics_heap_max( USES_REGS1 )
{
  Term tmax = MkIntegerTerm(HeapMax);

  return(Yap_unify(tmax, ARG1));
}

/* The results of the next routines are not to be trusted too */
/* much. Basically, any stack shifting will seriously confuse the */
/* results */

static Int    TrailTide = -1, LocalTide = -1, GlobalTide = -1;

/* maximum Trail usage */
static Int
TrailMax(void)
{
  CACHE_REGS
  Int i;
  Int TrWidth = Unsigned(LOCAL_TrailTop) - Unsigned(LOCAL_TrailBase);
  CELL *pt;

  if (TrailTide != TrWidth) {
    pt = (CELL *)TR;
    while (pt+2 < (CELL *)LOCAL_TrailTop) {
      if (pt[0] == 0 &&
	  pt[1] == 0 &&
	  pt[2] == 0)
	break;
      else
	pt++;
    }
    if (pt+2 < (CELL *)LOCAL_TrailTop)
      i = Unsigned(pt) - Unsigned(LOCAL_TrailBase);
    else
      i = TrWidth;
  } else
    return(TrWidth);
  if (TrailTide > i)
    i = TrailTide;
  else
    TrailTide = i;
  return(i);
}

static Int
p_statistics_trail_max( USES_REGS1 )
{
  Term tmax = MkIntegerTerm(TrailMax());

  return(Yap_unify(tmax, ARG1));
  
}

/* maximum Global usage */
static Int
GlobalMax(void)
{
  CACHE_REGS
  Int i;
  Int StkWidth = Unsigned(LCL0) - Unsigned(H0);
  CELL *pt;

  if (GlobalTide != StkWidth) {
    pt = HR;
    while (pt+2 < ASP) {
      if (pt[0] == 0 &&
	  pt[1] == 0 &&
	  pt[2] == 0)
	break;
      else
	pt++;
    }
    if (pt+2 < ASP)
      i = Unsigned(pt) - Unsigned(H0);
    else
      /* so that both Local and Global have reached maximum width */
      GlobalTide = LocalTide = i = StkWidth;
  } else
    return(StkWidth);
  if (GlobalTide > i)
    i = GlobalTide;
  else
    GlobalTide = i;
  return(i);
}

static Int 
p_statistics_global_max( USES_REGS1 )
{
  Term tmax = MkIntegerTerm(GlobalMax());

  return(Yap_unify(tmax, ARG1));
  
}


static Int
LocalMax(void)
{
  CACHE_REGS
  Int i;
  Int StkWidth = Unsigned(LCL0) - Unsigned(H0);
  CELL *pt;

  if (LocalTide != StkWidth) {
    pt = LCL0;
    while (pt-3 > HR) {
      if (pt[-1] == 0 &&
	  pt[-2] == 0 &&
	  pt[-3] == 0)
	break;
      else
	--pt;
    }
    if (pt-3 > HR)
      i = Unsigned(LCL0) - Unsigned(pt);
    else
      /* so that both Local and Global have reached maximum width */
      GlobalTide = LocalTide = i = StkWidth;
  } else
    return(StkWidth);
  if (LocalTide > i)
    i = LocalTide;
  else
    LocalTide = i;
  return(i);
}

static Int 
p_statistics_local_max( USES_REGS1 )
{
  Term tmax = MkIntegerTerm(LocalMax());

  return(Yap_unify(tmax, ARG1));
  
}



static Int 
p_statistics_heap_info( USES_REGS1 )
{
  Term tusage = MkIntegerTerm(HeapUsed);

#if USE_SYSTEM_MALLOC && HAVE_MALLINFO
  struct mallinfo mi = mallinfo();

  UInt sstack = Yap_HoleSize+(LOCAL_TrailTop-LOCAL_GlobalBase);
  UInt mmax = (mi.arena+mi.hblkhd);
  Term tmax = MkIntegerTerm(mmax-sstack);
  tusage = MkIntegerTerm(mmax-(mi.fordblks+sstack));
#else
  Term tmax = MkIntegerTerm((LOCAL_GlobalBase - Yap_HeapBase)-Yap_HoleSize);
#endif

  return(Yap_unify(tmax, ARG1) && Yap_unify(tusage,ARG2));
  
}


static Int 
p_statistics_stacks_info( USES_REGS1 )
{
  Term tmax = MkIntegerTerm(Unsigned(LCL0) - Unsigned(H0));
  Term tgusage = MkIntegerTerm(Unsigned(HR) - Unsigned(H0));
  Term tlusage = MkIntegerTerm(Unsigned(LCL0) - Unsigned(ASP));

  return(Yap_unify(tmax, ARG1) && Yap_unify(tgusage,ARG2) && Yap_unify(tlusage,ARG3));
  
}



static Int 
p_statistics_trail_info( USES_REGS1 )
{
  Term tmax = MkIntegerTerm(Unsigned(LOCAL_TrailTop) - Unsigned(LOCAL_TrailBase));
  Term tusage = MkIntegerTerm(Unsigned(TR) - Unsigned(LOCAL_TrailBase));

  return(Yap_unify(tmax, ARG1) && Yap_unify(tusage,ARG2));
  
}

static Int 
p_statistics_atom_info( USES_REGS1 )
{
  UInt count = 0, spaceused = 0, i;

  for (i =0; i < AtomHashTableSize; i++) {
    Atom catom;

    READ_LOCK(HashChain[i].AERWLock);
    catom = HashChain[i].Entry;
    if (catom != NIL) {
      READ_LOCK(RepAtom(catom)->ARWLock);
    }
    READ_UNLOCK(HashChain[i].AERWLock);
    while (catom != NIL) {
      Atom ncatom;
      count++;
      spaceused += sizeof(AtomEntry)+strlen(RepAtom(catom)->StrOfAE)+1;
      ncatom = RepAtom(catom)->NextOfAE;
      if (ncatom != NIL) {
	READ_LOCK(RepAtom(ncatom)->ARWLock);
      }
      READ_UNLOCK(RepAtom(catom)->ARWLock);
      catom = ncatom;
    }
  }
  for (i =0; i < WideAtomHashTableSize; i++) {
    Atom catom;

    READ_LOCK(WideHashChain[i].AERWLock);
    catom = WideHashChain[i].Entry;
    if (catom != NIL) {
      READ_LOCK(RepAtom(catom)->ARWLock);
    }
    READ_UNLOCK(WideHashChain[i].AERWLock);
    while (catom != NIL) {
      Atom ncatom;
      count++;
      spaceused += sizeof(AtomEntry)+sizeof(wchar_t)*(wcslen((wchar_t *)( RepAtom(catom)->StrOfAE)+1));
      ncatom = RepAtom(catom)->NextOfAE;
      if (ncatom != NIL) {
	READ_LOCK(RepAtom(ncatom)->ARWLock);
      }
      READ_UNLOCK(RepAtom(catom)->ARWLock);
      catom = ncatom;
    }
  }
  return Yap_unify(ARG1, MkIntegerTerm(count)) &&
    Yap_unify(ARG2, MkIntegerTerm(spaceused));
}


static Int 
p_statistics_db_size( USES_REGS1 )
{
  Term t = MkIntegerTerm(Yap_ClauseSpace);
  Term tit = MkIntegerTerm(Yap_IndexSpace_Tree);
  Term tis = MkIntegerTerm(Yap_IndexSpace_SW);
  Term tie = MkIntegerTerm(Yap_IndexSpace_EXT);

  return
    Yap_unify(t, ARG1) &&
    Yap_unify(tit, ARG2) &&
    Yap_unify(tis, ARG3) &&
    Yap_unify(tie, ARG4);
  
}

static Int 
p_statistics_lu_db_size( USES_REGS1 )
{
  Term t = MkIntegerTerm(Yap_LUClauseSpace);
  Term tit = MkIntegerTerm(Yap_LUIndexSpace_Tree);
  Term tic = MkIntegerTerm(Yap_LUIndexSpace_CP);
  Term tix = MkIntegerTerm(Yap_LUIndexSpace_EXT);
  Term tis = MkIntegerTerm(Yap_LUIndexSpace_SW);

  return
    Yap_unify(t, ARG1) &&
    Yap_unify(tit, ARG2) &&
    Yap_unify(tic, ARG3) &&
    Yap_unify(tis, ARG4) &&
    Yap_unify(tix, ARG5);
}



static Term
mk_argc_list( USES_REGS1 )
{
  int i =0;
  Term t = TermNil;
  while (i < GLOBAL_argc) {
    char *arg = GLOBAL_argv[i];
    /* check for -L -- */
    if (arg[0] == '-' && arg[1] == 'L') {
      arg += 2;
      while (*arg != '\0' && (*arg == ' ' || *arg == '\t'))
	arg++;
      if (*arg == '-' && arg[1] == '-' && arg[2] == '\0') {
	/* we found the separator */
	int j;
	for (j = GLOBAL_argc-1; j > i+1; --j) {
	  t = MkPairTerm(MkAtomTerm(Yap_LookupAtom(GLOBAL_argv[j])),t);
	}
	return t;
      } else if (GLOBAL_argv[i+1] && GLOBAL_argv[i+1][0] == '-' && GLOBAL_argv[i+1][1] == '-'  && GLOBAL_argv[i+1][2] == '\0') {
	/* we found the separator */
	int j;
	for (j = GLOBAL_argc-1; j > i+2; --j) {
	  t = MkPairTerm(MkAtomTerm(Yap_LookupAtom(GLOBAL_argv[j])),t);
	}
	return t;
      }
    }
    if (arg[0] == '-' && arg[1] == '-' && arg[2] == '\0') {
      /* we found the separator */
      int j;
      for (j = GLOBAL_argc-1; j > i; --j) {
	t = MkPairTerm(MkAtomTerm(Yap_LookupAtom(GLOBAL_argv[j])),t);
      }
      return(t);
    }
    i++;
  } 
  return(t);
}

static Term
mk_os_argc_list( USES_REGS1 )
{
  int i =0;
  Term t = TermNil;
  for (i = 0 ; i < GLOBAL_argc; i++) {
    char *arg = GLOBAL_argv[i];
    t = MkPairTerm(MkAtomTerm(Yap_LookupAtom(arg)),t);
  } 
  return(t);
}

static Int 
p_argv( USES_REGS1 )
{
  Term t = mk_argc_list( PASS_REGS1 );
  return Yap_unify(t, ARG1);
}

static Int 
p_os_argv( USES_REGS1 )
{
  Term t = mk_os_argc_list( PASS_REGS1 );
  return Yap_unify(t, ARG1);
}

static Int 
p_executable( USES_REGS1 )
{
  if (GLOBAL_argv && GLOBAL_argv[0])
    Yap_TrueFileName (GLOBAL_argv[0], LOCAL_FileNameBuf, FALSE);
  else
    strncpy(LOCAL_FileNameBuf, Yap_FindExecutable(), YAP_FILENAME_MAX-1) ;

  return Yap_unify(MkAtomTerm(Yap_LookupAtom(LOCAL_FileNameBuf)),ARG1);
}

static Int
p_access_yap_flags( USES_REGS1 )
{
  Term tflag = Deref(ARG1);
  Int flag;
  Term tout = 0;

  if (IsVarTerm(tflag)) {
    Yap_Error(INSTANTIATION_ERROR, tflag, "access_yap_flags/2");
    return(FALSE);		
  }
  if (!IsIntTerm(tflag)) {
    Yap_Error(TYPE_ERROR_INTEGER, tflag, "access_yap_flags/2");
    return(FALSE);		
  }
  flag = IntOfTerm(tflag);
  if (flag < 0 || flag >= NUMBER_OF_YAP_FLAGS) {
    return(FALSE);
  }
  if (flag == TABLING_MODE_FLAG) {
#ifdef TABLING
    tout = TermNil;
    if (IsMode_LocalTrie(yap_flags[flag]))
      tout = MkPairTerm(MkAtomTerm(AtomLocalTrie), tout);
    else if (IsMode_GlobalTrie(yap_flags[flag]))
      tout = MkPairTerm(MkAtomTerm(AtomGlobalTrie), tout);
    if (IsMode_LoadAnswers(yap_flags[flag]))
      tout = MkPairTerm(MkAtomTerm(AtomLoadAnswers), tout);
    else if (IsMode_ExecAnswers(yap_flags[flag]))
      tout = MkPairTerm(MkAtomTerm(AtomExecAnswers), tout);
    if (IsMode_Local(yap_flags[flag]))
      tout = MkPairTerm(MkAtomTerm(AtomLocal), tout);
    else if (IsMode_Batched(yap_flags[flag]))
      tout = MkPairTerm(MkAtomTerm(AtomBatched), tout);
    else if (IsMode_CoInductive(yap_flags[flag]))
      tout = MkPairTerm(MkAtomTerm(AtomCoInductive), tout);
#else
    tout = MkAtomTerm(AtomFalse);
#endif /* TABLING */
  } else
  tout = MkIntegerTerm(yap_flags[flag]);
  return(Yap_unify(ARG2, tout));
}

static Int 
p_has_yap_or( USES_REGS1 )
{
#ifdef YAPOR
  return(TRUE);
#else
  return(FALSE);
#endif
}

static Int 
p_has_eam( USES_REGS1 )
{
#ifdef BEAM
  return(TRUE);
#else
  return(FALSE);
#endif
}


static Int
p_set_yap_flags( USES_REGS1 )
{
  Term tflag = Deref(ARG1);
  Term tvalue = Deref(ARG2);
  Int flag, value;

  if (IsVarTerm(tflag)) {
    Yap_Error(INSTANTIATION_ERROR, tflag, "set_yap_flags/2");
    return(FALSE);		
  }
  if (!IsIntTerm(tflag)) {
    Yap_Error(TYPE_ERROR_INTEGER, tflag, "set_yap_flags/2");
    return(FALSE);		
  }
  flag = IntOfTerm(tflag);
  if (IsVarTerm(tvalue)) {
    Yap_Error(INSTANTIATION_ERROR, tvalue, "set_yap_flags/2");
    return(FALSE);		
  }
  if (!IsIntTerm(tvalue)) {
    Yap_Error(TYPE_ERROR_INTEGER, tvalue, "set_yap_flags/2");
    return(FALSE);		
  }
  value = IntOfTerm(tvalue);
  /* checking should have been performed */
  switch(flag) {
  case LANGUAGE_MODE_FLAG:
    if (value < 0 || value > 2)
      return(FALSE);
    if (value == 1) {
      Yap_heap_regs->pred_meta_call = RepPredProp(PredPropByFunc(FunctorMetaCall,0));
    } else {
      Yap_heap_regs->pred_meta_call = RepPredProp(PredPropByFunc(FunctorMetaCall,0));
    }
    yap_flags[LANGUAGE_MODE_FLAG] = value;
    break;
  case SOURCE_MODE_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[SOURCE_MODE_FLAG] = value;
    break;
  case FLOATING_POINT_EXCEPTION_MODE_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[FLOATING_POINT_EXCEPTION_MODE_FLAG] = value;
    break;
  case WRITE_QUOTED_STRING_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[WRITE_QUOTED_STRING_FLAG] = value;
    break;
  case ALLOW_ASSERTING_STATIC_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[ALLOW_ASSERTING_STATIC_FLAG] = value;
    break;
  case STACK_DUMP_ON_ERROR_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[STACK_DUMP_ON_ERROR_FLAG] = value;
    break;
  case INDEXING_MODE_FLAG:
    if (value < INDEX_MODE_OFF || value >  INDEX_MODE_MAX)
      return(FALSE);
    yap_flags[INDEXING_MODE_FLAG] = value;
    break;
#ifdef TABLING
  case TABLING_MODE_FLAG:
    if (value == 0) {  /* default */
      tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
      while(tab_ent) {
	TabEnt_mode(tab_ent) = TabEnt_flags(tab_ent);
	tab_ent = TabEnt_next(tab_ent);
      }
      yap_flags[TABLING_MODE_FLAG] = 0;
    } else if (value == 1) {  /* batched */
      tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
      while(tab_ent) {
	SetMode_Batched(TabEnt_mode(tab_ent));
	tab_ent = TabEnt_next(tab_ent);
      }
      SetMode_Batched(yap_flags[TABLING_MODE_FLAG]);
    } else if (value == 2) {  /* local */
      tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
      while(tab_ent) {
	SetMode_Local(TabEnt_mode(tab_ent));
	tab_ent = TabEnt_next(tab_ent);
      }
      SetMode_Local(yap_flags[TABLING_MODE_FLAG]);
    } else if (value == 3) {  /* exec_answers */
      tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
      while(tab_ent) {
	SetMode_ExecAnswers(TabEnt_mode(tab_ent));
	tab_ent = TabEnt_next(tab_ent);
      }
      SetMode_ExecAnswers(yap_flags[TABLING_MODE_FLAG]);
    } else if (value == 4) {  /* load_answers */
      tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
      while(tab_ent) {
	SetMode_LoadAnswers(TabEnt_mode(tab_ent));
	tab_ent = TabEnt_next(tab_ent);
      }
      SetMode_LoadAnswers(yap_flags[TABLING_MODE_FLAG]);
    } else if (value == 5) {  /* local_trie */
      tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
      while(tab_ent) {
	SetMode_LocalTrie(TabEnt_mode(tab_ent));
	tab_ent = TabEnt_next(tab_ent);
      }
      SetMode_LocalTrie(yap_flags[TABLING_MODE_FLAG]);
    } else if (value == 6) {  /* global_trie */
      tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
      while(tab_ent) {
	SetMode_GlobalTrie(TabEnt_mode(tab_ent));
	tab_ent = TabEnt_next(tab_ent);
      }
      SetMode_GlobalTrie(yap_flags[TABLING_MODE_FLAG]);
    } else if (value == 7) {  /* CoInductive */
      tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
      while(tab_ent) {
        SetMode_CoInductive(TabEnt_mode(tab_ent));
        tab_ent = TabEnt_next(tab_ent);
      }
      SetMode_CoInductive(yap_flags[TABLING_MODE_FLAG]);
    } 
    break;
#endif /* TABLING */
  case VARS_CAN_HAVE_QUOTE_FLAG:
    if (value != 0  && value != 1)
      return(FALSE);
    yap_flags[VARS_CAN_HAVE_QUOTE_FLAG] = value;
    break;
  case QUIET_MODE_FLAG:
    if (value != 0  && value != 1)
      return FALSE;
    yap_flags[QUIET_MODE_FLAG] = value;
    break;
  default:
    return(FALSE);
  }
  return(TRUE);
}

static Int
p_system_mode( USES_REGS1 )
{
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    if (LOCAL_PrologMode & SystemMode)
      return Yap_unify( t1, MkAtomTerm(AtomTrue));
    else
      return Yap_unify( t1, MkAtomTerm(AtomFalse));
  } else {
    Atom at = AtomOfTerm(t1);
    if (at == AtomFalse) 
      LOCAL_PrologMode &= ~SystemMode;
    else
      LOCAL_PrologMode |= SystemMode;
  }
  return TRUE;
}

static Int
p_lock_system( USES_REGS1 )
{
  LOCK(GLOBAL_BGL);
  return TRUE;
}

static Int
p_unlock_system( USES_REGS1 )
{
  UNLOCK(GLOBAL_BGL);
  return TRUE;
}

static Int
p_enterundefp( USES_REGS1 )
{
  if (LOCAL_DoingUndefp) {
    return FALSE;
  }
  LOCAL_DoingUndefp = TRUE;
  return TRUE;
}

static Int
p_exitundefp( USES_REGS1 )
{
  if (LOCAL_DoingUndefp) {
    LOCAL_DoingUndefp = FALSE;
    return TRUE;
  }
  return FALSE;
}

#ifdef DEBUG
extern void DumpActiveGoals(void);

static Int
p_dump_active_goals( USES_REGS1 ) {
  DumpActiveGoals();
  return(TRUE);
}
#endif

#ifdef INES
static Int
p_euc_dist( USES_REGS1 ) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  double d1 = (double)(IntegerOfTerm(ArgOfTerm(1,t1))-IntegerOfTerm(ArgOfTerm(1,t2)));
  double d2 = (double)(IntegerOfTerm(ArgOfTerm(2,t1))-IntegerOfTerm(ArgOfTerm(2,t2)));
  double d3 = (double)(IntegerOfTerm(ArgOfTerm(3,t1))-IntegerOfTerm(ArgOfTerm(3,t2)));
  Int result = (Int)sqrt(d1*d1+d2*d2+d3*d3);
  return(Yap_unify(ARG3,MkIntegerTerm(result)));
}

volatile int loop_counter = 0;

static Int
p_loop( USES_REGS1 ) {
  while (loop_counter == 0);
  return(TRUE);
}
#endif


static Int
p_break( USES_REGS1 ) {
  Atom at = AtomOfTerm(Deref( ARG1 ));
  if (at == AtomTrue) {
    LOCAL_PL_local_data_p->break_level++;
    return TRUE;
  }
  if (at == AtomFalse) {
    LOCAL_PL_local_data_p->break_level--;
    return TRUE;
  }
  return FALSE;
}

void 
Yap_InitBackCPreds(void)
{
  Yap_InitCPredBack("$current_predicate", 3, 1, init_current_predicate, cont_current_predicate,
		SafePredFlag|SyncPredFlag);
  Yap_InitCPredBack("$current_predicate_for_atom", 3, 1, init_current_predicate_for_atom, cont_current_predicate_for_atom,
		SafePredFlag|SyncPredFlag);
  Yap_InitCPredBack("$current_op", 5, 1, init_current_op, cont_current_op,
		SafePredFlag|SyncPredFlag);
  Yap_InitCPredBack("$current_atom_op", 5, 1, init_current_atom_op, cont_current_atom_op,
		SafePredFlag|SyncPredFlag);
#ifdef BEAM
  Yap_InitCPredBack("eam", 1, 0, start_eam, cont_eam,
		SafePredFlag);
#endif

  Yap_InitBackAtoms();
  Yap_InitBackIO();
  Yap_InitBackDB();
  Yap_InitUserBacks();
}

typedef void (*Proc)(void);

Proc E_Modules[]= {/* init_fc,*/ (Proc) 0 };

#ifndef YAPOR
static Int p_parallel_mode( USES_REGS1 ) {
  return FALSE;
}

static Int p_yapor_workers( USES_REGS1 ) {
  return FALSE;
}
#endif /* YAPOR */


void 
Yap_InitCPreds(void)
{
  /* numerical comparison */
  Yap_InitCPred("set_value", 2, p_setval, SafePredFlag|SyncPredFlag);
/** @pred  set_value(+ _A_,+ _C_) 


Associate atom  _A_ with constant  _C_.

The `set_value` and `get_value` built-ins give a fast alternative to
the internal data-base. This is a simple form of implementing a global
counter.

~~~~~
       read_and_increment_counter(Value) :-
                get_value(counter, Value),
                Value1 is Value+1,
                set_value(counter, Value1).
~~~~~
This predicate is YAP specific.




 
*/
  Yap_InitCPred("get_value", 2, p_value, TestPredFlag|SafePredFlag|SyncPredFlag);
/** @pred  get_value(+ _A_,- _V_) 


In YAP, atoms can be associated with constants. If one such
association exists for atom  _A_, unify the second argument with the
constant. Otherwise, unify  _V_ with `[]`.

This predicate is YAP specific.

 
*/
  Yap_InitCPred("$values", 3, p_values, SafePredFlag|SyncPredFlag);
  /* general purpose */
  Yap_InitCPred("$opdec", 4, p_opdec, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("=..", 2, p_univ, 0);
/** @pred  _T_ =..  _L_ is iso 


The list  _L_ is built with the functor and arguments of the term
 _T_. If  _T_ is instantiated to a variable, then  _L_ must be
instantiated either to a list whose head is an atom, or to a list
consisting of just a number.

 
*/
  Yap_InitCPred("$statistics_trail_max", 1, p_statistics_trail_max, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$statistics_heap_max", 1, p_statistics_heap_max, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$statistics_global_max", 1, p_statistics_global_max, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$statistics_local_max", 1, p_statistics_local_max, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$statistics_heap_info", 2, p_statistics_heap_info, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$statistics_stacks_info", 3, p_statistics_stacks_info, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$statistics_trail_info", 2, p_statistics_trail_info, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$statistics_atom_info", 2, p_statistics_atom_info, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$statistics_db_size", 4, p_statistics_db_size, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$statistics_lu_db_size", 5, p_statistics_lu_db_size, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$argv", 1, p_argv, SafePredFlag);
  Yap_InitCPred("$os_argv", 1, p_os_argv, SafePredFlag);
  Yap_InitCPred("$executable", 1, p_executable, SafePredFlag);
  Yap_InitCPred("$runtime", 2, p_runtime, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$cputime", 2, p_cputime, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$systime", 2, p_systime, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$walltime", 2, p_walltime, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$access_yap_flags", 2, p_access_yap_flags, SafePredFlag);
  Yap_InitCPred("$set_yap_flags", 2, p_set_yap_flags, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$system_mode", 1, p_system_mode, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("abort", 0, p_abort, SyncPredFlag);
/** @pred  abort 


Abandons the execution of the current goal and returns to top level. All
break levels (see break/0 below) are terminated. It is mainly
used during debugging or after a serious execution error, to return to
the top-level.

 
*/
  Yap_InitCPred("$break", 1, p_break, SafePredFlag);
#ifdef BEAM
  Yap_InitCPred("@", 0, eager_split, SafePredFlag);
  Yap_InitCPred(":", 0, force_wait, SafePredFlag);
  Yap_InitCPred("/", 0, commit, SafePredFlag);
  Yap_InitCPred("skip_while_var",1,skip_while_var,SafePredFlag);
  Yap_InitCPred("wait_while_var",1,wait_while_var,SafePredFlag);
  Yap_InitCPred("eamtime", 0, show_time, SafePredFlag);
  Yap_InitCPred("eam", 0, use_eam, SafePredFlag);
#endif
  Yap_InitCPred("$halt", 1, p_halt, SyncPredFlag);
  Yap_InitCPred("$lock_system", 0, p_lock_system, SafePredFlag);
  Yap_InitCPred("$unlock_system", 0, p_unlock_system, SafePredFlag);
  Yap_InitCPred("$enter_undefp", 0, p_enterundefp, SafePredFlag);
  Yap_InitCPred("$exit_undefp", 0, p_exitundefp, SafePredFlag);
  /* Accessing and changing the flags for a predicate */
  Yap_InitCPred("$flags", 4, p_flags, SyncPredFlag);
  Yap_InitCPred("$set_flag", 4, p_set_flag, SyncPredFlag);
  Yap_InitCPred("$has_yap_or", 0, p_has_yap_or, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$has_eam", 0, p_has_eam, SafePredFlag|SyncPredFlag);
#ifndef YAPOR
  Yap_InitCPred("parallel_mode", 1, p_parallel_mode, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_yapor_workers", 1, p_yapor_workers, SafePredFlag|SyncPredFlag);
#endif /* YAPOR */
#ifdef INES
  Yap_InitCPred("euc_dist", 3, p_euc_dist, SafePredFlag);
  Yap_InitCPred("loop", 0, p_loop, SafePredFlag);
#endif
#if QSAR
  Yap_InitCPred("in_range", 8, p_in_range, TestPredFlag|SafePredFlag);
  Yap_InitCPred("in_range", 4, p_in_range2, TestPredFlag|SafePredFlag);
#endif
#ifdef DEBUG
  Yap_InitCPred("dump_active_goals", 0, p_dump_active_goals, SafePredFlag|SyncPredFlag);
#endif

  Yap_InitArrayPreds();
  Yap_InitAtomPreds();
  Yap_InitBBPreds();
  Yap_InitBigNums();
  Yap_InitCdMgr();
  Yap_InitCmpPreds();
  Yap_InitCoroutPreds();
  Yap_InitDBPreds();
  Yap_InitExecFs();
  Yap_InitGlobals();
  Yap_InitInlines();
  Yap_InitIOPreds();
  Yap_InitExoPreds();
  Yap_InitLoadForeign();
  Yap_InitModulesC();
  Yap_InitSavePreds();
  Yap_InitRange();
  Yap_InitSysPreds();
  Yap_InitUnify();
  Yap_InitQLY();
  Yap_InitQLYR();
  Yap_udi_init();
  Yap_udi_Interval_init();
  Yap_InitSignalCPreds();
  Yap_InitUserCPreds();
  Yap_InitUtilCPreds();
  Yap_InitSortPreds();
  Yap_InitMaVarCPreds();
#ifdef DEPTH_LIMIT
  Yap_InitItDeepenPreds();
#endif
#ifdef ANALYST
  Yap_InitAnalystPreds();
#endif
#ifdef LOW_LEVEL_TRACER
  Yap_InitLowLevelTrace();
#endif
  Yap_InitEval();
  Yap_InitGrowPreds();
  Yap_InitLowProf();
#if defined(YAPOR) || defined(TABLING)
  Yap_init_optyap_preds();
#endif /* YAPOR || TABLING */
  Yap_InitThreadPreds();
  {
    void            (*(*(p))) (void) = E_Modules;
    while (*p)
      (*(*p++)) ();
  }
#if CAMACHO
  {
    extern void InitForeignPreds(void);
  
    Yap_InitForeignPreds();
  }
#endif
#if APRIL
  {
    extern void init_ol(void), init_time(void);
  
    init_ol();
    init_time();
  }
#endif
#if SUPPORT_CONDOR
  init_sys();
  init_random();
  //  init_tries();
  init_regexp();
#endif
  {
    CACHE_REGS
    Term cm = CurrentModule;
    CurrentModule = SWI_MODULE;
    Yap_swi_install();
    CurrentModule = cm;
  }
}
