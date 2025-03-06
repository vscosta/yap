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
* Last rev:     $Date: 2008-07-24 16:02:00 $,$Author: vsc $
*	 *
*									 *
*************************************************************************/

#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#define HAS_CACHE_REGS 1
/*
 * @file stdpreds.c
 *
* This file includes the definition of a miscellania of standard predicates
* for yap refering to: Consulting, Executing a C predicate from call,
* Comparisons (both general and numeric), Structure manipulation, Direct
* access to atoms and predicates, Basic support for the debugger
*
* It also includes a table where all C-predicates are initializated
*
*/

#include "Yap.h"
#if YAP_JIT
#include "amijit.h"
#endif
#include "Foreign.h"
#include "YapHeap.h"
#include "Yatom.h"
#include "YapEval.h"
#include "yapio.h"
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_MALLOC_H
#include <malloc.h>
#endif
#if YAP_JIT
#include <JIT_Compiler.hpp>
#endif
#include <fcntl.h>
#include <wchar.h>

extern int init_tries(void);


static Int p_setval(USES_REGS1);
static Int p_value(USES_REGS1);
static Int p_values(USES_REGS1);
#ifdef undefined
static CODEADDR *FindAtom(CODEADDR, int *);
#endif /* undefined */
static Int p_univ(USES_REGS1);
#ifdef BEAM
Int p_halt(USES_REGS1);
#else
static Int p_halt(USES_REGS1);
#endif
static OpEntry *NextOp(Prop CACHE_TYPE);
static Int init_current_op(USES_REGS1);
static Int cont_current_op(USES_REGS1);
static Int init_current_atom_op(USES_REGS1);
static Int cont_current_atom_op(USES_REGS1);
static Int TrailMax(void);
static Int GlobalMax(void);
static Int LocalMax(void);
static Int p_statistics_heap_max(USES_REGS1);
static Int p_statistics_global_max(USES_REGS1);
static Int p_statistics_local_max(USES_REGS1);
static Int p_statistics_heap_info(USES_REGS1);
static Int p_statistics_stacks_info(USES_REGS1);
static Int p_statistics_trail_info(USES_REGS1);
static Int p_cputime(USES_REGS1);
static Int p_systime(USES_REGS1);
static Int p_runtime(USES_REGS1);
static Int p_walltime(USES_REGS1);
static Int p_break(USES_REGS1);

#if YAP_JIT
void *(*Yap_JitCall)(JIT_Compiler *jc, yamop *p);
void (*Yap_llvmShutdown)(void);
Int (*Yap_traced_absmi)(void);

static Int p_jit(USES_REGS1) { /* '$set_value'(+Atom,+Atomic) */
  void *jit_handle;

  if ((jit_handle = Yap_LoadForeignFile(YAP_YAPJITLIB, 0))) {
    if (!Yap_CallForeignFile(jit_handle, "init_jit"))
      fprintf(stderr, "Could not load JIT\n");
    return FALSE;
  }
  return TRUE;
}

#endif /* YAP_JIT */

#ifdef BEAM
Int use_eam(USES_REGS1);
Int eager_split(USES_REGS1);
Int force_wait(USES_REGS1);
Int commit(USES_REGS1);
Int skip_while_var(USES_REGS1);
Int wait_while_var(USES_REGS1);
Int show_time(USES_REGS1);
Int start_eam(USES_REGS1);
Int cont_eam(USES_REGS1);

extern int EAM;
extern int eam_am(PredEntry *);
extern int showTime(void);

Int start_eam(USES_REGS1) {
  if (eam_am((PredEntry *)0x1))
    return (TRUE);
  else {
    cut_fail();
    return (FALSE);
  }
}

Int cont_eam(USES_REGS1) {
  if (eam_am((PredEntry *)0x2))
    return (TRUE);
  else {
    cut_fail();
    return (FALSE);
  }
}

Int use_eam(USES_REGS1) {
  if (EAM)
    EAM = 0;
  else {
    Yap_PutValue(AtomCArith, 0);
    EAM = 1;
  }
  return (TRUE);
}

Int commit(USES_REGS1) {
  if (EAM) {
    printf("Nao deveria ter sido chamado commit do stdpreds\n");
    exit(1);
  }
  return (TRUE);
}

Int skip_while_var(USES_REGS1) {
  if (EAM) {
    printf("Nao deveria ter sido chamado skip_while_var do stdpreds\n");
    exit(1);
  }
  return (TRUE);
}

Int wait_while_var(USES_REGS1) {
  if (EAM) {
    printf("Nao deveria ter sido chamado wait_while_var do stdpreds\n");
    exit(1);
  }
  return (TRUE);
}

Int force_wait(USES_REGS1) {
  if (EAM) {
    printf("Nao deveria ter sido chamado force_wait do stdpreds\n");
    exit(1);
  }
  return (TRUE);
}

Int eager_split(USES_REGS1) {
  if (EAM) {
    printf("Nao deveria ter sido chamado eager_split do stdpreds\n");
    exit(1);
  }
  return (TRUE);
}

Int show_time(USES_REGS1) /* MORE PRECISION */
{
  return (showTime());
}

#endif /* BEAM */

/**
   @defgroup YAPSetVal  Atom to Atomic Family of Built-ins.
   @ingroup Internal_Database
   @{

   Maintain a light-weight map where the key is an atom, and the value can be
   any constant.
*/

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
static Int p_setval(USES_REGS1) { /* '$set_value'(+Atom,+Atomic) */
  Term t1 = Deref(ARG1), t2 = Deref(ARG2);
  if (!IsVarTerm(t1) && IsAtomTerm(t1) &&
      (!IsVarTerm(t2) && (IsAtomTerm(t2) || IsNumTerm(t2)))) {
    Yap_PutValue(AtomOfTerm(t1), t2);
    return (TRUE);
  }
  return (FALSE);
}

/** @pred  get_value(+ _A_,- _V_)
    In YAP, atoms can be associated with constants. If one such
    association exists for atom  _A_, unify the second argument with the
    constant. Otherwise, unify  _V_ with `[]`.

    This predicate is YAP specific.
*/
static Int p_value(USES_REGS1) { /* '$get_value'(+Atom,?Val) */
  Term t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "get_value/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t1, "get_value/2");
    return (FALSE);
  }
  return (Yap_unify_constant(ARG2, Yap_GetValue(AtomOfTerm(t1))));
}

static Int p_values(USES_REGS1) { /* '$values'(Atom,Old,New) */
  Term t1 = Deref(ARG1), t3 = Deref(ARG3);

  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "set_value/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t1, "set_value/2");
    return (FALSE);
  }
  if (!Yap_unify_constant(ARG2, Yap_GetValue(AtomOfTerm(t1)))) {
    return (FALSE);
  }
  if (!IsVarTerm(t3)) {
    if (IsAtomTerm(t3) || IsNumTerm(t3)) {
      Yap_PutValue(AtomOfTerm(t1), t3);
    } else
      return (FALSE);
  }
  return (TRUE);
}

///@}

static Int opdec(USES_REGS1) { /* '$opdec'(p,type,atom)		 */
  /* we know the arguments are integer, atom, atom */
  Term p = Deref(ARG1), t = Deref(ARG2), at = Deref(ARG3);
  Term tmod = Deref(ARG4);
  if (tmod == TermProlog) {
    tmod = PROLOG_MODULE;
  }
  return Yap_OpDec((int)IntOfTerm(p), (char *)RepAtom(AtomOfTerm(t))->StrOfAE,
                   AtomOfTerm(at), tmod);
}

#ifdef NO_STRTOD

#if HAVE_CTYPE_H
#include <ctype.h>
#endif

double strtod(s, pe) char *s, **pe;
{
  double r = atof(s);
  *pe = s;
  while (*s == ' ') {
    ++s;
  }
  if (*s == '+' || *s == '-') {
    ++s;
  }
  if (!isdigit(*s)) {
    return (r);
  }
  while (isdigit(*s)) {
    ++s;
  }
  if (*s == '.') {
    ++s;
  }
  while (isdigit(*s)) {
    ++s;
  }
  if (*s == 'e' || *s == 'E') {
    ++s;
  }
  if (*s == '+' || *s == '-') {
    ++s;
  }
  while (isdigit(*s)) {
    ++s;
  }
  *pe = s;
  return (r);
}

#else

#include <stdlib.h>

#endif

#ifndef INFINITY
#define INFINITY (1.0 / 0.0)
#endif

static UInt runtime(USES_REGS1) {
  return (Yap_cputime() - Yap_total_gc_time() - Yap_total_stack_shift_time());
}

/* $runtime(-SinceInterval,-SinceStart)	 */
static Int p_runtime(USES_REGS1) {
  uint64_t now, interval, gc_time, ss_time;
  Term tnow, tinterval;

  Yap_cputime_interval(&now, &interval);
  gc_time = Yap_total_gc_time();
  now -= gc_time;
  ss_time = Yap_total_stack_shift_time();
  now -= ss_time;
  interval -= (gc_time - LOCAL_LastGcTime) + (ss_time - LOCAL_LastSSTime);
  LOCAL_LastGcTime = gc_time;
  LOCAL_LastSSTime = ss_time;
  tnow = MkIntegerTerm(now/1000000);
  tinterval = MkIntegerTerm(interval/1000000);
  return (Yap_unify_constant(ARG1, tnow) &&
          Yap_unify_constant(ARG2, tinterval));
}

/* $cputime(-SinceInterval,-SinceStart)	 */
static Int p_cputime(USES_REGS1) {
  uint64_t now, interval;
  Yap_cputime_interval(&now, &interval);
  return (Yap_unify_constant(ARG1, MkIntegerTerm(now/1000000)) &&
          Yap_unify_constant(ARG2, MkIntegerTerm(interval/1000000)));
}

static Int p_systime(USES_REGS1) {
  uint64_t now, interval;
  Yap_systime_interval(&now, &interval);
  return (Yap_unify_constant(ARG1, MkIntegerTerm(now/1000000)) &&
          Yap_unify_constant(ARG2, MkIntegerTerm(interval/1000000)));
}

static Int p_walltime(USES_REGS1) {
  uint64_t now, interval;
  uint64_t t = Yap_walltime();
  now = t - Yap_StartOfWTimes;
  interval = t - LOCAL_LastWTime;
  return (Yap_unify_constant(ARG1, MkIntegerTerm(now / 1000000)) &&
          Yap_unify_constant(ARG2, MkIntegerTerm(interval / 1000000)));
}

//  static int cnt;

  /** @infixpred  _T_ =..  _L_ is iso


  The list  _L_ is built with the functor and arguments of the term
  _T_. If  _T_ is instantiated to a variable, then  _L_ must be
  instantiated either to a list whose head is an atom, or to a list
  consisting of just a number.


  */

static Int p_univ(USES_REGS1) { /* A =.. L			 */
  unsigned int arity;
  register Term tin;
  Term twork, t2;
  Atom at;

  //cnt++;
  //if (cnt==5312800) jmp_deb(1);
  
  tin = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(tin)) {
    /* we need to have a list */
    Term *Ar;
    if (IsVarTerm(t2)) {
      Yap_ThrowError(INSTANTIATION_ERROR, t2, "(=..)/2");
      return (FALSE);
    }
    if (!IsPairTerm(t2)) {
      if (t2 == TermNil)
        Yap_ThrowError(DOMAIN_ERROR_NON_EMPTY_LIST, t2, "(=..)/2");
      else
        Yap_ThrowError(TYPE_ERROR_LIST, ARG2, "(=..)/2");
      return (FALSE);
    }
    twork = HeadOfTerm(t2);
    if (IsVarTerm(twork)) {
      Yap_ThrowError(INSTANTIATION_ERROR, twork, "(=..)/2");
      return (FALSE);
    }
    if (IsNumTerm(twork)) {
      Term tt = TailOfTerm(t2);
      if (IsVarTerm(tt)) {
        Yap_ThrowError(INSTANTIATION_ERROR, tt, "(=..)/2");
        return (FALSE);
      }
      if (tt != MkAtomTerm(AtomNil)) {
        Yap_ThrowError(TYPE_ERROR_ATOMIC, twork, "(=..)/2");
        return (FALSE);
      }
      return (Yap_unify_constant(ARG1, twork));
    }
    if (!IsAtomTerm(twork)) {
      Term tt = TailOfTerm(t2);
      if (IsVarTerm(tt)) {
        Yap_ThrowError(INSTANTIATION_ERROR, twork, "(=..)/2");
        return (FALSE);
      } else if (tt == MkAtomTerm(AtomNil)) {
        Yap_ThrowError(TYPE_ERROR_ATOMIC, twork, "(=..)/2");
        return (FALSE);
      } else {
        Yap_ThrowError(TYPE_ERROR_ATOM, twork, "(=..)/2");
        return (FALSE);
      }
    }
    at = AtomOfTerm(twork);
    twork = TailOfTerm(t2);
    if (IsVarTerm(twork)) {
      Yap_ThrowError(INSTANTIATION_ERROR, twork, "(=..)/2");
      return (FALSE);
    } else if (!IsPairTerm(twork)) {
      if (twork != TermNil) {
        Yap_ThrowError(TYPE_ERROR_LIST, ARG2, "(=..)/2");
        return (FALSE);
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
        if (!Yap_dogc(PASS_REGS1)) {
          Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
        twork = TailOfTerm(Deref(ARG2));
        goto build_compound;
      }
      twork = TailOfTerm(twork);
    }
    if (IsVarTerm(twork)) {
      Yap_ThrowError(INSTANTIATION_ERROR, twork, "(=..)/2");
      return (FALSE);
    }
    if (twork != TermNil) {
      Yap_ThrowError(TYPE_ERROR_LIST, ARG2, "(=..)/2");
      return (FALSE);
    }
#ifdef SFUNC
    DOES_NOT_WORK();
    {
      SFEntry *pe = (SFEntry *)Yap_GetAProp(at, SFProperty);
      if (pe)
        twork = MkSFTerm(Yap_MkFunctor(at, SFArity), arity, CellPtr(TR),
                         pe->NilValue);
      else
        twork = Yap_MkApplTerm(Yap_MkFunctor(at, arity), arity, CellPtr(TR));
    }
#else
    arity = HR - Ar - 1;
    if (at == AtomDot && arity == 2) {
      Ar[0] = Ar[1];
      Ar[1] = Ar[2];
      HR--;
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
    Functor fun = FunctorOfTerm(tin);
    if (IsExtensionFunctor(fun)) {
      twork = MkPairTerm(tin, MkAtomTerm(AtomNil));
      return (Yap_unify(twork, ARG2));
    }
    arity = ArityOfFunctor(fun);
    at = NameOfFunctor(fun);
#ifdef SFUNC
    if (arity == SFArity) {
      CELL *p = CellPtr(TR);
      CELL *q = ArgsOfSFTerm(tin);
      int argno = 1;
      while (*q) {
        while (*q > argno++)
          *p++ = MkVarTerm();
        ++q;
        *p++ = Deref(*q++);
      }
      twork = Yap_ArrayToList(CellPtr(TR), argno - 1);
      while (IsIntTerm(twork)) {
        if (!Yap_gc(2, ENV, gc_P(P, CP))) {
          Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return (FALSE);
        }
        twork = Yap_ArrayToList(CellPtr(TR), argno - 1);
      }
    } else
#endif
    {
      while (HR + arity * 2 > ASP - 1024) {
        if (!Yap_dogc(PASS_REGS1)) {
          Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return (FALSE);
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

#ifdef BEAM
extern void exit_eam(char *s);

Int
#else
static Int
#endif
    p_halt(USES_REGS1) { /* halt				 */
  Term t = Deref(ARG1);
  Int out;

#ifdef BEAM
  if (EAM)
    exit_eam("\n\n[ Prolog execution halted ]\n");
#endif

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "halt/1");
    return (FALSE);
  }
  if (!IsIntegerTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, t, "halt/1");
    return (FALSE);
  }
  out = IntegerOfTerm(t);
#if YAP_JIT
  if (ExpEnv.analysis_struc.stats_enabled ||
      ExpEnv.analysis_struc.time_pass_enabled) {
    if (strcmp(((char *)ExpEnv.analysis_struc.outfile), "STDERR")) {
      int stderrcopy = dup(2);
      if (strcmp(((char *)ExpEnv.analysis_struc.outfile), "STDOUT") == 0) {
        dup2(1, 2);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"
        shutdown_llvm();
#pragma GCC diagnostic pop
        dup2(stderrcopy, 2);
      } else {
        int Outputfile = open(((char *)ExpEnv.analysis_struc.outfile),
                              O_CREAT | O_APPEND | O_WRONLY, 0777);
        if (Outputfile < 0) {
`          fprintf(stderr,
                  "Error:: I can not write analysis passes's output on %s...\n",
                  ((char *)ExpEnv.analysis_struc.outfile));
          fprintf(stderr, "        %s...\n", strerror(errno));
          errno = 0;
          exit(1);
        }
        dup2(Outputfile, 2);
        shutdown_llvm();
        close(Outputfile);
        dup2(stderrcopy, 2);
      }
      close(stderrcopy);
    } else
      shutdown_llvm();
  }
#endif

  Yap_exit(out);
  return TRUE;
}

static bool valid_prop(Prop p, 
		       Term task) {
  if (p==NIL)
    return false;
  if (IsVarTerm(task)) {
    return true;
  }
  PredEntry *pe = RepPredProp(p);
  if (task == TermUndefined)
      return pe->PredFlags & UndefPredFlag;
  if ((pe->PredFlags & HiddenPredFlag)) {
    return false;
  }
  if (task == TermSystem || task == TermProlog) {
    return Yap_isSystemModule(pe->ModuleOfPred);
  }
  if (task == TermUser) {
    return !Yap_isSystemModule(pe->ModuleOfPred);
  }
  return true;
}



static Prop  next_pred_in_chain(Prop p, Term task)
{
  Prop np = p->NextOfPE;
  while (np) {
    if( IsPredProperty(np->KindOfPE)&& valid_prop((np),task)) {
      return np;
    }
    np = np->NextOfPE;
  }
  return NIL;
}
  
static Int continue_functor_predicate(USES_REGS1)
{
    // operating within the same module.
    Term task = Deref(ARG4); 
    Prop p =AddressOfTerm(EXTRA_CBACK_ARG(4, 1));
    Prop np = next_pred_in_chain(p, task);
    if (!np) {
      cut_succeed();
    }
    Term mod = RepPredProp( p)->ModuleOfPred;
    if (mod == PROLOG_MODULE)
      mod = TermProlog;
    Yap_unify(ARG1,mod);
    EXTRA_CBACK_ARG(4, 1) = MkAddressTerm(np);
    return true;
}



/**
 * functor_predicate(Mod, +Name, +Arity, Task)
 *
 * Enumerate all predicates for Mod.
*/
static Int functor_predicate(USES_REGS1)
{ 
  Term t2 = Deref(ARG2);
  Term t3 = Deref(ARG3);
  Term task = Deref(ARG4); 
  must_be_atom(t2);
  must_be_arity(t3);
  Term t1    = Deref(ARG1);
  Atom name = AtomOfTerm(t2);
  arity_t Arity = IntegerOfTerm(t3);
  if (IsVarTerm(t1)) {
    Prop p;
    if (Arity==0) {
	p=RepAtom(name)->PropsOfAE;
      } else {
	p = Yap_MkFunctor(name,Arity)->PropsOfFE;
      }
    p = next_pred_in_chain(p, task);
    if (!p) {
      cut_fail();
    }
    EXTRA_CBACK_ARG(4, 1) = MkAddressTerm(p);
    return continue_functor_predicate(PASS_REGS1);
  } else {
    Term task = Deref(ARG4);
    must_be_atom(t1);
    bool rc;
    PredEntry *np;
    if (Arity == 0) {
      np = Yap_get_pred(t2, t1, NULL);
      rc = np!= NULL&& valid_prop(AbsPredProp(np),task);
    } else {
      np = Yap_get_pred(Yap_MkNewApplTerm(Yap_MkFunctor(AtomOfTerm(t2),Arity), Arity),t1,NULL);
      rc = np != NULL&& valid_prop(AbsPredProp(np),task);
    }
    if (rc) {
      cut_succeed();
    } else {
      cut_fail();
    }
  }
}

Prop next_functor_for_atom(Prop p)
{
   while (p != NIL) {       
      if ( p->KindOfPE == FunctorProperty) {
       return p;
      }
      p=p->NextOfPE;
    }
   return NIL;
 }

/** @pred name_functor(Name,Arity)
    Succeeds if Name is an atom and `Name(...)` with arity _N_ > 0 exists in the */
static Int continue_atom_functor(USES_REGS1)
{
    // find functors 
  Prop p;
  arity_t Arity;

  p = AddressOfTerm(EXTRA_CBACK_ARG(2, 1));
  Prop np=next_functor_for_atom(p->NextOfPE);
  Arity = RepFunctorProp(p)->ArityOfFE;
  bool rc = Yap_unify(ARG2, MkIntTerm(Arity));
  if (np) {
    EXTRA_CBACK_ARG(2, 1) = MkAddressTerm(np);
    return rc;
  } else {
    if (rc)
      cut_succeed();
    else
      cut_fail();
  }
}


/**
 * atom_functor(Name, Arity)
 *
 * Enumerate all predicate functors for Mod.
*/
static Int atom_functor(USES_REGS1)
{
  
  Term t1 = Deref(ARG1);
  must_be_atom(t1);
  Atom at = AtomOfTerm(t1);
  Prop p = RepAtom(at)->PropsOfAE;
   if (!IsVarTerm(Deref(ARG2))) {
    must_be_arity(Deref(ARG2));
    cut_succeed();
   }
   p = next_functor_for_atom(p);
   bool rc = Yap_unify(ARG2,MkIntTerm(0));
   if (!p) {
     if (rc)
       cut_succeed();
     else
       cut_fail();
   } else {
     EXTRA_CBACK_ARG(2, 1) = MkAddressTerm(p);
     return rc;
   }
}

static Int continue_module_predicate(USES_REGS1)
{
    // operating within the same module.
  PredEntry *npp, *pp;
    Term task = Deref(ARG4), mod=Deref(ARG1);
    
    pp = AddressOfTerm(EXTRA_CBACK_ARG(4, 1));
    if (!pp) {
      if (mod && !IsAtomTerm(mod)) {
        Yap_ThrowError(TYPE_ERROR_ATOM, mod, "module name");
      }
      ModEntry *m = Yap_GetModuleEntry(mod);
      pp = m->PredForME;
    }
    while (pp && !valid_prop((Prop)pp, task)) {
      pp = pp->NextPredOfModule;
    }
    if (!pp) {
        /* try Prolog Module */
        cut_fail();
    }
    npp = pp->NextPredOfModule;
while (npp && !valid_prop((Prop)(npp), task)) {
      npp = npp->NextPredOfModule;
    }
 Term name;
 arity_t Arity;
    // just try next one
  if (pp->ModuleOfPred != IDB_MODULE) {
    Functor f = pp->FunctorOfPred;
    Arity = pp->ArityOfPE;
    if (Arity)
      name = MkAtomTerm(NameOfFunctor(f));
    else
      name = MkAtomTerm((Atom)f);
  } else {
    if (pp->PredFlags & NumberDBPredFlag) {
      name = MkIntegerTerm(pp->src.IndxId);
      Arity = 0;
    } else if (pp->PredFlags & AtomDBPredFlag) {
      Functor f = pp->FunctorOfPred;
      name = MkAtomTerm((Atom)f);
      Arity = 0;
    } else {
     Functor  f = pp->FunctorOfPred;
      name = MkAtomTerm(NameOfFunctor(f));
      Arity = ArityOfFunctor(pp->FunctorOfPred);
    }
  }
  Yap_unify(name, ARG2) ;
	 Yap_unify(MkIntTerm(Arity), ARG3);
    if (npp) {
      EXTRA_CBACK_ARG(4, 1) = MkAddressTerm(npp);
      return true;
    } else {
      cut_succeed();
    }
}


/**
 * module_predicate(+Mod, Name, Arity, Task)
 *
 * Enumerate all predicates for Mod.
*/
static Int module_predicate(USES_REGS1)
{
  EXTRA_CBACK_ARG(4, 1) = MkAddressTerm(NULL);
  return continue_module_predicate(PASS_REGS1);
}

static Int functors_for_atom(USES_REGS1) {
  Atom at = AtomOfTerm(Deref(ARG1));
  Term o = TermNil;
  Prop p = RepAtom(at)->PropsOfAE;
  while(!EndOfPAEntr(p)) {
    if (IsFunctorProperty(p->KindOfPE)) {
      Term t = Yap_MkNewApplTerm( (RepFunctorProp(p)),RepFunctorProp(p)->ArityOfFE);
      o = MkPairTerm(t,o);
    }
    p = p->NextOfPE;
  }
  return Yap_unify(o,ARG2);
}
static OpEntry *NextOp(Prop pp USES_REGS) {

  while (!EndOfPAEntr(pp) && 
    pp->KindOfPE != OpProperty &&
    (RepOpProp(pp)->OpModule != PROLOG_MODULE || RepOpProp(pp)->OpModule != CurrentModule)
   )
	 pp = pp->NextOfPE;
  return RepOpProp(pp);
}

int Yap_IsOp(Atom at) {
  CACHE_REGS
  OpEntry *op = NextOp(RepAtom(at)->PropsOfAE PASS_REGS);
  return (!EndOfPAEntr(op));
}

int Yap_IsOpMaxPrio(Atom at) {
  CACHE_REGS
    OpEntry *op = NextOp(RepAtom(at)->PropsOfAE PASS_REGS);
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

static Int unify_op(OpEntry *op USES_REGS) {
  Term tmod = op->OpModule;
  
  if (tmod == PROLOG_MODULE)
    tmod = TermProlog;
  return Yap_unify_constant(ARG2, tmod) &&
         Yap_unify_constant(ARG3, MkIntegerTerm(op->Prefix)) &&
         Yap_unify_constant(ARG4, MkIntegerTerm(op->Infix)) &&
         Yap_unify_constant(ARG5, MkIntegerTerm(op->Posfix));
}

static Int cont_current_op(USES_REGS1) {
  OpEntry *op = (OpEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(5, 1)), *next;

  READ_LOCK(op->OpRWLock);
  next = op->OpNext;
  if (Yap_unify_constant(ARG1, MkAtomTerm(op->OpName)) &&
      unify_op(op PASS_REGS)) {
    READ_UNLOCK(op->OpRWLock);
    if (next) {
      EXTRA_CBACK_ARG(5, 1) = (CELL)MkIntegerTerm((CELL)next);
      B->cp_h = HR;
      return true;
    } else {
      cut_succeed();
    }
  } else {
    READ_UNLOCK(op->OpRWLock);
    if (next) {
      EXTRA_CBACK_ARG(5, 1) = (CELL)MkIntegerTerm((CELL)next);
      B->cp_h = HR;
      return false;
    } else {
      cut_fail();
    }
  }
}

static Int init_current_op(
    USES_REGS1) { /* current_op(-Precedence,-Type,-Atom)		 */
  EXTRA_CBACK_ARG(5, 1) = (CELL)MkIntegerTerm((CELL)OpList);
  B->cp_h = HR;
  return cont_current_op(PASS_REGS1);
}

static Int cont_current_atom_op(USES_REGS1) {
  OpEntry *op = (OpEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(5, 1)), *next;

  READ_LOCK(op->OpRWLock);
  next = NextOp(op->NextOfPE PASS_REGS);
  if (unify_op(op PASS_REGS)) {
    READ_UNLOCK(op->OpRWLock);
    if (next) {
      EXTRA_CBACK_ARG(5, 1) = (CELL)MkIntegerTerm((CELL)next);
      B->cp_h = HR;
      return TRUE;
    } else {
      cut_succeed();
    }
  } else {
    READ_UNLOCK(op->OpRWLock);
    if (next) {
      EXTRA_CBACK_ARG(5, 1) = (CELL)MkIntegerTerm((CELL)next);
      B->cp_h = HR;
      return FALSE;
    } else {
      cut_fail();
    }
  }
}

static Int init_current_atom_op(
    USES_REGS1) { /* current_op(-Precedence,-Type,-Atom)		 */
  Term t = Deref(ARG1);
  AtomEntry *ae;
  OpEntry *ope;

  if (IsVarTerm(t) || !IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "current_op/3");
    cut_fail();
  }
  ae = RepAtom(AtomOfTerm(t));
  if (EndOfPAEntr((ope = NextOp(ae->PropsOfAE PASS_REGS)))) {
    cut_fail();
  }
  EXTRA_CBACK_ARG(5, 1) = (CELL)MkIntegerTerm((Int)ope);
  B->cp_h = HR;
  return cont_current_atom_op(PASS_REGS1);
}

#if 0
static Int
    copy_local_ops(USES_REGS1) { /* current_op(-Precedence,-Type,-Atom) */
  Term tmodin = Deref(ARG1);
  Term t = Deref(ARG1);
  AtomEntry *ae;
  OpEntry *ope;

  if (IsVarTerm(t) || !IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "current_op/3");
    cut_fail();
  }
  ae = RepAtom(AtomOfTerm(t));
  if (EndOfPAEntr((ope = NextOp(ae->PropsOfAE PASS_REGS)))) {
    cut_fail();
  }
  EXTRA_CBACK_ARG(5, 1) = (CELL)MkIntegerTerm((Int)ope);
  B->cp_h = HR;
  return cont_current_atom_op(PASS_REGS1);
}
#endif

void Yap_show_statistics(void) {
  CACHE_REGS
  unsigned long int heap_space_taken;
  double frag;

#if USE_SYSTEM_MALLOC && HAVE_MALLINFO
  struct mallinfo mi = mallinfo();

  heap_space_taken = (mi.arena + mi.hblkhd) - Yap_HoleSize;
#else
  heap_space_taken =
      (unsigned long int)(Unsigned(HeapTop) - Unsigned(Yap_HeapBase)) -
      Yap_HoleSize;
#endif
  frag = (100.0 * (heap_space_taken - HeapUsed)) / heap_space_taken;

  fprintf(stderr, "Code Space:  " UInt_FORMAT " (" UInt_FORMAT
                  " bytes needed, " UInt_FORMAT " bytes used, "
                  "fragmentation %.3f%%).\n",
          Unsigned(H0) - Unsigned(Yap_HeapBase),
          Unsigned(HeapTop) - Unsigned(Yap_HeapBase), Unsigned(HeapUsed), frag);
  fprintf(stderr, "Stack Space: " UInt_FORMAT " (" UInt_FORMAT
                  " for Global, " UInt_FORMAT " for local).\n",
          Unsigned(sizeof(CELL) * (LCL0 - H0)),
          Unsigned(sizeof(CELL) * (HR - H0)),
          Unsigned(sizeof(CELL) * (LCL0 - ASP)));
  fprintf(
      stderr, "Trail Space: " UInt_FORMAT " (" UInt_FORMAT " used).\n",
      Unsigned(sizeof(tr_fr_ptr) *
               (Unsigned(LOCAL_TrailTop) - Unsigned(LOCAL_TrailBase))),
      Unsigned(sizeof(tr_fr_ptr) * (Unsigned(TR) - Unsigned(LOCAL_TrailBase))));
  fprintf(stderr, "Runtime: " UInt_FORMAT "\n", runtime(PASS_REGS1)/1000000);
  fprintf(stderr, "Cputime:  %" PRIu64 "\n", Yap_cputime()/1000000);

  fprintf(stderr, "Walltime: %" PRIu64 ".\n", Yap_walltime() / (UInt)1000000);
}

static Int p_statistics_heap_max(USES_REGS1) {
  Term tmax = MkIntegerTerm(HeapMax);

  return (Yap_unify(tmax, ARG1));
}

/* The results of the next routines are not to be trusted too */
/* much. Basically, any stack shifting will seriously confuse the */
/* results */

static Int TrailTide = -1, LocalTide = -1, GlobalTide = -1;

/* maximum Trail usage */
static Int TrailMax(void) {
  CACHE_REGS
  Int i;
  Int TrWidth = Unsigned(LOCAL_TrailTop) - Unsigned(LOCAL_TrailBase);
  CELL *pt;

  if (TrailTide != TrWidth) {
    pt = (CELL *)TR;
    while (pt + 2 < (CELL *)LOCAL_TrailTop) {
      if (pt[0] == 0 && pt[1] == 0 && pt[2] == 0)
        break;
      else
        pt++;
    }
    if (pt + 2 < (CELL *)LOCAL_TrailTop)
      i = Unsigned(pt) - Unsigned(LOCAL_TrailBase);
    else
      i = TrWidth;
  } else
    return (TrWidth);
  if (TrailTide > i)
    i = TrailTide;
  else
    TrailTide = i;
  return (i);
}

static Int p_statistics_trail_max(USES_REGS1) {
  Term tmax = MkIntegerTerm(TrailMax());

  return (Yap_unify(tmax, ARG1));
}

/* maximum Global usage */
static Int GlobalMax(void) {
  CACHE_REGS
  Int i;
  Int StkWidth = Unsigned(LCL0) - Unsigned(H0);
  CELL *pt;

  if (GlobalTide != StkWidth) {
    pt = HR;
    while (pt + 2 < ASP) {
      if (pt[0] == 0 && pt[1] == 0 && pt[2] == 0)
        break;
      else
        pt++;
    }
    if (pt + 2 < ASP)
      i = Unsigned(pt) - Unsigned(H0);
    else
      /* so that both Local and Global have reached maximum width */
      GlobalTide = LocalTide = i = StkWidth;
  } else
    return (StkWidth);
  if (GlobalTide > i)
    i = GlobalTide;
  else
    GlobalTide = i;
  return (i);
}

static Int p_statistics_global_max(USES_REGS1) {
  Term tmax = MkIntegerTerm(GlobalMax());

  return (Yap_unify(tmax, ARG1));
}

static Int LocalMax(void) {
  CACHE_REGS
  Int i;
  Int StkWidth = Unsigned(LCL0) - Unsigned(H0);
  CELL *pt;

  if (LocalTide != StkWidth) {
    pt = LCL0;
    while (pt - 3 > HR) {
      if (pt[-1] == 0 && pt[-2] == 0 && pt[-3] == 0)
        break;
      else
        --pt;
    }
    if (pt - 3 > HR)
      i = Unsigned(LCL0) - Unsigned(pt);
    else
      /* so that both Local and Global have reached maximum width */
      GlobalTide = LocalTide = i = StkWidth;
  } else
    return (StkWidth);
  if (LocalTide > i)
    i = LocalTide;
  else
    LocalTide = i;
  return (i);
}

static Int p_statistics_local_max(USES_REGS1) {
  Term tmax = MkIntegerTerm(LocalMax());

  return (Yap_unify(tmax, ARG1));
}

static Int p_statistics_heap_info(USES_REGS1) {
  Term tusage = MkIntegerTerm(HeapUsed);

#if USE_SYSTEM_MALLOC && HAVE_MALLINFO
  struct mallinfo mi = mallinfo();

  UInt sstack = Yap_HoleSize + (LOCAL_TrailTop - LOCAL_GlobalBase);
  UInt mmax = (mi.arena + mi.hblkhd);
  Term tmax = MkIntegerTerm(mmax - sstack);
  tusage = MkIntegerTerm(mmax - (mi.fordblks + sstack));
#else
  Term tmax = MkIntegerTerm((LOCAL_GlobalBase - Yap_HeapBase) - Yap_HoleSize);
#endif

  return (Yap_unify(tmax, ARG1) && Yap_unify(tusage, ARG2));
}

static Int p_statistics_stacks_info(USES_REGS1) {
  Term tmax = MkIntegerTerm(Unsigned(LCL0) - Unsigned(H0));
  Term tgusage = MkIntegerTerm(Unsigned(HR) - Unsigned(H0));
  Term tlusage = MkIntegerTerm(Unsigned(LCL0) - Unsigned(ASP));

  return (Yap_unify(tmax, ARG1) && Yap_unify(tgusage, ARG2) &&
          Yap_unify(tlusage, ARG3));
}

static Int p_statistics_trail_info(USES_REGS1) {
  Term tmax =
      MkIntegerTerm(Unsigned(LOCAL_TrailTop) - Unsigned(LOCAL_TrailBase));
  Term tusage = MkIntegerTerm(Unsigned(TR) - Unsigned(LOCAL_TrailBase));

  return (Yap_unify(tmax, ARG1) && Yap_unify(tusage, ARG2));
}

static Int p_statistics_atom_info(USES_REGS1) {
  UInt count = 0, spaceused = 0, i;

  for (i = 0; i < AtomHashTableSize; i++) {
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
      spaceused +=
          sizeof(AtomEntry) + strlen((char *)RepAtom(catom)->StrOfAE) + 1;
      ncatom = RepAtom(catom)->NextOfAE;
      if (ncatom != NIL) {
        READ_LOCK(RepAtom(ncatom)->ARWLock);
      }
      READ_UNLOCK(RepAtom(catom)->ARWLock);
      catom = ncatom;
    }
  }
  for (i = 0; i < WideAtomHashTableSize; i++) {
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
      spaceused +=
          sizeof(AtomEntry) +
          sizeof(wchar_t) * (wcslen((wchar_t *)(RepAtom(catom)->StrOfAE) + 1));
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

static Int p_statistics_db_size(USES_REGS1) {
  Term t = MkIntegerTerm(Yap_ClauseSpace);
  Term tit = MkIntegerTerm(Yap_IndexSpace_Tree);
  Term tis = MkIntegerTerm(Yap_IndexSpace_SW);
  Term tie = MkIntegerTerm(Yap_IndexSpace_EXT);

  return Yap_unify(t, ARG1) && Yap_unify(tit, ARG2) && Yap_unify(tis, ARG3) &&
         Yap_unify(tie, ARG4);
}

static Int p_statistics_lu_db_size(USES_REGS1) {
  Term t = MkIntegerTerm(Yap_LUClauseSpace);
  Term tit = MkIntegerTerm(Yap_LUIndexSpace_Tree);
  Term tic = MkIntegerTerm(Yap_LUIndexSpace_CP);
  Term tix = MkIntegerTerm(Yap_LUIndexSpace_EXT);
  Term tis = MkIntegerTerm(Yap_LUIndexSpace_SW);

  return Yap_unify(t, ARG1) && Yap_unify(tit, ARG2) && Yap_unify(tic, ARG3) &&
         Yap_unify(tis, ARG4) && Yap_unify(tix, ARG5);
}

static Int p_executable(USES_REGS1) {
  int lvl = push_text_stack();
  const char *tmp =

    Yap_AbsoluteFile(GLOBAL_argv[0], true);
    if (!tmp || tmp[0] == '\0' ) {
      tmp = Malloc(MAX_PATH + 1);
      strncpy((char *)tmp, Yap_FindExecutable(), MAX_PATH);
    }
  Atom at = Yap_LookupAtom(tmp);
  pop_text_stack(lvl);
  return Yap_unify(MkAtomTerm(at), ARG1);
}

static Int p_system_mode(USES_REGS1) {
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    if (LOCAL_PrologMode & SystemMode)
      return Yap_unify(t1, MkAtomTerm(AtomTrue));
    else
      return Yap_unify(t1, MkAtomTerm(AtomFalse));
  } else {
    Atom at = AtomOfTerm(t1);
    if (at == AtomFalse)
      LOCAL_PrologMode &= ~SystemMode;
    else
      LOCAL_PrologMode |= SystemMode;
  }
  return TRUE;
}

static Int p_lock_system(USES_REGS1) {
  LOCK(GLOBAL_BGL);
  return TRUE;
}

static Int p_unlock_system(USES_REGS1) {
  UNLOCK(GLOBAL_BGL);
  return TRUE;
}

static Int enter_undefp(USES_REGS1) {
  if (LOCAL_DoingUndefp) {
    return FALSE;
  }
  LOCAL_DoingUndefp = TRUE;
  return TRUE;
}

static Int exit_undefp(USES_REGS1) {
    LOCAL_DoingUndefp = FALSE;
    return true;
}

#ifdef DEBUG

static Int p_dump_active_goals(USES_REGS1) {
  DumpActiveGoals(stderr,true PASS_REGS);
  return (TRUE);
}
#endif

#ifdef INES
static Int p_euc_dist(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2  = Deref(ARG2);
  double d1 = (double)(IntegerOfTerm(ArgOfTerm(1, t1)) -
                       IntegerOfTerm(ArgOfTerm(1, t2)));
  double d2 = (double)(IntegerOfTerm(ArgOfTerm(2, t1)) -
                       IntegerOfTerm(ArgOfTerm(2, t2)));
  double d3 = (double)(IntegerOfTerm(ArgOfTerm(3, t1)) -
                       IntegerOfTerm(ArgOfTerm(3, t2)));
  Int result = (Int)sqrt(d1 * d1 + d2 * d2 + d3 * d3);
  return (Yap_unify(ARG3, MkIntegerTerm(result)));
}

volatile int loop_counter = 0;

static Int p_loop(USES_REGS1) {
  while (loop_counter == 0)
    ;
  return (TRUE);
}
#endif

static Int p_break(USES_REGS1) {
  Atom at = AtomOfTerm(Deref(ARG1));
  if (at == AtomTrue) {
    LOCAL_BreakLevel++;
    return TRUE;
  }
  if (at == AtomFalse) {
    LOCAL_BreakLevel--;
    return TRUE;
  }
  return FALSE;
}

void Yap_InitBackCPreds(void) {
  Yap_InitCPredBack("$current_op", 5, 1, init_current_op, cont_current_op,
                    SafePredFlag | SyncPredFlag);
  Yap_InitCPredBack("$current_atom_op", 5, 1, init_current_atom_op,
                    cont_current_atom_op, SafePredFlag | SyncPredFlag);
  Yap_InitCPredBack("module_predicate", 4, 1, module_predicate,continue_module_predicate, SafePredFlag );
  Yap_InitCPredBack("atom_functor", 2, 1, atom_functor,continue_atom_functor, SafePredFlag );
  Yap_InitCPredBack("functor_predicate", 4, 1, functor_predicate,continue_functor_predicate, SafePredFlag );

#ifdef BEAM
  Yap_InitCPredBack("eam", 1, 0, start_eam, cont_eam, SafePredFlag);
#endif


  Yap_InitBackAtoms();
  Yap_InitBackIO();
  Yap_InitBackDB();
  Yap_InitUserBacks();
}

typedef void (*Proc)(void);

Proc E_Modules[] = {/* init_fc,*/ (Proc)0};

#ifdef YAPOR
static Int p_parallel_mode(USES_REGS1) { return FALSE; }

static Int p_yapor_workers(USES_REGS1) { return FALSE; }
#endif /* YAPOR */

void Yap_InitCPreds(void) {
  /* numerical comparison */
  Yap_InitCPred("set_value", 2, p_setval, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get_value", 2, p_value,
                TestPredFlag | SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$values", 3, p_values, SafePredFlag | SyncPredFlag);
  /* general purpose */
  Yap_InitCPred("opdec", 4, opdec, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("=..", 2, p_univ, 0);
  Yap_InitCPred("$statistics_trail_max", 1, p_statistics_trail_max,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$statistics_heap_max", 1, p_statistics_heap_max,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$statistics_global_max", 1, p_statistics_global_max,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$statistics_local_max", 1, p_statistics_local_max,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$statistics_heap_info", 2, p_statistics_heap_info,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$statistics_stacks_info", 3, p_statistics_stacks_info,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$statistics_trail_info", 2, p_statistics_trail_info,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$statistics_atom_info", 2, p_statistics_atom_info,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$statistics_db_size", 4, p_statistics_db_size,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$statistics_lu_db_size", 5, p_statistics_lu_db_size,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$executable", 1, p_executable, SafePredFlag);
  Yap_InitCPred("$runtime", 2, p_runtime, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$cputime", 2, p_cputime, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$systime", 2, p_systime, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$walltime", 2, p_walltime, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$system_mode", 1, p_system_mode, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$break", 1, p_break, SafePredFlag);
#ifdef BEAM
  Yap_InitCPred("@", 0, eager_split, SafePredFlag);
  Yap_InitCPred(":", 0, force_wait, SafePredFlag);
  Yap_InitCPred("/", 0, commit, SafePredFlag);
  Yap_InitCPred("skip_while_var", 1, skip_while_var, SafePredFlag);
  Yap_InitCPred("wait_while_var", 1, wait_while_var, SafePredFlag);
  Yap_InitCPred("eamtime", 0, show_time, SafePredFlag);
  Yap_InitCPred("eam", 0, use_eam, SafePredFlag);
#endif
  Yap_InitCPred("$halt", 1, p_halt, SyncPredFlag);
  Yap_InitCPred("$lock_system", 0, p_lock_system, SafePredFlag);
  Yap_InitCPred("$unlock_system", 0, p_unlock_system, SafePredFlag);
  Yap_InitCPred("$enter_undefp", 0, enter_undefp, SafePredFlag);
  Yap_InitCPred("$exit_undefp", 0, exit_undefp, SafePredFlag);
  Yap_InitCPred("$functors_for_atom", 2, functors_for_atom, SafePredFlag);

#ifdef YAP_JIT
  Yap_InitCPred("$jit_init", 1, p_jit, SafePredFlag | SyncPredFlag);
#endif /* YAPOR */
#ifdef INES
  Yap_InitCPred("euc_dist", 3, p_euc_dist, SafePredFlag);
  Yap_InitCPred("loop", 0, p_loop, SafePredFlag);
#endif
#if QSAR
  Yap_InitCPred("in_range", 8, p_in_range, TestPredFlag | SafePredFlag);
  Yap_InitCPred("in_range", 4, p_in_range2, TestPredFlag | SafePredFlag);
#endif
#ifdef DEBUG
  Yap_InitCPred("dump_active_goals", 0, p_dump_active_goals,
                SafePredFlag | SyncPredFlag);
#endif

  Yap_InitArrayPreds();
  Yap_InitAtomPreds();
  Yap_InitBBPreds();
  Yap_InitBigNums();
  Yap_InitCdMgr();
  Yap_InitCmpPreds();
  Yap_InitCopyTerm();
  Yap_InitCoroutPreds();
  Yap_InitDBPreds();
  Yap_InitErrorPreds();
  Yap_InitExecFs();
  Yap_InitExecStruct();
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
  Yap_InitStInfo();
  Yap_udi_init();
  Yap_udi_Interval_init();
  Yap_InitSignalCPreds();
  Yap_InitUserCPreds();
  Yap_InitUtilCPreds();
    Yap_InitTermCPreds();
    Yap_InitSortPreds();
  Yap_InitMaVarCPreds();
#ifdef DEPTH_LIMIT
  Yap_InitItDeepenPreds();
#endif
#ifdef ANALYST
  Yap_InitAnalystPreds();
#endif
  Yap_InitLowLevelTrace();
  Yap_InitEval();
  Yap_InitGrowPreds();
  Yap_InitLowProf();
#if defined(YAPOR) || defined(TABLING)
  Yap_init_optyap_preds();
#endif /* YAPOR || TABLING */
#if YAP_JIT
  Yap_InitCPred("jit", 0, p_jit, SafePredFlag | SyncPredFlag);
#endif
  Yap_InitThreadPreds();
  {
    void (*(*(p)))(void) = E_Modules;
    while (*p)
      (*(*p++))();
  }
#if USE_MYDDASX
  init_myddas();
#endif
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
  init_regexp();
#endif
}
