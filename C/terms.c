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
    * File:		utilpreds.c * Last rev:	4/03/88
    ** mods: * comments:	new utility predicates for YAP *
    *									 *
    *************************************************************************/

#ifndef TERMS_C
#define TERMS_C 1

//#define DEB_D0(S) printf("%s %s: d0=%lx ptd0=%p *ptd0=%lx\n",S,d0, ptd0,
/**
 * @file C/terms.c
 *
 * @brief applications of the tree walker pattern.
 *
 * @addtogroup Terms
 *
 * @{
 *
 */

#include "absmi.h"

#include "YapHeap.h"

#include "heapgc.h"

#include "attvar.h"
#include "clause.h"
#include "yapio.h"
#ifdef HAVE_STRING_H
#include "string.h"
#endif

/*#define err, "%s %ld %p->%p=%lx ", s, st->pt - st->pt0, pt0, ptd0, d0), */
/*   Yap_DebugPlWriteln(d0)) */

static inline bool push_sub_term(Ystack_t *sp, CELL d0, CELL *pt0, CELL *b,
                                 CELL *e) {
  copy_frame *pt = sp->pt;
  // DEB_DOOB("+");
  if (sp->max == pt)
    return false;
  pt->pt0 = b;
  pt->pt0_end = e;
  (pt)->oldv = d0;
  (pt)->oldp = pt0;
  sp->pt++;
  return true;
}

static inline bool pop_sub_term(Ystack_t *sp, CELL **b, CELL **e) {

  copy_frame *pt = --sp->pt;
  if (pt < sp->pt0)
    return false;
  //   DEB_DOOB("-");
  if (pt->oldp != NULL)
    pt->oldp[0] = pt->oldv;
  if (b)
    *b = pt->pt0;
  if (e)
    *e = pt->pt0_end;
  return true;
}

#define END_WALK()                                                             \
  }    }                                                                        \
  /***** start of bottom-macro ************/                                   \
  nomore:

#define CYC_LIST                                                               \
  if (IS_VISIT_MARKER(*ptd0)) {                                                \
    while (pop_sub_term(&stt, NULL, NULL)) {                                   \
    };                                                                         \
     close_stack(&stt); return true;                                                               \
  }

#define CYC()                                                                  \
  if (IS_VISIT_MARKER(*ptd0)) {                                                \
    while (pop_sub_term(&stt, NULL, NULL)) {                                   \
    };                                                                         \
     close_stack(&stt);return true;                                                               \
  }

/**
   @brief routine to locate all variables in a term, and its applications */

static Term cyclic_complex_term(CELL *pt0_, CELL *pt0_end_ USES_REGS) {
  tr_fr_ptr TR0 = TR;

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE CYC()
#define COMPOUND_HOOK_CODE CYC()
#define ATOMIC_HOOK_CODE                                                       \
  {}

#include "term_visit.h"
  /* leave an empty slot to fill in later */
  END_WALK();
  // no cycles found
  close_stack(&stt);
  return false;
}

bool Yap_IsCyclicTerm(Term t USES_REGS) {
  if (IsVarTerm(t)) {
    return false;
  } else if (IsPrimitiveTerm(t)) {
    return false;
  } else {
    return cyclic_complex_term(&(t)-1, &(t)PASS_REGS);
  }
}

/** @pred  cyclic_term( + _T_ )


    Succeeds if the graph representation of the term has loops. Say,
    The representation of a term `X` that obeys the equation `X=[X]`
    term has a loop from the list to its head.


*/
static Int cyclic_term(USES_REGS1) /* cyclic_term(+T)		 */
{
  return Yap_IsCyclicTerm(Deref(ARG1));
}

/**
   @brief routine to locate all variables in a term, and its application
   s */

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE                                                     \
  {}
#define ATOMIC_HOOK_CODE                                                       \
  {}

static bool ground_complex_term(CELL *pt0_, CELL *pt0_end_ USES_REGS) {
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;
  //  pp(pt0_     [1], 0);
#include "term_visit.h"
  while (pop_sub_term(&stt, NULL, NULL)) {
  };
  HR = InitialH;
  //  pp(pt0_[1], 0);
 close_stack(&stt);
  return false;

  END_WALK();
 close_stack(&stt);
 return true;
  /* should have a return before this point */
  /* close main processing loop */
}

bool Yap_IsGroundTerm(Term t) {
  CACHE_REGS

  if (IsVarTerm(t)) {
    return false;
  } else if (IsPrimitiveTerm(t)) {
    return true;
  } else {
    return ground_complex_term(&(t)-1, &(t)PASS_REGS);
  }
}

/** @pred  ground( _T_) is iso

    Succeeds if there are no free variables in the term  _T_.



*/
static Int ground(USES_REGS1) /* ground(+T)		 */
{
  return Yap_IsGroundTerm(Deref(ARG1));
}

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE                                                     \
  {}
#define ATOMIC_HOOK_CODE                                                       \
  {}

static Int var_in_complex_term(CELL *pt0_, CELL *pt0_end_, Term v USES_REGS) {
  tr_fr_ptr TR0 = TR;

#include "term_visit.h"

  if ((CELL)ptd0 == v) { /* we found it */
    /* Do we still have compound terms to visit */
    while (pop_sub_term(&stt, NULL, NULL)) {
    };
    close_stack(&stt);
    return true;
  }
  END_WALK();
 close_stack(&stt);
 return false;
}



static Int var_in_term(Term v,
                       Term t USES_REGS) /* variables in term t		 */
{
  must_be_variable(v);
  t = Deref(t);
  if (IsVarTerm(t)) {
    return (v == t);
  } else if (IsPrimitiveTerm(t)) {
    return (false);
  }
  return (var_in_complex_term(&(t)-1, &(t), v PASS_REGS));
}

/** @pred variable_in_term(? _Term_,? _Var_)


    Succeed if the second argument  _Var_ is a variable and occurs in
    term  _Term_.


*/
static Int variable_in_term(USES_REGS1) {
  return var_in_term(Deref(ARG2), Deref(ARG1) PASS_REGS);
}

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE                                                     \
  {}
#define ATOMIC_HOOK_CODE                                                       \
  {}

/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static Term vars_in_complex_term(CELL *pt0_, CELL *pt0_end_,
                                 Term inp USES_REGS) {
  tr_fr_ptr TR0;
  Term *end = NULL, first = inp;

  Int count = 0;
  // first get the extra variables
  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      Bind_and_Trail(ptr, TermFoundVar);
      count++;
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 1024)
        goto trail_overflow;
      /* do or pt2 are unbound  */
    }
   inp = TailOfTerm(inp);
  }
  TR0 = TR;
 #include "term_visit.h"
  if (HR + 1024 > ASP) {
    goto global_overflow;
  }
    if (end == NULL) {
        first = AbsPair(HR);
    } else {
        end[0] = AbsPair(HR);
    }
    HR[0] = (CELL)ptd0;
        HR[1] = inp;
        end = HR+1;
        HR+=2;
   
/* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
    goto trail_overflow;
  }
  mBind_And_Trail(ptd0, TermFoundVar);
  END_WALK();

  clean_tr(TR0 - count PASS_REGS);
  return first;
}

/**
 * @pred variables_in_term( +_T_, +_SetOfVariables_, +_ExtendedSetOfVariables_
 * )
 *
 * _SetOfVariables_ must be a list of unbound variables. If so,
 * _ExtendedSetOfVariables_ will include all te variables in the union
 * of `vars(_T_)` and _SetOfVariables_.
 */
static Int variables_in_term(USES_REGS1) /* variables in term t		 */
{
  Term out, inp;

  inp = Deref(ARG2);
  Term t = Deref(ARG1);
  out = vars_in_complex_term(&(t)-1, &(t), inp PASS_REGS);
  return Yap_unify(ARG3, out);
}

/** @pred  term_variables(? _Term_, - _Variables_, +_ExternalVars_) is iso



    Unify the difference list between _Variables_ and _ExternaVars_
    with the list of all variables of term _Term_.  The variables
    occur in the order of their first appearance when traversing the
    term depth-first, left-to-right.


*/
static Int term_variables3(USES_REGS1) /* variables in term t		 */
{
  Term out;

  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Term out = Yap_MkNewPairTerm();
    return Yap_unify(t, HeadOfTerm(out)) && Yap_unify(ARG3, TailOfTerm(out)) &&
           Yap_unify(out, ARG2);
  } else if (IsPrimitiveTerm(t)) {
    return Yap_unify(ARG2, ARG3);
  } else {
    out = vars_in_complex_term(&(t)-1, &(t), ARG3 PASS_REGS);
  }

  return Yap_unify(ARG2, out);
}

/**
 * Exports a nil-terminated list with all the variables in a term.
 * @param[t] the term
 * @param[arity] the arity of the calling predicate (required for exact
 * garbage collection).
 * @param[USES_REGS] threading
 */
Term Yap_TermVariables(
    Term t, arity_t arity USES_REGS) /* variables in term t		 */
{
  Term out;

  t = Deref(t);
  if (IsVarTerm(t)) {
    return MkPairTerm(t, TermNil);
  } else if (IsPrimitiveTerm(t)) {
    return TermNil;
  } else {
    out = vars_in_complex_term(&(t)-1, &(t), TermNil PASS_REGS);
  }
  return out;
}

#if UNUSED
static Term
Yap_TermAddVariables(Term t,
                     Term vs USES_REGS) /* variables in term t		 */
{
  Term out;

  t = Deref(t);
  if (IsVarTerm(t)) {
    return MkPairTerm(t, TermNil);
  } else if (IsPrimitiveTerm(t)) {
    return TermNil;
  } else {
    out = vars_in_complex_term(&(t)-1, &(t), vs PASS_REGS);
  }
  return out;
}
#endif

/** @pred  term_variables(? _Term_, - _Variables_) is iso



    Unify  _Variables_ with the list of all variables of term
    _Term_.  The variables occur in the order of their first
    appearance when traversing the term depth-first, left-to-right.


*/
static Int term_variables(USES_REGS1) /* variables in term t		 */
{
  Term out;
  if (!Yap_IsListOrPartialListTerm(ARG2)) {
    Yap_ThrowError(TYPE_ERROR_LIST, ARG2, "term_variables/2");
    return false;
  }

  Term t = Deref(ARG1);

  out = vars_in_complex_term(&(t)-1, &(t), TermNil PASS_REGS);
  return Yap_unify(ARG2, out);
}

/** routine to locate attributed variables */

typedef struct att_rec {
  CELL *beg, *end, d0;
} att_rec_t;

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE                                                     \
  {}
#define ATOMIC_HOOK_CODE                                                       \
  {}

static Term attvars_in_complex_term(CELL *pt0_, CELL *pt0_end_,
                                    Term inp USES_REGS) {
  tr_fr_ptr TR0 = TR;
  CELL output = inp;

#include "term_visit.h"
  if (!IS_VISIT_MARKER(*ptd0) && IsAttVar(ptd0)) {
    if (HR + 1024 > ASP) {
      goto global_overflow;
    }
    output = MkPairTerm((CELL)ptd0, output);
    /* store the terms to visit */
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
      /* Trail overflow */
      goto trail_overflow;
    }
    mBind_And_Trail(ptd0, TermFoundVar);
    //ptd0 = (CELL*)RepAttVar(ptd0);
    d0 = AbsAppl(ptd0);
    goto list_loop;

    }
  END_WALK();
  // no more variables to be found
  clean_tr(TR0 PASS_REGS);
  /*fprintf(stderr,"<%ld at %s\n", d0, __FUNCTION__)*/;
  return output;
}

/** @pred term_attvars(+ _Term_,- _AttVars_)


    _AttVars_ is a list of all attributed variables in  _Term_ and
    its attributes. I.e., term_attvars/2 works recursively through
    attributes.  This predicate is Cycle-safe.


*/
static Int term_attvars(USES_REGS1) /* variables in term t		 */
{
  Term out;

  Term t = Deref(ARG1);
  if (IsPrimitiveTerm(t)) {
    return Yap_unify(TermNil, ARG2);
  } else {
    out = attvars_in_complex_term(&(t)-1, &(t), TermNil PASS_REGS);
  }
  return Yap_unify(ARG2, out);
}

/** @brief output the difference between variables in _T_ and variables in
 * some list.
 */
#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE                                                     \
  {}
#define ATOMIC_HOOK_CODE                                                       \
  {}

static Term new_vars_in_complex_term(CELL *pt0_, CELL *pt0_end_,
                                     Term inp USES_REGS) {
  tr_fr_ptr TR0 = TR;
  Int n = 0;
  CELL output = TermNil;
  Term inp0 = inp;

  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      n++;
      Bind_and_Trail(VarOfTerm(t), TermFoundVar);
      if ((tr_fr_ptr)LOCAL_TrailTop - TR < 1024) {
        size_t expand = (tr_fr_ptr)LOCAL_TrailTop - TR;
        clean_tr(TR0 PASS_REGS);
        *HR++ = inp0;
        /* Trail overflow */
        if (!Yap_growtrail(expand, false)) {
          Yap_ThrowError(RESOURCE_ERROR_TRAIL, TermNil, expand);
        }
        inp = *--HR;
        continue;
      }
      inp = TailOfTerm(inp);
    }
  }
#include "term_visit.h"
  output = MkPairTerm((CELL)ptd0, output);
  mBind_And_Trail(ptd0, TermFoundVar);
  if ((tr_fr_ptr)LOCAL_TrailTop - TR < 1024) {
    goto trail_overflow;
  }
  /* leave an empty slot to fill in later */
  if (HR + 1024 > ASP) {
    goto global_overflow;
  }
  continue;
  END_WALK();

  clean_tr(TR0 PASS_REGS);

  return output;
}

/** @pred  new_variables_in_term(+_CurrentVariables_, ? _Term_, -_Variables_)



    Unify  _Variables_ with the list of all variables of term
    _Term_ that do not occur in _CurrentVariables_.  The variables occur in
    the order of their first appearance when traversing the term depth-first,
    left-to-right.


*/
static Int
p_new_variables_in_term(USES_REGS1) /* variables within term t		 */
{
  Term out;

  Term t = Deref(ARG2);
  if (IsPrimitiveTerm(t))
    out = TermNil;
  else {
    out = new_vars_in_complex_term(&(t)-1, &(t), Deref(ARG1) PASS_REGS);
  }
  return Yap_unify(ARG3, out);
}

#define FOUND_VAR                                                              \
  if (d0 == TermFoundVar) {                                                    \
    /* leave an empty slot to fill in later */                                 \
    if (HR + 1024 > ASP) {                                                     \
      goto global_overflow;                                                    \
    }                                                                          \
    HR[1] = AbsPair(HR + 2);                                                   \
    HR += 2;                                                                   \
    HR[-2] = (CELL)ptd0;                                                       \
    *ptd0 = TermNil;                                                           \
  }

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE                                                     \
  {}
#define ATOMIC_HOOK_CODE FOUND_VAR

static Term vars_within_complex_term(CELL *pt0_, CELL *pt0_end_,
                                     Term inp USES_REGS) {
  Int n = 0;
  CELL output = AbsPair(HR);
  tr_fr_ptr TR0 = TR;

  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      n++;
      Bind_and_Trail(ptr,TermFoundVar);
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
        Yap_growtrail(2 * n * sizeof(tr_fr_ptr *), true);
      }
    }
    inp = TailOfTerm(inp);
  }

#include "term_visit.h"
  continue;
  END_WALK();

  clean_tr(TR0 PASS_REGS);
  if (HR != HStart) {
    HR[-1] = TermNil;
    return output;
  } else {
    return TermNil;
  }
}

/** @pred  variables_within_term(+_CurrentVariables_, ? _Term_, -_Variables_)

    Unify _Variables_ with the list of all variables of term _Term_
    that *also* occur in _CurrentVariables_.  The variables occur in
    the order of their first appearance when traversing the term
    depth-first, left-to-right.

    This predicate performs the opposite of new_variables_in_term/3.

*/
static Int p_variables_within_term(USES_REGS1) /* variables within term t */
{
  Term out;

  Term t = Deref(ARG2);
  if (IsPrimitiveTerm(t))
    out = TermNil;
  else {
    out = vars_within_complex_term(&(t)-1, &(t), Deref(ARG1) PASS_REGS);
  }
  return Yap_unify(ARG3, out);
}

/* variables within term t		 */
static Int free_variables_in_term(USES_REGS1) {
  Term out;
  Term t, t0;
  Term found_module = 0L;
  Term bounds = TermNil;

  t = t0 = Deref(ARG1);

  while (!IsVarTerm(t) && IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorHat) {
      bounds = MkPairTerm(ArgOfTerm(1, t), bounds);
    } else if (f == FunctorModule) {
      found_module = ArgOfTerm(1, t);
    } else if (f == FunctorCall) {
      t = ArgOfTerm(1, t);
    } else if (f == FunctorExecuteInMod) {
      found_module = ArgOfTerm(2, t);
      t = ArgOfTerm(1, t);
    } else {
      break;
    }
    t = ArgOfTerm(2, t);
  }
  if (IsPrimitiveTerm(t))
    out = TermNil;
  else {
    out = new_vars_in_complex_term(&(t)-1, &(t),
                                   Yap_TermVariables(bounds, 3) PASS_REGS);
  }

  if (found_module && t != t0) {
    Term ts[2];
    ts[0] = found_module;
    ts[1] = t;
    t = Yap_MkApplTerm(FunctorModule, 2, ts);
  }
  return Yap_unify(ARG2, t) && Yap_unify(ARG3, out);
}

#define FOUND_VAR_AGAIN                                                        \
  if (d0 == TermFoundVar) {                                                    \
	    if (HR > ASP - 1024) {\
	      goto global_overflow;\
	    }\
    HR[0] = (CELL)ptd0;                                                        \
    HR[1] = AbsPair(HR + 2);                                                   \
    HR += 2;                                                                   \
    *ptd0 = TermRefoundVar;                                                    \
  }

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE FOUND_VAR_AGAIN
#define ATOMIC_HOOK_CODE                                                       \
  {}

static Term non_singletons_in_complex_term(CELL *pt0_, CELL *pt0_end_,
                                           Term s USES_REGS) {

  tr_fr_ptr TR0 = TR;

#include "term_visit.h"
  /* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
        goto trail_overflow;
  }
  mBind_And_Trail(ptd0, TermFoundVar);
  continue;
  END_WALK();

  clean_tr(TR0 PASS_REGS);

  if (HR != HStart) {
    /* close the list */
    HR[-1] = s;
    return AbsPair(HStart);
  } else {
    return s;
  }
}


Term Yap_NonSingletons(Term t, Term s USES_REGS) {
  if (IsVarTerm(t)) {
    return MkPairTerm(t, s);
  } else if (IsPrimitiveTerm(t)) {
    return s;
  } else {
    return non_singletons_in_complex_term(&(t)-1, &(t), s PASS_REGS);
  }
}

/** @pred  non_singletons_in_term(? _T_, _LV0_, _LVF_)
    
    Unify _LVF_-_LV0_ with the list of variables that occur at least twice in
    _T_ or that occur in _LV0_ and _T_.

*/
static Int p_non_singletons_in_term(
    USES_REGS1) /* non_singletons in term t		 */
{
  Term t;
  Term out;

  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    out = ARG2;
  } else if (IsPrimitiveTerm(t)) {
    out = ARG2;
  } else {
    out = non_singletons_in_complex_term(&(t)-1, &(t), TermNil PASS_REGS);
  }
  return Yap_unify(ARG3, out);
}

#define FOUND_VSAME_VAR_AGAIN                                                           \
  {                                                                            \
    if (RepAppl(d0)[0] == (CELL)FunctorDollarVar)  { \
        if (RepAppl(d0)[1] == MkIntTerm(-1))) {                                      \
      if (vno<0) {RepAppl(d0)[1] = MkIntTerm(vno); vno++; } else {      RepAppl(d0)[1] = MkIntTerm(vno); --vno; }                                    \
      continue;                                                                \
    }                                                                          \
  }\
}

/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE FOUND_VAR_AGAIN
#define ATOMIC_HOOK_CODE                                                       \
  {}

static Term numbervars_in_complex_term(CELL *pt0_, CELL *pt0_end_, Int vno,
                                       bool show_singletons,
                                       Int *tr_entries USES_REGS) {
    tr_fr_ptr TR0 = TR;

#include "term_visit.h"
            {
                if (HR + 1024 > ASP) {
                    goto global_overflow;
                }
                HR[0] = (CELL)FunctorDollarVar;
                mBind(ptd0, AbsAppl(HR));
                if (show_singletons)
                HR[1] = MkIntTerm(-1);
                if ( vno >= 0)
                    HR[1] = MkIntTerm(vno++);
                else if (vno < 0)
                    HR[1] = MkIntTerm(--vno);
                HR += 2;
   continue;
            }
    END_WALK();
    if (tr_entries) {
        *tr_entries = TR - TR0;
    }
    return vno;
}

Int Yap_NumberVars(Term t, Int numbv, bool handle_singles,
                   Int *tr_entries) /*
                                     * numbervariables in term t         */
{
    if ( handle_singles ) return t;
    if (IsPrimitiveTerm(t)) {
        return numbv;
    }
    Term vt = Deref(t);
    return numbervars_in_complex_term(&vt - 1, &vt, false, handle_singles, tr_entries PASS_REGS);
}

/** @pred  numbervars( _T_,+ _N1_,- _Nn_)

    Instantiates each variable in term  _T_ to a term of the form:
    `$VAR( _I_)`, with  _I_ increasing from  _N1_ to  _Nn_.
*/
static Int p_numbervars(USES_REGS1) {
    Term t2 = Deref(ARG2);
    Int out;
    if (IsVarTerm(t2)) {
        Yap_Error(INSTANTIATION_ERROR, t2, "numbervars/3");
        return false;
    }
    if (!IsIntegerTerm(t2)) {
        Yap_Error(TYPE_ERROR_INTEGER, t2, "numbervars/3");
        return (false);
    }
    out = Yap_NumberVars(Deref(ARG1), IntegerOfTerm(t2), false, NULL);
    return Yap_unify(ARG3, MkIntegerTerm(out));
}


#define MAX_NUMBERED                                                           \
  if (FunctorOfTerm(d0) == FunctorDollarVar) {                                 \
    Term t1 = ArgOfTerm(1, d0);                                                \
    Int i;                                                                     \
    if (IsIntegerTerm(t1) && ((i = IntegerOfTerm(t1)) > *maxp))                \
      *maxp = i;                                                               \
    continue;                                                                  \
  }

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE MAX_NUMBERED
#define ATOMIC_HOOK_CODE                                                       \
  {}

static int max_numbered_var(CELL *pt0_, CELL *pt0_end_, Int *maxp USES_REGS) {
  tr_fr_ptr TR0 = TR;

#include "term_visit.h"
  continue;
  END_WALK();
  /* Do we still have compound terms to visit */
  if (stt.pt > stt.pt0) {
    stt.pt--;

    pt0 = stt.pt->pt0;
    pt0_end = stt.pt->pt0_end;
    VUNMARK(stt.pt->oldp, stt.pt->oldv);
  }
  prune(B PASS_REGS);
 close_stack(&stt);
  return 0;
}

static Int MaxNumberedVar(Term inp, arity_t arity PASS_REGS) {
  Term t = Deref(inp);

  if (IsPrimitiveTerm(t)) {
    return MkIntegerTerm(0);
  } else {
    Int res;
    Int max;
    res = max_numbered_var(&t - 1, &t, &max PASS_REGS) - 1;
    if (res < 0)
      return -1;
    return MkIntegerTerm(max);
  }
}

/**
 * @pred largest_numbervar( +_Term_, -Max)
 *
 * Unify _Max_ with the largest integer _I_ such that `$VAR(I)` is  a
 * sub-term of _Term_.
 *
 * This built-in predicate is useful if part of a term has been grounded, and
 * now you want to ground the full term.
 */
static Int largest_numbervar(USES_REGS1) {
  return Yap_unify(MaxNumberedVar(Deref(ARG1), 2 PASS_REGS), ARG2);
}




void Yap_InitTermCPreds(void) {
  Yap_InitCPred("term_variables", 2, term_variables, 0);
  Yap_InitCPred("term_variables", 3, term_variables3, 0);
  Yap_InitCPred("$variables_in_term", 3, variables_in_term, 0);

  Yap_InitCPred("$free_variables_in_term", 3, free_variables_in_term, 0);
  Yap_InitCPred("free_variables_in_term", 3, free_variables_in_term, 0);

  Yap_InitCPred("term_attvars", 2, term_attvars, 0);

  CurrentModule = TERMS_MODULE;
  Yap_InitCPred("variable_in_term", 2, variable_in_term, 0);
  Yap_InitCPred("variables_within_term", 3, p_variables_within_term, 0);
  Yap_InitCPred("new_variables_in_term", 3, p_new_variables_in_term, 0);
  CurrentModule = PROLOG_MODULE;

  Yap_InitCPred("non_singletons_in_term", 3, p_non_singletons_in_term, 0);

  Yap_InitCPred("ground", 1, ground, SafePredFlag);
  Yap_InitCPred("cyclic_term", 1, cyclic_term, SafePredFlag);

  Yap_InitCPred("numbervars", 3, p_numbervars, 0);
  Yap_InitCPred("largest_numbervar", 2, largest_numbervar, 0);
}

#endif

///@}
