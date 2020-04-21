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
 *
 * mods:
 * comments:	new utility predicates for YAP *
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

staticbool push_sub_term(Ystack_t *sp, CELL d0, CELL *pt0, CELL *b,
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

staticbool pop_sub_term(Ystack_t *sp, CELL **b, CELL **e) {

  copy_frame *pt = sp->pt - 1;
  if (pt < sp->pt0)
    return false;
  //   DEB_DOOB("-");
  if (pt->oldp != NULL)
    pt->oldp[0] = pt->oldv;
  --sp->pt;
  if (b)
    *b = pt->pt0;
  if (e)
    *e = pt->pt0_end;
  return true;
}

#define END_WALK()                                                             \
  }                                                                            \
  }                                                                            \
  /***** start of bottom-macro ************/                                   \
  nomore:

#define ER(G)                                                                  \
  {                                                                            \
    if ((G) == 0 && LOCAL_Error_TYPE) {                                        \
      stt->arenap = NULL;                                                      \
      stt->t = &t;                                                             \
      stt->bindp = NULL;                                                       \
      Yap_visitor_error_handler(stt, NULL);                                    \
      LOCAL_Error_TYPE = 0;                                                    \
      goto reset;                                                              \
    }                                                                          \
  }

#define CYC()                                                                  \
  if (IS_VISIT_MARKER(*ptd1)) {                                                \
    while (pop_sub_term(stt, NULL, NULL)) {                                    \
    };                                                                         \
    return true;                                                               \
  }

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE CYC()
#define COMPOUND_HOOK_CODE CYC()
#define ATOMIC_HOOK_CODE                                                       \
  {}

/**
   @brief routine to locate all variables in a term, and its applications
   */

static Term cyclic_complex_term(CELL *pt0_, CELL *pt0_end_,
                                Ystack_t *stt USES_REGS) {
  COPY(pt0_[1]);
#include "term_visit.h"
  /* leave an empty slot to fill in later */
  END_WALK();
  // no cycles found
  return false;
}

bool Yap_IsCyclicTerm(Term t USES_REGS) {
  if (IsVarTerm(t)) {
    return false;
  } else if (IsPrimitiveTerm(t)) {
    return false;
  } else {
    Ystack_t stt_, *stt = &stt_;

    init_stack(stt, 0);
    bool rc = cyclic_complex_term(&(t)-1, &(t), stt PASS_REGS);
    close_stack(stt);
    return rc;
  }
}

/** @pred  cyclic_term( + _T_ )


    Succeeds if the graph representation of the term has loops. Say,
    The representation of a term `X` that obeys the equation `X=[X]`
    term has a loop from the list to its head.


*/
static Int cyclic_term(USES_REGS1) /* cyclic_term(+T)		 */
{

  Ystack_t stt_, *stt = &stt_;
  Int rc;
  Term t;
  init_stack(stt, 0);

reset:
  ER(rc = cyclic_complex_term(&(t)-1, &(t), stt PASS_REGS));
  close_stack(stt);
  return rc;
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

static bool ground_complex_term(CELL *pt0_, CELL *pt0_end_,
                                Ystack_t *stt USES_REGS) {
  //  pp(pt0_     [1], 0);
  COPY(pt0_[1]);
#include "term_visit.h"
  while (pop_sub_term(stt, NULL, NULL)) {
  };
  //  pp(pt0_[1], 0);
  return false;

  END_WALK();
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
    bool rc;
    Ystack_t stt_, *stt = &stt_;
    init_stack(stt, 0);
  reset:
    ER(rc = ground_complex_term(&(t)-1, &(t), stt PASS_REGS));
    close_stack(stt);
    reset_trail(stt->tr0 PASS_REGS);

    return rc;
  }
}

/** @pred  ground( _T_) is iso

    Succeeds if there are no free variables in the term  _T_.



*/
static Int ground(USES_REGS1) /* ground(+T)		 */
{
  Ystack_t stt_, *stt = &stt_;
  Term t;
  init_stack(stt, 0);
reset:
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    close_stack(stt);
    return false;
  } else if (IsPrimitiveTerm(t)) {
    close_stack(stt);
    return true;
  } else {
    int rc;
    ER(rc = ground_complex_term(&(t)-1, &(t), stt PASS_REGS));
    close_stack(stt);
    reset_trail(stt->tr0);
    return rc;
  }
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

static Int var_in_complex_term(CELL *pt0_, CELL *pt0_end_, Term v,
                               Ystack_t *stt USES_REGS) {
  COPY(pt0_[1]);

#include "term_visit.h"

  if ((CELL)ptd0 == v) { /* we found it */
    /* Do we still have compound terms to visit */
    while (pop_sub_term(stt, NULL, NULL)) {
    };
    return true;
  }
  END_WALK();
  return false;
}

/** @pred variable_in_term(? _Term_,? _Var_)


    Succeed if the second argument  _Var_ is a variable and occurs in
    term  _Term_.


*/
static Int variable_in_term(USES_REGS1) {
  Ystack_t stt_, *stt = &stt_;
  Term t, v;
  init_stack(stt, 0);
reset:
  t = Deref(ARG1), v = Deref(ARG2);
  must_be_variable(v);
  if (IsVarTerm(t)) {
    close_stack(stt);
    return (v == t);
  } else if (IsPrimitiveTerm(t)) {
    close_stack(stt);
    return (false);
  }
  Int rc;
  ER(rc = var_in_complex_term(&(t)-1, &(t), v, stt PASS_REGS));
  close_stack(stt);
  reset_trail(stt->tr0 PASS_REGS);
  return rc;
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
static Term vars_in_complex_term(CELL *pt0_, CELL *pt0_end_, tr_fr_ptr TR0,
                                 Term tail, Ystack_t *stt USES_REGS) {

  Term *end = NULL, first = tail;
  COPY(pt0_[1]);

#include "term_visit.h"

  if (HR + 1024 > ASP) {
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
    return 0;
  }
  if (end == NULL) {
    first = AbsPair(HR);
  } else {
    end[0] = AbsPair(HR);
  }
  HR[0] = (CELL)ptd0;
  HR[1] = tail;
  end = HR + 1;
  HR += 2;

  /* next make sure noone will see this, *start = HRs as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
    LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
    return 0;
  }
  mBind_And_Trail(ptd0, TermFoundVar);

  END_WALK();

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
  Ystack_t stt_, *stt = &stt_;

  Term out, t;
  if (!init_stack(stt, 0)) {
    LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
    return 0;
  }

reset:
  t = Deref(ARG1);

  ER(out = vars_in_complex_term(&(t)-1, &(t), TR, TermNil, stt PASS_REGS));
  close_stack(stt);
  reset_trail(stt->tr0 PASS_REGS);
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
  Ystack_t stt_, *stt = &stt_;
  Term t, in, out;

  init_stack(stt, 0);

reset:

  t = Deref(ARG1);

  in = Deref(ARG3);

  if (!IsVarTerm(t) && IsPrimitiveTerm(t)) {
    close_stack(stt);
    out = in;
  } else {
    ER((vars_in_complex_term(&(in)-1, &(in), stt->tr0, TermNil,
                             stt PASS_REGS)));
    ER((out =
            vars_in_complex_term(&(in)-1, &(in), stt->tr0, in, stt PASS_REGS)));
  }
  close_stack(stt);
  reset_trail(stt->tr0 PASS_REGS);

  return Yap_unify(ARG2, in);
}

/**
 * Exports a nil-terminated list with all the variables in a term.
 * @param[t] the term
 * @param[arity] the arity of the calling predicate (required for exact
 * garbage collection).
 * @param[USES_REGS] threading
 */
Term Yap_TermVariables(Term t, Term t0 USES_REGS) /* variables in term t  */
{
  Term out;
  Ystack_t stt_, *stt = &stt_;
  if (!init_stack(stt, 0)) {
    LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
    return 0;
  }
reset:
  t = Deref(t);
  if (!IsVarTerm(t) && IsPrimitiveTerm(t)) {
    close_stack(stt);
    return TermNil;
  } else {
    ER(out = vars_in_complex_term(&(t)-1, &(t), NULL, t0, stt PASS_REGS));
    ER(out = vars_in_complex_term(&(t)-1, &(t), TR, out, stt PASS_REGS));
    close_stack(stt);
    reset_trail(stt->tr0 PASS_REGS);
  }
  return out;
}

/** @pred  term_variables(? _Term_, - _Variables_) is iso

    Unify  _Variables_ with the list of all variables of term
    _Term_.  The variables occur in the order of their first
    appearance when traversing the term depth-first, left-to-right.

*/
static Int term_variables(USES_REGS1) /* variables in term t		 */
{
  Term out;
  Ystack_t stt_, *stt = &stt_;

  Term t;
  init_stack(stt, 0);
  if (!Yap_IsListOrPartialListTerm(Deref(ARG2))) {
    Yap_ThrowError(TYPE_ERROR_LIST, ARG2, "term_variables/2");
    return false;
  }
reset:

  t = Deref(ARG1);

  ER((out = vars_in_complex_term(&(t)-1, &(t), TR, TermNil, stt PASS_REGS)));
  close_stack(stt);
  reset_trail(stt->tr0 PASS_REGS);
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

static Term attvars_in_complex_term(CELL *pt0_, CELL *pt0_end_, Term inp,
                                    Ystack_t *stt USES_REGS) {

  CELL output = inp;
  COPY(pt0_[1]);

#include "term_visit.h"
  if (!IS_VISIT_MARKER(*ptd0) && IsAttVar(ptd0)) {
    if (HR + 1024 > ASP) {
      LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
      return 0;
    }
    output = MkPairTerm((CELL)ptd0, output);
    /* store the terms to visit */
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
      LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
      return 0;
    }
    mBind_And_Trail(ptd0, TermFoundVar);
    // ptd0 = (CELL*)RepAttVar(ptd0);
    push_sub_term(stt, d0, ptd0, pt0, pt0_end);
    if (stt->pt + 32 >= stt->max) {
      LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
      return 0;
    }
    pt0 = ptd0 - 1;
    pt0_end = ptd0 + 2;
    d0 = AbsAppl(ptd0);
    goto loop;
  }
  END_WALK();
  // no more variables to be found
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
  Ystack_t stt_, *stt = &stt_;

  Term out, t;
  init_stack(stt, 0);
  {
  reset:
    t = Deref(ARG1);
    if (IsPrimitiveTerm(t)) {
      close_stack(stt);
      return Yap_unify(TermNil, ARG2);
    } else {
      ER((out = attvars_in_complex_term(&(t)-1, &(t), TermNil, stt PASS_REGS)));
      close_stack(stt);
      reset_trail(stt->tr0 PASS_REGS);
    }
    return Yap_unify(ARG2, out);
  }
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

static int bind_vars_in_complex_term(CELL *pt0_, CELL *pt0_end_,
                                     Ystack_t *stt USES_REGS) {

  COPY(pt0_[1]);
#include "term_visit.h"
  mBind_And_Trail(ptd0, TermFoundVar);
  if ((tr_fr_ptr)LOCAL_TrailTop - TR < 1024) {
    LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
    return 0;
  }
  continue;
  END_WALK();

  return 0;
}

/** @pred  new_variables_in_term(+_CurrentVariables_, ? _Term_, -_Variables_)

    Unify  _Variables_ with the list of all variables of term
    _Term_ that do not occur in _CurrentVariables_. That is:

    `Variables = vars(Term) - CurrentVariables`

    The variables occur in
the order of their first appearance when traversing the term depth-first,
left-to-right.


*/
static Int
p_new_variables_in_term(USES_REGS1) /* variables within term t		 */
{
  Term out;
  Ystack_t stt_, *stt = &stt_;
  Term t, vs0;
  init_stack(stt, 0);

reset:
  t = Deref(ARG2);
  vs0 = Deref(ARG1);
  if (IsPrimitiveTerm(t)) {
    out = TermNil;
  } else {
    ER(bind_vars_in_complex_term(&(vs0)-1, &(vs0), stt PASS_REGS));
    reset_stack(stt);
    ER((out = vars_in_complex_term(&(t)-1, &(t), stt->tr0, TermNil,
                                   stt PASS_REGS)));
    reset_stack(stt);
    reset_trail(stt->tr0 PASS_REGS);
  }
  close_stack(stt);

  return Yap_unify(ARG3, out);
}

#define FOUND_VAR                                                              \
  if (d0 == TermFoundVar) {                                                    \
    /* leave an empty slot to fill in later */                                 \
    if (HR + 1024 > ASP) {                                                     \
      LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;                                 \
      return 0;                                                                \
    }                                                                          \
    HR[1] = AbsPair(HR + 2);                                                   \
    HR += 2;                                                                   \
    HR[-2] = (CELL)ptd0;                                                       \
    mTrailedMaBind(ptd0, TermNil);                                             \
  }

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE                                                     \
  {}
#define ATOMIC_HOOK_CODE FOUND_VAR

#define FOUND_VAR_AGAIN_AND_ADD                                                \
  if (d0 == TermFoundVar) {                                                    \
    if (HR > ASP - 1024) {                                                     \
      LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;                                 \
      return 0;                                                                \
    }                                                                          \
    if (end == NULL) {                                                         \
      first = AbsPair(HR);                                                     \
    } else {                                                                   \
      end[0] = AbsPair(HR);                                                    \
    }                                                                          \
    HR[0] = (CELL)ptd0;                                                        \
    end = HR + 1;                                                              \
    HR += 2;                                                                   \
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {                                \
      /* Trail overflow */                                                     \
      LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;                                 \
      return 0;                                                                \
    }                                                                          \
    mTrailedMaBind(ptd0, TermRefoundVar);                                      \
    continue;                                                                  \
  }

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE                                                     \
  {}
#define ATOMIC_HOOK_CODE FOUND_VAR_AGAIN_AND_ADD

    static Term intersection_vars_in_complex_term(CELL *pt0_, CELL *pt0_end_,
                                                Ystack_t *stt USES_REGS) {
    CELL first = AbsPair(HR);
    CELL *end;

    COPY(pt0_[1]);
    #include "term_visit.h"
    continue;
    END_WALK();
    close_stack(stt);
    if (HR != stt->hlow) {
        HR[-1] = TermNil;
        return first;
    } else {
        return TermNil;
    }
    }

    /** @pred  variables_in_both_terms(+_CurrentVariables_, ? _Term_,
    -_Variables_)

        Unify _Variables_ with the list of all variables of term _Term_
        that *also* occur in _CurrentVariables_.  The variables occur in
        the order of their first appearance when traversing the term
        depth-first, left-to-right.

        This predicate performs the opposite of new_variables_in_term/3.

    */
    static Int p_variables_in_both_terms(USES_REGS1) /* variables within term t */
    {
    Term out, t, inp;
    Ystack_t stt_, *stt = &stt_;

    if (!init_stack(stt, 0)) {
        LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
        return 0;
    }

    reset:
    t = Deref(ARG2);
    inp = Deref(ARG1);
    if (IsPrimitiveTerm(t))
        out = TermNil;
    else {
        ER(bind_vars_in_complex_term(&inp - 1, &inp, stt PASS_REGS));
        reset_stack(stt);
        ER((out = intersection_vars_in_complex_term(&(t)-1, &(t), stt PASS_REGS)));
    }
    close_stack(stt);
    reset_trail(stt->tr0);

    return Yap_unify(ARG3, out);
    }

/* variables within term t		 */
static Int free_variables_in_term(USES_REGS1) {
  Term out;
  Term t;
  Term bounds;

  Ystack_t stt_, *stt = &stt_;
  init_stack(stt, 0);
reset:

  t = Deref(ARG1);
  bounds = TermNil;

  while (!IsVarTerm(t) && IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorHat) {
      bounds = MkPairTerm(ArgOfTerm(1, t), bounds);
      t = ArgOfTerm(2, t);
    } else if (f == FunctorModule && IsAtomTerm(ArgOfTerm(1, t))) {
      t = ArgOfTerm(2, t);
    } else if (f == FunctorCall) {
      t = ArgOfTerm(1, t);
    } else if (f == FunctorExecuteInMod) {
      t = ArgOfTerm(1, t);
    } else {
      break;
    }
  }

  if (IsPrimitiveTerm(t)) {
    out = TermNil;
  } else {
    ER(vars_in_complex_term(&bounds - 1, &bounds, stt->tr0, TermNil,
                            stt PASS_REGS));
 
    ER((out = vars_in_complex_term(&(t)-1, &(t), stt->tr0, TermNil,
                                   stt PASS_REGS)));
  }

  reset_trail(stt->tr0 PASS_REGS);
  close_stack(stt);

  return Yap_unify(ARG2, t) && Yap_unify(ARG3, out);
}
    
#define RENUMBER_VAR()                                                         \
  if (f == FunctorDollarVar) {                                                 \
    if (show_singletons && ptd1[1] == MkIntTerm(-1)) {                         \
      Term d0 = MkIntTerm(vno++);                                              \
      MaBind(ptd1 + 1, d0);                                                    \
    }                                                                          \
  }

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE

#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE RENUMBER_VAR()
#define ATOMIC_HOOK_CODE                                                       \
  {}

static Term numbervars_in_complex_term(CELL *pt0_, CELL *pt0_end_, size_t vno,
                                       bool show_singletons,
                                       Ystack_t *stt USES_REGS) {
  COPY(pt0_[1]);

#include "term_visit.h"

  /* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
    LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
    return 0;
  }
  if (HR + 1024 > ASP) {
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
    return 0;
  }
  Term o = AbsAppl(HR);
  HR[0] = (CELL)FunctorDollarVar;
  if (show_singletons)
    HR[1] = MkIntTerm(-1);
  else
    HR[1] = MkIntTerm(vno++);
  HR += 2;
  YapBind(ptd0, o);

  continue;
  END_WALK();
  return vno;
}

/** numbervariables in term t         */
size_t Yap_NumberVars(Term t, size_t numbv, bool handle_singles USES_REGS) {
  Ystack_t stt_, *stt = &stt_;
  init_stack(stt, 0);
  if (IsPrimitiveTerm(t)) {
    return numbv;
  }
  Term vt = Deref(t);
  size_t rc = numbervars_in_complex_term(&vt - 1, &vt, numbv, handle_singles,
                                         stt PASS_REGS);
  close_stack(stt);
  return rc;
}

/** @pred  numbervars( _T_,+ _N1_,- _Nn_)

    Instantiates each variable in term  _T_ to a term of the form:
    `$VAR( _I_)`, with  _I_ increasing from  _N1_ to  _Nn_.
*/
static Int p_numbervars(USES_REGS1) {
  Ystack_t stt_, *stt = &stt_;
  bool handle_singles = false;

  Term vt, t;
  size_t out;
  if (!init_stack(stt, 0)) {
    LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
    return 0;
  }
reset:
  stt->hlow = HR;
     stt->tr0 = TR;
  t = Deref(ARG1);
  vt = Deref(ARG2);
  if (IsVarTerm(vt)) {
    close_stack(stt);
    Yap_Error(INSTANTIATION_ERROR, vt, "numbervars/3");
    return false;
  }
  if (!IsIntegerTerm(vt)) {
    close_stack(stt);
    Yap_Error(TYPE_ERROR_INTEGER, vt, "numbervars/3");
    return (false);
  }
  Int numbv = IntegerOfTerm(vt);
  if (IsPrimitiveTerm(t)) {
    close_stack(stt);
    return Yap_unify(ARG3, MkIntegerTerm(numbv));
  }
    ER(out = numbervars_in_complex_term(&t - 1, &t, numbv, handle_singles,
                                        stt PASS_REGS));

    close_stack(stt);
    return Yap_unify(ARG3, MkIntegerTerm(out));
  }

/** @pred  singleton_vs_numbervars( _T_,+ _N1_,- _Nn_)

    Instantiates each variable in term  _T_ to a term of the form:
    `$VAR( _I_)`, with  _I_ increasing from  _N1_ to  _Nn_.
*/

  static Int singleton_vs_numbervars(USES_REGS1) {
    Ystack_t stt_, *stt = &stt_;

    Term vt, t;
    Int out;
    if (!init_stack(stt, 0)) {
      LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
      return 0;
    }
    Functor fdv = FunctorDollarVar;
    FunctorDollarVar = FunctorHiddenVar;
    reset:
    stt->hlow = HR;
    stt->tr0 = TR;
    t = Deref(ARG1);
    vt = Deref(ARG2);
    if (IsVarTerm(vt)) {
      close_stack(stt);
      FunctorDollarVar = fdv;
      Yap_Error(INSTANTIATION_ERROR, vt, "numbervars/3");
      return false;
    }
    if (!IsIntegerTerm(vt)) {
      close_stack(stt);
      FunctorDollarVar = fdv;
      Yap_Error(TYPE_ERROR_INTEGER, vt, "numbervars/3");
      return (false);
    }
    size_t numbv = IntegerOfTerm(vt);
    if (IsPrimitiveTerm(t)) {
      close_stack(stt);
      FunctorDollarVar = fdv;
      return Yap_unify(ARG3, MkIntegerTerm(numbv));
    }
    ER(out = numbervars_in_complex_term(&t - 1, &t, numbv, true,
                                        stt PASS_REGS));
    close_stack(stt);
      FunctorDollarVar = fdv;
    return Yap_unify(ARG3, MkIntegerTerm(out));
  }


  #define HARD_RENUMBER_VAR()                                                         \
  if (f == FunctorDollarVar) {                                                 \
    if (show_singletons && ptd1[1] == MkIntTerm(-1)) {                         \
      Term d0 = MkIntTerm(vno++);                                              \
      TrailedMaBind(ptd1 + 1, d0);                                                    \
    }                                                                          \
  }

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE
#undef ATOMIC_HOOK_CODE

#define LIST_HOOK_CODE                                                         \
  {}
#define COMPOUND_HOOK_CODE HARD_RENUMBER_VAR()
#define ATOMIC_HOOK_CODE                                                       \
  {}

  
static Term hard_numbervars_in_complex_term(CELL *pt0_, CELL *pt0_end_, size_t vno,
                                       bool show_singletons,
                                       Ystack_t *stt USES_REGS) {
  COPY(pt0_[1]);

#include "term_visit.h"

  /* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
    LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
    return 0;
  }
  if (HR + 1024 > ASP) {
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
    return 0;
  }
  Term o = AbsAppl(HR);
  HR[0] = (CELL)FunctorDollarVar;
  if (show_singletons)
    HR[1] = MkIntTerm(-1);
  else
    HR[1] = MkIntTerm(vno++);
  HR += 2;
  mBind_And_Trail(ptd0, o);

  continue;
  END_WALK();
  return vno;
}

/** numbervariables in term t         */
size_t Yap_HardNumberVars(Term t, size_t numbv, bool handle_singles USES_REGS) {
  Ystack_t stt_, *stt = &stt_;
  init_stack(stt, 0);
  if (IsPrimitiveTerm(t)) {
    return numbv;
  }
  HB = HR;
  Term vt = Deref(t);
  size_t rc = hard_numbervars_in_complex_term(&vt - 1, &vt, numbv, handle_singles,
                                         stt PASS_REGS);
  close_stack(stt);
  HB = B->cp_h;
  return rc;
}

  void Yap_InitTermCPreds(void) {
    Yap_InitCPred("term_variables", 2, term_variables, 0);
    Yap_InitCPred("term_variables", 3, term_variables3, 0);
    Yap_InitCPred("variables_in_term", 3, variables_in_term, 0);

    Yap_InitCPred("$free_variables_in_term", 3, free_variables_in_term, 0);
    Yap_InitCPred("free_variables_in_term", 3, free_variables_in_term, 0);

    Yap_InitCPred("term_attvars", 2, term_attvars, 0);

    CurrentModule = TERMS_MODULE;
    Yap_InitCPred("variable_in_term", 2, variable_in_term, 0);
    Yap_InitCPred("variables_in_both_terms", 3, p_variables_in_both_terms, 0);
    Yap_InitCPred("new_variables_in_term", 3, p_new_variables_in_term, 0);
    CurrentModule = PROLOG_MODULE;

    Yap_InitCPred("ground", 1, ground, SafePredFlag);
    Yap_InitCPred("cyclic_term", 1, cyclic_term, SafePredFlag);

    Yap_InitCPred("numbervars", 3, p_numbervars, 0);
    Yap_InitCPred("$singleton_vs_numbervars", 3, singleton_vs_numbervars, 0);
  }

#endif

  ///@}

  
