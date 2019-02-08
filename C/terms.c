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

#define debug_pop_text_stack(l) [ if (to_visit != to_visit0) printf("%d\n",__LINE__); pop_text_stack(l) }

#include "attvar.h"
#include "yapio.h"
#ifdef HAVE_STRING_H
#include "string.h"
#endif

#define Malloc malloc
#define Realloc realloc


static int expand_vts(int args USES_REGS) {
  UInt expand = LOCAL_Error_Size;
  yap_error_number yap_errno = LOCAL_Error_TYPE;

  LOCAL_Error_Size = 0;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  if (yap_errno == RESOURCE_ERROR_TRAIL) {
    /* Trail overflow */
    if (!Yap_growtrail(expand, false)) {
      return false;
    }
  } else if (yap_errno == RESOURCE_ERROR_AUXILIARY_STACK) {
    /* Aux space overflow */
    if (expand > 4 * 1024 * 1024)
      expand = 4 * 1024 * 1024;
    if (!Yap_ExpandPreAllocCodeSpace(expand, NULL, true)) {
      return false;
    }
  } else {
    if (!Yap_gcl(expand, 3, ENV, gc_P(P, CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, "in term_variables");
      return false;
    }
  }
  return true;
}

static inline void clean_tr(tr_fr_ptr TR0 USES_REGS) {
  tr_fr_ptr pt0 = TR;
  while (pt0 != TR0) {
    Term p = TrailTerm(--pt0);
    if (IsApplTerm(p)) {
      CELL *pt = RepAppl(p);
#ifdef FROZEN_STACKS
      pt[0] = TrailVal(pt0);
#else
      pt[0] = TrailTerm(pt0 - 1);
      pt0--;
#endif /* FROZEN_STACKS */
    } else {
      RESET_VARIABLE(p);
    }
  }
  TR = TR0;
}


typedef struct {
  Term old_var;
  Term new_var;
} * vcell;

typedef struct non_single_struct_t {
  CELL *ptd0;
  CELL d0;
  CELL *pt0, *pt0_end, *ptf;
} non_singletons_t;

#define WALK_COMPLEX_TERM__(LIST0, STRUCT0, PRIMI0)                            \
                                                                               \
  struct non_single_struct_t *to_visit = Malloc(                               \
                                 1024 * sizeof(struct non_single_struct_t)),   \
                             *to_visit0 = to_visit,                            \
                             *to_visit_max = to_visit + 1024;                  \
                                                                               \
  while (to_visit >= to_visit0) {					\
     CELL d0;                                                          \
     CELL *ptd0;                                                       \
  restart:\
while (pt0 < pt0_end) {							\
    ++pt0;                                                                     \
    ptd0 = pt0;                                                                \
    d0 = *ptd0;                                                                \
  list_loop:                                                                   \
  /*fprintf(stderr, "%ld at %s\n", to_visit - to_visit0, __FUNCTION__);*/ \
    deref_head(d0, var_in_term_unk);                                           \
  var_in_term_nvar : {                                                         \
    if (IsPairTerm(d0)) {                                                      \
      if (to_visit + 32 >= to_visit_max) {                                     \
        goto aux_overflow;                                                     \
      }                                                                        \
      ptd0 = RepPair(d0);                                                      \
      d0 = ptd0[0];                                                            \
      LIST0;                                                                   \
      if (d0 == TermFreeTerm)                                                  \
        goto restart;                                                          \
      to_visit->pt0 = pt0;                                                     \
      to_visit->pt0_end = pt0_end;                                             \
      to_visit->ptd0 = ptd0;                                                   \
      to_visit->d0 = d0;                                                       \
      to_visit++;                                                              \
      *ptd0 = TermFreeTerm; \
      pt0 = ptd0;							\
      pt0_end = pt0 + 1;                                                       \
      goto list_loop;                                                          \
    } else if (IsApplTerm(d0)) {                                               \
      register Functor f;                                                      \
      /* store the terms to visit */                                           \
      ptd0 = RepAppl(d0);                                                      \
      f = (Functor)(d0 = *ptd0);                                               \
                                                                               \
      if (to_visit + 32 >= to_visit_max) {                                     \
        goto aux_overflow;                                                     \
      }                                                                        \
      STRUCT0;                                                                 \
      if (IsExtensionFunctor(f) || f == FunctorDollarVar || IsAtomTerm((CELL)f)) {                      \
                                                                               \
        continue;                                                          \
      }                                                                        \
      to_visit->pt0 = pt0;                                                     \
      to_visit->pt0_end = pt0_end;                                             \
      to_visit->ptd0 = ptd0;                                                   \
      to_visit->d0 = d0;                                                       \
      to_visit++;                                                              \
                                                                               \
      *ptd0 = TermNil;                                                         \
      Term d1 = ArityOfFunctor(f);                                             \
      pt0 = ptd0;                                                              \
      pt0_end = ptd0 + d1;                                                     \
      continue;                                                                \
    } else {                                                                   \
      PRIMI0;                                                                  \
      continue;                                                                \
    }                                                                          \
    derefa_body(d0, ptd0, var_in_term_unk, var_in_term_nvar);

#define WALK_COMPLEX_TERM() WALK_COMPLEX_TERM__({}, {}, {})

#define END_WALK() \
  }\
    }\
  /* Do we still have compound terms to visit */ \
    to_visit--; \
  if (to_visit >= to_visit0) {\
  pt0 = to_visit->pt0;				\
    pt0_end = to_visit->pt0_end;\
    *to_visit->ptd0 = to_visit->d0;		\
  }\
  }\
  pop_text_stack(lvl);

#define def_aux_overflow()                                                     \
  aux_overflow : {                                                             \
    size_t d1 = to_visit - to_visit0;                                          \
    size_t d2 = to_visit_max - to_visit0;				\
    to_visit0 =                                                                \
        Realloc(to_visit0, (d2 + 128) * sizeof(struct non_single_struct_t));   \
    to_visit = to_visit0 + d1;                                                 \
    to_visit_max = to_visit0 + (d2 + 128);                                     \
    pt0--;                                                                     \
  }                                                                            \
  goto restart;

#define def_trail_overflow()                                                   \
  trail_overflow : {                                                           \
    LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;                                   \
    LOCAL_Error_Size = (TR - TR0) * sizeof(tr_fr_ptr *);                       \
    clean_tr(TR0 PASS_REGS);                                                   \
    HR = InitialH;                                                             \
    pop_text_stack(lvl);                                                       \
    return 0L;                                                                 \
  }

#define def_global_overflow()                                                  \
  global_overflow : {                                                          \
    while (to_visit > to_visit0) {                                             \
      to_visit--;                                                              \
      CELL *ptd0 = to_visit->ptd0;                                             \
      *ptd0 = to_visit->d0;                                                    \
    }                                                                          \
    pop_text_stack(lvl);                                                       \
    clean_tr(TR0 PASS_REGS);                                                   \
    HR = InitialH;                                                             \
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;                                   \
    LOCAL_Error_Size = (ASP - HR) * sizeof(CELL);                              \
    return false;                                                              \
  }

#define CYC_LIST                                                               \
  if (d0 == TermFreeTerm) {						\
    /*fprintf(stderr,"+%ld at %s\n", to_visit-to_visit0, __FUNCTION__);*/ \
while (to_visit > to_visit0) {                                             \
      to_visit--;                                                              \
      to_visit->ptd0[0] =                                             \
       to_visit->d0;                                                    \
    }                                                                          \
 pop_text_stack(lvl);  /*fprintf(stderr,"<%ld at %s\n", to_visit-to_visit0, __FUNCTION__);*/ \
    return true;                                                               \
  }

#define CYC_APPL                                                               \
      if (IsAtomTerm((CELL)f)) {                                         \
    while (to_visit > to_visit0) {                                             \
      to_visit--;                            \
       to_visit->ptd0[0] =                                             \
       to_visit->d0;                                              \
    }                                                                     \
    /*fprintf(stderr,"<%ld at %s\n", to_visit-to_visit0, __FUNCTION__);*/ \
     return true;						      \
  }

/**
   @brief routine to locate all variables in a term, and its applications */

static Term cyclic_complex_term(register CELL *pt0,
                                register CELL *pt0_end USES_REGS) {

  int lvl = push_text_stack();                                                 \
  WALK_COMPLEX_TERM__(CYC_LIST, CYC_APPL, {});
  /* leave an empty slot to fill in later */
  END_WALK();


  return false;

  def_aux_overflow();
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
    the representation of a term `X` that obeys the equation `X=[X]`
    term has a loop from the list to its head.


*/
static Int cyclic_term(USES_REGS1) /* cyclic_term(+T)		 */
{
  return Yap_IsCyclicTerm(Deref(ARG1));
}

/**
   @brief routine to locate all variables in a term, and its applications */

static bool ground_complex_term(register CELL *pt0,
                                register CELL *pt0_end USES_REGS) {

  int lvl = push_text_stack();                                                 \
  WALK_COMPLEX_TERM();
  /* leave an empty slot to fill in later */
  while (to_visit > to_visit0) {
    to_visit--;

    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
  }
  pop_text_stack(lvl);
  return false;

  END_WALK();
  /* Do we still have compound terms to visit */

  pop_text_stack(lvl);

  return true;

  def_aux_overflow();
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

static Int var_in_complex_term(register CELL *pt0, register CELL *pt0_end,
                               Term v USES_REGS) {

  int lvl = push_text_stack();				\
  WALK_COMPLEX_TERM();

  if ((CELL)ptd0 == v) { /* we found it */
                       /* Do we still have compound terms to visit */
    while (to_visit > to_visit0) {
      to_visit--;

      CELL *ptd0 = to_visit->ptd0;
      *ptd0 = to_visit->d0;
    }
    pop_text_stack(lvl);
    return true;
  }
  goto restart;
  END_WALK();

  if (to_visit > to_visit0) {
    to_visit--;

    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
  }
  pop_text_stack(lvl);
  return false;

  def_aux_overflow();
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

/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static Term vars_in_complex_term(register CELL *pt0, register CELL *pt0_end,
                                 Term inp USES_REGS) {

  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  CELL output = AbsPair(HR);
	  int lvl = push_text_stack();
	  
  push_text_stack();				\
  WALK_COMPLEX_TERM();
  /* do or pt2 are unbound  */
  *ptd0 = TermNil;
  /* leave an empty slot to fill in later */
  if (HR + 1024 > ASP) {
    goto global_overflow;
  }
  HR[1] = AbsPair(HR + 2);
  HR += 2;
  HR[-2] = (CELL)ptd0;
  /* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
    if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), true)) {
      goto trail_overflow;
    }
  }
  TrailTerm(TR++) = (CELL)ptd0;

  END_WALK();

  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);

  if (HR != InitialH) {
    /* close the list */
    Term t2 = Deref(inp);
    if (IsVarTerm(t2)) {
      RESET_VARIABLE(HR - 1);
      Yap_unify((CELL)(HR - 1), inp);
    } else {
      HR[-1] = t2; /* don't need to trail */
    }
    return (output);
  } else {
    return (inp);
  }
  def_trail_overflow();

  def_aux_overflow();

  def_global_overflow();
}

/**
 * @pred variables_in_term( +_T_, +_SetOfVariables_, +_ExtendedSetOfVariables_ )
 *
 * _SetOfVariables_ must be a list of unbound variables. If so,
 * _ExtendedSetOfVariables_ will include all te variables in the union
 * of `vars(_T_)` and _SetOfVariables_.
 */
static Int variables_in_term(USES_REGS1) /* variables in term t		 */
{
  Term out, inp;
  int count;

restart:
  count = 0;
  inp = Deref(ARG2);
  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      *ptr = TermFoundVar;
      TrailTerm(TR++) = t;
      count++;
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
        clean_tr(TR - count PASS_REGS);
        if (!Yap_growtrail(count * sizeof(tr_fr_ptr *), false)) {
          return false;
        }
        goto restart;
      }
    }
    inp = TailOfTerm(inp);
  }
  do {
    Term t = Deref(ARG1);
    out = vars_in_complex_term(&(t)-1, &(t), ARG2 PASS_REGS);
    if (out == 0L) {
      if (!expand_vts(3 PASS_REGS))
        return false;
    }
  } while (out == 0L);
  clean_tr(TR - count PASS_REGS);
  return Yap_unify(ARG3, out);
}

/** @pred  term_variables(? _Term_, - _Variables_, +_ExternalVars_) is iso



    Unify the difference list between _Variables_ and _ExternaVars_
    with the list of all variables of term _Term_.  The variables
    occur in the order of their first appearance when traversing the
    term depth-first, left-to-right.


*/
static Int p_term_variables3(USES_REGS1) /* variables in term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG1);
    if (IsVarTerm(t)) {
      Term out = Yap_MkNewPairTerm();
      return Yap_unify(t, HeadOfTerm(out)) &&
             Yap_unify(ARG3, TailOfTerm(out)) && Yap_unify(out, ARG2);
    } else if (IsPrimitiveTerm(t)) {
      return Yap_unify(ARG2, ARG3);
    } else {
      out = vars_in_complex_term(&(t)-1, &(t), ARG3 PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts(3 PASS_REGS))
        return false;
    }
  } while (out == 0L);

  return Yap_unify(ARG2, out);
}

/**
 * Exports a nil-terminated list with all the variables in a term.
 * @param[t] the term
 * @param[arity] the arity of the calling predicate (required for exact garbage
 * collection).
 * @param[USES_REGS] threading
 */
Term Yap_TermVariables(
    Term t, UInt arity USES_REGS) /* variables in term t		 */
{
  Term out;

  do {
    t = Deref(t);
    if (IsVarTerm(t)) {
      return MkPairTerm(t, TermNil);
    } else if (IsPrimitiveTerm(t)) {
      return TermNil;
    } else {
      out = vars_in_complex_term(&(t)-1, &(t), TermNil PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts(arity PASS_REGS))
        return false;
    }
  } while (out == 0L);
  return out;
}

/** @pred  term_variables(? _Term_, - _Variables_) is iso



    Unify  _Variables_ with the list of all variables of term
    _Term_.  The variables occur in the order of their first
    appearance when traversing the term depth-first, left-to-right.


*/
static Int p_term_variables(USES_REGS1) /* variables in term t		 */
{
  Term out;

  if (!Yap_IsListOrPartialListTerm(ARG2)) {
    Yap_Error(TYPE_ERROR_LIST, ARG2, "term_variables/2");
    return false;
  }

  do {
    Term t = Deref(ARG1);

    out = vars_in_complex_term(&(t)-1, &(t), TermNil PASS_REGS);
    if (out == 0L) {
      if (!expand_vts(3 PASS_REGS))
        return false;
    }
  } while (out == 0L);
  return Yap_unify(ARG2, out);
}

/** routine to locate attributed variables */

typedef struct att_rec {
  CELL *beg, *end;
  CELL oval;
} att_rec_t;

static Term attvars_in_complex_term(register CELL *pt0, register CELL *pt0_end,
                                    Term inp USES_REGS) {
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  CELL output = inp;
  int lvl = push_text_stack();				\
  
  WALK_COMPLEX_TERM();
  if (IsAttVar(ptd0)) {
    /* do or pt2 are unbound  */
    attvar_record *a0 = RepAttVar(ptd0);
    if (a0->AttFunc == (Functor)TermNil)
      goto restart;
    /* leave an empty slot to fill in later */
    if (HR + 1024 > ASP) {
      goto global_overflow;
    }
    output = MkPairTerm( (CELL) & (a0->Done), output);
    /* store the terms to visit */
    if (to_visit + 32 >= to_visit_max) {
      goto aux_overflow;
    }
    ptd0 = (CELL *)a0;
    to_visit->pt0 = pt0;
    to_visit->pt0_end = pt0_end;
    to_visit->d0 = *ptd0;
    to_visit->ptd0 = ptd0;
    to_visit++;
    *ptd0 = TermNil;
    pt0_end = &RepAttVar(ptd0)->Atts;
    pt0 = pt0_end - 1;
  }

  END_WALK();

  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  if (HR != InitialH) {
    /* close the list */
    Term t2 = Deref(inp);
    if (IsVarTerm(t2)) {
      RESET_VARIABLE(HR - 1);
      Yap_unify((CELL)(HR - 1), t2);
    } else {
      HR[-1] = t2; /* don't need to trail */
    }

  }
  /*fprintf(stderr,"<%ld at %s\n", to_visit-to_visit0, __FUNCTION__)*/;
   return (output);

  def_aux_overflow();
  def_global_overflow();
}

/** @pred term_attvars(+ _Term_,- _AttVars_)


    _AttVars_ is a list of all attributed variables in  _Term_ and
    its attributes. I.e., term_attvars/2 works recursively through
    attributes.  This predicate is Cycle-safe.


*/
static Int p_term_attvars(USES_REGS1) /* variables in term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG1);
    if (IsPrimitiveTerm(t)) {
      return Yap_unify(TermNil, ARG2);
    } else {
      out = attvars_in_complex_term(&(t)-1, &(t), TermNil PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts(3 PASS_REGS))
        return false;
    }
  } while (out == 0L);
  return Yap_unify(ARG2, out);
}

/** @brief output the difference between variables in _T_ and variables in some
 * list.
 */
static Term new_vars_in_complex_term(register CELL *pt0, register CELL *pt0_end,
                                     Term inp USES_REGS) {
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  int lvl = push_text_stack();				\
  HB=ASP;
  CELL output = TermNil;
  {
    while (!IsVarTerm(inp) && IsPairTerm(inp)) {
      Term t = HeadOfTerm(inp);
      if (IsVarTerm(t)) {
	YapBind( VarOfTerm(t), TermFoundVar );
        if ((tr_fr_ptr)LOCAL_TrailTop - TR < 1024) {

          if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), true)) {
            goto trail_overflow;
          }
	  pop_text_stack( lvl );
        }
      }
      inp = TailOfTerm(inp);
    }
  }
  WALK_COMPLEX_TERM();
  output = MkPairTerm((CELL)ptd0,output);
	YapBind( ptd0, TermFoundVar );
        if ((tr_fr_ptr)LOCAL_TrailTop - TR < 1024) {
          if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), true)) {
            goto trail_overflow;
          }
        }
  /* leave an empty slot to fill in later */
  if (HR + 1024 > ASP) {
    goto global_overflow;
  }
  END_WALK();
  
  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  HB = B->cp_h;
  return output;
  
  def_aux_overflow();

  def_trail_overflow();

  def_global_overflow();
}

/** @pred  new_variables_in_term(+_CurrentVariables_, ? _Term_, -_Variables_)



    Unify  _Variables_ with the list of all variables of term
    _Term_ that do not occur in _CurrentVariables_.  The variables occur in the
   order of their first appearance when traversing the term depth-first,
   left-to-right.


*/
static Int
p_new_variables_in_term(USES_REGS1) /* variables within term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG2);
    if (IsPrimitiveTerm(t))
      out = TermNil;
    else {
      out = new_vars_in_complex_term(&(t)-1, &(t), Deref(ARG1) PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts(3 PASS_REGS))
        return false;
    }
  } while (out == 0L);
  return Yap_unify(ARG3, out);
}

#define FOUND_VAR()                                                            \
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

static Term vars_within_complex_term(register CELL *pt0, register CELL *pt0_end,
                                     Term inp USES_REGS) {

  tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  CELL output = AbsPair(HR);
	  int lvl = push_text_stack();
	  
  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      *ptr = TermFoundVar;
      TrailTerm(TR++) = t;
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
        Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), true);
      }
    }
    inp = TailOfTerm(inp);
  }

  WALK_COMPLEX_TERM__({}, {}, FOUND_VAR());
  goto restart;
  END_WALK();

  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  if (HR != InitialH) {
    HR[-1] = TermNil;
    return output;
  } else {
    return TermNil;
  }

  def_aux_overflow();

  def_global_overflow();
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

  do {
    Term t = Deref(ARG2);
    if (IsPrimitiveTerm(t))
      out = TermNil;
    else {
      out = vars_within_complex_term(&(t)-1, &(t), Deref(ARG1) PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts(3 PASS_REGS))
        return false;
    }
  } while (out == 0L);
  return Yap_unify(ARG3, out);
}

static Term free_vars_in_complex_term(CELL *pt0, CELL *pt0_end,
                                      tr_fr_ptr TR0 USES_REGS) {
  Term o = TermNil;
  CELL *InitialH = HR;
  int lvl = push_text_stack();
  WALK_COMPLEX_TERM();
  /* do or pt2 are unbound  */
  *ptd0 = TermNil;
  /* leave an empty slot to fill in later */
  if (HR + 1024 > ASP) {
    o = TermNil;
    goto global_overflow;
  }
  HR[0] = (CELL)ptd0;
  HR[1] = o;
  o = AbsPair(HR);
  HR += 2;
  /* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
    if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), true)) {
      goto trail_overflow;
    }
  }
  TrailTerm(TR++) = (CELL)ptd0;
  END_WALK();


  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  return o;

  def_aux_overflow();

  def_trail_overflow();

  def_global_overflow();
}

static Term bind_vars_in_complex_term(CELL *pt0, CELL *pt0_end,
                                      tr_fr_ptr TR0 USES_REGS) {
  CELL *InitialH = HR;
	  int lvl = push_text_stack();
  WALK_COMPLEX_TERM();
  /* do or pt2 are unbound  */
  *ptd0 = TermFoundVar;
  /* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
    if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), true)) {
      while (to_visit > to_visit0) {
        to_visit--;
        CELL *ptd0 = to_visit->ptd0;
        *ptd0 = to_visit->d0;
      }
      goto trail_overflow;
    }
  }
  TrailTerm(TR++) = (CELL)ptd0;

  END_WALK();

  pop_text_stack(lvl);
  return TermNil;

  def_aux_overflow();

  def_trail_overflow();
}

static Int
p_free_variables_in_term(USES_REGS1) /* variables within term t		 */
{
  Term out;
  Term t, t0;
  Term found_module = 0L;

  do {
    tr_fr_ptr TR0 = TR;

    t = t0 = Deref(ARG1);
    while (!IsVarTerm(t) && IsApplTerm(t)) {
      Functor f = FunctorOfTerm(t);
      if (f == FunctorHat) {
        out = bind_vars_in_complex_term(RepAppl(t), RepAppl(t) + 1,
                                        TR0 PASS_REGS);
        if (out == 0L) {
          goto trail_overflow;
        }
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
      out = free_vars_in_complex_term(&(t)-1, &(t), TR0 PASS_REGS);
    }
    if (out == 0L) {
    trail_overflow:
      if (!expand_vts(3 PASS_REGS))
        return false;
    }
  } while (out == 0L);
  if (found_module && t != t0) {
    Term ts[2];
    ts[0] = found_module;
    ts[1] = t;
    t = Yap_MkApplTerm(FunctorModule, 2, ts);
  }
  return Yap_unify(ARG2, t) && Yap_unify(ARG3, out);
}

#define FOUND_VAR_AGAIN()                                                      \
  if (d0 == TermFoundVar) {                                                    \
    CELL *pt2 = pt0;                                                           \
    while (IsVarTerm(*pt2))                                                    \
      pt2 = (CELL *)(*pt2);                                                    \
    HR[1] = AbsPair(HR + 2);                                                   \
    HR[0] = (CELL)pt2;                                                         \
    HR += 2;                                                                   \
    *pt2 = TermRefoundVar;                                                     \
  }

static Term non_singletons_in_complex_term(CELL *pt0, CELL *pt0_end USES_REGS) {
  tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  HB = (CELL *)ASP;
  CELL output = AbsPair(HR);
  int lvl = push_text_stack();
  WALK_COMPLEX_TERM__({}, {}, FOUND_VAR_AGAIN());
  /* do or pt2 are unbound  */
  YapBind(ptd0,TermFoundVar);
  goto restart; 
  END_WALK();

  clean_tr(TR0 PASS_REGS);

  pop_text_stack(lvl);
  HB = (CELL*)B->cp_b;
  if (HR != InitialH) {
    /* close the list */
    HR[-1] = Deref(ARG2);
    return output;
  } else {
    return ARG2;
  }

  def_aux_overflow();
}

static Int p_non_singletons_in_term(
    USES_REGS1) /* non_singletons in term t		 */
{
  Term t;
  Term out;

  while (true) {
    t = Deref(ARG1);
    if (IsVarTerm(t)) {
      out = ARG2;
    } else if (IsPrimitiveTerm(t)) {
      out = ARG2;
    } else {
      out = non_singletons_in_complex_term(&(t)-1, &(t)PASS_REGS);
    }
    if (out != 0L) {
      return Yap_unify(ARG3, out);
    }
  }
}

static Term numbervar(Int me USES_REGS) {
  Term ts[1];
  ts[0] = MkIntegerTerm(me);
  return Yap_MkApplTerm(FunctorDollarVar, 1, ts);
}

static Term numbervar_singleton(USES_REGS1) {
  Term ts[1];
  ts[0] = MkIntegerTerm(-1);
  return Yap_MkApplTerm(FunctorDollarVar, 1, ts);
}

static void renumbervar(Term t, Int me USES_REGS) {
  Term *ts = RepAppl(t);
  ts[1] = MkIntegerTerm(me);
}

#define RENUMBER_SINGLES                                                       \
  if (singles ) {                                \
    renumbervar(d0, numbv++ PASS_REGS);                                        \
    goto restart;                                                              \
  }

static Int numbervars_in_complex_term(CELL *pt0, CELL *pt0_end, Int numbv,
                                      int singles USES_REGS) {

  tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
	  int lvl = push_text_stack();
	  
  WALK_COMPLEX_TERM__({}, {}, {});

  if (IsAttVar(pt0))
      continue;
  /* do or pt2 are unbound  */
  if (singles || 0)
    d0 = numbervar_singleton(PASS_REGS1);
  else
    d0 = numbervar(numbv++ PASS_REGS);
  /* leave an empty slot to fill in later */
  if (HR + 1024 > ASP) {
    goto global_overflow;
  }
  /* next make sure noone will see this as a variable again */
  YapBind(ptd0, d0);

  END_WALK();

  pop_text_stack(lvl);
  return numbv;

  def_aux_overflow();

  def_global_overflow();
  
}

Int Yap_NumberVars(Term inp, Int numbv,
                   bool handle_singles) /*
                                         * numbervariables in term t	 */
{
  CACHE_REGS
  Int out;
  Term t;

restart:
  t = Deref(inp);
  if (IsPrimitiveTerm(t)) {
    return numbv;
  } else {

    out = numbervars_in_complex_term(&(t)-1, &(t), numbv,
                                     handle_singles PASS_REGS);
  }
  if (out < numbv) {
    if (!expand_vts(3 PASS_REGS))
      return false;
    goto restart;
  }
  return out;
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
  if ((out = Yap_NumberVars(ARG1, IntegerOfTerm(t2), false)) < 0)
    return false;
  return Yap_unify(ARG3, MkIntegerTerm(out));
}

#define MAX_NUMBERED                                                           \
  if (FunctorOfTerm(d0) == FunctorDollarVar) {                                 \
    Term t1 = ArgOfTerm(1, d0);                                                \
    Int i;                                                                     \
    if (IsIntegerTerm(t1) && ((i = IntegerOfTerm(t1)) > *maxp))                \
      *maxp = i;                                                               \
    goto restart;                                                              \
  }

static int max_numbered_var(CELL *pt0, CELL *pt0_end, Int *maxp USES_REGS) {
	  int lvl = push_text_stack();
  WALK_COMPLEX_TERM__({}, MAX_NUMBERED, {});
  END_WALK();
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
  }

  prune(B PASS_REGS);
  pop_text_stack(lvl);
  return 0;

  def_aux_overflow();
}

static Int MaxNumberedVar(Term inp, UInt arity PASS_REGS) {
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

static Term BREAK_LOOP(Int ddep) {
  char buf[64];
  snprintf(buf, 63, "@^[" Int_FORMAT "]", ddep);
  return MkAtomTerm(Yap_LookupAtom(buf));
}

static Term UNFOLD_LOOP(Term t, Term *b) {
  Term os[2], o;
  os[0] = o = MkVarTerm();
  os[1] = t;
  Term ti = Yap_MkApplTerm(FunctorEq, 2, os);
  *b = MkPairTerm(ti, *b);

  return o;
}


typedef struct block_connector {
  Int me;          //> index in the array;
  Term source;    //> source; 
  CELL *copy;      //> copy;
  CELL header;     //> backup of first word of the source data;
  CELL reference;  //> term used to refer the copy.
} cl_connector;


static Int
create_entry(Term t, Int i, Int j, cl_connector *q, Int max)
{
  Term ref, h, *s, *ostart;
  bool pair = false;
  ssize_t n;
  // first time, create a new term
  ostart = HR;
  if (IsPairTerm(t)) {
    s = RepPair(t);
     n = 2;
    pair = true;
    ref = AbsPair(ostart);
  } else {
    h = (CELL)FunctorOfTerm(t);
    s = RepAppl(t);
    n = ArityOfFunctor(FunctorOfTerm(t));
    ref = AbsAppl(ostart);
    *ostart++ = s[0];
  }
   if (H0 > s) {
     return (s[0]-EndSpecials)/sizeof(CELL);
  }
  q[max].me = max;
  q[max].source = t;
  q[max].copy = ostart;
  q[max].header = s[0];
  q[max].reference = ref;
  s[0] = max*sizeof(CELL)+EndSpecials;
  HR += n;
  max++;
  return max;
}


Int cp_link(Term t,Int i, Int j, cl_connector *q, Int max, CELL *tailp)
{
  Int me;

  t = Deref(t);
  if (IsVarTerm(t) || IsPrimitiveTerm(t)) {
    q[i].copy[j] = t;
    return max;
  }
  if ((me = create_entry(t, i, j, q, max)) < max) {
    Term  ref = Deref(q[me].reference);

    if (IsVarTerm(ref)) {
      q[i].copy[j] = ref;
    } else {
      q[i].copy[j] = q[me].reference = UNFOLD_LOOP(ref, tailp);
    }
    return max;
  }
  return max+1;
}


Term Yap_BreakCycles(Term inp, UInt arity, Term *listp USES_REGS) {

  int lvl = push_text_stack();

  Term t = Deref(inp);
  ssize_t qsize = 2048, qlen=0;
  cl_connector *q = Malloc(qsize * sizeof(cl_connector));
  Term *s;
    Int i=0;
    qlen = 0;
  
  HB=HR;
  if (IsVarTerm(t) || IsPrimitiveTerm(t)) {
    return t;
  } else {
    // initialization
    qlen = cp_link(t, i++, 1, q, qlen, listp);
    while(i < qlen) {
      arity_t n, j;
      if (IsPairTerm( q[i].source )) {
	s = RepPair( q[i].source );
	n = 2;
	// fetch using header field.
	qlen =  cp_link(q[i].header, i, 0, q, qlen, listp);
	// fetch using standard access
	qlen =  cp_link(s[1], i, 1, q, qlen, listp);
      } else {
	s = RepAppl( q[i].source )+1;
	n = ArityOfFunctor((Functor)q[i].header);
      for (j = 0; j<n; j++) {
	qlen =  cp_link(s[j], i, j, q, qlen, listp);
      }
      }
      i++;
    }
  }

  for (i =0; i < qlen; i++) {
  if (IsPairTerm(t)) {

    RepPair(q[i].source)[0] = q[i].header;
  } else {

    RepAppl(q[i].source)[0] = q[i].header;
  }
  }
  pop_text_stack(lvl);


    HB = B->cp_h;
      return q[0].reference;
}

/** @pred  rational_term_to_tree(? _TI_,- _TF_, ?SubTerms, ?MoreSubterms)


    The term _TF_ is a forest representation (without cycles) for
    the Prolog term _TI_. The term _TF_ is the main term.  The
    difference list _SubTerms_-_MoreSubterms_ stores terms of the
    form _V=T_, where _V_ is a new variable occuring in _TF_, and
    _T_ is a copy of a sub-term from _TI_.


*/
static Int p_break_rational(USES_REGS1) {
  Term t = Deref(ARG1);
  Term l = Deref(ARG4);
  if (IsVarTerm(l))
    Yap_unify(l, MkVarTerm());
  return Yap_unify(Yap_BreakCycles(t, 4, &l PASS_REGS), ARG2) &&
         Yap_unify(l, ARG3) ;
}

void Yap_InitTermCPreds(void) {
  Yap_InitCPred("rational_term_to_tree", 4, p_break_rational, 0);
  Yap_InitCPred("term_variables", 2, p_term_variables, 0);
  Yap_InitCPred("term_variables", 3, p_term_variables3, 0);
  Yap_InitCPred("$variables_in_term", 3, variables_in_term, 0);

  Yap_InitCPred("$free_variables_in_term", 3, p_free_variables_in_term, 0);

  Yap_InitCPred("term_attvars", 2, p_term_attvars, 0);

  CurrentModule = TERMS_MODULE;
  Yap_InitCPred("variable_in_term", 2, variable_in_term, 0);
  Yap_InitCPred("variables_within_term", 3, p_variables_within_term, 0);
  Yap_InitCPred("new_variables_in_term", 3, p_new_variables_in_term, 0);
  CurrentModule = PROLOG_MODULE;

  Yap_InitCPred("$non_singletons_in_term", 3, p_non_singletons_in_term, 0);

  Yap_InitCPred("ground", 1, ground, SafePredFlag);
  Yap_InitCPred("cyclic_term", 1, cyclic_term, SafePredFlag);

    Yap_InitCPred("numbervars", 3, p_numbervars, 0);
  Yap_InitCPred("largest_numbervar", 2, largest_numbervar, 0);
}
