
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

//#define DEB_D0(S) printf("%s: d0=%lx ptd0=%p *ptd0=%lx\n",S,d0, ptd0, *ptd0)
#define DEB_D0(S) printf("%s: d0=%lx/ptd0=%p *ptd0=%d ss=%ld\n",S,d0, ptd0, *ptd0==d0,to_visit-to_visit0)
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

#define debug_pop_text_stack(l) [ if (to_visit != to_visit0) printf("%d\n",__LINE__); pop_text_stack(l) \
				  }

#include "attvar.h"
#include "yapio.h"
#ifdef HAVE_STRING_H
#include "string.h"
#endif

extern int cs[10];

int cs[10];

typedef struct {
  Term old_var;
  Term new_var;
} * vcell;

typedef struct non_single_struct_t {
  CELL *ptd0;
  CELL *pt0, *pt0_end, *ptf;
  Term oldv;
} non_singletons_t;

//#define  CELL *pt0, *pt0_end, *ptf;
//} non_singletons_t;

#define WALK_COMPLEX_TERM__(LIST0, STRUCT0, PRIMI0)			\
									\
  reset:								\
  lvl = push_text_stack();						\
  to_visit = to_visit0 = Malloc(auxsz);					\
  pt0 = pt0_; pt0_end = pt0_end_;					\
  to_visit = to_visit0;							\
  to_visit_max = to_visit +  auxsz/sizeof(struct non_single_struct_t);	\
									\
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {				\
    /* Trail overflow */						\
    goto trail_overflow;						\
  }									\
  if (HR + 1024 > ASP) {						\
    goto global_overflow;						\
  }									\
  while (to_visit >= to_visit0) {					\
  CELL d0;								\
  CELL *ptd0;								\
restart:								\
 while (pt0 < pt0_end) {						\
 ++pt0;									\
 ptd0 = pt0;\
 d0 = VISIT_UNMARK(*ptd0);\
list_loop:								\
 /*fprintf(stderr, "%ld at %s\n", to_visit - to_visit0, __FUNCTION__);*/ \
 mderef_head(d0, var_in_term_unk);					\
var_in_term_nvar : {							\
 if (IsPairTerm(d0)) {DEB_D0("entry=");					\
   if (to_visit + 32 >= to_visit_max) {					\
     goto aux_overflow;							\
   }									\
   ptd0 = RepPair(d0);							\
   d0 = VISIT_UNMARK(ptd0[0]);DEB_D0("head=");				\
   LIST0;								\
   if (IS_VISIT_MARKER(*ptd0))						\
     goto restart;							\
   to_visit->pt0 = pt0;							\
   to_visit->pt0_end = pt0_end;						\
   to_visit->ptd0 = ptd0;						\
   to_visit->oldv = d0; \
   *ptd0 = VISIT_MARK();						\
   to_visit++;								\
   pt0 = ptd0;								\
   pt0_end = pt0 + 1;							\
   goto list_loop;							\
 } else if (IsApplTerm(d0)) {						\
   register Functor f;							\
   /* store the terms to visit */					\
   ptd0 = RepAppl(d0);							\
   f = (Functor)(d0 = VISIT_UNMARK(*ptd0));				\
   if (IsExtensionFunctor(f)) {						\
     continue;								\
   }									\
									\
   if (to_visit + 32 >= to_visit_max) {					\
     goto aux_overflow;							\
   }									\
   STRUCT0;								\
   if (IS_VISIT_MARKER(*ptd0)) {					\
									\
     continue;								\
   }									\
   to_visit->pt0 = pt0;							\
   to_visit->pt0_end = pt0_end;						\
   to_visit->ptd0 = ptd0;						\
   to_visit->oldv = d0;							\
   to_visit++;								\
   *ptd0 = VISIT_MARK();						\
   Term d1 = ArityOfFunctor(f);						\
   pt0 = ptd0;								\
   pt0_end = ptd0 + d1;							\
   continue;								\
 } else {								\
  PRIMI0;		DEB_D0("cons=");						\
  continue;								\
 }									\
mderef_body(d0, ptd0, var_in_term_unk, var_in_term_nvar);		\
 {}									\

#define WALK_COMPLEX_TERM() WALK_COMPLEX_TERM__({}, {}, {})

#define END_WALK()					\
  }							\
    }							\
    /* Do we still have compound terms to visit */	\
    to_visit--;						\
    if (to_visit >= to_visit0) {			\
      pt0 = to_visit->pt0;				\
      pt0_end = to_visit->pt0_end;			\
      VUNMARK(to_visit->ptd0,to_visit->oldv);		\
    }							\
    }							\
    pop_text_stack(lvl);

#define def_aux_overflow()			\
  aux_overflow: {				\
    while (to_visit > to_visit0) {		\
      to_visit--;				\
      CELL *ptd0 = to_visit->ptd0;		\
      VUNMARK(ptd0,to_visit->oldv);		\
    }						\
    clean_tr(TR0 PASS_REGS);			\
    auxsz += auxsz;				\
    pop_text_stack(lvl);			\
    goto reset; }

#define def_trail_overflow()					\
  trail_overflow: {						\
    while (to_visit > to_visit0) {				\
      to_visit--;						\
      CELL *ptd0 = to_visit->ptd0;				\
      VUNMARK(ptd0,to_visit->oldv);				\
    }								\
    size_t expand = (TR - TR0) * sizeof(tr_fr_ptr *);		\
    clean_tr(TR0 PASS_REGS);					\
    HR = InitialH;						\
    pop_text_stack(lvl);					\
    /* Trail overflow */					\
    if (!Yap_growtrail(expand, false)) {			\
      Yap_ThrowError(RESOURCE_ERROR_TRAIL, TermNil, expand);	\
    }								\
    goto reset;							\
  }

#define def_global_overflow()						\
  global_overflow : {							\
    while (to_visit > to_visit0) {					\
      to_visit--;							\
      CELL *ptd0 = to_visit->ptd0;					\
      VUNMARK(ptd0,to_visit->oldv);					\
    }									\
    clean_tr(TR0 PASS_REGS);						\
    pop_text_stack(lvl);						\
    HR = InitialH;							\
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;				\
    size_t expand  = 0L;						\
    if (!Yap_gcl(expand, 3, ENV, gc_P(P, CP))) {			\
      Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, sizeof(CELL)*(HR-H0)); \
      return false;							\
    }									\
    goto reset;								\
  }

#define CYC_LIST				\
  if (IS_VISIT_MARKER(*ptd0)) {			\
    while (to_visit > to_visit0) {		\
      to_visit--;				\
      VUNMARK(to_visit->ptd0,to_visit->oldv);	\
    }						\
    pop_text_stack(lvl);			\
    return true;				\
  }

#define def_overflow() 				\
  def_aux_overflow();				\
  def_global_overflow();			\
  def_trail_overflow()


#define CYC					\
  if (IS_VISIT_MARKER(*ptd0)) {			\
    while (to_visit > to_visit0) {		\
      to_visit--;				\
      VUNMARK(to_visit->ptd0, to_visit->oldv);	\
    }						\
    pop_text_stack(lvl);			\
    return true;				\
  }

/**
   @brief routine to locate all variables in a term, and its applications */

static Term cyclic_complex_term(CELL *pt0_, CELL *pt0_end_ USES_REGS) {
  CELL *pt0, *pt0_end;
  int lvl;
  size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
  struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;

  WALK_COMPLEX_TERM__(CYC, CYC, {});
  /* leave an empty slot to fill in later */
  END_WALK();

  return false;

  def_overflow();
}

bool Yap_IsCyclicTerm(Term t USES_REGS) {
  cs[2]++;

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


/**
   @brief routine to locate all variables in a term, and its applications */

static bool ground_complex_term(CELL * pt0_, CELL * pt0_end_ USES_REGS) {
  CELL *pt0, *pt0_end;
  int lvl;
  size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
  struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;

  WALK_COMPLEX_TERM();
  /* leave an empty slot to fill in later */
  while (to_visit > to_visit0) {
    to_visit--;

    VUNMARK(to_visit->ptd0, to_visit->oldv);			\
  }
  pop_text_stack(lvl);
  return false;

  END_WALK();
  /* Do we still have compound terms to visit */

  pop_text_stack(lvl);

  return true;

  def_overflow();
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

static Int var_in_complex_term(CELL *pt0_, CELL *pt0_end_ ,
			       Term v USES_REGS) {
  CELL *pt0, *pt0_end;
  int lvl;
  size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
  struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;

  WALK_COMPLEX_TERM();

  if ((CELL)ptd0 == v) { /* we found it */
    /* Do we still have compound terms to visit */
    while (to_visit > to_visit0) {
      to_visit--;

      VUNMARK(to_visit->ptd0, to_visit->oldv);			\
    }
    pop_text_stack(lvl);
    return true;
  }
  goto restart;
  END_WALK();

  if (to_visit > to_visit0) {
    to_visit--;


    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
  }
  pop_text_stack(lvl);
  return false;

  def_overflow();
}

static Int var_in_term(
		       Term v, Term t USES_REGS) /* variables in term t		 */
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
static Term vars_in_complex_term(CELL *pt0_, CELL *pt0_end_ ,
				 Term inp USES_REGS) {
  CELL *pt0, *pt0_end;
  int lvl;
  size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
  struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;
  Int count=0;
  HB = HR;
  to_visit0=to_visit=NULL;
  // first get the extra variables
  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      YapBind(ptr,TermFoundVar);
      if (TR > (tr_fr_ptr)LOCAL_TrailTop-1024)
	goto trail_overflow;
      /* do or pt2 are unbound  */
    }
    inp = TailOfTerm(inp);
  }
  WALK_COMPLEX_TERM();
  if (HR + 1024 > ASP) {
    goto global_overflow;
  }
  HR[1] = AbsPair(HR + 2);
  HR += 2;
  HR[-2] = (CELL)ptd0;
  /* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
    goto trail_overflow;
  }
  YapBind(ptd0, FVM);
  END_WALK();

  HB = B->cp_h;

  clean_tr(TR0-count PASS_REGS);
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
    return AbsPair(InitialH);
  } else {
    return (inp);
  }
  def_overflow();

}

/**
 * @pred variables_in_term( +_T_, +_SetOfVariables_, +_ExtendedSetOfVariables_
 * )
 *
 * _SetOfVariables_ must be a list of unbound variables. If so,
 * _ExtendedSetOfVariables_ will include all te variables in the union
 * of `vars(_T_)` and _SetOfVariables_.
 */
static Int variables_in_term(
			     USES_REGS1) /* variables in term t		 */
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
static Int term_variables3(
			   USES_REGS1) /* variables in term t		 */
{
  Term out;
  cs[0]++;
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
		       Term t, UInt arity USES_REGS) /* variables in term t		 */
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
static Term Yap_TermAddVariables(
				 Term t, Term vs USES_REGS) /* variables in term t		 */
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
static Int term_variables(
			  USES_REGS1) /* variables in term t		 */
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
  CELL *beg, *end;
  CELL oval;
} att_rec_t;

static Term attvars_in_complex_term(
				    CELL *pt0_, CELL *pt0_end_ , Term inp USES_REGS) {
  CELL *pt0, *pt0_end;
  int lvl;
  size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
  struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;
  CELL output = inp;
  WALK_COMPLEX_TERM();
  if (IsAttVar(ptd0)) {
    /* do or pt2 are unbound  */
    attvar_record *a0 = RepAttVar(ptd0);
    d0 = *ptd0;
    /* leave an empty slot to fill in later */
    if (HR + 1024 > ASP) {
      goto global_overflow;
    }
    output = MkPairTerm((CELL) & (a0->Done), output);
    /* store the terms to visit */
    if (to_visit + 32 >= to_visit_max) {
      goto aux_overflow;
    }
    TrailTerm(TR++) = a0->Done;
    a0->Done=TermNil;
    if ((tr_fr_ptr)LOCAL_TrailTop - TR < 1024) {

      if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), true)) {
	goto trail_overflow;
      }
      pop_text_stack(lvl);
    }

    pt0_end = &a0->Atts;
    pt0 = pt0_end - 1;
  }
  END_WALK();

  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  /*fprintf(stderr,"<%ld at %s\n", d0, __FUNCTION__)*/;
  return output;

  def_overflow();
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
static Term new_vars_in_complex_term(
				     CELL *pt0_, CELL *pt0_end_ , Term inp USES_REGS) {
  CELL *pt0, *pt0_end;
  int lvl;
  size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
  struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;
  Int n=0;
  CELL output = TermNil;
  Term inp0 = inp;

  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      n++;
      TrailTerm(TR++) = t;
      TrailTerm(TR++) = t;
      *VarOfTerm(t) = TermFoundVar;
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
  WALK_COMPLEX_TERM();
  output = MkPairTerm((CELL)ptd0, output);
  TrailTerm(TR++) = *ptd0;
  *ptd0 = FVM;
  if ((tr_fr_ptr)LOCAL_TrailTop - TR < 1024) {
    goto trail_overflow;
  }
  /* leave an empty slot to fill in later */
  if (HR + 1024 > ASP) {
    goto global_overflow;
  }
  END_WALK();

  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);

  return output;

  def_overflow();
}

/** @pred  new_variables_in_term(+_CurrentVariables_, ? _Term_, -_Variables_)



    Unify  _Variables_ with the list of all variables of term
    _Term_ that do not occur in _CurrentVariables_.  The variables occur in
    the order of their first appearance when traversing the term depth-first,
    left-to-right.


*/
static Int p_new_variables_in_term(
				   USES_REGS1) /* variables within term t		 */
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

#define FOUND_VAR()				\
  if (d0 == TermFoundVar) {			\
    /* leave an empty slot to fill in later */	\
    if (HR + 1024 > ASP) {			\
      goto global_overflow;			\
    }						\
    HR[1] = AbsPair(HR + 2);			\
    HR += 2;					\
    HR[-2] = (CELL)ptd0;			\
    *ptd0 = TermNil;				\
  }

static Term vars_within_complex_term(
				     CELL *pt0_, CELL *pt0_end_, Term inp USES_REGS) {
  Int n=0;
  CELL output = AbsPair(HR);
  CELL *pt0, *pt0_end;
  int lvl;
  size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
  struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;
  

  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      *ptr = TermFoundVar;
      n++;
      TrailTerm(TR++) = t;
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
	Yap_growtrail(2*n * sizeof(tr_fr_ptr *), true);
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

  def_overflow();

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
static Int free_variables_in_term(
				  USES_REGS1)
{
  Term out;
  Term t, t0;
  Term found_module = 0L;
  Term bounds = TermNil;

  t = t0 = Deref(ARG1);

  while (!IsVarTerm(t) && IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorHat) {
      bounds = MkPairTerm(ArgOfTerm(1,t),bounds);
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
    out = new_vars_in_complex_term(&(t)-1, &(t), Yap_TermVariables(bounds, 3) PASS_REGS);
  }

  if (found_module && t != t0) {
    Term ts[2];
    ts[0] = found_module;
    ts[1] = t;
    t = Yap_MkApplTerm(FunctorModule, 2, ts);
  }
  return Yap_unify(ARG2, t) && Yap_unify(ARG3, out);
}

#define FOUND_VAR_AGAIN()			\
  if (d0 == TermFoundVar)			\
    {						\
      HR[0] = (CELL)ptd0;			\
      HR[1] = AbsPair(HR + 2);			\
      HR += 2;					\
      refound_var(ptd0);			\
    }
  
static inline void refound_var(CELL *ptd0) {
  *ptd0 = FRVM;

}

static Term non_singletons_in_complex_term(CELL * pt0_,
					   CELL * pt0_end_,
					   
					   Term s USES_REGS) {
  CELL *pt0, *pt0_end;
  int lvl;
  size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
  struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;

  WALK_COMPLEX_TERM__({}, {}, FOUND_VAR_AGAIN());
  /* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
    /* Trail overflow */
    goto trail_overflow;
  }

  YapBind(ptd0,FVM);
 
  END_WALK();

  clean_tr(TR0 PASS_REGS);

  pop_text_stack(lvl);
  if (HR != InitialH) {
    /* close the list */
    HR[-1] = s;
    return AbsPair(InitialH);
  } else {
    return s;
  }

  def_overflow();
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

    Unify _LVF_-_LV0_ with the list of variables that occur at least twice in _T_ or that occur in _LV0_ and _T_.

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
  return Yap_unify(ARG3,out);
}


Int Yap_NumberVars(Term inp,
		   Int numbv,
		   bool handle_singles) /*
					 * numbervariables in term t	 */
{
  CACHE_REGS
    Term t;
  if (handle_singles) {
    t = Yap_NonSingletons(inp, TermNil PASS_REGS);
  } else {
    t = Yap_TermVariables(inp,TermNil PASS_REGS);
  }
  while(t != TermNil) {
    CELL *el = RepPair(Deref(t));
    Term v = Deref(el[0]);
    t = el[1];
    if (v == el[0]) {
      HR[0] = (CELL)FunctorDollarVar;
      HR[1] = MkIntegerTerm(numbv++);
      YapBind(VarOfTerm(v), AbsAppl(HR));
      HR+=2;
    } else {
      el[0] = (CELL)FunctorDollarVar;
      el[1] = MkIntegerTerm(numbv++);
      YapBind(VarOfTerm(v), AbsAppl(el));
    }
  }
  if (handle_singles) { 
    t = Yap_TermVariables(inp,TermNil PASS_REGS);
  } else {
    t = TermNil;
  }
  while(t != TermNil) {

    CELL *el = RepPair((t));
    Term v = Deref(el[0]);
    t = el[1];
    if (v == el[0]) {
      HR[0] = (CELL)FunctorDollarVar;
      HR[1] = MkIntTerm(numbv++);
      YapBind(VarOfTerm(v), AbsAppl(HR));
      HR+=2;
    } else {
      el[0] = (CELL)FunctorDollarVar;
      el[1] = MkIntTerm(numbv++);
      YapBind(VarOfTerm(v), AbsAppl(el));
    }
    if (IsVarTerm(v)) {
      HR[0] = (CELL)FunctorDollarVar;
      HR[1] = MkIntTerm(-1);
      YapBind(VarOfTerm(v), AbsAppl(HR));
      HR+=2;
    } else {
      el[1] = TermNil;
    }
    t = Deref(t);
  }
  return numbv;
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

#define MAX_NUMBERED						\
  if (FunctorOfTerm(d0) == FunctorDollarVar) {			\
    Term t1 = ArgOfTerm(1, d0);					\
    Int i;							\
    if (IsIntegerTerm(t1) && ((i = IntegerOfTerm(t1)) > *maxp))	\
      *maxp = i;						\
    goto restart;						\
  }

static int max_numbered_var(CELL * pt0_, CELL * pt0_end_,
			    Int * maxp USES_REGS) {
  CELL *pt0, *pt0_end;
  int lvl;
  size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
  struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;

  WALK_COMPLEX_TERM__({}, MAX_NUMBERED, {});
  END_WALK();
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    VUNMARK(to_visit->ptd0, to_visit->oldv);
  }
  prune(B PASS_REGS);
  pop_text_stack(lvl);
  return 0;

  def_overflow();
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

static Term UNFOLD_LOOP(Term t, Term * b) {
  Term os[2], o;
  os[0] = o = MkVarTerm();
  os[1] = t;
  Term ti = Yap_MkApplTerm(FunctorEq, 2, os), t0 = *b;
  *b = MkPairTerm(ti, t0);
  return o;
}

typedef struct block_connector {
  CELL * parent;         //> index in the array;
  Term source;    //> source;
  CELL *copy;     //> copy;
  CELL header;    //> backup of first word of the source data;
  CELL reference; //> term used to refer the copy.
} cl_connector;

static Int t_ref(cl_connector *d, cl_connector * q, Int *mep, Int max) {
  if ( d >= q && d < q+max) {
    *mep = d-q;
    return true;
  }
  return false; //&& d->source == (void *;
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
  Yap_InitCPred("cyclic_term",1, cyclic_term, SafePredFlag);

  Yap_InitCPred("numbervars", 3, p_numbervars, 0);
  Yap_InitCPred("largest_numbervar", 2, largest_numbervar, 0);
}

///@}

