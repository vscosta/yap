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

#define debug_pop_text_stack(l) [ if (to_visit != to_visit0) printf("%d\n",__LINE__); pop_text_stack(l) \
}

#include "attvar.h"
#include "yapio.h"
#ifdef HAVE_STRING_H
#include "string.h"
#endif

extern int cs[10];

int cs[10];

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

//#define  CELL *pt0, *pt0_end, *ptf;
//} non_singletons_t;

#define IS_VISIT_MARKER						\
(IsAtomTerm(d0) && AtomOfTerm(d0) >= (Atom)to_visit0 &&	\
 AtomOfTerm(d0) <= (Atom)to_visit)

#define VISIT_MARKER MkAtomTerm((Atom)to_visit)

typedef struct {
  Term old_var;
  Term new_var;
} * vcell;

typedef struct non_single_struct_t {
  CELL *ptd0;
  CELL d0;
  CELL *pt0, *pt0_end, *ptf;
} non_singletons_t;

#define WALK_COMPLEX_TERM__(LIST0, STRUCT0, PRIMI0)			\
\
 reset:\
 lvl = push_text_stack();\
  to_visit0 = Malloc(auxsz);				    \
pt0 = pt0_; pt0_end = pt0_end_;						\
to_visit = to_visit0,							\
    to_visit_max = to_visit +  auxsz/sizeof(struct non_single_struct_t);\
  \
 if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) { \
    /* Trail overflow */\
      goto trail_overflow;\
  }\
    if (HR + 1024 > ASP) { \
      goto global_overflow;\
    }\
while (to_visit >= to_visit0) {					\
  CELL d0;								\
  CELL *ptd0;								\
  restart:								\
  while (pt0 < pt0_end) {						\
   ++pt0;									\
   ptd0 = pt0;								\
   d0 = *ptd0;								\
   list_loop:								\
 /*fprintf(stderr, "%ld at %s\n", to_visit - to_visit0, __FUNCTION__);*/ \
   deref_head(d0, var_in_term_unk);					\
   var_in_term_nvar : {							\
     if (IsPairTerm(d0)) {							\
       if (to_visit + 32 >= to_visit_max) {					\
         goto aux_overflow;							\
       }									\
       ptd0 = RepPair(d0);							\
       d0 = ptd0[0];							\
       LIST0;								\
       if (IS_VISIT_MARKER)							\
         goto restart;							\
       to_visit->pt0 = pt0;							\
       to_visit->pt0_end = pt0_end;						\
       to_visit->ptd0 = ptd0;						\
       to_visit->d0 = d0;							\
       to_visit++;								\
       *ptd0 = VISIT_MARKER;						\
       pt0 = ptd0;								\
       pt0_end = pt0 + 1;							\
       goto list_loop;							\
     } else if (IsApplTerm(d0)) {						\
       register Functor f;							\
   /* store the terms to visit */					\
       ptd0 = RepAppl(d0);							\
       f = (Functor)(d0 = *ptd0);						\
       if (IsExtensionFunctor(f)) {\
        continue;\
      }\
       \
       if (to_visit + 32 >= to_visit_max) {					\
         goto aux_overflow;							\
       }									\
       STRUCT0;								\
       if (IS_VISIT_MARKER) {						\
         \
         continue;								\
       }									\
       to_visit->pt0 = pt0;							\
       to_visit->pt0_end = pt0_end;						\
       to_visit->ptd0 = ptd0;						\
       to_visit->d0 = d0;							\
       to_visit++;								\
       \
       *ptd0 = VISIT_MARKER;						\
       Term d1 = ArityOfFunctor(f);						\
       pt0 = ptd0;								\
       pt0_end = ptd0 + d1;							\
       continue;								\
     } else {								\
     if (IS_VISIT_MARKER) {						\
         \
         continue;								\
       }									\
       PRIMI0;								\
       continue;								\
     }									\
     derefa_body(d0, ptd0, var_in_term_unk, var_in_term_nvar)

#define WALK_COMPLEX_TERM() WALK_COMPLEX_TERM__({}, {}, {})

#define END_WALK()					\
   }							\
 }							\
     /* Do we still have compound terms to visit */	\
 to_visit--;					\
 if (to_visit >= to_visit0) {			\
   pt0 = to_visit->pt0;				\
   pt0_end = to_visit->pt0_end;			\
   *to_visit->ptd0 = to_visit->d0;			\
 }							\
}							\
pop_text_stack(lvl);

#define def_aux_overflow()						\
aux_overflow : {							\
  while (to_visit > to_visit0) {			\
    to_visit--;					\
    CELL *ptd0 = to_visit->ptd0;			\
    *ptd0 = to_visit->d0;				\
  }							\
  clean_tr(TR0 PASS_REGS);					\
  auxsz += auxsz;\
   pop_text_stack(lvl);					\
    goto reset; }

#define def_trail_overflow()					\
     trail_overflow: {					\
  while (to_visit > to_visit0) {			\
    to_visit--;					\
    CELL *ptd0 = to_visit->ptd0;			\
    *ptd0 = to_visit->d0;				\
  }							\
  size_t expand = (TR - TR0) * sizeof(tr_fr_ptr *);	\
  clean_tr(TR0 PASS_REGS);					\
  HR = InitialH;						\
  pop_text_stack(lvl);					\
  /* Trail overflow */  \
 if (!Yap_growtrail(expand, false)) { \
   Yap_ThrowError(RESOURCE_ERROR_TRAIL, TermNil, expand);\
    } \
goto reset;\
}

#define def_global_overflow()				\
global_overflow : {					\
  while (to_visit > to_visit0) {			\
    to_visit--;					\
    CELL *ptd0 = to_visit->ptd0;			\
    *ptd0 = to_visit->d0;				\
  }							\
  clean_tr(TR0 PASS_REGS);				\
  pop_text_stack(lvl);					\
  HR = InitialH;					\
  LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;		\
  size_t expand  = 0L;                                  \
    if (!Yap_gcl(expand, 3, ENV, gc_P(P, CP))) {  \
      Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, sizeof(CELL)*(HR-H0)); \
      return false;\
    }\
    goto reset;\
}

#define CYC_LIST				\
if (IS_VISIT_MARKER) {			\
  while (to_visit > to_visit0) {		\
    to_visit--;				\
    to_visit->ptd0[0] = to_visit->d0;		\
  }						\
  pop_text_stack(lvl);			\
  return true;				\
}

#define def_overflow() 				\
  def_aux_overflow(); \
  def_global_overflow();			\
  def_trail_overflow()


#define CYC_APPL				\
if (IS_VISIT_MARKER) {			\
  while (to_visit > to_visit0) {		\
    to_visit--;				\
    to_visit->ptd0[0] = to_visit->d0;		\
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

  WALK_COMPLEX_TERM__(CYC_LIST, CYC_APPL, {});
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

static Term BREAK_LOOP(CELL d0,struct non_single_struct_t  *to_visit ) {
  char buf[64];
  snprintf(buf, 63, "@^[" Int_FORMAT "]", to_visit-(struct non_single_struct_t*)AtomOfTerm(d0));
  return MkAtomTerm(Yap_LookupAtom(buf));
}

/**
   @brief routine to locate all variables in a term, and its applications */

static int cycles_in_complex_term( CELL *pt0_, CELL *pt0_end_ USES_REGS) {

  CELL *pt0, *pt0_end;
 size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
 struct non_single_struct_t *to_visit0, *to_visit, *to_visit_max;
 int lvl;

 reset:
  lvl = push_text_stack();
  pt0 = pt0_, pt0_end = pt0_end_;
    to_visit0 = Malloc(auxsz);
  to_visit= to_visit0;
  to_visit_max = to_visit0 + auxsz/sizeof(struct non_single_struct_t);
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) { \
    /* Trail overflow */\
      goto trail_overflow;\
  }\
  auxsz *= 2;
  int rc = 0;
  CELL *ptf;
  ptf = HR;
  HR++;
  while (to_visit >= to_visit0) {
    CELL d0;
    CELL *ptd0;

    while (pt0 < pt0_end) {
      ++pt0;
      ptd0 = pt0;
      d0 = *ptd0;
      list_loop:
      deref_head(d0, var_in_term_unk);
      var_in_term_nvar : {
       if (IsPairTerm(d0)) {
         if (to_visit + 32 >= to_visit_max) {
           goto aux_overflow;
         }
         ptd0 = RepPair(d0);
         d0 = ptd0[0];
         if (IS_VISIT_MARKER) {
           rc++;
           *ptf++ = BREAK_LOOP(d0, to_visit);
           continue;
         }
         *ptf++ = AbsPair(HR);
         to_visit->pt0 = pt0;
         to_visit->pt0_end = pt0_end;
         to_visit->ptd0 = ptd0;
         to_visit->d0 = d0;
         to_visit->ptf = ptf;
         to_visit++;
         ptf = HR;
    if (HR + 1024 > ASP) { \
      goto global_overflow;\
    }\
         HR += 2;
         *ptd0 = VISIT_MARKER;
         pt0 = ptd0;
         pt0_end = pt0+1;
         ptf = HR - 2;
         goto list_loop;
       } else if (IsApplTerm(d0)) {
        register Functor f;
          /* store the terms to visit */
        ptd0 = RepAppl(d0);
        f = (Functor)(d0 = *ptd0);
        if (IsExtensionFunctor(f)) {
	  *ptf++ = AbsAppl(ptd0);
        continue;
      }
       if (IS_VISIT_MARKER) {
         rc++;
         *ptf++ = BREAK_LOOP(d0, to_visit);
         continue;
       }
      if (to_visit + 32 >= to_visit_max) {
        goto aux_overflow;
      }
      *ptf++ = AbsAppl(HR);
      to_visit->pt0 = pt0;
      to_visit->pt0_end = pt0_end;
      to_visit->ptd0 = ptd0;
      to_visit->d0 = d0;
      to_visit->ptf = ptf;
      to_visit++;

      *ptd0 = VISIT_MARKER;
      *HR++ = (CELL)f;
      ptf = HR;
      Term d1 = ArityOfFunctor(f);
      pt0 = ptd0;
      pt0_end = ptd0 + (d1);
      HR+=d1;
      continue;
    } else {
        if (IS_VISIT_MARKER) {
           rc++;
           *ptf++ = BREAK_LOOP(d0, to_visit);
           continue;
         }
     *ptf++ = d0;
     continue;
   }
   derefa_body(d0, ptd0, var_in_term_unk, var_in_term_nvar);
   *ptf++ = d0;
 }
}
    /* Do we still have compound terms to visit */
to_visit--;
if (to_visit >= to_visit0) {
  pt0 = to_visit->pt0;
  pt0_end = to_visit->pt0_end;
  ptf = to_visit->ptf;
  *to_visit->ptd0 = to_visit->d0;
}
}
pop_text_stack(lvl);

return rc;

def_overflow();


}

Term Yap_CyclesInTerm(Term t USES_REGS) {
  cs[3]++;
  t = Deref(t);
  if (IsVarTerm(t)) {
    return t;
  } else if (IsPrimitiveTerm(t)) {
    return t;
  } else {
    CELL *Hi = HR;
    if ( cycles_in_complex_term(&(t)-1, &(t)PASS_REGS) >0) {
      return Hi[0];
    } else {
      HR = Hi;
      return t;
    }
  }
}

/** @pred  cycles_in_term( + _T_ )


    Succeeds if the graph representation of the term has markers in every
    loop. Say, the representation of a term `X` that obeys the equation `X=[X]`
    term has a loop from the list to its head.


*/
static Int cycles_in_term(USES_REGS1) /* cyclic_term(+T)		 */
{
  return Yap_CyclesInTerm(Deref(ARG1));
}

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

    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
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

  Int count=0;
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
     }
   }
   inp = TailOfTerm(inp);
 }

  CELL output = AbsPair(HR);
 CELL *pt0, *pt0_end;
 int lvl;
 size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
 struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;

  WALK_COMPLEX_TERM();
  /* do or pt2 are unbound  */

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
  TrailTerm(TR++) = (CELL)ptd0;
  *ptd0 = TermFoundVar;
  END_WALK();

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
    return (output);
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
 *ptd0 = TermFoundVar;
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

#define FOUND_VAR_AGAIN()   \
  if (d0 == TermFoundVar)   \
  {                         \
    HR[0] = (CELL)ptd0;     \
    HR[1] = AbsPair(HR + 2); \
    HR += 2;                \
    *ptd0 = TermRefoundVar; \
  }

static Term non_singletons_in_complex_term(CELL * pt0_,
  CELL * pt0_end_ USES_REGS) {
 CELL *pt0, *pt0_end;
 int lvl;
 size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
 struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;

  WALK_COMPLEX_TERM__({}, {}, FOUND_VAR_AGAIN());
  /* do or pt2 are unbound  */
  *ptd0 = TermFoundVar;
  /* next make sure noone will see this as a variable again */
  if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256)
  {
    goto trail_overflow;
  }
  TrailTerm(TR++) = (CELL)ptd0;
  END_WALK();

  clean_tr(TR0 PASS_REGS);

  pop_text_stack(lvl);
  if (HR != InitialH) {
    /* close the list */
    HR[-1] = Deref(ARG2);
    return AbsPair(InitialH);
  } else {
    return ARG2;
  }

  def_overflow();
}

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
      out = non_singletons_in_complex_term(&(t)-1, &(t)PASS_REGS);
    }
    return Yap_unify(ARG3,out);
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

#define RENUMBER_SINGLES			\
if (singles) {				\
  renumbervar(d0, numbv++ PASS_REGS);		\
  goto restart;				\
}

static Int numbervars_in_complex_term(CELL * pt0_, CELL * pt0_end_, Int numbv,
  int singles USES_REGS) {
 CELL *pt0, *pt0_end;
 int lvl;
 size_t auxsz = 1024 * sizeof(struct non_single_struct_t);
 struct non_single_struct_t *to_visit0, *to_visit,* to_visit_max;
  CELL *InitialH = HR;
  tr_fr_ptr TR0 = TR;

  WALK_COMPLEX_TERM__({}, {}, {});

  if (IsAttVar(pt0))
    continue;
  /* do or pt2 are unbound  */
  if (singles)
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

  def_overflow();

}

Int Yap_NumberVars(Term inp, Int numbv,
		   bool handle_singles) /*
					 * numbervariables in term t	 */
{
  CACHE_REGS
  Int out;
  Term t;

   t = Deref(inp);
  if (IsPrimitiveTerm(t)) {
    return numbv;
  } else {

    out = numbervars_in_complex_term(&(t)-1, &(t), numbv,
     handle_singles PASS_REGS);
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
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
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

static Int create_entry(Term t, Int i, Int j,  cl_connector * q, Int max) {
  Term ref, h, *s, *ostart;
  ssize_t n;
  //  fprintf(stderr,"[%ld,%ld]/%ld, %lx\n",i,j,max,t);
 restart:
  // first time, create a new term
  if (IsVarTerm(t)) {
    return -1;
  }
  if (IsPairTerm(t)) {
    Int me;
    s = RepPair(t);
    h = s[0];
    if (IsAtomTerm(h) && t_ref((cl_connector *)AtomOfTerm(h), q, &me, max)) {
      return me;
    }
    n = 2;
    ostart = HR;
    ref = AbsPair(ostart);
    HR += 2;
  } else if (IsApplTerm(t)) {
    Int me;
    h = (CELL)FunctorOfTerm(t);
    if (IsExtensionFunctor((Functor)h)) {
      return -1;
    }
     if (IsAtomTerm(h) &&
        t_ref((cl_connector*)AtomOfTerm(h),q,&me,max)) {
        return me;
    }
    n = ArityOfFunctor((Functor)h);
     s = RepAppl(t);
    ostart = HR;
    ref = AbsAppl(ostart);
    *ostart++ = s[0];
    HR=ostart+n;
  } else {
    Int me;
    if (IsAtomTerm(t) && t_ref((cl_connector*)AtomOfTerm(t),q,&me,max)) {
      t = q[me].source;
      goto restart;
    } else {
    return -1;
    }
      }
  q[max].header = h;
  q[max].parent = q[i].copy+j;
  q[i].copy[j] = ref;
  q[max].source = t;
  q[max].copy = ostart;
  q[max].reference = ref;
  s[0] = MkAtomTerm((void*)(q+max));
  return max+1;
}

Int cp_link(Term t, Int i, Int j, cl_connector * q, Int max, CELL * tailp) {
  Int me;
  t = Deref(t);
  if ((me = create_entry(t, i, j, q, max)) < max) {
    if (me < 0) {
      q[i].copy[j] = t;
      return max;
    }
    Term ref = q[me].reference;
    if (IsVarTerm(ref)) {
      q[i].copy[j] = ref;
      // fprintf(stderr," - %p\n", ref);
    }
   else {
    Term v = 	UNFOLD_LOOP(ref, tailp);
    q[i].copy[j] = v;
    if (me)
    q[me].parent[0] = v;
    q[me].reference = v;
  }
  return max;
}
return me;
}

Term Yap_BreakCycles(Term inp, UInt arity, Term * listp USES_REGS) {

  int lvl = push_text_stack();

  Term t = Deref(inp);
  ssize_t qsize = 2048, qlen = 0;
  cl_connector *q = Malloc(qsize * sizeof(cl_connector));
  Term *s;
  Int i = 0;

  HB = HR;
  qlen = 0;
  Term t0 = MkPairTerm(t,  TermNil);
  q[0].copy = HR;
    HR+=2;
  if (IsVarTerm(t) || IsPrimitiveTerm(t)) {
    return t;
  } else {
    // initialization
    qlen = create_entry(Deref(t0), i, 0, q, qlen);
    while(i<qlen) {
      arity_t n, j;
      if (IsPairTerm(q[i].source)) {
       s = RepPair(q[i].source);
       n = 2;
	// fetch using header field.
       qlen = cp_link(q[i].header, i, 0, q, qlen, listp);
	// fetch using standard access
       qlen = cp_link(s[1], i, 1, q, qlen, listp);
     } else {
       s = RepAppl(q[i].source) + 1;
       n = ArityOfFunctor((Functor)q[i].header);
       for (j = 0; j < n; j++) {
         qlen = cp_link(s[j], i, j, q, qlen, listp);
       }
     }
     i++;
   }
 }

 for (i=0; i< qlen; i++) {
  CELL *p = IsPairTerm(q[i].source) ? RepPair(q[i].source) : RepAppl(q[i].source);
  p[0] = (q[i].header);
}

pop_text_stack(lvl);

HB = B->cp_h;
 return HeadOfTerm( q[0].reference );
}

/** @pred  rational_term_to_tree(? _TI_,- _TF_, ?SubTerms, ?MoreSubterms)


    The term _TF_ is a forest representation (without cycles) for
    the Prolog term _TI_. The term _TF_ is the main term.  The
    difference list _SubTerms_-_MoreSubterms_ stores terms of the
    form _V=T_, where _V_ is a new variable occuring in _TF_, and
    _T_ is a copy of a sub-term from _TI_.


*/
static Int rational_term_to_tree(USES_REGS1) {
  Term t = Deref(ARG1);
  Term l = Deref(ARG4);
  if (IsVarTerm(l))
    Yap_unify(l, MkVarTerm());
  return Yap_unify(Yap_BreakCycles(t, 4, &l PASS_REGS), ARG2) &&
  Yap_unify(l, ARG3);
}

void Yap_InitTermCPreds(void) {
  Yap_InitCPred("cycles_in_term", 2, cycles_in_term, 0);
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
  Yap_InitCPred("rational_term_to_tree", 4, rational_term_to_tree, 0);

  Yap_InitCPred("$non_singletons_in_term", 3, p_non_singletons_in_term, 0);

  Yap_InitCPred("ground", 1, ground, SafePredFlag);
  Yap_InitCPred("cyclic_term", 1, cyclic_term, SafePredFlag);

  Yap_InitCPred("numbervars", 3, p_numbervars, 0);
  Yap_InitCPred("largest_numbervar", 2, largest_numbervar, 0);
}
//@}
