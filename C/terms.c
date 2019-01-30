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
* File:		utilpreds.c						 *
* Last rev:	4/03/88							 *
* mods:									 *
* comments:	new utility predicates for YAP				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "@(#)utilpreds.c	1.3";
#endif
/**
 * @file C/terms.c
 *
 * @brief applications of the tree walker pattern.
 *
 * @addtogroup Terms
 * @{
 */

#include "absmi.h"
#include "YapHeap.h"
#include "yapio.h"
#include "attvar.h"
#ifdef HAVE_STRING_H
#include "string.h"
#endif



static int
expand_vts( int args USES_REGS )
{
  UInt expand = LOCAL_Error_Size;
  yap_error_number yap_errno = LOCAL_Error_TYPE;

  LOCAL_Error_Size = 0;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  if (yap_errno == RESOURCE_ERROR_TRAIL) {
    /* Trail overflow */
    if (!Yap_growtrail(expand, FALSE)) {
      return FALSE;
    }
  } else if (yap_errno == RESOURCE_ERROR_AUXILIARY_STACK) {
    /* Aux space overflow */
    if (expand > 4*1024*1024)
      expand = 4*1024*1024;
    if (!Yap_ExpandPreAllocCodeSpace(expand, NULL, TRUE)) {
      return FALSE;
    }
  } else {
    if (!Yap_gcl(expand, 3, ENV, gc_P(P,CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, "in term_variables");
      return FALSE;
    }
  }
  return TRUE;
}

static inline void
clean_tr(tr_fr_ptr TR0 USES_REGS) {
  if (TR != TR0) {
    do {
      Term p = TrailTerm(--TR);
      RESET_VARIABLE(p);
    } while (TR != TR0);
  }
}

static inline void
clean_dirty_tr(tr_fr_ptr TR0 USES_REGS) {
  tr_fr_ptr pt0 = TR;
  while (pt0 != TR0) {
    Term p = TrailTerm(--pt0);
    if (IsApplTerm(p)) {
      CELL *pt = RepAppl(p);
#ifdef FROZEN_STACKS
      pt[0] = TrailVal(pt0);
#else
      pt[0] = TrailTerm(pt0 - 1);
      pt0 --;
#endif /* FROZEN_STACKS */
    } else {
      RESET_VARIABLE(p);
    }
  } 
  TR = TR0;
}

/// @brief recover original term while fixing direct refs.
///
/// @param USES_REGS 
///
static inline void
clean_complex_tr(tr_fr_ptr TR0 USES_REGS) {
  tr_fr_ptr pt0 = TR;
  while (pt0 != TR0) {
    Term p = TrailTerm(--pt0);
    if (IsApplTerm(p)) {
      /// pt: points to the address of the new term we may want to fix.
      CELL *pt = RepAppl(p);
      if (pt >= HB && pt < HR) { /// is it new?
	Term v = pt[0];
	if (IsApplTerm(v)) {
	  /// yes, more than a single ref
	  *pt = (CELL)RepAppl(v);
	}
#ifndef FROZEN_STACKS
	pt0 --;
#endif /* FROZEN_STACKS */
	continue;
      } 
#ifdef FROZEN_STACKS
      pt[0] = TrailVal(pt0);
#else
      pt[0] = TrailTerm(pt0 - 1);
      pt0 --;
#endif /* FROZEN_STACKS */
    } else {
      RESET_VARIABLE(p);
    }
  } 
  TR = TR0;
}

typedef struct {
  Term            old_var;
  Term            new_var;
}              *vcell;


typedef struct non_single_struct_t {
  CELL *ptd0;
  CELL d0;
  CELL *pt0, *pt0_end;
} non_singletons_t;

#define WALK_COMPLEX_TERM__(LIST0, STRUCT0)	\
  if (IsPairTerm(d0)) {				\
    if (to_visit +  32 >= to_visit_max) {	\
      goto aux_overflow;			\
    }						\
    LIST0;					\
    ptd0 = RepPair(d0);				\
    if (*ptd0 == TermFreeTerm) continue;	\
    to_visit->pt0 = pt0;			\
    to_visit->pt0_end = pt0_end;		\
    to_visit->ptd0 = ptd0;			\
    to_visit->d0 = *ptd0;			\
    to_visit ++;				\
    d0 = ptd0[0];				\
    pt0 = ptd0;					\
    *ptd0 = TermFreeTerm;			\
    pt0_end = pt0 + 1;				\
    if (pt0 <= pt0_end)                         \
    goto   list_loop;				\
  } else if (IsApplTerm(d0)) {			\
    register Functor f;				\
    register CELL *ap2;				\
    /* store the terms to visit */		\
    ap2 = RepAppl(d0);				\
    f = (Functor)(*ap2);			\
						\
    if (IsExtensionFunctor(f) ||  \
  IsAtomTerm((CELL)f)) {			\
						\
      continue;					\
    }						\
    STRUCT0;					\
    if (to_visit  + 32 >= to_visit_max) {	\
      goto aux_overflow;			\
    }						\
    to_visit->pt0 = pt0;			\
    to_visit->pt0_end = pt0_end;		\
    to_visit->ptd0 = ap2;			\
    to_visit->d0 = *ap2;			\
    to_visit ++;				\
						\
    *ap2 = TermNil;				\
    d0 = ArityOfFunctor(f);			\
    pt0 = ap2;					\
    pt0_end = ap2 + d0;				\
  }

#define WALK_COMPLEX_TERM()  WALK_COMPLEX_TERM__({}, {}) 

#define def_trail_overflow()				\
  trail_overflow:{					\
    while (to_visit > to_visit0) {			\
      to_visit --;					\
      CELL *ptd0 = to_visit->ptd0;			\
      *ptd0 = to_visit->d0;				\
    }							\
    pop_text_stack(lvl);				\
    LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;		\
    LOCAL_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);	\
    clean_tr(TR0 PASS_REGS);				\
    HR = InitialH;					\
    return 0L;						\
  }

#define def_aux_overflow()						\
  aux_overflow:{							\
    size_t d1 = to_visit-to_visit0;					\
    size_t d2 = to_visit_max-to_visit0;					\
    to_visit0 = Realloc(to_visit0,(d2+128)*sizeof(struct non_single_struct_t)); \
    to_visit = to_visit0+d1;						\
    to_visit_max = to_visit0+(d2+128);					\
    pt0--;								\
    goto restart;							\
  }

#define def_global_overflow()			\
  global_overflow:{				\
    while (to_visit > to_visit0) {		\
      to_visit --;				\
      CELL *ptd0 = to_visit->ptd0;		\
      *ptd0 = to_visit->d0;			\
    }						\
    pop_text_stack(lvl);			\
    clean_tr(TR0 PASS_REGS);			\
    HR = InitialH;				\
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;	\
    LOCAL_Error_Size = (ASP-HR)*sizeof(CELL);	\
    return false; }

static Int var_in_complex_term(register CELL *pt0,
			       register CELL *pt0_end,
			       Term v USES_REGS)
{

  int lvl = push_text_stack();

  struct non_single_struct_t  
    *to_visit = Malloc(1024*sizeof( struct non_single_struct_t)),
    *to_visit0 = to_visit,
    *to_visit_max = to_visit+1024;
 
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
  restart:
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
  list_loop:
    deref_head(d0, var_in_term_unk);
  var_in_term_nvar:
    {
    WALK_COMPLEX_TERM();
    continue;
    }
    deref_body(d0, ptd0, var_in_term_unk, var_in_term_nvar);
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
  }

  pop_text_stack(lvl);
  return false; 

  def_aux_overflow();
}

static Int
var_in_term(Term v, Term t USES_REGS)	/* variables in term t		 */
{

  if (IsVarTerm(t)) {
    return(v == t);
  } else if (IsPrimitiveTerm(t)) {
    return(FALSE);
  } else if (IsPairTerm(t)) {
    return(var_in_complex_term(RepPair(t)-1,
			       RepPair(t)+1,v PASS_REGS));
  }
  else return(var_in_complex_term(RepAppl(t),
				  RepAppl(t)+
				  ArityOfFunctor(FunctorOfTerm(t)),v PASS_REGS));
}

static Int
p_var_in_term( USES_REGS1 )
{
  return(var_in_term(Deref(ARG2), Deref(ARG1) PASS_REGS));
}

/**
   @brief routine to locate all variables in a term, and its applications */

static Term vars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp USES_REGS)
{

  int lvl = push_text_stack();

  struct non_single_struct_t  
    *to_visit = Malloc(1024*sizeof( struct non_single_struct_t)),
    *to_visit0 = to_visit,
    *to_visit_max = to_visit+1024;
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  CELL output = AbsPair(HR);

 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
  restart:

    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
  list_loop:
    deref_head(d0, vars_in_term_unk);
  vars_in_term_nvar:

    WALK_COMPLEX_TERM();
    continue ;

    derefa_body(d0, ptd0, vars_in_term_unk, vars_in_term_nvar);
    /* do or pt2 are unbound  */
    *ptd0 = TermNil;
    /* leave an empty slot to fill in later */
    if (HR+1024 > ASP) {
      goto global_overflow;
    }
    HR[1] = AbsPair(HR+2);
    HR += 2;
    HR[-2] = (CELL)ptd0;
    /* next make sure noone will see this as a variable again */
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
      /* Trail overflow */
      if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	goto trail_overflow;
      }
    }
    TrailTerm(TR++) = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
    goto loop;
  }


  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
 
  if (HR != InitialH) {
    /* close the list */
    Term t2 = Deref(inp);
    if (IsVarTerm(t2)) {
      RESET_VARIABLE(HR-1);
      Yap_unify((CELL)(HR-1),inp);
    } else {
      HR[-1] = t2;		/* don't need to trail */
    }
    return(output);
  } else {
    return(inp);
  }

  def_trail_overflow();
  def_aux_overflow();
  def_global_overflow();

}


static Int
p_variables_in_term( USES_REGS1 )	/* variables in term t		 */
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
	clean_tr(TR-count PASS_REGS);
	if (!Yap_growtrail(count*sizeof(tr_fr_ptr *), FALSE)) {
	  return FALSE;
	}
	goto restart;
      }
    }
    inp = TailOfTerm(inp);
  }
  do {
    Term t = Deref(ARG1);
    out = vars_in_complex_term(&(t)-1,
			       &(t),
			       ARG2 PASS_REGS);
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  clean_tr(TR-count PASS_REGS);
  return Yap_unify(ARG3,out);
}


/** @pred  term_variables(? _Term_, - _Variables_, +_ExternalVars_) is iso



    Unify the difference list between _Variables_ and _ExternaVars_
    with the list of all variables of term _Term_.  The variables
    occur in the order of their first appearance when traversing the
    term depth-first, left-to-right.


*/
static Int
p_term_variables3( USES_REGS1 )	/* variables in term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG1);
    if (IsVarTerm(t)) {
      Term out = Yap_MkNewPairTerm();
      return
	Yap_unify(t,HeadOfTerm(out)) &&
	Yap_unify(ARG3, TailOfTerm(out)) &&
	Yap_unify(out, ARG2);
    }  else if (IsPrimitiveTerm(t)) {
      return Yap_unify(ARG2, ARG3);
    } else {
      out = vars_in_complex_term(&(t)-1,
				 &(t), ARG3 PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);

  return Yap_unify(ARG2,out);
}

/**
 * Exports a nil-terminated list with all the variables in a term.
 * @param[t] the term
 * @param[arity] the arity of the calling predicate (required for exact garbage collection).
 * @param[USES_REGS] threading
 */
Term
Yap_TermVariables( Term t, UInt arity USES_REGS )	/* variables in term t		 */
{
  Term out;

  do {
    t = Deref(t);
    if (IsVarTerm(t)) {
      return MkPairTerm(t, TermNil);
    } else if (IsPrimitiveTerm(t)) {
      return TermNil;
    } else {
      out = vars_in_complex_term(&(t)-1,
				 &(t), TermNil PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( arity PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  return out;
}

/** @pred  term_variables(? _Term_, - _Variables_) is iso



    Unify  _Variables_ with the list of all variables of term
    _Term_.  The variables occur in the order of their first
    appearance when traversing the term depth-first, left-to-right.


*/
static Int
p_term_variables( USES_REGS1 )	/* variables in term t		 */
{
  Term out;

  if (!Yap_IsListOrPartialListTerm(ARG2)) {
    Yap_Error(TYPE_ERROR_LIST,ARG2,"term_variables/2");
    return FALSE;
  }

  do {
    Term t = Deref(ARG1);
    
    out = vars_in_complex_term(&(t)-1,
				 &(t), TermNil PASS_REGS);
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG2,out);
}

/** routine to locate attributed variables */


typedef struct att_rec {
  CELL *beg, *end;
  CELL oval;
} att_rec_t;

static Term attvars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp USES_REGS)
{
  int lvl = push_text_stack();
  struct non_single_struct_t  
    *to_visit = Malloc(1024*sizeof( struct non_single_struct_t)),
    *to_visit0 = to_visit,
    *to_visit_max = to_visit+1024;
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  CELL output = AbsPair(HR);

 restart:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
  list_loop:
    deref_head(d0, attvars_in_term_unk);
  attvars_in_term_nvar:
    {
      WALK_COMPLEX_TERM();
      continue;
    }


    derefa_body(d0, ptd0, attvars_in_term_unk, attvars_in_term_nvar);
    if (IsAttVar(ptd0)) {
      /* do or pt2 are unbound  */
      attvar_record *a0 = RepAttVar(ptd0);
      if (a0->AttFunc ==(Functor) TermNil) continue;
      /* leave an empty slot to fill in later */
      if (HR+1024 > ASP) {
	goto global_overflow;
      }
      HR[1] = AbsPair(HR+2);
      HR += 2;
      HR[-2] = (CELL)&(a0->Done);
      /* store the terms to visit */
      if (to_visit + 32 >= to_visit_max) {
	goto aux_overflow;
      }
      ptd0 = (CELL*)a0;
      to_visit->pt0 = pt0;
      to_visit->pt0_end = pt0_end;
      to_visit->d0 = *ptd0;
      to_visit->ptd0 = ptd0;
      to_visit ++;
      *ptd0 = TermNil;
      pt0_end = &RepAttVar(ptd0)->Atts;
      pt0 = pt0_end-1;
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
    goto restart;
  }

 
  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  if (HR != InitialH) {
    /* close the list */
    Term t2 = Deref(inp);
    if (IsVarTerm(t2)) {
      RESET_VARIABLE(HR-1);
      Yap_unify((CELL)(HR-1), t2);
    } else {
      HR[-1] = t2;		/* don't need to trail */
    }
    return(output);
  } else {
    return(inp);
  }

  def_aux_overflow();
  def_global_overflow();

}

  /** @pred term_attvars(+ _Term_,- _AttVars_)


      _AttVars_ is a list of all attributed variables in  _Term_ and
      its attributes. I.e., term_attvars/2 works recursively through
      attributes.  This predicate is Cycle-safe.


  */
static Int
p_term_attvars( USES_REGS1 )	/* variables in term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG1);
    if (IsPrimitiveTerm(t)) {
      return Yap_unify(TermNil, ARG2);
    } else {
      out = attvars_in_complex_term(&(t)-1,
				    &(t), TermNil PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return false;
     }
  } while (out == 0L);
  return Yap_unify(ARG2,out);
}

/** @brief output the difference between variables in _T_ and variables in some list.
 */
static Term new_vars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp USES_REGS)
{
  int lvl = push_text_stack();

  struct non_single_struct_t  
    *to_visit = Malloc(1024*sizeof( struct non_single_struct_t)),
    *to_visit0 = to_visit,
    *to_visit_max = to_visit+1024;
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  CELL output = AbsPair(HR);

  to_visit0 = to_visit;
  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      *ptr = TermFoundVar;
      TrailTerm(TR++) = t;
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
	if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	  goto trail_overflow;
	}
      }
    }
    inp = TailOfTerm(inp);
  }
 restart:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
  list_loop:
    deref_head(d0, vars_within_term_unk);
  vars_within_term_nvar:
    {
      WALK_COMPLEX_TERM();

      continue;
    }

    derefa_body(d0, ptd0, vars_within_term_unk, vars_within_term_nvar);
    /* do or pt2 are unbound  */
    *ptd0 = TermNil;
    /* leave an empty slot to fill in later */
    if (HR+1024 > ASP) {
      goto global_overflow;
    }
    HR[1] = AbsPair(HR+2);
    HR += 2;
    HR[-2] = (CELL)ptd0;
    /* next make sure noone will see this as a variable again */
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
      /* Trail overflow */
      if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	goto trail_overflow;
      }
    }
    TrailTerm(TR++) = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
    goto restart;
  }

  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  if (HR != InitialH) {
    HR[-1] = TermNil;
    return output;
  } else {
    return TermNil;
  }

  def_trail_overflow();
  def_aux_overflow();
  def_global_overflow();
}


/** @pred  new_variables_in_term(+_CurrentVariables_, ? _Term_, -_Variables_)



    Unify  _Variables_ with the list of all variables of term
    _Term_ that do not occur in _CurrentVariables_.  The variables occur in the order of their first
    appearance when traversing the term depth-first, left-to-right.


*/
static Int
p_new_variables_in_term( USES_REGS1 )	/* variables within term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG2);
   if (IsPrimitiveTerm(t))
      out = TermNil;
    else  {
      out = new_vars_in_complex_term(&(t)-1,
				     &(t), Deref(ARG1) PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG3,out);
}


static Term vars_within_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp USES_REGS)
{

  int lvl = push_text_stack();

  struct non_single_struct_t  
    *to_visit = Malloc(1024*sizeof( struct non_single_struct_t)),
    *to_visit0 = to_visit,
    *to_visit_max = to_visit+1024;
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  CELL output = AbsPair(HR);

  to_visit0 = to_visit;
  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      *ptr = TermFoundVar;
      TrailTerm(TR++) = t;
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
	if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	  goto trail_overflow;
	}
      }
    }
    inp = TailOfTerm(inp);
  }
 restart:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
  list_loop:
    deref_head(d0, vars_within_term_unk);
  vars_within_term_nvar:
    {
      WALK_COMPLEX_TERM()
      else if (d0 == TermFoundVar) {
	/* leave an empty slot to fill in later */
	if (HR+1024 > ASP) {
	  goto global_overflow;
	}
	HR[1] = AbsPair(HR+2);
	HR += 2;
	HR[-2] = (CELL)ptd0;
	*ptd0 = TermNil;
      }
    }
    continue;

    derefa_body(d0, ptd0, vars_within_term_unk, vars_within_term_nvar);
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
    goto restart;
  }

  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  if (HR != InitialH) {
    HR[-1] = TermNil;
    return output;
  } else {
    return TermNil;
  }


  def_trail_overflow();
  def_aux_overflow();
  def_global_overflow();
}

/** @pred  variables_within_term(+_CurrentVariables_, ? _Term_, -_Variables_)



    Unify  _Variables_ with the list of all variables of term
    _Term_ that *also* occur in _CurrentVariables_.  The variables occur in the order of their first
    appearance when traversing the term depth-first, left-to-right.

This predicate performs the opposite of new_variables_in_term/3.

*/
static Int
p_variables_within_term( USES_REGS1 )	/* variables within term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG2);
 if (IsPrimitiveTerm(t))
      out = TermNil;
 else {
   out = vars_within_complex_term(&(t)-1,
				     &(t), Deref(ARG1) PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG3,out);
}


static Term free_vars_in_complex_term(register CELL *pt0, register CELL *pt0_end, tr_fr_ptr TR0 USES_REGS)
{
  int lvl = push_text_stack();

  struct non_single_struct_t  
    *to_visit = Malloc(1024*sizeof( struct non_single_struct_t)),
    *to_visit0 = to_visit,
    *to_visit_max = to_visit+1024;
  Term o = TermNil;
  CELL *InitialH = HR;
  to_visit0 = to_visit;
 restart:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
  list_loop:
    deref_head(d0, vars_within_term_unk);
  vars_within_term_nvar:
    {
      WALK_COMPLEX_TERM();
      continue;
    }

    derefa_body(d0, ptd0, vars_within_term_unk, vars_within_term_nvar);
    /* do or pt2 are unbound  */
    *ptd0 = TermNil;
    /* leave an empty slot to fill in later */
    if (HR+1024 > ASP) {
      o = TermNil;
      goto global_overflow;
    }
    HR[0] =  (CELL)ptd0;
    HR[1] = o;
    o = AbsPair(HR);
    HR += 2;
    /* next make sure noone will see this as a variable again */
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
      /* Trail overflow */
      if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	goto trail_overflow;
      }
    }
    TrailTerm(TR++) = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
    goto restart;
  }

  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  return o;

 
  def_trail_overflow();
  def_aux_overflow();
  def_global_overflow();

}

static Term bind_vars_in_complex_term(register CELL *pt0, register CELL *pt0_end, tr_fr_ptr TR0 USES_REGS)
{
  register CELL **to_visit0, 
    **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  CELL *InitialH = HR;

  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, vars_within_term_unk);
  vars_within_term_nvar:
    {
      if (IsPairTerm(d0)) {
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
	pt0 = RepPair(d0) - 1;
	pt0_end = RepPair(d0) + 1;
      } else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2;
	/* store the terms to visit */
	ap2 = RepAppl(d0);
	f = (Functor)(*ap2);
	if (IsExtensionFunctor(f)) {
	  continue;
	}
	/* store the terms to visit */
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
      }
      continue;
    }

    derefa_body(d0, ptd0, vars_within_term_unk, vars_within_term_nvar);
    /* do or pt2 are unbound  */
    *ptd0 = TermFoundVar;
    /* next make sure noone will see this as a variable again */
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
      /* Trail overflow */
      if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	goto trail_overflow;
      }
    }
    TrailTerm(TR++) = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
#ifdef RATIONAL_TREES
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
#else
    to_visit -= 2;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
#endif
    goto loop;
  }

  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  return TermNil;

 trail_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
  LOCAL_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  HR = InitialH;
  return 0L;

 aux_overflow:
  LOCAL_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  HR = InitialH;
  return 0L;

}


static Int
p_free_variables_in_term( USES_REGS1 )	/* variables within term t		 */
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
	out = bind_vars_in_complex_term(RepAppl(t),
					RepAppl(t)+1, TR0 PASS_REGS);
	if (out == 0L) {
	  goto trail_overflow;
	}
      } else if (f == FunctorModule) {
	found_module = ArgOfTerm(1, t);
      } else if (f == FunctorCall) {
	t = ArgOfTerm(1, t);
	continue;
      } else if (f == FunctorExecuteInMod) {
	found_module = ArgOfTerm(2, t);
	t = ArgOfTerm(1, t);
	continue;
      } else {
	break;
      }
      t = ArgOfTerm(2,t);
    }
     if (IsPrimitiveTerm(t))
      out = TermNil;
    else {
      out = free_vars_in_complex_term(&(t)-1,
				      &(t), TR0 PASS_REGS);
    }
    if (out == 0L) {
    trail_overflow:
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  if (found_module && t!=t0) {
    Term ts[2];
    ts[0] = found_module;
    ts[1] = t;
    t = Yap_MkApplTerm(FunctorModule, 2, ts);
  }
  return
    Yap_unify(ARG2, t) &&
    Yap_unify(ARG3,out);
}



static Term non_singletons_in_complex_term(register CELL *pt0, register CELL *pt0_end USES_REGS)
{
  int lvl = push_text_stack();

  struct non_single_struct_t    *to_visit0,
    *to_visit = Malloc(1024*sizeof( struct non_single_struct_t)),
    *to_visit_max;
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;
  CELL output = AbsPair(HR);

  to_visit0 = to_visit;
  to_visit_max = to_visit0+1024;
 restart:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
  list_loop:
    deref_head(d0, vars_in_term_unk);
  vars_in_term_nvar:
    {
      WALK_COMPLEX_TERM()
      else if (d0 == TermFoundVar) {
	CELL *pt2 = pt0;
	while(IsVarTerm(*pt2))
	  pt2 = (CELL *)(*pt2);
	HR[1] = AbsPair(HR+2);
	HR[0] = (CELL)pt2;
	HR += 2;
	*pt2 = TermRefoundVar;
      }
      continue;
    }


    derefa_body(d0, ptd0, vars_in_term_unk, vars_in_term_nvar);
    /* do or pt2 are unbound  */
    *ptd0 = TermFoundVar;
    /* next make sure we can recover the variable again */
    TrailTerm(TR++) = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
    goto restart;
  }

  clean_tr(TR0 PASS_REGS);
  pop_text_stack(lvl);
  if (HR != InitialH) {
    /* close the list */
    HR[-1] = Deref(ARG2);
    return output;
  } else {
    return ARG2;
  }

  def_aux_overflow();
}

static Int
p_non_singletons_in_term( USES_REGS1 )	/* non_singletons in term t		 */
{
  Term t;
  Term out;

  while (TRUE) {
    t = Deref(ARG1);
    if (IsVarTerm(t)) {
      out = ARG2;
    }  else if (IsPrimitiveTerm(t)) {
      out = ARG2;
    } else {
     out = non_singletons_in_complex_term(&(t)-1,
					   &(t) PASS_REGS);
    }
    if (out != 0L) {
      return Yap_unify(ARG3,out);
    } else {
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK, ARG1, "overflow in singletons");
	return FALSE;
      }
    }
  }
}

static Int ground_complex_term(register CELL *pt0, register CELL *pt0_end USES_REGS)
{
  int lvl = push_text_stack();

  struct non_single_struct_t    *to_visit0,
    *to_visit = Malloc(1024*sizeof( struct non_single_struct_t)),
    *to_visit_max;

  to_visit0 = to_visit;
  to_visit_max = to_visit0+1024;
 restart:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;

    ++pt0;
    ptd0 = pt0;
    d0 = *ptd0;
  list_loop:
    deref_head(d0, vars_in_term_unk);
  vars_in_term_nvar:
    WALK_COMPLEX_TERM();
    continue;



    derefa_body(d0, ptd0, vars_in_term_unk, vars_in_term_nvar);
    while (to_visit > to_visit0) {
      to_visit --;
      CELL *ptd0 = to_visit->ptd0;
      *ptd0 = to_visit->d0;
    }
    pop_text_stack(lvl);
    return false;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
    goto restart;
  }
  pop_text_stack(lvl);
  return true;

  def_aux_overflow();
}

bool Yap_IsGroundTerm(Term t)
{
  CACHE_REGS
    while (TRUE) {
      Int out;

      if (IsVarTerm(t)) {
	return FALSE;
      }  else if (IsPrimitiveTerm(t)) {
	return TRUE;
      } else {
	if ((out =ground_complex_term(&(t)-1,
				      &(t) PASS_REGS)) >= 0) {
	  return out != 0;
	}
      if (out < 0) {
	*HR++ = t;
	if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	  Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK, ARG1, "overflow in ground");
	  return false;
	}
	t = *--HR;
      }
    }
    }
}

  /** @pred  ground( _T_) is iso


      Succeeds if there are no free variables in the term  _T_.


  */
static Int
p_ground( USES_REGS1 )			/* ground(+T)		 */
{
  return Yap_IsGroundTerm(Deref(ARG1));
}

static Term
numbervar(Int id USES_REGS)
{
  Term ts[1];
  ts[0] = MkIntegerTerm(id);
  return Yap_MkApplTerm(FunctorDollarVar, 1, ts);
}

static Term
numbervar_singleton(USES_REGS1)
{
  Term ts[1];
  ts[0] = MkIntegerTerm(-1);
  return Yap_MkApplTerm(FunctorDollarVar, 1, ts);
}

static void
renumbervar(Term t, Int id USES_REGS)
{
  Term *ts = RepAppl(t);
  ts[1] = MkIntegerTerm(id);
}

#define RENUMBER_SINGLES			\
  if (singles && ap2 >= InitialH && ap2 < HR) {	\
    renumbervar(d0, numbv++ PASS_REGS);		\
    continue;					\
  }


static Int numbervars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Int numbv, int singles USES_REGS)
{


  int lvl = push_text_stack();

  struct non_single_struct_t  
    *to_visit = Malloc(1024*sizeof( struct non_single_struct_t)),
    *to_visit0 = to_visit,
    *to_visit_max = to_visit+1024;
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = HR;

  to_visit0 = to_visit;
  to_visit_max = to_visit0+1024;
 restart:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
  list_loop:
    deref_head(d0, vars_in_term_unk);
  vars_in_term_nvar:
    {
      WALK_COMPLEX_TERM__({},RENUMBER_SINGLES);

      continue;
    }


    derefa_body(d0, ptd0, vars_in_term_unk, vars_in_term_nvar);
    /* do or pt2 are unbound  */
    if (singles)
      *ptd0 = numbervar_singleton( PASS_REGS1 );
    else
      *ptd0 = numbervar(numbv++ PASS_REGS);
    /* leave an empty slot to fill in later */
    if (HR+1024 > ASP) {
      goto global_overflow;
    }
    /* next make sure noone will see this as a variable again */
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
      /* Trail overflow */
      if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	goto trail_overflow;
      }
    }

#if defined(TABLING) || defined(YAPOR_SBA)
    TrailVal(TR) = (CELL)ptd0;
#endif
    TrailTerm(TR++) = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;

    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    CELL *ptd0 = to_visit->ptd0;
    *ptd0 = to_visit->d0;
    goto restart;
  }

  prune(B PASS_REGS);
  pop_text_stack(lvl);
  return numbv;

  def_trail_overflow();
  def_aux_overflow();
  def_global_overflow();
}


Int
Yap_NumberVars( Term inp, Int numbv, bool handle_singles )	/*
								 * numbervariables in term t	 */
{
  CACHE_REGS
    Int out;
  Term t;

 restart:
  t = Deref(inp);
  if (IsVarTerm(t)) {
    CELL *ptd0 = VarOfTerm(t);
    TrailTerm(TR++) = (CELL)ptd0;
    if (handle_singles) {
      *ptd0 = numbervar_singleton( PASS_REGS1 );
      return numbv;
    } else {
      *ptd0 = numbervar(numbv PASS_REGS);
      return numbv+1;
    }
  }  else if (IsPrimitiveTerm(t)) {
    return numbv;
  } else if (IsPairTerm(t)) {
    out = numbervars_in_complex_term(RepPair(t)-1,
				     RepPair(t)+1, numbv, handle_singles PASS_REGS);
  } else {
    Functor f = FunctorOfTerm(t);

    out = numbervars_in_complex_term(RepAppl(t),
				     RepAppl(t)+
				     ArityOfFunctor(f), numbv, handle_singles PASS_REGS);
  }
  if (out < numbv) {
    if (!expand_vts( 3 PASS_REGS ))
      return FALSE;
    goto restart;
  }
}

  /** @pred  numbervars( _T_,+ _N1_,- _Nn_)


      Instantiates each variable in term  _T_ to a term of the form:
      `$VAR( _I_)`, with  _I_ increasing from  _N1_ to  _Nn_.


  */
static Int
p_numbervars( USES_REGS1 )
{
  Term t2 = Deref(ARG2);
  Int out;

  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR,t2,"numbervars/3");
    return FALSE;
  }
  if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER,t2,"numbervars/3");
    return(FALSE);
  }
  if ((out = Yap_NumberVars(ARG1, IntegerOfTerm(t2), FALSE)) < 0)
    return FALSE;
  return Yap_unify(ARG3, MkIntegerTerm(out));
}

void Yap_InitTermCPreds(void)
{
  Yap_InitCPred("term_variables", 2, p_term_variables, 0);
  Yap_InitCPred("term_variables", 3, p_term_variables3, 0);
  Yap_InitCPred("$variables_in_term", 3, p_variables_in_term, 0);

  Yap_InitCPred("$free_variables_in_term", 3, p_free_variables_in_term, 0);

  Yap_InitCPred("term_attvars", 2, p_term_attvars, 0);

  CurrentModule = TERMS_MODULE;
  Yap_InitCPred("variable_in_term", 2, p_var_in_term, 0);
  Yap_InitCPred("variables_within_term", 3, p_variables_within_term, 0);
  Yap_InitCPred("new_variables_in_term", 3, p_new_variables_in_term, 0);
  CurrentModule = PROLOG_MODULE;
  
  Yap_InitCPred("$non_singletons_in_term", 3, p_non_singletons_in_term, 0);

  Yap_InitCPred("ground", 1, p_ground, SafePredFlag);

  Yap_InitCPred("numbervars", 3, p_numbervars, 0);
}

