 /**********************************************************************
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
 *	undi							 *
 *************************************************************************/



#include "utf8proc.h"
#include <stdio.h>

#ifndef TERMS_C
#define TERMS_C 1

//#define DEB_D0(S) printf("%s %s: d0=%lx ptd0=%p *ptd0=%lx\n",S,d0, ptd0,
/**
 * @file C/terms.c
 *
 * @brief applications of the tree walker pattern.
 *
 * @addtogroup YAPTerms Term Manipulation
 * @ingroup Builtins
 *
 * @{
 *
 */

#include "absmi.h"

#include "YapHeap.h"

#include "heapgc.h"

#include "YapCompoundTerm.h"

#include "Yapproto.h"

#include "attvar.h"
#include "clause.h"
#include "yapio.h"
#ifdef HAVE_STRING_H
#include "string.h"
#endif

#define DEB_DOOBOUT(d0)                                                    \
  // (fprintf(stderr, ">>>>>>> %s ", __FUNCTION__), Yap_DebugPlWriteln(d0))
#define DEB_DOOBIN(d0)  //(fprintf(stderr,"<<<<<<<<<<<<<<<<<<<<<<< %s:%d\n", __FUNCTION__, __LINE__)/*, Yap_DebugPlWriteln(d0))*/)
#define DEB_DOOB(S,sp)  //  fprintf(stderr, "%s %s:%d %ld \n ", S,__FUNCTION__, __LINE__, sp->pt- sp->pt0)

/*#define err, "%s %ld %p->%p=%lx ", s, st->pt - st->pt0, pt0, ptd0, d0), */
/*   Yap_DebugPlWriteln(d0)) */

#define push_sub_term(A,B,C,D,E) \
  if (A->pt + 2 > A->max) {\
if (realloc_stack(stt) == 0) {			\
      A->err = RESOURCE_ERROR_AUXILIARY_STACK;\
      continue;\
    }\
}						\
  push_sub_term__(A,B,C,D,E);
#define pop_sub_term(A,B,C) ( DEB_DOOB("-",A)   pop_sub_term__(A,B,C))
static inline bool push_sub_term__(Ystack_t *sp, CELL d0, CELL *pt0, CELL *b,
                                 CELL *e) {
  copy_frame *pt = sp->pt;
  pt->pt0 = b;
  pt->pt0_end = e;
  pt->oldv = d0;
  pt->oldp = pt0;
  *pt0 = AbsPair((Term*)pt);
  sp->pt++;
  return true;
}

static inline bool pop_sub_term__(Ystack_t *sp, CELL **b, CELL **e) {
  copy_frame *pt = --(sp->pt);
  if (pt < sp->pt0) {
    return false;
  }
  if (pt->oldp != NULL && *pt->oldp)
    pt->oldp[0] = pt->oldv;
  if (b)
    *b = pt->pt0;
  if (e)
    *e = pt->pt0_end;
  return true;
}

#define RESET_TERM_VISITOR_0()   \
  if (stt->err){   Term* pt0,*pt0_end;\
      while ((pop_sub_term(stt, &pt0, &pt0_end))){}	;				 \
    stt->arenap= NULL;							\
      stt->bindp = NULL;   \
      HR = stt->hlow;                    \
    t = term_error_handler(stt->bindp,t,stt);	\
stt->err = 0;					\
   }

#define RESET_TERM_VISITOR_1(v,v0)		\
  if (stt->err){   Term* pt0,*pt0_end;\
      while ((pop_sub_term(stt, &pt0, &pt0_end))){}	;	 \
    stt->arenap= NULL;							\
      stt->bindp = NULL;   \
      HR = stt->hlow;\
yhandle_t yv = Yap_InitHandle(v0);		\
    t = term_error_handler(stt->bindp,t,stt);	\
stt->err = 0;\
v = v0=Yap_PopHandle(yv);				\
   }
#endif


#define RESET_TERM_VISITOR_3(first,tail,tail0,end)	\
  if (stt->err){   Term* pt0,*pt0_end;\
    while ((pop_sub_term(stt, &pt0, &pt0_end))){};			\
    stt->arenap= NULL;							\
      stt->bindp = NULL;   \
      HR = stt->hlow;                    \
     yhandle_t yv = Yap_InitHandle(tail0);\
     t = term_error_handler(stt->bindp,t,stt);	\
     tail0 = Yap_PopHandle(yv);\
     tail = tail0;\
first = tail;\
end = NULL;					\
 stt->err = 0;					\
   }


#define RESET_TERM_VISITOR_4()    \
  if (stt->err){   Term* pt0,*pt0_end;\
    while ((pop_sub_term(stt, &pt0, &pt0_end))){};            \
    stt->arenap= NULL;                            \
      stt->bindp = NULL;   \
      HR = stt->hlow;                    \
 stt->err = 0;                    \
   }


#define RESET_TERM_VISITOR_5(first,tail,tail0)	\
  if (stt->err){   Term* pt0,*pt0_end;\
    while ((pop_sub_term(stt, &pt0, &pt0_end))){};			\
    stt->arenap= NULL;							\
      stt->bindp = NULL;   \
      HR = stt->hlow;                    \
     yhandle_t yv = Yap_InitHandle(tail0);\
     t = term_error_handler(stt->bindp,t,stt);	\
     tail0 = Yap_PopHandle(yv);\
     tail = tail0;\
first = tail;\
 stt->err = 0;					\
   }

static void reset_list_of_term_vars(Term t USES_REGS)
{
  while (IsPairTerm(t)) {
    Term v = HeadOfTerm(t);
    RESET_VARIABLE(VarOfTerm(v));
    t = TailOfTerm(t);
  }
}




static Term term_error_handler(Term *bindp, Term t0,Ystack_t *stt) {
    CACHE_REGS
  yhandle_t y0 = Yap_StartHandles();
  yhandle_t ctx=Yap_InitHandle(t0);
  if (bindp) {
    Yap_InitHandle(*bindp);
  }
  if (stt->err == RESOURCE_ERROR_AUXILIARY_STACK) {
    // size_t delta = stt->max-stt->pt0;
    // size_t nsz = delta > 1024*1024 ? delta+1024+1024 : 2*delta; 
    if (!init_stack(stt))  {
      Yap_ThrowError(RESOURCE_ERROR_AUXILIARY_STACK, TermNil, "while visiting terms");
    }
    // *pdepth += 4096;
  } else if (stt->err == RESOURCE_ERROR_TRAIL) {

    if (!Yap_growtrail(0, false)) {
      Yap_ThrowError(RESOURCE_ERROR_TRAIL, TermNil, "while visiting terms");
    }
  } else if (stt->err == RESOURCE_ERROR_STACK) {
    //    printf("In H0=%p Hb=%ld H=%ld G0=%ld GF=%ld ASP=%ld\n",H0, cs->oHB-H0,
    //     cs->oH-H0, ArenaPt(*arenap)-H0,ArenaLimit(*arenap)-H0,(LCL0-cs->oASP)-H0)  ;
    if (!Yap_dogcl(0 PASS_REGS)) {
       Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
      }
    //     printf("In H0=%p Hb=%ld H=%ld G0=%ld GF=%ld ASP=%ld\n",H0, cs->oHB-H0,
     //      cs->oH-H0, ArenaPt(*arenap)-H0,ArenaLimit(*arenap)-H0,LCL0-cs->oASP-H0)  ;
  }
  if (bindp) {
    *bindp = Yap_GetFromHandle(ctx + 1);
  }
  Term tf = Yap_PopHandle(ctx);
  Yap_CloseHandles(y0);
  return tf;
}


#define LIST_HOOK_CODE if (IS_VISIT_MARKER(ptd1[0])) { goto found;}
#define COMPOUND_HOOK_CODE if (IS_VISIT_MARKER(ptd1[0])) { goto found;}
/**
   @brief routine to locate all variables in a term, and its applications
   */

static Term cyclic_complex_term(Term t USES_REGS) {
    COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_0();
#include "term_visit.h"
            /* leave an empty slot to fill in later */
    // no cycles found
    return false;
   found:
    while (pop_sub_term(stt, NULL, NULL)) {
    stt->pt0=stt->pt=stt->max=NULL;
    }
    return true;
}

bool Yap_IsCyclicTerm(Term t USES_REGS) {
  if (IsVarTerm(t)) {
        return false;
    } else if (IsPrimitiveTerm(t)) {
        return false;
    } else
    {
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_0();
      return cyclic_complex_term(t PASS_REGS);
  }
}


/** @pred  cyclic_term( + _T_ )


    Succeeds if the graph representation of the term has loops. Say,
    The representation of a term `X` that obeys the equation `X=[X]`
    term has a loop from the list to its head.


*/
static Int cyclic_term(USES_REGS1) /* cyclic_term(+T)		 */
{

    Term t=Deref(ARG1);
    bool rc;
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_0();
    rc = cyclic_complex_term(t PASS_REGS);
   return rc;
}


/**
   @brief routine to locate all variables in a term, and its application
   s */

#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE

#define VAR_HOOK_CODE                                                       \
  while (pop_sub_term(stt, NULL, NULL)) ;\
  stt->pt0=stt->pt=stt->max=NULL;\
  return false;

static bool ground_complex_term(Term t  USES_REGS) {
  //  pp(pt0_     [1], 0);
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_0();
#include "term_visit.h"
  return true;
}


bool Yap_IsGroundTerm(Term t) {
  CACHE_REGS
  if (IsVarTerm(t)) {
    return false;
  } else if (IsPrimitiveTerm(t)) {
    return true;
  } else {
    return ground_complex_term(t PASS_REGS);
  }
}

/** @pred  ground( _T_) is iso

    Succeeds if there
are no free variables in the term  _T_.



*/
static Int ground(USES_REGS1) /* ground(+T)		 */
{
   Term t;
   t = Deref(ARG1);
  if (IsVarTerm(t)) {
    return false;
  } else if (IsPrimitiveTerm(t)) {
    return true;
  } 
  return ground_complex_term(t PASS_REGS);
}

/**
   @brief routine to locate all variables in a term, and its application
   s */

#undef VAR_HOOK_CODE

#define VAR_HOOK_CODE                                                       \
  while (pop_sub_term(stt, NULL, NULL)) ;\
  stt->pt0=stt->pt=stt->max=NULL;\
  return ptd0;

static CELL *non_ground_complex_term(Term t  USES_REGS) {
  //  pp(pt0_     [1], 0);
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_0();
#include "term_visit.h"
  return NULL;
}

/** @pred  non_ground( _T_, _V_) is iso

    Succeeds if there
are free variables in the term  _T_, and _V_ is one of them.

*/
static Int non_ground(USES_REGS1) /* ground(+T)		 */
{
   Term t;
   t = Deref(ARG1);
  if (IsVarTerm(t)) {
    return Yap_unify(ARG2,t);
  } else if (IsPrimitiveTerm(t)) {
    return true;
  }
  CELL *pt;
  if ((pt = non_ground_complex_term(t PASS_REGS))) {
    return Yap_unify(ARG2,(CELL)pt);
  }
    return false;
}

#undef VAR_HOOK_CODE


#define VAR_HOOK_CODE\
  if (v == d0) {\
    while (pop_sub_term(stt, NULL, NULL)) ;\
    stt->pt0=stt->pt=stt->max=NULL;\
    return true;\
}


static bool var_in_complex_term(Term t, Term v USES_REGS) {
  Term v0 = v;
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_1(v,v0);
#include "term_visit.h"
  // all bindings are left  trailed.
  return false;
 }

static Int var_in_term(USES_REGS1)
 {
   Term t = Deref(ARG1), v0;
   Term v = v0 = Deref(ARG2);
   return var_in_complex_term(t, v PASS_REGS);



}

#undef SAVE_EXTRA
#undef RESTORE_EXTRA
#undef VAR_HOOK_CODE

#define VAR_HOOK_CODE                                                       \
  if (HR + 1024 > ASP) {\
    stt->err = RESOURCE_ERROR_STACK;\
    continue;\
  }\
  if (end == NULL) {					\
    first = AbsPair(HR);\
  } else {\
    end[0] = AbsPair(HR);\
  }\
  HR[0] = (CELL)ptd0;\
  HR[1] = tail;\
  end = (HR + 1);			\
  HR += 2;\


#if 0
/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static Term  var_occurrences_in_complex_term(Term t,
                                 Term tail USES_REGS) {
  Term tail0 = tail;

  Term *end = NULL, first = tail;
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_3(first,tail,tail0,end)

#include "term_visit.h"
  // all bindings are left  trailed.
return first;
}
#endif

/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static Term vars_in_complex_term(Term t,
                                 Term tail USES_REGS) {
  Term tail0 = tail;
  Term *end = NULL, first = tail;
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_3(first,tail,tail0,end)
#include "term_visit.h"

#undef SAVE_EXTRA
#undef RESTORE_EXTRA

  return  Yap_SortList(first PASS_REGS);
}

#undef SAVE_EXTRA
#undef RESTORE_EXTRA
#undef VAR_HOOK_CODE


#define VAR_HOOK_CODE mSET(ptd0, TermNone);

/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static void mark_vars_in_complex_term(Term
				      t USES_REGS) {
  // this does not trail, because there will be a second visitor
  COPY(pt0_[1]);

#define RESET_TERM_VISITOR RESET_TERM_VISITOR_4()
#include "term_visit.h"
  // all bindings are left  trailed.
return;
}
#undef VAR_HOOK_CODE

#define VAR_HOOK_CODE mSET(pt0, TermNone);	\
  if (HR + 1024 > ASP) {\
    stt->err = RESOURCE_ERROR_STACK;\
    continue;\
  }\
  if (end == NULL) {\
    first = AbsPair(HR);\
  } else {\
    end[0] = AbsPair(HR);\
  }\
  HR[0] = (CELL)ptd0;\
  HR[1] = tail;\
  end = (HR + 1);			\
  HR += 2;\

#if 0
/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static Term list_and_mark_vars_in_complex_term(Term t,
                                 Term tail USES_REGS) {
  Term tail0 = tail;

  Term *end = NULL, first = tail;
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_3(first,tail,tail0,end)
#include "term_visit.h"
  // all bindings are left  trailed.
return first;
}

#endif

#undef SAVE_EXTRA
#undef RESTORE_EXTRA
#undef VAR_HOOK_CODE



#define ATOMIC_HOOK_CODE   if (d0==TermNone) mSET(ptd0,(CELL)ptd0);



/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static void unmark_vars_in_complex_term(Term t USES_REGS) {
  // this does not trail, because there will be a second visitor
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_0()
  COPY(pt0_[1]);

#include "term_visit.h"
  // all bindings are left  trailed.
return;
}

#undef ATOMIC_HOOK_CODE


#define ATOMIC_HOOK_CODE                                                       \
 \
  if (d0 == TermNone) {\
  if (HR + 1024 > ASP) {\
    stt->err = RESOURCE_ERROR_STACK;\
    continue;\
  }\
  if (end == NULL) {\
    first = AbsPair(HR);\
  } else {\
    end[0] = AbsPair(HR);\
  }\
 /* next make sure noone will see this, *start = HRs as a variable again */\
RESET_VARIABLE(ptd0);\
  HR[0] = (CELL)ptd0;\
  HR[1] = tail;\
  end = HR + 1;\
  HR += 2;\
}

/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static Term marked_vars_in_complex_term(Term t,
                                 Term tail  USES_REGS) {
  Term tail0=tail;
  Term *end = NULL, first = tail;
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_3(first,tail,tail0,end)
#include "term_visit.h"
  // all bindings are left  trailed.
  return first;
}

/**
 * @pred variables_in_term( +_t, +_SetOfVariables_, +_ExtendedSetOfVariables_
 * )
 *
 * _SetOfVariables_ must be a list of unbound variables. If so,
 * _ExtendedSetOfVariables_ will include all te variables in the union
 * of `vars(_T_)` and _SetOfVariables_.
 */
static Int variables_in_term(USES_REGS1) /* variables in term t		 */
{

  Term  t, out;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    out = MkPairTerm(MkGlobal(t),TermNil);
  } else   if (!IsVarTerm(t) && IsPrimitiveTerm(t)) {
    out = TermNil;
    } else {
    out = vars_in_complex_term(t, TermNil PASS_REGS);
    reset_list_of_term_vars(out PASS_REGS);
 }
  out = Yap_SortList(out PASS_REGS);
    return Yap_unify(out,ARG2);
  }

/** @pred  term_variables(? _Term_, - _Variables_, +_ExternalVars_) is iso

    Unify the difference list between _Variables_ and _ExternaVars_
    with the list of all variables of term _Term_.  The variables
    occur in the order of their first appearance when traversing the
    term depth-first, left-to-right.
*/
static Int term_variables3(USES_REGS1) /* variables in term t		 */
{
   Term  t, out;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    out = MkPairTerm(MkGlobal(t),TermNil);
  } else   if (!IsVarTerm(t) && IsPrimitiveTerm(t)) {
    out = ARG3;
    } else {
    out = vars_in_complex_term(t, Deref(ARG3) PASS_REGS);
  }
      reset_list_of_term_vars(out PASS_REGS);

      return Yap_unify(out,ARG2);
}


#undef SAVE_EXTRA
#undef RESTORE_EXTRA
#undef VAR_HOOK_CODE

#define VAR_HOOK_CODE                                                       \
  if (HR + 1024 > ASP) {\
    stt->err = RESOURCE_ERROR_STACK;\
    continue;\
  }\
  if (end == NULL) {					\
    first = AbsPair(HR);\
  } else {\
    end[0] = AbsPair(HR);\
  }\
  HR[0] = (CELL)ptd0;\
  HR[1] = tail;\
  end = (HR + 1);			\
  HR += 2;\


static Term occurrences_in_complex_term(Term t,
                                 Term tail USES_REGS) {
  Term tail0 = tail;
  Term *end = NULL, first = tail;
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_3(first,tail,tail0,end)
#include "term_visit.h"

#undef SAVE_EXTRA
#undef RESTORE_EXTRA

  return  first;
}

/** @pred  term_variable_occurrences(? _Term_, - _Variables_) 

    Unify _Variables_ the list of all occurrence of variables in term
    _Term_.

*/
static Int term_variable_occurrences(USES_REGS1) /* variables in term t		 */
{
   Term  t, out;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    out = MkPairTerm(MkGlobal(t),TermNil);
  } else   if (!IsVarTerm(t) && IsPrimitiveTerm(t)) {
    out = TermNil;
  } else {
    out = occurrences_in_complex_term(t, TermNil PASS_REGS);
  }
      reset_list_of_term_vars(out PASS_REGS);

      return Yap_unify(out,ARG2);
}


static Int term_variable_occurrences3(USES_REGS1) /* variables in term t		 */
{
   Term  t, out;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    out = MkPairTerm(MkGlobal(t),Deref(ARG3));
  } else   if ( IsPrimitiveTerm(t)) {
    out = Deref(ARG3);
  } else {
    out = occurrences_in_complex_term(t, Deref(ARG3) PASS_REGS);
  }
      reset_list_of_term_vars(out PASS_REGS);

      return Yap_unify(out,ARG2);
}





/** @pred  term_variables_union(? _Term1_, - _Term2_, +_Vars_) is iso

    Unify _Vars_ with all variables in either term.
*/

static Int term_variables_union(USES_REGS1) /* variables in term t		 */
{
  
  Term  t[2], out;
  t[0] = Deref(ARG1);
  t[1] = Deref(ARG2);
  out = vars_in_complex_term(AbsPair(t)
			     , TermNil PASS_REGS);
    reset_list_of_term_vars(out PASS_REGS);
  return Yap_unify(out,ARG3);
}

/** @pred  term_variables_difference(? _Term1_, - _Term2_, +_Vars_) is iso

    Unify _Vars_ with all variables in _Term1_ and not in _Term2_.
*/
static Int term_variables_difference(USES_REGS1) /* variables in term t		 */
{
  Term  t1, t2, out;
  t2 = Deref(ARG2);
  // bind them
  vars_in_complex_term(t2, TermNil PASS_REGS);
    // list of remainder
  t1 = Deref(ARG1);
  out = vars_in_complex_term(t1, TermNil PASS_REGS);
    reset_list_of_term_vars(out PASS_REGS);
  return Yap_unify(out,ARG3);

}

/** @pred  term_variables_difference(? _Term1_, - _Term2_, +_Vars_) is iso

    Unify _Vars_ with all variables in _Term1_ and not in _Term2_.
*/
static Int term_variables_intersection(USES_REGS1) /* variables in term t		 */
{
  Term  t1, t2, out;
   t2 = Deref(ARG2);
  // bind them
  mark_vars_in_complex_term(t2 PASS_REGS);
    // list of remainder
  t1 = Deref(ARG1);
  out = marked_vars_in_complex_term(t1
				    , TermNil PASS_REGS);
  unmark_vars_in_complex_term(t2 PASS_REGS);
	return Yap_unify(out,ARG3);

}

/**
 * Extends list with all the variables in a term.
 * @param[t] the term
 * @param[list] the original list.
 * @param[USES_REGS] threading
 */                                                                         
Term Yap_TermVariables(Term t, Term t0 USES_REGS) /* variables in term t  */
{
   Term  out;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    out = MkPairTerm(MkGlobal(t),t0);
  } else   if (!IsVarTerm(t) && IsPrimitiveTerm(t)) {
    out = t0;
    } else {
    out = vars_in_complex_term(t, t0 PASS_REGS);
    reset_list_of_term_vars(out PASS_REGS);
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
  if (!Yap_IsListOrPartialListTerm(Deref(ARG2))) {
    Yap_ThrowError(TYPE_ERROR_LIST, ARG2, "term_variables/2");
    return false;
  }
    Term  t, out;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    out = MkPairTerm(MkGlobal(t),TermNil);
  } else   if ( IsPrimitiveTerm(t)) {
    out = TermNil;
    } else {
    out = vars_in_complex_term(t, TermNil PASS_REGS);
    reset_list_of_term_vars(out PASS_REGS);
  }
  return Yap_unify(out,ARG2);
    
}

/** routine to locate attributed variables */

typedef struct att_rec {
  CELL *beg, *end, d0;
} att_rec_t;

/** @pred  new_variables_in_term(+_CurrentVariables_, ? _Term_, -_Variables_)

    Unify  _Variables_ with the list of all variables of term
    _Term_ that do not occur in _CurrentVariables_. That is:

    `Variables = vars(Term) - CurrentVariables`
=
    The variables occur in
the order of their first appearance when traversing the term depth-first,
left-to-right.


*/
static Int
p_new_variables_in_term(USES_REGS1) /* variables within term t		 */
{
  Term t = ARG2;
  ARG2 = ARG1;
  ARG1 = t;
  return term_variables_difference(PASS_REGS1);
    }

static Int
free_variables_in_term(USES_REGS1) /* variables within term t		 */
{  Term bounds;
  Term module = CurrentModule;

  Term t = Deref(ARG1);
  bounds = Deref(ARG2);




    if (IsVarTerm(t)) {
    t = MkGlobal(t);
  }

  while (!IsVarTerm(t) && IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorHat) {
      bounds = MkPairTerm(ArgOfTerm(1, t), bounds);
      t = ArgOfTerm(2, t);
    } else if (f==FunctorModule) { t = Yap_StripModule(t, &module);
    } else if (f == FunctorCall) {
      t = ArgOfTerm(1, t);
    } else if (f == FunctorExecuteInMod) {
            bounds = ArgOfTerm(2,t);
t = ArgOfTerm(1, t);
    } else {
      break;
    }
  }

  Term ts[2];
ts[0] = module;
ts[1] = t;
t = Yap_MkApplTerm(FunctorModule,2,ts);
    mark_vars_in_complex_term(bounds PASS_REGS);
    // list of remainder
    Term d = vars_in_complex_term(t,	     TermNil PASS_REGS);
    reset_list_of_term_vars(d PASS_REGS);
   unmark_vars_in_complex_term(bounds PASS_REGS);
return Yap_unify( t,ARG2)  && Yap_unify(ARG3,d);
}

/**
all attributed variables
*/
#undef VAR_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#undef COMPOUND_HOOK_CODE

static bool undo_vbindings(Term t,  Term t0 USES_REGS)
{
  while (IsPairTerm(t) && t != t0) {
    Term h =  *RepPair(t);
    RESET_VARIABLE(VarOfTerm(h));
    t = TailOfTerm(t);
  }
  return true;
}

#define VAR_HOOK_CODE   \
  {								\
   if (!IS_VISIT_MARKER(*ptd0) && GlobalIsAttVar(ptd0)) {\
     if (HR + 1024 > ASP) {\
       undo_vbindings(first, tail PASS_REGS);	\
       stt->err = RESOURCE_ERROR_STACK;		\
       continue;				\
     }						\
     if (end == NULL) {				\
       first = AbsPair(HR);			\
     } else {					\
    end[0] = AbsPair(HR);\
  }\
  HR[0] = (CELL)ptd0;\
  end = HR + 1;\
  HR += 2;\
  * ptd0 = TermNone;			\
ptd0 += 2;					\
dd0 = *ptd0;\
 mderef_head(d0, dd0, var_in_term_unk);  \
goto  var_in_term_nvar;\
   }\
    }



/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static Term attvars_in_complex_term(Term t,
                                 Term tail  USES_REGS) {
  Term *end  = NULL, first = TermNil;
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_0()
  //  bindings to attributed vars are  trailed.
  #include "term_visit.h"
  if (first == TermNil)
    return tail;
  end[0] = tail;
  return first;
}


/** @pred term_attvars(+ _Term_,- _AttVars_)

    _AttVars_ is a list of all attributed variables in  _Term_ and
    its attributes. I.e., term_attvars/2 works recursively through
    attributes.  This predicate is Cycle-safe.
*/
static Int term_attvars(USES_REGS1)
{
    Term  t, out;
  t = Deref(ARG1);
  if ( IsPrimitiveTerm(t)) {
    out = TermNil;
    } else {
    out = attvars_in_complex_term(t, TermNil PASS_REGS);
    undo_vbindings(out, TermNil PASS_REGS);
  }
  return Yap_unify(out,ARG2);
}


/**
all attributed variables
*/
#undef VAR_HOOK_CODE
#undef ATOMIC_HOOK_CODE
#undef COMPOUND_HOOK_CODE

static bool undo_vterms(Term t,  Term t0 USES_REGS)
{
  while (IsPairTerm(t) && t != t0) {
    Term h =  *RepPair(t);
    RESET_VARIABLE(VarOfTerm(h));
    *RepPair(t) = AbsAppl(VarOfTerm(h)-1);
    t = TailOfTerm(t);
  }
  return true;
}

#define VAR_HOOK_CODE   \
  {								\
   if (!IS_VISIT_MARKER(*ptd0) && GlobalIsAttVar(ptd0)) {\
     if (HR + 1024 > ASP) {\
       undo_vbindings(first, tail PASS_REGS);	\
       stt->err = RESOURCE_ERROR_STACK;		\
       continue;				\
     }						\
     if (end == NULL) {				\
       first = AbsPair(HR);			\
     } else {					\
    end[0] = AbsPair(HR);\
  }\
  HR[0] = (CELL)ptd0;\
  end = HR + 1;\
  HR += 2;\
  * ptd0 = TermNone;			\
ptd0 += 2;					\
dd0 = *ptd0;\
 mderef_head(d0, dd0, var_in_term_unk);  \
goto  var_in_term_nvar;\
   }\
    }



/**
 *  @brief routine to locate all variables in a term, and its applications.
 */
static Term attterms_in_complex_term(Term t,
                                 Term tail  USES_REGS) {
  Term *end  = NULL, first = TermNil;
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_0()
  //  bindings to attributed vars are  trailed.
  #include "term_visit.h"
  if (first == TermNil)
    return tail;
  end[0] = tail;
  return first;
}


/** @pred term_attvars(+ _Term_,- _AttVars_)

    _AttVars_ is a list of all attributed variables in  _Term_ and
    its attributes. I.e., term_attvars/2 works recursively through
    attributes.  This predicate is Cycle-safe.
*/
static Int term_attterms(USES_REGS1)
{
    Term  t, out;
  t = Deref(ARG1);
  if ( IsPrimitiveTerm(t)) {
    out = TermNil;
    } else {
    out = attterms_in_complex_term(t, TermNil PASS_REGS);
    undo_vterms(out, TermNil PASS_REGS);
  }
  return Yap_unify(out,ARG2);
}

        
#undef COMPOUND_HOOK_CODE
#undef VAR_HOOK_CODE
#undef ATOMIC_HOOK_CODE


#define VAR_HOOK_CODE\
  {			\
INC_H(2,ts);\
ts[0] = (CELL)fvar;			\
if ( handle_singles){				\
    ts[1] = TermUnderscore;			\
} \
 else if (prefix) {				\
 char *s = (char *)HR;\
        utf8proc_ssize_t j = l, k=numbv++;\
     while (k) {					\
            s[j++] = k%26+'A';\
            k = k/26;\
        }\
        s[j] = '\0';\
	ts[1]=MkAtomTerm(Yap_LookupAtom(s));	\
    } else {\
      ts[1] = MkIntTerm(numbv++); \
     }\
 mBind(ptd0, AbsAppl(ts));\
 }

 
#define COMPOUND_HOOK_CODE   \
  if ( fvar == f  ) {\
 if ( ptd1[1] == TermUnderscore )       {	\
   if ( ptd0[-1] == (CELL)FunctorAttVar) {		\
  utf8proc_ssize_t j = 0, k=numbv++;			\
s[j++] = '_';\
s[j++] = 'D';\
     while (k) {					\
            s[j++] = k%26+'A';\
            k = k/26;\
        }\
        s[j] = '\0';\
	ptd1[1]=MkAtomTerm(Yap_LookupAtom(s));	\
   } else {					\
if (prefix) {					\
  utf8proc_ssize_t j = l, k=numbv++;			\
  if (k==0) {\
  s[j++] = 'A';\
  } else {\
     while (k) {					\
            s[j++] = k%26+'A';\
            k = k/26;\
        }\
  }						\
        s[j] = '\0';\
	ptd1[1]=MkAtomTerm(Yap_LookupAtom(s));	\
  }  \
 else {						\
  ptd1[1] = MkIntTerm(numbv++);			\
    }\
  }\
 }						\
goto loop;\
}

static int numbervars_in_complex_term(Term t,
				      int numbv , Functor fvar, bool  handle_singles , const char* prefix USES_REGS) {
  char s[64];
  ssize_t l;
  if (prefix) {
    strncpy(s, prefix, 63);
    l = strlen(s);
  }
  COPY(pt0_[1]);
#define RESET_TERM_VISITOR RESET_TERM_VISITOR_0()
  //  bindings to attributed vars are  trailed.
  #include "term_visit.h"
return numbv;
}

/** numbervariables in term t         */
int Yap_NumberVars(Term t, int numbv, Functor f, bool  handle_singles , const char* prefix USES_REGS)
{
  if (IsPrimitiveTerm(t)) {
    return numbv;
  }
numbv = numbervars_in_complex_term( t,
				    numbv , f,   handle_singles ,  prefix PASS_REGS);
  return numbv;
}

/** @pred  numbervars( _T_,+ _N1_,- _Nn_)

        Instantiates each variable in term  _T_ to a term of the form:
    `$VAR( _I_)`, with  _I_ increasing from  _N1_ to  _Nn_.*
*/
static Int numbervars(USES_REGS1)
{
  Functor f = Yap_MkFunctor(AtomOfTerm(getAtomicLocalPrologFlag(NUMBERVARS_FUNCTOR_FLAG)),1);
 Int out;
    Term t = Deref(ARG1);
    Term numbt = Deref(ARG2);
    if (IsVarTerm(numbt)) {
        Yap_Error(INSTANTIATION_ERROR, numbt, "numbervars/3");
        return false;
    } else
    if (!IsIntegerTerm(numbt)) {
        Yap_Error(TYPE_ERROR_INTEGER, numbt, "numbervars/3");
        return (false);
    }
    Int numbv = IntegerOfTerm(numbt);
    if (IsPrimitiveTerm(t)) {
      return Yap_unify(ARG3, numbt);
    }
    while (( out = Yap_NumberVars( t, numbv, f, false, NULL  PASS_REGS))< numbv) {
     if (!Yap_dogcl(0 PASS_REGS)) {
       Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	  }
	  t = Deref(ARG1);
    }
  return Yap_unify(ARG3, MkIntegerTerm(out));
}
 
/** @pred  singleton_vs_numbervars( _t,+ _N1_,- _Nn_)
    
    Instantiates each variable in term  _T_ to a term of the form:
    - `$VAR( PP_I_)`, with  _I_ increasing from  _N1_ to  _Nn_.
    - or of the form '_J` if `J is abs(I)`
 
    YAP also accepts atoms and strings.
*/
static Int singleton_vs_numbervars(USES_REGS1) {
  Functor f = Yap_MkFunctor(AtomOfTerm(getAtomicLocalPrologFlag(NUMBERVARS_FUNCTOR_FLAG)),1);
    Term t, numbt;
    size_t out;
    t = Deref(ARG1);
    numbt = Deref(ARG2);
    if (IsVarTerm(t)) {
    t = MkGlobal(t);
    }
    if (IsVarTerm(numbt)) {
        Yap_Error(INSTANTIATION_ERROR, numbt, "numbervars/3");
        return false;
    }
    if (!IsIntegerTerm(numbt)) {
        Yap_Error(TYPE_ERROR_INTEGER, numbt, "numbervars/3");
        return (false);
    }
    Int numbv = IntegerOfTerm(numbt);
    if (IsPrimitiveTerm(t)) {
        return Yap_unify(ARG3, numbt);
    }
    out = Yap_NumberVars( t, numbv,f, true, NULL PASS_REGS);
  return Yap_unify(ARG3, MkIntegerTerm(out));
}

#undef VAR_HOOK_CODE
#undef LIST_HOOK_CODE
#undef COMPOUND_HOOK_CODE

#define COMPOUND_HOOK_CODE   \
  if ( f == FunctorDollarVar  ) {	\
  if ( ptd1[1] == TermUnderscore ) {\
 mMaBind(ptd0,(CELL)ptd0);				\
  goto loop;					\
  }\
    HR[0] = AbsPair(HR+2);		\
    HR[1] = *bp; \
*bp = AbsPair(HR);\
HR[2] = ptd1[1]; /* key */			\
 HR[3] = (CELL) ptd0; /* variable */		\
 mMaBind(ptd0,(CELL)ptd0);				\
 bp = HR+1;					\
 HR+= 4;\
  goto loop;					\
    }


static bool unnumbervars_in_complex_term(Term t, CELL *HLow   USES_REGS) {
   CELL *hr0= HR;
   CELL bindings = TermNil;
   CELL * bp = &bindings;
 COPY(pt0_[1]);
// Numbervars
 //Functor FunctorDollarVar = Yap_MkFunctor(AtomOfTerm(getAtomicLocalProlo  } else  } else  } else  } elsegFlag(NUMBERVARS_FUNCTOR_FLAG)), 1);

#define RESET_TERM_VISITOR RESET_TERM_VISITOR_4()
#include "term_visit.h"
  // all bindings are left  trailed.
  if (bindings == TermNil) return true;
   t = Yap_MergeSort(AbsPair(hr0) PASS_REGS);
  if (t==0) {
    HR = hr0;
    return 0;
  }
  Term  v= 0, k=0;
    while (IsPairTerm(t)) {
      Term kv = HeadOfTerm(t);
      if (HeadOfTerm(kv)==k) {
	mSET(CellPtr(TailOfTerm(kv)), v);
      } else {
	k = HeadOfTerm(kv);
	v = *CellPtr(TailOfTerm(kv));
      }
      t = TailOfTerm(t);
    }
    HR = hr0;
    return true;
}

/** @pred unnumbervars(+ _Term_)

Replace all terms of the form $VAR(_) by unbound variables.
*/
static Int unnumbervars(USES_REGS1) /* variables in term t		 */
{
    Term  t;
  t = Deref(ARG1);
  return unnumbervars_in_complex_term(t, NULL PASS_REGS);
}

/** @pred Yap_Unnumbervars(+ _Term_)

Replace all terms of the form $VAR(_) by unbound variables.
*/
Int Yap_UnNumberTerm(Term t, CELL *HLow  USES_REGS) /* variables in term t		 */
{

  t = Deref(ARG1);
  Int  out = unnumbervars_in_complex_term(t, HLow PASS_REGS);
  return out;
}

/** @pred varnumbers(+ _Term_, _Term_)

Replace all terms of the form $VAR(_) by unbound variables.
*/
static Int varnumbers(USES_REGS1) /* variables in term t		 */
{
  Term  t0=Deref(ARG1), t, vs0, vs1;
#define SAVE_EXTRA() yhandle_t yv = Yap_InitHandle(t0)
#define RESTORE_EXTRA()  t0 = Yap_PopHandle(yv)

   vs0 = vars_in_complex_term(t0,TermNil PASS_REGS);

#undef SAVE_EXTRA
#undef RESTORE_EXTRA
#define SAVE_EXTRA() yhandle_t yv = Yap_InitHandle(vs0)
#define RESTORE_EXTRA()  {vs0 = Yap_PopHandle(yv);};
     t = Yap_CopyTerm(MkPairTerm(ARG1,vs0)); vs1 = TailOfTerm(t);
    t = HeadOfTerm(t);
    unnumbervars_in_complex_term(t, NULL  PASS_REGS);
    return Yap_unify(vs0,vs1) && Yap_unify(t,ARG2);
}

void Yap_InitTermCPreds(void) {
  CACHE_REGS
    Yap_InitCPred("cyclic_term", 1, cyclic_term, TestPredFlag);

    Yap_InitCPred("ground", 1, ground, TestPredFlag);
    Yap_InitCPred("non_ground", 2, ground, 0);
    Yap_InitCPred("numbervars", 3, numbervars, 0);
    Yap_InitCPred("$singleton_vs_numbervars", 3, singleton_vs_numbervars, 0);
    Yap_InitCPred("$varnumbers", 2, varnumbers, 0);
    Yap_InitCPredInModule("non_ground", 1, non_ground, 0, TERMS_MODULE);
    Yap_InitCPredInModule("variable_in_term", 2, var_in_term, 0, TERMS_MODULE);
    Yap_InitCPredInModule("new_variables_in_term", 3, p_new_variables_in_term, 0, TERMS_MODULE);
    Yap_InitCPredInModule("variables_in_both_terms", 3,term_variables_intersection, 0, TERMS_MODULE);
    Yap_InitCPred("unnumbervars", 1, unnumbervars, 0);
#if 1
    Yap_InitCPred("term_variables", 2, term_variables, 0);
    Yap_InitCPred("term_variables", 3, term_variables3, 0);
    Yap_InitCPred("term_variable_occurrences", 2, term_variable_occurrences, 0);
    Yap_InitCPred("term_variable_occurrences", 3, term_variable_occurrences3, 0);
    Yap_InitCPred("variables_in_term", 3, variables_in_term, 0);
    Yap_InitCPred("$variables_in_term", 3, variables_in_term, 0);

    Yap_InitCPred("$free_variables_in_term", 3, free_variables_in_term, 0);
    Yap_InitCPred("free_variables_in_term", 3, free_variables_in_term, 0);

    Yap_InitCPred("term_attvars", 2, term_attvars, 0);
    Yap_InitCPred("term_attterms", 2, term_attterms, 0);

    Yap_InitCPred("variables_in_both_terms", 3, term_variables_intersection, 0);
    Yap_InitCPred("variables_in_any_term", 3, term_variables_union, 0);






#endif
}

///@}

  

