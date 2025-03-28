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
* File:		mavar.c   						 *
* Last rev:								 *
* mods:									 *
* comments:	support from multiple assignment variables in YAP	 *
*									 *
*************************************************************************/


/** 

@file mavar.c

@brief Multi Assignment on Terms
*/

/**

@defgroup Term_Modification Term Modification
@ingroup YapExtensions
@{

It is sometimes useful to change the value of instantiated
variables. Although, this is against the spirit of logic programming, it
is sometimes useful. As in other Prolog systems, YAP has
several primitives that allow updating Prolog terms. Note that these
primitives are also backtrackable.

The setarg/3 primitive allows updating any argument of a Prolog
compound terms. The _mutable_ family of predicates provides
<em>mutable variables</em>. They should be used instead of setarg/3,
as they allow the encapsulation of accesses to updatable
variables. Their implementation can also be more efficient for long
deterministic computations.

YAP also include non_backtrackable versions of these routines, that
should be used with care.

 
*/

#include "Yap.h"

#include "Yatom.h"
#include "YapHeap.h"
#include "YapEval.h"
#include "YapArenas.h"

static Int p_setarg( USES_REGS1 );
static Int p_create_mutable( USES_REGS1 );
static Int p_get_mutable( USES_REGS1 );
static Int p_update_mutable( USES_REGS1 );
static Int p_is_mutable( USES_REGS1 );

/** @pred  setarg(+ _I_,+ _S_,? _T_) 


Set the value of the  _I_th argument of term  _S_ to term  _T_. 

 
*/
static Int
p_setarg( USES_REGS1 )
{
  CELL ti = Deref(ARG1), ts = Deref(ARG2), t3 = Deref(ARG3);
  Int i;

  if (IsVarTerm(t3) &&
      VarOfTerm(t3) > HR &&VarOfTerm(t3) < ASP) {
    /* local variable */
    Term tn = MkVarTerm();
    Bind_Local(VarOfTerm(t3), tn);
    t3 = tn;
  }
  if (IsVarTerm(ti)) {
    Yap_Error(INSTANTIATION_ERROR,ti,"setarg/3");
    return FALSE;
  } else {
    if (IsIntTerm(ti))
      i = IntOfTerm(ti);
    else {
      Term te = Yap_Eval(ti);
      if (IsIntegerTerm(te)) {
	i = IntegerOfTerm(te);
      } else {
	Yap_Error(TYPE_ERROR_INTEGER,ti,"setarg/3");
	return FALSE;
      }
    }
  }
  if (IsVarTerm(ts)) {
    Yap_Error(INSTANTIATION_ERROR,ts,"setarg/3");
  } else if(IsApplTerm(ts)) {
    CELL *pt;
    if (IsExtensionFunctor(FunctorOfTerm(ts))) {
      Yap_Error(TYPE_ERROR_COMPOUND,ts,"setarg/3");
      return FALSE;
    }
    if (i < 1 || i > (Int)ArityOfFunctor(FunctorOfTerm(ts))) {
      if (i<0)
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,ts,"setarg/3");
      return FALSE;
      if (i==0)
	Yap_Error(DOMAIN_ERROR_NOT_ZERO,ts,"setarg/3");
      return FALSE;
    }
    pt = RepAppl(ts)+i;
    /* the evil deed is to be done now */
    MaBind(pt, t3);
  } else if(IsPairTerm(ts)) {
    CELL *pt;
    if (i < 1 || i > 2) {
      if (i<0)
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,ts,"setarg/3");
      return FALSE;
    }
    pt = RepPair(ts)+i-1;
    /* the evil deed is to be done now */
    MaBind(pt, t3);    
  } else {
    Yap_Error(TYPE_ERROR_COMPOUND,ts,"setarg/3");
    return FALSE;
  }
  return TRUE;
}


/* One problem with MAVars is that they you always trail on
   non-determinate bindings. This is not cool if you have a long
   determinate computation. One alternative could be to use
   timestamps.

   Because of !, the only timestamp one can trust is the trailpointer
   (ouch..). The trail is not reclaimed after cuts. Also, if there was
   a conditional binding, the trail is sure to have been increased
   since the last choicepoint. For maximum effect, we can actually
   store the current value of TR in the timestamp field, giving a way
   to actually follow a link of all trailings for these variables.

*/

/* create and initialize a new timed var. The problem is: how to set
   the clock?

   If I give it the current value of B->TR, we may have trouble if no
   non-determinate bindings are made before the next
   choice-point. Just to make sure this doesn't cause trouble, if (TR
   == B->TR) we will add a little something ;-).    
 */

static Term
NewTimedVar(CELL val USES_REGS)
{
  Term out;
  timed_var *tv;
  if (IsVarTerm(val) &&
      VarOfTerm(val) > HR) {
    Term nval = MkVarTerm();
    Bind_Local(VarOfTerm(val), nval);
    val = nval;
  }
  out = AbsAppl(HR);
  *HR++ = (CELL)FunctorMutable;
  tv = (timed_var *)HR;
  RESET_VARIABLE(&(tv->clock));
  tv->value = val;
  HR += sizeof(timed_var)/sizeof(CELL);
  return(out);
}

Term
Yap_NewTimedVar(CELL val)
{
  CACHE_REGS
  return NewTimedVar(val PASS_REGS);
}

Term
Yap_NewEmptyTimedVar( void )
{
  CACHE_REGS
  Term out = AbsAppl(HR);
  timed_var *tv;
  *HR++ = (CELL)FunctorMutable;
  tv = (timed_var *)HR;
  RESET_VARIABLE(&(tv->clock));
  RESET_VARIABLE(&(tv->value));
  HR += sizeof(timed_var)/sizeof(CELL);
  return(out);
}

Term
Yap_NewCompactTimedVar( Term v )
{
  CACHE_REGS
  Term out = AbsAppl(HR);
  timed_var *tv;
  *HR++ = (CELL)FunctorMutable;
  tv = (timed_var *)HR;
  RESET_VARIABLE(&(tv->clock));
  tv->value = v;
  HR += sizeof(timed_var)/sizeof(CELL);
  return(out);
}

static Term
ReadTimedVar(Term inv)
{
  timed_var *tv = (timed_var *)(RepAppl(inv)+1);
  return(Deref(tv->value));
}

Term
Yap_ReadTimedVar(Term inv)
{
  return ReadTimedVar(inv);
}


/* update a timed var with a new value */
static Term
UpdateTimedVar(Term inv, Term new USES_REGS)
{
  timed_var *tv = (timed_var *)(RepAppl(inv)+1);
  CELL t = tv->value;
  CELL* timestmp = (CELL *)(tv->clock);
  if (IsVarTerm(new) &&
      VarOfTerm(new) > HR) {
    Term nnew = MkVarTerm();
    Bind_Local(VarOfTerm(new), nnew);
    new = nnew;
  }
  if (timestmp > B->cp_h
#if FROZEN_STACKS
      && timestmp > H_FZ
#endif
      ) {
    /* last assignment more recent than last B */
#if YAPOR_SBA
    if (Unsigned((Int)(tv)-(Int)(H_FZ)) >
	Unsigned((Int)(B_FZ)-(Int)(H_FZ)))
      *STACK_TO_SBA(&(tv->value)) = new;
    else
#endif
      tv->value = new;
  } else {
    Term nclock = (Term)HR;
    *HR++ = TermFoundVar;
    MaBind(&(tv->clock), nclock);
    MaBind(&(tv->value), new);
  }
  return(t);
}

/* update a timed var with a new value */
Term
Yap_UpdateTimedVar(Term inv, Term new)
{
  CACHE_REGS
  return UpdateTimedVar(inv, new PASS_REGS);
}

/** @pred  create_mutable(+ _D_,- _M_) 


Create new mutable variable  _M_ with initial value  _D_.
*/
static Int
p_create_mutable( USES_REGS1 )
{
  Term t = NewTimedVar(Deref(ARG1) PASS_REGS);
  return(Yap_unify(ARG2,t));
}

/** @pred  get_mutable(? _D_,+ _M_) 


Unify the current value of mutable term  _M_ with term  _D_.
*/
static Int
p_get_mutable( USES_REGS1 )
{
  Term t = Deref(ARG2);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "get_mutable/3");
    return(FALSE);
  }
  if (!IsApplTerm(t)) {
    Yap_Error(TYPE_ERROR_COMPOUND,t,"get_mutable/3");
    return(FALSE);
  }
  if (FunctorOfTerm(t) != FunctorMutable) { 
    Yap_Error(DOMAIN_ERROR_MUTABLE,t,"get_mutable/3");
    return(FALSE);
  }
  t = ReadTimedVar(t);
  return(Yap_unify(ARG1, t));
}

/** @pred  update_mutable(+ _D_,+ _M_) 


Set the current value of mutable term  _M_ to term  _D_.



 */
static Int
p_update_mutable( USES_REGS1 )
{
  Term t = Deref(ARG2);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "update_mutable/3");
    return(FALSE);
  }
  if (!IsApplTerm(t)) {
    Yap_Error(TYPE_ERROR_COMPOUND,t,"update_mutable/3");
    return(FALSE);
  }
  if (FunctorOfTerm(t) != FunctorMutable) { 
    Yap_Error(DOMAIN_ERROR_MUTABLE,t,"update_mutable/3");
    return(FALSE);
  }
  UpdateTimedVar(t, Deref(ARG1) PASS_REGS);
  return(TRUE);
}

/** @pred  is_mutable(? _D_) 


Holds if  _D_ is a mutable term.
*/
static Int
p_is_mutable( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    return(FALSE);
  }
  if (!IsApplTerm(t)) {
    return(FALSE);
  }
  if (FunctorOfTerm(t) != FunctorMutable) { 
    return(FALSE);
  }
  return(TRUE);
}

    /** @pred  nb_setarg(+{Arg], + _Term_, + _Value_)



        Assigns the  _Arg_-th argument of the compound term  _Term_ with
        the given  _Value_ as setarg/3, but on backtracking the assignment
        is not reversed. If  _Term_ is not atomic, it is duplicated using
        duplicate_term/2. This predicate uses the same technique as
        nb_setval/2. We therefore refer to the description of
        nb_setval/2 for details on non-backtrackable assignment of
        terms. This predicate is compatible to GNU-Prolog
        `setarg(A,T,V,false)`, removing the type-restriction on
        _Value_. See also nb_linkarg/3. Below is an example for
        counting the number of solutions of a goal. Note that this
        implementation is thread-safe, reentrant and capable of handling
        exceptions. Realising these features with a traditional
        implementation based on assert/retract or flag/3 is much more
        complicated.

        ```
        succeeds_n_times(Goal, Times) :-
        Counter = counter(0),
        (   Goal,
        arg(1, Counter, N0),
        N is N0 + 1,
        nb_setarg(1, Counter, N),
        fail
        ;   arg(1, Counter, Times)
        ).
        ```


    */
static Int nb_setarg(USES_REGS1) {
    Term wheret = Deref(ARG1);
    Term dest;
    Term to;
    UInt arity, pos;
    CELL *destp;

    if (IsVarTerm(wheret)) {
        Yap_ThrowError(INSTANTIATION_ERROR, wheret, "nb_setarg");
        return FALSE;
    }
    if (!IsIntegerTerm(wheret)) {
        Yap_ThrowError(TYPE_ERROR_INTEGER, wheret, "nb_setarg");
        return FALSE;
    }
    pos = IntegerOfTerm(wheret);
    dest = Deref(ARG2);
    if (IsVarTerm(dest)) {
        Yap_ThrowError(INSTANTIATION_ERROR, dest, "nb_setarg");
        return FALSE;
    } else if (IsPrimitiveTerm(dest)) {
        arity = 0;
    } else if (IsPairTerm(dest)) {
        arity = 2;
    } else {
        arity = ArityOfFunctor(FunctorOfTerm(dest));
    }
    if (pos < 1 || pos > arity)
        return false;
    COPY(ARG3);

    to = Deref(ARG3);
    if (!IsPrimitiveTerm(to)) {
    to = CopyTermToArena(Deref(ARG3), FALSE, TRUE, NULL, &LOCAL_GlobalArena, NULL
                         PASS_REGS);
    if (to == 0L)
      return false;
    }

    dest = Deref(ARG2);
    if (IsPairTerm(dest)) {
        destp = RepPair(dest) - 1;
    } else {
        destp = RepAppl(dest);
    }
    destp[pos] = to;
    return TRUE;
}

    /** @pred  nb_set_shared_arg(+ _Arg_, + _Term_, + _Value_)



        As nb_setarg/3, but like nb_linkval/2 it does not
        duplicate the global sub-terms in  _Value_. Use with extreme care
        and consult the documentation of nb_linkval/2 before use.


    */
static Int nb_set_shared_arg(USES_REGS1) {
    Term wheret = Deref(ARG1);
    Term dest;
    Term to;
    UInt arity, pos;
    CELL *destp;

    if (IsVarTerm(wheret)) {
        Yap_ThrowError(INSTANTIATION_ERROR, wheret, "nb_setarg");
        return FALSE;
    }
    if (!IsIntegerTerm(wheret)) {
        Yap_ThrowError(TYPE_ERROR_INTEGER, wheret, "nb_setarg");
        return FALSE;
    }
    pos = IntegerOfTerm(wheret);
    dest = Deref(ARG2);
    if (IsVarTerm(dest)) {
        Yap_ThrowError(INSTANTIATION_ERROR, dest, "nb_setarg");
        return FALSE;
    } else if (IsPrimitiveTerm(dest)) {
        arity = 0;
    } else if (IsPairTerm(dest)) {
        arity = 2;
    } else {
        arity = ArityOfFunctor(FunctorOfTerm(dest));
    }
    if (pos < 1 || pos > arity)
        return FALSE;
    COPY(ARG3);
    to = CopyTermToArena(Deref(ARG3), TRUE, TRUE, NULL, &LOCAL_GlobalArena, NULL PASS_REGS);
    if (to == 0L)
        return FALSE;
    dest = Deref(ARG2);
    if (IsPairTerm(dest)) {
        destp = RepPair(dest) - 1;
    } else {
        destp = RepAppl(dest);
    }
    destp[pos] = to;
    return TRUE;
}

/** @pred  nb_linkarg(+ _Arg_, + _Term_, + _Value_)



        As nb_setarg/3, but like nb_linkval/2 it does not
        duplicate  _Value_. Use with extreme care and consult the
        documentation of nb_linkval/2 before use.


    */
static Int nb_linkarg(USES_REGS1) {
    Term wheret = Deref(ARG1);
    Term dest;
    UInt arity, pos;
    CELL *destp;

    if (IsVarTerm(wheret)) {
        Yap_ThrowError(INSTANTIATION_ERROR, wheret, "nb_setarg");
        return FALSE;
    }
    if (!IsIntegerTerm(wheret)) {
        Yap_ThrowError(TYPE_ERROR_INTEGER, wheret, "nb_setarg");
        return FALSE;
    }
    pos = IntegerOfTerm(wheret);
    dest = Deref(ARG3);
    if (IsVarTerm(dest)) {
        Yap_ThrowError(INSTANTIATION_ERROR, dest, "nb_setarg");
        return FALSE;
    } else if (IsPrimitiveTerm(dest)) {
        arity = 0;
        destp = NULL;
    } else if (IsPairTerm(dest)) {
        arity = 2;
        destp = RepPair(dest) - 1;
    } else {
        arity = ArityOfFunctor(FunctorOfTerm(dest));
        destp = RepAppl(dest);
    }
    if (pos < 1 || pos > arity)
        return FALSE;
    dest = Deref(ARG2);
    destp[pos] = Deref(ARG3);
    return TRUE;
}

/** @pred  nb_linkval(+ _Name_, + _Value_)


        Associates the term  _Value_ with the atom  _Name_ without
        copying it. This is a fast special-purpose variation of nb_setval/2
        intended for expert users only because the semantics on backtracking
        to a point before creating the link are poorly defined for compound
        terms. The principal term is always left untouched, but backtracking
        behaviour on arguments is undone if the original assignment was
        trailed and left alone otherwise, which implies that the history
        that created the term affects the behaviour on backtracking. Please
        consider the following example:

        ```
        demo_nb_linkval :-
        T = nice(N),
        (   N = world,
        nb_linkval(myvar, T),
        fail
        ;   nb_getval(myvar, V),
        writeln(V)
        ).
        ```


    */
static Int nb_linkval(USES_REGS1) {
    Term t = Deref(ARG1), to;
    GlobalEntry *ge;
    if (IsVarTerm(t)) {
        Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_linkval");
        return (TermNil);
    } else if (!IsAtomTerm(t)) {
        Yap_ThrowError(TYPE_ERROR_ATOM, t, "nb_linkval");
        return (FALSE);
    }
    ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
    to = Deref(ARG2);
    ge->global = to;
    return TRUE;
}


void
Yap_InitMaVarCPreds(void)
{
#ifdef MULTI_ASSIGNMENT_VARIABLES
  Yap_InitCPred("setarg", 3, p_setarg, SafePredFlag);  
  Yap_InitCPred("nb_setarg", 3, nb_setarg, 0L);
  Yap_InitCPred("create_mutable", 2, p_create_mutable, SafePredFlag);  
  Yap_InitCPred("get_mutable", 2, p_get_mutable, SafePredFlag);  
  Yap_InitCPred("update_mutable", 2, p_update_mutable, SafePredFlag);  
  Yap_InitCPred("is_mutable", 1, p_is_mutable, SafePredFlag);  
    Yap_InitCPred("nb_set_shared_arg", 3, nb_set_shared_arg, 0L);
    Yap_InitCPred("nb_linkval", 2, nb_linkval, 0L);
    Yap_InitCPred("nb_linkarg", 3, nb_linkarg, 0L);
#endif
}

/**
@}
*/
