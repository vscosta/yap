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
* File:		inlines.c						 *
* Last rev:								 *
* mods:									 *
* comments:	C-version for inline code used in meta-calls	         *
*									 *
*************************************************************************/


/** 


    @file inlines.c


    @defgroup YAP_Inlines Inlined Tests nad Ter Manipulation

    @ingroup builtins

   @{

*/


#define IN_INLINES_C 1

#include "absmi.h"

#include "cut_c.h"

static Int    p_atom( USES_REGS1 );
static Int    p_atomic( USES_REGS1 );
static Int    p_integer( USES_REGS1 );
static Int    p_nonvar( USES_REGS1 );
static Int    p_number( USES_REGS1 );
static Int    p_var( USES_REGS1 );
static Int    p_db_ref( USES_REGS1 );
static Int    p_primitive( USES_REGS1 );
static Int    p_compound( USES_REGS1 );
static Int    p_float( USES_REGS1 );
static Int    p_equal( USES_REGS1 );
static Int    p_dif( USES_REGS1 );
static Int    p_eq( USES_REGS1 );
static Int    p_arg( USES_REGS1 );
static Int    p_functor( USES_REGS1 );
static Int    p_fail( USES_REGS1 );
static Int    p_true( USES_REGS1 );

/** @pred  fail is iso

Always fails. Defined as if by:

 ~~~~~
 fail :- 2=1.
 ~~~~~
*/

/** @pred  false is iso

The same as fail. Defined as if by:

 ~~~~~
 false :- 2=1.
 ~~~~~
*/

static Int    p_fail( USES_REGS1 )
{
    return false;
}

/** @pred true is iso
Succeed.

Succeeds once. Defined as if by:

 ~~~~~
 true :- true.
 ~~~~~
*/

/** @pred otherwise is iso
Succeed.

Succeeds once. Defined as if by:

 ~~~~~
 otherwise.
 ~~~~~
*/


static Int    p_true( USES_REGS1 )
{
    return true;
}


/** @pred  atom( _T_) is iso


Succeeds if and only if  _T_ is currently instantiated to an  atom.


*/
static Int
p_atom( USES_REGS1 )
{				/* atom(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, atom_unk);
    atom_nvar:
      if (IsAtomTerm(d0)  && !IsBlob(AtomOfTerm(d0))) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, atom_unk, atom_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

/** @pred  atomic(T) is iso


Checks whether  _T_ is an atomic symbol (atom or number).


*/
static Int
p_atomic( USES_REGS1 )
{				/* atomic(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, atomic_unk);
    atomic_nvar:
      if (IsAtomicTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, atomic_unk, atomic_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

/** @pred  integer( _T_) is iso


Succeeds if and only if  _T_ is currently instantiated to an  integer.


*/
static Int
p_integer( USES_REGS1 )
{				/* integer(?,?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, integer_unk);
    integer_nvar:
      if (IsIntTerm(d0)) {
	return(TRUE);
      }
      if (IsApplTerm(d0)) {
	Functor f0 = FunctorOfTerm(d0);
	if (IsExtensionFunctor(f0)) {
	  switch ((CELL)f0) {
	  case (CELL)FunctorBigInt:
	    { CELL *pt = RepAppl(d0);
	      if ( pt[1] != BIG_INT ) {
		return FALSE;
	      }
        return TRUE;
	    }
	  case (CELL)FunctorLongInt:
	    return(TRUE);
	  default:
	    return(FALSE);
	  }
	}
	return(FALSE);
      } else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, integer_unk, integer_nvar);
      ENDP(pt0);
      return(FALSE);
      ENDD(d0);
}

/** @pred  number( _T_) is iso


Checks whether `T` is an integer, rational or a float.


*/
static Int
p_number( USES_REGS1 )
{				/* number(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, number_unk);
    number_nvar:
      if (IsIntTerm(d0)) {
	return(TRUE);
      }
      if (IsApplTerm(d0)) {
	Functor f0 = FunctorOfTerm(d0);
	if (IsExtensionFunctor(f0)) {
	  switch ((CELL)f0) {
	  case (CELL)FunctorBigInt:
	    { CELL *pt = RepAppl(d0);
	      if (  pt[1] != BIG_RATIONAL || pt[1] != BIG_INT ) {
		return FALSE;
	      }
        return(TRUE);
    }
	  case (CELL)FunctorLongInt:
	  case (CELL)FunctorDouble:
	    return(TRUE);
	  default:
	    return(FALSE);
	  }
	}
	return(FALSE);
      } else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, number_unk, number_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

/** @pred  db_reference( _T_)


Checks whether  _T_ is a database reference.


*/
static Int
p_db_ref( USES_REGS1 )
{				/* db_reference(?,?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, db_ref_unk);
    db_ref_nvar:
      if (IsDBRefTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, db_ref_unk, db_ref_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

/** @pred  primitive( ?_T_)


Checks whether  _T_ is an atomic term or a database reference.


*/
static Int
p_primitive( USES_REGS1 )
{				/* primitive(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, primitive_unk);
    primitive_nvar:
      if (IsPrimitiveTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, primitive_unk, primitive_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

/** @pred  float( _T_) is iso


Checks whether  _T_ is a floating point number.


*/
static Int
p_float( USES_REGS1 )
{				/* float(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, float_unk);
    float_nvar:
      if (IsFloatTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, float_unk, float_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

/** @pred  compound( _T_) is iso


Checks whether  _T_ is a compound term.


*/
static Int
p_compound( USES_REGS1 )
{				/* compound(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, compound_unk);
    compound_nvar:
      if (IsPairTerm(d0)) {
	return(TRUE);
      }
      else if (IsApplTerm(d0)) {
	if (IsExtensionFunctor(FunctorOfTerm(d0))) {
	  return(FALSE);
	}
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, compound_unk, compound_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

/** @pred  nonvar( _T_) is iso


The opposite of `var( _T_)`.


*/
static Int
p_nonvar( USES_REGS1 )
{				/* nonvar(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, nonvar_unk);
    nonvar_nvar:
      return(TRUE);

      BEGP(pt0);
      deref_body(d0, pt0, nonvar_unk, nonvar_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

/** @pred  var( _T_) is iso


Succeeds if  _T_ is currently a free variable, otherwise fails.


*/
static Int
p_var( USES_REGS1 )
{				/* var(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, var_unk);
    var_nvar:
      return(FALSE);

      BEGP(pt0);
      deref_body(d0, pt0, var_unk, var_nvar);
      return(TRUE);
      ENDP(pt0);
      ENDD(d0);
}

/** @pred  _X_ =  _Y_ is iso


Tries to unify terms  _X_ and  _Y_.


*/
static Int
p_equal( USES_REGS1 )
{				/* ?=? */
  return(Yap_IUnify(ARG1, ARG2));
}

static Int
eq(Term t1, Term t2 USES_REGS)
{				/* ? == ? */
      BEGD(d0);
      d0 = t1;
      deref_head(d0, p_eq_unk1);
    p_eq_nvar1:
      /* first argument is bound */
      BEGD(d1);
      d1 = t2;
      deref_head(d1, p_eq_nvar1_unk2);
    p_eq_nvar1_nvar2:
      /* both arguments are bound */
      if (d0 == d1) {
	return(TRUE);
      }
      if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  return(FALSE);
	}
	return(iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1));
      }
      if (IsApplTerm(d0)) {
	Functor f0 = FunctorOfTerm(d0);
	Functor f1;
	if (!IsApplTerm(d1)) {
	  return(FALSE);
	}
	f1 = FunctorOfTerm(d1);
	if (f0 != f1) {
	  return(FALSE);
	}
	if (IsExtensionFunctor(f0)) {
	  switch ((CELL)f0) {
	  case (CELL)FunctorDBRef:
	    return (d0 == d1);
	  case (CELL)FunctorLongInt:
	    return(LongIntOfTerm(d0) == LongIntOfTerm(d1));
	  case (CELL)FunctorString:
	    return(strcmp((char *)StringOfTerm(d0), (char *)StringOfTerm(d1)) == 0);
#ifdef USE_GMP
	  case (CELL)FunctorBigInt:
	    return (Yap_gmp_tcmp_big_big(d0, d1) == 0);
#endif
	  case (CELL)FunctorDouble:
	    return(FloatOfTerm(d0) == FloatOfTerm(d1));
	  default:
	    return(FALSE);
	  }
	}
	return(iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1)));
      }
      return(FALSE);

      BEGP(pt0);
      deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2);
      ENDP(pt0);
      /* first argument is bound */
      /* second argument is unbound */
      /* I don't need to worry about co-routining because an
	 unbound variable may never be == to a constrained variable!! */
      return(FALSE);
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1);
      BEGD(d1);
      d1 = ARG2;
      deref_head(d1, p_eq_var1_unk2);
    p_eq_var1_nvar2:
      /* I don't need to worry about co-routining because an
	 unbound variable may never be == to a constrained variable!! */
      return(FALSE);

      BEGP(pt1);
      deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2);
      /* first argument is unbound */
      /* second argument is unbound */
      return(pt1 == pt0);
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      ENDD(d0);
}



/** @pred ?_X_ ==  ?_Y_ is iso

Succeeds if terms  _X_ and  _Y_ are strictly identical. The
difference between this predicate and =/2 is that, if one of the
arguments is a free variable, it only succeeds when they have already
been unified.

~~~~~
?- X == Y.
~~~~~
fails, but,

~~~~~
?- X = Y, X == Y.
~~~~~
succeeds.

~~~~~
?- X == 2.
~~~~~
fails, but,

~~~~~
?- X = 2, X == 2.
~~~~~
succeeds.


*/
static Int
p_eq( USES_REGS1 )
{				/* ? == ? */
  return eq(ARG1,ARG2 PASS_REGS);
}

int
Yap_eq(Term t1, Term t2)
{				/* ? == ? */
  CACHE_REGS
  return eq(t1,t2 PASS_REGS);
}

/** @pred  _X_ \=  _Y_ is iso


Succeeds if terms  _X_ and  _Y_ are not unifiable.


*/
static Int
p_dif( USES_REGS1 )
{				/* ? \= ?  */
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif
  BEGD(d0);
  BEGD(d1);
  d0 = ARG1;
  deref_head(d0, dif_unk1);
 dif_nvar1:
  /* first argument is bound */
  d1 = ARG2;
  deref_head(d1, dif_nvar1_unk2);
 dif_nvar1_nvar2:
  /* both arguments are bound */
  if (d0 == d1) {
    return FALSE;
  }
  if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) {
    return TRUE;
  } else {
#ifdef COROUTINING
    /*
     * We may wake up goals during our attempt to unify the
     * two terms. If we are adding to the tail of a list of
     * woken goals that should be ok, but otherwise we need
     * to restore LOCAL_WokenGoals to its previous value.
     */
    CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals);
#endif
    register tr_fr_ptr pt0;
    /* store the old value of TR for clearing bindings */
    pt0 = TR;
    BEGCHO(pt1);
    pt1 = B;
    /* make B and HB point to H to guarantee all bindings will
     * be trailed
     */
    HBREG = HR;
    B = (choiceptr) HR;
    B->cp_h = HR;
    SET_BB(B);
    save_hb();
    d0 = Yap_IUnify(d0, d1);
#ifdef COROUTINING
    /* now restore Woken Goals to its old value */
    Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals);
    if (OldWokenGoals == TermNil) {
      Yap_get_signal(YAP_WAKEUP_SIGNAL);
    }
#endif
    /* restore B */
    B = pt1;
    SET_BB(PROTECT_FROZEN_B(pt1));
#ifdef COROUTINING
    HR = HBREG;
#endif
    HBREG = B->cp_h;
    /* untrail all bindings made by Yap_IUnify */
    while (TR != pt0) {
      BEGD(d1);
      d1 = TrailTerm(--TR);
      if (IsVarTerm(d1)) {
#if defined(YAPOR_SBA) && defined(YAPOR)
	/* clean up the trail when we backtrack */
	if (Unsigned((Int)(d1)-(Int)(H_FZ)) >
	    Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	  RESET_VARIABLE(STACK_TO_SBA(d1));
	} else
#endif
	  /* normal variable */
	  RESET_VARIABLE(d1);
#ifdef MULTI_ASSIGNMENT_VARIABLES
      } else /* if (IsApplTerm(d1)) */ {
	CELL *pt = RepAppl(d1);
	/* AbsAppl means */
	/* multi-assignment variable */
	/* so the next cell is the old value */
#ifdef FROZEN_STACKS
	pt[0] = TrailVal(--TR);
#else
	pt[0] = TrailTerm(--TR);
	TR--;
#endif /* FROZEN_STACKS */
#endif /* MULTI_ASSIGNMENT_VARIABLES */
      }
      ENDD(d1);
    }
    return !d0;
    ENDP(pt0);
  }

  BEGP(pt0);
  deref_body(d0, pt0, dif_unk1, dif_nvar1);
  ENDP(pt0);
  /* first argument is unbound */
  return FALSE;

  BEGP(pt0);
  deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2);
  ENDP(pt0);
  /* second argument is unbound */
  return FALSE;
  ENDD(d1);
  ENDD(d0);
}


/** @pred  arg(+ _N_,+ _T_, _A_) is iso


Succeeds if the argument  _N_ of the term  _T_ unifies with
 _A_. The arguments are numbered from 1 to the arity of the term.

The current version will generate an error if  _T_ or  _N_ are
unbound, if  _T_ is not a compound term, of if  _N_ is not a positive
integer. Note that previous versions of YAP would fail silently
under these errors.


*/
static Int
p_arg( USES_REGS1 )
{				/* arg(?,?,?)	 */
#if SHADOW_HB
      register CELL *HBREG = HB;
#endif
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, arg_arg1_unk);
    arg_arg1_nvar:
      /* ARG1 is ok! */
      if (IsIntTerm(d0))
	d0 = IntOfTerm(d0);
      else if (IsLongIntTerm(d0)) {
	d0 = LongIntOfTerm(d0);
      } else {
	if (!IsBigIntTerm( d0 ))
	  Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3");
	return(FALSE);
      }

      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = ARG2;
      deref_head(d1, arg_arg2_unk);
    arg_arg2_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
	BEGP(pt0);
	pt0 = RepAppl(d1);
	d1 = *pt0;
	if (IsExtensionFunctor((Functor) d1)) {
	  Yap_Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	  return(FALSE);
	}
	save_hb();
	if ((Int)d0 <= 0 ||
	    (Int)d0 > ArityOfFunctor((Functor) d1) ||
	    Yap_IUnify(pt0[d0], ARG3) == FALSE) {
	  /* don't complain here for Prolog compatibility
	  if ((Int)d0 <= 0) {
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");
	  }
	  */
	  return(FALSE);
	}
	return(TRUE);
	ENDP(pt0);
      }
      else if (IsPairTerm(d1)) {
	BEGP(pt0);
	pt0 = RepPair(d1);
	if (d0 == 1) {
	  save_hb();
	  if (Yap_IUnify((CELL)pt0, ARG3) == FALSE) {
	    return(FALSE);
	  }
	  return(TRUE);
	}
	else if (d0 == 2) {
	  save_hb();
	  if (Yap_IUnify((CELL)(pt0+1), ARG3) == FALSE) {
	    return(FALSE);
	  }
	  return(TRUE);
	}
	else {
	  if ((Int)d0 < 0)
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");
	  return(FALSE);
	}
	ENDP(pt0);
      }
      else {
	Yap_Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_arg2_unk, arg_arg2_nvar);
      Yap_Error(INSTANTIATION_ERROR,(CELL)pt0,"arg 2 of arg/3");;
      ENDP(pt0);
      return(FALSE);
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, arg_arg1_unk, arg_arg1_nvar);
      Yap_Error(INSTANTIATION_ERROR,(CELL)pt0,"arg 1 of arg/3");;
      ENDP(pt0);
      return(FALSE);
      ENDD(d0);

}

/** @pred  functor( _T_, _F_, _N_) is iso


The top functor of term  _T_ is named  _F_ and has  arity  _N_.

When  _T_ is not instantiated,  _F_ and  _N_ must be. If
 _N_ is 0,  _F_ must be an atomic symbol, which will be unified
with  _T_. If  _N_ is not 0, then  _F_ must be an atom and
 _T_ becomes instantiated to the most general term having functor
 _F_ and arity  _N_. If  _T_ is instantiated to a term then
 _F_ and  _N_ are respectively unified with its top functor name
and arity.

In the current version of YAP the arity  _N_ must be an
integer. Previous versions allowed evaluable expressions, as long as the
expression would evaluate to an integer. This feature is not available
in the ISO Prolog standard.


*/
static Int
p_functor( USES_REGS1 )			/* functor(?,?,?) */
{
#if SHADOW_HB
  register CELL *HBREG;
#endif

 restart:
#if SHADOW_HB
  HBREG = HB;
#endif
  BEGD(d0);
  d0 = ARG1;
  deref_head(d0, func_unk);
 func_nvar:
  /* A1 is bound */
  BEGD(d1);
  if (IsApplTerm(d0)) {
    d1 = *RepAppl(d0);
    if (IsExtensionFunctor((Functor) d1)) {
      if (d1 == (CELL)FunctorDouble) {
	d1 = MkIntTerm(0);
      } else if (d1 == (CELL)FunctorLongInt) {
	d1 = MkIntTerm(0);
      } else if (d1 == (CELL)FunctorString) {
	d1 = MkIntTerm(0);
      } else
	  return(FALSE);
    } else {
      d0 = MkAtomTerm(NameOfFunctor((Functor) d1));
      d1 = MkIntTerm(ArityOfFunctor((Functor) d1));
    }
  }
  else if (IsPairTerm(d0)) {
    d0 = TermDot;
    d1 = MkIntTerm(2);
  }
  else {
    d1 = MkIntTerm(0);
  }
  /* d1 and d0 now have the two arguments */
  /* let's go and bind them */
  {
    register CELL arity = d1;

    d1 = ARG2;
    deref_head(d1, func_nvar_unk);
  func_nvar_nvar:
    /* A2 was bound */
    if (d0 != d1) {
	return(FALSE);
    }
    /* have to buffer ENDP and label */
    d0 = arity;
    goto func_bind_x3;

    BEGP(pt0);
    deref_body(d1, pt0, func_nvar_unk, func_nvar_nvar);
    /* A2 is a variable, go and bind it */
    YapBind(pt0, d0);
    /* have to buffer ENDP and label */
    d0 = arity;
    ENDP(pt0);
    /* now let's process A3 */

  func_bind_x3:
    d1 = ARG3;
    deref_head(d1, func_nvar3_unk);
  func_nvar3_nvar:
    /* A3 was bound */
    if (d0 != d1) {
	return(FALSE);
    }
    /* Done */
    return(TRUE);


    BEGP(pt0);
    deref_body(d1, pt0, func_nvar3_unk, func_nvar3_nvar);
    /* A3 is a variable, go and bind it */
    YapBind(pt0, d0);
    return(TRUE);

    ENDP(pt0);

  }
  ENDD(d1);

  BEGP(pt0);
  deref_body(d0, pt0, func_unk, func_nvar);
  /* A1 is a variable */
  /* We have to build the structure */
  d0 = ARG2;
  deref_head(d0, func_var_2unk);
 func_var_2nvar:
  /* we do, let's get the third argument */
  BEGD(d1);
  d1 = ARG3;
  deref_head(d1, func_var_3unk);
 func_var_3nvar:
  /* Uuuff, the second and third argument are bound */
  if (IsIntegerTerm(d1))
    d1 = IntegerOfTerm(d1);
  else {
    if (IsBigIntTerm(d1)) {
      Yap_Error(RESOURCE_ERROR_STACK, ARG3, "functor/3");
    } else {
      Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3");
    }
    return(FALSE);
  }
  if (!IsAtomicTerm(d0)) {
    Yap_Error(TYPE_ERROR_ATOMIC,d0,"functor/3");
    return(FALSE);
  }
  /* We made it!!!!! we got in d0 the name, in d1 the arity and
   * in pt0 the variable to bind it to. */
  if (d0 == TermDot && d1 == 2) {
    RESET_VARIABLE(HR);
    RESET_VARIABLE(HR+1);
    d0 = AbsPair(HR);
    HR += 2;
  }
  else if ((Int)d1 > 0) {
    /* now let's build a compound term */
    if (!IsAtomTerm(d0)) {
      Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
      return(FALSE);
    }
    BEGP(pt1);
    if (!IsAtomTerm(d0)) {
      return(FALSE);
    }
    else
      d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
    pt1 = HR;
    *pt1++ = d0;
    d0 = AbsAppl(HR);
    if (d1 > 16 && pt1+d1 > ENV - StackGap( PASS_REGS1 )) {
      if (!Yap_dogc(3, NULL PASS_REGS)) {
	Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	return FALSE;
      }
      goto restart;
    }
    while (d1-- > 0) {
      RESET_VARIABLE(pt1);
      pt1++;
    }
    /* done building the term */
    HR = pt1;
    ENDP(pt1);
  } else if ((Int)d1  < 0) {
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
    return(FALSE);
  }
  /* else if arity is 0 just pass d0 through */
  /* Ding, ding, we made it */
  YapBind(pt0, d0);
  return(TRUE);


  BEGP(pt1);
  deref_body(d1, pt1, func_var_3unk, func_var_3nvar);
  Yap_Error(INSTANTIATION_ERROR,(CELL)pt1,"functor/3");
  ENDP(pt1);
  /* Oops, third argument was unbound */
  return(FALSE);
  ENDD(d1);

  BEGP(pt1);

  deref_body(d0, pt1, func_var_2unk, func_var_2nvar);
  Yap_Error(INSTANTIATION_ERROR,(CELL)pt1,"functor/3");
  ENDP(pt1);
  /* Oops, second argument was unbound too */
  return(FALSE);
  ENDP(pt0);
  ENDD(d0);
}

static Term
cp_as_integer(choiceptr cp USES_REGS)
{
  return(MkIntegerTerm(LCL0-(CELL *)cp));
}


static Int
p_cut_by( USES_REGS1 )
{
  BEGD(d0);
  d0 = ARG1;
  deref_head(d0, cutby_x_unk);
 cutby_x_nvar:
#if YAPOR_SBA
  if (!IsIntegerTerm(d0)) {
#else
  if (!IsIntTerm(d0)) {
#endif
    return(FALSE);
  }
  BEGCHO(pt0);
#if YAPOR_SBA
  pt0 = (choiceptr)IntegerOfTerm(d0);
#else
  pt0 = (choiceptr)(LCL0-IntOfTerm(d0));
#endif
  {
    while (POP_CHOICE_POINT(pt0))
      {
	POP_EXECUTE();
      }
  }
#ifdef YAPOR
    CUT_prune_to(pt0);
#endif /* YAPOR */
  /* find where to cut to */
  if (pt0 > B) {
    /* Wow, we're gonna cut!!! */
#ifdef TABLING
    while (B->cp_b < pt0) {
      B = B->cp_b;
    }
    abolish_incomplete_subgoals(B);
#endif /* TABLING */
    B = pt0;
    HB = B->cp_h;
    Yap_TrimTrail();
  }
  ENDCHO(pt0);
  return(TRUE);

  BEGP(pt0);
  deref_body(d0, pt0, cutby_x_unk, cutby_x_nvar);
  /* never cut to a variable */
  /* Abort */
  return(FALSE);
  ENDP(pt0);
  ENDD(d0);
}

static Int
p_erroneous_call( USES_REGS1 )
{
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "bad call to internal built-in");
  return(FALSE);
}

 static Int
   p_save_cp( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Term td;
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif
  if (!IsVarTerm(t)) return(FALSE);
  td = cp_as_integer(B PASS_REGS);
  YapBind((CELL *)t,td);
  return(TRUE);
}

 /// @}

 /** 
  *
  * @addtogroup args
  *
  * @{
  *
  *  @namespace args
  *
  * @pred genarg( ?Index, +Term , -Arg )
  *
  * 
  * Similar to arg/3, but it can also backtrack through _T_'s arguments, that is:

  ~~~~~~~~~
  ?- arg:genarg(I, f(a,b), A).
  A = a,
  I = 1.
  ;
  A = b,
  I = 2.
  ~~~~~~~~~
  * 
  * Note: SWI-Prolog defines arg/3 as genarg/3.  
  */
 static Int
   genarg( USES_REGS1 )
 {				/* getarg(?Atom)		 */
   Term t0 = Deref(ARG1);
   Term t1 = Deref(ARG2);
   CELL *pt, *end;
   int res;
   UInt arity;

   if (!IsVarTerm(t0)) {
     res = p_arg( PASS_REGS1 );
     if (res) {
       cut_succeed();
     } else {
       cut_fail();
     }
   }
   if (IsVarTerm(t1)) {
     Yap_Error(INSTANTIATION_ERROR,t1,"genarg/3");
    return FALSE;
  }
  if (IsPrimitiveTerm(t1)) {
    Yap_Error(TYPE_ERROR_COMPOUND,t1,"genarg/3");
    return FALSE;
  }
  if (IsPairTerm(t1)) {
    pt = RepPair(t1);
    end = RepPair(t1)+1;
    arity = 2;
  } else {
    arity = ArityOfFunctor(FunctorOfTerm(t1));
    pt = RepAppl(t1);
    end = pt+arity;
    pt += 1;
  }
  res = Yap_unify(ARG1,MkIntTerm(1)) &&
    Yap_unify(ARG3,pt[0]);
  if (arity == 1) {
    if (res) {
      cut_succeed();
    } else {
      cut_fail();
    }
  }
  EXTRA_CBACK_ARG(3,1) = (Term)(pt+1);
  EXTRA_CBACK_ARG(3,2) = (Term)(end);
  EXTRA_CBACK_ARG(3,3) = MkIntegerTerm(arity);
  return res;
}

static Int
cont_genarg( USES_REGS1 )
{				/* genarg(?Atom)		 */
  CELL *pt, *end;
  int res;
  UInt arity;

  pt = (CELL *)EXTRA_CBACK_ARG(3,1);
  end = (CELL *)EXTRA_CBACK_ARG(3,2);
  arity = IntegerOfTerm(EXTRA_CBACK_ARG(3,3));
  if (pt == end) {
    res = Yap_unify(ARG1,MkIntegerTerm(arity)) &&
      Yap_unify(ARG3,pt[0]);
    if (res) {
      cut_succeed();
    } else {
      cut_fail();
    }
  }
  EXTRA_CBACK_ARG(3,1) = (Term)(pt+1);
  return Yap_unify(ARG1,MkIntegerTerm(arity-(end-pt))) &&
      Yap_unify(ARG3,pt[0]);
}

/// @}

/// @addtogroup inlines
/// @{
 void
   Yap_InitInlines(void)
 {
   CACHE_REGS
     Term cm = CurrentModule;
   Yap_InitAsmPred("$$cut_by", 1, _cut_by, p_cut_by, SafePredFlag);
   Yap_InitAsmPred("$$save_by", 1, _save_by, p_save_cp, SafePredFlag);
   Yap_InitAsmPred("atom", 1, _atom, p_atom, SafePredFlag);
   Yap_InitAsmPred("atomic", 1, _atomic, p_atomic, SafePredFlag);
   Yap_InitAsmPred("integer", 1, _integer, p_integer, SafePredFlag);
   Yap_InitAsmPred("nonvar", 1, _nonvar, p_nonvar, SafePredFlag);
   Yap_InitAsmPred("number", 1, _number, p_number, SafePredFlag);
   Yap_InitAsmPred("var", 1, _var, p_var, SafePredFlag);
   Yap_InitAsmPred("db_reference", 1, _db_ref, p_db_ref, SafePredFlag);
   Yap_InitAsmPred("primitive", 1, _primitive, p_primitive, SafePredFlag);
   Yap_InitAsmPred("compound", 1, _compound, p_compound, SafePredFlag);
   Yap_InitAsmPred("float", 1, _float, p_float, SafePredFlag);
   Yap_InitAsmPred("=", 2, _equal, p_equal, SafePredFlag);
#if INLINE_BIG_COMPARISONS
   Yap_InitAsmPred("\\=", 2, _dif, p_dif, SafePredFlag|TestPredFlag);
   Yap_InitAsmPred("==", 2, _eq, p_eq, SafePredFlag|TestPredFlag);
#else
   Yap_InitCPred("\\=", 2, p_dif, SafePredFlag);
   Yap_InitCPred("==", 2, p_eq, SafePredFlag);
#endif
   Yap_InitAsmPred("arg", 3, _arg, p_arg, SafePredFlag);
   Yap_InitAsmPred("functor", 3, _functor, p_functor, 0);
   Yap_InitAsmPred("$label_ctl", 2, _p_label_ctl, p_erroneous_call, SafePredFlag);
   CurrentModule = ARG_MODULE;
   Yap_InitCPredBack("genarg", 3, 3, genarg, cont_genarg,SafePredFlag);
   CurrentModule = cm;
     Yap_InitCPred("true", 0, p_true, SafePredFlag);
     Yap_InitCPred("otherwise", 0, p_true, SafePredFlag);
     Yap_InitCPred("false", 0, p_fail, SafePredFlag);
     Yap_InitCPred("fail", 0, p_fail, SafePredFlag);
}


/// @}
