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
* File:		cmppreds.c						 *
* Last rev:								 *
* mods:									 *
* comments:	comparing two prolog terms				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "eval.h"
#if HAVE_STRING_H
#include <string.h>
#endif

STATIC_PROTO(Int compare, (Term, Term));
STATIC_PROTO(Int p_compare, (void));
STATIC_PROTO(Int p_acomp, (void));
STATIC_PROTO(Int a_eq, (Term,Term));
STATIC_PROTO(Int p_eq, (void));
STATIC_PROTO(Int a_dif, (Term,Term));
STATIC_PROTO(Int p_dif, (void));
STATIC_PROTO(Int a_gt, (Term, Term));
STATIC_PROTO(Int p_gt, (void));
STATIC_PROTO(Int a_ge, (Term,Term));
STATIC_PROTO(Int p_ge, (void));
STATIC_PROTO(Int a_lt, (Term,Term));
STATIC_PROTO(Int p_lt, (void));
STATIC_PROTO(Int a_le, (Term,Term));
STATIC_PROTO(Int p_le, (void));
STATIC_PROTO(Int p_noteq, (void));
STATIC_PROTO(Int p_gen_lt, (void));
STATIC_PROTO(Int p_gen_le, (void));
STATIC_PROTO(Int p_gen_gt, (void));
STATIC_PROTO(Int p_gen_ge, (void));

#define rfloat(X)	( X > 0.0 ? 1 : ( X == 0.0 ? 0 : -1))

static int compare_complex(register CELL *pt0, register CELL *pt0_end, register
		   CELL *pt1)
{

  register CELL **to_visit = (CELL **)H;
  register int out = 0;

 loop:
  while (pt0 < pt0_end) {
    register CELL d0, d1;
    ++ pt0;
    ++ pt1;
    d0 = Derefa(pt0);
    d1 = Derefa(pt1);
    if (IsVarTerm(d0)) {
      if (IsVarTerm(d1)) {
	out = Signed(d0) - Signed(d1);
	if (out) goto done;
      }
      else {
	out = 1;
	goto done;
      }
    } else if (IsVarTerm(d1)) {
      out = -1;
      goto done;
    } else {
      if (d0 == d1) continue;
      else if (IsAtomTerm(d0)) {
	if (IsAtomTerm(d1))
	    out = strcmp(
			 RepAtom(AtomOfTerm(d0))->StrOfAE,
			 RepAtom(AtomOfTerm(d1))->StrOfAE
			 );
	else if (IsPrimitiveTerm(d1))
	  out = 1;
	else out = -1;
	/* I know out must be != 0 */
	goto done;
      }
      else if (IsIntTerm(d0)) {
	if (IsIntTerm(d1))
	  out = IntOfTerm(d0) - IntOfTerm(d1);
	else if (IsFloatTerm(d1)) {
	  out = rfloat(IntOfTerm(d0) - FloatOfTerm(d1));
	  if (out == 0)
	     out = 1;
	} else if (IsLongIntTerm(d1)) {
	  out = IntOfTerm(d0) - LongIntOfTerm(d1);
#ifdef USE_GMP
	} else if (IsBigIntTerm(d1)) {
	  out = -mpz_cmp_si(BigIntOfTerm(d1), IntOfTerm(d0));
#endif
	} else if (IsRefTerm(d1))
		out = 1 ;
	else out = -1;
	if (out != 0)
	  goto done;
      }
      else if (IsFloatTerm(d0)) {
	if (IsFloatTerm(d1))
	  out = rfloat(FloatOfTerm(d0) - FloatOfTerm(d1));
	else if (IsIntTerm(d1)) {
	  out = rfloat(FloatOfTerm(d0) - IntOfTerm(d1));
	  if (out == 0)
	    out = -1;
	} else if (IsLongIntTerm(d1)) {
	  out = rfloat(FloatOfTerm(d0) - LongIntOfTerm(d1));
	  if (out == 0)
	     out = -1;
#ifdef USE_GMP
	} else if (IsBigIntTerm(d1)) {
	  Float outf = FloatOfTerm(d0) - mpz_get_d(BigIntOfTerm(d1));
	  if (outf <= 0.0)
	     out = -1;
	  else
	    out = 1;
#endif
	} else if (IsRefTerm(d1))
	  out = 1;
	else out = -1;
	if (out != 0)
	  goto done;
      }
      else if (IsLongIntTerm(d0)) {
	if (IsIntTerm(d1))
	  out = LongIntOfTerm(d0) - IntOfTerm(d1);
	else if (IsFloatTerm(d1)) {
	  out = rfloat(LongIntOfTerm(d0) - FloatOfTerm(d1));
	  if (out == 0)
	     out = 1;
	} else if (IsLongIntTerm(d1))
	  out = LongIntOfTerm(d0) - LongIntOfTerm(d1);
#ifdef USE_GMP
	else if (IsBigIntTerm(d1))
	  out = -mpz_cmp_si(BigIntOfTerm(d1), LongIntOfTerm(d0));
#endif
	else if (IsRefTerm(d1))
	  out = 1 ;
	else out = -1;
	if (out != 0)
	  goto done;
      }
#ifdef USE_GMP
      else if (IsBigIntTerm(d0)) {
	if (IsIntTerm(d1))
	  out = mpz_cmp_si(BigIntOfTerm(d0), IntOfTerm(d1));
	else if (IsFloatTerm(d1)) {
	  Float fout = mpz_get_d(BigIntOfTerm(d0)) - FloatOfTerm(d1);
	  if (fout >= 0.0)
	    out = 1;
	  else
	    out = -1;
	} else if (IsLongIntTerm(d1))
	  out = mpz_cmp_si(BigIntOfTerm(d0), LongIntOfTerm(d1));
	else if (IsBigIntTerm(d1))
	  out = mpz_cmp(BigIntOfTerm(d0), BigIntOfTerm(d1));
	else if (IsRefTerm(d1))
	  out = 1 ;
	else out = -1;
	if (out != 0)
	  goto done;
      }
#endif
      else if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  if (IsApplTerm(d1)) {
	    Functor f = FunctorOfTerm(d1);
	    if (IsExtensionFunctor(f))
	      out = 1;
	    else if (!(out = 2-ArityOfFunctor(f)))
	       out = strcmp(".",RepAtom(NameOfFunctor(f))->StrOfAE);
	  } else out = 1;
	  goto done;
	}
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	to_visit += 4;
	*pt0 = d1;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit += 3;
	}
#endif
	pt0 = RepPair(d0) - 1;
	pt0_end = RepPair(d0) + 1;
	pt1 = RepPair(d1) - 1;
	continue;
      }
      else if (IsRefTerm(d0)) {
	if (IsRefTerm(d1))
	  out = Unsigned(RefOfTerm(d1)) -
	    Unsigned(RefOfTerm(d0));
	else out = -1;
	goto done;
      } else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2, *ap3;
	if (!IsApplTerm(d1)) {
	  out = 1 ;
	  goto done;
	} else {
	  /* store the terms to visit */
	  Functor f2;
	  ap2 = RepAppl(d0);
	  ap3 = RepAppl(d1);
	  f = (Functor)(*ap2);
	  if (IsExtensionFunctor(f)) {
	    out = 1;
	    goto done;
	  }
	  f2 = (Functor)(*ap3);	
	  if (IsExtensionFunctor(f2)) {
	    out = -1;
	    goto done;
	  }
	  /* compare functors */
	  if (f != (Functor)*ap3) {
	    if (!(out = ArityOfFunctor(f)-ArityOfFunctor(f2)))
	       out = strcmp(RepAtom(NameOfFunctor(f))->StrOfAE,
			    RepAtom(NameOfFunctor(f2))->StrOfAE);
	    goto done;
	  }
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	to_visit += 4;
	*pt0 = d1;
#else
	  /* store the terms to visit */
	  if (pt0 < pt0_end) {
	    to_visit[0] = pt0;
	    to_visit[1] = pt0_end;
	    to_visit[2] = pt1;
	    to_visit += 3;
	  }
#endif
	  d0 = ArityOfFunctor(f);
	  pt0 = ap2;
	  pt0_end = ap2 + d0;
	  pt1 = ap3;
	  continue;
	}
      }

    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **)H) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
#endif
    goto loop;
  }

 done:
  /* failure */
#ifdef RATIONAL_TREES
  while (to_visit > (CELL **)H) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
  }
#endif
  return(out);
}

inline static Int 
compare(register Term t1,register Term t2) /* compare terms t1 and t2	 */
{
	Int             r;
	t1 = Deref(t1);
	t2 = Deref(t2);
	if (t1 == t2)
		return (0);
	if (IsVarTerm(t1)) {
		if (IsVarTerm(t2))
			return (Signed(t1) - Signed(t2));
		return (-1);
	} else if (IsVarTerm(t2))
		return (1);
	if (IsAtomTerm(t1)) {
	  if (IsAtomTerm(t2))
	    return (strcmp(
			   RepAtom(AtomOfTerm(t1))->StrOfAE,
			   RepAtom(AtomOfTerm(t2))->StrOfAE
			   ));
	  if (IsPrimitiveTerm(t2))
	    return (1);
	  return (-1);
	}
	if (IsIntTerm(t1)) {
	  if (IsIntTerm(t2))
	    return (IntOfTerm(t1) - IntOfTerm(t2));
	  if (IsFloatTerm(t2)) {
	    int out = rfloat(IntOfTerm(t1) - FloatOfTerm(t2));
	    if (out == 0)
	      return(1);
	    else
	      return(out);
	  }
	  if (IsLongIntTerm(t2))
	    return(IntOfTerm(t1) - LongIntOfTerm(t2));
#ifdef USE_GMP
	  if (IsBigIntTerm(t2))
	    return(-mpz_cmp_si(BigIntOfTerm(t2),IntOfTerm(t1)));
#endif
	  if (IsRefTerm(t2))
	    return (1);
	  return (-1);
	}
	if (IsFloatTerm(t1)) {
	  if (IsFloatTerm(t2))
	    return(rfloat(FloatOfTerm(t1) - FloatOfTerm(t2)));
	  else if (IsIntTerm(t2)) {
	    int out = rfloat(FloatOfTerm(t1) - IntOfTerm(t2));
	    if (out == 0)
	      return(-1);
	    else
	      return(out);
	  } else if (IsLongIntTerm(t2)) {
	    int out = rfloat(FloatOfTerm(t1) - LongIntOfTerm(t2));
	    if (out == 0)
	      return(-1);
	    else
	      return(out);
#ifdef USE_GMP
	  } else if (IsBigIntTerm(t2)) {
	    Float out = FloatOfTerm(t2) - mpz_get_d(BigIntOfTerm(t1));
	    if (out <= 0.0)
	      return (-1);
	    return(1);
#endif
	  } else if (IsRefTerm(t2))
	    return (1);
	  return (-1);
	}
	if (IsLongIntTerm(t1)) {
	  if (IsIntTerm(t2))
	    return (LongIntOfTerm(t1) - IntOfTerm(t2));
	  if (IsFloatTerm(t2)) {
	    int out = rfloat(LongIntOfTerm(t1) - FloatOfTerm(t2));
	    if (out == 0)
	      return(1);
	    else
	      return(out);
	  }
	  if (IsLongIntTerm(t2))
	    return (LongIntOfTerm(t1) - LongIntOfTerm(t2));
#ifdef USE_GMP
	  if (IsBigIntTerm(t2))
	    return(-mpz_cmp_si(BigIntOfTerm(t2), LongIntOfTerm(t1)));
#endif
	  if (IsRefTerm(t2))
	    return (1);
	  return (-1);
	}
#ifdef USE_GMP
        if (IsBigIntTerm(t1)) {
	  if (IsIntTerm(t2))
	    return(mpz_cmp_si(BigIntOfTerm(t1), IntOfTerm(t2)));
	  if (IsFloatTerm(t2)) {
	    Float out = mpz_get_d(BigIntOfTerm(t1)) - FloatOfTerm(t2);
	    if (out >= 0.0)
	      return(1);
	    else
	      return(-1);
	  }
	  if (IsLongIntTerm(t2))
	    return(mpz_cmp_si(BigIntOfTerm(t1), LongIntOfTerm(t2)));
	  if (IsBigIntTerm(t2))
	    return(mpz_cmp(BigIntOfTerm(t1), BigIntOfTerm(t2)));
	  if (IsRefTerm(t2))
	    return(1);
	  return(-1);
	}
#endif
	if (IsPairTerm(t1)) {
	  if (IsApplTerm(t2)) {
	    Functor f = FunctorOfTerm(t2);
	    if (IsExtensionFunctor(f))
	      return(1);
	    else {
	      int out;
	      if (!(out = 2-ArityOfFunctor(f)))
	        out = strcmp(".",RepAtom(NameOfFunctor(f))->StrOfAE);
	      return(out);
	    }
	  }
	  if (IsPairTerm(t2)) {
	    return(compare_complex(RepPair(t1)-1,
				   RepPair(t1)+1,
				   RepPair(t2)-1));
	  }
	  else return (1);
	}
	if (IsRefTerm(t1)) {
	  if (IsRefTerm(t2))
	    return (Unsigned(RefOfTerm(t2)) -
		    Unsigned(RefOfTerm(t1)));
	  return (-1);
	}
	if (!IsApplTerm(t2))
		return (1);
	/* compound term */
	{ Functor fun1 = FunctorOfTerm(t1);
	  Functor fun2;
	  fun2 = FunctorOfTerm(t2);
	  if (IsExtensionFunctor(fun2))
	    return(1);
	  r = ArityOfFunctor(fun1) - ArityOfFunctor(fun2);
	  if (r)
	    return (r);
	  r = strcmp(RepAtom(NameOfFunctor(fun1))->StrOfAE,
		     RepAtom(NameOfFunctor(fun2))->StrOfAE);
	  if (r)
	    return (r);
	  else
	    return(compare_complex(RepAppl(t1),
				   RepAppl(t1)+ArityOfFunctor(fun1),
				   RepAppl(t2)));
	}
}

int iequ(register CELL d0, register CELL d1)
{
  return (compare(d0,d1) == 0);
}

int compare_terms(register CELL d0, register CELL d1)
{
  return (compare(d0,d1));
}

static Int 
p_compare(void)
{				/* compare(?Op,?T1,?T2)	 */
	Int             r = compare(ARG2, ARG3);
	Atom            p;
	if (r < 0)
		p = AtomLT;
	else if (r > 0)
		p = AtomGT;
	else
		p = AtomEQ;
	return (unify_constant(ARG1, MkAtomTerm(p)));
}

inline static int
int_cmp(Int dif)
{
    if (dif < 0)
      return(unify_constant(ARG1,MkAtomTerm(AtomLT)));
    else if (dif > 0)
      return(unify_constant(ARG1,MkAtomTerm(AtomGT)));
    else
      return(unify_constant(ARG1,MkAtomTerm(AtomEQ)));
}

inline static int
flt_cmp(Float dif)
{
    if (dif < 0.0)
      return(unify_constant(ARG1,MkAtomTerm(AtomLT)));
    else if (dif > 0.0)
      return(unify_constant(ARG1,MkAtomTerm(AtomGT)));
    else
      return(unify_constant(ARG1,MkAtomTerm(AtomEQ)));
}


static Int 
p_acomp(void)
{				/* $a_compare(?R,+X,+Y) */
  register blob_type  bt1;
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  union arith_ret v1;

  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR, t1, "=:=/2");
    return(FALSE);
  }
  if (IsVarTerm(t2)) {
    Error(INSTANTIATION_ERROR, t2, "=:=/2");
    return(FALSE);
  }
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2)) {
    return(int_cmp(IntegerOfTerm(t1)-IntegerOfTerm(t2)));
  } if (IsFloatTerm(t1) && IsFloatTerm(t2)) {
    return(flt_cmp(FloatOfTerm(t1)-FloatOfTerm(t2)));
  }
  bt1 = Eval(t1, &v1);
  switch (bt1) {
  case  long_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(int_cmp(v1.Int-v2.Int));
      case double_e:
	return(flt_cmp(v1.Int-v2.dbl));
#ifdef USE_GMP
      case big_int_e:
	return(int_cmp(-mpz_cmp_si(v2.big,v1.Int)));
#endif
      default:
	return(FALSE);
      }
    }
  case double_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(flt_cmp(v1.dbl-v2.Int));
      case double_e:
	return(flt_cmp(v1.dbl-v2.dbl));
#ifdef USE_GMP
      case big_int_e:
	return(flt_cmp(v1.dbl-mpz_get_d(v2.big)));
#endif
      default:
	return(FALSE);
      }
    }
#ifdef USE_GMP
  case big_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(int_cmp(mpz_cmp_si(v1.big,v2.Int)));
      case double_e:
	return(flt_cmp(mpz_get_d(v1.big)-v2.dbl));
      case big_int_e:
	return(int_cmp(mpz_cmp(v1.big,v2.big)));
      default:
	return(FALSE);
      }
    }
#endif
  default:
    return(FALSE);
  }
}

static Int 
a_eq(Term t1, Term t2)
{				/* A =:= B		 */
  blob_type bt1;
  union arith_ret v1;

  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR, t1, "=:=/2");
    return(FALSE);
  }
  if (IsVarTerm(t2)) {
    Error(INSTANTIATION_ERROR, t2, "=:=/2");
    return(FALSE);
  }
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2))
    return (IntegerOfTerm(t1) == IntegerOfTerm(t2));
  if (IsFloatTerm(t1) && IsFloatTerm(t2))
    return (FloatOfTerm(t1) == FloatOfTerm(t2));
  bt1 = Eval(t1, &v1);
  switch (bt1) {
  case  long_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.Int == v2.Int);
      case double_e:
	return(v1.Int == v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(-mpz_cmp_si(v2.big,v1.Int) == 0);
#endif
      default:
	return(FALSE);
      }
    }
  case double_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.dbl == v2.Int);
      case double_e:
	return(v1.dbl == v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(v1.dbl == mpz_get_d(v2.big));
#endif
      default:
	return(FALSE);
      }
    }
#ifdef USE_GMP
  case big_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(mpz_cmp_si(v1.big,v2.Int) == 0);
      case double_e:
	return(mpz_get_d(v1.big) == v2.dbl);
      case big_int_e:
	return(mpz_cmp(v1.big,v2.big) == 0);
      default:
	return(FALSE);
      }
    }
#endif
  default:
    return(FALSE);
  }
}

static Int 
p_eq(void)
{				/* A =:= B		 */
  return(a_eq(Deref(ARG1),Deref(ARG2)));
}

static Int 
a_dif(Term t1, Term t2)
{				/* A =\\= B		 */
  blob_type bt1;
  union arith_ret v1;

  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR, t1, "=\\=/2");
    return(FALSE);
  }
  if (IsVarTerm(t2)) {
    Error(INSTANTIATION_ERROR, t2, "=\\=/2");
    return(FALSE);
  }
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2))
    return (IntegerOfTerm(t1) != IntegerOfTerm(t2));
  if (IsFloatTerm(t1) && IsFloatTerm(t2))
    return (FloatOfTerm(t1) != FloatOfTerm(t2));
  bt1 = Eval(t1, &v1);
  switch (bt1) {
  case  long_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.Int != v2.Int);
      case double_e:
	return(v1.Int != v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(-mpz_cmp_si(v2.big,v1.Int) != 0);
#endif
      default:
	return(FALSE);
      }
    }
  case double_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.dbl != v2.Int);
      case double_e:
	return(v1.dbl != v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(v1.dbl != mpz_get_d(v2.big));
#endif
      default:
	return(FALSE);
      }
    }
#ifdef USE_GMP
  case big_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(mpz_cmp_si(v1.big,v2.Int) != 0);
      case double_e:
	return(mpz_get_d(v1.big) != v2.dbl);
      case big_int_e:
	return(mpz_cmp(v1.big,v2.big) != 0);
      default:
	return(FALSE);
      }
    }
#endif
  default:
    return(FALSE);
  }
}

static Int 
p_dif(void)
{				/* A =\\= B		 */
  return(a_dif(Deref(ARG1),Deref(ARG2)));
}

static Int 
a_gt(Term t1, Term t2)
{				/* A > B		 */
  blob_type bt1;
  union arith_ret v1;

  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR, t1, ">/2");
    return(FALSE);
  }
  if (IsVarTerm(t2)) {
    Error(INSTANTIATION_ERROR, t2, ">/2");
    return(FALSE);
  }
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2))
    return (IntegerOfTerm(t1) > IntegerOfTerm(t2));
  if (IsFloatTerm(t1) && IsFloatTerm(t2))
    return (FloatOfTerm(t1) > FloatOfTerm(t2));
  bt1 = Eval(t1, &v1);
  switch (bt1) {
  case  long_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.Int > v2.Int);
      case double_e:
	return(v1.Int > v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(-mpz_cmp_si(v2.big,v1.Int) > 0);
#endif
      default:
	return(FALSE);
      }
    }
  case double_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.dbl > v2.Int);
      case double_e:
	return(v1.dbl > v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(v1.dbl > mpz_get_d(v2.big));
#endif
      default:
	return(FALSE);
      }
    }
#ifdef USE_GMP
  case big_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(mpz_cmp_si(v1.big,v2.Int) > 0);
      case double_e:
	return(mpz_get_d(v1.big) > v2.dbl);
      case big_int_e:
	return(mpz_cmp(v1.big,v2.big) > 0);
      default:
	return(FALSE);
      }
    }
#endif
  default:
    return(FALSE);
  }
}

static Int 
p_gt(void)
{				/* A > B		 */
  return(a_gt(Deref(ARG1),Deref(ARG2)));
}

static Int 
a_ge(Term t1, Term t2)
{				/* A >= B		 */
  blob_type bt1;
  union arith_ret v1;

  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR, t1, ">=/2");
    return(FALSE);
  }
  if (IsVarTerm(t2)) {
    Error(INSTANTIATION_ERROR, t1, ">=/2");
    return(FALSE);
  }
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2))
    return (IntegerOfTerm(t1) >= IntegerOfTerm(t2));
  if (IsFloatTerm(t1) && IsFloatTerm(t2))
    return (FloatOfTerm(t1) >= FloatOfTerm(t2));
  bt1 = Eval(t1, &v1);
  switch (bt1) {
  case  long_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.Int >= v2.Int);
      case double_e:
	return(v1.Int >= v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(-mpz_cmp_si(v2.big,v1.Int) >= 0);
#endif
      default:
	return(FALSE);
      }
    }
  case double_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.dbl >= v2.Int);
      case double_e:
	return(v1.dbl >= v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(v1.dbl >= mpz_get_d(v2.big));
#endif
      default:
	return(FALSE);
      }
    }
#ifdef USE_GMP
  case big_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(mpz_cmp_si(v1.big,v2.Int) >= 0);
      case double_e:
	return(mpz_get_d(v1.big) >= v2.dbl);
      case big_int_e:
	return(mpz_cmp(v1.big,v2.big) >= 0);
      default:
	return(FALSE);
      }
    }
#endif
  default:
    return(FALSE);
  }
}

static Int 
p_ge(void)
{				/* A >= B		 */
  return(a_ge(Deref(ARG1),Deref(ARG2)));
}

static Int 
a_lt(Term t1, Term t2)
{				/* A < B       */
  blob_type bt1;
  union arith_ret v1;

  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR, t1, "</2");
    return(FALSE);
  }
  if (IsVarTerm(t2)) {
    Error(INSTANTIATION_ERROR, t2, "</2");
    return(FALSE);
  }
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2))
    return (IntegerOfTerm(t1) < IntegerOfTerm(t2));
  if (IsFloatTerm(t1) && IsFloatTerm(t2))
    return (FloatOfTerm(t1) < FloatOfTerm(t2));
  bt1 = Eval(t1, &v1);
  switch (bt1) {
  case  long_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.Int < v2.Int);
      case double_e:
	return(v1.Int < v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(-mpz_cmp_si(v2.big,v1.Int) < 0);
#endif
      default:
	return(FALSE);
      }
    }
  case double_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.dbl < v2.Int);
      case double_e:
	return(v1.dbl < v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(v1.dbl < mpz_get_d(v2.big));
#endif
      default:
	return(FALSE);
      }
    }
#ifdef USE_GMP
  case big_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(mpz_cmp_si(v1.big,v2.Int) < 0);
      case double_e:
	return(mpz_get_d(v1.big) < v2.dbl);
      case big_int_e:
	return(mpz_cmp(v1.big,v2.big) < 0);
      default:
	return(FALSE);
      }
    }
#endif
  default:
    return(FALSE);
  }
}

static Int 
p_lt(void)
{				/* A < B		 */
  return(a_lt(Deref(ARG1),Deref(ARG2)));
}

static Int 
a_le(Term t1, Term t2)
{				/* A <= B */
  blob_type bt1;
  union arith_ret v1;

  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR, t1, "=</2");
    return(FALSE);
  }
  if (IsVarTerm(t2)) {
    Error(INSTANTIATION_ERROR, t2, "=</2");
    return(FALSE);
  }
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2))
    return (IntegerOfTerm(t1) <= IntegerOfTerm(t2));
  if (IsFloatTerm(t1) && IsFloatTerm(t2))
    return (FloatOfTerm(t1) <= FloatOfTerm(t2));
  bt1 = Eval(t1, &v1);
  switch (bt1) {
  case  long_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.Int <= v2.Int);
      case double_e:
	return(v1.Int <= v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(-mpz_cmp_si(v2.big,v1.Int) <= 0);
#endif
      default:
	return(FALSE);
      }
    }
  case double_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(v1.dbl <= v2.Int);
      case double_e:
	return(v1.dbl <= v2.dbl);
#ifdef USE_GMP
      case big_int_e:
	return(v1.dbl <= mpz_get_d(v2.big));
#endif
      default:
	return(FALSE);
      }
    }
#ifdef USE_GMP
  case big_int_e:
    {
      union arith_ret v2;
      blob_type bt2 = Eval(t2, &v2);

      switch (bt2) {
      case  long_int_e:
	return(mpz_cmp_si(v1.big,v2.Int) <= 0);
      case double_e:
	return(mpz_get_d(v1.big) <= v2.dbl);
      case big_int_e:
	return(mpz_cmp(v1.big,v2.big) <= 0);
      default:
	return(FALSE);
      }
    }
#endif
  default:
    return(FALSE);
  }
}

static Int 
p_le(void)
{				/* A =< B		 */
  return(a_le(Deref(ARG1),Deref(ARG2)));
}

static Int 
p_noteq(void)
{
  return (compare(ARG1, ARG2) != 0);
}

static Int 
p_gen_lt(void)
{
  return (compare(ARG1, ARG2) < 0);
}

static Int 
p_gen_le(void)
{
  return (compare(ARG1, ARG2) <= 0);
}

static Int 
p_gen_gt(void)
{
  return (compare(ARG1, ARG2) > 0);
}

static Int 
p_gen_ge(void)
{
  return (compare(ARG1, ARG2) >= 0);
}


void 
InitCmpPreds(void)
{
  InitCmpPred("=:=", 2, a_eq, p_eq, SafePredFlag | BinaryTestPredFlag);
  InitCmpPred("=\\=", 2, a_dif, p_dif, SafePredFlag | BinaryTestPredFlag);
  InitCmpPred(">", 2, a_gt, p_gt,  SafePredFlag | BinaryTestPredFlag);
  InitCmpPred("=<", 2, a_le, p_le, SafePredFlag | BinaryTestPredFlag);
  InitCmpPred("<", 2, a_lt, p_lt, SafePredFlag | BinaryTestPredFlag);
  InitCmpPred(">=", 2, a_ge, p_ge, SafePredFlag | BinaryTestPredFlag);
  InitCPred("$a_compare", 3, p_acomp, TestPredFlag | SafePredFlag);
  InitCPred("\\==", 2, p_noteq, TestPredFlag | SafePredFlag);
  InitCPred("@<", 2, p_gen_lt, TestPredFlag | SafePredFlag);
  InitCPred("@=<", 2, p_gen_le, TestPredFlag | SafePredFlag);
  InitCPred("@>", 2, p_gen_gt, TestPredFlag | SafePredFlag);
  InitCPred("@>=", 2, p_gen_ge, TestPredFlag | SafePredFlag);
  InitCPred("compare", 3, p_compare, TestPredFlag | SafePredFlag);
}
