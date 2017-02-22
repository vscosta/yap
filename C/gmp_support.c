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
* File:		gmp_support.c						 *
* Last rev:								 *
* mods:									 *
* comments:	bignum code						 *
*									 *
*************************************************************************/

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "YapEval.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#include <wchar.h>

#if USE_GMP

static inline Term
MkBigAndClose(MP_INT *new)
{
  Term t = Yap_MkBigIntTerm(new);
  mpz_clear(new);
  if (t == TermNil) {
    Yap_ArithError(RESOURCE_ERROR_STACK, t, ">>/2");
  }
  return t;
}

static inline Term
MkRatAndClose(MP_RAT *new)
{
  Term t = Yap_MkBigRatTerm(new);
  mpq_clear(new);
  if (t == TermNil) {
    Yap_ArithError(RESOURCE_ERROR_STACK, t, ">>/2");
  }
  return t;
}

/* add i + j using temporary bigint new */
Term
Yap_gmp_add_ints(Int i, Int j)
{
  MP_INT new;

  mpz_init_set_si(&new,i);
  if (j > 0) {
    mpz_add_ui(&new, &new, j);
  } else {
    if (j-1 > 0) { /* negative overflow */
      mpz_sub_ui(&new, &new, -(j+1));
      mpz_sub_ui(&new, &new, 1);
    } else {
      mpz_sub_ui(&new, &new, -j);
    }
  }
  return MkBigAndClose(&new);
}

Term
Yap_gmp_sub_ints(Int i, Int j)
{
  MP_INT new;
  Term t;

  mpz_init_set_si(&new,i);
  if (j > 0) {
    mpz_sub_ui(&new, &new, j);
  } else {
    if (j-1 > 0) { /* negative overflow */
      mpz_add_ui(&new, &new, -(j+1));
      mpz_add_ui(&new, &new, 1);
    } else {
      mpz_add_ui(&new, &new, -j);
    }
  }
  return MkBigAndClose(&new);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term
Yap_gmp_mul_ints(Int i, Int j)
{
  MP_INT new;

  mpz_init_set_si(&new,i);
  mpz_mul_si(&new, &new, j);
  return MkBigAndClose(&new);
}

Term 
Yap_gmp_sll_ints(Int i, Int j)
{
  MP_INT new;

  mpz_init_set_si(&new,i);
  mpz_mul_2exp(&new, &new, j);
  return MkBigAndClose(&new);
}

/* add i + b using temporary bigint new */
Term 
Yap_gmp_add_int_big(Int i, Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b = Yap_BigIntOfTerm(t);
    mpz_init_set_si(&new, i);
    mpz_add(&new, &new, b);
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b = Yap_BigRatOfTerm(t);
    mpq_init(&new);
    mpq_set_si(&new, i, 1L);
    mpq_add(&new, &new, b);
    return MkRatAndClose(&new);    
  }
}

/* add i + b using temporary bigint new */
void
Yap_gmp_set_bit(Int i, Term t)
{
  MP_INT *b = Yap_BigIntOfTerm(t);
  mpz_setbit(b, i);
}

/* sub i - b using temporary bigint new */
Term 
Yap_gmp_sub_int_big(Int i, Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b = Yap_BigIntOfTerm(t);

    mpz_init_set_si(&new, i);
    mpz_sub(&new, &new, b);
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b = Yap_BigRatOfTerm(t);

    mpq_init(&new);
    mpq_set_si(&new, i, 1L);
    mpq_sub(&new, &new, b);
    return MkRatAndClose(&new);    
  }
}

/* add i + b using temporary bigint new */
Term 
Yap_gmp_mul_int_big(Int i, Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b = Yap_BigIntOfTerm(t);

    mpz_init_set_si(&new, i);
    mpz_mul(&new, &new, b);
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b = Yap_BigRatOfTerm(t);

    mpq_init(&new);
    mpq_set_si(&new, i, 1L);
    mpq_mul(&new, &new, b);
    return MkRatAndClose(&new);    
  }
}

/* sub i - b using temporary bigint new */
Term 
Yap_gmp_sub_big_int(Term t, Int i)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b = Yap_BigIntOfTerm(t);

    mpz_init_set_si(&new, i);
    mpz_neg(&new, &new);
    mpz_add(&new, &new, b);
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b = Yap_BigRatOfTerm(t);

    mpq_init(&new);
    mpq_set_si(&new, i, 1L);
    mpq_sub(&new, b, &new);
    return MkRatAndClose(&new);    
  }
}

/* div i / b using temporary bigint new */
Term 
Yap_gmp_div_int_big(Int i, Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    /* cool */
    return MkIntTerm(0);
  } else {
    MP_RAT new;
    MP_RAT *b = Yap_BigRatOfTerm(t);

    mpq_init(&new);
    mpq_set_si(&new, i, 1L);
    mpq_div(&new, &new, b);
    return MkRatAndClose(&new);    
  }
}

/* div b / i using temporary bigint new */
Term 
Yap_gmp_div_big_int(Term t, Int i)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b = Yap_BigIntOfTerm(t);

    mpz_init_set(&new, b);
    if ( (-3 / 2) == -2 ) {
      if (i > 0) {
	mpz_tdiv_q_ui(&new, &new, i);
      } else if (i == 0) {
	Yap_ArithError(EVALUATION_ERROR_ZERO_DIVISOR, MkIntTerm(0), "// /2");
      } else {
	/* we do not handle MIN_INT */
	mpz_tdiv_q_ui(&new, &new, -i);
	mpz_neg(&new, &new);
      }
    } else {
      if (i > 0) {
	mpz_fdiv_q_ui(&new, &new, i);
      } else if (i == 0) {
	Yap_ArithError(EVALUATION_ERROR_ZERO_DIVISOR, MkIntTerm(0), "// /2");
      } else {
	/* we do not handle MIN_INT */
	mpz_fdiv_q_ui(&new, &new, -i);
	mpz_neg(&new, &new);
      }
    }
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b = Yap_BigRatOfTerm(t);

    mpq_init(&new);
    mpq_set_si(&new, i, 1L);
    mpq_div(&new, b, &new);
    return MkRatAndClose(&new);    
  }
}

/* div b / i using temporary bigint new */
Term 
Yap_gmp_div2_big_int(Term t, Int i)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b = Yap_BigIntOfTerm(t);

    mpz_init_set(&new, b);
    if (i > 0) {
      mpz_fdiv_q_ui(&new, &new, i);
    } else if (i == 0) {
      Yap_ArithError(EVALUATION_ERROR_ZERO_DIVISOR, MkIntTerm(0), "// /2");
    } else {
      /* we do not handle MIN_INT */
      mpz_fdiv_q_ui(&new, &new, -i);
      mpz_neg(&new, &new);
    }
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b = Yap_BigRatOfTerm(t);

    mpq_init(&new);
    mpq_set_si(&new, i, 1L);
    mpq_div(&new, b, &new);
    return MkRatAndClose(&new);    
  }
}

/* and i - b using temporary bigint new */
Term 
Yap_gmp_and_int_big(Int i, Term t)
{
  MP_INT new;
  CELL *pt = RepAppl(t);
  MP_INT *b;
  if (pt[1] != BIG_INT) {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "/\\/2");    
  }
  b = Yap_BigIntOfTerm(t);

  mpz_init_set_si(&new, i);
  mpz_and(&new, &new, b);
  return MkBigAndClose(&new);
}

/* or i - b using temporary bigint new */
Term 
Yap_gmp_ior_int_big(Int i, Term t)
{
  MP_INT new;
  CELL *pt = RepAppl(t);
  MP_INT *b;
  if (pt[1] != BIG_INT) {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "\\/ /2");
  }
  b = Yap_BigIntOfTerm(t);

  mpz_init_set_si(&new, i);
  mpz_ior(&new, &new, b);
  return MkBigAndClose(&new);
}

#if USE_GMP
// cross-compilers...
#if !defined(HAVE_MPZ_XOR) && !defined(mpz_xor)
static void
mpz_xor(MP_INT *new, MP_INT *r1, MP_INT *r2)
{
  MP_INT *n2, *n3;
  
  mpz_new(n2);
  mpz_new(n3);
  mpz_ior(new, r1, r2);
  mpz_com(n2,  r1);
  mpz_and(n2, n2, new);
  mpz_com(n3,  r2);
  mpz_and(n3, n3, new);
  mpz_ior(new, n2, n3);
  mpz_clear(n2);
  mpz_clear(n3);
}
#endif
#endif

/* or i - b using temporary bigint new */
Term 
Yap_gmp_xor_int_big(Int i, Term t)
{
  MP_INT new;
  CELL *pt = RepAppl(t);
  MP_INT *b;
  if (pt[1] != BIG_INT) {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "#/2");
  }
  b = Yap_BigIntOfTerm(t);

  mpz_init_set_si(&new,i);
  mpz_xor(&new, &new, b);
  return MkBigAndClose(&new);
}

/* <<< i + b using temporary bigint new */
Term 
Yap_gmp_sll_big_int(Term t, Int i)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b = Yap_BigIntOfTerm(t);

    if (i > 0) {
      mpz_init(&new);
      mpz_mul_2exp(&new, b, i);
    } else if (i == 0) {
      return t;
    } else {
      mpz_init(&new);
      if (i == Int_MIN) {
	CACHE_REGS
	Yap_ArithError(RESOURCE_ERROR_HUGE_INT, MkIntegerTerm(i), "<</2");
      }
      mpz_fdiv_q_2exp(&new, b, -i);
    }
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b = Yap_BigRatOfTerm(t);

    if (i > 0) {
      mpq_init(&new);
      mpq_mul_2exp (&new, b, i);
    } else if (i == 0) {
      return t;
    } else {
      mpq_init(&new);
      mpq_div_2exp (&new, b, i);
    }
    return MkRatAndClose(&new);
  }
}

Term 
Yap_gmp_add_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init_set(&new, b1);
    mpz_add(&new, &new, b2);
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b1, bb1;
    MP_RAT *b2, bb2;
    if (pt1[1] == BIG_INT) {
      b1 = &bb1;
      mpq_init(b1);
      mpq_set_z(b1, Yap_BigIntOfTerm(t1));
    } else {
      b1 = Yap_BigRatOfTerm(t1);
    }
    if (pt2[1] == BIG_INT) {
      b2 = &bb2;
      mpq_init(b2);
      mpq_set_z(b2, Yap_BigIntOfTerm(t2));
    } else {
      b2 = Yap_BigRatOfTerm(t2);
    }
    mpq_init(&new);
    mpq_add(&new, b1, b2);
    return MkRatAndClose(&new);
  }
}

Term 
Yap_gmp_sub_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init_set(&new, b1);
    mpz_sub(&new, &new, b2);
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b1, bb1;
    MP_RAT *b2, bb2;
    if (pt1[1] == BIG_INT) {
      b1 = &bb1;
      mpq_init(b1);
      mpq_set_z(b1, Yap_BigIntOfTerm(t1));
    } else {
      b1 = Yap_BigRatOfTerm(t1);
    }
    if (pt2[1] == BIG_INT) {
      b2 = &bb2;
      mpq_init(b2);
      mpq_set_z(b2, Yap_BigIntOfTerm(t2));
    } else {
      b2 = Yap_BigRatOfTerm(t2);
    }
    mpq_init(&new);
    mpq_sub(&new, b1, b2);
    return MkRatAndClose(&new);
  }
}

Term 
Yap_gmp_mul_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init_set(&new, b1);
    mpz_mul(&new, &new, b2);
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b1, bb1;
    MP_RAT *b2, bb2;
    int f1 = FALSE, f2 = FALSE;

    if (pt1[1] == BIG_INT) {
      b1 = &bb1;
      mpq_init(b1);
      mpq_set_z(b1, Yap_BigIntOfTerm(t1));
      f1 = TRUE;
    } else {
      b1 = Yap_BigRatOfTerm(t1);
    }
    if (pt2[1] == BIG_INT) {
      b2 = &bb2;
      mpq_init(b2);
      mpq_set_z(b2, Yap_BigIntOfTerm(t2));
      f2 = TRUE;
    } else {
      b2 = Yap_BigRatOfTerm(t2);
    }
    mpq_init(&new);
    mpq_mul(&new, b1, b2);
    if (f1) mpq_clear(b1);
    if (f2) mpq_clear(b2);
    return MkRatAndClose(&new);
  }
}

/* div i / b using temporary bigint new */
Term 
Yap_gmp_div_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init_set(&new, b1);
    if ( (-3 / 2) == -2 ) {
      mpz_tdiv_q(&new, &new, b2);
    } else {
      mpz_fdiv_q(&new, &new, b2);
    }
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b1, bb1;
    MP_RAT *b2, bb2;
    if (pt1[1] == BIG_INT) {
      b1 = &bb1;
      mpq_init(b1);
      mpq_set_z(b1, Yap_BigIntOfTerm(t1));
    } else {
      b1 = Yap_BigRatOfTerm(t1);
    }
    if (pt2[1] == BIG_INT) {
      b2 = &bb2;
      mpq_init(b2);
      mpq_set_z(b2, Yap_BigIntOfTerm(t2));
    } else {
      b2 = Yap_BigRatOfTerm(t2);
    }
    mpq_init(&new);
    mpq_div(&new, b1, b2);
    return MkRatAndClose(&new);
  }
}

/* div i div b using temporary bigint new */
Term 
Yap_gmp_div2_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init_set(&new, b1);
    mpz_fdiv_q(&new, &new, b2);
    return MkBigAndClose(&new);
  } else {
    MP_RAT new;
    MP_RAT *b1, bb1;
    MP_RAT *b2, bb2;
    if (pt1[1] == BIG_INT) {
      b1 = &bb1;
      mpq_init(b1);
      mpq_set_z(b1, Yap_BigIntOfTerm(t1));
    } else {
      b1 = Yap_BigRatOfTerm(t1);
    }
    if (pt2[1] == BIG_INT) {
      b2 = &bb2;
      mpq_init(b2);
      mpq_set_z(b2, Yap_BigIntOfTerm(t2));
    } else {
      b2 = Yap_BigRatOfTerm(t2);
    }
    mpq_init(&new);
    mpq_div(&new, b1, b2);
    return MkRatAndClose(&new);
  }
}

Term 
Yap_gmp_and_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init_set(&new, b1);
    mpz_and(&new, &new, b2);
    return MkBigAndClose(&new);
  } else {
    if (pt1[1] != BIG_INT) {
      Yap_ArithError(TYPE_ERROR_INTEGER, t1, "/\\/2");    
    }
    Yap_ArithError(TYPE_ERROR_INTEGER, t2, "/\\/2");    
  }
}

Term 
Yap_gmp_ior_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init_set(&new, b1);
    mpz_ior(&new, &new, b2);
    return MkBigAndClose(&new);
  } else {
    if (pt1[1] != BIG_INT) {
      Yap_ArithError(TYPE_ERROR_INTEGER, t1, "\\/ /2");    
    }
    Yap_ArithError(TYPE_ERROR_INTEGER, t2, "\\/ /2");    
  }
}

Term 
Yap_gmp_xor_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init_set(&new, b1);
    mpz_xor(&new, &new, b2);
    return MkBigAndClose(&new);
  } else {
    if (pt1[1] != BIG_INT) {
      Yap_ArithError(TYPE_ERROR_INTEGER, t1, "\\/ /2");    
    }
    Yap_ArithError(TYPE_ERROR_INTEGER, t2, "\\/ /2");    
  }
}

Term 
Yap_gmp_mod_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init(&new);
    mpz_fdiv_r(&new, b1, b2);
    return MkBigAndClose(&new);
  } else {
    if (pt1[1] != BIG_INT) {
      Yap_ArithError(TYPE_ERROR_INTEGER, t1, "mod/2");    
    }
    Yap_ArithError(TYPE_ERROR_INTEGER, t2, "mod/2");    
  }
}

Term 
Yap_gmp_mod_big_int(Term t, Int i2)
{
  CELL *pt = RepAppl(t);
  if (pt[1] != BIG_INT) {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "mod/2");
  } else {
    MP_INT *b = Yap_BigIntOfTerm(t);
    MP_INT new;

    mpz_init_set_si(&new, i2);
    mpz_fdiv_r(&new, b, &new);
    return MkBigAndClose(&new);
  }
}

Term 
Yap_gmp_mod_int_big(Int i1, Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] != BIG_INT) {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "mod/2");
  } else {
    MP_INT *b = Yap_BigIntOfTerm(t);
    /* integer is much smaller */

    if (mpz_sgn(b) > 0) {
      /* easy case next */
      if (i1 > 0) {
	/* 2 mod 23 -> 2 */
	return MkIntegerTerm(i1);
      } else {
	MP_INT new;
    
	/* 2 mod -23 -> 21 */
	mpz_init_set_si(&new, i1);
	mpz_add(&new, &new, b);
	return MkBigAndClose(&new);    
      }
    } else {
      if (i1 > 0) {
	MP_INT new;
    
	/* -2 mod 23 -> 21 */
	mpz_init_set_si(&new, i1);
	mpz_add(&new, b, &new);
	return MkBigAndClose(&new);    
      } else {
	/* -2 mod -23 -> -2 */
	return MkIntegerTerm(i1);
      }
    }
  }
}

Term 
Yap_gmp_rem_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init(&new);
    mpz_tdiv_r(&new, b1, b2);
    return MkBigAndClose(&new);
  } else {
    if (pt1[1] != BIG_INT) {
      Yap_ArithError(TYPE_ERROR_INTEGER, t1, "rem/2");    
    }
    Yap_ArithError(TYPE_ERROR_INTEGER, t2, "rem/2");    
  }
}

Term 
Yap_gmp_rem_big_int(Term t, Int i2)
{
  CELL *pt = RepAppl(t);
  if (pt[1] != BIG_INT) {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "rem/2");
  } else {
    MP_INT *b = Yap_BigIntOfTerm(t);
    MP_INT new;

    mpz_init_set_si(&new, i2);
    mpz_tdiv_r(&new, b, &new);
    return MkBigAndClose(&new);
  }
}

Term 
Yap_gmp_rem_int_big(Int i1, Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] != BIG_INT) {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "rem/2");
  } else {
    /* integer is much smaller */
    return MkIntegerTerm(i1);
  }
}

Term 
Yap_gmp_gcd_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT new;
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpz_init_set(&new, b1);
    mpz_gcd(&new, &new, b2);
    return MkBigAndClose(&new);
  } else {
    if (pt1[1] != BIG_INT) {
      Yap_ArithError(TYPE_ERROR_INTEGER, t1, "gcd/2");    
    }
    Yap_ArithError(TYPE_ERROR_INTEGER, t2, "gcd/2");    
  }
}

Term 
Yap_gmp_gcd_int_big(Int i, Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] != BIG_INT) {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "gcd/2");
  } else {
    /* integer is much smaller */
    if (i > 0) {
      return MkIntegerTerm(mpz_gcd_ui(NULL,Yap_BigIntOfTerm(t),i));
    } else if (i == 0) {
      return MkIntTerm(0);
    } else {
      return MkIntegerTerm(mpz_gcd_ui(NULL,Yap_BigIntOfTerm(t),-i));
    }
  }
}

Term
Yap_gmp_float_to_big(Float v)
{
  MP_INT new;

  mpz_init_set_d(&new, v);
  return MkBigAndClose(&new);
}

Float
Yap_gmp_to_float(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return mpz_get_d(b);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return mpq_get_d(b);
  }
}

Term
Yap_gmp_add_float_big(Float d, Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return MkFloatTerm(d+mpz_get_d(b));
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return MkFloatTerm(d+mpq_get_d(b));
  }
}

Term
Yap_gmp_sub_float_big(Float d, Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return MkFloatTerm(d-mpz_get_d(b));
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return MkFloatTerm(d-mpq_get_d(b));
  }
}

Term
Yap_gmp_sub_big_float(Term t, Float d)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return MkFloatTerm(mpz_get_d(b)-d);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return MkFloatTerm(mpq_get_d(b)-d);
  }
}

Term
Yap_gmp_mul_float_big(Float d, Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return MkFloatTerm(d*mpz_get_d(b));
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return MkFloatTerm(d*mpq_get_d(b));
  }
}

Term
Yap_gmp_fdiv_float_big(Float d, Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return MkFloatTerm(d/mpz_get_d(b));
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return MkFloatTerm(d/mpq_get_d(b));
  }
}

Term
Yap_gmp_fdiv_big_float(Term t, Float d)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return MkFloatTerm(mpz_get_d(b)/d);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return MkFloatTerm(mpq_get_d(b)/d);
  }
}

Term
Yap_gmp_exp_int_int(Int i1, Int i2)
{
  MP_INT new;
  
  mpz_init_set_si(&new, i1);
  mpz_pow_ui (&new, &new, (unsigned long int)i2);
  return MkBigAndClose(&new);
}

Term
Yap_gmp_exp_big_int(Term t, Int i)
{
  CACHE_REGS
  MP_INT new;
  
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);

    if (i > 0) {
      mpz_init(&new);
      mpz_pow_ui (&new, b, (unsigned long int)i);
    } else {
      MP_INT new;
      if (i==0) return MkIntTerm(1);
      mpz_init_set_si(&new, i);
      mpz_powm (&new, b, &new, b);
    }
    return MkBigAndClose(&new);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    Float dbl = mpq_get_d(b);
    return MkFloatTerm(pow(dbl,i));
  }
}

Term
Yap_gmp_exp_int_big(Int i, Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t, "^/2");
  } else {
    MP_INT *b = Yap_BigIntOfTerm(t);
    Float dbl = mpz_get_d(b);
    return MkFloatTerm(pow(i,dbl));
  }
}

Term 
Yap_gmp_exp_big_big(Term t1, Term t2)
{
  CACHE_REGS
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  Float dbl1, dbl2;

  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, "^/2");
  } else {
    if (pt1[1] != BIG_INT) {
      dbl1 = mpz_get_d(Yap_BigIntOfTerm(t1));
    } else {
      dbl1 = mpq_get_d(Yap_BigRatOfTerm(t1));
    }
    if (pt2[2] != BIG_INT) {
      dbl2 = mpz_get_d(Yap_BigIntOfTerm(t2));
    } else {
      dbl2 = mpq_get_d(Yap_BigRatOfTerm(t2));
    }
    return MkFloatTerm(pow(dbl1,dbl2));
  }
}


Term
Yap_gmp_big_from_64bits(YAP_LONG_LONG i)
{
  char s[64];
  MP_INT new;

#ifdef _WIN32
  snprintf(s,64,"%I64d", (long long int)i);
#elif HAVE_SNPRINTF
  snprintf(s, 64, "%lld", (long long int)i);
#else
  sprintf(s, "%lld", (long long int)i);
#endif
  mpz_init_set_str (&new, s, 10);
  return MkBigAndClose(&new);
}

Term 
Yap_gmq_rdiv_int_int(Int i1, Int i2)
{
  MP_RAT new;

  mpq_init(&new);
  if (i2 < 0) {
    i1 = -i1;
    i2 = -i2;
  }
  mpq_set_si(&new, i1, i2);
  mpq_canonicalize(&new);
  return MkRatAndClose(&new);    
}

Term 
Yap_gmq_rdiv_int_big(Int i1, Term t2)
{
  MP_RAT new;
  CELL *pt2 = RepAppl(t2);
  mpq_init(&new);
  mpq_set_si(&new, i1, 1L);
  if (pt2[1] == BIG_INT) {
    MP_RAT new2;
    MP_INT *b = Yap_BigIntOfTerm(t2);

    mpq_init(&new2);
    mpq_set_z(&new2, b);
    mpq_div(&new,&new,&new2);
    mpq_clear(&new2);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t2);
    mpq_div(&new,&new,b);
  }
  return MkRatAndClose(&new);    
}

Term 
Yap_gmq_rdiv_big_int(Term t1, Int i2)
{
  MP_RAT new;
  CELL *pt1 = RepAppl(t1);

  mpq_init(&new);
  mpq_set_si(&new, i2, 1L);
  if (pt1[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t1);
    MP_RAT new2;

    mpq_init(&new2);
    mpq_set_z(&new2, b);
    mpq_div(&new,&new2,&new);
    mpq_clear(&new2);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t1);

    mpq_div(&new,b,&new);
  }
  return MkRatAndClose(&new);    
}

Term 
Yap_gmq_rdiv_big_big(Term t1, Term t2)
{
  MP_RAT new;
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);

  mpq_init(&new);
  if (pt1[1] == BIG_INT) {
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    mpq_set_z(&new, b1);
  } else {
    MP_RAT *b1 = Yap_BigRatOfTerm(t1);
    mpq_set(&new, b1);
  }

  if (pt2[1] == BIG_INT) {
    MP_RAT new2;
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    mpq_init(&new2);
    mpq_set_z(&new2, b2);
    mpq_div(&new,&new,&new2);
    mpq_clear(&new2);
  } else {
    MP_RAT *b2 = Yap_BigRatOfTerm(t2);
    mpq_div(&new,&new,b2);
  }
  return MkRatAndClose(&new);    
}

Term 
Yap_gmp_fdiv_int_big(Int i1, Term t2)
{
  CACHE_REGS
  MP_RAT new;
  MP_RAT *b1, *b2;
  MP_RAT bb1, bb2;
  Float d;
  CELL *pt2 = RepAppl(t2);

  b1 = &bb1;
  mpq_init(b1);
  mpq_set_si(b1, i1, 1L);
  if (pt2[1] == BIG_INT) {
    b2 = &bb2;
    mpq_init(b2);
    mpq_set_z(b2, Yap_BigIntOfTerm(t2));
  } else {
    b2 = Yap_BigRatOfTerm(t2);
  }
  mpq_init(&new);
  mpq_div(&new, b1, b2);
  d = mpq_get_d(&new);
  mpq_clear(&new);
  return MkFloatTerm(d);
}

Term 
Yap_gmp_fdiv_big_int(Term t2, Int i1)
{
  CACHE_REGS
  MP_RAT new;
  MP_RAT *b1, *b2;
  MP_RAT bb1, bb2;
  Float d;
  CELL *pt2 = RepAppl(t2);

  b1 = &bb1;
  mpq_init(b1);
  mpq_set_si(b1, i1, 1L);
  if (pt2[1] == BIG_INT) {
    b2 = &bb2;
    mpq_init(b2);
    mpq_set_z(b2, Yap_BigIntOfTerm(t2));
  } else {
    b2 = Yap_BigRatOfTerm(t2);
  }
  mpq_init(&new);
  mpq_div(&new, b2, b1);
  d = mpq_get_d(&new);
  mpq_clear(&new);
  return MkFloatTerm(d);
}

Term 
Yap_gmp_fdiv_big_big(Term t1, Term t2)
{
  CACHE_REGS
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  MP_RAT new;
  MP_RAT *b1, bb1;
  MP_RAT *b2, bb2;
  Float d;

  if (pt1[1] == BIG_INT) {
    b1 = &bb1;
    mpq_init(b1);
    mpq_set_z(b1, Yap_BigIntOfTerm(t1));
  } else {
    b1 = Yap_BigRatOfTerm(t1);
  }
  if (pt2[1] == BIG_INT) {
    b2 = &bb2;
    mpq_init(b2);
    mpq_set_z(b2, Yap_BigIntOfTerm(t2));
  } else {
    b2 = Yap_BigRatOfTerm(t2);
  }
  mpq_init(&new);
  mpq_div(&new, b1, b2);
  d = mpq_get_d(&new);
  mpq_clear(&new);
  return MkFloatTerm(d);
}

int 
Yap_gmp_cmp_big_int(Term t, Int i)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return mpz_cmp_si(b,i);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return mpq_cmp_si(b,i,1);
  }
}

int 
Yap_gmp_cmp_int_big(Int i, Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return -mpz_cmp_si(b,i);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return -mpq_cmp_si(b,i,1);
  }
}

int 
Yap_gmp_cmp_big_float(Term t, Float d)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return mpz_cmp_d(b,d);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    Float d1 = mpq_get_d(b);
    if (d1 < d)
      return -1;
    if (d1 == d)
      return 0;
    return 1;
  }
}

int 
Yap_gmp_cmp_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);
  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    return mpz_cmp(b1, b2);
  } else {
    MP_RAT *b1 = NULL, bb1;
    int f1 = FALSE;
    MP_RAT *b2 = NULL, bb2;
    int f2 = FALSE;
    if (pt1[1] == BIG_INT) {
      b1 = &bb1;
      f1 = TRUE;
      mpq_init(b1);
      mpq_set_z(b1, Yap_BigIntOfTerm(t1));
    } else {
      b1 = Yap_BigRatOfTerm(t1);
    }
    if (pt2[1] == BIG_INT) {
      b2 = &bb2;
      f2 = TRUE;

      mpq_init(b2);
      mpq_set_z(b2, Yap_BigIntOfTerm(t2));
    } else {
      b2 = Yap_BigRatOfTerm(t2);
    }
    if (f1)
      mpq_clear(b1);
    if (f2)
      mpq_clear(b2);
    return mpq_cmp(b1, b2);
  }
}

int 
Yap_gmp_tcmp_big_int(Term t, Int i)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return mpz_cmp_si(b,i);
  } else {
    return -1;
  }
}

int 
Yap_gmp_tcmp_int_big(Int i, Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return -mpz_cmp_si(b,i);
  } else {
    return 1;
  }
}

int 
Yap_gmp_tcmp_big_float(Term t, Float d)
{
  return 1;
}

int 
Yap_gmp_tcmp_big_big(Term t1, Term t2)
{
  CELL *pt1 = RepAppl(t1);
  CELL *pt2 = RepAppl(t2);

  if (pt1[1] == BIG_INT && pt2[1] == BIG_INT) {
    MP_INT *b1 = Yap_BigIntOfTerm(t1);
    MP_INT *b2 = Yap_BigIntOfTerm(t2);

    return mpz_cmp(b1, b2);
  } else {
    MP_RAT *b1, *b2;

    if (pt1[1] == BIG_INT) {
      return 1;
    } else if (pt1[1] == BIG_RATIONAL) {
      b1 = Yap_BigRatOfTerm(t1);
    } else {
      return pt1-pt2;
    }
    if (pt2[1] == BIG_INT) {
      return -1;
    } else if (pt2[1] == BIG_RATIONAL) {
      b2 = Yap_BigRatOfTerm(t2);
    } else {
      return pt1-pt2;
    }
    return mpq_cmp(b1, b2);
  }
}

Term
Yap_gmp_neg_int(Int i)
{
  MP_INT new;

  mpz_init_set_si(&new, Int_MIN);
  mpz_neg(&new, &new);
  return MkBigAndClose(&new);
}

Term
Yap_gmp_neg_big(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    MP_INT new;
    mpz_init_set(&new, b);
    mpz_neg(&new, &new);
    return MkBigAndClose(&new);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    MP_RAT new;
    mpq_init(&new);
    mpq_neg(&new, b);
    return MkRatAndClose(&new);
  }
}

Term
Yap_gmp_float_to_rational(Float dbl)
{
  MP_RAT new;
  mpq_init(&new);
  mpq_set_d(&new, dbl);
  return MkRatAndClose(&new);  
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A is rationalize(Float)

Introduced on the suggestion of Richard   O'Keefe  after the Common Lisp
standard. The algorithm is taken from figure  3 in ``A Rational Rotation
Method for Robust Geometric Algorithms'' by John Canny, Bruce Donald and
Eugene K. Ressler.  Found at

http://www.cs.dartmouth.edu/~brd/papers/rotations-scg92.pdf
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef DBL_EPSILON			/* normal for IEEE 64-bit double */
#define DBL_EPSILON 0.00000000000000022204
#endif

Term
Yap_gmp_float_rationalize(Float dbl)
{
  Float e0 = dbl, p0 = 0.0, q0 = 1.0;
  Float e1 = -1.0, p1 = 1.0, q1 = 0.0;
  Float d;
  MP_RAT new;

  do { Float r = floor(e0/e1);
    Float e00 = e0, p00 = p0, q00 = q0;
    e0 = e1;
    p0 = p1;
    q0 = q1;
    e1 = e00 - r*e1;
    p1 = p00 - r*p1;
    q1 = q00 - r*q1;

    d = p1/q1 - dbl;
  } while(fabs(d) > DBL_EPSILON);

  mpz_init_set_d(mpq_numref(&new), p1);
  mpz_init_set_d(mpq_denref(&new), q1);
  mpq_canonicalize(&new);	/* is this needed? */
  return MkRatAndClose(&new);  
}

Term
Yap_gmp_abs_big(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    MP_INT new;
    mpz_init_set(&new, b);
    mpz_abs(&new, &new);
    return MkBigAndClose(&new);
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    MP_RAT new;
    mpq_init(&new);
    mpq_abs(&new, b);
    return MkRatAndClose(&new);
  }
}

Term
Yap_gmp_unot_big(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    MP_INT new;
    mpz_init_set(&new, b);
    mpz_com(&new, &new);
    return MkBigAndClose(&new);
  } else {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "#/1");
  }
}

Term
Yap_gmp_floor(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    return t;
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    MP_INT new;
    mpz_init(&new);
    mpz_set_q(&new, b);
    if (mpq_sgn(b) < 0 && mpz_cmp_si(mpq_denref(b),1L) != 0) {
      mpz_sub_ui(&new,&new,1L);
    }
    return MkBigAndClose(&new);
  }
}

Term
Yap_gmp_ceiling(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    return t;
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    MP_INT new;
    mpz_init(&new);
    mpz_set_q(&new, b);
    if (mpq_sgn(b) > 0 && mpz_cmp_si(mpq_denref(b),1L) != 0) {
      mpz_add_ui(&new,&new,1L);
    }
    return MkBigAndClose(&new);
  }
}

Term
Yap_gmp_round(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    return t;
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    MP_INT new;
    MP_RAT half, q;

    mpq_init(&half);
    mpq_init(&q);
    mpq_set_ui(&half, 1, 2);		/* 1/2 */
    if ( mpq_sgn(b) > 0 )
      mpq_add(&q, b, &half);
    else {
      mpq_sub(&q, b, &half);
    }
    mpz_init(&new);
    mpz_set_q(&new, &q);
    mpq_clear(&half);
    mpq_clear(&q);
    return MkBigAndClose(&new);
  }
}

Term
Yap_gmp_trunc(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    return t;
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    MP_INT new;
    int sgn = mpq_sgn(b);

    if (sgn) 
      mpq_neg(b, b);
    mpz_init(&new);
    mpz_set_q(&new, b);
    if (sgn) {
      mpq_neg(b, b);
      mpz_neg(&new, &new);
    }
    return MkBigAndClose(&new);
  }
}

Term
Yap_gmp_float_fractional_part(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    Yap_ArithError(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(%f)", FloatOfTerm(t));
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    MP_RAT new;

    mpq_init(&new);
    mpz_tdiv_q(mpq_numref(&new),
	       mpq_numref(b),
	       mpq_denref(b));
    mpz_set_ui(mpq_denref(&new), 1);
    mpq_sub(&new, b, &new);
    return MkRatAndClose(&new);
  }
}

Term
Yap_gmp_float_integer_part(Term t)
{
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    Yap_ArithError(TYPE_ERROR_FLOAT, t, "X is float_integer_part(%f)", FloatOfTerm(t));
  } else {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    MP_INT new;

    mpz_init(&new);
    mpz_tdiv_q(&new,
	       mpq_numref(b),
	       mpq_denref(b));
    return MkBigAndClose(&new);
  }
}

Term
Yap_gmp_sign(Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    return MkIntegerTerm(mpz_sgn(Yap_BigIntOfTerm(t)));
  } else {
    return MkIntegerTerm(mpq_sgn(Yap_BigRatOfTerm(t)));
  }
}

Term
Yap_gmp_lsb(Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *big = Yap_BigIntOfTerm(t);
    if ( mpz_sgn(big) <= 0 ) {
      Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t,
			    "lsb/1 received negative bignum");
    }
    return MkIntegerTerm(mpz_scan1(big,0));
  } else {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "lsb");    
  }
}

Term
Yap_gmp_msb(Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *big = Yap_BigIntOfTerm(t);
    if ( mpz_sgn(big) <= 0 ) {
      Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t,
			    "msb/1 received negative bignum");
    }
    return MkIntegerTerm(mpz_sizeinbase(big,2));
  } else {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "popcount");    
  }
}

Term
Yap_gmp_popcount(Term t)
{
  CACHE_REGS
  CELL *pt = RepAppl(t);
  if (pt[1] == BIG_INT) {
    MP_INT *big = Yap_BigIntOfTerm(t);
    if ( mpz_sgn(big) <= 0 ) {
      Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t,
			    "popcount/1 received negative bignum");
    }
    return MkIntegerTerm(mpz_popcount(big));
  } else {
    Yap_ArithError(TYPE_ERROR_INTEGER, t, "popcount");    
  }
}

char * 
Yap_mpz_to_string(	MP_INT *b, char *s, size_t sz, int base)
{
  if (s) {
    size_t size = mpz_sizeinbase(b, base);
    if (size+2 > sz) {
      return NULL;
    }
    return mpz_get_str (s, base, b);
  }
  return NULL;
}

char * 
Yap_gmp_to_string(Term t, char *s, size_t sz, int base)
{
  if (RepAppl(t)[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);

    if (s) {
      size_t size = mpz_sizeinbase(b, base);
      if (size+2 > sz) {
	return NULL;
      }
    }
    return mpz_get_str (s, base, b);
  } else if (RepAppl(t)[1] == BIG_RATIONAL) {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    size_t pos;
    size_t siz =
	mpz_sizeinbase(mpq_numref(b), base)+
	mpz_sizeinbase(mpq_denref(b), base)+
	8;
    if (s) {
      if (siz > sz) {
	return NULL;
      }
    } else {
      if (!(s = malloc(siz)))
	return NULL;
    }
    strncpy(s,"rdiv(",sz);
    pos = strlen(s);
    mpz_get_str (s+pos, base, mpq_numref(b));
    pos = strlen(s);
    s[pos] = ',';
    mpz_get_str (s+(pos+1), base, mpq_denref(b));
    pos = strlen(s);
    s[pos] = ')';
  }
  return s;
}

size_t 
Yap_gmp_to_size(Term t, int base)
{
  if (RepAppl(t)[1] == BIG_INT) {
    MP_INT *b = Yap_BigIntOfTerm(t);
    return mpz_sizeinbase(b, base);
  } else if (RepAppl(t)[1] == BIG_RATIONAL) {
    MP_RAT *b = Yap_BigRatOfTerm(t);
    return
	mpz_sizeinbase(mpq_numref(b), base)+
	mpz_sizeinbase(mpq_denref(b), base)+
	8;
  }
  return 1;
}

int
Yap_term_to_existing_big(Term t, MP_INT *b)
{
  if  (IsVarTerm(t))
    return FALSE;
  if (IsIntegerTerm(t)) {
    mpz_set_si(b,IntegerOfTerm(t));
    return TRUE;
  }
  if  (IsBigIntTerm(t)) {
    if (RepAppl(t)[1] != BIG_INT)
      return FALSE;
    mpz_set(b,Yap_BigIntOfTerm(t));
    return TRUE;
  }
  return FALSE;
}

int
Yap_term_to_existing_rat(Term t, MP_RAT *b)
{
  if  (IsVarTerm(t))
    return FALSE;
  if (IsIntegerTerm(t)) {
    mpq_set_si(b, IntegerOfTerm(t), 1);
    return TRUE;
  }
  if  (IsBigIntTerm(t)) {
    CELL flag = RepAppl(t)[1];
    if (flag == BIG_INT) {
      mpq_set_z(b, Yap_BigIntOfTerm(t));
      return TRUE;
    }
    if (flag == BIG_RATIONAL) {
      mpq_set(b, Yap_BigRatOfTerm(t));
      return TRUE;
    }
  }
  return FALSE;
}

#endif


