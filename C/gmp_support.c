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
#include "Heap.h"
#include "eval.h"

#if USE_GMP

/* add i + j using temporary bigint new */
Term
Yap_gmp_add_ints(Int i, Int j)
{
  MP_INT new;
  Term t;

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
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
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
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term
Yap_gmp_mul_ints(Int i, Int j)
{
  MP_INT new;
  Term t;

  mpz_init_set_si(&new,i);
  mpz_mul_si(&new, &new, j);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term 
Yap_gmp_sll_ints(Int i, Int j)
{
  MP_INT new;
  Term t;

  mpz_init_set_si(&new,i);
  mpz_mul_2exp(&new, &new, j);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

/* add i + b using temporary bigint new */
Term 
Yap_gmp_add_int_big(Int i, MP_INT *b)
{
  MP_INT new;
  Term t;

  mpz_init_set_si(&new, i);
  mpz_add(&new, &new, b);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

/* sub i - b using temporary bigint new */
Term 
Yap_gmp_sub_int_big(Int i, MP_INT *b)
{
  MP_INT new;
  Term t;

  mpz_init_set_si(&new, i);
  mpz_sub(&new, &new, b);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

/* add i + b using temporary bigint new */
Term 
Yap_gmp_mul_int_big(Int i, MP_INT *b)
{
  MP_INT new;
  Term t;

  mpz_init_set_si(&new, i);
  mpz_mul(&new, &new, b);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

/* sub i - b using temporary bigint new */
Term 
Yap_gmp_sub_big_int(MP_INT *b, Int i)
{
  MP_INT new;
  Term t;

  mpz_init_set_si(&new, i);
  mpz_neg(&new, &new);
  mpz_add(&new, &new, b);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

/* div i / b using temporary bigint new */
Term 
Yap_gmp_div_big_int(MP_INT *b, Int i)
{
  MP_INT new;
  Term t;

  mpz_init_set(&new, b);
  if (yap_flags[INTEGER_ROUNDING_FLAG] == 0) {
    if (i > 0) {
      mpz_tdiv_q_ui(&new, &new, i);
    } else if (i == 0) {
      Yap_Error(EVALUATION_ERROR_ZERO_DIVISOR, MkIntTerm(0), "// /2");
      return 0L;
    } else {
      /* we do not handle MIN_INT */
      mpz_tdiv_q_ui(&new, &new, -i);
      mpz_neg(&new, &new);
    }
  } else {
    if (i > 0) {
      mpz_fdiv_q_ui(&new, &new, i);
    } else if (i == 0) {
      Yap_Error(EVALUATION_ERROR_ZERO_DIVISOR, MkIntTerm(0), "// /2");
      return 0L;
    } else {
      /* we do not handle MIN_INT */
      mpz_fdiv_q_ui(&new, &new, -i);
      mpz_neg(&new, &new);
    }
  }
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

/* sub i - b using temporary bigint new */
Term 
Yap_gmp_and_int_big(Int i, MP_INT *b)
{
  MP_INT new;
  Term t;

  mpz_init_set_si(&new, i);
  mpz_and(&new, &new, b);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

/* sub i - b using temporary bigint new */
Term 
Yap_gmp_ior_int_big(Int i, MP_INT *b)
{
  MP_INT new;
  Term t;

  mpz_init_set_si(&new, i);
  mpz_ior(&new, &new, b);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

/* add i + b using temporary bigint new */
Term 
Yap_gmp_sll_big_int(MP_INT *b, Int i)
{
  MP_INT new;
  Term t;

  if (i > 0) {
    mpz_init_set(&new, b);
    mpz_mul_2exp(&new, &new, i);
  } else if (i == 0) {
    mpz_init_set(&new, b);
  } else {
    mpz_init_set(&new, b);
    if (i == Int_MIN) {
      return 0L;
    }
    mpz_tdiv_q_2exp(&new, &new, -i);
  }
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term 
Yap_gmp_add_big_big(MP_INT *b1, MP_INT *b2)
{
  MP_INT new;
  Term t;

  mpz_init_set(&new, b1);
  mpz_add(&new, &new, b2);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term 
Yap_gmp_sub_big_big(MP_INT *b1, MP_INT *b2)
{
  MP_INT new;
  Term t;

  mpz_init_set(&new, b1);
  mpz_sub(&new, &new, b2);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term 
Yap_gmp_mul_big_big(MP_INT *b1, MP_INT *b2)
{
  MP_INT new;
  Term t;

  mpz_init_set(&new, b1);
  mpz_mul(&new, &new, b2);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

/* div i / b using temporary bigint new */
Term 
Yap_gmp_div_big_big(MP_INT *b1, MP_INT *b2)
{
  MP_INT new;
  Term t;

  mpz_init_set(&new, b1);
  if (yap_flags[INTEGER_ROUNDING_FLAG] == 0) {
    mpz_tdiv_q(&new, &new, b2);
  } else {
    mpz_fdiv_q(&new, &new, b2);
  }
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term 
Yap_gmp_and_big_big(MP_INT *b1, MP_INT *b2)
{
  MP_INT new;
  Term t;

  mpz_init_set(&new, b1);
  mpz_and(&new, &new, b2);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term 
Yap_gmp_ior_big_big(MP_INT *b1, MP_INT *b2)
{
  MP_INT new;
  Term t;

  mpz_init_set(&new, b1);
  mpz_ior(&new, &new, b2);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term
Yap_gmp_add_float_big(Float d, MP_INT *b)
{
  return MkFloatTerm(d+mpz_get_d(b));
}

Term
Yap_gmp_sub_float_big(Float d, MP_INT *b)
{
  return MkFloatTerm(d-mpz_get_d(b));
}

Term
Yap_gmp_sub_big_float(MP_INT *b, Float d)
{
  return MkFloatTerm(mpz_get_d(b)-d);
}

Term
Yap_gmp_mul_float_big(Float d, MP_INT *b)
{
  return MkFloatTerm(d*mpz_get_d(b));
}

Term
Yap_gmp_exp_ints(Int i1, Int i2)
{
  MP_INT new;
  Term t;
  
  mpz_init_set_si(&new, i1);
  mpz_pow_ui (&new, &new, (unsigned long int)i2);
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

Term
Yap_gmp_exp_big_int(MP_INT *b, Int i)
{
  MP_INT new;
  Term t;
  
  if (b > 0) {
    mpz_init(&new);
    mpz_pow_ui (&new, b, (unsigned long int)i);
  } else {
    MP_INT new;
    if (b==0) return MkIntTerm(1);

    mpz_init_set_si(&new, i);
    mpz_powm (&new, b, &new, b);
  }
  t = Yap_MkBigIntTerm(&new);
  mpz_clear(&new);
  return t;
}

#endif


