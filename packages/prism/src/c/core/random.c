/*

This source module contains reduced (and slightly modified) version
of mt19937ar.c implemented by Makoto Matsumoto and Takuji Nishimura.
The original file is available in the following website:

    http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html

Here is the original copyright notice.

========================================================================

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote
        products derived from this software without specific prior written
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

========================================================================

*/

/***********[  REDUCED VERSION OF MT19937AR.C STARTS HERE  ]***********/

void init_by_array(unsigned long init_key[], int key_length);
int pc_random_auto_seed_1(void);
int pc_random_init_by_seed_1(void);
int pc_random_init_by_list_1(void);
int pc_random_float_1(void);
int pc_random_gaussian_1(void);
int pc_random_int_2(void);
int pc_random_int_3(void);
int pc_random_get_state_1(void);
int pc_random_set_state_1(void);

/* Period parameters */
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

static unsigned long mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* initializes mt[N] with a seed */
static void init_genrand(unsigned long s)
{
    mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt[mti] =
            (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti);
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
/* slight change for C++, 2004/2/26 */
void init_by_array(unsigned long init_key[], int key_length)
{
    int i, j, k;
    init_genrand(19650218UL);
    i=1;
    j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
                + init_key[j] + j; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        j++;
        if (i>=N) {
            mt[0] = mt[N-1];
            i=1;
        }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
                - i; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) {
            mt[0] = mt[N-1];
            i=1;
        }
    }

    mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */
}

/* generates a random number on [0,0xffffffff]-interval */
static unsigned long genrand_int32(void)
{
    unsigned long y;
    static unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if init_genrand() has not been called, */
            init_genrand(5489UL); /* a default initial seed is used */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        mti = 0;
    }

    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}

/* generates a random number on [0,1) with 53-bit resolution */
static double genrand_res53(void)
{
    unsigned long a=genrand_int32()>>5, b=genrand_int32()>>6;
    return(a*67108864.0+b)*(1.0/9007199254740992.0);
}
/* These real versions are due to Isaku Wada, 2002/01/09 added */

/***********[   REDUCED VERSION OF MT19937AR.C ENDS HERE   ]***********/

/*--------------------------------------------------------------------*/

#include <math.h>
#include <time.h>
#include <string.h>
#include <assert.h>
#include "core/bpx.h"
#include "core/random.h"
#include "core/vector.h"

#ifndef M_PI
#define M_PI (3.14159265358979324)
#endif

static int gauss_flag = 0;

/*--------------------------------------------------------------------*/

int random_int(int n)
{
    unsigned long p, q, r;

    assert(n > 0);

    if (n == 1) {
        return 0;
    }

    p = 0xFFFFFFFFul - (0xFFFFFFFFul % n + 1) % n;
    q = p / n + 1;

    while ((r = genrand_int32()) > p) ;
    return (int)(r / q);
}

double random_float(void)
{
    return genrand_res53();
}

/* Box-Muller method */
double random_gaussian(double mu, double sigma)
{
    double u1, u2;
    static double g1, g2;

    gauss_flag = !(gauss_flag);

    if (gauss_flag) {
        u1 = genrand_res53();
        u2 = genrand_res53();
        g1 = sqrt(-2.0 * log(u1)) * cos(2.0 * M_PI * u2);
        g2 = sqrt(-2.0 * log(u1)) * sin(2.0 * M_PI * u2);
        return sigma * g1 + mu;
    }
    else {
        return sigma * g2 + mu;
    }
}

/*  N(0,1)-version:
double random_gaussian(void)
{
    double u1, u2;
    static double next;

    gauss_flag = !(gauss_flag);

    if (gauss_flag) {
        do {
            u1 = genrand_res53();
        }
        while (u1 == 0.0);
        do {
            u2 = genrand_res53();
        }
        while (u2 == 0.0);
        next = sqrt(-2.0 * log(u1)) * sin(2.0 * M_PI * u2);
        return sqrt(-2.0 * log(u1)) * cos(2.0 * M_PI * u2);
    }
    else {
        return next;
    }
}
*/

/*--------------------------------------------------------------------*/

int pc_random_auto_seed_1(void)
{
  CACHE_REGS
    BPLONG seed = (BPLONG)(time(NULL));
    return bpx_unify(ARG(1,1), bpx_build_integer(seed));
}

int pc_random_init_by_seed_1(void)
{
  CACHE_REGS
    init_genrand((unsigned long)(bpx_get_integer(ARG(1,1))));
    return BP_TRUE;
}

int pc_random_init_by_list_1(void)
{
  CACHE_REGS
    unsigned long *seed;
    TERM t, u;

    VECTOR_INIT(seed);

    t = ARG(1,1);

    while (! bpx_is_nil(t)) {
        u = bpx_get_car(t);
        t = bpx_get_cdr(t);
        VECTOR_PUSH(seed, (unsigned long)(bpx_get_integer(u)));
    }

    init_by_array(seed, VECTOR_SIZE(seed));
    return BP_TRUE;
}

int pc_random_float_1(void)
{
  CACHE_REGS
    return bpx_unify(ARG(1,1), bpx_build_float(random_float()));
}

int pc_random_gaussian_1(void)
{
  CACHE_REGS
    return bpx_unify(ARG(1,1), bpx_build_float(random_gaussian(0.0,1.0)));
}

int pc_random_int_2(void)
{
  CACHE_REGS
    int n_max = bpx_get_integer(ARG(1,2));
    int n_out = random_int(n_max);
    return bpx_unify(ARG(2,2), bpx_build_integer((BPLONG)(n_out)));
}

int pc_random_int_3(void)
{
  CACHE_REGS
    int n_min = bpx_get_integer(ARG(1,3));
    int n_max = bpx_get_integer(ARG(2,3));
    int n_out = random_int(n_max - n_min + 1) + n_min;
    return bpx_unify(ARG(3,3), bpx_build_integer((BPLONG)(n_out)));
}

/*--------------------------------------------------------------------*/

int pc_random_get_state_1(void)
{
  CACHE_REGS
    int  i, j;
    TERM t, u;
    unsigned long temp;

    t = bpx_build_structure("$randstate", 4 * N / 3 + 1);
    bpx_unify(bpx_get_arg(1, t), bpx_build_integer(mti));

    for (i = 0; i < 4 * N / 3; i++) {
        j = i / 4 * 3;
        temp = 0;

        if (i % 4 > 0) {
            temp |= mt[j + i % 4 - 1] << (8 * (3 - i % 4));
        }
        if (i % 4 < 3) {
            temp |= mt[j + i % 4 - 0] >> (8 * (1 + i % 4));
        }

        temp &= 0xFFFFFF; /* == 2^24 - 1 */
        u = bpx_get_arg(i + 2, t);
        bpx_unify(u, bpx_build_integer(temp));
    }

    return bpx_unify(ARG(1,1), t);
}

int pc_random_set_state_1(void)
{
  CACHE_REGS
    int  i, j;
    TERM term;
    unsigned long temp;

    term = ARG(1,1);

    assert(strcmp(bpx_get_name(term), "$randstate") == 0);
    assert(bpx_get_arity(term) == 4 * N / 3 + 1);

    mti = bpx_get_integer(bpx_get_arg(1, term));

    for (i = 0; i < N; i++) {
        j = i / 3 * 4;
        mt[i] = 0;
        temp = bpx_get_integer(bpx_get_arg(j + i % 3 + 2, term));
        mt[i] |= temp << (8 * (1 + i % 3));
        temp = bpx_get_integer(bpx_get_arg(j + i % 3 + 3, term));
        mt[i] |= temp >> (8 * (2 - i % 3));
        mt[i] &= 0xFFFFFFFF;
    }

    return BP_TRUE;
}

/*--------------------------------------------------------------------*/
