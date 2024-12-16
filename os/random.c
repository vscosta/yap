/**
   @file random.c
   @brief YAP interface to random number generators.

*/

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef _WIN32
#if HAVE_IO_H
/* Windows */
#include <io.h>
#endif 
#if HAVE_SOCKET
#include <winsock2.h>
#endif
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR)==_S_IFDIR)
#endif
#endif

#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include "iopreds.h"

#if HAVE_TIME_H
#include <time.h>
#endif

// INLINE_ONLY  Term isatom( Term inp );
typedef    double drandom_t(void); /// returns a doublr
typedef     UInt lrandom_t(void); /// returns a positive number()
typedef     Int mrandom_t(void);
typedef      UInt random_range_t(unsigned int);
typedef      void srandom_t(unsigned int);


  typedef struct rdesc_t {
    const char *s;
    UInt max;
    drandom_t *drandom_;
    lrandom_t *lrandom_;
    mrandom_t *mrandom_;
    random_range_t *random_range_;
    srandom_t* qsrandom_;
    bool available;
  } rdesc_t;

#if HAVE_SRAND48


static UInt mylrand48(void)
{
  return lrand48 ();
}

static Int
mymrand48(void)
{
  return mrand48 ();
}

static  void mysrand48(unsigned int seed)
{
   srand48 (seed);
}

static UInt rand48_range(unsigned int range)
{
  return (lrand48 () *range)/  ((1<<30)-1) /* 2**31-1 */;
}
#else
#define rand48_range NULL
#define mylrand48 NULL
#define  myrand48 NULL
#define mysrand48 NULL
#endif

#if HAVE_RANDOM
static double   drandom(void)
{
  return ((double) random ()) / 0x7fffffffL; /* 2**31-1 */;
}

              static UInt myrandom(void)

	      {
		return  random ();
	      }

              static Int mymrandom(void)

	      {
		return  random ();
	      }

static UInt
random_range(unsigned int range)
{
  return (random () *range)/  0x7fffffffL; /* 2**31-1 */;
}

#else
#define drandom NULL
#define myrandom NULL
#define random_range NULL
#endif

#if HAVE_RAND
static double drand(void)
{
  return (((double) rand ()) / RAND_MAX /* 2**31-1 */);
}

static UInt myrand(void)
{
  return  rand ();
}


static Int mrand(void)
{      
  return rand () - (RAND_MAX/2); /* 2**31-1 */
}

static UInt rand_range(unsigned int range)
{
  return (random () *range)/  RAND_MAX; /* 2**31-1 */;
}

#else
#define drand NULL
  #define rand_range NULL
#endif

#if HAVE_ARC4RANDOM
static double darc4random(void)
{
  return ((double) arc4random ()) /( (1<<30)-1);
}

static UInt larc4random(void)
{
  return  arc4random ();
}

static UInt larc4random_uniform(unsigned int range)
{
  return  arc4random_uniform (range);
}


static Int marc4random(void)
{
  return arc4random()- (1<<29);
}

static void sarc4random(unsigned int seed)
{
  Yap_ThrowError(DOMAIN_ERROR_SEED_NOT_AVAILABLE, MkIntTerm(seed), "arc4random");
}
#else
#define arc4random NULL
#define larc4random_uniform NULL
#define darc4random NULL
#define larc4random NULL
#define marc4random NULL
#define sarc4random NULL
#endif

struct rdesc_t *rdef;

static  struct rdesc_t names[] =  
  {
    { "random",
    (1<<30)-1,
    drandom,
    myrandom,
    mymrandom,
    random_range,
    srandom,
#if HAVE_RANDOM
    true
#else
      false
#endif
  },
  { "rand48",
    (((uint64_t)1)<<46)-1,
     drand48,
      mylrand48,
      mymrand48,
      rand48_range,
      mysrand48,
      #if HAVE_SRAND48

      true
#else
      false
#endif
    },
  { "rand",
    RAND_MAX,
    drand,
    myrand,
    mrand,
    rand_range,
    srand,
#if HAVE_RAND
    true
#else
      false
#endif
  },
  { "arc4random",
    (1<<30)-1,
    darc4random,
    larc4random,
    marc4random,
   larc4random_uniform,
    sarc4random,
 #if HAVE_ARC4RANDOM
    true
#else
      false
#endif
  },
    { NULL,
    0,
    NULL,
    NULL,
    NULL,
    NULL,
      false
  }
  } ;


  typedef struct rand_generator_t {
    Term tname;
    bool available;
} rg_t;

  static struct rand_generator_t *rg;
  
static void init_rand_generator(void) {
  size_t sz = sizeof(names)/(sizeof(struct  rand_generator_t));
  struct rand_generator_t *r = rg = malloc(sz*sizeof(struct rand_generator_t));
  int i;
  for (i=0;names[i].s != NULL;i++) {
    r[i].tname=MkAtomTerm(Yap_LookupAtom(names[i].s));
    r[i].available=names[i].available;
  }
  r[i].tname=0;
  rdef = names;
}
 
 Term Yap_israndgen(Term inp) {
  //CACHE_REGS
  const char *s;
  if (IsVarTerm(inp)) {
    Yap_ThrowError(INSTANTIATION_ERROR, inp, "set_prolog_flag %s",
              "value must be bound");
    return TermZERO;
  }
  if (IsStringTerm(inp)) {
    s = StringOfTerm(inp);
    inp = MkAtomTerm(Yap_LookupAtom(s));
  } else if (IsAtomTerm(inp)) {
    s = StrOfAtomTerm(inp);
  } else {
    Yap_ThrowError(TYPE_ERROR_ATOM, inp, "set_prolog_flag");
  return TermZERO;
  }
  while (rg->tname) {
    if (inp != rg->tname && rg->available) {
        GLOBAL_Flags[RANDOM_NUMBER_GENERATOR_FLAG].at = inp;
	rdef = names+inp;
	return inp;
    }
    else {
    Yap_ThrowError(TYPE_ERROR_ATOM, inp, "set_prolog_flag");
    return false;
    }
  }
  Yap_ThrowError(TYPE_ERROR_ATOM, inp, "set_prolog_flag");
  return false;
}


static unsigned int current_seed;

/**
   @defgroup YAPRandom Interface to the OS number rg.
   @ingroup InputOutput
`   
   @{

Older versios of YAP supported random number access through the
expression `X is random`. Inspired by SWI-Prolog, the newer code
supports:

 - initialization;
 - random numbers as integers (signed or absolute) or as floating points;
 - cryptographic quality random numbers

 By default YAP uses `random`, the pseudo-random number generator. Use the prolog flag `random_number_generator` if you prefer `random` or `rand`.
 Use `arc4random` for higher quality random numbers, that do not have a seed. 

   

*/



double
Yap_random (void) {
  return rdef->drandom_();
}


UInt
Yap_unsigned_integer_random (void)
{ 
return rdef->lrandom_();
}


Int
Yap_signed_integer_random (void)
{
return rdef->mrandom_();

}

static  Int
Yap_integer_in_range (uint64_t min, uint64_t max)
{ 
  return min+rdef->random_range_(max-min);
}
  
static  Int
Yap_double_in_range (double min, double max)
{  
  return min+rdef->drandom_()*(max-min);
}

#if HAVE_RANDOM
/**
   @pred init_random_state( +__Seed__, __OLd__, __New__)

Seed the random number generator, where _Seed_ is a 32-bit number, collect the previous state and set up the new state.

This code interfaces to the random(), initstate(), and srandom() family of routines.
*/
static Int
p_init_random_state ( USES_REGS1 )
{
  register Term t0 = Deref (ARG1);
  char *old, *new;

  if (IsVarTerm (t0)) {
    return(Yap_unify(ARG1,MkIntegerTerm((Int)current_seed)));
  }
  if(!IsNumTerm (t0))
    return (FALSE);
  if (IsIntTerm (t0))
    current_seed = (unsigned int) IntOfTerm (t0);
  else if (IsFloatTerm (t0))
    current_seed  = (unsigned int) FloatOfTerm (t0);
  else
    current_seed  = (unsigned int) LongIntOfTerm (t0);

  new = (char *) malloc(256);
  old = initstate(current_seed, new, 256);

  return Yap_unify(ARG2, MkAddressTerm(old)) &&
    Yap_unify(ARG3, MkAddressTerm(new));
}

/**
   @pred set_random_state( +_New_, -_Old_)

Seed the random number generatorgenerator state  to _New_ and unify _Old) with the
previous state.
 */
static Int
p_set_random_state ( USES_REGS1 )
{
  register Term t0 = Deref (ARG1);
  char *old, * new;

  if (IsVarTerm (t0)) {
    return FALSE;
  }
  if (IsIntegerTerm (t0))
    new = (char *) IntegerOfTerm (t0);
  else
    return FALSE;
  old = setstate( new );
  return Yap_unify(ARG2, MkIntegerTerm((Int)old));
}

/**
   @pred release_random_state( +_State_)

 Remove the the memory allocated to store _State_.
 */

static Int
p_release_random_state ( USES_REGS1 )
{
  register Term t0 = Deref (ARG1);
  char *old;

  if (IsVarTerm (t0)) {
    return FALSE;
  }
  if (IsIntegerTerm (t0))
    old = (char *) IntegerOfTerm (t0);
  else
    return FALSE;
  free( old );
  return TRUE;
}
#endif

/**
   @pred srandom(_Seed_)

Set  the _Seed_ for the pseudo random generator.
*/
static Int
Srandom ( USES_REGS1 )
{
  register Term t0 = Deref (ARG1);
  if (IsVarTerm (t0)) {
    return(Yap_unify(ARG1,MkIntegerTerm((Int)current_seed)));
  }
  if(!IsNumTerm (t0))
    return (FALSE);
  if (IsIntTerm (t0))
    current_seed = (unsigned int) IntOfTerm (t0);
  else if (IsFloatTerm (t0))
    current_seed  = (unsigned int) FloatOfTerm (t0);
  else
    current_seed  = (unsigned int) LongIntOfTerm (t0);
#if HAVE_SRAND48
  srand48(current_seed);
#elif HAVE_SRANDOM
  srandom(current_seed);
#elif HAVE_SRAND
  srand(current_seed);

#endif
  return (TRUE);
}


/**
   @pred random(-_Positive_Integer_)

Unify the first argument with an  unsigned pseudo-random integer.
 */
static Int URandom(USES_REGS1)
{
  return Yap_unify(ARG1, MkIntegerTerm(Yap_unsigned_integer_random()));
}


/**
   @pred random(+_Min_,+_Max_,-_R_)

Unify the third argument with an pseudo-random number in the interval `[Min-Max[`. If both _Min_ and _Max_ are integers, _R_ will also be an integer, otherwise _R will be a floating point number. 
 */
static Int random3(USES_REGS1)
{
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2), t3;
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2)) {
    t3 = MkIntegerTerm(Yap_integer_in_range(IntegerOfTerm(t1),IntegerOfTerm(t2))
		       );
  } else {
    double d1,d2;
    if (IsIntegerTerm(t1)) d1=IntegerOfTerm(t1); else d1=FloatOfTerm(t1);
    if (IsIntegerTerm(t2)) d2=IntegerOfTerm(t2); else d2=FloatOfTerm(t2);
    t3 = MkFloatTerm(Yap_double_in_range(d1,d2));
  }
    return Yap_unify(ARG3, t3);
}



/**
   @pred signed_random(-_Integer_)

Unify the first argument with an   pseudo-random integer.
 */
static Int Random(USES_REGS1)
{
  return Yap_unify(ARG1, MkIntegerTerm(Yap_unsigned_integer_random()));
}


/** 
 * Initialize the default random number generator.
 *
 * uses the process's srand call.
 * 
 */
void
Yap_InitRandom (void)
{
  current_seed = (unsigned int) time (NULL);
#if HAVE_SRAND48
  srand48 (current_seed);
#elif HAVE_RANDOM
  srandom (current_seed);
#elif HAVE_SRAND
  srand (current_seed);
#endif
}

void
Yap_InitRandomPreds (void)
{
  Yap_InitCPred ("srandom", 1, Srandom, SafePredFlag);
  Yap_InitCPredInModule ("random", 1, Random, SafePredFlag, MkAtomTerm(Yap_LookupAtom("random")));
  Yap_InitCPred ("random", 1, Random, SafePredFlag);
  Yap_InitCPred ("random", 3, random3, SafePredFlag);
  Yap_InitCPred ("signed_random", 1, URandom, SafePredFlag);
#if HAVE_RANDOM
  Yap_InitCPred ("init_random_state", 3, p_init_random_state, SafePredFlag);
  Yap_InitCPred ("set_random_state", 2, p_set_random_state, SafePredFlag);
  Yap_InitCPred ("release_random_state", 1, p_release_random_state, SafePredFlag);
#endif
  init_rand_generator();
}

/// @}
