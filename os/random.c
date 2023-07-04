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

unsigned int current_seed;

extern int rand(void);

/**
   @groupdef YAPRandom Interface to the OS number generators.
   @ingroup InputOutput
`   
   @{

Older versios of YAP supported random number access through the expression `X is random`. This newer code supports:

 - initialization;
 - random numbers as integers (signed or absolute) or as floating points.

 By default YAP uses rand48, a pseudo-random number generator that uses linear congruence and 48-bit integers.
   

*/


double
Yap_random (void)
{
#if HAVE_DRAND48
  return drand48();
#elif HAVE_RANDOM
  /*  extern long random (); */
  return (((double) random ()) / 0x7fffffffL /* 2**31-1 */);
#elif HAVE_RAND
  return (((double) (rand ()) / RAND_MAX));
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "random not available in this configuration");
  return (0.0);
#endif
}


UInt
Yap_unsigned_integer_random (void)
{
#if HAVE_DRAND48
  return lrand48();
#elif HAVE_RANDOM
  /*  extern long random (); */
  return random();
#elif HAVE_RAND
  return rand () / RAND_MAX;
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "random not available in this configuration");
  return (0.0);
#endif
}

Int
Yap_signed_integer_random (void)
{
#if HAVE_DRAND48
  return lrand48();
#elif HAVE_RANDOM
  /*  extern long random (); */
  return random()-(1<<30);
#elif HAVE_RAND
  return rand () - RAND_MAX/2;
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "random not available in this configuration");
  return (0.0);
#endif
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
#elif HAVE_SRANDOM
  srandom (current_seed);
#elif HAVE_SRAND
  srand (current_seed);
#endif
}

void
Yap_InitRandomPreds (void)
{
  Yap_InitCPred ("srandom", 1, Srandom, SafePredFlag);
  Yap_InitCPred ("random", 1, Random, SafePredFlag);
  Yap_InitCPred ("signed_random", 1, URandom, SafePredFlag);
#if HAVE_RANDOM
  Yap_InitCPred ("init_random_state", 3, p_init_random_state, SafePredFlag);
  Yap_InitCPred ("set_random_state", 2, p_set_random_state, SafePredFlag);
  Yap_InitCPred ("release_random_state", 1, p_release_random_state, SafePredFlag);
#endif
}

/// @}
