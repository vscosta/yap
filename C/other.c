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
* File:		other.c							 *
* Last rev:	Dec/90							 *
* mods:									 *
* comments:	extra routines 						 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif


#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#if HAVE_STRING_H
#include <string.h>
#endif

/* exile Yap_standard_regs here, otherwise WIN32 linkers may complain */
REGSTORE Yap_standard_regs;

#if PUSH_REGS

#ifdef THREADS
/* PushRegs always on */

pthread_key_t Yap_yaamregs_key;

#else

REGSTORE *Yap_regp;

#endif

#else /* !PUSH_REGS */

REGSTORE Yap_REGS;

#endif

Term 
Yap_MkNewPairTerm(void)
{
  CACHE_REGS
  register CELL  *p = HR;

  RESET_VARIABLE(HR);
  RESET_VARIABLE(HR+1);
  HR+=2;
  return (AbsPair(p));
}

/**  compound term constructor, builds a compound term with functor f and n
 * args.
 *
 *
 * Room for the new term is allocated from the heap: the functor and arguments are copied there.
 *
*/
Term
Yap_MkApplTerm(Functor f, arity_t n,  const Term *a)
 {
  CACHE_REGS
  CELL           *t = HR;

  if (n == 0)
    return (MkAtomTerm(NameOfFunctor(f)));
  if (f == FunctorList)
    return MkPairTerm(a[0], a[1]);
  *HR++ = (CELL) f;
  while (n--)
    *HR++ =  * a++;
  return (AbsAppl(t));
}

Term 

Yap_MkNewApplTerm(Functor f, arity_t n)
     /* build compound term with functor f and n
      * args a */
{
  CACHE_REGS
  CELL           *t = HR;

  if (n == 0)
    return (MkAtomTerm(NameOfFunctor(f)));
  if (f == FunctorList) {
    RESET_VARIABLE(HR);
    RESET_VARIABLE(HR+1);
    HR+=2;
    return (AbsPair(t));
  }
  *HR++ = (CELL) f;
  while (n--) {
    RESET_VARIABLE(HR);
    HR++;
  }
  return (AbsAppl(t));
}


Term
Yap_Globalise(Term t)
{
  CACHE_REGS
  CELL *vt;
  Term tn;

  if (!IsVarTerm(t))
    return t;
  vt = VarOfTerm(t);
  if (vt <= HR && vt > H0)
    return t;
  tn = MkVarTerm();
  Yap_unify(t, tn);
  return tn;
}
