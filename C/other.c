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
#include "Heap.h"

/* exile _YAP_standard_regs here, otherwise WIN32 linkers may complain */
REGSTORE _YAP_standard_regs;

#if PUSH_REGS

REGSTORE *_YAP_regp;

#else

REGSTORE _YAP_REGS;

#endif

Term 
_YAP_MkNewPairTerm(void)
{
  register CELL  *p = H;

  RESET_VARIABLE(H);
  RESET_VARIABLE(H+1);
  H+=2;
  return (AbsPair(p));
}

Term
_YAP_MkApplTerm(Functor f, unsigned int n, register Term *a)	
     /* build compound term with functor f and n
      * args a */
{
  CELL           *t = H;

  if (n == 0)
    return (MkAtomTerm(NameOfFunctor(f)));
  if (f == FunctorList)
    return (MkPairTerm(a[0], a[1]));
  *H++ = (CELL) f;
  while (n--)
    *H++ = (CELL) * a++;
  return (AbsAppl(t));
}

Term 
_YAP_MkNewApplTerm(Functor f, unsigned int n)	
     /* build compound term with functor f and n
      * args a */
{
  CELL           *t = H;

  if (n == 0)
    return (MkAtomTerm(NameOfFunctor(f)));
  if (f == FunctorList) {
    RESET_VARIABLE(H);
    RESET_VARIABLE(H+1);
    H+=2;
    return (AbsPair(t));
  }
  *H++ = (CELL) f;
  while (n--) {
    RESET_VARIABLE(H);
    H++;
  }
  return (AbsAppl(t));
}
