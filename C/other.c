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

Term 
MkPairTerm(Term head, Term tail)
{
  register CELL  *p = H;

  *H++ = (CELL) (head);
  *H++ = (CELL) (tail);
  return (AbsPair(p));
}


Term 
MkApplTerm(Functor f, unsigned int n, register Term *a)	
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
