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
  register CELL  *p = H;

  RESET_VARIABLE(H);
  RESET_VARIABLE(H+1);
  H+=2;
  return (AbsPair(p));
}

Term
Yap_MkApplTerm(Functor f, unsigned int n, register Term *a)	
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
Yap_MkNewApplTerm(Functor f, unsigned int n)	
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


Term
Yap_Globalise(Term t)
{
  CELL *vt;
  Term tn;

  if (!IsVarTerm(t))
    return t;
  vt = VarOfTerm(t);
  if (vt <= H && vt > H0)
    return t;
  tn = MkVarTerm();
  Yap_unify(t, tn);
  return tn;
}
