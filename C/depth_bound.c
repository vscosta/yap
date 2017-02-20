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
* File:		it_deep.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Support for Iterative Deepening                          *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif /* SCCS */

#include <math.h>

#include "Yap.h"
#include "Yatom.h"
#include "YapEval.h"

#ifdef DEPTH_LIMIT

#include "Yatom.h"

static Int p_get_depth_limit( USES_REGS1 );
static Int p_set_depth_limit( USES_REGS1 );

static Int p_get_depth_limit( USES_REGS1 )
{
  Int d = IntOfTerm(DEPTH);
  if (d % 2 == 1) 
    return(Yap_unify(ARG1, MkFloatTerm(INFINITY)));
  return(Yap_unify_constant(ARG1, MkIntTerm(d/2)));
}

static Int p_set_depth_limit( USES_REGS1 )
{
  Term d = Deref(ARG1);

  if (IsVarTerm(d)) {
    Yap_Error(INSTANTIATION_ERROR, d, "set-depth_limit");
    return(FALSE);
  } else if (!IsIntegerTerm(d)) {
    if (IsFloatTerm(d) && isinf(FloatOfTerm(d))) {
      d = RESET_DEPTH();
    } else {
      Yap_Error(TYPE_ERROR_INTEGER, d, "set-depth_limit");
      return(FALSE);
    }
  }
  d = MkIntTerm(IntegerOfTerm(d)*2);

  YENV[E_DEPTH] = d;
  DEPTH = d;

  return(TRUE);
}

static Int p_set_depth_limit_for_next_call( USES_REGS1 )
{
  Term d = Deref(ARG1);

  if (IsVarTerm(d)) {
    Yap_Error(INSTANTIATION_ERROR, d, "set-depth_limit");
    return(FALSE);
  } else if (!IsIntegerTerm(d)) {
    if (IsFloatTerm(d) && isinf(FloatOfTerm(d))) {
      DEPTH = RESET_DEPTH();
      return TRUE;
    }
    Yap_Error(TYPE_ERROR_INTEGER, d, "set-depth_limit");
    return(FALSE);
  }
  d = MkIntTerm(IntegerOfTerm(d)*2);

  DEPTH = d;

  return(TRUE);
}

void Yap_InitItDeepenPreds(void)
{
  Yap_InitCPred("get_depth_limit", 1, p_get_depth_limit, SafePredFlag);
  Yap_InitCPred("$set_depth_limit", 1, p_set_depth_limit, 0);
  Yap_InitCPred("$set_depth_limit_for_next_call", 1, p_set_depth_limit_for_next_call, 0);
}

#endif
