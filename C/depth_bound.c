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

#include "Yap.h"

#ifdef DEPTH_LIMIT

#include "Yatom.h"

STD_PROTO(static Int p_get_depth_limit, (void));
STD_PROTO(static Int p_set_depth_limit, (void));

static Int p_get_depth_limit(void)
{
  return(Yap_unify_constant(ARG1, MkIntTerm(IntOfTerm(DEPTH/2))));
}

static Int p_set_depth_limit(void)
{
  Term d = Deref(ARG1);

  if (IsVarTerm(d)) {
    Yap_Error(INSTANTIATION_ERROR, d, "set-depth_limit");
    return(FALSE);
  } else if (!IsIntegerTerm(d)) {
    Yap_Error(TYPE_ERROR_INTEGER, d, "set-depth_limit");
    return(FALSE);
  }
  d = MkIntTerm(IntegerOfTerm(d)*2);

  YENV[E_DEPTH] = d;
  DEPTH = d;

  return(TRUE);
}

static Int p_set_depth_limit_for_next_call(void)
{
  Term d = Deref(ARG1);

  if (IsVarTerm(d)) {
    Yap_Error(INSTANTIATION_ERROR, d, "set-depth_limit");
    return(FALSE);
  } else if (!IsIntegerTerm(d)) {
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
  Yap_InitCPred("$set_depth_limit", 1, p_set_depth_limit, HiddenPredFlag);
  Yap_InitCPred("$set_depth_limit_for_next_call", 1, p_set_depth_limit_for_next_call, HiddenPredFlag);
}

#endif
