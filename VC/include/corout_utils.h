






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
* File:		corout_utils.h						 *
* Last rev:								 *
* mods:									 *
* comments:	Co-routining from within YAP				 *
*									 *
*************************************************************************/

STATIC_PROTO (Term SDeref, (Term));
STATIC_PROTO (Term SDerefa, (CELL *));
STATIC_PROTO (sus_tag * deref_susp_chain, (sus_tag *));

static inline Term
SDeref (Term a)
{
  while (IsVarTerm (a))
    {
      Term *b = (Term *) a;
      a = *b;
#if SBA
      if (a == (0))
	return (CELL) b;
#else
      if (a == ((Term) b))
	return a;
#endif
    }
  return (a);
}

static inline Term
SDerefa (CELL * b)
{
  Term a = *b;
  while (IsVarTerm (a))
    {
#if SBA
      if (a == (0))
	return (CELL) b;
#else
      if (a == ((Term) b))
	return a;
#endif
      b = (Term *) a;
      a = *b;
    }
  return (a);
}

static inline CELL *
SADerefa (CELL * b)
{
  Term a = *b;
  while (IsVarTerm (a))
    {
#if SBA
      if (a == (0))
	return b;
#else
      if (a == ((Term) b))
	return b;
#endif
      b = (Term *) a;
      a = *b;
    }
  return (b);
}



inline EXTERN Term SArgOfTerm (int i, Term t);

inline EXTERN Term
SArgOfTerm (int i, Term t)
{
  return (Term) (SDerefa (RepAppl (t) + (i)));
}



inline EXTERN Term SHeadOfTerm (Term);

inline EXTERN Term
SHeadOfTerm (Term t)
{
  return (Term) (SDerefa (RepPair (t)));
}



inline EXTERN Term STailOfTerm (Term);

inline EXTERN Term
STailOfTerm (Term t)
{
  return (Term) (SDerefa (RepPair (t) + 1));
}



static inline sus_tag *
deref_susp_chain (sus_tag * susp)
{
  /* we may have bound several suspension chains together. Follow the
     reference chain
   */
  while (IsVarTerm (susp->ActiveSus)
	 && susp->ActiveSus != (CELL) & susp->ActiveSus)
    susp =
      (sus_tag *) (susp->ActiveSus - (Int) (&(((sus_tag *) (0))->ActiveSus)));
  return (susp);
}
