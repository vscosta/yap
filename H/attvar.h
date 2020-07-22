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
* File:		corout.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Co-routining from within YAP				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[]="%W% %G%";
#endif

#ifndef ATTVAR_H

#define ATTVAR_H 1

/*

Attributed variales are controlled by the attvar_record. This includes
three pieces of information:
       A pointer to the list of attributes;
       The value for the variable, if the variable was bound
       Whether we are done with this variable as an attributed variable
       An array of NUM_OF_ATTS attributes.

Each attribute contains;

 o a time stamp;
 o the current value ([] if unbound).

*/

/*
  attvar_entry is just a Prolog structure such that the first argument is
  a pointer to the next args
*/

typedef struct attvar_struct {
  Functor AttFunc;      /* functor for attvar */
  Term Done;		/* if unbound suspension active, if bound terminated */
  Term Value;           /* value the variable will take */
  Term Atts; /* actual data */
} attvar_record;

#define ATT_RECORD_ARITY  3

/*********** tags for suspension variables */


#define IsAttVar(pt) __IsAttVar((pt)PASS_REGS)

INLINE_ONLY int
__IsAttVar(CELL *pt USES_REGS);

INLINE_ONLY int __IsAttVar(CELL *pt USES_REGS) {
#ifdef YAP_H
    return (pt)[-1] == (CELL)FunctorAttVar && pt < HR;


#else
    return (pt)[-1] == (CELL)attvar_e;
#endif
}

INLINE_ONLY int GlobalIsAttVar(CELL *pt);

INLINE_ONLY int GlobalIsAttVar(CELL *pt) {
    return (pt)[-1] == (CELL)FunctorAttVar;
}


INLINE_ONLY bool IsAttachFunc(Functor);

INLINE_ONLY bool IsAttachFunc(Functor f) { return (Int)(FALSE); }

#define IsAttachedTerm(t) __IsAttachedTerm(t PASS_REGS)

INLINE_ONLY bool __IsAttachedTerm(Term USES_REGS);

INLINE_ONLY bool __IsAttachedTerm(Term t USES_REGS) {
    return (IsVarTerm(t) &&
            IsAttVar(VarOfTerm(t)));
}

INLINE_ONLY bool GlobalIsAttachedTerm(Term);

INLINE_ONLY bool GlobalIsAttachedTerm(Term t) {
    return (IsVarTerm(t) &&
            GlobalIsAttVar(VarOfTerm(t)));
}

#define SafeIsAttachedTerm(t) __SafeIsAttachedTerm((t)PASS_REGS)

INLINE_ONLY bool __SafeIsAttachedTerm(Term USES_REGS);

INLINE_ONLY bool __SafeIsAttachedTerm(Term t USES_REGS) {
    return IsVarTerm(t) && IsAttVar(VarOfTerm(t));
}

static inline Term
AbsAttVar(attvar_record *attvar_ptr) {
  return attvar_ptr->Done;
}

static inline attvar_record *
RepAttVar(Term *var_ptr) {
  return (attvar_record *)(var_ptr-1);
}

static inline void suspend_goal(Term tg USES_REGS) {
  if (LOCAL_DoNotWakeUp)
    return;
  /* follow the chain */
  Term WGs = Yap_ReadTimedVar(LOCAL_WokenGoals);
  if (IsVarTerm(WGs)||WGs==TermTrue) {
    Yap_UpdateTimedVar(LOCAL_WokenGoals,tg);
  } else {
    if (!IsApplTerm(WGs) || FunctorOfTerm(WGs)!=FunctorComma) {
      Term t[2];
      t[0] = tg;
      t[1]= WGs;
      WGs = Yap_MkApplTerm(FunctorComma, 2, t);
      Yap_UpdateTimedVar(LOCAL_WokenGoals,WGs);
    } else {
      *HR++ = (CELL)FunctorComma;
      *HR++ = tg;
      Term v = (CELL)HR;
    RESET_VARIABLE(HR):
      HR++;
      Term p = Yap_ReadTimedVar(LOCAL_WokenTailGoals);
      Yap_unify(p,AbsAppl(HR-3));
      Yap_UpdateTimedVar(LOCAL_WokenTailGoals,WGs);		}
    }
#endif
}


