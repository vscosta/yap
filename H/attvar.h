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

#include <TermExt.h>

/**

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
    Term Done;            /* if unbound suspension active, if bound terminated */
    Term Future;           /* value the variable will take */
    Term Atts; /* actual data */
} attvar_record;

#define ATT_RECORD_ARITY  3

 #define MAX_EMPTY_WAKEUPS 16

/*
  attvar_entry is just a Prolog structure such that the first argument is
  a pointer to the next args
*/


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

extern void AddToQueue(attvar_record *attv USES_REGS);

#define TermVoidAtt TermFoundVar

#endif

