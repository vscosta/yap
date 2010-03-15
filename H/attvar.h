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

#ifdef COROUTINING

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

static inline Term
AbsAttVar(attvar_record *attvar_ptr) {
  return attvar_ptr->Done;
}

static inline attvar_record *
RepAttVar(Term *var_ptr) {
  return (attvar_record *)(var_ptr-1);
}

#endif



