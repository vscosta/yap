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

typedef struct attvar_struct {
  Term	Done;		/* if unbound suspension active, if bound terminated */
  CELL sus_id;
  Term NS;			 /* other attributed variables */
  Term Value;                    /* value the variable will take */
#ifdef __GNUC__
  /* GNUCC understands empty arrays */
  Term  Atts[0];
#else
  Term  Atts[2];                 /* size of an entry */
#endif
} attvar_record;

/*********** tags for suspension variables */

#define AbsAttVar(attvar_ptr)	AbsAppl(((CELL *)(attvar_ptr)))
#define RepAttVar(val)		((attvar_record *)RepAppl(val))

static inline attvar_record *
DelayTop(void) {
  return (attvar_record *)((CELL *)Yap_GlobalBase+IntegerOfTerm(Yap_ReadTimedVar(DelayedVars)));
}

static inline void
SetDelayTop(CELL *new_top) {
  Yap_UpdateTimedVar(DelayedVars, MkIntegerTerm((CELL)(new_top-(CELL *)Yap_GlobalBase)));
}

#endif



