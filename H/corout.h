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

typedef struct sus_record_struct {
  struct sus_record_struct *NR;		/* next record */
  Term  SG;				/* suspended goal */
#ifdef MULTI_ASSIGNMENT_VARIABLES
  struct sus_record_struct *NS;	        /* other suspended goals */
#endif
} sus_record;

typedef struct sus_tag_struct {
  Term	ActiveSus;		/* if unbound suspension active, if bound terminated   */
  CELL sus_id;
  sus_record *SG;		/* list of suspended goals */
} sus_tag;

#ifdef COROUTINING
/*********** tags for suspension variables */

#define AbsSuspendedVar(sustag_ptr)	AbsAppl(((CELL *)(sustag_ptr)))
#define RepSuspendedVar(val)		((sus_tag *)RepAppl(val))

#endif

