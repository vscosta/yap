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
* comments:	Support to YAP arrays					 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[]="%W% %G%";
#endif

/* This should never be followed by GC */
typedef struct array_access_struct {
  Functor  array_access_func;		/* identifier of array access  */
  Term	ArrayT;				/* term that references the array */
  Term  indx;				/* index in array, for now
				   keep it as an integer! */
} array_access;



