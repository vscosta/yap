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

/* first, the valid types */
typedef enum
{
  array_of_ints,
  array_of_chars,
  array_of_uchars,
  array_of_doubles,
  array_of_ptrs,
  array_of_atoms,
  array_of_dbrefs,
  array_of_nb_terms,
  array_of_terms
} static_array_types;

/* This should never be followed by GC */
typedef struct array_access_struct {
  Functor  array_access_func;		/* identifier of array access  */
  Term	ArrayT;				/* term that references the array */
  Term  indx;				/* index in array, for now
				   keep it as an integer! */
} array_access;

struct static_array_entry *
Yap_StaticVector( Atom Name, size_t size,  static_array_types props );

struct static_array_entry *
Yap_StaticArray(Atom na, size_t dim, static_array_types type, CODEADDR start_addr, struct static_array_entry *p);
