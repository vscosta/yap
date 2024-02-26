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

#ifndef ARRAYS_H
#define ARRAYS_H 1

#include "arrays.h"

/* second case is for static arrays */

typedef struct {
  Term tlive;
  Term tstore;
} live_term;

typedef union {
  Int *ints;
  char *chars;
  unsigned char *uchars;
  Float *floats;
  AtomEntry **ptrs;
  Term *atoms;
  Term *dbrefs;
    struct DB_TERM **terms;
  live_term *lterms;
} statarray_elements;

/* first, the valid types */
typedef enum static_array_type
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
  int base;
  } array_access;

typedef enum {
  STATIC_ARRAY = 1,
  DYNAMIC_ARRAY = 2,
  MMAP_ARRAY = 4,
  FIXED_ARRAY = 8
} array_type;



/*		array property entry structure				*/
/*		first case is for dynamic arrays */
typedef struct array_entry {
  Prop NextOfPE;      /* used to chain properties             */
  PropFlags KindOfPE; /* kind of property                     */
  Int ArrayEArity;    /* Arity of Array (positive)            */
  array_type TypeOfAE;
  size_t NDimsOfAE;
  ssize_t *DimsOfAE;
  int BaseOfAE;
#if defined(YAPOR) || defined(THREADS)
  rwlock_t ArRWLock; /* a read-write lock to protect the entry */
#if THREADS
  unsigned int owner_id;
#endif
#endif
  struct array_entry *NextAE;
 union {
   Term ValueOfDynamicVE; /* Pointer to the actual array          */
   struct {
     static_array_types ArrayType; /* Type of Array Elements.              */
     statarray_elements ValueOfStaticVE; /* Pointer to the Array itself  */
   };
 };   
} ArrayEntry;


extern struct array_entry *
Yap_StaticVector( Atom Name, size_t size,  static_array_types props );

extern ArrayEntry *Yap_StaticArray(Atom na, static_array_types type, size_t sz, size_t ndims, size_t *dims, CODEADDR start_addr, ArrayEntry *p);

#endif
