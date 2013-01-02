/*
 * This file is part of the YAP Prolog
 *
 * User Defined Indexing was developed by:
 *   David Vaz <davidvaz@dcc.fc.up.pt>
 *   Vitor Santos Costa <vsc@dcc.fc.up.pt>
 *
 * UDI Indexing Interface:
 *
 *   Each new indexing mechanism should register it self by filling up a
 *   UdiControlBlock and calling Yap_UdiRegister(UdiControlBlock).
 *
 *   UdiControlBlock has the main declaration that triggers the
 *   indexing structure as well as the pointers to the needed functions
 *   called at the appropriate times.
 *
 *   For now each indexing structure only works with a single argument
 *   even when multiple arguments are indexed with the same struture.
 *
 *   TODO: think of alternative ways of support both cases, e.g. a rtree
 *   does not benefit from multiple rtree indexing, but a hash table do
 */

/* This is called upon udi mode spec call, and the purpose is to allow
 * the indexing struture to initialize itself.
 * Should return the need opaque struture to be used in future calls
 *
 * arg is used to track the specific call, on multiple indexing with the
 * same struture
 */
typedef void * (* Yap_UdiInit)
		(YAP_Term spec,
		 int arg,         /* argument regarding this call */
		 int   arity);

/* Upon each assert the struture insert method is called to perform
 * its work
 */
typedef void * (* Yap_UdiInsert)
		(void *control,   /* indexing structure opaque handle */
		 YAP_Term term,   /* asserted argument */
		 int arg,         /* argument regarding this call */
		 void *data);     /* value to return on search */

/* Callback for each value found in a search
 * if it returns FALSE the search should be immediately aborted
 */
typedef int (* Yap_UdiCallback)
		(void *key,      /* index key */
         void *data,     /* data */
         void *arg);     /* auxiliary data to callback */

/* Called upon search
 *
 * If there is any search to do with this structure should return >= 0
 * corresponding to the values found
 *
 * returns -1 if there is nothing to search with this indexing structure
 * e.g. a Variable as argument
 */
typedef int (* Yap_UdiSearch)
		(void * control,    /* indexing structure opaque handle */
		 int arg,           /* argument regarding this call */
		 Yap_UdiCallback f, /* callback on each found value */
		 void *args);       /* auxiliary data to callback */

/* Called upon abolish of the term
 * to allow for a clean destroy of the indexing structures
 */
typedef int (* Yap_UdiDestroy)
		(void * control);

/*
 * Main structure used in UDI
 */
typedef struct udi_control_block {
  YAP_Atom       decl; //atom that triggers this indexing structure
  Yap_UdiInit    init;
  Yap_UdiInsert  insert;
  Yap_UdiSearch  search;
  Yap_UdiDestroy destroy;
} * UdiControlBlock;

/* Register a new indexing structure */
void Yap_UdiRegister(UdiControlBlock);
