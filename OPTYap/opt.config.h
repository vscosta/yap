/* ---------------------------------- **
**      Configuration Parameters      **
** ---------------------------------- */

#define MAX_LENGTH_ANSWER 500
#define MAX_DEPTH 1000
#define MAX_BEST_TIMES 21
#define MAX_TABLE_VARS 1000
#define TABLE_LOCK_BUCKETS 512
#define TG_ANSWER_SLOTS 20


#define STATISTICS
/*
#define YAPOR_ERRORS
#define TABLING_ERRORS
*/


/* x86_locks.h */
#define UNIQUE_WHILE_LOCK


/* amiops.h */
#define BFZ_TRAIL_SCHEME 1
/*#define BBREG_TRAIL_SCHEME*/


#define TABLING_INNER_CUTS


#define TIMESTAMP_CHECK


/*#define TABLE_LOCK_AT_ENTRY_LEVEL*/
/*#define TABLE_LOCK_AT_NODE_LEVEL*/
#define TABLE_LOCK_AT_WRITE_LEVEL


/* tab.tries.c */
/*#define ALLOC_BEFORE_CHECK*/

/* use heap instead of special areas for memory allocation */
#define USE_HEAP 1



/* -------------------------- **
**      Parameter Checks      **
** -------------------------- */

#ifndef YAPOR
#undef YAPOR_ERRORS
#endif
#ifndef TABLING
#undef TABLING_ERRORS
#endif
#if defined(YAPOR_ERRORS) && defined(TABLING_ERRORS)
#define OPTYAP_ERRORS
#endif


#ifndef YAPOR
#undef UNIQUE_WHILE_LOCK
#endif


#ifdef TABLING
#if !defined(BFZ_TRAIL_SCHEME) && !defined(BBREG_TRAIL_SCHEME)
#error Define a trail scheme
#endif
#if defined(BFZ_TRAIL_SCHEME) && defined(BBREG_TRAIL_SCHEME)
#error Do not define multiple trail schemes
#endif
#else
#undef BFZ_TRAIL_SCHEME
#undef BBREG_TRAIL_SCHEME
#endif


#if !defined(TABLING) || !defined(YAPOR)
#undef TABLING_INNER_CUTS
#endif


#if !defined(YAPOR) || !defined(TABLING)
#undef TIMESTAMP_CHECK
#endif


/* ------------------------------------------------------------------ **
**                                                                    **
** There are three lock schemes to access the table space.            **
**                                                                    **
** The TABLE_LOCK_AT_ENTRY_LEVEL scheme locks the access to the table **
** space in the entry data structure. It restricts the number of lock **
** operations needed to go through the table data structures.         **
**                                                                    **
** The TABLE_LOCK_AT_NODE_LEVEL scheme locks each data structure      **
** before accessing it. It decreases concurrrency for workers         **
** accessing commom parts of the table space.                         **
**                                                                    **
** The TABLE_LOCK_AT_WRITE_LEVEL scheme is an hibrid scheme, it only  **
** locks a table data structure when it is going to update it.        **
**                                                                    **
** The TABLE_LOCK_AT_WRITE_LEVEL is the default scheme.               **
**                                                                    **
** ------------------------------------------------------------------ */

#if defined(YAPOR) && defined(TABLING)
#if !defined(TABLE_LOCK_AT_ENTRY_LEVEL) && !defined(TABLE_LOCK_AT_NODE_LEVEL) && !defined(TABLE_LOCK_AT_WRITE_LEVEL)
#error Define a table lock scheme
#endif
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
#if defined(TABLE_LOCK_AT_NODE_LEVEL) || defined(TABLE_LOCK_AT_WRITE_LEVEL)
#error Do not define multiple table lock schemes
#endif
#endif
#if defined(TABLE_LOCK_AT_NODE_LEVEL) && defined(TABLE_LOCK_AT_WRITE_LEVEL)
#error Do not define multiple table lock schemes
#endif
#else
#undef TABLE_LOCK_AT_ENTRY_LEVEL
#undef TABLE_LOCK_AT_NODE_LEVEL
#undef TABLE_LOCK_AT_WRITE_LEVEL
#endif /* YAPOR && TABLING */


#ifndef TABLE_LOCK_AT_WRITE_LEVEL
#undef ALLOC_BEFORE_CHECK
#endif
