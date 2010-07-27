/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/



/************************************************************************
**                   General Configuration Parameters                  **
************************************************************************/

/**********************************************************
**      memory alloc scheme (mandatory, define one)      **
**********************************************************/
#ifdef USE_SYSTEM_MALLOC
#define MALLOC_MEMORY_ALLOC_SCHEME 1
#else
#define YAP_MEMORY_ALLOC_SCHEME 1
#endif
/* #define SHM_MEMORY_ALLOC_SCHEME 1 */



/************************************************************************
**                   TABLING Configuration Parameters                  **
************************************************************************/

/****************************
**      default sizes      **
****************************/
#define MAX_TABLE_VARS 1000

/**********************************************************
**      trail freeze scheme (mandatory, define one)      **
**********************************************************/
#define BFZ_TRAIL_SCHEME 1
/* #define BBREG_TRAIL_SCHEME 1 */

/****************************************************
**      support early completion ? (optional)      **
****************************************************/
#define TABLING_EARLY_COMPLETION 1

/******************************************************
**      support trie compact pairs ? (optional)      **
******************************************************/
#define TRIE_COMPACT_PAIRS 1

/************************************************************
**      support global trie for subterms ? (optional)      **
************************************************************/
/* #define GLOBAL_TRIE_FOR_SUBTERMS 1 */

/******************************************************
**      support incomplete tabling ? (optional)      **
******************************************************/
/* #define INCOMPLETE_TABLING 1 */

/******************************************************
**      limit the table space size ? (optional)      **
******************************************************/
/* #define LIMIT_TABLING 1 */

/*********************************************************
**      support deterministic tabling ? (optional)      **
*********************************************************/
/* #define DETERMINISTIC_TABLING 1 */

/*************************************************
**      enable error checking ? (optional)      **
*************************************************/
/* #define DEBUG_TABLING 1 */



/************************************************************************
**                    YAPOR Configuration Parameters                   **
************************************************************************/

/****************************
**      default sizes      **
****************************/
#define MAX_LENGTH_ANSWER  1000
#define MAX_BRANCH_DEPTH   1000
#define MAX_BEST_TIMES     21

/************************************************************
**      memory mapping scheme (mandatory, define one)      **
************************************************************/
#define MMAP_MEMORY_MAPPING_SCHEME 1
/* #define SHM_MEMORY_MAPPING_SCHEME  1 */

/*************************************************
**      enable error checking ? (optional)      **
*************************************************/
/* #define DEBUG_YAPOR 1 */



/************************************************************************
**                   OPTYAP Configuration Parameters                   **
************************************************************************/

/****************************
**      default sizes      **
****************************/
#define TABLE_LOCK_BUCKETS 512
#define TG_ANSWER_SLOTS    20

/***********************************************************
**      tries locking scheme (mandatory, define one)      **
************************************************************
** The TABLE_LOCK_AT_ENTRY_LEVEL scheme locks the access  **
** to the table space in the entry data structure. It     **
** restricts the number of lock operations needed to go   **
** through the table data structures.                     **
**                                                        **
** The TABLE_LOCK_AT_NODE_LEVEL scheme locks each data    **
** structure before accessing it. It decreases            **
** concurrrency for workers accessing commom parts of the **
** table space.                                           **
**                                                        **
** The TABLE_LOCK_AT_WRITE_LEVEL scheme is an hibrid      **
** scheme, it only locks a table data structure when it   **
** is going to update it. You can use ALLOC_BEFORE_CHECK  **
** with this scheme to allocate a node before checking    **
** if it will be necessary.                               **
***********************************************************/
/* #define TABLE_LOCK_AT_ENTRY_LEVEL 1 */
/* #define TABLE_LOCK_AT_NODE_LEVEL  1 */
#define TABLE_LOCK_AT_WRITE_LEVEL 1
/* #define ALLOC_BEFORE_CHECK        1 */

/**********************************************
**      support inner cuts ? (optional)      **
**********************************************/
#define TABLING_INNER_CUTS 1

/*********************************************************
**      use timestamps for suspension ? (optional)      **
*********************************************************/
#define TIMESTAMP_CHECK 1



/************************************************************************
**                           Parameter Checks                          **
************************************************************************/

#if !defined(SHM_MEMORY_ALLOC_SCHEME) && !defined(MALLOC_MEMORY_ALLOC_SCHEME) && !defined(YAP_MEMORY_ALLOC_SCHEME)
#error Define a memory alloc scheme
#endif /* !SHM_MEMORY_ALLOC_SCHEME && !MALLOC_MEMORY_ALLOC_SCHEME && !YAP_MEMORY_ALLOC_SCHEME */
#if defined(SHM_MEMORY_ALLOC_SCHEME)
#if defined(MALLOC_MEMORY_ALLOC_SCHEME) || defined(YAP_MEMORY_ALLOC_SCHEME)
#error Do not define multiple memory alloc schemes
#endif /* MALLOC_MEMORY_ALLOC_SCHEME || YAP_MEMORY_ALLOC_SCHEME */
#endif /* SHM_MEMORY_ALLOC_SCHEME */
#if defined(MALLOC_MEMORY_ALLOC_SCHEME) && defined(YAP_MEMORY_ALLOC_SCHEME)
#error Do not define multiple memory alloc schemes
#endif /* MALLOC_MEMORY_ALLOC_SCHEME && YAP_MEMORY_ALLOC_SCHEME */
#if defined(YAPOR) && defined(MALLOC_MEMORY_ALLOC_SCHEME)
#error YAPOR is incompatible with MALLOC_MEMORY_ALLOC_SCHEME 
#endif /* YAPOR && TABLING && (MALLOC_MEMORY_ALLOC_SCHEME || YAP_MEMORY_ALLOC_SCHEME) */

#ifdef YAPOR
#ifdef i386 /* For i386 machines we use shared memory segments */
#undef MMAP_MEMORY_MAPPING_SCHEME
#define SHM_MEMORY_MAPPING_SCHEME
#endif /* i386 */
#if !defined(MMAP_MEMORY_MAPPING_SCHEME) && !defined(SHM_MEMORY_MAPPING_SCHEME)
#error Define a memory mapping scheme
#endif /* !MMAP_MEMORY_MAPPING_SCHEME && !SHM_MEMORY_MAPPING_SCHEME */
#if defined(MMAP_MEMORY_MAPPING_SCHEME) && defined(SHM_MEMORY_MAPPING_SCHEME)
#error Do not define multiple memory mapping schemes
#endif /* MMAP_MEMORY_MAPPING_SCHEME && SHM_MEMORY_MAPPING_SCHEME */
#undef LIMIT_TABLING
#endif /* YAPOR */

#ifdef TABLING
#if !defined(BFZ_TRAIL_SCHEME) && !defined(BBREG_TRAIL_SCHEME)
#error Define a trail scheme
#endif /* !BFZ_TRAIL_SCHEME && !BBREG_TRAIL_SCHEME */
#if defined(BFZ_TRAIL_SCHEME) && defined(BBREG_TRAIL_SCHEME)
#error Do not define multiple trail schemes
#endif /* BFZ_TRAIL_SCHEME && BBREG_TRAIL_SCHEME */
#endif /* TABLING */

#if defined(YAPOR) && defined(TABLING)
#if !defined(TABLE_LOCK_AT_ENTRY_LEVEL) && !defined(TABLE_LOCK_AT_NODE_LEVEL) && !defined(TABLE_LOCK_AT_WRITE_LEVEL)
#error Define a table lock scheme
#endif /* !TABLE_LOCK_AT_ENTRY_LEVEL && !TABLE_LOCK_AT_NODE_LEVEL && !TABLE_LOCK_AT_WRITE_LEVEL */
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
#if defined(TABLE_LOCK_AT_NODE_LEVEL) || defined(TABLE_LOCK_AT_WRITE_LEVEL)
#error Do not define multiple table lock schemes
#endif /* TABLE_LOCK_AT_NODE_LEVEL || TABLE_LOCK_AT_WRITE_LEVEL */
#endif /* TABLE_LOCK_AT_ENTRY_LEVEL */
#if defined(TABLE_LOCK_AT_NODE_LEVEL) && defined(TABLE_LOCK_AT_WRITE_LEVEL)
#error Do not define multiple table lock schemes
#endif /* TABLE_LOCK_AT_NODE_LEVEL || TABLE_LOCK_AT_WRITE_LEVEL */
#ifndef TABLE_LOCK_AT_WRITE_LEVEL
#undef ALLOC_BEFORE_CHECK
#endif /* !TABLE_LOCK_AT_WRITE_LEVEL */
#else
#undef TABLE_LOCK_AT_ENTRY_LEVEL
#undef TABLE_LOCK_AT_NODE_LEVEL
#undef TABLE_LOCK_AT_WRITE_LEVEL
#undef ALLOC_BEFORE_CHECK
#endif /* YAPOR && TABLING */

#if !defined(TABLING) || !defined(YAPOR)
#undef TABLING_INNER_CUTS
#undef TIMESTAMP_CHECK
#endif /* !TABLING || !YAPOR */

#ifndef YAPOR
#undef DEBUG_YAPOR
#endif /* !YAPOR */

#ifndef TABLING
#undef BFZ_TRAIL_SCHEME
#undef BBREG_TRAIL_SCHEME
#undef TABLING_EARLY_COMPLETION
#undef TRIE_COMPACT_PAIRS
#undef GLOBAL_TRIE_FOR_SUBTERMS
#undef DETERMINISTIC_TABLING
#undef LIMIT_TABLING
#undef INCOMPLETE_TABLING
#undef DEBUG_TABLING
#endif /* !TABLING */

#ifndef SHM_MEMORY_ALLOC_SCHEME
#undef LIMIT_TABLING
#endif /* !SHM_MEMORY_ALLOC_SCHEME */

#if defined(DEBUG_YAPOR) && defined(DEBUG_TABLING)
#define DEBUG_OPTYAP
#endif /* DEBUG_YAPOR && DEBUG_TABLING */
