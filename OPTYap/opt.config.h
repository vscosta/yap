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



/****************************************************************
**                   Configuration Parameters                  **
****************************************************************/

/****************************
**      default sizes      **
****************************/
#define MAX_TABLE_VARS     1000
#define TRIE_LOCK_BUCKETS  512
#define THREADS_DIRECT_BUCKETS    32
#define THREADS_INDIRECT_BUCKETS  ((MAX_THREADS - THREADS_DIRECT_BUCKETS) / THREADS_DIRECT_BUCKETS)  /* (1024 - 32) / 32 = 31 */
#define THREADS_NUM_BUCKETS       (THREADS_DIRECT_BUCKETS + THREADS_INDIRECT_BUCKETS)
#define TG_ANSWER_SLOTS    20
#define MAX_BRANCH_DEPTH   1000

/**********************************************************************
**      memory mapping scheme for YapOr (mandatory, define one)      **
**********************************************************************/
#define MMAP_MEMORY_MAPPING_SCHEME 1
/* #define SHM_MEMORY_MAPPING_SCHEME 1 */

/****************************************************************
**      use shared pages memory alloc scheme ? (optional)      **
****************************************************************/
/* #define USE_PAGES_MALLOC 1 */

/**********************************************************************
**      trail freeze scheme for tabling (mandatory, define one)      **
**********************************************************************/
#define BFZ_TRAIL_SCHEME 1
/* #define BBREG_TRAIL_SCHEME 1 */

/************************************************************************
**      multithreading design for tabling (mandatory, define one)      **
************************************************************************/
#define THREADS_NO_SHARING 1
/* #define THREADS_SUBGOAL_SHARING 1 */
/* #define THREADS_FULL_SHARING 1 */
/* #define THREADS_CONSUMER_SHARING 1 */

/*************************************************************************
**      tries locking scheme (mandatory, define one per trie type)      **
**************************************************************************
** The (TRIE_TYPE)_LOCK_AT_ENTRY_LEVEL scheme locks the access to the   **
** table space in the entry data structure. It restricts the number of  **
** lock operations needed to go through the table data structures.      **
**                                                                      **
** The (TRIE_TYPE)_LOCK_AT_NODE_LEVEL scheme locks each data structure  **
** before accessing it. It decreases concurrrency for workers accessing **
** commom parts of the table space.                                     **
**                                                                      **
** The (TRIE_TYPE)_LOCK_AT_WRITE_LEVEL scheme is an hibrid scheme, it   **
** only locks a table data structure when it is going to update it. You **
** can use (TRIE_TYPE)_ALLOC_BEFORE_CHECK with this scheme to allocate  **
** a node before checking if it will be necessary.                      **
*************************************************************************/
/* #define SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL 1 */
#define SUBGOAL_TRIE_LOCK_AT_NODE_LEVEL  1
/* #define SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL 1 */
/* #define SUBGOAL_TRIE_ALLOC_BEFORE_CHECK  1 */

/* #define ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL 1 */
#define ANSWER_TRIE_LOCK_AT_NODE_LEVEL  1
/* #define ANSWER_TRIE_LOCK_AT_WRITE_LEVEL 1 */
/* #define ANSWER_TRIE_ALLOC_BEFORE_CHECK  1 */

#define GLOBAL_TRIE_LOCK_AT_NODE_LEVEL  1
/* #define GLOBAL_TRIE_LOCK_AT_WRITE_LEVEL 1 */
/* #define GLOBAL_TRIE_ALLOC_BEFORE_CHECK  1 */

/*******************************************************************
**      tries locking data structure (mandatory, define one)      **
********************************************************************
** Data structure to be used for locking the trie when using the  **
** (TRIE_TYPE)_LOCK_AT_[NODE|WRITE]_LEVEL schemes                 **
*******************************************************************/
#define TRIE_LOCK_USING_NODE_FIELD   1
/* #define TRIE_LOCK_USING_GLOBAL_ARRAY 1 */

/*********************************************************
**      support mode directed tabling ? (optional)      **
*********************************************************/
#define MODE_DIRECTED_TABLING 1

/****************************************************************
**      support early completion for tabling ? (optional)      **
*****************************************************************/
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

/******************************************************************
**      support tabling inner cuts with OPTYap ? (optional)      **
******************************************************************/
#define TABLING_INNER_CUTS 1

/*********************************************************************
**      use timestamps for suspension with OPTYap ? (optional)      **
*********************************************************************/
#define TIMESTAMP_CHECK 1

/*************************************************
**      enable error checking ? (optional)      **
*************************************************/
/* #define DEBUG_TABLING 1 */
/* #define DEBUG_YAPOR 1 */

/**************************************************
**      enable output checking ? (optional)      **
**************************************************/
/* #define OUTPUT_THREADS_TABLING 1 */

/*********************************************************
**      support rational terms ? (optional)      **
*********************************************************/
#define TRIE_RATIONAL_TERMS 1




/************************************************************************
**                           Parameter Checks                          **
************************************************************************/

#ifdef YAPOR
#ifdef i386 /* For i386 machines we use shared memory segments */
#undef MMAP_MEMORY_MAPPING_SCHEME
#define SHM_MEMORY_MAPPING_SCHEME
#endif
#if !defined(MMAP_MEMORY_MAPPING_SCHEME) && !defined(SHM_MEMORY_MAPPING_SCHEME)
#error Define a memory mapping scheme
#endif
#if defined(MMAP_MEMORY_MAPPING_SCHEME) && defined(SHM_MEMORY_MAPPING_SCHEME)
#error Do not define multiple memory mapping schemes
#endif
#else /* ! YAPOR */
#undef MMAP_MEMORY_MAPPING_SCHEME
#undef SHM_MEMORY_MAPPING_SCHEME
#undef DEBUG_YAPOR
#endif /* YAPOR */

#ifdef TABLING
#if !defined(BFZ_TRAIL_SCHEME) && !defined(BBREG_TRAIL_SCHEME)
#error Define a trail scheme
#endif
#if defined(BFZ_TRAIL_SCHEME) && defined(BBREG_TRAIL_SCHEME)
#error Do not define multiple trail schemes
#endif
#else /* ! TABLING */
#undef BFZ_TRAIL_SCHEME
#undef BBREG_TRAIL_SCHEME
#undef MODE_DIRECTED_TABLING
#undef TABLING_EARLY_COMPLETION
#undef TRIE_COMPACT_PAIRS
#undef GLOBAL_TRIE_FOR_SUBTERMS
#undef INCOMPLETE_TABLING
#undef LIMIT_TABLING
#undef DETERMINISTIC_TABLING
#undef DEBUG_TABLING
#endif /* TABLING */

#if defined(TABLING) && (defined(YAPOR) || defined(THREADS))
/* SUBGOAL_TRIE_LOCK_LEVEL */
#if !defined(SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL) && !defined(SUBGOAL_TRIE_LOCK_AT_NODE_LEVEL) && !defined(SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL)
#error Define a subgoal trie lock scheme
#endif
#if defined(SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL) && defined(SUBGOAL_TRIE_LOCK_AT_NODE_LEVEL)
#error Do not define multiple subgoal trie lock schemes
#endif
#if defined(SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL) && defined(SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL)
#error Do not define multiple subgoal trie lock schemes
#endif
#if defined(SUBGOAL_TRIE_LOCK_AT_NODE_LEVEL) && defined(SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL)
#error Do not define multiple subgoal trie lock schemes
#endif
#ifndef SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL
#undef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
#endif 
/* ANSWER_TRIE_LOCK_LEVEL */
#if !defined(ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL) && !defined(ANSWER_TRIE_LOCK_AT_NODE_LEVEL) && !defined(ANSWER_TRIE_LOCK_AT_WRITE_LEVEL)
#error Define a answer trie lock scheme
#endif
#if defined(ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL) && defined(ANSWER_TRIE_LOCK_AT_NODE_LEVEL)
#error Do not define multiple answer trie lock schemes
#endif
#if defined(ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL) && defined(ANSWER_TRIE_LOCK_AT_WRITE_LEVEL)
#error Do not define multiple answer trie lock schemes
#endif
#if defined(ANSWER_TRIE_LOCK_AT_NODE_LEVEL) && defined(ANSWER_TRIE_LOCK_AT_WRITE_LEVEL)
#error Do not define multiple answer trie lock schemes
#endif
#ifndef ANSWER_TRIE_LOCK_AT_WRITE_LEVEL
#undef ANSWER_TRIE_ALLOC_BEFORE_CHECK
#endif 
/* GLOBAL_TRIE_LOCK_LEVEL */
#if !defined(GLOBAL_TRIE_LOCK_AT_NODE_LEVEL) && !defined(GLOBAL_TRIE_LOCK_AT_WRITE_LEVEL)
#error Define a global trie lock scheme
#endif
#if defined(GLOBAL_TRIE_LOCK_AT_NODE_LEVEL) && defined(GLOBAL_TRIE_LOCK_AT_WRITE_LEVEL)
#error Do not define multiple global trie lock schemes
#endif
#ifndef GLOBAL_TRIE_LOCK_AT_WRITE_LEVEL
#undef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
#endif
/* TRIE_LOCK_USING */
#if !defined(TRIE_LOCK_USING_NODE_FIELD) && !defined(TRIE_LOCK_USING_GLOBAL_ARRAY)
#error Define a trie lock data structure
#endif
#if defined(TRIE_LOCK_USING_NODE_FIELD) && defined(TRIE_LOCK_USING_GLOBAL_ARRAY)
#error Do not define multiple trie lock data structures
#endif
#else /* ! TABLING || (! YAPOR && ! THREADS) */
#undef SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL
#undef SUBGOAL_TRIE_LOCK_AT_NODE_LEVEL
#undef SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL
#undef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
#undef ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL
#undef ANSWER_TRIE_LOCK_AT_NODE_LEVEL
#undef ANSWER_TRIE_LOCK_AT_WRITE_LEVEL
#undef ANSWER_TRIE_ALLOC_BEFORE_CHECK
#undef GLOBAL_TRIE_LOCK_AT_NODE_LEVEL
#undef GLOBAL_TRIE_LOCK_AT_WRITE_LEVEL
#undef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
#undef TRIE_LOCK_USING_NODE_FIELD
#undef TRIE_LOCK_USING_GLOBAL_ARRAY
#endif /* TABLING && (YAPOR || THREADS) */

#if defined(TABLING) && defined(THREADS)
#if !defined(THREADS_NO_SHARING) && !defined(THREADS_SUBGOAL_SHARING) && !defined(THREADS_FULL_SHARING) && !defined(THREADS_CONSUMER_SHARING)
#error Define a multithreading table design
#endif
#if defined(THREADS_NO_SHARING) && defined(THREADS_SUBGOAL_SHARING)
#error Do not define multiple multithreading table designs
#endif
#if defined(THREADS_NO_SHARING) && defined(THREADS_FULL_SHARING)
#error Do not define multiple multithreading table designs
#endif
#if defined(THREADS_NO_SHARING) && defined(THREADS_CONSUMER_SHARING)
#error Do not define multiple multithreading table designs
#endif
#if defined(THREADS_SUBGOAL_SHARING) && defined(THREADS_FULL_SHARING)
#error Do not define multiple multithreading table designs
#endif
#if defined(THREADS_SUBGOAL_SHARING) && defined(THREADS_CONSUMER_SHARING)
#error Do not define multiple multithreading table designs
#endif
#if defined(THREADS_FULL_SHARING) && defined(THREADS_CONSUMER_SHARING)
#error Do not define multiple multithreading table designs
#endif
#ifdef THREADS_NO_SHARING
#undef SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL
#undef SUBGOAL_TRIE_LOCK_AT_NODE_LEVEL
#undef SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL
#undef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
#endif
#if defined(THREADS_NO_SHARING) || defined(THREADS_SUBGOAL_SHARING)
#undef ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL
#undef ANSWER_TRIE_LOCK_AT_NODE_LEVEL
#undef ANSWER_TRIE_LOCK_AT_WRITE_LEVEL
#undef ANSWER_TRIE_ALLOC_BEFORE_CHECK
#endif
#else /* ! TABLING || ! THREADS */
#undef THREADS_NO_SHARING
#undef THREADS_SUBGOAL_SHARING
#undef THREADS_FULL_SHARING
#undef THREADS_CONSUMER_SHARING
#endif /* TABLING && THREADS */

#ifdef TRIE_LOCK_USING_NODE_FIELD
#if defined(SUBGOAL_TRIE_LOCK_AT_NODE_LEVEL) || defined(SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL)
#define SUBGOAL_TRIE_LOCK_USING_NODE_FIELD   1
#endif
#if defined(ANSWER_TRIE_LOCK_AT_NODE_LEVEL) || defined(ANSWER_TRIE_LOCK_AT_WRITE_LEVEL)
#define ANSWER_TRIE_LOCK_USING_NODE_FIELD    1
#endif
#if defined(GLOBAL_TRIE_LOCK_AT_NODE_LEVEL) || defined(GLOBAL_TRIE_LOCK_AT_WRITE_LEVEL)
#define GLOBAL_TRIE_LOCK_USING_NODE_FIELD    1
#endif
#elif defined(TRIE_LOCK_USING_GLOBAL_ARRAY)
#if defined(SUBGOAL_TRIE_LOCK_AT_NODE_LEVEL) || defined(SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL)
#define SUBGOAL_TRIE_LOCK_USING_GLOBAL_ARRAY 1
#endif
#if defined(ANSWER_TRIE_LOCK_AT_NODE_LEVEL) || defined(ANSWER_TRIE_LOCK_AT_WRITE_LEVEL)
#define ANSWER_TRIE_LOCK_USING_GLOBAL_ARRAY  1
#endif
#if defined(GLOBAL_TRIE_LOCK_AT_NODE_LEVEL) || defined(GLOBAL_TRIE_LOCK_AT_WRITE_LEVEL)
#define GLOBAL_TRIE_LOCK_USING_GLOBAL_ARRAY  1
#endif
#endif

#if !defined(TABLING) || !defined(YAPOR)
#undef TABLING_INNER_CUTS
#undef TIMESTAMP_CHECK
#endif

#if !defined(TABLING) || !defined(THREADS)
#undef OUTPUT_THREADS_TABLING
#endif

#if defined(DEBUG_YAPOR) && defined(DEBUG_TABLING)
#define DEBUG_OPTYAP
#endif

#if defined(LIMIT_TABLING) && !defined(USE_PAGES_MALLOC)
#error LIMIT_TABLING requires USE_PAGES_MALLOC
#endif

#if defined(YAPOR) || defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#undef TABLING_EARLY_COMPLETION
#endif

#if defined(YAPOR) || defined(THREADS)
#undef INCOMPLETE_TABLING
#undef LIMIT_TABLING
#undef DETERMINISTIC_TABLING
#endif

#if defined(YAPOR)
#undef MODE_DIRECTED_TABLING
#endif
