/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        opt.config.h
  version:     $Id: opt.config.h,v 1.6 2005-06-03 18:28:11 ricroc Exp $   
                                                                     
**********************************************************************/

/* ---------------------------------- **
**      Configuration Parameters      **
** ---------------------------------- */

/* --------------- **
**      sizes      **
** --------------- */
#define MAX_LENGTH_ANSWER  500
#define MAX_DEPTH          1000
#define MAX_BEST_TIMES     21
#define MAX_TABLE_VARS     100
#define TABLE_LOCK_BUCKETS 512
#define TG_ANSWER_SLOTS    20

/* ---------------------------- **
**      memory (define one)     **
** ---------------------------- */
#define MMAP_MEMORY_MAPPING_SCHEME 1
/* #define SHM_MEMORY_MAPPING_SCHEME  1 */

/* ------------------------------------- **
**      freezing trail (define one)      **
** ------------------------------------- */
#define BFZ_TRAIL_SCHEME   1
/* #define BBREG_TRAIL_SCHEME 1 */

/* ------------------------------------------------------------------ **
**                      locking tries (define one)                    **
** ------------------------------------------------------------------ **
** The TABLE_LOCK_AT_ENTRY_LEVEL scheme locks the access to the table **
** space in the entry data structure. It restricts the number of lock **
** operations needed to go through the table data structures.         **
**                                                                    **
** The TABLE_LOCK_AT_NODE_LEVEL scheme locks each data structure      **
** before accessing it. It decreases concurrrency for workers         **
** accessing commom parts of the table space.                         **
**                                                                    **
** The TABLE_LOCK_AT_WRITE_LEVEL scheme is an hibrid scheme, it only  **
** locks a table data structure when it is going to update it. You    **
** can use ALLOC_BEFORE_CHECK with this scheme to allocate a node     **
** before checking if it will be necessary.                           **
** ------------------------------------------------------------------ */
#define TABLE_LOCK_AT_WRITE_LEVEL 1
/* #define TABLE_LOCK_AT_ENTRY_LEVEL 1 */
/* #define TABLE_LOCK_AT_NODE_LEVEL  1 */
/* #define ALLOC_BEFORE_CHECK        1 */

/* ------------------------ **
**      cuts (optional)     **
** ------------------------ */
#define TABLING_INNER_CUTS 1

/* ------------------------------ **
**      suspension (optional)     **
** ------------------------------ */
#define TIMESTAMP_CHECK 1

/* ----------------------------- **
**      debugging (optional)     **
** ----------------------------- */
/* #define STATISTICS     1 */
/* #define YAPOR_ERRORS   1 */
/* #define TABLING_ERRORS 1 */



/* -------------------------- **
**      Parameter Checks      **
** -------------------------- */

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
#endif /* YAPOR */

#ifdef TABLING
#if !defined(BFZ_TRAIL_SCHEME) && !defined(BBREG_TRAIL_SCHEME)
#error Define a trail scheme
#endif /* !BFZ_TRAIL_SCHEME && !BBREG_TRAIL_SCHEME */
#if defined(BFZ_TRAIL_SCHEME) && defined(BBREG_TRAIL_SCHEME)
#error Do not define multiple trail schemes
#endif /* BFZ_TRAIL_SCHEME && BBREG_TRAIL_SCHEME */
#else
#undef BFZ_TRAIL_SCHEME
#undef BBREG_TRAIL_SCHEME
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
#undef YAPOR_ERRORS
#endif /* !YAPOR */
#ifndef TABLING
#undef TABLING_ERRORS
#endif /* !TABLING */
#if defined(YAPOR_ERRORS) && defined(TABLING_ERRORS)
#define OPTYAP_ERRORS
#endif /* YAPOR_ERRORS && TABLING_ERRORS */
