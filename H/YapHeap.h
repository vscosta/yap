/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapHeap.h      						 *
* mods:									 *
* comments:	Heap Init Structure					 *
* version:      $Id: Heap.h,v 1.136 2008-08-08 14:05:34 vsc Exp $	 *
*************************************************************************/

/* information that can be stored in Code Space */

#ifndef HEAP_H
#define HEAP_H 1

#if LOW_PROF
#include <stdio.h>
#endif

typedef int (*SWI_PutFunction)(int, void *);
typedef int (*SWI_GetFunction)(void *);
typedef int (*SWI_PutWideFunction)(int, void *);
typedef int (*SWI_GetWideFunction)(void *);
typedef int (*SWI_CloseFunction)(void *);
typedef int (*SWI_FlushFunction)(void *);
typedef int (*SWI_PLGetStreamFunction)(void *);
typedef int (*SWI_PLGetStreamPositionFunction)(void *);

#include "../include/dswiatoms.h"

#ifndef INT_KEYS_DEFAULT_SIZE
#define INT_KEYS_DEFAULT_SIZE 256
#endif

#if USE_DL_MALLOC

#define MAX_DLMALLOC_HOLES 32

typedef struct memory_hole {
  ADDR start;
  ADDR end;
} memory_hole_type;
#endif

typedef struct swi_reverse_hash {
  ADDR key;
  Int pos;
} swi_rev_hash;

#define GC_MAVARS_HASH_SIZE 512

typedef struct gc_ma_hash_entry_struct {
  UInt timestmp;
#ifdef TABLING
  tr_fr_ptr loc;
  struct gc_ma_hash_entry_struct *more;
#endif
  CELL* addr;
  struct gc_ma_hash_entry_struct *next;
} gc_ma_hash_entry;

typedef void (*HaltHookFunc)(int, void *);

typedef struct halt_hook {
  void * environment;
  HaltHookFunc hook;
  struct halt_hook *next;
} halt_hook_entry;

int	STD_PROTO(Yap_HaltRegisterHook,(HaltHookFunc, void *));

typedef struct atom_hash_entry {
#if defined(YAPOR) || defined(THREADS)
  rwlock_t AERWLock;
#endif
  Atom Entry;
} AtomHashEntry;

typedef struct reduction_counters {
  YAP_ULONG_LONG reductions;
  YAP_ULONG_LONG reductions_retries;
  YAP_ULONG_LONG retries;
  int reductions_on;
  int reductions_retries_on;
  int retries_on;
} red_counters;

typedef struct scratch_block_struct {
  char *ptr;
  UInt sz, msz;
} scratch_block;

typedef struct record_list {
  /* a list of dbterms associated with a clause */
  struct DB_TERM *dbrecord;
  struct record_list *next_rec, *prev_rec;
} DBRecordList;

typedef struct restore_info {
  Int base_diff;
  Int cl_diff;
  Int g_diff;
  Int g_diff0;
  Int h_diff;
  Int l_diff;
  Int tr_diff;
  Int x_diff;
  Int delay_diff;
  CELL *old_ASP;
  CELL *old_LCL0;
  CELL *g_split;
  tr_fr_ptr old_TR;  
  CELL *old_GlobalBase;
  CELL *old_H;
  CELL *old_H0;
  ADDR old_TrailBase;
  ADDR old_TrailTop;
  ADDR old_HeapBase;
  ADDR old_HeapTop;
} restoreinfo;

/* SWI Emulation */
#define SWI_BUF_SIZE 512
#define SWI_TMP_BUF_SIZE 2*SWI_BUF_SIZE
#define SWI_BUF_RINGS 16

#ifdef THREADS
typedef struct thandle {
  int in_use;
  int zombie;
  UInt ssize;
  UInt tsize;
  UInt sysize;
  void *stack_address;
  Term tdetach;
  Term  cmod, texit_mod;
  struct DB_TERM *tgoal, *texit;
  int id;
  int ret;
  REGSTORE *default_yaam_regs;
  REGSTORE *current_yaam_regs;
  struct pred_entry *local_preds;
  pthread_t pthread_handle;
  int ref_count;
#ifdef LOW_LEVEL_TRACER
  long long int thread_inst_count;
  int been_here1;
  int been_here2;
#endif
#ifdef DEBUG
  int been_here;
#endif
  pthread_mutex_t tlock;
  pthread_mutex_t tlock_status;
#if HAVE_GETRUSAGE||defined(_WIN32)
  struct timeval *start_of_timesp;
  struct timeval *last_timep;
#endif
} yap_thandle;
#endif

typedef int   (*Agc_hook)(Atom);

/*******************
  this is the data base: everything here should be possible to restore 
********************/
typedef struct various_codes {
  /* memory allocation and management */
  special_functors funcs;

#include "hstruct.h"

} all_heap_codes;

#include "hglobals.h"

#if defined(YAPOR) && !defined(THREADS)
extern struct worker_shared *Yap_global;
#else
extern struct worker_shared Yap_Global;
#define Yap_global (&Yap_Global)
#endif

#if defined(YAPOR) || defined(THREADS)
#if defined(THREADS)
extern struct worker_local	Yap_WLocal[MAX_AGENTS];
#else
extern struct worker_local	*Yap_WLocal;
#endif
#define WL (Yap_WLocal+worker_id)
#define FOREIGN_WL(wid) (Yap_WLocal+(wid))
#else
extern struct worker_local	Yap_WLocal;
#define WL (&Yap_WLocal)
#define FOREIGN_WL(wid) (&Yap_WLocal)
#endif


#ifdef USE_SYSTEM_MALLOC
extern struct various_codes *Yap_heap_regs;
#else
#define Yap_heap_regs  ((all_heap_codes *)HEAP_INIT_BASE)
#endif

#include "dhstruct.h"
#include "dglobals.h"

/*******************
  these are the global variables: they need not be restored...
********************/

#define UPDATE_MODE_IMMEDIATE          0
#define UPDATE_MODE_LOGICAL            1
#define UPDATE_MODE_LOGICAL_ASSERT     2




/* initially allow for files with up to 1024 predicates. This number
   is extended whenever needed */
#define  InitialConsultCapacity    1024


#if (defined(USE_SYSTEM_MALLOC) && HAVE_MALLINFO)||USE_DL_MALLOC
UInt STD_PROTO(Yap_givemallinfo, (void));
#endif

ADDR    STD_PROTO(Yap_ExpandPreAllocCodeSpace, (UInt, void *, int));
#define Yap_ReleasePreAllocCodeSpace(x)
ADDR    STD_PROTO(Yap_InitPreAllocCodeSpace, (void));
EXTERN inline ADDR
Yap_PreAllocCodeSpace(void) 
{
  return AuxBase;
}

#endif /* HEAP_H */
