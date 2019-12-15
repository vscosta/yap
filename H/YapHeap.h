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

#include "alloc.h"
#include "Atoms.h"
#include "Yap.h"

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

#ifndef INT_KEYS_DEFAULT_SIZE
#define INT_KEYS_DEFAULT_SIZE 256
#endif

#if USE_DL_MALLOC

#define MAX_DLMALLOC_HOLES 32

typedef struct memory_hole {
  ADDR start;
  ADDR end;
} memory_hole_type;
#endif // USE_DL_MALLOC

typedef struct swi_reverse_hash {
  ADDR key;
  Int pos;
} swi_rev_hash;

//#define GC_MAVARS_HASH_SIZE 512
//
// typedef struct gc_ma_hash_entry_struct {
//  UInt timestmp;
//#ifdef TABLING
//  tr_fr_ptr loc;
//  struct gc_ma_hash_entry_struct *more;
//#endif
//  CELL* addr;
//  struct gc_ma_hash_entry_struct *next;
//} gc_ma_hash_entry;

typedef void (*HaltHookFunc)(int, void *);

typedef struct halt_hook {
  void *environment;
  HaltHookFunc hook;
  struct halt_hook *next;
} halt_hook_entry;

int Yap_HaltRegisterHook(HaltHookFunc, void *);

typedef struct atom_hash_entry {
#if defined(YAPOR) || defined(THREADS)
  rwlock_t AERWLock;
#endif
  Atom Entry;
} AtomHashEntry;

// typedef struct scratch_block_struct {
//  char *ptr;
//  UInt sz, msz;
//} scratch_block;

typedef struct record_list {
  /* a list of dbterms associated with a clause */
  struct DB_TERM *dbrecord;
  struct record_list *next_rec, *prev_rec;
} DBRecordList;

/* SWI Emulation */
#define SWI_BUF_SIZE 512
#define SWI_TMP_BUF_SIZE 2 * SWI_BUF_SIZE
#define SWI_BUF_RINGS 16

/* ricardo
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
#endif */

// typedef int   (*Agc_hook)(Atom);

/*******************
  this is the data base: everything here should be possible to restore
********************/
#if __INIT_C__ || defined(MSC_VER)
#define EXTERNAL
#else
#define EXTERNAL extern
#endif


#if YAPOR

typedef struct various_codes {
  /* memory allocation and management */
  special_functors funcs;

#include "struct.h"

} all_heap_codes;

#include "generated/hglobals.h"

#include "generated/dhstruct.h"
#include "generated/dglobals.h"
#else
typedef struct various_codes {
  /* memory allocation and management */
  special_functors funcs;


} all_heap_codes;


#include "generated/tatoms.h"

#include "generated/h0struct.h"

#include "generated/h0globals.h"

#endif

#include "generated/hlocals.h"

#include "dlocals.h"


/* ricardo
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
extern struct global_data *Yap_global;
extern long Yap_worker_area_size;
#else
extern struct global_data Yap_Global;
#define Yap_global (&Yap_Global)
#endif

#if defined(THREADS)
extern struct worker_local *Yap_local[MAX_THREADS];
#define REMOTE(wid)        (Yap_local[wid])
#elif defined(YAPOR)
extern struct worker_local *Yap_local;
#define REMOTE(wid)        (Yap_local + wid)
#else
extern struct worker_local Yap_local;
#define REMOTE(wid)        (&Yap_local)
#endif
*/

#ifdef USE_SYSTEM_MALLOC
extern struct various_codes *Yap_heap_regs;
#else
#define Yap_heap_regs ((all_heap_codes *)HEAP_INIT_BASE)
#endif

/**
 * gc-P: how to start-up the grbage collector in C-code
 */

static inline yamop *gc_P(yamop *p, yamop *cp) {
  yamop *n= (p && p->opc == EXECUTE_CPRED_OPCODE ? cp : p);
 if (p->opc == Yap_opcode(_try_c) ||
      p->opc == Yap_opcode(_try_userc) ||
      p->opc == Yap_opcode(_retry_c) ||
      p->opc == Yap_opcode(_retry_userc))
    return cp;
  return n;
}

/**
  Yap_CurrentModule: access the current module for looking
  up predicates
  */

#define Yap_CurrentModule() Yap_CurrentModule__(PASS_REGS1)

INLINE_ONLY Term Yap_CurrentModule__(USES_REGS1);

INLINE_ONLY Term Yap_CurrentModule__(USES_REGS1) {
  if (CurrentModule)
    return CurrentModule;
  return TermProlog;
}

/*******************
  these are the global variables: they need not be restored...
********************/

#define UPDATE_MODE_IMMEDIATE 0
#define UPDATE_MODE_LOGICAL 1
#define UPDATE_MODE_LOGICAL_ASSERT 2

/* initially allow for files with up to 1024 predicates. This number
   is extended whenever needed */
#define InitialConsultCapacity 1024

#if (defined(USE_SYSTEM_MALLOC) && HAVE_MALLINFO) || USE_DL_MALLOC
extern UInt Yap_givemallinfo(void);
#endif

extern ADDR Yap_ExpandPreAllocCodeSpace(UInt, void *, int);
#define Yap_ReleasePreAllocCodeSpace(x)
extern ADDR Yap_InitPreAllocCodeSpace(int);

#include "inline-only.h"
INLINE_ONLY ADDR Yap_PreAllocCodeSpace(void);

INLINE_ONLY ADDR Yap_PreAllocCodeSpace(void) {
  CACHE_REGS
    return AuxBase;
}

#endif /* HEAP_H */
