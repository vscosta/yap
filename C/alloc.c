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
 * File:		alloc.c * Last
 *rev:								 * mods:
 ** comments:	allocating space					 *
 * version:$Id: alloc.c,v 1.95 2008-05-10 23:24:11 vsc Exp $		 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";

#endif

#include "Yap.h"

#include "YapHeap.h"
#include "Yatom.h"
#include "alloc.h"
#include "yapio.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_MALLOC_H
#include <malloc.h>
#endif
#if USE_DL_MALLOC
#include "dlmalloc.h"
#endif
#if HAVE_MEMORY_H
#include <memory.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_SYS_TIME_H
#include <sys/time.h> 
#endif
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h> 
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include <stdio.h>
#include <stdlib.h>

#if __simplescalar__
#ifdef USE_SYSTEM_MMAP
#undef USE_SYSTEM_MMAP
#endif
#ifdef USE_SBRK
#undef USE_SBRK
#endif
#endif

/************************************************************************/
/* Yap workspace management                                             */

#define MASK 0x968e00
#if USE_SYSTEM_MALLOC

#if 1

#undef free
#undef malloc
#undef realloc

int write_malloc = 0;

void *my_malloc(size_t sz) {
  void *p;

  p = malloc(sz);
  //    Yap_DebugPuts(stderr,"gof\n");
  if (Yap_do_low_level_trace) {
#if __ANDROID__
      //   __android_log_print(ANDROID_LOG_ERROR, "YAPDroid ", "+ %d %p", write_malloc,p);
#else
      fprintf(stderr, "+s %p\n @%p %ld\n", p, TR, LCL0 - (CELL *)LCL0);
#endif
  }
  return p;
}

void *my_realloc(void *ptr, size_t sz) {
  void *p;

  p = realloc(ptr, sz);
  //if (Yap_do_low_level_trace)
    //  fprintf(stderr, "+ %p -> %p : " Sizet_F "\n", ptr, p, sz);
    //    Yap_DebugPuts(stderr,"gof\n");
//    if (sz > 500 && write_malloc++ > 0)
//      __android_log_print(ANDROID_LOG_ERROR, "YAPDroid ", "* %d %p",
//                          write_malloc, p);
  return p;
}

void my_free(void *p) {
  // printf("f %p\n",p);
  if (Yap_do_low_level_trace)
    fprintf(stderr, "- %p\n @%p %ld\n", p, TR, (long int)(LCL0 - (CELL *)B) );
  //if (write_malloc && write_malloc++ > 0)
//    __android_log_print(ANDROID_LOG_ERROR, "YAPDroid ", "- %d %p", write_malloc,
//                        p);

  free(p);
  //    Yap_DebugPuts(stderr,"gof\n");
}

#endif

#else // USE_SYSTEM_MALLOC

#define my_malloc(sz) Yap_dlmalloc(sz)
#define my_realloc(ptr, sz) Yap_dlrealloc(ptr, sz)
#define my_free(sz) Yap_dlfree(sz)

static char *my_reallocl(char *ptr, UInt sz, UInt osz, int safe) {
  char *nptr;

restart:
  /* simple case */
  if (ptr < Yap_HeapBase || ptr > HeapTop) {
    /* we have enough room */
    nptr = Yap_dlmalloc(sz);
    if (nptr) {
      memmove(nptr, ptr, osz);
      free(ptr);
    }
  } else {
    nptr = Yap_dlrealloc(ptr, sz);
  }
  if (nptr) {
    return nptr;
  }
  /* we do not have enough room */
  if (safe) {
    if (Yap_growheap(FALSE, sz, NULL)) {
      /* now, we have room */
      goto restart;
    }
  }
  /* no room in Heap, gosh */
  if (ptr < Yap_HeapBase || ptr > HeapTop) {
    /* try expanding outside the heap */
    nptr = realloc(ptr, sz);
    if (nptr) {
      memmove(nptr, ptr, osz);
    }
  } else {
    /* try calling the outside world for help */
    nptr = malloc(sz);
    if (!nptr)
      return NULL;
    memmove(nptr, ptr, osz);
    Yap_dlfree(ptr);
  }
  /* did we suceed? at this point we could not care less */
  return nptr;
}
#endif

#if USE_SYSTEM_MALLOC || USE_DL_MALLOC

long long unsigned int mallocs, reallocs, frees;
long long unsigned int tmalloc;

#undef INSTRUMENT_MALLOC

static inline char *call_malloc(size_t size) {
  CACHE_REGS
  char *out;
#if USE_DL_MALLOC
  LOCK(DLMallocLock);
#endif
#if INSTRUMENT_MALLOC
  mallocs++;
  tmalloc += size;
  size += sizeof(CELL);
#endif
  LOCAL_PrologMode |= MallocMode;
  out = (char *)my_malloc(size);
#if INSTRUMENT_MALLOC
  *(CELL *)out = size - sizeof(CELL);
  out += sizeof(CELL);
#endif
  LOCAL_PrologMode &= ~MallocMode;
#if USE_DL_MALLOC
  UNLOCK(DLMallocLock);
#endif
  return out;
}

void *Yap_AllocCodeSpace(size_t size) {
  size = AdjustSize(size);
  return call_malloc(size);
}

static inline char *call_realloc(char *p, size_t size) {
  CACHE_REGS
  char *out;
#if USE_DL_MALLOC
  LOCK(DLMallocLock);
#endif
#if INSTRUMENT_MALLOC
  reallocs++;
  tmalloc += size;
  size += sizeof(CELL);
  p -= sizeof(CELL);
  tmalloc -= *(CELL *)p;
#endif
  LOCAL_PrologMode |= MallocMode;
  out = (char *)my_realloc(p, size);
#if INSTRUMENT_MALLOC
  *(CELL *)out = size - sizeof(CELL);
  out += sizeof(CELL);
#endif
  LOCAL_PrologMode &= ~MallocMode;
#if USE_DL_MALLOC
  UNLOCK(DLMallocLock);
#endif
  return out;
}

void *Yap_ReallocCodeSpace(void *p, size_t size) {
  size = AdjustSize(size);
  return call_realloc(p, size);
}

void Yap_FreeCodeSpace(void *p) {
  CACHE_REGS
#if USE_DL_MALLOC
  LOCK(DLMallocLock);
#endif
  LOCAL_PrologMode |= MallocMode;
#if INSTRUMENT_MALLOC
  p -= sizeof(CELL);
  tmalloc -= *(CELL *)p;
  frees++;
#endif
  my_free(p);
  LOCAL_PrologMode &= ~MallocMode;
#if USE_DL_MALLOC
  UNLOCK(DLMallocLock);
#endif
}

void *Yap_AllocAtomSpace(size_t size) {
  size = AdjustSize(size);
  return call_malloc(size);
}

void Yap_FreeAtomSpace(void *p) {
  CACHE_REGS
#if USE_DL_MALLOC
  LOCK(DLMallocLock);
#endif
  LOCAL_PrologMode |= MallocMode;
#if INSTRUMENT_MALLOC
  p -= sizeof(CELL);
  tmalloc -= *(CELL *)p;
  frees++;
#endif
  my_free(p);
  LOCAL_PrologMode &= ~MallocMode;
#if USE_DL_MALLOC
  UNLOCK(DLMallocLock);
#endif
}

#endif

/* If you need to dinamically allocate space from the heap, this is
 * the macro you should use */
ADDR Yap_InitPreAllocCodeSpace(int wid) {
  CACHE_REGS
  char *ptr;
  UInt sz = REMOTE_ScratchPad(wid).msz;

  if (REMOTE_ScratchPad(wid).ptr == NULL) {
#if USE_DL_MALLOC
    LOCK(DLMallocLock);
#endif
    REMOTE_PrologMode(wid) |= MallocMode;
#if INSTRUMENT_MALLOC
    mallocs++;
    tmalloc += sz;
    sz += sizeof(CELL);
#endif
    while (!(ptr =
#ifdef YAPOR_COPY
                 malloc(sz)
#else
                 my_malloc(sz)
#endif
    )) {
      REMOTE_PrologMode(wid) &= ~MallocMode;
#if USE_DL_MALLOC
      UNLOCK(DLMallocLock);
#endif
      if (!Yap_growheap(FALSE, LOCAL_Error_Size, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
        return (NULL);
      }
#if INSTRUMENT_MALLOC
      sz -= sizeof(CELL);
      *(CELL *)ptr = sz;
      ptr += sizeof(CELL);
#endif
#if USE_DL_MALLOC
      LOCK(DLMallocLock);
#endif
      REMOTE_PrologMode(wid) |= MallocMode;
    }
    REMOTE_PrologMode(wid) &= ~MallocMode;
#if USE_DL_MALLOC
    UNLOCK(DLMallocLock);
#endif
    REMOTE_ScratchPad(wid).ptr = ptr;
  } else {
    ptr = REMOTE_ScratchPad(wid).ptr;
  }
  AuxBase = (ADDR)(ptr);
  AuxSp = (CELL *)(AuxTop = AuxBase + REMOTE_ScratchPad(wid).sz);
  return ptr;
}

ADDR Yap_ExpandPreAllocCodeSpace(UInt sz0, void *cip, int safe) {
  CACHE_REGS
  char *ptr;
  UInt sz;
  if (sz0 < SCRATCH_INC_SIZE)
    sz0 = SCRATCH_INC_SIZE;
  if (sz0 < LOCAL_ScratchPad.sz)
    sz = LOCAL_ScratchPad.sz + sz0;
  else
    sz = sz0;
  sz = AdjustLargePageSize(sz + sz / 4);

#if USE_DL_MALLOC
  LOCK(DLMallocLock);
#endif
  LOCAL_PrologMode |= MallocMode;
#if INSTRUMENT_MALLOC
  reallocs++;
  tmalloc -= LOCAL_ScratchPad.sz;
  tmalloc += sz;
#endif
  if (!(ptr = my_realloc(LOCAL_ScratchPad.ptr, sz))) {
    LOCAL_PrologMode &= ~MallocMode;
#if USE_DL_MALLOC
    UNLOCK(DLMallocLock);
#endif
    return NULL;
  }
  LOCAL_PrologMode &= ~MallocMode;
#if USE_DL_MALLOC
  UNLOCK(DLMallocLock);
#endif
  LOCAL_ScratchPad.sz = LOCAL_ScratchPad.msz = sz;
  LOCAL_ScratchPad.ptr = ptr;
  AuxBase = ptr;
  AuxSp = (CELL *)(AuxTop = ptr + sz);
  return ptr;
}

#if USE_SYSTEM_MALLOC

struct various_codes *Yap_heap_regs;


// get an approximation to total memory data-base size.
 size_t Yap_HeapUsed(void)
{
  #if HAVE_MALLINFO
    struct mallinfo mi = mallinfo();
    return mi.uordblks - (LOCAL_TrailTop-LOCAL_GlobalBase);
#else
    return         Yap_ClauseSpace+Yap_IndexSpace_Tree+Yap_LUClauseSpace+Yap_LUIndexSpace_CP;
#endif
}

static void InitExStacks(int wid, size_t Trail, size_t Stack) {
  CACHE_REGS
  size_t pm, sa;

  /* sanity checking for data areas */
  if (Trail < MinTrailSpace)
    Trail = MinTrailSpace;
  if (Stack < MinStackSpace)
    Stack = MinStackSpace;
  pm = (Trail + Stack) * K; /* memory to be
                             * requested         */
  sa = Stack * K; /* stack area size   */

#ifdef THREADS
  if (wid)
    REMOTE_GlobalBase(wid) = (ADDR)REMOTE_ThreadHandle(wid).stack_address;
  else
    AuxSp = NULL;
#endif
  REMOTE_TrailTop(wid) = REMOTE_GlobalBase(wid) + pm;
  REMOTE_LocalBase(wid) = REMOTE_GlobalBase(wid) + sa;
  REMOTE_TrailBase(wid) = REMOTE_LocalBase(wid) + sizeof(CELL);

  REMOTE_ScratchPad(wid).ptr = NULL;
  REMOTE_ScratchPad(wid).sz = REMOTE_ScratchPad(wid).msz = SCRATCH_START_SIZE;

#if DEBUG
  if (Yap_output_msg) {
    size_t ta;

    fprintf(stderr,
            "HeapBase = %p  GlobalBase = %p\n  LocalBase = %p  TrailTop = %p\n",
            Yap_HeapBase, REMOTE_GlobalBase(wid), REMOTE_LocalBase(wid),
            REMOTE_TrailTop(wid));

    ta = Trail * K; /* trail area size   */
    fprintf(stderr, "Heap+Aux: %lu\tLocal+Global: %lu\tTrail: %lu\n",
            (long unsigned)(pm - sa - ta), (long unsigned)sa,
            (long unsigned)ta);
  }
#endif /* DEBUG */
}

void Yap_InitExStacks(int wid, size_t Trail, size_t Stack) {
  memset(&REMOTE_WorkerBuffer(wid),0,sizeof(scratch_sys_struct_t));
  InitExStacks(wid, Trail, Stack);
}

#if defined(THREADS)
void Yap_KillStacks(int wid) {
  ADDR gb = REMOTE_ThreadHandle(wid).stack_address;
  if (gb) {
    free(gb);
    REMOTE_ThreadHandle(wid).stack_address = NULL;
  }
}
#else
void Yap_KillStacks(int wid) {
  if (LOCAL_GlobalBase) {
    free(LOCAL_GlobalBase);
    LOCAL_GlobalBase = NULL;
  }
}
#endif

void Yap_InitMemory(size_t Trail, size_t Heap, size_t Stack) {
      Yap_HoleSize = 0;
  HeapMax = 0;
        Yap_heap_regs =
                (struct various_codes *)calloc(1, sizeof(struct various_codes));
    }

int Yap_ExtendWorkSpace(Int s) {
  CACHE_REGS
  void *basebp = LOCAL_GlobalBase, *nbp;
  UInt s0 = LOCAL_TrailTop - LOCAL_GlobalBase;
  nbp = realloc(basebp, s + s0);
  if (nbp == NULL)
    return FALSE;
#if defined(THREADS)
  LOCAL_ThreadHandle.stack_address = (char *)nbp;
#endif
  LOCAL_GlobalBase = (char *)nbp;
  return TRUE;
}

size_t Yap_ExtendWorkSpaceThroughHole(size_t s) { return 0; }

void Yap_AllocHole(UInt actual_request, UInt total_size) {}

#if HAVE_MALLINFO
UInt Yap_givemallinfo(void) {
  struct mallinfo mi = mallinfo();
  return mi.uordblks;
}
#endif

#else

#if HAVE_SNPRINTF
#define snprintf3(A, B, C) snprintf(A, B, C)
#define snprintf4(A, B, C, D) snprintf(A, B, C, D)
#define snprintf5(A, B, C, D, E) snprintf(A, B, C, D, E)
#else
#define snprintf3(A, B, C) sprintf(A, C)
#define snprintf4(A, B, C, D) sprintf(A, C, D)
#define snprintf5(A, B, C, D, E) sprintf(A, C, D, E)
#endif

#ifdef LIGHT
#include <stdlib.h>
#endif

#if !USE_DL_MALLOC

static void FreeBlock(BlockHeader *);
static BlockHeader *GetBlock(unsigned long int);
static char *AllocHeap(unsigned long int);
static void RemoveFromFreeList(BlockHeader *);
static void AddToFreeList(BlockHeader *);

#define MinHGap 256 * K

static void RemoveFromFreeList(BlockHeader *b) {
  BlockHeader *p;

  p = b->b_next_size;
  LOCK(HeapUsedLock);
  HeapUsed += (b->b_size + 1) * sizeof(YAP_SEG_SIZE);
  UNLOCK(HeapUsedLock);

  if (p && b->b_size == p->b_size) {
    b = b->b_next;
    p->b_next = b;
    if (b)
      b->b_next_size = p;
  } else {
    BlockHeader **q = &FreeBlocks;

    while ((*q) != b)
      q = &((*q)->b_next_size);
    if (b->b_next) {
      p = b->b_next;
      *q = p;
      p->b_next_size = b->b_next_size;
    } else {
      *q = b->b_next_size;
    }
  }
}

static void AddToFreeList(BlockHeader *b) {
  BlockHeader **q, *p;
  YAP_SEG_SIZE *sp;

  /* insert on list of free blocks */
  q = &FreeBlocks;
  sp = &(b->b_size) + b->b_size;
  *sp = b->b_size;
  LOCK(HeapUsedLock);
  HeapUsed -= (b->b_size + 1) * sizeof(YAP_SEG_SIZE);
  UNLOCK(HeapUsedLock);

  while ((p = *q) && p->b_size < b->b_size)
    q = &p->b_next_size;
  if (p && p->b_size == b->b_size) {
    b->b_next = p;
    b->b_next_size = p->b_next_size;
    p->b_next_size = b;
  } else {
    b->b_next = NIL;
    b->b_next_size = p;
  }
  *q = b;
}

static void FreeBlock(BlockHeader *b) {
  BlockHeader *p;
  YAP_SEG_SIZE *sp;

  /*  {
    static long long int vsc_free_ops;

    vsc_free_ops++;
    BlockHeader *q = FreeBlocks;
    while (q) q = q->b_next_size;
    }*/

  /* sanity check */
  sp = &(b->b_size) + (b->b_size & ~InUseFlag);
  if (!(b->b_size & InUseFlag) || *sp != b->b_size) {
#if !SHORT_INTS
    fprintf(
        stderr,
        "%% YAP INTERNAL ERROR: sanity check failed in FreeBlock %p %x %x\n", b,
        b->b_size, Unsigned(*sp));
#else
    fprintf(
        stderr,
        "%% YAP INTERNAL ERROR: sanity check failed in FreeBlock %p %lx %lx\n",
        b, b->b_size, *sp);
#endif
    return;
  }
  b->b_size &= ~InUseFlag;
  LOCK(FreeBlocksLock);
  /* check if we can collapse with other blocsks */
  /* check previous */
  sp = &(b->b_size) - 1;
  if (!(*sp & InUseFlag)) { /* previous block is free */
    p = (BlockHeader *)(sp - *sp);
    RemoveFromFreeList(p);
    p->b_size += b->b_size + 1;
    b = p;
  }
  /* check following */
  sp = &(b->b_size) + b->b_size + 1;
  if (!(*sp & InUseFlag)) { /* following block is free */
    p = (BlockHeader *)sp;
    RemoveFromFreeList(p);
    b->b_size += p->b_size + 1;
  }
  LOCK(HeapTopLock);
  if (sp == (YAP_SEG_SIZE *)HeapTop) {
    LOCK(HeapUsedLock);
    HeapUsed -= (b->b_size + 1) * sizeof(YAP_SEG_SIZE);
    UNLOCK(HeapUsedLock);
    HeapTop = (ADDR)b;
    *((YAP_SEG_SIZE *)HeapTop) = InUseFlag;
  } else {
    /* insert on list of free blocks */
    AddToFreeList(b);
  }
  UNLOCK(HeapTopLock);
  UNLOCK(FreeBlocksLock);
}

static BlockHeader *
GetBlock(unsigned long int n) { /* get free block with size at least n */
  register BlockHeader **p, *b, *r;

  if (FreeBlocks == NIL)
    return (NIL);
  /* check for bugs */
  p = &FreeBlocks;
  /* end check for bugs */
  p = &FreeBlocks;
  while (((b = *p) != NIL) && b->b_size < n)
    p = &b->b_next_size;
  if (b == NIL || b->b_size < n)
    return (NIL);
  if ((r = b->b_next) == NIL)
    *p = b->b_next_size;
  else {
    r->b_next_size = b->b_next_size;
    *p = r;
  }
  LOCK(HeapUsedLock);
  HeapUsed += (b->b_size + 1) * sizeof(YAP_SEG_SIZE);
  if (HeapUsed > HeapMax)
    HeapMax = HeapUsed;
  UNLOCK(HeapUsedLock);
  return (b);
}

static char *AllocHeap(size_t size) {
  BlockHeader *b, *n;
  YAP_SEG_SIZE *sp;
  UInt align, extra;

  /*  {
    static long long int vsc_alloc_ops;
    vsc_alloc_ops++;
    BlockHeader *q = FreeBlocks;
    while (q) q = q->b_next_size;
    }*/

  extra = size / 16;
#if SIZEOF_INT_P == 4
  align = 2 * sizeof(CELL); /* size in dwords + 2 */
#endif
#if SIZEOF_INT_P == 8
  align = sizeof(CELL);
#endif
  while (align < extra)
    align *= 2;
  size = ALIGN_SIZE(size, align);
  if (size < sizeof(BlockHeader))
    size = sizeof(BlockHeader);
  size += sizeof(YAP_SEG_SIZE);
  /* change units to cells */
  size = size / sizeof(CELL);
  LOCK(FreeBlocksLock);
  if ((b = GetBlock(size))) {
    if (b->b_size >= size + 24 + 1) {
      n = (BlockHeader *)(((YAP_SEG_SIZE *)b) + size + 1) v;
      n->b_size = b->b_size - size - 1;
      b->b_size = size;
      AddToFreeList(n);
    }
    sp = &(b->b_size) + b->b_size;
    *sp = b->b_size | InUseFlag;
    b->b_size |= InUseFlag;
    UNLOCK(FreeBlocksLock);
    return (Addr(b) + sizeof(YAP_SEG_SIZE));
  }
  LOCK(HeapTopLock);
  UNLOCK(FreeBlocksLock);
  b = (BlockHeader *)HeapTop;
  HeapTop += size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
  LOCK(HeapUsedLock);
  HeapUsed += size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);

#ifdef YAPOR
  if (HeapTop > Addr(LOCAL_GlobalBase) - MinHeapGap)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "no heap left (AllocHeap)");
#else
  if (HeapTop > HeapLim - MinHeapGap) {
    HeapTop -= size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
    HeapUsed -= size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
    if (HeapTop > HeapLim) {
      UNLOCK(HeapUsedLock);
      UNLOCK(HeapTopLock);
      /* we destroyed the stack */
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "Stack Crashed against Heap...");
      return (NULL);
    } else {
      if (HeapTop + size * sizeof(CELL) + sizeof(YAP_SEG_SIZE) < HeapLim) {
        /* small allocations, we can wait */
        HeapTop += size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
        HeapUsed += size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
        UNLOCK(HeapUsedLock);
        UNLOCK(HeapTopLock);
        Yap_signal(YAP_CDOVF_SIGNAL);
      } else {
        if (size > GLOBAL_SizeOfOverflow)
          GLOBAL_SizeOfOverflow = size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
        /* big allocations, the caller must handle the problem */
        UNLOCK(HeapUsedLock);
        UNLOCK(HeapTopLock);
        return (NULL);
      }
    }
  }
#endif /* YAPOR */
  *((YAP_SEG_SIZE *)HeapTop) = InUseFlag;
  if (HeapUsed > HeapMax)
    HeapMax = HeapUsed;
  UNLOCK(HeapUsedLock);
  b->b_size = size | InUseFlag;
  sp = &(b->b_size) + size;
  *sp = b->b_size;
  UNLOCK(HeapTopLock);
  return (Addr(b) + sizeof(YAP_SEG_SIZE));
}

/* If you need to dinamically allocate space from the heap, this is
 * the macro you should use */
static void FreeCodeSpace(char *p) {
  FreeBlock(((BlockHeader *)(p - sizeof(YAP_SEG_SIZE))));
}

static char *AllocCodeSpace(unsigned long int size) {
  if (size < SmallSize + 2 * OpCodeSize + 3 * CellSize)
    return (AllocHeap(SmallSize + 2 * OpCodeSize + 3 * CellSize));
  return (AllocHeap(size));
}

#if DEBUG_ALLOC
int vsc_mem_trace;
#endif

/* If you need to dinamically allocate space from the heap, this is
 * the macro you should use */
void Yap_FreeCodeSpace(char *p) {
#if DEBUG_ALLOC
  if (vsc_mem_trace)
    printf("-%p\n", p);
#endif
  FreeCodeSpace(p);
}

char *Yap_AllocAtomSpace(unsigned long int size) {
  char *out = AllocHeap(size);
#if DEBUG_ALLOC
  if (vsc_mem_trace)
    printf("+%p/%d\n", out, size);
#endif
  return out;
}

void Yap_FreeAtomSpace(char *p) {
#if DEBUG_ALLOC
  if (vsc_mem_trace)
    printf("-%p\n", p);
#endif
  FreeCodeSpace(p);
}

char *Yap_AllocCodeSpace(unsigned long int size) {
  char *out = AllocCodeSpace(size);
#if DEBUG_ALLOC
  if (vsc_mem_trace)
    printf("+%p/%d\n", out, size);
#endif
  return out;
}

#endif

/************************************************************************/
/* Workspace allocation                                                 */
/*                                                                      */
/* We provide four alternatives for workspace allocation.               */
/* - use 'mmap'                                                         */
/* - use 'shmat'                                                        */
/* - use 'sbrk' and provide a replacement to the 'malloc' library       */
/* - use 'malloc'                                                       */
/*                                                                      */
/*  In any of the alternatives the interface is through the following   */
/* functions:                                                           */
/*   void *InitWorkSpace(int s) - initial workspace allocation          */
/*   int ExtendWorkSpace(int s) - extend workspace                      */
/*   int Yap_FreeWorkSpace() - release workspace                            */
/************************************************************************/

#if defined(_WIN32) || defined(__CYGWIN__)

#undef DEBUG_WIN32_ALLO

#include "windows.h"

static LPVOID brk;

static int ExtendWorkSpace(Int s, int fixed_allocation) {
  LPVOID b = brk;
  prolog_exec_mode OldPrologMode = LOCAL_PrologMode;

  LOCAL_PrologMode = ExtendStackMode;

#if DEBUG_WIN32_ALLOC
  fprintf(stderr, "trying: %p (" Int_FORMAT "K) %d\n", b, s / 1024,
          fixed_allocation);
#endif
  if (fixed_allocation) {
    b = VirtualAlloc(b, s, MEM_RESERVE, PAGE_NOACCESS);
  } else {
    b = VirtualAlloc(NULL, s, MEM_RESERVE, PAGE_NOACCESS);
    if (b && b < brk) {
      return ExtendWorkSpace(s, fixed_allocation);
    }
  }
  if (!b) {
    LOCAL_PrologMode = OldPrologMode;
#if DEBUG_WIN32_ALLOC
    {
      char msg[256];
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                    NULL, GetLastError(),
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), msg, 256, NULL);
      fprintf(stderr, "NOT OK1: %p %p %s\n", brk, b, msg);
    }
#endif
    return FALSE;
  }
  b = VirtualAlloc(b, s, MEM_COMMIT, PAGE_READWRITE);
  if (!b) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
    snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "VirtualAlloc could not commit %ld bytes", (long int)s);
    LOCAL_PrologMode = OldPrologMode;
#if DEBUG_WIN32_ALLOC
    fprintf(stderr, "NOT OK2: %p--%p\n", b, brk);
#endif
    return FALSE;
  }
  brk = (LPVOID)((Int)b + s);
#if DEBUG_WIN32_ALLOC
  fprintf(stderr, "OK: %p--%p " Int_FORMAT "\n", b, brk, s);
#endif
  LOCAL_PrologMode = OldPrologMode;
  return TRUE;
}

static MALLOC_T InitWorkSpace(Int s) {
  SYSTEM_INFO si;
  Int psz;

  GetSystemInfo(&si);
  psz = Yap_page_size = si.dwPageSize;
  brk = (LPVOID)psz;
  if (!ExtendWorkSpace(s, 0))
    return FALSE;
  return (MALLOC_T)brk - s;
}

int Yap_FreeWorkSpace(void) { return TRUE; }

#elif USE_SYSTEM_MMAP

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef MMAP_ADDR
#define USE_FIXED 1
#endif

#ifndef MAP_FIXED
#define MAP_FIXED 1
#endif

static MALLOC_T WorkSpaceTop;

static MALLOC_T InitWorkSpace(Int s) {
  MALLOC_T a;
#if !defined(_AIX) && !defined(__APPLE__) && !__hpux
  int fd;
#endif
#if defined(_AIX)
  a = mmap(0, (size_t)s, PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_PRIVATE | MAP_ANONYMOUS | MAP_VARIABLE, -1, 0);
#elif __hpux
  a = mmap(((void *)MMAP_ADDR), (size_t)s, PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
  if (a != (MALLOC_T)MMAP_ADDR) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
              "mmap could not map ANON at %p, got %p", (void *)MMAP_ADDR, a);
    return (NULL);
  }
#elif defined(__APPLE__)
#ifdef MMAP_ADDR
  a = mmap(((void *)MMAP_ADDR), (size_t)s, PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
  if (a != (MALLOC_T)MMAP_ADDR) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
              "mmap could not map ANON at %p, got %p", (void *)MMAP_ADDR, a);
    return (NULL);
  }
#else
  a = mmap(NULL, (size_t)s, PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_PRIVATE | MAP_ANON, -1, 0);
#endif
#else
  fd = open("/dev/zero", O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if (fd < 0) {
#if HAVE_MKSTEMP
    char file[256];
    strncpy(file, "/tmp/YAP.TMPXXXXXX", 256);
    if (mkstemp(file) == -1) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
                "mkstemp could not create temporary file %s (%s)", file,
                strerror(errno));
#else
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
                "mkstemp could not create temporary file %s", file);
#endif
      return NULL;
    }
#else
#if HAVE_TMPNAM
    char *file = tmpnam(NULL);
#else
    char file[YAP_FILENAME_MAX];
    strcpy(file, "/tmp/mapfile");
    itos(getpid(), &file[12]);
#endif /* HAVE_TMPNAM */
#endif /* HAVE_MKSTEMP */
    fd = open(file, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (fd < 0) {
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "mmap could not open %s", file);
      return NULL;
    }
    if (lseek(fd, s, SEEK_SET) < 0) {
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
                "mmap could not lseek in mmapped file %s", file);
      close(fd);
      return FALSE;
    }
    if (write(fd, "", 1) < 0) {
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
                "mmap could not write in mmapped file %s", file);
      close(fd);
      return NULL;
    }
    if (unlink(file) < 0) {
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
                "mmap could not unlink mmapped file %s", file);
      close(fd);
      return NULL;
    }
  }
#if USE_FIXED
  a = mmap(((void *)MMAP_ADDR), (size_t)s, PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_PRIVATE | MAP_FIXED, fd, 0);
  if (a != (MALLOC_T)MMAP_ADDR) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "mmap could not map at %p, got %p",
              (void *)MMAP_ADDR, a);
    return NULL;
  }
#else
  a = mmap(0, (size_t)s, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE, fd,
           0);
  if ((CELL)a & YAP_PROTECTED_MASK) {
    close(fd);
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
              "mmapped address %p collides with YAP tags", a);
    return NULL;
  }
  if (close(fd) == -1) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "while closing mmaped file");
    return NULL;
  }
#endif
#endif
  if
#ifdef MMAP_FAILED
      (a == (MALLOC_T)MMAP_FAILED)
#else
      (a == (MALLOC_T)-1)
#endif
  {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "mmap cannot allocate memory ***");
    return (NULL);
  }
  WorkSpaceTop = (char *)a + s;
  return (void *)a;
}

#ifndef YAPOR
static MALLOC_T mmap_extension(Int s, MALLOC_T base, int fixed_allocation) {
  MALLOC_T a;

#if !defined(_AIX) && !defined(__hpux) && !defined(__APPLE__)
  int fd;
#endif
#if defined(_AIX) || defined(__hpux)
  a = mmap(base, (size_t)s, PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

#elif defined(__APPLE__)
  a = mmap(base, (size_t)s, PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_PRIVATE | MAP_ANON | fixed_allocation, -1, 0);
#else
  fd = open("/dev/zero", O_RDWR);
  if (fd < 0) {
#if HAVE_MKSTEMP
    char file[256];
    strncpy(file, "/tmp/YAP.TMPXXXXXX", 256);
    if (mkstemp(file) == -1) {
      LOCAL_ErrorMessage = LOCAL_ErrorSay;
#if HAVE_STRERROR
      snprintf5(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
                "mkstemp could not create temporary file %s (%s)", file,
                strerror(errno));
#else
      snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
                "mkstemp could not create temporary file %s", file);
#endif /* HAVE_STRERROR */
      return (MALLOC_T)-1;
    }
#else
#if HAVE_TMPNAM
    char *file = tmpnam(NULL);
#else
    char file[YAP_FILENAME_MAX];
    strcpy(file, "/tmp/mapfile");
    itos(getpid(), &file[12]);
#endif /* HAVE_TMPNAM */
#endif /* HAVE_MKSTEMP */
    fd = open(file, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (fd < 0) {
      LOCAL_ErrorMessage = LOCAL_ErrorSay;
      snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
                "mmap could not open %s", file);
      return (MALLOC_T)-1;
    }
    if (lseek(fd, s, SEEK_SET) < 0) {
      LOCAL_ErrorMessage = LOCAL_ErrorSay;
      snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
                "mmap could not lseek in mmapped file %s", file);
      close(fd);
      return (MALLOC_T)-1;
    }
    if (write(fd, "", 1) < 0) {
      LOCAL_ErrorMessage = LOCAL_ErrorSay;
      snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
                "mmap could not write in mmapped file %s", file);
      close(fd);
      return (MALLOC_T)-1;
    }
    if (unlink(file) < 0) {
      LOCAL_ErrorMessage = LOCAL_ErrorSay;
      snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
                "mmap could not unlink mmapped file %s", file);
      close(fd);
      return (MALLOC_T)-1;
    }
  }
  a = mmap(
      base, (size_t)s, PROT_READ | PROT_WRITE | PROT_EXEC,
      MAP_PRIVATE
#if !defined(__linux)
          /* use  MAP_FIXED, otherwise God knows where you will be placed */
          | fixed_allocation
#endif
      ,
      fd, 0);
  if (close(fd) == -1) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
#if HAVE_STRERROR
    snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "mmap could not close file (%s) ]\n", strerror(errno));
#else
    snprintf3(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "mmap could not close file ]\n");
#endif
    return (MALLOC_T)-1;
  }
#endif
  return a;
}
#endif /* !YAPOR */

/*
   s= how much memory we need;
   fixed_alloc = do we need to contiguously extend stack
   returns TRUE or FALSE

   updates WorkSpaceTop
*/
static int ExtendWorkSpace(Int s, int fixed_allocation) {
#ifdef YAPOR
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "cannot extend stacks (ExtendWorkSpace)");
  return (FALSE);
#else
  MALLOC_T a;
  prolog_exec_mode OldPrologMode = LOCAL_PrologMode;
  MALLOC_T base = WorkSpaceTop;

  if (fixed_allocation == MAP_FIXED)
    base = WorkSpaceTop;
  else
    base = 0L;
  LOCAL_PrologMode = ExtendStackMode;
  a = mmap_extension(s, base, fixed_allocation);
  LOCAL_PrologMode = OldPrologMode;
  if (a == (MALLOC_T)-1) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
#if HAVE_STRERROR
    snprintf5(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "could not allocate %d bytes (%s)", (int)s, strerror(errno));
#else
    snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "could not allocate %d bytes", (int)s);
#endif
    return FALSE;
  }
  if (fixed_allocation) {
    if (a != WorkSpaceTop) {
      munmap((void *)a, (size_t)s);
      LOCAL_ErrorMessage = LOCAL_ErrorSay;
      snprintf5(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
                "mmap could not grow memory at %p, got %p", WorkSpaceTop, a);
      LOCAL_PrologMode = OldPrologMode;
      return FALSE;
    }
  } else if (a < WorkSpaceTop) {
    /* try again */
    int res = ExtendWorkSpace(s, fixed_allocation);
    /* release memory back to system */
    munmap(a, s);
    return res;
  }
  WorkSpaceTop = (char *)a + s;
  LOCAL_PrologMode = OldPrologMode;
  return TRUE;
#endif /* YAPOR */
}

int Yap_FreeWorkSpace(void) { return 1; }

#elif USE_SYSTEM_SHM

#if HAVE_SYS_SHM_H
#include <sys/shm.h>
#endif

#ifndef MMAP_ADDR
#define MMAP_ADDR 0x0L
#endif

static MALLOC_T WorkSpaceTop;

static MALLOC_T InitWorkSpace(Int s) {
  MALLOC_T ptr;
  int shm_id;

  /* mapping heap area */
  if ((shm_id = shmget(IPC_PRIVATE, (size_t)s, SHM_R | SHM_W)) == -1) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "could not shmget %d bytes", s);
    return (NULL);
  }
  if ((ptr = (MALLOC_T)shmat(shm_id, (void *)MMAP_ADDR, 0)) == (MALLOC_T)-1) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "could not shmat at %p", MMAP_ADDR);
    return (NULL);
  }
  if (shmctl(shm_id, IPC_RMID, 0) != 0) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "could not remove shm segment",
              shm_id);
    return (NULL);
  }
  WorkSpaceTop = (char *)ptr + s;
  return (ptr);
}

static int ExtendWorkSpace(Int s) {
  MALLOC_T ptr;
  int shm_id;
  prolog_exec_mode OldPrologMode = LOCAL_PrologMode;

  LOCAL_PrologMode = ExtendStackMode;
  /* mapping heap area */
  if ((shm_id = shmget(IPC_PRIVATE, (size_t)s, SHM_R | SHM_W)) == -1) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
    snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "could not shmget %d bytes", s);
    LOCAL_PrologMode = OldPrologMode;
    return (FALSE);
  }
  if ((ptr = (MALLOC_T)shmat(shm_id, WorkSpaceTop, 0)) == (MALLOC_T)-1) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
    snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE, "could not shmat at %p",
              MMAP_ADDR);
    LOCAL_PrologMode = OldPrologMode;
    return (FALSE);
  }
  if (shmctl(shm_id, IPC_RMID, 0) != 0) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
    snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "could not remove shm segment", shm_id);
    LOCAL_PrologMode = OldPrologMode;
    return (FALSE);
  }
  WorkSpaceTop = (char *)ptr + s;
  LOCAL_PrologMode = OldPrologMode;
  return (TRUE);
}

int Yap_FreeWorkSpace(void) { return TRUE; }

#elif USE_SBRK

/***********************************************************************\
* Worspace allocation based on 'sbrk'					*
*   We have to provide a replacement for the 'malloc' functions.        *
*   The situation is further complicated by the need to provide         *
* temporary 'malloc' space when restoring a previously saved state.	*
\***********************************************************************/

#ifdef _AIX
char *sbrk(int);

#endif

int in_limbo; /* non-zero when restoring a saved state */

#ifndef LIMBO_SIZE
#define LIMBO_SIZE 32 * K
#endif

static char limbo_space[LIMBO_SIZE]; /* temporary malloc space */
static char *limbo_p = limbo_space, *limbo_pp = 0;

static MALLOC_T InitWorkSpace(Int s) {
  MALLOC_T ptr = (MALLOC_T)sbrk(s);

  if (ptr == ((MALLOC_T)-1)) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "could not allocate %d bytes", s);
    return (NULL);
  }
  return (ptr);
}

static int ExtendWorkSpace(Int s) {
  MALLOC_T ptr = (MALLOC_T)sbrk(s);
  prolog_exec_mode OldPrologMode = LOCAL_PrologMode;

  LOCAL_PrologMode = ExtendStackMode;
  if (ptr == ((MALLOC_T)-1)) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
    snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "could not expand stacks over %d bytes", s);
    LOCAL_PrologMode = OldPrologMode;
    return (FALSE);
  }
  LOCAL_PrologMode = OldPrologMode;
  return TRUE;
}

int Yap_FreeWorkSpace(void) { return TRUE; }

MALLOC_T
malloc(size_t size) {
  if (in_limbo) {
    limbo_pp = limbo_p;
    limbo_p += (size + 7) & 0xffff8;
    if (limbo_p >= &limbo_space[LIMBO_SIZE])
      return (NULL);
    return (limbo_pp);
  } else {
    CODEADDR codep = (CODEADDR)AllocCodeSpace(size + 2 * sizeof(void *));
    if (codep == NIL)
      return (NIL);
    else
      return (codep + 2 * sizeof(void *));
  }
}

void free(MALLOC_T ptr) {
  BlockHeader *b = (BlockHeader *)(((char *)ptr) - 2 * sizeof(void *) -
                                   sizeof(YAP_SEG_SIZE));

  if (ptr == limbo_pp) {
    limbo_p = limbo_pp;
    return;
  }
  if (!ptr)
    return;
  if ((char *)ptr < Yap_HeapBase || (char *)ptr > HeapTop)
    return;
  if (!(b->b_size & InUseFlag))
    return;
  FreeCodeSpace((char *)ptr - 2 * sizeof(void *));
}

MALLOC_T
XX realloc(MALLOC_T ptr, size_t size) {
  MALLOC_T new = malloc(size);

  if (ptr)
    memmove(new, ptr, size);
  free(ptr);
  return (new);
}

MALLOC_T
calloc(size_t n, size_t e) {
  unsigned k = n * e;
  MALLOC_T p = malloc(k);

  memset(p, 0, k);
  return (p);
}

#ifdef M_MXFAST
int mallopt(cmd, value) { return (value); }

static struct mallinfo xmall;

struct mallinfo mallinfo(void) {
  return (xmall);
}
#endif

#else

/* use malloc to initiliase memory */

/* user should ask for a lot of memory first */

#ifdef __linux
#define MAX_SPACE 420 * 1024 * 1024
#else
#define MAX_SPACE 128 * 1024 * 1024
#endif

static int total_space;

static MALLOC_T InitWorkSpace(Int s) {
  MALLOC_T ptr;

#ifdef M_MMAP_MAX
  mallopt(M_MMAP_MAX, 0);
#endif
  ptr = (MALLOC_T)calloc(MAX_SPACE, 1);
  total_space = s;

  if (ptr == NULL) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "could not allocate %d bytes", s);
    return (NULL);
  }
  return (ptr);
}

static int ExtendWorkSpace(Int s) {
  MALLOC_T ptr;
  prolog_exec_mode OldPrologMode = LOCAL_PrologMode;

  LOCAL_PrologMode = ExtendStackMode;
  total_space += s;
  if (total_space < MAX_SPACE)
    return TRUE;
  ptr = (MALLOC_T)realloc((void *)Yap_HeapBase, total_space);
  if (ptr == NULL) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
    snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "could not allocate %d bytes", s);
    LOCAL_PrologMode = OldPrologMode;
    return FALSE;
  }
  if (ptr != (MALLOC_T)Yap_HeapBase) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
    snprintf4(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "could not expand contiguous stacks  %d bytes", s);
    LOCAL_PrologMode = OldPrologMode;
    return FALSE;
  }
#if MBIT
  if ((CELL)ptr & MBIT) {
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
    snprintf5(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE,
              "memory at %p conflicts with MBIT %lx", ptr, (unsigned long)MBIT);
    LOCAL_PrologMode = OldPrologMode;
    return FALSE;
  }
#endif
  LOCAL_PrologMode = OldPrologMode;
  return TRUE;
}

int Yap_FreeWorkSpace(void) { return TRUE; }
#endif

static void InitHeap(void *heap_addr) {
  /* allocate space */
  Yap_HeapBase = heap_addr;

  /* reserve space for specially allocated functors and atoms so that
     their values can be known statically */
  HeapTop = Yap_HeapBase + AdjustSize(sizeof(all_heap_codes));
  Yap_HoleSize = 0;
#if USE_DL_MALLOC
  Yap_initdlmalloc();
#else
  HeapMax = HeapUsed = HeapTop - Yap_HeapBase;
  /* notice that this forces odd addresses */
  *((YAP_SEG_SIZE *)HeapTop) = InUseFlag;
  HeapTop = HeapTop + sizeof(YAP_SEG_SIZE);
  *((YAP_SEG_SIZE *)HeapTop) = InUseFlag;
#endif

  FreeBlocks = NIL;
}

void Yap_InitHeap(void *heap_addr) { InitHeap(heap_addr); }

void Yap_InitMemory(UInt Trail, UInt Heap, UInt Stack) {
  UInt pm, sa, ta;
  void *addr;

#if defined(_WIN32) || defined(__CYGWIN__)
  Stack = ((Stack + (YAP_ALLOC_SIZE - 1)) / YAP_ALLOC_SIZE) * YAP_ALLOC_SIZE;
  Heap = ((Heap + (YAP_ALLOC_SIZE - 1)) / YAP_ALLOC_SIZE) * YAP_ALLOC_SIZE;
  Trail = ((Trail + (YAP_ALLOC_SIZE - 1)) / YAP_ALLOC_SIZE) * YAP_ALLOC_SIZE;
#endif
  pm = (Trail + Heap + Stack); /* memory to be
                                * requested         */
  sa = Stack; /* stack area size   */
  ta = Trail; /* trail area size   */

#if RANDOMIZE_START_ADDRESS
  srand(time(NULL));
  UInt x = (rand() % 100) * YAP_ALLOC_SIZE;
  pm += x;
#endif
  addr = InitWorkSpace(pm);
#if RANDOMIZE_START_ADDRESS
  addr = (char *)addr + x;
  pm -= x;
#endif

  InitHeap(addr);

  LOCAL_TrailTop = Yap_HeapBase + pm;
  LOCAL_LocalBase = LOCAL_TrailTop - ta;
  LOCAL_TrailBase = LOCAL_LocalBase + sizeof(CELL);

  LOCAL_GlobalBase = LOCAL_LocalBase - sa;
  HeapLim = LOCAL_GlobalBase; /* avoid confusions while
                               * * restoring */
#if !USE_DL_MALLOC
  AuxTop = (ADDR)(AuxSp = (CELL *)LOCAL_GlobalBase);
#endif

#if DEBUG
#if SIZEOF_INT_P != SIZEOF_INT
  if (Yap_output_msg) {
    fprintf(stderr,
            "HeapBase = %p  GlobalBase = %p\n  LocalBase = %p  TrailTop = %p\n",
            Yap_HeapBase, LOCAL_GlobalBase, LOCAL_LocalBase, LOCAL_TrailTop);
#else
  if (Yap_output_msg) {
    fprintf(stderr,
            "HeapBase = %x  GlobalBase = %x\n  LocalBase = %x  TrailTop = %x\n",
            (UInt)Yap_HeapBase, (UInt)LOCAL_GlobalBase, (UInt)LOCAL_LocalBase,
            (UInt)LOCAL_TrailTop);
#endif

    fprintf(stderr,
            "Heap+Aux: " UInt_FORMAT "\tLocal+Global: " UInt_FORMAT
            "\tTrail: " UInt_FORMAT "\n",
            pm - sa - ta, sa, ta);
  }
#endif /* DEBUG */
}

void Yap_InitExStacks(int wid, int Trail, int Stack) {
#if USE_DL_MALLOC
  REMOTE_ScratchPad(wid).ptr = NULL;
  REMOTE_ScratchPad(wid).sz = REMOTE_ScratchPad(wid).msz = SCRATCH_START_SIZE;
  AuxSp = NULL;
#endif
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#if defined(_WIN32) || defined(__CYGWIN__)
#define WorkSpaceTop brk
#define MAP_FIXED 1
#endif

#if !USE_DL_MALLOC
/* dead code */
void Yap_add_memory_hole(ADDR Start, ADDR End) { Yap_HoleSize += Start - End; }
#endif

int Yap_ExtendWorkSpace(Int s) {
#if USE_SYSTEM_MMAP
  return ExtendWorkSpace(s, MAP_FIXED);
#elif defined(_WIN32)
  return ExtendWorkSpace(s, MAP_FIXED);
#else
  return ExtendWorkSpace(s);
#endif
}

size_t Yap_ExtendWorkSpaceThroughHole(size_t s) {
#if USE_SYSTEM_MMAP || defined(_WIN32) || defined(__CYGWIN__)
  MALLOC_T WorkSpaceTop0 = WorkSpaceTop;
#if SIZEOF_INT_P == 4
  while (WorkSpaceTop < (MALLOC_T)0xc0000000L) {
    /* progress 1 MB */
    WorkSpaceTop += 512 * 1024;
    if (ExtendWorkSpace(s, MAP_FIXED)) {
      Yap_add_memory_hole((ADDR)WorkSpaceTop0, (ADDR)WorkSpaceTop - s);
      LOCAL_ErrorMessage = NULL;
      return WorkSpaceTop - WorkSpaceTop0;
    }
#if defined(_WIN32)
    /* 487 happens when you step over someone else's memory */
    if (GetLastError() != 487) {
      WorkSpaceTop = WorkSpaceTop0;
      return 0;
    }
#endif
#elif SIZEOF_INT_P == 8
  {
    int n = 1024 * 1024;
    while (n--) {
      /* progress 1 MB */
      WorkSpaceTop += 512 * 1024;
      if (ExtendWorkSpace(s, MAP_FIXED)) {
        Yap_add_memory_hole((ADDR)WorkSpaceTop0, (ADDR)WorkSpaceTop - s);
        LOCAL_ErrorMessage = NULL;
        return WorkSpaceTop - WorkSpaceTop0;
      }
#if defined(_WIN32)
      /* 487 happens when you step over someone else's memory */
xx        WorkSpaceTop = WorkSpaceTop0;
        return 0;
      }
#endif
    }
#endif
  }
  WorkSpaceTop = WorkSpaceTop0;
#endif
  return 0;
}

void Yap_AllocHole(UInt actual_request, UInt total_size) {
#if (USE_SYSTEM_MMAP || defined(_WIN32) || defined(__CYGWIN__)) &&             \
    !USE_DL_MALLOC
  /* where we were when the hole was created,
   also where is the hole store */
  ADDR WorkSpaceTop0 = WorkSpaceTop - total_size;
  BlockHeader *newb = (BlockHeader *)HeapTop;
  BlockHeader *endb = (BlockHeader *)(WorkSpaceTop0 - sizeof(YAP_SEG_SIZE));
  YAP_SEG_SIZE bsiz = (WorkSpaceTop0 - HeapTop) / sizeof(CELL) -
                      2 * sizeof(YAP_SEG_SIZE) / sizeof(CELL);

  /* push HeapTop to after hole */
  HeapTop = WorkSpaceTop - (actual_request - sizeof(YAP_SEG_SIZE));
  ((YAP_SEG_SIZE *)HeapTop)[0] = InUseFlag;
  /* now simulate a block */
  ((YAP_SEG_SIZE *)HeapTop)[-1] = endb->b_size =
      (HeapTop - WorkSpaceTop0) / sizeof(YAP_SEG_SIZE) | InUseFlag;
  newb->b_size = bsiz;
  AddToFreeList(newb);
#endif
}



 
#endif /* USE_SYSTEM_MALLOC */

 /**
 * @defgroup shortalloc.c
 *
 * support for short-lived memory allocation. Either using a stack discipline,
 * or temporary buffers.
 *
 */
#include "Yap.h"
#include "YapEval.h"
#include "YapHeap.h"
#include "YapStreams.h"
#include "YapText.h"
#include "Yatom.h"
#include "yapio.h"

#include <YapText.h>
#include <string.h>
#include <wchar.h>


/**
 * @addtogroup  StackDisc
 * @ ingroup @MemAlloc
 *
 */
#define MAX_PATHNAME 2048

struct mblock {
  struct mblock *prev, *next;
  int lvl;
  size_t sz;
};

typedef struct TextBuffer_manager {
  void *buf, *ptr;
  size_t sz;
  struct mblock *first[16];
  struct mblock *last[16];
  int lvl;
} text_buffer_t;

int AllocLevel(void) { return LOCAL_TextBuffer->lvl; }
//	void pop_text_stack(int i) { LOCAL_TextBuffer->lvl = i; }
void insert_block(struct mblock *o) {
  int lvl = o->lvl;
  o->prev = LOCAL_TextBuffer->last[lvl];
  if (o->prev) {
    o->prev->next = o;
  }
  if (LOCAL_TextBuffer->first[lvl]) {
    LOCAL_TextBuffer->last[lvl] = o;
  } else {
    LOCAL_TextBuffer->first[lvl] = LOCAL_TextBuffer->last[lvl] = o;
  }
  o->next = NULL;
}

 void release_block(struct mblock *o) {
  int lvl = o->lvl;
  if (LOCAL_TextBuffer->first[lvl] == o) {
    if (LOCAL_TextBuffer->last[lvl] == o) {
      LOCAL_TextBuffer->first[lvl] = LOCAL_TextBuffer->last[lvl] = NULL;
    }
    LOCAL_TextBuffer->first[lvl] = o->next;
  } else if (LOCAL_TextBuffer->last[lvl] == o) {
    LOCAL_TextBuffer->last[lvl] = o->prev;
  }
  if (o->prev)
    o->prev->next = o->next;
  if (o->next)
    o->next->prev = o->prev;
}

int push_text_stack__(USES_REGS1) {
  int i = LOCAL_TextBuffer->lvl;
  i++;
  LOCAL_TextBuffer->lvl = i;

  return i;
}

int pop_text_stack__(int i) {
  int lvl = LOCAL_TextBuffer->lvl;
  while (lvl >= i) {
    struct mblock *p = LOCAL_TextBuffer->first[lvl];
    while (p) {
      struct mblock *np = p->next;
      free(p);
      p = np;
    }
    LOCAL_TextBuffer->first[lvl] = NULL;
    LOCAL_TextBuffer->last[lvl] = NULL;
    lvl--;
  }
  LOCAL_TextBuffer->lvl = lvl;
  return lvl;
}

void *pop_output_text_stack__(int i, const void *export) {
  int lvl = LOCAL_TextBuffer->lvl;
  bool found = false;
  while (lvl >= i) {
    struct mblock *p = LOCAL_TextBuffer->first[lvl];
    while (p) {
      struct mblock *np = p->next;
      if (p + 1 == export) {
	found = true;
      } else {
        free(p);
      }
      p = np;
    }
    LOCAL_TextBuffer->first[lvl] = NULL;
    LOCAL_TextBuffer->last[lvl] = NULL;
    lvl--;
  }
  LOCAL_TextBuffer->lvl = lvl;
  if (found) {
  if (lvl) {
    struct mblock *o = (struct mblock *)export-1;
    o->lvl = lvl;
    o->prev = o->next = 0;
    insert_block(o);

  } else {
        struct mblock *p = (struct mblock *)export-1;
	size_t sz = p->sz - sizeof(struct mblock);
        memmove(p, p + 1, sz);
        export = p;
    
  }
  }
  return (void *)export;
}

void *Malloc(size_t sz USES_REGS) {
  int lvl = LOCAL_TextBuffer->lvl;
  if (sz == 0)
    sz = 1024;
  sz = ALIGN_BY_TYPE(sz + sizeof(struct mblock), CELL);
  struct mblock *o = malloc(sz);
  if (!o)
    return NULL;
  o->sz = sz;
  o->lvl = lvl;
  o->prev = o->next = 0;
  insert_block(o);
  return o + 1;
}

void *MallocAtLevel(size_t sz, int atL USES_REGS) {
  int lvl = LOCAL_TextBuffer->lvl;
  if (atL > 0 && atL <= lvl) {
    lvl = atL;
  } else if (atL < 0 && lvl - atL >= 0) {
    lvl += atL;
  } else {
    return NULL;
  }
  if (sz == 0)
    sz = 1024;
  sz = ALIGN_BY_TYPE(sz + sizeof(struct mblock), CELL);
  struct mblock *o = malloc(sz);
  if (!o)
    return NULL;
  o->sz = sz;
  o->lvl = lvl;
  o->prev = o->next = 0;
  insert_block(o);
  return o + 1;
}

void *Realloc(void *pt, size_t sz USES_REGS) {
  struct mblock *old = pt, *o;
  if (!pt)
    return Malloc(sz PASS_REGS);
  old--;
  sz = ALIGN_BY_TYPE(sz, Yap_Max(CELLSIZE,sizeof(struct mblock)));
  sz += 2*sizeof(struct mblock);
  o = realloc(old, sz);
  if (o->next) {
    o->next->prev = o;
  } else {
    LOCAL_TextBuffer->last[o->lvl] = o;
  }
  if (o->prev) {
    o->prev->next = o;
  } else {
    LOCAL_TextBuffer->first[o->lvl] = o;
  }
  o->sz = sz;
  return o + 1;
}

/**
 * Export a local memory object as a RO object to the outside world, that is,
 * recovering as much storage as one can.
 * @param pt pointer to object
 * @return new object
 */
const void *MallocExportAsRO(const void *pt USES_REGS) {
  struct mblock *old = (void *)pt, *o = old - 1;
  if (old == NULL)
    return NULL;
  size_t sz = o->sz;
  release_block(o);
  memmove((void *)o, pt, sz);
  return realloc((void *)o, sz);
}

void Free(void *pt USES_REGS) {
  struct mblock *o = pt;
  o--;
  release_block(o);
  free(o);
}

void *Yap_InitTextAllocator(void) {
  struct TextBuffer_manager *new = calloc(sizeof(struct TextBuffer_manager), 1);
  return new;
}


 bool Yap_get_scratch_buf(scratch_struct_t *handle, size_t nof, size_t each) {
   handle->n_of = nof;
   handle->size_of = each;
   if (!handle->data) {
     if (LOCAL_WorkerBuffer.data && !LOCAL_WorkerBuffer.in_use) {
       if (LOCAL_WorkerBuffer.in_use < nof*each) {
	 LOCAL_WorkerBuffer.data = realloc( LOCAL_WorkerBuffer.data, nof*each);
	 LOCAL_WorkerBuffer.sz =  nof*each;
       }
       LOCAL_WorkerBuffer.in_use =  true;
       handle->data =  LOCAL_WorkerBuffer.data;
       handle->is_thread_scratch_buf = true;
      return true;
     }
   }
   handle->data = malloc(nof*each);
   handle->is_thread_scratch_buf = false;
   return true;
 }


bool Yap_realloc_scratch_buf(scratch_struct_t *handle, size_t nof) {
  handle->n_of = nof;
    handle->data = realloc(handle->data, handle->size_of*nof);
   return true;
 }

   bool Yap_release_scratch_buf(scratch_struct_t *handle) {
     if (handle->is_thread_scratch_buf &&
	 handle->data == LOCAL_WorkerBuffer.data) {
       LOCAL_WorkerBuffer.in_use= false;
     } else {
       free(handle->data);
     }
     return true;
 }
