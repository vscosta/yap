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
* File:		alloc.c							 *
* Last rev:								 *
* mods:									 *
* comments:	allocating space					 *
* version:$Id: alloc.c,v 1.16 2002-02-26 17:41:53 vsc Exp $		 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";

#endif

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "alloc.h"
#include "yapio.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_MEMORY_H
#include <memory.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <stdio.h>

#if __simplescalar__
#ifdef USE_MMAP
#undef USE_MMAP
#endif
#ifdef USE_SBRK
#undef USE_SBRK
#endif
#endif

STATIC_PROTO(void FreeBlock, (BlockHeader *));
STATIC_PROTO(BlockHeader *GetBlock, (unsigned int));
STATIC_PROTO(char *AllocHeap, (unsigned int));
STATIC_PROTO(void RemoveFromFreeList, (BlockHeader *));
STATIC_PROTO(void AddToFreeList, (BlockHeader *));

#ifdef LIGHT
#include <stdlib.h>
#endif

#define K		((Int) 1024)

#define MinHGap   256*K

/************************************************************************/
/* Yap workspace management                                             */

int
SizeOfBlock(CODEADDR p)
{
  BlockHeader *b = (BlockHeader *) (p - sizeof(YAP_SEG_SIZE));
  YAP_SEG_SIZE s = (b->b_size) & ~InUseFlag;
  return ((s - 1) * sizeof(YAP_SEG_SIZE));
}

static void
RemoveFromFreeList(BlockHeader *b)
{
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
  }
  else {
    BlockHeader **q = &FreeBlocks;

    while ((*q) != b)
      q = &((*q)->b_next_size);
    if (b->b_next) {
      p = b->b_next;
      *q = p;
      p->b_next_size = b->b_next_size;
    }
    else {
      *q = b->b_next_size;
    }
  }
}

static void
AddToFreeList(BlockHeader *b)
{
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
  }
  else {
    b->b_next = NIL;
    b->b_next_size = p;
  }
  *q = b;
}

long int call_counter;

static void
FreeBlock(BlockHeader *b)
{
  BlockHeader *p;
  YAP_SEG_SIZE *sp;

  /* sanity check */
  sp = &(b->b_size) + (b->b_size & ~InUseFlag);
  if (*sp != b->b_size) {
#if !SHORT_INTS
    YP_fprintf(YP_stderr, "** sanity check failed in FreeBlock %p %x %x\n",
	       b, b->b_size, Unsigned(*sp));
#else
    YP_fprintf(YP_stderr, "** sanity check failed in FreeBlock %p %lx %lx\n",
	       b, b->b_size, *sp);
#endif
    return;
  }
  b->b_size &= ~InUseFlag;
  LOCK(FreeBlocksLock);
  LOCK(GLOBAL_LOCKS_alloc_block);
  /* check if we can collapse with other blocsks */
  /* check previous */
  sp = &(b->b_size) - 1;
  if (!(*sp & InUseFlag)) {	/* previous block is free */
    p = (BlockHeader *) (sp - *sp);
    RemoveFromFreeList(p);
    p->b_size += b->b_size + 1;
    b = p;
  }
  /* check following */
  sp = &(b->b_size) + b->b_size + 1;
  if (!(*sp & InUseFlag)) {	/* following block is free */
    p = (BlockHeader *) sp;
    RemoveFromFreeList(p);
    b->b_size += p->b_size + 1;
  }
  /* insert on list of free blocks */
  AddToFreeList(b);
  UNLOCK(GLOBAL_LOCKS_alloc_block);
  UNLOCK(FreeBlocksLock);
}

static BlockHeader *
GetBlock(unsigned int n)
{				/* get free block with size at least n */
  register BlockHeader **p, *b, *r;

  if (FreeBlocks == NIL)
    return (NIL);
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

static char *
AllocHeap(unsigned int size)
{
  BlockHeader *b, *n;
  YAP_SEG_SIZE *sp;

#if SIZEOF_INT_P==4
  size = (((size + 7) & 0xffffff8) >> 2) + 2;	/* size in dwords + 2 */
#endif
#if SIZEOF_INT_P==8
  size = (((size + 7) & 0xffffff8) >> 3) + 2;	/* size in dwords + 2 */
#endif
  if (size < 6)
    size = 6;
  LOCK(FreeBlocksLock);
  LOCK(GLOBAL_LOCKS_alloc_block);
  if ((b = GetBlock(size))) {
    if (b->b_size >= size + 6 + 1) {
      n = (BlockHeader *) (((YAP_SEG_SIZE *) b) + size + 1);
      n->b_size = b->b_size - size - 1;
      b->b_size = size;
      AddToFreeList(n);
    }
    sp = &(b->b_size) + b->b_size;
    *sp = b->b_size | InUseFlag;
    b->b_size |= InUseFlag;
    UNLOCK(GLOBAL_LOCKS_alloc_block);
    UNLOCK(FreeBlocksLock);
    return (Addr(b) + sizeof(YAP_SEG_SIZE));
  }
  UNLOCK(FreeBlocksLock);
  if (!HEAPTOP_OWNER(worker_id)) {
    LOCK(HeapTopLock);
  }
  b = (BlockHeader *) HeapTop;
  HeapTop += size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
  LOCK(HeapUsedLock);
  HeapUsed += size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);

#ifdef YAPOR
  if (HeapTop > Addr(GlobalBase) - MinHeapGap) {
    abort_optyap("No heap left in function AllocHeap");
  }
#else
  if (HeapTop > Addr(AuxSp) - MinHeapGap) {
    HeapTop -= size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
    HeapUsed -= size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
    if (HeapTop > Addr(AuxSp)) {
      UNLOCK(HeapUsedLock);
      if (!HEAPTOP_OWNER(worker_id)) {
	UNLOCK(HeapTopLock);
      }
      /* we destroyed the stack */
      Error(SYSTEM_ERROR, TermNil, "Stack Crashed against Heap...");
      return(NULL);
    } else {
      if (HeapTop + size * sizeof(CELL) + sizeof(YAP_SEG_SIZE) < Addr(AuxSp)) {
	/* small allocations, we can wait */
	HeapTop += size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
	HeapUsed += size * sizeof(CELL) + sizeof(YAP_SEG_SIZE);
	UNLOCK(HeapUsedLock);
	if (!HEAPTOP_OWNER(worker_id)) {
	  UNLOCK(HeapTopLock);
	}
	CreepFlag = Unsigned(LCL0) - Unsigned(H0);
      } else {
	if (size > SizeOfOverflow)
	  SizeOfOverflow = size*sizeof(CELL) + sizeof(YAP_SEG_SIZE);
	/* big allocations, the caller must handle the problem */
	UNLOCK(HeapUsedLock);
	if (!HEAPTOP_OWNER(worker_id)) {
	  UNLOCK(HeapTopLock);
	}
	return(NULL);
      }
    }
  }
#endif /* YAPOR */
  *((YAP_SEG_SIZE *) HeapTop) = InUseFlag;
  if (HeapUsed > HeapMax)
    HeapMax = HeapUsed;
  HeapPlus = HeapTop + MinHGap / CellSize;
  UNLOCK(GLOBAL_LOCKS_alloc_block);
  UNLOCK(HeapUsedLock);
  b->b_size = size | InUseFlag;
  sp = &(b->b_size) + size;
  *sp = b->b_size;
  if (!HEAPTOP_OWNER(worker_id)) {
    UNLOCK(HeapTopLock);
  }
  return (Addr(b) + sizeof(YAP_SEG_SIZE));
}

/* If you need to dinamically allocate space from the heap, this is
 * the macro you should use */
ADDR
PreAllocCodeSpace(void)
{
  LOCK(HeapTopLock);
  HEAPTOP_OWN(worker_id);
  return (Addr(HeapTop) + sizeof(YAP_SEG_SIZE));
}

#if defined(YAPOR) || defined(THREADS)
/* Grabbing the HeapTop is an excellent idea for a sequential system,
   but does work as well in parallel systems. Anyway, this will do for now */
void
ReleasePreAllocCodeSpace(ADDR ptr)
{
  HEAPTOP_DISOWN(worker_id);
  UNLOCK(HeapTopLock);
}
#endif

/* If you need to dinamically allocate space from the heap, this is
 * the macro you should use */
void
FreeCodeSpace(char *p)
{
  FreeBlock(((BlockHeader *) (p - sizeof(YAP_SEG_SIZE))));
}

char *
AllocAtomSpace(unsigned int size)
{
  return (AllocHeap(size));
}

void
FreeAtomSpace(char *p)
{
  FreeCodeSpace(p);
}

char *
AllocCodeSpace(unsigned int size)
{
  if (size < SmallSize + 2 * OpCodeSize + 3 * CellSize)
    return (AllocHeap(SmallSize + 2 * OpCodeSize + 3 * CellSize));
  return (AllocHeap(size));
}

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
/*   int FreeWorkSpace() - release workspace                            */
/************************************************************************/

#ifdef _WIN32

#include "windows.h"

#define BASE_ADDRESS  ((LPVOID) MMAP_ADDR)
#define MAX_WORKSPACE 0x20000000L

static LPVOID brk;

int
ExtendWorkSpace(Int s)
{
  LPVOID b;
  s = ((s-1)/page_size+1)*page_size;
  b = VirtualAlloc(brk, s, MEM_COMMIT, PAGE_READWRITE);
  if (b) {
  	brk = (LPVOID) ((Int) brk + s);
	return TRUE;
  }
  return FALSE;
}

MALLOC_T
InitWorkSpace(Int s)
{
  SYSTEM_INFO si;
  LPVOID b;

  GetSystemInfo(&si);
  page_size = si.dwPageSize;
  b = VirtualAlloc(BASE_ADDRESS, MAX_WORKSPACE, MEM_RESERVE, PAGE_NOACCESS);
  if (b==NULL) {
    YP_fprintf(YP_stderr,"[ Warning: YAP reserving space at a variable address ]\n");
    b = VirtualAlloc(0x0, MAX_WORKSPACE, MEM_RESERVE, PAGE_NOACCESS);
    if (b == NULL) {
      YP_fprintf(YP_stderr,"[ FATAL ERROR: YAP failed to reserve space ]\n");
      exit(1);
    }
  }
  brk = BASE_ADDRESS;

  if (ExtendWorkSpace(s)) {
    return BASE_ADDRESS;
  } else {
    YP_fprintf(YP_stderr,"[ FATAL ERROR: YAP failed to reserve space ]\n");
    exit(1);
  }
}

int
FreeWorkSpace(void)
{
  return TRUE;
}

#elif USE_MMAP

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

static MALLOC_T WorkSpaceTop;

MALLOC_T
InitWorkSpace(Int s)
{
  MALLOC_T a;
#if !defined(_AIX) && !defined(__APPLE__) &&  !__hpux
  int fd;
#endif

#if defined(_AIX)
  a = mmap(0, (size_t) s, PROT_READ | PROT_WRITE | PROT_EXEC,
	   MAP_PRIVATE | MAP_ANONYMOUS | MAP_VARIABLE, -1, 0);
#elif __hpux
  a = mmap(((void *)MMAP_ADDR), (size_t) s, PROT_READ | PROT_WRITE | PROT_EXEC,
	   MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
  if (a != (MALLOC_T)MMAP_ADDR) {
    Error(SYSTEM_ERROR, TermNil, "mmap could not map ANON at %p, got %p", (void *)MMAP_ADDR, a);
    return(NULL);
  }
#elif defined(__APPLE__)
  a = mmap(((void *)MMAP_ADDR), (size_t) s, PROT_READ | PROT_WRITE | PROT_EXEC,
	   MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
  if (a != (MALLOC_T)MMAP_ADDR) {
    Error(SYSTEM_ERROR, TermNil, "mmap could not map ANON at %p, got %p", (void *)MMAP_ADDR,a );
    return(NULL);
  }
#else
  fd = open("/dev/zero", O_RDWR);
  if (fd < 0) {
#if HAVE_MKSTEMP
    char file[256];
    strncpy(file,"/tmp/YAP.TMPXXXXXX", 256);
    if (mkstemp(file) == -1) {
#if HAVE_STRERROR
      Error(SYSTEM_ERROR, TermNil, "mkstemp could not create temporary file %s (%s)", file, strerror(errno));
#else
      Error(SYSTEM_ERROR, TermNil, "mkstemp could not create temporary file %s", file);
#endif
      return NULL;
    }
#else
#if HAVE_TMPNAM
    char *file = tmpnam(NULL);
#else
    char file[YAP_FILENAME_MAX];
    strcpy(file,"/tmp/mapfile");
    itos(getpid(), &file[12]);
#endif /* HAVE_TMPNAM */
#endif /* HAVE_MKSTEMP */
    fd = open(file, O_CREAT|O_RDWR);
    if (fd < 0) {
      Error(SYSTEM_ERROR, TermNil, "mmap could not open %s", file);
      return NULL;
    }
    if (lseek(fd, s, SEEK_SET) < 0) {
      Error(SYSTEM_ERROR, TermNil, "mmap could not lseek in mmapped file %s", file);
      return FALSE;
    }
    if (write(fd, "", 1) < 0) {
      Error(SYSTEM_ERROR, TermNil, "mmap could not write in mmapped file %s", file);
      return NULL;
    }
    if (unlink(file) < 0) {
      Error(SYSTEM_ERROR,TermNil, "mmap could not unlink mmapped file %s", file);
      return NULL;
    }
  }
#if USE_FIXED
  a = mmap(((void *)MMAP_ADDR), (size_t) s, PROT_READ | PROT_WRITE | PROT_EXEC,
	   MAP_PRIVATE | MAP_FIXED, fd, 0);
  if (a != (MALLOC_T)MMAP_ADDR) {
    Error(SYSTEM_ERROR, TermNil, "mmap could not map at %p, got %p", (void *)MMAP_ADDR, a);
    return NULL;
  }
#else
  a = mmap(0, (size_t) s, PROT_READ | PROT_WRITE | PROT_EXEC,
	   MAP_PRIVATE, fd, 0);
  if ((CELL)a & YAP_PROTECTED_MASK) {
    close(fd);
    Error(FATAL_ERROR, TermNil, "mmapped address %p collides with YAP tags", a);
    return NULL;
  }
  if (close(fd) == -1) {
    Error(SYSTEM_ERROR, TermNil, "while closing mmaped file");
    return NULL;
  }
#endif
#endif
  if
#ifdef MMAP_FAILED
	 (a == (MALLOC_T) MMAP_FAILED)
#else
	 (a == (MALLOC_T) - 1)
#endif
    {
      Error(FATAL_ERROR, TermNil, "mmap cannot allocate memory ***");
      return(NULL);
    }
  WorkSpaceTop = (char *) a + s;
  return (void *) a;
}

int
ExtendWorkSpace(Int s)
{
#ifdef YAPOR
  abort_optyap("function ExtendWorkSpace called");
  return(FALSE);
#else

  MALLOC_T a;

#if defined(_AIX) || defined(__hpux)
  a = mmap(WorkSpaceTop, (size_t) s, PROT_READ | PROT_WRITE | PROT_EXEC,
		    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

#elif defined(__APPLE__)
  a = mmap(WorkSpaceTop, (size_t) s, PROT_READ | PROT_WRITE | PROT_EXEC,
		    MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
#else
  int fd;
  fd = open("/dev/zero", O_RDWR);
  if (fd < 0) {
#if HAVE_MKSTEMP
    char file[256];
    strncpy(file,"/tmp/YAP.TMPXXXXXX",256);
    if (mkstemp(file) == -1) {
#if HAVE_STRERROR
      Error(SYSTEM_ERROR, TermNil, "mkstemp could not create temporary file %s (%s)", file, strerror(errno));
#else
      Error(SYSTEM_ERROR, TermNil, "mkstemp could not create temporary file %s", file);
#endif /* HAVE_STRERROR */
      return FALSE;
    }
#else
#if HAVE_TMPNAM
    char *file = tmpnam(NULL);
#else
    char file[YAP_FILENAME_MAX];
    strcpy(file,"/tmp/mapfile");
    itos(getpid(), &file[12]);
#endif /* HAVE_TMPNAM */
#endif /* HAVE_MKSTEMP */
    fd = open(file, O_CREAT|O_RDWR);
    if (fd < 0) {
      Error(SYSTEM_ERROR, TermNil, "mmap could not open %s", file);
      return FALSE;
    }
    if (lseek(fd, s, SEEK_SET) < 0) {
      Error(SYSTEM_ERROR, TermNil, "mmap could not lseek in mmapped file %s", file);
      return FALSE;
    }
    if (write(fd, "", 1) < 0) {
      Error(SYSTEM_ERROR, TermNil, "mmap could not write in mmapped file %s", file);
      return FALSE;
    }
    if (unlink(file) < 0) {
      Error(SYSTEM_ERROR, TermNil, "mmap could not unlink mmapped file %s", file);
      return FALSE;
    }
  }
  a = mmap(WorkSpaceTop, (size_t) s, PROT_READ | PROT_WRITE | PROT_EXEC,
		    MAP_PRIVATE | MAP_FIXED, fd, 0);
  if (close(fd) == -1) {
#if HAVE_STRERROR
    Error(SYSTEM_ERROR, TermNil, "mmap could not close file (%s) ]\n", strerror(errno));
#else
    Error(SYSTEM_ERROR, TermNil, "mmap could not close file ]\n");
#endif
    return FALSE;
  }
#endif
  if (a == (MALLOC_T) - 1) {
#if HAVE_STRERROR
    Error(SYSTEM_ERROR, TermNil, "could not allocate %d bytes (%s)", (int)s, strerror(errno));
#else
    Error(SYSTEM_ERROR, TermNil, "could not allocate %d bytes", (int)s);
#endif
    return FALSE;
  }
  if (a != WorkSpaceTop) {
    Error(SYSTEM_ERROR, TermNil, "mmap could not grow memory at %p, got %p", WorkSpaceTop, a );
    return FALSE;
  }
  
  WorkSpaceTop = (char *) a + s;
  return TRUE;
#endif /* YAPOR */
}

int
FreeWorkSpace(void)
{
  return 1;
}

#elif USE_SHM

#if HAVE_SYS_SHM_H
#include <sys/shm.h>
#endif

#ifndef MMAP_ADDR
#define MMAP_ADDR 0x0L
#endif

static MALLOC_T WorkSpaceTop;

MALLOC_T
InitWorkSpace(Int s)
{
  MALLOC_T ptr;
  int shm_id;

  /* mapping heap area */
  if((shm_id = shmget(IPC_PRIVATE, (size_t)s, SHM_R|SHM_W)) == -1) {
    Error(FATAL_ERROR, TermNil, "could not shmget %d bytes", s);
    return(NULL);
   }
  if((ptr = (MALLOC_T)shmat(shm_id, (void *) MMAP_ADDR, 0)) == (MALLOC_T) -1) {
    Error(FATAL_ERROR, TermNil, "could not shmat at %p", MMAP_ADDR);
    return(NULL);
  }
  if (shmctl(shm_id, IPC_RMID, 0) != 0) {
    Error(FATAL_ERROR, TermNil, "could not remove shm segment", shm_id);
    return(NULL);
  }
  WorkSpaceTop = (char *) ptr + s;
  return(ptr);
}

int
ExtendWorkSpace(Int s)
{
  MALLOC_T ptr;
  int shm_id;

  /* mapping heap area */
  if((shm_id = shmget(IPC_PRIVATE, (size_t)s, SHM_R|SHM_W)) == -1) {
    Error(SYSTEM_ERROR, TermNil, "could not shmget %d bytes", s);
    return(FALSE);
   }
  if((ptr = (MALLOC_T)shmat(shm_id, WorkSpaceTop, 0)) == (MALLOC_T) -1) {
    Error(SYSTEM_ERROR, TermNil, "could not shmat at %p", MMAP_ADDR);
    return(FALSE);
  }
  if (shmctl(shm_id, IPC_RMID, 0) != 0) {
    Error(SYSTEM_ERROR, TermNil, "could not remove shm segment", shm_id);
    return(FALSE);
  }
  WorkSpaceTop = (char *) ptr + s;
  return(TRUE);
}

int
FreeWorkSpace(void)
{
  return TRUE;
}

#elif USE_SBRK

/***********************************************************************\
* Worspace allocation based on 'sbrk'					*
*   We have to provide a replacement for the 'malloc' functions.        *
*   The situation is further complicated by the need to provide         *
* temporary 'malloc' space when restoring a previously saved state.	*
\***********************************************************************/

#ifdef _AIX
char *STD_PROTO(sbrk, (int));

#endif

int in_limbo;		/* non-zero when restoring a saved state */

#ifndef LIMBO_SIZE
#define LIMBO_SIZE 32*K
#endif

static char limbo_space[LIMBO_SIZE];	/* temporary malloc space */
static char *limbo_p = limbo_space, *limbo_pp = 0;

MALLOC_T
InitWorkSpace(Int s)
{
  MALLOC_T ptr = (MALLOC_T)sbrk(s);

  if (ptr == ((MALLOC_T) - 1)) {
     Error(FATAL_ERROR, TermNil, "could not allocate %d bytes", s);
     return(NULL);
  }
  return(ptr);
}

int
ExtendWorkSpace(Int s)
{
  MALLOC_T ptr = (MALLOC_T)sbrk(s);
  if (ptr == ((MALLOC_T) - 1)) {
     Error(SYSTEM_ERROR, TermNil, "could not expand stacks over %d bytes", s);
     return(FALSE);
  }
  return TRUE;
}

int
FreeWorkSpace(void)
{
  return TRUE;
}

MALLOC_T
malloc(size_t size)
{
  if (in_limbo) {
    limbo_pp = limbo_p;
    limbo_p += (size + 7) & 0xffff8;
    if (limbo_p >= &limbo_space[LIMBO_SIZE])
      return(NULL);
    return (limbo_pp);
  }
  else {
    CODEADDR codep = (CODEADDR)AllocCodeSpace(size + 2*sizeof(void *));
    if (codep == NIL)
      return(NIL);
    else
      return(codep + 2*sizeof(void *));
  }
}

void
free(MALLOC_T ptr)
{
  BlockHeader *b = (BlockHeader *) (((char *) ptr) - 2*sizeof(void *) - sizeof(YAP_SEG_SIZE));

  if (ptr == limbo_pp) {
    limbo_p = limbo_pp;
    return;
  }
  if (!ptr)
    return;
  if ((char *) ptr < HeapBase || (char *) ptr > HeapTop)
    return;
  if (!(b->b_size & InUseFlag))
    return;
  FreeCodeSpace((char *) ptr - 2*sizeof(void *));
}

MALLOC_T
realloc(MALLOC_T ptr, size_t size)
{
  MALLOC_T new = malloc(size);

  if (ptr)
    memcpy(new, ptr, size);
  free(ptr);
  return (new);
}

MALLOC_T
calloc(size_t n, size_t e)
{
  unsigned k = n * e;
  MALLOC_T p = malloc(k);

  memset(p, 0, k);
  return (p);
}

#ifdef M_MXFAST
int
mallopt(cmd, value)
{
  return (value);
}

static struct mallinfo xmall;

struct mallinfo
mallinfo(void)
{
  return (xmall);
}
#endif

#else

/* use malloc to initiliase memory */

/* user should ask for a lot of memory first */

#define MAX_SPACE 128*1024*1024

static int total_space;

MALLOC_T
InitWorkSpace(Int s)
{
  MALLOC_T ptr = (MALLOC_T)malloc(MAX_SPACE);
  total_space = s;

  if (ptr == ((MALLOC_T) - 1)) {
     Error(FATAL_ERROR, TermNil, "could not allocate %d bytes", s);
     return(NULL);
  }
  return(ptr);
}

int
ExtendWorkSpace(Int s)
{
  MALLOC_T ptr;
  total_space += s;

  if (total_space < MAX_SPACE) return(TRUE);
  ptr = (MALLOC_T)realloc((void *)HeapBase, total_space);
  if (ptr == ((MALLOC_T) - 1)) {
     Error(SYSTEM_ERROR, TermNil, "could not expand stacks %d bytes", s);
     return(FALSE);
  }
  if (ptr != (MALLOC_T)HeapBase) {
     Error(SYSTEM_ERROR, TermNil, "could not expand contiguous stacks  %d bytes", s);
     return(FALSE);
  }
  if ((CELL)ptr & MBIT) {
     Error(SYSTEM_ERROR, TermNil, "memory at %p conflicts with MBIT %lx", ptr, MBIT);
     return(FALSE);
  }
  return TRUE;
}

int
FreeWorkSpace(void)
{
  return TRUE;
}
#endif

void
YAP_InitHeap(void *heap_addr)
{
  /* allocate space */
  HeapBase = heap_addr;

  /* reserve space for specially allocated functors and atoms so that
     their values can be known statically */
  HeapTop = HeapBase + AdjustSize(sizeof(all_heap_codes));

  HeapMax = HeapUsed = HeapTop-HeapBase;

  *((YAP_SEG_SIZE *) HeapTop) = InUseFlag;
  HeapTop = HeapTop + sizeof(YAP_SEG_SIZE);
  *((YAP_SEG_SIZE *) HeapTop) = InUseFlag;

  HeapPlus = HeapTop + MinHGap / CellSize;
  FreeBlocks = NIL;
  HEAPTOP_DISOWN(worker_id);  

#if defined(YAPOR) || defined(TABLING)
#ifdef USE_HEAP
  /* Try to make the system to crash */
  BaseAllocArea = NULL;
  TopAllocArea = BaseAllocArea; 
#else
  BaseAllocArea = AllocCodeSpace(OPT_CHUNK_SIZE);
  TopAllocArea = BaseAllocArea; 
#endif

  LOCAL = REMOTE; /* point to the first area */
#endif

}

void
InitMemory(int Trail, int Heap, int Stack)
{
  Int pm, sa, ta;

  Trail = AdjustPageSize(Trail * K);
  Stack = AdjustPageSize(Stack * K);
  Heap = AdjustPageSize(Heap * K);

  pm = (Trail + Heap + Stack);	/* memory to be
				 * requested         */
  sa = Stack;			/* stack area size   */
  ta = Trail;			/* trail area size   */

  YAP_InitHeap(InitWorkSpace(pm));

  TrailTop = HeapBase + pm;
  LocalBase = TrailTop - ta;
  TrailBase = LocalBase + sizeof(CELL);

  GlobalBase = LocalBase - sa;
  AuxTop = GlobalBase - CellSize;	/* avoid confusions while
					 * * restoring */
  AuxSp = (CELL *) AuxTop;

#ifdef DEBUG
#if SIZEOF_INT_P!=SIZEOF_INT
  if (output_msg) {
    fprintf(stderr, "HeapBase = %p  GlobalBase = %p\n  LocalBase = %p  TrailTop = %p\n",
	       HeapBase, GlobalBase, LocalBase, TrailTop);
#else
  if (output_msg) {
    fprintf(stderr, "HeapBase = %x  GlobalBase = %x\n  LocalBase = %x  TrailTop = %x\n",
	       (UInt) HeapBase, (UInt) GlobalBase,
	       (UInt) LocalBase, (UInt) TrailTop);
#endif

#if !SHORT_INTS
    fprintf(stderr, "Heap+Aux: %d\tLocal+Global: %d\tTrail: %d\n",
	       pm - sa - ta, sa, ta);
#else /* SHORT_INTS */
    fprintf(stderr, "Heap+Aux: %ld\tLocal+Global: %ld\tTrail: %ld\n",
	       pm - sa - ta, sa, ta);
#endif /* SHORT_INTS */
  }
#endif /* DEBUG */

}

