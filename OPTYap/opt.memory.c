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

/**************************************
**      Includes & Declarations      **
**************************************/

#include "Yap.h"
#if defined(YAPOR) && !defined(THREADS)
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/shm.h>
#include <sys/mman.h>
#include "Yatom.h"
#include "YapHeap.h"
#include "alloc.h"
#include "heapgc.h"

#define KBYTES 1024

#ifdef MMAP_MEMORY_MAPPING_SCHEME
int fd_mapfile;
#else /* SHM_MEMORY_MAPPING_SCHEME */
int shm_mapid[MAX_WORKERS + 1];
#endif /* MEMORY_MAPPING_SCHEME */

static  Int extra_area = 0;


/********************************
**      Global functions       **
********************************/

#ifdef SHM_MEMORY_MAPPING_SCHEME
void shm_map_memory(int id, int size, void *shmaddr) {
#define SHMMAX 0x2000000  /* as in <asm/shmparam.h> */
  if (size > SHMMAX)
    Yap_Error(FATAL_ERROR, TermNil, "maximum size for a shm segment exceeded (shm_map_memory)");
  if ((shm_mapid[id] = shmget(IPC_PRIVATE, size, SHM_R|SHM_W)) == -1) 
    Yap_Error(FATAL_ERROR, TermNil, "shmget error (shm_map_memory)");
  if (shmat(shm_mapid[id], shmaddr, 0) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "shmat error (shm_map_memory)");
  return;
}
#else /* MMAP_MEMORY_MAPPING_SCHEME */
void
open_mapfile(long TotalArea) {
  char mapfile[20];
  strcpy(mapfile,"./mapfile");
  itos(getpid(), &mapfile[9]);
  if ((fd_mapfile = open(mapfile, O_RDWR|O_CREAT|O_TRUNC, 0666)) < 0)
    Yap_Error(FATAL_ERROR, TermNil, "open error (open_mapfile)");
  if (lseek(fd_mapfile, TotalArea, SEEK_SET) < 0) 
    Yap_Error(FATAL_ERROR, TermNil, "lseek error (open_mapfile)");
  if (write(fd_mapfile, "", 1) < 0) 
    Yap_Error(FATAL_ERROR, TermNil, "write error (open_mapfile)");
  return;
}


void
close_mapfile(void) {
  if (close(fd_mapfile) < 0) 
    Yap_Error(FATAL_ERROR, TermNil, "close error (close_mapfile)");
}
#endif /* MMAP_MEMORY_MAPPING_SCHEME */

void map_memory(long HeapArea, long GlobalLocalArea, long TrailAuxArea, int n_workers) {
  void *mmap_addr = (void *)MMAP_ADDR;
#ifdef ACOW
  int private_fd_mapfile;
#if MMAP_MEMORY_MAPPING_SCHEME
  long TotalArea;
#endif /* MMAP_MEMORY_MAPPING_SCHEME */
#else /* ENV_COPY || SBA */
  int i;
  long WorkerArea;
  long TotalArea;
#endif /* YAPOR_MODEL */

  /* initial allocation - model independent */
  HeapArea = ADJUST_SIZE_TO_PAGE(HeapArea);
  GlobalLocalArea = ADJUST_SIZE(GlobalLocalArea);
  TrailAuxArea = ADJUST_SIZE(TrailAuxArea);

  /* we'll need this later */
#if defined(YAPOR) && !defined(THREADS)
  Yap_global = (struct worker_shared *)( mmap_addr  - sizeof(struct worker_shared));
  Yap_WLocal =  (struct worker_local *)( mmap_addr  - (sizeof(struct worker_shared)+MAX_WORKERS*sizeof(struct worker_local)));
  extra_area = ADJUST_SIZE_TO_PAGE(sizeof(struct worker_shared)+MAX_WORKERS*sizeof(struct worker_local));
#endif
  Yap_HeapBase = (ADDR)mmap_addr;
  Yap_GlobalBase = mmap_addr + HeapArea;

  /* shared memory allocation - model dependent */
#ifdef ACOW
  /* acow just needs one stack */
#ifdef MMAP_MEMORY_MAPPING_SCHEME
  /* I need this for MMAP to know what it must allocate */
  TotalArea = HeapArea;
#endif /* MMAP_MEMORY_MAPPING_SCHEME */
#else /* ENV_COPY || SBA */
  /* the others need n stacks */
  WorkerArea = ADJUST_SIZE_TO_PAGE(GlobalLocalArea + TrailAuxArea);
  TotalArea = HeapArea + WorkerArea * n_workers;
#endif /* YAPOR_MODEL */
  /* mmap heap area */
#ifdef MMAP_MEMORY_MAPPING_SCHEME
  /* map total area in a single go */
  open_mapfile(TotalArea+extra_area);
  if ((mmap_addr = mmap((void *) MMAP_ADDR-extra_area, (size_t) (TotalArea+extra_area), PROT_READ|PROT_WRITE, 
                         MAP_SHARED|MAP_FIXED, fd_mapfile, 0)) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "mmap error (map_memory)");
#if defined(YAPOR) && !defined(THREADS)
  mmap_addr += extra_area;
#endif
#else /* SHM_MEMORY_MAPPING_SCHEME */
/* Most systems are limited regarding what we can allocate */
#ifdef ACOW
  /* single shared segment in ACOW */
  shm_map_memory(0, HeapArea, mmap_addr);
#else /* ENV_COPY || SBA */
  /* place as segment n otherwise (0..n-1 reserved for worker areas */
  shm_map_memory(n_workers, HeapArea, mmap_addr);
#endif /* YAPOR_MODEL */
#endif /* MEMORY_MAPPING_SCHEME */

#ifdef ACOW
  /* just allocate local space for stacks */
  if ((private_fd_mapfile = open("/dev/zero", O_RDWR)) < 0)
    Yap_Error(FATAL_ERROR, TermNil, "open error (map_memory)");
  if (mmap(Yap_GlobalBase, GlobalLocalArea + TrailAuxArea, PROT_READ|PROT_WRITE, 
           MAP_PRIVATE|MAP_FIXED, private_fd_mapfile, 0) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "mmap error (map_memory)");
  close(private_fd_mapfile);
#else /* ENV_COPY || SBA */
  for (i = 0; i < n_workers; i++) {
    /* initialize worker vars */
    worker_area(i) = Yap_GlobalBase + i * WorkerArea;
    worker_offset(i) = worker_area(i) - worker_area(0);
#ifdef SHM_MEMORY_MAPPING_SCHEME
    /* mapping worker area */
    shm_map_memory(i, WorkerArea, worker_area(i));
#endif /* SHM_MEMORY_MAPPING_SCHEME */
  }
#endif /* YAPOR_MODEL */

#ifdef SBA
  /* alloc space for the sparse binding array */
  sba_size = WorkerArea * n_workers;
  if ((binding_array = (char *)malloc(sba_size)) == NULL)
    Yap_Error(FATAL_ERROR, TermNil, "malloc error (map_memory)");
  if ((CELL)binding_array & MBIT) {
    Yap_Error(INTERNAL_ERROR, TermNil, "binding_array start address conflicts with tag used in IDB (map_memory)");
  }
  sba_offset = binding_array - Yap_GlobalBase;
  sba_end = (int)binding_array + sba_size;
#endif /* SBA */
  Yap_TrailBase = Yap_GlobalBase + GlobalLocalArea;
  Yap_LocalBase = Yap_TrailBase - CellSize;

  if (TrailAuxArea > 262144)                       /* 262144 = 256 * 1024 */ 
    Yap_TrailTop = Yap_TrailBase + (TrailAuxArea - 131072);  /* 131072 = 262144 / 2 */ 
  else
    Yap_TrailTop = Yap_TrailBase + (TrailAuxArea / 2);

  HeapMax = (CELL)(Yap_TrailBase + (TrailAuxArea - CellSize));
  Yap_InitHeap(mmap_addr);
}


void unmap_memory (void) {
#ifdef SHM_MEMORY_MAPPING_SCHEME
  int i;
#else /* MMAP_MEMORY_MAPPING_SCHEME */
  char MapFile[20];
#endif /* MEMORY_MAPPING_SCHEME */
  {
    int proc;
    INFORMATION_MESSAGE("Worker %d exiting...", worker_id);
    for (proc = 0; proc < number_workers; proc++) {
      if (proc != worker_id && worker_pid(proc) != 0) {
        if (kill(worker_pid(proc), SIGKILL) != 0)
          INFORMATION_MESSAGE("Can't kill process %d", worker_pid(proc));
        else 
          INFORMATION_MESSAGE("Killing process %d", worker_pid(proc));
      }
    }
#ifdef ACOW
    if (number_workers > 1) {
      if (kill(GLOBAL_master_worker, SIGINT) != 0)
	INFORMATION_MESSAGE("Can't kill process %d", GLOBAL_master_worker);
      else 
	INFORMATION_MESSAGE("Killing process %d", GLOBAL_master_worker);
    }
#endif /* ACOW */
  }

#ifdef SHM_MEMORY_MAPPING_SCHEME
#ifdef ACOW
  i = 0;
#else
  for (i = 0; i < number_workers + 1; i++)
#endif /* ACOW */
  {
    if (shmctl(shm_mapid[i], IPC_RMID, 0) == 0)
      INFORMATION_MESSAGE("Removing shared memory segment %d", shm_mapid[i]);
    else INFORMATION_MESSAGE("Can't remove shared memory segment %d", shm_mapid[i]);
  }
#else /* MMAP_MEMORY_MAPPING_SCHEME */
  strcpy(MapFile,"./mapfile");
#ifdef ACOW
  itos(GLOBAL_master_worker, &MapFile[9]);
#else /* ENV_COPY || SBA */
  itos(worker_pid(0), &MapFile[9]);
#endif
  if (remove(MapFile) == 0)
    INFORMATION_MESSAGE("Removing mapfile \"%s\"", MapFile);
  else INFORMATION_MESSAGE("Can't remove mapfile \"%s\"", MapFile);
#endif /* MEMORY_MAPPING_SCHEME */
  return;
}


void remap_memory(void) {
#ifdef ACOW
  /* do nothing */
#endif /* ACOW */
#ifdef SBA
  /* setup workers so that they have different areas */
  long WorkerArea = worker_offset(1);

  Yap_GlobalBase += worker_id * WorkerArea;
  Yap_TrailBase += worker_id * WorkerArea;
  Yap_LocalBase += worker_id * WorkerArea;
  Yap_TrailTop += worker_id * WorkerArea;
#endif /* SBA */
#ifdef ENV_COPY
  void *remap_addr;
  long remap_offset;
  long WorkerArea;
  int i;

  remap_addr = worker_area(0);
  remap_offset = (char *)remap_addr - (char *)Yap_HeapBase;
  WorkerArea = worker_offset(1);
#ifdef SHM_MEMORY_MAPPING_SCHEME
  for (i = 0; i < number_workers; i++) {
    if (shmdt(worker_area(i)) == -1)
      Yap_Error(FATAL_ERROR, TermNil, "shmdt error (remap_memory)");
  }
  for (i = 0; i < number_workers; i++) {
    worker_area(i) = remap_addr + ((number_workers + i - worker_id) % number_workers) * WorkerArea;
    if(shmat(shm_mapid[i], worker_area(i), 0) == (void *) -1)
      Yap_Error(FATAL_ERROR, TermNil, "shmat error (remap_memory)");
  }
#else /* MMAP_MEMORY_MAPPING_SCHEME */
  if (munmap(remap_addr, (size_t)(WorkerArea * number_workers)) == -1)
    Yap_Error(FATAL_ERROR, TermNil, "munmap error (remap_memory)");
  for (i = 0; i < number_workers; i++) {
    worker_area(i) = remap_addr + ((number_workers + i - worker_id) % number_workers) * WorkerArea;
    if (mmap(worker_area(i), (size_t)WorkerArea, PROT_READ|PROT_WRITE, 
        MAP_SHARED|MAP_FIXED, fd_mapfile, remap_offset + i * WorkerArea + extra_area) == (void *) -1)
      Yap_Error(FATAL_ERROR, TermNil, "mmap error (remap_memory)");
  }
#endif /* MEMORY_MAPPING_SCHEME */
  for (i = 0; i < number_workers; i++) {
    worker_offset(i) = worker_area(i) - worker_area(worker_id);
  }
#endif /* ENV_COPY */
}
#endif /* YAPOR && !THREADS */
