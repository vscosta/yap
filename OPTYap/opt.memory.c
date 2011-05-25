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
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
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
#include "or.macros.h"



/************************************
**      Macros & Declarations      **
************************************/

#define KBYTES 1024

#ifdef MMAP_MEMORY_MAPPING_SCHEME
int fd_mapfile;
#else /* SHM_MEMORY_MAPPING_SCHEME */
int shm_mapid[MAX_WORKERS + 1];
#endif /* MEMORY_MAPPING_SCHEME */



/******************************************
**      Local functions declaration      **
******************************************/

#ifdef MMAP_MEMORY_MAPPING_SCHEME
void open_mapfile(long TotalArea);
#else /* SHM_MEMORY_MAPPING_SCHEME */
void shm_map_memory(int id, int size, void *shmaddr);
#endif /* MEMORY_MAPPING_SCHEME */



/********************************
**      Global functions       **
********************************/

void Yap_init_yapor_global_local_memory(void) {
#ifdef YAPOR_COW
  int private_fd_mapfile;
#endif /* YAPOR_COW */
  long ExtraArea = ADJUST_SIZE_TO_PAGE(sizeof(struct global_data) + MAX_WORKERS * sizeof(struct worker_local));

  Yap_local = (struct worker_local *)(MMAP_ADDR - ExtraArea);
  Yap_global = (struct global_data *)(MMAP_ADDR - sizeof(struct global_data));
 
#ifdef MMAP_MEMORY_MAPPING_SCHEME
  //open_mapfile(ExtraArea);
   char mapfile[20];
  strcpy(mapfile,"./mapfile");
  itos(getpid(), &mapfile[9]);
  if ((fd_mapfile = open(mapfile, O_RDWR|O_CREAT|O_TRUNC, 0666)) < 0)
    Yap_Error(FATAL_ERROR, TermNil, "open error (open_mapfile)");
  if (lseek(fd_mapfile, ExtraArea, SEEK_SET) < 0) 
    Yap_Error(FATAL_ERROR, TermNil, "lseek error (open_mapfile)");
  if (write(fd_mapfile, "", 1) < 0) 
    Yap_Error(FATAL_ERROR, TermNil, "write error (open_mapfile)");

  if (mmap((void *) Yap_local, (size_t) ExtraArea, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, fd_mapfile, 0) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "mmap error (Yap_init_global_local_memory)");
#else /* SHM_MEMORY_MAPPING_SCHEME */
  /* most systems are limited regarding what we can allocate */
#ifdef YAPOR_COW
  /* single shared segment in ACOW */
  shm_map_memory(0, ExtraArea + HeapArea, (void *) MMAP_ADDR);
#else /* YAPOR_COPY || YAPOR_SBA */
  /* place as segment n otherwise (0..n-1 reserved for worker areas */
  shm_map_memory(n_workers, ExtraArea + HeapArea, (void *) Yap_local);
  { int i;
     for (i = 0; i < n_workers; i++)
       shm_map_memory(i, Yap_worker_area_size, GlobalBase + Yap_worker_area_size * i);
   }
#endif
#endif /* MEMORY_MAPPING_SCHEME */

#ifdef YAPOR_COW
  /* just allocate local space for stacks */
  if ((private_fd_mapfile = open("/dev/zero", O_RDWR)) < 0)
    Yap_Error(FATAL_ERROR, TermNil, "open error (Yap_init_optyap_memory)");
  if (mmap(GlobalBase, GlobalLocalArea + TrailAuxArea, PROT_READ|PROT_WRITE, 
           MAP_PRIVATE|MAP_FIXED, private_fd_mapfile, 0) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "mmap error (Yap_init_optyap_memory)");
  close(private_fd_mapfile);
#endif /* YAPOR_COW */

#ifdef YAPOR_SBA
  /* alloc space for the sparse binding array */
  sba_size = Yap_worker_area_size * n_workers;
  if ((binding_array = (char *)malloc(sba_size)) == NULL)
    Yap_Error(FATAL_ERROR, TermNil, "malloc error (Yap_init_optyap_memory)");
  if ((CELL)binding_array & MBIT) {
    Yap_Error(INTERNAL_ERROR, TermNil, "binding_array start address conflicts with tag used in IDB (Yap_init_optyap_memory)");
  }
  sba_offset = binding_array - GlobalBase;
  sba_end = (int)binding_array + sba_size;
#endif /* YAPOR_SBA */

  return;
}


void Yap_init_yapor_stacks_memory(long TrailAuxArea, long HeapArea, long GlobalLocalArea, int n_workers) {
#ifdef YAPOR_COW
  int private_fd_mapfile;
#if MMAP_MEMORY_MAPPING_SCHEME
  long TotalArea;
#endif /* MMAP_MEMORY_MAPPING_SCHEME */
#else /* YAPOR_COPY || YAPOR_SBA */
  long TotalArea;
#endif
  long ExtraArea = ADJUST_SIZE_TO_PAGE(sizeof(struct global_data) + MAX_WORKERS * sizeof(struct worker_local));

  TrailAuxArea = ADJUST_SIZE(TrailAuxArea);
  HeapArea = ADJUST_SIZE_TO_PAGE(HeapArea);
  GlobalLocalArea = ADJUST_SIZE(GlobalLocalArea); 
  Yap_HeapBase = (ADDR) MMAP_ADDR;
  LOCAL_GlobalBase = (ADDR) (MMAP_ADDR + HeapArea);
  /* shared memory allocation - model dependent */
#ifdef YAPOR_COW
  /* acow just needs one stack */
#ifdef MMAP_MEMORY_MAPPING_SCHEME
  /* I need this for MMAP to know what it must allocate */
  TotalArea = HeapArea;
#endif /* MMAP_MEMORY_MAPPING_SCHEME */
#else /* YAPOR_COPY || YAPOR_SBA */
  /* the others need n stacks */
  Yap_worker_area_size = ADJUST_SIZE_TO_PAGE(GlobalLocalArea + TrailAuxArea);
  TotalArea = ExtraArea + HeapArea + Yap_worker_area_size * n_workers;
#endif

#ifdef MMAP_MEMORY_MAPPING_SCHEME
  /* map total area in a single go */
  //open_mapfile(TotalArea);
    if (lseek(fd_mapfile, TotalArea, SEEK_SET) < 0) 
    Yap_Error(FATAL_ERROR, TermNil, "lseek error (open_mapfile)");
  if (write(fd_mapfile, "", 1) < 0) 
    Yap_Error(FATAL_ERROR, TermNil, "write error (open_mapfile)");
  if (mmap((void *) Yap_HeapBase, (size_t) TotalArea, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, fd_mapfile, ExtraArea) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "mmap error (Yap_init_yapor_memory)");
#else /* SHM_MEMORY_MAPPING_SCHEME */
  /* most systems are limited regarding what we can allocate */
#ifdef YAPOR_COW
  /* single shared segment in ACOW */
  shm_map_memory(0, ExtraArea + HeapArea, (void *) MMAP_ADDR);
#else /* YAPOR_COPY || YAPOR_SBA */
  /* place as segment n otherwise (0..n-1 reserved for worker areas */
  shm_map_memory(n_workers, ExtraArea + HeapArea, (void *) Yap_local);
  { int i;
     for (i = 0; i < n_workers; i++)
       shm_map_memory(i, Yap_worker_area_size, LOCAL_GlobalBase + Yap_worker_area_size * i);
   }
#endif
#endif /* MEMORY_MAPPING_SCHEME */

#ifdef YAPOR_COW
  /* just allocate local space for stacks */
  if ((private_fd_mapfile = open("/dev/zero", O_RDWR)) < 0)
    Yap_Error(FATAL_ERROR, TermNil, "open error (Yap_init_optyap_memory)");
  if (mmap(LOCAL_GlobalBase, GlobalLocalArea + TrailAuxArea, PROT_READ|PROT_WRITE, 
           MAP_PRIVATE|MAP_FIXED, private_fd_mapfile, 0) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "mmap error (Yap_init_optyap_memory)");
  close(private_fd_mapfile);
#endif /* YAPOR_COW */

#ifdef YAPOR_SBA
  /* alloc space for the sparse binding array */
  sba_size = Yap_worker_area_size * n_workers;
  if ((binding_array = (char *)malloc(sba_size)) == NULL)
    Yap_Error(FATAL_ERROR, TermNil, "malloc error (Yap_init_optyap_memory)");
  if ((CELL)binding_array & MBIT) {
    Yap_Error(INTERNAL_ERROR, TermNil, "binding_array start address conflicts with tag used in IDB (Yap_init_optyap_memory)");
  }
  sba_offset = binding_array - LOCAL_GlobalBase;
  sba_end = (int)binding_array + sba_size;
#endif /* YAPOR_SBA */

  LOCAL_TrailBase = LOCAL_GlobalBase + GlobalLocalArea;
  LOCAL_LocalBase = LOCAL_TrailBase - CellSize;
  LOCAL_TrailTop = LOCAL_TrailBase + (TrailAuxArea /2);
  Yap_InitHeap(Yap_HeapBase);
  //HeapMax = (CELL)(LOCAL_TrailBase + (TrailAuxArea - CellSize));
 // HeapLim = LOCAL_GlobalBase;
  return;
}





void OLD_Yap_init_optyap_memory(long TrailAuxArea, long HeapArea, long GlobalLocalArea, int n_workers) {
#ifdef YAPOR_COW
  int private_fd_mapfile;
#if MMAP_MEMORY_MAPPING_SCHEME
  long TotalArea;
#endif /* MMAP_MEMORY_MAPPING_SCHEME */
#else /* YAPOR_COPY || YAPOR_SBA */
  long TotalArea;
#endif
  long ExtraArea;
  ADDR GlobalBase;

  HeapArea = ADJUST_SIZE_TO_PAGE(HeapArea);
  GlobalLocalArea = ADJUST_SIZE(GlobalLocalArea);
  TrailAuxArea = ADJUST_SIZE(TrailAuxArea);
  
  /* initial allocation - model independent */  
  ExtraArea = ADJUST_SIZE_TO_PAGE(sizeof(struct global_data) + MAX_WORKERS * sizeof(struct worker_local));
  Yap_local = (struct worker_local *)(MMAP_ADDR - ExtraArea);
  Yap_global = (struct global_data *)(MMAP_ADDR - sizeof(struct global_data));
  Yap_HeapBase = (ADDR) MMAP_ADDR;
  GlobalBase = (ADDR) (MMAP_ADDR + HeapArea);

  /* shared memory allocation - model dependent */
#ifdef YAPOR_COW
  /* acow just needs one stack */
#ifdef MMAP_MEMORY_MAPPING_SCHEME
  /* I need this for MMAP to know what it must allocate */
  TotalArea = HeapArea;
#endif /* MMAP_MEMORY_MAPPING_SCHEME */
#else /* YAPOR_COPY || YAPOR_SBA */
  /* the others need n stacks */
  Yap_worker_area_size = ADJUST_SIZE_TO_PAGE(GlobalLocalArea + TrailAuxArea);
  TotalArea = ExtraArea + HeapArea + Yap_worker_area_size * n_workers;
#endif

#ifdef MMAP_MEMORY_MAPPING_SCHEME
  /* map total area in a single go */
  open_mapfile(TotalArea);
  if (mmap((void *) Yap_local, (size_t) TotalArea, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, fd_mapfile, 0) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "mmap error (Yap_init_optyap_memory)");
#else /* SHM_MEMORY_MAPPING_SCHEME */
  /* most systems are limited regarding what we can allocate */
#ifdef YAPOR_COW
  /* single shared segment in ACOW */
  shm_map_memory(0, ExtraArea + HeapArea, (void *) MMAP_ADDR);
#else /* YAPOR_COPY || YAPOR_SBA */
  /* place as segment n otherwise (0..n-1 reserved for worker areas */
  shm_map_memory(n_workers, ExtraArea + HeapArea, (void *) Yap_local);
  { int i;
     for (i = 0; i < n_workers; i++)
       shm_map_memory(i, Yap_worker_area_size, GlobalBase + Yap_worker_area_size * i);
   }
#endif
#endif /* MEMORY_MAPPING_SCHEME */

#ifdef YAPOR_COW
  /* just allocate local space for stacks */
  if ((private_fd_mapfile = open("/dev/zero", O_RDWR)) < 0)
    Yap_Error(FATAL_ERROR, TermNil, "open error (Yap_init_optyap_memory)");
  if (mmap(GlobalBase, GlobalLocalArea + TrailAuxArea, PROT_READ|PROT_WRITE, 
           MAP_PRIVATE|MAP_FIXED, private_fd_mapfile, 0) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "mmap error (Yap_init_optyap_memory)");
  close(private_fd_mapfile);
#endif /* YAPOR_COW */

#ifdef YAPOR_SBA
  /* alloc space for the sparse binding array */
  sba_size = Yap_worker_area_size * n_workers;
  if ((binding_array = (char *)malloc(sba_size)) == NULL)
    Yap_Error(FATAL_ERROR, TermNil, "malloc error (Yap_init_optyap_memory)");
  if ((CELL)binding_array & MBIT) {
    Yap_Error(INTERNAL_ERROR, TermNil, "binding_array start address conflicts with tag used in IDB (Yap_init_optyap_memory)");
  }
  sba_offset = binding_array - GlobalBase;
  sba_end = (int)binding_array + sba_size;
#endif /* YAPOR_SBA */
  Yap_InitHeap(Yap_HeapBase);
  LOCAL = REMOTE(0);  /* point to the first area */
  LOCAL_GlobalBase = GlobalBase;
  LOCAL_TrailBase = LOCAL_GlobalBase + GlobalLocalArea;
  LOCAL_LocalBase = LOCAL_TrailBase - CellSize;
  LOCAL_TrailTop = LOCAL_TrailBase + (TrailAuxArea /2);
  //HeapMax = (CELL)(LOCAL_TrailBase + (TrailAuxArea - CellSize));
 // HeapLim = LOCAL_GlobalBase;
}


void Yap_remap_optyap_memory(void) {
#ifdef YAPOR_SBA
  /* setup workers so that they have different areas */
  LOCAL_GlobalBase += worker_id * Yap_worker_area_size;
  LOCAL_TrailBase += worker_id * Yap_worker_area_size;
  LOCAL_LocalBase += worker_id * Yap_worker_area_size;
  LOCAL_TrailTop += worker_id * Yap_worker_area_size;
#endif /* YAPOR_SBA */

#ifdef YAPOR_COPY
  int i;
  void *remap_addr = LOCAL_GlobalBase;
#ifdef MMAP_MEMORY_MAPPING_SCHEME
  long remap_offset = (ADDR) remap_addr - (ADDR) Yap_local;
  if (munmap(remap_addr, (size_t)(Yap_worker_area_size * GLOBAL_number_workers)) == -1)
    Yap_Error(FATAL_ERROR, TermNil, "munmap error (Yap_remap_optyap_memory)");
  for (i = 0; i < GLOBAL_number_workers; i++)
    if (mmap(remap_addr + worker_offset(i), (size_t)Yap_worker_area_size, PROT_READ|PROT_WRITE, 
             MAP_SHARED|MAP_FIXED, fd_mapfile, remap_offset + i * Yap_worker_area_size) == (void *) -1)
      Yap_Error(FATAL_ERROR, TermNil, "mmap error (Yap_remap_optyap_memory)");
#else /* SHM_MEMORY_MAPPING_SCHEME */
  for (i = 0; i < GLOBAL_number_workers; i++)
    if (shmdt(remap_addr + Yap_worker_area_size * i) == -1)
      Yap_Error(FATAL_ERROR, TermNil, "shmdt error (Yap_remap_optyap_memory)");
  for (i = 0; i < GLOBAL_number_workers; i++)
    if(shmat(shm_mapid[i], remap_addr + worker_offset(i), 0) == (void *) -1)
      Yap_Error(FATAL_ERROR, TermNil, "shmat error (Yap_remap_optyap_memory)");
#endif /* MEMORY_MAPPING_SCHEME */
#endif /* YAPOR_COPY */
}


void Yap_unmap_optyap_memory (void) {
#ifdef MMAP_MEMORY_MAPPING_SCHEME
  char MapFile[20];
#else /* SHM_MEMORY_MAPPING_SCHEME */
  int i;
#endif /* MEMORY_MAPPING_SCHEME */
  int proc;

  INFORMATION_MESSAGE("Worker %d exiting...", worker_id);
  for (proc = 0; proc < GLOBAL_number_workers; proc++) {
    if (proc != worker_id && GLOBAL_worker_pid(proc) != 0) {
      if (kill(GLOBAL_worker_pid(proc), SIGKILL) != 0)
        INFORMATION_MESSAGE("Can't kill process %d", GLOBAL_worker_pid(proc));
      else 
        INFORMATION_MESSAGE("Killing process %d", GLOBAL_worker_pid(proc));
    }
  }
      
#ifdef YAPOR_COW
  if (GLOBAL_number_workers > 1) {
    if (kill(GLOBAL_master_worker, SIGINT) != 0)
      INFORMATION_MESSAGE("Can't kill process %d", GLOBAL_master_worker);
    else 
      INFORMATION_MESSAGE("Killing process %d", GLOBAL_master_worker);
  }
#endif /* YAPOR_COW */


#ifdef MMAP_MEMORY_MAPPING_SCHEME
  strcpy(MapFile,"./mapfile");
#ifdef YAPOR_COW
  itos(GLOBAL_master_worker, &MapFile[9]);
#else /* YAPOR_COPY || YAPOR_SBA */
  itos(GLOBAL_worker_pid(0), &MapFile[9]);
#endif
  if (remove(MapFile) == 0)
    INFORMATION_MESSAGE("Removing mapfile \"%s\"", MapFile);
  else
	INFORMATION_MESSAGE("Can't remove mapfile \"%s\"", MapFile);
#else /* SHM_MEMORY_MAPPING_SCHEME */
#ifdef YAPOR_COW
  i = 0;
#else /* YAPOR_COPY || YAPOR_SBA */
  for (i = 0; i < GLOBAL_number_workers + 1; i++)
#endif
  {
    if (shmctl(shm_mapid[i], IPC_RMID, 0) == 0)
      INFORMATION_MESSAGE("Removing shared memory segment %d", shm_mapid[i]);
    else
      INFORMATION_MESSAGE("Can't remove shared memory segment %d", shm_mapid[i]);
  }
#endif /* MEMORY_MAPPING_SCHEME */
  return;
}



/* ------------------------- **
**      Local functions      **
** ------------------------- */

#ifdef MMAP_MEMORY_MAPPING_SCHEME
void open_mapfile(long TotalArea) {
  char mapfile[20];
  strcpy(mapfile,"./mapfile");
  itos(getpid(), &mapfile[9]);
  printf(" file %s \n", mapfile);
  if ((fd_mapfile = open(mapfile, O_RDWR|O_CREAT|O_TRUNC, 0666)) < 0)
    Yap_Error(FATAL_ERROR, TermNil, "open error (open_mapfile)");
  if (lseek(fd_mapfile, TotalArea, SEEK_SET) < 0) 
    Yap_Error(FATAL_ERROR, TermNil, "lseek error (open_mapfile)");
  if (write(fd_mapfile, "", 1) < 0) 
    Yap_Error(FATAL_ERROR, TermNil, "write error (open_mapfile)");
  return;
}
#else /* SHM_MEMORY_MAPPING_SCHEME */
void shm_map_memory(int id, int size, void *shmaddr) {
  if (size > SHMMAX)
    Yap_Error(FATAL_ERROR, TermNil, "maximum size for a shm segment exceeded (shm_map_memory)");
  if ((shm_mapid[id] = shmget(IPC_PRIVATE, size, SHM_R|SHM_W)) == -1) 
    Yap_Error(FATAL_ERROR, TermNil, "shmget error (shm_map_memory)");
  if (shmat(shm_mapid[id], shmaddr, 0) == (void *) -1)
    Yap_Error(FATAL_ERROR, TermNil, "shmat error (shm_map_memory)");
  return;
}
#endif /* MMAP_MEMORY_MAPPING_SCHEME */
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA */
