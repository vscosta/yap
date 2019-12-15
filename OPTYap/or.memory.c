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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#include <string.h>
#include <sys/shm.h>
#include <sys/mman.h>
#include "Yatom.h"
#include "alloc.h"
#include "or.macros.h"



/************************************
**      Macros & Declarations      **
************************************/

#define GLOBAL_LOCAL_STRUCTS_AREA  ADJUST_SIZE_TO_PAGE(sizeof(struct global_data) + MAX_WORKERS * sizeof(struct worker_local))

#ifdef MMAP_MEMORY_MAPPING_SCHEME
#define PATH_MAX 1000
char mapfile_path[PATH_MAX];
#elif defined(SHM_MEMORY_MAPPING_SCHEME)
int shm_mapid[MAX_WORKERS + 2];
#endif /* MEMORY_MAPPING_SCHEME */



/******************************************
**      Local functions declaration      **
******************************************/

#ifdef SHM_MEMORY_MAPPING_SCHEME
void shm_map_memory(int id, int size, void *shmaddr);
void shm_unmap_memory(int id);
#endif /* SHM_MEMORY_MAPPING_SCHEME */



/********************************
**      Global functions       **
********************************/

void Yap_init_yapor_global_local_memory(void) {
  Yap_local = (struct worker_local *)(MMAP_ADDR - GLOBAL_LOCAL_STRUCTS_AREA);
  Yap_global = (struct global_data *)(MMAP_ADDR - sizeof(struct global_data));
 
#ifdef MMAP_MEMORY_MAPPING_SCHEME
  { int fd_mapfile;
    if (getcwd(mapfile_path,PATH_MAX) == NULL)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "getcwd error (Yap_init_yapor_global_local_memory)");
    strcat(mapfile_path,"/mapfile");
    itos(getpid(), &mapfile_path[strlen(mapfile_path)]);
    if (strlen(mapfile_path) >= PATH_MAX)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "PATH_MAX error (Yap_init_yapor_global_local_memory)");
    if ((fd_mapfile = open(mapfile_path, O_RDWR|O_CREAT|O_TRUNC, 0666)) < 0)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "open error (Yap_init_yapor_global_local_memory)");
    if (lseek(fd_mapfile, GLOBAL_LOCAL_STRUCTS_AREA, SEEK_SET) < 0) 
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "lseek error (Yap_init_yapor_global_local_memory)");
    if (write(fd_mapfile, "", 1) < 0) 
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "write error (Yap_init_yapor_global_local_memory)");
    if (mmap((void *) Yap_local, (size_t) GLOBAL_LOCAL_STRUCTS_AREA, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, fd_mapfile, 0) == (void *) -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "mmap error (Yap_init_global_local_memory)");
    if (close(fd_mapfile) == -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "close error (Yap_init_yapor_global_local_memory)");
  }
#elif defined(SHM_MEMORY_MAPPING_SCHEME)
  /* place as segment MAX_WORKERS (0..MAX_WORKERS-1 reserved for worker areas) */
  shm_map_memory(MAX_WORKERS, GLOBAL_LOCAL_STRUCTS_AREA, (void *) Yap_local);
#endif /* MEMORY_MAPPING_SCHEME */

  return;
}


void Yap_9init_yapor_stacks_memory(UInt TrailStackArea, UInt HeapStackArea, UInt GlobalLocalStackArea, int n_workers) {
  long StacksArea;

  HeapStackArea *= (K);
  GlobalLocalStackArea *= (K);
  TrailStackArea *= (K);
  TrailStackArea = ADJUST_SIZE_TO_PAGE(TrailStackArea);
  HeapStackArea = ADJUST_SIZE_TO_PAGE(HeapStackArea);
  GlobalLocalStackArea = ADJUST_SIZE_TO_PAGE(GlobalLocalStackArea); 
  Yap_worker_area_size = GlobalLocalStackArea + TrailStackArea;
#if defined(YAPOR_COPY) || defined(YAPOR_SBA)
  StacksArea = HeapStackArea + Yap_worker_area_size * n_workers;
#elif defined(YAPOR_COW)
  StacksArea = HeapStackArea;
#endif /* YAPOR_COPY || YAPOR_SBA || YAPOR_COW */

  Yap_HeapBase = (ADDR) MMAP_ADDR;
  LOCAL_GlobalBase = (ADDR) (MMAP_ADDR + HeapStackArea);

#ifdef MMAP_MEMORY_MAPPING_SCHEME
  /* map stacks in a single go */
  { int fd_mapfile; 
    if ((fd_mapfile = open(mapfile_path, O_RDWR)) < 0)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "open error ( Yap_init_yapor_stacks_memory)");
    if (lseek(fd_mapfile, GLOBAL_LOCAL_STRUCTS_AREA + StacksArea, SEEK_SET) < 0) 
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "lseek error (Yap_init_yapor_stacks_memory)");
    if (write(fd_mapfile, "", 1) < 0) 
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "write error (Yap_init_yapor_stacks_memory)");
    if (mmap((void *) Yap_HeapBase, (size_t) StacksArea, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, fd_mapfile, GLOBAL_LOCAL_STRUCTS_AREA) == (void *) -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "mmap error (Yap_init_yapor_stacks_memory)");
    if (close(fd_mapfile) == -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "close error (Yap_init_yapor_stacks_memory)");
  }
#elif defined(SHM_MEMORY_MAPPING_SCHEME)
  /* place heap stack segment as MAX_WORKERS+1 */
  shm_map_memory(MAX_WORKERS + 1, HeapStackArea, (void *) Yap_HeapBase);
#if defined(YAPOR_COPY) || defined(YAPOR_SBA)
  /* map segments for worker areas as 0..MAX_WORKERS-1 */
  { int i;
    for (i = 0; i < n_workers; i++)
      shm_map_memory(i, Yap_worker_area_size, LOCAL_GlobalBase + Yap_worker_area_size * i);
  }
#endif /* YAPOR_COPY || YAPOR_SBA */
#endif /* MEMORY_MAPPING_SCHEME */

#ifdef YAPOR_COW
  /* just allocate local space for stacks */
  { int private_fd_mapfile;
    if ((private_fd_mapfile = open("/dev/zero", O_RDWR)) < 0)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "open error (Yap_init_yapor_stacks_memory)");
    if (mmap(LOCAL_GlobalBase, Yap_worker_area_size, PROT_READ|PROT_WRITE, 
             MAP_PRIVATE|MAP_FIXED, private_fd_mapfile, 0) == (void *) -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "mmap error (Yap_init_yapor_stacks_memory)");
    close(private_fd_mapfile);
  }
#endif /* YAPOR_COW */

#ifdef YAPOR_SBA
  /* alloc space for the sparse binding array */
  sba_size = Yap_worker_area_size * n_workers;
  if ((binding_array = (char *)malloc(sba_size)) == NULL)
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "malloc error (Yap_init_yapor_stacks_memory)");
  if ((CELL)binding_array & MBIT)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "binding_array start address conflicts with tag used in IDB (Yap_init_yapor_stacks_memory)");
  sba_offset = binding_array - LOCAL_GlobalBase;
  sba_end = (int)binding_array + sba_size;
#endif /* YAPOR_SBA */

  LOCAL_TrailBase = LOCAL_GlobalBase + GlobalLocalStackArea;
  LOCAL_LocalBase = LOCAL_TrailBase - CellSize;
  LOCAL_TrailTop = LOCAL_TrailBase + TrailStackArea;
  Yap_InitHeap(Yap_HeapBase);
  HeapLim = LOCAL_GlobalBase;
  return;
}


void Yap_remap_yapor_memory(void) {
#if defined(YAPOR_COPY)
  int i;
  void *remap_addr = LOCAL_GlobalBase;
#ifdef MMAP_MEMORY_MAPPING_SCHEME
  int fd_mapfile;
  long remap_offset = (ADDR) remap_addr - (ADDR) Yap_local;
  if ((fd_mapfile = open(mapfile_path, O_RDWR)) < 0)
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "open error (Yap_remap_yapor_memory)");
  if (munmap(remap_addr, (size_t)(Yap_worker_area_size * GLOBAL_number_workers)) == -1)
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "munmap error (Yap_remap_yapor_memory)");
  for (i = 0; i < GLOBAL_number_workers; i++)
    if (mmap(remap_addr + worker_offset(i), (size_t)Yap_worker_area_size, PROT_READ|PROT_WRITE, 
             MAP_SHARED|MAP_FIXED, fd_mapfile, remap_offset + i * Yap_worker_area_size) == (void *) -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "mmap error (Yap_remap_yapor_memory)");
  if (close(fd_mapfile) == -1)
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "close error (Yap_remap_yapor_memory)");
#else /* SHM_MEMORY_MAPPING_SCHEME */
  for (i = 0; i < GLOBAL_number_workers; i++)
    if (shmdt(remap_addr + Yap_worker_area_size * i) == -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "shmdt error (Yap_remap_yapor_memory)");
  for (i = 0; i < GLOBAL_number_workers; i++)
    if(shmat(shm_mapid[i], remap_addr + worker_offset(i), 0) == (void *) -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "shmat error (Yap_remap_yapor_memory)");
#endif /* MEMORY_MAPPING_SCHEME */
#elif defined(YAPOR_SBA)
  /* setup workers so that they have different areas */
  LOCAL_GlobalBase += worker_id * Yap_worker_area_size;
  LOCAL_TrailBase += worker_id * Yap_worker_area_size;
  LOCAL_LocalBase += worker_id * Yap_worker_area_size;
  LOCAL_TrailTop += worker_id * Yap_worker_area_size;
#endif /* YAPOR_COPY || YAPOR_SBA */
}


void Yap_unmap_yapor_memory (void) {
  int i;

  INFORMATION_MESSAGE("Worker %d exiting...", worker_id);
  for (i = 0; i < GLOBAL_number_workers; i++)
    if (i != worker_id && GLOBAL_worker_pid(i) != 0) {
      if (kill(GLOBAL_worker_pid(i), SIGKILL) != 0)
        INFORMATION_MESSAGE("Can't kill process %d", GLOBAL_worker_pid(i));
      else 
        INFORMATION_MESSAGE("Killing process %d", GLOBAL_worker_pid(i));
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
  if (remove(mapfile_path) == 0)
    INFORMATION_MESSAGE("Removing mapfile \"%s\"", mapfile_path);
  else
    INFORMATION_MESSAGE("Can't remove mapfile \"%s\"", mapfile_path);
#elif defined(SHM_MEMORY_MAPPING_SCHEME)
#if defined(YAPOR_COPY) || defined(YAPOR_SBA)
  shm_unmap_memory(MAX_WORKERS);
  shm_unmap_memory(MAX_WORKERS + 1);
  for (i = 0; i < GLOBAL_number_workers; i++)
    shm_unmap_memory(i);
#elif defined(YAPOR_COW)
  shm_unmap_memory(0);
#endif /* YAPOR_COPY || YAPOR_SBA || YAPOR_COW */
#endif /* MEMORY_MAPPING_SCHEME */
  return;
}



/* ------------------------- **
**      Local functions      **
** ------------------------- */

#ifdef SHM_MEMORY_MAPPING_SCHEME
void shm_map_memory(int id, int size, void *shmaddr) {
  if ((shm_mapid[id] = shmget(IPC_PRIVATE, size, SHM_R|SHM_W)) == -1) 
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "shmget error (shm_map_memory)");
  if (shmat(shm_mapid[id], shmaddr, 0) == (void *) -1)
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "shmat error (shm_map_memory)");
  return;
}


void shm_unmap_memory(int id) {
  if (shmctl(shm_mapid[id], IPC_RMID, 0) == 0)
    INFORMATION_MESSAGE("Removing shared memory segment %d", shm_mapid[id]);
  else
    INFORMATION_MESSAGE("Can't remove shared memory segment %d", shm_mapid[id]);
  return;
}
#endif /* SHM_MEMORY_MAPPING_SCHEME */
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA */
