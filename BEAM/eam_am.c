/*************************************************************************
*									 *
*	       BEAM -> Basic Extended Andorra Model                      *
*         BEAM extends the YAP Prolog system to support the EAM          *
* Copyright: Ricardo Lopes and NCC - University of Porto, Portugal       *
*									 *
**************************************************************************
* comments:	eam abstract machine emulator				 *
*									 *
*           IMPORTANT: ON i386 ISAPPL SHOUD ALWAYS BE AFTER ISVAR        *
*************************************************************************/

#ifdef BEAM

#include "Yap.h"
#include "Yatom.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h> 
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define Debug 0
#define Debug_GC 1
#define Debug_Dump_State 0  /* 0 =off || 1==only on Scheduling || 2== only on GC || 4=on every abs inst NOTE: DEBUG has to be enable to use 4*/
#define Debug_MEMORY 0
#define Memory_Stat  0
#define Clear_MEMORY 0      /* 0- do not clear || 1-> clear on request  || 2-> clear on release || 3 -> both*/
#define Fast_go 1           /* normaly 1 ; use 0 to run some extra tests only to control some possible bugs (slower) */
#define USE_SPLIT     1

#define MEM_FOR_BOXES  32  /* In Mb */
#define MEM_FOR_HEAP   32  /* In Mb */
#define MEM_FOR_VARS   32  /* In Mb */
#define MEM_BOXES      MEM_FOR_BOXES*1024*1024
#define MEM_H          MEM_FOR_HEAP*1024*1024
#define MEM_VARS       MEM_FOR_VARS*1024*1024
#define INDEX_SIZE     100000  /* size of vector for saving memory requests */

#define GARBAGE_COLLECTOR 2 /* 0= NO GC || 1 = Heap only || 2 = Heap + Box */
#define HYBRID_BOXMEM  1    /* 0 - Off  || 1 - On */
#define START_ON_NEXT  1    /* PLEASE DON'T CHANGE , specially if you use skip_while_var */
#define USE_LEFTMOST   1    /* SHOULD ALWAYS BE 1 for now... */ 
#define MICRO_TIME     1    /* 0 == eamtime uses CPU time   1 == eamtime uses total time */
#define MAX_MEMORYSTAT 5000
#define READ 0
#define WRITE 1

#include "eam.h"
#include "eamamasm.h"

int EAM=0;                 /* Is EAM enabled ?                       */
Cell *beam_ALTERNATIVES;   /* NEEDED FOR ABSMI */
PredEntry *bpEntry;
struct EAM_Global EAMGlobal;
struct EAM_Global *eamGlobal=&EAMGlobal;

#if !Debug
   #define INLINE  inline
   #define DIRECT_JUMP 1
#else
   #define INLINE
   #define DIRECT_JUMP 0
   void break_top(void);  void break_top(void) { };
   void break_debug(int); 
   void break_debug(int conta) { 
 #if Debug_Dump_State & 4
  		        dump_eam_state();
 #endif
			if (Debug!=-1 && conta>Debug*100) {printf("exit por contador>debug\n"); exit(1); }
   };
#endif

#define push_mode_and_sreg() { *--beam_sp = (Cell) beam_Mode; *--beam_sp  = (Cell) beam_S; }
#define pop_mode_and_sreg()  { beam_S = (Cell *) *beam_sp++; beam_Mode = (short) *beam_sp++; }

#define isvar(a)   IsVarTerm((Cell) a)
#define isappl(a)  IsApplTerm((Cell) a)  
#define ispair(a)  IsPairTerm((Cell) a)
#define isatom(a)  IsAtomOrIntTerm((Cell) a)                   
#define reppair(a) RepPair((Cell) a)
#define repappl(a) RepAppl((Cell) a)
#define abspair(a) AbsPair((Term *) a)
#define absappl(a) AbsAppl((Term *) a)

int is_perm_var(Cell *a); inline int is_perm_var(Cell *a) { if (a>=(Cell *)  beam_END_BOX && a<(Cell *) (beam_END_BOX+MEM_VARS)) return(1); else return (0); }
//int is_perm_var(Cell *a); inline int is_perm_var(Cell *a) { if (a<(Cell *) beam_END_BOX) return(0); else return (1); }
//int is_perm_var(Cell *a); inline int is_perm_var(Cell *a) { if ( a<(Cell *) beam_START_ADDR_HEAP || a>=(Cell *)  beam_END_BOX) return(1); else return (0); }

Cell deref(Cell a);
int Unify(Cell *a, Cell *b);
void UnifyCells(Cell *a, Cell *b);
void trail(struct AND_BOX *andbox,struct PERM_VAR *a);
void limpa_trail(struct AND_BOX *andbox);
void get_arguments(int nr, Cell *a);
Cell *save_arguments(int nr);
void remove_memory_arguments(Cell *a);
void initialize_memory_areas(void);
Cell *request_memory(int size);
void free_memory(Cell *mem,int size);
void limpa_trail_orbox(struct OR_BOX *orbox);
struct SUSPENSIONS *addto_suspensions_list(struct AND_BOX *a,int reason);
void delfrom_suspensions_list(struct SUSPENSIONS *b);
void totop_suspensions_list(struct SUSPENSIONS *b);
int verify_externals(struct AND_BOX *and_box);
void remove_from_perm_var_suspensions(struct PERM_VAR *v,struct AND_BOX *andbox);
void change_perm_var_suspensions(struct PERM_VAR *v,struct AND_BOX *andbox,struct AND_BOX *new);
void do_forking_andbox(struct AND_BOX *a);
void remove_all_externals(struct AND_BOX *andbox);
void remove_all_externals_suspensions(struct AND_BOX *andbox);
void del_andbox_and_sons(struct AND_BOX *andbox);
void del_orbox_and_sons(struct OR_BOX *orbox);
void waking_boxes_suspended_on_var(struct PERM_VAR *v);
struct PERM_VAR *request_permVar(struct AND_BOX *a);
void free_permVar(struct PERM_VAR *v);
Cell *request_memory_locals(int nr);
Cell *request_memory_locals_noinit(int nr);
void free_memory_locals(Cell *l);
void add_to_list_perms(struct PERM_VAR *var,struct AND_BOX *a);
void remove_list_perms(struct AND_BOX *a);
void move_perm_vars(struct AND_BOX *b, struct AND_BOX *a);
void move_perm_variables(struct AND_BOX *a);
void inc_level(struct AND_BOX *andbox,int dif);
void abort_eam(char *s);
void exit_eam(char *s);
int HEAP_MEM_FULL(void);
void change_from_to(struct PERM_VAR *o,struct PERM_VAR *d);
unsigned int index_of_hash_table_atom(Cell c, int nr);
unsigned int index_of_hash_table_appl(Cell c, int nr);
int deve_limpar_var(struct EXTERNAL_VAR *e);
struct status_and *remove_call_from_andbox(struct status_and *ncall, struct AND_BOX *a);
int is_leftmost(struct AND_BOX *a, struct status_and *n);
int exists_var_in(Cell *c);
void garbage_collector(void);
void conta_memoria_livre(int size);
int showTime(void);
struct AND_BOX *choose_leftmost(void);
extern Cell BEAM_is(void);
extern void do_eam_indexing(struct Predicates *);
extern void Yap_plwrite(Term, struct stream_desc *, int, int);

#if Debug_Dump_State
   void dump_eam_state(void);
#endif



/************************************************************************\
* Debug + Status routines  						 *
\************************************************************************/

void conta_memoria_livre(int size){
int i,nr,ult=0;
long total=0;
Cell *c;

 for(i=0;i<INDEX_SIZE;i++) {
   nr=0;
   c=beam_IndexFree[i];
   
   while(c!=NULL) {
     ult=i;
     nr++;
     c=(Cell *) *c;
   }
   total=total+nr*i;
 } 
 printf("Ultimo Pedido (bytes) =%d ¦ Ultimo bloco livre=%d\n",size,(int) ult*CELL_SIZE);
 printf("Memoria TOTAL (bytes)      =%ld \n",((unsigned long)  beam_END_BOX)-((unsigned long)  beam_START_ADDR_BOXES));
 printf("Memoria livre no IndexFree=%ld \n",total*CELL_SIZE);
 printf("Memoria Total livre        =%ld \n",total*CELL_SIZE+((unsigned long)  beam_END_BOX)-((unsigned long)beam_NextFree));
 printf("Memoria Total na HEAP=%ld    livre=%ld \n",(unsigned long) MEM_H,(unsigned long) beam_H-(unsigned long) beam_START_ADDR_HEAP);
}

void abort_eam(char *s)
{
  printf("%s\n",s);
  exit(1);
}

void exit_eam(char *s)
{
  printf("%s\n",s);
  if (beam_nr_call_forking) printf("%d forks executed\n",beam_nr_call_forking);

  if (beam_nr_gc_heap) 
   printf("GC was called %d times on Heap  Mem\n",beam_nr_gc_heap);
  if (beam_nr_gc_boxed)
   printf("GC was called %d times on Boxed Mem\n",beam_nr_gc_boxed);
  if (beam_nr_gc_boxed && beam_nr_gc_heap)
   printf("GC was called %d times \n",beam_nr_gc_boxed+beam_nr_gc_heap);

#if Memory_Stat
  {unsigned long req, used;
   req=beam_TOTAL_MEM+beam_TOTAL_PERMS;
   used=(beam_TOTAL_MEM+beam_TOTAL_PERMS)-(beam_MEM_REUSED+beam_PERMS_REUSED);

  printf("-------------------------------------------------------------------\n");
  printf("Total Mem: Requested %ld (%.2fKb) (%.2fMb) \n", req, req/1024.0, req/1048576.0);
  printf("           Used      %ld (%.2fKb) (%.2fMb) / Reused (%3.2f%c)\n", used,used/1024.0, used/1048576.0, (float) (req-used)/req*100,'%');
  printf("-------------------------------------------------------------------\n");

  used=(beam_TOTAL_MEM-beam_TOTAL_TEMPS)-(beam_MEM_REUSED-beam_TEMPS_REUSED);
  printf("Boxed Mem: Requested %ld (%.2fKb) (%.2fMb) \n", beam_TOTAL_MEM-beam_TOTAL_TEMPS, (beam_TOTAL_MEM-beam_TOTAL_TEMPS)/1024.0, (beam_TOTAL_MEM-beam_TOTAL_TEMPS)/1048576.0);
  printf("           Used      %ld (%.2fKb) (%.2fMb) / Reused (%3.2f%c)\n", used, used/1024.0, used/1048576.0, (float) (beam_MEM_REUSED-beam_TEMPS_REUSED)/(beam_TOTAL_MEM-beam_TOTAL_TEMPS)*100,'%');

  used=beam_TOTAL_TEMPS-beam_TEMPS_REUSED;
  printf("Temps Mem: Requested %ld (%.2fKb) (%.2fMB)\n", beam_TOTAL_TEMPS, beam_TOTAL_TEMPS/1024.0, beam_TOTAL_TEMPS/1048576.0);
  printf("           Used      %ld (%.2fKb) (%.2fMb) / Reused (%3.2f%c)\n", used, used/1024.0,used/1048576.0,(float) beam_TEMPS_REUSED/(beam_TOTAL_TEMPS)*100,'%');


  used=beam_TOTAL_PERMS-beam_PERMS_REUSED;
  printf("Perms Mem: Requested %ld (%.2fKb) (%.2fMB)\n", beam_TOTAL_PERMS, beam_TOTAL_PERMS/1024.0, beam_TOTAL_PERMS/1048576.0);
  printf("           Used      %ld (%.2fKb) (%.2fMb) / Reused (%3.2f%c)\n", used, used/1024.0,used/1048576.0,(float) beam_PERMS_REUSED/(beam_TOTAL_PERMS)*100,'%');
  }
  printf("-------------------------------------------------------------------\n");
  if (beam_nr_gc_boxed+beam_nr_gc_heap>0) {
  int i;
    beam_Memory_STAT[0][0]=0; beam_Memory_STAT[0][1]=0; beam_Memory_STAT[0][2]=0; beam_Memory_STAT[0][3]=0; beam_Memory_STAT[0][4]=0;
    for(i=1;i<=beam_nr_gc_boxed+beam_nr_gc_heap;i++) {
      beam_Memory_STAT[0][0]+=beam_Memory_STAT[i][0];
      beam_Memory_STAT[0][1]+=beam_Memory_STAT[i][1];
      beam_Memory_STAT[0][2]+=beam_Memory_STAT[i][2];
      beam_Memory_STAT[0][3]+=beam_Memory_STAT[i][3];
      beam_Memory_STAT[0][4]+=beam_Memory_STAT[i][4];
      printf("GC %4d Time=%ld  H=%ld to %ld (%3.2f) Box=%ld to %ld (%3.2f)\n",
           i, beam_Memory_STAT[i][0], beam_Memory_STAT[i][1], beam_Memory_STAT[i][3], 
           ((float)  beam_Memory_STAT[i][3]/beam_Memory_STAT[i][1])*100 , beam_Memory_STAT[i][2], beam_Memory_STAT[i][4],
           ((float)  beam_Memory_STAT[i][4]/beam_Memory_STAT[i][2])*100);
    }
      i--;
      printf("\nRESUME GC: Time=%ld  H=%ld to %ld (%3.2f) Box=%ld to %ld (%3.2f)\n",
           beam_Memory_STAT[0][0]/i, beam_Memory_STAT[0][1]/i, beam_Memory_STAT[0][3]/i, 
           100.0-((float)  beam_Memory_STAT[0][3]/beam_Memory_STAT[0][1])*100 , beam_Memory_STAT[0][2]/i, beam_Memory_STAT[0][4]/i,
           100.0-((float)  beam_Memory_STAT[0][4]/beam_Memory_STAT[0][2])*100);

  } else {
    printf("Heap Mem Requested %ld (%.2fKb) (%.2fMB) \n", ((unsigned long) beam_H-beam_START_ADDR_HEAP), ((unsigned long) beam_H-beam_START_ADDR_HEAP)/1024.0, ((unsigned long) beam_H-beam_START_ADDR_HEAP)/1048576.0);
  printf("-------------------------------------------------------------------\n");
  }
#endif
  exit(0);
}


/************************************************************************\
* Memory Management routines  						 *
\************************************************************************/

void initialize_memory_areas()
{
   static int first_time=1;

   if (first_time) {
     first_time=0;
     beam_IndexFree=(Cell **) malloc(INDEX_SIZE*POINTER_SIZE);
     if ((void *) beam_IndexFree==(void *)NULL) abort_eam("Memory Initialization Error IndexFree\n");

     beam_START_ADDR_HEAP=(unsigned long) malloc(MEM_H+MEM_BOXES+MEM_VARS);
     if ((void *)beam_START_ADDR_HEAP==(void *)NULL) abort_eam("Memory Initialization Error Heap+Boxes\n");
      beam_START_ADDR_BOXES=beam_START_ADDR_HEAP+MEM_H;
      beam_END_H=beam_START_ADDR_HEAP+MEM_H; 
      beam_END_BOX=beam_START_ADDR_BOXES+MEM_BOXES;
   } 

   beam_sp=(Cell *) beam_END_H; beam_sp-=2;

   beam_NextVar=(struct PERM_VAR *)  beam_END_BOX;
   beam_H=(Cell *) beam_START_ADDR_HEAP;
#if GARBAGE_COLLECTOR!=2
   beam_NextFree=(Cell *)  beam_END_BOX;
#else
   beam_NextFree=(Cell *)  beam_START_ADDR_BOXES;
#endif
   beam_MemGoing=1;
   memset(beam_IndexFree,0,INDEX_SIZE*POINTER_SIZE);
   { int i,max;
     max=MEM_VARS/PERM_VAR_SIZE;
     for(i=0;i<max-1;i++) {
       beam_NextVar[i].next=&beam_NextVar[i+1];
     }
     beam_NextVar[max-1].next=NULL;
   }

   beam_varlocals=NULL;
   beam_USE_SAME_ANDBOX=NULL;
   beam_nr_alternative=NULL;
   beam_nr_call=NULL;
   beam_nr_gc_heap=0;
   beam_nr_gc_boxed=0;
   beam_Mode=READ;
   beam_VAR_TRAIL_NR=0;
   beam_nr_call_forking=0;
   beam_Mem_FULL=0;
#if Memory_Stat
        beam_TOTAL_MEM=0; beam_MEM_REUSED=0; beam_TOTAL_TEMPS=0; beam_TEMPS_REUSED=0; beam_TOTAL_PERMS=0; beam_PERMS_REUSED=0;
	memset(beam_Memory_STAT,0,MAX_MEMORYSTAT*5*sizeof(unsigned long));
#endif
}

INLINE int HEAP_MEM_FULL(void)
{
    if (beam_MemGoing==1) {
      if ((unsigned long)beam_H>(unsigned long)(beam_START_ADDR_HEAP+MEM_H/2)) {
	beam_Mem_FULL|=2;
      }
   } else {
      if ((unsigned long) beam_H>(unsigned long)(beam_START_ADDR_HEAP+MEM_H)) {
	beam_Mem_FULL|=2;
      }
   }

  return(beam_Mem_FULL);
}


INLINE Cell *request_memory(int size) /* size in bytes */
{
 register Cell  *mem;
 register int size_cells;

 if (size==0) return NULL;
 size_cells=size/CELL_SIZE;

#if !Fast_go
   if (size_cells> INDEX_SIZE)
      abort_eam("Foi pedido um block de memoria grande demais !!! \n");
#endif

#if Debug & Debug_MEMORY
      printf("Requesting memory size %d\n",size_cells);
#endif

#if HYBRID_BOXMEM
   mem=beam_IndexFree[(unsigned) size_cells];
 #if Memory_Stat
   beam_TOTAL_MEM+=size;
   if (mem!=NULL) beam_MEM_REUSED+=size;
 #endif
   if (mem==NULL) {

#else  /* GC Only */
   #if Memory_Stat
     beam_TOTAL_MEM+=size;
   #endif
   if (1) {
#endif

  #if GARBAGE_COLLECTOR!=2
       beam_NextFree-=size_cells;
       mem=beam_NextFree;
       if (beam_NextFree< (Cell *)  beam_START_ADDR_BOXES) abort_eam("No more BOX_MEM \n");
  #else
     if (beam_MemGoing==1) {
       mem=beam_NextFree;
       beam_NextFree+=size_cells;
       if (beam_NextFree> (Cell *) ( beam_START_ADDR_BOXES+MEM_BOXES/2)) beam_Mem_FULL |= 1;
     } else {
       beam_NextFree-=size_cells;
       mem=beam_NextFree;
       if (beam_NextFree< (Cell *) ( beam_START_ADDR_BOXES+MEM_BOXES/2)) beam_Mem_FULL |=1;
     }
  #endif
   } else {
     beam_IndexFree[(unsigned) size_cells]=(Cell *) *mem;
   }

#if Clear_MEMORY & 1
    memset(mem,0,size);  /* NOT REALLY NECESSARY, use only to detect possible errors*/
#endif

 return(mem);
}

#if HYBRID_BOXMEM==0
void free_memory(Cell *mem,int size) { 
  #if Clear_MEMORY & 2
    memset(mem,0,size); 
  #endif
};
#else
INLINE void free_memory(Cell *mem,int size) /* size in bytes */
{
    register int size_cells;

    if (size==0 || mem==NULL) return;

    size_cells=size/CELL_SIZE;

#if Clear_MEMORY & 2 
    memset(mem,0,size);  /* NOT REALLY NECESSARY, use only to detect possible errors*/
#endif

#if Debug & Debug_MEMORY
      printf("Freeing memory size %d\n",size_cells);
#endif

    *mem=(Cell) beam_IndexFree[size_cells];
    beam_IndexFree[size_cells]=mem;
}
#endif

INLINE void get_arguments(int nr, Cell *a)
{
register int i;
   for(i=1;i<=nr;i++) beam_X[i]=a[i];
}

INLINE Cell *save_arguments(int nr) /* nr arguments */
{
   if (!nr) return(NULL);
   {
        register int i;
        register Cell *a;

	a=(Cell *)request_memory((nr+1)*CELL_SIZE);
	a[0]=nr+1;  
        for(i=1;i<=nr;i++) a[i]=beam_X[i];
	return(a);
   } 
}

INLINE void remove_memory_arguments(Cell *a)
{
  if (a==NULL) return;
#if !Fast_go 
  if (a[0]<1 || a[0]>1000)
      printf("%d Numero Invalido de Argumentos............\n",a[0]);
#endif

  free_memory(a,a[0]*CELL_SIZE);
}

struct PERM_VAR *request_permVar(struct AND_BOX *a) {
struct PERM_VAR *pv;

#if Memory_Stat
  static struct PERM_VAR *old=NULL;
  beam_TOTAL_PERMS+=PERM_VAR_SIZE;
  if (old<=beam_NextVar) old=beam_NextVar;
  else beam_PERMS_REUSED+=PERM_VAR_SIZE;
#endif  

#if Debug && Debug_MEMORY
  printf("Requesting a permVar...\n");
#endif

#if !Fast_go
  if (beam_NextVar->next==NULL) { printf("Fim da memoria para variaveis\n"); exit (-1); }
#endif

  pv=beam_NextVar;
  beam_NextVar=beam_NextVar->next;

  pv->value=(Cell) &(pv->value);
  pv->home=a;
  pv->suspensions=NULL;
  pv->yapvar=NULL;
  pv->next=a->perms;
  a->perms=pv;
  return (pv);  
}

void free_permVar(struct PERM_VAR *v) {
#if Clear_MEMORY 
  v->value=(Cell) NULL;
  v->home=(struct AND_BOX *) NULL;
  v->suspensions=(struct SUSPENSIONS_VAR *) NULL;
#endif

#if Debug & Debug_MEMORY
  printf("Freeing a permVar...\n");
#endif

  v->next=beam_NextVar;
  beam_NextVar=v;
  return;
}


INLINE Cell *request_memory_locals(int nr)
{
Cell *l;
int i;

#if Memory_Stat
    Cell *old;
    old=beam_NextFree;
    beam_TOTAL_TEMPS+=CELL_SIZE*(nr+1); 
#endif

#if Debug_MEMORY
  printf("Requesting Memory for %d+1 locals...\n",nr);
#endif


    l=(Cell *)request_memory(CELL_SIZE*(nr+1));
    l[0]=nr;
    l++;

    for(i=0;i<nr;i++) {
      l[i]=(Cell) &l[i];
    }

#if Memory_Stat
    if (old==beam_NextFree) beam_TEMPS_REUSED+=CELL_SIZE*(nr+1); 
#endif

return(l);
}

INLINE Cell *request_memory_locals_noinit(int nr)
{
Cell *l;

#if Memory_Stat
    Cell *old;
    old=beam_NextFree;
    beam_TOTAL_TEMPS+=CELL_SIZE*(nr+1); 
#endif

#if Debug_MEMORY
  printf("Requesting Memory for %d+1 locals (not initialized)...\n",nr);
#endif


    l=(Cell *)request_memory(CELL_SIZE*(nr+1));
    l[0]=nr;
    l++;

#if Memory_Stat
    if (old==beam_NextFree) beam_TEMPS_REUSED+=CELL_SIZE*(nr+1); 
#endif

return(l);
}

INLINE void free_memory_locals(Cell *l)
{
  if (l==NULL || l[-1]==0) return;

#if Debug_MEMORY
  printf("Freeing Memory for %ld+1 locals...\n",l[-1]);
#endif

  free_memory((Cell *) &l[-1], CELL_SIZE*(l[-1]+1));
  l[-1]=0; /* é necessário para evitar apagar este vector novamente 
  porque varias calls podem estar a referenciar o mesmo vector locals */
}




/************************************************************************\
* Manipulating And-Or-Boxes structures				         *
\************************************************************************/



void del_andbox_and_sons(struct AND_BOX *andbox )
{
  register struct status_and *ncall;

if (andbox==NULL) return;

    remove_all_externals(andbox);
    delfrom_suspensions_list(andbox->suspended);

    ncall=andbox->calls;
    while(ncall!=NULL) {
      del_orbox_and_sons(ncall->call);
      {
        struct status_and *ncall_old;
	ncall_old=ncall;
        ncall=ncall->next;
	free_memory_locals(ncall_old->locals); 
        free_memory((Cell *) ncall_old,STATUS_AND_SIZE);
      }
    }
    remove_list_perms(andbox);
    free_memory((Cell *) andbox,ANDBOX_SIZE); 
}

void del_orbox_and_sons(struct OR_BOX *orbox)
{
struct status_or *so;
Cell *a=NULL;

if (orbox==NULL) return;
 
    so=orbox->alternatives; 
    while (so!=NULL) {
      struct status_or *old;
      del_andbox_and_sons(so->alternative);
      a=so->args;
      old=so;
      so=so->next;
      free_memory((Cell *) old,STATUS_OR_SIZE);
    }
    remove_memory_arguments(a); /* remove args */
    free_memory((Cell *) orbox,ORBOX_SIZE);
}


INLINE struct status_and *remove_call_from_andbox(struct status_and *ncall, struct AND_BOX *a) 
{
register int nr;
struct status_and *r;
      nr=a->nr_all_calls;
      nr--;
      a->nr_all_calls=nr;
      if (nr==0) {
	 a->calls=NULL;
      } else {
         if (ncall->previous!=NULL) {
	   ncall->previous->next=ncall->next;
	 } else a->calls=ncall->next;
	 
	 if (ncall->next!=NULL) {
	   ncall->next->previous=ncall->previous;
	 }
      }

      r=ncall->next;
      { /* vou ver se as locals ainda estao a ser usadas por outra ncall */
 	 int aSerUsada=0;
	 struct status_and *l;
	 l=ncall->previous;
	 while (l!=NULL) {
	   if (l->locals==ncall->locals) { aSerUsada=1; break; }
	   l=l->previous;
	 }
 	 l=r;
	 while (aSerUsada==0 && l!=NULL) {
	   if (l->locals==ncall->locals) { aSerUsada=1; break; }
	   l=l->next;
	 }
	 //	 aSerUsada=1; /* CUIDADO ao apagar as var locals da call */
	 if (aSerUsada==0) free_memory_locals(ncall->locals); 
      }
      free_memory((Cell *) ncall,STATUS_AND_SIZE);
      return(r);
}



INLINE void totop_suspensions_list(struct SUSPENSIONS *b)
{
  if (beam_su==b) return; /* is already on top of list */ 
  if (beam_su->prev==b) { beam_su=b; return; } /* It was the last one */

  b->prev->next=b->next;
  b->next->prev=b->prev;

  b->next=beam_su;
  b->prev=beam_su->prev;
  beam_su->prev=b;
  b->prev->next=b;
  beam_su=b;
}

void waking_boxes_suspended_on_var(struct PERM_VAR *v)
{
struct SUSPENSIONS_VAR *s;

   s=v->suspensions;
	  
   while(s!=NULL) {
	register struct AND_BOX *a;
#if Debug
	     printf("Waking and_box assigment changed on a var that forced and_box to suspend \n");
#endif
	a=s->and_box;
        totop_suspensions_list(a->suspended);
        a->nr_alternative->state|=WAKE;
	s=s->next;
   }
}

/* THE FALLOWING ROTINES ARE TO BE APPLYED TO THE SUSPENSION LIST
   (DO NOT USE IT TO THE SUSPENSIONS ON THE LOCAL_VAR)              */
INLINE struct SUSPENSIONS *addto_suspensions_list(struct AND_BOX *a,int r)
{
struct SUSPENSIONS *s;

  if (a->suspended) return(a->suspended); /* already suspended */

  s=(struct SUSPENSIONS *) request_memory(SUSPENSIONS_SIZE);    
  s->and_box=a;
  s->reason=r;
  if (beam_su==NULL) {
    s->next=s;
    s->prev=s;
    beam_su=s;
  } else {
    s->next=beam_su;
    s->prev=beam_su->prev;
    beam_su->prev=s;
    if (beam_su->next==beam_su) { /* so existem 2 elementos na lista */
      beam_su->next=s;
    } else {
      s->prev->next=s;
    }
  }

return(s);
}


void delfrom_suspensions_list(struct SUSPENSIONS *b)
{
  if (b==NULL) return;
#if !Fast_go
  if ( b->and_box->suspended==NULL) 
    abort_eam("Nunca deveria acontecer no delfrom_suspensions_list ?????\n");
#endif

  remove_all_externals_suspensions(b->and_box);
  b->and_box->suspended=NULL;

  if (b==beam_su) beam_su=b->next;

  if (b==beam_su) {  /* so existe um */ 
    beam_su=NULL;
  } else {
    b->prev->next=b->next;
    b->next->prev=b->prev;
  }
  free_memory((Cell *) b,SUSPENSIONS_SIZE);
}


INLINE void change_perm_var_suspensions(struct PERM_VAR *v,struct AND_BOX *andbox,struct AND_BOX *new)
{
struct SUSPENSIONS_VAR *s;

     s=v->suspensions; 
     while(s!=NULL)
     {
         if (s->and_box==andbox) {
	   s->and_box=new;
	   return;
	 }
         s=s->next;
     }
}





/* MANIPULATE PERM VARS SUSPENSIONS */

INLINE void remove_from_perm_var_suspensions(struct PERM_VAR *v,struct AND_BOX *andbox)
{
struct SUSPENSIONS_VAR *s,*prev=NULL;

if (v==NULL) {
  #if !Fast_go
    abort_eam("Nunca deveria acontecer no remove_from_perm_var_suspensions ?????\n");
  #endif 
  return;
}

     s=v->suspensions; 
     while(s!=NULL)
     {
       struct SUSPENSIONS_VAR *next;
         next=s->next;
         if (s->and_box==andbox) {
	   if (prev==NULL) {
	      v->suspensions=s->next;
	   } else prev->next=s->next;
	   free_memory((Cell *) s,SUSPENSIONS_VAR_SIZE);
         } else {  /* acordar as boxes restantes porque houve uma alteracao */
	   s->and_box->nr_alternative->state |=WAKE;
	   prev=s;
	 }
         s=next;
     }
}

void remove_all_externals_suspensions(struct AND_BOX *andbox)
{
struct EXTERNAL_VAR *e;

     e=andbox->externals;
     while(e) {
	      remove_from_perm_var_suspensions(e->var,andbox);    
	      e=e->next;
     }
}

void remove_all_externals(struct AND_BOX *andbox)
{
struct EXTERNAL_VAR *e;

     e=andbox->externals;
     while(e) {
              struct EXTERNAL_VAR *next;
	      next=e->next;
	      remove_from_perm_var_suspensions(e->var,andbox);    
	      free_memory((Cell *)e,EXTERNAL_VAR_SIZE);
	      e=next;
     }
}

void remove_list_perms(struct AND_BOX *a)
{
struct PERM_VAR *l,*oldl;

     l=a->perms;
     a->perms=NULL;
     while(l) {
       oldl=l;
       l=oldl->next;
       free_permVar(oldl);
     }
}


INLINE void move_perm_vars(struct AND_BOX *b, struct AND_BOX *a) /* (from b to a) */
{
struct PERM_VAR *l,*old;

  l=b->perms;
  if (l==NULL) return;
  do {
     old=l;
     l->home=a;
     if (l->suspensions) change_perm_var_suspensions(l,b,a);
     l=l->next;
  } while(l!=NULL);
  old->next=a->perms;
  a->perms=b->perms;
  return;
}

void add_to_list_perms(struct PERM_VAR *var,struct AND_BOX *a) 
{
  var->next=a->perms;
  a->perms=var;
  return;
}

/* change all suspended external references of perm var o to perm var d */
void change_from_to(struct PERM_VAR *o,struct PERM_VAR *d) {
struct SUSPENSIONS_VAR *s,*last;

#if Debug
   printf("Change Vars from one andbox to another\n"); 
#endif

  s=o->suspensions;
  if (s==NULL) return;  
  /* CUIDADO - Don't Forget to Write de Code to verify if they are compatible */
  /* second change the references of o to point to d, also change suspensions from o to d */
  do {
    struct EXTERNAL_VAR *e;
#if Debug
    struct SUSPENSIONS_VAR *l;
    l=d->suspensions;
    while(l!=NULL) {
      if (l->and_box==s->and_box) {
	printf("Same and-box binding... must check for compatibility.......!!!!");
      }
      l=l->next;
    }
#endif

    e=s->and_box->externals;
    while(e!=NULL) {
      if (e->var==o) {
	e->var=d;
      }
      e=e->next;
    }

    last=s; 
    s=s->next;
  } while(s);
  last->next=d->suspensions;
  d->suspensions=o->suspensions;
  o->suspensions=NULL;

}


/************************************************************************\
* Other routines  						         *
\************************************************************************/

void inc_level(struct AND_BOX *andbox,int dif)
{
struct OR_BOX *orbox;
struct status_and *calls;

     if (andbox==NULL) return;

     andbox->level+=dif;
     calls=andbox->calls;
     while(calls!=NULL) {
         orbox=calls->call;
	 if (orbox!=NULL) {
   	     struct status_or *so;
	     so=orbox->alternatives;
	     while (so!=NULL) {
	       inc_level(so->alternative,dif);
	       so=so->next;
	     }
	 }
         calls=calls->next;
     }
}


INLINE int is_leftmost(struct AND_BOX *a, struct status_and *n)
{
  if (a==beam_top) return(1);
  if (a->calls!=n) return(0);
  if (a->nr_alternative->previous!=NULL) return(0);

return(is_leftmost(a->parent->parent,a->parent->nr_call));
}

struct AND_BOX *choose_leftmost(void)
{
  struct AND_BOX *a;
  struct OR_BOX *o=NULL;
  struct status_and *ncall;
  
  a=beam_top;
  do {
    ncall=a->calls;
    if (ncall==NULL) break;
    while(ncall!=NULL) {
      o=ncall->call;
      if (o!=NULL) break;
      ncall=ncall->next;
    }
    if (ncall==NULL) break;
    a=o->alternatives->alternative;
    if (a==NULL) { beam_OBX=o; return(a); }
  } while(1);

return a;
}

INLINE unsigned int index_of_hash_table_atom(Cell c, int nr)
{
return (((unsigned long) c >>3) % nr); 
}

INLINE unsigned int index_of_hash_table_appl(Cell c, int nr)
{
return (((unsigned long) c >>5) % nr);
}


/************************************************************************\
* Unification routines  						 *
\************************************************************************/

void trail(struct AND_BOX *andbox,struct PERM_VAR *v)
{
register struct EXTERNAL_VAR *e;
int var_level;

  if (!is_perm_var((Cell *) v)) return;
  var_level=(v->home)->level;
  if (var_level>=andbox->level) { /* Don't Need to Trail */
      waking_boxes_suspended_on_var(v);  /* Really Not Needed, just to speedup avoiding forks */
      if (isvar(v->value)) {  /* CUIDADO posso ter de fazer deref primeiro */
	change_from_to(v,(struct PERM_VAR *) *((Cell *) v->value));
      }
      return;
  }

#if Debug
  printf("Trailing var 0x%lX on ANDBOX 0x%lX\n", (unsigned long) v, (unsigned long) andbox);
#endif
  e=(struct  EXTERNAL_VAR *) request_memory(EXTERNAL_VAR_SIZE);
  e->next=andbox->externals;
  andbox->externals=e;
  e->var=v;
  e->value=v->value; 
}

INLINE int deve_limpar_var(struct EXTERNAL_VAR *e)
{
  return(e->var->value==e->value && isvar(e->var) ); /* ????? */
}

void limpa_trail(struct AND_BOX *andbox)
{
struct EXTERNAL_VAR *e;
Cell *l;

  if (andbox==NULL) return; 

  e=andbox->externals;
  while(e!=NULL) {
    if (deve_limpar_var(e)) {  
	l=(Cell *) e->var;
	*((Cell *) l)=(Cell) l;
    }
    e=e->next;
  }
  { register struct status_and *ncall;
    ncall=andbox->calls;
    while(ncall) {
      register struct OR_BOX *o;
      o=ncall->call;
      if (o!=NULL) {
	   struct status_or *so;
	   so=o->alternatives;
	   while (so!=NULL) {
	     limpa_trail(so->alternative);
	     so=so->next;
	   }
      }
      ncall=ncall->next;
    }
  }
}


INLINE void limpa_trail_orbox(struct OR_BOX *orbox)
{
struct status_or *so;
    
    so=orbox->alternatives;
    while(so!=NULL) {
      limpa_trail(so->alternative);
      so=so->next;
    }
}

INLINE Cell deref(Cell a)
{
  register Cell *b;

   while(isvar(a)) {
	b = (Cell *) a;
	a = *b;
	if(a==((Cell) b)) return a;
   }
   return a;
}

void UnifyCells(Cell *a, Cell *b) /* a e b variaveis  */
{

     if(a==b) return;
     if (is_perm_var(a)) {
       if (is_perm_var(b)) {
         register int i,j;
	 i=((struct PERM_VAR *) a)->home->level;
	 j=((struct PERM_VAR *) b)->home->level;
	 if (i<j) {
	   *b=(Cell) a;
	   trail(beam_ABX,(struct PERM_VAR *) b);
	   return;
	 } else {
	   *a=(Cell) b;
	   trail(beam_ABX,(struct PERM_VAR *) a);
	   return;
	 }
       } else {
	 *b=(Cell) a;
	 return;
       }
     }
     *a=(Cell) b;
     return;
}

int Unify(Cell *a, Cell *b)
{
   a = (Cell *) deref((Cell) a);
   b = (Cell *) deref((Cell) b);
   if(isvar(a)) {
	if(isvar(b)) {
		UnifyCells(a,b);
		return 1;
	}
        { *a=(Cell) b; trail(beam_ABX,(struct PERM_VAR *)a); }
	return 1;
    }
    if(isvar(b)) {
        { *b=(Cell) a; trail(beam_ABX,(struct PERM_VAR *)b); }
	return 1;
    }
    if(a==b) return 1;
    if(isappl(a)) {
	int arity;
	if(!isappl(b)) return 0;
	a = (Cell *) repappl(a);
	b = (Cell *) repappl(b);
	if(*a != *b) return 0;
	arity = ((int) ArityOfFunctor((Functor) *a));
	while(arity!=0) {
	   if(!Unify((Cell *)a[arity], (Cell *)b[arity])) return 0;
	   --arity;
	}
	return 1;
    }
    if(ispair(a)) {
	if(!ispair(b)) return 0;
	a = (Cell *) reppair(a);
	b = (Cell *) reppair(b);
	if(!Unify((Cell *)*a,(Cell *)*b)) return 0;
	return Unify((Cell *)a[1],(Cell *) b[1]);
    }
    if(a!=b) return 0;
    return 1;
}



int verify_externals(struct AND_BOX *andbox)
{
struct EXTERNAL_VAR *e,*prev;

#if Debug
    printf("Entering Verify Externals \n");
#endif
    e=andbox->externals;
    prev=NULL;

    while(e) {
      Cell d;
      d=deref((Cell) e->var); /* e->var->value */
      if (!isvar(d)) {   /* ja nao e' var */
	if (isvar(e->value)) {
	   struct PERM_VAR *old,*new;
	   struct SUSPENSIONS_VAR *s;

	   old=e->var;
	   new=(struct PERM_VAR *) e->value;
	   e->var=new;
	   e->value=(Cell) old;
	   remove_from_perm_var_suspensions(old,andbox);
	   s=(struct SUSPENSIONS_VAR *) request_memory(SUSPENSIONS_VAR_SIZE);
	   s->and_box=andbox;
	   s->next=new->suspensions;
	   new->suspensions=s;	   
	   if (e->var->home->level==andbox->level) {   /* ja nao e' uma var externa  */
	          e->var->value=e->value;	   
		  goto tudo_ok;
	   }
           prev=e;
           e=e->next;
	   continue;
	} else {
	   if (Unify((Cell *) d,(Cell *) e->value))  { 
	     /* Preciso de ter cuidado pois podem ter sido criadas External Vars */
	     if (prev==NULL && andbox->externals!=e) { 
		 prev=andbox->externals;
		 while (prev->next!=e) prev=prev->next;
	     }
	     goto tudo_ok;
	   }
#if Debug
 	   printf("Verify Externals Has failed \n");
#endif
	   return(0);
	}
      } else {       /* ainda e' var */
	if (e->var->home->level==andbox->level) {  /* ja nao e' uma var externa  */
	  /*	   e->var->value=e->value; */
	  *((Cell *) d)=e->value;
        tudo_ok:  
	   /* primeiro remover a andbox da lista de suspensoes da variavel */
	   remove_from_perm_var_suspensions(e->var,andbox);
           waking_boxes_suspended_on_var(e->var);
	   
	   /* remover a variavel da lista de externals */
	   if (prev==NULL) {  /* a var e' a primeira da lista */
	      andbox->externals=e->next;
	      free_memory((Cell *)e,EXTERNAL_VAR_SIZE);
	      e=andbox->externals;
	      continue;
	   } else {          
	      prev->next=e->next;
	      free_memory((Cell *)e,EXTERNAL_VAR_SIZE);
	      e=prev->next;
	      continue;
	   }
	}   
      }
      
      prev=e;
      e=e->next;
    }

    if (andbox->externals==NULL) { /* Se ja nao ha external vars posso remover andbox da lista suspensions */
          delfrom_suspensions_list(andbox->suspended);
    }
#if Debug
    printf("Verify Externals Has ended with Sucess\n");
#endif

return(1); /* Means OK */
}

int exists_var_in(Cell *c) 
{
Cell *C, *OldC;

  OldC=(Cell *) deref((Cell) c);

  if (isvar(OldC)) {
    return(1);
  }
  if (isatom(OldC)) { 
    return(0);
  }

  if (ispair(OldC)) {
     C=(Cell *) reppair(OldC);
     return(exists_var_in(C) || exists_var_in(++C));
  }

return(0);
}


/************************************************************************\
 * Emulador de EAM 					                 *
\************************************************************************/


void give_solution_toyap(void);
void give_solution_toyap(void) {
    struct PERM_VAR *l;
    l=beam_ABX->perms;
    while(l) {
	if (l->yapvar) {
	  *TR=(Cell) l->yapvar;
	  TR++;
	  *(l->yapvar)=l->value;
	}
	l=l->next;
    }
}

void add_vars_to_listperms(struct AND_BOX *a, Cell *arg);
void add_vars_to_listperms(struct AND_BOX *a, Cell *arg) {
Cell *_DR;
Cell *NewC;

    _DR=(Cell *) deref((Cell) arg);
    if (isvar((Cell *) _DR) && !is_perm_var(_DR)) {
	    struct PERM_VAR *l;
	    l=request_permVar(a);
	    l->yapvar=_DR;
	    *_DR=(Cell) l;
    }
    if (isappl(_DR)) {
      int i,arity; 

      NewC=(Cell *) repappl(_DR);
      arity = ((int) ArityOfFunctor((Functor) *NewC));
      for(i=0;i<arity ;i++) {
        NewC++;
	add_vars_to_listperms(a,NewC);
      }
    }
    if (ispair(_DR)) {
      NewC=(Cell *) reppair(_DR);
      add_vars_to_listperms(a,NewC);
      NewC++;
      add_vars_to_listperms(a,NewC);
      NewC++;
    }

    /* é atomic, posso terminar */
}

PredEntry *prepare_args_torun(void);
PredEntry *prepare_args_torun(void) {
Cell *_DR;
Prop pe;
PredEntry *ppe;

  /* at this time, ARG1=call */
    _DR=(Cell *) deref(beam_X[1]);

    if (isatom(_DR)) {
/*      char *name = AtomOfTerm((Term) _DR)->StrOfAE; */

      pe = PredPropByAtom(AtomOfTerm((Term) _DR), CurrentModule);
      ppe = RepPredProp(pe);

      return (ppe);
    }

    if (isappl(_DR)) {
/*      char *name = (NameOfFunctor((Functor) *NewC))->StrOfAE;  */
      int i, arity;
      Functor f = FunctorOfTerm((Term) _DR);
      if (IsBlobFunctor(f)) {
        Yap_Error(TYPE_ERROR_CALLABLE,(Term) _DR,"call/1");
        return(FALSE);
      }
      pe = PredPropByFunc(f, CurrentModule);
      ppe = RepPredProp(pe);

      _DR=(Cell *) repappl(_DR);
      arity = ArityOfFunctor(f);

      for(i=1;i<=arity ;i++) {
	  _DR++;
	  beam_X[i]=(Cell) _DR;
      }
      return (ppe);
    }
    
return (NULL);
}

#if DIRECT_JUMP
     #define execute_next() goto **((void **) beam_pc)
     Cell *TABLE_OPS=NULL;
#else
     #define execute_next()  goto *OpAddress[*beam_pc]
#endif



int eam_am(PredEntry *initPred);
int eam_am(PredEntry *initPred)
{
static void *OpAddress[]= {
        &&exit_eam,
        &&top_tree,
	&&scheduler,
	&&prepare_tries,
	&&prepare_calls,
        &&get_var_X,
        &&get_var_Y,             
        &&get_val_X,                     
        &&get_val_Y,             
        &&get_atom,
        &&get_list,       
        &&get_struct,     
        &&unify_void,        
        &&unify_val_X,       
        &&unify_val_Y,       
        &&unify_var_X,       
        &&unify_var_Y,       
        &&unify_atom,        
        &&unify_list,        
        &&unify_last_list,   
        &&unify_struct,      
        &&unify_last_struct, 
	&&unify_last_atom,
        &&unify_local_X,     
        &&unify_local_Y,     
        &&put_var_X,       
        &&put_var_Y,       
        &&put_val_X,       
        &&put_val_Y,       
        &&put_atom,        
        &&put_list,        
        &&put_struct,      
        &&put_unsafe, 
	&&put_var_P,
        &&write_void,          
        &&write_var_X,     
        &&write_var_Y,     
        &&write_val_X,     
        &&write_val_Y,     
        &&write_atom,      
        &&write_list,      
        &&write_struct,    
        &&write_last_list, 
        &&write_last_struct,
        &&write_local_X,   
        &&write_local_Y,   
	&&write_var_P,
        &&pop,         
        &&jump,        
        &&proceed,     
        &&call,        
        &&safe_call,   
        &&safe_call_unary,   
        &&safe_call_binary,   
        &&only_1_clause, 
        &&try_me,      
        &&retry_me,    
        &&trust_me,    
        &&do_nothing,
	&&direct_safe_call,
	&&direct_safe_call_unary,
	&&direct_safe_call_binary,
	&&skip_while_var,
	&&wait_while_var,
	&&force_wait,
	&&write_call,
	&&is_call,
	&&equal_call,
        &&cut,
	&&commit,
        &&fail,        
        &&save_b_X,    
        &&save_b_Y,    
        &&comit_b_X,   
        &&comit_b_Y,   
        &&save_appl_X,  
        &&save_appl_Y,
        &&save_pair_X,  
        &&save_pair_Y,
	&&either,
        &&orelse,
        &&orlast,
        &&p_atom,
        &&p_atomic,
        &&p_equal,
        &&p_integer,
        &&p_nonvar,
        &&p_number,
        &&p_var,
        &&p_db_ref,
        &&p_primitive,
        &&p_cut_by,
        &&p_save_by,
        &&p_succ,
        &&p_predc,
        &&p_plus,
        &&p_minus,
        &&p_times,
        &&p_div,
        &&p_dif,
        &&p_eq,
        &&p_arg,
        &&p_functor
};
#if Debug
static int contador=1;
#endif
Cell code2start[]={_prepare_calls,1,0,_call_op,0,0};


    	if ((long) initPred==2) { /* retry from call eam(goal) */
	   goto fail; 
        } else if ((long) initPred==1) { /* first time call eam(goal) */
	   initPred=prepare_args_torun();  
        } 
#if DIRECT_JUMP
        else if ((long) initPred==0) { /* first time call eam_am. Init TABLE_OPS */
	  TABLE_OPS=(Cell *) OpAddress;
	  return(FALSE);
        } 
#endif
	if (initPred==NULL || initPred->beamTable==NULL) return (FALSE);

#if DIRECT_JUMP
	code2start[0]=(Cell) OpAddress[_prepare_calls];
	code2start[3]=(Cell) OpAddress[_call_op];
#endif

	code2start[2]=(Cell) &code2start[5];
	code2start[4]=(Cell) initPred;

        printf("[ EAM execution started to solve %s/%d ]\n",
	        initPred->beamTable->name, initPred->beamTable->arity );

	initialize_memory_areas();

	beam_su=NULL;
	beam_OBX=NULL;
	beam_ABX=(struct AND_BOX *) request_memory(ANDBOX_SIZE);
	beam_ABX->parent=NULL;
	beam_ABX->nr_alternative=NULL;
	beam_ABX->nr_all_calls=0;
	beam_ABX->perms=NULL;
	beam_ABX->calls=NULL;
	beam_ABX->level=1;
	beam_ABX->externals=NULL;
	beam_ABX->suspended=NULL;
	beam_ABX->side_effects=0;
	beam_top=beam_ABX;

if (1) { int i;  /* criar mais um nivel acima do top para o caso de haver variaveis na chamada */
	beam_ABX->nr_all_calls=1;
        beam_ABX->calls=  (struct status_and *) request_memory(STATUS_AND_SIZE);
	beam_ABX->calls->locals=NULL;
	beam_ABX->calls->code=NULL;
	beam_ABX->calls->state=RUNNING;
	beam_ABX->calls->previous=NULL;
	beam_ABX->calls->next=NULL;
        beam_OBX= (struct OR_BOX *) request_memory(ORBOX_SIZE);
	beam_ABX->calls->call=beam_OBX;
	beam_OBX->nr_call=beam_ABX->calls;
	beam_OBX->parent=beam_ABX;
	beam_OBX->nr_all_alternatives=1;
	beam_OBX->eager_split=0;

	beam_OBX->alternatives=(struct status_or *) request_memory(STATUS_OR_SIZE);
	beam_OBX->alternatives->previous=NULL;
	beam_OBX->alternatives->next=NULL;
	beam_OBX->alternatives->args=NULL;
	beam_OBX->alternatives->code=NULL;
	beam_OBX->alternatives->state=RUNNING;

	beam_ABX=(struct AND_BOX *) request_memory(ANDBOX_SIZE);
	beam_OBX->alternatives->alternative=beam_ABX;
	beam_ABX->parent=beam_OBX;
	beam_ABX->nr_alternative=beam_OBX->alternatives;
	beam_ABX->nr_all_calls=0;
	beam_ABX->perms=NULL;
	beam_ABX->calls=NULL;
	beam_ABX->level=2;
	beam_ABX->externals=NULL;
	beam_ABX->suspended=NULL;
	beam_ABX->side_effects=WRITE;
	
	for(i=1;i<=initPred->beamTable->arity;i++) 
                add_vars_to_listperms(beam_ABX,(Cell *) beam_X[i]);
}

	beam_pc=code2start;
	execute_next();

	while (1) {

               exit_eam:
#if Debug
			printf("%5d->(%3d) exit_eam ->",contador++, (int) *beam_pc); 
break_debug(contador);
#endif		        

                wake:
#if Debug
			printf("%5d->Trying WAKE and_box on suspension \n",contador++);
break_debug(contador);
#endif
		        if (verify_externals(beam_ABX)==0) goto fail_verify_externals;
			if (beam_ABX->externals==NULL) {
			              beam_nr_call=beam_ABX->calls;
				      if (beam_nr_alternative->state & END) {
					  goto success;
			              } 
				      beam_nr_alternative->state=RUNAGAIN;
				      goto next_call;
			 }
			 beam_nr_alternative->state=SUSPEND;
			 /* must clear all external assignments */
			 limpa_trail(beam_ABX);
			 /* goto top_tree; */

	       top_tree:
#if Debug
		        printf("%5d->I'm on top of the Tree (maybe exit or look for suspended alternatives) \n",contador++);
break_debug(contador);
break_top();
#endif		        

#if GARBAGE_COLLECTOR
			if (HEAP_MEM_FULL()) garbage_collector();
#endif

#if USE_LEFTMOST
		      if (beam_su!=NULL) {
			 beam_ABX=beam_su->and_box;
		         beam_OBX=beam_ABX->parent;
		         beam_nr_alternative=beam_ABX->nr_alternative;
		         if (beam_nr_alternative->state & (WAKE))  goto wake;			 
		       }
		       beam_ABX=choose_leftmost();
		       if (beam_ABX==NULL) { /* Must return to next_alternative in beam_OBX  BECAUSE EAGER_SPLIT*/
			 beam_nr_alternative=beam_ABX->nr_alternative;
			 beam_ABX=beam_OBX->parent;
			 goto  next_alternative;
		       }
		       if (beam_ABX!=beam_top && beam_ABX->suspended!=NULL) {
#else
			if (beam_su!=NULL) { /* There are suspended alternatives */
			  beam_ABX=beam_su->and_box;
#endif

#if !Fast_go
			  if (beam_ABX==NULL || beam_ABX->parent==NULL || beam_ABX->parent->alternatives==NULL) abort_eam("Alternativa NULL NO TOP ?????"); 
#endif
			  beam_OBX=beam_ABX->parent;
			  beam_nr_alternative=beam_ABX->nr_alternative;

			  if (beam_ABX->suspended->reason==VAR_SUSPENSION) {
                                delfrom_suspensions_list(beam_ABX->suspended);
			        beam_nr_call=beam_ABX->calls;
			        goto next_call;			     
			  }
			  if (beam_ABX->suspended->reason!=NORMAL_SUSPENSION) {
			     if (beam_ABX->calls->state==WAITING_TO_BE_FIRST ||
				 (beam_ABX->calls->state & WAITING && is_leftmost(beam_ABX,0))) {

                                delfrom_suspensions_list(beam_ABX->suspended);
			        beam_ABX->calls->state=READY;
			        beam_nr_call=beam_ABX->calls;
			        goto next_call;
			     }
#if !USE_LEFTMOST
			     beam_su=beam_su->next;
			     goto top_tree;
#endif
			  }

			  if (beam_OBX->nr_all_alternatives==1 && beam_ABX->level>beam_OBX->parent->level) {
#if !Fast_go
			    if (beam_OBX->parent->parent==NULL) abort_eam("Null no top_tree ");
#endif
			    goto unique_alternative;
			  }
			  if (beam_nr_alternative->state & (WAKE))  goto wake;
			  if (beam_OBX->nr_all_alternatives>1) {
#if Debug
			     printf("%5d->Trying Fork in suspended and_box \n",contador++);
break_debug(contador);
#endif
			     /* pickup the left most alternative instead */
		 split:			     
			     beam_OBX=beam_ABX->parent;
#if USE_SPLIT
			     do_forking_andbox(beam_ABX);
#else
			     abort_eam("ERROR: Split disable, cannot run non-deterministic programs...");
#endif
			     beam_OBX=beam_ABX->parent;
			     beam_nr_alternative=beam_ABX->nr_alternative;
			     goto unique_alternative;
			  } 
			  
			  abort_eam("ERROR: exit on top, suspensions still available");
			} 
			/* There is no suspension */
			give_solution_toyap(); 
			return (TRUE);
			exit_eam("\nExit on top, there is no more work to do... \n");

		proceed:
#if Debug
		        printf("%5d->proceed... \n",contador++);
#endif

			if (beam_USE_SAME_ANDBOX!=NULL) {  /* was only one alternative */
			  beam_USE_SAME_ANDBOX=NULL;
			  beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
			  goto next_call;
			} 
			if (beam_ABX->externals!=NULL) {
			    beam_nr_alternative->state=SUSPEND_END;
			    goto suspend;
			}

	        success:
#if Debug
			printf("%5d->SUCCESS for call %p  in level %d \n",contador++, beam_nr_call, beam_ABX->level );
break_debug(contador);
#endif
			/* FOUND SOLUTION -> ALL_SOLUTIONS */ 
			//if ((beam_ABX->side_effects & WRITE) && beam_OBX->nr_all_alternatives>1) 
			  if (beam_OBX->parent==beam_top) {  
			      give_solution_toyap(); 
			      return (TRUE); 
			      goto fail; 
			  }

			beam_ABX=beam_OBX->parent;
			beam_nr_call=beam_OBX->nr_call;
			del_orbox_and_sons(beam_OBX);  
			beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);

			if (beam_ABX->externals!=NULL) {
			    if (beam_ABX->nr_all_calls==0) {
			         beam_nr_alternative->state=SUSPEND_END;
			    } else beam_nr_alternative->state=SUSPEND;
			    goto suspend;			    
			}

			if (beam_ABX->nr_all_calls==0) {
			    beam_OBX=beam_ABX->parent;

			    if (beam_OBX==NULL) {
			      goto top_tree;
			    }
			    beam_nr_alternative=beam_ABX->nr_alternative;
			    goto success;
			}

	        next_call:
#if Debug
		        printf("%5d->Searching for a next call in and_box... \n",contador++);
break_debug(contador);
#endif

#if GARBAGE_COLLECTOR
			if (HEAP_MEM_FULL()) { 
			    garbage_collector();
			}
#endif

	                { register int nr;
			nr=beam_ABX->nr_all_calls;

			if (beam_ABX->externals!=NULL && beam_ABX->side_effects<CUT) {
			    if (nr==0) beam_nr_alternative->state=SUSPEND_END;
			    else { /* if next call is a cut then execute it */
			      beam_pc=beam_ABX->calls->code;
#if Debug
			      if (*beam_pc==_cut_op) {
#else
			      if (*beam_pc==(Cell) &&cut) { 
#endif
				beam_nr_call=beam_ABX->calls;
			        execute_next();				
			      }
			      beam_nr_alternative->state=SUSPEND; 
			    }
			    goto suspend;	
			}			  
			if (nr==0) {
			  goto success;
			}
#if !START_ON_NEXT
			beam_nr_call=beam_ABX->calls;
#else
/*			if (beam_ABX->parent==beam_OBX) beam_nr_call=beam_ABX->calls; else beam_nr_call=beam_OBX->nr_call->next;  */
#endif
			while(beam_nr_call!=NULL) {
			  
			   if (beam_nr_call->state & WAITING) {
			     if (beam_nr_call->state==WAITING_TO_BE_LEFTMOST) {
			       if (!is_leftmost(beam_ABX,beam_nr_call)) {
				    beam_ABX->suspended=addto_suspensions_list(beam_ABX,LEFTMOST_SUSPENSION);
			            beam_nr_call=NULL;
			            break;			       
			       }
			       beam_nr_call->state=READY;
			     }

			     if (beam_nr_call->state==WAITING_TO_BE_LEFTMOST_PARENT) {
			       if (!is_leftmost(beam_ABX->parent->parent,beam_ABX->parent->nr_call)) {
				    beam_ABX->suspended=addto_suspensions_list(beam_ABX,LEFTMOST_SUSPENSION);
			            beam_nr_call=NULL;
			            break;			       
			       }
			       beam_nr_call->state=READY;
			     }

			     if (beam_nr_call->state==WAITING_TO_BE_FIRST) {
			            if (beam_nr_call->previous==NULL) { 
#if Debug
			               printf("I can stop Waiting on call %p\n", beam_nr_call);
#endif
				       beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
				       continue;
			            }
#if Debug
			            printf("Force Waiting on call %p\n", beam_nr_call);
#endif
			            beam_nr_call=NULL;
			            break;
			     }
			   }
			   if (beam_nr_call->state==READY) {
			     beam_varlocals=beam_nr_call->locals;
			     beam_pc=beam_nr_call->code;
			     execute_next();
			   }
			   beam_nr_call=beam_nr_call->next;
			}
			beam_OBX=beam_ABX->parent;
			/* In case (beam_nr_call==nr) */

			beam_nr_alternative=beam_ABX->nr_alternative;
  			if (beam_ABX->externals!=NULL) goto suspend;

			if (beam_nr_alternative!=NULL) beam_nr_alternative=beam_nr_alternative->next;
			goto next_alternative;
			}

	        fail_body:
	        fail_head:
	        fail:
#if Debug
		        printf("%5d->fail... \n",contador++);
break_debug(contador);
#endif

	        fail_verify_externals:
			if (beam_ABX->externals!=NULL) { 
			     limpa_trail(beam_ABX);
			}

			beam_OBX=beam_ABX->parent;
			beam_nr_alternative=beam_ABX->nr_alternative;
			if (beam_OBX==NULL) {
			  if (beam_ABX==beam_top) return(FALSE);
			  abort_eam("ERROR ->  beam_ABX->parent = NULL  (em fail_verify_externals) ?????\n");
			}

			beam_OBX->nr_all_alternatives=beam_OBX->nr_all_alternatives-1;
			if (beam_nr_alternative->next!=NULL) beam_nr_alternative->next->previous=beam_nr_alternative->previous;
			if (beam_nr_alternative->previous!=NULL) beam_nr_alternative->previous->next=beam_nr_alternative->next;
			else beam_OBX->alternatives=beam_nr_alternative->next;  /* apaguei o primeiro da lista */
		      { register struct status_or *i;
			i=beam_nr_alternative;
			beam_nr_alternative=beam_nr_alternative->next;
			free_memory((Cell *) i,STATUS_OR_SIZE);
  		        del_andbox_and_sons(beam_ABX);
  		      }	/* verificar se existe ainda alguma alternativa viavel nesta or_box */

	        next_alternative:
#if Debug
		        printf("%5d->Searching for a next alternative in or_box... \n",contador++);
break_debug(contador);
#endif

#if GARBAGE_COLLECTOR
			if (HEAP_MEM_FULL()) garbage_collector();
#endif

			if (beam_OBX==NULL) {
#if !Fast_go
			      if (beam_ABX!=beam_top) abort_eam("Erro no next_Alternative");
#endif
			  goto top_tree;
			}

			if (beam_OBX->nr_all_alternatives==0) {
			  beam_ABX=beam_OBX->parent;
			  goto fail;
			} 
			if (beam_OBX->nr_all_alternatives==1 && beam_ABX->level>beam_OBX->parent->level) {
			    beam_nr_alternative=beam_OBX->alternatives;
			    beam_ABX=beam_OBX->alternatives->alternative;
			    if (beam_ABX==NULL) {
			      beam_pc=beam_OBX->alternatives->code;
			      execute_next();
			    }
      		            if (beam_OBX->parent->parent==NULL) goto top_tree;
			    goto unique_alternative;
			}
#if !START_ON_NEXT
			beam_nr_alternative=beam_OBX->alternatives;
#else
			/*			if (beam_OBX->parent==beam_ABX) beam_nr_alternative=beam_OBX->alternatives; 
						else { if (beam_nr_alternative!=NULL) beam_nr_alternative=beam_nr_alternative->next; }  */
#endif
			while(beam_nr_alternative!=NULL) {
			   if (beam_nr_alternative->state & (WAKE) ) {
			      beam_ABX=beam_nr_alternative->alternative;
			      goto wake;
			   }
			   if (beam_nr_alternative->state==READY) {
			       beam_pc=beam_nr_alternative->code;
		 	       execute_next();
			   }
			   beam_nr_alternative=beam_nr_alternative->next;
			}

			/* beam_nr_alternative==NULL -> No more alternatives */
			beam_ABX=beam_OBX->parent;
			beam_nr_call=beam_OBX->nr_call->next;
			goto next_call;

	        unique_alternative:  
#if Debug
			printf("%5d->Unique alternative, Does Promotion on and-box\n",contador++);
break_debug(contador);
#endif

#if GARBAGE_COLLECTOR
			if (HEAP_MEM_FULL() ) garbage_collector();
#endif
			if (beam_OBX->parent->parent==NULL) {
			   goto top_tree;
			}

			{ int nr_a;
			  struct AND_BOX *a;
			  if (beam_ABX->side_effects >= CUT) { 
			      /* Cut -> Avoid doing the Promotion */
			      inc_level(beam_ABX,beam_OBX->parent->level-beam_ABX->level);

			      delfrom_suspensions_list(beam_ABX->suspended); 
		              if (verify_externals(beam_ABX)==0) goto fail_verify_externals; 
			      beam_nr_alternative=beam_ABX->nr_alternative;
			      if (beam_ABX->externals==NULL) {
				beam_nr_call=beam_ABX->calls;
				goto next_call;
			      } 
			      beam_ABX->suspended=addto_suspensions_list(beam_ABX,NORMAL_SUSPENSION);
			      beam_nr_alternative->state=SUSPEND;
			      beam_nr_alternative=beam_nr_alternative->next;
			      goto next_alternative;
			  } 
			  a=beam_ABX;
			  beam_ABX=beam_OBX->parent;
			  nr_a=a->nr_all_calls;
			  beam_nr_call=beam_OBX->nr_call;
			  beam_ABX->side_effects+=a->side_effects;
			  if (nr_a==0) {  /* Means SUSPENDED ON END */
			      beam_nr_call->call=NULL;
			      beam_nr_call->state=SUCCESS;
			      beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
			  } else {  /* IF nr_all_calls==1 can be optimized ????? */
			      if (nr_a==1) {
				
				if (a->calls->call!=NULL) {
				    a->calls->call->nr_call=beam_nr_call;
				    a->calls->call->parent=beam_ABX;
				}
				beam_nr_call->call=a->calls->call;
				beam_nr_call->locals=a->calls->locals;
				beam_nr_call->code=a->calls->code;
				beam_nr_call->state=a->calls->state;
				free_memory((Cell *) a->calls,STATUS_AND_SIZE);
			      } else {
				struct status_and *first, *last;
			        int nr;

			        nr=beam_ABX->nr_all_calls;
				
				first=a->calls;
				last=a->calls;
				while(1) {
				  if (last->call!=NULL) {
				    last->call->parent=beam_ABX;
				  }
				  if (last->next==NULL) break;
				  last=last->next;
				}
				last->next=beam_nr_call->next;
				if (beam_nr_call->next!=NULL) beam_nr_call->next->previous=last;
				first->previous=beam_nr_call->previous;
				if (beam_nr_call->previous!=NULL) beam_nr_call->previous->next=first;
			        else beam_ABX->calls=first; /* nr_call era o primeiro */
		                free_memory((Cell *) beam_nr_call,STATUS_AND_SIZE);
				beam_nr_call=first;
			        beam_ABX->nr_all_calls=nr+nr_a-1;
			      }
			      /* Set local vars from a to point to new and_box beam_ABX */
			  } 
			  move_perm_vars(a,beam_ABX);

			    /* change local vars suspensions to point to new andbox */
			  { struct EXTERNAL_VAR *end,*e;
			    e=a->externals;
			    end=NULL;
			    while(e!=NULL) {
			      struct SUSPENSIONS_VAR *s;
			      s=e->var->suspensions;
			      while(s!=NULL) {
				if (s->and_box==a) { s->and_box=beam_ABX; break; }
				s=s->next;
			      }
			      end=e;
			      e=e->next;
			    }
			    /* Clear bindings made on externals so that we are able to
			       run the verify externals */
			    e=beam_ABX->externals;
			    while(e!=NULL) {
			      struct PERM_VAR *v;
			      v=e->var;
			      *((Cell *) v)=(Cell) v;
			      e=e->next;
			    }
			    if (end!=NULL) {
				end->next=beam_ABX->externals;
				beam_ABX->externals=a->externals;
			    }

			    delfrom_suspensions_list(a->suspended); /* remove suspensions */
			    free_memory((Cell *) a,ANDBOX_SIZE);
			    free_memory((Cell *) beam_OBX->alternatives,STATUS_OR_SIZE);
			    free_memory((Cell *) beam_OBX,ORBOX_SIZE);

			    beam_OBX=beam_ABX->parent;
		            if (verify_externals(beam_ABX)==0) goto fail_verify_externals; 
			  }

			    beam_nr_alternative=beam_ABX->nr_alternative;
			    if (beam_ABX->externals==NULL) {
				beam_nr_call=beam_ABX->calls;
				goto next_call;
			    } 
			    beam_ABX->suspended=addto_suspensions_list(beam_ABX,NORMAL_SUSPENSION);
			    beam_nr_alternative->state=SUSPEND;
			    beam_nr_alternative=beam_nr_alternative->next;
			    goto next_alternative;
			}
			
			abort_eam("cheguei aqui para tentar executar o prepare_tries antigo...\n");
			
	        prepare_tries:
#if Debug
		        printf("%5d->prepare_tries for %d clauses with arity=%d \n",contador++,(int) arg1,(int) arg2);
break_debug(contador);
#endif
			if (!arg1) goto fail;
		      { register int nr;
			nr=arg1;

			if (nr==1 && beam_ABX->parent!=NULL) {
			  beam_ES=0;
			  beam_nr_call->state=RUNNING;
			  beam_pc+=3;
			  /*			  execute_next(); */
			  goto only_1_clause;
			} 

                        beam_OBX=(struct OR_BOX *) request_memory(ORBOX_SIZE);
			beam_nr_call->call=beam_OBX;
			beam_nr_call->state=RUNNING;
			beam_OBX->nr_call=beam_nr_call;
			beam_OBX->parent=beam_ABX;
			beam_OBX->eager_split=beam_ES;
			beam_ES=0;
			beam_OBX->nr_all_alternatives=nr;

			{ register int i;
			  register struct status_or *p=NULL;
			  register Cell *a;
			    
			    if (nr>1) a=save_arguments(arg2);  else a=NULL;
			    beam_pc+=3;
			    for(i=0;i<nr;i++) {
			      beam_nr_alternative=(struct status_or *) request_memory(STATUS_OR_SIZE);
			      if (i==0) beam_OBX->alternatives=beam_nr_alternative;  else  p->next=beam_nr_alternative; 
			      beam_nr_alternative->previous=p;
			      p=beam_nr_alternative;
			      beam_nr_alternative->alternative=NULL;
			      beam_nr_alternative->code=beam_pc;
			      beam_nr_alternative->state=READY;
			      beam_nr_alternative->args=a;
			      beam_pc+=5;
			    }
			    beam_nr_alternative->next=NULL;
			}
		      }
			beam_nr_alternative=beam_OBX->alternatives;
			/* goto next_alternative; */
                        beam_pc=beam_nr_alternative->code;
			goto try_me;
			execute_next();

		/* explore_alternative */
	        trust_me:
			get_arguments(arg2,beam_nr_alternative->args);
			remove_memory_arguments(beam_nr_alternative->args);
			goto try_me;
		retry_me:
			get_arguments(arg2,beam_nr_alternative->args);
	        try_me:
			beam_nr_alternative->args=NULL;
#if Debug
		        printf("%5d->Create AND_BOX for the %dth clause of predicate %s/%d (Yvars=%d) \n",contador++,(int) arg4,((struct Clauses *)arg1)->predi->name,(int) arg2,(int) arg3);
break_debug(contador);
#endif
			if (beam_OBX->nr_all_alternatives>1 || beam_OBX->parent->parent==NULL) {

			  beam_USE_SAME_ANDBOX=NULL;
			  beam_ABX=(struct AND_BOX *)request_memory(ANDBOX_SIZE);
			  beam_nr_alternative->alternative=beam_ABX;
			  beam_nr_alternative->state=RUNNING;

			  beam_ABX->nr_alternative=beam_nr_alternative;
			  beam_ABX->level=beam_OBX->parent->level+1;
			  beam_ABX->parent=beam_OBX;
			  beam_ABX->externals=NULL;
			  beam_ABX->suspended=NULL;
			  beam_ABX->perms=NULL;
			  beam_ABX->calls=NULL;
			  beam_ABX->nr_all_calls=0;
			  beam_ABX->side_effects=((struct Clauses *)arg1)->side_effects;
			  /* continue on middle of only_1_clause code */
			} else {
			  beam_nr_call=beam_OBX->nr_call;
			  beam_ABX=beam_OBX->parent;
			  del_orbox_and_sons(beam_OBX);
			  beam_nr_call->call=NULL;
			  /* continue to only 1 clause */

	        only_1_clause:
#if Debug
		          printf("Only 1 Clause -> Use the same AND_BOX for the %dth clause of predicate %s/%d (Yvars=%d) \n",(int) arg4,((struct Clauses *)arg1)->predi->name,(int) arg2,(int) arg3);
#endif

			  if (((struct Clauses *)arg1)->side_effects >= CUT) {
			    /* printf("Must create or-box still the same ?????\n"); MUST SEE THIS CASE */
			  }
			  beam_USE_SAME_ANDBOX=beam_nr_call;
			  beam_nr_alternative=beam_ABX->nr_alternative;
			  beam_OBX=beam_ABX->parent;
			}

			if (arg3) {
			  register int nr_locals;
			  nr_locals=arg3;
			  /* nr_locals=((struct Clauses *)arg1)->nr_vars; */
			  beam_varlocals=request_memory_locals(nr_locals);
			  // add_to_list_locals(beam_varlocals,beam_ABX);
			} else { 
			  beam_varlocals=NULL; 
			}
			beam_pc=((struct Clauses *)arg1)->code+5;
			execute_next();

	        prepare_calls:
#if Debug
		        printf("%5d->prepare_calls %d\n",contador++,(int) arg1);
break_debug(contador);
#endif
			if (beam_USE_SAME_ANDBOX!=NULL) {  /* only one alternative */
			  register int nr;

			  nr=(int) arg1;
			  beam_pc+=2;
			  if (nr) {
			    beam_nr_call=beam_USE_SAME_ANDBOX;
			    if (nr==1) {   /* ONLY ONE CALL , CHANGE DIRECTLY */
			      beam_nr_call->call=NULL;
			      beam_nr_call->code=beam_pc+1;
			      beam_nr_call->locals=beam_varlocals; 
			      beam_nr_call->state=READY;
			    } else {
			      struct status_and *calls,*first=NULL,*last=NULL;
			      int i,nr2;

			      nr2=beam_ABX->nr_all_calls;
			      
			      for(i=0;i<nr;i++) {
				calls=(struct status_and *) request_memory(STATUS_AND_SIZE);
				if (first==NULL) first=calls;
				if (last!=NULL) last->next=calls;
				calls->previous=last;
			        calls->call=NULL;
			        calls->code=beam_pc+1;
			        calls->locals=beam_varlocals; 
			        calls->state=READY;
			        beam_pc=(Cell *) *beam_pc;
				last=calls;
			      }
			      
			      last->next=beam_nr_call->next;
			      if (beam_nr_call->next!=NULL) beam_nr_call->next->previous=last;
			      first->previous=beam_nr_call->previous;
			      if (beam_nr_call->previous!=NULL) beam_nr_call->previous->next=first;
			      else beam_ABX->calls=first; /* nr_call era o primeiro */
				
		              free_memory((Cell *) beam_nr_call,STATUS_AND_SIZE);
			      beam_nr_call=first;
			      beam_ABX->nr_all_calls=nr+nr2-1;
			    } 
			  } else {
			      beam_nr_call->call=NULL;
			  }
			} else 
                          { /* there where more than one alternative */
			  register int nr;
			  nr=(int) arg1;
			  beam_pc+=2;
			  beam_ABX->nr_all_calls=nr;
			  if (nr) {
			    struct status_and *calls, *first=NULL, *last=NULL;
			    register int i;

			    for(i=0;i<nr;i++) {
			      calls=(struct status_and *) request_memory(STATUS_AND_SIZE);
			      if (first==NULL) first=calls;
			      if (last!=NULL) last->next=calls;
			      calls->previous=last;
			      calls->call=NULL;
			      calls->code=beam_pc+1;
			      calls->locals=beam_varlocals; 
			      calls->state=READY;
			      beam_pc=(Cell *) *beam_pc;
			      last=calls;
			    }
			    last->next=NULL;
			    beam_ABX->calls=first;

			  } else beam_ABX->calls=NULL;
			  beam_nr_call=beam_ABX->calls;
			}
			/* goto scheduler;*/

	        scheduler:
#if Debug
		        printf("%5d->Scheduler... \n",contador++);
break_debug(contador);
#endif
#if Debug_Dump_State 
  		        dump_eam_state();
#endif
			/* Have to decide if I go up or continue on same level */
			/* If I go up the I have to suspend the and_box, 
			   else I can continue to the next clause (1st) of the and_box 
			   Another Alternative is to pick up a SUSPEND and_box       */
			/* for the meantime I Will always suspend unless there is a cut */

			if (beam_ABX->externals==NULL || beam_ABX->side_effects>=CUT) {
			  beam_pc=beam_nr_call->code;
			  execute_next();
			}
			beam_nr_alternative->state=SUSPEND;
			/* goto suspend; */

	        suspend:
#if Debug
          	        printf("%5d->SUSPEND on alternative %p\n",contador++,beam_nr_alternative);
break_debug(contador);
#endif
			beam_OBX=beam_ABX->parent;
		        {   struct EXTERNAL_VAR *e;
			    struct PERM_VAR *v;
			    struct SUSPENSIONS_VAR *s;
		
			    beam_ABX->suspended=addto_suspensions_list(beam_ABX,NORMAL_SUSPENSION);
			    e=beam_ABX->externals;
			    while(e!=NULL) {
			      v=e->var;
			      *((Cell *) v)=(Cell) v;
			      if (v->suspensions==NULL || v->suspensions->and_box!=beam_ABX) {
				/* se a and_box ja esta na lista  nao adiciona */
 			         s=(struct SUSPENSIONS_VAR *) request_memory(SUSPENSIONS_VAR_SIZE);
			         s->and_box=beam_ABX;
			         s->next=v->suspensions;
			         v->suspensions=s;
			      }
			      e=e->next;
			    }
			 }
			if (beam_OBX->eager_split) goto split;

		        beam_nr_alternative=beam_nr_alternative->next;
			goto next_alternative;


		call_yap:
		  /* Must create term to call */
		  /* YAP_RunGoal(t_goal); */

		  if (!Yap_execute_goal(beam_X[1],0,CurrentModule)) goto success;
		  else goto fail;
		  	  
		call:
#if Debug
		        printf("%5d->call %s/%d \n",contador++,((PredEntry *) arg1)->beamTable->name,(int) ((PredEntry *) arg1)->beamTable->arity);
break_debug(contador);
#endif
			beam_ES=((PredEntry *) arg1)->beamTable->eager_split;
			
			/* CUIDADO : vou tentar libertar a memoria caso seja o ultimo call */
#if DIRECT_JUMP
			if ((void *) arg3==&&exit_eam) /* Estou no ultimo call deste predicado */
#else
 		        if (arg3==_exit_eam)  /* Estou no ultimo call deste predicado */
#endif
			  {
			    if (beam_ABX->nr_all_calls==1) {
			      free_memory_locals(beam_nr_call->locals);
			    } else {
			      struct status_and *calls;
			      calls=beam_ABX->calls;
			      while(calls!=beam_nr_call) {
				if (calls->locals==beam_nr_call->locals) break;
				calls=calls->next;
			      }
			      if (calls==beam_nr_call) {
				free_memory_locals(beam_nr_call->locals);
			      }
			    }
			  }
			beam_nr_call->locals=NULL;
			bpEntry=(PredEntry *) arg1;
			beam_ALTERNATIVES=beam_H;
			Yap_absmi(-9000);
{
                        int NR_INDEXED;
			NR_INDEXED=beam_ALTERNATIVES-beam_H;
#if Debug
			printf("Back from yap-index with %d alternativas\n",NR_INDEXED);
#endif
			if (NR_INDEXED==0) goto fail;
 			if (NR_INDEXED==1 && beam_ABX->parent!=NULL) {
			  struct Clauses *clause=(struct Clauses *) *(beam_H);
			  beam_ES=0;
			  beam_nr_call->state=RUNNING;

#if Debug
		          printf("Only 1 Alternative\n");
#endif
 		          if (clause->side_effects >= CUT) {
			    /* printf("Must create or-box still the same ?????\n"); RSLOPES: MUST SEE THIS CASE */
			  }

			  beam_USE_SAME_ANDBOX=beam_nr_call;
			  beam_nr_alternative=beam_ABX->nr_alternative;
			  beam_OBX=beam_ABX->parent;

			  if (clause->nr_vars) {
			    register int nr_locals;
			    nr_locals=clause->nr_vars;
			    beam_varlocals=request_memory_locals(nr_locals);
			    // add_to_list_locals(beam_varlocals,beam_ABX);
			  } else { 
			    beam_varlocals=NULL; 
			  }
			  beam_pc=clause->code+5;
			  execute_next();
			} else { 
			  int i, arity;
			  struct status_or *p=NULL;
			  Cell *a;
			  arity=((PredEntry *) arg1)->beamTable->arity;

                            beam_OBX=(struct OR_BOX *) request_memory(ORBOX_SIZE);
	 		    beam_nr_call->call=beam_OBX;
  			    beam_nr_call->state=RUNNING;
			    beam_OBX->nr_call=beam_nr_call;
			    beam_OBX->parent=beam_ABX;
			    beam_OBX->eager_split=beam_ES;
			    beam_ES=0;
 			    beam_OBX->nr_all_alternatives=NR_INDEXED;

			    if (NR_INDEXED>1) a=save_arguments(arity);  else a=NULL;
			    for(i=0;i<NR_INDEXED;i++) {
			      beam_nr_alternative=(struct status_or *) request_memory(STATUS_OR_SIZE);
			      if (i==0) beam_OBX->alternatives=beam_nr_alternative;  else  p->next=beam_nr_alternative; 
			      beam_nr_alternative->previous=p;
			      p=beam_nr_alternative;
			      beam_nr_alternative->alternative=NULL;
			      beam_pc=((struct Clauses *) beam_H[i])->code;

#if DIRECT_JUMP
			      if (i==0) {
				if (NR_INDEXED==1) *beam_pc=(Cell) &&only_1_clause;
				else *beam_pc=(Cell) &&try_me;
			      } else if (i==NR_INDEXED-1) *beam_pc=(Cell) &&trust_me;
			      else *beam_pc=(Cell) &&retry_me;
#else
			      if (i==0) {
				if (NR_INDEXED==1) *beam_pc=_only_1_clause_op;
				else *beam_pc=_try_me_op;
			      } else if (i==NR_INDEXED-1) *beam_pc=_trust_me_op;
			      else *beam_pc=_retry_me_op;
#endif
			      arg2=arity;
			      arg1=beam_H[i];
			      arg3=((struct Clauses *) beam_H[i])->nr_vars;
			      arg4=i;
			      beam_nr_alternative->code=beam_pc;
			      beam_nr_alternative->state=READY;
			      beam_nr_alternative->args=a;
			    }
			    beam_nr_alternative->next=NULL;

			beam_nr_alternative=beam_OBX->alternatives;
			/* goto next_alternative; */
                        beam_pc=beam_nr_alternative->code;
			execute_next();

                       }
}
			/* goto prepare_tries; */

		safe_call:
#if Debug
		        printf("%5d->safe_call 0x%lX X1=%d (0x%lX) ,X2=%d (0x%lX) \n",contador++,(unsigned long) arg1,(int) beam_X[1],(unsigned long) beam_X[1],(int) beam_X[2],(unsigned long) beam_X[2]);
break_debug(contador);
#endif
			beam_S=(Cell *) arg1;
			beam_S=(Cell *) (* ((int long  (*)(void)) beam_S))();
			if (!beam_S) goto fail_body;
			
			/* we didn't get to created a or_box */
			beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
 		        beam_OBX=beam_ABX->parent;
			goto next_call;

		safe_call_unary:
#if Debug
		        printf("%5d->safe_call_unary 0x%lX X1=%d (0x%lX) ,X2=%d (0x%lX) \n",contador++,(unsigned long) arg1,(int) beam_X[1],(unsigned long) beam_X[1],(int) beam_X[2],(unsigned long) beam_X[2]);
break_debug(contador);
#endif
			beam_S=(Cell *) arg1;
			beam_S=(Cell *) (* ((int long  (*)(Term)) beam_S))(deref(beam_X[1]));
			if (!beam_S) goto fail_body;
			
			/* we didn't get to created a or_box */
			beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
 		        beam_OBX=beam_ABX->parent;
			goto next_call;

		safe_call_binary:
#if Debug
		        printf("%5d->safe_call_binary 0x%lX X1=%d (0x%lX) ,X2=%d (0x%lX) \n",contador++,(unsigned long) arg1,(int) beam_X[1],(unsigned long) beam_X[1],(int) beam_X[2],(unsigned long) beam_X[2]);
break_debug(contador);
#endif
			beam_S=(Cell *) arg1;
			beam_S=(Cell *) (* ((int long  (*)(Term, Term)) beam_S))(deref(beam_X[1]),deref(beam_X[2]));
                        if (!beam_S) goto fail_body;
			
			/* we didn't get to created a or_box */
			beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
 		        beam_OBX=beam_ABX->parent;
			goto next_call;


		direct_safe_call:
#if Debug
		        printf("%5d->direct_safe_call %p X1=%d,X2=%d \n",contador++,(void *) arg1,(int) beam_X[1],(int) beam_X[2]);
break_debug(contador);
#endif
			beam_S=(Cell *) arg1;
			beam_S=(Cell *) (* ((int long  (*)(void)) beam_S))();
			/* beam_S=(Cell *) (* ((int long  (*)(Term,Term)) beam_S))(beam_X[1],beam_X[2]); */
			if (!beam_S) goto fail_head;
			beam_pc+=2;
			execute_next();

		direct_safe_call_unary:
#if Debug
		        printf("%5d->direct_safe_call_unary %p X1=%d,X2=%d \n",contador++,(void *) arg1,(int) beam_X[1],(int) beam_X[2]);
break_debug(contador);
#endif
			beam_S=(Cell *) arg1;
			beam_S=(Cell *) (* ((int long  (*)(Term)) beam_S))(deref(beam_X[1]));
			if (!beam_S) goto fail_head;
			beam_pc+=2;
			execute_next();

		direct_safe_call_binary:
#if Debug
		        printf("%5d->direct_safe_call_binary %p X1=%d,X2=%d \n",contador++,(void *) arg1,(int) beam_X[1],(int) beam_X[2]);
break_debug(contador);
#endif
			beam_S=(Cell *) arg1;
			beam_S=(Cell *) (* ((int long  (*)(Term,Term)) beam_S))(deref(beam_X[1]),deref(beam_X[2]));
			if (!beam_S) goto fail_head;
			beam_pc+=2;
			execute_next();

	        skip_while_var:
#if Debug
			    printf("%5d->Skip_while_var on call %p\n",contador++, beam_nr_call);
break_debug(contador);
#endif   
			 if (exists_var_in((Cell *) beam_X[1])) { 
			   beam_ABX->suspended=addto_suspensions_list(beam_ABX,VAR_SUSPENSION);
			   beam_nr_call=beam_nr_call->next;
			   goto next_call; 
			 }
			beam_pc+=1;
			execute_next();

	        wait_while_var:
#if Debug
			    printf("%5d->Wait_while_var on call %p\n",contador++, beam_nr_call);
break_debug(contador);
#endif   
			 if (exists_var_in((Cell *) beam_X[1])) { 
			       beam_ABX->suspended=addto_suspensions_list(beam_ABX,VAR_SUSPENSION);
			       beam_OBX=beam_ABX->parent; 
                               beam_nr_alternative=beam_ABX->nr_alternative->next;
                               goto next_alternative; 
			 }
			 beam_pc+=1;
			 execute_next();

	        force_wait:
#if Debug
			 printf("%5d->Force Waiting on call %p\n",contador++, beam_nr_call);
break_debug(contador);
#endif   
			 /* we didn't get to created a or_box */

 		         beam_OBX=beam_ABX->parent;
			 if (beam_nr_call->previous!=NULL) {
			    beam_nr_call->call=NULL;
			    beam_nr_call->state=WAITING_TO_BE_FIRST;
			    beam_ABX->suspended=addto_suspensions_list(beam_ABX,WAIT_SUSPENSION);
			    beam_nr_alternative=beam_ABX->nr_alternative->next;
			    goto next_alternative;
			 } 
			 beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
			 goto next_call;

	        write_call:
#if Debug
		         printf("%5d->write_call\n",contador++);
break_debug(contador);
#endif
#if USE_LEFTMOST
			 if (!is_leftmost(beam_ABX,beam_nr_call)) {
  #if Debug
		           printf("Force Waiting Before write_call\n");
  #endif
			   beam_nr_call->call=NULL;
			   beam_nr_call->state=WAITING_TO_BE_LEFTMOST;
			   beam_ABX->suspended=addto_suspensions_list(beam_ABX,LEFTMOST_SUSPENSION);
			   goto top_tree;
			 }
#endif

#ifdef DEBUG
			 Yap_plwrite ((Term) beam_X[1], NULL, 0, GLOBAL_MaxPriority);
#else
			 extern int beam_write (void);
			 beam_write();
#endif
			 beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
			 beam_ABX->side_effects=beam_ABX->side_effects | WRITE;
		         beam_OBX=beam_ABX->parent;
			 goto next_call;

	        is_call:
#if Debug
		        printf("%5d->is_call\n",contador++);
break_debug(contador);
#endif
			{
			  Cell *_DR;
			/* BEAM_is is declared on C/eval.c */
			  _DR=(Cell *) BEAM_is();
			  if (_DR==NULL) { /* erro no Eval */
			    beam_top=NULL;
			    return (FALSE);
			  }
			  if (!Unify((Cell *) beam_X[1],_DR)) goto fail_body;
			}
			beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
 		        beam_OBX=beam_ABX->parent;

			goto next_call;

	        equal_call:
#if Debug
		        printf("%5d->equal_call\n",contador++);
break_debug(contador);
#endif
			beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
			if (beam_ABX->externals!=NULL) {
			    if (beam_ABX->nr_all_calls==0) {
			         beam_nr_alternative->state=SUSPEND_END;
			    } else beam_nr_alternative->state=SUSPEND;
			    goto suspend;			    
			}

			goto next_call;


		pop:
#if Debug
		        printf("%5d->pop %d \n",contador++,(int) arg1);  
break_debug(contador);
#endif
                        if (arg1>1) {
			  beam_sp+=arg1>>2; 
			}			
			pop_mode_and_sreg();
#if Debug
                        if (beam_Mode==READ) printf("Continues in READ mode\n"); 
                        else  printf("Continues in WRITE mode\n"); 
#endif
			beam_pc+=2;
			execute_next();

		do_nothing:
#if Debug
		        printf("%5d->do_nothing \n",contador++);
break_debug(contador);
#endif
			beam_pc++;
		        execute_next();


		get_var_X:
#if Debug
		        printf("%5d->get_var_X X%d=X%d \n",contador++,(int) arg2,(int) arg1);
break_debug(contador);
    
#endif
			beam_X[arg2]=beam_X[arg1];
			beam_pc+=3;
			execute_next();

		get_var_Y:
#if Debug
		        printf("%5d->get_var_Y Y%d=X%d \n",contador++,(int) arg2,(int) arg1);
break_debug(contador);
#endif
			beam_varlocals[arg2]=beam_X[arg1];
#if !Fast_go
			{ Cell *a;
			  a = (Cell *) deref(beam_X[arg1]);
			  if(isvar(a) && !isappl(a) && !is_perm_var(a))
			    abort_eam("Sério problema no get_var_Y\n");
  			    /* acho que vou ter de criar uma variavel local nova no nivel superior */
			}
#endif
			beam_pc+=3;
			execute_next();

		get_val_X:
#if Debug
		        printf("%5d->get_val_X X%d,X%d \n",contador++,(int) arg1,(int) arg2);    
break_debug(contador);
#endif
			{ register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(beam_X[arg1]);
			if (isvar((Cell) _DR)) { 
			        _DR1=(Cell *) deref(beam_X[arg2]);
				if (!isvar((Cell) _DR1)) { 
				    *(_DR)=(Cell) _DR1;
				    trail(beam_ABX,(struct PERM_VAR *) _DR);
				} else {
				    UnifyCells(_DR,_DR1);
				}		
			} else {
			        _DR1=(Cell *) deref(beam_X[arg2]);
			        if (isvar((Cell) _DR1)) { 
				    *(_DR1)=(Cell) _DR;
				    trail(beam_ABX,(struct PERM_VAR *) _DR1);
				} else {
				    if (!Unify(_DR1,_DR)) goto fail_head;
				}
			}
			}
			beam_pc+=3;
		        execute_next();

		get_val_Y:
#if Debug
		        printf("%5d->get_val_Y X%d,Y%d \n",contador++,(int) arg1,(int) arg2);   
break_debug(contador);
#endif
			{ register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(beam_X[arg1]);
			if (isvar((Cell) _DR)) {  
				 _DR1=(Cell *) deref(beam_varlocals[arg2]);
				 if (!isvar((Cell) _DR1)) { 
				     *(_DR)=(Cell) _DR1;
				     trail(beam_ABX,(struct PERM_VAR *) _DR);
				 } else {
				     UnifyCells(_DR,_DR1);
				 }		
		        } else {
			         _DR1=(Cell *) deref(beam_varlocals[arg2]);
				 if (isvar((Cell) _DR1)) { 
				    *(_DR1)=(Cell) _DR;
				    trail(beam_ABX,(struct PERM_VAR *) _DR1);
				 } else {
				    if (!Unify(_DR1,_DR)) goto fail_head;
				 }
			}
			}
			beam_pc+=3;
		        execute_next();

		get_atom:
#if Debug
		        printf("%5d->get_atom X%d, 0x%lX\n",contador++,(int) arg1,(unsigned long) arg2);
break_debug(contador);
#endif
			{ register Cell *_DR;
			_DR=(Cell *) deref(beam_X[arg1]);
			if (isvar((Cell) _DR)) {  
			      *(_DR)=arg2;
			      trail(beam_ABX,(struct PERM_VAR *) _DR);
			} else {
			      if ((Cell) _DR!=arg2) goto fail_head; 
			}
			}
			beam_pc+=3;
		        execute_next();

		get_list:
#if Debug
		        printf("%5d->get_list X%d\n",contador++,(int) arg1);
break_debug(contador);
#endif
			{ register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(beam_X[arg1]);
			if (isvar((Cell) _DR)) { beam_Mode=WRITE;
		                 beam_S = beam_H;
		                 beam_H+= 2;
			         _DR1=(Cell *) abspair(beam_S);
				 *(_DR)=(Cell) _DR1;
				 trail(beam_ABX,(struct PERM_VAR *) _DR);
				 beam_pc+=2;
				 execute_next();
			} else {
			         if (!ispair((Cell) _DR)) goto fail_head;
				 beam_Mode=READ;
				 _DR1=_DR; /* SaveExpression in DR1*/
				 beam_S=(Cell *) reppair((Cell) _DR);
				 beam_pc+=2;
				 execute_next();
			}
			}

		get_struct:
#if Debug
		        printf("%5d->get_struct X%d, 0x%lX/%d\n",contador++,(int) arg1,(unsigned long) arg2,(int) arg3);
break_debug(contador);
    
#endif
			{ register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(beam_X[arg1]);
			if (isvar((Cell) _DR)) { beam_Mode=WRITE;
			          _DR1=(Cell *) absappl((Cell) beam_H); /* SaveExpression in _DR1*/
				  *(_DR)=(Cell) _DR1;
				  trail(beam_ABX,(struct PERM_VAR *) _DR);
				  *( beam_H++)=arg2;
				  beam_S= beam_H;
				   beam_H+=arg3;  /* arg3 = arity */
				  beam_pc+=4;
				  execute_next();
			} else {
			          if (!isappl((Cell) _DR)) goto fail_head;
				  beam_Mode=READ;
				  beam_S=(Cell *) repappl((Cell) _DR);
				  if (*beam_S!=arg2) goto fail_head;
				  beam_S++;
				  _DR1=_DR; /* SaveExpression in _DR1*/
				  beam_pc+=4;
				  execute_next();
			}
			}

		unify_void:
#if Debug
		        printf("%5d->unify_void\n",contador++);
break_debug(contador);
#endif
			if (beam_Mode==WRITE) {
			  *beam_S=(Cell) request_permVar(beam_ABX);
			}
			beam_S++;
			beam_pc+=1;
			execute_next();


		unify_local_Y:
#if Debug
		        printf("%5d->unify_local_Y Y%d \n",contador++,(int) arg1);
break_debug(contador);
    
#endif
		     if (beam_Mode==READ) {
			 register Cell *_DR, *_DR1;
			_DR1=(Cell *) deref(beam_varlocals[arg1]);
			if (isvar((Cell) _DR1)) {                 
			  _DR=(Cell *) deref((Cell) beam_S);
			  if (isvar((Cell) _DR)) {
			    UnifyCells(_DR1,_DR);  /* var , var */
			  } else {
			    *(_DR1)=(Cell) _DR;    /* var , nonvar */
			    trail(beam_ABX,(struct PERM_VAR *) _DR1);
			  }
			}
			else {
			  _DR=(Cell *) deref((Cell) beam_S);
			  if (isvar((Cell) _DR)) {
			    *(_DR)=(Cell) _DR1;    /* nonvar, var */
			    trail(beam_ABX,(struct PERM_VAR *) _DR);
			  } else {
			    if (!Unify(_DR,_DR1)) goto fail_head; /* nonvar, nonvar */
			  }			  
			}
			beam_S++;
			beam_pc+=2;
			execute_next();
		      }  else {  /* write Mode */
			register Cell *_DR;
			_DR=(Cell *) deref(beam_varlocals[arg1]);
			if (isvar((Cell) _DR) && !is_perm_var((Cell *) _DR)) {
			  *beam_S=(Cell) request_permVar(beam_ABX);
			  UnifyCells(_DR,beam_S);
			} else {
			  *(beam_S)=(Cell) _DR;
			}
			beam_S++;
			beam_pc+=2;
			execute_next();
		      }

		unify_local_X:
#if Debug
		        printf("%5d->unify_local_X X%d \n",contador++,(int) arg1);
break_debug(contador);
#endif
		     if (beam_Mode==READ) {			
			 register Cell *_DR, *_DR1;
			_DR1=(Cell *) deref(beam_X[arg1]);
			if (isvar((Cell) _DR1)) {                 
			  _DR=(Cell *) deref((Cell) beam_S);
			  if (isvar((Cell) _DR)) {
			    UnifyCells(_DR1,_DR);  /* var , var */
			  } else {
			    *(_DR1)=(Cell) _DR;    /* var , nonvar */
			    trail(beam_ABX,(struct PERM_VAR *) _DR1);
			  }
			}
			else {
			  _DR=(Cell *) deref((Cell) beam_S);
			  if (isvar((Cell) _DR)) {
			    *(_DR)=(Cell) _DR1;    /* nonvar, var */
			    trail(beam_ABX,(struct PERM_VAR *) _DR);
			  } else {
			    if (!Unify(_DR,_DR1)) goto fail_head; /* nonvar, nonvar */
			  }			  
			}
			beam_S++;
			beam_pc+=2;
			execute_next();
		     } else {  /* write mode */
			register Cell *_DR;
			_DR=(Cell *) deref(beam_X[arg1]);

			if (isvar((Cell) _DR) && !is_perm_var((Cell *) _DR)) {
			  *beam_S=(Cell) request_permVar(beam_ABX);
			  UnifyCells(_DR,beam_S);
			} else {
			  *(beam_S)=(Cell) _DR;
			}
			beam_S++;
			beam_pc+=2;
			execute_next();
		     }

		unify_val_Y:
#if Debug
		        printf("%5d->unify_val_Y Y%d \n",contador++,(int) arg1);
break_debug(contador);
    
#endif
		     if (beam_Mode==READ) {			
			register Cell *_DR, *_DR1;
			_DR1=(Cell *) deref(beam_varlocals[arg1]);
			if (isvar((Cell) _DR1)) {
			  _DR=(Cell *) deref((Cell) beam_S);
			  if (isvar((Cell) _DR)) {
			    UnifyCells(_DR1,_DR);
			  } else {
			    *(_DR1)=(Cell) _DR;
			    trail(beam_ABX,(struct PERM_VAR *) _DR1);
			  }
			}
			else {
			  _DR=(Cell *) deref((Cell) beam_S);
			  if (isvar((Cell) _DR)) {
			    *(_DR)=(Cell) _DR1;
			    trail(beam_ABX,(struct PERM_VAR *) _DR);
			  } else {
			    if (!Unify(_DR,_DR1)) goto fail_head;
			  }			  
			}
			beam_S++;
			beam_pc+=2;
			execute_next();
		     } else { /* write mode */
 		        *(beam_S)=beam_varlocals[arg1];
			beam_S++;
			beam_pc+=2;
			execute_next();
		     }


		unify_val_X:
#if Debug
		        printf("%5d->unify_val_X X%d \n",contador++,(int) arg1);
break_debug(contador);
#endif
		     if (beam_Mode==READ) {			
			 register Cell *_DR, *_DR1;
			_DR1=(Cell *) deref((Cell) beam_X[arg1]);
			if (isvar((Cell) _DR1)) {
			  _DR=(Cell *) deref((Cell) beam_S);
			  if (isvar((Cell) _DR)) {
			    UnifyCells(_DR1,_DR);
			  } else {
			    *(_DR1)=(Cell) _DR;
			    trail(beam_ABX,(struct PERM_VAR *) _DR1);
			  }
			}
			else {
			  _DR=(Cell *) deref((Cell) beam_S);
			  if (isvar((Cell) _DR)) {
			    *(_DR)=(Cell) _DR1;
			    trail(beam_ABX,(struct PERM_VAR *) _DR);
			  } else {
			    if (!Unify(_DR,_DR1)) goto fail_head;
			  }			  
			}
			beam_S++;
			beam_pc+=2;
			execute_next();
		     } else {
			*(beam_S)=beam_X[arg1];
			beam_S++;
			beam_pc+=2;
			execute_next();
		     }

		unify_var_X:
#if Debug
		        printf("%5d->unify_var_X X%d=*S \n",contador++,(int) arg1);
break_debug(contador);
#endif
		     if (beam_Mode==READ) {			
		        beam_X[arg1]=*(beam_S++);
			beam_pc+=2;
			execute_next();
		     } else {
			*beam_S=(Cell) request_permVar(beam_ABX);
			beam_X[arg1]=(Cell) beam_S;
			beam_S++;
			beam_pc+=2;
			execute_next();
		     }

		unify_var_Y:
#if Debug
		        printf("%5d->unify_var_Y Y%d \n",contador++,(int) arg1);
break_debug(contador);
#endif
		     if (beam_Mode==READ) {
			beam_varlocals[arg1]=*(beam_S++);
			beam_pc+=2;
			execute_next();
		     } else {
			*beam_S=(Cell )request_permVar(beam_ABX);
			beam_varlocals[arg1]=*beam_S;
			beam_S++;
			beam_pc+=2;
			execute_next();
		      }

		unify_last_atom:
		unify_atom:
#if Debug
		        printf("%5d->unify_atom 0x%lX \n",contador++,(unsigned long) arg1);
break_debug(contador);
#endif
		     if (beam_Mode==READ) {
			 register Cell *_DR;
			_DR=(Cell *) deref((Cell) beam_S);
			if (isvar((Cell) _DR)) {
			  *(_DR)=arg1;
			  trail(beam_ABX,(struct PERM_VAR *) _DR);
			} else {
			  if ((Cell) _DR!=arg1)  goto fail_head;
			}
			beam_S++; 
			beam_pc+=2;
			execute_next();
		     } else {
			*(beam_S)=arg1;
			beam_S++;
			beam_pc+=2;
			execute_next();
		     }

		unify_list:
#if Debug
		        printf("%5d->unify_list \n",contador++);
break_debug(contador);
#endif
		     if (beam_Mode==READ) {
			 register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(*beam_S);
			if (isvar((Cell) _DR)) { 
                              _DR1=(Cell *) abspair((Cell)  beam_H);  /* SavedExpression  in _DR1 */
			      *(_DR)=(Cell) _DR1;
			      trail(beam_ABX,(struct PERM_VAR *) _DR);
			      beam_S++;
			      push_mode_and_sreg();
			      beam_Mode=WRITE;  /* goes int write mode */
			      beam_S= beam_H;
			       beam_H+=2;
			      beam_pc+=1;
			      execute_next();
			} else {
			      if (!ispair((Cell) _DR)) goto fail_head;
			      beam_S++;
			      push_mode_and_sreg();
			      beam_S=(Cell *) reppair((Cell) _DR);
			      _DR1=_DR;  /* SavedExpression in _DR1 */ 
			      beam_pc+=1;
			      execute_next();
			}
		     } else {
			 register Cell *_DR1;
                        _DR1=(Cell *) abspair((Cell)  beam_H);  /* SavedExpression  in _DR1 */
                        *(beam_S)=(Cell) _DR1;
			beam_S++;
			push_mode_and_sreg();
			beam_S= beam_H;
		         beam_H+=2;
			beam_pc+=1;
			execute_next();
		      }

		unify_last_list:
#if Debug
		        printf("%5d->unify_last_list \n",contador++);
break_debug(contador);
#endif
		     if (beam_Mode==READ) {
			register Cell *_DR, *_DR1;
		        _DR=(Cell *) deref(*beam_S);
			if (isvar((Cell) _DR)) { beam_Mode=WRITE;  /* goes into write mode */
			         _DR1=(Cell *) abspair((Cell)  beam_H);  /* SavedExpression  in _DR1 */
				 *(_DR)=(Cell) _DR1;
				 trail(beam_ABX,(struct PERM_VAR *) _DR);
				 beam_S= beam_H;
				  beam_H+=2;
				 beam_pc+=1;
				 execute_next();
	                } else {
			         if (!ispair((Cell) _DR)) goto fail_head;
				 beam_S=(Cell *) reppair((Cell) _DR);
				 _DR1=_DR;  /* SavedExpression  in _DR1 */
				 beam_pc+=1;
				 execute_next();
			}
		     } else {
			 register Cell *_DR1;
			_DR1=(Cell *) abspair((Cell)  beam_H);  /* SavedExpression  in _DR1 */
			*(beam_S)=(Cell) _DR1;
			beam_S= beam_H;
			 beam_H+=2;
			beam_pc+=1;
			execute_next();
		     }

		unify_struct:
#if Debug
		        printf("%5d->unify_struct 0x%lX,%d \n",contador++,(unsigned long) arg1,(int) arg2);
break_debug(contador);
#endif
		     if (beam_Mode==READ) {
			 register Cell *_DR, *_DR1;
		        _DR=(Cell *) deref(*beam_S);
			if (isvar((Cell) _DR)) { 
			           _DR1=(Cell *) absappl((Cell)  beam_H); /* SaveExpression in _DR1*/
				   *(_DR)=(Cell) _DR1;
				   trail(beam_ABX,(struct PERM_VAR *) _DR);
				   beam_S++;
				   push_mode_and_sreg();
				   beam_Mode=WRITE;  /* goes into write mode */
				   *( beam_H++)=arg1;
				   beam_S= beam_H;
				    beam_H+=arg2;
				   beam_pc+=3;
				   execute_next();
			} else {
			          if (!isappl((Cell) _DR)) goto fail_head;
				  _DR1=(Cell *) repappl((Cell) _DR);
				  if (*_DR1!=arg1) goto fail_head;
				  ++beam_S;
				  push_mode_and_sreg();
				  beam_S=++_DR1;
				  _DR1=_DR; /* SaveExpression in _DR1*/
				  beam_pc+=3;
				  execute_next();
			}
		     } else {
			register Cell *_DR1;
			_DR1=(Cell *) absappl((Cell)  beam_H); /* SaveExpression in _DR1*/
			*(beam_S)=(Cell) _DR1;
			beam_S++;
			push_mode_and_sreg();
			*( beam_H++)=arg1;
		        beam_S= beam_H;
			 beam_H+=arg2;
			beam_pc+=3;
			execute_next();
		     }

		unify_last_struct:
#if Debug
		        printf("%5d->unify_last_struct 0x%lX, %d \n",contador++,(unsigned long) arg1,(int) arg2);
break_debug(contador);
#endif
		     if (beam_Mode==READ) {
			 register Cell *_DR, *_DR1;
		        _DR=(Cell *) deref(*beam_S);
			if (isvar((Cell) _DR)) { beam_Mode=WRITE;  /* goes into write mode */
			           _DR1=(Cell *) absappl((Cell)  beam_H); /* SaveExpression in _DR1*/
				   *(_DR)=(Cell) _DR1;
				   trail(beam_ABX,(struct PERM_VAR *) _DR);
				   *( beam_H++)=arg1;
				   beam_S= beam_H;
				    beam_H+=arg2;
				   beam_pc+=3;
				   execute_next();
			} else {
			          if (!isappl((Cell) _DR)) goto fail_head;
				  _DR1=(Cell *) repappl((Cell) _DR);
				  if (*_DR1!=arg1) goto fail_head;
				  beam_S=++_DR1;
				  _DR1=_DR; /* SaveExpression in _DR1*/
				  beam_pc+=3;
				  execute_next();
			}
		     } else {
			 register Cell *_DR1;
			_DR1=(Cell *) absappl((Cell)  beam_H); /* SaveExpression in _DR1*/
			*(beam_S)=(Cell) _DR1;
			*( beam_H++)=arg1;
		        beam_S= beam_H;
			 beam_H+=arg2; 
			beam_pc+=3;
			execute_next();
		     }

		put_var_X:
#if Debug
		        printf("%5d->put_var_X X%d,X%d \n",contador++,(int) arg1,(int) arg2);
break_debug(contador);
#endif
			beam_X[arg1]=(Cell)  beam_H;
			beam_X[arg2]=(Cell)  beam_H;
			*(beam_H)=(Cell)  beam_H;
			beam_H++;
			beam_pc+=3;
			execute_next();
			


		put_val_X:
#if Debug
		        printf("%5d->put_val_X X%d,X%d \n",contador++,(int) arg1,(int) arg2);
break_debug(contador);
#endif
			beam_X[arg1]=beam_X[arg2];
			beam_pc+=3;
			execute_next();


		put_var_P:
#if Debug
		        printf("%5d->put_var_P X%d,Y%d \n",contador++,(int) arg1,(int) arg2);
break_debug(contador);
#endif
			if (isvar(beam_varlocals[arg2]) && !is_perm_var((Cell *) beam_varlocals[arg2])) 
			   beam_varlocals[arg2]=(Cell) request_permVar(beam_ABX);
			beam_X[arg1]=beam_varlocals[arg2];
			beam_pc+=3;
			execute_next();
 
		put_var_Y:
			/*
#if Debug
		        printf("%5d->put_var_Y X%d,Y%d \n",contador++,(int) arg1,(int) arg2);
break_debug(contador);
    
#endif
                        { register Cell *a;
			a = &(beam_varlocals[arg2]);
			*a=(Cell) a;
			beam_X[arg1]=(Cell) a; }
			beam_pc+=3;
			execute_next();
			*/
		put_val_Y:
#if Debug
		        printf("%5d->put_val_Y X%d,Y%d \n",contador++,(int) arg1,(int) arg2);
break_debug(contador);
#endif
			beam_X[arg1]=beam_varlocals[arg2];
			beam_pc+=3;
			execute_next();

		put_unsafe:
#if Debug
		        printf("%5d->put_unsafe X%d, Y%d \n",contador++,(int) arg1,(int) arg2);
break_debug(contador);
#endif
			beam_X[arg1]=beam_varlocals[arg2]; 
			beam_pc+=3;
			execute_next();


		put_atom:
#if Debug
		        printf("%5d->put_atom X%d, 0x%lX \n",contador++,(int) arg1,(unsigned long) arg2);
break_debug(contador);
#endif
			beam_X[arg1]=arg2;
			beam_pc+=3;
			execute_next();

		put_list:
#if Debug
		        printf("%5d->put_list X%d \n",contador++,(int) arg1);
break_debug(contador);
#endif
			{ register Cell *_DR1;

                        _DR1=(Cell *) abspair((Cell)  beam_H); /* SaveExpression in _DR1*/
			beam_X[arg1]=(Cell) _DR1;
			beam_S=beam_H;
			beam_H+=2;
			beam_pc+=2;
			execute_next();
			}

		put_struct:
#if Debug
		        printf("%5d->put_struct X%d, 0x%lX, %d \n",contador++,(int) arg1,(unsigned long) arg2,(int) arg3);
break_debug(contador);
#endif
			{ register Cell _DR1;

                        _DR1=absappl((Cell) beam_H); /* SaveExpression in _DR1*/
			beam_X[arg1]=(Cell) _DR1;
			*(beam_H++)=arg2;
			beam_S=beam_H;
			beam_H+=arg3;
			beam_pc+=4;
			execute_next();
			}

		write_var_X:
#if Debug
		        printf("%5d->write_var_X X%d \n",contador++,(int) arg1);
break_debug(contador);
#endif
			*beam_S=(Cell) request_permVar(beam_ABX);
			beam_X[arg1]=(Cell) beam_S;
			beam_S++;
			beam_pc+=2;
			execute_next();

		write_var_Y:
#if Debug
		        printf("%5d->write_var_Y Y%d \n",contador++,(int) arg1);
break_debug(contador);
#endif 
			{ Cell *c;
			c=&beam_varlocals[arg1];
			*c=(Cell) c;
			*beam_S=(Cell) c;
			}
			beam_S++;
			beam_pc+=2;
			execute_next();


		write_var_P:
#if Debug
		        printf("%5d->write_var_P Y%d \n",contador++,(int) arg1);
break_debug(contador);
#endif 
			if (isvar(beam_varlocals[arg1]) && !is_perm_var((Cell *) beam_varlocals[arg1])) 
                           beam_varlocals[arg1]=(Cell) request_permVar(beam_ABX);
			*(beam_S)=beam_varlocals[arg1];
			beam_S++;
			beam_pc+=2;
			execute_next();


	        write_local_X:
		write_val_X:
#if Debug
		        printf("%5d->write_val_X X%d  (or write_local)\n",contador++,(int) arg1);
break_debug(contador);
#endif
			*(beam_S)=beam_X[arg1];
			beam_S++;
			beam_pc+=2;
			execute_next();

	        write_local_Y:
		write_val_Y:
#if Debug
		        printf("write_val_Y Y%d (or write_local)\n",(int) arg1);  
#endif
			*(beam_S)=beam_varlocals[arg1];
			beam_S++;
			beam_pc+=2;
			execute_next();

	        write_void:
#if Debug
		        printf("%5d->write_void \n",contador++);  
break_debug(contador);
#endif
			*beam_S=(Cell) request_permVar(beam_ABX);
			beam_S++;
			beam_pc+=1;
			execute_next();
		write_atom:	
#if Debug
		        printf("%5d->write_atom 0x%lX \n",contador++,(unsigned long) arg1);
break_debug(contador);
#endif
			*(beam_S)=arg1;
			beam_S++;
			beam_pc+=2;
			execute_next();


		write_list:
#if Debug
		        printf("%5d->write_list \n",contador++);
break_debug(contador);
#endif
			{ register Cell *_DR1;

                        _DR1=(Cell *) abspair((Cell) beam_H); /* SaveExpression in _DR1*/
			*(beam_S++)=(Cell) _DR1; 
			push_mode_and_sreg();
			beam_S=beam_H;
			beam_H+=2;
			beam_pc+=1;
			execute_next();
			}

		write_last_list:
#if Debug
		        printf("%5d->write_last_list \n",contador++);
break_debug(contador);
#endif
			{ register Cell *_DR1;

                        _DR1=(Cell *) abspair((Cell) beam_H); /* SaveExpression in _DR1*/
			*(beam_S)=(Cell) _DR1;
			beam_S=beam_H;
			beam_H+=2;
			beam_pc+=1;
			execute_next();
			}

		write_struct:
#if Debug
		        printf("%5d->write_struct 0x%lX, %d \n",contador++,(unsigned long) arg1,(int) arg2);
break_debug(contador);
#endif
			{ register Cell *_DR1;

                        _DR1=(Cell *) absappl((Cell) beam_H); /* SaveExpression in _DR1*/
			*(beam_S++)=(Cell) _DR1; 
			push_mode_and_sreg();
			*(beam_H++)=arg1;
			beam_S=beam_H;
			beam_H+=arg2;
			beam_pc+=3;
			execute_next();
			}

		write_last_struct:
#if Debug
		        printf("%5d->write_last_struct 0x%lX, %d \n",contador++,(unsigned long) arg1,(int) arg2);
break_debug(contador);
#endif
			{ register Cell *_DR1;
			_DR1=(Cell *) absappl((Cell) beam_H); /* SaveExpression in _DR1*/
			*(beam_S)=(Cell) _DR1;  
			*(beam_H++)=arg1;
			beam_S=beam_H;
			beam_H+=arg2;
			beam_pc+=3;
			execute_next();
			}

		cut: 
#if Debug
		        printf("%5d->cut na alternativa %pª de %d \n",contador++,beam_ABX->nr_alternative, beam_ABX->parent->nr_all_alternatives);
break_debug(contador);
#endif
			beam_OBX=beam_ABX->parent;
			{
			  struct status_or *new;
			  if (!is_leftmost(beam_ABX,beam_nr_call)) {
#if Debug 
			    printf("Force Waiting Before Cut\n");
#endif
			    beam_nr_call->call=NULL;
			    beam_nr_call->state=WAITING_TO_BE_LEFTMOST;
			    beam_ABX->suspended=addto_suspensions_list(beam_ABX,LEFTMOST_SUSPENSION);
			    beam_nr_call=beam_nr_call->next;
			    goto next_call;
			  }
			    beam_ABX->side_effects-=CUT;
			    beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);
#if Debug
			    printf("Executando o cut \n");
			    if (beam_ABX->externals!=NULL && beam_OBX->nr_all_alternatives>1) printf("cut com externals (noisy) \n");
			    if (beam_ABX->externals!=NULL && beam_OBX->nr_all_alternatives==1) printf("cut com externals (degenerate) \n");
#endif
                            beam_nr_alternative=beam_ABX->nr_alternative;
                            new=beam_nr_alternative->next;
			    beam_nr_alternative->next=NULL;
			    if (new!=NULL) {
  			       do{
			          struct status_or *old;
			          old=new;
			          new=new->next;
			          del_andbox_and_sons(old->alternative);
			          if (new==NULL) remove_memory_arguments(old->args);
                                  free_memory((Cell *) old,STATUS_OR_SIZE);
			          beam_OBX->nr_all_alternatives--;
			       } while (new!=NULL);
			       if (beam_OBX->nr_all_alternatives==1) {
				  beam_nr_alternative=beam_OBX->alternatives;
				  goto unique_alternative;
			       }
			    }
			    goto next_call;			  
			}

		commit:
#if Debug
		        printf("%5d->commit na alternativa %pª de %d \n",contador++,beam_ABX->nr_alternative, beam_ABX->parent->nr_all_alternatives);
break_debug(contador);
#endif
			beam_OBX=beam_ABX->parent;
			{
			  struct status_or *new;
			  if (!is_leftmost(beam_OBX->parent,beam_OBX->nr_call)) {
#if Debug
			    printf("Force Waiting Before Commit\n");
#endif
			    beam_nr_call->call=NULL;
			    beam_nr_call->state=WAITING_TO_BE_LEFTMOST_PARENT;
			    beam_ABX->suspended=addto_suspensions_list(beam_ABX,LEFTMOST_SUSPENSION);
			    beam_nr_call=beam_nr_call->next;
			    goto next_call;
			  }
			    beam_ABX->side_effects-=CUT;
			    beam_nr_call=remove_call_from_andbox(beam_nr_call,beam_ABX);

#if Debug
			    printf("Executando o commit (apaga %d alternatives) \n",beam_OBX->nr_all_alternatives-1);
			    if (beam_ABX->externals!=NULL && beam_OBX->nr_all_alternatives>1) printf("commit com externals (noisy) \n");
			    if (beam_ABX->externals!=NULL && beam_OBX->nr_all_alternatives==1) printf("commit com externals (degenerate) \n");
#endif

			    if (beam_OBX->nr_all_alternatives>1) {
			      beam_nr_alternative=beam_ABX->nr_alternative;
			      beam_OBX->nr_all_alternatives=1;
			      new=beam_OBX->alternatives;
			      beam_OBX->alternatives=beam_nr_alternative; /* fica a ser a unica alternativa */
			      do {
			          struct status_or *old;
			          old=new;
			          new=new->next;
				  if (old!=beam_nr_alternative) {
 			            del_andbox_and_sons(old->alternative);
			            if (new==NULL) remove_memory_arguments(old->args);
                                    free_memory((Cell *) old,STATUS_OR_SIZE);
				  }
			      } while (new!=NULL);
			      beam_nr_alternative->next=NULL;
			      beam_nr_alternative->previous=NULL;
			    }
			    goto unique_alternative;			  
			}

		jump:
#if Debug
		        printf("%5d->jump inst %ld\n",contador++,(long int) arg1);
break_debug(contador);
#endif
		        beam_pc=(Cell *) arg1; 
			execute_next();


	save_pair_Y:
#if Debug
		        printf("%5d->save_pair Y%ld\n",contador++,(long int) arg1);
break_debug(contador);
#endif
		        abort_eam("save_exp no emulador ?????");
			--S;
			beam_varlocals[arg1]=abspair(beam_S);
			++S;
			beam_pc+=2;
			execute_next();

	save_appl_Y:
#if Debug
		        printf("%5d->save_appl Y%ld\n",contador++,(long int) arg1);
break_debug(contador);
#endif
		        abort_eam("save_exp no emulador ?????");
			--S;
			beam_varlocals[arg1]=absappl(beam_S);
			++S;
			beam_pc+=2;
			execute_next();


	save_appl_X:
#if Debug
		        printf("%5d->save_appl X%ld\n",contador++,(long int) arg1);
break_debug(contador);
#endif
		        abort_eam("save_exp no emulador ?????");
			--S;
			beam_X[arg1]=absappl(beam_S);
			++S;
			beam_pc+=2;
			execute_next();

	save_pair_X:
#if Debug
		        printf("%5d->save_pair X%ld\n",contador++,(long int) arg1);
break_debug(contador);
#endif
		        abort_eam("save_exp no emulador ?????");
			--S;
			beam_X[arg1]=abspair(beam_S);
			++S;
			beam_pc+=2;
			execute_next();

        p_atom:
        p_atomic:
        p_integer:
        p_nonvar:
        p_number:
        p_var:
        p_db_ref:
        p_primitive:
        p_cut_by:
        p_save_by:
        p_succ:
        p_predc:
        p_plus:
        p_minus:
        p_times:
        p_div:
        p_equal:
        p_dif:
        p_eq:
        p_arg:
        p_functor:

			 abort_eam("std_pred no emulador ?????");
        orelse:
        orlast:
	either:
		        abort_eam("either/orelse/orlast ainda nao implementadas ?????");

	save_b_X:
	save_b_Y:
	comit_b_X:
	comit_b_Y:
			abort_eam("save_b_X/Y ou comit_b_X/Y no emulador ?????\n");

     }
return (TRUE);
}

/* The Inst_am instruction is used in eamamasm.c */

Cell inst_am(int n);
Cell am_to_inst(Cell inst);

Cell inst_am(int n)
{
#if DIRECT_JUMP
     if (TABLE_OPS==NULL) eam_am(NULL);
     return TABLE_OPS[n];
#else
     return(n);
#endif
}

Cell am_to_inst(Cell inst)
{
#if DIRECT_JUMP
int n;
  for(n=0;n<=_p_functor; n++) if ((Cell) TABLE_OPS[n]==inst) return (n);
#endif

return(inst);
}



#if Debug_Dump_State
/************************************************************************\
 * MORE DEBUG STUFF 					                 *
\************************************************************************/
#define DUMP_BOXES  0
#define DUMP_STATES 1
#define DUMP_VARS   2


void dump_eam_orbox(struct OR_BOX *o, struct AND_BOX *pai, struct status_and *pai2);
void dump_eam_andbox(struct AND_BOX *a, struct OR_BOX *pai, struct status_or *pai2);
char *SPACES(int level);

#define SPACE_MULT 4
char *SPACES(int level) {
  static char spaces[2000];
  int i;

  for(i=0;i<level*SPACE_MULT;i++) {
      spaces[i]=' ';
  }
  spaces[level*SPACE_MULT]=0;
  return (spaces);
}

void dump_eam_state() {
  static int nr_state=0;
  int nr=0;
  printf("State %d:\n",++nr_state);

  /* verify suspended boxes */
  if (beam_su!=NULL) {
       struct SUSPENSIONS *s,*l;
       l=beam_su->prev;
       s=beam_su;
       do {
	 nr++;
	 if (s->prev!=l) abort_eam("Invalid list of Suspended boxes\b");
	 l=s;
	 s=s->next;
       } while(s!=beam_su);
  }
  printf("%d suspended boxes\n",nr);

  dump_eam_andbox(beam_top,NULL, NULL);
}


void dump_eam_andbox(struct AND_BOX *a, struct OR_BOX *pai, struct status_or *pai2) {
  struct status_and *calls, *last;

  if (a==NULL) return;
  if (pai!=a->parent) abort_eam("Pai diferente do parent\n");
  if (pai2!=a->nr_alternative) abort_eam("Status call Pai diferente do nralternative\n");
  if (a==beam_ABX) printf("->"); else printf("  ");
  if (a->suspended) printf("*"); else printf(" ");
  printf("%s+ANDBOX with %d goals\n",SPACES(2*(a->level)),a->nr_all_calls); 

  calls=a->calls;
  last=NULL;
  while(calls!=NULL) {
    if (calls->previous!=last) abort_eam("link errado nos calls\n");
    if (calls->locals==NULL) printf("   %sNO local vars\n",SPACES(2*(a->level)+1)); 
    else printf("   %s%d local vars\n",SPACES(2*(a->level)+1),calls->locals[-1]); 
    if (calls->call==NULL) { 
      printf("   %s>ORBOX EMPTY\n",SPACES(2*(a->level)+1));   
    } else {
      dump_eam_orbox(calls->call,a,calls);
    }
    last=calls;
    calls=calls->next;
  }
  //  printf("Exit from dum_eam_andbox\n");
}

void dump_eam_orbox(struct OR_BOX *o, struct AND_BOX *pai, struct status_and *pai2) {
  struct status_or *i,*last;
  if (o==NULL) return;
  if (pai!=o->parent) abort_eam("Pai diferente do parent\n");
  if (pai2!=o->nr_call) abort_eam("Status call Pai diferente do nrcall\n");
  if (o==beam_OBX) printf("=> "); else printf("   ");

  printf("%s>ORBOX with %d alternatives\n",SPACES(2*(o->parent->level)+1),o->nr_all_alternatives); 

  i=o->alternatives;
  last=NULL;
  while(i!=NULL) {
    if (i->previous!=last) abort_eam("link errado nas alternativas\n");
    if (i->args) {
        printf("   %s+%d Arguments\n",SPACES(2*(o->parent->level+1)),i->args[0]);
        if (i->args[0]<0 || i->args[0]>1000) abort_eam("Num Invalido de Args\n");
    }
    if (i->alternative==NULL) { 
      printf("   %s+ANDBOX EMPTY\n",SPACES(2*(o->parent->level+2))); 
    } else {
      dump_eam_andbox(i->alternative,o, i);
    }
    last=i;
    i=i->next;
  }
}

#endif


#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

int showTime(void)  /* MORE PRECISION */
{
static int call_time=0;
static struct timeval StartTime;
static struct timezone TimeZone={0,0};

#if MICRO_TIME
  if (!call_time) {
    gettimeofday(&StartTime,&TimeZone);
    call_time=1;
  } else {
    struct timeval time,diff;

    call_time=0;
    gettimeofday(&time,&TimeZone);
    diff.tv_sec = time.tv_sec - StartTime.tv_sec;
    diff.tv_usec = time.tv_usec - StartTime.tv_usec;
    if(diff.tv_usec < 0){
   	diff.tv_usec += 1000000;
        diff.tv_sec -= 1;
    }
    printf("CPU Time %ld (Microseconds)\n", (diff.tv_sec*1000000)+(diff.tv_usec));
  }
  return(TRUE);
#else
   struct rusage rusage;

  /* InitTime() and cputime() from sysbits.c */
  if (!call_time) {
    getrusage(RUSAGE_SELF, &rusage);
    StartTime.tv_sec = rusage.ru_utime.tv_sec;
    StartTime.tv_usec = rusage.ru_utime.tv_usec;
    call_time=1;
  } else {
    struct timeval	diff;

    call_time=0;
    getrusage(RUSAGE_SELF, &rusage);
    diff.tv_sec = rusage.ru_utime.tv_sec - StartTime.tv_sec;
    diff.tv_usec = rusage.ru_utime.tv_usec - StartTime.tv_usec;
    if(diff.tv_usec < 0){
   	diff.tv_usec += 1000000;
        diff.tv_sec -= 1;
    }
    printf("CPU Time %ld (Miliseconds)\n", (diff.tv_sec*1000)+(diff.tv_usec/1000));
  }
  
  return(TRUE);
#endif
}





#if USE_SPLIT
   #include "eam_split.c"
#endif

#if GARBAGE_COLLECTOR
/************************************************************************\
 * GC             					                 *
\************************************************************************/

  #include "eam_gc.c"
#endif


#endif  /* BEAM */
