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

#include "eam.h"
#include "eamamasm.h"

#define Debug 0
#define Debug_GC 0
#define Debug_Dump_State 0  /* 0 =off || 1==only on Scheduling || 2== only on GC || 4=on every abs inst NOTE: DEBUG has to be enable to use 4*/
#define Debug_MEMORY 0
#define Memory_Stat  0
#define Clear_MEMORY 0      /* 0- do not clear || 1-> clear on request  || 2-> clear on release || 3 -> both*/
#define Fast_go 1           /* normaly 1 ; use 0 to run some extra tests only to control some possible bugs (slower) */
#define USE_SPLIT     1

#define MEM_FOR_BOXES  128  /* In Mb */
#define MEM_FOR_HEAP   128  /* In Mb */
#define MEM_FOR_VARS   64   /* In Mb */
#define MEM_BOXES      MEM_FOR_BOXES*1024*1024
#define MEM_H          MEM_FOR_HEAP*1024*1024
#define MEM_VARS       MEM_FOR_VARS*1024*1024
#define GARBAGE_COLLECTOR 2 /* 0= NO GC || 1 = Heap only || 2 = Heap + Box */
#define HYBRID_BOXMEM  1    /* 0 - Off  || 1 - On */

#define START_ON_NEXT  1    /* PLEASE DON'T CHANGE , specially if you use skip_while_var */
#define USE_LEFTMOST   1    /* SHOULD ALWAYS BE 1 for now... */ 
#define ENABLE_INDEX   1    /* 0 == indexing disable        1 == indexing on first arg enable */
#define MICRO_TIME     1    /* 0 == eamtime uses CPU time   1 == eamtime uses total time */

#define READ 0
#define WRITE 1


/* HERE ARE THE REGS NEEDED FOR EAM EMULATOR */

#define _X   XREGS      /* use the same X-Regs as YAP */
Cell *pc;
Cell *_H;
Cell *_S;
short _Mode;            /* read or write mode                     */
short ES;               /* goal shoud do Eager Split yes or no ?  */ 
Cell *var_locals;       /* local vars to the working AND-BOX      */
struct AND_BOX  *ABX;   /* working AND-BOX                        */ 
struct OR_BOX   *OBX;   /* working OR-BOX                         */
struct SUSPENSIONS *SU; /* list with suspended work               */

struct status_and *USE_SAME_ANDBOX;  /* when only 1 alternative   */
struct status_or *nr_alternative;    /* working alternative       */
struct status_and *nr_call;          /* working goal              */

int EAM=0;              /* Is EAM enabled ?                       */
Cell *VAR_TRAIL;        
int VAR_TRAIL_NR;
int Mem_FULL;           /*  if mem_full, then perform GC          */
int nr_call_forking;    /* number of splits already performed     */
unsigned long START_ADDR_HEAP, START_ADDR_BOXES, END_BOX, END_H;

#define isvar(a)   IsVarTerm((Cell) a)
#define isappl(a)  IsApplTerm((Cell) a)  
#define ispair(a)  IsPairTerm((Cell) a)
#define isatom(a)  IsAtomOrIntTerm((Cell) a)                   
#define reppair(a) RepPair((Cell) a)
#define repappl(a) RepAppl((Cell) a)
#define abspair(a) AbsPair((Term *) a)
#define absappl(a) AbsAppl((Term *) a)
int is_perm_var(Cell *a); inline int is_perm_var(Cell *a) { if (a<(Cell *) END_BOX) return(0); else return (1); }

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
extern Int BEAM_is(void);
extern void do_eam_indexing(struct Predicates *);
extern void Yap_plwrite(Term, int (*mywrite) (int, int), int);

#if Debug_Dump_State
   void dump_eam_state(void);
#endif

#define Direct_Jump 1
struct AND_BOX  *top;
#if Debug
   #define INLINE
   #define DIRECT_JUMP 0
   int contador;
   void break_top(void); void break_top(void) { };
   void break_debug(void); 
   void break_debug(void) {
#if Debug_Dump_State & 4
  		        dump_eam_state();
#endif
			printf("(%d %1d) ->", contador++,_Mode); 
   };
#else
#define INLINE  inline
#define DIRECT_JUMP Direct_Jump
#endif
#if Memory_Stat
   #define MAX_MEMORYSTAT 5000
   unsigned long TOTAL_MEM, MEM_REUSED, TOTAL_TEMPS,TEMPS_REUSED, TOTAL_PERMS, PERMS_REUSED;
   unsigned long Memory_STAT[MAX_MEMORYSTAT][5];
#endif
 
#define arg1  *(pc+1)
#define arg2  *(pc+2)
#define arg3  *(pc+3)
#define arg4  *(pc+4)

#define STACK_SIZE 4000
Cell MyStack[STACK_SIZE];
Cell *sp;
#if Fast_go
    #define test_stack_overflow() {};
#else
    #define test_stack_overflow() { if (sp>&MyStack[STACK_SIZE]) abort_eam("PopStack too Small\n"); }
#endif
#define push_mode_and_sreg() { *--sp = (Cell) _Mode; *--sp  = (Cell) _S; }
#define pop_mode_and_sreg()  { _S = (Cell *) *sp++; _Mode = (short) *sp++; test_stack_overflow(); }

int Force_Wait;
#define CELL_SIZE  (sizeof(Cell))
#define POINTER_SIZE (sizeof(Cell *))
#define ANDBOX_SIZE (sizeof(struct AND_BOX))
#define ORBOX_SIZE (sizeof(struct OR_BOX))
#define PERM_VAR_SIZE (sizeof(struct PERM_VAR))
#define EXTERNAL_VAR_SIZE (sizeof(struct EXTERNAL_VAR))
#define SUSPENSIONS_SIZE (sizeof(struct SUSPENSIONS))
#define SUSPENSIONS_VAR_SIZE (sizeof(struct SUSPENSIONS_VAR))
#define STATUS_AND_SIZE (sizeof(struct status_and))
#define STATUS_OR_SIZE (sizeof(struct status_or))

#define INDEX_SIZE       100000  /* size of vector for saving memory requests */
Cell *Index_Free[INDEX_SIZE];
Cell *Next_Free;
struct PERM_VAR *Next_Var;
unsigned int MEM_Going;
unsigned int nr_call_gc_heap;
unsigned int nr_call_gc_boxed;


/************************************************************************\
* Debug + Status routines  						 *
\************************************************************************/

void conta_memoria_livre(int size){
int i,nr,ult=0;
long total=0;
Cell *c;

 for(i=0;i<INDEX_SIZE;i++) {
   nr=0;
   c=Index_Free[i];
   
   while(c!=NULL) {
     ult=i;
     nr++;
     c=(Cell *) *c;
   }
   total=total+nr*i;
 } 
 printf("Ultimo Pedido (bytes) =%d ¦ Ultimo bloco livre=%d\n",size,ult*CELL_SIZE);
 printf("Memoria TOTAL (bytes)      =%ld \n",((unsigned long) END_BOX)-((unsigned long) START_ADDR_BOXES));
 printf("Memoria livre no Index_Free=%ld \n",total*CELL_SIZE);
 printf("Memoria Total livre        =%ld \n",total*CELL_SIZE+((unsigned long) END_BOX)-((unsigned long)Next_Free));
 printf("Memoria Total na HEAP=%ld    livre=%ld \n",(unsigned long) MEM_H,(unsigned long) _H-(unsigned long) START_ADDR_HEAP);
}

void abort_eam(char *s)
{
  printf("%s\n",s);
  exit(1);
}

void exit_eam(char *s)
{
  printf("%s\n",s);
  if (nr_call_forking) printf("%d forks executed\n",nr_call_forking);

  if (nr_call_gc_heap) 
   printf("GC was called %d times on Heap  Mem\n",nr_call_gc_heap);
  if (nr_call_gc_boxed)
   printf("GC was called %d times on Boxed Mem\n",nr_call_gc_boxed);
  if (nr_call_gc_boxed && nr_call_gc_heap)
   printf("GC was called %d times \n",nr_call_gc_boxed+nr_call_gc_heap);

#if Memory_Stat
  {unsigned long req, used;
   req=TOTAL_MEM+TOTAL_PERMS;
   used=(TOTAL_MEM+TOTAL_PERMS)-(MEM_REUSED+PERMS_REUSED);

  printf("-------------------------------------------------------------------\n");
  printf("Total Mem: Requested %ld (%.2fKb) (%.2fMb) \n", req, req/1024.0, req/1048576.0);
  printf("           Used      %ld (%.2fKb) (%.2fMb) / Reused (%3.2f%c)\n", used,used/1024.0, used/1048576.0, (float) (req-used)/req*100,'%');
  printf("-------------------------------------------------------------------\n");

  used=(TOTAL_MEM-TOTAL_TEMPS)-(MEM_REUSED-TEMPS_REUSED);
  printf("Boxed Mem: Requested %ld (%.2fKb) (%.2fMb) \n", TOTAL_MEM-TOTAL_TEMPS, (TOTAL_MEM-TOTAL_TEMPS)/1024.0, (TOTAL_MEM-TOTAL_TEMPS)/1048576.0);
  printf("           Used      %ld (%.2fKb) (%.2fMb) / Reused (%3.2f%c)\n", used, used/1024.0, used/1048576.0, (float) (MEM_REUSED-TEMPS_REUSED)/(TOTAL_MEM-TOTAL_TEMPS)*100,'%');

  used=TOTAL_TEMPS-TEMPS_REUSED;
  printf("Temps Mem: Requested %ld (%.2fKb) (%.2fMB)\n", TOTAL_TEMPS, TOTAL_TEMPS/1024.0, TOTAL_TEMPS/1048576.0);
  printf("           Used      %ld (%.2fKb) (%.2fMb) / Reused (%3.2f%c)\n", used, used/1024.0,used/1048576.0,(float) TEMPS_REUSED/(TOTAL_TEMPS)*100,'%');


  used=TOTAL_PERMS-PERMS_REUSED;
  printf("Perms Mem: Requested %ld (%.2fKb) (%.2fMB)\n", TOTAL_PERMS, TOTAL_PERMS/1024.0, TOTAL_PERMS/1048576.0);
  printf("           Used      %ld (%.2fKb) (%.2fMb) / Reused (%3.2f%c)\n", used, used/1024.0,used/1048576.0,(float) PERMS_REUSED/(TOTAL_PERMS)*100,'%');
  }
  printf("-------------------------------------------------------------------\n");
  if (nr_call_gc_boxed+nr_call_gc_heap>0) {
  int i;
    Memory_STAT[0][0]=0; Memory_STAT[0][1]=0; Memory_STAT[0][2]=0; Memory_STAT[0][3]=0; Memory_STAT[0][4]=0;
    for(i=1;i<=nr_call_gc_boxed+nr_call_gc_heap;i++) {
      Memory_STAT[0][0]+=Memory_STAT[i][0];
      Memory_STAT[0][1]+=Memory_STAT[i][1];
      Memory_STAT[0][2]+=Memory_STAT[i][2];
      Memory_STAT[0][3]+=Memory_STAT[i][3];
      Memory_STAT[0][4]+=Memory_STAT[i][4];
      printf("GC %4d Time=%ld  H=%ld to %ld (%3.2f) Box=%ld to %ld (%3.2f)\n",
           i, Memory_STAT[i][0], Memory_STAT[i][1], Memory_STAT[i][3], 
           ((float)  Memory_STAT[i][3]/Memory_STAT[i][1])*100 , Memory_STAT[i][2], Memory_STAT[i][4],
           ((float)  Memory_STAT[i][4]/Memory_STAT[i][2])*100);
    }
      i--;
      printf("\nRESUME GC: Time=%ld  H=%ld to %ld (%3.2f) Box=%ld to %ld (%3.2f)\n",
           Memory_STAT[0][0]/i, Memory_STAT[0][1]/i, Memory_STAT[0][3]/i, 
           100.0-((float)  Memory_STAT[0][3]/Memory_STAT[0][1])*100 , Memory_STAT[0][2]/i, Memory_STAT[0][4]/i,
           100.0-((float)  Memory_STAT[0][4]/Memory_STAT[0][2])*100);

  } else {
    printf("Heap Mem Requested %ld (%.2fKb) (%.2fMB) \n", ((unsigned long) _H-START_ADDR_HEAP), ((unsigned long) _H-START_ADDR_HEAP)/1024.0, ((unsigned long) _H-START_ADDR_HEAP)/1048576.0);
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
     START_ADDR_HEAP=(unsigned long) malloc(MEM_H+MEM_BOXES+MEM_VARS);
     if ((void *)START_ADDR_HEAP==(void *)NULL) abort_eam("Memory Initialization Error\n");
     START_ADDR_BOXES=START_ADDR_HEAP+MEM_H;

     END_H=START_ADDR_HEAP+MEM_H; 
     END_BOX=START_ADDR_BOXES+MEM_BOXES;
   } 


   Next_Var=(struct PERM_VAR *) END_BOX;
   _H=(Cell *) START_ADDR_HEAP;
#if GARBAGE_COLLECTOR!=2
   Next_Free=(Cell *) END_BOX;
#else
   Next_Free=(Cell *) START_ADDR_BOXES;
#endif
   MEM_Going=1;
   memset(Index_Free,0,INDEX_SIZE*POINTER_SIZE);
   { int i,max;
     max=MEM_VARS/PERM_VAR_SIZE;
     for(i=0;i<max-1;i++) {
       Next_Var[i].next=&Next_Var[i+1];
     }
     Next_Var[max-1].next=NULL;
   }

#if Debug
   contador=1;
#endif
   var_locals=NULL;
   USE_SAME_ANDBOX=NULL;
   nr_alternative=NULL;
   nr_call=NULL;
   Force_Wait=0;
   sp=&MyStack[STACK_SIZE-1];
   nr_call_gc_heap=0;
   nr_call_gc_boxed=0;
   _Mode=READ;
   VAR_TRAIL_NR=0;
   nr_call_forking=0;
   Mem_FULL=0;
#if Memory_Stat
        TOTAL_MEM=0; MEM_REUSED=0; TOTAL_TEMPS=0; TEMPS_REUSED=0; TOTAL_PERMS=0; PERMS_REUSED=0;
	memset(Memory_STAT,0,MAX_MEMORYSTAT*5*sizeof(unsigned long));
#endif
}

INLINE int HEAP_MEM_FULL(void)
{
    if (MEM_Going==1) {
      if ((unsigned long)_H>(unsigned long)(START_ADDR_HEAP+MEM_H/2)) {
	Mem_FULL|=2;
      }
   } else {
      if ((unsigned long) _H>(unsigned long)(START_ADDR_HEAP+MEM_H)) {
	Mem_FULL|=2;
      }
   }

  return(Mem_FULL);
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
   mem=Index_Free[(unsigned) size_cells];
 #if Memory_Stat
   TOTAL_MEM+=size;
   if (mem!=NULL) MEM_REUSED+=size;
 #endif
   if (mem==NULL) {

#else  /* GC Only */
   #if Memory_Stat
     TOTAL_MEM+=size;
   #endif
   if (1) {
#endif

  #if GARBAGE_COLLECTOR!=2
       Next_Free-=size_cells;
       mem=Next_Free;
       if (Next_Free< (Cell *) START_ADDR_BOXES) abort_eam("No more BOX_MEM \n");
  #else
     if (MEM_Going==1) {
       mem=Next_Free;
       Next_Free+=size_cells;
       if (Next_Free> (Cell *) (START_ADDR_BOXES+MEM_BOXES/2)) Mem_FULL |= 1;
     } else {
       Next_Free-=size_cells;
       mem=Next_Free;
       if (Next_Free< (Cell *) (START_ADDR_BOXES+MEM_BOXES/2)) Mem_FULL |=1;
     }
  #endif
   } else {
     Index_Free[(unsigned) size_cells]=(Cell *) *mem;
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

    *mem=(Cell) Index_Free[size_cells];
    Index_Free[size_cells]=mem;
}
#endif

INLINE void get_arguments(int nr, Cell *a)
{
register int i;
   for(i=1;i<=nr;i++) _X[i]=a[i];
}

INLINE Cell *save_arguments(int nr) /* nr arguments */
{
   if (!nr) return(NULL);
   {
        register int i;
        register Cell *a;

	a=(Cell *)request_memory((nr+1)*CELL_SIZE);
	a[0]=nr+1;  
        for(i=1;i<=nr;i++) a[i]=_X[i];
	return(a);
   } 
}

INLINE void remove_memory_arguments(Cell *a)
{
  if (a==NULL) return;
  free_memory(a,a[0]*CELL_SIZE);
}

struct PERM_VAR *request_permVar(struct AND_BOX *a) {
struct PERM_VAR *pv;

#if Memory_Stat
  static struct PERM_VAR *old=NULL;
  TOTAL_PERMS+=PERM_VAR_SIZE;
  if (old<=Next_Var) old=Next_Var;
  else PERMS_REUSED+=PERM_VAR_SIZE;
#endif  

#if Debug || Debug_MEMORY
  printf("Requesting a permVar...\n");
#endif

#if !Fast_go
  if (Next_Var->next==NULL) { printf("Fim da memoria para variaveis\n"); exit (-1); }
#endif

  pv=Next_Var;
  Next_Var=Next_Var->next;

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

  v->next=Next_Var;
  Next_Var=v;
  return;
}


INLINE Cell *request_memory_locals(int nr)
{
Cell *l;
int i;

#if Memory_Stat
    Cell *old;
    old=Next_Free;
    TOTAL_TEMPS+=CELL_SIZE*(nr+1); 
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
    if (old==Next_Free) TEMPS_REUSED+=CELL_SIZE*(nr+1); 
#endif

return(l);
}

INLINE Cell *request_memory_locals_noinit(int nr)
{
Cell *l;

#if Memory_Stat
    Cell *old;
    old=Next_Free;
    TOTAL_TEMPS+=CELL_SIZE*(nr+1); 
#endif

#if Debug_MEMORY
  printf("Requesting Memory for %d+1 locals (not initialized)...\n",nr);
#endif


    l=(Cell *)request_memory(CELL_SIZE*(nr+1));
    l[0]=nr;
    l++;

#if Memory_Stat
    if (old==Next_Free) TEMPS_REUSED+=CELL_SIZE*(nr+1); 
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
  if (SU==b) return; /* is already on top of list */ 
  if (SU->prev==b) { SU=b; return; } /* It was the last one */

  b->prev->next=b->next;
  b->next->prev=b->prev;

  b->next=SU;
  b->prev=SU->prev;
  SU->prev=b;
  b->prev->next=b;
  SU=b;
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
  if (SU==NULL) {
    s->next=s;
    s->prev=s;
    SU=s;
  } else {
    s->next=SU;
    s->prev=SU->prev;
    SU->prev=s;
    if (SU->next==SU) { /* so existem 2 elementos na lista */
      SU->next=s;
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

  if (b==SU) SU=b->next;

  if (b==SU) {  /* so existe um */ 
    SU=NULL;
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
  if (a==top) return(1);
  if (a->calls!=n) return(0);
  if (a->nr_alternative->previous!=NULL) return(0);

return(is_leftmost(a->parent->parent,a->parent->nr_call));
}

struct AND_BOX *choose_leftmost(void)
{
  struct AND_BOX *a;
  struct OR_BOX *o=NULL;
  struct status_and *ncall;
  
  a=top;
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
    if (a==NULL) { OBX=o; return(a); }
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
	(Cell *) l=(Cell *) e->var;
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
	   trail(ABX,(struct PERM_VAR *) b);
	   return;
	 } else {
	   *a=(Cell) b;
	   trail(ABX,(struct PERM_VAR *) a);
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
        { *a=(Cell) b; trail(ABX,(struct PERM_VAR *)a); }
	return 1;
    }
    if(isvar(b)) {
        { *b=(Cell) a; trail(ABX,(struct PERM_VAR *)b); }
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
    l=ABX->perms;
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
    _DR=(Cell *) deref(_X[1]);

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
	  _X[i]=(Cell) _DR;
      }
      return (ppe);
    }
    
return (NULL);
}


#if Debug
     #define execute_next()  if (Debug!=-1 && contador>Debug*100) abort_eam("exit por contador>debug\n"); else goto *OpAddress[*pc]
 
#else
 #if DIRECT_JUMP
     #define execute_next() goto **((void **) pc)
 #else
     #define execute_next()  goto *OpAddress[*pc]
 #endif
#endif

int eam_am(PredEntry *initPred);
int eam_am(PredEntry *initPred)
{
static void *OpAddress[] asm("TABLE")= {
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
Cell code2start[]={_prepare_calls,1,0,_call_op,0,0};

    	if ((long) initPred==2) { /* retry from call eam(goal) */
	   goto fail; 
        } else if ((long) initPred==1) { /* first time call eam(goal) */
	   initPred=prepare_args_torun();  
        } 

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

	SU=NULL;
	OBX=NULL;
	ABX=(struct AND_BOX *) request_memory(ANDBOX_SIZE);
	ABX->parent=NULL;
	ABX->nr_alternative=NULL;
	ABX->nr_all_calls=0;
	ABX->perms=NULL;
	ABX->calls=NULL;
	ABX->level=1;
	ABX->externals=NULL;
	ABX->suspended=NULL;
	ABX->side_effects=0;
	top=ABX;

if (1) { int i;  /* criar mais um nivel acima do top para o caso de haver variaveis na chamada */
	ABX->nr_all_calls=1;
        ABX->calls=  (struct status_and *) request_memory(STATUS_AND_SIZE);
	ABX->calls->locals=NULL;
	ABX->calls->code=NULL;
	ABX->calls->state=RUNNING;
	ABX->calls->previous=NULL;
	ABX->calls->next=NULL;
        OBX= (struct OR_BOX *) request_memory(ORBOX_SIZE);
	ABX->calls->call=OBX;
	OBX->nr_call=ABX->calls;
	OBX->parent=ABX;
	OBX->nr_all_alternatives=1;
	OBX->eager_split=0;

	OBX->alternatives=(struct status_or *) request_memory(STATUS_OR_SIZE);
	OBX->alternatives->previous=NULL;
	OBX->alternatives->next=NULL;
	OBX->alternatives->args=NULL;
	OBX->alternatives->code=NULL;
	OBX->alternatives->state=RUNNING;

	ABX=(struct AND_BOX *) request_memory(ANDBOX_SIZE);
	OBX->alternatives->alternative=ABX;
	ABX->parent=OBX;
	ABX->nr_alternative=OBX->alternatives;
	ABX->nr_all_calls=0;
	ABX->perms=NULL;
	ABX->calls=NULL;
	ABX->level=2;
	ABX->externals=NULL;
	ABX->suspended=NULL;
	ABX->side_effects=WRITE;
	
	for(i=1;i<=initPred->beamTable->arity;i++) add_vars_to_listperms(ABX,(Cell *) _X[i]);
}

	pc=code2start;
	execute_next();

	while (1) {

               exit_eam:
#if Debug
break_debug();
			printf("(%3d) %d ->", (int) *pc, contador++); 
#endif		        

                wake:
#if Debug
break_debug();
			printf("Trying WAKE and_box on suspension \n");
#endif
		        if (verify_externals(ABX)==0) goto fail_verify_externals;
			if (ABX->externals==NULL) {
			              nr_call=ABX->calls;
				      if (nr_alternative->state & END) {
					  goto success;
			              } 
				      nr_alternative->state=RUNAGAIN;
				      goto next_call;
			 }
			 nr_alternative->state=SUSPEND;
			 /* must clear all external assignments */
			 limpa_trail(ABX);
			 /* goto top_tree; */

	       top_tree:
#if Debug
break_debug();
		        printf("I'm on top of the Tree (maybe exit or look for suspended alternatives) \n");
			break_top();
#endif		        

#if GARBAGE_COLLECTOR
			if (HEAP_MEM_FULL()) garbage_collector();
#endif

#if USE_LEFTMOST
		      if (SU!=NULL) {
			 ABX=SU->and_box;
		         OBX=ABX->parent;
		         nr_alternative=ABX->nr_alternative;
		         if (nr_alternative->state & (WAKE))  goto wake;			 
		       }
		       ABX=choose_leftmost();
		       if (ABX==NULL) { /* Must return to next_alternative in OBX  BECAUSE EAGER_SPLIT*/
			 nr_alternative=ABX->nr_alternative;
			 ABX=OBX->parent;
			 goto  next_alternative;
		       }
		       if (ABX!=top && ABX->suspended!=NULL) {
#else
			if (SU!=NULL) { /* There are suspended alternatives */
			  ABX=SU->and_box;
#endif

#if !Fast_go
			  if (ABX==NULL || ABX->parent==NULL || ABX->parent->alternatives==NULL) abort_eam("Alternativa NULL NO TOP ?????"); 
#endif
			  OBX=ABX->parent;
			  nr_alternative=ABX->nr_alternative;

			  if (ABX->suspended->reason==VAR_SUSPENSION) {
                                delfrom_suspensions_list(ABX->suspended);
			        nr_call=ABX->calls;
			        goto next_call;			     
			  }
			  if (ABX->suspended->reason!=NORMAL_SUSPENSION) {
			     if (ABX->calls->state==WAITING_TO_BE_FIRST ||
				 (ABX->calls->state & WAITING && is_leftmost(ABX,0))) {

                                delfrom_suspensions_list(ABX->suspended);
			        ABX->calls->state=READY;
			        nr_call=ABX->calls;
			        goto next_call;
			     }
#if !USE_LEFTMOST
			     SU=SU->next;
			     goto top_tree;
#endif
			  }

			  if (OBX->nr_all_alternatives==1 && ABX->level>OBX->parent->level) {
#if !Fast_go
			    if (OBX->parent->parent==NULL) abort_eam("Null no top_tree ");
#endif
			    goto unique_alternative;
			  }
			  if (nr_alternative->state & (WAKE))  goto wake;
			  if (OBX->nr_all_alternatives>1) {
#if Debug
break_debug();
			     printf("Trying Fork in suspended and_box \n");
#endif
			     /* pickup the left most alternative instead */
		 split:			     
			     OBX=ABX->parent;
#if USE_SPLIT
			     do_forking_andbox(ABX);
#else
			     abort_eam("ERROR: Split disable, cannot run non-deterministic programs...");
#endif
			     OBX=ABX->parent;
			     nr_alternative=ABX->nr_alternative;
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
break_debug();
		        printf("proceed... \n");
#endif

			if (USE_SAME_ANDBOX!=NULL) {  /* was only one alternative */
			  USE_SAME_ANDBOX=NULL;
			  nr_call=remove_call_from_andbox(nr_call,ABX);
			  goto next_call;
			} 
			if (ABX->externals!=NULL) {
			    nr_alternative->state=SUSPEND_END;
			    goto suspend;
			}

	        success:
#if Debug
break_debug();
			printf("SUCCESS for call %p  in level %d \n", nr_call, ABX->level );
#endif
			/* FOUND SOLUTION -> ALL_SOLUTIONS */ 
			//if ((ABX->side_effects & WRITE) && OBX->nr_all_alternatives>1) 
			  if (OBX->parent==top) {  
			      give_solution_toyap(); 
			      return (TRUE); 
			      goto fail; 
			  }

			ABX=OBX->parent;
			nr_call=OBX->nr_call;
			del_orbox_and_sons(OBX);  
			nr_call=remove_call_from_andbox(nr_call,ABX);

			if (ABX->externals!=NULL) {
			    if (ABX->nr_all_calls==0) {
			         nr_alternative->state=SUSPEND_END;
			    } else nr_alternative->state=SUSPEND;
			    goto suspend;			    
			}

			if (ABX->nr_all_calls==0) {
			    OBX=ABX->parent;

			    if (OBX==NULL) {
			      goto top_tree;
			    }
			    nr_alternative=ABX->nr_alternative;
			    goto success;
			}

	        next_call:
#if Debug
break_debug();
		        printf("Searching for a next call in and_box... \n");
#endif

#if GARBAGE_COLLECTOR
			if (HEAP_MEM_FULL()) { 
			    garbage_collector();
			}
#endif

	                { register int nr;
			nr=ABX->nr_all_calls;

			if (ABX->externals!=NULL && ABX->side_effects<CUT) {
			    if (nr==0) nr_alternative->state=SUSPEND_END;
			    else { /* if next call is a cut then execute it */
			      pc=ABX->calls->code;
#if Debug
			      if (*pc==_cut_op) {
#else
			      if (*pc==(Cell) &&cut) { 
#endif
				nr_call=ABX->calls;
			        execute_next();				
			      }
			      nr_alternative->state=SUSPEND; 
			    }
			    goto suspend;	
			}			  
			if (nr==0) {
			  goto success;
			}
#if !START_ON_NEXT
			nr_call=ABX->calls;
#else
/*			if (ABX->parent==OBX) nr_call=ABX->calls; else nr_call=OBX->nr_call->next;  */
#endif
			while(nr_call!=NULL) {
			  
			   if (nr_call->state & WAITING) {
			     if (nr_call->state==WAITING_TO_BE_LEFTMOST) {
			       if (!is_leftmost(ABX,nr_call)) {
				    ABX->suspended=addto_suspensions_list(ABX,LEFTMOST_SUSPENSION);
			            nr_call=NULL;
			            break;			       
			       }
			       nr_call->state=READY;
			     }

			     if (nr_call->state==WAITING_TO_BE_LEFTMOST_PARENT) {
			       if (!is_leftmost(ABX->parent->parent,ABX->parent->nr_call)) {
				    ABX->suspended=addto_suspensions_list(ABX,LEFTMOST_SUSPENSION);
			            nr_call=NULL;
			            break;			       
			       }
			       nr_call->state=READY;
			     }

			     if (nr_call->state==WAITING_TO_BE_FIRST) {
			            if (nr_call->previous==NULL) { 
#if Debug
			               printf("I can stop Waiting on call %p\n", nr_call);
#endif
				       nr_call=remove_call_from_andbox(nr_call,ABX);
				       continue;
			            }
#if Debug
			            printf("Force Waiting on call %p\n", nr_call);
#endif
			            nr_call=NULL;
			            break;
			     }
			   }
			   if (nr_call->state==READY) {
			     var_locals=nr_call->locals;
			     pc=nr_call->code;
			     execute_next();
			   }
			   nr_call=nr_call->next;
			}
			OBX=ABX->parent;
			/* In case (nr_call==nr) */

			nr_alternative=ABX->nr_alternative;
  			if (ABX->externals!=NULL) goto suspend;

			if (nr_alternative!=NULL) nr_alternative=nr_alternative->next;
			goto next_alternative;
			}

	        fail_body:
	        fail_head:
	        fail:
#if Debug
break_debug();
		        printf("fail... \n");
#endif

	        fail_verify_externals:
			if (ABX->externals!=NULL) { 
			     limpa_trail(ABX);
			}

			OBX=ABX->parent;
			nr_alternative=ABX->nr_alternative;
			if (OBX==NULL) {
			  if (ABX==top) return(FALSE);
			  abort_eam("ERROR ->  ABX->parent = NULL  (em fail_verify_externals) ?????\n");
			}

			OBX->nr_all_alternatives=OBX->nr_all_alternatives-1;
			if (nr_alternative->next!=NULL) nr_alternative->next->previous=nr_alternative->previous;
			if (nr_alternative->previous!=NULL) nr_alternative->previous->next=nr_alternative->next;
			else OBX->alternatives=nr_alternative->next;  /* apaguei o primeiro da lista */
		      { register struct status_or *i;
			i=nr_alternative;
			nr_alternative=nr_alternative->next;
			free_memory((Cell *) i,STATUS_OR_SIZE);
  		        del_andbox_and_sons(ABX);
  		      }	/* verificar se existe ainda alguma alternativa viavel nesta or_box */

	        next_alternative:
#if Debug
break_debug();
		        printf("Searching for a next alternative in or_box... \n");
#endif

#if GARBAGE_COLLECTOR
			if (HEAP_MEM_FULL()) garbage_collector();
#endif

			if (OBX==NULL) {
#if !Fast_go
			      if (ABX!=top) abort_eam("Erro no next_Alternative");
#endif
			  goto top_tree;
			}

			if (OBX->nr_all_alternatives==0) {
			  ABX=OBX->parent;
			  goto fail;
			} 
			if (OBX->nr_all_alternatives==1 && ABX->level>OBX->parent->level) {
			    nr_alternative=OBX->alternatives;
			    ABX=OBX->alternatives->alternative;
			    if (ABX==NULL) {
			      pc=OBX->alternatives->code;
			      execute_next();
			    }
      		            if (OBX->parent->parent==NULL) goto top_tree;
			    goto unique_alternative;
			}
#if !START_ON_NEXT
			nr_alternative=OBX->alternatives;
#else
			/*			if (OBX->parent==ABX) nr_alternative=OBX->alternatives; 
						else { if (nr_alternative!=NULL) nr_alternative=nr_alternative->next; }  */
#endif
			while(nr_alternative!=NULL) {
			   if (nr_alternative->state & (WAKE) ) {
			      ABX=nr_alternative->alternative;
			      goto wake;
			   }
			   if (nr_alternative->state==READY) {
			       pc=nr_alternative->code;
		 	       execute_next();
			   }
			   nr_alternative=nr_alternative->next;
			}

			/* nr_alternative==NULL -> No more alternatives */
			ABX=OBX->parent;
			nr_call=OBX->nr_call->next;
			goto next_call;

	        unique_alternative:  
#if Debug
break_debug();
			printf("Unique alternative, Does Promotion on and-box\n");
#endif

#if GARBAGE_COLLECTOR
			if (HEAP_MEM_FULL() ) garbage_collector();
#endif
			if (OBX->parent->parent==NULL) {
			   goto top_tree;
			}

			{ int nr_a;
			  struct AND_BOX *a;
			  if (ABX->side_effects >= CUT) { 
			      /* Cut -> Avoid doing the Promotion */
			      inc_level(ABX,OBX->parent->level-ABX->level);

			      delfrom_suspensions_list(ABX->suspended); 
		              if (verify_externals(ABX)==0) goto fail_verify_externals; 
			      nr_alternative=ABX->nr_alternative;
			      if (ABX->externals==NULL) {
				nr_call=ABX->calls;
				goto next_call;
			      } 
			      ABX->suspended=addto_suspensions_list(ABX,NORMAL_SUSPENSION);
			      nr_alternative->state=SUSPEND;
			      nr_alternative=nr_alternative->next;
			      goto next_alternative;
			  } 
			  a=ABX;
			  ABX=OBX->parent;
			  nr_a=a->nr_all_calls;
			  nr_call=OBX->nr_call;
			  ABX->side_effects+=a->side_effects;
			  if (nr_a==0) {  /* Means SUSPENDED ON END */
			      nr_call->call=NULL;
			      nr_call->state=SUCCESS;
			      nr_call=remove_call_from_andbox(nr_call,ABX);
			  } else {  /* IF nr_all_calls==1 can be optimized ????? */
			      if (nr_a==1) {
				
				if (a->calls->call!=NULL) {
				    a->calls->call->nr_call=nr_call;
				    a->calls->call->parent=ABX;
				}
				nr_call->call=a->calls->call;
				nr_call->locals=a->calls->locals;
				nr_call->code=a->calls->code;
				nr_call->state=a->calls->state;
				free_memory((Cell *) a->calls,STATUS_AND_SIZE);
			      } else {
				struct status_and *first, *last;
			        int nr;

			        nr=ABX->nr_all_calls;
				
				first=a->calls;
				last=a->calls;
				while(1) {
				  if (last->call!=NULL) {
				    last->call->parent=ABX;
				  }
				  if (last->next==NULL) break;
				  last=last->next;
				}
				last->next=nr_call->next;
				if (nr_call->next!=NULL) nr_call->next->previous=last;
				first->previous=nr_call->previous;
				if (nr_call->previous!=NULL) nr_call->previous->next=first;
			        else ABX->calls=first; /* nr_call era o primeiro */
		                free_memory((Cell *) nr_call,STATUS_AND_SIZE);
				nr_call=first;
			        ABX->nr_all_calls=nr+nr_a-1;
			      }
			      /* Set local vars from a to point to new and_box ABX */
			  } 
			  move_perm_vars(a,ABX);

			    /* change local vars suspensions to point to new andbox */
			  { struct EXTERNAL_VAR *end,*e;
			    e=a->externals;
			    end=NULL;
			    while(e!=NULL) {
			      struct SUSPENSIONS_VAR *s;
			      s=e->var->suspensions;
			      while(s!=NULL) {
				if (s->and_box==a) { s->and_box=ABX; break; }
				s=s->next;
			      }
			      end=e;
			      e=e->next;
			    }
			    /* Clear bindings made on externals so that we are able to
			       run the verify externals */
			    e=ABX->externals;
			    while(e!=NULL) {
			      struct PERM_VAR *v;
			      v=e->var;
			      *((Cell *) v)=(Cell) v;
			      e=e->next;
			    }
			    if (end!=NULL) {
				end->next=ABX->externals;
				ABX->externals=a->externals;
			    }

			    delfrom_suspensions_list(a->suspended); /* remove suspensions */
			    free_memory((Cell *) a,ANDBOX_SIZE);
			    free_memory((Cell *) OBX->alternatives,STATUS_OR_SIZE);
			    free_memory((Cell *) OBX,ORBOX_SIZE);

			    OBX=ABX->parent;
		            if (verify_externals(ABX)==0) goto fail_verify_externals; 
			  }

			    nr_alternative=ABX->nr_alternative;
			    if (ABX->externals==NULL) {
				nr_call=ABX->calls;
				goto next_call;
			    } 
			    ABX->suspended=addto_suspensions_list(ABX,NORMAL_SUSPENSION);
			    nr_alternative->state=SUSPEND;
			    nr_alternative=nr_alternative->next;
			    goto next_alternative;
			}

	        prepare_tries:
#if Debug
break_debug();
		        printf("prepare_tries for %d clauses with arity=%d \n",(int) arg1,(int) arg2);
#endif
			if (!arg1) goto fail;
			{ register int nr;
			nr=arg1;

			if (nr==1 && ABX->parent!=NULL) {
			  ES=0;
			  nr_call->state=RUNNING;
			  pc+=3;
			  /*			  execute_next(); */
			  goto only_1_clause;
			} 

                        OBX=(struct OR_BOX *) request_memory(ORBOX_SIZE);
			nr_call->call=OBX;
			nr_call->state=RUNNING;
			OBX->nr_call=nr_call;
			OBX->parent=ABX;
			OBX->eager_split=ES;
			ES=0;
			OBX->nr_all_alternatives=nr;

			{ register int i;
			  register struct status_or *p=NULL;
			  register Cell *a;
			    
			    if (nr>1) a=save_arguments(arg2);  else a=NULL;
			    pc+=3;
			    for(i=0;i<nr;i++) {
			      nr_alternative=(struct status_or *) request_memory(STATUS_OR_SIZE);
			      if (i==0) OBX->alternatives=nr_alternative;  else  p->next=nr_alternative; 
			      nr_alternative->previous=p;
			      p=nr_alternative;
			      nr_alternative->alternative=NULL;
			      nr_alternative->code=pc;
			      nr_alternative->state=READY;
			      nr_alternative->args=a;
			      pc+=5;
			    }
			    nr_alternative->next=NULL;
			}
			}
			nr_alternative=OBX->alternatives;
			/* goto next_alternative; */
                        pc=nr_alternative->code;
			goto try_me;
			execute_next();

		/* explore_alternative */
	        trust_me:
			get_arguments(arg2,nr_alternative->args);
			remove_memory_arguments(nr_alternative->args);
			goto try_me;
		retry_me:
			get_arguments(arg2,nr_alternative->args);
	        try_me:
			nr_alternative->args=NULL;
#if Debug
break_debug();
		        printf("Create AND_BOX for the %dth clause of predicate %s/%d (Yvars=%d) \n",(int) arg4,((struct Clauses *)arg1)->predi->name,(int) arg2,(int) arg3);
#endif
			if (OBX->nr_all_alternatives>1 || OBX->parent->parent==NULL) {

			  USE_SAME_ANDBOX=NULL;
			  ABX=(struct AND_BOX *)request_memory(ANDBOX_SIZE);
			  nr_alternative->alternative=ABX;
			  nr_alternative->state=RUNNING;

			  ABX->nr_alternative=nr_alternative;
			  ABX->level=OBX->parent->level+1;
			  ABX->parent=OBX;
			  ABX->externals=NULL;
			  ABX->suspended=NULL;
			  ABX->perms=NULL;
			  ABX->calls=NULL;
			  ABX->nr_all_calls=0;
			  ABX->side_effects=((struct Clauses *)arg1)->side_effects;
			  /* continue on middle of only_1_clause code */
			} else {
			  nr_call=OBX->nr_call;
			  ABX=OBX->parent;
			  del_orbox_and_sons(OBX);
			  nr_call->call=NULL;
			  /* continue to only 1 clause */

	        only_1_clause:
#if Debug
		          printf("Only 1 Clause -> Use the same AND_BOX for the %dth clause of predicate %s/%d (Yvars=%d) \n",(int) arg4,((struct Clauses *)arg1)->predi->name,(int) arg2,(int) arg3);
#endif

			  if (((struct Clauses *)arg1)->side_effects >= CUT) {
			    /* printf("Must create or-box still the same ?????\n"); MUST SEE THIS CASE */
			  }
			  USE_SAME_ANDBOX=nr_call;
			  nr_alternative=ABX->nr_alternative;
			  OBX=ABX->parent;
			}

			if (arg3) {
			  register int nr_locals;
			  nr_locals=arg3;
			  /* nr_locals=((struct Clauses *)arg1)->nr_vars; */
			  var_locals=request_memory_locals(nr_locals);
			  // add_to_list_locals(var_locals,ABX);
			} else { 
			  var_locals=NULL; 
			}
			pc=((struct Clauses *)arg1)->code;
			execute_next();

	        prepare_calls:
#if Debug
break_debug();
		        printf("prepare_calls %d\n",(int) arg1);
#endif
			if (USE_SAME_ANDBOX!=NULL) {  /* only one alternative */
			  register int nr;

			  nr=(int) arg1;
			  pc+=2;
			  if (nr) {
			    nr_call=USE_SAME_ANDBOX;
			    if (nr==1) {   /* ONLY ONE CALL , CHANGE DIRECTLY */
			      nr_call->call=NULL;
			      nr_call->code=pc+1;
			      nr_call->locals=var_locals; 
			      nr_call->state=READY;
			    } else {
			      struct status_and *calls,*first=NULL,*last=NULL;
			      int i,nr2;

			      nr2=ABX->nr_all_calls;
			      
			      for(i=0;i<nr;i++) {
				calls=(struct status_and *) request_memory(STATUS_AND_SIZE);
				if (first==NULL) first=calls;
				if (last!=NULL) last->next=calls;
				calls->previous=last;
			        calls->call=NULL;
			        calls->code=pc+1;
			        calls->locals=var_locals; 
			        calls->state=READY;
			        pc=(Cell *) *pc;
				last=calls;
			      }
			      
			      last->next=nr_call->next;
			      if (nr_call->next!=NULL) nr_call->next->previous=last;
			      first->previous=nr_call->previous;
			      if (nr_call->previous!=NULL) nr_call->previous->next=first;
			      else ABX->calls=first; /* nr_call era o primeiro */
				
		              free_memory((Cell *) nr_call,STATUS_AND_SIZE);
			      nr_call=first;
			      ABX->nr_all_calls=nr+nr2-1;
			    } 
			  } else {
			      nr_call->call=NULL;
			  }
			} else 
                          { /* there where more than one alternative */
			  register int nr;
			  nr=(int) arg1;
			  pc+=2;
			  ABX->nr_all_calls=nr;
			  if (nr) {
			    struct status_and *calls, *first=NULL, *last=NULL;
			    register int i;

			    for(i=0;i<nr;i++) {
			      calls=(struct status_and *) request_memory(STATUS_AND_SIZE);
			      if (first==NULL) first=calls;
			      if (last!=NULL) last->next=calls;
			      calls->previous=last;
			      calls->call=NULL;
			      calls->code=pc+1;
			      calls->locals=var_locals; 
			      calls->state=READY;
			      pc=(Cell *) *pc;
			      last=calls;
			    }
			    last->next=NULL;
			    ABX->calls=first;

			  } else ABX->calls=NULL;
			  nr_call=ABX->calls;
			}
			/* goto scheduler;*/

	        scheduler:
#if Debug
break_debug();
		        printf("Scheduler... \n");
#endif
#if Debug_Dump_State & 1
  		        dump_eam_state();
#endif
			/* Have to decide if I go up or continue on same level */
			/* If I go up the I have to suspend the and_box, 
			   else I can continue to the next clause (1st) of the and_box 
			   Another Alternative is to pick up a SUSPEND and_box       */
			/* for the meantime I Will always suspend unless there is a cut */

			if (ABX->externals==NULL || ABX->side_effects>=CUT) {
			  pc=nr_call->code;
			  execute_next();
			}
			nr_alternative->state=SUSPEND;
			/* goto suspend; */

	        suspend:
#if Debug
break_debug();
          	        printf("SUSPEND on alternative %p\n",nr_alternative);
#endif
			OBX=ABX->parent;
		        {   struct EXTERNAL_VAR *e;
			    struct PERM_VAR *v;
			    struct SUSPENSIONS_VAR *s;
		
			    ABX->suspended=addto_suspensions_list(ABX,NORMAL_SUSPENSION);
			    e=ABX->externals;
			    while(e!=NULL) {
			      v=e->var;
			      *((Cell *) v)=(Cell) v;
			      if (v->suspensions==NULL || v->suspensions->and_box!=ABX) {
				/* se a and_box ja esta na lista  nao adiciona */
 			         s=(struct SUSPENSIONS_VAR *) request_memory(SUSPENSIONS_VAR_SIZE);
			         s->and_box=ABX;
			         s->next=v->suspensions;
			         v->suspensions=s;
			      }
			      e=e->next;
			    }
			 }
			if (OBX->eager_split) goto split;

		        nr_alternative=nr_alternative->next;
			goto next_alternative;


		call_yap:
			/* Must create term to call */

		  if (!Yap_execute_goal(_X[1],0,CurrentModule)) goto success;
		  else goto fail;
		  
		  
		call:
{
                        struct Predicates *predi;
			
			predi=((PredEntry *) arg1)->beamTable;
			if (predi->idx==0) { /* predicado precisa de ser indexado  */
#if Debug
		        printf("Indexing pred %s/%d \n",predi->name,(int) predi->arity);
#endif
			   do_eam_indexing(predi);  /* gera indexing caso seja necessario */
			}
#if Debug
break_debug();
		        printf("call %s/%d \n",predi->name,(int) predi->arity);
#endif
			ES=predi->eager_split;
			
			/* CUIDADO : vou tentar libertar a memoria caso seja o ultimo call */
#if DIRECT_JUMP
			if ((void *) arg3==&&exit_eam) /* Estou no ultimo call deste predicado */
#else
 		        if (arg3==_exit_eam)  /* Estou no ultimo call deste predicado */
#endif
			  {
			    if (ABX->nr_all_calls==1) {
			      free_memory_locals(nr_call->locals);
			    } else {
			      struct status_and *calls;
			      calls=ABX->calls;
			      while(calls!=nr_call) {
				if (calls->locals==nr_call->locals) break;
				calls=calls->next;
			      }
			      if (calls==nr_call) {
				free_memory_locals(nr_call->locals);
			      }
			    }
			  }
			  nr_call->locals=NULL;

#if ENABLE_INDEX
                        if (predi->idx>0) { 
			  register Cell *_DR;
			  _DR=(Cell *) deref(_X[1]);
			  _X[1]=(Cell) _DR;

			  if (isvar((Cell *) _DR) ) {
#if Debug
			      printf("Caso X1=Var\n");
#endif
			      pc=predi->code;
			      execute_next();
			  }

			  if (isatom((Cell) _DR) ) {
                              int index,nr;
			      struct HASH_TABLE *t;
#if Debug
				printf("Caso X1=Atom\n");
#endif
				nr=predi->idx_atom;
				if (nr) {
				  index=index_of_hash_table_atom((Cell) _DR,nr);
				  t=predi->atom[index];
				  while(t) {
				    if ((Cell) t->value==(Cell) _DR) {
				      pc=t->code;
				      execute_next();
				    }
				    t=t->next;
				  }  
				} 
 			        if (predi->idx_var!=0) {
                                /* Not found on index but I still have code with var args */
				 pc=predi->vars;
				 execute_next();
				}
				goto fail;
			  }
			  if (ispair((Cell) _DR)) {
#if Debug
			      printf("Caso X1=Pair\n");
#endif
			      pc=predi->list;
			      execute_next();
			  }
			  if (isappl((Cell) _DR)) {
                              int index,nr;
			      struct HASH_TABLE *t;
#if Debug
			        printf("Caso X1=Functor\n");
#endif
			        _DR=(Cell *) *repappl((Cell *)_DR); 
			        nr=predi->idx_functor;
				if (nr) {
				  index=index_of_hash_table_appl((Cell) _DR,nr);
				  /* index=((int)_DR>>5) % nr; */
				  t=predi->functor[index];
				  while(t) {
				    if (t->value==(Cell) _DR) {
				      pc=t->code;
				      execute_next();
				    }
				    t=t->next;
				  }  
				} 
 			        if (predi->idx_var!=0) {
                                /* Not found on index but I still have code with var args */
				   pc=predi->vars;
				   execute_next();
				}
				goto fail;
			  }
		        }
#endif /* ENABLE_INDEX */
#if Debug
			if (predi->idx>0) printf("Caso X1=Var\n");
			else printf("Caso em que o predicado nao esta indexado\n");
#endif
			pc=predi->code;
			/*			goto prepare_tries; */
			execute_next();			
}


		safe_call:
#if Debug
break_debug();
		        printf("safe_call 0x%lX X1=%d (0x%lX) ,X2=%d (0x%lX) \n",(unsigned long) arg1,(int) _X[1],(unsigned long) _X[1],(int) _X[2],(unsigned long) _X[2]);
#endif
			_S=(Cell *) arg1;
			_S=(Cell *) (* ((int long  (*)(void)) _S))();
#if !Fast_go
			if (EAMError)
			  abort_eam("Cought one Safe Call Error..........?????\n");
#endif
			if (!_S) goto fail_body;
			
			/* we didn't get to created a or_box */
			nr_call=remove_call_from_andbox(nr_call,ABX);
 		        OBX=ABX->parent;
			goto next_call;

		safe_call_unary:
#if Debug
break_debug();
		        printf("safe_call_unary 0x%lX X1=%d (0x%lX) ,X2=%d (0x%lX) \n",(unsigned long) arg1,(int) _X[1],(unsigned long) _X[1],(int) _X[2],(unsigned long) _X[2]);
#endif
			_S=(Cell *) arg1;
			_S=(Cell *) (* ((int long  (*)(Term)) _S))(deref(_X[1]));
#if !Fast_go
			if (EAMError)
			  abort_eam("Cought one Safe Call Error..........?????\n");
#endif
			if (!_S) goto fail_body;
			
			/* we didn't get to created a or_box */
			nr_call=remove_call_from_andbox(nr_call,ABX);
 		        OBX=ABX->parent;
			goto next_call;

		safe_call_binary:
#if Debug
break_debug();
		        printf("safe_call_binary 0x%lX X1=%d (0x%lX) ,X2=%d (0x%lX) \n",(unsigned long) arg1,(int) _X[1],(unsigned long) _X[1],(int) _X[2],(unsigned long) _X[2]);
#endif
			_S=(Cell *) arg1;
			_S=(Cell *) (* ((int long  (*)(Term, Term)) _S))(deref(_X[1]),deref(_X[2]));
#if !Fast_go
			if (EAMError)
			  abort_eam("Cought one Safe Call Error..........?????\n");
#endif
			if (!_S) goto fail_body;
			
			/* we didn't get to created a or_box */
			nr_call=remove_call_from_andbox(nr_call,ABX);
 		        OBX=ABX->parent;
			goto next_call;


		direct_safe_call:
#if Debug
break_debug();
		        printf("direct_safe_call %p X1=%d,X2=%d \n",(void *) arg1,(int) _X[1],(int) _X[2]);
#endif
			Force_Wait=0;
			_S=(Cell *) arg1;
			_S=(Cell *) (* ((int long  (*)(void)) _S))();
			/* _S=(Cell *) (* ((int long  (*)(Term,Term)) _S))(_X[1],_X[2]); */
			if (!_S) goto fail_head;
			pc+=2;
			execute_next();

		direct_safe_call_unary:
#if Debug
break_debug();
		        printf("direct_safe_call_unary %p X1=%d,X2=%d \n",(void *) arg1,(int) _X[1],(int) _X[2]);
#endif
			Force_Wait=0;
			_S=(Cell *) arg1;
			_S=(Cell *) (* ((int long  (*)(Term)) _S))(deref(_X[1]));
			if (!_S) goto fail_head;
			pc+=2;
			execute_next();

		direct_safe_call_binary:
#if Debug
break_debug();
		        printf("direct_safe_call_binary %p X1=%d,X2=%d \n",(void *) arg1,(int) _X[1],(int) _X[2]);
#endif
			Force_Wait=0;
			_S=(Cell *) arg1;
			_S=(Cell *) (* ((int long  (*)(Term,Term)) _S))(deref(_X[1]),deref(_X[2]));
			if (!_S) goto fail_head;
			pc+=2;
			execute_next();

	        skip_while_var:
#if Debug
break_debug();
			    printf("Skip_while_var on call %p\n", nr_call);
#endif   
			 if (exists_var_in((Cell *) _X[1])) { 
			   ABX->suspended=addto_suspensions_list(ABX,VAR_SUSPENSION);
			   nr_call=nr_call->next;
			   goto next_call; 
			 }
			pc+=1;
			execute_next();

	        wait_while_var:
#if Debug
break_debug();
			    printf("Wait_while_var on call %p\n", nr_call);
#endif   
			 if (exists_var_in((Cell *) _X[1])) { 
			       ABX->suspended=addto_suspensions_list(ABX,VAR_SUSPENSION);
			       OBX=ABX->parent; 
                               nr_alternative=ABX->nr_alternative->next;
                               goto next_alternative; 
			 }
			 pc+=1;
			 execute_next();

	        force_wait:
#if Debug
break_debug();
			 printf("Force Waiting on call %p\n", nr_call);
#endif   
			 /* we didn't get to created a or_box */

 		         OBX=ABX->parent;
			 if (nr_call->previous!=NULL) {
			    nr_call->call=NULL;
			    nr_call->state=WAITING_TO_BE_FIRST;
			    ABX->suspended=addto_suspensions_list(ABX,WAIT_SUSPENSION);
			    nr_alternative=ABX->nr_alternative->next;
			    goto next_alternative;
			 } 
			 nr_call=remove_call_from_andbox(nr_call,ABX);
			 goto next_call;

	        write_call:
#if Debug
break_debug();
		         printf("write_call\n");
#endif
#if USE_LEFTMOST
			 if (!is_leftmost(ABX,nr_call)) {
  #if Debug
		           printf("Force Waiting Before write_call\n");
  #endif
			   nr_call->call=NULL;
			   nr_call->state=WAITING_TO_BE_LEFTMOST;
			   ABX->suspended=addto_suspensions_list(ABX,LEFTMOST_SUSPENSION);
			   goto top_tree;
			 }
#endif

#ifdef DEBUG
			 Yap_plwrite ((Term) _X[1], Yap_DebugPutc, 0);
#else
			 extern int beam_write (void);
			 beam_write();
#endif
			 nr_call=remove_call_from_andbox(nr_call,ABX);
			 ABX->side_effects=ABX->side_effects | WRITE;
		         OBX=ABX->parent;
			 goto next_call;

	        is_call:
#if Debug
break_debug();
		        printf("is_call\n");
#endif
			{
			  Cell *_DR;
			/* BEAM_is is declared on C/eval.c */
			  _DR=(Cell *) BEAM_is();
			  if (_DR==NULL) { /* erro no Eval */
			    top=NULL;
			    return (FALSE);
			  }
			  if (!Unify((Cell *) XREGS[1],_DR)) goto fail_body;
			}
			nr_call=remove_call_from_andbox(nr_call,ABX);
 		        OBX=ABX->parent;

			goto next_call;

	        equal_call:
#if Debug
break_debug();
		        printf("equal_call\n");
#endif
			nr_call=remove_call_from_andbox(nr_call,ABX);
			if (ABX->externals!=NULL) {
			    if (ABX->nr_all_calls==0) {
			         nr_alternative->state=SUSPEND_END;
			    } else nr_alternative->state=SUSPEND;
			    goto suspend;			    
			}

			goto next_call;


		pop:
#if Debug
break_debug();
		        printf("pop %d \n",(int) arg1);  
#endif
                        if (arg1>1) {
			  sp+=arg1>>2; 
			}			
			pop_mode_and_sreg();
#if Debug
                        if (_Mode==READ) printf("Continues in READ mode\n"); 
                        else  printf("Continues in WRITE mode\n"); 
#endif
			pc+=2;
			execute_next();

		do_nothing:
#if Debug
break_debug();
		        printf("do_nothing \n");
#endif
			pc++;
		        execute_next();


		get_var_X:
#if Debug
break_debug();
		        printf("get_var_X X%d=X%d \n",(int) arg2,(int) arg1);
    
#endif
			_X[arg2]=_X[arg1];
			pc+=3;
			execute_next();

		get_var_Y:
#if Debug
break_debug();
		        printf("get_var_Y Y%d=X%d \n",(int) arg2,(int) arg1);
#endif
			var_locals[arg2]=_X[arg1];
#if !Fast_go
			{ Cell *a;
			  a = (Cell *) deref(_X[arg1]);
			  if(isvar(a) && !isappl(a) && !is_perm_var(a))
			    abort_eam("Sério problema no get_var_Y\n");
  			    /* acho que vou ter de criar uma variavel local nova no nivel superior */
			}
#endif
			pc+=3;
			execute_next();

		get_val_X:
#if Debug
break_debug();
		        printf("get_val_X X%d,X%d \n",(int) arg1,(int) arg2);    
#endif
			{ register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(_X[arg1]);
			if (isvar((Cell) _DR)) { 
			        _DR1=(Cell *) deref(_X[arg2]);
				if (!isvar((Cell) _DR1)) { 
				    *(_DR)=(Cell) _DR1;
				    trail(ABX,(struct PERM_VAR *) _DR);
				} else {
				    UnifyCells(_DR,_DR1);
				}		
			} else {
			        _DR1=(Cell *) deref(_X[arg2]);
			        if (isvar((Cell) _DR1)) { 
				    *(_DR1)=(Cell) _DR;
				    trail(ABX,(struct PERM_VAR *) _DR1);
				} else {
				    if (!Unify(_DR1,_DR)) goto fail_head;
				}
			}
			}
			pc+=3;
		        execute_next();

		get_val_Y:
#if Debug
break_debug();
		        printf("get_val_Y X%d,Y%d \n",(int) arg1,(int) arg2);   
#endif
			{ register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(_X[arg1]);
			if (isvar((Cell) _DR)) {  
				 _DR1=(Cell *) deref(var_locals[arg2]);
				 if (!isvar((Cell) _DR1)) { 
				     *(_DR)=(Cell) _DR1;
				     trail(ABX,(struct PERM_VAR *) _DR);
				 } else {
				     UnifyCells(_DR,_DR1);
				 }		
		        } else {
			         _DR1=(Cell *) deref(var_locals[arg2]);
				 if (isvar((Cell) _DR1)) { 
				    *(_DR1)=(Cell) _DR;
				    trail(ABX,(struct PERM_VAR *) _DR1);
				 } else {
				    if (!Unify(_DR1,_DR)) goto fail_head;
				 }
			}
			}
			pc+=3;
		        execute_next();

		get_atom:
#if Debug
break_debug();
		        printf("get_atom X%d, 0x%lX\n",(int) arg1,(unsigned long) arg2);
#endif
			{ register Cell *_DR;
			_DR=(Cell *) deref(_X[arg1]);
			if (isvar((Cell) _DR)) {  
			      *(_DR)=arg2;
			      trail(ABX,(struct PERM_VAR *) _DR);
			} else {
			      if ((Cell) _DR!=arg2) goto fail_head; 
			}
			}
			pc+=3;
		        execute_next();

		get_list:
#if Debug
break_debug();
		        printf("get_list X%d\n",(int) arg1);
#endif
			{ register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(_X[arg1]);
			if (isvar((Cell) _DR)) { _Mode=WRITE;
		                 _S = _H;
		                 _H+= 2;
			         _DR1=(Cell *) abspair(_S);
				 *(_DR)=(Cell) _DR1;
				 trail(ABX,(struct PERM_VAR *) _DR);
				 pc+=2;
				 execute_next();
			} else {
			         if (!ispair((Cell) _DR)) goto fail_head;
				 _Mode=READ;
				 _DR1=_DR; /* SaveExpression in DR1*/
				 _S=(Cell *) reppair((Cell) _DR);
				 pc+=2;
				 execute_next();
			}
			}

		get_struct:
#if Debug
break_debug();
		        printf("get_struct X%d, 0x%lX/%d\n",(int) arg1,(unsigned long) arg2,(int) arg3);
    
#endif
			{ register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(_X[arg1]);
			if (isvar((Cell) _DR)) { _Mode=WRITE;
			          _DR1=(Cell *) absappl((Cell) _H); /* SaveExpression in _DR1*/
				  *(_DR)=(Cell) _DR1;
				  trail(ABX,(struct PERM_VAR *) _DR);
				  *(_H++)=arg2;
				  _S=_H;
				  _H+=arg3;  /* arg3 = arity */
				  pc+=4;
				  execute_next();
			} else {
			          if (!isappl((Cell) _DR)) goto fail_head;
				  _Mode=READ;
				  _S=(Cell *) repappl((Cell) _DR);
				  if (*_S!=arg2) goto fail_head;
				  _S++;
				  _DR1=_DR; /* SaveExpression in _DR1*/
				  pc+=4;
				  execute_next();
			}
			}

		unify_void:
#if Debug
break_debug();
		        printf("unify_void\n");
#endif
			if (_Mode==WRITE) {
			  *_S=(Cell) request_permVar(ABX);
			}
			_S++;
			pc+=1;
			execute_next();


		unify_local_Y:
#if Debug
break_debug();
		        printf("unify_local_Y Y%d \n",(int) arg1);
    
#endif
		     if (_Mode==READ) {
			 register Cell *_DR, *_DR1;
			_DR1=(Cell *) deref(var_locals[arg1]);
			if (isvar((Cell) _DR1)) {                 
			  _DR=(Cell *) deref((Cell) _S);
			  if (isvar((Cell) _DR)) {
			    UnifyCells(_DR1,_DR);  /* var , var */
			  } else {
			    *(_DR1)=(Cell) _DR;    /* var , nonvar */
			    trail(ABX,(struct PERM_VAR *) _DR1);
			  }
			}
			else {
			  _DR=(Cell *) deref((Cell) _S);
			  if (isvar((Cell) _DR)) {
			    *(_DR)=(Cell) _DR1;    /* nonvar, var */
			    trail(ABX,(struct PERM_VAR *) _DR);
			  } else {
			    if (!Unify(_DR,_DR1)) goto fail_head; /* nonvar, nonvar */
			  }			  
			}
			_S++;
			pc+=2;
			execute_next();
		      }  else {  /* write Mode */
			register Cell *_DR;
			_DR=(Cell *) deref(var_locals[arg1]);
			if (isvar((Cell) _DR) && !is_perm_var((Cell *) _DR)) {
			  *_S=(Cell) request_permVar(ABX);
			  UnifyCells(_DR,_S);
			} else {
			  *(_S)=(Cell) _DR;
			}
			_S++;
			pc+=2;
			execute_next();
		      }

		unify_local_X:
#if Debug
break_debug();
		        printf("unify_local_X X%d \n",(int) arg1);
#endif
		     if (_Mode==READ) {			
			 register Cell *_DR, *_DR1;
			_DR1=(Cell *) deref(_X[arg1]);
			if (isvar((Cell) _DR1)) {                 
			  _DR=(Cell *) deref((Cell) _S);
			  if (isvar((Cell) _DR)) {
			    UnifyCells(_DR1,_DR);  /* var , var */
			  } else {
			    *(_DR1)=(Cell) _DR;    /* var , nonvar */
			    trail(ABX,(struct PERM_VAR *) _DR1);
			  }
			}
			else {
			  _DR=(Cell *) deref((Cell) _S);
			  if (isvar((Cell) _DR)) {
			    *(_DR)=(Cell) _DR1;    /* nonvar, var */
			    trail(ABX,(struct PERM_VAR *) _DR);
			  } else {
			    if (!Unify(_DR,_DR1)) goto fail_head; /* nonvar, nonvar */
			  }			  
			}
			_S++;
			pc+=2;
			execute_next();
		     } else {  /* write mode */
			register Cell *_DR;
			_DR=(Cell *) deref(_X[arg1]);

			if (isvar((Cell) _DR) && !is_perm_var((Cell *) _DR)) {
			  *_S=(Cell) request_permVar(ABX);
			  UnifyCells(_DR,_S);
			} else {
			  *(_S)=(Cell) _DR;
			}
			_S++;
			pc+=2;
			execute_next();
		     }

		unify_val_Y:
#if Debug
break_debug();
		        printf("unify_val_Y Y%d \n",(int) arg1);
    
#endif
		     if (_Mode==READ) {			
			register Cell *_DR, *_DR1;
			_DR1=(Cell *) deref(var_locals[arg1]);
			if (isvar((Cell) _DR1)) {
			  _DR=(Cell *) deref((Cell) _S);
			  if (isvar((Cell) _DR)) {
			    UnifyCells(_DR1,_DR);
			  } else {
			    *(_DR1)=(Cell) _DR;
			    trail(ABX,(struct PERM_VAR *) _DR1);
			  }
			}
			else {
			  _DR=(Cell *) deref((Cell) _S);
			  if (isvar((Cell) _DR)) {
			    *(_DR)=(Cell) _DR1;
			    trail(ABX,(struct PERM_VAR *) _DR);
			  } else {
			    if (!Unify(_DR,_DR1)) goto fail_head;
			  }			  
			}
			_S++;
			pc+=2;
			execute_next();
		     } else { /* write mode */
 		        *(_S)=var_locals[arg1];
			_S++;
			pc+=2;
			execute_next();
		     }


		unify_val_X:
#if Debug
break_debug();
		        printf("unify_val_X X%d \n",(int) arg1);
#endif
		     if (_Mode==READ) {			
			 register Cell *_DR, *_DR1;
			_DR1=(Cell *) deref((Cell) _X[arg1]);
			if (isvar((Cell) _DR1)) {
			  _DR=(Cell *) deref((Cell) _S);
			  if (isvar((Cell) _DR)) {
			    UnifyCells(_DR1,_DR);
			  } else {
			    *(_DR1)=(Cell) _DR;
			    trail(ABX,(struct PERM_VAR *) _DR1);
			  }
			}
			else {
			  _DR=(Cell *) deref((Cell) _S);
			  if (isvar((Cell) _DR)) {
			    *(_DR)=(Cell) _DR1;
			    trail(ABX,(struct PERM_VAR *) _DR);
			  } else {
			    if (!Unify(_DR,_DR1)) goto fail_head;
			  }			  
			}
			_S++;
			pc+=2;
			execute_next();
		     } else {
			*(_S)=_X[arg1];
			_S++;
			pc+=2;
			execute_next();
		     }

		unify_var_X:
#if Debug
break_debug();
		        printf("unify_var_X X%d=*S \n",(int) arg1);
#endif
		     if (_Mode==READ) {			
		        _X[arg1]=*(_S++);
			pc+=2;
			execute_next();
		     } else {
			*_S=(Cell) request_permVar(ABX);
			_X[arg1]=(Cell) _S;
			_S++;
			pc+=2;
			execute_next();
		     }

		unify_var_Y:
#if Debug
break_debug();
		        printf("unify_var_Y Y%d \n",(int) arg1);
#endif
		     if (_Mode==READ) {
			var_locals[arg1]=*(_S++);
			pc+=2;
			execute_next();
		     } else {
			*_S=(Cell )request_permVar(ABX);
			var_locals[arg1]=*_S;
			_S++;
			pc+=2;
			execute_next();
		      }

		unify_last_atom:
		unify_atom:
#if Debug
break_debug();
		        printf("unify_atom 0x%lX \n",(unsigned long) arg1);
#endif
		     if (_Mode==READ) {
			 register Cell *_DR;
			_DR=(Cell *) deref((Cell) _S);
			if (isvar((Cell) _DR)) {
			  *(_DR)=arg1;
			  trail(ABX,(struct PERM_VAR *) _DR);
			} else {
			  if ((Cell) _DR!=arg1)  goto fail_head;
			}
			_S++; 
			pc+=2;
			execute_next();
		     } else {
			*(_S)=arg1;
			_S++;
			pc+=2;
			execute_next();
		     }

		unify_list:
#if Debug
break_debug();
		        printf("unify_list \n");
#endif
		     if (_Mode==READ) {
			 register Cell *_DR, *_DR1;
			_DR=(Cell *) deref(*_S);
			if (isvar((Cell) _DR)) { 
                              _DR1=(Cell *) abspair((Cell) _H);  /* SavedExpression  in _DR1 */
			      *(_DR)=(Cell) _DR1;
			      trail(ABX,(struct PERM_VAR *) _DR);
			      _S++;
			      push_mode_and_sreg();
			      _Mode=WRITE;  /* goes int write mode */
			      _S=_H;
			      _H+=2;
			      pc+=1;
			      execute_next();
			} else {
			      if (!ispair((Cell) _DR)) goto fail_head;
			      _S++;
			      push_mode_and_sreg();
			      _S=(Cell *) reppair((Cell) _DR);
			      _DR1=_DR;  /* SavedExpression in _DR1 */ 
			      pc+=1;
			      execute_next();
			}
		     } else {
			 register Cell *_DR1;
                        _DR1=(Cell *) abspair((Cell) _H);  /* SavedExpression  in _DR1 */
                        *(_S)=(Cell) _DR1;
			_S++;
			push_mode_and_sreg();
			_S=_H;
		        _H+=2;
			pc+=1;
			execute_next();
		      }

		unify_last_list:
#if Debug
break_debug();
		        printf("unify_last_list \n");
#endif
		     if (_Mode==READ) {
			register Cell *_DR, *_DR1;
		        _DR=(Cell *) deref(*_S);
			if (isvar((Cell) _DR)) { _Mode=WRITE;  /* goes into write mode */
			         _DR1=(Cell *) abspair((Cell) _H);  /* SavedExpression  in _DR1 */
				 *(_DR)=(Cell) _DR1;
				 trail(ABX,(struct PERM_VAR *) _DR);
				 _S=_H;
				 _H+=2;
				 pc+=1;
				 execute_next();
	                } else {
			         if (!ispair((Cell) _DR)) goto fail_head;
				 _S=(Cell *) reppair((Cell) _DR);
				 _DR1=_DR;  /* SavedExpression  in _DR1 */
				 pc+=1;
				 execute_next();
			}
		     } else {
			 register Cell *_DR1;
			_DR1=(Cell *) abspair((Cell) _H);  /* SavedExpression  in _DR1 */
			*(_S)=(Cell) _DR1;
			_S=_H;
			_H+=2;
			pc+=1;
			execute_next();
		     }

		unify_struct:
#if Debug
break_debug();
		        printf("unify_struct 0x%lX,%d \n",(unsigned long) arg1,(int) arg2);
#endif
		     if (_Mode==READ) {
			 register Cell *_DR, *_DR1;
		        _DR=(Cell *) deref(*_S);
			if (isvar((Cell) _DR)) { 
			           _DR1=(Cell *) absappl((Cell) _H); /* SaveExpression in _DR1*/
				   *(_DR)=(Cell) _DR1;
				   trail(ABX,(struct PERM_VAR *) _DR);
				   _S++;
				   push_mode_and_sreg();
				   _Mode=WRITE;  /* goes into write mode */
				   *(_H++)=arg1;
				   _S=_H;
				   _H+=arg2;
				   pc+=3;
				   execute_next();
			} else {
			          if (!isappl((Cell) _DR)) goto fail_head;
				  _DR1=(Cell *) repappl((Cell) _DR);
				  if (*_DR1!=arg1) goto fail_head;
				  ++_S;
				  push_mode_and_sreg();
				  _S=++_DR1;
				  _DR1=_DR; /* SaveExpression in _DR1*/
				  pc+=3;
				  execute_next();
			}
		     } else {
			register Cell *_DR1;
			_DR1=(Cell *) absappl((Cell) _H); /* SaveExpression in _DR1*/
			*(_S)=(Cell) _DR1;
			_S++;
			push_mode_and_sreg();
			*(_H++)=arg1;
		        _S=_H;
			_H+=arg2;
			pc+=3;
			execute_next();
		     }

		unify_last_struct:
#if Debug
break_debug();
		        printf("unify_last_struct 0x%lX, %d \n",(unsigned long) arg1,(int) arg2);
#endif
		     if (_Mode==READ) {
			 register Cell *_DR, *_DR1;
		        _DR=(Cell *) deref(*_S);
			if (isvar((Cell) _DR)) { _Mode=WRITE;  /* goes into write mode */
			           _DR1=(Cell *) absappl((Cell) _H); /* SaveExpression in _DR1*/
				   *(_DR)=(Cell) _DR1;
				   trail(ABX,(struct PERM_VAR *) _DR);
				   *(_H++)=arg1;
				   _S=_H;
				   _H+=arg2;
				   pc+=3;
				   execute_next();
			} else {
			          if (!isappl((Cell) _DR)) goto fail_head;
				  _DR1=(Cell *) repappl((Cell) _DR);
				  if (*_DR1!=arg1) goto fail_head;
				  _S=++_DR1;
				  _DR1=_DR; /* SaveExpression in _DR1*/
				  pc+=3;
				  execute_next();
			}
		     } else {
			 register Cell *_DR1;
			_DR1=(Cell *) absappl((Cell) _H); /* SaveExpression in _DR1*/
			*(_S)=(Cell) _DR1;
			*(_H++)=arg1;
		        _S=_H;
			_H+=arg2; 
			pc+=3;
			execute_next();
		     }

		put_var_X:
#if Debug
break_debug();
		        printf("put_var_X X%d,X%d \n",(int) arg1,(int) arg2);
#endif
			_X[arg1]=(Cell) _H;
			_X[arg2]=(Cell) _H;
			*(_H)=(Cell) _H;
			_H++;
			pc+=3;
			execute_next();
			


		put_val_X:
#if Debug
break_debug();
		        printf("put_val_X X%d,X%d \n",(int) arg1,(int) arg2);
#endif
			_X[arg1]=_X[arg2];
			pc+=3;
			execute_next();


		put_var_P:
#if Debug
break_debug();
		        printf("put_var_P X%d,Y%d \n",(int) arg1,(int) arg2);
#endif
			if (isvar(var_locals[arg2]) && !is_perm_var((Cell *) var_locals[arg2])) 
			   var_locals[arg2]=(Cell) request_permVar(ABX);
			_X[arg1]=var_locals[arg2];
			pc+=3;
			execute_next();
 
		put_var_Y:
			/*
#if Debug
break_debug();
		        printf("put_var_Y X%d,Y%d \n",(int) arg1,(int) arg2);
    
#endif
                        { register Cell *a;
			a = &(var_locals[arg2]);
			*a=(Cell) a;
			_X[arg1]=(Cell) a; }
			pc+=3;
			execute_next();
			*/
		put_val_Y:
#if Debug
break_debug();
		        printf("put_val_Y X%d,Y%d \n",(int) arg1,(int) arg2);
#endif
			_X[arg1]=var_locals[arg2];
			pc+=3;
			execute_next();

		put_unsafe:
#if Debug
break_debug();
		        printf("put_unsafe X%d, Y%d \n",(int) arg1,(int) arg2);
#endif
			_X[arg1]=var_locals[arg2]; 
			pc+=3;
			execute_next();


		put_atom:
#if Debug
break_debug();
		        printf("put_atom X%d, 0x%lX \n",(int) arg1,(unsigned long) arg2);
#endif
			_X[arg1]=arg2;
			pc+=3;
			execute_next();

		put_list:
#if Debug
break_debug();
		        printf("put_list X%d \n",(int) arg1);
#endif
			{ register Cell *_DR1;

                        _DR1=(Cell *) abspair((Cell) _H); /* SaveExpression in _DR1*/
			_X[arg1]=(Cell) _DR1;
			_S=_H;
			_H+=2;
			pc+=2;
			execute_next();
			}

		put_struct:
#if Debug
break_debug();
		        printf("put_struct X%d, 0x%lX, %d \n",(int) arg1,(unsigned long) arg2,(int) arg3);
#endif
			{ register Cell _DR1;

                        (Cell) _DR1=absappl((Cell) _H); /* SaveExpression in _DR1*/
			_X[arg1]=(Cell) _DR1;
			*(_H++)=arg2;
			_S=_H;
			_H+=arg3;
			pc+=4;
			execute_next();
			}

		write_var_X:
#if Debug
break_debug();
		        printf("write_var_X X%d \n",(int) arg1);
#endif
			*_S=(Cell) request_permVar(ABX);
			_X[arg1]=(Cell) _S;
			_S++;
			pc+=2;
			execute_next();

		write_var_Y:
#if Debug
break_debug();
		        printf("write_var_Y Y%d \n",(int) arg1);
#endif 
			{ Cell *c;
			c=&var_locals[arg1];
			*c=(Cell) c;
			*_S=(Cell) c;
			}
			_S++;
			pc+=2;
			execute_next();


		write_var_P:
#if Debug
break_debug();
		        printf("write_var_P Y%d \n",(int) arg1);
#endif 
			if (isvar(var_locals[arg1]) && !is_perm_var((Cell *) var_locals[arg1])) 
                           var_locals[arg1]=(Cell) request_permVar(ABX);
			*(_S)=var_locals[arg1];
			_S++;
			pc+=2;
			execute_next();


	        write_local_X:
		write_val_X:
#if Debug
break_debug();
		        printf("write_val_X X%d  (or write_local)\n",(int) arg1);
#endif
			*(_S)=_X[arg1];
			_S++;
			pc+=2;
			execute_next();

	        write_local_Y:
		write_val_Y:
#if Debug
		        printf("write_val_Y Y%d (or write_local)\n",(int) arg1);  
#endif
			*(_S)=var_locals[arg1];
			_S++;
			pc+=2;
			execute_next();

	        write_void:
#if Debug
break_debug();
		        printf("write_void \n");  
#endif
			*_S=(Cell) request_permVar(ABX);
			_S++;
			pc+=1;
			execute_next();
		write_atom:	
#if Debug
break_debug();
		        printf("write_atom 0x%lX \n",(unsigned long) arg1);
#endif
			*(_S)=arg1;
			_S++;
			pc+=2;
			execute_next();


		write_list:
#if Debug
break_debug();
		        printf("write_list \n");
#endif
			{ register Cell *_DR1;

                        _DR1=(Cell *) abspair((Cell) _H); /* SaveExpression in _DR1*/
			*(_S++)=(Cell) _DR1; 
			push_mode_and_sreg();
			_S=_H;
			_H+=2;
			pc+=1;
			execute_next();
			}

		write_last_list:
#if Debug
break_debug();
		        printf("write_last_list \n");
#endif
			{ register Cell *_DR1;

                        _DR1=(Cell *) abspair((Cell) _H); /* SaveExpression in _DR1*/
			*(_S)=(Cell) _DR1;
			_S=_H;
			_H+=2;
			pc+=1;
			execute_next();
			}

		write_struct:
#if Debug
break_debug();
		        printf("write_struct 0x%lX, %d \n",(unsigned long) arg1,(int) arg2);
#endif
			{ register Cell *_DR1;

                        _DR1=(Cell *) absappl((Cell) _H); /* SaveExpression in _DR1*/
			*(_S++)=(Cell) _DR1; 
			push_mode_and_sreg();
			*(_H++)=arg1;
			_S=_H;
			_H+=arg2;
			pc+=3;
			execute_next();
			}

		write_last_struct:
#if Debug
break_debug();
		        printf("write_last_struct 0x%lX, %d \n",(unsigned long) arg1,(int) arg2);
#endif
			{ register Cell *_DR1;
			_DR1=(Cell *) absappl((Cell) _H); /* SaveExpression in _DR1*/
			*(_S)=(Cell) _DR1;  
			*(_H++)=arg1;
			_S=_H;
			_H+=arg2;
			pc+=3;
			execute_next();
			}

		cut: 
#if Debug
break_debug();
		        printf("cut na alternativa %pª de %d \n",ABX->nr_alternative, ABX->parent->nr_all_alternatives);
#endif
			OBX=ABX->parent;
			{
			  struct status_or *new;
			  if (!is_leftmost(ABX,nr_call)) {
#if Debug 
			    printf("Force Waiting Before Cut\n");
#endif
			    nr_call->call=NULL;
			    nr_call->state=WAITING_TO_BE_LEFTMOST;
			    ABX->suspended=addto_suspensions_list(ABX,LEFTMOST_SUSPENSION);
			    nr_call=nr_call->next;
			    goto next_call;
			  }
			    ABX->side_effects-=CUT;
			    nr_call=remove_call_from_andbox(nr_call,ABX);
#if Debug
			    printf("Executando o cut \n");
			    if (ABX->externals!=NULL && OBX->nr_all_alternatives>1) printf("cut com externals (noisy) \n");
			    if (ABX->externals!=NULL && OBX->nr_all_alternatives==1) printf("cut com externals (degenerate) \n");
#endif
                            nr_alternative=ABX->nr_alternative;
                            new=nr_alternative->next;
			    nr_alternative->next=NULL;
			    if (new!=NULL) {
  			       do{
			          struct status_or *old;
			          old=new;
			          new=new->next;
			          del_andbox_and_sons(old->alternative);
			          if (new==NULL) remove_memory_arguments(old->args);
                                  free_memory((Cell *) old,STATUS_OR_SIZE);
			          OBX->nr_all_alternatives--;
			       } while (new!=NULL);
			       if (OBX->nr_all_alternatives==1) {
				  nr_alternative=OBX->alternatives;
				  goto unique_alternative;
			       }
			    }
			    goto next_call;			  
			}

		commit:
#if Debug
break_debug();
		        printf("commit na alternativa %pª de %d \n",ABX->nr_alternative, ABX->parent->nr_all_alternatives);
#endif
			OBX=ABX->parent;
			{
			  struct status_or *new;
			  if (!is_leftmost(OBX->parent,OBX->nr_call)) {
#if Debug
			    printf("Force Waiting Before Commit\n");
#endif
			    nr_call->call=NULL;
			    nr_call->state=WAITING_TO_BE_LEFTMOST_PARENT;
			    ABX->suspended=addto_suspensions_list(ABX,LEFTMOST_SUSPENSION);
			    nr_call=nr_call->next;
			    goto next_call;
			  }
			    ABX->side_effects-=CUT;
			    nr_call=remove_call_from_andbox(nr_call,ABX);

#if Debug
			    printf("Executando o commit (apaga %d alternatives) \n",OBX->nr_all_alternatives-1);
			    if (ABX->externals!=NULL && OBX->nr_all_alternatives>1) printf("commit com externals (noisy) \n");
			    if (ABX->externals!=NULL && OBX->nr_all_alternatives==1) printf("commit com externals (degenerate) \n");
#endif

			    if (OBX->nr_all_alternatives>1) {
			      nr_alternative=ABX->nr_alternative;
			      OBX->nr_all_alternatives=1;
			      new=OBX->alternatives;
			      OBX->alternatives=nr_alternative; /* fica a ser a unica alternativa */
			      do {
			          struct status_or *old;
			          old=new;
			          new=new->next;
				  if (old!=nr_alternative) {
 			            del_andbox_and_sons(old->alternative);
			            if (new==NULL) remove_memory_arguments(old->args);
                                    free_memory((Cell *) old,STATUS_OR_SIZE);
				  }
			      } while (new!=NULL);
			      nr_alternative->next=NULL;
			      nr_alternative->previous=NULL;
			    }
			    goto unique_alternative;			  
			}

		jump:
#if Debug
break_debug();
		        printf("jump inst %ld\n",(long int) arg1);
#endif
		        pc=(Cell *) arg1; 
			execute_next();


	save_pair_Y:
#if Debug
break_debug();
		        printf("save_pair Y%ld\n",(long int) arg1);
#endif
		        abort_eam("save_exp no emulador ?????");
			--S;
			var_locals[arg1]=abspair(_S);
			++S;
			pc+=2;
			execute_next();

	save_appl_Y:
#if Debug
break_debug();
		        printf("save_appl Y%ld\n",(long int) arg1);
#endif
		        abort_eam("save_exp no emulador ?????");
			--S;
			var_locals[arg1]=absappl(_S);
			++S;
			pc+=2;
			execute_next();


	save_appl_X:
#if Debug
break_debug();
		        printf("save_appl X%ld\n",(long int) arg1);
#endif
		        abort_eam("save_exp no emulador ?????");
			--S;
			_X[arg1]=absappl(_S);
			++S;
			pc+=2;
			execute_next();

	save_pair_X:
#if Debug
break_debug();
		        printf("save_pair X%ld\n",(long int) arg1);
#endif
		        abort_eam("save_exp no emulador ?????");
			--S;
			_X[arg1]=abspair(_S);
			++S;
			pc+=2;
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
}

/* The Inst_am instruction is used in eamamasm.c */
#define Int long int 

Int inst_am(int n);
Int am_to_inst(Cell inst);

#if DIRECT_JUMP
     extern Int TABLE[];
#endif

Int inst_am(int n)
{
#if DIRECT_JUMP
     return TABLE[n];
#else
     return(n);
#endif
}

Int am_to_inst(Cell inst)
{
#if DIRECT_JUMP
int n;
  for(n=0;n<=_p_functor; n++) if (TABLE[n]==inst) return (n);
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
  if (SU!=NULL) {
       struct SUSPENSIONS *s,*l;
       l=SU->prev;
       s=SU;
       do {
	 nr++;
	 if (s->prev!=l) abort_eam("Invalid list of Suspended boxes\b");
	 l=s;
	 s=s->next;
       } while(s!=SU);
  }
  printf("%d suspended boxes\n",nr);

  dump_eam_andbox(top,NULL, NULL);
}


void dump_eam_andbox(struct AND_BOX *a, struct OR_BOX *pai, struct status_or *pai2) {
  struct status_and *calls, *last;

  if (a==NULL) return;
  if (pai!=a->parent) abort_eam("Pai diferente do parent\n");
  if (pai2!=a->nr_alternative) abort_eam("Status call Pai diferente do nralternative\n");
  if (a==ABX) printf("->"); else printf("  ");
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
    } else dump_eam_orbox(calls->call,a,calls);
    last=calls;
    calls=calls->next;
  }
}

void dump_eam_orbox(struct OR_BOX *o, struct AND_BOX *pai, struct status_and *pai2) {
  struct status_or *i,*last;
  if (o==NULL) return;
  if (pai!=o->parent) abort_eam("Pai diferente do parent\n");
  if (pai2!=o->nr_call) abort_eam("Status call Pai diferente do nrcall\n");
  if (o==OBX) printf("=> "); else printf("   ");

  printf("%s>ORBOX with %d alternatives\n",SPACES(2*(o->parent->level)+1),o->nr_all_alternatives); 

  i=o->alternatives;
  last=NULL;
  while(i!=NULL) {
    if (i->previous!=last) abort_eam("link errado nas alternativas\n");
    if (i->alternative==NULL) { 
      printf("   %s+ANDBOX EMPTY\n",SPACES(2*(o->parent->level+1))); 
    } else dump_eam_andbox(i->alternative,o, i);
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
