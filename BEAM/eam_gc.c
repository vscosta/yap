/*************************************************************************
*									 *
*	       BEAM -> Basic Extended Andorra Model                      *
*         BEAM extends the YAP Prolog system to support the EAM          *
*									 *
* Copyright: Ricardo Lopes and NCC - University of Porto, Portugal       *
*									 *
**************************************************************************
* comments:	garbage collector routines   		                 *
*************************************************************************/


void garbage_collector(void);
struct OR_BOX *move_orbox(struct OR_BOX *o,struct AND_BOX *parent,struct status_and *nr_call);
struct AND_BOX *move_andbox(struct AND_BOX *a,struct OR_BOX *parent,struct status_or *alt);
Cell refresh_structures(Cell c);
Cell move_structures(Cell c);
void refresh_andbox(struct AND_BOX *a);
void refresh_orbox(struct OR_BOX *o);

Cell refresh_structures(Cell c) 
{
Cell *C, OldC;

  OldC=deref((Cell) c);

  if (isvar(OldC)) {
    return(OldC);
  }
  if (isatom(OldC)) { 
    return(OldC);
  }

  if (isappl(OldC)) {
    int i,arity;

    C=(Cell *) repappl(OldC);
    arity = ((int) ArityOfFunctor((Functor) *C));
    for(i=0;i<arity ;i++) {
      C++;
      *C=refresh_structures((Cell) C);
    }
    return(OldC);
  } 
  /* else if (ispair(c)) { */
     C=(Cell *) reppair(OldC);
     *C=refresh_structures((Cell) C);
     C++; 
     *C=refresh_structures((Cell) C);
     return(OldC);
}

Cell move_structures(Cell c) 
{
Cell *NewC, *NewH;
Cell OldC,OldH;
 
  OldC=deref((Cell) c);
  /*
  if (beam_MemGoing==1 && ((unsigned long) OldC) <beam_START_ADDR_HEAP+beam_MEM_H/2) return(OldC); 
  if (beam_MemGoing==-1 && ((unsigned long) OldC)>=beam_START_ADDR_HEAP+beam_MEM_H/2 && ((unsigned long) OldC) <beam_START_ADDR_BOXES) return(OldC); 
  */
  if (isvar(OldC)) {
    return(OldC);
  }
  if (isatom(OldC)) { 
    return(OldC);
  }

  OldH=(Cell) beam_H;
  NewH=beam_H;
  if (isappl(OldC)) {
    int i,arity;

    NewC=(Cell *) repappl(OldC);
    arity = ((int) ArityOfFunctor((Functor) *NewC));
    *NewH++=*NewC++;
    beam_H+=arity+1;
    for(i=0;i<arity ;i++) {
       *NewH=move_structures((Cell) NewC);
       NewH++;
       NewC++;
    }
    return(absappl(OldH));
  } 
  /* else if (ispair(c)) { */
     NewC=(Cell *) reppair(OldC);
     beam_H+=2;
     *NewH=move_structures((Cell) NewC);
     NewC++; 
     NewH++;
     *NewH=move_structures((Cell) NewC);
     return(abspair(OldH));
}



void garbage_collector()
{
#if GARBAGE_COLLECTOR==2 
struct AND_BOX  *new_top;
#endif

 if (beam_Mem_FULL & 2) beam_nr_gc_heap++; else beam_nr_gc_boxed++; 
#if Debug || Debug_GC 
 printf("Entering Garbage Collector for the %dth time (Reason=%d)\n",beam_nr_gc_heap+beam_nr_gc_boxed,beam_Mem_FULL); 
#endif
#if Debug_Dump_State & 2
 dump_eam_state();
 printf("--------------------------------------------------------------------\n");
#endif

 beam_Mem_FULL=0;

#if Memory_Stat
    if (beam_MemGoing==1) {
       beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][1]=(unsigned long) beam_H-beam_START_ADDR_HEAP;
       beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][2]=(unsigned long) beam_NextFree-beam_START_ADDR_BOXES;
    } else {
       beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][1]=(unsigned long) beam_H-beam_START_ADDR_HEAP-MEM_H/2;
       beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][2]=beam_END_BOX- ((unsigned long) beam_NextFree);
    }
    if (GARBAGE_COLLECTOR==1)
      beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][2]=beam_END_BOX- ((unsigned long) beam_NextFree);
#endif

#if GARBAGE_COLLECTOR==1 
   if (beam_MemGoing==1) {
      if (beam_H < (Cell *) (beam_START_ADDR_HEAP+MEM_H/2)) beam_H=(Cell *) (beam_START_ADDR_HEAP+MEM_H/2); else beam_H++;
      beam_MemGoing=-1;
      beam_sp=(Cell *) beam_START_ADDR_HEAP+MEM_H/2;
      beam_sp--;
   } else {
      beam_H=(Cell *) beam_START_ADDR_HEAP;
      beam_MemGoing=1;
      beam_sp=(Cell *) beam_END_H;
      beam_sp--;
   }
   refresh_andbox(beam_top);

 #if Clear_MEMORY 
   if (beam_MemGoing==-1) { 
     memset(beam_START_ADDR_HEAP,0,MEM_H/2);
   } else {
     memset(beam_START_ADDR_HEAP+MEM_H/2,0,MEM_H/2);
   }
 #endif

#else
   memset(beam_IndexFree,0,INDEX_SIZE*POINTER_SIZE);
   if (beam_MemGoing==1) {
      if (beam_H < (Cell *) (beam_START_ADDR_HEAP+MEM_H/2)) beam_H=(Cell *) (beam_START_ADDR_HEAP+MEM_H/2); else beam_H++;
      beam_NextFree=(Cell *) beam_END_BOX;
      beam_MemGoing=-1;
      beam_sp=(Cell *) beam_START_ADDR_HEAP+MEM_H/2; beam_sp-=2;
   } else {
      if (beam_H>=(Cell *) beam_START_ADDR_BOXES) beam_NextFree=beam_H+1; else beam_NextFree=(Cell *)  beam_START_ADDR_BOXES;
      beam_H=(Cell *)  beam_START_ADDR_HEAP;
      beam_MemGoing=1;
      beam_sp=(Cell *) beam_END_H; beam_sp-=2;
   }
   beam_Mem_FULL=0;

   beam_su=NULL;
   new_top=move_andbox(beam_top,NULL,NULL);
   beam_top=new_top;

 #if Clear_MEMORY 
   if (beam_MemGoing==-1) { 
     memset((void *) beam_START_ADDR_HEAP,0,MEM_H/2);
     memset((void *) beam_START_ADDR_BOXES,0,MEM_BOXES/2);
   } else {
     memset((void *) beam_START_ADDR_HEAP+MEM_H/2,0,MEM_H/2);
     memset((void *) beam_START_ADDR_BOXES+MEM_BOXES/2,0,MEM_BOXES/2);
   }
 #endif
#endif

#if Memory_Stat
    if (beam_MemGoing==1) {
       beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][3]=(unsigned long) beam_H- beam_START_ADDR_HEAP;
       beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][4]=(unsigned long) beam_NextFree- beam_START_ADDR_BOXES;
    } else {
       beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][3]=(unsigned long) beam_H- beam_START_ADDR_HEAP-MEM_H/2;
       beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][4]= beam_END_BOX- ((unsigned long) beam_NextFree);
    }
    if (GARBAGE_COLLECTOR==1)
      beam_Memory_STAT[beam_nr_gc_heap+beam_nr_gc_boxed][4]= beam_END_BOX- ((unsigned long) beam_NextFree);
#endif

#if Debug_Dump_State & 2
  		        dump_eam_state();
#endif
#if Debug
   printf("End of Garbage Collector\n");
#endif
}


#if GARBAGE_COLLECTOR!=1 


struct OR_BOX *move_orbox(struct OR_BOX *o,struct AND_BOX *parent,struct status_and *nr_call)
{
struct OR_BOX *new_orbox;
struct status_or *old, *new, *first=NULL, *last=NULL;
Cell *args,*newargs;

   if (o==NULL) return(NULL);
#if !Fast_go
   if ((Cell *) o<(Cell *) beam_START_ADDR_BOXES || (Cell *) o>(Cell *)  beam_END_BOX) return (NULL);
#endif   
   new_orbox=(struct OR_BOX *) request_memory(ORBOX_SIZE);
   if (beam_Mem_FULL) abort_eam("Sem Memoria para GC\n");
   if (beam_OBX==o) beam_OBX=new_orbox;
   new_orbox->parent=parent;
   new_orbox->nr_call=nr_call;
   new_orbox->nr_all_alternatives=o->nr_all_alternatives;

   old=o->alternatives;
   while(old!=NULL) {
     new=(struct status_or *) request_memory(STATUS_OR_SIZE);
     if (beam_Mem_FULL) abort_eam("Sem Memoria para GC\n");

     if (beam_nr_alternative==old) beam_nr_alternative=new;
     new->args=old->args; 
     new->code=old->code;
     new->state=old->state;
     new->alternative=move_andbox(old->alternative,new_orbox,new);

     if (first==NULL) first=new;
     else last->next=new;
     new->previous=last;
     new->next=NULL;
     last=new;
     old=old->next;
   }
   new_orbox->alternatives=first;

   args=NULL;
   newargs=NULL;
   while(last!=NULL) {
     if (last->args==NULL) {
           args=NULL;
           newargs=NULL;
     } else if (args!=last->args) {
       int y;
         args=last->args;
#if Debug
	 printf("Request args=%d \n",(int) args[0]);
#endif
	 newargs=(Cell *)request_memory((args[0])*sizeof(Cell));
         if (beam_Mem_FULL) abort_eam("Sem Memoria para GC\n");
	 newargs[0]=args[0];
	 for(y=1;y<args[0];y++) newargs[y]=move_structures(args[y]);
     }
     last->args=newargs;
     last=last->previous;
   }
   
return(new_orbox);
}

struct AND_BOX *move_andbox(struct AND_BOX *a,struct OR_BOX *parent, struct status_or *alt )
{
int OLD_VAR_TRAIL_NR; 
struct AND_BOX *new_andbox;
struct PERM_VAR *l;
struct EXTERNAL_VAR *old_externals,*externals;

  if (a==NULL) return(NULL);
  OLD_VAR_TRAIL_NR=beam_VAR_TRAIL_NR;

  new_andbox=(struct AND_BOX *) request_memory(ANDBOX_SIZE); 
  if (beam_Mem_FULL) abort_eam("Sem Memoria para GC\n");
  if (beam_ABX==a) beam_ABX=new_andbox;
  new_andbox->parent=parent;
  new_andbox->nr_alternative=alt;
  new_andbox->level=a->level;
  new_andbox->side_effects=a->side_effects;
  new_andbox->suspended=NULL;
  if (a->suspended) {
     new_andbox->suspended=addto_suspensions_list(new_andbox,a->suspended->reason);
  }
  new_andbox->perms=a->perms;
  l=a->perms;
  while(l!=NULL) { /* ainda nao estou a fazer GC nas Var Perm */
      l->value=move_structures(l->value);
      l->home=new_andbox;
      l->suspensions=NULL;
      l=l->next;
  }
  
  old_externals=a->externals;
  externals=NULL;
  while(old_externals) {
      struct EXTERNAL_VAR *e;
      struct SUSPENSIONS_VAR *s;

      e=(struct  EXTERNAL_VAR *) request_memory(EXTERNAL_VAR_SIZE);
 
      e->next=externals;
      externals=e;

      e->value=move_structures(old_externals->value);
      e->var=(struct PERM_VAR *) old_externals->var;
      //e->var=(struct PERM_VAR *) old_externals->var;  CUIDADO QUANDO FIZER GC PERM_VARS
      
      if (isvar(e->var)) {
	s=(struct SUSPENSIONS_VAR *) request_memory(SUSPENSIONS_VAR_SIZE); 
	s->and_box=new_andbox;
	s->next=e->var->suspensions;
	e->var->suspensions=s;
      }
      old_externals=old_externals->next;
   }
  new_andbox->externals=externals;
  if (beam_Mem_FULL) abort_eam("Sem Memoria para GC\n");


 /* CUIDADO: Preciso agora de duplicar os vectores das variaveis locais */     
  { struct status_and *calls;
#if !Fast_go
    Cell **backup=NULL; int i, counted=0,max=1000;
    backup=(Cell **) malloc(max);
#else
    Cell *backup[1000]; int i, counted=0;
#endif
 
   calls=a->calls;
   while(calls!=NULL) {
        if (calls->locals!=NULL) {
	  /* primeiro vou ver se já foi copiado */
	  for(i=0;i<counted;i+=2) {
	    if (backup[i]==calls->locals) {
	      calls->locals=backup[i+1];
	      break;
	    }
	  }
	  if (i==counted) { /* afinal ainda nao foi copiado: fazer copia em duas fases*/
	    Cell *c, *newvars, *oldvars; int nr;

	    oldvars=calls->locals;
	    nr=oldvars[-1];
       	    newvars=request_memory_locals(nr);

	    if (beam_varlocals==oldvars) beam_varlocals=newvars;

	    calls->locals=newvars;
	    /* primeiro actualizo as variaveis */
	    for(i=0;i<nr;i++) {
	      c=&oldvars[i];
              if ((Cell *)*c==c) {
	        newvars[i]=(Cell) &newvars[i];
	        *c=newvars[i];
  	      } else {
		newvars[i]= (Cell) *c;
	        *c=(Cell) &newvars[i];
	      }	      
	    }
	    /* depois copio as estruturas */
	    for(i=0;i<nr;i++) {
	      newvars[i]=move_structures(oldvars[i]);
	    }
#if !Fast_go
	    if (max<counted+2) { 
	       max+=200;  
	       backup=realloc(backup,max); 
	       if (backup==NULL) abort_eam("No more memory... realloc in gc \n");
	    } 
#else
	    if (counted>=998) abort_eam("No more memory... realloc in gc \n");
#endif
	    backup[counted]=oldvars;
	    backup[counted+1]=newvars;
	    counted+=2;
	  }
        }
	calls=calls->next;
   }
#if !Fast_go
   free(backup);
#endif
  }


  new_andbox->nr_all_calls=a->nr_all_calls;
  { struct status_and *first=NULL, *last=NULL,*calls,*calls_new;
    calls=a->calls;
    while(calls!=NULL){ 
       calls_new=(struct status_and *) request_memory(STATUS_AND_SIZE);
       if (beam_Mem_FULL) abort_eam("Sem Memoria para GC\n");
       calls_new->code=calls->code;
       calls_new->state=calls->state;     
       calls_new->locals=calls->locals;
       if (beam_nr_call==calls) beam_nr_call=calls_new;

       calls_new->call=move_orbox(calls->call,new_andbox,calls_new);  

       if (first==NULL) first=calls_new;
       else last->next=calls_new; 
       calls_new->previous=last;
       calls_new->next=NULL;
       last=calls_new;
       calls=calls->next;
    }
    new_andbox->calls=first;
  }

return(new_andbox);
}



#else  /* used by GC Only on Heap || Keep boxes on same memory */

void refresh_orbox(struct OR_BOX *o)
{
struct status_or *old, *last=NULL;
Cell *args;

   if (o==NULL) return; 

   old=o->alternatives;
   while(old!=NULL) {
     refresh_andbox(old->alternative);
     last=old;
     old=old->next;
   }

   args=NULL;
   while(last!=NULL) {
     if (last->args==NULL) {
	args=NULL;
     }else if (args!=last->args) {
       int y;
         args=last->args;
	 for(y=1;y<args[0];y++) {
           args[y]=move_structures(args[y]);
	 }
     }
     last=last->previous;
   }

return;
}

void refresh_andbox(struct AND_BOX *a)
{
struct PERM_VAR *l;
struct EXTERNAL_VAR *externals;
struct status_and *calls;

  if (a==NULL) return;
  
  l=a->perms;
  while(l!=NULL) {
      l->value=move_structures(l->value);
      l=l->next;
  }

  externals=a->externals;
  while(externals) {
      externals->value=move_structures(externals->value);
      externals=externals->next;
  }

  calls=a->calls;
  while(calls!=NULL) {
    //    if (calls->calls!=NULL) {
       if (calls->locals!=NULL && ((int) calls->locals[-1]>0) {
	 int nr,i;
	 nr=calls->locals[-1];
	 calls->locals[-1]=-nr;
	 for(i=0;i<nr;i++) {
             calls->locals[i]=move_structures(calls->locals[i]);
	 }
       }
       refresh_orbox(calls->call);  
   //  }
       calls=calls->next;
  }
  calls=a->calls;
  while(calls!=NULL) {
       if (calls->locals!=NULL && ((int) calls->locals[-1])<0) {
	   int nr;
   	   nr=calls->locals[-1];
	   calls->locals[-1]=-nr;
       }
       calls=calls->next;
  }

return;
}

#endif 


