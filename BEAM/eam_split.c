/*************************************************************************
*									 *
*	       BEAM -> Basic Extended Andorra Model                      *
*         BEAM extends the YAP Prolog system to support the EAM          *
*									 *
* Copyright: Ricardo Lopes and NCC - University of Porto, Portugal       *
*									 *
**************************************************************************
* comments:	split related functions                                  *
*************************************************************************/

void do_forking_andbox(struct AND_BOX *a);
Cell copy_structures(Cell c);
void replicate_local_variables(struct AND_BOX *a);
struct OR_BOX *copy_orbox(struct OR_BOX *o,struct AND_BOX *parent,struct status_and *nr_call);
struct AND_BOX *copy_andbox(struct AND_BOX *a,struct OR_BOX *parent);


void do_forking_andbox(struct AND_BOX *a)
{
struct OR_BOX *op,*opp, *new_orbox;
struct AND_BOX *ap, *new_andbox;
int nr_all_alternatives, nr_all_calls;
struct status_and *nr_call,*new_call;
struct status_or *nr_alternative, *alternatives, *new_alternatives;

  beam_nr_call_forking++;
  op=a->parent;     /* or box parent */
  ap=op->parent;    /* and box parent */
  opp=ap->parent;   /* or box parent parent */
  if (opp==NULL) {
    abort_eam("Forking with orbox parent parent NULL, maybe I'm on top ?????");
  }

  alternatives=opp->alternatives;
  nr_all_alternatives=opp->nr_all_alternatives;
  nr_alternative=ap->nr_alternative;
  nr_all_calls=ap->nr_all_calls;
  nr_call=op->nr_call;

  new_andbox=(struct AND_BOX *) request_memory(ANDBOX_SIZE); 
  new_orbox=(struct OR_BOX *) request_memory(ORBOX_SIZE);
  new_andbox->parent=opp;
  //  new_andbox->nr_alternative=nr_alternative;    /* seted after creating a new status_or */
  new_andbox->nr_all_calls=nr_all_calls;
  new_andbox->level=ap->level;
  new_andbox->perms=ap->perms;
  new_andbox->suspended=NULL;
  if (ap->suspended) new_andbox->suspended=addto_suspensions_list(new_andbox,ap->suspended->reason);
  new_andbox->side_effects=ap->side_effects;
 
  if (ap->externals) {
    struct EXTERNAL_VAR *old_externals, *list=NULL;
    old_externals=ap->externals;
    while (old_externals) {
      struct EXTERNAL_VAR *e;
      struct SUSPENSIONS_VAR *s;
      
      e=(struct  EXTERNAL_VAR *) request_memory(EXTERNAL_VAR_SIZE);
      e->value=old_externals->value;
      e->var=(struct PERM_VAR *) old_externals->var;
      e->next=list;
      list=e;

      if (isvar(e->var)) {
        s=(struct SUSPENSIONS_VAR *) request_memory(SUSPENSIONS_VAR_SIZE); /* Add and_box to suspension list of var*/
        s->and_box=new_andbox;
        s->next=e->var->suspensions;
        e->var->suspensions=s;
      }

      old_externals=old_externals->next;
    }
    new_andbox->externals=list;
  } else new_andbox->externals=NULL; 

  new_call=(struct status_and *) request_memory(STATUS_AND_SIZE);
  new_call->call=new_orbox;
  new_call->locals=nr_call->locals;
  new_call->code=nr_call->code;
  new_call->state=WAKE;
  nr_call->state=WAKE;     /* NEW PARA TORNAR A CALL NUM WAKE STATE   */

  new_orbox->parent=new_andbox;
  new_orbox->nr_call=new_call;
  new_orbox->nr_all_alternatives=1;
  new_alternatives=a->nr_alternative;
  new_orbox->alternatives=new_alternatives;

  /* remove  andbox from op */
  op->nr_all_alternatives-=1;
  if (new_alternatives->previous==NULL) op->alternatives=new_alternatives->next; 
  else new_alternatives->previous->next=new_alternatives->next;
  if (new_alternatives->next!=NULL) new_alternatives->next->previous=new_alternatives->previous;
  new_alternatives->next=NULL;
  new_alternatives->previous=NULL;

  a->parent=new_orbox;

  /* increase the nr_alternatives by 1 in opp or_box parent parent and conect new_andbox*/
  new_alternatives=(struct status_or *) request_memory(STATUS_OR_SIZE);
  new_andbox->nr_alternative=new_alternatives;   
  
  new_alternatives->next=nr_alternative;
  new_alternatives->previous=nr_alternative->previous;
  if (nr_alternative->previous==NULL) opp->alternatives=new_alternatives;
  else nr_alternative->previous->next=new_alternatives;
  nr_alternative->previous=new_alternatives;

  new_alternatives->args=nr_alternative->args;
  new_alternatives->code=nr_alternative->code;
  new_alternatives->state=nr_alternative->state;
  new_alternatives->alternative=new_andbox;

  opp->nr_all_alternatives=nr_all_alternatives+1;

  /* copy and_box ap to new_and-box  */
  { struct status_and *first=NULL, *last=NULL,*calls,*calls_new;
     calls=ap->calls;
     while(calls!=NULL) {
       if (calls==nr_call) {
	 calls_new=new_call;
       } else {
        calls_new=(struct status_and *) request_memory(STATUS_AND_SIZE);
	calls_new->code=calls->code;
	calls_new->locals=calls->locals;
	calls_new->state=calls->state;
	calls_new->call=copy_orbox(calls->call,new_andbox,calls_new);  /* Do a exact copy of the tree*/

       }
       if (first==NULL) first=calls_new;
       else  last->next=calls_new; 
       calls_new->previous=last;
       calls_new->next=NULL;
       last=calls_new;
       calls=calls->next;
     }
     new_andbox->calls=first;

  }

   
  /* remove and_box a from suspension list on vars */
  if (a->externals) {
    struct EXTERNAL_VAR *e;
    e=a->externals;
    while(e) {
      if (e->var->home->level>=a->parent->parent->level)
         remove_from_perm_var_suspensions(e->var,a);    
      e=e->next;
    }
  }

  /* Now we have to create new local vars and refresh the external vars to point to those */

  if (beam_MemGoing==1) { 
         beam_VAR_TRAIL=((Cell *) beam_START_ADDR_BOXES)-1;
  } else beam_VAR_TRAIL=(Cell *) beam_START_ADDR_HEAP;
  beam_VAR_TRAIL_NR=0;
  replicate_local_variables(new_andbox);
}


struct OR_BOX *copy_orbox(struct OR_BOX *o,struct AND_BOX *parent,struct status_and *nr_call)
{
struct OR_BOX *new_orbox;
struct status_or *old,*new,*first=NULL,*last=NULL;

   if (o==NULL) return(NULL);
   
   new_orbox=(struct OR_BOX *) request_memory(ORBOX_SIZE);
   new_orbox->parent=parent;
   new_orbox->nr_call=nr_call;
   new_orbox->nr_all_alternatives=o->nr_all_alternatives;
   old=o->alternatives;
   while(old!=NULL) {
     new=(struct status_or *) request_memory(STATUS_OR_SIZE);
     new->args=old->args;
     new->code=old->code;
     new->state=old->state;
     new->alternative=copy_andbox(old->alternative,new_orbox);
     if (new->alternative!=NULL) new->alternative->nr_alternative=new;

     if (first==NULL) first=new;
     else last->next=new;
     new->previous=last;
     new->next=NULL;
     last=new;			
     old=old->next;
   }
   new_orbox->alternatives=first;

return(new_orbox);
}

struct AND_BOX *copy_andbox(struct AND_BOX *a,struct OR_BOX *parent)
{
struct AND_BOX *new_andbox;

  if (a==NULL) return(NULL);

  new_andbox=(struct AND_BOX *) request_memory(ANDBOX_SIZE); 
  new_andbox->parent=parent;
  //  new_andbox->nr_alternative=a->nr_alternative;  /* this is seted in the copy_orbox, after calling copy_andbox */
  new_andbox->nr_all_calls=a->nr_all_calls;
  new_andbox->level=a->level;
  new_andbox->perms=a->perms;
  new_andbox->externals=a->externals;
  new_andbox->side_effects=a->side_effects;
  new_andbox->suspended=NULL;
  if (a->suspended) {
     new_andbox->suspended=addto_suspensions_list(new_andbox,a->suspended->reason);
  }

  { struct status_and *first=NULL, *last=NULL,*calls,*calls_new;
     calls=a->calls;
     while(calls!=NULL) {
        calls_new=(struct status_and *) request_memory(STATUS_AND_SIZE);
	calls_new->code=calls->code;
	calls_new->locals=calls->locals;
	calls_new->state=calls->state;
	calls_new->call=copy_orbox(calls->call,new_andbox,calls_new);  /* Do a exact copy of the tree*/

        if (first==NULL) first=calls_new;
        else  last->next=calls_new;
        calls_new->previous=last;
	calls_new->next=NULL;
        last=calls_new;
        calls=calls->next;
     }
     new_andbox->calls=first;
  }

return(new_andbox);
}


void replicate_local_variables(struct AND_BOX *a) /* used by fork -ABX is set*/
{
struct PERM_VAR *l,*new_list;
int i,OLD_VAR_TRAIL_NR;
struct EXTERNAL_VAR *old_externals,*externals;

if (a==NULL) return;

  OLD_VAR_TRAIL_NR=beam_VAR_TRAIL_NR; 
  l=a->perms;
  new_list=NULL;
  while(l) {
        struct PERM_VAR *new;
        Cell *c;
    
        new=request_permVar(a);
	new->yapvar=l->yapvar;
	new->next=new_list;
	new_list=new;

        c=&l->value;
        beam_VAR_TRAIL[beam_VAR_TRAIL_NR]=(Cell) c;
	beam_VAR_TRAIL_NR-=beam_MemGoing;
        beam_VAR_TRAIL[beam_VAR_TRAIL_NR]=(Cell) *c;
	beam_VAR_TRAIL_NR-=beam_MemGoing;

        if ((Cell *)*c==c) {
	     new->value=(Cell) &new->value;
	     *c=new->value;
	} else {
	     new->value= (Cell) *c;
	     *c=(Cell) &new->value;
	}
        l=l->next;
  }
  a->perms=new_list;
  l=new_list;
  while(l) {
	l->value=copy_structures(l->value);
        l=l->next;
  }

  /* At this point all old local vars are pointing to the new local vars */

  if (a==beam_ABX) {   /* Nao preciso de criar um novo vector das externals */
    old_externals=a->externals;
    while(old_externals) {
      if (old_externals->var->home->level>=beam_ABX->parent->parent->level) {
	  old_externals->value=copy_structures((Cell ) old_externals->value);
	  old_externals->var=(struct PERM_VAR *) old_externals->var->value;
          if (isvar(old_externals->var)) {
	    struct SUSPENSIONS_VAR *s;
	    s=(struct SUSPENSIONS_VAR *) request_memory(SUSPENSIONS_VAR_SIZE); /* Add and_box to suspension list of var*/
	    s->and_box=a;
	    s->next=old_externals->var->suspensions;
	    old_externals->var->suspensions=s;
	  }
      }
      old_externals=old_externals->next;
    }
 } else {

  old_externals=a->externals;
  externals=NULL;
  a->externals=NULL;

  while(old_externals) {
      struct EXTERNAL_VAR *e;
      struct SUSPENSIONS_VAR *s;

      e=(struct  EXTERNAL_VAR *) request_memory(EXTERNAL_VAR_SIZE);
      e->next=externals;
      externals=e;

      if (old_externals->var->home->level>=beam_ABX->parent->parent->level) {
	e->value=copy_structures((Cell ) old_externals->value);
	e->var=(struct PERM_VAR *) old_externals->var->value;
      } else {
	e->value=old_externals->value;
	e->var=(struct PERM_VAR *) old_externals->var->value;
      }
      
      if (isvar(e->var)) {
	s=(struct SUSPENSIONS_VAR *) request_memory(SUSPENSIONS_VAR_SIZE); /* Add and_box to suspension list of var*/
	s->and_box=a;
	s->next=e->var->suspensions;
	e->var->suspensions=s;
      }

      old_externals=old_externals->next;
   }
   a->externals=externals;
 }


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
       	    newvars=request_memory_locals_noinit(nr);
	    calls->locals=newvars;
	    /* primeiro actualizo as variaveis */
	    for(i=0;i<nr;i++) {
	      c=&oldvars[i];
              beam_VAR_TRAIL[beam_VAR_TRAIL_NR]=(Cell) c;
 	      beam_VAR_TRAIL_NR-=beam_MemGoing;
              beam_VAR_TRAIL[beam_VAR_TRAIL_NR]=(Cell) *c;
	      beam_VAR_TRAIL_NR-=beam_MemGoing;	    

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
	      newvars[i]=copy_structures(oldvars[i]);
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

  /* redo the process to the inner boxes */
  { struct status_and *calls;

   calls=a->calls;
   while(calls!=NULL) {
     
    if (calls->call!=NULL) {
      register struct OR_BOX *o;
      register struct status_or *nr;

      o=calls->call;
      nr=o->alternatives;
      while(nr!=NULL) {
	replicate_local_variables(nr->alternative); 
	nr=nr->next;
      }
    }
    calls=calls->next;
   }
  }

  if (beam_MemGoing==1) {
     for(i=OLD_VAR_TRAIL_NR;i>beam_VAR_TRAIL_NR;i-=2) {
        Cell *c;
        c=(Cell *) beam_VAR_TRAIL[i];
        *c=(Cell) beam_VAR_TRAIL[i-1];
     }
  } else {
     for(i=OLD_VAR_TRAIL_NR;i<beam_VAR_TRAIL_NR;i+=2) {
        Cell *c;
        c=(Cell *) beam_VAR_TRAIL[i];
        *c=(Cell) beam_VAR_TRAIL[i+1];
     }
  }

  beam_VAR_TRAIL_NR=OLD_VAR_TRAIL_NR;
}



Cell copy_structures(Cell c) 
{
Cell *NewC, *NewH;
Cell OldC,LOCAL_OldH;

  OldC=deref((Cell) c);

  if (isvar(OldC)) {
    return(OldC);
  }
  if (isatom(OldC)) { 
    return(OldC);
  }

  LOCAL_OldH=(Cell) beam_H;
  NewH=beam_H;
  if (isappl(OldC)) {
    int i,arity;

    NewC=(Cell *) repappl(OldC);
    arity = ((int) ArityOfFunctor((Functor) *NewC));
    *NewH++=*NewC++;
    beam_H+=arity+1;
    for(i=0;i<arity ;i++) {
       *NewH=copy_structures((Cell) NewC);
       NewH++;
       NewC++;
    }
    return(absappl(LOCAL_OldH));
  } 
  /* else if (ispair(c)) { */
     NewC=(Cell *) reppair(OldC);
     beam_H+=2;
     *NewH=copy_structures((Cell) NewC);
     NewC++; 
     NewH++;
     *NewH=copy_structures((Cell) NewC);
     return(abspair(LOCAL_OldH));
}
