/*************************************************************************
*									 *
*	       BEAM -> Basic Extended Andorra Model                      *
*         BEAM extends the YAP Prolog system to support the EAM          *
*									 *
* Copyright: Ricardo Lopes and NCC - University of Porto, Portugal       *
*									 *
**************************************************************************
* comments:	indexing related functions                               *
*************************************************************************/

#ifdef BEAM

#include "Yap.h"
#include "YapCompile.h"
#include "clause.h"
#include "eam.h"
#include <stdio.h>
#include <stdlib.h>

CInstr *StartCode,*inter_code;

extern void eam_showcode(Cell *code);
extern unsigned int index_of_hash_table_atom(Cell c, int nr);
extern unsigned int index_of_hash_table_appl(Cell c, int nr);

extern CInstr *emit_new(int o, int r1,CELL r4);
Cell *gera_codigo_try(struct Predicates *);
Cell *gera_codigo_try_list(struct Predicates *predi);
Cell *gera_codigo_try_only_vars(struct Predicates *predi);
struct HASH_TABLE **gera_codigo_try_atom(struct Predicates *predi);
struct HASH_TABLE **gera_codigo_try_functor(struct Predicates *predi);
extern Cell *eam_assemble(CInstr *code);
void do_eam_indexing(struct Predicates *p);
void ver_predicados(struct Predicates *p);
int exists_on_table(Cell a,struct HASH_TABLE **table, int i);

int exists_on_table(Cell a,struct HASH_TABLE **table, int i)
{
struct HASH_TABLE *t;

   t=table[i];

   while(t) {
     if (t->value==a) return(1);

     t=t->next;
   }

return(0);
}

Cell *gera_codigo_try(struct Predicates *predi) /* gerar os try's para o  predicado i */
{
  struct Clauses *c;
  int nr=0;

  StartCode=NULL;
  inter_code=NULL;
  c=predi->first;

  emit_new(prepare_tries,predi->nr_alt,predi->arity);
  if (predi->nr_alt==1) {
      emit_new(only_1_clause_op,0,(unsigned long) c);
  } else if (predi->nr_alt>1) {
    while(c!=NULL) {
      if (nr+1==predi->nr_alt) emit_new(trust_me_op,nr,(unsigned long) c);
      else if (nr==0) emit_new(try_me_op,predi->nr_alt,(unsigned long) c);
           else emit_new(retry_me_op,nr,(unsigned long) c);

      c=c->next;
      nr++;
    }
  } else {
      emit_new(fail_op,0,0);
  }

  return(eam_assemble(StartCode));
}



Cell *gera_codigo_try_list(struct Predicates *predi) /* gerar os try's para o  predicado i */
{
  struct Clauses *c;
  int nr=0,nr_preds;

  StartCode=NULL;
  inter_code=NULL;
  nr_preds=predi->idx_list+predi->idx_var;
  c=predi->first;

  emit_new(prepare_tries,nr_preds,predi->arity);
  if (nr_preds>=1) {
    while(c!=NULL) {
      if (c->predi==predi && (c->idx==Lista || c->idx==Variavel)) {
 	 if (nr_preds==1) {
           emit_new(only_1_clause_op,0,(unsigned long) c);
           break;
	 }
	 if (nr+1==nr_preds) { emit_new(trust_me_op,nr,(unsigned long) c); break; }
	 else if (nr==0) emit_new(try_me_op,nr_preds,(unsigned long) c);
	      else emit_new(retry_me_op,nr,(unsigned long) c);
         nr++;
      }
      c=c->next;
    }
  } else {
      emit_new(fail_op,0,0);
  }

  return(eam_assemble(StartCode));
}



struct HASH_TABLE **gera_codigo_try_atom(struct Predicates *predi)
{
int j,nr_preds,nr_atoms;
struct HASH_TABLE **table;
struct HASH_TABLE *t;
struct Clauses *cla;

  nr_atoms=predi->idx_atom;
  nr_preds=nr_atoms+predi->idx_var;
  table=malloc(sizeof(struct HASH_TABLE *)*(nr_atoms+1));
  for (j=0;j<=nr_atoms;j++) table[j]=NULL;

  cla=predi->first;
  while(cla) {
   if (cla->idx==Constante) {
     Cell a;
     unsigned int index;
     int nr;

     a=cla->val;
     if (a && nr_atoms) {
       index=index_of_hash_table_atom(a,nr_atoms);
     } else index=nr_atoms;

     /*     printf("nr_atoms=%d index=%d  -> 0x%X \n",nr_atoms,index,a);  */

     if (!exists_on_table(a,table,index)) {
       CInstr *first,*last=NULL,*prepare;
       struct Clauses *cla2;

       /* printf("a gerar codigo para atom index=%d value %ld\n",index,cla->val); */
       t=malloc(sizeof(struct HASH_TABLE));
       t->next=table[index];
       table[index]=t;
       t->value=a;

       StartCode=NULL;
       inter_code=NULL;
       prepare=emit_new(prepare_tries,0,predi->arity);
       cla2=predi->first;
       nr=0;
       first=NULL;
       while(cla2) {
           if ((cla2->idx==Constante && cla2->val==a) || cla2->idx==Variavel) {
               last=emit_new(retry_me_op,nr,(unsigned long) cla2);
	       if (first==NULL) first=last;
	       nr++;
	   }
	   cla2=cla2->next;
       }
       prepare->new1=nr;
       if (first==last) {
	 first->op=only_1_clause_op;
       } else {
	 first->op=try_me_op;
         last->op=trust_me_op;
       }
       t->code=eam_assemble(StartCode);
     }
   }
   cla=cla->next;
  }

return(table);
}

struct HASH_TABLE **gera_codigo_try_functor(struct Predicates *predi) /*gerar os try's para o predicado i*/
{
int j,nr_preds,nr_appls;
struct HASH_TABLE **table;
struct HASH_TABLE *t;
struct Clauses *cla;

  nr_appls=predi->idx_functor;
  nr_preds=nr_appls+predi->idx_var;
  table=malloc(sizeof(struct HASH_TABLE *)*(nr_appls+1));
  for (j=0;j<=nr_appls;j++) table[j]=NULL;

  cla=predi->first;
  while(cla) {
   if (cla->idx==Estrutura) {
    Cell a;
     long int index;
     int nr;

     a=cla->val;
     if (a && nr_appls) {
       index=index_of_hash_table_appl(a,nr_appls);
     } else index=nr_appls;

     if (!exists_on_table(a,table,index)) {
       CInstr *first,*last=NULL,*prepare;
       struct Clauses *cla2;

       /* printf("a gerar codigo para appl index=%d value %ld\n",index,cla->val); */
       t=malloc(sizeof(struct HASH_TABLE));
       t->next=table[index];
       table[index]=t;
       t->value=a;

       StartCode=NULL;
       inter_code=NULL;
       prepare=emit_new(prepare_tries,0,predi->arity);
       cla2=predi->first;
       nr=0;
       first=NULL;
       while(cla2) {
           if ((cla2->idx==Estrutura && cla2->val==a) || cla2->idx==Variavel) {
               last=emit_new(retry_me_op,nr,(unsigned long) cla2);
	       if (first==NULL) first=last;
	       nr++;
	   }
	   cla2=cla2->next;
       }
       prepare->new1=nr;
       if (first==last) {
	 first->op=only_1_clause_op;
       } else {
	 first->op=try_me_op;
         last->op=trust_me_op;
       }
       t->code=eam_assemble(StartCode);
     }
   }
   cla=cla->next;
  }

return(table);
}



Cell *gera_codigo_try_only_vars(struct Predicates *predi) /* gerar os try's de Vars para o predicado i */
{
  struct Clauses *c;
  int nr=0,nr_preds;

  StartCode=NULL;
  inter_code=NULL;
  nr_preds=predi->idx_var;
  c=predi->first;

  emit_new(prepare_tries,nr_preds,predi->arity);
  if (nr_preds>=1) {
    while(c!=NULL) {
      if (c->predi==predi && c->idx==Variavel) {
 	 if (nr_preds==1) {
           emit_new(only_1_clause_op,0,(unsigned long) c);
           break;
	 }
	 if (nr+1==nr_preds) { emit_new(trust_me_op,nr,(unsigned long) c); break; }
	 else if (nr==0) emit_new(try_me_op,nr_preds,(unsigned long) c);
	      else emit_new(retry_me_op,nr,(unsigned long) c);
         nr++;
      }
      c=c->next;
    }
  } else {
      emit_new(fail_op,0,0);
  }

  return(eam_assemble(StartCode));
}


void do_eam_indexing(struct Predicates *p)
{
   p->code=gera_codigo_try(p);
   p->idx=-1;

   if (p->arity && (p->idx_list || p->idx_atom || p->idx_functor)) {
	   p->vars=gera_codigo_try_only_vars(p);
	   p->list=gera_codigo_try_list(p);
	   p->functor=gera_codigo_try_functor(p);
	   p->atom=gera_codigo_try_atom(p);
	   p->idx=1;
   }

   if((Print_Code & 4) && (Print_Code & 8)) {
	   printf("General Case :\n");
	   eam_showcode(p->code);
   }

   if (Print_Code & 1) ver_predicados(p);
}


void ver_predicados(struct Predicates *p)
{
  struct Clauses *c; int i=0;

   printf("Predicado %s:%d  (ES=%d) tem %d clausulas do tipo V=%d L=%d A=%d F=%d \n",p->name,p->arity,p->eager_split,p->nr_alt,p->idx_var,p->idx_list,p->idx_atom,p->idx_functor);

   c=p->first;
   while(c!=NULL) {
     printf("Clausula %d do tipo %d (%d locals %d args) (val=0x%X)\n",++i,c->idx,c->nr_vars,c->predi->arity, (unsigned )c->val);
     c=c->next;
   }


}

#endif /* BEAM */
