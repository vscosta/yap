/*************************************************************************
*									 *
*	       BEAM -> Basic Extended Andorra Model                      *
*         BEAM extends the YAP Prolog system to support the EAM          *
*									 *
* Copyright: Ricardo Lopes and NCC - University of Porto, Portugal       *
*									 *
**************************************************************************
* comments:	eam code compiler 		                         *
*************************************************************************/

#ifdef BEAM

#include "eam.h" 
#include "eamamasm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int skip_while_var(void);
extern int wait_while_var(void);
extern int force_wait(void);
extern int p_write(void);
extern int p_is(void);
extern int p_halt(void);
extern int p_halt0(void);
extern int commit(void);
extern int eager_split(void);

extern void eam_showcode(Cell *);
extern Cell *eam_assemble(CInstr *);
extern void ShowCode_new2(int, int, CELL);
extern Cell *gera_codigo_try(int);
extern Cell *gera_codigo_try_list(int);
extern Cell *gera_codigo_try_only_vars(int);
extern struct HASH_TABLE **gera_codigo_try_atom(int);
extern struct HASH_TABLE **gera_codigo_try_functor(int);

/* Novas Definicoes */
compiler_struct *CGLOBS;
int labelno;
extern int nperm;
CInstr *inter_code,*StartCode;

void convert_Yaam(struct Clauses *);
void anota_predicados(struct Clauses *, PredEntry *,unsigned long ,int ,int ,int);
void verifica_predicados(struct Clauses *);
void ShowCode_new(int);
void codigo_eam(compiler_struct *);
void ver_predicados(void);
void eam_instructions(struct Clauses *);
void identify_calls(CInstr *);
int needs_box(Cell);
int is_skip(Cell);
void delay_prepare_calls(void);
int test_for_side_effects(void);
CInstr *insert_inst(CInstr *, int,int,CELL);
CInstr *emit_new(int, int, CELL);
CInstr *new_inst(int, int, CELL);
void *alloc_mem_temp(Cell);
void *alloc_mem(Cell);

/***********************************************************************\
*         Aqui estao as novas partes do compilador                      *
\***********************************************************************/

void anota_predicados(struct Clauses *clause, PredEntry *p, unsigned long a,int b,int info_type,int call)
{
struct Predicates *predi;

	if (p->beamTable==NULL) { /*1 vez que aparece, inicializar uma nova estrutura */
	    predi=(struct Predicates *) alloc_mem(sizeof(struct Predicates));
	    p->beamTable=predi;

	    predi->id=a;
	    predi->name=(char *) RepAtom(AtomOfTerm(MkAtomTerm((Atom) a)))->StrOfAE;
	    predi->arity=b;
	    predi->nr_alt=0;
	    predi->calls=0;
	    predi->idx_var=0;
	    predi->idx_list=0;
	    predi->idx_atom=0;
	    predi->idx_functor=0;
	    predi->first=NULL;
	    predi->last=NULL;

	} else predi=p->beamTable;

	if (!call) {  /* se nao foi chamado por um call, entao anota informacao */
	  predi->id=a; 
	  predi->nr_alt++; 
	  if (info_type & Variavel ) predi->idx_var++; /* info_type=Lista+Estrutura+Constante; */
	  if (info_type & Lista    ) predi->idx_list++;
	  if (info_type & Estrutura) predi->idx_functor++;
	  if (info_type & Constante) predi->idx_atom++;
	  if (predi->last==NULL) {
	      predi->first=clause;
	      predi->last=clause;
	      clause->next=NULL;
	  } else {
	      predi->last->next=clause;
	      predi->last=clause;
     	      clause->next=NULL;
	  }
	  
        }

return;
}

void identify_calls(CInstr *code) {
    PredEntry *p = RepPredProp((Prop) code->new4);
    Functor f = p->FunctorOfPred;
    int arity=p->ArityOfPE;
    char *name;
		
    if ( arity == 0) name=((AtomEntry *) f)->StrOfAE;
    else name=((AtomEntry *) NameOfFunctor(f))->StrOfAE;

    /*
    if (code->op==call_op) printf("call: ");
    else if (code->op==safe_call_op) printf("call: ");
    else if (code->op==execute_op) printf("execute: ");
    printf("->%s/%d...............\n",name,arity);
    */

    if (arity==0) {
      if (strcmp(name,"/")==0) { code->op=commit_op; return; }
      if (strcmp(name,":")==0) { code->op=force_wait_op; return; }
      if (strcmp(name,"nl")==0) { code->op=write_op; code->new1='\n'; return; }
      if (strcmp(name,"halt")==0) { code->op=exit_op; return; }

    } else if (arity==1) {
      if (strcmp(name,"wait_while_var")==0) { code->op=wait_while_var_op; return; }
      if (strcmp(name,"skip_while_var")==0) { code->op=skip_while_var_op; return; }
      if (strcmp(name,"write")==0) { code->op=write_op; return; }

    } else if (arity==2) {
      if (strcmp(name,"is")==0) { code->op=is_op; return; }
     } 

    /* não é nenhum call conhecido, deve ser um predicado em Prolog */

    return;
}

/* no verifica_predicados, vou transformar  os calls para */
void verifica_predicados(struct Clauses *clause)
{
  CELL Flags;

  inter_code=StartCode;
  anota_predicados(clause,(CGLOBS->cint).CurrentPred, StartCode->new4,StartCode->new1,clause->idx,0);
 
  while(inter_code!=NULL) {
    if (inter_code->op==safe_call_op) { /* new1 deve continuar igual */
	Flags = RepPredProp((Prop) (inter_code->new4))->PredFlags;
	if (Flags & AsmPredFlag) {
	   inter_code->op=std_base_op;
	   inter_code->new4=(Flags &0x7f);  
	} else {   
           PredEntry   *p=RepPredProp((Prop) inter_code->new4);
	   inter_code->op=safe_call_op;
  	   inter_code->new4= (unsigned long) p->cs.f_code;
	   if (Flags & BinaryPredFlag) inter_code->new1=2;
	   else inter_code->new1=0;
	}      
    }
    else if (inter_code->op==call_op || inter_code->op==execute_op) {
	             PredEntry *p = RepPredProp((Prop) inter_code->new4);
	             Flags = p->PredFlags;
	             Functor f = p->FunctorOfPred;
		
		     if (Flags & AsmPredFlag) {
		       int op;
		       switch (Flags & 0x7f) {
		       case _equal:
			 op = _p_equal;
			 break;
		       case _dif:
			 op = _p_dif;
			 break;
		       case _eq:
			 op = _p_eq;
			 break;
		       case _arg:
			 op = _p_arg;
			 break;
		       case _functor:
			 op = _p_functor;
			 break;
		       default:
			 printf("Internal eam assembler error for built-in %d\n",((int) (Flags & 0x7f)));
			 exit(1);
		       }
		     }

		     if (!(Flags & CPredFlag)) {
			if (p->ArityOfPE == 0) f = Yap_MkFunctor((Atom) f, 0);
			inter_code->new4=(unsigned long) p;
			anota_predicados(clause, p, (unsigned long) NameOfFunctor(f),ArityOfFunctor(f),0,1); 
			p->beamTable->calls++;

		     } else {/* safe_call */
		        inter_code->op=safe_call_op;
			inter_code->new4=  (unsigned long) p->cs.f_code;
			if (Flags & BinaryPredFlag) inter_code->new1=2;
			else inter_code->new1=0;
		     }
    }
    inter_code=inter_code->nextInst;
  }

return;
}


void ShowCode_new(int i)
{
  /*
struct intermediates c;
    c.CodeStart=StartCode;

    Yap_ShowCode(&c);
    return;
  */
#ifdef DEBUG

  switch(i) {
  case 1: printf("\nVer Predicados \n");
          break;
  case 2: printf("\nVer yaam Original\n");
          break;
  case 4: printf("\nVer abs machine code\n");
          break;
  case 8: printf("\nVer o codigo dos trys\n");
          break;
  case 16: printf("\nVer o codigo yaam ja transformado\n");
          break;
  case 32: printf("\nver codigo EAM com direct calls\n");
           break;
  case 128: printf("\nVer codigo EAM final\n");
           break;
  }

	inter_code = StartCode;
	while (inter_code) {
	  ShowCode_new2(inter_code->op, inter_code->new1,inter_code->new4);
	  inter_code = inter_code->nextInst;
	}
	printf("\n");
#endif
}


void codigo_eam(compiler_struct *cglobs)
{ 
struct Clauses *clause;

        CGLOBS=cglobs; 
	labelno=cglobs->labelno;

#ifdef DEBUG
        if (Print_Code & 2 ) Yap_ShowCode(&CGLOBS->cint);
#endif
        clause=(struct Clauses *) alloc_mem(sizeof(struct Clauses));
        convert_Yaam(clause);           /* convert into an internal struct code and check IDX */
        verifica_predicados(clause);    /* check predicates and convert calls */

	clause->predi=(CGLOBS->cint).CurrentPred->beamTable;
	(CGLOBS->cint).CurrentPred->beamTable->idx=0;  /* will need to go by indexing */

	if (Print_Code & 4) ShowCode_new(2);   /* codigo YAAM */

        /* transf os safe_calls em instrucoes eam e verifica se existem side_effects */
        clause->side_effects=test_for_side_effects();

        eam_instructions(clause);
	if (Print_Code & 16) ShowCode_new(16);   /* codigo EAM */
	inter_code=NULL;
	delay_prepare_calls();    /* transforma alguns safe_calls em direct_calls */

	if (Print_Code & 32) ShowCode_new(32);   /* codigo com direct_callss */
	clause->code=eam_assemble(StartCode);
        clause->nr_vars=nperm; 

        if (Print_Code & 128) eam_showcode((Cell *)clause->code); 

}




/********************************************************\
*                  Convert Code                          *
\********************************************************/


int is_skip(Cell op)
{
  if (op==skip_while_var_op) return(1);
  if (op==wait_while_var_op) return(1);

return(0);	        
}

void eam_instructions(struct Clauses *clause)
{
int calls=0,nrcall=0;
CInstr *b_code=NULL;

   inter_code=StartCode;
   while(inter_code!=NULL){ 
     if (inter_code->op==body_op) calls=0;
     if (inter_code->op==procceed_op) inter_code->nextInst=NULL;  /* CUIDADO */
     if (inter_code->op==allocate_op) inter_code->op=nop_op;
     if (inter_code->op==deallocate_op) inter_code->op=nop_op;
     if (inter_code->op==cutexit_op) {
	  inter_code->op=cut_op;
          insert_inst(inter_code,procceed_op,0,0);
     }
     if (inter_code->op==fail_op) insert_inst(inter_code,procceed_op,0,0);

     if (inter_code->op==execute_op) {
	  inter_code->op=call_op;
	  insert_inst(inter_code,procceed_op,0,0);
     }
     if (inter_code->op==safe_call_op) {
        if ((void *)inter_code->new4==(void *) eager_split) { 
	   inter_code->op=nop_op;
	   clause->predi->eager_split=1;
	} 
     }
     if (needs_box(inter_code->op)) calls++;

     inter_code=inter_code->nextInst;
   }

if (calls) {  
   inter_code=StartCode;
   while(inter_code!=NULL){ 
     if (inter_code->op==body_op) {
           inter_code->new4=calls;
	   insert_inst(inter_code,create_first_box_op,calls,++labelno);
	   inter_code=inter_code->nextInst;
     }
     if (needs_box(inter_code->op)) {
         insert_inst(inter_code,remove_box_op,nrcall,0);
         inter_code=inter_code->nextInst;
         b_code=inter_code;
	 insert_inst(inter_code,label_op,nrcall,labelno);
	 inter_code=inter_code->nextInst;
	 insert_inst(inter_code,create_box_op,++nrcall,++labelno);
     }
     inter_code=inter_code->nextInst;
   }

b_code->op=remove_last_box_op;
b_code->nextInst->nextInst->op=nop_op;
}

}

void delay_prepare_calls(void) {
CInstr *b_code=NULL;

   inter_code=StartCode;
   while(inter_code!=NULL){ 
     if (inter_code->op==body_op) b_code=inter_code;
     if (inter_code->op!=safe_call_op && inter_code->op!=cut_op && (needs_box(inter_code->op) || is_skip(inter_code->op))) break;

     if (inter_code->op==safe_call_op) {   
      inter_code->op=direct_safe_call_op;

       b_code->nextInst->op=nop_op;
       inter_code->nextInst->op=nop_op;
       if (b_code->new4>1) {
	   inter_code->nextInst->nextInst->op=body_op; 
	   inter_code->nextInst->nextInst->new1=0; 
	   inter_code->nextInst->nextInst->new4=b_code->new4-1;
       } else {
	   inter_code->nextInst->nextInst->op=procceed_op; 
	   inter_code->nextInst->nextInst->new1=0; 
	   inter_code->nextInst->nextInst->new4=0;
       }
       b_code->op=nop_op;

     }

     inter_code=inter_code->nextInst;
   }

}


int needs_box(Cell op)
{
  if (op==safe_call_op)  return(1);
  if (op==call_op)       return(1);
  if (op==std_base_op)   return(1);
  if (op==fail_op)       return(1);
  if (op==force_wait_op) return(1);
  if (op==cut_op)        return(1);
  if (op==commit_op)     return(1);
  if (op==cutexit_op)    return(1);
  if (op==write_op)      return(1);
  if (op==is_op)         return(1);
  if (op==equal_op)      return(1);
  if (op==exit_op)       return(1);

return(0);	      
}

int test_for_side_effects()
{
  int side_effects=0;

   inter_code=StartCode;
   while(inter_code!=NULL){ 
     switch (inter_code->op) {
         case write_op: 
	   side_effects+=WRITE;
	   break;

         case cutexit_op:
         case commit_op:
         case cut_op:
	   side_effects+=CUT;
	   break;	   
         case force_wait_op:
	   side_effects+=SEQUENCIAL;
	   break;
     }
     inter_code=inter_code->nextInst;
   }

return(side_effects);
}

void convert_Yaam(struct Clauses *clause)
{
PInstr *CodeStart, *ppc;
int calls=0;

        clause->val=0;
	clause->idx=Variavel;

	StartCode=NULL;
	inter_code=NULL;
	CodeStart=(&CGLOBS->cint)->CodeStart;
	ppc=CodeStart;
	while(ppc!=NULL){  /* copia o codigo YAAM para poder ser alterado  e ve o tipo de indexacao*/
	   if (ppc->op!=nop_op) { 
	     if (ppc->op==get_var_op && ppc->rnd2==1)    { clause->idx=Variavel;          clause->val=0; }
	     if (ppc->op==get_list_op && ppc->rnd2==1)   { clause->idx=Lista;             clause->val=0; }
	     if (ppc->op==get_struct_op && ppc->rnd2==1) { clause->idx=Estrutura;         clause->val=ppc->rnd1; }
	     if ((ppc->op==get_atom_op || ppc->op==get_num_op) && ppc->rnd2==1) {  clause->idx=Constante; clause->val=ppc->rnd1; }

	     if (ppc->op==body_op || ppc->op==safe_call_op || ppc->op==call_op || ppc->op==execute_op) calls=1;

	     if (ppc->op==endgoal_op) {
	       if (calls==0) emit_new(equal_op, 0,  0);
	       calls=0;
	     } else {
	       emit_new(ppc->op, ppc->rnd2,  ppc->rnd1);
	       if (ppc->op==body_op) calls=1;
  	       if (ppc->op==safe_call_op || ppc->op==call_op || ppc->op==execute_op) {
		 calls=1; identify_calls(inter_code); 
	       }
	     }

	   }
	   ppc=ppc->nextInst;
	}
        emit_new(nop_op, 0,0);
        emit_new(nop_op, 0,0);

	/*
        CodeStart->nextInst=NULL;
        ppc=CodeStart;

	(&CGLOBS->cint)->cpc=CodeStart;

	Yap_emit(cut_op,Zero,Zero,&CGLOBS->cint);
	Yap_emit(run_op,Zero,(unsigned long) (CGLOBS->cint).CurrentPred,&CGLOBS->cint);
	Yap_emit(procceed_op, Zero, Zero, &CGLOBS->cint);
	*/
return;
}


CInstr *insert_inst(CInstr  *inst, int o,int  r1,CELL r4)
{
CInstr *p;
   
     p=new_inst(o,r1,r4);
     if (inst==NULL) inst=p;
     else {
          p->nextInst=inst->nextInst;
	  inst->nextInst=p;
     }
return (p);
}

CInstr *emit_new(int o, int r1,CELL r4)
{
CInstr         *p;

     p=new_inst(o,r1,r4);
     if (inter_code == NULL) {
		inter_code = StartCode = p;
     }
     else {
		inter_code->nextInst = p;
		inter_code = p;
     }
return(inter_code);
}

CInstr *new_inst(int o, int r1,CELL r4)
{
	CInstr         *p;

	p = (CInstr *) alloc_mem_temp(sizeof(CInstr));
	p->op = o;
	p->new1 = r1;
	p->new4 = r4;
	p->nextInst = NULL;

return(p);
}

void *alloc_mem(Cell size)
{
  void *p;
  
  p=malloc(size); 
  if  (p==NULL) { printf(" Erro, falta de memoria \n"); exit(1); }
  //  p=Yap_AllocCMem(size,&CGLOBS->cint);
  
return(p);
}

void *alloc_mem_temp(Cell size)  /* memory that will be discard after compiling */
{
  void *p;
  
  p=malloc(size); 
  if  (p==NULL) { printf(" Erro, falta de memoria \n"); exit(1); }
  //  p=Yap_AllocCMem(size,&CGLOBS->cint);
  
return(p);
}



#ifdef DEBUG

static char *opformat2[] =
{
  "nop",
  "get_var %1,%4",
  "put_var %1,%4",
  "get_val %1,%4",
  "put_val %1,%4",
  "get_atom %1,%4",
  "put_atom %1,%4",
  "get_num %1,%4",
  "put_num %1,%4",
  "get_float %1,%4",
  "put_float %1,%4",
  "align_float %1,%4",
  "get_longint %1,%4",
  "put_longint %1,%4",
  "get_bigint %1,%4",
  "put_bigint %1,%4",
  "get_list %1,%4",
  "put_list %1,%4",
  "get_struct %1,%4",
  "put_struct %1,%4",
  "put_unsafe %1,%4",
  "unify_var %1,%4",
  "write_var %1,%4",
  "unify_val %1,%4",
  "write_val %1,%4",
  "unify_atom %1,%4",
  "write_atom %1,%4",
  "unify_num %1,%4",
  "write_num %1,%4",
  "unify_float %1,%4",
  "write_float %1,%4",
  "unify_longint %1,%4",
  "write_longint %1,%4",
  "unify_bigint %1,%4",
  "write_bigint %1,%4",
  "unify_list %1,%4",
  "write_list %1,%4",
  "unify_struct %1,%4",
  "write_struct %1,%4",
  "write_unsafe %1,%4",
  "fail %1,%4",
  "cut %1,%4",
  "cutexit %1,%4",
  "allocate %1,%4",
  "deallocate %1,%4",
  "try_me_else %1,%4",
  "jump %1,%4",
  "jump %1,%4",
  "proceed %1,%4",
  "call %1,%4",
  "execute %1,%4",
  "sys %1,%4",
  "%l: %1,%4",
  "name %1,%4",
  "pop %1,%4",
  "retry_me_else %1,%4",
  "trust_me_else_fail %1,%4",
  "either_me %1,%4",
  "or_else %1,%4",
  "or_last %1,%4",
  "push_or %1,%4",
  "pushpop_or %1,%4",
  "pop_or %1,%4",
  "save_by %1,%4",
  "commit_by %1,%4",
  "patch_by %1,%4",
  "try %1,%4",
  "retry %1,%4",
  "trust %1,%4",
  "try_in %1,%4",
  "jump_if_var %1,%4",
  "jump_if_nonvar %1,%4",
  "cache_arg %1,%4",
  "cache_sub_arg %1,%4",
  "switch_on_type %1,%4",
  "switch_on_constant %1,%4",
  "if_constant %1,%4",
  "switch_on_functor %1,%4",
  "if_functor %1,%4",
  "if_not_then %1,%4",
  "index_on_dbref %1,%4",
  "index_on_blob %1,%4",
  "check_var %1,%4",
  "save_pair %1,%4",
  "save_appl %1,%4",
  "fail_label %1,%4",
  "unify_local %1,%4",
  "write local %1,%4",
  "unify_last_list %1,%4",
  "write_last_list %1,%4",
  "unify_last_struct %1,%4",
  "write_last_struct %1,%4",
  "unify_last_var %1,%4",
  "unify_last_val %1,%4",
  "unify_last_local %1,%4",
  "unify_last_atom %1,%4",
  "unify_last_num %1,%4",
  "unify_last_float %1,%4",
  "unify_last_longint %1,%4",
  "unify_last_bigint %1,%4",
  "pvar_bitmap %1,%4",
  "pvar_live_regs %1,%4",
  "fetch_reg1_reg2 %1,%4",
  "fetch_constant_reg %1,%4",
  "fetch_reg_constant %1,%4",
  "function_to_var %1,%4",
  "function_to_al %1,%4",
  "enter_profiling %1,%4",
  "retry_profiled %1,%4",
  "count_call_op %1,%4",
  "count_retry_op %1,%4",
  "restore_temps %1,%4",
  "restore_temps_and_skip %1,%4",
  "enter_lu %1,%4",
  "empty_call %1,%4",
#ifdef YAPOR
  "sync
#endif /* YAPOR */
#ifdef TABLING
  "table_new_answer %1,%4",
  "table_try_single %1,%4",
#endif /* TABLING */
#ifdef TABLING_INNER_CUTS
  "clause_with_cut %1,%4",
#endif /* TABLING_INNER_CUTS */
#ifdef BEAM
  "run_op %1,%4",
  "body_op %1",
  "endgoal_op",
  "try_me_op %1,%4",
  "retry_me_op %1,%4",
  "trust_me_op %1,%4",
  "only_1_clause_op %1,%4",
  "create_first_box_op %1,%4",
  "create_box_op %1,%4",
  "create_last_box_op %1,%4",
  "remove_box_op %1,%4",
  "remove_last_box_op %1,%4",
  "prepare_tries",
  "std_base_op %1,%4",
  "direct_safe_call",
  "commit_op",
  "skip_while_var_op",
  "wait_while_var_op",
  "force_wait_op",
  "write_op",
  "is_op",
  "exit",
#endif
  "fetch_args_for_bccall %1,%4",
  "binary_cfunc %1,%4",
  "blob %1,%4",
#ifdef SFUNC
  ,
  "get_s_f_op %1,%4",
  "put_s_f_op %1,%4",
  "unify_s_f_op %1,%4",
  "write_s_f_op %1,%4",
  "unify_s_var %1,%4",
  "write_s_var %1,%4",
  "unify_s_val %1,%4",
  "write_s_val %1,%4",
  "unify_s_a %1,%4",
  "write_s_a %1,%4",
  "get_s_end",
  "put_s_end",
  "unify_s_end",
  "write_s_end"
#endif
};

void ShowCode_new2(int op, int new1,CELL new4);

void ShowCode_new2(int op, int new1,CELL new4) 
{
  char *f,ch;
  f=opformat2[op];

  while ((ch = *f++) != 0)
    {
      if (ch == '%')
	switch (ch = *f++)
	  {
	case '1':
	        Yap_plwrite(MkIntTerm(new1), NULL, 30, 0, GLOBAL_MaxPriority);
		break;
	case '4':
	        Yap_plwrite(MkIntTerm(new4), NULL, 20, 0, GLOBAL_MaxPriority);
		break;
	  default:
	    Yap_DebugPutc (LOCAL_c_error_stream,'%');
	    Yap_DebugPutc (LOCAL_c_error_stream,ch);
	  }
      else
        Yap_DebugPutc (LOCAL_c_error_stream,ch);
    }
    Yap_DebugPutc (LOCAL_c_error_stream,'\n');
}


#endif


#endif /* BEAM */
