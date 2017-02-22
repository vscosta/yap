/*************************************************************************
*									 *
*	       BEAM -> Basic Extended Andorra Model                      *
*         BEAM extends the YAP Prolog system to support the EAM          *
*									 *
* Copyright: Ricardo Lopes and NCC - University of Porto, Portugal       *
*									 *
**************************************************************************
* comments:	abstract machine assembler                               *
*************************************************************************/

#ifdef BEAM

#include "Yap.h"
#include "YapCompile.h"
#include "clause.h"
#include "eam.h"
#include "eamamasm.h"
#include <stdio.h>
#include <stdlib.h>

Cell *inst_code;
int pass=0;
Cell *labels[1000];

Cell *Code_Start;
Cell Area_Code[200000];
Cell area_code=0;

extern  Cell inst_am(int n);
void emit_inst(long int i);
void emit_par(long int i);
void emit_upar(Cell i);
Cell *get_addr(void);
int Is_X_Var(Ventry *ve);
int Is_P_Var(Ventry *ve);
int X_Var(Ventry *ve);
int Y_Var(Ventry *ve);
void eam_pass(CInstr *ppc);
Cell *eam_assemble(CInstr *code);
int next_not_nop_inst(CInstr *ppc);
extern void *alloc_mem(Cell);

void emit_inst(long int i)
{
  if (pass) *inst_code=inst_am(i);
  inst_code++;
}

void emit_par(long int i)
{
  if (pass) *inst_code=i;
  inst_code++;
}

void emit_upar(Cell i)
{
  if (pass) *inst_code=i;
  inst_code++;
}


Cell *get_addr(void)
{
  return(inst_code);
}


int Is_P_Var(Ventry *ve)
{
  if (ve->FirstOfVE>0) return (1);  /* var aparece pela primeira no corpo da clausula */
return(0);
}

int Is_X_Var(Ventry *ve)
{
	if (ve->KindOfVE == PermVar) return(0);
	if (ve->KindOfVE == VoidVar) return(0);

return(1);
}

int X_Var(Ventry *ve)
{
int var;

if (ve->KindOfVE == PermVar || ve->KindOfVE == VoidVar ) {
    printf("Erro no tipo de variavel X ->eamamas.c \n");
    exit(1);
}
	var = ((ve->NoOfVE) & MaskVarAdrs);

return (var);
}

extern int nperm;

int Y_Var(Ventry *ve)
{
int var;
if (ve->KindOfVE != PermVar) {
    printf("Erro no tipo de variavel Y ->eamamas.c \n");
    exit(1);
}
        var = ((ve->NoOfVE) & MaskVarAdrs);
        return (var);
}


int next_not_nop_inst(CInstr *ppc) {
  while(ppc) {
    if ((int) ppc->op!=nop_op) return ((int) ppc->op);
    ppc = ppc->nextInst;
  }
return exit_op;
}

void eam_pass(CInstr *ppc)
{
  int alloc_found=0;
  int body=0;

	while (ppc) {
		switch ((int) ppc->op) {

		case get_var_op:
		        if (Is_X_Var((Ventry *) ppc->new4)) {
			    emit_inst(_get_var_X_op);
			    emit_par(ppc->new1);
			    emit_par(X_Var((Ventry *) ppc->new4));
			} else {
		            emit_inst(_get_var_Y_op);
			    emit_par(ppc->new1);
			    emit_par(Y_Var((Ventry *) ppc->new4));
			}
			break;
		case get_val_op:
		        if (Is_X_Var((Ventry *) ppc->new4)) {
			    emit_inst(_get_val_X_op);
			    emit_par(ppc->new1);
			    emit_par(X_Var((Ventry *) ppc->new4));
			} else {
			    emit_inst(_get_val_Y_op);
			    emit_par(ppc->new1);
			    emit_par(Y_Var((Ventry *) ppc->new4));
			}
		        break;

		case get_num_op:
		case get_atom_op:
		        emit_inst(_get_atom_op);
			emit_par(ppc->new1);
			emit_par(ppc->new4);
			break;

		case get_list_op:
		        emit_inst(_get_list_op);
			emit_par(ppc->new1);
			break;
		case get_struct_op:
		        emit_inst(_get_struct_op);
			emit_par(ppc->new1);
			emit_par(ppc->new4);
			emit_par(ArityOfFunctor((Functor ) ppc->new4));
			break;

		case unify_last_local_op:
		case unify_local_op:
		        if (Is_X_Var((Ventry *) ppc->new4)) {
			          emit_inst(_unify_local_X_op);
			          emit_par(X_Var((Ventry *) ppc->new4));
			} else {
			          emit_inst(_unify_local_Y_op);
			          emit_par(Y_Var((Ventry *) ppc->new4));
		        }
			break;

		case unify_last_val_op:
		case unify_val_op:
		        if (((Ventry *)(ppc->new4))->KindOfVE!=VoidVar) {
		           if (Is_X_Var((Ventry *) ppc->new4)) {
			          emit_inst(_unify_val_X_op);
			          emit_par(X_Var((Ventry *) ppc->new4));
			   } else {
			          emit_inst(_unify_val_Y_op);
			          emit_par(Y_Var((Ventry *) ppc->new4));

			   }
		        } else { emit_inst(_unify_void_op); }
			break;

		case unify_last_var_op:
		case unify_var_op:
		        if (((Ventry *)(ppc->new4))->KindOfVE!=VoidVar) {
		           if (Is_X_Var((Ventry *) ppc->new4)) {
			          emit_inst(_unify_var_X_op);
			          emit_par(X_Var((Ventry *) ppc->new4));
			   } else {
			          emit_inst(_unify_var_Y_op);
			          emit_par(Y_Var((Ventry *) ppc->new4));
			   }
		        } else { emit_inst(_unify_void_op); }
			break;

		case unify_last_atom_op:
		case unify_last_num_op:
		        emit_inst(_unify_last_atom_op);
			emit_par(ppc->new4);
			break;
		case unify_num_op:
		case unify_atom_op:
		        emit_inst(_unify_atom_op);
			emit_par(ppc->new4);
			break;
		case unify_list_op:
		        emit_inst(_unify_list_op);
			break;
		case unify_last_list_op:
		        emit_inst(_unify_last_list_op);
			break;
		case unify_struct_op:
		        emit_inst(_unify_struct_op);
			emit_par(ppc->new4);
			emit_par(ArityOfFunctor((Functor )ppc->new4));
			break;
		case unify_last_struct_op:
		        emit_inst(_unify_last_struct_op);
			emit_par(ppc->new4);
			emit_par(ArityOfFunctor((Functor )ppc->new4));
			break;

		case put_unsafe_op:
		  /*
		  printf("Got a put_unsafe...\n");
		        emit_inst(_put_unsafe_op);
			emit_par(ppc->new1);
			emit_par(Y_Var((Ventry *) ppc->new4));
			break;
		  */
		case put_val_op:
		  /*
		        if (Is_X_Var((Ventry *) ppc->new4)) {
			       emit_inst(_put_val_X_op);
			       emit_par(ppc->new1);
			       emit_par(X_Var((Ventry *) ppc->new4));
			       break;
			} else {
			       emit_inst(_put_val_Y_op);
			       emit_par(ppc->new1);
			       emit_par(Y_Var((Ventry *) ppc->new4));
			       break;
			}
		  */
		case put_var_op:
		        if (Is_X_Var((Ventry *) ppc->new4)) {
			       emit_inst(_put_var_X_op);
			       emit_par(ppc->new1);
			       emit_par(X_Var((Ventry *) ppc->new4));
			} else {
 		           if (Is_P_Var((Ventry *) ppc->new4)) emit_inst(_put_var_P_op);
			   else emit_inst(_put_var_Y_op);
			       emit_par(ppc->new1);
			       emit_par(Y_Var((Ventry *) ppc->new4));
			}
			break;

		case put_num_op:
		case put_atom_op:
		        emit_inst(_put_atom_op);
			emit_par(ppc->new1);
			emit_par(ppc->new4);
			break;
		case put_list_op:
		        emit_inst(_put_list_op);
			emit_par(ppc->new1);
			break;
		case put_struct_op:
		        emit_inst(_put_struct_op);
			emit_par(ppc->new1);
			emit_par(ppc->new4);
			emit_par(ArityOfFunctor((Functor )ppc->new4));
			break;

		case write_local_op:
		        if (Is_X_Var((Ventry *) ppc->new4)) {
		                  emit_inst(_write_local_X_op);
	  		          emit_par(X_Var((Ventry *) ppc->new4));
			} else {
		                  emit_inst(_write_local_Y_op);
			          emit_par(Y_Var((Ventry *) ppc->new4));
			}
			break;

		case write_val_op:
		        if (((Ventry *)(ppc->new4))->KindOfVE!=VoidVar) {
		           if (Is_X_Var((Ventry *) ppc->new4)) {
		                  emit_inst(_write_val_X_op);
	  		          emit_par(X_Var((Ventry *) ppc->new4));
			   } else {
		                  emit_inst(_write_val_Y_op);
			          emit_par(Y_Var((Ventry *) ppc->new4));
			   }
		        } else emit_inst(_write_void);
			break;

		case write_var_op:
		        if (((Ventry *)(ppc->new4))->KindOfVE!=VoidVar) {
		           if (Is_X_Var((Ventry *) ppc->new4)) {
		                  emit_inst(_write_var_X_op);
	  		          emit_par(X_Var((Ventry *) ppc->new4));
			   } else {
 		              if (Is_P_Var((Ventry *) ppc->new4)) emit_inst(_write_var_P_op);
		              else emit_inst(_write_var_Y_op);
			           emit_par(Y_Var((Ventry *) ppc->new4));
			   }
		        } else emit_inst(_write_void);
			break;


		case write_num_op:
		case write_atom_op:
		        emit_inst(_write_atom_op);
			emit_par(ppc->new4);
			break;
		case write_list_op:
		        emit_inst(_write_list_op);
			break;
		case write_last_list_op:
		        emit_inst(_write_last_list_op);
			break;
		case write_struct_op:
		        emit_inst(_write_struct_op);
			emit_par(ppc->new4);
			emit_par(ArityOfFunctor((Functor )ppc->new4));
			break;
		case write_last_struct_op:
		        emit_inst(_write_last_struct_op);
			emit_par(ppc->new4);
			emit_par(ArityOfFunctor((Functor )ppc->new4));
			break;

		case fail_op:
		        emit_inst(_fail_op);
			break;
		case cutexit_op:
		        printf("cutexit \n");
			exit(1);
			break;

		case cut_op:
		        emit_inst(_cut_op);
			break;
		case commit_op:
		        emit_inst(_commit_op);
			break;

		case procceed_op:
		        emit_inst(_proceed_op);
			break;
		case pop_op:
			emit_inst(_pop_op);
			emit_par(ppc->new4);
			break;
		case save_b_op:
		        if (Is_X_Var((Ventry *) ppc->new4)) {
			   emit_inst(_save_b_X_op);
			   emit_par(X_Var((Ventry *) ppc->new4));
		        } else {
			   emit_inst(_save_b_Y_op);
			   emit_par(Y_Var((Ventry *) ppc->new4));
		        }
			break;
	        case save_pair_op:
		       if (Is_X_Var((Ventry *) ppc->new4)) {
			  emit_inst(_save_pair_X_op);
			  emit_par(X_Var((Ventry *) ppc->new4));
		       } else {
			   emit_inst(_save_pair_Y_op);
			   emit_par(Y_Var((Ventry *) ppc->new4));
		       }
		       break;
	        case save_appl_op:
		        if (Is_X_Var((Ventry *) ppc->new4)) {
			  emit_inst(_save_appl_X_op);
			  emit_par(X_Var((Ventry *) ppc->new4));
		         } else {
			   emit_inst(_save_appl_Y_op);
			   emit_par(Y_Var((Ventry *) ppc->new4));
		         }
			break;
		case std_base_op:
		        emit_inst(_std_base+ppc->new4);
			break;

		case safe_call_op:
		        if (ppc->new1==1) {
		           emit_inst(_safe_call_unary_op);
			} else if (ppc->new1==2) {
		           emit_inst(_safe_call_binary_op);
			} else {
		           emit_inst(_safe_call_op);
			}
			emit_par(ppc->new4);
			break;

		case direct_safe_call_op:
		        if (ppc->new1==1) {
  		           emit_inst(_direct_safe_call_unary_op);
		        } else if (ppc->new1==2) {
  		           emit_inst(_direct_safe_call_binary_op);
			} else {
  		           emit_inst(_direct_safe_call_op);
			}
			emit_par(ppc->new4);
			break;

		case call_op:
			emit_inst(_call_op);
			emit_par(ppc->new4);
			break;

		case skip_while_var_op:
			emit_inst(_skip_while_var);
			break;
		case wait_while_var_op:
			emit_inst(_wait_while_var);
			break;
		case force_wait_op:
			emit_inst(_force_wait);
			break;
		case write_op:
		        if (ppc->new1=='\n') {
			  static Atom a=NULL;
			  if (a==NULL) a=Yap_LookupAtom("\n");
		          emit_inst(_put_atom_op);
			  emit_par(1);
			  emit_par((Cell) MkAtomTerm(a));
			}
 		        emit_inst(_write_call);
			break;
		case is_op:
			emit_inst(_is_call);
			break;
		case equal_op:
			emit_inst(_equal_call);
			break;

		case either_op:
			emit_inst(_either_op);
			emit_par(ppc->new1);
                        emit_upar((Cell) Code_Start+ (Cell) labels[ppc->new4]);
			break;
		case orelse_op:
	                emit_inst(_orelse_op);
                        emit_upar((Cell) Code_Start+ (Cell) labels[ppc->new4]);
			break;
		case orlast_op:
			emit_inst(_orlast_op);
			break;

		case create_first_box_op:
		case create_box_op:
		case create_last_box_op:
			emit_upar((Cell) Code_Start+ (Cell) labels[ppc->new4]);
		        alloc_found=1;
			break;

		case remove_box_op:
		case remove_last_box_op:
			break;

		case jump_op:
		        emit_inst(_jump_op);
			emit_upar((Cell) Code_Start+ (Cell) labels[ppc->new4]);
			break;
		case label_op:
		        if (pass==0) labels[ppc->new4] = get_addr();
			break;

		case run_op:
/* se ficar vazio, retirar no eam_am.c o +5 das linhas pc=clause->code+5 no only_1_clause e no call */
		        emit_inst(_try_me_op);
			emit_par(0);
			emit_par(0);
			emit_par(0);
			emit_par(0);
		        break;

		case only_1_clause_op:
		        emit_inst(_only_1_clause_op);
		        emit_par(ppc->new4);
			emit_par(((struct Clauses *)ppc->new4)->predi->arity);
		        emit_par(((struct Clauses *)ppc->new4)->nr_vars);
			emit_par(0); /* Nr da alternativa */
			break;
		case try_me_op:
		        emit_inst(_try_me_op);
			emit_par(ppc->new4);
			emit_par(((struct Clauses *)ppc->new4)->predi->arity);
		        emit_par(((struct Clauses *)ppc->new4)->nr_vars);
			emit_par(0); /* Nr da alternativa */
		        break;
		case retry_me_op:
		        emit_inst(_retry_me_op);
			emit_par(ppc->new4);
			emit_par(((struct Clauses *)ppc->new4)->predi->arity);
		        emit_par(((struct Clauses *)ppc->new4)->nr_vars);
			emit_par(ppc->new1);
		        break;
		case trust_me_op:
		        emit_inst(_trust_me_op);
			emit_par(ppc->new4);
			emit_par(((struct Clauses *)ppc->new4)->predi->arity);
		        emit_par(((struct Clauses *)ppc->new4)->nr_vars);
			emit_par(ppc->new1);
		        break;

		case body_op:
		        if (next_not_nop_inst(ppc->nextInst)==procceed_op) {
			  //emit_inst(_proceed_op);
			    break;
		        } else if (next_not_nop_inst(ppc->nextInst)==fail_op) {
			  //emit_inst(_fail_op);
			    break;
			}
			if (ppc->new4!=0) {
 		           emit_inst(_prepare_calls);
			   emit_par(ppc->new4); /* nr_calls */
			}
			body=1;
			break;

		case prepare_tries:
		        emit_inst(_prepare_tries);
			emit_par(ppc->new1);
			emit_par(ppc->new4);
			break;

		case exit_op:
		        emit_inst(_exit_eam);
			break;

		case mark_initialized_pvars_op:
		        break;
		case fetch_args_for_bccall:
		case bccall_op:
	 	        printf("[ Fatal Error: fetch and bccall instructions not supported ]\n");
 			exit(1);
		        break;

		case endgoal_op:
		case nop_op:
		case name_op:
			break;

		default:
		  if (pass) {
			printf("[ Sorry, there is at least one unsupported instruction in your code... %3d] %d\n",ppc->op,exit_op);
			printf("[ please note that beam still does not support a lot of builtins          ]\n");
		  }
		        emit_inst(_fail_op);

		}
		ppc = ppc->nextInst;
	}
	emit_inst(_exit_eam);
        emit_par(-1);
}


Cell *eam_assemble(CInstr *code)
{

  Code_Start=0;
  pass=0;
  inst_code=0;
  eam_pass(code);

  pass=1;
  Code_Start=alloc_mem((Cell) inst_code);
  inst_code=Code_Start;
  eam_pass(code);

  return(Code_Start);
}


#endif /* BEAM */
