/*************************************************************************
*									 *
*	       BEAM -> Basic Extended Andorra Model                      *
*         BEAM extends the YAP Prolog system to support the EAM          *
*									 *
* Copyright: Ricardo Lopes and NCC - University of Porto, Portugal       *
*									 *
**************************************************************************
* comments:	eam show abstract machine assembler		         *
*************************************************************************/

#ifdef BEAM

#include<stdio.h>
#include "Yap.h"
#include "Yatom.h"
#include "eam.h"
#include "eamamasm.h"

void eam_showcode(Cell *code);
extern int am_to_inst(Cell inst);

void eam_showcode(Cell *code)
{
int n;
#define carg1 *(code+1)
#define carg2 *(code+2)
#define carg3 *(code+3)
#define carg4 *(code+4)

  printf("--------------------------------------------------\n");
  while (1) {
      n=am_to_inst(*code);
      printf("%ld->",(long) code);
      switch(n) {
         case(_exit_eam):
	      printf("_exit\n");
	      code++;
              if  (*(code)==-1) return;
              break;
        case(_top_tree):
	      printf("_top_tree \n");   
	      code++;
	      break;
        case(_scheduler):
	      printf("_scheduler \n");
	      code++;
	      break;
        case(_prepare_tries):
	      printf("_prepare_tries for %d clauses with arity=%d   \n",(int) carg1,(int) carg2); 
	      code+=3;
	      break;
        case(_prepare_calls ):
	      printf("_prepare_calls %d \n",(int) carg1);
	      code+=2;
	      break;
        case(_get_var_X_op ):
	      printf("_get_var_X_op X%d, X%d\n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_get_var_Y_op ):
	      printf("_get_var_Y_op X%d, Y%d\n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_get_val_X_op ):
	      printf("_get_val_X_op X%d, X%d\n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_get_val_Y_op ):
	      printf("_get_val_Y_op X%d, Y%d\n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_get_atom_op ):
	      printf("_get_atom_op  X%d, %d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_get_list_op ):
	      printf("_get_list_op  X%d\n",(int) carg1);
	      code+=2;
	      break;
        case(_get_struct_op ):
	      printf("_get_struct_op X%d, %lX/%d\n",(int) carg1,(unsigned long) carg2,(int) carg3);
	      code+=4;
	      break;
        case(_unify_void_op ):
	      printf("_unify_void_op\n");   
	      code++;
	      break;
        case(_unify_val_X_op ):
	      printf("_unify_val_X_op X%d\n",(int) carg1);   
	      code+=2;
	      break;
        case(_unify_val_Y_op ):
	      printf("_unify_val_Y_op Y%d\n",(int) carg1);   
	      code+=2;
	      break;
        case(_unify_var_X_op ):
	      printf("_unify_var_X_op X%d\n",(int) carg1);   
	      code+=2;
	      break;
        case(_unify_var_Y_op ):
	      printf("_unify_var_Y_op Y%d\n",(int) carg1);   
	      code+=2;
	      break;
        case(_unify_atom_op ):
	      printf("_unify_atom_op 0x%lX\n",(unsigned long) carg1);   
	      code+=2;
	      break;
        case(_unify_list_op ):
	      printf("_unify_list_op \n");   
	      code++;
	      break;
        case(_unify_last_list_op ):
	      printf("_unify_last_list_op    \n");   
	      code++;
	      break;
        case(_unify_struct_op ):
	      printf("_unify_struct_op 0x%lX,%d\n",(unsigned long) carg1,(int) carg2);   
	      code+=3;
	      break;
        case(_unify_last_struct_op ):
	      printf("_unify_last_struct_op 0x%lX,%d\n",(unsigned long) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_unify_last_atom_op ):
	      printf("_unify_last_atom_op 0x%lX\n",(unsigned long) carg1); 
	      code+=2;
	      break;
        case(_unify_local_X_op ):
	      printf("_unify_local_X_op X%d\n",(int) carg1);   
	      code+=2;
	      break;
        case(_unify_local_Y_op ):
	      printf("_unify_local_Y_op X%d\n",(int) carg1);   
	      code+=2;
	      break;
        case(_put_var_X_op ):
	      printf("_put_var_X_op  X%d,X%d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_put_var_Y_op ):
	      printf("_put_var_Y_op X%d,Y%d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_put_var_P_op ):
	      printf("_put_var_P_op X%d,Y%d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_put_val_X_op ):
	      printf("_put_val_X_op X%d,X%d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_put_val_Y_op ):
	      printf("_put_val_Y_op X%d,Y%d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_put_atom_op ):
	      printf("_put_atom_op X%d, %d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_put_list_op ):
	      printf("_put_list_op X%d \n",(int) carg1);
	      code+=2;
	      break;
        case(_put_struct_op ):
	      printf("_put_struct_op X%d,%d,%d \n",(int) carg1,(int) carg2,(int) carg3);
	      code+=4;
	      break;
        case(_put_unsafe_op ):
	      printf("_put_unsafe_op X%d, Y%d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_write_void ):
	      printf("_write_void    \n");   
	      code++;
	      break;
        case(_write_var_X_op ):
	      printf("_write_var_X_op X%d \n",(int) carg1);
	      code+=2;
	      break;
        case(_write_var_Y_op ):
	      printf("_write_var_Y_op Y%d \n",(int) carg1);
	      code+=2;
	      break;
        case(_write_var_P_op ):
	      printf("_write_var_P_op Y%d \n",(int) carg1);
	      code+=2;
	      break;
        case(_write_val_X_op ):
	      printf("_write_val_X_op X%d \n",(int) carg1);
	      code+=2;
	      break;
        case(_write_val_Y_op ):
	      printf("_write_val_Y_op Y%d \n",(int) carg1);
	      code+=2;
	      break;
        case(_write_atom_op ):
	      printf("_write_atom_op %d \n",(int) carg1);
	      code+=2;
	      break;
        case(_write_list_op  ):
	      printf("_write_list_op     \n");   
	      code++;
	      break;
        case(_write_struct_op ):
	      printf("_write_struct_op %d,%d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_write_last_list_op ):
	      printf("_write_last_list_op    \n");   
	      code++;
	      break;
        case(_write_last_struct_op ):
	      printf("_write_last_struct_op %d,%d \n",(int) carg1,(int) carg2);
	      code+=3;
	      break;
        case(_write_local_X_op ):
	      printf("_write_local_X_op X%d \n",(int) carg1);
	      code+=2;
	      break;
        case(_write_local_Y_op ):
	      printf("_write_local_Y_op Y%d \n",(int) carg1);
	      code+=2;
	      break;
        case(_pop_op ):
	      printf("_pop_read_op    \n");   
	      code++;
	      break;
        case(_jump_op ):
	      printf("_jump_op  %ld\n",(long int) carg1);
	      code+=4;
	      break;
        case(_proceed_op ):
	      printf("_proceed_op    \n");   
	      code++;
	      break;
        case(_call_op ):
	      printf("_call_op %s/%d \n", ((PredEntry *) carg1)->beamTable->name,((PredEntry *) carg1)->beamTable->arity);
	      code+=2;
	      break;
        case(_safe_call_op ):
	      printf("_safe_call_op %ld \n",(long) carg1);
	      code+=2;
	      break;
        case(_safe_call_unary_op ):
	      printf("_safe_call_unary_op %ld \n",(long) carg1);
	      code+=2;
	      break;
        case(_safe_call_binary_op ):
	      printf("_safe_call_binary_op %ld \n",(long) carg1);
	      code+=2;
	      break;

        case(_only_1_clause_op ):
	      printf("_only_1_clause_op -> Use the same AND_BOX for the %dth clause of predicate %s/%d (Yvars=%d) \n",(int) carg4,((struct Clauses *)carg1)->predi->name,(int) carg2,(int) carg3);
	      code+=4;
	      break;
        case(_try_me_op ):
	      printf("_try_me_op (not final)\n");
	      code+=5;
	      break;
        case(_retry_me_op ):
	      printf("_retry_me_op (not final)\n");
	      code+=5;
	      break;
        case(_trust_me_op ):
	      printf("_trust_me_op (not final)\n");
	      code+=5;
	      break;
        case(_do_nothing_op ):
	      printf("do_nothing_op \n");   
	      code++;
	      break;
        case(_direct_safe_call_op ):
	      printf("_direct_safe_call_op %ld \n",(long) carg1);
	      code+=2;
	      break;
        case(_direct_safe_call_unary_op ):
	      printf("_direct_safe_call_unary_op %ld \n",(long) carg1);
	      code+=2;
	      break;
        case(_direct_safe_call_binary_op ):
	      printf("_direct_safe_call_binary_op %ld \n",(long) carg1);
	      code+=2;
	      break;



        case(_skip_while_var ):
	      printf("_skip_while_var \n");   
	      code++;
	      break;
        case(_wait_while_var ):
	      printf("_wait_while_var \n");   
	      code++;
	      break;
        case(_force_wait ):
	      printf("_force_wait \n");   
	      code++;
	      break;
        case(_write_call ):
	      printf("_write_call \n");   
	      code++;
	      break;
        case(_is_call ):
	      printf("_is_call \n");   
	      code++;
	      break;
        case(_equal_call ):
	      printf("_equal_call \n");   
	      code++;
	      break;
        case(_cut_op ):
	      printf("_cut_op    \n");   
	      code++;
	      break;
        case(_commit_op ):
	      printf("_commit_op    \n");   
	      code++;
	      break;
        case(_fail_op ):
	      printf("_fail_op    \n");   
	      code++;
	      break;

        case(_save_b_X_op ):
	      printf("_save_b_X_op    \n");   
	      code++;
	      break;
        case(_save_b_Y_op ):
	      printf("_save_b_Y_op    \n");   
	      code++;
	      break;
        case(_save_appl_X_op ):
	      printf("_save_appl_X_op    \n");   
	      code++;
	      break;
        case(_save_appl_Y_op ):
	      printf("_save_appl_Y_op    \n");   
	      code++;
	      break;
        case(_save_pair_X_op ):
	      printf("_save_pair_X_op    \n");   
	      code++;
	      break;
        case(_save_pair_Y_op ):
	      printf("_save_pair_Y_op    \n");   
	      code++;
	      break;
        case(_either_op ):
	      printf("_either_op    \n");   
	      code++;
	      break;
        case(_orelse_op ):
	      printf("_orelse_op    \n");   
	      code++;
	      break;
        case(_orlast_op ):
	      printf("_orlast_op    \n");   
	      code++;
	      break;

         default: 
            if (n!=*code) printf("inst(%d)\n",n);
            else printf("Label Next Call %d\n",n);
	    code++;
      }

  }

}


#endif /* BEAM */
