/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		analyst.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Tracing the abstract machine				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";

#endif

#include "Yap.h"

#ifdef ANALYST
#include "Yatom.h"
#include "yapio.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

STATIC_PROTO(Int p_reset_op_counters, (void));
STATIC_PROTO(Int p_show_op_counters, (void));
STATIC_PROTO(Int p_show_ops_by_group, (void));

static char *op_names[_std_top + 1] =
{
#define OPCODE(OP,TYPE) #OP
#include "YapOpcodes.h"
#undef  OPCODE
};

static Int 
p_reset_op_counters()
{
  int i;

  for (i = 0; i <= _std_top; ++i)
    _YAP_opcount[i] = 0;
  return (TRUE);
}

static void 
print_instruction(int inst)
{
  int j;

  fprintf(_YAP_stderr, "%s", op_names[inst]);
  for (j = strlen(op_names[inst]); j < 25; j++)
    putc(' ', _YAP_stderr);
  j = _YAP_opcount[inst];
  if (j < 100000000) {
    putc(' ', _YAP_stderr);
    if (j < 10000000) {
      putc(' ', _YAP_stderr);
      if (j < 1000000) {
	putc(' ', _YAP_stderr);
	if (j < 100000) {
	  putc(' ', _YAP_stderr);
	  if (j < 10000) {
	    putc(' ', _YAP_stderr);
	    if (j < 1000) {
	      putc(' ', _YAP_stderr);
	      if (j < 100) {
		putc(' ', _YAP_stderr);
		if (j < 10) {
		  putc(' ', _YAP_stderr);
		}
	      }
	    }
	  }
	}
      }
    }
  }
  fprintf(_YAP_stderr, "%d\n", _YAP_opcount[inst]);
}

static Int 
p_show_op_counters()
{
  int i;
  char *program;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1) || !IsAtomTerm(t1))
    return (FALSE);
  else
    program = RepAtom(AtomOfTerm(t1))->StrOfAE;

  fprintf(_YAP_stderr, "\n Instructions Executed in %s \n", program);
  for (i = 0; i <= _std_top; ++i)
    print_instruction(i);
  fprintf(_YAP_stderr, "\n Control Instructions \n");
  print_instruction(_op_fail);
  print_instruction(_execute);
  print_instruction(_dexecute);
  print_instruction(_call);
  print_instruction(_fcall);
  print_instruction(_call_cpred);
  print_instruction(_call_c_wfail);
  print_instruction(_procceed);
  print_instruction(_allocate);
  print_instruction(_deallocate);

  fprintf(_YAP_stderr, "\n Choice Point Manipulation Instructions\n");
  print_instruction(_try_me);
  print_instruction(_retry_me);
  print_instruction(_trust_me);
  print_instruction(_try_me0);
  print_instruction(_retry_me0);
  print_instruction(_trust_me0);
  print_instruction(_try_me1);
  print_instruction(_retry_me1);
  print_instruction(_trust_me1);
  print_instruction(_try_me2);
  print_instruction(_retry_me2);
  print_instruction(_trust_me2);
  print_instruction(_try_me3);
  print_instruction(_retry_me3);
  print_instruction(_trust_me3);
  print_instruction(_try_me4);
  print_instruction(_retry_me4);
  print_instruction(_trust_me4);
  print_instruction(_try_clause);
  print_instruction(_try_in);
  print_instruction(_retry);
  print_instruction(_trust_in);
  print_instruction(_trust);
  print_instruction(_retry_first);
  print_instruction(_trust_first_in);
  print_instruction(_trust_first);
  print_instruction(_retry_tail);
  print_instruction(_trust_tail_in);
  print_instruction(_trust_tail);
  print_instruction(_retry_head);
  print_instruction(_trust_head_in);
  print_instruction(_trust_head);

  fprintf(_YAP_stderr, "\n Disjunction Instructions\n");
  print_instruction(_either);
  print_instruction(_or_else);
  print_instruction(_or_last);
  print_instruction(_jump);
  print_instruction(_move_back);

  fprintf(_YAP_stderr, "\n Dynamic Predicates Choicepoint Instructions\n");
  print_instruction(_try_and_mark);
  print_instruction(_retry_and_mark);

  fprintf(_YAP_stderr, "\n C Predicates Choicepoint Instructions\n");
  print_instruction(_try_c);
  print_instruction(_retry_c);

  fprintf(_YAP_stderr, "\n Indexing Instructions\n");
  fprintf(_YAP_stderr, "\n  Switch on Type\n");
  print_instruction(_switch_on_type);
  print_instruction(_switch_on_nonv);
  print_instruction(_switch_last);
  print_instruction(_switch_on_head);
  print_instruction(_switch_list_nl);
  print_instruction(_switch_list_nl_prefetch);
  print_instruction(_switch_nv_list);
  print_instruction(_switch_l_list);
  fprintf(_YAP_stderr, "\n  Switch on Value\n");
  print_instruction(_if_cons);
  print_instruction(_go_on_cons);
  print_instruction(_switch_on_cons);
  print_instruction(_if_func);
  print_instruction(_go_on_func);
  print_instruction(_switch_on_func);
  fprintf(_YAP_stderr, "\n  Other Switches\n");
  print_instruction(_if_not_then);

  fprintf(_YAP_stderr, "\n Get Instructions\n");
  print_instruction(_get_x_var);
  print_instruction(_get_y_var);
  print_instruction(_get_x_val);
  print_instruction(_get_y_val);
  print_instruction(_get_atom);
  print_instruction(_get_list);
  print_instruction(_get_struct);
  fprintf(_YAP_stderr, "\n   Optimised Get Instructions\n");
  print_instruction(_glist_valx);
  print_instruction(_glist_valy);
  print_instruction(_gl_void_varx);
  print_instruction(_gl_void_vary);
  print_instruction(_gl_void_valx);
  print_instruction(_gl_void_valy);

  fprintf(_YAP_stderr, "\n Unify Read Instructions\n");
  print_instruction(_unify_x_var);
  print_instruction(_unify_x_var2);
  print_instruction(_unify_y_var);
  print_instruction(_unify_x_val);
  print_instruction(_unify_y_val);
  print_instruction(_unify_x_loc);
  print_instruction(_unify_y_loc);
  print_instruction(_unify_atom);
  print_instruction(_unify_n_atoms);
  print_instruction(_unify_n_voids);
  print_instruction(_unify_list);
  print_instruction(_unify_struct);
  fprintf(_YAP_stderr, "\n   Unify Last Read Instructions\n");
  print_instruction(_unify_l_x_var);
  print_instruction(_unify_l_x_var2);
  print_instruction(_unify_l_y_var);
  print_instruction(_unify_l_x_val);
  print_instruction(_unify_l_y_val);
  print_instruction(_unify_l_x_loc);
  print_instruction(_unify_l_y_loc);
  print_instruction(_unify_l_atom);
  print_instruction(_unify_l_n_voids);
  print_instruction(_unify_l_list);
  print_instruction(_unify_l_struc);

  fprintf(_YAP_stderr, "\n Unify Write Instructions\n");
  print_instruction(_unify_x_var_write);
  print_instruction(_unify_x_var2_write);
  print_instruction(_unify_y_var_write);
  print_instruction(_unify_x_val_write);
  print_instruction(_unify_y_val_write);
  print_instruction(_unify_x_loc_write);
  print_instruction(_unify_y_loc_write);
  print_instruction(_unify_atom_write);
  print_instruction(_unify_n_atoms_write);
  print_instruction(_unify_n_voids_write);
  print_instruction(_unify_list_write);
  print_instruction(_unify_struct_write);
  fprintf(_YAP_stderr, "\n   Unify Last Read Instructions\n");
  print_instruction(_unify_l_x_var_write);
  print_instruction(_unify_l_x_var2_write);
  print_instruction(_unify_l_y_var_write);
  print_instruction(_unify_l_x_val_write);
  print_instruction(_unify_l_y_val_write);
  print_instruction(_unify_l_x_loc_write);
  print_instruction(_unify_l_y_loc_write);
  print_instruction(_unify_l_atom_write);
  print_instruction(_unify_l_n_voids_write);
  print_instruction(_unify_l_list_write);
  print_instruction(_unify_l_struc_write);

  fprintf(_YAP_stderr, "\n Put Instructions\n");
  print_instruction(_put_x_var);
  print_instruction(_put_y_var);
  print_instruction(_put_x_val);
  print_instruction(_put_y_val);
  print_instruction(_put_unsafe);
  print_instruction(_put_atom);
  print_instruction(_put_list);
  print_instruction(_put_struct);

  fprintf(_YAP_stderr, "\n Write Instructions\n");
  print_instruction(_write_x_var);
  print_instruction(_write_y_var);
  print_instruction(_write_x_val);
  print_instruction(_write_y_val);
  print_instruction(_write_x_loc);
  print_instruction(_write_y_loc);
  print_instruction(_write_atom);
  print_instruction(_write_n_atoms);
  print_instruction(_write_n_voids);
  print_instruction(_write_list);
  print_instruction(_write_struct);
  fprintf(_YAP_stderr, "\n   Last Write Instructions\n");
  print_instruction(_write_l_list);
  print_instruction(_write_l_struc);

  fprintf(_YAP_stderr, "\n Miscellaneous Instructions\n");
  print_instruction(_cut);
  print_instruction(_cut_t);
  print_instruction(_cut_e);
  print_instruction(_skip);
  print_instruction(_pop);
  print_instruction(_pop_n);
  print_instruction(_trust_fail);
  print_instruction(_index_pred);
  print_instruction(_save_b_x);
  print_instruction(_save_b_y);
  print_instruction(_save_pair_x);
  print_instruction(_save_pair_y);
  print_instruction(_save_pair_x_write);
  print_instruction(_save_pair_y_write);
  print_instruction(_save_appl_x);
  print_instruction(_save_appl_y);
  print_instruction(_save_appl_x_write);
  print_instruction(_save_appl_y_write);
  print_instruction(_Ystop);
  print_instruction(_Nstop);

  return (TRUE);
}

typedef struct {
  int nxvar, nxval, nyvar, nyval, ncons, nlist, nstru, nmisc;
} u_YAP_opcount;

typedef struct {
  int ncalls, nexecs, nproceeds, ncallbips, ncuts, nallocs, ndeallocs;
} c_YAP_opcount;

typedef struct {
  int ntries, nretries, ntrusts;
} ccpcount;

static Int 
p_show_ops_by_group(void)
{

  u_YAP_opcount c_get, c_unify, c_put, c_write;
  c_YAP_opcount c_control;
  ccpcount c_cp;
  int gets, unifies, puts, writes, controls, choice_pts, indexes, misc,
    total;
  char *program;
  Term t1;

  t1 = Deref(ARG1);
  if (IsVarTerm(t1) || !IsAtomTerm(t1))
    return (FALSE);
  else
    program = RepAtom(AtomOfTerm(t1))->StrOfAE;

  c_get.nxvar =
    _YAP_opcount[_get_x_var];
  c_get.nyvar =
    _YAP_opcount[_get_y_var];
  c_get.nxval =
    _YAP_opcount[_get_x_val];
  c_get.nyval =
    _YAP_opcount[_get_y_val];
  c_get.ncons =
    _YAP_opcount[_get_atom];
  c_get.nlist =
    _YAP_opcount[_get_list] +
    _YAP_opcount[_glist_valx] +
    _YAP_opcount[_glist_valy] +
    _YAP_opcount[_gl_void_varx] +
    _YAP_opcount[_gl_void_vary] +
    _YAP_opcount[_gl_void_valx] +
    _YAP_opcount[_gl_void_valy];
  c_get.nstru =
    _YAP_opcount[_get_struct];

  gets = c_get.nxvar + c_get.nyvar + c_get.nxval + c_get.nyval +
    c_get.ncons + c_get.nlist + c_get.nstru;

  c_unify.nxvar =
    _YAP_opcount[_unify_x_var] +
    _YAP_opcount[_unify_void] +
    _YAP_opcount[_unify_n_voids] +
    2 * _YAP_opcount[_unify_x_var2] +
    2 * _YAP_opcount[_gl_void_varx] +
    _YAP_opcount[_gl_void_vary] +
    _YAP_opcount[_gl_void_valx] +
    _YAP_opcount[_unify_l_x_var] +
    _YAP_opcount[_unify_l_void] +
    _YAP_opcount[_unify_l_n_voids] +
    2 * _YAP_opcount[_unify_l_x_var2] +
    _YAP_opcount[_unify_x_var_write] +
    _YAP_opcount[_unify_void_write] +
    _YAP_opcount[_unify_n_voids_write] +
    2 * _YAP_opcount[_unify_x_var2_write] +
    _YAP_opcount[_unify_l_x_var_write] +
    _YAP_opcount[_unify_l_void_write] +
    _YAP_opcount[_unify_l_n_voids_write] +
    2 * _YAP_opcount[_unify_l_x_var2_write];
  c_unify.nyvar =
    _YAP_opcount[_unify_y_var] +
    _YAP_opcount[_gl_void_vary] +
    _YAP_opcount[_unify_l_y_var] +
    _YAP_opcount[_unify_y_var_write] +
    _YAP_opcount[_unify_l_y_var_write];
  c_unify.nxval =
    _YAP_opcount[_unify_x_val] +
    _YAP_opcount[_unify_x_loc] +
    _YAP_opcount[_glist_valx] +
    _YAP_opcount[_gl_void_valx] +
    _YAP_opcount[_unify_l_x_val] +
    _YAP_opcount[_unify_l_x_loc] +
    _YAP_opcount[_unify_x_val_write] +
    _YAP_opcount[_unify_x_loc_write] +
    _YAP_opcount[_unify_l_x_val_write] +
    _YAP_opcount[_unify_l_x_loc_write];
  c_unify.nyval =
    _YAP_opcount[_unify_y_val] +
    _YAP_opcount[_unify_y_loc] +
    _YAP_opcount[_glist_valy] +
    _YAP_opcount[_gl_void_valy] +
    _YAP_opcount[_unify_l_y_val] +
    _YAP_opcount[_unify_l_y_loc] +
    _YAP_opcount[_unify_y_val_write] +
    _YAP_opcount[_unify_y_loc_write] +
    _YAP_opcount[_unify_l_y_val_write] +
    _YAP_opcount[_unify_l_y_loc_write];
  c_unify.ncons =
    _YAP_opcount[_unify_atom] +
    _YAP_opcount[_unify_n_atoms] +
    _YAP_opcount[_unify_l_atom] +
    _YAP_opcount[_unify_atom_write] +
    _YAP_opcount[_unify_n_atoms_write] +
    _YAP_opcount[_unify_l_atom_write];
  c_unify.nlist =
    _YAP_opcount[_unify_list] +
    _YAP_opcount[_unify_l_list] +
    _YAP_opcount[_unify_list_write] +
    _YAP_opcount[_unify_l_list_write];
  c_unify.nstru =
    _YAP_opcount[_unify_struct] +
    _YAP_opcount[_unify_l_struc] +
    _YAP_opcount[_unify_struct_write] +
    _YAP_opcount[_unify_l_struc_write];
  c_unify.nmisc =
    _YAP_opcount[_pop] +
    _YAP_opcount[_pop_n];

  unifies = c_unify.nxvar + c_unify.nyvar + c_unify.nxval + c_unify.nyval +
    c_unify.ncons + c_unify.nlist + c_unify.nstru + c_unify.nmisc;

  c_put.nxvar =
    _YAP_opcount[_put_x_var];
  c_put.nyvar =
    _YAP_opcount[_put_y_var];
  c_put.nxval =
    _YAP_opcount[_put_x_val];
  c_put.nyval =
    _YAP_opcount[_put_y_val];
  c_put.ncons =
    _YAP_opcount[_put_atom];
  c_put.nlist =
    _YAP_opcount[_put_list];
  c_put.nstru =
    _YAP_opcount[_put_struct];

  puts = c_put.nxvar + c_put.nyvar + c_put.nxval + c_put.nyval +
    c_put.ncons + c_put.nlist + c_put.nstru;

  c_write.nxvar =
    _YAP_opcount[_write_x_var] +
    _YAP_opcount[_write_void] +
    _YAP_opcount[_write_n_voids];
  c_write.nyvar =
    _YAP_opcount[_write_y_var];
  c_write.nxval =
    _YAP_opcount[_write_x_val];
  c_write.nyval =
    _YAP_opcount[_write_y_val];
  c_write.ncons =
    _YAP_opcount[_write_atom];
  c_write.nlist =
    _YAP_opcount[_write_list];
  c_write.nstru =
    _YAP_opcount[_write_struct];

  writes = c_write.nxvar + c_write.nyvar + c_write.nxval + c_write.nyval +
    c_write.ncons + c_write.nlist + c_write.nstru;

  c_control.nexecs =
    _YAP_opcount[_execute] +
    _YAP_opcount[_dexecute];

  c_control.ncalls =
    _YAP_opcount[_call] +
    _YAP_opcount[_fcall];

  c_control.nproceeds =
    _YAP_opcount[_procceed];

  c_control.ncallbips =
    _YAP_opcount[_call_cpred] +
    _YAP_opcount[_call_c_wfail] +
    _YAP_opcount[_try_c] +
    _YAP_opcount[_retry_c] +
    _YAP_opcount[_op_fail] +
    _YAP_opcount[_trust_fail] +
    _YAP_opcount[_p_atom_x] +
    _YAP_opcount[_p_atom_y] +
    _YAP_opcount[_p_atomic_x] +
    _YAP_opcount[_p_atomic_y] +
    _YAP_opcount[_p_compound_x] +
    _YAP_opcount[_p_compound_y] +
    _YAP_opcount[_p_float_x] +
    _YAP_opcount[_p_float_y] +
    _YAP_opcount[_p_integer_x] +
    _YAP_opcount[_p_integer_y] +
    _YAP_opcount[_p_nonvar_x] +
    _YAP_opcount[_p_nonvar_y] +
    _YAP_opcount[_p_number_x] +
    _YAP_opcount[_p_number_y] +
    _YAP_opcount[_p_var_x] +
    _YAP_opcount[_p_var_y] +
    _YAP_opcount[_p_db_ref_x] +
    _YAP_opcount[_p_db_ref_y] +
    _YAP_opcount[_p_cut_by_x] +
    _YAP_opcount[_p_cut_by_y] +
    _YAP_opcount[_p_primitive_x] +
    _YAP_opcount[_p_primitive_y] +
    _YAP_opcount[_p_equal] +
    _YAP_opcount[_p_plus_vv] +
    _YAP_opcount[_p_plus_vc] +
    _YAP_opcount[_p_plus_y_vv] +
    _YAP_opcount[_p_plus_y_vc] +
    _YAP_opcount[_p_minus_vv] +
    _YAP_opcount[_p_minus_cv] +
    _YAP_opcount[_p_minus_y_vv] +
    _YAP_opcount[_p_minus_y_cv] +
    _YAP_opcount[_p_times_vv] +
    _YAP_opcount[_p_times_vc] +
    _YAP_opcount[_p_times_y_vv] +
    _YAP_opcount[_p_times_y_vc] +
    _YAP_opcount[_p_div_vv] +
    _YAP_opcount[_p_div_vc] +
    _YAP_opcount[_p_div_cv] +
    _YAP_opcount[_p_div_y_vv] +
    _YAP_opcount[_p_div_y_vc] +
    _YAP_opcount[_p_div_y_cv] +
    _YAP_opcount[_p_or_vv] +
    _YAP_opcount[_p_or_vc] +
    _YAP_opcount[_p_or_y_vv] +
    _YAP_opcount[_p_or_y_vc] +
    _YAP_opcount[_p_and_vv] +
    _YAP_opcount[_p_and_vc] +
    _YAP_opcount[_p_and_y_vv] +
    _YAP_opcount[_p_and_y_vc] +
    _YAP_opcount[_p_sll_vv] +
    _YAP_opcount[_p_sll_vc] +
    _YAP_opcount[_p_sll_y_vv] +
    _YAP_opcount[_p_sll_y_vc] +
    _YAP_opcount[_p_slr_vv] +
    _YAP_opcount[_p_slr_vc] +
    _YAP_opcount[_p_slr_y_vv] +
    _YAP_opcount[_p_slr_y_vc] +
    _YAP_opcount[_p_dif] +
    _YAP_opcount[_p_eq] +
    _YAP_opcount[_p_arg_vv] +
    _YAP_opcount[_p_arg_cv] +
    _YAP_opcount[_p_arg_y_vv] +
    _YAP_opcount[_p_arg_y_cv] +
    _YAP_opcount[_p_functor];
    _YAP_opcount[_p_func2s_vv] +
    _YAP_opcount[_p_func2s_cv] +
    _YAP_opcount[_p_func2s_vc] +
    _YAP_opcount[_p_func2s_y_vv] +
    _YAP_opcount[_p_func2s_y_cv] +
    _YAP_opcount[_p_func2s_y_vc] +
    _YAP_opcount[_p_func2f_xx] +
    _YAP_opcount[_p_func2f_xy] +
    _YAP_opcount[_p_func2f_yx] +
    _YAP_opcount[_p_func2f_yy];

  c_control.ncuts =
    _YAP_opcount[_cut] +
    _YAP_opcount[_cut_t] +
    _YAP_opcount[_cut_e] +
    _YAP_opcount[_comit_b_x] +
    _YAP_opcount[_comit_b_y];

  c_control.nallocs =
    _YAP_opcount[_allocate] +
    _YAP_opcount[_fcall];

  c_control.ndeallocs =
    _YAP_opcount[_dexecute] +
    _YAP_opcount[_deallocate];

  controls =
    c_control.nexecs +
    c_control.ncalls +
    c_control.nproceeds +
    c_control.ncuts +
    c_control.nallocs +
    c_control.ndeallocs +
    _YAP_opcount[_jump] +
    _YAP_opcount[_move_back] +
    _YAP_opcount[_try_in];



  c_cp.ntries =
    _YAP_opcount[_try_me] +
    _YAP_opcount[_try_me0] +
    _YAP_opcount[_try_me1] +
    _YAP_opcount[_try_me2] +
    _YAP_opcount[_try_me3] +
    _YAP_opcount[_try_me4] +
    _YAP_opcount[_try_and_mark] +
    _YAP_opcount[_try_c] +
    _YAP_opcount[_try_clause] +
    _YAP_opcount[_either];

  c_cp.nretries =
    _YAP_opcount[_retry_me] +
    _YAP_opcount[_retry_me0] +
    _YAP_opcount[_retry_me1] +
    _YAP_opcount[_retry_me2] +
    _YAP_opcount[_retry_me3] +
    _YAP_opcount[_retry_me4] +
    _YAP_opcount[_retry_and_mark] +
    _YAP_opcount[_retry_c] +
    _YAP_opcount[_retry] +
    _YAP_opcount[_trust_in] +
    _YAP_opcount[_retry_first] +
    _YAP_opcount[_trust_first_in] +
    _YAP_opcount[_retry_tail] +
    _YAP_opcount[_trust_tail_in] +
    _YAP_opcount[_retry_head] +
    _YAP_opcount[_trust_head_in] +
    _YAP_opcount[_or_else];

  c_cp.ntrusts =
    _YAP_opcount[_trust_me] +
    _YAP_opcount[_trust_me0] +
    _YAP_opcount[_trust_me1] +
    _YAP_opcount[_trust_me2] +
    _YAP_opcount[_trust_me3] +
    _YAP_opcount[_trust_me4] +
    _YAP_opcount[_trust] +
    _YAP_opcount[_trust_first] +
    _YAP_opcount[_trust_tail] +
    _YAP_opcount[_trust_head] +
    _YAP_opcount[_or_last];

  choice_pts =
    c_cp.ntries +
    c_cp.nretries +
    c_cp.ntrusts;

  indexes =
    _YAP_opcount[_jump_if_var] +
    _YAP_opcount[_switch_on_type] +
    _YAP_opcount[_switch_on_nonv] +
    _YAP_opcount[_switch_last] +
    _YAP_opcount[_switch_on_head] +
    _YAP_opcount[_switch_list_nl] +
    _YAP_opcount[_switch_list_nl_prefetch] +
    _YAP_opcount[_switch_nv_list] +
    _YAP_opcount[_switch_l_list] +
    _YAP_opcount[_switch_on_cons] +
    _YAP_opcount[_go_on_cons] +
    _YAP_opcount[_if_cons] +
    _YAP_opcount[_switch_on_func] +
    _YAP_opcount[_go_on_func] +
    _YAP_opcount[_if_func] +
    _YAP_opcount[_if_not_then];
  misc =
    c_control.ncallbips +
    _YAP_opcount[_Ystop] +
    _YAP_opcount[_Nstop] +
    _YAP_opcount[_index_pred] +
    _YAP_opcount[_save_b_x] +
    _YAP_opcount[_save_b_y] +
    _YAP_opcount[_undef_p] +
    _YAP_opcount[_spy_pred] +
    _YAP_opcount[_spy_or_trymark] +
    _YAP_opcount[_save_pair_x] +
    _YAP_opcount[_save_pair_y] +
    _YAP_opcount[_save_pair_x_write] +
    _YAP_opcount[_save_pair_y_write] +
    _YAP_opcount[_save_appl_x] +
    _YAP_opcount[_save_appl_y] +
    _YAP_opcount[_save_appl_x_write] +
    _YAP_opcount[_save_appl_y_write];
  total = gets + unifies + puts + writes + controls + choice_pts + indexes + misc;

  /*  for (i = 0; i <= _std_top; ++i)
   * print_instruction(i);
   */

  fprintf(_YAP_stderr, "\n Instructions Executed in %s\n", program);
  fprintf(_YAP_stderr, "Groups are\n\n");
  fprintf(_YAP_stderr, "  GET               instructions: %8d (%3d%%)\n", gets,
	     (gets * 100) / total);
  fprintf(_YAP_stderr, "  UNIFY             instructions: %8d (%3d%%)\n", unifies,
	     (unifies * 100) / total);
  fprintf(_YAP_stderr, "  PUT               instructions: %8d (%3d%%)\n", puts,
	     (puts * 100) / total);
  fprintf(_YAP_stderr, "  WRITE             instructions: %8d (%3d%%)\n", writes,
	     (writes * 100) / total);
  fprintf(_YAP_stderr, "  CONTROL           instructions: %8d (%3d%%)\n", controls,
	     (controls * 100) / total);
  fprintf(_YAP_stderr, "  CHOICE POINT      instructions: %8d (%3d%%)\n", choice_pts,
	     (choice_pts * 100) / total);
  fprintf(_YAP_stderr, "  INDEXING          instructions: %8d (%3d%%)\n", indexes,
	     (indexes * 100) / total);
  fprintf(_YAP_stderr, "  MISCELLANEOUS     instructions: %8d (%3d%%)\n", misc,
	     (misc * 100) / total);
  fprintf(_YAP_stderr, "_______________________________________________\n");
  fprintf(_YAP_stderr, "   TOTAL            instructions: %8d (%3d%%)\n\n", total,
	     (total * 100) / total);

  fprintf(_YAP_stderr, "\n Analysis of Unification Instructions in %s \n", program);
  fprintf(_YAP_stderr, "           XVAR,   YVAR,    XVAL,    YVAL,     CONS,     LIST,  STRUCT\n");
  fprintf(_YAP_stderr, "  GET: %8d %8d %8d %8d %8d %8d %8d\n",
	     c_get.nxvar,
	     c_get.nyvar,
	     c_get.nxval,
	     c_get.nyval,
	     c_get.ncons,
	     c_get.nlist,
	     c_get.nstru);
  fprintf(_YAP_stderr, "UNIFY: %8d %8d %8d %8d %8d %8d %8d\n",
	     c_unify.nxvar,
	     c_unify.nyvar,
	     c_unify.nxval,
	     c_unify.nyval,
	     c_unify.ncons,
	     c_unify.nlist,
	     c_unify.nstru);
  fprintf(_YAP_stderr, "  PUT: %8d %8d %8d %8d %8d %8d %8d\n",
	     c_put.nxvar,
	     c_put.nyvar,
	     c_put.nxval,
	     c_put.nyval,
	     c_put.ncons,
	     c_put.nlist,
	     c_put.nstru);
  fprintf(_YAP_stderr, "WRITE: %8d %8d %8d %8d %8d %8d %8d\n",
	     c_write.nxvar,
	     c_write.nyvar,
	     c_write.nxval,
	     c_write.nyval,
	     c_write.ncons,
	     c_write.nlist,
	     c_write.nstru);
  fprintf(_YAP_stderr, "      ___________________________________________________\n");
  fprintf(_YAP_stderr, "TOTAL: %8d %8d %8d %8d %8d %8d %8d\n",
	     c_get.nxvar + c_unify.nxvar + c_put.nxvar + c_write.nxvar,
	     c_get.nyvar + c_unify.nyvar + c_put.nyvar + c_write.nyvar,
	     c_get.nxval + c_unify.nxval + c_put.nxval + c_write.nxval,
	     c_get.nyval + c_unify.nyval + c_put.nyval + c_write.nyval,
	     c_get.ncons + c_unify.ncons + c_put.ncons + c_write.ncons,
	     c_get.nlist + c_unify.nlist + c_put.nlist + c_write.nlist,
	     c_get.nstru + c_unify.nstru + c_put.nstru + c_write.nstru
    );

  fprintf(_YAP_stderr, "\n Analysis of Unification Instructions in %s \n", program);
  fprintf(_YAP_stderr, "           XVAR,   YVAR,    XVAL,    YVAL,     CONS,     LIST,  STRUCT\n");
  fprintf(_YAP_stderr, "  GET:  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%\n",
	     (((double) c_get.nxvar) * 100) / total,
	     (((double) c_get.nyvar) * 100) / total,
	     (((double) c_get.nxval) * 100) / total,
	     (((double) c_get.nyval) * 100) / total,
	     (((double) c_get.ncons) * 100) / total,
	     (((double) c_get.nlist) * 100) / total,
	     (((double) c_get.nstru) * 100) / total);
  fprintf(_YAP_stderr, "UNIFY:  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%\n",
	     (((double) c_unify.nxvar) * 100) / total,
	     (((double) c_unify.nyvar) * 100) / total,
	     (((double) c_unify.nxval) * 100) / total,
	     (((double) c_unify.nyval) * 100) / total,
	     (((double) c_unify.ncons) * 100) / total,
	     (((double) c_unify.nlist) * 100) / total,
	     (((double) c_unify.nstru) * 100) / total);
  fprintf(_YAP_stderr, "  PUT:  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%\n",
	     (((double) c_put.nxvar) * 100) / total,
	     (((double) c_put.nyvar) * 100) / total,
	     (((double) c_put.nxval) * 100) / total,
	     (((double) c_put.nyval) * 100) / total,
	     (((double) c_put.ncons) * 100) / total,
	     (((double) c_put.nlist) * 100) / total,
	     (((double) c_put.nstru) * 100) / total);
  fprintf(_YAP_stderr, "WRITE:  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%\n",
	     (((double) c_write.nxvar) * 100) / total,
	     (((double) c_write.nyvar) * 100) / total,
	     (((double) c_write.nxval) * 100) / total,
	     (((double) c_write.nyval) * 100) / total,
	     (((double) c_write.ncons) * 100) / total,
	     (((double) c_write.nlist) * 100) / total,
	     (((double) c_write.nstru) * 100) / total);
  fprintf(_YAP_stderr, "      ___________________________________________________\n");
  fprintf(_YAP_stderr, "TOTAL:  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%  %3.2f%%\n",
	     (((double) c_get.nxvar + c_unify.nxvar + c_put.nxvar + c_write.nxvar) * 100) / total,
	     (((double) c_get.nyvar + c_unify.nyvar + c_put.nyvar + c_write.nyvar) * 100) / total,
	     (((double) c_get.nxval + c_unify.nxval + c_put.nxval + c_write.nxval) * 100) / total,
	     (((double) c_get.nyval + c_unify.nyval + c_put.nyval + c_write.nyval) * 100) / total,
	     (((double) c_get.ncons + c_unify.ncons + c_put.ncons + c_write.ncons) * 100) / total,
	     (((double) c_get.nlist + c_unify.nlist + c_put.nlist + c_write.nlist) * 100) / total,
	     (((double) c_get.nstru + c_unify.nstru + c_put.nstru + c_write.nstru) * 100) / total
    );

  fprintf(_YAP_stderr, "\n Control Instructions Executed in %s \n", program);
  fprintf(_YAP_stderr, "Grouped as\n\n");
  fprintf(_YAP_stderr, "  CALL              instructions: %8d (%3d%%)\n",
	     c_control.ncalls, (c_control.ncalls * 100) / total);
  fprintf(_YAP_stderr, "  PROCEED           instructions: %8d (%3d%%)\n",
	     c_control.nproceeds, (c_control.nproceeds * 100) / total);
  fprintf(_YAP_stderr, "  EXECUTE           instructions: %8d (%3d%%)\n",
	     c_control.nexecs, (c_control.nexecs * 100) / total);
  fprintf(_YAP_stderr, "  CUT               instructions: %8d (%3d%%)\n",
	     c_control.ncuts, (c_control.ncuts * 100) / total);
  fprintf(_YAP_stderr, "  CALL_BIP          instructions: %8d (%3d%%)\n",
	     c_control.ncallbips, (c_control.ncallbips * 100) / total);
  fprintf(_YAP_stderr, "  ALLOCATE          instructions: %8d (%3d%%)\n",
	     c_control.nallocs, (c_control.nallocs * 100) / total);
  fprintf(_YAP_stderr, "  DEALLOCATE        instructions: %8d (%3d%%)\n",
	     c_control.ndeallocs, (c_control.ndeallocs * 100) / total);
  fprintf(_YAP_stderr, "_______________________________________________\n");
  fprintf(_YAP_stderr, "   TOTAL            instructions: %8d (%3d%%)\n\n", total,
	     (total * 100) / total);

  fprintf(_YAP_stderr, "\n Choice Point Manipulation Instructions Executed in %s \n", program);
  fprintf(_YAP_stderr, "Grouped as\n\n");
  fprintf(_YAP_stderr, "  TRY              instructions: %8d (%3d%%)\n",
	     c_cp.ntries, (c_cp.ntries * 100) / total);
  fprintf(_YAP_stderr, "  RETRY            instructions: %8d (%3d%%)\n",
	     c_cp.nretries, (c_cp.nretries * 100) / total);
  fprintf(_YAP_stderr, "  TRUST            instructions: %8d (%3d%%)\n",
	     c_cp.ntrusts, (c_cp.ntrusts * 100) / total);
  fprintf(_YAP_stderr, "_______________________________________________\n");
  fprintf(_YAP_stderr, "   TOTAL            instructions: %8d (%3d%%)\n\n", total,
	     (total * 100) / total);

  return (TRUE);
}

void 
_YAP_InitAnalystPreds(void)
{
  _YAP_InitCPred("reset_op_counters", 0, p_reset_op_counters, SafePredFlag |SyncPredFlag);
  _YAP_InitCPred("show_op_counters", 1, p_show_op_counters, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("show_ops_by_group", 1, p_show_ops_by_group, SafePredFlag |SyncPredFlag);

}

#endif /* ANALYST */
