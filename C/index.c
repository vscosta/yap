/*************************************************************************
*									 *
*	 Yap Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		index.c							 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Indexing a Prolog predicate				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

/*
 * This file compiles and removes the indexation code for the prolog compiler 
 *
 * Some remarks: *try_me always point to inside the code;
 * try always points to outside 
 *

 Algorithm:

 - fetch info on all clauses
 - if #clauses =1  return
 - compute groups:
    seq of variable only clauses
    seq: of one or more type instructions
         bound clauses
 - sort group
 - select constant
          --> type instructions
          --> count constants
          --> switch
	       for all arguments:
	       select new argument 

 */

#include "Yap.h"
#include "compile.h"
#include "clause.h"
#include "index.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#ifdef DEBUG
#include "yapio.h"
#endif
#ifndef NULL
#define NULL (void *)0
#endif
#if HAVE_STRING_H
#include <string.h>
#endif

UInt STATIC_PROTO(do_index, (ClauseDef *,ClauseDef *,PredEntry *,UInt,UInt,int,int,CELL *));
UInt STATIC_PROTO(do_compound_index, (ClauseDef *,ClauseDef *,PredEntry *,UInt,UInt,UInt,int,int,int,CELL *));

static UInt labelno;

static inline int
smaller(Term t1, Term t2)
{
  if (IsVarTerm(t1)) {
    if (!IsVarTerm(t2)) return TRUE;
    return (t1 < t2);
  } else if (IsIntTerm(t1)) {
    if (IsVarTerm(t2)) return FALSE;
    if (!IsIntTerm(t2)) return TRUE;
    return (IntOfTerm(t1) < IntOfTerm(t2));
  } else if (IsAtomTerm(t1)) {
    if (IsVarTerm(t2) || IsIntTerm(t2)) return FALSE;
    if (IsApplTerm(t2) || IsPairTerm(t2)) return TRUE;
    return (t1 < t2);
  } else if (IsApplTerm(t1)) {
    if (IsVarTerm(t2) || IsAtomTerm(t2) || IsIntTerm(t2)) return FALSE;
    if (IsPairTerm(t2)) return TRUE;
    return (t1 < t2);
  } else /* if (IsPairTerm(t1)) */ {
    return FALSE;
  }
}

static inline void
clcpy(ClauseDef *d, ClauseDef *s)
{
  memcpy((void *)d, (void *)s, sizeof(ClauseDef));
}

/*
  original code from  In Hyuk Choi,
  found at http://userpages.umbc.edu/~ichoi1/project/cs441.htm
*/

static inline void
exchange(ClauseDef b[], Int i, Int j)
{
  ClauseDef t;

  clcpy(&t, b+j);
  clcpy(b+j, b+i);
  clcpy(b+i, &t);
}

static UInt
partition(ClauseDef a[], Int p, Int r)
{ 
  Term x;
  UInt i, j;

  x = a[p].Tag;
  i = p+1;
  j = r;

  while (smaller(x,a[j].Tag) && i < j) {
    j--;
  }
  while (smaller(a[i].Tag, x) && i < j) {
    i++;
  }
  while(i < j) {
    exchange(a, i, j);
    i++;
    j--;
    while (smaller(x, a[j].Tag) && i < j) {
      j--;
    }
    while (smaller(a[i].Tag, x) && i < j) {
      i++;
    }
  }
  if (smaller(x, a[i].Tag))
    i--;
  exchange(a, p, i);
  return(i);
}

static void
insort(ClauseDef a[], Int p, Int q)
{
  Int j;
    
  for (j = p+1; j <= q; j ++) {
    ClauseDef key;
    Int i;

    clcpy(&key, a+j);
    i = j;
	 
    while (i > p && smaller(key.Tag,a[i-1].Tag)) {
      clcpy(a+i, a+(i-1));
      i --;
    }
    clcpy(a+i, &key);
  }
}


static void
quicksort(ClauseDef a[], Int p, Int r)
{ 
  Int q;
  if (p < r) {
    if (r - p < 100) {
      insort(a, p, r);
      return;
    }
    exchange(a, p, (p+r)/2);
    q = partition (a, p, r);  
    quicksort(a, p, q-1);
    quicksort(a, q + 1, r);
  }
}

/* sort a group of clauses by using their tags */
static void
sort_group(GroupDef *grp)
{
  quicksort(grp->FirstClause, 0, grp->LastClause-grp->FirstClause);
}

/* add copy to register stack for original reg */
static int
add_regcopy(wamreg regs[MAX_REG_COPIES], int regs_count, wamreg copy)
{
  if (regs_count == MAX_REG_COPIES) {
    regs[0] = copy;
  }
  regs[regs_count] = copy;
  return regs_count+1;
}

/* add copy to register stack for original reg */
static int
delete_regcopy(wamreg regs[MAX_REG_COPIES], int regs_count, wamreg copy)
{
  int i = 0;
  while (i < regs_count) {
    if (regs[i] == copy) {
      /* we found it */
      regs[i] = regs[MAX_REG_COPIES-1];
      return regs_count-1;
    }
    i++;
  }
  /* this copy had overflowed */
  return regs_count;
}

/* add copy to register stack for original reg */
inline static int
regcopy_in(wamreg regs[MAX_REG_COPIES], int regs_count, wamreg copy)
{
  int i = 0;
  while (i < regs_count) {
    if (regs[i] == copy) {
      return TRUE;
    }
    i++;
  }
  /* this copy could not be found */
  return FALSE;
}

/* Restores a prolog clause, in its compiled form */
static int 
has_cut(yamop *pc)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not 
 */
{
  do {
    op_numbers op = Yap_op_from_opcode(pc->opc);
    pc->opc = Yap_opcode(op);
    switch (op) {
    case _Ystop:
    case _Nstop:
      return FALSE;
      /* instructions type ld */
    case _cut:
    case _cut_t:
    case _cut_e:
    case _p_cut_by_y:
    case _p_cut_by_x:
    case _comit_b_y:
    case _comit_b_x:
      return TRUE;
    case _try_me:
    case _retry_me:
    case _trust_me:
    case _profiled_retry_me:
    case _profiled_trust_me:
    case _count_retry_me:
    case _count_trust_me:
    case _try_me0:
    case _retry_me0:
    case _trust_me0:
    case _try_me1:
    case _retry_me1:
    case _trust_me1:
    case _try_me2:
    case _retry_me2:
    case _trust_me2:
    case _try_me3:
    case _retry_me3:
    case _trust_me3:
    case _try_me4:
    case _retry_me4:
    case _trust_me4:
    case _spy_or_trymark:
    case _try_and_mark:
    case _profiled_retry_and_mark:
    case _count_retry_and_mark:
    case _retry_and_mark:
    case _try_clause:
    case _retry:
    case _trust:
#ifdef YAPOR
    case _getwork:
    case _getwork_seq:
    case _sync:
#endif
#ifdef TABLING
    case _table_try_me_single:
    case _table_try_me:
    case _table_retry_me:
    case _table_trust_me:
    case _table_answer_resolution:
    case _table_completion:
#endif
      pc = NEXTOP(pc,ld);
      break;
      /* instructions type l */
    case _enter_profiling:
    case _count_call:
    case _retry_profiled:
    case _count_retry:
    case _try_logical_pred:
    case _trust_logical_pred:
    case _execute:
    case _dexecute:
    case _jump:
    case _move_back:
    case _skip:
    case _jump_if_var:
    case _try_in:
      pc = NEXTOP(pc,l);
      break;
      /* instructions type EC */
    case _alloc_for_logical_pred:
      pc = NEXTOP(pc,EC);
      break;
      /* instructions type e */
    case _trust_fail:
    case _op_fail:
    case _procceed:
    case _allocate:
    case _deallocate:
    case _write_void:
    case _write_list:
    case _write_l_list:
#if !defined(YAPOR)
    case _or_last:
#endif
    case _pop:
    case _index_pred:
    case _undef_p:
    case _spy_pred:
    case _p_equal:
    case _p_dif:
    case _p_eq:
    case _p_functor:
    case _p_execute_tail:
    case _enter_a_profiling:
    case _count_a_call:
#ifdef YAPOR
    case _getwork_first_time:
#endif
#ifdef TABLING
    case _trie_do_var:
    case _trie_trust_var:
    case _trie_try_var:
    case _trie_retry_var:
    case _trie_do_val:
    case _trie_trust_val:
    case _trie_try_val:
    case _trie_retry_val:
    case _trie_do_atom:
    case _trie_trust_atom:
    case _trie_try_atom:
    case _trie_retry_atom:
    case _trie_do_list:
    case _trie_trust_list:
    case _trie_try_list:
    case _trie_retry_list:
    case _trie_do_struct:
    case _trie_trust_struct:
    case _trie_try_struct:
    case _trie_retry_struct:
#endif
      pc = NEXTOP(pc,e);
      break;
      /* instructions type x */
    case _save_b_x:
    case _get_list:
    case _put_list:
    case _write_x_var:
    case _write_x_val:
    case _write_x_loc:
    case _p_atom_x:
    case _p_atomic_x:
    case _p_integer_x:
    case _p_nonvar_x:
    case _p_number_x:
    case _p_var_x:
    case _p_db_ref_x:
    case _p_primitive_x:
    case _p_compound_x:
    case _p_float_x:
      pc = NEXTOP(pc,x);
      break;
      /* instructions type y */
    case _save_b_y:
    case _write_y_var:
    case _write_y_val: 
    case _write_y_loc:
    case _p_atom_y:
    case _p_atomic_y:
    case _p_integer_y:
    case _p_nonvar_y:
    case _p_number_y:
    case _p_var_y:
    case _p_db_ref_y:
    case _p_primitive_y:
    case _p_compound_y:
    case _p_float_y:
      pc = NEXTOP(pc,y);
      break;
      /* instructions type sla */
    case _p_execute:
    case _fcall:
    case _call:
#ifdef YAPOR
    case _or_last:
#endif
      pc = NEXTOP(pc,sla);
      break;
      /* instructions type sla, but for disjunctions */
    case _either:
    case _or_else:
      pc = NEXTOP(pc,sla);
      break;
      /* instructions type sla, but for functions */
    case _call_cpred:
    case _call_usercpred:
      pc = NEXTOP(pc,sla);
      break;
      /* instructions type xx */
    case _get_x_var:
    case _get_x_val:
    case _glist_valx:
    case _gl_void_varx:
    case _gl_void_valx:
    case _put_x_var:
    case _put_x_val:
      pc = NEXTOP(pc,xx);
      break;
      /* instructions type yx */
    case _get_y_var:
    case _get_y_val:
    case _put_y_var:
    case _put_y_val:
    case _put_unsafe:
      pc = NEXTOP(pc,yx);
      break;
      /* instructions type xc */
    case _get_atom:
    case _put_atom:
    case _get_float:
    case _get_longint:
    case _get_bigint:
      pc = NEXTOP(pc,xc);
      break;
      /* instructions type xf */
    case _get_struct:
    case _put_struct:
      pc = NEXTOP(pc,xf);
      break;
      /* instructions type xy */
    case _glist_valy:
    case _gl_void_vary:
    case _gl_void_valy:
      pc = NEXTOP(pc,xy);
      break;
      /* instructions type ox */
    case _unify_x_var:
    case _unify_x_var_write:
    case _unify_l_x_var:
    case _unify_l_x_var_write:
    case _unify_x_val_write:
    case _unify_x_val:
    case _unify_l_x_val_write:
    case _unify_l_x_val:
    case _unify_x_loc_write:
    case _unify_x_loc:
    case _unify_l_x_loc_write:
    case _unify_l_x_loc:
    case _save_pair_x_write:
    case _save_pair_x:
    case _save_appl_x_write:
    case _save_appl_x:
      pc = NEXTOP(pc,ox);
      break;
      /* instructions type oxx */
    case _unify_x_var2:
    case _unify_x_var2_write:
    case _unify_l_x_var2:
    case _unify_l_x_var2_write:
      pc = NEXTOP(pc,oxx);
      break;
      /* instructions type oy */
    case _unify_y_var:
    case _unify_y_var_write:
    case _unify_l_y_var:
    case _unify_l_y_var_write:
    case _unify_y_val_write:
    case _unify_y_val:
    case _unify_l_y_val_write:
    case _unify_l_y_val:
    case _unify_y_loc_write:
    case _unify_y_loc:
    case _unify_l_y_loc_write:
    case _unify_l_y_loc:
    case _save_pair_y_write:
    case _save_pair_y:
    case _save_appl_y_write:
    case _save_appl_y:
      pc = NEXTOP(pc,oy);
      break;
      /* instructions type o */
    case _unify_void_write:
    case _unify_void:
    case _unify_l_void_write:
    case _unify_l_void:
    case _unify_list_write:
    case _unify_list:
    case _unify_l_list_write:
    case _unify_l_list:
      pc = NEXTOP(pc,o);
      break;
      /* instructions type os */
    case _unify_n_voids_write:
    case _unify_n_voids:
    case _unify_l_n_voids_write:
    case _unify_l_n_voids:
      pc = NEXTOP(pc,os);
      break;
      /* instructions type oc */
    case _unify_atom_write:
    case _unify_atom:
    case _unify_l_atom_write:
    case _unify_l_atom:
    case _unify_float:
    case _unify_l_float:
    case _unify_longint:
    case _unify_l_longint:
    case _unify_bigint:
    case _unify_l_bigint:
      pc = NEXTOP(pc,oc);
      break;
      /* instructions type osc */
    case _unify_n_atoms_write:
    case _unify_n_atoms:
      pc = NEXTOP(pc,osc);
      break;
      /* instructions type of */
    case _unify_struct_write:
    case _unify_struct:
    case _unify_l_struc_write:
    case _unify_l_struc:
      pc = NEXTOP(pc,of);
      break;
      /* instructions type s */
    case _write_n_voids:
    case _pop_n:
#ifdef TABLING
    case _table_new_answer:
#endif
      pc = NEXTOP(pc,s);
      break;
      /* instructions type ps */
   case _write_atom:
      pc = NEXTOP(pc,c);
      break;
      /* instructions type sc */
   case _write_n_atoms:
      pc = NEXTOP(pc,sc);
      break;
      /* instructions type f */
   case _write_struct:
   case _write_l_struc:
      pc = NEXTOP(pc,f);
      break;
      /* instructions type sdl */
    case _call_c_wfail:
      pc = NEXTOP(pc,sdl);
      break;
      /* instructions type lds */
    case _try_c:
    case _try_userc:
      pc = NEXTOP(pc,lds);
      break;
    case _retry_c:
    case _retry_userc:
      pc = NEXTOP(pc,lds);
      break;
      /* instructions type llll */
    case _switch_on_type:
      pc = NEXTOP(pc,llll);
      break;
    case _switch_list_nl:
      pc = NEXTOP(pc,ollll);
      break;
    case _switch_on_arg_type:
      pc = NEXTOP(pc,xllll);
      break;
    case _switch_on_sub_arg_type:
      pc = NEXTOP(pc,sllll);
      break;
      /* instructions type lll */
      /* instructions type cll */
    case _if_not_then:
      pc = NEXTOP(pc,cll);
      break;
      /* instructions type ollll */
    case _switch_on_func:
    case _switch_on_cons:
    case _if_func:
    case _if_cons:
      {
	int             i;
	CELL            *startcode;

	i = pc->u.s.s;
	startcode = (CELL *)NEXTOP(pc,s);
	pc = (yamop *)(startcode+2*i);
      }
      break;
    case _go_on_func:
      pc = NEXTOP(pc,fll);
      break;
      /* instructions type cll */
    case _go_on_cons:
      pc = NEXTOP(pc,cll);
      break;
      /* instructions type xxx */
    case _p_plus_vv:
    case _p_minus_vv:
    case _p_times_vv:
    case _p_div_vv:
    case _p_and_vv:
    case _p_or_vv:
    case _p_sll_vv:
    case _p_slr_vv:
    case _p_arg_vv:
    case _p_func2s_vv:
    case _p_func2f_xx:
      pc = NEXTOP(pc,xxx);
      break;
      /* instructions type xxc */
    case _p_plus_vc:
    case _p_minus_cv:
    case _p_times_vc:
    case _p_div_cv:
    case _p_and_vc:
    case _p_or_vc:
    case _p_sll_vc:
    case _p_slr_vc:
    case _p_func2s_vc:
      pc = NEXTOP(pc,xxc);
      break;
    case _p_div_vc:
    case _p_sll_cv:
    case _p_slr_cv:
    case _p_arg_cv:
      pc = NEXTOP(pc,xcx);
      break;
    case _p_func2s_cv:
      pc = NEXTOP(pc,xcx);
      break;
      /* instructions type xyx */
    case _p_func2f_xy:
      pc = NEXTOP(pc,xyx);
      break;
      /* instructions type yxx */
    case _p_plus_y_vv:
    case _p_minus_y_vv:
    case _p_times_y_vv:
    case _p_div_y_vv:
    case _p_and_y_vv:
    case _p_or_y_vv:
    case _p_sll_y_vv:
    case _p_slr_y_vv:
    case _p_arg_y_vv:
    case _p_func2s_y_vv:
    case _p_func2f_yx:
      pc = NEXTOP(pc,yxx);
      break;
      /* instructions type yyx */
    case _p_func2f_yy:
      pc = NEXTOP(pc,yyx);
      break;
      /* instructions type yxc */
    case _p_plus_y_vc:
    case _p_minus_y_cv:
    case _p_times_y_vc:
    case _p_div_y_vc:
    case _p_div_y_cv:
    case _p_and_y_vc:
    case _p_or_y_vc:
    case _p_sll_y_vc:
    case _p_slr_y_vc:
    case _p_func2s_y_vc:
      pc = NEXTOP(pc,yxc);
      break;
      /* instructions type ycx */
    case _p_sll_y_cv:
    case _p_slr_y_cv:
    case _p_arg_y_cv:
      pc = NEXTOP(pc,ycx);
      break;
      /* instructions type lxx */
    case _p_func2s_y_cv:
      pc = NEXTOP(pc,ycx);
      break;
      /* instructions type lxx */
    case _call_bfunc_xx:
      pc = NEXTOP(pc,lxx);
      break;
      /* instructions type lxy */
    case _call_bfunc_yx:
    case _call_bfunc_xy:
      pc = NEXTOP(pc,lxy);
      break;
    case _call_bfunc_yy:
      pc = NEXTOP(pc,lyy);
      break;
    }
  } while (TRUE);
}

static void 
add_info(ClauseDef *clause, UInt regno)
{
  wamreg myregs[MAX_REG_COPIES];
  int nofregs;
  yslot ycopy = 0;
  yamop *cl;
  
  nofregs = add_regcopy(myregs, 0, Yap_regnotoreg(regno));
  cl = clause->CurrentCode;
  while (TRUE) {
    op_numbers op = Yap_op_from_opcode(cl->opc);
    switch (op) {
    case _Ystop:
    case _Nstop:
    case _try_me:
    case _retry_me:
    case _trust_me:
    case _profiled_retry_me:
    case _profiled_trust_me:
    case _count_retry_me:
    case _count_trust_me:
    case _try_me0:
    case _retry_me0:
    case _trust_me0:
    case _try_me1:
    case _retry_me1:
    case _trust_me1:
    case _try_me2:
    case _retry_me2:
    case _trust_me2:
    case _try_me3:
    case _retry_me3:
    case _trust_me3:
    case _try_me4:
    case _retry_me4:
    case _trust_me4:
    case _spy_or_trymark:
    case _try_and_mark:
    case _profiled_retry_and_mark:
    case _count_retry_and_mark:
    case _retry_and_mark:
    case _try_clause:
    case _retry:
    case _trust:
#ifdef YAPOR
    case _getwork:
    case _getwork_seq:
    case _sync:
#endif
#ifdef TABLING
    case _table_try_me_single:
    case _table_try_me:
    case _table_retry_me:
    case _table_trust_me:
    case _table_answer_resolution:
    case _table_completion:
#endif
    case _enter_profiling:
    case _count_call:
    case _retry_profiled:
    case _count_retry:
    case _try_logical_pred:
    case _trust_logical_pred:
    case _execute:
    case _dexecute:
    case _jump:
    case _move_back:
    case _skip:
    case _jump_if_var:
    case _try_in:
      clause->Tag = (CELL)NULL;
      return;
    case _alloc_for_logical_pred:
      cl = NEXTOP(cl,EC);
      break;
      /* instructions type e */
    case _trust_fail:
    case _op_fail:
    case _procceed:
#if !defined(YAPOR)
    case _or_last:
#endif
    case _pop:
    case _index_pred:
    case _undef_p:
    case _spy_pred:
    case _p_equal:
    case _p_dif:
    case _p_eq:
    case _p_functor:
    case _p_execute_tail:
#ifdef YAPOR
    case _getwork_first_time:
#endif
#ifdef TABLING
    case _trie_do_var:
    case _trie_trust_var:
    case _trie_try_var:
    case _trie_retry_var:
    case _trie_do_val:
    case _trie_trust_val:
    case _trie_try_val:
    case _trie_retry_val:
    case _trie_do_atom:
    case _trie_trust_atom:
    case _trie_try_atom:
    case _trie_retry_atom:
    case _trie_do_list:
    case _trie_trust_list:
    case _trie_try_list:
    case _trie_retry_list:
    case _trie_do_struct:
    case _trie_trust_struct:
    case _trie_try_struct:
    case _trie_retry_struct:
#endif
      clause->Tag = (CELL)NULL;
      return;
    case _cut:
    case _cut_t:
    case _cut_e:
    case _allocate:
    case _deallocate:
    case _write_void:
    case _write_list:
    case _write_l_list:
    case _enter_a_profiling:
    case _count_a_call:
      cl = NEXTOP(cl,e);
      break;
    case _save_b_x:
    case _comit_b_x:
    case _p_cut_by_x:
    case _write_x_val:
    case _write_x_loc:
    case _write_x_var:
    case _put_list:
    case _p_nonvar_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _p_number_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = (_number+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _p_atomic_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = (_atomic+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _p_integer_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = (_integer+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _p_primitive_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = (_primitive+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _p_compound_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = (_compound+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _p_var_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = (_var+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _p_db_ref_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = AbsAppl((CELL *)FunctorDBRef);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _p_float_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = AbsAppl((CELL *)FunctorDouble);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _p_atom_x:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = (_atom+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _get_list:
      if (regcopy_in(myregs, nofregs, cl->u.x.x)) {
	clause->Tag = AbsPair(NULL);
	clause->WorkPC = NEXTOP(cl,x);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _save_b_y:
    case _comit_b_y:
    case _write_y_var:
    case _write_y_val: 
    case _write_y_loc:
    case _p_cut_by_y:
    case _p_nonvar_y:
      if (cl->u.y.y == ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_atomic_y:
      if (ycopy == cl->u.y.y) {
	clause->WorkPC = cl;
	clause->Tag = (_atomic+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_integer_y:
      if (ycopy == cl->u.y.y) {
	clause->WorkPC = cl;
	clause->Tag = (_integer+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_number_y:
      if (ycopy == cl->u.y.y) {
	clause->WorkPC = cl;
	clause->Tag = (_number+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_primitive_y:
      if (ycopy == cl->u.y.y) {
	clause->WorkPC = cl;
	clause->Tag = (_primitive+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_compound_y:
      if (ycopy == cl->u.y.y) {
	clause->WorkPC = cl;
	clause->Tag = (_compound+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_db_ref_y:
      if (ycopy == cl->u.y.y) {
	clause->WorkPC = cl;
	clause->Tag = AbsAppl((CELL *)FunctorDBRef);
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_float_y:
      if (ycopy == cl->u.y.y) {
	clause->WorkPC = cl;
	clause->Tag = AbsAppl((CELL *)FunctorDouble);
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_atom_y:
      if (cl->u.y.y == ycopy) {
	clause->Tag = (_atom+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_var_y:
      if (cl->u.y.y == ycopy) {
	clause->Tag = (_var+1)*sizeof(CELL);
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_execute:
    case _fcall:
    case _call:
#ifdef YAPOR
    case _or_last:
#endif
    case _either:
    case _or_else:
    case _call_cpred:
    case _call_usercpred:
      clause->Tag = (CELL)NULL;
      return;
    case _get_x_var:
      if (regcopy_in(myregs, nofregs, cl->u.xx.xr)) {
	nofregs = add_regcopy(myregs, nofregs, cl->u.xx.xl);
	break;
      }
    case _put_x_var:
      /* if the last slot I am using, get out */
      if (regcopy_in(myregs, nofregs, cl->u.xx.xl) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.xx.xl)) == 0 &&
	  !ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _get_x_val:
      /* alias two registers */
      if (regcopy_in(myregs, nofregs, cl->u.xx.xl)) {
	nofregs = add_regcopy(myregs, nofregs, cl->u.xx.xr);
      } else if (regcopy_in(myregs, nofregs, cl->u.xx.xr)) {
	nofregs = add_regcopy(myregs, nofregs, cl->u.xx.xl);
      } 
      cl = NEXTOP(cl,xx);
      break;
    case _put_x_val:
      if (regcopy_in(myregs, nofregs, cl->u.xx.xl)) {
	nofregs = add_regcopy(myregs, nofregs, cl->u.xx.xr);
      } else if (regcopy_in(myregs, nofregs, cl->u.xx.xr) &&
		 (nofregs = delete_regcopy(myregs, nofregs, cl->u.xx.xr)) == 0 &&
		 !ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _glist_valx:
    case _gl_void_varx:
    case _gl_void_valx:
      if (regcopy_in(myregs, nofregs, cl->u.xx.xl)) {
	clause->WorkPC = cl;
	clause->Tag = AbsPair(NULL);
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _get_y_var:
      if (regcopy_in(myregs, nofregs, cl->u.xx.xr)) {
	ycopy = cl->u.yx.y;
      }
    case _put_y_var:
      cl = NEXTOP(cl,yx);
      break;
    case _put_y_val:
    case _put_unsafe:
      if (regcopy_in(myregs, nofregs, cl->u.yx.x)) {
	ycopy = cl->u.yx.y;
      }
      cl = NEXTOP(cl,yx);
      break;      
    case _get_y_val:
      if (regcopy_in(myregs, nofregs, cl->u.xy.x)) {
	ycopy = cl->u.yx.y;
      } else if (ycopy == cl->u.yx.y) {
	nofregs = add_regcopy(myregs, nofregs, cl->u.xy.x);
      }
      cl = NEXTOP(cl,xy);
      break;
    case _get_atom:
      if (regcopy_in(myregs, nofregs, cl->u.xc.x)) {
	clause->Tag = cl->u.xc.c;
	return;
      } else {
	cl = NEXTOP(cl,xc);
      }
      break;
    case _get_float:
      if (regcopy_in(myregs, nofregs, cl->u.xc.x)) {
	clause->WorkPC = cl;
	clause->Tag = AbsAppl((CELL *)FunctorDouble);
	return;
      } else {
	cl = NEXTOP(cl,xc);
      }
      break;
    case _get_longint:
      if (regcopy_in(myregs, nofregs, cl->u.xc.x)) {
	clause->WorkPC = cl;
	clause->Tag = AbsAppl((CELL *)FunctorLongInt);
	return;
      } else {
	cl = NEXTOP(cl,xc);
      }
      break;
   case _get_bigint:
      if (regcopy_in(myregs, nofregs, cl->u.xc.x)) {
	clause->WorkPC = cl;
	clause->Tag = AbsAppl((CELL *)FunctorBigInt);
	return;
      } else {
	cl = NEXTOP(cl,xc);
      }
      break;
    case _put_atom:
      if (regcopy_in(myregs, nofregs, cl->u.xc.x) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.xc.x)) == 0 &&
	  !ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      } else {
	cl = NEXTOP(cl,xc);
      }
      break;
    case _get_struct:
      if (regcopy_in(myregs, nofregs, cl->u.xf.x)) {
	clause->WorkPC = NEXTOP(cl,xf);
	clause->Tag = AbsAppl((CELL *)cl->u.xf.f);
	return;
      } else {
	cl = NEXTOP(cl,xf);
      }
      break;
    case _put_struct:
      if (regcopy_in(myregs, nofregs, cl->u.xf.x) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.xf.x)) == 0 &&
	  !ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      } else {
	cl = NEXTOP(cl,xf);
      }
      break;
    case _glist_valy:
    case _gl_void_vary:
    case _gl_void_valy:
      if (regcopy_in(myregs, nofregs, cl->u.xy.x)) {
	clause->WorkPC = cl;
	clause->Tag = AbsPair(NULL);
	return;
      }
      cl = NEXTOP(cl,xy);
      break;
    case _unify_x_var:
    case _unify_x_var_write:
    case _unify_l_x_var:
    case _unify_l_x_var_write:
      if (regcopy_in(myregs, nofregs, cl->u.ox.x) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.ox.x)) == 0 &&
	  !ycopy) {
	/* we just initialised the argument, so nothing can happen now */
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_x_val_write:
    case _unify_x_val:
    case _unify_l_x_val_write:
    case _unify_l_x_val:
    case _unify_x_loc_write:
    case _unify_x_loc:
    case _unify_l_x_loc_write:
    case _unify_l_x_loc:
      /* we're just done with the head of a list, but there
	 is nothing inside.
       */
      cl = NEXTOP(cl,ox);
      break;
    case _save_pair_x_write:
    case _save_pair_x:
    case _save_appl_x_write:
    case _save_appl_x:
      if (regcopy_in(myregs, nofregs, cl->u.ox.x) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.ox.x)) == 0 &&
	  !ycopy) {
	/* we just initialised the argument, so nothing can happen now */
	clause->Tag = (CELL)NULL;
	return;	
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_x_var2:
    case _unify_x_var2_write:
    case _unify_l_x_var2:
    case _unify_l_x_var2_write:
      if (regcopy_in(myregs, nofregs, cl->u.oxx.xl) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.oxx.xl)) == 0 &&
	  !ycopy) {
	/* we just initialised the argument, so nothing can happen now */
	clause->Tag = (CELL)NULL;
	return;
      }
      if (regcopy_in(myregs, nofregs, cl->u.oxx.xr) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.oxx.xr)) == 0 &&
	  !ycopy) {
	/* we just initialised the argument, so nothing can happen now */
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oxx);
      break;
    case _unify_y_var:
    case _unify_y_var_write:
    case _unify_l_y_var:
    case _unify_l_y_var_write:
      /* we're just done with the head of a list, but there
	 is nothing inside.
       */
      if (cl->u.oy.y == ycopy) {
	ycopy = 0;	/* weird stuff, let's just reset ycopy */
	if (nofregs == 0) {
	  clause->Tag = (CELL)NULL;
	  return;
	}
      }
      cl = NEXTOP(cl,oy);
      break;
    case _unify_y_val_write:
    case _unify_y_val:
    case _unify_l_y_val_write:
    case _unify_l_y_val:
    case _unify_y_loc_write:
    case _unify_y_loc:
    case _unify_l_y_loc_write:
    case _unify_l_y_loc:
      /* we're just done with the head of a list, but there
	 is nothing inside.
       */
      cl = NEXTOP(cl,oy);
      break;
    case _save_pair_y_write:
    case _save_pair_y:
    case _save_appl_y_write:
    case _save_appl_y:
      if (cl->u.oy.y == ycopy) {
	ycopy = 0;	/* weird stuff, let's just reset ycopy */
	if (nofregs == 0) {
	  clause->Tag = (CELL)NULL;
	  return;
	}
      }
      cl = NEXTOP(cl,oy);
      break;
    case _unify_void_write:
    case _unify_void:
    case _unify_l_void_write:
    case _unify_l_void:
      /* we're just done with the head of a list, but there
	 is nothing inside.
       */
      cl = NEXTOP(cl,o);
      break;
    case _unify_list_write:
    case _unify_list:
    case _unify_l_list_write:
    case _unify_l_list:
      cl = NEXTOP(cl,o);
      break;      
    case _unify_n_voids_write:
    case _unify_n_voids:
    case _unify_l_n_voids_write:
    case _unify_l_n_voids:
      cl = NEXTOP(cl,os);
      break;      
    case _unify_atom_write:
    case _unify_atom:
    case _unify_l_atom_write:
    case _unify_l_atom:
      cl = NEXTOP(cl,oc);
      break;      
    case _unify_float:
    case _unify_l_float:
      cl = NEXTOP(cl,oc);
      break;      
    case _unify_longint:
    case _unify_l_longint:
      cl = NEXTOP(cl,oc);
      break;      
    case _unify_bigint:
    case _unify_l_bigint:
      cl = NEXTOP(cl,oc);
      break;      
    case _unify_n_atoms_write:
    case _unify_n_atoms:
      cl = NEXTOP(cl,osc);
      break;      
    case _unify_struct_write:
    case _unify_struct:
    case _unify_l_struc_write:
    case _unify_l_struc:
      cl = NEXTOP(cl,of);
      break;      
    case _write_n_voids:
    case _pop_n:
      cl = NEXTOP(cl,s);
      break;      
    case _write_atom:
      cl = NEXTOP(cl,c);
      break;
    case _write_n_atoms:
      cl = NEXTOP(cl,sc);
      break;
   case _write_struct:
   case _write_l_struc:
      cl = NEXTOP(cl,f);
      break;
    case _call_c_wfail:
    case _try_c:
    case _try_userc:
    case _retry_c:
    case _retry_userc:
    case _switch_on_type:
    case _switch_list_nl:
    case _switch_on_arg_type:
    case _switch_on_sub_arg_type:
    case _if_not_then:
    case _switch_on_func:
    case _switch_on_cons:
    case _go_on_func:
    case _go_on_cons:
    case _if_func:
    case _if_cons:
      clause->Tag = (CELL)NULL;
      return;
    case _p_plus_vv:
    case _p_minus_vv:
    case _p_times_vv:
    case _p_div_vv:
    case _p_and_vv:
    case _p_or_vv:
    case _p_sll_vv:
    case _p_slr_vv:
    case _p_arg_vv:
    case _p_func2s_vv:
    case _p_func2f_xx:
      if (regcopy_in(myregs, nofregs, cl->u.xxx.x) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.xxx.x)) == 0 &&
	  !ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_plus_vc:
    case _p_minus_cv:
    case _p_times_vc:
    case _p_div_cv:
    case _p_and_vc:
    case _p_or_vc:
    case _p_sll_vc:
    case _p_slr_vc:
    case _p_func2s_vc:
      if (regcopy_in(myregs, nofregs, cl->u.xxc.x) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.xxc.x)) == 0 &&
	  !ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxc);
      break;
    case _p_div_vc:
    case _p_sll_cv:
    case _p_slr_cv:
    case _p_arg_cv:
    case _p_func2s_cv:
      if (regcopy_in(myregs, nofregs, cl->u.xcx.x) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.xcx.x)) == 0 &&
	  !ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xcx);
      break;
    case _p_func2f_xy:
      if (regcopy_in(myregs, nofregs, cl->u.xyx.x) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.xyx.x)) == 0 &&
	  !ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xyx);
      break;
    case _p_plus_y_vv:
    case _p_minus_y_vv:
    case _p_times_y_vv:
    case _p_div_y_vv:
    case _p_and_y_vv:
    case _p_or_y_vv:
    case _p_sll_y_vv:
    case _p_slr_y_vv:
    case _p_arg_y_vv:
    case _p_func2s_y_vv:
    case _p_func2f_yx:
      if (cl->u.yxx.y == ycopy) {
	ycopy = 0;	/* weird stuff, let's just reset ycopy */
	if (nofregs == 0) {
	  clause->Tag = (CELL)NULL;
	  return;
	}
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_func2f_yy:
      if (regcopy_in(myregs, nofregs, cl->u.yyx.x) &&
	  (nofregs = delete_regcopy(myregs, nofregs, cl->u.yyx.x)) == 0 &&
	  !ycopy) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yyx);
      break;
    case _p_plus_y_vc:
    case _p_minus_y_cv:
    case _p_times_y_vc:
    case _p_div_y_vc:
    case _p_div_y_cv:
    case _p_and_y_vc:
    case _p_or_y_vc:
    case _p_sll_y_vc:
    case _p_slr_y_vc:
    case _p_func2s_y_vc:
      if (cl->u.yxc.y == ycopy) {
	ycopy = 0;	/* weird stuff, let's just reset ycopy */
	if (nofregs == 0) {
	  clause->Tag = (CELL)NULL;
	  return;
	}
      }
      cl = NEXTOP(cl,yxc);
      break;
    case _p_sll_y_cv:
    case _p_slr_y_cv:
    case _p_arg_y_cv:
    case _p_func2s_y_cv:
      if (cl->u.ycx.y == ycopy) {
	ycopy = 0;	/* weird stuff, let's just reset ycopy */
	if (nofregs == 0) {
	  clause->Tag = (CELL)NULL;
	  return;
	}
      }
      cl = NEXTOP(cl,ycx);
      break;
    case _call_bfunc_xx:
      cl = NEXTOP(cl,lxx);
      break;
    case _call_bfunc_yx:
    case _call_bfunc_xy:
      cl = NEXTOP(cl,lxy);
      break;
    case _call_bfunc_yy:
      cl = NEXTOP(cl,lyy);
      break;
    }
  }
}

static void 
move_next(ClauseDef *clause, UInt regno)
{
  yamop *cl = clause->CurrentCode;
  wamreg wreg = Yap_regnotoreg(regno);
  op_numbers op = Yap_op_from_opcode(cl->opc);

  switch (op) {
  case _p_db_ref_x:
  case _p_float_x:
    if (wreg == cl->u.x.x) {
      clause->CurrentCode = NEXTOP(cl,x);
    }	
    return;
  case _get_list:
    if (wreg == cl->u.x.x) {
      clause->CurrentCode = NEXTOP(cl,x);
    }	
    return;
  case _get_atom:
  case _get_float:
  case _get_longint:
  case _get_bigint:
    if (wreg == cl->u.xc.x) {
      clause->CurrentCode = NEXTOP(cl,xc);
    }	
    return;
  case _get_struct:
    if (wreg == cl->u.xf.x) {
      clause->CurrentCode = NEXTOP(cl,xf);
    }	
  default:
    return;
  }
}

static void
add_arg_info(ClauseDef *clause, UInt argno)
{
  yamop *cl = clause->WorkPC;
  while (TRUE) {
    op_numbers op = Yap_op_from_opcode(cl->opc);
    switch (op) {
    case _glist_valx:
      if (argno == 1) {
	clause->Tag = (CELL)NULL;
	return;
      }
      argno--;
      cl = NEXTOP(cl,xx);
      break;
    case _gl_void_vary:
    case _gl_void_valy:
    case _gl_void_varx:
    case _gl_void_valx:
      clause->Tag = (CELL)NULL;
      return;
    case _glist_valy:
      if (argno == 1) {
	clause->Tag = (CELL)NULL;
	return;
      }
      argno--;
      cl = NEXTOP(cl,xy);
      break;
    case _unify_l_x_var:
    case _unify_l_x_val:
    case _unify_l_x_loc:
    case _unify_x_var:
    case _unify_x_val:
    case _unify_x_loc:
      if (argno == 1) {
	clause->Tag = (CELL)NULL;
	return;
      }
      argno--;
    case _unify_l_x_var_write:
    case _unify_l_x_val_write:
    case _unify_l_x_loc_write:
    case _unify_x_var_write:
    case _unify_x_val_write:
    case _unify_x_loc_write:
      cl = NEXTOP(cl,ox);
      break;
    case _save_pair_x_write:
    case _save_pair_x:
    case _save_appl_x_write:
    case _save_appl_x:
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_var2:
    case _unify_x_var2:
      if (argno == 1 || argno == 2) {
	clause->Tag = (CELL)NULL;
	return;
      }
      argno -= 2;
    case _unify_l_x_var2_write:
    case _unify_x_var2_write:
      cl = NEXTOP(cl,oxx);
      break;
    case _unify_y_var:
    case _unify_y_val:
    case _unify_y_loc:
    case _unify_l_y_var:
    case _unify_l_y_val:
    case _unify_l_y_loc:
      /* we're just done with the head of a list, but there
	 is nothing inside.
       */
      if (argno == 1) {
	clause->Tag = (CELL)NULL;
	return;
      }
      argno--;
    case _unify_y_var_write:
    case _unify_y_val_write:
    case _unify_y_loc_write:
    case _unify_l_y_var_write:
    case _unify_l_y_val_write:
    case _unify_l_y_loc_write:
      cl = NEXTOP(cl,oy);
      break;
    case _save_pair_y_write:
    case _save_pair_y:
    case _save_appl_y_write:
    case _save_appl_y:
      cl = NEXTOP(cl,oy);
      break;
    case _unify_l_void:
    case _unify_void:
      if (argno == 1) {
	clause->Tag = (CELL)NULL;
	return;
      }
      argno--;
    case _unify_l_void_write:
    case _unify_void_write:
      cl = NEXTOP(cl,o);
      break;
    case _unify_list:
    case _unify_l_list:
      if (argno == 1) {
	clause->Tag = AbsPair(NULL);
	return;
      }
      argno += 1; /* 2-1: have two extra arguments to skip */
    case _unify_list_write:
    case _unify_l_list_write:
      cl = NEXTOP(cl,o);
      break;
    case _unify_n_voids:
    case _unify_l_n_voids:
      if (argno <= cl->u.os.s) {
	clause->Tag = (CELL)NULL;
	return;
      }
      argno -= cl->u.os.s;
    case _unify_n_voids_write:
    case _unify_l_n_voids_write:
      cl = NEXTOP(cl,os);
      break;      
    case _unify_atom:
    case _unify_l_atom:
      if (argno == 1) {
	clause->Tag = cl->u.oc.c;
	return;
      }
      argno--;
    case _unify_atom_write:
    case _unify_l_atom_write:
      cl = NEXTOP(cl,oc);
      break;      
    case _unify_l_float:
      if (argno == 1) {
	clause->Tag = AbsAppl((CELL *)FunctorDouble);
	return;
      }
      argno--;
    case _unify_longint:
    case _unify_l_longint:
      if (argno == 1) {
	clause->Tag = AbsAppl((CELL *)FunctorLongInt);
	return;
      }
      argno--;
    case _unify_bigint:
    case _unify_l_bigint:
      if (argno == 1) {
	clause->Tag = AbsAppl((CELL *)FunctorBigInt);
	return;
      }
      argno--;
    case _unify_n_atoms:
      if (argno <= cl->u.osc.s) {
	clause->Tag = cl->u.osc.c;
	return;
      }
      argno -= cl->u.osc.s;
    case _unify_n_atoms_write:
      cl = NEXTOP(cl,osc);
      break;      
    case _unify_struct:
    case _unify_l_struc:
      if (argno == 1) {
	clause->Tag = AbsAppl((CELL *)cl->u.of.f);
	return;
      }
      argno--;
    case _unify_l_struc_write:
    case _unify_struct_write:
      cl = NEXTOP(cl,of);
      break;      
    case _pop:
      cl = NEXTOP(cl,e);
      break;            
    case _pop_n:
      cl = NEXTOP(cl,s);
      break;      
    default:
      return;
    }
  }
}

static void
skip_to_arg(ClauseDef *clause, UInt argno, int at_point)
{
  yamop *cl = clause->WorkPC;
  int done = FALSE;

  at_point = at_point & (clause->WorkPC == clause->CurrentCode); 
  while (!done) {
    op_numbers op = Yap_op_from_opcode(cl->opc);
    switch (op) {
    case _glist_valx:
      at_point = FALSE;
      cl = NEXTOP(cl,xx);
      if (argno == 1) {
	clause->WorkPC=cl;
	done = TRUE;
      } else {
	/* looking to adjust workpc */
	argno--;
      }
      break;
    case _gl_void_vary:
    case _gl_void_valy:
      if (argno == 2) {
	clause->WorkPC = NEXTOP(cl,xy);
      } else {
	clause->WorkPC = cl;
      }
      done = TRUE;
      break;
    case _gl_void_varx:
    case _gl_void_valx:
      if (argno == 2) {
	clause->WorkPC = NEXTOP(cl,xx);
      } else {
	clause->WorkPC = cl;
      }
      done = TRUE;
      break;
    case _glist_valy:
      done = TRUE;
      at_point = FALSE;
      clause->WorkPC = NEXTOP(cl,xy);
      break;
    case _unify_l_x_var:
    case _unify_l_x_val:
    case _unify_l_x_loc:
    case _unify_x_var:
    case _unify_x_val:
    case _unify_x_loc:
      if (argno == 1) {
	clause->WorkPC = NEXTOP(cl,ox);
	done = TRUE;
      } else {
	argno--;
	at_point = FALSE;
      }
    case _unify_l_x_var_write:
    case _unify_l_x_val_write:
    case _unify_l_x_loc_write:
    case _unify_x_var_write:
    case _unify_x_val_write:
    case _unify_x_loc_write:
      cl = NEXTOP(cl,ox);
      break;
    case _save_pair_x_write:
    case _save_pair_x:
    case _save_appl_x_write:
    case _save_appl_x:
      at_point = FALSE;
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_var2:
    case _unify_x_var2:
      at_point = FALSE;
      if (argno == 1 || argno == 2) {
	if (argno == 2) {
	  clause->WorkPC = NEXTOP(cl,oxx);
	} else {
	  clause->WorkPC = cl;
	}
	done = TRUE;
      } else {
	argno -= 2;
      }
    case _unify_l_x_var2_write:
    case _unify_x_var2_write:
      break;
    case _unify_y_var:
    case _unify_y_val:
    case _unify_y_loc:
    case _unify_l_y_var:
    case _unify_l_y_val:
    case _unify_l_y_loc:
      /* we're just done with the head of a list, but there
	 is nothing inside.
       */
      at_point = FALSE;
      if (argno == 1) {
	clause->WorkPC = NEXTOP(cl,oy);
	done = TRUE;
      } else {
	argno--;
      }
    case _unify_y_var_write:
    case _unify_y_val_write:
    case _unify_y_loc_write:
    case _unify_l_y_var_write:
    case _unify_l_y_val_write:
    case _unify_l_y_loc_write:
      cl = NEXTOP(cl,oy);
      break;
    case _save_pair_y_write:
    case _save_pair_y:
    case _save_appl_y_write:
    case _save_appl_y:
      at_point = FALSE;
      cl = NEXTOP(cl,oy);
      break;
    case _unify_l_void:
    case _unify_void:
      if (argno == 1) {
	done = TRUE;
      } else {
	argno--;
      }
    case _unify_l_void_write:
    case _unify_void_write:
      cl = NEXTOP(cl,o);
      break;
    case _unify_list:
    case _unify_l_list:
      if (argno == 1) {
	clause->WorkPC = NEXTOP(cl,o);
	done = TRUE;
      } else {
	argno += 1; /* 2-1: have two extra arguments to skip */
	at_point = FALSE;
      }
    case _unify_list_write:
    case _unify_l_list_write:
      cl = NEXTOP(cl,o);
      break;
    case _unify_n_voids:
    case _unify_l_n_voids:
      if (argno <= cl->u.os.s) {
	clause->WorkPC = cl;
	done = TRUE;
      } else {
	argno -= cl->u.os.s;
      }
    case _unify_n_voids_write:
    case _unify_l_n_voids_write:
      cl = NEXTOP(cl,os);
      break;      
    case _unify_atom:
    case _unify_l_atom:
    case _unify_longint:
    case _unify_l_longint:
    case _unify_bigint:
    case _unify_l_bigint:
    case _unify_l_float:
      if (argno == 1) {
	clause->WorkPC = NEXTOP(cl,oc);
	done = TRUE;
      } else {
	at_point = FALSE;
	argno--;
      }
    case _unify_atom_write:
    case _unify_l_atom_write:
      cl = NEXTOP(cl,oc);
      break;      
    case _unify_n_atoms:
      if (argno <= cl->u.osc.s) {
	if (argno == cl->u.osc.s) {
	  clause->WorkPC = NEXTOP(cl,oc);
	} else {
	  clause->WorkPC = cl;
	  at_point = FALSE;
	}
	done = TRUE;
      } else {
	at_point = FALSE;
	argno -= cl->u.osc.s;
      }
    case _unify_n_atoms_write:
      cl = NEXTOP(cl,osc);
      break;      
    case _unify_struct:
    case _unify_l_struc:
      if (argno == 1) {
	clause->WorkPC = NEXTOP(cl,of);
	done = TRUE;
      } else {
	at_point = FALSE;
	argno--;
      }
    case _unify_l_struc_write:
    case _unify_struct_write:
      cl = NEXTOP(cl,of);
      break;      
    case _pop:
      cl = NEXTOP(cl,e);
      break;            
    case _pop_n:
      cl = NEXTOP(cl,s);
      break;      
    default:
      done = TRUE;
    }
  }
  if (at_point) {
    clause->CurrentCode = clause->WorkPC;
  } else {
    clause->CurrentCode = clause->Code;
  }
}

static UInt
groups_in(ClauseDef *min, ClauseDef *max, GroupDef *grp)
{
  UInt groups = 0;

  while(min <= max) {
    grp->FirstClause = min;
    grp->AtomClauses = 0;
    grp->PairClauses = 0;
    grp->StructClauses = 0;
    grp->TestClauses = 0;
    if (min->Tag == (_var+1)*sizeof(CELL)) {
      min++;
      continue;
    }
    /* only do this for the first clauses in a group */
    if (IsVarTerm(min->Tag)) {
      ClauseDef *clp = min+1;

      grp->VarClauses = 1;
      do {
	if (clp > max ||
	    !IsVarTerm(clp->Tag)) {
	  grp->LastClause = (min = clp)-1;
	  break;
	}
	clp++;
	if (clp->Tag != (_var+1)*sizeof(CELL))
	  grp->VarClauses++;
      } while (TRUE);
    } else {
      grp->VarClauses = 0;
      do {
      restart_loop:
	if (IsAtomTerm(min->Tag) || IsIntTerm(min->Tag)) {
	  grp->AtomClauses++;
	} else if (IsPairTerm(min->Tag)) {
	  grp->PairClauses++;
	} else if (IsApplTerm(min->Tag)) {
	  grp->StructClauses++;
	} else {
	  grp->TestClauses++;
	}
	min++;
      } while (min <= max &&
	       (!IsVarTerm(min->Tag)));
      if (min <= max && min->Tag == (_var+1)*sizeof(CELL)) {
	min++;
	goto restart_loop;
      }
      grp->LastClause = min-1;
    }
    groups++;
    grp++;
  }
  return groups;
}

static UInt
new_label(void)
{
  UInt lbl = labelno;
  labelno += 2;
  return lbl;
}

static void
emit_trust(ClauseDef *cl, PredEntry *ap, UInt nxtlbl, int clauses)
{
  if (CurrentPred->PredFlags & ProfiledPredFlag) {
    Yap_emit(retry_profiled_op, Unsigned(ap), Zero);
  }
  if (CurrentPred->PredFlags & CountPredFlag) {
    Yap_emit(count_retry_op, Unsigned(ap), Zero);
  }
  if (clauses == 0) {
    Yap_emit(trust_op, (CELL)(cl->Code), has_cut(cl->CurrentCode) );
  } else {
    Yap_emit(retry_op, (CELL)(cl->Code), (clauses << 1) | has_cut(cl->CurrentCode) );
    Yap_emit(jump_op, nxtlbl, Zero);
  }
}

static void
emit_retry(ClauseDef *cl, PredEntry *ap, int clauses)
{
  if (CurrentPred->PredFlags & ProfiledPredFlag) {
    Yap_emit(retry_profiled_op, Unsigned(ap), Zero);
  }
  if (CurrentPred->PredFlags & CountPredFlag) {
    Yap_emit(count_retry_op, Unsigned(ap), Zero);
  }
  Yap_emit(retry_op, (CELL)(cl->Code), (clauses << 1) | has_cut(cl->CurrentCode) );
}

static void
emit_try(ClauseDef *cl, PredEntry *ap, int var_group, int first, int clauses, int clleft, UInt nxtlbl)
{
  /* var group */
  if (var_group || clauses == 0) {
    if (first) {
      Yap_emit(try_op, (CELL)(cl->CurrentCode), ((clauses+clleft) << 1) | has_cut(cl->CurrentCode) );
    } else if (clleft+clauses) {
      Yap_emit(retry_op, (CELL)(cl->CurrentCode), ((clauses+clleft) << 1) | has_cut(cl->CurrentCode) );      
    } else {
      Yap_emit(trust_op, (CELL)(cl->CurrentCode), ((clauses+clleft) << 1) | has_cut(cl->CurrentCode));
    }
  } else if (clleft == 0) {
    /* last group */
    Yap_emit(try_op, (CELL)(cl->CurrentCode), ((clauses+clleft) << 1) | has_cut(cl->CurrentCode));
  } else {
    /* nonvar group */
    Yap_emit(try_in_op, (CELL)(cl->CurrentCode), ((clauses+clleft) << 1) | has_cut(cl->CurrentCode) );
  }
}

static TypeSwitch *
emit_type_switch(compiler_vm_op op)
{
  return (TypeSwitch *)Yap_emit_extra_size(op, 0, sizeof(TypeSwitch));
}


static AtomSwiEntry *
emit_cswitch(int n, UInt fail_l)
{
  compiler_vm_op op;
  AtomSwiEntry *target;

  if (n > MIN_HASH_ENTRIES) {
    int cases = MIN_HASH_ENTRIES, i;
    while (cases < n+1) cases *= 2;
    n = cases;
    op = switch_c_op;
    target = (AtomSwiEntry *)Yap_emit_extra_size(op, Unsigned(n), n*sizeof(FuncSwiEntry));
    for (i=0; i<n; i++) {
      target[i].Tag = Zero;
      target[i].Label = fail_l;
    }
  } else {
    CELL *tmp;

    op = if_c_op;
    tmp = Yap_emit_extra_size(op, Unsigned(n), n*sizeof(AtomSwiEntry)+sizeof(CELL));
    *tmp++ = fail_l;
    target = (AtomSwiEntry *)tmp;
  }
  return target;
}

static AtomSwiEntry *
fetch_centry(AtomSwiEntry *cebase, Term wt, int i, int n)
{
  if (n > MIN_HASH_ENTRIES) {
    int cases = MIN_HASH_ENTRIES, hash, d;
    AtomSwiEntry *hentry;

    while (cases < n+1) cases *= 2;
    hash = (wt >> HASH_SHIFT) & (cases-1);
    hentry = cebase + hash;
    d = (cases-1) & (wt|1);
    while (hentry->Tag != Zero) {
#ifdef DEBUG
#ifdef CLASHES
      ++clashes;
#endif /* CLASHES */
#endif /* DEBUG */
      hash = (hash + d) & (cases-1);
      hentry = cebase + hash;
    }
    return hentry;
  } else {
    return cebase + i;
  }
}

static FuncSwiEntry *
emit_fswitch(int n, UInt fail_l)
{
  compiler_vm_op op;
  FuncSwiEntry *target;

  if (n > MIN_HASH_ENTRIES) {
    int cases = MIN_HASH_ENTRIES, i;
    while (cases < n+1) cases *= 2;
    n = cases;
    op = switch_f_op;
    target = (FuncSwiEntry *)Yap_emit_extra_size(op, Unsigned(n), n*sizeof(FuncSwiEntry));
    for (i=0; i<n; i++) {
      target[i].Tag = NULL;
      target[i].Label = fail_l;
    }
  } else {
    CELL *tmp;
    op = if_f_op;
    tmp = Yap_emit_extra_size(op, Unsigned(n), n*sizeof(FuncSwiEntry)+sizeof(CELL));
    *tmp++ = fail_l;
    target = (FuncSwiEntry *)tmp;
  }
  return target;
}

static FuncSwiEntry *
fetch_fentry(FuncSwiEntry *febase, Functor ft, int i, int n)
{
  if (n > MIN_HASH_ENTRIES) {
    int cases = MIN_HASH_ENTRIES, hash, d;
    FuncSwiEntry *hentry;
    Term wt = (CELL)ft;

    while (cases < n+1) cases *= 2;
    hash = (wt >> HASH_SHIFT) & (cases-1);
    hentry = febase + hash;
    d = (cases-1) & (wt|1);
    while (hentry->Tag != NULL) {
#ifdef DEBUG
#ifdef CLASHES
      ++clashes;
#endif /* CLASHES */
#endif /* DEBUG */
      hash = (hash + d) & (cases-1);
      hentry = febase + hash;
    }
    return hentry;
  } else {
    return febase + i;
  }
}

/* we assume there is at least one clause, that is, c0 < cf */
static void
do_var_clauses(ClauseDef *c0, ClauseDef *cf, int var_group, PredEntry *ap, UInt labl, int first, int clleft, UInt nxtlbl) {
  Yap_emit(label_op, labl, Zero);
  if (c0 == cf) {
    emit_try(c0, ap, var_group, first, 0, clleft, nxtlbl);
  } else {
    if (c0 < cf) {
      emit_try(c0, ap, var_group, first, cf-c0, clleft, nxtlbl);
    }
    c0++;
    while (c0 < cf) {
      emit_retry(c0, ap, clleft+(cf-c0));
      c0++;
    }
    if (c0 == cf) {
      emit_trust(c0, ap, nxtlbl, clleft);
    }
  }
}

static void
do_var_group(GroupDef *grp, PredEntry *ap, UInt labl, int var_group, int first, int clleft, UInt nxtlbl) {
  return do_var_clauses(grp->FirstClause, grp->LastClause, var_group, ap, labl, first, clleft, nxtlbl);
}

static void
add_lu_cl_info(yamop *codep)
{
  LogUpdClause *cl = ClauseCodeToLogUpdClause(codep);
  if (cl->ClFlags & LogUpdRuleMask) {
    cl->u2.ClExt->u.EC.ClRefs++;
  } else {
    cl->u2.ClUse++;
  }
}

static UInt
log_update_chain(PredEntry *ap)
{
  yamop *codep = ap->cs.p_code.FirstClause;
  yamop *lastp = ap->cs.p_code.LastClause;

  Yap_emit(label_op, 1, Zero);
  Yap_emit(try_op, (CELL)NEXTOP(codep,ld), Zero);
  add_lu_cl_info(codep);
  codep = NextClause(codep);
  while (codep != lastp) { 
    Yap_emit(retry_op, (CELL)NEXTOP(codep,ld), Zero);
    add_lu_cl_info(codep);
    codep = NextClause(codep);
  }
  Yap_emit(trust_op, (CELL)NEXTOP(codep,ld), Zero);
  add_lu_cl_info(codep);
  return 1;
}


/* count the number of different constants */
static UInt
count_consts(GroupDef *grp)
{
  Term current = MkAtomTerm(AtomFoundVar);
  UInt i = 0;
  ClauseDef *cl = grp->FirstClause;
    
  while (IsAtomTerm(cl->Tag) || IsIntTerm(cl->Tag)) {
    if (current != cl->Tag) {
      i++;
      current = cl->Tag;
    }
    if (cl == grp->LastClause) {
      return i;
    }
    cl++;
  }
  return i;
}

/* count the number of different constants */
static UInt
count_funcs(GroupDef *grp)
{
  Term current = MkAtomTerm(AtomFoundVar);
  UInt i = 0;
  ClauseDef *cl = grp->FirstClause;
    
  while (IsApplTerm(cl->Tag)) {
    if (current != cl->Tag) {
      i++;
      current = cl->Tag;
    }
    if (cl == grp->LastClause) {
      return i;
    }
    cl++;
  }
  return i;
}

static UInt
emit_single_switch_case(ClauseDef *min, PredEntry *ap, int first, int clleft, UInt nxtlbl)
{
  return (UInt)(min->CurrentCode);
}


static UInt
do_var_entries(GroupDef *grp, PredEntry *ap, UInt argno, int first, int clleft, UInt nxtlbl){
  if (argno == 1) {
    /* in this case we want really to jump to the first clause */
    if (ap->PredFlags & LogUpdatePredFlag) {
      if (first && clleft == 0) {
	return log_update_chain(ap);
      } else {
	/* 1 is label for log_update_chain, which should never be taken */
	return 1;
      }
    } else {
      if (first && clleft == 0) {
	/* not protected by a choice-point */
	return (UInt)PREVOP(grp->FirstClause->Code,ld);
      } else {
	/* this code should never execute */
	return nxtlbl;
      }
    }
  } else {
    UInt  labl = new_label();
    do_var_group(grp, ap, labl, FALSE, first, clleft, nxtlbl);
    return labl;
  }
}

static UInt
do_consts(GroupDef *grp, PredEntry *ap, UInt argno, int first, UInt nxtlbl, int clleft, CELL *top)
{
  UInt n;
  ClauseDef *min = grp->FirstClause;
  UInt i;
  UInt lbl;
  /* generate a switch */
  AtomSwiEntry *cs;

  if (!IsAtomTerm(min->Tag) && !IsIntTerm(min->Tag)) {
    /* no clauses, just skip */
    return nxtlbl;
  }
  n = count_consts(grp);
  lbl = new_label();
  Yap_emit(label_op, lbl, Zero);
  cs = emit_cswitch(n, nxtlbl);
  for (i = 0; i < n; i++) {
    AtomSwiEntry *ics;
    ClauseDef *max = min;

    ics = fetch_centry(cs, min->Tag, i, n);
    ics->Tag = min->Tag;
    while ((max+1)->Tag == min->Tag &&
	   max != grp->LastClause) max++;
    ics->Label = do_index(min, max, ap, argno+1, nxtlbl, first, clleft, top);
    grp->FirstClause = min = max+1;
  }
  return lbl;
}

static UInt
do_funcs(GroupDef *grp, PredEntry *ap, UInt argno, int first, int last_arg, UInt nxtlbl, int clleft, CELL *top)
{
  UInt n = count_funcs(grp);
  ClauseDef *min = grp->FirstClause;
  UInt i;
  FuncSwiEntry *fs;
  UInt lbl;

  if (min > grp->LastClause || !IsApplTerm(min->Tag)) {
    /* no clauses, just skip */
    return nxtlbl;
  }
  lbl = new_label();
  Yap_emit(label_op, lbl, Zero);
  /* generate a switch */
  fs = emit_fswitch(n, nxtlbl);
  for (i = 0; i < n ; i++) {
    Functor f = (Functor)RepAppl(min->Tag);
    FuncSwiEntry *ifs;
    ClauseDef *max = min;

    ifs = fetch_fentry(fs, f, i, n);
    ifs->Tag = f;
    while ((max+1)->Tag == min->Tag &&
	   max != grp->LastClause) max++;
    if (IsExtensionFunctor(f)) {
      ifs->Label = do_index(min, max, ap, argno+1, nxtlbl, first, clleft, top);
    } else {
      ifs->Label = do_compound_index(min, max, ap, ArityOfFunctor(f), argno+1, nxtlbl, first, last_arg, clleft, top);
    }
    grp->FirstClause = min = max+1;
  }
  return lbl;
}

static UInt
do_pair(GroupDef *grp, PredEntry *ap, UInt argno, int first, int last_arg, UInt nxtlbl, int clleft, CELL *top)
{
  ClauseDef *min = grp->FirstClause;
  ClauseDef *max = grp->LastClause;

  if (min > max) {
    /* no clauses, just skip */
    return nxtlbl;
  } else if (min == max) {
    /* single clause, no need to do indexing, but we do know it is a list */ 
    return (UInt)(min->CurrentCode);
  }
  return do_compound_index(min, max, ap, 2, argno+1, nxtlbl, first, last_arg, clleft, top);
}

static void
group_prologue(int compound_term, UInt argno, int first)
{
  if (compound_term) {
    Yap_emit(cache_sub_arg_op, compound_term-1, compound_term-1);
  } else {
    if (!first || argno != 1) {
      Yap_emit(cache_arg_op, argno, argno);
    }
  }
}

/* make sure that we can handle failure correctly */
static void
emit_protection_choicepoint(int first, int clleft, UInt nxtlbl)
{
  if (first) {
    if (clleft) {
      Yap_emit(tryme_op, nxtlbl, (clleft << 1));
    }
  } else {
    /* !first */
    if (clleft) {
      Yap_emit(retryme_op, nxtlbl, (clleft << 1));
    } else {
      Yap_emit(trustme_op, 0, 0);
    }
  }
}


static ClauseDef *
cls_move(ClauseDef *min, ClauseDef *max, int compound_term, UInt argno, int last_arg)
{
  ClauseDef *cl=min;

  cl = min;
  if (compound_term) {
    while (cl <= max) {
      skip_to_arg(cl, compound_term, last_arg );
      cl++;
    }
  } else {
    while (cl <= max) {
      if (cl->Tag == (_var+1)*sizeof(CELL)) {
	ClauseDef *cli = cl;
	while (cli < max) {
	  clcpy(cli,cli+1);
	  cli++;
	}
	max--;
      } else {
	move_next(cl, argno);
      }
      cl++;
    }
  }
  return max;
}

static void
purge_pvar(GroupDef *group) {
  ClauseDef *max = group->LastClause;
  ClauseDef *cl = group->FirstClause;

  while (cl <= max) {
    if (cl->Tag == (_var+1)*sizeof(CELL)) {
      ClauseDef *cli = cl;
      while (cli < max) {
	clcpy(cli,cli+1);
	cli++;
      }
      group->VarClauses--;
      max--;
    }
    cl++;
  }
  group->LastClause = max;
}


static void
do_nonvar_group(GroupDef *grp, int compound_term, UInt labl, PredEntry *ap, UInt argno, int first, int last_arg, UInt nxtlbl, int clleft, CELL *top) {
  TypeSwitch *type_sw;

  /* move cl pointer */
  if (grp->AtomClauses + grp->PairClauses + grp->StructClauses > 1) {
    Yap_emit(label_op, labl, Zero);
    if (argno == 1) {
      emit_protection_choicepoint(first, clleft, nxtlbl);
    }
    group_prologue(compound_term, argno, first);
    if (grp->LastClause < grp->FirstClause) { /* only tests */
      return;
    }
    type_sw = emit_type_switch(switch_on_type_op);
    type_sw->VarEntry = do_var_entries(grp, ap, argno, first, clleft, nxtlbl);
    grp->LastClause = cls_move(grp->FirstClause, grp->LastClause, compound_term, argno, last_arg);
    sort_group(grp);
    type_sw->ConstEntry = do_consts(grp, ap, argno, first, nxtlbl, clleft, top);
    type_sw->FuncEntry = do_funcs(grp, ap, argno, first, last_arg, nxtlbl, clleft, top);
    type_sw->PairEntry = do_pair(grp, ap, argno, first, last_arg, nxtlbl, clleft, top);
  } else {
    do_var_group(grp, ap, labl, TRUE, first, clleft, nxtlbl);
  }
}

static UInt
do_optims(GroupDef *group, int ngroups, UInt fail_l)
{
  if (ngroups==2 && group[0].FirstClause ==  group[0].LastClause &&
      group[0].AtomClauses == 1 && group[1].VarClauses == 1) {
    CELL *sp;
    UInt labl;

    labl = new_label();
    sp = Yap_emit_extra_size(if_not_op, Zero, 3*CellSize);
    sp[0] = (CELL)(group[0].FirstClause->Tag);
    sp[1] = (CELL)(group[1].FirstClause->Code);
    sp[2] = (CELL)PREVOP(group[0].FirstClause->Code,ld);
    return labl;
  }
  return fail_l;
}

static int
cls_info(ClauseDef *min, ClauseDef *max, UInt argno)
{
  ClauseDef *cl=min;
  int found_pvar = FALSE;

  while (cl <= max) {
    add_info(cl, argno);
    if (cl->Tag == (_var+1)*sizeof(CELL)) {
      found_pvar = TRUE;
    }
    /*    if (IsVarTerm(cl->Tag)) cl->Tag = (CELL)NULL; */
    cl++;
  }
  return found_pvar;
}

static UInt
do_index(ClauseDef *min, ClauseDef* max, PredEntry *ap, UInt argno, UInt fail_l, int first, int clleft, CELL *top)
{
  UInt ngroups, found_pvar = FALSE;
  UInt i = 0;
  GroupDef *group = (GroupDef *)top;
  UInt labl, labl0;

  if (min == max) {
    /* base case, just commit to the current code */
    return emit_single_switch_case(min, ap, first, clleft, fail_l);
  }
  if (ap->ArityOfPE < argno) {
    UInt labl = new_label();
    do_var_clauses(min, max, FALSE, ap, labl, first, clleft, fail_l);
    return labl;
  }
  found_pvar = cls_info(min, max, argno);
  ngroups = groups_in(min, max, group);
  top = (CELL *)(group+ngroups);
  labl0 = labl = new_label();
  if (argno >1) {
    /* don't try being smart for other arguments than the first */
    if (ngroups > 1 || group->VarClauses != 0 || found_pvar) {
      if (ap->KindOfPE == argno) {
	labl = new_label();
	do_var_clauses(min, max, FALSE, ap, labl, first, clleft, fail_l);
	return labl;
      } else {
	return do_index(min, max, ap, argno+1, fail_l, first, clleft, top);
      }
    } else {
      ClauseDef *cl = min;
      /*
	need to reset the code pointer, otherwise I could be in
	the middle of a compound term.
       */
      while (cl <= max) {
	cl->CurrentCode = cl->Code;
	cl++;
      }    
    }
  } else {
    UInt special_options;
    if ((special_options = do_optims(group, ngroups, fail_l)) != fail_l) {
      return special_options;
    }
    if (ap->PredFlags & LogUpdatePredFlag) {
      /* complicated stuff */
      if (ngroups == 1 && group->VarClauses) {
	return log_update_chain(ap);
      } else if (ngroups > 1) {
	TypeSwitch *type_sw;

	Yap_emit(label_op, labl0, Zero);
	/* first group has variables */
	type_sw = emit_type_switch(switch_on_type_op);
	type_sw->VarEntry = log_update_chain(ap);
	labl = new_label();
	type_sw->ConstEntry = 
	  type_sw->FuncEntry = 
	  type_sw->PairEntry = 
	  labl;
      }
    } else if (ngroups == 1 && group->VarClauses && !found_pvar) {
      return fail_l;
    } else if (ngroups > 1 || found_pvar) {
      Yap_emit(label_op, labl0, Zero);
      Yap_emit(jump_v_op, (CELL)PREVOP(min->Code,ld), Zero);
      labl = new_label();
    }
  }
  for (i=0; i < ngroups; i++) {
    UInt nextlbl;
    int left_clauses = clleft+(max-group->LastClause);
    /* a group may end up not having clauses*/

    if (i < ngroups-1) {
      nextlbl = new_label();
    } else {
      nextlbl = fail_l;
    }
    if (found_pvar && argno == 1) {
      purge_pvar(group);
    }
    if (group->FirstClause==group->LastClause && first && left_clauses == 0) {
      Yap_emit(jumpi_op, (CELL)(group->FirstClause->Code), Zero);
    } else {
      if (group->VarClauses) {
	do_var_group(group, ap, labl, argno == 1, first, left_clauses, nextlbl);
      } else {
	do_nonvar_group(group, 0, labl, ap, argno, first, TRUE, nextlbl, left_clauses, top);
      }
    }
    first = FALSE;
    group++;
    labl = nextlbl;
  }
  return labl0;
}

/* execute an index inside a structure */
static UInt
do_compound_index(ClauseDef *min, ClauseDef* max, PredEntry *ap, UInt arity, UInt argno, UInt fail_l, int first, int last_arg, int clleft, CELL *top)
{
  UInt ngroups;
  UInt i = 0;
  GroupDef *group;
  int labl;
  ClauseDef *cl = min;

  if (min == max) {
    /* base case, just commit to the current code */
    return emit_single_switch_case(cl, ap, first, clleft, fail_l);
  }
  group = (GroupDef *)top;
  cl = min;
  while (i < arity) { 
    ClauseDef *cl = min;
    /* search for a subargument */
    while (cl <= max) {
      add_arg_info(cl, i+1);
      cl++;
    }
    ngroups = groups_in(min, max, group);
    if (ngroups == 1 && group->VarClauses == 0) break;
    i++;
  }
  if (i == arity) {
    return do_index(min, max, ap, argno+1, fail_l, first, clleft, top);
  } else {
    last_arg = (last_arg && i+1 == arity);
  }
  /* ok, we are doing a sub-argument */
  /* process groups */
  labl = new_label();
  top = (CELL *)(group+1);
  do_nonvar_group(group, i+1, labl, ap, argno, argno == 1, last_arg, fail_l, clleft, top);
  return labl;
}

static void
init_clauses(ClauseDef *cl, PredEntry *ap)
{
  yamop *codep = ap->cs.p_code.FirstClause;
  UInt n = ap->cs.p_code.NOfClauses;

  while (n > 0) {
    cl->Code = cl->CurrentCode = NEXTOP(codep,ld);
    n--;
    cl++;
    codep = NextClause(codep);
  }
}

static UInt
compile_index(PredEntry *ap)
{
  int NClauses = ap->cs.p_code.NOfClauses;
  ClauseDef *cls = (ClauseDef *)H;
  CELL *top = (CELL *) TR;
  /* only global variable I use directly */
  labelno = 1;

  if (cls+NClauses > (ClauseDef *)(ASP-4096)) {
    /* grow stack */
    longjmp(Yap_CompilerBotch,3);
  }
  freep = (char *)(cls+NClauses);
  CodeStart = cpc = NIL;
  if (ap->PredFlags & LogUpdatePredFlag) {
    /* throw away a label */
    new_label();
  }
  /* prepare basic data structures */ 
  init_clauses(cls,ap);
  return do_index(cls, cls+(NClauses-1), ap, 1, (UInt)FAILCODE, TRUE, 0, top);
}


yamop *
Yap_PredIsIndexable(PredEntry *ap)
{
  yamop *indx_out;

  if (setjmp(Yap_CompilerBotch) == 3) {
    /* just duplicate the stack */
    restore_machine_regs();
    Yap_gc(ap->ArityOfPE, ENV, CP);
  }
 restart_index:
  Yap_ErrorMessage = NULL;
  if (compile_index(ap) == (UInt)FAILCODE) {
    return NULL;
  }
#ifdef DEBUG
  if (Yap_Option['i' - 'a' + 1]) {
    Yap_ShowCode();
  }
#endif
  /* globals for assembler */
  CurrentPred = ap;
  IPredArity = ap->ArityOfPE;
  if ((indx_out = Yap_assemble(ASSEMBLING_INDEX)) == NULL) {
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return NULL;
    }
    goto restart_index;
  }
  return(indx_out);
}
