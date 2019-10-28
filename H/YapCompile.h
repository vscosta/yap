/*************************************************************************
* *
* YAP Prolog %W% %G% *
* *
*Yap Prolog was developed at NCCUP - Universidade do Porto *
* *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997 *
* *
**************************************************************************
* *
* File:YapCompile.h *
* Last rev: *
* mods: *
* comments:compiler data structures and routines *
* *
*************************************************************************/

/* consult stack management */

#undef COMPILER_OPS
#undef COMPILER_OPS_END
#undef f

#ifdef COMPILER_NAMES

#define COMPILER_OPS() char *opDesc[] =
#define COMPILER_OPS_END()
#define f(x,y) y

#else

#define COMPILER_OPS() typedef enum compiler_op
#define COMPILER_OPS_END() compiler_vm_op
#define f(x,y) x

#endif

/* virtual machine instruction op-codes*/
COMPILER_OPS() {
   f(nop_op, "nop"),
 f(get_var_op, "get_var\t\t %v,%r"),
 f(put_var_op, "put_var\t\t %v,%r"),
 f(   get_val_op, "get_val\t\t %v,%r"),
 f(put_val_op,   "put_val\t\t %v,%r"),
 f(get_atom_op,   "get_atom\t %a,%r"),
 f(put_atom_op,   "put_atom\t %a,%r"),
   f(get_num_op, "get_num\t\t %n,%r"),
 f(put_num_op, "put_num\t\t %n,%r"),
 f(   get_float_op,   "get_float\t\t %w,%r"),
 f(put_float_op,   "put_float\t\t %w,%r"),
 f(get_dbterm_op,   "get_dbterm\t %w,%r"),
   f(put_dbterm_op, "put_dbterm\t %w,%r"),
 f(get_longint_op, "get_longint\t\t %w,%r"),
 f(   put_longint_op,   "put_longint\t\t %w,%r"),
 f(get_string_op,   "get_string\t\t %w,%S"),
 f(put_string_op,   "put_string\t\t %w,%S"),
   f(get_bigint_op, "get_bigint\t\t %l,%r"),
 f(put_bigint_op, "put_bigint\t\t %l,%r"),
 f(   get_list_op,   "get_list\t %r"),
 f(put_list_op,   "put_list\t %r"),
 f(get_struct_op,   "get_struct\t %f,%r"),
   f(put_struct_op, "put_struct\t %f,%r"),
 f(put_unsafe_op, "put_unsafe\t %v,%r"),
 f(   unify_var_op,   "unify_var\t %v"),
 f(write_var_op,   "write_var\t %v"),
 f(unify_val_op,   "unify_val\t %v"),
   f(write_val_op, "write_val\t %v"),
 f(unify_atom_op, "unify_atom\t %a"),
 f(   write_atom_op,   "write_atom\t %a"),
 f(unify_num_op,   "unify_num\t %n"),
 f(write_num_op,   "write_num\t %n"),
   f(unify_float_op, "unify_float\t %w"),
 f(   write_float_op,   "write_float\t %w"),
 f(unify_dbterm_op,   "unify_dbterm\t %w"),
   f(write_dbterm_op, "write_dbterm\t %w"),
 f(   unify_longint_op,   "unify_longint\t %w"),
 f(write_longint_op,   "write_longint\t %w"),
   f(unify_string_op, "unify_string\t %S"),
 f(   write_string_op,   "write_string\t %S"),
 f(unify_bigint_op,   "unify_bigint\t %l"),
 f(write_bigint_op,   "write_bigint\t %l"),
   f(unify_list_op, "unify_list"),
 f(   write_list_op,   "write_list"),
 f(unify_struct_op,   "unify_struct\t %f"),
   f(write_struct_op, "write_struct\t %f"),
 f(   write_unsafe_op,   "write_unsafe\t %v"),
 f(unify_local_op,   "unify_local\t %v"),
   f(write_local_op, "write local\t %v"),
 f(   unify_last_list_op,   "unify_last_list"),
   f(write_last_list_op, "write_last_list"),
 f(   unify_last_struct_op,   "unify_last_struct\t %f"),
   f(write_last_struct_op,   "write_last_struct\t %f"),
 f(unify_last_var_op,   "unify_last_var\t %v"),
   f(   unify_last_val_op,   "unify_last_val\t %v"),
 f(unify_last_local_op,   "unify_last_local\t %v"),
 f(unify_last_atom_op,   "unify_last_atom\t %a"),
 f(unify_last_num_op, "unify_last_num\t %n"),
 f(unify_last_float_op, "unify_last_float\t %w"),
 f(unify_last_dbterm_op, "unify_last_dbterm\t %w"),
 f(unify_last_longint_op, "unify_last_longint\t %w"),
 f(unify_last_string_op, "unify_last_string\t %S"),
 f(unify_last_bigint_op, "unify_last_bigint\t %l"),
 f(ensure_space_op,   "ensure_space"),
 f(native_op,   "native_code"),
 f(f_var_op, "function_to_var\t %v,%B"),
 f(f_val_op, "function_to_val\t %v,%B"),
 f(f_0_op, "function_to_0\t %B"),
 f(align_float_op,   "align_float"),
 f(fail_op, "fail"),
 f(cut_op, "cut"),
 f(cutexit_op,   "cutexit"),
 f(allocate_op,   "allocate"),
 f(deallocate_op,   "deallocate"),
 f(tryme_op,   "try_me_else\t\t %l\t %x"),
 f(jump_op,   "jump\t\t %l"),
 f(jumpi_op,   "jump_in_indexing\t\t %i"),
 f(procceed_op, "proceed"),
 f(call_op, "call\t\t %p,%d,%z"),
 f(execute_op, "execute\t\t %p"),
 f(safe_call_op, "sys\t\t %p"),
 f(label_op,   "%l:"),
 f(name_op, "name\t\t %m,%d"),
 f(pop_op,   "pop\t\t %l"),
 f(retryme_op,   "retry_me_else\t\t %l\t %x"),
 f(trustme_op,   "trust_me_else_fail\t %x"),
 f(either_op, "either_me\t\t %l,%d,%z"),
 f(orelse_op, "or_else\t\t %l,%z"),
 f(orlast_op,   "or_last"),
 f(push_or_op, "push_or"),
 f(pushpop_or_op, "pushpop_or"),
 f(pop_or_op, "pop_or"),
 f(save_b_op, "save_by\t\t %v"),
 f(commit_b_op,   "commit_by\t\t %v"),
 f(patch_b_op, "patch_by\t\t %v"),
 f(try_op,   "try\t\t %g\t %x"),
 f(retry_op, "retry\t\t %g\t %x"),
 f(trust_op,   "trust\t\t %g\t %x"),
 f(try_in_op,   "try_in\t\t %g\t %x"),
 f(jump_v_op,   "jump_if_var\t\t %g"),
 f(jump_nv_op, "jump_if_nonvar\t\t %g"),
 f(cache_arg_op, "cache_arg\t %r"),
 f(cache_sub_arg_op, "cache_sub_arg\t %d"),
 f(user_switch_op,   "user_switch"),
 f(switch_on_type_op, "switch_on_type\t %h\t %h\t %h\t %h"),
 f(switch_c_op,   "switch_on_constant\t %i,n%c"),
 f(if_c_op,   "if_constant\t %i,n%c"),
 f(switch_f_op,   "switch_on_functor\t %i,n%e"),
 f(if_f_op, "if_functor\t %i,n%e"),
 f(if_not_op, "if_not_then\t %i\t %h\t %h\t %h"),
   f(index_dbref_op,   "index_on_dbref"),
 f(index_blob_op,   "index_on_blob"),
 f(index_string_op, "index_on_string"),
   f(index_long_op, "index_on_blob"),
 f(   if_nonvar_op,   "check_var\t %r"),
 f(save_pair_op, "save_pair\t %v"),
   f(save_appl_op, "save_appl\t %v"),
 f(   mark_initialized_pvars_op,   "pvar_bitmap\t %l,%b"),
 f(mark_live_regs_op,   "pvar_live_regs\t %l,%b"),
   f(fetch_args_vv_op, "fetch_reg1_reg2\t %N,%N"),
 f(   fetch_args_cv_op,   "fetch_constant_reg\t %l,%N"),
 f(fetch_args_vc_op,   "fetch_reg_constant\t %l,%N"),
   f(fetch_args_iv_op, "fetch_integer_reg\t %d,%N"),
 f(   fetch_args_vi_op,   "fetch_reg_integer\t %d,%N"),
 f(enter_profiling_op,   "enter_profiling\t\t %g"),
   f(retry_profiled_op, "retry_profiled\t\t %g"),
 f(   count_call_op,   "count_call_op\t\t %g"),
 f(count_retry_op,   "count_retry_op\t\t %g"),
   f(restore_tmps_op, "restore_temps\t\t %l"),
 f(   restore_tmps_and_skip_op,   "restore_temps_and_skip\t\t %l"),
   f(enter_lu_op, "enter_lu"),
 f(   empty_call_op,   "empty_call\t\t %l,%d"),
   f(bccall_op,   "binary_cfunc\t\t %v,%r,%2"),
   f(blob_op,   "blob\t %O"),
   f(string_op,   "string\t %O"),
   f(label_ctl_op,   "label_control\t"),
#ifdef YAPOR
 f(sync_op, "sync"),
#endif /* YAPOR */
#ifdef TABLING
 f(table_new_answer_op, "table_new_answer"),
   f(table_try_single_op, "table_try_single\t %g\t %x"),
#ifdef TABLING_INNER_CUTS
 f(clause_with_cut_op, "clause_with_cut"),
#endif /* TABLING_INNER_CUTS */
     #endif
#ifdef BEAM
 f(run_op, "run_op %1,%4"),
 f(body_op, "body_op %1"),
 f(   endgoal_op, "endgoal_op"),
 f(try_me_op, "try_me_op %1,%4"),
   f(retry_me_op, "retry_me_op %1,%4"),
 f(trust_me_op, "trust_me_op %1,%4"),
   f(only_1_clause_op, "only_1_clause_op %1,%4"),
 f(   create_first_box_op, "create_first_box_op %1,%4"),
       f(create_box_op, "create_box_op %1,%4"),
 f(   create_last_box_op, "create_last_box_op %1,%4"),
   f(remove_box_op, "remove_box_op %1,%4"),
 f(   remove_last_box_op, "remove_last_box_op %1,%4"),
   f(prepare_tries, "prepare_tries"),
 f(std_base_op,   "std_base_op %1,%4"),
   f(direct_safe_call_op, "direct_safe_call"),
   f(commit_op, ),
 f(skip_while_var_op,   "skip_while_var_op"),
   f(wait_while_var_op, "wait_while_var_op"),
   f(force_wait_op, "force_wait_op"),
   f(is_op, "is_op"),
   f(write_op, "write_op"),
   f(equal_op, "equal_op"),
   f(exit_op, "exit"),
#endif
#ifdef SFUNC
   f(get_s_f_op, "get_s_f_op\t %f,%r"),
   f(put_s_f_op, "put_s_f_op\t %f,%r"),
 f(unify_s_f_op, "unify_s_f_op\t %f"),
   f(write_s_f_op, "write_s_f_op\t %f"),
   f(unify_s_var_op, "unify_s_var\t %v,%r"),
 f(write_s_var_op,   "write_s_var\t %v,%r"),
   f(unify_s_val_op, "unify_s_val\t %v,%r"),
   f(write_s_val_op, "write_s_val\t %v,%r"),
   f(unify_s_a_op, "unify_s_a\t %a,%r"),
   f(write_s_a_op, "write_s_a\t %a,%r"),
   f(get_s_end_op, "get_s_end"),
   f(put_s_end_op, "put_s_end"),
   f(unify_s_end_op, "unify_s_end"),
   f(write_s_end_op, "write_s_end"),
#endif
     } COMPILER_OPS_END();

#ifndef COMPILER_NAMES

typedef struct PSEUDO {
 struct PSEUDO *nextInst;
 enum compiler_op op;
 CELL rnd1;
 union {
 Int oprnd2;
#if MIN_ARRAY == 0
 CELL opseqt[MIN_ARRAY];
#else
 CELL opseqt[1];
#endif
 } ops;
} PInstr;

#define arnds ops.opseqt
#define rnd2 ops.oprnd2
#define rnd3 ops.opseqt[1]
#define rnd4 ops.opseqt[2]
#define rnd5 ops.opseqt[3]
#define rnd6 ops.opseqt[4]
#define rnd7 ops.opseqt[5]
#define rnd8 ops.opseqt[6]

typedef struct VENTRY {
 CELL SelfOfVE;
 Term AdrsOfVE;
 Int KindOfVE;
 CELL NoOfVE;
 PInstr *FirstOpForV;
 PInstr *LastOpForV;
 BITS16 AgeOfVE;
 BITS16 BranchOfVE;
 BITS16 LastBranchOfVE;
 BITS16 FirstOfVE;
 BITS16 RCountOfVE;
 BITS16 FlagsOfVE;
 struct VENTRY *NextOfVE;
} Ventry;

typedef struct CEXPENTRY {
 Term TermOfCE;
 PInstr *CodeOfCE;
 Term VarOfCE;
 struct CEXPENTRY *NextCE;
} CExpEntry;

#define COMPILER_ERR_BOTCH 1
#define OUT_OF_HEAP_BOTCH 2
#define OUT_OF_STACK_BOTCH 3
#define OUT_OF_TEMPS_BOTCH 4
#define OUT_OF_AUX_BOTCH 5
#define OUT_OF_TRAIL_BOTCH 6

typedef struct intermediates {
 char *freep;
 char *freep0;
 struct mem_blk *blks;
 char *blk_cur, *blk_top;
 struct PSEUDO *cpc;
 struct PSEUDO *CodeStart;
 struct PSEUDO *icpc;
 struct PSEUDO *BlobsStart;
 struct dbterm_list *dbterml;
 Int *label_offset;
 Int *uses;
 Term *contents;
 struct pred_entry *CurrentPred;
 sigjmp_buf CompilerBotch;
 yamop *code_addr;
 yamop *expand_block;
 UInt i_labelno;
 UInt exception_handler, success_handler, failure_handler;
 /* for expanding code */
 yamop **current_try_lab, **current_trust_lab;
 yamop *try_instructions;
 struct StructClauseDef *cls;
 int clause_has_cut;
 UInt term_depth, last_index_at_depth;
 UInt last_index_new_depth, last_depth_size;
 /* for expanding code */
 union {
 struct static_index *si;
 struct logic_upd_index *lui;
 } current_cl;
} CIntermediates;

typedef enum special_label_id_enum {
 SPECIAL_LABEL_SUCCESS = 0,
 SPECIAL_LABEL_FAILURE = 1,
 SPECIAL_LABEL_EXCEPTION = 2
} special_label_id;

typedef enum special_label_op_enum {
 SPECIAL_LABEL_INIT = 0,
 SPECIAL_LABEL_SET = 1,
 SPECIAL_LABEL_CLEAR = 2
} special_label_op;

#define SafeVar 0x01
#define PermFlag 0x02
#define GlobalVal 0x04
#define OnHeadFlag 0x08
#define NonVoid 0x10
#define BranchVar 0x20
#define OnLastGoal 0x40

#define MaskVarClass 0x0f000000L
#define MaskVarAdrs 0x00ffffffL
#define Unassigned 0x00000000L
#define VoidVar 0x01000000L
#define TempVar 0x02000000L
#define PermVar 0x03000000L

#define save_b_flag 0x10000
#define commit_b_flag 0x10001
#define save_appl_flag 0x10002
#define save_pair_flag 0x10004
#define f_flag 0x10008
#define bt_flag 0x10010
#define bt2_flag 0x10020 // unused
#define patch_b_flag 0x10040
#define init_v_flag 0x10080

#define Zero 0
#define One 1
#define Two 2

extern yamop *Yap_assemble(int, Term, struct pred_entry *, int, struct intermediates *, UInt);
extern void Yap_emit(compiler_vm_op, Int, CELL, struct intermediates *);
extern void Yap_emit_3ops(compiler_vm_op, CELL, CELL, CELL, struct intermediates *);
extern void Yap_emit_4ops(compiler_vm_op, CELL, CELL, CELL, CELL, struct intermediates *);
extern void Yap_emit_5ops(compiler_vm_op, CELL, CELL, CELL, CELL, CELL,struct intermediates *);
extern void Yap_emit_6ops(compiler_vm_op, CELL, CELL, CELL, CELL, CELL, CELL,struct intermediates *);
extern void Yap_emit_7ops(compiler_vm_op, CELL, CELL, CELL, CELL, CELL, CELL, CELL, struct intermediates *);
extern CELL *Yap_emit_extra_size(compiler_vm_op, CELL, int, struct intermediates *);
extern char *Yap_AllocCMem(UInt, struct intermediates *);
extern void Yap_ReleaseCMem(struct intermediates *);
extern int Yap_is_a_test_pred(Term, Term);
extern const char * Yap_bip_name(Int);
#ifdef DEBUG
extern void Yap_ShowCode(struct intermediates *);
#endif /* DEBUG */

#endif
