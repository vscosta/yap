/*************************************************************************
*									 *
*	 YAP Prolog  %W% %G%						 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		compile.h						 *
* Last rev:								 *
* mods:									 *
* comments:	compiler data structures and routines			 *
*									 *
*************************************************************************/

/* virtual machine instruction op-codes					*/
typedef enum compiler_op {
  nop_op,
  get_var_op,
  put_var_op,
  get_val_op,
  put_val_op,
  get_atom_op,
  put_atom_op,
  get_num_op,
  put_num_op,
  get_float_op,
  put_float_op,
  get_longint_op,
  put_longint_op,
  get_bigint_op,
  put_bigint_op,
  get_list_op,
  put_list_op,
  get_struct_op,
  put_struct_op,
  put_unsafe_op,
  unify_var_op,
  write_var_op,
  unify_val_op,
  write_val_op,
  unify_atom_op,
  write_atom_op,
  unify_num_op,
  write_num_op,
  unify_float_op,
  write_float_op,
  unify_longint_op,
  write_longint_op,
  unify_bigint_op,
  write_bigint_op,
  unify_list_op,
  write_list_op,
  unify_struct_op,
  write_struct_op,
  write_unsafe_op,
  fail_op,
  cut_op,
  cutexit_op,
  allocate_op,
  deallocate_op,
  tryme_op,
  jump_op,
  procceed_op,
  call_op,
  execute_op,
  safe_call_op,
  label_op,
  name_op,
  pop_op,
  retryme_op,
  trustme_op,
  either_op,
  orelse_op,
  orlast_op,
  push_or_op,
  pushpop_or_op,
  pop_or_op,
  save_b_op,
  comit_b_op,
  patch_b_op,
  try_op,
  retry_op,
  trust_op,
  tryin_op,
  retryin_op,
  trustin_op,
  tryf_op,
  retryf_op,
  trustf_op,
  tryfin_op,
  retryfin_op,
  trustfin_op,
  tryt_op,
  retryt_op,
  trustt_op,
  trytin_op,
  retrytin_op,
  trusttin_op,
  tryh_op,
  retryh_op,
  trusth_op,
  tryhin_op,
  retryhin_op,
  trusthin_op,
  trylf_op,
  trylh_op,
  jump_v_op,
  switch_t_op,
  switch_nv_op,
  switch_l_op,
  switch_h_op,
  switch_lnl_op,
  switch_nvl_op,
  switch_ll_op,
  switch_c_op,
  if_c_op,
  go_c_op,
  switch_f_op,
  if_f_op,
  go_f_op,
  if_not_op,
  save_pair_op,
  save_appl_op,
  comit_opt_op,
  unify_local_op,
  write_local_op,
  unify_last_list_op,
  write_last_list_op,
  unify_last_struct_op,
  write_last_struct_op,
  unify_last_var_op,
  unify_last_val_op,
  unify_last_local_op,
  unify_last_atom_op,
  unify_last_num_op,
  unify_last_float_op,
  unify_last_longint_op,
  unify_last_bigint_op,
  mark_initialised_pvars_op,
  mark_live_regs_op,
  fetch_args_vv_op,
  fetch_args_cv_op,
  fetch_args_vc_op,
  f_var_op,
  f_val_op,
  enter_profiling_op,
  retry_profiled_op,
  restore_tmps_op,
  restore_tmps_and_skip_op,
  empty_call_op,
#ifdef TABLING
  table_new_answer_op,
#endif /* TABLING */
#ifdef YAPOR
  sync_op,
#endif /* YAPOR */
  fetch_args_for_bccall,
  bccall_op,
  blob_op
#ifdef SFUNC
  ,
  get_s_f_op,
  put_s_f_op,
  unify_s_f_op,
  write_s_f_op,
  unify_s_var_op,
  write_s_var_op,
  unify_s_val_op,
  write_s_val_op,
  unify_s_a_op,
  write_s_a_op,
  get_s_end_op,
  put_s_end_op,
  unify_s_end_op,
  write_s_end_op,
#endif

} compiler_vm_op;

typedef struct PSEUDO {
	struct PSEUDO *nextInst;
	enum compiler_op op;
        CELL rnd1;
	union {
	    Int   oprnd2;
	    CELL opseqt[1];
	} ops;
    } PInstr;

#define rnd2  ops.oprnd2
#define arnds ops.opseqt
#define rnd3  ops.opseqt[1]

typedef struct VENTRY {
	 CELL SelfOfVE;
	 Term AdrsOfVE;
	 Int  KindOfVE;
	 CELL NoOfVE;
	 PInstr *FirstOpForV;
	 BITS16 AgeOfVE;
	 BITS16 BranchOfVE;
	 BITS16 FirstOfVE;
	 BITS16 RCountOfVE;
	 BITS16 FlagsOfVE;
	 struct VENTRY *NextOfVE;
    } Ventry;

typedef struct CEXPENTRY {
         Term   TermOfCE;
	 PInstr *CodeOfCE;
	 Term   VarOfCE;
	 struct CEXPENTRY *RightCE, *LeftCE;
       } CExpEntry;

#define	 SafeVar	0x01
#define  PermFlag	0x02
#define  GlobalVal	0x04
#define  OnHeadFlag	0x08
#define  NonVoid	0x10
#define  BranchVar      0x20
#define  OnLastGoal     0x40

#define  MaskVarClass   0x0f000000L
#define  MaskVarAdrs    0x00ffffffL
#define	 Unassigned	0x00000000L
#define	 VoidVar	0x01000000L
#define	 TempVar	0x02000000L
#define	 PermVar	0x03000000L


#define save_b_flag	   10000
#define comit_b_flag	   10001
#define save_appl_flag	   10002
#define save_pair_flag	   10004
#define f_flag		   10008
#define bt1_flag	   10010
#define bt2_flag	   10020
#define patch_b_flag	   10040


#define Zero	 0
#define One	 1
#define Two	 2


void	STD_PROTO(emit,(compiler_vm_op,Int,CELL));
void	STD_PROTO(emit_3ops,(compiler_vm_op,CELL,CELL,CELL));
CELL   *STD_PROTO(emit_extra_size,(compiler_vm_op,CELL,int));
char   *STD_PROTO(AllocCMem,(int));
int	STD_PROTO(is_a_test_pred,(Term));
void    STD_PROTO(bip_name,(Int, char *));
#ifdef DEBUG
void	STD_PROTO(ShowCode,(void));
#endif /* DEBUG */


extern PInstr *cpc, *CodeStart;

extern PInstr *icpc, *BlobsStart;

extern char *freep, *freep0;

extern int IPredArity;

extern jmp_buf CompilerBotch;

extern int profiling;

