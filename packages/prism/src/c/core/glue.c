#include <stdint.h>
#include <stdlib.h>

void bp4p_init(int *argc, char **argv[]);
void bp4p_exit(int status);
void bp4p_quit(int status);
void bp4p_register_preds(void);

/*--------------------------------------------------------------------*/

#define REGISTER_CPRED(p,n) \
	do { extern int pc_ ## p ## _ ## n (void); insert_cpred("$pc_" #p, n, pc_ ## p ## _ ## n); } while (0)

/*--------------------------------------------------------------------*/

typedef struct sym_rec * SYM_REC_PTR;
typedef long int TERM;
SYM_REC_PTR insert_cpred(const char *, int, int(*)(void));
void exit(int);

#ifdef __YAP_PROLOG__
typedef int (*CPredicate)(void);
void
YAP_UserCPredicate(const char *name, CPredicate def, uintptr_t arity);

SYM_REC_PTR insert_cpred(const char *s, int n, int(*f)(void))
{
  YAP_UserCPredicate(s, f, n);
  return NULL;
}

#endif

/*--------------------------------------------------------------------*/

void register_prism_errors(void);
#ifdef MPI
void mp_init(int *argc, char **argv[]);
void mp_done(void);
void mp_quit(int);
#endif

/*--------------------------------------------------------------------*/

void bp4p_init(int *argc, char **argv[])
{
#ifdef MPI
    mp_init(argc, argv);
#endif
}

void bp4p_exit(int status)
{
#ifdef MPI
    mp_done();
#endif
    exit(status);
}

void bp4p_quit(int status)
{
#ifdef MPI
    mp_quit(status);
#else
    exit(status);
#endif
}

void bp4p_register_preds(void)
{
    /* core/idtable.c */
    REGISTER_CPRED(prism_id_table_init,0);
    REGISTER_CPRED(prism_goal_id_register,2);
    REGISTER_CPRED(prism_sw_id_register,2);
    REGISTER_CPRED(prism_sw_ins_id_register,2);
    REGISTER_CPRED(prism_goal_id_get,2);
    REGISTER_CPRED(prism_sw_id_get,2);
    REGISTER_CPRED(prism_sw_ins_id_get,2);
    REGISTER_CPRED(prism_goal_count,1);
    REGISTER_CPRED(prism_sw_count,1);
    REGISTER_CPRED(prism_sw_ins_count,1);
    REGISTER_CPRED(prism_goal_term,2);
    REGISTER_CPRED(prism_sw_term,2);
    REGISTER_CPRED(prism_sw_ins_term,2);

    /* core/random.c */
    REGISTER_CPRED(random_auto_seed, 1);
    REGISTER_CPRED(random_init_by_seed, 1);
    REGISTER_CPRED(random_init_by_list, 1);
    REGISTER_CPRED(random_float, 1);
    REGISTER_CPRED(random_gaussian, 1);
    REGISTER_CPRED(random_int, 2);
    REGISTER_CPRED(random_int, 3);
    REGISTER_CPRED(random_get_state, 1);
    REGISTER_CPRED(random_set_state, 1);

    /* core/util.c */
    REGISTER_CPRED(lngamma, 2);

    /* up/em_preds.c */
    REGISTER_CPRED(prism_prepare,4);
    REGISTER_CPRED(prism_em,6);
    REGISTER_CPRED(prism_vbem,2);
    REGISTER_CPRED(prism_both_em,2);
    REGISTER_CPRED(compute_inside,2);
    REGISTER_CPRED(compute_probf,1);

    /* up/viterbi.c */
    REGISTER_CPRED(compute_viterbi,5);
    REGISTER_CPRED(compute_n_viterbi,3);
    REGISTER_CPRED(compute_n_viterbi_rerank,4);

    /* up/hindsight.c */
    REGISTER_CPRED(compute_hindsight,4);

    /* up/graph.c */
    REGISTER_CPRED(alloc_egraph,0);
    REGISTER_CPRED(clean_base_egraph,0);
    REGISTER_CPRED(clean_egraph,0);
    REGISTER_CPRED(export_switch,2);
    REGISTER_CPRED(add_egraph_path,3);
    REGISTER_CPRED(alloc_sort_egraph,1);
    REGISTER_CPRED(clean_external_tables,0);
    REGISTER_CPRED(export_sw_info,1);
    REGISTER_CPRED(import_sorted_graph_size,1);
    REGISTER_CPRED(import_sorted_graph_gid,2);
    REGISTER_CPRED(import_sorted_graph_paths,2);
    REGISTER_CPRED(get_gnode_inside,2);
    REGISTER_CPRED(get_gnode_outside,2);
    REGISTER_CPRED(get_gnode_viterbi,2);
    REGISTER_CPRED(get_snode_inside,2);
    REGISTER_CPRED(get_snode_expectation,2);
    REGISTER_CPRED(import_occ_switches,3);
    REGISTER_CPRED(import_graph_stats,4);

    /* up/flags.c */
    REGISTER_CPRED(set_daem,1);
    REGISTER_CPRED(set_em_message,1);
    REGISTER_CPRED(set_em_progress,1);
    REGISTER_CPRED(set_error_on_cycle,1);
    REGISTER_CPRED(set_explicit_empty_expls,1);
    REGISTER_CPRED(set_fix_init_order,1);
    REGISTER_CPRED(set_init_method,1);
    REGISTER_CPRED(set_itemp_init,1);
    REGISTER_CPRED(set_itemp_rate,1);
    REGISTER_CPRED(set_log_scale,1);
    REGISTER_CPRED(set_max_iterate,1);
    REGISTER_CPRED(set_num_restart,1);
    REGISTER_CPRED(set_prism_epsilon,1);
    REGISTER_CPRED(set_show_itemp,1);
    REGISTER_CPRED(set_std_ratio,1);
    REGISTER_CPRED(set_verb_em,1);
    REGISTER_CPRED(set_verb_graph,1);
    REGISTER_CPRED(set_warn,1);
    REGISTER_CPRED(set_debug_level,1);

    /* up/util.c */
    REGISTER_CPRED(mp_mode,0);
    REGISTER_CPRED(get_term_depth,2);
    REGISTER_CPRED(mtrace,0);
    REGISTER_CPRED(muntrace,0);
    REGISTER_CPRED(sleep,1);

#ifdef MPI
    /* mp/mp_preds.c */
    REGISTER_CPRED(mp_size,1);
    REGISTER_CPRED(mp_rank,1);
    REGISTER_CPRED(mp_master,0);
    REGISTER_CPRED(mp_abort,0);
    REGISTER_CPRED(mp_wtime,1);
    REGISTER_CPRED(mp_sync,2);
    REGISTER_CPRED(mp_send_goal,1);
    REGISTER_CPRED(mp_recv_goal,1);
    REGISTER_CPRED(mpm_bcast_command,1);
    REGISTER_CPRED(mps_bcast_command,1);
    REGISTER_CPRED(mps_revert_stdout,0);

    /* mp/mp_em_preds.c */
    REGISTER_CPRED(mpm_prism_em,6);
    REGISTER_CPRED(mps_prism_em,0);
    REGISTER_CPRED(mpm_prism_vbem,2);
    REGISTER_CPRED(mps_prism_vbem,0);
    REGISTER_CPRED(mpm_prism_both_em,2);
    REGISTER_CPRED(mps_prism_both_em,0);
    REGISTER_CPRED(mpm_import_graph_stats,4);
    REGISTER_CPRED(mps_import_graph_stats,0);

    /* mp/mp_sw.c */
    REGISTER_CPRED(mp_send_switches,0);
    REGISTER_CPRED(mp_recv_switches,0);
    REGISTER_CPRED(mp_send_swlayout,0);
    REGISTER_CPRED(mp_recv_swlayout,0);
    REGISTER_CPRED(mpm_alloc_occ_switches,0);

    /* mp/mp_flags.c */
    REGISTER_CPRED(mpm_share_prism_flags,0);
    REGISTER_CPRED(mps_share_prism_flags,0);
#endif

    /* up/error.c; FIXME: There would be a better place to call */
    register_prism_errors();
}

/*--------------------------------------------------------------------*/
