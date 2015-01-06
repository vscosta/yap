/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "up/up.h"
#include "up/graph.h"
#include "up/graph_aux.h"
#include "up/em.h"
#include "up/em_ml.h"
#include "up/em_vb.h"
#include "up/em_aux.h"
#include "up/em_aux_ml.h"
#include "up/em_aux_vb.h"
#include "up/viterbi.h"
#include "up/hindsight.h"
#include "up/flags.h"
#include "up/util.h"

/*------------------------------------------------------------------------*/

/* mic.c (B-Prolog) */
NORET myquit(int, const char *);

int pc_prism_prepare_4(void);
int pc_prism_em_6(void);
int pc_prism_vbem_2(void);
int pc_prism_both_em_2(void);
int pc_compute_inside_2(void);
int pc_compute_probf_1(void);

/*------------------------------------------------------------------------*/

int pc_prism_prepare_4(void)
{
    TERM  p_fact_list;
    int   size;

    p_fact_list        = bpx_get_call_arg(1,4);
    size               = bpx_get_integer(bpx_get_call_arg(2,4));
    num_goals          = bpx_get_integer(bpx_get_call_arg(3,4));
    failure_root_index = bpx_get_integer(bpx_get_call_arg(4,4));

    failure_observed = (failure_root_index != -1);

    if (failure_root_index != -1) {
        failure_subgoal_id = prism_goal_id_get(failure_atom);
        if (failure_subgoal_id == -1) {
            emit_internal_error("no subgoal ID allocated to `failure'");
            RET_INTERNAL_ERR;
        }
    }

    initialize_egraph_index();
    alloc_sorted_egraph(size);
    RET_ON_ERR(sort_egraphs(p_fact_list));
#ifndef MPI
    if (verb_graph) {
        print_egraph(0, PRINT_NEUTRAL);
    }
#endif /* !(MPI) */

    alloc_occ_switches();
    if (fix_init_order) {
        sort_occ_switches();
    }
    alloc_num_sw_vals();

    return BP_TRUE;
}

int pc_prism_em_6(void)
{
    struct EM_Engine em_eng;

    RET_ON_ERR(check_smooth(&em_eng.smooth));
    RET_ON_ERR(run_em(&em_eng));
    release_num_sw_vals();

    return
        bpx_unify(bpx_get_call_arg(1,6), bpx_build_integer(em_eng.iterate   )) &&
        bpx_unify(bpx_get_call_arg(2,6), bpx_build_float  (em_eng.lambda    )) &&
        bpx_unify(bpx_get_call_arg(3,6), bpx_build_float  (em_eng.likelihood)) &&
        bpx_unify(bpx_get_call_arg(4,6), bpx_build_float  (em_eng.bic       )) &&
        bpx_unify(bpx_get_call_arg(5,6), bpx_build_float  (em_eng.cs        )) &&
        bpx_unify(bpx_get_call_arg(6,6), bpx_build_integer(em_eng.smooth    )) ;
}

int pc_prism_vbem_2(void)
{
    struct VBEM_Engine vb_eng;

    RET_ON_ERR(check_smooth_vb());
    RET_ON_ERR(run_vbem(&vb_eng));
    release_num_sw_vals();

    return
        bpx_unify(bpx_get_call_arg(1,2), bpx_build_integer(vb_eng.iterate)) &&
        bpx_unify(bpx_get_call_arg(2,2), bpx_build_float(vb_eng.free_energy));
}

int pc_prism_both_em_2(void)
{
    struct VBEM_Engine vb_eng;

    RET_ON_ERR(check_smooth_vb());
    RET_ON_ERR(run_vbem(&vb_eng));

	get_param_means();

    release_num_sw_vals();

    return
        bpx_unify(bpx_get_call_arg(1,2), bpx_build_integer(vb_eng.iterate)) &&
        bpx_unify(bpx_get_call_arg(2,2), bpx_build_float(vb_eng.free_energy));
}

int pc_compute_inside_2(void)
{
    int gid;
    double prob;
    EG_NODE_PTR eg_ptr;

    gid = bpx_get_integer(bpx_get_call_arg(1,2));

    initialize_egraph_index();
    alloc_sorted_egraph(1);
    RET_ON_ERR(sort_one_egraph(gid, 0, 1));

    if (verb_graph) {
        print_egraph(0, PRINT_NEUTRAL);
    }

    eg_ptr = expl_graph[gid];

	if (log_scale) {
        RET_ON_ERR(compute_inside_scaling_log_exp());
        prob = eg_ptr->inside;
	}
	else {
        RET_ON_ERR(compute_inside_scaling_none());
        prob = eg_ptr->inside;
	}

    return bpx_unify(bpx_get_call_arg(2,2), bpx_build_float(prob));
}

/*------------------------------------------------------------------------*/

int pc_compute_probf_1(void)
{
    // EG_NODE_PTR eg_ptr;
    int prmode;

    prmode = bpx_get_integer(bpx_get_call_arg(1,1));

    if (prmode == 3) {
        compute_max();
        return BP_TRUE;
    }

    // eg_ptr = expl_graph[roots[0]->id];
    failure_root_index = -1;

    /* [31 Mar 2008, by yuizumi]
     * compute_outside_scaling_*() is needed to be called because
     * eg_ptr->outside computed by compute_expectation_scaling_*()
     * is different from the outside probability.
     */
	if (log_scale) {
        RET_ON_ERR(compute_inside_scaling_log_exp());
        if (prmode != 1) {
            RET_ON_ERR(compute_expectation_scaling_log_exp());
            RET_ON_ERR(compute_outside_scaling_log_exp());
        }
	}
	else {
        RET_ON_ERR(compute_inside_scaling_none());
        if (prmode != 1) {
            RET_ON_ERR(compute_expectation_scaling_none());
            RET_ON_ERR(compute_outside_scaling_none());
        }
	}

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/
