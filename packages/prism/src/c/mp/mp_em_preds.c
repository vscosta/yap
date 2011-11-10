/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifdef MPI

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "up/up.h"
#include "up/em.h"
#include "up/em_aux.h"
#include "up/em_aux_ml.h"
#include "up/em_aux_vb.h"
#include "up/graph.h"
#include "up/flags.h"
#include "mp/mp.h"
#include "mp/mp_core.h"
#include "mp/mp_em_aux.h"
#include "mp/mp_em_ml.h"
#include "mp/mp_em_vb.h"
#include "mp/mp_sw.h"
#include <mpi.h>

/*------------------------------------------------------------------------*/

/* mic.c (B-Prolog) */
NORET myquit(int, const char *);

/*------------------------------------------------------------------------*/

int pc_mpm_prism_em_6(void)
{
    struct EM_Engine em_eng;

    /* [28 Aug 2007, by yuizumi]
     * occ_switches[] will be freed in pc_import_occ_switches/1.
     * occ_position[] is not allocated.
     */
    RET_ON_ERR(check_smooth(&em_eng.smooth));
    mpm_share_preconds_em(&em_eng.smooth);
    RET_ON_ERR(mpm_run_em(&em_eng));
    release_sw_msg_buffers();
    release_num_sw_vals();

    return
        bpx_unify(bpx_get_call_arg(1,6), bpx_build_integer(em_eng.iterate))  &&
        bpx_unify(bpx_get_call_arg(2,6), bpx_build_float(em_eng.lambda))     &&
        bpx_unify(bpx_get_call_arg(3,6), bpx_build_float(em_eng.likelihood)) &&
        bpx_unify(bpx_get_call_arg(4,6), bpx_build_float(em_eng.bic))        &&
        bpx_unify(bpx_get_call_arg(5,6), bpx_build_float(em_eng.cs))         &&
        bpx_unify(bpx_get_call_arg(6,6), bpx_build_integer(em_eng.smooth));
}

int pc_mps_prism_em_0(void)
{
    struct EM_Engine em_eng;

    mps_share_preconds_em(&em_eng.smooth);
    RET_ON_ERR(mps_run_em(&em_eng));
    release_sw_msg_buffers();
    release_occ_switches();
    release_num_sw_vals();
    release_occ_position();

    return BP_TRUE;
}

int pc_mpm_prism_vbem_2(void)
{
    struct VBEM_Engine vb_eng;

    RET_ON_ERR(check_smooth_vb());
    mpm_share_preconds_vbem();
    RET_ON_ERR(mpm_run_vbem(&vb_eng));
    release_sw_msg_buffers();
    release_num_sw_vals();

    return
        bpx_unify(bpx_get_call_arg(1,2), bpx_build_integer(vb_eng.iterate)) &&
        bpx_unify(bpx_get_call_arg(2,2), bpx_build_float(vb_eng.free_energy));
}

int pc_mps_prism_vbem_0(void)
{
    struct VBEM_Engine vb_eng;

    mps_share_preconds_vbem();
    RET_ON_ERR(mps_run_vbem(&vb_eng));
    release_sw_msg_buffers();
    release_occ_switches();
    release_num_sw_vals();
    release_occ_position();

    return BP_TRUE;
}

int pc_mpm_prism_both_em_2(void)
{
    struct VBEM_Engine vb_eng;

    RET_ON_ERR(check_smooth_vb());
    mpm_share_preconds_vbem();
    RET_ON_ERR(mpm_run_vbem(&vb_eng));

	get_param_means();

    release_sw_msg_buffers();
    release_num_sw_vals();

    return
        bpx_unify(bpx_get_call_arg(1,2), bpx_build_integer(vb_eng.iterate)) &&
        bpx_unify(bpx_get_call_arg(2,2), bpx_build_float(vb_eng.free_energy));
}

int pc_mps_prism_both_em_0(void)
{
    struct VBEM_Engine vb_eng;

    mps_share_preconds_vbem();
    RET_ON_ERR(mps_run_vbem(&vb_eng));

	get_param_means();

    release_sw_msg_buffers();
    release_occ_switches();
    release_num_sw_vals();
    release_occ_position();

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

int pc_mpm_import_graph_stats_4(void)
{
    int dummy[4] = { 0 };
    int stats[4];
    double avg_shared;

    MPI_Reduce(dummy, stats, 4, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    avg_shared = (double)(stats[3]) / stats[0];

    return
        bpx_unify(bpx_get_call_arg(1,4), bpx_build_integer(stats[0])) &&
        bpx_unify(bpx_get_call_arg(2,4), bpx_build_integer(stats[1])) &&
        bpx_unify(bpx_get_call_arg(3,4), bpx_build_integer(stats[2])) &&
        bpx_unify(bpx_get_call_arg(4,4), bpx_build_float(avg_shared));
}

int pc_mps_import_graph_stats_0(void)
{
    int dummy[4];
    int stats[4];

    graph_stats(stats);
    MPI_Reduce(stats, dummy, 4, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

    mp_debug("# subgoals     = %d", stats[0]);
    mp_debug("# goal nodes   = %d", stats[1]);
    mp_debug("# switch nodes = %d", stats[2]);
    mp_debug("# sharings     = %d", stats[3]);

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

#endif /* MPI */
