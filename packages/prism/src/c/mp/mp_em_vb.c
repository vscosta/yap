/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifdef MPI

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "up/up.h"
#include "up/em.h"
#include "up/em_aux.h"
#include "up/em_aux_vb.h"
#include "up/em_vb.h"
#include "up/graph.h"
#include "up/flags.h"
#include "up/util.h"
#include "mp/mp.h"
#include "mp/mp_core.h"
#include "mp/mp_em_aux.h"
#include <mpi.h>

/*------------------------------------------------------------------------*/

void mpm_share_preconds_vbem(void)
{
    int ivals[3];
    int ovals[3];

    ivals[0] = sw_msg_size;
    ivals[1] = 0;
    ivals[2] = 0;

    MPI_Allreduce(ivals, ovals, 3, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    sw_msg_size      = ovals[0];
    num_goals     = ovals[1];
    failure_observed = ovals[2];

    mp_debug("msgsize=%d, #goals=%d, failure=%s",
             sw_msg_size, num_goals, failure_observed ? "on" : "off");

    alloc_sw_msg_buffers();
    mpm_bcast_fixed();
}

void mps_share_preconds_vbem(void)
{
    int ivals[3];
    int ovals[3];

    ivals[0] = 0;
    ivals[1] = num_goals;
    ivals[2] = failure_observed;

    MPI_Allreduce(ivals, ovals, 3, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    sw_msg_size      = ovals[0];
    num_goals     = ovals[1];
    failure_observed = ovals[2];

    mp_debug("msgsize=%d, #goals=%d, failure=%s",
             sw_msg_size, num_goals, failure_observed ? "on" : "off");

    alloc_sw_msg_buffers();
    mps_bcast_fixed();
}

/*------------------------------------------------------------------------*/

int mpm_run_vbem(VBEM_ENG_PTR vbptr)
{
    int     r, iterate, old_valid, converged, saved=0;
    double  free_energy, old_free_energy=0.0;
    double  l0, l1;

    config_vbem(vbptr);

    for (r = 0; r < num_restart; r++) {
        SHOW_PROGRESS_HEAD("#vbem-iters", r);

        initialize_hyperparams();
        mpm_bcast_inside_h();
        mpm_bcast_smooth();
        clear_sw_msg_send();

        itemp = daem ? itemp_init : 1.0;
        iterate = 0;

        while (1) {
            if (daem) {
                SHOW_PROGRESS_TEMP(itemp);
            }
            old_valid = 0;

            while (1) {
                if (CTRLC_PRESSED) {
                    SHOW_PROGRESS_INTR();
                    RET_ERR(err_ctrl_c_pressed);
                }

                RET_ON_ERR(vbptr->compute_pi());

                if (failure_observed) {
                    inside_failure = mp_sum_value(0.0);
                }

                l0 = vbptr->compute_free_energy_l0();
                l1 = vbptr->compute_free_energy_l1();
                free_energy = mp_sum_value(l0 - l1);

                mp_debug("local free_energy = %.9f, free_energy = %.9f", l0 - l1, free_energy);

                if (verb_em) {
                    prism_printf("Iteration #%d:\tfree_energy=%.9f\n", iterate, free_energy);
                }

                if (!isfinite(free_energy)) {
                    emit_internal_error("invalid variational free energy: %s (at iteration #%d)",
                                        isnan(free_energy) ? "NaN" : "infinity", iterate);
                    RET_ERR(err_invalid_free_energy);
                }
                if (old_valid && old_free_energy - free_energy > prism_epsilon) {
                    emit_error("variational free energy decreased [old: %.9f, new: %.9f] (at iteration #%d)",
                               old_free_energy, free_energy, iterate);
                    RET_ERR(err_invalid_free_energy);
                }
                if (itemp == 1.0 && free_energy > 0.0) {
                    emit_error("variational free energy greater than zero [value: %.9f] (at iteration #%d)",
                               free_energy, iterate);
                    RET_ERR(err_invalid_free_energy);
                }

                converged = (old_valid && free_energy - old_free_energy <= prism_epsilon);
                if (converged || REACHED_MAX_ITERATE(iterate)) {
                    break;
                }

                old_free_energy = free_energy;
                old_valid = 1;

                mpm_share_expectation();

                SHOW_PROGRESS(iterate);
                RET_ON_ERR(vbptr->update_hyperparams());

                iterate++;
            }

            if (itemp == 1.0) {
                break;
            }
            itemp *= itemp_rate;
            if (itemp >= 1.0) {
                itemp = 1.0;
            }
        }

        SHOW_PROGRESS_TAIL(converged, iterate, free_energy);

        if (r == 0 || free_energy > vbptr->free_energy) {
            vbptr->free_energy = free_energy;
            vbptr->iterate     = iterate;

            saved = (r < num_restart - 1);
            if (saved) {
                save_hyperparams();
            }
        }
    }

    if (saved) {
        restore_hyperparams();
    }

    transfer_hyperparams();

    return BP_TRUE;
}

int mps_run_vbem(VBEM_ENG_PTR vbptr)
{
    int     r, iterate, old_valid, converged, saved=0;
    double  free_energy, old_free_energy=0.0;
    double  l2;

    config_vbem(vbptr);

    for (r = 0; r < num_restart; r++) {
        mps_bcast_inside_h();
        mps_bcast_smooth();
        clear_sw_msg_send();

        itemp = daem ? itemp_init : 1.0;
        iterate = 0;

        while (1) {
            old_valid = 0;

            while (1) {
                RET_ON_ERR(vbptr->compute_pi());
                RET_ON_ERR(vbptr->compute_inside());
                RET_ON_ERR(vbptr->examine_inside());

                if (failure_observed) {
                    inside_failure = mp_sum_value(inside_failure);
                }

                l2 = vbptr->compute_likelihood() / itemp;
                free_energy = mp_sum_value(l2);

                mp_debug("local free_energy = %.9f, free_energy = %.9f", l2, free_energy);

                converged = (old_valid && free_energy - old_free_energy <= prism_epsilon);
                if (converged || REACHED_MAX_ITERATE(iterate)) {
                    break;
                }

                old_free_energy = free_energy;
                old_valid = 1;

                RET_ON_ERR(vbptr->compute_expectation());
                mps_share_expectation();

                RET_ON_ERR(vbptr->update_hyperparams());
                iterate++;
            }

            if (itemp == 1.0) {
                break;
            }
            itemp *= itemp_rate;
            if (itemp >= 1.0) {
                itemp = 1.0;
            }
        }

        if (r == 0 || free_energy > vbptr->free_energy) {
            vbptr->free_energy = free_energy;
            saved = (r < num_restart - 1);
            if (saved) {
                save_hyperparams();
            }
        }
    }

    if (saved) {
        restore_hyperparams();
    }

    transfer_hyperparams();

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

#endif /* MPI */
