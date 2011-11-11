/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifdef MPI

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "core/error.h"
#include "up/up.h"
#include "up/em.h"
#include "up/em_aux.h"
#include "up/em_aux_ml.h"
#include "up/em_ml.h"
#include "up/graph.h"
#include "up/flags.h"
#include "up/util.h"
#include "mp/mp.h"
#include "mp/mp_core.h"
#include "mp/mp_em_aux.h"
#include <mpi.h>

/*------------------------------------------------------------------------*/

void mpm_share_preconds_em(int *smooth)
{
    int ivals[4];
    int ovals[4];

    ivals[0] = sw_msg_size;
    ivals[1] = 0;
    ivals[2] = 0;
    ivals[3] = *smooth;

    MPI_Allreduce(ivals, ovals, 4, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    sw_msg_size      = ovals[0];
    num_goals     = ovals[1];
    failure_observed = ovals[2];
    *smooth          = ovals[3];

    mp_debug("msgsize=%d, #goals=%d, failure=%s, smooth = %s",
             sw_msg_size, num_goals, failure_observed ? "on" : "off", *smooth ? "on" : "off");

    alloc_sw_msg_buffers();
    mpm_bcast_fixed();
    if (*smooth) {
        mpm_bcast_smooth();
    }
}

void mps_share_preconds_em(int *smooth)
{
    int ivals[4];
    int ovals[4];

    ivals[0] = 0;
    ivals[1] = num_goals;
    ivals[2] = failure_observed;
    ivals[3] = 0;

    MPI_Allreduce(ivals, ovals, 4, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    sw_msg_size      = ovals[0];
    num_goals     = ovals[1];
    failure_observed = ovals[2];
    *smooth          = ovals[3];

    mp_debug("msgsize=%d, #goals=%d, failure=%s, smooth = %s",
             sw_msg_size, num_goals, failure_observed ? "on" : "off", *smooth ? "on" : "off");

    alloc_sw_msg_buffers();
    mps_bcast_fixed();
    if (*smooth) {
        mps_bcast_smooth();
    }
}

/*------------------------------------------------------------------------*/

int mpm_run_em(EM_ENG_PTR emptr)
{
    int     r, iterate, old_valid, converged, saved=0;
    double  likelihood, log_prior;
    double  lambda, old_lambda=0.0;

    config_em(emptr);

    for (r = 0; r < num_restart; r++) {
        SHOW_PROGRESS_HEAD("#em-iters", r);

        initialize_params();
        mpm_bcast_inside();
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

                if (failure_observed) {
                    inside_failure = mp_sum_value(0.0);
                }

                log_prior  = emptr->smooth ? emptr->compute_log_prior() : 0.0;
                lambda = mp_sum_value(log_prior);
                likelihood = lambda - log_prior;

                mp_debug("local lambda = %.9f, lambda = %.9f", log_prior, lambda);

                if (verb_em) {
                    if (emptr->smooth) {
                        prism_printf("Iteration #%d:\tlog_likelihood=%.9f\tlog_prior=%.9f\tlog_post=%.9f\n", iterate, likelihood, log_prior, lambda);
                    }
                    else {
                        prism_printf("Iteration #%d:\tlog_likelihood=%.9f\n", iterate, likelihood);
                    }
                }

                if (!isfinite(lambda)) {
                    emit_internal_error("invalid log likelihood or log post: %s (at iterateion #%d)",
                                        isnan(lambda) ? "NaN" : "infinity", iterate);
                    RET_ERR(ierr_invalid_likelihood);
                }
                if (old_valid && old_lambda - lambda > prism_epsilon) {
                    emit_error("log likelihood or log post decreased [old: %.9f, new: %.9f] (at iteration #%d)",
                               old_lambda, lambda, iterate);
                    RET_ERR(err_invalid_likelihood);
                }
                if (itemp == 1.0 && likelihood > 0.0) {
                    emit_error("log likelihood greater than zero [value: %.9f] (at iteration #%d)",
                               likelihood, iterate);
                    RET_ERR(err_invalid_likelihood);
                }

                converged = (old_valid && lambda - old_lambda <= prism_epsilon);
                if (converged || REACHED_MAX_ITERATE(iterate)) {
                    break;
                }

                old_lambda = lambda;
                old_valid  = 1;

                mpm_share_expectation();

                SHOW_PROGRESS(iterate);
                RET_ON_ERR(emptr->update_params());
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

        SHOW_PROGRESS_TAIL(converged, iterate, lambda);

        if (r == 0 || lambda > emptr->lambda) {
            emptr->lambda     = lambda;
            emptr->likelihood = likelihood;
            emptr->iterate    = iterate;

            saved = (r < num_restart - 1);
            if (saved) {
                save_params();
            }
        }
    }

    if (saved) {
        restore_params();
    }

    emptr->bic = compute_bic(emptr->likelihood);
    emptr->cs  = emptr->smooth ? compute_cs(emptr->likelihood) : 0.0;

    return BP_TRUE;
}

int mps_run_em(EM_ENG_PTR emptr)
{
    int     r, iterate, old_valid, converged, saved=0;
    double  likelihood;
    double  lambda, old_lambda=0.0;

    config_em(emptr);

    for (r = 0; r < num_restart; r++) {
        mps_bcast_inside();
        clear_sw_msg_send();
        itemp = daem ? itemp_init : 1.0;
        iterate = 0;

        while (1) {
            old_valid = 0;

            while (1) {
                RET_ON_ERR(emptr->compute_inside());
                RET_ON_ERR(emptr->examine_inside());

                if (failure_observed) {
                    inside_failure = mp_sum_value(inside_failure);
                }

                likelihood = emptr->compute_likelihood();
                lambda = mp_sum_value(likelihood);

                mp_debug("local lambda = %.9f, lambda = %.9f", likelihood, lambda);

                converged = (old_valid && lambda - old_lambda <= prism_epsilon);
                if (converged || REACHED_MAX_ITERATE(iterate)) {
                    break;
                }

                old_lambda = lambda;
                old_valid  = 1;

                RET_ON_ERR(emptr->compute_expectation());
                mps_share_expectation();

                RET_ON_ERR(emptr->update_params());
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

        if (r == 0 || lambda > emptr->lambda) {
            emptr->lambda = lambda;
            saved = (r < num_restart - 1);
            if (saved) {
                save_params();
            }
        }
    }

    if (saved) {
        restore_params();
    }

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

#endif /* MPI */
