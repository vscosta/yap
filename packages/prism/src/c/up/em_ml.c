/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "up/up.h"
#include "up/graph_aux.h"
#include "up/em.h"
#include "up/em_aux.h"
#include "up/em_aux_ml.h"
#include "up/flags.h"
#include "up/util.h"

void config_em(EM_ENG_PTR em_ptr);
int run_em(EM_ENG_PTR em_ptr);

/*------------------------------------------------------------------------*/

void config_em(EM_ENG_PTR em_ptr)
{
	if (log_scale) {
        em_ptr->compute_inside      = daem ? compute_daem_inside_scaling_log_exp : compute_inside_scaling_log_exp;
        em_ptr->examine_inside      = examine_inside_scaling_log_exp;
        em_ptr->compute_expectation = compute_expectation_scaling_log_exp;
        em_ptr->compute_likelihood  = compute_likelihood_scaling_log_exp;
        em_ptr->compute_log_prior   = daem ? compute_daem_log_prior : compute_log_prior;
        em_ptr->update_params       = em_ptr->smooth ? update_params_smooth : update_params;
	}
	else {
        em_ptr->compute_inside      = daem ? compute_daem_inside_scaling_none : compute_inside_scaling_none;
        em_ptr->examine_inside      = examine_inside_scaling_none;
        em_ptr->compute_expectation = compute_expectation_scaling_none;
        em_ptr->compute_likelihood  = compute_likelihood_scaling_none;
        em_ptr->compute_log_prior   = daem ? compute_daem_log_prior : compute_log_prior;
        em_ptr->update_params       = em_ptr->smooth ? update_params_smooth : update_params;
	}
}

/*------------------------------------------------------------------------*/

int run_em(EM_ENG_PTR em_ptr)
{
    int     r, iterate, old_valid, converged, saved = 0;
    double  likelihood, log_prior;
    double  lambda, old_lambda = 0.0;

    config_em(em_ptr);

    for (r = 0; r < num_restart; r++) {
        SHOW_PROGRESS_HEAD("#em-iters", r);

        initialize_params();
        itemp = daem ? itemp_init : 1.0;
        iterate = 0;

        /* [21 Aug 2007, by yuizumi]
         * while-loop for inversed temperature (DAEM).  Note that this
         * loop is evaluated only once for EM without annealing, since
         * itemp initially set to 1.0 by the code above.
         */
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

                RET_ON_ERR(em_ptr->compute_inside());
                RET_ON_ERR(em_ptr->examine_inside());

                likelihood = em_ptr->compute_likelihood();
                log_prior  = em_ptr->smooth ? em_ptr->compute_log_prior() : 0.0;
                lambda = likelihood + log_prior;

                if (verb_em) {
                    if (em_ptr->smooth) {
                        prism_printf("Iteration #%d:\tlog_likelihood=%.9f\tlog_prior=%.9f\tlog_post=%.9f\n", iterate, likelihood, log_prior, lambda);
                    }
                    else {
                        prism_printf("Iteration #%d:\tlog_likelihood=%.9f\n", iterate, likelihood);
                    }
                }

                if (debug_level) {
                    prism_printf("After I-step[%d]:\n", iterate);
                    prism_printf("likelihood = %.9f\n", likelihood);
                    print_egraph(debug_level, PRINT_EM);
                }

                if (!isfinite(lambda)) {
                    emit_internal_error("invalid log likelihood or log post: %s (at iteration #%d)",
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

                RET_ON_ERR(em_ptr->compute_expectation());

                if (debug_level) {
                    prism_printf("After O-step[%d]:\n", iterate);
                    print_egraph(debug_level, PRINT_EM);
                }

                SHOW_PROGRESS(iterate);
                RET_ON_ERR(em_ptr->update_params());
                iterate++;
            }

            /* [21 Aug 2007, by yuizumi]
             * Note that 1.0 can be represented exactly in IEEE 754.
             */
            if (itemp == 1.0) {
                break;
            }
            itemp *= itemp_rate;
            if (itemp >= 1.0) {
                itemp = 1.0;
            }
        }

        SHOW_PROGRESS_TAIL(converged, iterate, lambda);

        if (r == 0 || lambda > em_ptr->lambda) {
            em_ptr->lambda     = lambda;
            em_ptr->likelihood = likelihood;
            em_ptr->iterate    = iterate;

            saved = (r < num_restart - 1);
            if (saved) {
                save_params();
            }
        }
    }

    if (saved) {
        restore_params();
    }

    em_ptr->bic = compute_bic(em_ptr->likelihood);
    em_ptr->cs  = em_ptr->smooth ? compute_cs(em_ptr->likelihood) : 0.0;

    return BP_TRUE;
}
