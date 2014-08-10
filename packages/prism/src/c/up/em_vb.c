/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "core/random.h"
#include "up/up.h"
#include "up/graph_aux.h"
#include "up/em.h"
#include "up/em_aux.h"
#include "up/em_aux_ml.h"
#include "up/em_aux_vb.h"
#include "up/flags.h"
#include "up/util.h"

void config_vbem(VBEM_ENG_PTR vb_ptr);
int run_vbem(VBEM_ENG_PTR vb_ptr);

/*------------------------------------------------------------------------*/

void config_vbem(VBEM_ENG_PTR vb_ptr)
{
	if (log_scale) {
        vb_ptr->compute_pi             = compute_pi_scaling_log_exp;
        vb_ptr->compute_inside         = daem ? compute_daem_inside_vb_scaling_log_exp : compute_inside_vb_scaling_log_exp;
        vb_ptr->examine_inside         = examine_inside_scaling_log_exp;
        vb_ptr->compute_expectation    = compute_expectation_scaling_log_exp;
        vb_ptr->compute_free_energy_l0 = daem ? compute_daem_free_energy_l0 : compute_free_energy_l0;
        vb_ptr->compute_free_energy_l1 = daem ? compute_daem_free_energy_l1_scaling_log_exp : compute_free_energy_l1_scaling_log_exp;
        vb_ptr->compute_likelihood     = compute_likelihood_scaling_log_exp;
        vb_ptr->update_hyperparams     = daem ? update_daem_hyperparams : update_hyperparams;
	}
	else {
        vb_ptr->compute_pi             = compute_pi_scaling_none;
        vb_ptr->compute_inside         = daem ? compute_daem_inside_vb_scaling_none : compute_inside_vb_scaling_none;
        vb_ptr->examine_inside         = examine_inside_scaling_none;
        vb_ptr->compute_expectation    = compute_expectation_scaling_none;
        vb_ptr->compute_free_energy_l0 = daem ? compute_daem_free_energy_l0 : compute_free_energy_l0;
        vb_ptr->compute_free_energy_l1 = daem ? compute_daem_free_energy_l1_scaling_none : compute_free_energy_l1_scaling_none;
        vb_ptr->compute_likelihood     = compute_likelihood_scaling_none;
        vb_ptr->update_hyperparams     = daem ? update_daem_hyperparams : update_hyperparams;
	}
}

/*------------------------------------------------------------------------*/

int run_vbem(VBEM_ENG_PTR vb_ptr)
{
    int     r, iterate, old_valid, converged, saved = 0;
    double  free_energy, old_free_energy = 0.0;
    double  l0, l1, l2;

    config_vbem(vb_ptr);

    for (r = 0; r < num_restart; r++) {
        SHOW_PROGRESS_HEAD("#vbem-iters", r);

        initialize_hyperparams();
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

                RET_ON_ERR(vb_ptr->compute_pi());
                RET_ON_ERR(vb_ptr->compute_inside());
                RET_ON_ERR(vb_ptr->examine_inside());

                /* compute free_energy */
                l0 = vb_ptr->compute_free_energy_l0();
                l1 = vb_ptr->compute_free_energy_l1();
                l2 = vb_ptr->compute_likelihood() / itemp; /* itemp == 1.0 for non-DAEM */
                free_energy = l0 - l1 + l2;

                if (verb_em) {
                    prism_printf("Iteration #%d:\tfree_energy=%.9f\n", iterate, free_energy);
                }

                if (debug_level) {
                    prism_printf("After I-step[%d]:\n", iterate);
                    prism_printf("free_energy = %.9f\n", free_energy);
                    print_egraph(debug_level, PRINT_VBEM);
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
                    emit_error("variational free energy exceeds zero [value: %.9f] (at iteration #%d)",
                               free_energy, iterate);
                    RET_ERR(err_invalid_free_energy);
                }

                converged = (old_valid && free_energy - old_free_energy <= prism_epsilon);
                if (converged || REACHED_MAX_ITERATE(iterate)) {
                    break;
                }

                old_free_energy = free_energy;
                old_valid = 1;

                RET_ON_ERR(vb_ptr->compute_expectation());

                if (debug_level) {
                    prism_printf("After O-step[%d]:\n", iterate);
                    print_egraph(debug_level, PRINT_VBEM);
                }

                SHOW_PROGRESS(iterate);
                RET_ON_ERR(vb_ptr->update_hyperparams());

                if (debug_level) {
                    prism_printf("After update[%d]:\n", iterate);
                    print_egraph(debug_level, PRINT_VBEM);
                }

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

        SHOW_PROGRESS_TAIL(converged, iterate, free_energy);

        if (r == 0 || free_energy > vb_ptr->free_energy) {
            vb_ptr->free_energy = free_energy;
            vb_ptr->iterate     = iterate;

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
