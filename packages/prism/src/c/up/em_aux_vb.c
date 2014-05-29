/* -*- c-basic-offset: 2; tab-width: 8 -*- */

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "core/random.h"
#include "core/gamma.h"
#include "up/up.h"
#include "up/graph.h"
#include "up/em_aux.h"
#include "up/em_aux_ml.h"
#include "up/flags.h"
#include "up/util.h"

int check_smooth_vb(void);
int compute_daem_inside_vb_scaling_none(void);
double compute_free_energy_l0(void);
double compute_free_energy_l1_scaling_none(void);
double compute_free_energy_l1_scaling_log_exp(void);
double compute_daem_free_energy_l1_scaling_none(void);
double compute_daem_free_energy_l1_scaling_log_exp(void);
void initialize_hyperparams(void);
int update_hyperparams(void);
int update_daem_hyperparams(void);
void save_hyperparams(void);
void restore_hyperparams(void);
void transfer_hyperparams(void);
void get_param_means(void);
int compute_pi_scaling_none(void);
int compute_pi_scaling_log_exp(void);
int compute_inside_vb_scaling_none(void);
int compute_inside_vb_scaling_log_exp(void);
int compute_daem_inside_vb_scaling_log_exp(void);
double compute_daem_free_energy_l0(void);

/*------------------------------------------------------------------------*/

/* Just check if there is any negative hyperparameter */
int check_smooth_vb(void)
{
    int i;
    SW_INS_PTR sw_ins_ptr;

    for (i = 0; i < occ_switch_tab_size; i++) {
        sw_ins_ptr = occ_switches[i];
        while (sw_ins_ptr != NULL) {
	    if (sw_ins_ptr->smooth_prolog <= -1.0) {
                emit_internal_error("illegal hyperparameters");
                RET_INTERNAL_ERR;
            }
            sw_ins_ptr = sw_ins_ptr->next;
        }
    }

    transfer_hyperparams_prolog();

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

void initialize_hyperparams(void)
{
    int i;
    SW_INS_PTR ptr;
    double p,r;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        while (ptr != NULL) {
            ptr->smooth = ptr->smooth_prolog;
            ptr = ptr->next;
        }
    }

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];

        if (ptr->fixed_h > 0) {
            while (ptr != NULL) {
                ptr->inside_h = ptr->smooth + 1.0;
                ptr->total_expect = 0.0;
                ptr = ptr->next;
            }
        }
        else {
            p = 1.0 / num_sw_vals[i];
            while (ptr != NULL) {
                r = random_gaussian(0.0, std_ratio * p);
                ptr->inside_h =
		  (ptr->smooth + 1.0 < EPS) ? EPS : ptr->smooth + 1.0;
                ptr->inside_h *= (1.0 + fabs(r));
                ptr->smooth = ptr->inside_h - 1.0;
                ptr->total_expect = 0.0;
                ptr = ptr->next;
            }
        }
    }
}

/*------------------------------------------------------------------------*/

int compute_pi_scaling_none(void)
{
    int i;
    SW_INS_PTR ptr;
    double alpha_sum, psi0;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];

        alpha_sum = 0.0;
        while (ptr != NULL) {
            alpha_sum += ptr->inside_h;
            ptr = ptr->next;
        }
        psi0 = digamma(alpha_sum);

        ptr = occ_switches[i];
        while (ptr != NULL) {
            ptr->pi = exp(digamma(ptr->inside_h) - psi0);
            ptr = ptr->next;
        }
    }

    return BP_TRUE;
}

int compute_pi_scaling_log_exp(void)
{
    int i;
    SW_INS_PTR ptr;
    double alpha_sum, psi0;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];

        alpha_sum = 0.0;
        while (ptr != NULL) {
            alpha_sum += ptr->inside_h;
            ptr = ptr->next;
        }
        psi0 = digamma(alpha_sum);

        ptr = occ_switches[i];
        while (ptr != NULL) {
            ptr->pi = digamma(ptr->inside_h) - psi0;
            ptr = ptr->next;
        }
    }

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

int compute_inside_vb_scaling_none(void)
{
    int i,k;
    double sum,this_path_inside;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        sum = 0.0;
        path_ptr = eg_ptr->path_ptr;
        if (path_ptr == NULL) sum = 1.0;

        while (path_ptr != NULL) {
            this_path_inside = 1.0;
            for (k = 0; k < path_ptr->children_len; k++) {
                this_path_inside *= path_ptr->children[k]->inside;
            }
            for (k = 0; k < path_ptr->sws_len; k++) {
                this_path_inside *= path_ptr->sws[k]->pi;
            }
            path_ptr->inside = this_path_inside;
            sum += this_path_inside;
            path_ptr = path_ptr->next;
        }

        eg_ptr->inside = sum;
    }

    return BP_TRUE;
}

int compute_inside_vb_scaling_log_exp(void)
{
    int i,k,u;
    double sum, this_path_inside, first_path_inside = 0.0, sum_rest;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        sum = 0.0;
        path_ptr = eg_ptr->path_ptr;

        if (path_ptr == NULL) {
            sum = 0.0;
        }
        else {
            sum_rest = 0.0;
            u = 0;
            while (path_ptr != NULL) {
                this_path_inside = 0.0;
                for (k = 0; k < path_ptr->children_len; k++) {
                    this_path_inside += path_ptr->children[k]->inside;
                }
                for (k = 0; k < path_ptr->sws_len; k++) {
                    this_path_inside += path_ptr->sws[k]->pi; /* log-scale */
                }
                path_ptr->inside = this_path_inside;

                if (u == 0) {
                    first_path_inside = this_path_inside;
                    sum_rest += 1.0;
                }
                else if (this_path_inside - first_path_inside >= log(HUGE_PROB)) {
                    sum_rest *= exp(first_path_inside - this_path_inside);
                    first_path_inside = this_path_inside;
                    sum_rest += 1.0;
                }
                else {
                    sum_rest += exp(this_path_inside - first_path_inside);
                }
                path_ptr = path_ptr->next;
                u++;
            }
            sum = first_path_inside + log(sum_rest);
        }

        eg_ptr->inside = sum;
    }

    return BP_TRUE;
}

int compute_daem_inside_vb_scaling_none(void)
{
    int i,k;
    double sum,this_path_inside;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        sum = 0.0;
        path_ptr = eg_ptr->path_ptr;
        if (path_ptr == NULL) sum = 1.0;

        while (path_ptr != NULL) {
            this_path_inside = 1.0;
            for (k = 0; k < path_ptr->children_len; k++) {
                this_path_inside *= path_ptr->children[k]->inside;
            }
            for (k = 0; k < path_ptr->sws_len; k++) {
                this_path_inside *= pow(path_ptr->sws[k]->pi,itemp);
            }
            path_ptr->inside = this_path_inside;
            sum += this_path_inside;
            path_ptr = path_ptr->next;
        }

        eg_ptr->inside = sum;
    }

    return BP_TRUE;
}

int compute_daem_inside_vb_scaling_log_exp(void)
{
    int i,k,u;
    double sum, this_path_inside, first_path_inside = 0.0, sum_rest;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        sum = 0.0;
        path_ptr = eg_ptr->path_ptr;

        if (path_ptr == NULL) {
            sum = 0.0;
        }
        else {
            sum_rest = 0.0;
            u = 0;
            while (path_ptr != NULL) {
                this_path_inside = 0.0;
                for (k = 0; k < path_ptr->children_len; k++) {
                    this_path_inside += path_ptr->children[k]->inside;
                }
                for (k = 0; k < path_ptr->sws_len; k++) {
                    this_path_inside += itemp * path_ptr->sws[k]->pi;
                }
                path_ptr->inside = this_path_inside;

                if (u == 0) {
                    first_path_inside = this_path_inside;
                    sum_rest += 1.0;
                }
                else if (this_path_inside - first_path_inside >= log(HUGE_PROB)) {
                    sum_rest *= exp(first_path_inside - this_path_inside);
                    first_path_inside = this_path_inside;
                    sum_rest += 1.0;
                }
                else {
                    sum_rest += exp(this_path_inside - first_path_inside);
                }
                path_ptr = path_ptr->next;
                u++;
            }
            sum = first_path_inside + log(sum_rest);
        }

        eg_ptr->inside = sum;
    }

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

/* [27 Aug 2007, by yuizumi]
 * A variational free energy F is given by:
 *     F = F0 - F1 + L'
 * where:
 *     F0 = compute_[daem_]free_energy_l0()
 *     F1 = compute_[daem_]free_energy_l1_scaling_{none|log_exp}()
 *     L' = compute_likelihood() / itemp
 */

double compute_free_energy_l0(void)
{
    double l0 = 0.0;
    double smooth_sum;
    SW_INS_PTR ptr;
    int i;

    for (i = 0; i < occ_switch_tab_size; i++) {
        smooth_sum = 0.0;
        ptr = occ_switches[i];

        while (ptr != NULL) {
            smooth_sum += (ptr->smooth + 1.0);
            ptr = ptr->next;
        }
        l0 += lngamma(smooth_sum);

        smooth_sum = 0.0;
        ptr = occ_switches[i];
        while (ptr != NULL) {
            smooth_sum += (ptr->inside_h);
            ptr = ptr->next;
        }
        l0 -= lngamma(smooth_sum);

        ptr = occ_switches[i];
        while (ptr != NULL) {
            l0 += lngamma(ptr->inside_h);
            l0 -= lngamma(ptr->smooth + 1.0);
            ptr = ptr->next;
        }
    }

    return l0;
}

double compute_daem_free_energy_l0(void)
{
    double l0 = 0.0;
    double smooth_sum;
    SW_INS_PTR ptr;
    int i;

    for (i = 0; i < occ_switch_tab_size; i++) {
        smooth_sum = 0.0;
        ptr = occ_switches[i];

        while (ptr != NULL) {
            smooth_sum += (ptr->smooth + 1.0);
            ptr = ptr->next;
        }
        l0 += lngamma(smooth_sum);

        smooth_sum = 0.0;
        ptr = occ_switches[i];
        while (ptr != NULL) {
            smooth_sum += (ptr->inside_h);
            ptr = ptr->next;
        }
        l0 -= lngamma(smooth_sum) / itemp;

        ptr = occ_switches[i];
        while (ptr != NULL) {
            l0 += lngamma(ptr->inside_h) / itemp;
            l0 -= lngamma(ptr->smooth + 1.0);
            ptr = ptr->next;
        }
    }

    return l0;
}

double compute_free_energy_l1_scaling_none(void)
{
    double l1 = 0.0;
    SW_INS_PTR ptr;
    int i;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        while (ptr != NULL) {
            l1 += ((ptr->inside_h - 1.0) - ptr->smooth) * log(ptr->pi);
            ptr = ptr->next;
        }
    }

    return l1;
}

double compute_free_energy_l1_scaling_log_exp(void)
{
    double l1 = 0.0;
    SW_INS_PTR ptr;
    int i;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        while (ptr != NULL) {
            /* pi is in log-scale */
            l1 += (ptr->inside_h - (ptr->smooth + 1.0)) * ptr->pi;
            ptr = ptr->next;
        }
    }

    return l1;
}

double compute_daem_free_energy_l1_scaling_none(void)
{
    double l1 = 0.0;
    SW_INS_PTR ptr;
    int i;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        while (ptr != NULL) {
            l1 += ((ptr->inside_h - 1.0) / itemp - ptr->smooth) * log(ptr->pi);
            ptr = ptr->next;
        }
    }

    return l1;
}

double compute_daem_free_energy_l1_scaling_log_exp(void)
{
    double l1 = 0.0;
    SW_INS_PTR ptr;
    int i;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        while (ptr != NULL) {
            /* pi is in log-scale */
            l1 += ((ptr->inside_h - 1.0) / itemp - ptr->smooth) * ptr->pi;
            ptr = ptr->next;
        }
    }

    return l1;
}

/*------------------------------------------------------------------------*/

int update_hyperparams(void)
{
    int i;
    SW_INS_PTR ptr;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        if (ptr->fixed_h > 0) continue;

        while (ptr != NULL) {
            ptr->inside_h = ptr->total_expect + ptr->smooth + 1.0;
            ptr = ptr->next;
        }
    }

    return BP_TRUE;
}

int update_daem_hyperparams(void)
{
    int i;
    SW_INS_PTR ptr;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        if (ptr->fixed_h > 0) continue;

        while (ptr != NULL) {
            ptr->inside_h = itemp * (ptr->total_expect + ptr->smooth) + 1.0;
            ptr = ptr->next;
        }
    }

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

void save_hyperparams(void)
{
    int i;
    SW_INS_PTR ptr;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        if (ptr->fixed_h > 0) continue;
        while (ptr != NULL) {
            ptr->best_inside_h = ptr->inside_h;
            ptr = ptr->next;
        }
    }
}

void restore_hyperparams(void)
{
    int i;
    SW_INS_PTR ptr;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        if (ptr->fixed_h > 0) continue;
        while (ptr != NULL) {
            ptr->inside_h = ptr->best_inside_h;
            ptr = ptr->next;
        }
    }
}

void transfer_hyperparams(void)
{
    int i;
    SW_INS_PTR ptr;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        if (ptr->fixed_h > 0) continue;

        while (ptr != NULL) {
            ptr->smooth = ptr->inside_h - 1.0;
            ptr = ptr->next;
        }
    }
}

/*------------------------------------------------------------------------*/

void get_param_means(void)
{
    int i;
    SW_INS_PTR ptr;
    double sum;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        if (ptr->fixed > 0) continue;

        sum = 0.0;
        while (ptr != NULL) {
            sum += ptr->inside_h;
            ptr = ptr->next;
        }

        ptr = occ_switches[i];
        while (ptr != NULL) {
            ptr->inside = ptr->inside_h / sum;
            ptr = ptr->next;
        }
    }
}

/*------------------------------------------------------------------------*/
