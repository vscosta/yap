/* -*- c-basic-offset: 2; tab-width: 8 -*- */

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "core/random.h"
#include "core/gamma.h"
#include "up/up.h"
#include "up/graph.h"
#include "up/flags.h"
#include "up/em_aux.h"
#include "up/util.h"

int check_smooth(int *smooth);
void initialize_params(void);
int compute_inside_scaling_none(void);
int compute_inside_scaling_none(void);
int compute_daem_inside_scaling_none(void);
int compute_daem_inside_scaling_log_exp(void);
int examine_inside_scaling_none(void);
int examine_inside_scaling_log_exp(void);
int compute_expectation_scaling_none(void);
int compute_inside_scaling_log_exp(void);
int compute_expectation_scaling_log_exp(void);
double compute_daem_log_prior(void);
double compute_likelihood_scaling_none(void);
void release_num_sw_vals(void);
void transfer_hyperparams_prolog(void);
double compute_likelihood_scaling_log_exp(void);
void save_params(void);
void restore_params(void);
double compute_log_prior(void);
double compute_bic(double likelihood);
double compute_cs(double likelihood);
int update_params(void);

int update_params_smooth(void);
/*------------------------------------------------------------------------*/

/* We check if all smoothing constants are positive (MAP),
 * or all smoothing constants are zero.  If some are positive,
 * but the others are zero, die immediately.  We also check
 * if there exist parameters fixed at zero in MAP estimation.
 */
int check_smooth(int *smooth)
{
    /*
      q = +4 : found non-zero smoothing constants
          +2 : found zero-valued smoothing constants
          +1 : found parameters fixed to zero
    */
    int i, q = 0;
    SW_INS_PTR sw_ins_ptr;

    for (i = 0; i < occ_switch_tab_size; i++) {
        sw_ins_ptr = occ_switches[i];
        while (sw_ins_ptr != NULL) {
	    if (sw_ins_ptr->smooth_prolog < 0) {
		emit_error("negative delta values in MAP estimation");
		RET_ERR(err_invalid_numeric_value);
	    }

            q |= (sw_ins_ptr->smooth_prolog < TINY_PROB) ? 2 : 4;
            q |= (sw_ins_ptr->fixed && sw_ins_ptr->inside < TINY_PROB) ? 1 : 0;

            sw_ins_ptr = sw_ins_ptr->next;
        }
    }

    switch (q) {
    case 0: /* p.counts = (none), w/o  0-valued params */
    case 1: /* p.counts = (none), with 0-valued params */
        emit_internal_error("unexpected case in check_smooth()");
        RET_ERR(ierr_unmatched_branches);
	break;
    case 2: /* p.counts = 0 only, w/o  0-valued params */
    case 3: /* p.counts = 0 only, with 0-valued params */
        *smooth = 0;
        break;
    case 4: /* p.counts = + only, w/o  0-valued params */
        *smooth = 1;
        break;
    case 5: /* p.counts = + only, with 0-valued params */
        emit_error("parameters fixed to zero in MAP estimation");
        RET_ERR(err_invalid_numeric_value);
        break;
    case 6: /* p.counts = (both), w/o  0-valued params */
    case 7: /* p.counts = (both), with 0-valued params */
        emit_error("mixture of zero and non-zero pseudo counts");
        RET_ERR(err_invalid_numeric_value);
    }

    transfer_hyperparams_prolog();

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

static void initialize_params_noisy_uniform(void)
{
    int i;
    SW_INS_PTR ptr;
    double sum,p;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];

        if (ptr->fixed > 0) continue;

        p = 1.0 / num_sw_vals[i];
        sum = 0.0;
        while (ptr != NULL) {
            ptr->inside = random_gaussian(p, std_ratio * p);
            if (ptr->inside < INIT_PROB_THRESHOLD)
                ptr->inside = INIT_PROB_THRESHOLD;
            sum += ptr->inside;
            ptr = ptr->next;
        }
        ptr = occ_switches[i];
        while (ptr != NULL) {  /* normalize */
            ptr->inside = ptr->inside / sum;
            ptr = ptr->next;
        }
    }
}

static void initialize_params_random(void)
{
    int i;
    SW_INS_PTR ptr;
    double sum,p;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];

        if (ptr->fixed > 0) continue;

        p = 1.0 / num_sw_vals[i];
        sum = 0.0;
        while (ptr != NULL) {
            sum += (ptr->inside = p + random_float());
            ptr = ptr->next;
        }
        ptr = occ_switches[i];
        while (ptr != NULL) { /* normalize */
            ptr->inside = ptr->inside / sum;
            ptr = ptr->next;
        }
    }
}

void initialize_params(void)
{
    if (init_method == 1)
        initialize_params_noisy_uniform();
    if (init_method == 2)
        initialize_params_random();
}

/*------------------------------------------------------------------------*/

int compute_inside_scaling_none(void)
{
    int i,k;
    double sum,this_path_inside;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        sum = 0.0;
        path_ptr = eg_ptr->path_ptr;
        if (path_ptr == NULL)
            sum = 1.0; /* path_ptr should not be NULL; but it happens */
        while (path_ptr != NULL) {
            this_path_inside = 1.0;
            for (k = 0; k < path_ptr->children_len; k++) {
                this_path_inside *= path_ptr->children[k]->inside;
            }
            for (k = 0; k < path_ptr->sws_len; k++) {
                this_path_inside *= path_ptr->sws[k]->inside;
            }
            path_ptr->inside = this_path_inside;
            sum += this_path_inside;
            path_ptr = path_ptr->next;
        }

        eg_ptr->inside = sum;
    }

    return BP_TRUE;
}

int compute_inside_scaling_log_exp(void)
{
    int i,k,u;
    double sum, this_path_inside, first_path_inside = 0.0, sum_rest;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        path_ptr = eg_ptr->path_ptr;
        if (path_ptr == NULL) {
            sum = 0.0; /* path_ptr should not be NULL; but it happens */
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
                    this_path_inside += log(path_ptr->sws[k]->inside);
                }
                path_ptr->inside = this_path_inside;
                if (u == 0) {
                    first_path_inside = this_path_inside;
                    sum_rest += 1.0;
                }
                else if (this_path_inside - first_path_inside >= log(HUGE_PROB)) {
                    sum_rest *= exp(first_path_inside - this_path_inside);
                    first_path_inside = this_path_inside;
                    sum_rest += 1.0; /* maybe sum_rest gets 1.0 */
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

int compute_daem_inside_scaling_none(void)
{
    int i,k;
    double sum,this_path_inside;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        sum = 0.0;
        path_ptr = eg_ptr->path_ptr;
        if (path_ptr == NULL)
            sum = 1.0; /* path_ptr should not be NULL; but it happens */
        while (path_ptr != NULL) {
            this_path_inside = 1.0;
            for (k = 0; k < path_ptr->children_len; k++) {
                this_path_inside *= path_ptr->children[k]->inside;
            }
            for (k = 0; k < path_ptr->sws_len; k++) {
                this_path_inside *= pow(path_ptr->sws[k]->inside, itemp);
            }
            path_ptr->inside = this_path_inside;
            sum += this_path_inside;
            path_ptr = path_ptr->next;
        }

        eg_ptr->inside = sum;
    }

    return BP_TRUE;
}

int compute_daem_inside_scaling_log_exp(void)
{
    int i,k,u;
    double sum, this_path_inside, first_path_inside = 0.0, sum_rest;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        path_ptr = eg_ptr->path_ptr;
        if (path_ptr == NULL) {
            sum = 0.0; /* path_ptr should not be NULL; but it happens */
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
                    this_path_inside += itemp * log(path_ptr->sws[k]->inside);
                }
                path_ptr->inside = this_path_inside;
                if (u == 0) {
                    first_path_inside = this_path_inside;
                    sum_rest += 1.0;
                }
                else if (this_path_inside - first_path_inside >= log(HUGE_PROB)) {
                    sum_rest *= exp(first_path_inside - this_path_inside);
                    first_path_inside = this_path_inside;
                    sum_rest += 1.0; /* maybe sum_rest gets 1.0 */
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

int examine_inside_scaling_none(void)
{
    int i;
    double inside;

    inside_failure = 0.0;

    for (i = 0; i < num_roots; i++) {
        inside = expl_graph[roots[i]->id]->inside;
        if (i == failure_root_index) {
            inside_failure = inside;
            if (!(1.0 - inside_failure > 0.0)) {
                emit_error("Probability of failure being unity");
                RET_ERR(err_invalid_numeric_value);
            }
        }
        else {
            if (!(inside > 0.0)) {
                emit_error("Probability of an observed goal being zero");
                RET_ERR(err_invalid_numeric_value);
            }
        }
    }

    return BP_TRUE;
}

int examine_inside_scaling_log_exp(void)
{
    int i;
    double inside;

    /* [23 Aug 2007, by yuizumi]
     * By the code below, inside_failure can take only a non-zero value
     * when `failure' is observed.  We can therefore safely use zero as
     * an indicator of failure being not observed.  Zero is chosen just
     * for convenience in implementation of the parallel version.
     */
    inside_failure = 0.0;

    for (i = 0; i < num_roots; i++) {
        inside = expl_graph[roots[i]->id]->inside;
        if (i == failure_root_index) {
            inside_failure = inside; /* log-scale */
            if (!(inside_failure < 0.0)) {
                emit_error("Probability of failure being unity");
                RET_ERR(err_invalid_numeric_value);
            }
        }
        else {
            if (!isfinite(inside)) {
                emit_error("Probability of an observed goal being zero");
                RET_ERR(err_invalid_numeric_value);
            }
        }
    }

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

int compute_expectation_scaling_none(void)
{
    int i,k;
    EG_PATH_PTR path_ptr;
    EG_NODE_PTR eg_ptr,node_ptr;
    SW_INS_PTR sw_ptr;
    double q;

    for (i = 0; i < sw_ins_tab_size; i++) {
        switch_instances[i]->total_expect = 0.0;
    }

    for (i = 0; i < sorted_egraph_size; i++) {
        sorted_expl_graph[i]->outside = 0.0;
    }

    for (i = 0; i < num_roots; i++) {
        eg_ptr = expl_graph[roots[i]->id];
        if (i == failure_root_index) {
            eg_ptr->outside = num_goals / (1.0 - inside_failure);
        }
        else {
            eg_ptr->outside = roots[i]->count / eg_ptr->inside;
        }
    }

    for (i = sorted_egraph_size - 1; i >= 0; i--) {
        eg_ptr = sorted_expl_graph[i];
        path_ptr = eg_ptr->path_ptr;
        while (path_ptr != NULL) {
            q = eg_ptr->outside * path_ptr->inside;
            if (q > 0.0) {
                for (k = 0; k < path_ptr->children_len; k++) {
                    node_ptr = path_ptr->children[k];
                    node_ptr->outside += q / node_ptr->inside;
                }
                for (k = 0; k < path_ptr->sws_len; k++) {
                    sw_ptr = path_ptr->sws[k];
                    sw_ptr->total_expect += q;
                }
            }
            path_ptr = path_ptr->next;
        }
    }

    return BP_TRUE;
}

int compute_expectation_scaling_log_exp(void)
{
    int i,k;
    EG_PATH_PTR path_ptr;
    EG_NODE_PTR eg_ptr,node_ptr;
    SW_INS_PTR sw_ptr;
    double q,r;

    for (i = 0; i < sw_ins_tab_size; i++) {
        switch_instances[i]->total_expect = 0.0;
        switch_instances[i]->has_first_expectation = 0;
        switch_instances[i]->first_expectation = 0.0;
    }

    for (i = 0; i < sorted_egraph_size; i++) {
        sorted_expl_graph[i]->outside = 0.0;
        sorted_expl_graph[i]->has_first_outside = 0;
        sorted_expl_graph[i]->first_outside = 0.0;
    }

    for (i = 0; i < num_roots; i++) {
        eg_ptr = expl_graph[roots[i]->id];
        if (i == failure_root_index) {
            eg_ptr->first_outside =
                log(num_goals / (1.0 - exp(inside_failure)));
        }
        else {
            eg_ptr->first_outside =
                log((double)(roots[i]->count)) - eg_ptr->inside;
        }
        eg_ptr->has_first_outside = 1;
        eg_ptr->outside = 1.0;
    }

    /* sorted_expl_graph[to] must be a root node */
    for (i = sorted_egraph_size - 1; i >= 0; i--) {
        eg_ptr = sorted_expl_graph[i];

        /* First accumulate log-scale outside probabilities: */
        if (!eg_ptr->has_first_outside) {
            emit_internal_error("unexpected has_first_outside[%s]",
                                prism_goal_string(eg_ptr->id));
            RET_INTERNAL_ERR;
        }
        else if (!(eg_ptr->outside > 0.0)) {
            emit_internal_error("unexpected outside[%s]",
                                prism_goal_string(eg_ptr->id));
            RET_INTERNAL_ERR;
        }
        else {
            eg_ptr->outside = eg_ptr->first_outside + log(eg_ptr->outside);
        }

        path_ptr = sorted_expl_graph[i]->path_ptr;
        while (path_ptr != NULL) {
            q = sorted_expl_graph[i]->outside + path_ptr->inside;
            for (k = 0; k < path_ptr->children_len; k++) {
                node_ptr = path_ptr->children[k];
                r = q - node_ptr->inside;
                if (!node_ptr->has_first_outside) {
                    node_ptr->first_outside = r;
                    node_ptr->outside += 1.0;
                    node_ptr->has_first_outside = 1;
                }
                else if (r - node_ptr->first_outside >= log(HUGE_PROB)) {
                    node_ptr->outside *= exp(node_ptr->first_outside - r);
                    node_ptr->first_outside = r;
                    node_ptr->outside += 1.0;
                }
                else {
                    node_ptr->outside += exp(r - node_ptr->first_outside);
                }
            }
            for (k = 0; k < path_ptr->sws_len; k++) {
                sw_ptr = path_ptr->sws[k];
                if (!sw_ptr->has_first_expectation) {
                    sw_ptr->first_expectation = q;
                    sw_ptr->total_expect += 1.0;
                    sw_ptr->has_first_expectation = 1;
                }
                else if (q - sw_ptr->first_expectation >= log(HUGE_PROB)) {
                    sw_ptr->total_expect *= exp(sw_ptr->first_expectation - q);
                    sw_ptr->first_expectation = q;
                    sw_ptr->total_expect += 1.0;
                }
                else {
                    sw_ptr->total_expect += exp(q - sw_ptr->first_expectation);
                }
            }
            path_ptr = path_ptr->next;
        }
    }

    /* unscale total_expect */
    for (i = 0; i < sw_ins_tab_size; i++) {
        sw_ptr = switch_instances[i];
        if (!sw_ptr->has_first_expectation) continue;
        if (!(sw_ptr->total_expect > 0.0)) {
            emit_error("unexpected expectation for %s",prism_sw_ins_string(i));
            RET_ERR(err_invalid_numeric_value);
        }
        sw_ptr->total_expect =
            exp(sw_ptr->first_expectation + log(sw_ptr->total_expect));
    }

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

double compute_likelihood_scaling_none(void)
{
    int i;
    double likelihood,adjuster,inside;

    likelihood = 0.0;
    adjuster = failure_observed ? log(1.0-inside_failure) : 0.0;

    for (i = 0; i < num_roots; i++) {
        if (i == failure_root_index) continue;       /* skip failure */
        inside = expl_graph[roots[i]->id]->inside; /* always positive */
        likelihood += roots[i]->count * (log(inside) - adjuster);
    }

    return likelihood;
}

double compute_likelihood_scaling_log_exp(void)
{
    int i;
    double likelihood,adjuster,inside;

    likelihood = 0.0;
    adjuster = failure_observed ? log(1.0-exp(inside_failure)) : 0.0;

    for (i = 0; i < num_roots; i++) {
        if (i == failure_root_index) continue;        /* skip failure */
        inside = expl_graph[roots[i]->id]->inside;  /* log-scale */
        likelihood += roots[i]->count * (inside - adjuster);
    }

    return likelihood;
}

/*------------------------------------------------------------------------*/

double compute_log_prior(void)
{
    int i;
    SW_INS_PTR sw_ins_ptr;
    double lp;

    lp = 0.0;
    for (i = 0; i < occ_switch_tab_size; i++) {
        sw_ins_ptr = occ_switches[i];
        while (sw_ins_ptr != NULL) {
            lp += sw_ins_ptr->smooth * log(sw_ins_ptr->inside);
            sw_ins_ptr = sw_ins_ptr->next;
        }
    }

    return lp;
}

double compute_daem_log_prior(void)
{
    int i;
    SW_INS_PTR sw_ins_ptr;
    double lp;

    lp = 0.0;
    for (i = 0; i < occ_switch_tab_size; i++) {
        sw_ins_ptr = occ_switches[i];
        while (sw_ins_ptr != NULL) {
            lp += sw_ins_ptr->smooth * log(sw_ins_ptr->inside);
            sw_ins_ptr = sw_ins_ptr->next;
        }
    }

    return itemp * lp;
}

/*------------------------------------------------------------------------*/

int update_params(void)
{
    int i;
    SW_INS_PTR ptr,next;
    double sum,cur_prob_sum;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        sum = 0.0;
        while (ptr != NULL) {
            sum += ptr->total_expect;
            ptr = ptr->next;
        }
        if (sum != 0.0) {
            cur_prob_sum = 0.0;
            ptr = occ_switches[i];
            if (ptr->fixed > 0) continue;
            next = ptr->next;
            while (next != NULL) {
                if (ptr->fixed == 0) ptr->inside = ptr->total_expect / sum;
                if (log_scale && ptr->inside < log(TINY_PROB)) {
                    emit_error("Parameter being zero (-inf in log scale) -- %s",
                               prism_sw_ins_string(ptr->id));
                    RET_ERR(err_underflow);
                }
                cur_prob_sum += ptr->inside;
                ptr = next;
                next = ptr->next;
            }
            ptr->inside = 1.0-cur_prob_sum;  /* Normalize */
        }
    }

    return BP_TRUE;
}

int update_params_smooth(void)
{
    int i;
    SW_INS_PTR ptr,next;
    double sum,cur_prob_sum;
    double denom;
    // int n;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        // n = num_sw_vals[i];
        sum = 0.0;
        while (ptr != NULL) {
            sum += ptr->total_expect + ptr->smooth;
            ptr = ptr->next;
        }
        denom = sum;
        if (sum != 0.0) {
            cur_prob_sum = 0.0;
            ptr = occ_switches[i];
            if (ptr->fixed > 0) continue;
            next = ptr->next;
            while (next != NULL) {
                if (ptr->fixed == 0)
                    ptr->inside = (ptr->total_expect + ptr->smooth) / denom;
                cur_prob_sum += ptr->inside;
                ptr = next;
                next = ptr->next;
            }
            ptr->inside = 1.0-cur_prob_sum;  /* Normalize */
        }
    }

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

void save_params(void)
{
    int i;
    SW_INS_PTR ptr;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        if (ptr->fixed > 0) continue;
        while (ptr != NULL) {
            ptr->best_inside = ptr->inside;
            ptr->best_total_expect = ptr->total_expect;
            ptr = ptr->next;
        }
    }
}

void restore_params(void)
{
    int i;
    SW_INS_PTR ptr;

    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        if (ptr->fixed > 0) continue;
        while (ptr != NULL) {
            ptr->inside = ptr->best_inside;
            ptr->total_expect = ptr->best_total_expect;
            ptr = ptr->next;
        }
    }
}

/*------------------------------------------------------------------------*/

double compute_bic(double likelihood)
{
    double bic = likelihood;
    int i, num_sw_ins, num_params;

    num_sw_ins = 0;
    for (i = 0; i < occ_switch_tab_size; i++) {
        SW_INS_PTR ptr = occ_switches[i];
        while (ptr != NULL) {
            num_sw_ins++;
            ptr = ptr->next;
        }
    }

    /* Get the number of free parameters: */
    num_params = num_sw_ins - occ_switch_tab_size;
    bic = likelihood - 0.5 * num_params * log(num_goals);

    return bic;
}

double compute_cs(double likelihood)
{
    double cs;
    double l0, l1, l2;
    int i;
    SW_INS_PTR ptr;
    double smooth_sum;

    /* Compute BD score using the expectations: */
    l0 = 0.0;
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
            smooth_sum += (ptr->total_expect + ptr->smooth + 1.0);
            ptr = ptr->next;
        }
        l0 -= lngamma(smooth_sum);

        ptr = occ_switches[i];
        while (ptr != NULL) {
            l0 += lngamma(ptr->total_expect + ptr->smooth + 1.0);
            l0 -= lngamma(ptr->smooth + 1.0);
            ptr = ptr->next;
        }
    }

    /* Compute the likelihood of complete data using the expectations: */
    l1 = 0.0;
    for (i = 0; i < occ_switch_tab_size; i++) {
        ptr = occ_switches[i];
        while (ptr != NULL) {
            l1 += ptr->total_expect * log(ptr->inside);
            ptr = ptr->next;
        }
    }

    /* Get the log-likelihood: */
    l2 = likelihood;

    cs = l0 - l1 + l2;

    return cs;
}

/*------------------------------------------------------------------------*/
