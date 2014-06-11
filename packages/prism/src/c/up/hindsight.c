#include "up/up.h"
#include "up/graph.h"
#include "up/graph_aux.h"
#include "up/em_aux.h"
#include "up/em_aux_ml.h"
#include "up/flags.h"
#include "up/util.h"

/*------------------------------------------------------------------------*/

#define INIT_MAX_HINDSIGHT_GOAL_SIZE 100

/*------------------------------------------------------------------------*/

/* mic.c (B-Prolog) */
extern NORET quit(const char *);
int compute_outside_scaling_none(void);
int compute_outside_scaling_log_exp(void);
int pc_compute_hindsight_4(void);
int compute_outside_scaling_none(void);
int compute_outside_scaling_log_exp(void);
int pc_compute_hindsight_4(void);

/*------------------------------------------------------------------------*/

static int    * hindsight_goals = NULL;
static double * hindsight_probs = NULL;
static int max_hindsight_goal_size;
static int hindsight_goal_size;

/*------------------------------------------------------------------------*/

static void alloc_hindsight_goals(void)
{
    int i;

    hindsight_goal_size = 0;
    max_hindsight_goal_size = INIT_MAX_HINDSIGHT_GOAL_SIZE;
    hindsight_goals = (int *)MALLOC(max_hindsight_goal_size * sizeof(TERM));
    hindsight_probs =
        (double *)MALLOC(max_hindsight_goal_size * sizeof(double));

    for (i = 0; i < max_hindsight_goal_size; i++) {
        hindsight_goals[i] = -1;
        hindsight_probs[i] = 0.0;
    }
}

static void expand_hindsight_goals(int req_hindsight_goal_size)
{
    int old_size,i;

    if (req_hindsight_goal_size > max_hindsight_goal_size) {
        old_size = max_hindsight_goal_size;

        while (req_hindsight_goal_size > max_hindsight_goal_size) {
            max_hindsight_goal_size *= 2;
        }

        hindsight_goals =
            (int *)REALLOC(hindsight_goals,
                           max_hindsight_goal_size * sizeof(TERM));
        hindsight_probs =
            (double *)REALLOC(hindsight_probs,
                              max_hindsight_goal_size * sizeof(double));

        for (i = old_size; i < max_hindsight_goal_size; i++) {
            hindsight_goals[i] = -1;
            hindsight_probs[i] = 0.0;
        }
    }
}

/*
 * Be warned that eg_ptr->outside will have a value different from that
 * in the compute_expectation-family functions.
 */
int compute_outside_scaling_none(void)
{
    int i,k;
    EG_PATH_PTR path_ptr;
    EG_NODE_PTR eg_ptr,node_ptr;
    double q;

    if (num_roots != 1) {
        emit_internal_error("illegal call to compute_outside");
        RET_ERR(build_internal_error("no_observed_data"));
    }

    for (i = 0; i < sorted_egraph_size; i++) {
        sorted_expl_graph[i]->outside = 0.0;
    }

    eg_ptr = expl_graph[roots[0]->id];
    eg_ptr->outside = roots[0]->count;

    for (i = (sorted_egraph_size - 1); i >= 0; i--) {
        eg_ptr = sorted_expl_graph[i];
        path_ptr = eg_ptr->path_ptr;
        while (path_ptr != NULL) {
            q = eg_ptr->outside * path_ptr->inside;
            if (q > 0.0) {
                for (k = 0; k < path_ptr->children_len; k++) {
                    node_ptr = path_ptr->children[k];
                    node_ptr->outside += q / node_ptr->inside;
                }
            }
            path_ptr = path_ptr->next;
        }
    }

    return BP_TRUE;
}

int compute_outside_scaling_log_exp(void)
{
    int i,k;
    EG_PATH_PTR path_ptr;
    EG_NODE_PTR eg_ptr,node_ptr;
    double q,r;

    if (num_roots != 1) {
        emit_internal_error("illegal call to compute_outside");
        RET_ERR(build_internal_error("no_observed_data"));
    }

    for (i = 0; i < sorted_egraph_size; i++) {
        sorted_expl_graph[i]->outside = 0.0;
        sorted_expl_graph[i]->has_first_outside = 0;
        sorted_expl_graph[i]->first_outside = 0.0;
    }

    eg_ptr = expl_graph[roots[0]->id];
    eg_ptr->outside = 1.0;
    eg_ptr->has_first_outside = 1;
    eg_ptr->first_outside = log((double)(roots[0]->count));

    /* sorted_expl_graph[to] must be a root node */
    for (i = sorted_egraph_size - 1; i >= 0; i--) {
        eg_ptr = sorted_expl_graph[i];

        /* First accumulate log-scale outside probabilities: */
        if (!eg_ptr->has_first_outside) {
            emit_internal_error("unexpected has_first_outside[%s]",prism_goal_string(eg_ptr->id));
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
            path_ptr = path_ptr->next;
        }
    }

    return BP_TRUE;
}

static int get_hindsight_goals_scaling_none(TERM p_subgoal, int is_cond)
{
    int i,j;
    EG_NODE_PTR eg_ptr;
    TERM t;
    double denom;

    if (is_cond) {
        denom = expl_graph[roots[0]->id]->inside;
    }
    else {
        denom = 1.0;
    }

    j = 0;
    for (i = 0; i < sorted_egraph_size - 1; i++) {
        eg_ptr = sorted_expl_graph[i];
        t = prism_goal_term((IDNUM)(eg_ptr->id));
        if (bpx_is_unifiable(p_subgoal, t)) {
            if (j >= max_hindsight_goal_size) expand_hindsight_goals(j + 1);
            if (j >= hindsight_goal_size) hindsight_goal_size = j + 1;
            hindsight_goals[j] = eg_ptr->id;
            hindsight_probs[j] = eg_ptr->inside * eg_ptr->outside / denom;
            j++;
        }
    }

    return BP_TRUE;
}

static int get_hindsight_goals_scaling_log_exp(TERM p_subgoal, int is_cond)
{
    int i,j;
    EG_NODE_PTR eg_ptr;
    TERM t;
    double denom;

    if (is_cond) {
        denom = expl_graph[roots[0]->id]->inside;
    }
    else {
        denom = 0.0;
    }

    j = 0;
    for (i = 0; i < sorted_egraph_size - 1; i++) {
        eg_ptr = sorted_expl_graph[i];
        t = prism_goal_term(eg_ptr->id);
        if (bpx_is_unifiable(p_subgoal, t)) {
            if (j >= max_hindsight_goal_size) expand_hindsight_goals(j + 1);
            if (j >= hindsight_goal_size) hindsight_goal_size = j + 1;
            hindsight_goals[j] = eg_ptr->id;
            hindsight_probs[j] = eg_ptr->inside + eg_ptr->outside - denom;
            j++;
        }
    }

    return BP_TRUE;
}

int pc_compute_hindsight_4(void)
{
    TERM p_subgoal,p_hindsight_pairs,t,t1,p_pair;
    int goal_id,is_cond,j;

    goal_id   = bpx_get_integer(bpx_get_call_arg(1,4));
    p_subgoal = bpx_get_call_arg(2,4);
    is_cond   = bpx_get_integer(bpx_get_call_arg(3,4));

    initialize_egraph_index();
    alloc_sorted_egraph(1);
    RET_ON_ERR(sort_one_egraph(goal_id,0,1));
    if (verb_graph) print_egraph(0,PRINT_NEUTRAL);

    alloc_hindsight_goals();

	if (log_scale) {
        RET_ON_ERR(compute_inside_scaling_log_exp());
        RET_ON_ERR(compute_outside_scaling_log_exp());
        RET_ON_ERR(get_hindsight_goals_scaling_log_exp(p_subgoal,is_cond));
	}
	else {
        RET_ON_ERR(compute_inside_scaling_none());
        RET_ON_ERR(compute_outside_scaling_none());
        RET_ON_ERR(get_hindsight_goals_scaling_none(p_subgoal,is_cond));
	}

    if (hindsight_goal_size > 0) {
        /* Build the list of pairs of a subgoal and its hindsight probability */
        p_hindsight_pairs = bpx_build_list();
        t = p_hindsight_pairs;

        for (j = 0; j < hindsight_goal_size; j++) {
            p_pair = bpx_build_list();

            t1 = p_pair;
            bpx_unify(bpx_get_car(t1),
                      bpx_build_integer(hindsight_goals[j]));
            bpx_unify(bpx_get_cdr(t1),bpx_build_list());

            t1 = bpx_get_cdr(t1);
            bpx_unify(bpx_get_car(t1),bpx_build_float(hindsight_probs[j]));
            bpx_unify(bpx_get_cdr(t1),bpx_build_nil());

            bpx_unify(bpx_get_car(t),p_pair);

            if (j == hindsight_goal_size - 1) {
                bpx_unify(bpx_get_cdr(t),bpx_build_nil());
            }
            else {
                bpx_unify(bpx_get_cdr(t),bpx_build_list());
                t = bpx_get_cdr(t);
            }
        }
    }
    else {
        p_hindsight_pairs = bpx_build_nil();
    }

    FREE(hindsight_goals);
    FREE(hindsight_probs);

    return bpx_unify(bpx_get_call_arg(4,4),p_hindsight_pairs);
}
