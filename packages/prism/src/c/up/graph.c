#include "up/up.h"
#include "up/flags.h"
#include "up/graph.h"
#include "up/util.h"

/*------------------------------------------------------------------------*/

/* mic.c (B-Prolog) */
NORET  quit(const char *);
NORET  myquit(int, const char *);

/* univ.c (B-Prolog) */
int    list_length(BPLONG, BPLONG);

int pc_import_graph_stats_4(void);

/*------------------------------------------------------------------------*/

static int max_egraph_size        = INIT_MAX_EGRAPH_SIZE;
static int max_sorted_egraph_size = INIT_MAX_EGRAPH_SIZE;
static int egraph_size            = 0;

static int max_sw_tab_size     = INIT_MAX_SW_TABLE_SIZE;
static int max_sw_ins_tab_size = INIT_MAX_SW_INS_TABLE_SIZE;

static int index_to_sort       = 0;
static int suppress_init_flags = 0;  /* flag: suppress INIT_VISITED_FLAGS? */

int sorted_egraph_size         = 0;
EG_NODE_PTR *expl_graph        = NULL;
EG_NODE_PTR *sorted_expl_graph = NULL;
ROOT *roots                    = NULL;

int num_roots;
int num_goals;

int min_node_index;
int max_node_index;

SW_INS_PTR *switches         = NULL;
SW_INS_PTR *switch_instances = NULL;
SW_INS_PTR *occ_switches     = NULL;  /* subset of switches */
int sw_tab_size              = 0;
int sw_ins_tab_size          = 0;
int occ_switch_tab_size      = 0;

int failure_subgoal_id;
int failure_root_index;

/*------------------------------------------------------------------------*/

static void alloc_switch_table(void)
{
    int i;
    sw_tab_size = 0;
    switches = (SW_INS_PTR *)MALLOC(max_sw_tab_size * sizeof(SW_INS_PTR));

    for (i = 0; i < max_sw_tab_size; i++)
        switches[i] = NULL;
}

static void expand_switch_table(int req_sw_tab_size)
{
    int old_size,i;

    if (req_sw_tab_size > max_sw_tab_size) {
        old_size = max_sw_tab_size;

        while (req_sw_tab_size > max_sw_tab_size)
            max_sw_tab_size *= 2;

        switches = (SW_INS_PTR *)REALLOC(switches,
                                         max_sw_tab_size * sizeof(SW_INS_PTR));

        for (i = old_size; i < max_sw_tab_size; i++)
            switches[i] = NULL;
    }
}

static void clean_switch_table(void)
{
    if (switches != NULL) {
        FREE(switches);
        sw_tab_size = 0;
        max_sw_tab_size = INIT_MAX_SW_TABLE_SIZE;
    }
}

/*------------------------------------------------------------------------*/

static SW_INS_PTR alloc_switch_instance(void)
{
    SW_INS_PTR sw_ptr = (SW_INS_PTR)MALLOC(sizeof(struct SwitchInstance));
    sw_ptr->inside = 0.5;

    return sw_ptr;
}

static void alloc_switch_instance_table(void)
{
    int i;
    sw_ins_tab_size = 0;
    switch_instances =
        (SW_INS_PTR *)MALLOC(max_sw_ins_tab_size * sizeof(SW_INS_PTR));

    for (i = 0; i < max_sw_ins_tab_size; i++)
        switch_instances[i] = NULL;
}

static void expand_switch_instance_table(int req_sw_ins_tab_size)
{
    int old_size,i;

    if (req_sw_ins_tab_size > max_sw_ins_tab_size) {
        old_size = max_sw_ins_tab_size;

        while (req_sw_ins_tab_size > max_sw_ins_tab_size)
            max_sw_ins_tab_size *= 2;

        switch_instances =
            (SW_INS_PTR *)REALLOC(switch_instances,
                                  max_sw_ins_tab_size * sizeof(SW_INS_PTR));

        for (i = old_size; i < max_sw_ins_tab_size; i++)
            switch_instances[i] = NULL;
    }
}

static void clean_switch_instance_table(void)
{
    int i;

    if (switch_instances != NULL) {
        for (i = 0; i < max_sw_ins_tab_size; i++)
            FREE(switch_instances[i]);
        FREE(switch_instances);
        sw_ins_tab_size = 0;
        max_sw_ins_tab_size = INIT_MAX_SW_INS_TABLE_SIZE;
    }
}

/*------------------------------------------------------------------------*/

static EG_NODE_PTR alloc_egraph_node(void)
{
    EG_NODE_PTR node_ptr = (EG_NODE_PTR)MALLOC(sizeof(struct ExplGraphNode));

    node_ptr->inside    = 1.0;
    node_ptr->visited   = 0;
    node_ptr->path_ptr  = NULL;
    node_ptr->top_n     = NULL;
    node_ptr->top_n_len = 0;
    node_ptr->shared    = 0;

    return node_ptr;
}

int pc_alloc_egraph_0(void)
{
    int i;

    alloc_switch_table();
    alloc_switch_instance_table();

    egraph_size = 0;
    expl_graph = (EG_NODE_PTR *)MALLOC(max_egraph_size * sizeof(EG_NODE_PTR));

    for (i = 0; i < max_egraph_size; i++) {
        expl_graph[i] = alloc_egraph_node();
        expl_graph[i]->id = i;
    }

    return BP_TRUE;
}

static void expand_egraph(int req_egraph_size)
{
    int old_size,i;

    if (req_egraph_size > max_egraph_size) {
        old_size = max_egraph_size;

        while (req_egraph_size > max_egraph_size) {
            if (max_egraph_size > MAX_EGRAPH_SIZE_EXPAND_LIMIT) {
                max_egraph_size += MAX_EGRAPH_SIZE_EXPAND_LIMIT;
            }
            else {
                max_egraph_size *= 2;
            }
        }

        expl_graph =
            (EG_NODE_PTR *)REALLOC(expl_graph,
                                   max_egraph_size * sizeof(EG_NODE_PTR));

        for (i = old_size; i < max_egraph_size; i++) {
            expl_graph[i] = alloc_egraph_node();
            expl_graph[i]->id = i;
        }
    }
}

static void clean_sorted_egraph(void)
{
    FREE(sorted_expl_graph);
}

/* Clean-up the base support graphs and switches */
static void clean_base_egraph(void)
{
    int i,j;
    EG_PATH_PTR path_ptr,next_path_ptr;

    clean_switch_table();
    clean_switch_instance_table();

    if (expl_graph != NULL) {
        for (i = 0; i < max_egraph_size; i++) {
            if (expl_graph[i] == NULL) continue;
            path_ptr = expl_graph[i]->path_ptr;
            while (path_ptr != NULL) {
                FREE(path_ptr->children);
                FREE(path_ptr->sws);
                next_path_ptr = path_ptr->next;
                FREE(path_ptr);
                path_ptr = next_path_ptr;
            }
            if (expl_graph[i]->top_n != NULL) {
                for (j = 0; j < expl_graph[i]->top_n_len; j++) {
					FREE(expl_graph[i]->top_n[j]->top_n_index);
                    FREE(expl_graph[i]->top_n[j]);
                }
                FREE(expl_graph[i]->top_n);
            }
            FREE(expl_graph[i]);
        }
        FREE(expl_graph);
        egraph_size = 0;
        max_egraph_size = INIT_MAX_EGRAPH_SIZE;
        INIT_MIN_MAX_NODE_NOS;
    }
}

int pc_clean_base_egraph_0(void)
{
    clean_base_egraph();
    return BP_TRUE;
}

int pc_clean_egraph_0(void)
{
    clean_sorted_egraph();
    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

int pc_export_switch_2(void)
{
    BPLONG sw,sw_ins_ids,sw_ins_id;
    SW_INS_PTR *curr_ins_ptr;

    sw = bpx_get_integer(bpx_get_call_arg(1,2));
    sw_ins_ids = bpx_get_call_arg(2,2);

    if (sw >= max_sw_tab_size) expand_switch_table(sw + 1);
    if (sw >= sw_tab_size) sw_tab_size = sw + 1;

    curr_ins_ptr = &switches[sw];
    while (bpx_is_list(sw_ins_ids)) {
        sw_ins_id = bpx_get_integer(bpx_get_car(sw_ins_ids));
        sw_ins_ids = bpx_get_cdr(sw_ins_ids);

        if (sw_ins_id >= max_sw_ins_tab_size)
            expand_switch_instance_table(sw_ins_id + 1);
        if (sw_ins_id >= sw_ins_tab_size) sw_ins_tab_size = sw_ins_id + 1;

		switch_instances[sw_ins_id] = alloc_switch_instance();
        switch_instances[sw_ins_id]->id = sw_ins_id;

        *curr_ins_ptr = switch_instances[sw_ins_id];
        curr_ins_ptr = &switch_instances[sw_ins_id]->next;
    }
    *curr_ins_ptr = NULL;

    return BP_TRUE;
}

static int add_egraph_path(int node_id, TERM children_prolog, TERM sws_prolog)
{
    EG_PATH_PTR path_ptr;
    EG_NODE_PTR *children;
    SW_INS_PTR *sws;
    int len,k;
    int child,sw;
	TERM p_child,p_sw;
    int list_length(BPLONG, BPLONG);

    if (node_id >= max_egraph_size) expand_egraph(node_id + 1);
    if (node_id >= egraph_size) egraph_size = node_id + 1;

    path_ptr = (EG_PATH_PTR)MALLOC(sizeof(struct ExplGraphPath));

    len = list_length(children_prolog, children_prolog);
    if (len > 0) {
        path_ptr->children_len = len;
        children = (EG_NODE_PTR *)MALLOC(sizeof(EG_NODE_PTR) * len);
        k = 0;
        while (bpx_is_list(children_prolog)) {
			p_child = bpx_get_car(children_prolog);
			if (!bpx_is_integer(p_child))
				RET_ERR(err_invalid_goal_id);
            child = bpx_get_integer(p_child);
            children[k] = expl_graph[child];
            k++;
            children_prolog = bpx_get_cdr(children_prolog);
        }
        path_ptr->children = children;
    }
    else {
        path_ptr->children_len = 0;
        path_ptr->children = NULL;
    }

    len = list_length(sws_prolog, sws_prolog);
    if (len > 0) {
        path_ptr->sws_len = len;
        sws = (SW_INS_PTR *)MALLOC(sizeof(SW_INS_PTR) * len);
        k = 0;
        while (bpx_is_list(sws_prolog)) {
			p_sw = bpx_get_car(sws_prolog);
			if (!bpx_is_integer(p_sw))
				RET_ERR(err_invalid_switch_instance_id);
            sw = bpx_get_integer(p_sw);
            sws[k] = switch_instances[sw];
            k++;
            sws_prolog = bpx_get_cdr(sws_prolog);
        }
        path_ptr->sws = sws;
    }
    else {
        path_ptr->sws_len = 0;
        path_ptr->sws = NULL;
    }

    path_ptr->next = expl_graph[node_id]->path_ptr;
    expl_graph[node_id]->path_ptr = path_ptr;

	return BP_TRUE;
}

int pc_add_egraph_path_3(void)
{
    TERM p_node_id,p_children,p_sws;
	int node_id;

    /* children_prolog and sws_prolog must be in the table area */
    p_node_id  = bpx_get_call_arg(1,3);
    p_children = bpx_get_call_arg(2,3);
    p_sws      = bpx_get_call_arg(3,3);

	if (!bpx_is_integer(p_node_id))	RET_ERR(err_invalid_goal_id);
	node_id = bpx_get_integer(p_node_id);

    XDEREF(p_children);
    XDEREF(p_sws);

    RET_ON_ERR(add_egraph_path(node_id,p_children,p_sws));

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

void alloc_sorted_egraph(int n)
{
    int i;

    max_sorted_egraph_size = INIT_MAX_EGRAPH_SIZE;
    sorted_expl_graph =
        (EG_NODE_PTR *)MALLOC(sizeof(EG_NODE_PTR) * max_sorted_egraph_size);
    roots = (ROOT *)MALLOC(sizeof(ROOT *) * n);

    for (i = 0; i < n; i++)
        roots[i] = NULL;

    num_roots = n;
}

static void expand_sorted_egraph(int req_sorted_egraph_size)
{
    if (req_sorted_egraph_size > max_sorted_egraph_size) {
        while (req_sorted_egraph_size > max_sorted_egraph_size) {
            if (max_sorted_egraph_size > MAX_EGRAPH_SIZE_EXPAND_LIMIT)
                max_sorted_egraph_size += MAX_EGRAPH_SIZE_EXPAND_LIMIT;
            else
                max_sorted_egraph_size *= 2;
        }
        sorted_expl_graph =
            (EG_NODE_PTR *)
            REALLOC(sorted_expl_graph,
                    max_sorted_egraph_size * sizeof(EG_NODE_PTR));
    }
}

/*------------------------------------------------------------------------*/

void initialize_egraph_index(void)
{
    index_to_sort = 0;
}

static int topological_sort(int node_id)
{
    EG_PATH_PTR path_ptr;
    EG_NODE_PTR *children;
    int k,len;
    EG_NODE_PTR child_ptr;

    expl_graph[node_id]->visited = 2;
    UPDATE_MIN_MAX_NODE_NOS(node_id);

    path_ptr = expl_graph[node_id]->path_ptr;
    while (path_ptr != NULL) {
        children = path_ptr->children;
        len = path_ptr->children_len;
        for (k = 0; k < len; k++) {
            child_ptr = children[k];

            if (child_ptr->visited == 2 && error_on_cycle)
                RET_ERR(err_cycle_detected);

            if (child_ptr->visited == 0) {
                RET_ON_ERR(topological_sort(child_ptr->id));
                expand_sorted_egraph(index_to_sort + 1);
                sorted_expl_graph[index_to_sort++] = child_ptr;
            }
            child_ptr->shared += 1;
        }

        path_ptr = path_ptr->next;
    }
    expl_graph[node_id]->visited = 1;
    return BP_TRUE;
}

int sort_one_egraph(int root_id, int root_index, int count)
{
    roots[root_index] = (ROOT)MALLOC(sizeof(struct ObservedFactNode));
    roots[root_index]->id = root_id;
    roots[root_index]->count = count;

    if (expl_graph[root_id]->visited == 1) {
        /*
         * This top-goal is also a sub-goal of another top-goal.  This
         * should occur only when INIT_VISITED_FLAGS is suppressed
         * (i.e. we have more than one observed goal in learning).
         */
        if (suppress_init_flags) return BP_TRUE;
    }

    if (expl_graph[root_id]->visited != 0) RET_INTERNAL_ERR;

    RET_ON_ERR(topological_sort(root_id));

    expand_sorted_egraph(index_to_sort + 1);
    sorted_expl_graph[index_to_sort] = expl_graph[root_id];

    index_to_sort++;
    sorted_egraph_size = index_to_sort;

    /* initialize flags after use */
    if (!suppress_init_flags) INIT_VISITED_FLAGS;

    return BP_TRUE;
}

int sort_egraphs(TERM p_fact_list) /* assumed to be dereferenced in advance */
{
    TERM pair;
    int root_index = 0, goal_id, count;

    sorted_egraph_size = 0;
    suppress_init_flags = 1;

    while (bpx_is_list(p_fact_list)) {
        pair = bpx_get_car(p_fact_list);
        p_fact_list = bpx_get_cdr(p_fact_list);

        goal_id = bpx_get_integer(bpx_get_arg(1,pair));
        count   = bpx_get_integer(bpx_get_arg(2,pair));

        if (sort_one_egraph(goal_id,root_index,count) == BP_ERROR) {
            INIT_VISITED_FLAGS;
            return BP_ERROR;
        }
        root_index++;
    }

    suppress_init_flags = 0;

    INIT_VISITED_FLAGS;
    return BP_TRUE;
}

/*
 * Sort the explanation graph such that no node sorted_expl_graph[i] calls
 * node sorted_expl_graph[j] if i < j.
 *
 * This function is used only for probf/1-2, so we don't have to consider
 * about scaling here.
 */
int pc_alloc_sort_egraph_1(void)
{
    int root_id;

    root_id = bpx_get_integer(bpx_get_call_arg(1,1));

    index_to_sort = 0;
    alloc_sorted_egraph(1);
    RET_ON_ERR(sort_one_egraph(root_id,0,1));

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

static void clean_root_tables(void)
{
    int i;
    if (roots != NULL) {
        for (i = 0; i < num_roots; i++)
            FREE(roots[i]);
        FREE(roots);
    }
}

int pc_clean_external_tables_0(void)
{
    clean_root_tables();
    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

/*
 * Export probabilities of switches from Prolog to C.  Switches is
 * a list of switches, each of which takes the form:
 *
 *   sw(Id,InstanceIds,Probs,SmoothCs,Fixed,FixedH),
 *
 * where
 *    Id:          identifier of the switch
 *    InstanceIds: list of ids of the instances of the switch
 *    Probs:       current probabilities assigned to the instance switches
 *    SmoothCs:    current pseudo counts assigned to the instance switches
 *    Fixed:       probabilities fixed?
 *    FixedH:      pseudo counts fixed?
 *
 * The structures for switch instances have been allocated. This
 * function only fills out the initial probabilities.
 */
int pc_export_sw_info_1(void)
{
  //  int sw_id;
  int instance_id,fixed,fixed_h;
    double prob,smooth;
    TERM p_switches, p_switch;
    TERM p_instance_list,p_prob_list,p_smooth_list;
    TERM p_prob,p_smooth;

    p_switches = bpx_get_call_arg(1,1);

    while (bpx_is_list(p_switches)) {
        /* p_switch: sw(Id,InstList,ProbList,SmoothCList,FixedP,FixedH) */
        p_switch = bpx_get_car(p_switches);

	//   sw_id           = bpx_get_integer(bpx_get_arg(1,p_switch));
        p_instance_list = bpx_get_arg(2,p_switch);
        p_prob_list     = bpx_get_arg(3,p_switch);
        p_smooth_list   = bpx_get_arg(4,p_switch);
        fixed           = bpx_get_integer(bpx_get_arg(5,p_switch));
        fixed_h         = bpx_get_integer(bpx_get_arg(6,p_switch));

        while (bpx_is_list(p_instance_list)) {
            instance_id = bpx_get_integer(bpx_get_car(p_instance_list));
            p_prob      = bpx_get_car(p_prob_list);
            p_smooth    = bpx_get_car(p_smooth_list);

            if (bpx_is_integer(p_prob)) {
                prob = (double)bpx_get_integer(p_prob);
            }
            else if (bpx_is_float(p_prob)) {
                prob = bpx_get_float(p_prob);
            }
            else {
                RET_ERR(illegal_arguments);
            }

            if (bpx_is_integer(p_smooth)) {
                smooth = (double)bpx_get_integer(p_smooth);
            }
            else if (bpx_is_float(p_smooth)) {
                smooth = bpx_get_float(p_smooth);
            }
            else {
                RET_ERR(illegal_arguments);
            }

            switch_instances[instance_id]->inside         = prob;
            switch_instances[instance_id]->fixed          = fixed;
            switch_instances[instance_id]->fixed_h        = fixed_h;
            switch_instances[instance_id]->smooth_prolog  = smooth;

            p_instance_list = bpx_get_cdr(p_instance_list);
            p_prob_list     = bpx_get_cdr(p_prob_list);
            p_smooth_list   = bpx_get_cdr(p_smooth_list);
        }
        p_switches = bpx_get_cdr(p_switches);
    }

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

/* the following functions are needed by probf */

int pc_import_sorted_graph_size_1(void)
{
    return bpx_unify(bpx_get_call_arg(1,1),
                     bpx_build_integer(sorted_egraph_size));
}

int pc_import_sorted_graph_gid_2(void)
{
    int idx = bpx_get_integer(bpx_get_call_arg(1,2));
    return bpx_unify(bpx_get_call_arg(2,2),
                     bpx_build_integer(sorted_expl_graph[idx]->id));
}

int pc_import_sorted_graph_paths_2(void)
{
    TERM paths0,paths1,glist,slist,t0,t1,p_tmp;
    EG_PATH_PTR path_ptr;
    EG_NODE_PTR *children;
    SW_INS_PTR *sws;
    int node_id,k,len;

    node_id = bpx_get_integer(bpx_get_call_arg(1,2));

    path_ptr = sorted_expl_graph[node_id]->path_ptr;

    if (path_ptr == NULL) {
        if (explicit_empty_expls) {
            t0 = bpx_build_list();
            t1 = bpx_build_list();
            bpx_unify(bpx_get_car(t0),bpx_build_nil());
            bpx_unify(bpx_get_cdr(t0),t1);
            bpx_unify(bpx_get_car(t1),bpx_build_nil());
            bpx_unify(bpx_get_cdr(t1),bpx_build_nil());

            paths0 = bpx_build_list();
            bpx_unify(bpx_get_car(paths0),t0);
            bpx_unify(bpx_get_cdr(paths0),bpx_build_nil());
        }
        else paths0 = bpx_build_nil();
    }
    else {
        paths0 = bpx_build_nil();
        while (path_ptr != NULL) {

            len = path_ptr->children_len;
            children = path_ptr->children;

            if (len > 0) {
                glist = bpx_build_list();
                p_tmp = glist;
                for (k = 0; k < len; k++) {
                    bpx_unify(bpx_get_car(p_tmp),
                              bpx_build_integer(children[k]->id));
                    if (k == len - 1) {
                        bpx_unify(bpx_get_cdr(p_tmp),bpx_build_nil());
                    }
                    else {
                        bpx_unify(bpx_get_cdr(p_tmp),bpx_build_list());
                        p_tmp = bpx_get_cdr(p_tmp);
                    }
                }
            }
            else glist = bpx_build_nil();

            len = path_ptr->sws_len;
            sws = path_ptr->sws;

            if (len > 0) {
                slist = bpx_build_list();
                p_tmp = slist;
                for (k = 0; k < len; k++) {
                    bpx_unify(bpx_get_car(p_tmp),bpx_build_integer(sws[k]->id));
                    if (k == len - 1) {
                        bpx_unify(bpx_get_cdr(p_tmp),bpx_build_nil());
                    }
                    else {
                        bpx_unify(bpx_get_cdr(p_tmp),bpx_build_list());
                        p_tmp = bpx_get_cdr(p_tmp);
                    }
                }
            }
            else slist = bpx_build_nil();

            if (explicit_empty_expls ||
                    !bpx_is_nil(glist) || !bpx_is_nil(slist)) {

                t0 = bpx_build_list();
                t1 = bpx_build_list();
                bpx_unify(bpx_get_car(t0),glist);
                bpx_unify(bpx_get_cdr(t0),t1);
                bpx_unify(bpx_get_car(t1),slist);
                bpx_unify(bpx_get_cdr(t1),bpx_build_nil());

                paths1 = bpx_build_list();
                bpx_unify(bpx_get_car(paths1),t0);
                bpx_unify(bpx_get_cdr(paths1),paths0);

                paths0 = paths1;
            }

            path_ptr = path_ptr->next;
        }
    }

    return bpx_unify(bpx_get_call_arg(2,2),paths0);
}

int pc_get_gnode_inside_2(void)
{
    int idx = bpx_get_integer(bpx_get_call_arg(1,2));
    return bpx_unify(bpx_get_call_arg(2,2),
                     bpx_build_float(expl_graph[idx]->inside));
}

int pc_get_gnode_outside_2(void)
{
    int idx = bpx_get_integer(bpx_get_call_arg(1,2));
    return bpx_unify(bpx_get_call_arg(2,2),
                     bpx_build_float(expl_graph[idx]->outside));
}

int pc_get_gnode_viterbi_2(void)
{
    int idx = bpx_get_integer(bpx_get_call_arg(1,2));
    return bpx_unify(bpx_get_call_arg(2,2),
                     bpx_build_float(expl_graph[idx]->max));
}

int pc_get_snode_inside_2(void)
{
    int idx = bpx_get_integer(bpx_get_call_arg(1,2));
	double val = switch_instances[idx]->inside;

	if (log_scale) val = log(val);

    return bpx_unify(bpx_get_call_arg(2,2),bpx_build_float(val));
}

int pc_get_snode_expectation_2(void)
{
    int idx = bpx_get_integer(bpx_get_call_arg(1,2));
    return bpx_unify(bpx_get_call_arg(2,2),
                     bpx_build_float(switch_instances[idx]->total_expect));
}

int pc_import_occ_switches_3(void)
{
  CACHE_REGS
    TERM p_sw_list,p_sw_list0,p_sw_list1;
    TERM p_sw_ins_list0,p_sw_ins_list1,sw,sw_ins;
    TERM p_num_sw, p_num_sw_ins;
    int i;
    int num_sw_ins;
    void release_occ_switches( void );

#ifdef __YAP_PROLOG__
    TERM *hstart;
 restart:
    hstart = heap_top;
#endif
    p_sw_list    = bpx_get_call_arg(1,3);
    p_num_sw     = bpx_get_call_arg(2,3);
    p_num_sw_ins = bpx_get_call_arg(3,3);

    p_sw_list0 = bpx_build_nil();
    num_sw_ins = 0;
    for (i = 0; i < occ_switch_tab_size; i++) {
        SW_INS_PTR  ptr;

#ifdef __YAP_PROLOG__
	if ( heap_top + 64*1024 >= local_top ) {			    
	  HR = hstart;
	  /* running out of stack */
	  extern int Yap_gcl(UInt gc_lim, Int predarity, CELL *current_env, yamop *nextop);

	  Yap_gcl(4*64*1024, 3, ENV, CP);
	  goto restart;
	}
#endif

        sw = bpx_build_structure("sw",2);
        bpx_unify(bpx_get_arg(1,sw), bpx_build_integer(i));

        p_sw_ins_list0 = bpx_build_nil();
        ptr = occ_switches[i];
        while (ptr != NULL) {
            num_sw_ins++;

            if (ptr->inside <= 0.0) ptr->inside = 0.0;  /* FIXME: quick hack */

            sw_ins = bpx_build_structure("sw_ins",4);
            bpx_unify(bpx_get_arg(1,sw_ins),bpx_build_integer(ptr->id));
            bpx_unify(bpx_get_arg(2,sw_ins),bpx_build_float(ptr->inside));
            bpx_unify(bpx_get_arg(3,sw_ins),bpx_build_float(ptr->smooth));
            bpx_unify(bpx_get_arg(4,sw_ins),bpx_build_float(ptr->total_expect));

            p_sw_ins_list1 = bpx_build_list();
            bpx_unify(bpx_get_car(p_sw_ins_list1),sw_ins);
            bpx_unify(bpx_get_cdr(p_sw_ins_list1),p_sw_ins_list0);
            p_sw_ins_list0 = p_sw_ins_list1;

            ptr = ptr->next;
        }

        bpx_unify(bpx_get_arg(2,sw),p_sw_ins_list0);

        p_sw_list1 = bpx_build_list();
        bpx_unify(bpx_get_car(p_sw_list1),sw);
        bpx_unify(bpx_get_cdr(p_sw_list1),p_sw_list0);
        p_sw_list0 = p_sw_list1;
    }

    release_occ_switches();

    return
        bpx_unify(p_sw_list,    p_sw_list0) &&
        bpx_unify(p_num_sw,     bpx_build_integer(occ_switch_tab_size)) &&
        bpx_unify(p_num_sw_ins, bpx_build_integer(num_sw_ins));
}

/*------------------------------------------------------------------------*/

void graph_stats(int stats[4])
{
    int num_goal_nodes = 0;
    int num_switch_nodes = 0;
    int total_shared = 0;
    int i;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        total_shared += eg_ptr->shared;

        path_ptr = eg_ptr->path_ptr;

        while (path_ptr != NULL) {
            num_goal_nodes += path_ptr->children_len;
            num_switch_nodes += path_ptr->sws_len;
            path_ptr = path_ptr->next;
        }
    }

    stats[0] = sorted_egraph_size;
    stats[1] = num_goal_nodes;
    stats[2] = num_switch_nodes;
    stats[3] = total_shared;
}

int pc_import_graph_stats_4(void)
{
    int stats[4];
    double avg_shared;

    graph_stats(stats);
    avg_shared = (double)(stats[3]) / stats[0];

    return
        bpx_unify(bpx_get_call_arg(1,4), bpx_build_integer(stats[0])) &&
        bpx_unify(bpx_get_call_arg(2,4), bpx_build_integer(stats[1])) &&
        bpx_unify(bpx_get_call_arg(3,4), bpx_build_integer(stats[2])) &&
        bpx_unify(bpx_get_call_arg(4,4), bpx_build_float(avg_shared));
}
