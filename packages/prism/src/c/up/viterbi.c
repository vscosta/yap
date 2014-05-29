#include "core/gamma.h"
#include "up/up.h"
#include "up/graph.h"
#include "up/graph_aux.h"
#include "up/em_aux.h"
#include "up/em_aux_vb.h"
#include "up/flags.h"

/*------------------------------------------------------------------------*/

typedef struct ViterbiRankEntry *V_RANK_PTR;
struct ViterbiRankEntry {
    int size;
    V_ENT_PTR *expl;
    double score;
};

/*------------------------------------------------------------------------*/

/* mic.c (B-Prolog) */
NORET quit(const char *);
NORET myquit(int, const char *);

int pc_compute_n_viterbi_3(void);
int pc_compute_n_viterbi_rerank_4(void);
void transfer_hyperparams_prolog(void);

/*------------------------------------------------------------------------*/

static EG_NODE_PTR *  viterbi_egraphs = NULL;
static int            max_viterbi_egraph_size;
static int            viterbi_egraph_size;

static V_LIST_PTR     queue_first;
static V_LIST_PTR     queue_last;
static int            queue_len;
static V_LIST_PTR     top_n_first;
static V_LIST_PTR     top_n_last;
static int            top_n_len;
static V_ENT_PTR *    n_viterbi_egraphs = NULL;
static int            max_n_viterbi_egraph_size;
static int            n_viterbi_egraph_size;

static V_RANK_PTR     viterbi_rank = NULL;

void compute_max(void);
void compute_n_max(int n);
int pc_compute_viterbi_5(void);

/* Viterbi works on only one explanation graph */
void compute_max(void)
{
    int i,k;
    double max_p,this_path_max;
    EG_PATH_PTR max_path = NULL;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;

    if (log_scale) {
        for (i = 0; i < sorted_egraph_size; i++) {
            max_p = 1.0;          /* any positive value is possible */
            eg_ptr = sorted_expl_graph[i];
            path_ptr = eg_ptr->path_ptr;

            /* path_ptr should not be NULL; but it happens */
            if (path_ptr == NULL) {
                max_p = 0.0;      /* log-scale */
                max_path = NULL;
            }

            /* [Note] we perform probability computations in log-scale */
            while (path_ptr != NULL) {
                this_path_max = 0.0;
                for (k = 0; k < path_ptr->children_len; k++) {
                    this_path_max += path_ptr->children[k]->max;
                }
                for (k = 0; k < path_ptr->sws_len; k++) {
                    this_path_max += log(path_ptr->sws[k]->inside);
                }
                path_ptr->max = this_path_max;

                if (max_p > 0 || max_p <= this_path_max) {
                    max_p = this_path_max;
                    max_path = path_ptr;
                }

                path_ptr = path_ptr->next;
            }

            sorted_expl_graph[i]->max = max_p;
            sorted_expl_graph[i]->max_path = max_path;
        }
    }
    else {
        for (i = 0; i < sorted_egraph_size; i++) {
            max_p = 0.0;
            eg_ptr = sorted_expl_graph[i];
            path_ptr = eg_ptr->path_ptr;

            /* path_ptr should not be NULL; but it happens */
            if (path_ptr == NULL) {
                max_p = 1.0;
                max_path = NULL;
            }

            while (path_ptr != NULL) {
                this_path_max = 1.0;
                for (k = 0; k < path_ptr->children_len; k++) {
                    this_path_max *= path_ptr->children[k]->max;
                }
                for (k = 0; k < path_ptr->sws_len; k++) {
                    this_path_max *= path_ptr->sws[k]->inside;
                }
                path_ptr->max = this_path_max;

                if (this_path_max > max_p) {
                    max_p = this_path_max;
                    max_path = path_ptr;
                }

                path_ptr = path_ptr->next;
            }

            sorted_expl_graph[i]->max = max_p;
            sorted_expl_graph[i]->max_path = max_path;
        }
    }

}

static void clean_queue(void)
{
    V_LIST_PTR ptr,next_ptr;

    ptr = queue_first;
    while (ptr != NULL) {
        next_ptr = ptr->next;
        free(ptr);
        ptr = next_ptr;
    }
    queue_first = queue_last = NULL;
    queue_len = 0;
}

static void clean_top_n(void)
{
    V_LIST_PTR ptr,next_ptr;

    ptr = top_n_first;
    while (ptr != NULL) {
        next_ptr = ptr->next;
        free(ptr);
        ptr = next_ptr;
    }
    top_n_first = top_n_last = NULL;
    top_n_len = 0;
}

void compute_n_max(int n)
{
    int i,k,j,m;
    EG_NODE_PTR eg_ptr;
    EG_PATH_PTR path_ptr;
    V_LIST_PTR queue_ptr;
    V_LIST_PTR top_n_ptr,next_top_n_ptr,new_top_n_ptr,old_top_n_last;
    V_ENT_PTR v_ent;
    V_ENT_PTR v_ent_next;
    double p;
    int inserted;
    int old_mth_index,new_mth_index;
    EG_NODE_PTR mth_child;

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];
        eg_ptr->inside = -1.0;
        eg_ptr->outside = -1.0;

        if (eg_ptr->path_ptr != NULL) {
            eg_ptr->top_n = (V_ENT_PTR *)MALLOC(sizeof(V_ENT_PTR) * n);
            for (j = 0; j < n; j++)
                eg_ptr->top_n[j] = NULL;
        }
        else {
            eg_ptr->top_n = NULL;
        }
        eg_ptr->top_n_len = 0;
    }

    for (i = 0; i < sorted_egraph_size; i++) {
        eg_ptr = sorted_expl_graph[i];

        queue_len = 0;
        queue_first = queue_last = NULL;

        path_ptr = eg_ptr->path_ptr;

        if (path_ptr == NULL) continue;

        /* Constructing the initial queue: */
        while (path_ptr != NULL) {

            /* Create an entry which is the most probable for the path */
            v_ent = (V_ENT_PTR)MALLOC(sizeof(struct ViterbiEntry));
            v_ent->goal_id = eg_ptr->id;
            v_ent->path_ptr = path_ptr;
            v_ent->children_len = path_ptr->children_len;
            v_ent->top_n_index =
                (int *)MALLOC(sizeof(int) * path_ptr->children_len);

            for (k = 0; k < path_ptr->children_len; k++) {
                v_ent->top_n_index[k] = 0;
            }
            if (log_scale) {
                p = 0.0;
                for (k = 0; k < path_ptr->children_len; k++) {
                    if (path_ptr->children[k]->top_n != NULL)
                        p += path_ptr->children[k]->top_n[0]->max;
                }
                for (k = 0; k < path_ptr->sws_len; k++) {
                    p += log(path_ptr->sws[k]->inside);
                }
            }
            else {
                p = 1.0;
                for (k = 0; k < path_ptr->children_len; k++) {
                    if (path_ptr->children[k]->top_n != NULL)
                        p *= path_ptr->children[k]->top_n[0]->max;
                }
                for (k = 0; k < path_ptr->sws_len; k++) {
                    p *= path_ptr->sws[k]->inside;
                }
            }
            v_ent->max = p;

            /* Enqueue the entry */
            queue_ptr = (V_LIST_PTR)MALLOC(sizeof(struct ViterbiList));
            queue_ptr->entry = v_ent;
            queue_ptr->prev = NULL;  /* Never use for the queue */
            queue_ptr->next = NULL;
            if (queue_first == NULL) {
                queue_first = queue_last = queue_ptr;
                queue_len = 1;
            }
            else {
                queue_last->next = queue_ptr;
                queue_last = queue_ptr;
                queue_len++;
            }

            path_ptr = path_ptr->next;
        }

        /* Create the header of top-N list */
        top_n_first = (V_LIST_PTR)MALLOC(sizeof(struct ViterbiList));
        top_n_first->entry = NULL;  /* null entry */
        top_n_first->prev = NULL;
        top_n_first->next = NULL;
        top_n_last = top_n_first;
        top_n_len = 0;

        while (queue_len > 0) {
            /* Dequeue */
            v_ent = queue_first->entry;
            queue_ptr = queue_first;
            queue_first = queue_ptr->next;
            free(queue_ptr);
            queue_len--;

            /** Add the element to the top-N list **/
            top_n_ptr = top_n_first;
            next_top_n_ptr = top_n_first->next;
            inserted = 0;
            while (next_top_n_ptr != NULL) { /* compare the current entry with the ones in the top-N list */
                if (v_ent->max > next_top_n_ptr->entry->max) {
                    new_top_n_ptr =
                        (V_LIST_PTR)MALLOC(sizeof(struct ViterbiList));

                    new_top_n_ptr->entry = v_ent;
                    new_top_n_ptr->prev = top_n_ptr;
                    new_top_n_ptr->next = next_top_n_ptr;

                    next_top_n_ptr->prev = new_top_n_ptr;
                    top_n_ptr->next = new_top_n_ptr;
                    top_n_len++;
                    inserted = 1;
                    break;
                }
                top_n_ptr = next_top_n_ptr;
                next_top_n_ptr = next_top_n_ptr->next;
            }

            if (top_n_len < n) {
                if (!inserted) {
                    new_top_n_ptr =
                        (V_LIST_PTR)MALLOC(sizeof(struct ViterbiList));
                    new_top_n_ptr->entry = v_ent;
                    new_top_n_ptr->prev = top_n_ptr;
                    new_top_n_ptr->next = NULL;

                    top_n_ptr->next = new_top_n_ptr;
                    top_n_last = new_top_n_ptr;
                    top_n_len++;
                    inserted = 1;
                }
            }
            else if (top_n_len == n) {
                if (!inserted) {
                    /* Erase the current entry */
                    free(v_ent->top_n_index);
                    free(v_ent);
                    v_ent = NULL;
                }
            }
            else {  /* top_n_len > n */
                if (!inserted) {
                    /* Erase the current entry */
                    free(v_ent->top_n_index);
                    free(v_ent);
                    v_ent = NULL;
                }
                else {
                    /* Erase the last entry */
                    old_top_n_last = top_n_last;
                    top_n_last = top_n_last->prev;
                    top_n_last->next = NULL;
                    free(old_top_n_last->entry->top_n_index);
                    free(old_top_n_last->entry);
                    free(old_top_n_last);
                    top_n_len--;
                }
            }

            /* If the current entry is not added to the top-N list, there is no
             * need to pursue the entries that have lower probabilities than
             * the current entry's probability.
             */
            if (!inserted) continue;

            /* Otherwise, propose the futher entries based on the current entry */
            for (m = 0; m < v_ent->children_len; m++) {

                old_mth_index = v_ent->top_n_index[m];
                new_mth_index = v_ent->top_n_index[m] + 1;
                mth_child = v_ent->path_ptr->children[m];

                if (new_mth_index >= mth_child->top_n_len)
                    continue;

                v_ent_next = (V_ENT_PTR)MALLOC(sizeof(struct ViterbiEntry));
                v_ent_next->goal_id = v_ent->goal_id;
                v_ent_next->path_ptr = v_ent->path_ptr;
                v_ent_next->children_len = v_ent->children_len;
                v_ent_next->top_n_index =
                    (int *)MALLOC(sizeof(int) * v_ent_next->children_len);

                for (k = 0; k < v_ent_next->children_len; k++) {
                    v_ent_next->top_n_index[k] =
                        (k == m) ?
                        (v_ent->top_n_index[k] + 1) : v_ent->top_n_index[k];
                }

                if (log_scale) {
                    v_ent_next->max =
                        v_ent->max
                        - mth_child->top_n[old_mth_index]->max
                        + mth_child->top_n[new_mth_index]->max;
                }
                else {
                    v_ent_next->max =
                        v_ent->max
                        * mth_child->top_n[new_mth_index]->max
                        / mth_child->top_n[old_mth_index]->max;
                }

                /* Enqueue the derived entries */
                queue_ptr = (V_LIST_PTR)MALLOC(sizeof(struct ViterbiList));
                queue_ptr->entry = v_ent_next;
                queue_ptr->prev = NULL;  /* Never use for the queue */
                queue_ptr->next = NULL;
                if (queue_first == NULL) {
                    queue_first = queue_last = queue_ptr;
                    queue_len = 1;
                }
                else {
                    queue_last->next = queue_ptr;
                    queue_last = queue_ptr;
                    queue_len++;
                }
            }
        }

        j = 0;
        top_n_ptr = top_n_first->next;
        while (top_n_ptr != NULL) {
            if (eg_ptr->top_n != NULL)
                eg_ptr->top_n[j] = top_n_ptr->entry;  /* shallow copy */
            j++;
            top_n_ptr = top_n_ptr->next;
        }
        eg_ptr->top_n_len = j;

        clean_queue();
        clean_top_n();
    }
}

static void alloc_viterbi_egraphs(void)
{
    int i;

    /* [Note] The size of viterbi path can exceed the number of subgoals in the
     *  explanation graph. we will expand the array size on demand.
     */
    viterbi_egraph_size = 0;
    max_viterbi_egraph_size = sorted_egraph_size;
    viterbi_egraphs =
        (EG_NODE_PTR *)MALLOC(max_viterbi_egraph_size * sizeof(EG_NODE_PTR));

    /* Initialize to extra Ids */
    for (i = 0; i < max_viterbi_egraph_size; i++)
        viterbi_egraphs[i] = NULL;
}

static void expand_viterbi_egraphs(int req_viterbi_egraph_size)
{
    int old_size,i;

    if (req_viterbi_egraph_size > max_viterbi_egraph_size) {
        old_size = max_viterbi_egraph_size;

        while (req_viterbi_egraph_size > max_viterbi_egraph_size) {
            max_viterbi_egraph_size *= 2;
        }

        viterbi_egraphs =
            (EG_NODE_PTR *)
            REALLOC(viterbi_egraphs,
                    max_viterbi_egraph_size * sizeof(EG_NODE_PTR));

        for (i = old_size; i < max_viterbi_egraph_size; i++) {
            viterbi_egraphs[i] = NULL;
        }
    }
}

static void alloc_n_viterbi_egraphs(void)
{
    int i;

    n_viterbi_egraph_size = 0;
    max_n_viterbi_egraph_size = sorted_egraph_size;
    n_viterbi_egraphs =
        (V_ENT_PTR *)MALLOC(max_n_viterbi_egraph_size * sizeof(V_ENT_PTR));

    for (i = 0; i < max_n_viterbi_egraph_size; i++) {
        n_viterbi_egraphs[i] = NULL;
    }
}

static void expand_n_viterbi_egraphs(int req_n_viterbi_egraph_size)
{
    int old_size,i;

    if (req_n_viterbi_egraph_size > max_n_viterbi_egraph_size) {
        old_size = max_n_viterbi_egraph_size;

        while (req_n_viterbi_egraph_size > max_n_viterbi_egraph_size) {
            max_n_viterbi_egraph_size *= 2;
        }

        n_viterbi_egraphs =
            (V_ENT_PTR *)REALLOC(n_viterbi_egraphs,
                                 max_n_viterbi_egraph_size * sizeof(V_ENT_PTR));

        for (i = old_size; i < max_n_viterbi_egraph_size; i++) {
            n_viterbi_egraphs[i] = NULL;
        }
    }
}

/* This function returns the last index of the current path */
static int visit_most_likely_path(EG_NODE_PTR eg_ptr,
                                  int start_vindex)
{
    int k;
    int curr_vindex;
    EG_PATH_PTR max_path;

    curr_vindex = start_vindex;

    if (curr_vindex >= max_viterbi_egraph_size)
        expand_viterbi_egraphs(curr_vindex + 1);

    if (curr_vindex >= viterbi_egraph_size)
        viterbi_egraph_size = curr_vindex + 1;

    viterbi_egraphs[curr_vindex] = eg_ptr;
    curr_vindex++;

    max_path = eg_ptr->max_path;

    if (max_path == NULL) return curr_vindex;

    for (k = 0; k < max_path->children_len; k++) {
        if (max_path->children == NULL) quit("Internal error: visit_most_likely_path\n");
        curr_vindex =
            visit_most_likely_path(max_path->children[k],curr_vindex);
    }

    return curr_vindex;
}

static void get_most_likely_path(int goal_id,
                                 TERM *p_goal_path_ptr,
                                 TERM *p_subpath_goal_ptr,
                                 TERM *p_subpath_sw_ptr,
                                 double *viterbi_prob_ptr)
{
    TERM p_goal_path;
    TERM p_subpath_goal, p_subpath_sw;
    TERM p_tmp, p_tmp_g, p_tmp_g0, p_tmp_g1, p_tmp_sw, p_tmp_sw0, p_tmp_sw1;
    int m,k;
    EG_NODE_PTR eg_ptr = NULL;
    EG_PATH_PTR path_ptr = NULL;
    int viterbi_egraph_size;
    int c_len, sw_len;

    alloc_viterbi_egraphs();

    viterbi_egraph_size = visit_most_likely_path(expl_graph[goal_id],0);

    /* Build the Viterbi path as a Prolog list: */
    p_goal_path = bpx_build_list();
    p_tmp = p_goal_path;
    for (m = 0; m < viterbi_egraph_size; m++) {
        bpx_unify(bpx_get_car(p_tmp),bpx_build_integer(viterbi_egraphs[m]->id));
        if (m == viterbi_egraph_size - 1) {
            bpx_unify(bpx_get_cdr(p_tmp),bpx_build_nil());
        }
        else {
            bpx_unify(bpx_get_cdr(p_tmp),bpx_build_list());
            p_tmp = bpx_get_cdr(p_tmp);
        }
    }

    p_subpath_goal = bpx_build_list();
    p_subpath_sw = bpx_build_list();

    p_tmp_g = p_subpath_goal;
    p_tmp_sw = p_subpath_sw;

    for (m = 0; m < viterbi_egraph_size; m++) {
        eg_ptr = viterbi_egraphs[m];

        if (eg_ptr->max_path == NULL) {
            p_tmp_g0 = bpx_build_nil();
            p_tmp_sw0 = bpx_build_nil();
        }
        else {
            path_ptr = eg_ptr->max_path;
            c_len = path_ptr->children_len;
            sw_len = path_ptr->sws_len;

            if (c_len == 0) {
                p_tmp_g0 = bpx_build_nil();
            }
            else {
                p_tmp_g0 = bpx_build_list();
                p_tmp_g1 = p_tmp_g0;
                for (k = 0; k < c_len; k++) {
                    bpx_unify(bpx_get_car(p_tmp_g1),
                              bpx_build_integer(path_ptr->children[k]->id));
                    if (k == c_len - 1) {
                        bpx_unify(bpx_get_cdr(p_tmp_g1),bpx_build_nil());
                    }
                    else {
                        bpx_unify(bpx_get_cdr(p_tmp_g1),bpx_build_list());
                        p_tmp_g1 = bpx_get_cdr(p_tmp_g1);
                    }
                }
            }

            if (sw_len == 0) {
                p_tmp_sw0 = bpx_build_nil();
            }
            else {
                p_tmp_sw0 = bpx_build_list();
                p_tmp_sw1 = p_tmp_sw0;
                for (k = 0; k < sw_len; k++) {
                    bpx_unify(bpx_get_car(p_tmp_sw1),bpx_build_integer(path_ptr->sws[k]->id));
                    if (k == sw_len - 1) {
                        bpx_unify(bpx_get_cdr(p_tmp_sw1),bpx_build_nil());
                    }
                    else {
                        bpx_unify(bpx_get_cdr(p_tmp_sw1),bpx_build_list());
                        p_tmp_sw1 = bpx_get_cdr(p_tmp_sw1);
                    }
                }
            }
        }

        bpx_unify(bpx_get_car(p_tmp_g),p_tmp_g0);
        bpx_unify(bpx_get_car(p_tmp_sw),p_tmp_sw0);

        if (m == viterbi_egraph_size - 1) {
            bpx_unify(bpx_get_cdr(p_tmp_g),bpx_build_nil());
            bpx_unify(bpx_get_cdr(p_tmp_sw),bpx_build_nil());
        }
        else {
            bpx_unify(bpx_get_cdr(p_tmp_g),bpx_build_list());
            bpx_unify(bpx_get_cdr(p_tmp_sw),bpx_build_list());
            p_tmp_g = bpx_get_cdr(p_tmp_g);
            p_tmp_sw = bpx_get_cdr(p_tmp_sw);
        }
    }

    free(viterbi_egraphs);
    viterbi_egraphs = NULL;

    *p_goal_path_ptr = p_goal_path;
    *p_subpath_goal_ptr = p_subpath_goal;
    *p_subpath_sw_ptr = p_subpath_sw;
    *viterbi_prob_ptr = expl_graph[goal_id]->max; /* top goal's max prob */
}

/* This function returns the last index of the current path */
static int visit_n_most_likely_path(V_ENT_PTR v_ent, int start_vindex)
{
    int k,j;
    int curr_vindex;
    V_ENT_PTR new_v_ent = NULL;

    curr_vindex = start_vindex;

    if (curr_vindex >= max_n_viterbi_egraph_size)
        expand_n_viterbi_egraphs(curr_vindex + 1);

    if (curr_vindex >= n_viterbi_egraph_size)
        n_viterbi_egraph_size = curr_vindex + 1;

    n_viterbi_egraphs[curr_vindex] = v_ent;
    curr_vindex++;

    for (k = 0; k < v_ent->children_len; k++) {
        if (v_ent->path_ptr->children == NULL)
            quit("Internal error: visit_n_most_likely_path\n");

        if (v_ent->path_ptr->children[k]->top_n == NULL) {
            new_v_ent = (V_ENT_PTR)MALLOC(sizeof(struct ViterbiEntry));
            new_v_ent->goal_id = v_ent->path_ptr->children[k]->id;
            new_v_ent->path_ptr = NULL;

            if (curr_vindex >= max_n_viterbi_egraph_size)
                expand_n_viterbi_egraphs(curr_vindex + 1);

            if (curr_vindex >= n_viterbi_egraph_size)
                n_viterbi_egraph_size = curr_vindex + 1;

            n_viterbi_egraphs[curr_vindex] = new_v_ent;
            curr_vindex++;
        }
        else {
            j = v_ent->top_n_index[k];
            curr_vindex =
                visit_n_most_likely_path(v_ent->path_ptr->children[k]->top_n[j],
                                         curr_vindex);
        }
    }

    return curr_vindex;
}

static void get_n_most_likely_path(int n, int goal_id,
                                   TERM *p_n_viterbi_list_ptr)
{
    TERM p_goal_path;
    TERM p_subpath_goal, p_subpath_sw;
    TERM p_tmp, p_tmp_g, p_tmp_g0, p_tmp_g1, p_tmp_sw, p_tmp_sw0, p_tmp_sw1;
    TERM p_n_viterbi, p_n_viterbi_list, p_tmp_list;
    TERM p_viterbi_prob;
    int j,m,k;
    EG_PATH_PTR path_ptr = NULL;
    int c_len, sw_len;
    V_ENT_PTR v_ent;

    p_n_viterbi_list = bpx_build_list();
    p_tmp_list = p_n_viterbi_list;

    for (j = 0; j < n; j++) {

        if (expl_graph[goal_id]->top_n[j] == NULL) continue;

        alloc_n_viterbi_egraphs();

        n_viterbi_egraph_size =
            visit_n_most_likely_path(expl_graph[goal_id]->top_n[j],0);

        /* Build the Viterbi path as a Prolog list: */
        p_goal_path = bpx_build_list();
        p_tmp = p_goal_path;
        for (m = 0; m < n_viterbi_egraph_size; m++) {
            bpx_unify(bpx_get_car(p_tmp),bpx_build_integer(n_viterbi_egraphs[m]->goal_id));
            if (m == n_viterbi_egraph_size - 1) {
                bpx_unify(bpx_get_cdr(p_tmp),bpx_build_nil());
            }
            else {
                bpx_unify(bpx_get_cdr(p_tmp),bpx_build_list());
                p_tmp = bpx_get_cdr(p_tmp);
            }
        }

        p_subpath_goal = bpx_build_list();
        p_subpath_sw = bpx_build_list();

        p_tmp_g = p_subpath_goal;
        p_tmp_sw = p_subpath_sw;

        for (m = 0; m < n_viterbi_egraph_size; m++) {
            v_ent = n_viterbi_egraphs[m];

            if (v_ent->path_ptr == NULL) {
                p_tmp_g0 = bpx_build_nil();
                p_tmp_sw0 = bpx_build_nil();
            }
            else {
                path_ptr = v_ent->path_ptr;
                c_len = path_ptr->children_len;
                sw_len = path_ptr->sws_len;

                if (c_len == 0) {
                    p_tmp_g0 = bpx_build_nil();
                }
                else {
                    p_tmp_g0 = bpx_build_list();
                    p_tmp_g1 = p_tmp_g0;
                    for (k = 0; k < c_len; k++) {
                        bpx_unify(bpx_get_car(p_tmp_g1),bpx_build_integer(path_ptr->children[k]->id));
                        if (k == c_len - 1) {
                            bpx_unify(bpx_get_cdr(p_tmp_g1),bpx_build_nil());
                        }
                        else {
                            bpx_unify(bpx_get_cdr(p_tmp_g1),bpx_build_list());
                            p_tmp_g1 = bpx_get_cdr(p_tmp_g1);
                        }
                    }
                }

                if (sw_len == 0) {
                    p_tmp_sw0 = bpx_build_nil();
                }
                else {
                    p_tmp_sw0 = bpx_build_list();
                    p_tmp_sw1 = p_tmp_sw0;
                    for (k = 0; k < sw_len; k++) {
                        bpx_unify(bpx_get_car(p_tmp_sw1),bpx_build_integer(path_ptr->sws[k]->id));
                        if (k == sw_len - 1) {
                            bpx_unify(bpx_get_cdr(p_tmp_sw1),bpx_build_nil());
                        }
                        else {
                            bpx_unify(bpx_get_cdr(p_tmp_sw1),bpx_build_list());
                            p_tmp_sw1 = bpx_get_cdr(p_tmp_sw1);
                        }
                    }
                }
            }

            bpx_unify(bpx_get_car(p_tmp_g),p_tmp_g0);
            bpx_unify(bpx_get_car(p_tmp_sw),p_tmp_sw0);

            if (m == n_viterbi_egraph_size - 1) {
                bpx_unify(bpx_get_cdr(p_tmp_g),bpx_build_nil());
                bpx_unify(bpx_get_cdr(p_tmp_sw),bpx_build_nil());
            }
            else {
                bpx_unify(bpx_get_cdr(p_tmp_g),bpx_build_list());
                bpx_unify(bpx_get_cdr(p_tmp_sw),bpx_build_list());
                p_tmp_g = bpx_get_cdr(p_tmp_g);
                p_tmp_sw = bpx_get_cdr(p_tmp_sw);
            }
        }

        p_viterbi_prob = bpx_build_float(expl_graph[goal_id]->top_n[j]->max);

        p_n_viterbi = bpx_build_structure("v_expl",5);
        bpx_unify(bpx_get_arg(1,p_n_viterbi),bpx_build_integer(j));
        bpx_unify(bpx_get_arg(2,p_n_viterbi),p_goal_path);
        bpx_unify(bpx_get_arg(3,p_n_viterbi),p_subpath_goal);
        bpx_unify(bpx_get_arg(4,p_n_viterbi),p_subpath_sw);
        bpx_unify(bpx_get_arg(5,p_n_viterbi),p_viterbi_prob);

        bpx_unify(bpx_get_car(p_tmp_list),p_n_viterbi);

        if (j == n - 1 ||
                (j < n - 1 && expl_graph[goal_id]->top_n[j + 1] == NULL)) {
            bpx_unify(bpx_get_cdr(p_tmp_list),bpx_build_nil());
        }
        else {
            bpx_unify(bpx_get_cdr(p_tmp_list),bpx_build_list());
            p_tmp_list = bpx_get_cdr(p_tmp_list);
        }

        for (m = 0; m < n_viterbi_egraph_size; m++) {
            /* Release the entries newly added in visit_n_most_likely_path() */
            if (n_viterbi_egraphs[m]->path_ptr == NULL) {
                free(n_viterbi_egraphs[m]);
            }
        }

        free(n_viterbi_egraphs);
        n_viterbi_egraphs = NULL;
    }

    *p_n_viterbi_list_ptr = p_n_viterbi_list;
}

static double compute_rerank_score(void)
{
    int i,s;
    V_ENT_PTR v_ent;
    EG_PATH_PTR path_ptr = NULL;
    int k;
    SW_INS_PTR sw_ins_ptr;
    double score = 0.0;
    double alpha_sum0,alpha_sum1;

    for (i = 0; i < occ_switch_tab_size; i++) {
        sw_ins_ptr = occ_switches[i];
        while (sw_ins_ptr != NULL) {
            sw_ins_ptr->count = 0;
            sw_ins_ptr = sw_ins_ptr->next;
        }
    }

    for (s = 0; s < n_viterbi_egraph_size; s++) {
        v_ent = n_viterbi_egraphs[s];
        path_ptr = v_ent->path_ptr;

        if (path_ptr == NULL) continue;

        for (k = 0; k < path_ptr->sws_len; k++) {
            path_ptr->sws[k]->count++;
        }
    }

    score = 0.0;
    for (i = 0; i < occ_switch_tab_size; i++) {

        alpha_sum0 = 0.0;
        alpha_sum1 = 0.0;
        sw_ins_ptr = occ_switches[i];
        while (sw_ins_ptr != NULL) {
		    alpha_sum0 += sw_ins_ptr->inside_h;
			alpha_sum1 += sw_ins_ptr->count + sw_ins_ptr->inside_h;
			sw_ins_ptr = sw_ins_ptr->next;
        }
        score += lngamma(alpha_sum0) - lngamma(alpha_sum1);

        sw_ins_ptr = occ_switches[i];
        while (sw_ins_ptr != NULL) {
            score += lngamma(sw_ins_ptr->count + sw_ins_ptr->inside_h);
            score -= lngamma(sw_ins_ptr->inside_h);
            sw_ins_ptr = sw_ins_ptr->next;
        }
    }

    return score;
}

static int compare_viterbi_rank(const void *a, const void *b)
{
    double score_a = ((V_RANK_PTR)a)->score;
    double score_b = ((V_RANK_PTR)b)->score;

    if (score_a > score_b) return -1;
    if (score_a < score_b) return 1;

    return 0;
}

static void get_n_most_likely_path_rerank(int n, int l, int goal_id,
        TERM *p_n_viterbi_list_ptr)
{
    TERM p_goal_path;
    TERM p_subpath_goal, p_subpath_sw;
    TERM p_tmp, p_tmp_g, p_tmp_g0, p_tmp_g1, p_tmp_sw, p_tmp_sw0, p_tmp_sw1;
    TERM p_n_viterbi, p_n_viterbi_list, p_tmp_list;
    TERM p_viterbi_prob;
    int j,m,k;
    EG_PATH_PTR path_ptr = NULL;
    int c_len, sw_len;
    V_ENT_PTR v_ent;
    int l_used;
    double n_viterbi_egraph_score;

    p_n_viterbi_list = bpx_build_list();
    p_tmp_list = p_n_viterbi_list;

    l_used = 0;
    for (j = 0; j < l; j++) {
        if (expl_graph[goal_id]->top_n[j] != NULL) l_used++;
    }

    viterbi_rank =
        (V_RANK_PTR)MALLOC(sizeof(struct ViterbiRankEntry) * l_used);

    for (j = 0; j < l_used; j++) {
        alloc_n_viterbi_egraphs();

        n_viterbi_egraph_size =
            visit_n_most_likely_path(expl_graph[goal_id]->top_n[j],0);

        viterbi_rank[j].size = n_viterbi_egraph_size;
        viterbi_rank[j].expl = n_viterbi_egraphs;
        viterbi_rank[j].score = compute_rerank_score();
    }

    qsort(viterbi_rank, l_used, sizeof(struct ViterbiRankEntry),
          compare_viterbi_rank);

    for (j = 0; j < l_used && j < n; j++) {
        n_viterbi_egraph_size = viterbi_rank[j].size;
        n_viterbi_egraphs = viterbi_rank[j].expl;
        n_viterbi_egraph_score = viterbi_rank[j].score;

        /* Build the Viterbi path as a Prolog list: */
        p_goal_path = bpx_build_list();
        p_tmp = p_goal_path;
        for (m = 0; m < n_viterbi_egraph_size; m++) {
            bpx_unify(bpx_get_car(p_tmp),
                      bpx_build_integer(n_viterbi_egraphs[m]->goal_id));

            if (m == n_viterbi_egraph_size - 1) {
                bpx_unify(bpx_get_cdr(p_tmp),bpx_build_nil());
            }
            else {
                bpx_unify(bpx_get_cdr(p_tmp),bpx_build_list());
                p_tmp = bpx_get_cdr(p_tmp);
            }
        }

        p_subpath_goal = bpx_build_list();
        p_subpath_sw = bpx_build_list();

        p_tmp_g = p_subpath_goal;
        p_tmp_sw = p_subpath_sw;

        for (m = 0; m < n_viterbi_egraph_size; m++) {
            v_ent = n_viterbi_egraphs[m];

            if (v_ent->path_ptr == NULL) {
                p_tmp_g0 = bpx_build_nil();
                p_tmp_sw0 = bpx_build_nil();
            }
            else {
                path_ptr = v_ent->path_ptr;
                c_len = path_ptr->children_len;
                sw_len = path_ptr->sws_len;

                if (c_len == 0) {
                    p_tmp_g0 = bpx_build_nil();
                }
                else {
                    p_tmp_g0 = bpx_build_list();
                    p_tmp_g1 = p_tmp_g0;
                    for (k = 0; k < c_len; k++) {
                        bpx_unify(bpx_get_car(p_tmp_g1),
                                  bpx_build_integer(path_ptr->children[k]->id));
                        if (k == c_len - 1) {
                            bpx_unify(bpx_get_cdr(p_tmp_g1),bpx_build_nil());
                        }
                        else {
                            bpx_unify(bpx_get_cdr(p_tmp_g1),bpx_build_list());
                            p_tmp_g1 = bpx_get_cdr(p_tmp_g1);
                        }
                    }
                }

                if (sw_len == 0) {
                    p_tmp_sw0 = bpx_build_nil();
                }
                else {
                    p_tmp_sw0 = bpx_build_list();
                    p_tmp_sw1 = p_tmp_sw0;
                    for (k = 0; k < sw_len; k++) {
                        bpx_unify(bpx_get_car(p_tmp_sw1),bpx_build_integer(path_ptr->sws[k]->id));
                        if (k == sw_len - 1) {
                            bpx_unify(bpx_get_cdr(p_tmp_sw1),bpx_build_nil());
                        }
                        else {
                            bpx_unify(bpx_get_cdr(p_tmp_sw1),bpx_build_list());
                            p_tmp_sw1 = bpx_get_cdr(p_tmp_sw1);
                        }
                    }
                }
            }

            bpx_unify(bpx_get_car(p_tmp_g),p_tmp_g0);
            bpx_unify(bpx_get_car(p_tmp_sw),p_tmp_sw0);

            if (m == n_viterbi_egraph_size - 1) {
                bpx_unify(bpx_get_cdr(p_tmp_g),bpx_build_nil());
                bpx_unify(bpx_get_cdr(p_tmp_sw),bpx_build_nil());
            }
            else {
                bpx_unify(bpx_get_cdr(p_tmp_g),bpx_build_list());
                bpx_unify(bpx_get_cdr(p_tmp_sw),bpx_build_list());
                p_tmp_g = bpx_get_cdr(p_tmp_g);
                p_tmp_sw = bpx_get_cdr(p_tmp_sw);
            }
        }

        p_viterbi_prob = bpx_build_float(n_viterbi_egraph_score);

        p_n_viterbi = bpx_build_structure("v_expl",5);
        bpx_unify(bpx_get_arg(1,p_n_viterbi),bpx_build_integer(j));
        bpx_unify(bpx_get_arg(2,p_n_viterbi),p_goal_path);
        bpx_unify(bpx_get_arg(3,p_n_viterbi),p_subpath_goal);
        bpx_unify(bpx_get_arg(4,p_n_viterbi),p_subpath_sw);
        bpx_unify(bpx_get_arg(5,p_n_viterbi),p_viterbi_prob);

        bpx_unify(bpx_get_car(p_tmp_list),p_n_viterbi);

        if (j == (l_used - 1) || j == (n - 1)) {
            bpx_unify(bpx_get_cdr(p_tmp_list),bpx_build_nil());
        }
        else {
            bpx_unify(bpx_get_cdr(p_tmp_list),bpx_build_list());
            p_tmp_list = bpx_get_cdr(p_tmp_list);
        }
    }

    for (j = 0; j < l_used; j++) {
        free(viterbi_rank[j].expl);
    }
    free(viterbi_rank);
    viterbi_rank = NULL;

    *p_n_viterbi_list_ptr = p_n_viterbi_list;
}

/*------------------------------------------------------------------------*/

/* [Note] node copying is not required here even in computation without
 * inter-goal sharing, but we need to declare it explicitly.
 */
int pc_compute_viterbi_5(void)
{
    TERM p_goal_path,p_subpath_goal,p_subpath_sw;
    int goal_id;
    double viterbi_prob;

    goal_id = bpx_get_integer(bpx_get_call_arg(1,5));

    initialize_egraph_index();
    alloc_sorted_egraph(1);
    /* INIT_MIN_MAX_NODE_NOS; */
    RET_ON_ERR(sort_one_egraph(goal_id,0,1));
    if (verb_graph) print_egraph(0,PRINT_NEUTRAL);

    compute_max();

    if (debug_level) print_egraph(1,PRINT_VITERBI);

    get_most_likely_path(goal_id,&p_goal_path,&p_subpath_goal,
                         &p_subpath_sw,&viterbi_prob);

    return
        bpx_unify(bpx_get_call_arg(2,5), p_goal_path)    &&
        bpx_unify(bpx_get_call_arg(3,5), p_subpath_goal) &&
        bpx_unify(bpx_get_call_arg(4,5), p_subpath_sw)   &&
        bpx_unify(bpx_get_call_arg(5,5), bpx_build_float(viterbi_prob));
}

int pc_compute_n_viterbi_3(void)
{
    TERM p_n_viterbi_list;
    int n,goal_id;

    n       = bpx_get_integer(bpx_get_call_arg(1,3));
    goal_id = bpx_get_integer(bpx_get_call_arg(2,3));

    initialize_egraph_index();
    alloc_sorted_egraph(1);
    /* INIT_MIN_MAX_NODE_NOS; */
    RET_ON_ERR(sort_one_egraph(goal_id,0,1));
    if (verb_graph) print_egraph(0,PRINT_NEUTRAL);

    compute_n_max(n);

    if (debug_level) print_egraph(1,PRINT_VITERBI);

    get_n_most_likely_path(n,goal_id,&p_n_viterbi_list);

    return bpx_unify(bpx_get_call_arg(3,3),p_n_viterbi_list);
}

/*
 * Note: parameters are always refreshed in advance by $pc_export_sw_info/1,
 *       so it causes no problem to overwrite them temporarily
 */
int pc_compute_n_viterbi_rerank_4(void)
{
    TERM p_n_viterbi_list;
    int n,l,goal_id;

    n       = bpx_get_integer(bpx_get_call_arg(1,4));
    l       = bpx_get_integer(bpx_get_call_arg(2,4));
    goal_id = bpx_get_integer(bpx_get_call_arg(3,4));

    initialize_egraph_index();
    alloc_sorted_egraph(1);
    /* INIT_MIN_MAX_NODE_NOS; */
    RET_ON_ERR(sort_one_egraph(goal_id,0,1));
    if (verb_graph) print_egraph(0,PRINT_NEUTRAL);

    alloc_occ_switches();
    transfer_hyperparams_prolog();
	get_param_means();

    compute_n_max(l);

    get_n_most_likely_path_rerank(n,l,goal_id,&p_n_viterbi_list);

    release_occ_switches();

    return bpx_unify(bpx_get_call_arg(4,4),p_n_viterbi_list);
}

/*------------------------------------------------------------------------*/
