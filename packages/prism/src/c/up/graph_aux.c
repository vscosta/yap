#include <stdlib.h>
#include "bprolog.h"
#include "up/up.h"
#include "up/graph.h"
#include "up/graph_aux.h"
#include "up/flags.h"

/*------------------------------------------------------------------------*/

/* mic.c (B-Prolog) */
void quit(const char *);

/*------------------------------------------------------------------------*/

static EG_NODE_PTR *subgraph;
static int subgraph_size;
static int max_subgraph_size;

/*------------------------------------------------------------------------*/

static void alloc_subgraph(void)
{
    max_subgraph_size = INIT_MAX_EGRAPH_SIZE;
    subgraph = (EG_NODE_PTR *)MALLOC(sizeof(EG_NODE_PTR) * max_subgraph_size);
}

static void expand_subgraph(int req_subgraph_size)
{
    if (req_subgraph_size > max_subgraph_size) {
        while (req_subgraph_size > max_subgraph_size) {
            if (max_subgraph_size > MAX_EGRAPH_SIZE_EXPAND_LIMIT)
                max_subgraph_size += MAX_EGRAPH_SIZE_EXPAND_LIMIT;
            else
                max_subgraph_size *= 2;
        }

        subgraph = REALLOC(subgraph, sizeof(EG_NODE_PTR) * max_subgraph_size);
    }
}

static void release_subgraph(void)
{
    free(subgraph);
    subgraph = NULL;
}

static void traverse_egraph(EG_NODE_PTR node_ptr)
{
    int i;
    EG_NODE_PTR c_node_ptr;
    EG_PATH_PTR path_ptr;

    node_ptr->visited = 1;
    path_ptr = node_ptr->path_ptr;

    while (path_ptr != NULL) {
        for (i = 0; i < path_ptr->children_len; i++) {
            c_node_ptr = path_ptr->children[i];
            if (c_node_ptr->visited != 1) {
                if (c_node_ptr->visited == 0) {
                    traverse_egraph(c_node_ptr);
                }
                expand_subgraph(subgraph_size + 1);
                subgraph[subgraph_size] = c_node_ptr;
                subgraph_size++;
            }
        }
        path_ptr = path_ptr->next;
    }
}

/*------------------------------------------------------------------------*/

/* `mode' is a macro prefixed by `PRINT_' */
void print_egraph(int level, int mode)
{
    ROOT root_ptr;
    EG_NODE_PTR eg_ptr, node_ptr;
    EG_PATH_PTR path_ptr;
    SW_INS_PTR sw_ptr;
    int log_scale1;
    int r,u,e,i,k,len;

    /* disable scaling for non-learning */
    log_scale1 = (mode > 0) ? log_scale : 0;

    alloc_subgraph();

    for (r = 0; r < num_roots; r++) {
        root_ptr = roots[r];

        if (level >= 1) {
            fprintf(curr_out,"  <<Goal[%d]: %s (id=%d, count=%d)>>\n",
                    r,prism_goal_string(root_ptr->id),
                    root_ptr->id,root_ptr->count);
        }
        else {
            fprintf(curr_out,"  <<Goal[%d]: (count=%d)>>\n",r,root_ptr->count);
        }

        subgraph_size = 0;

        traverse_egraph(expl_graph[root_ptr->id]);
        expand_subgraph(subgraph_size + 1);
        subgraph[subgraph_size] = expl_graph[root_ptr->id];

        for (i = subgraph_size; i >= 0; i--) {
            eg_ptr = subgraph[i];

            if (eg_ptr->visited == 2) {
                fprintf(curr_out,"  g[%d]:%s\n",
                        eg_ptr->id,prism_goal_string(eg_ptr->id));
                fprintf(curr_out,"    **** already shown ****\n");
                continue;
            }

            eg_ptr->visited = 2;

            if (level == 0) {
                fprintf(curr_out,"  g[%d]:%s\n",
                        eg_ptr->id,prism_goal_string(eg_ptr->id));
            }
            if (level >= 3) {
                fprintf(curr_out,"  g[%d]:%s.addr = <%p>\n",
                        eg_ptr->id,prism_goal_string(eg_ptr->id),eg_ptr);
            }
            if (level >= 1) {
                if (log_scale1) {
                    fprintf(curr_out,"  g[%d]:%s.inside = %.9e (%.9e)\n",
                            eg_ptr->id,prism_goal_string(eg_ptr->id),
                            eg_ptr->inside,exp(eg_ptr->inside));
                    fprintf(curr_out,"  g[%d]:%s.outside = %.9e (%.9e)\n",
                            eg_ptr->id,prism_goal_string(eg_ptr->id),
                            eg_ptr->outside,exp(eg_ptr->outside));
                    fprintf(curr_out,"  g[%d]:%s.first_outside = %.9e (%.9e)\n",
                            eg_ptr->id,prism_goal_string(eg_ptr->id),
                            eg_ptr->first_outside,exp(eg_ptr->first_outside));
                }
                else {
                    fprintf(curr_out,"  g[%d]:%s.inside = %.9e\n",
                            eg_ptr->id,prism_goal_string(eg_ptr->id),
                            eg_ptr->inside);
                    fprintf(curr_out,"  g[%d]:%s.outside = %.9e\n",
                            eg_ptr->id,prism_goal_string(eg_ptr->id),
                            eg_ptr->outside);
                }
                if (mode == PRINT_VITERBI) {
                    fprintf(curr_out,"  g[%d]:%s.max = %.9e\n",
                            eg_ptr->id,prism_goal_string(eg_ptr->id),
                            eg_ptr->max);
                    fprintf(curr_out,"  g[%d]:%s.top_n_len = %d\n",
                            eg_ptr->id,prism_goal_string(eg_ptr->id),
                            eg_ptr->top_n_len);
                    if (eg_ptr->top_n != NULL) {
                        for (e = 0; e < eg_ptr->top_n_len; e++) {
                            if (eg_ptr->top_n[e] == NULL) continue;
                            fprintf(curr_out,"      top_n[%d]->goal_id = %d\n",
                                    e,eg_ptr->top_n[e]->goal_id);
                            fprintf(curr_out,"      top_n[%d]->path_ptr = %p\n",
                                    e,eg_ptr->top_n[e]->path_ptr);
                            len = eg_ptr->top_n[e]->children_len;
                            for (k = 0; k < len; k++) {
                                fprintf(curr_out,
                                        "      top_n[%d]->goal[%d] = %s (%d)\n",
                                        e,k,prism_goal_string(eg_ptr->top_n[e]->path_ptr->children[k]->id),eg_ptr->top_n[e]->path_ptr->children[k]->id);
                                fprintf(curr_out,"      top_n[%d]->top_n_index[%d] = %d\n",
                                        e,k,eg_ptr->top_n[e]->top_n_index[k]);
                            }
                            fprintf(curr_out,"      top_n[%d]->max = %.9e\n",
                                    e,eg_ptr->top_n[e]->max);
                        }
                    }
                }
            }

            path_ptr = eg_ptr->path_ptr;
            u = 0;
            while (path_ptr != NULL) {
                if (level == 0) {
                    fprintf(curr_out,"    path[%d]:\n",u);
                }
                if (level >= 3) {
                    fprintf(curr_out,"    path[%d].chilren_len = %d\n",
                            u,path_ptr->children_len);
                    fprintf(curr_out,"    path[%d].sws_len = %d\n",
                            u,path_ptr->sws_len);
                }
                if (level >= 1) {
  				    if (log_scale1) {
                        fprintf(curr_out,"    path[%d].inside = %.9e (%.9e)\n",
                                u,path_ptr->inside,exp(path_ptr->inside));
                    }
                    else {
                        fprintf(curr_out,"    path[%d].inside = %.9e\n",
                                u,path_ptr->inside);
                    }
                }
                for (k = 0; k < path_ptr->children_len; k++) {
                    node_ptr = path_ptr->children[k];
                    if (level == 0) {
                        fprintf(curr_out,"      g[%d]:%s\n",
                                node_ptr->id,prism_goal_string(node_ptr->id));
                    }
                    if (level >= 3) {
                        fprintf(curr_out,"      g[%d]:%s.addr = <%p>\n",
                                node_ptr->id,prism_goal_string(node_ptr->id),
                                node_ptr);
                    }
                    if (level >= 1) {
                        if (log_scale1) {
                            fprintf(curr_out,
                                    "      g[%d]:%s.inside = %.9e (%.9e)\n",
                                    node_ptr->id,
                                    prism_goal_string(node_ptr->id),
                                    node_ptr->inside,exp(node_ptr->inside));
                            fprintf(curr_out,
                                    "      g[%d]:%s.outside = %.9e (%.9e)\n",
                                    node_ptr->id,
                                    prism_goal_string(node_ptr->id),
                                    node_ptr->outside,exp(node_ptr->outside));
                            fprintf(curr_out,
                                    "      g[%d]:%s.first_outside = %.9e (%.9e)\n",
                                    node_ptr->id,
                                    prism_goal_string(node_ptr->id),
                                    node_ptr->first_outside,
                                    exp(node_ptr->first_outside));
                        }
                        else {
                            fprintf(curr_out,"      g[%d]:%s.inside = %.9e\n",
                                    node_ptr->id,
                                    prism_goal_string(node_ptr->id),
                                    node_ptr->inside);
                            fprintf(curr_out,"      g[%d]:%s.outside = %.9e\n",
                                    node_ptr->id,
                                    prism_goal_string(node_ptr->id),
                                    node_ptr->outside);
                        }
                    }
                }
                for (k = 0; k < path_ptr->sws_len; k++) {
                    sw_ptr = path_ptr->sws[k];
                    if (level == 0) {
                        fprintf(curr_out,"      sw[%d]:%s\n",
                                sw_ptr->id,prism_sw_ins_string(sw_ptr->id));
                    }
                    if (level >= 1) {
                        if (mode == PRINT_EM) {
                            fprintf(curr_out,"      sw[%d]:%s.inside = %.9e\n",
                                    sw_ptr->id,
                                    prism_sw_ins_string(sw_ptr->id),
                                    sw_ptr->inside);
                            fprintf(curr_out,"      sw[%d]:%s.total_e = %.9e\n",
                                    sw_ptr->id,
                                    prism_sw_ins_string(sw_ptr->id),
                                    sw_ptr->total_expect);
                        }
                        if (mode == PRINT_VBEM) {
                            fprintf(curr_out,"      sw[%d]:%s.pi = %.9e\n",
                                    sw_ptr->id,
                                    prism_sw_ins_string(sw_ptr->id),
                                    sw_ptr->pi);
                            fprintf(curr_out,"      sw[%d]:%s.smooth = %.9e\n",
                                    sw_ptr->id,
                                    prism_sw_ins_string(sw_ptr->id),
                                    sw_ptr->smooth);
                            fprintf(curr_out,"      sw[%d]:%s.inside = %.9e\n",
                                    sw_ptr->id,
                                    prism_sw_ins_string(sw_ptr->id),
                                    sw_ptr->inside);
                            fprintf(curr_out,
                                    "      sw[%d]:%s.inside_h = %.9e\n",
                                    sw_ptr->id,
                                    prism_sw_ins_string(sw_ptr->id),
                                    sw_ptr->inside_h);
                            fprintf(curr_out,"      sw[%d]:%s.total_e = %.9e\n",
                                    sw_ptr->id,
                                    prism_sw_ins_string(sw_ptr->id),
                                    sw_ptr->total_expect);
                        }
                        if (mode == PRINT_VITERBI) {
                            fprintf(curr_out,"      sw[%d]:%s.inside = %.9e\n",
                                    sw_ptr->id,
                                    prism_sw_ins_string(sw_ptr->id),
                                    sw_ptr->inside);
                        }
                    }
                }

                path_ptr = path_ptr->next;
                u++;
            }
        }
    }

    INIT_VISITED_FLAGS;
    release_subgraph();
}

/*------------------------------------------------------------------------*/
