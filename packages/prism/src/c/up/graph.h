#ifndef GRAPH_H
#define GRAPH_H

/*====================================================================*/

#define INIT_MAX_SW_TABLE_SIZE       16
#define INIT_MAX_SW_INS_TABLE_SIZE   64
#define INIT_MAX_EGRAPH_SIZE         (1 << 8)
#define MAX_EGRAPH_SIZE_EXPAND_LIMIT (128 << 10)

/* node_id should be non-negative */
#define UPDATE_MIN_MAX_NODE_NOS(node_id) do {               \
    if (min_node_index < 0 || node_id < min_node_index)     \
        min_node_index = node_id;                           \
    if (node_id > max_node_index)                           \
        max_node_index = node_id;                           \
	} while (0)
#define INIT_MIN_MAX_NODE_NOS do {                          \
        min_node_index = -1;                                \
        max_node_index = -1;                                \
	} while (0)
#define INIT_VISITED_FLAGS do {                             \
        int i;                                              \
        for (i = min_node_index; i <= max_node_index; i++)  \
            expl_graph[i]->visited = 0;                     \
    } while (0)

/*====================================================================*/

int pc_alloc_egraph_0(void);
int pc_clean_base_egraph_0(void);
int pc_clean_egraph_0(void);
int pc_export_switch_2(void);
int pc_add_egraph_path_3(void);
int pc_alloc_sort_egraph_1(void);
int pc_clean_external_tables_0(void);
int pc_export_sw_info_1(void);
int pc_import_sorted_graph_size_1(void);
int pc_import_sorted_graph_gid_2(void);
int pc_import_sorted_graph_paths_2(void);
int pc_get_gnode_inside_2(void);
int pc_get_gnode_outside_2(void);
int pc_get_gnode_viterbi_2(void);
int pc_get_snode_inside_2(void);
int pc_get_snode_expectation_2(void);
int pc_import_occ_switches_3(void);
void graph_stats(int[4]);

/*--------------------------------------------------------------------*/

void alloc_sorted_egraph(int);
void initialize_egraph_index(void);
int sort_one_egraph(int, int, int);
int sort_egraphs(TERM);

/*====================================================================*/

extern int sorted_egraph_size;
extern EG_NODE_PTR *expl_graph;
extern EG_NODE_PTR *sorted_expl_graph;
extern int num_roots;
extern int num_goals;

extern ROOT *roots;

extern int min_node_index;
extern int max_node_index;

extern int sw_tab_size;
extern int sw_ins_tab_size;
extern int occ_switch_tab_size;

extern SW_INS_PTR *switches;
extern SW_INS_PTR *switch_instances;
extern SW_INS_PTR *occ_switches;

extern int failure_subgoal_id;
extern int failure_root_index;

/*====================================================================*/

#endif /* GRAPH_H */
