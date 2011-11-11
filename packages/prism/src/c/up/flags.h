#ifndef FLAGS_H
#define FLAGS_H

/*========================================================================*/

int pc_set_daem_1(void);
int pc_set_em_message_1(void);
int pc_set_em_progress_1(void);
int pc_set_error_on_cycle_1(void);
int pc_set_explicit_empty_expls_1(void);
int pc_set_fix_init_order_1(void);
int pc_set_init_method_1(void);
int pc_set_itemp_init_1(void);
int pc_set_itemp_rate_1(void);
int pc_set_log_scale_1(void);
int pc_set_max_iterate_1(void);
int pc_set_num_restart_1(void);
int pc_set_prism_epsilon_1(void);
int pc_set_show_itemp_1(void);
int pc_set_std_ratio_1(void);
int pc_set_verb_em_1(void);
int pc_set_verb_graph_1(void);
int pc_set_warn_1(void);
int pc_set_debug_level_1(void);

/*========================================================================*/

extern int     daem;
extern int     em_message;
extern int     em_progress;
extern int     error_on_cycle;
extern int     explicit_empty_expls;
extern int     fix_init_order;
extern int     init_method;
extern double  itemp_init;
extern double  itemp_rate;
extern int     log_scale;
extern int     max_iterate;
extern int     num_restart;
extern double  prism_epsilon;
extern int     show_itemp;
extern double  std_ratio;
extern int     verb_em;
extern int     verb_graph;
extern int     warn;
extern int     debug_level;

#endif /* FLAGS_H */
