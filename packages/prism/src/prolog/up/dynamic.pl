% predicate_info
:- dynamic $pd_is_prob_pred/2.
:- dynamic $pd_is_tabled_pred/2.

% switch_info
:- dynamic $pd_parameters/3.
:- dynamic $pd_hyperparameters/4.
:- dynamic $pd_expectations/3.
:- dynamic $pd_hyperexpectations/3.
:- dynamic $pd_fixed_parameters/1.
:- dynamic $pd_fixed_hyperparameters/1.

% dummy_goal_table
:- dynamic $pd_dummy_goal_table/2.

% learn_stats
:- dynamic $ps_log_likelihood/1.
:- dynamic $ps_log_post/1.
:- dynamic $ps_num_switches/1.
:- dynamic $ps_num_switch_values/1.
:- dynamic $ps_num_iterations/1.
:- dynamic $ps_num_iterations_vb/1.
:- dynamic $ps_bic_score/1.
:- dynamic $ps_cs_score/1.
:- dynamic $ps_free_energy/1.
:- dynamic $ps_learn_time/1.
:- dynamic $ps_learn_search_time/1.
:- dynamic $ps_em_time/1.
:- dynamic $ps_learn_table_space/1.

% graph_stats
:- dynamic $ps_num_subgraphs/1.
:- dynamic $ps_num_nodes/1.
:- dynamic $ps_num_goal_nodes/1.
:- dynamic $ps_num_switch_nodes/1.
:- dynamic $ps_avg_shared/1.

% infer_stats
:- dynamic $ps_infer_time/1.
:- dynamic $ps_infer_search_time/1.
:- dynamic $ps_infer_calc_time/1.
