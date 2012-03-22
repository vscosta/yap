
:- module(clpbn_horus,
	[
    create_lifted_network/3,
    create_ground_network/2,
    set_parfactor_graph_params/2,
    set_bayes_net_params/2,
    run_lifted_solver/3,
    run_other_solvers/3,
    set_extra_vars_info/2,
    set_horus_flag/2,
    free_bayesian_network/1,
    free_parfactor_graph/1
    ]).

:- load_foreign_files(['horus'], [], init_predicates).
