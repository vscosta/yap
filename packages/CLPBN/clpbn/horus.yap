/*******************************************************

 Horus Interface

********************************************************/

:- module(clpbn_horus,
		[set_horus_flag/1,
		 cpp_create_lifted_network/3,
		 cpp_create_ground_network/4,
		 cpp_set_parfactors_params/3,
		 cpp_set_factors_params/3,
		 cpp_run_lifted_solver/3,
		 cpp_run_ground_solver/3,
		 cpp_set_vars_information/2,
		 cpp_set_horus_flag/2,
		 cpp_free_lifted_network/1,
		 cpp_free_ground_network/1
		]).


:- catch(load_foreign_files([horus], [], init_predicates), _, patch_things_up)
	-> true ; warning.


patch_things_up :-
	assert_static(clpbn_horus:cpp_set_horus_flag(_,_)).


warning :-
	format(user_error,"Horus library not installed: cannot use hve, bp, cbp, lve, lkc and lbp~n.",[]).


set_horus_flag(K,V) :- cpp_set_horus_flag(K,V).


:- cpp_set_horus_flag(schedule, seq_fixed).
%:- cpp_set_horus_flag(schedule, seq_random).
%:- cpp_set_horus_flag(schedule, parallel).
%:- cpp_set_horus_flag(schedule, max_residual).

:- cpp_set_horus_flag(accuracy, 0.0001).

:- cpp_set_horus_flag(max_iter, 1000).

:- cpp_set_horus_flag(use_logarithms, false).
% :- cpp_set_horus_flag(use_logarithms, true).

