/*******************************************************

 Horus Interface
 
********************************************************/

:- module(clpbn_horus,
		[set_solver/1,
		 set_horus_flag/1,
		 cpp_create_lifted_network/3,
		 cpp_create_ground_network/4,
		 cpp_set_parfactors_params/2,
		 cpp_set_factors_params/2,
		 cpp_run_lifted_solver/3,
		 cpp_run_ground_solver/3,
		 cpp_set_vars_information/2,
		 cpp_set_horus_flag/2,
		 cpp_free_lifted_network/1,
		 cpp_free_ground_network/1
		]).

:- use_module(library(clpbn),
		[set_clpbn_flag/2]).


patch_things_up :-
	assert_static(clpbn_horus:cpp_set_horus_flag(_,_)).


warning :-
	format(user_error,"Horus library not installed: cannot use bp, fove~n.",[]).


:- catch(load_foreign_files([horus], [], init_predicates), _, patch_things_up)
	-> true ; warning.


set_solver(ve)    :- !, set_clpbn_flag(solver,ve).
set_solver(bdd)   :- !, set_clpbn_flag(solver,bdd).
set_solver(jt)    :- !, set_clpbn_flag(solver,jt).
set_solver(gibbs) :- !, set_clpbn_flag(solver,gibbs).
set_solver(lve)   :- !, set_clpbn_flag(solver,fove), set_horus_flag(lifted_solver, lve).
set_solver(lbp)   :- !, set_clpbn_flag(solver,fove), set_horus_flag(lifted_solver, lbp).
set_solver(lkc)   :- !, set_clpbn_flag(solver,fove), set_horus_flag(lifted_solver, lkc).
set_solver(hve)   :- !, set_clpbn_flag(solver,bp),   set_horus_flag(ground_solver, ve).
set_solver(bp)    :- !, set_clpbn_flag(solver,bp),   set_horus_flag(ground_solver, bp).
set_solver(cbp)   :- !, set_clpbn_flag(solver,bp),   set_horus_flag(ground_solver, cbp).
set_solver(S)     :- throw(error('unknown solver ', S)).


set_horus_flag(K,V) :- cpp_set_horus_flag(K,V).


:- cpp_set_horus_flag(schedule, seq_fixed).
%:- cpp_set_horus_flag(schedule, seq_random).
%:- cpp_set_horus_flag(schedule, parallel).
%:- cpp_set_horus_flag(schedule, max_residual).

:- cpp_set_horus_flag(accuracy, 0.0001).

:- cpp_set_horus_flag(max_iter, 1000).

:- cpp_set_horus_flag(use_logarithms, false).
% :- cpp_set_horus_flag(use_logarithms, true).

