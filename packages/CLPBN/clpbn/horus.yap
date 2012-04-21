
/*******************************************************

 Interface with C++
 
********************************************************/

:- module(clpbn_horus,
          [set_solver/1,
           create_lifted_network/3,
           create_ground_network/4,
           set_parfactors_params/2,
           set_factors_params/2,
           run_lifted_solver/3,
           run_ground_solver/3,
           set_vars_information/2,
           set_horus_flag/2,
           free_parfactors/1,
           free_ground_network/1
          ]).


:- use_module(library(pfl),
          [set_pfl_flag/2]).


patch_things_up :-
  assert_static(clpbn_horus:set_horus_flag(_,_)).


warning :-
  format(user_error,"Horus library not installed: cannot use bp, fove~n.",[]).


:- catch(load_foreign_files([horus], [], init_predicates), _, patch_things_up)
    -> true ; warning.


set_solver(ve)    :- set_pfl_flag(solver,ve).
set_solver(jt)    :- set_pfl_flag(solver,jt).
set_solver(gibbs) :- set_pfl_flag(solver,gibbs).
set_solver(fove)  :- set_pfl_flag(solver,fove).
set_solver(hve)   :- set_pfl_flag(solver,bp), set_horus_flag(inf_alg, ve).
set_solver(bp)    :- set_pfl_flag(solver,bp), set_horus_flag(inf_alg, bp).
set_solver(cbp)   :- set_pfl_flag(solver,bp), set_horus_flag(inf_alg, cbp).
set_solver(S)     :- throw(error('unknow solver ', S)).


%:- set_horus_flag(inf_alg, ve).
%:- set_horus_flag(inf_alg, bp).
%: -set_horus_flag(inf_alg, cbp).

:- set_horus_flag(schedule, seq_fixed).
%:- set_horus_flag(schedule, seq_random).
%:- set_horus_flag(schedule, parallel).
%:- set_horus_flag(schedule, max_residual).

:- set_horus_flag(accuracy, 0.0001).

:- set_horus_flag(max_iter, 1000).

:- set_horus_flag(order_vars, false).
%:- set_horus_flag(order_vars, true).

:- set_horus_flag(use_logarithms, false).
% :- set_horus_flag(use_logarithms, true).

