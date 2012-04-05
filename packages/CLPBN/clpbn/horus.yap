
/*******************************************************

 Interface with C++
 
********************************************************/

:- module(clpbn_horus,
          [create_lifted_network/3,
           create_ground_network/4,
           set_parfactors_params/2,
           set_bayes_net_params/2,
           run_lifted_solver/3,
           run_ground_solver/3,
           set_extra_vars_info/2,
           set_horus_flag/2,
           free_parfactors/1,
           free_ground_network/1
          ]).


patch_things_up :-
  assert_static(clpbn_horus:set_horus_flag(_,_)).

warning :-
  format(user_error,"Horus library not installed: cannot use bp, fove~n.",[]).

:- catch(load_foreign_files([horus], [], init_predicates), _, patch_things_up) -> true ; warning.



%:- set_horus_flag(inf_alg, ve).
:- set_horus_flag(inf_alg, bn_bp).
%:- set_horus_flag(inf_alg, fg_bp).
%: -set_horus_flag(inf_alg, cbp).

:- set_horus_flag(schedule, seq_fixed).
%:- set_horus_flag(schedule, seq_random).
%:- set_horus_flag(schedule, parallel).
%:- set_horus_flag(schedule, max_residual).

:- set_horus_flag(accuracy, 0.0001).

:- set_horus_flag(max_iter, 1000).

:- set_horus_flag(order_factor_variables, false).
%:- set_horus_flag(order_factor_variables, true).


:- set_horus_flag(use_logarithms, false).
% :- set_horus_flag(use_logarithms, true).

