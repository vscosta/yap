%
% Utilities for learning
%

:- module(clpbn_learn_utils, [run_all/1,
			      clpbn_vars/2,
			      normalise_counts/2,
			      compute_likelihood/3]).

:- use_module(library(matrix),
	      [matrix_agg_lines/3,
	       matrix_op_to_lines/4,
	       matrix_to_logs/2,
	       matrix_op/4,
	       matrix_sum/2]).

:- meta_predicate run_all(:).

run_all([]).
run_all([G|Gs]) :-
	call(G),
	run_all(Gs).
run_all(M:Gs) :-
	run_all(Gs,M).

run_all([],_).
run_all([G|Gs],M) :-
	call(M:G),
	run_all(Gs,M).

clpbn_vars(Vs,BVars) :-
	get_clpbn_vars(Vs,CVs),
	keysort(CVs,KVs),
	merge_vars(KVs,BVars).
	
get_clpbn_vars([],[]).
get_clpbn_vars([V|GVars],[K-V|CLPBNGVars]) :-
	clpbn:get_atts(V, [key(K)]), !,
	get_clpbn_vars(GVars,CLPBNGVars).
get_clpbn_vars([_|GVars],CLPBNGVars) :-
	get_clpbn_vars(GVars,CLPBNGVars).

merge_vars([],[]).
merge_vars([K-V|KVs],[V|BVars]) :-
	get_var_has_same_key(KVs,K,V,KVs0),
	merge_vars(KVs0,BVars).
	
get_var_has_same_key([K-V|KVs],K,V,KVs0)  :- !,
	get_var_has_same_key(KVs,K,V,KVs0).
get_var_has_same_key(KVs,_,_,KVs).

normalise_counts(MAT,NMAT) :-
	matrix_agg_lines(MAT, +, Sum),
	matrix_op_to_lines(MAT, Sum, /, NMAT).

compute_likelihood(Table0, NewTable, DeltaLik) :-
	matrix_to_logs(NewTable, Logs),
	matrix_op(Table0, Logs, *, Logs),
	matrix_sum(Logs, DeltaLik).


