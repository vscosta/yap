%
% Utilities for learning
%

:- module(clpbn_learn_utils,
		[run_all/1,
		 clpbn_vars/2,
		 normalise_counts/2,
		 compute_likelihood/3,
		 soften_sample/2,
		 soften_sample/3
		]).

:- use_module(library(clpbn),
		[clpbn_flag/2]).

:- use_module(library('clpbn/table'),
		[clpbn_reset_tables/0]).

:- use_module(library(matrix),
		[matrix_agg_lines/3,
		 matrix_op_to_lines/4,
		 matrix_agg_cols/3,
		 matrix_op_to_cols/4,
		 matrix_to_logs/2,
		 matrix_op/4,
		 matrix_sum/2,
		 matrix_to_list/2,
		 matrix_op_to_all/4]).

:- meta_predicate run_all(:).

run_all([]).
run_all([G|Gs]) :-
	run_all(user:[G:Gs]).
run_all(M:Gs) :-
	clpbn_reset_tables,
	run_all(Gs,M).

run_all([],_).
run_all([example(Gs0)|Gs],M) :-
	run_all(Gs0,M),
	run_all(Gs,M).
run_all([G|Gs],M) :-
	( call(M:G) -> true ;  throw(bad_call(M:G)) ),
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

soften_sample(T0,T) :-
	clpbn_flag(parameter_softening, Soften),
	soften_sample(Soften, T0, T).

soften_sample(no,T,T).
soften_sample(m_estimate(M), T0, T) :-
	matrix_agg_cols(T0,+,Cols),
	matrix_op_to_all(Cols, *, M, R),
	matrix_op_to_cols(T0,R,+,T).
soften_sample(auto_m, T0,T) :-
	matrix_agg_cols(T0,+,Cols),
	matrix_sum(Cols,TotM),
	M is sqrt(TotM),
	matrix_op_to_all(Cols, *, M, R),
	matrix_op_to_cols(T0,R,+,T).
soften_sample(laplace,T0,T) :-
	matrix_op_to_all(T0, +, 1, T).


normalise_counts(MAT,NMAT) :-
	matrix_agg_lines(MAT, +, Sum),
	matrix_op_to_lines(MAT, Sum, /, NMAT).

compute_likelihood(Table0, NewTable, DeltaLik) :-
	matrix_to_logs(NewTable, Logs),
	matrix_to_list(Table0,L1),
	matrix_to_list(Logs,L2),
	sum_prods(L1,L2,0,DeltaLik).

sum_prods([],[],DeltaLik,DeltaLik).
sum_prods([0.0|L1],[_|L2],DeltaLik0,DeltaLik) :- !,
	sum_prods(L1,L2,DeltaLik0,DeltaLik).
sum_prods([Count|L1],[Log|L2],DeltaLik0,DeltaLik) :- !,
	DeltaLik1 is DeltaLik0+Count*Log,
	sum_prods(L1,L2,DeltaLik1,DeltaLik).

