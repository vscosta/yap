%
% Utilities for learning
%

:- module(bnt_learn_utils, [run_all/1,
			    clpbn_vars/2]).

run_all([]).
run_all([G|Gs]) :-
	call(user:G),
	run_all(Gs).

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



