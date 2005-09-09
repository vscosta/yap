:- module(clpbn_utils, [
	clpbn_not_var_member/2,
	clpbn_var_member/2,
	check_for_hidden_vars/3,
	sort_vars_by_key/3,
	sort_vars_by_key_and_parents/4]).

%
% It may happen that variables from a previous query may still be around.
% and be used in the next evaluation, so we cannot trust the list of *new*
% variables.
%
check_for_hidden_vars([], _, []).
check_for_hidden_vars([V|Vs], AllVs0, [V|NVs]) :-
	check_for_extra_variables(V,AllVs0, AllVs, Vs, IVs),
	check_for_hidden_vars(IVs, AllVs, NVs).

check_for_extra_variables(V,AllVs0, AllVs, Vs, IVs) :-
	var(V),
	clpbn:get_atts(V, [dist(_,_,[V1|LV])]), !,
	add_old_variables([V1|LV], AllVs0, AllVs, Vs, IVs).
check_for_extra_variables(_,AllVs, AllVs, Vs, Vs).

add_old_variables([], AllVs, AllVs, Vs, Vs).
add_old_variables([V1|LV], AllVs0, AllVs, Vs, IVs) :-
	clpbn_not_var_member(AllVs0, V1), !,
	add_old_variables(LV, [V1|AllVs0], AllVs, [V1|Vs], IVs).
add_old_variables([_|LV], AllVs0, AllVs, Vs, IVs) :-
	add_old_variables(LV, AllVs0, AllVs, Vs, IVs).

clpbn_var_member([V1|_], V) :- V1 == V, !.
clpbn_var_member([_|Vs], V) :-
	clpbn_var_member(Vs, V).

clpbn_not_var_member([], _).
clpbn_not_var_member([V1|Vs], V) :- V1 \== V,
	clpbn_not_var_member(Vs, V).


sort_vars_by_key(AVars, SortedAVars, Keys) :-
	get_keys(AVars, KeysVars),
	keysort(KeysVars, KVars),
	merge_same_key(KVars, SortedAVars, Keys).

get_keys([], []).
get_keys([V|AVars], [K-V|KeysVars]) :-
	clpbn:get_atts(V, [key(K)]), !,
	get_keys(AVars, KeysVars).
get_keys([_|AVars], KeysVars) :-  % may be non-CLPBN vars.
	get_keys(AVars, KeysVars).

merge_same_key([], [], []).
merge_same_key([K1-V1|KVs], [V1|Vs], [K1|Ks]) :-
	eat_same_key(KVs,K1,V1,RKVs),
	merge_same_key(RKVs, Vs, Ks).

eat_same_key([K-V|KVs],K,V,RKVs) :- !,
	eat_same_key(KVs,K,V,RKVs).
eat_same_key(KVs,_,_,KVs).


