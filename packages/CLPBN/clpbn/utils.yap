
:- module(clpbn_utils,
		[clpbn_not_var_member/2,
		 clpbn_var_member/2,
		 check_for_hidden_vars/3,
		 sort_vars_by_key/3,
		 sort_vars_by_key_and_parents/3
		]).

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
	clpbn:get_atts(V, [dist(_,[V1|LV])]), !,
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


sort_vars_by_key(AVars, SortedAVars, UnifiableVars) :-
	get_keys(AVars, KeysVars),
	msort(KeysVars, KVars),
	merge_same_key(KVars, SortedAVars, [], UnifiableVars).

get_keys([], []).
get_keys([V|AVars], [K-V|KeysVars]) :-
	clpbn:get_atts(V, [key(K)]), !,
	get_keys(AVars, KeysVars).
get_keys([_|AVars], KeysVars) :-  % may be non-CLPBN vars.
	get_keys(AVars, KeysVars).

merge_same_key([], [], _, []).
merge_same_key([K1-V1,K2-V2|Vs], SortedAVars, Ks, UnifiableVars) :-
	K1 == K2, !,
	(clpbn:get_atts(V1, [evidence(E)]) ->
	  clpbn:put_atts(V2, [evidence(E)])
	;
	  clpbn:get_atts(V2, [evidence(E)]) ->
	    clpbn:put_atts(V1, [evidence(E)])
	  ;
	    true
	),
%	V1 = V2,
	attributes:fast_unify_attributed(V1,V2),
	merge_same_key([K1-V1|Vs], SortedAVars, Ks, UnifiableVars).
merge_same_key([K1-V1,K2-V2|Vs], [V1|SortedAVars], Ks, [K1|UnifiableVars]) :-
	(in_keys(K1, Ks) ; \+ \+ K1 == K2), !,
	add_to_keys(K1, Ks, NKs),
	merge_same_key([K2-V2|Vs], SortedAVars, NKs, UnifiableVars).
merge_same_key([K-V|Vs], [V|SortedAVars], Ks, UnifiableVars) :-
	add_to_keys(K, Ks, NKs),
	merge_same_key(Vs, SortedAVars, NKs, UnifiableVars).

in_keys(K1,[K|_]) :- \+ \+ K1 = K, !.
in_keys(K1,[_|Ks]) :-
	in_keys(K1,Ks).

add_to_keys(K1, Ks, Ks) :- ground(K1), !.
add_to_keys(K1, Ks, [K1|Ks]).

sort_vars_by_key_and_parents(AVars, SortedAVars, UnifiableVars) :-
	get_keys_and_parents(AVars, KeysVars),
	keysort(KeysVars, KVars),
	merge_same_key(KVars, SortedAVars, [], UnifiableVars).

get_keys_and_parents([], []).
get_keys_and_parents([V|AVars], [K-V|KeysVarsF]) :-
	clpbn:get_atts(V, [key(K),dist(Id,Parents)]), !,
	add_parents(Parents,V,Id,KeysVarsF,KeysVars0),
	get_keys_and_parents(AVars, KeysVars0).
get_keys_and_parents([_|AVars], KeysVars) :-  % may be non-CLPBN vars.
	get_keys_and_parents(AVars, KeysVars).

add_parents(Parents,_,_,KeyVars,KeyVars) :-
	all_vars(Parents), !.
add_parents(Parents,V,Id,KeyVarsF,KeyVars0) :-
	transform_parents(Parents,NParents,KeyVarsF,KeyVars0),
	clpbn:put_atts(V, [dist(Id,NParents)]).


all_vars([]).
all_vars([P|Parents]) :-
	var(P),
	all_vars(Parents).


transform_parents([],[],KeyVars,KeyVars).
transform_parents([P|Parents0],[P|NParents],KeyVarsF,KeyVars0) :-
	var(P), !,
	transform_parents(Parents0,NParents,KeyVarsF,KeyVars0).
transform_parents([P|Parents0],[V|NParents],[P-V|KeyVarsF],KeyVars0) :-
	transform_parents(Parents0,NParents,KeyVarsF,KeyVars0).

