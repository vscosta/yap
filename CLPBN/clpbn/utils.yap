:- module(clpbn_utils, [
	clpbn_not_var_member/2,
	clpbn_var_member/2,
	check_for_hidden_vars/3]).

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

