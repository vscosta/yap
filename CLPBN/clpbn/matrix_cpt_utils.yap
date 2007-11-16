:- module(clpbn_matrix_utils, [init_CPT/2,
			       project_from_CPT/3,
			       reorder_CPT/5,
			       get_dist_size/2,
			       normalise_CPT/2,
			       multiply_CPTs/3,
			       list_from_CPT/2]).

:- use_module(dists, [get_dist_domain_size/2,
		      get_dist_domain/2]).

:- use_module(library(matrix), [matrix_new/4,
				matrix_select/4,
				matrix_dims/2,
				matrix_shuffle/3,
				matrix_expand/3,
				matrix_op/4,
				matrix_dims/2,
				matrix_sum/2,
				matrix_sum_out/3,
				matrix_op_to_all/4,
				matrix_to_list/2]).

:- use_module(library(lists), [nth0/3]).

init_CPT(List, Sizes, TAB) :-
	matrix_new(floats, Sizes, List, TAB).

project_from_CPT(V,tab(Table,Deps,_),tab(NewTable,NDeps,NSzs)) :-
	evidence(V,Pos), !,
	vnth(Deps, 0, V, N, NDeps),
	matrix_select(Table, N, Pos, NewTable),
	matrix_dims(NewTable, NSzs).
project_from_CPT(V,tab(Table,Deps,_),tab(NewTable,NDeps,NSzs)) :-
	vnth(Deps, 0, V, N, NDeps),
	matrix_sum_out(Table, N, NewTable),
	matrix_dims(NewTable, NSzs).

evidence(V, Pos) :-
	clpbn:get_atts(V, [evidence(Ev),dist(Id,_)]),
	get_dist_domain(Id, Dom),
	nth0(Pos, Dom, Ev).

vnth([V1|Deps], N, V, N, Deps) :-
	V == V1, !.	
vnth([V1|Deps], N0, V, N, [V1|NDeps]) :-
	N1 is N0+1,
	vnth(Deps, N1, V, N, NDeps).

reorder_CPT(Vs0,T0,Vs,TF,Sizes) :-
	var(Vs), !,
	order_vec(Vs0,Vs,Map),
	matrix_to_list(T0,_),
	matrix_shuffle(T0,Map,TF),
	matrix_dims(TF, Sizes).
reorder_CPT(Vs0,T0,Vs,TF,Sizes) :-
	mapping(Vs0,Vs,Map),
	matrix_shuffle(T0,Map,TF),
	matrix_dims(TF, Sizes).

order_vec(Vs0,Vs,Map) :-
	add_indices(Vs0,0,Is),
	keysort(Is,NIs),
	get_els(NIs, Vs, Map).

add_indices([],_,[]).
add_indices([V|Vs0],I0,[V-I0|Is]) :-
	I is I0+1,
	add_indices(Vs0,I,Is).

get_els([], [], []).
get_els([V-I|NIs], [V|Vs], [I|Map]) :-
	get_els(NIs, Vs, Map).
	
mapping(Vs0,Vs,Map) :-
	add_indices(Vs0,0,I1s),
	add_indices( Vs,I2s),
	keysort(I1s,Ks),
	keysort(I2s,Ks),
	split_map(I2s, Map).

add_indices([],[]).
add_indices([V|Vs0],[V-_|I1s]) :-
	add_indices(Vs0,I1s).

split_map([], []).
split_map([_-M|Is], [M|Map]) :-
	split_map(Is, Map).

multiply_CPTs(tab(Tab1, Deps1, Sz1), tab(Tab2, Deps2, Sz2), tab(OT, NDeps, NSz)) :-
	expand_tabs(Deps1, Sz1, Deps2, Sz2, Map1, Map2, NDeps),
	matrix_expand(Tab1, Map1, NTab1),
	matrix_expand(Tab2, Map2, NTab2),
	matrix_op(NTab1,NTab2,*,OT),
	matrix_dims(OT,NSz).

expand_tabs([], [], [], [], [], [], []).
expand_tabs([V1|Deps1], [S1|Sz1], [], [], [0|Map1], [S1|Map2], [V1|NDeps]) :-
	expand_tabs(Deps1, Sz1, [], [], Map1, Map2, NDeps).
expand_tabs([], [], [V2|Deps2], [S2|Sz2], [S2|Map1], [0|Map2], [V2|NDeps]) :-
	expand_tabs([], [], Deps2, Sz2, Map1, Map2, NDeps).
expand_tabs([V1|Deps1], [S1|Sz1], [V2|Deps2], [S2|Sz2], Map1, Map2, NDeps) :-
	compare(C,V1,V2),
	(C == = ->
	 NDeps = [V1|MDeps],
	 Map1 = [0|M1],
	 Map2 = [0|M2],
	 NDeps = [V1|MDeps],
	 expand_tabs(Deps1, Sz1, Deps2, Sz2, M1, M2, MDeps)
	;
	 C == < ->
	 NDeps = [V1|MDeps],
	 Map1 = [0|M1],
	 Map2 = [S1|M2],
	 NDeps = [V1|MDeps],
	 expand_tabs(Deps1, Sz1, [V2|Deps2], [S2|Sz2], M1, M2, MDeps)
	;
	 NDeps = [V2|MDeps],
	 Map1 = [S2|M1],
	 Map2 = [0|M2],
	 NDeps = [V2|MDeps],
	 expand_tabs([V1|Deps1], [S1|Sz1], Deps2, Sz2, M1, M2, MDeps)
	).
	
normalise_CPT(MAT,NMAT) :-
	matrix_sum(MAT, Sum),
	matrix_op_to_all(MAT,/,Sum,NMAT).

list_from_CPT(MAT, List) :-
	matrix_to_list(MAT, List).


