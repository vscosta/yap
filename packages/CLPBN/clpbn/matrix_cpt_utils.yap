
:- module(clpbn_matrix_utils,
		[init_CPT/3,
		 project_from_CPT/3,
		 sum_out_from_CPT/5,
		 project_from_CPT/6,
		 reorder_CPT/5,
		 get_CPT_sizes/2,
		 normalise_CPT/2,
		 multiply_CPTs/4,
		 multiply_CPTs/6,
		 divide_CPTs/3,
		 expand_CPT/4,
		 reset_CPT_that_disagrees/5,
		 unit_CPT/2,
		 sum_out_from_CPT/4,
		 list_from_CPT/2,
		 multiply_factors/3,
		 normalise_possibly_deterministic_CPT/2,
		 column_from_possibly_deterministic_CPT/3,
		 multiply_possibly_deterministic_factors/3,
		 random_CPT/2,
		 uniform_CPT/2,
		 uniform_CPT_as_list/2,
		 normalise_CPT_on_lines/3
		]).

:- use_module(library(matrix),
		[matrix_new/4,
		 matrix_new_set/4,
		 matrix_select/4,
		 matrix_dims/2,
		 matrix_size/2,
		 matrix_shuffle/3,
		 matrix_expand/3,
		 matrix_op/4,
		 matrix_dims/2,
		 matrix_sum/2,
		 matrix_sum_logs_out/3,
		 matrix_sum_out/3,
		 matrix_sum_logs_out_several/3,
		 matrix_op_to_all/4,
		 matrix_to_exps2/1,
		 matrix_to_logs/1,
		 matrix_set_all_that_disagree/5,
		 matrix_to_list/2,
		 matrix_agg_lines/3,
		 matrix_agg_cols/3,
		 matrix_op_to_lines/4,
		 matrix_column/3
		]).

init_CPT(List, Sizes, TAB) :-
	matrix_new(floats, Sizes, List, TAB),
	matrix_to_logs(TAB).

init_possibly_deterministic_CPT(List, Sizes, TAB) :-
	matrix_new(floats, Sizes, List, TAB).

%
% select elements of matrix Table such that V=Pos
%
project_from_CPT(V, Pos, Table, Deps, NewTable, NDeps) :-
	vnth(Deps, 0, V, N, NDeps),
	matrix_select(Table, N, Pos, NewTable).

%
% sum-out varibale V from Table
%
sum_out_from_CPT(V, Table, Deps, NewTable, NDeps) :-
	vnth(Deps, 0, V, N, NDeps),
	matrix_sum_logs_out(Table, N, NewTable).

project_from_CPT(V,tab(Table,Deps,_),tab(NewTable,NDeps,NSzs)) :-
	evidence(V,Pos), !,
	vnth(Deps, 0, V, N, NDeps),
	matrix_select(Table, N, Pos, NewTable),
	matrix_dims(NewTable, NSzs).
project_from_CPT(V,tab(Table,Deps,_),tab(NewTable,NDeps,NSzs)) :-
	vnth(Deps, 0, V, N, NDeps),
%	matrix_to_exps2(Table),
	matrix_sum_logs_out(Table, N, NewTable),
%	matrix_to_logs(NewTable),
	matrix_dims(NewTable, NSzs).

evidence(V, Pos) :-
	clpbn:get_atts(V, [evidence(Pos)]).

vnth([V1|Deps], N, V, N, Deps) :-
	V == V1, !.
vnth([V1|Deps], N0, V, N, [V1|NDeps]) :-
	N1 is N0+1,
	vnth(Deps, N1, V, N, NDeps).

reorder_CPT(Vs0,T0,Vs,TF,Sizes) :-
	var(Vs), !,
	order_vec(Vs0,Vs,Map),
	(
	  Vs == Vs0
	->
	  TF = T0
	;
	  matrix_shuffle(T0,Map,TF)
	),
	matrix_dims(TF, Sizes).
reorder_CPT(Vs0,T0,Vs,TF,Sizes) :-
	mapping(Vs0,Vs,Map),
	(
	  Vs == Vs0
	->
	  TF = T0
	;
	  matrix_shuffle(T0,Map,TF)
	),
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

divide_CPTs(Tab1, Tab2, OT) :-
	matrix_op(Tab1,Tab2,-,OT).

multiply_CPTs(tab(Tab1, Deps1, Sz1), tab(Tab2, Deps2, Sz2), tab(OT, NDeps, NSz), NTab2) :-
	expand_tabs(Deps1, Sz1, Deps2, Sz2, Map1, Map2, NDeps),
	matrix_expand_compact(Tab1, Map1, NTab1),
	matrix_expand_compact(Tab2, Map2, NTab2),
	matrix_op(NTab1,NTab2,+,OT),
	matrix_dims(OT,NSz).

multiply_CPTs(Tab1, Deps1, Tab2, Deps2, OT, NDeps) :-
	matrix_dims(Tab1, Sz1),
	matrix_dims(Tab2, Sz2),
	expand_tabs(Deps1, Sz1, Deps2, Sz2, Map1, Map2, NDeps),
	matrix_expand_compact(Tab1, Map1, NTab1),
	matrix_expand_compact(Tab2, Map2, NTab2),
	matrix_op(NTab1,NTab2,+,OT).


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
	matrix_to_exps2(MAT),
	matrix_sum(MAT, Sum),
	matrix_op_to_all(MAT, /, Sum, NMAT).

list_from_CPT(MAT, List) :-
	matrix_to_list(MAT, List).

expand_CPT(MAT0, Dims0, DimsNew, MAT) :-
	generate_map(DimsNew, Dims0, Map),
	matrix_expand(MAT0, Map, MAT).

generate_map([], [], []).
generate_map([V|DimsNew], [V0|Dims0], [0|Map]) :- V == V0, !,
	generate_map(DimsNew, Dims0, Map).
generate_map([V|DimsNew], Dims0, [Sz|Map]) :-
	clpbn:get_atts(V, [dist(Id,_)]),
	clpbn_dist:get_dist_domain_size(Id, Sz),
	generate_map(DimsNew, Dims0, Map).

unit_CPT(V,CPT) :-
	clpbn:get_atts(V, [dist(Id,_)]),
	clpbn_dist:get_dist_domain_size(Id, Sz),
	matrix_new_set(floats,[Sz],1.0,CPT).

reset_CPT_that_disagrees(CPT, Vars, V, Pos, NCPT) :-
	vnth(Vars, 0, V, Dim,  _),
	matrix_set_all_that_disagree(CPT, Dim, Pos, -inf, NCPT).

sum_out_from_CPT(Vs,Table,Deps,tab(NewTable,Vs,Sz)) :-
	conversion_matrix(Vs, Deps, Conv),
	matrix_sum_logs_out_several(Table, Conv, NewTable),
	matrix_dims(NewTable, Sz).

conversion_matrix([], [], []).
conversion_matrix([], [_|Deps], [1|Conv]) :-
	conversion_matrix([], Deps, Conv).
conversion_matrix([V|Vs], [V1|Deps], [0|Conv]) :- V==V1, !,
	conversion_matrix(Vs, Deps, Conv).
conversion_matrix([V|Vs], [_|Deps], [1|Conv]) :-
	conversion_matrix([V|Vs], Deps, Conv).

get_CPT_sizes(CPT, Sizes) :-
	matrix_dims(CPT, Sizes).

matrix_expand_compact(M0,Zeros,M0) :-
	zero_map(Zeros), !.
matrix_expand_compact(M0,Map,M) :-
	matrix_expand(M0, Map, M).

zero_map([]).
zero_map([0|Zeros]) :-
	zero_map(Zeros).

col_from_CPT(CPT, Parents, Column) :-
	matrix_col(CPT, Parents, Column),
	matrix_to_logs(Column).

column_from_possibly_deterministic_CPT(CPT, Parents, Column) :-
	matrix_column(CPT, Parents, Column).

multiply_factors(F1, F2, F) :-
	matrix_op(F1,F2,+,F).

multiply_possibly_deterministic_factors(F1, F2, F) :-
	matrix_op(F1,F2,*,F).

normalise_possibly_deterministic_CPT(MAT,NMAT) :-
	matrix_agg_lines(MAT, +, Sum),
	matrix_op_to_lines(MAT, Sum, /, NMAT).

random_CPT(Dims, M) :-
	mult_all(Dims,1,Size),
	generate_random_entries(Size, Randoms),
	matrix_new(floats, Dims, Randoms, M1),
	normalise_possibly_deterministic_CPT(M1, M).

mult_all([],Size,Size).
mult_all([D|Dims],Size0,Size) :-
	Size1 is Size0*D,
	mult_all(Dims,Size1,Size).

generate_random_entries(0, []) :- !.
generate_random_entries(Size, [R|Randoms]) :-
	R is random,
	Size1 is Size-1,
	generate_random_entries(Size1, Randoms).

uniform_CPT_as_list(Dims, L) :-
	uniform_CPT(Dims, M),
	matrix_to_list(M, L).

uniform_CPT(Dims, M) :-
	matrix_new_set(floats,Dims,1.0,M1),
	normalise_possibly_deterministic_CPT(M1, M).

normalise_CPT_on_lines(MAT0, MAT2, L1) :-
	matrix_agg_cols(MAT0, +, MAT1),
	matrix_sum(MAT1, SUM),
	matrix_op_to_all(MAT1, /, SUM, MAT2),
	matrix:matrix_to_list(MAT2,L1).

