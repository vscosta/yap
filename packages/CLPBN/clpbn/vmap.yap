
:- module(clpbn_vmap,
		[init_vmap/1, % init_vmap(-Vmap)
		 add_to_vmap/4, % add_to_vmap(+V,-I,+VMap0,VMapF)
		 get_from_vmap/3, % add_to_vmap(+V,-I,+VMap0)
		 vars_to_numbers/4, % vars_to_numbers(+Vs,-Is,+VMap0,VMapF)
		 lvars_to_numbers/4, % lvars_to_numbers(+LVs,-LIs,+VMap0,VMapF)
		 vmap_to_list/2
		]).

:- use_module(library(rbtrees)).
:- use_module(library(maplist)).

%
% vmap: map V->I
% contiguous Vs to contiguous integers
%
init_vmap(vmap(0,Empty)) :-
	rb_new(Empty).

get_from_vmap(V, I, VMap0) :-
	VMap0 = vmap(_I,Map0),
	rb_lookup(V, I, Map0).

add_to_vmap(V, I, VMap0, VMap0) :-
	VMap0 = vmap(_I,Map0),
	rb_lookup(V, I, Map0), !.
add_to_vmap(V, I0, vmap(I0,Map0), vmap(I, Map)) :-
	I is I0+1,
	rb_insert(Map0, V, I0, Map).

vars_to_numbers(Vs, Is, VMap0, VMap) :-
	foldl(add_to_vmap, Vs, Is, VMap0, VMap).

lvars_to_numbers(LVs, LIs, VMap0, VMap) :-
	foldl(vars_to_numbers, LVs, LIs, VMap0, VMap).

vmap_to_list(vmap(_,Map), L) :-
	rb_visit(Map, L).

