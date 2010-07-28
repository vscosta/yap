% This file has been included as an YAP library by Vitor Santos Costa, 1999

% Red-Black Implementation of Association Lists.

%   Note   : the keys should be bound, the associated values need not be.

:- module(assoc, [
        empty_assoc/1,
	assoc_to_list/2,
	is_assoc/1,
	min_assoc/3,
	max_assoc/3,
	gen_assoc/3,
	get_assoc/3,
	get_assoc/5,
	get_next_assoc/4,
	get_prev_assoc/4,
	list_to_assoc/2,
	ord_list_to_assoc/2,
	map_assoc/2,
	map_assoc/3,
	put_assoc/4,
	del_assoc/4,
		  assoc_to_keys/2,
	del_min_assoc/4,
	del_max_assoc/4
    ]).

:- meta_predicate map_assoc(2, +, -), map_assoc(1, +).

:- use_module(library(rbtrees), [
	rb_empty/1,
	rb_visit/2,
	is_rbtree/1,
	rb_min/3,
	rb_max/3,
	rb_in/3,
	rb_lookup/3,
	rb_update/5,
	rb_next/4,
	rb_previous/4,
	list_to_rbtree/2,
	ord_list_to_rbtree/2,
	rb_map/2,
	rb_map/3,
	rb_keys/2,
	rb_update/4,
	rb_insert/4,
	rb_delete/4,
	rb_del_min/4,
	rb_del_max/4
    ]).

empty_assoc(t).

assoc_to_list(t, L) :- !, L = [].
assoc_to_list(T, L) :-
	rb_visit(T, L).

is_assoc(t) :- !.
is_assoc(T) :-
	is_rbtree(T).

min_assoc(T,K,V) :-
	rb_min(T,K,V).

max_assoc(T,K,V) :-
	rb_max(T,K,V).

gen_assoc(T,K,V) :-
	rb_in(K,V,T).

get_assoc(K,T,V) :-
	rb_lookup(K,V,T).

get_assoc(K,T,V,NT,NV) :-
	rb_update(T,K,V,NV,NT).

get_next_assoc(K,T,KN,VN) :-
	rb_next(T,K,KN,VN).

get_prev_assoc(K,T,KP,VP) :-
	rb_previous(T,K,KP,VP).

list_to_assoc(L, T) :-
	list_to_rbtree(L, T).

ord_list_to_assoc(L, T) :-
	ord_list_to_rbtree(L, T).

map_assoc(t, _) :- !.
map_assoc(P, T) :-
	yap_flag(typein_module, M0),
	extract_mod(P, M0, M, G),
	functor(G, Name, 1),
	rb_map(T, M:Name).

map_assoc(t, T, T) :- !.
map_assoc(P, T, NT) :-
	yap_flag(typein_module, M0),
	extract_mod(P, M0, M, G),
	functor(G, Name, 2),
	rb_map(T, M:Name, NT).


extract_mod(G,_,_) :- var(G), !, fail.
extract_mod(M:G, _, FM, FG ) :- !,
	extract_mod(G, M, FM, FG ).
extract_mod(G, M, M, G ).

put_assoc(K, T, V, NT) :-
	rb_update(T, K, V, NT), !.
put_assoc(K, t, V, NT) :- !,
	rbtrees:rb_new(K,V,NT).
put_assoc(K, T, V, NT) :-
	rb_insert(T, K, V, NT).

del_assoc(K, T, V, NT) :-
	rb_delete(T, K, V, NT).

del_min_assoc(T, K, V, NT) :-
	rb_del_min(T, K, V, NT).

del_max_assoc(T, K, V, NT) :-
	rb_del_max(T, K, V, NT).


assoc_to_keys(T, Ks) :-
	rb_keys(T, Ks).


