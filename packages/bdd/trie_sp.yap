
:- module(trie_sp, [trie_to_bdd/3,
	tabled_trie_to_bdd/3]).

:- use_module((bdd)).
:- use_module(library(tries)).
:- use_module(library(rbtrees)).

trie_to_bdd(Trie, BDD, MapList) :-
	%trie_print(Trie),
	trie_to_list(Trie, Complex),
	%(numbervars(Complex,1,_), writeln(Complex), fail ; true ),
	rb_new(Map0),
	complex_to_andor(Complex,Map0,Map,Tree),
	%numbervars(Tree,1,_), writeln(Tree), fail ; true ),
	rb_visit(Map, MapList),
	extract_vars(MapList, Vs),
	bdd_new(Tree, Vs, BDD). %writeln(BDD).

tabled_trie_to_bdd(Trie, BDD, MapList) :-
	trie_to_list(Trie, Complex),
	rb_new(Map0),
	rb_new(Tab0),
	Complex = [list(Els)],
	tabled_complex_to_andor(Els,Map0,Map,Tab0,_Tab,Tree),
	rb_visit(Map, MapList),
	extract_vars(MapList, Vs),
	bdd_new(Tree, Vs, BDD),
	bdd_tree(BDD, bdd(_, L, _)), length(L,Len), writeln(Len).

extract_vars([], []).
extract_vars((_-V).MapList, V.Vs) :-
	extract_vars(MapList, Vs).

complex_to_andor(empty, Map, Map, 0).
complex_to_andor([list(Els)], Map0, MapF, Tree) :- !,
	complex_to_andor(Els, Map0, MapF, Tree).
complex_to_andor([endlist|_], Map, Map, 1) :- !.
complex_to_andor([El1,El2|Els], Map0, MapF, or(T1,T2)) :- !,
	complex_to_and(El1, Map0, MapI, T1),
	complex_to_andor([El2|Els], MapI, MapF, T2).
complex_to_andor([Els], Map0, MapF, V) :-
	complex_to_and(Els, Map0, MapF, V).

complex_to_and(int(A1,[endlist]), Map0, MapF, V) :- !,
	check(Map0, A1, V, MapF).
complex_to_and(atom(true,[endlist]), Map, Map, 1) :- !.
complex_to_and(atom(A1,[endlist]), Map0, MapF, V) :- !,
	check(Map0, A1, V, MapF).
complex_to_and(functor(not,1,[int(A1,[endlist])]), Map0, MapF, not(V)) :- !,
	check(Map0, A1, V, MapF).
complex_to_and(functor(not,1,[atom(A1,[endlist])]), Map0, MapF, not(V)) :- !,
	check(Map0, A1, V, MapF).
complex_to_and(int(A1,Els), Map0, MapF, and(V,T2)) :-  !,
	check(Map0, A1, V, MapI),
	complex_to_andor(Els, MapI, MapF, T2).
complex_to_and(atom(A1,Els), Map0, MapF, and(V,T2)) :-  !,
	check(Map0, A1, V, MapI),
	complex_to_andor(Els, MapI, MapF, T2).
complex_to_and(functor(not,1,[int(A1,Els)]), Map0, MapF, and(not(V),T2)) :- !,
	check(Map0, A1, V, MapI),
	complex_to_andor(Els, MapI, MapF, T2).
complex_to_and(functor(not,1,[atom(A1,Els)]), Map0, MapF, and(not(V),T2)) :- !,
	check(Map0, A1, V, MapI),
	complex_to_andor(Els, MapI, MapF, T2).
% HASH TABLE, it can be an OR or an AND.
complex_to_and(functor(not,1,[int(A1,Els)|More]), Map0, MapF, or(NOTV1,O2)) :-
	check(Map0, A1, V, MapI),
	(Els == [endlist]
	->
	  NOTV1 = not(V),
	  MapI = MapI2
	;
	  complex_to_andor(Els, MapI, MapI2, T2),
	  NOTV1 = and(not(V), T2)
	),
	complex_to_and(functor(not,1,More), MapI2, MapF, O2).
complex_to_and(functor(not,1,[atom(A1,Els)|More]), Map0, MapF, or(NOTV1,O2)) :-
	check(Map0, A1, V, MapI),
	(Els == [endlist]
	->
	  NOTV1 = not(V),
	  MapI = MapI2
	;
	  complex_to_andor(Els, MapI, MapI2, T2),
	  NOTV1 = and(not(V), T2)
	),
	complex_to_and(functor(not,1,More), MapI2, MapF, O2).

tabled_complex_to_andor(empty, Map, Map, Tab, Tab, 0).
tabled_complex_to_andor(T, Map, Map, Tab, Tab, V) :-
	rb_lookup(T, V, Tab), !,
	increment_ref_count(V).
tabled_complex_to_andor(IN, Map, Map, Tab, Tab, 1) :-
	IN =  !.
tabled_complex_to_andor([Els], Map0, MapF, Tab0, TabF, V) :-
	tabled_complex_to_and(Els, Map0, MapF, Tab0, TabF, V).
tabled_complex_to_andor([El1,El2|Els], Map0, MapF, Tab0, TabF, or(T1,T2)) :-
	tabled_complex_to_and(El1, Map0, MapI, Tab0, TabI, T1),
	tabled_complex_to_andor([El2|Els], MapI, MapF, TabI, TabF, T2).

tabled_complex_to_and(int(A1,[endlist]), Map0, MapF, Tab, Tab, V) :- !,
	check(Map0, A1, V, MapF).
tabled_complex_to_and(atom(A1,[endlist]), Map0, MapF, Tab, Tab, V) :- !,
	check(Map0, A1, V, MapF).
tabled_complex_to_and(functor(not,1,[int(A1,[endlist])]), Map0, MapF, Tab, Tab, not(V)) :- !,
	check(Map0, A1, V, MapF).
tabled_complex_to_and(functor(not,1,[atom(A1,[endlist])]), Map0, MapF, Tab, Tab, not(V)) :- !,
	check(Map0, A1, V, MapF).
tabled_complex_to_and(T, Map, Map, Tab, Tab, V) :-
	rb_lookup(T, V, Tab), !,
	increment_ref_count(V).
tabled_complex_to_and(IN, Map0, MapF, Tab0, TabF, OUT) :-
	IN = int(A1,Els), !,
	check(Map0, A1, V, MapI),
	rb_insert(Tab0, IN, OUT, TabI),
	OUT = and(0, _, V, T2),
	tabled_complex_to_andor(Els, MapI, MapF, TabI, TabF, T2).
tabled_complex_to_and(IN, Map0, MapF, Tab0, TabF, OUT) :-
	IN = atom(A1,Els), !,
	check(Map0, A1, V, MapI),
	rb_insert(Tab0, IN, OUT, TabI),
	OUT = and(0, _, V, T2),
	tabled_complex_to_andor(Els, MapI, MapF, TabI, TabF, T2).
tabled_complex_to_and(IN, Map0, MapF, Tab0, TabF, OUT) :-
	IN = functor(not,1,[int(A1,Els)]), !,
	check(Map0, A1, V, MapI),
	rb_insert(Tab0, IN, OUT, TabI),
	OUT = and(0, _, not(V), T2),
	tabled_complex_to_andor(Els, MapI, MapF, TabI, TabF, T2).
tabled_complex_to_and(IN, Map0, MapF, Tab0, TabF, OUT) :-
	IN = functor(not,1,[atom(A1,Els)]), !,
	check(Map0, A1, V, MapI),
	rb_insert(Tab0, IN, OUT, TabI),
	OUT = and(0, _, not(V), T2),
	tabled_complex_to_andor(Els, MapI, MapF, TabI, TabF, T2).

check(M0, K, V, M) :- rb_lookup(K, V, M0), !, M = M0.
check(M0, K, V, M) :- rb_insert(M0, K, V, M).

increment_ref_count(V)  :-
	arg(1,V,I0),
	I is I0+1,
	setarg(1,V,I).
