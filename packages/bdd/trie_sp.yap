
:- module(trie_sp, [
	      trie_to_cudd/2,
	      trie_to_cudd/3,
	  trie_to_formula/4]).

:- use_module((bdd)).
:- use_module(library(tries)).
:- use_module(library(rbtrees)).
:- dynamic user :debug_problog/0.

trie_to_formula(Trie,F,Map0,Map) :-
    trie_get_first_entry(Trie, E),
    !,
    collect(E,F,Map0,Map).
trie_to_formula(_,0,Map,Map).

collect(E,or(V,Disjs),Map0,Map) :-
    trie_get_entry(E, V0),
    form(V0,V,Map0,MapI),
    !,
    more(E,Disjs,MapI,Map).
    
more(E,Disjs,MapI,Map) :-
    trie_traverse_next(E, E1),
    !,
    collect(E1,Disjs,MapI,Map).
more(_,0,Map,Map).    

form([],1,Map,Map).
form([not(A)|As],and(not(V),Vs),Map0,Map) :-
    !,
    check(Map0,A,V,MapI),
    form(As,Vs,MapI,Map).
form([A|As],and(V,Vs),Map0,Map) :-
    check(Map0,A,V,MapI),
    form(As,Vs,MapI,Map).

trie_to_cudd(Trie, BDD) :-
    trie_to_cudd(Trie, _MapList, BDD).

trie_to_cudd(Trie, MapList, BDD) :-
    rb_new(Map0),
    trie_to_formula(Trie, Formula,Map0,Map),
    rb_visit(Map, MapList),
    extract_vars(MapList, Vs),
    bdd_new(Formula, Vs, BDD),
    (user:debug_problog ->
	 numbervars(Formula, 1, _),
	 term_to_atom(Formula, Name),
	 atom_concat(Name, '.dot', F),
	 bdd_print(Formula, F)
    ;
    true
    ).

prolog_lbdd_tree(Trie, Tree) :-
    prolog_lbdd_tree(Trie, _Vs, Tree).

prolog_lbdd_tree(Trie, MapList, Tree) :-
    trie_to_cudd(Trie, MapList, BDD),
    bdd_tree(BDD, MapList,Tree),
    bdd_close(BDD).



extract_vars([], []).
extract_vars([(_-V)|MapList], [V|Vs]) :-
	extract_vars(MapList, Vs).

complex_to_andor(empty, Map, Map, 0).
complex_to_andor([list(El)], Map0, MapF, T) :-
    !,
    complex_to_andor(El, Map0, MapF, T).
complex_to_andor([list(El)|Lists], Map0, MapF, or(T1,T2)) :-
    !,
    complex_to_andor((El), Map0, MapI, T1),
    complex_to_andor(Lists, MapI, MapF, T2).
%% complex_to_andor([list(Els),Els1|Lists], Map0, MapF, or(T1,T2)) :-
%%     !,
%%     complex_to_and(Els, Map0, MapI, T1),
%%     complex_to_andor([Els1|Lists], MapI, MapF, T2).
complex_to_andor(Els, Map0, MapF, T) :-
    complex_to_and(Els, Map0, MapF, T).

complex_to_and([El], Map0, MapF, T) :-
    complex_to_and(El, Map0, MapF, T).
complex_to_and([El1|REls], Map0, MapF, and(T1,T2)) :-
    !,
    complex_to_and(El1, Map0, MapI, T1),
    complex_to_and([REls], MapI, MapF, T2).
complex_to_and(Els, Map0, MapF, T) :-
    complex_to_node(Els, Map0, MapF, T).

complex_to_node([El|Els], Map0, MapF,T2) :-  !,
	complex_to_andor([El|Els], Map0, MapF, T2).
complex_to_node(int(A1,[endlist]), Map0, MapF, V) :- !,
	check(Map0, A1, V, MapF).
complex_to_node(atom(true,[endlist]), Map, Map, 1) :- !.
complex_to_node(atom(false,[endlist]), Map, Map, 1) :- !.
complex_to_node(atom(A1,[endlist]), Map0, MapF, V) :- !,
	check(Map0, A1, V, MapF).
complex_to_node(functor(not,1,[int(A1,[endlist])]), Map0, MapF, not(V)) :- !,
    check(Map0, A1, V, MapF).
complex_to_node(functor(not,1,[atom(A1,[endlist])]), Map0, MapF, not(V)) :- !,
	check(Map0, A1, V, MapF).
complex_to_node(int(A1,Els), Map0, MapF, and(V,T2)) :-  !,
	check(Map0, A1, V, MapI),
	complex_to_andor(Els, MapI, MapF, T2).
complex_to_node(atom(A1,Els), Map0, MapF, and(V,T2)) :-  !,
	check(Map0, A1, V, MapI),
	complex_to_andor(Els, MapI, MapF, T2).
complex_to_node(functor(not,1,[int(A1,Els)]), Map0, MapF, and(not(V),T2)) :- !,
	check(Map0, A1, V, MapI),
	complex_to_andor(Els, MapI, MapF, T2).
complex_to_node(functor(not,1,[atom(A1,Els)]), Map0, MapF, and(not(V),T2)) :- !,
	check(Map0, A1, V, MapI),
	complex_to_andor(Els, MapI, MapF, T2).
% HASH TABLE, it can be an OR or an AND.
complex_to_node(functor(not,1,[int(A1,Els)|More]), Map0, MapF, or(NOTV1,O2)) :-
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
complex_to_node(functor(not,1,[atom(A1,Els)|More]), Map0, MapF, or(NOTV1,O2)) :-
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
