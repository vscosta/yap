
:- module(trie_sp, [
	      trie_to_cudd/2,
	      trie_to_cudd/3,
	      trie_to_dcf/3,
		  trie_to_bdd_tree/2,
	  trie_to_formula/4]).

:- use_module(library(bdd)).
:- use_module(library(tries)).
:- use_module(library(rbtrees)).
:- dynamic user :debug_problog/0.

trie_to_formula(Trie,F,Map0,Map) :-
    trie_to_list(Trie, E),
    !,
    collect(E,F,Map0,Map).
trie_to_formula(_,0,Map,Map).

collect(list(E),F) -->
	collect(E,F).
collect([A,B|R],or(FA,FBR)) -->
  !,
	collect(A,FA),
	collect([B|R],FBR).
collect([R],FR) -->
	collect(R,FR).
collect(int(I,[endlist]),FI) -->
  !,
	leaf(I,FI).
collect(int(I,L),and(FI,FL)) -->
  !,
	leaf(I,FI),
	collect(L,FL).
collect(atom(A,[endlist]),IA) -->
	  !,
		{trl(A,IA)}.
collect(atom(A,L),and(IA,FL)) -->
	  !,
		{trl(A,IA)},
		collect(L,FL).
collect(functor(not,A,[endlist]),not(FA)) -->
	  !,
		collect(A,FA).
collect(functor(not,A,L),and(not(FA),FL)) -->
	  !,
		collect(A,FA),
		collect(L,FL).

trl(true,1).
trl(false,0).

leaf(K, V, M0, M) :- rb_lookup(K, V, M0), !, M = M0.
leaf(K, V, M0, M) :- rb_insert(M0, K, V, M).

trie_to_dcf(E,Or,Map) :-
    rb_new(Map0),
	collectl(E,Or,Map0,Map).

collectl(E,or(V,Disjs)) -->
    {trie_get_entry(E, V0)},
    form(V0,V),
    !,
    more(E,Disjs).
    
more(E,Disjs) -->
    { trie_traverse_next(E, E1) },
    !,
    collect(E1,Disjs).
more(_,0) --> [].    

form([],1)-->[].
form([not(A)|As],and(not(V),Vs)) -->
    !,
    leaf(A,V),
    form(As,Vs).
form([A|As],and(V,Vs)) -->
    leaf(A,V),
    form(As,Vs).

trie_to_cudd(Trie, BDD) :-
    trie_to_cudd(Trie, _MapList, BDD).

trie_to_cudd(Trie, MapList, BDD) :-
    rb_new(Map0),
    trie_to_formula(Trie, Formula,Map0,Map),
    rb_visit(Map, MapList),
    bdd_new(Formula, MapList, BDD),
    (user:debug_problog ->
	 numbervars(Formula, 1, _),
	 term_to_atom(Formula, Name),
	 atom_concat(Name, '.dot', F),
	 bdd_print(BDD, F, MapList)
    ;
    true
    ).

trie_to_bdd_tree(Trie,BDD) :-
    trie_to_cudd(Trie, MapList, CUDD),
	bdd_to_tree(CUDD, MapList, BDD).

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
