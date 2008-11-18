/*
 Deterministcally table a predicate of the form
  K -> RV

  where the K are the first N-1 arguments.

  Note that this does not include support for backtracking
*/

:- module(clpbn_table,
	[clpbn_table/1,
	 clpbn_tabled_clause/2,
	 clpbn_tabled_abolish/1,
	 clpbn_tabled_asserta/1,
	 clpbn_tabled_assertz/1,
	 clpbn_tabled_asserta/2,
	 clpbn_tabled_assertz/2,
	 clpbn_tabled_dynamic/1,
	 clpbn_tabled_number_of_clauses/2,
	 clpbn_reset_tables/0,
	 clpbn_reset_tables/1,
	 clpbn_is_tabled/1
	]).

:- use_module(library(bhash),
	[b_hash_new/2,
	 b_hash_lookup/3,
	 b_hash_insert/3]).

:- meta_predicate clpbn_table(:),
	clpbn_tabled_clause(:.?),
	clpbn_tabled_abolish(:),
	clpbn_tabled_asserta(:), 
	clpbn_tabled_assertz(:), 
	clpbn_tabled_asserta(:,-), 
	clpbn_tabled_assertz(:,-), 
	clpbn_tabled_number_of_clauses(:,-), 
	clpbn_is_tabled(:).

:- dynamic clpbn_table/3.

:- initialization(init).

init :-
	clpbn_reset_tables.

clpbn_reset_tables :-
	clpbn_reset_tables(1024).

clpbn_reset_tables(Sz) :-
	b_hash_new(Tab, Sz),
	nb_setval(clpbn_tables, Tab).

clpbn_table(M:X) :- !,
	clpbn_table(X,M).
clpbn_table(X) :-
	prolog_load_context(module, M),
	clpbn_table(X,M).

clpbn_table(M:X,_) :- !,
	clpbn_table(X,M).
clpbn_table((P1,P2),M) :- !,
	clpbn_table(P1,M),
	clpbn_table(P2,M).
clpbn_table(F/N,M) :-
	functor(S,F,N),
	S =.. L0,
	take_tail(L0, V, L1, V1, L2),
	Key =.. L1,
	atom_concat(F, '___tabled', NF),
	L2 = [_|Args],
	S1 =.. [NF|Args],
	L0 = [_|OArgs],
	S2 =.. [NF|OArgs],
	asserta(clpbn_table(S, M, S2)),
	assert((M:S :- nb_getval(clpbn_tables, Tab), ( b_hash_lookup(Key, V1, Tab) -> true ; M:S1, b_hash_insert(Tab, Key, V1) ; true), ( nonvar(V) -> clpbn_evidence:put_evidence(V, V1)  ; V = V1 ), ! ) ).

take_tail([V], V, [], V1, [V1]) :- !.
take_tail([A|L0], V, [A|L1], V1, [A|L2]) :-
	take_tail(L0, V, L1, V1, L2).

user:term_expansion((P :- Gs), NC) :-
	clpbn_table(P, M, NP),
	prolog_load_context(module, M), !,
	assert(M:(NP :- Gs)),
	NC = (:-true).

in_table(K, V) :-
	nb_getval(clpbn_tables, Tab),
	b_hash_lookup(K, V, Tab).

store_in_table(K, V) :-
	nb_getval(clpbn_tables, Tab),
	b_hash_insert(Tab, K, V).

clpbn_tabled_clause(M:Head, Body) :- !,
	clpbn_tabled_clause(Head, M, Body).
clpbn_tabled_clause(Head, Body) :-
	prolog_load_context(module, M),
	clpbn_tabled_clause(Head, M, Body).

clpbn_tabled_clause(M:Head, _, Body) :- !,
	clpbn_table(Head, M, Body).
clpbn_tabled_clause(Head, M, Body) :-
	clpbn_table(Head, M, THead),
	clause(THead, Body).


clpbn_tabled_assertz(M:Clause) :- !,
	clpbn_tabled_assertz2(Clause, M).
clpbn_tabled_assertz(Clause) :-
	prolog_load_context(module, M),
	clpbn_tabled_assertz2(Clause, M).

clpbn_tabled_assertz2(M:Clause, _) :- !,
	clpbn_tabled_assertz2(Clause, M).
clpbn_tabled_assertz2((Head:-Body), M) :- !,
	clpbn_table(Head, M, THead),
	assertz(M:(THead :- Body)).
clpbn_tabled_assertz2(Head, M) :-
	clpbn_table(Head, M, THead),
	assertz(THead).

clpbn_tabled_assertz(M:Clause, Ref) :- !,
	clpbn_tabled_assertz2(Clause, M, Ref).
clpbn_tabled_assertz(Clause, Ref) :-
	prolog_load_context(module, M),
	clpbn_tabled_assertz2(Clause, M, Ref).

clpbn_tabled_assertz2(M:Clause, _, Ref) :- !,
	clpbn_tabled_assertz2(Clause, M, Ref).
clpbn_tabled_assertz2((Head:-Body), M, Ref) :- !,
	clpbn_table(Head, M, THead),
	assertz(M:(THead :- Body), Ref).
clpbn_tabled_assertz2(Head, M, Ref) :-
	clpbn_table(Head, M, THead, Ref),
	assertz(THead).


clpbn_tabled_asserta(M:Clause) :- !,
	clpbn_tabled_asserta2(Clause, M).
clpbn_tabled_asserta(Clause) :-
	prolog_load_context(module, M),
	clpbn_tabled_asserta2(Clause, M).

clpbn_tabled_asserta2(M:Clause, _) :- !,
	clpbn_tabled_asserta2(Clause, M).
clpbn_tabled_asserta2((Head:-Body), M) :- !,
	clpbn_table(Head, M, THead),
	asserta(M:(THead :- Body)).
clpbn_tabled_asserta2(Head, M) :-
	clpbn_table(Head, M, THead),
	asserta(THead).

clpbn_tabled_asserta(M:Clause, Ref) :- !,
	clpbn_tabled_asserta2(Clause, M, Ref).
clpbn_tabled_asserta(Clause, Ref) :-
	prolog_load_context(module, M),
	clpbn_tabled_asserta2(Clause, M, Ref).

clpbn_tabled_asserta2(M:Clause, _, Ref) :- !,
	clpbn_tabled_asserta2(Clause, M, Ref).
clpbn_tabled_asserta2((Head:-Body), M, Ref) :- !,
	clpbn_table(Head, M, THead),
	asserta(M:(THead :- Body), Ref).
clpbn_tabled_asserta2(Head, M, Ref) :-
	clpbn_table(Head, M, THead, Ref),
	asserta(THead).


clpbn_tabled_abolish(M:Clause) :- !,
	clpbn_tabled_abolish(Clause, M).
clpbn_tabled_abolish(Clause) :-
	prolog_load_context(module, M),
	clpbn_tabled_abolish(Clause, M).

clpbn_tabled_abolish(M:Clause, _) :- !,
	clpbn_tabled_abolish(Clause, M).
clpbn_tabled_abolish(N/A, M) :-
	functor(Head, N, A),
	clpbn_table(Head, M, THead),
	functor(THead, TN, A),
	abolish(M:TN/A).

clpbn_tabled_dynamic(M:Clause) :- !,
	clpbn_tabled_dynamic(Clause, M).
clpbn_tabled_dynamic(Clause) :-
	prolog_load_context(module, M),
	clpbn_tabled_dynamic(Clause, M).

clpbn_tabled_dynamic(M:Clause, _) :- !,
	clpbn_tabled_dynamic(Clause, M).
clpbn_tabled_dynamic(N/A, M) :-
	functor(Head, N, A),
	clpbn_table(Head, M, THead),
	functor(THead, TN, A),
	dynamic(M:TN/A).

clpbn_tabled_number_of_clauses(M:Clause, N) :- !,
	clpbn_tabled_number_of_clauses(Clause, M, N).
clpbn_tabled_number_of_clauses(Clause, N) :-
	prolog_load_context(module, M),
	clpbn_tabled_number_of_clauses(Clause, M, N).

clpbn_tabled_number_of_clauses(M:Clause, _, N) :- !,
	clpbn_tabled_number_of_clauses(Clause, M, N).
clpbn_tabled_number_of_clauses(Head, M, N) :-
	clpbn_table(Head, M, THead),
	predicate_property(M:THead,number_of_clauses(N)).


clpbn_is_tabled(M:Clause) :- !,
	clpbn_is_tabled(Clause, M).
clpbn_is_tabled(Clause) :-
	prolog_load_context(module, M),
	clpbn_is_tabled(Clause, M).

clpbn_is_tabled(M:Clause, _) :- !,
	clpbn_is_tabled(Clause, M).
clpbn_is_tabled(Head, M) :-
	clpbn_table(Head, M, _).


