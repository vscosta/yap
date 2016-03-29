/*
 Deterministcally table a predicate of the form
  K -> RV

  where the K are the first N-1 arguments.

  Note that this does not include support for backtracking
*/

:- module(clpbn_table,
		[clpbn_table/1,
		 clpbn_tableallargs/1,
		 clpbn_table_nondet/1,
		 clpbn_tabled_clause/2,
		 clpbn_tabled_clause_ref/3,
		 clpbn_tabled_retract/2,
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
		[b_hash_new/4,
		 b_hash_lookup/3,
		 b_hash_insert/4
		]).

:- meta_predicate clpbn_table(:),
	clpbn_tabled_clause(:.?),
	clpbn_tabled_clause_ref(:.?,?),
	clpbn_tabled_retract(:),
	clpbn_tabled_abolish(:),
	clpbn_tabled_asserta(:),
	clpbn_tabled_assertz(:),
	clpbn_tabled_asserta(:,-),
	clpbn_tabled_assertz(:,-),
	clpbn_tabled_number_of_clauses(:,-),
	clpbn_is_tabled(:).

:- use_module(library(terms),
		[instantiated_term_hash/4,
		 variant/2
		]).

:- use_module(evidence,
		[put_evidence/2]).

:- dynamic clpbn_table/3.

:- meta_predicate clpbn_table(:), clpbn_table_all_args(:).

:- initialization(init).

init :-
	clpbn_reset_tables.

clpbn_reset_tables :-
	clpbn_reset_tables(2048).

clpbn_reset_tables(Sz) :-
	b_hash_new(Tab, Sz, myf, myc),
	nb_setval(clpbn_tables, Tab).

myf(Key, Size, Index) :-
	instantiated_term_hash(Key, -1, Size, Index).

myc(A, B) :-
	ground(A), !, B == A.
myc(A, B) :-
	variant(A,B),
	term_variables(A,L1),
	term_variables(B,L2),
	match_keys(L1,L2).

match_keys([],[]).
match_keys([V1|L1],[V2|L2]) :-
	clpbn:get_atts(V1,[key(K)]),
	clpbn:get_atts(V2,[key(K)]),
	match_keys(L1,L2).


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
	take_tail(L0, A0, L1, V2, L2),
	Key =.. L1,
	atom_concat(F, '___tabled', NF),
	L2 = [_|Args2],
	Goal =.. [NF|Args2],
	L0 = [_|Args0],
	IGoal =.. [NF|Args0],
	asserta(clpbn_table(S, M, IGoal)),
	assert((M:S :-
	  !,
	  b_getval((clpbn_tables), Tab),
	  % V2 is unbound.
	  (b_hash_lookup(Key, V2, Tab) ->
%	    (attvar(V2) -> writeln(ok:A0:V2) ; writeln(error(V2:should_be_attvar(S)))),
	    (var(A0) -> A0 = V2 ; put_evidence(A0, V2))
	  ;
%	    writeln(new),
	    b_hash_insert(Tab, Key, V2, NewTab),
	    b_setval(clpbn_tables,NewTab),
	    once(M:Goal), !,
	    % enter evidence after binding.
	    (var(A0) -> A0 = V2 ; put_evidence(A0, V2))
	  ;
	    clpbn:clpbn_flag(solver,none) ->
	      true
	    ;
	      throw(error(tabled_clpbn_predicate_should_never_fail,S))
	  )
	)).

take_tail([V], V, [], V1, [V1]) :- !.
take_tail([A|L0], V, [A|L1], V1, [A|L2]) :-
	take_tail(L0, V, L1, V1, L2).

clpbn_tableallargs(M:X) :- !,
	clpbn_tableallargs(X,M).
clpbn_tableallargs(X) :-
	prolog_load_context(module, M),
	clpbn_tableallargs(X,M).

clpbn_tableallargs(M:X,_) :- !,
	clpbn_tableallargs(X,M).
clpbn_tableallargs((P1,P2),M) :- !,
	clpbn_tableallargs(P1,M),
	clpbn_tableallargs(P2,M).
clpbn_tableallargs(F/N,M) :-
	functor(Key,F,N),
	Key =.. [F|Args],
	atom_concat(F, '___tabled', NF),
	NKey =.. [NF|Args],
	asserta(clpbn_table(Key, M, NKey)),
	assert((M:Key :-
	  !,
	  b_getval(clpbn_tables, Tab),
	  (b_hash_lookup(Key, Out, Tab) ->
	    true
	  ;
	    b_hash_insert(Tab, Key, Out, NewTab),
	    b_setval(clpbn_tables, NewTab),
	    once(M:NKey)
	  )
	)).

clpbn_table_nondet(M:X) :- !,
	clpbn_table_nondet(X,M).
clpbn_table_nondet(X) :-
	prolog_load_context(module, M),
	clpbn_table_nondet(X,M).

clpbn_table_nondet(M:X,_) :- !,
	clpbn_table_nondet(X,M).
clpbn_table_nondet((P1,P2),M) :- !,
	clpbn_table_nondet(P1,M),
	clpbn_table_nondet(P2,M).
clpbn_table_nondet(F/N,M) :-
	functor(Key,F,N),
	Key =.. [F|Args],
	atom_concat(F, '___tabled', NF),
	NKey =.. [NF|Args],
	asserta(clpbn_table(Key, M, NKey)),
	assert((M:Key :-
	  % writeln(in:Key),
	  b_getval(clpbn_tables, Tab),
	  (b_hash_lookup(Key, Out, Tab) ->
	    fail
	  ;
	    b_hash_insert(Tab, Key, Out, NewTab),
	    b_setval(clpbn_tables, NewTab),
	    M:NKey
	  )
	)).

user:term_expansion((P :- Gs), NC) :-
	clpbn_table(P, M, NP),
	prolog_load_context(module, M), !,
	assert(M:(NP :- Gs)),
	NC = (:-true).

in_table(K, V) :-
	b_getval(clpbn_tables, Tab),
	b_hash_lookup(K, V, Tab).

store_in_table(K, V) :-
	b_getval(clpbn_tables, Tab),
	b_hash_insert(Tab, K, V, NewTab),
    ( Tab == NewTab -> true ; b_setval(clpbn_tables, NewTab)).

clpbn_tabled_clause(M:Head, Body) :- !,
	clpbn_tabled_clause(Head, M, Body).
clpbn_tabled_clause(Head, Body) :-
	prolog_load_context(module, M),
	clpbn_tabled_clause(Head, M, Body).

clpbn_tabled_clause(M:Head, _, Body) :- !,
	clpbn_tabled_clause(Head, M, Body).
clpbn_tabled_clause(Head, M, Body) :-
	clpbn_table(Head, M, THead),
	clause(M:THead, Body).

clpbn_tabled_clause_ref(M:Head, Body, Ref) :- !,
	clpbn_tabled_clause_ref(Head, M, Body, Ref).
clpbn_tabled_clause_ref(Head, Body, Ref) :-
	prolog_load_context(module, M),
	clpbn_tabled_clause_ref(Head, M, Body, Ref).

clpbn_tabled_clause_ref(M:Head, _, Body, Ref) :- !,
	clpbn_tabled_clause_ref(Head, M, Body, Ref).
clpbn_tabled_clause_ref(Head, M, Body, Ref) :-
	clpbn_table(Head, M, THead),
	clause(M:THead, Body, Ref).


clpbn_tabled_retract(M:Head) :- !,
	clpbn_tabled_retract(Head, M).
clpbn_tabled_retract(Head) :-
	prolog_load_context(module, M),
	clpbn_tabled_retract(Head, M).

clpbn_tabled_retract(M:Head, _) :- !,
	clpbn_tabled_retract(Head, M).
clpbn_tabled_retract(Head, M) :-
	clpbn_table(Head, M, THead),
	retract(M:THead).


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

