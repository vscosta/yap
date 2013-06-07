:- module(mln,
		[op(1150,fx,mln),
		 op(1150,fx,mln_domain),
		 mln_domain/1]).

:- use_module(library(pfl)).
:- use_module(library(maplist)).
:- use_module(library(lists)).

:- dynamic mln/1, mln/2, mln_domain/4.

user:term_expansion(mln_domain(P),[]) :-
	expand_domain(P).

user:term_expansion( mln(W: D), pfl:factor(markov,Id,FList,FV,Phi,Constraints)) :-
	translate_to_factor(W, D, FList, Id, FV, Phi, Constraints),
	writeln(factor(markov,Id,FList,FV,Phi,Constraints)).

expand_domain((P1,P2)) :- !,
	expand_domain(P1),
	expand_domain(P2).
expand_domain(P) :-
	P =.. [Name|Types],
	functor(P, Name, Arity),
	functor(NP, Name, Arity),
	foldl(do_type(NP), Types, 1, _).

do_type(NP, Type, I0, I) :-
	I is I0+1,
	arg(I0, NP, A),
	TypeG =.. [Type, A],
	assert(mln_domain(I0, NP, TypeG, A)).

translate_to_factor(W, D, Lits, Id, Vs, Phi, Domain) :-
	W0 is exp(W),
	(
	    disj_to_list(D, LP, [], Lits, [])
	->
	    findall(F, weight(LP, W0, 1, F), Phi)
	;
	    conj_to_list(D, LP, [], Lits, [])
	->
            findall(F, weight(LP, 1, W0, F), Phi)
	;
	    cnf(D, Fs0, []),
	    clean_cnf(Fs0, Fs)
	->
	    member( LP, Fs), 
            findall(F, weight(LP, W0, 1, F), Phi),
	    maplist(remove_not, LP, Lits)
	),
	new_mln(Id),
	maplist(new_skolem(Id), Lits),
	term_variables(Lits, Vs),
	create_domain(Lits, Domain).


new_skolem(Id, Lit) :-
	pfl:new_skolem(Lit, [t,f]),
	assert(pfl:skolem_in(Lit, Id)).

mln(0).

new_mln(Id) :-
	retract(mln(Id)),
	Id1 is Id+1,
	assert(mln(Id1)).

%
% make it easier to manipulate
%
disj_to_list((C1;C2), L1, L10, L, L0) :-
	!,
	disj_to_list2(C1, L1, L1I, L, LI),
	disj_to_list2(C2, L1I, L10, LI, L0).
disj_to_list((C1+C2), L1, L10, L, L0) :-
	!,
	disj_to_list2(C1, L1, L1I, L, LI),
	disj_to_list2(C2, L1I, L10, LI, L0).

disj_to_list2((C1;C2), L1, L10, L, L0) :-
	!,
	disj_to_list2(C1, L1, L1I, L, LI),
	disj_to_list2(C2, L1I, L10, LI, L0).
disj_to_list2((C1+C2), L1, L10, L, L0) :-
	!,
	disj_to_list2(C1, L1, L1I, L, LI),
	disj_to_list2(C2, L1I, L10, LI, L0).
disj_to_list2((_C1,_C2), _L1, _L10, _L, _L0) :- !, fail.
disj_to_list2((_C1*_C2), _L1, _L10, _L, _L0) :- !, fail.
disj_to_list2((\+ C), [(-C)|L1], L1, [C|L], L) :- literal(C), !.
disj_to_list2((- C), [(-C)|L1], L1, [C|L], L) :- literal(C), !.
disj_to_list2(C, [C|L1], L1, [C|L], L).

conj_to_list((C1,C2), L1, L10, L, L0) :-
	!,
	conj_to_list2(C1, L1, L1I, L, LI),
	conj_to_list2(C2, L1I, L10, LI, L0).
conj_to_list((C1*C2), L1, L10, L, L0) :-
	!,
	conj_to_list2(C1, L1, L1I, L, LI),
	conj_to_list2(C2, L1I, L10, LI, L0).

conj_to_list2((_C1;_C2), _L1, _L10, _L, _L0) :- !, fail.
conj_to_list2((_C1+_C2), _L1, _L10, _L, _L0) :- !, fail.
conj_to_list2((C1,C2), L1, L10, L, L0) :-
	!,
	conj_to_list2(C1, L1, L1I, L, LI),
	conj_to_list2(C2, L1I, L10, LI, L0).
conj_to_list2((C1*C2), L1, L10, L, L0) :-
	!,
	conj_to_list2(C1, L1, L1I, L, LI),
	conj_to_list2(C2, L1I, L10, LI, L0).
conj_to_list2((\+ C), [(C)|L1], L1, [C|L], L) :- literal(C), !.
conj_to_list2((- C), [(C)|L1], L1, [C|L], L) :- literal(C), !.
conj_to_list2(C, [-C|L1], L1, [C|L], L).

remove_not(-G, G) :- !.
remove_not( G, G).

%
% compute the weight table, assuming it is a disjunction
%
weight([(- _)], W0, W1, P) :- !,
	% true case              false case
	( P = W1 ;  P = W0 ).
weight([_], W0, W1, P) :- !,
	% true case              false case
	( P = W0  ;  P = W1 ).
weight([(- _)|R], W0, W1, P) :- !,
	% true case              false case
	( weight(R, W0, W1, P) ;  weight(R, W0, W1, _), P = W0 ).
weight([_|R], W0, W1, P) :-
	% true case              false case
	( weight(R, W0, W1, _), P = W0 ;  weight(R, W0, W1, P) ).

create_domain(Lits, Domain) :-
	foldl(create_goals, Lits, RDomain, []),
	sort(RDomain, Domain).

create_goals(Lit) -->
	{ functor(Lit, _, Arity) },
	create_dgoal(0, Arity, Lit).

create_dgoal(Arity, Arity, _Lit) --> !.
create_dgoal(I0, Arity, Lit) -->
	{ I is I0+1, arg(I, Lit, A), mln_domain(I, Lit, TypeG, A) },
        [ TypeG ],
	create_dgoal(I, Arity, Lit).


cnf(V) --> { var(V) }, !,
	[[V]].
cnf((A->B)) --> !,
	cnf(-A+B).
cnf((A*B)) --> !,
	cnf(A),
	cnf(B).
cnf((A,B)) --> !,
	cnf(A),
	cnf(B).
cnf((-A)) --> !,
	{ cnf(A, B, []) },
	neg(B).
cnf((\+ A)) --> !,
	{ cnf(A, B, []) },
	neg(B).
cnf(A+B, Lf, L0) :- !,
	cnf(A, LA, []),
	cnf(B, LB, []),
	foldl(or(LB), LA, Lf, L0).
cnf((A;B), Lf, L0) :- !,
	cnf(A, LA, []),
	cnf(B, LB, []),
	foldl(or(LB), LA, Lf, L0).
cnf((A==B)) --> !,
	cnf((A+ -B)),
	cnf((B+ -A)).
cnf(xor(A,B)) --> !,
	cnf(A+B),
	cnf(-B + -A).
cnf(A) -->
	[[A]].

or(LB, Disj, Lf, L0) :-
	foldl( add(Disj), LB, Lf, L0).

add( Disj, El) -->
	{ append(Disj, El, UnSort),
	  sort(UnSort, Els) },
	[Els].

neg(Els) -->
	{ maplist( neg, Els, Conjs) },
	orl(Conjs).

neg(El, Conj) :-
	maplist(neg2, El, Conj).

neg2(-X, [X]) :- !.
neg2(X, [-X]).

orl([C1,C2|C]) -->
	{ foldl(or(C2), C1, C3, []) },
	orl([C3|C]).
orl([Cs]) -->
	Cs.

clean_cnf(CNF, NCNF) :-
	maplist(sort, CNF, CNF1),
	sort(CNF1, NCNF).

