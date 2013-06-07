:- module(mln,
		[op(1150,fx,mln),
		 op(1150,fx,mln_domain),
		 mln_domain/1]).

:- use_module(library(pfl)).
:- use_module(library(maplist)).

:- dynamic mln/1, mln/2, mln_domain/4.

user:term_expansion(mln_domain(P),[]) :-
	P =.. [Name|Types],
	functor(P, Name, Arity),
	functor(NP, Name, Arity),
	foldl(do_type(NP), Types, 1, _).

user:term_expansion( mln(W: D), pfl:factor(markov,Id,FList,FV,Phi,Constraints)) :-
	translate_to_factor(W, D, FList, Id, FV, Phi, Constraints),
	writeln(factor(markov,Id,FList,FV,Phi,Constraints)).

do_type(NP, Type, I0, I) :-
	I is I0+1,
	arg(I0, NP, A),
	TypeG =.. [Type, A],
	assert(mln_domain(I0, NP, TypeG, A)).

translate_to_factor(W, D, Lits, Id, Vs, Phi, Domain) :-
	new_mln(Id),
	disj_to_list(D, LP, [], Lits, []),
	W0 is exp(W),
	findall(F, weight(LP, W0, F), Phi),
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
	disj_to_list(C1, L1, L1I, L, LI),
	disj_to_list(C2, L1I, L10, LI, L0).
disj_to_list((\+ C), [(\+ C)|L1], L1, [C|L], L) :- !.
disj_to_list(C, [C|L1], L1, [C|L], L).

%
% compute the weight table, assuming it is a disjunction
%
weight([(\+ _)], W0, P) :- !,
	% true case              false case
	( P = 1 ;  P = W0 ).
weight([_], W0, P) :- !,
	% true case              false case
	( P = W0  ;  P = 1 ).
weight([(\+ _)|R], W0, P) :- !,
	% true case              false case
	( weight(R, W0, P) ;  weight(R, W0, _), P = W0 ).
weight([_|R], W0, P) :-
	% true case              false case
	( weight(R, W0, _), P = W0 ;  weight(R, W0, P) ).

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

