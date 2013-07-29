:- module(mln,
		[op(1150,fx,mln),
		 op(1150,fx,mln_domain),
		 mln_domain/1,
		 mln_literal/1,
		 mln/1,
		 mln/5,
		 mln_w/2,
		 portray_mln/0,
		 portray_mln/1]).

:- use_module(library(pfl)).
:- use_module(library(maplist)).
:- use_module(library(lists)).
:- use_module(library(terms)).

:- dynamic mln/1, mln/2, mln_domain/4, mln/5, mln_w/2, mln_domain/5, mln_type_def/1.

user:term_expansion(mln_domain(P),[]) :-
	expand_domain(P).

user:term_expansion( mln(W: D), pfl:factor(markov,Id,FList,FV,Phi,Constraints)) :-
	translate_to_factor(W, D, FList, Id, FV, Phi, Constraints), !.
user:term_expansion( mln(W: D), _) :-
	throw(error(domain_error(mln,W:D),error)).

user:term_expansion(end_of_file,_) :-
	mln_domain(TypeG, NP, I0, A, Type),
	add_mln_domain(TypeG, NP, I0, A, Type),
	fail.
user:term_expansion(end_of_file,end_of_file).

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
	assert(mln_domain(TypeG, NP, I0, A, Type)),
	assert(mln_domain(I0, NP, TypeG, A)).

add_mln_domain(TypeG, NP, I0, A, _) :-
	mln_type_def(TypeG), !,
	functor(NP, G, Ar),
	functor(NNP, G, Ar),
	arg(I0, NNP, A),
	assert_static(user:(TypeG :- NNP)).
add_mln_domain(TypeG, _NP, _I0, _A, _) :-
	predicate_property(user:TypeG, _), !.
add_mln_domain(TypeG, NP, I0, A, Type) :-
	assert(mln_type_def(TypeG)), !,
	functor(NP, G, Ar),
	functor(NNP, G, Ar),
	arg(I0, NNP, A),
	table(user:Type/1),
	assert_static(user:(TypeG :- NNP)).
	

translate_to_factor(W, D, Lits, Id, Vs, Phi, Domain) :-
	W0 is exp(W),
	(
	    Do = disj,
	    disj_to_list(D, LP, [], Lits, [])
	->
	    findall(F, weight(LP, W0, 1, F), Phi)
	;
	    Do = conj,
	    conj_to_list(D, LP, [], Lits, [])
	->
            findall(F, weight(LP, 1, W0, F), Phi)
	;
	    Do = disj,
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
	create_domain(Lits, Domain0),
	exclude(ground, Domain0, Domain),
	make_clause(Id, Do, Vs, Domain, Lits, LP, Head), 
	assert(mln_w(Id, W)),
	find_pos(LP, NLP, Pos),
	foldl2(merge_domain_lits, Domain, NDomain, [], _, NLP, [] ),
	exclude(empty_goal, [true-Pos|NDomain], FDomain),
	assert(mln(Id, Do, LP, Head, FDomain)).

empty_goal(_-[]).

find_pos([], [], []).
find_pos([-Lit|Lits], LP, [Lit|Pos]) :- !,
	find_pos(Lits, LP, Pos).
find_pos([Lit|Lits], [Lit|LP], Pos) :-
	find_pos(Lits, LP, Pos).

%
% naive translation of conj/disjunction into Prolog
%
make_clause(Id, Do, Vs, Domain, _Lits, Fs, Head) :-
	Head =.. [mln,Id|Vs],
	order_body(Do, Fs, Bd0),
	add_domain(Domain, Bd, once(Bd0)),
	assert_static(user:(Head :- Bd)).

order_body(disj, Fs, Bd0) :-
	order_body(Fs, Bd0).
order_body(conj, Fs, Bd0) :-
	ander_body(Fs, Bd0),


order_body([-G], (\+ G)). 
order_body([G], (G)). 	
order_body([-G|Gs], (\+G ; NGs)) :-
	order_body(Gs, NGs).
order_body([G|Gs], (G ; NGs)) :-
	order_body(Gs, NGs).

ander_body([-G], (\+ G)). 
ander_body([G], (G)). 	
ander_body([-G|Gs], (\+G , NGs)) :-
	ander_body(Gs, NGs).
ander_body([G|Gs], (G , NGs)) :-
	ander_body(Gs, NGs).

add_domain([G], (G,B0), B0) :- !.
add_domain([G|Gs], (G,NGs), G0) :-
	add_domain(Gs, NGs, G0).

new_skolem(Id, Lit) :-
	pfl:new_skolem(Lit, [f,t]),
	assert(pfl:skolem_in(Lit, Id)).

mln(0).

new_mln(Id) :-
	retract(mln(Id)),
	Id1 is Id+1,
	assert(mln(Id1)).

merge_domain_lits( Domain, Domain-Gs, Vs, [V|Vs], LP0, LPF) :-
	arg(1, Domain, V),
	delete_with_v(LP0, [V|Vs], LPF, Gs).

delete_with_v([], _Vs, [], []).
delete_with_v([G|LP0], Vs, LPF, NGs) :-
	new_variables_in_term(Vs, G, NVs),
	( NVs == [] ->
	    NGs = [G|Gs],
	    LPF = LP1
	;
	    NGs = Gs,
	    LPF = [G|LP1]
	),
	delete_with_v(LP0, Vs, LP1, Gs).
 

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
disj_to_list2((\+ C), [(-C)|L1], L1, [C|L], L) :- !.
disj_to_list2((- C), [(-C)|L1], L1, [C|L], L) :- !.
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
conj_to_list2((\+ C), [(C)|L1], L1, [C|L], L) :-  !.
conj_to_list2((- C), [(C)|L1], L1, [C|L], L) :- !.
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

%
% very simple, inneficient converter from logic expressions to cnf
%
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


%
% count groundings
%
all_true(Id, V) :-
	mln(Id, _F, _D, Head, _),
	nb_create_accumulator(0, Acc),
	(
	    call(user:Head),
	    nb_add_to_accumulator(Acc, 1),
	    fail
        ;
	    nb_accumulator_value(Acc, V)
        ).

portray_mln :-
	portray_mln( user_error ).

portray_mln(Stream) :-
	mln(Id, _, LP, _, _),
	mln_w(Id, W),
	format(Stream, 'mln ~4f : ( ', [W] ),
	numbervars(LP, 1, _),
	portray_lits(Stream, LP),
	format(Stream, ' ).~n', [] ),
	fail.
portray_mln(_Stream).

portray_lits(Stream, [L1]) :- !,
	portray_lit( Stream, L1 ).
portray_lits(Stream, [L1|Ls]) :- !,
	portray_lit( Stream, L1 ),
	format(Stream, ' ; ', []),
	portray_lits( Stream, Ls ).

portray_lit( Stream, -L ) :- !,
	format( Stream, '\\+ ~q', [L] ).
portray_lit( Stream, L ) :-
	format( Stream, '~q', [L] ).


