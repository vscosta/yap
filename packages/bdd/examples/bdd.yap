
:- style_check(all).

:- use_module(library(bdd)).
:- use_module(library(lists)).
:- use_module(library(maplist)).

t1 :-
	t1(X),
	writeln(X),
	fail.
t1.

t1(BDD:T) :-
	bdd_new(X+(_Y+X)*(-_Z), BDD),
	bdd_tree(BDD,T).


t2 :-
	t2(X),
	writeln(X),
	fail.
t2.

t2(V) :-
	bdd_new(X+(Y+X)*(-Z), BDD),
	member(X, [0,1]),
	member(Y, [0,1]),
	member(Z, [0,1]),
	bdd_eval(BDD, V).

t3 :-
	t3(X),
	writeln(X),
	fail.
t3.

t3(V) :-
	bdd_new(X+(Y+X)*(-Z), BDD),
	bdd_tree(BDD, bdd(_, T, _Vs) ),
	reverse(T, RT),
	member(X, [0,1]),
	member(Y, [0,1]),
	member(Z, [0,1]),
	foldl(eval_bdd, RT, _, V).

eval_bdd(pp(P,X,L,R), _, P) :-
	P is ( X/\L ) \/ ( (1-X) /\ R ).
eval_bdd(pn(P,X,L,R), _, P) :-
	P is ( X/\L ) \/ ( (1-X) /\ (1-R) ).



t4 :-
	t4(_),
	fail.
t4.

t4([X,Y,Z]) :-
	bdd_new(X+(Y+X)*(-Z), BDD),
	bdd_print(BDD, user_output).



t5 :-
	t5(X),
	writeln(X),
	fail.
t5.

t5(V) :-
	bdd_new(X+(Y+X)*(-Z), BDD),
	member(X, [0.3,0.7]),
	member(Y, [0.4,0.6]),
	member(Z, [0.9,0.1]),
	bdd_to_probability_sum_product(BDD, V).


t6 :-
	t6(X),
	writeln(X),
	fail.
t6.

t6(V) :-
	bdd_new(X+(Y+X)*(-Z), BDD),
	bdd_tree(BDD, bdd(_, T, _Vs) ),
	reverse(T, RT),
	member(X, [0.3,0.7]),
	member(Y, [0.4,0.6]),
	member(Z, [0.9,0.1]),
	foldl(eval_prob, RT, _, V).

eval_prob(pp(P,X,L,R), _, P) :-
	P is  X*L +  (1-X) * R.
eval_prob(pn(P,X,L,R), _, P) :-
	P is  X * L + (1-X) * (1-R).
