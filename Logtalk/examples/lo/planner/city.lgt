
:- object(city).


	:- public(step/3).
	:- mode(step(+, +, -), zero_or_more).

	:- public(airport/1).
	:- mode(airport(?atom), zero_or_more).


	step(X, Y, P1-P-P2) :-
		\+ same_city(X, Y), !,
		X::airport(XA),
		Y::airport(YA),
		plan(fly)::from(XA, YA, P),
		plan(city)::from(X, XA, P1),
		plan(city)::from(YA, Y, P2).

	step(X, Y, taxi(X, Y)) :-
		same_city(X, Y),
		X \= Y.


	same_city(X, Y) :-
		X::airport(A),
		Y::airport(A).


:- end_object.
