
:- object(fly).


	:- public(step/3).
	:- mode(step(+, +, -), zero_or_more).


	step(From, To, fly(From, To)) :-
		From::fly(To).


:- end_object.
