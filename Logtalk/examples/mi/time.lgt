
:- object(time). 


	:- public(t/1).
	:- mode(t(?integer), zero_or_one).
	
	:- private(t_/1).
	:- mode(t_(?integer), zero_or_one).
	:- dynamic(t_/1).

	:- public(translate/1).
	:- mode(translate(+integer), zero_or_one).


	t(T) :-
		::t_(T).


	translate(T) :-
		integer(T),
		::retractall(t_(_)),
		::assertz(t_(T)).


:- end_object.
