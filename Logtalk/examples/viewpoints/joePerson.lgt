
:- object(joePerson).


	:- public(getOlder/0).

	:- public(address/1).

	:- public(age/1).
	:- dynamic(age/1).

	:- public(name/1).

	:- public(phone/1).

	:- public(score/1).
	:- dynamic(score/1).

	:- public(setScore/1).


	getOlder :-
		retract(age(Old)),
		New is Old + 1,
		asserta(age(New)).


	address('8 Octave Street').


	age(30).


	name('John').


	phone(11-11-11-11).


	score(0).


	setScore(Score) :-
		::retractall(score(_)),
		::asserta(score(Score)).


:- end_object.
