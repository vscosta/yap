
:- object(joePerson).


	:- public(growOld/0).

	:- public(address/1).

	:- public(age/1).
	:- dynamic(age/1).

	:- public(name/1).

	:- public(phone/1).

	:- public(counter/1).
	:- dynamic(counter/1).

	:- public(incCounter/0).


	growOld :-
		retract(age(Old)),
		New is Old + 1,
		asserta(age(New)).


	address('8 Octave Street').


	age(30).


	name('John').


	phone(11-11-11-11).


	counter(0).


	incCounter :-
		(::retract(counter(Old)) ->
			true
			;
			Old = 0),
		New is Old + 1,
		::asserta(counter(New)).


:- end_object.
