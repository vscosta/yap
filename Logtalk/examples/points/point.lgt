
:- object(point,
	instantiates(class),
	specializes(object)).


	:- info([
		version is 1.1,
		date is 2000/10/31,
		authors is 'Paulo Moura',
		comment is 'Two dimensional point class.',
		source is 'Example adopted from the SICStus Objects documentation.']).


	:- public(move/2).
	:- mode(move(+integer, +integer), zero_or_one).

	:- public(position/2).
	:- mode(position(?integer, ?integer), one).

	:- private(position_/2).
	:- dynamic(position_/2).
	:- mode(position_(?integer, ?integer), one).


	move(X, Y) :-
		::retractall(position_(_, _)),
		::assertz(position_(X, Y)).


	position(X, Y) :-
		::position_(X, Y).


	print :-
		self(Self),
		::position_(X, Y),
		writeq(Self), write(' @ '), write((X, Y)), nl.


	default_init_option(position-(0, 0)).

	default_init_option(Default) :-
		^^default_init_option(Default).


	process_init_option(position-(X, Y)) :-
		::assertz(position_(X, Y)).

	process_init_option(Option) :-
		^^process_init_option(Option).


	instance_base_name(p).


:- end_object.
