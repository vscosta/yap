
:- object(brick,
	instantiates(class),
	specializes(object)).


	:- info([
		version is 1.1,
		date is 2000/10/31,
		author is 'Paulo Moura',
		comment is 'Two-dimensional brick (or should I say square?) class.']).


	:- public(position/2).

	:- mode(position(?integer, ?integer), zero_or_one).

	:- info(position/2, [
		comment is 'Brick current position.',
		argnames is ['X', 'Y']]).


	:- private(position_/2).
	:- dynamic(position_/2).

	:- mode(position_(?integer, ?integer), zero_or_one).

	:- info(position_/2, [
		comment is 'Stores brick current position.',
		argnames is ['X', 'Y']]).


	:- public(move/2).

	:- mode(move(+integer, +integer), one).

	:- info(move/2, [
		comment is 'Moves a brick to a new position.',
		argnames is ['X', 'Y']]).


	position(X, Y) :-
		::position_(X, Y).


	move(X, Y) :-
		::retractall(position_(_, _)),
		::assertz(position_(X, Y)).


	default_init_option(position-(0, 0)).

	default_init_option(Default) :-
		^^default_init_option(Default).


	process_init_option(position-(X, Y)) :-
		::assertz(position_(X, Y)).

	process_init_option(Option) :-
		^^process_init_option(Option).


	valid_init_option(position-(X, Y)) :-
		!,
		integer(X),
		integer(Y).

	valid_init_option(Option) :-
		^^valid_init_option(Option).


	instance_base_name(b).


:- end_object.
