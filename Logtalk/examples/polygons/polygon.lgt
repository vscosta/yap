
:- object(polygon,
	instantiates(abstract_class),
	specializes(object)).


	:- info([
		version is 1.1,
		date is 2000/10/31,
		author is 'Paulo Moura',
		comment is 'Polygon predicates.']).


	:- uses(list).


	:- public(move/2).
	:- mode(move(+integer, +integer), one).

	:- public(number_of_sides/1).
	:- mode(number_of_sides(?integer), zero_or_one).

	:- private(position_/2).
	:- dynamic(position_/2).
	:- mode(position_(?integer, ?integer), zero_or_one).

	:- public(position/2).
	:- mode(position(?integer, ?integer), zero_or_one).


	:- info([
		version is 2,
		date is 1998/2/23,
		author is 'Paulo Moura',
		comment is 'Default protocol for all polygons.']).


	position(X, Y) :-
		::position_(X, Y).


	move(X, Y) :-
		::retractall(position_(_, _)),
		::assertz(position_(X, Y)).


	trans_x(X) :-
		::retractall(position_(_, Y)),
		::assertz(position_(X, Y)).


	trans_y(Y) :-
		::retractall(position_(X, _)),
		::assertz(position_(X, Y)).


	default_init_option(position-(0, 0)).

	default_init_option(Default) :-
		^^default_init_option(Default).


	process_init_option(position-(X, Y)) :-
		::move(X, Y).

	process_init_option(Option) :-
		^^process_init_option(Option).


:- end_object.

