
:- category(bounded_coordinate).


	:- info([
		version is 1.0,
		date is 1998/3/23,
		authors is 'Paulo Moura',
		comment is 'Point coordinate bounds management predicates.',
		source is 'Example adopted from the SICStus Objects documentation.']).


	:- public(set_bounds/3).
	:- mode(set_bounds(+atom, +integer, +integer), one).

	:- public(clear_bounds/1).
	:- mode(clear_bounds(+atom), one).

	:- public(bounds/3).
	:- mode(bounds(?atom, ?integer, ?integer), zero_or_more).

	:- public(check_bounds/2).
	:- mode(check_bounds(+atom, +integer), zero_or_one).

	:- public(print_bounds/1).
	:- mode(print_bounds(?atom), zero_or_more).

	:- public(valid_value/2).
	:- mode(valid_value(+atom, +integer), zero_or_one).

	:- private(bounds_/3).
	:- dynamic(bounds_/3).
	:- mode(bounds_(?atom, ?integer, ?integer), zero_or_more).


	set_bounds(Coordinate, Min, Max) :-
		::retractall(bounds_(Coordinate, _, _)),
		::assertz(bounds_(Coordinate, Min, Max)).


	clear_bounds(Coordinate) :-
		::retractall(bounds_(Coordinate, _, _)).


	bounds(Coordinate, Min, Max) :-
		::bounds_(Coordinate, Min, Max).


	check_bounds(Coordinate, Value) :-
		::bounds_(Coordinate, Min, Max),
		Value >= Min,
		Value =< Max.


	print_bounds(Coordinate) :-
		::bounds_(Coordinate, Min, Max),
		writeq(bounds(Coordinate)),
		write(' : '),
		write((Min, Max)),
		nl.


	valid_value(Coordinate, Value) :-
		::bounds_(Coordinate, Min, Max) ->
			Value >= Min, Value =< Max
			;
			true.


:- end_category.
