
:- object(bounded_history_point,
	imports(bounded_coordinate, point_history),
	instantiates(class),
	specializes(point)).


	:- info([
		version is 1.1,
		date is 2000/10/31,
		author is 'Paulo Moura',
		comment is 'Two dimensional point moving in a constrained area and remembering past point positions.',
		source is 'Example adopted from the SICStus Objects documentation.']).


	move(X, Y) :-
		::check_bounds(x, X),
		::check_bounds(y, Y),
		::position(OldX, OldY),
		^^move(X, Y),
		::add_to_history((OldX, OldY)).


	print :-
		::print_bounds(x),
		::print_bounds(y),
		::print_history,
		^^print.


	instance_base_name(bhp).


	default_init_option(history-[]).

	default_init_option(bounds(x)-(-10, 10)).

	default_init_option(bounds(y)-(-10, 10)).

	default_init_option(Default) :-
		^^default_init_option(Default).


	process_init_option(history-History) :-
		::init_history(History).

	process_init_option(bounds(Coordinate)-(Min, Max)) :-
		::set_bounds(Coordinate, Min, Max).

	process_init_option(Option) :-
		^^process_init_option(Option).


:- end_object.
