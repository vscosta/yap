
:- object(bounded_point,
	imports(bounded_coordinate),
	instantiates(class),
	specializes(point)).


	:- info([
		version is 1.1,
		date is 2000/10/31,
		authors is 'Paulo Moura',
		comment is 'Two dimensional point moving in a constrained area.',
		source is 'Example adopted from the SICStus Objects documentation.']).


	:- uses(list).


	move(X, Y) :-
		::check_bounds(x, X),
		::check_bounds(y, Y),
		^^move(X, Y).


	print :-
		::print_bounds(x),
		::print_bounds(y),
		^^print.


	instance_base_name(bp).


	default_init_option(bounds(x)-(-10, 10)).

	default_init_option(bounds(y)-(-10, 10)).

	default_init_option(Default) :-
		^^default_init_option(Default).


	process_init_option(bounds(Coordinate)-(Min, Max)) :-
		::set_bounds(Coordinate, Min, Max).

	process_init_option(Option) :-
		^^process_init_option(Option).


:- end_object.
