
:- object(history_point,
	imports(point_history),
	instantiates(class),
	specializes(point)).


	:- info([
		version is 1.1,
		date is 2000/10/31,
		authors is 'Paulo Moura',
		comment is 'Two dimensional point remembering past positions.',
		source is 'Example adopted from the SICStus Objects documentation.']).


	move(X, Y) :-
		::position(OldX, OldY),
		^^move(X, Y),
		::add_to_history((OldX, OldY)).


	print :-
		::print_history,
		^^print.


	instance_base_name(hp).


	default_init_option(history-[]).

	default_init_option(Default) :-
		^^default_init_option(Default).


	process_init_option(history-History) :-
		::init_history(History).

	process_init_option(Option) :-
		^^process_init_option(Option).


:- end_object.
