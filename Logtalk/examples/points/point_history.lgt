
:- category(point_history).


	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Point position history management predicates.',
		source is 'Example adopted from the SICStus Objects documentation.']).


	:- public(add_to_history/1).
	:- mode(add_to_history(+nonvar), one).

	:- public(init_history/1).
	:- mode(init_history(+list), one).

	:- public(history/1).
	:- mode(history(-list), zero_or_one).

	:- public(print_history/0).
	:- mode(print_history, zero_or_one).

	:- private(history_/1).
	:- dynamic(history_/1).
	:- mode(history_(-list), zero_or_one).


	add_to_history(Location) :-
		::retract(history_(History)),
		::assertz(history_([Location| History])).


	init_history(History) :-
		::retractall(history_(_)),
		::assertz(history_(History)).


	history(History) :-
		::history_(History).


	print_history :-
		::history_(History),
		write('location history: '),
		write(History),
		nl.


:- end_category.
