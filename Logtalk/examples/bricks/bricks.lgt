
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


:- object(brick_stack,
	instantiates(constrained_relation)).

	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Stack of bricks as a constrained binary relation.']).

	descriptor_([top, bottom]).

	domain_(top, brick).
	domain_(bottom, brick).

	key_([top, bottom]).

	cardinality_(top, 0, 1).
	cardinality_(bottom, 0, 1).

	delete_option_(top, cascade).
	delete_option_(bottom, restrict).

	add_tuple([A, B]) :-
		B::position(Xb, Yb),
		Ya2 is Yb + 1,
		{A::move(Xb, Ya2)},
		^^add_tuple([A, B]).

	activ_points_(top, before, []).
	activ_points_(top, after, [move(_, _)]).

	activ_points_(bottom, before, []).
	activ_points_(bottom, after, [move(_, _)]).

	propagate(after, move(X, Y), Top, top, [Top, Bottom]) :-
		!,
		Y2 is Y - 1,
		(Bottom::position(X, Y2) ->
			true
			;
			::remove_tuple([Top, Bottom])).

	propagate(after, move(X, Y), Bottom, bottom, [Top, Bottom]) :-
		!,
		Y2 is Y + 1,
		{Top::move(X, Y2)}.

:- end_object.


:- object(stack_monitor,
	implements(event_handlersp)).

	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Monitor for brick movements printing an ascii representation of each brick position.']).

	:- uses(loop).
	:- uses(list).

	after(_, move(_, _), _) :-
		findall(
			(Brick, X, Y),
			(instantiates_class(Brick, brick), Brick::position(X, Y)),
			Bricks),
		setof(X, Brick^Y^ (list::member((Brick,X,Y), Bricks)), Xs),
		list::last(Xs, Xmax),
		setof(Y, Brick^X^ (list::member((Brick,X,Y), Bricks)), Ys),
		list::last(Ys, Ymax),
		loop::fordownto(Y, Ymax, 1,
			(write('|'),
			 loop::forto(X, 1, Xmax,
        		(list::member((Brick, X, Y), Bricks) ->
            		write(Brick)
            		;
            		write('.'))),
			 nl)),
		write('-'),
		loop::forto(X, 1, Xmax, write('-')), nl.

:- end_object.
