
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
