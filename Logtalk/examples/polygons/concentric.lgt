
:- object(concentric,
	instantiates(constrained_relation)).


	:- info([
		version is 1.1,
		date is 2004/8/15,
		author is 'Paulo Moura',
		comment is 'Concentric polygons as a constrained binary relation.']).


	:- uses(list,
		[member/2, select/3]).


	descriptor_([x1, x2]).


	domain_(x1, polygon).
	domain_(x2, polygon).


	key_([x1, x2]).


	cardinality_(x1, 0, n).
	cardinality_(x2, 0, n).


	delete_option_(x1, cascade).
	delete_option_(x2, cascade).


	add_tuple([Polygon| Polygons]) :-
		Polygon::position(X, Y),
		forall(member(Polygon2, Polygons), {Polygon2::move(X, Y)}),
		^^add_tuple([Polygon| Polygons]).


	activ_points_(x1, before, []).
	activ_points_(x1, after, [move(_, _), transX(_), transY(_)]).

	activ_points_(x2, before, []).
	activ_points_(x2, after, [move(_, _), transX(_), transY(_)]).


	propagate(after, move(X, Y), Polygon, _, Tuple) :-
		select(Polygon, Tuple, Polygons),
		!,
		forall(
			(member(Polygon2, Polygons),\+ Polygon2::position(X, Y)),
			{Polygon2::move(X, Y)}).

	propagate(after, transX(X), Polygon, _, Tuple) :-
		select(Polygon, Tuple, Polygons),
		!,
		forall(
			(member(Polygon2, Polygons), \+ Polygon2::position(X, _)),
			{Polygon2::transX(X)}).

	propagate(after, transY(Y), Polygon, _, Tuple) :-
		select(Polygon, Tuple, Polygons),
		!,
		forall(
			(member(Polygon2, Polygons), \+ Polygon2::position(_, Y)),
			{Polygon2::transY(Y)}).


:- end_object.
