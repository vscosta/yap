
:- object(location(_X, _Y)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		parnames is ['X', 'Y'],
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- op(400, yfx, ~).


	:- public(at/2).
	:- mode(at(-integer, -integer), one).

	:- public(crow_flies/2).
	:- mode(crow_flies(+atom, -integer), one).

	:- public(drive/2).
	:- mode(drive(+atom, -nonvar), zero_or_more).

	:- public(links/1).
	:- mode(links(-list), one).

	:- public(road_distance/2).
	:- mode(road_distance(?atom, ?integer), zero_or_more).


	at(X, Y) :-
		parameter(1, X),
		parameter(2, Y).


	crow_flies(Town, Distance) :-
		::at(X, Y),
		Town::at(U, V),
		U0 is U-X,
		V0 is V-Y,
		Distance is sqrt(U0*U0+V0*V0).


	road_distance(Town, Distance) :-
		::links(Links),
		member((Town, Distance), Links).


	drive(To, Route) :-  % plan a road journey
		self(Self),
		plan_drive(Self, To, [], _, Route).


	% go directly

	plan_drive(From, To, _, Distance, From~To):-
		To::links(Links),
		member((From, Distance), Links).


	% go indirectly

	plan_drive(From, To, R, D+DI, Route~To):-
		To::links(Links),
		nearest(Links, From, Int, DI),
		\+ member(Int, R),
		plan_drive(From, Int, [To| R], D, Route).


	nearest(Links, To, Int, Distance):-
		quick(metric(To))::sort(Links, Sorted),
		member((Int, Distance), Sorted).


	member(Head, [Head| _]).

	member(Head, [_| Tail]) :-
		member(Head, Tail).


:- end_object.
