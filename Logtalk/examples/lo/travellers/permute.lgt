
:- object(permute,
	extends(salesman)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- op(400, yfx, ~).


	route(Towns, Route) :-
		findall(
			(Towns2, Length),
			(permute(Towns, Towns2), route_length(Towns2, Length)),
			List),
		shortest(List, Route).


	permute([Town], Town).

	permute(Towns, Towns2~Town) :-
		delete(Towns, Town, Towns3),
		permute(Towns3, Towns2).


	delete([Head| Tail], Head, Tail).

	delete([Head| Tail], Element, [Head| Tail2]):-
		delete(Tail, Element, Tail2).


	route_length(Town, 0) :-
		atom(Town), !.

	route_length(Towns~Town1~Town2, Length) :-
		!,
		route_length(Towns~Town1, Length1),
		Town1::crow_flies(Town2, Length2),
		Length is Length1 + Length2.

	route_length(Town1~Town2, Length) :-
		Town1::crow_flies(Town2, Length).


	shortest(List, Shortest) :-
		shortest(List, null, 1000000, Shortest).


	shortest([], Route, Length, (Route, Length)).

	shortest([(Route, Length)| Routes], _, LX, Shortest) :-
		Length < LX, !,
		shortest(Routes, Route, Length, Shortest).

	shortest([_| Routes], RX, LX, Shortest) :-
		shortest(Routes, RX, LX, Shortest).


:- end_object.
