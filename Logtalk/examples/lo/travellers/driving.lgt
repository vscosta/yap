
:- object(driving,
	extends(salesman)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- op(400, yfx, ~).


	route(Towns, Route) :-
		presort::route(Towns, Presort), 
		drive_around(Presort, Route).


	drive_around(Route~Town1~Town2, Route1~Route2) :-
		!,
		drive_around(Route~Town1, Route1),
		Town1::drive(Town2, Route2).

	drive_around(Town1~Town2, Route) :-
		!,
		Town1::drive(Town2, Route).

	drive_around(Town, Town).


	drive_length(Route, Length) :-
		drive_length(Route, 0, Length).


	drive_length(Route~Town1~Town2, Acc, Length) :-
		!,
		Town1::road_distance(Town2, Length2),
		Acc2 is Acc + Length2,
		drive_length(Route~Town1, Acc2, Length).

	drive_length(Town1~Town2, Acc, Length) :-
		!,
		Town1::road_distance(Town2, Length2),
		Length is Acc + Length2.

	drive_length(_, Length, Length).


:- end_object.
