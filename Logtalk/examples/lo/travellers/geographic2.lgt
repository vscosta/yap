
:- object(geographic(_OX, _OY)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		parnames is ['OX', 'OY'],
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- public(less/2).


	less(Town1, Town2):-
		angle(Town1, Angle1),
		angle(Town2, Angle2),
		Angle1 < Angle2.


    angle(Town, Angle) :-
		Town::at(X, Y),
		parameter(1, OX),
		parameter(2, OY),
    	angle(X, Y, OX, OY, Angle).


    angle(X, Y, OX, OY, Angle) :-
		X > OX,
		Y >= OY,
		Angle is atan((Y-OY)/(X-OX)).

    angle(X, Y, OX, OY, Angle) :-
		X > OX,
		Y < OY,
		pi(Pi),
		Angle is Pi + Pi - atan((OY-Y)/(X-OX)).

    angle(X, Y, OX, OY, Angle) :-
		X < OX,
		Y >= OY,
		pi(Pi),
		Angle is Pi - atan((Y-OY)/(OX-X)).

    angle(X, Y, OX, OY, Angle) :-
		X < OX,
		Y < OY,
		pi(Pi),
		Angle is Pi + atan((OY-Y)/(OX-X)).

    angle(OX, Y, OX, OY, Angle) :- 
		Y > OY,
    	pi(Pi),
    	Angle is Pi / 2.

    angle(OX, Y, OX, OY, Angle) :-
		Y =< OY,
    	pi(Pi),
    	Angle is 1.5 * Pi.


	pi(Pi) :-
		Pi is 4.0*atan(1.0).


:- end_object.
