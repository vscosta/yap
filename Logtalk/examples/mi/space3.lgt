
:- object(space(_X,_Y,_Z)). 


	:- public(distance/1).
	:- mode(xyz(?nunber), one).


	distance(Distance) :-
		parameter(1, X),
		parameter(2, Y),
		parameter(3, Z),
		Distance is sqrt(X*X+Y*Y+Z*Z).


:- end_object.
