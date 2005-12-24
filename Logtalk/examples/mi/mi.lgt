
:- object(space). 

	:- public(xyz/3).
	:- mode(xyz(?integer, ?integer, ?integer), zero_or_one).

	:- private(xyz_/3).
	:- mode(xyz_(?integer, ?integer, ?integer), zero_or_one).
	:- dynamic(xyz_/3).

	:- public(rotate/3).
	:- mode(rotate(+integer, +integer, +integer), zero_or_one).

	xyz(X, Y, Z) :-
		::xyz_(X, Y, Z).

	rotate(X, Y, Z) :-
		integer(X),
		integer(Y),
		integer(Z),
		::retractall(xyz_(_, _, _)),
		::assertz(xyz_(X, Y, Z)).

:- end_object.


:- object(time). 

	:- public(t/1).
	:- mode(t(?integer), zero_or_one).
	
	:- private(t_/1).
	:- mode(t_(?integer), zero_or_one).
	:- dynamic(t_/1).

	:- public(translate/1).
	:- mode(translate(+integer), zero_or_one).

	t(T) :-
		::t_(T).

	translate(T) :-
		integer(T),
		::retractall(t_(_)),
		::assertz(t_(T)).

:- end_object.


:- object(space_time,
	extends(space, time)). 

	:- public(xyzt/4).
	:- mode(xyzt(?integer, ?integer, ?integer, ?integer), zero_or_one).

	xyzt(X, Y, Z, T) :-
		::xyz(X, Y, Z),
		::t(T).

:- end_object.




:- object(space(_X,_Y,_Z)). 

	:- public(distance/1).
	:- mode(xyz(?nunber), one).

	distance(Distance) :-
		parameter(1, X),
		parameter(2, Y),
		parameter(3, Z),
		Distance is sqrt(X*X+Y*Y+Z*Z).

:- end_object.


:- object(time(_T)). 

	:- public(time/1).
	:- mode(time(?integer), zero_or_one).

	time(Time) :-
		parameter(1, Time).

:- end_object.


:- object(space_time(X, Y, Z, T),
	extends(space(X, Y, Z), time(T))). 

:- end_object.
