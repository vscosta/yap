
% we use a parametric object in order to give an interpretation to an 
% object proxy arguments and to encapsulate relevant predicates:

:- object(circle(_Radius, _Color)).

	:- public([
		radius/1,
		color/1,
		area/1,
		perimeter/1,
		print/0]).

	radius(Radius) :-
		parameter(1, Radius).

	color(Color) :-
		parameter(2, Color).

	area(Area) :-
		::radius(Radius),
		Area is 3.1415927*Radius*Radius.

	perimeter(Perimeter) :-
		::radius(Radius),
		Perimeter is 2*3.1415927*Radius.

	print :-
		area(Area), write('area: '), write(Area),
		perimeter(Perimeter), write(', perimeter: '), write(Perimeter),
		color(Color), write(', color: '), write(Color), nl.

:- end_object.


% parametric object proxies (with an extra argument to represent identity):

circle('#1', 1.23, blue).
circle('#2', 3.71, yellow).
circle('#3', 0.39, green).
circle('#4', 5.74, black).
circle('#5', 8.32, cyan).
