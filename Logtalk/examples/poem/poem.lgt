
/* Points are going to be the fundamental quantity.                     */
/* These will be defined in Cartesian co-ordinates.                     */

:- object(point(_X, _Y)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric points.',
		parnames is ['X', 'Y'],
		source is 'Example adopted from the POEM system by Ben Staveley-Taylor.']).

	:- public(identical/1).
	:- mode(identical(+nonvar), one).

	:- public(distance/2).
	:- mode(distance(+nonvar, -number), one).

	identical(point(X1, Y1)) :-
		/* succeeds if the argument and owner points are the same.  */
		parameter(1, X),
		parameter(2, Y),
		X1 = X,
		Y1 = Y.

	distance(point(X1, Y1), Distance) :-
		/* finds the distance between argument and owner points.    */
		parameter(1, X),
		parameter(2, Y),
		Distance is sqrt((X1-X)*(X1-X)+(Y1-Y)*(Y1-Y)).

:- end_object.



/* A line is defined by its end points.                                 */
/* This class shows examples of calling its own and other class         */
/* predicates.                                                          */

:- object(line(_Point1, _Point2)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric lines.',
		parnames is ['Point1', 'Point2'],
		source is 'Example adopted from the POEM system by Ben Staveley-Taylor.']).

	:- public(length/1).
	:- mode(length(-number), one).

	:- public(intersects/1).
	:- mode(intersects(+nonvar), one).

	:- public(signed_distance/2).
	:- mode(signed_distance(+nonvar, -number), one).

	:- public(distance/2).
	:- mode(distance(+nonvar, -number), one).

	length(Length) :-
		/* sets Len to the length of the owner line                 */
		parameter(1, P1),
		parameter(2, P2),
		P1::distance(P2, Length).

    intersects(Line2) :-
		/* succeeds if Line2 intersects the owner line.             */
		/* this isn't necessarily a good method, but shows how to   */
		/* call class procedures from within the class definition.  */
		parameter(1, P1),
		parameter(2, P2),
		Line1 = line(P1, P2),
		Line2 = line(P3, P4),
		Line2::signed_distance(P1, D1),
		Line2::signed_distance(P2, D2),
		opposite_signs(D1, D2),
		Line1::signed_distance(P3, D3),
		Line1::signed_distance(P4, D4),
		opposite_signs(D3, D4).

	signed_distance(Point, Dist) :-
		/* finds the perpendicular distance from point to line.     */
		/* the sign of the answer depends on which side of the      */
		/* line the point is on.                                    */
		parameter(1, P1),
		parameter(2, P2),
		P1 = point(X1, Y1),
		P2 = point(X2, Y2),
		Point = point(X3, Y3),
		A is X2-X1,
		B is Y1-Y2,
		C is X1*Y2-X2*Y1,
		Dist is (A*Y3+B*X3+C)/sqrt(A*A+B*B).

	distance(Point, Dist) :-
		/* as 'signed_distance', but Dist always >= 0               */
		parameter(1, P1),
		parameter(2, P2),
		line(P1, P2)::signed_distance(Point, Temp),
		Dist is abs(Temp).

	/* 'opposite_signs' succeeds if its arguments are of opposite signs.    */
	/* It has a feature in that 'opposite_signs(0,0)' succeeds: this is     */
	/* because 0 is treated as having optional sign.                        */

	opposite_signs(A, B) :-
		o_s_aux(A, B).
	opposite_signs(A, B) :-
		o_s_aux(B, A).

	o_s_aux(A, B) :-
		A >= 0,
		B =< 0.

:- end_object.



/* Ellipses are defined by centre and semi-axes                         */

:- object(ellipse(_Center, _A, _B)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric ellipses.',
		parnames is ['Center', 'Rx', 'Ry'],
		source is 'Example adopted from the POEM system by Ben Staveley-Taylor.']).

	:- public(area/1).
	:- mode(area(-number), one).

	area(Area) :-
		pi(Pi),
		parameter(2, A),
		parameter(3, B),
		Area is Pi*A*B.

	pi(3.14196).

:- end_object.



/* Circle is a special form of ellipse                                  */
/* Subclasses ('circle' here) must have the same number of arguments    */
/* as their superclass ('ellipse') for the superclass predicates to     */
/* be applicable.  The arguments may be renamed for clarity.            */

:- object(circle(Center, Radius),
	extends(ellipse(Center, Radius, Radius))).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric circles.',
		parnames is ['Center', 'Radius'],
		source is 'Example adopted from the POEM system by Ben Staveley-Taylor.']).

	:- public(circumference/1).
	:- mode(circumference(-number), one).

	circumference(Circumference) :-
		pi(Pi),
		parameter(2, Radius),
		Circumference is 2*Pi*Radius.

	pi(3.14196).

:- end_object.
