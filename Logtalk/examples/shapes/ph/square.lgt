
:- object(square,
    extends(regular_polygon)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2003/2/3,
		comment is 'Geometric square.']).


	nsides(4).


	area(Area) :-
		::side(Side),
		Area is Side*Side.


:- end_object.
