:- object(regular_polygon,
    instantiates(abstract_class),
    specializes(polygon)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2003/2/3,
		comment is 'Generic regular polygon.']).


	perimeter(Perimeter) :-
		::nsides(Number),
		::side(Side),
		Perimeter is Number*Side.


:- end_object.
