
:- object(regular_polygon,
    instantiates(abstract_class),
    specializes(polygon)).


	:- info([
		author is 'Paulo Moura',
		version is 1.1,
		date is 2004/1/8,
		comment is 'Generic regular polygon.']).


	:- public(side/1).

	:- mode(side(?atom), zero_or_one).

	:- info(side/1, [
		comment is 'Regular polygon side length.',
		argnames is ['Length']]).


	side(1).         % default side length


	perimeter(Perimeter) :-
		::nsides(Number),
		::side(Side),
		Perimeter is Number*Side.


:- end_object.
