:- object(polygon,
    instantiates(abstract_class),
    specializes(shape)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2003/2/3,
		comment is 'Generic polygon.']).


	:- public(nsides/1).

	:- mode(nsides(?integer), zero_or_one).

	:- info(nsides/1, [
		comment is 'Polygon number of sides.',
		argnames is ['Number']]).


	:- public(area/1).

	:- mode(area(-float), zero_or_one).

	:- info(area/1, [
		comment is 'Polygon area.',
		argnames is ['Area']]).


	:- public(perimeter/1).

	:- mode(perimeter(?atom), zero_or_one).

	:- info(perimeter/1, [
		comment is 'Polygon perimeter.',
		argnames is ['Perimeter']]).


	:- public(side/1).

	:- mode(side(?atom), zero_or_one).

	:- info(side/1, [
		comment is 'Polygon side length.',
		argnames is ['Length']]).


	side(1).         % default side length


:- end_object.
