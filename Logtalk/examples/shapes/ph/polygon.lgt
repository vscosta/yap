:- object(polygon,
    extends(shape)).


	:- info([
		author is 'Paulo Moura',
		version is 1.1,
		date is 2004/1/8,
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


:- end_object.
