
:- object(rectangle(_Width, _Height)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric rectangles.',
		parnames is ['Width', 'Height']]).


	:- public(width/1).

	:- mode(width(?number), zero_or_one).

	:- info(width/1, [
		comment is 'Rectangle width.',
		argnames is ['Width']]).


	:- public(height/1).

	:- mode(height(?number), zero_or_one).

	:- info(height/1, [
		comment is 'Rectangle height.',
		argnames is ['Height']]).


	:- public(area/1).

	:- mode(area(-number), one).

	:- info(area/1, [
		comment is 'Rectangle area.',
		argnames is ['Area']]).


	width(Width) :-
		parameter(1, Width).


	height(Height) :-
		parameter(2, Height).


	area(Area) :-
		::width(Width),
		::height(Height),
		Area is Width*Height.


:- end_object.
