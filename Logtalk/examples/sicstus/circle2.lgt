
:- object(circle(Radius, Color),
	extends(ellipse(Radius, Radius, Color))).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric circles.',
		parnames is ['Radius', 'Color'],
		source is 'Example adopted from the SICStus Objects documentation.']).


	:- public(r/1).

	:- mode(r(?number), zero_or_one).

	:- info(r/1, [
		comment is 'Circle radius.',
		argnames is ['Radius']]).


	r(Radius) :-
		parameter(1, Radius).


	color(Color) :-
		parameter(2, Color).


	rx(Radius) :-
		::r(Radius).


	ry(Radius) :-
		::r(Radius).


	context :-
		write(circle2), nl,
		self(Self), write('self: '), writeq(Self), nl,
		this(This), write('this: '), writeq(This), nl,
		sender(Sender), write('sender: '), writeq(Sender), nl, nl,
		^^context.


:- end_object.
