
:- object(circle1(Color),
	extends(circle(1, Color))).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric circles with radius = 1.',
		parnames is ['Color'],
		source is 'Example adopted from the SICStus Objects documentation.']).


	context :-
		write(circle11), nl,
		self(Self), write('self: '), writeq(Self), nl,
		this(This), write('this: '), writeq(This), nl,
		sender(Sender), write('sender: '), writeq(Sender), nl,
		^^context.


:- end_object.
