
:- object(red_circle(Radius),
	extends(circle(Radius, red))).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric red circles.',
		parnames is ['Radius'],
		source is 'Example adopted from the SICStus Objects documentation.']).


	context :-
		write(red_circle1), nl,
		self(Self), write('self: '), writeq(Self), nl,
		this(This), write('this: '), writeq(This), nl,
		sender(Sender), write('sender: '), writeq(Sender), nl, nl,
		^^context.


:- end_object.
