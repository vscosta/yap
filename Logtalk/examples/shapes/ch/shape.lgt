:- object(shape,
	instantiates(abstract_class),
	specializes(object)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2003/2/3,
		comment is 'Generic geometric shape.']).


	:- public(color/1).

	:- mode(color(?atom), zero_or_one).

	:- info(color/1, [
		comment is 'Shape color.',
		argnames is ['Color']]).


	:- public(position/2).

	:- mode(position(?integer, ?integer), zero_or_one).

	:- info(position/2, [
		comment is 'Shape position.',
		argnames is ['X', 'Y']]).


	color(red).      % default shape color


	position(0, 0).  % default shape position


:- end_object.
