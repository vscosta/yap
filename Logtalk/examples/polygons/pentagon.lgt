
:- object(pentagon,
	instantiates(class),
	specializes(polygon)).


	:- info([
		version is 1.0,
		date is 1998/3/23,
		authors is 'Paulo Moura',
		comment is 'Pentagon class.']).


	number_of_sides(5).


	instance_base_name(pen).


:- end_object.
