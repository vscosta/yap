
:- object(salesman).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- op(400, yfx, ~).


	:- public(route/2).
	:- mode(route(+list, -nonvar), zero_or_more).


:- end_object.
