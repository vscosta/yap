
:- object(integer,
	extends(number)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Integer data type predicates.']).


	:- public(between/3).

	:- mode(between(+integer, +integer, ?integer), zero_or_more).

	:- info(between/3, [
		comment is 'Returns integers in the interval defined by the two first arguments.',
		argnames is ['Lower', 'Upper', 'Integer']]).


	between(Lower, Upper, Integer) :-
		integer(Lower),
		integer(Upper),
		(var(Integer) ->
			Lower =< Upper,
			generate(Lower, Upper, Integer)
			;
			integer(Integer),
			Lower =< Integer,
			Integer =< Upper).


	generate(Lower, _, Lower).

	generate(Lower, Upper, Integer) :-
		Lower < Upper,
		Next is Lower + 1,
		generate(Next, Upper, Integer).


	valid(Integer) :-
		integer(Integer).


:- end_object.
