
:- object(hook).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2006/01/29,
		comment is 'Example of an object defining compiler hook predicates.']).

	:- public(hook/2).
	:- mode(hook(@nonvar, -list), zero_or_one).
	:- info(hook/2, [
		comment is 'Compiler hook predicate.',
		arguments is ['Term'-'Source file term', 'Terms'-'Resulting list of terms']]).

	hook((:- info(Original)), [(:- info(New))]) :-
		expand_author(Original, New).

	expand_author([], []).
	expand_author([Info| Infos], [Info2| Infos2]) :-
		(	Info = (author is Abbreviation) ->
			author(Abbreviation, FullName),
			Info2 = (author is FullName)
		;	Info = Info2
		),
		expand_author(Infos, Infos2).

	author(pm, 'Paulo Moura, pmoura@logtalk.org').

:- end_object.
