
bar :-
	write('bar predicate called'), nl.


:- object(bypass).

	:- public(foo//0).
	:- mode(foo, one).
	:- info(foo//0, [
		comment is 'Just the almighty and famous old foo.']).

	foo --> {{bar}}.

:- end_object.
