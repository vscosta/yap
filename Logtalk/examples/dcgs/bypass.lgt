
bar :-
	write('bar predicate called'), nl.


:- object(bypass).

	:- public(foo/2).

	foo --> {{bar}}.

:- end_object.
