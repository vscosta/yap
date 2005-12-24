
bar :-					% test predicate
	write('bar predicate called'), nl.


:- object(bypass).

	:- public(foo//0).
	:- mode(foo, one).
	:- info(foo//0, [
		comment is 'Just the almighty and famous old foo.']).

	foo --> {{bar}}.	% the external pair of {}'s have the usual DCG semantics;
						% the internal pair of {}'s is the Logtalk compiler bypass control construct
:- end_object.
