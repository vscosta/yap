
:- object(object,
	imports(category)).

	:- info([
		version is 3.0,
		author is 'Paulo Moura',
		date is 2007/06/11,
		comment is 'Example object for benchmarking library predicate calls and imported category predicate calls.']).

	:- public(length/2).

	length(List, Length) :-
		length(List, 0, Length).

	length([], Length, Length).
	length([_| Tail], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Tail, Acc2, Length).

	:- public(ctg_self/0).

	% call an imported category predicate by sending a message to self;
	% performance will depend on the distance between "self" and "this"
	% (always uses dynamic binding)
	ctg_self :-
		::ctg_pred.

	:- public(ctg_direct/0).

	% call an imported category predicate directly by using the :/1 control construct;
	% (static binding may be used, depending on how the category is compiled)
	ctg_direct :-
		:ctg_pred.

:- end_object.


:- object(descendant,
	extends(object)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Example object used for simulating a small hierarchy.']).

:- end_object.


:- object(leaf,
	extends(descendant)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Example object used for simulating a small hierarchy.']).

:- end_object.
