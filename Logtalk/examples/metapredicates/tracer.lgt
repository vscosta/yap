
% example adopted from:
% Programming Language Prolog Part 2, Modules
% Committee Draft - January 14, 1998 X3J17/97/5


:- object(tracer).


	:- info([
		version is 2,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Tracer for a goal call and exit ports.']).


	:- public(trace/1).
	:- metapredicate(trace(::)).

	:- mode(trace(+callable), zero_or_more).

	:- info(trace/1, [
		comment is '.',
		argnames is ['Goal']]).


	trace(Goal) :-
		write('call: '), writeq(Goal), nl,
		call(Goal),
		write('exit: '), writeq(Goal), nl.
		
	trace(Goal) :-
		write('fail: '), writeq(Goal), nl,
		fail.


:- end_object.
