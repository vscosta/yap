
:- object(tracer).

	:- info([
		version is 1.0,
		date is 2004/5/5,
		author is 'Paulo Moura',
		comment is 'A simple tracer meta-interpreter for pure Prolog.']).

	:- public(trace/1).
	:- mode(trace(+goal), zero_or_more).
	:- info(trace/1, [
		comment is 'Traces goal proof.',
		argnames is ['Goal']]).

	trace(Goal) :-
		trace(Goal, 1).

	trace(true, _) :-
		!.
	trace((A, B), Depth) :-
		!, trace(A, Depth), trace(B, Depth). 
	trace(A, Depth) :-
		write_trace(call, A, Depth),
		clause(A, B),
		Depth2 is Depth + 1,
		trace(B, Depth2),
		(	write_trace(exit, A, Depth)
			;
			write_trace(redo, A, Depth),
			fail).
	trace(A, Depth) :-
		write_trace(fail, A, Depth),
		fail.

	write_trace(Port, Goal, Depth) :-
		write(Depth), write(' '), write(Port), write(': '), writeq(Goal), nl.

:- end_object.
