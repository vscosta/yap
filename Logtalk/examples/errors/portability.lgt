
:- object(portability).


	:- public(predicate/0).


	predicate :-
		compare(Result, first, second),
		retractall(result(Result, _)),
		sort([], []),
		consult(file).


:- end_object.
