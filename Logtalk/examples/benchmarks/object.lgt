
:- object(object).

	:- public(length/2).

	length(List, Length) :-
		integer(Length) ->
			Length >= 0,
			make_list(Length, List)
			;
			length(List, 0, Length).

	make_list(0, []) :-
		!.
	make_list(N, [_| Tail]):-
		M is N-1,
		make_list(M, Tail).

	length([], Length, Length).
	length([_| Tail], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Tail, Acc2, Length).

:- end_object.
