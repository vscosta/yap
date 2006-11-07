
:- object(lists).

	:- threaded.

	:- public(member/2).

	member(H, [H| _]).
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
