
my_length(List, Length) :-
	integer(Length) ->
		Length >= 0,
		my_make_list(Length, List)
		;
		my_length(List, 0, Length).


my_make_list(0, []):-
	!.

my_make_list(N, [_| Tail]):-
	M is N-1,
	my_make_list(M, Tail).


my_length([], Length, Length).

my_length([_| Tail], Acc, Length) :-
	Acc2 is Acc + 1,
	my_length(Tail, Acc2, Length).
