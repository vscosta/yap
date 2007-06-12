
my_length(List, Length) :-
	my_length(List, 0, Length).

my_length([], Length, Length).
my_length([_| Tail], Acc, Length) :-
	Acc2 is Acc + 1,
	my_length(Tail, Acc2, Length).


:- dynamic(pred_plain/1).

plain_dyndb(N) :-
	assertz(pred_plain(N)),
	retract(pred_plain(N)).


my_between(Lower, _, Lower).
my_between(Lower, Upper, Integer) :-
	Lower < Upper,
	Next is Lower + 1,
	my_between(Next, Upper, Integer).


my_repeat(_).
my_repeat(N) :-
	N > 1,
	N2 is N - 1,
	my_repeat(N2).


% generate a list containing the first N non-negative integers

generate_list(N, List) :-
	generate_list(0, N, List).

generate_list(N, N, []) :-
	!.
generate_list(M, N, [M| Ms]) :-
	M < N,
	M2 is M + 1,
	generate_list(M2, N, Ms).
