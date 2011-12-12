#!/home/tgomes/bin/yap -L --

/* 
Steps:
1. generate N facts lives(I, nyc), 0 <= I < N.
2. generate evidence on descn for N people, *** except for 1 ***
3. Run query ?- guilty(joe, Guilty), witness(joe, t), descn(2,t), descn(3, f), descn(4, f) ...
*/

:- initialization(main).


main :-
	unix(argv([H])),
	generate_town(H).


generate_town(N) :-
	atomic_concat(['town_', N, '.yap'], FileName),
	open(FileName, 'write', S),
	write(S, ':- source.\n'),
	write(S, ':- style_check(all).\n'),
	write(S, ':- yap_flag(unknown,error).\n'),
	write(S, ':- yap_flag(write_strings,on).\n'),
	write(S, ':- use_module(library(clpbn)).\n'),
	write(S, ':- set_clpbn_flag(solver, bp).\n'),
	write(S, ':- [-schema].\n\n'),
	write(S, 'lives(_joe, nyc).\n'),
    atom_number(N, N2),
	generate_people(S, N2, 2),
	write(S, '\nrun_query(Guilty) :- \n'),
	write(S, '\tguilty(joe, Guilty),\n'),
	write(S, '\twitness(nyc, t),\n'),
	write(S, '\trunall(X, ev(X)).\n\n\n'),
	write(S, 'runall(G, Wrapper) :-\n'),
	write(S, '\tfindall(G, Wrapper, L),\n'),
	write(S, '\texecute_all(L).\n\n\n'),
	write(S, 'execute_all([]).\n'),
	write(S, 'execute_all(G.L) :-\n'),
	write(S, '\tcall(G),\n'),
	write(S, '\texecute_all(L).\n\n\n'),
	generate_query(S, N2, 2),
	close(S).


generate_people(_, N, Counting1) :- !.
generate_people(S, N, Counting) :-
	format(S, 'lives(p~w, nyc).~n', [Counting]),
	Counting1 is Counting + 1,
	generate_people(S, N, Counting1).


generate_query(S, N, Counting) :- 
	Counting > N, !.
generate_query(S, N, Counting) :- !,
	format(S, 'ev(descn(p~w, t)).~n', [Counting]),
	Counting1 is Counting + 1,
	generate_query(S, N, Counting1).

