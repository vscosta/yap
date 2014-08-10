% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(N,H),

  init_fasta(ALU, IUB, HOMOSAPIENS, RAND0),

  N1 is N * 2,
  N2 is N * 3,
  N3 is N * 5,

  repeat_fasta('ONE', 'Homo sapiens alu', N1, ALU),

  make_cumulative(IUB, CVIUB),

  random_fasta('TWO', 'IUB ambiguity codes', N2, CVIUB, RAND0, RAND1),

  make_cumulative(HOMOSAPIENS, CVHOMOSAPIENS),

  random_fasta('THREE', 'Homo sapiens frequency', N3, CVHOMOSAPIENS, RAND1, RAND),

  statistics,
  statistics_jit.

% ------------------------------- %

init_fasta(ALU, IUB, HOMOSAP, RAND) :-
  ALU = 'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA',
  IUB = [a:0.27, c:0.12, g:0.12, t:0.27,
		'B':0.02, 'D':0.02, 'H':0.02, 'K':0.02, 'M':0.02,
		'N':0.02, 'R':0.02, 'S':0.02, 'V':0.02, 'W':0.02, 'Y':0.02],
  HOMOSAP = [a:0.3029549426680, c:0.1979883004921,
			g:0.1975473066391, t:0.3015094502008],
  init_gen_random(42, RAND).

% ------------------------------- %

repeat_fasta(Id, Desc, N, ALU) :-
  LineLength = 60,
  atom_length(ALU, ALULength),
  write('>'), write(Id), tab(1), write(Desc), nl,
  repeat_fasta_(N, 0, LineLength, ALU, ALULength).

% ------------- %

repeat_fasta_(N, _, _, _, _) :- N =< 0, !.

repeat_fasta_(N, Q, L, ALU, ALULength) :-
  (N < L -> L1 = N ; L1 = L),
  (L1 + Q < ALULength ->
    sub_atom(ALU, Q, L1, Lineout), Q1 is L1 + Q,
    write(Lineout), nl
  ;
    Rest is ALULength - Q, sub_atom(ALU, Q, Rest, Prefix),
    atom_length(Prefix, PrefixLength), Q1 is L1 - PrefixLength,
    sub_atom(ALU, 0, Q1, Segment),
	write(Prefix), write(Segment), nl),

  N1 is N - L1, !, repeat_fasta_(N1, Q1, L1, ALU, ALULength).

% ------------------------------- %

random_fasta(Id, Desc, N, CumTbl, RAND0, RAND) :-
  LineLength = 60,
  write('>'), write(Id), tab(1), write(Desc), nl,
  random_fasta_(N, LineLength, CumTbl, RAND0, RAND).

% ------------- %

random_fasta_(N, _, _, RAND, RAND) :- N =< 0, !.

random_fasta_(N, L, CumTbl, RAND0, RAND) :-
  (N < L -> L1 = N ; L1 = L),
  gen_line(L1, CumTbl, Codesout, RAND0, RAND1),
  atom_chars(Lineout, Codesout), write(Lineout), nl,
  N1 is N - L1, !, random_fasta_(N1, L1, CumTbl, RAND1, RAND).

% ------------- %

gen_line(0, _, [], RAND, RAND).
gen_line(N, CumTbl, K, RAND0, RAND) :-
  select_random(CumTbl, C, RAND0, RAND1),
  char_code(C, C1), K = [C1|T1], N1 is N - 1, !,
  gen_line(N1, CumTbl, T1, RAND1, RAND).

% ------------------------------- %

make_cumulative(L, RL) :- make_cumulative_(L, RL, 0).

make_cumulative_([], [], _) :- !.
make_cumulative_([K:V|T], L, CV) :-
	CV1 is CV + V, L = [K:CV1|T1], !, make_cumulative_(T, T1, CV1).

% ------------- %

select_random(L, RK, RAND0, RAND) :-
	gen_random(1.0, R, RAND0, RAND),
	select_random_(L, R, RK).

select_random_([], _, _) :- !.
select_random_([K:V|T], R, RK) :-
	(R < V -> RK = K ; !, select_random_(T, R, RK)).

% ------------------------------- %

init_gen_random(Seed, [3877, 29573, 139968, Seed]).

% ------------- %

gen_random(UB, R, RAND0, RAND) :-
  RAND0 = [IA, IC, IM, LAST],
  LAST1 is (LAST * IA + IC) mod IM,
  RAND = [IA, IC, IM, LAST1],
  R is UB * LAST1 / IM.

% ------------------------------- %
% BUG FIX - sub_atom/5 errors out if Size = 0.

sub_atom(_,_,0,'') :- !.
sub_atom(A,Bef,Size,Aout) :- sub_atom(A,Bef,Size,_,Aout).

