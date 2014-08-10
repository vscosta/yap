% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% contributed by Anthony Borla
% modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(assoc)).

:- initialization(main).

main :-
   current_input(Cin),
   load_sequence(Cin, Seq),

   FragmentLengths = [1, 2],
   forall(member(E, FragmentLengths), (print_frequencies(Seq, E), nl)),

   Fragments = ["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"],
   forall(member(E, Fragments), print_count(Seq, E)),

   statistics,
   statistics_jit.

% ------------------------------- %

print_frequencies(Seq, KeyLen) :-
   generate_counts(Seq, KeyLen, CountTable),
   sum_counts_(CountTable, 0, SumCounts),
   make_freq_table_(CountTable, SumCounts, [], FTable),
   keysort(FTable, SFTable), reverse(SFTable, FreqTable),
   print_freq_table_(FreqTable).

% ------------- %

sum_counts_([_-C|T], Acc, Sum) :- Acc1 is Acc + C, !, sum_counts_(T, Acc1, Sum).
sum_counts_([], Acc, Acc).

% ------------- %

make_freq_table_([K-C|T], SumCounts, FTA, FreqTable) :-
   F is C / SumCounts * 100.0, append([F-K], FTA, FTA1),
   !, make_freq_table_(T, SumCounts, FTA1, FreqTable).
make_freq_table_([], _, FTA, FTA).

% ------------- %

print_freq_table_([F-K|T]) :-
   format('~w ~3f\n', [K, F]),
   !, print_freq_table_(T).
print_freq_table_([]).

% ------------------------------- %

print_count(Seq, Fragment) :-
   length(Fragment, FragLen),
   generate_counts(Seq, FragLen, CountTable),
   atom_codes(FragKey, Fragment),
   (
      select(FragKey-Count, CountTable, _)
   ;
      Count = 0
   ), !,
   format('~d\t~s\n', [Count, Fragment]).

% ------------- %

generate_counts(Seq, Length, CountTable) :-
   length(Seq, SeqLen), Last is SeqLen - Length + 1,
   make_count_table(Length, Last, Seq, CountTable).

% ------------------------------- %

make_count_table(Length, Last, Seq, CountTable) :-
   empty_assoc(A),
   mct_i_loop_(0, Length, Last, Seq, A, ACT),
   assoc_to_list(ACT, CountTable).

% ------------- %

mct_i_loop_(I, Length, Last, Seq, CTA, CountTable) :-
   I < Length, !,
   mct_j_loop_(Last, Length, Seq, CTA, CTA1),
   I1 is I + 1, !,
   Seq = [_|Ss], Last1 is Last - 1,
   mct_i_loop_(I1, Length, Last1, Ss, CTA1, CountTable).
mct_i_loop_(Length, Length, _, _, CTA, CTA).


% ------------- %

mct_j_loop_(Last, Length, Seq, CTA, CountTable) :-
   Last > 0, !,
   sub_list_(Seq, Length, KeyString, Rest), atom_codes(Key, KeyString),
   (
      get_assoc(Key, CTA, Value) ->
      V1 is Value + 1, put_assoc(Key, CTA, V1, CTA1)
   ;
      put_assoc(Key, CTA, 1, CTA1)
   ),
   !, Last1 is Last - Length,
   mct_j_loop_(Last1, Length, Rest, CTA1, CountTable).
mct_j_loop_(Last, _, _, CTA, CTA) :- Last =< 0, !.

% ------------------------------- %

load_sequence(S, Seq) :- load_sequence_(S, fail, "", Seq).

% ------------- %

load_sequence_(S, Loading, Seq, RetSeq) :-
   catch(read_line_to_codes(S, L), _, fail), is_list(L), !,
   (
      Loading ->
      process_sequence(L, S, Seq, RetSeq)
   ;
      ignore_sequence(L, S, Seq, RetSeq)
   ).
load_sequence_(S, _, Seq, Seq).

% ------------- %

ignore_sequence([62,84,72,82,69,69|_], S, Seq, RetSeq) :- !,
   load_sequence_(S, true, Seq, RetSeq).
ignore_sequence(_, S, Seq, RetSeq) :- !,
   load_sequence_(S, fail, Seq, RetSeq).

process_sequence([62|_], _, Seq, Seq) :- !.
process_sequence([59|_], S, Seq, RetSeq) :- !,
   load_sequence_(S, true, Seq, RetSeq).

process_sequence(L, S, Seq, RetSeq) :-
   to_upper(L, UL),
   append(Seq, UL, NewSeq),
   !, load_sequence_(S, true, NewSeq, RetSeq).

% ------------------------------- %

to_upper(L, U) :- to_upper_(L, [], U).

% ------------- %

to_upper_([], UA, U) :- reverse(UA, U), !.

to_upper_([C|T], UA, U) :-
   is_lower(C), C1 is C - 32,
   !, to_upper_(T, [C1|UA], U).

to_upper_([C|T], UA, U) :-
   !, to_upper_(T, [C|UA], U).

% ------------- %

is_lower(C) :- C >= 97, C =< 122.

% ------------------------------- %

forall(Gen, Proc) :- findall(_,(Gen, Proc), _).

% ------------- %

sub_list_([S|Seq], L, [S|Ks], Rs) :- L > 0, !,
   L1 is L - 1,
   sub_list_(Seq, L1, Ks, Rs).
sub_list_(Rs, 0, [], Rs).

% ------------------------------- %
