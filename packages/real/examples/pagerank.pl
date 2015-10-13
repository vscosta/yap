:- use_module(library(real)).
%:-    r(install..packages("igraph")).
:-  r(library("igraph") ).

aleph :-
     pagerank('aleph.pl').

pagerank(F) :-
%     g <- graph([0,0],n=1000),
     parse(F,S),
     g <- graph(S),
     r <- page..rank(g),
     Scores <- r$vector,
     max_element(Scores, I, V),
     found(N, A, I),
     format('most linked predicate is ~a/~d, score ~2f.~n',[N,A,V]).

max_element(S, IF, VF) :-
    S = [V|Els],
    max_element(Els, V, 0, 0, IF, VF).

max_element([], VF, IM, _, IM, VF).
max_element([V|Els], VM, _IM, I0, IF, VF) :-
    V > VM, !,
    I is I0+1,
    max_element(Els, V, I0, I, IF, VF).
max_element([_|Els], VM, IM, I0, IF, VF) :-
    I is I0+1,
    max_element(Els, VM, IM, I, IF, VF).

parse(File, L) :-
     open(File, read, S),
     findall(O, process(S, O),L),
     close(S).

process(S, O) :-
     repeat,
     read(S, T),
%  writeln(T),
     (
       T == end_of_file -> !, fail ;
       do(T, O)
     ).

do((A :- B), O) :-
  id(A, IDA),
  process_body(B, IDA, O).
do((:- G),_) :- catch(call(G),_,fail), !, fail.

process_body(B, _IDA, _) :-  var(B), !, fail.
process_body((B1,B2), IDA,O) :- !,
  process_body(B1, IDA, O) ;
  process_body(B2, IDA, O).
process_body((B1;B2), IDA, O) :- !,
  process_body(B1, IDA, O) ;
  process_body(B2, IDA, O).
process_body((B1->B2), IDA, O) :- !,
  process_body(B1, IDA, O) ;
  process_body(B2, IDA, O).
process_body(B, IDA, O) :-
  id(B,IDB),
  ( O = IDB; O = IDA ).
%  r(add..edges(g,c(IDA,IDB))).

:- dynamic ids/1, found/3, exists/2.

new(IDA,IDB) :-
  \+ exists(IDA,IDB),
  assert(exists(IDA,IDB)).


id(F, I) :-
  functor(F,N,A),
  (found(N,A,I) -> true ; new(N, A, I) ).

ids(0).

new(N, A, I) :-
   retract(ids(I)),
   I1 is I+1,
   assert(found(N,A,I)),
   assert(ids(I1)).
