
:- use_module( library(python) ).

:- := import( collections ).
:- := import( yap ).
:- e := yap.'YAPEngine'().

main :-
     system_predicate(N/A),
     args(0,A,L),
     N := namedtuple( N, L),
     fail.
main :-
     := e.call( writeln( 1 ) ).

args(N, N, []) :- !.
args(I0,IF,[AI|Ais]) :-
	I is I0+1,
	number_string(I, IS),
	string_concat("A", IS, AI),
	args(I, IF, Ais). 
