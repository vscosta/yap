% Example with matrices,based on:
%
% Three jugs problem in Minzinc modelled as a shortest path problem.
%
% Problem from Taha "Introduction to Operations Research", page 245
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

%
% VSC: had to transpose the matrix, and change the constraints....
%

:- style_check( all ).

:- use_module(library(gecode/clpfd)).
:- use_module(library(maplist)).
:- use_module(library(lists)).

main :-
	problem(Z, X, InFlow, OutFlow, N),
	out(Z, X, InFlow, OutFlow, N),
	fail.
main.

problem(Z, X, InFlow, OutFlow, N) :-
	N = 15,
	Start = 1,
	End = 15,
	M = 999,
	d( M, DD ),
	D <== array[1..N,1..N] of DD, % distance
	RHS <== array[1..N] of _, % requirements (right hand statement)
	X <== array[1..N, 1..N] of 0..1, % the resulting matrix, 1 if connected, 0 else
	OutFlow <== array[1..N] of 0..1,
	InFlow <== array[1..N] of 0..1,

% objective to minimize
	Z in 0..M,
	Z #= sum( [I in 1..N, J in 1..N] where D[I,J]<M,
		  D[I,J]*X[I,J]),

% solve minimize z;
% alternative solve statements which may give faster solution
%solve :: int_search([ x[i,j] | i,j in 1..n], first_fail, indomain_min, complete) minimize z;
% solve minimize z;
	minimize(Z),
	

% constraint 
	foreach(I in 1..N,
	    ( I == Start ->
	      RHS[I] <== 1 ;
	      I == End ->
	      RHS[I] <== -1 ;
	      RHS[I] <== 0 )
	   ),


    % must be larger than 0??
   foreach( [I in 1..N, J in 1..N],
	( D[J,I] = M ->
	  X[J,I] #= 0 ;
	  true )
      ),
				% outflow constraint
   foreach(I in 1..N,
       OutFlow[I] #= sum(J in 1..N where D[J,I]<M, X[J,I])
      ),
   % inflow constraint
  foreach(J in 1..N,
      InFlow[J] #= sum(I in 1..N where D[J,I]<M, X[J,I])
     ),
   % inflow = outflow
  foreach(I in 1..N,  OutFlow[I]-InFlow[I]#=RHS[I]),

  % labeling
  labeling( [], X).

% data
d(M, [
  M, 1, M, M, M, M, M, M, 1, M, M, M, M, M, M,
  M, M, 1, M, M, M, M, M, M, M, M, M, M, M, M,
  M, M, M, 1, M, M, M, M, 1, M, M, M, M, M, M,
  M, M, M, M, 1, M, M, M, M, M, M, M, M, M, M,
  M, M, M, M, M, 1, M, M, 1, M, M, M, M, M, M,
  M, M, M, M, M, M, 1, M, M, M, M, M, M, M, M,
  M, M, M, M, M, M, M, 1, 1, M, M, M, M, M, M,
  M, M, M, M, M, M, M, M, M, M, M, M, M, M, 1, 
  M, M, M, M, M, M, M, M, M, 1, M, M, M, M, M,
  M, 1, M, M, M, M, M, M, M, M, 1, M, M, M, M,
  M, M, M, M, M, M, M, M, M, M, M, 1, M, M, M,
  M, 1, M, M, M, M, M, M, M, M, M, M, 1, M, M,
  M, M, M, M, M, M, M, M, M, M, M, M, M, 1, M,
  M, 1, M, M, M, M, M, M, M, M, M, M, M, M, 1, 
  M, M, M, M, M, M, M, M, M, M, M, M, M, M, M
]).

/*
% shows the result matrix
output [
       if i = 1 /\ j = 1 then 
         "z: " ++ show(z) ++ "\n" ++
         "inFlow:  " ++ show(inFlow) ++ "\n" ++ "outFlow: " ++ show(outFlow) ++ "\n" ++
         "    1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n" 
       else "" endif ++
       if j = 1 then show(i) ++ " : " else "" endif ++
       show(x[i,j]) ++ if j = n then "\n" else " " endif
       | i in 1..n, j in 1..n
];

*/

out(Cost, Ts, Ins, Out, N) :-
	format('cost = ~d~n', [Cost]),
	InsL <== list(Ins),
	OutL <== list(Out),
	format('Inputs  =', []), maplist(out, InsL), nl,
	format('Outputs =', []), maplist(out, OutL), nl,
	format('transitions =~n', []),
	foreach(I in 1..N, outl(Ts[_,I]) ).

outl( X ) :-
	L <== X, % evaluate matrix notation to Prolog lists.
	format('  ', []),
	maplist(out, L), nl.

out(0) :- format(' .', []).
out(1) :- format(' 1', []).
