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
:- source.

:- use_module(library(maplist)).
:- use_module(library(lists)).
:- use_module(library(matrix)).
:- use_module(library(gecode/clpfd)).

main :-
    gecode_clpfd:init_gecode(Space, new),
    
    problem(Z, X, InFlow, OutFlow),
      term_variables(Z+X+InFlow+OutFlow,Vs),
 gecode_clpfd:close_gecode(Space, Vs, new),
    out(Z, X, InFlow, OutFlow).

'..'(A,B,V) :-
    V in A..B.

problem(Z, X, InFlow, OutFlow) :-
    N=15,
    M=99,
    d(M, DD ),
    D <==  DD.transpose(), % distance
    RHS <== zeros[N], % requirements (right hand statement)
    RHS[0] <== 1,
   N1 is N-1,
    RHS[N1] <== -1,
    X <==  matrix([N,N]) of (0..1), % the resulting matrix, 1 if connected, 0 else
    OutFlow <== matrix([N]) of 0..1,
    InFlow <==  matrix([N]) of 0..1,
    

% constraint 
    % must be larger than 0??
% objective to minimize
    Z in 0..M,
    matrix_foldl(dx(M), D, X,[], Els),
    sum(Els) #= Z,    
     matrix_map( inflow(M,D,X),InFlow),
   matrix_map( outflow(M,D,X),OutFlow),
    matrix_map(z,X), 
      matrix_map(oi,RHS,OutFlow,InFlow),
   % solve minimize z;
% alternative solve statements which may give faster solution
%solve :: int_search([ x[i,j] | i,j in 1..n], first_fail, indomain_min, complete) minimize z;
% solve minimize z;
    minimize(Z),
    labeling( [], X).


    
dx(M,D,X,Els0,Els,[I,J]) :-
  DIJ <== D[I,J],
  XIJ <== X[I,J],
    ( DIJ == 1 ->
      Els = [XIJ|Els0] ;
      DIJ < M ->
      Els = [DIJ*XIJ|Els0] ;
      Els = Els0
    ).

inflow(M,D,X,InFlow,[I]) :-
    matrix_foldl(ssm(I,M,D,X),InFlow,[],Els),
    In <== InFlow[I],
    (Els == [] -> true ; In #= sum(Els) ).

ssm(I,M,D,X,_,S0,[E|S0],[J]):-
    DIJ <== D[I,J],
    DIJ < M,
    !,
    E <== X[I,J].
ssm(_,_M,_D,_X,_,S0,S0,_J).

outflow(M,D,X,OutFlow,[I]) :-
    matrix_foldl(ssmt(I,M,D,X),OutFlow,[],Els),
    Oi <==    OutFlow[I],
 (Els == [] -> true ; Oi #= sum(Els) ).

ssmt(J,M,D,X,_,S0,[E|S0],[I]) :-
    DIJ <== D[I,J],
    DIJ < M,
    !,
    E <== X[I,J].
ssmt(_,_M,_D,_X,_,S0,S0,_I).


oi(RHS,O,I,[K]) :-
    A <== RHS[K],
    B <== O[K],
    C <== I[K],
    A #= B-C.

z(X,[I,I]) :-
    !,
    XI <== X[I,I],
    XI #= 0.
z(_,_).


/*				% inflow constraint
   foreach(I in 1..N,
       InFlow[I] #= sum(J in 1..N where D[J,I]<M, X[J,I])
      ),
   % inflow constraint
  foreach(J in 1..N,
      InFlow[J] #= sum(I in 1..N where D[J,I]<M, X[J,I])
     ),
   % inflow = inflow
  foreach(I in 1..N,  InFlow[I]-InFlow[I]#=RHS[I]),
*/

% data
d(M, [
 [  M, 1, M, M, M, M, M, M, 1, M, M, M, M, M, M ],
 [  M, M, 1, M, M, M, M, M, M, M, M, M, M, M, M ],
 [  M, M, M, 1, M, M, M, M, 1, M, M, M, M, M, M ],
 [  M, M, M, M, 1, M, M, M, M, M, M, M, M, M, M ],
 [  M, M, M, M, M, 1, M, M, 1, M, M, M, M, M, M ],
 [  M, M, M, M, M, M, 1, M, M, M, M, M, M, M, M ],
 [  M, M, M, M, M, M, M, 1, 1, M, M, M, M, M, M ],
 [  M, M, M, M, M, M, M, M, M, M, M, M, M, M, 1 ], 
 [  M, M, M, M, M, M, M, M, M, 1, M, M, M, M, M ],
 [  M, 1, M, M, M, M, M, M, M, M, 1, M, M, M, M ],
 [  M, M, M, M, M, M, M, M, M, M, M, 1, M, M, M ],
 [  M, 1, M, M, M, M, M, M, M, M, M, M, 1, M, M ],
 [  M, M, M, M, M, M, M, M, M, M, M, M, M, 1, M ],
 [  M, 1, M, M, M, M, M, M, M, M, M, M, M, M, 1 ], 
 [  M, M, M, M, M, M, M, M, M, M, M, M, M, M, M ]
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

out(Cost, Ts, Ins, Out) :-
	format('cost = ~d~n', [Cost]),
	InsL <== Ins.list(),
	OutL <== Out.list(),
	format('Inputs  =', []), maplist(out, InsL), nl,
	format('Outputs =', []), maplist(out, OutL), nl,
	format('transitions =~n', []),
    matrix_map( outm, Ts).


outm( Ts, [I,J] ) :-
    V <== Ts[I,J],
    V==1,
    !,
    format('~d -> ~d; ',[I,J]).
outm( _Ts, _ ).


out(0) :- format('.', []).
out(1) :- format(' 1', []).


