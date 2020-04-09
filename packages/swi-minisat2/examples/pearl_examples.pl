
:- use_module(library(cnf)).
:- use_module(library(minisat)).
:- use_module(adder).

%% Example, pg3
%%
%%   ?- cnf(X==Y,Cnf).
%%   Cnf = [[T], [-X, Y, -T], [X, -Y, -T]] 
%%   ?- cnf((X*Y)+(-X*Z),Cnf).
%%   Cnf = [[T], [-T, T1, T2], [-T2, -X], 
%%          [-T2, Z], [-T1, X], [-T1, Y]] 


%% Example, pg4
%%
%%   ?- sum([X+Y,X*Y,X==Y,X xor Y],[S1,S2,S3],Psi).
%%   Psi = (T1==X+Y)*(T2==(X==Y))*(T3==T1 xor T2)*(T4==T1*T2)*(T5==X*Y)* 
%%      (T6==X xor Y)*(T7==T5 xor T6)* (T8==T5*T6)*(S1==T3 xor T7)*
%%      (S2==T4 xor T8 xor (T3*T7))* (S3==(T3*T7->T4+T8;T4*T8)) 


%% Examples, pg6
%%
%% ?- cnf(X==Y,Cnf), solve(Cnf).
%% X=0, Y=0
%%
%% ?- cnf(X==Y,Cnf), sat(Cnf).
%% Yes
%%
%% ?- sum([X+Y,X*Y,X==Y,X xor Y],Sum,F), cnf(F,Cnf), solve(Cnf).
%% X = 0, Y = 0
%% Sum = [1, 0, 0]
%%
%% ?- sum([X+Y,X*Y,X==Y,X xor Y],[0,1,0],F), cnf(F,Cnf), solve(Cnf).
%% X = 0, Y = 1
%%
%% ?- sum([X+Y,X*Y,X==Y,X xor Y],Sum,F), cnf(F,Cnf),
%%    maximize(Sum,Cnf), solve(Cnf).
%%  Sum=[1,1,0]
%%  X=1, Y=1


%% Figure 3, pg 6
%%
%% partialMaxSat(+,+).
partialMaxSat(Phi,Psi) :-
	sum(Psi,Max,SumPsi), cnf(Phi*SumPsi,Cnf), 
	maximize(Max,Cnf),   solve(Cnf).

%% Example, pg 7
%%
%%  ?- partialMaxSat(X+Y,[X*Y,X==Y,X xor Y,-X+Y, -X, -Y, X]).
%%  X = 1, Y = 1
