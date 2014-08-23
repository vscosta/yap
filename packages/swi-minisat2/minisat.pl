%%============================================================================ 
%% The SWI-Prolog interface to MiniSat SAT solver
%% http://www.cs.chalmers.se/Cs/Research/FormalMethods/MiniSat/MiniSat.html
%%
%% Copyright (c) 2006, Michael Codish, Vitaly Lagoon, and Peter J. Stuckey
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

:- module(minisat,[
	solve/1,
	sat/1,
	sat_init/0,
	sat_deinit/0,
	sat_add_clauses/3,
	sat_solve/1,

	minimize/2,
	maximize/2,
	minimize_v1/2,
	maximize_v1/2,
	minimize_v2/2,
	maximize_v2/2,
	minimize_v3/2,
	maximize_v3/2
]).


:- use_module(library(lists)).

:- load_foreign_library('pl-minisat',install).

:- dynamic tmp/1.



%%
%%
sat_init :-
	minisat_new_solver,             % - create the new solve
	minisat_add_clause([-1]),       % - add zero
	minisat_add_clause([2]).        % - add one

%%
%%
sat_deinit :-
	minisat_delete_solver.



%%
%%
sat_add_clauses(Cs, Vs, MiniSat_Vs) :-
	term_variables(Cs,CsVars),        
	\+ \+ ( 
		  bind2index(CsVars),    
		  add_cnf_clauses(Cs), 
		  asserta(tmp(Vs))
	      ),
	      retract(tmp(MiniSat_Vs)).

add_cnf_clauses([]).
add_cnf_clauses([Cl|Cls]) :-
	to_minisat(Cl,MiniSatCl),
	minisat_add_clause(MiniSatCl),
	add_cnf_clauses(Cls).

to_minisat([],[]).
to_minisat([L|Ls],[N|Ns])  :- 
	minisat_aux(L,N), 
	to_minisat(Ls,Ns).

minisat_aux(0,1)    :- !.
minisat_aux(1,2)    :- !.
minisat_aux(-(1),1) :- !.
minisat_aux(-(0),2) :- !.
minisat_aux(N,NN)   :- NN is N.


bind2index(Vs) :-
	minisat_nvars(N),
	N1 is N+1,
	bind2index_aux(Vs,N1).

bind2index_aux([],_N).
bind2index_aux([V|Ns],N) :- 
	var(V),
	!,
	V=N,
	N1 is N+1, bind2index_aux(Ns,N1).
bind2index_aux([V|Ns],N) :- 
	integer(V),
	bind2index_aux(Ns,N).

%%
%%
sat_solve(As) :-
	to_minisat(As,MINISAT_As),
	minisat_solve(MINISAT_As).


%%
%%
sat_get_values([],[]).
sat_get_values([SAT_V|SVs],[PL_V|PL_Vs]) :-
	minisat_get_var_assignment(SAT_V,N),
	( N<0 -> PL_V=0 ; PL_V=1),
	sat_get_values(SVs,PL_Vs).




%%
%% sat(+CNF): succeds if CNF is satisfaiable, it does not bind the variables in CNF 
%%

sat(CNF) :-
	sat_init,
	sat_add_clauses(CNF,_,_),
	sat_solve([]),
	sat_deinit, 
	!.
sat(_CNF) :- 
	sat_deinit,
	!,
	fail.


%%
%% solve(+CNF): like CNF/1 but bind the variables of CNF to the solution
%%
solve(CNF) :-
	sat_init,
	term_variables(CNF,CNF_Vs),
	sat_add_clauses(CNF,CNF_Vs,SAT_Vs),
	sat_solve([]),
	sat_get_values(SAT_Vs,CNF_Vs),
	sat_deinit, 
	!.
solve(_) :- 
	sat_deinit,
	!,
	fail.


%%
%%
%%
minimize(Vec,CNF) :- minimize_v1(Vec,CNF).
maximize(Vec,CNF) :- maximize_v1(Vec,CNF).


%%
%%
minimize_v1(Vec,CNF) :- 
	minimize_v1_aux(Vec,CNF), 
	sat(CNF).

minimize_v1_aux([],_CNF).
minimize_v1_aux([B|Bs],CNF) :-
	minimize_v1_aux(Bs,CNF),
	( (B=0, sat(CNF)) -> true ; B=1 ).


%%
%%
maximize_v1(Vec,CNF) :- 
	maximize_v1_aux(Vec,CNF), 
	sat(CNF).

maximize_v1_aux([],_CNF).
maximize_v1_aux([B|Bs],CNF) :-
	maximize_v1_aux(Bs,CNF),
	( (B=1, sat(CNF)) -> true ; B=0 ).




%%
%%
minimize_v2(Vec,CNF) :-
	retractall(tmp(_)),
	reverse(Vec,Vec_MSB),
	term_variables(CNF,CNF_Vars),
	sat_init,
	sat_add_clauses(CNF,[Vec_MSB,CNF_Vars],[Vec_MSB_SVars,CNF_SVars]),
	minimize_v2_loop(Vec_MSB_SVars),
	sat_get_values(CNF_SVars,CNF_Vars),
	sat_deinit,
	!.
	
minimize_v2_loop([]) :- 
	sat_solve([]).
minimize_v2_loop([V|Vs]) :-
	( sat_solve([-V]) ->
	    eliminate_prefix(Vs,0,New_Vs)
	;
	    sat_add_clauses([[V]],_,_),
	    New_Vs=Vs
	),
	minimize_v2_loop(New_Vs).



%%
%%
maximize_v2(Vec,CNF) :-
	reverse(Vec,Vec_MSB),
	term_variables(CNF,CNF_Vars),
	sat_init,
	sat_add_clauses(CNF,[Vec_MSB,CNF_Vars],[Vec_MSB_SVars,CNF_SVars]),
	maximize_v2_loop(Vec_MSB_SVars),
	sat_get_values(CNF_SVars,CNF_Vars),
	sat_deinit,
	!.
	
maximize_v2_loop([]) :- 
	sat_solve([]).
maximize_v2_loop([V|Vs]) :-
	( sat_solve([V]) ->
	    eliminate_prefix(Vs,1,New_Vs)
	;
	    sat_add_clauses([[V]],_,_),
	    New_Vs=Vs
	),
	maximize_v2_loop(New_Vs).





%%
%%
minimize_v3(Vec,CNF) :-
	retractall(tmp(_)),
	term_variables(CNF,CNF_Vars),
	sat_init,
	sat_add_clauses(CNF,[Vec,CNF_Vars],[Vec_SVars,CNF_SVars]),
	sat_solve([]),
	sat_get_values(Vec_SVars,Curr_Min),
	sat_get_values(CNF_SVars,Curr_Sol),
	minimize_v3_loop(Vec_SVars,CNF_SVars,Curr_Min,Curr_Sol,Vec,CNF_Vars),
	minisat_delete_solver, 
	!.

minimize_v3_loop(Vec,CNF_SVars,Last_Min,_Last_Sol,Final_Min,Final_Sol) :-
	xs_gt_ys(Last_Min,Vec,CNF-[]),
	sat_add_clauses(CNF,_,_),
	sat_solve([]),
	sat_get_values(Vec,Curr_Min),
	sat_get_values(CNF_SVars,Curr_Sol),
	!,
	minimize_v3_loop(Vec,CNF_SVars,Curr_Min,Curr_Sol,Final_Min,Final_Sol).
minimize_v3_loop(_Vec,_CNF_SVars,Final_Min,Final_Sol,Final_Min,Final_Sol) :-
	!.



%%
%%
maximize_v3(Vec,CNF) :-
	retractall(tmp(_)),
	term_variables(CNF,CNF_Vars),
	sat_init,
	sat_add_clauses(CNF,[Vec,CNF_Vars],[Vec_SVars,CNF_SVars]),
	sat_solve([]),
	sat_get_values(Vec_SVars,Curr_Max),
	sat_get_values(CNF_SVars,Curr_Sol),
	maximize_v3_loop(Vec_SVars,CNF_SVars,Curr_Max,Curr_Sol,Vec,CNF_Vars),
	minisat_delete_solver, 
	!.

maximize_v3_loop(Vec,CNF_SVars,Last_Max,_Last_Sol,Final_Max,Final_Sol) :-
	xs_gt_ys(Vec,Last_Max,CNF-[]),
	sat_add_clauses(CNF,_,_),
	sat_solve([]),
	sat_get_values(Vec,Curr_Max),
	sat_get_values(CNF_SVars,Curr_Sol),
	!,
	maximize_v3_loop(Vec,CNF_SVars,Curr_Max,Curr_Sol,Final_Max,Final_Sol).
maximize_v3_loop(_Vec,_CNF_SVars,Final_Max,Final_Sol,Final_Max,Final_Sol) :-
	!.




%%
%%
eliminate_prefix([],_Bit,[]) :- !.
eliminate_prefix([V|Vs],Bit,New_Vs) :- 
	sat_get_values([V],[VVal]),
	VVal = Bit,
	( Bit = 0 -> sat_add_clauses([[-V]],_,_) ; sat_add_clauses([[V]],_,_) ),
	!,
	eliminate_prefix(Vs,Bit,New_Vs).
eliminate_prefix(Vs,Vs).



%%
%%
% B == (Xs = Ys)
xs_eq_ys([X],[Y],B,Cnf1-Cnf2) :- 
	!,
   eq(X,Y,B,Cnf1-Cnf2).
xs_eq_ys([X|Xs],[Y|Ys],B,Cnf1-Cnf4) :-
   eq(X,Y,B1,Cnf1-Cnf2),
   xs_eq_ys(Xs,Ys,B2,Cnf2-Cnf3),
   and(B1,B2,B,Cnf3-Cnf4).

%%
xs_gt_ys(Xs,Ys,[[B]|Cnf1]-Cnf2) :-
	xs_gt_ys(Xs,Ys,B,Cnf1-Cnf2).

%%
%%
xs_gt_ys([X],[Y],B,Cnf1-Cnf2) :- !,
   gt(X,Y,B,Cnf1-Cnf2).
xs_gt_ys(Xs,Ys,B, Cnf1-Cnf6) :-
   split(Xs,LoXs,HiXs),
   split(Ys,LoYs,HiYs),
   xs_gt_ys(HiXs,HiYs,B1,Cnf1-Cnf2),
   xs_eq_ys(HiXs,HiYs,B2,Cnf2-Cnf3),
   xs_gt_ys(LoXs,LoYs,B3,Cnf3-Cnf4),
   and(B2,B3,Tmp, Cnf4-Cnf5),
   or(B1,Tmp,B,Cnf5-Cnf6).

% Z == X > Y (equivalently Z == X * -Y)
gt(X,Y,Z,[[Z,-X,Y],[-Z,X],[-Z,-Y]|Cnf]-Cnf).

% Z == (X == Y)
eq(X,Y,Z, [[-Z,-X,Y],[-Z,X,-Y],
            [Z,X,Y],[Z,-X,-Y] | Cnf]-Cnf).

% Z == X or Y
or(X,Y,Z, [[Z,-X],[Z,-Y],[-Z,X,Y] | Cnf]-Cnf).

% Z == X and Y
and(X,Y,Z, [[Z,X,Y],[-Z,X],[-Z,Y] | Cnf]-Cnf).

%
split(Xs,As,Bs) :-
    length(Xs,N), M is N // 2,
    length(As,M), append(As,Bs,Xs).
