%%% -*- Mode: Prolog; -*-


%  This file is part of YAP-LBFGS.
%  Copyright (C) 2009 Bernd Gutmann
%
%  YAP-LBFGS is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  YAP-LBFGS is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with YAP-LBFGS.  If not, see <http://www.gnu.org/licenses/>.

:- use_module(library(lbfgs)).
:- use_module(library(matrix)).

f(X0,X1,FX) :-
    FX is (X0-2)*(X0-2) + (X1-1)*(X1-1).

% This is the call back function which evaluates F and the gradient of F
evaluate(FX,X,G,_N,_Step,_U) :-
    X0 <== X[0],
    X1 <== X[1],
    f(X0,X1,FX),
	G0 is 2*(X0-2),
	G1 is 2*(X1-2),
	G[0]  <==  G0,
	G[1] <==  G1.
    
% This is the call back function which is invoked to report the progress
% if the last argument is set to anything else than 0, the optimizer will
% stop right now
progress(FX,X,_G,X_Norm,G_Norm,Step,_N,Iteration,Ls,0) :-
    X0 <== X[0],
    X1 <== X[1],
    format('~d. Iteration : (x0,x1)=(~4f,~4f)  f(X)=~4f  |X|=~4f  |X\'|=~4f  Step=~4f  Ls=~4f~n',[Iteration,X0,X1,FX,X_Norm,G_Norm,Step,Ls]).



demo :-
	format('Optimizing the function f(x0,x1) = (x0-2)^2 + (x1-1)^2~n',[]),

	lbfgs_initialize(2,X,0,Solver),


	StartX0 is random*1000-500,
	StartX1 is random*1000-500,
	
	format('We start the search at the random position (x0,x1)=(~5f,~5f)~2n',[StartX0,StartX1]),
	X[0] <== StartX0,
	X[1] <== StartX1,	
	lbfgs_run(Solver,BestF,Status),
	BestX0 <== X[0],
	BestX1 <== X[1],

	optimizer_finalize(Solver),
	format('~2nOptimization done~nWe found a minimum at f(~f,~f)=~f~2nLBFGS Status=~w~n',[BestX0,BestX1,BestF,Status]).


