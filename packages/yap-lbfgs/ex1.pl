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



% This is the call back function which evaluates F and the gradient of F
evaluate(FX,X,G,_N,_Step,_User) :-
	X0 <== X[0],
	FX is sin(X0),
	G0 is cos(X0),
	G[0] <== G0.

% This is the call back function which is invoked to report the progress
% if the last argument is set to anything else than 0, the lbfgs will
% stop right now
progress(FX,X,G,X_Norm,G_Norm,Step,_N,Iteration,Ls,0) :-
	X0 <== X[0],
	format('~d. Iteration : x0=~4f  f(X)=~4f  |X|=~4f
                |X\'|=~4f  Step=~4f  Ls=~4f~n',
                [Iteration,X0,FX,X_Norm,G_Norm,Step,Ls]).



demo :-
	format('Optimizing the function f(x0) = sin(x0)~n',[]),
	lbfgs_initialize(1,X,FX,Solver),
	StartX is random*10,
	format('We start the search at the random position x0=~5f~2n',[StartX]),
	X[0] <== StartX,
	lbfgs_run(Solver,BestF),
	BestX0 <== X[0],
	lbfgs_finalize(Solver),
	format('~2nOptimization done~nWe found a minimum at
	f(~f)=~f~2nLBFGS Status=~w~n',[BestX0,BestF,Status]).
