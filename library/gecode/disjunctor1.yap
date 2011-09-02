%% -*- prolog -*-
%%=============================================================================
%% Copyright (C) 2011 by Denys Duchier
%%
%% This program is free software: you can redistribute it and/or modify it
%% under the terms of the GNU Lesser General Public License as published by the
%% Free Software Foundation, either version 3 of the License, or (at your
%% option) any later version.
%% 
%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
%% more details.
%% 
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%=============================================================================

:- use_module(library(gecode)).

disjunctor1(X_,Y_) :-
	Space := space,
	[X,Y] := intvars(Space,2,0,3),
	D := disjunctor(Space),
	C1 := clause(D),
	[X1,Y1] := intvars(C1,2,0,3),
	C1 += rel(X1,'IRT_EQ',1),
	C1 += rel(Y1,'IRT_EQ',1),
	C1 += forward([X,Y],[X1,Y1]),
	C2 := clause(D),
	[X2,Y2] := intvars(C2,2,0,3),
	C2 += rel(X2,'IRT_EQ',2),
	C2 += rel(Y2,'IRT_EQ',2),
	C2 += forward([X,Y],[X2,Y2]),
	Space += branch([X,Y],'INT_VAR_SIZE_MIN','INT_VAL_MIN'),
	SolSpace := search(Space),
	[X_,Y_] := val(SolSpace,[X,Y]).
