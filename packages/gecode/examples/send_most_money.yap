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

%   S E N D
% + M O S T
% ---------
% M O N E Y
send_most_money(Solution,Amount) :-
	Space := space,
	Letters := intvars(Space,8,0,9),
	[S,E,N,D,M,O,T,Y] = Letters,
	Space += rel(M,'IRT_NQ',0),
	Space += rel(S,'IRT_NQ',0),
	Space += distinct(Letters),
	C = [1000, 100, 10, 1,
	     1000, 100, 10, 1,
	     -10000, -1000, -100, -10, -1],
	X = [S,E,N,D,
	     M,O,S,T,
	     M,O,N,E,Y],
	Space += linear(C, X, 'IRT_EQ', 0),
	Money := intvar(Space,0,99999),
	Space += linear([10000,1000,100,10,1],[M,O,N,E,Y],'IRT_EQ',Money),
	Space += maximize(Money),
	Space += branch(Letters,'INT_VAR_SIZE_MIN','INT_VAL_MIN'),
	SolSpace := search(Space),
	Solution := val(SolSpace,Letters),
	Amount := val(SolSpace,Money).
