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
:- use_module(library(maplist)).

% use alldiff constraints
queens(N, Solution) :-
	Space := space,
	length(Queens, N),
	Queens := intvars(Space,N,1,N),
	Space += distinct(Queens),
	foldl(inc, Queens, Inc, 0, _),
	foldl(dec, Queens, Dec, 0, _),
	Space += distinct(Inc,Queens),
	Space += distinct(Dec,Queens),
	Space += branch(Queens, 'INT_VAR_SIZE_MIN', 'INT_VAL_MIN'),
	SolSpace := search(Space),
	Solution := val(SolSpace,Queens).

inc(_, I0, I0, I) :-
	I is I0+1.

dec(_, I0, I0, I) :-
	I is I0-1.

%
% Using gecode linear constraints for diagonals.
%
lqueens(N, Solution) :-
	Space := space,
	length(Queens, N),
	Queens := intvars(Space,N,1,N),
	Space += distinct(Queens),
	lconstrain( Queens, Space, 0),
	Space += branch(Queens, 'INT_VAR_SIZE_MIN', 'INT_VAL_MIN'),
	SolSpace := search(Space),
	Solution := val(SolSpace,Queens).

lconstrain([], _, _).
lconstrain( [Q|Queens], Space, I0) :-
	I is I0+1,
	foldl(constrain(Q, I0, Space), Queens, I, _),
	lconstrain( Queens, Space, I).

constrain(Q, I, Space, R, J, J1) :-
	% Q+I != R+J, Q-I != R-J <=> Q-R != J-I, Q-R != I-J,
	J1 is J+1,
	Sum is I-J,
	Diff is J-I,
	Space += linear([1,-1], [Q,R], 'IRT_NQ', Diff),
	Space += linear([1,-1], [Q,R], 'IRT_NQ', Sum).
