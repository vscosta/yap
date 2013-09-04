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

% 5 people want to have a photograph together, but they have preferences.
photo(Ex, Solution,Amount) :-
	db(Ex, People, Preferences),
	Space := space,
	length(People, Len),
	Len0 is Len-1,
	People := intvars(Space,Len,0,Len0),
	Space += distinct(People),
	% Bools are the satisfied constraints
	maplist(preferences(Space, Len), Preferences, Bools),
	length(Preferences, PLen),
	Sum := intvar(Space,0,PLen),
	Space += linear(Bools,'IRT_EQ',Sum),
	% add all satisfied constraints
	Space += maximize(Sum),
	Space += branch(People,'INT_VAR_SIZE_MIN','INT_VAL_MIN'),
	SolSpace := search(Space),
	Solution := val(SolSpace,People),
	Amount := val(SolSpace,Sum).

%reification, use with care
preferences(Space, Len, X-Y, B) :-
	NLen is -Len,
	I0 := intvar(Space, NLen, Len),
	I := intvar(Space, 0, Len),
	B := boolvar(Space),
	Space += reify(B, 'RM_EQV', R),
	Space += linear([1,-1],[X,Y],'IRT_EQ', I0),
	Space += abs(I0, I),
	Space += rel(I, 'IRT_EQ', 1, R).


db(s,[Alice,Bob,Carl,Deb,Evan], [Alice-Carl,
		Carl-Deb,
		Deb-Alice,
		Evan-Alice,
		Bob-Evan,
		Carl-Evan,
		Deb-Evan,
		Evan-Bob]).

db(l,[Betty,Chris,Donald,Fred,Gary,Mary,Paul,Peter,Susan],
	[Betty-Donald,
	 Betty-Gary,
	 Betty-Peter,
	 Chris-Gary,
	 Chris-Susan,
	 Donald-Fred,
	 Donald-Gary,
	 Fred-Betty,
	 Fred-Gary,
	 Gary-Mary,
	 Gary-Betty,
	 Mary-Betty,
	 Mary-Susan,
	 Paul-Donald,
	 Paul-Peter,
	 Peter-Susan,
	 Peter-Paul]).
