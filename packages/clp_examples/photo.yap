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

:- use_module(library(gecode/clpfd)).
:- use_module(library(maplist)).

main :- ex(Ex, People, Names, _Preferences),
	photo(Ex, People, Amount ),
	format( 'Example ~a: ~w~n', [Ex, Amount]),
	maplist(join, People, Names, PeopleNames),
	keysort( PeopleNames, SortedPeopleNames),
	maplist(join, _People, SortedNames, SortedPeopleNames),
	maplist(output, SortedNames ),
	fail.
main.

join( Key, El, Key-El ).

output( Name ) :- format('   ~a~n', [Name]).

% 5 people want to have a photograph together, but they have preferences.
photo(Ex, People, Amount) :-
	ex(Ex, People, _, Preferences),
	length(People, Len),
	Len0 is Len-1,
	People ins 0..Len0,
	all_distinct(People),
	% Bools are the satisfied constraints
	maplist(preference_satisfied, Preferences, Bools),
	length(Preferences, PLen),
	Amount in 0..PLen,
	sum( Bools ) #= Amount,
	% add all satisfied constraints
	maximize(Amount),
	labeling([], People).

%reification, use with care
preference_satisfied(X-Y, B) :-
	abs(X - Y) #= 1 #<==> B.

ex(s,[Alice,Bob,Carl,Deb,Evan],
   ['Alice','Bob','Carl','Deb','Evan'],
   [Alice-Carl,
		Carl-Deb,
		Deb-Alice,
		Evan-Alice,
		Bob-Evan,
		Carl-Evan,
		Deb-Evan,
		Evan-Bob]).

ex(l,[Betty,Chris,Donald,Fred,Gary,Mary,Paul,Peter,Susan],
    ['Betty','Chris','Donald','Fred','Gary','Mary','Paul','Peter','Susan'],
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
