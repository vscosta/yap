:- module(line_utils,
	  [search_for/3,
	   scan_natural/3,
	   scan_integer/3,
	   split/3
	  ]).

:- use_module(library(lists),
	      [member/2]).


search_for(C) --> [C], !.
search_for(C) --> [_],
	search_for(C).

scan_integer(N) -->
	"-", !,
	scan_natural(0, N0),
	N is -N0.
scan_integer(N) -->
	scan_natural(0, N).

scan_natural(N0,N) -->
	[C],
	{C >= 0'0, C =< 0'9 }, !,
	{ N1 is N0*10+(C-0'0) },
	get_natural(N1,N).
scan_natural(N,N) --> [].

split(String, SplitCodes, [S|Strings]) :-
	split(SplitCodes, S, Strings, String, []).

split(SplitCodes, [], [New|Set]) -->
	[C],
	{ member(C, SplitCodes) }, !,
	split(SplitCodes, New, Set).
split(SplitCodes, [C|New], Set) -->
	[C], !,
	split(SplitCodes, New, Set).
split(_, [], []) --> [].

