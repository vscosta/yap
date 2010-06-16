%   memberchk(+Element, +Set)
%   means the same thing, but may only be used to test whether a known
%   Element occurs in a known Set.  In return for this limited use, it
%   is more efficient when it is applicable.

lists:memberchk(X,[X|_]) :- !.
lists:memberchk(X,[_|L]) :-
	lists:memberchk(X,L).

%   member(?Element, ?Set)
%   is true when Set is a list, and Element occurs in it.  It may be used
%   to test for an element or to enumerate all the elements by backtracking.
%   Indeed, it may be used to generate the Set!

lists:member(X,[X|_]).
lists:member(X,[_|L]) :-
	lists:member(X,L).

lists:append([], L, L).
lists:append([H|T], L, [H|R]) :-
	lists:append(T, L, R).



