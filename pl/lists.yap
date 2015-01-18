
:- system_module( '$_lists', [], []).

:- '$set_yap_flags'(11,1). % source.

%   memberchk(+Element, +Set)
%   means the same thing, but may only be used to test whether a known
%   Element occurs in a known Set.  In return for this limited use, it
%   is more efficient when it is applicable.

/** @pred memberchk(+ _Element_, + _Set_) 


As member/2, but may only be used to test whether a known
 _Element_ occurs in a known Set.  In return for this limited use, it
is more efficient when it is applicable.

 
*/
lists:memberchk(X,[X|_]) :- !.
lists:memberchk(X,[_|L]) :-
	lists:memberchk(X,L).

%   member(?Element, ?Set)
%   is true when Set is a list, and Element occurs in it.  It may be used
%   to test for an element or to enumerate all the elements by backtracking.
%   Indeed, it may be used to generate the Set!

/** @pred member(? _Element_, ? _Set_) 


True when  _Set_ is a list, and  _Element_ occurs in it.  It may be used
to test for an element or to enumerate all the elements by backtracking.

 
*/
lists:member(X,[X|_]).
lists:member(X,[_|L]) :-
	lists:member(X,L).

/**  @pred append(? _List1_,? _List2_,? _List3_) 


Succeeds when  _List3_ unifies with the concatenation of  _List1_
and  _List2_. The predicate can be used with any instantiation
pattern (even three variables).

 
*/
lists:append([], L, L).
lists:append([H|T], L, [H|R]) :-
	lists:append(T, L, R).


:- '$set_yap_flags'(11,0). % :- no_source.

%   lists:delete(List, Elem, Residue)
%   is true when List is a list, in which Elem may or may not occur, and
%   Residue is a copy of List with all elements identical to Elem lists:deleted.

/** @pred delete(+ _List_, ? _Element_, ? _Residue_) 


True when  _List_ is a list, in which  _Element_ may or may not
occur, and  _Residue_ is a copy of  _List_ with all elements
identical to  _Element_ deleted.

 
*/
lists:delete([], _, []).
lists:delete([Head|List], Elem, Residue) :-
	Head == Elem, !,
	lists:delete(List, Elem, Residue).
lists:delete([Head|List], Elem, [Head|Residue]) :-
	lists:delete(List, Elem, Residue).

:- '$set_yap_flags'(11,0). % disable source.

