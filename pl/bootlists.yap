/**
 * @file   bootlists.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 09:54:00 2015
 *
 * @addtogroup lists
 * @{
*/

:- set_prolog_flag(source, true). % source.

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

%%  member(?Element, ?Set)
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

%% @pred  identical_member(?Element, ?Set) is nondet
%
% identical_member holds true when Set is a list, and Element is
% exactly identical to one of the elements that occurs in it.

lists:identical_member(X,[Y|M]) :-
       (
        X == Y
       ;
        M \= [], lists:identical_member(X,M)
       ).

/**  @pred append(? _List1_,? _List2_,? _List3_)


Succeeds when  _List3_ unifies with the concatenation of  _List1_
and  _List2_. The predicate can be used with any instantiation
pattern (even three variables).


*/
lists:append([], L, L).
lists:append([H|T], L, [H|R]) :-
       lists:append(T, L, R).


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
    ( Head \= Elem
    ->
    lists:delete(List, Elem, Residue)
    ;
    Residue = [Head|MoreResidue],
    lists:delete(List, Elem, MoreResidue)
    ).




%   length of a list.

:- set_prolog_flag(source, false). % disable source.

/** @pred  length(? _L_,? _S_)


Unify the well-defined list  _L_ with its length. The procedure can
be used to find the length of a pre-defined list, or to build a list
of length  _S_.

*/

prolog:length(L, M) :-
    '$skip_list'(L, M, M0, R),
         ( var(R) -> '$$_length'(R, M, M0) ;
           R == []
         ).

%
% in case A1 is unbound or a difference list, things get tricky
%
'$$_length'(R, M, M0) :-
       ( var(M) -> '$$_length1'(R,M,M0)
       ; M >= M0 -> '$$_length2'(R,M,M0) ).

%
% Size is unbound, generate lists
%
'$$_length1'([], M, M).
'$$_length1'([_|L], O, N) :-
       M is N + 1,
       '$$_length1'(L, O, M).

%
% Size is bound, generate single list
%
'$$_length2'(NL, O, N) :-
       ( N =:= O -> NL = [];
          M is N + 1, NL  = [_|L], '$$_length2'(L, O, M) ).

prolog:'$foldl'(Goal, List, V0, V) :-
    '$foldl_'(List, Goal, V0, V).

'$foldl_'([], _, V, V).
'$foldl_'([H|T], Goal, V0, V) :-
    call(Goal, H, V0, V1),
    '$foldl_'(T, Goal, V1, V).


%% @}
