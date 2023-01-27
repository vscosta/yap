/**
 * @file   bootlists.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 09:54:00 2015
 *
 * @addtogroup lists
 * @{
*/

:- system_module_( '$_lists', [], []).


%   memberchk(+Element, +Set)
%   means the same thing, but may only be used to test whether a known
%   Element occurs in a known Set.  In return for this limited use, it
%   is more efficient when it is applicable.
/** @pred memberchk(+ _Element_, + _Set_)


As member/2, but may only be used to test whether a known
 _Element_ occurs in a known Set.  In return for this limited use, it
is more efficient when it is applicable.


*/
'$memberchk'(X,[X|_]) :- !.
'$memberchk'(X,[_|L]) :-
       '$memberchk'(X,L).

%%  member(?Element, ?Set)
%   is true when Set is a list, and Element occurs in it.  It may be used
%   to test for an element or to enumerate all the elements by backtracking.
%   Indeed, it may be used to generate the Set!

/** @pred member(? _Element_, ? _Set_)


True when  _Set_ is a list, and  _Element_ occurs in it.  It may be used
to test for an element or to enumerate all the elements by backtracking.


*/
'$member'(X,[X|_]).
'$member'(X,[_|L]) :-
       '$member'(X,L).

%% @pred  identical_member(?Element, ?Set) is nondet
%
% identical_member holds true when Set is a list, and Element is
% exactly identical to one of the elements that occurs in it.

'$identical_member'(X,[Y|M]) :-
       (
        X == Y
       ;
        M \= [], '$identical_member'(X,M)
       ).

/**  @pred append(? _List1_,? _List2_,? _List3_)


Succeeds when  _List3_ unifies with the concatenation of  _List1_
and  _List2_. The predicate can be used with any instantiation
pattern (even three variables).


*/
'$append'([], L, L).
'$append'([H|T], L, [H|R]) :-
       '$append'(T, L, R).


:- set_prolog_flag(source, true). % :- no_source.

%   '$delete'(List, Elem, Residue)
%   is true when List is a list, in which Elem may or may not occur, and
%   Residue is a copy of List with all elements identical to Elem '$delete'd.

/** @pred delete(+ _List_, ? _Element_, ? _Residue_)

eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
True when  _List_ is a list, in which  _Element_ may or may not
occur, and  _Residue_ is a copy of  _List_ with all elements
identical to  _Element_ deleted.

eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
*/
'$delete'([], _, []).
'$delete'([Head|List], Elem, Residue) :-
       Head = Elem,
       '$delete'(List, Elem, Residue).
'$delete'([Head|List], Elem, [Head|Residue]) :-
       '$delete'(List, Elem, Residue).

:- set_prolog_flag(source, false). % disable source.



%   length of a list.

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

%% @}

