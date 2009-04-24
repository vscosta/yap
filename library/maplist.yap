%   Also has code from:
%   File   : APPLIC.PL
%   Author : Lawrence Byrd + Richard A. O'Keefe
%   Updated: 4 August 1984 and Ken Johnson 11-8-87
%   Purpose: Various "function" application routines based on apply/2.
%   Needs  : append/3 from listut.pl


%   File   : apply_macros.yap
%   Author : E. Alphonse from code by Joachim Schimpf, Jan Wielemaker, Vitor Santos Costa
%   Purpose: Macros to apply a predicate to all elements
%            of a list or to all sub-terms of a term.


:- module(maplist, [selectlist/3,
			 checklist/2,
			 maplist/2,			% :Goal, +List
			 maplist/3,			% :Goal, ?List1, ?List2
			 maplist/4,			% :Goal, ?List1, ?List2, ?List
			 maplist/5,			% :Goal, ?List1, ?List2, ?List3, List4
			 convlist/3,
			 mapargs/3,
			 sumargs/4,
			 mapnodes/3,
			 checknodes/2,
			 sumlist/4,
			 sumnodes/4,
			 include/3,
			 exclude/3,
			 partition/4,
			 partition/5			
			]).


:- meta_predicate
	selectlist(:,+,-),
	checklist(:,+),
	maplist(:,+),
	maplist(:,+,-),
	maplist(:,+,+,-),
	maplist(:,+,+,+,-),
	convlist(:,+,-),
	mapargs(:,+,-),
	mapargs_args(:,+,-,+),
	sumargs(:,+,+,-),
	sumargs_args(:,+,+,-,+),
	mapnodes(:,+,-),
	mapnodes_list(:,+,-),
	checknodes(:,+),
	checknodes_list(:,+),
	sumlist(:,+,+,-),
	sumnodes(:,+,+,-),
	sumnodes_body(:,+,+,-,+,+),
	include(:,+,-),
	exclude(:,+,-),
	partition(:,+,-,-),
	partition(:,+,-,-,-).
	
	
:- use_module(library(error), [must_be/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Definitions for Metacalls
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

include(G,In,Out) :-
	selectlist(G, In, Out).

selectlist(_, [], []).
selectlist(Pred, [In|ListIn], ListOut) :-
    (call(Pred, In) ->
	ListOut = [In|NewListOut]
    ;
	ListOut = NewListOut
    ),
    selectlist(Pred, ListIn, NewListOut).

exclude(_, [], []).
exclude(Pred, [In|ListIn], ListOut) :-
    (call(Pred, In) ->
	ListOut = NewListOut
    ;
	ListOut = [In|NewListOut]
    ),
    exclude(Pred, ListIn, NewListOut).

partition(_, [], [], []).
partition(Pred, [In|ListIn], List1, List2) :-
    (call(Pred, In) ->
	List1 = [In|RList1],
	List2 = RList2
    ;
	List1 = RList1,
	List2 = [In|RList2]
    ),
    partition(Pred, ListIn, RList1, RList2).

partition(_, [], [], [], []).
partition(Pred, [In|ListIn], List1, List2, List3) :-
    call(Pred, In, Diff),
    ( Diff == (<)  ->
	List1 = [In|RList1],
	List2 = RList2,
	List3 = RList3
    ;
       Diff == (=)  ->
      List1 = RList1,
      List2 = [In|RList2],
      List3 = RList3
    ;
       Diff == (>)  ->
      List1 = RList1,
      List2 = RList2,
      List3 = [In|RList3]
    ;
      must_be(oneof([<,=,>]), Diff)
    ),
    partition(Pred, ListIn, RList1, RList2, RList3).

checklist(_, []).
checklist(Pred, [In|ListIn]) :-
    call(Pred, In),
    checklist(Pred, ListIn).

%   maplist(Pred, OldList)
%   succeeds when Pred(Old,New) succeeds for each corresponding
%   Old in OldList, New in NewList.  In InterLisp, this is MAPCAR. 
%   It is also MAP2C.  Isn't bidirectionality wonderful?
maplist(_, []).
maplist(Pred, [In|ListIn]) :-
    call(Pred, In),
    maplist(Pred, ListIn).

%   maplist(Pred, OldList, NewList)
%   succeeds when Pred(Old,New) succeeds for each corresponding
%   Old in OldList, New in NewList.  In InterLisp, this is MAPCAR. 
%   It is also MAP2C.  Isn't bidirectionality wonderful?
maplist(_, [], []).
maplist(Pred, [In|ListIn], [Out|ListOut]) :-
    call(Pred, In, Out),
    maplist(Pred, ListIn, ListOut).

%   maplist(Pred, List1, List2, List3)
%   succeeds when Pred(Old,New) succeeds for each corresponding
%   Gi in Listi, New in NewList.  In InterLisp, this is MAPCAR. 
%   It is also MAP2C.  Isn't bidirectionality wonderful?
maplist(_, [], [], []).
maplist(Pred, [A1|L1], [A2|L2], [A3|L3]) :-
    call(Pred, A1, A2, A3),
    maplist(Pred, L1, L2, L3).

%   maplist(Pred, List1, List2, List3, List4)
%   succeeds when Pred(Old,New) succeeds for each corresponding
%   Gi in Listi, New in NewList.  In InterLisp, this is MAPCAR. 
%   It is also MAP2C.  Isn't bidirectionality wonderful?
maplist(_, [], [], [], []).
maplist(Pred, [A1|L1], [A2|L2], [A3|L3], [A4|L4]) :-
    call(Pred, A1, A2, A3, A4),
    maplist(Pred, L1, L2, L3, L4).

%   convlist(Rewrite, OldList, NewList)
%   is a sort of hybrid of maplist/3 and sublist/3.
%   Each element of NewList is the image under Rewrite of some
%   element of OldList, and order is preserved, but elements of
%   OldList on which Rewrite is undefined (fails) are not represented.
%   Thus if foo(X,Y) :- integer(X), Y is X+1.
%   then convlist(foo, [1,a,0,joe(99),101], [2,1,102]).
convlist(_, [], []).
convlist(Pred, [Old|Olds], NewList) :-
	call(Pred, Old, New),
	!,
	NewList = [New|News],
	convlist(Pred, Olds, News).
convlist(Pred, [_|Olds], News) :-
	convlist(Pred, Olds, News).

mapargs(Pred, TermIn, TermOut) :-
    functor(TermIn, F, N),
    functor(TermOut, F, N),
    mapargs_args(Pred, TermIn, TermOut, N).

mapargs_args(_, _, _, 0) :- !.
mapargs_args(Pred, TermIn, TermOut, I) :-
    arg(I, TermIn, InArg),
    arg(I, TermOut, OutArg),
    I1 is I-1,
    call(Pred, InArg, OutArg),
    mapargs_args(Pred, TermIn, TermOut, I1).

sumargs(Pred, Term, A0, A1) :-
    functor(Term, _, N),
    sumargs(Pred, Term, A0, A1, N).

sumargs_args(_, _, A0, A1, 0) :-
    !,
    A0 = A1.
sumargs_args(Pred, Term, A1, A3, N) :-
    arg(N, Term, Arg),
    N1 is N - 1,
    call(Pred, Arg, A1, A2),
    sumargs_args(Pred, Term, A2, A3, N1).

mapnodes(Pred, TermIn, TermOut) :-
    (atomic(TermIn); var(TermOut)), !,
    call(Pred, TermIn, TermOut).
mapnodes(Pred, TermIn, TermOut) :-
    call(Pred, TermIn, Temp),
    Temp =.. [Func|ArgsIn],
    mapnodes_list(Pred, ArgsIn, ArgsOut),
    TermOut =.. [Func|ArgsOut].

mapnodes_list(_, [], []).
appnodes_list(Pred, [TermIn|ArgsIn], [TermOut|ArgsOut]) :-
    mapnodes(Pred, TermIn, TermOut),
    mapnodes_list(Pred, ArgsIn, ArgsOut).

checknodes(Pred, Term) :-
    (atomic(Term); var(Term)), !,
    call(Pred, Term).
checknodes(Pred, Term) :-
    call(Pred, Term),
    Term =.. [_|Args],
    checknodes_list(Pred, Args).

checknodes_list(_, []).
checknodes_list(Pred, [Term|Args]) :-
    checknodes_body(Pred, Term),
    checknodes_list(Pred, Args).

sumlist(_, [], Acc, Acc).
sumlist(Pred, [H|T], AccIn, AccOut) :-
    call(Pred, H, AccIn, A1),
    sumlist(Pred, T, A1, AccOut).

sumnodes(Pred, Term, A0, A2) :-
    call(Pred, Term, A0, A1),
    (compound(Term) ->
	functor(Term, _, N),
	sumnodes_body(Pred, Term, A1, A2, 0, N)
    ;	% simple term or variable
	A1 = A2
    ).

sumnodes_body(Pred, Term, A1, A3, N0, Ar) :-
    N0 < Ar ->
	N is N0+1,
	arg(N, Term, Arg),
	sumnodes(Pred, Arg, A1, A2),
	sumnodes_body(Pred, Term, A2, A3, N, Ar)
    ;
	A1 = A3.

