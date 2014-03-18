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
	            selectlist/4,
		    checklist/2,
		    maplist/2,			% :Goal, +List
		    maplist/3,			% :Goal, ?List1, ?List2
		    maplist/4,			% :Goal, ?List1, ?List2, ?List
		    maplist/5,			% :Goal, ?List1, ?List2, ?List3, List4
		    convlist/3,
		    mapnodes/3,
		    checknodes/2,
		    sumlist/4,
		    sumnodes/4,
		    include/3,
		    exclude/3,
		    partition/4,
		    partition/5,			
		    foldl/4,			% :Pred, +List, ?V0, ?V
		    foldl2/6,			% :Pred, +List, ?V0, ?V, ?W0, ?W
		    foldl2/7,			% :Pred, +List1, ?List2, ?V0, ?V, ?W0, ?W
		    foldl2/8,			% :Pred, +List1, ?List2, ?List3, ?V0, ?V, ?W0, ?W
		    foldl3/8,			% :Pred, +List, ?V0, ?V, ?W0, ?W
		    foldl4/10,			% :Pred, +List, ?V0, ?V, ?W0, ?W, ...
		    foldl/5,			% :Pred, +List1, +List2, ?V0, ?V
		    foldl/6,			% :Pred, +List1, +List2, +List3, ?V0, ?V
		    foldl/7,			% :Pred, +List1, +List2, +List3, +List4,
				% ?V0, ?V
		    scanl/4,			% :Pred, +List, ?V0, ?Vs
		    scanl/5,			% :Pred, +List1, +List2, ?V0, ?Vs
		    scanl/6,			% :Pred, +List1, +List2, +List3, ?V0, ?Vs
		    scanl/7			% :Pred, +List1, +List2, +List3, +List4,
			]).


:- meta_predicate
	selectlist(2,+,-),
	selectlist(3,+,+,-),
	checklist(1,+),
	maplist(1,+),
	maplist(2,+,-),
	maplist(3,+,+,-),
	maplist(4,+,+,+,-),
	convlist(2,+,-),
	mapnodes(2,+,-),
	mapnodes_list(2,+,-),
	checknodes(1,+),
	checknodes_list(1,+),
	sumlist(3,+,+,-),
	sumnodes(3,+,+,-),
	sumnodes_body(3,+,+,-,+,+),
	include(1,+,-),
	exclude(1,+,-),
	partition(2,+,-,-),
	partition(2,+,-,-,-),
	foldl(3, +, +, -),
	foldl2(5, +, +, -, +, -),
	foldl2(6, +, ?, +, -, +, -),
	foldl2(6, +, ?, ?, +, -, +, -),
	foldl3(5, +, +, -, +, -, +, -),
	foldl4(7, +, +, -, +, -, +, -, +, -),
	foldl(4, +, +, +, -),
	foldl(5, +, +, +, +, -),
	foldl(6, +, +, +, +, +, -),
	scanl(3, +, +, -),
	scanl(4, +, +, +, -),
	scanl(5, +, +, +, +, -),
	scanl(6, +, +, +, +, +, -).
	
:- use_module(library(maputils)).
:- use_module(library(lists), [append/3]).
:- use_module(library(charsio), [format_to_chars/3, read_from_chars/2]).
:- use_module(library(error), [must_be/2]).
:- use_module(library(occurs), [sub_term/2]).

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

selectlist(_, [], [], []).
selectlist(Pred, [In|ListIn], [In1|ListIn1], ListOut) :-
    (call(Pred, In, In1) ->
	ListOut = [In|NewListOut]
    ;
	ListOut = NewListOut
    ),
    selectlist(Pred, ListIn, ListIn1, NewListOut).

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

mapnodes(Pred, TermIn, TermOut) :-
    (atomic(TermIn); var(TermIn)), !,
    call(Pred, TermIn, TermOut).
mapnodes(Pred, TermIn, TermOut) :-
    call(Pred, TermIn, Temp),
    Temp =.. [Func|ArgsIn],
    mapnodes_list(Pred, ArgsIn, ArgsOut),
    TermOut =.. [Func|ArgsOut].

mapnodes_list(_, [], []).
mapnodes_list(Pred, [TermIn|ArgsIn], [TermOut|ArgsOut]) :-
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


		 /*******************************
		 *	      FOLDL		*
		 *******************************/

%%	foldl(:Goal, +List, +V0, -V, +W0, -WN).
%
%	Fold a list, using arguments of the   list as left argument. The
%	foldl family of predicates is defined by:
%
%	  ==
%	  foldl(P, [X11,...,X1n],V0, Vn, W0, WN) :-
%		P(X11, V0, V1, W0, W1),
%		...
%		P(X1n, Vn1, Vn, Wn1, Wn).
%	  ==

foldl(Goal, List, V0, V) :-
	foldl_(List, Goal, V0, V).

foldl_([], _, V, V).
foldl_([H|T], Goal, V0, V) :-
	call(Goal, H, V0, V1),
	foldl_(T, Goal, V1, V).

foldl(Goal, List1, List2, V0, V) :-
	foldl_(List1, List2, Goal, V0, V).

foldl_([], [], _, V, V).
foldl_([H1|T1], [H2|T2], Goal, V0, V) :-
	call(Goal, H1, H2, V0, V1),
	foldl_(T1, T2, Goal, V1, V).


foldl(Goal, List1, List2, List3, V0, V) :-
	foldl_(List1, List2, List3, Goal, V0, V).

foldl_([], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], Goal, V0, V) :-
	call(Goal, H1, H2, H3, V0, V1),
	foldl_(T1, T2, T3, Goal, V1, V).


foldl(Goal, List1, List2, List3, List4, V0, V) :-
	foldl_(List1, List2, List3, List4, Goal, V0, V).

foldl_([], [], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal, V0, V) :-
	call(Goal, H1, H2, H3, H4, V0, V1),
	foldl_(T1, T2, T3, T4, Goal, V1, V).


%%	foldl(:Goal, +List, +V0, -V).
%%	foldl(:Goal, +List1, +List2, +V0, -V).
%%	foldl(:Goal, +List1, +List2, +List3, +V0, -V).
%%	foldl(:Goal, +List1, +List2, +List3, +List4, +V0, -V).
%
%	Fold a list, using arguments of the   list as left argument. The
%	foldl family of predicates is defined by:
%
%	  ==
%	  foldl(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V0, Vn) :-
%		P(X11, ..., Xm1, V0, V1),
%		...
%		P(X1n, ..., Xmn, V', Vn).
%	  ==

foldl2(Goal, List, V0, V, W0, W) :-
	foldl2_(List, Goal, V0, V, W0, W).

foldl2_([], _, V, V, W, W).
foldl2_([H|T], Goal, V0, V, W0, W) :-
	call(Goal, H, V0, V1, W0, W1),
	foldl2_(T, Goal, V1, V, W1, W).


foldl2(Goal, List1, List2, V0, V, W0, W) :-
	foldl2_(List1, List2, Goal, V0, V, W0, W).

foldl2_([], [], _Goal, V, V, W, W).
foldl2_([H1|T1], [H2|T2], Goal, V0, V, W0, W) :-
	call(Goal, H1, H2, V0, V1, W0, W1),
	foldl2_(T1, T2, Goal, V1, V, W1, W).

foldl2(Goal, List1, List2, List3, V0, V, W0, W) :-
	foldl2_(List1, List2, List3, Goal, V0, V, W0, W).

foldl2_([], [], [], _Goal, V, V, W, W).
foldl2_([H1|T1], [H2|T2], [H3|T3], Goal, V0, V, W0, W) :-
	call(Goal, H1, H2, H3, V0, V1, W0, W1),
	foldl2_(T1, T2, T3, Goal, V1, V, W1, W).


foldl3(Goal, List, V0, V, W0, W, X0, X) :-
	foldl3_(List, Goal, V0, V, W0, W, X0, X).

foldl3_([], _, V, V, W, W, X, X).
foldl3_([H|T], Goal, V0, V, W0, W, X0, X) :-
	call(Goal, H, V0, V1, W0, W1, X0, X1),
	fold3_(T, Goal, V1, V, W1, W, X1, X).

foldl4(Goal, List, V0, V, W0, W, X0, X, Y0, Y) :-
	foldl4_(List, Goal, V0, V, W0, W, X0, X, Y0, Y).

foldl4_([], _, V, V, W, W, X, X, Y, Y).
foldl4_([H|T], Goal, V0, V, W0, W, X0, X, Y0, Y) :-
    call(Goal, H, V0, V1, W0, W1, X0, X1, Y0, Y1),
    foldl4_(T, Goal, V1, V, W1, W, X1, X, Y1, Y).



		 /*******************************
		 *	       SCANL		*
		 *******************************/

%%	scanl(:Goal, +List, +V0, -Values).
%%	scanl(:Goal, +List1, +List2, +V0, -Values).
%%	scanl(:Goal, +List1, +List2, +List3, +V0, -Values).
%%	scanl(:Goal, +List1, +List2, +List3, +List4, +V0, -Values).
%
%	Left scan of  list.  The  scanl   family  of  higher  order list
%	operations is defined by:
%
%	  ==
%	  scanl(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V0, [V0,V1,...,Vn]) :-
%		P(X11, ..., Xmn, V0, V1),
%		...
%	        P(X1n, ..., Xmn, V', Vn).
%	  ==

scanl(Goal, List, V0, [V0|Values]) :-
	scanl_(List, Goal, V0, Values).

scanl_([], _, _, []).
scanl_([H|T], Goal, V, [VH|VT]) :-
	call(Goal, H, V, VH),
	scanl_(T, Goal, VH, VT).


scanl(Goal, List1, List2, V0, [V0|Values]) :-
	scanl_(List1, List2, Goal, V0, Values).

scanl_([], [], _, _, []).
scanl_([H1|T1], [H2|T2], Goal, V, [VH|VT]) :-
	call(Goal, H1, H2, V, VH),
	scanl_(T1, T2, Goal, VH, VT).


scanl(Goal, List1, List2, List3, V0, [V0|Values]) :-
	scanl_(List1, List2, List3, Goal, V0, Values).

scanl_([], [], [], _, _, []).
scanl_([H1|T1], [H2|T2], [H3|T3], Goal, V, [VH|VT]) :-
	call(Goal, H1, H2, H3, V, VH),
	scanl_(T1, T2, T3, Goal, VH, VT).


scanl(Goal, List1, List2, List3, List4, V0, [V0|Values]) :-
	scanl_(List1, List2, List3, List4, Goal, V0, Values).

scanl_([], [], [], [], _, _, []).
scanl_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal, V, [VH|VT]) :-
	call(Goal, H1, H2, H3, H4, V, VH),
	scanl_(T1, T2, T3, T4, Goal, VH, VT).


goal_expansion(checklist(Meta, List), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(checklist, 2, Proto, GoalName),
	append(MetaVars, [List], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	append_args(HeadPrefix, [[]], Base),
	append_args(HeadPrefix, [[In|Ins]], RecursionHead),
	append_args(Pred, [In], Apply),
	append_args(HeadPrefix, [Ins], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(maplist(Meta, List), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto), 
	!,
	% the new goal
	pred_name(maplist, 2, Proto, GoalName),
	append(MetaVars, [List], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	append_args(HeadPrefix, [[]], Base),
	append_args(HeadPrefix, [[In|Ins]], RecursionHead),
	append_args(Pred, [In], Apply),
	append_args(HeadPrefix, [Ins], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(maplist(Meta, ListIn, ListOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),	
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(maplist, 3, Proto, GoalName),
	append(MetaVars, [ListIn, ListOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], []], Base),
	append_args(HeadPrefix, [[In|Ins], [Out|Outs]], RecursionHead),
	append_args(Pred, [In, Out], Apply),
	append_args(HeadPrefix, [Ins, Outs], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(maplist(Meta, L1, L2, L3), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(maplist, 4, Proto, GoalName),
	append(MetaVars, [L1, L2, L3], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], [], []], Base),
	append_args(HeadPrefix, [[A1|A1s], [A2|A2s], [A3|A3s]], RecursionHead),
	append_args(Pred, [A1, A2, A3], Apply),
	append_args(HeadPrefix, [A1s, A2s, A3s], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(maplist(Meta, L1, L2, L3, L4), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(maplist, 5, Proto, GoalName),
	append(MetaVars, [L1, L2, L3, L4], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], [], [], []], Base),
	append_args(HeadPrefix, [[A1|A1s], [A2|A2s], [A3|A3s], [A4|A4s]], RecursionHead),
	append_args(Pred, [A1, A2, A3, A4], Apply),
	append_args(HeadPrefix, [A1s, A2s, A3s, A4s], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(selectlist(Meta, ListIn, ListOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(selectlist, 3, Proto, GoalName),
	append(MetaVars, [ListIn, ListOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], []], Base),
	append_args(HeadPrefix, [[In|Ins], Outs], RecursionHead),
	append_args(Pred, [In], Apply),
	append_args(HeadPrefix, [Ins, NOuts], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         (Apply -> Outs = [In|NOuts]; Outs = NOuts),
			 RecursiveCall)
		    ], Mod).

goal_expansion(selectlist(Meta, ListIn, ListIn1, ListOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(selectlist, 3, Proto, GoalName),
	append(MetaVars, [ListIn, ListIn1, ListOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], [], []], Base),
	append_args(HeadPrefix, [[In|Ins], [In1|Ins1], Outs], RecursionHead),
	append_args(Pred, [In, In1], Apply),
	append_args(HeadPrefix, [Ins, Ins1, NOuts], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         (Apply -> Outs = [In|NOuts]; Outs = NOuts),
			 RecursiveCall)
		    ], Mod).

% same as selectlist
goal_expansion(include(Meta, ListIn, ListOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(include, 3, Proto, GoalName),
	append(MetaVars, [ListIn, ListOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], []], Base),
	append_args(HeadPrefix, [[In|Ins], Outs], RecursionHead),
	append_args(Pred, [In], Apply),
	append_args(HeadPrefix, [Ins, NOuts], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         (Apply -> Outs = [In|NOuts]; Outs = NOuts),
			 RecursiveCall)
		    ], Mod).

goal_expansion(exclude(Meta, ListIn, ListOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(exclude, 3, Proto, GoalName),
	append(MetaVars, [ListIn, ListOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], []], Base),
	append_args(HeadPrefix, [[In|Ins], Outs], RecursionHead),
	append_args(Pred, [In], Apply),
	append_args(HeadPrefix, [Ins, NOuts], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         (Apply -> Outs = NOuts; Outs = [In|NOuts]),
			 RecursiveCall)
		    ], Mod).

goal_expansion(partition(Meta, ListIn, List1, List2), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(partition, 4, Proto, GoalName),
	append(MetaVars, [ListIn, List1, List2], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], [], []], Base),
	append_args(HeadPrefix, [[In|Ins], Outs1, Outs2], RecursionHead),
	append_args(Pred, [In], Apply),
	append_args(HeadPrefix, [Ins, NOuts1, NOuts2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         (Apply -> Outs1 = [In|NOuts1], Outs2 = NOuts2; Outs1 = NOuts1, Outs2 = [In|NOuts2]),
			 RecursiveCall)
		    ], Mod).

goal_expansion(partition(Meta, ListIn, List1, List2, List3), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(partition2, 5, Proto, GoalName),
	append(MetaVars, [ListIn, List1, List2, List3], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], [], [], []], Base),
	append_args(HeadPrefix, [[In|Ins], Outs1, Outs2, Outs3], RecursionHead),
	append_args(Pred, [In,Diff], Apply),
	append_args(HeadPrefix, [Ins, NOuts1, NOuts2, NOuts3], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         Apply,
		         (Diff == (<)  ->
	                   Outs1 = [In|NOuts1],
	                   Outs2 = NOuts2,
	                   Outs3 = NOuts3
			 ;
			  Diff == (=)  ->
			  Outs1 = NOuts1,
			  Outs2 = [In|NOuts2],
			  Outs3 = NOuts3
			 ;
			  Diff == (>)  ->
			  Outs1 = NOuts1,
			  Outs2 = NOuts2,
			  Outs3 = [In|NOuts3]
			 ;
			  error:must_be(oneof([<,=,>]), Diff)
			 ),
			 RecursiveCall)
		    ], Mod).

goal_expansion(convlist(Meta, ListIn, ListOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(convlist, 3, Proto, GoalName),
	append(MetaVars, [ListIn, ListOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], []], Base),
	append_args(HeadPrefix, [[In|Ins], Outs], RecursionHead),
	append_args(Pred, [In, Out], Apply),
	append_args(HeadPrefix, [Ins, NOuts], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         (Apply -> Outs = [Out|NOuts]; Outs = NOuts),
			 RecursiveCall)
		    ], Mod).

goal_expansion(sumlist(Meta, List, AccIn, AccOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(sumlist, 4, Proto, GoalName),
	append(MetaVars, [List, AccIn, AccOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], Acc, Acc], Base),
	append_args(HeadPrefix, [[In|Ins], Acc1, Acc2], RecursionHead),
	append_args(Pred, [In, Acc1, Acc3], Apply),
	append_args(HeadPrefix, [Ins, Acc3, Acc2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(foldl(Meta, List, AccIn, AccOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldl, 4, Proto, GoalName),
	append(MetaVars, [List, AccIn, AccOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], Acc, Acc], Base),
	append_args(HeadPrefix, [[In|Ins], Acc1, Acc2], RecursionHead),
	append_args(Pred, [In, Acc1, Acc3], Apply),
	append_args(HeadPrefix, [Ins, Acc3, Acc2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(foldl(Meta, List1, List2, AccIn, AccOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldl, 5, Proto, GoalName),
	append(MetaVars, [List1, List2, AccIn, AccOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], [], Acc, Acc], Base),
	append_args(HeadPrefix, [[In|Ins], [I2|Is2], Acc1, Acc2], RecursionHead),
	append_args(Pred, [In, I2, Acc1, Acc3], Apply),
	append_args(HeadPrefix, [Ins, Is2, Acc3, Acc2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(foldl(Meta, List1, List2, List3, AccIn, AccOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldl, 6, Proto, GoalName),
	append(MetaVars, [List1, List2, List3, AccIn, AccOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], [], [], Acc, Acc], Base),
	append_args(HeadPrefix, [[In|Ins], [I2|I2s], [I3|I3s], Acc1, Acc2], RecursionHead),
	append_args(Pred, [In, I2, I3, Acc1, Acc3], Apply),
	append_args(HeadPrefix, [Ins, I2s, I3s, Acc3, Acc2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(foldl2(Meta, List, AccIn, AccOut, W0, W), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldl2, 6, Proto, GoalName),
	append(MetaVars, [List, AccIn, AccOut, W0, W], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], Acc, Acc, W, W], Base),
	append_args(HeadPrefix, [[In|Ins], Acc1, Acc2, W1, W2], RecursionHead),
	append_args(Pred, [In, Acc1, Acc3, W1, W3], Apply),
	append_args(HeadPrefix, [Ins, Acc3, Acc2, W3, W2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(foldl2(Meta, List1, List2, AccIn, AccOut, W0, W), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldl2, 7, Proto, GoalName),
	append(MetaVars, [List1, List2, AccIn, AccOut, W0, W], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], [], Acc, Acc, W, W], Base),
	append_args(HeadPrefix, [[In1|Ins1], [In2|Ins2], Acc1, Acc2, W1, W2], RecursionHead),
	append_args(Pred, [In1, In2, Acc1, Acc3, W1, W3], Apply),
	append_args(HeadPrefix, [Ins1, Ins2, Acc3, Acc2, W3, W2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(foldl2(Meta, List1, List2, List3, AccIn, AccOut, W0, W), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldl2, 7, Proto, GoalName),
	append(MetaVars, [List1, List2, List3, AccIn, AccOut, W0, W], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], [], [], Acc, Acc, W, W], Base),
	append_args(HeadPrefix, [[In1|Ins1], [In2|Ins2], [In3|Ins3], Acc1, Acc2, W1, W2], RecursionHead),
	append_args(Pred, [In1, In2, In3, Acc1, Acc3, W1, W3], Apply),
	append_args(HeadPrefix, [Ins1, Ins2, Ins3, Acc3, Acc2, W3, W2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(foldl3(Meta, List, AccIn, AccOut, W0, W, X0, X), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldl3, 8, Proto, GoalName),
	append(MetaVars, [List, AccIn, AccOut, W0, W, X0, X], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], Acc, Acc, W, W, X, X], Base),
	append_args(HeadPrefix, [[In|Ins], Acc1, Acc2, W1, W2, X1, X2], RecursionHead),
	append_args(Pred, [In, Acc1, Acc3, W1, W3, X1, X3], Apply),
	append_args(HeadPrefix, [Ins, Acc3, Acc2, W3, W2, X3, X2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(foldl4(Meta, List, AccIn, AccOut, W0, W, X0, X, Y0, Y), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldl4, 8, Proto, GoalName),
	append(MetaVars, [List, AccIn, AccOut, W0, W, X0, X, Y0, Y], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], Acc, Acc, W, W, X, X, Y, Y], Base),
	append_args(HeadPrefix, [[In|Ins], Acc1, Acc2, W1, W2, X1, X2, Y1, Y2], RecursionHead),
	append_args(Pred, [In, Acc1, Acc3, W1, W3, X1, X3, Y1, Y3], Apply),
	append_args(HeadPrefix, [Ins, Acc3, Acc2, W3, W2, X3, X2, Y3, Y2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :- Apply, RecursiveCall)
		    ], Mod).

goal_expansion(mapnodes(Meta, InTerm, OutTerm), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(mapnodes, 3, Proto, GoalName),
	append(MetaVars, [[InTerm], [OutTerm]], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	append_args(HeadPrefix, [[], []], Base),
	append_args(HeadPrefix, [[In|Ins], [Out|Outs]], RecursionHead),
	append_args(Pred, [In, Temp], Apply),
	append_args(HeadPrefix, [InArgs, OutArgs], SubRecursiveCall),
	append_args(HeadPrefix, [Ins, Outs], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         Apply,
			 (compound(Temp)
			 ->
			     Temp =.. [F|InArgs],
			     SubRecursiveCall,
			     Out =.. [F|OutArgs]
			 ;
			     Out = Temp
			 ),
			 RecursiveCall)
		    ], Mod).

goal_expansion(checknodes(Meta, Term), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(checknodes, 2, Proto, GoalName),
	append(MetaVars, [[Term]], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	append_args(HeadPrefix, [[]], Base),
	append_args(HeadPrefix, [[In|Ins]], RecursionHead),
	append_args(Pred, [In], Apply),
	append_args(HeadPrefix, [Args], SubRecursiveCall),
	append_args(HeadPrefix, [Ins], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         Apply,
			 (compound(In)
			 ->
			     In =.. [_|Args],SubRecursiveCall
			 ;
			     true
			 ),
			 RecursiveCall)
		    ], Mod).

goal_expansion(sumnodes(Meta, Term, AccIn, AccOut), Mod:Goal) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(sumnodes, 4, Proto, GoalName),
	append(MetaVars, [[Term], AccIn, AccOut], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],	
	append_args(HeadPrefix, [[], Acc, Acc], Base),
	append_args(HeadPrefix, [[In|Ins], Acc1, Acc2], RecursionHead),
	append_args(Pred, [In, Acc1, Acc3], Apply),
	append_args(HeadPrefix, [Args, Acc3, Acc4], SubRecursiveCall),
	append_args(HeadPrefix, [Ins, Acc4, Acc2], RecursiveCall),
	compile_aux([
		     Base,
		     (RecursionHead :-
		         Apply,
			 (compound(In)
			 ->
			     In =.. [_|Args],SubRecursiveCall
			 ;
			     Acc3 = Acc4
			 ),
			 RecursiveCall)
		    ], Mod).

/*
:- unhide('$translate_rule').
% stolen from SWI-Prolog
user:goal_expansion(phrase(NT,Xs), Mod, NTXsNil) :-
	user:goal_expansion(phrase(NT,Xs,[]), Mod, NTXsNil).
user:goal_expansion(phrase(NT,Xs0,Xs), Mod, NewGoal) :-
	goal_expansion_allowed,
	Goal = phrase(NT,Xs0,Xs),
	nonvar(NT),
	catch('$translate_rule'((pseudo_nt --> NT), Rule),
	      error(Pat,ImplDep),
	      ( \+ harmless_dcgexception(Pat), 
		throw(error(Pat,ImplDep))
	      )),
	Rule = (pseudo_nt(Xs0c,Xsc) :- NewGoal0),
	Goal \== NewGoal0,
	% apply translation only if we are safe
	\+ contains_illegal_dcgnt(NT), !,
	(   var(Xsc), Xsc \== Xs0c
	->  Xs = Xsc, NewGoal1 = NewGoal0
	;   NewGoal1 = (NewGoal0, Xsc = Xs)
	),
	(   var(Xs0c)
	-> Xs0 = Xs0c,
	   NewGoal = NewGoal1
	;  ( Xs0 = Xs0c, NewGoal1 ) = NewGoal
	).
:- hide('$translate_rule').
*/

