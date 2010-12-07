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
	selectlist(2,+,-),
	checklist(1,+),
	maplist(1,+),
	maplist(2,+,-),
	maplist(3,+,+,-),
	maplist(4,+,+,+,-),
	convlist(2,+,-),
	mapargs(2,+,-),
	mapargs_args(2,+,-,+),
	sumargs(3,+,+,-),
	sumargs_args(3,+,+,-,+),
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
	partition(2,+,-,-,-).
	
	
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


:- dynamic number_of_expansions/1.

number_of_expansions(0).

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

goal_expansion(mapargs(Meta, In, Out), Mod:NewGoal) :-
	goal_expansion_allowed,
	prolog_load_context(module, Mod),
	( var(Out)
	->
	    NewGoal = (
			In =.. [F|InArgs],
			maplist(Meta, InArgs, OutArgs),
			Out =.. [F|OutArgs]
		      )
	;
	    NewGoal = (
			Out =.. [F|OutArgs],
			maplist(Meta, InArgs, OutArgs),
			In =.. [F|InArgs]
		      )
	).	    

goal_expansion(sumargs(Meta, Term, AccIn, AccOut), Mod:Goal) :-
	goal_expansion_allowed,
	prolog_load_context(module, Mod),
	Goal = (
		 Term =.. [_|TermArgs],
		 sumlist(Meta, TermArgs, AccIn, AccOut)
	       ).

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

%%%%%%%%%%%%%%%%%%%%
% utilities
%%%%%%%%%%%%%%%%%%%%

compile_aux([Clause|Clauses], Module) :-
	% compile the predicat declaration if needed
	( Clause = (Head :- _)
	; Clause = Head ),
	!,
	functor(Head, F, N),
	( current_predicate(Module:F/N)
	->
	    true
	;
%	    format("*** Creating auxiliary predicate ~q~n", [F/N]),
%	    checklist(portray_clause, [Clause|Clauses]),
	    compile_term([Clause|Clauses], Module)
	).

compile_term([], _).
compile_term([Clause|Clauses], Module) :-
	assert_static(Module:Clause),
	compile_term(Clauses, Module).

append_args(Term, Args, NewTerm) :-
	Term =.. [Meta|OldArgs],
	append(OldArgs, Args, GoalArgs),
	NewTerm =.. [Meta|GoalArgs].

aux_preds(Meta, _, _, _, _) :-
	var(Meta), !,
	fail.
aux_preds(_:Meta, MetaVars, Pred, PredVars, Proto) :- !,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto).
aux_preds(Meta, MetaVars, Pred, PredVars, Proto) :-
	Meta =.. [F|Args],
	aux_args(Args, MetaVars, PredArgs, PredVars, ProtoArgs),
	Pred =.. [F|PredArgs],
	Proto =.. [F|ProtoArgs].

aux_args([], [], [], [], []).
aux_args([Arg|Args], MVars, [Arg|PArgs], PVars, [Arg|ProtoArgs]) :-
	ground(Arg), !,
	aux_args(Args, MVars, PArgs, PVars, ProtoArgs).
aux_args([Arg|Args], [Arg|MVars], [PVar|PArgs], [PVar|PVars], ['_'|ProtoArgs]) :-
	aux_args(Args, MVars, PArgs, PVars, ProtoArgs).

pred_name(Macro, Arity, _ , Name) :-
	transformation_id(Id),
	atomic_concat(['$$$__Auxiliary_predicate__ for ',Macro,'/',Arity,'    ',Id], Name).

transformation_id(Id) :-
	retract(number_of_expansions(Id)),
	Id1 is Id+1,
	assert(number_of_expansions(Id1)).

harmless_dcgexception(instantiation_error).	% ex: phrase(([1],x:X,[3]),L)
harmless_dcgexception(type_error(callable,_)).	% ex: phrase(27,L)


%%	contains_illegal_dcgnt(+Term) is semidet.
%
%	True if Term contains a non-terminal   we cannot deal with using
%	goal-expansion. The test is too general approximation, but safe.

contains_illegal_dcgnt(NT) :-
	sub_term(I, NT),
	nonvar(I),
	( I = ! ; I = phrase(_,_,_) ), !.
%	write(contains_illegal_nt(NT)),		% JW: we do not want to write
%	nl.

goal_expansion_allowed :-
	once( prolog_load_context(_, _) ), % make sure we are compiling.
	\+ current_prolog_flag(xref, true).

