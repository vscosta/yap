%   File   : apply_macros.yap
%   Author : E. Alphonse from code by Joachim Schimpf
%   Updated: 15 June 2002
%   Purpose: Macros to apply a predicate to all elements
%            of a list or to all sub-terms of a term.

:- module(apply_macros, []).

:- multifile user:goal_expansion/3.

:- use_module(library(lists), [append/3]).
:- use_module(library(charsio), [format_to_chars/3, read_from_chars/2]).

user:goal_expansion(maplist(Meta, ListIn, ListOut), Module, Goal) :-
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	% the new goal
	pred_name(maplist, Proto, GoalName),
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
		    ], Module).

user:goal_expansion(checklist(Meta, List), Module, Goal) :-
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	% the new goal
	pred_name(checklist, Proto, GoalName),
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
		    ], Module).

user:goal_expansion(selectlist(Meta, ListIn, ListOut), Module, Goal) :-
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	% the new goal
	pred_name(selectlist, Proto, GoalName),
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
		    ], Module).

user:goal_expansion(convlist(Meta, ListIn, ListOut), Module, Goal) :-
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	% the new goal
	pred_name(convlist, Proto, GoalName),
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
		    ], Module).

user:goal_expansion(sumlist(Meta, List, AccIn, AccOut), Module, Goal) :-
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	% the new goal
	pred_name(sumlist, Proto, GoalName),
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
		    ], Module).

user:goal_expansion(mapargs(Meta, In, Out), _Module, NewGoal) :-
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

user:goal_expansion(sumargs(Meta, Term, AccIn, AccOut), _Module, Goal) :-
	Goal = (
		 Term =.. [_|TermArgs],
		 sumlist(Meta, TermArgs, AccIn, AccOut)
	       ).

user:goal_expansion(mapnodes(Meta, InTerm, OutTerm), Module, Goal) :-
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	% the new goal
	pred_name(mapnodes, Proto, GoalName),
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
		    ], Module).

user:goal_expansion(checknodes(Meta, Term), Module, Goal) :-
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	% the new goal
	pred_name(checknodes, Proto, GoalName),
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
		    ], Module).

user:goal_expansion(sumnodes(Meta, Term, AccIn, AccOut), Module, Goal) :-
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	% the new goal
	pred_name(sumnodes, Proto, GoalName),
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
		    ], Module).

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

aux_preds(Meta, MetaVars, Pred, PredVars, Proto) :-
	Meta =.. [F|Args],
	aux_args(Args, MetaVars, PredArgs, PredVars, ProtoArgs),
	Pred =.. [F|PredArgs],
	Proto =.. [F|ProtoArgs].

aux_args([], [], [], [], []).
aux_args([Arg|Args], [Arg|MVars], [PVar|PArgs], [PVar|PVars], ['_'|ProtoArgs]) :-
	var(Arg),
	!,
	aux_args(Args, MVars, PArgs, PVars, ProtoArgs).
aux_args([Arg|Args], MVars, [Arg|PArgs], PVars, [Arg|ProtoArgs]) :-
	aux_args(Args, MVars, PArgs, PVars, ProtoArgs).

pred_name(Macro, Proto, Name) :-
	format_to_chars("'~a(~w)'.",[Macro, Proto], Chars),
	read_from_chars(Chars, Name).
