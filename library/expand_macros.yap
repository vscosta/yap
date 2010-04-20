
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% preprocessing for meta-calls
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(expand_macros, []).

:- use_module(library(lists), [append/3]).
:- use_module(library(charsio), [format_to_chars/3, read_from_chars/2]).
:- use_module(library(error), [must_be/2]).
:- use_module(library(occurs), [sub_term/2]).

:- multifile user:goal_expansion/3.

:- dynamic number_of_expansions/1.

number_of_expansions(0).

user:goal_expansion(checklist(Meta, List), Mod, Goal) :-
	goal_expansion_allowed(checklist(Meta, List), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(maplist(Meta, List), Mod, Goal) :-
	goal_expansion_allowed(maplist(Meta, List), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(maplist(Meta, ListIn, ListOut), Mod, Goal) :-
	goal_expansion_allowed(maplist(Meta, ListIn, ListOut), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(maplist(Meta, L1, L2, L3), Mod, Goal) :-
	goal_expansion_allowed(maplist(Meta, L1, L2, L3), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(maplist(Meta, L1, L2, L3, L4), Mod, Goal) :-
	goal_expansion_allowed(maplist(Meta, L1, L2, L3, L4), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(selectlist(Meta, ListIn, ListOut), Mod, Goal) :-
	goal_expansion_allowed(selectlist(Meta, ListIn, ListOut), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

% same as selectlist
user:goal_expansion(include(Meta, ListIn, ListOut), Mod, Goal) :-
	goal_expansion_allowed(include(Meta, ListIn, ListOut), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(exclude(Meta, ListIn, ListOut), Mod, Goal) :-
	goal_expansion_allowed(exclude(Meta, ListIn, ListOut), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		         (Apply -> Outs = [In|NOuts]; Outs = NOuts),
			 RecursiveCall)
		    ], Module).

user:goal_expansion(partition(Meta, ListIn, List1, List2), Mod, Goal) :-
	goal_expansion_allowed(partition(Meta, ListIn, List1, List2), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(partition(Meta, ListIn, List1, List2, List3), Mod, Goal) :-
	goal_expansion_allowed(partition(Meta, ListIn, List1, List2, List3), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(convlist(Meta, ListIn, ListOut), Mod, Goal) :-
	goal_expansion_allowed(convlist(Meta, ListIn, ListOut), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(sumlist(Meta, List, AccIn, AccOut), Mod, Goal) :-
	goal_expansion_allowed(sumlist(Meta, List, AccIn, AccOut), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(mapargs(Meta, In, Out), Module, NewGoal) :-
	goal_expansion_allowed(mapargs(Meta, In, Out), Module),
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

user:goal_expansion(sumargs(Meta, Term, AccIn, AccOut), Module, Goal) :-
	goal_expansion_allowed(sumargs(Meta, Term, AccIn, AccOut), Module),
	Goal = (
		 Term =.. [_|TermArgs],
		 sumlist(Meta, TermArgs, AccIn, AccOut)
	       ).

user:goal_expansion(mapnodes(Meta, InTerm, OutTerm), Mod, Goal) :-
	goal_expansion_allowed(mapnodes(Meta, InTerm, OutTerm), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(checknodes(Meta, Term), Mod, Goal) :-
	goal_expansion_allowed(checknodes(Meta, Term), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

user:goal_expansion(sumnodes(Meta, Term, AccIn, AccOut), Mod, Goal) :-
	goal_expansion_allowed(sumnodes(Meta, Term, AccIn, AccOut), Mod),
	callable(Meta),
	!,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Mod, Module),
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
		    ], Module).

:- unhide('$translate_rule').
% stolen from SWI-Prolog
user:goal_expansion(phrase(NT,Xs), Mod, NTXsNil) :-
	user:goal_expansion(phrase(NT,Xs,[]), Mod, NTXsNil).
user:goal_expansion(phrase(NT,Xs0,Xs), Mod, NewGoal) :-
	goal_expansion_allowed(phrase(NT,Xs0,Xs), Mod),
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

aux_preds(Module:Meta, MetaVars, Pred, PredVars, Proto, _, OModule) :- !,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Module, OModule).
aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Module, Module) :-
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
	atomic_concat(['$$$__Auxiliary_predicate__ for',Macro,'/',Arity,'    ',Id], Name).

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

goal_expansion_allowed(Pred, Mod) :-
	allowed_module(Pred,Mod),
	once( prolog_load_context(_, _) ), % make sure we are compiling.
	\+ current_prolog_flag(xref, true).

allowed_module(checklist(_,_),expand_macros).
allowed_module(checklist(_,_),apply_macros).
allowed_module(checklist(_,_),maplist).
allowed_module(maplist(_,_),expand_macros).
allowed_module(maplist(_,_),apply_macros).
allowed_module(maplist(_,_),maplist).
allowed_module(maplist(_,_,_),expand_macros).
allowed_module(maplist(_,_,_),apply_macros).
allowed_module(maplist(_,_,_),maplist).
allowed_module(maplist(_,_,_,_),expand_macros).
allowed_module(maplist(_,_,_,_),apply_macros).
allowed_module(maplist(_,_,_,_),maplist).
allowed_module(maplist(_,_,_,_,_),expand_macros).
allowed_module(maplist(_,_,_,_,_),apply_macros).
allowed_module(maplist(_,_,_,_,_),maplist).
allowed_module(maplist(_,_,_,_,_,_),expand_macros).
allowed_module(maplist(_,_,_,_,_,_),apply_macros).
allowed_module(maplist(_,_,_,_,_,_),maplist).
allowed_module(selectlist(_,_,_),expand_macros).
allowed_module(selectlist(_,_,_),apply_macros).
allowed_module(selectlist(_,_,_),maplist).
allowed_module(include(_,_,_),expand_macros).
allowed_module(include(_,_,_),apply_macros).
allowed_module(include(_,_,_),maplist).
allowed_module(exclude(_,_,_),expand_macros).
allowed_module(exclude(_,_,_),apply_macros).
allowed_module(exclude(_,_,_),maplist).
allowed_module(partition(_,_,_,_),expand_macros).
allowed_module(partition(_,_,_,_),apply_macros).
allowed_module(partition(_,_,_,_),maplist).
allowed_module(partition(_,_,_,_,_),expand_macros).
allowed_module(partition(_,_,_,_,_),apply_macros).
allowed_module(partition(_,_,_,_,_),maplist).
allowed_module(convlist(_,_,_),expand_macros).
allowed_module(convlist(_,_,_),apply_macros).
allowed_module(convlist(_,_,_),maplist).
allowed_module(sumlist(_,_,_,_),expand_macros).
allowed_module(sumlist(_,_,_,_),apply_macros).
allowed_module(sumlist(_,_,_,_),maplist).
allowed_module(mapargs(_,_,_),expand_macros).
allowed_module(mapargs(_,_,_),apply_macros).
allowed_module(mapargs(_,_,_),maplist).
allowed_module(sumargs(_,_,_,_),expand_macros).
allowed_module(sumargs(_,_,_,_),apply_macros).
allowed_module(sumargs(_,_,_,_),maplist).
allowed_module(mapnodes(_,_,_),expand_macros).
allowed_module(mapnodes(_,_,_),apply_macros).
allowed_module(mapnodes(_,_,_),maplist).
allowed_module(checknodes(_,_),expand_macros).
allowed_module(checknodes(_,_),apply_macros).
allowed_module(checknodes(_,_),maplist).
allowed_module(sumnodes(_,_,_,_),expand_macros).
allowed_module(sumnodes(_,_,_,_),apply_macros).
allowed_module(sumnodes(_,_,_,_),maplist).
allowed_module(phrase(_,_),_).
allowed_module(phrase(_,_,_),_).
