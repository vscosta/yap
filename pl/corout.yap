/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		corout.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	Coroutines implementation				 *
*									 *
*************************************************************************/


%:- module(coroutining,[
%dif/2,
%when/2,
%block/1,
%wait/1,
%frozen/2
%]). 

%
% operators defined in this module:
%
:- op(1150, fx, block).

%
% Tell the system how to present frozen goals.
%

:- assert((extensions_to_present_answer(Level) :-
	   '$show_frozen_goals'(Level))).

'$show_frozen'(G,V,LGs) :-
	\+ '$undefined'(all_attvars(LAV), attributes),
	attributes:all_attvars(LAV),
	LAV = [_|_], !,
	'$convert_to_list_of_frozen_goals'(V,LAV,G,LGs).
'$show_frozen'(_,_,[]).


'$convert_to_list_of_frozen_goals'(LIV,LAV,_,NLG) :-
	'$project'(LAV,LIV,NLG).


'$get_rid_of_vls'((_-G),G).
'$get_rid_of_vls'((A,B),(NA,NB)) :-
	'$get_rid_of_vls'(A,NA),
	'$get_rid_of_vls'(B,NB).

%
% wake_up_goal is called by the system whenever a suspended goal
% resumes.
%

/* The first case may happen if this variable was used for dif.
   In this case, we need a way to keep the original
   suspended goal around
*/
%'$wake_up_goal'([Module1|Continuation],G) :-
%	'$write'(4,vsc_woke:G+[Module1|Continuation]:'
%'), fail.
'$wake_up_goal'([Module1|Continuation], LG) :-
%write(waking:LG),nl,
	'$execute_woken_system_goals'(LG),
	'$do_continuation'(Continuation, Module1).


%
% in the first two cases restore register  immediately and proceed
% to continuation. In the last case take care with modules, but do
% not act as if a meta-call.
% 
%
'$do_continuation'('$cut_by'(X), _) :- !,
	'$$cut_by'(X).
'$do_continuation'('$restore_regs'(X), _) :- !,
	'$restore_regs'(X).
'$do_continuation'('$restore_regs'(X,Y), _) :- !,
	'$restore_regs'(X,Y).
'$do_continuation'(Continuation, Module1) :-
	'$execute_continuation'(Continuation,Module1).

'$execute_continuation'(Continuation, Module1) :-
	'$undefined'(Continuation, Module1), !,
        '$undefp'([Module1|Continuation]).
'$execute_continuation'(Continuation, Mod) :-
         % do not do meta-expansion nor any fancy stuff.
	'$execute0'(Continuation, Mod).


'$execute_woken_system_goals'([]).
'$execute_woken_system_goals'([G|LG]) :-
	'$execute_woken_system_goal'(G),
	'$execute_woken_system_goals'(LG).

%
% X surely was bound, otherwise we would not be awaken.
%
'$execute_woken_system_goal'('$att_do'(V,New)) :-
	( '$frozen_goals'(V, Goals) ->
	    '$call_atts'(V,New),
	    '$execute_frozen_goals'(Goals)
	 ;
	    '$call_atts'(V,New)
	).

'$call_atts'(V,_) :-
	nonvar(V), !.
'$call_atts'(V,_) :-
	'$undefined'(woken_att_do(_,_), attributes), !,
	attributes:bind_attvar(V).
'$call_atts'(V,_) :-
	'$att_bound'(V), !.
'$call_atts'(V,New) :-
	attributes:woken_att_do(V,New).

'$execute_frozen_goals'([]).
'$execute_frozen_goals'([G0|Gs]) :-
	'$execute_frozen_goal'(G0,G0),
	'$execute_frozen_goals'(Gs).

%
% X and Y may not be bound (multiple suspensions on the same goal).
%
'$execute_frozen_goal'('$redo_dif'(Done, X, Y), G) :-
	'$redo_dif'(Done, X, Y, G).
'$execute_frozen_goal'('$redo_freeze'(Done, V, Goal), _) :-
	'$redo_freeze'(Done, V, Goal).
'$execute_frozen_goal'('$redo_eq'(Done, X, Y, Goal), G) :-
	'$redo_eq'(Done, X, Y, Goal, G).
'$execute_frozen_goal'('$redo_ground'(Done, X, Goal), _) :-
	'$redo_ground'(Done, X, Goal).

freeze(V, G) :-
	var(V), !,
	'$freeze_goal'(V,G).
freeze(_, G) :-
	'$execute'(G).

'$freeze_goal'(V,VG) :-
	var(VG), !,
	'$current_module'(M),
	'$freeze'(V, '$redo_freeze'(_Done,V,M:VG)).
'$freeze_goal'(V,M:G) :- !,
	'$freeze'(V, '$redo_freeze'(_Done,V,M:G)).
'$freeze_goal'(V,G) :-
	'$current_module'(M),
	'$freeze'(V, '$redo_freeze'(_Done,V,M:G)).

%
%
% Dif is tricky because we need to wake up on the two variables being
% bound together, or on any variable of the term being bound to
% another. Also, the day YAP fully supports infinite rational trees,
% dif should work for them too. Hence, term comparison should not be
% implemented in Prolog.
%
% This is the way dif works. The '$can_unify' predicate does not know
% anything about dif semantics, it just compares two terms for
% equaility and is based on compare. If it succeeds without generating
% a list of variables, the terms are equal and dif fails. If it fails,
% dif succeeds.
%
% If it succeeds but it creates a list of variables, dif creates
% suspension records for all these variables on the '$redo_dif'(V,
% X, Y) goal. V is a flag that says whether dif has completed or not,
% X and Y are the original goals. Whenever one of these variables is
% bound, it calls '$redo_dif' again. '$redo_dif' will then check whether V
% was bound. If it was, dif has succeeded and redo_dif just
% exits. Otherwise, '$redo_dif' will call dif again to see what happened.
%
% Dif needs two extensions from the suspension engine:
%
% First, it needs
% for the engine to be careful when binding two suspended
% variables. Basically, in this case the engine must be sure to wake
% up one of the goals, as they may make dif fail. The way the engine
% does so is by searching the list of suspended variables, and search
% whether they share a common suspended goal. If they do, that
% suspended goal is added to the WokenList.
%
% Second, thanks to dif we may try to suspend on the same variable
% several times. dif calls a special version of freeze that checks
% whether that is in fact the case.
%
dif(X, Y) :- '$can_unify'(X, Y, LVars), !,
	LVars = [_|_], 
	'$dif_suspend_on_lvars'(LVars, '$redo_dif'(_Done, X, Y)).
dif(_, _).


'$dif_suspend_on_lvars'([], _).
'$dif_suspend_on_lvars'([H|T], G) :-
	'$freeze'(H, G),
	'$dif_suspend_on_lvars'(T, G).

%
% This predicate is called whenever a variable dif was suspended on is
% bound. Note that dif may have already executed successfully.
%
% Three possible cases: dif has executed and Done is bound; we redo
% dif and the two terms either unify, hence we fail, or may unify, and
% we try to increase the number of suspensions; last, the two terms
% did not unify, we are done, so we succeed and bind the Done variable.
%
'$redo_dif'(Done, _, _, _) :- nonvar(Done), !.
'$redo_dif'(_, X, Y, G) :-
	'$can_unify'(X, Y, LVars), !,
	LVars = [_|_],
	'$dif_suspend_on_lvars'(LVars, G).
'$redo_dif'('$done', _, _, _).

% If you called nonvar as condition for when, then you may find yourself
% here.
%
% someone else (that is Cond had ;) did the work, do nothing
%
'$redo_freeze'(Done, _, _) :- nonvar(Done), !.
%
% We still have some more conditions: continue the analysis.
%
'$redo_freeze'(Done, _, '$when'(C, G, Done)) :- !,
	'$when'(C, G, Done).
	
%
% check if the variable was really bound
%
'$redo_freeze'(Done, V, G) :- var(V), !,
	'$freeze'(V, '$redo_freeze'(Done,V,G)).
%
% I can't believe it: we're done and can actually execute our
% goal. Notice we have to say we are done, otherwise someone else in
% the disjunction might decide to wake up the goal themselves.
%
'$redo_freeze'('$done', _, G) :-
	'$execute'(G).

%
% eq is a combination of dif and freeze
'$redo_eq'(Done, _, _, _, _) :- nonvar(Done), !.
'$redo_eq'(_, X, Y, _, G) :-
	'$can_unify'(X, Y, LVars),
	LVars = [_|_], !,
	'$dif_suspend_on_lvars'(LVars, G).
'$redo_eq'(Done, _, _, '$when'(C, G, Done), _) :- !,
	'$when'(C, G, Done).
'$redo_eq'('$done', _ ,_ , Goal, _) :-
	'$execute'(Goal).

%
% ground is similar to freeze
'$redo_ground'(Done, _, _) :- nonvar(Done), !.
'$redo_ground'(Done, X, Goal) :-
	'$non_ground'(X, Var), !,
	'$freeze'(Var, '$redo_ground'(Done, X, Goal)).
'$redo_ground'(Done, _, '$when'(C, G, Done)) :- !,
	'$when'(C, G, Done).
'$redo_ground'('$done', _, Goal) :-
	'$execute'(Goal).


%
% support for when/2 built-in
%
when(Conds,Goal) :-
	'$current_module'(Mod),
	'$prepare_goal_for_when'(Goal, Mod, ModG),
	'$when'(Conds, ModG, Done, [], LG), !,
%write(vsc:freezing(LG,Done)),nl,
	'$suspend_when_goals'(LG, Done).
when(_,Goal) :-
	'$execute'(Goal).

%
% support for when/2 like declaration.
%
%
% when will block on a conjunction or disjunction of nonvar, ground,
% ?=, where ?= is both terms being bound together
%
%
'$declare_when'(Cond, G) :-
	'$generate_code_for_when'(Cond, G, Code),
	'$current_module'(Module),
	'$$compile'(Code, Code, 5, Module), fail.
'$declare_when'(_,_).

%
% use a meta interpreter for now
%
'$generate_code_for_when'(Conds, G,
	( G :- '$when'(Conds, ModG, Done, [], LG), !,
	'$suspend_when_goals'(LG, Done)) ) :-
	'$current_module'(Mod),
	'$prepare_goal_for_when'(G, Mod, ModG).


%
% make sure we have module info for G!
%
'$prepare_goal_for_when'(G, Mod, Mod:call(G)) :- var(G), !.
'$prepare_goal_for_when'(M:G, _,  M:G) :- !.
'$prepare_goal_for_when'(G, Mod, Mod:G).
	

%
% now for the important bit
%

% Done is used to synchronise: when it is bound someone else did the
% goal and we can give up.
%
% $when/5 and $when_suspend succeds when there is need to suspend a goal
%
%
'$when'(nonvar(V), G, Done, LG0, LGF) :-
	'$when_suspend'(nonvar(V), G, Done, LG0, LGF).
'$when'(?=(X,Y), G, Done, LG0, LGF) :-
	'$when_suspend'(?=(X,Y), G, Done, LG0, LGF).
'$when'(ground(T), G, Done, LG0, LGF) :-
	'$when_suspend'(ground(T), G, Done, LG0, LGF).
'$when'((C1, C2), G, Done, LG0, LGF) :-
	% leave it open to continue with when.
	(
	    '$when'(C1, '$when'(C2, G, Done), Done, LG0, LGI)
        ->
	    LGI = LGF
        ;
	    % we solved C1, great, now we just have to solve C2!
	    '$when'(C2, G, Done, LG0, LGF)
        ).
'$when'((G1 ; G2), G, Done, LG0, LGF) :-
	'$when'(G1, G, Done, LG0, LGI),
	'$when'(G2, G, Done, LGI, LGF).

%
% Auxiliary predicate called from within a conjunction.
% Repeat basic code for when,  as inserted in first clause for predicate.
%
'$when'(_, _, Done) :-
	nonvar(Done), !.
'$when'(Cond, G, Done) :-
	'$when'(Cond, G, Done, [], LG),
	!,
	'$suspend_when_goals'(LG, Done).
'$when'(_, G, '$done') :-
	'$execute'(G).

%
% Do something depending on the condition!
%
% some one else did the work.
%
'$when_suspend'(_, _, Done, _, []) :- nonvar(Done), !.
%
% now for the serious stuff.
%
'$when_suspend'(nonvar(V), G, Done, LG0, LGF) :-
	'$try_freeze'(V, G, Done, LG0, LGF).
'$when_suspend'(?=(X,Y), G, Done, LG0, LGF) :-
	'$try_eq'(X, Y, G, Done, LG0, LGF).
'$when_suspend'(ground(X), G, Done, LG0, LGF) :-
	'$try_ground'(X, G, Done, LG0, LGF).


'$try_freeze'(V, G, Done, LG0, LGF) :-
	var(V),
	LGF = ['$freeze'(V, '$redo_freeze'(Done, V, G))|LG0].

'$try_eq'(X, Y, G, Done, LG0, LGF) :- 
	'$can_unify'(X, Y, LVars), LVars = [_|_],
	LGF = ['$dif_suspend_on_lvars'(LVars, '$redo_eq'(Done, X, Y, G))|LG0].

'$try_ground'(X, G, Done, LG0, LGF) :-
	'$non_ground'(X, Var),    % the C predicate that succeds if
				  % finding out the term is nonground
				  % and gives the first variable it
				  % finds. Notice that this predicate
				  % must know about svars.
	LGF = ['$freeze'(Var, '$redo_ground'(Done, X, G))| LG0].

%
% When executing a when, if nobody succeeded, we need to create suspensions.
%
'$suspend_when_goals'([], _).
'$suspend_when_goals'(['$freeze'(V, G)|Ls], Done) :-
	var(Done), !,
	'$freeze'(V, G),
	'$suspend_when_goals'(Ls, Done).
'$suspend_when_goals'(['$dif_suspend_on_lvars'(LVars, G)|LG], Done) :-
	var(Done), !,
	'$dif_suspend_on_lvars'(LVars, G),
	'$suspend_when_goals'(LG, Done).
'$suspend_when_goals'([_|_], _).

%
% Support for wait declarations on goals.
% Or we also use the more powerful, SICStus like, "block" declarations.
%
% block or wait declarations must precede the first clause.
%

%
% I am using the simplest solution now: I'll add an extra clause at
% the beginning of the procedure to do this work. This creates a
% choicepoint and make things a bit slower, but it's probably not as
% significant as the remaining overheads.
%
'$block'(Conds) :-
	'$generate_blocking_code'(Conds, _, Code),
	'$current_module'(Module),
	'$$compile'(Code, Code, 5, Module), fail.
'$block'(_).

'$generate_blocking_code'(Conds, G, Code) :-
	'$extract_head_for_block'(Conds, G),
	'$recorded'('$blocking_code','$code'(G,OldConds),R), !,
	erase(R),
	functor(G, Na, Ar),
	'$current_module'(M),
	abolish(M:Na, Ar),
	'$generate_blocking_code'((Conds,OldConds), G, Code).	
'$generate_blocking_code'(Conds, G, (G :- (If, !, when(When, G)))) :-
	'$extract_head_for_block'(Conds, G),
	recorda('$blocking_code','$code'(G,Conds),_),
	'$generate_body_for_block'(Conds, G, If, When).

%
% find out what we are blocking on.
%
'$extract_head_for_block'((C1, _), G) :- !,
	'$extract_head_for_block'(C1, G).
'$extract_head_for_block'(C, G) :-
	functor(C, Na, Ar),
	functor(G, Na, Ar).

%
% If we suspend on the conditions, we should continue
% execution. If we don't suspend we should fail so that we can take
% the next clause. To
% know what we have to do we just test how many variables we suspended
% on ;-).
%

%
% We generate code as follows:
%
% block a(-,-,?) 
%
% (var(A1), var(A2) -> true ; fail), !, when((nonvar(A1);nonvar(A2)),G).
%
% block a(-,-,?), a(?,-, -)
%
% (var(A1), var(A2) -> true ; (var(A2), var(A3) -> true ; fail)), !,
%                   when(((nonvar(A1);nonvar(A2)),(nonvar(A2);nonvar(A3))),G).

'$generate_body_for_block'((C1, C2), G, (Code1 -> true ; Code2), (WhenConds,OtherWhenConds)) :- !,
	'$generate_for_cond_in_block'(C1, G, Code1, WhenConds),
	'$generate_body_for_block'(C2, G, Code2, OtherWhenConds).
'$generate_body_for_block'(C, G, (Code -> true ; fail), WhenConds) :-
	'$generate_for_cond_in_block'(C, G, Code, WhenConds).

'$generate_for_cond_in_block'(C, G, Code, Whens) :-
	C =.. [_|Args],
	G =.. [_|GArgs],
	'$fetch_out_variables_for_block'(Args,GArgs,L0Vars),
	'$add_blocking_vars'(L0Vars, LVars),
	'$generate_for_each_arg_in_block'(LVars, Code, Whens).

'$add_blocking_vars'([], [_]) :- !.
'$add_blocking_vars'(LV, LV).

'$fetch_out_variables_for_block'([], [], []).
'$fetch_out_variables_for_block'(['?'|Args], [_|GArgs], LV) :-
	'$fetch_out_variables_for_block'(Args, GArgs, LV).
'$fetch_out_variables_for_block'(['-'|Args], [GArg|GArgs],
	       [GArg|LV]) :-
	'$fetch_out_variables_for_block'(Args, GArgs, LV).

'$generate_for_each_arg_in_block'([], false, true).
'$generate_for_each_arg_in_block'([V], var(V), nonvar(V)) :- !.
'$generate_for_each_arg_in_block'([V|L], (var(V),If), (nonvar(V);Whens)) :-
	'$generate_for_each_arg_in_block'(L, If, Whens).


%
% The wait declaration is a simpler and more efficient version of block.
%
'$wait'(Na/Ar) :-
	functor(S, Na, Ar),
	arg(1, S, A),
	'$current_module'(M),
	'$$compile'((S :- var(A), !, freeze(A, S)), (S :- var(A), !, freeze(A, S)), 5, M), fail.
'$wait'(_).

frozen(V, G) :- nonvar(V), !,
	'$do_error'(type_error(variable,V),frozen(V,G)).
frozen(V, LG) :-
	'$project'([V],[V],Gs),
	'$simplify_list_of_frozen_goals'(Gs,LG).

'$simplify_list_of_frozen_goals'([],[]).
'$simplify_list_of_frozen_goals'([(_-G)|Gs],[G|NGs]) :-
	'$simplify_list_of_frozen_goals'(Gs,NGs).

'$find_att_vars'([], []).
'$find_att_vars'([V|LGs], [V|AttVars]) :- attvar(V), !,
	'$find_att_vars'(LGs, AttVars).
'$find_att_vars'([_|LGs], AttVars) :-
	'$find_att_vars'(LGs, AttVars).

'$purge_done_goals'([], []).
'$purge_done_goals'([V|G0], GF) :- attvar(V), !,
	'$purge_done_goals'(G0, GF).
'$purge_done_goals'(['$redo_dif'(Done, _ , _)|G0], GF) :- nonvar(Done), !,
	'$purge_done_goals'(G0, GF).
'$purge_done_goals'(['$redo_freeze'(Done, _, _)|G0], GF) :- nonvar(Done), !,
	'$purge_done_goals'(G0, GF).
'$purge_done_goals'(['$redo_freeze'(_Done, _, CallCleanup)|G0], GF) :-
	nonvar(CallCleanup),
	CallCleanup = _:'$clean_call'(_), !,
	'$purge_done_goals'(G0, GF).
'$purge_done_goals'(['$redo_eq'(Done, _, _, _)|G0], GF) :- nonvar(Done), !,
	'$purge_done_goals'(G0, GF).
'$purge_done_goals'(['$redo_ground'(Done, _, _)|G0], GF) :- nonvar(Done), !,
	'$purge_done_goals'(G0, GF).
'$purge_done_goals'([G|G0], [G|GF]) :-
	'$purge_done_goals'(G0, GF).


'$convert_frozen_goal'(V, _, _, V, _) :- attvar(V), !.
'$convert_frozen_goal'('$redo_dif'(Done, X, Y), LV, Done, [X,Y|LV], dif(X,Y)).
'$convert_frozen_goal'('$redo_freeze'(Done, FV, G), LV, Done, [FV|LV], G).
'$convert_frozen_goal'('$redo_eq'(Done, X, Y, G), LV, Done, [X,Y|LV], G).
'$convert_frozen_goal'('$redo_ground'(Done, V, G), LV, Done, [V|LV], G).

'$fetch_same_done_goals'([], _, [], []).
'$fetch_same_done_goals'([V|G0], Done, NL, GF) :- attvar(V), !,
	'$fetch_same_done_goals'(G0, Done, NL, GF).
'$fetch_same_done_goals'(['$redo_dif'(Done, X , Y)|G0], D0, [X,Y|LV], GF) :-
	Done == D0, !,
	'$fetch_same_done_goals'(G0, D0, LV, GF).
'$fetch_same_done_goals'(['$redo_freeze'(Done, V, _)|G0], D0, [V|LV], GF) :-
	Done == D0, !,
	'$fetch_same_done_goals'(G0, D0, LV, GF).
'$fetch_same_done_goals'(['$redo_eq'(Done, X, Y, _)|G0], D0, [X,Y|LV], GF) :-
	Done == D0, !,
	'$fetch_same_done_goals'(G0, D0, LV, GF).
'$fetch_same_done_goals'(['$redo_ground'(Done, G, _)|G0], D0, [G|LV], GF) :-
	Done == D0, !,
	'$fetch_same_done_goals'(G0, D0, LV, GF).
'$fetch_same_done_goals'([G|G0], D0, LV, [G|GF]) :-
	'$fetch_same_done_goals'(G0, D0, LV, GF).


call_residue(Goal,Residue) :-
	var(Goal), !,
	'$do_error'(instantiation_error,call_residue(Goal,Residue)).
call_residue(Module:Goal,Residue) :-
	atom(Module), !,
	'$call_residue'(Goal,Module,Residue).
call_residue(Goal,Residue) :-
	'$current_module'(Module),
	'$call_residue'(Goal,Module,Residue).
	
'$call_residue'(Goal,Module,Residue) :-
	'$read_svar_list'(OldAttsList),
	copy_term_nat(Goal, NGoal),
	( '$set_svar_list'(CurrentAttsList),
	  '$system_catch'(NGoal,Module,Error,'$residue_catch_trap'(Error,OldAttsList)),

	    '$call_residue_continuation'(NGoal,NResidue),
	    ( '$set_svar_list'(OldAttsList),
		copy_term_nat(NGoal+NResidue, Goal+Residue)
		;
		'$set_svar_list'(CurrentAttsList), fail
	    )
           ;
          '$set_svar_list'(OldAttsList), fail
        ).

% make sure we set the suspended goal list to its previous state!
'$residue_catch_trap'(Error,OldAttsList) :-
	'$set_svar_list'(OldAttsList),
	throw(Error).

%
% goal needs to be in a different procedure to catch suspended goals.
%
'$call_residue_continuation'(Goal,Residue) :-
	'$variables_in_term'(Goal,[],LIV),
	'$show_frozen'(Goal,LIV,Residue).

'$purge_and_set_done_goals'([], L, L).
'$purge_and_set_done_goals'([AttV|G0], [_-GS|GF], Atts) :-
	attvar(AttV), !,
	attributes:convert_att_var(AttV, GS),
	'$purge_and_set_done_goals'(G0, GF, Atts).
'$purge_and_set_done_goals'(['$redo_dif'(Done, X , Y)|G0], [LVars-dif(X,Y)|GF], Atts) :-
	var(Done), !,
	Done = '$done',
	'$can_unify'(X, Y, LVars),
	'$purge_and_set_done_goals'(G0, GF, Atts).
'$purge_and_set_done_goals'(['$redo_freeze'(Done, V, G)|G0], [[V]-freeze(G)|GF], Atts) :-
        var(Done), !,
	Done = '$done',
	'$purge_and_set_done_goals'(G0, GF, Atts).
'$purge_and_set_done_goals'(['$redo_eq'(Done, X, Y, G)|G0], [[X,Y]-G|GF], Atts) :-
	var(Done), !,
	Done = '$done',
	'$purge_and_set_done_goals'(G0, GF, Atts).
'$purge_and_set_done_goals'(['$redo_ground'(Done, X, G)|G0], [[X]-G|GF], Atts) :-
	var(Done), !,
	Done = '$done',
	'$purge_and_set_done_goals'(G0, GF, Atts).
'$purge_and_set_done_goals'([_|G0], GF, Atts) :-
	'$purge_and_set_done_goals'(G0, GF, Atts).


'$project'([],_,[]).
'$project'(Vs,_,LGs) :-
	% we don't have constraints yet, so we must be talking about delays.
	'$undefined'(modules_with_attributes(_),attributes), !,
	'$fetch_delays'(Vs, LGs, []).
'$project'([V|LAV],LIV,LDs) :-
	attvar(V), !,
	attributes:modules_with_attributes(LMods),
	'$pick_vars_for_project'(LIV,NLIV),
	'$project_module'(LMods,NLIV,[V|LAV]),
	attributes:all_attvars(NLAV),
	'$convert_att_vars'(NLAV, LIV, LGs),
	'$fetch_delays'(NLAV, LDs, LGs).

'$pick_vars_for_project'([],[]).
'$pick_vars_for_project'([V|L],[V|NL]) :- var(V), !,
	'$pick_vars_for_project'(L,NL).
'$pick_vars_for_project'([_|L],NL) :-
	'$pick_vars_for_project'(L,NL).

'$project_module'([], _, _).
'$project_module'([Mod|LMods], LIV, LAV) :-
	\+ '$undefined'(project_attributes(LIV, LAV), Mod),
	'$execute'(Mod:project_attributes(LIV, LAV)), !,
	attributes:all_attvars(NLAV),
	'$project_module'(LMods,LIV,NLAV).
'$project_module'([_|LMods], LIV, LAV) :-
	'$project_module'(LMods,LIV,LAV).


'$convert_att_vars'(Vs, LIV, []) :-
	% do nothing
	'$undefined'(convert_att_var(Vs,LIV),attributes), !.
'$convert_att_vars'(Vs0, LIV, LGs) :-
	'$sort'(Vs0, Vs),
	'$do_convert_att_vars'(Vs, LIV, LGs).
	
'$do_convert_att_vars'([], _, []).
'$do_convert_att_vars'([V|LAV], LIV, NGs) :-
	attvar(V),
	attributes:convert_att_var(V,G),
	G \= true,
	!,
	'$split_goals_for_catv'(G,V,NGs,IGs),
	'$do_convert_att_vars'(LAV, LIV, IGs).
'$do_convert_att_vars'([_|LAV], LIV, Gs) :-
	'$do_convert_att_vars'(LAV, LIV, Gs).

'$split_goals_for_catv'((G,NG),V,[V-G|Gs],Gs0) :- !,
	'$split_goals_for_catv'(NG,V,Gs,Gs0).
'$split_goals_for_catv'(NG,V,[V-NG|Gs],Gs).

'$vars_interset_for_constr'([V1|_],[V2|_]) :-
	V1 == V2, !.
'$vars_interset_for_constr'([V1|GV],[V2|LIV]) :-
	V1 @< V2, !,
	'$vars_interset_for_constr'(GV,[V2|LIV]).
'$vars_interset_for_constr'([V1|GV],[_|LIV]) :-
	'$vars_interset_for_constr'([V1|GV],LIV).

%'$fetch_delays'(_, L, L) :-
%	'$no_delayed_goals', !.
'$fetch_delays'(Vs, LDs, LAs) :-
	'$do_fetch_delays'(Vs, LGs0),
	'$sort'(LGs0, LGs),
	'$purge_done_goals'(LGs, LG),
	'$clean_list_of_frozen_goals'(LG, LDs, LAs).
	
	
'$do_fetch_delays'([], []).
'$do_fetch_delays'([V|NLAV], GF) :-
	'$frozen_goals'(V,G), !,
	'$hole_in_frozen_goals'(G,GF,G1),
	'$do_fetch_delays'(NLAV, G1).
'$do_fetch_delays'([_|NLAV], GF) :-
	'$do_fetch_delays'(NLAV, GF).


'$hole_in_frozen_goals'([],V,V).
'$hole_in_frozen_goals'([G|Gs],[G|GF],G1) :-
	'$hole_in_frozen_goals'(Gs,GF,G1).

'$clean_list_of_frozen_goals'([], L, L).
'$clean_list_of_frozen_goals'([A|B], Gs, Gs0) :-
	'$convert_list_of_frozen_goals_into_list'([A|B], Gs, Gs0).

'$convert_list_of_frozen_goals_into_list'([A], [LV-G|L], L) :- !,
	'$convert_frozen_goal'(A, [], _, LV0, G0),
	'$clean_bound_args'(LV0, LV1),
	'$sort'(LV1, LV),
	'$process_when'(G0, G).
'$convert_list_of_frozen_goals_into_list'([A|L], OUT, Gs0) :- !,
	'$convert_frozen_goal'(A, LV, Done, NA, G0),
	'$process_when'(G0, Gf),
	'$fetch_same_done_goals'(L, Done, LV, NL),
	'$clean_bound_args'(NA, NA1),
	'$sort'(NA1, LVf),
	( NL = [] -> OUT = [LVf-Gf|Gs0];
	  OUT = [(LVf-Gf)|Gs],
	'$convert_list_of_frozen_goals_into_list'(NL, Gs, Gs0)).


'$clean_bound_args'([], []).
'$clean_bound_args'([NV|L], NL) :- nonvar(NV), !,
	'$clean_bound_args'(L,NL).
'$clean_bound_args'([V|L], [V|NL]) :-
	'$clean_bound_args'(L,NL).

'$process_when'('$when'(_,G,_), NG) :- !,
	'$process_when'(G, NG).
'$process_when'(G, G).

%'$freeze'(V,G) :-
%	attributes:get_att(V, 0, Gs), write(G+Gs),nl,fail.
'$freeze'(V,G) :-
	'$update_att'(V, G).

'$update_att'(V, G) :-
	  attributes:get_module_atts(V, prolog(_,Gs)), !,
	  attributes:put_module_atts(V, prolog(_,[G|Gs])).
'$update_att'(V, G) :-
	attributes:put_module_atts(V, prolog(_,[G])).
	  
'$goal_in'(G,[G1|_]) :- G == G1, !.
'$goal_in'(G,[_|Gs]) :-	
	'$goal_in'(G,Gs).

'$frozen_goals'(V,Gs) :-
	var(V),
	attributes:get_att(V, prolog, 2, Gs), nonvar(Gs).


