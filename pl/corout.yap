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
/**
 * @file   corout.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Mon Nov 16 22:47:27 2015
 * *
 */


:-  module(coroutining,
		 [	     
/*	      prolog:dif/1,
	      prolog:dif/2,
	      prolog:when/2,
%	      prolog:block/1,
	      prolog:wait/1,
	      prolog:frozen/2,
	      prolog:freeze/2 */
		 ]).

:-  op(1150, fx, prolog:block).

:- use_system_module( '$_boot', ['$$compile'/4]).


%:- use_module( attributes, [attvars_residuals/3]).



/**
 *  @addtogroup   New_Style_Attribute_Declarations
 *  @{
 *


Prolog uses a simple left-to-right flow of control. It is sometimes
convenient to change this control so that goals will only execute when
sufficiently instantiated. This may result in a more "data-driven"
execution, or may be necessary to correctly implement extensions such
as negation by failure.

Initially, YAP used a separate mechanism for co-routining. Nowadays, YAP uses
attributed variables to implement co-routining.

 *
*/


/** @pred attr_unify_hook(+ _AttValue_,+ _VarValue_)


Hook that must be defined in the module an attributed variable refers
to. Is is called <em>after</em> the attributed variable has been
unified with a non-var term, possibly another attributed variable.
[ _AttValue_ is the attribute that was associated to the variable
in this module and  _VarValue_ is the new value of the variable.
Normally this predicate fails to veto binding the variable to
 _VarValue_, forcing backtracking to undo the binding.  If
 _VarValue_ is another attributed variable the hook often combines
the two attribute and associates the combined attribute with
 _VarValue_ using put_attr/3.


*/

coroutining:attr_unify_hook(Delay, V) :-
    strip_module(Delay,_,D),
	wake_delay(D, V).

%
% Interface to attributed variables.
%
wake_delay((C1,C2), V) :-
       !,
       wake_delay(C1, V),
       wake_delay(C2, V).
wake_delay(when( _, _Goal, Done), _V) :-
	nonvar(Done),
	!.
wake_delay(when( ground(X), Goal, Done), _V) :-
       term_variables(X,[Var|_]),
       !,
       internal_freeze(Var, when(ground(X),Goal,Done)).
wake_delay(when( ?=(X,Y), Goal, Done), _V) :-
	constraining_variables(X, Y, LBindings),
	!,
	(
	LBindings = []
	->
	Done = true,
	call(Goal)
	;
	dif_suspend_on_lvars(LBindings, when( ?=(X,Y), Goal, _Done))
	).
wake_delay(when( _, Goal, Done), _V) :-
	Done=true,
	call(Goal).


attribute_goals(Var)-->
	{ get_attr(Var, 'coroutining', Delays) },
	{ nonvar( Delays ) },
	attgoals_for_delays(Delays, Var).

attgoals_for_delays((G1,G2), V) -->
    !,
    attgoals_for_delays(G1, V),
    attgoals_for_delays(G2, V).
attgoals_for_delays(G, V) -->
    !,
    attgoal_for_delay(G, V).

attgoal_for_delay(when(_, _,Done), _V) -->
        { nonvar(Done)},
	!.
attgoal_for_delay(when(_, dif(X,Y),_Done), _V) -->
	!,
	[ dif(X,Y) ].		
attgoal_for_delay(when(A, when(B,G,D),	_Done), _V) -->
	!,
	attgoal_for_delay( when((A,B),G,D) , _V).
attgoal_for_delay(when(X, Goal,Done), _V) -->
	{ var(Done) },  !,  
	[ when((X),Goal) ].   
attgoal_for_delay(_, _V) --> [].

remove_whendeclarations(when(Cond,Goal,_), when(Cond,NoWGoal)) :- !,
	remove_whendeclarations(Goal, NoWGoal).
remove_whendeclarations(Goal, Goal).

/** 
@}

@defgroup CohYroutining Co-Routining

@ingroup AttributedVariables

@{


Prolog uses a simple left-to-right flow of control. It is sometimes
convenient to change this control so that goals will only execute when
sufficiently instantiated. This may result in a more "data-driven"
execution, or may be necessary to correctly implement extensions such
as negation by failure.


Initially, YAP used a separate mechanism for co-routining. Nowadays, YAP uses
attributed variables to implement co-routining.

*/


/**
  @pred freeze(? _X_,: _G_)

Delay execution of goal  _G_ until the variable  _X_ is bound.


*/
prolog:freeze(V, G) :-
	var(V), !,
	when(nonvar(V),G).
prolog:freeze(_, G) :-
	'$execute'(G).

prolog:dif([]).
prolog:dif([Term | Terms]) :-
    dif_cs(Terms, Term),
    dif(Terms).

dif_cs([], _).
dif_cs([Next| Terms], Term) :-	 
    dif(Term, Next),
    dif_cs(Terms, Term).




/** @pred dif( _X_, _Y_)


Succeed if the two arguments do not unify. A call to dif/2 will
suspend if unification may still succeed or fail, anxd will fail if they
always unify.




 Dif is tricky because we need to wake up on the two variables being
 bound together, or on any variable of the term being bound to
 another. Also, the day YAP fully supports infinite rational trees,
 dif should work for them too. Hence, term comparison should not be
 implemented in Prolog.

 This is the way dif works. The '$constraining_variables' predicate does not know
 anything about dif semantics, it just compares two terms for
  equaility and is based on compare. If it succeeds without generating
 a list of variables, the terms are equal and dif fails. If it fails,
 dif succeeds.

 If it succeeds but it creates a list of variables, dif creates
 suspension records for all these variables on the '$redo_dif'(X, Y, V) goal. V is a flag that says whether dif has completed or not,
 X and Y are the original goals. Whenever one of these variables is
 bound, it calls '$redo_dif' again. '$redo_dif' will then check whether V
 was bound. If it was, dif has succeeded and redo_dif just
 exits. Otherwise, '$redo_dif' will call dif again to see what happened.

 Dif needs two extensions from the suspension engine:

 First, it needs
 for the engine to be careful when binding two suspended
j variables. Basically, in this case the engine must be sure to wake
 up one of the goals, as they may make dif fail. The way the engine
 does so is by searching the list of suspended variables, and search
 whether they share a common suspended goal. If they do, that
 suspended goal is added to the WokenList.

 Second, thanks to dif we may try to suspend on the same variable
 several times. dif calls a special version of freeze that checks
 whether that is in fact the case.

*/
prolog:dif(X, Y) :-
    constraining_variables(X, Y, LVars),
    !,
    LVars = [_|_],
    dif_suspend_on_lvars(LVars, when( ?=(X,Y), dif(X,Y), _Done)).
prolog:dif(_,_).


dif_suspend_on_lvars([], _).
dif_suspend_on_lvars([H|T], G) :-
    internal_freeze(H, G),
    dif_suspend_on_lvars(T, G).


%
% support for when/2 built-in
%
/** @pred when(+ _C_,: _G_)


Delay execution of goal  _G_ until the conditions  _C_ are
satisfied. The conditions are of the following form:

+ _C1_, _C2_
Delay until both conditions  _C1_ and  _C2_ are satisfied.
+ _C1_; _C2_
Delay until either condition  _C1_ or condition  _C2_ is satisfied.
+ ?=( _V1_, _C2_)
Delay until terms  _V1_ and  _V1_ have been unified.
+ nonvar( _V_)
Delay until variable  _V_ is bound.
+ ground( _V_)
Delay until variable  _V_ is ground.


Note that when/2 will fail if the conditions fail.



*/
prolog:when((C1,C2), Goal) :-
    !,
    prolog:when(C1,when(C2,Goal)).
prolog:when(Conds,Goal) :-
    strip_module(Goal, Mod, G),
    prepare_goal_for_when(G, Mod, ModG),
    when(Conds, ModG, _Done).

when((C1;C2), Goal, Done) :-
    !,
    when(C1, Goal, Done),
    when(C2, Goal, Done).
when(C, Goal, Done) :-
    ( Done == true -> true ;
      call(C) -> Done = true, call(Goal) ;
      when_suspend(C, Goal, Done) ).

?=(X,Y) :- X==Y.

%
% support for when/2 like declaration.
'$declare_when'(Cond, G) :-
	generate_code_for_when(Cond, G, Code),
	compile_clause(Code), fail.
'$declare_when'(_,_).


%
% use a meta interpreter for now
%
generate_code_for_when(Conds, G,
	( G :- when(Conds, ModG, Done, [], LG), !,
	suspend_whengoals(LG, Done)) ) :-
	'$current_module'(Mod),
	prepare_goal_for_when(G, Mod, ModG).


%
% make sure we have module info for G!
%
prepare_goal_for_when(G, Mod, Mod:call(G)) :- var(G), !.
prepare_goal_for_when(M:G, _,  M:G) :- !.
prepare_goal_for_when(G, Mod, Mod:G).


%
% now for the important bit
%

% Done is used to synchronise: when it is bound someone else did the
% goal and we can give up.
%
%
% now for the serious stuff.
%

when_suspend(?=(X, Y), G, Done) :-
    constraining_variables(X, Y, LVars),
    dif_suspend_on_lvars(LVars, when( X=Y, G, Done)).

when_suspend(nonvar(X), G, Done) :-
    var(X),
    !,
    internal_freeze(X, when(nonvar(X),G,Done)).

when_suspend(ground(X), G, Done) :-
    non_ground(X,Var),
    !,
    % the C predicate that succeds if
    % finding out the term is nonground
    % and gives the first variable it
    % finds. Notice that this predicate
    % must know about svars.
    internal_freeze(Var, when(ground(X),G,Done)).

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
prolog:'$block'(Conds) :-
    generate_blocking_code(Conds, _, Code),
    compile_clause(Code), fail.
prolog:'$block'(_).

generate_blocking_code(Conds, G, Code) :-
	extract_head_for_block(Conds, G),
	recorded('$blocking_code','$code'(G,OldConds),R), !,
	erase(R),
	functor(G, Na, Ar),
	'$current_module'(M),
	abolish(M:Na, Ar),
	generate_blocking_code((Conds,OldConds), G, Code).
generate_blocking_code(Conds, G, (G :- (If, !, when(When, G)))) :-
	extract_head_for_block(Conds, G),
	recorda('$blocking_code','$code'(G,Conds),_),
	generate_body_for_block(Conds, G, If, When).

%
% find out what we are blocking on.
%
extract_head_for_block((C1, _), G) :- !,
	extract_head_for_block(C1, G).
extract_head_for_block(C, G) :-
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

generate_body_for_block((C1, C2), G, (Code1 -> true ; Code2), (WhenConds,OtherWhenConds)) :- !,
       generate_for_cond_in_block(C1, G, Code1, WhenConds),
       generate_body_for_block(C2, G, Code2, OtherWhenConds).
generate_body_for_block(C, G, (Code -> true ; fail), WhenConds) :-
       generate_for_cond_in_block(C, G, Code, WhenConds).

generate_for_cond_in_block(C, G, Code, Whens) :-
	C =.. [_|Args],
	G =.. [_|GArgs],
	fetch_out_variables_for_block(Args,GArgs,L0Vars),
	add_blocking_vars(L0Vars, LVars),
	generate_for_each_arg_in_block(LVars, Code, Whens).

add_blocking_vars([], [_]) :- !.
add_blocking_vars(LV, LV).

fetch_out_variables_for_block([], [], []).
fetch_out_variables_for_block(['?'|Args], [_|GArgs], LV) :-
	fetch_out_variables_for_block(Args, GArgs, LV).
fetch_out_variables_for_block(['-'|Args], [GArg|GArgs],
	       [GArg|LV]) :-
	fetch_out_variables_for_block(Args, GArgs, LV).

generate_for_each_arg_in_block([], false, true).
generate_for_each_arg_in_block([V], var(V), nonvar(V)) :- !.
generate_for_each_arg_in_block([V|L], (var(V),If), (nonvar(V);Whens)) :-
	generate_for_each_arg_in_block(L, If, Whens).


%
/**

@pred wait(_G_)
The argument to `wait/1` is a predicate descriptor or a conjunction
of these predicates. These predicates will suspend until their first
argument is bound.

The wait declaration is a simpler and more efficient version of block.

*/
prolog:wait((X,Y)) :-
    !,
    wait(X),
    wait(Y).
prolog:wait(G) :-
    arg(1,G,A),
    freeze(A,G).

'$wait'(Na/Ar) :-
	functor(S, Na, Ar),
	arg(1, S, A),
	compile_clause((S :- var(A), !, freeze(A, S))), fail.
'$wait'(_).

/** @pred frozen( _X_, _G_)


Unify  _G_ with a conjunction of goals suspended on variable  _X_,
or `true` if no goal has suspended. Also succeeds if _X_ is bound.	


*/
prolog:frozen(V, LG) :-
    var(V),
    attributes:attvars_residuals([V], Gs, []),
    simplify_frozen( Gs, LGs ),
    list_to_conj( LGs, LG ).
prolog:frozen(_V, true).

conj_to_list( (A,B) ) -->
    !,
    conj_to_list( A ),
    conj_to_list( B ).
conj_to_list( true ) --> !.
conj_to_list( A ) --> [A].
     
simplify_frozen( [_:freeze(_, G)|Gs], [G|NGs] ) :-
    simplify_frozen( Gs,NGs ).
simplify_frozen( [_:when(_, G)|Gs], [G|NGs] ) :-
    simplify_frozen( Gs,NGs ).
simplify_frozen( [_:G|Gs], [G|NGs] ) :-
    simplify_frozen( Gs,NGs ).
simplify_frozen( [], [] ).

list_to_conj([], true).
list_to_conj([El], El).

list_to_conj([E,E1|Els], (E,C) ) :-
    list_to_conj([E1|Els], C).

%internal_freeze(V,G) :-
%	attributes:get_att(V, 0, Gs), write(G+Gs),nl,fail.
internal_freeze(V,G) :-
	update_att(V, G).

update_att(V, G) :-
    attributes:get_module_atts(V, att('coroutining',Gs,[])),
    !,
    (	not_cjmember(G, Gs, NGs)  ->
	attributes:put_module_atts(V, att('coroutining',(NGs),[]))
    ;
    true
    ).
update_att(V, G) :-
	attributes:put_module_atts(V, att('coroutining',G,[])).


not_cjmember(A, V, A ) :-
    var(V), !,
    fail.
not_cjmember(A, true, A ) :-
    !.
not_cjmember(A, [], A ) :-
    !.
not_cjmember(A, (G,H),(G,NH) ) :-
    !,
    A\==G ,
    not_cjmember(A,H,NH).
not_cjmember(A, G,  (G,A)) :-
    G\==A.

first_att(T, V) :-
    term_variables(T, Vs),
    check_first_attvar(Vs, V).

check_first_attvar([V|_Vs], V0) :- attvar(V), !, V == V0.
check_first_attvar([_|Vs], V0) :-
	check_first_attvar(Vs, V0).

/**
  @}
*/
