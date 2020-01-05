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
* File:		atts.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:
	attribute support for Prolog				 *
*									 *
*************************************************************************/

/**
  @file attributes.yap

@defgroup New_Style_Attribute_Declarations hProlog and SWI-Prolog style Attribute Declarations
@ingroup attributes

  @{

*/

:- module( attributes, [delayed_goals/4,
all_attvars/1,
        bind_attvar/1,
        del_all_atts/1,
        del_all_module_atts/2,
        get_all_swi_atts/2,
        get_module_atts/2,
        modules_with_attributes/1,
        put_att_term/2,
        put_module_atts/2,
        unbind_attvar/1,
        woken_att_do/4]) .

:- use_system_module( '$_boot', ['$undefp'/1]).

:- use_system_module( '$_errors', ['$do_error'/2]).

%:- use_system_module( '$coroutining', [attr_unify_hook/2]).

:- dynamic attributes:existing_attribute/4.
:- dynamic attributes:modules_with_attributes/1.
:- dynamic attributes:attributed_module/3.

    :- multifile
        attributes:attributed_module/3.

:- dynamic existing_attribute/4.
:- dynamic modules_with_attributes/1.
:- dynamic attributed_module/3.



/** @pred copy_term(? _TI_,- _TF_,- _Goals_)

Term  _TF_ is a variant of the original term  _TI_, such that for
each variable  _V_ in the term  _TI_ there is a new variable  _V'_
in term  _TF_ without any attributes attached.  Attributed
variables are thus converted to standard variables.   _Goals_ is
unified with a list that represents the attributes.  The goal
`maplist(call, _Goals_)` can be called to recreate the
attributes.

Before the actual copying, `copy_term/3` calls
`attribute_goals/1` in the module where the attribute is
defined.


*/
prolog:copy_term(Term, NTerm, LGs) :-
	term_attvars(Term, Vs),
	(   Vs == []
	->  LGs = [], copy_term_nat(Term,NTerm)
	;
	findall(NTerm+NGs,
	 (attributes:attvars_residuals(Vs, Gs, []),
	  copy_term_nat(Term+Gs,NTerm+NGs)),
	  [NTerm+LGs])
	   ).

attvars_residuals([]) --> [].
attvars_residuals([V|Vs]) -->
	{ nonvar(V) }, !,
	attvars_residuals(Vs).
attvars_residuals([V|Vs]) -->
	(   { get_attrs(V, As) }
	->  attvar_residuals(As, V)
	;   []
	),
	attvars_residuals(Vs).

%
% wake_up_goal is called by the system whenever a suspended goal
% resumes.
%

/* The first case may happen if this variable was used for dif.
   In this case, we need a way to keep the original
   suspended goal around
*/
prolog:'$wake_up_goal'(Continuation, LG) :-
    execute_woken_system_goals(LG),
    call(Continuation).

execute_woken_system_goals([]).
execute_woken_system_goals(['$att_do'(V,New)|LG]) :-
	execute_woken_system_goals(LG),
	call_atts(V,New).

%
% what to do when an attribute gets bound
%
call_atts(V,_) :-
	nonvar(V), !.
call_atts(V,_) :-
	'$att_bound'(V), !.
call_atts(V,New) :-
    attributes:get_attrs(V,SWIAtts),
    (
	predicate_property(M:woken_att_do(V, New, LGoals, DoNotBind),number_of_clauses(N)), N>=1
	->
	 M:woken_att_do(V, New, LGoals, DoNotBind)
	;
	 LGoals = [],
	 DoNotBind = false
	),
	( DoNotBind == true
	->
	  attributes:unbind_attvar(V)
	;
	  attributes:bind_attvar(V)
	),
	do_hook_attributes(SWIAtts, New),
	lcall(LGoals).

do_hook_attributes([], _).
do_hook_attributes(att(Mod,Att,Atts), Binding) :-
	ignore((
	     predicate_property(Mod:attr_unify_hook(_,_), number_of_clauses(N)),
	     N>=1->
		 Mod:attr_unify_hook(Att, Binding)
	)),
	do_hook_attributes(Atts, Binding).


lcall([]).
lcall([G|Goals]) :-
	G,!,
	lcall(Goals).

lcall2([], _).

lcall2([Goal|Goals], Mod) :-
	call(Goal),
	lcall2(Goals, Mod).



/** @pred call_residue_vars(: _G_, _L_)



Call goal  _G_ and unify  _L_ with a list of all constrained variables created <em>during</em> execution of  _G_:

~~~~~
  ?- dif(X,Z), call_residue_vars(dif(X,Y),L).
dif(X,Z), call_residue_vars(dif(X,Y),L).
L = [Y],
dif(X,Z),
dif(X,Y) ? ;

no
~~~~~
 */
prolog:call_residue_vars(Goal,Residue) :-
	attributes:all_attvars(Vs0),
	call(Goal),
	attributes:all_attvars(Vs),
	% this should not be actually strictly necessary right now.
	% but it makes it a safe bet.
	sort(Vs, Vss),
	sort(Vs0, Vs0s),
	'$ord_remove'(Vss, Vs0s, Residue).

'$ord_remove'([], _, []).
'$ord_remove'([V|Vs], [], [V|Vs]).
'$ord_remove'([V1|Vss], [V2|Vs0s], Residue) :-
	( V1 == V2 ->
	  '$ord_remove'(Vss, Vs0s, Residue)
	;
	  V1 @< V2 ->
	  Residue = [V1|ResidueF],
	  '$ord_remove'(Vss, [V2|Vs0s], ResidueF)
	;
	  '$ord_remove'([V1|Vss], Vs0s, Residue)
	).

/** @pred attribute_goals(+ _Var_,- _Gs_,+ _GsRest_)



This nonterminal, if it is defined in a module, is used by  _copy_term/3_
to project attributes of that module to residual goals. It is also
used by the toplevel to obtain residual goals after executing a query.


Normal user code should deal with put_attr/3, get_attr/3 and del_attr/2.
The routines in this section fetch or set the entire attribute list of a
variables. Use of these predicates is anticipated to be restricted to
printing and other special purpose operations.

*/

/** @pred Module:attribute_goal( -Var, Goal)

User-defined procedure, called to convert the attributes in  _Var_ to
a  _Goal_. Should fail when no interpretation is available.
 */
attvar_residuals(att(Module,Value,As), V) -->
	(   { nonvar(V) }
	->  % a previous projection predicate could have instantiated
	    % this variable, for example, to avoid redundant goals
	    []
	;
	generate_goals(  V, As, Value,  Module)
    ).

    generate_goals( V, _, Value, Module) -->
        {
	    attributes:module_has_attributes(Module)  },
    	    %  like run, put attributes back first
    	    { Value =.. [att,Module,Vs,_],
    	      NValue =.. [att,Module,Vs,_],
    	      put_attr(V,Module,NValue)
    	    },
            {
		'$current_predicate'(attribute_goal,Module,attribute_goal(_,_),_) },
	    { call(Module:attribute_goal(V, Goal)) },
	    dot_list(Goal,Module),
            { put_attr(V, Module, Value) }.
generate_goals( V, As, Value   , Module) -->
    fetch_module(V, att(Module,Value,As) ).

fetch_module(_,V) --> {var(V)}, !.
fetch_module(_,[]) --> !.
fetch_module(V,Value) -->
    { Value = att(Module,_,OVal)} ,
	 {'$current_predicate'(attribute_goal,Module,attribute_goals(_,_,_),_)},
     Module:attribute_goals(V),
     !,
     fetch_module(V,OVal).
fetch_module(V,Value) -->
    { Value = att(_Module,_,OVal)} ,
	  fetch_module(V,OVal).



attributes:module_has_attributes(Mod) :-
        attributes:attributed_module(Mod, _, _), !.


list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

dot_list((A,B),M) --> !, dot_list(A,M), dot_list(B,M).
dot_list(A,M)	--> [M:A].

delete_attributes(Term) :-
	term_attvars(Term, Vs),
	delete_attributes_(Vs).

delete_attributes_([]).
delete_attributes_([V|Vs]) :-
	del_attrs(V),
	delete_attributes_(Vs).



/** @pred call_residue(: _G_, _L_)



Call goal  _G_. If subgoals of  _G_ are still blocked, return
a list containing these goals and the variables they are blocked in. The
goals are then considered as unblocked. The next example shows a case
where dif/2 suspends twice, once outside call_residue/2,
and the other inside:

~~~~~
?- dif(X,Y),
       call_residue((dif(X,Y),(X = f(Z) ; Y = f(Z))), L).

X = f(Z),
L = [[Y]-dif(f(Z),Y)],
dif(f(Z),Y) ? ;

Y = f(Z),
L = [[X]-dif(X,f(Z))],
dif(X,f(Z)) ? ;

no
~~~~~
The system only reports one invocation of dif/2 as having
suspended.


*/
prolog:call_residue(Module:Goal,Residue) :-
	'$yap_strip_module'(Goal,Module,TGoal),
	must_be_callable(Goal),
	prolog:call_residue_vars(Module:TGoal,NewAttVars),
	(
	 attributes:modules_with_attributes([_|_])
	->
	 project_attributes(NewAttVars, Module:TGoal)
	;
	 true
	),
	copy_term(Goal, _NGoal, Residue).

attributes:delayed_goals(G, Vs, NVs, Gs) :-
	project_delayed_goals(G),
%	term_factorized([G|Vs], [_|NVs], Gs).
	copy_term(G+Vs, _+NVs, Gs).

project_delayed_goals(G) :-
% SICStus compatible step,
% just try to simplify store  by projecting constraints
% over query variables.
% called by top_level to find out about delayed goals
	attributes:modules_with_attributes([_|_]),
	attributes:all_attvars(LAV),
	LAV = [_|_],
	project_attributes(LAV, G), !.
project_delayed_goals(_).


attributed(G, Vs) :-
	term_variables(G, LAV),
	att_vars(LAV, Vs).

att_vars([], []).
att_vars([V|LGs], [V|AttVars]) :- attvar(V), !,
	att_vars(LGs, AttVars).
att_vars([_|LGs], AttVars) :-
	att_vars(LGs, AttVars).

% make sure we set the suspended goal list to its previous state!
% make sure we have installed a SICStus like constraint solver.

/** @pred Module:project_attributes( +AttrVars, +Goal)



Given a goal _Goal_ with variables  _QueryVars_ and list of attributed
variables  _AttrVars_, project all attributes in  _AttrVars_ to
 _QueryVars_. Although projection is constraint system dependent,
typically this will involve expressing all constraints in terms of

 _QueryVars_ and considering all remaining variables as existentially
quantified.

Projection interacts with attribute_goal/2 at the Prolog top
level. When the query succeeds, the system first calls
project_attributes/2. The system then calls
attribute_goal/2 to get a user-level representation of the
constraints. Typically, project_attributes/2 will convert from the
original constraints into a set of new constraints on the projection,
and these constraints are the ones that will have an
attribute_goal/2 handler.
 */
project_attributes(AllVs, G) :-
	attributes:modules_with_attributes(LMods),
	LMods = [_|_],
	term_variables(G, InputVs),
	pick_att_vars(InputVs, AttIVs),
	project_module(LMods, AttIVs, AllVs).

pick_att_vars([],[]).
pick_att_vars([V|L],[V|NL]) :- attvar(V), !,
	pick_att_vars(L,NL).
pick_att_vars([_|L],NL) :-
	pick_att_vars(L,NL).

project_module([], _, []).
project_module([Mod|LMods], LIV, LAV) :-
	current_predicate(Mod:project_attributes/2),
	call(Mod:project_attributes(LIV, LAV)), !,
	attributes:all_attvars(NLAV),
	project_module(LMods,LIV,NLAV).
project_module([_|LMods], LIV, LAV) :-
	project_module(LMods,LIV,LAV).

%% @}
