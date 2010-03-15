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
* comments:	attribute support for Prolog				 *
*									 *
*************************************************************************/

:- module('$attributes', [
			  project_delayed_goals/2
			  ]).

prolog:get_attr(Var, Mod, Att) :-
	functor(AttTerm, Mod, 2),
	arg(2, AttTerm, Att),
	attributes:get_module_atts(Var, AttTerm).

prolog:put_attr(Var, Mod, Att) :-
	functor(AttTerm, Mod, 2),
	arg(2, AttTerm, Att),
	attributes:put_module_atts(Var, AttTerm).

prolog:del_attr(Var, Mod) :-
	functor(AttTerm, Mod, 2),
	attributes:del_all_module_atts(Var, AttTerm).

prolog:del_attrs(Var) :-
	attributes:del_all_atts(Var).

prolog:get_attrs(AttVar, SWIAtts) :-
	attributes:get_all_swi_atts(AttVar,SWIAtts).

prolog:put_attrs(_, []).
prolog:put_attrs(V, Atts) :-
	cvt_to_swi_atts(Atts, YapAtts),
	attributes:put_att_term(V, YapAtts).

cvt_to_swi_atts([], _).
cvt_to_swi_atts(att(Mod,Attribute,Atts), ModAttribute) :-
	ModAttribute =.. [Mod, YapAtts, Attribute],
	cvt_to_swi_atts(Atts, YapAtts).

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
prolog:'$wake_up_goal'([Module1|Continuation], LG) :-
	execute_woken_system_goals(LG),
	do_continuation(Continuation, Module1).


%
% in the first two cases restore register  immediately and proceed
% to continuation. In the last case take care with modules, but do
% not act as if a meta-call.
% 
%
do_continuation('$cut_by'(X), _) :- !,
	'$$cut_by'(X).
do_continuation('$restore_regs'(X), _) :- !,
	'$restore_regs'(X).
do_continuation('$restore_regs'(X,Y), _) :- !,
	'$restore_regs'(X,Y).
do_continuation(Continuation, Module1) :-
	execute_continuation(Continuation,Module1).

execute_continuation(Continuation, Module1) :-
	'$undefined'(Continuation, Module1), !,
        '$undefp'([Module1|Continuation]).
execute_continuation(Continuation, Mod) :-
         % do not do meta-expansion nor any fancy stuff.
	'$execute0'(Continuation, Mod).


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
	attributes:get_all_swi_atts(V,SWIAtts),
	(
	 '$undefined'(woken_att_do(V, New, LGoals, DoNotBind), attributes)
	->
	 LGoals = [],
	 DoNotBind = false
	;
	 attributes:woken_att_do(V, New, LGoals, DoNotBind)
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
	('$undefined'(attr_unify_hook(Att,Binding), Mod)
	->
	 true
	;
	 Mod:attr_unify_hook(Att, Binding)
	),
	do_hook_attributes(Atts, Binding).


lcall([]).
lcall([Mod:Gls|Goals]) :-
	lcall2(Gls,Mod),
	lcall(Goals).

lcall2([], _).
lcall2([Goal|Goals], Mod) :-
	call(Mod:Goal),
	lcall2(Goals, Mod).



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

prolog:copy_term(Term, Copy, Goals) :-
	term_variables(Term, TVars),
	'$get_goalist_from_attvars'(TVars, Goals0),
	copy_term_nat([Term|Goals0], [Copy|Goals]).

prolog:call_residue(Goal,Residue) :-
	var(Goal), !,
	'$do_error'(instantiation_error,call_residue(Goal,Residue)).
prolog:call_residue(Module:Goal,Residue) :-
	atom(Module), !,
	call_residue(Goal,Module,Residue).
prolog:call_residue(Goal,Residue) :-
	'$current_module'(Module),
	call_residue(Goal,Module,Residue).

call_residue(Goal,Module,Residue) :-
	call(Module:Goal).

project_delayed_goals(G,LGs) :-
	(
	 current_predicate(attributes:modules_with_attributes/1), false
	->
% SICStus compatible step,
% just try to simplify store  by projecting constraints
% over query variables.
% called by top_level to find out about delayed goals
	 attributes:all_attvars(LAV),
	 LAV = [_|_],
	 !,
	 project_attributes(LAV, G),
	% now get a list of frozen goals.
	 attributes:all_attvars(NLAV)
	;
	 attributed(G, NLAV),
	 NLAV = [_|_]
	),
	!,
	get_goalist_from_attvars(NLAV, LGs).
project_delayed_goals(_,[]).


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
project_attributes(_, _) :-
	'$undefined'(modules_with_attributes(_),attributes), !.
project_attributes(AllVs, G) :-
	attributes:modules_with_attributes(LMods),
	term_variables(G, InputVs),
	pick_att_vars(InputVs, AttIVs),
	project_module(LMods, AttIVs, AllVs).

pick_att_vars([],[]).
pick_att_vars([V|L],[V|NL]) :- attvar(V), !,
	pick_att_vars(L,NL).
pick_att_vars([_|L],NL) :-
	pick_att_vars(L,NL).

project_module([], _, _).
project_module([Mod|LMods], LIV, LAV) :-
	'$pred_exists'(project_attributes(LIV, LAV),Mod),
	'$notrace'(Mod:project_attributes(LIV, LAV)), !,
	attributes:all_attvars(NLAV),
	project_module(LMods,LIV,NLAV).
project_module([_|LMods], LIV, LAV) :-
	project_module(LMods,LIV,LAV).

% given a list of attributed variables, generate a conjunction of goals.
%
get_conj_from_attvars(TVars, Goals) :-
	get_goalist_from_attvars(TVars, [], GoalList, []),
	list_to_conjunction(GoalList, Goals).

%
% same, but generate list
%
get_goalist_from_attvars(TVars, GoalList) :-
	get_goalist_from_attvars(TVars, GoalList, []).

get_goalist_from_attvars([]) --> [].
get_goalist_from_attvars([V|TVars]) -->
	get_goalist_from_attvar(V),
	get_goalist_from_attvars(TVars).

get_goalist_from_attvar(V) --> { attvar(V) }, !,
	{ attributes:get_all_atts(V, AllAtts) },
	all_atts_to_goals(AllAtts, V).
get_goalist_from_attvar(_) --> [].

all_atts_to_goals(AllAtts, _) --> { var(AllAtts) }, !.
all_atts_to_goals(AllAtts, V) --> 
	{
	 functor(AllAtts, Mod, _),
	 arg(1, AllAtts, MoreAtts)
	},
	attgoals_for_module(Mod, V, AllAtts),
	all_atts_to_goals(MoreAtts, V).

%
% check constraints for variable
%
attgoals_for_module(Mod, V, _Gs, GoalListF, GoalList0) :-
	 % SWI, HProlog
	'$pred_exists'(attribute_goals(V,GoalListF,GoalList0), Mod), !,
	(
	 '$notrace'(Mod:attribute_goals(V,GoalListF,GoalList0))
	->
	 true
	;
	 GoalListF = GoalList0
	).
attgoals_for_module(Mod, V, _, GoalListF, GoalList0) :-
	 % SICStus
	'$pred_exists'(attribute_goal(V,G),Mod), !,
	(
	 '$notrace'(Mod:attribute_goal(V,G))
	->
	 GoalListF = [G|GoalList0]
	;
	 GoalListF = GoalList0
	).
attgoals_for_module(Mod, V, _, GoalList, GoalList).

list_to_conjunction([], true).
list_to_conjunction([G], G)  :- !.
list_to_conjunction([G|GoalList], (G,Goals0))  :-
	list_to_conjunction(GoalList, Goals0).

