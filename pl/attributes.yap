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


/** @defgroup Attributed_Variables Attributed Variables
@ingroup YAPExtensions

YAP supports attributed variables, originally developed at OFAI by
Christian Holzbaur. Attributes are a means of declaring that an
arbitrary term is a property for a variable. These properties can be
updated during forward execution. Moreover, the unification algorithm is
aware of attributed variables and will call user defined handlers when
trying to unify these variables.

Attributed variables provide an elegant abstraction over which one can
extend Prolog systems. Their main application so far has been in
implementing constraint handlers, such as Holzbaur's CLPQR, Fruewirth
and Holzbaur's CHR, and CLP(BN). 

Different Prolog systems implement attributed variables in different
ways. Originally, YAP  used the interface designed by SICStus
Prolog. This interface is still
available through the <tt>atts</tt> library, and is still used by CLPBN.

From YAP-6.0.3 onwards we recommend using the hProlog, SWI style
interface. We believe that this design is easier to understand and
work with. Most packages included in YAP that use attributed
variables, such as CHR, CLP(FD), and CLP(QR), rely on the SWI-Prolog
interface.

@{
 */



/** @defgroup New_Style_Attribute_Declarations hProlog and SWI-Prolog style Attribute Declarations
@ingroup Attributed_Variables
@{

The following documentation is taken from the SWI-Prolog manual.

Binding an attributed variable schedules a goal to be executed at the
first possible opportunity. In the current implementation the hooks are
executed immediately after a successful unification of the clause-head
or successful completion of a foreign language (built-in) predicate. Each
attribute is associated to a module and the hook attr_unify_hook/2 is
executed in this module.  The example below realises a very simple and
incomplete finite domain reasoner.

~~~~~
:- module(domain,
      [ domain/2            % Var, ?Domain
      ]).
:- use_module(library(ordsets)).

domain(X, Dom) :-
    var(Dom), !,
    get_attr(X, domain, Dom).
domain(X, List) :-
    list_to_ord_set(List, Domain),
    put_attr(Y, domain, Domain),
    X = Y.

%    An attributed variable with attribute value Domain has been
%    assigned the value Y

attr_unify_hook(Domain, Y) :-
    (   get_attr(Y, domain, Dom2)
    ->  ord_intersection(Domain, Dom2, NewDomain),
        (   NewDomain == []
        ->    fail
        ;    NewDomain = [Value]
        ->    Y = Value
        ;    put_attr(Y, domain, NewDomain)
        )
    ;   var(Y)
    ->  put_attr( Y, domain, Domain )
    ;   ord_memberchk(Y, Domain)
    ).

%    Translate attributes from this module to residual goals

attribute_goals(X) -->
    { get_attr(X, domain, List) },
    [domain(X, List)].
~~~~~

Before explaining the code we give some example queries:

The predicate `domain/2` fetches (first clause) or assigns
(second clause) the variable a <em>domain</em>, a set of values it can
be unified with.  In the second clause first associates the domain
with a fresh variable and then unifies X to this variable to deal
with the possibility that X already has a domain. The
predicate attr_unify_hook/2 is a hook called after a variable with
a domain is assigned a value.  In the simple case where the variable
is bound to a concrete value we simply check whether this value is in
the domain. Otherwise we take the intersection of the domains and either
fail if the intersection is empty (first example), simply assign the
value if there is only one value in the intersection (second example) or
assign the intersection as the new domain of the variable (third
example). The nonterminal `attribute_goals/3` is used to translate
remaining attributes to user-readable goals that, when executed, reinstate
these attributes.

*/



:- module('$attributes', [
			  delayed_goals/4
			  ]).

:- use_system_module( '$_boot', ['$undefp'/1]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$coroutining', [attr_unify_hook/2]).

:- use_system_module( attributes, [all_attvars/1,
        attributed_module/3,
        bind_attvar/1,
        del_all_atts/1,
        del_all_module_atts/2,
        get_all_swi_atts/2,
        get_module_atts/2,
        modules_with_attributes/1,
        put_att_term/2,
        put_module_atts/2,
        unbind_attvar/1,
        woken_att_do/4]).


:- dynamic attributes:attributed_module/3, attributes:modules_with_attributes/1.

/** @pred get_attr(+ _Var_,+ _Module_,- _Value_) 



Request the current  _value_ for the attribute named  _Module_.  If
 _Var_ is not an attributed variable or the named attribute is not
associated to  _Var_ this predicate fails silently.  If  _Module_
is not an atom, a type error is raised.

 
*/
prolog:get_attr(Var, Mod, Att) :-
	functor(AttTerm, Mod, 2),
	arg(2, AttTerm, Att),
	attributes:get_module_atts(Var, AttTerm).

/**
 @pred put_attr(+ _Var_,+ _Module_,+ _Value_) 



If  _Var_ is a variable or attributed variable, set the value for the
attribute named  _Module_ to  _Value_. If an attribute with this
name is already associated with  _Var_, the old value is replaced.
Backtracking will restore the old value (i.e., an attribute is a mutable
term. See also `setarg/3`). This predicate raises a representation error if
 _Var_ is not a variable and a type error if  _Module_ is not an atom.

 
*/
prolog:put_attr(Var, Mod, Att) :-
	functor(AttTerm, Mod, 2),
	arg(2, AttTerm, Att),
	attributes:put_module_atts(Var, AttTerm).

/** @pred del_attr(+ _Var_,+ _Module_) 



Delete the named attribute.  If  _Var_ loses its last attribute it
is transformed back into a traditional Prolog variable.  If  _Module_
is not an atom, a type error is raised. In all other cases this
predicate succeeds regardless whether or not the named attribute is
present.

 
*/
prolog:del_attr(Var, Mod) :-
	functor(AttTerm, Mod, 2),
	attributes:del_all_module_atts(Var, AttTerm).

/** @pred del_attrs(+ _Var_) 


If  _Var_ is an attributed variable, delete <em>all</em> its
attributes.  In all other cases, this predicate succeeds without
side-effects.

 
*/
prolog:del_attrs(Var) :-
	attributes:del_all_atts(Var).

prolog:get_attrs(AttVar, SWIAtts) :-
	attributes:get_all_swi_atts(AttVar,SWIAtts).

/** @pred put_attrs(+ _Var_,+ _Attributes_) 


Set all attributes of  _Var_.  See get_attrs/2 for a description of
 _Attributes_.

 
*/
prolog:put_attrs(_, []).
prolog:put_attrs(V, Atts) :-
	cvt_to_swi_atts(Atts, YapAtts),
	attributes:put_att_term(V, YapAtts).

cvt_to_swi_atts([], _).
cvt_to_swi_atts(att(Mod,Attribute,Atts), ModAttribute) :-
	ModAttribute =.. [Mod, YapAtts, Attribute],
	cvt_to_swi_atts(Atts, YapAtts).

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
prolog:copy_term(Term, Copy, Gs) :-
	term_attvars(Term, Vs),
	(   Vs == []
	->  Gs = [],
	    copy_term(Term, Copy)
	;   findall(Term-Gs,
	            '$attributes':residuals_and_delete_attributes(Vs, Gs, Term),
		    [Copy-Gs])
	).

residuals_and_delete_attributes(Vs, Gs, Term) :-
	attvars_residuals(Vs, Gs, []),
	delete_attributes(Term).

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

%% @}

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
%	writeln( [Module1|Continuation]:LG),
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
%	yap_flag(gc_trace,verbose),
%	garbage_collect,
	'$restore_regs'(X).
do_continuation('$restore_regs'(X,Y), _) :- !,
%	yap_flag(gc_trace,verbose),
%	garbage_collect,
	'$restore_regs'(X,Y).
do_continuation(Continuation, Module1) :-
	execute_continuation(Continuation,Module1).

execute_continuation(Continuation, Module1) :-
	'$undefined'(Continuation, Module1), !,
	'$current_module'( M ),
	'$swi_current_prolog_flag'( M:unknown, Default ),
        '$undefp'([Module1|Continuation] , Default ).
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

/** @pred _Module_:attribute_goal( _-Var_,  _-Goal_)

User-defined procedure, called to convert the attributes in  _Var_ to
a  _Goal_. Should fail when no interpretation is available.
 */
attvar_residuals([], _) --> [].
attvar_residuals(att(Module,Value,As), V) -->
	(   { nonvar(V) }
	->  % a previous projection predicate could have instantiated
	    % this variable, for example, to avoid redundant goals
	    []
	; { attributes:attributed_module(Module, _, _)  } ->
	    % SICStus like run, put attributes back first
	    { Value =.. [Name,_|Vs],
	      NValue =.. [Name,_|Vs],
	      attributes:put_module_atts(V,NValue)
	    },
	    attvar_residuals(As, V),
	    ( { '$undefined'(attribute_goal(V, Goal), Module) }
	       ->
	      []
	      ;
	      { call(Module:attribute_goal(V, Goal)) },
	      dot_list(Goal)
	    )
	;   (	{ current_predicate(Module:attribute_goals/3) }
	    ->	{ call(Module:attribute_goals(V, Goals, [])) },
		list(Goals)
	    ;	{ current_predicate(Module:attribute_goal/2) }
	    ->	{ call(Module:attribute_goal(V, Goal)) },
		dot_list(Goal)
	    ;	[put_attr(V, Module, Value)]
	    ),
	    attvar_residuals(As, V)
	).

list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

dot_list((A,B)) --> !, dot_list(A), dot_list(B).
dot_list(A)	--> [A].

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
	prolog:call_residue_vars(Module:Goal,NewAttVars),
	(
	 attributes:modules_with_attributes([_|_])
	->
	 project_attributes(NewAttVars, Module:Goal)
	;
	 true
	),
	copy_term(Goal, Goal, Residue).

delayed_goals(G, Vs, NVs, Gs) :-
	project_delayed_goals(G),
%	term_factorized([G|Vs], [_|NVs], Gs).
	copy_term([G|Vs], [_|NVs], Gs).

project_delayed_goals(G) :-
% SICStus compatible step,
% just try to simplify store  by projecting constraints
% over query variables.
% called by top_level to find out about delayed goals
	attributes:modules_with_attributes([_|_]), !,
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
/** @pred _Module_:project_attributes( _+QueryVars_,  _+AttrVars_) 


Given a list of variables  _QueryVars_ and list of attributed
variables  _AttrVars_, project all attributes in  _AttrVars_ to
 _QueryVars_. Although projection is constraint system dependent,
typically this will involve expressing all constraints in terms of
 _QueryVars_ and considering all remaining variables as existentially
quantified.


Projection interacts with attribute_goal/2 at the Prolog top
level. When the query succeeds, the system first calls
project_attributes/2. The system then calls
attribute_goal/2 to get a user-level representation of the
constraints. Typically, attribute_goal/2 will convert from the
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

project_module([], _, _).
project_module([Mod|LMods], LIV, LAV) :-
	'$pred_exists'(project_attributes(LIV, LAV),Mod),
	call(Mod:project_attributes(LIV, LAV)), !,
	attributes:all_attvars(NLAV),
	project_module(LMods,LIV,NLAV).
project_module([_|LMods], LIV, LAV) :-
	project_module(LMods,LIV,LAV).

/**
@}
*/
