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

:- module(attributes, [op(1150, fx, attribute)]).


/** @defgroup Old_Style_Attribute_Declarations SICStus Prolog style Attribute Declarations
@ingroup Attributed_Variables
@{

Old style attribute declarations are activated through loading the
library <tt>atts</tt> . The command

~~~~~
| ?- use_module(library(atts)).
~~~~~
enables this form of use of attributed variables. The package provides the
following functionality:

+ Each attribute must be declared first. Attributes are described by a functor
and are declared per module. Each Prolog module declares its own sets of
attributes. Different modules may have different functors with the same
module.
+ The built-in put_atts/2 adds or deletes attributes to a
variable. The variable may be unbound or may be an attributed
variable. In the latter case, YAP discards previous values for the
attributes.
+ The built-in get_atts/2 can be used to check the values of
an attribute associated with a variable.
+ The unification algorithm calls the user-defined predicate
<tt>verify_attributes/3</tt> before trying to bind an attributed
variable. Unification will resume after this call.
+ The user-defined predicate
<tt>attribute_goal/2</tt> converts from an attribute to a goal.
+ The user-defined predicate
<tt>project_attributes/2</tt> is used from a set of variables into a set of
constraints or goals. One application of <tt>project_attributes/2</tt> is in
the top-level, where it is used to output the set of
floundered constraints at the end of a query.

/** @defgroup Attribute_Declarations Attribute Declarations
@ingroup Old_Style_Attribute_Declarations
@{

Attributes are compound terms associated with a variable. Each attribute
has a <em>name</em> which is <em>private</em> to the module in which the
attribute was defined. Variables may have at most one attribute with a
name. Attribute names are defined with the following declaration:

~~~~~
:- attribute AttributeSpec, ..., AttributeSpec.
~~~~~

where each  _AttributeSpec_ has the form ( _Name_/ _Arity_).
One single such declaration is allowed per module  _Module_.

Although the YAP module system is predicate based, attributes are local
to modules. This is implemented by rewriting all calls to the
built-ins that manipulate attributes so that attribute names are
preprocessed depending on the module.  The `user:goal_expansion/3`
mechanism is used for this purpose.


The  attribute manipulation predicates always work as follows:

+ The first argument is the unbound variable associated with
attributes,
+ The second argument is a list of attributes. Each attribute will
be a Prolog term or a constant, prefixed with the <tt>+</tt> and <tt>-</tt> unary
operators. The prefix <tt>+</tt> may be dropped for convenience.

The following three procedures are available to the user. Notice that
these built-ins are rewritten by the system into internal built-ins, and
that the rewriting process <em>depends</em> on the module on which the
built-ins have been invoked.

 
The user-predicate predicate verify_attributes/3 is called when
attempting to unify an attributed variable which might have attributes
in some  _Module_.

 
Attributes are usually presented as goals. The following routines are
used by built-in predicates such as call_residue/2 and by the
Prolog top-level to display attributes:

 
Constraint solvers must be able to project a set of constraints to a set
of variables. This is useful when displaying the solution to a goal, but
may also be used to manipulate computations. The user-defined
project_attributes/2 is responsible for implementing this
projection.


The following two examples example is taken from the SICStus Prolog manual. It
sketches the implementation of a simple finite domain `solver`.  Note
that an industrial strength solver would have to provide a wider range
of functionality and that it quite likely would utilize a more efficient
representation for the domains proper.  The module exports a single
predicate `domain( _-Var_, _?Domain_)` which associates
 _Domain_ (a list of terms) with  _Var_.  A variable can be
queried for its domain by leaving  _Domain_ unbound.

We do not present here a definition for project_attributes/2.
Projecting finite domain constraints happens to be difficult.

~~~~~
:- module(domain, [domain/2]).

:- use_module(library(atts)).
:- use_module(library(ordsets), [
        ord_intersection/3,
        ord_intersect/2,
        list_to_ord_set/2
   ]).

:- attribute dom/1.

verify_attributes(Var, Other, Goals) :-
        get_atts(Var, dom(Da)), !,          % are we involved?
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other, dom(Db)) -> %   has a domain?
                ord_intersection(Da, Db, Dc),
                Dc = [El|Els],              % at least one element
                (   Els = [] ->             % exactly one element
                    Goals = [Other=El]      % implied binding
                ;   Goals = [],
                    put_atts(Other, dom(Dc))% rescue intersection
                )
            ;   Goals = [],
                put_atts(Other, dom(Da))    % rescue the domain
            )
        ;   Goals = [],
            ord_intersect([Other], Da)      % value in domain?
        ).
verify_attributes(_, _, []).                % unification triggered
                                            % because of attributes
                                            % in other modules

attribute_goal(Var, domain(Var,Dom)) :-     % interpretation as goal
        get_atts(Var, dom(Dom)).

domain(X, Dom) :-
        var(Dom), !,
        get_atts(X, dom(Dom)).
domain(X, List) :-
        list_to_ord_set(List, Set),
        Set = [El|Els],                     % at least one element
        (   Els = [] ->                     % exactly one element
            X = El                          % implied binding
        ;   put_atts(Fresh, dom(Set)),
            X = Fresh                       % may call
                                            % verify_attributes/3
        ).
~~~~~

Note that the _implied binding_ `Other=El` was deferred until after
the completion of `verify_attribute/3`.  Otherwise, there might be a
danger of recursively invoking `verify_attribute/3`, which might bind
`Var`, which is not allowed inside the scope of `verify_attribute/3`.
Deferring unifications into the third argument of `verify_attribute/3`
effectively serializes the calls to `verify_attribute/3`.

Assuming that the code resides in the file domain.yap, we
can use it via:

~~~~~
| ?- use_module(domain).
~~~~~

Let's test it:

~~~~~
| ?- domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]).

domain(X,[1,5,6,7]),
domain(Y,[3,4,5,6]),
domain(Z,[1,6,7,8]) ? 

yes
| ?- domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]), 
     X=Y.

Y = X,
domain(X,[5,6]),
domain(Z,[1,6,7,8]) ? 

yes
| ?- domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]),
     X=Y, Y=Z.

X = 6,
Y = 6,
Z = 6
~~~~~

To demonstrate the use of the  _Goals_ argument of
verify_attributes/3, we give an implementation of
freeze/2.  We have to name it `myfreeze/2` in order to
avoid a name clash with the built-in predicate of the same name.

~~~~~
:- module(myfreeze, [myfreeze/2]).

:- use_module(library(atts)).

:- attribute frozen/1.

verify_attributes(Var, Other, Goals) :-
        get_atts(Var, frozen(Fa)), !,       % are we involved?
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other, frozen(Fb)) % has a pending goal?
            ->  put_atts(Other, frozen((Fa,Fb))) % rescue conjunction
            ;   put_atts(Other, frozen(Fa)) % rescue the pending goal
            ),
            Goals = []
        ;   Goals = [Fa]
        ).
verify_attributes(_, _, []).

attribute_goal(Var, Goal) :-                % interpretation as goal
        get_atts(Var, frozen(Goal)).

myfreeze(X, Goal) :-
        put_atts(Fresh, frozen(Goal)),
        Fresh = X.
~~~~~

Assuming that this code lives in file myfreeze.yap,
we would use it via:

~~~~~
| ?- use_module(myfreeze).
| ?- myfreeze(X,print(bound(x,X))), X=2.

bound(x,2)                      % side effect
X = 2                           % bindings
~~~~~

The two solvers even work together:

~~~~~
| ?- myfreeze(X,print(bound(x,X))), domain(X,[1,2,3]),
     domain(Y,[2,10]), X=Y.

bound(x,2)                      % side effect
X = 2,                          % bindings
Y = 2
~~~~~

The two example solvers interact via bindings to shared attributed
variables only.  More complicated interactions are likely to be found
in more sophisticated solvers.  The corresponding
verify_attributes/3 predicates would typically refer to the
attributes from other known solvers/modules via the module prefix in
` _Module_:get_atts/2`.


 */


:- use_module(library(lists), [member/2]).

:- multifile
	user:goal_expansion/3.
:- multifile
	user:term_expansion/2.

:- dynamic existing_attribute/4.
:- dynamic modules_with_attributes/1.
:- dynamic attributed_module/3.

modules_with_attributes([]).

%
% defining a new attribute is just a question of establishing a
% Functor, Mod -> INT mappings
%
new_attribute(V) :- var(V), !,
	throw(error(instantiation_error,attribute(V))).
new_attribute((At1,At2)) :-
	new_attribute(At1),
	new_attribute(At2).
new_attribute(Na/Ar) :-
	source_module(Mod),
	functor(S,Na,Ar),
	existing_attribute(S,Mod,_,_) , !.
new_attribute(Na/Ar) :-
	source_module(Mod),
	functor(S,Na,Ar),
	store_new_module(Mod,Ar,Position),
	assertz(existing_attribute(S,Mod,Ar,Position)).

store_new_module(Mod,Ar,ArgPosition) :-
	(
	  retract(attributed_module(Mod,Position,_))
	->
	  true
	;
	  retract(modules_with_attributes(Mods)),
	  assert(modules_with_attributes([Mod|Mods])), Position = 2
	),
	ArgPosition is Position+1,
	( Ar == 0 -> NOfAtts is Position+1 ; NOfAtts is Position+Ar),
	functor(AccessTerm,Mod,NOfAtts),
	assertz(attributed_module(Mod,NOfAtts,AccessTerm)).
	
:- user_defined_directive(attribute(G), attributes:new_attribute(G)).

/** @pred _Module_:get_atts( _-Var_, _?ListOfAttributes_) 


Unify the list  _?ListOfAttributes_ with the attributes for the unbound
variable  _Var_. Each member of the list must be a bound term of the
form `+( _Attribute_)`, `-( _Attribute_)` (the <tt>kbd</tt>
prefix may be dropped). The meaning of <tt>+</tt> and <tt>-</tt> is:
+ +( _Attribute_)
Unifies  _Attribute_ with a corresponding attribute associated with
 _Var_, fails otherwise.

+ -( _Attribute_)
Succeeds if a corresponding attribute is not associated with
 _Var_. The arguments of  _Attribute_ are ignored.

 
*/
user:goal_expansion(get_atts(Var,AccessSpec), Mod, Goal) :-
	expand_get_attributes(AccessSpec,Mod,Var,Goal).
/** @pred _Module_:put_atts( _-Var_, _?ListOfAttributes_) 


Associate with or remove attributes from a variable  _Var_. The
attributes are given in  _?ListOfAttributes_, and the action depends
on how they are prefixed:
+ +( _Attribute_)
Associate  _Var_ with  _Attribute_. A previous value for the
attribute is simply replace (like with `set_mutable/2`).

+ -( _Attribute_)
Remove the attribute with the same name. If no such attribute existed,
simply succeed.



 */
user:goal_expansion(put_atts(Var,AccessSpec), Mod, Goal) :-
	expand_put_attributes(AccessSpec, Mod, Var, Goal).


expand_get_attributes(V,_,_,_) :- var(V), !, fail.
expand_get_attributes([],_,_,true) :- !.
expand_get_attributes([-G1],Mod,V,attributes:free_att(V,Mod,Pos)) :-
	existing_attribute(G1,Mod,_,Pos), !.
expand_get_attributes([+G1],Mod,V,attributes:get_att(V,Mod,Pos,A)) :-
	existing_attribute(G1,Mod,1,Pos), !,
	arg(1,G1,A).
expand_get_attributes([G1],Mod,V,attributes:get_att(V,Mod,Pos,A)) :-
	existing_attribute(G1,Mod,1,Pos), !,
	arg(1,G1,A).
expand_get_attributes(Atts,Mod,Var,attributes:get_module_atts(Var,AccessTerm)) :- Atts = [_|_], !,
	attributed_module(Mod,NOfAtts,AccessTerm),
	void_term(Void),
	cvt_atts(Atts,Mod,Void,LAtts),
	sort(LAtts,SortedLAtts),
	free_term(Free),
	build_att_term(1,NOfAtts,SortedLAtts,Free,AccessTerm).
expand_get_attributes(Att,Mod,Var,Goal) :- 
	expand_get_attributes([Att],Mod,Var,Goal).

build_att_term(NOfAtts,NOfAtts,[],_,_) :- !.
build_att_term(I0,NOfAtts,[I-Info|SortedLAtts],Void,AccessTerm) :-
	I is I0+1, !,
	copy_att_args(Info,I0,NI,AccessTerm),
	build_att_term(NI,NOfAtts,SortedLAtts,Void,AccessTerm).
build_att_term(I0,NOfAtts,SortedLAtts,Void,AccessTerm) :-
	I is I0+1,
	arg(I,AccessTerm,Void),
	build_att_term(I,NOfAtts,SortedLAtts,Void,AccessTerm).

cvt_atts(V,_,_,_) :- var(V), !, fail.
cvt_atts([],_,_,[]).
cvt_atts([V|_],_,_,_) :- var(V), !, fail.
cvt_atts([+Att|Atts],Mod,Void,[Pos-LAtts|Read]) :- !,
	existing_attribute(Att,Mod,_,Pos),
	(atom(Att) -> LAtts = [_] ; Att=..[_|LAtts]),
	cvt_atts(Atts,Mod,Void,Read).
cvt_atts([-Att|Atts],Mod,Void,[Pos-LVoids|Read]) :- !,
	existing_attribute(Att,Mod,_,Pos),
	(
	  atom(Att)
	->
	  LVoids = [Void]
	;
	  Att =..[_|LAtts],
	  void_vars(LAtts,Void,LVoids)
	),	  
	cvt_atts(Atts,Mod,Void,Read).
cvt_atts([Att|Atts],Mod,Void,[Pos-LAtts|Read]) :- !,
	existing_attribute(Att,Mod,_,Pos),
	(atom(Att) -> LAtts = [_] ; Att=..[_|LAtts]),
	cvt_atts(Atts,Mod,Void,Read).

copy_att_args([],I,I,_).
copy_att_args([V|Info],I,NI,AccessTerm) :-
	I1 is I+1,
	arg(I1,AccessTerm,V),
	copy_att_args(Info,I1,NI,AccessTerm).

void_vars([],_,[]).
void_vars([_|LAtts],Void,[Void|LVoids]) :-
	void_vars(LAtts,Void,LVoids).

expand_put_attributes(V,_,_,_) :- var(V), !, fail.
expand_put_attributes([-G1],Mod,V,attributes:rm_att(V,Mod,NOfAtts,Pos)) :-
	existing_attribute(G1,Mod,_,Pos), !,
	attributed_module(Mod,NOfAtts,_).
expand_put_attributes([+G1],Mod,V,attributes:put_att(V,Mod,NOfAtts,Pos,A)) :-
	existing_attribute(G1,Mod,1,Pos), !,
	attributed_module(Mod,NOfAtts,_),
	arg(1,G1,A).
expand_put_attributes([G1],Mod,V,attributes:put_att(V,Mod,NOfAtts,Pos,A)) :-
	existing_attribute(G1,Mod,1,Pos), !,
	attributed_module(Mod,NOfAtts,_),
	arg(1,G1,A).
expand_put_attributes(Atts,Mod,Var,attributes:put_module_atts(Var,AccessTerm)) :- Atts = [_|_], !,
	attributed_module(Mod,NOfAtts,AccessTerm),
	void_term(Void),
	cvt_atts(Atts,Mod,Void,LAtts),
	sort(LAtts,SortedLAtts),
	free_term(Free),
	build_att_term(1,NOfAtts,SortedLAtts,Free,AccessTerm).
expand_put_attributes(Att,Mod,Var,Goal) :- 
	expand_put_attributes([Att],Mod,Var,Goal).

woken_att_do(AttVar, Binding, NGoals, DoNotBind) :-
	modules_with_attributes(AttVar,Mods0),
	modules_with_attributes(Mods),
	find_used(Mods,Mods0,[],ModsI),
	do_verify_attributes(ModsI, AttVar, Binding, Goals),
	process_goals(Goals, NGoals, DoNotBind).

% dirty trick to be able to unbind a variable that has been constrained.
process_goals([], [], _).
process_goals((M:do_not_bind_variable(Gs)).Goals, (M:Gs).NGoals, true) :- !,
	process_goals(Goals, NGoals, _).
process_goals(G.Goals, G.NGoals, Do) :-
	process_goals(Goals, NGoals, Do).

find_used([],_,L,L).
find_used([M|Mods],Mods0,L0,Lf) :-
        member(M,Mods0), !,
	find_used(Mods,Mods0,[M|L0],Lf).
find_used([_|Mods],Mods0,L0,Lf) :-
	find_used(Mods,Mods0,L0,Lf).

/** @pred _Module_:verify_attributes( _-Var_,  _+Value_,  _-Goals_) 



The predicate is called when trying to unify the attributed variable
 _Var_ with the Prolog term  _Value_. Note that  _Value_ may be
itself an attributed variable, or may contain attributed variables.  The
goal <tt>verify_attributes/3</tt> is actually called before  _Var_ is
unified with  _Value_.

It is up to the user to define which actions may be performed by
<tt>verify_attributes/3</tt> but the procedure is expected to return in
 _Goals_ a list of goals to be called <em>after</em>  _Var_ is
unified with  _Value_. If <tt>verify_attributes/3</tt> fails, the
unification will fail.

Notice that the <tt>verify_attributes/3</tt> may be called even if  _Var_<
has no attributes in module <tt>Module</tt>. In this case the routine should
simply succeed with  _Goals_ unified with the empty list.

 
*/
do_verify_attributes([], _, _, []).
do_verify_attributes([Mod|Mods], AttVar, Binding, [Mod:Goal|Goals]) :-
	current_predicate(verify_attributes,Mod:verify_attributes(_,_,_)), !,
	Mod:verify_attributes(AttVar, Binding, Goal),
	do_verify_attributes(Mods, AttVar, Binding, Goals).
do_verify_attributes([_|Mods], AttVar, Binding, Goals) :-
	do_verify_attributes(Mods, AttVar, Binding, Goals).

/**
@}
*/
