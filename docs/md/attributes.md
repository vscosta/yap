

## [Attributed Variables](AttributedVariables)

<!---- @{ ---->


<!---- @ingroup extensions ---->

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
available through the <tt>atts</tt> library, and is used by CLPBN.

From YAP-6.0.3 onwards we recommend using the hProlog, SWI style
interface. We believe that this design is easier to understand and
work with. Most packages included in YAP that use attributed
variables, such as CHR, CLP(FD), and CLP(QR), rely on the SWI-Prolog
awi interface.

### [SICStus Style attribute declarations.](SICS_attributes)

<!---- @{ ---->
<!---- @ingroup  AttributedVariables ---->

The YAP library `atts` implements attribute variables in the style of
SICStus Prolog. Attributed variables work as follows:

+ Each attribute must be declared beforehand. Attributes are described
as a functor with name and arity and are local to a module. Each
Prolog module declares its own sets of attributes. Different modules
may have attributes with the same name and arity.

+ The built-in put_atts/2 adds or deletes attributes to a
variable. The variable may be unbound or may be an attributed
variable. In the latter case, YAP discards previous values for the
attributes.

+ The built-in get_atts/2 can be used to check the values of
an attribute associated with a variable.

+ The unification algorithm calls the user-defined predicate
verify_attributes/3 before trying to bind an attributed
variable. Unification will resume after this call.

+ The user-defined predicate
<tt>attribute_goal/2</tt> converts from an attribute to a goal.

+ The user-defined predicate
<tt>project_attributes/2</tt> is used from a set of variables into a set of
constraints or goals. One application of <tt>project_attributes/2</tt> is in
the top-level, where it is used to output the set of
floundered constraints at the end of a query.


Attributes are compound terms associated with a variable. Each attribute
has a <em>name</em> which is <em>private</em> to the module in which the
attribute was defined. Variables may have at most one attribute with a
name. Attribute names are defined through the following declaration:

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


The following examples are taken from the SICStus Prolog
manual. The sketches the implementation of a simple finite domain
`solver`.  Note that an industrial strength solver would have to
provide a wider range of functionality and that it quite likely would
utilize a more efficient representation for the domains proper.  The
module exports a single predicate `domain( _-Var_, _?Domain_)` which
associates _Domain_ (a list of terms) with _Var_.  A variable can be
queried for its domain by leaving _Domain_ unbound.

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

myfreeze(X, Goal) :- put_atts(Fresh, frozen(Goal)), Fresh = X.  ~~~~~

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
Module:get_atts/2`.

<!---- @} ---->


## [hProlog and SWI-Prolog style Attribute Declarations](New_Style_Attribute_Declarations)

<!---- @{ ---->
<!----   @ingroup AttributedVariables ---->

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
    [ domain/2            % Var, ?Domain %
    ]).
:- use_module(library(ordsets)).

domain(X, Dom) :-
  var(Dom), !,
  get_attr(X, domain, Dom).
domain(X, List) :-
  list_to_ord_set(List, Domain),
  put_attr(Y, domain, Domain),
  X = Y.
~~~~~

  An attributed variable with attribute value Domain has been assigned the value Y.

~~~~~
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
~~~~~

Translate attributes from this module to residual goals:

~~~~~
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

<!---- @} ---->

## [Co-routining](CohYroutining)

<!---- @{ ---->
<!----   @ingroup AttributedVariables ---->

Prolog uses a simple left-to-right flow of control. It is sometimes
convenient to change this control so that goals will only execute when
sufficiently instantiated. This may result in a more "data-driven"
execution, or may be necessary to correctly implement extensions such
as negation by failure.

Initially, YAP used a separate mechanism for co-routining. Nowadays, YAP uses
attributed variables to implement co-routining.

Two declarations are supported:

+ block/1
The argument to `block/1` is a condition on a goal or a conjunction
of conditions, with each element separated by commas. Each condition is
of the form `predname( _C1_,..., _CN_)`, where  _N_ is the
arity of the goal, and each  _CI_ is of the form `-`, if the
argument must suspend until the first such variable is bound, or
`?`, otherwise.

+ wait/1
The argument to `wait/1` is a predicate descriptor or a conjunction
of these predicates. These predicates will suspend until their first
argument is bound.


The following primitives can be used:

- freeze/2

- dif/2

- when/2

- frozen/2


<!---- @} ---->

<!---- @} ---->
