patttCHR: Constraint Handling Rules  {#chr}
==============================


This chapter is written by Tom Schrijvers, K.U. Leuven for the hProlog
system. Adjusted by Jan Wielemaker to fit the SWI-Prolog documentation
infrastructure and remove hProlog specific references.

The CHR system of SWI-Prolog is the K.U.Leuven CHR system.  The
runtime environment is written by Christian Holzbaur and Tom
Schrijvers while the compiler is written by Tom Schrijvers. Both are
integrated with SWI-Prolog and licenced under compatible conditions
with permission from the authors. Porting and maintenance on YAP is
the entire responsability of Vítor Santos Costa.

The main reference for SWI-Prolog's CHR system is:

+ T. Schrijvers, and B. Demoen, <em>The K.U.Leuven CHR System: Implementation and Application</em>, First Workshop on Constraint Handling Rules: Selected
Contributions (Fruwirth, T. and Meister, M., eds.), pp. 1--5, 2004.

# Introduction

Constraint Handling Rules (CHR) is a committed-choice bottom-up language
embedded in Prolog. It is designed for writing constraint solvers and is
particularily useful for providing application-specific constraints.
It has been used in many kinds of applications, like scheduling,
model checking, abduction, type checking among many others.

CHR has previously been implemented in other Prolog systems (SICStus,
Eclipse, Yap), Haskell and Java. This CHR system is based on the
compilation scheme and runtime environment of CHR in SICStus.

In this documentation we restrict ourselves to giving a short overview
of CHR in general and mainly focus on elements specific to this
implementation. For a more thorough review of CHR we refer the reader to
[Freuhwirth:98]. More background on CHR can be found at the CHR web site.

### Syntax and Semantics

We present informally the syntax and semantics of CHR.


####  CHR Syntax

The syntax of CHR rules in hProlog is the following:

~~~~~
rules --> rule, rules.
rules --> [].

rule --> name, actual_rule, pragma, [atom(`.`)].

name --> atom, [atom(`@`)].
name --> [].

actual_rule --> simplification_rule.
actual_rule --> propagation_rule.
actual_rule --> simpagation_rule.

simplification_rule --> constraints, [atom(`<=>`)], guard, body.
propagation_rule --> constraints, [atom(`==>`)], guard, body.
simpagation_rule --> constraints, [atom(`\`)], constraints, [atom(`<=>`)],
                     guard, body.

constraints --> constraint, constraint_id.
constraints --> constraint, [atom(`,`)], constraints.

constraint --> compound_term.

constraint_id --> [].
constraint_id --> [atom(`#`)], variable.

guard --> [].
guard --> goal, [atom(`|`)].

body --> goal.

pragma --> [].
pragma --> [atom(`pragma`)], actual_pragmas.

actual_pragmas --> actual_pragma.
actual_pragmas --> actual_pragma, [atom(`,`)], actual_pragmas.

actual_pragma --> [atom(`passive(`)], variable, [atom(`)`)].

~~~~~

Additional syntax-related terminology:

+ *head:* the constraints in an `actual_rule` before
the arrow (either `<=>` or `==>`)


#### Semantics Semantics

In this subsection the operational semantics of CHR in Prolog are presented
informally. They do not differ essentially from other CHR systems.

When a constraint is called, it is considered an active constraint and
the system will try to apply the rules to it. Rules are tried and executed
sequentially in the order they are written.

A rule is conceptually tried for an active constraint in the following
way. The active constraint is matched with a constraint in the head of
the rule. If more constraints appear in the head they are looked for
among the suspended constraints, which are called passive constraints in
this context. If the necessary passive constraints can be found and all
match with the head of the rule and the guard of the rule succeeds, then
the rule is committed and the body of the rule executed. If not all the
necessary passive constraint can be found, the matching fails or the
guard fails, then the body is not executed and the process of trying and
executing simply continues with the following rules. If for a rule,
there are multiple constraints in the head, the active constraint will
try the rule sequentially multiple times, each time trying to match with
another constraint.

This process ends either when the active constraint disappears, i.e. it
is removed by some rule, or after the last rule has been processed. In
the latter case the active constraint becomes suspended.

A suspended constraint is eligible as a passive constraint for an active
constraint. The other way it may interact again with the rules, is when
a variable appearing in the constraint becomes bound to either a nonvariable
or another variable involved in one or more constraints. In that case the
constraint is triggered, i.e. it becomes an active constraint and all
the rules are tried.

### Rules

There are three different kinds of rules, each with their specific semantics:

+ simplification
The simplification rule removes the constraints in its head and calls its body.

+ propagation
The propagation rule calls its body exactly once for the constraints in
its head.

+ simpagation
The simpagation rule removes the constraints in its head after the
`\` and then calls its body. It is an optimization of
simplification rules of the form: \[constraints_1, constraints_2 <=>
constraints_1, body \] Namely, in the simpagation form:

~~~~~
constraints1 \ constraints2 <=> body
~~~~~
 _constraints1_
constraints are not called in the body.



#### Rule Names

Naming a rule is optional and has no semantical meaning. It only functions
as documentation for the programmer.

### Pragmas

The semantics of the pragmas are:

+ passive(Identifier)
The constraint in the head of a rule  _Identifier_ can only act as a
passive constraint in that rule.


Additional pragmas may be released in the future.

### CHR_Options Options

It is possible to specify options that apply to all the CHR rules in the module.
Options are specified with the `option/2` declaration:

~~~~~
                option(Option,Value).
~~~~~

Available options are:

+ check_guard_bindings
This option controls whether guards should be checked for illegal
variable bindings or not. Possible values for this option are
`on`, to enable the checks, and `off`, to disable the
checks.

+ optimize
This is an experimental option controlling the degree of optimization.
Possible values are `full`, to enable all available
optimizations, and `off` (default), to disable all optimizations.
The default is derived from the SWI-Prolog flag `optimise`, where
`true` is mapped to `full`.  Therefore the commandline
option `-O` provides full CHR optimization.
If optimization is enabled, debugging should be disabled.

+ debug
This options enables or disables the possibility to debug the CHR code.
Possible values are `on` (default) and `off`. See
`debugging` for more details on debugging.  The default is
derived from the prolog flag `generate_debug_info`, which
is `true` by default.  See `-nodebug`.
If debugging is enabled, optimization should be disabled.

+ mode
This option specifies the mode for a particular constraint. The
value is a term with functor and arity equal to that of a constraint.
The arguments can be one of `-`, `+` or `?`.
The latter is the default. The meaning is the following:

+ -
The corresponding argument of every occurrence
of the constraint is always unbound.
+ +
The corresponding argument of every occurrence
of the constraint is always ground.
+ ?
The corresponding argument of every occurrence
of the constraint can have any instantiation, which may change
over time. This is the default value.

The declaration is used by the compiler for various optimizations.
Note that it is up to the user the ensure that the mode declaration
is correct with respect to the use of the constraint.
This option may occur once for each constraint.

+ type_declaration
This option specifies the argument types for a particular constraint. The
value is a term with functor and arity equal to that of a constraint.
The arguments can be a user-defined type or one of
the built-in types:

+ int
The corresponding argument of every occurrence
of the constraint is an integer number.
+ float
...{} a floating point number.
+ number
...{} a number.
+ natural
...{} a positive integer.
+ any
The corresponding argument of every occurrence
of the constraint can have any type. This is the default value.


Currently, type declarations are only used to improve certain
optimizations (guard simplification, occurrence subsumption, ...{}).

+ type_definition
This option defines a new user-defined type which can be used in
type declarations. The value is a term of the form
`type(` _name_`,` _list_`)`, where
 _name_ is a term and  _list_ is a list of alternatives.
Variables can be used to define generic types. Recursive definitions
are allowed. Examples are

~~~~~
type(bool,[true,false]).
type(complex_number,[float + float * i]).
type(binary_tree(T),[ leaf(T) | node(binary_tree(T),binary_tree(T)) ]).
type(list(T),[ [] | [T | list(T)]).
~~~~~



The mode, type_declaration and type_definition options are provided
for backward compatibility. The new syntax is described below.



### CHR in Prolog Programs


The CHR constraints defined in a particulary chr file are
associated with a module. The default module is `user`. One should
never load different chr files with the same CHR module name.



#### Constraint Declarations


Every constraint used in CHR rules has to be declared.
There are two ways to do this. The old style is as follows:

~~~~~
option(type_definition,type(list(T),[ [] , [T|list(T)] ]).
option(mode,foo(+,?)).
option(type_declaration,foo(list(int),float)).
:- constraints foo/2, bar/0.
~~~~~

The new style is as follows:

~~~~~
:- chr_type list(T) ---> [] ; [T|list(T)].
:- constraints foo(+list(int),?float), bar.
~~~~~



#### Compilation

The
 SWI-Prolog CHR compiler exploits term_expansion/2 rules to translate
the constraint handling rules to plain Prolog. These rules are loaded
from the library chr.   They are activated if the compiled file
has the chr extension or after finding a declaration of the
format below.

~~~~~
:- constraints ...
~~~~~

It is adviced to define CHR rules in a module file, where the module
declaration is immediately followed by including the chr
library as examplified below:

~~~~~
:- module(zebra, [ zebra/0 ]).
:- use_module(library(chr)).

:- constraints ...
~~~~~

Using this style CHR rules can be defined in ordinary Prolog
pl files and the operator definitions required by CHR do not
leak into modules where they might cause conflicts.





#### CHR Debugging

The CHR debugging facilities are currently rather limited. Only tracing
is currently available. To use the CHR debugging facilities for a CHR
file it must be compiled for debugging. Generating debug info is
controlled by the CHR option debug, whose default is derived
from the SWI-Prolog flag `generate_debug_info`.  Therefore debug
info is provided unless the `-nodebug` is used.

#### Ports

vFor CHR constraints the four standard ports are defined:

+ call
A new constraint is called and becomes active.
+ exit
An active constraint exits: it has either been inserted in the store after
trying all rules or has been removed from the constraint store.
+ fail
An active constraint fails.
+ redo
An active constraint starts looking for an alternative solution.


In addition to the above ports, CHR constraints have five additional
ports:

+ wake
A suspended constraint is woken and becomes active.
+ insert
An active constraint has tried all rules and is suspended in
the constraint store.
+ remove
An active or passive constraint is removed from the constraint
store, if it had been inserted.
+ try
An active constraints tries a rule with possibly
some passive constraints. The try port is entered
just before committing to the rule.
+ apply
An active constraints commits to a rule with possibly
some passive constraints. The apply port is entered
just after committing to the rule.

#### Tracing

Tracing is enabled with the chr_trace/0 predicate
and disabled with the chr_notrace/0 predicate.

When enabled the tracer will step through the `call`,
`exit`, `fail`, `wake` and `apply` ports,
accepting debug commands, and simply write out the other ports.

The following debug commans are currently supported:

~~~~~
        CHR debug options:

                <cr>    creep           c       creep
		s	skip
		g	ancestors
                n       nodebug
		b	break
                a       abort
                f       fail
                ?       help            h       help
~~~~~

Their meaning is:

+ creep
Step to the next port.
+ skip
Skip to exit port of this call or wake port.
+ ancestors
Print list of ancestor call and wake ports.
+ nodebug
Disable the tracer.
+ break
Enter a recursive Prolog toplevel.  See break/0.
+ abort
Exit to the toplevel.  See abort/0.
+ fail
Insert failure in execution.
+ help
Print the above available debug options.


#### CHR Debugging Predicates


The chr module contains several predicates that allow
inspecting and printing the content of the constraint store.

+ chr_trace
Activate the CHR tracer.  By default the CHR tracer is activated and
deactivated automatically by the Prolog predicates trace/0 and
notrace/0.

### CHR_Examples Examples

Here are two example constraint solvers written in CHR.

+
The program below defines a solver with one constraint,
`leq/2`, which is a less-than-or-equal constraint.

~~~~~
:- module(leq,[cycle/3, leq/2]).
:- use_module(library(chr)).

:- constraints leq/2.
reflexivity  @ leq(X,X) <=> true.
antisymmetry @ leq(X,Y), leq(Y,X) <=> X = Y.
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true.
transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

cycle(X,Y,Z):-
        leq(X,Y),
        leq(Y,Z),
        leq(Z,X).
~~~~~

+
The program below implements a simple finite domain
constraint solver.

~~~~~
:- module(dom,[dom/2]).
:- use_module(library(chr)).

:- constraints dom/2.

dom(X,[]) <=> fail.
dom(X,[Y]) <=> X = Y.
dom(X,L1), dom(X,L2) <=> intersection(L1,L2,L3), dom(X,L3).

intersection([],_,[]).
intersection([H|T],L2,[H|L3]) :-
        member(H,L2), !,
        intersection(T,L2,L3).
intersection([_|T],L2,L3) :-
        intersection(T,L2,L3).
~~~~~



### Compatibility with SICStus CHR


There are small differences between CHR in SWI-Prolog and newer
YAPs and SICStus and older versions of YAP.  Besides differences in
available options and pragmas, the following differences should be
noted:

+ [The handler/1 declaration]
In SICStus every CHR module requires a `handler/1`
declaration declaring a unique handler name. This declaration is valid
syntax in SWI-Prolog, but will have no effect. A warning will be given
during compilation.

+ [The rules/1 declaration]
In SICStus, for every CHR module it is possible to only enable a subset
of the available rules through the `rules/1` declaration. The
declaration is valid syntax in SWI-Prolog, but has no effect. A
warning is given during compilation.

+ [Sourcefile naming]
SICStus uses a two-step compiler, where chr files are
first translated into pl files.  For SWI-Prolog CHR
rules may be defined in a file with any extension.

### Guidelines

In this section we cover several guidelines on how to use CHR to write
constraint solvers and how to do so efficiently.

+ [Set semantics]
The CHR system allows the presence of identical constraints, i.e.
multiple constraints with the same functor, arity and arguments. For
most constraint solvers, this is not desirable: it affects efficiency
and possibly termination. Hence appropriate simpagation rules should be
added of the form:

~~~~~
{constraint \ constraint <=> true}.
~~~~~

+ [Multi-headed rules]
Multi-headed rules are executed more efficiently when the constraints
share one or more variables.

+ [Mode and type declarations]
Provide mode and type declarations to get more efficient program execution.
Make sure to disable debug (`-nodebug`) and enable optimization
(`-O`).
