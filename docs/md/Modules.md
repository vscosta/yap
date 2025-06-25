/**
   @file extra/modules.h
   @brief the Module Systenm
*/

/**
   @defgroup YAPModules The YAP Module system
@ingroup YAPProgramming
@{

@brief The YAP Predicate Module System

The YAP module system is based on the Quintus/SISCtus module
system \cite quintus . In this design, modules are named collections of predicates,
and all predicates belong to a single module. By default, predicates are only
visible within a module, or _private_ to that module. The module
may also define a list of predicates that are
_exported_, that is, visible to other modules.

The main predicates in the module system are:

- module/2 associates a source file to a module. It has two arguments: the name of the new module, and a list of predicates exported by the module.

- use_module/1 and use_module/2 can be used to load a module. They take as first argument the source file for the module. Whereas use_module/1 loads all exported predicates, use_module/2 only takes the ones given by the second argument.

YAP pre-defines a number of modules.

- Most system predicates belong to
  the module `prolog`. Predicates from the module `prolog` are
automatically visible to every module.
- The `system` module was
  introduced for SWI-Prolog compatibility, and in YAP mostly acts as an
alias to `prolog`.
- The `user` module is also visible to all other modules.

The YAP engine is always associated to a module, the current <em>source
module</em> or <em>type-in module</em>. By default, all predicates
read-in and all calls to a goal will be made to predicates visible to
the current source module, Initially, the source module for YAP is the
module `user`. Thus Prolog programs that do not define modules will
operate within the `user` module. In this case, all predicates will be
visible to all source files.

YAP  includes a number of libraries and packages, most of them
 defining their own modules. Note that there is no system mechanism to
 avoid clashes between module names, so it is up to the programmer to
 carefully choose the names for her own program modules.

The main mechanism to change the current type-in module is by using
the module/2 declaration.This declaration sets the source module when
  it starts consulting a file, and resets it at the end.  One can set
the type-in module permanently by using the built-in `module/1`.

## Explicit Naming

The module system allows one to _explicitly_ specify the source mode for
 a clause by prefixing a clause with its module, say:
```
user:(a :- b).  
```

it is also possible to type

```

user:a :- user:b.

```

both formulations describe the same clause, independently of the
current type-in module.

In fact, it is sufficient to specify the source mode for the clause's
head:

```
user:a :- b.
```

if the current type-in module is `m`, the clause could also be written as:

```
user:a :- m:b.
```

The compiler rewrites the source clauses to ensure that explicit calls
are respected, and that implicit calls are made to the current source
module.

A goal should refer to a predicate visible within the current type-in
module. Thus, if a goal appears in a text file with a module
declaration, the goal refers to that module's context (but see the
initialization/1 directive for more details).

Again, one can override this rule by prefixing a goal with a module to
be consulted. The following query:

```
?- nasa:launch(apollo,13).
```
 invokes the goal `launch(apollo,13)` as if the current source
module was `nasa`.

YAP and other Prolog systems allow the module prefix to see all
predicates visible in the module, including predicates private to the
module.  This rule allows maximum flexibility, but it also breaks
encapsulation and should be used with care. The ciao language proposes
a different approach to this problem, see \cite DBLP:conf/cl/GrasH00 .

Modules are not always associated with a source-file. They
may range over several files, by using the
`include`directive. Moreover, they may not be associated to any source
file. As an example,
```
?- assert( nasa:launch(apollo,13) ).
```
will create a module `nasa`, if does not already exist. In fact it is
sufficient to call a predicate from a module to implicitly create the
module. Hence after this call:

```
?- nasa:launch(apollo,13).
```

there will be a `nasa`module in the system, even if nasa:launch/2 is
not at all defined.

 @}

*/


