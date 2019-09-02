@title

@page YAPModules  The YAP Module system

### [The YAP Module system ](YAPModules)
 <!--- $0 --->
<!--- $0 --->


The YAP module system is based on the Quintus/SISCtus module
  system @cite quintus . In this design, modules are named collections of predicates,
  and all predicates belong to a single module. By default, predicates are only
  visible within a module, or _private_ to that module. The module
  may also define a list of predicates that are
  _exported_, that is, visible to other modules.

  The main predicates in the module system are:

  * module/2 associates a source file to a module. It has two arguments: the name of the new module, and a list of predicates exported by the module.

  * use_module/1 and use_module/2 can be used to load a module. They take as first argument the source file for the module. Whereas use_module/1 loads all exported predicates, use_module/2 only takes the ones given by the second argument.

  YAP pre-defines a number of modules. Most system predicates belong to
    the module `prolog`. Predicates from the module `prolog` are
  automatically visible to every module.  The `system` module was
    introduced for SWI-Prolog compatibility, and in YAP mostly acts as an
  alias to `prolog`. The `user` module is also visible to all other modules.

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


  @section ExplicitNaming Explicit Naming 



  The module system allows one to _explicitly_ specify the source mode for
  a clause by prefixing a clause with its module, say:
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  user:(a :- b).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  it is also possible to type

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  user:a :- user:b.

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  both formulations describe the same clause, independently of the
  current type-in module.

  In fact, it is sufficient to specify the source mode for the clause's
  head:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  user:a :- b.
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if the current type-in module is `m`, the clause could also be written as:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  user:a :- m:b.
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The compiler rewrites the source clauses to ensure that explicit calls
  are respected, and that implicit calls are made to the current source
  module.

  A goal should refer to a predicate visible within the current type-in
  module. Thus, if a goal appears in a text file with a module
  declaration, the goal refers to that module's context (but see the
  initialization/1 directive for more details).

  Again, one can override this rule by prefixing a goal with a module to
  be consulted. The following query:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ?- nasa:launch(apollo,13).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ?- assert( nasa:launch(apollo,13) ).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  will create a module `nasa`, if does not already exist. In fact it is
  sufficient to call a predicate from a module to implicitly create the
  module. Hence after this call:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ?- nasa:launch(apollo,13).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  there will be a `nasa`module in the system, even if nasa:launch/2 is
  not at all defined.

   \pred use_module( +Files ) is directive
   loads a module file

  This predicate loads the file specified by _Files_, importing all
  their public predicates into the current type-in module. It is
  implemented as if by:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.yap}
  use_module(F) :-
  	load_files(F, [if(not_loaded),must_be_module(true)]).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Notice that _Files_ may be a single file, or a list with a number
  files. The _Files_  are loaded in YAP only once, even if they have been
  updated meanwhile. YAP should also verify whether the files actually
  define modules. Please consult load_files/3 for other options when
  loading a file.

  Predicate name clashes between two different modules may arise, either
  when trying to import predicates that are also defined in the current
  type-in module, or by trying to import the same predicate from two
  different modules.

  In the first case, the local predicate is considered to have priority
  and use_module/1 simply gives a warning. As an example, if the file
  `a.pl` contains:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  :- module( a, [a/1] ).

  :- use_module(b).

  a(1).
  a(X) :- b(X).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  and the file `b.pl` contains:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  :- module( b, [a/1,b/1] ).

  a(2).

  b(1).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  YAP will execute as follows:


  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ?- [a].
   % consulting .../a.pl...
    % consulting .../b.pl...
    % consulted .../b.pl in module b, 0 msec 0 bytes
   % consulted .../a.pl in module a, 1 msec 0 bytes
  true.   
   ?- a(X).
  X = 1 ? ;
  X = 1.
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The example shows that the query `a(X)`has a single answer, the one
  defined in `a.pl`. Calls to `a(X)`succeed in the top-level, because
  the module `a` was loaded into `user`. On the other hand, `b(X)`is not
  exported by `a.pl`, and is not available to calls, although it can be
  accessed as a predicate in the module 'a' by using the `:` operator.

  Next, consider the three files `c.pl`, `d1.pl`, and `d2.pl`:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  % c.pl
  :- module( c, [a/1] ).

  :- use_module([d1, d2]).

  a(X) :-
  	b(X).
  a(X) :-
  	c(X).
  a(X) :-
     d(X).

  % d1.pl
  :- module( d1, [b/1,c/1] ).

  b(2).
  c(3).


  % d2.pl
  :- module( d2, [b/1,d/1] ).

  b(1).
  d(4).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The result is as follows:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ./yap -l c
  YAP 6.3.4 (x86_64-darwin13.3.0): Tue Jul 15 10:42:11 CDT 2014

       ERROR!!
       at line 3 in o/d2.pl,
       PERMISSION ERROR- loading .../c.pl: modules d1 and d2 both define b/1
   ?- a(X).
  X = 2 ? ;
       ERROR!!
       EXISTENCE ERROR- procedure c/1 is undefined, called from context  prolog:$user_call/2
                   Goal was c:c(_131290)
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The state of  the module system after this error is undefined.

  @pred module(+ M:atom,+ L:list ) is directive
    the current file defines module _M_ with exports _L_. The list may include

    + predicate indicators

    + operator definitions that look like calls to op/3.

  The list _L_ may include predicates imported from other modules. If
  you want to fully reexport a module, or a sub-set, also consider reexport/1.

  Similar to module/2, this directive defines the file where it
  appears in as a module file; it must be the first declaration in the file.
   _M_ must be an atom specifying the module name;  _L_ must be a
  list containing the module's public predicates specification, in the
  form `[predicate_name/arity,...]`.

  The last argument  _Options_ must be a list of options, which can be:
       + <b>filename</b>
         the filename for a module to import into the current module.

       + <b>library( +File )</b>
         a library file to import into the current module.

       + <b>hide( +Opt)</b>
          if  _Opt_ is `false`, keep source code for current module, if `true`, disable.

       + <b>export(+PredicateIndicator )</b>
  		Add predicates to the public list of the context module. This implies
  	the predicate will be imported into another module if this module
  	is imported with use_module/1 and use_module/2.

       + <b>export_list(? _Mod_,? _ListOfPredicateIndicator_)</b>
         The list  _ListOfPredicateIndicator_ contains all predicates
       	exported by module  _Mod_

  Note that predicates are normally exported using the directive
  `module/2`. The `export/1` argumwnt is meant to allow export from
  dynamically created modules. The directive argument may also be a list
  of predicates.

   @pred  use_module(+Files, +Imports)
    loads a module file but only imports the named predicates


  This predicate loads the file specified by _Files_, importing their
  public predicates specified by _Imports_ into the current type-in
  module. It is implemented as if by:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  use_module(Files, Imports) :-
  	load_files(Files, [if(not_loaded),must_be_module(true),imports(Imports)]).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The _Imports_ argument may be use to specify which predicates one
  wants to load. It can also be used to give the predicates a different name. As an example,
  the graphs library is implemented on top of the red-black trees library, and some predicates are just aliases:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  :- use_module(library(rbtrees), [
  	rb_min/3 as min_assoc,
  	rb_max/3 as max_assoc,

          ...]).
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Unfortunately it is still not possible to change argument order.


  \pred module(+ M:atom,+ L:list ) is directive
    the current file defines module _M_ with exports _L_. The list may include

    + predicate indicators

    + operator definitions that look like calls to op/3.

  The list _L_ may include predicates imported from other modules. If
  you want to fully reexport a module, or a sub-set, also consider reexport/1.

  Similar to module/2, this directive defines the file where it
  appears in as a module file; it must be the first declaration in the file.
   _M_ must be an atom specifying the module name;  _L_ must be a
  list containing the module's public predicates specification, in the
  form `[predicate_name/arity,...]`.

  The last argument  _Options_ must be a list of options, which can be:
      +<b>filename</b>
         the filename for a module to import into the current module.

      + <b>library( +File )</b>
         a library file to import into the current module.

      + <b>hide( +Opt)</b>
          if  _Opt_ is `false`, keep source code for current module, if `true`, disable.

      + <b>export(+PredicateIndicator )</b>
  		Add predicates to the public list of the context module. This implies
  	the predicate will be imported into another module if this module
  	is imported with use_module/1 and use_module/2.

      + <b>export_list(? _Mod_,? _ListOfPredicateIndicator_)</b>
         The list  _ListOfPredicateIndicator_ contains all predicates
       	exported by module  _Mod_

  Note that predicates are normally exported using the directive
  `module/2`. The `export/1` argument is meant to allow export from
  dynamically created modules. The directive argument may also be a list
  of predicates.

  
