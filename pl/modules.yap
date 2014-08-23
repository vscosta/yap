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
* File:		modules.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	module support						 *
*									 *
*************************************************************************/

/**

   \defgroup YAPModules The YAP Module system 

   @group YAPProgramming

  The YAP module system is based on the Quintus/SISCtus module
system. In this design, modules are named collections of predicates,
and all predicates belong to a single module. Predicates are only
visible within a module, or _private_ to that module, but the module
will most often will also define a list of predicates that are
_exported_, that is, visible to other modules.

The main predicates in the module system are:

     * module/2 associates a source file to a module. It has two arguments: the name of the new module, and a list of predicates exported by the module.

     * use_module/1 and use_module/2 can be used to load a module. They take as first argument the source file for the module. Whereas use_module/1 loads all exported predicates, use_module/2 only takes the ones given by the second argument.

YAP pre-defines a number of modules. Most system predicates belong to
the module `prolog`. Predicates from the module `prolog` are
automatically visible to every module.  The `system` module was
introduced for SWI-Prolog compatibility, and in YAP mostly acts as an
alias to `prolog`.

YAP is always associated to a module, the current <em>source
module</em> or <em>type-in module</em>. By default, all predicates
read-in and all calls to a goal will be made to predicates visible to
the current source module, Initially, the source module for YAP is the
module `user`. Thus Prolog programs that do not define modules will
operate within the `user` module. In this case, all predicates will be
visible to all source files.

YAP also includes a number of libraries and packages, most of them
 defining their own modules. Note that there is no system mechanism to
 avoid clashes between module names, so it is up to the programmer to
 carefully choose the names for her own program modules.

The main mechanism to change the current type-in module is by using
the module/2 declaration.This declaration sets the source module when
it starts consulting a file, and resets it at the end.  One can set
the type-in module permanently by using the built-in `module/1`.

\subsection Explicit Naming

The module system allows one to _explicitly_ specify the source mode for
a clause by prefixing a clause with its module, say:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
user:(a :- b).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

it is also possible to type

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl 

user:a :- user:b.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

both formulations describe the same clause, independently of the
current type-in module.

In fact, it is sufficient to specify the source mode for the clause's
head:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
user:a :- b.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if the current type-in module is `m`, the clause could also be written as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
?- nasa:launch(apollo,13).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 invokes the goal `launch(apollo,13)` as if the current source
module was `nasa`.

YAP and other Prolog systems allow the module prefix to see all
predicates visible in the module, including predicates private to the
module.  This rule allows maximum flexibility, but it also breaks
encapsulation and should be used with care. The ciao language proposes
a different approach to this problem, see \cite .

Modules are not always associated with a source-file. They
may range over several files, by using the
`include`directive. Moreover, they may not be associated to any source
file. As an example, 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
?- assert( nasa:launch(apollo,13) ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will create a module `nasa`, if does not already exist. In fact it is
sufficient to call a predicate from a module to implicitly create the
module. Hence after this call:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
?- nasa:launch(apollo,13).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

there will be a `nasa`module in the system, even if nasa:launch/2 is
not at all defined.

\{ 

**/
:- system_module( '$_modules', [abolish_module/1,
        add_import_module/3,
        current_module/1,
        current_module/2,
        delete_import_module/2,
        expand_goal/2,
        export/1,
        export_list/2,
        export_resource/1,
        import_module/2,
        ls_imports/0,
        module/1,
        module_property/2,
        set_base_module/1,
        source_module/1,
        use_module/1,
        use_module/2,
        use_module/3], ['$add_to_imports'/3,
        '$clean_cuts'/2,
        '$convert_for_export'/7,
        '$do_import'/3,
        '$extend_exports'/3,
        '$get_undefined_pred'/4,
        '$imported_pred'/4,
        '$meta_expansion'/6,
        '$meta_predicate'/2,
        '$meta_predicate'/4,
        '$module'/3,
        '$module'/4,
        '$module_expansion'/5,
        '$module_transparent'/2,
        '$module_transparent'/4]).

:- use_system_module( '$_arith', ['$c_built_in'/3]).

:- use_system_module( '$_consult', ['$lf_opt'/3,
        '$load_files'/3]).

:- use_system_module( '$_debug', ['$skipeol'/1]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_eval', ['$full_clause_optimisation'/4]).

:- '$purge_clauses'(module(_,_), prolog).
:- '$purge_clauses'('$module'(_,_), prolog).
:- '$purge_clauses'(use_module(_), prolog).
:- '$purge_clauses'(use_module(_,_), prolog).
%
% start using default definition of module.
%

/**
   \pred use_module( +Files ) is directive
   loads a module file

This predicate loads the file specified by _Files_, importing all
their public predicates into the current type-in module. It is
implemented as if by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
:- module( a, [a/1] ).

:- use_module(b).

a(1).
a(X) :- b(X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and the file `b.pl` contains:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
:- module( b, [a/1,b/1] ).

a(2).

b(1).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

YAP will execute as follows:


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
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


**/

use_module(F) :- '$load_files'(F,
			       [if(not_loaded),must_be_module(true)], use_module(F)).

	

/**
  \pred  use_module(+Files, +Imports)
  loads a module file but only imports the named predicates


This predicate loads the file specified by _Files_, importing their
public predicates specified by _Imports_ into the current type-in
module. It is implemented as if by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
use_module(Files, Imports) :-
	load_files(Files, [if(not_loaded),must_be_module(true),imports(Imports)]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The _Imports_ argument may be use to specify which predicates one
wants to load. It can also be used to give the predicates a different name. As an example,
the graphs library is implemented on top of the red-black trees library, and some predicates are just aliases:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
:- use_module(library(rbtrees), [
	rb_min/3 as min_assoc,
	rb_max/3 as max_assoc,

        ...]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unfortunately it is still not possible to change argument order.

**/
use_module(F,Is) :-
	'$load_files'(F, [if(not_loaded),must_be_module(true),imports(Is)], use_module(F,Is)).

/** 
  \pred module(+M) is det
   set the type-in module


Defines  _M_ to be the current working or type-in module. All files
which are not bound to a module are assumed to belong to the working
module (also referred to as type-in module). To compile a non-module
file into a module which is not the working one, prefix the file name
with the module name, in the form ` _Module_: _File_`, when
loading the file.

**/
module(N) :-
	var(N), 
	'$do_error'(instantiation_error,module(N)).
module(N) :-
	atom(N), !,
	% set it as current module.
	'$current_module'(_,N).
module(N) :-
	'$do_error'(type_error(atom,N),module(N)).

/**
 \pred	module(+ Module:atom, +ExportList:list) is directive
  define a new module

This directive defines the file where it appears as a _module file_;
it must be the first declaration in the file.  _Module_ must be an
atom specifying the module name; _ExportList_ must be a list
containing the module's public predicates specification, in the form
`[predicate_name/arity,...]`. The _ExportList_ can  include
operator declarations for operators that are exported by the module.

The public predicates of a module file can be made accessible to other
files through loading the source file, using the directives
use_module/1 or use_module/2,
ensure_loaded/1 and the predicates
consult/1 or reconsult/1. The
non-public predicates of a module file are not supposed to be visible
to other modules; they can, however, be accessed by prefixing the module
name with the `:/2` operator.

**/
'$module_dec'(N, Ps) :-
	source_location(F, _),
	'$add_module_on_file'(N, F, Ps),
	'$current_module'(_,N).

'$module'(_,N,P) :-
	'$module_dec'(N,P).

/** 
 \pred module(+ M:atom,+ L:list ) is directive
  the current file defines module _M_ with exports _L_. The list may include

  + predicatae indicators
  
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

**/
'$module'(O,N,P,Opts) :- !,
	'$module'(O,N,P),
	'$process_module_decls_options'(Opts,module(Opts,N,P)).

	
'$process_module_decls_options'(Var,Mod) :-
	var(Var), !,
	'$do_error'(instantiation_error,Mod).
'$process_module_decls_options'([],_) :- !.
'$process_module_decls_options'([H|L],M) :- !,
	'$process_module_decls_option'(H,M),
	'$process_module_decls_options'(L,M).
'$process_module_decls_options'(T,M) :-
	'$do_error'(type_error(list,T),M).

'$process_module_decls_option'(Var,M) :- 
	var(Var), 
	'$do_error'(instantiation_error,M).
'$process_module_decls_option'(At,M) :- 
	atom(At), !,
	use_module(M:At).
'$process_module_decls_option'(library(L),M) :- !,
	use_module(M:library(L)).
'$process_module_decls_option'(hidden(Bool),M) :- !,
	'$process_hidden_module'(Bool, M).
'$process_module_decls_option'(Opt,M) :- 
	'$do_error'(domain_error(module_decl_options,Opt),M).

'$process_hidden_module'(TNew,M) :-
        '$convert_true_off_mod3'(TNew, New, M),
	source_mode(Old, New),
	'$prepare_restore_hidden'(Old,New).

'$convert_true_off_mod3'(true, off, _) :- !.
'$convert_true_off_mod3'(false, on, _) :- !.
'$convert_true_off_mod3'(X, _, M) :-
	'$do_error'(domain_error(module_decl_options,hidden(X)),M).

'$prepare_restore_hidden'(Old,Old) :- !.
'$prepare_restore_hidden'(Old,New) :-
	recorda('$system_initialisation', source_mode(New,Old), _).

'$add_module_on_file'(DonorMod, DonorF, Exports) :-
    recorded('$module','$module'(DonorF, DonorMod, _, _),R),
    % the module has been found, are we reconsulting?
    (
	DonorF \= OtherF
    ->
        '$do_error'(permission_error(module,redefined,DonorMod, OtherFile, DonorF),module(Mod,Exports))
    ;
      recorded('$module','$module'(DonorF,DonorM, _, _), R),
      erase( R ),
      fail
    ).
'$add_module_on_file'(DonorM, DonorF, Exports) :-
    '$current_module'( HostM ),
    ( recorded('$module','$module'( HostF, HostM, _, _),_) -> true ; HostF = user_input ),
    % first build the initial export tablee
    '$convert_for_export'(all, Exports, DonorM, HostM, TranslationTab, AllExports0, load_files),
    sort( AllExports0, AllExports ),
    ( source_location(_, Line) -> true ; Line = 0 ),
    '$add_to_imports'(TranslationTab, DonorM, DonorM), % insert ops, at least for now
    % last, export everything to the host: if the loading crashed you didn't actually do
    % no evil.
    recorda('$module','$module'(DonorF,DonorM,AllExports, Line),_).

'$extend_exports'(HostF, Exports, DonorF ) :-
    ( recorded('$module','$module'( DonorF, DonorM, _, DonorExports),_) -> true ; DonorF = user_input ),
    ( recorded('$module','$module'( HostF, HostM, _, _),_) -> true ; HostF = user_input ),
    recorded('$module','$module'(HostF,HostM,AllExports, _Line), R), erase(R),
    '$convert_for_export'(Exports, DonorExports, DonorM, HostM, TranslationTab, AllReExports, reexport(DonorF, Exports)),
    lists:append( AllReExports, AllExports, Everything0 ),
    sort( Everything0, Everything ),
    ( source_location(_, Line) -> true ; Line = 0 ),
    recorda('$module','$module'(HostF,HostM,Everything, Line),_).

'$module_produced by'(M, M0, N, K) :-
	recorded('$import','$import'(M,M0,_,_,N,K),_), !.
'$module_produced by'(M, M0, N, K) :-
	recorded('$import','$import'(MI,M0,G1,_,N,K),_),
	functor(G1, N1, K1),
	'$module_produced by'(M,MI,N1,K1).	


/** \pred current_module( ? Mod:atom) is nondet
   : _Mod_ is any user-visible module.

*/
current_module(Mod) :-
	'$all_current_modules'(Mod),
	\+ '$system_module'(Mod).

/** \pred current_module( ? Mod:atom, ? File : file ) is nondet
   : _Mod_ is any user-visible module and _File_ its source file, or `user` if none exists.

*/
current_module(Mod,TFN) :-
	'$all_current_modules'(Mod),
	( recorded('$module','$module'(TFN,Mod,_Publics, _),_) -> true ; TFN = user ).

/** \pred source_module( - Mod:atom ) is nondet
   : _Mod_ is the current read-in or source module.

*/
source_module(Mod) :-
	'$current_module'(Mod).


% expand module names in a clause
% A1: Input Clause
% A2: Output Class to Compiler (lives in module HM) 
% A3: Output Class to clause/2 and listing (lives in module HM)
%
% modules:
% A4: module for body of clause (this is the one used in looking up predicates)
% A5: context module (this is the current context
% A6: head module (this is the one used in compiling and accessing).
%
%
'$module_expansion'(H,H,H,_M,_HM) :- var(H), !.
'$module_expansion'((H:-B),(H:-B1),(H:-BOO),M,HM) :- !,
	'$is_mt'(M, H, B, IB, MM),
	'$module_u_vars'(H,UVars,M),	 % collect head variables in
					 % expanded positions
	'$module_expansion'(IB,B1,BO,M,MM,HM,UVars),
	('$full_clause_optimisation'(H, M, BO, BOO) ->
	  true
	  ;
	  BO = BOO
	).
% do not expand bodyless clauses.
'$module_expansion'(H,H,H,_,_).


'$trace_module'(X) :-
	telling(F),
	tell('P0:debug'),
	write(X),nl,
	tell(F), fail.
'$trace_module'(_).

'$trace_module'(X,Y) :- X==Y, !.
'$trace_module'(X,Y) :-
	telling(F),
	tell('~/.dbg.modules'),
	write('***************'), nl,
	portray_clause(X),
	portray_clause(Y),
	tell(F),fail.
'$trace_module'(_,_).

	
% expand module names in a body
% args are:
%       goals to expand
%       code to pass to listing
%       code to pass to compiler
%       current module for looking up preds  M
%       source module  SM
%       head module   HM
%
% to understand the differences, you can consider:
%
%  a:(d:b(X) :- g:c(X), d(X), user:hello(X)).
%
% when we process meta-predicate c, HM=d, DM=a, BM=a, M=g and we should get:
%
%  d:b(X) :- g:c(g:X), a:d(X), user:hello(X).
%
% on the other hand,
%
%  a:(d:b(X) :- c(X), d(X), d:e(X)).
%
% will give
%
%  d:b(X) :- a:c(a:X), a:d(X), e(X).
%
%
%       head variables.
%       goals or arguments/sub-arguments?
% I cannot use call here because of format/3
% modules:
% A4: module for body of clause (this is the one used in looking up predicates)
% A5: context module (this is the current context
% A6: head module (this is the one used in compiling and accessing).
%
%
'$module_expansion'(V,NG,NG,_,SM,_,HVars) :-
	var(V), !,
	( '$not_in_vars'(V,HVars)
	->
	  NG = call(SM:V)
	;
	  NG = call(V)
	).	
'$module_expansion'((A,B),(A1,B1),(AO,BO),M,SM,HM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,SM,HM,HVars),
	'$module_expansion'(B,B1,BO,M,SM,HM,HVars).
'$module_expansion'((A;B),(A1;B1),(AO;BO),M,SM,HM,HVars) :- var(A), !,
	'$module_expansion'(A,A1,AO,M,SM,HM,HVars),
	'$module_expansion'(B,B1,BO,M,SM,HM,HVars).
'$module_expansion'((A*->B;C),(A1*->B1;C1),(yap_hacks:current_choicepoint(DCP),AO,yap_hacks:cut_at(DCP),BO; CO),M,SM,HM,HVars) :- !,
	'$module_expansion'(A,A1,AOO,M,SM,HM,HVars),
	'$clean_cuts'(AOO, AO),
	'$module_expansion'(B,B1,BO,M,SM,HM,HVars),
	'$module_expansion'(C,C1,CO,M,SM,HM,HVars).
'$module_expansion'((A;B),(A1;B1),(AO;BO),M,SM,HM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,SM,HM,HVars),
	'$module_expansion'(B,B1,BO,M,SM,HM,HVars).
'$module_expansion'((A|B),(A1|B1),(AO|BO),M,SM,HM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,SM,HM,HVars),
	'$module_expansion'(B,B1,BO,M,SM,HM,HVars).
'$module_expansion'((A->B),(A1->B1),(AO->BO),M,SM,HM,HVars) :- !,
	'$module_expansion'(A,A1,AOO,M,SM,HM,HVars),
	'$clean_cuts'(AOO, AO),
	'$module_expansion'(B,B1,BO,M,SM,HM,HVars).
'$module_expansion'(\+A,\+A1,\+AO,M,SM,HM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,SM,HM,HVars).
'$module_expansion'(not(A),not(A1),not(AO),M,SM,HM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,SM,HM,HVars).
'$module_expansion'(true,true,true,_,_,_,_) :- !.
'$module_expansion'(fail,fail,fail,_,_,_,_) :- !.
'$module_expansion'(false,false,false,_,_,_,_) :- !.
% if I don't know what the module is, I cannot do anything to the goal,
% so I just put a call for later on.
'$module_expansion'(M:G,call(M:G),'$execute_wo_mod'(G,M),_,_,_,_) :- var(M), !.
'$module_expansion'(M:G,G1,GO,_,_CM,HM,HVars) :- !,
	'$module_expansion'(G,G1,GO,M,M,HM,HVars).
'$module_expansion'(G, G1, GO, CurMod, SM, HM,HVars) :-
	% is this imported from some other module M1?
	'$imported_pred'(G, CurMod, GG, M1),
	!,
	'$module_expansion'(GG, G1, GO, M1, SM, HM,HVars).
'$module_expansion'(G, G1, GO, CurMod, SM, HM,HVars) :-
	'$meta_expansion'(G, CurMod, SM, HM, GI, HVars), !,
	'$complete_goal_expansion'(GI, CurMod, SM, HM, G1, GO, HVars).
'$module_expansion'(G, G1, GO, CurMod, SM, HM, HVars) :-
	'$complete_goal_expansion'(G, CurMod, SM, HM, G1, GO, HVars).

expand_goal(G, G) :-
	var(G), !.
expand_goal(M:G, M:NG) :-
	'$do_expand'(G, M, prolog, [], NG), !.
expand_goal(G, NG) :- 
	'$current_module'(Mod),
	'$do_expand'(G, Mod, prolog, [],  NG), !.
expand_goal(G, G).
	
'$do_expand'(G, _, _, _, G) :- var(G), !.
'$do_expand'(M:G, _CurMod, SM, HVars, M:GI) :- !,
	'$do_expand'(G, M, SM, HVars, GI).
'$do_expand'(G, CurMod, _SM, _HVars, GI) :-
	(
	 '$pred_exists'(goal_expansion(G,GI), CurMod),
	 call(CurMod:goal_expansion(G, GI))
	->
	 true
	;
	 '$pred_exists'(goal_expansion(G,GI), system),
	 system:goal_expansion(G, GI)
	->
	  true
	;
	 user:goal_expansion(G, CurMod, GI)
	->
	  true
	;
	  user:goal_expansion(G, GI)
	), !.
'$do_expand'(G, CurMod, SM, HVars, NG) :-
	'$is_metapredicate'(G,CurMod), !,
	functor(G, Name, Arity),
	prolog:'$meta_predicate'(Name,CurMod,Arity,PredDef),
	G =.. [Name|GArgs],
	PredDef =.. [Name|GDefs],
	'$expand_args'(GArgs, CurMod, SM, GDefs, HVars, NGArgs),
	NG =.. [Name|NGArgs].

'$expand_args'([], _, _, [], _, []).
'$expand_args'([A|GArgs], CurMod, SM,  [0|GDefs], HVars, [NA|NGArgs]) :-
	'$do_expand'(A, CurMod, SM, HVars, NA), !,
	'$expand_args'(GArgs, CurMod, SM, GDefs, HVars, NGArgs).
'$expand_args'([A|GArgs], CurMod, SM, [_|GDefs], HVars, [A|NGArgs]) :-
	'$expand_args'(GArgs, CurMod, SM, GDefs, HVars, NGArgs).

% args are:
%       goal to expand
%       current module for looking up pred
%       context module 
%       current module from head of clause  
% :- module(m, []). o:p :- n:(g, l).
%
%       goal to pass to listing
%       goal to pass to compiler
%       head variables.
'$complete_goal_expansion'(G, CurMod, SM, HM, G1, GO, HVars) :-
%	'$pred_goal_expansion_on',
	'$do_expand'(G, CurMod, SM, HVars, GI),
	GI \== G, !,
	'$module_expansion'(GI, G1, GO, CurMod, SM, HM, HVars).
'$complete_goal_expansion'(G, M, _SM, HM, G1, G2, _HVars) :-
	'$all_system_predicate'(G,M,ORIG), !,
	% make built-in processing transparent.
	'$match_mod'(G, M, ORIG, HM, G1),
	'$c_built_in'(G1, M, Gi),
	Gi = G2.
'$complete_goal_expansion'(G, GMod, _, HM, NG, NG, _) :-
	'$match_mod'(G, GMod, GMod, HM, NG).

%'$match_mod'(G, GMod, GMod, NG) :- !,
%	NG = G.
'$match_mod'(G, _, M, _, G) :- 
    nonvar(G),
    '$system_predicate'(G,prolog),
%    \+ '$is_metapredicate'(G, prolog),
    \+ '$is_multifile'(G,H), 
    !. % prolog: needs no module info.
% same module as head,  and body goal (I cannot get rid of qualifier before
% meta-call.
'$match_mod'(G, HMod, _, HM, G) :- HMod == HM, !.
'$match_mod'(G, GMod, _, _,  GMod:G).


% be careful here not to generate an undefined exception.
'$imported_pred'(G, ImportingMod, G0, ExportingMod) :-
	'$enter_undefp',
	'$undefined'(G, ImportingMod),
	'$get_undefined_pred'(G, ImportingMod, G0, ExportingMod),
	ExportingMod \= ImportingMod, !,
	'$exit_undefp'.
'$imported_pred'(_G, _ImportingMod, _, _) :-
	'$exit_undefp',
	fail.

'$get_undefined_pred'(G, ImportingMod, G0, ExportingMod) :-
	recorded('$import','$import'(ExportingModI,ImportingMod,G0I,G,_,_),_),
	'$continue_imported'(ExportingMod, ExportingModI, G0, G0I), !.
% SWI builtin
'$get_undefined_pred'(G, _ImportingMod, G0, ExportingMod) :-
	recorded('$dialect',Dialect,_),
	Dialect \= yap,
	functor(G, Name, Arity),
	call(Dialect:index(Name,Arity,ExportingModI,_)), !,
	'$continue_imported'(ExportingMod, ExportingModI, G0, G), !.
'$get_undefined_pred'(G, _ImportingMod, G0, ExportingMod) :-
	yap_flag(autoload, V),
	V = true,
	'$autoloader_find_predicate'(G,ExportingModI), !,
	'$continue_imported'(ExportingMod, ExportingModI, G0, G).
'$get_undefined_pred'(G, ImportingMod, G0, ExportingMod) :-
	prolog:'$parent_module'(ImportingMod,ExportingModI),
	'$continue_imported'(ExportingMod, ExportingModI, G0, G).


'$autoloader_find_predicate'(G,ExportingModI) :-
	'$nb_getval'('$autoloader_set', true, fail), !,
	autoloader:find_predicate(G,ExportingModI).
'$autoloader_find_predicate'(G,ExportingModI) :-
	'$exit_undefp',
	yap_flag(autoload, false), 
	load_files([library(autoloader),
		    autoloader:library('INDEX'),
		    swi:library('dialect/swi/INDEX')],
		   [autoload(true),if(not_loaded)]),
	nb_setval('$autoloader_set', true),
	yap_flag(autoload, true), 
	'$enter_undefp',
	autoloader:find_predicate(G,ExportingModI).


'$continue_imported'(Mod,Mod,Pred,Pred) :-
	\+ '$undefined'(Pred, Mod), !.
'$continue_imported'(FM,Mod,FPred,Pred) :-
	recorded('$import','$import'(IM,Mod,IPred,Pred,_,_),_), !,
	'$continue_imported'(FM, IM, FPred, IPred).
'$continue_imported'(FM,Mod,FPred,Pred) :-
	prolog:'$parent_module'(Mod,IM),
	'$continue_imported'(FM, IM, FPred, Pred).


/**

 \defgroup YAPMetaPredicates Using Meta-Calls with Modules
 \ingroup YAPModules

@{

  \pred meta_predicate(_G1_,...., _Gn) is directive

Declares that this predicate manipulates references to predicates.
Each _Gi_ is a mode specification.

If the argument is `:`, it does not refer directly to a predicate
but must be module expanded. If the argument is an integer, the argument
is a goal or a closure and must be expanded. Otherwise, the argument is 
not expanded. Note that the system already includes declarations for all 
built-ins.

For example, the declaration for call/1 and setof/3 are:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- meta_predicate call(0), setof(?,0,?).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

meta_predicate declaration
 implemented by asserting $meta_predicate(SourceModule,Functor,Arity,Declaration)

*/


% directive now meta_predicate Ps :- $meta_predicate(Ps).

:- dynamic('$meta_predicate'/4).

:- multifile '$meta_predicate'/4.

'$meta_predicate'(P, M) :-
	var(P),
	'$do_error'(instantiation_error,module(M)).
'$meta_predicate'((P,Ps), M) :- !, 
	'$meta_predicate'(P, M),
	'$meta_predicate'(Ps, M).
'$meta_predicate'(M:D, _) :- !,
	'$meta_predicate'(D, M).
'$meta_predicate'(P, M1) :-
	'$install_meta_predicate'(P, M1).


'$install_meta_predicate'(P, M1) :-
	functor(P,F,N),
	( M1 = prolog -> M = _ ; M1 = M),
	( retractall(prolog:'$meta_predicate'(F,M,N,_)), fail ; true),
	asserta(prolog:'$meta_predicate'(F,M,N,P)),
	'$flags'(P, M1, Fl, Fl),
	NFlags is Fl \/ 0x200000,
	'$flags'(P, M1, Fl, NFlags).

% return list of vars in expanded positions on the head of a clause.
%
% these variables should not be expanded by meta-calls in the body of the goal.
%
'$module_u_vars'(H,UVars,M) :-
	functor(H,F,N),
	'$meta_predicate'(F,M,N,D), !,
	'$module_u_vars'(N,D,H,UVars).
'$module_u_vars'(_,[],_).

'$module_u_vars'(0,_,_,[]) :- !.
'$module_u_vars'(I,D,H,LF) :-
	arg(I,D,X), ( X=':' ; integer(X)),
	arg(I,H,A), '$uvar'(A, LF, L), !,
	I1 is I-1,
	'$module_u_vars'(I1,D,H,L).
'$module_u_vars'(I,D,H,L) :-
	I1 is I-1,
	'$module_u_vars'(I1,D,H,L).

'$uvar'(Y, [Y|L], L)  :- var(Y), !.
% support all/3
'$uvar'(same( G, _), LF, L)  :-
    '$uvar'(G, LF, L).
'$uvar'('^'( _, G), LF, L)  :-
    '$uvar'(G, LF, L).

% expand arguments of a meta-predicate
% $meta_expansion(ModuleWhereDefined,CurrentModule,Goal,ExpandedGoal,MetaVariables)


'$meta_expansion'(G,Mod,MP,HM,G1,HVars) :-
	functor(G,F,N),
	'$meta_predicate'(F,Mod,N,D), !, % we're in an argument
%	format(user_error,'[ ~w ',[G]),
	functor(G1,F,N),
	'$meta_expansion_loop'(N, D, G, G1, HVars, Mod, MP, HM).
%	format(user_error,' gives ~w]`n',[G1]).


% expand argument
'$meta_expansion_loop'(0,_,_,_,_,_,_,_) :- !.
'$meta_expansion_loop'(I,D,G,NG,HVars,CurMod,M,HM) :- 
	arg(I,D,X), (X==':' -> true ; integer(X)),
	arg(I,G,A), 
	'$should_expand'(A,HVars),
	!,
	( X ==0 ->
	  '$values'('$c_arith',Old, false),
	  '$meta_expansion0'(A,CurMod,M,HM,NA,HVars),
	  '$values'('$c_arith', _False, Old)
	  ;
	  NA = M:A
	),
	arg(I,NG,NA),
	I1 is I-1,
	'$meta_expansion_loop'(I1, D, G, NG, HVars, CurMod, M, HM).
'$meta_expansion_loop'(I, D, G, NG, HVars, CurMod, M, HM) :- 
	arg(I,G,A),
	arg(I,NG,A),
	I1 is I-1,
	'$meta_expansion_loop'(I1, D, G, NG, HVars, CurMod, M, HM).

'$meta_expansion0'(G,_Mod,MP,_HM, G1,_HVars) :-
    var(G), !,
    G1 = MP:G.
'$meta_expansion0'(M:G,_Mod,_MP,_HM,G1,_HVars) :-
    var(M), !,
    G1 = '$execute_wo_mod'(G,M).
% support for all/3
'$meta_expansion0'(same(G, P),Mod,MP,HM, same(G1, P),HVars) :- !,
    '$meta_expansion0'(G,Mod,MP,HM,G1,HVars).
'$meta_expansion0'(G,Mod,MP,HM,M1:G1,HVars) :-
    strip_module(Mod:G,M1,G1).


% check if an argument should be expanded
'$should_expand'(V,HVars) :- var(V), !, '$not_in_vars'(V,HVars).
'$should_expand'(_:_,_) :- !, fail.
'$should_expand'(_,_).

'$not_in_vars'(_,[]).
'$not_in_vars'(V,[X|L]) :- X\==V, '$not_in_vars'(V,L).

/*
 \pred module_transparent( + _Preds_ ) is directive
   _Preds_ is a list of predicates that can access the calling context.

 _Preds_ is a comma separated sequence of name/arity predicate
indicators (like in dynamic/1). Each goal associated with a
transparent declared predicate will inherit the context module from
its parent goal. 

*/
:- dynamic('$module_transparent'/4).

'$module_transparent'((P,Ps), M) :- !, 
	'$module_transparent'(P, M),
	'$module_transparent'(Ps, M).
'$module_transparent'(M:D, _) :- !,
	'$module_transparent'(D, M).
'$module_transparent'(F/N, M) :-
	'$module_transparent'(F,M,N,_), !.
'$module_transparent'(F/N, M) :-
	functor(P,F,N),
	asserta(prolog:'$module_transparent'(F,M,N,P)),
	'$flags'(P, M, Fl, Fl),
	NFlags is Fl \/ 0x200004,
	'$flags'(P, M, Fl, NFlags).

'$is_mt'(M, H, B, (context_module(CM),B), CM) :-
	'$module_transparent'(_, M, _, H), !.
'$is_mt'(M, _, B, B, M).

% comma has its own problems.
:- '$install_meta_predicate'(','(0,0), prolog).

:- meta_predicate
	abolish(:),
	abolish(:,+),
	all(?,0,-),
	assert(:),
	assert(:,+),
	assert_static(:),
	asserta(:),
	asserta(:,+),
	asserta_static(:),
	assertz(:),
	assertz(:,+),
	assertz_static(:),
	at_halt(0),
	bagof(?,0,-),
	bb_get(:,-),
	bb_put(:,+),
	bb_delete(:,?),
	bb_update(:,?,?),
	call(0),
	call(1,?),
	call(2,?,?),
	call(3,?,?,?),
	call_with_args(0),
	call_with_args(1,?),
	call_with_args(2,?,?),
	call_with_args(3,?,?,?),
	call_with_args(4,?,?,?,?),
	call_with_args(5,?,?,?,?,?),
	call_with_args(6,?,?,?,?,?,?),
	call_with_args(7,?,?,?,?,?,?,?),
	call_with_args(8,?,?,?,?,?,?,?,?),
	call_with_args(9,?,?,?,?,?,?,?,?,?),
	call_cleanup(0,0),
	call_cleanup(0,?,0),
	call_residue(0,?),
	call_residue_vars(0,?),
	call_shared_object_function(:,+),
	catch(0,?,0),
	clause(:,?),
	clause(:,?,?),
	compile(:),
	consult(:),
	current_predicate(:),
	current_predicate(?,:),
	db_files(:),
	depth_bound_call(0,+),
	discontiguous(:),
	ensure_loaded(:),
	exo_files(:),
	findall(?,0,-),
	findall(?,0,-,?),
	forall(0,0),
	format(+,:),
	format(+,+,:),
	freeze(?,0),
	hide_predicate(:),
	if(0,0,0),
	ignore(0),
	incore(0),
	multifile(:),
	nospy(:),
        not(0),
        notrace(0),
        once(0),
        phrase(2,?),
        phrase(2,?,+),
	predicate_property(:,?),
	predicate_statistics(:,-,-,-),
	on_exception(+,0,0),
	qsave_program(+,:),
	reconsult(:),
	retract(:),
	retract(:,?),
	retractall(:),
	reconsult(:),
	setof(?,0,-),
	setup_call_cleanup(0,0,0),
	setup_call_catcher_cleanup(0,0,?,0),
	spy(:),
	stash_predicate(:),
	unknown(+,:),
	use_module(:),
	use_module(:,?),
	use_module(?,:,?),
	when(+,0),
	with_mutex(+,0),
	with_output_to(?,0),
	'->'(0 , 0),
	'*->'(0 , 0),
	';'(0 , 0),
	','(0 , 0),
	^(+,0),
	{}(0,?,?),
	','(2,2,?,?),
	;(2,2,?,?),
	'|'(2,2,?,?),
	->(2,2,?,?),
	\+(2,?,?),
	\+( 0 ).

/** 

@}

@{
 \defgroup YAPDynamicYAPModules Dynamic Modules
 \ingroup YAPModules

  YAP (in the footsteps of SWI-Prolog) allows to create modules that
  are not bound to files. One application is in Inductive Logic Programming,
  where dynamic modules can be used to represent training examples. YAP now include 
  built-ins to create a module. manipulate its interface, and eventually abolish the 
  module, releasing all the data therein.

*/

/**

  \pred declare_module(+Module, +Super, +File, +Line, +Redefine) is det
   declare explicitely a module

Start a new (source-)module _Module_ that inherits all exports from
_Super_. The module is as if defined in file _File_ and _Line_ and if _Redefine_
holds true may
be associated to a new file.

\param[in]	_Module_ is the name of the module to declare
\param[in]	_MSuper_ is the name of the context module. Use `prolog`or `system`
                if you do not need a context.
\param[in]	_File_ is the canonical name of the file from which the module is loaded
\param[in]  Line is the line-number of the :- module/2 directive.
\param[in]	 If _Redefine_ `true`, allow associating the module to a new file
*/

'$declare_module'(Name, _Super, Context, _File, _Line) :-
	add_import_module(Name, Context, start).


/**
 \pred abolish_module( + Mod) is det
 get rid of a module and of all predicates included in the module.
*/
abolish_module(Mod) :-
	recorded('$module','$module'(_,Mod,_,_),R), erase(R),
	fail.
abolish_module(Mod) :-
	recorded('$import','$import'(Mod,_,_,_,_,_),R), erase(R),
	fail.
abolish_module(Mod) :-
	'$current_predicate'(Mod,Na,Ar),
	abolish(Mod:Na/Ar),
	fail.
abolish_module(_).

export(Resource) :-
	var(Resource),
	'$do_error'(instantiation_error,export(Resource)).	
export([]) :- !.
export([Resource| Resources]) :- !,
	export_resource(Resource),
	export(Resources).
export(Resource) :-
	export_resource(Resource).

export_resource(Resource) :-
	var(Resource), !,
	'$do_error'(instantiation_error,export(Resource)).	
export_resource(P) :-
	P = F/N, atom(F), number(N), N >= 0, !,
	'$current_module'(Mod), 
	(	recorded('$module','$module'(File,Mod,ExportedPreds,Line),R) ->
		erase(R), 
		recorda('$module','$module'(File,Mod,[P|ExportedPreds],Line),_)
	;	prolog_load_context(file, File) ->
		recorda('$module','$module'(File,Mod,[P],Line),_)
	;	recorda('$module','$module'(user_input,Mod,[P],1),_)
	).
export_resource(P0) :-
	P0 = F//N, atom(F), number(N), N >= 0, !,
	N1 is N+2, P = F/N1,
	'$current_module'(Mod), 
	(	recorded('$module','$module'(File,Mod,ExportedPreds,Line),R) ->
		erase(R), 
		recorda('$module','$module'(File,Mod,[P|ExportedPreds],Line ),_)
	;	prolog_load_context(file, File) ->
		recorda('$module','$module'(File,Mod,[P],Line),_)
	;	recorda('$module','$module'(user_input,Mod,[P],1),_)
	).
export_resource(op(Prio,Assoc,Name)) :- !,
	op(Prio,Assoc,prolog:Name).
export_resource(Resource) :-
	'$do_error'(type_error(predicate_indicator,Resource),export(Resource)).
	
export_list(Module, List) :-
	recorded('$module','$module'(_,Module,List,_),_).

'$convert_for_export'(all, Exports, _Module, _ContextModule, Tab, MyExports, _) :-
	'$simple_conversion'(Exports, Tab, MyExports).
'$convert_for_export'([], Exports, Module, ContextModule, Tab, MyExports, Goal) :-
	'$clean_conversion'([], Exports, Module, ContextModule, Tab, MyExports, Goal).
'$convert_for_export'([P1|Ps], Exports, Module, ContextModule, Tab, MyExports, Goal) :-
	'$clean_conversion'([P1|Ps], Exports, Module, ContextModule, Tab, MyExports, Goal).
'$convert_for_export'(except(Excepts), Exports, Module, ContextModule, Tab, MyExports, Goal) :-
	'$neg_conversion'(Excepts, Exports, Module, ContextModule, MyExports, Goal),
	'$simple_conversion'(MyExports, Tab, _).

'$simple_conversion'([], [], []).
'$simple_conversion'([F/N|Exports], [F/N-F/N|Tab], [F/N|E]) :-
	'$simple_conversion'(Exports, Tab, E).
'$simple_conversion'([F//N|Exports], [F/N2-F/N2|Tab], [F/N2|E]) :-
	N2 is N+1,
	'$simple_conversion'(Exports, Tab, E).
'$simple_conversion'([F/N as NF|Exports], [F/N-NF/N|Tab], [NF/N|E]) :-
	'$simple_conversion'(Exports, Tab, E).
'$simple_conversion'([F//N as BF|Exports], [F/N2-NF/N2|Tab], [NF/N2|E]) :-
	N2 is N+1,
	'$simple_conversion'(Exports, Tab, E).
'$simple_conversion'([op(Prio,Assoc,Name)|Exports], [op(Prio,Assoc,Name)|Tab], [op(Prio,Assoc,Name)|E]) :-
	'$simple_conversion'(Exports, Tab, E).

'$clean_conversion'([], _, _, _, [], [], _).
'$clean_conversion'([(N1/A1 as N2)|Ps], List, Module, ContextModule, [N1/A1-N2/A1|Tab], [N2/A1|MyExports], Goal) :- !,
	( lists:memberchk(N1/A1, List)
	->
	  true
	;
	  '$bad_export'((N1/A1 as A2), Module, ContextModule)
	),
	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).	
'$clean_conversion'([N1/A1|Ps], List, Module, ContextModule, [N1/A1-N1/A1|Tab], [N1/A1|MyExports], Goal) :- !,
	(
	 lists:memberchk(N1/A1, List)
	->
	   true
	;
	  '$bad_export'(N1/A1, Module, ContextModule)
	),
	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).
'$clean_conversion'([N1//A1|Ps], List, Module, ContextModule, [N1/A2-N1/A2|Tab], [N1/A2|MyExports], Goal) :- !,
	A2 is A1+2,
	(
	  lists:memberchk(N1/A2, List)
	->
	  true
	;
	  '$bad_export'(N1//A1, Module, ContextModule)

	),
	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).
'$clean_conversion'([N1//A1 as N2|Ps], List, Module, ContextModule, [N2/A2-N1/A2|Tab], [N2/A2|MyExports], Goal) :- !,
	A2 is A1+2,
	(
	  lists:memberchk(N2/A2, List)
	->
	  true
	;
	  '$bad_export'((N1//A1 as A2), Module, ContextModule)
	),
	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).
'$clean_conversion'([op(Prio,Assoc,Name)|Ps], List, Module, ContextModule, [op(Prio,Assoc,Name)|Tab], [op(Prio,Assoc,Name)|MyExports], Goal) :- !,
	(
	 lists:memberchk(op(Prio,Assoc,Name), List)
	->
	 true
	;
	 '$bad_export'(op(Prio,Assoc,Name), Module, ContextModule)
	),
	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).
'$clean_conversion'([P|_], _List, _, _, _, _, Goal) :-
	'$do_error'(domain_error(module_export_predicates,P), Goal).

'$bad_export'(_, _Module, _ContextModule) :- !.
'$bad_export'(Name/Arity, Module, ContextModule) :-
	functor(P, Name, Arity),
	predicate_property(Module:P, _), !,
	print_message(warning, declaration(Name/Arity, Module, ContextModule, private)).
'$bad_export'(Name//Arity, Module, ContextModule) :-
	Arity2 is Arity+2,
	functor(P, Name, Arity2),
	predicate_property(Module:P, _), !,
	print_message(warning, declaration(Name/Arity, Module, ContextModule, private)).
'$bad_export'(Indicator, Module, ContextModule) :- !,
	print_message(warning, declaration( Indicator, Module, ContextModule, undefined)).

'$neg_conversion'([], Exports, _, _, Exports, _).
'$neg_conversion'([N1/A1|Ps], List, Module, ContextModule, MyExports, Goal) :- !,
	(
	 lists:delete(List, N1/A1, RList)
	->
	 '$neg_conversion'(Ps, RList, Module, ContextModule, MyExports, Goal)
	;
	 '$bad_export'(N1/A1, Module, ContextModule)
	).
'$neg_conversion'([N1//A1|Ps], List, Module, ContextModule, MyExports, Goal) :- !,
	A2 is A1+2,
	(
	 lists:delete(List, N1/A2, RList)
	->
	 '$neg_conversion'(Ps, RList, Module, ContextModule, MyExports, Goal)
	;
	 '$bad_export'(N1//A1, Module, ContextModule)
	).
'$neg_conversion'([op(Prio,Assoc,Name)|Ps], List, Module, ContextModule, MyExports, Goal) :- !,
	(
	 lists:delete(List, op(Prio,Assoc,Name), RList)
	->
	 '$neg_conversion'(Ps, RList, Module, ContextModule, MyExports, Goal)
	;
	 '$bad_export'(op(Prio,Assoc,Name), Module, ContextModule)
	).
'$clean_conversion'([P|_], _List, _, _, _, Goal) :-
	'$do_error'(domain_error(module_export_predicates,P), Goal).


'$add_to_imports'([], _, _).
% no need to import from the actual module
'$add_to_imports'([T|Tab], Module, ContextModule) :- 
	'$do_import'(T, Module, ContextModule),
	'$add_to_imports'(Tab, Module, ContextModule).

'$do_import'(op(Prio,Assoc,Name), _Mod, ContextMod) :-
	op(Prio,Assoc,ContextMod:Name).
'$do_import'(N0/K0-N0/K0, Mod, Mod) :- !.
'$do_import'(_N/K-N1/K, _Mod, ContextMod) :-
       recorded('$module','$module'(_F, ContextMod, MyExports,_),_),
       once(lists:member(N1/K, MyExports)),
       functor(S, N1, K),
       %  reexport predicates if they are undefined in the current module.
       \+ '$undefined'(S,ContextMod), !.
'$do_import'( N/K-N1/K, Mod, ContextMod) :-
	functor(G,N,K),
	'$follow_import_chain'(Mod,G,M0,G0),
	G0=..[N0|Args],
	G1=..[N1|Args],
	( '$check_import'(M0,ContextMod,N1,K) ->
	  ( ContextMod = user ->
	    ( recordzifnot('$import','$import'(M0,user,G0,G1,N1,K),_) -> true ; true)
	  ;
	    ( recordaifnot('$import','$import'(M0,ContextMod,G0,G1,N1,K),_) -> true ; true )
	  )
	;
	  true
	).

'$follow_import_chain'(M,G,M0,G0) :-
	recorded('$import','$import'(M1,M,G1,G,_,_),_), M \= M1, !,
	'$follow_import_chain'(M1,G1,M0,G0).
'$follow_import_chain'(M,G,M,G).

% trying to import Mod:N/K into ContextM
'$check_import'(Mod, ContextM, N, K) :-
	recorded('$import','$import'(MI, ContextM, _, _, N,K),_R),
	% dereference MI to M1, in order to find who 
	% is actually generating
	( '$module_produced by'(M1, MI,  N, K) -> true ; MI = M1 ),
	( '$module_produced by'(M2, Mod, N, K) -> true ; Mod = M2 ),
	M2 \= M1,  !,
	b_getval('$lf_status', TOpts),
	'$lf_opt'(redefine_module, TOpts, Action),
	'$redefine_action'(Action, M1, M2, M, ContextM, N/K).
'$check_import'(_,_,_,_).

'$redefine_action'(ask, M1, M2, M, _, N/K) :-
	stream_property(user_input,tty(true)), !,
	format(user_error,'NAME CLASH: ~w was already imported to module ~w;~n',[M1:N/K,M2]),
	format(user_error,'            Do you want to import it from ~w ? [y, n, e or h] ',M),
	'$mod_scan'(C),
	( C == e -> halt(1) ;
	  C == y ).  
'$redefine_action'(true, M1, _, _, _, _) :- !,
	recorded('$module','$module'(F, M1, _MyExports,_Line),_),
	unload_file(F).
'$redefine_action'(false, M1, M2, M, ContextM, N/K) :-
	recorded('$module','$module'(F, ContextM, _MyExports,_Line),_),
	'$current_module'(_, M2),
	'$do_error'(permission_error(import,M1:N/K,redefined,M2),F).

'$mod_scan'(C) :-
	get_char(C),
	'$skipeol'(C),
	(C == y -> true; C == n).

% I assume the clause has been processed, so the
% var case is long gone! Yes :)
'$clean_cuts'(G,(yap_hacks:current_choicepoint(DCP),NG)) :-
	'$conj_has_cuts'(G,DCP,NG,OK), OK == ok, !.
'$clean_cuts'(G,G).

'$conj_has_cuts'(V,_,V, _) :- var(V), !.
'$conj_has_cuts'(!,DCP,'$$cut_by'(DCP), ok) :- !. 
'$conj_has_cuts'((G1,G2),DCP,(NG1,NG2), OK) :- !,
	'$conj_has_cuts'(G1, DCP, NG1, OK),
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1;G2),DCP,(NG1;NG2), OK) :- !,
	'$conj_has_cuts'(G1, DCP, NG1, OK),
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1->G2),DCP,(G1;NG2), OK) :- !,
	% G1: the system must have done it already
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1*->G2),DCP,(G1;NG2), OK) :- !,
	% G1: the system must have done it already
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'(if(G1,G2,G3),DCP,if(G1,NG2,NG3), OK) :- !,
	% G1: the system must have done it already
	'$conj_has_cuts'(G2, DCP, NG2, OK),
	'$conj_has_cuts'(G3, DCP, NG3, OK).
'$conj_has_cuts'(G,_,G, _).

/**
    set_base_module( +ExportingModule ) is det
All exported predicates from _ExportingModule_ are automatically available to the 
current source  module.

This built-in was introduced by SWI-Prolog. In YAP, by default, modules only
inherit from `prolog`. This extension allows predicates in the current
module (see module/2 and module/1) to inherit from `user` or other modules.

*/
set_base_module(ExportingModule) :-
	var(ExportingModule),
	'$do_error'(instantiation_error,set_base_module(ExportingModule)).
set_base_module(ExportingModule) :-
	atom(ExportingModule), !,
	'$current_module'(Mod),
	retractall(prolg:'$parent_module'(Mod,_)),
	asserta(prolog:'$parent_module'(Mod,ExportingModule)).
set_base_module(ExportingModule) :-
	'$do_error'(type_error(atom,ExportingModule),set_base_module(ExportingModule)).

/**
    import_module( +ImportingModule, +ExportingModule ) is det
All exported predicates from _ExportingModule_ are automatically available to the 
 source  module _ImportModule_.

This innovation was introduced by SWI-Prolog. By default, modules only
inherit from `prolog`. This extension allows predicates in any module
to inherit from `user`oe other modules.

*/
import_module(Mod, ImportModule) :-
	var(Mod),
	'$do_error'(instantiation_error,import_module(Mod, ImportModule)).
import_module(Mod, ImportModule) :-
	atom(Mod), !,
	prolog:'$parent_module'(Mod,ImportModule).
import_module(Mod, EM) :-
	'$do_error'(type_error(atom,Mod),import_module(Mod, EM)).

add_import_module(Mod, ImportModule, Pos) :-
	var(Mod),
	'$do_error'(instantiation_error,add_import_module(Mod, ImportModule, Pos)).
add_import_module(Mod, ImportModule, Pos) :-
	var(Pos),
	'$do_error'(instantiation_error,add_import_module(Mod, ImportModule, Pos)).
add_import_module(Mod, ImportModule, start) :-
	atom(Mod), !,
	retractall(prolog:'$parent_module'(Mod,ImportModule)),
	asserta(prolog:'$parent_module'(Mod,ImportModule)).
add_import_module(Mod, ImportModule, end) :-
	atom(Mod), !,
	retractall(prolog:'$parent_module'(Mod,ImportModule)),
	assertz(prolog:'$parent_module'(Mod,ImportModule)).
add_import_module(Mod, ImportModule, Pos) :-
	\+ atom(Mod), !,
	'$do_error'(type_error(atom,Mod),add_import_module(Mod, ImportModule, Pos)).
add_import_module(Mod, ImportModule, Pos) :-
	'$do_error'(domain_error(start_end,Pos),add_import_module(Mod, ImportModule, Pos)).

delete_import_module(Mod, ImportModule) :-
	var(Mod),
	'$do_error'(instantiation_error,delete_import_module(Mod, ImportModule)).
delete_import_module(Mod, ImportModule) :-
	var(ImportModule),
	'$do_error'(instantiation_error,delete_import_module(Mod, ImportModule)).
delete_import_module(Mod, ImportModule) :-
	atom(Mod),
	atom(ImportModule), !,
	retractall(prolog:'$parent_module'(Mod,ImportModule)).
delete_import_module(Mod, ImportModule) :-
	\+ atom(Mod), !,
	'$do_error'(type_error(atom,Mod),delete_import_module(Mod, ImportModule)).
delete_import_module(Mod, ImportModule) :-
	'$do_error'(type_error(atom,ImportModule),delete_import_module(Mod, ImportModule)).

'$set_source_module'(Source0, SourceF) :-
	prolog_load_context(module, Source0), !,
	module(SourceF).
'$set_source_module'(Source0, SourceF) :-
	current_module(Source0, SourceF).

module_property(Mod, class(L)) :-
	'$module_class'(Mod, L).
module_property(Mod, line_count(L)) :-
	recorded('$module','$module'(_F,Mod,_,L),_).
module_property(Mod, file(F)) :-
	recorded('$module','$module'(F,Mod,_,_),_).
module_property(Mod, exports(Es)) :-
	recorded('$module','$module'(_,Mod,Es,_),_).

'$module_class'(Mod, system) :- '$system_module'( Mod ).
'$module_class'(Mod, library) :- '$library_module'( Mod ).
'$module_class'(Mod, user) :- '$user_module'( Mod ).
'$module_class'(_, temporary) :- fail.
'$module_class'(_, test) :- fail.
'$module_class'(_, development) :- fail.

'$library_module'(M1) :-
	recorded('$module','$module'(F, M1, _MyExports,_Line),_),
	user:library_directory(D),
	sub_atom(F, 0, _, _, D).

'$user_module'( Mod ) :- 
    \+ '$library_module'( Mod),
    \+ '$system_module'( Mod).

ls_imports :-
	recorded('$import','$import'(M0,M,G0,G,_N,_K),_R),
	numbervars(G0+G, 0, _),
	format('~a:~w <- ~a:~w~n', [M, G, M0, G0]),
	fail.
ls_imports.


'$system_module'('$swi').
'$system_module'('$win_menu').
'$system_module'('prolog').
'$system_module'('system').
'$system_module'('$attributes').

/**

@}

@}

**/
