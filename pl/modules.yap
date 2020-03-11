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
@file modules.yap
*/
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
						'$imported_predicate'/2,
						'$meta_expand'/6,
						'$meta_predicate'/2,
						'$meta_predicate'/4,
						'$module'/3,
						'$module'/4,
						'$module_expansion'/6,
						'$module_transparent'/2,
						'$module_transparent'/4]).

/**
  * @defgroup ModuleBuiltins Module Support
  * @ingroup YAPModules
  * @{
  *
  * @brief module loading directives.
  *
  *
  */


:- use_system_module( '$_arith', ['$c_built_in'/3]).

:- use_system_module( '$_consult', ['$lf_opt'/3,
				    '$load_files'/3]).

:- use_system_module( '$_debug', ['$skipeol'/1]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_eval', ['$full_clause_optimisation'/4]).

:- multifile '$system_module'/1.


:- '$purge_clauses'(module(_,_), prolog).
:- '$purge_clauses'('$module'(_,_), prolog).
:- '$purge_clauses'(use_module(_), prolog).
:- '$purge_clauses'(use_module(_,_), prolog).
%
% start using default definition of module.
%

/**
   @pred use_module( +Files ) is directive


   @brief load a module file

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

vvb(2).
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


**/
use_module(F) :- load_files(F,
			    [if(not_loaded),must_be_module(true)]).


/**
  \pred  use_module(+Files, +Imports)
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

**/
use_module(F,Is) :-
    load_files(F, [if(not_loaded),must_be_module(true),imports(Is)]).

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
    recorda('$system_initialization', source_mode(New,Old), _).


'$extend_exports'(HostF, Exports, DonorF ) :-
    ( recorded('$module','$module'( DonorF, DonorM, _,DonorExports, _),_) -> true ; DonorF = user_input ),
    ( recorded('$module','$module'( HostF, HostM, SourceF, _, _),_) -> true ; HostF = user_input ),
    recorded('$module','$module'(HostF, HostM, _, AllExports, _Line), R), erase(R),
    '$convert_for_export'(Exports, DonorExports, DonorM, HostM, _TranslationTab, AllReExports, reexport(DonorF, Exports)),
    lists:append( AllReExports, AllExports, Everything0 ),
    '$sort'( Everything0, Everything ),
    ( source_location(_, Line) -> true ; Line = 0 ),
    recorda('$module','$module'(HostF,HostM,SourceF, Everything, Line),_).

'$module_produced by'(M, M0, N, K) :-
    recorded('$import','$import'(M,M0,_,_,N,K),_), !.
'$module_produced by'(M, M0, N, K) :-
    recorded('$import','$import'(MI,M0,G1,_,N,K),_),
    functor(G1, N1, K1),
    '$module_produced by'(M,MI,N1,K1).

%
% check if current module redefines an imported predicate.
% and remove import.
%
'$not_imported'(H, Mod) :-
    recorded('$import','$import'(NM,Mod,NH,H,_,_),R),
    NM \= Mod,
    functor(NH,N,Ar),
    print_message(warning,redefine_imported(Mod,NM,N/Ar)),
    erase(R),
    fail.
'$not_imported'(_, _).



 /** @pred current_module( ? Mod:atom) is nondet


Succeeds if  _M_ is a user-visible modules. A module is defined as soon as some
predicate defined in the module is loaded, as soon as a goal in the
module is called, or as soon as it becomes the current type-in module.


*/
current_module(Mod) :-
    '$all_current_modules'(Mod),
    \+ '$hidden_atom'(Mod).

/** @pred current_module( ? Mod:atom, ? _F_ : file ) is nondet

Succeeds if  _M_ is a module associated with the file  _F_, that is, if _File_ is the source for _M_. If _M_ is not declared in a file, _F_ unifies with `user`.
 */
current_module(Mod,TFN) :-
    ( atom(Mod) -> true ; '$all_current_modules'(Mod) ),
    ( recorded('$module','$module'(TFN,Mod,_,_Publics, _),_) -> true ; TFN = user ).



% prevent modules within the kernel module...
/** @pred use_module(? _M_,? _F_,+ _L_) is directive
    SICStus compatible way of using a module

If module _M_ is instantiated, import the procedures in _L_ to the
current module. Otherwise, operate as use_module/2, and load the files
specified by _F_, importing the predicates specified in the list _L_.
*/

use_module(M,F,Is) :-
    '$yap_strip_module'(F,M1,F1),
    var(F1),
    !,
    ignore(M=M1),
    '$use_module'(M,M1,Is).
use_module(_M,F,Is) :-
    use_module(F,Is).


'$use_module'(M, M1, Is) :-
	      recorded('$module','$module'(F,M,_,_,_),_),
	      !,
	  load_files(M1:F, [if(not_loaded),must_be_module(true),imports(Is)]).
'$use_module'(M, M1, Is) :-
nonvar(M),
	  !,
	  load_files(M1:M, [if(not_loaded),must_be_module(true),imports(Is)]).
'$use_module'(M, F, Is) :-
'$do_error'(error(instantiation_error, use_module(M,F,Is))).


/** @pred current_module( ? Mod:atom, ? _F_ : file ) is nondet

Succeeds if  _M_ is a system module
 */
system_module(Mod) :-
    ( atom(Mod) -> true ; '$all_current_modules'(Mod) ),
    '$is_system_module'(Mod).

'$trace_module'(X) :-
    open('P0:debug', append, S),
    fornat(S, '~w~n', [X]),
    close(S).
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



/*
@pred '$declare_module'(


be associated to a new file.

\param[in]	_Module_ is the name of the module to declare
\param[in]	_MSuper_ is the name of the context module. Use `prolog`or `system`
                if you do not need a context.
\param[in]	_File_ is the canonical name of the file from which the modulvvvvve is loaded
\param[in]  Line is the line-number of the :- module/2 directive.
\param[in]	 If _Redefine_ `true`, allow associating the module to a new file
*/
/*
'$declare_module'(Name, _Super, Context, _File, _Line) :-
    add_import_module(Name, Context, start).
*/

/**
 @pred abolish_module( + Mod) is det
 get rid of a module and of all predicates included in the module.
*/
abolish_module(Mod) :-
    recorded('$module','$module'(_,Mod,_,_,_),R), erase(R),
    fail.
abolish_module(Mod) :-
    recorded('$import','$import'(Mod,_,_,_,_,_),R), erase(R),
    fail.
abolish_module(Mod) :-
    '$current_predicate'(Na,Mod,S,_),
    functor(S, Na, Ar),
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
    (	recorded('$module','$module'(File,Mod,SourceF,ExportedPreds,Line),R) ->
	erase(R),
	recorda('$module','$module'(File,Mod,SourceF,[P|ExportedPreds],Line),_)
    ;	prolog_load_context(file, File) ->
	recorda('$module','$module'(File,Mod,SourceF,[P],Line),_)
    ;	recorda('$module','$module'(user_input,Mod,user_input,[P],1),_)
    ).
export_resource(P0) :-
    P0 = F//N, atom(F), number(N), N >= 0, !,
    N1 is N+2, P = F/N1,
    '$current_module'(Mod),
    (	recorded('$module','$module'(File,Mod,SourceF,ExportedPreds,Line),R) ->
	erase(R),
	recorda('$module','$module'(File,Mod,SourceF,[P|ExportedPreds],Line ),_)
    ;	prolog_load_context(file, File) ->
	recorda('$module','$module'(File,Mod,SourceF,[P],Line),_)
    ;	recorda('$module','$module'(user_input,Mod,user_input,[P],1),_)
    ).
export_resource(op(Prio,Assoc,Name)) :- !,
    '$current_module'(Mod),
    op(Prio,Assoc,Mod:Name),
    (	recorded('$module','$module'(File,Mod,SourceF,ExportedPreds,Line),R) ->
	erase(R),
	recorda('$module','$module'(File,Mod,SourceF,[op(Prio,Assoc,Name)|ExportedPreds],Line ),_)
    ;	prolog_load_context(file, File) ->
	recorda('$module','$module'(File,Mod,SourceF,[op(Prio,Assoc,Name)],Line),_)
    ;	recorda('$module','$module'(user_input,Mod,user_input,[op(Prio,Assoc,Name)],1),_)
    ).
export_resource(Resource) :-
    '$do_error'(type_error(predicate_indicator,Resource),export(Resource)).

export_list(Module, List) :-
    recorded('$module','$module'(_,Module,_,List,_),_).


'$add_to_imports'([], _, _).
% no need to import from the actual module
'$add_to_imports'([T|Tab], Module, ContextModule) :-
    '$do_import'(T, Module, ContextModule),
    '$add_to_imports'(Tab, Module, ContextModule).

%'$do_import'(K, _, _) :- writeln(K), fail.
'$do_import'(op(Prio,Assoc,Name), Mod, ContextMod) :-
    op(Prio,Assoc,Mod:Name),
    op(Prio,Assoc,ContextMod:Name),
    !.
'$do_import'(N0/K0-N0/K0, Mod, Mod) :- !.
'$do_import'(N0/K0-N0/K0, _Mod, prolog) :- !.
% '$do_import'(_N/K-N1/K, _Mod, ContextMod) :-
%        recorded('$module','$module'(_F, ContextMod, _SourceF, MyExports,_),_),
%        once(lists:member(N1/K, MyExports)),
%        functor(S, N1, K),
%        %  reexport predicates if they are undefined in the current module.
%        \+ '$undefined'(S,ContextMod), !.
'$do_import'( N0/K-N1/K, M0, ContextMod) :-
    %'$one_predicate_definition'(Mod:G,M0:G0),
    %	M0\=prolog,
    (M0==ContextMod->N0\=N1;true),
    functor(G1,N1,K),
    (N0 == N1
    ->
	G0=G1
    ;
    G1=..[N1|Args],
    G0=..[N0|Args]
    ),
    %writeln((ContextMod:G1:-M0:G0)),
    recordaifnot('$import','$import'(M0,ContextMod,G0,G1,N1,K),_),
    !.
'$do_import'( _,_,_ ).


'$follow_import_chain'(M,G,M0,G0) :-
    recorded('$import','$import'(M1,M,G1,G,_,_),_), M \= M1, !,
    '$follow_import_chain'(M1,G1,M0,G0).
'$follow_import_chain'(M,G,M,G).

% trying to import Mod:N/K into ContextM
'$check_import'(Mod, ContextM, N, K) :-
    recorded('$import','$import'(MI, ContextM, _, _, N,K),_R),
    % dereference MI to M1, in order to find who
    % is actually generating
    ( '$module_produced by'(M1, MI,  N, K) -> true ; MI = M1 	),
    ( '$module_produced by'(M2, Mod, N, K) -> true ; Mod = M2 ),
    M2 \= M1,  !,
    '$redefine_import'( M1, M2, Mod, ContextM, N/K).
'$check_import'(_,_,_,_).

'$redefine_import'( M1, M2, Mod, ContextM, N/K) :-
    '__NB_getval__'('$lf_status', TOpts, fail),
    '$lf_opt'(redefine_module, TOpts, Action), !,
    '$redefine_action'(Action, M1, M2, Mod, ContextM, N/K).
'$redefine_import'( M1, M2, Mod, ContextM, N/K) :-
    '$redefine_action'(false, M1, M2, Mod, ContextM, N/K).


'$redefine_action'(ask, M1, M2, M, _, N/K) :-
    stream_property(user_input,tty(true)), !,
    format(user_error,'NAME CLASH: ~w was already imported to module ~w;~n',[M1:N/K,M2]),
    format(user_error,'            Do you want to import it from ~w ? [y, n, e or h] ',M),
    '$mod_scan'(C),
    ( C == e -> halt(1) ;
      C == y ).
'$redefine_action'(true, M1, _, _, _, _) :- !,
    recorded('$module','$module'(F, M1, _, _MyExports,_Line),_),
    unload_file(F).
'$redefine_action'(false, M1, M2, _M, ContextM, N/K) :-
    recorded('$module','$module'(F, ContextM, _, _MyExports,_Line),_),
    '$current_module'(_, M2),
    '$do_error'(permission_error(import,M1:N/K,redefined,M2),F).

'$mod_scan'(C) :-
    get_char(C),
    '$skipeol'(C),
    (C == y -> true; C == n).

/**
  @pred set_base_module( +ExportingModule ) is det
  @brief All
predicates exported from _ExportingModule_ are automatically available to the
other source modules.

This built-in was introduced by SWI-Prolog. In YAP, by default, modules only
inherit from `prolog`. This extension allows predicates in the current
module (see module/2 and module/1) to inherit from `user` or other modules.
  x2
*/
set_base_module(ExportingModule) :-
    var(ExportingModule),
    '$do_error'(instantiation_error,set_base_module(ExportingModule)).
set_base_module(ExportingModule) :-
    atom(ExportingModule), !,
    '$current_module'(Mod),
    retractall(prolog:'$parent_module'(Mod,_)),
    asserta(prolog:'$parent_module'(Mod,ExportingModule)).
set_base_module(ExportingModule) :-
    '$do_error'(type_error(atom,ExportingModule),set_base_module(ExportingModule)).

/**
 *  @pred  import_module( +ImportingModule, +ExportingModule ) is det
 *  All exported predicates from _ExportingModule_
 * are automatically available to the
 * source  module _ImportModule_.

This innovation was introduced by SWI-Prolog. By default, modules only
inherit from `prolog` and `user`. This extension allows predicates in
any module to inherit from `user`  and other modules.

*/
import_module(Mod, ImportModule) :-
    var(Mod),
    '$do_error'(instantiation_error,import_module(Mod, ImportModule)).
import_module(Mod, ImportModule) :-
    atom(Mod), !,
    prolog:'$parent_module'(Mod,ImportModule).
import_module(Mod, EM) :-
    '$do_error'(type_error(atom,Mod),import_module(Mod, EM)).


/**
  @pred add_import_module( + _Module_, + _ImportModule_ , +_Pos_) is det
Add all exports in _ImportModule_ as available to _Module_.


All exported predicates from _ExportModule_ are made available to the
 source  module _ImportModule_. If _Position_ is bound to `start` the
 module _ImportModule_ is tried first, if _Position_ is bound to `end`,
 the module is consulted last.

*/
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

/**
  @pred delete_import_module( + _ExportModule_, + _ImportModule_ ) is det
Exports in _ImportModule_ are no longer available to _Module_.


All exported predicates from _ExportModule_ are discarded from the
 ones used vy the source  module _ImportModule_.

*/
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

/**
  @pred module_property( +Module, ? _Property_ ) is nondet

  Enumerate non-deterministically the main properties of _Module_ .

  Reports the following properties of _Module_:

  + `class`( ?_Class_ ): whether it is a `system`, `library`, or `user` module.

  + `line_count`(?_Ls_): number of lines in source file (if there is one).

  + `file`(?_F_): source file for _Module_ (if there is one).

  + `exports`(-Es): list of all predicate symbols and
   operator symbols exported or re-exported by this module.

*/
module_property(Mod, Prop) :-
    var(Mod),
    !,
    recorded('$module','$module'(_,Mod,_,_Es,_),_),
    module_property(Mod, Prop).
module_property(Mod, class(L)) :-
    '$module_class'(Mod, L).
module_property(Mod, line_count(L)) :-
    recorded('$module','$module'(_F,Mod,_,_,L),_).
module_property(Mod, file(F)) :-
    recorded('$module','$module'(F,Mod,_,_,_),_).
module_property(Mod, exports(Es)) :-
    (
        recorded('$module','$module'(_,Mod,_,Es,_),_)
    ->
    true
    ;
    Mod==user
    ->
    findall( P, (current_predicate(user:P)), Es)
    ;
    Mod==prolog
    ->
    findall( N/A, (predicate_property(Mod:P0, public),functor(P0,N,A)), Es)
    ).

'$module_class'( Mod, system) :- '$is_system_module'( Mod ), !.
'$module_class'( Mod, library) :- '$library_module'( Mod ), !.
'$module_class'(_Mod, user) :- !.
'$module_class'(   _, temporary) :- fail.
'$module_class'(   _, test) :- fail.
'$module_class'(   _, development) :- fail.

'$library_module'(M1) :-
    recorded('$module','$module'(_, M1, library(_), _MyExports,_Line),_).

ls_imports :-
    recorded('$import','$import'(M0,M,G0,G,_N,_K),_R),
    numbervars(G0+G, 0, _),
    format('~a:~w <- ~a:~w~n', [M, G, M0, G0]),
    fail.
ls_imports.

unload_module(Mod) :-
    clause( '$meta_predicate'(_F,Mod,_N,_P), _, R),
    erase(R),
    fail.
unload_module(Mod) :-
    recorded('$multifile_defs','$defined'(_FileName,_Name,_Arity,Mod), R),
    erase(R),
    fail.
unload_module(Mod) :-
    recorded( '$foreign', Mod:_Foreign, R),
    erase(R),
    fail.
% remove imported modules
unload_module(Mod) :-
    setof( M, recorded('$import',_G0^_G^_N^_K^_R^'$import'(Mod,M,_G0,_G,_N,_K),_R), Ms),
    recorded('$module','$module'( _, Mod, _, _, Exports), _),
    lists:member(M, Ms),
    current_op(X, Y, M:Op),
    lists:member( op(X, Y, Op), Exports ),
    op(X, 0, M:Op),
    fail.
unload_module(Mod) :-
    recorded('$module','$module'( _, Mod, _, _, Exports), _),
    lists:member( op(X, _Y, Op), Exports ),
    op(X, 0, Mod:Op),
    fail.
unload_module(Mod) :-
    current_predicate(Mod:P),
    abolish(P),
    fail.
unload_module(Mod) :-
    recorded('$import','$import'(Mod,_M,_G0,_G,_N,_K),R),
    erase(R),
    fail.
unload_module(Mod) :-
    recorded('$module','$module'( _, Mod, _, _, _), R),
    erase(R),
    fail.

/*  debug */
module_state :-
    recorded('$module','$module'(HostF,HostM,SourceF, Everything, Line),_),
    format('HostF ~a, HostM ~a, SourceF ~w, Line ~d,~n     Everything ~w.~n', [HostF,HostM,SourceF, Line, Everything]),
    recorded('$import','$import'(HostM,M,G0,G,_N,_K),_R),
    format('   ~w:~w :- ~w:~w.~n',[M,G,HostM,G0]),
    fail.
module_state.

%% @}

