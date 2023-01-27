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

  @defgroup ModuleBuiltins Module Support

  @ingroup YAPModules
  @{

  **/
:- system_module_( '$_modules', [abolish_module/1,
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
						'$convert_for_export'/6,
						'$do_import'/3,
						'$extend_exports'/3,
						'$get_undefined_pred'/4,
						'$imported_predicate'/4,
						'$meta_expand'/6,
						'$meta_predicate'/2,
						'$meta_predicate'/4,
						'$module'/3,
						'$module'/4,
						'$module_expansion'/6,
						'$module_transparent'/2,
						'$module_transparent'/4]).



:- use_system_module( '$_arith', ['$c_built_in'/3]).

:- use_system_module( '$_consult', ['$lf_opt'/3,
				    '$load_files'/3]).

:- use_system_module( '$_debug', ['$skipeol'/1]).

:- use_system_module( '$_errors', [throw_error/2]).

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

```
use_module(F) :-
	load_files(F, [if(not_loaded),must_be_module(true)]).
```

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

```
:- module( a, [a/1] ).

:- use_module(b).

a(1).
a(X) :- b(X).
```

and the file `b.pl` contains:

```
:- module( b, [a/1,b/1] ).

a(2).

b(1).
```

YAP will execute as follows:


```
?- [a].
 % consulting .../a.pl...
  % consulting .../b.pl...
  % consulted .../b.pl in module b, 0 msec 0 bytes
 % consulted .../a.pl in module a, 1 msec 0 bytes
true.
 ?- a(X).
X = 1 ? ;
X = 1.
```

The example shows that the query `a(X)`has a single answer, the one
defined in `a.pl`. Calls to `a(X)`succeed in the top-level, because
the module `a` was loaded into `user`. On the other hand, `b(X)`is not
exported by `a.pl`, and is not available to calls, although it can be
accessed as a predicate in the module 'a' by using the `:` operator.

Next, consider the three files `c.pl`, `d1.pl`, and `d2.pl`:

```
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

```

The result is as follows:

```
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
```

The state of  the module system after this error is undefined.


**/
use_module(F) :- load_files(F,[if(not_loaded),must_be_module(true)]).



/**
  \pred  use_module(+Files, +Imports)
  loads a module file but only imports the named predicates


This predicate loads the file specified by _Files_, importing their
public predicates specified by _Imports_ into the current type-in
module. It is implemented as if by:

```
use_module(Files, Imports) :-
	load_files(Files, [if(not_loaded),must_be_module(true),imports(Imports)]).
```

The _Imports_ argument may be use to specify which predicates one
wants to load. It can also be used to give the predicates a different name. As an example,
the graphs library is implemented on top of the red-black trees library, and some predicates are just aliases:

```
:- use_module(library(rbtrees), [
	rb_min/3 as min_assoc,
	rb_max/3 as max_assoc,

        ...]).
```

Unfortunately it is still not possible to change argument order.

**/
use_module(F,Is) :-
    load_files(F, [if(not_loaded),must_be_module(true),imports(Is)] ).


'$process_hidden_module'(TNew,M) :-
    '$convert_true_off_mod3'(TNew, New, M),
    source_mode(Old, New),
    '$prepare_restore_hidden'(Old,New).

'$convert_true_off_mod3'(true, off, _) :- !.
'$convert_true_off_mod3'(false, on, _) :- !.
'$convert_true_off_mod3'(X, _, M) :-
    throw_error(domain_error(module_decl_options,hidden(X)),M).

'$prepare_restore_hidden'(Old,Old) :- !.
'$prepare_restore_hidden'(Old,New) :-
    recorda('$system_initialization', source_mode(New,Old), _).


'$module_produced by'(M, M0, N, K) :-
     '$import'(M,M0,_,_,N,K),
    !.
'$module_produced by'(M, M0, N, K) :-
    '$import'(MI,M0,G1,_,N,K),
    functor(G1, N1, K1),
    '$module_produced by'(M,MI,N1,K1).
% prevent modules within the kernel module...
/** @pred use_module(? _M_,? _F_,+ _L_) 

    SICStus directive for loading a module, it can operate in two 
different ways:

1. If the module _M_ is given, import the procedures in _L_ from _M_
to the current source module.

2. Otherwise, operate as use_module/2, and load the files
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
    '$module'(F,M,_,_),
    !,
    load_files(M1:F, [if(not_loaded),must_be_module(true),imports(Is)]).
'$use_module'(M, M1, Is) :-
    nonvar(M),
    !,
    load_files(M1:M, [if(not_loaded),must_be_module(true),imports(Is)]).
'$use_module'(M, F, Is) :-
    throw_error(error(instantiation_error, use_module(M,F,Is))).


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
    ('$module'(TFN,Mod,_Publics, _) -> true ; TFN = user_input ).

system_module(_Mod,_,_y) :-
    !.
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


/**
be associated to a new file.

\param[in]	_Module_ is the name of the module to declare
\param[in]	_MSuper_ is the name of the context module. Use `prolog`or `system`
                if you do not need a context.
\param[in]	_File_ is the canonical name of the file from which the module is loaded
\param[in]  Line is the line-number of the :- module/2 directive.
\param[in]	 If _Redefine_ `true`, allow associating the module to a new file
*/

/**
 @pred abolish_module( + Mod) is det
 get rid of a module and of all predicates included in the module.
*/
abolish_module(Mod) :-
    retractall('$module'(_,Mod,_,_)),
    fail.
abolish_module(Mod) :-
    retractall('$import'(Mod,_,_,_,_,_)),
    fail.
abolish_module(Mod) :-
    '$current_predicate'(Na,Mod,S,_),
    functor(S, Na, Ar),
    abolish(Mod:Na/Ar),
    fail.


abolish_module(_).

export(Resource) :-
    var(Resource),
    throw_error(instantiation_error,export(Resource)).
export([]) :- !.
export([Resource| Resources]) :- !,
    export_resource(Resource),
    export(Resources).
export(Resource) :-
    export_resource(Resource).

export_resource(Resource) :-
    var(Resource), !,
    throw_error(instantiation_error,export(Resource)).
export_resource(P) :-
    P = F/N, atom(F), number(N), N >= 0, !,
    '$current_module'(Mod),
    (
	clause('$module'(File,Mod,ExportedPreds,Line),_,R)
    ->
    (
	'$member'(P,ExportedPreds)
    ->
    true
    ;
    erase(R),
    asserta('$module'(File,Mod,[P|ExportedPreds],Line))
    )
    ;
    (
	prolog_load_context(file, File)
    ->
    asserta('$module'(File,Mod,[P],Line))
    ;
    asserta('$module'(user_input,Mod,[P],1))
    )
    ).
export_resource(P0) :-
    P0 = F//N, atom(F), number(N), N >= 0, !,
    N1 is N+2, P = F/N1,
    export_resource(P).
export_resource(op(Prio,Assoc,Name)) :-
    '$current_module'(Mod),
    op(Prio,Assoc,Mod:Name),
    
    prolog_load_context(file, File),
    (
	clause('$module'(File,Mod,ExportedPreds,Line),R)
    ->
    (
	'$delete'(ExportedPreds, op(OldPrio, Assoc, Name), Rem)
    ->
    (
	OldPrio == Prio
    ->
    Update = false
    ;
    Update = true
    )
    ;
    Update = true, Rem = ExportedPreds
    ),
    Update == true,
    erase(R)
    ),
    !,
    (
	Update == true
    ->
    	  asserta('$module'(File,Mod,[op(Prio,Assoc,Name)|Rem],Line ))
    ;
	nonvar(File)
    ->
    asserta('$module'(File,Mod,[op(Prio,Assoc,Name)],Line))
    ;
    asserta('$module'(user_input,Mod,[op(Prio,Assoc,Name)],1))
    ).
export_resource(Resource) :-
    throw_error(type_error(predicate_indicator,Resource),export(Resource)).

export_list(Module, List) :-
    '$module'(_,Module,List,_).


'$add_to_imports'([], _, _).
% no need to import from the actual module
'$add_to_imports'([T|_Tab], Module, ContextModule) :-
    '$do_import'(T, Module, ContextModule),
    fail.
'$add_to_imports'([_T|Tab], Module, ContextModule) :-
    '$add_to_imports'(Tab, Module, ContextModule).

'$do_import'(op(Prio,Assoc,Name), Mod, ContextMod) :-
		!,
    op(Prio,Assoc,ContextMod:Name),
    op(Prio,Assoc,Mod:Name),
    fail.
'$do_import'( NDonor/K-NHost/K, MDonor, MHost) :-
    MDonor\=MHost,
    MDonor\=prolog,
    once('$check_import'(MHost,MDonor,NHost,K)),
    functor(GDonor,NDonor,K),
    GDonor=..[NDonor|Args],
    GHost=..[NHost|Args],
    \+ '$import'(_,MHost,_,GHost,_,_),
    asserta('$import'(MDonor,MHost,GDonor,GHost,NHost,K)),
    %writeln((MHost:GHost :- MDonor:GDonor)),
    current_prolog_flag(source, YFlag),
    set_prolog_flag(source, false),
    asserta_static(MHost:(GHost :- MDonor:GDonor)),
    set_prolog_flag(source, YFlag),
    '$mk_proxy_predicate'(GHost,MHost),
    fail.


% trying to import Mod:N/K into ContextM
'$check_import'(prolog, _ContextM, _N, _K) :-
	!,
	fail.
'$check_import'(M, M, _N, _K) :-
	!,
	fail.
'$check_import'(_, _, N, K) :-
	system_predicate(N/K),
	!,
	fail.
'$check_import'(M0, M1, N, K) :-
    '$import'(M2, M1, _, _, N,K),
    !,
    (M2 == M0
    ->
	'$redefine_import'( M2, M1, M0, N/K)
    ;
	'$check_import'(M0, M2, N, K)
    ).
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
    '$module'(F, M1, _MyExports,_Line),
    unload_file(F).
'$redefine_action'(false, M1, M2, _M, ContextM, N/K) :-
    '$module'(F, ContextM, _MyExports,_Line),
    '$current_module'(_, M2),
    throw_error(permission_error(import,M1:N/K,redefined,M2),F).

'$mod_scan'(C) :-
    get_char(C),
    '$skipeol'(C),
    (C == y -> true; C == n).

/**
  @pred set_base_module( +Expor
tingModule ) is det
  @brief All
predicates exported from _ExportingModule_ are automatically available to the
other source modules.

This built-in was introduced by SWI-Prolog. In YAP, by default, modules only
inherit from `prolog`. This extension allows predicates in the current
module (see module/2 and module/1) to inherit from `user` or other modules.

*/
set_base_module(ExportingModule) :-
    var(ExportingModule),
    throw_error(instantiation_error,set_base_module(ExportingModule)).
set_base_module(ExportingModule) :-
    atom(ExportingModule), !,
    '$current_module'(Mod),
    retractall(prolog:'$parent_module'(Mod,_)),
    asserta(prolog:'$parent_module'(Mod,ExportingModule)).
set_base_module(ExportingModule) :-
    throw_error(type_error(atom,ExportingModule),set_base_module(ExportingModule)).

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
    throw_error(instantiation_error,import_module(Mod, ImportModule)).
import_module(Mod, ImportModule) :-
    atom(Mod), !,
    prolog:'$parent_module'(Mod,ImportModule).
import_module(Mod, EM) :-
    throw_error(type_error(atom,Mod),import_module(Mod, EM)).


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
    throw_error(instantiation_error,add_import_module(Mod, ImportModule, Pos)).
add_import_module(Mod, ImportModule, Pos) :-
    var(Pos),
    throw_error(instantiation_error,add_import_module(Mod, ImportModule, Pos)).
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
    throw_error(type_error(atom,Mod),add_import_module(Mod, ImportModule, Pos)).
add_import_module(Mod, ImportModule, Pos) :-
    throw_error(domain_error(start_end,Pos),add_import_module(Mod, ImportModule, Pos)).

/**
  @pred delete_import_module( + _ExportModule_, + _ImportModule_ ) is det
Exports in _ImportModule_ are no longer available to _Module_.


All exported predicates from _ExportModule_ are discarded from the
 ones used vy the source  module _ImportModule_.

*/
delete_import_module(Mod, ImportModule) :-
    var(Mod),
    throw_error(instantiation_error,delete_import_module(Mod, ImportModule)).
delete_import_module(Mod, ImportModule) :-
    var(ImportModule),
    throw_error(instantiation_error,delete_import_module(Mod, ImportModule)).
delete_import_module(Mod, ImportModule) :-
    atom(Mod),
    atom(ImportModule), !,
    retractall(prolog:'$parent_module'(Mod,ImportModule)).
delete_import_module(Mod, ImportModule) :-
    \+ atom(Mod), !,
    throw_error(type_error(atom,Mod),delete_import_module(Mod, ImportModule)).
delete_import_module(Mod, ImportModule) :-
    throw_error(type_error(atom,ImportModule),delete_import_module(Mod, ImportModule)).

'$set_source_module'(Source0, SourceF) :-
    prolog_load_context(module, Source0), !,
    module(SourceF).
'$set_source_module'(Source0, SourceF) :-
    current_module(Source0, SourceF).

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
    '$module'(_,Mod,_Es,_),
    module_property(Mod, Prop).
module_property(Mod, class(L)) :-
    '$module_class'(Mod, L).
module_property(Mod, line_count(L)) :-
    '$module'(_F,Mod,_,L).
module_property(Mod, file(F)) :-
    '$module'(F,Mod,_,_).
module_property(Mod, exports(Es)) :-
    (
        '$module'(_,Mod,Es,_)
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

'$library_module'(_M1) :-
    fail.

ls_imports :-
    '$import'(M0,M,G0,G,_N,_K),
    numbervars(G0+G, 0, _),
    format('~a:~w <- ~a:~w~n', [M, G, M0, G0]),
    fail.
ls_imports.

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
    setof( M, _G0^_G^_N^_K^_R^'$import'(Mod,M,_G0,_G,_N,_K), Ms),
    '$module'( _, Mod, _, Exports),
    '$memberchk'(M, Ms),
    current_op(X, Y, M:Op),
    '$memberchk'( op(X, Y, Op), Exports ),
    op(X, 0, M:Op),
    fail.
unload_module(Mod) :-
    '$module'( _, Mod, _, Exports),
    '$memberchk'( op(X, _Y, Op), Exports ),
    op(X, 0, Mod:Op),
    fail.
unload_module(Mod) :-
    current_predicate(Mod:P),
    abolish(P),
    fail.
unload_module(Mod) :-
    retractall('$import'(Mod,_M,_G0,_G,_N,_K)),
    fail.
unload_module(Mod) :-
    retractall('$module'( _, Mod, _, _)).

/*  debug */
module_state :-
    '$module'(HostF,HostM, Everything, Line),
    \+ system_module(HostM),
    atom(HostM),
    format('%%%%%%~n          ~a,~n%% at ~w,~n%% loaded at ~a:~d,~n%% Exporlist ~w.~nImports~n:', [HostM,HostF, Line, Everything]),
    (
	'$import'(M,HostM,_G,_GO,N,K),                                   
	format('%   ~w:~a/~d:.~n',[M,N,K])
    ;
    format('%%%%%%~nExports~n:', []),
    '$import'(HostM,M,_G,_GO,N,K),
    format('%   ~w:~a/~d:.~n',[M,N,K])
    ),
    fail.
module_state.


:- dynamic( '$module'/4 ).
:- dynamic( '$import'/6 ).

'$module'(user_input,user,[],1).

'$check_module'(File,Mod) :-
    '$module'(File,Mod,ExpL,_Line),
    '$simplify_functors'(ExpL, CallL),
    '$check_module_exports'(CallL,File,Mod),
    '$check_module_undefineds'(File,Mod).

'$simplify_functors'([], []).
'$simplify_functors'([N/A-_|ExpL], [N/A|CallL]) :-
    !,
    '$simplify_functors'(ExpL, CallL).
'$simplify_functors'([_|ExpL], CallL) :- 
    '$simplify_functors'(ExpL, CallL).

'$check_module_exports'([],_,_Mod).
'$check_module_exports'([N/A|Exports],File,Mod) :-
    current_predicate(Mod:N/A),
    !,
    '$check_module_exports'(Exports,File,Mod).
'$check_module_exports'([N/A|Exports],File,Mod) :-
    current_predicate(prolog:N/A),
    !,
    '$check_module_exports'(Exports,File,Mod).
'$check_module_exports'([N/A|Exports],File,Mod) :-
    '$import'(_,Mod,_,_,N,A),
    !,
    '$check_module_exports'(Exports,File,Mod).
'$check_module_exports'([NE/AE|Exports],File,Mod) :-
    print_message(warning, error(compilation_warning(export_undefined,Mod,NE/AE),[parserFile=File,parserLine=1,parserPos=0,errorMsg:`trying to export undefined predicate`,prologConsulting=true ])),
    '$check_module_exports'(Exports,File,Mod).


'$check_module_undefineds'(File,Mod) :-
    '$current_predicate'(_,Mod,P,undefined),
    \+ '$imported_predicate'(Mod:P, _),
    functor(P,NE,AE),
    print_message(warning, error(compilation_warning(undefined_in_module,Mod,NE/AE),[parserFile=File,parserLine=1,
    parserPos=0,errorMsg:`could not find a definition within the module ir its imports`,prologConsulting=true ])),
fail.
'$check_module_undefineds'(_,_Mod).

%% @}



