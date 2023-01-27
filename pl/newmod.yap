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
* File:		newmod.pl						 *
* Last rev:								 *
* mods:									 *
* comments:     support	for new modules.					 *
*									 *
*************************************************************************/
/**

  @file newmod.yap
  @short support for creating a new module.

  @ingroup ModuleBuiltins
  @pred module(+M) is det
   set the type-in module


Defines  _M_ to be the current working or type-in module. All files
which are not bound to a module are assumed to belong to the working
module (also referred to as type-in module). To compile a non-module
file into a module which is not the working one, prefix the file name
with the module name, in the form ` _Module_: _File_`, when
loading the file.

*/
module(N) :-
	var(N),
	throw_error(instantiation_error,module(N)).
module(N) :-
	atom(N), !,
	% set it as current module.
	'$current_module'(_,N).
module(N) :-
	throw_error(type_error(atom,N),module(N)).

/**
 \pred	module(+ Module:atom, +ExportList:list) is directive

  @brief define a new module

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

*/

'$declare_module'(HostM, DonorM, Ps) :-
    source_location(F,Line),
       ('__NB_getval__'( '$user_source_file', F0 , fail)
       ->
           true
       ;
           F0 = F
       ),
    
    '$add_module_on_file'(DonorM, F, HostM, Ps, Line),
    current_source_module(HostM,DonorM).



'$declare_system_module'(HostM,N,Ps,Ss) :-
    '$mk_system_predicates'( Ss ),
    set_module_property(N,type(system)),
    '$declare_module'(HostM,N,Ps).

'$mk_system_predicates'( Ps ) :-
    '$memberchk'(Name/A , Ps),
    '$new_system_predicate'(Name, A, prolog),
    fail.
'$mk_system_predicates'( _Ps ).


/** set_module_property( +Mod, +Prop)

   Set a property for a module. Currently this includes:
     - base module, a module from where we automatically import all definitions, see add_import_module/2.
	 - the export list
	 - module class is currently ignored.
*/
set_module_property(Mod, base(Base)) :-
	must_be_of_type( module, Mod),
	add_import_module(Mod, Base, start).
set_module_property(Mod, exports(Exports)) :-
	must_be_of_type( module, Mod),
	current_source_module(OMod,OMod),
	'$add_module_on_file'(Mod, user_input, OMod, Exports, 1).
set_module_property(Mod, exports(Exports, File, Line)) :-
	current_source_module(OMod,OMod),
	must_be_of_type( module, Mod),
	'$add_module_on_file'(Mod, File, OMod, Exports, Line).
set_module_property(Mod, class(Class)) :-
	must_be_of_type( module, Mod),
	must_be_of_type( atom, Class).

'$add_module_on_file'( DonorM, DonorF, _HostM, Exports, _LineF) :-
    '$module'(OtherF, DonorM, OExports, _),
    % the module has been found, are we reconsulting?
    (
	DonorF \= OtherF
	->
	throw_error(permission_error(module,redefined,DonorM, OtherF, DonorF),module(DonorM,Exports) )
    ;
    OExports == Exports
    ->
    !
    ;
    unload_module(DonorM),
    fail
    ).
'$add_module_on_file'( DonorM, DonorF0, _, Exports, Line) :-
    (DonorM= prolog -> DonorF = user_input ;
     DonorM= user -> DonorF = user_input ;
     DonorF0 = DonorF),
    '$m_normalize'(Exports,DonorM,Tab),
    '$sort'( Tab, AllExports ),
    % last, export to the host.
    asserta('$module'(DonorF,DonorM, AllExports, Line)),
   (
       recorded('$source_file','$source_file'( DonorF, Time), R), erase(R),
       fail
   ;
   recorda('$source_file','$source_file'( DonorF, Time), _) 
	).

'$operators'([],_).
'$operators'([op(A,B,C)|AllExports0], DonorM) :-
    op(A,B,DonorM:C),
    !,
    '$operators'(AllExports0, DonorM).
'$operators'([_|AllExports0], DonorM) :-
    '$operators'(AllExports0, DonorM).

/**

@defgroup ModPreds Module Interface Predicates
@ingroup YAPModules


  @{

**/


'$export_preds'([]).
'$export_preds'([N/A|Decls]) :-
    functor(S, N, A),
    '$sys_export'(S, prolog),
    '$export_preds'(Decls).

/**

  @pred reexport(+F) is directive
  @pred reexport(+F, +Decls ) is directive
  allow a module to use and export predicates from another module

Export all predicates defined in list  _F_ as if they were defined in
the current module.

Export predicates defined in file  _F_ according to  _Decls_. The
declarations should be of the form:

<ul>
    A list of predicate declarations to be exported. Each declaration
may be a predicate indicator or of the form `` _PI_ `as`
 _NewName_'', meaning that the predicate with indicator  _PI_ is
to be exported under name  _NewName_.

    `except`( _List_)
In this case, all predicates not in  _List_ are exported. Moreover,
if ` _PI_ `as`  _NewName_` is found, the predicate with
indicator  _PI_ is to be exported under name  _NewName_ as
before.


Re-exporting predicates must be used with some care. Please, take into
account the following observations:

<ul>
  + The `reexport` declarations must be the first declarations to
  follow the `module` declaration.  </li>

  + It is possible to use both `reexport` and `use_module`, but all
  predicates reexported are automatically available for use in the
  current module.

  + In order to obtain efficient execution, YAP compiles
  dependencies between re-exported predicates. In practice, this means
  that changing a `reexport` declaration and then *just* recompiling
  the file may result in incorrect execution.

*/
'$reexport'(M, _, M ) :-
    !.
'$reexport'(user, _, _M ) :-
    !.
'$reexport'(HostM, AllReExports, _DonorM ) :-
%        writeln(r0:DonorM/HostM),
    ( retract('$module'( HostF, HostM, AllExports, Line)) -> true ; HostF = user_input,AllExports=[] ,Line=1),
    '$append'( AllReExports, AllExports, Everything0 ),
    '$sort'( Everything0, Everything ),
    '$operators'(AllReExports, HostM),
    %    writeln(r:DonorM/HostM),
    asserta('$module'(HostF,HostM, Everything, Line)).



/**
@}
**/

/**
 
This predicate actually exports _Module to the _ContextModule_.
 _Imports is what the ContextModule needed.																			
*/

'$import_module'(DonorM, HostM, F, _Opts) :-
    DonorM ==  HostM,
    !,
    (
	'$source_file_scope'( F, M)
    ->
    true;
	assert('$source_file_scope'( F, M) )
    ).
'$import_module'(DonorM, HostM, File, _Opts) :-
    \+
	'$module'(File, DonorM, _ModExports, _),                                                                 
	% enable loading C-predicates from a different file
	recorded( '$load_foreign_done', [File, DonorM], _),
	'$import_foreign'(File, DonorM, HostM ),
	fail.
'$import_module'(DonorM, HostM,File, Opts) :-
    DonorM \= HostM,
    '$module'(File, DonorM, Exports, _),
    ignore('$member'(imports(Imports),Opts)),
    '$filter'(Imports, Exports, Tab),
    '$add_to_imports'(Tab, DonorM, HostM),
    (     '$memberchk'(reexport(true),Opts)
    ->
    '$reexport'(HostM, Tab, DonorM )
    ;
    true
    ),
    !.

'$operators'([],_).
'$operators'([op(A,B,C)|AllExports0], DonorM) :-
    op(A,B,DonorM:C),
    !,
    '$operators'(AllExports0, DonorM).
'$operators'([_|AllExports0], DonorM) :-
    '$operators'(AllExports0, DonorM).

/**

@defgroup ModPreds Module Interface Predicates
@ingroup YAPModules


  @{

**/


'$export_preds'([]).
'$export_preds'([N/A|Decls]) :-
    functor(S, N, A),
    '$sys_export'(S, prolog),
    '$export_preds'(Decls).

/**

  @pred reexport(+F) is directive
  @pred reexport(+F, +Decls ) is directive
  allow a module to use and export predicates from another module

Export all predicates defined in list  _F_ as if they were defined in
the current module.

Export predicates defined in file  _F_ according to  _Decls_. The
declarations should be of the form:

<ul>
    A list of predicate declarations to be exported. Each declaration
may be a predicate indicator or of the form `` _PI_ `as`
 _NewName_'', meaning that the predicate with indicator  _PI_ is
vto be exported under name  _NewName_.

    `except`( _List_)
In this case, all predicates not in  _List_ are exported. Moreover,
if ` _PI_ `as`  _NewName_` is found, the predicate with
indicator  _PI_ is to be exported under name  _NewName_ as
before.


Re-exporting predicates must be used with some care. Please, take into
account the following observations:

<ul>
  + The `reexport` declarations must be the first declarations to
  follow the `module` declaration.  </li>

  + It is possible to use both `reexport` and `use_module`, but all
  predicates reexported are automatically available for use in the
  current module.

  + In order to obtain efficient execution, YAP compiles
  dependencies between re-exported predicates. In practice, this means
  that changing a `reexport` declaration and then *just* recompiling
  the file may result in incorrect execution.

*/


/**
@}
**/

/**
 
This predicate actually exports _Module to the _ContextModule_.
 _Imports is what the ContextModule needed.																			
*/

'$import_module'(DonorM, HostM, F, _Opts) :-
    DonorM ==  HostM,
    !,
    (
	'$source_file_scope'( F, M)
    ->
    true;
	assert('$source_file_scope'( F, M) )
    ).
'$import_module'(DonorM, HostM, File, _Opts) :-
    \+
	'$module'(File, DonorM, _ModExports, _),                                                                 
	% enable loading C-predicates from a different file
	recorded( '$load_foreign_done', [File, DonorM], _),
	'$import_foreign'(File, DonorM, HostM ),
	fail.
'$import_module'(DonorM, HostM,File, Opts) :-
    DonorM \= HostM,
    '$module'(File, DonorM, Exports, _),
    ignore('$member'(imports(Imports),Opts)),
    '$filter'(Imports, Exports, Tab),
    '$add_to_imports'(Tab, DonorM, HostM),
    (     '$memberchk'(reexport(true),Opts)
    ->
    '$reexport'(HostM,Tab, DonorM )
    ;
    true
    ),
    !.
'$import_module'(_, _, _, _).

'$m_normalize'([],_, []).
'$m_normalize'([Decl|Ls], M, [NDecl|NLs]) :-
    '$m_norm'(Decl,M,NDecl),
    !,
    '$m_normalize'(Ls, M, NLs).
'$m_normalize'([Decl|Ls], M, [Decl|NLs]) :-
    '$m_normalize'(Ls, M, NLs).

'$m_norm'(N/A, _M, N/A-N/A).
'$m_norm'(N//A, _M, N/A2-N/A2) :- A2 is A+2.
'$m_norm'(N/A as M, _M, N/A-M/A).
'$m_norm'(N//A as M, _M, N/A2-M/A2) :- A2 is A+2.
'$m_norm'(op(A,B,V), M, op(A,B,V)) :- op(A,B,M:V).

'$filter'(V,E,E) :- 
    var(V), !.
'$filter'(all,E,E) :- !.
'$filter'(except(L),E,NE) :- !,
    '$drop_ms'(L,E,NE).
'$filter'(L,E,NL) :- !,
    '$select_ms'(L,E,NL,[]).

'$drop_ms'([],E,E).
'$drop_ms'([P|L],E,NE) :-
     '$drop_p'(P,E,IE),
 '$drop_ms'(L,IE,NE).

'$drop_p'(P/N,E,IE) :-
    '$delete'(E,_-P/N,IE),
    !.
'$drop_p'(P//N,E,IE) :-
    (var(N)
    ->
	'$delete'(E,_-P/N2,IE),
	N is N2-2
    ;
    N2 is N+2,
    '$delete'(E,_-P/N2,IE)
    ),
    !.
'$drop_p'(_P,E,E).

'$select_ms'([],_E, E, E).
'$select_ms'([P|L],E, LF, L0) :-
    '$select_p'(P,E, LF, LI),
     '$select_ms'(L,E, LI,L0).

'$select_p'(P/N,E,    [P0-P/N|F],   F) :-
    '$member'(P0-P/N,E),
    !.
'$select_p'(op(A,B,C),_E,    [op(A,B,C)|F],   F) :-
%    '$member'(P0-P/N,_E),
    !,
    [op(A,B,C)].
'$select_p'(P//N,E,    [P0-P/N|F],   F) :-
    (var(N)
    ->
	'$member'(P0-P/N2,E),
	N is N2-2
    ;
    N2 is N+2,
    '$member'(P0-P/N2,E)
    ),
    !,
    [P0-P/N2].
'$select_p'(_P,_E,F,F).


