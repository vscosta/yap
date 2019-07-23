/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2011	 *
*									 *
**************************************************************************
*									 *
* File:		qly.yap							 *
* Last rev:								 *
* mods:									 *
* comments:	fast save/restore					 *
*									 *
*************************************************************************/

%% @file qly.yap

/**
@defgroup QLY Creating and Using a saved state
@ingroup load_files
@{
*/

:- system_module( '$_qly', [qload_module/1,
        qsave_file/1,
        qsave_module/1,
        qsave_program/1,
        qsave_program/2,
        restore/1,
        save_program/1,
        save_program/2], ['$init_state'/0]).

:- use_system_module( '$_absf', ['$system_library_directories'/2]).

:- use_system_module( '$_boot', ['$system_catch'/4]).

:- use_system_module( '$_consult', ['$do_startup_reconsult'/1]).

:- use_system_module( '$_control', ['$run_atom_goal'/1]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_preds', ['$init_preds'/0]).

:- use_system_module( '$_protect', ['$protect'/0]).

:- use_system_module( '$_threads', ['$reinit_thread0'/0]).

:- use_system_module( '$_yio', ['$extend_file_search_path'/1]).

/**
YAP can save and read images of its current state to files, known as
saved states. It is possible to save the entire state or just a module
or a file. Notice that saved states in YAP depend on the architecture
where they were made, and may also depend on the version of YAP being
saved.

YAP always tries to find saved states from the current directory
first. If it cannot it will use the environment variable [YAPLIBDIR](@ref YAPLIBDIR), if
defined, or search the default library directory.

*/

/** @pred save_program(+ _F_)
Saves the current state of the data-base in file _F_ .

The result is a resource archive containing a saved state that
expresses all Prolog data from the running program and all
user-defined resources. Depending on the stand_alone option, the
resource is headed by the emulator, a Unix shell script or nothing.

**/
save_program(File) :-
	qsave_program(File).

/** @pred save_program(+ _F_, : _G_)

Saves an image of the current state of the YAP database in file
 _F_, and guarantee that execution of the restored code will start by
trying goal  _G_.
**/
qsave_program(File) :-
	'$save_program_status'([], qsave_program(File)),
	open(File, write, S, [type(binary)]),
	'$qsave_program'(S),
	  close(S).

/** @pred qsave_program(+ _F_, Opts)

Saves an image of the current state of the YAP database in file
 _F_, currently the options in _Opts_ are ignored:

   + stack(+ _KBytes_)
     Limit for the local and global stack.

   + trail(+ _KBytes_)
     Limit for the trail stack.

   + goal(: _Callable_)
     Initialization goal for the new executable (see `-g`).

   + init_file(+ _Atom_)
     Default initialization file for the new executable. See `-f`.

*/
qsave_program(File, Opts) :-
	'$save_program_status'(Opts, qsave_program(File,Opts)),
	open(File, write, S, [type(binary)]),
	'$qsave_program'(S),
	% make sure we're not going to bootstrap from this file.
	close(S).

/** @pred save_program(+ _F_, : _G_)

Saves an image of the current state of the YAP database in file
 _F_, and guarantee that execution of the restored code will start by
trying goal  _G_.
**/
save_program(_File, Goal) :-
	recorda('$restore_goal', Goal ,_R),
	fail.
save_program(File, _Goal) :-
        qsave_program(File).

/** @pred qend_program

  Saves an image of the current state of the YAP database in default
  filee, usually `startup.yss`.
  **/
qend_program :-
	module(user),
	qsave_program('startup.yss'),
	halt(0).

'$save_program_status'(Flags, G) :-
                  findall(F-V, 'x_yap_flag'(F,V),L),
    recordz('$program_state',L,_),
    'cvt_qsave_flags'(Flags, G),
    fail.
'$save_program_status'(_Flags, _G).

'cvt_qsave_flags'(Flags, G) :-
    nonvar(Flags),
    strip_module(Flags, M, LFlags),
    '$skip_list'(_Len, LFlags, []),
    'cvt_qsave_lflags'(LFlags, G, M).
'cvt_qsave_flags'(Flags, G,_OFlags) :-
    var(Flags),
    '$do_error'(instantiation_error,G).
'cvt_qsave_flags'(Flags, G,_OFlags) :-
    '$do_error'(type_error(list,Flags),G).

'cvt_qsave_lflags'([], _, _).
'cvt_qsave_lflags'([Flag|Flags], G, M) :-
    'cvt_qsave_flag'(Flag, G, M),
    'cvt_qsave_lflags'(Flags, G, M).

'cvt_qsave_flag'(Flag, G, _) :-
    var(Flag), !,
    '$do_error'(instantiation_error,G).
'cvt_qsave_flag'(local(B), G, _) :- !,
    ( number(B) ->
      (
       B > 0 -> recordz('$restore_flag',local(B),_) ;
       B =:= 0 -> true ;
       '$do_error'(domain_error(not_less_than_zero,B),G))
    ;
      '$do_error'(type_error(integer,B),G)
      ).
'cvt_qsave_flag'(global(B), G, _) :- !,
    ( number(B) ->
      (
       B > 0 -> recordz('$restore_flag',global(B),_) ;
       B =:= 0 -> true ;
       '$do_error'(domain_error(not_less_than_zero,B),G))
    ;
      '$do_error'(type_error(integer,B),G)
    ).
'cvt_qsave_flag'(stack(B), G, _) :- !,
    ( number(B) ->
      (
       B > 0 -> recordz('$restore_flag',stack(B),_) ;
       B =:= 0 -> true ;
       '$do_error'(domain_error(not_less_than_zero,B),G))
    ;
      '$do_error'(type_error(integer,B),G)
    ).
'cvt_qsave_flag'(trail(B), G, _) :- !,
    ( number(B) ->
      (
       B > 0 -> recordz('$restore_flag',trail(B),_) ;
       B =:= 0 -> true ;
       '$do_error'(domain_error(not_less_than_zero,B),G))
    ;
      '$do_error'(type_error(integer,B),G)
    ).
'cvt_qsave_flag'(goal(B), G, M) :- !,
    ( callable(B) ->
      strip_module(M:B, M1, G1),
      recordz('$restore_flag',goal(M1:G1),_)
    ;
       strip_module(M:B, M1, G1),
     '$do_error'(type_error(callable,G1),G)
    ).
'cvt_qsave_flag'(toplevel(B), G, M) :- !,
    ( callable(B) ->
      strip_module(M:B, M1, G1),
      recordz('$restore_flag',toplevel(M1:G1),_)
    ;
       strip_module(M:B, M1, G1),
     '$do_error'(type_error(callable,G1),G)
    ).
'cvt_qsave_flag'(init_file(B), G, M) :- !,
    ( atom(B) ->
      recordz('$restore_flag', init_file(M:B), _)
    ;
      '$do_error'(type_error(atom,B),G)
    ).
%% 'cvt_qsave_flag'(autoload(_B), G, autoload(_B)).
%% 'cvt_qsave_flag'(op(_B), G, op(_B)).
%% 'cvt_qsave_flag'(stand_alone(_B), G, stand_alone(_B)).
%% 'cvt_qsave_flag'(emulator(_B), G, emulator(_B)).
%% 'cvt_qsave_flag'(foreign(_B), G, foreign(_B)).
'cvt_qsave_flag'(Opt, G, _M) :-
    '$do_error'(domain_error(qsave_program,Opt), G).

% there is some ordering between flags.
'x_yap_flag'(language, V) :-
	yap_flag(language, V).
%if silent keep silent, otherwise use the saved state.
'x_yap_flag'(verbose, _) :- !.
'x_yap_flag'(verbose_load, _) :- !.
'x_yap_flag'(M:P, V) :-
	current_module(M),
	yap_flag(M:P, V).
'x_yap_flag'(X, V) :-
	prolog_flag_property(X, [access(read_write)]),
	atom(X),
	yap_flag(X, V),
	X \= gc_margin, % different machines will have different needs,
	X \= argv,
	X \= os_argv,
	X \= language,
	X \= encoding,
	fail.

qsave_file(F0) :-
    ensure_loaded(  F0 ),
    absolute_file_name( F0, File, [expand(true),file_type(prolog),access(read),file_errors(fail),solutions(first)]),
    absolute_file_name( F0, State, [expand(true),file_type(qly)]),
    '$qsave_file_'(File, State).

/** @pred qsave_file(+ _File_, +_State_)

Saves an image of all the information compiled by the system from file _F_ to _State_.
This includes modules and predicates eventually including multi-predicates.
**/
qsave_file(F0, State) :-
    ensure_loaded(  F0 ),
    absolute_file_name( F0, File, [expand(true),file_type(prolog),access(read),file_errors(fail),solutions(first)]),
    '$qsave_file_'(File, State).

'$qsave_file_'(File, UserF, _State) :-
    ( File == user_input -> Age = 0 ; time_file64(File, Age) ),
    '$current_module'(M),
    assert(user:'$file_property'( '$lf_loaded'( UserF, Age, M) ) ),
    '$set_owner_file'( '$file_property'( _ ), user, File ),
    fail.
'$qsave_file_'(File, UserF, _State) :-
    recorded('$lf_loaded','$lf_loaded'( File, M, Reconsult, UserFile, OldF, Line, Opts), _),
    assert(user:'$file_property'( '$lf_loaded'( UserF, M, Reconsult, UserFile, OldF, Line, Opts) ) ),
    '$set_owner_file'( '$file_property'( _ ), user, File ),
    fail.
'$qsave_file_'(File, _UserF, _State) :-
    recorded('$directive',directive( File, M:G, Mode,  VL, Pos ), _),
    assert(user:'$file_property'( directive( M:G, Mode,  VL, Pos ) ) ),
    '$set_owner_file'('$file_property'( _ ), user, File ),
    fail.
'$qsave_file_'(File, _UserF, _State) :-
    '$fetch_multi_files_file'(File, MultiFiles),
    assert(user:'$file_property'( multifile(MultiFiles  ) ) ),
    '$set_owner_file'('$file_property'( _ ), user, File ),
    fail.
'$qsave_file_'( File, _UserF, State ) :-
    (
	is_stream( State )
    ->
	'$qsave_file_preds'(State, File)
	;
	open(State, write, S, [type(binary)]),
        '$qsave_file_preds'(S, File),
        close(S)
    ),
    abolish(user:'$file_property'/1).

'$fetch_multi_files_file'(File, Multi_Files) :-
	setof(Info, '$fetch_multi_file_module'(File, Info), Multi_Files).

'$fetch_multi_file_file'(FileName, (M:G :- Body)) :-
	recorded('$multifile_defs','$defined'(FileName,Name,Arity,M), _),
	functor(G, Name, Arity ),
        clause(M:G, Body, ClauseRef),
	clause_property(ClauseRef, file(FileName) ).


/** @pred qsave_module(+ _Module_, +_State_)
Saves an image of all the information compiled by the systemm on module _F_ to _State_.
**/

qsave_module(Mod, OF) :-
	recorded('$module', '$module'(_F,Mod,Source,Exps,L), _),
	'$fetch_parents_module'(Mod, Parents),
	'$fetch_imports_module'(Mod, Imps),
	'$fetch_multi_files_module'(Mod, MFs),
	'$fetch_meta_predicates_module'(Mod, Metas),
	'$fetch_module_transparents_module'(Mod, ModTransps),
	'$fetch_term_expansions_module'(Mod, TEs),
	'$fetch_foreigns_module'(Mod, Foreigns),
	asserta(Mod:'@mod_info'(Source, Exps, MFs, L, Parents, Imps, Metas, ModTransps, Foreigns, TEs)),
	open(OF, write, S, [type(binary)]),
	'$qsave_module_preds'(S, Mod),
	close(S),
	abolish(Mod:'@mod_info'/10),
	fail.
qsave_module(_, _).

/** @pred qsave_module(+ Module x)

Saves an image of all the information compiled by the systemm on
module _F_ to a file _State.qly_ in the current directory.

**/

qsave_module(Mod) :-
	atom_concat(Mod,'.qly',OF),
	qsave_module(Mod, OF).

/**
  @pred restore(+ _F_)
Restores a previously saved state of YAP from file  _F_.

*/
restore(File) :-
	open(File, read, S, [type(binary)]),
	'$qload_program'(S),
	close(S).

/**
@pred qload_module(+ _M_)

Restores a previously save image of module  _M_. This built-in searches
for a file M.qly or M according to the rules for qly files.

The q_load_module/1 built-in tries to reload any modules it imports
from and any foreign files that had been loaded with the original
module. It tries first reloading from qly images, but if they are not
available it tries reconsulting the source file.

*/
qload_module(Mod) :-
  prolog_flag(verbose_load, OldF, false),
      prolog_flag(verbose, OldV, silent),
	Verbosity = silent,
    StartMsg = loading_module,
    EndMsg = module_loaded,
    '$current_module'(SourceModule, Mod),
    H0 is heapused, '$cputime'(T0,_),
    absolute_file_name( Mod, File, [expand(true),file_type(qly)]),
    print_message(Verbosity, loading(StartMsg, File)),
    file_directory_name( File, Dir),
    working_directory(OldD, Dir),
    '$qload_module'(Mod, File, SourceModule ),
    H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
    print_message(Verbosity, loaded(EndMsg, File, Mod, T, H)),
    '$current_module'(_, SourceModule),
  prolog_flag(verbose_load, _, OldF),
  prolog_flag(verbose, _, OldV),
  '$undefp_handler'('$undefp_search'(_,_), prolog),
    working_directory(_, OldD).

'$qload_module'(Mod, S, SourceModule) :-
    is_stream( S ), !,
    '$q_header'( S, Type ),
    stream_property( S, file_name( File )),
    ( Type == module ->
	  '$qload_module'(S , Mod, File, SourceModule)
    ;
      Type == file ->
	  '$qload_file'(S, File)
    ).
'$qload_module'(Mod, File, SourceModule) :-
    open(File, read, S, [type(binary)]),
    %check verifies if a saved state;
    '$q_header'( S, Type ), !,
    ( Type == module ->
	  '$qload_module'(S , Mod, File, SourceModule)
    ;
      Type == file ->
	  '$qload_file'(S, File)
    ),
    !,
    close(S).

'$qload_module'(_S, Mod, _File, _SourceModule) :-
    unload_module( Mod ), fail.
'$qload_module'(S, _Mod, _File, _SourceModule) :-
	'$qload_module_preds'(S), fail.

'$qload_module'(_S, Mod, File, SourceModule) :-
    Mod:'@mod_info'(F, Exps, MFs, Line,Parents, Imps, Metas, ModTransps, Foreigns, TEs),
    abolish(Mod:'@mod_info'/10),
    recorda('$module', '$module'(File, Mod, F, Exps, Line), _),
    '$install_parents_module'(Mod, Parents),
    '$install_imports_module'(Mod, Imps, []),
    '$install_multi_files_module'(Mod, MFs),
    '$install_meta_predicates_module'(Mod, Metas),
    '$install_foreigns_module'(Mod, Foreigns),
    '$install_module_transparents_module'(Mod, ModTransps),
    '$install_term_expansions_module'(Mod, TEs),
    % last, export everything to the host: if the loading crashed you didn't actually do
    % no evil.
    '$convert_for_export'(all, Exps, Mod, SourceModule, TranslationTab, _AllExports0, qload_module),
    '$add_to_imports'(TranslationTab, Mod, SourceModule). % insert ops, at least for now

'$fetch_imports_module'(Mod, Imports) :-
	findall(Info, '$fetch_import_module'(Mod, Info), Imports).

% detect an import that is local to the module.
'$fetch_import_module'(Mod, '$impcort'(Mod0,Mod,G0,G,N,K) - S) :-
	recorded('$import', '$import'(Mod0,Mod,G0,G,N,K), _),
	( recorded('$module','$module'(_, Mod0, S, _, _), _) -> true ; S = user_input ).

'$fetch_parents_module'(Mod, Parents) :-
	findall(Parent, prolog:'$parent_module'(Mod,Parent), Parents).

'$fetch_module_transparents_module'(Mod, Module_Transparents) :-
	findall(Info, '$fetch_module_transparent_module'(Mod, Info), Module_Transparents).

% detect an module_transparenterator that is local to the module.
'$fetch_module_transparent_module'(Mod, '$module_transparent'(F,Mod,N,P)) :-
	prolog:'$module_transparent'(F,Mod0,N,P), Mod0 == Mod.

'$fetch_meta_predicates_module'(Mod, Meta_Predicates) :-
	findall(Info, '$fetch_meta_predicate_module'(Mod, Info), Meta_Predicates).

% detect a meta_predicate that is local to the module.
'$fetch_meta_predicate_module'(Mod, '$meta_predicate'(F,Mod,N,P)) :-
	prolog:'$meta_predicate'(F,M,N,P), M==Mod.

'$fetch_multi_files_module'(Mod, Multi_Files) :-
	findall(Info, '$fetch_multi_file_module'(Mod, Info), Multi_Files).

% detect an multi_file that is local to the module.
'$fetch_multi_file_module'(Mod, '$defined'(FileName,Name,Arity,Mod)) :-
	recorded('$multifile_defs','$defined'(FileName,Name,Arity,Mod), _).
'$fetch_multi_file_module'(Mod, '$mf_clause'(FileName,_Name,_Arity,Mod,Clause), _) :-
    recorded('$mf','$mf_clause'(FileName,_Name,_Arity,Mod,ClauseRef), _),
    instance(ClauseRef, Clause ).

'$fetch_term_expansions_module'(Mod, TEs) :-
	findall(Info, '$fetch_term_expansion_module'(Mod, Info), TEs).

% detect an term_expansionerator that is local to the module.
'$fetch_term_expansion_module'(Mod, ( user:term_expansion(G, GI) :- Bd )) :-
	clause( user:term_expansion(G, GI), Bd, _),
	strip_module(G, Mod, _).
% detect an term_expansionerator that is local to the module.
'$fetch_term_expansion_module'(Mod, ( system:term_expansion(G, GI) :- Bd )) :-
	clause( system:term_expansion(G, GI), Bd, _),
	strip_module(G, Mod, _).
% detect an term_expansionerator that is local to the module.
'$fetch_term_expansion_module'(Mod, ( user:goal_expansion(G, CurMod, GI) :- Bd )) :-
	clause( user:goal_expansion(G, CurMod, GI), Bd, _),
	Mod == CurMod.
% detect an term_expansionerator that is local to the module.
'$fetch_term_expansion_module'(Mod, ( user:goal_expansion(G, GI) :- Bd )) :-
	clause( user:goal_expansion(G, GI), Bd, _),
	strip_module(G, Mod, _).
% detect an term_expansionerator that is local to the module.
'$fetch_term_expansion_module'(Mod, ( system:goal_expansion(G, GI) :- Bd )) :-
	clause( system:goal_expansion(G, GI), Bd, _),
	strip_module(G, Mod, _).

'$fetch_foreigns_module'(Mod, Foreigns) :-
	findall(Info, '$fetch_foreign_module'(Mod, Info), Foreigns).

% detect an term_expansionerator that is local to the module.
'$fetch_foreign_module'(Mod,Foreign) :-
	recorded( '$foreign', Mod:Foreign, _).

'$install_term_expansions_module'(_, []).
'$install_term_expansions_module'(Mod, [TE|TEs]) :-
    assert(TE),
    '$install_term_expansions_module'(Mod, TEs).

'$install_imports_module'(_, [], Fs0) :-
    sort(Fs0, Fs),
    '$restore_load_files'(Fs).
'$install_imports_module'(Mod, [Import-F|Imports], Fs0) :-
	recordz('$import', Import, _),
	arg(1, Import, M),
	'$install_imports_module'(Mod, Imports, [M-F|Fs0]).

'$restore_load_files'([]).
'$restore_load_files'([M-F0|Fs]) :-
    (
	absolute_file_name( M,_File, [expand(true),file_type(qly),access(read),file_errors(fail)])
    ->
        qload_module(M)
    ;
	use_module(M, F0, _)
    ),
    '$restore_load_files'(Fs).

'$install_parents_module'(_, []).
'$install_parents_module'(Mod, [Parent|Parents]) :-
	assert(prolog:Parent),
	'$install_parents_module'(Mod, Parents).

'$install_module_transparents_module'(_, []).
'$install_module_transparents_module'(Mod, [Module_Transparent|Module_Transparents]) :-
	assert(prolog:Module_Transparent),
	'$install_module_transparents_module'(Mod, Module_Transparents).

'$install_meta_predicates_module'(_, []).
'$install_meta_predicates_module'(Mod, [Meta_Predicate|Meta_Predicates]) :-
	assert(prolog:Meta_Predicate),
	'$install_meta_predicates_module'(Mod, Meta_Predicates).

'$install_multi_files_module'(_, []).
'$install_multi_files_module'(Mod, [Multi_File|Multi_Files]) :-
	recordz('$multifile_defs',Multi_File, _),
	'$install_multi_files_module'(Mod, Multi_Files).

'$install_foreigns_module'(_, []).
'$install_foreigns_module'(Mod, [Foreign|Foreigns]) :-
	'$do_foreign'(Foreign, Foreigns),
	'$install_foreigns_module'(Mod, Foreigns).

'$do_foreign'('$foreign'(Objs,Libs,Entry), _) :-
    load_foreign_files(Objs,Libs,Entry).
'$do_foreign'('$swi_foreign'(File, Opts, Handle), More) :-
    open_shared_object(File, Opts, Handle, NewHandle),
    '$init_foreigns'(More, NewHandle).
'$do_foreign'('$swi_foreign'(_,_), _More).

'$init_foreigns'([], _Handle, _NewHandle).
'$init_foreigns'(['$swi_foreign'( Handle, Function )|More], Handle, NewHandle) :-
    !,
    call_shared_object_function( NewHandle, Function),
    '$init_foreigns'(More, Handle, NewHandle).
'$init_foreigns'([_|More], Handle, NewHandle) :-
    '$init_foreigns'(More, Handle, NewHandle).

/**
@pred qload_file(+ _F_)

Restores a previously saved state of YAP contaianing a qly file  _F_.

*/
qload_file( F0 ) :-
    ( current_prolog_flag(verbose_load, false)
      ->
	Verbosity = silent
	;
	current_prolog_flag(verbose, Verbosity)
    ),
    StartMsg = loading_module,
    EndMsg = module_loaded,
    '$current_module'( SourceModule ),
    H0 is heapused,
    '$cputime'(T0,_),
    ( is_stream( F0 )
      ->
      stream_property(F0, file_name(File) ),
      File = FilePl,
      S = File
    ;
      absolute_file_name( F0, File, [expand(true),file_type(qly)]),
      absolute_file_name( F0, FilePl, [expand(true),file_type(prolog)]),
      unload_file( FilePl ),
      open(File, read, S, [type(binary)])
    ),
    print_message(Verbosity, loading(StartMsg, File)),
    file_directory_name(File, DirName),
    working_directory(OldD, DirName),
    '$q_header'( S, Type ),
    ( Type == module ->
	  '$qload_module'(S , Mod, File, SourceModule)
    ;
      Type == file ->
	'$lf_option'(last_opt, LastOpt),
	functor( TOpts, opt, LastOpt ),
	'$lf_default_opts'(1, LastOpt, TOpts),
	  '$qload_file'(S, SourceModule, File, FilePl, F0, all, TOpts)
    ),
    close(S),
    working_directory( _, OldD),
    H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
    '$current_module'(Mod, Mod ),
    print_message(Verbosity, loaded(EndMsg, File, Mod, T, H)),
    '$init_prolog'.

'$qload_file'(_S, SourceModule, _F, FilePl, _F0, _ImportList, _TOpts) :-
    recorded('$source_file','$source_file'( FilePl, _Age, SourceModule), _),
   !.
'$qload_file'(_S, SourceModule, _F, FilePl, _F0, _ImportList, _TOpts) :-
    ( FilePl == user_input -> Age = 0 ; time_file64(FilePl, Age) ),
    recordaifnot('$source_file','$source_file'( FilePl, Age, SourceModule), _),
    fail.
'$qload_file'(S, _SourceModule, _File, _FilePl, _F0, _ImportList, _TOpts) :-
    '$qload_file_preds'(S),
    fail.
'$qload_file'(_S, SourceModule, F, _FilePl, _F0, _ImportList, _TOpts) :-
    user:'$file_property'( '$lf_loaded'( F, Age, _ ) ),
    recordaifnot('$source_file','$source_file'( F, Age, SourceModule), _),
    fail.
'$qload_file'(_S, _SourceModule, _File, FilePl, F0, _ImportList, _TOpts) :-
    b_setval('$user_source_file', F0 ),
    '$ql_process_directives'( FilePl ),
    fail.
'$qload_file'(_S, SourceModule, _File,  FilePl, _F0, ImportList, TOpts) :-
    '$import_to_current_module'(FilePl, SourceModule, ImportList, _, TOpts).

'$ql_process_directives'( FilePl ) :-
    user:'$file_property'( '$lf_loaded'( FilePl, M, Reconsult, UserFile, OldF, Line, Opts) ),
    recorda('$lf_loaded','$lf_loaded'( FilePl, M, Reconsult, UserFile, OldF, Line, Opts), _),
    fail.
'$ql_process_directives'( _FilePl ) :-
    user:'$file_property'( multifile( List ) ),
    lists:member( Clause, List ),
    assert( Clause ),
    fail.
'$ql_process_directives'( FilePl ) :-
    user:'$file_property'( directive( MG, _Mode,  VL, Pos  ) ),
    '$set_source'( FilePl, Pos ),
    '$yap_strip_module'(MG, M, G),
    '$process_directive'(G, reconsult, M, VL, Pos),
    fail.
'$ql_process_directives'( _FilePl ) :-
    abolish(user:'$file_property'/1).

%% @}
