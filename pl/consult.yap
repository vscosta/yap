/*************************************************************************
*									 *
  *	 YAP Prolog  							 *
*									 *
  *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		consult.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Consulting Files in YAP					 *
*									 *
*************************************************************************/
/**
 * @file   consult.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Wed Nov 18 14:01:10 2015
 *
 * @brief  loading programs into YAP
 *
 *
*/
:- system_module( '$_consult', [compile/1,
        consult/1,
        db_files/1,
        ensure_loaded/1,
        exists_source/1,
        exo_files/1,
        (initialization)/2,
        load_files/2,
        make/0,
        make_library_index/1,
        module/2,
        prolog_load_context/2,
        reconsult/1,
        source_file/1,
        source_file/2,
        source_file_property/2,
        use_module/3],
	['$add_multifile'/3,
        '$csult'/2,
        '$do_startup_reconsult'/1,
        '$elif'/2,
        '$else'/1,
        '$endif'/1,
        '$if'/2,
        '$include'/2,
        '$initialization'/1,
        '$initialization'/2,
        '$lf_opt'/3,
        '$load_files'/3,
        '$require'/2,
        '$set_encoding'/1,
        '$use_module'/3]).

:- use_system_module( '$_absf', ['$full_filename'/2]).

:- use_system_module( '$_boot', ['$clear_reconsulting'/0,
        '$init_system'/0,
        '$init_win_graphics'/0,
        '$loop'/2,
        '$system_catch'/4]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_load_foreign', ['$import_foreign'/3]).

:- use_system_module( '$_modules', ['$add_to_imports'/3,
        '$convert_for_export'/6,
        '$extend_exports'/3]).

:- use_system_module( '$_preds', ['$current_predicate'/4]).

/**

  @defgroup YAPConsulting Loading files into YAP
  @ingroup load_files

  @{

  We present the main predicates and directives available to load
files and to set-up the Prolog environment. We discuss

  + @ref YAPReadFiles

  + @ref YAPCompilerSettings

    @}
  */

/**
@defgroup YAPReadFiles The Predicates that Read Source Files
  @ingroup  YAPConsulting

@{
 */

/**

 @pred load_files(+_Files_, +_Options_)

Implementation of the consult/1 family. Execution is controlled by the
following flags:

+ consult(+ _Mode_)

    This extension controls the type of file to load. If  _Mode_ is:

  `consult`, clauses are added to the data-base, unless from the same file;
  `reconsult`, clauses are recompiled,
  `db`, these are facts that need to be added to the data-base,
  `exo`, these are facts with atoms and integers that can be stored in a compact representation (see load_exo/1).

+ silent(+ _Bool_)

    If true, load the file without printing a message. The specified
    value is the default for all files loaded as a result of loading
    the specified files.

+ stream(+ _Input_)

    This SWI-Prolog extension compiles the data from the stream
    _Input_. If this option is used, _Files_ must be a single atom
    which is used to identify the source-location of the loaded
    clauses as well as remove all clauses if the data is re-consulted.

    This option is added to allow compiling from non-file locations
    such as databases, the web, the user (see consult/1) or other
    servers.

+ compilation_mode(+ _Mode_)

    This extension controls how procedures are compiled. If _Mode_ is
    `compact` clauses are compiled and no source code is stored; if it
    is `source` clauses are compiled and source code is stored; if it
    is `assert_all` clauses are asserted into the data-base.

+ encoding(+ _Encoding_)

    Character encoding used in consulting files. Please (see
    [Encoding](@ref Encoding)) for supported encodings.

+ expand(+ _Bool_)

    If `true`, run the filenames through expand_file_name/2 and load
    the returned files. Default is false, except for consult/1 which
    is intended for interactive use.

+ if(+ _Condition_)

    Load the file only if the specified _Condition_ is satisfied. The
    value `true` the file unconditionally, `changed` loads the file if
    it was not loaded before, or has been modified since it was loaded
    the last time, `not_loaded` loads the file if it was not loaded
    before.

+ imports(+ _ListOrAll_)

    If `all` and the file is a module file, import all public
    predicates. Otherwise import only the named predicates. Each
    predicate is referred to as `\<name\>/\<arity\>`. This option has
    no effect if the file is not a module file.

+ must_be_module(+ _Bool_)

    If true, raise an error if the file is not a module file. Used by
    use_module/1 and use_module/2.

+ qcompile(+ _Value_)

    SWI-Prolog flag that controls whether loaded files should be also
    compiled into `qly` files. The default value is obtained from the flag
    `qcompile`:

   `never`, no `qly` file is generated unless the user calls
 qsave_file/1 and friends, or sets the qcompile option in
 load_files/2;

  `auto`, all files are qcompiled.

  `large`, files above 100KB are qcompiled.

  `part`, not supported in YAP.

+ autoload(+ _Autoload_)

SWI-compatible option where if _Autoload_ is `true` undefined
  predicates are loaded on first call.

+ derived_from(+ _File_)

    SWI-compatible option to control make/0. Currently not supported.

*/

load_files(Files0,Opts) :-
    '$yap_strip_module'(Files0,M,Files),
    '$load_files'(Files,M,Opts,M:load_files(Files,Opts)).




/**

  @pred ensure_loaded(+ _F_) is iso

When the files specified by  _F_ are module files,
ensure_loaded/1 loads them if they have note been previously
loaded, otherwise advertises the user about the existing name clashes
  and prompts about importing or not those predicates. Predicates which
are not public remain invisible.

When the files are not module files, ensure_loaded/1 loads them
                                       eh  if they have not been loaded before, and does nothing otherwise.

 _F_ must be a list containing the names of the files to load.
*/
ensure_loaded(Fs) :-
	load_files(Fs, [if(not_loaded)]).

compile(Fs) :-
	load_files(Fs, []).

/**
 @pred [ _F_ ]
 @pred consult(+ _F_)


Adds the clauses written in file  _F_ or in the list of files  _F_
to the program.

In YAP consult/1 does not remove previous clauses for
the procedures defined in other files than _F_, but since YAP-6.4.3 it will redefine all procedures defined in _F_.

All code in YAP is compiled, and the compiler generates static
procedures by default. In case you need to manipulate the original
code, the expanded version of the original source code is available by
calling source/0 or by enabling the source flag.

*/
% consult(Fs) :-
% 	'$has_yap_or'
% 	'$do_error'(context_error(consult(Fs),cla ,query).
consult(V) :-
	var(V), !,
	'$do_error'(instantiation_error,consult(V)).
consult(M0:Fs) :- !,
	'$consult'(Fs, M0).
consult(Fs) :-
	current_source_module(M0,M0),
	'$consult'(Fs, M0).

'$consult'(Fs,Module) :-
	current_prolog_flag(language_mode, iso), % SICStus Prolog compatibility
	!,
	load_files(Module:Fs,[]).
'$consult'(Fs, Module) :-
    load_files(Module:Fs,[consult(consult)]).


/**

@pred [ - _F_ ]
@pred reconsult(+ _F_ )
  @pred compile(+ _F_ )

Updates the program by replacing the
previous definitions for the predicates defined in  _F_. It differs from consult/1
in that it only multifile/1 predicates are not reset in a reconsult. Instead, consult/1
sees all predicates as multifile.

YAP also offers no difference between consult/1 and compile/1. The two
are  implemented by the same exact code.

Example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- [file1, -file2, -file3, file4].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  will consult `file1` `file4` and reconsult `file2` and
`file3`. That is, it could be written as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- consult(file1),
   reconsult( [file2, file3],
   consult( [file4] ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/
reconsult(Fs) :-
	load_files(Fs, []).


/* exo_files(+ _Files_)

Load compactly a database of facts with equal structure, see @cite
x. Useful when wanting to read in a very compact way database tables,
it saves space by storing data, not a compiled program. The idea was
introduced in @cite y, but never implemented because often indexing
just takes more room. It was redefined recebtly by exploiting
different forms of indexing, as shown in @cite x.

@note implementation

  The function Yap_ExoLookup() is the mai interface betwwen the WAM
  and exo components. The algorithms are straightforward, that is,
  mostly hash-tables but have close to linear performance..
*/

exo_files(Fs) :-
	load_files(Fs, [consult(exo), if(not_loaded)]).

/**

  @pred load_db(+ _Files_)


Load a database of ground facts. All facts must take up the same amount of storage, so that
 a fact $I$ can be accessed at position _P[I-1]_. This representation thus stores the facts as a huge continuous array, the so-called mega clause.

See \cite for a motivation for this technique. YAP implements this
optimization by default whenever it loads a large number of facts (see
Yap_BuildMegaClause(PredEntry *ap) ) for the details. On the other
hand, loading the data-base will cause fragmentation because
individual facts facts need extra headers ands tails, and because
often new atoms will be stored in the Symbol Table, see
LookupAtom(const char *atom). The main advantage of load_db/1 is
that it allocates the necessary memory only once. Just doing this
 may halve total memory usage in large in-memory database-oriented applications.

@note Implementation

YAP implements load_db/1 as a two-step non-optimised process. First,
   it counts the nmuber of facts and checks their size. Second, it
   allocates and fills the memory. The first step of the algorithm is
   implemented by dbload_get_space(), and the second by
   dbload_add_facts().

   db_files/1 itself is just a call to load_files/2.
*/
db_files(Fs) :-
    load_files(Fs, [consult(db), if(not_loaded)]).


'$csult'(Fs, _M) :-
	 '$skip_list'(_, Fs ,L),
	 L \== [],
	 !,
	 user:dot_qualified_goal(Fs).
'$csult'(Fs, M) :-
	'$extract_minus'(Fs, MFs), !,
	load_files(M:MFs,[]).
'$csult'(Fs, M) :-
	load_files(M:Fs,[consult(consult)]).

'$csult_in_mod'(M, -F ) :- load_files(M:F,[]).
'$csult_in_mod'(M, F ) :-  load_files(M:F,[consult(consult)]).

'$extract_minus'([], []).
'$extract_minus'([-F|Fs], [F|MFs]) :-
	'$extract_minus'(Fs, MFs).


/** @defgroup  YAPCompilerSettings Directing and Configuring the Compiler
    @ingroup  YAPProgramming

@{

  The YAP system also includes a number of primitives designed to set
  compiler parameters and to track the state of the compiler. One
  important example is the number of directivees that allow setting up
  properties of predicates. It is also possible to enable or disable
  waraanings about possible issues with the code in the program, sich
  as the occurrence .

This section presents a set of built-ins predicates designed to set the
environment for the compiler.

*/


/** @pred prolog_to_os_filename(+ _PrologPath_,- _OsPath_)

This is an SWI-Prolog built-in. Converts between the internal Prolog
pathname conventions and the operating-system pathname conventions. The
internal conventions are Unix and this predicates is equivalent to =/2
(unify) on Unix systems. On DOS systems it will change the
directory-separator, limit the filename length map dots, except for the
last one, onto underscores.

*/


% retract old multifile clauses for current file.
'$remove_multifile_clauses'(FileName) :-
	recorded('$multifile_defs','$defined'(FileName,_,_,_),R1),
	erase(R1),
	fail.
'$remove_multifile_clauses'(FileName) :-
	recorded('$mf','$mf_clause'(FileName,_,_,Module,Ref),R),
	'$erase_clause'(Ref, Module),
	erase(R),
	fail.
'$remove_multifile_clauses'(_).

/** @pred initialization(+ _G_) is iso

The compiler will execute goals  _G_ after consulting the current
file.

Notice that the goal will execute in the calling context, not within the file context,
In other words, the source module and execution directory will be the ones of the parent
environment. Use initialization/2 for more flexible behavior.

*/
'$initialization'(G) :-
    '$initialization'( G, after_load ).

'$initialization_queue'(G) :-
    (
	'__NB_getval__'('$lf_status', TOpts,fail)
    ->
    true
    ;
    TOpts=[]),
	'$lf_opt'( initialization, TOpts, Ref),
	nb:nb_queue_enqueue(Ref, G),
	fail.
'$initialization_queue'(_).



/** @pred initialization(+ _Goal_,+ _When_)

Similar to initialization/1, but allows for specifying when
 _Goal_ is executed while loading the program-text:


    + now
      Execute  _Goal_ immediately.

    + after_load
      Execute  _Goal_ after loading program-text. This is the same as initialization/1.

    + restore
      Do not execute  _Goal_ while loading the program, but only when restoring a state (not implemented yet).

*/
initialization(G,OPT) :-
    '$initialization'(G, OPT),
    fail.
initialization(_G,_OPT).

'$initialization'(G0,OPT) :-
    must_be_callable( G0),
    expand_goal(G0, G),
    %   must_be_of_type(oneof([after_load, now, restore]),
 %               OPT),

    (
	 OPT == now
	->
	( catch(call(G),
		Error,
		'$LoopError'( Error, consult )
	       ) ->
	  true ;
	  format(user_error,':- ~w failed.~n',[G])
	)
	;
	 OPT == after_load
	->
	 '$initialization_queue'(G)
	;
	 OPT == restore
	->
    recordz('$call_at_restore', G, _ )
    ).

/**

@}
*/


'$exec_initialization_goals'(_) :-
     set_prolog_flag(optimise, true),
     set_prolog_flag(verbose_load, false),
	recorded('$blocking_code',_,R),
	erase(R),
	fail.
% system goals must be performed first
'$exec_initialization_goals'(_TOpts) :-
	recorded('$system_initialization',G,R),
	erase(R),
	G \= '$',
	( catch(G, Error, user:'$LoopError'(Error, top))
	->
	  true
	;
	  format(user_error,':- ~w failed.~n',[G])
	),
	fail.
'$exec_initialization_goals'(TOpts) :-
	'$lf_opt'( initialization, TOpts, Ref),
	nb:nb_queue_close(Ref, Answers, []),
	'$process_init_goal'(Answers),
	fail.
'$exec_initialization_goals'(_TOpts).


'$process_init_goal'([]).

'$process_init_goal'([G|_]) :-
	(catch(
	  call(G),
	 E,
	 '$LoopError'(E,top)
	     )
	->
	fail
	;
	 format(user_error,':- ~w failed.~n',[G]),
	 fail
	 ).
'$process_init_goal'([_|Gs]) :-
   '$process_init_goal'(Gs).
%
% reconsult at startup...
%
'$do_startup_reconsult'(_X) :-
    '$init_win_graphics',
	fail.
'$do_startup_reconsult'(X) :-
	catch(load_files(user:X, [silent(true)]), Error, '$LoopError'(Error, consult)),
	!,
	( current_prolog_flag(halt_after_consult, false) -> true ; halt(0)).
'$do_startup_reconsult'(_) .

'$skip_unix_header'(Stream) :-
	peek_code(Stream, 0'#), !, % 35 is ASCII for '#
	skip(Stream, 10),
	'$skip_unix_header'(Stream).
'$skip_unix_header'(_).


source_file(FileName) :-
	recorded('$source_file','$source_file'(FileName, _, _),_).

source_file(Mod:Pred, FileName) :-
	current_module(Mod),
	Mod \= prolog,
	'$current_predicate'(_,Mod,Pred,all),
	'$owned_by'(Pred, Mod, FileName).

'$owned_by'(T, Mod, FileName) :-
	'$is_multifile'(T, Mod),
	functor(T, Name, Arity),
	setof(FileName, Ref^recorded('$multifile_defs','$defined'(FileName,Name,Arity,Mod), Ref), L),
	lists:member(FileName, L).
'$owned_by'(T, Mod, FileName) :-
	'$owner_file'(T, Mod, FileName).

/** @pred prolog_load_context(? _Key_, ? _Value_)

  Obtain information on what is going on in the compilation process. The
  following keys are available:

  + directory  (prolog_load_context/2 option)

  Full name for the directory where YAP 									is currently consulting the
  file.

  + file  (prolog_load_context/2 option)

  Full name for the file currently being consulted. Notice that included
  filed are ignored.

  + module  (prolog_load_context/2 option)

  Current source module.

  + `source` (prolog_load_context/2 option)

  Full name for the file currently being read in, which may be consulted,
  reconsulted, or included.

  + `stream`  (prolog_load_context/2 option)

  Stream currently being read in.

  + `term_position`  (prolog_load_context/2 option)

  Stream position at the stream currently being read in. For SWI
  compatibility, it is a term of the form
  '$stream_position'(0,Line,0,0).

  + `source_location(? _File Name_, ? _Line_)`   (prolog_load_context/2 option)

  SWI-compatible predicate. If the last term has been read from a physical file (i.e., not from the file user or a string), unify File with an absolute path to the file and Line with the line-number in the file. Please use prolog_load_context/2.

  + `source_file(? _File_)`  (prolog_load_context/2 option)

  SWI-compatible predicate. True if  _File_ is a loaded Prolog source file.

  + `source_file(? _ModuleAndPred_ ,? _File_)`  (prolog_load_context/2 option)

  SWI-compatible predicate. True if the predicate specified by  _ModuleAndPred_ was loaded from file  _File_, where  _File_ is an absolute path name (see `absolute_file_name/2`).

*/
prolog_load_context(directory, DirName) :-
        ( source_location(F, _)
        -> file_directory_name(F, DirName) ;
          working_directory( DirName, DirName )
        ).
prolog_load_context(file, FileName) :-
        ( source_location(FileName, _)
        ->
          true
        ;
          FileName = user_input
        ).
prolog_load_context(module, X) :-
        '__NB_getval__'('$consulting_file', _, fail),
        current_source_module(Y,Y),
        Y = X.
prolog_load_context(source, F0) :-
        ( source_location(F0, _) /*,
                                   '$input_context'(Context),
                                   '$top_file'(Context, F0, F) */
          ->
          true
        ;
          F0 = user_input
        ).
prolog_load_context(stream, Stream) :-
	stream_property(Stream, alias(loop_stream) ).


% if the file exports a module, then we can
% be imported from any module.
'$file_loaded'(F0, M) :-
				%format( 'L=~w~n', [(F0)] ),
	(
	    atom_concat(Prefix, '.qly', F0 );
	    Prefix=F0
	),
	(
	 absolute_file_name(Prefix,F,[access(read),file_type(qly),file_errors(fail),solutions(first),expand(true)])
        ;
           F0 = F
	),
	'$ensure_file_loaded'(F, M).

'$ensure_file_loaded'(F, NM) :-
				% loaded from the same module, but does not define a module.
	 recorded('$source_file','$source_file'(F, _Age, NM), _R),
				% make sure: it either defines a new module or it was loaded in the same context
	 	(recorded('$module','$module'(F,NM,_ASource,_P,_),_) ->
	    true
	;
	    current_source_module(M,M), M == NM
).

				% if the file exports a module, then we can
% be imported from any module.
'$file_unchanged'(F, NM) :-
        % loaded from the same module, but does not define a module.
	recorded('$source_file','$source_file'(F, Age, NM), R),
	% make sure: it either defines a new module or it was loaded in the same context
	'$file_is_unchanged'(F, R, Age),
	!,
%	( F = '/usr/local/share/Yap/rbtrees.yap' ->start_low_level_trace ; true),
	(recorded('$module','$module'(F,NM,_ASource,_P,_),_) ->
	    true
	;
	    current_source_module(M,M), M == NM
	).

'$file_is_unchanged'(F, R, Age) :-
        time_file64(F,CurrentAge),
        ( (Age == CurrentAge ; Age = -1)  -> true; erase(R), fail).

	% inform the file has been loaded and is now available.
'$loaded'(F, UserFile, M, OldF, Line, Reconsult0, Reconsult, Dir, TOpts, Opts) :-
	( '$lf_opt'('$from_stream',TOpts,true) -> working_directory(Dir,Dir) ; file_directory_name(F, Dir) ),
	nb_setval('$consulting_file', F ),
	(
	 % if we are reconsulting, always start from scratch
	 Reconsult0 \== consult,
	 Reconsult0 \== not_loaded,
	 Reconsult0 \== changed,
	 recorded('$source_file','$source_file'(F, _,_),R),
	 erase(R),
	 fail
	;
	 var(Reconsult0)
	->
	 Reconsult = consult
	;
	 Reconsult = Reconsult0
	),
	(
	    Reconsult \== consult,
	    recorded('$lf_loaded','$lf_loaded'(F, _, _, _, _, _, _),R),
	    erase(R),
	    fail
	    ;
	    var(Reconsult)
	    ->
		Reconsult = consult
	    ;
	    Reconsult = Reconsult0
	),
	( '$lf_opt'('$from_stream',TOpts,true) -> Age = 0 ; time_file64(F, Age) ),
				% modules are logically loaded only once

	( recorded('$module','$module'(F,_DonorM,_SourceF, _AllExports, _Line),_) -> true  ;
		  recordaifnot('$source_file','$source_file'( F, Age, M), _) -> true ;
		  true ),
	recorda('$lf_loaded','$lf_loaded'( F, M, Reconsult, UserFile, OldF, Line, Opts), _).

/** @pred make is det

SWI-Prolog originally included this built-in as a Prolog version of the Unix `make`
utility program. In this case the idea is to reconsult all source files that have been changed since they were originally compiled into Prolog. YAP has a limited implementation of make/0 that
just goes through every loaded file and verifies whether reloading is needed.

*/

make :-
	recorded('$lf_loaded','$lf_loaded'(F1,_M,reconsult,_,_,_,_),_),
	load_files(F1, [if(changed)]),
	fail.
make.

make_library_index(_Directory).

'$fetch_stream_alias'(OldStream,Alias) :-
	stream_property(OldStream, alias(Alias)), !.

'$require'(_Ps, _M).

'$store_clause'('$source_location'(File, _Line):Clause, File) :-
	assert_static(Clause).
% reload_file(File) :-
%         ' $source_base_name'(File, Compile),
%         findall(M-Opts,
%                 source_file_property(File, load_context(M, _, Opts)),
%                 Modules),
%         (   Modules = [First-OptsFirst|Rest]
%         ->  Extra = [ silent(false),
%                       register(false)
%                     ],
%             merge_options([if(true)|Extra], OptsFirst, OFirst),
% %            debug(make, 'Make: First load ~q', [load_files(First:Compile, OFirst)]),
%             load_files(First:Compile, OFirst),
%             forall(member(Context-Opts, Rest),
%                    ( merge_options([if(not_loaded)|Extra], Opts, O),
% %                     debug(make, 'Make: re-import: ~q',
% %                           [load_files(Context:Compile, O)]),
%                      load_files(Context:Compile, O)
%                    ))
%         ;   load_files(user:Compile)
%         ).

% ' $source_base_name'(File, Compile) :-
%         file_name_extension(Compile, Ext, File),
%         user:prolog_file_type(Ext, prolog), !.
% ' $source_base_name'(File, File).

source_file_property( File0, Prop) :-
	( nonvar(File0) -> absolute_file_name(File0,File) ; File = File0 ),
	'$source_file_property'( File, Prop).

'$source_file_property'( OldF, includes(F, Age)) :-
	recorded('$lf_loaded','$lf_loaded'( F, _M, include, _File, OldF, _Line, _), _),
	recorded('$source_file','$source_file'( F, Age, _), _).
'$source_file_property'( F, included_in(OldF, Line)) :-
	recorded('$lf_loaded','$lf_loaded'( F, _M, include, _File, OldF, Line, _), _).
'$source_file_property'( F, load_context(OldF, Line, Options)) :-
	recorded('$lf_loaded','$lf_loaded'( F, _M, V, _File, OldF, Line, Options), _), V \== include.
'$source_file_property'( F, modified(Age)) :-
	recorded('$source_file','$source_file'( F, Age, _), _).
'$source_file_property'( F, module(M)) :-
	recorded('$module','$module'(F,M,_,_,_),_).

unload_file( F0 ) :-
    absolute_file_name( F0, F1, [expand(true),file_type(prolog)] ),
    '$unload_file'( F1, F0 ).

% eliminate multi-files;
% get rid of file-only predicataes.
'$unload_file'( FileName, _F0 ) :-
	current_module(Mod),
	'$current_predicate'(_A,Mod,P,all),
	'$owner_file'(P,Mod,FileName),
	\+ '$is_multifile'(P,Mod),
	functor( P, Na, Ar),
	abolish(Mod:Na/Ar),
	fail.
%next multi-file.
'$unload_file'( FileName, _F0 ) :-
    recorded('$source_file','$source_file'( FileName, _Age, _), R),
    erase(R),
    fail.
'$unload_file'( FileName, _F0 ) :-
    recorded('$mf','$mf_clause'(FileName,_Name,_Arity, Module,ClauseRef), R),
    erase(R),
    '$erase_clause'(ClauseRef, Module),
    fail.
'$unload_file'( FileName, _F0 ) :-
   recorded('$multifile_dynamic'(_,_,_), '$mf'(_Na,_A,_M,FileName,R), R1),
    erase(R1),
    erase(R),
    fail.
'$unload_file'( FileName, _F0 ) :-
    recorded('$multifile_defs','$defined'(FileName,_Name,_Arity,_Mod), R),
    erase(R),
    fail.
'$unload_file'( FileName, _F0 ) :-
    recorded('$module','$module'( FileName, Mod, _SourceF, _, _), R),
    erase( R ),
    unload_module(Mod),
    fail.
'$unload_file'( FileName, _F0 ) :-
    recorded('$directive','$d'( FileName, _M:_G, _Mode,  _VL, _Pos ), R),
    erase(R),
    fail.



/**

@defgroup Conditional_Compilation Conditional Compilation

@ingroup  YAPCompilerSettings

@{
 Conditional compilation builds on the same principle as
term_expansion/2, goal_expansion/2 and the expansion of
grammar rules to compile sections of the source-code
conditionally. One of the reasons for introducing conditional
compilation is to simplify writing portable code.


Note that these directives can only be appear as separate terms in the
  input.  Typical usage scenarios include:


    Load different libraries on different dialects

    Define a predicate if it is missing as a system predicate

    Realise totally different implementations for a particular
part of the code due to different capabilities.

    Realise different configuration options for your software.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- if(test1).
section_1.
:- elif(test2).
section_2.
:- elif(test3).
section_3.
:- else.
section_else.
  :- endif.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/

/** @pred    if( : _Goal_)

  Compile subsequent code only if  _Goal_ succeeds.  For enhanced
portability,  _Goal_ is processed by `expand_goal/2` before execution.
If an error occurs, the error is printed and processing proceeds as if
 _Goal_ has failed.

*/
%
% This is complicated because of embedded ifs.
%
'$if'(_,top) :- !, fail.
'$if'(_Goal,_) :-
   '__NB_getval__'('$if_level',Level0,Level=0),
   Level is Level0 + 1,
   nb_setval('$if_level',Level),
   ( '__NB_getval__'('$endif', OldEndif, fail) -> true ; OldEndif=top),
   ( '__NB_getval__'('$if_skip_mode', Mode, fail) -> true ; Mode = run ),
   nb_setval('$endif',elif(Level,OldEndif,Mode)),
   fail.
% we are in skip mode, ignore....
'$if'(_Goal,_) :-
    '__NB_getval__'('$endif',elif(Level, OldEndif, skip), fail), !,
    nb_setval('$endif',endif(Level, OldEndif, skip)).
% we are in non skip mode, check....
'$if'(Goal,_) :-
    (
	'$if_call'(Goal)
    ->
    % we will execute this branch, and later enter skip
	 '__NB_getval__'('$endif', elif(Level,OldEndif,Mode), fail),
	 nb_setval('$endif',endif(Level,OldEndif,Mode))
	;
	 % we are now in skip, but can start an elif.
	 nb_setval('$if_skip_mode',skip)
    ).

/**
@pred    else
Start `else' branch.

*/
'$else'(top) :- !, fail.
'$else'(_) :-
    '__NB_getval__'('$if_level',0,true),
    !,
    '$do_error'(context_error(no_if),(:- else)).
% we have done an if, so just skip
'$else'(_) :-
    nb_getval('$endif',endif(_Level,_,_)), !,
    nb_setval('$if_skip_mode',skip).
% we can try the elif
'$else'(_) :-
   '__NB_getval__'('$if_level',Level,Level=0),
   nb_getval('$endif',elif(Level,OldEndif,Mode)),
   nb_setval('$endif',endif(Level,OldEndif,Mode)),
   nb_setval('$if_skip_mode',run).

/** @pred   elif(+ _Goal_)


Equivalent to `:- else. :-if(Goal) ... :- endif.`  In a sequence
as below, the section below the first matching elif is processed, If
no test succeeds the else branch is processed.
*/
'$elif'(_,top) :- !, fail.
'$elif'(Goal,_) :-
    '__NB_getval__'('$if_level',0,true),
    !,
    '$do_error'(context_error(no_if),(:- elif(Goal))).
% we have done an if, so just skip
    nb_getval('$endif',endif(_,_,_)), !,
    nb_setval('$if_skip_mode',skip).
% we can try the elif
'$elif'(Goal,_) :-
  '__NB_getval__'('$if_level',Level,fail),
	'__NB_getval__'('$endif',elif(Level,OldEndif,Mode),fail),
	('$if_call'(Goal)
	    ->
% we will not skip, and we will not run any more branches.
		nb_setval('$endif',endif(Level,OldEndif,Mode)),
		nb_setval('$if_skip_mode',run)
	;
% we will (keep) on skipping
	nb_setval('$if_skip_mode',skip)
	).
'$elif'(_,_).

/** @pred    endif
End of conditional compilation.

*/
'$endif'(top) :- !, fail.
'$endif'(_) :-
% unmmatched endif.
    '__NB_getval__'('$if_level',0,true),
    !,
   '$do_error'(context_error(no_if),(:- endif)).
'$endif'(_) :-
% back to where you belong.
    '__NB_getval__'('$if_level',Level,Level=0),
    nb_getval('$endif',Endif),
    Level0 is Level-1,
    nb_setval('$if_level',Level0),
    arg(2,Endif,OldEndif),
    arg(3,Endif,OldMode),
    nb_setval('$endif',OldEndif),
    nb_setval('$if_skip_mode',OldMode).


'$if_call'(G) :-
	catch('$eval_if'(G), E, (print_message(error, E), fail)).

'$eval_if'(Goal) :-
	'$expand_term'(Goal,TrueGoal),
	once(TrueGoal).

'$if_directive'((:- if(_))).
'$if_directive'((:- else)).
'$if_directive'((:- elif(_))).
'$if_directive'((:- endif)).


'$comp_mode'( OldCompMode, CompMode) :-
	var(CompMode), !,
	'$fetch_comp_status'( OldCompMode ).
'$comp_mode'(OldCompMode, assert_all) :-
!,
	'$fetch_comp_status'(OldCompMode),
	nb_setval('$assert_all',on).
'$comp_mode'(OldCompMode, source) :-
    !,
	'$fetch_comp_status'(OldCompMode),
	set_prolog_flag(source, true).
'$comp_mode'(OldCompMode, compact) :-
	'$fetch_comp_status'(OldCompMode),
	set_prolog_flag(source, false).

'$fetch_comp_status'(assert_all) :-
	'__NB_getval__'('$assert_all',on, fail), !.
'$fetch_comp_status'(source) :-
	 current_prolog_flag(source, true), !.
'$fetch_comp_status'(compact).

/** consult_depth(-int:_LV_)
 *
 * Unify _LV_ with the number of files being consulted.
 */
consult_depth(LV) :- '$show_consult_level'(LV).

prolog_library(File) :-
    yap_flag(verbose_load,Old,false),
    ensure_loaded(library(File)),
    yap_flag(verbose_load,_,Old).

'$full_filename'(File0,File) :-
	absolute_file_name(File0,[access(read),file_type(prolog),file_errors(fail),solutions(first),expand(true)],File).

:- '$add_multifile'(dot_qualified_goal,1,user).


/**

  @}
*/
