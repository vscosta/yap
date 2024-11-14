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
:- system_module_( '$_consult', [compile/1,
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

:- use_system_module( '$_errors', [throw_error/2]).

:- use_system_module( '$_load_foreign', ['$import_foreign'/3]).

:- use_system_module( '$_modules', ['$add_to_imports'/3,
        '$convert_for_export'/6,
        '$extend_exports'/3]).

:- use_system_module( '$_preds', ['$current_predicate'/4]).

/**



  @defgroup YAPConsulting Loading files into YAP
  @ingroup  YAPProgramming

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
	 skip_list(_, Fs ,L),
	 L \== [],
	 !,
	 python:python_proc( Fs ) .
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

%% @}

/** @defgroup  YAPCompilerSettings Directing and Configuring the Compiler
    @ingroup  YAPConsulting

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
    recorded('$multifile_defs','$defined'(FileName,Name,Arity,Module),R1),
    '$erasell_multifile'(FileName,Module,Name,Arity),
	erase(R1),
	fail.
'$remove_multifile_clauses'(_).

'$initialization'(G) :-
    '$initialization'( G, after_load ).



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
	    _Error,
	    error_handler 
	   ) ->
      true ;
      format(user_error,':- ~w failed.~n',[G])
    )
    ;
    OPT == after_load
    ->
        '__NB_getval__'('$consulting_file', LC, fail),
    strip_module(G,M,H),
    recordz('$initialization_queue',q(LC,M:H),_)
	;
	 OPT == restore
	->
    recordz('$call_at_restore', G, _ )
    ).


'$exec_initialization_goals'(_LC) :-
    set_prolog_flag(optimise, true),
	recorded('$blocking_code',_LC,R),
	erase(R),
	fail.
% system goals must be performed first
'$exec_initialization_goals'(LC) :-
    recorded('$initialization_queue',q(LC,G),R),
	'$conditional_compilation_get_state'(State),
	'$conditional_compilation_init',
	 erase(R),
	(catch(
	 (G),
	 _E,
	 error_handler
	 )
	->
	    	 true %format(user_error,':- ~w ok.~n',[G]),
	;
  	 format(user_error,':- ~q failed.~n',[G])
	 ),
	'$conditional_compilation_set_state'(State),
	fail.
'$exec_initialization_goals'(_).

%
% reconsult at startup...
%
'$do_startup_reconsult'(_X) :-
    '$init_win_graphics',
    fail.
'$do_startup_reconsult'(X) :-
    catch(load_files(user:X, [silent(true)]), _Error, error_handler),
  % still need to run -g or -z
    get_value('$top_level_goal',[]),
    !,
    ( current_prolog_flag(halt_after_consult, false) -> true ; halt(0)).
'$do_startup_reconsult'(_) .

'$skip_unix_header'(Stream) :-
	peek_code(Stream, 0'#), !, % 35 is ASCII for '#
	skip(Stream, 10),
	'$skip_unix_header'(Stream).
'$skip_unix_header'(_).

/**
@pred source_file(?FileName)

SWI-compatible predicate. _FileName_ is the absolute and canonical path for a loaded source file
*/ 
source_file(FileName) :-
	'$source_file'(FileName, __).

/**
@pred source_file(:Pred,?File	)

SWI-compatible predicate. True if the predicate specified by  _Pred_ was loaded from file  _File_, where  _File_ is an absolute path name (see absolute_file_name/2).
*/ 
source_file(Mod:Pred, FileName) :-
	current_module(Mod),
	Mod \= prolog,
	current_predicate(_,Mod:Pred),
	'$owned_by'(Pred, Mod, FileName).

'$owned_by'(T, Mod, FileName) :-
	functor(T, Name, Arity),
	setof(FileName, Ref^recorded('$multifile_defs','$defined'(FileName,Name,Arity,Mod), Ref), L),
        member(FileName, L).
'$owned_by'(T, Mod, FileName) :-
	'$owner_file'(T, Mod, FileName).


/**
@pred source_file_property(?Pred,?Property)

_FileName_ is the absolute and canonical path source  for the source file that
has  the property


*/
source_file_property( F, includes(I)) :-
        recorded('$includes',(I->F), _).
source_file_property( I, included_from(F)) :- 
       recorded('$includes',(I->F), _).

source_file_property( F, load_context(M,OldF:Line,Opts)) :-
	recorded('$lf_loaded','$lf_loaded'( F, M, _Reconsult, _UserFile, OldF, Line, Opts), _).

/** @pred prolog_load_context(? _Key_, _Value_)
 * 
 *

  Obtain information on the current compilation process. 


  The predicate allows the
  following keys:
    
  + `directory`  (prolog_load_context/2 option)

  Full name for the directory where YAP 		is currently consulting the
  file.

  + file  (prolog_load_context/2 option)

  Full name for the file currently being consulted. Notice that included
  filed are ignored.

  + module  (prolog_load_context/2 option)

  Current source module.

  + `source` (prolog_load_context/2 option)

  Full name for the file currently being read in, which may be consulted,
  reconsulted, or included

2  + `stream`  (prolog_load_context/2 option)

  Stream currently being read in.

  + `term_position`  (prolog_load_context/2 option)

  Stream position at the stream currently being read in. For SWI
  compatibility, it is a term of the form
  '$stream_position'(0,Line,0,0).

   + `term`   (prolog_load_context/2 option)

   The term being processed, if any.

   + `variable_names`   (prolog_load_context/2 option)

   The names of the variables in the term being processed, if any.
*/
prolog_load_context(directory, DirName) :-
        ( source_location(F, _)
        -> file_directory_name(F, DirName) ;
          working_directory( DirName, DirName )
        ).
prolog_load_context(source, SourceName) :-
    '__NB_getval__'('$consulting_file', FileName, fail),
    (
        recorded('$includes',(FileName->SourceName), _) 
    ->
    true
    ;
    FileName = SourceName
    ),
    !.
prolog_load_context(source, user_input).

prolog_load_context(module, X) :-
        '__NB_getval__'('$consulting_file', _, fail),
        current_source_module(Y,Y),
        Y = X.
prolog_load_context(file, Term ) :-
    b_getval('$current_clause', T),
    nonvar(T),
    T = [_,_,Term|_].
prolog_load_context(stream, Stream) :-
    stream_property(Stream, alias(loop_stream) ).
prolog_load_context(term, Term ) :-
    b_getval('$current_clause', T),
    nonvar(T),
    T = [Term|_].
prolog_load_context(term_position, Term ) :-
    (
    b_getval('$current_clause', T),
    nonvar(T) ->
    T = [_,_,_,Term]
    ;
    predicate_property( user_input, position(Term))
    ).
prolog_load_context(variable_names, Term ) :-
    (
    b_getval('$current_clause', T),
    nonvar(T)
    ->
    T = [_,Term|_]
    ;
    Term = []
    ).

% if the file exports a module, then we can
% be imported from any module.
'$file_loaded'(F0, TargetModule, M) :-
    (
	atom_concat(Prefix, '.qly', F0 );
	Prefix=F0
    ),
    (
	absolute_file_name(Prefix,F,[access(read),file_type(qly),file_errors(fail),solutions(first),expand(true)])
    ;
    F0 = F
    ),

    '$ensure_file_loaded'(F,TargetModule, M).

'$ensure_file_loaded'(F, TargetModule,   NM) :-
    % make sure: it either defines a new module or it was loaded in the same context
    ('$module'(F,NM,_P,_) ->
	 true
    ;
    % loaded from the same module, but does not define a module.
    '$source_file'(F, NM), % that does not define a module
    NM==TargetModule
    ).

% if the file exports a module, then we can
% be imported from any module.
'$file_unchanged'(F0, TargetModule, NM) :-
    '$file_loaded'(F0, TargetModule, NM),
    '$source_file'(F0, Age),
    time_file64(F0,CurrentAge),
    ( Age == CurrentAge ; Age = -1).

	% inform the file has been loaded and is now available.
'$tell_loaded'(F, UserFile, M, OldF, Line, Reconsult0, Reconsult, Dir, _TOpts, Opts) :-
    prolog_load_context(directory, Dir),
    b_setval('$consulting_file', F ),
	(
	 % if we are reconsulting, always start from scratch
	 Reconsult0 \== consult,
	 Reconsult0 \== not_loaded,
	 Reconsult0 \== changed,
%	 retractall('$source_file'(F, _)),
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
	(catch(time_file64(F, Age),_,fail) -> true ; Age = 0),
	assert('$source_file'(F,Age)),
	% modules are logically loaded only once

	(
	    '$module'(F,_DonorM, _AllExports, _Line) ->
	    true
	;

	recorda('$lf_loaded','$lf_loaded'( F, M, Reconsult, UserFile, OldF, Line, Opts), _)
	).

/** @pred make

SWI-Prolog originally included this built-in as a Prolog version of the Unix make
utility program. In this case the idea is to reconsult all source files that have been changed since they were originally compiled into Prolog. YAP has a limited implementation of make/0 that
just goes through every loaded file and verifies whether reloading is needed.

*/

make :-
	recorded('$lf_loaded','$lf_loaded'(F1,_M,reconsult,_,_,_,_),_),
	load_files(F1, [if(changed)]),
	fail.
make.

unload_file(user) :-
    !,
    '$unload_file'(user_input).
unload_file(user_input) :-
    !,
    '$unload_file'(user_input).
unload_file(F) :-
    absolute_file_name(F,[access(read),file_type(prolog),file_errors(fail),solutions(first),expand(true)],File),
    unload_file_(File).


unload_file_(File) :-
     recorded('$lf_loaded','$lf_loaded'(File,_M,_,_,_,_,_),_),
     !,
    '$unload_file'(File).
unload_file_(_).

'$unload_file'(File) :-
    current_predicate(N,M:P),
    M \= prolog,
    M \= idb,
    (
    '$is_multifile'(P,M) 
    ->
	clause(M:P,_,R),
	clause_property(R,file(File)),
	'$erase_clause'(R,M)
	;
      predicate_property(M:P, dynamic)
	->
	clause(M:P,_,R),
	clause_property(R,file(File)),
	erase(R)
	;
	predicate_property(M:P, file(File)),
    functor(P,N,A),
	abolish(M:N/A)
	),
	fail.
'$unload_file'(File) :-
    '$module'(File,DonorM, _AllExports, _),
    DonorM \= prolog,
    DonorM \= user,
    unload_module(DonorM),
    fail.
'$unload_file'(File) :-
	retractall('$source_file'(File,_Age)),
	fail.
'$unload_file'(File) :-
	recorded('$lf_loaded','$lf_loaded'(File,_M,_,_,_,_,_),R),
	erase(R),
	fail.
'$unload_file'(_F).

'$fetch_stream_alias'(OldStream,Alias) :-
	stream_property(OldStream, alias(Alias)), !.

'$require'(_Ps, _M).

'$store_clause'('$source_location'(File, _Line,_,_),Clause, File) :-
	assert_static(Clause).

%% @}

/**

@defgroup Conditional_Compilation Conditional Compilation

@ingroup  YAPConsulting

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

```
:- if(test1).
section_1.
:- elif(test2).
section_2.
:- elif(test3).
section_3.
:- else.
section_else.
  :- endif.
```

*/

'$if_directive'(if(Goal), M, _VL, _Pos, Option) :- 
    '$if'(M:Goal, Option).
'$if_directive'(elif(Goal), M, _VL, _Pos, Option) :-
    '$elif'(M:Goal, Option).
'$if_directive'(else, _M, _VL, _Pos, Option) :-
    '$else'( Option).
'$if_directive'(endif, _M, _VL, _Pos, Option) :-
    '$endif'( Option).

/** @pred    if( : _Goal_)

  Compile subsequent code only if  _Goal_ succeeds.  For enhanced
portability,  _Goal_ is processed by `expand_goal/2` before execution.
If an error occurs, the error is printed and processing proceeds as if
 _Goal_ has failed.

*/
%
% This is complicated because of embedded ifs.
%
'$if'(Goal) :-
    must_be_callable(Goal),
    '$conditional_compilation'(Inp),
    (Inp == skip
   ->
       Mode=done
  ;
    Inp == done
    ->
    Mode=done
    ;
	call(Goal)
    ->
    Mode = run
    ;
    Mode = skip
    ),
    '$conditional_compilation_push'(Mode).

/**
@pred    else
Start `else' branch.

*/
'$else' :-
    '$conditional_compilation'(Inp),
    ( Inp == run
    ->
    Mode = done
    ;
    Inp == skip
    ->
    Mode = run
    ;
    Mode = done
    ),
    '$conditional_compilation_set'(Mode).



/** @pred   elif(+ _Goal_)


Equivalent to `:- else. :-if(Goal) ... :- endif.`  In a sequence
as below, the section below the first matching elif is processed, If
no test succeeds the else branch is processed.
*/
'$elif'(Goal) :-
 	 '$conditional_compilation'(Inp),
   (
   Inp == run
    ->
    Mode = done
    
    ;
	 Inp == done
    ->
    Mode = done
    ;
	call(Goal)
    ->
    Mode = run
    ;
    Mode = skip
      ),
    '$conditional_compilation_set'(Mode).

/** @pred    endif
QEnd of cond  itional compilation.

*/
'$endif' :-
    '$conditional_compilation_pop'.

%% base layer runs 
'$conditional_compilation_init':-
    nb_setval('$conditional_compilation_level',[run]).

'$conditional_compilation_get_state'(state(LB)) :-
    catch(nb_getval('$conditional_compilation_level', LB), _Undef,
('$conditional_compilation_init',
'$conditional_compilation_get_state'(state(LB)) )
).

'$conditional_compilation_set_state'(state(LB)) :-
    nb_setval('$conditional_compilation_level', LB).

'$conditional_compilation_push'(Mode) :-
    '__NB_getval__'('$conditional_compilation_level', Levels,fail),
    nb_setval('$conditional_compilation_level', [Mode|Levels]).


'$conditional_compilation'(Mode) :-
    '__NB_getval__'('$conditional_compilation_level', [Mode|_Levels], fail).


'$conditional_compilation_skip'(V)  :-
    var(V),
    !,
    '__NB_getval__'('$conditional_compilation_level', [L|_Levels], fail),
    (L == skip
    ;
    L == done),
    !.
'$conditional_compilation_skip'((:-if(G)))  :-
      '$if'(G),
      !.
'$conditional_compilation_skip'((:-elif(G)))  :-
      '$elif'(G),
!.
'$conditional_compilation_skip'((:-else))  :-
      '$else',
!.
'$conditional_compilation_skip'((:-endif))  :-
      '$endif',
!.
'$conditional_compilation_skip'(_)  :-
    '__NB_getval__'('$conditional_compilation_level', [L|_Levels], fail),
    (L == skip
    ;
    L == done),
    !.

'$conditional_compilation_set'(Mode) :-
    '__NB_getval__'('$conditional_compilation_level', [_Mode_|Levels], fail),
    nb_setval('$conditional_compilation_level', [Mode|Levels]).


'$conditional_compilation_pop' :-
    '__NB_getval__'('$conditional_compilation_level', [_|Levels], fail),
    nb_setval('$conditional_compilation_level', Levels).
    
:- '$conditional_compilation_init'.

'$if_call'(G) :-
	catch('$eval_if'(G), _E, error_handler).

'$eval_if'(Goal) :-
	expand_term(Goal,TrueGoal),
	once(TrueGoal).

'$if_directive'((if(_))).
'$if_directive'((else)).
'$if_directive'((elif(_))).
'$if_directive'((endif)).


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
    current_prolog_flag(verbose_load,Old),
    set_prolog_flag(verbose_load,false),
    ensure_loaded(library(File)),
    set_prolog_flag(verbose_load,Old).

'$full_filename'(File0,File) :-
	absolute_file_name(File0,[access(read),file_type(prolog),file_errors(fail),solutions(first),expand(true)],File).

:- multifile user:dot_qualified_goal/1.



:- dynamic '$source_file'/2, '$source_file_scope'/2.
/**

  @}
*/
