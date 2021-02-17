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
%
% SWI options
% autoload(true,false)
% derived_from(File) -> make
% encoding(Encoding) => implemented
% expand(true,false)
% if(changed,true,not_loaded) => implemented
% imports(all,List) => implemented
% qcompile() => implemented
% silent(true,false)  => implemented
% stream(Stream)  => implemented
% consult(consult,reconsult,exo,db) => implemented
% compilation_mode(compact,source,assert_all) => implemented
% register(true, false) => implemented
%
load_files(Files0,Opts) :-
    '$yap_strip_module'(Files0,M,Files),
    '$load_files'(Files,M,Opts,M:load_files(Files,Opts)).


'$lf_option'(autoload, 1, false).
'$lf_option'(derived_from, 2, false).
'$lf_option'(encoding, 3, default).
'$lf_option'(expand, 4, false).
'$lf_option'(if, 5, true).


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
