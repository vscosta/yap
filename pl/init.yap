/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
**	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		init.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	initializing the full prolog system			 *
*									 *
*************************************************************************/

/**
  * Initialise a Prolog engine.
  *
  * Must be called after restoring.
  */
init_prolog :-
    % do catch as early as possible
    '$init_preds',
    '$bootstrap_globals',
    % simple trick to find out if this is we are booting from Prolog.
    % boot from a saved state
    '$bootstrap',
    '$db_clean_queues'(0),
    '$version',
				% this must be executed from C-code.
    %	'$bootstrap_saved_state',
    set_input(user_input),
    set_output(user_output),
    '$init_or_threads',
    '$run_at_thread_start'.



'$bootstrap_globals' :-
    working_directory(D,D),
    nb_setval(parent_directory,D),
    set_prolog_flag(open_expands_filename, true),
    set_prolog_flag(file_errors, false),
    set_prolog_flag(verbose_file_search, false),
%    set_prolog_flag( source_mode, true),
    %set_prolog_flag(file_name_variables, OldF),
    set_prolog_flag(optimise, true ),
    nb_setval('$assert_all',off),
    nb_setval('$initialization_goals',off),
    nb_setval('$included_file',[]),
    \+ '$undefined'('$init_preds',prolog),
	% set_prolog_flag(break_level, 0),
	% '$set_read_error_handler'(error), let the user do that
    set_value('$gc',on),
    ('$exit_undefp' -> true ; true),
    prompt1(' ?- '),
    set_prolog_flag(debug, false),
	nb_setval('$chr_toplevel_show_store',false).


'$init_win_graphics' :-
    '$undefined'(window_title(_,_), system), !.
'$init_win_graphics' :-
    load_files([library(win_menu)], [silent(true),if(not_loaded)]),
    fail.
'$init_win_graphics'.

'$init_or_threads' :-
	'$c_yapor_workers'(W), !,
	'$start_orp_threads'(W).
'$init_or_threads'.

'$start_orp_threads'(1) :- !.
'$start_orp_threads'(W) :-
	thread_create('$c_worker',_,[detached(true)]),
	W1 is W-1,
	'$start_orp_threads'(W1).

'$version' :-
	'$version_specs'(Specs),
	print_message(informational, Specs).

'$version_specs'(version(YAP,VersionGit,AT,Saved)) :-
      current_prolog_flag(version_git,VersionGit),
      current_prolog_flag(compiled_at,AT),
      current_prolog_flag(version_data, YAP),
      current_prolog_flag(resource_database, Saved ).


%
% reconsult at bootstrap...
%
'$bootstrap' :-
    '$init_win_graphics',
    fail.
% myddas support
'$bootstrap' :-
	get_value('$myddas_goal',GA), GA \= [],
	set_value('$myddas_goal',[]),
	get_value('$myddas_user',User), User \= [],
	set_value('$myddas_user',[]),
	get_value('$myddas_db',Db), Db \= [],
	set_value('$myddas_db',[]),
	get_value('$myddas_host',HostT),
	( HostT \= [] ->
	  Host = HostT,
	  set_value('$myddas_host',[])
	;
	  Host = localhost
	),
	get_value('$myddas_pass',PassT),
	( PassT \= [] ->
	  Pass = PassT,
	  set_value('$myddas_pass',[])
	;
	  Pass = ''
	),
	use_module(library(myddas)),
	call(db_open(mysql,myddas,Host/Db,User,Pass)),
	'$myddas_import_all',
	fail.
% use if we come from a save_program and we have SWI's shlib
'$bootstrap' :-
	current_prolog_flag(hwnd, _HWND),
	load_files(library(win_menu), [silent(true)]),
	fail.
% fetch foreign
'$bootstrap' :-
	recorded('$reload_foreign_libraries',_G,R),
	erase(R),
	shlib:reload_foreign_libraries,
	fail.
% commands from the restore file go first:
% files to load,
'$bootstrap' :-
	recorded('$restore_flag', init_file(M:B), R),
	erase(R),
	'$do_bootstrap_reconsult'(M:B),
	fail.
% what to do with unknown goals
'$bootstrap' :-
	recorded('$restore_flag', unknown(M:B), R),
	erase(R),
	set_prolog_flag(M:unknown,B),
	fail.
% goals to execute
'$bootstrap' :-
    recorded('$bootstrap_goal',G,_),
    catch(once(user:G),_Error,error_handler),
    fail.
% next, consult the user rc files.
% this should be done before -l kicks in.
'$bootstrap' :-
	current_prolog_flag(fast_boot, false),
	  ( exists('~/.yaprc') -> load_files('~/.yaprc', []) ; true ),
	  ( exists('~/.prologrc') -> load_files('~/.prologrc', []) ; true ),
	  ( exists('~/prolog.ini') -> load_files('~/prolog.ini', []) ; true ),
	  fail.
% proceed to process the -l and -L flags
'$bootstrap' :-
	get_value('$consult_on_boot',X), X \= [],
	set_value('$consult_on_boot',[]),
	catch(load_files(user:X, [silent(true)]), _Error, error_handler),
	get_value('$top_level_goal',[]),
	!,
	( current_prolog_flag(halt_after_consult, false) -> true ; halt(0)),
	fail.
%  follow by the -g goal, if there.
'$bootstrap' :-
    get_value('$init_goal',GA),
    GA \= [],
    set_value('$init_goal',[]),
    '$run_atom_goal'(GA),
    fail.
% the -z goal
'$bootstrap' :-
    get_value('$top_level_goal',GA),
    GA \= [],
    set_value('$top_level_goal',[]),
    '$run_atom_goal'(GA),
    fail.
'$bootstrap' :-
    '__NB_getval__'('$top_level_goal',G,fail),
    G \= [],
    nb_setval('$top_level_goal',[]),
    catch(once(G),_Error,error_handler),
    fail.
'$bootstrap'.


 %
 % MYDDAS: Import all the tables from one database
 %

 '$myddas_import_all':-
	 call(db_my_show_tables(myddas,table(Table))),
	 call(db_import(myddas,Table,Table)),
	 fail.
 '$myddas_import_all'.


'$init_path_extensions' :-
	get_value('$extend_file_search_path',P), !,
	P \= [],
	set_value('$extend_file_search_path',[]),
	'$extend_file_search_path'(P).
'$init_path_extensions'.

/*
 initialization is called at the end of its file
*/


/** @pred initialization(+ _G_) is iso

Theu compiler will execute goals  _G_ after consulting the current
file. Only the first answer is
considered.

Notice that the goal will execute in the calling context, not within the file context,
In other words, the source module and execution directory will be the ones of the parent
environment. Use initialization/2 for more flexible behavior.

*/



'$initialization'(G) :-
    '$initialization'( G, after_load ).



/** @pred initialization(+ _Goal_,+ _When_)

Similar to initialization/1, but allows  specifying when
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


% then we can execute the programs.
'$initialization_goals' :-
    module(user),
    set_prolog_flag(optimise, true),
	recorded('$blocking_code',_LC,R),
	erase(R),
	fail.

% system goals must be performed first
'$initialization_goals'(LC) :-
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
