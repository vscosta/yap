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
 * @file init.yap
 *
 * @brief how to boot and run the top-level.
 *
*/

/**
 * @ingroup YAPControl
 * @{
 *
*/

'$init_globals' :-
	% set_prolog_flag(break_level, 0),
	% '$set_read_error_handler'(error), let the user do that
	nb_setval('$chr_toplevel_show_store',false).

'$init_consult' :-
    set_value('$open_expands_filename',true),
    nb_setval('$assert_all',off),
    nb_setval('$if_level',0),
    nb_setval('$endif',off),
    nb_setval('$initialization_goals',off),
    nb_setval('$included_file',[]),
    nb_setval('$loop_streams',[]),
    (
	'$undefined'('$init_preds',prolog)
    ->
    true
    ;
    '$init_preds'
	).

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
      current_prolog_flag(verbose, normal), !,
      current_prolog_flag(version_git,VersionGit),
      current_prolog_flag(compiled_at,AT),
      current_prolog_flag(version_data, yap(Mj, Mi,  Patch, _) ),
      sub_atom( VersionGit, 0, 8, _, VERSIONGIT ),
      current_prolog_flag(version_data, yap(Mj, Mi,  Patch, _) ),
      current_prolog_flag(resource_database, Saved ),
      format(user_error, '% YAP ~d.~d.~d-~a (compiled  ~a)~n', [Mj,Mi, Patch, VERSIONGIT,  AT]),
      format(user_error, '% database loaded from ~a~n', [Saved]),
      fail.
'$version'.

/**
  * Initialise a Prolog engine.
  *
  * Must be called after restoring.
  */
'$init_prolog' :-
	'$init_step'(_),
	fail.
'$init_prolog'.
				% do catch as early as possible
'$init_step'(1) :-
	'$version'.
'$init_step'(2) :-
	set_prolog_flag(file_name_variables, true),
	'$init_consult'.
				%set_prolog_flag(file_name_variables, OldF),
'$init_step'(3) :-
	'$init_globals',
	set_prolog_flag(fileerrors, true),
	set_value('$gc',on),
	('$exit_undefp' -> true ; true),
	prompt1(' ?- '),
	set_prolog_flag(debug, false).
				% simple trick to find out if this is we are booting from Prolog.
				% boot from a saved state
'$init_step'(4) :-
	'$init_from_saved_state_and_args'.

'$init_step'(5) :-
    '$db_clean_queues'(_).
				% this must be executed from C-code.
				%	'$startup_saved_state',
'$init_step'(6) :-
    set_input(user_input),
    set_output(user_output),
    '$init_or_threads',
    '$run_at_thread_start'.


% then we can execute the programs.
'$startup_goals' :-
	'$startup_step',
	fail.

'$startup_step' :-
    module(user).
'$startup_step' :- 
	recorded('$startup_goal',G,_),
	catch(once(user:G),Error,user:'$Error'(Error)).
'$startup_step' :-
	get_value('$init_goal',GA),
	GA \= [],
	set_value('$init_goal',[]),
	'$run_atom_goal'(GA).
'$startup_step' :-
	recorded('$restore_flag', goal(Module:GA), R),
	erase(R),
	catch(once(Module:GA),Error,user:'$Error'(Error)).
'$startup_step' :-
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
	'$myddas_import_all'.
'$startup_step'.

 %
 % MYDDAS: Import all the tables from one database
 %

 '$myddas_import_all':-
	 call(db_my_show_tables(myddas,table(Table))),
	 call(db_import(myddas,Table,Table)),
	 fail.
 '$myddas_import_all'.

% use if we come from a save_program and we have SWI's shlib
'$init_from_saved_state_and_args' :-
	'$rebuild',
	fail.
'$init_from_saved_state_and_args'.

'$rebuild' :-	
	current_prolog_flag(hwnd, _HWND),
	load_files(library(win_menu), [silent(true)]).
'$rebuild' :-
	recorded('$reload_foreign_libraries',_G,R),
	erase(R),
	shlib:reload_foreign_libraries.
% this should be done before -l kicks in.
'$rebuild' :-
	current_prolog_flag(fast_boot, false),
	  ( exists('~/.yaprc') -> load_files('~/.yaprc', []) ; true ),
	  ( exists('~/.prologrc') -> load_files('~/.prologrc', []) ; true ),
	  ( exists('~/prolog.ini') -> load_files('~/prolog.ini', []) ; true ),
	  fail.
% use if we come from a save_program and we have a goal to execute
'$rebuild' :-
	get_value('$consult_on_boot',X), X \= [],
	load_files(X, [silent(true)]),
	set_value('$consult_on_boot',[]).
'$rebuild' :-
	recorded('$restore_flag', init_file(M:B), R),
	erase(R),
	load_files(M:B, [silent(true)]).
'$rebuild' :-
	recorded('$restore_flag', unknown(M:B), R),
	erase(R),
	load_files(M:B, [silent(true)]),
	yap_flag(M:unknown,B).
'$rebuild' :-
	'$startup_step'.
'$rebuild' :-
	current_prolog_flag(halt_after_consult, true),
	halt.
'$rebuild' :-
	recorded('$restore_goal',G,R),
	erase(R),
	prompt(_,'| '),
	catch(once(user:G),Error,user:'$Error'(Error)).

'$init_path_extensions' :-
	get_value('$extend_file_search_path',P), !,
	P \= [],
	set_value('$extend_file_search_path',[]),
	'$extend_file_search_path'(P).
'$init_path_extensions'.


/**
 * @pred top_query(?0 Goal).
 *
 * run _Goal_ as f it had been called from the Prolog
 * top-level.
 */
top_query(G)  :-
    '$init_step'(2), % consult
    '$init_step'(3), % globals
    '$init_step'(4), % check if a saved state,
    '$init_step'(5), % queues
    '$init_step'(6), % I/O, threads.
	'$alarm'(0, 0, _, _),
	'$clean_up_dead_clauses',
    flush_output,
	'$run_toplevel_hooks',
	prompt1(' ?- '),
	nb_setval('$spy_gn',1),
				% stop at spy-points if debugging is on.
	nb_setval('$debug_run',off),
	nb_setval('$debug_jump',off),
	'__NB_setval__'('$trace',off),
	'$set_debugger_state'( zip, 0, stop,off, false),
	set_prolog_flag(break_level, 0),
	catch(user:G,  Error, '$Error'(Error)).


/**
 *
 * @}
 */
