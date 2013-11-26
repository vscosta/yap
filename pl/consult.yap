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
* File:		consult.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Consulting Files in YAP					 *
*									 *
*************************************************************************/

%
% SWI options
% autoload(true,false)
% derived_from(File) -> make
% encoding(Encoding) => implemented
				% expand(true,false)
% if(changed,true,not_loaded) => implemented
% imports(all,List) => implemented
% qcompile(true,false)
% silent(true,false)  => implemented
% stream(Stream)  => implemented
% consult(consult,reconsult,exo,db) => implemented
% compilation_mode(compact,source,assert_all) => implemented
% register(true, false) => implemented
%
load_files(Files,Opts) :-
	'$load_files'(Files,Opts,load_files(Files,Opts)).

'$lf_option'(autoload, 1, _).
'$lf_option'(derived_from, 2, false).
'$lf_option'(encoding, 3, default).
'$lf_option'(expand, 4, false).
'$lf_option'(if, 5, true).
'$lf_option'(imports, 6, all).
'$lf_option'(qcompile, 7, never).
'$lf_option'(silent, 8, _).
'$lf_option'(skip_unix_header, 9, false).
'$lf_option'(compilation_mode, 10, source).
'$lf_option'(consult, 11, reconsult).
'$lf_option'(stream, 12, _).
'$lf_option'(register, 13, true).
'$lf_option'('$files', 14, _).
'$lf_option'('$call', 15, _).
'$lf_option'('$use_module', 16, _).
'$lf_option'('$consulted_at', 17, _).
'$lf_option'('$options', 18, _).
'$lf_option'('$location', 19, _).
'$lf_option'(dialect, 20, yap).
'$lf_option'(format, 21, source).
'$lf_option'(redefine_module, 22, ask).
'$lf_option'(reexport, 23, false).
'$lf_option'(sandboxed, 24, false).
'$lf_option'(scope_settings, 25, false).
'$lf_option'(modified, 26, _).
'$lf_option'('$context_module', 27, _).
'$lf_option'('$parent_topts', 28, _).
'$lf_option'(must_be_module, 29, false).

'$lf_option'(last_opt, 29).

'$lf_opt'( Op, TOpts, Val) :-
	'$lf_option'(Op, Id, _),
	arg( Id, TOpts, Val ).

'$load_files'(Files, Opts, Call) :-
	( '$nb_getval'('$lf_status', OldTOpts, fail), nonvar(OldTOpts) ->
	  '$lf_opt'(silent, OldTOpts, OldVerbosity),
	  '$lf_opt'(autoload, OldTOpts, OldAutoload)
	;
	  true ),
	'$check_files'(Files,load_files(Files,Opts)),
	'$lf_option'(last_opt, LastOpt),
	functor( TOpts, opt, LastOpt ),
	( source_location(ParentF, Line) -> true ; ParentF = user_input, Line = -1 ),
	'$lf_opt'('$location', TOpts, ParentF:Line),
	'$lf_opt'('$files', TOpts, Files),
	'$lf_opt'('$call', TOpts, Call),
	'$lf_opt'('$options', TOpts, Opts),
	'$lf_opt'('$parent_topts', TOpts, OldTOpts),
	'$process_lf_opts'(Opts,TOpts,Files,Call),
	'$lf_default_opts'(1, LastOpt, TOpts),
	'$check_use_module'(Call,UseModule),
	'$lf_opt'('$use_module', TOpts, UseModule),
        '$current_module'(M0),
	( '$lf_opt'(silent, TOpts, Verbosity),
	  var(Verbosity) ->
	  Verbosity = OldVerbosity
	;
	  true
	),
	( '$lf_opt'(autoload, TOpts, Autoload),
	  var(Autoload) ->
	  Autoload = OldAutoload
	;
	  true
	),
	% make sure we can run consult
	'$init_system',
	'$lf'(Files, M0, Call, TOpts).

'$check_files'(Files, Call) :-
	var(Files), !,
	'$do_error'(instantiation_error, Call).
'$check_files'(M:Files, Call) :- !,
	(var(M)
	->
	'$do_error'(instantiation_error, Call)
	;
	 atom(M)
	->
	 '$check_files'(Files,Call)
	;
	 '$do_error'(type_error(atom,M), Call)
	).
'$check_files'(Files, Call) :-
	( ground(Files)
	->
	 true
	;
	'$do_error'(instantiation_error, Call)
	).

'$process_lf_opts'(V, _, _, Call) :-
	var(V), !,
	'$do_error'(instantiation_error,Call).
'$process_lf_opts'([], _, _, _).
'$process_lf_opts'([Opt|Opts],TOpt,Files,Call) :-
	Opt =.. [Op, Val],
	ground(Val),
	'$lf_opt'(Op, TOpt, Val),
	'$process_lf_opt'(Op, Val,Call), !, 
	'$process_lf_opts'(Opts, TOpt, Files, Call).
'$process_lf_opts'([Opt|_],_,_,Call) :-
	'$do_error'(domain_error(unimplemented_option,Opt),Call).

'$process_lf_opt'(autoload, Val, Call) :-
	( Val == false -> true ;
	    Val == true -> true ;
	    '$do_error'(domain_error(unimplemented_option,autoload(Val)),Call) ).
'$process_lf_opt'(derived_from, File, Call) :-
	( atom(File) -> true ;  '$do_error'(type_error(atom,File),Call) ).
'$process_lf_opt'(encoding, Encoding, _Call) :-
	atom(Encoding).
'$process_lf_opt'(expand, Val, Call) :-
	( Val == true -> '$do_error'(domain_error(unimplemented_option,expand),Call) ;
	    Val == false -> true ;
	    '$do_error'(domain_error(unimplemented_option,expand(Val)),Call) ).
'$process_lf_opt'(if, If, Call) :-
	( If == changed -> true ;
	    If == true -> true ;
	    If == not_loaded -> true ;
	    '$do_error'(domain_error(unimplemented_option,if),Call) ).
'$process_lf_opt'(imports, Val, Call) :-
	( Val == all -> true ;
	    var(Val) -> Val = all ;
	    is_list(Val) -> ( ground(Val) -> true ; '$do_error'(instantiation_error,Call) ) ;
	    '$do_error'(domain_error(unimplemented_option,imports(Val)),Call) ).
'$process_lf_opt'(qcompile, Val,Call) :-
	( Val == true -> '$do_error'(domain_error(unimplemented_option,expand),Call) ;
	    Val == false -> true ;
	    '$do_error'(domain_error(unimplemented_option,expand(Val)),Call) ).
'$process_lf_opt'(silent, Val, Call) :-
	( Val == false -> true ;
	    Val == true -> true ;
	    '$do_error'(domain_error(unimplemented_option,silent(Val)),Call) ).
'$process_lf_opt'(skip_unix_header, Val, Call) :-
	( Val == false -> true ;
	    Val == true -> true ;
	    '$do_error'(domain_error(unimplemented_option,skip_unix_header(Val)),Call) ).
'$process_lf_opt'(compilation_mode, Val, Call) :-
( Val == source -> true ;
	    Val == compact -> true ;
	    Val == assert_all -> true ;
	    '$do_error'(domain_error(unimplemented_option,compilation_mode(Val)),Call) ).
'$process_lf_opt'(consult, Val , Call) :-
	( Val == reconsult -> true ;
	    Val == consult -> true ;
	    Val == exo -> true ;
	    Val == db -> true ;
	    '$do_error'(domain_error(unimplemented_option,consult(Val)),Call) ).
'$process_lf_opt'(reexport, Val , Call) :-
	( Val == true -> true ;
	    Val == false -> true ;
	    '$do_error'(domain_error(unimplemented_option,reexport(Val)),Call) ).
'$process_lf_opt'(must_be_module, Val , Call) :-
	( Val == true -> true ;
	    Val == false -> true ;
	    '$do_error'(domain_error(unimplemented_option,must_be_module(Val)),Call) ).
'$process_lf_opt'(stream, Val, Call) :-
	( current_stream(Val) -> true ;
	    '$do_error'(type_error(stream,Val),Call) ).
'$process_lf_opt'(register, Val, Call) :-
	( Val == false -> true ;
	    Val == true -> true ;
	    '$do_error'(domain_error(unimplemented_option,register(Val)),Call) ).

'$lf_default_opts'(I, LastOpt, _TOpts) :- I > LastOpt, !.
'$lf_default_opts'(I, LastOpt, TOpts) :-
	I1 is I+1,
	arg(I, TOpts, A),
	( nonvar(A) -> true ;
	  '$lf_option'(_Name, I, A)
	),
	'$lf_default_opts'(I1, LastOpt, TOpts).


	  
'$check_use_module'(use_module(_), use_module(_)) :- !.
'$check_use_module'(use_module(_,_), use_module(_)) :- !.
'$check_use_module'(use_module(M,_,_), use_module(M)) :- !.
'$check_use_module'(_, load_files) :- !.

'$lf'(V,_,Call, _ ) :- var(V), !,
	'$do_error'(instantiation_error,Call).
'$lf'([], _, _, _) :- !.
'$lf'(M:X, _, Call, TOpts) :- !,
	(
	  atom(M)
	->
	  '$lf'(X, M, Call, TOpts)
	  ;
	  '$do_error'(type_error(atom,M),Call)
	).
'$lf'([F|Fs], Mod, Call, TOpts) :- !,
	% clean up after each consult
	( '$lf'(F,Mod,Call, TOpts), fail ;
	  '$lf'(Fs, Mod, Call, TOpts) ).
'$lf'(user, Mod, _, TOpts) :- !,
	'$do_lf'(Mod, user_input, user_input, TOpts).
'$lf'(user_input, Mod, _, TOpts) :- !,
	'$do_lf'(Mod, user_input, user_input, TOpts).
'$lf'(File, Mod, Call, TOpts) :-
	'$lf_opt'(stream, TOpts, Stream),
	( var(Stream) ->
	  /* need_to_open_file */
	  '$full_filename'(File, Y, Call),
	  open(Y, read, Stream)
        ;
	  true
        ), !,
	'$lf_opt'(if, TOpts, If),
	( var(If) -> If = true ; true ),
	'$lf_opt'(imports, TOpts, Imports),
	'$start_lf'(If, Mod, Stream, TOpts, File, Imports),
	close(Stream).
'$lf'(X, _, Call, _) :-
	'$do_error'(permission_error(input,stream,X),Call).

'$start_lf'(not_loaded, Mod, Stream, TOpts, UserFile, Imports) :-
	'$file_loaded'(Stream, Mod, Imports, TOpts), !,
	'$lf_opt'('$options', TOpts, Opts),
	'$lf_opt'('$location', TOpts, ParentF:Line),
	'$loaded'(Stream, UserFile, Mod, ParentF, Line, not_loaded, _File, _Dir, Opts).
'$start_lf'(changed, Mod, Stream, TOpts, UserFile, Imports) :-
	'$file_unchanged'(Stream, Mod, Imports, TOpts), !,
	'$lf_opt'('$options', TOpts, Opts),
	'$lf_opt'('$location', TOpts, ParentF:Line),
	'$loaded'(Stream, UserFile, Mod, ParentF, Line, changed, _File, _Dir, Opts).
'$start_lf'(_, Mod, Stream, TOpts, File, _) :-
	'$do_lf'(Mod, Stream, File, TOpts).

ensure_loaded(Fs) :-
	'$load_files'(Fs, [if(not_loaded)],ensure_loaded(Fs)).

compile(Fs) :-
	'$load_files'(Fs, [], compile(Fs)).

% consult(Fs) :-
% 	'$has_yap_or',
% 	'$do_error'(context_error(consult(Fs),clause),query).
consult(V) :-
	var(V), !,
	'$do_error'(instantiation_error,consult(V)).
consult(M0:Fs) :- !,
	'$consult'(Fs, M0).
consult(Fs) :-
	'$current_module'(M0),
	'$consult'(Fs, M0).

'$consult'(Fs,Module) :-
	'$access_yap_flags'(8, 2), % SICStus Prolog compatibility
	!,
	'$load_files'(Module:Fs,[],consult(Fs)).
'$consult'(Fs, Module) :-
	'$load_files'(Module:Fs,[consult(consult)],consult(Fs)).

reconsult(Fs) :-
	'$load_files'(Fs, [], reconsult(Fs)).

exo_files(Fs) :-
	'$load_files'(Fs, [consult(exo), if(not_loaded)], exo_files(Fs)).

db_files(Fs) :-
	'$load_files'(Fs, [consult(db), if(not_loaded)], exo_files(Fs)).

use_module(F) :-
	'$load_files'(F, [if(not_loaded),must_be_module(true)], use_module(F)).

use_module(F,Is) :-
	'$load_files'(F, [if(not_loaded),must_be_module(true),imports(Is)], use_module(F,Is)).

use_module(M,F,Is) :-
	'$use_module'(M,F,Is).

'$use_module'(M,F,Is) :- nonvar(M), !,
	recorded('$module','$module'(F1,M,_,_),_),
	'$load_files'(F1, [if(not_loaded),must_be_module(true),imports(Is)], use_module(M,F,Is)),
	( F1 = F -> true ; true ).
'$use_module'(M,F,Is) :-
	'$load_files'(F, [if(not_loaded),must_be_module(true),imports(Is)], use_module(M,F,Is)).

'$csult'(Fs, M) :-
	'$extract_minus'(Fs, MFs), !,
	'$load_files'(M:MFs,[],[M:Fs]).
'$csult'(Fs, M) :-
	'$load_files'(M:Fs,[consult(consult)],[M:Fs]).

'$extract_minus'([], []).
'$extract_minus'([-F|Fs], [F|MFs]) :-
	'$extract_minus'(Fs, MFs).


'$do_lf'(ContextModule, Stream, UserFile, TOpts) :-
	'$lf_opt'('$context_module', TOpts, ContextModule),
	'$msg_level'( TOpts, Verbosity),
%	format( 'I=~w~n', [Verbosity=UserFile] ),
	'$lf_opt'(encoding, TOpts, Encoding),
	'$set_encoding'(Stream, Encoding),
	% export to process
	b_setval('$lf_status', TOpts),
	'$reset_if'(OldIfLevel),
	'$into_system_mode'(OldMode),
	% take care with [a:f], a is the ContextModule
	'$current_module'(SourceModule, ContextModule),
	'$lf_opt'(consult, TOpts, Reconsult),
	'$lf_opt'('$options', TOpts, Opts),
	'$lf_opt'('$location', TOpts, ParentF:Line),
	'$loaded'(Stream, UserFile, SourceModule, ParentF, Line, Reconsult, File, Dir, Opts),
	working_directory(OldD, Dir),
	H0 is heapused, '$cputime'(T0,_),
	'$set_current_loop_stream'(OldStream, Stream),
	'$swi_current_prolog_flag'(generate_debug_info, GenerateDebug),
	'$lf_opt'(compilation_mode, TOpts, CompMode),
	'$comp_mode'(OldCompMode, CompMode),
	( get_value('$syntaxcheckflag',on) -> '$init_style_check'(File) ; true ),
	recorda('$initialisation','$',_),
	( Reconsult = reconsult ->
	    '$start_reconsulting'(File),
	    '$start_consult'(Reconsult,File,LC),
	    '$remove_multifile_clauses'(File),
	    StartMsg = reconsulting,
	    EndMsg = reconsulted
	    ;
	    '$start_consult'(Reconsult,File,LC),
	    StartMsg = consulting,
	    EndMsg = consulted
	),
	print_message(Verbosity, loading(StartMsg, File)),
	'$lf_opt'(skip_unix_header , TOpts, SkipUnixHeader),
	( SkipUnixHeader == true->
	    '$skip_unix_header'(Stream)
	;
	    true
	),
	'$loop'(Stream,Reconsult),
	H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
	'$current_module'(Mod, SourceModule),
	print_message(Verbosity, loaded(EndMsg, File, Mod, T, H)),
	'$end_consult',
	( 
	    Reconsult = reconsult ->
	    '$clear_reconsulting'
	;
	    true
	),
	'$set_current_loop_stream'(Stream, OldStream),
	'$swi_set_prolog_flag'(generate_debug_info, GenerateDebug),
	'$comp_mode'(_CompMode, OldCompMode),
	working_directory(_,OldD),
	% surely, we were in run mode or we would not have included the file!
	nb_setval('$if_skip_mode',run),
	% back to include mode!
	nb_setval('$if_level',OldIfLevel),
	'$lf_opt'('$use_module', TOpts, UseModule),
	'$bind_module'(Mod, UseModule),
%	( File = '/Users/vsc/Yap/bins/threads/share/Yap/error.pl' -> start_low_level_trace ; stop_low_level_trace ),
	'$lf_opt'(imports, TOpts, Imports),
	'$import_to_current_module'(File, ContextModule, Imports, _, TOpts),
	'$lf_opt'(reexport, TOpts, Reexport),
	( Reexport == false -> true ;
	  '$lf_opt'('$parent_topts', TOpts, OldTOpts),
	  '$lf_opt'('$context_module', OldTOpts, OldContextModule),
	  '$import_to_current_module'(File, OldContextModule, Imports, _, TOpts),
	  '$extend_exports'(ContextModule, Imports)
	),
	( LC == 0 -> prompt(_,'   |: ') ; true),
        ( OldMode == off -> '$exit_system_mode' ; true ),
	'$exec_initialisation_goals',
%	format( 'O=~w~n', [Mod=UserFile] ),
	!.

% are we in autoload and autoload_flag is false?
'$msg_level'( TOpts, Verbosity) :-
	'$lf_opt'(autoload, TOpts, AutoLoad),
	AutoLoad == true,
	'$swi_current_prolog_flag'(verbose_autoload, false), !,
	Verbosity = silent.
'$msg_level'( _TOpts, Verbosity) :-
	'$swi_current_prolog_flag'(verbose_load, false), !,
	Verbosity = silent.
'$msg_level'( _TOpts, Verbosity) :-
	'$swi_current_prolog_flag'(verbose, silent), !,
	Verbosity = silent.
'$msg_level'( TOpts, Verbosity) :-
	'$lf_opt'(silent, TOpts, Silent),
	Silent == true, !,
	Verbosity = silent.
'$msg_level'( _TOpts, informational).

'$reset_if'(OldIfLevel) :-
	'$nb_getval'('$if_level', OldIfLevel, fail), !,
	nb_setval('$if_level',0).
'$reset_if'(0) :-
	nb_setval('$if_level',0).

'$get_if'(Level0) :-
	'$nb_getval'('$if_level', Level, fail), !,
	Level0 = Level.
'$get_if'(0).

'$into_system_mode'(OldMode) :-
	( '$nb_getval'('$system_mode', OldMode, fail) -> true ; OldMode = off),
        ( OldMode == off -> '$enter_system_mode' ; true ).

'$bind_module'(_, load_files).
'$bind_module'(Mod, use_module(Mod)).

'$import_to_current_module'(File, ContextModule, Imports, RemainingImports, TOpts) :-
	recorded('$module','$module'(File, Module, ModExports, _),_),
	Module \= ContextModule, !,
	'$lf_opt'('$call', TOpts, Call),
	'$convert_for_export'(Imports, ModExports, Module, ContextModule, TranslationTab, RemainingImports, Goal),
	'$add_to_imports'(TranslationTab, Module, ContextModule).
'$import_to_current_module'(_, _, _, _, _).

'$start_reconsulting'(F) :-
	recorda('$reconsulted','$',_),
	recorda('$reconsulting',F,_).

'$initialization'(V) :-
	var(V), !,
	'$do_error'(instantiation_error,initialization(V)).
'$initialization'(C) :- number(C), !,
	'$do_error'(type_error(callable,C),initialization(C)).
'$initialization'(C) :- db_reference(C), !,
	'$do_error'(type_error(callable,C),initialization(C)).
'$initialization'(G) :-
	'$show_consult_level'(Level1),
	% it will be done after we leave the current consult level.
	Level is Level1-1,
	recordz('$initialisation',do(Level,G),_),
	fail.
'$initialization'(_).

initialization(G,OPT) :-
	'$initialization'(G,OPT).

'$initialization'(G,OPT) :-
	( 
	   var(G)
	->
	  '$do_error'(instantiation_error,initialization(G,OPT))
	;
	   number(G)
	->
	  '$do_error'(type_error(callable,G),initialization(G,OPT))
	;
	   db_reference(G)
	->
	  '$do_error'(type_error(callable,G),initialization(G,OPT))
	;
	   var(OPT)
	->
	  '$do_error'(instantiation_error,initialization(G,OPT))
	;
	  atom(OPT)
	->
	  (
	   OPT == now
	  ->
	   fail
	  ;
	   OPT == after_load
	  ->
	   fail
	  ;
	   OPT == restore
	  ->
	   fail
	  ;
	   '$do_error'(domain_error(initialization,OPT),initialization(OPT))
	  )
	;
	  '$do_error'(type_error(OPT),initialization(G,OPT))
	).
'$initialization'(G,now) :-
	( call(G) -> true ; format(user_error,':- ~w:~w failed.~n',[M,G]) ).
'$initialization'(G,after_load) :-
	'$initialization'(G).
% ignore for now.
'$initialization'(_G,restore).

'$exec_initialisation_goals' :-
	nb_setval('$initialization_goals',on),
	fail.
'$exec_initialisation_goals' :-
	recorded('$blocking_code',_,R),
	erase(R),
	fail.
% system goals must be performed first 
'$exec_initialisation_goals' :-
	recorded('$system_initialisation',G,R),
	erase(R),
	G \= '$',
	once( call(G) ),
	fail.
'$exec_initialisation_goals' :-
	'$show_consult_level'(Level),
	'$current_module'(M),
	recorded('$initialisation',do(Level,_),_),
	findall(G,
	        '$fetch_init_goal'(Level, G),
		LGs),
	lists:member(G,LGs),
	'$nb_getval'('$system_mode', OldMode, fail),
        ( OldMode == on -> '$exit_system_mode' ; true ),
	% run initialization under user control (so allow debugging this stuff).
	(
	  '$system_catch'(once(M:G), M, Error, user:'$LoopError'(Error, top)),
	  fail
	;
          OldMode = on,
	  '$enter_system_mode',
	  fail
	).
'$exec_initialisation_goals' :-
	nb_setval('$initialization_goals',off).


'$fetch_init_goal'(Level, G) :-
	recorded('$initialisation',do(Level,G),R),
	erase(R),
	G\='$'.

'$include'(V, _) :- var(V), !,
	'$do_error'(instantiation_error,include(V)).
'$include'([], _) :- !.
'$include'([F|Fs], Status) :- !,
	'$include'(F, Status),
	'$include'(Fs, Status).
'$include'(X, Status) :-
	b_getval('$lf_status', TOpts),
	'$msg_level'( TOpts, Verbosity),
	'$full_filename'(X, Y , ( :- include(X)) ),
	'$lf_opt'(stream, TOpts, OldStream),
	source_location(F, L),
	'$current_module'(Mod),
	( open(Y, read, Stream) 	->
	  true ; 
	  '$do_error'(permission_error(input,stream,Y),include(X))
	),
	'$set_current_loop_stream'(OldStream, Stream),
	H0 is heapused, '$cputime'(T0,_),
	'$loaded'(Stream, X, Mod, F, L, include, Y, _Dir, []),
	( '$nb_getval'('$included_file', OY, fail ) -> true ; OY = [] ),
	'$lf_opt'(encoding, TOpts, Encoding),
	'$set_encoding'(Stream, Encoding),
	nb_setval('$included_file', Y),
	print_message(Verbosity, loading(including, Y)),
	'$loop'(Stream,Status),
	'$set_current_loop_stream'(Stream, OldStream),
	close(Stream),
	H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
	print_message(Verbosity, loaded(included, Y, Mod, T, H)),
	nb_setval('$included_file',OY).

'$do_startup_reconsult'(X) :-
	( '$access_yap_flags'(15, 0) ->
	  '$system_catch'(load_files(X, [silent(true)]), Module, Error, '$Error'(Error))
	;
	  '$swi_set_prolog_flag'(verbose, silent),
	  '$system_catch'(load_files(X, [silent(true),skip_unix_header(true)]),Module,_,fail)
	;
	  true
	),
	!,
	( '$access_yap_flags'(15, 0) -> true ; halt).
'$do_startup_reconsult'(_).

'$skip_unix_header'(Stream) :-
	peek_code(Stream, 0'#), !, % 35 is ASCII for '#
	skip(Stream, 10),
	'$skip_unix_header'(Stream).
'$skip_unix_header'(_).


source_file(FileName) :-
	recorded('$lf_loaded','$lf_loaded'(FileName, _),_).

source_file(Mod:Pred, FileName) :-
	current_module(Mod),
	Mod \= prolog,
	'$current_predicate_no_modules'(Mod,_,Pred),
	'$owned_by'(Pred, Mod, FileName).

'$owned_by'(T, Mod, FileName) :-
	'$is_multifile'(T, Mod),
	functor(T, Name, Arity),
	setof(FileName, Ref^recorded('$multifile_defs','$defined'(FileName,Name,Arity,Mod), Ref), L),
	lists:member(FileName, L).
'$owned_by'(T, Mod, FileName) :-
	'$owner_file'(T, Mod, FileName).

prolog_load_context(directory, DirName) :- 
	source_location(F, _),
	file_directory_name(F, DirName).
prolog_load_context(file, FileName) :- 
	source_location(FileName, _).
prolog_load_context(module, X) :-
	'$nb_getval'('$consulting_file', _, fail),
	'$current_module'(X).
prolog_load_context(source, F0) :-
	source_location(F0, _) /*,
	'$input_context'(Context),
	'$top_file'(Context, F0, F) */.
prolog_load_context(stream, Stream) :- 
	'$nb_setval'('$consulting_file', _, fail),
	'$current_loop_stream'(Stream).
% return this term for SWI compatibility.
prolog_load_context(term_position, '$stream_position'(0,Line,0,0,0)) :- 
	source_location(_, Line).


% if the file exports a module, then we can
% be imported from any module.
'$file_loaded'(Stream, M, Imports, TOpts) :-
	'$file_name'(Stream, F),
	'$ensure_file_loaded'(F, M, F1),
%	format( 'IL=~w~n', [(F1:Imports->M)] ),
	'$import_to_current_module'(F1, M, Imports, _, TOpts).

'$ensure_file_loaded'(F, M, F1) :-
	recorded('$module','$module'(F1,_NM,_P,_),_),
	recorded('$lf_loaded','$lf_loaded'(F1,_),_),
	same_file(F1,F), !.
'$ensure_file_loaded'(F, _M, F1) :-
	recorded('$lf_loaded','$lf_loaded'(F1,_),_),
	same_file(F1,F), !.
	

% if the file exports a module, then we can
% be imported from any module.
'$file_unchanged'(Stream, M, Imports, TOpts) :-
	'$file_name'(Stream, F),
	'$ensure_file_unchanged'(F, M, F1),
%	format( 'IU=~w~n', [(F1:Imports->M)] ),
	'$import_to_current_module'(F1, M, Imports, _, TOpts).

'$ensure_file_unchanged'(F, M, F1) :-
	recorded('$module','$module'(F1,_NM,_P,_),_),
	recorded('$lf_loaded','$lf_loaded'(F1,Age),R),
	same_file(F1,F), !,
	'$file_is_unchanged'(F, R, Age).
'$ensure_file_unchanged'(F, _M, F1) :-
	recorded('$lf_loaded','$lf_loaded'(F1,Age),R),
	same_file(F1,F), !,
	'$file_is_unchanged'(F, R, Age).

'$file_is_unchanged'(F, R, Age) :-
        time_file64(F,CurrentAge),
        ( (Age == CurrentAge ; Age = -1)  -> true; erase(R), fail).



path(Path) :- findall(X,'$in_path'(X),Path).

'$in_path'(X) :- recorded('$path',Path,_),
		atom_codes(Path,S),
		( S = ""  -> X = '.' ;
		  atom_codes(X,S) ).

add_to_path(New) :- add_to_path(New,last).

add_to_path(New,Pos) :-
	atom(New), !,
	'$check_path'(New,Str),
	atom_codes(Path,Str),
	'$add_to_path'(Path,Pos).

'$add_to_path'(New,_) :- recorded('$path',New,R), erase(R), fail.
'$add_to_path'(New,last) :- !, recordz('$path',New,_).
'$add_to_path'(New,first) :- recorda('$path',New,_).

remove_from_path(New) :- '$check_path'(New,Path),
			recorded('$path',Path,R), erase(R).

'$check_path'(At,SAt) :- atom(At), !, atom_codes(At,S), '$check_path'(S,SAt).
'$check_path'([],[]).
'$check_path'([Ch],[Ch]) :- '$dir_separator'(Ch), !.
'$check_path'([Ch],[Ch,A]) :- !, integer(Ch), '$dir_separator'(A).
'$check_path'([N|S],[N|SN]) :- integer(N), '$check_path'(S,SN).

% add_multifile_predicate when we start consult
'$add_multifile'(Name,Arity,Module) :-
	source_location(File,_),
	'$add_multifile'(File,Name,Arity,Module).

'$add_multifile'(File,Name,Arity,Module) :-
	recorded('$multifile_defs','$defined'(File,Name,Arity,Module), _), !.
%	print_message(warning,declaration((multifile Module:Name/Arity),ignored)).
'$add_multifile'(File,Name,Arity,Module) :-
	recordz('$multifile_defs','$defined'(File,Name,Arity,Module),_), !,
	fail.
'$add_multifile'(File,Name,Arity,Module) :-
	recorded('$mf','$mf_clause'(File,Name,Arity,Module,Ref),R),
	erase(R),
	'$erase_clause'(Ref,Module),
	fail.
'$add_multifile'(_,_,_,_).

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

% inform the file has been loaded and is now available.
'$loaded'(Stream, UserFile, M, OldF, Line, Reconsult, F, Dir, Opts) :-
	'$file_name'(Stream, F0),
	( F0 == user_input, nonvar(UserFile) -> UserFile = F ; F = F0 ),
	( F == user_input -> working_directory(Dir,Dir) ; file_directory_name(F, Dir) ),
	nb_setval('$consulting_file', F ),
	( Reconsult \== consult, Reconsult \== not_loaded, Reconsult \== changed, recorded('$lf_loaded','$lf_loaded'(F, _),R), erase(R), fail ; var(Reconsult) -> Reconsult = consult ; true ),
	( Reconsult \== consult, recorded('$lf_loaded','$lf_loaded'(F, _, _, _, _, _, _),R), erase(R), fail ; var(Reconsult) -> Reconsult = consult ; true ),
	( F == user_input -> Age = 0 ; time_file64(F, Age) ),
	( recordaifnot('$lf_loaded','$lf_loaded'( F, Age), _) -> true ; true ),
	recorda('$lf_loaded','$lf_loaded'( F, M, Reconsult, UserFile, OldF, Line, Opts), _).

'$set_encoding'(Encoding) :-
	'$current_loop_stream'(Stream),
	'$set_encoding'(Stream, Encoding).

'$set_encoding'(Stream, Encoding) :-
	( Encoding == default -> true ; set_stream(Stream, encoding(Encoding)) ).

%
% This is complicated because of embedded ifs.
%
'$if'(_,top) :- !, fail.
'$if'(_Goal,_) :-
	'$get_if'(Level0),
	Level is Level0 + 1,
	nb_setval('$if_level',Level),
	( '$nb_getval'('$endif', OldEndif, fail) -> true ; OldEndif=top),
	( '$nb_getval'('$if_skip_mode', Mode, fail) -> true ; Mode = run ),
	nb_setval('$endif',elif(Level,OldEndif,Mode)),
	fail.
% we are in skip mode, ignore....
'$if'(_Goal,_) :-
	'$nb_getval'('$endif',elif(Level, OldEndif, skip), fail), !,
	nb_setval('$endif',endif(Level, OldEndif, skip)).	
% we are in non skip mode, check....
'$if'(Goal,_) :-
	('$if_call'(Goal)
	    ->
	 % we will execute this branch, and later enter skip
	 '$nb_getval'('$endif', elif(Level,OldEndif,Mode), fail),
	 nb_setval('$endif',endif(Level,OldEndif,Mode))

	;
	 % we are now in skip, but can start an elif.
	 nb_setval('$if_skip_mode',skip)
	).

'$else'(top) :- !, fail.
'$else'(_) :-
	'$get_if'(0), !,
	'$do_error'(context_error(no_if),(:- else)).
% we have done an if, so just skip
'$else'(_) :-
	nb_getval('$endif',endif(_Level,_,_)), !,
	nb_setval('$if_skip_mode',skip).
% we can try the elif
'$else'(_) :-
	'$get_if'(Level),
	nb_getval('$endif',elif(Level,OldEndif,Mode)),
	nb_setval('$endif',endif(Level,OldEndif,Mode)),
	nb_setval('$if_skip_mode',run).

'$elif'(_,top) :- !, fail.
'$elif'(Goal,_) :-
	'$get_if'(0),
	'$do_error'(context_error(no_if),(:- elif(Goal))).
% we have done an if, so just skip
'$elif'(_,_) :-
	 nb_getval('$endif',endif(_,_,_)), !,
	 nb_setval('$if_skip_mode',skip).
% we can try the elif
'$elif'(Goal,_) :-
	'$get_if'(Level),
	nb_getval('$endif',elif(Level,OldEndif,Mode)),
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

'$endif'(top) :- !, fail.
'$endif'(_) :-
% unmmatched endif.
	'$get_if'(0),
	'$do_error'(context_error(no_if),(:- endif)).
'$endif'(_) :-
% back to where you belong.
	'$get_if'(Level),
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
	expand_term(Goal,TrueGoal),
	once(TrueGoal).

'$if_directive'((:- if(_))).
'$if_directive'((:- else)).
'$if_directive'((:- elif(_))).
'$if_directive'((:- endif)).


'$comp_mode'(_OldCompMode, CompMode) :-
	var(CompMode), !. % just do nothing.
'$comp_mode'(OldCompMode, assert_all) :-
	'$fetch_comp_status'(OldCompMode),
	nb_setval('$assert_all',on).
'$comp_mode'(OldCompMode, source) :-
	'$fetch_comp_status'(OldCompMode),
	'$set_yap_flags'(11,1).
'$comp_mode'(OldCompMode, compact) :-
	'$fetch_comp_status'(OldCompMode),
	'$set_yap_flags'(11,0).

'$fetch_comp_status'(assert_all) :-
	'$nb_getval'('$assert_all',on, fail), !.
'$fetch_comp_status'(source) :-
	 '$access_yap_flags'(11,1), !.
'$fetch_comp_status'(compact).

make :-
	recorded('$lf_loaded','$lf_loaded'(F1,_M,reconsult,_,_,_,_),_),
	'$load_files'(F1, [if(changed)],make),
	fail.
make.

make_library_index(_Directory).

'$file_name'(Stream,F) :-
	stream_property(Stream, file_name(F)), !.
'$file_name'(user_input,user_input).
'$file_name'(user_output,user_ouput).
'$file_name'(user_error,user_error).


'$fetch_stream_alias'(OldStream,Alias) :-
	stream_property(OldStream, alias(Alias)), !.

'$require'(_Ps, _M).

'$store_clause'('$source_location'(File, _Line):Clause, File) :-
	assert_static(Clause).


'$set_current_loop_stream'(OldStream, Stream) :-
	'$current_loop_stream'(OldStream), !,
	'$new_loop_stream'(Stream).
'$set_current_loop_stream'(_OldStream, Stream) :-
	'$new_loop_stream'(Stream).

'$new_loop_stream'(Stream) :-
	(var(Stream) ->
	    nb_delete('$loop_stream')
	;
	    nb_setval('$loop_stream',Stream)
	).
	    
'$current_loop_stream'(Stream) :-
	'$nb_getval'('$loop_stream',Stream, fail).

exists_source(File) :-
	'$full_filename'(File, _AbsFile, exists_source(File)).

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
	recorded('$lf_loaded','$lf_loaded'( F, Age), _).
'$source_file_property'( F, included_in(OldF, Line)) :-
	recorded('$lf_loaded','$lf_loaded'( F, _M, include, _File, OldF, Line, _), _).
'$source_file_property'( F, load_context(OldF, Line, Options)) :-
	recorded('$lf_loaded','$lf_loaded'( F, _M, V, _File, OldF, Line, Options), _), V \== include.
'$source_file_property'( F, modified(Age)) :-
	recorded('$lf_loaded','$lf_loaded'( F, Age), _).
'$source_file_property'( F, module(M)) :-
	recorded('$module','$module'(F,M,_,_),_).

