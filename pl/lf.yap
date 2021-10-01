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

'$lf_option'(autoload, 1, false).
'$lf_option'(derived_from, 2, false).
'$lf_option'(encoding, 3, default).
'$lf_option'(expand, 4, false).
'$lf_option'(if, 5, true).
'$lf_option'(imports, 6, all).
'$lf_option'(qcompile, 7, Current) :-
	(
	  '__NB_getval__'('$qcompile', Current, fail) ->
	  true
	;
	  nb_setval('$qcompile',never)
	).
'$lf_option'(silent, 8, Default) :-
    (
	prolog_flag( verbose_load, false )
    ->
    Default = true
    ;
    Default = false
    ).
'$lf_option'(skip_unix_header, 9, Skip) :-
    stream_property(loop_stream,[tty(TTy),reposition(Rep)]),
    ( Rep == true
    ->
	     (TTy = true   -> Skip = false ; Skip = true)
      ;
      Skip = false
      ).
'$lf_option'(compilation_mode, 10, Flag) :-
	current_prolog_flag(source, YFlag),
	( YFlag == false -> Flag = compact ; Flag = source ).
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
'$lf_option'(redefine_module, 22, Warn) :-
	( var(Warn) ->	current_prolog_flag( redefine_warnings, Redefine ), Redefine = Warn ; true ).
'$lf_option'(reexport, 23, false).
'$lf_option'(sandboxed, 24, false).
'$lf_option'(scope_settings, 25, false).
'$lf_option'(modified, 26, _).
'$lf_option'(source_module, 27, _).
'$lf_option'('$parent_topts', 28, _).
'$lf_option'(must_be_module, 29, false).
'$lf_option'('$source_pos', 30, _).
'$lf_option'('$from_stream', 31, false).


'$lf_option'(last_opt, 32).

'$lf_opt'( Op, TOpts, Val) :-
	'$lf_option'(Op, Id, _),
	arg( Id, TOpts, Val ).

'$set_lf_opt'( Op, TOpts, Val) :-
	'$lf_option'(Op, Id, _),
	setarg( Id, TOpts, Val ).

'$load_files'([user], M,Opts, Call) :-
    current_input(S),
    '$load_files__'(user, M, [stream(S)|Opts], Call).
'$load_files'(user, M,Opts, Call) :-
    current_input(S),
    '$load_files__'(user, M, [stream(S)|Opts], Call).
'$load_files'([-user], M,Opts, Call) :-
    current_input(S),
    '$load_files__'(user, M, [consult(reconsult),stream(S)|Opts], Call).
'$load_files'(-user, M,Opts, Call) :-
    current_input(S),
    '$load_files__'(user, M, [consult(reconsult),stream(S)|Opts], Call).
'$load_files'([user_input], M,Opts, Call) :-
    current_input(S),
    '$load_files__'(user_input, M, [stream(S)|Opts], Call).
'$load_files'(user_input, M,Opts, Call) :-
    current_input(S),
    '$load_files__'(user_input, M, [stream(S)|Opts], Call).
'$load_files'([-user_input], M,Opts, Call) :-
    current_input(S),
    '$load_files__'(user_input, M, [consult(reconsult),stream(S)|Opts], Call).
'$load_files'(-user_input, M,Opts, Call) :-
    current_input(S),
    '$load_files__'(user_input, M, [consult(reconsult),stream(S)|Opts], Call).
'$load_files'(File, M,Opts, Call) :-
    lists:member(consult(db),Opts),
    !,
    dbload(File, M, Call).
'$load_files'(File, M,Opts, Call) :-
    lists:member(consult(exo),Opts),
    !,
    exoload(File, M, Call).
'$load_files'(Files, M, Opts, Call) :-
    '$load_files__'(Files, M, Opts, Call).
'$load_files__'(Files, M, Opts, Call) :-
    '$lf_option'(last_opt, LastOpt),
    (
	'$nb_current'(lf_default_opts),
	nb_getval('$lf_status', OldTOpts),
	nonvar(OldTOpts),
	OldTOpts \= []
             ->
    '$lf_opt'(autoload, OldTOpts, OldAutoload),
    '$lf_opt'(source_module, OldTOpts, OldContextModule)
	     ;
    functor( OldTOpts, opt, LastOpt ),
    '$lf_default_opts'(1, LastOpt, OldTOpts),
     OldAutoload = false,
     source_module( OldContextModule)
    ),
    functor( TOpts, opt, LastOpt ),
    ( source_location(ParentF, Line) -> true ; ParentF = user_input, Line = 1 ),
    '$lf_opt'('$location', TOpts, ParentF:Line),
    '$lf_opt'('$files', TOpts, Files),
    '$lf_opt'('$call', TOpts, Call),
    '$lf_opt'('$options', TOpts, Opts),
    '$lf_opt'('$parent_topts', TOpts, OldTOpts),
    '$process_lf_opts'(Opts,TOpts,Files,Call),
    '$lf_default_opts'(1, LastOpt, TOpts),
    '$lf_opt'(stream, TOpts, Stream),
    (  nonvar(Stream) ->
       '$set_lf_opt'('$from_stream', TOpts, true )
    ;
    '$check_files'(Files,load_files(Files,Opts))
    ),
    '$check_use_module'(Call,UseModule),
    '$lf_opt'('$use_module', TOpts, UseModule),
    ( '$lf_opt'(autoload, TOpts, Autoload),
      var(Autoload) ->
      Autoload = OldAutoload
    ;
    true
    ),
    '$lf'(Files, M, Call, TOpts).

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
	( Val == part -> '$do_error'(domain_error(unimplemented_option,expand),Call) ;
	    Val == never -> true ;
	    Val == auto -> true ;
	    Val == large -> true ;
	    '$do_error'(domain_error(unknown_option,qcompile(Val)),Call) ).
'$process_lf_opt'(silent, Val, Call) :-
    ( Val == false -> yap_flag(verbose_load,_,true) ; 
      Val == true -> yap_flag(verbose_load,_,false) ; 
      '$do_error'(domain_error(out_of_domain_option,silent(Val)),Call)
    ).
'$process_lf_opt'(skip_unix_header, Val, Call)  :-
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
	( Val == true -> true
	;
	    Val == false -> true
	;
	    '$do_error'(domain_error(unimplemented_option,reexport(Val)),Call) ).
'$process_lf_opt'(must_be_module, Val , Call) :-
	( Val == true -> true ;
	    Val == false -> true ;
	    '$do_error'(domain_error(unimplemented_option,must_be_module(Val)),Call) ).
'$process_lf_opt'(stream, Val, Call) :-
	( '$stream'(Val) -> true ;
	    '$do_error'(type_error(stream,Val),Call) ).
'$process_lf_opt'(register, Val, Call) :-
	( Val == false -> true ;
	    Val == true -> true ;
	    '$do_error'(domain_error(unimplemented_option,register(Val)),Call) ).
'$process_lf_opt'(source_module, Mod, Call) :-
	( atom(Mod) -> true ;  '$do_error'(type_error(atom,Mod),Call) ).


'$lf_default_opts'(I, LastOpt, _TOpts) :- I > LastOpt, !.
'$lf_default_opts'(I, LastOpt, TOpts) :-
	I1 is I+1,
	arg(I, TOpts, A),
	( nonvar(A) -> true ;
	 ignore( '$lf_option'(_Name, I, A) )
	),
	'$lf_default_opts'(I1, LastOpt, TOpts).



'$check_use_module'(use_module(_), use_module(_)) :- !.
'$check_use_module'(use_module(_,_), use_module(_)) :- !.
'$check_use_module'(use_module(M,_,_), use_module(M)) :- !.
'$check_use_module'(_, load_files) :- !.

'$lf'(V,_,Call, _ ) :-   var(V), !,
	'$do_error'(instantiation_error,Call).
'$lf'([], _, _, _) :- !.
'$lf'([F|Fs], Mod, Call, TOpts) :- !,
	% clean up after each consult
	( '$lf'(F,Mod,Call, TOpts), fail;
	  '$lf'(Fs, Mod, Call, TOpts), fail;
	  true
	).
'$lf'(File, Mod, Call, TOpts) :-
	'$lf_opt'(stream, TOpts, Stream),
	b_setval('$user_source_file', File),
	'$lf_opt'(encoding, TOpts, Enc),
	( '$lf_opt'('$from_stream', TOpts, false ) ->
	  /* need_to_open_file */
	  (
	      absolute_file_name(File, Y, [access(read),file_type(prolog),file_errors(fail),solutions(first)]) ->
	      true
	  ;
	  '$do_io_error'(existence_error(source_sink,File),Call)
	  ),
	  (
	      open(Y, read, Stream, [encoding(Enc)])
	  ->
	  true
	  ;
	   '$do_error'(permission_error(input,stream,Y),Call) 
	  )
	;
	stream_property(Stream, file_name(Y))
	),
	!,
	( file_size(Stream, Pos) -> true ; Pos = 0),
	'$set_lf_opt'('$source_pos', TOpts, Pos),
	'$lf_opt'(if, TOpts, If),
	( var(If) -> If = true ; true ),
	'$start_lf'(If, Mod, Stream, TOpts, File, Y),
	close(Stream).

% consulting from a stream
'$start_lf'(_not_loaded, Mod, Stream, TOpts, UserFile, File) :-
	'$lf_opt'('$from_stream', TOpts, true ),
    !,
    '$do_lf'(Mod, Stream, UserFile, File, TOpts).
'$start_lf'(not_loaded, _Mod, _Stream, TOpts, UserFile, File) :-
	'$file_loaded'(File, InnerMod), !,
	current_source_module(M,M),
	'$lf_opt'('$options', TOpts, Opts),
	'$lf_opt'('$location', TOpts, ParentF:Line),
	'$loaded'(File, UserFile, M, ParentF, Line, not_loaded, _, _Dir, TOpts, Opts),
	'$lf_opt'(imports, TOpts, Imports),
	'$import_module'( InnerMod, M, Imports, _),
	'$reexport'(TOpts,InnerMod,Imports,File).
'$start_lf'(changed, _Mod, _Stream, TOpts, UserFile, File) :-
	'$file_unchanged'(File, InnerMod), !,
	current_source_module(M,M),
	'$lf_opt'('$options', TOpts, Opts),
	'$lf_opt'('$location', TOpts, ParentF:Line),
	'$loaded'(File, UserFile, _Mod, ParentF, Line, changed, _, _Dir, TOpts, Opts),
	'$lf_opt'(imports, TOpts, Imports),
	'$import_module'(InnerMod, M, Imports, _),
	'$reexport'(TOpts,InnerMod,Imports,File).

'$start_lf'(_, OuterModule, PlStream, TOpts, _UserFile, File) :-
    % check if there is a qly file
				%	start_low_level_trace,
	absolute_file_name(File,F,[access(read),file_type(qly),file_errors(fail),solutions(first),expand(true)]),
	    open( F, read, Stream , [type(binary)] ),
	(
	  '$q_header'( Stream, Type ),
	 Type == file
	->
	 !
	 ;
	 close(Stream),
	fail
	),
	H0 is heapused, '$cputime'(T0,_),
       time_file64(F, T0F),
       stream_property(PlStream, file_name(FilePl)),
       time_file64(FilePl, T0Fl),
       T0F >= T0Fl,
       !,
       file_directory_name(F, Dir),
       working_directory(OldD, Dir),
       '$qload_file'(Stream, OuterModule, F, FilePl, File, Imports, TOpts),
       close( Stream ),
       H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
       current_source_module(M, OuterModule),
       working_directory( _, OldD),
       %'$lf_opt'('$location', TOpts, ParentF:_Line),
       print_message(informational, loaded( loaded, F, M, T, H)),
       working_directory( _, OldD),
	'$reexport'(TOpts,M,Imports,F).
'$start_lf'(_, Mod, Stream, TOpts, UserFile, File) :-
	'$do_lf'(Mod, Stream, UserFile, File, TOpts).


'$do_lf'(_ContextModule, Stream, _UserFile, _File, _TOpts) :-
    stream_property(Stream, file_name(Y)),
    '$being_consulted'(Y),
    !.
'$do_lf'(ContextModule, Stream, UserFile, File,  TOpts) :-
	'$init_do_lf'(CtxVL,ContextModule,
		      ContextQCompiling,
		      Stream, UserFile, File, LC,
		      TOpts, OldCompMode, OldIfLevel,
		      OldD,H0,T0,Reconsult),

	/*** core consult */
	'$loop'(Stream,Reconsult),
	'$close_do_lf'(	CtxVL,
			ContextModule,
			ContextQCompiling,
	       Reconsult,
	       UserFile, File, LC,
	       TOpts, OldCompMode, OldIfLevel, 
	       OldD,H0,T0
		      ),
	fail.
'$do_lf'(_ContextModule, _Stream, _UserFile, _File,  _TOpts).
		      


'$init_do_lf'(CtxVL,ContextModule,
			ContextQCompiling,
		      Stream, UserFile, File, LC,
		      TOpts, OldCompMode, OldIfLevel,
		      OldD,H0,T0,Reconsult) :-
    prolog_flag(verbose_load, CtxVL),
    prompt1(': '), prompt(_,'     '),
    '$lf_opt'(qcompile, TOpts, QCompiling),
    '__NB_getval__'('$qcompile', ContextQCompiling, ContextQCompiling = never),
    nb_setval('$qcompile', QCompiling),
    %	format( 'I=~w~n', [Verbosity=UserFile] ),
    % export to process
    b_setval('$lf_status', TOpts),
    '__NB_getval__'('$if_level', OldIfLevel, OldIfLevel=0),
    nb_setval('$if_level',0),
	% take care with [a:f], a is the ContextModule
    '$lf_opt'(consult, TOpts, Reconsult0),
    '$lf_opt'('$options', TOpts, Opts),
    '$lf_opt'('$location', TOpts, ParentF:Line),
    '$loaded'(File, UserFile, ContextModule, ParentF, Line, Reconsult0, Reconsult, Dir, TOpts, Opts),
    working_directory(OldD, Dir),
    H0 is heapused, '$cputime'(T0,_),
    '$lf_opt'(compilation_mode, TOpts, CompMode),
    '$comp_mode'(OldCompMode, CompMode),
    ( Reconsult \== consult ->
	'$start_reconsulting'(File),
	'$start_consult'(Reconsult,File,Stream,LC),
	'$remove_multifile_clauses'(File),
	StartMsg = reconsulting
    ;
	'$start_consult'(Reconsult,File,Stream,LC),
	( File \= user_input, File \= [] -> '$remove_multifile_clauses'(File) ; true ),
	StartMsg = consulting
    ),
    print_message(informational, loading(StartMsg, UserFile)),
    '$lf_opt'(skip_unix_header , TOpts, SkipUnixHeader),
    ( SkipUnixHeader == true
    ->
	'$skip_unix_header'(Stream)
    ;
	true
    ).

'$close_do_lf'(	CtxVL,
		OuterModule,
		ContextQCompiling,
	       Reconsult,
	       UserFile, File, LC,
	       TOpts, OldCompMode, OldIfLevel,
		      OldD,H0,T0) :-
	H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
	EndMsg = consulted,
	'$q_do_save_file'(File, UserFile, TOpts ),
	(
	    Reconsult = reconsult ->
		'$clear_reconsulting'
	    ;
	    true
	),
	nb_setval('$if_level',OldIfLevel),
	'$comp_mode'(_CompMode, OldCompMode),
	working_directory(_,OldD),
	% surely, we were in run mode or we would not have included the file!
				% back to include mode!
	'$lf_opt'('$use_module', TOpts, UseModule),
	current_source_module(InnerModule,InnerModule),
	'$bind_module'(InnerModule, UseModule),
	nb_setval('$qcompile', ContextQCompiling),
   	( LC == 0 -> prompt(_,'   |: ') ; true),
	print_message(informational, loaded(EndMsg, File,  InnerModule, T, H)),
	'$exec_initialization_goals',
		'$end_consult',
module(OuterModule),
	set_prolog_flag(verbose_load,CtxVL).




'$q_do_save_file'(File, UserF, TOpts ) :-
    '$lf_opt'(qcompile, TOpts, QComp),
    '$lf_opt'('$source_pos', TOpts, Pos),
    '$lf_opt'('$from_stream', TOpts, false),
    ( QComp ==  auto ; QComp == large, Pos > 100*1024),
    absolute_file_name(UserF,F,[file_type(qly),solutions(first),expand(true)]),
    !,
    '$qsave_file_'( File, UserF, F ).
'$q_do_save_file'(_File, _, _TOpts ).

'$bind_module'(_, load_files).
'$bind_module'(Mod, use_0module(Mod)).



'$start_reconsulting'(F) :-
	recorda('$reconsulted','$',_),
	recorda('$reconsulting',F,_).


/**
  @pred include(+ _F_) is directive

  The `include` directive adds the text files or sequence of text
  files specified by  _F_ into the files being currently consulted.
  It may be used
  as an replacement to consult/1 by allowing the programmer to include a
  data-base
  split into several files.
*/

'$include'(V, _) :- var(V), !,
	'$do_error'(instantiation_error,include(V)).
'$include'([], _) :- !.
'$include'([F|Fs], Status) :- !,
	'$include'(F, Status),
	'$include'(Fs, Status).
'$include'(File, Status) :-
    (
	'__NB_getval__'('$lf_status', TOpts,fail)
    ->
    true
    ;
      TOpts=[]),
    H0 is heapused, '$cputime'(T0,_),
    '$stream_and_dir'(File,Y,Dir,Stream),
    working_directory(Dir0, Dir),
    '$including'(OV, Y),
    '$lf_opt'(encoding, TOpts, Encoding),
    set_stream(Stream, [alias(loop_stream),encoding(Encoding)] ),
    '$loaded'(Y, File,  _Mod, _OldY, _L, include, _, Dir, TOpts,[]),
    ( '__NB_getval__'('$included_file', OY, fail ) -> true ; OY = [] ),
    nb_setval('$included_file', Y),
    print_message(informational, loading(including, Y)),
    '$loop'(Stream,Status),
    close(Stream),
    H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
    current_source_module(Mod, Mod),
    print_message(informational, loaded(included, Y, Mod, T, H)),
    working_directory(_Dir, Dir0),
    '$including'(Y, OV),
    b_setval('$lf_status', TOpts),
    nb_setval('$included_file',OY).

'$stream_and_dir'(user,user_input,Dir,user_input) :-
	!,
        working_directory(_Dir, Dir).
'$stream_and_dir'(user_input,user_input,Dir,user_input) :-
	!,
        working_directory(_Dir, Dir).
'$stream_and_dir'(File,Y,Dir,Stream) :-
    absolute_file_name(File, Y, [access(read),file_type(prolog),file_errors(fail),solutions(first),expand(true)]),
    ( open(Y, read, Stream) 	->
      true ;
      '$do_error'(permission_error(input,stream,Y),include(File))
    ),
    file_directory_name(Y, Dir).



%% @}


