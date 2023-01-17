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
 * File:		lf.yap *
 * Last rev:	8/2/88							 *
 * mods: *
 * comments:	Execute Prolog code					 *
 *									 *

 *************************************************************************/

 /**
  * @file lf.yap
  * @brief Implementation of load-files
  *
  *
  */
%
% SWI options
% autoloa(true,false)
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

'$lf_option'(derived_from, 2, false).
'$lf_option'(encoding, 3, default).
'$lf_option'(if, 5, true).
'$lf_option'(imports, 6, all).
'$lf_option'(qcompile, 7, Current) :-
	(
	  '__NB_getval__'('$qcompile', Current, fail) ->
	  true
	;
	  nb_setval('$qcompile',never)
	).
'$lf_option'(silent, 8, D) :-
    current_prolog_flag(verbose_load,F),
    (F==true->D=false;D=true).
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
'$lf_option'(modified, 26, true).
'$lf_option'(source_module, 27, _).
'$lf_option'('$parent_topts', 28, _).
'$lf_option'(must_be_module, 29, false).
'$lf_option'('$source_pos', 30, _).
'$lf_option'('$from_stream', 31, false).


'$lf_option'(last_opt, 32, end).

'$lf_opt'( Op, TOpts, Val) :-
	'$lf_option'(Op, Id, _),
	arg( Id, TOpts, Val ).

'$set_lf_opt'( Op, TOpts, Val) :-
	'$lf_option'(Op, Id, _),
	setarg( Id, TOpts, Val ).


'$mk_opts'(Opts,File,Stream,_M,Call,TOpts) :-
    '$lf_option'(last_opt, LastOpt, _),
    functor( TOpts, opt, LastOpt ),
    (
	'$nb_current'('$lf_status'),
	nb_getval('$lf_status', OldTOpts),
	nonvar(OldTOpts),
	OldTOpts \= []
             ->
    '$lf_default_opts'(1, LastOpt, TOpts)
	     ;
	     true
    ),
    '$lf_opt'('$call', TOpts, Call),
    '$lf_opt'('$options', TOpts, Opts),
    '$lf_opt'('$parent_topts', TOpts, OldTOpts),
    '$process_lf_opts'(Opts,TOpts,File,Call),
    '$lf_default_opts'(1, LastOpt, TOpts),
    '$lf_opt'(stream, TOpts, Stream),
    '$check_use_module'(Call,UseModule),
    '$lf_opt'('$use_module', TOpts, UseModule).

'$mk_file_opts'(TOpts) :-
 %   ( file_size(Stream, Pos) -> true ; Pos = 0),
    '$set_lf_opt'('$source_pos', TOpts, 0).

'$process_lf_opts'(V, _, _, Call) :-
	var(V), !,
	throw_error(instantiation_error,Call).
'$process_lf_opts'([], _, _, _).
'$process_lf_opts'([Opt|Opts],TOpt,File,Call) :-
	Opt =.. [Op, Val],
	ground(Val),
	'$lf_opt'(Op, TOpt, Val),
	'$process_lf_opt'(Op, Val,Call), !,
	'$process_lf_opts'(Opts, TOpt, File, Call).
'$process_lf_opts'([Opt|_],_,_,Call) :-
	throw_error(domain_error(unimplemented_option,Opt),Call).

'$process_lf_opt'(autoload, Val, Call) :-
	( Val == false -> true ;
	    Val == true -> true ;
	    throw_error(domain_error(unimplemented_option,autoload(Val)),Call) ).
'$process_lf_opt'(derived_from, File, Call) :-
	( atom(File) -> true ;  throw_error(type_error(atom,File),Call) ).
'$process_lf_opt'(encoding, Encoding, _Call) :-
	atom(Encoding).
'$process_lf_opt'(expand, Val, Call) :-
	( Val == true -> throw_error(domain_error(unimplemented_option,expand(Val)),Call) ;
	    Val == false -> true ;
	    throw_error(domain_error(unimplemented_option,expand(Val)),Call) ).
'$process_lf_opt'(if, If, Call) :-
	( If == changed -> true ;
	    If == true -> true ;
	    If == not_loaded -> true ;
	    throw_error(domain_error(unimplemented_option,if(If) ),Call) ).
'$process_lf_opt'(imports, Val, Call) :-
	( Val == all -> true ;
	    var(Val) -> Val = all ;
	    is_list(Val) -> ( ground(Val) -> true ; throw_error(instantiation_error,Call) ) ;
	    throw_error(domain_error(unimplemented_option,imports(Val)),Call) ).
'$process_lf_opt'(qcompile, Val,Call) :-
	( Val == part -> throw_error(domain_error(unimplemented_option,expand),Call) ;
	    Val == never -> true ;
	    Val == auto -> true ;
	    Val == large -> true ;
	    throw_error(domain_error(unknown_option,qcompile(Val)),Call) ).
'$process_lf_opt'(silent, Val, Call) :-
    ( Val == false -> set_prolog_flag(verbose_load, true) ; 
      Val == true ->  set_prolog_flag(verbose_load, false) ; 
       Val == off -> set_prolog_flag(verbose_load, true) ; 
      Val == on ->  set_prolog_flag(verbose_load, false) ; 
      throw_error(domain_error(out_of_domain_option,silent(Val)),Call)
    ).
'$process_lf_opt'(skip_unix_header, Val, Call)  :-
	( Val == false -> true ;
	    Val == true -> true ;
	    throw_error(domain_error(unimplemented_option,skip_unix_header(Val)),Call) ).
'$process_lf_opt'(compilation_mode, Val, Call) :-
    ( Val == source -> true ;
      Val == compact -> true ;
      Val == assert_all -> true ;
      throw_error(domain_error(unimplemented_option,compilation_mode(Val)),Call) ).
'$process_lf_opt'(consult, Val , Call) :-
    ( Val == reconsult -> true ;
      Val == consult -> true ;
      Val == exo -> true ;
      Val == db -> true ;
      throw_error(domain_error(unimplemented_option,consult(Val)),Call) ).
'$process_lf_opt'(reexport, Val , Call) :-
   nb_setval('$reexport' , Val),
	( Val == true -> true
	;
	    Val == false -> true
	;
	    throw_error(domain_error(unimplemented_option,reexport(Val)),Call) ).
'$process_lf_opt'(must_be_module, Val , Call) :-
	( Val == true -> true ;
	    Val == false -> true ;
	    throw_error(domain_error(unimplemented_option,must_be_module(Val)),Call) ).
'$process_lf_opt'(stream, Val, Call) :-
	( '$stream'(Val) -> true ;
	    throw_error(type_error(stream,Val),Call) ).
'$process_lf_opt'(register, Val, Call) :-
	( Val == false -> true ;
	    Val == true -> true ;
	    throw_error(domain_error(unimplemented_option,register(Val)),Call) ).
'$process_lf_opt'(source_module, Mod, Call) :-
	( atom(Mod) -> true ;  throw_error(type_error(atom,Mod),Call) ).


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

%% @pred $load_files(Module, Path, OPtions, InitialGoal)
%%
%% actual interface to file-loading machinery
'$load_files'(V0, M0, _O, Call) :-
    '$yap_strip_module'(M0:V0, M, V),
    '$load_files_'(V, M, _O, Call).

'$load_files_'(V, M, _O, Call) :-
    (var(V);var(M)),
    !,
    throw(error(instantiation_error, Call)).
'$load_files_'([], _M,_O,_Call) :- !.
'$load_files_'([H|T], M,O,Call) :- !,
    (
	'$load_files'(H, M,O,Call),
	fail
    ;
    '$load_files'(T, M,O,Call)
    ).
'$load_files_'(user, M,Opts, Call) :-
    !,
     '$load_stream__'(prolog,  user, user_input, user_input, M, Opts, Call).
'$load_files_'(user_input, M,Opts, Call) :-
    !,
    '$load_stream__'(prolog,  user_input, user_input,  user_input, M, Opts, Call).
'$load_files_'(-F, M,Opts, Call) :-
    !,
    '$load_files'( F, M, [consult(reconsult)|Opts], Call).
'$load_files_'(File, M,Opts, Call) :-
    '$memberchk'(consult(db),Opts),
    !,
    dbload(File, M, Call).
'$load_files_'(File, M,Opts, Call) :-
    '$memberchk'(consult(exo),Opts),
    !,
    exoload(File, M, Call).
%% Prolog stream
'$load_files_'(File, M,Opts, Call) :-
    atom(File),
    '$memberchk'(stream(Stream),Opts),
    !,
  '$load_stream__'(prolog, File,Stream, (File), M, Opts, Call).

'$load_files_'(File, M, Opts, Call) :-
%   writeln(+M:File),
   current_prolog_flag(autoload,OldAutoload),
    (
     '$memberchk'(autoload(Autoload), Opts)
      ->
       			  set_prolog_flag(autoload,Autoload) ;
       			   true), 
   ( '$memberchk'(expand(Expand),Opts) -> true ; Expand = true ),
    (
	absolute_file_name(File, Y, [access(read),file_type(prolog),file_errors(fail),solutions(first)]) 
    ->
	Type = prolog
    ;
    absolute_file_name(File, Y, [access(read),file_type(qly),file_errors(fail),solutions(first),expand(Expand)])
    ->
	Type = qly
  ;
    '$do_io_error'(existence_error(source_sink,File),Call)
    ),
    (
	'$memberchk'(encoding(Encoding), Opts)
    ->		    
    open(Y, read, Stream, [encoding(Encoding)])
    ;
    open(Y, read, Stream, [])
    ),
   file_directory_name(Y, Dir),
    working_directory(OldD, Dir),
    '$load_file__'(Type,File,Stream,Y, M, Opts, Call),
    working_directory( _, OldD),
    set_prolog_flag(autoload,OldAutoload),
    !,
    '$exec_initialization_goals'(Y),
    close(Stream).

'$load_stream__'(Type,File,Stream, Y, M, Opts, Call) :-
    '$mk_opts'(Opts,File,Stream,M,Call,TOpts),
    b_setval('$opts',Opts),
    '$lf'(always, Type, File, Y,  Stream, M, Call, Opts, TOpts),
    '$exec_initialization_goals'(Y),
    close(Stream),
     !.

    
'$load_file__'(Type,File,Stream, Y, M, Opts, Call) :-
    '$mk_opts'(Opts,File,Stream,M,Call,TOpts),
    '$mk_file_opts'(TOpts) ,
    (
	'$memberchk'(if(If), Opts)
    ->
    true
    ;
    If = true
    ),
    (
	'$memberchk'(qcompile(QCompiling), Opts)
    ->
    true
    ;
    QCompiling = never
    ),
    '__NB_getval__'('$qcompile', ContextQCompiling, (ContextQCompiling = never)),
    nb_setval('$qcompile', QCompiling),
    '$lf'(If, Type, File, Y,  Stream, M, Call, Opts, TOpts),
     nb_setval('$qcompile', ContextQCompiling).


% consulting from a stream
'$lf'(not_loaded, _Type,_UserFile,File, _Stream, HostM, _Call, Opts, _TOpts) :-
    '$file_loaded'(File, HostM, DonorM), !,
    '$import_module'(DonorM, HostM,File, Opts).
'$lf'(unchanged, _Type,_UserFile,File,_Stream, HostM, _Call, Opts, _TOpts) :-
    '$file_unchanged'(File, HostM, DonorM), !,
    '$import_module'(DonorM, HostM,File, Opts).
'$lf'(_, _, _UserFile,File,_Stream, _ContextModule, _Call, _TOpts) :-
    '$being_consulted'(File),
    !.
'$lf'(_, qly, _UserFile,File,Stream, OuterModule, _Call, _Opts, TOpts) :-
    % check if there is a qly file
		(
	  '$q_header'( Stream, Type ),
	 Type == file
	->
	 !
	 ;
	fail
	),
	stream_property(Stream, file_name(Y)),
       '$qload_file'(Stream, OuterModule, File, Y, _Imports, TOpts).
'$lf'(_, _Type, UserFile,File,Stream, OuterModule, _Call, Opts, TOpts) :-
    !,
    prompt1(': '), prompt(_,'     '),
    %	format( 'I=~w~n', [Verbosity=UserFile] ),
    % export to process
    '$conditional_compilation_get_state'(State),
    '$conditional_compilation_init',
    (
     '$memberchk'(consult(Reconsult0), Opts)
      ->
       			  true ;
      Reconsult0 = reconsult
    ),
    '$lf_storefile'(File, UserFile, OuterModule, Reconsult0, Reconsult, TOpts, Opts),
   ( Reconsult \== consult ->
	'$start_reconsulting'(File),
	'$start_consult'(Reconsult,File,Stream,LC)
   ;
	'$start_consult'(Reconsult,File,Stream,LC)
   ),
    (
     '$memberchk'(skip_unix_header(SkipUnixHeader), Opts)
      ->
       			  true ;
      SkipUnixHeader = true
    ),
    '$report'(in, OldLoadVerbose,T0,H0,OuterModule,UserFile,Opts),

   (
	'$memberchk'(source_module(M1),Opts)
   ->
   true
   ;
   M1 = OuterModule
   ),
   current_source_module(_M0,M1),
   '$loop'(Stream,Reconsult),
   	( LC == 0 -> prompt(_,'   |: ') ; true),
    '$conditional_compilation_set_state'(State),
    current_source_module(_OM,_M0),
    ('$module'(File,InnerModule,_,_) ->
	'$check_module'(File,InnerModule)
   ;
   InnerModule=M1),
	% surely, we were in run mode or we would not have included the file!
				% back to include mode!
%	'$memberchk'(must_be_module, Opts),
%	'$bind_module'(InnerModule, UseModule),
    '$import_module'(InnerModule, M1, File, Opts),
 '$report'(out, OldLoadVerbose,T0,H0,InnerModule,File,Opts),
    '$end_consult'.


'$lf_storefile'(File, UserFile, OuterModule, Reconsult0, Reconsult, TOpts, Opts) :-
    source_location(ParentF, Line),
    '$tell_loaded'(File, UserFile, OuterModule, ParentF, Line, Reconsult0, Reconsult,_Dir, TOpts, Opts),
    !.
'$lf_storefile'(_UserFile, _OuterModule, _Reconsult0, _Reconsult, _TOpts, _Opts) :- 
    !.

'$report'(in, OldLoadVerbose,T0,H0,_,UserFile, Opts) :-
    current_prolog_flag(verbose_load, OldLoadVerbose),
    (
	'$memberchk'(silent(Silent), Opts)
    ->
    ( Silent == true -> Verbose = false;
      Silent == false -> Verbose = true;
      Silent == on -> Verbose = false;
      Silent == off -> Verbose = true
    ),
    set_prolog_flag(verbose_load, Verbose)
    ;
    OldLoadVerbose = Verbose
    ), 
    (
	Verbose ==  true
    ->
    H0 is heapused, '$cputime'(T0,_),
    StartMessage = loading,
    print_message(informational, loading(StartMessage, UserFile))
    ;
    true
    ).
'$report'(out, OldLoadVerbose,T0,H0,InnerModule,File,_) :-
    (
	current_prolog_flag(verbose_load, true)
    ->    
    H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
    EndMsg = consulted,
    print_message(informational, loaded(EndMsg, File,  InnerModule, T, H))
    ;
    true),
    set_prolog_flag(verbose_load, OldLoadVerbose).

'$q_do_save_file'(File, UserF, TOpts ) :-
    '$lf_opt'(qcompile, TOpts, QComp),
    '$lf_opt'('$source_pos', TOpts, Pos),
    '$lf_opt'('$from_stream', TOpts, false),
    ( QComp ==  auto ; QComp == large, Pos > 100*1024),
    absolute_file_name(UserF,F,[file_type(qly),solutions(first),expand(true)]),
    !,
    '$qsave_file_'( File, UserF, F ).
'$q_do_save_file'(_File, _, _TOpts ).


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
	throw_error(instantiation_error,include(V)).
'$include'([], _) :- !.
'$include'([F|Fs], Status) :- !,
	'$include'(F, Status),
	'$include'(Fs, Status).
'$include'(File, Status) :-
    H0 is heapused, '$cputime'(T0,_),
    '$stream_and_dir'(File,Y,Dir,Stream),
    working_directory(Dir0, Dir),
    '$including'(OV, Y),
    stream_property(loop_stream,[encoding(Encoding)] ),
    set_stream(Stream, [alias(loop_stream),encoding(Encoding)] ),
    print_message(informational, loading(including, Y)),
    '$loop'(Stream,Status),
    close(Stream),
    H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
    current_source_module(Mod, Mod),
    print_message(informational, loaded(included, Y, Mod, T, H)),
    working_directory(_Dir, Dir0),
    '$including'(Y, OV).

'$stream_and_dir'(user,user_input,Dir,user_input) :-
	!,
        working_directory(Dir, Dir).
'$stream_and_dir'(user_input,user_input,Dir,user_input) :-
	!,
        working_directory(_Dir, Dir).
'$stream_and_dir'(File,Y,Dir,Stream) :-
    absolute_file_name(File, Y, [access(read),file_type(prolog),file_errors(fail),solutions(first),expand(true)]),
    ( open(Y, read, Stream) 	->
      true ;
      throw(error(permission_error(input,stream,Y),include(File)))
    ),
    file_directory_name(Y, Dir).



/**

  @pred ensure_loaded(+ _F_) is iso

When the files specified by  _F_ are module files,
ensure_loaded/1 loads them if they have note been previously
loaded, otherwise advertises the user about the existing name clashes
  and prompts about importing or not those predicates. Predicates which
are not public remain invisible.

When the files are not module files, ensure_loaded/1 loads them
if they have not been loaded before, and does nothing otherwise.

 _F_ must be a list containing the names of the files to load.
*/
ensure_loaded(Fs) :-
    load_files(Fs, [if(not_loaded), silent(true)]).

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
% 	throw_error(context_error(consult(Fs),cla ,query).
consult(V) :-
	var(V), !,
	throw_error(instantiation_error,consult(V)).
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

```
?- [file1, -file2, -file3, file4].
```
  will consult `file1` `file4` and reconsult `file2` and
`file3`. That is, it could be written as:

```
?- consult(file1),
   reconsult( [file2, file3],
   consult( [file4] ).
```

*/
reconsult(Fs) :-
	load_files(Fs, []).



%% @}


