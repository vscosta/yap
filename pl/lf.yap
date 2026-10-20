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
  */

/**
  * @addtogroup YAPReadFiles
  * @{
  */

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
% compilation_mode(compact,consult,reconsult,exo,db,assert_all_consult) => implemented
% register(true, false) => implemented
%

'$check_use_module'(use_module(_), use_module(_)) :- !.
'$check_use_module'(use_module(_,_), use_module(_)) :- !.
'$check_use_module'(use_module(M,_,_), use_module(M)) :- !.
'$check_use_module'(_, load_files) :- !.

%% @pred $load_files(Module, Path, Options)
%%
%% predicate call interface to file-loading machinery
'$load_files'(Fs,M, Opts) :-
    strip_module(Fs,M, Fs1),
    must_be_ground(M:Fs1),
    setup_call_cleanup(
	current_source_module(M0,M),
	'$pick_files_for_loading'(Fs1, M0, M, Opts),
	current_source_module(_,M0)
    ).

'$pick_files_for_loading'(List, M0, M, Opts) :-
    is_list(List),
    !,
    forall(lists:member(UserNamePath,List), 
     '$pick_file_for_loading'(UserNamePath, M0, M,Opts)
).
'$pick_files_for_loading'(UserNamePath, M0, M, Opts) :-
     '$pick_file_for_loading'(UserNamePath, M0, M,Opts).

    
'$pick_file_for_loading'(M:Fs, M0, _M, Opts) :-
    !,
    '$pick_file_for_loading'(Fs, M0, M, Opts).

'$pick_file_for_loading'(user, M0, M, Opts) :-
    !,
    '$load_stream'( user, user_input, M0, M, [ stream(user_input)|Opts] ).
'$pick_file_for_loading'(user_input, M0, M, Opts) :-
    !,
    '$load_stream'( user_input, user_input, M0, M, [ stream(user_input)|Opts] ).
'$pick_file_for_loading'(S, M0, M, Opts) :-
     string(S),
    !,
    atom_string(UserNamePath,S),
    '$pick_file_for_loading'(UserNamePath, M0, M, Opts).
'$pick_file_for_loading'(-UserNamePath, M0, M, Opts) :-
!,
    '$pick_file_for_loading'(UserNamePath, M0, M, Opts).
'$pick_file_for_loading'(StreamName, M0, M, Opts) :-
    lists:member(stream(Stream), Opts),
    strip_module(M:StreamName,M,Name),
    atom(Name),
    !,
   '$load_stream'(  Name, Stream, M0, M, Opts ).
'$pick_file_for_loading'(UserNamePath, _M0, M, Opts) :-
    % first argument s
    lists:member(consult(db),Opts),
    !,
    dbload(UserNamePath, M).    
'$pick_file_for_loading'(UserNamePath,_M0, _M, Opts ) :-
    % first argument 
    lists:member( consult(exo),Opts),
    !,
    exoload(UserNamePath ).
%% Prolog stream
'$pick_file_for_loading'(UserNamePath, M0, M, Opts) :-
    '$load_file'(UserNamePath, M0, M, Opts).

'$load_file'(UserNamePath, M0, M, Opts) :-
     writeln(UserNamePath:'$file_to_abs_path'),
    '$file_to_abs_path'(UserNamePath,AbsPath, Opts),
     writeln('$acces_path'),
    '$accept_path'(AbsPath,M, Opts),
     writeln('prepare_path'),
    '$prepare_load_file'(UserNamePath,AbsPath,M,OuterModule,Opts, Reconsult, OldLoadVerbose,Verbose,T0,H0,D0,Source,State),
     writeln('pen_path'),
    '$open_stream'(AbsPath, Opts, Stream),
     writeln('prepare_loop'),
    '$prepare_loop'(Stream,Reconsult,AbsPath, Opts),
     writeln(do_loop),
writeln(loop:Stream+Reconsult),
    '$loop'(Stream, Reconsult),
     writeln(xlose),
    '$close_stream'(Stream,AbsPath, Opts, OldLoadVerbose,Verbose,T0,H0,D0,Source,M0, OuterModule,State).

'$load_stream'(StreamName,Stream,M0, M, Opts) :-
    '$prepare_load_stream'(StreamName,M,OuterModule,Opts, Reconsult, OldLoadVerbose,Verbose,T0,H0,D0,Source,State),
    '$prepare_loop'(Stream, Reconsult, StreamName, Opts),
    '$loop'(Stream, Reconsult),
    '$close_stream'(Stream, StreamName,Opts, OldLoadVerbose,Verbose,T0,H0,D0,Source,M0,OuterModule,State).

'$file_to_abs_path'(user,user_input, _Opts) :-
    !.
'$file_to_abs_path'(user_input,user_input, _Opts) :-
    !.
'$file_to_abs_path'(UserName, AbsPath, Opts) :-   
   ( lists:member(expand(Expand),Opts) -> true ; Expand = true ),
    (   absolute_file_name(UserName, AbsPath, [access(read),file_type(prolog),file_errors(fail),solutions(first),expand(Expand)]) 
     ->
     Type = prolog
	;
     absolute_file_name(UserName, AbsPath, [access(read),file_type(qly),file_errors(fail),solutions(first),expand(Expand)])
     ->
     Type = qly
	;
     throw(error(existence_error(source_sink,UserName)))
     ).


'$accept_path'(user, _, _Opts) :-
    !.
'$accept_path'(user_input, _, _Opts) :-
    !.
% first case: we have the file already loaded, or at
% list declared, so we can reuse it.
'$accept_path'(AbsPath, OuterModule, Opts) :-
    ( lists:member(if(If), Opts) -> true ; If = true),
    '$accept'(If,AbsPath,InnerModule,OuterModule),
    !,
    '$import_module'(InnerModule,OuterModule, AbsPath, Opts),
    fail.
%
%
% case 2: we have a loop
'$accept_path'(AbsPath, _, _Opts) :-
    '__NB_getval__'('$consulting_file', [F|Fs], fail),
    % % writeln(l(AbsPath, Files)),
    lists:member( l(AbsPath,_), [F|Fs]),
    !,
    throw(error(compile_loop(AbsPath),compiling([F|Fs])),load_files(AbsPath)).
%
% case 3: we can load the file.
%

'$accept_path'(_AbsPath, _, _Opts).
    
'$accept'(not_loaded,UserNamePath,InnerModule,OuterModule) :-
     '$file_loaded'(UserNamePath, OuterModule, InnerModule),
    !.
'$accept'(unchanged,UserNamePath,InnerModule, OuterModule) :-
   '$file_unchanged'(UserNamePath, OuterModule, InnerModule),
    !.

'$prepare_load_file'(UserNamePath,AbsPath,M,OuterModule,Opts, Reconsult,OldLoadVerbose,Verbose,T0,H0,D0,_Source,State) :-
writeln(UserNamePath),
    '$conditional_compilation_get_state'(State),
writeln(st=State),
    '$set_lf_dir'(AbsPath, D0, _DF),
writeln(st=D0),
    '$set_lf_module'(M, OuterModule, Opts),
writeln(m=OuterModule),
    '$set_consult_stack'(AbsPath, Reconsult, Parentf, Opts),
    writeln(cs=OuterModule),
    prolog_load_context(term_position, Pos),
    '$lf_storefile'(AbsPath, UserNamePath, OuterModule,Parentf, Opts, Pos),
    (
     true %Reconsult == reconsult
     ->
     unload_file(AbsPath)
     ;
     true
     ),
'$report'(in, OldLoadVerbose,Verbose,T0,H0,OuterModule,UserNamePath,Opts).

'$set_lf_module'(M, OuterModule, Opts) :-
% '$conditional_compilation_get_state'(State),
    (
	lists:member(module(OuterModule),Opts)
   ->
     current_source_module(_,OuterModule)
   ;
   M = OuterModule
    ).


'$set_lf_dir'(AbsPath, D0, DF) :-
    (
	file_directory_name(AbsPath,DF)
->

	working_directory(D0,DF)
      ;
	working_directory(D0,D0), D0= DF
).

'$set_consult_stack'(AbsPath, Reconsult, Parentf, Opts) :-
(
'__NB_getval__'('$consulting_file', Files0, fail),
Files0 = [l(Parentf,Reconsult0)|_]
      ->
    Files = Files0
      ;
    Files=[],
    Reconsult0 = reconsult,
    Parentf =  user_input
 ),
writeln(Files:Opts),    (
lists:member(reconsult(Reconsult), Opts)   ->
	true
      ;
    Reconsult = Reconsult0
     ),
    nb_setval('$consulting_file',[l(AbsPath,Reconsult)|Files]).

'$prepare_load_stream'(StreamName,M,OuterModule, Opts, Reconsult, OldLoadVerbose,Verbose,T0,H0,D0,_Source,State) :-
     '$conditional_compilation_get_state'(State),
    '$set_lf_module'(M, OuterModule, Opts),
%writeln(m=OuterModule),
    '$set_consult_stack'(StreamName, Reconsult, _Parentf, Opts),
%writeln(cs=OuterModule),
    working_directory(D0,D0),  
    '$report'(in, OldLoadVerbose,Verbose,T0,H0,OuterModule,StreamName,Opts).

'$open_stream'(AbsPath, Opts, Stream) :-
    (
     lists:member(encoding(Enc),Opts)
     ->		    
     Encoding = [encoding(Enc)]
	;
     Encoding =[]
     ),
    open(AbsPath, read, Stream,Encoding).


'$prepare_loop'(Stream, Reconsult, File, Opts) :-
     working_directory(Dir,Dir),
    '$start_consult'(Reconsult,File,Dir,Stream,_LC),
    (
lists:member(skip_unix_header(Skip), Opts)
     ->
     true ;
     Skip = true
     ),
    (
     Skip == true ->
     '$skip_unix_header'(Stream)
	;
     true
     ).
 
'$close_stream'(Stream, File,Opts, OldLoadVerbose,Verbose,T0,H0,D0,_Source,M0,OuterModule,State ):-
 writeln(f=Stream),
   (
    '$module'(File,InnerModule,_,_)
     ->
     '$import_module'(InnerModule, OuterModule, File, Opts),
      '$check_module'(File,InnerModule)
     ;
     InnerModule=OuterModule
     ),
    '$conditional_compilation_set_state'(State),
    '$report'(out, OldLoadVerbose,Verbose,T0,H0,InnerModule,File,Opts),
    current_source_module(_,M0),
    '$end_consult'(D0),
writeln(d=D0),
    nb_getval('$consulting_file', [_|Files]), 
     nb_setval('$consulting_file', Files), 
    '$exec_file_initialization_goals'(File),
 close(Stream).
    
'$skip_unix_header'(Stream) :-
   peek_char(Stream, '#'), !, % 35 is ASCII for '#[
    skip(Stream, 10),
    '$skip_unix_header'(Stream).
'$skip_unix_header'(_).


'$loop'(Stream, Reconsult) :-
    repeat,
    catch(
     '$loop_'(Stream,Reconsult),
     _Error,
     error_handler),
    !.

'$loop_'(Stream, Status) :-
	 enter_compiler(Stream,Status) .

enter_compiler(Stream,Status) :-
    prompt1(': '), prompt(_,'     '),
    Options = [syntax_errors(dec10),variable_names(Vars), term_position(Pos)],
    read_clause(Stream, Clause, Options),
    (
      Clause == end_of_file
     ->
     !
	;
     '$conditional_compilation_skip'(Clause)
     ->
     fail
	;
    call_compiler(Clause, Status,Vars,Pos),
     fail
     ).


%%
% @pred '$go_compile_clause'(G,Vs,Pos, Where, Source) is det
%
% interfaces the loader and the compiler
% not 100% compatible with SICStus Prolog, as SICStus Prolog would put
% module prefixes all over the place, although unnecessarily so.
%
% @param [in] _G_ is the clause to compile
% @param [in] _Vs_ a list of variables and their name
% @param [in] _Pos_ the source-code position
% @param [in] _N_  a flag telling whether to add first or last
% @param [out] _Source_ the user-tranasformed clause
call_compiler((:-G),Status,VL,Pos) :-
    !,
    % allow user expansion
    expand_term((:- G), O, _ExpandedClause),
    '$yap_strip_module'(O, NM, NO),
    (
     NO = (:- G1)
     ->
     must_be_callable(NM:G1),
     '$process_directive'(G1, Status , NM, VL, Pos)
	;
     '$goal'(G1,VL,Pos)).
call_compiler((?-G),_, VL, Pos) :-
    !,
    '$goal'(G,VL, Pos).
call_compiler(G, Where,_VL, Pos) :-
    current_source_module(SM,SM),
    expand_term(G, Source,EC),
    '$head_and_body'(EC, MH, B ),
    strip_module( MH, Mod, H),
    (
     '$undefined'(H, Mod)
     ->
     '$handle_import_conflict'(H, Mod)
	;
     true
     ),
    ( B==true
     ->
     '$compile'((Mod:H), Where, Mod:H, SM, Pos, [])
	;
     '$compile'((Mod:H:-B), Where, Source, SM, Pos, [])
     ).


'$lf_storefile'(File, UserFile, OuterModule,ParentF, Opts, Pos) :-
    '$tell_loaded'(File, UserFile, OuterModule, ParentF, Pos,_Dir, Opts),
    !.
'$lf_storefile'(_, _UserFile, _OuterModule, _, _Opts, _Pos).

'$report'(in, OldLoadVerbose,Verbose,T0,H0,_,UserFile, Opts) :-
    current_prolog_flag(verbose_load, OldLoadVerbose),
    (
     lists:member(silent(Silent), Opts)
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
    % required to boot
    (
     Verbose == true
     ->
     H0	 is heapused, '$cputime'(T0,_),
     StartMessage = loading,
     print_message(informational, loading(StartMessage, UserFile))
	;
     true
     ).

'$report'(out, OldLoadVerbose,Verbose,T0,H0,InnerModule,File,_) :-
       % required to boot
    (
     Verbose == true
     ->
     H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
     EndMsg = consulted,
     print_message(informational, loaded(EndMsg, File,  InnerModule, T, H))
	;
     true
     ),			 
    set_prolog_flag(verbose_load, OldLoadVerbose).



/**
  @pred include(+ _F_) is directive

  The `include` directive adds the text files or sequence of text
  files specified by  _F_ into the files being currently consulted.
  It may be used
  as an replacement to consult/1 by allowing the programmer to include a
  data-base
  split into several files.
*/	

include(Fs) :-
    strip_module(Fs,_M,S),
     '$include'(S).

'$include'(V) :- var(V), !,
    throw_error(instantiation_error,include(V)).
'$include'([]) :- !.
'$include'([F|Fs]) :- !,
    '$include'(F),
    '$include'(Fs).
'$include'(File) :-
    H0 is heapused, '$cputime'(T0,_),
    '$stream_and_dir'(File,Y,Dir,Stream),
    working_directory(Dir0, Dir),
    prolog_load_context(stream,Old),
    stream_property(loop_stream,[encoding(Encoding)] ),
    ignore(recordzifnot('$includes', (Old ->Y),_)),
    set_stream(Stream, [encoding(Encoding), alias(include_stream)] ),
    print_message(informational, loading(including, Y)),
    '$loop'(Stream),
    close(Stream),
    H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
    current_source_module(Mod,Mod),
    print_message(informational, loaded(included, Y, Mod, T, H)),
    working_directory(_Dir, Dir0).

'$stream_and_dir'(F,F,Dir,_Stream) :-
    file_directory_name(F,'.'),
    !,
    working_directory(_Dir, Dir).
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
1
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

/**
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
consult(Fs) :-
    load_files(Fs,[consult(consult)]).


/**

@pred reconsult(+ _F_ )
  @pred compile(+ _F_ )


Updates the program by replacing the
previous definitions for the predicates defined in  _F_. 

YAP also offers no difference between consult/1 and compile/1. The two
are  implemented by the same code.

Example:

```
?- [file1, -file2, -file3, file4].
```
  will consult `file1` `file4` and   reconsult `file2` and
`file3`. That is, it could be written as:

```
?- consult(file1),
      reconsult( [file2, file3],
   consult( [file4] ).
```

*/
reconsult(Fs) :-
	load_files(Fs, [reconsult(consult)]).

compile(Fs) :-
	reconsult(Fs).

compile_clauses(Commands) :-
	 '$active_predicate'(P),
    (
     member(C,Commands),
     compile_clause(C),
     fail;
     '$active_predicate'(P)
     ).
 

compile_clause(EC) :-
    current_source_module(SM,SM),
    prolog_load_context(term_position, Pos),
    % prolog_load_context(variable_names, Vs),
    '$head_and_body'(EC, MH, B ),
    strip_module( MH, Mod, H),
    (
     '$undefined'(H, Mod)
     ->
     '$handle_import_conflict'(H, Mod)
	;
     true
     ),
    ( B==true
     ->
     '$compile'((Mod:H),  consult, EC, SM, Pos, [])
	;
     '$compile'((Mod:H:-B), consult, EC, SM, Pos, [])
     ),
    current_source_module(_,SM).




%% @}


