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

%% @pred $load_files(Module, Path, Options, InitialGoal)
%%
%% predicate call interface to file-loading machinery
'$load_files'(Fs, Opts) :-
    '_exec_load_files'(Fs, Opts).

%% @pred $load_files(Paths, Module, Options, consult/reconsult/top/goal, Vars, CallerLocation)
%%

'_exec_load_files'(Fs,  Opts) :-
    must_be_ground(Fs),
%    current_prolog_flag(compiler_skip,true),
%    start_low_level_trace,
     % writeln(in:Fs),
    setup_call_cleanup(
	current_source_module(M0,M0),
	'$pick_files_for_loading'(Fs, Opts),
	current_source_module(_,M0)
    ),
%    stop_low_level_trace,
%    '$halt'(0),
    fail.
'_exec_load_files'(_Fs,_Opts).

    

    
'$pick_files_for_loading'(user, Opts) :-
!,
    '$load_stream'( user, user_input, [ stream(user_input)|Opts] ).
'$pick_files_for_loading'(user_input, Opts) :-
!,
    '$load_stream'( user_input, user_input, [ stream(user_input)|Opts] ).
'$pick_files_for_loading'(S, Opts) :-
     string(S),
    !,
    atom_string(UserNamePath,S),
    '$pick_files_for_loading'(UserNamePath, Opts).
'$pick_files_for_loading'(M:Fs,Opts) :-
    !,
    setup_call_cleanup(
    current_source_module(M0,M),
    '$pick_files_for_loading'(Fs, Opts),
    current_source_module(_,M0)
    ).
'$pick_files_for_loading'(-UserNamePath, Opts) :-
!,
    '$pick_files_for_loading'(UserNamePath, [consult(reconsult)|Opts]).
'$pick_files_for_loading'(Name, Opts) :-
    '$memberchk'(stream(Stream), Opts),
    atom(Name),
    !,
   '$load_stream'( Name, Stream, Opts ).
'$pick_files_for_loading'(UserNamePath, Opts) :-
    % first argument s
 '$memberchk'(consult(db),Opts),
    !,
    dbload(UserNamePath).
 '$pick_files_for_loading'(UserNamePath, Opts ) :-
    % first argument 
    '$memberchk'( consult(exo),Opts),
 !,
    exoload(UserNamePath ).
%% Prolog stream
'$pick_files_for_loading'(List, Opts) :-
     is_list(List),
    !,
    lists:member(UserNamePath,List),
    '$pick_files_for_loading'(UserNamePath,Opts).
    '$pick_files_for_loading'(UserNamePath, Opts) :-
   '$file_to_abs_path'(UserNamePath,AbsPath, Opts),
    '$load_file'(UserNamePath, AbsPath, Opts).

'$load_file'(_UserNamePath, AbsPath, _Opts) :-
    '__NB_getval__'('$consulting_file', AbsPaths, fail),
    % writeln(aATTTTin:AbsPath:AbsPaths),
    '$memberchk'( l(AbsPath,_), AbsPaths),
    !.
'$load_file'(_UserNamePath, AbsPath, Opts) :-
current_source_module( OuterModule, OuterModule),
    ('$memberchk'(if(If),Opts) -> true ; If = true ),
  '$use_loaded_file'(If, AbsPath, OuterModule, InnerModule),
    !,
    '$import_module'(InnerModule,OuterModule,AbsPath, Opts).
    
'$load_file'(UserNamePath, AbsPath, Opts) :-
    % writeln('$acces_path'=AbsPath),
%(atom_concat(_,'lists.yap', AbsPath)
  setup_call_cleanup(
      (
working_directory(MyD0,MyD0),
      current_source_module(OuterModule, OuterModule)
      ),
    (
% writeln(plf),
      '$prepare_load_file'(UserNamePath,AbsPath,OuterModule,Opts,
    OldLoadVerbose,Verbose,T0,H0,D0,Reconsult,Source),
     % writeln('pen_path'),
    '$open_stream'(AbsPath, Opts, Stream),
    % % writeln('prepare_loop'),
    !,
    '$prepare_loop'(Stream,AbsPath),
     % writeln(do_loop),
    '$do_loop'(Stream, Reconsult),
    % writeln(xlose),
    '$close_stream'(Stream,AbsPath, Opts, OldLoadVerbose,Verbose,T0,H0,D0,Source, OuterModule)
    ),
    (
current_source_module(_, OuterModule),
working_directory(_,MyD0)
    )
    )
.


'$load_stream'(StreamName,Stream, Opts) :-
    set_stream(Stream,file_name(StreamName)),
    current_source_module(OuterModule, OuterModule),
    '$prepare_load_file'(StreamName,StreamName,OuterModule,Opts, OldLoadVerbose,Verbose,T0,H0,D0,Reconsult,Source),
  '$prepare_loop'(Stream,StreamName),
  b_getval('$consulting_file',NAbsPaths),
  b_setval('$consulting_file',[l(StreamName,Reconsult)|NAbsPaths]),
  '$do_loop'(Stream, Reconsult),
  '$close_stream'(Stream, StreamName,Opts, OldLoadVerbose,Verbose,T0,H0,D0,Source,OuterModule).

'$file_to_abs_path'(UserName,AbsPath, Opts) :-   
   ( '$memberchk'(expand(Expand),Opts) -> true ; Expand = true ),
    (   absolute_file_name(UserName, AbsPath, [access(read),file_type(prolog),file_errors(fail),solutions(first),expand(Expand)]) ->
     Type = prolog
	;
     absolute_file_name(UserName, AbsPath, [access(read),file_type(qly),file_errors(fail),solutions(first),expand(Expand)])
     ->
     Type = qly
	;
     throw(error(existence_error(source_sink,UserName),[]))
     ).

'$do_loop'(Stream, Reconsult) :-
    '$loop'(Stream, Reconsult).

'$prepare_load_file'(UserNamePath,AbsPath,OuterModule,Opts, OldLoadVerbose,Verbose,T0,H0,D0,Reconsult,_Source) :-
   % writeln(l(AbsPath)),
% writeln(cc),
   '$set_source_module'(Opts,OuterModule, M),
% writeln(sm),
   '$set_compilation_mode_and_parent'(AbsPath, Opts, Parentf, Reconsult), 
% writeln(mode),
   '$set_working_directory'(AbsPath, D0, Opts), 
% writeln(f),
   '$loaded_file_info'(AbsPath, UserNamePath, M,Parentf, Reconsult, Opts),

   % writeln(start/AbsPath),

    '$report'(in, OldLoadVerbose,Verbose,T0,H0,M,UserNamePath,Opts).  

'$set_source_module'(Opts,M0, M) :-
   ( 
	'$memberchk'(source_module(M),Opts)
   ->
       current_source_module(_,M )
     ;
   current_source_module( M0      ,M0 )
   ).

'$set_compilation_mode_and_parent'(AbsPath, Opts, Parentf, Reconsult) :-
   '__NB_getval__'('$consulting_file', AbsPaths, fail),
(
     AbsPaths = [l(Parentf,Reconsult0)|_]
     ->
     NAbsPaths = AbsPaths
    ;
 Parentf = user_input,
 Reconsult0 = reconsult,
     NAbsPaths = [l(user_input,reconsult)]
     ),
    ( %'$memberchk'(compilation_mode(Reconsult), Opts)   ->
     '$memberchk'(consult(Reconsult), Opts)   ->
     true
	;
     Reconsult = Reconsult0
     ),
    b_setval('$consulting_file',[l(AbsPath,Reconsult)|NAbsPaths]).

'$set_working_directory'(AbsPath, D, Opts) :-
   (  '$memberchk'(stream(_), Opts)
       ->

       working_directory(D,D)
     ;
   file_directory_name(AbsPath,D)
),
   working_directory(_,D).

'$open_stream'(AbsPath, Opts, Stream) :-
    (
     '$memberchk'(encoding(Enc),Opts)
     ->		    
     Encoding = [encoding(Enc)]
	;
     Encoding =[]
     ),
    open(AbsPath, read, Stream,Encoding),
    (
     '$memberchk'(skip_unix_header(Skip), Opts)
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

'$prepare_loop'(Stream, AbsPath) :-
    working_directory(Dir,Dir),
    '$start_consult'(reconsult,AbsPath,Dir,Stream,_LC).

'$close_stream'(Stream, AbsPath,Opts, OldLoadVerbose,Verbose,T0,H0,D0,_Source,OuterModule) :-
%    '$conditional_compilation_pop',
%    ( '$memberchk'(import(Preds), Opts) -> true ; Opts = all),
    (
'$module'(AbsPath,InnerModule,_,_)
	->
     '$import_module'(InnerModule, OuterModule, AbsPath, Opts),
     '$check_module'(AbsPath,InnerModule);
     current_source_module(InnerModule,InnerModule)
     ),
     '$report'(out, OldLoadVerbose,Verbose,T0,H0,InnerModule,AbsPath,Opts),
    current_source_module(_,OuterModule) ,
    '$end_consult'(D0),
    close(Stream),
    ignore('$exec_file_initialization_goals'(AbsPath)).

    
'$skip_unix_header'(Stream) :-
    peek_char(Stream, '#'), !, % 35 is ASCII for '#
    skip(Stream, 10),
    '$skip_unix_header'(Stream).
'$skip_unix_header'(_).


'$loop'(Stream,Status) :-
    repeat,
    catch(
     '$loop_'(Stream,Status),
     _Error,
     error_handler),
    !.

'$loop_'(Stream,Status) :-
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
        '$conditional_compilation_get_state'(State),          
	(
	    Clause = (:- Goal),	    
	    '$ifctrl'( Goal,  State)
	    ->
	    fail 
	    ;
	    State == run
	      ->
	    call_compiler(Clause, Status,Vars,Pos),
    fail
    )


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

'$report'(in, OldLoadVerbose,Verbose,T0,H0,_,UserAbsPath, Opts) :-
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
    % required to boot
     H0	 is heapused, '$cputime'(T0,_),
     StartMessage = loading,
(Verbose == true
  ->
  print_message(informational, loading(StartMessage, UserAbsPath))
    ;
  true
     ).

'$report'(out, OldLoadVerbose,Verbose,T0,H0,InnerModule,AbsPath,_) :-
       % required to boot
    (
     Verbose == true
     ->
     H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
     EndMsg = consulted,
     print_message(informational, loaded(EndMsg, AbsPath,  InnerModule, T, H))
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
'$include'(AbsPath) :-
    H0 is heapused, '$cputime'(T0,_),
    '$stream_and_dir'(AbsPath,Y,Dir,Stream),
     working_directory(Dir0, Dir),
    stream_property(loop_stream,[encoding(Encoding),file_name(Old)] ),
    ignore(recordzifnot('$includes', (Old ->Y),_)),
    set_stream(Stream, [encoding(Encoding), alias(include_stream)] ),
    print_message(informational, loading(including, Y)),
    b_getval('$consulting_file',[l(_,Reconsult)|_AbsPaths]), 
    '$loop'(Stream,Reconsult),
    close(Stream),
    H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
    current_source_module(Mod,Mod),
    print_message(informational, loaded(included, Y, Mod, T, H)),
    working_directory(_Dir, Dir0).

'$stream_and_dir'(AbsPath,Y,Dir,Stream) :-
    absolute_file_name(AbsPath, Y, [access(read),file_type(prolog),file_errors(fail),solutions(first),expand(true)]),
open(Y,read,Stream),
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
  will consult `file1` `file4` and reconsult `file2` and
`file3`. That is, it could be written as:

```
?- consult(file1),
      reconsult( [file2, file3],
   consult( [file4] ).
```

*/
reconsult(Fs) :-
	load_files(Fs, [consult(reconsult)]).

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


