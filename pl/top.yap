


live :- '$live'.

'$live' :-
    repeat,
    current_source_module(Module,Module),
    set_prolog_flag(verbose,normal),
    ( Module==user ->
      true % '$compile_mode'(_,0)
    ;
    format(user_error,'[~w]~n', [Module])
    ),
    '$system_catch'('$enter_top_level',Module,Error,'$Error'(Error)).

% Start file for yap

/*		I/O predicates


%%   Meaning of flags for '$write' is
%% 	  1	quote illegal atoms
%% 	  2	ignore operator declarations
%% 	  4	output '$VAR'(N) terms as A, B, C, ...
%% 	  8	use portray(_)
%% */

/* main execution loop							*/
'$read_toplevel'(Goal, Bindings, Pos) :-
    '$prompt',
    read_term(user_input,
		    Goal,
		    [variable_names(Bindings), syntax_errors(dec10), term_position(Pos)]).



/** @pred  stream_property( Stream, Prop )

*/

% reset alarms when entering top-level.
'$enter_top_level' :-
    alarm(0, 0, _, _),
    fail.
'$enter_top_level' :-
    '$clean_up_dead_clauses',
    fail.
'$enter_top_level' :-
    current_prolog_flag(debug, DebugOK),
    '$set_debugger_state'(debug, DebugOK),
    fail.
'$enter_top_level' :-
    get_value('$top_level_goal',GA), GA \= [], !,
    set_value('$top_level_goal',[]),
    '$run_atom_goal'(GA),
    fail.
'$enter_top_level' :-
    flush_output,
    '$run_toplevel_hooks',
     prompt1(' ?- '),
    '$read_toplevel'(Command,Varnames,Pos),
    nb_setval('$spy_gn',1),
    % stop at spy-points if debugging is on.
    '$init_debugger_trace',
    '$command'(Command,Varnames,Pos,top),
    (    current_prolog_flag(break_level, BreakLevel),

	BreakLevel \= 0
    ->
    true
    ;
    '$pred_exists'(halt(_), user)
    ->
    halt(0)
    ;
    '$halt'(0)
    ).

'$erase_sets' :-
    eraseall('$'),
    eraseall('$$set'),
    eraseall('$$one'),
    eraseall('$reconsulted'), fail.
'$erase_sets' :- \+ recorded('$path',_,_), recorda('$path',[],_).
'$erase_sets'.

'$start_corouts' :-
    eraseall('$corout'),
    eraseall('$result'),
    eraseall('$actual'),
    fail.
'$start_corouts' :-
    recorda('$actual',main,_),
    recordz('$corout','$corout'(main,main,'$corout'([],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[])),_Ref),
    recorda('$result',going,_).

%
% Hack in case expand_term has created a list of commands.
%
'$execute_commands'(V,_,_,_,_,_) :- var(V), '$error'(instantiation_error).
'$execute_commands'(C,M,VL,Pos,Con,Source) :-
    '$system_catch'('$execute_command'(C,M,VL,Pos,Con,Source),prolog,Error,'$Error'(Error, Con)).
%
%
%

assert_in_program(G0) :-
        '$yap_strip_module'(G0, M, G),
    '$execute_command'((G),M,[],0,top,_).
 
'$execute_command'((:-G),M,VL,Pos,Option,_) :-
   !,			% allow user expansion
    expand_term((:- M:G), O, _),
    '$yap_strip_module'(O, NM, NO),
    (
        NO = (:- G1)
    ->
    must_be_callable(G1),
    '$process_directive'(G1, Option, NM, VL, Pos)
    ;
    '$execute_commands'(G1,NM,VL,Pos,Option,O)
    ),
    fail.
'$execute_command'((?-G), M, VL, Pos, top, Source) :-
    !,
    '$execute_command'(G, M, VL, Pos, top, Source).
'$execute_command'(G, M, VL, Pos, Option, Source) :-
    strip_module(M:G,NM,NG),
    '$continue_with_command'(Option, VL, Pos, NM:NG, Source).


/** @pred  expand_term( _T_,- _X_)

  This user-defined predicate is called by YAP after
  reading goals and clauses.

  - _Module_:`expand_term`( _T_ , _X_) is called first on the
  current source module _Module_ ; if i
  - `user:expand_term`( _T_ , _X_ `)` is available on every module.

This predicate is used by YAP for preprocessing each top level
term read when consulting a file and before asserting or executing it.
It rewrites a term  _T_ to a term  _X_ according to the following
rules: first try term_expansion/2  in the current module, and then try to use the user defined predicate user:term_expansion/2`. If this call fails then the translating process
for DCG rules is applied, together with the arithmetic optimizer
whenever the compilation of arithmetic expressions is in progress.




*/
expand_term(Term,Expanded) :-
    expand_term(Term,Expanded,_).


expand_term(_T,[],[]) :-
    '$conditional_compilation_skip',
    !.
expand_term( [H|T], [H|T], [H|T]) :-
    !.
expand_term( Term, UExpanded,  Expanded) :-
   (
        '$do_term_expansion'(Term,TermI)
    ->
    true
    ;
      Term=TermI
   ),
   (
       TermI = [_|_]
   ->
   '$member'(T,TermI)
   ;
   T = TermI
   ),
   '$expand_term_grammar'(T,TI),
   expand_clause(TI, UExpanded, Expanded).
%writeln(e:(Term,' ----------------------->  ',Expanded)).

'$continue_with_command'(consult,V,Pos,G,Source) :-
    '$go_compile_clause'(G,V,Pos,consult,Source),
    !,
    fail.
'$continue_with_command'(reconsult,V,Pos,G,Source) :-
    '$go_compile_clause'(G,V,Pos,reconsult,Source),
    !,
    fail.
'$continue_with_command'(top,Names,_,G,_) :-
    prolog_flag(prompt_alternatives_on, OPT),
    (
	query_to_answer(G,Names,Port,GVs,LGs),
	'$another'(Names, GVs, LGs, Port, OPT)
    ;
    print_message(help,false)
    ),
    !,
    fail.

'$expand_program_goal'(G,G,G) :-
    is_list(G),
    !.
'$expand_program_goal'(G,GN,GO) :-
    '$expand_term'(G,GN,GO).
    

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
'$go_compile_clause'(G, _Vs, Pos, Where, Source) :-
    '$$compile'(G, Where, Pos, Source, _),
    !.
'$go_compile_clause'(G,_Vs,_Pos, _Where, _Source) :-
    throw(error(system, compilation_failed(G))).

'$$compile'(C, Where, C0, Pos, R) :-
    strip_module( C, M, CN),
    '$head_and_body'( CN, MH, B ),
    strip_module( M:MH, Mod, H),
    (
	'$undefined'(H, Mod)
    ->
     '$handle_import_conflict'(H, Mod)
    ;
    true
    ),
    %        writeln(Mod:((H:-B))),
    '$compile'((H:-B), Where, Pos, Mod, C0, R).

/*'$init_pred'(H, Mod, _Where ) :-
    '$import'(NM,Mod,NH,H,_,_),
    %    NM \= Mod,
    functor(NH,N,Ar),
    functor(H,ON,Ar),
    print_message(warning,redefine_imported(Mod,NM,Mod:N/Ar)),
    abolish(Mod:ON/Ar),
    '$import'(NM,Mod,NH,H,_,_),
    fail.
*/
'$init_pred'(H, Mod, Where ) :-
    '$init_as_dynamic'(Where),
    !,
    functor(H, Na, Ar),
    '$dynamic'(Na/Ar, Mod).
'$init_pred'(_H, _Mod, _Where ).

'$init_as_dynamic'( asserta ).
'$init_as_dynamic'( assertz ).
'$init_as_dynamic'( consult ) :-
    '__NB_getval__'('$assert_all',on,fail).
'$init_as_dynamic'( reconsult ) :-
    '__NB_getval__'('$assert_all',on,fail).

'$check_if_reconsulted'(N,A) :-
    once(recorded('$reconsulted',N/A,_)),
    recorded('$reconsulted',X,_),
    ( X = N/A , !;
      X = '$', !, fail;
      fail
    ).

'$inform_as_reconsulted'(N,A) :-
    recorda('$reconsulted',N/A,_).

'$clear_reconsulting' :-
    recorded('$reconsulted',X,Ref),
    erase(Ref),
    X == '$',
    !,
    ( recorded('$reconsulting',_,R) -> erase(R) ).

'$prompt_alternatives_on'(determinism).

/* Executing a query */

/* Executing a query */

query_to_answer(end_of_file,_,exit,[],[]) :-
    !.
query_to_answer(G0,Vs,Port, NVs, Gs) :-
    '$query'(G0,Vs,Port),
    all_attvars(AVs),
    attributes:delayed_goals(G0+AVs, Vs, NVs, Gs).



				
'$query'(G,[]) :-
    '$query'(G,[],_Port).


'$query'(G,_Vs,Port) :-
    prolog_flag(debug,true),
    '$get_debugger_state'(trace,on),
    '$get_debugger_state'(creep,Creep),
    Creep \= zip,
    !,
    gated_call(
	true,
	'$spy'(G),
	Port,
	true
    ).
'$query'(G,_,Port) :-
    catch(
	gated_call(
	    true,
	    G,
	    Port,
	    true
	),
	Error,
	'$Error'(Error)
    ).

%
'$another'([], _, _, _, _) :-
    !,
    print_message(help, answer([],[],[])).
'$another'(Names, GVs,LGs, exit, determinism) :-
    !,
    print_message(help, answer(Names, GVs,LGs)),
    print_message(help,yes).
'$another'(_,_, _, fail, _) :-
    !,
    print_message(help,no).
'$another'(Names, GVs,LGs,_,_) :-
    '$clear_input'(user_input),
    prompt1(' '),
    print_message(help, answer(Names, GVs,LGs) ),
    get_code(user_input,C),
    '$do_another'(C).

'$do_another'(C) :-
    (   C=:= ";" ->
        skip(user_input,10),
	!,
	fail
    ;
    C== 10
    ->
    '$add_nl_outside_console'
    ),
    !.

%'$add_nl_outside_console' :-
%	'$is_same_tty'(user_input, user_error), !.
'$add_nl_outside_console' :-
    format(user_error,'~n',[]).

'$ask_again_for_another' :-
    prompt(_Old,'Action (\";\" for more choices, <return> for exit)', []),
    '$another'.




%
% standard meta-call, called if $execute could not do everything.
%

'$disable_debugging_on_port'(retry) :-
    !,
    current_prolog_flag(debug,true),
    '$set_debugger_state'(debug, true).
'$disable_debugging_on_port'(_Port) :-
    '$set_debugger_state'(debug, false).



% enable creeping
'$enable_debugging':-
    prolog_flag(debug, false), !.
'$enable_debugging' :-
    '$get_debugger_state'(trace,on),
    !,
    '$set_debugger_state'(creep, 0, stop, on, true),
    '$creep'.
'$enable_debugging' :-
    '$set_debugger_state'(zip, 0, stop, on, true).

'$trace_on' :-
    '$get_debugger_state'(debug, true),
    '$set_debugger_state'(trace, on).




'$trace_off' :-
        '$get_debugger_state'(debug, true),
    '$set_debugger_state'(trace, off).


'$call'(V, _CP, G0, M) :-
    (
	var(V)
    ->
    throw_error(instantiation_error,call(G0))
    ;
    var(M)
    ->
    throw_error(instantiation_error,call(G0))
    ).
'$call'(!, CP, _G0, _) :-
    !,
    cut_by(CP).
'$call'(M:G, CP, G0, _) :-
    !,
    '$yap_strip_module'(M:G, M0, GG),
    '$call'(GG, CP, G0, M0 ).
'$call'((A,B), CP, G0, M) :- !,
    '$call'(A, CP, G0, M),
    '$call'(B, CP, G0, M).
'$call'((A->B;C), CP, G0, M) :- !,
    (call(M:A) ->
	 '$call'(B, CP, G0, M);
	 '$call'(C, CP, G0, M)).
'$call'((A*->B;C), CP, G0, M) :- !,
    (call(M:A) *->
	 '$call'(B, CP, G0, M);
     '$call'(C, CP, G0, M)
    ).
'$call'((A->B), CP, G0, M) :- !,
    (  call(M:A) ->
       '$call'(B, CP, G0, M)
    ).
'$call'((A*->B), CP, G0, M) :- !,
    ('$call'(A, CP, G0, M),
     '$call'(B, CP, G0, M)).
'$call'((A;B), CP, G0, M) :- !,
    ('$call'(A, CP, G0, M);
     '$call'(B, CP, G0, M)).
'$call'((A|B), CP, G0, M) :- !,
    ('$call'(A, CP, G0, M);
     '$call'(B, CP, G0, M)).
'$call'(G, _CP, _G0, M) :-
    call(M:G).



'$boot_loop'(Stream,Where) :-
    repeat,
    '$current_module'( OldModule, OldModule ),
    read_clause(Stream, Command, [module(OldModule), syntax_errors(dec10),variable_names( Vars), term_position(_Pos)]),
    (Command == end_of_file
    ->
	!
    ;
    Command = (:- Goal) ->
	query(Goal, []),
         fail
;
    Command = (?- Goal) ->
	query(Goal, Vars),
         fail
    ;
    Command = (H --> B) ->
    '$system_catch'('$boot_dcg'(H,B, Where),   prolog, Error,
		    '$LoopError'(Error, consult) ),

    fail
    ;
    '$system_Catch'('$boot_Clause'( Command, Where ),  prolog, Error,
		    '$Error'(Error, consult) ),
    fail
    ).

'$boot_execute'( Goal ) :-
    '$execute0'( Goal ),
    !.
'$boot_execute'( Goal ) :-
    format(user_error, ':- ~w failed.~n', [Goal]).

'$boot_dcg'( H, B, Where ) :-
    '$translate_rule'((H --> B), (NH :- NB) ),
    '$$compile'((NH :- NB), Where, ( H --> B),0, _R),
    !.
'$boot_dcg'( H, B, _ ) :-
    format(user_error, ' ~w --> ~w failed.~n', [H,B]).

'$boot_clause'( Command, Where ) :-
    '$$compile'(Command, Where, Command,0, _R),
    !.
'$boot_clause'( Command, _ ) :-
    format(user_error, ' ~w failed.~n', [Command]).



enter_command(Stream, Mod, Status) :-
    prompt1(': '), prompt(_,'     '),
    Options = [module(Mod), syntax_errors(dec10),variable_names(Vars), term_position(Pos)],
    (
	Status == top
    ->
    read_term(Stream, Command, Options)
    ;
    read_clause(Stream, Command, Options)
    ),
    '$command'(Command,Vars,Pos, Status).

compile_clauses(Commands) :-
     current_source_module(M,M),
     '$start_reconsulting'(user_input),
     '$start_consult'(reconsult,user_input,user_input,_),
     '$member'(C,Commands),
     compile_clause(C),
     fail.
compile_clauses(_Commands) :-
     '$end_consult'.
 

compile_clause(Command) :-
    '$command'(Command,[],0, reconsult),
    !.
compile_clause(_Command).

/* General purpose predicates				*/

'$head_and_body'((H:-B),H,B) :- !.
'$head_and_body'(H,H,true).


gated_call(Setup, Goal, Catcher, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup),
    '$gated_call'( true , Goal, Catcher, Cleanup)  .

'$gated_call'( All , Goal, Catcher, Cleanup) :-
    TaskF = cleanup( All, Catcher, Cleanup, Tag, false, CP0),
    Task0 = cleanup( All, Catcher, Cleanup, Tag, true, CP0),
    '$tag_cleanup'(CP0, Task0),
    '$execute0'( Goal ),
    '$cleanup_on_exit'(CP0, TaskF).


%
% split head and body, generate an error if body is unbound.
%
'$check_head_and_body'(C,M,H,B,_P) :-
    '$yap_strip_module'(C,M1,(MH:-B0)),
    !,
    '$yap_strip_module'(M1:MH,M,H),
    ( M == M1 -> B = B0 ; B = M1:B0),
    must_be_callable(M:H).

'$check_head_and_body'(MH, M, H, true, _P) :-
    '$yap_strip_module'(MH,M,H),
    must_be_callable(M:H ).

%  @pred expand_clause(+Clause, -ListingClause, -FinalClause)
%
% return two arguments: Expanded0 is the term as seen after "USER" expansion,
%                       Expanded is the actual term that us sent to the compiler.
%
expand_clause(Term, ExpandedUser, Expanded) :-
    %format('[ ~w~n',[Term]),
    '$expand_clause'(Term, ExpandedUser, ExpandedI),
    !,
    %format('      -> ~w~n',[Expanded0]),
    (
	current_prolog_flag(strict_iso, true)      /* strict_iso on */
    ->
    Expanded = ExpandedI,
    '$check_iso_strict_clause'(ExpandedUser)
    ;
    '$expand_array_accesses_in_term'(ExpandedI,Expanded)
    -> true
    ;
    Expanded = ExpandedI
    ).
expand_clause(Term, Term, Term).

'$expand_clause'(InputCl, C1, CO) :-
    current_source_module(SM,SM),
    '$expand_a_clause'( InputCl, SM, C1, CO),
    !.
'$expand_clause'(Cl, Cl, Cl).



%
% Grammar Rules expansion
%
'$expand_term_grammar'((A-->B), C) :-
    prolog:'$translate_rule'((A-->B),C), !.
'$expand_term_grammar'(A, A).

%
% Arithmetic expansion
%
'$expand_array_accesses_in_term'(Expanded0,ExpandedF) :-
    '$array_refs_compiled',
    '$arrays':'$c_arrays'(Expanded0,ExpandedF), !.
'$expand_array_accesses_in_term'(Expanded,Expanded).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   catch/throw implementation

% at each catch point I need to know:
% what is ball;
% where was the previous catch
/** @pred  catch( : _Goal_,+ _Exception_,+ _Action_) is iso


The goal `catch( _Goal_, _Exception_, _Action_)` tries to
execute goal  _Goal_. If during its execution,  _Goal_ throws an
exception  _E'_ and this exception unifies with  _Exception_, the
exception is considered to be caught and  _Action_ is executed. If
the exception  _E'_ does not unify with  _Exception_, control
again throws the exception.

The top-level of YAP maintains a default exception handler that
is responsible to capture uncaught exceptions.


*/

catch(MG,_E,_G) :-
    current_choice_point(CP0),
    '$execute0'(MG),
    current_choice_point(CPF),
    (CP0 == CPF -> ! ; true ).
catch(_MG,E,G) :-    
    '$drop_exception'(E0,Info),
    (var(Info) ->


	 Info = []
    ;
	 Info = exception(Data)
    ->
	read_exception(Data, List)
    ;
    is_list(Info)
    ->
    Info = List
    ),

    (E=E0
->(
	    Info = exception(I)
	->
	print_exception(I,L);
	    L=Info),
  (E0 = error(Id,UserInfo),
   G =.. [Pred,E0|Args],
   (Pred == '$Error' ; Pred == '$LoopError')
    ->
	'$add_error_hint'(UserInfo,List,TotalInfo),
	GG =.. [Pred,error(Id,TotalInfo)|Args]
    ;
    GG = G
    ),
			     
     '$run_catch'(E0, L, GG)
    ;

    throw(E0)
    ).

'$rm_user_wrapper'(error(user_defined_error(user_defined_error,EW),_),E0) :-
    !,
    '$rm_user_wrapper'(EW,E0).
'$rm_user_wrapper'(error(user_defined_error,EW),E0) :-
    !,
    '$rm_user_wrapper'(EW,E0).
'$rm_user_wrapper'(E,E).

'$add_error_hint'(V, Info, Info) :-
    var(V),
    !.    
'$add_error_hint'(Info, V, Info) :-
    var(V),
    !.    
'$add_error_hint'([], Info, Info) :-
    !.    
'$add_error_hint'(Info, [], Info) :-
    !.    
'$add_error_hint'(Info,Hint, NewInfo) :-
    atom(Hint),
    !,
    atom_string(Hint, String),
    (
	'$delete'(Info, errorMsg = Msg, Left) 
    ->
    string_concat([Msg,`\n user message: `,String], FullMsg),
    NewInfo = [errorMsg=FullMsg|Left]
    ;
    string_concat([` user message: `,String], FullMsg),
    NewInfo = [errorMsg=FullMsg|Info]
    ).
'$add_error_hint'(Info, String, NewInfo) :-
    string(String),
    !,
    (
	'$delete'(Info, errorMsg = Msg, Left) 
    ->
    string_concat([Msg,`\n user info: `,String], FullMsg),
    NewInfo = [errorMsg=FullMsg|Left]
    ;
    string_concat([` user message: `,String], FullMsg),
    NewInfo = [errorMsg=String|Info]
    ).
'$add_error_hint'( Info, Codes,NewInfo) :-
    Codes=[_|_],
    !,
    (
	'$delete'(Info, errorMsg = Msg, Left) 
    ->
    string_concat([Msg,`\n user message: `,String], FullMsg),
    NewInfo = [errorMsg=FullMsg|Left]
    ;
    NewInfo = [errorMsg=String|Info]
    ).
'$add_error_hint'(Info,Goal,  NewInfo) :-
    term_to_string(Goal, String),
    (
	'$delete'(Info, errorMsg = Msg, Left),
	nonvar(Msg),
	Msg \= ''
    ->
    string_concat([Msg,`\n YAP crashed while running : `,String], FullMsg),
    NewInfo = [errorMsg=FullMsg|Left]
    ;
    string_concat([`\n YAP crashed while running : `,String], FullMsg),
    NewInfo = [errorMsg=String|Info]
    ).
		     
% makes sure we have an environment.
'$true'.


% system_catch is like catch, but it avoids the overhead of a full
% meta-call by calling '$execute0' instead of execute.
% This way it
% also avoids module preprocessing and goal_expansion
%
'$system_catch'(G, M, C, A) :-
    % check current trail
    catch(M:G,C,A).



'$run_catch'(  _,_,G) :-
    must_be_callable(G),
    fail.
'$run_catch'(  abort,_,_) :-
    abort.
'$run_catch'(error(E1,Ctx),Info,Command) :-
    nonvar(Command),
    Command = '$Error'(error(E1,Ctx)),
    !,
    '$Error'(error(E1,Ctx), Info),
    fail.
'$run_catch'(_E,_Info,G) :-
    is_callable(G),
    !,
    '$execute0'(G).


'$run_toplevel_hooks' :-
    current_prolog_flag(break_level, 0 ),
    recorded('$toplevel_hooks',H,_),
    H \= fail,
    !,
    ( call(user:H) -> true ; true).
'$run_toplevel_hooks'.

'$run_at_thread_start' :-
    recorded('$thread_initialization',M:D,_),
    '$execute0'(M:D),
    fail.
'$run_at_thread_start'.

log_event( String, Args ) :-
    format( atom( M ), String, Args),
    log_event( M ).

'$prompt' :-
    current_prolog_flag(break_level, BreakLevel),
    (
	    BreakLevel == 0
    ->
    LF = LD
    ;
    LF = ['[Break (level ', BreakLevel, ')] '|LD]
    ),
    (
	current_prolog_flag(debug,true),
	'$get_debugger_state'(trace, on)
    ->
    LD  = ['[trace] '|L]
     ;
     current_prolog_flag(debug,true)
    ->
    LD  = ['[debug] '|L]
     ;
    LD =  L 
     ),
    yap_flag(toplevel_prompt, P),
    L = [P],
    atomic_concat(LF, PF),
    prompt1(PF),
    prompt(_,' |   '),
    '$ensure_prompting'.

'$loop'(Stream,Status) :-
    repeat,
    '$current_module'( OldModule, OldModule ),
    catch(
	enter_command(Stream,OldModule,Status),
	Error,
	'$Error'(Error)),
    !.


/**
@}  @}
*/
