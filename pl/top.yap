
live :- '$live'.

'$live' :-
    repeat,
    '$current_module'(Module),
    yap_flag(verbose,normal),
    ( Module==user ->
      true % '$compile_mode'(_,0)
    ;
    format(user_error,'[~w]~n', [Module])
    ),
    '$system_catch'('$enter_top_level',Module,Error,'$Error'(Error)).

% Start file for yap

/*		I/O predicates						*/

/* meaning of flags for '$write' is
	  1	quote illegal atoms
	  2	ignore operator declarations
	  4	output '$VAR'(N) terms as A, B, C, ...
	  8	use portray(_)
*/

/* main execution loop							*/
'$read_toplevel'(Goal, Bindings, Pos) :-
    '$prompt',
    catch(read_term(user_input,
		    Goal,
		    [variable_names(Bindings), syntax_errors(dec10), term_position(Pos)]),
	  E, '$handle_toplevel_error'( E) ).

'$handle_toplevel_error'( syntax_error(_)) :-
    !,
    fail.
'$handle_toplevel_error'( error(io_error(read,user_input),_)) :-
    !.
'$handle_toplevel_error'(_, E) :-
    throw(E).


/** @pred  stream_property( Stream, Prop )

*/

% reset alarms when entering top-level.
'$enter_top_level' :-
    '$alarm'(0, 0, _, _),
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
    nb_setval('$debug_run',off),
    nb_setval('$debug_jump',off),
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
'$execute_commands'([],_,_,_,_,_) :- !.
'$execute_commands'([C|Cs],M,VL,Pos,Con,Source) :-
    !,
    (
	'$system_catch'('$execute_command'(C,M,VL,Pos,Con,Source),prolog,Error,'$LoopError'(Error, Con)),
	fail
    ;
    '$execute_commands'(Cs,M,VL,Pos,Con,Source)
    ).
'$execute_commands'(C,M,VL,Pos,Con,Source) :-
    must_be_callable(C),
	'$system_catch'('$execute_command'(C,M,VL,Pos,Con,Source),prolog,Error,'$LoopError'(Error, Con)).

%
%
%

'$execute_command'(end_of_file,_,_,_,_,_) :- !.
'$execute_command'(Command,_,_,_,_,_) :-
    '__NB_getval__'('$if_skip_mode', skip, fail),
    \+ '$if_directive'(Command),
    !,
    fail.
'$execute_command'((:-G),M,VL,Pos,Option,_) :-
    !,			% allow user expansion
    '$expand_term'((:- M:G), O),
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
    '$continue_with_command'(Option, VL, Pos, M:G, Source).

'$expand_term'(T,O) :-
    '$expand_term'(T,top,O).

'$expand_term'(T,Con,O) :-
    catch( '$expand_term0'(T,Con,O), _,( '$disable_debugging', fail) ),
    !.

'$expand_term0'(T,consult,O) :-
    expand_term( T,  O).
'$expand_term0'(T,reconsult,O) :-
    expand_term( T,  O).
'$expand_term0'(T,top,O) :-
    expand_term( T,  T1),
    !,
    '$expand_term1'(T1,O).
'$expand_term0'(T,_,T).

'$expand_term1'(T,O) :-
    expand_goal(T,O),
    !.
'$expand_term1'(O,O).

'$continue_with_command'(Where,V,'$stream_position'(C,_P,A1,A2,A3),'$source_location'(_F,L):G,Source) :-
    !,
    '$continue_with_command'(Where,V,'$stream_position'(C,L,A1,A2,A3),G,Source).
'$continue_with_command'(reconsult,V,Pos,G,Source) :-
    %    writeln(G),
    '$go_compile_clause'(G,V,Pos,reconsult,Source),
    fail.
'$continue_with_command'(consult,V,Pos,G,Source) :-
    '$go_compile_clause'(G,V,Pos,consult,Source),
    fail.
'$continue_with_command'(top,V,_,G,_) :-
	'$query'(G,V),
	'$add_env_and_fail'.

'$add_env_and_fail' :- fail.

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
'$go_compile_clause'(G, _Vs, _Pos, Where, Source) :-
    '$precompile_term'(G, Source, G1),
    !,
    '$$compile'(G1, Where, Source, _).
'$go_compile_clause'(G,_Vs,_Pos, _Where, _Source) :-
    throw(error(system, compilation_failed(G))).

'$$compile'(C, Where, C0, R) :-
    '$head_and_body'( C, MH, B ),
    strip_module( MH, Mod, H),
    (
	'$undefined'(H, Mod)
    ->
    '$init_pred'(H, Mod, Where)
    ;
    true
    ),
    %    writeln(Mod:((H:-B))),
    '$compile'((H:-B), Where, C0, Mod, R).

'$init_pred'(H, Mod, _Where ) :-
    recorded('$import','$import'(NM,Mod,NH,H,_,_),RI),
    %    NM \= Mod,
    functor(NH,N,Ar),
    print_message(warning,redefine_imported(Mod,NM,Mod:N/Ar)),
    erase(RI),
    fail.
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

'$query'(end_of_file,_) :-
	!.
'$query'(G, Vs) :-
	prolog_flag(debug,true),
	!,
	prolog_flag(prompt_alternatives_on,OPT),
	(
	    '$trace'(G)
	    *->
	Port = answer,
	    attributes:delayed_goals(G, Vs, GVs, LGs),
	    print_message(help, answer(Vs, GVs,LGs) ),
	    (
		'$another'(Vs, Port, OPT)
	    ->
	    true
	    ;
	    !
	    )
	;
	print_message(help,false),
	fail
	).
'$query'(G, Vs) :-
	prolog_flag(prompt_alternatives_on,OPT),
	(
	    catch(
		gated_call(
			 true,
			G,
			 Port,
			 true
	      ),
	      Error,
	      '$Error'(Error)
	    )
	*->
	    (
		attributes:delayed_goals(G, Vs, GVs, LGs),
		print_message(help, answer(Vs, GVs,LGs) ),
		(
		    '$another'(Vs, Port, OPT)
		->
		fail
		;
		!
		)
	    )
	    ;
	    print_message(help,false)
	).


			

'$another'([], _, groundness) :-
    !,
    fail.
'$another'([], _, determinism) :-
    !,
    fail.
'$another'(_, exit, determinism) :-
    !,
    fail.
'$another'(_,_,_) :-
    format(user_error,' ? ',[]),
    '$clear_input'(user_input),
    get_code(user_input,C),
    '$do_another'(C).

'$do_another'(C) :-
    (   C=:= ";" ->
        skip(user_input,10)
    ;
    C== 10
    ->
    '$add_nl_outside_console',
    fail
    ;
    C== 13
    ->
    get0(user_input,NC),
    '$do_another'(NC)
    ;
    C== -1
    ->
    halt
    ;
    skip(user_input,10),
    get_code(user_input,CN),
    '$do_another'(CN)
    ).

%'$add_nl_outside_console' :-
%	'$is_same_tty'(user_input, user_error), !.
'$add_nl_outside_console' :-
    format(user_error,'~n',[]).

'$ask_again_for_another' :-
    format(user_error,'Action (\";\" for more choices, <return> for exit)', []),
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
     '$set_debugger_state'(creep, 0, stop, on, true),
     !,
    '$creep'.
'$enable_debugging'.

'$trace_on' :-
    '$get_debugger_state'(debug, true),
    '$set_debugger_state'(trace, on).




'$trace_off' :-
        '$get_debugger_state'(debug, true),
    '$set_debugger_state'(trace, off).


'$cut_by'(CP) :- '$$cut_by'(CP).

%
% do it in ISO mode.
%
'$call'(G, CP, G0, _, M) :-  /* iso version */
    '$iso_check_goal'(G,G0),
    '$call'(G, CP, G0, M).


'$call'(M:_,_,G0,_) :- var(M), !,
		       '$do_error'(instantiation_error,call(G0)).

'$call'(M:G,CP,G0,_M0) :- !,
    '$yap_strip_module'(M:G,NM,NC),
	'$call'(NC,CP,G0,NM).

'$call'('$call'(X,CP,_G0,M),_,G0,_) :-
    !,
    '$call'(X,CP,G0,M).
'$call'((X,Y),CP,G0,M) :- !,
    '$call'(X,CP,G0,M),
    '$call'(Y,CP,G0,M).
'$call'((X->Y),CP,G0,M) :- !,
    (
	'$call'(X,CP,G0,M)
    ->
    '$call'(Y,CP,G0,M)
    ).
'$call'((X*->Y),CP,G0,M) :- !,
    '$call'(X,CP,G0,M),
    '$call'(Y,CP,G0,M).
'$call'((X->Y; Z),CP,G0,M) :- !,
    (
	'$call'(X,CP,G0,M)
    ->
    '$call'(Y,CP,G0,M)
    ;
    '$call'(Z,CP,G0,M)
    ).
'$call'((X*->Y; Z),CP,G0,M) :- !,
    yap_hacks:current_choicepoint(CP0),
    (
	yap_hacks:current_choicepoint(CP),
	'$call'(X,CP,G0,M),
	yap_hacks:cut_at(CP0,CP),
	'$call'(Y,CP,G0,M)
    ;
    '$call'(Z,CP,G0,M)
    ).
'$call'((A;B),CP,G0,M) :- !,
    (
	'$call'(A,CP,G0,M)
    ;
    '$call'(B,CP,G0,M)
    ).
'$call'((X->Y| Z),CP,G0,M) :- !,
    (
	'$call'(X,CP,G0,M)
    ->
    '$call'(Y,CP,G0,M)
    ;
    '$call'(Z,CP,G0,M)
    ).
'$call'((X*->Y| Z),CP,G0,M) :- !,
    yap_hacks:current_choicepoint(CP0),
    (
	yap_hacks:current_choicepoint(CP),
	'$call'(X,CP,G0,M),
	yap_hacks:cut_at(CP0,CP),
	'$call'(Y,CP,G0,M)
    ;
    '$call'(Z,CP,G0,M)
    ).
'$call'((A|B),CP, G0,M) :- !,
    (
	'$call'(A,CP,G0,M)
    ;
    '$call'(B,CP,G0,M)
    ).
'$call'(\+ X, _CP, G0, M) :- !,
    \+ (yap_hacks:current_choicepoint(CP),
	'$call'(X,CP,G0,M) ).
'$call'(not(X), _CP, G0, M) :- !,
    \+ (yap_hacks:current_choicepoint(CP),
	'$call'(X,CP,G0,M) ).
'$call'(!, CP, _,_) :- !,
    '$$cut_by'(CP).
'$call'([A|B], _, _, M) :- !,
    '$csult'([A|B], M).
'$call'(G, _CP, _G0, CurMod) :-
    '$pred_exists'(G,CurMod),
!,  		   
    % /*
    % 	(
    %      '$is_metapredicate'(G,CurMod)
    %     ->
    %      '$disable_debugging',
    %      ( '$expand_meta_call'(CurMod:G, [], NG) ->  true ; true ),
    %      '$enable_debugging'
    %     ;
    %      NG = G
    %     ),
    % 	*/
    '$execute0'(G, CurMod).
'$call'(G, _CP, _G0, CurMod) :-
  '$get_undefined'(G, CurMod, NG, M),
    '$execute0'(NG, M).

'$check_callable'(V,G) :- var(V), !,
			  '$do_error'(instantiation_error,G).
'$check_callable'(M:_G1,G) :- var(M), !,
			      '$do_error'(instantiation_error,G).
'$check_callable'(_:G1,G) :- !,
    '$check_callable'(G1,G).
'$check_callable'(A,G) :- number(A), !,
			  '$do_error'(type_error(callable,A),G).
'$check_callable'(R,G) :- db_reference(R), !,
			  '$do_error'(type_error(callable,R),G).
'$check_callable'(_,_).


'$loop'(Stream,Status) :-
    repeat,
    '$current_module'( OldModule, OldModule ),
    '$enter_command'(Stream,OldModule,Status),
    !.

'$boot_loop'(Stream,Where) :-
    repeat,
    '$current_module'( OldModule, OldModule ),
    read_clause(Stream, Command, [module(OldModule), syntax_errors(dec10),variable_names( Vars), term_position(_Pos)]),
    (Command == end_of_file
    ->
	!
    ;
    Command = (:- Goal) ->
	'$query'(Goal, []),
         fail
;
    Command = (?- Goal) ->
	'$query'(Goal, Vars),
         fail
    ;
    Command = (H --> B) ->
    '$system_catch'('$boot_dcg'(H,B, Where),   prolog, Error,
		    user:'$LoopError'(Error, consult) ),

    fail
    ;
    '$system_catch'('$boot_clause'( Command, Where ),  prolog, Error,
		    user:'$LoopError'(Error, consult) ),
    fail
    ).

'$boot_execute'( Goal ) :-
    '$execute'( Goal ),
    !.
'$boot_execute'( Goal ) :-
    format(user_error, ':- ~w failed.~n', [Goal]).

'$boot_dcg'( H, B, Where ) :-
    '$translate_rule'((H --> B), (NH :- NB) ),
    '$$compile'((NH :- NB), Where, ( H --> B), _R),
    !.
'$boot_dcg'( H, B, _ ) :-
    format(user_error, ' ~w --> ~w failed.~n', [H,B]).

'$boot_clause'( Command, Where ) :-
    '$$compile'(Command, Where, Command, _R),
    !.
'$boot_clause'( Command, _ ) :-
    format(user_error, ' ~w failed.~n', [Command]).



'$enter_command'(Stream, Mod, Status) :-
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

/** @pred  user:expand_term( _T_,- _X_) is dynamic,multifile.

  This user-defined predicate is called by YAP after
  reading goals and clauses.

  - _Module_:`expand_term(` _T_ , _X_) is called first on the
  current source module _Module_ ; if i
  - `user:expand_term(` _T_ , _X_ `)` is available on every module.

  */

/* General purpose predicates				*/

'$head_and_body'((H:-B),H,B) :- !.
'$head_and_body'(H,H,true).


gated_call(Setup, Goal, Catcher, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup),
    '$gated_call'( true , Goal, Catcher, Cleanup)  .

'$gated_call'( All , Goal, Catcher, Cleanup) :-
    Task0 = cleanup( All, Catcher, Cleanup, Tag, true, CP0),
    TaskF = cleanup( All, Catcher, Cleanup, Tag, false, CP0),
    '$tag_cleanup'(CP0, Task0),
    '$execute'( Goal ),
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
% term expansion
%
% return two arguments: Expanded0 is the term after "USER" expansion.
%                       Expanded is the final expanded term.
%
'$precompile_term'(Term, ExpandedUser, Expanded) :-
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
'$precompile_term'(Term, Term, Term).

'$expand_clause'(InputCl, C1, CO) :-
    source_module(SM),
    '$expand_a_clause'( InputCl, SM, C1, CO),
    !.
'$expand_clause'(Cl, Cl, Cl).

/** @pred  expand_term( _T_,- _X_)

This predicate is used by YAP for preprocessing each top level
term read when consulting a file and before asserting or executing it.
It rewrites a term  _T_ to a term  _X_ according to the following
rules: first try term_expansion/2  in the current module, and then try to use the user defined predicate user:term_expansion/2`. If this call fails then the translating process
for DCG rules is applied, together with the arithmetic optimizer
whenever the compilation of arithmetic expressions is in progress.


*/
expand_term(Term,Expanded) :-
    (
	'$do_term_expansion'(Term,Expanded)
    ->
    true
    ;
    '$expand_term_grammar'(Term,Expanded)
    ).


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
catch(MG,_,_) :-
    '$current_choice_point'(CP0),
    '$execute'(MG),
    '$current_choice_point'(CPF),
    (CP0 == CPF -> ! ; true ).
catch(_,E,G) :-
    '$drop_exception'(E0),
    (
	E = E0
    ->
    '$run_catch'(E0, E, G)
    ;
    throw(E0)
    ).

% makes sure we have an environment.
'$true'.


% system_catch is like catch, but it avoids the overhead of a full
% meta-call by calling '$execute0' instead of $execute.
% This way it
% also avoids module preprocessing and goal_expansion
%
'$system_catch'(G, M, C, A) :-
    % check current trail
    catch(M:G,C,A).



'$run_catch'(error(Event,_ ),_, G) :-
    functor(Event, event, N),
    N > 0,
    arg(1, Event, Error),
    !,
    '$run_catch'(Error, Error, G).
'$run_catch'(  abort,abort,_) :-
    abort.
'$run_catch'(_E,_E,G) :-
    is_callable(G),
    !,
    '$execute'(G).
'$run_catch'(error(A, B), error(A, B), _) :-
    !,
    '$LoopError'(error(A, B), error).
'$run_catch'(E,E,_).

'$run_toplevel_hooks' :-
    current_prolog_flag(break_level, 0 ),
    recorded('$toplevel_hooks',H,_),
    H \= fail,
    !,
    ( call(user:H) -> true ; true).
'$run_toplevel_hooks'.

'$run_at_thread_start' :-
    recorded('$thread_initialization',M:D,_),
    '$execute'(M:D),
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
     true
     ),
    yap_flag(toplevel_prompt, P),
    L = [P],
    atomic_concat(L, PF),
    prompt1(PF),
    prompt(_,' |   '),
    '$ensure_prompting'.


/**
@}  @}
*/
