/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
*									 *
**************************************************************************
*								         *
* File:		boot.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* commen    ts:	boot file for Prolog					 *
*									 *
*************************************************************************/

/**
  @file boot.yap
  @brief YAP bootstrap

  @defgroup YAPControl Control Predicates
  @ingroup builtins

@{


*/

'$bc'(G , VL) :-
	'$pred_exists'( expand_term((:- G), O),prolog),
% allow user expansion
	expand_term((:- G), O),
	!,
	(
	 O = (:- G1)
	->
	'$yap_strip_module'(G1, M, G2)
	  ;
	 '$yap_strip_module'(O, M, G2)
	),
	'$b2'(G2, VL, M).

'$bc'(G,_VL) :-
	'$yap_strip_module'(G, M, G2),
	    '$execute'(M:G2).

'$b2'(G2, VL, M) :-
	(
	  '$directive'(G2)
	 ->
	  '$exec_directives'(G2, _Option, M, VL, _Pos)
	 ;
	    '$execute'(M:G2)
	 ).

system_module(_Mod, _SysExps, _Decls).
%    new_system_module(Mod).

use_system_module(_Module, _SysExps).

private(_).

%
% boootstrap predicates.
%
:- system_module( '$_boot', [
        bootstrap/1,
        call/1,
        catch/3,
        catch_ball/2,
        expand_term/2,
        import_system_module/2,
        incore/1,
        (not)/1,
        repeat/0,
        throw/1,
        true/0], ['$$compile'/4,
        '$call'/4,
        '$catch'/3,
        '$check_callable'/2,
        '$check_head_and_body'/4,
        '$check_if_reconsulted'/2,
        '$clear_reconsulting'/0,
        '$command'/4,
        '$cut_by'/1,
        '$disable_debugging'/0,
        '$do_live'/0,
        '$'/0,
        '$find_goal_definition'/4,
        '$head_and_body'/3,
        '$inform_as_reconsulted'/2,
        '$init_system'/0,
        '$init_win_graphics'/0,
        '$loop'/2,
        '$meta_call'/2,
        '$prompt_alternatives_on'/1,
        '$run_at_thread_start'/0,
        '$system_catch'/4,
        '$undefp'/1,
		  '$version'/0]).

:- use_system_module( '$_absf', ['$system_library_directories'/2]).

:- use_system_module( '$_checker', ['$check_term'/5,
        '$sv_warning'/2]).

:- use_system_module( '$_consult', ['$csult'/2]).

:- use_system_module( '$_control', ['$run_atom_goal'/1]).

:- use_system_module( '$_directives', ['$all_directives'/1,
        '$exec_directives'/5]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_grammar', ['$translate_rule'/2]).

:- use_system_module( '$_modules', ['$get_undefined_pred'/4,
        '$meta_expansion'/6,
        '$module_expansion'/6]).

:- use_system_module( '$_preddecls', ['$dynamic'/2]).

:- use_system_module( '$_preds', ['$assert_static'/5,
				  '$assertz_dynamic'/4,
        '$init_preds'/0,
        '$unknown_error'/1,
        '$unknown_warning'/1]).

:- use_system_module( '$_qly', ['$init_state'/0]).

:- use_system_module( '$_strict_iso', ['$check_iso_strict_clause'/1,
        '$iso_check_goal'/2]).

% be careful here not to generate an undefined exception.

'$undefp0'([prolog_complete|print_message(_,_), _Action) :-
	  format( user_error, '~w in bootstrap: got ~w~n',[L,E]).
'$undefp0'([M|G], _Action) :-
    stream_property( loop_stream, file_name(F)),
    stream_property( loop_stream, line_number(L)),
	%'$bootstrap_predicate'(G, M, Action).
	writeln(F:L:M:G),
	fail.

:- '$undefp_handler'('$undefp0'(_,_),prolog).


%'$undefp0'([_M|'$imported_predicate'(G, _ImportingMod, G, prolog)], _Action) :-
% 	nonvar(G), '$is_system_predicate'(G, prolog), !.
%'$undefp0'([_M|print_message(A,B)], _Action) :-
% 	!.
%'$undefp0'([_M|sort(A,B)], _Action) :-
% 	!,
% 	'$sort'(A,B).


live :-
initialize_prolog,
 	repeat,
	'$current_module'(Module),
	( Module==user ->
	  true % '$compile_mode'(_,0)
	;
	  format(user_error,'[~w]~n', [Module])
	),
	'$system_catch'('$enter_top_level',Module,Error,'$Error'(Error)).

initialize_prolog :-
    '$init_system'.




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
	\+ '$undefined'('$init_preds',prolog),
	'$init_preds',
	fail.
'$init_consult'.

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
      current_prolog_flag(halt_after_consult, false),
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

'$init_system' :-
    get_value('$yap_inited', true), !.
'$init_system' :-
    set_value('$yap_inited', true),    % start_low_level_trace,
    % do catch as early as possible
    '$version',
    current_prolog_flag(file_name_variables, OldF),
    set_prolog_flag(file_name_variables, true),
    '$init_consult',
    set_prolog_flag(file_name_variables, OldF),
    '$init_globals',
    set_prolog_flag(fileerrors, true),
    set_value('$gc',on),
    ('$exit_undefp' -> true ; true),
    prompt1(' ?- '),
    set_prolog_flag(debug, false),
    % simple trick to find out if this is we are booting from Prolog.
    % boot from a saved state
    (
      current_prolog_flag(saved_program, true)
      % use saved state
    ->
      '$init_state'
    ;
	qsave_program( 'startup.yss')
    ),
    '$db_clean_queues'(0),
				% this must be executed from C-code.
				%	'$startup_saved_state',
    set_input(user_input),
    set_output(user_output),
    '$init_or_threads',
    '$run_at_thread_start'.


% Start file for yap

/*		I/O predicates						*/

/* meaning of flags for '$write' is
	  1	quote illegal atoms
	  2	ignore operator declarations
	  4	output '$VAR'(N) terms as A, B, C, ...
	  8	use portray(_)
*/

/* main execution loop							*/
'$read_toplevel'(Goal, Bindings) :-
	'$prompt',
	catch(read_term(user_input,
			Goal,
			[variable_names(Bindings), syntax_errors(dec10)]),
			 E, '$handle_toplevel_error'( E) ).

'$handle_toplevel_error'( syntax_error(_)) :-
	!,
	fail.
'$handle_toplevel_error'( error(io_error(read,user_input),_)) :-
	!.
'$handle_toplevel_error'(_, E) :-
	throw(E).


/** @pred  stream_property( _Stream_, _Prop_)

*/

% reset alarms when entering top-level.
'$enter_top_level' :-
	'$alarm'(0, 0, _, _),
	fail.
'$enter_top_level' :-
	'$clean_up_dead_clauses',
	fail.
'$enter_top_level' :-
	get_value('$top_level_goal',GA), GA \= [], !,
	set_value('$top_level_goal',[]),
	'$run_atom_goal'(GA),
	current_prolog_flag(break_level, BreakLevel),
	(
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
'$enter_top_level' :-
        flush_output,
	'$run_toplevel_hooks',
	prompt1(' ?- '),
	'$read_toplevel'(Command,Varnames),
	nb_setval('$spy_gn',1),
	% stop at spy-points if debugging is on.
	nb_setval('$debug_state', state(creep,0,stop)),
	'$command'(Command,Varnames,_Pos,top),
	current_prolog_flag(break_level, BreakLevel),
	(
	 BreakLevel \= 0
	->
	 true
	;
	 '$pred_exists'(halt(_), user)
	-> halt(0)
	;
	 '$halt'(0)
	).


'$erase_sets' :-
                 eraseall('$'),
		 eraseall('$$set'),
		 eraseall('$$one'),
		 eraseall('$reconsulted'), fail.
'$erase_sets' :- \+ recorded('$path',_,_), recorda('$path',"",_).
'$erase_sets'.

'$start_corouts' :-
	eraseall('$corout'),
	eraseall('$result'),
	eraseall('$actual'),
	fail.
'$start_corouts' :- recorda('$actual',main,_),
	recordz('$corout','$corout'(main,main,'$corout'([],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[])),_Ref),
	recorda('$result',going,_).

'$command'(C,VL,Pos,Con) :-
	current_prolog_flag(strict_iso, true), !,      /* strict_iso on */
	 '$execute_command'(C,VL,Pos,Con,_Source).
'$command'(C,VL,Pos,Con) :-
	( (Con = top ; var(C) ; C = [_|_])  ->
	  '$execute_command'(C,VL,Pos,Con,C), ! ;
	  % do term expansion
	  expand_term(C, EC),
	  % execute a list of commands
	  '$execute_commands'(EC,VL,Pos,Con,_Source),
	  % succeed only if the *original* was at end of file.
	  C == end_of_file
	).

 %
 % Hack in case expand_term has created a list of commands.
 %
 '$execute_commands'(V,_,_,_,Source) :- var(V), !,
	 '$do_error'(instantiation_error,meta_call(Source)).
 '$execute_commands'([],_,_,_,_) :- !.
 '$execute_commands'([C|Cs],VL,Pos,Con,Source) :- !,
	 (
	   '$system_catch'('$execute_command'(C,VL,Pos,Con,Source),prolog,Error,'$LoopError'(Error, Con)),
	   fail
	 ;
	   '$execute_commands'(Cs,VL,Pos,Con,Source)
	 ).
 '$execute_commands'(C,VL,Pos,Con,Source) :-
	 '$execute_command'(C,VL,Pos,Con,Source).

				%
 %
 %

'$execute_command'(C,_,_,top,Source) :-
    var(C),
    !,
	'$do_error'(instantiation_error,meta_call(Source)).
'$execute_command'(C,_,_,top,Source) :-
    number(C),
    !,
	'$do_error'(type_error(callable,C),meta_call(Source)).
 '$execute_command'(R,_,_,top,Source) :-
     db_reference(R),
     !,
	 '$do_error'(type_error(callable,R),meta_call(Source)).
 '$execute_command'(end_of_file,_,_,_,_) :- !.
 '$execute_command'(Command,_,_,_,_) :-
	 '__NB_getval__'('$if_skip_mode', skip, fail),
	 \+ '$if_directive'(Command),
	 !.
 '$execute_command'((:-G),VL,Pos,Option,_) :-
%          !,
	 Option \= top, !,
	 % allow user expansion
	 expand_term((:- G), O),
	 (
	     O = (:- G1)
	 ->
	  '$yap_strip_module'(G1, M, G2),

	   '$process_directives'(G2, Option, M, VL, Pos)
     ;
	    '$execute_commands'(G1,VL,Pos,Option,O)
	 ).
 '$execute_command'((?-G), VL, Pos, Option, Source) :-
	 Option \= top,
     !,
	 '$execute_command'(G, VL, Pos, top, Source).
 '$execute_command'(G, VL, Pos, Option, Source) :-
	 '$continue_with_command'(Option, VL, Pos, G, Source).


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
	'$query'(G,V).

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

'$query'(end_of_file,_).
'$query'(G,[]) :-
	 '$prompt_alternatives_on'(OPT),
	 ( OPT = groundness ; OPT = determinism),
     !,
	 '$yes_no'(G,(?-)).
'$query'(G,V) :-
	 (
     '$current_module'(M),
     '$current_choice_point'(CP),
     '$user_call'(G, M),
     '$current_choice_point'(NCP),
     '$delayed_goals'(G, V, Vs, LGs, DCP),
     '$write_answer'(Vs, LGs, Written),
	  '$write_query_answer_true'(Written),
	  (
	   '$prompt_alternatives_on'(determinism), CP == NCP, DCP = 0
	   ->
	   format(user_error, '.~n', []),
	   !
	  ;
	   '$another',
	   !
	  ),
	  fail
	 ;
	  '$out_neg_answer'
	 ).

 '$yes_no'(G,C) :-
	 '$current_module'(M),
	 '$do_yes_no'(G,M),
	 '$delayed_goals'(G, [], NV, LGs, _),
	 '$write_answer'(NV, LGs, Written),
	 ( Written = [] ->
	   !,'$present_answer'(C, true)
	 ;
	   '$another', !
	 ),
	 fail.
 '$yes_no'(_,_) :-
	 '$out_neg_answer'.

'$add_env_and_fail' :- fail.


'$process_answer'(Vs, LGs, Bindings) :-
'$purge_dontcares'(Vs,IVs),
'$sort'(IVs, NVs),
'$prep_answer_var_by_var'(NVs, LAnsw, LGs),
'$name_vars_in_goals'(LAnsw, Vs, Bindings).

%
% *-> at this point would require compiler support, which does not exist.
%
'$delayed_goals'(G, V, NV, LGs, NCP) :-
	(
	  CP is '$last_choice_pt',
	 '$current_choice_point'(NCP1),
	 attributes:delayed_goals(G, V, NV, LGs),
	 '$current_choice_point'(NCP2),
	 '$clean_ifcp'(CP),
	 NCP is NCP2-NCP1
	  ;
	   copy_term_nat(V, NV),
	   LGs = [],
%	   term_factorized(V, NV, LGs),
	   NCP = 0
    ).

'$out_neg_answer' :-
	print_message( help, false),
	 fail.


'$do_yes_no'([X|L], M) :-
	!,
	'$csult'([X|L], M).
'$do_yes_no'(G, M) :-
	'$user_call'(G, M).

'$write_query_answer_true'([]) :- !,
	format(user_error,true,[]).
'$write_query_answer_true'(_).


%
% present_answer has three components. First it flushes the streams,
% then it presents the goals, and last it shows any goals frozen on
% the arguments.
%
'$present_answer'(_,_):-
        flush_output,
	fail.
'$present_answer'((?-), Answ) :-
	current_prolog_flag(break_level, BL ),
	( BL \= 0 -> 	format(user_error, '[~p] ',[BL]) ;
			true ),
        ( current_prolog_flag(toplevel_print_options, Opts) ->
	   write_term(user_error,Answ,Opts) ;
	   format(user_error,'~w',[Answ])
        ),
	format(user_error,'.~n', []).

'$another' :-
	format(user_error,' ? ',[]),
	'$clear_input'(user_input),
	get_code(user_input,C),
	'$do_another'(C).

'$do_another'(C) :-
	(   C=:= ";" ->
         skip(user_input,10), %
	%    '$add_nl_outside_console',
	    fail
	;
	    C== 10
    ->
        '$add_nl_outside_console',
		(
         '$undefined'(print_message(_,_),prolog)
        ->
         format(user_error,'yes~n', [])
        ;
         print_message(help,yes)
		)
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
	    skip(user_input,10), '$ask_again_for_another'
	).

%'$add_nl_outside_console' :-
%	'$is_same_tty'(user_input, user_error), !.
'$add_nl_outside_console' :-
	format(user_error,'~n',[]).

'$ask_again_for_another' :-
	format(user_error,'Action (\";\" for more choices, <return> for exit)', []),
	'$another'.

'$write_answer'(_,_,_) :-
    flush_output,
	fail.
'$write_answer'(Vs, LBlk, FLAnsw) :-
    '$process_answer'(Vs, LBlk, NLAnsw),
    '$write_vars_and_goals'(NLAnsw, first, FLAnsw).

write_query_answer( Bindings ) :-
   '$write_vars_and_goals'(Bindings, first, _FLAnsw).

'$purge_dontcares'([],[]).
'$purge_dontcares'([Name=_|Vs],NVs) :-
	atom_codes(Name, [C|_]), C is "_", !,
	'$purge_dontcares'(Vs,NVs).
'$purge_dontcares'([V|Vs],[V|NVs]) :-
	'$purge_dontcares'(Vs,NVs).


'$prep_answer_var_by_var'([], L, L).
'$prep_answer_var_by_var'([Name=Value|L], LF, L0) :-
	'$delete_identical_answers'(L, Value, NL, Names),
	'$prep_answer_var'([Name|Names], Value, LF, LI),
	'$prep_answer_var_by_var'(NL, LI, L0).

% fetch all cases that have the same solution.
'$delete_identical_answers'([], _, [], []).
'$delete_identical_answers'([(Name=Value)|L], Value0, FL, [Name|Names]) :-
	Value == Value0, !,
	'$delete_identical_answers'(L, Value0, FL, Names).
'$delete_identical_answers'([VV|L], Value0, [VV|FL], Names) :-
	'$delete_identical_answers'(L, Value0, FL, Names).

% now create a list of pairs that will look like goals.
'$prep_answer_var'(Names, Value, LF, L0) :- var(Value), !,
	'$prep_answer_unbound_var'(Names, LF, L0).
'$prep_answer_var'(Names, Value, [nonvar(Names,Value)|L0], L0).

% ignore unbound variables
'$prep_answer_unbound_var'([_], L, L) :- !.
'$prep_answer_unbound_var'(Names, [var(Names)|L0], L0).

'$gen_name_string'(I,L,[C|L]) :- I < 26, !, C is I+65.
'$gen_name_string'(I,L0,LF) :-
	I1 is I mod 26,
	I2 is I // 26,
	C is I1+65,
	'$gen_name_string'(I2,[C|L0],LF).

'$write_vars_and_goals'([], _, []).
'$write_vars_and_goals'([nl,G1|LG], First, NG) :- !,
	nl(user_error),
	'$write_goal_output'(G1, First, NG, Next, IG),
	'$write_vars_and_goals'(LG, Next, IG).
'$write_vars_and_goals'([G1|LG], First, NG) :-
	'$write_goal_output'(G1, First, NG, Next, IG),
	'$write_vars_and_goals'(LG, Next, IG).

'$goal_to_string'(Format, G, String) :-
	format(codes(String),Format,G).

'$write_goal_output'(var([V|VL]), First, [var([V|VL])|L], next, L) :- !,
    ( First = first -> true ; format(user_error,',~n',[]) ),
	format(user_error,'~a',[V]),
	'$write_output_vars'(VL).
'$write_goal_output'(nonvar([V|VL],B), First, [nonvar([V|VL],B)|L], next, L) :- !,
        ( First = first -> true ; format(user_error,',~n',[]) ),
	format(user_error,'~a',[V]),
	'$write_output_vars'(VL),
	format(user_error,' = ', []),
        ( yap_flag(toplevel_print_options, Opts) ->
	   write_term(user_error,B,[priority(699)|Opts]) ;
	   write_term(user_error,B,[priority(699)])
        ).
'$write_goal_output'(nl, First, NG, First, NG) :- !,
	format(user_error,'~n',[]).
'$write_goal_output'(Format-G, First, NG, Next, IG) :- !,
	G = [_|_], !,
	% dump on string first so that we can check whether we actually
	% had any output from the solver.
	'$goal_to_string'(Format, G, String),
	( String == [] ->
	    % we didn't
	    IG = NG, First = Next
	;
	    % we did
	    ( First = first -> true ; format(user_error,',~n',[]) ),
	    format(user_error, '~s', [String]),
	    NG = [G|IG]
	).
'$write_goal_output'(_-G, First, [G|NG], next, NG) :- !,
        ( First = first -> true ; format(user_error,',~n',[]) ),
        (  yap_flag(toplevel_print_options, Opts) ->
	   write_term(user_error,G,Opts) ;
	   format(user_error,'~w',[G])
        ).
'$write_goal_output'(_M:G, First, [G|NG], next, NG) :- !,
        ( First = first -> true ; format(user_error,',~n',[]) ),
        (  yap_flag(toplevel_print_options, Opts) ->
	   write_term(user_error,G,Opts) ;
	   format(user_error,'~w',[G])
        ).
'$write_goal_output'(G, First, [M:G|NG], next, NG) :-
	'$current_module'(M),
        ( First = first -> true ; format(user_error,',~n',[]) ),
        (  yap_flag(toplevel_print_options, Opts) ->
	   write_term(user_error,G,Opts) ;
	   format(user_error,'~w',[G])
        ).

'$name_vars_in_goals'(G, VL0, G) :-
	'$name_well_known_vars'(VL0),
	'$variables_in_term'(G, [], GVL),
	'$name_vars_in_goals1'(GVL, 0, _).

'$name_well_known_vars'([]).
'$name_well_known_vars'([Name=V|NVL0]) :-
	var(V), !,
	V = '$VAR'(Name),
	'$name_well_known_vars'(NVL0).
'$name_well_known_vars'([_|NVL0]) :-
	'$name_well_known_vars'(NVL0).

'$name_vars_in_goals1'([], I, I).
'$name_vars_in_goals1'([V|NGVL], I0, IF) :-
	I is I0+1,
	'$gen_name_string'(I0,[],SName), !,
	atom_codes(Name, [95|SName]),
	V = '$VAR'(Name),
	'$name_vars_in_goals1'(NGVL, I, IF).
'$name_vars_in_goals1'([NV|NGVL], I0, IF) :-
	nonvar(NV),
	'$name_vars_in_goals1'(NGVL, I0, IF).

'$write_output_vars'([]).
'$write_output_vars'([V|VL]) :-
	format(user_error,' = ~a',[V]),
	'$write_output_vars'(VL).


%
% standard meta-call, called if $execute could not do everything.
%
'$meta_call'(G, M) :-
	'$current_choice_point'(CP),
	'$call'(G, CP, G, M).

'$user_call'(G, M) :-
        gated_call(
                '$enable_debugging',
                M:G,
	         Port,
  	         '$disable_debugging_on_port'(Port)
       ).

'$disable_debugging_on_port'(retry) :-
    !,
    '$enable_debugging'.
'$disable_debugging_on_port'(_Port) :-
    '$disable_debugging'.



% enable creeping
'$enable_debugging':-
    current_prolog_flag(debug, false), !.
'$enable_debugging' :-
	'__NB_setval__'('$debug_status', state(creep, 0, stop)),
    '$trace_on', !,
    '$creep'.
'$enable_debugging'.

'$trace_on' :-
    '__NB_getval__'('$trace', on, fail).

'$trace_off' :-
    '__NB_getval__'('$trace', off, fail).

'$cut_by'(CP) :- '$$cut_by'(CP).

%
% do it in ISO mode.
%
'$meta_call'(G,_ISO,M) :-
	'$iso_check_goal'(G,G),
	'$current_choice_point'(CP),
	'$call'(G, CP, G, M).

'$meta_call'(G, CP, G0, M) :-
	'$call'(G, CP, G0, M).

'$call'(G, CP, G0, _, M) :-  /* iso version */
	'$iso_check_goal'(G,G0),
	'$call'(G, CP, G0, M).


'$call'(M:_,_,G0,_) :- var(M), !,
	'$do_error'(instantiation_error,call(G0)).
'$call'(M:G,CP,G0,_) :- !,
        '$call'(G,CP,G0,M).
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
	(
	 '$current_choice_point'(DCP),
	 '$call'(X,CP,G0,M),
	 yap_hacks:cut_at(DCP),
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
	(
	 '$current_choice_point'(DCP),
	 '$call'(X,CP,G0,M),
	 yap_hacks:cut_at(DCP),
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
	\+ ('$current_choice_point'(CP),
	  '$call'(X,CP,G0,M) ).
'$call'(not(X), _CP, G0, M) :- !,
	\+ ('$current_choice_point'(CP),
	  '$call'(X,CP,G0,M) ).
'$call'(!, CP, _,_) :- !,
	'$$cut_by'(CP).
'$call'([A|B], _, _, M) :- !,
	'$csult'([A|B], M).
'$call'(G, _CP, _G0, CurMod) :-
/*
	(
     '$is_metapredicate'(G,CurMod)
    ->
     '$disable_debugging',
     ( '$expand_meta_call'(CurMod:G, [], NG) ->  true ; true ),
     '$enable_debugging'
    ;
     NG = G
    ),
	*/
    '$execute0'(G, CurMod).

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


'$loop'(Stream,exo) :-
	prolog_flag(agc_margin,Old,0),
    prompt1(': '), prompt(_,'     '),
	'$current_module'(OldModule),
	repeat,
		'$system_catch'(dbload_from_stream(Stream, OldModule, exo), '$db_load', Error,
			 user:'$LoopError'(Error, top)),
	prolog_flag(agc_margin,_,Old),
	!.
'$loop'(Stream,db) :-
	prolog_flag(agc_margin,Old,0),
    prompt1(': '), prompt(_,'     '),
	'$current_module'(OldModule),
	repeat,
		'$system_catch'(dbload_from_stream(Stream, OldModule, db), '$db_load', Error,
			 user:'$LoopError'(Error, top)),
	prolog_flag(agc_margin,_,Old),
	!.
'$loop'(Stream,Status) :-
 	repeat,
  '$current_module'( OldModule, OldModule ),
	'$system_catch'( '$enter_command'(Stream,OldModule,Status),
                     OldModule, Error,
			         user:'$LoopError'(Error, Status)
                   ),
	!.

'$boot_loop'(Stream,Where) :-
	repeat,
	'$current_module'( OldModule, OldModule ),
	read_clause(Stream, Command, [module(OldModule), syntax_errors(dec10),variable_names(_Vars), term_position(_Pos)]),
	(Command == end_of_file
  ->
    !
	;
   Command = (:- Goal) ->
     '$system_catch'('$boot_execute'(Goal),   prolog, Error,
        user:'$LoopError'(Error, consult) ),
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
'$check_head_and_body'(C,M,H,B,P) :-
    '$yap_strip_module'(C,M1,(MH:-B0)),
    !,
    '$yap_strip_module'(M1:MH,M,H),
    ( M == M1 -> B = B0 ; B = M1:B0),
    is_callable(M:H,P).

'$check_head_and_body'(MH, M, H, true, P) :-
    '$yap_strip_module'(MH,M,H),
    is_callable(M:H,P).
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
    '$yap_strip_clause'(SM:InputCl, M, ICl),
    '$expand_a_clause'( M:ICl, SM, C1, CO),
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
catch(G, C, A) :-
	'$catch'(G,C,A).

% makes sure we have an environment.
'$true'.


% system_catch is like catch, but it avoids the overhead of a full
% meta-call by calling '$execute0' instead of $execute.
% This way it
% also avoids module preprocessing and goal_expansion
%
'$system_catch'(G, M, C, A) :-
	% check current trail
	'$catch'(M:G,C,A).

'$catch'(MG,_,_) :-
	'$$save_by'(CP0),
	'$execute'(MG),
	'$$save_by'(CP1),
    % remove catch
	(
    CP0 == CP1
     ->
     !
   ;
     true
  ).
'$catch'(_,C,A) :-
	'$get_exception'(C),
	'$run_catch'(A, C).

% variable throws are user-handled.
'$run_catch'(G,E) :-
  E = '$VAR'(_),
      !,
    	call(G ).
'$run_catch'(abort,_) :-
        abort.
'$run_catch'('$Error'(E),E) :-
        !,
        	'$LoopError'(E, top ).
'$run_catch'('$LoopError'(E, Where),E) :-
      !,
      '$LoopError'(E, Where).
'$run_catch'('$TraceError'(E, GoalNumber, G, Module, CalledFromDebugger),E) :-
      !,
      '$TraceError'(E, GoalNumber, G, Module, CalledFromDebugger).
'$run_catch'(_Signal,E) :-
      functor( E, N, _),
      '$hidden_atom'(N), !,
      throw(E).
'$run_catch'( Signal, _E) :-
    call( Signal ).

%
% throw has to be *exactly* after system catch!
%
/** @pred  throw(+ _Ball_) is iso


The goal `throw( _Ball_)` throws an exception. Execution is
stopped, and the exception is sent to the ancestor goals until reaching
a matching catch/3, or until reaching top-level.

*/
throw(Ball) :-
	% get current jump point
	    '$jump_env_and_store_ball'(Ball).

'$run_toplevel_hooks' :-
	current_prolog_flag(break_level, 0 ),
	recorded('$toplevel_hooks',H,_),
	H \= fail, !,
	( call(user:H) -> true ; true).
'$run_toplevel_hooks'.

'$run_at_thread_start' :-
	recorded('$thread_initialization',M:D,_),
	'$meta_call'(D, M),
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
	  LF = ['Break (level ', BreakLevel, ')'|LD]
	),
    current_prolog_flag(debug, DBON),
	(
	 '$trace_on'
	->
     (
      var(LF)
     ->
      LD  = ['trace'|LP]
     ;
      LD  = [', trace '|LP]
     )
	;
	 DBON == true
	->
     (var(LF)
     ->
      LD  = ['debug'|LP]
     ;
      LD  = [', debug'|LP]
     )
	;
	 LD = LP
	),
    (
     var(LF)
    ->
     LP = [P]
    ;
     LP = [' ',P]
    ),
	yap_flag(toplevel_prompt, P),
	atomic_concat(LF, PF),
	prompt1(PF),
	prompt(_,' |   '),
    '$ensure_prompting'.


/**
@}  @}
*/


/**

@{
 @defgroup library The Prolog library



  @addtogroup YAPControl
@ingroup builtins
    @{
*/
:- system_module( '$_init', [!/0,
        ':-'/1,
        '?-'/1,
        []/0,
        extensions_to_present_answer/1,
        fail/0,
        false/0,
        goal_expansion/2,
        goal_expansion/3,
        otherwise/0,
        term_expansion/2,
        version/2,
	    '$do_log_upd_clause'/6,
        '$do_log_upd_clause0'/6,
        '$do_log_upd_clause_erase'/6,
        '$do_static_clause'/5], [
        '$system_module'/1]).

:- use_system_module( '$_boot', ['$cut_by'/1]).

%:- start_low_level_trace.

% This is the YAP init file
% should be consulted first step after booting

% These are pseudo declarations
% so that the user will get a redefining system predicate


% just create a choice-point
% the 6th argument marks the time-stamp.
'$do_log_upd_clause'(_,_,_,_,_,_).
'$do_log_upd_clause'(A,B,C,D,E,_) :-
	'$continue_log_update_clause'(A,B,C,D,E).
'$do_log_upd_clause'(_,_,_,_,_,_).


'$do_log_upd_clause_erase'(_,_,_,_,_,_).
'$do_log_upd_clause_erase'(A,B,C,D,E,_) :-
	'$continue_log_update_clause_erase'(A,B,C,D,E).
'$do_log_upd_clause_erase'(_,_,_,_,_,_).

'$do_log_upd_clause0'(_,_,_,_,_,_).
'$do_log_upd_clause0'(A,B,C,D,_,_) :-
	'$continue_log_update_clause'(A,B,C,D).
'$do_log_upd_clause0'(_,_,_,_,_,_).


'$do_static_clause'(_,_,_,_,_).
'$do_static_clause'(A,B,C,D,E) :-
	'$continue_static_clause'(A,B,C,D,E).
'$do_static_clause'(_,_,_,_,_).

%:- start_low_level_trace.
:- c_compile('arith.yap').
:- c_compile('builtins.yap').
%:- stop_low_level_trace.

:- '$all_current_modules'(M), yap_flag(M:unknown, error) ; true.


:- compile_expressions.


:- c_compile('directives.yap').
:- c_compile('imports.yap').
:- c_compile('bootutils.yap').
:- c_compile('bootlists.yap').
:- c_compile('consult.yap').
:- c_compile('preddecls.yap').
:- c_compile('preddyns.yap').
:- c_compile('meta.yap').
:- c_compile('newmod.yap').

:- c_compile('atoms.yap').
:- c_compile('os.yap').
:- c_compile('grammar.yap').
:- c_compile('errors.yap').
:- c_compile('absf.yap').

%:- set_prolog_flag(verbose_file_search, true ).
%:- yap_flag(write_strings,on).
%:- start_low_level_trace.
:- [
	 'preds.yap',
	 'modules.yap'
   ].


   :- use_module('error.yap').


:- [
    'utils.yap',
    'control.yap',
    'flags.yap'
].


:- [
    % lists is often used.
   	 '../os/yio.yap',
	 '../pl/debug.yap',
	 'checker.yap',
	 'depth_bound.yap',
	 'ground.yap',
	 'listing.yap',
    'arithpreds.yap',
	 % modules must be after preds, otherwise we will have trouble
	 % with meta-predicate expansion being invoked
	 % must follow grammar
	 'eval.yap',
	 'signals.yap',
	 'profile.yap',
	 'callcount.yap',
	 'load_foreign.yap',
%	 'save.yap',
	 'setof.yap',
	 'sort.yap',
	 'statistics.yap',
	 'strict_iso.yap',
	 'tabling.yap',
 	 'threads.yap',
	 'eam.yap',
	 'yapor.yap',
     'qly.yap',
     'spy.yap',
     'udi.yap'].


:- meta_predicate(log_event(+,:)).

:- dynamic prolog:'$user_defined_flag'/4.

:- multifile prolog:debug_action_hook/1.

:- multifile prolog:'$system_predicate'/2.

:-	 ['protect.yap'].

version(yap,[6,3]).

:- op(1150,fx,(mode)).

:- dynamic 'extensions_to_present_answer'/1.

:- 	['arrays.yap'].
%:- start_low_level_trace.

:- multifile user:portray_message/2.

:- dynamic user:portray_message/2.

/** @pred  _CurrentModule_:goal_expansion(+ _G_,+ _M_,- _NG_), user:goal_expansion(+ _G_,+ _M_,- _NG_)


YAP now supports goal_expansion/3. This is an user-defined
procedure that is called after term expansion when compiling or
asserting goals for each sub-goal in a clause. The first argument is
bound to the goal and the second to the module under which the goal
 _G_ will execute. If goal_expansion/3 succeeds the new
sub-goal  _NG_ will replace  _G_ and will be processed in the same
 way. If goal_expansion/3 fails the system will use the defaultyap+flrules.


*/
:- multifile user:goal_expansion/3.

:- dynamic user:goal_expansion/3.

:- multifile user:goal_expansion/2.

:- dynamic user:goal_expansion/2.

:- multifile system:goal_expansion/2.

:- dynamic system:goal_expansion/2.

:- multifile goal_expansion/2.

:- dynamic goal_expansion/2.

:- use_module('messages.yap').

:- 	['undefined.yap'].

:- use_module('hacks.yap').


:- use_module('attributes.yap').
:- use_module('corout.yap').
:- use_module('dialect.yap').
:- use_module('dbload.yap').
:- use_module('../library/ypp.yap').
:- use_module('../os/chartypes.yap').
:- ensure_loaded('../os/edio.yap').

yap_hacks:cut_by(CP) :- '$$cut_by'(CP).

:- '$change_type_of_char'(36,7). % Make $ a symbol character

:-	set_prolog_flag(generate_debug_info,true).

%
% cleanup ensure loaded and recover some data-base space.
%
%:- ( recorded('$lf_loaded',_,R), erase(R), fail ; true ).
%:- ( recorded('$module',_,R), erase(R), fail ; true ).

:- set_value('$user_module',user), '$protect'.

:- style_check([+discontiguous,+multiple,+single_var]).

%
% moved this to init_gc in gc.c to separate the alpha
%
% :- yap_flag(gc,on).

% :- yap_flag(gc_trace,verbose).

:- multifile
	prolog:comment_hook/3.

:- source.

:- module(user).


/** @pred  _CurrentModule_:term_expansion( _T_,- _X_),  user:term_expansion( _T_,- _X_)


This user-defined predicate is called by `expand_term/3` to
preprocess all terms read when consulting a file. If it succeeds:

+
If  _X_ is of the form `:- G` or `?- G`, it is processed as
a directive.
+
If  _X_ is of the form `$source_location`( _File_, _Line_): _Clause_` it is processed as if from `File` and line `Line`.

+
If  _X_ is a list, all terms of the list are asserted or processed
as directives.
+ The term  _X_ is asserted instead of  _T_.



*/
:- multifile term_expansion/2.

:- dynamic term_expansion/2.

:- multifile system:term_expansion/2.

:- dynamic system:term_expansion/2.

:- multifile swi:swi_predicate_table/4.

/** @pred  user:message_hook(+ _Term_, + _Kind_, + _Lines_)


Hook predicate that may be define in the module `user` to intercept
messages from print_message/2.  _Term_ and  _Kind_ are the
same as passed to print_message/2.  _Lines_ is a list of
format statements as described with print_message_lines/3.

This predicate should be defined dynamic and multifile to allow other
modules defining clauses for it too.


*/
:- multifile user:message_hook/3.

:- dynamic user:message_hook/3.

/** @pred  exception(+ _Exception_, + _Context_, - _Action_)


Dynamic predicate, normally not defined. Called by the Prolog system on run-time exceptions that can be repaired `just-in-time`. The values for  _Exception_ are described below. See also catch/3 and throw/1.
If this hook predicate succeeds it must instantiate the  _Action_ argument to the atom `fail` to make the operation fail silently, `retry` to tell Prolog to retry the operation or `error` to make the system generate an exception. The action `retry` only makes sense if this hook modified the environment such that the operation can now succeed without error.

+ `undefined_predicate`
 _Context_ is instantiated to a predicate-indicator ( _Module:Name/Arity_). If the predicate fails Prolog will generate an existence_error exception. The hook is intended to implement alternatives to the SWI built-in autoloader, such as autoloading code from a database. Do not use this hook to suppress existence errors on predicates. See also `unknown`.
+ `undefined_global_variable`
 _Context_ is instantiated to the name of the missing global variable. The hook must call nb_setval/2 or b_setval/2 before returning with the action retry.

*/

:- multifile user:exception/3.

:- dynamic user:exception/3.

:- ensure_loaded('../pl/pathconf.yap').

:- yap_flag(user:unknown,error).
