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
* File:		boot.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	boot file for Prolog					 *
*									 *
*************************************************************************/
% process an input clause

'$test'(I,D,H,[Y|L]) :-
	arg(I,D,X), ( X=':' ; integer(X)),
	arg(I,H,Y), var(Y), !,
	I1 is I-1,
	'$module_u_vars'(I1,D,H,L).


% This one should come first so that disjunctions and long distance
% cuts are compiled right with co-routining.
%

true :- true.

'$live' :-
	'$init_system',
        '$do_live'.

'$do_live' :-
	repeat,
		'$set_input'(user),'$set_output'(user),
		'$current_module'(Module),
		( Module==user ->
		    '$compile_mode'(_,0)
		;
		    format(user_error,'[~w]~n', [Module])
		),
		'$system_catch'('$enter_top_level',Module,Error,user:'$Error'(Error)).

'$init_system' :-
        % do catch as early as possible
	(
	 '$access_yap_flags'(15, 0), \+ '$uncaught_throw' ->
	  '$version'
	;
	  true
	),
	'$set_yap_flags'(10,0),
	set_value(fileerrors,1),
	set_value('$gc',on),
	set_value('$verbose',on),
	('$exit_undefp' -> true ; true),
	prompt('  ?- '),
	(
	    get_value('$break',0)
	->
	    % '$set_read_error_handler'(error), let the user do that
	    % after an abort, make sure all spy points are gone.
	    '$clean_debugging_info',
	    % simple trick to find out if this is we are booting from Prolog.
	    get_value('$user_module',V),
	    (  V = [] ->
		'$current_module'(_,prolog)
	    ;
		'$current_module'(_,V), '$compile_mode'(_,0),
		('$access_yap_flags'(16,0) ->
		    ( exists('~/.yaprc') -> [-'~/.yaprc'] ; true ),
		    ( exists('~/.prologrc') -> [-'~/.prologrc'] ; true ),
		    ( exists('~/prolog.ini') -> [-'~/prolog.ini'] ; true )
		;
		    true
		)
	    ),
	    '$db_clean_queues'(0),
	    '$startup_reconsult',
	    '$startup_goals'
	;
	    true
	).


%
% encapsulate $cut_by because of co-routining.
%
'$cut_by'(X) :- '$$cut_by'(X).

% Start file for yap

/*		I/O predicates						*/

/* meaning of flags for '$write' is
	 1	quote illegal atoms
	 2	ignore operator declarations
	 4	output '$VAR'(N) terms as A, B, C, ...
	 8	use portray(_)
*/

/* main execution loop							*/
'$read_vars'(Stream,T,Pos,V) :-
	'$read'(true,T,V,Pos,Err,Stream),
	(nonvar(Err) ->
	    '$print_message'(error,Err), fail
	    ;
	    true
	).

% reset alarms when entering top-level.
'$enter_top_level' :-
        '$alarm'(0, _),
	fail.
'$enter_top_level' :-
	'$clean_up_dead_clauses',
	fail.
'$enter_top_level' :-
	recorded('$restore_goal',G,R),
	erase(R),
	prompt(_,'   | '),
	'$system_catch'('$do_yes_no'((G->true),user),user,Error,user:'$Error'(Error)),
	fail.
'$enter_top_level' :-
	( recorded('$trace',on,_) ->
	    format(user_error, '% trace~n', [])
	;
	  recorded('$debug', on, _) ->
	  format(user_error, '% debug~n', [])
	),
	fail.
'$enter_top_level' :-
	prompt(_,'   ?- '),
	prompt('   | '),
	'$run_toplevel_hooks',
	'$read_vars'(user_input,Command,_,Varnames),
	set_value(spy_gn,1),
	( recorded('$spy_skip',_,R), erase(R), fail ; true),
	( recorded('$spy_stop',_,R), erase(R), fail ; true),
	prompt(_,'   |: '),
	'$command'((?-Command),Varnames,top),
	'$sync_mmapped_arrays',
	set_value('$live','$false').

'$startup_goals' :-
	recorded('$startup_goal',G,_),
	'$current_module'(Module),
	'$system_catch'('$query'((G->true), []),Module,Error,user:'$Error'(Error)),
	fail.
'$startup_goals'.

'$startup_reconsult' :-
	get_value('$consult_on_boot',X), X \= [], !,
	set_value('$consult_on_boot',[]),
	'$do_startup_reconsult'(X).
'$startup_reconsult'.

%
% remove any debugging info after an abort.
%
'$clean_debugging_info' :-
	recorded('$spy',_,R),
	erase(R),
	fail.
'$clean_debugging_info'.

'$erase_sets' :- 
		eraseall('$'),
		eraseall('$$set'),
		eraseall('$$one'), 
		eraseall('$reconsulted'), fail.
'$erase_sets' :- \+ recorded('$path',_,_), recorda('$path',"",_).
'$erase_sets'.

'$version' :- 
	get_value('$version_name',VersionName),
	'$print_message'(help, version(VersionName)),
	fail.
'$version' :- recorded('$version',VersionName,_),
	'$print_message'(help, VersionName),
	fail.
'$version'.

repeat :- '$repeat'.

'$repeat'.
'$repeat'.
'$repeat'.
'$repeat'.
'$repeat'.
'$repeat'.
'$repeat'.
'$repeat'.
'$repeat'.
'$repeat' :- '$repeat'.

'$start_corouts' :- recorded('$corout','$corout'(Name,_,_),R), Name \= main, finish_corout(R),
		fail.
'$start_corouts' :- 
		eraseall('$corout'),
		eraseall('$result'),
		eraseall('$actual'),
		fail.
'$start_corouts' :- recorda('$actual',main,_),
	recordz('$corout','$corout'(main,main,'$corout'([],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[])),_Ref),
	recorda('$result',going,_).

'$command'(C,VL,Con) :-
	'$access_yap_flags'(9,1), !,
	'$execute_command'(C,VL,Con,C).
'$command'(C,VL,Con) :-
	( (Con = top ; var(C) ; C = [_|_])  ->  
	'$execute_command'(C,VL,Con,C), ! ;
        expand_term(C, EC),
	'$execute_commands'(EC,VL,Con,C)
        ).

%
% Hack in case expand_term has created a list of commands.
%
'$execute_commands'(V,_,_,Source) :- var(V), !,
	'$do_error'(instantiation_error,meta_call(Source)).
'$execute_commands'([],_,_,_) :- !, fail.
'$execute_commands'([C|Cs],VL,Con,Source) :- !,
	(
	  '$execute_command'(C,VL,Con,Source)
	;
	  '$execute_commands'(Cs,VL,Con,Source)
	),
	fail.
'$execute_commands'(C,VL,Con,Source) :-
	'$execute_command'(C,VL,Con,Source).

%
%
%

'$execute_command'(C,_,top,Source) :- var(C), !,
	'$do_error'(instantiation_error,meta_call(Source)).
'$execute_command'(C,_,top,Source) :- number(C), !,
	'$do_error'(type_error(callable,C),meta_call(Source)).
'$execute_command'(R,_,top,Source) :- db_reference(R), !,
	'$do_error'(type_error(callable,R),meta_call(Source)).
'$execute_command'(end_of_file,_,_,_) :- !.
'$execute_command'((:-G),_,Option,_) :- !,
	'$current_module'(M),
	'$process_directive'(G, Option, M),
	fail.
'$execute_command'((?-G),V,_,Source) :- !,
	'$execute_command'(G,V,top,Source).
'$execute_command'(G,V,Option,Source) :-
	'$continue_with_command'(Option,V,G,Source).

%
% This command is very different depending on the language mode we are in.
%
% ISO only wants directives in files
% SICStus accepts everything in files
% YAP accepts everything everywhere
% 
'$process_directive'(G, top, M) :-
	'$access_yap_flags'(8, 0), !, % YAP mode, go in and do it,
	'$process_directive'(G, consult, M).
'$process_directive'(G, top, _) :- !,
	'$do_error'(context_error((:- G),clause),query).
%
% allow modules
%
'$process_directive'(M:G, Mode, _) :- !,
	'$process_directive'(G, Mode, M).
%
% default case
%
'$process_directive'(Gs, Mode, M) :-
	'$all_directives'(Gs), !,
	'$exec_directives'(Gs, Mode, M).

%
% ISO does not allow goals (use initialization).
%
'$process_directive'(D, _, M) :-
	'$access_yap_flags'(8, 1), !, % ISO Prolog mode, go in and do it,
	'$do_error'(context_error((:- M:D),query),directive).
%
% but YAP and SICStus does.
%
'$process_directive'(G, _, M) :-
	( '$do_yes_no'(G,M) -> true ; format(user_error,':- ~w:~w failed.~n',[M,G]) ),
	'$do_not_creep'.

'$continue_with_command'(reconsult,V,G,Source) :-
	'$go_compile_clause'(G,V,5,Source),
	fail.
'$continue_with_command'(consult,V,G,Source) :-
	'$go_compile_clause'(G,V,13,Source),
	fail.
'$continue_with_command'(top,V,G,_) :-
	'$query'(G,V),
	'$do_not_creep'.

%
% not 100% compatible with SICStus Prolog, as SICStus Prolog would put
% module prefixes all over the place, although unnecessarily so.
%
'$go_compile_clause'(Mod:G,V,N,Source) :- !,
	'$go_compile_clause'(G,V,N,Mod,Source).
'$go_compile_clause'((M:G :- B),V,N,Source) :- !,
	'$current_module'(M1),
	(M1 = M ->
	   NG = (G :- B)
        ;
	   '$preprocess_clause_before_mod_change'((G:-B),M1,M,NG)
	),
	'$go_compile_clause'(NG,V,N,M,Source).
'$go_compile_clause'(G,V,N,Source) :-
	'$current_module'(Mod),
	'$go_compile_clause'(G,V,N,Mod,Source).

'$go_compile_clause'(G, V, N, Mod, Source) :-
	'$prepare_term'(G, V, G0, G1, Mod, Source),
	'$$compile'(G1, G0, N, Mod).

'$prepare_term'(G, V, G0, G1, Mod, Source) :-
	( get_value('$syntaxcheckflag',on) ->
		'$check_term'(Source, V, Mod) ; true ),
	'$precompile_term'(G, G0, G1, Mod).

% process an input clause
'$$compile'(G, G0, L, Mod) :-
	'$head_and_body'(G,H,_), 
	'$flags'(H, Mod, Fl, Fl),
	is(NFl, /\, Fl, 0x00002000),
	( NFl \= 0 -> '$assertz_dynamic'(L,G,G0,Mod) ;
	    '$compile'(G, L, G0, Mod) ).

% process a clause for a static predicate 
'$$compile_stat'(G,G0,L,H, Mod) :-
      '$compile'(G,L,G0,Mod).

'$check_if_reconsulted'(N,A) :-
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
	X == '$', !,
	( recorded('$reconsulting',_,R) -> erase(R) ).

/* Executing a query */

'$query'(end_of_file,_).

% ***************************
% * -------- YAPOR -------- *
% ***************************

'$query'(G,V) :-
	\+ '$undefined'('$yapor_on', prolog),
	'$yapor_on',
        \+ '$undefined'('$start_yapor', prolog),
	'$parallelizable'(G), !,
	'$parallel_query'(G,V),
	fail.

% end of YAPOR

'$query'(G,[]) :- !,
	'$yes_no'(G,(?-)).
'$query'(G,V) :-
	(
	        ( recorded('$trace',on,_) -> '$creep' ; true),
		'$execute'(G),
		'$do_not_creep',
		'$extract_goal_vars_for_dump'(V,LIV),
		'$show_frozen'(G,LIV,LGs),
		'$write_answer'(V, LGs, Written),
		'$write_query_answer_true'(Written),
		'$another',
		!, fail ;
		'$do_not_creep',
		( '$undefined'('$print_message'(_,_),prolog) -> 
		   '$present_answer'(user_error,"no~n", [])
	        ;
		   print_message(help,no)
		),
		fail
	).

'$yes_no'(G,C) :-
	'$current_module'(M),
	'$do_yes_no'(G,M),
	'$do_not_creep',
	'$show_frozen'(G, [], LGs),
	'$write_answer'([], LGs, Written),
        ( Written = [] ->
	!,'$present_answer'(C, yes);
	'$another', !
	),
	fail.
'$yes_no'(_,_) :-
	'$do_not_creep',
	( '$undefined'('$print_message'(_,_),prolog) -> 
	   '$present_answer'(user_error,"no~n", [])
	;
	   print_message(help,no)
	),
	fail.

'$do_yes_no'([X|L], M) :- !, '$csult'([X|L], M).
'$do_yes_no'(G, M) :-
	  ( recorded('$trace',on,_) -> '$creep' ; true),
	  '$execute'(M:G).

'$extract_goal_vars_for_dump'([],[]).
'$extract_goal_vars_for_dump'([[_|V]|VL],[V|LIV]) :-
	'$extract_goal_vars_for_dump'(VL,LIV).

'$write_query_answer_true'([]) :- !,
	format(user_error,'~ntrue',[]).
'$write_query_answer_true'(_).

'$show_frozen'(_,_,[]) :-
	'$undefined'(all_attvars(LAV), attributes), !.
'$show_frozen'(G,V,LGs) :-
	attributes:all_attvars(LAV),
	LAV = [_|_], !,
	'$convert_to_list_of_frozen_goals'(V,LAV,G,LGs).
'$show_frozen'(_,_,[]).

%
% present_answer has three components. First it flushes the streams,
% then it presents the goals, and last it shows any goals frozen on
% the arguments.
%
'$present_answer'(_,_):-
        '$flush_all_streams',
	fail.
'$present_answer'((?-), Answ) :-
	get_value('$break',BL),
	( BL \= 0 -> 	format(user_error, '[~p] ',[BL]) ;
			true ),
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,Answ,Opts) ;
	   format(user_error,'~w',[Answ])
        ),
	format(user_error,'~n', []).

'$another' :-
	format(user_error,' ? ',[]),
	'$get0'(user_input,C),
	(   C== 0'; ->  '$skip'(user_input,10),
	    '$add_nl_outside_console',
	    fail
	;
	    C== 10 -> '$add_nl_outside_console',
		( '$undefined'('$print_message'(_,_),prolog) -> 
			format(user_error,'yes~n', [])
	        ;
		   print_message(help,yes)
		)
	;
	    C== -1 -> halt
	;
	    '$skip'(user_input,10), '$ask_again_for_another'
	).

'$add_nl_outside_console' :-
	'$is_same_tty'(user_input, user_error), !.
'$add_nl_outside_console' :-
	format(user_error,'~n',[]).

'$ask_again_for_another' :-
	format(user_error,'Action (\";\" for more choices, <return> for exit)', []),
	'$another'.

'$write_answer'(_,_,_) :-
        '$flush_all_streams',
	fail.
'$write_answer'(Vs, LBlk, LAnsw) :-
	'$purge_dontcares'(Vs,IVs),
	'$sort'(IVs, NVs),
	'$prep_answer_var_by_var'(NVs, LAnsw, LBlk),
	'$name_vars_in_goals'(LAnsw, Vs, NLAnsw),
        '$write_vars_and_goals'(NLAnsw).

'$purge_dontcares'([],[]).
'$purge_dontcares'([[[95|_]|_]|Vs],NVs) :- !,
	'$purge_dontcares'(Vs,NVs).
'$purge_dontcares'([V|Vs],[V|NVs]) :-
	'$purge_dontcares'(Vs,NVs).


'$prep_answer_var_by_var'([], L, L).
'$prep_answer_var_by_var'([[Name|Value]|L], LF, L0) :- 
	'$delete_identical_answers'(L, Value, NL, Names),
	'$prep_answer_var'([Name|Names], Value, LF, LI),
	'$prep_answer_var_by_var'(NL, LI, L0).

% fetch all cases that have the same solution.
'$delete_identical_answers'([], _, [], []).
'$delete_identical_answers'([[Name|Value]|L], Value0, FL, [Name|Names]) :-
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

'$write_vars_and_goals'([]).
'$write_vars_and_goals'([G1|LG]) :-
	'$write_goal_output'(G1),
	'$write_remaining_vars_and_goals'(LG).

'$write_remaining_vars_and_goals'([]).
'$write_remaining_vars_and_goals'([G1|LG]) :-
	format(user_error,',~n',[]),
	'$write_goal_output'(G1),
	'$write_remaining_vars_and_goals'(LG).

'$write_goal_output'(var([V|VL])) :-
	format(user_error,'~s',[V]),
	'$write_output_vars'(VL).
'$write_goal_output'(nonvar([V|VL],B)) :-
	format(user_error,'~s',[V]),
	'$write_output_vars'(VL),
	format(user_error,' = ', []),
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,B,Opts) ;
	   format(user_error,'~w',[B])
        ).
'$write_goal_output'(_-G) :-
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,G,Opts) ;
	   format(user_error,'~w',[G])
        ).

'$name_vars_in_goals'(G, VL0, NG) :-
	'$copy_term_but_not_constraints'(G+VL0, NG+NVL0),
	'$name_well_known_vars'(NVL0),
	'$variables_in_term'(NG, [], NGVL),
	'$name_vars_in_goals1'(NGVL, 0, _).

'$name_well_known_vars'([]).
'$name_well_known_vars'([[Name|V]|NVL0]) :-
	var(V), !,
	V = '$VAR'(Name),
	'$name_well_known_vars'(NVL0).
'$name_well_known_vars'([_|NVL0]) :-
	'$name_well_known_vars'(NVL0).

'$name_vars_in_goals1'([], I, I).
'$name_vars_in_goals1'(['$VAR'([95|Name])|NGVL], I0, IF) :-
	I is I0+1,
	'$gen_name_string'(I0,[],Name), !,
	'$name_vars_in_goals1'(NGVL, I, IF).
'$name_vars_in_goals1'([NV|NGVL], I0, IF) :-
	nonvar(NV),
	'$name_vars_in_goals1'(NGVL, I0, IF).

'$write_output_vars'([]).
'$write_output_vars'([V|VL]) :-
	format(user_error,' = ~s',[V]),
	'$write_output_vars'(VL).

call(G) :- '$execute'(G).

incore(G) :- '$execute'(G).

%
% standard meta-call, called if $execute could not do everything.
%
'$meta_call'(G, M) :-
	'$save_current_choice_point'(CP),
	'$call'(G, CP, G, M).


','(X,Y) :-
	'$save_current_choice_point'(CP),
	'$current_module'(M),
        '$call'(X,CP,(X,Y),M),
        '$call'(Y,CP,(X,Y),M).
';'(X,Y) :-
	'$save_current_choice_point'(CP),
	'$current_module'(M),
        ( '$call'(X,CP,(X;Y),M) ; '$call'(Y,CP,(X;Y),M) ).
'|'(X,Y) :-
	'$save_current_choice_point'(CP),
	'$current_module'(M),
        ( '$call'(X,CP,(X|Y),M) ; '$call'(Y,CP,(X|Y),M) ).
'->'(X,Y) :-
	'$save_current_choice_point'(CP),
	'$current_module'(M),
        ( '$call'(X,CP,G0,M) -> '$call'(Y,CP,(X->Y),M) ).
\+(G) :-     \+ '$execute'(G).
not(G) :-    \+ '$execute'(G).


%
% do it in ISO mode.
%
'$meta_call'(G,_ISO,M) :-
	'$iso_check_goal'(G,G),
	'$save_current_choice_point'(CP),
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
'$call'((X->Y; Z),CP,G0,M) :- !,
	(
	    '$call'(X,CP,G0,M)
         ->
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
'$call'((A|B),CP, G0,M) :- !,
	(
	    '$call'(A,CP,G0,M)
        ;
	    '$call'(B,CP,G0,M)
	).
'$call'(\+ X, CP, G0, M) :- !,
	\+ '$execute'(X).
'$call'(not(X), CP, G0, M) :- !,
	\+ '$execute'(X).
'$call'(!, CP, _,_) :- !,
	'$$cut_by'(CP).
'$call'([A|B], _, _, M) :- !,
	'$csult'([A|B], M).
'$call'(G, CP, G0, CurMod) :-
	( '$is_expand_goal_or_meta_predicate'(G,CurMod) ->
	   (
	     user:goal_expansion(G, CurMod, NG) ->
	       '$call'(NG, CP, G0,CurMod)
	     ;
	       % repeat other code.
             '$is_metapredicate'(G,CurMod) ->
	       (
	         '$meta_expansion'(CurMod,CurMod,G,NG,[]) ->
	         '$execute0'(NG, CurMod)
	       ;
	         '$execute0'(G, CurMod)
	       )
	   ;
	     '$execute0'(G, CurMod)
	   )
	;
	  '$execute0'(G, CurMod)
	).

'$check_callable'(V,G) :- var(V), !,
	'$current_module'(Mod),
	'$do_error'(instantiation_error,Mod:G).
'$check_callable'(A,G) :- number(A), !,
	'$current_module'(Mod),
	'$do_error'(type_error(callable,A),Mod:G).
'$check_callable'(R,G) :- db_reference(R), !,
	'$current_module'(Mod),
	'$do_error'(type_error(callable,R),Mod:G).
'$check_callable'(_,_).

% Called by the abstract machine, if no clauses exist for a predicate
'$undefp'([M|G]) :-
	% make sure we do not loop on undefined predicates
        % for undefined_predicates.
	'$enter_undefp',
	'$do_undefp'(G,M).

'$do_undefp'(G,M) :-
	functor(G,F,N),
	recorded('$import','$import'(S,M,F,N),_),
	S \= M, % can't try importing from the module itself.
	!,
	'$exit_undefp',
	(
	  '$meta_expansion'(S,M,G,G1,[])
	   ->
	  '$execute'(S:G1)
	;
	  '$execute'(S:G)
	).
'$do_undefp'(G,M) :-
	'$is_expand_goal_or_meta_predicate'(G,M),
	'$system_catch'(goal_expansion(G, M, NG), user, _, fail), !,
	'$exit_undefp',
	'$execute0'(NG,M).
'$do_undefp'(G,M) :-
	\+ '$undefined'(unknown_predicate_handler(_,_,_), user),
	'$system_catch'(unknown_predicate_handler(G,M,NG), user, Error, '$leave_undefp'(Error)),
	'$exit_undefp', !,
	'$execute'(user:NG).
'$do_undefp'(G,M) :-
	recorded('$unknown','$unknown'(M:G,US),_), !,
	'$exit_undefp',
	'$execute'(user:US).
'$do_undefp'(_,_) :-
	'$exit_undefp',
	fail.

'$leave_undefp'(Ball) :-
	'$exit_undefp',
	throw(Ball).


/* This is the break predicate,
	it saves the importante data about current streams and
	debugger state */

break :-
	( recorded('$trace',Val,R) -> Trace = Val, erase(R); true),
	( recorded('$debug',Val,R1) -> Debug = Val, erase(R1); true),
	get_value('$break',BL), NBL is BL+1,
	get_value(spy_gn,SPY_GN),
	'$access_yap_flags'(10,SPY_CREEP),
	get_value(spy_cl,SPY_CL),
	get_value(spy_leap,_Leap),
	set_value('$break',NBL),
	current_output(OutStream), current_input(InpStream),
	format(user_error, '% Break (level ~w)~n', [NBL]),
	'$do_live',
	!,
	set_value('$live','$true'),
	set_value(spy_gn,SPY_GN),
	'$set_yap_flags'(10,SPY_CREEP),
	set_value(spy_cl,SPY_CL),
	set_value(spy_leap,_Leap),
	'$set_input'(InpStream), '$set_output'(OutStream),
	( recorded('$trace',_,R2), erase(R2), fail; true),
	( recorded('$debug',_,R3), erase(R3), fail; true),
	(nonvar(Trace) -> recorda('$trace',Trace,_)),
	(nonvar(Debug) -> recorda('$debug',Debug,_)),
	set_value('$break',BL).


'$csult'(V, _) :- var(V), !,
	'$do_error'(instantiation_error,consult(V)).
'$csult'([], _).
'$csult'([-F|L], M) :- !, '$reconsult'(F, M), '$csult'(L, M).
'$csult'([F|L], M) :- '$consult'(F, M), '$csult'(L, M).

'$consult'(V, _) :- var(V), !,
	'$do_error'(instantiation_error,consult(V)).
'$consult'([], _) :- !.
'$consult'([F|Fs], M) :- !,
	'$consult'(F, M),
	'$consult'(Fs, M).
'$consult'(M:X, _) :- !,
	( atom(M) ->
	    '$consult'(X, M)
	;
	    '$do_error'(type_error(atom,M),[M:X])
	).
'$consult'(X, OldModule) :-
	'$find_in_path'(X,Y,consult(X)),
	'$open'(Y,'$csult',Stream,0), !,
        '$consult'(X,OldModule,Stream),
	'$close'(Stream).
'$consult'(X, _) :-
	'$do_error'(permission_error(input,stream,X),[X]).


'$consult'(_,Module,Stream) :-
        '$record_loaded'(Stream,Module),
	fail.
'$consult'(F,Module,Stream) :-
	'$access_yap_flags'(8, 2), % SICStus Prolog compatibility
	!,
	'$reconsult'(F,Module,Stream).
'$consult'(F,Mod,Stream) :-
	'$current_module'(OldModule, Mod),
	'$getcwd'(OldD),
	get_value('$consulting_file',OldF),
	'$set_consulting_file'(Stream),
	H0 is heapused, '$cputime'(T0,_),
	'$current_stream'(File,_,Stream),
	'$start_consult'(consult,File,LC),
	get_value('$consulting',Old),
	set_value('$consulting',true),
	recorda('$initialisation','$',_),
	( '$undefined'('$print_message'(_,_),prolog) -> 
	    ( get_value('$verbose',on) ->
		format(user_error, '~*|% consulting ~w...~n', [LC,F])
		; true )
	;
	    '$print_message'(informational, loading(consulting, File))
	),
	( recorded('$trace', on, TraceR) -> erase(TraceR) ; true),
	'$loop'(Stream,consult),
	'$end_consult',
	( nonvar(TraceR) -> recorda('$trace', on, _) ; true),
	set_value('$consulting',Old),
	set_value('$consulting_file',OldF),
	'$current_module'(NewMod,OldModule),
	'$cd'(OldD),
	( LC == 0 -> prompt(_,'   |: ') ; true),
	H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
	( '$undefined'('$print_message'(_,_),prolog) -> 
	  ( get_value('$verbose',on) ->
	     format(user_error, '~*|% ~w consulted ~w bytes in ~d msecs~n', [LC,F,H,T])
	  ;
	     true
	  )
	;
	    '$print_message'(informational, loaded(consulted, File, NewMod, T, H))
	),
	'$exec_initialisation_goals',
	!.


'$record_loaded'(user, _).
'$record_loaded'(user_input, _).
'$record_loaded'(Stream, M) :-
	'$loaded'(Stream, M, _), !.
'$record_loaded'(Stream, M) :-
	'$file_name'(Stream,F),
	'$file_age'(F,Age),
	recorda('$loaded','$loaded'(F,M,Age),_).

'$set_consulting_file'(user) :- !,
	set_value('$consulting_file',user_input).
'$set_consulting_file'(user_input) :- !,
	set_value('$consulting_file',user_input).
'$set_consulting_file'(Stream) :-
	'$file_name'(Stream,F),
	set_value('$consulting_file',F),
	'$set_consulting_dir'(F).

%
% Use directory where file exists
%
'$set_consulting_dir'(F) :-
	atom_codes(F,S),
	'$strip_file_for_scd'(S,Dir,Unsure,Unsure),
	'$cd'(Dir).

%
% The algorithm: I have two states, one for what I am sure will be an answer,
% the other for what I have found so far.
%
'$strip_file_for_scd'([], [], _, _).
'$strip_file_for_scd'([D|L], Out, Out, Cont) :-
	'$dir_separator'(D), !,
	'$strip_file_for_scd'(L, Cont, [D|C2], C2).
'$strip_file_for_scd'([F|L], Out, Cont, [F|C2]) :-
	'$strip_file_for_scd'(L, Out, Cont, C2).
	

'$loop'(Stream,Status) :-
	'$change_alias_to_stream'('$loop_stream',Stream),
	repeat,
		( '$current_stream'(_,_,Stream) -> true
		 ; '$abort_loop'(Stream)
		),
		prompt('|     '), prompt(_,'| '),
		'$current_module'(OldModule),
		'$system_catch'('$enter_command'(Stream,Status), OldModule, Error,
			 user:'$LoopError'(Error, Status)),
	!.

'$enter_command'(Stream,Status) :-
	'$read_vars'(Stream,Command,_,Vars),
	'$command'(Command,Vars,Status).

'$abort_loop'(Stream) :-
	'$do_error'(permission_error(input,closed_stream,Stream), loop).

/* General purpose predicates				*/

'$head_and_body'((H:-B),H,B) :- !.
'$head_and_body'(H,H,true).

%
% split head and body, generate an error if body is unbound.
%
'$check_head_and_body'((H:-B),H,B,P) :- !,
	'$check_head'(H,P).
'$check_head_and_body'(H,H,true,P) :-
	'$check_head'(H,P).

'$check_head'(H,P) :- var(H), !,
	'$do_error'(instantiation_error,P).
'$check_head'(H,P) :- number(H), !,
	'$do_error'(type_error(callable,H),P).
'$check_head'(H,P) :- db_reference(H), !,
	'$do_error'(type_error(callable,H),P).
'$check_head'(_,_).

% Path predicates

'$exists'(F,Mode) :-
	get_value(fileerrors,V),
	set_value(fileerrors,0),
	( '$open'(F,Mode,S,0) -> '$close'(S), set_value(fileerrors,V) ; set_value(fileerrors,V), fail).


'$find_in_path'(user,user_input, _) :- !.
'$find_in_path'(user_input,user_input, _) :- !.
'$find_in_path'(S,NewFile, _) :-
	S =.. [Name,File], !,
	'$dir_separator'(D),
	atom_codes(A,[D]),
	( user:file_search_path(Name, Dir), '$do_not_creep' ; '$do_not_creep', fail),
	'$extend_path'(Dir,A,File,NFile),
	'$search_in_path'(NFile, NewFile).
'$find_in_path'(File,NewFile,_) :- atom(File), !,
	'$search_in_path'(File,NewFile),!.
'$find_in_path'(File,_,Call) :-
	'$do_error'(domain_error(source_sink,File),Call).

'$search_in_path'(New,New) :-
	'$exists'(New,'$csult'), !.
'$search_in_path'(File,New) :-
	recorded('$path',Path,_),
	atom_concat([Path,File],New),
	'$exists'(New,'$csult').

'$extend_path'(Dir,A,File,NFile) :-
	atom(Dir), !,
	atom_concat([Dir,A,File],NFile).
'$extend_path'(Name,A,File,NFile) :-
	nonvar(Name),
	Name =.. [Dir1,Dir2],
	( user:file_search_path(Dir1, Dir), '$do_not_creep' ; '$do_not_creep', fail),
	'$extend_path'(Dir2,A,File,EFile),
	atom_concat([Dir,A,EFile],NFile).

% term expansion
%
% return two arguments: Expanded0 is the term after "USER" expansion.
%                       Expanded is the final expanded term.
%
'$precompile_term'(Term, Expanded0, Expanded, Mod) :-
	(
	    '$access_yap_flags'(9,1)      /* strict_iso on */
        ->
	    '$expand_term_modules'(Term, Expanded0, Expanded, Mod),
	    '$check_iso_strict_clause'(Expanded0)
        ;
	    '$expand_term_modules'(Term, Expanded0, ExpandedI, Mod),
	    '$expand_array_accesses_in_term'(ExpandedI,Expanded)
	).

expand_term(Term,Expanded) :-
	( \+ '$undefined'(term_expansion(_,_), user),
	  user:term_expansion(Term,Expanded),
	 '$do_not_creep'
        ;
	  '$do_not_creep',
	  '$expand_term_grammar'(Term,Expanded)
	),
!.


%
% Grammar Rules expansion
%
'$expand_term_grammar'((A-->B), C) :-
	'$translate_rule'((A-->B),C), !.
'$expand_term_grammar'(A, A).

%
% Arithmetic expansion
%
'$expand_term_arith'(G1, G2) :-
	get_value('$c_arith',true),
	'$c_arith'(G1, G2), !.
'$expand_term_arith'(G,G).


%
% Arithmetic expansion
%
'$expand_array_accesses_in_term'(Expanded0,ExpandedF) :-
	'$array_refs_compiled',
	'$c_arrays'(Expanded0,ExpandedF), !.
'$expand_array_accesses_in_term'(Expanded,Expanded).

%
% Module system expansion
%
'$expand_term_modules'(A,B,C,M) :- '$module_expansion'(A,B,C,M), !.
'$expand_term_modules'(A,A,A,_).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   catch/throw implementation

% at each catch point I need to know:
% what is ball;
% where was the previous catch	
catch(G, C, A) :-
	'$catch'(C,A,_),
	'$execute'(G).


% system_catch is like catch, but it avoids the overhead of a full
% meta-call by calling '$execute0' instead of $execute.
% This way it
% also avoids module preprocessing and goal_expansion
%
'$system_catch'(G, M, C, A) :-
	% check current trail
	'$catch'(C,A,_),
	'$execute_nonstop'(G, M).

%
% throw has to be *exactly* after system catch!
%
throw(Ball) :-
	% get this off the unwound computation.
	copy_term(Ball,NewBall),
	% get current jump point
	'$jump_env_and_store_ball'(NewBall).


% just create a choice-point
'$catch'(_,_,_).
'$catch'(_,_,_) :- fail.

'$handle_throw'(_, _, _).
'$handle_throw'(C, A, Ball) :-
        % reset info 
	(Ball \== '$abort', C = Ball ->
	    '$execute'(A)
	    ;
	    throw(Ball)
	).

'$exec_initialisation_goals' :-
	recorded('$blocking_code',_,R),
	erase(R),
	fail.
% system goals must be performed first 
'$exec_initialisation_goals' :-
	recorded('$system_initialisation',G,R),
	erase(R),
	G \= '$',
	call(G),
	fail.
'$exec_initialisation_goals' :-
	recorded('$initialisation',G,R),
	erase(R),
	G \= '$',
	'$current_module'(M),
	'$system_catch'(once(M:G), M, Error, user:'$LoopError'(Error, top)),
	'$do_not_creep',
	fail.
'$exec_initialisation_goals'.

'$run_toplevel_hooks' :-
	get_value('$break',0),
	recorded('$toplevel_hooks',H,_), !,
	( '$execute'(H) -> true ; true),
	'$do_not_creep'.
'$run_toplevel_hooks'.

