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
		( Module=user ->
		    '$compile_mode'(_,0)
		;
		    '$format'(user_error,"[~w]~n", [Module])
		),
		'$system_catch'('$enter_top_level',Module,Error,user:'$Error'(Error)).

read_sig :-
	recorded('$sig_handler',X,_),
	writeq(X),nl,
	fail.
read_sig.


'$init_system' :-
        % do catch as early as possible
	(
	 '$access_yap_flags'(15, 0) ->
	  '$version'
	;
	  true
	),
	% If this is not here, the following get written twice in the idb. Why?
	eraseall('$sig_handler'),
	% The default interrupt handlers are kept, so that it's
	% possible to revert to them with on_signal(S,_,default)
	recordz('$sig_handler',default(sig_hup,
		         (( exists('~/.yaprc') -> [-'~/.yaprc'] ; true ),
			  ( exists('~/.prologrc') -> [-'~/.prologrc'] ; true ),
			  ( exists('~/prolog.ini') -> [-'~/prolog.ini'] ; true ))), _),
	recordz('$sig_handler',default(sig_usr1,
		       (nl,writeq('[ Received user signal 1 ]'),nl,halt)), _),
	recordz('$sig_handler',default(sig_usr2,
		       (nl,writeq('[ Received user signal 2 ]'),nl,halt)), _),
	% The current interrupt handlers are also set the default values
	recordz('$sig_handler',action(sig_hup,
		         (( exists('~/.yaprc') -> [-'~/.yaprc'] ; true ),
			  ( exists('~/.prologrc') -> [-'~/.prologrc'] ; true ),
			  ( exists('~/prolog.ini') -> [-'~/prolog.ini'] ; true ))), _),
	recordz('$sig_handler',action(sig_usr1,
		       (nl,writeq('[ Received user signal 1 ]'),nl,halt)), _),
	recordz('$sig_handler',action(sig_usr2,
		       (nl,writeq('[ Received user signal 2 ]'),nl,halt)), _),
	'$set_yap_flags'(10,0),
	set_value('$gc',on),
	set_value('$verbose',on),
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
	( get_value('$trace', 1) ->
	    '$format'(user_error, "[trace]~n", [])
	;
	  get_value(debug, 1) ->
	    '$format'(user_error, "[debug]~n", [])
	),
	fail.
'$enter_top_level' :-
	prompt(_,'   ?- '),
	prompt('   | '),
	'$read_vars'(user_input,Command,_,Varnames),
	set_value(spy_fs,0),
	set_value(spy_sp,0),
	set_value(spy_gn,1),
	set_value(spy_skip,off),
	set_value(spy_stop,on),
	prompt(_,'   |: '),
	'$run_toplevel_hooks',
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
	'$format'(user_error, "[ YAP version ~w ]~n", [VersionName]),
	fail.
'$version' :- recorded('$version',VersionName,_),
	'$format'(user_error, "~w~n", [VersionName]),
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
	'$execute_command'(C,VL,Con).
'$command'(C,VL,Con) :-
	( (Con = top ; var(C) ; C = [_|_])  ->  
	'$execute_command'(C,VL,Con) ;
        expand_term(C, EC),
	'$execute_commands'(EC,VL,Con)
        ).

%
% Hack in case expand_term has created a list of commands.
%
'$execute_commands'(V,_,_) :- var(V), !,
	'$do_error'(instantiation_error,meta_call(V)).
'$execute_commands'([],_,_) :- !, fail.
'$execute_commands'([C|_],VL,Con) :-
	'$execute_command'(C,VL,Con).
'$execute_commands'([_|Cs],VL,Con) :- !,
	'$execute_commands'(Cs,VL,Con).
'$execute_commands'(C,VL,Con) :-
	'$execute_command'(C,VL,Con).

%
%
%

'$execute_command'(C,_,top) :- var(C), !,
	'$do_error'(instantiation_error,meta_call(C)).
'$execute_command'(C,_,top) :- number(C), !,
	'$do_error'(type_error(callable,C),meta_call(C)).
'$execute_command'(R,_,top) :- db_reference(R), !,
	'$do_error'(type_error(callable,R),meta_call(R)).
'$execute_command'(end_of_file,_,_) :- !.
'$execute_command'((:-G),_,Option) :- !,
	'$current_module'(M),
	'$process_directive'(G, Option, M),
	fail.
'$execute_command'((?-G),V,_) :- !,
	'$execute_command'(G,V,top).
'$execute_command'(G,V,Option) :- '$continue_with_command'(Option,V,G).

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
% always allow directives.
%
'$process_directive'(D, Mode, M) :-
	'$directive'(D), !,
	( '$exec_directive'(D, Mode, M) -> true ; true ).
%
% allow multiple directives
%
'$process_directive'((G1,G2), Mode, M) :-
	'$all_directives'(G1),
	'$all_directives'(G2), !,
	'$exec_directives'(G1, Mode, M),
	'$exec_directives'(G2, Mode, M).
%
% allow modules
%
'$process_directive'(M:G, Mode, _) :- !,
	'$process_directive'(G, Mode, M).
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
	( '$do_yes_no'(G,M) -> true ; '$format'(user_error,":- ~w:~w failed.~n",[M,G]) ).

'$all_directives'(_:G1) :- !,
	'$all_directives'(G1).
'$all_directives'((G1,G2)) :- !,
	'$all_directives'(G1),
	'$all_directives'(G2).
'$all_directives'(G) :- !,
	'$directive'(G).

'$continue_with_command'(reconsult,V,G) :-
	'$go_compile_clause'(G,V,5),
	fail.
'$continue_with_command'(consult,V,G) :-
	'$go_compile_clause'(G,V,13),
	fail.
'$continue_with_command'(top,V,G) :-
	'$query'(G,V).

%
% not 100% compatible with SICStus Prolog, as SICStus Prolog would put
% module prefixes all over the place, although unnecessarily so.
%
'$go_compile_clause'(Mod:G,V,N) :- !,
	'$go_compile_clause'(G,V,N,Mod).
'$go_compile_clause'((M:G :- B),V,N) :- !,
	'$current_module'(M1),
	(M1 = M ->
	   NG = (G :- B)
        ;
	   '$preprocess_clause_before_mod_change'((G:-B),M1,M,NG)
	),
	'$go_compile_clause'(NG,V,N,M).
'$go_compile_clause'(G,V,N) :-
	'$current_module'(Mod),
	'$go_compile_clause'(G,V,N,Mod).

'$go_compile_clause'(G, V, N, Mod) :-
	'$prepare_term'(G, V, G0, G1, Mod),
	'$$compile'(G1, G0, N, Mod).

'$prepare_term'(G,V,G0,G1, Mod) :-
	( get_value('$syntaxcheckflag',on) ->
		'$check_term'(G,V,Mod) ; true ),
	'$precompile_term'(G, G0, G1, Mod).

% process an input clause
'$$compile'(G, G0, L, Mod) :-
	'$head_and_body'(G,H,_), 
	'$inform_of_clause'(H,L),
	'$flags'(H, Mod, Fl, Fl),
	( Fl /\ 16'000008 =\= 0 -> '$compile'(G,L,G0,Mod)
	;
	  Fl /\ 16'002000 =\= 0 -> '$assertz_dynamic'(L,G,G0,Mod) ;
	    '$$compile_stat'(G,G0,L,H, Mod) ).

% process a clause for a static predicate 
'$$compile_stat'(G,G0,L,H, Mod) :-
      '$compile'(G,L,G0,Mod),
      % first occurrence of this predicate in this file,
      % check if we need to erase the source and if 
      % it is a multifile procedure.
      '$flags'(H,Mod,Fl,Fl),
      ( get_value('$abol',true)
         ->
            ( Fl /\ 16'400000 =\= 0 -> '$erase_source'(H, Mod) ; true ),
	    ( Fl /\ 16'040000 =\= 0 -> '$check_multifile_pred'(H,Mod,Fl) ; true )
        ;
            true
      ),	    
      ( Fl /\ 16'400000 =:= 0 ->       % is this procedure in source mode?
        % no, just ignore
        true
      ;
	% and store our clause
	'$store_stat_clause'(G0, H, L, Mod)
      ).

'$store_stat_clause'(G0, H, L, M) :-
	'$head_and_body'(G0,H0,B0),
	'$record_stat_source'(M:H,(H0:-B0),L,R),
	( '$is_multifile'(H,M) -> 
	    get_value('$consulting_file',F),
	    functor(H, Na, Ar),
	    recordz('$multifile'(_,_,_), '$mf'(Na,Ar,M,F,R), _) 
	;
	   true
        ).	

'$erase_source'(G, M) :- 
	'$is_multifile'(G, M), !,
	functor(G, Na, Ar),
	'$erase_mf_source'(Na, Ar, M).
'$erase_source'(G, M) :- '$recordedp'(M:G,_,R), erase(R), fail.
'$erase_source'(_, _).

'$erase_mf_source'(Na, Ar, M) :-
	get_value('$consulting_file',F),
	recorded('$multifile'(_,_,_), '$mf'(Na,Ar,M,F,R), R1),
	erase(R1),
	erase(R),
	fail.
'$erase_mf_source'(Na, Ar, M) :-
	get_value('$consulting_file',F),
	recorded('$multifile_dynamic'(_,_,_), '$mf'(Na,Ar,M,F,R), R1),
	erase(R1),
	erase(R),
	fail.
'$erase_mf_source'(_,_,_).

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
		'$start_creep',
		'$execute'(G),
		'$stop_creep',
		'$extract_goal_vars_for_dump'(V,LIV),
		'$show_frozen'(G,LIV,LGs),
		'$write_answer'(V, LGs, Written),
		'$write_query_answer_true'(Written),
		'$another',
		!, fail ;
		'$stop_creep',
		'$present_answer'(_, no),
		fail
	).

'$yes_no'(G,C) :-
	'$current_module'(M),
	'$do_yes_no'(G,M),
	'$stop_creep',
	'$show_frozen'(G, [], LGs),
	'$write_answer'([], LGs, Written),
        ( Written = [] ->
	!,'$present_answer'(C, yes);
	'$another', !
	),
	fail.
'$yes_no'(_,_) :-
	'$stop_creep',
	'$present_answer'(_, no),
	fail.


'$start_creep' :-
	( get_value('$trace', 1) ->
	    '$creep'
	;
	  '$setflop'(1)
	).

'$do_yes_no'([X|L], M) :- !, '$csult'([X|L], M).
'$do_yes_no'(G, M) :- '$start_creep', '$execute'(M:G).

'$extract_goal_vars_for_dump'([],[]).
'$extract_goal_vars_for_dump'([[_|V]|VL],[V|LIV]) :-
	'$extract_goal_vars_for_dump'(VL,LIV).

'$write_query_answer_true'([]) :- !,
	'$format'(user_error,"~ntrue",[]).
'$write_query_answer_true'(_).

'$show_frozen'(G,V,LGs) :-
	'$all_frozen_goals'(LGs0), LGs0 = [_|_], !,
	attributes:all_attvars(LAV),
	'$convert_to_list_of_frozen_goals'(LGs0,V,LAV,G,LGs).
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
	( BL \= 0 -> 	'$format'(user_error, "[~p] ",[BL]) ;
			true ),
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,Answ,Opts) ;
	   '$format'(user_error,"~w",[Answ])
        ),
	'$format'(user_error,"~n", []).

'$another' :-
	'$format'(user_error," ? ",[]),
	'$get0'(user_input,C),
	(   C== 0'; ->  '$skip'(user_input,10),
	    '$add_nl_outside_console',
	    fail
	;
	    C== 10 -> '$add_nl_outside_console',
		'$format'(user_error,"yes~n", [])
	;
	    C== -1 -> halt
	;
	    '$skip'(user_input,10), '$ask_again_for_another'
	).

'$add_nl_outside_console' :-
	'$is_same_tty'(user_input, user_error), !.
'$add_nl_outside_console' :-
	'$format'(user_error,"~n",[]).
	
	

'$ask_again_for_another' :-
	'$format'(user_error,"Action (\";\" for more choices, <return> for exit)", []),
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
	'$format'(user_error,",~n",[]),
	'$write_goal_output'(G1),
	'$write_remaining_vars_and_goals'(LG).

'$write_goal_output'(var([V|VL])) :-
	'$format'(user_error,"~s",[V]),
	'$write_output_vars'(VL).
'$write_goal_output'(nonvar([V|VL],B)) :-
	'$format'(user_error,"~s",[V]),
	'$write_output_vars'(VL),
	'$format'(user_error," = ", []),
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,B,Opts) ;
	   '$format'(user_error,"~w",[B])
        ).
'$write_goal_output'(_-G) :-
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,G,Opts) ;
	   '$format'(user_error,"~w",[G])
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
	'$format'(user_error," = ~s",[V]),
	'$write_output_vars'(VL).

call(G) :- '$execute'(G).

incore(G) :- '$execute'(G).

%
% standard meta-call, called if $execute could not do everything.
%
'$meta_call'(G, M) :-
	'$save_current_choice_point'(CP),
	'$call'(G, CP, G, M).

%
% do it in ISO mode.
%
'$meta_call'(G,_ISO,M) :-
	'$iso_check_goal'(G,G),
	'$save_current_choice_point'(CP),
	'$call'(G, CP, G, M).

'$meta_call'(G, CP, G0, M) :-
	'$call'(G, CP, G0, M).

'$spied_meta_call'(G, M) :-
	'$save_current_choice_point'(CP),
	'$spied_call'(G, CP, G, M).

'$spied_meta_call'(G, CP, G0, M) :-
	'$spied_call'(G, CP, G0, M).

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
'$call'(\+ X, _, _,_) :- !,
	\+ '$execute'(X).
'$call'(not(X), _, _,_) :- !,
	\+ '$execute'(X).
'$call'(!, CP, _,_) :- !,
	'$$cut_by'(CP).
'$call'([A|B], _, _, M) :- !,
	'$csult'([A|B], M).
'$call'(A, _, _,CurMod) :-
	(
	  % goal_expansion is defined, or
	'$pred_goal_expansion_on' ->
	  '$expand_call'(A,CurMod)
        % this is a meta-predicate
        ; '$flags'(A,CurMod,F,_), F /\ 0x200000 =:= 0x200000 ->
	  '$expand_call'(A, CurMod)
	;
	  '$execute0'(A, CurMod)
	).

'$expand_call'(A,CurMod) :-	
	'$expand_goal'(A, CurMod, CurMod, NG, NMod),
	'$execute0'(NG, NMod).

'$spied_call'((A,B),CP,G0,M) :- !,
	'$call'(A,CP,G0,M),
	'$call'(B,CP,G0,M).
'$spied_call'((X->Y),CP,G0,M) :- !,
	(
	    '$call'(X,CP,G0,M)
          ->
	    '$call'(Y,CP,G0,M)
	).
'$spied_call'((X->Y; Z),CP,G0,M) :- !,
	(
	    '$call'(X,CP,G0,M)
         ->
	    '$call'(Y,CP,G0,M)
        ;
	    '$call'(Z,CP,G0,M)
	).
'$spied_call'((A;B),CP,G0,M) :- !,
	(
	    '$call'(A,CP,G0,M)
        ;
	    '$call'(B,CP,G0,M)
	).
'$spied_call'((A|B),CP,G0,M) :- !,
	(
	    '$call'(A,CP,G0,M)
        ;
	    '$call'(B,CP,G0,M)
	).
'$spied_call'(\+ X,_,_,M) :- !,
	\+ '$execute'(M:X).
'$spied_call'(not X,_,_,M) :- !,
	\+ '$execute'(M:X).
'$spied_call'(!,CP,_,_) :-
	'$$cut_by'(CP).
'$spied_call'([A|B],_,_,M) :- !,
	'$csult'([A|B], M).
'$spied_call'(A, CP, G0, CurMod) :-
	(
	  % goal_expansion is defined, or
	  '$pred_goal_expansion_on'
	 ->
	  '$finish_spied_call'(A,CurMod)
        ;
          % this is a meta-predicate
	  '$flags'(A,CurMod,F,_), F /\ 0x200000 =:= 0x200000
	 ->
	  '$finish_spied_call'(A,CurMod)
	;
	  % finish_it_off (careful with co-routining)
	  '$std_spied_call'(A, CP, G0, CurMod)
	).

'$finish_spied_call'(A,CurMod) :-
	'$expand_goal'(A, CurMod, CurMod, NG, NMod),
	'$execute0'(NG, NMod).

'$std_spied_call'(A, CP, G0, M) :-
	( '$undefined'(A, M) ->
		functor(A,F,N),
		( recorded('$import','$import'(S,M,F,N),_) ->
		  '$spied_call'(S:A,CP,G0,M) ;
		  '$spy'(A)
	        )
	    ;
	       '$spy'(A)
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
	functor(G,F,N),
	recorded('$import','$import'(S,M,F,N),_),
	S \= M, % can't try importing from the module itself.
	!,
	'$expand_goal'(G, S, M, NG, NMod),
	'$execute0'(NG, NMod).
'$undefp'([M|G]) :-
	\+ '$undefined'(unknown_predicate_handler(_,_,_), user),
	user:unknown_predicate_handler(G,M,NG), !,
	'$execute'(M:NG).
'$undefp'([M|G]) :-
	recorded('$unknown','$unknown'(M:G,US),_), !,
	'$execute'(user:US).


/* This is the break predicate,
	it saves the importante data about current streams and
	debugger state */

break :- get_value('$break',BL), NBL is BL+1,
	get_value(spy_fs,SPY_FS),
	get_value(spy_sp,SPY_SP),
	get_value(spy_gn,SPY_GN),
	'$access_yap_flags'(10,SPY_CREEP),
	get_value(spy_cl,SPY_CL),
	get_value(spy_leap,_Leap),
	set_value('$break',NBL),
	current_output(OutStream), current_input(InpStream),
	'$format'(user_error, "[ Break (level ~w) ]~n", [NBL]),
	'$do_live',
	!,
	set_value('$live','$true'),
	get_value(spy_fs,SPY_FS),
	set_value(spy_sp,SPY_SP),
	set_value(spy_gn,SPY_GN),
	'$set_yap_flags'(10,SPY_CREEP),
	set_value(spy_cl,SPY_CL),
	set_value(spy_leap,_Leap),
	'$set_input'(InpStream), '$set_output'(OutStream),
	set_value('$break',BL).


'$csult'(V, _) :- var(V), !,
	'$do_error'(instantiation_error,consult(V)).
'$csult'([], _).
'$csult'([-F|L], M) :- !, '$reconsult'(M:F), '$csult'(L, M).
'$csult'([F|L], M) :- '$consult'(M:F), '$csult'(L, M).

'$consult'(V) :- var(V), !,
	'$do_error'(instantiation_error,consult(V)).
'$consult'([]) :- !.
'$consult'([F|Fs]) :- !,
	'$consult'(F),
	'$consult'(Fs).
'$consult'(M:X) :- atom(M), !,
        '$current_module'(M0),
        '$change_module'(M),
        '$consult'(X),
        '$change_module'(M0).
'$consult'(X) :-
	'$find_in_path'(X,Y,consult(X)),
	'$open'(Y,'$csult',Stream,0), !,
        '$consult'(X,Stream),
	'$close'(Stream).
'$consult'(X) :-
	'$do_error'(permission_error(input,stream,X),consult(X)).


'$consult'(_,Stream) :-
        '$record_loaded'(Stream),
	fail.
'$consult'(F,Stream) :-
	'$access_yap_flags'(8, 2), % SICStus Prolog compatibility
	!,
	'$reconsult'(F,Stream).
'$consult'(F,Stream) :-
	'$getcwd'(OldD),
	get_value('$consulting_file',OldF),
	'$set_consulting_file'(Stream),
	H0 is heapused, '$cputime'(T0,_),
	'$current_stream'(File,_,Stream),
	'$current_module'(OldModule),
	'$start_consult'(consult,File,LC),
	get_value('$consulting',Old),
	set_value('$consulting',true),
	recorda('$initialisation','$',_),
	( '$undefined'('$print_message'(_,_),prolog) -> 
	    ( get_value('$verbose',on) ->
		'$format'(user_error, "~*|[ consulting ~w... ]~n", [LC,F])
		; true )
	;
	    '$print_message'(informational, loading(consulting, File))
	),
	'$loop'(Stream,consult),
	'$end_consult',
	'$cd'(OldD),
	set_value('$consulting',Old),
	set_value('$consulting_file',OldF),
	( LC == 0 -> prompt(_,'   |: ') ; true),
	'$exec_initialisation_goals',
	'$current_module'(Mod,OldModule),
	H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
	( '$undefined'('$print_message'(_,_),prolog) -> 
	  ( get_value('$verbose',on) ->
	     '$format'(user_error, "~*|[ ~w consulted ~w bytes in ~d msecs ]~n", [LC,F,H,T])
	  ;
	     true
	  )
	;
	    '$print_message'(informational, loaded(consulted, File, Mod, T, H))
	),
	!.


'$record_loaded'(user).
'$record_loaded'(user_input).
'$record_loaded'(Stream) :-
	'$loaded'(Stream,_), !.
'$record_loaded'(Stream) :-
	'$file_name'(Stream,F),
	'$file_age'(F,Age),
	recorda('$loaded','$loaded'(F,Age),_).

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
			 user:'$LoopError'(Error)),
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

'$exists'(F,Mode) :- get_value(fileerrors,V), set_value(fileerrors,0),
	( '$open'(F,Mode,S,0), !, '$close'(S), set_value(fileerrors,V);
	  set_value(fileerrors,V), fail).


'$find_in_path'(user,user_input, _) :- !.
'$find_in_path'(user_input,user_input, _) :- !.
'$find_in_path'(S,NewFile, _) :-
	S =.. [Name,File], !,
	user:file_search_path(Name, Dir),
	'$dir_separator'(D),
	atom_codes(A,[D]),
	atom_concat([Dir,A,File],NFile),
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
	  user:term_expansion(Term,Expanded)
        ;
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

				%
% system_catch is like catch, but it avoids the overhead of a full
% meta-call by calling '$execute0' instead of $execute.
% This way it
% also avoids module preprocessing and goal_expansion
%
'$system_catch'(G, M, C, A) :-
	% check current trail
	'$catch'(C,A,_),
	'$execute0'(G, M).

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
	(C = Ball ->
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
	'$system_catch'(once(M:G), M, Error, user:'$LoopError'(Error)),
	fail.
'$exec_initialisation_goals'.


'$run_toplevel_hooks' :-
	get_value('$break',0),
	recorded('$toplevel_hooks',H,_), !,
	( '$execute'(H) -> true ; true).
'$run_toplevel_hooks'.


