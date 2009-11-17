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
		'$current_module'(Module),
		( Module==user ->
		    '$compile_mode'(_,0)
		;
		    format(user_error,'[~w]~n', [Module])
		),
		'$system_catch'('$enter_top_level',Module,Error,user:'$Error'(Error)).

'$init_system' :-
	'$change_alias_to_stream'('$loop_stream','$stream'(0)),
        % do catch as early as possible
	(
	 '$access_yap_flags'(15, 0),
	 '$access_yap_flags'(22, 0),
	 \+ '$uncaught_throw'
	->
	  '$version'
	;
	  true
	),
	(
	 '$access_yap_flags'(22, 0) ->
	 set_value('$verbose',on)
	;
	 set_value('$verbose',off)
	),
	(
	 retractall(user:library_directory(_)),
	 '$system_library_directories'(D),
	 assert(user:library_directory(D)),
	 fail
	;
	 true
	),
	'$stream_representation_error'(user_input, 512),
	'$stream_representation_error'(user_output, 512),
	'$stream_representation_error'(user_error, 512),
	'$enter_system_mode',
	set_value(fileerrors,1),
	'$init_consult',
	set_value('$gc',on),
	('$exit_undefp' -> true ; true),
	prompt('  ?- '),
	nb_setval('$break',0),
	% '$set_read_error_handler'(error), let the user do that
	nb_setval('$open_expands_filename',true),
	'$debug_on'(false),
	nb_setval('$trace',off),
	b_setval('$spy_glist',[]),
	% simple trick to find out if this is we are booting from Prolog.
	get_value('$user_module',V),
	(
	  V == []
	->
	  '$current_module'(_,prolog)
	  ;
	  '$current_module'(_,V), '$compile_mode'(_,0),
	  ('$access_yap_flags'(16,0) ->
	      ( exists('~/.yaprc') -> load_files('~/.yaprc', []) ; true ),
	      ( exists('~/.prologrc') -> load_files('~/.prologrc', []) ; true ),
	      ( exists('~/prolog.ini') -> load_files('~/prolog.ini', []) ; true )
	  ;
	      true
	  )
	),
	'$db_clean_queues'(0),
	'$startup_reconsult',
	'$startup_goals',
	'$set_input'(user_input),'$set_output'(user),
	'$run_at_thread_start'.

'$init_consult' :-
	nb_setval('$lf_verbose',informational),
	nb_setval('$if_level',0),
	nb_setval('$endif',off),
	nb_setval('$consulting_file',[]),
	nb_setval('$initialization_goals',off),
	nb_setval('$consulting',false),
	nb_setval('$included_file',[]).
	

% Start file for yap

/*		I/O predicates						*/

/* meaning of flags for '$write' is
	  1	quote illegal atoms
	  2	ignore operator declarations
	  4	output '$VAR'(N) terms as A, B, C, ...
	  8	use portray(_)
*/

/* main execution loop							*/
'$read_vars'(Stream,T,Mod,Pos,V) :-
	'$read'(true,T,Mod,V,Pos,Err,Stream),
	(nonvar(Err) ->
	 print_message(error,Err), fail
	;
	 true
	).

% reset alarms when entering top-level.
'$enter_top_level' :-
	'$alarm'(0, 0, _, _),
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
	nb_getval('$break',BreakLevel),
	 '$debug_on'(DBON),
	(
	 nb_getval('$trace',on)
	->
	 TraceDebug = trace
	;
	 DBON == true
	->
	 TraceDebug = debug
	;
	 true
	),
	print_message(informational,prompt(BreakLevel,TraceDebug)),
	fail.
'$enter_top_level' :-
	get_value('$top_level_goal',GA), GA \= [], !,
	set_value('$top_level_goal',[]),
	'$run_atom_goal'(GA),
	set_value('$live','$false').
'$enter_top_level' :-
	'$disable_docreep',
	prompt(_,'   ?- '),
	prompt('   | '),
	'$run_toplevel_hooks',
	'$read_vars'(user_input,Command,_,Pos,Varnames),
	nb_setval('$spy_gn',1),
		% stop at spy-points if debugging is on.
	nb_setval('$debug_run',off),
	nb_setval('$debug_zip',off),
	prompt(_,'   |: '),
	'$command'((?-Command),Varnames,Pos,top),
	'$sync_mmapped_arrays',
	set_value('$live','$false').

'$startup_goals' :-
	get_value('$extend_file_search_path',P), P \= [],
	set_value('$extend_file_search_path',[]),
	'$extend_file_search_path'(P),
	fail.
'$startup_goals' :-
	recorded('$startup_goal',G,_),
	'$current_module'(Module),
	'$system_catch'('$query'(once(G), []),Module,Error,user:'$Error'(Error)),
	fail.
'$startup_goals' :-
	get_value('$init_goal',GA),
	GA \= [],
	set_value('$init_goal',[]),
	'$run_atom_goal'(GA),
	fail.
'$startup_goals' :-
	get_value('$myddas_goal',GA), GA \= [],
	set_value('$myddas_goal',[]),
	get_value('$myddas_user',User), User \= [],
	set_value('$myddas_user',[]),
	get_value('$myddas_db',Db), Db \= [],
	set_value('$myddas_db',[]),
	get_value('$myddas_host',HostT),
	( HostT \= [] ->
	  Host = HostT,
	  set_value('$myddas_host',[])
	;
	  Host = localhost
	),
	get_value('$myddas_pass',PassT),
	( PassT \= [] ->
	  Pass = PassT,
	  set_value('$myddas_pass',[])
	;
	  Pass = ''
	),
	use_module(library(myddas)),
	call(db_open(mysql,myddas,Host/Db,User,Pass)),
	'$myddas_import_all',
	fail.
'$startup_goals'.

'$startup_reconsult' :-
	get_value('$consult_on_boot',X), X \= [], !,
	set_value('$consult_on_boot',[]),
	'$do_startup_reconsult'(X).
'$startup_reconsult'.

 %
 % MYDDAS: Import all the tables from one database
 %

 '$myddas_import_all':-
	 call(db_my_show_tables(myddas,table(Table))),
	 call(db_import(myddas,Table,Table)),
	 fail.
 '$myddas_import_all'.
	 


 '$erase_sets' :- 
		 eraseall('$'),
		 eraseall('$$set'),
		 eraseall('$$one'), 
		 eraseall('$reconsulted'), fail.
 '$erase_sets' :- \+ recorded('$path',_,_), recorda('$path',"",_).
 '$erase_sets'.

 '$version' :- 
	 get_value('$version_name',VersionName),
	 print_message(help, version(VersionName)),
	 get_value('$myddas_version_name',MYDDASVersionName),
	 MYDDASVersionName \== [],
	 print_message(help, myddas_version(MYDDASVersionName)),
	 fail.
 '$version' :-
	 recorded('$version',VersionName,_),
	 print_message(help, VersionName),
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

'$start_corouts' :-
	recorded('$corout','$corout'(Name,_,_),R),
	Name \= main,
	finish_corout(R),
	fail.
'$start_corouts' :- 
	eraseall('$corout'),
	eraseall('$result'),
	eraseall('$actual'),
	fail.
'$start_corouts' :- recorda('$actual',main,_),
	recordz('$corout','$corout'(main,main,'$corout'([],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[])),_Ref),
	recorda('$result',going,_).

'$command'(C,VL,Pos,Con) :-
	'$access_yap_flags'(9,1), !,
	 '$execute_command'(C,VL,Pos,Con,C).
'$command'(C,VL,Pos,Con) :-
	( (Con = top ; var(C) ; C = [_|_])  ->  
	  '$execute_command'(C,VL,Pos,Con,C), ! ;
	  % do term expansion
	  expand_term(C, EC),
	  % execute a list of commands
	  '$execute_commands'(EC,VL,Pos,Con,C),
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
	   '$execute_command'(C,VL,Pos,Con,Source),
	   fail	
	 ;
	   '$execute_commands'(Cs,VL,Pos,Con,Source)
	 ).
 '$execute_commands'(C,VL,Pos,Con,Source) :-
	 '$execute_command'(C,VL,Pos,Con,Source).



				%
 %
 %

 '$execute_command'(C,_,_,top,Source) :- var(C), !,
	 '$do_error'(instantiation_error,meta_call(Source)).
 '$execute_command'(C,_,_,top,Source) :- number(C), !,
	 '$do_error'(type_error(callable,C),meta_call(Source)).
 '$execute_command'(R,_,_,top,Source) :- db_reference(R), !,
	 '$do_error'(type_error(callable,R),meta_call(Source)).
 '$execute_command'(end_of_file,_,_,_,_) :- !.
 '$execute_command'(Command,_,_,_,_) :-
	 nb_getval('$if_skip_mode',skip),
	 \+ '$if_directive'(Command),
	 !.
 '$execute_command'((:-G),_,_,Option,_) :- !,
	 '$current_module'(M),
	 % allow user expansion
	 expand_term((:- G), O),
         O = (:- G1),
	 '$process_directive'(G1, Option, M).
 '$execute_command'((?-G),V,Pos,_,Source) :- !,
	 '$execute_command'(G,V,Pos,top,Source).
 '$execute_command'(G,V,Pos,Option,Source) :-
	 '$continue_with_command'(Option,V,Pos,G,Source).

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
	 ( '$notrace'(M:G) -> true ; format(user_error,':- ~w:~w failed.~n',[M,G]) ).

 '$continue_with_command'(reconsult,V,Pos,G,Source) :-
	 '$go_compile_clause'(G,V,Pos,5,Source),
	 fail.
 '$continue_with_command'(consult,V,Pos,G,Source) :-
	 '$go_compile_clause'(G,V,Pos,13,Source),
	 fail.
 '$continue_with_command'(top,V,_,G,_) :-
	 '$query'(G,V).

 %
 % not 100% compatible with SICStus Prolog, as SICStus Prolog would put
 % module prefixes all over the place, although unnecessarily so.
 %
 '$go_compile_clause'(G,V,Pos,N,Source) :-
	 '$current_module'(Mod),
	 '$go_compile_clause'(G,V,Pos,N,Mod,Mod,Source).
 
'$go_compile_clause'(M:G,V,Pos,N,_,_,Source) :- !,
	  '$go_compile_clause'(G,V,Pos,N,M,M,Source).
'$go_compile_clause'((M:H :- B),V,Pos,N,_,BodyMod,Source) :- !,
	  '$go_compile_clause'((H :- B),V,Pos,N,M,BodyMod,Source).
'$go_compile_clause'(G,V,Pos,N,HeadMod,BodyMod,Source) :- !,
	 '$prepare_term'(G, V, Pos, G0, G1, BodyMod, HeadMod, Source),
	 '$$compile'(G1, G0, N, HeadMod).

 '$prepare_term'(G, V, Pos, G0, G1, BodyMod, SourceMod, Source) :-
	 ( get_value('$syntaxcheckflag',on) ->
		 '$check_term'(Source, V, Pos, BodyMod) ; true ),
	 '$precompile_term'(G, G0, G1, BodyMod, SourceMod).

 % process an input clause
 '$$compile'(G, G0, L, Mod) :-
	 '$head_and_body'(G,H,_),
	 '$flags'(H, Mod, Fl, Fl),
	 is(NFl, /\, Fl, 0x00002000),
	 (
	  NFl \= 0
	 ->
	  '$assertz_dynamic'(L,G,G0,Mod)
	 ;
	  nb_getval('$assert_all',on)
	 ->
	  functor(H,N,A),
	  '$dynamic'(N/A,Mod),
	  '$assertz_dynamic'(L,G,G0,Mod)
	 ;
	  '$not_imported'(H, Mod),
	  '$compile'(G, L, G0, Mod)
	 ).

'$not_imported'(H, Mod) :-
	recorded('$import','$import'(NM,Mod,NH,H,_,_),_),
	NM \= Mod, !,
	functor(NH,N,Ar),
	'$do_error'(permission_error(modify, static_procedure, NM:N/Ar), consult).
'$not_imported'(_, _).


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
	   '$exit_system_mode',
	   '$execute'(G),
	   ( '$enter_system_mode' ; '$exit_system_mode', fail),
	   '$output_frozen'(G, V, LGs),
	   '$write_answer'(V, LGs, Written),
	   '$write_query_answer_true'(Written),
	   '$another',
	   !, fail
	 ;
	   '$enter_system_mode',
           '$out_neg_answer'
	 ).

 '$yes_no'(G,C) :-
	 '$current_module'(M),
	 '$do_yes_no'(G,M),
	 '$output_frozen'(G, [], LGs),
	 '$write_answer'([], LGs, Written),
	 ( Written = [] ->
	 !,'$present_answer'(C, yes);
	 '$another', !
	 ),
	 fail.
 '$yes_no'(_,_) :-
	 '$enter_system_mode',
	 '$out_neg_answer'.

'$add_env_and_fail' :- fail.

'$out_neg_answer' :-
	 ( '$undefined'(print_message(_,_),prolog) -> 
	    '$present_answer'(user_error,"no~n", [])
	 ;
	    print_message(help,no)
	 ),
	 fail.

'$do_yes_no'([X|L], M) :- !, '$csult'([X|L], M).
'$do_yes_no'(G, M) :-
	'$exit_system_mode',
	'$execute'(M:G),
	( '$enter_system_mode' ; '$exit_system_mode', fail).

'$write_query_answer_true'([]) :- !,
	format(user_error,'~ntrue',[]).
'$write_query_answer_true'(_).

'$output_frozen'(_,V,LGs) :-
	\+ '$undefined'(bindings_message(_,_,_), swi),
	swi:bindings_message(V, LGs, []), !.
'$output_frozen'(G,V,LGs) :-
	'$project_and_delayed_goals'(G,LGs).

%
% present_answer has three components. First it flushes the streams,
% then it presents the goals, and last it shows any goals frozen on
% the arguments.
%
'$present_answer'(_,_):-
        '$flush_all_streams',
	fail.
'$present_answer'((?-), Answ) :-
	nb_getval('$break',BL),
	( BL \= 0 -> 	format(user_error, '[~p] ',[BL]) ;
			true ),
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,Answ,Opts) ;
	   format(user_error,'~w',[Answ])
        ),
	format(user_error,'~n', []).

'$another' :-
	format(user_error,' ? ',[]),
	get0(user_input,C),
	(   C== 0'; ->  '$skip'(user_input,10), %'
	    '$add_nl_outside_console',
	    fail
	;
	    C== 10 -> '$add_nl_outside_console',
		( '$undefined'(print_message(_,_),prolog) -> 
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
'$write_answer'(Vs, LBlk, FLAnsw) :-
	'$purge_dontcares'(Vs,IVs),
	'$sort'(IVs, NVs),
	'$prep_answer_var_by_var'(NVs, LAnsw, LBlk),
	'$name_vars_in_goals'(LAnsw, Vs, NLAnsw),
        '$write_vars_and_goals'(NLAnsw, first, FLAnsw).

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

'$write_vars_and_goals'([], _, []).
'$write_vars_and_goals'([nl,G1|LG], First, NG) :- !,
	nl(user_error),
	'$write_goal_output'(G1, First, NG, Next, IG),
	'$write_vars_and_goals'(LG, Next, IG).
'$write_vars_and_goals'([G1|LG], First, NG) :-
	'$write_goal_output'(G1, First, NG, Next, IG),
	'$write_vars_and_goals'(LG, Next, IG).

'$goal_to_string'(Format, G, String) :-			  
	charsio:open_mem_write_stream(W),
        format(W,Format,G),
	charsio:peek_mem_write_stream(W, [], String),
	close(W).

'$write_goal_output'(var([V|VL]), First, [var([V|VL])|L], next, L) :- !,
        ( First = first -> true ; format(user_error,',~n',[]) ),
	format(user_error,'~s',[V]),
	'$write_output_vars'(VL).
'$write_goal_output'(nonvar([V|VL],B), First, [nonvar([V|VL],B)|L], next, L) :- !,
        ( First = first -> true ; format(user_error,',~n',[]) ),
	format(user_error,'~s',[V]),
	'$write_output_vars'(VL),
	format(user_error,' = ', []),
        ( recorded('$print_options','$toplevel'(Opts),_) ->
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
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,G,Opts) ;
	   format(user_error,'~w',[G])
        ).
'$write_goal_output'(_M:G, First, [G|NG], next, NG) :- !,
        ( First = first -> true ; format(user_error,',~n',[]) ),
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,G,Opts) ;
	   format(user_error,'~w',[G])
        ).
'$write_goal_output'(G, First, [M:G|NG], next, NG) :-
	'$current_module'(M),
        ( First = first -> true ; format(user_error,',~n',[]) ),
        ( recorded('$print_options','$toplevel'(Opts),_) ->
	   write_term(user_error,G,Opts) ;
	   format(user_error,'~w',[G])
        ).

'$name_vars_in_goals'(G, VL0, NG) :-
	copy_term_nat(G+VL0, NG+NVL0),
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
	yap_hacks:current_choice_point(CP),
	'$call'(G, CP, G, M).


','(X,Y) :-
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        '$call'(X,CP,(X,Y),M),
        '$call'(Y,CP,(X,Y),M).
';'((X->A),Y) :- !,
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        ( '$execute'(X)
	->
	  '$call'(A,CP,(X->A;Y),M)
	;
	  '$call'(Y,CP,(X->A;Y),M)
	).
';'((X*->A),Y) :- !,
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
	(
	 yap_hacks:current_choicepoint(DCP),
	 '$execute'(X),
	 yap_hacks:cut_at(DCP),
	 '$call'(A,CP,((X*->A),Y),M)
        ;
	 '$call'(Y,CP,((X*->A),Y),M)
	).
';'(X,Y) :-
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        ( '$call'(X,CP,(X;Y),M) ; '$call'(Y,CP,(X;Y),M) ).
'|'(X,Y) :-
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        ( '$call'(X,CP,(X|Y),M) ; '$call'(Y,CP,(X|Y),M) ).
'->'(X,Y) :-
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        ( '$call'(X,CP,(X->Y),M) -> '$call'(Y,CP,(X->Y),M) ).
'*->'(X,Y) :-
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        ( '$call'(X,CP,(X*->Y),M), '$call'(Y,CP,(X*->Y),M) ).
\+(G) :-     \+ '$execute'(G).
not(G) :-    \+ '$execute'(G).

'$cut_by'(CP) :- '$$cut_by'(CP).

%
% do it in ISO mode.
%
'$meta_call'(G,_ISO,M) :-
	'$iso_check_goal'(G,G),
	yap_hacks:current_choice_point(CP),
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
	 yap_hacks:current_choicepoint(DCP),
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
	 yap_hacks:current_choicepoint(DCP),
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
'$call'(\+ X, _CP, _G0, M) :- !,
	\+  '$call'(X,CP,G0,M).
'$call'(not(X), _CP, _G0, M) :- !,
	\+  '$call'(X,CP,G0,M).
'$call'(!, CP, _,_) :- !,
	'$$cut_by'(CP).
'$call'([A|B], _, _, M) :- !,
	'$csult'([A|B], M).
'$call'(G, CP, G0, CurMod) :-
	( '$is_expand_goal_or_meta_predicate'(G,CurMod) ->
	   (
	     '$notrace'(user:goal_expansion(G, CurMod, NG)) ->
	       '$call'(NG, CP, G0,CurMod)
	     ;
	       % repeat other code.
             '$is_metapredicate'(G,CurMod) ->
	       (
	         '$meta_expansion'(G,CurMod,CurMod,CurMod,NG,[]) ->
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

% Called by the abstract machine, if no clauses exist for a predicate
'$undefp'([M|G]) :-
	% make sure we do not loop on undefined predicates
        % for undefined_predicates.
	(
	 recorded('$import','$import'(NM,M,Goal,G,_,_),_)
	->
	 true
	;
	 '$enter_undefp',
	 (
	  swi:swi_predicate_table(M,G,NM,Goal)
	 ->
	  '$exit_undefp'
	 ;
	  once('$find_undefp_handler'(G,M,Goal,NM))
	 )
	),
	!,
	Goal \= fail,
	'$complete_goal'(M, Goal, NM, G).

'$complete_goal'(M, G, CurMod, G0) :-
	  (
	   '$is_metapredicate'(G,CurMod)
	  ->
	   '$meta_expansion'(G, CurMod, M, M, NG,[]) ->
	   '$execute0'(NG, CurMod)
	  ;
	   '$execute0'(G, CurMod)
	  ).

'$find_undefp_handler'(G,M,NG,user) :-
	\+ '$undefined'(unknown_predicate_handler(_,_,_), user),
	'$system_catch'(unknown_predicate_handler(G,M,NG), user, Error, '$leave_undefp'(Error)), !,
	'$exit_undefp'.
'$find_undefp_handler'(G,M,US,user) :-
	recorded('$unknown','$unknown'(M:G,US),_), !,
	'$exit_undefp'.
'$find_undefp_handler'(_,_,_,_) :-
	'$exit_undefp',
	fail.

'$leave_undefp'(Ball) :-
	'$exit_undefp',
	throw(Ball).


/* This is the break predicate,
	it saves the importante data about current streams and
	debugger state */

break :-
	nb_getval('$system_mode',SystemMode),
	nb_getval('$trace',Trace),
	nb_setval('$trace',off),
	'$debug_on'(Debug),
	'$debug_on'(false),
	nb_getval('$break',BL), NBL is BL+1,
	nb_getval('$spy_gn',SPY_GN),
	b_getval('$spy_glist',GList),
	b_setval('$spy_glist',[]),
	nb_setval('$break',NBL),
	current_output(OutStream), current_input(InpStream),
	format(user_error, '% Break (level ~w)~n', [NBL]),
	'$do_live',
	!,
	set_value('$live','$true'),
	b_setval('$spy_glist',GList),
	nb_setval('$spy_gn',SPY_GN),
	'$set_input'(InpStream), '$set_output'(OutStream),
	'$debug_on'(Debug),
	nb_setval('$trace',Trace),
	nb_setval('$break',BL),
	nb_setval('$system_mode',SystemMode).

'$silent_bootstrap'(F) :-
	'$init_consult',
	nb_setval('$if_level',0),
	nb_getval('$lf_verbose',OldSilent),
	nb_setval('$lf_verbose',silent),
	bootstrap(F),
	nb_setval('$lf_verbose', OldSilent).

bootstrap(F) :-
	'$open'(F,'$csult',Stream,0,0),
	'$current_stream'(File,_,Stream),
	'$start_consult'(consult, File, LC),
	file_directory_name(File, Dir),
	getcwd(OldD),
	cd(Dir),
	(
	  nb_getval('$lf_verbose',silent)
	->
	  true
	;
	  H0 is heapused, '$cputime'(T0,_),
	  format(user_error, '~*|% consulting ~w...~n', [LC,F])
	),
	'$loop'(Stream,consult),
	cd(OldD),
	'$end_consult',
	(
	  nb_getval('$lf_verbose',silent)
	->
	  true
	;
	  H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
	  format(user_error, '~*|% ~w consulted ~w bytes in ~d msecs~n', [LC,F,H,T])
	),
	!,
	'$close'(Stream).



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
	'$read_vars'(Stream,Command,_,Pos,Vars),
	'$command'(Command,Vars,Pos,Status).

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

access_file(F,Mode) :-
	'$exists'(F,Mode).

'$exists'(_,none) :- !.
'$exists'(F,exist) :- !,
	'$access'(F).
'$exists'(F,Mode) :-
	get_value(fileerrors,V),
	set_value(fileerrors,0),
	system:true_file_name(F, F1),
	(
	 '$open'(F1,Mode,S,0,1)
	->
	 '$close'(S),
	 set_value(fileerrors,V)
	;
	 set_value(fileerrors,V),
	 fail
	).


% term expansion
%
% return two arguments: Expanded0 is the term after "USER" expansion.
%                       Expanded is the final expanded term.
%
'$precompile_term'(Term, Expanded0, Expanded, BodyMod, SourceMod) :-
	'$module_expansion'(Term, Expanded0, ExpandedI, BodyMod, SourceMod), !,
	(
	 '$access_yap_flags'(9,1)      /* strict_iso on */
        ->
	 Expanded = ExpandedI,
	 '$check_iso_strict_clause'(Expanded0)
        ;
	 '$expand_array_accesses_in_term'(ExpandedI,Expanded)
	).
'$precompile_term'(Term, Term, Term, _, _).
	

expand_term(Term,Expanded) :-
	( \+ '$undefined'(term_expansion(_,_), user),
	  '$notrace'(user:term_expansion(Term,Expanded))
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   catch/throw implementation

% at each catch point I need to know:
% what is ball;
% where was the previous catch	
catch(G, C, A) :-
	'$catch'(C,A,_),
	yap_hacks:current_choice_point(CP0),
	'$execute'(G),
	yap_hacks:current_choice_point(CP1),
	(CP0 == CP1 -> !; true ).

% makes sure we have an environment.
'$true'.


% system_catch is like catch, but it avoids the overhead of a full
% meta-call by calling '$execute0' instead of $execute.
% This way it
% also avoids module preprocessing and goal_expansion
%
'$system_catch'(G, M, C, A) :-
	% check current trail
	'$catch'(C,A,_),
	yap_hacks:current_choice_point(CP0),
	'$execute_nonstop'(G, M),
	yap_hacks:current_choice_point(CP1),
	(CP0 == CP1 -> !; true ).

%
% throw has to be *exactly* after system catch!
%
throw(Ball) :-
	% get current jump point
	'$jump_env_and_store_ball'(Ball).


% just create a choice-point
'$catch'(_,_,_).
'$catch'(_,_,_) :- fail.

'$handle_throw'(_, _, _).
'$handle_throw'(C, A, Ball) :-
        % reset info
	('catch_ball'(Ball, C) ->
	    '$execute'(A)
	    ;
	    throw(Ball)
	).

'catch_ball'('$abort', _) :- !, fail.
% system defined throws should be ignored by used, unless the
% user is hacking away.
'catch_ball'(Ball, V) :-
	var(V),
	nonvar(Ball),
	Ball = error(Type,_), % internal error ??
	functor(Type, Name, _),
	atom_codes(Name, [0'$|_]), %'0
	!, fail.
'catch_ball'(C, C).

'$run_toplevel_hooks' :-
	nb_getval('$break',0),
	recorded('$toplevel_hooks',H,_), !,
	( '$oncenotrace'(H) -> true ; true).
'$run_toplevel_hooks'.

'$enter_system_mode' :-
	nb_setval('$system_mode',on).

'$exit_system_mode' :-
	nb_setval('$system_mode',off),
	( nb_getval('$trace',on) -> '$creep' ; true).

%
% just prevent creeping from going on...
%
'$notrace'(G) :-
	'$disable_creep', !,
	(
		% creep was going on...
	 yap_hacks:current_choice_point(CP0),
	 '$execute'(G),
	 yap_hacks:current_choice_point(CP1),
	 ( CP0 == CP1 ->
	   !,
	   '$creep'
	 ;
	   (
	    '$creep'
	   ;
	    '$disable_docreep',
	    fail
	   )
	 )
	;
	 '$creep',
	 fail
	).
'$notrace'(G) :-
	'$execute'(G).

'$oncenotrace'(M:G) :-
	'$disable_creep', !,
	(
	 '$execute'(G)
	->
	 '$creep'
	;
	 '$creep',
	 fail
	).	
'$oncenotrace'(G) :-
	'$execute'(G), !.


'$run_at_thread_start' :-
	recorded('$thread_initialization',M:D,_),
	'$notrace'(M:D),
	fail.
'$run_at_thread_start'.


