/**
  * @file   top.yap
  * @author VITOR SANTOS COSTA <vsc@vcosta-laptop.dcc.fc.up.pt>
  * @date   Sat Apr  7 03:14:17 2018
  *
  * @brief  top-level implementation puls system booting.
  *
  *   @addtogroup TopLevel Top-Level and Boot Predicates
  *   @ingroup YAPControl
  *
  * [TOC]
  *
  *    @{
  *
  */
:- '$system_meta_predicates'([gated_call(0, 0, ?, 0), catch(0, ?, 0), log_event(+, :)]).

% @pred live
%
% start a Prolog engine.
live :-
    repeat,
    yap_flag(verbose, normal),
    current_source_module(Module, Module),
    (   Module==user
    ->  true % '$compile_mode'(_,0)
    ;   format(user_error, '[~w]~n', [Module])
    ),
    '$system_catch'('$enter_top_level',
                    Module,
                    Error,
                    '$Error'(Error)).

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

                    [ variable_names(Bindings),
                      syntax_errors(dec10),
                      term_position(Pos),
		      input_closing_blank(true)
                    ]),
          E,
          '$handle_toplevel_error'(E)).

'$handle_toplevel_error'(syntax_error(_)) :-
    !,
    fail.
'$handle_toplevel_error'(error(io_error(read, user_input), _)) :-
    !.
'$handle_toplevel_error'(_, E) :-
    throw(E).


% reset alarms when entering top-level.
'$enter_top_level' :-
    '$alarm'(0, 0, _, _),
    fail.
'$enter_top_level' :-
    '$clean_up_dead_clauses',
    fail.
'$enter_top_level' :-
    get_value('$top_level_goal', GA),
    GA\=[],
    !,
    set_value('$top_level_goal', []),
    '$run_atom_goal'(GA),
    fail.
'$enter_top_level' :-
    flush_output,
    '$run_toplevel_hooks',
    prompt1(' ?- '),
    '$read_toplevel'(Command, Varnames, Pos),
    ( '$pred_exists'('$init_debugger',prolog) -> '$init_debugger';true),
   '$command'(Command, user, Varnames, Pos, top),
    current_prolog_flag(break_level, BreakLevel),
    (   BreakLevel\=0
    ->  true
    ;   '$pred_exists'(halt(_), user)
    ->  halt(0)
    ;   '$halt'(0)
    ).


'$erase_sets' :-
    eraseall($),
    eraseall('$$set'),
    eraseall('$$one'),
    eraseall('$reconsulted'),
    fail.
'$erase_sets' :-
    \+ recorded('$path', _, _),
    recorda('$path', [], _).
'$erase_sets'.

'$start_corouts' :-
	eraseall('$corout'),
	eraseall('$result'),
	eraseall('$actual'),
	fail.
'$start_corouts' :- recorda('$actual',main,_),
	recordz('$corout','$corout'(main,main,'$corout'([],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[])),_Ref),
	recorda('$result',going,_).

 %
 % Hack in case expand_term has created a list of commands.
 %
'$execute_commands'(V,_,_,_,_,Source) :- var(V), !,
	 '$do_error'(instantiation_error,meta_call(Source)).
'$execute_commands'([],_,_,_,_,[]) :- !.
'$execute_commands'([C|Cs],M,VL,Pos,Con,[Source|Ss]) :-
    !,
    (
	'$system_catch'('$execute_command'(C,M,VL,Pos,Con,Source),prolog,Error,'$LoopError'(Error, Con)),
	fail
    ;
    '$execute_commands'(Cs,M,VL,Pos,Con,Ss)
    ).
 '$execute_commands'(C,M,VL,Pos,Con,Source) :-
	 '$execute_command'(C,M,VL,Pos,Con,Source).

				%
 %
 %

'$execute_command'(C,_,_,_,_,_Source) :-
    must_be_callable(C),
    fail.
 '$execute_command'(end_of_file,_,_,_,_,_) :- !.
 '$execute_command'(Command,_,_,_,_,_) :-
	 '__NB_getval__'('$if_skip_mode', skip, fail),
	 \+ '$if_directive'(Command),
	 !.
'$execute_command'((:-G),M,VL,Pos,Option,Source) :-
    Option \= top,
    !,			% allow user expansion
    '$expand_term'((:- M:G), O),
    '$yap_strip_module'(O, NM, NO),
    (
            NO = (:- G1)
        ->
	      '$process_directive'(G1, Option, NM, VL, Pos)
     ;
           '$execute_commands'(	NO,NM,VL,Pos,Option,Source)
        ).
'$execute_command'((?-G), M, VL, Pos, Option, Source) :-
	 Option \= top,
	 !,
	 '$execute_command'(G, M, VL, Pos, top, Source).
 '$execute_command'(G, M, VL, Pos, Option, Source) :-
	 '$continue_with_command'(Option, VL, Pos, M:G, Source).

'$expand_term'(T,O) :-
	 '$expand_term'(T,top,O).

'$expand_term'(T,Con,O) :-
	catch( '$expand_term0'(T,Con,O), _,( '$reenter_debugger'(exit), fail) ),
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
        expand_goal(T, O).

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
    '$head_and_body'( C, H, B ),
    '$yap_strip_module'(H,Mod,H0),
   (
     '$undefined'(H0, Mod)
    ->
     '$init_pred'(H0, Mod, Where)
   ;
     true
    ),
%    writeln(Mod:((H:-B))),
    '$compile'((H0:-B), Where, C0, Mod, R).

'$init_pred'(H, Mod, _Where ) :-
    recorded('$import','$import'(NM,Mod,NH,H,_,_),RI),
%    NM \= Mod,
    functor(NH,N,Ar),
    print_message(warning,redefine_imported(Mod,NM,Mod:N/Ar)),
    erase(RI),
    clause(Mod:H,_,R), erase(R),
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

'$query'(end_of_file,[]).
'$query'(G,[]) :-
	 current_prolog_flag(prompt_alternatives_on, OPT),
	 ( OPT = groundness ; OPT = determinism),
	 !,
	 '$yes_no'(G,(?-)).
'$query'(G0,V) :-
    (
	'$current_choice_point'(CP),
	query(G0,V,_,_),
	'$current_choice_point'(NCP),
	(
	    yap_flag(prompt_alternatives_on,determinism),
	    CP == NCP
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

query(G0, Vs, NVs,NLGs) :-
    '$yap_strip_module'(G0,M,G),
    '$user_call'(G, M),
    %start_low_level_trace,`
   copy_term(G+Vs, _IG+IVs, LGs),
   rational_term_to_forest(IVs+LGs,NVs+ILGs,Extra,[]),
 lists:append(Extra,ILGs,NLGs),	 
  '$write_answer'(NVs, NLGs, Written),
    '$write_query_answer_true'(Written).

'$yes_no'(G,C) :-
    '$current_module'(M),
    '$do_yes_no'(G,M),
    '$delayed_goals'(G, [], NV, LGs, _),
    '$write_answer'(NV, LGs, Written),
    (
	Written = []
    ->
    !,
    '$present_answer'(C, true)
    ;
    '$another', !
    ),
    fail.
'$yes_no'(_,_) :-
    '$out_neg_answer'.

'$add_env_and_fail' :- fail.


'$process_answer'(Vs, LGs, Bindings) :-
    %'$purge_dontcares'(Vs,IVs),
    '$sort'(Vs, NVs),
    '$prep_answer_var_by_var'(NVs, LAnsw, LGs),
    '$name_vars_in_goals'(LAnsw, Vs, Bindings).

%
% *-> at this point would require compiler support, which does not exist.
%
'$delayed_goals'(G, V, NV, LGs, NCP) :-
        (
         '$$save_by'(NCP1),
         attributes:delayed_goals(G, V, NV, LGs),
         '$clean_ifcp'(NCP1),
         '$$save_by'(NCP2),
         NCP is NCP2-NCP1
          ;
           LGs = [],
	   V = NV,
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
        current_prolog_flag(toplevel_print_options, Opts),
	write_term(user_error,Answ,Opts).

'$another' :-
	'$clear_input'(user_input),
	prompt1(' ? '),
	get_code(user_input,C),
	'$do_another'(C).

'$do_another'(C) :-
    (   C=:= ";" ->
         skip(user_input,10),
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

%% @pred write_query_answer( +Bindings )
%
% YAP uses this routine to  output the answer to a query.
% _Bindings_ are
% - unifications
% - suspended or floundered goals, representing constraints.
%
write_query_answer( Bindings ) :-
   '$write_vars_and_goals'(Bindings, first, _FLAnsw).

'$purge_dontcares'([],[]).
'$purge_dontcares'([Name=_|Vs],NVs) :-
    atom_codes(Name, [C|_]),
    C is "_",
    !,
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
	format(string(String),Format,G),
	( String == `` ->
	    % we didn't
	    IG = NG, First = Next
	;
	    % we did
	    format(user_error, '~N~s', [String]),
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
'$write_goal_output'(G0, First, [M:G|NG], next, NG) :-
	'$yap_strip_module'(G0,M,G),
        ( First = first -> true ; format(user_error,',~n',[]) ),
        (  yap_flag(toplevel_print_options, Opts) ->
	   write_term(user_error,G,Opts) ;
	   format(user_error,'~w',[G])
        ).

'$name_vars_in_goals'(G, VL0, G) :-
	'$name_well_known_vars'(VL0),
	numbervars(G,-1,_).

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
    '$yap_strip_module'(M:G, M1, G1),
	'$current_choice_point'(CP),
	'$call'(G1, CP, G, M1).

'$user_call'(G, M) :-
    '$yap_strip_module'(M:G, M1,G1),
    '$dotrace'(G1, M1, _),
    !,
    '$trace'(M1:G1, true).
'$user_call'(G, M) :-
    gated_call(
	true,
	%		'$trace_port'([call], GoalNumber, G, M, CP,  H)
	M:G,
	Port,
 	'$cross_run_deb'(Port,true, _)
    ).

'$cut_by'(CP) :- '$$cut_by'(CP).

%
% do it in ISO mode.
%
'$meta_call'(G,_ISO,M) :-
	'$iso_check_goal'(G,G),
	'$current_choice_point'(CP),
	'$call'(G, CP, G, M).

'$meta_call'(G, CP, G0, M) :-
%	expand_goal(M:G, NG),
%	must_be_callable(NG),
	'$yap_strip_module'(M:G,NM,NC),
        '$call'(NC,CP,G0,NM).
/*
'$call'(G, CP, G0, _, M) :-  
	'$iso_check_goal'(G,G0),
	'$call'(G, CP, G0, M).
*/
'$call'(X,_CP,_G0,_M) :-
must_be_callable(X),
fail.
'$call'((X,Y),CP,G0,M) :- !,			
        '$call'(X,CP,G0,M),
        '$call'(Y,CP,G0,M).
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
	 '$current_choice_point'(DCP),
	 '$call'(X,DCP,G0,M),
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
	 '$call'(X,DCP,G0,M),
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
	 '$call'(X,DCP,G0,M),
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
'$call'(!, CP, _G0, _m) :- !,
	'$$cut_by'(CP).
'$call'(forall(X,Y), CP, _G0, M) :- !,
	\+ ('$call'(X, CP, G0, M),
	     \+ '$call'(Y, CP, G0, M) ).
'$call'(once(X), _CP, G0, M) :- !,
	'$current_choice_point'(CP),
	( '$call'(X, CP, G0, M) -> true).
'$call'(ignore(X), _CP, G0, M) :-
	!,
	'$current_choice_point'(CP),  
	( '$call'(X, CP, G0, M) -> true;true).
'$call'(!, CP, _G0, _m) :- !,
	'$$cut_by'(CP).
'$call'([X|Y], _, _, M) :-
    (Y == [] ->
    consult(M:X)
    ;
 	 '$csult'([X|Y] ,M)
 	 ).
'$call'(G, _CP, _G0, CurMod) :-
% /*
% 	(
%      '$is_metapredicate'(G,CurMod)
%     ->
    %      	'$reenspter_debugger'(exit)',
%      ( '$expand_meta_call'(CurMod:G, [], NG) ->  true ; true ),
%      '$enable_debugging'
%     ;
%      NG = G
%     ),
% 	*/
    '$execute0'(G, CurMod).

'$loop'(Stream,exo) :-
    prolog_flag(agc_margin,Old,0),
    prompt1(': '), prompt(_,'|     '),
    source_module(OldModule,OldModule),
    repeat,
    '$system_catch'(dbload_from_stream(Stream, OldModule, exo), '$db_load', Error,
		    user:'$LoopError'(Error, top)),
    prolog_flag(agc_margin,_,Old),
    !.
'$loop'(Stream,db) :-
    prolog_flag(agc_margin,Old,0),
    prompt1(': '), prompt(_,'|     '),
    source_module(OldModule,OldModule),
	repeat,
		'$system_catch'(dbload_from_stream(Stream, OldModule, db), '$db_load', Error, user:'$LoopError'(Error, db)
                   ),
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
	source_module( OldModule, OldModule ),
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
    prompt1(': '),
    prompt(_,'     '),
    Options = [module(Mod), syntax_errors(dec10),variable_names(Vars), term_position(Pos)],
    (
      Status == top
    ->
      read_term(Stream, Command, Options)
    ;
      read_clause(Stream, Command, Options)
    ),
    '$command'(Command,Mod,Vars,Pos, Status) .

/** @pred  user:expand_term( _T_,- _X_) is dynamic,multifile.

  This user-defined predicate is called by YAP after
  reading goals and clauses.

  - _Module_:`expand_term(` _T_ , _X_) is called first on the
  current source module _Module_ ; if i 
 - `user:expand_term(` _T_ , _X_ `)` is available on every module.

  */

/* General purpose predicates				*/

'$head_and_body'(M:(H:-B),M:H,M:B) :- !.
'$head_and_body'((H:-B),H,B) :- !.
'$head_and_body'(H,H,true).


gated_call(Setup, Goal, Catcher, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup),
    '$gated_call'( true , Goal, Catcher, Cleanup)  .

'$gated_call'( All , Goal, Catcher, Cleanup) :-
        Task0 = cleanup( All, Catcher, Cleanup, Tag, true, CP0),
	TaskF = cleanup( All, Catcher, Cleanup, Tag, false, CP0),
	'$tag_cleanup'(CP0, Task0),
	call( Goal ),
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

'$check_head_and_body'(MH, M, H, true, _XsP) :-
    '$yap_strip_module'(MH,M,H),
    must_be_callable(M:H).
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
    '$expand_clause'( InputCl,compile, C1, CO),
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

%% @}

%% @addtogroup CathThrow Catch and Throw
%  @ingroup YAPControl
%  @{

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   catch/throw implementation

% at each catch point I need to know:
% what is ball;
% where was the previous catch

/**
@pred  catch( : _Goal_,+ _Exception_,+ _Action_) is iso


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
	'$get_exception'(C0),
	( C = C0 -> '$execute_nonstop'(A, prolog) ; throw(C0) ).

% variable throws are user-handled.
'$run_catch'(G,E) :-
	var(E),
      !,
       call(G ).
'$run_catch'(abort,_) :-
        abort.
'$run_catch'('$Error'(E),E) :-
        !,
               '$LoopError'(E, top ).
'$run_catch'('$debugger'(E),E) :-
        !,
               '$LoopError'('$debugger'(E), top ).
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
	current_prolog_flag(debug, Debug),
	( '__NB_getval__'(  '$trace',Trace, fail) -> true ; Trace = off ),
	yap_flag(toplevel_prompt, P),
	'$break_info'(BreakLevel,P,P0),
	'$prompts'( Trace, Debug, P0, PF),
	prompt1(PF),
	prompt(_,' |   '),
	'$ensure_prompting'.

'$break_info'(0,P,P) :-
	!.
'$break_info'(BreakLevel, Prompt, P) :-
	atomic_concat( ['(Break level ', BreakLevel, ') ',Prompt], P).

'$prompts'(on,_,P,Prompt) :-
	!,
	atom_concat( 'trace ', P, Prompt).
'$prompts'(off,true,P,Prompt) :-
	!,
	atom_concat( 'debug ', P, Prompt).
'$prompts'(off,false,P,P) :-
	!.

/**
@}
*/
