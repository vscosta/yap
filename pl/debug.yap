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
* File:		debug.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	YAP's debugger						 *
*									 *
*************************************************************************/

/*-----------------------------------------------------------------------------

			Debugging / creating spy points

-----------------------------------------------------------------------------*/

:- op(900,fx,[spy,nospy]).


% First part : setting and reseting spy points

% $suspy does most of the work
'$suspy'(V,S,M) :- var(V) , !,
	'$do_error'(instantiation_error,M:spy(V,S)).
'$suspy'((M:S),P,_) :- !,
    '$suspy'(S,P,M).
'$suspy'([],_,_) :- !.
'$suspy'([F|L],S,M) :- !, ( '$suspy'(F,S,M) ; '$suspy'(L,S,M) ).
'$suspy'(F/N,S,M) :- !,
	functor(T,F,N),
	'$do_suspy'(S, F, N, T, M).
'$suspy'(A,S,M) :- atom(A), !,
	'$suspy_predicates_by_name'(A,S,M).
'$suspy'(P,spy,M) :- !,
	 '$do_error'(domain_error(predicate_spec,P),spy(M:P)).
'$suspy'(P,nospy,M) :-
	 '$do_error'(domain_error(predicate_spec,P),nospy(M:P)).

'$suspy_predicates_by_name'(A,S,M) :-
	% just check one such predicate exists
	(
	  current_predicate(A,M:_)
	->
	 M = EM,
	 A = NA
	;
	 recorded('$import','$import'(EM,M,GA,_,A,_),_),
	 functor(GA,NA,_)
	),
	!,
	'$do_suspy_predicates_by_name'(NA,S,EM).
'$suspy_predicates_by_name'(A,spy,M) :- !,
	print_message(warning,no_match(spy(M:A))).
'$suspy_predicates_by_name'(A,nospy,M) :-
	print_message(warning,no_match(nospy(M:A))).
	
'$do_suspy_predicates_by_name'(A,S,M) :-
	current_predicate(A,M:T),
	functor(T,A,N),
	'$do_suspy'(S, A, N, T, M).
'$do_suspy_predicates_by_name'(A, S, M) :-
	recorded('$import','$import'(EM,M,T0,_,A,N),_),
	functor(T0,A0,N0),
	'$do_suspy'(S, A0, N0, T, EM).


%
% protect against evil arguments.
%
'$do_suspy'(S, F, N, T, M) :-
	recorded('$import','$import'(EM,M,T0,_,F,N),_), !,
	functor(T0, F0, N0),
	'$do_suspy'(S, F0, N0, T, EM).
'$do_suspy'(S, F, N, T, M) :-
	 '$undefined'(T,M), !,
	 ( S = spy ->
	     print_message(warning,no_match(spy(M:F/N)))
	 ;
	     print_message(warning,no_match(nospy(M:F/N)))
	 ).
'$do_suspy'(S, F, N, T, M) :-
	 '$system_predicate'(T,M),
	'$flags'(T,M,F,F),
	F /\ 0x118dd080 =\= 0,
	 ( S = spy ->
	     '$do_error'(permission_error(access,private_procedure,T),spy(M:F/N))
	 ;
	     '$do_error'(permission_error(access,private_procedure,T),nospy(M:F/N))
	 ).
'$do_suspy'(S, F, N, T, M) :-
	 '$undefined'(T,M), !,
	 ( S = spy ->
	     print_message(warning,no_match(spy(M:F/N)))
	 ;
	     print_message(warning,no_match(nospy(M:F/N)))
	 ).
'$do_suspy'(S,F,N,T,M) :-
	'$suspy2'(S,F,N,T,M).

'$suspy2'(spy,F,N,T,M) :- 
	recorded('$spy','$spy'(T,M),_), !,
	print_message(informational,breakp(bp(debugger,plain,M:T,M:F/N,N),add,already)).
'$suspy2'(spy,F,N,T,M) :- !,
	recorda('$spy','$spy'(T,M),_), 
	'$set_spy'(T,M),
	print_message(informational,breakp(bp(debugger,plain,M:T,M:F/N,N),add,ok)).
'$suspy2'(nospy,F,N,T,M) :- 
	recorded('$spy','$spy'(T,M),R), !,
	erase(R),
	'$rm_spy'(T,M),
	print_message(informational,breakp(bp(debugger,plain,M:T,M:F/N,N),remove,last)).
'$suspy2'(nospy,F,N,_,M) :-
	print_message(informational,breakp(no,breakpoint_for,M:F/N)).

'$pred_being_spied'(G, M) :-
	recorded('$spy','$spy'(G,M),_), !.

spy Spec :-
	'$notrace'(prolog:debug_action_hook(spy(Spec))), !.
spy L :-
	'$current_module'(M),
	'$suspy'(L, spy, M), fail.
spy _ :- debug.

nospy Spec :-
	'$notrace'(prolog:debug_action_hook(nospy(Spec))), !.
nospy L :-
	'$current_module'(M),
	'$suspy'(L, nospy, M), fail.
nospy _.

nospyall :-
	'$notrace'(prolog:debug_action_hook(nospyall)), !.
nospyall :-
	recorded('$spy','$spy'(T,M),_), functor(T,F,N), '$suspy'(F/N,nospy,M), fail.
nospyall.

% debug mode -> debug flag = 1

debug :-
	( nb_getval('$spy_gn',L) -> true ; nb_setval('$spy_gn',1) ),
	'$start_debugging'(on),
	print_message(informational,debug(debug)).

'$start_debugging'(Mode) :-
	(Mode == on ->
	 '$debug_on'(true)
	;
	 '$debug_on'(false)
	),
	nb_setval('$debug_run',off),
	nb_setval('$debug_jump',false).
	
nodebug :-
	'$debug_on'(false),
	nb_setval('$trace',off),
	print_message(informational,debug(off)).

 %
 % remove any debugging info after an abort.
 %
	
trace :- 
	nb_getval('$trace',on), !.
trace :-
	nb_setval('$trace',on),
	'$start_debugging'(on),
	print_message(informational,debug(trace)),
	'$creep'.

notrace :-
	nodebug.

/*-----------------------------------------------------------------------------

				leash

-----------------------------------------------------------------------------*/


leash(X) :- var(X),
	'$do_error'(instantiation_error,leash(X)).
leash(X) :-
	'$leashcode'(X,Code),
	set_value('$leash',Code),
	'$show_leash'(informational,Code), !.
leash(X) :-
	'$do_error'(type_error(leash_mode,X),leash(X)).

'$show_leash'(Msg,0) :-
	print_message(Msg,leash([])).
'$show_leash'(Msg,Code) :-
	'$check_leash_bit'(Code,0x8,L3,call,LF),
	'$check_leash_bit'(Code,0x4,L2,exit,L3),
	'$check_leash_bit'(Code,0x2,L1,redo,L2),
	'$check_leash_bit'(Code,0x1,[],fail,L1),
	print_message(Msg,leash(LF)).

'$check_leash_bit'(Code,Bit,L0,_,L0) :- Bit /\ Code =:= 0, !.
'$check_leash_bit'(_,_,L0,Name,[Name|L0]).

'$leashcode'(full,0xf) :- !.
'$leashcode'(on,0xf) :- !.
'$leashcode'(half,0xb) :- !.
'$leashcode'(loose,0x8) :- !.
'$leashcode'(off,0x0) :- !.
'$leashcode'(none,0x0) :- !.
%'$leashcode'([L|M],Code) :- !, '$leashcode_list'([L|M],Code).
'$leashcode'([L|M],Code) :- !,
	'$list2Code'([L|M],Code).
'$leashcode'(N,N) :- integer(N), N >= 0, N =< 0xf.

'$list2Code'(V,_) :- var(V), !,
	'$do_error'(instantiation_error,leash(V)).
'$list2Code'([],0) :- !.
'$list2Code'([V|L],_) :- var(V), !,
	'$do_error'(instantiation_error,leash([V|L])).
'$list2Code'([call|L],N) :- '$list2Code'(L,N1), N is 0x8 + N1.
'$list2Code'([exit|L],N) :- '$list2Code'(L,N1), N is 0x4 + N1.
'$list2Code'([redo|L],N) :- '$list2Code'(L,N1), N is 0x2 + N1.
'$list2Code'([fail|L],N) :- '$list2Code'(L,N1), N is 0x1 + N1.

/*-----------------------------------------------------------------------------

				debugging

-----------------------------------------------------------------------------*/


debugging :-
	prolog:debug_action_hook(nospyall), !.
debugging :-
	( '$debug_on'(true) ->
	    print_message(help,debug(debug))
	    ;
	    print_message(help,debug(off))
	),
	findall(M:(N/A),(recorded('$spy','$spy'(T,M),_),functor(T,N,A)),L),
	print_message(help,breakpoints(L)),
	get_value('$leash',Leash),
	'$show_leash'(help,Leash).

/*-----------------------------------------------------------------------------

				spy

-----------------------------------------------------------------------------*/

% ok, I may have a spy point for this goal, or not.
%  if I do, I should check what mode I am in.
% Goal/Mode          Have Spy     Not Spied
% Creep                 Stop        Stop
% Leap                  Stop        Create CP
% Skip               Create CP     Create CP
% FastLeap              Stop        Ignore
% FastIgnore           Ignore       Ignore
    

%	flag		description		initial possible values

%	spy_gn		goal number		1	1...
%	spy_trace	trace		0	0, 1
%	spy_skip	leap			off	Num (stop level)
%	debug_prompt	stop at spy points	on	on,off
% a flip-flop is also used
%	when 1 spying is enabled *(the same as spy stop).


%'$spy'(G) :- write(user_error,'$spy'(G)), nl, fail.
%
% handle suspended goals
% take care with hidden goals.
%
% $spy may be called from user code, so be careful.
'$spy'([Mod|G]) :-
	'$debug_on'(F), F = false, !,
	'$execute_nonstop'(G,Mod).
'$spy'([Mod|G]) :-
	nb_getval('$system_mode',on), !,
	'$execute_nonstop'(G,Mod).
'$spy'([Mod|G]) :-
	CP is '$last_choice_pt',	
	'$do_spy'(G, Mod, CP, no).

% last argument to do_spy says that we are at the end of a context. It
% is required to know whether we are controlled by the debugger.
'$do_spy'(V, M, CP, Flag) :- var(V), !, '$do_spy'(call(V), M, CP, Flag).
'$do_spy'(!, _, CP, _) :- !, '$$cut_by'(CP).
'$do_spy'('$cut_by'(M), _, _, _) :- !, '$$cut_by'(M).
'$do_spy'(true, _, _, _) :- !.
%'$do_spy'(fail, _, _, _) :- !, fail.
'$do_spy'(M:G, _, CP, CalledFromDebugger) :- !,
	'$do_spy'(G, M, CP, CalledFromDebugger).
'$do_spy'((A,B), M, CP, CalledFromDebugger) :- !,
	'$do_spy'(A, M, CP, yes),
	'$do_spy'(B, M, CP, CalledFromDebugger).
'$do_spy'((T->A;B), M, CP, CalledFromDebugger) :- !,
	( '$do_spy'(T, M, CP, yes) -> '$do_spy'(A, M, CP, yes)
	;
	  '$do_spy'(B, M, CP, CalledFromDebugger)
	).
'$do_spy'((T->A|B), M, CP, CalledFromDebugger) :- !,
	( '$do_spy'(T, M, CP, yes) -> 	'$do_spy'(A, M, CP, yes)
	;
	  '$do_spy'(B, M, CP, CalledFromDebugger)
	).
'$do_spy'((T->A), M, CP, _) :- !,
	( '$do_spy'(T, M, CP, yes) -> '$do_spy'(A, M, CP, yes) ).
'$do_spy'((A;B), M, CP, CalledFromDebugger) :- !,
	(
	  '$do_spy'(A, M, CP, yes)
	;
	  '$do_spy'(B, M, CP, CalledFromDebugger)
	).
'$do_spy'((A|B), M, CP, CalledFromDebugger) :- !,
	(
	  '$do_spy'(A, M, CP, yes)
	;
	  '$do_spy'(B, M, CP, CalledFromDebugger)
	).
'$do_spy'((\+G), M, CP, CalledFromDebugger) :- !,
	\+ '$do_spy'(G, M, CP, CalledFromDebugger).
'$do_spy'((not(G)), M, CP, CalledFromDebugger) :- !,
	\+ '$do_spy'(G, M, CP, CalledFromDebugger).
'$do_spy'(G, Module, _, CalledFromDebugger) :-
        nb_getval('$spy_gn',L),		/* get goal no.			*/
	L1 is L+1,			/* bump it			*/
	nb_setval('$spy_gn',L1),	/* and save it globaly		*/
        b_getval('$spy_glist',History),	/* get goal list		*/
	b_setval('$spy_glist',[info(L,Module,G,_Retry,_Det,_HasFoundAnswers)|History]),	/* and update it		*/
	'$loop_spy'(L, G, Module, CalledFromDebugger).	/* set creep on		*/

% we are skipping, so we can just call the goal,
% while leaving the minimal structure in place.
'$loop_spy'(GoalNumber, G, Module, CalledFromDebugger) :-
	yap_hacks:current_choice_point(CP),
	'$system_catch'('$loop_spy2'(GoalNumber, G, Module, CalledFromDebugger, CP),
		    Module, error(Event,Context),
		    '$loop_spy_event'(error(Event,Context), GoalNumber, G, Module, CalledFromDebugger)).

% handle weird things happening in the debugger.		    
'$loop_spy_event'('$pass'(Event), _, _, _, _) :- !,
	throw(Event).
'$loop_spy_event'(error('$retry_spy'(G0),_), GoalNumber, G, Module, CalledFromDebugger) :-
	G0 >= GoalNumber, !,
	'$loop_spy'(GoalNumber, G, Module, CalledFromDebugger).
'$loop_spy_event'(error('$retry_spy'(GoalNumber),_), _, _, _, _) :- !,
	throw(error('$retry_spy'(GoalNumber),[])).
'$loop_spy_event'(error('$fail_spy'(G0),_), GoalNumber, G, Module, CalledFromDebugger) :-
	G0 >= GoalNumber, !,
	'$loop_fail'(GoalNumber, G, Module, CalledFromDebugger).
'$loop_spy_event'(error('$fail_spy'(GoalNumber),_), _, _, _, _) :- !,
	throw(error('$fail_spy'(GoalNumber),[])).
'$loop_spy_event'(error('$done_spy'(G0),_), GoalNumber, G, _, CalledFromDebugger) :-
	G0 >= GoalNumber, !,
	'$continue_debugging'(CalledFromDebugger).
'$loop_spy_event'(error('$done_spy'(GoalNumber),_), _, _, _, _) :- !,
	throw(error('$done_spy'(GoalNumber),[])).
'$loop_spy_event'(Event, GoalNumber, G, Module, CalledFromDebugger) :-
	'$debug_error'(Event),
	'$system_catch'(
		     ('$trace'(exception(Event),G,Module,GoalNumber,_),fail),
		     Module,
		     error(NewEvent,NewContext),
		     '$loop_spy_event'(error(NewEvent,NewContext), GoalNumber, G, Module, CalledFromDebugger)
		    ).


'$debug_error'(Event) :-
	'$Error'(Event), fail.
'$debug_error'(_).


% just fail here, don't really need to call debugger, the user knows what he
% wants to do
'$loop_fail'(_GoalNumber, _G, _Module, _CalledFromDebugger) :-
	'$continue_debugging'(CalledFromDebugger),
	fail.

% if we are in 
'$loop_spy2'(GoalNumber, G0, Module, CalledFromDebugger, CP) :- 
/* the following choice point is where the predicate is  called */
	   (
             '$is_metapredicate'(G0, Module)
	   ->
	    '$meta_expansion'(G0,Module,Module,Module,G,[])
	   ;
	     G = G0
	   ),
	   b_getval('$spy_glist',[Info|_]),	/* get goal list		*/
	   Info = info(_,_,_,Retry,Det,false),
	   (
	    /* call port */
	    '$enter_goal'(GoalNumber, G, Module),
	    '$spycall'(G, Module, CalledFromDebugger, Retry),
	    '$disable_docreep',
	    (
	      '$debugger_deterministic_goal'(G) ->
	      Det=true
	    ;
	      Det=false
	    ),
	/* go execute the predicate	*/
	    (
	      Retry = false ->
	       /* found an answer, so it can redo */
	       nb_setarg(6, Info, true),
	      '$show_trace'(exit,G,Module,GoalNumber,Det),	/* output message at exit	*/
	       /* exit port */
	       /* get rid of deterministic computations */
	      (
		Det == true
		->
		'$$cut_by'(CP)
		;
		true
	      ),
	      '$continue_debugging'(CalledFromDebugger)	   
	      ;
		/* backtracking from exit				*/
	        /* we get here when we want to redo a goal		*/
		/* redo port */
	     '$disable_docreep',
	      (
	       arg(6, Info, true)
	      ->
	        '$show_trace'(redo,G,Module,GoalNumber,_), /* inform user_error		*/
	        nb_setarg(6, Info, false)
	       ;
	         true
	      ),
	     '$continue_debugging'(CalledFromDebugger),
	     fail			/* to backtrack to spycalls	*/
	     )
	  ;
	    '$show_trace'(fail,G,Module,GoalNumber,_), /* inform at fail port		*/
	    '$continue_debugging'(CalledFromDebugger),
	    /* fail port */
	    fail
	).
	
'$enter_goal'(GoalNumber, G, Module) :-
    '$zip'(GoalNumber, G, Module), !.
'$enter_goal'(GoalNumber, G, Module) :-
    '$trace'(call, G, Module, GoalNumber, _).

'$show_trace'(_, G, Module, GoalNumber,_) :-
	'$zip'(GoalNumber, G, Module), !.
'$show_trace'(P,G,Module,GoalNumber,Deterministic) :-
	'$trace'(P,G,Module,GoalNumber,Deterministic).

%
% skip a goal or a port
%
'$zip'(GoalNumber, G, Module) :-
    nb_getval('$debug_run',StopPoint),
    % zip mode off, we cannot zip
    StopPoint \= off,
    (
      % skip spy points (eg, s).
      StopPoint == spy
    ->
      \+ '$pred_being_spied'(G, Module)
    ;
      % skip goals (eg, l).
      number(StopPoint)
    ->
      StopPoint < GoalNumber
    ).
	


% 
'$spycall'(G, M, _, _) :-
	nb_getval('$debug_jump',true),
	!,
	'$execute_nonstop'(G,M).
'$spycall'(G, M, _, _) :-
        '$system_predicate'(G,M),
	\+ '$is_metapredicate'(G,M), !,
	'$execute'(M:G).
'$spycall'(G, M, _, _) :-
        '$system_module'(M), !,
	'$execute'(M:G).
'$spycall'(G, M, _, _) :-
        '$tabled_predicate'(G,M),
	 !,
	'$continue_debugging'(no, '$execute_nonstop'(G,M)).
'$spycall'(G, M, CalledFromDebugger, InRedo) :-
	'$flags'(G,M,F,F),
	F /\ 0x18402000 =\= 0, !, % dynamic procedure, logical semantics, user-C, or source
	% use the interpreter
	CP is '$last_choice_pt',
	'$clause'(G, M, Cl, _),
	% I may backtrack to here from far away
	'$disable_docreep',	
	( '$do_spy'(Cl, M, CP, CalledFromDebugger) ; InRedo = true ).
'$spycall'(G, M, CalledFromDebugger, InRedo) :-
	'$undefined'(G, M), !,
	'$find_goal_definition'(M, G, NM, Goal),
	'$spycall'(Goal, NM, CalledFromDebugger, InRedo).
'$spycall'(G, M, _, InRedo) :-
	% I lost control here.
	CP is '$last_choice_pt',
	'$static_clause'(G,M,_,R),
	% I may backtrack to here from far away
	'$disable_docreep',	
	(
	 '$continue_debugging'(no, '$execute_clause'(G, M, R, CP))
	;
	 InRedo = true
	).

'$tabled_predicate'(G,M) :-
	'$flags'(G,M,F,F),
	F /\ 0x00000040 =\= 0.

'$trace'(P,G,Module,L,Deterministic) :-
	% at this point we are done with leap or skip
	nb_setval('$debug_run',off),
	% make sure we run this code outside debugging mode.
	'$debug_on'(false),
	repeat,
	'$trace_msg'(P,G,Module,L,Deterministic),
	( 
	  '$unleashed'(P) ->
	  '$action'(10,P,L,G,Module,Debug),
	  put_code(user_error, 10)
	  ;
	  write(user_error,' ? '), get0(user_input,C),
	  '$action'(C,P,L,G,Module,Debug)
	),
	(Debug = on
	->
	 '$debug_on'(true)
	;
	 Debug = zip
	->
	 '$debug_on'(true)
	;
	 '$debug_on'(false)
	),
	!.

'$trace_msg'(P,G,Module,L,Deterministic) :-
	flush_output(user_output),
	flush_output(user_error),
	functor(P,P0,_),
	(P = exit, Deterministic \= true -> Det = '?' ; Det = ' '),
	('$pred_being_spied'(G,Module) -> CSPY = '*' ; CSPY = ' '),
% vsc: fix this
		%		( SL = L -> SLL = '>' ; SLL = ' '),
	SLL = ' ',
	( Module\=prolog,
	  Module\=user
	->
	    GW = Module:G
	;
	    GW = G	  
	),
	format(user_error,'~a~a~a       (~d)    ~q:',[Det,CSPY,SLL,L,P0]),
	'$debugger_write'(user_error,GW).

'$unleashed'(call) :- get_value('$leash',L), L /\ 2'1000 =:= 0. %'
'$unleashed'(exit) :- get_value('$leash',L), L /\ 2'0100 =:= 0. %'
'$unleashed'(redo) :- get_value('$leash',L), L /\ 2'0010 =:= 0. %'
'$unleashed'(fail) :- get_value('$leash',L), L /\ 2'0001 =:= 0. %'
% the same as fail.
'$unleashed'(exception(_)) :- get_value('$leash',L), L /\ 2'0001 =:= 0.  %'

'$debugger_write'(Stream, G) :-
	recorded('$print_options','$debugger'(OUT),_), !,
	write_term(Stream, G, OUT).
'$debugger_write'(Stream, G) :-
	writeq(Stream, G).

'$action'(13,P,CallNumber,G,Module,Zip) :- !,	% newline 	creep
	get0(user_input,C),
	'$action'(C,P,CallNumber,G,Module,Zip).
%'$action'(10,_,_,_,_,on) :-			% newline 	creep
%	nb_setval('$debug_jump',false).
'$action'(10,_,_,_,_,on) :- !,			% newline 	creep
	nb_setval('$debug_jump',false).
'$action'(0'!,_,_,_,_,_) :- !,			% ! 'g		execute
	read(user,G),
	% don't allow yourself to be caught by creep.
	'$debug_on'(OldDeb),
	'$debug_on'(false),
	( '$execute'(G) -> true ; true),
	'$debug_on'(OldDeb),
%	'$skipeol'(0'!),                        % '
	fail.
'$action'(0'<,_,_,_,_,_) :- !,			% <'Depth
	'$new_deb_depth',
	'$skipeol'(0'<),
	fail.
'$action'(0'^,_,_,G,_,_) :- !,			% '
	'$print_deb_sterm'(G),
	'$skipeol'(0'^),
	fail.
'$action'(0'a,_,_,_,_,off) :- !,		% 'a		abort
	'$skipeol'(0'a),
	abort.
'$action'(0'b,_,_,_,_,_) :- !,			% 'b		break
	'$skipeol'(0'b),
	break,
	fail.
'$action'(0'A,_,_,_,_,_) :- !,			% 'b		break
	'$skipeol'(0'A),
	'$show_choicepoint_stack',
	fail.
'$action'(0'c,_,_,_,_,on) :- !,			% 'c		creep
	'$skipeol'(0'c),
	nb_setval('$debug_jump',false).
'$action'(0'e,_,_,_,_,_) :- !,			% 'e		exit
	'$skipeol'(0'e),
	halt.
'$action'(0'f,_,CallId,_,_,_) :- !,		% 'f		fail
	'$scan_number'(0'f, CallId, GoalId),    %'f
	throw(error('$fail_spy'(GoalId),[])).
'$action'(0'h,_,_,_,_,_) :- !,			% 'h		help
	'$action_help',
	'$skipeol'(104),
	fail.
'$action'(0'?,_,_,_,_,_) :- !,			% '?		help
	'$action_help',
	'$skipeol'(104),
	fail.
'$action'(0'p,_,_,G,Module,_) :- !,		% 'p		print
	((Module = prolog ; Module = user) ->
	    print(user_error,G), nl(user_error)
	;
	    print(user_error,Module:G), nl(user_error)
	),
	'$skipeol'(0'p),
	fail.
'$action'(0'd,_,_,G,Module,_) :- !,		% 'd		display
	((Module = prolog ; Module = user) ->
	    display(user_error,G), nl(user_error)
	;
	    display(user_error,Module:G), nl(user_error)
	),
	'$skipeol'(0'd),
	fail.
'$action'(0'l,_,_,_,_,on) :- !,			% 'l		leap
	'$skipeol'(0'l),
	nb_setval('$debug_run',spy),
	nb_setval('$debug_jump',false).
'$action'(0'z,_,_,_,_,zip) :- !,		% 'z		zip, fast leap
	'$skipeol'(0'z),			% 'z
	nb_setval('$debug_run',spy),
	nb_setval('$debug_jump',true).
	% skip first call (for current goal),
	% stop next time.
'$action'(0'k,_,_,_,_,zip) :- !,		% 'k		zip, fast leap
	'$skipeol'(0'k),			% '
	nb_setval('$debug_run',spy),
	nb_setval('$debug_jump',true).
	% skip first call (for current goal),
	% stop next time.
'$action'(0'n,_,_,_,_,off) :- !,			% 'n		nodebug
	'$skipeol'(0'n),				% '
	% tell debugger never to stop.
        nb_setval('$debug_run', -1),
	nb_setval('$debug_jump',true),
	nodebug.
'$action'(0'r,_,CallId,_,_,_) :- !,		        % 'r		retry
        '$scan_number'(0'r,CallId,ScanNumber),		% '
        '$debug_on'(true),
	throw(error('$retry_spy'(ScanNumber),[])).
'$action'(0's,P,CallNumber,_,_,on) :- !,		% 's		skip
	'$skipeol'(0's),				% '		
	( (P=call; P=redo) ->
	  nb_setval('$debug_run',CallNumber),
	  nb_setval('$debug_jump',false)
	;
	    '$ilgl'(0's)				% '
	).
'$action'(0't,P,CallNumber,_,_,zip) :- !,		% 't		fast skip
	'$skipeol'(0't),				% '
	( (P=call; P=redo) ->
	  nb_setval('$debug_run',CallNumber),
	  nb_setval('$debug_jump',true)
	;
	    '$ilgl'(0't)				% '
	).
'$action'(0'+,_,_,G,M,_) :- !,			% '+		spy this
	functor(G,F,N), spy(M:(F/N)),
	'$skipeol'(0'+),			% '
	fail.
'$action'(0'-,_,_,G,M,_) :- !,			% '-		nospy this
	functor(G,F,N), nospy(M:(F/N)),
	'$skipeol'(0'-),			% '
	fail.
'$action'(0'g,_,_,_,_,_) :- !,			% 'g		ancestors
        '$scan_number'(0'g,-1,HowMany),         % '
        '$show_ancestors'(HowMany),
	fail.
'$action'(C,_,_,_,_,_) :-
	'$skipeol'(C),
	'$ilgl'(C),
	fail.

'$continue_debugging'(yes).
% do not need to debug!
'$continue_debugging'(no) :-
	'$creep'.

% if we are in the interpreter, don't need to care about forcing a trace, do we?
'$continue_debugging'(yes,G) :- !,
	'$execute_dgoal'(G).
% do not need to debug!
'$continue_debugging'(_,G) :-
	'nb_getval'('$debug_run',Zip),
        (Zip == nodebug ; number(Zip) ; Zip = spy(_) ), !,
	'$execute_dgoal'(G).
'$continue_debugging'(_,G) :-
	'$execute_creep_dgoal'(G).
	
'$execute_dgoal'('$execute_nonstop'(G,M)) :-
	'$execute_nonstop'(G,M).
'$execute_dgoal'('$execute_clause'(G, M, R, CP)) :-
	'$execute_clause'(G, M, R, CP).

'$execute_creep_dgoal'('$execute_nonstop'(G,M)) :-
	'$signal_creep',
	'$execute_nonstop'(G,M).
'$execute_creep_dgoal'('$execute_clause'(G, M, R, CP)) :-
	'$signal_creep',
	'$execute_clause'(G, M, R, CP).

'$show_ancestors'(HowMany) :-
	b_getval('$spy_glist',[_|History]),
	(
	  History == []
	->
	  print_message(help, ancestors([]))
	;
	  '$show_ancestors'(History,HowMany),
	  nl(user_error)
	).

'$show_ancestors'([],_).
'$show_ancestors'([_|_],0) :- !.
'$show_ancestors'([info(L,M,G,Retry,Det,_Exited)|History],HowMany) :-
	'$show_ancestor'(L,M,G,Retry,Det,HowMany,HowMany1),
	'$show_ancestors'(History,HowMany1).

% skip exit port, we're looking at true ancestors
'$show_ancestor'(_,_,_,_,Det,HowMany,HowMany) :-
	nonvar(Det), !.
% look at retry
'$show_ancestor'(GoalNumber, M, G, Retry, _, HowMany, HowMany1) :-
	nonvar(Retry), !,
	HowMany1 is HowMany-1,
	'$trace_msg'(redo, G, M, GoalNumber, _), nl(user_error).
'$show_ancestor'(GoalNumber, M, G, _, _, HowMany, HowMany1) :-
	HowMany1 is HowMany-1,
	'$trace_msg'(call, G, M, GoalNumber, _), nl(user_error).


'$action_help' :-
	format(user_error,'newline  creep       a       abort~n', []),
	format(user_error,'c        creep       e       exit~n', []),
	format(user_error,'f Goal   fail        h       help~n', []),
	format(user_error,'l        leap        r Goal  retry~n', []),
	format(user_error,'s        skip        t       fastskip~n', []),
	format(user_error,'q        quasiskip   k       quasileap~n', []),
	format(user_error,'b        break       n       no debug~n', []),
	format(user_error,'p        print       d       display~n', []),
	format(user_error,'<D       depth D     <       full term~n', []),
	format(user_error,'+        spy this    -       nospy this~n', []),
	format(user_error,'^        view subg   ^^      view using~n', []),
	format(user_error,'A        choices     g [N]   ancestors~n', []),
	format(user_error,'! g execute goal~n', []).
	
'$ilgl'(C) :-
	print_message(warning, trace_command(C)),
	print_message(help, trace_help),
	fail.

'$skipeol'(10) :- !.
'$skipeol'(_) :- get0(user,C), '$skipeol'(C).

'$scan_number'(_, _, Nb) :-
	get0(user,C),
	'$scan_number2'(C, Nb), !.
'$scan_number'(_, CallId, CallId).

'$scan_number2'(10, _) :- !, fail.
'$scan_number2'(0' , Nb) :- !, % '
	get0(user,C),
	'$scan_number2'(C , Nb).
'$scan_number2'(0'	, Nb) :- !, %'
	get0(user,C),
	'$scan_number2'(C, Nb).
'$scan_number2'(C, Nb) :-
	'$scan_number3'(C, 0, Nb).

'$scan_number3'(10,  Nb, Nb) :- !, Nb > 0.
'$scan_number3'( C, Nb0, Nb) :-
	C >= "0", C =< "9",
	NbI is Nb0*10+(C-"0"),
	get0(user, NC),
	'$scan_number3'( NC, NbI, Nb).
	
'$print_deb_sterm'(G) :-
	'$get_sterm_list'(L), !,
	'$deb_get_sterm_in_g'(L,G,A),
	recorda('$debug_sub_skel',L,_),
	format(user_error,'~n~w~n~n',[A]).
'$print_deb_sterm'(_) :- '$skipeol'(94).

'$get_sterm_list'(L) :-
	get0(user_input,C),
	'$deb_inc_in_sterm_oldie'(C,L0,CN),
	'$get_sterm_list'(L0,CN,0,L).

'$deb_inc_in_sterm_oldie'(94,L0,CN) :- !,
	get0(user_input,CN),
	( recorded('$debug_sub_skel',L0,_) -> true ;
	  CN = [] ).
'$deb_inc_in_sterm_oldie'(C,[],C).

'$get_sterm_list'(L0,C,N,L) :-
	( C =:= "^", N =\= 0 -> get0(CN),
				'$get_sterm_list'([N|L0],CN,0,L) ;
	  C >= "0", C =< "9" -> NN is 10*N+C-"0", get0(CN),
				'$get_sterm_list'(L0,CN,NN,L);
	  C =:= 10 -> (N =:= 0 -> L = L0 ; L=[N|L0]) ).

'$deb_get_sterm_in_g'([],G,G).
'$deb_get_sterm_in_g'([H|T],G,A) :-
	'$deb_get_sterm_in_g'(T,G,A1),
	arg(H,A1,A).

'$new_deb_depth' :-
	get0(user_input,C),
	'$get_deb_depth'(C,D),
	'$set_deb_depth'(D).

'$get_deb_depth'(10,10) :-  !. % default depth is 0
'$get_deb_depth'(C,XF) :-
	'$get_deb_depth_char_by_char'(C,0,XF).

'$get_deb_depth_char_by_char'(10,X,X) :- !.
'$get_deb_depth_char_by_char'(C,X0,XF) :-
	C >= "0", C =< "9", !,
	XI is X0*10+C-"0",
	get0(user_input,NC),
	'$get_deb_depth_char_by_char'(NC,XI,XF).
% reset when given garbage.
'$get_deb_depth_char_by_char'(C,_,10) :- '$skipeol'(C).

'$set_deb_depth'(D) :-
	recorded('$print_options','$debugger'(L),R), !,
	'$delete_if_there'(L, max_depth(_), LN),
	erase(R),
	recorda('$print_options','$debugger'([max_depth(D)|LN]),_).
'$set_deb_depth'(D) :-
	recorda('$print_options','$debugger'([quoted(true),numbervars(true),portrayed(true),max_depth(D)]),_).
	
'$delete_if_there'([], _, []).
'$delete_if_there'([T|L], T, LN) :- !,
	'$delete_if_there'(L, T, LN).
'$delete_if_there'([Q|L], T, [Q|LN]) :-
	'$delete_if_there'(L, T, LN).

'$show_choicepoint_stack' :-
	yap_hacks:current_choicepoints(Cps),
	length(Cps,Level),
	'$debug_show_cps'(Cps,Level).

'$debug_show_cps'([],_).
'$debug_show_cps'([C|Cps],Level) :-
	'$debug_show_cp'(C, Level),
	Level1 is Level-1,
	'$debug_show_cps'(Cps, Level1).

'$debug_show_cp'(C, Level) :-
	yap_hacks:choicepoint(C,_,Module,Name,Arity,Goal,_),
	'$continue_debug_show_cp'(Module,Name,Arity,Goal,Level).

'$continue_debug_show_cp'(prolog,'$do_live',0,(_;_),Level) :- !,
	format(user_error,'      [~d] \'$toplevel\'',[Level]).
'$continue_debug_show_cp'(prolog,'$do_log_upd_clause',4,'$do_log_upd_clause'(_,_,Goal,_),Level) :- !,
	format(user_error,'      [~d] ',[Level]),
	'$debugger_write'(user_error,Goal),
	nl(user_error).
'$continue_debug_show_cp'(prolog,'$do_static_clause',5,'$do_static_clause'(_,_,Goal,_,_),Level) :- !,
	format(user_error,'      [~d] ',[Level]),
	'$debugger_write'(user_error,Goal),
	nl(user_error).
'$continue_debug_show_cp'(Module,Name,Arity,_,_) :-
	functor(G0, Name, Arity),
	'$hidden_predicate'(G0,Module),
	!.
'$continue_debug_show_cp'(Module,Name,Arity,Goal,Level) :-
	var(Goal), !,
	format(user_error,'      [~d] ~q:~q/~d~n',[Level,Module,Name,Arity]).
'$continue_debug_show_cp'(Module,Name,Arity,(V1;V2),Level) :-
	var(V1),  var(V2), !,
	format(user_error,'      [~d] ~q:~q/~d: ;/2~n',[Level,Module,Name,Arity]).
'$continue_debug_show_cp'(_,_,_,G,Level) :-
	format(user_error,'      [~d] ~q~n',[Level,G]).
	
'$debugger_deterministic_goal'(G) :-
	yap_hacks:current_choicepoints(CPs0),
%	$cps(CPs0),
	'$debugger_skip_traces'(CPs0,CPs1),
	'$debugger_skip_loop_spy2'(CPs1,CPs2),
	'$debugger_skip_spycall'(CPs2,CPs3),
	'$debugger_skip_loop_spy2'(CPs3,[Catch|_]),
	yap_hacks:choicepoint(Catch,_,prolog,'$catch',3,'$catch'(_,'$loop_spy_event'(_,_,G,_,_),_),_).


'$cps'([CP|CPs]) :-
    yap_hacks:choicepoint(CP,A,B,C,D,E,F),
    write(A:B:C:D:E:F),nl,
    '$cps'(CPs).
'$cps'([]).


'$debugger_skip_spycall'([CP|CPs],CPs1) :-
	yap_hacks:choicepoint(CP,_,prolog,'$spycall',4,(_;_),_), !,
	'$debugger_skip_spycall'(CPs,CPs1).
'$debugger_skip_spycall'(CPs,CPs).

'$debugger_skip_traces'([CP|CPs],CPs1) :-
	yap_hacks:choicepoint(CP,_,prolog,'$trace',4,(_;_),_), !,
	'$debugger_skip_traces'(CPs,CPs1).
'$debugger_skip_traces'(CPs,CPs).

'$debugger_skip_loop_spy2'([CP|CPs],CPs1) :-
	yap_hacks:choicepoint(CP,_,prolog,'$loop_spy2',5,(_;_),_), !,
	'$debugger_skip_loop_spy2'(CPs,CPs1).
'$debugger_skip_loop_spy2'(CPs,CPs).



