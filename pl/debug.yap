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

:- thread_local([idb:'$debug',idb:'$trace',idb:'$spy_skip',idb:'$spy_stop']).

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
	;
	  recorded('$import','$import'(EM,M,A,_),_)
	),
	!,
	'$do_suspy_predicates_by_name'(A,S,M).
'$suspy_predicates_by_name'(A,spy,M) :- !,
	'$print_message'(warning,no_match(spy(M:A))).
'$suspy_predicates_by_name'(A,nospy,M) :-
	'$print_message'(warning,no_match(nospy(M:A))).
	
'$do_suspy_predicates_by_name'(A,S,M) :-
	current_predicate(A,M:T),
	functor(T,A,N),
	'$do_suspy'(S, A, N, T, M).
'$do_suspy_predicates_by_name'(A, S, M) :-
	recorded('$import','$import'(EM,M,A,N),_),
	functor(T,A,N),
	'$do_suspy'(S, A, N, T, EM).


%
% protect against evil arguments.
%
'$do_suspy'(S, F, N, T, M) :-
	recorded('$import','$import'(EM,M,F,N),_), !,
	'$do_suspy'(S, F, N, T, EM).
'$do_suspy'(S, F, N, T, M) :-
	 '$undefined'(T,M), !,
	 ( S = spy ->
	     '$print_message'(warning,no_match(spy(M:F/N)))
	 ;
	     '$print_message'(warning,no_match(nospy(M:F/N)))
	 ).
'$do_suspy'(S, F, N, T, M) :-
	 '$system_predicate'(T,M),
	 ( S = spy ->
	     '$do_error'(permission_error(access,private_procedure,T),spy(M:F/N))
	 ;
	     '$do_error'(permission_error(access,private_procedure,T),nospy(M:F/N))
	 ).
'$do_suspy'(S,F,N,T,M) :-
	'$suspy2'(S,F,N,T,M).

'$suspy2'(spy,F,N,T,M) :- 
	recorded('$spy','$spy'(T,M),_), !,
	'$print_message'(informational,breakp(bp(debugger,plain,M:T,M:F/N,N),add,already)).
'$suspy2'(spy,F,N,T,M) :- !,
	recorda('$spy','$spy'(T,M),_), 
	'$set_spy'(T,M),
	'$print_message'(informational,breakp(bp(debugger,plain,M:T,M:F/N,N),add,ok)).
'$suspy2'(nospy,F,N,T,M) :- 
	recorded('$spy','$spy'(T,M),R), !,
	erase(R),
	'$rm_spy'(T,M),
	'$print_message'(informational,breakp(bp(debugger,plain,M:T,M:F/N,N),remove,last)).
'$suspy2'(nospy,F,N,_,M) :-
	'$print_message'(informational,breakp(no,breakpoint_for,M:F/N)).

'$pred_being_spied'(G, M) :-
	recorded('$spy','$spy'(G,M),_), !.

spy L :-
	'$current_module'(M),
	'$suspy'(L, spy, M), fail.
spy _ :- debug.

nospy L :-
	'$current_module'(M),
	'$suspy'(L, nospy, M), fail.
nospy _.

nospyall :-
	recorded('$spy','$spy'(T,M),_), functor(T,F,N), '$suspy'(F/N,nospy,M), fail.
nospyall.

% debug mode -> debug flag = 1

debug :- recordaifnot('$debug',on,_), !,
	'$print_message'(informational,debug(debug)).
debug.
	
nodebug :- 
	recorded('$debug',_,R), erase(R), fail.
nodebug :- 
	recorded('$trace',_,R), erase(R), fail.
nodebug :- nospyall,
	'$set_yap_flags'(10,0),
	'$print_message'(informational,debug(off)).

trace :- 
	recorded('$trace',on,_), !.
trace :- 
	recorded('$spy_skip',_,R), erase(R), fail.
trace :-
	( recordaifnot('$trace',on,_) -> true ; true),
	( recordaifnot('$debug',on,_) -> true ; true),
	( recordaifnot('$spy_stop',on,_) -> true ; true),
	'$set_yap_flags'(10,1),
	'$print_message'(informational,debug(trace)),
	'$creep'.

notrace :- 
	recorded('$debug',_,R), erase(R), fail.
notrace :- 
	recorded('$trace',_,R), erase(R), fail.
notrace :- 
	'$print_message'(informational,debug(off)).

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
	'$print_message'(Msg,leash([])).
'$show_leash'(Msg,Code) :-
	'$check_leash_bit'(Code,2'1000,L3,call,LF),
	'$check_leash_bit'(Code,2'0100,L2,exit,L3),
	'$check_leash_bit'(Code,2'0010,L1,redo,L2),
	'$check_leash_bit'(Code,2'0001,[],fail,L1),
	'$print_message'(Msg,leash(LF)).

'$check_leash_bit'(Code,Bit,L0,_,L0) :- Bit /\ Code =:= 0, !.
'$check_leash_bit'(_,_,L0,Name,[Name|L0]).

'$leashcode'(full,2'1111) :- !.
'$leashcode'(on,2'1111) :- !.
'$leashcode'(half,2'1010) :- !.
'$leashcode'(loose,2'1000) :- !.
'$leashcode'(off,2'0000) :- !.
'$leashcode'(none,2'0000) :- !.
%'$leashcode'([L|M],Code) :- !, '$leashcode_list'([L|M],Code).
'$leashcode'([L|M],Code) :- !,
	'$list2Code'([L|M],Code).
'$leashcode'(N,N) :- integer(N), N >= 0, N =< 2'1111.

'$list2Code'(V,_) :- var(V), !,
	'$do_error'(instantiation_error,leash(V)).
'$list2Code'([],0) :- !.
'$list2Code'([V|L],_) :- var(V), !,
	'$do_error'(instantiation_error,leash([V|L])).
'$list2Code'([call|L],N) :- '$list2Code'(L,N1), N is 2'1000 + N1.
'$list2Code'([exit|L],N) :- '$list2Code'(L,N1), N is 2'0100 + N1.
'$list2Code'([redo|L],N) :- '$list2Code'(L,N1), N is 2'0010 + N1.
'$list2Code'([fail|L],N) :- '$list2Code'(L,N1), N is 2'0001 + N1.

/*-----------------------------------------------------------------------------

				debugging

-----------------------------------------------------------------------------*/

debugging :-
	( recorded('$debug',on,_) ->
	    '$print_message'(help,debug(debug))
	    ;
	    '$print_message'(help,debug(off))
	),
	findall(M:(N/A),(recorded('$spy','$spy'(T,M),_),functor(T,N,A)),L),
	'$print_message'(help,breakpoints(L)),
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
%	spy_stop	stop at spy points	on	on,off
% a flip-flop is also used
%	when 1 spying is enabled *(the same as spy stop).


%'$spy'(G) :- write(user_error,'$spy'(G)), nl, fail.
%
% handle suspended goals
% take care with hidden goals.
%
% $spy may be called from user code, so be careful.
'$spy'([Mod|G]) :-
	CP is '$last_choice_pt',	
	'$do_spy'(G, Mod, CP, yes).

% last argument to do_spy says that we are at the end of a context. It
% is required to know whether we are controlled by the debugger.
'$do_spy'(_, _, _, _) :-
	'$stop_debugging', fail.
'$do_spy'(!, _, CP, _) :- !, '$cut_by'(CP).
'$do_spy'('$cut_by'(M), _, _, _) :- !, '$cut_by'(M).
'$do_spy'(true, _, _, _) :- !.
'$do_spy'(M:G, _, CP, InControl) :- !,
	'$do_spy'(G, M, CP, InControl).
'$do_spy'((A,B), M, CP, InControl) :- !,
	'$do_spy'(A, M, CP, yes),
	'$do_spy'(B, M, CP, InControl).
'$do_spy'((T->A;B), M, CP, InControl) :- !,
	( '$do_spy'(T, M, CP, yes) -> '$do_spy'(A, M, CP, yes)
	;
	  '$do_spy'(B, M, CP, InControl)
	).
'$do_spy'((A;B), M, CP, InControl) :- !,
	(
	  '$do_spy'(A, M, CP, yes)
	;
	  '$do_spy'(B, M, CP, InControl)
	).
'$do_spy'((T->A|B), M, CP, InControl) :- !,
	( '$do_spy'(T, M, CP, yes) -> 	'$do_spy'(A, M, CP, yes)
	;
	  '$do_spy'(B, M, CP, InControl)
	).
'$do_spy'((A|B), M, CP, InControl) :- !,
	(
	  '$do_spy'(A, M, CP, yes)
	;
	  '$do_spy'(B, M, CP, InControl)
	).
'$do_spy'((\+G), M, CP, InControl) :- !,
	\+ '$do_spy'(G, M, CP, InControl).
'$do_spy'((not(G)), M, InControl) :- !,
	\+ '$do_spy'(G, M, CP, InControl).
'$do_spy'(G, Module, _, InControl) :-
    get_value(spy_gn,L),		/* get goal no.			*/
    L1 is L+1,				/* bump it			*/
    set_value(spy_gn,L1),		/* and save it globaly		*/
    '$loop_spy'(L, G, Module, InControl).		/* set creep on			*/

% we are skipping, so we can just call the goal,
% while leaving the minimal structure in place.
'$loop_spy'(GoalNumber, G, Module, InControl) :-
    '$system_catch'('$loop_spy2'(GoalNumber, G, Module, InControl),
		    Module, Event,
		    '$loop_spy_event'(Event, GoalNumber, G, Module, InControl)).

% handle weird things happening in the debugger.		    
'$loop_spy_event'(_, _, _, _, _) :-
	'$stop_debugging', fail.
'$loop_spy_event'('$retry_spy'(G0), GoalNumber, G, Module, InControl) :-
     G0 >= GoalNumber, !,
     '$loop_spy'(GoalNumber, G, Module, InControl).
'$loop_spy_event'('$retry_spy'(GoalNumber), _, _, _, _) :- !,
     throw('$retry_spy'(GoalNumber)).
'$loop_spy_event'('$fail_spy'(G0), GoalNumber, G, Module, InControl) :-
     G0 >= GoalNumber, !,
    '$loop_fail'(GoalNumber, G, Module, InControl).
'$loop_spy_event'('$fail_spy'(GoalNumber), _, _, _, _) :- !,
     throw('$fail_spy'(GoalNumber)).
'$loop_spy_event'(abort, _, _, _, _) :- !,
     throw(abort).
'$loop_spy_event'(Event, GoalNumber, G, Module, InControl) :- !,
     '$system_catch'(
		     ('$trace'(exception(Event),G,Module,GoalNumber),fail),
		     Module,NewEvent,
		     '$loop_spy_event'(NewEvent, GoalNumber, G, Module, InControl)).


'$loop_fail'(GoalNumber, G, Module, InControl) :-
    '$system_catch'(('$trace'(fail, G, Module, GoalNumber),
		     fail ),
		    Module, Event,
		    '$loop_spy_event'(Event, GoalNumber, G, Module, InControl)).

% if we are in 
'$loop_spy2'(GoalNumber, G, Module, InControl) :- 
/* the following choice point is where the predicate is  called */
	(
	    '$enter_goal'(GoalNumber, G, Module),
	    '$spycall'(G, Module, InControl),
	/* go execute the predicate	*/
	    (
	       '$stop_debugging',
	       '$show_trace'(exit,G,Module,GoalNumber),	/* output message at exit	*/
	       '$continue_debugging'(InControl)
	     ;		/* exit				*/
	        /* we get here when we want to redo a goal		*/
	        '$stop_debugging',
	        '$show_trace'(redo,G,Module,GoalNumber), /* inform user_error		*/
	        '$continue_debugging'(InControl),
	        fail			/* to backtrack to spycalls	*/
	     )
	  ;
	    '$stop_debugging',
	    '$show_trace'(fail,G,Module,GoalNumber), /* inform at fail port		*/
	    '$continue_debugging'(InControl),
	    fail
	).

'$enter_goal'(GoalNumber, G, Module) :-
    '$avoid_goal'(GoalNumber, G, Module), !.
'$enter_goal'(GoalNumber, G, Module) :-
    '$trace'(call, G, Module, GoalNumber).

'$show_trace'(_, G, Module, GoalNumber) :-
	'$avoid_goal'(GoalNumber, G, Module), !.
'$show_trace'(P,G,Module,GoalNumber) :-
	'$trace'(P,G,Module,GoalNumber).

'$avoid_goal'(GoalNumber, G, Module) :-
    \+ recorded('$debug',on,_), !.
'$avoid_goal'(GoalNumber, G, Module) :-
    recorded('$spy_skip', Value, _),
    '$continue_avoid_goal'(GoalNumber, G, Module, Value).

% for leap keep on going until finding something spied.
'$continue_avoid_goal'(_, G, Module, _) :-
    recorded('$spy_stop', on, _), !,
    \+ '$pred_being_spied'(G, Module).
% for skip keep on going until we get back.
'$continue_avoid_goal'(GoalNumber, _, _, Value) :-
    number(Value),
    Value < GoalNumber.
	

% 
'$spycall'(G, M, _) :-
	'$access_yap_flags'(10,0), !,
	'$execute_nonstop'(G, M).
'$spycall'(G, M, InControl) :-
	'$flags'(G,M,F,F),
	F /\ 0x8402000 =\= 0, !, % dynamic procedure, logical semantics, or source
	% use the interpreter
	CP is '$last_choice_pt',
	'$clause'(G, M, Cl),
	'$stop_debugging',
	'$do_spy'(Cl, M, CP, InControl).
'$spycall'(G, M, InControl) :-
	% I lost control here.
	'$continue_debugging'(InControl),
	'$execute_nonstop'(G, M).


'$trace'(P,G,Module,L) :-
	flush_output(user_output),
	flush_output(user_error),
	recorded('$debug',on,R0), erase(R0),
	repeat,
		('$pred_being_spied'(G,Module) -> CSPY = '*' ; CSPY = ' '),
		( SL = L -> SLL = '>' ; SLL = ' '),
	        ( recorded('$debug',on, R), erase(R), fail ; true),
		( Module\=prolog,
		  Module\=user ->
		    '$format'(user_error,"~a~a (~d) ~q: ~a:",[CSPY,SLL,L,P,Module])
		    ;
		    '$format'(user_error,"~a~a (~d) ~q:",[CSPY,SLL,L,P])
		),
		'$debugger_write'(user_error,G),
	        ( nonvar(R0), recordaifnot('$debug',on,_), fail ; true),
		( 
		  '$unleashed'(P),
		  '$action'(10,P,L,G,Module)
		 ;
		  write(user_error,' ? '), get0(user_input,C),
		  '$action'(C,P,L,G,Module)
		),
		!.

'$unleashed'(call) :- get_value('$leash',L), L /\ 2'1000 =:= 0.
'$unleashed'(exit) :- get_value('$leash',L), L /\ 2'0100 =:= 0.
'$unleashed'(redo) :- get_value('$leash',L), L /\ 2'0010 =:= 0.
'$unleashed'(fail) :- get_value('$leash',L), L /\ 2'0001 =:= 0.
% the same as fail.
'$unleashed'(exception(_)) :- get_value('$leash',L), L /\ 2'0001 =:= 0.

'$debugger_write'(Stream, G) :-
	recorded('$print_options','$debugger'(OUT),_), !,
	write_term(Stream, G, OUT).
'$debugger_write'(Stream, G) :-
	writeq(Stream, G).

'$action'(10,_,_,_,_) :- 			% newline 	creep
	( recorded('$spy_skip',_,R), erase(R), fail ; true ),
	'$set_yap_flags'(10,1).
'$action'(33,_,_,_,_) :- !,			% ! g		execute
	read(user,G),
	% don't allow yourself to be caught by creep.
	'$access_yap_flags'(10, CL),
	'$set_yap_flags'(10, 0),
	( '$execute'(G) -> true ; true),
	'$set_yap_flags'(10, CL),
	!,
	'$skipeol'(33),
	fail.
'$action'(0'<,_,_,_,_) :- !,			% <Depth
	'$new_deb_depth',
	'$skipeol'(0'<),
	fail.
'$action'(0'^,_,_,G,_) :- !,
	'$print_deb_sterm'(G),
	'$skipeol'(0'^),
	fail.
'$action'(0'a,_,_,_,_) :- !,			% a		abort
	'$skipeol'(0'^),
	abort.
'$action'(0'b,_,_,_,_) :- !,			% b		break
	'$skipeol'(0'b),
	break,
	fail.
'$action'(0'c,_,_,_,_) :- !,			% c		creep
	'$set_yap_flags'(10,1),
	'$skipeol'(0'c).
'$action'(0'e,_,_,_,_) :- !,			% e		exit
	'$skipeol'(0'e),
	halt.
'$action'(0'f,_,CallId,_,_) :- !,		% f		fail
	'$scan_number'(0'f, CallId, GoalId),
	throw('$fail_spy'(GoalId)).
'$action'(0'h,_,_,_,_) :- !,			% h		help
	'$action_help',
	'$skipeol'(104),
	fail.
'$action'(0'?,_,_,_,_) :- !,			% ?		help
	'$action_help',
	'$skipeol'(104),
	fail.
'$action'(0'p,_,_,G,Module) :- !,		% p		print
	((Module = prolog ; Module = user) ->
	    print(user_error,G), nl(user_error)
	;
	    print(user_error,Module:G), nl(user_error)
	),
	'$skipeol'(0'p),
	fail.
'$action'(0'd,_,_,G,Module) :- !,		% d		display
	((Module = prolog ; Module = user) ->
	    display(user_error,G), nl(user_error)
	;
	    display(user_error,Module:G), nl(user_error)
	),
	'$skipeol'(0'd),
	fail.
'$action'(0'l,_,CallNumber,_,_) :- !,		% l		leap
	'$skipeol'(0'l),
	( recorded('$spy_skip',_,R), erase(R), fail ; recorda('$spy_skip',CallNumber,_) ),
	( recordaifnot('$spy_stop',on,_) -> true ; true ),
	'$set_yap_flags'(10,1).
'$action'(0'n,_,_,_,_) :- !,			% n		nodebug
	'$skipeol'(0'n),
	'$set_yap_flags'(10,0),
	( recorded('$spy_stop',_,R), erase(R), fail ; true),
	nodebug.
'$action'(0'k,_,CallNumber,_,_) :- !,		% k		quasi leap
	'$skipeol'(0'k),
	( recorded('$spy_skip',_,R), erase(R), fail ; recorda('$spy_skip',CallNumber,_) ),
	( recordaifnot('$spy_stop',on,_) -> true ; true ),
	'$set_yap_flags'(10,0).
	% skip first call (for current goal),
	% stop next time.
'$action'(0'r,P,CallId,_,_) :- !,		% r		retry
	'$scan_number'(0'r,CallId,ScanNumber),
	throw('$retry_spy'(ScanNumber)).
'$action'(0's,P,CallNumber,_,_) :- !,		% s		skip
	'$skipeol'(0's),
	( (P=call; P=redo) ->
		'$set_yap_flags'(10,1),
		( recorded('$spy_skip',_,R), erase(R), fail ; recorda('$spy_skip',CallNumber,_) ),
		( recorded('$spy_stop',_,R), erase(R), fail ; true)
	;
	    '$ilgl'(0's)
	).
'$action'(0't,P,CallNumber,_,_) :- !,		% t		fast skip
	'$skipeol'(0't),
	( (P=call; P=redo) ->
		( recorded('$spy_skip',_,R), erase(R), fail ; recorda('$spy_skip',CallNumber,_) ),
		( recorded('$spy_stop',_,R), erase(R), fail ; true),
	        '$set_yap_flags'(10,0)
	    ;
	        '$ilgl'(0't)
	).
'$action'(0'+,_,_,G,M) :- !,			% +		spy this
	functor(G,F,N), spy(M:(F/N)),
	'$skipeol'(0'+),
	fail.
'$action'(0'-,_,_,G,M) :- !,			% -		nospy this
	functor(G,F,N), nospy(M:(F/N)),
	'$skipeol'(0'-),
	fail.
'$action'(C,_,_,_,_) :-
	'$skipeol'(C),
	'$ilgl'(C),
	fail.

% if we are in the interpreter, don't need to care about forcing a trace, do we?
'$continue_debugging'(_) :-
	recorded('$trace',on, _),
	fail.
'$continue_debugging'(no) :- !.
'$continue_debugging'(_) :-
	'$access_yap_flags'(10,1), !,
	'$creep'.
'$continue_debugging'(_) :-
	recorded('$spy_stop', _, _).

'$stop_debugging' :-
	'$stop_creep'.

'$action_help' :-
	'$format'(user_error,"newline  creep       a       abort~n", []),
	'$format'(user_error,"c        creep       e       exit~n", []),
	'$format'(user_error,"f Goal   fail        h       help~n", []),
	'$format'(user_error,"l        leap        r Goal  retry~n", []),
	'$format'(user_error,"s        skip        t       fastskip~n", []),
	'$format'(user_error,"q        quasiskip   k       quasileap~n", []),
	'$format'(user_error,"b        break       n       no debug~n", []),
	'$format'(user_error,"p        print       d       display~n", []),
	'$format'(user_error,"<D       depth D     <       full term~n", []),
	'$format'(user_error,"+        spy this    -       nospy this~n", []),
	'$format'(user_error,"^        view subg   ^^      view using~n", []),
	'$format'(user_error,"! g execute goal~n", []).
	
'$ilgl'(C) :-
	'$print_message'(warning, trace_command(C)),
	'$print_message'(help, trace_help),
	fail.

'$skipeol'(10) :- !.
'$skipeol'(_) :- get0(user,C), '$skipeol'(C).

'$scan_number'(_, _, Nb) :-
	get0(user,C),
	'$scan_number2'(C, Nb), !.
'$scan_number'(_, CallId, CallId).

'$scan_number2'(10, _) :- !, fail.
'$scan_number2'(0' , Nb) :- !,
	get0(user,C),
	'$scan_number2'(C , Nb).
'$scan_number2'(0'	, Nb) :- !,
	get0(user,C),
	'$scan_number2'(C, Nb).
'$scan_number2'(C, Nb) :-
	'$scan_number3'(C, 0, Nb).

'$scan_number3'(10,  Nb, Nb) :- !, Nb > 0.
'$scan_number3'( C, Nb0, Nb) :-
	C >= 0'0, C =< 0'9,
	NbI is Nb0*10+(C-0'0),
	get0(user, NC),
	'$scan_number3'( NC, NbI, Nb).
	
'$print_deb_sterm'(G) :-
	'$get_sterm_list'(L), !,
	'$deb_get_sterm_in_g'(L,G,A),
	recorda('$debug_sub_skel',L,_),
	'$format'(user_error,"~n~w~n~n",[A]).
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

