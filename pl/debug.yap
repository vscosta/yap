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
	throw(error(instantiation_error,M:spy(V,S))).
'$suspy'((M:S),P,_) :- !,
    '$suspy'(S,P,M).
'$suspy'([],_,_) :- !.
'$suspy'([F|L],S,M) :- !, ( '$suspy'(F,S,M) ; '$suspy'(L,S,M) ).
'$suspy'(F/N,S,M) :- !, functor(T,F,N),
    ( '$system_predicate'(T,M) ->
	 throw(error(permission_error(access,private_procedure,F/N),spy(F/N,S)));
	'$undefined'(T,M) ->
	 throw(error(existence_error(procedure,F/N),spy(F/N,S)));
	 '$suspy2'(S,F,N,T,M) ).
'$suspy'(A,S,_) :- \+ atom(A) , !, 
        throw(error(type_error(predicate_indicator,A),spy(A,S))).
'$suspy'(A,spy,M) :- '$noclausesfor'(A,M), !,
	throw(error(existence_error(procedure,A),spy(A))).
'$suspy'(A,nospy,M) :- '$noclausesfor'(A,M), !,
	throw(error(existence_error(procedure,A),nospy(A))).
'$suspy'(A,S,M) :- current_predicate(A,M:T),
	\+ '$undefined'(T,M), \+ '$system_predicate'(T,M),
	 functor(T,F,N),
	'$suspy2'(S,F,N,T,M).

'$noclausesfor'(A,M) :- current_predicate(A,M:T),
	\+ '$undefined'(T,M) , \+ '$system_predicate'(T,M) ,
	!, fail .
'$noclausesfor'(_,_).

'$suspy2'(spy,F,N,T,M) :- 
	'$recorded'('$spy','$spy'(T,M),_), !,
	'$format'(user_error, "[ Warning: there is already a spy point on ~w:~w/~w ]~n",[M,F,N]).
'$suspy2'(spy,F,N,T,M) :- !,
	'$warn_if_undef'(T,F,N,M),
	'$recorda'('$spy','$spy'(T,M),_), 
	'$set_value'('$spypoint_added', true), 
	'$set_spy'(T,M),
	'$format'(user_error,"[ Spy point set on ~w:~w/~w ]~n", [M,F,N]).
'$suspy2'(nospy,F,N,T,M) :- 
	'$recorded'('$spy','$spy'(T,M),R), !,
	erase(R),
	'$rm_spy'(T,M),
	'$format'(user_error,"[ Spy point on ~w:~w/~w removed ]~n", [M,F,N]).
'$suspy2'(nospy,F,N,_,M) :-
	'$format'(user_error,"[ Warning: there is no spy point on ~w:~w/~w ]~n", [M,F,N]).

'$warn_if_undef'(T,F,N,M) :-  '$undefined'(T,M), !,
	write(user_error,'[ Warning: you have no clauses for '),
	write(user_error,M:F/N), write(user_error,' ]'), nl(user_error).
'$warn_if_undef'(_,_,_,_).

'$pred_being_spied'(G, M) :-
	'$recorded'('$spy','$spy'(G,M),_), !.

spy _ :- '$set_value'('$spypoint_added', false), fail.
spy L :-
	'$current_module'(M),
	'$suspy'(L, spy, M), fail.
spy _ :- '$get_value'('$spypoint_added', false), !.
spy _ :- debug.

nospy L :-
	'$current_module'(M),
	'$suspy'(L, nospy, M), fail.
nospy _.

nospyall :-
	'$recorded'('$spy','$spy'(T,M),_), functor(T,F,N), '$suspy'(F/N,nospy,M), fail.
nospyall.

% debug mode -> debug flag = 1

debug :- '$get_value'(debug,1), !.
debug :- '$set_value'(debug,1), write(user_error,'[ Debug mode on ]'), nl(user_error).

nodebug :- nospyall,
	'$set_value'(debug,0),
	'$set_value'('$trace',0),
	'$format'(user_error,"[ Debug mode off ]~n",[]).

trace :- '$get_value'('$trace',1), !.
trace :-
	'$format'(user_error,"[ Trace mode on ]~n",[]),
	'$set_value'('$trace',1),
	'$set_value'(debug,1),
	'$set_value'(spy_sl,0),
	% start creep,
	'$set_yap_flags'(10,1),
	'$creep'.

notrace :- 
	'$set_value'('$trace',0),
	'$set_value'(debug,0),
	'$format'(user_error,"[ Trace and Debug mode off ]",[]).

/*-----------------------------------------------------------------------------

				leash

-----------------------------------------------------------------------------*/


leash(X) :- var(X),
	throw(error(instantiation_error,leash(X))).
leash(X) :- '$leashcode'(X,Code),
	'$set_value'('$leash',Code),
	'$show_leash'(Code), !.
leash(X) :-
	throw(error(type_error(leash_mode,X),leash(X))).

'$show_leash'(0) :- write(user_error,'[ No leashing ]'), nl(user_error).
'$show_leash'(L) :-
	'$leashcode'(Code,L),
	write(user_error,'[ Leashing set to '), write(user_error,Code),
	write(user_error,' ('),
	'$show_leash_bit'(WasWritten,2'1000,L,call),
	'$show_leash_bit'(WasWritten,2'0100,L,exit),
	'$show_leash_bit'(WasWritten,2'0010,L,redo),
	'$show_leash_bit'(WasWritten,2'0001,L,fail),
	write(user_error,') ]'), nl(user_error).

'$show_leash_bit'(_,Bit,Code,_) :- Bit /\ Code =:= 0, !.
'$show_leash_bit'(Was,_,_,Name) :- var(Was), !,
	Was = yes, write(user_error,Name).
'$show_leash_bit'(_,_,_,Name) :-
	write(user_error,','), write(user_error,Name).

'$leashcode'(full,2'1111) :- !.
'$leashcode'(on,2'1111) :- !.
'$leashcode'(half,2'1010) :- !.
'$leashcode'(loose,2'1000) :- !.
'$leashcode'(off,2'0000) :- !.
%'$leashcode'([L|M],Code) :- !, '$leashcode_list'([L|M],Code).
'$leashcode'([L|M],Code) :- !, ( var(Code) -> '$list2Code'([L|M],Code)
					    ; '$code2List'(Code,[L|M]) ).
'$leashcode'(N,N) :- integer(N), N >= 0, N =< 2'1111.

'$list2Code'(V,_) :- var(V), !,
	throw(error(instantiation_error,leash(V))).
'$list2Code'([],0) :- !.
'$list2Code'([V|L],_) :- var(V), !,
	throw(error(instantiation_error,leash([V|L]))).
'$list2Code'([call|L],N) :- '$list2Code'(L,N1), N is 2'1000 + N1.
'$list2Code'([exit|L],N) :- '$list2Code'(L,N1), N is 2'0100 + N1.
'$list2Code'([redo|L],N) :- '$list2Code'(L,N1), N is 2'0010 + N1.
'$list2Code'([fail|L],N) :- '$list2Code'(L,N1), N is 2'0001 + N1.

'$code2List'(0,[]) :- !.
'$code2List'(N,[call|L]) :- X is N /\ 2'1000, X \= 0, !,
	M is N-X, '$code2List'(M,L).
'$code2List'(N,[exit|L]) :- X is N /\ 2'0100, X \= 0, !,
	M is N-X, '$code2List'(M,L).
'$code2List'(N,[redo|L]) :- X is N /\ 2'0010, X \= 0, !,
	M is N-X, '$code2List'(M,L).
'$code2List'(N,[fail|L]) :- X is N /\ 2'0001, X \= 0, !,
	M is N-X, '$code2List'(M,L).

/*-----------------------------------------------------------------------------

				debugging

-----------------------------------------------------------------------------*/

debugging :-
	'$get_value'(debug,1) ->
		write(user_error,'[ Debug mode is switched on ]') ,
		nl(user_error),
		'$debugging_mode'
		;
		write(user_error,'[ Debug mode is switched off ]') ,
		nl(user_error)
	.
	
'$debugging_mode' :-
	( '$recorded'('$spy',_,_) -> '$show_spies' ;
		   write(user_error,'[ Warning: there are no spy-points set ]') ,
		   nl(user_error) ),
	'$get_value'('$leash',Leash),
	'$show_leash'(Leash).

'$show_spies' :-
	write(user_error,'[ Spy points set on :'), nl(user_error),
	( '$recorded'('$spy','$spy'(T,M),_), functor(T,F,N),
		write(user_error,'        '),write(user_error,M:F/N),nl(user_error),
		fail ;
	  write(user_error,' ]'), nl(user_error) ).


/*-----------------------------------------------------------------------------

				spy

-----------------------------------------------------------------------------*/


% This is executed before from the abstract
% machine when the spy_flag is on

% these flags are used

%	flag		description	initial/possible values

%	spy_gn		goal number	1	1...
%	spy_creep	creep		0	0, 1
%	spy_sl  	skip level	0	0...
%	spy_sp		skip port	0	0, call, fail, redo, exit
%	spy_leap	leap		0	0...
%	spy_cl		clause number	1	1...
%	spy_fs		fast skip	0	0, 1
%	spy_trace	trace		0	0, 1
% a flip-flop is also used
%	when 1 spying is enabled
%'$spy'(G) :- write(user_error,'$spy'(G)), nl, fail.
%
% handle suspended goals
% take care with hidden goals.
%
% $spy may be called from user code, so be careful.
'$spy'(G) :-
	'$awoken_goals'(LG), !,
	'$creep',
	'$wake_up_goal'(G, LG).
'$spy'([_|Mod:G]) :- !,
	'$spy'([Mod|G]).
'$spy'([Module|G]) :-
%    '$format'(user_error,"$spym(~w,~w)~n",[Module,G]),
         '$hidden'(G),
         !,
	 /* called from prolog module   */
	 '$execute0'(G,Module),
 	 '$creep'.
'$spy'([Mod|G]) :-
	'$do_spy'(G,Mod).


'$direct_spy'(G) :-
	'$awoken_goals'(LG), !,
	'$creep',
	'$wake_up_goal'(G, LG).
'$direct_spy'([M|G]) :-
         '$hidden'(G),
         !,
	 (
	   G = '$leave_creep'
	 ->
	   true
	 ;
	   /* called from prolog module   */
	   '$execute0'(G,M),
	   '$creep'
	 ).
'$direct_spy'([Mod|G]) :-
	'$do_spy'(G, Mod).


'$do_spy'(true, _) :- !, '$creep'.
'$do_spy'('$cut_by'(M), _) :- !, '$cut_by'(M).
'$do_spy'(G, Module) :-
%   write(user_error,$spy(G)), nl,
    '$get_value'(debug,1),		/* ditto if debug off		*/
    '$get_value'(spy_fs,0),		/* ditto if fast skipping	*/
    ( '$access_yap_flags'(10,0) ->	/* if not creeping ...		*/
	      '$pred_being_spied'(G,Module)  /* ... spy only if at a spy-point */
	; true
    ),
%    ( \+ '$undefined'(user_error_spy(_), user) -> user_error_spy(G) ;
%	true );
    !,					/* you sure want to spy this ... */
    '$get_value'(spy_gn,L),		/* get goal no.			*/
    L1 is L+1,				/* bump it			*/
    '$set_value'(spy_gn,L1),		/* and save it globaly		*/
    '$access_yap_flags'(10,SC),
    '$set_yap_flags'(10,1),		/* set creep on			*/
    '$get_value'(spy_cl,CL),		/* save global clause no.	*/
    repeat,		/* we need this to be able to implement retry	*/
	'$init_spy_cl'(G,Module),
	'$trace'(call,G,Module,L,CF),	/* inform about call port	*/
	/* the following choice point is where the predicate is  called */
	( '$get_value'(spy_sp,0),	/* make sure we are not skipping*/
	  '$system_catch'('$spycalls'(G,Module,Res), Module,
			 Error,
			  prolog:'$DebugError'(Error))
			 /* go execute the predicate	*/
	   ;	/* we get here when the predicate fails */
	        ( '$get_value'(spy_sl, -1) ->
		    '$trace'(exception,G,Module,L,CF)
		    ;
		    '$trace'(fail,G,Module,L,CF) /* inform at fail port		*/
		),
		'$get_value'(spy_sl,L2),/* make sure we are not ...	*/
		L2 \= L,		/* ... skiping to this level	*/
		!,			/* if not prepare to exit spy	*/
	        '$set_value'(spy_cl,CL),/* restore global value of clause no */
		'$setflop'(0), 
		'$set_creep'(SC),	/* restore creep value		*/
		'$cont_creep'(CF), fail ),	/* and exit			*/
	'$get_value'(spy_cl,Cla),	/* save no. of clause to try	*/
	( var(Res),			/* check not redoing		*/
	  '$trace'(exit,G,Module,L,CF),	/* output message at exit	*/
	  '$get_value'(spy_sp,0),	/* check not skipping		*/
	  '$set_value'(spy_cl,CL),	/* restore clause no.		*/
	  '$set_creep'(SC),		/* restore creep value		*/
	  '$setflop'(0),
	  '$cont_creep'(CF);		/* exit				*/
		/* we get here when we want to redo a goal		*/
		'$set_value'(spy_cl,Cla),/* restore clause no. to try	*/
		'$trace'(redo,G,Module,L,_), /* inform user_error		*/
		fail			/* to backtrack to spycalls	*/
	).
'$do_spy'(G,Mod) :-
	'$execute0'(G,Mod).	/* this clause applies when we do not want
				   to spy the goal			*/

'$cont_creep'( _) :-  '$get_value'('$trace',1), '$set_yap_flags'(10,1), fail.
'$cont_creep'(CF) :- nonvar(CF), !, '$set_yap_flags'(10,1), '$creep'.
'$cont_creep'( _) :- '$access_yap_flags'(10,1), !, '$creep'.
'$cont_creep'( _).

'$set_creep'(0) :- !, '$set_yap_flags'(10,0).
'$set_creep'(_).

%'$spycalls'(G,_) :- write(user_error,'$spycalls'(G)), nl(user_error), fail.
'$spycalls'([_|_],_,_) :- !, fail.
'$spycalls'('!'(CP),Mod,_) :-
	'$call'(!, CP, !,Mod).
'$spycalls'(Mod:G,_,Res) :-
	!,
	'$spycalls'(G,Mod,Res).
'$spycalls'(repeat,_,_) :-
	!,
	repeat.
'$spycalls'(fail,_,_) :-
	!,
	fail.
'$spycalls'(false,_,_) :-
	!,
	false.
'$spycalls'(true,_,_) :-
	!.
'$spycalls'(otherwise,_,_) :-
	!.
'$spycalls'(\+ G,Mod,Res) :-
	!,
	CP is '$last_choice_pt',
	'$spycalls'('$call'((\+ G), CP, (\+ G),Mod),Mod,Res).
'$spycalls'(not(G),Mod,Res) :-
	!,
	CP is '$last_choice_pt',
	'$spycalls'('$call'(not(G), CP, not(G),Mod),Mod,Res).
'$spycalls'(G,M,Res) :-                                % undefined predicate
	'$undefined'(G, M), !,
	functor(G,F,N),
	( '$recorded'('$import','$import'(S,M,F,N),_) ->
	  '$spycalls'(G,S,Res) ;
	  '$undefp'([M|G])
	).
'$spycalls'(G,M,_) :-
	'$system_predicate'(G,M),
	'$flags'(G,M,F,_),
	F /\ 0xc00000 =:= 0,		% but not meta-predicate or cut transparent
	!,
	'$spycall_stdpred'(G,M),
	(true;
		'$get_value'(spy_sp,P), P \= 0, !, fail),
	( true;
		'$get_value'(spy_sp,P1), P1 \= 0, !, fail)
	.
'$spycalls'(G,M,Res) :-		% asserts and retracts can complicate live
	( '$get_value'(spy_sp,0) -> true ; !, fail ),
	'$flags'(G,M,F,F),
	F /\ 16'2000 =\= 0, !, % dynamic procedure, immediate semantics
	repeat,
	        '$db_last_age'(M:G,Max),	
		'$get_value'(spy_cl,Cl),
		'$get_value'(spy_gn,L),
		Maxx is Max+1,
		'$set_value'(spy_cl,Maxx),
		( Cl > Max -> !, fail ; true ),
		( '$spycall_dynamic'(G,M,Cl) ;
			('$get_value'(spy_gn,L) -> '$leave_creep', fail ;
			  Res = redo )
		),
		( true ;
		  '$get_value'(spy_sp,P), P \= 0, !, fail )
	.
'$spycalls'(G,M,Res) :-
	( '$get_value'(spy_sp,0) -> true ; !, fail ),
	'$flags'(G,M,F,F),
	F /\ 16'8 =\= 0, !, % dynamic procedure, logical update semantics
	'$hold_index'(M:G, Index, Max), % hold an index on the procedure state when we called this goal
	repeat,
		'$get_value'(spy_cl,Cl),
		'$get_value'(spy_gn,L),
		Maxx is Max+1,
		'$set_value'(spy_cl,Maxx),
		( Cl > Max -> !, fail ; true),
		( '$log_upd_spycall'(G,M,Cl,Index) ;
			('$get_value'(spy_gn,L) ->
				'$leave_creep', fail ; % to backtrack to repeat
				Res = redo )
		),
		( true ;
		  '$get_value'(spy_sp,P), P \= 0, !, fail
	      )
	.
'$spycalls'(G,M,Res) :-
	( '$get_value'(spy_sp,0) -> true ; !, fail ),
	repeat,
	        '$number_of_clauses'(G,M,Max),
		'$get_value'(spy_cl,Cl),
		'$get_value'(spy_gn,L),
		Maxx is Max+1,
		'$set_value'(spy_cl,Maxx),
		( Cl > Max -> !, fail ; true),
		( '$spycall'(G,M,Cl) ;
			('$get_value'(spy_gn,L) ->
				'$leave_creep', fail ; % to backtrack to repeat
				Res = redo )
		),
		( true ;
		  '$get_value'(spy_sp,P), P \= 0, !, fail )
	.

'$spycall'(G,M,Cl) :-
	'$access_yap_flags'(10,0),
	!,
	'$setflop'(0),
	'$do_execute_clause'(G,M,Cl).
'$spycall'(G,M,Cl) :-
	'$setflop'(0),
	'$do_creep_execute'(G,M,Cl),
	'$leave_creep'.

'$log_upd_spycall'(G,M,Cl,Index) :-
	'$access_yap_flags'(10,0),
	!,
	'$setflop'(0),
	'$do_execute_log_upd_clause'(G,M,Cl,Index).
'$log_upd_spycall'(G,M,Cl,Index) :-
	'$setflop'(0),
	'$do_execute_log_upd_clause'(G,M,Cl,Index).

% this is to be used only for dynamic predicates
'$spycall_dynamic'(G,M,Cl) :-
	'$access_yap_flags'(10,0),
	!,
	'$setflop'(0),
	'$do_execute_dynamic_clause'(G,M,Cl).
'$spycall_dynamic'(G,M,Cl) :-
	'$setflop'(0),
	'$do_creep_execute_dynamic'(G,M,Cl),
	'$leave_creep'.

'$spycall_stdpred'(G,M) :-
        CP is '$last_choice_pt',
	functor(G,F,N),
	(
	    user:'$meta_predicate'(F,M,N,_) ->
	    '$setflop'(1),
	    '$creep',
	    % I need to use call, otherwise I'll be in trouble if G
	    % is a meta-call.
	    '$call'(G,CP,G,M)
	;
	    '$setflop'(1),
	    '$call'(G,CP,G,M)
	),
	'$setflop'(0).


'$do_execute_clause'(G,M,Cl) :-
	'$some_recordedp'(M:G), !,
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_),prolog) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    '$fetch_clause'(G,M,Cl,Clause),
	    (Clause = true -> true ; '$call'(Clause,CP,Clause,M) )
	;
	     Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).
'$do_execute_clause'(G,M,Cl) :-
	'$execute'(G,M,Cl) ; Next is Cl+1, '$set_value'(spy_cl,Next), fail.

'$do_execute_log_upd_clause'(G,M,Cl,Index) :-
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_),prolog) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    (Index = [] ->
               /* We have a single clause */
	       Cl = 1,
               '$clause'(G, M, Clause)
            ;
	      Cl1 is Cl-1,
	      '$fetch_reference_from_index'(Index, Cl1, Ref),
              instance(Ref, (G :- Clause))
            ),
	    (Clause = true -> true ; '$call'(Clause,CP,Clause,M) )
	;
	     Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).

'$check_depth_for_interpreter'(10000000) :-
	'$undefined'(get_depth_limit(_), prolog), !.
'$check_depth_for_interpreter'(D1) :-
	get_depth_limit(D0),
	D0 =\= 0,
	D1 is D0-1.

'$do_execute_dynamic_clause'(G,M,Cl) :-
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_),prolog) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    '$db_nb_to_ref'(Cl,M:G,Ref),
	    instance(Ref, (G :- Clause)),
	    (Clause = true -> true ; '$call'(Clause,CP,Clause,M) )
	;
	    Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).

'$do_creep_execute'(G,M,Cl) :-
	 % fast skip should ignore source mode
	'$get_value'(spy_fs,0),
	'$some_recordedp'(M:G),
	 !,
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_),prolog) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    '$fetch_clause'(G,M,Cl,Clause),
	    (Clause = true -> true ;
             '$creep_call'(Clause,M,CP)
            )
	;
	    Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).
'$do_creep_execute'(G,M,Cl) :-
	'$creep_execute'(G,M,Cl), '$leave_creep' ; 
	'$leave_creep', Next is Cl+1, '$set_value'(spy_cl,Next), fail.

'$do_creep_log_upd_execute'(G,M,Cl,Index) :-
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_),prolog) -> true ; '$set_depth_limit'(D)),
        ( CP is '$last_choice_pt',
	  (Index = [] ->
             /* We have a single clause */
	     Cl = 1,
             '$clause'(G, M, Clause)
            ;
	    Cl1 is Cl-1,
	    '$fetch_reference_from_index'(Index, Cl1, Ref),
	    instance(Ref, (G :- Clause))
	  ),
          (Clause = true -> true ;
		% otherwise fast skip may try to interpret assembly builtins.
             '$get_value'(spy_fs,1) -> '$call'(Clause,CP,Clause,M) ;
             '$creep_call'(Clause,M,CP)
            )
	;
	    Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).

'$do_creep_execute_dynamic'(G,M,Cl) :-
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_),prolog) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    '$db_nb_to_ref'(Cl,M:G,Ref),
	    instance(Ref, (G :- Clause)),
	    (Clause = true -> true ;
	     % otherwise fast skip may try to interpret assembly builtins.
             '$get_value'(spy_fs,1) -> '$call'(Clause,CP,Clause,M) ;
             '$creep_call'(Clause,M,CP)
            )
	;
	     Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).

'$leave_creep'.

'$creep_execute'(G,M,Cl) :-
	'$creep',
	'$execute'(G,M,Cl).

'$fetch_clause'(G,M,ClNum,Body) :-
	% I'd like an easier way to keep a counter
	'$set_value'('$fetching_clauses',1),
	'$recordedp'(M:G,Clause,_),
	'$get_value'('$fetching_clauses',Num),
	( Num = ClNum ->
	    !,
	    Clause = (G :- Body)
	;
	    Num1 is Num+1,
	    '$set_value'('$fetching_clauses',Num1),
	    fail
	).


%'$creep_call'(G,_) :- write(user_error,'$creepcall'(G)), nl(user_error), fail.
'$creep_call'(V,M,_) :- var(V), !,
	throw(error(instantiation_error,meta_call(M:V))).
'$creep_call'(A,M,_) :- number(A), !,
	throw(error(type_error(callable,A),meta_call(M:A))).
'$creep_call'(R,M,_) :- db_reference(R), !,
	throw(error(type_error(callable,R),meta_call(M:R))).
'$creep_call'(M:G,_,CP) :- !,
        '$creep_call'(G,M,CP).
'$creep_call'(fail,Module,_) :- !,
	'$direct_spy'([Module|fail]).
'$creep_call'(false,Module,_) :- !,
	'$direct_spy'([Module|false]).
'$creep_call'(true,Module,_) :- !,
	'$direct_spy'([Module|true]).
'$creep_call'(otherwise,Module,_) :- !,
	'$direct_spy'([Module|otherwise]).
'$creep_call'((A,B),Module,CP) :- !,
	'$creep_call'(A,Module,CP), '$creep_call'(B,Module,CP).
'$creep_call'((X->Y; Z),Module,CP) :- !,
	( '$creep_call'(X,Module,CP), !, '$creep_call'(Y,Module,CP); '$creep_call'(Z,Module,CP)).
'$creep_call'((A;B),Module,CP) :- !,
	('$creep_call'(A,Module,CP) ; '$creep_call'(B,Module,CP)).
'$creep_call'((A|B),Module,CP) :- !,
	('$creep_call'(A,Module,CP) ; '$creep_call'(B,Module,CP)).
'$creep_call'(atom(A),Module,_) :- !,
	'$direct_spy'([Module|atom(A)]).
'$creep_call'(atomic(A),Module,_) :- !,
	'$direct_spy'([Module|atomic(A)]).
'$creep_call'(integer(A),Module,_) :- !,
	'$direct_spy'([Module|integer(A)]).
'$creep_call'(nonvar(A),Module,_) :- !,
	'$direct_spy'([Module|nonvar(A)]).
'$creep_call'(var(A),Module,_) :- !,
	'$direct_spy'([Module|var(A)]).
'$creep_call'(number(A),Module,_) :- !,
	'$direct_spy'([Module|number(A)]).
'$creep_call'(prismitive(A),Module,_) :- !,
	'$direct_spy'([Module|primitive(A)]).
'$creep_call'(compound(A),Module,_) :- !,
	'$direct_spy'([Module|compound(A)]).
'$creep_call'(float(A),Module,_) :- !,
	'$direct_spy'([Module|float(A)]).
'$creep_call'(db_reference(A),Module,_) :- !,
	'$direct_spy'([Module|db_reference(A)]).
'$creep_call'(\+ X,Module,_) :- !,
	'$direct_spy'([Module|(\+ X)]).
'$creep_call'(not X,Module,_) :- !,
	'$direct_spy'([Module|not(X)]).
'$creep_call'(X=Y,Module,_) :- !,
	'$direct_spy'([Module|X=Y]).
'$creep_call'(X\=Y,Module,_) :- !,
	'$direct_spy'([Module|X\=Y]).
'$creep_call'(X==Y,Module,_) :- !,
	'$direct_spy'([Module|X==Y]).
'$creep_call'(X>Y,Module,_) :- !,
	'$direct_spy'([Module|X>Y]).
'$creep_call'(X>=Y,Module,_) :- !,
	'$direct_spy'([Module|X>=Y]).
'$creep_call'(X<Y,Module,_) :- !,
	'$direct_spy'([Module|X<Y]).
'$creep_call'(X=<Y,Module,_) :- !,
	'$direct_spy'([Module|X=<Y]).
'$creep_call'(X=:=Y,Module,_) :- !,
	'$direct_spy'([Module|X=:=Y]).
'$creep_call'(X=\=Y,Module,_) :- !,
	'$direct_spy'([Module|X=\=Y]).
'$creep_call'(arg(X,Y,Z),Module,_) :- !,
	'$direct_spy'([Module|arg(X,Y,Z)]).
'$creep_call'(functor(X,Y,Z),Module,_) :- !,
	'$direct_spy'([Module|functor(X,Y,Z)]).
'$creep_call'((X->Y),Module,CP) :- !,
	CP1 is '$last_choice_pt',
	'$creep_call'(X,Module,CP),
	'$$cut_by'(CP1),
	'$creep_call'(Y,Module,CP).
'$creep_call'(!,Module,CP) :- !,
	'$direct_spy'([Module|'!'(CP)]),
	% clean up any garbage left here by the debugger.
	'$$cut_by'(CP).
'$creep_call'('$cut_by'(X),_,_) :- !,
	'$$cut_by'(X).
'$creep_call'(repeat,Module,_) :- !,
	'$direct_spy'([Module|repeat]).
'$creep_call'([A|B],Module,_) :- !,
	'$direct_spy'([Module|[A|B]]).
'$creep_call'(A,Module,CP) :-
	'$undefined'(A,Module),
	functor(A,F,N),
	'$recorded'('$import','$import'(S,Module,F,N),_), !, 
	'$creep_call'(A,S,CP).
'$creep_call'(A,Module,_) :-
	'$direct_spy'([Module|A]).

%'$creep'(G) :- $current_module(M),write(user_error,[creep,M,G]),nl(user_error),fail.
% skip calls to assembly versions of execute.
'$creep'(G) :-
	'$get_value'('$alarm', true), !,
	'$set_value'('$alarm', []),
	( '$recorded'('$alarm_handler',A,_) ->
	    '$execute'(A),
	    G=[M|Goal] 
	;
	    true
	),
	'$execute'(M:Goal).
'$creep'(G) :-
	'$get_value'('$sig_pending', Signals),
	Signals \== [], !,
	'$set_value'('$sig_pending', [] ),
	'$handle_signals'(Signals),
	G=[M|Goal],
	'$execute'(M:Goal).
'$creep'([M|V]) :- var(V), !,
	throw(error(instantiation_error,M:call(M:V))).
'$creep'([M|'$execute_in_mod'(G,ModNum)]) :- !,
	'$module_number'(Mod,ModNum),
	'$creep'([Mod|G]).
'$creep'([M|'$execute_within'(G)]) :- !,
	'$creep'([M|G]).
'$creep'([M|'$last_execute_within'(G)]) :- !,
	'$creep'([M|G]).
'$creep'(G) :- '$direct_spy'(G).

'$trace'(P,'!'(_),Mod,L,NC) :- !,
	'$trace'(P,!,Mod,L,NC).
'$trace'(P,G,Mod,L,NC) :-
	'$chk'(P,L,G,Mod,SL),
	'$msg'(P,G,Mod,L,SL,NC), !.
'$trace'(_,_,_,_,_).

'$handle_signals'([]).
'$handle_signals'([S|Rest]) :-
	'$recorded'('$sig_handler', action(S,A),_),
	'$execute'(A),
	'$handle_signals'(Rest).
'$handle_signals'([_|Rest]) :- '$handle_signals'(Rest).

'$msg'(P,G,Module,L,SL,NC):-
	flush_output(user_output),
	flush_output(user_error),
	'$get_value'(debug,1),
	repeat,
		('$pred_being_spied'(G,Module) -> write(user_error,'*') ; write(user_error,' ')),
		( SL = L -> write(user_error,'>') ; write(user_error,' ')),
		write(user_error,' ('), write(user_error,L), write(user_error,') '),
		write(user_error,P), write(user_error,': '),
		( Module\=prolog,
		  Module\=user -> write(user_error,Module),write(user_error,':');
		   true
		),
		'$debugger_write'(user_error,G),
		( 
		  '$unleashed'(P),
		  nl(user_error)
		  ;
		  write(user_error,' ? '), get0(user_input,C),
		  '$action'(C,P,L,G,NC),
		  '$skipeol'(C)
		),
		!.

'$unleashed'(call) :- '$get_value'('$leash',L), L /\ 2'1000 =:= 0.
'$unleashed'(exit) :- '$get_value'('$leash',L), L /\ 2'0100 =:= 0.
'$unleashed'(redo) :- '$get_value'('$leash',L), L /\ 2'0010 =:= 0.
'$unleashed'(fail) :- '$get_value'('$leash',L), L /\ 2'0001 =:= 0.
% the same as fail.
'$unleashed'(exception) :- '$get_value'('$leash',L), L /\ 2'0001 =:= 0.

'$debugger_write'(Stream, G) :-
	'$recorded'('$print_options','$debugger'(OUT),_), !,
	write_term(Stream, G, OUT).
'$debugger_write'(Stream, G) :-
	writeq(Stream, G).

/*
'$chk'(P,L,G,_) :-
	'$get_value'(spy_leap,Leap),
	'$get_value'(spy_sp,SP),
	'$access_yap_flags'(10,SC),
	'$get_value'(spy_sl,SL),
	'$get_value'(spy_fs,FS),
	'$get_value'(spy_cl,CL),
	write(user_error,[chk,L,P,Leap,SP,SC,SL,FS,CL,G]), nl(user_error),
	fail.
*/
'$chk'(_,_,[_|_],_,_) :- !, fail.
'$chk'(P,L,G,Mod,SL) :-
	'$get_value'(spy_leap,Leap),
	(Leap = 0 -> true;			      % not leaping
		('$pred_being_spied'(G,Mod) ; Leap = L),  % leaping or quasileaping
		'$set_value'(spy_leap,0) ),
	'$get_value'(spy_sp,SP),
	(SP = 0; SP = P),		% the current skipport or no skipport
	'$access_yap_flags'(10,SC),
	(SC = 1; '$pred_being_spied'(G,Mod)),
	'$get_value'(spy_sl,SL),
	(SL = 0; SL = L, '$set_value'(spy_sl,0), '$set_value'(spy_fs,0)),
	'$set_value'(spy_sp,0), !.

'$skipeol'(10) :- !.
'$skipeol'(_) :- get0(user,C), '$skipeol'(C).

'$action'(10,_,_,_,continue) :- !.		% newline 	creep
'$action'(33,_,_,_,_) :- !,		% ! g		execute
	read(user,G),
	% don't allow yourself to be caught by creep.
	'$access_yap_flags'(10, CL),
	'$set_yap_flags'(10, 0),
	( '$execute'(G) -> true ; true),
	'$set_yap_flags'(10, CL),
	!, fail.
'$action'(60,_,_,_,_) :- !,		% <Depth
	'$new_deb_depth',
	fail.
'$action'(94,_,_,G,_) :- !,
	'$print_deb_sterm'(G), fail.
'$action'(97,_,_,_,_) :- !, abort.	% a		abort
'$action'(98,_,_,_,_) :- !, break,	% b		break
	fail.
'$action'(99,call,_,_,_) :- !,		% c		creep
	'$set_yap_flags'(10,1),
	'$creep'.
'$action'(99,exit,_,_,continue) :- !.	% c		creep
'$action'(99,fail,_,_,continue) :- !,	% c		creep
	'$set_yap_flags'(10,1),
	'$creep'.
'$action'(101,_,_,_,_) :- !,		% e		exit
	halt.
'$action'(102,P,L,_,_) :- !,		% f		fail
	( \+ P = fail, !; '$ilgl'(102) ),
	'$set_value'(spy_sp,fail),
	'$set_value'(spy_sl,L).
'$action'(104,_,_,_,_) :- !,		% h		help
	'$action_help',
	'$skipeol'(104), fail.
'$action'(63,_,_,_,_) :- !,		% ?		help
	'$action_help',
	'$skipeol'(104), fail.
'$action'(112,_,_,G,_) :- !,		% p		print
	print(user_error,G), nl(user_error),
	'$skipeol'(112), fail.
'$action'(100,_,_,G,_) :- !,		% d		display
	display(user_error,G), nl(user_error),
	'$skipeol'(100), fail.
'$action'(113,_,L,_,_) :- !,		% k		quasi skip
	'$set_value'(spy_leap,L).
'$action'(108,_,_,_,_) :- !,		% l		leap
	'$set_value'(spy_leap,1).
'$action'(110,_,_,_,_) :- !,		% n		nodebug
	nodebug.
'$action'(107,_,_,_,_) :- !,		% k		quasi leap
	'$set_yap_flags'(10,0).
'$action'(114,P,L,_,_) :- !,		% r		retry
	( P=call, !, '$ilgl'(114); true),
	'$set_value'(spy_sp,call),
	'$set_value'(spy_sl,L),
	write(user_error,'[ retry ]'), nl(user_error).
'$action'(115,P,L,_,_) :- !,		% s		skip
	( P=call; P=redo; '$ilgl'(115) ), !,
	'$set_value'(spy_sl,L).
'$action'(116,P,L,_,_) :- !,		% t		fast skip
	( P=call; P=redo; '$ilgl'(116) ), !,
	'$set_value'(spy_sl,L), '$set_value'(spy_fs,1).
'$action'(43,_,_,G,_) :- !,		% +		spy this
	functor(G,F,N), spy(F/N),
	'$skipeol'(43), fail.
'$action'(45,_,_,G,_) :- !,		% -		nospy this
	functor(G,F,N), nospy(F/N),
	'$skipeol'(45), fail.
'$action'(C,_,_,_,_) :- '$ilgl'(C).


'$action_help' :-
	format(user_error,"newline  creep       a   abort~n", []),
	format(user_error,"c        creep       e   exit~n", []),
	format(user_error,"f        fail        h   help~n", []),
	format(user_error,"l        leap        r   retry~n", []),
	format(user_error,"s        skip        t   fastskip~n", []),
	format(user_error,"q        quasiskip   k   quasileap~n", []),
	format(user_error,"b        break       n   no debug~n", []),
	format(user_error,"p        print       d   display~n", []),
	format(user_error,"<D       depth D     <   full term~n", []),
	format(user_error,"+        spy this    -   nospy this~n", []),
	format(user_error,"^        view subg   ^^  view using~n", []),
	format(user_error,"! g execute goal~n").
	
'$ilgl'(C) :- '$skipeol'(C), write(user_error,'[ Illegal option. Use h for help. ]'),
	nl(user_error), fail.

'$print_deb_sterm'(G) :-
	'$get_sterm_list'(L), !,
	'$deb_get_sterm_in_g'(L,G,A),
	recorda('$debug_sub_skel',L,_),
	nl(user_error), write(user_error,A), nl(user_error), nl(user_error).
'$print_deb_sterm'(_) :- '$skipeol'(94).

'$get_sterm_list'(L) :-
	get0(user_input,C),
	'$deb_inc_in_sterm_oldie'(C,L0,CN),
	'$get_sterm_list'(L0,CN,0,L).

'$deb_inc_in_sterm_oldie'(94,L0,CN) :- !,
	get0(user_input,CN),
	( '$recorded'('$debug_sub_skel',L0,_) -> true ;
	  CN = [] ).
'$deb_inc_in_sterm_oldie'(C,[],C).

'$get_sterm_list'(L0,C,N,L) :-
	( C =:= "^", N \== 0 -> get0(CN),
				'$get_sterm_list'([N|L0],CN,0,L) ;
	  C >= "0", C =< "9" -> NN is 10*N+C-"0", get0(CN),
				'$get_sterm_list'(L0,CN,NN,L);
	  C =:= 10 -> (N =:= 0 -> L = L0 ; L=[N|L0]) ).

'$deb_get_sterm_in_g'([],G,G).
'$deb_get_sterm_in_g'([H|T],G,A) :-
	'$deb_get_sterm_in_g'(T,G,A1),
	arg(H,A1,A).

'$new_deb_depth' :-
	'$get_deb_depth'(0,D),
	'$set_deb_depth'(D).

'$get_deb_depth'(X0,XF) :-
	get0(user_input,C),
	'$get_depth_handle_char'(C,X0,XI),
	'$post_process_depth'(XI, XF).

'$post_process_depth'(0, 10) :- !.
'$post_process_depth'(X, X).

'$get_depth_handle_char'(10,X,X) :- !.
'$get_depth_handle_char'(C,X0,XF) :-
	 C >= "0", C =< "9", !,
	XI is X0*10+C-"0",
	'$get_deb_depth'(XI,XF).
'$get_depth_handle_char'(C,X,X) :- '$skipeol'(C).

'$set_deb_depth'(D) :-
	recorded('$print_options','$debugger'(L),R), !,
	'$delete_if_there'(L, max_depth(_), LN),
	erase(R),
	'$recorda'('$print_options','$debugger'([max_depth(D)|LN]),_).
'$set_deb_depth'(D) :-
	'$recorda'('$print_options','$debugger'([quoted(true),numbervars(true),portrayed(true),max_depth(D)]),_).
	
'$delete_if_there'([], _, []).
'$delete_if_there'([T|L], T, LN) :- !,
	'$delete_if_there'(L, T, LN).
'$delete_if_there'([Q|L], T, [Q|LN]) :-
	'$delete_if_there'(L, T, LN).

%
% catch errors
%
'$DebugError'(error(Msg,Error)) :- !,
	'$LoopError'(error(Msg,Error)),
	fail.
% allow abort and user defined exceptions to go through.
'$DebugError'(Ball) :- !,
	throw(Ball).

'$init_spy_cl'(G,M) :-
	% dynamic, immediate update procedure.
	'$flags'(G,M,F,F), F /\ 16'2000 =\= 0, !,
	( '$db_first_age'(M:G,A) ->
	  '$set_value'(spy_cl, A) ;
% no clauses for pred.
	  '$set_value'(spy_cl, 1) ).
'$init_spy_cl'(_,_) :-
	'$set_value'(spy_cl, 1).

