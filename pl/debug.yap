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
'$suspy'(V,S) :- var(V) , !,
	throw(error(instantiation_error,spy(V,S))).
'$suspy'((M:S),P) :- !,
    '$mod_switch'(M, '$suspy'(S,P)).
'$suspy'([],_) :- !.
'$suspy'([F|L],M) :- !, ( '$suspy'(F,M) ; '$suspy'(L,M) ).
'$suspy'(F/N,M) :- !, functor(T,F,N),
    ( '$system_predicate'(T) ->
	 throw(error(permission_error(access,private_procedure,F/N),spy(F/N,S)));
	'$undefined'(T) ->
	 throw(error(existence_error(procedure,F/N),spy(F/N,S)));
	 '$suspy2'(M,F,N,T) ).
'$suspy'(A,S) :- \+ atom(A) , !, 
        throw(error(type_error(predicate_indicator,A),spy(A,S))).
'$suspy'(A,spy) :- '$noclausesfor'(A), !,
	throw(error(existence_error(procedure,A),spy(A))).
'$suspy'(A,nospy) :- '$noclausesfor'(A), !,
	throw(error(existence_error(procedure,A),nospy(A))).
'$suspy'(A,M) :- current_predicate(A,T),
	\+ '$undefined'(T), \+ '$system_predicate'(T),
	 functor(T,F,N),
	'$suspy2'(M,F,N,T).

'$noclausesfor'(A) :- current_predicate(A,T),
	\+ '$undefined'(T) , \+ '$system_predicate'(T) ,
	!, fail .
'$noclausesfor'(_).

'$suspy2'(spy,F,N,T) :- 
	'$current_module'(M),
	'$recorded'('$spy','$spy'(T,M),_), !,
	format('[ Warning: there is already a spy point on ~w ]~n',M:F/N).
'$suspy2'(spy,F,N,T) :- !,
	'$warn_if_undef'(T,F,N),
	'$current_module'(M),
	'$recorda'('$spy','$spy'(T,M),_), 
	'$set_value'('$spypoint_added', true), 
	'$set_spy'(T),
	write(user_error,'[ Spy point set on '), write(user_error,F/N),
	write(user_error,' ]'), nl(user_error).
'$suspy2'(nospy,F,N,T) :- 
	'$current_module'(M),
	'$recorded'('$spy','$spy'(T,M),R), !,
	erase(R),
	'$rm_spy'(T),
	write(user_error,'[ Spy point on '), write(user_error,F/N), write(user_error,' removed ]'),
	nl(user_error).
'$suspy2'(nospy,F,N,_) :-
	write(user_error,'[ Warning: there is no spy-point on '),
	write(user_error,F/N), write(user_error,' ]'), nl(user_error).

'$warn_if_undef'(T,F,N) :-  '$undefined'(T), !,
	write(user_error,'[ Warning: you have no clauses for '),
	write(user_error,F/N), write(user_error,' ]'), nl(user_error).
'$warn_if_undef'(_,_,_).

'$pred_being_spied'(G) :-
	'$current_module'(M),
	'$recorded'('$spy','$spy'(G,M),_), !.

spy _ :- '$set_value'('$spypoint_added', false), fail.
spy L :- '$suspy'(L,spy), fail.
spy _ :- '$get_value'('$spypoint_added', false), !.
spy _ :- debug.

nospy L :- '$suspy'(L,nospy), fail.
nospy _.

nospyall :- '$recorded'('$spy','$spy'(T,M),_), functor(T,F,N), '$suspy'(M:F/N,nospy), fail.
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
'$spy'([_Module|G]) :-
%    '$format'(user_error,"$spym(~w,~w)~n",[Module,G]),
         ( '$hidden'(G)
	 ;
	'$parent_pred'(0,_,_),
        '$system_predicate'(G)
	 ),
         !,
	 /* called from prolog module   */
	 '$execute0'(G),
 	 '$creep'.
'$spy'(G) :-
	'$do_spy'(G).


'$direct_spy'(G) :-
	'$awoken_goals'(LG), !,
	'$creep',
	'$wake_up_goal'(G, LG).
'$direct_spy'([_|G]) :-
         '$hidden'(G),
         !,
	 /* called from prolog module   */
	 '$execute0'(G),
	 '$creep'.
'$direct_spy'(G) :-
	'$do_spy'(G).


'$do_spy'([Module|G]) :- !,
    (	  Module=prolog -> '$do_spy'(G);
	  '$mod_switch'(Module, '$do_spy'(G))
     ).
'$do_spy'(true) :- !, '$creep'.
'$do_spy'('$cut_by'(M)) :- !, '$cut_by'(M).
'$do_spy'(G) :-
%   write(user_error,$spy(G)), nl,
    '$get_value'(debug,1),		/* ditto if debug off		*/
    '$get_value'(spy_fs,0),		/* ditto if fast skipping	*/
    ( '$access_yap_flags'(10,0) ->	/* if not creeping ...		*/
	      '$pred_being_spied'(G)    /* ... spy only if at a spy-point */
	; true
    ),
%    ( \+ '$undefined'(user_error_spy(_)) -> user_error_spy(G) ;
%	true );
    !,					/* you sure want to spy this ... */
    '$get_value'(spy_gn,L),		/* get goal no.			*/
    L1 is L+1,				/* bump it			*/
    '$set_value'(spy_gn,L1),		/* and save it globaly		*/
    '$access_yap_flags'(10,SC),
    '$set_yap_flags'(10,1),		/* set creep on			*/
    '$get_value'(spy_cl,CL),		/* save global clause no.	*/
    '$current_module'(Module),
    repeat,		/* we need this to be able to implement retry	*/
	'$init_spy_cl'(G),
	'$trace'(call,G,L),		/* inform about call port	*/
	/* the following choice point is where the predicate is  called */
	( '$get_value'(spy_sp,0),	/* make sure we are not skipping*/
	  '$current_module'(_,Module),
	  '$spycalls'(G,Res)		/* go execute the predicate	*/
	   ;	/* we get here when the predicate fails */
	  	'$trace'(fail,G,L),	/* inform at fail port		*/
		'$get_value'(spy_sl,L2),/* make sure we are not ...	*/
		L2 \= L,		/* ... skiping to this level	*/
		!,			/* if not prepare to exit spy	*/
	        '$set_value'(spy_cl,CL),/* restore global value of clause no */
		'$setflop'(0), 
		'$set_creep'(SC),	/* restore creep value		*/
		'$cont_creep', fail ),	/* and exit			*/
	'$get_value'(spy_cl,Cla),	/* save no. of clause to try	*/
	( var(Res),			/* check not redoing		*/
	  '$trace'(exit,G,L),		/* output message at exit	*/
	  '$get_value'(spy_sp,0),	/* check not skipping		*/
	  '$set_creep'(SC),		/* restore creep value		*/
	  '$set_value'(spy_cl,CL),	/* restore clause no.		*/
	  '$setflop'(0),
	  '$cont_creep';		/* exit				*/
		/* we get here when we want to redo a goal		*/
		'$set_value'(spy_cl,Cla),/* restore clause no. to try	*/
		'$current_module'(_,Module),
		'$trace'(redo,G,L),	/* inform user_error		*/
		fail			/* to backtrack to spycalls	*/
	).
'$do_spy'(G) :- '$execute0'(G).	/* this clause applies when we do not want
				   to spy the goal			*/

'$cont_creep' :-  '$get_value'('$trace',1), '$set_yap_flags'(10,1), fail.
'$cont_creep' :- '$access_yap_flags'(10,1), !, '$creep'.
'$cont_creep'.

'$set_creep'(0) :- !, '$set_yap_flags'(10,0).
'$set_creep'(_).

%'$spycalls'(G,_) :- write(user_error,'$spycalls'(G)), nl(user_error), fail.
'$spycalls'([_|_],_) :- !, fail.
'$spycalls'('!'(CP),_) :-
	'$call'(!, CP, !).
'$spycalls'(Mod:G,Res) :-
	!,
	'$mod_switch'(Mod,'$spycalls'(G,Res)).
'$spycalls'(repeat,_) :-
	!,
	repeat.
'$spycalls'(fail,_) :-
	!,
	fail.
'$spycalls'(false,_) :-
	!,
	false.
'$spycalls'(true,_) :-
	!.
'$spycalls'(otherwise,_) :-
	!.
'$spycalls'(\+ G,Res) :-
	!,
	CP is '$last_choice_pt',
	'$spycalls'('$call'((\+ G), CP, (\+ G)),Res).
'$spycalls'(not(G),Res) :-
	!,
	CP is '$last_choice_pt',
	'$spycalls'('$call'(not(G), CP, not(G)),Res).
'$spycalls'(G,Res) :-                                   % undefined predicate
	'$undefined'(G), !,
	functor(G,F,N), '$current_module'(M),
	( '$recorded'('$import','$import'(S,M,F,N),_) ->
	  '$spycalls'(S:G,Res) ;
	  '$undefp'([M|G])
	).
'$spycalls'(G,_) :-
	'$flags'(G,F,_), F /\ 8'50000 =\= 0,		% Standard and C pred
	!,
	'$catch_spycall_stdpred'(G),
	(true;
		'$get_value'(spy_sp,P), P \= 0, !, fail),
	( true;
		'$get_value'(spy_sp,P1), P1 \= 0, !, fail)
	.
'$spycalls'(G,Res) :-		% asserts and retracts can complicate live
	( '$get_value'(spy_sp,0) -> true ; !, fail ),
	'$flags'(G,F,F),
	F /\ 16'2000 =\= 0, !, % dynamic procedure, immediate semantics
	repeat,
	        '$db_last_age'(G,Max),	
		'$get_value'(spy_cl,Cl),
		'$get_value'(spy_gn,L),
		Maxx is Max+1,
		'$set_value'(spy_cl,Maxx),
		( Cl > Max -> !, fail ; true ),
		( '$spycall_dynamic'(G,Cl) ;
			('$get_value'(spy_gn,L) -> '$leave_creep', fail ;
			  Res = redo )
		),
		( true ;
		  '$get_value'(spy_sp,P), P \= 0, !, fail )
	.
'$spycalls'(G,Res) :-
	( '$get_value'(spy_sp,0) -> true ; !, fail ),
	'$flags'(G,F,F),
	F /\ 16'8 =\= 0, !, % dynamic procedure, logical update semantics
	'$hold_index'(G, Index, Max), % hold an index on the procedure state when we called this goal
	repeat,
		'$get_value'(spy_cl,Cl),
		'$get_value'(spy_gn,L),
		Maxx is Max+1,
		'$set_value'(spy_cl,Maxx),
		( Cl > Max -> !, fail ; true),
		( '$log_upd_spycall'(G,Cl,Index) ;
			('$get_value'(spy_gn,L) ->
				'$leave_creep', fail ; % to backtrack to repeat
				Res = redo )
		),
		( true ;
		  '$get_value'(spy_sp,P), P \= 0, !, fail
	      )
	.
'$spycalls'(G,Res) :-
	( '$get_value'(spy_sp,0) -> true ; !, fail ),
	repeat,
	        '$number_of_clauses'(G,Max),
		'$get_value'(spy_cl,Cl),
		'$get_value'(spy_gn,L),
		Maxx is Max+1,
		'$set_value'(spy_cl,Maxx),
		( Cl > Max -> !, fail ; true),
		( '$spycall'(G,Cl) ;
			('$get_value'(spy_gn,L) ->
				'$leave_creep', fail ; % to backtrack to repeat
				Res = redo )
		),
		( true ;
		  '$get_value'(spy_sp,P), P \= 0, !, fail )
	.

'$spycall'(G,Cl) :-
	'$access_yap_flags'(10,0),
	!,
	'$setflop'(0),
	'$call_clause'(G,Cl).
'$spycall'(G,Cl) :-
	'$setflop'(0),
	'$creepcallclause'(G,Cl).

'$log_upd_spycall'(G,Cl,Index) :-
	'$access_yap_flags'(10,0),
	!,
	'$setflop'(0),
	'$call_log_updclause'(G,Cl,Index).
'$log_upd_spycall'(G,Cl,Index) :-
	'$setflop'(0),
	'$creepcall_log_upd_clause'(G,Cl,Index).

% this is to be used only for dynamic predicates
'$spycall_dynamic'(G,Cl) :-
	'$access_yap_flags'(10,0),
	!,
	'$setflop'(0),
	'$call_dynamic_clause'(G,Cl).
'$spycall_dynamic'(G,Cl) :-
	'$setflop'(0),
	'$creepcall_dynamic_clause'(G,Cl).

'$catch_spycall_stdpred'(G) :-
	'$system_catch'('$spycall_stdpred'(G), Error, user:'$DebugError'(Error)).

'$spycall_stdpred'(G) :-
	functor(G,F,N),
	(
	    '$recorded'('$meta_predicate','$meta_predicate'(_,F,N,_),_) ->
	    '$setflop'(1),
	    '$creep',
	    '$execute0'(G)
	;
	    '$setflop'(1),
	    '$execute0'(G)
	),
	'$setflop'(0).


'$call_clause'(G,Cl) :-
	'$system_catch'('$do_execute_clause'(G,Cl),Error,user:'$DebugError'(Error)).

'$do_execute_clause'(G,Cl) :-
	'$some_recordedp'(G), !,
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_)) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    '$fetch_clause'(G,Cl,Clause),
	    (Clause = true -> true ; '$debug_catch_call'(Clause,CP) )
	;
	     Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).
'$do_execute_clause'(G,Cl) :-
	'$execute'(G,Cl) ; Next is Cl+1, '$set_value'(spy_cl,Next), fail.

'$call_log_updclause'(G,Cl,Index) :-
	'$system_catch'('$do_execute_log_upd_clause'(G,Cl,Index),Error,user:'$DebugError'(Error)).

'$do_execute_log_upd_clause'(G,Cl,Index) :-
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_)) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    (Index = [] ->
               /* We have a single clause */
	       Cl = 1,
               clause(G, Clause)
            ;
	      Cl1 is Cl-1,
	      '$fetch_reference_from_index'(Index, Cl1, Ref),
              instance(Ref, (G :- Clause))
            ),
	    (Clause = true -> true ; '$debug_catch_call'(Clause,CP) )
	;
	     Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).

'$check_depth_for_interpreter'(10000000) :-
	'$undefined'(get_depth_limit(_)), !.
'$check_depth_for_interpreter'(D1) :-
	get_depth_limit(D0),
	D0 =\= 0,
	D1 is D0-1.

'$debug_catch_call'(Clause,CP) :-
	'$system_catch'('$call'(Clause,CP,Clause),Error,user:'$DebugError'(Error)).

'$call_dynamic_clause'(G,Cl) :-
	'$system_catch'('$do_execute_dynamic_clause'(G,Cl),Error,user:'$DebugError'(Error)).

'$do_execute_dynamic_clause'(G,Cl) :-
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_)) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    '$db_nb_to_ref'(Cl,G,Ref),
	    instance(Ref, (G :- Clause)),
	    (Clause = true -> true ; '$debug_catch_call'(Clause,CP) )
	;
	    Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).

'$creepcallclause'(G,Cl) :-
	'$system_catch'('$do_creep_execute'(G,Cl),Error,user:'$DebugError'(Error)).

'$do_creep_execute'(G,Cl) :-
	 % fast skip should ignore source mode
	'$get_value'(spy_fs,0),
	'$some_recordedp'(G),
	 !,
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_)) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    '$fetch_clause'(G,Cl,Clause),
	    (Clause = true -> true ;
             '$catch_creep_call'(Clause,CP)
            )
	;
	    Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).
'$do_creep_execute'(G,Cl) :-
	'$creep_execute'(G,Cl) ;
	Next is Cl+1, '$set_value'(spy_cl,Next), fail.

'$creepcall_log_upd_clause'(G,Cl,Index) :-
	'$system_catch'('$do_creep_log_upd_execute'(G,Cl,Index),Error,user:'$DebugError'(Error)).

'$do_creep_log_upd_execute'(G,Cl,Index) :-
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_)) -> true ; '$set_depth_limit'(D)),
        ( CP is '$last_choice_pt',
	  (Index = [] ->
             /* We have a single clause */
	     Cl = 1,
             clause(G, Clause)
            ;
	    Cl1 is Cl-1,
	    '$fetch_reference_from_index'(Index, Cl1, Ref),
	    instance(Ref, (G :- Clause))
	  ),
          (Clause = true -> true ;
		% otherwise fast skip may try to interpret assembly builtins.
             '$get_value'(spy_fs,1) -> '$debug_catch_call'(Clause,CP) ;
             '$catch_creep_call'(Clause,CP)
            )
	;
	    Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).

'$catch_creep_call'(Clause,CP) :-
	'$system_catch'('$creep_call'(Clause,CP),Error,user:'$DebugError'(Error)).

'$creepcall_dynamic_clause'(G,Cl) :-
	'$system_catch'('$do_creep_execute_dynamic'(G,Cl),Error,user:'$DebugError'(Error)).

'$do_creep_execute_dynamic'(G,Cl) :-
	'$check_depth_for_interpreter'(D),
	('$undefined'('$set_depth_limit'(_)) -> true ; '$set_depth_limit'(D)),
        CP is '$last_choice_pt',
	(
	    '$db_nb_to_ref'(Cl,G,Ref),
	    instance(Ref, (G :- Clause)),
	    (Clause = true -> true ;
	     % otherwise fast skip may try to interpret assembly builtins.
             '$get_value'(spy_fs,1) -> '$debug_catch_call'(Clause,CP) ;
             '$catch_creep_call'(Clause,CP)
            )
	;
	     Next is Cl+1, '$set_value'(spy_cl,Next), fail
        ).

'$leave_creep'.

'$creep_execute'(G,Cl) :-
	'$creep',
	'$execute'(G,Cl).

'$fetch_clause'(G,ClNum,Body) :-
	% I'd like an easier way to keep a counter
	'$set_value'('$fetching_clauses',1),
	'$recordedp'(G,Clause,_),
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
'$creep_call'(V,_) :- var(V), !,
	throw(error(instantiation_error,meta_call(V))).
'$creep_call'(A,_) :- number(A), !,
	throw(error(type_error(callable,A),meta_call(A))).
'$creep_call'(R,_) :- db_reference(R), !,
	throw(error(type_error(callable,R),meta_call(R))).
'$creep_call'(M:G,CP) :- !,
        '$mod_switch'(M, '$creep_call'(G,CP)),
	'$current_module'(Module),
	'$direct_spy'([Module|fail]).
'$creep_call'(fail,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|fail]).
'$creep_call'(false,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|false]).
'$creep_call'(true,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|true]).
'$creep_call'(otherwise,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|otherwise]).
'$creep_call'((A,B),CP) :- !,
	'$creep_call'(A,CP), '$creep_call'(B,CP).
'$creep_call'((X->Y; Z),CP) :- !,
	( '$creep_call'(X,CP), !, '$creep_call'(Y,CP); '$creep_call'(Z,CP)).
'$creep_call'((A;B),CP) :- !,
	('$creep_call'(A,CP) ; '$creep_call'(B,CP)).
'$creep_call'((A|B),CP) :- !,
	('$creep_call'(A,CP) ; '$creep_call'(B,CP)).
'$creep_call'(atom(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|atom(A)]).
'$creep_call'(atomic(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|atomic(A)]).
'$creep_call'(integer(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|integer(A)]).
'$creep_call'(nonvar(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|nonvar(A)]).
'$creep_call'(var(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|var(A)]).
'$creep_call'(number(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|number(A)]).
'$creep_call'(prismitive(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|primitive(A)]).
'$creep_call'(compound(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|compound(A)]).
'$creep_call'(float(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|float(A)]).
'$creep_call'(db_reference(A),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|db_reference(A)]).
'$creep_call'(\+ X,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|(\+ X)]).
'$creep_call'(not X,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|not(X)]).
'$creep_call'(X=Y,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|X=Y]).
'$creep_call'(X\=Y,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|X\=Y]).
'$creep_call'(X==Y,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|X==Y]).
'$creep_call'(X>Y,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|X>Y]).
'$creep_call'(X>=Y,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|X>=Y]).
'$creep_call'(X<Y,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|X<Y]).
'$creep_call'(X=<Y,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|X=<Y]).
'$creep_call'(X=:=Y,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|X=:=Y]).
'$creep_call'(X=\=Y,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|X=\=Y]).
'$creep_call'(arg(X,Y,Z),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|arg(X,Y,Z)]).
'$creep_call'(functor(X,Y,Z),_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|functor(X,Y,Z)]).
'$creep_call'((X->Y),CP) :- !,
	CP1 is '$last_choice_pt',
	'$creep_call'(X,CP),
	'$$cut_by'(CP1),
	'$creep_call'(Y,CP).
'$creep_call'(!,CP) :- !,
	'$current_module'(M),
	'$direct_spy'([M|'!'(CP)]),
	% clean up any garbage left here by the debugger.
	'$$cut_by'(CP).
'$creep_call'('$cut_by'(X),_) :- !,
	'$$cut_by'(X).
'$creep_call'(repeat,_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|repeat]).
'$creep_call'([A|B],_) :- !,
	'$current_module'(Module),
	'$direct_spy'([Module|[A|B]]).
'$creep_call'(A,CP) :-
	'$undefined'(A), !,
	'$creep_call_undefined'(A,CP).
'$creep_call'(A,_) :-
	'$current_module'(Module),
	'$direct_spy'([Module|A]).

'$creep_call_undefined'(A,CP) :-
	functor(A,F,N),
	'$current_module'(M),
	'$recorded'('$import','$import'(S,M,F,N),_), !,
	'$creep_call'(S:A,CP).
'$creep_call_undefined'(G, _) :-
	( \+ '$undefined'(user:unknown_predicate_handler(_,_,_)),
	  user:unknown_predicate_handler(G,M,NG) ->
	  '$creep_call'(M:NG) ;
	   '$is_dynamic'(G) -> fail ;
	    '$recorded'('$unknown','$unknown'(M:G,US),_),
	    '$creep_call'(user:US,_)
        ).

%'$creep'(G) :- $current_module(M),write(user_error,[creep,M,G]),nl(user_error),fail.
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
'$creep'(_) :-
	'$get_value'('$throw', true), !,
	'$set_value'('$throw', false),
	abort.
'$creep'([Module|'$trace'(P,G,L)]) :- !,
    (     Module=prolog -> '$trace'(P,G,L);
	  '$mod_switch'(Module, '$trace'(P,G,L))
     ).
'$creep'([Module|'$creep_call'(G,CP)]) :- !,
    (     Module=prolog -> '$creep_call'(G,CP);
	  '$mod_switch'(Module, '$creep_call'(G,CP) )
     ).
'$creep'([_|'$leave_creep']) :- !.
'$creep'(G) :- '$direct_spy'(G).

'$trace'(P,'!'(_),L) :- !,
	'$trace'(P,!,L).
'$trace'(P,G,L) :-
	'$chk'(P,L,G,SL),
	'$msg'(P,G,L,SL).
'$trace'(_,_,_).

'$msg'(P,G,L,SL):-
	flush_output(user_output),
	flush_output(user_error),
	'$get_value'(debug,1),
	repeat,
		('$pred_being_spied'(G) -> write(user_error,'*') ; write(user_error,' ')),
		( SL = L -> write(user_error,'>') ; write(user_error,' ')),
		write(user_error,' ('), write(user_error,L), write(user_error,') '),
		write(user_error,P), write(user_error,': '),
		( '$current_module'(Module), Module\=prolog,
			Module\=user -> write(user_error,Module),write(user_error,':');
		   true
		),
		'$debugger_write'(user_error,G),
		( 
		  '$unleashed'(P),
		  nl(user_error)
		  ;
		  write(user_error,' ? '), get0(user_input,C),
		  '$action'(C,P,L,G),
		  '$skipeol'(C)
		) ,
		!, fail.

'$unleashed'(call) :- '$get_value'('$leash',L), L /\ 2'1000 =:= 0.
'$unleashed'(exit) :- '$get_value'('$leash',L), L /\ 2'0100 =:= 0.
'$unleashed'(redo) :- '$get_value'('$leash',L), L /\ 2'0010 =:= 0.
'$unleashed'(fail) :- '$get_value'('$leash',L), L /\ 2'0001 =:= 0.

'$debugger_write'(Stream,G) :-
	'$recorded'('$debug_depth',D,_), !,
	write_depth(OS,OL),
	write_depth(D,D),
	'$write_deb2'(Stream,G),
	write_depth(OS,OL).
'$debugger_write'(Stream,G) :- '$write_deb2'(Stream,G).

'$write_deb2'(Stream, G) :-
	'$recorded'('$print_options','$debugger'(OUT),_), !,
	write_term(Stream, G, OUT).
'$write_deb2'(Stream, G) :-
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
'$chk'(_,_,[_|_],_) :- !, fail.
'$chk'(P,L,G,SL) :-
	'$get_value'(spy_leap,Leap),
	(Leap = 0 -> true;			      % not leaping
		('$pred_being_spied'(G) ; Leap = L),  % leaping or quasileaping
		'$set_value'(spy_leap,0) ),
	'$get_value'(spy_sp,SP),
	(SP = 0; SP = P),		% the current skipport or no skipport
	'$access_yap_flags'(10,SC),
	(SC = 1; '$pred_being_spied'(G)),
	'$get_value'(spy_sl,SL),
	(SL = 0; SL = L, '$set_value'(spy_sl,0), '$set_value'(spy_fs,0)),
	'$set_value'(spy_sp,0), !.

'$skipeol'(10) :- !.
'$skipeol'(_) :- get0(user,C), '$skipeol'(C).

'$action'(10,_,_,_) :- !,		% newline 	creep
	'$set_yap_flags'(10,1).
'$action'(33,_,_,_) :- !,		% ! g		execute
	read(user,G),
	% don't allow yourself to be caught by creep.
	'$access_yap_flags'(10, CL),
	'$set_yap_flags'(10, 0),
	( '$execute'(G) -> true ; true),
	'$set_yap_flags'(10, CL),
	!, fail.
'$action'(60,_,_,_) :- !,		% <Depth
	'$new_deb_depth'.
'$action'(94,_,_,G) :- !,
	'$print_deb_sterm'(G), fail.
'$action'(97,_,_,_) :- !, abort.	% a		abort
'$action'(98,_,_,_) :- !, break,	% b		break
	fail.
'$action'(99,call,_,_) :- !,		% c		creep
	'$set_yap_flags'(10,1).
'$action'(101,_,_,_) :- !,		% e		exit
	halt.
'$action'(102,P,L,_) :- !,		% f		fail
	( \+ P = fail, !; '$ilgl'(102) ),
	'$set_value'(spy_sp,fail),
	'$set_value'(spy_sl,L).
'$action'(104,_,_,_) :- !,		% h		help
	write(user_error,'newline  creep       a   abort'), nl(user_error),
	write(user_error,'c        creep       e   exit'), nl(user_error),
	write(user_error,'f        fail        h   help'), nl(user_error),
	write(user_error,'l        leap        r   retry'), nl(user_error),
	write(user_error,'s        skip        t   fastskip'), nl(user_error),
	write(user_error,'q        quasiskip   k   quasileap'), nl(user_error),
	write(user_error,'b        break       n   no debug'), nl(user_error),
	write(user_error,'p        print       d   display'), nl(user_error),
	write(user_error,'<D       depth D     <   full term'), nl(user_error),
	write(user_error,'+        spy this    -   nospy this'), nl(user_error),
	write(user_error,'^        view subg   ^^  view using'), nl(user_error),
	write(user_error,'! g execute goal'), nl(user_error),
	'$skipeol'(104), fail.
'$action'(63,_,_,_) :- !,		% ?		help
	write(user_error,'newline  creep       a   abort'), nl(user_error),
	write(user_error,'c        creep       e   exit'), nl(user_error),
	write(user_error,'f        fail        h   help'), nl(user_error),
	write(user_error,'l        leap        r   retry'), nl(user_error),
	write(user_error,'s        skip        t   fastskip'), nl(user_error),
	write(user_error,'q        quasiskip   k   quasileap'), nl(user_error),
	write(user_error,'b        break       n   no debug'), nl(user_error),
	write(user_error,'p        print       d   display'), nl(user_error),
	write(user_error,'<D       depth D     <   full term'), nl(user_error),
	write(user_error,'+        spy this    -   nospy this'), nl(user_error),
	write(user_error,'^        view subg   ^^  view using'), nl(user_error),
	write(user_error,'! g execute goal'), nl(user_error),
	'$skipeol'(104), fail.
'$action'(112,_,_,G) :- !,		% p		print
	print(user_error,G), nl(user_error),
	'$skipeol'(112), fail.
'$action'(100,_,_,G) :- !,		% d		display
	display(user_error,G), nl(user_error),
	'$skipeol'(100), fail.
'$action'(113,_,L,_) :- !,		% k		quasi skip
	'$set_value'(spy_leap,L).
'$action'(108,_,_,_) :- !,		% l		leap
	'$set_value'(spy_leap,1).
'$action'(110,_,_,_) :- !,		% n		nodebug
	nodebug.
'$action'(107,_,_,_) :- !,		% k		quasi leap
	'$set_yap_flags'(10,0).
'$action'(114,P,L,_) :- !,		% r		retry
	( P=call, !, '$ilgl'(114); true),
	'$set_value'(spy_sp,call),
	'$set_value'(spy_sl,L),
	write(user_error,'[ retry ]'), nl(user_error).
'$action'(115,P,L,_) :- !,		% s		skip
	( P=call; P=redo; '$ilgl'(115) ), !,
	'$set_value'(spy_sl,L).
'$action'(116,P,L,_) :- !,		% t		fast skip
	( P=call; P=redo; '$ilgl'(116) ), !,
	'$set_value'(spy_sl,L), '$set_value'(spy_fs,1).
'$action'(43,_,_,G) :- !,		% +		spy this
	functor(G,F,N), spy(F/N),
	'$skipeol'(43), fail.
'$action'(45,_,_,G) :- !,		% -		nospy this
	functor(G,F,N), nospy(F/N),
	'$skipeol'(45), fail.
'$action'(C,_,_,_) :- '$ilgl'(C).

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

'$new_deb_depth' :- '$get_deb_depth'(0,D),
	( '$recorded'('$debug_depth',_,R) -> erase(R) ; true ),
	( D \= 0 -> recorda('$debug_depth',D,_) ),
	fail.

'$get_deb_depth'(X0,XF) :-
	get0(user_input,C),
	'$get_depth_handle_char'(C,X0,XF).

'$get_depth_handle_char'(10,X,X) :- !.
'$get_depth_handle_char'(C,X0,XF) :-
	 C >= "0", C =< "9", !,
	XI is X0*10+C-"0",
	'$get_deb_depth'(XI,XF).
'$get_depth_handle_char'(C,X,X) :- '$skipeol'(C).

'$DebugError'(error(Msg, Where )) :- !,
	'$output_error_message'(Msg, Where), !,
	fail.
%
% do not try to handle other throws or aborts.
%
'$DebugError'(T) :- !,
	throw(T).

'$init_spy_cl'(G) :-
	% dynamic, immediate update procedure.
	'$flags'(G,F,F), F /\ 16'2000 =\= 0, !,
	( '$db_first_age'(G,A) ->
	  '$set_value'(spy_cl, A) ;
% no clauses for pred.
	  '$set_value'(spy_cl, 1) ).
'$init_spy_cl'(_) :-
	'$set_value'(spy_cl, 1).

