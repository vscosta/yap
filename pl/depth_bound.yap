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
* File:		corout.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	Coroutines implementation				 *
*									 *
*************************************************************************/

%depth_bound_call(A,D) :-
%write(depth_bound_call(A,D)), nl, fail.
depth_bound_call(A,D) :-
	'$execute_under_depth_limit'(A,D).

$old_depth_bound_call(A,D) :-
	'$check_callable'(A,A),
	'$user_call_depth_limited'(A, D).

'$user_call_depth_limited'(V,D) :- var(V), !,
	throw(error(instantiation_error,V)).
'$user_call_depth_limited'(A,D) :- number(A), !,
	throw(error(type_error(callable,A),A,D)).
'$user_call_depth_limited'(R,D) :- db_reference(R), !,
	throw(error(type_error(callable,R),R)).
'$user_call_depth_limited'(A,D) :-
	'$access_yap_flags'(10,V),
	V \= 0, !,
	'$save_current_choice_point'(CP),
	'$spied_call_depth_limited'(A,CP,D).
'$user_call_depth_limited'(A,D) :-
	'$save_current_choice_point'(CP),
	'$call_depth_limited'(A,CP,D).

'$call_depth_limited'(M:G,CP,D) :- !,
	( '$current_module'(M) ->
	    '$check_callable'(G,M:G),
	    '$call_depth_limited'(G,CP,D)
        ;
	     '$current_module'(Old,M),
	     '$check_callable'(G,M:G),
	     ( '$call_depth_limited'(G,CP,D); '$current_module'(_,Old), fail ),
	     ( '$current_module'(_,Old); '$current_module'(_,M), fail)
        ).
'$call_depth_limited'(fail,_,_) :- !, fail.
'$call_depth_limited'(false,_,_) :- !, false.
'$call_depth_limited'(true,_,_) :- !.
'$call_depth_limited'(otherwise,_,_) :- !.
'$call_depth_limited'((A,B),CP,D) :- !,
	'$check_callable'(A,(A,B)),
	D1 is D+1,
	'$call_depth_limited'(A,CP,D1),
	'$check_callable'(B,(A,B)),
	'$call_depth_limited'(B,CP,D1).
'$call_depth_limited'((X->Y),CP,D) :- !,
	'$check_callable'(X,(X->Y)),
	 CP1 is local_sp,
	D1 is D+1,
	'$call_depth_limited'(X,CP,D1),
	'$$cut_by'(CP1),
	'$check_callable'(Y,(X->Y)),
	'$call_depth_limited'(Y,CP,D1).
'$call_depth_limited'((X->Y; Z),CP,D) :- !,
	'$check_callable'(X,(X->Y;Z)),
	D1 is D+1,
	(
	    '$call_depth_limited'(X,CP,D1), !,
	    '$check_callable'(Y,(X->Y;Z)),
	    '$call_depth_limited'(Y,CP,D1) 
        ;
	    '$check_callable'(Z,(X->Y;Z)),
	    '$call_depth_limited'(Z,CP,D1)
	).
'$call_depth_limited'((A;B),CP,D) :- !,
	'$check_callable'(A,(A;B)),
	D1 is D+1,
	(
	    '$call_depth_limited'(A,CP,D1)
	;
	    '$check_callable'(B,(A;B)),
	    '$call_depth_limited'(B,CP,D1)
	).
'$call_depth_limited'((A|B),CP,D) :- !,
	'$check_callable'(A,(A|B)),
	D1 is D+1,
	(
	    '$call_depth_limited'(A,CP,D1)
	;
	    '$check_callable'(B,(A|B)),
	    '$call_depth_limited'(B,CP,D1)
	).
'$call_depth_limited'(\+ X,CP,D) :- !,
	'$check_callable'(X, \+ X),
	\+ '$call_depth_limited'(X,CP,D).
'$call_depth_limited'(not X,CP,D) :- !,
	'$check_callable'(X, not X),
	\+ '$call_depth_limited'(X,CP,D).
'$call_depth_limited'(!,CP,_) :- $$cut_by(CP).
'$call_depth_limited'(repeat,_,_) :- !, $repeat.
'$call_depth_limited'([A|B],_,_) :- !, '$csult'([A|B]).
'$call_depth_limited'(A,CP,D) :-
	( '$undefined'(A) ->
		functor(A,F,N), $current_module(M),
		( '$recorded'($import,$import(S,M,F,N),_) ->
		  '$call_depth_limited'(S:A,CP,D) ;
		  get_depth_limit(D0),
		  '$set_depth_limit'(D),
		  '$undefp'([M|A]),
		  '$set_depth_limit'(D0),
		  '$ensure_env_for_call_depth_limited'
	        )
	    ;
		get_depth_limit(D0),
		'$set_depth_limit'(D),
		'$execute0'(A),
		'$set_depth_limit'(D0),
		'$ensure_env_for_call_depth_limited'
	 ).


'$spied_call_depth_limited'(M:G,CP,D) :- !,
	( '$current_module'(M) ->
	    '$check_callable'(G,M:G),
	    '$spied_call_depth_limited'(G,CP,D)
        ;
	     '$current_module'(Old,M),
	     '$check_callable'(G,M:G),
	     ( '$spied_call_depth_limited'(G,CP,D); '$current_module'(_,Old), fail ),
	     ( '$current_module'(_,Old); '$current_module'(_,M), fail)
        ).
'$spied_call_depth_limited'(fail,_,_) :- !, fail.
'$spied_call_depth_limited'(false,_,_) :- !, false.
'$spied_call_depth_limited'(true,_,_) :- !.
'$spied_call_depth_limited'(otherwise,_,_) :- !.
'$spied_call_depth_limited'((A,B),CP,D) :- !,
	'$check_callable'(A,(A,B)),
	D1 is D+1,
	'$spied_call_depth_limited'(A,CP,D1),
	'$check_callable'(B,(A,B)),
	'$spied_call_depth_limited'(B,CP,D1).
'$spied_call_depth_limited'((X->Y),CP,D) :- !,
	'$check_callable'(X,(X->Y)),
	 CP1 is local_sp,
	 D1 is D+1,
	'$spied_call_depth_limited'(X,CP,D1),
	'$$cut_by'(CP1),
	'$check_callable'(Y,(X->Y)),
	'$spied_call_depth_limited'(Y,CP,D1).
'$spied_call_depth_limited'((X->Y; Z),CP, D) :- !,
	'$check_callable'(X,(X->Y;Z)),
	D1 is D+1,
	(
	    '$spied_call_depth_limited'(X,CP,D1), !,
	    '$check_callable'(Y,(X->Y;Z)),
	    '$spied_call_depth_limited'(Y,CP,D1) 
        ;
	    '$check_callable'(Z,(X->Y;Z)),
	    '$spied_call_depth_limited'(Z,CP,D1)
	).
'$spied_call_depth_limited'((A;B),CP,D) :- !,
	'$check_callable'(A,(A;B)),
	D1 is D+1,
	(
	    '$spied_call_depth_limited'(A,CP,D1)
	;
	    '$check_callable'(B,(A;B)),
	    '$spied_call_depth_limited'(B,CP,D1)
	).
'$spied_call_depth_limited'((A|B),CP,D) :- !,
	'$check_callable'(A,(A|B)),
	D1 is D+1,
	(
	    '$spied_call_depth_limited'(A,CP,D1)
	;
	    '$check_callable'(B,(A|B)),
	    '$spied_call_depth_limited'(B,CP,D1)
	).
'$spied_call_depth_limited'(\+ X,CP,D) :- !,
	'$check_callable'(X, \+ X),
	\+ '$spied_call_depth_limited'(X,CP,D).
'$spied_call_depth_limited'(not X,CP,D) :- !,
	'$check_callable'(X, not X),
	\+ '$spied_call_depth_limited'(X,CP,D).
'$spied_call_depth_limited'(!,CP,_) :- $$cut_by(CP).
'$spied_call_depth_limited'(repeat,_,_) :- !, '$repeat'.
'$spied_call_depth_limited'([A|B],_,_) :- !, '$csult'([A|B]).
'$spied_call_depth_limited'(A,CP,D) :-
	( '$undefined'(A) ->
		functor(A,F,N), $current_module(M),
		( '$recorded'($import,$import(S,M,F,N),_) ->
		  '$spied_call_depth_limited'(S:A,CP,D) ;
		  get_depth_limit(D0),
		  '$set_depth_limit'(D),
		  '$spy'(A),
		  '$set_depth_limit'(D0),
		  '$ensure_env_for_call_depth_limited'
	        )
	    ;
		get_depth_limit(D0),
		'$set_depth_limit'(D),
		'$spy'(A),
		'$set_depth_limit'(D0),
		'$ensure_env_for_call_depth_limited'
	 ).

'$ensure_env_for_call_depth_limited'.

