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
* File:		profile.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Some profiling predicates available in yap		 *
*									 *
*************************************************************************/

%% @file pl/profile.yap

:- system_module( '$_profile', [profile_data/3,
        profile_reset/0,
        showprofres/0,
        showprofres/1], []).

/**
`* @defgroup Exc_Profiling The Exception Based Tick Profiler.
 * @ingroup Profiling
 * @{
 * 
 * The count profiler works by incrementing counters at procedure entry or
 * backtracking. It provides exact information:
 * 
 * + Profiling works for both static and dynamic predicates.
 * + Currently only information on entries and retries to a predicate
 * are maintained. This may change in the future.
 * + As an example, the following user-level program gives a list of
 * the most often called procedures in a program. The procedure
 * list_profile/0 shows all procedures, irrespective of module, and
 * the procedure list_profile/1 shows the procedures being used in
 * a specific module.
 * 
 * ~~~~~
 * list_profile :-
 *         % get number of calls for each profiled procedure
 *         setof(D-[M:P|D1],(current_module(M),profile_data(M:P,calls,D),profile_data(M:P,retries,D1)),LP),
 *         % output so that the most often called
 *         % predicates will come last:
 *         write_profile_data(LP).
 * 
 * list_profile(Module) :-
 *         % get number of calls for each profiled procedure
 *         setof(D-[Module:P|D1],(profile_data(Module:P,calls,D),profile_data(Module:P,retries,D1)),LP),
 *         % output so that the most often called
 *         % predicates will come last:
 *         write_profile_data(LP).
 * 
 * write_profile_data([]).
 * write_profile_data([D-[M:P|R]|SLP]) :-
 *         % swap the two calls if you want the most often
 *         %  called predicates first.
 *         format('~a:~w: ~32+~t~d~12+~t~d~12+~n', [M,P,D,R]),
 *         write_profile_data(SLP).
 * ~~~~~
 * 
 * 
 * These are  the current predicates to access and clear profiling data:
 * 
 * 
 * 
 **/


:- use_system_module( '$_errors', ['$do_error'/2]).

%% user:prolog_predicate_name()/
%
% hook predicate, taken from SWI-Prolog, for converting possibly explicitly-
% qualified callable terms into an atom that can be used as a label for
% describing a predicate; used e.g. on the tick profiler defined below
:- multifile(user:prolog_predicate_name/2).

/** @pred  profile_data( ?Na/Ar, ?Parameter, -Data_)
 * 
 * 
 * Give current profile data on  _Parameter_ for a predicate described
 * by the predicate indicator  _Na/Ar_. If any of  _Na/Ar_ or
 *  _Parameter_ are unbound, backtrack through all profiled predicates
 * or stored parameters. Current parameters are:
 * 
 * + calls
 * Number of times a procedure was called.
 * 
 * + retries
 * Number of times a call to the procedure was backtracked to and retried.
 * 
 * 
 * + profile_reset
 * Reset all profiling information.
 * 
 */
:- meta_predicate profile_data(:,+,-).

profile_data(M:D, Parm, Data) :-!,
	(
	  var(M) ->
	  '$do_error'(instantiation_error,profile_data(M:D, Parm, Data))
	;
	  '$profile_data'(D, Parm, Data, M)
	).
profile_data(P, Parm, Data) :-
	'$current_module'(M),
	'$profile_data'(P, Parm, Data, M).

'$profile_data'(P, Parm, Data,M) :- var(P), !,
	'$profile_data_for_var'(P, Parm, Data,M).
'$profile_data'(M:P, Parm, Data, _) :-  !,
	'$profile_data'(P, Parm, Data, M).
'$profile_data'(P, Parm, Data, M) :-
	'$profile_data2'(P, Parm, Data, M).

'$profile_data2'(Na/Ar,Parm,Data, M) :-
	functor(P, Na, Ar),
	'$profile_info'(M, P, Stats),
	'$profile_say'(Stats, Parm, Data).

'$profile_data_for_var'(Name/Arity, Parm, Data, M) :-
	functor(P,Name,Arity),
	'$current_predicate'(Name,M,P,_),
	\+ '$hidden'(Name), % don't show hidden predicates.
	'$profile_info'(M, P, Stats),
	'$profile_say'(Stats, Parm, Data).

'$profile_say'('$profile'(Entries, _, _), calls, Entries).
'$profile_say'('$profile'(_, _, Backtracks), retries, Backtracks).

profile_reset :-
	current_module(M),
	'$current_predicate'(_Na,M,P,_),
	'$profile_reset'(M, P),
	fail.
profile_reset.

/** @pred  showprofres
 * 
 * 
 * Show profiling counts for all predicates.
 * 
 * 
 */
showprofres :-
	showprofres(-1).

/** @pred  showprofres( _N_)
 * 
 * Show profiling info for the top-most  _N_ predicates.
 * 
 * 
 * 
 * The showprofres/0 and `showprofres/1` predicates call a user-defined multifile hook predicate, `user:prolog_predicate_name/2`, that can be used for converting a possibly explicitly-qualified callable term into an atom that will used when printing the profiling information.
 * 
 * 
 */
showprofres(A) :-
	'$offline_showprofres',
	('$profison' -> profoff, Stop = true ; Stop = false),
	'$profglobs'(Tot,GCs,HGrows,SGrows,Mallocs,_Indexing,ProfOns),
	% root node has no useful info.
	'$get_all_profinfo'(0,[],ProfInfo0,0,_TotCode),
	msort(ProfInfo0,ProfInfo),
	'$get_ppreds'(ProfInfo,Preds0),
	'$add_extras_prof'(GCs, HGrows, SGrows, Mallocs, Preds0, PredsI),
	keysort(PredsI,Preds),
	'$sum_alls'(Preds,0,Tot0),
	Accounted is -Tot0,
	(ProfOns == 0 ->
	    format(user_error,'~d ticks, ~d accounted for~n',[Tot,Accounted])
	;
	    format(user_error,'~d ticks, ~d accounted for (~d overhead)~n',[Tot,Accounted,ProfOns])
	),
%	format(user_error,'     ~d ticks in indexing code~n',[Indexing]),
	A1 is A+1,
	'$display_preds'(Preds, Tot, 0, 1, A1),
	 (Stop = true -> profon ; true).

/*
'$check_duplicates'([]).
'$check_duplicates'([A,A|ProfInfo]) :- !,
	write(A),nl,
	'$check_duplicates'(ProfInfo).
'$check_duplicates'([_|ProfInfo]) :-
	'$check_duplicates'(ProfInfo).
*/


'$get_all_profinfo'([],L,L,Tot,Tot) :- !.
'$get_all_profinfo'(Node,L0,Lf,Tot0,Totf) :-
	'$profnode'(Node,Clause,PredId,Count,Left,Right),
	Tot1 is Tot0+Count,
	'$get_all_profinfo'(Left,L0,Li,Tot1,Tot2),
	'$get_all_profinfo'(Right,[gprof(PredId,Clause,Count)|Li],Lf,Tot2,Totf).

'$get_ppreds'([],[]).
'$get_ppreds'([gprof(0,_,0)|Cls],Ps) :- !,
	'$get_ppreds'(Cls,Ps).
'$get_ppreds'([gprof(0,_,Count)|_],_) :- !,
	'$do_error'('SYSTEM_ERROR_INTERNAL',showprofres(gprof(0,_,Count))).
'$get_ppreds'([gprof(PProfInfo,_,Count0)|Cls],[Sum-(Mod:Name/Arity)|Ps]) :-
	'$get_more_ppreds'(Cls,PProfInfo,Count0,NCls,Sum),
	'$get_pred_pinfo'(PProfInfo,Mod,Name,Arity),
	'$get_ppreds'(NCls,Ps).

'$get_more_ppreds'([gprof(PProfInfo,_,Count)|Cls],PProfInfo,Count0,NCls,Sum)
:- !,
	Count1 is Count+Count0,
	'$get_more_ppreds'(Cls,PProfInfo,Count1,NCls,Sum).
'$get_more_ppreds'(Cls, _, Sum, Cls, NSum) :- NSum is -Sum.

'$display_preds'(_, _, _, N, N) :- !.
'$display_preds'([], _, _, _, _).
'$display_preds'([0-_|_], _Tot, _SoFar, _I, _N) :- !.
'$display_preds'([NSum-P|Ps], Tot, SoFar, I, N) :-
	Sum is -NSum,
	Perc is (100*Sum)/Tot,
	Next is SoFar+Sum,
	NextP is (100*Next)/Tot,
	(	(	P = M:F/A ->
			G = M:H
		;	P = F/A,
			G = H
		),
		functor(H, F, A),
		user:prolog_predicate_name(G, PL) ->
		true
	;	PL = P
	),
	format(user_error,'~|~t~d.~7+ ~|~w:~t~d~50+ (~|~t~2f~6+%)  |~|~t~2f~6+%|~n',[I,PL,Sum,Perc,NextP]),
	I1 is I+1,
	'$display_preds'(Ps,Tot,Next,I1, N).

'$sum_alls'([],Tot,Tot).
'$sum_alls'([C-_|Preds],Tot0,Tot) :-
	TotI is C+Tot0,
	'$sum_alls'(Preds,TotI,Tot).


'$add_extras_prof'(GCs, HGrows, SGrows, Mallocs, Preds0, PredsI) :-
	'$add_extra_prof'(GCs, 'Garbage Collections',Preds0,Preds1),
	'$add_extra_prof'(HGrows, 'Code Expansion',Preds1,Preds2),
	'$add_extra_prof'(SGrows, 'Stack Expansion',Preds2,Preds3),
	'$add_extra_prof'(Mallocs, 'Heap Allocation',Preds3,PredsI).

'$add_extra_prof'(0, _,Preds, Preds) :- !.
'$add_extra_prof'(Ticks, Name, Preds, [NTicks-Name|Preds]) :-
	NTicks is -Ticks.


/**
@}
*/
