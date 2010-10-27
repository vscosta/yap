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


% hook predicate, taken from SWI-Prolog, for converting possibly explicitly-
% qualified callable terms into an atom that can be used as a label for
% describing a predicate; used e.g. on the tick profiler defined below
:- multifile(user:prolog_predicate_name/2).

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
	'$current_predicate'(M,Name,Arity),
	functor(P,Name,Arity),
	\+ '$hidden'(Name), % don't show hidden predicates.
	'$profile_info'(M, P, Stats),
	'$profile_say'(Stats, Parm, Data).

'$profile_say'('$profile'(Entries, _, _), calls, Entries).
'$profile_say'('$profile'(_, _, Backtracks), retries, Backtracks).

profile_reset :-
	current_module(M),
	'$current_predicate'(M,Na,Ar),
	functor(P,Na,Ar),
	'$profile_reset'(M, P),
	fail.
profile_reset.

showprofres :-
	showprofres(-1).

showprofres(A) :-
	('$proftype'(offline) -> '$offline_showprofres' ; true),
	('$profison' -> profoff, Stop = true ; Stop = false),
	'$profglobs'(Tot,GCs,HGrows,SGrows,Mallocs,ProfOns),
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
	'$do_error'(system_error,showprofres(gprof(0,_,Count))).
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

