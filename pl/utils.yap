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
* File:		utils.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Some utility predicates available in yap		 *
*									 *
*************************************************************************/

once(G) :- '$execute'(G), !.

if(X,Y,_Z) :-
	CP is '$last_choice_pt',
	'$execute'(X),
	'$clean_ifcp'(CP),
	'$execute'(Y).
if(_X,_Y,Z) :-
	'$execute'(Z).
	

call_with_args(V) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V))).
call_with_args(M:A) :- !,
	'$call_with_args'(A,M).
call_with_args(A) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,M).
call_with_args(A) :- 
	throw(error(type_error(atom,A),call_with_args(A))).
	

call_with_args(V,A1) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1))).
call_with_args(M:A,A1) :- !,
	'$call_with_args'(A,A1,M).
call_with_args(A,A1) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,M).
call_with_args(A,A1) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1))).
	
call_with_args(V,A1,A2) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1,A2))).
call_with_args(M:A,A1,A2) :- !,
	'$call_with_args'(A,A1,A2,M).
call_with_args(A,A1,A2) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,M).
call_with_args(A,A1,A2) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1,A2))).
	
call_with_args(V,A1,A2,A3) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1,A2,A3))).
call_with_args(M:A,A1,A2,A3) :- !,
	'$call_with_args'(A,A1,A2,A3,M).
call_with_args(A,A1,A2,A3) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,M).
call_with_args(A,A1,A2,A3) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1,A2,A3))).
	
call_with_args(V,A1,A2,A3,A4) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1,A2,A3,A4))).
call_with_args(M:A,A1,A2,A3,A4) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,M).
call_with_args(A,A1,A2,A3,A4) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,M).
call_with_args(A,A1,A2,A3,A4) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1,A2,A3,A4))).
	
call_with_args(V,A1,A2,A3,A4,A5) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1,A2,A3,A4,A5))).
call_with_args(M:A,A1,A2,A3,A4,A5) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,M).
call_with_args(A,A1,A2,A3,A4,A5) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,M).
call_with_args(A,A1,A2,A3,A4,A5) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5))).
	
call_with_args(V,A1,A2,A3,A4,A5,A6) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1,A2,A3,A4,A5,A6))).
call_with_args(M:A,A1,A2,A3,A4,A5,A6) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,M).
call_with_args(A,A1,A2,A3,A4,A5,A6) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,M).
call_with_args(A,A1,A2,A3,A4,A5,A6) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6))).
	
call_with_args(V,A1,A2,A3,A4,A5,A6,A7) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1,A2,A3,A4,A5,A6,A7))).
call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6,A7))).
	
call_with_args(V,A1,A2,A3,A4,A5,A6,A7,A8) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1,A2,A3,A4,A5,A6,A7,A8))).
call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7,A8) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8))).
	
call_with_args(V,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1,A2,A3,A4,A5,A6,A7,A8,A9))).
call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9))).
	

call_with_args(V,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- var(V), !,
	throw(error(instantiation_error,call_with_args(V,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10))).
call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- 
	throw(error(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10))).
	

op(P,T,V) :- var(P), !,
	throw(error(instantiation_error,op(P,T,V))).
op(P,T,V) :- \+integer(P), !,
	throw(error(type_error(integer,P),op(P,T,V))).
op(P,T,V) :- (P < 0 ; P > 1200), !,
	throw(error(domain_error(operator_priority,P),op(P,T,V))).
op(P,T,V) :- var(T), !,
	throw(error(instantiation_error,op(P,T,V))).
op(P,T,V) :- \+atom(T), !,
	throw(error(type_error(atom,T),op(P,T,V))).
op(P,T,V) :- var(V), !,
	throw(error(instantiation_error,op(P,T,V))).
op(P,T,V) :-
	\+ atom(V), \+ '$check_list_of_operators'(V, op(P,T,V)),
	throw(error(type_error(list,V),op(P,T,V))).
op(P,T,V) :- '$op2'(P,T,V).

'$check_list_of_operators'(V, T) :- var(V), !,
	throw(error(instantiation_error,T)).
'$check_list_of_operators'([], _).
'$check_list_of_operators'([H|L], T) :-
	'$check_if_operator'(H,T),
	'$check_list_of_operators'(L, T).

'$check_if_operator'(H,T) :- var(H), !,
	throw(error(instantiation_error,T)).
'$check_if_operator'(H,_) :- atom(H), !.
'$check_if_operator'(H,T) :-
	throw(error(type_error(atom,H),T)).

'$op2'(_,_,[]) :- !.
'$op2'(P,T,[A|L]) :- !, '$op'(P,T,A), '$op2'(P,T,L).
'$op2'(P,T,A) :- atom(A), '$op'(P,T,A).

'$op'(P,T,',') :- !,
	throw(error(permission_error(modify,operator,','),op(P,T,','))).
'$op'(P,T,A) :- '$opdec'(P,T,A).

%%% Operating System utilities

cd(A) :- atom(A), !, atom_codes(A,S), '$cd'(S).
cd(S) :- '$cd'(S).

getcwd(D) :- '$getcwd'(SD), atom_codes(D, SD).

system(A) :- atom(A), !, atom_codes(A,S), '$system'(S).
system(S) :- '$system'(S).

rename(Old,New) :- atom(Old), atom(New), !,
	name(Old,SOld), name(New,SNew),
	'$rename'(SOld,SNew).

unix(V) :- var(V), !,
	throw(error(instantiation_error,unix(V))).
unix(argv(L)) :- (var(L) ; atom(L)), !, '$argv'(L).
unix(argv(V)) :-
	throw(error(type_error(atomic,V),unix(argv(V)))).
unix(cd) :- cd('~').
unix(cd(V)) :- var(V), !,
	throw(error(instantiation_error,unix(cd(V)))).
unix(cd(A)) :- atomic(A), !, cd(A).
unix(cd(V)) :-
	throw(error(type_error(atomic,V),unix(cd(V)))).
unix(environ(X,Y)) :- do_environ(X,Y).
unix(getcwd(X)) :- getcwd(X).
unix(shell(V)) :- var(V), !,
	throw(error(instantiation_error,unix(shell(V)))).
unix(shell(A)) :- atomic(A), !, '$shell'(A).
unix(shell(V)) :-
	throw(error(type_error(atomic,V),unix(shell(V)))).
unix(system(V)) :- var(V), !,
	throw(error(instantiation_error,unix(system(V)))).
unix(system(A)) :- atomic(A), !, system(A).
unix(system(V)) :-
	throw(error(type_error(atomic,V),unix(system(V)))).
unix(shell) :- sh.
unix(putenv(X,Y)) :- '$putenv'(X,Y).

putenv(Na,Val) :-
	'$putenv'(Na,Val).

getenv(Na,Val) :-
	'$getenv'(Na,Val).

alarm(_, _, _) :-
	recorded('$alarm_handler',_, Ref), erase(Ref), fail.
alarm(Interval, Goal, Left) :-
	'$current_module'(M),
	'$recordz'('$alarm_handler',M:Goal,_),
	'$alarm'(Interval, Left).

on_signal(Signal,OldAction,Action) :-
	recorded('$sig_handler', action(Signal,OldAction), Ref),
	erase(Ref),
	'$current_module'(M),
	'$recordz'('$sig_handler', action(Signal,M:Action),Ref2).


%%% Saving and restoring a computation

save(A) :- var(A), !,
	throw(error(instantiation_error,save(A))).
save(A) :- atom(A), !, name(A,S), '$save'(S).
save(S) :- '$save'(S).

save(A,_) :- var(A), !,
	throw(error(instantiation_error,save(A))).
save(A,OUT) :- atom(A), !, name(A,S), '$save'(S,OUT).
save(S,OUT) :- '$save'(S,OUT).

save_program(A) :- var(A), !,
	throw(error(instantiation_error,save_program(A))).
save_program(A) :- atom(A), !, name(A,S), '$save_program'(S).
save_program(S) :- '$save_program'(S).

save_program(A, G) :- var(A), !,
	throw(error(instantiation_error,save_program(A,G))).
save_program(A, G) :- var(G), !,
	throw(error(instantiation_error,save_program(A,G))).
save_program(A, G) :- \+ callable(G), !,
	throw(error(type_error(callable,G),save_program(A,G))).
save_program(A, G) :-
	( atom(A) -> name(A,S) ; A = S),
	'$recorda'('$restore_goal',G,R),
	'$save_program'(S),
	erase(R),
	fail.
save_program(_,_).

restore(A) :- var(A), !,
	throw(error(instantiation_error,restore(A))).
restore(A) :- atom(A), !, name(A,S), '$restore'(S).
restore(S) :- '$restore'(S).

%%% Data base predicates -> the interface to the external world

recorda(Key,Term,Ref) :- '$recorda'(Key,Term,Ref).
recordz(Key,Term,Ref) :- '$recordz'(Key,Term,Ref).

recordaifnot(Key,Term,Ref) :- '$recordaifnot'(Key,Term,Ref).
recordzifnot(Key,Term,Ref) :- '$recordzifnot'(Key,Term,Ref).

%%% Atoms with value

get_value(X,Y) :- '$get_value'(X,Y).
set_value(X,Y) :- '$set_value'(X,Y).

%%% current ....

current_atom(A) :-				% check
	atom(A), !.
current_atom(A) :-				% generate
	'$current_atom'(A).

current_predicate(A,T) :- var(T), !,		% only for the predicate
	'$current_module'(M),
	'$current_predicate_no_modules'(M,A,T).
current_predicate(A,M:T) :-			% module specified
	var(M), !,
	current_module(M),
	M \= prolog,
	'$current_predicate_no_modules'(M,A,T).
current_predicate(A,M:T) :- % module specified
	nonvar(T),
	!,
	functor(T,A,_),
	'$pred_exists'(T,M).
current_predicate(A,M:T) :- % module specified
	!,
	'$current_predicate_no_modules'(M,A,T).
current_predicate(A,T) :-			% only for the predicate
	'$current_module'(M),
	'$current_predicate_no_modules'(M,A,T).

current_predicate(F) :-	var(F), !,		% only for the predicate
	'$current_module'(M),
	'$current_predicate3'(M,F).
current_predicate(M:F) :-			% module specified
	var(M), !,
	'$current_module'(M),
	M \= prolog,
	'$current_predicate3'(M,F).
current_predicate(M:F) :- % module specified
	!,
	'$current_predicate3'(M,F).
current_predicate(F) :-			% only for the predicate
	'$current_module'(M),
	'$current_predicate3'(M,F).

system_predicate(A,P) :-
	'$current_predicate_no_modules'(prolog,A,P),
	\+ '$hidden'(A).

system_predicate(P) :-
	'$current_module'(M),
	'$system_predicate'(P,M).

'$current_predicate_no_modules'(M,A,T) :-
	'$current_predicate'(M,A,Arity),
	\+ '$hidden'(A),
	functor(T,A,Arity),
	'$pred_exists'(T,M).

'$current_predicate3'(M,A/Arity) :-
	'$current_predicate'(M,A,Arity),
	\+ '$hidden'(A),
	functor(T,A,Arity),
	'$pred_exists'(T,M).

%%% User interface for statistics

statistics :-
	'$runtime'(Runtime,_), 
	'$cputime'(CPUtime,_), 
	'$walltime'(Walltime,_), 
	'$statistics_heap_info'(HpSpa, HpInUse), 
	'$statistics_heap_max'(HpMax), 
	'$statistics_trail_info'(TrlSpa, TrlInUse), 
	'$statistics_trail_max'(TrlMax), 
	'$statistics_stacks_info'(StkSpa, GlobInU, LocInU), 
	'$statistics_global_max'(GlobMax), 
	'$statistics_local_max'(LocMax), 
	'$inform_heap_overflows'(NOfHO,TotHOTime),
	'$inform_stack_overflows'(NOfSO,TotSOTime),
	'$inform_trail_overflows'(NOfTO,TotTOTime),
	'$inform_gc'(NOfGC,TotGCTime,TotGCSize),
	'$statistics'(Runtime,CPUtime,Walltime,HpSpa,HpInUse,HpMax,TrlSpa, TrlInUse,TrlMax,StkSpa, GlobInU, LocInU,GlobMax,LocMax,NOfHO,TotHOTime,NOfSO,TotSOTime,NOfTO,TotTOTime,NOfGC,TotGCTime,TotGCSize).

'$statistics'(Runtime,CPUtime,Walltime,HpSpa,HpInUse,_HpMax,TrlSpa, TrlInUse,_TrlMax,StkSpa, GlobInU, LocInU,GlobMax,LocMax,NOfHO,TotHOTime,NOfSO,TotSOTime,NOfTO,TotTOTime,NOfGC,TotGCTime,TotGCSize) :-
	TotalMemory is HpSpa+StkSpa+TrlSpa,
	'$format'(user_error,"memory (total)~t~d bytes~35+~n", [TotalMemory]),
	'$format'(user_error,"   program space~t~d bytes~35+", [HpSpa]),
	'$format'(user_error,":~t  ~d in use~19+", [HpInUse]),
	HpFree is HpSpa-HpInUse,
	'$format'(user_error,",~t  ~d free~19+~n", [HpFree]),
	'$format'(user_error,"   stack space~t~d bytes~35+", [StkSpa]),
	StackInUse is GlobInU+LocInU,
	'$format'(user_error,":~t  ~d in use~19+", [StackInUse]),
	StackFree is StkSpa-StackInUse,
	'$format'(user_error,",~t  ~d free~19+~n", [StackFree]),
	'$format'(user_error,"     global stack:~t~35+", []),
	'$format'(user_error," ~t  ~d in use~19+", [GlobInU]),
	'$format'(user_error,",~t  ~d max~19+~n", [GlobMax]),
	'$format'(user_error,"      local stack:~t~35+", []),
	'$format'(user_error," ~t  ~d in use~19+", [LocInU]),
	'$format'(user_error,",~t  ~d max~19+~n", [LocMax]),
	'$format'(user_error,"   trail stack~t~d bytes~35+", [TrlSpa]),
	'$format'(user_error,":~t  ~d in use~19+", [TrlInUse]),
	TrlFree is TrlSpa-TrlInUse,
	'$format'(user_error,",~t  ~d free~19+~n", [TrlFree]),
	OvfTime is TotHOTime+TotSOTime+TotTOTime,
	'$format'(user_error,"~n~t~3f~12+ sec. for ~w code, ~w stack, and ~w trail space overflows~n",
	       [OvfTime,NOfHO,NOfSO,NOfTO]),
	'$format'(user_error,"~t~3f~12+ sec. for ~w garbage collections which collected ~d bytes~n",
	       [TotGCTime,NOfGC,TotGCSize]),
	RTime is float(Runtime)/1000,
	'$format'(user_error,"~t~3f~12+ sec. runtime~n", [RTime]),
	CPUTime is float(CPUtime)/1000,
	'$format'(user_error,"~t~3f~12+ sec. cputime~n", [CPUTime]),
	WallTime is float(Walltime)/1000,
	'$format'(user_error,"~t~3f~12+ sec. elapsed time~n~n", [WallTime]),
	fail.
'$statistics'(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

statistics(runtime,[T,L]) :-
	'$runtime'(T,L).
statistics(cputime,[T,L]) :-
	'$cputime'(T,L).
statistics(walltime,[T,L]) :-
	'$walltime'(T,L).
%statistics(core,[_]).
%statistics(memory,[_]).
statistics(heap,[Hp,HpF]) :-
	'$statistics_heap_info'(HpM, Hp),
	HpF is HpM-Hp.
statistics(program,Info) :-
	statistics(heap,Info).
statistics(global_stack,[GlobInU,GlobFree]) :-
	'$statistics_stacks_info'(StkSpa, GlobInU, LocInU),
	GlobFree is StkSpa-GlobInU-LocInU.
statistics(local_stack,[LocInU,LocFree]) :-
	'$statistics_stacks_info'(StkSpa, GlobInU, LocInU),
	LocFree is StkSpa-GlobInU-LocInU.
statistics(trail,[TrlInUse,TrlFree]) :-
	'$statistics_trail_info'(TrlSpa, TrlInUse),
	TrlFree is TrlSpa-TrlInUse.
statistics(garbage_collection,[NOfGC,TotGCSize,TotGCTime]) :-
	'$inform_gc'(NOfGC,TotGCTime,TotGCSize).
statistics(stack_shifts,[NOfHO,NOfSO,NOfTO]) :-
	'$inform_heap_overflows'(NOfHO,_),
	'$inform_stack_overflows'(NOfSO,_),
	'$inform_trail_overflows'(NOfTO,_).


%%% The unknown predicate,
%	informs about what the user wants to be done when
%	there are no clauses for a certain predicate */

unknown(V0,V) :-
	'$current_module'(M),
	'$unknown'(V0,V,M).

% query mode
'$unknown'(V0,V,_) :- var(V), !,
	'$ask_unknown_flag'(V),
	V = V0.
% handle modules.
'$unknown'(V0,Mod:Handler,_) :-
	'$unknown'(V0,Handler,Mod).
% check if we have one we like.
'$unknown'(_,New,Mod) :- 
	'$valid_unknown_handler'(New,Mod), fail.
% clean up previous unknown predicate handlers
'$unknown'(Old,New,Mod) :-
	'$recorded'('$unknown','$unknown'(_,MyOld),Ref), !,
	erase(Ref),
	'$cleanup_unknown_handler'(MyOld,Old),
	'$new_unknown'(New, Mod).
% store the new one.
'$unknown'(fail,New,Mod) :-
	'$new_unknown'(New, Mod).

'$valid_unknown_handler'(V,_) :-
	var(V), !,
	throw(error(instantiation_error,yap_flag(unknown,V))).
'$valid_unknown_handler'(fail,_) :- !.
'$valid_unknown_handler'(error,_) :- !.
'$valid_unknown_handler'(warning,_) :- !.
'$valid_unknown_handler'(S,M) :-
	functor(S,_,1),
	arg(1,S,A),
	var(A), 
	\+ '$undefined'(S,M),
	!.
'$valid_unknown_handler'(S,_) :-
	throw(error(domain_error(flag_value,unknown+S),yap_flag(unknown,S))).

'$ask_unknown_flag'(Old) :-
	'$recorded'('$unknown','$unkonwn'(_,MyOld),_), !,
	'$cleanup_unknwon_handler'(MyOld,Old).
'$ask_unknown_flag'(fail).

'$cleanup_unknown_handler'('$unknown_error'(_),error) :- !.
'$cleanup_unknown_handler'('$unknown_warning'(_),warning) :- !.
'$cleanup_unknown_handler'(Handler, Handler).

'$new_unknown'(fail,_) :- !.
'$new_unknown'(error,_) :- !,
	'$recorda'('$unknown','$unknown'(P,'$unknown_error'(P)),_).
'$new_unknown'(warning,_) :- !,
	'$recorda'('$unknown','$unknown'(P,'$unknown_warning'(P)),_).
'$new_unknown'(X,M) :-
	arg(1,X,A),
	'$recorda'('$unknown','$unknown'(A,M:X),_).

'$unknown_error'(P) :-
	throw(error(unknown,P)).

'$unknown_warning'(P) :-
	P=M:F,
	functor(F,Na,Ar),
	'$format'(user_error,"[ EXISTENCE ERROR: ~w, procedure ~w:~w/~w undefined ]~n",
	[P,M,Na,Ar]),
	fail.

predicate_property(Mod:Pred,Prop) :- !,
	'$predicate_property2'(Pred,Prop,Mod).
predicate_property(Pred,Prop) :- 
	'$current_module'(Mod),
	'$predicate_property2'(Pred,Prop,Mod).

'$predicate_property2'(Pred,Prop,M) :- var(Pred), !,
	'$current_predicate'(M,_,Pred),
	'$pred_exists'(Pred,M),
	'$predicate_property'(Pred,M,Prop).
'$predicate_property2'(M:Pred,Prop,_) :-
	'$predicate_property'(Pred,Prop,M).
'$predicate_property2'(Pred,Prop,Mod) :- 
	'$predicate_property'(Pred,Mod,Prop),
	'$pred_exists'(Pred,Mod).

'$predicate_property'(P,M,built_in) :- 
	'$system_predicate'(P,M), !.
'$predicate_property'(P,M,dynamic) :-
	'$is_dynamic'(P,M).
'$predicate_property'(P,M,static) :-
	\+ '$is_dynamic'(P,M).
'$predicate_property'(P,M,meta_predicate(P)) :-
	functor(P,Na,Ar),
	user:'$meta_predicate'(M,Na,Ar,P).
'$predicate_property'(P,M,multifile) :-
	'$is_multifile'(P,M).
'$predicate_property'(P,_,imported_from(Mod)) :-
	functor(P,N,A),
	'$recorded'('$module','$module'(_TFN,Mod,Publics),_),
	'$member'(N/A,Publics).	/* defined in modules.yap */
'$predicate_property'(P,M,public) :-
	'$is_public'(P,M).
'$predicate_property'(P,M,exported) :-
	functor(P,N,A),
	'$recorded'('$module','$module'(_TFN,M,Publics),_),
	'$member'(N/A,Publics).	/* defined in modules.yap */

%%% Some "dirty" predicates

% Only efective if yap compiled with -DDEBUG
% this predicate shows the code produced by the compiler
'$show_code' :- '$debug'(0'f).

'$pred_exists'(Pred,M) :- '$is_dynamic'(Pred,M), !.
'$pred_exists'(Pred,M) :- \+ '$undefined'(Pred,M).


grow_heap(X) :- '$grow_heap'(X).
grow_stack(X) :- '$grow_stack'(X).

%
% gc() expects to be called from "call". Make sure it has an
% environment to return to.
%
%garbage_collect :- save(dump), '$gc',  save(dump2).
garbage_collect :-
	'$gc'.
gc :-
	yap_flag(gc,on).
nogc :-
	yap_flag(gc,off).

'$force_environment_for_gc'.

profile_data(P, Parm, Data) :- var(P), !,
	'$current_module'(M),
	'$profile_data'(P, Parm, Data, M).

'$profile_data'(P, Parm, Data,M) :- var(P), !,
	'$profile_data_for_var'(P, Parm, Data,M).
'$profile_data'(M:P, Parm, Data, _) :- var(M), !,
	throw(error(instantiation_error,profile_data(M:P, Parm, Data))).
'$profile_data'(M:P, Parm, Data, _) :- 
	'$profile_data'(P, Parm, Data, M).
'$profile_data'(P, Parm, Data, M) :-
	'$profile_data2'(P, Parm, Data, M).

'$profile_data2'(Na/Ar,Parm,Data, M) :-
	functor(P, Na, Ar),
	'$profile_info'(M, P, Stats),
	'$profile_say'(Stats, Parm, Data).

'$profile_data_for_var'(Name/Arity, Parm, Data, M) :-
	'$current_predicate'(M,_,P),
	functor(P, Name, Arity),
	'$profile_info'(M, P, Stats),
	'$profile_say'(Stats, Parm, Data).



'$profile_say'('$profile'(Entries, _, _), calls, Entries).
'$profile_say'('$profile'(_, _, Backtracks), retries, Backtracks).

profile_reset :-
	current_module(M),
	'$current_predicate'(M,_,P0),
	'$profile_reset'(M, P0),
	fail.
profile_reset.

'$good_list_of_character_codes'(V) :- var(V), !.
'$good_list_of_character_codes'([]).
'$good_list_of_character_codes'([X|L]) :-
	'$good_character_code'(X),
	'$good_list_of_character_codes'(L).

'$good_character_code'(X) :- var(X), !.
'$good_character_code'(X) :- integer(X), X > -2, X < 256.

atom_concat(X,Y,At) :-
	atom(At), !,
	atom_length(At,Len),
	'$atom_contact_split'(At,0,Len,X,Y).
/* Let atom_chars do our error handling */
atom_concat(X,Y,At) :-
	atom_codes(X,S1),
	atom_codes(Y,S2),
	'$append'(S1,S2,S),
	atom_codes(At,S).



'$atom_contact_split'(At,Len,Len,X,Y) :- !,
	'$atom_split'(At,Len,X,Y).
'$atom_contact_split'(At,Len1,_,X,Y) :-
	'$atom_split'(At,Len1,X,Y).
'$atom_contact_split'(At,Len1,Len,X,Y) :-
	Len2 is Len1+1,
	'$atom_contact_split'(At,Len2,Len,X,Y).

sub_atom(At, Bef, Size, After, SubAt) :-
	var(At), !,
	throw(error(instantiation_error,sub_atom(At, Bef, Size,After, SubAt))).
sub_atom(At, Bef, Size, After, SubAt) :-
	\+ atom(At), !,
	throw(error(type_error(atom,At),sub_atom(At, Bef, Size,After, SubAt))).
sub_atom(At, Bef, Size, After, SubAt) :-
	nonvar(SubAt), \+ atom(SubAt), !,
	throw(error(type_error(atom,SubAt),sub_atom(At, Bef, Size,After, SubAt))).
sub_atom(At, Bef, Size, After, SubAt) :-
	'$check_type_sub_atom'(Bef, sub_atom(At, Bef, Size,After, SubAt)),
	'$check_type_sub_atom'(Size, sub_atom(At, Bef, Size,After, SubAt)),
	'$check_type_sub_atom'(After, sub_atom(At, Bef, Size,After, SubAt)),
	atom_codes(At, Atl),
	'$$_length1'(Atl,0,Len),
	'$split_len_in_parts'(Atl, Len, Bef, Size, After, SubAtl),
	atom_codes(SubAt, SubAtl).

'$check_type_sub_atom'(I, _) :-
	var(I), !.
'$check_type_sub_atom'(I, P) :-
	integer(I), I < 0, !,
	throw(error(domain_error(not_less_than_zero,I),P)).
'$check_type_sub_atom'(I, P) :-
	\+ integer(I), !,
	throw(error(type_error(integer,I),P)).
'$check_type_sub_atom'(_, _).

'$split_len_in_parts'(Atl, Len, Bef, Size, After, SubAt) :-
	var(Bef), !,
	'$range_var'(0, Len, Bef),
	'$split_len_in_parts2'(Atl, Len, Bef, Size, After, SubAt).
'$split_len_in_parts'(Atl, Len, Bef, Size, After, SubAt) :-
	'$split_len_in_parts2'(Atl, Len, Bef, Size, After, SubAt).

'$split_len_in_parts2'(Atl, Len, 0, Size, After, SubAt) :- !,
	'$split_len_in_parts3'(Atl, Len, Size, After, SubAt).
'$split_len_in_parts2'([_|Atl], Len0, Bef0, Size, After, SubAt) :-
	Bef is Bef0-1 ,
	Len is Len0-1,
	'$split_len_in_parts2'(Atl, Len, Bef, Size, After, SubAt).
	
	
'$split_len_in_parts3'(Atl, Len, Size, After, SubAt) :-
	var(Size), !,
	'$range_var'(0, Len, Size),
	'$split_len_in_parts4'(Atl, Len, Size, After, SubAt).
'$split_len_in_parts3'(Atl, Len, Size, After, SubAt) :-
	'$split_len_in_parts4'(Atl, Len, Size, After, SubAt).

'$split_len_in_parts4'(_, Len, 0, After, SubAt) :- !,
	After = Len,
	SubAt = [].
'$split_len_in_parts4'([Code|Atl], Len0, Size0, After, [Code|SubAt]) :-
	Size is Size0-1,
	Len is Len0-1,
	'$split_len_in_parts4'(Atl, Len, Size, After, SubAt).
	
'$range_var'(X,X,S) :- !, S = X.
'$range_var'(X1,_,X1).
'$range_var'(X1,X2,XF) :-
	X11 is X1+1,
	'$range_var'(X11,X2,XF).

'$singletons_in_term'(T,VL) :-
	'$variables_in_term'(T,[],V10),
	'$sort'(V10, V1),
	'$non_singletons_in_term'(T,[],V20),
	'$sort'(V20, V2),	
	'$subtract_lists_of_variables'(V2,V1,VL).

'$subtract_lists_of_variables'([],VL,VL).
'$subtract_lists_of_variables'([_|_],[],[]) :- !.
'$subtract_lists_of_variables'([V1|VL1],[V2|VL2],VL) :-
	V1 == V2, !,
	'$subtract_lists_of_variables'(VL1,VL2,VL).
'$subtract_lists_of_variables'([V1|VL1],[V2|VL2],[V2|VL]) :-
	'$subtract_lists_of_variables'([V1|VL1],VL2,VL).
	
simple(V) :- var(V), !.
simple(A) :- atom(A), !.
simple(N) :- number(N).

callable(V) :- var(V), !, fail.
callable(V) :- atom(V), !.
callable(V) :- functor(V,_,Ar), Ar > 0.

initialization :-
	'$initialisation_goals'.

prolog_initialization(G) :- var(G), !,
	throw(error(instantiation_error,initialization(G))).
prolog_initialization(T) :- callable(T), !,
	'$assert_init'(T).
prolog_initialization(T) :-
	throw(error(type_error(callable,T),initialization(T))).

'$assert_init'(T) :- '$recordz'('$startup_goal',T,_), fail.
'$assert_init'(_).

version :- '$version'.

version(V) :- var(V),  !,
	throw(error(instantiation_error,version(V))).
version(T) :- atom(T), !, '$assert_version'(T).
version(T) :-
	throw(error(type_error(atom,T),version(T))).

'$assert_version'(T) :- '$recordz'('$version',T,_), fail.
'$assert_version'(_).

term_variables(Term, L) :-
	'$variables_in_term'(Term, [], L).

term_hash(X,Y) :-
	term_hash(X,-1,16'1000000,Y).



%
% allow users to define their own directives.
%
user_defined_directive(Dir,_) :-
	'$directive'(Dir), !.
user_defined_directive(Dir,Action) :-
	functor(Dir,Na,Ar),
	functor(NDir,Na,Ar),
	'$current_module'(M, prolog),
	assert_static('$directive'(NDir)),
	assert_static(('$exec_directive'(Dir, _, _) :- Action)),
	'$current_module'(_, M).

'$set_toplevel_hook'(_) :- 
	'$recorded'('$toplevel_hooks',_,R),
	erase(R),
	fail.
'$set_toplevel_hook'(H) :- 
	'$recorda'('$toplevel_hooks',H,_),
	fail.
'$set_toplevel_hook'(_).


raise_exception(Ball) :- throw(Ball).
on_exception(Pat, G, H) :- catch(G, Pat, H).

