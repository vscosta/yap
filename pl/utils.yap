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

forall(Cond, Action) :- \+((Cond, \+(Action))).

ignore(Goal) :- (Goal->true;true).

if(X,Y,Z) :-
	yap_hacks:env_choice_point(CP0),
	(
	 CP is '$last_choice_pt',
	 '$call'(X,CP,if(X,Y,Z),M),
	 '$execute'(X),
	 '$clean_ifcp'(CP),
	 '$call'(Y,CP,if(X,Y,Z),M)
	;
	 '$call'(Z,CP,if(X,Y,Z),M)
	).

call(X,A) :- '$execute'(X,A).

call(X,A1,A2) :- '$execute'(X,A1,A2).

call(X,A1,A2,A3) :- '$execute'(X,A1,A2,A3).

call(X,A1,A2,A3,A4) :- '$execute'(X,A1,A2,A3,A4).

call(X,A1,A2,A3,A4,A5) :- '$execute'(X,A1,A2,A3,A4,A5).

call(X,A1,A2,A3,A4,A5,A6) :- '$execute'(X,A1,A2,A3,A4,A5,A6).

call(X,A1,A2,A3,A4,A5,A6,A7) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7).

call(X,A1,A2,A3,A4,A5,A6,A7,A8) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7,A8).

call(X,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7,A8,A9).

call(X,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10).

call(X,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11).

call_with_args(M:V) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V)).
call_with_args(_:M:A) :- !,
	call_with_args(M:A).
call_with_args(M:A) :- !,
	'$call_with_args'(A,M).
call_with_args(A) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,M).
call_with_args(A) :- 
	'$do_error'(type_error(atom,A),call_with_args(A)).
	

call_with_args(M:V,A1) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1)).
call_with_args(_:M:A,A1) :- !,
	call_with_args(M:A,A1).
call_with_args(M:A,A1) :- !,
	'$call_with_args'(A,A1,M).
call_with_args(A,A1) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,M).
call_with_args(A,A1) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1)).
	
call_with_args(M:V,A1,A2) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1,A2)).
call_with_args(_:M:A,A1,A2) :- !,
	call_with_args(M:A,A1,A2).
call_with_args(M:A,A1,A2) :- !,
	'$call_with_args'(A,A1,A2,M).
call_with_args(A,A1,A2) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,M).
call_with_args(A,A1,A2) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1,A2)).
	
call_with_args(M:V,A1,A2,A3) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1,A2,A3)).
call_with_args(_:M:A,A1,A2,A3) :- !,
	call_with_args(M:A,A1,A2,A3).
call_with_args(M:A,A1,A2,A3) :- !,
	'$call_with_args'(A,A1,A2,A3,M).
call_with_args(A,A1,A2,A3) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,M).
call_with_args(A,A1,A2,A3) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1,A2,A3)).
	
call_with_args(M:V,A1,A2,A3,A4) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1,A2,A3,A4)).
call_with_args(_:M:A,A1,A2,A3,A4) :- !,
	call_with_args(M:A,A1,A2,A3,A4).
call_with_args(M:A,A1,A2,A3,A4) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,M).
call_with_args(A,A1,A2,A3,A4) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,M).
call_with_args(A,A1,A2,A3,A4) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1,A2,A3,A4)).
	
call_with_args(M:V,A1,A2,A3,A4,A5) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1,A2,A3,A4,A5)).
call_with_args(_:M:A,A1,A2,A3,A4,A5) :- !,
	call_with_args(M:A,A1,A2,A3,A4,A5).
call_with_args(M:A,A1,A2,A3,A4,A5) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,M).
call_with_args(A,A1,A2,A3,A4,A5) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,M).
call_with_args(A,A1,A2,A3,A4,A5) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5)).
	
call_with_args(M:V,A1,A2,A3,A4,A5,A6) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1,A2,A3,A4,A5,A6)).
call_with_args(_:M:A,A1,A2,A3,A4,A5,A6) :- !,
	call_with_args(M:A,A1,A2,A3,A4,A5,A6).
call_with_args(M:A,A1,A2,A3,A4,A5,A6) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,M).
call_with_args(A,A1,A2,A3,A4,A5,A6) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,M).
call_with_args(A,A1,A2,A3,A4,A5,A6) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6)).
	
call_with_args(M:V,A1,A2,A3,A4,A5,A6,A7) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1,A2,A3,A4,A5,A6,A7)).
call_with_args(_:M:A,A1,A2,A3,A4,A5,A6,A7) :- !,
	call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7).
call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6,A7)).
	
call_with_args(M:V,A1,A2,A3,A4,A5,A6,A7,A8) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1,A2,A3,A4,A5,A6,A7,A8)).
call_with_args(_:M:A,A1,A2,A3,A4,A5,A6,A7,A8) :- !,
	call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7,A8).
call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7,A8) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8)).
	
call_with_args(M:V,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1,A2,A3,A4,A5,A6,A7,A8,A9)).
call_with_args(_:M:A,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- !,
	call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7,A8,A9).
call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9)).
	

call_with_args(M:V,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- var(V), !,
	'$do_error'(instantiation_error,call_with_args(M:V,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)).
call_with_args(_:M:A,A1,A2,A3,A4,A5,A6,A7,A8,A10) :- !,
	call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7,A8,A10).
call_with_args(M:A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- !,
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- atom(A), !,
	'$current_module'(M),
	'$call_with_args'(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,M).
call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- 
	'$do_error'(type_error(atom,A),call_with_args(A,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)).
	

call_cleanup(Goal, Cleanup) :-
	call_cleanup(Goal, _Catcher, Cleanup).

call_cleanup(Goal, Catcher, Cleanup) :-
	catch('$call_cleanup'(Goal,Cleanup,_),
	      Exception,
	      '$cleanup_exception'(Exception,Catcher,Cleanup)).

'$cleanup_exception'(Exception, exception(Exception), Cleanup) :- !,
	'$clean_call'(Cleanup),
	throw(Exception).
'$cleanup_exception'(Exception, _, _) :-
	throw(Exception).

'$call_cleanup'(Goal, Cleanup, Result) :-
	'$freeze_goal'(Result, '$clean_call'(Cleanup)),
	yap_hacks:trail_suspension_marker(Result),
	(
	 yap_hacks:current_choice_point(CP0),
	 '$execute'(Goal),
	 yap_hacks:current_choice_point(CPF),
	 (
	  CP0 =:= CPF ->
	  Result = exit,
	  !
	 ;
	  true
	 )
	;
	 Result = fail,
	 fail
	).

'$holds_true'.

'$clean_call'(Cleanup) :-
	'$execute'(Cleanup), !.
'$clean_call'(_).

op(P,T,V) :-
	'$check_op'(P,T,V,op(P,T,V)),
	'$op'(P, T, V).

'$check_op'(P,T,V,G) :-
	(
	 var(P) ->
	 '$do_error'(instantiation_error,G)
	;
	 var(T) ->
	 '$do_error'(instantiation_error,G)
	;
	 var(V) ->
	 '$do_error'(instantiation_error,G)
	;
	 \+ integer(P) ->
	 '$do_error'(type_error(integer,P),G)
	;
	 \+ atom(T) ->
	 '$do_error'(type_error(atom,T),G)
	;
	 P < 0 ->
	 '$do_error'(domain_error(out_of_range,P),G)
	;
	 P > 1200 ->
	 '$do_error'(domain_error(out_of_range,P),G)
	;
	 \+ '$associativity'(T) ->
	 '$do_error'(domain_error(operator_specifier,T),G)
	;
	 '$check_op_name'(V,G)
	).

'$associativity'(xfx).
'$associativity'(xfy).
'$associativity'(yfx).
'$associativity'(yfy).
'$associativity'(xf).
'$associativity'(yf).
'$associativity'(fx).
'$associativity'(fy).

 '$check_op_name'(V,_) :-
	 atom(V), !.
 '$check_op_name'(M:A, G) :-
	 (
	  var(M) ->
	 '$do_error'(instantiation_error,G)
	 ;
	  var(A) ->
	 '$do_error'(instantiation_error,G)
	 ;
	  \+ atom(A) ->
	 '$do_error'(instantiation_error,G)
	 ;
	  \+ atom(M) ->
	 '$do_error'(instantiation_error,G)
	 ;
	  true
	 ).
 '$check_op_name'([A|As], G) :-
	  '$check_op_name'(A, G),
	  '$check_op_names'(As, G).

'$check_op_names'([], _).
'$check_op_names'([A|As], G) :-
	'$check_op_name'(A, G),
	'$check_op_names'(As, G).

	  
'$op'(P, T, [A|As]) :- !,
	'$opl'(P, T, [A|As]).
'$op'(P, T, A) :-
	'$op2'(P,T,A).

'$opl'(P, T, []).
'$opl'(P, T, [A|As]) :-
	'$op2'(P, T, A),
	'$opl'(P, T, As).

'$op2'(P,T,A) :-
	atom(A), !,
	'$opdec'(P,T,A,prolog).
'$op2'(P,T,A) :-
	strip_module(A,M,N),
	(M = user -> NM = prolog ; NM = M),
	'$opdec'(P,T,N,NM).

current_op(X,Y,V) :- var(V), !,
	'$current_module'(M),
	V = M:Z,
	'$do_current_op'(X,Y,Z,M).
current_op(X,Y,M:Z) :- !,
	'$current_opm'(X,Y,Z,M).
current_op(X,Y,Z) :-
	'$current_module'(M),
	'$do_current_op'(X,Y,Z,M).


'$current_opm'(X,Y,Z,M) :-
	var(Z), !,
	'$do_current_op'(X,Y,Z,M).
'$current_opm'(X,Y,M:Z,_) :- !,
	'$current_opm'(X,Y,Z,M).
'$current_opm'(X,Y,Z,M) :-
	'$do_current_op'(X,Y,Z,M).

'$do_current_op'(X,Y,Z,M) :-
	'$current_op'(X,Y,Z,M1),
	( M1 = prolog -> true ; M1 = M ).

%%% Operating System utilities

unix(V) :- var(V), !,
	'$do_error'(instantiation_error,unix(V)).
unix(argv(L)) :- '$is_list_of_atoms'(L,L), !, '$argv'(L).
unix(argv(V)) :-
	'$do_error'(type_error(atomic,V),unix(argv(V))).
unix(cd) :- cd('~').
unix(cd(V)) :- var(V), !,
	'$do_error'(instantiation_error,unix(cd(V))).
unix(cd(A)) :- atomic(A), !, cd(A).
unix(cd(V)) :-
	'$do_error'(type_error(atomic,V),unix(cd(V))).
unix(environ(X,Y)) :- '$do_environ'(X,Y).
unix(getcwd(X)) :- getcwd(X).
unix(shell(V)) :- var(V), !,
	'$do_error'(instantiation_error,unix(shell(V))).
unix(shell(A)) :- atom(A), !, '$shell'(A).
unix(shell(V)) :-
	'$do_error'(type_error(atomic,V),unix(shell(V))).
unix(system(V)) :- var(V), !,
	'$do_error'(instantiation_error,unix(system(V))).
unix(system(A)) :- atom(A), !, system(A).
unix(system(V)) :-
	'$do_error'(type_error(atom,V),unix(system(V))).
unix(shell) :- sh.
unix(putenv(X,Y)) :- '$putenv'(X,Y).


'$is_list_of_atoms'(V,_) :- var(V),!.
'$is_list_of_atoms'([],_) :- !.
'$is_list_of_atoms'([H|L],L0) :- !,
	'$check_if_head_may_be_atom'(H,L0),
	'$is_list_of_atoms'(L,L0).
'$is_list_of_atoms'(H,L0) :-
	'$do_error'(type_error(list,H),unix(argv(L0))).

'$check_if_head_may_be_atom'(H,_) :-
	var(H), !.
'$check_if_head_may_be_atom'(H,_) :-
	atom(H), !.
'$check_if_head_may_be_atom'(H,L0) :-
	'$do_error'(type_error(atom,H),unix(argv(L0))).


'$do_environ'(X, Y) :-
	var(X), !,
	'$do_error'(instantiation_error,unix(environ(X,Y))).
'$do_environ'(X, Y) :- atom(X), !,
	'$getenv'(X,Y).
'$do_environ'(X, Y) :-
	'$do_error'(type_error(atom,X),unix(environ(X,Y))).
	

putenv(Na,Val) :-
	'$putenv'(Na,Val).

getenv(Na,Val) :-
	'$getenv'(Na,Val).

%%% Saving and restoring a computation

save(A) :- var(A), !,
	'$do_error'(instantiation_error,save(A)).
save(A) :- atom(A), !, name(A,S), '$save'(S).
save(S) :- '$save'(S).

save(A,_) :- var(A), !,
	'$do_error'(instantiation_error,save(A)).
save(A,OUT) :- atom(A), !, name(A,S), '$save'(S,OUT).
save(S,OUT) :- '$save'(S,OUT).

save_program(A) :- var(A), !,
	'$do_error'(instantiation_error,save_program(A)).
save_program(A) :- atom(A), !, name(A,S), '$save_program'(S).
save_program(S) :- '$save_program'(S).

save_program(A, G) :- var(A), !,
	'$do_error'(instantiation_error,save_program(A,G)).
save_program(A, G) :- var(G), !,
	'$do_error'(instantiation_error,save_program(A,G)).
save_program(A, G) :- \+ callable(G), !,
	'$do_error'(type_error(callable,G),save_program(A,G)).
save_program(A, G) :-
	( atom(A) -> name(A,S) ; A = S),
	recorda('$restore_goal',G,R),
	'$save_program'(S),
	erase(R),
	fail.
save_program(_,_).

restore(A) :- var(A), !,
	'$do_error'(instantiation_error,restore(A)).
restore(A) :- atom(A), !, name(A,S), '$restore'(S).
restore(S) :- '$restore'(S).

%%% current ....

recordaifnot(K,T,R) :-
	recorded(K,T,R), % force non-det binding to R.
	'$still_variant'(R,T),
	!,
	fail.
recordaifnot(K,T,R) :-
	recorda(K,T,R).

recordzifnot(K,T,R) :-
	recorded(K,T,R),
	'$still_variant'(R,T),
	!,
	fail.
recordzifnot(K,T,R) :-
	recordz(K,T,R).

current_atom(A) :-				% check
	atom(A), !.
current_atom(A) :-				% generate
	'$current_atom'(A).
current_atom(A) :-				% generate
	'$current_wide_atom'(A).

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
	recorded('$unknown','$unknown'(_,MyOld),Ref), !,
	erase(Ref),
	'$cleanup_unknown_handler'(MyOld,Old),
	'$new_unknown'(New, Mod).
% store the new one.
'$unknown'(fail,New,Mod) :-
	'$new_unknown'(New, Mod).

'$valid_unknown_handler'(V,_) :-
	var(V), !,
	'$do_error'(instantiation_error,yap_flag(unknown,V)).
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
	'$do_error'(domain_error(flag_value,unknown+S),yap_flag(unknown,S)).


'$ask_unknown_flag'(Old) :-
	recorded('$unknown','$unkonwn'(_,MyOld),_), !,
	'$cleanup_unknwon_handler'(MyOld,Old).
'$ask_unknown_flag'(fail).

'$cleanup_unknown_handler'('$unknown_error'(_),error) :- !.
'$cleanup_unknown_handler'('$unknown_warning'(_),warning) :- !.
'$cleanup_unknown_handler'(Handler, Handler).

'$new_unknown'(fail,_) :- !.
'$new_unknown'(error,_) :- !,
	recorda('$unknown','$unknown'(P,'$unknown_error'(P)),_).
'$new_unknown'(warning,_) :- !,
	recorda('$unknown','$unknown'(P,'$unknown_warning'(P)),_).
'$new_unknown'(X,M) :-
	arg(1,X,A),
	recorda('$unknown','$unknown'(A,M:X),_).

'$unknown_error'(Mod:Goal) :-
	functor(Goal,Name,Arity),
	'$program_continuation'(PMod,PName,PAr),
	'$do_error'(existence_error(procedure,Name/Arity),context(Mod:Goal,PMod:PName/PAr)).

'$unknown_warning'(Mod:Goal) :-
	functor(Goal,Name,Arity),
	'$program_continuation'(PMod,PName,PAr),
	print_message(error,error(existence_error(procedure,Name/Arity), context(Mod:Goal,PMod:PName/PAr))),
	fail.

%%% Some "dirty" predicates

% Only efective if yap compiled with -DDEBUG
% this predicate shows the code produced by the compiler
'$show_code' :- '$debug'(0'f). %' just make emacs happy

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

garbage_collect_atoms :-
	'$atom_gc'.

'$force_environment_for_gc'.

'$good_list_of_character_codes'(V) :- var(V), !.
'$good_list_of_character_codes'([]).
'$good_list_of_character_codes'([X|L]) :-
	'$good_character_code'(X),
	'$good_list_of_character_codes'(L).

'$good_character_code'(X) :- var(X), !.
'$good_character_code'(X) :- integer(X), X > -2, X < 256.

atom_concat(X,Y,At) :-
	(
	  nonvar(X),  nonvar(Y)
	->
	  atom_concat([X,Y],At)
	;
	  atom(At) ->
	  atom_length(At,Len),
	  '$atom_contact_split'(At,0,Len,X,Y)
	;
	  var(At) ->
	  '$do_error'(instantiation_error,atom_concat(X,Y,At))
	;
	  '$do_error'(type_error(atom,At),atomic_concant(X,Y,At))
	).

atomic_concat(X,Y,At) :-
	(
	  nonvar(X),  nonvar(Y)
	->
	  atomic_concat([X,Y],At)
	;
	  atom(At) ->
	  atom_length(At,Len),
	  '$atom_contact_split'(At,0,Len,X,Y)
	;
	  number(At) ->
	  number_codes(At,Codes),
	  lists:append(X0,Y0,Codes),
	  name(X,X0),
	  name(Y,Y0)
	;
	  var(At) ->
	  '$do_error'(instantiation_error,atomic_concat(X,Y,At))
	;
	  '$do_error'(type_error(atomic,At),atomic_concant(X,Y,At))
	).

'$atom_contact_split'(At,Len,Len,X,Y) :- !,
	'$atom_split'(At,Len,X,Y).
'$atom_contact_split'(At,Len1,_,X,Y) :-
	'$atom_split'(At,Len1,X,Y).
'$atom_contact_split'(At,Len1,Len,X,Y) :-
	Len2 is Len1+1,
	'$atom_contact_split'(At,Len2,Len,X,Y).

sub_atom(At, Bef, Size, After, SubAt) :-
	% extract something from an atom
	atom(At), integer(Bef), integer(Size), !,
	'$sub_atom_extract'(At, Bef, Size, After, SubAt).
sub_atom(At, Bef, Size, After, SubAt) :-
	atom(At), !,
	atom_codes(At, Atl),
	'$sub_atom2'(Bef, Atl, Size, After, SubAt, sub_atom(At, Bef, Size, After, SubAt)).
sub_atom(At, Bef, Size, After, SubAt) :-
	var(At), !,
	'$do_error'(instantiation_error,sub_atom(At, Bef, Size,After, SubAt)).
sub_atom(At, Bef, Size, After, SubAt) :-
	\+ atom(At), !,
	'$do_error'(type_error(atom,At),sub_atom(At, Bef, Size,After, SubAt)).


'$sub_atom2'(Bef, Atl, Size, After, SubAt, ErrorTerm) :-
	var(Bef), !,
	'$sub_atombv'(Bef, Size, After, SubAt, Atl, ErrorTerm).
'$sub_atom2'(Bef, Atl, Size, After, SubAt, ErrorTerm) :-
	'$sub_atom_get_subchars'(Bef, Atl, NewAtl),
	'$sub_atom3'(Size, After, SubAt, NewAtl, ErrorTerm).

% if SubAt is bound, the rest is deterministic.
'$sub_atom3'(Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(SubAt), !,
	'$sub_atom_needs_atom'(SubAt,ErrorTerm),
	'$sub_atom_needs_int'(Size,ErrorTerm),
	'$sub_atom_needs_int'(After,ErrorTerm),
	atom_codes(SubAt,Atls),
	'$$_length1'(Atls, 0, Size),
	'$sub_atom_get_subchars_and_match'(Size, Atl, Atls, NAtl),
	'$$_length1'(NAtl,0,After).
% SubAt is unbound, but Size is bound
'$sub_atom3'(Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(Size), !,
	'$sub_atom_needs_int'(Size,ErrorTerm),
	'$sub_atom_needs_int'(After,ErrorTerm),
	'$sub_atom_get_subchars_and_match'(Size, Atl, SubAts, NAtl),
	'$$_length1'(NAtl,0,After),
	atom_codes(SubAt,SubAts).
% SubAt and Size are unbound, but After is bound.
'$sub_atom3'(Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(After), !,
	'$sub_atom_needs_int'(After,ErrorTerm),
	'$sub_atom_get_last_subchars'(Atl,SubAts,After,Total,Size),
	Total >= After,
	atom_codes(SubAt,SubAts).
% SubAt, Size, and After are unbound.
'$sub_atom3'(Size, After, SubAt, Atl, _) :-
	'$$_length1'(Atl,0,Len),
	'$sub_atom_split'(Atl,Len,SubAts,Size,_,After),
	atom_codes(SubAt,SubAts).

% Bef is unbound, so we've got three hypothesis
% ok: in the best case we just try to find SubAt in  the original atom.
'$sub_atombv'(Bef, Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(SubAt), !,
	'$sub_atom_needs_atom'(SubAt, ErrorTerm),
	atom_codes(SubAt,SubAts),
	'$sub_atom_search'(SubAts, Atl, 0, Bef, AfterS),
	'$$_length1'(SubAts, 0, Size),
	'$$_length1'(AfterS, 0, After).
% ok: in the second best case we just get rid of the tail
'$sub_atombv'(Bef, Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(After), !,
	'$sub_atom_needs_int'(After, ErrorTerm),
	'$sub_atom_get_last_subchars'(Atl,SubAt0,After,Total,Size0),
	Total >= After,
	'$sub_atom_split'(SubAt0,Size0,_,Bef,SubAts,Size),
	atom_codes(SubAt,SubAts).
% ok: just do everything
'$sub_atombv'(Bef, Size, After, SubAt, Atl, _) :-
	'$$_length1'(Atl, 0, Len),
	'$sub_atom_split'(Atl,Len,_,Bef,Atls2,Len2),
	'$sub_atom_split'(Atls2,Len2,SubAts,Size,_,After),
	atom_codes(SubAt,SubAts).

'$sub_atom_search'([], AfterS, BefSize, BefSize, AfterS).
'$sub_atom_search'([C|SubAts], [C|Atl], BefSize, BefSize, AfterS) :-
	'$sub_atom_search2'(SubAts, Atl, AfterS).
'$sub_atom_search'([C|SubAts], [_|Atl], BefSize, BefSizeF, AfterS) :-
	NBefSize is BefSize+1,
	'$sub_atom_search'([C|SubAts], Atl, NBefSize, BefSizeF, AfterS).

'$sub_atom_search2'([], AfterS, AfterS).
'$sub_atom_search2'([C|SubAts], [C|Atl], AfterS) :-
	'$sub_atom_search2'(SubAts, Atl, AfterS).

'$sub_atom_get_subchars'(0, Atl, Atl) :- !.
'$sub_atom_get_subchars'(I0, [_|Atl], NAtl) :-
	I is I0-1,
	'$sub_atom_get_subchars'(I, Atl, NAtl).

'$sub_atom_get_subchars'(0, Atl, [], Atl) :- !.
'$sub_atom_get_subchars'(I0, [C|Atl], [C|L], NAtl) :-
	I is I0-1,
	'$sub_atom_get_subchars'(I, Atl, L, NAtl).

'$sub_atom_get_subchars_and_match'(0, Atl, [], Atl) :- !.
'$sub_atom_get_subchars_and_match'(I0, [C|Atl], [C|Match], NAtl) :-
	I is I0-1,
	'$sub_atom_get_subchars_and_match'(I, Atl, Match, NAtl).

'$sub_atom_check_length'([],0).
'$sub_atom_check_length'([_|L],N1) :-
	N1 > 0,
	N is N1-1,
	'$sub_atom_check_length'(L,N).	

'$sub_atom_get_last_subchars'([],[],_,0,0).
'$sub_atom_get_last_subchars'([C|Atl],SubAt,After,Total,Size) :-
	'$sub_atom_get_last_subchars'(Atl,SubAt0,After,Total0,Size0),
	Total is Total0+1,
	( Total > After ->
	    Size is Size0+1, SubAt = [C|SubAt0]
	 ;
	    Size = Size0, SubAt = SubAt0
	).

'$sub_atom_split'(Atl,After,[],0,Atl,After).
'$sub_atom_split'([C|Atl],Len,[C|Atls],Size,NAtl,After) :-
	Len1 is Len-1,
	'$sub_atom_split'(Atl,Len1,Atls,Size0,NAtl,After),
	Size is Size0+1.
	
'$sub_atom_needs_int'(V,_) :- var(V), !.
'$sub_atom_needs_int'(I,_) :- integer(I), I >= 0, !.
'$sub_atom_needs_int'(I,ErrorTerm) :- integer(I), !,
	'$do_error'(domain_error(not_less_than_zero,I),ErrorTerm).
'$sub_atom_needs_int'(I,ErrorTerm) :-
	'$do_error'(type_error(integer,I),ErrorTerm).

'$sub_atom_needs_atom'(V,_) :- var(V), !.
'$sub_atom_needs_atom'(A,_) :- atom(A), !.
'$sub_atom_needs_atom'(A,ErrorTerm) :-
	'$do_error'(type_error(atom,A),ErrorTerm).

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
	'$do_error'(instantiation_error,initialization(G)).
prolog_initialization(T) :- callable(T), !,
	'$assert_init'(T).
prolog_initialization(T) :-
	'$do_error'(type_error(callable,T),initialization(T)).

'$assert_init'(T) :- recordz('$startup_goal',T,_), fail.
'$assert_init'(_).

version :- '$version'.

version(V) :- var(V),  !,
	'$do_error'(instantiation_error,version(V)).
version(T) :- atom(T), !, '$assert_version'(T).
version(T) :-
	'$do_error'(type_error(atom,T),version(T)).

'$assert_version'(T) :- recordz('$version',T,_), fail.
'$assert_version'(_).

'$set_toplevel_hook'(_) :- 
	recorded('$toplevel_hooks',_,R),
	erase(R),
	fail.
'$set_toplevel_hook'(H) :- 
	recorda('$toplevel_hooks',H,_),
	fail.
'$set_toplevel_hook'(_).

halt(X) :- '$halt'(X).

halt :-
	print_message(informational, halt),
	'$halt'(0).

halt(X) :-
	'$halt'(X).

nth_instance(X,Y,Z) :-
	nonvar(X), var(Y), var(Z), !,
	recorded(X,_,Z),
	'$nth_instance'(_,Y,Z).
nth_instance(X,Y,Z) :-
	'$nth_instance'(X,Y,Z).

'$run_atom_goal'(GA) :-
	'$current_module'(Module),
	atom_codes(GA,Gs0),
	'$add_dot_to_atom_goal'(Gs0,Gs),
	charsio:open_mem_read_stream(Gs, Stream),
	( '$system_catch'(read(Stream, G),Module,_,fail) ->
	    close(Stream)
	;
	    close(Stream),
	    fail
	),
	'$system_catch'('$query'(once(G), []),Module,Error,user:'$Error'(Error)).

'$add_dot_to_atom_goal'([],[0'.]) :- !.
'$add_dot_to_atom_goal'([0'.],[0'.]) :- !.
'$add_dot_to_atom_goal'([C|Gs0],[C|Gs]) :-
	'$add_dot_to_atom_goal'(Gs0,Gs).


prolog_current_frame(Env) :-
	Env is '$env'.

nb_current(GlobalVariable, Val) :-
	var(GlobalVariable), !,
	'$nb_current'(GlobalVariable),
	nb_getval(GlobalVariable, Val).
nb_current(GlobalVariable, Val) :-
	nb_getval(GlobalVariable, Val).

