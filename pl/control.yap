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
* File:		control.yap						 *
* Last rev:     20/08/09						 *
* mods:									 *
* comments:	control predicates available in yap			 *
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

call_cleanup(Goal, Cleanup) :-
	setup_call_catcher_cleanup(true, Goal, _Catcher, Cleanup).

call_cleanup(Goal, Catcher, Cleanup) :-
	setup_call_catcher_cleanup(true, Goal, Catcher, Cleanup).

setup_call_cleanup(Setup, Goal, Cleanup) :-
	setup_call_catcher_cleanup(Setup, Goal, _Catcher, Cleanup).

setup_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup) :-
	yap_hacks:disable_interrupts,
	'$check_goal_for_setup_call_cleanup'(Setup, setup_call_cleanup(Setup, Goal, Cleanup)),
	catch('$do_setup'(Setup),Exception,'$handle_broken_setup'(Exception)),
	'$check_goal_for_setup_call_cleanup'(Cleanup, setup_call_cleanup(Setup, Goal, Cleanup)),
	'$safe_call_cleanup'(Goal,Cleanup,Catcher,Exception).

% make sure we don't lose interrupts if we get exceptions
% with setup.
'$handle_broken_setup'(Exception) :-
	yap_hacks:enable_interrupts,
	throw(Exception).

'$check_goal_for_setup_call_cleanup'(Goal, G) :-
	strip_module(Goal, _, MG),
	(
	 var(MG)
	->
	 yap_hacks:enable_interrupts,
	 '$do_error'(instantiation_error,G)
	;
	 true
	).

% this is simple, do nothing
'$do_setup'(A:true) :- atom(A), !.
% this is tricky: please don't forget that interrupts are disabled at this point
% and that they will only be enabled after setting up Cleanup
'$do_setup'(Setup) :-
	(
	 '$execute'(Setup),
	 % we don't need to care about enabling interrupts
	 !
	;
	 % reenable interrupts if Setup failed
	 yap_hacks:enable_interrupts,
	 fail
	).
	 

'$cleanup_exception'(Exception, exception(Exception), Cleanup) :- !,
	% whatever happens, let exception go through 
	catch('$clean_call'(_,Cleanup),_,true),
	throw(Exception).
'$cleanup_exception'(Exception, _, _) :-
	throw(Exception).

'$safe_call_cleanup'(Goal, Cleanup, Catcher, Exception) :-
	yap_hacks:current_choice_point(MyCP1),
	'$coroutining':freeze_goal(Catcher, '$clean_call'(Active, Cleanup)),
	(
	 yap_hacks:trail_suspension_marker(Catcher),
	 yap_hacks:enable_interrupts,
	 yap_hacks:current_choice_point(CP0),
	 '$execute'(Goal),
	 % ensure environment for delayed variables in Goal
	 '$true',
	 yap_hacks:current_choice_point(CPF),
	 (
	  CP0 =:= CPF
	 ->
	  Catcher = exit,
	  !
	 ;
	  true
	 )
	;
	 Catcher = fail,
	 fail
	).

'$holds_true'.

% The first argument is used by JumpEnv to verify if a throw
% is going to be handled by the cleanup catcher. If it is so,
% clean_call will not be called from JumpToEnv.
'$clean_call'(_, Cleanup) :-
	'$execute'(Cleanup), !.
'$clean_call'(_, _).

'$cc_check_throw' :-
	nb_getval('$catch',Ball),
	throw(Ball).	

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
	recorded('$unknown','$unknown'(_,MyOld),_), !,
	'$cleanup_unknown_handler'(MyOld,Old).
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

(initialization) :-
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

halt :-
	print_message(informational, halt),
	'$halt'(0).

halt(X) :-
	'$halt'(X).

prolog_current_frame(Env) :-
	Env is '$env'.

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

'$add_dot_to_atom_goal'([],[0'.]) :- !. %'
'$add_dot_to_atom_goal'([0'.],[0'.]) :- !.
'$add_dot_to_atom_goal'([C|Gs0],[C|Gs]) :-
	'$add_dot_to_atom_goal'(Gs0,Gs).


