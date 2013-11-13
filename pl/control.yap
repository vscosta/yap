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
	'$current_choice_point'(MyCP1),
	'$coroutining':freeze_goal(Catcher, '$clean_call'(Active, Cleanup)),
	(
	 yap_hacks:trail_suspension_marker(Catcher),
	 yap_hacks:enable_interrupts,
	 '$current_choice_point'(CP0),
	 '$execute'(Goal),
         '$stop_creeping',
	 '$current_choice_point'(CPF),
	 (
	  CP0 =:= CPF
	 ->
	  Catcher = exit,
	  !
	 ;
	  true
	 )
	;
         '$stop_creeping',
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
	'$nb_getval'('$catch', Ball, fail),
	throw(Ball).	

%%% The unknown predicate,
%	informs about what the user wants to be done when
%	there are no clauses for a certain predicate */

unknown(V0, V) :-
	prolog_flag(unknown, V0, V).

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

nb_getval(GlobalVariable, Val) :-
	'$nb_getval'(GlobalVariable, Val, Error),
	(var(Error)
	->
	 true
	;
	 '$getval_exception'(GlobalVariable, Val, nb_getval(GlobalVariable, Val)) ->
	 nb_getval(GlobalVariable, Val)
	;
	 '$do_error'(existence_error(variable, GlobalVariable),nb_getval(GlobalVariable, Val))
	).
		    

b_getval(GlobalVariable, Val) :-
	'$nb_getval'(GlobalVariable, Val, Error),
	(var(Error)
	->
	 true
	;
	 '$getval_exception'(GlobalVariable, Val, b_getval(GlobalVariable, Val)) ->
	 true
	;
	 '$do_error'(existence_error(variable, GlobalVariable),b_getval(GlobalVariable, Val))
	).


/* This is the break predicate,
	it saves the importante data about current streams and
	debugger state */

break :-
	'$init_debugger',
	nb_getval('$system_mode',SystemMode),
	nb_getval('$trace',Trace),
	nb_setval('$trace',off),
	nb_getval('$debug_jump',Jump),
	nb_getval('$debug_run',Run),
	'$swi_current_prolog_flag'(debug, Debug),
	'$swi_set_prolog_flag'(debug, false),
	nb_getval('$break',BL), NBL is BL+1,
	nb_getval('$spy_gn',SPY_GN),
	b_getval('$spy_glist',GList),
	b_setval('$spy_glist',[]),
	nb_setval('$break',NBL),
	current_output(OutStream), current_input(InpStream),
	format(user_error, '% Break (level ~w)~n', [NBL]),
	'$do_live',
	!,
	set_value('$live','$true'),
	b_setval('$spy_glist',GList),
	nb_setval('$spy_gn',SPY_GN),
	set_input(InpStream), 
	set_output(OutStream),
	'$swi_set_prolog_flag'(debug, Debug),
	nb_setval('$debug_jump',Jump),
	nb_setval('$debug_run',Run),
	nb_setval('$trace',Trace),
	nb_setval('$break',BL),
	nb_setval('$system_mode',SystemMode).


at_halt(G) :-
	recorda('$halt', G, _),
	fail.
at_halt(_).

halt :-
	print_message(informational, halt),
	fail.
halt :-
	'$halt'(0).

halt(_) :-
	recorded('$halt', G, _),
	call(G),
	fail.
halt(X) :-
	'$sync_mmapped_arrays',
	set_value('$live','$false'),
	'$halt'(X).

prolog_current_frame(Env) :-
	Env is '$env'.

'$run_atom_goal'(GA) :-
	'$current_module'(Module),
	atom_to_term(GA, G, _),
	'$system_catch'('$query'(once(G), []),Module,Error,user:'$Error'(Error)).

'$add_dot_to_atom_goal'([],[0'.]) :- !. %'
'$add_dot_to_atom_goal'([0'.],[0'.]) :- !.
'$add_dot_to_atom_goal'([C|Gs0],[C|Gs]) :-
	'$add_dot_to_atom_goal'(Gs0,Gs).


