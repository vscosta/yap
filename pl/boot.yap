/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
*									 *
**************************************************************************
*								         *
* File:		boot.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* commen    ts:	boot file for Prolog					 *
*									 *
*************************************************************************/

/**
  @file boot.yap
  @brief YAP bootstrap


  @addtogroup TopLevel Top-Level and Boot Predicates

  @ingroup builtins
  @{


*/


system_module(_Mod, _SysExps, _Decls).
%    new_system_module(Mod).

use_system_module(_Module, _SysExps).

private(_).

%
% boootstrap predicates.
%
:- system_module( '$_boot', [
        bootstrap/1,
        call/1,
        catch/3,
        catch_ball/2,
        expand_term/2,
        import_system_module/2,
        incore/1,
        (not)/1,
        repeat/0,
        throw/1,
        true/0], ['$$compile'/4,
        '$call'/4,
        '$catch'/3,
        '$check_callable'/2,
        '$check_head_and_body'/4,
        '$check_if_reconsulted'/2,
        '$clear_reconsulting'/0,
        '$command'/4,
        '$cut_by'/1,
        '$disable_debugging'/0,
        '$do_live'/0,
        '$'/0,
        '$find_goal_definition'/4,
        '$head_and_body'/3,
        '$inform_as_reconsulted'/2,
        '$init_system'/0,
        '$init_win_graphics'/0,
        '$loop'/2,
        '$meta_call'/2,
        '$prompt_alternatives_on'/1,
        '$run_at_thread_start'/0,
        '$system_catch'/4,
        '$undefp'/1,
		  '$version'/0]).

:- use_system_module( '$_absf', ['$system_library_directories'/2]).

:- use_system_module( '$_checker', ['$check_term'/5,
        '$sv_warning'/2]).

:- use_system_module( '$_consult', ['$csult'/2]).

:- use_system_module( '$_control', ['$run_atom_goal'/1]).

:- use_system_module( '$_directives', ['$all_directives'/1,
        '$exec_directives'/5]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_grammar', ['$translate_rule'/2]).

:- use_system_module( '$_modules', ['$get_undefined_pred'/4,
        '$meta_expansion'/6,
        '$module_expansion'/6]).

:- use_system_module( '$_preddecls', ['$dynamic'/2]).

:- use_system_module( '$_preds', ['$assert_static'/5,
				  '$assertz_dynamic'/4,
        '$init_preds'/0,
        '$unknown_error'/1,
        '$unknown_warning'/1]).

:- use_system_module( '$_qly', ['$init_state'/0]).

:- use_system_module( '$_strict_iso', ['$check_iso_strict_clause'/1,
        '$iso_check_goal'/2]).

% be careful here not to generate an undefined exception..

print_message(L,E) :-
 %stop_low_level_trace,
	'$number_of_clauses'(print_message(L,E), prolog_complete, 1),
	!,
	(L = informational
	->
	 true
	;
	error(_,Info),	
	'$error_descriptor'(Info, Desc),
  query_exception(prologPredFile, Desc, File),
  query_exception(prologPredLine, Desc, FilePos),
  format(user_error,'~a:~d:  error:', [File,FilePos]),
  '$print_exception'(Info),
	format( user_error, '~w from bootstrap: got ~w~n',[L,E])
	).

'$undefp0'([M|G], _Action) :-
    stream_property( loop_stream, [file_name(F), line_number(L)]),
    format(user_error,'~a:~d:  error: undefined ~w~n:',[F,L,M:G]),
    fail
    ;
    format(user_error,' call to ~w~n',[M:G]),
	fail.

:- '$undefp_handler'('$undefp0'(_,_),prolog).

/**
  * @pred $system_meta_predicates'( +L )
  *
  * @param L declare a set of system meta-predicates
  *
  * @return system predicates
*/
'$system_meta_predicates'([]).
'$system_meta_predicates'([P|L]) :-
	functor(P,N,A),
	'$new_meta_pred'(P, prolog),
	G = ('$meta_predicate'(N,_M2,A,P) :- true),
	'$compile'(G, assertz, G, prolog, _R),
	'$system_meta_predicates'(L).

  :- '$mk_dynamic'( prolog_file_type(_Ext, _NType), user).
  :- '$new_multifile'( prolog_file_type(_Ext, _NType), user).

  :- '$mk_dynamic'( '$meta_predicate'(_N,_M,_A,_P), prolog).
  :- '$new_multifile'( '$meta_predicate'(_N,_M,_A,_P), prolog).

:-  '$new_multifile'('$full_clause_optimisation'(_H, _M, _B0, _BF), prolog).
:-  '$new_multifile'('$exec_directive'(_,_,_,_,_), prolog).

:- system_module( '$_init', [!/0,
        ':-'/1,
        '?-'/1,
        []/0,
        extensions_to_present_answer/1,
        fail/0,
        false/0,
        goal_expansion/2,
        goal_expansion/3,
        otherwise/0,
        term_expansion/2,
        version/2,
	    '$do_log_upd_clause'/6,
        '$do_log_upd_clause0'/6,
        '$do_log_upd_clause_erase'/6,
        '$do_static_clause'/5], [
        '$system_module'/1]).

:- use_system_module( '$_boot', ['$cut_by'/1]).


%:- start_low_level_trace.

% This is the YAP init file
% should be consulted first step after booting


:-  yap_flag(prolog:unknown, error).

:- c_compile('top.yap').

% These are pseudo declarations
% so that the user will get a redefining system predicate

% just create a choice-point
% the 6th argument marks the time-stamp.
'$do_log_upd_clause'(_,_,_,_,_,_).
'$do_log_upd_clause'(A,B,C,D,E,_) :-
	'$continue_log_update_clause'(A,B,C,D,E).
'$do_log_upd_clause'(_,_,_,_,_,_).


'$do_log_upd_clause_erase'(_,_,_,_,_,_).
'$do_log_upd_clause_erase'(A,B,C,D,E,_) :-
	'$continue_log_update_clause_erase'(A,B,C,D,E).
'$do_log_upd_clause_erase'(_,_,_,_,_,_).

'$do_log_upd_clause0'(_,_,_,_,_,_).
'$do_log_upd_clause0'(A,B,C,D,_,_) :-
	'$continue_log_update_clause'(A,B,C,D).
'$do_log_upd_clause0'(_,_,_,_,_,_).


'$do_static_clause'(_,_,_,_,_).
'$do_static_clause'(A,B,C,D,E) :-
	'$continue_static_clause'(A,B,C,D,E).
'$do_static_clause'(_,_,_,_,_).


:- c_compile('directives.yap').
:- c_compile('init.yap').

'$command'(C,VL,Pos,Con) :-
	current_prolog_flag(strict_iso, true), !,      /* strict_iso on */
	 '$yap_strip_module'(C, EM, EG),
   '$execute_command'(EG,EM,VL,Pos,Con,_Source).
'$command'(C,VL,Pos,Con) :-
	( (Con = top ; var(C) ; C = [_|_])  ->
	 '$yap_strip_module'(C, EM, EG),
	  '$execute_command'(EG,EM,VL,Pos,Con,C) ;
	  % do term expansion
	  '$expand_term'(C, Con, EC),
    '$yap_strip_module'(EC, EM2, EG2),
	  % execute a list of commands
	  '$execute_commands'(EG2,EM2,VL,Pos,Con,_Source)
	),
	  % succeed only if the *original* was at end of file.
	  C == end_of_file.

:- c_compile('arith.yap').
%:- stop_low_level_trace.

:- compile_expressions.


:- c_compile('imports.yap').
:- c_compile('bootutils.yap').
:- c_compile('bootlists.yap').
:- c_compile('consult.yap').
:- c_compile('preddecls.yap').
:- c_compile('meta.yap').
:- c_compile('metadecls.yap').
:- c_compile('preddyns.yap').
:- c_compile('builtins.yap').
:- c_compile('newmod.yap').

:- c_compile('atoms.yap').
:- c_compile('os.yap').
:- c_compile('errors.yap').

%%
% @pred initialize_prolog
%
% User-interface to Prolog bootstrap routine.
%
initialize_prolog :-
	'$init_prolog'.

:- set_prolog_flag(verbose, silent).
%:- set_prolog_flag(verbose_file_search, true ).
%:- yap_flag(write_strings,on).
:- c_compile( 'preds.yap' ).
:- c_compile( 'modules.yap' ).
:- c_compile( 'grammar.yap' ).
:- c_compile( 'protect.yap' ).

:- ['absf.yap'].

:- use_module('error.yap').

:- [
    'utils.yap',
    'control.yap',
    'flags.yap'
].


:- [
    % lists is often used.
   	 '../os/yio.yap',
	 'debug.yap',
	 'checker.yap',
	 'depth_bound.yap',
	 'ground.yap',
	 'listing.yap',
    'arithpreds.yap',
	 % modules must be after preds, otherwise we will have trouble
	 % with meta-predicate expansion being invoked
	 % must follow grammar
	 'eval.yap',
	 'signals.yap',
	 'profile.yap',
	 'callcount.yap',
	 'load_foreign.yap',
%	 'save.yap',
	 'setof.yap',
	 'sort.yap',
	 'statistics.yap',
	 'strict_iso.yap',
	 'tabling.yap',
 	 'threads.yap',
	 'eam.yap',
	 'yapor.yap',
     'qly.yap',
     'spy.yap',
     'udi.yap',
     'boot2.yap'].

%% @}

