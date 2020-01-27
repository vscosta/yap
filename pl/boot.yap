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

print_message(informational,_) :-
	yap_flag(verbose, silent),
	!.
print_message(informational,E) :-
	format('informational message ~q.~n',[E]),
	!.
%%
% boot:print_message( Type, Error )
%

print_message(Type,error(_,exception(Desc))) :-
    !,
    '$print_exception'(Desc).
print_message(Type,error(warning(_,_),exception(Desc))) :-
	!,
	'$print_exception'(Desc).
print_message(Type,Error) :-
	format( user_error, '~w while bootstraping: event is ~q~n',[Type,Error]).


/**
* @pred system_module( _Mod_, _ListOfPublicPredicates, ListOfPrivatePredicates * 
 * Define a system module _Mod_. _ListOfPublicPredicates_ . Currentlt, all
 * predicates are in the 'prolog' module. The first
 * are visible outside the Prolog module, all others are hidden at the end of booting.
 *
*/
system_module(Mod, SysExps) :-
    system_module(Mod, SysExps, []).

system_module(_Mod, _SysExps, _Decls) :- !.
system_module(_Mod, _SysExps, _Decls) :-
      % '$new_system_predicates'(SysExps),
       fail.
system_module(_Mod, _SysExps, _Decls) :-
    stream_property(loop_stream,[file_name(File)]),
    !,
    recordz(system_file, File, _ ).
system_module(_Mod, _SysExps, _Decls) :-
    recordz(system_file, loop_stream, _ ).

'$new_system_predicates'([]).
'$new_system_predicates'([N/Ar|_Ps])  :-
    '$new_system_predicate'(N, Ar, prolog).
'$new_system_predicates'([_P|Ps]) :-
    '$new_system_predicates'(Ps).

use_system_module(_Module, _SysExps).

private(_).

%
% boootstrap predicates.
%
:- system_module( '$_boot', [
	!/0,
        ':-'/1,
        '?-'/1,
        []/0,
        bootstrap/1,
        call/1,
        catch/3,
        catch_ball/2,
        expand_term/2,
	print_message/2,
        import_system_module/2,
	system_module/2,
	private/1,
        incore/1,
        (not)/1,
        repeat/0,
        throw/1,
	true/0,
        extensions_to_present_answer/1,
        fail/0,
        false/0,
        goal_expansion/2,
        goal_expansion/3,
        otherwise/0,
        term_expansion/2,
        version/2],
	[
	'$do_log_upd_clause'/6,
        '$do_log_upd_clause0'/6,
        '$do_log_upd_clause_erase'/6,
        '$do_static_clause'/5,
        '$system_module'/1]).


% be careful here not to generate an undefined exception..



print_boot_message(Type,Error,Desc) :-		
	'$query_exception'(parserFile, Desc, File),
	'$query_exception'(parserLine, Desc, FilePos),
	!,
	format(user_error,'~a:~d:  ~a: ~q~n', [File,FilePos,Type,Error]).
print_boot_message(Type,Error,Desc) :-		
	'$query_exception'(prologPredFile, Desc, File),
	'$query_exception'(prologPredLine, Desc, FilePos),
	format(user_error,'~a:~d:  ~a: ~q~n', [File,FilePos,Type,Error]).
print_boot_message(Type,Error,Desc) :-
	'$query_exception'(errorFile, Desc, File),
	'$query_exception'(errorLine, Desc, FilePos),
	format(user_error,'~a:~d:  ~a: ~q~n', [File,FilePos,Type,Error]).

'$undefp0'(MG, _Action) :-
	strip_module(MG,M,G),
	functor(G,N,A),
	print_message( error, error(error(unknown, M:N/A),MG)),
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
'$do_log_upd_clause'(A,B,C,D,E,F) :-
	'$continue_log_update_clause'(A,B,C,D,E,F).
'$do_log_upd_clause'(_,_,_,_,_,_).


'$do_log_upd_clause_erase'(_,_,_,_,_,_).
'$do_log_upd_clause_erase'(A,B,C,D,E,F) :-
	'$continue_log_update_clause_erase'(A,B,C,D,E,F).
'$do_log_upd_clause_erase'(_,_,_,_,_,_).

'$do_log_upd_clause0'(_,_,_,_,_,_).
'$do_log_upd_clause0'(A,B,C,D,_,_) :-
	'$continue_log_update_clause'(A,B,C,D).
'$do_log_upd_clause0'(_,_,_,_,_,_).



'$do_static_clause'(_,_,_,_,_,_).
'$do_static_clause'(A,B,C,D,E,F) :-
	'$continue_static_clause'(A,B,C,D,E,F).
'$do_static_clause'(_,_,_,_,_,_).


:- c_compile('directives.yap').
:- c_compile('init.yap').

'$command'(C,M,VL,Pos,Con) :-
    current_prolog_flag(strict_iso, true), !,      /* strict_iso on */
    '$yap_strip_module'(M:C, EM, EG),
    '$execute_command'(EG,EM,VL,Pos,Con,_Source).
'$command'(C,M,VL,Pos,Con) :-
    ( (Con = top ; var(C) ; C = [_|_])  ->
      '$yap_strip_module'(M:C, EM, EG),
      '$execute_command'(EG,EM,VL,Pos,Con,C) ;
      % do term expansion
      '$expand_term'(C, Con, EC),
      ( nonvar(EC) ->
	'$yap_strip_module'(EC, EM2, EG2)
      ;
      '$yap_strip_module'(C, EM2, EG2)
      ),
      % execute a list of commands
      '$execute_commands'(EG2,EM2,VL,Pos,Con,_Source)
    ),
    % succeed only if the *original* was at end of file.
    C == end_of_file.

%% we're coming back from external code to a debugger call.
%%
'$reenter_debugger'(!) :- !.
'$reenter_debugger'(_) :-
    '$stop_creeping'(_),
    '$set_debugger_state'(debug, false).

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
:- c_compile('error.yap').

:- c_compile('absf.yap' ).


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

