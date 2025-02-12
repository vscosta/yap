/*************************************************************************
*
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		directives.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	directing system execution				 *
*									 *
*************************************************************************/

/**
  * @file   directives.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 11:47:38 2017
  *
  * @brief  Control File Loading
  %
  % @defgroup Directives Prolog Directives
  % @ingroup YAPConsulting
  *
  * Directives are instructions to start or change the compilation process.
  * @{
*/


%:- system_module( '$_directives', [user_defined_directive/2], ['$all_directives'/1,
 %       '$exec_directives'/5]).

:- use_system_module( '$_boot', ['$command'/4,
        '$system_catch'/4]).

:- use_system_module( '$_consult', ['$elif'/2,
        '$else'/1,
        '$endif'/1,
        '$if'/2,
        '$include'/2,
        '$initialization'/1,
        '$initialization'/2,
        '$require'/2,
        '$set_encoding'/1,
        '$use_module'/3]).

:- use_system_module( '$_modules', ['$meta_predicate'/2,
        '$module'/3,
        '$module'/4,
        '$module_transparent'/2]).

:- use_system_module( '$_preddecls', ['$discontiguous'/2,
        '$dynamic'/2]).

:- use_system_module( '$_preds', ['$noprofile'/2,
        '$public'/2]).

:- use_system_module( '$_threads', ['$thread_local'/2]).

'$all_directives'(_:G1) :- !,
	'$all_directives'(G1).
'$all_directives'((G1,G2)) :- !,
	'$all_directives'(G1),
	'$all_directives'(G2).
'$all_directives'(G) :- !,
	'$directive'(G).

%:- '$multifile'( '$directive'/1, prolog ).
%:- multifile prolog:'$exec_directive'/5, prolog:'$directive'/1.


'$directive'(block(_)).
'$directive'(char_conversion(_,_)).
'$directive'(compile(_)).
'$directive'(consult(_)).
'$directive'(discontiguous(_)).
'$directive'(dynamic(_)).
'$directive'(elif(_)).
'$directive'(else).
'$directive'(encoding(_)).
'$directive'(endif).
'$directive'(ensure_loaded(_)).
'$directive'(expects_dialect(_)).
'$directive'(if(_)).
'$directive'(include(_)).
'$directive'(initialization(_)).
'$directive'(initialization(_,_)).
'$directive'(license(_)).
'$directive'(meta_predicate(_)).
'$directive'(module(_,_)).
'$directive'(module_transparent(_)).
'$directive'(multifile(_)).
'$directive'(noprofile(_)).
'$directive'(public(_)).
'$directive'(op(_,_,_)).
'$directive'(require(_)).
'$directive'(set_prolog_flag(_,_)).
'$directive'(reconsult(_)).
'$directive'(reexport(_)).
'$directive'(reexport(_,_)).
'$directive'(system_module(_,_,_)).
'$directive'(predicate_options(_,_,_)).
'$directive'(thread_initialization(_)).
'$directive'(thread_local(_)).
'$directive'(uncutable(_)).
'$directive'(use_module(_)).
'$directive'(use_module(_,_)).
'$directive'(use_module(_,_,_)).
'$directive'(wait(_)).

/** @pred initialization(+ _G_) is iso

Theu compiler will execute goals  _G_ after consulting the current
file. Only the first answer is
considered.

Notice that the goal will execute in the calling context, not within the file context,
In other words, the source module and execution directory will be the ones of the parent
environment. Use initialization/2 for more flexible behavior.

*/
/** @pred initialization(+ _Goal_,+ _When_)

Similar to initialization/1, but allows  specifying when
 _Goal_ is executed while loading the program-text:


    + now
      Execute  _Goal_ immediately.

    + after_load
      Execute  _Goal_ after loading program-text. This is the same as initialization/1.

    + restore
      Do not execute  _Goal_ while loading the program, but only when restoring a state (not implemented yet).

*/
/*
'$exec_directive'(system_module(N,Ps,Ss), _Status, HostM, Log, Pos) :-
    (
current_prolog_flag(compiler_top_level, scanner:scanner_loop)
    ->
      current_source_module(_,prolog)
    ;
      '$declare_system_module'(HostM,N,Ps,Ss,Log,Pos)
).
*/

predicate_options(PI, Arg, Options) :-
    '__NB_getval__'('$consulting_file', _, fail),
	predopts:expand_predicate_options(PI, Arg, Options, Clauses),
	prolog_load_context(term_position, Pos),
	current_source_module(Module,Module),
	'$assert_list'(Clauses, Module, [], Pos).

'$assert_list'([], _Module, _VL, _Pos).
'$assert_list'([Clause|Clauses], Module, VL, Pos) :-
	'$command'(Clause, VL, Pos, top),
	'$assert_list'(Clauses, Module, VL, Pos).


%% @pred user_defined_directive(Dir,Action)
%
% allow users to define their own directives.
%
user_defined_directive(Dir,_) :-
        '$directive'(Dir), !.
user_defined_directive(Dir,Action) :-
        functor(Dir,Na,Ar),
        functor(NDir,Na,Ar),
        current_source_module(M, prolog),
	assert_static(prolog:'$directive'(NDir)),
	assert_static(prolog:('$exec_directive'(Dir, _, _, _, _) :- Action)),
        current_source_module(_, M).

'$thread_initialization'(M:D) :-
	eraseall('$thread_initialization'),
	%writeln(M:D),
	recorda('$thread_initialization',M:D,_),
	fail.
'$thread_initialization'(M:D) :-
	'$initialization'(M:D).

%
 % This command is very different depending on the language mode we are in.
 %
 % ISO only wants directives in files
 % SICStus accepts everything in files
 % YAP accepts everything everywhere
 %
 '$process_directive'(M:G, Where, _M, VL, Pos) :-
    strip_module(M:G,NM,NG),
    !,
    '$process_directive'(NG, Where , NM, VL, Pos).

    '$process_directive'(G, top, M, VL, Pos) :-
	 current_prolog_flag(language_mode, yap), !,      /* strict_iso on */
	 '$process_directive'(G, consult, M, VL, Pos).
 '$process_directive'(G, top, M, _, _) :-
     !,
	 throw_error(context_error((:-M:G),clause),query).
 %
 % ISO does not allow goals (use initialization).
 %
'$process_directive'(D, _, M, _VL, _Pos) :-
    current_prolog_flag(language_mode, iso),
    \+ '$directive'(D),
    !, % ISO Prolog mode, go in and do it,
	throw_error(context_error((:- M:D),query),directive).
 %
 % but YAP and SICStus do.
 %
'$process_directive'(G, Mode, M, VL, Pos) :-
    '$exec_directive'(G, Mode, M, VL, Pos),
    !.
'$process_directive'(G, _Mode, M, _VL, Pos) :-
    format(user_error,'~w warning: ~w failed.~n',[Pos,M:G]).

'$exec_directive'(encoding(Enc), _, _, _, _) :-
    !,
    '$set_encoding'(Enc).
'$exec_directive'(include(F), _Status, _, _, _) :-
    !,
    '$include'(F).
% don't declare modules into Prolog Module
'$exec_directive'(module(N,P), _Status, HostM, _, Pos) :-
    !,
    '$declare_module'(HostM,N,P,Pos).
'$exec_directive'(op(P,OPSEC,OP), _, _, _, _) :-
    !,
    current_source_module(M,M),
	op(P,OPSEC,M:OP).
'$exec_directive'(ensure_loaded(Fs), _, M, _, Loc) :-
        load_files(M:Fs, [if(changed),'consulted_at'(Loc)]).
'$exec_directive'(compile(Fs), _, M, _, Loc) :-
    !,
    load_files(M:Fs, ['consulted_at'(Loc)]).
'$exec_directive'([Fs], _, M, _, Loc) :-
    is_list([Fs]),
    !,
    load_files(M:Fs, ['consulted_at'(Loc)]).
'$exec_directive'(reconsult(Fs), _, M, _, Loc) :-
    !,
    load_files(M:Fs, ['consulted_at'(Loc)]).
'$exec_directive'(consult(Fs), _, M, _, Loc) :-
    !,
    load_files(M:Fs, [consult(consult),'consulted_at'(Loc)]).
'$exec_directive'(use_system_module(_F), _, _M, _, _Loc) :-
    !.
'$exec_directive'(use_module(F), _, M, _, Loc) :-
    !,
    load_files(M:F,[if(not_loaded),must_be_module(true),'consulted_at'(Loc)]).
'$exec_directive'(reexport(F), _, M, _, Loc) :-
    !,
    load_files(M:F, [if(not_loaded), silent(true), reexport(true),must_be_module(true),'consulted_at'(Loc)]).
'$exec_directive'(reexport(F,Spec), _, M, _, Loc) :-
    !,
    load_files(M:F, [if(not_loaded), silent(true), imports(Spec), reexport(true),must_be_module(true),'consulted_at'(Loc)]).
'$exec_directive'(use_module(F, Is), _, M, _, _Loc) :-
    !,
        use_module(M:F, Is ).
'$exec_directive'(use_module(Mod,F,Is), _,M, _, _Loc) :-
    !,
    use_module(Mod,M:F,Is).
'$exec_directive'(G, _, M, _, _) :-
    call(M:G).


%% @}




