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
* File:		errors.yap						 *
* comments:	error messages for YAP					 *
*									 *
* Last rev:     $Date: 2004-11-19 21:32:53 $,$Author: vsc $						 *
* $Log: not supported by cvs2svn $
* Revision 1.57  2004/10/27 15:56:34  vsc
* bug fixes on memory overflows and on clauses :- fail being ignored by clause.
*
* Revision 1.56  2004/10/04 18:56:20  vsc
* fixes for thread support
* fix indexing bug (serious)
*
* Revision 1.55  2004/09/17 19:34:53  vsc
* simplify frozen/2
*
* Revision 1.54  2004/07/22 21:32:22  vsc
* debugger fixes
* initial support for JPL
* bad calls to garbage collector and gc
* debugger fixes
*
* Revision 1.53  2004/06/23 17:24:20  vsc
* New comment-based message style
* Fix thread support (at least don't deadlock with oneself)
* small fixes for coroutining predicates
* force Yap to recover space in arrays of dbrefs
* use private predicates in debugger.
*
* Revision 1.52  2004/06/18 15:41:19  vsc
* fix extraneous line in yes/no messages
*
* Revision 1.51  2004/06/09 03:32:03  vsc
* fix bugs
*
* Revision 1.50  2004/04/27 16:21:25  vsc
* stupid bug
*									 *
*									 *
*************************************************************************/

'$do_error'(Type,Message) :-
	'$current_stack'(local_sp(_,Envs,CPs)),
	throw(error(Type,[Message|local_sp(Message,Envs,CPs)])).

'$Error'(E) :-
	'$LoopError'(E,top).

'$LoopError'(_, _) :-
	flush_output(user_output),
	flush_output(user_error),
	fail.
'$LoopError'(Error, Level) :- !,
	'$process_error'(Error, Level),
	fail.
'$LoopError'(_, _) :-
	current_stream(_, write, S),
	flush_all_streams,
	fail.

'$process_error'(abort, top) :- !,
	print_message(informational,abort(user)).
'$process_error'(abort, _) :- !,
	throw('$abort').
'$process_error'(error(Msg, Where), _) :- !,
	'$set_fpu_exceptions',
	'$print_message'(error,error(Msg, Where)).
'$process_error'(Throw, _) :-
	print_message(error,Throw).

print_message(Level, Mss) :-
	'$print_message'(Level, Mss).

'$print_message'(force(_Severity), Msg) :- !,
	print(user_error,Msg).
'$print_message'(Severity, Msg) :-
	nonvar(Severity), nonvar(Msg),
	\+ '$undefined'(portray_message(Severity, Msg), user),
	user:portray_message(Severity, Msg), !.
'$print_message'(error,error(Msg,Info)) :-
	( var(Msg) ; var(Info) ), !,
	format(user_error,'% YAP: no handler for error ~w~n', [error(Msg,Info)]).
'$print_message'(error,error(syntax_error(A,B,C,D,E,F),_)) :- !,
	'$output_error_message'(syntax_error(A,B,C,D,E,F), 'SYNTAX ERROR').
'$print_message'(error,error(Msg,[Info|local_sp(Where,Envs,CPs)])) :-
	'$prepare_loc'(Info,Where,Location),
	'$output_error_message'(Msg, Location), !,
	'$do_stack_dump'(Envs, CPs).
% old format: don't want a stack dump.
'$print_message'(error,error(Type,Where)) :-
	'$output_error_message'(Type, Where), !.
'$print_message'(error,Throw) :-
	format(user_error,'% YAP: no handler for error ~w~n', [Throw]).
'$print_message'(informational,M) :-
	( get_value('$verbose',on) ->
	    '$do_informational_message'(M) ;
	    true
	).
'$print_message'(warning,M) :-
	format(user_error, '% ', []),
	'$do_print_message'(M),
	format(user_error, '~n', []).
'$print_message'(help,M) :-
	'$do_print_message'(M),
	format(user_error, '~n', []).


'$do_informational_message'(halt) :- !,
	format(user_error, '% YAP execution halted~n', []).
'$do_informational_message'('$abort') :- !,
	format(user_error, '% YAP execution aborted~n', []).
'$do_informational_message'(loading(_,user)) :- !.
'$do_informational_message'(loading(What,AbsoluteFileName)) :- !,
	'$show_consult_level'(LC),
	format(user_error, '~*|% ~a ~a...~n', [LC, What, AbsoluteFileName]).
'$do_informational_message'(loaded(_,user,_,_,_)) :- !.
'$do_informational_message'(loaded(included,AbsoluteFileName,Mod,Time,Space)) :- !,
	'$show_consult_level'(LC),
	format(user_error, '~*|% ~a included in module ~a, ~d msec ~d bytes~n', [LC, AbsoluteFileName,Mod,Time,Space]).
'$do_informational_message'(loaded(What,AbsoluteFileName,Mod,Time,Space)) :- !,
	'$show_consult_level'(LC0),
	LC is LC0+1,
	format(user_error, '~*|% ~a ~a in module ~a, ~d msec ~d bytes~n', [LC, What, AbsoluteFileName,Mod,Time,Space]).
'$do_informational_message'(M) :-
	format(user_error,'% ', []),
	'$do_print_message'(M),
	format(user_error,'~n', []).

%message(loaded(Past,AbsoluteFileName,user,Msec,Bytes), Prefix, Suffix) :- !,
'$do_print_message'(format(Msg, Args)) :- !,
	format(user_error,Msg,Args).
'$do_print_message'(breakp(bp(debugger,_,_,M:F/N,_),add,already)) :- !,
	format(user_error,'There is already a spy point on ~w:~w/~w.',
	[M,F,N]).	
'$do_print_message'(breakp(bp(debugger,_,_,M:F/N,_),add,ok)) :- !,
	format(user_error,'Spy point set on ~w:~w/~w.',
	[M,F,N]).	
'$do_print_message'(breakp(bp(debugger,_,_,M:F/N,_),remove,last)) :- !,
	format(user_error,'Spy point on ~w:~w/~w removed.',
	[M,F,N]).
'$do_print_message'(breakp(no,breakpoint_for,M:F/N)) :- !,
	format(user_error,'There is no spy point on ~w:~w/~w.',
	[M,F,N]).
'$do_print_message'(breakpoints([])) :- !,
	format(user_error,'There are no spy-points set.',
	[M,F,N]).
'$do_print_message'(breakpoints(L)) :- !,
	format(user_error,'Spy-points set on:', []),
	'$print_list_of_preds'(L).
'$do_print_message'(debug(debug)) :- !,
	format(user_error,'Debug mode on.',[]).
'$do_print_message'(debug(off)) :- !,
	format(user_error,'Debug mode off.',[]).
'$do_print_message'(debug(trace)) :- !,
	format(user_error,'Trace mode on.',[]).
'$do_print_message'(import(Pred,To,From,private)) :- !,
	format(user_error,'Importing private predicate ~w:~w to ~w.',
	[From,Pred,To]).
'$do_print_message'(leash([])) :- !,
	format(user_error,'No leashing.',
	[M,F,N]).
'$do_print_message'(leash([A|B])) :- !,
	format(user_error,'Leashing set to ~w.',
	[[A|B]]).
'$do_print_message'(no) :- !,
	format(user_error, 'no', []).
'$do_print_message'(no_match(P)) :- !,
	format(user_error,'No matching predicate for ~w.',
	[P]).
'$do_print_message'(trace_command(C)) :- !,
	format(user_error,'Invalid trace command: ~c', [C]).
'$do_print_message'(trace_help) :- !,
	format(user_error,'  Please enter a valid debugger command (h for help).', []).
'$do_print_message'(version(Version)) :- !,
	format(user_error,'YAP version ~a', [Version]).
'$do_print_message'(yes) :- !,
	format(user_error, 'yes', []).
'$do_print_message'(Messg) :-
	format(user_error,'~q',Messg).

'$print_list_of_preds'([]).
'$print_list_of_preds'([P|L]) :-
	format(user_error,'~n      ~w',[P]),
	'$print_list_of_preds'(L).

'$do_stack_dump'(Envs, CPs) :-
	'$preprocess_stack'(CPs,0, PCPs),
	'$preprocess_stack'(Envs,0, PEnvs),
	'$say_stack_dump'(PEnvs, PCPs),
	'$show_cps'(PCPs),
	'$show_envs'(PEnvs),
	'$close_stack_dump'(PEnvs, PCPs).

'$preprocess_stack'([], _, []).
'$preprocess_stack'([G|Gs],40, [overflow]) :- !.
'$preprocess_stack'([G|Gs],I, NGs) :-
	'$pred_for_code'(G,Name,Arity,Mod,Clause),
	I1 is I+1,
	'$beautify_stack_goal'(Name,Arity,Mod,Clause,Gs,I1,NGs).
	
'$beautify_stack_goal'(Name,Arity,Module,0,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs,I,NGs).
'$beautify_stack_goal'(Name,Arity,Module,Clause,Gs,I,NGs) :-
	functor(G,Name,Arity),
	'$hidden_predicate'(G,Module), !,
	'$beautify_hidden_goal'(Name,Arity,Module,Clause,Gs,I,NGs).
'$beautify_stack_goal'(Name,Arity,Module,Clause,Gs,I,[cl(Name,Arity,Module,Clause)|NGs]) :-
	'$preprocess_stack'(Gs,I,NGs).


'$beautify_hidden_goal'('$yes_no',_,_,_,_,_,[]) :- !.
'$beautify_hidden_goal'('$do_yes_no',_,_,_,_,_,[]) :- !.
'$beautify_hidden_goal'('$query',_,_,_,_,_,[]) :- !.
'$beautify_hidden_goal'('$enter_top_level',_,_,_,_,_,[]) :- !.
% The user should never know these exist.
'$beautify_hidden_goal'('$csult',_,prolog,ClNo,Gs,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$use_module',2,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$ensure_loaded',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$continue_with_command',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$spycall_stdpred',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$spycalls',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$spycall',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$do_spy',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$spy',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$do_creep_execute',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$creep_execute',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$direct_spy',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$system_catch',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$execute_command',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$process_directive',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$catch',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$loop',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$consult',3,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$reconsult',_,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$undefp',1,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$use_module',2,prolog,ClNo,Gs,I,NGs) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$repeat',0,prolog,ClNo,Gs,I,[cl(repeat,0,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$recorded_with_key',3,prolog,ClNo,Gs,I,[cl(recorded,3,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$consult',3,prolog,ClNo,Gs,I,[cl(consult,1,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$findall_with_common_vars',_,prolog,ClNo,Gs,I,[cl(findall,4,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$findall',_,prolog,ClNo,Gs,I,[cl(findall,4,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$bagof',_,prolog,ClNo,Gs,I,[cl(bagof,3,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$listing',_,prolog,ClNo,Gs,I,[cl(listing,1,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$call',Args,prolog,ClNo,Gs,I,[cl(call,Args,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$current_predicate',Args,prolog,ClNo,Gs,I,[cl(current_predicate,Args,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$list_clauses',_,prolog,ClNo,Gs,I,[cl(listing,1,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'('$use_module',1,prolog,ClNo,Gs,I,[cl(use_module,1,prolog,ClNo)|NGs]) :- !,
	'$preprocess_stack'(Gs, I, NGs).
'$beautify_hidden_goal'(Name,Args,Mod,ClNo,Gs,I,[cl(Name,Args,Mod,ClNo)|NGs]) :-
	'$preprocess_stack'(Gs, I, NGs).


'$say_stack_dump'([], []) :- !.
'$say_stack_dump'(_, _) :-
	format(user_error,'% Stack dump for error:', []).
	
'$close_stack_dump'([], []) :- !.
'$close_stack_dump'(_, _) :-
	format(user_error,'~n', []).
	
'$show_cps'([]) :- !.
'$show_cps'(List) :-
	format(user_error,'%  ~n   choice-points (goals with alternatives left):',[]),
        '$print_stack'(List).

'$show_envs'([]) :- !.
'$show_envs'(List) :-
	format(user_error,'%  ~n   environments (partially executed clauses):',[]),
        '$print_stack'(List).

'$prepare_loc'(Info,Where,Location) :- integer(Where), !,
	'$pred_for_code'(Where,Name,Arity,Mod,Clause),
	'$construct_code'(Clause,Name,Arity,Mod,Info,Location).
'$prepare_loc'(Info,Where,Info).

'$print_stack'([]).
'$print_stack'([overflow]) :- !,
	format(user_error,'~n%	...',[]).
'$print_stack'([cl(Name,Arity,Mod,Clause)|List]) :-
	'$show_goal'(Clause,Name,Arity,Mod),
	'$print_stack'(List).

'$show_goal'(-1,Name,Arity,Mod) :- !,
	format('~n%      ~a:~a/~d at indexing code',[Mod,Name,Arity]).
'$show_goal'(0,Name,Arity,Mod) :- !.
'$show_goal'(I,Name,Arity,Mod) :-
	format(user_error,'~n%      ~a:~a/~d at clause ~d',[Mod,Name,Arity,I]).

'$construct_code'(-1,Name,Arity,Mod,Where,Location) :- !,
	number_codes(Arity,ArityCode),
	atom_codes(ArityAtom,ArityCode),
	atom_concat([Where,' at ',Mod,':',Name,'/',ArityAtom,' at indexing code'],Location).
'$construct_code'(0,_,_,_,Location,Location) :- !.
'$construct_code'(Cl,Name,Arity,Mod,Where,Location) :-
	number_codes(Arity,ArityCode),
	atom_codes(ArityAtom,ArityCode),
	number_codes(Cl,ClCode),
	atom_codes(ClAtom,ClCode),
	atom_concat([Where,' at ',Mod,':',Name,'/',ArityAtom,' (clause ',ClAtom,')'],Location).

'$output_error_message'(consistency_error(Who),Where) :-
	format(user_error,'% CONSISTENCY ERROR- ~w ~w~n',
	[Who,Where]).
'$output_error_message'(context_error(Goal,Who),Where) :-
	format(user_error,'% CONTEXT ERROR- ~w: ~w appeared in ~w~n',
	[Goal,Who,Where]).
'$output_error_message'(domain_error(array_overflow,Opt), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid index ~w for array~n',
	[Where,Opt]).
'$output_error_message'(domain_error(array_type,Opt), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid static array type ~w~n',
	[Where,Opt]).
'$output_error_message'(domain_error(builtin_procedure,P), P) :-
	format(user_error,'% DOMAIN ERROR- non-iso built-in procedure  ~w~n',
	[P]).
'$output_error_message'(domain_error(character_code_list,Opt), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid list of codes ~w~n',
	[Where,Opt]).
'$output_error_message'(domain_error(delete_file_option,Opt), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid list of options ~w~n',
	[Where,Opt]).
'$output_error_message'(domain_error(operator_specifier,Op), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid operator specifier ~w~n',
	[Where,Op]).
'$output_error_message'(domain_error(out_of_range,Value), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: expression ~w is out of range~n',
	[Where,Value]).
'$output_error_message'(domain_error(close_option,Opt), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid close option ~w~n',
	[Where,Opt]).
'$output_error_message'(domain_error(radix,Opt), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid radix ~w~n',
	[Where,Opt]).
'$output_error_message'(domain_error(shift_count_overflow,Opt), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: shift count overflow in ~w~n',
	[Where,Opt]).
'$output_error_message'(domain_error(flag_value,F+V), W) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid value ~w for flag ~w~n',
	[W,V,F]).
'$output_error_message'(domain_error(io_mode,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid io mode ~w~n',
	[Where,N]).
'$output_error_message'(domain_error(mutable,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: invalid mutable ~w~n',
	[Where,N]).
'$output_error_message'(domain_error(module_decl_options,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: expect module declaration options, found ~w~n',
	[Where,N]).
'$output_error_message'(domain_error(not_empty_list,_), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: found empty list~n',
	[Where]).
'$output_error_message'(domain_error(not_less_than_zero,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: number ~w less than zero~n',
	[Where,N]).
'$output_error_message'(domain_error(not_newline,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: number ~w not newline~n',
	[Where,N]).
'$output_error_message'(domain_error(not_zero,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w is not allowed in the domain ~n',
	[Where,N]).
'$output_error_message'(domain_error(operator_priority,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w invalid operator priority~n',
	[Where,N]).
'$output_error_message'(domain_error(operator_specifier,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w invalid operator specifier~n',
	[Where,N]).
'$output_error_message'(domain_error(predicate_spec,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w invalid predicate specifier~n',
	[Where,N]).
'$output_error_message'(domain_error(read_option,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w invalid option to read~n',
	[Where,N]).
'$output_error_message'(domain_error(semantics_indicator,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected predicate indicator, got ~w~n',
	[Where,W]).
'$output_error_message'(domain_error(source_sink,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w is not a source sink term~n',
	[Where,N]).
'$output_error_message'(domain_error(stream,What), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w not a stream~n',
	[Where,What]).
'$output_error_message'(domain_error(stream_or_alias,What), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w not a stream~n',
	[Where,What]).
'$output_error_message'(domain_error(stream_option,What), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w not a stream option~n',
	[Where,What]).
'$output_error_message'(domain_error(stream_position,What), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w not a stream position~n',
	[Where,What]).
'$output_error_message'(domain_error(stream_property,What), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w not a stream property~n',
	[Where,What]).
'$output_error_message'(domain_error(syntax_error_handler,What), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w not a syntax error handler~n',
	[Where,What]).
'$output_error_message'(domain_error(thread_create_option,Option+Opts), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w not in ~w~n',
	[Where,Option, Opts]).
'$output_error_message'(domain_error(time_out_spec,What), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w not a valid specification for a time out~n',
	[Where,What]).
'$output_error_message'(domain_error(write_option,N), Where) :-
	format(user_error,'% DOMAIN ERROR- ~w: ~w invalid option to write~n',
	[Where,N]).
'$output_error_message'(existence_error(array,F), W) :-
	format(user_error,'% EXISTENCE ERROR- ~w could not open array ~w~n',
	[W,F]).
'$output_error_message'(existence_error(mutex,F), W) :-
	format(user_error,'% EXISTENCE ERROR- ~w could not open mutex ~w~n',
	[W,F]).
'$output_error_message'(existence_error(queue,F), W) :-
	format(user_error,'% EXISTENCE ERROR- ~w could not open message queue ~w~n',
	[W,F]).
'$output_error_message'(existence_error(procedure,P), _) :-
	format(user_error,'% EXISTENCE ERROR- procedure ~w undefined~n',
	[P]).
'$output_error_message'(existence_error(source_sink,F), W) :-
	format(user_error,'% EXISTENCE ERROR- ~w could not find file ~w~n',
	[W,F]).
'$output_error_message'(existence_error(stream,Stream), Where) :-
	format(user_error,'% EXISTENCE ERROR- ~w: ~w not an open stream~n',
	[Where,Stream]).
'$output_error_message'(evaluation_error(int_overflow), Where) :-
	format(user_error,'% INTEGER OVERFLOW ERROR- ~w~n',
	[Where]).
'$output_error_message'(evaluation_error(float_overflow), Where) :-
	format(user_error,'% FLOATING POINT OVERFLOW ERROR- ~w~n',
	[Where]).
'$output_error_message'(evaluation_error(undefined), Where) :-
	format(user_error,'% UNDEFINED ARITHMETIC RESULT ERROR- ~w~n',
	[Where]).
'$output_error_message'(evaluation_error(underflow), Where) :-
	format(user_error,'% UNDERFLOW ERROR- ~w~n',
	[Where]).
'$output_error_message'(evaluation_error(float_underflow), Where) :-
	format(user_error,'% FLOATING POINT UNDERFLOW ERROR- ~w~n',
	[Where]).
'$output_error_message'(evaluation_error(zero_divisor), Where) :-
	format(user_error,'% ZERO DIVISOR ERROR- ~w~n',
	[Where]).
'$output_error_message'(instantiation_error, Where) :-
	format(user_error,'% INSTANTIATION ERROR- ~w: expected bound value~n',
	[Where]).
'$output_error_message'(out_of_heap_error, Where) :-
	format(user_error,'% OUT OF DATABASE SPACE ERROR- ~w~n',
	[Where]).
'$output_error_message'(out_of_stack_error, Where) :-
	format(user_error,'% OUT OF STACK SPACE ERROR- ~w~n',
	[Where]).
'$output_error_message'(out_of_trail_error, Where) :-
	format(user_error,'% OUT OF TRAIL SPACE ERROR- ~w~n',
	[Where]).
'$output_error_message'(out_of_attvars_error, Where) :-
	format(user_error,'% OUT OF STACK SPACE ERROR- ~w~n',
	[Where]).
'$output_error_message'(out_of_auxspace_error, Where) :-
	format(user_error,'% OUT OF AUXILIARY STACK SPACE ERROR- ~w~n',
	[Where]).
'$output_error_message'(permission_error(access,private_procedure,P), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot see clauses for ~w~n',
	[Where,P]).
'$output_error_message'(permission_error(access,static_procedure,P), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot access static procedure ~w~n',
	[Where,P]).
'$output_error_message'(permission_error(alias,new,P), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot create alias ~w~n',
	[Where,P]).
'$output_error_message'(permission_error(create,array,P), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot create array ~w~n',
	[Where,P]).
'$output_error_message'(permission_error(create,mutex,P), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot create mutex ~a~n',
	[Where,P]).
'$output_error_message'(permission_error(create,queue,P), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot create queue ~a~n',
	[Where,P]).
'$output_error_message'(permission_error(create,operator,P), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot create operator ~w~n',
	[Where,P]).
'$output_error_message'(permission_error(input,binary_stream,Stream), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot read from binary stream ~w~n',
	[Where,Stream]).
'$output_error_message'(permission_error(input,closed_stream,Stream), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: trying to read from closed stream ~w~n',
	[Where,Stream]).
'$output_error_message'(permission_error(input,past_end_of_stream,Stream), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: past end of stream ~w~n',
	[Where,Stream]).
'$output_error_message'(permission_error(input,stream,Stream), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot read from ~w~n',
	[Where,Stream]).
'$output_error_message'(permission_error(input,text_stream,Stream), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot read from text stream ~w~n',
	[Where,Stream]).
'$output_error_message'(permission_error(modify,dynamic_procedure,_), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: modifying a dynamic procedure~n',
	[Where]).
'$output_error_message'(permission_error(modify,flag,W), _) :-
	format(user_error,'% PERMISSION ERROR- cannot modify flag ~w~n',
	[W]).
'$output_error_message'(permission_error(modify,operator,W), _) :-
	format(user_error,'% PERMISSION ERROR- T cannot declare ~w an operator~n',
	[W]).
'$output_error_message'(permission_error(modify,dynamic_procedure,_), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: modifying a dynamic procedure~n',
	[Where]).
'$output_error_message'(permission_error(modify,static_procedure,_), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: modifying a static procedure~n',
	[Where]).
'$output_error_message'(permission_error(modify,static_procedure_in_use,_), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: modifying a static procedure in use~n',
	[Where]).
'$output_error_message'(permission_error(module,redefined,Mod), Who) :-
	format(user_error,'% PERMISSION ERROR ~w- redefining module ~a in a different file~n',
	[Who,Mod]).
'$output_error_message'(permission_error(open,source_sink,Stream), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot open file ~w~n',
	[Where,Stream]).
'$output_error_message'(permission_error(output,binary_stream,Stream), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot write to binary stream ~w~n',
	[Where,Stream]).
'$output_error_message'(permission_error(output,stream,Stream), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot write to ~w~n',
	[Where,Stream]).
'$output_error_message'(permission_error(output,text_stream,Stream), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot write to text stream ~w~n',
	[Where,Stream]).
'$output_error_message'(permission_error(resize,array,P), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot resize array ~w~n',
	[Where,P]).
'$output_error_message'(permission_error(unlock,mutex,P), Where) :-
	format(user_error,'% PERMISSION ERROR- ~w: cannot unlock mutex ~w~n',
	[Where,P]).
'$output_error_message'(representation_error(character), Where) :-
	format(user_error,'% REPRESENTATION ERROR- ~w: expected character~n',
	[Where]).
'$output_error_message'(representation_error(character_code), Where) :-
	format(user_error,'% REPRESENTATION ERROR- ~w: expected character code~n',
	[Where]).
'$output_error_message'(representation_error(max_arity), Where) :-
	format(user_error,'% REPRESENTATION ERROR- ~w: number too big~n',
	[Where]).
'$output_error_message'(syntax_error(G,0,Msg,[],0,0), Where) :- !,
	format(user_error,'% SYNTAX ERROR in ~w: ~a~n',[G,Msg]).
'$output_error_message'(syntax_error(_,Position,_,Term,Pos,Start), Where) :-
	format(user_error,'% ~w ',[Where]),
	'$dump_syntax_error_line'(Start,Position),
	'$dump_syntax_error_term'(10,Pos, Term),
	format(user_error,'.~n]~n',[]).
'$output_error_message'(system_error, Where) :-
	format(user_error,'% SYSTEM ERROR- ~w~n',
	[Where]).
'$output_error_message'(system_error(Message), Where) :-
	format(user_error,'% SYSTEM ERROR- ~w at ~w]~n',
	[Message,Where]).
'$output_error_message'(type_error(T,_,Err,M), _Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected ~w, got ~w~n',
	[T,Err,M]).
'$output_error_message'(type_error(array,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected array, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(atom,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected atom, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(atomic,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected atomic, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(byte,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected byte, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(callable,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected callable goal, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(char,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected char, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(character,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected character, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(character_code,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected character code, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(compound,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected compound, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(db_reference,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected data base reference, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(db_term,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected data base term, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(evaluable,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected evaluable term, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(float,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected float, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(in_byte,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected byte, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(in_character,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected atom character, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(in_character_code,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected character code, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(integer,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected integer, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(key,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected database key, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(leash_mode,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected modes for leash, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(list,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected list, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(number,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected number, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(pointer,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected pointer, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(predicate_indicator,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected predicate indicator, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(unsigned_byte,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected unsigned byte, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(unsigned_char,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected unsigned char, got ~w~n',
	[Where,W]).
'$output_error_message'(type_error(variable,W), Where) :-
	format(user_error,'% TYPE ERROR- ~w: expected unbound variable, got ~w~n',
	[Where,W]).
'$output_error_message'(unknown, Where) :-
	format(user_error,'% EXISTENCE ERROR- procedure ~w undefined~n',
	[Where]).


'$dump_syntax_error_line'(Pos,_) :-
	format(user_error,'at line ~d:~n',
	[Pos]).

'$dump_syntax_error_term'(0,J,L) :- !,
	format(user_error,'~n', []),
	'$dump_syntax_error_term'(10,J,L).
'$dump_syntax_error_term'(_,0,L) :- !,
	format(user_error,'~n<==== HERE ====>~n', []),
	'$dump_syntax_error_term'(10,-1,L).
'$dump_syntax_error_term'(_,_,[]) :- !.
'$dump_syntax_error_term'(I,J,[T-P|R]) :-
	'$dump_error_token'(T),
	I1 is I-1,
	J1 is J-1,
	'$dump_syntax_error_term'(I1,J1,R).

'$dump_error_token'(atom(A)) :- !,
	format(user_error,' ~a', [A]).
'$dump_error_token'(number(N)) :- !,
	format(user_error,' ~w', [N]).
'$dump_error_token'(var(_,S,_)) :- !,
	format(user_error,' ~s ', [S]).
'$dump_error_token'(string(S)) :- !,
	format(user_error,' ""~s""', [S]).
'$dump_error_token'('(') :- !,
	format(user_error,"(", []).
'$dump_error_token'(')') :- !,
	format(user_error," )", []).
'$dump_error_token'(',') :- !,
	format(user_error," ,", []).
'$dump_error_token'(A) :-
	format(user_error," ~a", [A]).

