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
* File:		utilities for displaying messages in YAP.		 *
* comments:	error messages for YAP					 *
*									 *
* Last rev:     $Date: 2008-07-16 10:58:59 $,$Author: vsc $						 *
*									 *
*									 *
*************************************************************************/

/** @defgroup Messages Message Handling
@ingroup YAPControl
@{

The interaction between YAP and the user relies on YAP's ability to
portray messages. These messages range from prompts to error
information. All message processing is performed through the builtin
print_message/2, in two steps:

+ The message is processed into a list of commands 
+ The commands in the list are sent to the `format/3` builtin
in sequence.


The first argument to print_message/2 specifies the importance of
the message. The options are:

+ `error`
error handling
+ `warning`
compilation and run-time warnings,
+ `informational`
generic informational messages
+ `help`
help messages (not currently implemented in YAP)
+ `query`
query 	used in query processing (not currently implemented in YAP)
+ `silent`
messages that do not produce output but that can be intercepted by hooks.


The next table shows the main predicates and hooks associated to message
handling in YAP:


*/

:- module('$messages',
	  [system_message/4,
	   prefix/6,
	   prefix/5,
	   file_location/3]).


:- use_system_module( user, [generate_message_hook/3]).

:- multifile prolog:message/3.

:- multifile user:generate_message_hook/3.

file_location -->
	{ source_location(FileName, LN) },
	file_position(FileName,LN),
	[ nl ].

file_position(user_input,LN) -->
	[ 'at line ~d in user_input,' - [LN] ].
file_position(FileName,LN) -->
	[ 'at line ~d in ~a,' - [LN,FileName] ].

translate_message(Term) -->
	generate_message(Term), !.
translate_message(Term) -->
	{ Term = error(_, _) },
	[ 'Unknown exception: ~p'-[Term] ].
translate_message(Term) -->
	[ 'Unknown message: ~p'-[Term] ].

generate_message(Term, Lines, []) :-
	user:generate_message_hook(Term, [], Lines), !.
generate_message(Term) -->
	prolog:message(Term), !.
generate_message(halt) --> !,
	['YAP execution halted'].
generate_message(false) --> !,
	['false.'].
generate_message('$abort') --> !,
	['YAP execution aborted'].
generate_message(abort(user)) --> !,
	['YAP execution aborted'].
generate_message(loading(_,F)) --> F == user, !.
generate_message(loading(What,FileName)) --> !,
	[ '~a ~w...' - [What, FileName] ].
generate_message(loaded(_,user,_,_,_)) --> !.
generate_message(loaded(included,AbsoluteFileName,Mod,Time,Space)) --> !,
	[ '~a included in module ~a, ~d msec ~d bytes' - [AbsoluteFileName,Mod,Time,Space] ].
generate_message(loaded(What,AbsoluteFileName,Mod,Time,Space)) --> !,
	[ '~a ~a in module ~a, ~d msec ~d bytes' - [What, AbsoluteFileName,Mod,Time,Space] ].
generate_message(prompt(BreakLevel,TraceDebug)) --> !,
	( { BreakLevel =:= 0 } ->
	    (
	     { var(TraceDebug) } ->
	     []
	    ;
	     [ '~a' - [TraceDebug] ]
	    )
	;
	    (
	     { var(TraceDebug) } ->
	     [ '~d' - [BreakLevel] ]
	    ;
	     [ '~d ~a' - [BreakLevel, TraceDebug] ]
	    )
	).
generate_message(debug) --> !,
	[ debug ].
generate_message(trace) --> !,
	[ trace ].
generate_message(error(Error,Context)) -->
	{ Error = existence_error(procedure,_) }, !,	
	system_message(error(Error,Context)),
	stack_dump(error(Error,Context)).
generate_message(error(Error,context(Cause,Extra))) -->
	system_message(error(Error,Cause)),
	stack_dump(error(Error,context(Cause,Extra))).
generate_message(M) -->
	system_message(M),
	stack_dump(M).

stack_dump(error(_,_)) -->
	{ fail }, 
	{ recorded(sp_info,local_sp(_P,CP,Envs,CPs),_) },
	{ Envs = [_|_] ; CPs = [_|_] }, !,
	[nl],
	'$hacks':display_stack_info(CPs, Envs, 20, CP).
stack_dump(_) --> [].

prolog_message(X,Y,Z) :-
	system_message(X,Y,Z).	      

%message(loaded(Past,AbsoluteFileName,user,Msec,Bytes), Prefix, Suffix) :- !,
system_message(query(_QueryResult,_)) --> [].
system_message(format(Msg, Args)) -->
	[Msg - Args].
system_message(ancestors([])) -->
	[ 'There are no ancestors.' ].
system_message(breakp(bp(debugger,_,_,M:F/N,_),add,already)) -->
	[ 'There is already a spy point on ~w:~w/~w.' - [M,F,N]].
system_message(breakp(bp(debugger,_,_,M:F/N,_),add,ok)) -->
	[ 'Spy point set on ~w:~w/~w.' - [M,F,N] ].
system_message(breakp(bp(debugger,_,_,M:F/N,_),remove,last)) -->
	[ 'Spy point on ~w:~w/~w removed.' - [M,F,N] ].
system_message(breakp(no,breakpoint_for,M:F/N)) -->
	[ 'There is no spy point on ~w:~w/~w.' - [M,F,N]].
system_message(breakpoints([])) -->
	[ 'There are no spy-points set.' ].
system_message(breakpoints(L)) -->
	[ 'Spy-points set on:' ],
	list_of_preds(L).
system_message(clauses_not_together(P)) -->
	[ 'Discontiguous definition of ~q.' - [P] ].
system_message(debug(debug)) -->
	[ 'Debug mode on.' ].
system_message(debug(off)) -->
	[ 'Debug mode off.' ].
system_message(debug(trace)) -->
	[ 'Trace mode on.' ].
system_message(declaration(Args,Action)) -->
	[ 'declaration ~w ~w.' - [Args,Action] ].
system_message(defined_elsewhere(P,F)) -->
	[  'predicate ~q previously defined in file ~w' - [P,F] ].
system_message(functionality(Library)) -->
	[  '~q not available' - [Library] ].
system_message(import(Pred,To,From,private)) -->
	[ 'Importing private predicate ~w:~w to ~w.' - [From,Pred,To] ].
system_message(redefine_imported(M,M0,PI)) -->
	[ 'Module ~w redefines imported predicate ~w:~w.' - [M,M0,PI] ].
system_message(leash([])) -->
	[ 'No leashing.' ].
system_message(leash([A|B])) -->
	[ 'Leashing set to ~w.' - [[A|B]] ].
system_message(no) -->
	[ 'no'  ].
system_message(no_match(P)) -->
	[ 'No matching predicate for ~w.' - [P] ].
system_message(leash([A|B])) -->
	[ 'Leashing set to ~w.' - [[A|B]] ].
system_message(singletons(SVs,P,W)) -->
	[  'Singleton variable~*c ~s in ~q, starting at line ~d' - [NVs, 0's, SVsL, I, L] ], % '
	{ svs(SVs,SVsL,[]),
	  ( SVs = [_] -> NVs = 0 ; NVs = 1 ),
	  clause_to_indicator(P, I),
	  stream_position_data( line_count, W, L)
	}.
system_message(multiple(P,W,F)) -->
	[  'Redefinition: clause at line ~d redefines ~w from file ~a' - [L, I, F] ], % '
	{ clause_to_indicator(P, I),
	  stream_position_data( line_count, W, L)
	}.
system_message(discontiguous(P,W)) -->
	[  'Discontiguous clause for ~w at line ~d' - [I, L] ], % '
	{ clause_to_indicator(P, I),
	  stream_position_data( line_count, W, L)
	}.
system_message(trace_command(-1)) -->
	[ 'EOF is not a valid debugger command.'  ].
system_message(trace_command(C)) -->
	[ '~c is not a valid debugger command.' - [C] ].
system_message(trace_help) -->
	[ '   Please enter a valid debugger command (h for help).'  ].
system_message(version(Version)) -->
	[ '~a' - [Version] ].
system_message(myddas_version(Version)) -->
	[ 'MYDDAS version ~a' - [Version] ].
system_message(yes) -->
	[  'yes'  ].
system_message(error(Msg,Info)) -->
	( { var(Msg) } ; { var(Info)} ), !,
	['bad error ~w' - [error(Msg,Info)]].
system_message(error(consistency_error(Who),Where)) -->
	[ 'CONSISTENCY ERROR (arguments not compatible with format)- ~w ~w' - [Who,Where] ].
system_message(error(context_error(Goal,Who),Where)) -->
	[ 'CONTEXT ERROR- ~w: ~w appeared in ~w' - [Goal,Who,Where] ].
system_message(error(domain_error(DomainType,Opt), Where)) -->
	[ 'DOMAIN ERROR- ~w: ' - Where],
	domain_error(DomainType, Opt).
system_message(error(format_argument_type(Type,Arg), Where)) -->
	[ 'FORMAT ARGUMENT ERROR- ~~~a called with ~w in ~w: ' - [Type,Arg,Where]].
system_message(error(existence_error(directory,Key), Where)) -->
	[ 'EXISTENCE ERROR- ~w: ~w not an existing directory' - [Where,Key] ].
system_message(error(existence_error(key,Key), Where)) -->
	[ 'EXISTENCE ERROR- ~w: ~w not an existing key' - [Where,Key] ].
system_message(existence_error(prolog_flag,F)) -->
	[ 'Prolog Flag ~w: new Prolog flags must be created using create_prolog_flag/3.' - [F] ].
system_message(error(existence_error(prolog_flag,P), Where)) --> !,
	[ 'EXISTENCE ERROR- ~w: prolog flag ~w is undefined' - [Where,P] ].
system_message(error(existence_error(procedure,P), context(Call,Parent))) --> !,
	[ 'EXISTENCE ERROR- procedure ~w is undefined, called from context  ~w~n                 Goal was ~w' - [P,Parent,Call] ].
system_message(error(existence_error(stream,Stream), Where)) -->
	[ 'EXISTENCE ERROR- ~w: ~w not an open stream' - [Where,Stream] ].
system_message(error(existence_error(thread,Thread), Where)) -->
	[ 'EXISTENCE ERROR- ~w: ~w not a running thread' - [Where,Thread] ].
system_message(error(existence_error(variable,Var), Where)) -->
	[ 'EXISTENCE ERROR- ~w: variable ~w does not exist' - [Where,Var] ].
system_message(error(existence_error(Name,F), W)) -->
	{ object_name(Name, ObjName) },
	[ 'EXISTENCE ERROR- ~w could not open ~a ~w' - [W,ObjName,F] ].
system_message(error(evaluation_error(int_overflow), Where)) -->
	[ 'INTEGER OVERFLOW ERROR- ~w' - [Where] ].
system_message(error(evaluation_error(float_overflow), Where)) -->
	[ 'FLOATING POINT OVERFLOW ERROR- ~w' - [Where] ].
system_message(error(evaluation_error(undefined), Where)) -->
	[ 'UNDEFINED ARITHMETIC RESULT ERROR- ~w' - [Where] ].
system_message(error(evaluation_error(underflow), Where)) -->
	[ 'UNDERFLOW ERROR- ~w' - [Where] ].
system_message(error(evaluation_error(float_underflow), Where)) -->
	[ 'FLOATING POINT UNDERFLOW ERROR- ~w' - [Where] ].
system_message(error(evaluation_error(zero_divisor), Where)) -->
	[ 'ZERO DIVISOR ERROR- ~w' - [Where] ].
system_message(error(instantiation_error, Where)) -->
	[ 'INSTANTIATION ERROR- ~w: expected bound value' - [Where] ].
system_message(error(not_implemented(Type, What), Where)) -->
	[ '~w: ~w not implemented- ~w' - [Where, Type, What] ].
system_message(error(operating_system_error, Where)) -->
	[ 'OPERATING SYSTEM ERROR- ~w' - [Where] ].
system_message(error(out_of_heap_error, Where)) -->
	[ 'OUT OF DATABASE SPACE ERROR- ~w' - [Where] ].
system_message(error(out_of_stack_error, Where)) -->
	[ 'OUT OF STACK SPACE ERROR- ~w' - [Where] ].
system_message(error(out_of_trail_error, Where)) -->
	[ 'OUT OF TRAIL SPACE ERROR- ~w' - [Where] ].
system_message(error(out_of_attvars_error, Where)) -->
	[ 'OUT OF STACK SPACE ERROR- ~w' - [Where] ].
system_message(error(out_of_auxspace_error, Where)) -->
	[ 'OUT OF AUXILIARY STACK SPACE ERROR- ~w' - [Where] ].
system_message(error(permission_error(access,private_procedure,P), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot see clauses for ~w' - [Where,P] ].
system_message(error(permission_error(access,static_procedure,P), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot access static procedure ~w' - [Where,P] ].
system_message(error(permission_error(alias,new,P), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot create alias ~w' - [Where,P] ].
system_message(error(permission_error(create,Name,P), Where)) -->
	{ object_name(Name, ObjName) },
	[ 'PERMISSION ERROR- ~w: cannot create ~a ~w' - [Where,ObjName,P] ].
system_message(error(permission_error(import,M1:I,redefined,SecondMod), Where)) -->
	[ 'PERMISSION ERROR- loading ~w: modules ~w and ~w both define ~w' - [Where,M1,SecondMod,I] ].
system_message(error(permission_error(input,binary_stream,Stream), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot read from binary stream ~w' - [Where,Stream] ].
system_message(error(permission_error(input,closed_stream,Stream), Where)) -->
	[ 'PERMISSION ERROR- ~w: trying to read from closed stream ~w' - [Where,Stream] ].
system_message(error(permission_error(input,past_end_of_stream,Stream), Where)) -->
	[ 'PERMISSION ERROR- ~w: past end of stream ~w' - [Where,Stream] ].
system_message(error(permission_error(input,stream,Stream), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot read from ~w' - [Where,Stream] ].
system_message(error(permission_error(input,text_stream,Stream), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot read from text stream ~w' - [Where,Stream] ].
system_message(error(permission_error(modify,dynamic_procedure,_), Where)) -->
	[ 'PERMISSION ERROR- ~w: modifying a dynamic procedure' - [Where] ].
system_message(error(permission_error(modify,flag,W), _)) -->
	[ 'PERMISSION ERROR- cannot modify flag ~w' - [W] ].
system_message(error(permission_error(modify,operator,W), Q)) -->
	[ 'PERMISSION ERROR- ~w: cannot modify operator ~q' - [Q,W] ].
system_message(error(permission_error(modify,dynamic_procedure,F), Where)) -->
	[ 'PERMISSION ERROR- ~w: modifying dynamic procedure ~w' - [Where,F] ].
system_message(error(permission_error(modify,static_procedure,F), Where)) -->
	[ 'PERMISSION ERROR- ~w: modifying static procedure ~w' - [Where,F] ].
system_message(error(permission_error(modify,static_procedure_in_use,_), Where)) -->
	[ 'PERMISSION ERROR- ~w: modifying a static procedure in use' - [Where] ].
system_message(error(permission_error(modify,table,P), _)) -->
	[ 'PERMISSION ERROR- cannot table procedure ~w' - [P] ].
system_message(error(permission_error(module,redefined,Mod), Who)) -->
	[ 'PERMISSION ERROR ~w- redefining module ~a in a different file' - [Who,Mod] ].
system_message(error(permission_error(open,source_sink,Stream), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot open file ~w' - [Where,Stream] ].
system_message(error(permission_error(output,binary_stream,Stream), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot write to binary stream ~w' - [Where,Stream] ].
system_message(error(permission_error(output,stream,Stream), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot write to ~w' - [Where,Stream] ].
system_message(error(permission_error(output,text_stream,Stream), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot write to text stream ~w' - [Where,Stream] ].
system_message(error(permission_error(resize,array,P), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot resize array ~w' - [Where,P] ].
system_message(error(permission_error(unlock,mutex,P), Where)) -->
	[ 'PERMISSION ERROR- ~w: cannot unlock mutex ~w' - [Where,P] ].
system_message(error(representation_error(character), Where)) -->
	[ 'REPRESENTATION ERROR- ~w: expected character' - [Where] ].
system_message(error(representation_error(character_code), Where)) -->
	[ 'REPRESENTATION ERROR- ~w: expected character code' - [Where] ].
system_message(error(representation_error(max_arity), Where)) -->
	[ 'REPRESENTATION ERROR- ~w: number too big' - [Where] ].
system_message(error(representation_error(variable), Where)) -->
	[ 'REPRESENTATION ERROR- ~w: should be a variable' - [Where] ].
system_message(error(resource_error(code_space), Where)) -->
	[ 'RESOURCE ERROR- not enough code space' - [Where] ].
system_message(error(resource_error(huge_int), Where)) -->
	[ 'RESOURCE ERROR- too large an integer in absolute value' - [Where] ].
system_message(error(resource_error(memory), Where)) -->
	[ 'RESOURCE ERROR- not enough virtual memory' - [Where] ].
system_message(error(resource_error(stack), Where)) -->
	[ 'RESOURCE ERROR- not enough stack' - [Where] ].
system_message(error(resource_error(streams), Where)) -->
	[ 'RESOURCE ERROR- could not find a free stream' - [Where] ].
system_message(error(resource_error(threads), Where)) -->
	[ 'RESOURCE ERROR- too many open threads' - [Where] ].
system_message(error(resource_error(trail), Where)) -->
	[ 'RESOURCE ERROR- not enough trail space' - [Where] ].
system_message(error(signal(SIG,_), _)) -->
	[ 'UNEXPECTED SIGNAL: ~a' - [SIG] ].
system_message(error(syntax_error(_), [syntax_error(G,_,Msg,[],_,0,File)|_])) -->
	[ 'SYNTAX ERROR at "~a", goal ~q: ~a' - [File,G,Msg] ].
% SWI like I/O error message.
system_message(error(syntax_error(end_of_clause), [stream(Stream, Line, _, _)|_])) -->
	[ 'SYNTAX ERROR ~a, stream ~w, near line ~d.' - ['Unexpected end of clause',Stream,Line] ].
system_message(error(syntax_error(_), [syntax_error(read(_),_,_,Term,Pos,Start,File)|_])) -->
	{ Term = [_|_] },
	['SYNTAX ERROR' - []],
	syntax_error_line(File, Start, Pos),
	syntax_error_term(10, Pos, Term),
	[ '.' ].
system_message(error(system_error, Where)) -->
	[ 'SYSTEM ERROR- ~w' - [Where] ].
system_message(error(internal_compiler_error, Where)) -->
	[ 'INTERNAL COMPILER ERROR- ~w' - [Where] ].
system_message(error(system_error(Message), Where)) -->
	[ 'SYSTEM ERROR- ~w at ~w]' - [Message,Where] ].
system_message(error(timeout_error(T,Obj), _Where)) -->
	[ 'TIMEOUT ERROR- operation ~w on object ~w' - [T,Obj] ].
system_message(error(type_error(T,_,Err,M), _Where)) -->
	[ 'TYPE ERROR- ~w: expected ~w, got ~w' - [T,Err,M] ].
system_message(error(type_error(TE,W), Where)) -->
	{ object_name(TE, M) }, !,
	[ 'TYPE ERROR- ~w: expected ~a, got ~w' - [Where,M,W] ].
system_message(error(type_error(TE,W), Where)) -->
	[ 'TYPE ERROR- ~w: expected ~q, got ~w' - [Where,TE,W] ].
system_message(error(unknown, Where)) -->
	[ 'EXISTENCE ERROR- procedure ~w undefined' - [Where] ].
system_message(error(unhandled_exception,Throw)) -->
	[ 'UNHANDLED EXCEPTION - message ~w unknown' - [Throw] ].
system_message(error(uninstantiation_error(TE), _Where)) -->
	[ 'UNINSTANTIATION ERROR - expected unbound term, got ~q' - [TE] ].
system_message(Messg) -->
	[ '~q' - Messg ].


domain_error(array_overflow, Opt) --> !,
	[ 'invalid static index ~w for array' - Opt ].
domain_error(array_type, Opt) --> !,
	[ 'invalid static array type ~w' - Opt ].
domain_error(builtin_procedure, _) --> !,
	[ 'non-iso built-in procedure'  ].
domain_error(character_code_list, Opt) --> !,
	[ 'invalid list of codes ~w' - [Opt] ].
domain_error(close_option, Opt) --> !,
	[ 'invalid close option ~w' - [Opt] ].
domain_error(delete_file_option, Opt) --> !,
	[ 'invalid list of options ~w' - [Opt] ].
domain_error(encoding, Opt) --> !,
	[ 'invalid encoding ~w' - [Opt] ].
domain_error(flag_value, [Opt,Flag]) --> !,
	[ 'invalid value ~w for flag ~w' - [Opt,Flag] ].
domain_error(flag_value, Opt) --> !,
	[ 'invalid value ~w for flag' - [Opt] ].
domain_error(io_mode, Opt) --> !,
	[ 'invalid io mode ~w' - [Opt] ].
domain_error(mutable, Opt) --> !,
	[ 'invalid id mutable ~w' - [Opt] ].
domain_error(module_decl_options, Opt) --> !,
	[ 'expect module declaration options, found ~w' - [Opt] ].
domain_error(non_empty_list, Opt) --> !,
	[ 'found empty list' - [Opt] ].
domain_error(not_less_than_zero, Opt) --> !,
	[ 'number ~w less than zero' - [Opt] ].
domain_error(not_newline, Opt) --> !,
	[ 'number ~w not newline' - [Opt] ].
domain_error(not_zero, Opt) --> !,
	[ '~w is not allowed in the domain' - [Opt] ].
domain_error(operator_priority, Opt) --> !,
	[ '~w invalid operator priority' - [Opt] ].
domain_error(operator_specifier, Opt) --> !,
	[ 'invalid operator specifier ~w' - [Opt] ].
domain_error(out_of_range, Opt) --> !,
	[ 'expression ~w is out of range' - [Opt] ].
domain_error(predicate_spec, Opt) --> !,
	[ '~w invalid predicate specifier' - [Opt] ].
domain_error(radix, Opt) --> !,
	[ 'invalid radix ~w' - [Opt] ].
domain_error(read_option, Opt) --> !,
	[ '~w invalid option to read_term' - [Opt] ].
domain_error(semantics_indicator, Opt) --> !,
	[ 'predicate indicator, got ~w' - [Opt] ].
domain_error(shift_count_overflow, Opt) --> !,
	[ 'shift count overflow in ~w' - [Opt] ].
domain_error(source_sink, Opt) --> !,
	[ '~w is not a source sink term' - [Opt] ].
domain_error(stream, Opt) --> !,
	[ '~w is not a stream' - [Opt] ].
domain_error(stream_or_alias, Opt) --> !,
	[ '~w is not a stream (or alias)' - [Opt] ].
domain_error(stream_encoding, Opt) --> !,
	[ '~w is not a supported stream encoding' - [Opt] ].
domain_error(stream_position, Opt) --> !,
	[ '~w is not a stream position' - [Opt] ].
domain_error(stream_property, Opt) --> !,
	[ '~w is not a stream property' - [Opt] ].
domain_error(syntax_error_handler, Opt) --> !,
	[ '~w is not a syntax error handler' - [Opt] ].
domain_error(table, Opt) --> !,
	[ 'non-tabled procedure ~w' - [Opt] ].
domain_error(thread_create_option, Opt) --> !,
	[ '~w is not a thread_create option' - [Opt] ].
domain_error(time_out_spec, Opt) --> !,
	[ '~w is not valid specificatin for time_out' - [Opt] ].
domain_error(unimplemented_option, Opt) --> !,
	[ '~w is not yet implemented' - [Opt] ].
domain_error(write_option, Opt) --> !,
	[ '~w invalid write option' - [Opt] ].
domain_error(Domain, Opt) -->
	[ '~w not a valid element for ~w' - [Opt,Domain] ].


object_name(array, array).
object_name(atom, atom).
object_name(atomic, atomic).
object_name(byte, byte).
object_name(callable, 'callable goal').
object_name(char, char).
object_name(character_code, 'character code').
object_name(compound, 'compound term').
object_name(db_reference, 'data base reference').
object_name(evaluable, 'evaluable term').
object_name(file, file).
object_name(float, float).
object_name(in_byte, byte).
object_name(in_character, character).
object_name(integer, integer).
object_name(key, 'database key').
object_name(leash_mode, 'leash mode').
object_name(library, library).
object_name(list, list).
object_name(message_queue, 'message queue').
object_name(mutex, mutex).
object_name(number, number).
object_name(operator, operator).
object_name(pointer, pointer).
object_name(predicate_indicator, 'predicate indicator').
object_name(source_sink, file).
object_name(unsigned_byte, 'unsigned byte').
object_name(unsigned_char, 'unsigned char').
object_name(variable, 'unbound variable').

svs([A=_]) --> !, { atom_codes(A, H) }, H.
svs([A=_|L]) -->
	{ atom_codes(A, H) },
	H,
	", ",
	svs(L).

list_of_preds([]) --> [].
list_of_preds([P|L]) -->
	['~q' - [P]],
	list_of_preds(L).


syntax_error_line('', _,_) --> !,
	[':~n' ].
syntax_error_line(File, Position,_) -->
	[' at ~a, near line ~d:~n' - [File,Position]].

syntax_error_term(0,J,L) -->
	['~n' ],
	syntax_error_term(10,J,L).
syntax_error_term(_,0,L) --> !,
	[ '~n<==== HERE ====>~n' ],
	syntax_error_term(10,-1,L).
syntax_error_term(_,_,[]) --> !.
syntax_error_term(I,J,[T-_P|R]) -->
	syntax_error_token(T),
	{
	 I1 is I-1,
	 J1 is J-1
	},
	syntax_error_term(I1,J1,R).

syntax_error_token(atom(A)) --> !,
	[ ' ~a' - [A] ].
syntax_error_token(number(N)) --> !,
	[ ' ~w' - [N] ].
syntax_error_token(var(_,S,_))  --> !,
	[ ' ~s'  - [S] ].
syntax_error_token(string(S)) --> !,
	[ ' ""~s"' - [S] ].
syntax_error_token('(') --> !,
	[ '('  ].
syntax_error_token(')') --> !,
	[ ' )'  ].
syntax_error_token(',') --> !,
	[ ' ,' ].
syntax_error_token(A) --> !,
	[ ' ~a' - [A] ].


%	print_message_lines(+Stream, +Prefix, +Lines)
%
%	Quintus/SICStus/SWI compatibility predicate to print message lines
%       using  a prefix.

/** @pred  print_message_lines(+ _Stream_, + _Prefix_, + _Lines_) 


Print a message (see print_message/2) that has been translated to
a list of message elements.  The elements of this list are:

+ _Format_-_Args_
Where  _Format_ is an atom and  _Args_ is a list
of format argument.  Handed to `format/3`.
+ `flush`
If this appears as the last element,  _Stream_ is flushed
(see `flush_output/1`) and no final newline is generated.
+ `at_same_line`
If this appears as first element, no prefix is printed for
the first line and the line-position is not forced to 0
(see `format/1`, `~N`).
+ `<Format>`
Handed to `format/3` as `format(Stream, Format, [])`.
+ nl
A new line is started and if the message is not complete
the  _Prefix_ is printed too.


 
*/
prolog:print_message_lines(_S, _, []) :- !.
prolog:print_message_lines(_S, P, [at_same_line|Lines]) :- !,
	print_message_line(S, Lines, Rest),
	prolog:print_message_lines(S, P, Rest).
prolog:print_message_lines(S, kind(Kind), Lines) :- !,
	prefix(Kind, Prefix, _),
	lists:append([ begin(Kind, Ctx)
		  | Lines
		  ],
		  [ end(Ctx)
		  ],
		  AllLines),
	print_message_lines(S, Prefix, AllLines).
prolog:print_message_lines(S, P-Opts, Lines) :-
	atom(P), !,
	atom_concat('~N', P, Prefix),
	format(S, Prefix, Opts),
	print_message_line(S, Lines, Rest),
	prolog:print_message_lines(S, P-Opts, Rest).
prolog:print_message_lines(S, P, Lines) :-
	atom(P), !,
	atom_concat('~N', P, Prefix),
	format(S, Prefix, []),
	print_message_line(S, Lines, Rest),
	prolog:print_message_lines(S, P, Rest).

print_message_line(S, [flush], []) :- !,
	flush_output(S).
print_message_line(S, [], []) :- !,
	nl(S).
print_message_line(S, [nl|T], T) :- !,
	nl(S).
print_message_line(S, [begin(_,_)|T0], T) :- !,
	print_message_line(S, T0, T).
print_message_line(S, [end(_)|T0], T) :- !,
	print_message_line(S, T0, T).
print_message_line(S, [Fmt-Args|T0], T) :- !,
	format(S, Fmt, Args),
	print_message_line(S, T0, T).
print_message_line(S, [Fmt|T0], T) :-
	format(S, Fmt, []),
	print_message_line(S, T0, T).


prefix(help,	      '',          user_error) --> [].
prefix(query,	      '',          user_error) --> [].
prefix(debug,	      '',          user_output) --> [].
prefix(warning,	      '% ',      user_error) -->
	{ thread_self(Id) },
	(   { Id == main }
	->  [ 'Warning: ', nl ]
	;   { atom(Id) }
	->  ['Warning: [Thread ~a ]' - [Id], nl ]
	;   ['Warning: [Thread ~d ]' - [Id], nl ]
	).
prefix(error,	      '     ',   user_error) -->
	{ recorded(sp_info,local_sp(P,_,_,_),_) },
	{ thread_self(Id) },
	(   { Id == main }
	->  [ 'ERROR at ' ]
	;   { atom(Id) }
	->  [ 'ERROR [ Thread ~a ] at ' - [Id] ]
	;   [ 'ERROR [ Thread ~d ] at ' - [Id] ]
	),
	'$hacks':display_pc(P),
	!,
	[' !!', nl].
prefix(error,	      '     ',   user_error) -->
	{ thread_self(Id) },
	(   { Id == main }
	->  [ 'ERROR!!', nl ]
	;   { atom(Id) }
	->  [ 'ERROR!! [ Thread ~a ]' - [Id], nl ]
	;   [ 'ERROR!! [ Thread ~d ]' - [Id], nl ]
	).
prefix(banner,	      '',	   user_error) --> [].
prefix(informational, '~*|% '-[LC],     user_error) -->
	{ '$show_consult_level'(LC) }.
prefix(debug(_),      '% ',	   user_error).
prefix(information,   '% ',	   user_error).


clause_to_indicator(T, M:Name/Arity) :-
	strip_module(T, M, T1),
	pred_arity( T1, Name, Arity ).

pred_arity(V,call,1) :- var(V), !.
pred_arity((H:-_),Name,Arity) :- 
    nonvar(H),
    !,
    functor(H,Name,Arity).
pred_arity((H-->_),Name,Arity) :- !,
    nonvar(H),
    !,
    functor(H,Name,A1),
    Arity is A1+2.
pred_arity(H,Name,Arity) :-
    functor(H,Name,Arity).


/**
@}
*/
