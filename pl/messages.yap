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

/**
 * @file   messages.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 10:22:26 2015
 *
 * @brief  The YAP Message Handler
 *
 *
*/

/**

  @defgroup Messages Message Handling
  @ingroup YAPControl

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
+ `silent`,M,Na,Ar,File, FilePos]],
	[nl, nl].
caller( error(_,Term), _) -->
	{  lists:memberchk([g|g(Call)], Term) },
	['~*|called from
messages that do not produce output but that can be intercepted by hooks.


The next table shows the main predicates and hooks associated to message
handling in YAP:

@{

*/



:- module(system('$messages'),
	  [system_message/4,
	   prefix/6,
	   prefix/5,
	   file_location/3]).


:- use_system_module( user, [message_hook/3]).

:- multifile prolog:message/3.

:- multifile user:message_hook/3.


/** @pred  message_to_string(+ _Term_, - _String_)


Translates a message-term into a string object. Primarily intended for SWI-Prolog emulation.



 */
prolog:message_to_string(Event, Message) :-
	translate_message(Event, warning, Message, []).


%%	@pred compose_message(+Term, +Level, +Lines, -Lines0) is det
%
%	Print the message if the user did not intercept the message.
%	The first is used for errors and warnings that can be related
%	to source-location.  Note that syntax errors have their own
%	source-location and should therefore not be handled this way.
compose_message( Term, Level ) -->
	['   ~w:'- [Level]
	],
	prolog:message(Term), !.
compose_message( query(_QueryResult,_), _Level) -->
	[].
compose_message( absolute_file_path(File), _Level) -->
	[ '~N~n  absolute_file of ~w' - [File] ].
compose_message( absolute_file_path(Msg, Args), _Level) -->
	[ '     : ' - [],
	  Msg - Args,
	  nl ].
compose_message( arguments([]), _Level) -->
	[].
compose_message( arguments([A|As]), Level) -->
	[ '  ~w' - [A],
	  nl ],
	  compose_message( arguments(As), Level).
compose_message( ancestors([]), _Level) -->
	[ 'There are no ancestors.' ].
compose_message( breakp(bp(debugger,_,_,M:F/N,_),add,already), _Level) -->
	[ 'There is already a spy point on ~w:~w/~w.' - [M,F,N] ].
compose_message( breakp(bp(debugger,_,_,M:F/N,_),add,ok), _Level) -->
	[ 'Spy point set on ~w:~w/~w.' - [M,F,N] ].
compose_message( breakp(bp(debugger,_,_,M:F/N,_),remove,last), _Level) -->
	[ 'Spy point on ~w:~w/~w removed.' - [M,F,N] ].
compose_message( breakp(no,breakpoint_for,M:F/N), _Level) -->
	[ 'There is no spy point on ~w:~w/~w.' - [M,F,N] ].
compose_message( breakpoints([]), _Level) -->
	[ 'There are no spy-points set.' ].
compose_message( breakpoints(L), _Level) -->
	[ 'Spy-points set on:' ],
	list_of_preds(L).
compose_message( clauses_not_together(P), _Level) -->
	[ 'Discontiguous definition of ~q.' - [P] ].
compose_message( debug(debug), _Level) -->
	[ 'Debug mode on.' - [] ].
compose_message( debug(off), _Level) -->
	[ 'Debug mode off.'- [] ].
compose_message( debug(trace), _Level) -->
	[ 'Trace mode on.'- [] ].
compose_message( declaration(Args,Action), _Level) -->
	[ 'declaration ~w ~w.' - [Args,Action] ].
compose_message( defined_elsewhere(P,F), _Level) -->
	[  'predicate ~q previously defined in file ~w' - [P,F] ].
compose_message( functionality(Library), _Level) -->
	[  '~q not available' - [Library] ].
compose_message( import(Pred,To,From,private), _Level) -->
	[ 'Importing private predicate ~w:~w to ~w.' - [From,Pred,To] ].
compose_message( redefine_imported(M,M0,PI), _Level) -->
	{ source_location(ParentF, Line) },
	[ '~w:~w: Module ~w redefines imported predicate ~w:~w.' - [ParentF, Line, M,M0,PI] ].
compose_message( leash([]), _Level) -->
	[ 'No leashing.' ].
compose_message( leash([A|B]), _Level) -->
	[ 'Leashing set to ~w.' - [[A|B]] ].
compose_message( no, _Level) -->
	[ 'no' - []  ].
compose_message( no_match(P), _Level) -->
	[ 'No matching predicate for ~w.' - [P] ].
compose_message( leash([A|B]), _Level) -->
	[  'Leashing set to ~w.' - [[A|B]] ].
compose_message( halt, _Level) --> !,
	[ 'YAP execution halted.'-[] ].
compose_message( false, _Level) --> !,
	[ 'false.'-[] ].
compose_message( '$abort', _Level) --> !,
	[ 'YAP execution aborted'-[] ].
compose_message( abort(user), _Level) --> !,
	[ 'YAP execution aborted' - [] ].
compose_message( loading(_,F), _Level) --> { F == user }, !.
compose_message( loading(What,FileName), _Level) --> !,
	[ '~a ~w...' - [What, FileName] ].
compose_message( loaded(_,user,_,_,_), _Level) --> !.
compose_message( loaded(included,AbsFileName,Mod,Time,Space), _Level) --> !,
	[ '~a included in module ~a, ~d msec ~d bytes' -
		     [AbsFileName,Mod,Time,Space] ].
compose_message( loaded(What,AbsoluteFileName,Mod,Time,Space), _Level) --> !,
	[ '~a ~a in module ~a, ~d msec ~d bytes' -
		     [What, AbsoluteFileName,Mod,Time,Space] ].
compose_message(trace_command(-1), _Leve) -->
	[ 'EOF is not a valid debugger command.'  ].
compose_message(trace_command(C), _Leve) -->
	[ '~c is not a valid debugger command.' - [C] ].
compose_message(trace_help, _Leve) -->
	[ '   Please enter a valid debugger command (h for help).'  ].
compose_message(version(Version), _Leve) -->
	[ '~a' - [Version] ].
compose_message(myddas_version(Version), _Leve) -->
	[ 'MYDDAS version ~a' - [Version] ].
compose_message(yes, _Level) --> !,
	[  'yes'- []  ].
compose_message(Term, Level) -->
		{ '$show_consult_level'(LC) },
	location(Term, Level, LC),
	main_message( Term, Level, LC ),
	c_goal( Term, Level ),
	caller( Term, Level ),
	extra_info(  Term, Level ),
	!,
	[nl,nl].
compose_message(Term, Level) -->
	{  Level == error -> true ; Level == warning },
	{ '$show_consult_level'(LC) },
	main_message( Term, Level, LC),
	[nl,nl].

location(error(syntax_error(syntax_error(_,between(_,LN,_),FileName,_)),_), _ , _) -->
	!,
	[ '~a:~d:0 ' - [FileName,LN] ] .
location(error(style_check(style_check(_,LN,FileName,_ ) ),_), _ , _) -->
	%	  { stream_position_data( line_count, LN) },
	!,
	[ '~a:~d:0 ' - [FileName,LN] ] .
location( error(_,Term), Level, LC ) -->
	{  source_location(F0, L),
	   stream_property(_Stream, alias(loop_stream)) }, !,
	display_consulting( F0, Level, LC ),
	{ lists:memberchk([p|p(M,Na,Ar,_File,_FilePos)], Term ) },
	[  '~a:~d:0 ~a in ~a:~q/~d:'-[F0, L,Level,M,Na,Ar] ].
location( error(_,Term), Level, LC ) -->
	{ lists:memberchk([p|p(M,Na,Ar,File,FilePos)], Term ) }, !,
	display_consulting( File, Level, LC ),
	[  '~a:~d:0 ~a in ~a:~q/~d:'-[File, FilePos,Level,M,Na,Ar] ].

%message(loaded(Past,AbsoluteFileName,user,Msec,Bytes), Prefix, Suffix) :- !,
main_message(error(Msg,Info), _, _) --> {var(Info)}, !,
	[  ' error: uninstantiated message ~w~n.' - [Msg], nl ].
main_message( error(syntax_error(syntax_error(Msg,between(L0,LM,LF),_Stream,Term)),_), Level, LC ) -->
	!,
	  [' ~a: syntax error ~s' - [Level,Msg]],
	  [nl],
	  ( syntax_error_term( between(L0,LM,LF), Term, LC )
	  ->
	    []
	  ;
	    [' ~a: failed_processing syntax error term ~q' - [Level,Term]],
	    [nl]
	  ).
main_message(error(style_check(style_check(singleton(SVs),_Pos,_File,P)),_), Level, _LC) -->
    !,
%    {writeln(ci)},
	{ clause_to_indicator(P, I) },
	[  ' ~a: singleton variable~*c ~s in ~q.' - [ Level,  NVs, 0's, SVsL, I] ],
	{ svs(SVs,SVs,SVsL),
	  ( SVs = [_] -> NVs = 0 ; NVs = 1 )
	}.
main_message(error(style_check(style_check(multiple(N,A,Mod,I0),_Pos,File,_P)),_), Level, _LC) -->
    !,
	[  ' ~a: ~a redefines ~q from  ~a.' - [Level,File, Mod:N/A, I0] ].
main_message(error(style_check(style_check(discontiguous(N,A,Mod),_S,_W,_P)),_) , Level, _LC)-->
    !,
	[  ' ~a: discontiguous definition for ~p.' - [Level,Mod:N/A] ].
main_message(error(consistency_error(Who)), Level, _LC) -->
    !,
	[ ' ~a: has argument ~a not consistent with type.'-[Level,Who] ].
main_message(error(domain_error(Who , Type), _Where), Level, _LC) -->
    !,
    	[ ' ~a:  ~q does not belong to domain ~a,' - [Level,Type,Who], nl ].
main_message(error(evaluation_error(What, Who), _Where), Level, _LC) -->
        !,
        [ ' ~a: ~w caused ~a during evaluation of arithmetic expressions,' - [Level,Who,What], nl ].
main_message(error(existence_error(Type , Who), _Where), Level, _LC) -->
    !,
	[  ' ~a:  ~q ~q could not be found,' - [Level,Type, Who], nl ].
main_message(error(permission_error(Op, Type, Id), _Where), Level, _LC) -->
	[ ' ~a:  ~q is not allowed in ~a ~q,' - [Level, Op, Type,Id], nl ].
main_message(error(instantiation_error, _Where), Level, _LC) -->
	[ ' ~a: unbound variable' - [Level], nl ].
main_message(error(representation_error(Type)), Level, _LC) -->
	[ ' ~a: ~a representation error ~a' - [Level, Type], nl ].
main_message(error(type_error(Type,Who), _What), Level, _LC) -->
	[ ' ~a: ~q should be of type ~a' - [Level,Who,Type]],
	[ nl ].
main_message(error(system_error(Who), _What), Level, _LC) -->
	[ ' ~a: ~q error' - [Level,Who]],
	[ nl ].
main_message(error(uninstantiation_error(T),_), Level, _LC) -->
	[ ' ~a: found ~q, expected unbound variable ' - [Level,T], nl ].

display_consulting( F, Level, LC) -->
	{  LC > 0,
	   source_location(F0, L),
		  F \= F0
	 }, !,
	[ '~a:~d:0: ~a  while compiling.'-[F0,L,Level], nl ].
display_consulting(_F, _, _LC) -->
	  [].

caller( error(_,Term), _) -->
	{  lists:memberchk([p|p(M,Na,Ar,File,FilePos)], Term ) },
	{  lists:memberchk([g|g(Call)], Term) },
	  !,
	  ['~*|goal was ~q' - [10,Call]],
	  [nl],
	  ['~*|exception raised from ~a:~q:~d, ~a:~d:0: '-[10,M,Na,Ar,File, FilePos]],
	[nl].
caller( error(_,Term), _) -->
	{ lists:memberchk([e|p(M,Na,Ar,File,FilePos)], Term ) },
	!,
	['~*|exception raised from  ~a:~q/~d, ~a:~d:0: '-[10,M,Na,Ar,File, FilePos]],
	[nl].
caller( error(_,Term), _) -->
	{  lists:memberchk([g|g(Call)], Term) },
	!,
	['~*|goal  ~q '-[10,Call]],
	[nl].
caller( _, _) -->
	[].

c_goal( error(_,Term), Level ) -->
	{ lists:memberchk([c|c(File, Line, Func)], Term ) },
	!,
	['~*|~a raised at C-function ~a() in ~a:~d:0: '-[10, Level, Func, File, Line]],
	[nl].
c_goal( _, _Level ) --> [].


prolog_message(X) -->
	system_message(X).

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
system_message(error(existence_error(mutex,Key), Where)) -->
    [ 'EXISTENCE ERROR- ~w: ~w is an erased mutex' - [Where,Key] ].
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
system_message(error(not_implemented(Type, What), Where)) -->
	[ '~w: ~w not implemented- ~w' - [Where, Type, What] ].
system_message(error(operating_SYSTEM_ERROR_INTERNAL, Where)) -->
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
	[ 'PERMISSION ERROR- ~w: cannot create ~a ~w' - [Where,Name,P] ].
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
	[ 'PERMISSION ERROR- ~w: cannot
	write to binary stream ~w' - [Where,Stream] ].
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
% SWI like I/O error message.
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

extra_info( error(_,Extra), _ ) -->
	    {lists:memberchk([i|Msg], Extra)}, !,
        ['~*|user provided data is: ~q' - [10,Msg]],
        [nl].
extra_info( _, _ ) -->
	[].

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

svs([A=VA], [A=VA], S) :- !,
	atom_string(A, S).
svs([A=VA,B=VB], [A=VA,B=VB], SN) :- !,
	atom_string(A, S),
	atom_string(B, S1),
	string_concat([S,` and `,S1], SN).
svs([A=_], _, SN) :- !,
	atom_string(A, S),
	string_concat(`, and `,S, SN).
svs([A=V|L], [A=V|L], SN) :- !,
	atom_string(A, S),
	svs(L, [A=V|L], S1 ),
	string_concat([ S, S1], SN).
svs([A=_V|L], All, SN) :- !,
	atom_string(A, S),
	svs(L, All, S1 ),
	string_concat([`, `, S, S1], SN).

list_of_preds([]) --> [].
list_of_preds([P|L]) -->
	['~q' - [P]],
	list_of_preds(L).

syntax_error_term(between(_I,_J,_L),LTaL,LC) -->
	['error found at line ~d to line ~d' - [_I,_L], nl ],
	syntax_error_tokens(LTaL, LC).

syntax_error_tokens([], _LC) --> [].
syntax_error_tokens([T|L], LC) -->
	syntax_error_token(T, LC),
	syntax_error_tokens(L, LC).

syntax_error_token(atom(A), _LC) --> !,
	[ '~q' - [A] ].
syntax_error_token(number(N), _LC) --> !,
	[ '~w' - [N] ].
syntax_error_token(var(_,S), _LC)  --> !,
	[ '~s'  - [S] ].
syntax_error_token(string(S), _LC) --> !,
	[ '`~s`' - [S] ].
syntax_error_token(error, _LC) --> !,
	[ ' <== HERE ==> ' ].
syntax_error_token('EOT', _LC) --> !,
	[ '.' - [], nl  ].
syntax_error_token('(', _LC) --> !,
	[ '( '- []  ].
syntax_error_token('{', _LC) --> !,
	[ '{ '- []  ].
syntax_error_token('[', _LC) --> !,
	[ '[' - [] ].
syntax_error_token(')', _LC) --> !,
	[ ' )'- []  ].
syntax_error_token(']', _LC) --> !,
	[ ']'- []  ].
syntax_error_token('}', _LC) --> !,
	[ ' }' - [] ].
syntax_error_token(',', _LC) --> !,
	[ ', ' - [] ].
syntax_error_token('.', _LC) --> !,
	[ '.' - [] ].
syntax_error_token(';', _LC) --> !,
	[ '; ' - [] ].
syntax_error_token(':', _LC) --> !,
	[ ':' - [] ].
syntax_error_token('|', _LC) --> !,
	[ '|' - [] ].
syntax_error_token(nl, LC) --> !,
	[  '~*|     ' -[LC], nl ].
syntax_error_token(B, _LC) --> !,
	[ nl, 'bad_token: ~q' - [B], nl ].


print_lines( S, _, Key) -->
	[nl, end(Key0)],
	{ Key == Key0 },
	!,
	{ nl(S),
	  flush_output(S) }.
print_lines( S, _, Key) -->
	[flush, end(Key0)],
	{ Key == Key0 },
	!,
	{ flush_output(S) }.
print_lines(S, _, Key) -->
	[ end(Key0) ],
	{ Key0 == Key }, !,
	{ nl(S) }.
print_lines( S, Prefix, Key) -->
	[at_same_line],
	!,
	print_lines( S, Prefix, Key).
print_lines( S, Prefixes, Key) -->
	[nl],
	!,
	{ nl(S),
	  Prefixes = [PrefixS - Cmds|More],
	  format(S, PrefixS, Cmds)
	},
	{
	   More == []
	  ->
	   NPrefixes = Prefixes
	  ;
	   NPrefixes = More
	},
	print_lines( S, NPrefixes, Key).
print_lines( S, Prefixes, Key) -->
	[flush],
	!,
	{ flush_output(S) },
	print_lines( S, Prefixes, Key ).
print_lines(S, Prefixes, Key) -->
	[end(_OtherKey)],
	!,
	print_lines( S, Prefixes, Key ).
% consider this a message within the message
print_lines(S, Prefixes, Key) -->
	[begin(Severity, OtherKey)],
	!,
	{ prefix( Severity, P ) },
	print_message_lines(S, [P], OtherKey),
	print_lines( S, Prefixes, Key ).
print_lines(S, Prefixes, Key) -->
	[prefix(Fmt-Args)],
	!,
	print_lines( S, [Fmt-Args|Prefixes], Key ).
print_lines(S, Prefixes, Key) -->
	[prefix(Fmt)],
	{ atom( Fmt ) ; string( Fmt ) },
	!,
	print_lines( S, [Fmt-[]|Prefixes], Key ).
print_lines(S, Prefixes, Key) -->
	[Fmt-Args],
	!,
	{ format(S, Fmt, Args) },
	print_lines( S, Prefixes, Key ).
print_lines(S, Prefixes, Key) -->
	[format(Fmt,Args)],
	!,
	{ format(S, Fmt, Args) },
	print_lines( S, Prefixes, Key ).
% deprecated....
print_lines(S, Prefixes, Key) -->
	[ Fmt ],
	{ atom(Fmt) ; string( Fmt ) },
	!,
	{ format(S, Fmt, []) },
	print_lines(S, Prefixes, Key).
print_lines(S, _Prefixes, _Key) -->
	[ Msg ],
	{ format(S, 'Illegal message Component: ~q !!!.~n', [Msg]) }.

prefix(help,	      '~N'-[]).
prefix(query,	      '~N'-[]).
prefix(debug,	      '~N'-[]).
prefix(warning,	      '~N'-[]).
/*	{ thread_self(Id) },
	(   { Id == main }
	->  [ 'warning, ' - [] ]
	;   { atom(Id) }
	->  ['warning [Thread ~a ], ' - [Id] ]
	;   ['warning [Thread ~d ], ' - [Id] ]
	).
*/
prefix(error,	      '~N'-[]).
/*
	{ thread_self(Id) },
	(   { Id == main }
	->  [ 'error ' ]
        ;   { thread_main_name(Id) }
	->  [ 'error [ Thread ~w ] ' - [Id] ]
	),
	!.
prefix(error,	      '',   user_error) -->
	{ thread_self(Id) },
	(   { Id == main }
	->  [ 'error ' - [], nl ]
	;   { atom(Id) }
	->  [ 'error [ Thread ~a ] ' - [Id], nl ]
	;   [ 'error [ Thread ~d ] ' - [Id], nl ]
	).
*/
prefix(banner,	      '~N'-[]).
prefix(informational, '~N~*|% '-[LC]) :-
	'$show_consult_level'(LC).
prefix(debug(_),      '~N% '-[]).
prefix(information,   '~N% '-[]).


clause_to_indicator(T, MNameArity) :-
	strip_module(T, M0, T1),
	pred_arity( T1, M0, MNameArity ).

pred_arity(V, M, M:call/1) :- var(V), !.
pred_arity((:- _Path), _M, prolog:(:-)/1 ) :- !.
pred_arity((?- _Path), _M, prolog:(?)/1 ) :- !.
pred_arity((H:-_),M, MNameArity) :-
    nonvar(H),
    !,
    strip_module(M:H, M1, H1),
    pred_arity( H1, M1, MNameArity).
pred_arity((H-->_), M, M2:Name//Arity) :-
    nonvar(H),
    !,
    strip_module(M:H, M1, H1),
    pred_arity( H1, M1, M2:Name/Arity).
% special for a, [x] --> b, [y].
pred_arity((H,_), M, MNameArity) :-
    nonvar(H),
    !,
    strip_module(M:H, M1, H1),
    pred_arity( H1, M1, MNameArity).
pred_arity(Name/Arity, M, M:Name/Arity) :-
    !.
pred_arity(Name//Arity, M, M:Name//Arity) :-
    !.
pred_arity(H,M, M:Name/Arity) :-
    functor(H,Name,Arity).


translate_message(Term, Level) -->
	compose_message(Term, Level), !.
translate_message(Term, _) -->
	{ Term = error(_, _) },
	[ 'Unknown exception: ~p'-[Term] ].
translate_message(Term, _) -->
	[ 'Unknown message: ~p'-[Term] ].

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
the  line and the line-position is not forced to 0
(see `format/1`, `~N`).
+ `prefix`(Prefix)
define a prefix for the next line, say `''` will be seen as an
empty prefix.
(see `format/1`, `~N`).
+ `<Format>`
Handed to `format/3` as `format(Stream, Format, [])`, may get confused
with other commands.
+ nl
A new line is started and if the message is not complete
the  _Prefix_ is printed too.
*/
prolog:print_message_lines(S, Prefix0, Lines) :-
	Lines = [begin(_, Key)|Msg],
	(
	 atom(Prefix0)
	->
	 Prefix = Prefix0-[]
	;
	 string(Prefix0)
	->
	 Prefix = Prefix0-[]
	;
	 Prefix = Prefix0
	),
	(Msg = [at_same_line|Msg1]
	->
	 print_lines(S, [Prefix], Key, Msg1, [])
		;
	 print_lines(S, [Prefix], Key, [Prefix|Msg], [])
	).

/** @pred prolog:print_message(+ Severity, +Term)

The predicate print_message/2 is used to print messages, notably from
exceptions, in a human-readable format.  _Kind_ is one of
`informational`, `banner`, `warning`, `error`, `help` or `silent`. In YAP, the message is always outut to the stream user_error.

If the Prolog flag verbose is `silent`, messages with
 _Kind_ `informational`, or `banner` are treated as
  silent.  See `-q` in [Running_YAP_Interactively].

This predicate first translates the  _Term_ into a list of `message
lines` (see print_message_lines/3 for details).  Next it will
call the hook message_hook/3 to allow the user intercepting the
message.  If message_hook/3 fails it will print the message unless
 _Kind_ is silent.

If you need to report errors from your own predicates, we advise you to
stick to the existing error terms if you can; but should you need to
invent new ones, you can define corresponding error messages by
asserting clauses for `prolog:message/2`. You will need to declare
the predicate as multifile.

Note: errors in the implementation of print_message/2 are very
confusing to YAP (who will process the error?). So we write this small
stub to ensure everything os ok

*/
prolog:print_message(Severity, Msg) :-
	(
	 var(Severity)
	->
	 !,
	 format(user_error, 'malformed message ~q: message level is unbound~n', [Msg])
	;
	 var(Msg)
	->
	 !,
	 format(user_error, 'uninstantiated message~n', [])
 ;
	 Severity == silent
 ->
	 true
	;
	 '$pred_exists'(portray_message(_,_),user),
	 user:portray_message(Severity, Msg)
	),
	!.
prolog:print_message(Level, _Msg) :-
	current_prolog_flag(verbose_load, silent),
	stream_property(_Stream, alias(loop_stream) ),
	Level = informational,
	!.
prolog:print_message(Level, _Msg) :-
	current_prolog_flag(verbose, silent),
	Level \= error,
	Level \= warning,
	!.
prolog:print_message(_, _Msg) :-
	% first step at hook processing
	'$nb_getval'('$if_skip_mode',skip,fail),
	!.
prolog:print_message(force(_Severity), Msg) :- !,
	print(user_error,Msg).
% This predicate has more hooks than a pirate ship!
prolog:print_message(Severity, Term) :-
	prolog:message( Term,Lines0, [ end(Id)]),
	Lines = [begin(Severity, Id)| Lines0],
	(
	 user:message_hook(Term, Severity, Lines)
	->
	 true
	;
	 prefix( Severity, Prefix ),
	 prolog:print_message_lines(user_error, Prefix, Lines)
	),
	!.
prolog:print_message(Severity, Term) :-
	translate_message( Term, Severity, Lines0, [ end(Id)]),
	Lines = [begin(Severity, Id)| Lines0],
	(
	 user:message_hook(Term, Severity, Lines)
	->
	 true
	;
	 prefix( Severity, Prefix ),
	 prolog:print_message_lines(user_error, Prefix, Lines)
	),
	!.
prolog:print_message(Severity, _Term) :-
	format('No handler for ~a message ~q,~n',[Severity, _Term]).


/**
  @}
*/
