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
* Last rev:     $Date: 2008-07-16 10:58:59 $,$Author: vsc            	 *
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


:- system_module('$messages',
	  [system_message/4,
	   file_location/3],
	  [
	      message//1,
	      message_to_string/2,
	      print_message_lines/4,
	      print_message/2
	  ]).

/**

  @defgroup YAPMessages Message Handling
  @{
  @ingroup YAPErrors

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
messages that do not produce output but that can be intercepted by hooks.


The next table shows the main predicates and hooks associated to message
handling in YAP:


An error record comsists of An ISO compatible descriptor of the format

error(errror_kind(Culprit,..), In)

In YAP, the info field describes:

- what() more detauls on the event

- input_stream, may be one of;

  - loop_stream
  - file()
  - non
e
  - prolog_source(_) a record containing file, line, predicate, and clause
 that activated the goal, or a list therof. YAP tries to search for the user
 code generatinng the error.

 - c_source(0), a record containing the line of C-code thhat caused the event. This
 is reported under systm debugging mode, or if  this is user code.

 - stream_source() - a record containg data on the the I/O stream datum causisng the evwnt.

 - user_message () - text on the event.


*/

:- dynamic  message//1.
:- multifile prolog:message//1.


/** @pred  message_hook(+ _Term_, + _Kind_, + _Lines_)


Hook predicate that may be define in the module `user` to intercept
messages from print_message/2.  _Term_ and  _Kind_ are the
same as passed to print_message/2.  _Lines_ is a list of
format statements as described with print_message_lines/3.

This predicate should be defined dynamic and multifile to allow other
modules defining clauses for it too.


*/
:- multifile user:message_hook/3.


:- multifile user:portray_message/2.

:- dynamic user:portray_message/2.

/** @pred  message_to_string(+ _Term_, - _String_)


``Translates a message-term into a string object. Primarily intended for SWI-Prolog emulation.



 */
message_to_string(Event, Message) :-
    translate_message(Event, Message, []).


%%	@pred translate_message(+Term, +Lines, -Lines0) is det
%
%	Print the message if the user did not intercept the message.
%	The first is used for errors and warnings that can be related
%	to source-location.  Note that syntax errors have their own
%	source-location and should therefore not be handled this way.
translate_message(answer(Vs, GVs, LGs)) -->
	!,
	write_query_answer( Vs, GVs , LGs).
translate_message( absolute_file_path(File)) -->
	!,
    [ '~N~n  absolute_file of ~w' - [File] ].
translate_message( absolute_file_path(Msg, Args)) -->
	!,
    [ '     : ' - [],
      Msg - Args,
      nl ].
translate_message( absolute_file_path_component(Msg, Args)) -->
    !,
    [ '     ~s: ' - [Msg]],
    ['$messages':seq(Args) ].
translate_message( arguments([])) -->
    !.
translate_message( arguments([A|As])) -->
    !,
    [ '  ~w' - [A],
      nl ],
    translate_message( arguments(As)).
translate_message( ancestors([])) -->
    !,
    [ 'There are no ancestors.' ].
translate_message( breakp(bp(debugger,_,_,M:F/N,_),add,already)) -->
    !,
    [ 'There is already a spy point on ~w:~w/~w.' - [M,F,N] ].
translate_message( breakp(bp(debugger,_,_,M:F/N,_),add,ok)) -->
    !,
    [ 'Spy point set on ~w:~w/~w.' - [M,F,N] ].
translate_message( breakp(bp(debugger,_,_,M:F/N,_),remove,last)) -->
    !,
    [ 'Spy point on ~w:~w/~w removed.' - [M,F,N] ].
translate_message( breakp(no,breakpoint_for,M:F/N)) -->
    !,
    [ 'There is no spy point on ~w:~w/~w.' - [M,F,N] ].
translate_message( breakpoints([])) -->
    !,
    [ 'There are no spy-points set.' ].
translate_message( breakpoints(L)) -->
	!,
    [ 'Spy-points set on:' ],
    list_of_preds(L).
translate_message( clauses_not_together(P)) -->
	!,
    [ 'Discontiguous definition of ~q.' - [P] ].
translate_message( debug(debug)) -->
	!,
    [ 'Debug mode on.' - [] ].
translate_message( debug(off)) -->
	!,
    [ 'Debug mode off.'- [] ].
translate_message( debug(trace)) -->
	!,
    [ 'Trace mode on.'- [] ].
translate_message( declaration(Args,Action)) -->
	!,
    [ 'declaration ~w ~w.' - [Args,Action] ].
translate_message( defined_elsewhere(P,F)) -->
	!,
    [  'predicate ~q previously defined in file ~w' - [P,F] ].
translate_message( functionality(Library)) -->
	!,
	[  '~q not available' - [Library] ].
translate_message( import(Pred,To,From,private)) -->
    	!,
	[ 'Importing private predicate ~w:~w to ~w.' - [From,Pred,To] ].
translate_message( redefine_imported(M,M0,PI)) -->
  	!,
	{ source_location(ParentF, Line) },
	[ '~N~w:~w:~w Module ~w redefines imported predicate ~w:~w.' - [ParentF, Line,0, M,M0,PI] ].
translate_message( leash([])) -->
	!,
    [ 'No leashing.' ].
translate_message( leash([A|B])) -->
	!,
    [ 'Leashing set to ~w.' - [[A|B]] ].
translate_message(yes) -->
    !,
    [  '.' - [],
       nl,
       'yes~n'- []  ].
translate_message(false) -->
    !,
    [  '~Nno~n'- []  ].
translate_message( no) -->
    !,
    [ '~Nno~n' - []  ].
translate_message( no_match(P)) -->
	!,
	[ 'No matching predicate for ~w.' - [P] ].
translate_message( leash([A|B])) -->
	!,
	[  'Leashing set to ~w.' - [[A|B]] ].
translate_message( halt) -->
    !,
    [ 'YAP execution halted.'-[] ].
translate_message( '$abort') --> !,
				       [ 'YAP execution aborted'-[] ].
translate_message( abort(user)) --> !,
					  [ 'YAP execution aborted' - [] ].
translate_message( loading(_,F)) --> { F == user }, !.
translate_message( loading(What,FileName)) --> !,
	{ '$show_consult_level'(LC) },
	[ '~N~*|~s ~w...' - [LC, What, FileName] ].
translate_message( loaded(_,user,_,_,_)) --> !.
translate_message( loaded(What,AbsFileName,Mod,Time,Space)) --> !,
        { '$show_consult_level'(LC) },
	[ '~N~*|~a ~a in module ~a, ~d msec ~d bytes' -   [LC, AbsFileName,What,Mod,Time,Space] ].
translate_message(signal(SIG,_)) -->
    !,
    [ 'UNEXPECTED SIGNAL: ~a' - [SIG] ].
translate_message(trace_command(C)) -->
    !,
    [ '~a is not a valid debugger command.' - [C] ].
translate_message(trace_help) -->
    !,
    [ '   Please enter a valid debugger command (h for help).'  ].
translate_message(version(yap(Mj,Mi,Patch,_),VersionGit,AT,Saved)) -->
    !,
    {      sub_atom( VersionGit, 0, 8, _, VERSIONGIT
		   ) },
    [ 'YAP ~d.~d.~d-~a (compiled  ~a)~n' - [Mj,Mi, Patch, VERSIONGIT,  AT],
     'database loaded from ~a~n' - [Saved] ].
translate_message(myddas_version(Version)) -->
    !,
    [ 'MYDDAS version ~a' - [Version] ].
translate_message(throw(BALL)) -->
    !,
    [ 'WARNING: throw of  ~W had no catch' - [BALL,[]] ].
translate_message( Term ) -->
    message(Term), !.



translate_message(error(style_check(What,File,Line,Clause),Exc))-->
    !,
    {      error_descriptor(Exc, Desc),
     '$show_consult_level'(LC) },
  syntax_error_location( Desc, error, full, LC),
  main_message(error(style_check(What,File,Line,Clause),Exc),  warning, LC ).
translate_message(error(syntax_error(E), Info)) -->
    {
     '$show_consult_level'(LC),
      error_descriptor(Info, Desc),
     Level = error
    },
      %{start_low_level_trace},
    syntax_error_location( Desc, Level,full , LC),
   main_message(error(syntax_error(E),Info) , Level, LC ),
    c_goal( Desc, Level, LC ),
    extra_info( Desc, Level, LC ),
    stack_info( Desc, Level, LC ),
    !,
    [nl],
    [nl].
translate_message(error(E, Info)) -->
    {
     '$show_consult_level'(LC),
      error_descriptor(Info, Desc),
     Level = error
    },
      %{start_low_level_trace},
    location( Desc, Level,full , LC),
   main_message(error(E,Info) , Level, LC ),
    c_goal( Desc, Level, LC ),
    extra_info( Desc, Level, LC ),
    stack_info( Desc, Level, LC ),
    !,
    [nl],
    [nl].
translate_message(error(user_defined_error(Error),Info))-->
    !,
    { '$show_consult_level'(LC),
         error_descriptor(Info, Desc) },
   location(Desc, error, ful, LC),
    translate_message(Error).
translate_message(error(Exc, Info)) -->
 {
     '$show_consult_level'(LC),
        error_descriptor(Info, Desc),
% Level = error,
     Exc \= exception(_)
    },
    !,
    c_goal( Desc, error, LC ).
translate_message(error(Descriptor,exception(Error))) -->
	!,
    [ 'ERROR NOT RECOGNISED - ~w unsupported both by YAP system code and by  user hooks:' -  [Descriptor] , nl],
    [ '~@' - ['print_exception'(Error)] ,nl].
translate_message(Throw) -->
    !,
    [Throw].

seq([]) --> [].
seq([A|Args]) -->
    [['~w '] - A],
    seq(Args).

/** @pred location: output error location.
 *
 */
:- set_prolog_flag(discontiguous_warnings, false).

syntax_error_location( Desc, Level, _More, _LC ) -->
    {
     %       query_exception(parserReadingCode, Desc, true),
     query_exception(parserLine, Desc, LN), 
     nonvar(LN),
     query_exception(parserFile, Desc, FileName),
     nonvar(FileName),
     query_exception(parserPos, Desc, Pos),
     (var(Pos) -> Pos=1;true),
     query_exception(parserPos, Desc, Pos)
    },
    [  '~n~s:~d:~d ~a:' -[FileName, LN,Pos,Level] ],
     ({ query_exception(parserTextA,Desc, TextA) }
     ->
	 [ '~n%%%%%%%%%%%~n~s'-[ TextA]]
     ;
     []
     ),
     ['<<<<<<<<<<<<< Syntax Error found at line ~d>>>>>>>' -[LN]],
     ({ query_exception(parserTextB, Desc, TextB) }
     ->
	 [ '~n~s~n~NN%%%%%%%%%%%~n~n'-[ TextB]]
     ;
     []
     ).

location( Desc, Level, More, LC ) -->
    {
%     query_exception(prologConsulting, Desc, true),
     %       query_exception(parserReadingCode, Desc, true),
     query_exception(parserLine, Desc, LN),
     nonvar(LN),
     query_exception(parserFile, Desc, FileName),
     nonvar(FileName),
     query_exception(parserPos, Desc, Pos),
     (var(Pos) -> Pos=1;true)
    },
    [  '~N~s:~d:~d ~a:' -[FileName, LN,Pos,Level], nl ],
    !,
    ({More == full}
    ->
    prolog_caller( Desc, Level, LC)
    ;
    []
    ).
location( Desc, Level, _, LC ) -->
    prolog_caller( Desc, Level, LC).


prolog_caller( Desc, Level, LC ) -->
    {
     query_exception(prologPredLine, Desc, LN),
     query_exception(prologPredFile, Desc, FileName),
     query_exception(prologPredName, Desc, Name),
     query_exception(prologPredArity, Desc, Arity),
     query_exception(prologPredModule, Desc, Module)
    },
    !,
    [  '~N~s:~d:0 ~a executing ~s:~s/~d:'-[FileName, LN,Level,Module,Name,Arity] ],
     [nl],
     c_caller( Desc, Level, LC).
prolog_caller( Desc, Level, LC ) -->
    {
     query_exception(prologPredFile, Desc, FileName),
     query_exception(prologPredName, Desc, Name),
     query_exception(prologPredArity, Desc, Arity),
     query_exception(prologPredModule, Desc, Module)
 },
     !,
	     [  '~N~s:1:0 ~a executing ~s:~s/~d:'-[FileName, 1,Level,Module,Name,Arity] ],
     [nl],
     c_caller( Desc, Level, LC).

prolog_caller( Desc, Level, LC ) -->
    [  '~Nuser:~d:0 ~a in top-level goal.'-[0,Level]],
    [nl],
    c_caller( Desc, Level, LC).

c_caller( Desc, Level, _LC ) -->
    {
     query_exception(errorFile, Desc, FileName),
     query_exception(errorLine, Desc, LN),
     query_exception(errorFunction, Desc, F)
    },
    !,
    [  '~N~s:~d:0: ~a in ~s():'-[FileName, LN,Level,F] ].
c_caller( _Desc, _Level, _LC ) --> [].

event(redo, _Desc) --> {fail}.
event(fail, _Desc) --> {fail}.
event(abort, Desc) --> { throw(event(abort, Desc)) }.

simplify_pred(user:F, F) :- !.
simplify_pred(prolog:F, F) :- !.
simplify_pred(F, F).

%message(loaded(Past,AbsoluteFileName,user,Msec,Bytes), Prefix_, Suffix) :- !,

main_message(error(Msg,In), _, _) -->
    {var(Msg)}, !,
				      [  'Uninstantiated message ~w~n.' - [error(Msg,In)], nl ].
main_message(error( style_check(singleton(SVs),_Pos, _File,P), _Exc), _Level, LC) -->
    !,
    {
	clause_to_indicator(P, I),
	svs(SVs, SVsL),
	( SVs = [_] -> NVs = 0 ; NVs = 1 )
    },
    [
	nl,
	'~*|singleton variable~*c ~s in ~q.' -
	[ LC,  NVs, 0's, SVsL, I]  % '
    ].
main_message(error(style_check(discontiguous(N,A,Mod),_Pos,_File,_P), _Exc), _Level, LC) -->
    !,
    [  '~*|discontiguous definition for ~p.' - [ LC,Mod:N/A] ].
main_message(error(style_check(multiple(N,A,Mod,F0),L,F,_P ), _Info), Level, LC) -->
    !,
    [ '~N~*|~a:~d:0: ~a: ~q previously defined in ~a!!'-[LC,F, L, Level ,Mod:N/A,F0], nl, nl ].
main_message(error(What, _Exc), _Level, LC) -->
    !,
    main_error_message(What, LC ).
main_message( error(syntax_error(Msg),_Info), _Level, _LC ) -->
    !,
    [  '[ Syntax Error:~n      ~s~n]'-[Msg]   ].
main_message(error(ErrorInfo,_), _Level, LC) -->
    [nl],
    main_error_message( ErrorInfo, LC ).


main_error_message(consistency_error(Who),LC) -->
    [ '~*|%%% argument ~a not consistent with type.'-[ LC,Who] ].
main_error_message(domain_error(Who , Type),LC) -->
    [ '~*|%%% domain error: ~q does not belong to domain ~q.' - [ LC,Type,Who] ].
main_error_message(evaluation_error(What),LC) -->
    [ '~*|%%% found ~w during evaluation of arithmetic expression.' - [ LC,What] ].
main_error_message(evaluation_error(What, Who),LC) -->
    [ '~*|%%% ~w caused ~a during evaluation of arithmetic expressions.' - [ LC,Who,What]].
main_error_message(existence_error(Type , Who),LC) -->
    [  '~*|%%% ~q ~q does not exist.' - [ LC,Type, Who] ].
main_error_message(permission_error(Op, Type, Id),LC) -->
    [ '~*|%%% value ~q is not allowed in ~a ~q.' - [ LC, Op, Type,Id] ].
main_error_message(instantiation_error,LC) -->
    [ '~*|%%% unbound variable.' - [LC] ].
main_error_message(representation_error(Type),LC) -->
    [ '~*|%%% YAP cannot represent as ~w.' - [LC, Type] ].
main_error_message(resource_error(Who),LC) -->
    [ '~*|%%% ~q.' - [LC,Who]].
main_error_message(syntax_error(Who),LC) -->
    [ '~*|%%% ~w while parsing.' - [ LC,Who]].
main_error_message(type_error(callable,PredicateIndicator),LC) -->
    { fix_pi(PredicateIndicator, Who) },
    !,
    [ '~*|%%% ~q should be of type ~a.' - [LC,Who,callable]].
main_error_message(type_error(predicate_indicator,PredicateIndicator),LC) -->
    { fix_pi(PredicateIndicator, Who) },
    !,
    [ '~*|%%% ~q should be of type ~a.' - [LC,Who,predicate_indicator]].
main_error_message(type_error(Type,Who),LC) -->
    [ '~*|%%% ~q should be of type ~a.' - [LC,Who,Type]].
main_error_message(system_error(Who, In),LC) -->
    [ '~*|%%% ~q ~q.' - [LC,Who, In]].
main_error_message(uninstantiation_error(T),LC) -->
    [ '~*|%%% found ~q, expected unbound variable.' - [LC,T]].
main_error_message(compilation_warning(Type,Mod,F),LC) -->
    [ '~*|%%% ~a: ~q in ~a.' - [LC,Type,F,Mod]].

fix_pi(Var, Var) :-
    var(Var),
    !.
fix_pi(Var:PI, Var:NPI) :-
    var(Var),
    !,
    fix_pi(PI,NPI).
fix_pi(user:PI, NPI) :-
    !,
    fix_pi(PI,NPI).
fix_pi(user:PI, NPI) :-
    !,
    fix_pi(PI,NPI).
fix_pi(prolog:PI, NPI) :-
    !,
    fix_pi(PI,NPI).
fix_pi(M:PI, M:NPI) :-
    !,
    fix_pi(PI,NPI).
fix_pi(P/0, P) :-
    !.
fix_pi(PI, PI) .


display_consulting( _F, Level, Info, LC) -->
    {  LC > 0,
       error_descriptor(Info, Desc),
       query_exception(prologParserFile, Desc, F0),
       query_exception(prologParserLine, Desc, L),
       integer(L)
%       ,       F \= F0
    }, !,
    [ '~a:~d:0: ~a raised!!:'-[F0,Level], nl ].

display_consulting(_F, _, _, _LC) -->
    [].

c_goal( Desc,_, LC) -->
	{
	  query_exception(errorGoal, Desc, G) },
	!,
	['~*|call was: ~w' - [LC,G]].
c_goal(_,_,_) --> [].


extra_info( Desc, Level, LC ) -->
   {
	query_exception(errorMsg, Desc, Msg),
	Msg \= '',
	Msg \= "",
	Msg \= []
    },
    !,
    [nl],
    ['~*|%'-[LC]],
    [nl],
    ['~*|% info: ~s' - [LC,Msg]],
    [nl],
    extra_info_(Desc, Level,LC).
extra_info(Desc, Level, LC  ) -->
    extra_info_( Desc, Level, LC ).

extra_info_( Desc, _Level, LC ) -->
   {
	query_exception(errorUserTerm, Desc, Goal),
	Msg \= '',
	Msg \= "",
	Msg \= []
    },
    !,
    [nl],
    ['~*|%'-[LC]],
    [nl],
    write_goal_output( Goal ),
    ['~*|% info: ~s' - [LC,Msg]],
    [nl].
extra_info_( _, _, _ ) --> [].

code_stream(Stream, S, L) :-
    open( codes(S,L), write, Stream ).

close_codes(Stream, L, L) :-
    close(Stream).

stack_info( _, _, _ ) --> !.
stack_info( Desc,_, LC) -->
    {
     query_exception(prologStack, Desc, Stack),
	Stack \= []

    },
    !,
    ['~*|Prolog execution stack is:' - [LC]],
    [nl],
    [Stack - []].
stack_info( _, _ ) -->
    [].



prolog_message(X) -->
    system_message(X).

system_message(error(Msg,In)) -->
    ( { var(Msg) } ; { var(In)} ), !,
    ['bad error ~w' - [error(Msg,In)]].
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
system_message(error(existence_error(procedure,P), context(Call,Parent))) --> !,      [ 'EXISTENCE ERROR- procedure ~w is undefined, called fromcontext  ~w~n                 Goal was ~w' - [P,Parent,Call] ].
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
    [ 'PERMISSION ERROR- ~w: cannot write to binary stream ~w' - [Where,Stream] ].
system_message(error(permission_error(output,stream,Stream), Where)) -->
    [ 'PERMISSION ERROR- ~w: cannot write to ~w' - [Where,Stream] ].
system_message(error(permission_erroro(utput,text_stream,Stream), Where)) -->
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
system_message(error(_,exception(Error))) -->
    [ 'UNHANDLED ERROR - unsupported by YAP engine or hooks: ~@' -  [print_exception(Error)] ].
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
				       [ 'non-iso built-in procedure' - [] ].
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
    [ '~w does not belong to domain ~w' - [Opt,Domain] ].

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

svs([A=_VA], S) :- !,
    atom_string(A, S).
svs([A=_VA,B=_VB], SN) :- !,
    atom_string(A, SA),
    atom_string(B, SB),
    string_concat([SA,` and `,SB], SN).
svs([A=_V|L], SN) :-
    atom_string(A, S),
    svs(L, S1 ),
    string_concat([S, `, `, S1], SN).

list_of_preds([]) --> [].
list_of_preds([P|L]) -->
    ['~q' - [P]],
    list_of_preds(L).

syntax_error_term(between(_I,J,_L),[S|T],_LC) -->
    {string(S)},
    !,
    [ '~s' - [S] ],
    [' <<<< at line ~d >>>> ~s' - [J,T], nl ].
syntax_error_term(between(_I,J,_L),LTaL,LC) -->
    syntax_error_tokens(LTaL, J, LC).

syntax_error_tokens([], _, _LC) --> [].
syntax_error_tokens([T|L], J, LC) -->
    syntax_error_token(T, J, LC),
    syntax_error_tokens(L, J, LC).

syntax_error_token(atom(A), _, _LC) --> !,
					[ '~q' - [A] ].
syntax_error_token(number(N), _, _LC) --> !,
					  [ '~w' - [N] ].
syntax_error_token(var(_,S), _, _LC)  --> !,
					  [ '~a'  - [S] ].
syntax_error_token(string(S), _, _LC) --> !,
					  [ '~s' - [S] ].
syntax_error_token(error, L, _LC) --> !,
				      [ ' <<<< at line ~d >>>> ' - [L] ].
syntax_error_token('EOT',_,  _LC) --> !,
				      [ '.' - [], nl  ].
syntax_error_token('(',_,  _LC) --> !,
				    [ '( '- []  ].
syntax_error_token('{',_,  _LC) --> !,
				    [ '{ '- []  ].
syntax_error_token('[', _, _LC) --> !,
				    [ '[' - [] ].
syntax_error_token(')',_,  _LC) --> !,
				    [ ' )'- []  ].
syntax_error_token(']',_,  _LC) --> !,
				    [ ']'- []  ].
syntax_error_token('}',_,  _LC) --> !,
				    [ ' }' - [] ].
syntax_error_token(',',_,  _LC) --> !,
				    [ ', ' - [] ].
syntax_error_token('.',_,  _LC) --> !,
				    [ '.' - [] ].
syntax_error_token(';', _, _LC) --> !,
				    [ '; ' - [] ].
syntax_error_token(':', _, _LC) --> !,
				    [ ':' - [] ].
syntax_error_token('|', _, _LC) --> !,
				    [ '|' - [] ].
syntax_error_token('l',_,  _LC) --> !,
				    [ '|' - [] ].
syntax_error_token(nl, _, LC) --> !,
				  [  '~*|     ' -[LC], nl ].
syntax_error_token(B,_,  _LC) --> !,
				  [ nl, 'bad_token: ~q' - [B], nl ].
%%%%%%%%%%%%%%%%%%%%

write_break_level -->
    { current_prolog_flag(break_level, BL ), BL > 0 },
    !
    ->
	['[~p] ' -[BL]].

write_break_level -->
    [].


write_query_answer( [], _ , _ ) -->
    !,
    write_break_level,
    [yes-[]].
write_query_answer( _Vs, GVs0 , LGs0 ) -->
    write_break_level,
    {	
	rational_term_to_forest(f(GVs0, LGs0), f(GVs,LGs), LTs, LGs),
	swap_eq(GVs,VGs),
	sort(VGs, SVGs),
	purge_dontcares(SVGs,IVs),
	prep_answer_var_by_var(IVs, GVs, VNames, AllBindings, LTs),
	list2conj_(AllBindings, Goals),
	yap_flag(toplevel_print_options, Opts)
    },
    !,
    ['~N~W'-  [Goals,[conjunction(true),variable_names(VNames)|Opts]],
    flush].
write_query_answer( _Vs, _GVs0 , _LGs0 ) -->
    [yes-[]].

list2conj_([Last], Last).
list2conj_([Head,Next|Tail], (Head,Goals)) :-
	list2conj_([Next|Tail], Goals).

purge_dontcares([],[]).
purge_dontcares([_=Name|Vs],NVs) :-
    atom_codes(Name, [C|_]), C is "_", !,
    purge_dontcares(Vs,NVs).
purge_dontcares([V|Vs],[V|NVs]) :-
    purge_dontcares(Vs,NVs).

prep_answer_var_by_var([],VNames, VNames) -->
    [].
prep_answer_var_by_var([Value=Name1,Value2=Name2|L],VNames, NVNames) -->
    { Value==Value2 },
    !,
    { replace_named_var(VNames,Name1=NV1,IVNames),
      replace_named_var(IVNames,Name2=NV2,I1VNames)
    },
    [NV2 = NV1],
    prep_answer_var_by_var([Value=Name1|L], I1VNames, NVNames).
prep_answer_var_by_var([Binding=Name|L],VNames,NVNames) -->
    { nonvar(Binding) },
    !,
    { replace_named_var(VNames,Name=V,IVNames)
    },
    [V = Binding],
    prep_answer_var_by_var(L,IVNames,NVNames).
prep_answer_var_by_var([_|L],VNames,NVNames) -->
    prep_answer_var_by_var(L,VNames,NVNames).

swap_eq([A=B|L],[B=A|NL]) :-
    swap_eq(L,NL).
swap_eq([],[]).

% ignore unbound variables

is_named_var([Name=V|_LVs],V0,N) :-
    V == V0,
    !,
    N = Name.
is_named_var([_|LVs],V,N) :-
    is_named_var(LVs,V,N).

replace_named_var([Name=V|LVs],Name=V,[Name=V|LVs]) :-
    var(V),
    !.
replace_named_var([Name=_V|LVs],Name=V,[Name=V|LVs]) :-
    !.
replace_named_var([B|LVs],NV,[B|NLVs]) :-
    replace_named_var(LVs,NV, NLVs).

gen_name_string(I,L,[C|L]) :- I < 26, !, C is I+65.
gen_name_string(I,L0,LF) :-
    I1 is I mod 26,
    I2 is I // 26,
    C is I1+65,
    gen_name_string(I2,[C|L0],LF).

write_goal_output(nl, First, First) -->
	!,
	['~N'-[]].
write_goal_output(Format-G, First, Next) -->
	!,
    G = [_|_], !,
    % dump on string first so that we can check whether we actually
    % had any output from the solver.
    {    format(codes(String),Format,G) },
    ( {String == [] }->
      % we didn't
      {  First = Next }
    ;
				% we did
	add_nl(First),
	[ '~s' - [String]],
	{ Next = next }
    ).


name_vars_in_goals(G, VL0, G) :-
    name_well_known_vars(VL0),
    term_variable_occurrences(G, AllVs0),
    msort(AllVs0, AllVs),
    name_vars_in_goals_(AllVs, VL0, 0, _).

name_well_known_vars([]).
name_well_known_vars([Name=V|NVL0]) :-
    var(V), !,
    V = '$VAR'(Name),
    name_well_known_vars(NVL0).
name_well_known_vars([_|NVL0]) :-
    name_well_known_vars(NVL0).

name_vars_in_goals_([],_, I, I).
name_vars_in_goals_([V, V1|NGVL], NamedVs, I0, IF) :-
    attvar(V),
    V==V1, % multiple occurrences
    I is I0+1,
    gen_name_string( I0, [],SName),
    atom_codes(Name, [95, 68 |SName]),
    \+ '$in'(Name=V, NamedVs),
    !,
    V = '$VAR'(Name),
    takev(NGVL, V, IGVL),
    name_vars_in_goals_(IGVL, [Name = V|NamedVs], I, IF).
name_vars_in_goals_([V, V1|NGVL], NamedVs, I0, IF) :-
    V==V1, % multiple occurrences
    I is I0+1,
    gen_name_string( I0,[],SName),
    atom_codes(Name, [95|SName]),
    \+ '$in'(Name=V, NamedVs),
    !,
    V = '$VAR'(Name),
    takev(NGVL, V, IGVL),
    name_vars_in_goals_(IGVL, [Name = V|NamedVs], I, IF).
name_vars_in_goals_([NV|NGVL],NamedVs, I0, IF) :-
    % singletons
    attvar(NV),
    !,
    gen_name_string( I0,[],SName),
    atom_codes(Name, [95|SName]),
    \+ '$in'(Name=V, NamedVs),
    !, 
  name_vars_in_goals_(NGVL,[Name = V|NamedVs], I0, IF).
name_vars_in_goals_([V|NGVL],NamedVs, I0, IF) :-
    % singletons
    V = '$VAR'('_'),
    !, 
    name_vars_in_goals_(NGVL, NamedVs,I0, IF).

takev([V|L], V1, NL) :-
    V == V1,
    !,
    takev(L, V1, NL).
takev(L, _V1, L).


%%%%%%%%%%%%%%%%%%%%%%
print_lines( S, A, Key) -->
    [Tok],
    !,
    print_lines_(Tok, S, A, Key).
print_lines( _S, _A, _Key) -->
    [].

print_lines_( at_same_line, S, Prefix_, Key) -->
    !,
    print_lines( S, Prefix_, Key).
print_lines_(begin(Severity, OtherKey), S, Prefix_es, Key) -->
    !,
    { prefix_( Severity, P ) },
    print_message_lines(S, [P], OtherKey),
    print_lines( S, Prefix_es, Key ).
print_lines_( flush, _S, _, Key) -->
	[ end(Key0)],
    { Key == Key0 },
    !.
print_lines_( end(Key0), S, _, Key) -->
    { Key0 == Key },
    !,
    { nl(S) }.
print_lines_( end(_OtherKey), S, Prefix_es, Key) -->
    !,
    print_lines( S, Prefix_es, Key ).
print_lines_(flush, S, Prefix_es, Key) -->
    !,
    { flush_output(S) },
    print_lines( S, Prefix_es, Key ).
print_lines_(format(Fmt,Args), S, Prefix_es, Key) -->
    !,
    { format(S, Fmt, Args) },
    print_lines( S, Prefix_es, Key ).
print_lines_( nl, S, _, Key) -->
    [ end(Key0)],
    { Key == Key0 },
    !,
    { nl(S)}.
print_lines_(nl, S, Prefix_es, Key) -->
    !,
    { nl(S),
      Prefix_es = [Prefix_S - Cmds|More],
      format(S, Prefix_S, Cmds)
    },
    {
	More == []
	->
	    NPrefix_es = Prefix_es
	;
	NPrefix_es = More
    },
    print_lines( S, NPrefix_es, Key).
% consider this a message within the message
print_lines_(prefix_(Fmt-Args), S, Prefix_es, Key) -->
    !,
    print_lines( S, [Fmt-Args|Prefix_es], Key ).
print_lines_(prefix_(Fmt), S, Prefix_es, Key) -->
    { atom( Fmt ) ; string( Fmt ) },
    !,
    print_lines( S, [Fmt-[]|Prefix_es], Key ).
print_lines_(Fmt-Args, S, Prefix_es, Key) -->
    !,
    { format(S, Fmt, Args) },
    print_lines( S, Prefix_es, Key ).
% deprecated....
print_lines_(Fmt, S, Prefix_es, Key) -->
    { atom(Fmt) ; string( Fmt ) },
    !,
    { format(S, Fmt, []) },
    print_lines(S, Prefix_es, Key).
print_lines_(Msg, S, _Prefix_es, _Key) -->
    { format(S, 'Illegal message Component: ~q !!!.~n', [Msg]) }.

prefix_(help,	      '~N'-[]).
prefix_(query,	      '~N'-[]).
prefix_(debug,	      '~N'-[]).
prefix_(warning,	      '~N'-[]).
prefix_(error,	      '~N'-[]).
prefix_(banner,	      '~N'-[]).
prefix_(informational, '~N~*|% '-[LC]) :-
    '$show_consult_level'(LC),
    LC > 0,
    !.
prefix_(informational,	      '~N'-[]).
prefix_(debug(_),      '~N'-[]).

/*	{ thread_self(Id) },
  (   { Id == main }
  ->  [ 'warning, ' - [] ]
  ;   { atom(Id) }
  ->  ['warning [Thread ~a ], ' - [Id] ]
  ;   ['warning [Thread ~d ], ' - [Id] ]
  ).
*/

/*
  { thread_self(Id) },
  (   { Id == main }
  ->  [ 'error ' ]
        ;   { thread_main_name(Id) }
  ->  [ 'error [ Thread ~w ] ' - [Id] ]
  ),
  !.
prefix_(error,	      '',   user_error) -->
  { thread_self(Id) },
  (   { Id == main }
  ->  [ 'error ' - [], nl ]
  ;   { atom(Id) }
  ->  [ 'error [ Thread ~a ] ' - [Id], nl ]
  ;   [ 'error [ Thread ~d ] ' - [Id], nl ]
  ).
*/



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


/** @pred  print_message_lines(+ _Stream_, + _Prefix__, + _Lines_)

Quintus/SICStus/SWI compatibility predicate to print message lines
  using  a prefix.


Print a message (see print_message/2) that has been translated to
a list of message elements.  The elements of this list are:

+ _Format_-_Args_
Where  _Format_ is an atom and  _Args_ is a list
of format argument.  Handed to `format/3`.
+ `flush`
If this appears as the last element,  _stream_ is flushed
(see `flush_output/1`) and no final newline is generated.
+ `at_same_line`
If this appears as first element, no prefix is printed for
the  line and the line-position is not forced to 0
(see `format/1`, `~N`).
+ `prefix`(_Prefix_)
define a prefix_ for the next line, say `''` will be seen as an
empty prefix.
(see `format/1`, `~N`)+ `<Format>`
Handed to `format/3` as `format(Stream, Format, [])`, may get confused
with other commands.
+ nl
A new line is started and if the message is not complete
the  _Prefix_ is printed too.
*/
print_message_lines(S, Prefix_0, Lines) :-
    Lines = [begin(_, Key)|Msg],
    (
	atom(Prefix_0)
    ->
    Prefix_ = Prefix_0-[]
    ;
    string(Prefix_0)
    ->
    Prefix_ = Prefix_0-[]
    ;
    Prefix_ = Prefix_0
    ),
    (Msg = [at_same_line|Msg1]
    ->
	print_lines(S, [Prefix_], Key, Msg1, [])
    ;
    print_lines(S, [Prefix_], Key, [Prefix_|Msg], [])
    ).



/** 
@pred print_message(+ _Kind_, +_Term_)

The predicate print_message/2 is used to print messages, notably from
exceptions, in a human-readable format.  _Kind_ is one of
`informational`, `banner`, `warning`, `error`, `help` or `silent`. In YAP, the message is always outut to the stream user_error.

If the Prolog flag verbose is `silent`, messages with
 _Kind_ `informational`, or `banner` are treated as
  silent.  See `-q` in [Running_YAP_Interactively].

4This predicate first translates the  _Term_ into a list of `message
lines` (see print_message_lines/3 for details).  Next it will
call the hook message_hook/3 to allow the user intercepting the
message.  If message_hook/3 fails it will print the message unless
 _Kind_ is silent.

If you need to report errors from your own predicates, we advise you to

stick to the existing error terms if you can; but should you need to
invent new ones, you can define corresponding error messages by
asserting clauses for message/2. You will need to declare
the predicate as multifile/1.

Note: errors in the implementation of print_message/2 are very
confusing to YAP (who will process the error?). So we write this small
stub to ensure everything os ok

*/

:- set_prolog_flag( redefine_warnings, false ).


:- dynamic in/0.


error_descriptor( V, [] ) :-
    must_be_bound(V),
    fail.
error_descriptor( exception(Info), List ) :-
    !,
    '$read_exception'(Info,List).
error_descriptor( (Info), Info ).


query_exception(K0,[H|L],V) :-
    (atom(K0) -> K=K0 ;  atom_to_string(K, K0) ),
    !,
    '$in'(K0=V,[H|L]).
query_exception(M,K,V) :-
    '$query_exception'(M,K,V).



print_message_(Severity, Msg) :-
    (
	var(Severity)
    ->
    !,
    format(user_error, 'malformed message ~q: message level is unbound~n', [Msg])
    ;
     var(Msg)
    ->
    !,
    format(user_error, 'uninstantiated message', [])
    ;
    Severity = silent
    ),
    !.
print_message_(Level, _Msg) :-
    current_prolog_flag(compiling, true),
    current_prolog_flag(verbose_load, false),
    Level \= error,
    Level \= warning,
    !.
print_message_(Level, _Msg) :-
    current_prolog_flag(verbose, silent),
    Level \= error,
    Level \= warning,
    !.
print_message_(Severity, Msg) :-
    user:portray_message(Severity, Msg),
    !.
print_message_(_, _Msg) :-
    % first step at hook processing
    '$conditional_compilation_skip',
    !.
print_message_(force(_Severity), Msg) :- !,
    print(user_error,Msg).
% This predicate has more hooks than a pirate ship!
print_message_(Severity, Term) :-
    Lines = [begin(Severity, Id)| Lines0],
    Linesf = [ end(Id)],
    build_message( Term, Lines0, Linesf),
    ignore(    	user:message_hook(Term, Severity, Lines) ),
    prefix_( Severity, Prefix_ ),
    print_message_lines(user_error, Prefix_, Lines),
    !.
print_message_(_Severity, _Term) :-
    format(user_error,'failed to print ~w: ~q~n'  ,[ _Severity, _Term]).


build_message( Term, Lines0, Linesf) :- 
    translate_message( Term,Lines0, Linesf).




print_message(Severity, Msg) :-
       print_message_(Severity, Msg),
	fail.
print_message(_Severity, _Msg).


/**
  @} 
*/

