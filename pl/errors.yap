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
* Last rev:     $Date: 2008-07-22 23:34:50 $,$Author: vsc $						 *
* $Log: not supported by cvs2svn $
* Revision 1.89  2008/06/12 10:55:52  vsc
* fix syntax error messages
*
* Revision 1.88  2008/04/04 10:02:44  vsc
* implement thread_cancel using signals
* use duplicate_term instead of copy_term in throw: throw may lose
* reference to term.
*
* Revision 1.87  2008/03/17 12:08:28  vsc
* avoid silly message
*
* Revision 1.86  2008/02/23 01:32:31  vsc
* fix chr bootstrap.
*
* Revision 1.85  2008/02/22 15:08:37  vsc
* Big update to support more SICStus/SWI like message handling
* fix YAPSHAREDIR
* fix yap.tex (Bernd)
*
* Revision 1.84  2008/01/23 17:57:55  vsc
* valgrind it!
* enable atom garbage collection.
*
* Revision 1.83  2007/11/26 23:43:10  vsc
* fixes to support threads and assert correctly, even if inefficiently.
*
* Revision 1.82  2007/09/27 23:02:00  vsc
* encoding/1
*
* Revision 1.81  2007/09/27 15:25:34  vsc
* upgrade JPL
*
* Revision 1.80  2007/01/24 14:20:04  vsc
* Fix typos across code
* Change debugger to backtrack more alike byrd model
* Fix typo in debugger option f
*
* Revision 1.79  2006/12/13 16:10:26  vsc
* several debugger and CLP(BN) improvements.
*
* Revision 1.78  2006/05/22 16:12:01  tiagosoares
* MYDDAS: MYDDAS version boot message
*
* Revision 1.77  2006/04/10 19:24:52  vsc
* fix syntax error message handling
* improve redblack trees and use it to reimplement association lists and
* to have better implementation of several graph algorithms.
*
* Revision 1.76  2006/04/05 00:16:55  vsc
* Lots of fixes (check logfile for details
*
* Revision 1.75  2006/02/24 14:26:37  vsc
* fix informational_messages
*
* Revision 1.74  2006/01/26 19:20:00  vsc
* syntax error was giving the offset
*
* Revision 1.73  2006/01/20 04:35:28  vsc
*
* fix error message
*
* Revision 1.72  2005/11/23 13:24:00  vsc
* cleanups in OS interface predicates.
*
* Revision 1.71  2005/11/10 01:27:12  vsc
* fix debugger message for EOF input
* fix fix to setof
* fix profiler spewing out hidden atoms.
*
* Revision 1.70  2005/11/03 18:27:10  vsc
* fix quote
*
* Revision 1.69  2005/11/01 18:54:06  vsc
* small fixes
*
* Revision 1.68  2005/10/29 01:28:37  vsc
* make undefined more ISO compatible.
*
* Revision 1.67  2005/10/28 17:38:50  vsc
* sveral updates
*
* Revision 1.66  2005/10/18 17:04:43  vsc
* 5.1:
* - improvements to GC
*    2 generations
*    generic speedups
* - new scheme for attvars
*    - hProlog like interface also supported
* - SWI compatibility layer
*    - extra predicates
*    - global variables
*    - moved to Prolog module
* - CLP(R) by Leslie De Koninck, Tom Schrijvers, Cristian Holzbaur, Bart
* Demoen and Jan Wielemacker
* - load_files/2
*
* from 5.0.1
*
* - WIN32 missing include files (untested)
* - -L trouble (my thanks to Takeyuchi Shiramoto-san)!
* - debugging of backtrable user-C preds would core dump.
* - redeclaring a C-predicate as Prolog core dumps.
* - badly protected  YapInterface.h.
* - break/0 was failing at exit.
* - YAP_cut_fail and YAP_cut_succeed were different from manual.
* - tracing through data-bases could core dump.
* - cut could break on very large computations.
* - first pass at BigNum issues (reported by Roberto).
* - debugger could get go awol after fail port.
* - weird message on wrong debugger option.
*
* Revision 1.65  2005/05/25 21:43:33  vsc
* fix compiler bug in 1 << X, found by Nuno Fonseca.
* compiler internal errors get their own message.
*
* Revision 1.64  2005/05/25 18:18:02  vsc
* fix error handling
* configure should not allow max-memory and use-malloc at same time
* some extensions for jpl
*
* Revision 1.63  2005/04/20 20:06:26  vsc
* try to improve error handling and warnings from within consults.
*
* Revision 1.62  2005/04/07 17:55:05  ricroc
* Adding tabling support for mixed strategy evaluation (batched and local scheduling)
*   UPDATE: compilation flags -DTABLING_BATCHED_SCHEDULING and -DTABLING_LOCAL_SCHEDULING removed. To support tabling use -DTABLING in the Makefile or --enable-tabling in configure.
*   NEW: yap_flag(tabling_mode,MODE) changes the tabling execution mode of all tabled predicates to MODE (batched, local or default).
*   NEW: tabling_mode(PRED,MODE) changes the default tabling execution mode of predicate PRED to MODE (batched or local).
*
* Revision 1.61  2005/02/21 16:50:21  vsc
* amd64 fixes
* library fixes
*
* Revision 1.60  2005/01/28 23:14:41  vsc
* move to Yap-4.5.7
* Fix clause size
*
* Revision 1.59  2005/01/13 05:47:27  vsc
* lgamma broke arithmetic optimisation
* integer_y has type y
* pass original source to checker (and maybe even use option in parser)
* use warning mechanism for checker messages.
*
* Revision 1.58  2004/11/19 21:32:53  vsc
* change abort so that it won't be caught by handlers.
*
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


/** @defgroup YAPError Error Handling
@ingroup YAPControl

The error handler is called when there is an execution error or a
warning needs to be displayed. The handlers include a number of hooks
to allow user-control.

*/	

:- system_module( '$_errors', [message_to_string/2,
        print_message/2], ['$Error'/1,
        '$do_error'/2]).

:- use_system_module( '$messages', [file_location/2,
        generate_message/3,
        translate_message/3]).


'$do_error'(Type,Message) :-
%        format('~w~n', [Type]),
	'$current_stack'(local_sp(_,CP,Envs,CPs)),
%	'$stack_dump',
	throw(error(Type,[Message|local_sp(Message,CP,Envs,CPs)])).

'$do_pi_error'(type_error(callable,Name/0),Message) :- !,
	'$do_error'(type_error(callable,Name),Message).
'$do_pi_error'(Error,Message) :- !,
	'$do_error'(Error,Message).

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
	flush_output,
	fail.

'$process_error'('$abort', top) :- !,
	print_message(informational,abort(user)).
'$process_error'('$abort', _) :- !,
	throw('$abort').
'$process_error'(abort, top) :- !,
	print_message(informational,abort(user)).
'$process_error'(abort, _) :- !,
	throw(abort).
'$process_error'(error(thread_cancel(Id), G),top) :- !.
'$process_error'(error(thread_cancel(Id), G), _) :- !,
	throw(error(thread_cancel(Id), G)).
'$process_error'(error(permission_error(module,redefined,A),B), Level) :-
	Level \= top, !,
	throw(error(permission_error(module,redefined,A),B)).
'$process_error'(error(Msg, Where), _) :- !,
	'$set_fpu_exceptions'(true),
	print_message(error,error(Msg, Where)).
'$process_error'(Throw, _) :-
	print_message(error,error(unhandled_exception,Throw)).

/** @pred  message_to_string(+ _Term_, - _String_) 


Translates a message-term into a string object. Primarily intended for SWI-Prolog emulation.



 */
message_to_string(Event, Message) :-
	'$messages':generate_message(Event, Message, []).

/** @pred print_message(+ _Kind_,  _Term_) 

The predicate print_message/2 is used to print messages, notably from
exceptions in a human-readable format.  _Kind_ is one of
`informational`, `banner`, `warning`, `error`,
`help` or `silent`. A human-readable message is printed to
the stream user_error.

If the Prolog flag verbose is `silent`, messages with
 _Kind_ `informational`, or `banner` are treated as
silent.@c  See \cmdlineoption{-q}.

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

 
*/
print_message(_, _) :-
    '$nb_getval'('$if_skip_mode',skip,fail), 
    !.
print_message(force(_Severity), Msg) :- !,
	print(user_error,Msg).
print_message(error, error(Msg,Info)) :- var(Info), !,
	print_message(error, error(Msg, '')).
print_message(error, error(Msg,[Info|local_sp(P,CP,Envs,CPs)])) :- !,
	recorda(sp_info,local_sp(P,CP,Envs,CPs),R),
	print_message(error, error(Msg, Info)),
	erase(R).
print_message(Severity, Msg) :-
	nonvar(Severity), nonvar(Msg),
	user:portray_message(Severity, Msg), !.
% This predicate has more hooks than a pirate ship!
print_message(Severity, Term) :-
	% first step at hook processing
	'$messages':translate_message(Term, Lines, []),
	(   nonvar(Term),
	    user:message_hook(Term, Severity, Lines)
	->
           true
	;
	   '$print_system_message'(Term, Severity, Lines)
	), !.
print_message(silent, _) :-  !.
print_message(_, loading(A, F)) :- !,
	format(user_error,'  % ~a ~a~n',[A,F]).
print_message(_, loaded(A, F, _, Time, Space)) :- !,
	format(user_error,'  % ~a ~a ~d bytes in ~d msecs~n',[F,A,Space,Time]).
print_message(_, Term) :-
	format(user_error,'~q~n',[Term]).

%	print_system_message(+Term, +Level, +Lines)
%
%	Print the message if the user did not intercept the message.
%	The first is used for errors and warnings that can be related
%	to source-location.  Note that syntax errors have their own
%	source-location and should therefore not be handled this way.

'$print_system_message'(_, silent, _) :- !.
'$print_system_message'(_, informational, _) :-
	current_prolog_flag(verbose, silent), !.
'$print_system_message'(_, banner, _) :-
	current_prolog_flag(verbose, silent), !.
'$print_system_message'(Term, Level, Lines) :-
	( Level == error -> Term \= error(syntax_error(_), _) ; Level == warning ),
	'$messages':prefix(Level, LinePrefix, Stream, LinesF, Lines2),
	'$messages':file_location(Lines2, Lines), !,
	flush_output(user_output),
	flush_output(user_error),
	print_message_lines(Stream, LinePrefix, [nl|LinesF]).
'$print_system_message'(_Error, Level, Lines) :-
	flush_output(user_output),
	flush_output(user_error),
	'$messages':prefix(Level, LinePrefix, Stream, LinesF, Lines), !,
	print_message_lines(Stream, LinePrefix, LinesF).


