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
*									 *
*									 *
*************************************************************************/

/**
 @file: pl/errors.yap

 @brief YAP error handler
*/


/** @defgroup YAPErrorHandler Error Handling

@ingroup YAPErrors
@{

The error handler is called when there is an execution error or a
warning needs to be displayed. The handlers include a number of hooks
to allow user-control.

Errors are terms of the form:

   - error( domain_error( Domain, Culprit )`
   - error( evaluation_error( Expression, Culprit )`
   - error( existence_error( Object, Culprit )`
   - error( instantiation_error )`
   - error( permission_error( Error, Permission, Culprit)`
   - error( representation_error( Domain, Culprit )`
   - error( resource_error( Resource, Culprit )`
   - error( syntax_error( Error )`
   - error( system_error( Domain, Culprit )`
   - error( type_error( Type, Culprit )`
   - error( uninstantiation_error(  Culprit )`

*/

:- system_module( '$_errors', [system_error/2], ['$Error'/1,
                                                 '$do_error'/2,
                                                 system_error/3,
                                                 system_error/2]).

:- use_system_module( '$messages', [file_location/2,
        generate_message/3,
        translate_message/4]).


/**
 * @pred system_error( +Error, +Cause)
 *
 * Generate a system error _Error_, informing the possible cause _Cause_.
 *
 */
prolog:system_error(Type,Goal) :-
    '$do_error'(Type,Goal).


'$do_error'(Type,Goal) :-
	throw(error(Type, print_message(['while calling goal = ~w'-Goal,nl]))).

/**
 * @pred system_error( +Error, +Cause, +Culprit)
 *
 * Generate a system error _Error_, informing the source goal _Cause_
 *
 *
 * ~~~~~~~~~~
 * ~~~~~~~~~~
 *
 *
 */
system_error(Type,Goal) :-
  throw(error(Type, print_message(['while calling goal = ~w'-Goal,nl]))) .

'$do_pi_error'(type_error(callable,Name/0),Message) :- !,
	'$do_error'(type_error(callable,Name),Message).
'$do_pi_error'(Error,Message) :- !,
	'$do_error'(Error,Message).

'$Error'(E) :-
	'$LoopError'(E, top).
%%
% error_handler(+Error,+ Level)
%
% process an error term.
%
error_handler(Error, Level) :-
    '$LoopError'(Error, Level).

'$LoopError'(_, _) :-
    flush_output(user_output),
	flush_output(user_error),
	fail.
'$LoopError'(Error, Level) :- !,
	'$process_error'(Error, Level),
	fail.
'$LoopError'(_, _) :-
	flush_output,
	'$close_error',
	fail.

'$process_error'('$forward'(Msg),  _) :-
	!,
	throw( '$forward'(Msg) ).
'$process_error'(error(event(abort,I),C), Level) :-
	!,
	(
  current_prolog_flag(break_level, 0),
	 Level \== top
	->
    print_message(informational,abort(user)),
 	 fail
 	;
	 throw( error(event(abort,I),C) )
	).
'$process_error'(error(permission_error(module,redefined,A),B), Level) :-
        Level \= top, !,
        throw(error(permission_error(module,redefined,A),B)).
'$process_error'(Error, _Level) :-
	functor(Error, Severity, _),
	print_message(Severity, Error),
	!,
	'$close_error'.
'$process_error'(error(Type,Info), _, _) :-
	print_message(error,error(unhandled_exception(Type),Info)).

%% @}
