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


/** @defgroup YAPErrors Error Handling
@ingroup Builtins
@brief Top-level Prolog code to support error-handling.
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

Other types of terms result in a message.

*/

:- system_module_( '$_errors', [], [throw_error/2,
				    error_handler/2,
				   error_handler/0]).

:- use_system_module( '$messages', [file_location/2,
        generate_message/3,
        translate_message/4]).


/**
 * @pred throw_error( +Error, +Cause)
 *
 * Generate a system error _Error_, informing the possible cause _Cause_.
 *
 */



throw_error(_,error(Type,Goal)) :-
    !,
    throw(error(Type,Goal)).
throw_error(Type,Goal) :-
    throw(error(Type,Goal)).

throw_file_error(_Type,__Goal) :-
   current_prolog_flag(file_errors,fail),
      !,
      false.

throw_file_error(Type,Goal) :-
    throw(error(Type,Goal)).

% error_handler(+E                          rror,+ Level)
%
% process an error term.
%
error_handler(_, _) :-
    set_prolog_flag(compiling, false),
    flush_output(user_output),
    flush_output(user_error),
    fail.
error_handler(_Level, error(event(abort,_I),_C)) :-
    !,
    abort.
error_handler(Level,error(Spec,Info)) :-
    !,
    '$process_error'(error(Spec,Info), Level).
    
'$error_clean' :-
	flush_output,
	'$close_error'(_),
	fail.

'$process_error'(error(permission_error(module,redefined,A),B), Level) :-
        Level == error, !,
        throw(error(permission_error(module,redefined,A),B)).
'$process_error'(Error, Level) :-
	print_message(Level, Error),
	!,
%	'$close_error'(_),
        fail.
'$process_error'(error(Type,Info), Level) :-
    print_message(Level,error(unhandled_exception(Type),Info)),
fail.

%% @}
