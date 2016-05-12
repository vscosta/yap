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
* File:		undefined.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Predicate Undefined for YAP				 *
*									 *
*************************************************************************/

/** @defgroup Undefined_Procedures Handling Undefined Procedures
@ingroup YAPControl
@{

A predicate in a module is said to be undefined if there are no clauses
defining the predicate, and if the predicate has not been declared to be
dynamic. What YAP does when trying to execute undefined predicates can
be specified in three different ways:


+ By setting an YAP flag, through the yap_flag/2 or
set_prolog_flag/2 built-ins. This solution generalizes the
ISO standard by allowing module-specific behavior.
+ By using the unknown/2 built-in (this deprecated solution is
compatible with previous releases of YAP).
+ By defining clauses for the hook predicate
`user:unknown_predicate_handler/3`. This solution is compatible
with SICStus Prolog.


*/

/**  @pred  user:unknown_predicate_handler(+ _Call_, + _M_, - _N_)

In YAP, the default action on undefined predicates is to output an
`error` message. Alternatives are to silently `fail`, or to print a
`warning` message and then fail.  This follows the ISO Prolog standard
where the default action is `error`.

The user:unknown_predicate_handler/3 hook was originally include in
SICStus Prolog. It allows redefining the answer for specifici
calls. As an example. after defining `undefined/1` by:

~~~~~{.prolog}
undefined(A) :- format('Undefined predicate: ~w~n',[A]), fail.
~~~~~
and executing the goal:

~~~~~{.prolog}
:- assert(user:unknown_predicate_handler(U,M,undefined(M:U)) )
~~~~~
a call to a predicate for which no clauses were defined will result in
the output of a message of the form:

~~~~~{.prolog}
Undefined predicate: user:xyz(A1,A2)
~~~~~
followed by the failure of that call.
*/
:- multifile user:unknown_predicate_handler/3.

'$handle_error'(error,Goal,Mod) :-
    functor(Goal,Name,Arity),
    'program_continuation'(PMod,PName,PAr),
    '$do_error'(existence_error(procedure,Name/Arity),
           context(Mod:Goal,PMod:PName/PAr)).
'$handle_error'(warning,Goal,Mod) :-
    functor(Goal,Name,Arity),
    'program_continuation'(PMod,PName,PAr),
    print_message(warning,error(existence_error(procedure,Name/Arity), context(Mod:Goal,PMod:PName/PAr))),
    fail.
'$handle_error'(fail,_Goal,_Mod) :-
    fail.

:- '$set_no_trace'('$handle_error'(_,_,_), prolog).

/**
 * @pred '$undefp_expand'(+ M0:G0, -MG)
 *
 * @param G0 input goal
 * @param M0 current module
 * @param G1 new goal
 *
 * @return succeeds on finding G1, otherwise fails.
 *
 * Tries:
 *   1 - `user:unknown_predicate_handler`
 *   2 - `goal_expansion`
 *   1 - `import` mechanism`
*/
'$undefp_search'(M0:G0, MG) :-
    '$pred_exists'(unknown_predicate_handler(_,_,_,_), user),
    '$yap_strip_module'(M0:G0,  EM0, GM0),
    user:unknown_predicate_handler(GM0,EM0,M1:G1),
    !,
    expand_goal(M1:G1, MG).
'$undefp_search'(MG, FMG) :-
    expand_goal(MG, FMG).


/** @pred  unknown(- _O_,+ _N_)

The unknown predicate, informs about what the user wants to be done
	when there are no clauses for a predicate. Using unknown/3 is
	strongly deprecated. We recommend setting the `unknown` prolog
	flag for generic behaviour, and calling the hook
	user:unknown_predicate_handler/3 to fine-tune specific cases
	undefined goals.

*/

unknown(P, NP) :-
    prolog_flag( unknown, P, NP ).

/**
@}
*/
