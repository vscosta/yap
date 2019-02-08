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
Undefined predicate:
~~~~~
followed by the failure of that call.
*/
:- multifile user:unknown_predicate_handler/3.

undefined_query(G0, M0, Cut) :-
    recorded('$import','$import'(M,M0,G,G0,_,_),_),
    '$call'(G, Cut, G, M).


/**
 * @pred '$undefp_search'(+ M0:G0, -MG)
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
    user:unknown_predicate_handler(GM0,EM0,MG),
    !.
'$undefp_search'(M0:G0, MG) :-
    '$get_undefined_predicates'(M0:G0, MG), !.

% undef handler
'$undefp'([M0|G0],MG) :-
    % make sure we do not loop on undefined predicates
    '$undef_setup'(M0:G0, Action,Debug,Current, MGI),
    ('$get_undefined_predicates'( MGI, MG )                                  
    ->
        true
    ;
    '$undef_error'(Current, M0:G0, MGI,  MG)
    ),
     '$undef_cleanup'(Action,Debug,Current)
    .

'$undef_error'(_, M0:G0, _, MG) :-
    '$pred_exists'(unknown_predicate_handler(_,_,_,_), user),
    '$yap_strip_module'(M0:G0,  EM0, GM0),
    user:unknown_predicate_handler(GM0,EM0,MG),
    !.
'$undef_error'(error,  Mod:Goal, I,_) :-
    '$do_error'(existence_error(procedure,I), Mod:Goal).
'$undef_error'(warning,Mod:Goal,I,_) :-
    'program_continuation'(PMod,PName,PAr),
    print_message(warning,error(existence_error(procedure,I), context(Mod:Goal,PMod:PName/PAr))),
    fail.
'$undef_error'(fail,_Goal,_Mod) :-
    fail.

'$undef_setup'(G0,Action,Debug,Current,GI) :-
    yap_flag( unknown, Action, fail),
    yap_flag( debug, Debug, false),
    '$stop_creeping'(Current),
    '$g2i'(G0,GI).

'$g2i'(user:G, Na/Ar ) :-
        !,
	functor(G, Na, Ar).
'$g2i'(prolog:G, Na/Ar ) :-
        !,
	functor(G, Na, Ar).
'$g2i'(M:G, M:Na/Ar ) :-
        !,
	functor(G, Na, Ar).

'$undef_cleanup'(Action,Debug,_Current) :-
    yap_flag( unknown, _, Action),
    yap_flag( debug, _, Debug),
    '$start_creep'([prolog|true], creep).

:- '$undefp_handler'('$undefp'(_,_), prolog).

/** @pred  unknown(- _O_,+ _N_)

The unknown predicate, informs about what the user wants to be done
  when there are no clauses for a predicate. Using unknown/3 is
  strongly deprecated. We recommend setting the `unknown` prolog
  flag for generic behaviour, and calling the hook
  user:unknown_predicate_handler/3 to fine-tune specific cases
  undefined goals.

*/

unknown(P, NP) :-
    yap_flag( unknown, P, NP ).

/**
@}
*/
