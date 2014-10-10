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

'$undefp'([M0|G0], Default) :-
    % make sure we do not loop on undefined predicates
    % for undefined_predicates.
    '$enter_undefp',
    (
	'$get_undefined_pred'(G0, M0, Goal, NM)
	->
	    '$exit_undefp',
	    Goal \= fail,
	    '$complete_goal'(M0, G0, Goal, NM, NG),
	    '$execute0'(NG, NM)
	;
	user:unknown_predicate_handler(G0,M0,NG)
	->
	    '$exit_undefp',
	    call(M0:NG)
	;
	'$exit_undefp',
	'$handle_error'(Default,G0,M0)
    ).



/** @pred  unknown(- _O_,+ _N_) 

The unknown predicate, informs about what the user wants to be done
	when there are no clauses for a certain predicate.

This predicate is strongly deprecated. Use prolog_flag for generic
behaviour, and user:unknown_predicate_handler/3 for flexible behaviour
on undefined goals.

*/

unknown(P, NP) :-
    prolog_flag( unknown, P, NP ).

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

'$handle_error'(0x0080,Goal,Mod) :-
    functor(Goal,Name,Arity),
    '$program_continuation'(PMod,PName,PAr),
    '$do_error'(existence_error(procedure,Name/Arity),context(Mod:Goal,PMod:PName/PAr)).
'$handle_error'(0x0040,Goal,Mod) :-
    functor(Goal,Name,Arity),
    '$program_continuation'(PMod,PName,PAr),
    print_message(warning,error(existence_error(procedure,Name/Arity), context(Mod:Goal,PMod:PName/PAr))),
    fail.
'$handle_error'(0x0020,_Goal,_Mod) :-
    fail.

'$complete_goal'(M, G, CurMod, CurG, NG) :-
	  (
	   '$is_metapredicate'(CurG,CurMod)
	  ->
	   '$meta_expansion'(G, M, CurMod, M, NG, [])
	  ;
	   NG = G
	  ).

/**
@}
*/
