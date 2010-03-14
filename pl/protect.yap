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
* File:		protect.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	protecting the system functions				 *
*									 *
*************************************************************************/

% This protects all code from further changes
% and also makes it impossible from some predicates to be seen
'$protect' :-
	current_atom(Name),
	atom_codes(Name,[0'$|_]),
	'$hide_predicates'(Name),
	'$hide'(Name).
'$protect' :-
	'$hide_predicates'(bootstrap),
	'$hide'(bootstrap).
'$protect'.

'$hide_predicates'(Name) :-
	'$current_predicate_for_atom'(Name, prolog, P),
	'$hide_predicate'(P,prolog),
	fail.
'$hide_predicates'(_).

% hide all atoms who start by '$'
'$hide'('$VAR') :- !, fail.			/* not $VAR */
'$hide'('$dbref') :- !, fail.			/* not stream position */
'$hide'('$stream') :- !, fail.			/* not $STREAM */
'$hide'('$stream_position') :- !, fail.		/* not stream position */
'$hide'('$hacks') :- !, fail.			
'$hide'('$source_location') :- !, fail.			
'$hide'('$messages') :- !, fail.		
'$hide'(Name) :- hide(Name), fail.

