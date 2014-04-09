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

:- system_module( '$_protect', [], ['$protect'/0]).

% This protects all code from further changes
% and also makes it impossible from some predicates to be seen
'$protect' :-
	current_atom(Name),
	atom_codes(Name,[0'$|_]),
%	'$hide_predicates'(Name),
	'$hide'(Name).
'$protect' :-
	'$hide_predicates'(bootstrap),
	'$hide'(bootstrap).
'$protect'.

'$hide_predicates'(Name) :-
	'$current_predicate_for_atom'(Name, prolog, Ar),
	functor(P, Name, Ar),
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
'$hide'('$push_input_context') :- !, fail.		
'$hide'('$pop_input_context') :- !, fail.		
'$hide'('$set_source_module') :- !, fail.		
'$hide'('$declare_module') :- !, fail.		
'$hide'('$store_clause') :- !, fail.		
'$hide'('$skip_list') :- !, fail.		
'$hide'('$win_insert_menu_item') :- !, fail.
'$hide'('$set_predicate_attribute') :- !, fail.
'$hide'('$parse_quasi_quotations') :- !, fail.
'$hide'('$quasi_quotation') :- !, fail.
'$hide'('$qq_open') :- !, fail.
'$hide'(Name) :- hide(Name), fail.

