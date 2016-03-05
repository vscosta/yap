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
	'$current_predicate'(Name,prolog,P,_),
    M \= user,
    functor(P,Name,Arity),
    '$new_system_predicate'(Name,Arity,M),
    sub_atom(Name,0,1,_, '$'),
    functor(P,Name,Arity),
    '$hide_predicate'(P,M),
    fail.
'$protect' :-
    '$system_module'(M),
	'$current_predicate'(Name,M,P,_),
    M \= user,
    functor(P,Name,Arity),
    '$new_system_predicate'(Name,Arity,M),
    fail.
'$protect' :-
    current_atom(Name),
    sub_atom(Name,0,1,_, '$'),
    \+ '$visible'(Name),
    hide_atom(Name),
    fail.
'$protect'.


% hide all atoms who start by '$'
'$visible'('$').			/* not $VAR */
'$visible'('$VAR').			/* not $VAR */
'$visible'('$dbref').			/* not stream position */
'$visible'('$stream').			/* not $STREAM */
'$visible'('$stream_position').		/* not stream position */
'$visible'('$hacks').
'$visible'('$source_location').
'$visible'('$messages').
'$visible'('$push_input_context').
'$visible'('$pop_input_context').
'$visible'('$set_source_module').
'$visible'('$declare_module').
'$visible'('$store_clause').
'$visible'('$skip_list').
'$visible'('$win_insert_menu_item').
'$visible'('$set_predicate_attribute').
'$visible'('$parse_quasi_quotations').
'$visible'('$quasi_quotation').
'$visible'('$qq_open').
'$visible'('$live').
'$visible'('$init_prolog').
