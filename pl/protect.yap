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
	'$make_system_preds'(Name),
	atom_codes(Name,[36|_]),
	'$hide'(Name).
'$protect'.

'$make_system_preds'('$directive') :- !.
'$make_system_preds'('$meta_predicate') :- !.
'$make_system_preds'('$exec_directive') :- !.
'$make_system_preds'(goal_expansion) :- !.
'$make_system_preds'(term_expansion) :- !.
'$make_system_preds'(portray) :- !.
'$make_system_preds'(library_directory) :- !.
'$make_system_preds'(modules_with_attributes) :- !.
'$make_system_preds'(woken_att_do) :- !.
'$make_system_preds'(convert_att_var) :- !.
'$make_system_preds'(Name) :- 
%	'$format'("~NProtecting ~a",Name),
	'$pred_defined_for'(Name,Pred),
%	'$format'("~NProtecting ~q",Pred),	
	'$protect_system_pred'(Pred),
	fail.
'$make_system_preds'(_).

'$protect_system_pred'(Pred) :-
	'$flags'(Pred,OldFlags,OldFlags\/8'40000).
	

% hide all atoms who start by '$'
'$hide'('$VAR') :- !, fail.			/* not $VAR */
'$hide'('$stream') :- !, fail.			/* not $STREAM */
'$hide'('$stream_position') :- !, fail.		/* not stream position */
'$hide'(Name) :- hide(Name), fail.

