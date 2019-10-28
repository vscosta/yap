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
/**
 * @file protect.yap
 */

:- system_module( '$_protect', [], ['$protect'/0]).

/**
 *
 * @addtogroup ProtectCore Freeze System Configuration
 * @{
 * @ingroup YAPControl
 *
 * This protects current code from further changes
 *  and also makes it impossible for some predicates to be seen
 * in user-space.
 *
 * Algorithm:
 *  - fix system modules
 *  - fix system predicates
 *  - hide atoms with `$`
 */


prolog:'$protect' :-
    '$all_current_modules'(M),
    ( sub_atom(M,0,1,_, '$') ; M= prolog; M= system ),
    new_system_module( M ),
    fail.
prolog:'$protect' :-
   '$current_predicate'(Name,M,P,_),
    '$is_system_module'(M),
    functor(P,Name,Arity),
    '$new_system_predicate'(Name,Arity,M),
    atom_concat('$',_,Name),
%    '$hide_predicate'(P,M),
    fail.
prolog:'$protect' :-
   '$current_predicate'(Name,M,P,_),
    '$is_system_module'(M),
    functor(P,Name,_Arity),
    \+ '$visible'(Name), 
    '$set_private'(P,M).
prolog:'$protect' :-
    current_atom(Name),
	sub_atom(Name,0,1,_, '$'),
    \+ '$visible'(Name),
    hide_atom(Name),
    fail.

prolog:'$protect' :-
    recorded('$module','$module'(_F,_DonorM,_SourceF, _AllExports, _Line), R),erase(R), fail.
prolog:'$protect' :-
    recorded('$source_file','$source_file'( _F, _Age, _M), R),erase(R), fail.
prolog:'$protect' :-
    recorded('$lf_loaded','$lf_loaded'( _F, _M, _Reconsult, _UserFile, _OldF, _Line, _Opts), R),erase(R), fail. 

prolog:'$protect'.


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
'$visible'('$x_yap_flag' ).
'$visible'('$x_yap_flag' ).
'$visible'(X) :- \+ atomic_concat('$',_,X).

%% @}

