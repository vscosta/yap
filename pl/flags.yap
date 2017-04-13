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
* File:		flags.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	controlling YAP						 *
*									 *
*************************************************************************/
/**
 * @file flagd.ysp
 *
 * @defgroup Flags Yap Flags
 *n@{}
 * @ingroup builtins
 * @}@[                                                                                                                                                                                 ]
 */



:- system_module( '$_flags', [create_prolog_flag/3,
        current_prolog_flag/2,
        no_source/0,
        prolog_flag/2,
        prolog_flag/3,
        set_prolog_flag/2,
        source/0,
        source_mode/2,
        yap_flag/2,
        yap_flag/3], []).


'$adjust_language'(cprolog) :-
%	'$switch_log_upd'(0),
	'$syntax_check_mode'(_,off),
	'$syntax_check_single_var'(_,off),
	'$syntax_check_discontiguous'(_,off),
	'$syntax_check_multiple'(_,off),
	'$swi_set_prolog_flag'(character_escapes, false), % disable character escapes.
	'$set_yap_flags'(14,1),
	'$set_fpu_exceptions'(true),
	unknown(_,fail).
'$adjust_language'(sicstus) :-
	'$switch_log_upd'(1),
	leash(full),
	'$syntax_check_mode'(_,on),
	'$syntax_check_single_var'(_,on),
	'$syntax_check_discontiguous'(_,on),
	'$syntax_check_multiple'(_,on),
	'$transl_to_on_off'(X1,on),
	'$set_yap_flags'(5,X1),
	'$force_char_conversion',
	'$set_yap_flags'(14,0),
	% CHARACTER_ESCAPE
	'$swi_set_prolog_flag'(character_escapes, true), % disable character escapes.
	'$set_fpu_exceptions'(true),
	'$swi_set_prolog_flag'(fileerrors, true),
	unknown(_,error).
'$adjust_language'(iso) :-
	'$switch_log_upd'(1),
	style_check(all),
	fileerrors,
	'$transl_to_on_off'(X1,on),
	% CHAR_CONVERSION
	'$set_yap_flags'(5,X1),
	'$force_char_conversion',
	% ALLOW_ASSERTING_STATIC
	'$set_yap_flags'(14,0),
	% CHARACTER_ESCAPE
	'$swi_set_prolog_flag'(character_escapes, true), % disable character escapes.
	'$set_fpu_exceptions'(true),
	unknown(_,error).


/** @pred create_prolog_flag(+ _Flag_,+ _Value_,+ _Options_)

Create a new YAP Prolog flag.  _Options_ include

   * `type(+_Type_)` with _Type_ one of `boolean`, `integer`, `float`, `atom`
and `term` (that is, any ground term)

   * `access(+_Access_)` with  _Access_ one of `read_only` or `read_write`

   * `keeep(+_Keep_) protect existing flag.
*/
create_prolog_flag(Name, Value, Options) :-
	'$flag_domain_from_value'( Value, Type ),
	'$create_prolog_flag'(Name, Value, [type(Type)|Options]).

'$flag_domain_from_value'(true, boolean) :- !.
'$flag_domain_from_value'(false, boolean) :- !.
'$flag_domain_from_value'(Value, integer) :- integer(Value), !.
'$flag_domain_from_value'(Value, float) :- float(Value), !.
'$flag_domain_from_value'(Value, atom) :- atom(Value), !.
'$flag_domain_from_value'(_, term).

/**
@}
*/
