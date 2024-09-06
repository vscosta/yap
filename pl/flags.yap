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
 * @file flags.yap
 *
     @addtogroup YAPFlags
    @ingroup Builtins
    @{
 */



:- system_module_( '$_flags', [create_prolog_flag/3,
        current_prolog_flag/2,
        no_source/0,
        prolog_flag/2,
        prolog_flag/3,
        set_prolog_flag/2,
        source/0,
        source_mode/2,
        yap_flag/2,
        yap_flag/3],
		  []).

/* yap_flag( ?Key, ? CurrentValue, ?NewValue)
 *
 * Atomically read and set a flag _Key_. It is useful to temporarily set a flag, eg:
 * ~~~~
 * main :-
 *      yap_flag(key,DefaultValue,TemporaryValue),
 *      code,
 *      yap_flag(key, _, DefaultValue),
 *  ~~~
 *
 * The predicate is very similar to current_prolog_flag/3. We suggest using yap_flag/3 only for yap specific flags.
 */
yap_flag(K,O,N) :-
    current_prolog_flag(K,O),
    yap_flag(K,N).


/** @pred prolog_flag(? _Flag_,- _OldValue_,+ _NewValue_)

    Obtain the value for a YAP Prolog flag and then set it to a new
    value. Equivalent to first calling current_prolog_flag/2 with the
    second argument  _OldValue_ unbound and then calling
    set_prolog_flag/2 with the third argument  _NewValue_.


*/
prolog_flag(K,O,N) :-
    current_prolog_flag(K,O),
    set_prolog_flag(K,N).

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
	'$swi_set_prolog_flag'(file_errors, error),
	unknown(_,error).
'$adjust_language'(iso) :-
	'$switch_log_upd'(1),
	style_check(all),
	set_prolog_flag(file_errors,error),
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


/** @pred create_prolog_flag(  +Flag, +Value, +Options)

Create a new YAP Prolog flag.  _Options_ include

   * `type(+_Type_)` with _Type_ one of `boolean`, `integer`, `float`, `atom`
and `term` (that is, any ground term)

   * `access(+_Access_)` with  _Access_ one of `read_only` or `read_write`

   * `keep(+_Keep_)`  protect existing flag.
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

/* @pred yap_flag( ?Key, ? Value)
 *
 * Deprecated! If _Value_ is bound, set the flag _Key_; if unbound unify _Value_ with it's value. Consider using prolog_flag/2 and
 * set_prolog_flag/2.
 *
 */
yap_flag(Flag,Val) :-
    ( nonvar(Val) -> set_prolog_flag(Flag,Val) ; current_prolog_flag(Flag,Val)).


/**
@}
*/
