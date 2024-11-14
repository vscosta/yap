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
* File:		utilities for messing around in YAP internals.		 *
* comments:	error messages for YAP					 *
*									 *
* Last rev:     $Date: 2008-03-24 23:48:47 $,$Author: vsc $						 *
*									 *
*									 *
*************************************************************************/

%% @file pl/hacks.yap
%% @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
%%  @date   Thu Oct 19 12:02:56 2017
%%
%% @brief Access and Manipulation of YAD's internals

:- module('$yap_hacks').

/**
  *
  * @defgroup Hacks Access to YAP internal data-structures
  * @ingroup Builtins
  * @{
  * The _hacks_ predicate collection predicaates
  * provides a 
  *limited introspection to the execution stack and of error representation.
 Most of this functionnality requires to first load the module `library(hacks)`
*/


/**
 * @pred context_variables(-VarAndNames)
 *
 * makes available a list with the variable names of the last interaction.
 *
 */
yap_hacks:context_variables(Vs) :-
    b_getval(name_variables, Vs).

/** @pred yap_query_exception(Key, Term, Val).
 *
 * Term describes an exception as a set of mappings: unify val with the value for key Key, or fil if the key is not in Tern,
 */
yap_hacks:exception_property(Q,E,V) :-
    '$messages':query_exception(Q,E,V).

/**
 * @pred yap_error_descriptor(+Term,-List).
 *
 * If _Term_ describes an exception, _List_ will be unfied with the
 * fiekds storing error information.
 *
 * _List_ shpi;d be unbound, as YAP does not fuarantee an irder for the resulting _List_.
 */
yap_hacks:yap_error_descriptor(Inf,Des) :-
   '$messages': error_descriptor(Inf,Des).


:- meta_predicate(prolog:ctrace(0)).

prolog:ctrace(G) :-
    gated_call(start_low_level_trace,
	       user:G,
	       _,
	       stop_low_level_trace).



%% @}

