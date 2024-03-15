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

:- system_module('$yap_hacks',
		 [],
		 [ctrace/1,
		  fully_strip_module/3
	 ]).

/**
  *
  * @defgroup Hacks Access to YAP internal data-structures
  * @ingroup Builtins
  * @{
  * The _hacks_ predicate collection predicaates
  * provides a 
  limoted introspection t the eecution stack and of error representation. Most of this functionnality requires to first load the module `library(hacks)`
*/


/**
 * @pred ctrace(Goal)
 *
 * This predicate is only available if the YAP
 * compile option was set. It generates a
 * step-by-step trace of the execution of _Goal_
 *
 */
	
:- meta_predicate(ctrace(0)).

ctrace(G) :-
    gated_call(start_low_level_trace,
	       G,
	       _,
	       stop_low_level_trace).

/**
 * @pred context_variables(+VarAndNames)
 *
 * makes available a list with the variable names of the last interaction.
 *
 */
yap_hacks:context_variables(Vs) :-
    b_getval(name_variables, Vs).

%% @}

