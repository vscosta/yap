/**
 * @file   apply.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Mon Nov 16 23:00:08 2015
 * 
 * @brief  Stub for maplist and friends
 * 
*/

:- module(apply_stub,[]).


:- reexport(library(maplist),
	    [maplist/2,
	     maplist/3,
	     maplist/4,
	     maplist/5,
	     include/3,
	     exclude/3,
	     partition/4,
	     partition/5
	    ]).

/**

@defgroup ApplyMaplist Appy Stub for maplist Predicates
@ingroup maplist

@{

This library provides a SWI-compatible set of utilities for applying a
predicate to all elements of a list.

The apply library is a _stub_, it just forwards definitions to the
@ref maplist library. The predicates forwarded are:

*  maplist/2,
*  maplist/3,
*  maplist/4,
*  maplist/5,
*  include/3,
*  exclude/3,
*  partition/4,
*  partition/5

 */


%% @}

