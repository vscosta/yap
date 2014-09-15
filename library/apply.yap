/** @defgroup Apply Apply Macros
@ingroup YAPLibrary
@{

This library provides a SWI-compatible set of utilities for applying a
predicate to all elements of a list. In practice, the library just forwards
definitions from the @ref maplist library library.

 */

:- module(apply,[]).

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
@}
*/
