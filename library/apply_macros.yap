
%   File   : apply_macros.yap
%   Author : E. Alphonse from code by Joachim Schimpf
%   Updated: 15 June 2002
%   Purpose: Macros to apply a predicate to all elements
%            of a list or to all sub-terms of a term.

:- module(apply_macros, []).

/** @defgroup apply_macros Apply Interface to maplist
@ingroup library
@{

This library provides a SWI-compatible set of utilities for applying a
predicate to all elements of a list.

The apply library just forwards
definitions to the @ref maplist library, these include:

  - maplist/2,
  - maplist/3,
  - maplist/4,
  - maplist/5,
  - include/3,
  - exclude/3,
  - partition/4,
  - partition/5


*/

:- reexport(maplist).

:- reexport(mapargs).

%% @}
