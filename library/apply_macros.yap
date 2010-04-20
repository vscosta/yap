%   File   : apply_macros.yap
%   Author : E. Alphonse from code by Joachim Schimpf
%   Updated: 15 June 2002
%   Purpose: Macros to apply a predicate to all elements
%            of a list or to all sub-terms of a term.

:- module(apply_macros, []).

:- reexport(maplist, [selectlist/3,
			 checklist/2,
			 maplist/2,
			 maplist/3,
			 maplist/4,
			 maplist/5,
			 convlist/3,
			 mapargs/3,
			 sumargs/4,
			 mapnodes/3,
			 checknodes/2,
			 sumlist/4,
			 sumnodes/4,
			 include/3,
			 exclude/3,
			 partition/4,
			 partition/5			
			]).


