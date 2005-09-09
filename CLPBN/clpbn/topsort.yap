
:- module(topsort, [topsort/2,
		    topsort/3,
		    reversed_topsort/3]).

:- use_module(library(ordsets),
	      [ord_subtract/3,
	       ord_insert/3]).

:- attribute index/1,count/1.

/* simple implementation of a topological sorting algorithm */
/* graph is as Node-[Parents] */

topsort([], []) :- !.
topsort(Graph0,Sorted) :-
	add_parentless(Graph0, Sorted, IncludedI, Graph1, SortedRest),
	sort(IncludedI, Included),
	delete_parents(Graph1, Included, NoParents),
	topsort(NoParents, SortedRest).

topsort([], Sorted0, Sorted0) :- !.
topsort(Graph0,Sorted0, Sorted) :-
	add_parentless(Graph0, Sorted, IncludedI, Graph1, SortedRest),
	sort(IncludedI, Included),
	delete_parents(Graph1, Included, NoParents),
	topsort(NoParents, Sorted0, SortedRest).

%
% add the first elements found by topsort to the end of the list, so we
% have: a-> [], b -> [], c->[a,b], d ->[b,c] gives [d,c,a,b|Sorted0]
%
reversed_topsort([], Sorted, Sorted) :- !.
reversed_topsort(Graph0, Sorted0, Sorted) :-
	add_parentless(Graph0, [], SortedRest, New, Graph1, Sorted0),
	delete_parents(Graph1, New, NoParents),
	reversed_topsort(NoParents, SortedRest, Sorted).

add_parentless([], New, Sorted, New, [], Sorted).
add_parentless([Node-Parents|Graph0], New, Sorted, Included, Graph1, SortedRest) :-
%	Parents = [], !,
	ord_subtract(Parents,New,[]), !,
	ord_insert(New, Node, NNew),
	add_parentless(Graph0, NNew, Sorted, Included, Graph1, [Node|SortedRest]).
add_parentless([Node|Graph0], New, Sorted, Included, [Node|Graph1], SortedRest) :-
	add_parentless(Graph0, New, Sorted, Included, Graph1, SortedRest).

delete_parents([], _, []).
delete_parents([Node-Parents|Graph1], Included, [Node-NewParents|NoParents]) :-
	ord_subtract(Parents, Included, NewParents),
	delete_parents(Graph1, Included, NoParents).


	
