
:- module(topsort, [topsort/2]).

:- use_module(library(ordsets),
	      [ord_subtract/3]).

/* simple implementation of a topological sorting algorithm */
/* graph is as Node-[Parents] */

topsort([], []) :- !.
topsort(Graph0,Sorted) :-
	add_parentless(Graph0, Sorted, IncludedI, Graph1, SortedRest),
	sort(IncludedI, Included),
	delete_parents(Graph1, Included, NoParents),
	topsort(NoParents, SortedRest).

add_parentless([], Sorted, [], [], Sorted).
add_parentless([Node-[]|Graph0], [Node|Sorted], [Node|Included], Graph1, SortedRest) :- !,
	add_parentless(Graph0, Sorted, Included, Graph1, SortedRest).
add_parentless([Node|Graph0], Sorted, Included, [Node|Graph1], SortedRest) :-
	add_parentless(Graph0, Sorted, Included, Graph1, SortedRest).

delete_parents([], _, []).
delete_parents([Node-Parents|Graph1], Included, [Node-NewParents|NoParents]) :-
	ord_subtract(Parents, Included, NewParents),
	delete_parents(Graph1, Included, NoParents).

