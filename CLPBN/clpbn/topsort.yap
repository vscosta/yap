
:- module(topsort, [topsort/2,
		    topsort/3,
	reversed_topsort/2]).

:- use_module(library(rbtrees),
	      [rb_new/1,
	       rb_lookup/3,
	       rb_insert/4]).

:- use_module(library(lists),
	      [reverse/2]).

/* simple implementation of a topological sorting algorithm */
/* graph is as Node-[Parents] */

topsort(Graph0, Sorted) :-
	rb_new(RB),
	topsort(Graph0, [], RB, Sorted).

topsort(Graph0, Sorted0, Sorted) :-
        rb_new(RB),
	topsort(Graph0, Sorted0, RB, Sorted).

%
% Have children first in the list
%
reversed_topsort(Graph0, RSorted) :-
	rb_new(RB),
	topsort(Graph0, [], RB, Sorted),
	reverse(Sorted, RSorted).

topsort([], Sort, _, Sort) :- !.
topsort(Graph0, Sort0, Found0, Sort) :-
	add_nodes(Graph0, Found0, SortI, NewGraph, Found, Sort),
	topsort(NewGraph, Sort0, Found, SortI).

add_nodes([], Found, Sort, [], Found, Sort).
add_nodes([N-Ns|Graph0], Found0, SortI, NewGraph, Found, NSort) :-
(N=1600 -> write(Ns), nl ; true),
	delete_nodes(Ns, Found0, NNs),
	( NNs == [] ->
	   NewGraph = IGraph,
	   NSort = [N|Sort],
	   rb_insert(Found0, N, '$', FoundI)
	;
	   NewGraph = [N-NNs|IGraph],
	   NSort = Sort,
	   FoundI = Found0
	),	   
	add_nodes(Graph0, FoundI, SortI, IGraph, Found, Sort).

delete_nodes([], _, []).
delete_nodes([N|Ns], Found, NNs) :-
	rb_lookup(N,'$',Found), !,
	delete_nodes(Ns, Found, NNs).
delete_nodes([N|Ns], Found, [N|NNs]) :-
	delete_nodes(Ns, Found, NNs).


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


	
