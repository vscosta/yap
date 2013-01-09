
:- module(topsort,
		[topsort/2]).

:- use_module(library(dgraphs),
		[dgraph_new/1,
		 dgraph_add_edges/3,
		 dgraph_add_vertices/3,
		 dgraph_top_sort/2
		]).

/* simple implementation of a topological sorting algorithm */
/* graph is as Node-[Parents] */

topsort(Graph0, Sorted) :-
	mkedge_list(Graph0, EdgeList, []),
	mkvertices_list(Graph0, VList, []),
	dgraph_new(DGraph0),
	dgraph_add_vertices(DGraph0, VList, DGraph1),
	dgraph_add_edges(DGraph1, EdgeList,  DGraph2),
	dgraph_top_sort(DGraph2, Sorted).

mkvertices_list([]) --> [].
mkvertices_list([V-_|More]) --> [V],
	mkvertices_list(More).

mkedge_list([]) --> [].
mkedge_list([V-Parents|More]) -->
	add_edges(Parents, V),
	mkedge_list(More).

add_edges([], _V) --> [].
add_edges([P|Parents], V) --> [P-V],
	add_edges(Parents, V).

