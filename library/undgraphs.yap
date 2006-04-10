%   File   : dgraphs.yap
%   Author : Vitor Santos Costa
%   Updated: 2006
%   Purpose: Directed Graph Processing Utilities.

:- module( undgraphs,
	   [
	    undgraph_new/1,
	    undgraph_add_edge/4,
	    undgraph_add_edges/3,
	    undgraph_add_vertex/3,
	    undgraph_add_vertices/3,
	    undgraph_del_edge/4,
	    undgraph_del_edges/3,
	    undgraph_del_vertex/3,
	    undgraph_del_vertices/3,
	    undgraph_edges/2,
	    undgraph_vertices/2,
	    undgraph_neighbors/2,
	    undgraph_neighbours/2,
	    undgraph_complement/2]).

:- use_module( library(dgraphs),
	   [
	    dgraph_new/1,
	    dgraph_add_edge/4,
	    dgraph_add_edges/3,
	    dgraph_add_vertex/3,
	    dgraph_add_vertices/3,
	    dgraph_del_edge/4,
	    dgraph_del_edges/3,
	    dgraph_del_vertex/3,
	    dgraph_del_vertices/3,
	    dgraph_edges/2,
	    dgraph_vertices/2,
	    dgraph_neighbors/2,
	    dgraph_neighbours/2,
	    dgraph_complement/2]).

:- use_module(library(ordsets),
	[ ord_del_element/3,
	  ord_union/3,
	  ord_subtract/3]).

:- use_module(library(rbtrees),
	[ rb_delete/4]).

undgraph_new(Vertices) :-
	dgraph_new(Vertices).

undgraph_add_edge(V1,V2,Vs0,Vs2) :-
	dgraphs:dgraph_new_edge(V1,V2,Vs0,Vs1),
	dgraphs:dgraph_new_edge(V2,V1,Vs1,Vs2).
	
undgraph_add_edges(Edges) -->
	{ dup_edges(Edges, DupEdges) },
	dgraph_add_edges(Edges).

dup_edges([],[]).
dup_edges([E1-E2|Edges], [E1-E2,E2-E1|DupEdges]) :-
	dup_edges(Edges, DupEdges).

undgraph_add_vertices([]) --> [].
undgraph_add_vertices([V|Vs]) -->
	dgraph_add_vertex(V),
	undgraph_add_vertices(Vs).

undgraph_add_vertex(V) -->
	dgraph_add_vertex(V).

undgraph_edges(Vs,Edges) :-
	dgraph_edges(Vs,DupEdges),
	remove_dups(DupEdges,Edges).

remove_dups([],[]).
remove_dups([V1-V2|DupEdges],NEdges) :- V1 @< V2, !,
	NEdges = [V1-V2|Edges],
	remove_dups(DupEdges,Edges).
remove_dups([_|DupEdges],Edges) :-
	remove_dups(DupEdges,Edges).

undgraph_vertices(Vs,Vertices) :-
	dgraph_vertices(Vs,Vertices).

undgraph_neighbours(V,Vertices,Children) :-
	dgraph_neighbours(V,Vertices,Children0),
	ord_del_element(V,Children0,Children).
undgraph_neighbors(V,Vertices,Children) :-
	dgraph_neighbors(V,Vertices,Children0),
	ord_del_element(V,Children0,Children).

undgraph_complement(Vs0,VsF) :-
	dgraph_complement(Vs0,VsF).

undgraph_del_edge(V1,V2,Vs0,VsF) :-
	dgraph_del_edge(V1,V2,Vs0,Vs1),
	dgraph_del_edge(V2,V1,Vs1,VsF).

undgraph_del_edges(Edges) -->
	{
	  dup_edges(Edges,DupEdges)
	},
	dgraph_del_edges(DupEdges).

undgraph_del_vertex(V, Vs0, Vsf) :-
	rb_delete(Vs0, V, BackEdges, Vsi),
	ord_del_element(BackEdges,V,RealBackEdges),
	rb_partial_map(Vsi, RealBackEdges, del_edge(V), Vsf).

undgraph_del_vertices(Vs) -->
	{ sort(Vs,SortedVs) },
	delete_all(SortedVs, [], BackEdges),
	{ ord_subtract(BackEdges, SortedVs, TrueBackEdges) },
	delete_remaining_edges(Vs, TrueBackEdges, SortedVs).

% it would be nice to be able to delete a set of elements from an RB tree
% but I don't how to do it yet.
delete_all([], BackEdges, BackEdges) --> [].
delete_all([V|Vs], BackEdges0, BackEdgesF, Vs0,Vsf) :-
	rb_delete(Vs0, V, NewEdges, Vsi),
	ord_union(NewEdges,BackEdges0,BackEdgesI),
	delete_all(Vs, BackEdgesI ,BackEdgesF, Vsi,Vsf).

delete_remaining_edges(SortedVs, TrueBackEdges, Vs0,Vsf) :-
	rb_partial_map(Vs0, TrueBackEdges, del_edges(SortedVs), Vsf).

del_edges(ToRemove,E0,E) :-
	ord_subtract(E0,ToRemove,E).

del_edges(ToRemove,E0,E) :-
	ord_del_element(E0,ToRemove,E).


