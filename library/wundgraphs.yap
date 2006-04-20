%   File   : dgraphs.yap
%   Author : Vitor Santos Costa
%   Updated: 2006
%   Purpose: Directed Graph Processing Utilities.

:- module( wundgraphs,
	   [
	    wundgraph_new/1,
	    wundgraph_add_edge/5,
	    wundgraph_add_edges/3,
	    wundgraph_add_vertex/3,
	    wundgraph_add_vertices/3,
	    wundgraph_del_edge/5,
	    wundgraph_del_edges/3,
	    wundgraph_del_vertex/3,
	    wundgraph_del_vertices/3,
	    wundgraph_edge/4,
	    wundgraph_edges/2,
	    wundgraph_vertices/2,
	    wundgraph_neighbors/3,
	    wundgraph_neighbours/3,
	    wdgraph_to_wundgraph/2,
	    wundgraph_to_wdgraph/2,
	    undgraph_to_wundgraph/2,
	    wundgraph_to_undgraph/2,
	    wundgraph_min_tree/3,
	    wundgraph_max_tree/3,
	    wundgraph_min_path/5,
	    wundgraph_min_paths/3,
	    wundgraph_max_path/5,
	    wundgraph_path/3]).

:- use_module( library(wdgraphs),
	   [
	    wdgraph_new/1,
	    wdgraph_add_edge/5,
	    wdgraph_add_edges/3,
	    wdgraph_add_vertex/3,
	    wdgraph_add_vertices/3,
	    wdgraph_del_edge/5,
	    wdgraph_del_edges/3,
	    wdgraph_del_vertex/3,
	    wdgraph_del_vertices/3,
	    wdgraph_edge/4,
	    wdgraph_edges/2,
	    wdgraph_to_dgraph/2,
	    dgraph_to_wdgraph/2,
	    wdgraph_symmetric_closure/2,
	    wdgraph_min_path/5,
	    wdgraph_min_paths/3,
	    wdgraph_max_path/5,
	    wdgraph_path/3]).

:- use_module( library(dgraphs),
	   [
            dgraph_vertices/2,
	    dgraph_neighbors/3
	]).

:- use_module(library(rbtrees),
	[  
         rb_new/1,
         rb_delete/4,
	 rb_partial_map/4,
	 rb_visit/2,
	 rb_insert/4,
	 rb_lookup/3
	]).

:- use_module(library(lists),
	[  
         reverse/2
	]).

wundgraph_new(Vertices) :-
	wdgraph_new(Vertices).

wundgraph_add_edge(V1,V2,K,Vs0,Vs2) :-
	wdgraphs:wdgraph_new_edge(V1,V2,K,Vs0,Vs1),
	wdgraphs:wdgraph_new_edge(V2,V1,K,Vs1,Vs2).
	
wundgraph_add_edges(Edges) -->
	{ dup_edges(Edges, DupEdges) },
	wdgraph_add_edges(DupEdges).

dup_edges([],[]).
dup_edges([E1-(E2-K)|Edges], [E1-(E2-K),E2-(E1-K)|DupEdges]) :-
	dup_edges(Edges, DupEdges).

wundgraph_add_vertices(Vs) -->
	wdgraph_add_vertices(Vs).

wundgraph_add_vertex(V) -->
	wdgraph_add_vertex(V).

wundgraph_edges(Vs,Edges) :-
	wdgraph_edges(Vs,DupEdges),
	remove_dups(DupEdges,Edges).

remove_dups([],[]).
remove_dups([V1-(V2-K)|DupEdges],NEdges) :- V1 @< V2, !,
	NEdges = [V1-(V2-K)|Edges],
	remove_dups(DupEdges,Edges).
remove_dups([_|DupEdges],Edges) :-
	remove_dups(DupEdges,Edges).

wundgraph_vertices(Vs,Vertices) :-
	dgraph_vertices(Vs,Vertices).

wundgraph_neighbours(V,Vertices,Children) :-
	dgraph_neighbours(V,Vertices,Children0),
	(
	    del_me(Children0,V,Children)
	->
	    true
	;
	    Children = Children0
	).
wundgraph_neighbors(V,Vertices,Children) :-
	dgraph_neighbors(V,Vertices,Children0),
	(
	    del_me(Children0,V,Children)
	->
	    true
	;
	    Children = Children0
	).

del_me([], _, []).
del_me([K-_|Children], K1, NewChildren) :-
	( K == K1 ->
	    Children = NewChildren
	;
	    K @< K1 ->
	    NewChildren = [K|ChildrenLeft],
	    del_me(Children, K1, ChildrenLeft)
	;
	    NewChildren = [K|MoreChildren],
	    compact(Children, MoreChildren)
	).

wundgraph_del_edge(V1,V2,K,Vs0,VsF) :-
	wdgraph_del_edge(V1,V2,K,Vs0,Vs1),
	wdgraph_del_edge(V2,V1,K,Vs1,VsF).

wundgraph_del_edges(Edges) -->
	{
	  dup_edges(Edges,DupEdges)
	},
	wdgraph_del_edges(DupEdges).

wundgraph_del_vertex(V, Vs0, Vsf) :-
	rb_delete(Vs0, V, BackEdges, Vsi),
	del_and_compact(BackEdges,V,BackVertices),
	rb_partial_map(Vsi, BackVertices, del_edge(V), Vsf).

del_and_compact([], _, []).
del_and_compact([K-_|Children], K1, NewChildren) :-
	( K == K1 ->
	    compact(Children, NewChildren)
	;
	    K @< K1 ->
	    NewChildren = [K|ChildrenLeft],
	    del_and_compact(Children, K1, ChildrenLeft)
	;
	    NewChildren = [K|CompactChildren],
	    compact(Children, CompactChildren)
	).

compact([], []).
compact([K-_|Children], [K|CompactChildren]) :-
	compact(Children, CompactChildren).


wundgraph_del_vertices(Vs) -->
	wdgraph_del_vertices(Vs).

del_edge(_, [], []).
del_edge(K1, [K-W|Children], NewChildren) :-
	( K == K1 ->
	    Children = NewChildren
	;
	    K @< K1 ->
	    NewChildren = [K-W|ChildrenLeft],
	    del_edge(K1, Children, ChildrenLeft)
	;
	    NewChildren = [K-W|Children]
	).

wundgraph_edge(N1, N2, K, G) :-
	wdgraph_edge(N1, N2, K, G).

wdgraph_to_wundgraph(G, U) :-
	wdgraph_symmetric_closure(G, U).

wundgraph_to_wdgraph(G, G).

wundgraph_min_path(V1, V2, WGraph, Path, Cost) :-
	wdgraph_min_path(V1, V2, WGraph, Path, Cost).

wundgraph_max_path(V1, V2, WGraph, Path, Cost) :-
	wdgraph_max_path(V1, V2, WGraph, Path, Cost).

wundgraph_min_paths(V1, WGraph, T) :-
	wdgraph_min_paths(V1, WGraph, T).

wundgraph_path(V, WG, P) :-
	wdgraph_path(V, WG, P).

undgraph_to_wundgraph(G1, G2) :-
	dgraph_to_wdgraph(G1, G2).

wundgraph_to_undgraph(G1, G2) :-
	    wdgraph_to_dgraph(G1, G2).


% simplistic algorithm to build a minimal spanning tree.
% Just sort edges and then walk over each one.

wundgraph_min_tree(G, T, C) :-
	rb_visit(G, Els0),
	mk_list_of_edges(Els0, Edges),
	keysort(Edges, SortedEdges),
	rb_new(V0),
	rb_new(T0),
	add_sorted_edges(SortedEdges, V0, TreeEdges, 0, C),
	wundgraph_add_edges(TreeEdges, T0, T).

wundgraph_max_tree(G, T, C) :-
	rb_visit(G, Els0),
	mk_list_of_edges(Els0, Edges),
	keysort(Edges, SortedEdges),
	reverse(SortedEdges, ReversedEdges),
	rb_new(V0),
	rb_new(T0),
	add_sorted_edges(ReversedEdges, V0, TreeEdges, 0, C),
	wundgraph_add_edges(TreeEdges, T0, T).

mk_list_of_edges([], []).
mk_list_of_edges([V-Els|Els0], Edges) :-
	add_neighbs(Els, V, Edges, Edges0),
	mk_list_of_edges(Els0, Edges0).

add_neighbs([], _, Edges, Edges).
add_neighbs([V-W|Els], V0, [W-(V0-V)|Edges], Edges0) :-
	V0 @< V, !,
	add_neighbs(Els, V0, Edges, Edges0).
add_neighbs([_|Els], V0, Edges, Edges0) :-
	add_neighbs(Els, V0, Edges, Edges0).

	
add_sorted_edges([], _, [], C, C).
add_sorted_edges([W-(V0-V)|SortedEdges], T0, NewTreeEdges, C0, C) :-
	( rb_lookup(V0, Component, T0) ->
	    ( rb_lookup(V, Component1, T0) ->
		( Component \== Component1 ->
		    /* edge that links two separate sub-trees (components) */
	          Component = Component1,
		  Ti = T0
	        ;
	        /* same component, can't add edge */
	          fail
	        )
	      ;
              /* V is new */
	      rb_insert(T0, V, Component, Ti)
	    )
	;
	    ( rb_lookup(V, Component1, T0) ->
              /* V0 is new */
	      rb_insert(T0, V0, Component1, Ti)
	    ;
              /* new edges, new tree */
	      rb_insert(T0, V0, NewComponent, T1),
	      rb_insert(T1, V, NewComponent, Ti)
	    )
        ),
	!,
	NewTreeEdges = [(V0-(V-W)),(V-(V0-W))|TreeEdges],
	Ci is C0+W,
	add_sorted_edges(SortedEdges, Ti, TreeEdges, Ci, C).
add_sorted_edges([_|SortedEdges],  T0, NewTreeEdges, C0, C) :-
	add_sorted_edges(SortedEdges, T0, NewTreeEdges, C0, C).

