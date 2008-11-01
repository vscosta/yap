
:- module(clpbn_connected,
	[clpbn_subgraphs/2,
	influences/4,
	init_influences/3,
	influences/5]).

:- use_module(library(dgraphs),
	[dgraph_new/1,
	dgraph_add_edges/3,
	dgraph_add_vertex/3,
	dgraph_neighbors/3,
	dgraph_edge/3]).

:- use_module(library(rbtrees),
	[rb_new/1,
	rb_insert/4,
	rb_lookup/3]).

:- attribute component/1.

% search for connected components, that is, where we know that A influences B or B influences A.
clpbn_subgraphs(Vs, Gs) :-
	mark_components(Vs, Components),
	keysort(Components, Ordered),
	same_key(Ordered, Gs).

% ignore variables with evidence,
% the others mark the MB.
mark_components([], []).
mark_components([V|Vs], Components) :-
	clpbn:get_atts(V, [evidence(_),dist(_,Parents)]), !,
	merge_parents(Parents, _),
	mark_components(Vs, Components).
mark_components([V|Vs], [Mark-V|Components]) :-
	mark_var(V, Mark),
	mark_components(Vs, Components).

mark_var(V, Mark) :-
	get_atts(V, [component(Mark)]), !,
	clpbn:get_atts(V, [dist(_,Parents)]), !,
	merge_parents(Parents, Mark).
mark_var(V, Mark) :-
	clpbn:get_atts(V, [dist(_,Parents)]), !,
	put_atts(V,[component(Mark)]),
	merge_parents(Parents, Mark).

merge_parents([], _).
merge_parents([V|Parents], Mark) :-
	clpbn:get_atts(V,[evidence(_)]), !,
	merge_parents(Parents, Mark).
merge_parents([V|Parents], Mark) :-
	get_atts(V,[component(Mark)]), !,
	merge_parents(Parents, Mark).
merge_parents([V|Parents], Mark) :-
	put_atts(V,[component(Mark)]),
	merge_parents(Parents, Mark).

	
same_key([],[]).
same_key([K-El|More],[[El|Els]|Gs]) :-
	same_keys(More, K, Els, Rest),
	same_key(Rest,Gs).

same_keys([], _, [], []).
same_keys([K1-El|More], K, [El|Els], Rest) :-
	K == K1, !,
	same_keys(More, K, Els, Rest).
same_keys(Rest, _, [], Rest).

influences_more([], _, _, Is, Is, Evs, Evs, V2, V2).
influences_more([V|LV], G, RG, Is0, Is, Evs0, Evs, GV0, GV2) :-
	rb_lookup(V, _, GV0), !,
	influences_more(LV, G, RG, Is0, Is, Evs0, Evs, GV0, GV2).
influences_more([V|LV], G, RG, Is0, Is, Evs0, Evs, GV0, GV3) :-
	rb_insert(GV0, V, _, GV1),
	follow_dgraph(V, G, RG, [V|Is0], Is1, [V|Evs0], Evs1, GV1, GV2),
	influences_more(LV, G, RG, Is1, Is, Evs1, Evs, GV2, GV3).

% search for the set of variables that influence V
influences(Vs, LV, Is, Evs) :-
	init_influences(Vs, G, RG),
	influences(LV, Is, Evs, G, RG).

init_influences(Vs, G, RG) :-
	dgraph_new(G0),
	dgraph_new(RG0),
	to_dgraph(Vs, G0, G, RG0, RG).

influences([], [], [], _, _).
influences([V|LV], Is, Evs, G, RG) :-
	rb_new(V0),
	rb_insert(V0, V, _, V1),
	follow_dgraph(V, G, RG, [V], Is1, [V], Evs1, V1, V2),
	influences_more(LV, G, RG, Is1, Is, Evs1, Evs, V2, _).

to_dgraph([], G, G, RG, RG).
to_dgraph([V|Vs], G0, G, RG0, RG) :-
	clpbn:get_atts(V, [evidence(_),dist(_,Parents)]), !,
	build_edges(Parents, V, Edges, REdges),
	dgraph_add_edges(G0,[V-e|Edges],G1),
	dgraph_add_edges(RG0,REdges,RG1),
	to_dgraph(Vs, G1, G, RG1, RG).
to_dgraph([V|Vs], G0, G, RG0, RG) :-
	clpbn:get_atts(V, [dist(_,Parents)]),
	build_edges(Parents, V, Edges, REdges),
	dgraph_add_vertex(G0,V,G1),
	dgraph_add_edges(G1, Edges, G2),
	dgraph_add_vertex(RG0,V,RG1),
	dgraph_add_edges(RG1, REdges, RG2),
	to_dgraph(Vs, G2, G, RG2, RG).


build_edges([], _, [], []).
build_edges([P|Parents], V, [P-V|Edges], [V-P|REdges]) :-
	build_edges(Parents, V, Edges, REdges).

follow_dgraph(V, G, RG, Is0, IsF, Evs0, EvsF, Visited0, Visited) :-
	dgraph_neighbors(V, RG, Parents),
	add_parents(Parents, G, RG, Is0, IsI, Evs0, EvsI, Visited0, Visited1),
	dgraph_neighbors(V, G, Kids),
	add_kids(Kids, G, RG, IsI, IsF, EvsI, EvsF, Visited1, Visited).

add_parents([], _, _, Is, Is, Evs, Evs, Visited, Visited).
% been here already, can safely ignore.
add_parents([V|Vs], G, RG, Is0, IsF, Evs0, EvsF, Visited0, VisitedF) :-
	rb_lookup(V, _, Visited0), !,
	add_parents(Vs, G, RG, Is0, IsF, Evs0, EvsF, Visited0, VisitedF).
% evidence node,
% just say that we visited it
add_parents([V|Vs], G, RG, Is0, IsF, Evs0, EvsF, Visited0, VisitedF) :-
	dgraph_edge(V,e,G), !, % has evidence
	rb_insert(Visited0, V, _, VisitedI),
	add_parents(Vs, G, RG, Is0, IsF, [V|Evs0], EvsF, VisitedI, VisitedF).
% non-evidence node,
% we will need to find its parents.
add_parents([V|Vs], G, RG, Is0, IsF, Evs0, EvsF, Visited0, VisitedF) :-
	rb_insert(Visited0, V, _, VisitedI),
	follow_dgraph(V, G, RG, [V|Is0], IsI, [V|Evs0], EvsI, VisitedI, VisitedII),
	add_parents(Vs, G, RG, IsI, IsF, EvsI, EvsF, VisitedII, VisitedF).
	
add_kids([], _, _, Is, Is, Evs, Evs, Visited, Visited).
add_kids([V|Vs], G, RG, Is0, IsF, Evs0, EvsF, Visited0, VisitedF) :-
	dgraph_edge(V,e,G), % has evidence
	% we will go there even if it was visited
	( rb_insert(Visited0, V, _, Visited1) ->
	    true
	;
	    % we've been there, but were we there as a father or as a kid?
	    not_in(Evs0, V),
	    Visited1 = Visited0
	),
	!,
	dgraph_neighbors(V, RG, Parents),	
	add_parents(Parents, G, RG, Is0, Is1, [V|Evs0], EvsI, Visited1, VisitedI),	
	(Is1 = Is0 ->
	    % ignore whatever we did with this node,
	    % it didn't lead anywhere (all parents have evidence).
	    add_kids(Vs, G, RG, Is0, IsF, [V|Evs0], EvsF, Visited1, VisitedF)
	;
	    % insert parents
	    add_kids(Vs, G, RG, Is1, IsF, EvsI, EvsF, VisitedI, VisitedF)
	).
add_kids([_|Vs], G, RG, Is0, IsF, Evs0, EvsF, Visited0, VisitedF) :-
	add_kids(Vs, G, RG, Is0, IsF, Evs0, EvsF, Visited0, VisitedF).
	

not_in([V1|_], V) :- V1 == V, !, fail.
not_in([_|Evs0], V) :-
	not_in(Evs0, V).


