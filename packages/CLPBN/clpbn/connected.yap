
:- module(clpbn_connected,
	[influences/3,
	init_influences/3,
	influences/4]).

:- use_module(library(dgraphs),
	[dgraph_new/1,
	dgraph_add_edges/3,
	dgraph_add_vertex/3,
	dgraph_neighbors/3,
	dgraph_edge/3,
	dgraph_transpose/2]).

:- use_module(library(rbtrees),
	[rb_new/1,
	rb_lookup/3,
	rb_insert/4,
	rb_visit/2]).

influences(Vs, QVars, LV) :-
	init_influences(Vs, G, RG),
	influences(QVars, G, RG, LV).

init_influences(Vs, G, RG) :-
	dgraph_new(G0),
	to_dgraph(Vs, G0, G),
	dgraph_transpose(G, RG).

to_dgraph([], G, G).
to_dgraph([V|Vs], G0, G) :-
	clpbn:get_atts(V, [dist(_,Parents)]), !,
	dgraph_add_vertex(G0, V, G00),
	build_edges(Parents, V, Edges),
	dgraph_add_edges(G00, Edges, G1),
	to_dgraph(Vs, G1, G).

build_edges([], _, []).
build_edges([P|Parents], V, [P-V|Edges]) :-
	build_edges(Parents, V, Edges).

% search for the set of variables that influence V
influences(Vs, G, RG, Vars) :-
	rb_new(Visited0),
	influences(Vs, G, RG, Visited0, Visited),
	all_top(Visited, Vars),
length(Vars,Leng), writeln(done:Leng).

influences([], _,  _, Visited, Visited).
influences([V|LV], G, RG, Vs, NVs) :-
	rb_lookup(V, T.B, Vs), T == t, B == b, !,
	influences(LV, G, RG, Vs, NVs).
influences([V|LV], G, RG, Vs0, Vs3) :-
	rb_insert(Vs0, V, t.b, Vs1),
	process_new_variable(V, G, RG, Vs1, Vs2),
	influences(LV, G, RG, Vs2, Vs3).

process_new_variable(V, _G, _RG, _Vs0, _Vs1) :-
	clpbn:get_atts(V,[evidence(Ev)]), !,
	throw(error(bound_to_evidence(V/Ev))).
process_new_variable(V, G, RG, Vs0, Vs2) :-
	dgraph_neighbors(V, G, Children),
	throw_all_below(Children, G, RG, Vs0, Vs1),
	dgraph_neighbors(V, RG, Parents),
	throw_all_above(Parents, G, RG, Vs1, Vs2).

throw_all_below([], _, _, Vs, Vs).
throw_all_below(Child.Children, G, RG, Vs0, Vs2) :-
%	clpbn:get_atts(Child,[key(K)]), rb_visit(Vs0, Pairs), writeln(down:Child:K:Pairs),
	throw_below(Child, G, RG, Vs0, Vs1),
	throw_all_below(Children, G, RG, Vs1, Vs2).

% visited
throw_below(Child, G, RG, Vs0, Vs1) :-
	rb_lookup(Child, _.B, Vs0), !,
	( 
	    B == b ->
	  Vs0 = Vs1  % been there before
	    ;
	  B = b, % mark it
          handle_ball_from_above(Child, G, RG, Vs0, Vs1)
        ).
throw_below(Child, G, RG, Vs0, Vs2) :-
	rb_insert(Vs0, Child, _.b, Vs1),
	handle_ball_from_above(Child, G, RG, Vs1, Vs2).

% share this with parents, if we have evidence
handle_ball_from_above(V, G, RG, Vs0, Vs1) :-
	clpbn:get_atts(V,[evidence(_)]), !,
	dgraph_neighbors(V, RG, Parents),
	throw_all_above(Parents, G, RG, Vs0, Vs1).
% propagate to kids, if we do not
handle_ball_from_above(V, G, RG, Vs0, Vs1) :-
	dgraph_neighbors(V, G, Children),
	throw_all_below(Children, G, RG, Vs0, Vs1).
	
throw_all_above([], _, _, Vs, Vs).
throw_all_above(Parent.Parentren, G, RG, Vs0, Vs2) :-
%	clpbn:get_atts(Parent,[key(K)]), rb_visit(Vs0, Pairs), writeln(up:Parent:K:Pairs),
	throw_above(Parent, G, RG, Vs0, Vs1),
	throw_all_above(Parentren, G, RG, Vs1, Vs2).

% visited
throw_above(Parent, G, RG, Vs0, Vs1) :-
	rb_lookup(Parent, T._, Vs0), !,
	( 
	    T == t ->
	  Vs1 = Vs0  % been there before
	    ;
	  T = t, % mark it
          handle_ball_from_below(Parent, G, RG, Vs0, Vs1)
        ).
throw_above(Parent, G, RG, Vs0, Vs2) :-
	rb_insert(Vs0, Parent, t._, Vs1),
	handle_ball_from_below(Parent, G, RG, Vs1, Vs2).

% share this with parents, if we have evidence
handle_ball_from_below(V, _, _, Vs, Vs) :-
	clpbn:get_atts(V,[evidence(_)]), !.
% propagate to kids, if we do not
handle_ball_from_below(V, G, RG, Vs0, Vs1) :-
	dgraph_neighbors(V, RG, Parents),
	propagate_ball_from_below(Parents, V, G, RG, Vs0, Vs1).

propagate_ball_from_below([], V, G, RG, Vs0, Vs1) :- !,
	dgraph_neighbors(V, G, Children),
	throw_all_below(Children, G, RG, Vs0, Vs1).
propagate_ball_from_below(Parents, _V, G, RG, Vs0, Vs1) :-
	throw_all_above(Parents, G, RG, Vs0, Vs1).

all_top(T, Vs) :-
	rb_visit(T, Pairs),
	get_tops(Pairs, Vs).

get_tops([], []).
get_tops([V-(T._)|Pairs], V.Vs) :-
	T == t, !,	
	get_tops(Pairs, Vs).
get_tops([V-_|Pairs], V.Vs) :-
	clpbn:get_atts(V,[evidence(_)]), !,
	get_tops(Pairs, Vs).
get_tops(_.Pairs, Vs) :-
	get_tops(Pairs, Vs).

