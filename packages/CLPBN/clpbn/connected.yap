
:- module(clpbn_connected,
		[influences/3,
		 factor_influences/4,
		 init_influences/3,
		 influences/4
		]).

:- use_module(library(maplist)).

:- use_module(library(dgraphs),
		[dgraph_new/1,
		 dgraph_add_edges/3,
		 dgraph_add_vertex/3,
		 dgraph_neighbors/3,
		 dgraph_edge/3,
		 dgraph_transpose/2
		]).

:- use_module(library(rbtrees),
		[rb_new/1,
		 rb_lookup/3,
		 rb_insert/4,
		 rb_visit/2
		]).

factor_influences(Vs, QVars, Ev, LV) :-
	init_factor_influences(Vs, G, RG),
	influences(QVars, Ev, G, RG, LV).

init_factor_influences(Vs, G, RG) :-
	dgraph_new(G0),
	foldl(factor_to_dgraph, Vs, G0, G),
	dgraph_transpose(G, RG).

influences(Vs, QVars, LV) :-
	init_influences(Vs, G, RG),
	influences(QVars, [], G, RG, LV).

init_influences(Vs, G, RG) :-
	dgraph_new(G0),
	to_dgraph(Vs, G0, G),
	dgraph_transpose(G, RG).

factor_to_dgraph(fn([V|Parents],_,_,_,_), G0, G) :-
	dgraph_add_vertex(G0, V, G00),
	build_edges(Parents, V, Edges),
	dgraph_add_edges(G00, Edges, G).

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
	influences(Vs, [], G, RG, Vars).

% search for the set of variables that influence V
influences(Vs, Evs, G, RG, Vars) :-
	rb_new(Visited0),
	foldl(influence(Evs, G, RG), Vs, Visited0, Visited),
	all_top(Visited, Evs, Vars).

influence(_, _G, _RG, V, Vs, Vs) :-
	rb_lookup(V, [T|B], Vs), T == t, B == b, !.
influence(Ev, G, RG, V, Vs0, Vs) :-
	rb_insert(Vs0, V, [t|b], Vs1),
	process_new_variable(V, Ev, G, RG, Vs1, Vs).

process_new_variable(V, _Evs, _G, _RG, _Vs0, _Vs1) :-
	var(V),
	clpbn:get_atts(V,[evidence(Ev)]), !,
	throw(error(bound_to_evidence(V/Ev))).
process_new_variable(V, Evs, _G, _RG, _Vs0, _Vs1) :-
	rb_lookup(V, Ev, Evs), !,
	throw(error(bound_to_evidence(V/Ev))).
process_new_variable(V, Evs, G, RG, Vs0, Vs2) :-
	dgraph_neighbors(V, G, Children),
	foldl(throw_below(Evs, G, RG), Children, Vs0, Vs1),
	dgraph_neighbors(V, RG, Parents),
	foldl(throw_above(Evs, G, RG), Parents, Vs1, Vs2).

% visited
throw_below(Evs, G, RG, Child, Vs0, Vs1) :-
	rb_lookup(Child, [_|B], Vs0), !,
	(
	  B == b
	->
	  Vs0 = Vs1  % been there before
	;
	  B = b, % mark it
	  handle_ball_from_above(Child, Evs, G, RG, Vs0, Vs1)
	).
throw_below(Evs, G, RG, Child, Vs0, Vs2) :-
	rb_insert(Vs0, Child, [_|b], Vs1),
	handle_ball_from_above(Child, Evs, G, RG, Vs1, Vs2).

% share this with parents, if we have evidence
handle_ball_from_above(V, Evs, G, RG, Vs0, Vs1) :-
	var(V),
	clpbn:get_atts(V,[evidence(_)]), !,
	dgraph_neighbors(V, RG, Parents),
	foldl(throw_above(Evs, G, RG), Parents, Vs0, Vs1).
handle_ball_from_above(V, Evs, G, RG, Vs0, Vs1) :-
	nonvar(V),
	rb_lookup(V,_,Evs), !,
	dgraph_neighbors(V, RG, Parents),
	foldl(throw_above(Evs, G, RG), Parents, Vs0, Vs1).
% propagate to kids, if we do not
handle_ball_from_above(V, Evs, G, RG, Vs0, Vs1) :-
	dgraph_neighbors(V, G, Children),
	foldl(throw_below(Evs, G, RG), Children, Vs0, Vs1).

% visited
throw_above(Evs, G, RG, Parent, Vs0, Vs1) :-
	rb_lookup(Parent, [T|_], Vs0), !,
	(
	  T == t
	->
	  Vs1 = Vs0  % been there before
	;
	  T = t, % mark it
	  handle_ball_from_below(Parent, Evs, G, RG, Vs0, Vs1)
	).
throw_above(Evs, G, RG, Parent, Vs0, Vs2) :-
	rb_insert(Vs0, Parent, [t|_], Vs1),
	handle_ball_from_below(Parent, Evs, G, RG, Vs1, Vs2).

% share this with parents, if we have evidence
handle_ball_from_below(V, _Evs, _, _, Vs, Vs) :-
	var(V),
	clpbn:get_atts(V,[evidence(_)]), !.
handle_ball_from_below(V, Evs, _, _, Vs, Vs) :-
	nonvar(V),
	rb_lookup(V, _, Evs), !.
% propagate to kids, if we do not
handle_ball_from_below(V, Evs, G, RG, Vs0, Vs1) :-
	dgraph_neighbors(V, RG, Parents),
	propagate_ball_from_below(Parents, Evs, V, G, RG, Vs0, Vs1).

propagate_ball_from_below([], Evs, V, G, RG, Vs0, Vs1) :- !,
	dgraph_neighbors(V, G, Children),
	foldl(throw_below(Evs, G, RG), Children, Vs0, Vs1).
propagate_ball_from_below(Parents, Evs, _V, G, RG, Vs0, Vs1) :-
	foldl(throw_above(Evs, G, RG), Parents, Vs0, Vs1).

all_top(T, Evs, Vs) :-
	rb_visit(T, Pairs),
	foldl( get_top(Evs), Pairs, [], Vs).

get_top(_EVs, V-[T|_], Vs, [V|Vs]) :-
	T == t, !.
get_top(_EVs, V-_, Vs, [V|Vs]) :-
	var(V),
	clpbn:get_atts(V,[evidence(_)]), !.
get_top(EVs, V-_, Vs, [V|Vs]) :-
	nonvar(V),
	rb_lookup(V, _, EVs), !.
get_top(_, _, Vs, Vs).

