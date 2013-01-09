
%:- style_check(all).

:- module(viterbi,
		[viterbi/4]).

:- use_module(library(lists),
		[nth/3,
		 member/2
		]).

:- use_module(library(assoc)).

:- use_module(library(dgraphs)).

:- use_module(library(matrix)).

:- use_module(library(clpbn), []).

:- ensure_loaded(library('clpbn/hmm')).

:- use_module(library('clpbn/dists'),
		[get_dist_params/2]).

:- meta_predicate viterbi(:,:,+,-).


viterbi(Start,End,String,Trace) :-
	init_hmm,
	Start,
	mk_graph(NOfNodes, Map, ViterbiCode),
	compile_trace(String, Emissions),
	get_id(Start, Map, SI),
	get_id(End, Map, EI),
	% add a random symbol in front (for the c/1 state).
	compiled_viterbi(NOfNodes, SI, ViterbiCode, Emissions, Dump, L),
	backtrace(Dump, EI, Map, L, Trace).

state_from_goal(_:Start,S) :-
	state_from_goal(Start,S).
state_from_goal(Start,S) :-
	functor(Start, N, Ar),
	% get rid of position and random var
	NAr is Ar-2,
	functor(S, N, NAr).


mk_graph(NOfNodes, Map, ViterbiCode) :-
	attributes:all_attvars(Vars0),
	empty_assoc(KeyMap0),
	get_graph(Vars0, Nodes, Edges, KeyMap0, KeyMap),
	dgraph_new(G0),
	dgraph_add_vertices(G0, Nodes, G1),
	dgraph_add_edges(G1, Edges, G2),
	dgraph_top_sort(G2, SortedNodes),
	compile_viterbi(SortedNodes, KeyMap, NOfNodes, Map, ViterbiCode).

get_graph([V|Vs], [NKey|Keys], EdgesF, KeyMap0, KeyMap) :-
	clpbn:get_atts(V,[key(Key), dist(Id,Parents)]),
	( Key =.. [N,2|More] ; Key = s(0), N=s, More=[] ), !,
	NKey =.. [N|More],
	fetch_edges(Parents, NKey, EdgesF, Edges0, PKeys),
	get_emission(V, Key, EmissionProb),
	put_assoc(NKey,KeyMap0,nodeinfo(_,Id,EmissionProb,PKeys),KeyMapI),
	get_graph(Vs, Keys, Edges0, KeyMapI, KeyMap).
get_graph([_|Vs], Keys, Edges, KeyMap0, KeyMap) :-
	get_graph(Vs, Keys, Edges, KeyMap0, KeyMap).
get_graph([], [], [], KeyMap, KeyMap).

get_emission(V, Key, EmissionProbs) :-
	hmm:get_atts(V,[emission(_)]), !,
	user:emission_cpt(Key, EmissionProbs).
get_emission(_, _, []).

fetch_edges([V|Parents], Key0, EdgesF, Edges0, [Slice-AKey|PKeys]) :-
	var(V), !,
	clpbn:get_atts(V,[key(Key)]),
	abstract_key(Key, AKey, Slice),
	(
	  Slice < 3
	->
	  EdgesF = [Key0-AKey|EdgesI]
	;
	  EdgesF = EdgesI
	),
	fetch_edges(Parents, Key0, EdgesI, Edges0, PKeys).
fetch_edges([Key|Parents], Key0, EdgesF, Edges0, [Slice-AKey|PKeys]) :-
	abstract_key(Key, AKey, Slice),
	(
	  Slice < 3
	->
	  EdgesF = [Key0-AKey|EdgesI]
	;
	  EdgesF = EdgesI
	),
	fetch_edges(Parents, Key0, EdgesI, Edges0, PKeys).
fetch_edges([], _, Edges, Edges, []).

abstract_key(Key, NKey, Slice) :-
	Key =.. [N,Slice|More],
	NKey =.. [N|More].


compile_viterbi(Keys, KeyMap, Nodes, Map, ViterbiCode) :-
	enum_keys(Keys, KeyMap, 0, Nodes, Map),
	compile_keys(Keys, KeyMap, ViterbiCode).

% just enumerate keys
enum_keys([], _, I, I, []).
enum_keys([Key|Keys], KeyMap, I0, Nodes, [I0-Key|Map]) :-
	get_assoc(Key,KeyMap,nodeinfo(I0,_,_,_)),
	I is I0+1,
	enum_keys(Keys, KeyMap, I, Nodes, Map).

compile_keys([Key|Keys], KeyMap, ViterbiCodeF) :-
	get_assoc(Key,KeyMap,nodeinfo(IKey,Id,Emission,PKeys)),
	compile_emission(Emission,IKey,ViterbiCodeF,ViterbiCodeI),
	get_dist_params(Id,Probs),
	compile_propagation(PKeys,Probs,IKey,KeyMap,ViterbiCodeI,ViterbiCode0),
	compile_keys(Keys, KeyMap, ViterbiCode0).
compile_keys([], _, []).


% add a random symbol to the end.
compile_emission([],_) --> !, [].
compile_emission(EmissionTerm,IKey) --> [emit(IKey,EmissionTerm)].

compile_propagation([],[],_,_) --> [].
compile_propagation([0-PKey|Ps], [Prob|Probs], IKey, KeyMap) -->
	[prop_same(IKey,Parent,Prob)],
	{ get_assoc(PKey,KeyMap,nodeinfo(Parent,_,_,_)) },
	compile_propagation(Ps, Probs, IKey, KeyMap).
compile_propagation([2-PKey|Ps], [Prob|Probs], IKey, KeyMap) -->
	[prop_same(IKey,Parent,Prob)],
	{ get_assoc(PKey,KeyMap,nodeinfo(Parent,_,_,_)) },
	compile_propagation(Ps, Probs, IKey, KeyMap).
compile_propagation([3-PKey|Ps], [Prob|Probs], IKey, KeyMap) -->
	[prop_next(IKey,Parent,Prob)],
	{ get_assoc(PKey,KeyMap,nodeinfo(Parent,_,_,_)) },
	compile_propagation(Ps, Probs, IKey, KeyMap).

get_id(_:S, Map, SI) :- !,
	get_id(S, Map, SI).
get_id(S, Map, SI) :-
	functor(S,N,A),
	A2 is A-2,
	functor(S2,N,A2),
	once(member(SI-S2,Map)).

compile_trace(Trace, Emissions) :-
	user:hmm_domain(Domain),
	(atom(Domain) ->
	  hmm:cvt_vals(Domain, Vals)
	;
	  Vals = Domain
	),
	compile_trace(Trace, Vals, Emissions).

compile_trace([], _, []).
compile_trace([El|Trace], Vals, [N|Emissions]) :-
	once(nth(N, Vals, El)),
	compile_trace(Trace, Vals, Emissions).

compiled_viterbi(Nodes, S, Commands, Input, Trace, L) :-
	length(Input,L),
	prolog_flag(min_tagged_integer, Min),
	matrix_new_set(ints,[Nodes], Min, Current),
	matrix_new_set(ints,[Nodes], Min, Next),
	L1 is L+1,
	matrix_new(ints,[L1,Nodes], Trace),
	matrix_set(Current, [S], 0),
	run_commands(Input, Commands, 0, Current, Next, Trace, Min).


run_commands([], _, _, _, _, _, _).
run_commands([E|Input], Commands, I, Current, Next, Trace, Min) :-
	run_code(Commands, E, I, Current, Next, Trace),
	matrix_get(Current, [32], M10),
	matrix_get(Current, [34], C),
	matrix_set_all(Current,Min),
	I1 is I+1,
	run_commands(Input, Commands, I1, Next, Current, Trace, Min).

run_code([], _, _, _, _, Trace).
run_code([Inst|Input], E, I, Current, Next, Trace) :-
	run_inst(Inst, E, I, Current, Next, Trace) ,
	run_code(Input, E, I, Current, Next, Trace).

run_inst(emit(Id,T), E, _SP, Current, _, Trace) :-
	arg(E,T,P),
	matrix_add(Current, [Id], P).
run_inst(prop_same(I,P,Prob), _, SP, Current, _, Trace) :-
	matrix_get(Current, [I], PI),
	NP is PI+Prob,
	matrix_get(Current, [P], P0),
	(NP > P0 ->
	  matrix_set(Current, [P], NP),
	  matrix_set(Trace, [SP,P], I)
	;
	  true
	).
run_inst(prop_next(I,P,Prob), _, SP, Current, Next, Trace) :-
	matrix_get(Current, [I], PI),
	NP is PI+Prob,
	matrix_get(Next, [P], P0),
	(NP > P0 ->
	  matrix_set(Next, [P], NP),
	  SP1 is SP+1,
	  IN is -I,
	  matrix_set(Trace, [SP1,P], IN)
	;
	  true
	).

backtrace(Dump, EI, Map, L, Trace) :-
	L1 is L-1,
	Pos = [L1,EI],
	matrix_get(Dump,Pos,Next),
	trace(L1,Next,Dump,Map,[],Trace).

trace(0,0,_,_,Trace,Trace) :- !.
trace(L1,Next,Dump,Map,Trace0,Trace) :-
	(Next < 0 ->
	  NL is L1-1,
	  P is -Next
	;
	  NL = L1,
	  P = Next
	),
	once(member(P-AKey,Map)),
	AKey=..[N|Args],
	Key=..[N,NL|Args],
	matrix_get(Dump,[NL,P],New),
	trace(NL,New,Dump,Map,[Key|Trace0],Trace).

