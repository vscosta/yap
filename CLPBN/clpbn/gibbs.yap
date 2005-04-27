
%
% each variable is represented by a node in a binary tree.
% each node contains:
% key,
% current_value
% Markov Blanket
%

:- module(gibbs, [gibbs/3,
		check_if_gibbs_done/1]).

:- use_module(library(rbtrees),
	      [new/1,
	       insert/4]).

:- use_module(library(lists),
	      [member/2,
	       append/3,
	       delete/3]).

:- use_module(library('clpbn/discrete_utils'), [
	project_from_CPT/3,
	reorder_CPT/5]).

:- use_module(library('clpbn/utils'), [
	check_for_hidden_vars/3]).

:- dynamic gibbs_params/3.

gibbs([],_,_) :- !.
gibbs(LVs,Vs0,_) :-
	check_for_hidden_vars(Vs0, Vs0, Vs1),
	sort(Vs1,Vs),
	(clpbn:output(xbif(XBifStream)) -> clpbn2xbif(XBifStream,vel,Vs) ; true),
	(clpbn:output(gviz(XBifStream)) -> clpbn2gviz(XBifStream,vel,Vs,LVs) ; true),
	initialise(Vs, Graph, LVs, OutputVars),
%	write(Graph),nl,
	process(Graph, OutputVars, Estimates),
	write(Estimates),nl,
	clean_up.

initialise(LVs, Graph, GVs, OutputVars) :-
	init_keys(Keys0),
	gen_keys(LVs, 0, VLen, Keys0, Keys),
	functor(Graph,graph,VLen),
	graph_representation(LVs, Graph, 0, Keys),
	compile_graph(Graph),
	listing(mblanket),
	add_output_vars(GVs, Keys, OutputVars).

init_keys(Keys0) :-
	new(Keys0).

gen_keys([], I, I, Keys, Keys).
gen_keys([V|Vs], I0, If, Keys0, Keys) :-
	clpbn:get_atts(V,[evidence(_)]), !,
	gen_keys(Vs, I0, If, Keys0, Keys).
gen_keys([V|Vs], I0, If, Keys0, Keys) :-
	I is I0+1,
	insert(Keys0,V,I,KeysI),
	gen_keys(Vs, I, If, KeysI, Keys).

graph_representation([],_,_,_).
graph_representation([V|Vs], Graph, I0, Keys) :-
	clpbn:get_atts(V,[evidence(_)]), !,
	clpbn:get_atts(V, [dist(Vals,Table,Parents)]),
	get_sizes(Parents, Szs),
	length(Vals,Sz),
	project_evidence_out([V|Parents],[V|Parents],Table,[Sz|Szs],Variables,NewTable),
	% all variables are parents
	propagate2parents(Variables, NewTable, Variables, Graph, Keys),
	graph_representation(Vs, Graph, I0, Keys).
graph_representation([V|Vs], Graph, I0, Keys) :-
	I is I0+1,
	clpbn:get_atts(V, [dist(Vals,Table,Parents)]),
	get_sizes(Parents, Szs),
	length(Vals,Sz),
	project_evidence_out([V|Parents],[V|Parents],Table,[Sz|Szs],Variables,NewTable),
	Variables = [V|NewParents],
	compact_table(NewTable, RepTable),
	add2graph(V, Vals, RepTable, NewParents, Graph, Keys),
	propagate2parents(NewParents, NewTable, Variables, Graph, Keys),
	graph_representation(Vs, Graph, I, Keys).

get_sizes([], []).
get_sizes([V|Parents], [Sz|Szs]) :-
	clpbn:get_atts(V, [dist(Vals,_,_)]),
	length(Vals,Sz),
	get_sizes(Parents, Szs).

%
% first, remove nodes that have evidence from tables.
%
project_evidence_out([],Deps,Table,_,Deps,Table).
project_evidence_out([V|Parents],Deps,Table,Szs,NewDeps,NewTable) :-
	clpbn:get_atts(V,[evidence(_)]), !,
	NTab =.. [t|Table],
	project_from_CPT(V,tab(NTab,Deps,Szs),tab(ITable,IDeps,ISzs)),
	ITable =.. [_|LITable],
	project_evidence_out(Parents,IDeps,LITable,ISzs,NewDeps,NewTable).
project_evidence_out([Par|Parents],Deps,Table,Szs,NewDeps,NewTable) :-
	project_evidence_out(Parents,Deps,Table,Szs,NewDeps,NewTable).

propagate2parents([], _, _, _, _).
propagate2parents([V|NewParents], Table, Variables, Graph, Keys) :-
	delete(Variables,V,NVs),
	reorder_CPT(Variables,Table,[V|NVs],NewTable,_),
	add2graph(V, _, NewTable, NVs, Graph, Keys),
	NewTable =.. [_|LNewTable],
	propagate2parents(NewParents, LNewTable, Variables, Graph, Keys).

add2graph(V, Vals, Table, Parents, Graph, Keys) :-
	lookup(V, Index, Keys),	
	(var(Vals) -> true ; length(Vals,Sz)),
	arg(Index, Graph, var(V,Index,_,Vals,Sz,VarSlot,_)),
	vars2indices(Parents,Keys,IParents),
	member(tabular(Table,Index,IParents), VarSlot), !.

vars2indices([],_,[]).
vars2indices([V|Parents],Keys,[I|IParents]) :-
	lookup(V, I, Keys),
	vars2indices(Parents,Keys,IParents).

compact_table(NewTable, RepTable) :-
	NewTable = [_|_], !,
	RepTable =.. [t|NewTable].

%
% This is the really cool bit.
%
compile_graph(Graph) :-
	Graph =.. [_|VarsInfo],
	compile_vars(VarsInfo,Graph).

compile_vars([],_).
compile_vars([var(_,I,_,Vals,Sz,VarSlot,Parents)|VarsInfo],Graph) :-
	compile_var(I,Vals,Sz,VarSlot,Parents,Graph),
	compile_vars(VarsInfo,Graph).

compile_var(I,Vals,Sz,VarSlot,Parents,Graph) :-
	fetch_all_parents(VarSlot,Graph,[],Parents,[],Sizes),
	mult_list(Sizes,1,TotSize),
	compile_var(TotSize,I,Vals,Sz,VarSlot,Parents,Sizes,Graph).

fetch_all_parents([],_,Parents,Parents,Sizes,Sizes).
fetch_all_parents([tabular(_,_,Ps)|CPTs],Graph,Parents0,ParentsF,Sizes0,SizesF) :-
	merge_this_parents(Ps,Graph,Parents0,ParentsI,Sizes0,SizesI),
	fetch_all_parents(CPTs,Graph,ParentsI,ParentsF,SizesI,SizesF).

merge_this_parents([],_,Parents,Parents,Sizes,Sizes).
merge_this_parents([I|Ps],Graph,Parents0,ParentsF,Sizes0,SizesF) :-
	member(I,Parents0), !,
	merge_this_parents(Ps,Graph,Parents0,ParentsF,Sizes0,SizesF).
merge_this_parents([I|Ps],Graph,Parents0,ParentsF,Sizes0,SizesF) :-
	arg(I,Graph,var(_,I,_,Vals,_,_,_)),
	length(Vals, Sz),
	merge_this_parents(Ps,Graph,[I|Parents0],ParentsF,[Sz|Sizes0],SizesF).

mult_list([],Mult,Mult).
mult_list([Sz|Sizes],Mult0,Mult) :-
	MultI is Sz*Mult0,
	mult_list(Sizes,MultI,Mult).

% we'd need 32 facts for each case
compile_var(_TotSize,I,_Vals,Sz,CPTs,Parents,_Sizes,Graph) :-
%	TotSize =< 32,
	multiply_all(I,Parents,CPTs,Sz,Graph).

multiply_all(I,Parents,CPTs,Sz,Graph) :-
	markov_blanket_instance(Parents,Graph,Values),
	multiply_all(CPTs,Sz,Graph,Probs),
	write(Values:Probs:CPTs),nl,
	store_mblanket(I,Values,Probs),
	fail.
multiply_all(_,_,_,_,_).

% note: what matters is how this predicate instantiates the temp
% slot in the graph!
markov_blanket_instance([],_,[]).
markov_blanket_instance([I|Parents],Graph,[Pos|Values]) :-
	arg(I,Graph,var(_,I,Pos,Vals,_,_,_)),
	fetch_val(Vals,0,Pos),
	markov_blanket_instance(Parents,Graph,Values).

% backtrack through every value in domain
%
fetch_val([_|_],Pos,Pos).
fetch_val([_|Vals],I0,Pos) :-
	I is I0+1,
	fetch_val(Vals,I,Pos).

multiply_all(CPTs,Size,Graph,Probs) :-
	init_factors(Size,Factors0),
	mult_factors(CPTs,Size,Graph,Factors0,Factors),
	normalise_factors(Factors,0,_,Probs,_).

init_factors(0,[]) :- !.
init_factors(I0,[1|Factors]) :-
	I is I0-1,
	init_factors(I,Factors).
	
mult_factors([],_,_,Factors,Factors) :- !.
mult_factors([tabular(Table,_,Parents)|CPTs],Size,Graph,Factors0,Factors) :-
	factor(Parents,Table,Graph,0,1,Indx0),
	functor(Table,_,CPTSize),
	Off is CPTSize//Size,
	Indx is Indx0+1,
	mult_with_probs(Factors0,Indx,Off,Table,FactorsI),
	mult_factors(CPTs,Size,Graph,FactorsI,Factors).
	
factor([],_,_,Arg,_,Arg).
factor([I|Parents],Table,Graph,Pos0,Weight0,Pos) :-
	arg(I,Graph,var(_,I,CurPos,_,Sz,_,_)),
	PosI is Pos0+(Weight0*CurPos),
	NWeight is Weight0*Sz,
	factor(Parents,Table,Graph,PosI,NWeight,Pos).

mult_with_probs([],_,_,_,[]).
mult_with_probs([F0|Factors0],Indx,Off,Table,[F|Factors]) :-
	arg(Indx,Table,P1),
	F is F0*P1,
	Indx1 is Indx+Off,
	mult_with_probs(Factors0,Indx1,Off,Table,Factors).	

normalise_factors([],Sum,Sum,[],1.0) :- Sum > 0.0.
normalise_factors([F|Factors],S0,S,[P0|Probs],PF) :-
	Si is S0+F,
	normalise_factors(Factors,Si,S,Probs,P0),
	PF is P0-F/S.

store_mblanket(I,Values,Probs) :-
	append(Values,Probs,Args),
	Rule =.. [mblanket,I|Args],
	assert(Rule).

add_output_vars([], _, []).
add_output_vars([V|LVs], Keys, [I|OutputVars]) :-
	lookup(V, I, Keys),
	add_output_vars(LVs, Keys, OutputVars).

process(Graph,OutputVars,Estimates) :-
	gibbs_params(NChains,BurnIn,NSamples),
	functor(Graph,_,Len),
	init_chains(NChains,Len,Graph,Chains0),
	init_estimates(NChains,OutputVars,Graph,Est0),
	process_chains(BurnIn,BurnedIn,Chains0,Graph,Len,Est0,_),
	process_chains(NSamples,_,BurnedIn,Graph,Len,Est0,Estimates).

%
% I use an uniform distribution to generate the initial sample.
%
init_chains(0,_,_,[]) :- !.
init_chains(I,Len,Graph,[Chain|Chains]) :-
	init_chain(Len,Graph,Chain),
	I1 is I-1,
	init_chains(I1,Len,Graph,Chains).


init_chain(Len,Graph,Chain) :-
	gen_sample(Len,Graph,LChain),
	Chain =.. [sample|LChain].

gen_sample(0,_,[]) :- !.
gen_sample(I,Graph,[R|LChain]) :-
	arg(I,Graph,var(_,I,_,_,Sz,_,_)),
	R is integer(random*Sz),
	I1 is I-1,
	gen_sample(I1,Graph,LChain).


init_estimates(0,_,_,[]) :- !.
init_estimates(NChains,OutputVars,Graph,[Est|Est0]) :-
	NChainsI is NChains-1,
	init_estimate(OutputVars,Graph,Est),
	init_estimates(NChainsI,OutputVars,Graph,Est0).

init_estimate([],_,[]).
init_estimate([V|OutputVars],Graph,[[I|E0L]|Est]) :-
	arg(V,Graph,var(_,I,_,_,Sz,_,_)),
	gen_e0(Sz,E0L),
	init_estimate(OutputVars,Graph,Est).

gen_e0(0,[]) :- !.
gen_e0(Sz,[0|E0L]) :-
	Sz1 is Sz-1,
	gen_e0(Sz1,E0L).


process_chains(0,F,F,_,_,Est,Est) :- !.
process_chains(ToDo,End,Start,Graph,Len,Est0,Estf) :-
	process_chains(Start,Int,Graph,Len,Est0,Esti),
	ToDo1 is ToDo-1,
	process_chains(ToDo1,End,Int,Graph,Len,Esti,Estf).


process_chains([], [], _, _,[],[]).
process_chains([Sample0|Samples0], [Sample|Samples], Graph, SampLen,[E0|E0s],[Ef|Efs]) :-
	functor(Sample,sample,SampLen),
	do_sample(0,SampLen,Sample,Sample0,Graph),
	update_estimate(E0,Sample,Ef),
	process_chains(Samples0, Samples, Graph, SampLen,E0s,Efs).

do_sample(Len,Len,_,_,_) :- !.
do_sample(I0,Len,Sample,Sample0,Graph) :-
	I is I0+1,
	do_var(I,Sample,Sample0,Graph),
	do_sample(I,Len,Sample,Sample0,Graph).

do_var(I,Sample,Sample0,Graph) :-
	arg(I,Graph,var(_,I,_,_,Sz,_,Parents)),
	length(Vals,Sz),
	fetch_parents(Parents,I,Sample,Sample0,Args,Vals),
	Goal =.. [mblanket,I|Args],
	(call(Goal) -> true ; throw(agg)),
	X is random,
	pick_new_value(Vals,X,0,Val),
	arg(I,Sample,Val).

fetch_parents([],_,_,_,Args,Args).
fetch_parents([P|Parents],I,Sample,Sample0,[VP|Args],Vals) :-
	P < I, !,
	arg(P,Sample,VP),
	fetch_parents(Parents,I,Sample,Sample0,Args,Vals).
fetch_parents([P|Parents],I,Sample,Sample0,[VP|Args],Vals) :-
	arg(P,Sample0,VP),
	fetch_parents(Parents,I,Sample,Sample0,Args,Vals).

pick_new_value([V|_],X,Val,Val) :-
	X < V, !.
pick_new_value([_|Vals],X,I0,Val) :-
	I is I0+1,
	pick_new_value(Vals,X,I,Val).

update_estimate([],_,[]).
update_estimate([[I|E]|E0],Sample,[[I|NE]|Ef]) :-
	arg(I,Sample,V),
	update_estimate_for_var(V,E,NE),
	update_estimate(E0,Sample,Ef).

update_estimate_for_var(0,[X|T],[X1|T]) :- !, X1 is X+1.
update_estimate_for_var(V,[E|Es],[E|NEs]) :-
	V1 is V-1,
	update_estimate_for_var(V1,Es,NEs).



check_if_gibbs_done(Var) :-
	get_atts(Var, [dist(_)]), !.

clean_up :-
	current_predicate(mblanket,P),
	retractall(P),
	fail.
clean_up.


gibbs_params(5,1000,100000).

