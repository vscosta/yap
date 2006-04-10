
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
	      [rb_new/1,
	       rb_insert/4,
	       rb_lookup/3]).

:- use_module(library(lists),
	      [member/2,
	       append/3,
	       delete/3,
	       max_list/2]).

:- use_module(library(ordsets),
	      [ord_subtract/3]).

:- use_module(library('clpbn/discrete_utils'), [
	project_from_CPT/3,
	reorder_CPT/5]).

:- use_module(library('clpbn/utils'), [
	check_for_hidden_vars/3]).

:- use_module(library('clpbn/topsort'), [
	topsort/2]).

:- dynamic gibbs_params/3.

:- dynamic implicit/1.

gibbs([],_,_) :- !.
gibbs(LVs,Vs0,_) :-
	clean_up,
	check_for_hidden_vars(Vs0, Vs0, Vs1),
	sort(Vs1,Vs),
	(clpbn:output(xbif(XBifStream)) -> clpbn2xbif(XBifStream,vel,Vs) ; true),
	(clpbn:output(gviz(XBifStream)) -> clpbn2gviz(XBifStream,vel,Vs,LVs) ; true),
	initialise(Vs, Graph, LVs, OutputVars, VarOrder),
%	write(Graph),nl,
	process(VarOrder, Graph, OutputVars, Estimates),
	write(Estimates),nl,
	clean_up.

initialise(LVs, Graph, GVs, OutputVars, VarOrder) :-
	init_keys(Keys0),
	gen_keys(LVs, 0, VLen, Keys0, Keys),
	functor(Graph,graph,VLen),
	graph_representation(LVs, Graph, 0, Keys, TGraph),
	compile_graph(Graph),
	topsort(TGraph, VarOrder),
%	show_sorted(VarOrder, Graph),
	add_output_vars(GVs, Keys, OutputVars).

init_keys(Keys0) :-
	rb_new(Keys0).

gen_keys([], I, I, Keys, Keys).
gen_keys([V|Vs], I0, If, Keys0, Keys) :-
	clpbn:get_atts(V,[evidence(_)]), !,
	gen_keys(Vs, I0, If, Keys0, Keys).
gen_keys([V|Vs], I0, If, Keys0, Keys) :-
	I is I0+1,
	rb_insert(Keys0,V,I,KeysI),
	gen_keys(Vs, I, If, KeysI, Keys).

graph_representation([],_,_,_,[]).
graph_representation([V|Vs], Graph, I0, Keys, TGraph) :-
	clpbn:get_atts(V,[evidence(_)]), !,
	clpbn:get_atts(V, [dist(Vals,Table,Parents)]),
	get_sizes(Parents, Szs),
	length(Vals,Sz),
	project_evidence_out([V|Parents],[V|Parents],Table,[Sz|Szs],Variables,NewTable),
	% all variables are parents
	propagate2parents(Variables, NewTable, Variables, Graph, Keys),
	graph_representation(Vs, Graph, I0, Keys, TGraph).
graph_representation([V|Vs], Graph, I0, Keys, [I-IParents|TGraph]) :-
	I is I0+1,
	clpbn:get_atts(V, [dist(Vals,Table,Parents)]),
	get_sizes(Parents, Szs),
	length(Vals,Sz),
	project_evidence_out([V|Parents],[V|Parents],Table,[Sz|Szs],Variables,NewTable),
	Variables = [V|NewParents],
	sort_according_to_indices(NewParents,Keys,SortedNVs,SortedIndices),
	reorder_CPT(Variables,NewTable,[V|SortedNVs],NewTable2,_),
	add2graph(V, Vals, NewTable2, SortedIndices, Graph, Keys),
	propagate2parents(NewParents, NewTable, Variables, Graph,Keys),
	parent_indices(NewParents, Keys, IVariables0),
	sort(IVariables0, IParents),
	arg(I, Graph, var(_,_,_,_,_,_,_,NewTable2,SortedIndices)),
	graph_representation(Vs, Graph, I, Keys, TGraph).

write_pars([]).
write_pars([V|Parents]) :- 
	clpbn:get_atts(V, [key(K)]),write(K),nl,
	write_pars(Parents).

get_sizes([], []).
get_sizes([V|Parents], [Sz|Szs]) :-
	clpbn:get_atts(V, [dist(Vals,_,_)]),
	length(Vals,Sz),
	get_sizes(Parents, Szs).

parent_indices([], _, []).
parent_indices([V|Parents], Keys, [I|IParents]) :-
	rb_lookup(V, I, Keys),	
	parent_indices(Parents, Keys, IParents).



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
project_evidence_out([_Par|Parents],Deps,Table,Szs,NewDeps,NewTable) :-
	project_evidence_out(Parents,Deps,Table,Szs,NewDeps,NewTable).

propagate2parents([], _, _, _, _).
propagate2parents([V|NewParents], Table, Variables, Graph, Keys) :-
	delete(Variables,V,NVs),
	sort_according_to_indices(NVs,Keys,SortedNVs,SortedIndices),
	reorder_CPT(Variables,Table,[V|SortedNVs],NewTable,_),
	add2graph(V, _, NewTable, SortedIndices, Graph, Keys),
	propagate2parents(NewParents,Table, Variables, Graph, Keys).

add2graph(V, Vals, Table, IParents, Graph, Keys) :-
	rb_lookup(V, Index, Keys),	
	(var(Vals) -> true ; length(Vals,Sz)),
	arg(Index, Graph, var(V,Index,_,Vals,Sz,VarSlot,_,_,_)),
	member(tabular(Table,Index,IParents), VarSlot), !.

sort_according_to_indices(NVs,Keys,SortedNVs,SortedIndices) :-
	vars2indices(NVs,Keys,ToSort),
	keysort(ToSort, Sorted),
	split_parents(Sorted, SortedNVs,SortedIndices).

split_parents([], [], []).
split_parents([I-V|Sorted], [V|SortedNVs],[I|SortedIndices]) :-
	split_parents(Sorted, SortedNVs, SortedIndices).


vars2indices([],_,[]).
vars2indices([V|Parents],Keys,[I-V|IParents]) :-
	rb_lookup(V, I, Keys),
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
compile_vars([var(_,I,_,Vals,Sz,VarSlot,Parents,_,_)|VarsInfo],Graph)
:-
	
	compile_var(I,Vals,Sz,VarSlot,Parents,Graph),
	compile_vars(VarsInfo,Graph).

compile_var(I,Vals,Sz,VarSlot,Parents,Graph) :-
	fetch_all_parents(VarSlot,Graph,[],Parents,[],Sizes),
	mult_list(Sizes,1,TotSize),
	compile_var(TotSize,I,Vals,Sz,VarSlot,Parents,Sizes,Graph).

fetch_all_parents([],_,Parents,Parents,Sizes,Sizes).
fetch_all_parents([tabular(_,_,Ps)|CPTs],Graph,Parents0,ParentsF,Sizes0,SizesF) :-
	merge_these_parents(Ps,Graph,Parents0,ParentsI,Sizes0,SizesI),
	fetch_all_parents(CPTs,Graph,ParentsI,ParentsF,SizesI,SizesF).

merge_these_parents([],_,Parents,Parents,Sizes,Sizes).
merge_these_parents([I|Ps],Graph,Parents0,ParentsF,Sizes0,SizesF) :-
	member(I,Parents0), !,
	merge_these_parents(Ps,Graph,Parents0,ParentsF,Sizes0,SizesF).
merge_these_parents([I|Ps],Graph,Parents0,ParentsF,Sizes0,SizesF) :-
	arg(I,Graph,var(_,I,_,Vals,_,_,_,_,_)),
	length(Vals, Sz),
	add_parent(Parents0,I,ParentsI,Sizes0,Sz,SizesI),
	merge_these_parents(Ps,Graph,ParentsI,ParentsF,SizesI,SizesF).

add_parent([],I,[I],[],Sz,[Sz]).
add_parent([P|Parents0],I,[I,P|Parents0],Sizes0,Sz,[Sz|Sizes0]) :-
	P > I, !.
add_parent([P|Parents0],I,[P|ParentsI],[S|Sizes0],Sz,[S|SizesI]) :-
	add_parent(Parents0,I,ParentsI,Sizes0,Sz,SizesI).


mult_list([],Mult,Mult).
mult_list([Sz|Sizes],Mult0,Mult) :-
	MultI is Sz*Mult0,
	mult_list(Sizes,MultI,Mult).

% compile node as set of facts, faster execution 
compile_var(TotSize,I,_Vals,Sz,CPTs,Parents,_Sizes,Graph) :-
	TotSize < 1024*64, TotSize > 0, !,
	multiply_all(I,Parents,CPTs,Sz,Graph).
compile_var(_,I,_,_,_,_,_,_) :-
	assert(implicit(I)).

multiply_all(I,Parents,CPTs,Sz,Graph) :-
	markov_blanket_instance(Parents,Graph,Values),
	multiply_all(CPTs,Sz,Graph,Probs),
	store_mblanket(I,Values,Probs),
	fail.
multiply_all(_,_,_,_,_).

% note: what matters is how this predicate instantiates the temp
% slot in the graph!
markov_blanket_instance([],_,[]).
markov_blanket_instance([I|Parents],Graph,[Pos|Values]) :-
	arg(I,Graph,var(_,I,Pos,Vals,_,_,_,_,_)),
	fetch_val(Vals,0,Pos),
	markov_blanket_instance(Parents,Graph,Values).

% backtrack through every value in domain
%
fetch_val([_|_],Pos,Pos).
fetch_val([_|Vals],I0,Pos) :-
	I is I0+1,
	fetch_val(Vals,I,Pos).

:- dynamic a/0.

multiply_all(CPTs,Size,Graph,Probs) :-
	init_factors(Size,Factors0),
	mult_factors(CPTs,Size,Graph,Factors0,Factors),
	normalise_factors(Factors,Probs).

init_factors(0,[]) :- !.
init_factors(I0,[0.0|Factors]) :-
	I is I0-1,
	init_factors(I,Factors).
	
mult_factors([],_,_,Factors,Factors).
mult_factors([tabular(Table,_,Parents)|CPTs],Size,Graph,Factors0,Factors) :-
	functor(Table,_,CPTSize),
	Off is CPTSize//Size,
	factor(Parents,Table,Graph,0,Off,Indx0),
	Indx is Indx0+1,
	mult_with_probs(Factors0,Indx,Off,Table,FactorsI),
	mult_factors(CPTs,Size,Graph,FactorsI,Factors).
	
factor([],_,_,Arg,_,Arg).
factor([I|Parents],Table,Graph,Pos0,Weight0,Pos) :-
	arg(I,Graph,var(_,I,CurPos,_,Sz,_,_,_,_)),
	NWeight is Weight0 // Sz,
	PosI is Pos0+(NWeight*CurPos),
	factor(Parents,Table,Graph,PosI,NWeight,Pos).

mult_with_probs([],_,_,_,[]).
mult_with_probs([F0|Factors0],Indx,Off,Table,[F|Factors]) :-
	arg(Indx,Table,P1),
	F is F0+log(P1),
	Indx1 is Indx+Off,
	mult_with_probs(Factors0,Indx1,Off,Table,Factors).	

normalise_factors(Factors,Probs) :-
	max_list(Factors,Max),
	logs2list(Factors,Max,NFactors),
	normalise_factors(NFactors,0,_,Probs,_).

logs2list([],_,[]).
logs2list([Log|Factors],Max,[P|NFactors]) :-
	P is exp(Log+Max),
	logs2list(Factors,Max,NFactors).


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
	rb_lookup(V, I, Keys),
	add_output_vars(LVs, Keys, OutputVars).

process(VarOrder, Graph, OutputVars, Estimates) :-
	gibbs_params(NChains,BurnIn,NSamples),
	functor(Graph,_,Len),
	init_chains(NChains,VarOrder,Len,Graph,Chains0),
	init_estimates(NChains,OutputVars,Graph,Est0),
	process_chains(BurnIn,VarOrder,BurnedIn,Chains0,Graph,Len,Est0,_),
	process_chains(NSamples,VarOrder,_,BurnedIn,Graph,Len,Est0,Estimates).

%
% I use an uniform distribution to generate the initial sample.
%
init_chains(0,_,_,_,[]) :- !.
init_chains(I,VarOrder,Len,Graph,[Chain|Chains]) :-
	init_chain(VarOrder,Len,Graph,Chain),
	I1 is I-1,
	init_chains(I1,VarOrder,Len,Graph,Chains).


init_chain(VarOrder,Len,Graph,Chain) :-
	functor(Chain,sample,Len),
	gen_sample(VarOrder,Graph,Chain).

gen_sample([],_,_) :- !.
gen_sample([I|Vs],Graph,Chain) :-
	arg(I,Graph,var(_,I,_,_,Sz,_,_,Table,IPars)),
	functor(Table,_,CPTSize),
	Off is CPTSize//Sz,
	iparents_pos_sz(IPars, Chain, IPos, Graph, ISz),
	R is random,
	project(IPos, ISz, Table,0,Off,Indx0),
	Indx is Indx0+1,
	fetch_from_dist(Table,R,Indx,Off,0,Pos),
	arg(I,Chain,Pos),
	gen_sample(Vs,Graph,Chain).

project([],[],_,Arg,_,Arg).
project([CurPos|Parents],[Sz|Sizes],Table,Pos0,Weight0,Pos) :-
	NWeight is Weight0 // Sz,
	PosI is Pos0+(NWeight*CurPos),
	project(Parents,Sizes,Table,PosI,NWeight,Pos).

fetch_from_dist(Table,R,Indx,Off,IPos,Pos) :-
	arg(Indx,Table,P),
	( P >= R ->
	  Pos = IPos
	;
	  NR is R-P,
	  NIndx is Indx+Off,
	  NPos is IPos+1,
	  fetch_from_dist(Table,NR,NIndx,Off,NPos,Pos)
	).


iparents_pos_sz([], _, [], _, []).
iparents_pos_sz([I|IPars], Chain, [P|IPos], Graph, [Sz|Sizes]) :-
	arg(I,Chain,P),
	arg(I,Graph, var(_,I,_,_,Sz,_,_,_,_)),
	iparents_pos_sz(IPars, Chain, IPos, Graph, Sizes).


init_estimates(0,_,_,[]) :- !.
init_estimates(NChains,OutputVars,Graph,[Est|Est0]) :-
	NChainsI is NChains-1,
	init_estimate(OutputVars,Graph,Est),
	init_estimates(NChainsI,OutputVars,Graph,Est0).

init_estimate([],_,[]).
init_estimate([V|OutputVars],Graph,[[I|E0L]|Est]) :-
	arg(V,Graph,var(_,I,_,_,Sz,_,_,_,_)),
	gen_e0(Sz,E0L),
	init_estimate(OutputVars,Graph,Est).

gen_e0(0,[]) :- !.
gen_e0(Sz,[0|E0L]) :-
	Sz1 is Sz-1,
	gen_e0(Sz1,E0L).

process_chains(0,_,F,F,_,_,Est,Est) :- !.
process_chains(ToDo,VarOrder,End,Start,Graph,Len,Est0,Estf) :-
	process_chains(Start,VarOrder,Int,Graph,Len,Est0,Esti),
(ToDo mod 100 =:= 0 -> statistics,cvt2problist(Esti, Probs), Int =[S|_], format('did ~d: ~w~n ~w~n',[ToDo,Probs,S]) ; true),
	ToDo1 is ToDo-1,
	process_chains(ToDo1,VarOrder,End,Int,Graph,Len,Esti,Estf).


process_chains([], _, [], _, _,[],[]).
process_chains([Sample0|Samples0], VarOrder, [Sample|Samples], Graph, SampLen,[E0|E0s],[Ef|Efs]) :-
	functor(Sample,sample,SampLen),
	do_sample(VarOrder,Sample,Sample0,Graph),
%format('Sample = ~w~n',[Sample]),
	update_estimate(E0,Sample,Ef),
	process_chains(Samples0, VarOrder, Samples, Graph, SampLen,E0s,Efs).

do_sample([],_,_,_).
do_sample([I|VarOrder],Sample,Sample0,Graph) :-
	do_var(I,Sample,Sample0,Graph),
	do_sample(VarOrder,Sample,Sample0,Graph).

do_var(I,Sample,Sample0,Graph) :-
	arg(I,Graph,var(_,I,_,_,Sz,CPTs,Parents,_,_)),
	( implicit(I) ->
	   fetch_parents(Parents,I,Sample,Sample0,Bindings,[]),
	   multiply_all_in_context(Parents,Bindings,CPTs,Sz,Graph,Vals)
	;
	   length(Vals,Sz),	   
	   fetch_parents(Parents,I,Sample,Sample0,Args,Vals),
	   Goal =.. [mblanket,I|Args],
	   call(Goal)
	),
	X is random,
	pick_new_value(Vals,X,0,Val),
	arg(I,Sample,Val).

multiply_all_in_context(Parents,Args,CPTs,Sz,Graph,Vals) :-
	set_pos(Parents,Args,Graph),
	multiply_all(CPTs,Sz,Graph,Vals),
	assert(mall(Vals)), fail.
multiply_all_in_context(_,_,_,_,_,Vals) :-
	retract(mall(Vals)).

set_pos([],[],_).
set_pos([I|Is],[Pos|Args],Graph) :-
	arg(I,Graph,var(_,I,Pos,_,_,_,_,_,_)),
	set_pos(Is,Args,Graph).

fetch_parents([],_,_,_,Args,Args).
fetch_parents([P|Parents],I,Sample,Sample0,[VP|Args],Vals) :-
	arg(P,Sample,VP),
	nonvar(VP), !,
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
clean_up :-
	retractall(implicit(_)),
	fail.
clean_up.


gibbs_params(5,10000,100000).

cvt2problist([], []).
cvt2problist([[[_|E]]|Est0], [Ps|Probs]) :-
	sum_all(E,0,Sum),
	do_probs(E,Sum,Ps),
	cvt2problist(Est0, Probs) .

sum_all([],Sum,Sum).
sum_all([E|Es],S0,Sum) :-
	SI is S0+E,
	sum_all(Es,SI,Sum).

do_probs([],_,[]).
do_probs([E|Es],Sum,[P|Ps]) :-
	P is E/Sum,
	do_probs(Es,Sum,Ps).

show_sorted([], _) :- nl.
show_sorted([I|VarOrder], Graph) :-
	arg(I,Graph,var(V,I,_,_,_,_,_,_,_)),		
	clpbn:get_atts(V,[key(K)]),
	format('~w ',[K]),
	show_sorted(VarOrder, Graph).
