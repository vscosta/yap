
%
% each variable is represented by a node in a binary tree.
% each node contains:
% key,
% current_value
% Markov Blanket
%

:- module(clpbn_gibbs,
	  [gibbs/3,
	   check_if_gibbs_done/1,
	   init_gibbs_solver/4,
	   run_gibbs_solver/3]).

:- use_module(library(rbtrees),
	      [rb_new/1,
	       rb_insert/4,
	       rb_lookup/3]).

:- use_module(library(lists),
	      [member/2,
	       append/3,
	       delete/3,
	       max_list/2,
	       sum_list/2]).

:- use_module(library(ordsets),
	      [ord_subtract/3]).

:- use_module(library('clpbn/matrix_cpt_utils'), [
	project_from_CPT/3,
	reorder_CPT/5,
	multiply_possibly_deterministic_factors/3,
	column_from_possibly_deterministic_CPT/3,
	normalise_possibly_deterministic_CPT/2,
	list_from_CPT/2]).

:- use_module(library('clpbn/utils'), [
	check_for_hidden_vars/3]).

:- use_module(library('clpbn/dists'), [
	get_possibly_deterministic_dist_matrix/5,
	get_dist_domain_size/2]).

:- use_module(library('clpbn/topsort'), [
	topsort/2]).

:- use_module(library('clpbn/display'), [
	clpbn_bind_vals/3]).

:- use_module(library('clpbn/connected'),
	      [
	       influences/4
	      ]).

:- dynamic gibbs_params/3.

:- dynamic explicit/1.

% arguments:
%
% list of output variables
% list of attributed variables
%
gibbs(LVs,Vs0,AllDiffs) :-
	init_gibbs_solver(LVs, Vs0, AllDiffs, Vs),
	run_gibbs_solver(LVs, LPs, Vs),
	clpbn_bind_vals(LVs,LPs,AllDiffs),
	clean_up.

init_gibbs_solver(GoalVs, Vs0, _, Vs) :-
	clean_up,
	term_variables(GoalVs, LVs),
	check_for_hidden_vars(Vs0, Vs0, Vs1),
	influences(Vs1, LVs, _, Vs2),
	sort(Vs2,Vs).

run_gibbs_solver(LVs, LPs, Vs) :-
	initialise(Vs, Graph, LVs, OutputVars, VarOrder),
	process(VarOrder, Graph, OutputVars, Estimates),
	sum_up_all(Estimates, LPs),
	clean_up.

initialise(LVs, Graph, GVs, OutputVars, VarOrder) :-
	init_keys(Keys0),
	gen_keys(LVs, 0, VLen, Keys0, Keys),
	functor(Graph,graph,VLen),
	graph_representation(LVs, Graph, 0, Keys, TGraph),
	compile_graph(Graph),
	topsort(TGraph, VarOrder),
%writeln(TGraph:VarOrder),
%	show_sorted(VarOrder, Graph),
	add_all_output_vars(GVs, Keys, OutputVars).

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
	clpbn:get_atts(V, [dist(Id,Parents)]),
	get_possibly_deterministic_dist_matrix(Id, Parents, _, Vals, Table),
	get_sizes(Parents, Szs),
	length(Vals,Sz),
	project_evidence_out([V|Parents],[V|Parents],Table,[Sz|Szs],Variables,NewTable),
	% all variables are parents
	propagate2parents(Variables, NewTable, Variables, Graph, Keys),
	graph_representation(Vs, Graph, I0, Keys, TGraph).
graph_representation([V|Vs], Graph, I0, Keys, [I-IParents|TGraph]) :-
	I is I0+1,
	clpbn:get_atts(V, [dist(Id,Parents)]),
	get_possibly_deterministic_dist_matrix(Id, Parents, _, Vals, Table),
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
	clpbn:get_atts(V, [key(K),dist(I,_)]),write(K:I),nl,
	write_pars(Parents).

get_sizes([], []).
get_sizes([V|Parents], [Sz|Szs]) :-
	clpbn:get_atts(V, [dist(Id,_)]),
	get_dist_domain_size(Id, Sz),
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
	project_from_CPT(V,tab(Table,Deps,Szs),tab(ITable,IDeps,ISzs)),
	project_evidence_out(Parents,IDeps,ITable,ISzs,NewDeps,NewTable).
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

fetch_all_parents([],_,Parents,Parents,Sizes,Sizes) :- !.
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
% do it dynamically
compile_var(_,_,_,_,_,_,_,_).

multiply_all(I,Parents,CPTs,Sz,Graph) :-
	markov_blanket_instance(Parents,Graph,Values),
	(
	    multiply_all(CPTs,Graph,Probs)
	->
	    store_mblanket(I,Values,Probs)
	;
	    throw(error(domain_error(bayesian_domain),gibbs_cpt(I,Parents,Values,Sz)))
	),
	fail.
multiply_all(I,_,_,_,_) :-
	assert(explicit(I)).

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

multiply_all([tabular(Table,_,Parents)|CPTs],Graph,Probs) :-
	fetch_parents(Parents, Graph, Vals),
	column_from_possibly_deterministic_CPT(Table,Vals,Probs0),
	multiply_more(CPTs,Graph,Probs0,Probs).

fetch_parents([], _, []).
fetch_parents([P|Parents], Graph, [Val|Vals]) :-
	arg(P,Graph,var(_,_,Val,_,_,_,_,_,_)),
	fetch_parents(Parents, Graph, Vals).
	
multiply_more([],_,Probs0,LProbs) :-
	normalise_possibly_deterministic_CPT(Probs0, Probs),
	list_from_CPT(Probs, LProbs0),
	accumulate_up_list(LProbs0, 0.0, LProbs).
multiply_more([tabular(Table,_,Parents)|CPTs],Graph,Probs0,Probs) :-
	fetch_parents(Parents, Graph, Vals),
	column_from_possibly_deterministic_CPT(Table, Vals, P0),
	multiply_possibly_deterministic_factors(Probs0, P0, ProbsI),
	multiply_more(CPTs,Graph,ProbsI,Probs).

accumulate_up_list([], _, []).
accumulate_up_list([P|LProbs], P0, [P1|L]) :-
	P1 is P0+P,
	accumulate_up_list(LProbs, P1, L).

	
store_mblanket(I,Values,Probs) :-
	recordz(mblanket,m(I,Values,Probs),_).

add_all_output_vars([], _, []).
add_all_output_vars([Vs|LVs], Keys, [Is|OutputVars]) :-
	add_output_vars(Vs, Keys, Is),
	add_all_output_vars(LVs, Keys, OutputVars).

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
	arg(I,Graph,var(_,I,_,_,Sz,_,_,_,_)),
	Pos is integer(random*Sz),
	arg(I,Chain,Pos),
	gen_sample(Vs,Graph,Chain).


init_estimates(0,_,_,[]) :- !.
init_estimates(NChains,OutputVars,Graph,[Est|Est0]) :-
	NChainsI is NChains-1,
	init_estimate_all_outvs(OutputVars,Graph,Est),
	init_estimates(NChainsI,OutputVars,Graph,Est0).

init_estimate_all_outvs([],_,[]).
init_estimate_all_outvs([Vs|OutputVars],Graph,[E|Est]) :-
	init_estimate(Vs, Graph, E),
	init_estimate_all_outvs(OutputVars,Graph,Est).

init_estimate([],_,[]).
init_estimate([V],Graph,[I|E0L]) :- !,
	arg(V,Graph,var(_,I,_,_,Sz,_,_,_,_)),
	gen_e0(Sz,E0L).
init_estimate(Vs,Graph,me(Is,Mults,Es)) :-
	generate_est_mults(Vs, Is, Graph, Mults, Sz),
	gen_e0(Sz,Es).


generate_est_mults([], [], _, [], 1).
generate_est_mults([V|Vs], [I|Is], Graph, [M0|Mults], M) :-
	arg(V,Graph,var(_,I,_,_,Sz,_,_,_,_)),
	generate_est_mults(Vs, Is, Graph, Mults, M0),
	M is M0*Sz.	
	
gen_e0(0,[]) :- !.
gen_e0(Sz,[0|E0L]) :-
	Sz1 is Sz-1,
	gen_e0(Sz1,E0L).

process_chains(0,_,F,F,_,_,Est,Est) :- !.
process_chains(ToDo,VarOrder,End,Start,Graph,Len,Est0,Estf) :-
%format('ToDo = ~d~n',[ToDo]),
	process_chains(Start,VarOrder,Int,Graph,Len,Est0,Esti),
% (ToDo mod 100 =:= 1 -> statistics,cvt2problist(Esti, Probs), Int =[S|_], format('did ~d: ~w~n ~w~n',[ToDo,Probs,S]) ; true),
	ToDo1 is ToDo-1,
	process_chains(ToDo1,VarOrder,End,Int,Graph,Len,Esti,Estf).


process_chains([], _, [], _, _,[],[]).
process_chains([Sample0|Samples0], VarOrder, [Sample|Samples], Graph, SampLen,[E0|E0s],[Ef|Efs]) :-
	functor(Sample,sample,SampLen),
	do_sample(VarOrder,Sample,Sample0,Graph),
% format('Sample = ~w~n',[Sample]),
	update_estimates(E0,Sample,Ef),
	process_chains(Samples0, VarOrder, Samples, Graph, SampLen,E0s,Efs).

do_sample([],_,_,_).
do_sample([I|VarOrder],Sample,Sample0,Graph) :-
	do_var(I,Sample,Sample0,Graph),
	do_sample(VarOrder,Sample,Sample0,Graph).

do_var(I,Sample,Sample0,Graph) :-
	( explicit(I) ->
	  arg(I,Graph,var(_,_,_,_,_,_,Parents,_,_)),
	  fetch_parents(Parents,I,Sample,Sample0,Args),
	  recorded(mblanket,m(I,Args,Vals),_)
	;
	  arg(I,Graph,var(_,_,_,_,_,CPTs,Parents,_,_)),
	  fetch_parents(Parents,I,Sample,Sample0,Bindings),
	  multiply_all_in_context(Parents,Bindings,CPTs,Graph,Vals)
	),
	X is random,
	pick_new_value(Vals,X,0,Val),
	arg(I,Sample,Val).

multiply_all_in_context(Parents,Args,CPTs,Graph,Vals) :-
	set_pos(Parents,Args,Graph),
	multiply_all(CPTs,Graph,Vals),
	assert(mall(Vals)), fail.
multiply_all_in_context(_,_,_,_,Vals) :-
	retract(mall(Vals)).

set_pos([],[],_).
set_pos([I|Is],[Pos|Args],Graph) :-
	arg(I,Graph,var(_,I,Pos,_,_,_,_,_,_)),
	set_pos(Is,Args,Graph).

fetch_parents([],_,_,_,[]).
fetch_parents([P|Parents],I,Sample,Sample0,[VP|Args]) :-
	arg(P,Sample,VP),
	nonvar(VP), !,
	fetch_parents(Parents,I,Sample,Sample0,Args).
fetch_parents([P|Parents],I,Sample,Sample0,[VP|Args]) :-
	arg(P,Sample0,VP),
	fetch_parents(Parents,I,Sample,Sample0,Args).

pick_new_value([V|Vals],X,I0,Val) :-
	( X < V ->
	  Val = I0
	;
	  I is I0+1,
	  pick_new_value(Vals,X,I,Val)
	).

update_estimates([],_,[]).
update_estimates([Est|E0],Sample,[NEst|Ef]) :-
	update_estimate(Est,Sample,NEst),
	update_estimates(E0,Sample,Ef).

update_estimate([I|E],Sample,[I|NE]) :-
	arg(I,Sample,V),
	update_estimate_for_var(V,E,NE).
update_estimate(me(Is,Mult,E),Sample,me(Is,Mult,NE)) :-
	get_estimate_pos(Is, Sample, Mult, 0, V),
	update_estimate_for_var(V,E,NE).

get_estimate_pos([], _, [], V, V).
get_estimate_pos([I|Is], Sample, [M|Mult], V0, V) :-
	arg(I,Sample,VV),
	VI is VV*M+V0,
	get_estimate_pos(Is, Sample, Mult, VI, V).

update_estimate_for_var(V0,[X|T],[X1|NT]) :-
	( V0 == 0 ->
	  X1 is X+1,
	  NT = T
	;
	  V1 is V0-1,
	  X1 = X,
	  update_estimate_for_var(V1,T,NT)
	).


check_if_gibbs_done(Var) :-
	get_atts(Var, [dist(_)]), !.

clean_up :-
	eraseall(mblanket),
	fail.
clean_up :-
	retractall(explicit(_)),
	fail.
clean_up.

gibbs_params(5,100,1000).

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

sum_up_all([[]|_], []).
sum_up_all([[C|MoreC]|Chains], [Dist|Dists]) :-
	extract_sums(Chains, CurrentChains, LeftChains),
	sum_up([C|CurrentChains], Dist),
	sum_up_all([MoreC|LeftChains], Dists).

extract_sums([], [], []).
extract_sums([[C|Chains]|MoreChains], [C|CurrentChains], [Chains|LeftChains]) :-
	extract_sums(MoreChains, CurrentChains, LeftChains).

sum_up([[_|Counts]|Chains], Dist) :-
	add_up(Counts,Chains, Add),
	normalise(Add, Dist).
sum_up([me(_,_,Counts)|Chains], Dist) :-
	add_up_mes(Counts,Chains, Add),
	normalise(Add, Dist).

add_up(Counts,[],Counts).
add_up(Counts,[[_|Cs]|Chains], Add) :-
	sum_lists(Counts, Cs, NCounts),
	add_up(NCounts, Chains, Add).

add_up_mes(Counts,[],Counts).
add_up_mes(Counts,[me(_,_,Cs)|Chains], Add) :-
	sum_lists(Counts, Cs, NCounts),
	add_up_mes(NCounts, Chains, Add).

sum_lists([],[],[]).	
sum_lists([Count|Counts], [C|Cs], [NC|NCounts]) :-
	NC is Count+C,
	sum_lists(Counts, Cs, NCounts).

normalise(Add, Dist) :-
	sum_list(Add, Sum),
	divide_list(Add, Sum, Dist).

divide_list([],  _, []).
divide_list([C|Add], Sum, [P|Dist]) :-
	P is C/Sum,
	divide_list(Add, Sum, Dist).



