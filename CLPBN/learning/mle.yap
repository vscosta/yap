%
% This assumes we have a single big example.
%

:- use_module(library('clpbn_learning/utils'),
	      [run_all/1,
	       clpbn_vars/2]).

:- module(bnt_mle, [learn_parameters/2]).

%
% full evidence learning
%
learn_parameters(Items, Tables, Extras) :-
	run_all(Items),
	attributes:all_attvars(AVars),
	% sort and incorporte evidence
	clpbn_vars(AVars, AllVars),
	mk_sample(AllVars, Sample),
	compute_tables(Extras, Sample, Tables).

mk_sample(AllVars, NVars, LL) :-
	add2sample(AllVars, Sample),
	msort(Sample, AddL),
	compute_params(AddL, Parms).

add2sample([],  []).
add2sample([V|Vs],[val(Id,[Ev|EParents])|Vals]) :-
	clpbn:get_atts(V, [evidence(Ev),dist(Id,Parents)]),
	get_eparents(Parents, EParents),
	add2sample(Vs, Vals).

get_eparents([P|Parents], [E|EParents]) :-
	clpbn:get_atts(V, [evidence(Ev)]),
	get_eparents(Parents, EParents).
get_eparents([], []).


compute_tables([], Sample, Tables) :-
	mle(Sample, Tables).
compute_tables([laplace|_], Sample, Tables) :-
	laplace(Sample, Tables).
