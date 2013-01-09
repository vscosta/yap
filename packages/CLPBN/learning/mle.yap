%
% Maximum likelihood estimator and friends.
%
%
% This assumes we have a single big example.
%

:- module(clpbn_mle,
		[learn_parameters/2,
		 learn_parameters/3,
		 parameters_from_evidence/3
		]).

:- use_module(library('clpbn')).

:- use_module(library('clpbn/learning/learn_utils'),
		[run_all/1,
		 clpbn_vars/2,
		 normalise_counts/2,
		 soften_table/2,
		 normalise_counts/2
		]).

:- use_module(library('clpbn/dists'),
		[empty_dist/2,
		 dist_new_table/2
		]).

:- use_module(library(matrix),
		[matrix_inc/2]).


learn_parameters(Items, Tables) :-
	learn_parameters(Items, Tables, []).

%
% full evidence learning
%
learn_parameters(Items, Tables, Extras) :-
	run_all(Items),
	attributes:all_attvars(AVars),
	% sort and incorporate evidence
	clpbn_vars(AVars, AllVars),
	mk_sample(AllVars, Sample),
	compute_tables(Extras, Sample, Tables).

parameters_from_evidence(AllVars, Sample, Extras) :-
	mk_sample_from_evidence(AllVars, Sample),
	compute_tables(Extras, Sample, Tables).

mk_sample_from_evidence(AllVars, SortedSample) :-
	add_evidence2sample(AllVars, Sample),
	msort(Sample, SortedSample).

mk_sample(AllVars, SortedSample) :-
	add2sample(AllVars, Sample),
	msort(Sample, SortedSample).

%
% assumes we have full data, meaning evidence for every variable
%
add2sample([],  []).
add2sample([V|Vs],[val(Id,[Ev|EParents])|Vals]) :-
	clpbn:get_atts(V, [evidence(Ev),dist(Id,Parents)]),
	get_eparents(Parents, EParents),
	add2sample(Vs, Vals).

get_eparents([P|Parents], [E|EParents]) :-
	clpbn:get_atts(P, [evidence(E)]),
	get_eparents(Parents, EParents).
get_eparents([], []).


%
% assumes we ignore variables without evidence or without evidence
% on a parent!
%
add_evidence2sample([],  []).
add_evidence2sample([V|Vs],[val(Id,[Ev|EParents])|Vals]) :-
	clpbn:get_atts(V, [evidence(Ev),dist(Id,Parents)]),
	get_eveparents(Parents, EParents), !,
	add_evidence2sample(Vs, Vals).
add_evidence2sample([_|Vs],Vals) :-
	add_evidence2sample(Vs, Vals).

get_eveparents([P|Parents], [E|EParents]) :-
	clpbn:get_atts(P, [evidence(E)]),
	get_eparents(Parents, EParents).
get_eveparents([], []).


compute_tables(Parameters, Sample, NewTables) :-
	estimator(Sample, Tables),
	add_priors(Parameters, Tables, NewTables).

estimator([], []).
estimator([val(Id,Sample)|Samples], [NewDist|Tables]) :-
	empty_dist(Id, NewTable),
	id_samples(Id, Samples, IdSamples, MoreSamples),
	mle([Sample|IdSamples], NewTable),
	soften_table(NewTable, SoftenedTable),
	normalise_counts(SoftenedTable, NewDist),
	% replace matrix in distribution
	dist_new_table(Id, NewDist),
	estimator(MoreSamples, Tables).


id_samples(_, [], [], []).
id_samples(Id, [val(Id,Sample)|Samples], [Sample|IdSamples], MoreSamples) :- !,
	id_samples(Id, Samples, IdSamples, MoreSamples).
id_samples(_, Samples, [], Samples).

mle([Sample|IdSamples], Table) :-
	matrix_inc(Table, Sample),
	mle(IdSamples, Table).
mle([], _).

