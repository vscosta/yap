%
% Learn parameters using the BNT toolkit
%

:- yap_flag(unknown,error).

:- style_check(all).

:- module(bnt_parameters, [learn_parameters/2]).

:- use_module(library('clpbn'),
		[clpbn_flag/3]).

:- use_module(library('clpbn/bnt'),
		[create_bnt_graph/2]).

:- use_module(library('clpbn/display'),
		[clpbn_bind_vals/3]).

:- use_module(library('clpbn/dists'),
		[get_dist_domain/2]).

:- use_module(library(matlab),
		[matlab_initialized_cells/4,
		 matlab_call/2,
		 matlab_get_variable/2
		]).

:- dynamic bnt_em_max_iter/1.
bnt_em_max_iter(10).


% syntactic sugar for matlab_call.
:- op(800,yfx,<--).

G <-- Y :-
	matlab_call(Y,G).


learn_parameters(Items, Tables) :-
	run_all(Items),
	clpbn_flag(solver, OldSolver, bnt),
	clpbn_flag(bnt_model, Old, tied),
	attributes:all_attvars(AVars),
	% sort and incorporte evidence
	clpbn_vars(AVars, AllVars),
	length(AllVars,NVars),
	create_bnt_graph(AllVars, Reps),
	mk_sample(AllVars,NVars,EvVars),
	bnt_learn_parameters(NVars,EvVars),
	get_parameters(Reps, Tables),
	clpbn_flag(solver, bnt, OldSolver),
	clpbn_flag(bnt_model, tied, Old).

run_all([]).
run_all([G|Gs]) :-
	call(user:G),
	run_all(Gs).

clpbn_vars(Vs,BVars) :-
	get_clpbn_vars(Vs,CVs),
	keysort(CVs,KVs),
	merge_vars(KVs,BVars).

get_clpbn_vars([],[]).
get_clpbn_vars([V|GVars],[K-V|CLPBNGVars]) :-
	clpbn:get_atts(V, [key(K)]), !,
	get_clpbn_vars(GVars,CLPBNGVars).
get_clpbn_vars([_|GVars],CLPBNGVars) :-
	get_clpbn_vars(GVars,CLPBNGVars).

merge_vars([],[]).
merge_vars([K-V|KVs],[V|BVars]) :-
	get_var_has_same_key(KVs,K,V,KVs0),
	merge_vars(KVs0,BVars).

get_var_has_same_key([K-V|KVs],K,V,KVs0) :- !,
	get_var_has_same_key(KVs,K,V,KVs0).
get_var_has_same_key(KVs,_,_,KVs).


mk_sample(AllVars,NVars, LL) :-
	add2sample(AllVars, LN),
	length(LN,LL),
	matlab_initialized_cells( NVars, 1, LN, sample).

add2sample([], []).
add2sample([V|Vs],[val(VId,1,Val)|Vals]) :-
	clpbn:get_atts(V, [evidence(Ev),dist(Id,_)]), !,
	bnt:get_atts(V,[bnt_id(VId)]),
	get_dist_domain(Id, Domain),
	evidence_val(Ev,1,Domain,Val),
	add2sample(Vs, Vals).
add2sample([_V|Vs],Vals) :-
	add2sample(Vs, Vals).

evidence_val(Ev,Val,[Ev|_],Val) :- !.
evidence_val(Ev,I0,[_|Domain],Val) :-
	I1 is I0+1,
	evidence_val(Ev,I1,Domain,Val).

bnt_learn_parameters(_,_) :-
	engine <-- jtree_inf_engine(bnet),
%	engine <-- var_elim_inf_engine(bnet),
%	engine <-- gibbs_sampling_inf_engine(bnet),
%	engine <-- belprop_inf_engine(bnet),
%	engine <-- pearl_inf_engine(bnet),
	bnt_em_max_iter(MaxIters),
	[new_bnet, trace] <-- learn_params_em(engine, sample, MaxIters).


get_parameters([],[]).
get_parameters([Rep-v(_,_,_)|Reps],[CPT|CPTs]) :-
	get_new_table(Rep,CPT),
	get_parameters(Reps,CPTs).

get_new_table(Rep,CPT) :-
	s <-- struct(new_bnet.'CPD'({Rep})),
	matlab_get_variable( s.'CPT', CPT).


