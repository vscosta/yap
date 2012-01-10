:- module(clpbn_fove,
          [fove/3,
           set_solver_parameter/2,
           init_fove_solver/4,
           run_fove_solver/3,
           finalize_fove_solver/1
          ]).

:- use_module(library(pfl), [
          factor/5,
	  skolem/2]).

%
% support fove method
%

fove([[]],_,_) :- !.
fove([QueryVars], AllVars, Output) :-
	init_fove_solver(_, AllVars, _, GraphicalNet),
	run_fove_solver([QueryVars], LPs, GraphicalNet),
	finalize_fove_solver(GraphicalNet),
	clpbn_bind_vals([QueryVars], LPs, Output).

%
% set up network, add evidence, and query all marginals at the same time?
%
init_fove_solver(_, AllAttVars, _, fove(ParNet, EvidenceVariables)) :-
	all_factors(Factors),
	all_domains(Domains),
	evidence_variables(AllAttVars, EvidenceVariables),
writeln(ev:EvidenceVariables),
	% c-code, just receives the par factors
	init_fove(Factors, Domains, ParNet).	

evidence_variables([], []).
evidence_variables(V.AllAttVars, [K:E|EvidenceVariables]) :-
	clpbn:get_atts(V,[key(K),evidence(E)]), !,
	evidence_variables(AllAttVars, EvidenceVariables).
evidence_variables(_V.AllAttVars, EvidenceVariables) :-
	evidence_variables(AllAttVars, EvidenceVariables).

all_domains(Domains) :-
	findall(X:Y, skolem(X,Y), Domains).



:- table all_factors/1.

%
% enumerate all par-factors and enumerate their domain as tuples.
%
% output is list of pf(
%        ID: an unique number
%        Ks: a list of keys, also known as the pf formula [a(X),b(Y),c(X,Y)]
%        Vs: the list of free variables [X,Y]
%        Phi: the table following usual CLP(BN) convention
%        Tuples: tuples with all ground bindings for variables in Vs, of the form [fv(x,y)]
%
all_factors(Factors) :-
	findall(F, is_factor(F), Factors).

is_factor(pf(Id, Ks, Vs, Phi, Tuples)) :-
	factor(Id, Ks, Vs, Table, Constraints),
	Table \= avg,
	gen_table(Table, Phi),
	all_tuples(Constraints, Vs, Tuples).

gen_table(Table, Phi) :-
	( is_list(Table)
          -> 
	  Phi = Table
          ;
	  call(user:Table, Phi)
        ).

all_tuples(Constraints, Tuple, Tuples) :-
	setof(Tuple, Constraints^run(Constraints), Tuples).

run([]).
run(Goal.Constraints) :-
	user:Goal,
	run(Constraints).

%
% ask probability of a single variable
%
run_fove_solver(QueryVars, LPs, fove(ParFactors, EvidenceVariables)) :-
	fove(QueryVars, EvidenceVariables, ParFactors, LPs).	



	


