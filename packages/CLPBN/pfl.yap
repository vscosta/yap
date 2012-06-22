%
% This module defines PFL, the prolog factor language.
%
%

:- module(pfl, [
          factor/6,
	  skolem/2,
	  defined_in_factor/2,
	  get_pfl_parameters/2, % given id return par factor parameter
	  new_pfl_parameters/2, % given id  set new parameters
	  get_first_pvariable/2, % given id get firt pvar (useful in bayesian)
	  get_factor_pvariable/2, % given id get any pvar
	  op(550,yfx,@),
	  op(550,yfx,::),
	  op(1150,fx,bayes),
	  op(1150,fx,markov)]).

:- use_module(library(lists),
	[nth0/3,
	 append/3,
	 member/2]).

:- dynamic factor/6, skolem_in/2, skolem/2, preprocess/3, evidence/2, id/1.

:- reexport(library(clpbn),
	[clpbn_flag/2 as pfl_flag,
	 set_clpbn_flag/2 as set_pfl_flag]).

:- reexport(library(clpbn/horus),
	[set_solver/1]).

:- ( % if clp(bn) has done loading, we're top-level
	predicate_property(set_pfl_flag(_,_), imported_from(clpbn))
    ->
	% we're using factor language
	% set appropriate flag
	set_pfl_flag(use_factors,on)
    ;
	% we're within clp(bn), no need to do anything
	true
    ).

user:term_expansion( bayes((Formula ; Phi ; Constraints)), pfl:factor(bayes,Id,FList,FV,Phi,Constraints)) :-
	!,
	term_variables(Formula, FreeVars),
	FV =.. [''|FreeVars],
	new_id(Id),
	process_args(Formula, Id, 0, _, FList, []).
user:term_expansion( markov((Formula ; Phi ; Constraints)), pfl:factor(markov,Id,FList,FV,Phi,Constraints)) :-
	!,
	term_variables(Formula, FreeVars),
	FV =.. [''|FreeVars],
	new_id(Id),
	process_args(Formula, Id, 0, _, FList, []).
user:term_expansion( Id@N, L ) :-
	atom(Id), number(N), !,
	N1 is N + 1,
	findall(G,generate_entity(1, N1, Id, G), L).
user:term_expansion( Goal, [] ) :-
	preprocess(Goal, Sk,Var), !,
	(ground(Goal) -> true ; throw(error('non ground evidence',Goal))),
%	prolog_load_context(module, M),
	assert(pfl:evidence(Sk,Var)).

Id@N :-
	generate_entity(0, N, Id, G),
	assert_static(user:G),
	fail.
_Id@_N.
	
defined_in_factor(Key, Factor) :-
	skolem_in(Key, Id),
	factor(bayes, Id, [Key|FList], FV, Phi, Constraints), !,
	Factor = factor(bayes, Id, [Key|FList], FV, Phi, Constraints).
defined_in_factor(Key, Factor) :-
	skolem_in(Key, Id),
	factor(markov, Id, FList, FV, Phi, Constraints),
	member(Key, FList),
	Factor = factor(markov, Id, FList, FV, Phi, Constraints).


generate_entity(N, N, _, _) :- !.
generate_entity(I0, _N, Id, T) :-
	atomic_concat(p, I0, P),
	T =.. [Id, P].
generate_entity(I0, N, Id, T) :-
	I is I0+1,
	generate_entity(I, N, Id, T).

id(0).

new_id(Id) :-
	retract(id(Id0)),
	Id is Id0+1,
	assert(id(Id)).

process_args((Arg1,Arg2), Id, I0, I ) --> !,
	process_args(Arg1, Id, I0, I1),
	process_args(Arg2, Id, I1, I).
process_args(Arg1, Id, I0, I ) -->
	{ I is I0+1 },
	process_arg(Arg1, Id, I).

process_arg(Sk::D, Id, _I) -->
	!,
	{
         new_skolem(Sk,D),
	 assert(skolem_in(Sk, Id))
        },
	[Sk].
process_arg(Sk, Id, _I) -->
	!,
	{
	 % if :: been used before for this skolem
	 % just keep on using it,
         % otherwise, assume it is t,f
         ( \+ \+ skolem(Sk,_D) -> true ; new_skolem(Sk,[t,f]) ),	
	 assert(skolem_in(Sk, Id))
        },
	[Sk].

new_skolem(Sk,D) :-
	copy_term(Sk, Sk1),
	skolem(Sk1, D1),
	Sk1 =@= Sk,
	!,
	( D1 = D -> true ; throw(pfl(permission_error(redefining_domain(Sk),D:D1)))).
new_skolem(Sk,D) :-
	interface_predicate(Sk),
	assert(skolem(Sk, D)).

interface_predicate(Sk) :-
	Sk =.. SKAs,
	append(SKAs, [Var], ESKAs),
	ESk =.. ESKAs,
	assert(preprocess(ESk, Sk, Var)),
	% transform from PFL to CLP(BN) call
	assert_static((user:ESk :-
	     evidence(Sk,Ev) -> Ev = Var;
	     var(Var) -> insert_atts(Var,Sk) ;
	     add_evidence(Sk,Var)
	  )
	).

insert_atts(Var,Sk) :-
	clpbn:put_atts(Var,[key(Sk)]).

add_evidence(Sk,Var) :-
	skolem(Sk,D),
	once(nth0(E,D,Var)),
	clpbn:put_atts(_V,[key(Sk),evidence(E)]).


get_pfl_parameters(Id,Out) :-
	factor(_Type,Id,_FList,_FV,Phi,_Constraints),
	%writeln(factor(_Type,Id,_FList,_FV,_Phi,_Constraints)),
	( is_list(Phi) -> Out = Phi ; call(user:Phi, Out) ).


new_pfl_parameters(Id, NewPhi) :-
	retract(factor(Type,Id,FList,FV,_Phi,Constraints)),
	assert(factor(Type,Id,FList,FV,NewPhi,Constraints)),
	fail.
new_pfl_parameters(_Id, _NewPhi).

get_pfl_factor_sizes(Id, DSizes) :-
	factor(_Type, Id, FList, _FV, _Phi, _Constraints),
	get_sizes(FList, DSizes).

get_sizes([], []).
get_sizes(Key.FList, Sz.DSizes) :-
	skolem(Key, Domain),
	length(Domain, Sz),
	get_sizes(FList, DSizes).
	
% only makes sense for bayesian networks
get_first_pvariable(Id,Var) :-
	factor(_Type, Id,Var._FList,_FV,_Phi,_Constraints).

% only makes sense for bayesian networks
get_factor_pvariable(Id,Var) :-
	factor(_Type, Id,FList,_FV,_Phi,_Constraints),
	member(Var, FList).

