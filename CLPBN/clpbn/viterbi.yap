
%:- style_check(all).

:- module(viterbi, [viterbi/5]).

:- use_module(library(lists),
	      [nth/3]).

:- use_module(library(clpbn), []).

:- attribute prob/1, emission/1, backp/1, ancestors/1.

viterbi(Start,End,Trace,Ticks,Slices) :-
	attributes:all_attvars(Vars0),
	group_vars_by_key_and_parents(Vars0,Ticks,Slices),
	init_viterbi(Start),
	viterbi_alg([Start|R],R),
	backtrace(Start,End,[],Trace).

group_vars_by_key_and_parents(AVars, NTicks, Slices) :-
	NTicks1 is NTicks+2,
	functor(Hashes,slots,NTicks1),
	NSlices is Slices+2,
	build_slices(0,NTicks1,NSlices,Hashes),
	get_keys(AVars, Hashes),
	get_parents(AVars, Hashes).

build_slices(NTicks,NTicks,_,_) :- !.
build_slices(I0,NTicks,NSlices,Hashes) :-
	functor(Slice,slices,NSlices),
	I is I0+1,
	arg(I,Hashes,Slice),
	build_slices(I,NTicks,NSlices,Hashes).

get_keys([], _).
get_keys([V|AVars], Trees) :-
	clpbn:get_atts(V, [key(K)]), !,
	arg(1,K,Time0),
	Time is Time0+1,
	arg(Time, Trees, Tree),
	make_key(K, TKey),
	arg(TKey, Tree, List),
	lookup(List, K, V),
	get_keys(AVars, Trees).
get_keys([_|AVars], Trees) :-  % may be non-CLPBN vars.
	get_keys(AVars, Trees).

get_parents([], _).
get_parents([V|AVars], Trees) :-
	clpbn:get_atts(V, [dist(D,T,Parents)]), !,
%clpbn:get_atts(V, [key(K)]), format('~w (~w): ~w~n',[V,K,Parents]),
	add_parents(Parents,V,D,T,Trees),
	get_parents(AVars, Trees).
get_parents([_|AVars], Trees) :-  % may be non-CLPBN vars.
	get_parents(AVars, Trees).

add_parents(Parents,V,D,T,Trees) :-
	transform_parents(Parents,NParents,Copy,Trees),
	( var(Copy) -> true ; clpbn:put_atts(V, [dist(D,T,NParents)]) ).

transform_parents([],[],_,_).
transform_parents([P|Parents0],[P|NParents],Copy,Trees) :-
	var(P), !,
	inc_ancestors(P),
	transform_parents(Parents0,NParents,Copy,Trees).
transform_parents([P|Parents0],[V|NParents],copy,Trees) :-
	arg(1,P,Time0),
	Time is Time0+1,
	arg(Time, Trees, Tree),
	make_key(P, TKey),
	arg(TKey, Tree, List),
	lookup(List, P, V),
	inc_ancestors(V),
	transform_parents(Parents0,NParents,copy,Trees).

inc_ancestors(P) :-
	get_atts(P,[ancestors(N)]), !,
	N1 is N+1,
%format('  ~w->~d:~n',[P,N1]),
	put_atts(P,[ancestors(N1)]).
inc_ancestors(P) :-
%format('  ~w->1:~n',[P]),
	put_atts(P,[ancestors(1)]).

make_key(T,K) :-
	arg(2,T,I), !,
	K is I+2.
make_key(_,1).

lookup(Tree, K, V) :- var(Tree), !,
	Tree = [[K|V]|_].
lookup([[K1|V]|_],K2,V) :- K1 == K2, !.
lookup([_|List],K,V) :-
	lookup(List,K,V).


init_viterbi(V) :-
	put_atts(V,[prob(0)]).

viterbi_alg(L0, Lf) :- L0 == Lf, !.
viterbi_alg([V|Vs], Rs) :-
%format('<< ~w~n',[V]),
	% get the current status
	get_atts(V,[prob(P0)]), !,
	clpbn:get_atts(V,[dist(_,trans(Probs),States)]),
	% adjust to consider emission probabilities
	adjust_for_emission(V, P0, Pf),
	propagate(Probs,States,Pf,V,Rs,NRs),
	viterbi_alg(Vs,NRs).

adjust_for_emission(V, P0, Pf) :-
	hmm:get_atts(V,[emission(P)]), !,
	Pf is P+P0,
	put_atts(V,[prob(Pf)]).
adjust_for_emission(_, P, P).

propagate([],[],_,_,Rs,Rs).
propagate([Prob|Probs],[State|States],Pf,V,Rs,Rs0) :-
%format('   ~w~n',[State]),
	get_atts(State,[prob(P0),ancestors(N)]), !,
	mprob(Pf,Prob,P),
	N1 is N-1,
	(P > P0 ->
	    put_atts(State,[prob(P),backp(V),ancestors(N1)])
	;
	    put_atts(State,[ancestors(N1)])
	),
	(N1 == 0 -> Rs = [State|NRs] ; Rs = NRs),
	propagate(Probs,States,Pf,V,NRs,Rs0).
propagate([Prob|Probs],[State|States],Pf,V,Rs,Rs0) :-
	get_atts(State,[ancestors(N)]), !,
	N1 is N-1,
	mprob(Pf,Prob,P),
	put_atts(State,[prob(P),backp(V),ancestors(N1)]),
	(N1 == 0 -> Rs = [State|NRs] ; Rs = NRs),
	propagate(Probs,States,Pf,V,NRs,Rs0).

backtrace(Start,Var,Trace,Trace) :- Start == Var, !.
backtrace(Start,Var,Trace0,Trace) :-
	get_atts(Var,[backp(V)]),
	clpbn:get_atts(Var, [key(K)]),
	backtrace(Start,V,[K|Trace0],Trace).

mprob(P0,P1,Pf) :- Pf is P0+P1.
