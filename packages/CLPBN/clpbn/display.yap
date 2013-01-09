
:- module(clpbn_display,
		[clpbn_bind_vals/3]).

:- use_module(library(lists),
		[member/2]).

:- use_module(library(clpbn/dists),
		[get_dist_domain/2]).

:- use_module(library(clpbn),
		[use_parfactors/1]).

:- use_module(library(maplist)).

:- use_module(library(atts)).

:- attribute posterior/4.


%
% what is actually output
%
attribute_goal(V, G) :-
	clpbn:suppress_attribute_display(false),
	get_atts(V, [posterior(Vs,Vals,Ps,AllDiffs)]),
	massage_out(Vs, Vals, Ps, G, AllDiffs, V).

massage_out([], _Ev, _, Out, _, _V) :- !,
	out_query_evidence(Out).
massage_out(Vs, [D], [P], O, AllDiffs, _) :- !,
	gen_eqs(Vs,D,Eqs),
	add_alldiffs(AllDiffs,Eqs,CEqs),
	out_query_evidence(Out),
	( Out = true -> O = (p(CEqs)=P) ; O = (p(CEqs)=P, Out) ).
massage_out(Vs, [D|Ds], [P|Ps], (p(CEqs)=P,G) , AllDiffs, V) :-
	gen_eqs(Vs,D,Eqs),
	add_alldiffs(AllDiffs,Eqs,CEqs),
	massage_out(Vs, Ds, Ps, G, AllDiffs, V).

out_query_evidence(Out) :-
	catch(b_getval(clpbn_query_variables, f(QVs,Evidence)), _, fail), !,
	foldl( process_qv(Evidence), QVs, [], OL),
	list_to_conj(OL, Out).
out_query_evidence(true).

process_qv(Evidence, V, L0, LF) :-
	clpbn:get_atts(V,[key(K)]),
	member(K=Ev, Evidence), !,
	pfl:skolem(K,Dom),
	foldl2( add_goal(V,Ev), Dom, 0, _, L0, LF ).
process_qv(_Ev, _V, L, L).

list_to_conj([], true).
list_to_conj([O], O) :- !.
list_to_conj([O|OL], (O,Out)) :-
	list_to_conj(OL, Out).

add_goal(V, Ev, DVal, Ev, I, L, [(p(V=DVal) = 1.0)|L]) :- !,
	I is Ev+1.
add_goal(V, _Ev, DVal, I0, I, L, [(p(V=DVal) = 0.0)|L]) :- !,
	I is I0+1.


gen_eqs([V], [D], (V=D)) :- !.
gen_eqs([V], D, (V=D)) :- !.
gen_eqs([V|Vs], [D|Ds], ((V=D),Eqs)) :-
	gen_eqs(Vs,Ds,Eqs).

add_alldiffs([],Eqs,Eqs) :- !.
add_alldiffs(AllDiffs,Eqs,(Eqs/alldiff(AllDiffs))).


clpbn_bind_vals([],[],_).
clpbn_bind_vals([Vs|MoreVs],[Ps|MorePs],AllDiffs) :-
	clpbn_bind_vals2(Vs, Ps, AllDiffs),
	clpbn_bind_vals(MoreVs,MorePs,AllDiffs).

clpbn_bind_vals2([],_,_) :- !.
% simple case, we want a distribution on a single variable.
clpbn_bind_vals2([V],Ps,AllDiffs) :-
	use_parfactors(on), !,
	clpbn:get_atts(V, [key(K)]),
	pfl:skolem(K,Vals),
	put_atts(V, posterior([V], Vals, Ps, AllDiffs)).
% complex case, we want a joint distribution, do it on a leader.
% should split on cliques ?
clpbn_bind_vals2(Vs,Ps,AllDiffs) :-
	get_all_combs(Vs, Vals),
	Vs = [V|_],
	put_atts(V, posterior(Vs, Vals, Ps, AllDiffs)).

get_all_combs(Vs, Vals) :-
	get_all_doms(Vs,Ds),
	findall(L,ms(Ds,L),Vals).

get_all_doms([], []).
get_all_doms([V|Vs], [D|Ds]) :-
	clpbn:get_atts(V, [dist(Id,_)]), !,
	get_dist_domain(Id,D),
	get_all_doms(Vs, Ds).
get_all_doms([V|Vs], [D|Ds]) :-
	clpbn:get_atts(V, [key(K)]),
	pfl:skolem(K,D),
	get_all_doms(Vs, Ds).

ms([], []).
ms([H|L], [El|Els]) :-
	member(El,H),
	ms(L, Els).

