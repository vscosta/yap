:- module(clpbn_display, [
	clpbn_bind_vals/3]).

:- use_module(library(lists),
	      [
	       member/2
	      ]).

:- use_module(dists, [get_dist_domain/2]).

:- attribute posterior/4.


%
% what is actually output
%
attribute_goal(V, G) :-
	clpbn:suppress_attribute_display(false),
	get_atts(V, [posterior(Vs,Vals,Ps,AllDiffs)]),
	massage_out(Vs, Vals, Ps, G, AllDiffs, V).

massage_out([], Ev, _, V=Ev, _, V) :- !.
massage_out(Vs, [D], [P], p(CEqs)=P, AllDiffs, _) :- !,
	gen_eqs(Vs,D,Eqs),
	add_alldiffs(AllDiffs,Eqs,CEqs).
massage_out(Vs, [D|Ds], [P|Ps], (p(CEqs)=P,G) , AllDiffs, V) :-
	gen_eqs(Vs,D,Eqs),
	add_alldiffs(AllDiffs,Eqs,CEqs),
	massage_out(Vs, Ds, Ps, G, AllDiffs, V).

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
%bind_vals([V],Ps) :- !,
%	clpbn:get_atts(V, [dist(Vals,_,_)]),
%	put_atts(V, posterior([V], Vals, Ps)).
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
	clpbn:get_atts(V, [dist(Id,_)]),
	get_dist_domain(Id,D),
	get_all_doms(Vs, Ds).

ms([], []).
ms([H|L], [El|Els]) :-
	member(El,H),
	ms(L, Els).

