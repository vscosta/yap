
:- module(discrete_utils,
		[project_from_CPT/3,
		 reorder_CPT/5,
		 get_dist_size/2
		]).

:- use_module(library(clpbn/dists),
		[get_dist_domain_size/2,
		 get_dist_domain/2
		]).
%
% remove columns from a table
%
project_from_CPT(V,f(Table,Deps,Szs),f(NewTable,NDeps,NSzs)) :-
	propagate_evidence(V,Evs),
	functor(Table,_,Max),
	find_projection_factor(Deps, V, NDeps, Szs, NSzs, F, Sz),
	OLoop is Max//(Sz*F),
	project_outer_loop(0,OLoop,F,Sz,Table,Evs,NTabl),
	NewTable =.. [t|NTabl].

propagate_evidence(V, Evs) :-
	clpbn:get_atts(V, [evidence(Ev),dist(Id,_)]), !,
	get_dist_domain(Id, Out),
	generate_szs_with_evidence(Out,Ev,0,Evs,Found),
	(var(Found) ->
	  clpbn:get_atts(V, [key(K)]),
	  throw(clpbn(evidence_does_not_match,K,Ev,[Out]))
	;
	  true
	).
propagate_evidence(_, _).

generate_szs_with_evidence([],_,_,[],_).
generate_szs_with_evidence([_|Out],Ev,Ev,[ok|Evs],found) :- !,
	I is Ev+1,
	generate_szs_with_evidence(Out,Ev,I,Evs,found).
generate_szs_with_evidence([_|Out],Ev,I0,[not_ok|Evs],Found) :-
	I is I0+1,
	generate_szs_with_evidence(Out,Ev,I,Evs,Found).

find_projection_factor([V|Deps], V1, Deps, [Sz|Szs], Szs, F, Sz) :-
	V == V1, !,
	mult(Szs, 1, F).
find_projection_factor([V|Deps], V1, [V|NDeps], [Sz|Szs], [Sz|NSzs], F, NSz) :-
	find_projection_factor(Deps, V1, NDeps, Szs, NSzs, F, NSz).

mult([], F, F).
mult([Sz|Szs], Sz0, F) :-
	SzI is Sz0*Sz,
	mult(Szs, SzI, F).

project_outer_loop(OLoop,OLoop,_,_,_,_,[]) :- !.
project_outer_loop(I,OLoop,F,Sz,Table,Evs,NTabl) :-
	Base is I*Sz*F,
	project_mid_loop(0,F,Base,Sz,Table,Evs,NTabl,NTabl0),
	I1 is I+1,
	project_outer_loop(I1,OLoop,F,Sz,Table,Evs,NTabl0).

project_mid_loop(F,F,_,_,_,_,NTabl,NTabl) :- !.
project_mid_loop(I,F,Base,Sz,Table,Evs,[Ent|NTablF],NTabl0) :-
	I1 is I+1,
	NBase is I+Base,
	project_inner_loop(0,Sz,Evs,NBase,F,Table,0.0,Ent),
	project_mid_loop(I1,F,Base,Sz,Table,Evs,NTablF,NTabl0).

project_inner_loop(Sz,Sz,[],_,_,_,Ent,Ent) :- !.
project_inner_loop(I,Sz,[ok|Evs],NBase,F,Table,Ent0,Ent) :- !,
	I1 is I+1,
	Pos is NBase+I*F+1,
	arg(Pos,Table,E1),
	Ent1 is E1+Ent0,
	project_inner_loop(I1,Sz,Evs,NBase,F,Table,Ent1,Ent).
project_inner_loop(I,Sz,[_|Evs],NBase,F,Table,Ent0,Ent) :- !,
	I1 is I+1,
	project_inner_loop(I1,Sz,Evs,NBase,F,Table,Ent0,Ent).

%
% Given a set of variables Vs0 and a discrete CPT T0,
% reorder according to keysort if Vs is unbound, or according to Vs
% resulting in CPT
% TF. Sizes of variables in Vs are given as Sizes.
%
reorder_CPT(Vs0, T0, Vs, TF, Sizes) :-
	var(Vs), !,
	get_sizes(Vs0, Szs),
	numb_vars(Vs0, Szs, _, VPs0, VLs0),
	keysort(VLs0, VLs),
	compute_new_factors(VLs, _, Vs, Sizes),
	get_factors(VLs0,Fs),
	length(T0,L),
	functor(TF,t,L),
	copy_to_new_array(T0, 0, VPs0, Fs, TF).
reorder_CPT(Vs0, T0, Vs, TF, Sizes) :-
	get_sizes(Vs0, Szs),
	numb_vars(Vs0, Szs, _, VPs0, VLs0),
	sort_according_to_parent(Vs, VLs0, VLs),
	compute_new_factors(VLs, _, Vs, Sizes),
	get_factors(VLs0,Fs),
	length(T0,L),
	functor(TF,t,L),
	copy_to_new_array(T0, 0, VPs0, Fs, TF).

numb_vars([], [], 1, [], []).
numb_vars([V|Vs], [L|Ls], A0, [Ai|VPs], [V-(L,_)|VLs]) :-
	numb_vars(Vs, Ls, Ai, VPs, VLs),
	A0 is Ai*L.

sort_according_to_parent([],[], []).
sort_according_to_parent([V|Vs],VLs0, [Arg|VLs]) :-
	fetch_var(V,VLs0,VLsI,Arg),
	sort_according_to_parent(Vs,VLsI, VLs).

fetch_var(V,[V0-(L,A)|VLs],VLs,V0-(L,A)) :- V == V0, !.
fetch_var(V,[A|VLs0],[A|VLsI],Arg) :-
	fetch_var(V,VLs0,VLsI,Arg).

compute_new_factors([], 1, [], []).
compute_new_factors([V-(L,F)|VLs], NF, [V|Vs], [L|Szs]) :-
	compute_new_factors(VLs, F, Vs, Szs),
	NF is F*L.

get_factors([],[]).
get_factors([_-(_,F)|VLs0],[F|Fs]) :-
	get_factors(VLs0,Fs).

copy_to_new_array([], _, _, _, _).
copy_to_new_array([P|Ps], I, F0s, Fs, S) :-
	convert_factor(F0s, Fs, I, N),
	I1 is I+1,
	N1 is N+1,
	arg(N1,S,P),
	copy_to_new_array(Ps, I1, F0s, Fs, S).

convert_factor([], [], _, 0).
convert_factor([F0|F0s], [F|Fs], I, OUT) :-
	X is I//F0,
	NI is I mod F0,
	NEXT is F*X,
	convert_factor(F0s, Fs, NI, OUT1),
	OUT is OUT1+NEXT.

get_sizes([], []).
get_sizes([V|Deps], [Sz|Sizes]) :-
	clpbn:get_atts(V, [dist(Id,_)]),
	get_dist_domain_size(Id,Sz),
	get_sizes(Deps, Sizes).

