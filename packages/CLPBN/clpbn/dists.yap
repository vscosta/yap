%
% routines to manipulate distributions
%

:- module(clpbn_dist,
		[dist/1,
		 dist/4,
		 dists/1,
		 dist_new_table/2,
		 get_dist/4,
		 get_dist_matrix/5,
		 get_possibly_deterministic_dist_matrix/5,
		 get_dist_domain/2,
		 get_dist_domain_size/2,
		 get_dist_params/2,
		 get_dist_key/2,
		 get_dist_all_sizes/2,
		 get_evidence_position/3,
		 get_evidence_from_position/3,
		 dist_to_term/2,
		 empty_dist/2,
		 all_dist_ids/1,
		 randomise_all_dists/0,
		 randomise_dist/1,
		 uniformise_all_dists/0,
		 uniformise_dist/1,
		 reset_all_dists/0,
		 add_dist/6,
		 additive_dists/6
		]).

:- use_module(library(lists),
		[nth0/3,
		 append/3
		]).

:- use_module(library(clpbn),
		[use_parfactors/1]).

:- use_module(library(matrix),
		[matrix_new/4,
		 matrix_new/3,
		 matrix_to_list/2,
		 matrix_to_logs/1
		]).

:- use_module(library(clpbn/matrix_cpt_utils),
		[random_CPT/2,
		 uniform_CPT/2
		]).

/*
:- mode dist(+, -).

:- mode get_dist(+, -, -, -).

:- mode get_dist_params(+, -).

:- mode get_dist_domain_size(+, -).

:- mode get_dist_domain(+, -).

:- mode get_dist_nparams(+, -).

:- mode dist(?).

:- mode dist_to_term(+,-).
*/

/*******************************************

store stuff in a DB of the form:
	db(Id, Key, CPT, Type, Domain, CPTSize, DSize)

where Id is the id,
  Key is a skeleton of the key(main functor only)
  cptsize is the table size or -1,
  DSize is the domain size,
  Type is
    tab for tabular
    avg for average
    max for maximum
    min for minimum
    trans  for HMMs
    continuous
  Domain is
    a list of values
    bool for [t,f]
    aminoacids for [a,c,d,e,f,g,h,i,k,l,m,n,p,q,r,s,t,v,w,y]
    dna for [a,c,g,t]
    rna for [a,c,g,u]
    reals


********************************************/

 :- dynamic id/1.

id(1).

new_id(Id) :-
	retract(id(Id)),
	Id1 is Id+1,
	assert(id(Id1)).

reset_id :-
	retract(id(_)),
	assert(id(1)).

dists(X) :- id(X1), X is X1-1.

dist(V, Id, Key, Parents) :-
	dist_unbound(V, Culprit), !,
	when(Culprit, dist(V, Id, Key, Parents)).
dist(V, Id, Key, Parents) :-
	var(Key), !,
	when(Key, dist(V, Id, Key, Parents)).
dist(avg(Domain, Parents), avg(Domain), _, Parents).
dist(ip(Domain, Tabs, Parents), ip(Domain,Tabs), _, Parents).
dist(max(Domain, Parents), max(Domain), _, Parents).
dist(min(Domain, Parents), min(Domain), _, Parents).
dist(cons(Domain, Parent), cons(Domain), _, Parent).
dist(p(Type, CPT), Id, Key, FParents) :-
	copy_structure(Key, Key0),
	distribution(Type, CPT, Id, Key0, [], FParents).
dist(p(Type, CPT, Parents), Id, Key, FParents) :-
	copy_structure(Key, Key0),
	distribution(Type, CPT, Id, Key0, Parents, FParents).

dist_unbound(V, ground(V)) :-
	var(V), !.
dist_unbound(p(Type,_), ground(Type)) :-
	\+ ground(Type), !.
dist_unbound(p(_,CPT), ground(CPT)) :-
	\+ ground(CPT).
dist_unbound(p(Type,_,_), ground(Type)) :-
	\+ ground(Type), !.
dist_unbound(p(_,CPT,_), ground(CPT)) :-
	\+ ground(CPT).

distribution(bool, trans(CPT), Id, Key, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([t,f], trans, Tab, Parents, Key, Id).
distribution(bool, CPT, Id, Key, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([t,f], tab, CPT, Parents, Key, Id).
distribution(aminoacids, trans(CPT), Id, Key, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([a,c,d,e,f,g,h,i,k,l,m,n,p,q,r,s,t,v,w,y], trans, Tab, FParents, Key, Id).
distribution(aminoacids, CPT, Id, Key, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([a,c,d,e,f,g,h,i,k,l,m,n,p,q,r,s,t,v,w,y], tab, CPT, Parents, Key, Id).
distribution(dna, trans(CPT), Key, Id, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([a,c,g,t], trans, Tab, FParents, Key, Id).
distribution(dna, CPT, Id, Key, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([a,c,g,t], tab, CPT, Key, Id).
distribution(rna, trans(CPT), Id, Key, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents, FParents),
	add_dist([a,c,g,u], trans, Tab, Key, Id).
distribution(rna, CPT, Id, Key, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([a,c,g,u], tab, CPT, Parents, Key, Id).
distribution(Domain, trans(CPT), Id, Key, Parents, FParents) :-
	is_list(Domain),
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist(Domain, trans, Tab, FParents, Key, Id).
distribution(Domain, CPT, Id, Key, Parents, Parents) :-
	is_list(Domain),
	is_list(CPT), !,
	add_dist(Domain, tab, CPT, Parents, Key, Id).

add_dist(Domain, Type, CPT, _, Key, Id) :-
	recorded(clpbn_dist_db, db(Id, Key, CPT, Type, Domain, _, _), _), !.
add_dist(Domain, Type, CPT, Parents, Key, Id) :-
	length(CPT, CPTSize),
	length(Domain, DSize),
	new_id(Id),
	find_parent_sizes(Parents, Id, PSizes, [DSize|PSizes]),
	recordz(clpbn_dist_db,db(Id, Key, CPT, Type, Domain, CPTSize, DSize),_).


find_parent_sizes([], Id, [], DSizes) :-
	recordz(clpbn_dist_psizes,db(Id, DSizes),_).
find_parent_sizes([P|Parents], Id, [Size|Sizes], DSizes) :-
	integer(P), !,
	Size = P,
	find_parent_sizes(Parents, Id, Sizes, DSizes).
find_parent_sizes([P|Parents], Id, [Size|Sizes], DSizes) :-
	clpbn:get_atts(P,dist(Dist,_)), !,
	get_dist_domain_size(Dist, Size),
	find_parent_sizes(Parents, Id, Sizes, DSizes).
find_parent_sizes([_|_], _, _, _).

%
% Often, * is used to code empty in HMMs.
%
compress_hmm_table([], [], [], []).
compress_hmm_table([*|L],[_|Parents],NL,NParents) :- !,
	compress_hmm_table(L,Parents,NL,NParents).
compress_hmm_table([Prob|L],[P|Parents],[Prob|NL],[P|NParents]) :-
	compress_hmm_table(L,Parents,NL,NParents).

dist(Id) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, _, _, _), _).

get_dist(Id, Type, Domain, Tab) :-
	recorded(clpbn_dist_db, db(Id, _, Tab, Type, Domain, _, _), _).

get_dist_matrix(Id, Parents, Type, Domain, Mat) :-
	recorded(clpbn_dist_db, db(Id, _, Tab, Type, Domain, _, DomainSize), _),
	get_dsizes(Parents, Sizes, []),
	matrix_new(floats, [DomainSize|Sizes], Tab, Mat),
	matrix_to_logs(Mat).

get_possibly_deterministic_dist_matrix(Id, Parents, Type, Domain, Mat) :-
	recorded(clpbn_dist_db, db(Id, _, Tab, Type, Domain, _, DomainSize), _),
	get_dsizes(Parents, Sizes, []),
	matrix_new(floats, [DomainSize|Sizes], Tab, Mat).

get_dsizes([], Sizes, Sizes).
get_dsizes([P|Parents], [Sz|Sizes], Sizes0) :-
	clpbn:get_atts(P,dist(Dist,_)),
	get_dist_domain_size(Dist, Sz),
	get_dsizes(Parents, Sizes, Sizes0).

get_dist_params(Id, Parms) :-
	recorded(clpbn_dist_db, db(Id, _, Parms, _, _, _, _), _).

get_dist_all_sizes(Id, DSizes) :-
	recorded(clpbn_dist_psizes,db(Id, DSizes),_).

get_dist_domain_size(DistId, DSize) :-
	use_parfactors(on), !,
	pfl:get_pfl_parameters(DistId, _, Dist),
	length(Dist, DSize).
get_dist_domain_size(avg(D,_), DSize) :- !,
	length(D, DSize).
get_dist_domain_size(ip(D,_,_), DSize) :- !,
	length(D, DSize).
get_dist_domain_size(Id, DSize) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, _, _, DSize), _).

get_dist_domain(Id, Domain) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, Domain, _, _), _), !.
get_dist_domain(avg(Domain), Domain).

get_dist_key(Id, Key) :-
	use_parfactors(on), !,
	pfl:get_first_pvariable(Id, Key).
get_dist_key(Id, Key) :-
	recorded(clpbn_dist_db, db(Id, Key, _, _, _, _, _), _).

get_dist_nparams(Id, NParms) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, _, NParms, _), _).

get_evidence_position(El, ip(Domain,_,_), Pos) :- !,
	nth0(Pos, Domain, El), !.
get_evidence_position(El, avg(Domain), Pos) :- !,
	nth0(Pos, Domain, El), !.
get_evidence_position(El, Id, Pos) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, Domain, _, _), _),
	nth0(Pos, Domain, El), !.
get_evidence_position(El, Id, Pos) :-
	recorded(clpbn_dist_db, db(Id, _,  _, _, _, _, _), _), !,
	throw(error(domain_error(evidence,Id),get_evidence_position(El, Id, Pos))).
get_evidence_position(El, Id, Pos) :-
	throw(error(domain_error(no_distribution,Id),get_evidence_position(El, Id, Pos))).

get_evidence_from_position(El, Id, Pos) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, Domain, _, _), _),
	nth0(Pos, Domain, El), !.
get_evidence_from_position(El, Id, Pos) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, _, _, _), _), !,
	throw(error(domain_error(evidence,Id),get_evidence_from_position(El, Id, Pos))).
get_evidence_from_position(El, Id, Pos) :-
	throw(error(domain_error(no_distribution,Id),get_evidence_from_position(El, Id, Pos))).

dist_to_term(_Id,_Term).

empty_dist(Dist, TAB) :-
	use_parfactors(on), !,
	pfl:get_pfl_factor_sizes(Dist, DSizes),
	matrix_new(floats, DSizes, TAB).
empty_dist(Dist, TAB) :-
	recorded(clpbn_dist_psizes,db(Dist, DSizes),_), !,
	matrix_new(floats, DSizes, TAB).
empty_dist(Dist, TAB) :-
	throw(error(domain_error(no_distribution,Dist),empty_dist(Dist,TAB))).

dist_new_table(DistId, NewMat) :-
	use_parfactors(on), !,
	matrix_to_list(NewMat, List),
	pfl:new_pfl_parameters(DistId, _, List).
dist_new_table(Id, NewMat) :-
	matrix_to_list(NewMat, List),
	recorded(clpbn_dist_db, db(Id, Key, _, A, B, C, D), R),
	erase(R),
	recorda(clpbn_dist_db, db(Id, Key, List, A, B, C, D), _),
	fail.
dist_new_table(_, _).

copy_structure(V, _) :- attvar(V), !.
copy_structure(V, V) :- var(V), !.
copy_structure(V, _) :- primitive(V), !.
copy_structure(Key, Key0) :-
	Key =.. [A|LKey],
	copy_Lstructure(LKey, LKey0),
	Key0 =.. [A|LKey0].

copy_Lstructure([], []).
copy_Lstructure([H|LKey], [NH|LKey0]) :-
	copy_structure(H, NH),
	copy_Lstructure(LKey, LKey0).

randomise_all_dists :-
	randomise_dist(_),
	fail.
randomise_all_dists.

randomise_dist(Dist) :-
	(
	    use_parfactors(on)
	->
	    pfl:get_pfl_factor_sizes(Dist, DSizes)
	;
	    recorded(clpbn_dist_psizes, db(Dist,DSizes), _)
	),
	random_CPT(DSizes, NewCPT),
	dist_new_table(Dist, NewCPT).

uniformise_all_dists :-
	uniformise_dist(_),
	fail.
uniformise_all_dists.

uniformise_dist(Dist) :-
	(
	    use_parfactors(on)
	->
	    pfl:get_pfl_factor_sizes(Dist, DSizes)
	;
	    recorded(clpbn_dist_psizes, db(Dist,DSizes), _)
	),
	uniform_CPT(DSizes, NewCPT),
	dist_new_table(Dist, NewCPT).


reset_all_dists :-
	recorded(clpbn_dist_psizes, _, R),
	erase(R),
	fail.
reset_all_dists :-
	recorded(clpbn_dist_db, _, R),
	erase(R),
	fail.
reset_all_dists :-
	reset_id,
	fail.
reset_all_dists.


additive_dists(ip(Domain,Tabs1), ip(Domain,Tabs2), Parents1, Parents2, ip(Domain,Tabs), Parents) :-
	append(Tabs1, Tabs2, Tabs),
	append(Parents1, Parents2, Parents).

