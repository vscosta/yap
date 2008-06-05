%
% routines to manipulate distributions
%

:- module(clpbn_dist,
	  [
	   dist/1,
	   dist/3,
	   dists/1,
	   dist_new_table/2,
	   get_dist/4,
	   get_dist_matrix/5,
	   get_dist_domain/2,
	   get_dist_params/2,
	   get_dist_domain_size/2,
	   get_dist_tparams/2,
	   get_evidence_position/3,
	   get_evidence_from_position/3,
	   dist_to_term/2,
	   empty_dist/2,
	   dist_new_table/2
	]).

:- use_module(library(lists),[is_list/1,nth0/3]).

:- use_module(library(matrix),
	      [matrix_new/4,
	       matrix_new/3,
	       matrix_to_list/2,
	       matrix_to_logs/1]).

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
	db(Id, CPT, Type, Domain, CPTSize, DSize)

where Id is the id,
	cptsize is the table size or -1,
	DSize is the domain size,
	Type is
	     tab for tabular
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

dists(X) :- id(X1), X is X1-1.

dist(V, Id, Parents) :-
	dist_unbound(V, Culprit), !,
	when(Culprit, dist(V, Id, Parents)).
dist(p(Type, CPT), Id, FParents) :-
	distribution(Type, CPT, Id, [], FParents).
dist(p(Type, CPT, Parents), Id, FParents) :-
	distribution(Type, CPT, Id, Parents, FParents).

dist_unbound(V, ground(V)) :-
	var(V), !.
dist_unbound(p(Type,CPT), ground(Type)) :-
	\+ ground(Type), !.
dist_unbound(p(_,CPT), ground(CPT)) :-
	\+ ground(CPT).
dist_unbound(p(Type,CPT,_), ground(Type)) :-
	\+ ground(Type), !.
dist_unbound(p(_,CPT,_), ground(CPT)) :-
	\+ ground(CPT).

distribution(bool, trans(CPT), Id, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([t,f], trans, Tab, Parents, Id).
distribution(bool, CPT, Id, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([t,f], tab, CPT, Parents, Id).
distribution(aminoacids, trans(CPT), Id, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([a,c,d,e,f,g,h,i,k,l,m,n,p,q,r,s,t,v,w,y], trans, Tab, FParents, Id).
distribution(aminoacids, CPT, Id, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([a,c,d,e,f,g,h,i,k,l,m,n,p,q,r,s,t,v,w,y], tab, CPT, Parents, Id).
distribution(dna, trans(CPT), Id, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([a,c,g,t], trans, Tab, FParents, Id).
distribution(dna, CPT, Id, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([a,c,g,t], tab, CPT, Id).
distribution(rna, trans(CPT), Id, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents, FParents),
	add_dist([a,c,g,u], trans, Tab, Id).
distribution(rna, CPT, Id, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([a,c,g,u], tab, CPT, Parents, Id).
distribution(Domain, trans(CPT), Id, Parents, FParents) :-
	is_list(Domain),
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist(Domain, trans, Tab, FParents, Id).
distribution(Domain, CPT, Id, Parents, Parents) :-
	is_list(Domain),
	is_list(CPT), !,
	add_dist(Domain, tab, CPT, Parents, Id).

add_dist(Domain, Type, CPT, _, Id) :-
	recorded(clpbn_dist_db, db(Id, CPT, Type, Domain, _, _), _), !.
add_dist(Domain, Type, CPT, Parents, Id) :-
	length(CPT, CPTSize),
	length(Domain, DSize),
	new_id(Id),
	record_parent_sizes(Parents, Id, PSizes, [DSize|PSizes]),
	recordz(clpbn_dist_db,db(Id, CPT, Type, Domain, CPTSize, DSize),_).


record_parent_sizes([], Id, [], DSizes) :-
	recordz(clpbn_dist_psizes,db(Id, DSizes),_).
record_parent_sizes([P|Parents], Id, [Size|Sizes], DSizes) :-
	clpbn:get_atts(P,dist(Dist,_)), !,
	get_dist_domain_size(Dist, Size),
	record_parent_sizes(Parents, Id, Sizes, DSizes).
record_parent_sizes([_|_], _, _, _).

%
% Often, * is used to code empty in HMMs.
%
compress_hmm_table([], [], [], []).
compress_hmm_table([*|L],[_|Parents],NL,NParents) :- !,
	compress_hmm_table(L,Parents,NL,NParents).
compress_hmm_table([Prob|L],[P|Parents],[Prob|NL],[P|NParents]) :-
	compress_hmm_table(L,Parents,NL,NParents).

dist(Id) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, _, _), _).

get_dist(Id, Type, Domain, Tab) :-
	recorded(clpbn_dist_db, db(Id, Tab, Type, Domain, _, _), _).

get_dist_matrix(Id, Parents, Type, Domain, Mat) :-
	recorded(clpbn_dist_db, db(Id, Tab, Type, Domain, _, DomainSize), _),
	get_dsizes(Parents, Sizes, []),
	matrix_new(floats, [DomainSize|Sizes], Tab, Mat),
	matrix_to_logs(Mat).

get_dsizes([], Sizes, Sizes).
get_dsizes([P|Parents], [Sz|Sizes], Sizes0) :-
	clpbn:get_atts(P,dist(Dist,_)),
	get_dist_domain_size(Dist, Sz),
	get_dsizes(Parents, Sizes, Sizes0).

get_dist_params(Id, Parms) :-
	recorded(clpbn_dist_db, db(Id, Parms, _, _, _, _), _).

get_dist_domain_size(Id, DSize) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, _, DSize), _).

get_dist_domain(Id, Domain) :-
	recorded(clpbn_dist_db, db(Id, _, _, Domain, _, _), _).

get_dist_nparams(Id, NParms) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, NParms, _), _).

get_evidence_position(El, Id, Pos) :-
	recorded(clpbn_dist_db, db(Id, _, _, Domain, _, _), _),
	nth0(Pos, Domain, El), !.
get_evidence_position(El, Id, Pos) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, _, _), _), !,
	throw(error(domain_error(evidence,Id),get_evidence_position(El, Id, Pos))).
get_evidence_position(El, Id, Pos) :-
	throw(error(domain_error(no_distribution,Id),get_evidence_position(El, Id, Pos))).

get_evidence_from_position(El, Id, Pos) :-
	recorded(clpbn_dist_db, db(Id, _, _, Domain, _, _), _),
	nth0(Pos, Domain, El), !.
get_evidence_from_position(El, Id, Pos) :-
	recorded(clpbn_dist_db, db(Id, _, _, _, _, _), _), !,
	throw(error(domain_error(evidence,Id),get_evidence_from_position(El, Id, Pos))).
get_evidence_from_position(El, Id, Pos) :-
	throw(error(domain_error(no_distribution,Id),get_evidence_from_position(El, Id, Pos))).

dist_to_term(_Id,_Term).

empty_dist(Dist, TAB) :-
	recorded(clpbn_dist_psizes,db(Dist, DSizes),_), !,
	matrix_new(floats, DSizes, TAB).
empty_dist(Dist, TAB) :-
	throw(error(domain_error(no_distribution,Dist),empty_dist(Dist,TAB))).

dist_new_table(Id, NewMat) :-
	matrix_to_list(NewMat, List),
	recorded(clpbn_dist_db, db(Id, _, A, B, C, D), R),
	erase(R),
	recorda(clpbn_dist_db, db(Id, List, A, B, C, D), R),
	fail.
dist_new_table(_, _).


