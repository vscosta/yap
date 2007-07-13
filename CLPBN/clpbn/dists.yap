%
% distribution
%

:- module(clpbn_dist,[
	dist/1,
	dist/3,
	get_dist/4,
	get_dist_domain/2,
	get_dist_params/2,
	get_dist_domain_size/2,
	get_dist_tparams/2,
	dist_to_term/2
	]).

:- use_module(library(lists),[is_list/1]).


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

 :- dynamic id/1, db/6.

id(1).

new_id(Id) :-
	retract(id(Id)),
	Id1 is Id+1,
	assert(id(Id1)).


dist(V, Id, Parents) :-
	var(V), !,
	freeze(V, dist(V, Id, Parents)).
dist(p(Type, CPT, Parents), Id, FParents) :-
	when(
	 (ground(Type), ground(CPT))
	,
	 distribution(Type, CPT, Id, Parents, FParents)
	).
dist(p(Type, CPT), Id, FParents) :-
	  when(
	   (ground(Type), ground(CPT))
	,
	    distribution(Type, CPT, Id, [], FParents)
	).

distribution(bool, trans(CPT), Id, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([t,f], trans, Tab, Id).
distribution(bool, CPT, Id, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([t,f], tab, CPT, Id).
distribution(aminoacids, trans(CPT), Id, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([a,c,d,e,f,g,h,i,k,l,m,n,p,q,r,s,t,v,w,y], trans, Tab, Id).
distribution(aminoacids, CPT, Id, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([a,c,d,e,f,g,h,i,k,l,m,n,p,q,r,s,t,v,w,y], tab, CPT, Id).
distribution(dna, trans(CPT), Id, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([a,c,g,t], trans, Tab, Id).
distribution(dna, CPT, Id, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([a,c,g,t], tab, CPT, Id).
distribution(rna, trans(CPT), Id, Parents, FParents) :-
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist([a,c,g,u], trans, Tab, Id).
distribution(rna, CPT, Id, Parents, Parents) :-
	is_list(CPT), !,
	add_dist([a,c,g,u], tab, CPT, Id).
distribution(Domain, trans(CPT), Id, Parents, FParents) :-
	is_list(Domain),
	is_list(CPT), !,
	compress_hmm_table(CPT, Parents, Tab, FParents),
	add_dist(Domain, trans, Tab, Id).
distribution(Domain, CPT, Id, Parents, Parents) :-
	is_list(Domain),
	is_list(CPT), !,
	add_dist(Domain, tab, CPT, Id).

add_dist(Domain, Type, CPT, Id) :-
	db(Id, CPT, Type, Domain, _, _), !.
add_dist(Domain, Type, CPT, Id) :-
	length(CPT, CPTSize),
	length(Domain, DSize),
	new_id(Id),
	assert(db(Id, CPT, Type, Domain, CPTSize, DSize)).

%
% Often, * is used to code empty in HMMs.
%
compress_hmm_table([], [], [], []).
compress_hmm_table([*|L],[_|Parents],NL,NParents) :- !,
	compress_hmm_table(L,Parents,NL,NParents).
compress_hmm_table([Prob|L],[P|Parents],[Prob|NL],[P|NParents]) :-
	compress_hmm_table(L,Parents,NL,NParents).

dist(Id) :-
	db(Id, _, _, _, _, _).

get_dist(Id, Type, Domain, Tab) :-
	db(Id, Tab, Type, Domain, _, _).

get_dist_params(Id, Parms) :-
	db(Id, Parms, _, _, _, _).

get_dist_domain_size(Id, DSize) :-
	db(Id, _, _, _, _, DSize).

get_dist_domain(Id, Domain) :-
	db(Id, _, _, Domain, _, _).

get_dist_nparams(Id, NParms) :-
	db(Id, _, _, _, NParms, _).

dist_to_term(_Id,_Term).
