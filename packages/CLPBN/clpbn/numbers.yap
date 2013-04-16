
:- module(clpbn_numbers,
		[keys_to_numbers/7,
		 keys_to_numbers/9,
		 lists_of_keys_to_ids/6
		]).

:- use_module(library(bhash)).
:- use_module(library(maplist)).

:- use_module(library(pfl),
		[skolem/2,
		 get_pfl_cpt/5
		]).

%
% convert key representation into numeric representation
% (+keys, +all factors, +all evidence, -ConvTable, -NextId, -FactorsWithIds, -EvidenceWithIds)
%
keys_to_numbers(AllKeys, Factors, Evidence, Hash, Id4, FactorIds, EvidenceIds) :-
	b_hash_new(Hash0),
	keys_to_numbers(AllKeys, Factors, Evidence, Hash0, Hash, 0, Id4, FactorIds, EvidenceIds).


keys_to_numbers(AllKeys, Factors, Evidence, Hash0, Hash4, Id0, Id4, FactorIds, EvidenceIds) :-
	foldl2(key_to_id, AllKeys, _Ids, Hash0, Hash1, Id0, Id1),
	foldl2(evidence_to_id, Evidence, EvidenceIds, Hash1, Hash2, Id1, Id2),
	foldl2(factor_to_id(Evidence), Factors, FactorIds, Hash2, Hash3, Id2, Id3),
	sort(AllKeys,SKeys), %% writeln(allSortedKeys:SKeys), writeln(''),
	foldl2(key_to_id, SKeys, _, Hash3, Hash4, Id3, Id4).

lists_of_keys_to_ids(QueryKeys, QueryIds, Hash0, Hash, Id0, Id) :-
	foldl2(list_of_keys_to_ids, QueryKeys, QueryIds, Hash0, Hash, Id0, Id).

list_of_keys_to_ids(List, IdList, Hash0, Hash, I0, I) :-
	foldl2(key_to_id, List, IdList, Hash0, Hash, I0, I).

key_to_id(Key, Id, Hash0, Hash0, I0, I0) :-
	b_hash_lookup(Key, Id, Hash0), !.
key_to_id(Key, I0, Hash0, Hash, I0, I) :-
	b_hash_insert(Hash0, Key, I0, Hash),
	I is I0+1.

factor_to_id(Ev, f(_, DistId, Keys), fn(Ids, Ranges, CPT, DistId, Keys), Hash0, Hash, I0, I) :-
	get_pfl_cpt(DistId, Keys, Ev, NKeys, CPT),
	foldl2(key_to_id, NKeys, Ids, Hash0, Hash, I0, I),
	maplist(get_range, Keys, Ranges).

get_range(_Id:K, Range) :- !,
	skolem(K,Domain),
	length(Domain,Range).
get_range(K, Range) :-
	skolem(K,Domain),
	length(Domain,Range).


evidence_to_id(Key=Ev, Id=Ev, Hash0, Hash0, I0, I0) :-
	b_hash_lookup(Key, Id, Hash0), !.
evidence_to_id(Key=Ev, I0=Ev, Hash0, Hash, I0, I) :-
	b_hash_insert(Hash0, Key, I0, Hash),
	I is I0+1.

