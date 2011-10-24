:- module(bphash, [new_hashtable/1,
   new_hashtable/2,
   is_hashtable/1,
   hashtable_get/3,
   hashtable_put/3,
   hashtable_register/3,
   hashtable_size/2,
   hashtable_to_list/2,
   hashtable_values_to_list/2,
   hashtable_keys_to_list/2]).

:- use_module(library(bhash), [b_hash_new/2,
    is_b_hash/1,
    b_hash_lookup/3,
    b_hash_insert/3,
    b_hash_size/2,
   b_hash_to_list/2,
   b_hash_values_to_list/2,
   b_hash_keys_to_list/2]).

new_hashtable(Hash) :-
	b_hash_new(Hash, 7).

new_hashtable(Hash, Size) :-
	b_hash_new(Hash, Size).

is_hashtable(Hash) :-
	is_b_hash(Hash).

hashtable_get(Hash, Key, Value) :-
	b_hash_lookup(Key, Value, Hash).

hashtable_put(Hash, Key, Value) :-
	b_hash_insert(Key, Value, Hash).

hashtable_register(Hash, Key, Value) :-
	b_hash_lookup(Key, Value0, Hash), !,
	Value0 = Value.
hashtable_register(Hash, Key, Value) :-
	b_hash_insert(Hash, Key, Value).

hashtable_size(Hash, Size) :-
	b_hash_size(Hash, Size).

hashtable_to_list(Hash, List) :-
	b_hash_to_list(Hash, List).

hashtable_keys_to_list(Hash, List) :-
	b_hash_keys_to_list(Hash, List).

hashtable_values_to_list(Hash, List) :-
	b_hash_values_to_list(Hash, List).




