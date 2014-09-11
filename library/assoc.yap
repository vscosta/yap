% This file has been included as an YAP library by Vitor Santos Costa, 1999

% Red-Black Implementation of Association Lists.

%   Note   : the keys should be bound, the associated values need not be.

/** @defgroup Association_Lists Association Lists
@ingroup YAPLibrary
@{

The following association list manipulation predicates are available
once included with the `use_module(library(assoc))` command. The
original library used Richard O'Keefe's implementation, on top of
unbalanced binary trees. The current code utilises code from the
red-black trees library and emulates the SICStus Prolog interface.

 
*/


/** @pred assoc_to_list(+ _Assoc_,? _List_) 


Given an association list  _Assoc_ unify  _List_ with a list of
the form  _Key-Val_, where the elements  _Key_ are in ascending
order.

 
*/
/** @pred del_assoc(+ _Key_, + _Assoc_, ? _Val_, ? _NewAssoc_) 


Succeeds if  _NewAssoc_ is an association list, obtained by removing
the element with  _Key_ and  _Val_ from the list  _Assoc_.

 
*/
/** @pred del_max_assoc(+ _Assoc_, ? _Key_, ? _Val_, ? _NewAssoc_) 


Succeeds if  _NewAssoc_ is an association list, obtained by removing
the largest element of the list, with  _Key_ and  _Val_ from the
list  _Assoc_.

 
*/
/** @pred del_min_assoc(+ _Assoc_, ? _Key_, ? _Val_, ? _NewAssoc_) 


Succeeds if  _NewAssoc_ is an association list, obtained by removing
the smallest element of the list, with  _Key_ and  _Val_
from the list  _Assoc_.

 
*/
/** @pred empty_assoc(+ _Assoc_) 


Succeeds if association list  _Assoc_ is empty.

 
*/
/** @pred gen_assoc(+ _Assoc_,? _Key_,? _Value_) 


Given the association list  _Assoc_, unify  _Key_ and  _Value_
with two associated elements. It can be used to enumerate all elements
in the association list.

 
*/
/** @pred get_assoc(+ _Key_,+ _Assoc_,? _Value_) 


If  _Key_ is one of the elements in the association list  _Assoc_,
return the associated value.

 
*/
/** @pred get_assoc(+ _Key_,+ _Assoc_,? _Value_,+ _NAssoc_,? _NValue_) 


If  _Key_ is one of the elements in the association list  _Assoc_,
return the associated value  _Value_ and a new association list
 _NAssoc_ where  _Key_ is associated with  _NValue_.

 
*/
/** @pred get_next_assoc(+ _Key_,+ _Assoc_,? _Next_,? _Value_)

If  _Key_ is one of the elements in the association list  _Assoc_,
return the next key,  _Next_, and its value,  _Value_.

 
*/
/** @pred get_prev_assoc(+ _Key_,+ _Assoc_,? _Next_,? _Value_) 


If  _Key_ is one of the elements in the association list  _Assoc_,
return the previous key,  _Next_, and its value,  _Value_.

 
*/
/** @pred is_assoc(+ _Assoc_) 


Succeeds if  _Assoc_ is an association list, that is, if it is a
red-black tree.

 
*/
/** @pred list_to_assoc(+ _List_,? _Assoc_) 


Given a list  _List_ such that each element of  _List_ is of the
form  _Key-Val_, and all the  _Keys_ are unique,  _Assoc_ is
the corresponding association list.

 
*/
/** @pred map_assoc(+ _Pred_,+ _Assoc_) 


Succeeds if the unary predicate name  _Pred_( _Val_) holds for every
element in the association list.

 
*/
/** @pred map_assoc(+ _Pred_,+ _Assoc_,? _New_)

Given the binary predicate name  _Pred_ and the association list
 _Assoc_,  _New_ in an association list with keys in  _Assoc_,
and such that if  _Key-Val_ is in  _Assoc_, and  _Key-Ans_ is in
 _New_, then  _Pred_( _Val_, _Ans_) holds.

 
*/
/** @pred max_assoc(+ _Assoc_,- _Key_,? _Value_) 


Given the association list
 _Assoc_,  _Key_ in the largest key in the list, and  _Value_
the associated value.

 
*/
/** @pred min_assoc(+ _Assoc_,- _Key_,? _Value_) 


Given the association list
 _Assoc_,  _Key_ in the smallest key in the list, and  _Value_
the associated value.

 
*/
/** @pred ord_list_to_assoc(+ _List_,? _Assoc_) 


Given an ordered list  _List_ such that each element of  _List_ is
of the form  _Key-Val_, and all the  _Keys_ are unique,  _Assoc_ is
the corresponding association list.

 
*/
/** @pred put_assoc(+ _Key_,+ _Assoc_,+ _Val_,+ _New_) 


The association list  _New_ includes and element of association
 _key_ with  _Val_, and all elements of  _Assoc_ that did not
have key  _Key_.




 */
:- module(assoc, [
        empty_assoc/1,
	assoc_to_list/2,
	is_assoc/1,
	min_assoc/3,
	max_assoc/3,
	gen_assoc/3,
	get_assoc/3,
	get_assoc/5,
	get_next_assoc/4,
	get_prev_assoc/4,
	list_to_assoc/2,
	ord_list_to_assoc/2,
	map_assoc/2,
	map_assoc/3,
	put_assoc/4,
	del_assoc/4,
		  assoc_to_keys/2,
	del_min_assoc/4,
	del_max_assoc/4
    ]).

:- meta_predicate map_assoc(2, +, -), map_assoc(1, +).

:- use_module(library(rbtrees), [
	rb_empty/1,
	rb_visit/2,
	is_rbtree/1,
	rb_min/3,
	rb_max/3,
	rb_in/3,
	rb_lookup/3,
	rb_update/5,
	rb_next/4,
	rb_previous/4,
	list_to_rbtree/2,
	ord_list_to_rbtree/2,
	rb_map/2,
	rb_map/3,
	rb_keys/2,
	rb_update/4,
	rb_insert/4,
	rb_delete/4,
	rb_del_min/4,
	rb_del_max/4
    ]).

empty_assoc(t).

assoc_to_list(t, L) :- !, L = [].
assoc_to_list(T, L) :-
	rb_visit(T, L).

is_assoc(t) :- !.
is_assoc(T) :-
	is_rbtree(T).

min_assoc(T,K,V) :-
	rb_min(T,K,V).

max_assoc(T,K,V) :-
	rb_max(T,K,V).

gen_assoc(T,K,V) :-
	rb_in(K,V,T).

get_assoc(K,T,V) :-
	rb_lookup(K,V,T).

get_assoc(K,T,V,NT,NV) :-
	rb_update(T,K,V,NV,NT).

get_next_assoc(K,T,KN,VN) :-
	rb_next(T,K,KN,VN).

get_prev_assoc(K,T,KP,VP) :-
	rb_previous(T,K,KP,VP).

list_to_assoc(L, T) :-
	list_to_rbtree(L, T).

ord_list_to_assoc(L, T) :-
	ord_list_to_rbtree(L, T).

map_assoc(t, _) :- !.
map_assoc(P, T) :-
	yap_flag(typein_module, M0),
	extract_mod(P, M0, M, G),
	functor(G, Name, 1),
	rb_map(T, M:Name).

map_assoc(t, T, T) :- !.
map_assoc(P, T, NT) :-
	yap_flag(typein_module, M0),
	extract_mod(P, M0, M, G),
	functor(G, Name, 2),
	rb_map(T, M:Name, NT).


extract_mod(G,_,_) :- var(G), !, fail.
extract_mod(M:G, _, FM, FG ) :- !,
	extract_mod(G, M, FM, FG ).
extract_mod(G, M, M, G ).

put_assoc(K, T, V, NT) :-
	rb_update(T, K, V, NT), !.
put_assoc(K, t, V, NT) :- !,
	rbtrees:rb_new(K,V,NT).
put_assoc(K, T, V, NT) :-
	rb_insert(T, K, V, NT).

del_assoc(K, T, V, NT) :-
	rb_delete(T, K, V, NT).

del_min_assoc(T, K, V, NT) :-
	rb_del_min(T, K, V, NT).

del_max_assoc(T, K, V, NT) :-
	rb_del_max(T, K, V, NT).


assoc_to_keys(T, Ks) :-
	rb_keys(T, Ks).


/**
@}
*/
