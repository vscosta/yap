/****************************************
  File:     tries.yap
  Author:   Ricardo Rocha
  Comments: Tries module for Yap Prolog
  version:  $ID$
****************************************/


/** @defgroup Tries Trie DataStructure
@ingroup YAPLibrary
@{

The next routines provide a set of utilities to create and manipulate
prefix trees of Prolog terms. Tries were originally proposed to
implement tabling in Logic Programming, but can be used for other
purposes. The tries will be stored in the Prolog database and can seen
as alternative to `assert` and `record` family of
primitives. Most of these utilities have been implemented in `C`
for efficiency. They are available through the
`use_module(library(tries))` command.

 
*/


/** @pred trie_check_entry(+ _Trie_,+ _Term_,- _Ref_) 



Succeeds if a variant of term  _Term_ is in trie  _Trie_. An handle
 _Ref_ gives a reference to the term.

 
*/
/** @pred trie_close(+ _Id_) 



Close trie with identifier  _Id_.

 
*/
/** @pred trie_close_all 



Close all available tries.

 
*/
/** @pred trie_get_entry(+ _Ref_,- _Term_) 


Unify  _Term_ with the entry for handle  _Ref_.

 
*/
/** @pred trie_load(+ _Trie_,+ _FileName_) 


Load trie  _Trie_ from the contents of file  _FileName_.

 
*/
/** @pred trie_max_stats(- _Memory_,- _Tries_,- _Entries_,- _Nodes_) 


Give maximal statistics on tries, including the amount of memory,
 _Memory_, the number of tries,  _Tries_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_.

 
*/
/** @pred trie_mode(? _Mode_) 



Unify  _Mode_ with trie operation mode. Allowed values are either
`std` (default) or `rev`.

 
*/
/** @pred trie_open(- _Id_) 



Open a new trie with identifier  _Id_.

 
*/
/** @pred trie_print(+ _Trie_) 


Print trie  _Trie_ on standard output.




 */
/** @pred trie_put_entry(+ _Trie_,+ _Term_,- _Ref_) 



Add term  _Term_ to trie  _Trie_. The handle  _Ref_ gives
a reference to the term.

 
*/
/** @pred trie_remove_entry(+ _Ref_) 



Remove entry for handle  _Ref_.

 
*/
/** @pred trie_remove_subtree(+ _Ref_) 



Remove subtree rooted at handle  _Ref_.

 
*/
/** @pred trie_save(+ _Trie_,+ _FileName_) 


Dump trie  _Trie_ into file  _FileName_.

 
*/
/** @pred trie_stats(- _Memory_,- _Tries_,- _Entries_,- _Nodes_) 


Give generic statistics on tries, including the amount of memory,
 _Memory_, the number of tries,  _Tries_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_.

 
*/
/** @pred trie_usage(+ _Trie_,- _Entries_,- _Nodes_,- _VirtualNodes_) 


Give statistics on trie  _Trie_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_, and the
number of  _VirtualNodes_.

 
*/
:- module(tries, [
		  trie_open/1,
		  trie_close/1,
		  trie_close_all/0,
		  trie_empty/1,
		  trie_mode/1,
		  trie_put_entry/3,
		  trie_check_entry/3,
		  trie_get_entry/2,
		  trie_get_first_entry/2,
		  trie_get_last_entry/2,
		  trie_traverse/2,
		  trie_traverse/3,
		  trie_remove_entry/1,
		  trie_remove_subtree/1,
		  trie_join/2,
		  trie_intersect/2,
		  trie_count_join/3,
		  trie_count_intersect/3,
		  trie_dup/2,
		  trie_save/2,
		  trie_load/2,
		  trie_stats/4,
		  trie_max_stats/4,
		  trie_usage/4,
		  trie_print/1,
		  open_trie/1,
		  close_trie/1,
		  close_all_tries/0,
		  put_trie_entry/4,
		  get_trie_entry/3,
		  remove_trie_entry/1,
		  print_trie/1,
          trie_traverse_mode/1,
          trie_disable_hash/0,
          trie_enable_hash/0,
          trie_traverse_first/2,
          trie_traverse_next/2,
          trie_to_list/2,
          trie_to_depth_breadth_trie/4,
          trie_to_depth_breadth_trie/6,
          trie_get_depth_breadth_reduction_entry/1,
          trie_get_depth_breadth_reduction_opt_level_count/2,
          trie_replace_nested_trie/3
          ]).

:- load_foreign_files([tries], [], init_tries).

trie_empty(Trie) :-
	trie_usage(Trie, 0, 0, _).

trie_dup(Trie, CopyTrie) :-
	trie_open(CopyTrie),
	trie_join(CopyTrie, Trie).

trie_traverse(Trie, Ref) :- 
	trie_traverse(Trie, 0, Ref).

trie_to_depth_breadth_trie(Trie, DepthBreadthTrie, FinalLabel, OptimizationLevel) :-
  integer(OptimizationLevel),
  trie_dup(Trie, CopyTrie),
  trie_open(DepthBreadthTrie),
  trie_depth_breadth(CopyTrie, DepthBreadthTrie, FinalLabel, OptimizationLevel, 0, _),
  trie_close(CopyTrie).

trie_to_depth_breadth_trie(Trie, DepthBreadthTrie, FinalLabel, OptimizationLevel, StartCounter, EndCounter) :-
  trie_depth_breadth(Trie, DepthBreadthTrie, FinalLabel, OptimizationLevel, StartCounter, EndCounter).
