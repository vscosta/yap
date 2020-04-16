/**
 * @file   tries.yap
 * @author Ricardo Rocha
 * 
 * @brief YAP tries interface  
 * 
 * 
*/
/****************************************
  File:     tries.yap
  Author:   Ricardo Rocha
  Comments: Tries module for Yap Prolog
  version:  $ID$
****************************************/

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

/** @defgroup tries Trie DataStructure
@ingroup library
@{

@brief Engine Independent trie library

The next routines provide a set of utilities to create and manipulate
prefix trees of Prolog terms. Tries were originally proposed to
implement tabling in Logic Programming, but can be used for other
purposes. The tries will be stored in the Prolog database and can seen
as alternative to `assert` and `record` family of
primitives. Most of these utilities have been implemented in `C`
for efficiency. They are available through the
`use_module(library(tries))` command.

 
*/



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

%% @}

