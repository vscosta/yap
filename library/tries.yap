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
		  print_trie/1
          ]).

:- load_foreign_files([tries], [], init_tries).

trie_empty(Trie) :-
	trie_usage(Trie, 0, 0, _).

trie_dup(Trie, CopyTrie) :-
	trie_open(CopyTrie),
	trie_join(CopyTrie, Trie).

trie_traverse(Trie, Ref) :- 
	trie_traverse(Trie, 0, Ref).
