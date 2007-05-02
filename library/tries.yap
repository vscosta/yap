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
		  trie_mode/1,
		  trie_put_entry/3,
		  trie_check_entry/3,
		  trie_get_entry/2,
		  trie_remove_entry/1,
		  trie_remove_subtree/1,
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
