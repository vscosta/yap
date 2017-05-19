/**
 * @file   itries.yap
 * @author Ricardo Rocha
 * @date   
 * 
 * @brief  Tries module for ILP
 * 
 * 
*/

/*********************************
  File:     itries.yap
  Author:   Ricardo Rocha
  Comments: Tries module for ILP
  version:  $ID$
*********************************/

:- module(itries, [
		  itrie_open/1,
		  itrie_close/1,
		  itrie_close_all/0,
                  itrie_mode/2,
		  itrie_timestamp/2,
		  itrie_put_entry/2,
		  itrie_update_entry/2,
		  itrie_check_entry/3,
		  itrie_get_entry/2,
		  itrie_get_data/2,
		  itrie_traverse/2,
		  itrie_remove_entry/1,
		  itrie_remove_subtree/1,
		  itrie_add/2,
		  itrie_subtract/2,
		  itrie_join/2,
		  itrie_intersect/2,
		  itrie_count_join/3,
		  itrie_count_intersect/3,
		  itrie_save/2,
		  itrie_save_as_trie/2,
		  itrie_load/2,
		  itrie_save2stream/2,
		  itrie_loadFromstream/2,
		  itrie_stats/4,
		  itrie_max_stats/4,
		  itrie_usage/4,
		  itrie_print/1
          ]).

:- load_foreign_files([itries], [], init_itries).
