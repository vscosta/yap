:- module(myddas,[
		  db_open/5,
		  db_open/4,
%		  db_open/2,
%		  db_open/1,
%		  db_open/0,
		  db_close/1,
		  db_close/0,

		  db_verbose/1,
		  db_module/1,
		  db_is_database_predicate/3,
		  %#ifdef MYDDAS_STATS
%		 db_stats/1,
%		  db_stats/2,
%		  db_stats_time/2,
		  %#endif
		 db_sql/2,
		  db_sql/3,
		  db_sql_select/3,
		  db_prolog_select/2,
		  db_prolog_select/3,
		  db_prolog_select_multi/3,
		  db_command/2,
		  db_assert/2,
		  db_assert/1,
		  db_create_table/3,
		  db_export_view/4,
		  db_update/2,
		  db_describe/2,
		  db_describe/3,
		  db_show_tables/2,
		  db_show_tables/1,
	  db_get_attributes_types/2,
		  db_get_attributes_types/3,
		  db_number_of_fields/2,
		  db_number_of_fields/3,
      % myddas_shared.c
      c_db_initialize_myddas/0,
      c_db_connection_type/2,
      c_db_add_preds/4,
      c_db_preds_conn/4,
      c_db_connection/1,
      c_db_check_if_exists_pred/3,
      c_db_delete_predicate/3,
      c_db_multi_queries_number/2,
/*      %#ifdef MYDDAS_STATS
      c_db_stats/2,
      c_db_stats_walltime/1,
      c_db_stats_translate/2,
      c_db_stats_time/2,
      %#endif
      %#ifdef DEBUG
      c_db_check/0,
      %#endif
		  %#ifdef MYDDAS_TOP_LEVEL
          db_top_level/4,
		  db_top_level/5,
		  db_datalog_select/3,
		  %#endif
*/
				% myddas_assert_predicates.ypp
				% myddas_mysql.ypp,
          		  db_multi_queries_number/2
]).


:- load_foreign_files([], ['YAPmyddas'], c_db_initialize_myddas).

:- use_module('myddas/myddas_sqlite3').
:- use_module('myddas/myddas_odbc').
:- use_module('myddas/myddas_postgres').
:- use_module('myddas/myddas_mysql').

:- reexport(myddas/myddas_assert_predicates,[
					db_import/2,
					db_import/3,
					db_view/2,
					db_view/3,
					db_insert/2,
					db_insert/3,
					db_abolish/2,
					db_listing/0,
					db_listing/1
	      ]).

:- meta_predicate db_import(+,+,:), db_import(+,:).



:- use_module(myddas/myddas_util_predicates,[
				      '$prolog2sql'/3,
				      '$create_multi_query'/3,
				      '$get_multi_results'/4,
				      '$process_sql_goal'/4,
				      '$process_fields'/3,
				      '$get_values_for_insert'/3,
				      '$make_atom'/2,
				      '$write_or_not'/1,
				      '$abolish_all'/1,
				      '$make_a_list'/2,
				      '$get_table_name'/2,
				      '$get_values_for_update'/4,
				      '$extract_args'/4,
/*				      #if MYDDAS_STATS
				      '$make_stats_list'/2,
				      #endif
*/				     '$lenght'/2
				     ]).

:- use_module(myddas/myddas_errors,[
			     '$error_checks'/1

			    ]).

:- use_module(myddas/myddas_prolog2sql,[
				 translate/3,
				 queries_atom/2
				]).

:- use_module(library(lists),[
		  append/3,
      member/2
	      ]).

:- set_prolog_flag(verbose,silent).

:- multifile user:file_search_path/2.

:- dynamic user:file_search_path/2.

user:file_search_path(dataset, C) :-
    user:library_directory(Dir),
    (   current_prolog_flag(windows, true)
    ->  atomic_list_concat([Dir,data], ;, C)
    ;
    atomic_list_concat([Dir,data], :, C)
    ).
user:file_search_path(dataset, '.').


:- multifile user:prolog_file_type/2.

:- dynamic user:prolog_file_type/2.

user:prolog_file_type( db, dataset ).

:- include( myddas/myddas_core ).
