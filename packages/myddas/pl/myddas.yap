:- module(myddas,[
		  db_open/5,
		  db_open/4,
		  db_open/2,
		  db_open/1,
		  db_open/0,
		  db_close/1,
		  db_close/0,

		  db_verbose/1,
		  db_module/1,
		  db_is_database_predicate/3,
		  %#ifdef MYDDAS_STATS
		 db_stats/1,
		  db_stats/2,
		  db_stats_time/2,
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
      c_db_connection_type/2,
      c_db_add_preds/4,
      c_db_preds_conn/4,
      c_db_connection/1,
      c_db_check_if_exists_pred/3,
      c_db_delete_predicate/2,
      c_db_multi_queries_number/2,
      %#ifdef MYDDAS_STATS
      c_db_stats/2,
      c_db_stats_walltime/1,
      c_db_stats_translate/2,
      c_db_stats_time/2,
      %#endif
      %#ifdef DEBUG
      c_db_check/0,
      %#endif
      c_db_initialize_myddas/0,
      c_db_connection_type/2,
      c_db_add_preds/4,
      c_db_preds_conn/4,
      c_db_connection/1,
      c_db_check_if_exists_pred/3,
      c_db_delete_predicate/2,
      c_db_multi_queries_number/2,
      %#ifdef MYDDAS_STATS
      c_db_stats/2,
      c_db_stats_walltime/1,
      c_db_stats_translate/2,
      c_db_stats_time/2,
      %#endif
      %#ifdef DEBUG
      c_db_check/0,
      %#endif
				% myddas_top_level.ypp
		  %#ifdef MYDDAS_TOP_LEVEL
          db_top_level/4,
		  db_top_level/5,
		  db_datalog_select/3,
		  %#endif
				% myddas_assert_predicates.ypp
				% myddas_mysql.ypp,
          		  db_multi_queries_number/2
]).
:- include( myddas/myddas_core ).
