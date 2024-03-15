/* MYDDAS */

#ifdef USE_MYDDAS

/* myddas_initialization.c */
X_API extern MYDDAS_GLOBAL myddas_init_initialize_myddas(void);
X_API extern MYDDAS_UTIL_CONNECTION
 myddas_init_initialize_connection(void *, void *, MYDDAS_API,
 MYDDAS_UTIL_CONNECTION);
X_API extern MYDDAS_UTIL_PREDICATE myddas_init_initialize_predicate(const char *, int,
 const char *,
 MYDDAS_UTIL_PREDICATE);
X_API extern int myddas_util_connection_type(void *con);

#ifdef MYDDAS_STATS
/* myddas_statistics.c */
X_API extern MYDDAS_GLOBAL myddas_stats_initialize_global_stats(MYDDAS_GLOBAL);
X_API extern MYDDAS_STATS_STRUCT myddas_stats_initialize_connection_stats(void);
X_API extern void myddas_stats_delete_stats_list(MYDDAS_STATS_STRUCT);
#endif /* MYDDAS_STATS */

#ifdef MYDDAS_MYSQL
/* myddas_util.c */
X_API extern void init_mysq( void );
X_API extern void myddas_util_table_write(MYSQL_RES *);
X_API extern myddas_util_add_connection(void *, void *, MYDDAS_API);
#endif
X_API extern MYDDAS_UTIL_CONNECTION myddas_util_add_connection(void *con, void *enviromment, MYDDAS_API api);
X_API extern MYDDAS_UTIL_CONNECTION myddas_util_search_connection(void *con);
X_API extern void myddas_util_delete_connection(void *);
  X_API extern MYDDAS_UTIL_CONNECTION myddas_util_add_predicate(const char *, Int,
 const char *, void *);
X_API extern MYDDAS_UTIL_PREDICATE myddas_util_search_predicate(const char *, Int,
 const char *);
X_API extern void myddas_util_delete_predicate(MYDDAS_UTIL_PREDICATE);

/* Get's the number of queries to save */
X_API extern UInt myddas_util_get_total_multi_queries_number(MYDDAS_UTIL_CONNECTION);
X_API extern void myddas_util_set_total_multi_queries_number(MYDDAS_UTIL_CONNECTION, UInt);

X_API extern void *myddas_util_get_list_pred(MYDDAS_UTIL_CONNECTION);
X_API extern void *myddas_util_get_pred_next(void *);
X_API extern const char *myddas_util_get_pred_module(void *);
X_API extern const char *myddas_util_get_pred_name(void *);
X_API extern MyddasInt myddas_util_get_pred_arity(void *);
// DELETE THIS WHEN DB_STATS  IS COMPLETED
X_API extern MyddasInt get_myddas_top(void);

#ifdef DEBUG
X_API extern void check_int(void);
#endif

#endif /* MYDDAS_MYSQL || MYDDAS_ODBC */

/* myddas_mysql.c */
#if defined MYDDAS_MYSQL
X_API extern void Yap_InitMYDDAS_MySQLPreds(void);
X_API extern void Yap_InitBackMYDDAS_MySQLPreds(void);
#endif

/* myddas_odbc.c */
#if defined MYDDAS_ODBC
X_API extern void init_odbc( void );
X_API extern void Yap_InitMYDDAS_ODBCPreds(void);
X_API extern void Yap_InitBackMYDDAS_ODBCPreds(void);
#endif

/* myddas_postgres.c */
#if defined MYDDAS_POSTGRES
X_API extern void init_postgres( void );
X_API extern void Yap_InitMYDDAS_PostgresPreds(void);
X_API extern void Yap_InitBackMYDDAS_PostgresPreds(void);
#endif

/* myddas_sqlite3.c */
#if defined MYDDAS_SQLITE3
X_API extern void init_sqlite3( void );
#endif

/* Myddas_shared.c */
#if defined USE_MYDDAS
X_API extern void Yap_MYDDAS_delete_all_myddas_structs(void);
X_API extern void Yap_InitMYDDAS_SharedPreds(void);
X_API extern void Yap_InitBackMYDDAS_SharedPreds(void);
#endif

/* myddas_top_level.c */
#if defined MYDDAS_TOP_LEVEL &&                                                \
 defined MYDDAS_MYSQL //&& defined HAVE_LIBREADLINE
X_API extern void Yap_InitMYDDAS_TopLevelPreds(void);
#endif
