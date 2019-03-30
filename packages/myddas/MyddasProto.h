/* MYDDAS */

#ifdef USE_MYDDAS

/* myddas_initialization.c */
extern MYDDAS_GLOBAL myddas_init_initialize_myddas(void);
extern MYDDAS_UTIL_CONNECTION
 myddas_init_initialize_connection(void *, void *, MYDDAS_API,
 MYDDAS_UTIL_CONNECTION);
extern MYDDAS_UTIL_PREDICATE myddas_init_initialize_predicate(const char *, int,
 const char *,
 MYDDAS_UTIL_PREDICATE);
extern int myddas_util_connection_type(void *con);

#ifdef MYDDAS_STATS
/* myddas_statistics.c */
extern MYDDAS_GLOBAL myddas_stats_initialize_global_stats(MYDDAS_GLOBAL);
extern MYDDAS_STATS_STRUCT myddas_stats_initialize_connection_stats(void);
extern void myddas_stats_delete_stats_list(MYDDAS_STATS_STRUCT);
#endif /* MYDDAS_STATS */

#ifdef MYDDAS_MYSQL
/* myddas_util.c */
extern void init_mysq( void );
extern void myddas_util_table_write(MYSQL_RES *);
extern MYDDAS_UTIL_CONNECTION myddas_util_add_connection(void *, void *, MYDDAS_API);
#endif
extern MYDDAS_UTIL_CONNECTION myddas_util_search_connection(void *con);
extern MYDDAS_UTIL_CONNECTION myddas_util_search_connection(void *);
extern void myddas_util_delete_connection(void *);
extern MYDDAS_UTIL_CONNECTION myddas_util_add_predicate(const char *, Int,
 const char *, void *);
extern MYDDAS_UTIL_PREDICATE myddas_util_search_predicate(const char *, Int,
 const char *);
extern void myddas_util_delete_predicate(MYDDAS_UTIL_PREDICATE);

/* Get's the number of queries to save */
extern UInt myddas_util_get_total_multi_queries_number(MYDDAS_UTIL_CONNECTION);
extern void myddas_util_set_total_multi_queries_number(MYDDAS_UTIL_CONNECTION, UInt);

extern void *myddas_util_get_list_pred(MYDDAS_UTIL_CONNECTION);
extern void *myddas_util_get_pred_next(void *);
extern const char *myddas_util_get_pred_module(void *);
extern const char *myddas_util_get_pred_name(void *);
extern MyddasInt myddas_util_get_pred_arity(void *);
// DELETE THIS WHEN DB_STATS  IS COMPLETED
extern MyddasInt get_myddas_top(void);

#ifdef DEBUG
extern void check_int(void);
#endif

#endif /* MYDDAS_MYSQL || MYDDAS_ODBC */

/* myddas_mysql.c */
#if defined MYDDAS_MYSQL
extern void Yap_InitMYDDAS_MySQLPreds(void);
extern void Yap_InitBackMYDDAS_MySQLPreds(void);
#endif

/* myddas_odbc.c */
#if defined MYDDAS_ODBC
extern void init_odbc( void );
extern void Yap_InitMYDDAS_ODBCPreds(void);
extern void Yap_InitBackMYDDAS_ODBCPreds(void);
#endif

/* myddas_postgres.c */
#if defined MYDDAS_POSTGRES
extern void init_postgres( void );
extern void Yap_InitMYDDAS_PostgresPreds(void);
extern void Yap_InitBackMYDDAS_PostgresPreds(void);
#endif

/* myddas_sqlite3.c */
#if defined MYDDAS_SQLITE3
extern void init_sqlite3( void );
#endif

/* Myddas_shared.c */
#if defined USE_MYDDAS
extern void Yap_MYDDAS_delete_all_myddas_structs(void);
extern void Yap_InitMYDDAS_SharedPreds(void);
extern void Yap_InitBackMYDDAS_SharedPreds(void);
#endif

/* myddas_top_level.c */
#if defined MYDDAS_TOP_LEVEL &&                                                \
 defined MYDDAS_MYSQL //&& defined HAVE_LIBREADLINE
extern void Yap_InitMYDDAS_TopLevelPreds(void);
#endif
