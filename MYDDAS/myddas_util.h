#ifndef __MYDDAS_UTIL_H__
#define __MYDDAS_UTIL_H__

#include <stdio.h>
#ifdef MYDDAS_ODBC
#include <sql.h>
#endif

#ifdef MYDDAS_MYSQL
#include <mysql/mysql.h>
#endif

#ifdef MYDDAS_STATS
#include <time.h>
#include <sys/time.h>
#endif

typedef struct myddas_global *MYDDAS_GLOBAL;
typedef struct myddas_util_query *MYDDAS_UTIL_QUERY;
typedef struct myddas_list_connection *MYDDAS_UTIL_CONNECTION;
typedef struct myddas_list_preds *MYDDAS_UTIL_PREDICATE;

#if defined MYDDAS_STATS || defined MYDDAS_TOP_LEVEL
typedef struct myddas_stats_time_struct *MYDDAS_STATS_TIME;
#endif

#ifdef MYDDAS_STATS
typedef struct myddas_global_stats *MYDDAS_GLOBAL_STATS;
#endif

MYDDAS_GLOBAL
myddas_util_initialize_myddas(void);

#ifdef MYDDAS_MYSQL
void
myddas_util_table_write(MYSQL_RES *);
#endif

/* Returns the connection type (mysql -> 1  or odbc -> 2) */
short int 
myddas_util_connection_type(void *);
/* Adds a connection identifier to the MYDDAS connections list*/
MYDDAS_UTIL_CONNECTION 
myddas_util_add_connection(void *,void *);
/* Search for the node of the specified connection*/
MYDDAS_UTIL_CONNECTION 
myddas_util_search_connection(void *);
/* Deletes a connection node from the MYDDAS connections list*/
void 
myddas_util_delete_connection(void *);

/* Adds a new predicate to it's connection node list*/
MYDDAS_UTIL_CONNECTION 
myddas_util_add_predicate(char *,int , char *,void *);
/* Search for a predicate node*/
MYDDAS_UTIL_PREDICATE
myddas_util_search_predicate(char *,int , char *);
/* Deletes predicate from the prediate list */
void
myddas_util_delete_predicate(MYDDAS_UTIL_PREDICATE);

/* Get's the number of queries to save */
unsigned long
myddas_util_get_total_multi_queries_number(MYDDAS_UTIL_CONNECTION);
void
myddas_util_set_total_multi_queries_number(MYDDAS_UTIL_CONNECTION,unsigned long);

#ifdef MYDDAS_ODBC
/* Return enviromment identifier*/
SQLHENV
myddas_util_get_odbc_enviromment(SQLHDBC);
#endif 

void *
myddas_util_get_list_pred(MYDDAS_UTIL_CONNECTION);
void *
myddas_util_get_pred_next(void *);
char *
myddas_util_get_pred_module(void *);
char *
myddas_util_get_pred_name(void *);
int 
myddas_util_get_pred_arity(void *);


#ifdef DEBUG
void check_int(void);
#endif


//DELETE THIS WHEN DB_STATS  IS COMPLETED
int
get_myddas_top(void);

#endif /*__MYDDAS_UTIL_H__*/
