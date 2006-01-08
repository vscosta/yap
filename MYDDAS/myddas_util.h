#ifndef __MYDDAS_UTIL_H__
#define __MYDDAS_UTIL_H__

#include <stdio.h>
#ifdef MYDDAS_ODBC
#include <sql.h>
#endif

#ifdef MYDDAS_STATS
#include <time.h>
#include <sys/time.h>
#endif

typedef struct myddas_global *MYDDAS_GLOBAL;
#ifdef MYDDAS_STATS
typedef struct myddas_global_stats *MYDDAS_GLOBAL_STATS;
#endif
typedef struct list_connection *MYDDAS_UTIL_CONNECTION;
typedef struct list_preds *MYDDAS_UTIL_PREDICATE;
typedef struct myddas_temp_tables *MYDDAS_TEMP_TABLES;


char * 
myddas_util_delete_all_temp_table(void);
char *
myddas_util_get_table_name(void *);
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
/* Search for a predicate node in it's connection list*/
MYDDAS_UTIL_PREDICATE
myddas_util_search_predicate(char *,int , char *);

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


#ifdef MYDDAS_STATS
int 
myddas_util_get_conn_total_rows(MYDDAS_UTIL_CONNECTION);
void 
myddas_util_set_conn_total_rows(MYDDAS_UTIL_CONNECTION,int);

unsigned long
myddas_util_get_conn_total_time_DBServer(MYDDAS_UTIL_CONNECTION);
void 
myddas_util_set_conn_total_time_DBServer(MYDDAS_UTIL_CONNECTION,unsigned long);

unsigned long
myddas_util_get_conn_last_time_DBServer(MYDDAS_UTIL_CONNECTION);
void 
myddas_util_set_conn_last_time_DBServer(MYDDAS_UTIL_CONNECTION,unsigned long);
unsigned long
myddas_util_get_conn_total_time_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION);
void 
myddas_util_set_conn_total_time_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION,unsigned long);

unsigned long
myddas_util_get_conn_last_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION);
void 
myddas_util_set_conn_last_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION,unsigned long);
unsigned long
myddas_util_get_conn_total_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION);
void 
myddas_util_set_conn_total_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION,unsigned long);

unsigned long
myddas_util_get_conn_last_bytes_transfering_from_DBserver(MYDDAS_UTIL_CONNECTION);
void 
myddas_util_set_conn_last_bytes_transfering_from_DBserver(MYDDAS_UTIL_CONNECTION,unsigned long);
unsigned long
myddas_util_get_conn_total_bytes_transfering_from_DBserver(MYDDAS_UTIL_CONNECTION);
void 
myddas_util_set_conn_total_bytes_transfering_from_DBserver(MYDDAS_UTIL_CONNECTION,unsigned long);

unsigned long
myddas_util_get_conn_number_querys_made(MYDDAS_UTIL_CONNECTION);
void
myddas_util_set_conn_number_querys_made(MYDDAS_UTIL_CONNECTION, unsigned long);

unsigned long
myddas_util_get_total_db_row_function(void);
void
myddas_util_set_total_db_row_function(unsigned long);




unsigned long
myddas_current_time(void);

#endif /* MYDDAS_STATS */


#ifdef DEBUG
void check_int(void);
#endif


//DELETE THIS WHEN DB_STATS  IS COMPLETED
int
get_myddas_top(void);

#endif /*__MYDDAS_UTIL_H__*/
