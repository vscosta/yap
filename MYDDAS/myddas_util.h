#ifndef __MYDDAS_UTIL_H__
#define __MYDDAS_UTIL_H__

#include <stdio.h>
#ifdef MYDDAS_ODBC
#include <sql.h>
#endif


typedef struct list_connection *MYDDAS_UTIL_CONNECTION;
typedef struct list_preds *MYDDAS_UTIL_PREDICATE;

#ifdef MYDDAS_STATS
#include <time.h>
#include <sys/time.h>
#endif


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
myddas_current_time(void);
#endif


#ifdef DEBUG
void check_int(void);
#endif

#endif /*__MYDDAS_UTIL_H__*/
