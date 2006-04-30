#ifndef __MYDDAS_STRUCTS_H__
#define __MYDDAS_STRUCTS_H__

#include "myddas_util.h"

struct myddas_global {
  MYDDAS_UTIL_CONNECTION myddas_top_connections;
#ifdef MYDDAS_TOP_LEVEL
  MYDDAS_UTIL_CONNECTION myddas_top_level_connection;
#endif
#ifdef MYDDAS_STATS
  MYDDAS_GLOBAL_STATS myddas_statistics;
#endif
};

#ifdef MYDDAS_STATS
/* This strucuture holds some global statistics*/
struct myddas_global_stats {
  MYDDAS_STATS_TIME total_db_row;
};
#endif /* MYDDAS_STATS */

struct myddas_list_preds {
  char *pred_module;
  char *pred_name;
  short pred_arity;
  //void *pe;
  MYDDAS_UTIL_PREDICATE next;
  MYDDAS_UTIL_PREDICATE previous;
};

struct myddas_list_connection {
  void *connection;
    
  /*If variable env is NULL, then it's a 
    MySQL connection, if not then it as the pointer 
    to the ODBC enviromment variable */
  void *odbc_enviromment;
#ifdef MYDDAS_STATS
  /* Total number of Rows returnes from the DataBase Server */
  unsigned long totalNumberOfRows;
  
  /* Total Time spent by the DataBase Server
     processing all querys */
  MYDDAS_STATS_TIME totalTimeofDBServer; 
  /* Time spent by the DataBase Server, processing
     the last query */
  MYDDAS_STATS_TIME lastTimeofDBServer;
  
  /* Total Time spent by the DataBase Server,
     transfering all the data to the client */
  MYDDAS_STATS_TIME totalFromDBServer; 
  /* Time spent by the DataBase Server, 
     transfering the data of the last query */
  MYDDAS_STATS_TIME lastFromDBServer; 
  
  /* Last bytes transfered from the server */
  unsigned long totalBytesTransfered;
  /* Total bytes transfered from the server */
  unsigned long lastBytesTransfered;

  /* Total Time spent on the db_row function */
  MYDDAS_STATS_TIME total_db_row;

  /* Number of querys made to the Server*/
  unsigned long total_querys_made;
#endif
  MYDDAS_UTIL_PREDICATE predicates;

  /* Multi Queries Section */
  unsigned long total_number_queries;
  unsigned long actual_number_queries;
  MYDDAS_UTIL_QUERY *queries;

  /* List Integrety */
  MYDDAS_UTIL_CONNECTION next;
  MYDDAS_UTIL_CONNECTION previous;
};

struct myddas_util_query{
  char *query;
  MYDDAS_UTIL_QUERY next;
};


#if defined MYDDAS_STATS || defined MYDDAS_TOP_LEVEL
struct myddas_stats_time_struct{
  enum {time_copy,
	time_final} type;
  
  union {
    struct {
      unsigned long tv_sec;
      unsigned long tv_usec;
    } time_copy;
    struct {
      unsigned short hours;  
      unsigned short minutes;  //Max 59
      unsigned short seconds;  //Max 59
      unsigned short miliseconds; //Max 999
      unsigned short microseconds; //Max 999
    } time_final;
  } u;
};
#endif /* MYDDAS_STATS  || MYDDAS_TOP_LEVEL */ 

#endif 
