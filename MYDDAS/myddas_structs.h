#ifndef __MYDDAS_STRUCTS_H__
#define __MYDDAS_STRUCTS_H__

struct myddas_global {
  MYDDAS_UTIL_CONNECTION myddas_top_connections;
#ifdef MYDDAS_STATS
  MYDDAS_GLOBAL_STATS myddas_statistics;
#endif
};

struct myddas_table_integers {
  unsigned int number;
  struct myddas_table_integers *next;
};

struct myddas_temp_tables{
  struct myddas_table_integers *table_numbers;
  struct myddas_table_integers *last_number;
  char *default_table_name;
};

#ifdef MYDDAS_STATS
/* This strucuture holds some global statistics*/
struct myddas_global_stats {
  unsigned long total_db_row;
};
#endif /* MYDDAS_STATS */

struct list_preds {
  char *pred_module;
  char *pred_name;
  short pred_arity;
  struct list_preds *next;
};

struct list_connection {
  void *connection;
  MYDDAS_TEMP_TABLES temporary_tables;
  
  /*If variable env is NULL, then it's a 
    MySQL connection, if not then it as the pointer 
    to the ODBC enviromment variable */
  void *odbc_enviromment;
#ifdef MYDDAS_STATS
  /* Total number of Rows returnes from the DataBase Server */
  unsigned long totalNumberOfRows;
  
  /* Total Time spent by the DataBase Server
     processing all querys */
  unsigned long totalTimeofDBServer; 
  /* Time spent by the DataBase Server, processing
     the last query */
  unsigned long lastTimeofDBServer;
  
  /* Total Time spent by the DataBase Server,
     transfering all the data to the client */
  unsigned long totalFromDBServer; 
  /* Time spent by the DataBase Server, 
     transfering the data of the last query */
  unsigned long lastFromDBServer; 
  
  /* Total Time spent on the db_row function */
  unsigned long total_db_row;
#endif
  MYDDAS_UTIL_PREDICATE predicates;
  struct list_connection *next;
};



#endif 
