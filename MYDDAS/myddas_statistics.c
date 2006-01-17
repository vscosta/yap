#ifdef CUT_C
#if defined MYDDAS_ODBC || defined MYDDAS_MYSQL 


#include "myddas_structs.h"
#include "myddas_statistics.h"
#include "Yap.h"

#ifdef MYDDAS_STATS
int
myddas_util_get_conn_total_rows(MYDDAS_UTIL_CONNECTION node){
  return node->totalNumberOfRows;
}
void
myddas_util_set_conn_total_rows(MYDDAS_UTIL_CONNECTION node ,
		    int totalRows){
  node->totalNumberOfRows = totalRows;
}

unsigned long
myddas_util_get_conn_total_time_DBServer(MYDDAS_UTIL_CONNECTION node){
  return node->totalTimeofDBServer;
}
void
myddas_util_set_conn_total_time_DBServer(MYDDAS_UTIL_CONNECTION node ,
		    unsigned long totaltime){
  node->totalTimeofDBServer = totaltime;
}

unsigned long
myddas_util_get_conn_last_time_DBServer(MYDDAS_UTIL_CONNECTION node){
  return node->lastTimeofDBServer;
}
void
myddas_util_set_conn_last_time_DBServer(MYDDAS_UTIL_CONNECTION node ,
		    unsigned long lasttime){
  node->lastTimeofDBServer = lasttime;
}

unsigned long
myddas_util_get_conn_last_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION node){
  return node->lastFromDBServer;
}
void
myddas_util_set_conn_last_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION node ,
		    unsigned long lasttime){
  node->lastFromDBServer = lasttime;
}

unsigned long
myddas_util_get_conn_total_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION node){
  return node->totalFromDBServer;
}
void
myddas_util_set_conn_total_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION node ,
		    unsigned long totaltime){
  node->totalFromDBServer = totaltime;
}

unsigned long
myddas_util_get_conn_last_bytes_transfering_from_DBserver(MYDDAS_UTIL_CONNECTION node){
  return node->lastBytesTransfered;
}
void
myddas_util_set_conn_last_bytes_transfering_from_DBserver(MYDDAS_UTIL_CONNECTION node, unsigned long bytes){
  node->lastBytesTransfered = bytes;
}

unsigned long
myddas_util_get_conn_total_bytes_transfering_from_DBserver(MYDDAS_UTIL_CONNECTION node){
  return node->totalBytesTransfered;
}
void
myddas_util_set_conn_total_bytes_transfering_from_DBserver(MYDDAS_UTIL_CONNECTION node, unsigned long bytes){
  node->totalBytesTransfered = bytes;
}

unsigned long
myddas_util_get_conn_number_querys_made(MYDDAS_UTIL_CONNECTION node){
  return node->total_querys_made;
}
void
myddas_util_set_conn_number_querys_made(MYDDAS_UTIL_CONNECTION node, unsigned long number){
  node->total_querys_made = number;
}

unsigned long
myddas_util_get_total_db_row_function(void){
  return Yap_regp->MYDDAS_GLOBAL_POINTER->myddas_statistics->total_db_row;
}
void
myddas_util_set_total_db_row_function(unsigned long time){
  Yap_regp->MYDDAS_GLOBAL_POINTER->myddas_statistics->total_db_row = time;
}


unsigned long
myddas_current_time(void) {
  /* to get time as Yap */
  
    Int now, interval;
    Yap_cputime_interval(&now, &interval);
    //return ((realtime)now);
    return (now);
  
  /*Fine grained time
    tv_usec -> microseconds [0-999999]
  */
  /*Fine grained time
    sec -> [0-999]
    tv_usec -> microseconds [0-99999] -> last digit is negleted
    -> max execution time: 16minutes
    milliseconds -> s/1000
    microseconds -> s/1000000
  */
  /* struct timeval tempo; */
 /*   if (!gettimeofday(&tempo, NULL)) */
/*     //returns time in microseconds */
/*     return (tempo.tv_sec %1000)*1000000+tempo.tv_usec; */
/*   //return (tempo.tv_sec %1000)*1000+tempo.tv_usec; */
  return 0;
}


#endif /* MYDDAS_STATS */

#endif
#endif
