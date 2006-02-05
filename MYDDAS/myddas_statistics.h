#ifndef __MYDDAS_STATISTICS_H__
#define __MYDDAS_STATISTICS_H__


#if defined MYDDAS_STATS || defined MYDDAS_TOP_LEVEL

#define MYDDAS_STATS_PRINT_TIME_STRUCT(TIME)				\
  if (TIME->type == time_final) {						\
    printf ("%d Hours, %d Minutes, %d Seconds, %d Miliseconds, %d Microseconds", \
	    TIME->u.time_final.hours,					\
	    TIME->u.time_final.minutes,					\
	    TIME->u.time_final.seconds,					\
	    TIME->u.time_final.miliseconds,				\
	    TIME->u.time_final.microseconds);				\
  } else {								\
    printf ("%lu Seconds, %lu Microseconds", \
	    TIME->u.time_copy.tv_sec,					\
	    TIME->u.time_copy.tv_usec);					\
  }


#define MYDDAS_STATS_INITIALIZE_TIME_STRUCT(TIME,TYPE)			\
  TIME = (MYDDAS_STATS_TIME) malloc (sizeof(struct myddas_stats_time_struct)); \
									\
  if (TYPE == time_copy){						\
    TIME->type = TYPE;							\
    TIME->u.time_copy.tv_sec = 0;					\
    TIME->u.time_copy.tv_usec = 0;					\
  } else {								\
    TIME->type = TYPE;							\
    TIME->u.time_final.hours = 0;					\
    TIME->u.time_final.minutes = 0;					\
    TIME->u.time_final.seconds = 0;					\
    TIME->u.time_final.miliseconds = 0;				\
    TIME->u.time_final.microseconds = 0;				\
  }


MYDDAS_STATS_TIME
myddas_stats_walltime(void);
void
myddas_stats_add_time(MYDDAS_STATS_TIME, MYDDAS_STATS_TIME,MYDDAS_STATS_TIME);
void
myddas_stats_subtract_time(MYDDAS_STATS_TIME, MYDDAS_STATS_TIME,MYDDAS_STATS_TIME);
void
myddas_stats_move_time(MYDDAS_STATS_TIME,MYDDAS_STATS_TIME);
MYDDAS_STATS_TIME
myddas_stats_time_copy_to_final(MYDDAS_STATS_TIME);
#endif 


#ifdef MYDDAS_STATS

#define MYDDAS_STATS_CON_GET_TOTAL_ROWS(NODE,NUMBER)	\
  NUMBER = NODE->totalNumberOfRows;
#define MYDDAS_STATS_CON_SET_TOTAL_ROWS(NODE,NUMBER)	\
  NODE->totalNumberOfRows = NUMBER;

#define MYDDAS_STATS_CON_GET_TOTAL_TIME_DBSERVER(NODE,TIME)	\
  TIME = NODE->totalTimeofDBServer;

#define MYDDAS_STATS_CON_GET_LAST_TIME_DBSERVER(NODE,TIME)	\
  TIME = NODE->lastTimeofDBServer;

#define MYDDAS_STATS_CON_GET_TOTAL_TIME_TRANSFERING(NODE,TIME)	\
  TIME = NODE->totalFromDBServer;

#define MYDDAS_STATS_CON_GET_LAST_TIME_TRANSFERING(NODE,TIME)	\
  TIME = NODE->lastFromDBServer;

#define MYDDAS_STATS_CON_GET_LAST_BYTES_TRANSFERING_FROM_DBSERVER(NODE,NUMBER) \
  NUMBER = NODE->lastBytesTransfered;
#define MYDDAS_STATS_CON_SET_LAST_BYTES_TRANSFERING_FROM_DBSERVER(NODE,NUMBER) \
  NODE->lastBytesTransfered = NUMBER;

#define MYDDAS_STATS_CON_GET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER(NODE,NUMBER) \
  NUMBER = NODE->totalBytesTransfered;
#define MYDDAS_STATS_CON_SET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER(NODE,NUMBER) \
  NODE->totalBytesTransfered = NUMBER;


#define MYDDAS_STATS_CON_GET_NUMBER_QUERIES_MADE(NODE,NUMBER) \
  NUMBER = NODE->total_querys_made;
#define MYDDAS_STATS_CON_SET_NUMBER_QUERIES_MADE(NODE,NUMBER) \
  NODE->total_querys_made = NUMBER;

#define MYDDAS_STATS_GET_DB_ROW_FUNCTION(TIME) \
  TIME = Yap_regp->MYDDAS_GLOBAL_POINTER->myddas_statistics->total_db_row;

#endif /* MYDDAS_STATS */

#endif
