#ifndef __MYDDAS_STATISTICS_H__
#define __MYDDAS_STATISTICS_H__

#ifdef MYDDAS_STATS

#define MYDDAS_STATS_TIME_HOURS(TIME) TIME->u.time_final.hours;
#define MYDDAS_STATS_TIME_MINUTES(TIME) TIME->u.time_final.minutes;
#define MYDDAS_STATS_TIME_SECONDS(TIME) TIME->u.time_final.seconds;
#define MYDDAS_STATS_TIME_MILISECONDS(TIME) TIME->u.time_final.miliseconds;
#define MYDDAS_STATS_TIME_MICROSECONDS(TIME) TIME->u.time_final.microseconds;

#ifdef DEBUG
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
#endif

#define MYDDAS_STATS_INITIALIZE_TIME_STRUCT(TIME,TYPE)			\
  MYDDAS_MALLOC(TIME,struct myddas_stats_time_struct);                  \
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

#define MYDDAS_STATS_CON_GET_TOTAL_TIME_DBSERVER(NODE,TIME)	\
  TIME = myddas_stats_get_stat(NODE->stats,1)->u.time_str.time_str;
#define MYDDAS_STATS_CON_GET_TOTAL_TIME_DBSERVER_COUNT(NODE,COUNT)	\
  COUNT = myddas_stats_get_stat(NODE->stats,1)->count;
#define MYDDAS_STATS_CON_SET_TOTAL_TIME_DBSERVER_COUNT(NODE,COUNT)	\
  myddas_stats_get_stat(NODE->stats,1)->count = COUNT;

#define MYDDAS_STATS_CON_GET_LAST_TIME_DBSERVER(NODE,TIME)	\
  TIME = myddas_stats_get_stat(NODE->stats,2)->u.time_str.time_str;
#define MYDDAS_STATS_CON_GET_LAST_TIME_DBSERVER_COUNT(NODE,COUNT)	\
  COUNT = myddas_stats_get_stat(NODE->stats,2)->count;
#define MYDDAS_STATS_CON_SET_LAST_TIME_DBSERVER_COUNT(NODE,COUNT)	\
  myddas_stats_get_stat(NODE->stats,2)->count = COUNT;

#define MYDDAS_STATS_CON_GET_TOTAL_TIME_TRANSFERING(NODE,TIME)	\
  TIME = myddas_stats_get_stat(NODE->stats,3)->u.time_str.time_str;
#define MYDDAS_STATS_CON_GET_TOTAL_TIME_TRANSFERING_COUNT(NODE,COUNT)	\
  COUNT = myddas_stats_get_stat(NODE->stats,3)->count;
#define MYDDAS_STATS_CON_SET_TOTAL_TIME_TRANSFERING_COUNT(NODE,COUNT)	\
  myddas_stats_get_stat(NODE->stats,3)->count = COUNT;

#define MYDDAS_STATS_CON_GET_LAST_TIME_TRANSFERING(NODE,TIME)	\
  TIME = myddas_stats_get_stat(NODE->stats,4)->u.time_str.time_str;
#define MYDDAS_STATS_CON_GET_LAST_TIME_TRANSFERING_COUNT(NODE,COUNT)	\
  COUNT = myddas_stats_get_stat(NODE->stats,4)->count;
#define MYDDAS_STATS_CON_SET_LAST_TIME_TRANSFERING_COUNT(NODE,COUNT)	\
  myddas_stats_get_stat(NODE->stats,4)->count = COUNT;


#define MYDDAS_STATS_CON_GET_TOTAL_ROWS(NODE,NUMBER)	\
  NUMBER = myddas_stats_get_stat(NODE->stats,5)->u.integer.integer;
#define MYDDAS_STATS_CON_SET_TOTAL_ROWS(NODE,NUMBER)	\
  myddas_stats_get_stat(NODE->stats,5)->u.integer.integer = NUMBER;
#define MYDDAS_STATS_CON_GET_TOTAL_ROWS_COUNT(NODE,COUNT)	\
  COUNT = myddas_stats_get_stat(NODE->stats,5)->count;
#define MYDDAS_STATS_CON_SET_TOTAL_ROWS_COUNT(NODE,COUNT)	\
  myddas_stats_get_stat(NODE->stats,5)->count = COUNT;


#define MYDDAS_STATS_CON_GET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER(NODE,NUMBER) \
  NUMBER = myddas_stats_get_stat(NODE->stats,6)->u.integer.integer;
#define MYDDAS_STATS_CON_SET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER(NODE,NUMBER) \
  myddas_stats_get_stat(NODE->stats,6)->u.integer.integer = NUMBER;
#define MYDDAS_STATS_CON_GET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER_COUNT(NODE,COUNT)	\
  COUNT = myddas_stats_get_stat(NODE->stats,6)->count;
#define MYDDAS_STATS_CON_SET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER_COUNT(NODE,COUNT)	\
  myddas_stats_get_stat(NODE->stats,6)->count = COUNT;

#define MYDDAS_STATS_CON_GET_LAST_BYTES_TRANSFERING_FROM_DBSERVER(NODE,NUMBER) \
  NUMBER = myddas_stats_get_stat(NODE->stats,7)->u.integer.integer;
#define MYDDAS_STATS_CON_SET_LAST_BYTES_TRANSFERING_FROM_DBSERVER(NODE,NUMBER) \
  myddas_stats_get_stat(NODE->stats,7)->u.integer.integer = NUMBER;
#define MYDDAS_STATS_CON_GET_LAST_BYTES_TRANSFERING_FROM_DBSERVER_COUNT(NODE,COUNT)	\
  COUNT = myddas_stats_get_stat(NODE->stats,7)->count;
#define MYDDAS_STATS_CON_SET_LAST_BYTES_TRANSFERING_FROM_DBSERVER_COUNT(NODE,COUNT)	\
  myddas_stats_get_stat(NODE->stats,7)->count = COUNT;

#define MYDDAS_STATS_CON_GET_NUMBER_QUERIES_MADE(NODE,NUMBER) \
  NUMBER = myddas_stats_get_stat(NODE->stats,8)->u.integer.integer;
#define MYDDAS_STATS_CON_SET_NUMBER_QUERIES_MADE(NODE,NUMBER) \
  myddas_stats_get_stat(NODE->stats,8)->u.integer.integer = NUMBER;
#define MYDDAS_STATS_CON_GET_NUMBER_QUERIES_MADE_COUNT(NODE,COUNT)	\
  COUNT = myddas_stats_get_stat(NODE->stats,8)->count;
#define MYDDAS_STATS_CON_SET_NUMBER_QUERIES_MADE_COUNT(NODE,COUNT)	\
  myddas_stats_get_stat(NODE->stats,8)->count = COUNT;

#define MYDDAS_STATS_GET_DB_ROW_FUNCTION(TIME) \
  TIME = myddas_stats_get_stat(Yap_REGS.MYDDAS_GLOBAL_POINTER->myddas_statistics->stats,1)->u.time_str.time_str;
#define MYDDAS_STATS_GET_DB_ROW_FUNCTION_COUNT(COUNT)	\
  COUNT = myddas_stats_get_stat(Yap_REGS.MYDDAS_GLOBAL_POINTER->myddas_statistics->stats,1)->count;
#define MYDDAS_STATS_SET_DB_ROW_FUNCTION_COUNT(COUNT)	\
  myddas_stats_get_stat(Yap_REGS.MYDDAS_GLOBAL_POINTER->myddas_statistics->stats,1)->count = COUNT;

#define MYDDAS_STATS_GET_TRANSLATE(TIME) \
  TIME = myddas_stats_get_stat(Yap_REGS.MYDDAS_GLOBAL_POINTER->myddas_statistics->stats,2)->u.time_str.time_str;
#define MYDDAS_STATS_GET_TRANSLATE_COUNT(COUNT)	\
  COUNT = myddas_stats_get_stat(Yap_REGS.MYDDAS_GLOBAL_POINTER->myddas_statistics->stats,2)->count;
#define MYDDAS_STATS_SET_TRANSLATE_COUNT(COUNT)	\
  myddas_stats_get_stat(Yap_REGS.MYDDAS_GLOBAL_POINTER->myddas_statistics->stats,2)->count = COUNT;

MYDDAS_STATS_TIME myddas_stats_walltime(void);
void myddas_stats_add_time(MYDDAS_STATS_TIME, MYDDAS_STATS_TIME,MYDDAS_STATS_TIME);
void myddas_stats_subtract_time(MYDDAS_STATS_TIME, MYDDAS_STATS_TIME,MYDDAS_STATS_TIME);
void myddas_stats_move_time(MYDDAS_STATS_TIME,MYDDAS_STATS_TIME);
MYDDAS_STATS_TIME myddas_stats_time_copy_to_final(MYDDAS_STATS_TIME);

/* Related to the statistics linked list */
MYDDAS_STATS_STRUCT myddas_stats_initialize_stat(MYDDAS_STATS_STRUCT,int);
MYDDAS_STATS_STRUCT myddas_stats_get_stat(MYDDAS_STATS_STRUCT,int);
#endif /* MYDDAS_STATS */

#endif
