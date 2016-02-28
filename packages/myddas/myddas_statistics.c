#include <stdlib.h>
#if HAVE_SYS_TIME_H 
#include <sys/time.h>
#endif

#if defined MYDDAS_STATS

#include "myddas_structs.h"
#include "myddas_statistics.h"
#include "Yap.h"


/* Documentation: Time Units 
------------------------------------------------------------------------
*****| Second(s) | MiliSeconds(ms) | MicroSeconds(us) | NanoSecond(ns) |
-----|-----------|-----------------|------------------|----------------|
  s  |    1      |    0.001        |   0.000001       |    1e-9        |
  ms |  1000     |      1          |     0.001        |  0.000001      |
  us | 10000000  |    1000         |       1          |    0.001       |
  ns |1000000000 |   1000000       |     1000         |      1         |
------------------------------------------------------------------------

------

The struct timeval structure represents an elapsed time. It is
declared in `sys/time.h' and has the following members:

long int tv_sec -> This represents the number of whole seconds of
    elapsed time.

long int tv_usec -> This is the rest of the elapsed time (a fraction
    of a second), represented as the number of microseconds. It is
    always less than one million.


------

The struct timespec structure represents an elapsed time. It is
declared in `time.h' and has the following members:

long int tv_sec -> This represents the number of whole seconds of
    elapsed time.

long int tv_nsec -> This is the rest of the elapsed time (a fraction
    of a second), represented as the number of nanoseconds. It is
    always less than one billion.

-----

    The gettimeofday() function shall obtain the current time,
    expressed as seconds and microseconds since the Epoch, and store
    it in the timeval structure pointed to by tp. The resolution of
    the system clock is unspecified.

    If tzp is not a null pointer, the behavior is unspecified.

*/

static void
myddas_stats_time_subtract (unsigned long *, unsigned long *, MYDDAS_STATS_TIME, MYDDAS_STATS_TIME);
static void
myddas_stats_add_seconds_time(MYDDAS_STATS_TIME,unsigned long, unsigned long);
static void
myddas_stats_integrity_of_time(MYDDAS_STATS_TIME);

/* Be shore to delete MYDDAS_STATS_TIME structure */
MYDDAS_STATS_TIME
myddas_stats_walltime(void) {

  MYDDAS_STATS_TIME myddas_time = NULL;
  MYDDAS_MALLOC(myddas_time,struct myddas_stats_time_struct);
  myddas_time->type = time_copy;

  struct timeval *time = NULL;
  MYDDAS_MALLOC(time,struct timeval);
  
  gettimeofday(time,NULL);
  
  myddas_time->u.time_copy.tv_sec = time->tv_sec;
  myddas_time->u.time_copy.tv_usec = time->tv_usec;
  
  MYDDAS_FREE(time,struct timeval);

  return myddas_time;
}

void
myddas_stats_add_time(MYDDAS_STATS_TIME sum, MYDDAS_STATS_TIME time1,MYDDAS_STATS_TIME time2){
  
  if (sum->type == time_final){
    sum->u.time_final.microseconds =
      time1->u.time_final.microseconds +
      time2->u.time_final.microseconds;
    sum->u.time_final.miliseconds =
      time1->u.time_final.miliseconds +
      time2->u.time_final.miliseconds;
    sum->u.time_final.seconds =
      time1->u.time_final.seconds +
      time2->u.time_final.seconds;
    sum->u.time_final.minutes =
      time1->u.time_final.minutes +
      time2->u.time_final.minutes;
    sum->u.time_final.hours =
      time1->u.time_final.hours +
      time2->u.time_final.hours;
  } else {
    sum->u.time_copy.tv_sec =
      time1->u.time_copy.tv_sec +
      time2->u.time_copy.tv_sec;
    sum->u.time_copy.tv_usec =
      time1->u.time_copy.tv_usec +
      time2->u.time_copy.tv_usec;
  }
  
  myddas_stats_integrity_of_time(sum);
}
		       

void
myddas_stats_subtract_time(MYDDAS_STATS_TIME result, MYDDAS_STATS_TIME t1,MYDDAS_STATS_TIME t2){
  
  if (result->type == time_copy){
  
    unsigned long sec;
    unsigned long usec;
    myddas_stats_time_subtract(&sec,&usec,t1,t2);
    
    result->u.time_copy.tv_sec = sec;
    result->u.time_copy.tv_usec = usec;
    
  } else {
    
  }
  
}

void
myddas_stats_move_time(MYDDAS_STATS_TIME from,
		       MYDDAS_STATS_TIME to)
{
  if (from->type == time_copy)
    {
      to->type = time_copy;
      to->u.time_copy.tv_sec = from->u.time_copy.tv_sec; 
      to->u.time_copy.tv_usec = from->u.time_copy.tv_usec; 
    }
  else if (from->type == time_final)
    {
      to->u.time_final.hours = from->u.time_final.hours; 
      to->u.time_final.minutes = from->u.time_final.minutes; 
      to->u.time_final.seconds = from->u.time_final.seconds; 
      to->u.time_final.miliseconds = from->u.time_final.miliseconds; 
      to->u.time_final.microseconds = from->u.time_final.microseconds; 
    }
  MYDDAS_FREE(from,struct myddas_stats_time_struct);
}

MYDDAS_STATS_TIME
myddas_stats_time_copy_to_final(MYDDAS_STATS_TIME t_copy){
  
  MYDDAS_STATS_TIME t_final;
  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(t_final,time_final);
  
  myddas_stats_add_seconds_time(t_final,
				t_copy->u.time_copy.tv_sec,
				t_copy->u.time_copy.tv_usec);
  
  MYDDAS_FREE(t_copy,struct myddas_stats_time_struct);
  return t_final;
}

static void
myddas_stats_add_seconds_time(MYDDAS_STATS_TIME myddas_time,
			      unsigned long sec,
			      unsigned long usec){
 
  short hours = sec / 3600;					
  sec %= 3600;						
  short minutes = sec / 60;					
  sec %= 60;						
  short milisec = usec / 1000;				 
  usec %= 1000;
								
  myddas_time->u.time_final.microseconds += usec ;			
  myddas_time->u.time_final.miliseconds += milisec;			
  myddas_time->u.time_final.seconds += sec ;				
  myddas_time->u.time_final.minutes += minutes ;			
  myddas_time->u.time_final.hours += hours;

  myddas_stats_integrity_of_time(myddas_time);
}


static void
myddas_stats_time_subtract(unsigned long *sec,unsigned long *usec,
			   MYDDAS_STATS_TIME start, MYDDAS_STATS_TIME end){

  /* Perform the carry for the later subtraction by updating y. */
  if (start->u.time_copy.tv_usec < end->u.time_copy.tv_usec) {
    int nsec = (end->u.time_copy.tv_usec - start->u.time_copy.tv_usec) / 1000000 + 1;
    end->u.time_copy.tv_usec -= 1000000 * nsec;
    end->u.time_copy.tv_sec += nsec;
  }
  if (start->u.time_copy.tv_usec - end->u.time_copy.tv_usec > 1000000) {
    int nsec = (start->u.time_copy.tv_usec - end->u.time_copy.tv_usec) / 1000000;
    end->u.time_copy.tv_usec += 1000000 * nsec;
    end->u.time_copy.tv_sec -= nsec;
  }

  /* Compute the time remaining to wait.
     tv_usec is certainly positive. */
  *sec = start->u.time_copy.tv_sec - end->u.time_copy.tv_sec;
  *usec = start->u.time_copy.tv_usec - end->u.time_copy.tv_usec;
}

static void
myddas_stats_integrity_of_time(MYDDAS_STATS_TIME myddas_time){
  
  if (myddas_time->u.time_final.microseconds > 999)			
    {
      myddas_time->u.time_final.microseconds -= 1000;			
      myddas_time->u.time_final.miliseconds++;			
    }	
  if (myddas_time->u.time_final.miliseconds > 999)			
    {								
      myddas_time->u.time_final.miliseconds -= 1000;			
      myddas_time->u.time_final.seconds++;				
    }								
  
  if (myddas_time->u.time_final.seconds > 59)			
    {								
      myddas_time->u.time_final.seconds -= 60;			
      myddas_time->u.time_final.minutes++;				
    }								
  
  if (myddas_time->u.time_final.minutes > 59)			
    {								
      myddas_time->u.time_final.minutes -= 60;			
      myddas_time->u.time_final.hours++;				
    }								
}

MYDDAS_GLOBAL
myddas_stats_initialize_global_stats(MYDDAS_GLOBAL global){
  
  MYDDAS_STATS_STRUCT stats = NULL; 
  
  short i;
  
  /* For the time statistics */
  /*
    Stats [1] - Total Time spent on the db_row function 
    Stats [2] - Total Time spent on the translate/3 predicate
  */

  /* First */
  stats = myddas_stats_initialize_stat(stats,time_str);
  (global->myddas_statistics)->stats = stats;
  for(i=0;i<1;i++){ 
    myddas_stats_initialize_stat(stats,time_str);
  }
  
  return global;
}

MYDDAS_STATS_STRUCT
myddas_stats_initialize_connection_stats(){
  /*
  Stats [1] - Total of Time Spent by the DB Server processing all the  SQL Querys
  Stats [2] - Total of Time Spent by the DB Server processing the last SQL Query
  Stats [3] - Total of Time Spent by the DB Server transfering all the results of the  SQL Querys
  Stats [4] - Total of Time Spent by the DB Server transfering the result of the last SQL Query 
    
  Stats [5] - Total number of Rows returned by the server
  Stats [6] - Total of Bytes Transfered by the DB Server on all SQL Querys
  Stats [7] - Total of Bytes Transfered by the DB Server on the last SQL Query 
  Stats [8] - Number of querys made to the DBserver  
  */

  short i;
  MYDDAS_STATS_STRUCT new = NULL ;
  MYDDAS_STATS_STRUCT first;        
  /* For the time statistics */ 
               
  /* First */  
  new = myddas_stats_initialize_stat(new,time_str);
  first = new;
  for(i=0;i<3;i++){                 
     new = myddas_stats_initialize_stat(new,time_str);
  }                                               
                                                  
  /* For number statistics*/                      
  for (i=0;i<4;i++){                              
    new = myddas_stats_initialize_stat(new,integer);  
  }

  return first;
}

MYDDAS_STATS_STRUCT
myddas_stats_initialize_stat(MYDDAS_STATS_STRUCT stat,int type){
  
  MYDDAS_STATS_STRUCT temp_str = stat;

  if (stat == NULL){
    MYDDAS_MALLOC(stat,struct myddas_stats_struct);
    temp_str = stat;
  } else {
    for (;temp_str->nxt != NULL;temp_str = temp_str->nxt);
    MYDDAS_MALLOC(temp_str->nxt,struct myddas_stats_struct);
    temp_str = temp_str->nxt;
  }
  
  if (type == time_str){
    MYDDAS_STATS_INITIALIZE_TIME_STRUCT(temp_str->u.time_str.time_str,time_final);
  } else {
    temp_str->u.integer.integer = 0;
  }
  temp_str->type = type;
  temp_str->count = 0;
  temp_str->nxt = NULL;
  return temp_str;
}

MYDDAS_STATS_STRUCT
myddas_stats_get_stat(MYDDAS_STATS_STRUCT stat,int index){
  
  MYDDAS_STATS_STRUCT temp = stat;

  for (;index>1;index--){
    temp = temp->nxt;
  }
  return temp;
}

void
myddas_stats_delete_stats_list(MYDDAS_STATS_STRUCT list){
  
  MYDDAS_STATS_STRUCT to_delete = list;
  
  for (;to_delete!=NULL;){
    list = list->nxt;
 
    
    if (to_delete->type == time_str){
      MYDDAS_FREE(to_delete->u.time_str.time_str,struct myddas_stats_time_struct);
    }

    MYDDAS_FREE(to_delete,struct myddas_stats_struct);
    
    to_delete = list;
  }

}


#endif /* MYDDAS_STATS || MYDDAS_TOP_LEVEL */

