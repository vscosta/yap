#ifndef __MYDDAS_STATISTICS_STRUCTS_H__
#define __MYDDAS_STATISTICS_STRUCTS_H__

#ifdef MYDDAS_STATS

/* This strucuture holds some global statistics*/
struct myddas_global_stats {
  MYDDAS_STATS_STRUCT stats;
};

/* Structure to hold any kind of statistics */
struct myddas_stats_struct{
  enum {time_str,
	integer} type;
  union {
    struct {
      MYDDAS_STATS_TIME time_str; 
    } time_str;
    struct {
      MyddasULInt integer;
    } integer;
  } u;
  MyddasULInt count;
  MYDDAS_STATS_STRUCT nxt;
};

/* Time structure for the MYDDAS Interface */
struct myddas_stats_time_struct{
  enum {time_copy,
	time_final} type;
  
  union {
    struct {
      unsigned long tv_sec;
      unsigned long tv_usec;
    } time_copy;
    struct {
      MyddasUSInt hours;  
      MyddasUSInt minutes;  //Max 59
      MyddasUSInt seconds;  //Max 59
      MyddasUSInt miliseconds; //Max 999
      MyddasUSInt microseconds; //Max 999
    } time_final;
  } u;
};


#endif /* MYDDAS_STATS */

#endif 
