#ifndef __MYDDAS_UTIL_H__
#define __MYDDAS_UTIL_H__

//#include "Yap.h"
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

#endif /*__MYDDAS_UTIL_H__*/
