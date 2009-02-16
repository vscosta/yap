#ifndef __MYDDAS_H__
#define __MYDDAS_H__

#include "config.h"
#include <stdio.h>

#ifdef MYDDAS_ODBC
#include <sql.h>
#endif

#ifdef MYDDAS_MYSQL
#include <mysql/mysql.h>
#endif

/* MYDDAS TYPES */
/* sizeof(MyddasPointer) Equal to the size of a integer on the given architecture */
/* sizeof(MyddasInt32) = 4 (Always) */
/* sizeof(MyddasUInt32) = 4 (Always) */

#if SIZEOF_INT_P==4

#  if SIZEOF_INT==4
/*   */ typedef int MyddasInt;
/*   */ typedef unsigned int MyddasUInt;
/*   */ typedef unsigned int MyddasPointer;
/*   */ typedef int MyddasInt32;
/*   */ typedef unsigned int MyddasUInt32;
#  elif SIZEOF_LONG_INT==4
/*   */ typedef long int MyddasInt;
/*   */ typedef unsigned long int MyddasUInt;
/*   */ typedef unsigned long int MyddasPointer;
/*   */ typedef long int MyddasInt32;
/*   */ typedef unsigned long int MyddasUInt32;
#  else
#	error MYDDAS require integer types of the same size as a pointer
#  endif

#  if SIZEOF_SHORT_INT==2
/*   */ typedef short int MyddasSInt;
/*   */ typedef unsigned short int MyddasUSInt;
#  else
#	error MYDDAS requires integer types half the size of a pointer
#  endif

#  if SIZEOF_LONG_INT==8
/*   */ typedef long int MyddasLInt;
/*   */ typedef unsigned long int MyddasULInt;
#  elif SIZEOF_LONG_LONG_INT==8
/*   */ typedef long long int MyddasLInt;
/*   */ typedef unsigned long long int MyddasULInt;
#  else
#	error MYDDAS requires integer types double the size of a pointer
#  endif

#elif SIZEOF_INT_P==8

#  if SIZEOF_INT==8
/*   */ typedef int MyddasInt;
/*   */ typedef unsigned int MyddasUInt;
/*   */ typedef int MyddasLInt;
/*   */ typedef unsigned int MyddasULInt;
/*   */ typedef unsigned int MyddasPointer;
#  elif SIZEOF_LONG_INT==8
/*   */ typedef long int MyddasInt;
/*   */ typedef unsigned long int MyddasUInt;
/*   */ typedef int MyddasLInt;
/*   */ typedef unsigned int MyddasULInt;
/*   */ typedef unsigned long int MyddasPointer;
#  elif SIZEOF_LONG_LONG_INT==8
/*   */ typedef long long int MyddasInt;
/*   */ typedef unsigned long long int MyddasUInt;
/*   */ typedef int MyddasLInt;
/*   */ typedef unsigned int MyddasULInt;
/*   */ typedef unsigned long long int MyddasPointer;
#  else
#	error MYDDAS requires integer types of the same size as a pointer
#  endif

#   if SIZEOF_SHORT_INT==4
/*   */ typedef short int MyddasSInt;
/*   */ typedef unsigned short int MyddasUSInt;
/*   */ typedef short int MyddasInt32;
/*   */ typedef unsigned short int MyddasUInt32;
#   elif SIZEOF_INT==4
/*   */ typedef int MyddasSInt;
/*   */ typedef unsigned int MyddasUSInt;
/*   */ typedef int MyddasInt32;
/*   */ typedef unsigned int MyddasUInt32;
#   else
#	error MYDDAS requires integer types half the size of a pointer
#   endif

#else
#   error MYDDAS requires pointers of size 4 or 8
#endif




/* Passar para o myddas_statictics.h ???????? */
#ifdef MYDDAS_STATS
#include <time.h>
#include <sys/time.h>
#endif

typedef struct myddas_global *MYDDAS_GLOBAL;
typedef struct myddas_util_query *MYDDAS_UTIL_QUERY;
typedef struct myddas_list_connection *MYDDAS_UTIL_CONNECTION;
typedef struct myddas_list_preds *MYDDAS_UTIL_PREDICATE;

#ifdef MYDDAS_STATS
typedef struct myddas_stats_time_struct *MYDDAS_STATS_TIME;
typedef struct myddas_global_stats *MYDDAS_GLOBAL_STATS;
typedef struct myddas_stats_struct *MYDDAS_STATS_STRUCT;
#endif

#ifdef DEBUG                                                
#define MYDDAS_MALLOC(POINTER,TYPE)                                \
 {                                                                 \
   POINTER = (TYPE *) malloc(sizeof(TYPE));                        \
   Yap_REGS.MYDDAS_GLOBAL_POINTER->memory_allocated+=sizeof(TYPE); \
   /*printf ("MALLOC %p %s %d\n",POINTER,__FILE__,__LINE__);*/ \
   Yap_REGS.MYDDAS_GLOBAL_POINTER->malloc_called++;                \
 }
#else
#define MYDDAS_MALLOC(POINTER,TYPE)                                \
 {                                                                 \
   POINTER = (TYPE *) malloc(sizeof(TYPE));                        \
 }
#endif

#ifdef DEBUG                                                
#define MYDDAS_FREE(POINTER,TYPE)                                  \
 {                                                                 \
   Yap_REGS.MYDDAS_GLOBAL_POINTER->memory_freed+=sizeof(TYPE);     \
   Yap_REGS.MYDDAS_GLOBAL_POINTER->free_called++;                  \
   /*printf ("FREE   %p %s %d\n",POINTER,__FILE__,__LINE__);*/ \
   free(POINTER);                                                  \
 }
#else
#define MYDDAS_FREE(POINTER,TYPE)                                  \
 {                                                                 \
   free(POINTER);                                                  \
 }
#endif

#ifdef DEBUG
#define MYDDAS_MEMORY_MALLOC_NR(NUMBER)   \
  NUMBER = Yap_REGS.MYDDAS_GLOBAL_POINTER->malloc_called;
#define MYDDAS_MEMORY_MALLOC_SIZE(NUMBER) \
  NUMBER = Yap_REGS.MYDDAS_GLOBAL_POINTER->memory_allocated;
#define MYDDAS_MEMORY_FREE_NR(NUMBER)     \
  NUMBER = Yap_REGS.MYDDAS_GLOBAL_POINTER->free_called;
#define MYDDAS_MEMORY_FREE_SIZE(NUMBER)   \
  NUMBER = Yap_REGS.MYDDAS_GLOBAL_POINTER->memory_freed;
#endif


#endif /*__MYDDAS_H__*/
