

#ifndef __MYDDAS_H__
#define __MYDDAS_H__

#include "YapConfig.h"
#include <stdio.h>

#include "myddas_types.h"

/* Passar para o myddas_statictics.h ???????? */
#ifdef MYDDAS_STATS
#include <sys/time.h>
#include <time.h>
#endif

typedef struct myddas_global *MYDDAS_GLOBAL;
#include "myddas_util.h"

#ifdef MYDDAS_STATS
typedef struct myddas_stats_time_struct *MYDDAS_STATS_TIME;
typedef struct myddas_global_stats *MYDDAS_GLOBAL_STATS;
typedef struct myddas_stats_struct *MYDDAS_STATS_STRUCT;
#else
typedef void *MYDDAS_STATS_TIME;
#endif

#ifdef DEBUG
#define MYDDAS_MEMORY_MALLOC_NR(NUMBER)                                        \
  NUMBER = Yap_REGS.MYDDAS_GLOBAL_POINTER->malloc_called;
#define MYDDAS_MEMORY_MALLOC_SIZE(NUMBER)                                      \
  NUMBER = Yap_REGS.MYDDAS_GLOBAL_POINTER->memory_allocated;
#define MYDDAS_MEMORY_FREE_NR(NUMBER)                                          \
  NUMBER = Yap_REGS.MYDDAS_GLOBAL_POINTER->free_called;
#define MYDDAS_MEMORY_FREE_SIZE(NUMBER)                                        \
  NUMBER = Yap_REGS.MYDDAS_GLOBAL_POINTER->memory_freed;
#endif

#include "MyddasProto.h"
#include "myddas_structs.h"

#endif /*__MYDDAS_H__*/
