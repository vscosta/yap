
#include <stdio.h>
#include <stdlib.h>
#include "Yap.h"
#include "myddas.h"
#ifdef MYDDAS_STATS
#include "myddas_statistics.h"
#endif

MYDDAS_GLOBAL
myddas_init_initialize_myddas(void) {
  MYDDAS_GLOBAL global = NULL;

  /* We cannot call MYDDAS_MALLOC were because the global
     register isn't yet initialized */
  global = (MYDDAS_GLOBAL)malloc(sizeof(struct myddas_global));
#ifdef DEBUGX
  printf("MALLOC %p %s %d\n", global, __FILE__, __LINE__);
#endif
  global->myddas_top_connections = NULL;
#ifdef MYDDAS_TOP_LEVEL
  global->myddas_top_level_connection = NULL;
#endif
#ifdef MYDDAS_STATS
  global->myddas_statistics =
      (MYDDAS_GLOBAL_STATS)malloc(sizeof(struct myddas_global_stats));
#ifdef DEBUG
  printf("MALLOC %p %s %d\n", global->myddas_statistics, __FILE__, __LINE__);
#endif
  global->myddas_statistics->stats = NULL;
#endif

#ifdef DEBUG
/* We first malloc for this struct and the stats struct */
#ifdef MYDDAS_STATS
  global->malloc_called = 2;
  global->memory_allocated =
      sizeof(struct myddas_global) + sizeof(struct myddas_global_stats);
#else
  global->malloc_called = 1;
  global->memory_allocated = sizeof(struct myddas_global);
#endif /* MYDDAS_STATS */
  global->free_called = 0;
  global->memory_freed = 0;
#endif

  return global;
}

/* Inserts the new node on the front of the list */
MYDDAS_UTIL_CONNECTION
myddas_init_initialize_connection(void *conn, void *enviromment, MYDDAS_API api,
                                  MYDDAS_UTIL_CONNECTION next) {
  CACHE_REGS
  MYDDAS_UTIL_CONNECTION new = NULL;
  MYDDAS_MALLOC(new, struct myddas_list_connection);

  if (new == NULL) {
    return NULL;
  }
  new->api = api;
  new->predicates = NULL;
  new->connection = conn;
  new->odbc_enviromment = enviromment;

  /* It saves n queries, doing at once n+1 queries */
  new->total_number_queries = 0; // Default
  new->actual_number_queries = 0;
  new->queries = NULL;

  /* List integrity */
  new->next = next;
  new->previous = NULL;
  /* If there's already at least one node
   on the list */
  if (next != NULL)
    next->previous = new;

#ifdef MYDDAS_STATS
  new->stats = NULL;
  new->stats = myddas_stats_initialize_connection_stats();
#endif
  return new;
}

MYDDAS_UTIL_PREDICATE
myddas_init_initialize_predicate(const char *pred_name, int pred_arity,
                                 const char *pred_module,
                                 MYDDAS_UTIL_PREDICATE next) {
  CACHE_REGS
  MYDDAS_UTIL_PREDICATE new = NULL;
  MYDDAS_MALLOC(new, struct myddas_list_preds);

  if (new == NULL) {
    return NULL;
  }
  new->pred_name = pred_name;
  new->pred_arity = pred_arity;
  new->pred_module = pred_module;

  /* List integrity */
  new->next = next;
  new->previous = NULL;
  /* If there's already at least one node
     on the list */
  if (next != NULL)
    next->previous = new;

  return new;
}
