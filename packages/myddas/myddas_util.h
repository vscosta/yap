
#include "myddas_structs.h"

void myddas_util_error_message(char *message, Int line, char *file);

UInt myddas_util_get_total_multi_queries_number(MYDDAS_UTIL_CONNECTION con);

void myddas_util_set_total_multi_queries_number(MYDDAS_UTIL_CONNECTION con,
                                                UInt number);

//void myddas_util_table_write(MYSQL_RES *res_set);

void *myddas_util_get_pred_next(void *pointer);

MyddasInt myddas_util_get_pred_arity(void *pointer);

const char *myddas_util_get_pred_name(void *pointer);

void myddas_util_delete_predicate(MYDDAS_UTIL_PREDICATE to_delete);

const char *myddas_util_get_pred_module(void *pointer);

void *myddas_util_get_list_pred(MYDDAS_UTIL_CONNECTION node);

void myddas_util_delete_predicate_list(MYDDAS_UTIL_PREDICATE preds_list);

MYDDAS_UTIL_CONNECTION myddas_util_search_connection(void *con);

MYDDAS_UTIL_PREDICATE myddas_util_find_predicate(const char *pred_name,
                                                 Int pred_arity,
                                                 const char *pred_module,
                                                 MYDDAS_UTIL_PREDICATE list);

MYDDAS_UTIL_CONNECTION myddas_util_add_connection(void *conn, void *enviromment,
                                                  MYDDAS_API api);
void myddas_util_delete_connection(void *conn);

MYDDAS_UTIL_PREDICATE
myddas_init_initialize_predicate(const char *pred_name, int pred_arity,
                                 const char *pred_module,
                                 MYDDAS_UTIL_PREDICATE next);

MYDDAS_UTIL_CONNECTION
myddas_init_initialize_connection(void *conn, void *enviromment, MYDDAS_API api,
                                  MYDDAS_UTIL_CONNECTION next);
