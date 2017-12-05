p #include<string.h>
#include <stdlib.h>

#include "Yap.h"
#include "cut_c.h"

    extern MYDDAS_UTIL_CONNECTION
    myddas_init_initialize_connection(void *conn, void *enviromment,
                                      MYDDAS_API api,
                                      MYDDAS_UTIL_CONNECTION next);

extern MYDDAS_UTIL_CONNECTION
myddas_util_add_connection(void *conn, void *enviromment, MYDDAS_API api);

extern MYDDAS_UTIL_PREDICATE
myddas_init_initialize_predicate(const char *pred_name, int pred_arity,
                                 const char *pred_module,
                                 MYDDAS_UTIL_PREDICATE next);

extern MYDDAS_UTIL_PREDICATE
myddas_util_find_predicate(const char *pred_name, Int pred_arity,
                           const char *pred_module, MYDDAS_UTIL_PREDICATE list);
