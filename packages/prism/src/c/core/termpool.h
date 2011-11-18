#ifndef TERMPOOL_H
#define TERMPOOL_H

#include "bpx.h"

/*--------------------------------------------------------------------*/

typedef struct term_pool TERM_POOL;

/*--------------------------------------------------------------------*/

TERM_POOL * term_pool_create(void);
void        term_pool_delete(TERM_POOL *);

TERM        term_pool_retrieve(const TERM_POOL *, TERM);
TERM        term_pool_register(TERM_POOL *, TERM);

/*--------------------------------------------------------------------*/

#endif /* TERMPOOL_H */
