#ifndef VECTOR_H
#define VECTOR_H

#include "stddef.h"

/*--------------------------------------------------------------------*/

#define VECTOR_INIT(v) \
	((v) = vector_create(sizeof(*(v)), 0, 0))
#define VECTOR_INIT_SIZE(v, n) \
	((v) = vector_create(sizeof(*(v)), n, n))
#define VECTOR_INIT_CAPA(v, m) \
	((v) = vector_create(sizeof(*(v)), 0, m))

#define VECTOR_FREE(v) \
	((v) = (vector_delete(v), NULL))

/*--------------------------------------------------------------------*/

#define VECTOR_SIZE(v) \
	((size_t)(((const size_t *)(v))[-1]))
#define VECTOR_CAPA(v) \
	((size_t)(((const size_t *)(v))[-2]))

#define VECTOR_PUSH(v, x) \
	((v) = vector_expand(v, sizeof(*(v))), (v)[VECTOR_SIZE(v) - 1] = (x))
#define VECTOR_POP(v) \
	((v) = vector_reduce(v), (v)[VECTOR_SIZE(v)])

#define VECTOR_PUSH_NONE(v) \
	((v) = vector_expand(v, sizeof(*(v))))

#define VECTOR_RESIZE(v, n) \
	((v) = vector_resize(v, sizeof(*(v)), n))
#define VECTOR_RESERVE(v, m) \
	((v) = vector_reserve(v, sizeof(*(v)), m))
#define VECTOR_STRIP(v) \
	((v) = vector_realloc(v, sizeof(*(v)), VECTOR_SIZE(v)))

#define VECTOR_CLEAR(v) \
	((void)(((const size_t *)(v))[-1] = 0))
#define VECTOR_EMPTY(v) \
	(VECTOR_SIZE(v) == 0)

/*--------------------------------------------------------------------*/

void *  vector_create(size_t, size_t, size_t);
void    vector_delete(void *);

void *  vector_expand(void *, size_t);
void *  vector_reduce(void *);

void *  vector_resize(void *, size_t, size_t);
void *  vector_reserve(void *, size_t, size_t);
void *  vector_realloc(void *, size_t, size_t);

/*--------------------------------------------------------------------*/

#endif /* VECTOR_H */
