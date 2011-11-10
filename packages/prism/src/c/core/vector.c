#include "core/xmalloc.h"
#include "core/vector.h"
#include <assert.h>

/*--------------------------------------------------------------------*/

#define INITIAL_CAPA 16

#undef VECTOR_SIZE
#undef VECTOR_CAPA

/* allow these to be L-values */
#define VECTOR_SIZE(v) (((size_t *)(v))[-1])
#define VECTOR_CAPA(v) (((size_t *)(v))[-2])

/*--------------------------------------------------------------------*/

void * vector_create(size_t unit, size_t size, size_t capa)
{
    void *ptr, *vec;
    ptr = MALLOC(sizeof(size_t) * 2 + unit * capa);
    vec = ((size_t *)(ptr)) + 2;
    VECTOR_SIZE(vec) = size;
    VECTOR_CAPA(vec) = capa;
    return vec;
}

void vector_delete(void *vec)
{
    free(((size_t *)(vec)) - 2);
}

void * vector_expand(void *vec, size_t unit)
{
    size_t capa;

    if (VECTOR_SIZE(vec) >= VECTOR_CAPA(vec)) {
        capa = VECTOR_CAPA(vec) * 2;
        if (capa < INITIAL_CAPA) {
            capa = INITIAL_CAPA;
        }
        vec = vector_realloc(vec, unit, capa);
    }

    ++(VECTOR_SIZE(vec));
    return vec;
}

void * vector_reduce(void *vec)
{
    assert(VECTOR_SIZE(vec) > 0);
    --(VECTOR_SIZE(vec));
    return vec;
}

void * vector_resize(void *vec, size_t unit, size_t size)
{
    vec = vector_reserve(vec, unit, size);
    VECTOR_SIZE(vec) = size;
    return vec;
}

void * vector_reserve(void *vec, size_t unit, size_t capa)
{
    if (VECTOR_CAPA(vec) < capa) {
        vec = vector_realloc(vec, unit, capa);
    }
    return vec;
}

void * vector_realloc(void *vec, size_t unit, size_t capa)
{
    void *ptr;

    if (VECTOR_CAPA(vec) == capa)
        return vec;

    assert(VECTOR_SIZE(vec) <= capa);

    ptr = ((size_t *)(vec)) - 2;
    ptr = REALLOC(ptr, sizeof(size_t) * 2 + unit * capa);
    vec = ((size_t *)(ptr)) + 2;
    VECTOR_CAPA(vec) = capa;
    return vec;
}

/*--------------------------------------------------------------------*/
