#include <stdio.h>
#include <stdlib.h>
#include "core/xmalloc.h"

/*--------------------------------------------------------------------*/

void * xmalloc
(size_t size, const char *file, unsigned int line)
{
    void *ptr;
    ptr = malloc(size);

    if (ptr == NULL) {
        fprintf(stderr, "Out of memory in %s(%u)\n", file, line);
        exit(1); /* FIXME */
    }

    return ptr;
}

void * xrealloc
(void *oldptr, size_t size, const char *file, unsigned int line)
{
    void *newptr;
    newptr = realloc(oldptr, size);

    if (newptr == NULL && size > 0) {
        fprintf(stderr, "Out of memory in %s(%u)\n", file, line);
        exit(1); /* FIXME */
    }

    return newptr;
}

/*--------------------------------------------------------------------*/
