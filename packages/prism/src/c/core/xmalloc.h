#ifndef XMALLOC_H
#define XMALLOC_H

#include <stdlib.h>

/*--------------------------------------------------------------------*/

void * xmalloc(size_t, const char *, unsigned int);
void * xrealloc(void *, size_t, const char *, unsigned int);

/*--------------------------------------------------------------------*/

#ifdef MALLOC_TRACE
#  define MALLOC(size) malloc((size))
#  define REALLOC(oldptr,size) realloc((oldptr),(size))
#  define FREE(ptr) (free(ptr), (ptr) = NULL)
#else
#  define MALLOC(size) xmalloc((size), __FILE__, __LINE__)
#  define REALLOC(oldptr,size) xrealloc((oldptr), (size), __FILE__, __LINE__)
#  define FREE(ptr) (free(ptr), (ptr) = NULL)
#endif

/*--------------------------------------------------------------------*/

#endif /* XMALLOC_H */
