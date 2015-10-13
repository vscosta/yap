#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

/*
 * hack to emulate flexible array member of C99
 *
 * Example
 *
 * struct header {
 *    ...
 *    int data[FLEXIBLE_SIZE];
 * };
 *
 * ...
 *
 * size_t n = 123;
 * struct header *my_header = malloc(SIZEOF_FLEXIBLE(struct header, data, n));
 *
 */
#include <stddef.h>
#define FLEXIBLE_SIZE 1
#define SIZEOF_FLEXIBLE(type, member, length) \
  ( offsetof(type, member) + (length) * sizeof ((type *)0)->member[0] )

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
