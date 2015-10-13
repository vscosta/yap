#ifndef _UTHASH_UDI_PRIVATE_
#define _UTHASH_UDI_PRIVATE_

#include "uthash.h"

union AI {
  YAP_Atom atom;
  YAP_Int integer;
};

struct UTHash
{
  union AI key;
  void *data;
  UT_hash_handle hh;
};
typedef struct UTHash *uthash_t;

/*
  Used to Iterate over equal keys in hash table
*/
#define HASH_FIND_NEXT(hh,last,keyptr,keylen_in)                        \
  do {                                                                  \
    if (last->hh.hh_next)                                               \
      DECLTYPE_ASSIGN(last,ELMT_FROM_HH(last->hh.tbl,last->hh.hh_next)); \
    else last = NULL;                                                   \
    while (last) {                                                      \
      if (last->hh.keylen == keylen_in) {                               \
        if ((HASH_KEYCMP(last->hh.key,keyptr,keylen_in)) == 0) {        \
          break;                                                        \
        }                                                               \
      }                                                                 \
      if (last->hh.hh_next)                                             \
        DECLTYPE_ASSIGN(last,ELMT_FROM_HH(last->hh.tbl,last->hh.hh_next)); \
      else last = NULL;                                                 \
    }                                                                   \
  } while (0)                                                           \

/* to ease code for a Atom hash table*/
#define HASH_FIND_AI(head,find,out)               \
  HASH_FIND(hh,head,find,sizeof(union AI),out)
#define HASH_ADD_AI(head,add)                 \
  HASH_ADD(hh,head,key,sizeof(union AI),add)
#define HASH_FIND_NEXT_AI(last,find)       \
  HASH_FIND_NEXT(hh,last,find,sizeof(union AI))

#endif /* _UTHASH_UDI_PRIVATE_ */
