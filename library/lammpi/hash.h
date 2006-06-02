/*===========================================================================================% 
 * Copyright (c) 2002,2003,2004,2005,2006 Nuno Fonseca. All rights reserved.
 * This code is freely available for academic purposes.
 * If you intend to use it for commercial purposes then please contact the author first.
 *
 * Author: Nuno Fonseca
 * Date: 2002-07-10
 * $Id: hash.h,v 1.1 2006-06-02 04:16:31 nunofonseca Exp $
 *
 *===========================================================================================*/
#ifndef HASH
#define HASH
#include <stdlib.h>
#if defined (__cplusplus) || (defined (__STDC__) && __STDC__)
#define __ptr_t         void *
#else /* Not C++ or ANSI C.  */
#define __ptr_t         char *
#endif /* C++ or ANSI C.  */           

#ifndef ulong
#define ulong unsigned long int
#endif

#ifndef NULL
#define NULL    0
#endif  


struct bucket {
 struct bucket *next;
 ulong value;      /* Value >=0 used as key in the hashing*/ 
 __ptr_t  obj;     /* pointer to a object*/
};
typedef struct bucket  hashnode;


struct hashtable_s {
  hashnode **buckets; //
  ulong size;         // number of buckets
  ulong last_bucket; // used in searchs/ hash traversals
  ulong n_entries; // number of entries in the hashtable
  hashnode* last_node;
};

//typedef hashnode **hashtable;
typedef struct hashtable_s* hashtable;

/* functions */
hashtable new_hashtable(ulong hashsize);
__ptr_t get_next_object(hashtable,ulong);
__ptr_t delete(hashtable,ulong);
__ptr_t replace_object(hashtable,ulong,__ptr_t);
__ptr_t get_object(hashtable,ulong);
int insere(hashtable,ulong,__ptr_t);
void free_hashtable(hashtable);

void init_hash_traversal(hashtable table);
__ptr_t next_hash_object(hashtable table);
__ptr_t next_hashnode(hashtable table);
#endif
