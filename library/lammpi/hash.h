/*
Copyright (C) 2004,2005,2006 (Nuno A. Fonseca) <nuno.fonseca@gmail.com>

This program is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License 
as published by the Free Software Foundation; either 
version 2 of the License, or (at your option) any later 
version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


Last rev: $Id: hash.h,v 1.2 2006-06-04 19:02:07 nunofonseca Exp $
*/
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
