
#ifdef MULTI_ASSIGNMENT_VARIABLES
/* 
   Set of routines to allow restoring updatable variables when we go *up*
   in the tree. Required by copying, SBA, and tabling. Not required by ACOW.
*/

#ifndef OPT_MAVAR_STATIC
#define OPT_MAVAR_STATIC inline static
#endif

#define MAVARS_HASH_SIZE 512

typedef struct ma_h_entry {
  CELL* addr;
  struct ma_h_entry *next;
} ma_h_inner_struct;

typedef struct {
  UInt timestmp;
  struct ma_h_entry val;
} ma_hash_entry;

extern ma_hash_entry ma_hash_table[MAVARS_HASH_SIZE];

extern UInt timestamp;    /* an unsigned int */

OPT_MAVAR_STATIC unsigned int MAVAR_HASH(CELL *);
OPT_MAVAR_STATIC struct ma_h_entry *ALLOC_NEW_MASPACE(void);
OPT_MAVAR_STATIC int lookup_ma_var(CELL *);
OPT_MAVAR_STATIC UInt NEW_MAHASH(ma_h_inner_struct *);

OPT_MAVAR_STATIC unsigned int
MAVAR_HASH(CELL *addr) {
#if SIZEOF_INT_P==8
  return((((unsigned int)((CELL)(addr)))>>3)%MAVARS_HASH_SIZE);
#else
  return((((unsigned int)((CELL)(addr)))>>2)%MAVARS_HASH_SIZE); 
#endif
}

extern ma_h_inner_struct *ma_h_top;

OPT_MAVAR_STATIC struct ma_h_entry *
ALLOC_NEW_MASPACE(void)
{
  ma_h_inner_struct *new = ma_h_top;
  ma_h_top++;
  return(new);
}

OPT_MAVAR_STATIC int
lookup_ma_var(CELL *addr) {
  unsigned int i = MAVAR_HASH(addr);
  struct ma_h_entry *nptr, *optr;

  if (ma_hash_table[i].timestmp != timestamp) {
    ma_hash_table[i].timestmp = timestamp;
    ma_hash_table[i].val.addr = addr;
    ma_hash_table[i].val.next = NULL;
    return(FALSE);
  }
  if (ma_hash_table[i].val.addr == addr) 
    return(TRUE);
  optr = &(ma_hash_table[i].val);
  nptr = ma_hash_table[i].val.next;
  while (nptr != NULL) {
    if (nptr->addr == addr) {
      return(TRUE);
    }
    optr = nptr;
    nptr = nptr->next;
  }
  nptr = ALLOC_NEW_MASPACE();
  nptr->addr = addr;
  nptr->next = optr;
  return(FALSE);
}

OPT_MAVAR_STATIC UInt
NEW_MAHASH(ma_h_inner_struct *top) {
  UInt time = ++timestamp;
  if (time == 0) {
    unsigned int i;
    /* damn, we overflowed */
    for (i = 0; i < MAVARS_HASH_SIZE; i++)
      ma_hash_table[i].timestmp = 0;
    time = ++timestamp;
  }
  ma_h_top = top;
  return(time);
}

#endif
