/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

#ifdef MULTI_ASSIGNMENT_VARIABLES
/* 
   Set of routines to allow restoring updatable variables when we go *up*
   in the tree. Required by copying, SBA, and tabling. Not required by ACOW.
*/

#ifndef OPT_MAVAR_STATIC
#define OPT_MAVAR_STATIC inline static
#endif /* !OPT_MAVAR_STATIC */

OPT_MAVAR_STATIC unsigned int Yap_MAVAR_HASH(CELL *addr);
OPT_MAVAR_STATIC struct ma_h_entry * Yap_ALLOC_NEW_MASPACE(void);
OPT_MAVAR_STATIC int Yap_lookup_ma_var(CELL *addr);
OPT_MAVAR_STATIC UInt Yap_NEW_MAHASH(ma_h_inner_struct *top);

OPT_MAVAR_STATIC unsigned int
Yap_MAVAR_HASH(CELL *addr) {
#if SIZEOF_INT_P==8
  return((((unsigned int)((CELL)(addr)))>>3)%MAVARS_HASH_SIZE);
#else
  return((((unsigned int)((CELL)(addr)))>>2)%MAVARS_HASH_SIZE); 
#endif
}

OPT_MAVAR_STATIC struct ma_h_entry *
Yap_ALLOC_NEW_MASPACE(void)
{
  ma_h_inner_struct *new = LOCAL_ma_h_top;
  LOCAL_ma_h_top++;
  return new;
}

OPT_MAVAR_STATIC int
Yap_lookup_ma_var(CELL *addr) {
  unsigned int i = Yap_MAVAR_HASH(addr);
  struct ma_h_entry *nptr, *optr;

  if (LOCAL_ma_hash_table[i].timestmp != LOCAL_ma_timestamp) {
    LOCAL_ma_hash_table[i].timestmp = LOCAL_ma_timestamp;
    LOCAL_ma_hash_table[i].val.addr = addr;
    LOCAL_ma_hash_table[i].val.next = NULL;
    return FALSE;
  }
  if (LOCAL_ma_hash_table[i].val.addr == addr) 
    return TRUE;
  optr = &(LOCAL_ma_hash_table[i].val);
  nptr = LOCAL_ma_hash_table[i].val.next;
  while (nptr != NULL) {
    if (nptr->addr == addr) {
      return TRUE;
    }
    optr = nptr;
    nptr = nptr->next;
  }
  nptr = Yap_ALLOC_NEW_MASPACE();
  nptr->addr = addr;
  nptr->next = optr;
  return FALSE;
}

OPT_MAVAR_STATIC UInt
Yap_NEW_MAHASH(ma_h_inner_struct *top) {
  UInt time = ++LOCAL_ma_timestamp;
  if (time == 0) {
    unsigned int i;
    /* damn, we overflowed */
    for (i = 0; i < MAVARS_HASH_SIZE; i++)
      LOCAL_ma_hash_table[i].timestmp = 0;
    time = ++LOCAL_ma_timestamp;
  }
  LOCAL_ma_h_top = top;
  return time;
}
#endif /* MULTI_ASSIGNMENT_VARIABLES */
