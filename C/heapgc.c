/*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		heapgc.c * Last rev:
 ** mods: * comments:	Global Stack garbage collector *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif /* SCCS */

#include "absmi.h"
#include "alloc.h"
#include "attvar.h"
#include "yapio.h"

#if !defined(TABLING)
//#define EASY_SHUNTING 1
#endif /* !TABLING */
#define HYBRID_SCHEME 1

#define DEBUG_printf0(A, B)
#define DEBUG_printf1(A, B, C)
#define DEBUG_printf20(A, B)
#define DEBUG_printf21(A, B, C)

/* global variables for garbage collection */

static Int p_inform_gc(CACHE_TYPE1);
static Int p_gc(CACHE_TYPE1);
static void marking_phase(tr_fr_ptr, CELL *, yamop *CACHE_TYPE);
static void compaction_phase(tr_fr_ptr, CELL *, yamop *CACHE_TYPE);
static void init_dbtable(tr_fr_ptr CACHE_TYPE);
static void mark_external_reference(CELL *CACHE_TYPE);
static void mark_db_fixed(CELL *CACHE_TYPE);
static void mark_regs(tr_fr_ptr CACHE_TYPE);
static void mark_trail(tr_fr_ptr, tr_fr_ptr, CELL *, choiceptr CACHE_TYPE);
static void mark_environments(CELL *, size_t, CELL *, yamop * CACHE_TYPE);
static void mark_choicepoints(choiceptr, tr_fr_ptr, bool CACHE_TYPE);
static void into_relocation_chain(CELL *, CELL *CACHE_TYPE);
static void sweep_trail(choiceptr, tr_fr_ptr CACHE_TYPE);
static void sweep_environments(CELL *, size_t, CELL *, yamop * CACHE_TYPE);
static void sweep_choicepoints(choiceptr CACHE_TYPE);
static void compact_heap(CACHE_TYPE1);
static void update_relocation_chain(CELL *, CELL *CACHE_TYPE);
static bool is_gc_verbose(void);
static bool is_gc_very_verbose(void);
static void LeaveGCMode(CACHE_TYPE1);
#ifdef EASY_SHUNTING
static void set_conditionals(tr_fr_ptr CACHE_TYPE);
#endif /* EASY_SHUNTING */

#include "heapgc.h"

typedef struct gc_mark_continuation {
  CELL *v;
  int nof;
} cont;

/* straightforward binary tree scheme that, given a key, finds a
   matching dbref */

typedef enum {
  db_entry,
  cl_entry,
  lcl_entry,
  li_entry,
  dcl_entry
} db_entry_type;

typedef struct db_entry {
  CODEADDR val;
  db_entry_type db_type;
  int in_use;
  struct db_entry *left;
  CODEADDR lim;
  struct db_entry *right;
} * dbentry;

typedef struct RB_red_blk_node {
  CODEADDR key;
  CODEADDR lim;
  db_entry_type db_type;
  int in_use;
  int red; /* if red=0 then the node is black */
  struct RB_red_blk_node *left;
  struct RB_red_blk_node *right;
  struct RB_red_blk_node *parent;
} rb_red_blk_node;

#ifdef EASY_SHUNTING
#undef LOCAL_cont_top0
#define LOCAL_cont_top0 (cont *)LOCAL_sTR
#endif

/* support for hybrid garbage collection scheme */

yamop *Yap_gcP(void) {
  CACHE_REGS
  return gc_P(P, CP);
}

/* support for hybrid garbage collection scheme */

static void gc_growtrail(int committed, tr_fr_ptr begsTR,
                         cont *old_cont_top0 USES_REGS) {
  UInt sz = LOCAL_TrailTop - (ADDR)LOCAL_OldTR;
  /* ask for double the size */
  sz = 2 * sz;

  if (!Yap_locked_growtrail(sz, TRUE)) {
#ifdef EASY_SHUNTING
    if (begsTR) {
      LOCAL_sTR = (tr_fr_ptr)old_cont_top0;
      while (begsTR != NULL) {
        tr_fr_ptr newsTR = (tr_fr_ptr)TrailTerm(begsTR);
        TrailTerm(LOCAL_sTR) = TrailTerm(begsTR + 1);
        TrailTerm(LOCAL_sTR + 1) = TrailTerm(begsTR + 2);
        begsTR = newsTR;
        LOCAL_sTR += 2;
      }
    }
    set_conditionals(LOCAL_sTR PASS_REGS);
#endif
    /* could not find more trail */
    save_machine_regs();
    siglongjmp(LOCAL_gc_restore, 2);
  }
}

inline static void PUSH_CONTINUATION(CELL *v, int nof USES_REGS) {
  cont *x;
  x = LOCAL_cont_top;
  x++;
  if ((ADDR)x > LOCAL_TrailTop - 1024) {
    gc_growtrail(TRUE, NULL, NULL PASS_REGS);
  }
  x->v = v;
  x->nof = nof;
  LOCAL_cont_top = x;
}

#define POP_CONTINUATION()                                                     \
  {                                                                            \
    if (LOCAL_cont_top == LOCAL_cont_top0)                                     \
      return;                                                                  \
    else {                                                                     \
      int nof = LOCAL_cont_top->nof;                                           \
      cont *x = LOCAL_cont_top;                                                \
                                                                               \
      current = x->v;                                                          \
      if (nof == 1)                                                            \
        LOCAL_cont_top = --x;                                                  \
      else {                                                                   \
        x->nof = nof - 1;                                                      \
        x->v = current + 1;                                                    \
      }                                                                        \
    }                                                                          \
    goto begin;                                                                \
  }

#ifdef HYBRID_SCHEME

inline static void PUSH_POINTER(CELL *v USES_REGS) {
  if (LOCAL_iptop >= (CELL_PTR *)ASP)
    return;
  *LOCAL_iptop++ = v;
}

#ifdef EASY_SHUNTING
inline static void POP_POINTER(USES_REGS1) {
  if (LOCAL_iptop >= (CELL_PTR *)ASP)
    return;
  --LOCAL_iptop;
}
#endif

inline static void POPSWAP_POINTER(CELL_PTR *vp, CELL_PTR v USES_REGS) {
  if (LOCAL_iptop >= (CELL_PTR *)ASP || LOCAL_iptop == vp)
    return;
  if (*vp != v)
    return;
  --LOCAL_iptop;
  if (vp != LOCAL_iptop)
    *vp = *LOCAL_iptop;
}

/*
  original code from  In Hyuk Choi,
  found at http://userpages.umbc.edu/~ichoi1/project/cs441.htm
*/

static inline void exchange(CELL_PTR *b, Int i, Int j) {
  CELL *t = b[j];

  b[j] = b[i];
  b[i] = t;
}

static UInt partition(CELL *a[], Int p, Int r) {
  CELL *x;
  UInt i, j;

  x = a[p];
  i = p + 1;
  j = r;

  while (a[j] > x && i < j) {
    j--;
  }
  while (a[i] < x && i < j) {
    i++;
  }
  while (i < j) {
    exchange(a, i, j);
    i++;
    j--;
    while (a[j] > x && i < j) {
      j--;
    }
    while (a[i] < x && i < j) {
      i++;
    }
  }
  if (a[i] > x)
    i--;
  exchange(a, p, i);
  return (i);
}

static void insort(CELL *a[], Int p, Int q) {
  Int j;

  for (j = p + 1; j <= q; j++) {
    CELL *key;
    Int i;

    key = a[j];
    i = j;

    while (i > p && a[i - 1] > key) {
      a[i] = a[i - 1];
      i--;
    }
    a[i] = key;
  }
}

static void quicksort(CELL *a[], Int p, Int r) {
  Int q;
  if (p < r) {
    if (r - p < 100) {
      insort(a, p, r);
      return;
    }
    exchange(a, p, (p + r) / 2);
    q = partition(a, p, r);
    quicksort(a, p, q - 1);
    quicksort(a, q + 1, r);
  }
}

#else

#define PUSH_POINTER(P PASS_REGS)
#define POP_POINTER(PASS_REGS1)
#define POPSWAP_POINTER(P)

#endif /* HYBRID_SCHEME */

#ifdef MULTI_ASSIGNMENT_VARIABLES
/*
   Based in opt.mavar.h. This is a set of routines to find out if a
   ma trail entry has appeared before in the same trail segment. All ma
   entries for the same cell are then linked. At the end of mark_trail() only
   one will remain.
*/

static inline unsigned int GC_MAVAR_HASH(CELL *addr) {
#if SIZEOF_INT_P == 8
  return ((((unsigned int)((CELL)(addr))) >> 3) % GC_MAVARS_HASH_SIZE);
#else
  return ((((unsigned int)((CELL)(addr))) >> 2) % GC_MAVARS_HASH_SIZE);
#endif
}

static inline gc_ma_hash_entry *GC_ALLOC_NEW_MASPACE(USES_REGS1) {
  gc_ma_hash_entry *new = LOCAL_gc_ma_h_top;
  if ((char *)LOCAL_gc_ma_h_top > LOCAL_TrailTop - 1024)
    gc_growtrail(FALSE, NULL, NULL PASS_REGS);
  LOCAL_gc_ma_h_top++;
  LOCAL_cont_top = (cont *)LOCAL_gc_ma_h_top;
#ifdef EASY_SHUNTING
  LOCAL_sTR = LOCAL_sTR0 = (tr_fr_ptr)LOCAL_cont_top;
#else
  LOCAL_cont_top0 = LOCAL_cont_top;
#endif
  return new;
}

static inline gc_ma_hash_entry *gc_lookup_ma_var(CELL *addr,
                                                 tr_fr_ptr trp USES_REGS) {
  unsigned int i = GC_MAVAR_HASH(addr);
  gc_ma_hash_entry *nptr, *optr = NULL;

  if (LOCAL_gc_ma_hash_table[i].timestmp != LOCAL_gc_timestamp) {
    LOCAL_gc_ma_hash_table[i].timestmp = LOCAL_gc_timestamp;
    LOCAL_gc_ma_hash_table[i].addr = addr;
#if TABLING
    LOCAL_gc_ma_hash_table[i].loc = trp;
    LOCAL_gc_ma_hash_table[i].more = LOCAL_gc_ma_h_list;
    LOCAL_gc_ma_h_list = LOCAL_gc_ma_hash_table + i;
#endif /* TABLING */
    LOCAL_gc_ma_hash_table[i].next = NULL;
    return NULL;
  }
  nptr = LOCAL_gc_ma_hash_table + i;
  while (nptr) {
    optr = nptr;
    if (nptr->addr == addr) {
#if TABLING
      /*
        we're moving from oldest to more recent, so only a new entry
        has the correct new value
      */
      TrailVal(nptr->loc + 1) = TrailVal(trp + 1);
#endif /* TABLING */
      return nptr;
    }
    nptr = nptr->next;
  }
  nptr = GC_ALLOC_NEW_MASPACE(PASS_REGS1);
  optr->next = nptr;
  nptr->addr = addr;
#if TABLING
  nptr->loc = trp;
  nptr->more = LOCAL_gc_ma_h_list;
#endif /* TABLING */
  nptr->next = NULL;
  LOCAL_gc_ma_h_list = nptr;
  return NULL;
}

static inline void GC_NEW_MAHASH(gc_ma_hash_entry *top USES_REGS) {
  UInt time = ++LOCAL_gc_timestamp;

  LOCAL_gc_ma_h_list = NULL;
  if (time == 0) {
    unsigned int i;

    /* damn, we overflowed */
    for (i = 0; i < GC_MAVARS_HASH_SIZE; i++)
      LOCAL_gc_ma_hash_table[i].timestmp = 0L;
    time = ++LOCAL_gc_timestamp;
  }
  LOCAL_gc_ma_h_top = top;
  LOCAL_cont_top = (cont *)LOCAL_gc_ma_h_top;
#ifdef EASY_SHUNTING
  LOCAL_sTR = (tr_fr_ptr)LOCAL_cont_top;
#else
  LOCAL_cont_top0 = LOCAL_cont_top;
#endif
}

#endif

/* find all accessible objects on the heap and squeeze out all the rest */

static tr_fr_ptr check_pr_trail(tr_fr_ptr rc USES_REGS) {
  if ((tr_fr_ptr)LOCAL_TrailTop - TR < 1024) {
    size_t n = TR - rc;
    if (!Yap_locked_growtrail(0, TRUE) || TRUE) {
      /* could not find more trail */
      save_machine_regs();
      siglongjmp(LOCAL_gc_restore, 2);
    }
    rc = TR - n;
  }
  return rc;
}

/* push the active registers onto the trail for inclusion during gc */

static tr_fr_ptr push_registers(Int num_regs, yamop *nextop USES_REGS) {
  int i;
  StaticArrayEntry *sal = LOCAL_StaticArrays;
  tr_fr_ptr ret = TR;

  /* push array entries first */
  ArrayEntry *al = LOCAL_DynamicArrays;
  GlobalEntry *gl = LOCAL_GlobalVariables;
  TrailTerm(TR++) = LOCAL_GlobalArena;
  while (al) {
    ret = check_pr_trail(ret PASS_REGS);
 //   printf("al %p\n", TR);
    TrailTerm(TR++) = al->ValueOfVE;
    al = al->NextAE;
  }
  while (gl) {
    Term t = gl->global;
    if (!IsUnboundVar(&gl->global) && !IsAtomTerm(t) && !IsIntTerm(t)) {
      ret = check_pr_trail(ret PASS_REGS);
    //printf("gl %p\n", TR);
      // fprintf(stderr,"in=%s %p\n", gl->AtomOfGE->StrOfAE, gl->global);
      TrailTerm(TR++) = t;
    }
    gl = gl->NextGE;
  }
  while (sal) {
    if (sal->ArrayType == array_of_nb_terms) {
      UInt arity = -sal->ArrayEArity, i;
      for (i = 0; i < arity; i++) {
        Term tlive = sal->ValueOfVE.lterms[i].tlive;
        if (!IsVarTerm(tlive) ||
            !IsUnboundVar(&sal->ValueOfVE.lterms[i].tlive)) {
          ret = check_pr_trail(ret PASS_REGS);
  //  printf("sal %p\n", TR);
          TrailTerm(TR++) = tlive;
        }
      }
    }
    sal = sal->NextAE;
  }
  ret = check_pr_trail(ret PASS_REGS);
  TrailTerm(TR) = LOCAL_GcGeneration;
 //   printf("GcG %p\n", TR);
  TR++;
  //  printf("GcGP %p\n", TR);
  TrailTerm(TR) = LOCAL_GcPhase;
  TR++;
#ifdef COROUTINING
 //   printf("WP %p\n", TR);
    TrailTerm(TR) = LOCAL_WokenGoals;
  TrailTerm(TR + 1) = LOCAL_AttsMutableList;
  TR += 2;
#endif
  {
    CELL *curslot = LOCAL_SlotBase, *topslot = LOCAL_SlotBase + LOCAL_CurSlot;
    while (curslot < topslot) {
      // printf("%p <- %p\n", TR, topslot);
      ret = check_pr_trail(ret PASS_REGS);
    if (!IsVarTerm(*curslot) &&
          ((*curslot<(CELL)LOCAL_GlobalBase && * curslot>(CELL) HR))) {
        *curslot = TermFreeTerm;
      }
 //     printf("Sl %p %ld\n", TR, curslot-LOCAL_SlotBase);
      TrailTerm(TR++) = *curslot++;
    }
  }
  for (i = 1; i <= num_regs; i++) {
    ret = check_pr_trail(ret PASS_REGS);
 //     printf("X[%d] %p \n", i, TR);
    TrailTerm(TR++) = (CELL)XREGS[i];
  }
  /* push any live registers we might have hanging around */
  if (nextop->opc == Yap_opcode(_move_back) ||
      nextop->opc == Yap_opcode(_skip)) {
    CELL *lab = (CELL *)(nextop->y_u.l.l);
    CELL max = lab[0];
    Int curr = lab[1];
    lab += 2;
    if (max) {
      CELL i;
      for (i = 0L; i <= max; i++) {
        if (i == 8 * CellSize) {
          curr = lab[0];
          lab++;
        }
        if (curr & 1) {
          ret = check_pr_trail(ret PASS_REGS);
//	  printf("X[%ld] %p \n", i, TR);
          TrailTerm(TR++) = XREGS[i];
        }
        curr >>= 1;
      }
    }
  }
  return ret;
}

/* pop the corrected register values from the trail and update the registers */

static void pop_registers(Int num_regs, yamop *nextop USES_REGS) {
  int i;
  tr_fr_ptr ptr = TR;
  StaticArrayEntry *sal = LOCAL_StaticArrays;

  /* pop info on opaque variables */
  while (LOCAL_extra_gc_cells > LOCAL_extra_gc_cells_base) {
    YAP_Opaque_CallOnGCRelocate f;
    CELL *ptr = LOCAL_extra_gc_cells - 1;
    size_t n = ptr[0], t = ptr[-1];

    LOCAL_extra_gc_cells -= (n + 1);
    if ((f = Yap_blob_gc_relocate_handler(t))) {
      int out = (f)(Yap_BlobTag(t), Yap_BlobInfo(t), LOCAL_extra_gc_cells, n);
      if (out < 0) {
        /* error: we don't have enough room */
        /* could not find more trail */
        save_machine_regs();
        siglongjmp(LOCAL_gc_restore, 4);
      }
    }
  }

  /* pop array entries first */
  ArrayEntry *al = LOCAL_DynamicArrays;
  GlobalEntry *gl = LOCAL_GlobalVariables;

  LOCAL_GlobalArena = TrailTerm(ptr++);
  while (al) {
    al->ValueOfVE = TrailTerm(ptr++);
    al = al->NextAE;
  }
  while (gl) {
    Term t = gl->global;
    if (!IsUnboundVar(&gl->global) && !IsAtomTerm(t) && !IsIntTerm(t)) {
      // fprintf(stderr,"out=%s %p\n", gl->AtomOfGE->StrOfAE, gl->global);
      gl->global = TrailTerm(ptr++);
    }
    gl = gl->NextGE;
  }
  sal = LOCAL_StaticArrays;
  while (sal) {
    if (sal->ArrayType == array_of_nb_terms) {
      UInt arity = -sal->ArrayEArity;
      for (i = 0; i < arity; i++) {
        Term tlive = sal->ValueOfVE.lterms[i].tlive;
        if (!IsVarTerm(tlive) ||
            !IsUnboundVar(&sal->ValueOfVE.lterms[i].tlive)) {
          sal->ValueOfVE.lterms[i].tlive = TrailTerm(ptr++);
        }
      }
    }
    sal = sal->NextAE;
  }
   LOCAL_GcGeneration = TrailTerm(ptr++);
  LOCAL_GcPhase = TrailTerm(ptr++);
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
  LOCAL_WokenGoals = TrailTerm(ptr++);
  LOCAL_AttsMutableList = TrailTerm(ptr++);
#endif
#endif

  // copy slots back
  {
    CELL *curslot = LOCAL_SlotBase, *topslot = LOCAL_SlotBase + LOCAL_CurSlot;
    while (curslot < topslot) {
      *curslot++ = TrailTerm(ptr++);
    }
  }

  for (i = 1; i <= num_regs; i++)
    XREGS[i] = TrailTerm(ptr++);
  /* pop any live registers we might have hanging around */
#if 0
if (nextop->opc == Yap_opcode(_move_back) ||
      nextop->opc == Yap_opcode(_skip)) {
    CELL *lab = (CELL *)(nextop->y_u.l.l);
    CELL max = lab[0];
    Int curr = lab[1];
    lab += 2;
    if (max) {
      CELL i;
      for (i = 0L; i <= max; i++) {
        if (i == 8 * CellSize) {
          curr = lab[0];
          lab++;
        }
        if (curr & 1) {
          XREGS[i] = TrailTerm(ptr++);
        }
        curr >>= 1;
      }
    }
  }
 #endif
}

#if DEBUG && COUNT_CELLS_MARKED
static int count_cells_marked(void) {
  CELL *current;
  int found_marked = 0;

  for (current = H; current > H0; current--) {
    if (MARKED_PTR(current)) {
      found_marked++;
    }
  }
  return (found_marked);
}
#endif

static rb_red_blk_node *RBMalloc(UInt size USES_REGS) {
  ADDR new = LOCAL_db_vec;

  LOCAL_db_vec += size;
  if ((ADDR)LOCAL_db_vec > LOCAL_TrailTop - 1024) {
    gc_growtrail(FALSE, NULL, NULL PASS_REGS);
  }
  return (rb_red_blk_node *)new;
}

static rb_red_blk_node *RBTreeCreate(void) {
  CACHE_REGS
  rb_red_blk_node *temp;

  /*  see the comment in the rb_red_blk_tree structure in red_black_tree.h */
  /*  for information on nil and root */
  temp = LOCAL_db_nil = RBMalloc(sizeof(rb_red_blk_node) PASS_REGS);
  temp->parent = temp->left = temp->right = temp;
  temp->red = 0;
  temp->key = NULL;
  temp = RBMalloc(sizeof(rb_red_blk_node) PASS_REGS);
  temp->parent = temp->left = temp->right = LOCAL_db_nil;
  temp->key = NULL;
  temp->red = 0;
  return temp;
}

/* This is code originally written by Emin Martinian */

/***********************************************************************/
/*  FUNCTION:  LeftRotate */
/**/
/*  INPUTS:  This takes a tree so that it can access the appropriate */
/*           root and nil pointers, and the node to rotate on. */
/**/
/*  OUTPUT:  None */
/**/
/*  Modifies Input: tree, x */
/**/
/*  EFFECTS:  Rotates as described in _Introduction_To_Algorithms by */
/*            Cormen, Leiserson, Rivest (Chapter 14).  Basically this */
/*            makes the parent of x be to the left of x, x the parent of */
/*            its parent before the rotation and fixes other pointers */
/*            accordingly. */
/***********************************************************************/

static void LeftRotate(rb_red_blk_node *x USES_REGS) {
  rb_red_blk_node *y;
  rb_red_blk_node *rb_nil = LOCAL_db_nil;

  /*  I originally wrote this function to use the sentinel for */
  /*  nil to avoid checking for nil.  However this introduces a */
  /*  very subtle bug because sometimes this function modifies */
  /*  the parent pointer of nil.  This can be a problem if a */
  /*  function which calls LeftRotate also uses the nil sentinel */
  /*  and expects the nil sentinel's parent pointer to be unchanged */
  /*  after calling this function.  For example, when RBDeleteFixUP */
  /*  calls LeftRotate it expects the parent pointer of nil to be */
  /*  unchanged. */

  y = x->right;
  x->right = y->left;

  if (y->left != rb_nil)
    y->left->parent = x; /* used to use sentinel here */
  /* and do an unconditional assignment instead of testing for nil */

  y->parent = x->parent;

  /* instead of checking if x->parent is the root as in the book, we */
  /* count on the root sentinel to implicitly take care of this case */
  if (x == x->parent->left) {
    x->parent->left = y;
  } else {
    x->parent->right = y;
  }
  y->left = x;
  x->parent = y;

#ifdef DEBUG_ASSERT
  Assert(!LOCAL_db_nil->red, "nil not red in LeftRotate");
#endif
}

/***********************************************************************/
/*  FUNCTION:  RighttRotate */
/**/
/*  INPUTS:  This takes a tree so that it can access the appropriate */
/*           root and nil pointers, and the node to rotate on. */
/**/
/*  OUTPUT:  None */
/**/
/*  Modifies Input?: tree, y */
/**/
/*  EFFECTS:  Rotates as described in _Introduction_To_Algorithms by */
/*            Cormen, Leiserson, Rivest (Chapter 14).  Basically this */
/*            makes the parent of x be to the left of x, x the parent of */
/*            its parent before the rotation and fixes other pointers */
/*            accordingly. */
/***********************************************************************/

static void RightRotate(rb_red_blk_node *y USES_REGS) {
  rb_red_blk_node *x;
  rb_red_blk_node *rb_nil = LOCAL_db_nil;

  /*  I originally wrote this function to use the sentinel for */
  /*  nil to avoid checking for nil.  However this introduces a */
  /*  very subtle bug because sometimes this function modifies */
  /*  the parent pointer of nil.  This can be a problem if a */
  /*  function which calls LeftRotate also uses the nil sentinel */
  /*  and expects the nil sentinel's parent pointer to be unchanged */
  /*  after calling this function.  For example, when RBDeleteFixUP */
  /*  calls LeftRotate it expects the parent pointer of nil to be */
  /*  unchanged. */

  x = y->left;
  y->left = x->right;

  if (rb_nil != x->right)
    x->right->parent = y; /*used to use sentinel here */
  /* and do an unconditional assignment instead of testing for nil */

  /* instead of checking if x->parent is the root as in the book, we */
  /* count on the root sentinel to implicitly take care of this case */
  x->parent = y->parent;
  if (y == y->parent->left) {
    y->parent->left = x;
  } else {
    y->parent->right = x;
  }
  x->right = y;
  y->parent = x;

#ifdef DEBUG_ASSERT
  Assert(!LOCAL_db_nil->red, "nil not red in RightRotate");
#endif
}

/***********************************************************************/
/*  FUNCTION:  TreeInsertHelp  */
/**/
/*  INPUTS:  tree is the tree to insert into and z is the node to insert */
/**/
/*  OUTPUT:  none */
/**/
/*  Modifies Input:  tree, z */
/**/
/*  EFFECTS:  Inserts z into the tree as if it were a regular binary tree */
/*            using the algorithm described in _Introduction_To_Algorithms_ */
/*            by Cormen et al.  This funciton is only intended to be called */
/*            by the RBTreeInsert function and not by the user */
/***********************************************************************/

static void TreeInsertHelp(rb_red_blk_node *z USES_REGS) {
  /*  This function should only be called by InsertRBTree (see above) */
  rb_red_blk_node *x;
  rb_red_blk_node *y;
  rb_red_blk_node *rb_nil = LOCAL_db_nil;

  z->left = z->right = rb_nil;
  y = LOCAL_db_root;
  x = LOCAL_db_root->left;
  while (x != rb_nil) {
    y = x;
    if (x->key < z->key) { /* x.key > z.key */
      x = x->left;
    } else { /* x,key <= z.key */
      x = x->right;
    }
  }
  z->parent = y;
  if ((y == LOCAL_db_root) || (y->key < z->key)) { /* y.key > z.key */
    y->left = z;
  } else {
    y->right = z;
  }

#ifdef DEBUG_ASSERT
  Assert(!LOCAL_db_nil->red, "nil not red in TreeInsertHelp");
#endif
}

/*  Before calling Insert RBTree the node x should have its key set */

/***********************************************************************/
/*  FUNCTION:  RBTreeInsert */
/**/
/*  INPUTS:  tree is the red-black tree to insert a node which has a key */
/*           pointed to by key and info pointed to by info.  */
/**/
/*  OUTPUT:  This function returns a pointer to the newly inserted node */
/*           which is guarunteed to be valid until this node is deleted. */
/*           What this means is if another data structure stores this */
/*           pointer then the tree does not need to be searched when this */
/*           is to be deleted. */
/**/
/*  Modifies Input: tree */
/**/
/*  EFFECTS:  Creates a node node which contains the appropriate key and */
/*            info pointers and inserts it into the tree. */
/***********************************************************************/

static rb_red_blk_node *RBTreeInsert(CODEADDR key, CODEADDR end,
                                     db_entry_type db_type USES_REGS) {
  rb_red_blk_node *y;
  rb_red_blk_node *x;
  rb_red_blk_node *newNode;

  x = (rb_red_blk_node *)RBMalloc(sizeof(rb_red_blk_node) PASS_REGS);
  x->key = key;
  x->lim = end;
  x->db_type = db_type;
  x->in_use = FALSE;

  TreeInsertHelp(x PASS_REGS);
  newNode = x;
  x->red = 1;
  while (x->parent->red) { /* use sentinel instead of checking for root */
    if (x->parent == x->parent->parent->left) {
      y = x->parent->parent->right;
      if (y->red) {
        x->parent->red = 0;
        y->red = 0;
        x->parent->parent->red = 1;
        x = x->parent->parent;
      } else {
        if (x == x->parent->right) {
          x = x->parent;
          LeftRotate(x PASS_REGS);
        }
        x->parent->red = 0;
        x->parent->parent->red = 1;
        RightRotate(x->parent->parent PASS_REGS);
      }
    } else { /* case for x->parent == x->parent->parent->right */
      y = x->parent->parent->left;
      if (y->red) {
        x->parent->red = 0;
        y->red = 0;
        x->parent->parent->red = 1;
        x = x->parent->parent;
      } else {
        if (x == x->parent->left) {
          x = x->parent;
          RightRotate(x PASS_REGS);
        }
        x->parent->red = 0;
        x->parent->parent->red = 1;
        LeftRotate(x->parent->parent PASS_REGS);
      }
    }
  }
  LOCAL_db_root->left->red = 0;
  return newNode;

#ifdef DEBUG_ASSERT
  Assert(!LOCAL_db_nil->red, "nil not red in RBTreeInsert");
  Assert(!LOCAL_db_root->red, "root not red in RBTreeInsert");
#endif
}

/* init the table */
static void store_in_dbtable(CODEADDR entry, CODEADDR end,
                             db_entry_type db_type USES_REGS) {
  RBTreeInsert(entry, end, db_type PASS_REGS);
}

/* find an element in the dbentries table */
static rb_red_blk_node *find_ref_in_dbtable(CODEADDR entry USES_REGS) {
  rb_red_blk_node *current = LOCAL_db_root->left;

  while (current != LOCAL_db_nil) {
    if (current->key <= entry && current->lim > entry) {
      return current;
    }
    if (entry < current->key)
      current = current->right;
    else
      current = current->left;
  }
  return current;
}

/* find an element in the dbentries table */
static void mark_ref_in_use(DBRef ref USES_REGS) {
  rb_red_blk_node *el = find_ref_in_dbtable((CODEADDR)ref PASS_REGS);
  el->in_use = TRUE;
}

static int ref_in_use(DBRef ref USES_REGS) {
  rb_red_blk_node *el = find_ref_in_dbtable((CODEADDR)ref PASS_REGS);
  return el->in_use;
}

static void mark_db_fixed(CELL *ptr USES_REGS) {
  rb_red_blk_node *el;

  el = find_ref_in_dbtable((CODEADDR)ptr PASS_REGS);
  if (el != LOCAL_db_nil) {
    el->in_use = TRUE;
  }
}

static void init_dbtable(tr_fr_ptr trail_ptr USES_REGS) {
  StaticClause *sc = DeadStaticClauses;
  MegaClause *mc = DeadMegaClauses;
  StaticIndex *si = DeadStaticIndices;

  LOCAL_extra_gc_cells = LOCAL_extra_gc_cells_base = (CELL *)TR;
  LOCAL_extra_gc_cells_top =
      LOCAL_extra_gc_cells_base + LOCAL_extra_gc_cells_size;
  if ((char *)LOCAL_extra_gc_cells_top > LOCAL_TrailTop - 1024)
    gc_growtrail(FALSE, NULL, NULL PASS_REGS);
  LOCAL_db_vec0 = LOCAL_db_vec = (ADDR)LOCAL_extra_gc_cells_top;
  LOCAL_db_root = RBTreeCreate();
  while (trail_ptr > (tr_fr_ptr)LOCAL_TrailBase) {
    register CELL trail_cell;

    trail_ptr--;

    trail_cell = TrailTerm(trail_ptr);

    if (!IsVarTerm(trail_cell) && IsPairTerm(trail_cell)) {
      CELL *pt0 = RepPair(trail_cell);
      /* DB pointer */
      CELL flags;

#ifdef FROZEN_STACKS /* TRAIL */
                     /* avoid frozen segments */
      if (
#ifdef YAPOR_SBA
          (ADDR)pt0 >= HeapTop
#else
          (ADDR)pt0 >= LOCAL_TrailBase && (ADDR)pt0 < LOCAL_TrailTop
#endif
      ) {
        continue;
      }
#endif /* FROZEN_STACKS */

      flags = *pt0;
      /* for the moment, if all references to the term in the stacks
         are only pointers, reset the flag */
      if (FlagOn(DBClMask, flags)) {
        DBRef dbr = DBStructFlagsToDBStruct(pt0);
        store_in_dbtable((CODEADDR)dbr,
                         (CODEADDR)dbr + sizeof(DBStruct) +
                             sizeof(CELL) * dbr->DBT.NOfCells,
                         db_entry PASS_REGS);
      } else if (flags & LogUpdMask) {
        if (flags & IndexMask) {
          LogUpdIndex *li = ClauseFlagsToLogUpdIndex(pt0);
          store_in_dbtable((CODEADDR)li, (CODEADDR)li + li->ClSize,
                           li_entry PASS_REGS);
        } else {
          LogUpdClause *cli = ClauseFlagsToLogUpdClause(pt0);
          store_in_dbtable((CODEADDR)cli, (CODEADDR)cli + cli->ClSize,
                           lcl_entry PASS_REGS);
        }
      } else {
        DynamicClause *dcl = ClauseFlagsToDynamicClause(pt0);
        store_in_dbtable((CODEADDR)dcl, (CODEADDR)dcl + dcl->ClSize,
                         dcl_entry PASS_REGS);
      }
    }
  }
  while (sc) {
    store_in_dbtable((CODEADDR)sc, (CODEADDR)sc + sc->ClSize,
                     dcl_entry PASS_REGS);
    sc = sc->ClNext;
  }
  while (si) {
    store_in_dbtable((CODEADDR)si, (CODEADDR)si + si->ClSize,
                     dcl_entry PASS_REGS);
    si = si->SiblingIndex;
  }
  while (mc) {
    store_in_dbtable((CODEADDR)mc, (CODEADDR)mc + mc->ClSize,
                     dcl_entry PASS_REGS);
    mc = mc->ClNext;
  }
  if (LOCAL_db_vec == LOCAL_db_vec0) {
    /* could not find any entries: probably using LOG UPD semantics */
    LOCAL_db_vec0 = NULL;
  }
}

#ifdef DEBUG

/* #define INSTRUMENT_GC 1 */

#ifdef INSTRUMENT_GC
typedef enum {
  gc_var,
  gc_ref,
  gc_atom,
  gc_int,
  gc_num,
  gc_list,
  gc_appl,
  gc_func,
  gc_susp
} gc_types;
unsigned long chain[16];
unsigned long env_vars;
unsigned long vars[gc_susp + 1];

unsigned long num_bs;
unsigned long old_vars, new_vars;

static CELL *TrueHB;

static void inc_vars_of_type(CELL *curr, gc_types val) {
  if (curr >= H0 && curr < TrueHB) {
    old_vars++;
  } else if (curr >= TrueHB && curr < HR) {
    new_vars++;
  } else {
    return;
  }
  vars[val]++;
}

static void put_type_info(unsigned long total) {
  fprintf(stderr, "%%  type info for %lu cells\n", total);
  fprintf(stderr, "%%      %lu vars\n", vars[gc_var]);
  fprintf(stderr, "%%      %lu refs\n", vars[gc_ref]);
  fprintf(stderr, "%%      %lu references from env\n", env_vars);
  fprintf(stderr, "%%      %lu atoms\n", vars[gc_atom]);
  fprintf(stderr, "%%      %lu small ints\n", vars[gc_int]);
  fprintf(stderr, "%%      %lu other numbers\n", vars[gc_num]);
  fprintf(stderr, "%%      %lu lists\n", vars[gc_list]);
  fprintf(stderr, "%%      %lu compound terms\n", vars[gc_appl]);
  fprintf(stderr, "%%      %lu functors\n", vars[gc_func]);
  fprintf(stderr, "%%      %lu suspensions\n", vars[gc_susp]);
}

static void inc_var(CELL *current, CELL *next) {
  int len = 1;
  CELL *mynext = next;

  if (ONHEAP(current)) {
    if (next == current) {
      inc_vars_of_type(current, gc_var);
      chain[0]++;
    } else {
      inc_vars_of_type(current, gc_ref);
      while (ONHEAP(mynext) && IsVarTerm(*mynext)) {
        CELL *prox = GET_NEXT(*mynext);
        if (prox == mynext) {
          chain[0]++;
          break;
        }
        len++;
        mynext = prox;
      }
      if (len >= 15)
        (chain[15])++;
      else
        (chain[len])++;
    }
  }
}
#endif /* INSTRUMENT_GC */

int vsc_stop(void);

int vsc_stop(void) { return (1); }

#endif

#ifdef CHECK_GLOBAL
static void check_global(void) {
  CELL *current;

#ifdef INSTRUMENT_GC
  vars[gc_var] = 0;
  vars[gc_ref] = 0;
  vars[gc_atom] = 0;
  vars[gc_int] = 0;
  vars[gc_num] = 0;
  vars[gc_list] = 0;
  vars[gc_appl] = 0;
  vars[gc_func] = 0;
  vars[gc_susp] = 0;
#endif
  for (current = H - 1; current >= H0; current--) {
    CELL ccurr = *current;

    if (MARKED_PTR(current)) {
      CELL ccell = UNMARK_CELL(ccurr);
      if (ccell == EndSpecials) {
        /* oops, we found a blob */
        CELL *ptr = current - 1;
        UInt nofcells;

        while (!MARKED_PTR(ptr))
          ptr--;
        nofcells = current - ptr;
        current = ptr;
        ccurr = *current;
        /* process the functor next */
      }
    }
#if INSTRUMENT_GC
    if (IsVarTerm(ccurr)) {
      if (IsBlobFunctor((Functor)ccurr))
        vars[gc_num]++;
      else if (ccurr != 0 && (ccurr < (CELL)LOCAL_GlobalBase ||
                              ccurr > (CELL)LOCAL_TrailTop)) {
        /*	printf("%p: %s/%d\n", current,
               RepAtom(NameOfFunctor((Functor)ccurr))->StrOfAE,
               ArityOfFunctor((Functor)ccurr));*/
        vars[gc_func]++;
      } else if (IsUnboundVar(current))
        vars[gc_var]++;
      else
        vars[gc_ref]++;
    } else if (IsApplTerm(ccurr)) {
      /*      printf("%p: f->%p\n",current,RepAppl(ccurr)); */
      vars[gc_appl]++;
    } else if (IsPairTerm(ccurr)) {
      /*      printf("%p: l->%p\n",current,RepPair(ccurr)); */
      vars[gc_list]++;
    } else if (IsAtomTerm(ccurr)) {
      /*      printf("%p: %s\n",current,RepAtom(AtomOfTerm(ccurr))->StrOfAE); */
      vars[gc_atom]++;
    } else if (IsIntTerm(ccurr)) {
      /*      printf("%p: %d\n",current,IntOfTerm(ccurr)); */
      vars[gc_int]++;
    }
#endif
  }
#if INSTRUMENT_GC
  put_type_info(H - H0);
  vars[gc_var] = 0;
  vars[gc_ref] = 0;
  vars[gc_atom] = 0;
  vars[gc_int] = 0;
  vars[gc_num] = 0;
  vars[gc_list] = 0;
  vars[gc_appl] = 0;
  vars[gc_func] = 0;
  vars[gc_susp] = 0;
#endif
}
#else
#define check_global()
#endif /* CHECK_GLOBAL */

/* mark a heap object and all heap objects accessible from it */

static void mark_variable(CELL_PTR current USES_REGS) {
  CELL_PTR next;
  register CELL ccur;
  unsigned int arity;
  char *local_bp = LOCAL_bp;

begin:
  if (current == 0 || UNMARKED_MARK(current, local_bp)) {
    POP_CONTINUATION();
  }
  if (current >= H0 && current < HR) {
    // fprintf(stderr,"%p M\n", current);
    LOCAL_total_marked++;
    if (current < LOCAL_HGEN) {
      LOCAL_total_oldies++;
    } else {
      DEBUG_printf0("%p 1\n", current);
    }
  }
  PUSH_POINTER(current PASS_REGS);
  ccur = *current;
  next = GET_NEXT(ccur);

  if (IsVarTerm(ccur)) {
    if (IN_BETWEEN(LOCAL_GlobalBase, current, HR) && GlobalIsAttVar(current) &&
        current == next) {
      if (next < H0)
        POP_CONTINUATION();
      if (!UNMARKED_MARK(next - 1, local_bp)) {
        // fprintf(stderr,"%p M\n", next-1);
        LOCAL_total_marked++;
        if (next - 1 < LOCAL_HGEN) {
          LOCAL_total_oldies++;
        } else {
          DEBUG_printf0("%p 1\n", next - 1);
        }
        PUSH_POINTER(next - 1 PASS_REGS);
      }
      PUSH_CONTINUATION(next + 1, 2 PASS_REGS);
      current = next;
      goto begin;
    } else if (ONHEAP(next)) {
#ifdef EASY_SHUNTING
      CELL cnext;
      /* do variable shunting between variables in the global */
      cnext = *next;

      if (!MARKED_PTR(next)) {
        if (IsVarTerm(cnext) && (CELL)next == cnext) {
          /* new global variable to new global variable */
          if (next > current && current < LOCAL_prev_HB && current >= HB &&
              next >= HB && next < LOCAL_prev_HB) {
#ifdef INSTRUMENT_GC
            inc_var(current, current);
#endif
            *next = (CELL)current;
            UNMARK(next);
            MARK(current);
            *current = (CELL)current;
            POP_CONTINUATION();
          } else {
            /* can't help here */
#ifdef INSTRUMENT_GC
            inc_var(current, next);
#endif
            current = next;
          }
        } else {
          /* binding to a determinate reference */
          if (next >= HB && current < LCL0 && cnext != TermFoundVar) {
            UNMARK(current);
            *current = cnext;
            if (current >= H0 && current < HR) {
              // fprintf(stderr,"%p M\n", current-1);
              LOCAL_total_marked--;
              if (current < LOCAL_HGEN) {
                LOCAL_total_oldies--;
              } else {
                DEBUG_printf0("%p-1\n", next - 1);
              }
            }
            POP_POINTER(PASS_REGS1);
          } else {
#ifdef INSTRUMENT_GC
            inc_var(current, next);
#endif
            current = next;
          }
        }
        /* try to shorten chains if they go through the current CP */
      } else if (next > HB && IsVarTerm(cnext) &&
                 UNMARK_CELL(cnext) != (CELL)next && current < LCL0) {
        /* This step is possible because we clean up the trail */
        *current = UNMARK_CELL(cnext);
        UNMARK(current);
        if (current >= H0 && current < HR) {
          // fprintf(stderr,"%p M\n", current);
          LOCAL_total_marked--;
          if (current < LOCAL_HGEN) {
            LOCAL_total_oldies--;
          } else {
            DEBUG_printf0("%p-1\n", next - 1);
          }
        }
        POP_POINTER(PASS_REGS1);
      } else
#endif
      /* what I'd do without variable shunting */
      {
#ifdef INSTRUMENT_GC
        inc_var(current, next);
#endif
        current = next;
      }
      goto begin;
#ifdef DEBUG
    } else if (next < (CELL *)LOCAL_GlobalBase ||
               next > (CELL *)LOCAL_TrailTop) {
      fprintf(stderr,
              "OOPS in GC: marking, TR=%p, current=%p, *current=" UInt_FORMAT
              " next=%p\n",
              TR, current, ccur, next);
#endif
    } else {
#ifdef COROUTING
      LOCAL_totalmarked++;
#endif
#ifdef INSTRUMENT_GC
      inc_var(current, next);
#endif
    }
    POP_CONTINUATION();
  } else if (IsAtomOrIntTerm(ccur)) {
#ifdef INSTRUMENT_GC
    if (IsAtomTerm(ccur))
      inc_vars_of_type(current, gc_atom);
    else
      inc_vars_of_type(current, gc_int);
#endif
    POP_CONTINUATION();
  } else if (IsPairTerm(ccur)) {
#ifdef INSTRUMENT_GC
    inc_vars_of_type(current, gc_list);
#endif
    if (ONHEAP(next)) {
      /* speedup for strings */
      if (IsAtomOrIntTerm(*next)) {
        if (!UNMARKED_MARK(next, local_bp)) {
          // fprintf(stderr,"%p M\n", next);
          LOCAL_total_marked++;
          if (next < LOCAL_HGEN) {
            LOCAL_total_oldies++;
          } else {
            DEBUG_printf0("%p 1\n", next);
          }
          PUSH_POINTER(next PASS_REGS);
        }
        current = next + 1;
        goto begin;
      } else {
        PUSH_CONTINUATION(next + 1, 1 PASS_REGS);
        current = next;
        goto begin;
      }
    } else if (ONCODE(next)) {
      mark_db_fixed(RepPair(ccur) PASS_REGS);
    }
    POP_CONTINUATION();
  } else if (IsApplTerm(ccur)) {
    register CELL cnext = *next;

#ifdef INSTRUMENT_GC
    if (!IsExtensionFunctor((Functor)cnext))
      inc_vars_of_type(current, gc_appl);
    else
      inc_vars_of_type(current, gc_num);
#endif
    if (ONCODE(next)) {
      if ((Functor)cnext == FunctorDBRef) {
        DBRef tref = DBRefOfTerm(ccur);

        /* make sure the reference is marked as in use */
        if ((tref->Flags & (ErasedMask | LogUpdMask)) ==
            (ErasedMask | LogUpdMask)) {
          *current = MkDBRefTerm((DBRef)LogDBErasedMarker);
          MARK(current);
        } else {
          mark_ref_in_use(tref PASS_REGS);
        }
      } else {
        mark_db_fixed(next PASS_REGS);
      }
      POP_CONTINUATION();
    }
    if (MARKED_PTR(next) || !ONHEAP(next))
      POP_CONTINUATION();

    if (next < H0)
      POP_CONTINUATION();
    if (IsExtensionFunctor((Functor)cnext)) {
      switch (cnext) {
      case (CELL)FunctorLongInt:
        MARK(next);
        MARK(next + 2);
        if (next < LOCAL_HGEN) {
          LOCAL_total_oldies += 3;
        } else {
          DEBUG_printf0("%p 1\n", next);
          DEBUG_printf0("%p 3\n", next);
        }
        // fprintf(stderr,"%p M 3\n", next);
        LOCAL_total_marked += 3;
        PUSH_POINTER(next PASS_REGS);
        PUSH_POINTER(next + 2 PASS_REGS);
        POP_CONTINUATION();
      case (CELL)FunctorDouble:
        MARK(next);
        PUSH_POINTER(next PASS_REGS);
        {
          UInt sz = 1 + SIZEOF_DOUBLE / SIZEOF_INT_P;
          if (next < LOCAL_HGEN) {
            LOCAL_total_oldies += 1 + sz;
          } else {
            DEBUG_printf0("%p 1\n", next);
            DEBUG_printf1("%p %ld\n", next, (long int)(sz + 1));
          }
          // fprintf(stderr,"%p M %d\n", next,1+sz);
          LOCAL_total_marked += 1 + sz;
          PUSH_POINTER(next + sz PASS_REGS);
          MARK(next + sz);
        }
        POP_CONTINUATION();
      case (CELL)FunctorString:
        MARK(next);
        PUSH_POINTER(next PASS_REGS);
        {
          UInt sz = 2 + next[1];
          if (next < LOCAL_HGEN) {
            LOCAL_total_oldies += 1 + sz;
          } else {
            DEBUG_printf0("%p 1\n", next);
            DEBUG_printf1("%p %ld\n", next, (long int)(sz + 1));
          }
          // fprintf(stderr,"%p M %d\n", next,1+sz);
          LOCAL_total_marked += 1 + sz;
          PUSH_POINTER(next + sz PASS_REGS);
          MARK(next + sz);
        }
        POP_CONTINUATION();
      case (CELL)FunctorBigInt: {
        YAP_Opaque_CallOnGCMark f;
        Term t = AbsAppl(next);
        UInt sz = (sizeof(MP_INT) + CellSize +
                   ((MP_INT *)(next + 2))->_mp_alloc * sizeof(mp_limb_t)) /
                  CellSize;

        MARK(next);
        if ((f = Yap_blob_gc_mark_handler(t))) {
          Int n = (f)(Yap_BlobTag(t), Yap_BlobInfo(t), LOCAL_extra_gc_cells,
                      LOCAL_extra_gc_cells_top - (LOCAL_extra_gc_cells + 2));
          if (n < 0) {
            /* error: we don't have enough room */
            /* could not find more trail */
            save_machine_regs();
            siglongjmp(LOCAL_gc_restore, 3);
          } else if (n > 0) {
            CELL *ptr = LOCAL_extra_gc_cells;

            LOCAL_extra_gc_cells += n + 2;
            PUSH_CONTINUATION(ptr, n + 1 PASS_REGS);
            ptr += n;
            ptr[0] = t;
            ptr[1] = n + 1;
          }
        }

        /* size is given by functor + friends */
        if (next < LOCAL_HGEN) {
          LOCAL_total_oldies += 2 + sz;
        } else {
          DEBUG_printf0("%p 1\n", next);
          DEBUG_printf1("%p %ld\n", next, (long int)(sz + 2));
        }
        // fprintf(stderr,"%p M %d\n", next,2+sz);
        LOCAL_total_marked += 2 + sz;
        PUSH_POINTER(next PASS_REGS);
        sz++;
#if DEBUG
        if (next[sz] != EndSpecials) {
          fprintf(
              stderr,
              "[ Error: could not find EndSpecials at blob %p type " UInt_FORMAT
              " ]\n",
              next, next[1]);
        }
#endif
        MARK(next + sz);
        PUSH_POINTER(next + sz PASS_REGS);
      }
      default:
        POP_CONTINUATION();
      }
    }
    if (next < H0)
      POP_CONTINUATION();
#ifdef INSTRUMENT_GC
    inc_vars_of_type(next, gc_func);
#endif
    arity = ArityOfFunctor((Functor)(cnext));
    MARK(next);
    // fprintf(stderr,"%p M\n", next);
    ++LOCAL_total_marked;
    if (next < LOCAL_HGEN) {
      ++LOCAL_total_oldies;
    } else {
      DEBUG_printf0("%p 1\n", next);
    }
    PUSH_POINTER(next PASS_REGS);
    next++;
    /* speedup for leaves */
    while (arity && IsAtomOrIntTerm(*next)) {
      if (!UNMARKED_MARK(next, local_bp)) {
        // fprintf(stderr,"%p M\n", next);
        LOCAL_total_marked++;
        if (next < LOCAL_HGEN) {
          LOCAL_total_oldies++;
        } else {
          DEBUG_printf0("%p 1\n", next);
        }
        PUSH_POINTER(next PASS_REGS);
      }
      next++;
      arity--;
    }
    if (!arity)
      POP_CONTINUATION();
    current = next;
    if (arity == 1)
      goto begin;
    PUSH_CONTINUATION(current + 1, arity - 1 PASS_REGS);
    goto begin;
  }
}

void Yap_mark_variable(CELL_PTR current) {
  CACHE_REGS
  mark_variable(current PASS_REGS);
}

static void mark_code(CELL_PTR ptr, CELL *next USES_REGS) {
  if (ONCODE(next)) {
    CELL reg = *ptr;
    if (IsApplTerm(reg) && (Functor)(*next) == FunctorDBRef) {
      DBRef tref = DBRefOfTerm(reg);
      /* make sure the reference is marked as in use */
      if ((tref->Flags & (LogUpdMask | ErasedMask)) ==
          (LogUpdMask | ErasedMask)) {
        *ptr = MkDBRefTerm((DBRef)LogDBErasedMarker);
      } else {
        mark_ref_in_use(tref PASS_REGS);
      }
    } else {
      mark_db_fixed(next PASS_REGS);
    }
  }
}

static void mark_external_reference(CELL *ptr USES_REGS) {
  CELL *next = GET_NEXT(*ptr);

  if (ONHEAP(next)) {
#ifdef HYBRID_SCHEME
    CELL_PTR *old = LOCAL_iptop;
#endif
    mark_variable(ptr PASS_REGS);
    POPSWAP_POINTER(old, ptr PASS_REGS);
  } else if (next < H0 || next > (CELL *)LOCAL_TrailTop) {
    MARK(ptr);
    mark_code(ptr, next PASS_REGS);
  }
}

/*
 * mark all heap objects accessible from the trail (which includes the active
 * general purpose registers)
 */

void Yap_mark_external_reference(CELL *ptr) {
  CACHE_REGS
  mark_external_reference(ptr PASS_REGS);
}

static void mark_regs(tr_fr_ptr old_TR USES_REGS) {
  tr_fr_ptr trail_ptr, tr = TR;

  /* first, whatever we dumped on the trail. Easier just to do
     the registers separately?  */
  for (trail_ptr = old_TR; trail_ptr < tr; trail_ptr++) {
   // printf("%p\n", trail_ptr);
    mark_external_reference(&TrailTerm(trail_ptr) PASS_REGS);
  }
}

/* mark all heap objects accessible from a chain of environments */

/* mark all heap objects accessible from a chain of environments */

/* mark all heap objects accessible from a chain of environments */

static inline void output_env_entry(CELL *gc_ENV, yamop *e_CP, UInt size) {
  return;
#if 0 && defined(ANALYST) || defined(DEBUG)
    PredEntry *pe = EnvPreg(e_CP);
    if (pe) {
      op_numbers op = Yap_op_from_opcode(e_CP->opc);
      fprintf(stderr, "ENV %p-%p^%lx(%ld) %s  ", gc_ENV, e_CP, gc_ENV[E_E],
              size - EnvSizeInCells, Yap_op_names[op]);
      Yap_DebugPlWriteln(Yap_PredicateToIndicator(pe));
    }
#endif  
}

static void
mark_env_cells(CELL *gc_ENV, UInt size, CELL *pvbmap)
{
  CELL_PTR saved_var;
  UInt bmap, bit = 1;
  if (size <= EnvSizeInCells || pvbmap == NULL) {
    return;
  }
  bmap = *pvbmap;
  for (saved_var = gc_ENV - (EnvSizeInCells + 1);
       saved_var >= gc_ENV - size; saved_var--) {
    // next bitmap
    if ((Int)bit < 0) {
      pvbmap++;
      bmap = *pvbmap;
    }
    /* we may have already been here */
    if ((bmap & bit) != 0) {
       if (!MARKED_PTR(saved_var)) { 
//      	printf("%c [%lx/%lx]  %lx->%lx\n",
//      	       //(MARKED_PTR(saved_var) ?'*':'f '),
//      	       bmap, bit, gc_ENV - saved_var,  *saved_var);

      mark_variable(saved_var PASS_REGS);
      }
    }
#ifdef INSTRUMENT_GC
    {
          Term ccur = *saved_var;
          if (IsVarTerm(ccur)) {
            int len = 1;
            CELL *mynext = GET_NEXT(ccur);
	    
            if (ONHEAP(mynext)) {
              env_vars++;
              while (ONHEAP(mynext) && IsVarTerm(*mynext)) {
                CELL *prox = GET_NEXT(*mynext);
                if (prox == mynext) {
                  chain[0]++;
                  break;
                }
                len++;
                mynext = prox;
              }
              if (len >= 15) {
                (chain[15])++;
              } else {
                (chain[len])++;
              }
            }
          }
	}
#endif
    bit <<= 1;
  }
}

static void mark_environments(CELL_PTR gc_ENV, size_t size,
                              CELL *pvbmap, yamop *e_CP USES_REGS) {
  while (true) {
    if (gc_ENV == LCL0 ||
	 gc_ENV == NULL ||
	 (CELL)gc_ENV == gc_ENV[E_E]
	 ) { /* no more environments */
      break;
    }
    //          fprintf(stderr, "ENV %p %ld\n", gc_ENV, size);
#ifdef DEBUG
    if (size < 0) {
      fprintf(stderr, "OOPS in GC: env size for %p is " UInt_FORMAT "\n",
              gc_ENV, (CELL)size);
    }
#endif
 output_env_entry( gc_ENV, e_CP,  size);
    /* for each saved variable */
    mark_env_cells( gc_ENV, size, pvbmap);
    /* have we met this environment before?? */
    /* we use the B field in the environment to tell whether we have
       been here before or not.

       We do it at the end because we don't want to lose any variables
       that would have been trimmed at the first environment visit.
    */
    if (MARKED_PTR(gc_ENV + E_CB))
      return;
    MARK(gc_ENV + E_CB);
    e_CP = (yamop *)gc_ENV[E_CP];
    if (e_CP == BORDERCODE) {
      if (gc_ENV == LCL0)
        e_CP = NULL;
      else
	e_CP = (yamop *)gc_ENV[-EnvSizeInCells - 1];
    }
    if (!e_CP || !gc_ENV[E_E])
      return;
    size = EnvSize(e_CP); /* size = EnvSize(CP) */
    if (size > EnvSizeInCells) {
      pvbmap = EnvBMap(e_CP);
    } else {
	pvbmap = NULL;
    }
    gc_ENV = (CELL_PTR)gc_ENV[E_E]; /* link to prev                                     * environment */
  }
}

/*
   Cleaning the trail should be quick and simple, right? Well, not
   really :-(. The problem is that the trail includes a dumping ground
   of the WAM registers and of extra choice-point fields, which need
   to be cleaned from somewhere.

   And cleaning the trail itself is not easy. The problem is that we
   may not have cleaned the trail after cuts. If we naively followed
   these pointers, we could have direct references to the global
   stack!  A solution is to verify whether we are poiting at a
   legitimate trail entry. Unfortunately this requires some extra work
   following choice-points.

*/

static void mark_trail(tr_fr_ptr trail_ptr, tr_fr_ptr trail_base, CELL *gc_H,
                       choiceptr gc_B USES_REGS) {
#ifdef EASY_SHUNTING
  tr_fr_ptr begsTR = NULL, endsTR = NULL;
  tr_fr_ptr OldsTR0 = LOCAL_sTR0;
#endif
#ifdef COROUTINING
  CELL *detatt = NULL;
#endif
  cont *old_cont_top0 = LOCAL_cont_top0;

  GC_NEW_MAHASH((gc_ma_hash_entry *)LOCAL_cont_top0 PASS_REGS);
  while (trail_base < trail_ptr) {
    register CELL trail_cell;

    trail_cell = TrailTerm(trail_base);
    if (IsVarTerm(trail_cell)) {
      CELL *hp = (CELL *)trail_cell;
      /* if a variable older than the current CP has not been marked yet,
         than its new binding is not accessible and we can reset it. Note
         we must use gc_H to avoid trouble with dangling variables
         in the heap */
      if ((hp < gc_H && hp >= H0) && !MARKED_PTR(hp)) {
        /* perform early reset */
        /* reset term to be a variable */
        RESET_VARIABLE(hp);
        LOCAL_discard_trail_entries++;
        RESET_VARIABLE(&TrailTerm(trail_base));
#ifdef FROZEN_STACKS
        RESET_VARIABLE(&TrailVal(trail_base));
#endif
      } else if (hp < (CELL *)LOCAL_GlobalBase || hp > (CELL *)LOCAL_TrailTop) {
        /*  pointers from the Heap back into the trail are process in mark_regs.
         */
        /* do nothing !!! */
      } else if ((hp < (CELL *)gc_B && hp >= gc_H) ||
                 hp > (CELL *)LOCAL_TrailBase) {
        /* clean the trail, avoid dangling pointers! */
        RESET_VARIABLE(&TrailTerm(trail_base));
#ifdef FROZEN_STACKS
        RESET_VARIABLE(&TrailVal(trail_base));
#endif
        LOCAL_discard_trail_entries++;
      } else {
        if (trail_cell == (CELL)trail_base)
          LOCAL_discard_trail_entries++;
        else {
          /* This is a bit of a mess: when I find an attributed variable that
             was bound nondeterministically, I know that after backtracking it
             will be back to be an unbound variable. The ideal solution would be
             to unbind all variables. The current solution is to
             remark it as an attributed variable */
          if (IN_BETWEEN(LOCAL_GlobalBase, hp, HR) && GlobalIsAttVar(hp) &&
              !UNMARKED_MARK(hp - 1, LOCAL_bp)) {
            // fprintf(stderr,"%p M\n", hp);
            LOCAL_total_marked++;
            PUSH_POINTER(hp - 1 PASS_REGS);
            if (hp - 1 < LOCAL_HGEN) {
              LOCAL_total_oldies++;
            } else {
              DEBUG_printf0("%p 1\n", hp - 1);
            }
            mark_variable(hp + 1 PASS_REGS);
            mark_variable(hp + 2 PASS_REGS);
          }
#ifdef FROZEN_STACKS
          mark_external_reference(&TrailVal(trail_base) PASS_REGS);
#endif
        }
#ifdef EASY_SHUNTING
        if (hp < gc_H && hp >= H0 && !MARKED_PTR(hp)) {
          tr_fr_ptr nsTR = (tr_fr_ptr)LOCAL_cont_top0;
          CELL *cptr = (CELL *)trail_cell;

          if ((ADDR)nsTR > LOCAL_TrailTop - 1024) {
            gc_growtrail(TRUE, begsTR, old_cont_top0 PASS_REGS);
          }
          TrailTerm(nsTR) = (CELL)NULL;
          TrailTerm(nsTR + 1) = *hp;
          TrailTerm(nsTR + 2) = trail_cell;
          if (begsTR == NULL)
            begsTR = nsTR;
          else
            TrailTerm(endsTR) = (CELL)nsTR;
          endsTR = nsTR;
          LOCAL_cont_top = (cont *)(nsTR + 3);
          LOCAL_sTR = (tr_fr_ptr)LOCAL_cont_top;
          LOCAL_gc_ma_h_top = (gc_ma_hash_entry *)(nsTR + 3);
          RESET_VARIABLE(cptr);
          MARK(cptr);
        }
#endif
      }
    } else if (IsPairTerm(trail_cell)) {
      /* cannot safely ignore this */
      CELL *cptr = RepPair(trail_cell);
      if (IN_BETWEEN(LOCAL_GlobalBase, cptr, HR)) {
        if (GlobalIsAttVar(cptr)) {
          TrailTerm(trail_base) = (CELL)cptr;
          mark_external_reference(&TrailTerm(trail_base) PASS_REGS);
          TrailTerm(trail_base) = trail_cell;
        } else {
          mark_external_reference(&TrailTerm(trail_base) PASS_REGS);
        }
      }
    }
#if MULTI_ASSIGNMENT_VARIABLES
    else {
      CELL *cptr = RepAppl(trail_cell);
      /* This is a bit complex. The idea is that we may have several
         trailings for the same mavar in the same trail segment. Essentially,
         the problem arises because of !. What we want is to ignore all but
         the last entry, or in this case, all but the first entry with the last
         value.

      */
      if (cptr < (CELL *)gc_B && cptr >= gc_H) {
        goto remove_trash_entry;
      } else if (cptr < (CELL *)gc_H && cptr >= H0 && IsAttVar(cptr)) {
        /* MABINDING that should be recovered */
        if (detatt && cptr < detatt) {
          goto remove_trash_entry;
        } else {
          /* This attributed variable is still in play */
          mark_variable(cptr PASS_REGS);
        }
      }
      if (!gc_lookup_ma_var(cptr, trail_base PASS_REGS)) {
        /* check whether this is the first time we see it*/
        Term t0 = TrailTerm(trail_base + 1);

        if (!IsAtomicTerm(t0)) {
          CELL *next = GET_NEXT(t0);
          /* check if we have a garbage entry, where we are setting a
             pointer to ourselves. */
          if (next < (CELL *)gc_B && next >= gc_H) {
            goto remove_trash_entry;
          }
        }
        if (HEAP_PTR(trail_cell)) {
          /* fool the gc into thinking this is a variable */
          TrailTerm(trail_base) = (CELL)cptr;
          mark_external_reference(&(TrailTerm(trail_base))PASS_REGS);
          /* reset the gc to believe the original tag */
          TrailTerm(trail_base) = AbsAppl((CELL *)TrailTerm(trail_base));
        }
#ifdef FROZEN_STACKS
        mark_external_reference(&(TrailVal(trail_base))PASS_REGS);
        trail_base++;
        if (HEAP_PTR(trail_cell)) {
          TrailTerm(trail_base) = (CELL)cptr;
          mark_external_reference(&(TrailTerm(trail_base))PASS_REGS);
          /* reset the gc to believe the original tag */
          TrailTerm(trail_base) = AbsAppl((CELL *)TrailTerm(trail_base));
        }
        /* don't need to mark the next TrailVal, this is done at the end
           of segment */
#else
        trail_base++;
        mark_external_reference(&(TrailTerm(trail_base))PASS_REGS);
        trail_base++;
        if (HEAP_PTR(trail_cell)) {
          /* fool the gc into thinking this is a variable */
          TrailTerm(trail_base) = (CELL)cptr;
          mark_external_reference(&(TrailTerm(trail_base))PASS_REGS);
          /* reset the gc to believe the original tag */
          TrailTerm(trail_base) = AbsAppl((CELL *)TrailTerm(trail_base));
        }
#endif /* TABLING */
      } else {
      remove_trash_entry:
        /* we can safely ignore this little monster */
#ifdef FROZEN_STACKS
        LOCAL_discard_trail_entries += 2;
        RESET_VARIABLE(&TrailTerm(trail_base));
        RESET_VARIABLE(&TrailVal(trail_base));
#else
        LOCAL_discard_trail_entries += 3;
        RESET_VARIABLE(&TrailTerm(trail_base));
        trail_base++;
        RESET_VARIABLE(&TrailTerm(trail_base));
#endif
        trail_base++;
        RESET_VARIABLE(&TrailTerm(trail_base));
#ifdef FROZEN_STACKS
        RESET_VARIABLE(&TrailVal(trail_base));
#endif
      }
    }
#endif
    trail_base++;
  }
#if TABLING
  /*
     Ugly, but needed: we're not really sure about what were the new
     values until the very end
  */
  {
    gc_ma_hash_entry *gl = LOCAL_gc_ma_h_list;
    while (gl) {
      mark_external_reference(&(TrailVal(gl->loc + 1)) PASS_REGS);
      gl = gl->more;
    }
  }
#endif /* TABLING */
#ifdef EASY_SHUNTING
  /* set back old variables */
  LOCAL_sTR = (tr_fr_ptr)old_cont_top0;
  while (begsTR != NULL) {
    tr_fr_ptr newsTR = (tr_fr_ptr)TrailTerm(begsTR);
    TrailTerm(LOCAL_sTR) = TrailTerm(begsTR + 1);
    TrailTerm(LOCAL_sTR + 1) = TrailTerm(begsTR + 2);
    begsTR = newsTR;
    LOCAL_sTR += 2;
  }
  LOCAL_sTR0 = OldsTR0;
#else
  LOCAL_cont_top0 = old_cont_top0;
#endif
  LOCAL_cont_top = LOCAL_cont_top0;
}

/*
 * mark all heap objects accessible from each choicepoint & its chain of
 * environments
 */

#ifdef TABLING
#define init_substitution_pointer(GCB, SUBS_PTR, DEP_FR)                       \
  if (DepFr_leader_cp(DEP_FR) == GCB) {                                        \
    /* GCB is a generator-consumer node */                                     \
    /* never here if batched scheduling */                                     \
    SUBS_PTR = (CELL *)(GEN_CP(GCB) + 1);                                      \
    SUBS_PTR += SgFr_arity(GEN_CP(GCB)->cp_sg_fr);                             \
  } else {                                                                     \
    SUBS_PTR = (CELL *)(CONS_CP(GCB) + 1);                                     \
  }
#endif /* TABLING */

#ifdef TABLING
static choiceptr youngest_cp(choiceptr gc_B, dep_fr_ptr *depfrp) {
  dep_fr_ptr depfr = *depfrp;
  choiceptr min = gc_B;

  if (!gc_B) {
    return gc_B;
  }
  if (depfr && min > DepFr_cons_cp(depfr)) {
    min = DepFr_cons_cp(depfr);
  }
  if (depfr && min == DepFr_cons_cp(depfr)) {
    *depfrp = DepFr_next(depfr);
  }
  return min;
}
#endif /* TABLING */

static void mark_choicepoints(register choiceptr gc_B, tr_fr_ptr saved_TR,
                              bool very_verbose USES_REGS) {
  OPCODE
  trust_lu = Yap_opcode(_trust_logical),
  count_trust_lu = Yap_opcode(_count_trust_logical),
  profiled_trust_lu = Yap_opcode(_profiled_trust_logical);

  yamop *lu_cl0 = NEXTOP(PredLogUpdClause0->CodeOfPred, Otapl),
        *lu_cl = NEXTOP(PredLogUpdClause->CodeOfPred, Otapl),
        *lu_cle = NEXTOP(PredLogUpdClauseErase->CodeOfPred, Otapl),
        *su_cl = NEXTOP(PredStaticClause->CodeOfPred, Otapl);
#ifdef TABLING
  dep_fr_ptr depfr = LOCAL_top_dep_fr;
  sg_fr_ptr aux_sg_fr = LOCAL_top_sg_fr;
#endif /* TABLING */

#ifdef TABLING
  gc_B = youngest_cp(gc_B, &depfr);
#endif /* TABLING */
  while (gc_B != NULL) {
    op_numbers opnum;
    register OPCODE op;
    yamop *rtp = gc_B->cp_ap;

    /* if (gc_B->cp_ap) */
    /*   fprintf(stderr,"B %p->%p %s\n", gc_B, gc_B->cp_b,
     * Yap_op_names[Yap_op_from_opcode(gc_B->cp_ap->opc)]) ; */
    /* else */
    /*   fprintf(stderr,"B %p->%p\n", gc_B, gc_B->cp_b); */
    mark_db_fixed((CELL *)rtp PASS_REGS);
#ifdef DETERMINISTIC_TABLING
    if (!IS_DET_GEN_CP(gc_B))
#endif /* DETERMINISTIC_TABLING */
      mark_db_fixed((CELL *)(gc_B->cp_cp)PASS_REGS);
#ifdef EASY_SHUNTING
    LOCAL_current_B = gc_B;
    LOCAL_prev_HB = HB;
#endif
    HB = gc_B->cp_h;
#ifdef INSTRUMENT_GC
    num_bs++;
#endif
#ifdef TABLING
    if (rtp == NULL) {
      if (aux_sg_fr && gc_B == SgFr_gen_cp(aux_sg_fr)) {
        /* found generator */
        opnum = _table_completion;
      } else {
        /* found sld node is done */
        opnum = _trust_fail;
      }
    } else {
#endif /* TABLING */
      op = rtp->opc;
      opnum = Yap_op_from_opcode(op);
      //      fprintf(stderr, "%s\n", Yap_op_names[opnum]);
#ifdef TABLING
    }
    // printf("MARK CP %p (%d)\n", gc_B, opnum);
    if (aux_sg_fr && gc_B == SgFr_gen_cp(aux_sg_fr)) {
      aux_sg_fr = SgFr_next(aux_sg_fr);
    }
#endif /* TABLING */
    if (very_verbose) {
      PredEntry *pe = Yap_PredForChoicePt(gc_B, NULL);
#if defined(ANALYST) || 0
      if (pe == NULL) {
        fprintf(stderr, "%%       marked  " UInt_FORMAT " (%s)\n",
                LOCAL_total_marked, Yap_op_names[opnum]);
      } else if (pe->ArityOfPE) {
        fprintf(stderr,
                "%%       %s/" UInt_FORMAT " marked  " UInt_FORMAT " (%s)\n",
                RepAtom(NameOfFunctor(pe->FunctorOfPred))->StrOfAE,
                pe->ArityOfPE, LOCAL_total_marked, Yap_op_names[opnum]);
      } else {
        fprintf(stderr, "%%       %s marked  " UInt_FORMAT " (%s)\n",
                RepAtom((Atom)(pe->FunctorOfPred))->StrOfAE, LOCAL_total_marked,
                Yap_op_names[opnum]);
      }
#else
      if (pe == NULL) {
        fprintf(stderr, "%%       marked " Int_FORMAT " (%u)\n",
                LOCAL_total_marked, (unsigned int)opnum);
      } else if (pe->ArityOfPE) {
        fprintf(stderr, "%%       %s/%lu marked " Int_FORMAT " (%u)\n",
                RepAtom(NameOfFunctor(pe->FunctorOfPred))->StrOfAE,
                (unsigned long int)pe->ArityOfPE, LOCAL_total_marked,
                (unsigned int)opnum);
      } else {
        fprintf(stderr, "%%       %s marked " Int_FORMAT " (%u)\n",
                RepAtom((Atom)(pe->FunctorOfPred))->StrOfAE, LOCAL_total_marked,
                (unsigned int)opnum);
      }
#endif
    }
    {
      /* find out how many cells are still alive in the trail */
      mark_trail(saved_TR, gc_B->cp_tr, gc_B->cp_h, gc_B PASS_REGS);
      saved_TR = gc_B->cp_tr;
    }
    if (opnum == _or_else || opnum == _or_last || opnum==_either) {
      /* ; choice point */
      //CELL *env = gc_B->cp_env;
      yamop *e_cp = NEXTOP(gc_B->cp_ap,Osblp);
      mark_environments((CELL_PTR)(gc_B->cp_a1),
                       EnvSize(e_cp),
			EnvBMap(e_cp),
			e_cp PASS_REGS);
    } else {
      /* choicepoint with arguments */
      register CELL_PTR saved_reg;
      OPREG nargs;

      // printf("gc_B=%p %ld\n", gc_B, opnum);
      if (opnum == _Nstop && gc_B->cp_env) {
        mark_environments((CELL_PTR)gc_B->cp_env, EnvSizeInCells,
                          NULL, gc_B->cp_cp PASS_REGS);
      } else if (opnum != _trust_fail) {
        Int mark = TRUE;
#ifdef DETERMINISTIC_TABLING
        mark &= !IS_DET_GEN_CP(gc_B);
#endif /* DETERMINISTIC_TABLING */
        if (mark)
          mark_environments((CELL_PTR)gc_B->cp_env,
                            EnvSize((yamop *)(gc_B->cp_cp)),
                            EnvBMap((yamop *)(gc_B->cp_cp)),
			    gc_B->cp_cp PASS_REGS);
      }
      /* extended choice point */
    restart_cp:
      switch (opnum) {
      case _Nstop:
        if (gc_B->cp_env == LCL0 || gc_B->cp_env == NULL) {
          return;
        } else {
          // This must be a border choicepoint, just move up
          gc_B = (choiceptr)(gc_B->cp_env[E_B]);
          continue;
        }
      case _retry_c:
      case _retry_userc:
        if (gc_B->cp_ap == RETRY_C_RECORDED_K_CODE ||
            gc_B->cp_ap == RETRY_C_RECORDEDP_CODE) {
          /* we have a reference from the choice-point stack to a term */
          choiceptr old_b = B;
          DBRef ref;
          B = gc_B;
          ref = (DBRef)EXTRA_CBACK_ARG(3, 1);
          if (IsVarTerm((CELL)ref)) {
            mark_ref_in_use(ref PASS_REGS);
          } else {
            if (ONCODE((CELL)ref)) {
              mark_db_fixed(RepAppl((CELL)ref) PASS_REGS);
            }
          }
          B = old_b;
        }
        nargs = rtp->y_u.OtapFs.s + rtp->y_u.OtapFs.extra;
        break;
      case _jump:
        rtp = rtp->y_u.l.l;
        op = rtp->opc;
        opnum = Yap_op_from_opcode(op);
        goto restart_cp;
      case _retry_profiled:
      case _count_retry:
        rtp = NEXTOP(rtp, l);
        op = rtp->opc;
        opnum = Yap_op_from_opcode(op);
        goto restart_cp;
      case _trust_fail:
        nargs = 0;
        break;
#ifdef TABLING
      case _table_load_answer: {
        CELL *vars_ptr, vars;
        vars_ptr = (CELL *)(LOAD_CP(gc_B) + 1);
        vars = *vars_ptr++;
        while (vars--) {
          mark_external_reference(vars_ptr PASS_REGS);
          vars_ptr++;
        }
      }
        nargs = 0;
        break;
      case _table_try_answer:
      case _table_retry_me:
      case _table_trust_me:
      case _table_retry:
      case _table_trust: {
        CELL *vars_ptr, vars;
        vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
        nargs = rtp->y_u.Otapl.s;
        while (nargs--) {
          mark_external_reference(vars_ptr PASS_REGS);
          vars_ptr++;
        }
        vars = *vars_ptr++;
        while (vars--) {
          mark_external_reference(vars_ptr PASS_REGS);
          vars_ptr++;
        }
      }
        nargs = 0;
        break;
      case _table_completion:
#ifdef THREADS_CONSUMER_SHARING
      case _table_answer_resolution_completion:
#endif /* THREADS_CONSUMER_SHARING */
      {
        CELL *vars_ptr, vars;
#ifdef DETERMINISTIC_TABLING
        if (IS_DET_GEN_CP(gc_B))
          vars_ptr = (CELL *)(DET_GEN_CP(gc_B) + 1);
        else
#endif /* DETERMINISTIC_TABLING */
        {
          vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
          nargs = SgFr_arity(GEN_CP(gc_B)->cp_sg_fr);
          while (nargs--) {
            mark_external_reference(vars_ptr PASS_REGS);
            vars_ptr++;
          }
        }
        vars = *vars_ptr++;
        while (vars--) {
          mark_external_reference(vars_ptr PASS_REGS);
          vars_ptr++;
        }
      }
        nargs = 0;
        break;
      case _table_answer_resolution: {
        CELL *vars_ptr, vars;
        dep_fr_ptr dep_fr = CONS_CP(gc_B)->cp_dep_fr;
        ans_node_ptr ans_node = DepFr_last_answer(dep_fr);
        if (TRUE || TrNode_child(ans_node)) {
          /* unconsumed answers */
#ifdef MODE_DIRECTED_TABLING
          if (TrNode_child(ans_node) &&
              IS_ANSWER_INVALID_NODE(TrNode_child(ans_node))) {
            ans_node_ptr old_ans_node;
            old_ans_node = ans_node;
            ans_node = TrNode_child(ans_node);
            do {
              ans_node = TrNode_child(ans_node);
            } while (IS_ANSWER_INVALID_NODE(ans_node));
            TrNode_child(old_ans_node) = ans_node;
          } else
#endif /* MODE_DIRECTED_TABLING */
            ans_node = TrNode_child(ans_node);
          if (gc_B == DepFr_leader_cp(dep_fr)) {
            /*  gc_B is a generator-consumer node  */
            /* never here if batched scheduling */
            TABLING_ERROR_CHECKING(generator_consumer, IS_BATCHED_GEN_CP(gc_B));
            vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
            vars_ptr += SgFr_arity(GEN_CP(gc_B)->cp_sg_fr);
          } else {
            vars_ptr = (CELL *)(CONS_CP(gc_B) + 1);
          }

          vars = *vars_ptr++;
          while (vars--) {
            mark_external_reference(vars_ptr PASS_REGS);
            vars_ptr++;
          }
        }
      }
        nargs = 0;
        break;
      case _trie_trust_var:
      case _trie_retry_var:
      case _trie_trust_var_in_pair:
      case _trie_retry_var_in_pair:
      case _trie_trust_val:
      case _trie_retry_val:
      case _trie_trust_val_in_pair:
      case _trie_retry_val_in_pair:
      case _trie_trust_atom:
      case _trie_retry_atom:
      case _trie_trust_atom_in_pair:
      case _trie_retry_atom_in_pair:
      case _trie_trust_null:
      case _trie_retry_null:
      case _trie_trust_null_in_pair:
      case _trie_retry_null_in_pair:
      case _trie_trust_pair:
      case _trie_retry_pair:
      case _trie_trust_appl:
      case _trie_retry_appl:
      case _trie_trust_appl_in_pair:
      case _trie_retry_appl_in_pair:
      case _trie_trust_extension:
      case _trie_retry_extension:
      case _trie_trust_double:
      case _trie_retry_double:
      case _trie_trust_longint:
      case _trie_retry_longint:
      case _trie_trust_gterm:
      case _trie_retry_gterm: {
        CELL *vars_ptr;
        int heap_arity, vars_arity, subs_arity;
        vars_ptr = (CELL *)(gc_B + 1);
        heap_arity = vars_ptr[0];
        vars_arity = vars_ptr[1 + heap_arity];
        subs_arity = vars_ptr[2 + heap_arity + vars_arity];
        vars_ptr += 2 + heap_arity + subs_arity + vars_arity;
        if (subs_arity) {
          while (subs_arity--) {
            mark_external_reference(vars_ptr PASS_REGS);
            vars_ptr--;
          }
        }
        vars_ptr--; /* skip subs_arity entry */
        if (vars_arity) {
          while (vars_arity--) {
            mark_external_reference(vars_ptr PASS_REGS);
            vars_ptr--;
          }
        }
        vars_ptr--; /* skip vars_arity entry */
        if (heap_arity) {
          while (heap_arity--) {
            if (*vars_ptr == 0) /* double/longint extension mark */
              break;
            mark_external_reference(vars_ptr PASS_REGS);
            vars_ptr--;
          }
        }
      }
        nargs = 0;
        break;
#endif /* TABLING */
      case _profiled_retry_and_mark:
      case _count_retry_and_mark:
      case _retry_and_mark:
        mark_ref_in_use((DBRef)ClauseCodeToDynamicClause(gc_B->cp_ap)
                            PASS_REGS);
      case _retry2:
        nargs = 2;
        break;
      case _retry3:
        nargs = 3;
        break;
      case _retry4:
        nargs = 4;
        break;
      case _try_logical:
      case _retry_logical: {
        /* find out who owns this sequence of try-retry-trust */
        /* I don't like this code, it's a bad idea to do a linear scan,
           on the other hand it's the only way we can be sure we can reclaim
           space
        */
        yamop *end = rtp->y_u.OtaLl.n;
        while (end->opc != trust_lu && end->opc != count_trust_lu &&
               end->opc != profiled_trust_lu)
          end = end->y_u.OtaLl.n;
        mark_ref_in_use((DBRef)end->y_u.OtILl.block PASS_REGS);
      }
        /* mark timestamp */
        nargs = rtp->y_u.OtaLl.s + 1;
        break;
      case _count_retry_logical: {
        /* find out who owns this sequence of try-retry-trust */
        /* I don't like this code, it's a bad idea to do a linear scan,
           on the other hand it's the only way we can be sure we can reclaim
           space
        */
        yamop *end = rtp->y_u.OtaLl.n;
        while (Yap_op_from_opcode(end->opc) != _count_trust_logical)
          end = end->y_u.OtaLl.n;
        mark_ref_in_use((DBRef)end->y_u.OtILl.block PASS_REGS);
      }
        /* mark timestamp */
        nargs = rtp->y_u.OtaLl.s + 1;
        break;
      case _profiled_retry_logical: {
        /* find out who owns this sequence of try-retry-trust */
        /* I don't like this code, it's a bad idea to do a linear scan,
           on the other hand it's the only way we can be sure we can reclaim
           space
        */
        yamop *end = rtp->y_u.OtaLl.n;
        while (Yap_op_from_opcode(end->opc) != _profiled_trust_logical)
          end = end->y_u.OtaLl.n;
        mark_ref_in_use((DBRef)end->y_u.OtILl.block PASS_REGS);
      }
        /* mark timestamp */
        nargs = rtp->y_u.OtaLl.s + 1;
        break;
      case _trust_logical:
      case _count_trust_logical:
      case _profiled_trust_logical:
        /* mark timestamp */
        mark_ref_in_use((DBRef)rtp->y_u.OtILl.block PASS_REGS);
        nargs = rtp->y_u.OtILl.d->ClPred->ArityOfPE + 1;
        break;
      case _retry_exo:
      case _retry_exo_udi:
      case _retry_all_exo:
        nargs = rtp->y_u.lp.p->ArityOfPE;
        break;
      case _retry_udi:
        nargs = rtp->y_u.p.p->ArityOfPE;
        break;
#ifdef DEBUG
      case _retry_me:
      case _trust_me:
      case _profiled_retry_me:
      case _profiled_trust_me:
      case _count_retry_me:
      case _count_trust_me:
      case _retry:
      case _trust:
        if (IN_BETWEEN(H0, (CELL *)(gc_B->cp_ap), HR)) {
          fprintf(stderr, "OOPS in GC: gc not supported in this case!!!\n");
          exit(1);
        }
        nargs = rtp->y_u.Otapl.s;
        break;
      default:
        fprintf(stderr, "OOPS in GC: Unexpected opcode: %d\n", opnum);
        nargs = 0;
#else
      default:
        nargs = rtp->y_u.Otapl.s;
#endif
      }

      if (gc_B->cp_ap == lu_cl0 || gc_B->cp_ap == lu_cl ||
          gc_B->cp_ap == lu_cle || gc_B->cp_ap == su_cl) {
        yamop *pt = (yamop *)IntegerOfTerm(gc_B->cp_args[1]);
        if (gc_B->cp_ap == su_cl) {
          mark_db_fixed((CELL *)pt PASS_REGS);
        } else {
          while (pt->opc != trust_lu && pt->opc != count_trust_lu &&
                 pt->opc != profiled_trust_lu)
            pt = pt->y_u.OtaLl.n;
          mark_ref_in_use((DBRef)pt->y_u.OtILl.block PASS_REGS);
        }
      }
      /* for each saved register */
      for (saved_reg = &gc_B->cp_a1;
           /* assumes we can count registers in CP this
              way */
           saved_reg < &gc_B->cp_a1 + nargs; saved_reg++) {
        mark_external_reference(saved_reg PASS_REGS);
      }
    }
#if TABLING
    gc_B = youngest_cp(gc_B->cp_b, &depfr);
#else
    gc_B = gc_B->cp_b;
#endif /* TABLING */
  }
}

/*
 * insert a cell which points to a heap object into relocation chain of that
 * object
 */

static inline void into_relocation_chain(CELL_PTR current,
                                         CELL_PTR next USES_REGS) {
  CELL current_tag;

  current_tag = TAG(*current);
  if (RMARKED(next))
    RMARK(current);
  else {
    UNRMARK(current);
    RMARK(next);
  }
  *current = *next;
  *next = (CELL)current | current_tag;
}

static void CleanDeadClauses(USES_REGS1) {
  {
    StaticClause **cptr;
    StaticClause *cl;

    cptr = &(DeadStaticClauses);
    cl = DeadStaticClauses;
    while (cl) {
      if (!ref_in_use((DBRef)cl PASS_REGS)) {
        char *ocl = (char *)cl;
        Yap_ClauseSpace -= cl->ClSize;
        cl = cl->ClNext;
        *cptr = cl;
        Yap_FreeCodeSpace(ocl);
      } else {
        cptr = &(cl->ClNext);
        cl = cl->ClNext;
      }
    }
  }
  {
    StaticIndex **cptr;
    StaticIndex *cl;

    cptr = &(DeadStaticIndices);
    cl = DeadStaticIndices;
    while (cl) {
      if (!ref_in_use((DBRef)cl PASS_REGS)) {
        char *ocl = (char *)cl;
        if (cl->ClFlags & SwitchTableMask)
          Yap_IndexSpace_SW -= cl->ClSize;
        else
          Yap_IndexSpace_Tree -= cl->ClSize;
        cl = cl->SiblingIndex;
        *cptr = cl;
        Yap_FreeCodeSpace(ocl);
      } else {
        cptr = &(cl->SiblingIndex);
        cl = cl->SiblingIndex;
      }
    }
  }
  {
    MegaClause **cptr;
    MegaClause *cl;

    cptr = &(DeadMegaClauses);
    cl = DeadMegaClauses;
    while (cl) {
      if (!ref_in_use((DBRef)cl PASS_REGS)) {
        char *ocl = (char *)cl;
        Yap_ClauseSpace -= cl->ClSize;
        cl = cl->ClNext;
        *cptr = cl;
        Yap_FreeCodeSpace(ocl);
      } else {
        cptr = &(cl->ClNext);
        cl = cl->ClNext;
      }
    }
  }
}

/* insert trail cells which point to heap objects into relocation chains */

static void sweep_trail(choiceptr gc_B, tr_fr_ptr old_TR USES_REGS) {
  tr_fr_ptr trail_ptr, dest;
  Int OldHeapUsed = HeapUsed;
#ifdef DEBUG
  Int hp_entrs = 0, hp_erased = 0, hp_not_in_use = 0, hp_in_use_erased = 0,
      code_entries = 0;
#endif
  CELL *ptr = LOCAL_extra_gc_cells;

  while (ptr > LOCAL_extra_gc_cells_base) {
    Int k = ptr[-1], i;
    ptr = ptr - 1;

    for (i = 0; i < k; i++) {
      ptr--;
      if (IN_BETWEEN(LOCAL_GlobalBase, ptr[0], LOCAL_TrailTop) &&
          MARKED_PTR(ptr)) {
        UNMARK(ptr);
        if (HEAP_PTR(ptr[0])) {
          into_relocation_chain(ptr, GET_NEXT(ptr[0]) PASS_REGS);
        }
      }
    }
  }

#ifndef FROZEN_STACKS
  {
    choiceptr current = gc_B;
    choiceptr next = gc_B->cp_b;
    tr_fr_ptr source, dest;

    /* invert cp ptrs */
    current->cp_b = NULL;
    while (next) {
      choiceptr n = next;
      next = n->cp_b;
      n->cp_b = current;
      current = n;
    }

    next = current;
    current = NULL;
    /* next, clean trail */
    source = dest = (tr_fr_ptr)LOCAL_TrailBase;
    while (source < old_TR) {
      CELL trail_cell;

      while (next && source == next->cp_tr) {
        choiceptr b = next;
        b->cp_tr = dest;
        next = b->cp_b;
        b->cp_b = current;
        current = b;
      }
      trail_cell = TrailTerm(source);
      if (trail_cell != (CELL)source) {
        dest++;
      }
      source++;
    }
    while (next) {
      choiceptr b = next;
      b->cp_tr = dest;
      next = b->cp_b;
      b->cp_b = current;
      current = b;
    }
  }
#endif /* FROZEN_STACKS */

  /* first, whatever we dumped on the trail. Easier just to do
     the registers separately?  */
  for (trail_ptr = old_TR; trail_ptr < TR; trail_ptr++) {
    if (IN_BETWEEN(LOCAL_GlobalBase, TrailTerm(trail_ptr), LOCAL_TrailTop) &&
        MARKED_PTR(&TrailTerm(trail_ptr))) {
      UNMARK(&TrailTerm(trail_ptr));
      if (HEAP_PTR(TrailTerm(trail_ptr))) {
        into_relocation_chain(&TrailTerm(trail_ptr),
                              GET_NEXT(TrailTerm(trail_ptr)) PASS_REGS);
      }
    }
  }

  /* next, follows the real trail entries */
  trail_ptr = (tr_fr_ptr)LOCAL_TrailBase;
  dest = trail_ptr;
  while (trail_ptr < old_TR) {
    register CELL trail_cell;

    trail_cell = TrailTerm(trail_ptr);

#ifndef FROZEN_STACKS
    /* recover a trail cell */
    if (trail_cell == (CELL)trail_ptr) {
      TrailTerm(dest) = trail_cell;
      trail_ptr++;
      /* just skip cell */
    } else
#endif
    {
      TrailTerm(dest) = trail_cell;
      if (IsVarTerm(trail_cell)) {
        /* we need to check whether this is a honest to god trail entry */
        /* make sure it is a heap cell before we test whether it has been marked
         */
        if ((CELL *)trail_cell < HR && (CELL *)trail_cell >= H0 &&
            MARKED_PTR((CELL *)trail_cell)) {
          if (HEAP_PTR(trail_cell)) {
            into_relocation_chain(&TrailTerm(dest),
                                  GET_NEXT(trail_cell) PASS_REGS);
          }
        }
#ifdef FROZEN_STACKS
        /* it is complex to recover cells with frozen segments */
        TrailVal(dest) = TrailVal(trail_ptr);
        if (MARKED_PTR(&TrailVal(dest))) {
          if (HEAP_PTR(TrailVal(dest))) {
            into_relocation_chain(&TrailVal(dest),
                                  GET_NEXT(TrailVal(dest)) PASS_REGS);
          }
        }
#endif
      } else if (IsPairTerm(trail_cell)) {
        CELL *pt0 = RepPair(trail_cell);
        CELL flags;

        if (IN_BETWEEN(LOCAL_GlobalBase, pt0, HR)) {
          if (GlobalIsAttVar(pt0)) {
            TrailTerm(dest) = trail_cell;
            /* be careful with partial gc */
            if (HEAP_PTR(TrailTerm(dest))) {
              into_relocation_chain(&TrailTerm(dest),
                                    GET_NEXT(trail_cell) PASS_REGS);
            }
          } else {
            TrailTerm(dest) = trail_cell;
            /* be careful with partial gc */
            if (HEAP_PTR(TrailTerm(dest))) {
              into_relocation_chain(&TrailTerm(dest),
                                    GET_NEXT(trail_cell) PASS_REGS);
            }
          }
          dest++;
          trail_ptr++;
          continue;
        }
#ifdef FROZEN_STACKS /* TRAIL */
        /* process all segments */
        if (
#ifdef YAPOR_SBA
            (ADDR)pt0 >= LOCAL_GlobalBase
#else
            (ADDR)pt0 >= LOCAL_TrailBase
#endif
        ) {
          trail_ptr++;
          dest++;
          continue;
        }
#endif /* FROZEN_STACKS */
        flags = *pt0;
#ifdef DEBUG
        hp_entrs++;
        if (!ref_in_use((DBRef)pt0 PASS_REGS)) {
          hp_not_in_use++;
          if (!FlagOn(DBClMask, flags)) {
            code_entries++;
          }
          if (FlagOn(ErasedMask, flags)) {
            hp_erased++;
          }
        } else {
          if (FlagOn(ErasedMask, flags)) {
            hp_in_use_erased++;
          }
        }
#endif
        if (!ref_in_use((DBRef)pt0 PASS_REGS)) {
          if (FlagOn(DBClMask, flags)) {
            DBRef dbr = (DBRef)((CELL)pt0 - (CELL) & (((DBRef)NIL)->Flags));
            dbr->Flags &= ~InUseMask;
            DEC_DBREF_COUNT(dbr);
            if (dbr->Flags & ErasedMask) {
              Yap_ErDBE(dbr);
            }
          } else {
            if (flags & LogUpdMask) {
              if (flags & IndexMask) {
                LogUpdIndex *indx = ClauseFlagsToLogUpdIndex(pt0);
                int erase;
#if defined(YAPOR) || defined(THREADS)
                /*
                  gc may be called when executing a dynamic goal,
                  check PP to avoid deadlock
                */
                PredEntry *ap = indx->ClPred;
                if (ap != PP)
                  PELOCK(85, ap);
#endif
                DEC_CLREF_COUNT(indx);
                indx->ClFlags &= ~InUseMask;
                erase = (indx->ClFlags & ErasedMask && !indx->ClRefCount);
                if (erase) {
                  /* at this point,
                     no one is accessing the clause */
                  Yap_ErLogUpdIndex(indx);
                }
#if defined(YAPOR) || defined(THREADS)
                if (ap != PP)
                  UNLOCK(ap->PELock);
#endif
              } else {
                LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt0);
#if defined(YAPOR) || defined(THREADS)
                PredEntry *ap = cl->ClPred;
#endif
                int erase;

#if defined(YAPOR) || defined(THREADS)
                if (ap != PP)
                  PELOCK(86, ap);
#endif
                DEC_CLREF_COUNT(cl);
                cl->ClFlags &= ~InUseMask;
                erase = ((cl->ClFlags & ErasedMask) && !cl->ClRefCount);
                if (erase) {
                  /* at this point,
                     no one is accessing the clause */
                  Yap_ErLogUpdCl(cl);
                }
#if defined(YAPOR) || defined(THREADS)
                if (ap != PP)
                  UNLOCK(ap->PELock);
#endif
              }
            } else {
              DynamicClause *cl = ClauseFlagsToDynamicClause(pt0);
              int erase;
              DEC_CLREF_COUNT(cl);
              cl->ClFlags &= ~InUseMask;
              erase = (cl->ClFlags & ErasedMask)
#if defined(YAPOR) || defined(THREADS)
                      && (cl->ClRefCount == 0)
#endif
                  ;
              if (erase) {
                /* at this point,
                   no one is accessing the clause */
                Yap_ErCl(cl);
              }
            }
          }
          RESET_VARIABLE(&TrailTerm(dest));
#ifdef FROZEN_STACKS
          RESET_VARIABLE(&TrailVal(dest));
#endif
          LOCAL_discard_trail_entries++;
        }
#if MULTI_ASSIGNMENT_VARIABLES
      } else {
#ifdef FROZEN_STACKS
        CELL trail_cell = TrailTerm(trail_ptr + 1);
        CELL old = TrailVal(trail_ptr);
        CELL old1 = TrailVal(trail_ptr + 1);
        Int marked_ptr = MARKED_PTR(&TrailTerm(trail_ptr + 1));
        Int marked_val_old = MARKED_PTR(&TrailVal(trail_ptr));
        Int marked_val_ptr = MARKED_PTR(&TrailVal(trail_ptr + 1));

        TrailTerm(dest + 1) = TrailTerm(dest) = trail_cell;
        TrailVal(dest) = old;
        TrailVal(dest + 1) = old1;
        if (marked_ptr) {
          UNMARK(&TrailTerm(dest));
          UNMARK(&TrailTerm(dest + 1));
          if (HEAP_PTR(trail_cell)) {
            into_relocation_chain(&TrailTerm(dest),
                                  GET_NEXT(trail_cell) PASS_REGS);
            into_relocation_chain(&TrailTerm(dest + 1),
                                  GET_NEXT(trail_cell) PASS_REGS);
          }
        }
        if (marked_val_old) {
          UNMARK(&TrailVal(dest));
          if (HEAP_PTR(old)) {
            into_relocation_chain(&TrailVal(dest), GET_NEXT(old) PASS_REGS);
          }
        }
        if (marked_val_ptr) {
          UNMARK(&TrailVal(dest + 1));
          if (HEAP_PTR(old1)) {
            into_relocation_chain(&TrailVal(dest + 1),
                                  GET_NEXT(old1) PASS_REGS);
          }
        }
        trail_ptr++;
        dest++;
#else
        CELL trail_cell = TrailTerm(trail_ptr + 2);
        CELL old = TrailTerm(trail_ptr + 1);
        Int marked_ptr = MARKED_PTR(&TrailTerm(trail_ptr + 2));
        Int marked_old = MARKED_PTR(&TrailTerm(trail_ptr + 1));
        CELL *ptr;
        /* be sure we don't overwrite before we read */

        if (marked_ptr)
          ptr = RepAppl(UNMARK_CELL(trail_cell));
        else
          ptr = RepAppl(trail_cell);

        TrailTerm(dest + 1) = old;
        if (marked_old) {
          UNMARK(&TrailTerm(dest + 1));
          if (HEAP_PTR(old)) {
            into_relocation_chain(&TrailTerm(dest + 1),
                                  GET_NEXT(old) PASS_REGS);
          }
        }
        TrailTerm(dest + 2) = TrailTerm(dest) = trail_cell;
        if (marked_ptr) {
          UNMARK(&TrailTerm(dest));
          UNMARK(&TrailTerm(dest + 2));
          if (HEAP_PTR(trail_cell)) {
            into_relocation_chain(&TrailTerm(dest),
                                  GET_NEXT(trail_cell) PASS_REGS);
            into_relocation_chain(&TrailTerm(dest + 2),
                                  GET_NEXT(traivl_cell) PASS_REGS);
          }
        }
        trail_ptr += 2;
        dest += 2;
#endif
#endif
      }
      trail_ptr++;
      dest++;
    }
  }
  LOCAL_new_TR = dest;
  if (is_gc_verbose()) {
    if (old_TR != (tr_fr_ptr)LOCAL_TrailBase)
      fprintf(stderr, "%%       Trail: discarded %d (%ld%%) cells out of %ld\n",
              LOCAL_discard_trail_entries,
              (unsigned long int)(LOCAL_discard_trail_entries * 100 /
                                  (old_TR - (tr_fr_ptr)LOCAL_TrailBase)),
              (unsigned long int)(old_TR - (tr_fr_ptr)LOCAL_TrailBase));
#ifdef DEBUG
    if (hp_entrs > 0)
      fprintf(stderr,
              "%%       Trail: unmarked %ld dbentries (%ld%%) out of %ld\n",
              (long int)hp_not_in_use,
              (long int)(hp_not_in_use * 100 / hp_entrs), (long int)hp_entrs);
    if (hp_in_use_erased > 0 && hp_erased > 0)
      fprintf(stderr,
              "%%       Trail: deleted %ld dbentries (%ld%%) out of %ld\n",
              (long int)hp_erased,
              (long int)(hp_erased * 100 / (hp_erased + hp_in_use_erased)),
              (long int)(hp_erased + hp_in_use_erased));
#endif
    if (OldHeapUsed) {
      fprintf(
          stderr, "%%       Heap: recovered %ld bytes (%ld%%) out of %ld\n",
          (unsigned long int)(OldHeapUsed - HeapUsed),
          (unsigned long int)((OldHeapUsed - HeapUsed) / (OldHeapUsed / 100)),
          (unsigned long int)OldHeapUsed);
    }
  }
  CleanDeadClauses(PASS_REGS1);
}

/*
 * insert cells of a chain of environments which point to heap objects into
 * relocation chains
 */

static void
sweep_env_cells(CELL *gc_ENV, UInt size, CELL *pvbmap)
{
  CELL_PTR saved_var;
  UInt bmap, bit = 1;
  if (size <= EnvSizeInCells || pvbmap == NULL) {
    return;
  }
  bmap = *pvbmap;
      for (saved_var = gc_ENV - (EnvSizeInCells + 1);
	 saved_var >= gc_ENV - size; saved_var--) {
      // next bitmap
      if ((Int)bit < 0) {
	pvbmap++;
	bmap = *pvbmap;
      }
      /* we may have already been here */
      if ((bmap & bit) != 0) {
//	  printf("%c [%lx/%lx]  %lx ->%lx\n",
//		 (MARKED_PTR(saved_var) ?'*':' '),
//		 bmap, bit, gc_ENV - saved_var,  *saved_var);
	if ( MARKED_PTR(saved_var)) {
	  UNMARK(saved_var);
          CELL cp_cell = *saved_var;
	  if (HEAP_PTR(cp_cell)) {
	    into_relocation_chain(saved_var, GET_NEXT(cp_cell) PASS_REGS);
	  }
	}
      }
      bit <<= 1;
      }
}

static void sweep_environments(CELL_PTR gc_ENV, size_t size,
                               CELL *pvbmap, yamop *e_CP USES_REGS) {
 while (true) {
    if (gc_ENV == LCL0 ||
	 gc_ENV == NULL ||
	 (CELL)gc_ENV == gc_ENV[E_E]
	 ) { /* no more environments */
      break;
    }
    //          fprintf(stderr, "ENV %p %ld\n", gc_ENV, size);
#ifdef DEBUG
    if (size < 0) {
      fprintf(stderr, "OOPS in GC: env size for %p is " UInt_FORMAT "\n",
              gc_ENV, (CELL)size);
    }
#endif
    output_env_entry( gc_ENV,  e_CP,  size);
    /* for each saved variable */
    sweep_env_cells( gc_ENV, size, pvbmap);
    /* have we met this environment before?? */
    /* we use the B field in the environment to tell whether we have
       been here before or not.

       We do it at the end because we don't want to lose any variables
       that would have been trimmed at the first environment visit.
    */
    if (!MARKED_PTR(gc_ENV + E_CB))
      return;
    UNMARK(gc_ENV + E_CB);
    e_CP = (yamop *)gc_ENV[E_CP];
    if (e_CP == BORDERCODE) {
      if (gc_ENV == LCL0)
        e_CP = NULL;
      else
	e_CP = (yamop *)gc_ENV[-EnvSizeInCells - 1];
    }
    if (!e_CP || !gc_ENV[E_E])
      return;
    size = EnvSize(e_CP); /* size = EnvSize(CP) */
    if (size > EnvSizeInCells) {
      pvbmap = EnvBMap(e_CP);
    } else {
	pvbmap = NULL;
    }
    gc_ENV = (CELL_PTR)gc_ENV[E_E]; /* link to prev                                     * environment */
  }
 }
  


static void sweep_b(choiceptr gc_B, UInt arity USES_REGS) {
  register CELL_PTR saved_reg;

  sweep_environments(gc_B->cp_env, EnvSize((yamop *)(gc_B->cp_cp)),
                     EnvBMap((yamop *)(gc_B->cp_cp)),
		     gc_B->cp_cp PASS_REGS);

  /* for each saved register */
  for (saved_reg = &gc_B->cp_a1; saved_reg < &gc_B->cp_a1 + arity;
       saved_reg++) {
    CELL cp_cell = *saved_reg;
    if (MARKED_PTR(saved_reg)) {
      UNMARK(saved_reg);
      if (HEAP_PTR(cp_cell)) {
        into_relocation_chain(saved_reg, GET_NEXT(cp_cell) PASS_REGS);
      }
    }
  }
}

/*
 * insert cells of each choicepoint & its chain of environments which point
 * to heap objects into relocation chains
 */
static void sweep_choicepoints(choiceptr gc_B USES_REGS) {
#ifdef TABLING
  dep_fr_ptr depfr = LOCAL_top_dep_fr;
  sg_fr_ptr aux_sg_fr = LOCAL_top_sg_fr;
#endif /* TABLING */

#ifdef TABLING
  gc_B = youngest_cp(gc_B, &depfr);
#endif /* TABLING */
  while (gc_B != NULL) {
    yamop *rtp = gc_B->cp_ap;
    register OPCODE op;
    op_numbers opnum;

#ifdef TABLING
    if (rtp == NULL) {
      if (aux_sg_fr && gc_B == SgFr_gen_cp(aux_sg_fr)) {
        /* found generator */
        opnum = _table_completion;
      } else {
        /* found sld node is done */
        opnum = _trust_fail;
      }
    } else {
#endif /* TABLING */
      op = rtp->opc;
      opnum = Yap_op_from_opcode(op);
#ifdef TABLING
    }
    if (aux_sg_fr && gc_B == SgFr_gen_cp(aux_sg_fr)) {
      aux_sg_fr = SgFr_next(aux_sg_fr);
    }
#endif /* TABLING */

  restart_cp:
    /*
     * fprintf(stderr,"sweeping cps: %x, %x, %x\n",
     * *gc_B,CP_Extra(gc_B),CP_Nargs(gc_B));
     */
    /* any choice point */
    switch (opnum) {
    case _Nstop:
      /* end of the road, say bye bye! */
      sweep_environments(gc_B->cp_env, EnvSizeInCells, NULL, NOCODE PASS_REGS);
      if (gc_B->cp_env == LCL0) {
        return;
      } else {
        // This must be a border choicepoint, just move up
        gc_B = (choiceptr)(gc_B->cp_env[E_B]);
        continue;
      }
    case _trust_fail:
      break;
    case _or_else:
    case _or_last:
    case _either: {
        yamop *e_cp = NEXTOP(gc_B->cp_ap, Osblp);
        sweep_environments((CELL_PTR) (gc_B->cp_a1),
                           EnvSize(e_cp),
                           EnvBMap(e_cp),
                           e_cp PASS_REGS);
    }
      break;
    case _retry_profiled:
    case _count_retry:
      rtp = NEXTOP(rtp, l);
      op = rtp->opc;
      opnum = Yap_op_from_opcode(op);
      goto restart_cp;
    case _jump:
      rtp = rtp->y_u.l.l;
      op = rtp->opc;
      opnum = Yap_op_from_opcode(op);
      goto restart_cp;
#ifdef TABLING
    case _table_load_answer: {
      CELL *vars_ptr, vars;
      sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp),
                         EnvBMap(gc_B->cp_cp),
			 gc_B->cp_cp PASS_REGS);
      vars_ptr = (CELL *)(LOAD_CP(gc_B) + 1);
      vars = *vars_ptr++;
      while (vars--) {
        CELL cp_cell = *vars_ptr;
        if (MARKED_PTR(vars_ptr)) {
          UNMARK(vars_ptr);
          if (HEAP_PTR(cp_cell)) {
            into_relocation_chain(vars_ptr, GET_NEXT(cp_cell) PASS_REGS);
          }
        }
        vars_ptr++;
      }
    } break;
    case _table_try_answer:
    case _table_retry_me:
    case _table_trust_me:
    case _table_retry:
    case _table_trust: {
      int nargs;
      CELL *vars_ptr, vars;
      sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp),
                         EnvBMap(gc_B->cp_cp),
			 gc_B->cp_cp PASS_REGS);
      vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
      nargs = rtp->y_u.Otapl.s;
      while (nargs--) {
        CELL cp_cell = *vars_ptr;
        if (MARKED_PTR(vars_ptr)) {
          UNMARK(vars_ptr);
          if (HEAP_PTR(cp_cell)) {
            into_relocation_chain(vars_ptr, GET_NEXT(cp_cell) PASS_REGS);
          }
        }
        vars_ptr++;
      }
      vars = *vars_ptr++;
      while (vars--) {
        CELL cp_cell = *vars_ptr;
        if (MARKED_PTR(vars_ptr)) {
          UNMARK(vars_ptr);
          if (HEAP_PTR(cp_cell)) {
            into_relocation_chain(vars_ptr, GET_NEXT(cp_cell) PASS_REGS);
          }
        }
        vars_ptr++;
      }
    } break;
    case _table_completion:
#ifdef THREADS_CONSUMER_SHARING
    case _table_answer_resolution_completion:
#endif /* THREADS_CONSUMER_SHARING */
    {
      int nargs;
      CELL *vars_ptr, vars;
#ifdef DETERMINISTIC_TABLING
      if (IS_DET_GEN_CP(gc_B))
        vars_ptr = (CELL *)(DET_GEN_CP(gc_B) + 1);
      else
#endif /* DETERMINISTIC_TABLING */
      {
        sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp),
                           EnvBMap(gc_B->cp_cp),
			   gc_B->cp_cp PASS_REGS);
        vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
        nargs = SgFr_arity(GEN_CP(gc_B)->cp_sg_fr);
        while (nargs--) {
          CELL cp_cell = *vars_ptr;
          if (MARKED_PTR(vars_ptr)) {
            UNMARK(vars_ptr);
            if (HEAP_PTR(cp_cell)) {
              into_relocation_chain(vars_ptr, GET_NEXT(cp_cell) PASS_REGS);
            }
          }
          vars_ptr++;
        }
      }
      vars = *vars_ptr++;
      while (vars--) {
        CELL cp_cell = *vars_ptr;
        if (MARKED_PTR(vars_ptr)) {
          UNMARK(vars_ptr);
          if (HEAP_PTR(cp_cell)) {
            into_relocation_chain(vars_ptr, GET_NEXT(cp_cell) PASS_REGS);
          }
        }
        vars_ptr++;
      }
    } break;
    case _table_answer_resolution: {
      CELL *vars_ptr, vars;
      dep_fr_ptr dep_fr = CONS_CP(gc_B)->cp_dep_fr;
      ans_node_ptr ans_node = DepFr_last_answer(dep_fr);
      if (TRUE || TrNode_child(ans_node)) {
        /* unconsumed answers */
#ifdef MODE_DIRECTED_TABLING
        if (TrNode_child(ans_node) &&
            IS_ANSWER_INVALID_NODE(TrNode_child(ans_node))) {
          ans_node_ptr old_ans_node;
          old_ans_node = ans_node;
          ans_node = TrNode_child(ans_node);
          do {
            ans_node = TrNode_child(ans_node);
          } while (IS_ANSWER_INVALID_NODE(ans_node));
          TrNode_child(old_ans_node) = ans_node;
        } else
#endif /* MODE_DIRECTED_TABLING */
          ans_node = TrNode_child(ans_node);
        if (gc_B ==
            DepFr_leader_cp(dep_fr)) { /*  gc_B is a generator-consumer node  */
          /* never here if batched scheduling */
          TABLING_ERROR_CHECKING(generator_consumer, IS_BATCHED_GEN_CP(gc_B));
          vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
          vars_ptr += SgFr_arity(GEN_CP(gc_B)->cp_sg_fr);
        } else {
          vars_ptr = (CELL *)(CONS_CP(gc_B) + 1);
        }
        sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp),
                           EnvBMap(gc_B->cp_cp),
			   gc_B->cp_cp PASS_REGS);
        vars = *vars_ptr++;
        while (vars--) {
          CELL cp_cell = *vars_ptr;
          if (MARKED_PTR(vars_ptr)) {
            UNMARK(vars_ptr);
            if (HEAP_PTR(cp_cell)) {
              into_relocation_chain(vars_ptr, GET_NEXT(cp_cell) PASS_REGS);
            }
          }
          vars_ptr++;
        }
      }
    } break;
    case _trie_trust_var:
    case _trie_retry_var:
    case _trie_trust_var_in_pair:
    case _trie_retry_var_in_pair:
    case _trie_trust_val:
    case _trie_retry_val:
    case _trie_trust_val_in_pair:
    case _trie_retry_val_in_pair:
    case _trie_trust_atom:
    case _trie_retry_atom:
    case _trie_trust_atom_in_pair:
    case _trie_retry_atom_in_pair:
    case _trie_trust_null:
    case _trie_retry_null:
    case _trie_trust_null_in_pair:
    case _trie_retry_null_in_pair:
    case _trie_trust_pair:
    case _trie_retry_pair:
    case _trie_trust_appl:
    case _trie_retry_appl:
    case _trie_trust_appl_in_pair:
    case _trie_retry_appl_in_pair:
    case _trie_trust_extension:
    case _trie_retry_extension:
    case _trie_trust_double:
    case _trie_retry_double:
    case _trie_trust_longint:
    case _trie_retry_longint:
    case _trie_trust_gterm:
    case _trie_retry_gterm: {
      CELL *vars_ptr;
      int heap_arity, vars_arity, subs_arity;
      sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp),
                         EnvBMap(gc_B->cp_cp),
			 gc_B->cp_cp PASS_REGS);
      vars_ptr = (CELL *)(gc_B + 1);
      heap_arity = vars_ptr[0];
      vars_arity = vars_ptr[1 + heap_arity];
      subs_arity = vars_ptr[2 + heap_arity + vars_arity];
      vars_ptr += 2 + heap_arity + subs_arity + vars_arity;
      if (subs_arity) {
        while (subs_arity--) {
          CELL cp_cell = *vars_ptr;
          if (MARKED_PTR(vars_ptr)) {
            UNMARK(vars_ptr);
            if (HEAP_PTR(cp_cell)) {
              into_relocation_chain(vars_ptr, GET_NEXT(cp_cell) PASS_REGS);
            }
          }
          vars_ptr--;
        }
      }
      vars_ptr--; /* skip subs_arity entry */
      if (vars_arity) {
        while (vars_arity--) {
          CELL cp_cell = *vars_ptr;
          if (MARKED_PTR(vars_ptr)) {
            UNMARK(vars_ptr);
            if (HEAP_PTR(cp_cell)) {
              into_relocation_chain(vars_ptr, GET_NEXT(cp_cell) PASS_REGS);
            }
          }
          vars_ptr--;
        }
      }
      vars_ptr--; /* skip vars_arity entry */
      if (heap_arity) {
        while (heap_arity--) {
          CELL cp_cell = *vars_ptr;
          if (*vars_ptr == 0) /* double/longint extension mark */
            break;
          if (MARKED_PTR(vars_ptr)) {
            UNMARK(vars_ptr);
            if (HEAP_PTR(cp_cell)) {
              into_relocation_chain(vars_ptr, GET_NEXT(cp_cell) PASS_REGS);
            }
          }
          vars_ptr--;
        }
      }
    } break;
#endif /* TABLING */
    case _try_logical:
    case _retry_logical:
    case _count_retry_logical:
    case _profiled_retry_logical:
      /* sweep timestamp */
      sweep_b(gc_B, rtp->y_u.OtaLl.s + 1 PASS_REGS);
      break;
    case _trust_logical:
    case _count_trust_logical:
    case _profiled_trust_logical:
      sweep_b(gc_B, rtp->y_u.OtILl.d->ClPred->ArityOfPE + 1 PASS_REGS);
      break;
    case _retry2:
      sweep_b(gc_B, 2 PASS_REGS);
      break;
    case _retry3:
      sweep_b(gc_B, 3 PASS_REGS);
      break;
    case _retry4:
      sweep_b(gc_B, 4 PASS_REGS);
      break;
    case _retry_udi:
      sweep_b(gc_B, rtp->y_u.p.p->ArityOfPE PASS_REGS);
      break;
    case _retry_exo:
    case _retry_exo_udi:
    case _retry_all_exo:
      sweep_b(gc_B, rtp->y_u.lp.p->ArityOfPE PASS_REGS);
      break;
    case _retry_c:
    case _retry_userc: {
      register CELL_PTR saved_reg;

      /* for each extra saved register */
      for (saved_reg = &(gc_B->cp_a1) + rtp->y_u.OtapFs.s;
           saved_reg <
           &(gc_B->cp_a1) + rtp->y_u.OtapFs.s + rtp->y_u.OtapFs.extra;
           saved_reg++) {
        CELL cp_cell = *saved_reg;
        if (MARKED_PTR(saved_reg)) {
          UNMARK(saved_reg);
          if (HEAP_PTR(cp_cell)) {
            into_relocation_chain(saved_reg, GET_NEXT(cp_cell) PASS_REGS);
          }
        }
      }
    }
      /* continue to clean environments and arguments */
    default:
      sweep_b(gc_B, rtp->y_u.Otapl.s PASS_REGS);
    }

    /* link to prev choicepoint */
#if TABLING
    gc_B = youngest_cp(gc_B->cp_b, &depfr);
#else
    gc_B = gc_B->cp_b;
#endif /* TABLING */
  }
}

/* update a relocation chain to point all its cells to new location of object */
static void update_relocation_chain(CELL_PTR current, CELL_PTR dest USES_REGS) {
  CELL_PTR next;
  CELL ccur = *current;

  int rmarked = RMARKED(current);

  UNRMARK(current);
  while (rmarked) {
    CELL current_tag;
    next = GET_NEXT(ccur);
    current_tag = TAG(ccur);
    ccur = *next;
    rmarked = RMARKED(next);
    UNRMARK(next);
    *next = (CELL)dest | current_tag;
  }
  *current = ccur;
}

static inline choiceptr update_B_H(choiceptr gc_B, CELL *current, CELL *dest,
                                   CELL *odest
#ifdef TABLING
                                   ,
                                   dep_fr_ptr *depfrp
#endif /* TABLING */
) {
  /* also make the value of H in a choicepoint
     coherent with the new global
     */
#ifdef TABLING
  dep_fr_ptr depfr = *depfrp;
#endif /* TABLING */

  while (gc_B && current <= gc_B->cp_h) {
    if (gc_B->cp_h == current) {
      gc_B->cp_h = dest;
    } else {
      gc_B->cp_h = odest;
    }
    gc_B = gc_B->cp_b;
#ifdef TABLING
    /* make sure we include consumers */
    if (depfr && gc_B >= DepFr_cons_cp(depfr)) {
      gc_B = DepFr_cons_cp(depfr);
      *depfrp = depfr = DepFr_next(depfr);
    }
#endif /* TABLING */
  }
  return gc_B;
}

static inline CELL *set_next_hb(choiceptr gc_B USES_REGS) {
  if (gc_B) {
    return gc_B->cp_h;
  } else {
    return H0;
  }
}

/*
 * move marked objects on the heap upwards over unmarked objects, and reset
 * all pointers to point to new locations
 */
static void compact_heap(USES_REGS1) {
  CELL_PTR dest, current, next;
#ifdef DEBUG
  Int found_marked = 0;
#endif /* DEBUG */
  choiceptr gc_B = B;
  int in_garbage = 0;
  CELL *next_hb;
  CELL *start_from = H0;
#ifdef TABLING
  dep_fr_ptr depfr = LOCAL_top_dep_fr;
#endif /* TABLING */

  /*
   * upward phase - scan heap from high to low, setting marked upward
   * ptrs to point to what will be the new locations of the
   * objects pointed to
   */

#ifdef TABLING
  if (depfr && gc_B >= DepFr_cons_cp(depfr)) {
    gc_B = DepFr_cons_cp(depfr);
    depfr = DepFr_next(depfr);
  }
#endif /* TABLING */
  next_hb = set_next_hb(gc_B PASS_REGS);
  dest = H0 + LOCAL_total_marked - 1;

  gc_B = update_B_H(gc_B, HR, dest + 1, dest + 2
#ifdef TABLING
                    ,
                    &depfr
#endif /* TABLING */
  );
  for (current = HR - 1; current >= start_from; current--) {

    if (MARKED_PTR(current)) {
      CELL ccell = UNMARK_CELL(*current);

      if (in_garbage > 0) {
        current[1] = in_garbage;
        in_garbage = 0;
      }

      if (current <= next_hb) {
        gc_B = update_B_H(gc_B, current, dest, dest + 1
#ifdef TABLING
                          ,
                          &depfr
#endif /* TABLING */
        );
        next_hb = set_next_hb(gc_B PASS_REGS);
      }

      if (ccell == EndSpecials) {
        /* oops, we found a blob */
        CELL *ptr = current - 1;
        UInt nofcells;

        while (!MARKED_PTR(ptr)) {
          ptr--;
        }
        nofcells = current - ptr;
        ptr++;
        MARK(ptr);
#ifdef DEBUG
        // fprintf(stderr,"%p U %d\n", ptr, nofcells);
        found_marked += nofcells;
#endif
        /* first swap the tag so that it will be seen by the next step */
        current[0] = ptr[0];
        ptr[0] = EndSpecials;
        dest -= nofcells;
        current = ptr;
        /* process the functor on a separate cycle */
        DEBUG_printf21("%p %ld\n", current - 1, (long int)(nofcells + 1));
        continue;
      } else {
        DEBUG_printf20("%p 1\n", current);
      }
#ifdef DEBUG
      //  fprintf(stderr,"%p U\n", current);
      found_marked++;
#endif /* DEBUG */
      update_relocation_chain(current, dest PASS_REGS);
      if (HEAP_PTR(*current)) {
        next = GET_NEXT(*current);
        if (next < current) /* push into reloc.
                             * chain */
          into_relocation_chain(current, next PASS_REGS);
        else if (current == next) { /* cell pointing to
                                     * itself */
          UNRMARK(current);
          *current = (CELL)dest; /* no tag */
        }
      }
      dest--;
    } else {
      in_garbage++;
    }
  }
  if (in_garbage)
    start_from[0] = in_garbage;

#ifdef DEBUG
  if (dest != start_from - 1)
    fprintf(stderr, "%% Bad Dest (%lu): %p should be %p\n",
            (unsigned long int)LOCAL_GcCalls, dest, start_from - 1);
  if (LOCAL_total_marked != found_marked)
    fprintf(stderr, "%% Upward (%lu): %lu total against %lu found\n",
            (unsigned long int)LOCAL_GcCalls,
            (unsigned long int)LOCAL_total_marked,
            (unsigned long int)found_marked);
  found_marked = start_from - H0;
#endif

  /*
   * downward phase - scan heap from low to high, moving marked objects
   * to their new locations & setting downward pointers to pt to new
   * locations
   */

  dest = (CELL_PTR)start_from;
  for (current = start_from; current < HR; current++) {
    CELL ccur = *current;
    if (MARKED_PTR(current)) {
      CELL uccur = UNMARK_CELL(ccur);
      if (uccur == EndSpecials) {
        CELL *old_dest = dest;

        dest++;
        current++;
        while (!MARKED_PTR(current)) {
          *dest++ = *current++;
        }
        *old_dest = *current;
        /* if we have are calling from the C-interface,
           we may have an open array when we start the gc */
        if (LOCAL_OpenArray) {
          CELL *start = current + (dest - old_dest);
          if (LOCAL_OpenArray < current && LOCAL_OpenArray > start) {
            UInt off = LOCAL_OpenArray - start;
            LOCAL_OpenArray = old_dest + off;
          }
        }
        *dest++ = EndSpecials;
#ifdef DEBUG
        found_marked += (dest - old_dest);
#endif
        continue;
      }
#ifdef DEBUG
      found_marked++;
#endif
      update_relocation_chain(current, dest PASS_REGS);
      ccur = *current;
      next = GET_NEXT(ccur);
      if (HEAP_PTR(ccur) && (next = GET_NEXT(ccur)) < HR && /* move current cell
                                                             * & push */
          next > current) { /* into relocation chain  */
        *dest = ccur;
        into_relocation_chain(dest, next PASS_REGS);
        UNMARK(dest);
      } else {
        /* just move current cell */
        *dest = ccur = UNMARK_CELL(ccur);
      }
      /* next cell, please */
      dest++;
    } else {
      current += (ccur - 1);
    }
  }
#ifdef DEBUG
  if (LOCAL_total_marked != found_marked)
    fprintf(stderr, "%% Downward (%lu): %lu total against %lu found\n",
            (unsigned long int)LOCAL_GcCalls,
            (unsigned long int)LOCAL_total_marked,
            (unsigned long int)found_marked);
#endif

  HR = dest; /* reset H */
  HB = B->cp_h;
#ifdef TABLING
  if (B_FZ == (choiceptr)LCL0)
    H_FZ = H0;
  else
    H_FZ = B_FZ->cp_h;
#endif /* TABLING */
}

#ifdef HYBRID_SCHEME
/*
 * move marked objects on the heap upwards over unmarked objects, and reset
 * all pointers to point to new locations
 */
static void icompact_heap(USES_REGS1) {
  CELL_PTR *iptr, *ibase = (CELL_PTR *)HR;
  CELL_PTR dest;
  CELL *next_hb;
#ifdef DEBUG
  Int found_marked = 0;
#endif /* DEBUG */
#ifdef TABLING
  dep_fr_ptr depfr = LOCAL_top_dep_fr;
#endif /* TABLING */
  choiceptr gc_B = B;

  /*
   * upward phase - scan heap from high to low, setting marked upward
   * ptrs to point to what will be the new locations of the
   * objects pointed to
   */

#ifdef TABLING
  if (depfr && gc_B >= DepFr_cons_cp(depfr)) {
    gc_B = DepFr_cons_cp(depfr);
    depfr = DepFr_next(depfr);
  }
#endif /* TABLING */
  next_hb = set_next_hb(gc_B PASS_REGS);
  dest = (CELL_PTR)H0 + LOCAL_total_marked - 1;
  gc_B = update_B_H(gc_B, HR, dest + 1, dest + 2
#ifdef TABLING
                    ,
                    &depfr
#endif /* TABLING */
  );
  for (iptr = LOCAL_iptop - 1; iptr >= ibase; iptr--) {
    CELL ccell;
    CELL_PTR current;

    current = *iptr;
    ccell = UNMARK_CELL(*current);
    if (current <= next_hb) {
      gc_B = update_B_H(gc_B, current, dest, dest + 1
#ifdef TABLING
                        ,
                        &depfr
#endif /* TABLING */
      );
      next_hb = set_next_hb(gc_B PASS_REGS);
    }
    if (ccell == EndSpecials) {
      /* oops, we found a blob */
      CELL_PTR ptr;
      UInt nofcells;

      /* use the first cell after the functor for all our dirty tricks  */
      ptr = iptr[-1] + 1;
      nofcells = current - ptr;
#ifdef DEBUG
      found_marked += (nofcells + 1);
#endif /* DEBUG */
      dest -= nofcells + 1;
      /* this one's being used */
      /* make the second step see the EndSpecial tag */
      current[0] = ptr[0];
      ptr[0] = EndSpecials;
      iptr[0] = ptr;
      continue;
    }
#ifdef DEBUG
    found_marked++;
#endif /* DEBUG */
    update_relocation_chain(current, dest PASS_REGS);
    if (HEAP_PTR(*current)) {
      CELL_PTR next;
      next = GET_NEXT(*current);
      if (next < current) /* push into reloc.
                           * chain */
        into_relocation_chain(current, next PASS_REGS);
      else if (current == next) { /* cell pointing to
                                   * itself */
        UNRMARK(current);
        *current = (CELL)dest; /* no tag */
      }
    }
    dest--;
  }

#ifdef DEBUG
  if (dest != H0 - 1)
    fprintf(stderr, "%% Bad Dest (%lu): %p should be %p\n",
            (unsigned long int)LOCAL_GcCalls, dest, H0 - 1);
  if (LOCAL_total_marked != found_marked)
    fprintf(stderr, "%% Upward (%lu): %lu total against %lu found\n",
            (unsigned long int)LOCAL_GcCalls,
            (unsigned long int)LOCAL_total_marked,
            (unsigned long int)found_marked);
  found_marked = 0;
#endif

  /*
   * downward phase - scan heap from low to high, moving marked objects
   * to their new locations & setting downward pointers to pt to new
   * locations
   */

  dest = H0;
  for (iptr = ibase; iptr < LOCAL_iptop; iptr++) {
    CELL_PTR next;
    CELL *current = *iptr;
    CELL ccur = *current;
    CELL uccur = UNMARK_CELL(ccur);

    if (uccur == EndSpecials) {
      CELL *old_dest = dest;

      /* leave a hole */
      dest++;
      current++;
      while (!MARKED_PTR(current)) {
        *dest++ = *current++;
      }
      /* fill in hole */
      *old_dest = *current;
      *dest++ = EndSpecials;
#ifdef DEBUG
      found_marked += dest - old_dest;
#endif
      continue;
    }
#ifdef DEBUG
    found_marked++;
#endif
    update_relocation_chain(current, dest PASS_REGS);
    ccur = *current;
    next = GET_NEXT(ccur);
    if (HEAP_PTR(ccur) && /* move current cell &
                           * push */
        next > current) { /* into relocation chain  */
      *dest = ccur;
      into_relocation_chain(dest, next PASS_REGS);
      UNMARK(dest);
      dest++;
    } else {
      /* just move current cell */
      *dest++ = ccur = UNMARK_CELL(ccur);
    }
  }
#ifdef DEBUG
  if (H0 + LOCAL_total_marked != dest)
    fprintf(stderr, "%% Downward (%lu): %p total against %p found\n",
            (unsigned long int)LOCAL_GcCalls, H0 + LOCAL_total_marked, dest);
  if (LOCAL_total_marked != found_marked)
    fprintf(stderr, "%% Downward (%lu): %lu total against %lu found\n",
            (unsigned long int)LOCAL_GcCalls,
            (unsigned long int)LOCAL_total_marked,
            (unsigned long int)found_marked);
#endif

  HR = dest; /* reset H */
  HB = B->cp_h;
#ifdef TABLING
  if (B_FZ == (choiceptr)LCL0)
    H_FZ = H0;
  else
    H_FZ = B_FZ->cp_h;
#endif /* TABLING */
}
#endif /* HYBRID_SCHEME */

#ifdef EASY_SHUNTING
static void set_conditionals(tr_fr_ptr str USES_REGS) {
  while (str != LOCAL_sTR0) {
    CELL *cptr;
    str -= 2;
    cptr = (CELL *)TrailTerm(str + 1);
    *cptr = TrailTerm(str);
  }
  LOCAL_sTR = LOCAL_sTR0 = NULL;
}
#endif

/*
 * mark all objects on the heap that are accessible from active registers,
 * the trail, environments, and choicepoints
 */

static void marking_phase(tr_fr_ptr old_TR, CELL *current_env,
                          yamop *curp USES_REGS) {

#ifdef EASY_SHUNTING
  LOCAL_current_B = B;
  LOCAL_prev_HB = H;
#endif
  init_dbtable(old_TR PASS_REGS);
#ifdef EASY_SHUNTING
  LOCAL_sTR0 = (tr_fr_ptr)LOCAL_db_vec;
  LOCAL_sTR = (tr_fr_ptr)LOCAL_db_vec;
  /* make sure we set HB before we do any variable shunting!!! */
#else
  LOCAL_cont_top0 = (cont *)LOCAL_db_vec;
#endif
  LOCAL_cont_top = (cont *)LOCAL_db_vec;
  /* These two must be marked first so that our trail optimisation won't lose
     values */
  mark_regs(old_TR PASS_REGS); /* active registers & trail */
  /* active environments */
  mark_environments(current_env, EnvSize(curp), EnvBMap(curp), curp PASS_REGS);
  mark_choicepoints(B, old_TR,
                    is_gc_very_verbose()
                        PASS_REGS); /* choicepoints, and environs  */
#ifdef EASY_SHUNTING
  set_conditionals(LOCAL_sTR PASS_REGS);
#endif
}

static void sweep_oldgen(CELL *max, CELL *base USES_REGS) {
  CELL *ptr = base;
  char *bpb = LOCAL_bp + (base - (CELL *)LOCAL_GlobalBase);

  while (ptr < max) {
    if (*bpb) {
      if (HEAP_PTR(*ptr)) {
        into_relocation_chain(ptr, GET_NEXT(*ptr) PASS_REGS);
      }
    }
    ptr++;
    bpb++;
  }
}

/*
 * move marked heap objects upwards over unmarked objects, and reset all
 * pointers to point to new locations
 */

static void compaction_phase(tr_fr_ptr old_TR, CELL *current_env,
                             yamop *curp USES_REGS) {
  CELL *CurrentH0 = NULL;

  int icompact =
      (LOCAL_iptop < (CELL_PTR *)ASP && 10 * LOCAL_total_marked < HR - H0);

  if (icompact) {
    /* we are going to reuse the total space */
    if (LOCAL_HGEN != H0) {
      /* undo optimisation */
      LOCAL_total_marked += LOCAL_total_oldies;
    }
  } else {
    if (LOCAL_HGEN != H0) {
      CurrentH0 = H0;
      H0 = LOCAL_HGEN;
      sweep_oldgen(LOCAL_HGEN, CurrentH0 PASS_REGS);
    }
  }
  sweep_environments(current_env, EnvSize(curp), EnvBMap(curp), curp PASS_REGS);
  sweep_choicepoints(B PASS_REGS);
  sweep_trail(B, old_TR PASS_REGS);
#ifdef HYBRID_SCHEME
  if (icompact) {
#ifdef DEBUG
    /*
    if (LOCAL_total_marked
#ifdef COROUTINING
        -LOCAL_total_smarked
#endif
        != LOCAL_iptop-(CELL_PTR *)H && LOCAL_iptop < (CELL_PTR *)ASP -1024)
      fprintf(stderr,"%% Oops on LOCAL_iptop-H (%ld) vs %ld\n", (unsigned long
int)(LOCAL_iptop-(CELL_PTR *)HR), LOCAL_total_marked);
    */
#endif
#if DEBUGX
    int effectiveness = (((H - H0) - LOCAL_total_marked) * 100) / (H - H0);
    fprintf(stderr, "%% using pointers (%d)\n", effectiveness);
#endif
    if (CurrentH0) {
      H0 = CurrentH0;
      LOCAL_HGEN = H0;
      LOCAL_total_marked += LOCAL_total_oldies;
      CurrentH0 = NULL;
    }
    quicksort((CELL_PTR *)HR, 0, (LOCAL_iptop - (CELL_PTR *)HR) - 1);
    icompact_heap(PASS_REGS1);
  } else
#endif /* HYBRID_SCHEME */
  {
#ifdef DEBUG
    /*
#ifdef HYBRID_SCHEME
    int effectiveness = (((H-H0)-LOCAL_total_marked)*100)/(H-H0);
    fprintf(stderr,"%% not using pointers (%d) ASP: %p, ip %p (expected %p) \n",
effectiveness, ASP, LOCAL_iptop, H+LOCAL_total_marked);

#endif
    */
#endif
    compact_heap(PASS_REGS1);
  }
  if (CurrentH0) {
    H0 = CurrentH0;
#ifdef TABLING
    /* make sure that we havce the correct H_FZ if we're not tabling */
    if (B_FZ == (choiceptr)LCL0)
      H_FZ = H0;
#endif /* TABLING */
  }
}

static int do_gc(Int predarity, CELL *current_env, yamop *nextop USES_REGS) {
  Int heap_cells;
  int gc_verbose;
  volatile tr_fr_ptr old_TR = NULL;
  UInt m_time, c_time, time_start, gc_time;
  Int effectiveness, tot;
  bool gc_trace;
  UInt gc_phase;
  UInt alloc_sz;
  int jmp_res;
  sigjmp_buf jmp;

  heap_cells = HR - H0;
  gc_verbose = is_gc_verbose();
  effectiveness = 0;
  gc_trace = false;
  LOCAL_GcCalls++;
#ifdef INSTRUMENT_GC
  {
    int i;
    for (i = 0; i < 16; i++)
      chain[i] = 0;
    vars[gc_var] = 0;
    vars[gc_ref] = 0;
    vars[gc_atom] = 0;
    vars[gc_int] = 0;
    vars[gc_num] = 0;
    vars[gc_list] = 0;
    vars[gc_appl] = 0;
    vars[gc_func] = 0;
    vars[gc_susp] = 0;
    env_vars = 0;
    old_vars = new_vars = 0;
    TrueHB = HB;
    num_bs = 0;
  }
#endif
#ifdef DEBUG
  check_global();
#endif
  if (gcTrace() != TermOff)
    gc_trace = true;
  if (gc_trace) {
    fprintf(stderr, "%% gc\n");
  } else if (gc_verbose) {
#if defined(YAPOR) || defined(THREADS)
    fprintf(stderr, "%% Worker Id %d:\n", worker_id);
#endif
    fprintf(stderr, "%% Start of garbage collection %lu:\n",
            (unsigned long int)LOCAL_GcCalls);
    fprintf(stderr, "%%       Global: %8ld cells (%p-%p)\n",
            (long int)heap_cells, H0, HR);
    fprintf(stderr, "%%       Local:%8ld cells (%p-%p)\n",
            (unsigned long int)(LCL0 - ASP), LCL0, ASP);
    fprintf(stderr, "%%       Trail:%8ld cells (%p-%p)\n",
            (unsigned long int)(TR - (tr_fr_ptr)LOCAL_TrailBase),
            LOCAL_TrailBase, TR);
  }
#if !USE_SYSTEM_MALLOC
  if (HeapTop >= LOCAL_GlobalBase - MinHeapGap) {
    *--ASP = (CELL)current_env;
    if (!Yap_locked_growheap(FALSE, MinHeapGap, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
      return -1;
    }
    current_env = (CELL *)*ASP;
    ASP++;
  }
#endif
  time_start = Yap_cputime();
  jmp_res = sigsetjmp(jmp, 0);
  if (jmp_res == 2) {
    UInt sz;

    /* we cannot recover, fail system */
    restore_machine_regs();
    sz = LOCAL_TrailTop - (ADDR)LOCAL_OldTR;
    /* ask for double the size */
    sz = 2 * sz;
    TR = LOCAL_OldTR;

    *--ASP = (CELL)current_env;
    if (!Yap_locked_growtrail(sz, FALSE)) {
      Yap_Error(RESOURCE_ERROR_TRAIL, TermNil, "out of %lB during gc", sz);
      return -1;
    } else {
      LOCAL_total_marked = 0;
      LOCAL_total_oldies = 0;
#ifdef COROUTING
      LOCAL_total_smarked = 0;
#endif
      LOCAL_discard_trail_entries = 0;
      current_env = (CELL *)*ASP;
      ASP++;
    }
  } else if (jmp_res == 3) {
    /* we cannot recover, fail system */
    restore_machine_regs();
    TR = LOCAL_OldTR;

    LOCAL_total_marked = 0;
    LOCAL_total_oldies = 0;
#ifdef COROUTING
    LOCAL_total_smarked = 0;
#endif
    LOCAL_discard_trail_entries = 0;
    if (LOCAL_extra_gc_cells_size < 1024 * 104) {
      LOCAL_extra_gc_cells_size <<= 1;
    } else {
      LOCAL_extra_gc_cells_size += 1024 * 1024;
    }
  } else if (jmp_res == 4) {
    /* we cannot recover, fail completely */
    Yap_exit(1);
  }
#if EASY_SHUNTING
  LOCAL_sTR0 = LOCAL_sTR = NULL;
#endif
  LOCAL_total_marked = 0;
  LOCAL_total_oldies = 0;
#ifdef COROUTING
  LOCAL_total_smarked = 0;
#endif
  LOCAL_discard_trail_entries = 0;
  alloc_sz = (CELL *)LOCAL_TrailTop - (CELL *)LOCAL_GlobalBase;
  LOCAL_bp = Yap_PreAllocCodeSpace();
  while (IN_BETWEEN(LOCAL_bp, AuxSp, LOCAL_bp + alloc_sz)) {
    /* not enough space */
    *--ASP = (CELL)current_env;
    LOCAL_bp = (char *)Yap_ExpandPreAllocCodeSpace(alloc_sz, NULL, TRUE);
    if (!LOCAL_bp)
      return -1;
    current_env = (CELL *)*ASP;
    ASP++;
  }
  memset((void *)LOCAL_bp, 0, alloc_sz);
#ifdef HYBRID_SCHEME
  LOCAL_iptop = (CELL_PTR *)HR;
#endif
  /* get the number of active registers */
  LOCAL_HGEN = VarOfTerm(Yap_ReadTimedVar(LOCAL_GcGeneration));

  gc_phase = (UInt)IntegerOfTerm(Yap_ReadTimedVar(LOCAL_GcPhase));
  /* old LOCAL_HGEN are not very reliable, but still may have data to recover */
  if (gc_phase != LOCAL_GcCurrentPhase) {
    LOCAL_HGEN = H0;
  }
  /*  fprintf(stderr,"LOCAL_HGEN is %ld, %p, %p/%p\n",
   * IntegerOfTerm(Yap_ReadTimedVar(LOCAL_GcGeneration)), LOCAL_HGEN, H,H0);*/
  LOCAL_OldTR = old_TR = push_registers(predarity, nextop PASS_REGS);
  /* make sure we clean bits after a reset */
  marking_phase(old_TR, current_env, nextop PASS_REGS);
  if (LOCAL_total_oldies > ((LOCAL_HGEN - H0) * 8) / 10) {
    LOCAL_total_marked -= LOCAL_total_oldies;
    tot = LOCAL_total_marked + (LOCAL_HGEN - H0);
  } else {
    if (LOCAL_HGEN != H0) {
      LOCAL_HGEN = H0;
      LOCAL_GcCurrentPhase++;
    }
    tot = LOCAL_total_marked;
  }
  m_time = Yap_cputime();
  gc_time = m_time - time_start;
  if (heap_cells) {
    if (heap_cells > 1000000)
      effectiveness = (heap_cells - tot) / (heap_cells / 100);
    else
      effectiveness = 100 * (heap_cells - tot) / heap_cells;
  } else
    effectiveness = 0;
  if (gc_verbose) {
    fprintf(
        stderr,
        "%%   Mark: Marked %ld cells of %ld (efficiency: %ld%%) in %g sec\n",
        (long int)tot, (long int)heap_cells, (long int)effectiveness,
        (double)(m_time - time_start) / 1000);
    if (LOCAL_HGEN - H0)
      fprintf(stderr,
              "%%       previous generation has size " UInt_FORMAT
              ", with " UInt_FORMAT " (" UInt_FORMAT "%%) unmarked\n",
              (UInt)(LOCAL_HGEN - H0),
              (UInt)((LOCAL_HGEN - H0) - LOCAL_total_oldies),
              (UInt)(100 * ((LOCAL_HGEN - H0) - LOCAL_total_oldies) /
                     (LOCAL_HGEN - H0)));
#ifdef INSTRUMENT_GC
    {
      int i;
      for (i = 0; i < 16; i++) {
        if (chain[i]) {
          fprintf(stderr, "%%     chain[%d]=%lu\n", i, chain[i]);
        }
      }
      put_type_info((unsigned long int)tot);
      fprintf(stderr, "%%  %lu/%ld before and %lu/%ld after\n", old_vars,
              (unsigned long int)(B->cp_h - H0), new_vars,
              (unsigned long int)(H - B->cp_h));
      fprintf(stderr, "%%  %ld choicepoints\n", num_bs);
    }
#endif
  }
  time_start = m_time;
  compaction_phase(old_TR, current_env, nextop PASS_REGS);
  TR = old_TR;
  pop_registers(predarity, nextop PASS_REGS);
  TR = LOCAL_new_TR;
  /*  fprintf(stderr,"NEW LOCAL_HGEN %ld (%ld)\n", H-H0, LOCAL_HGEN-H0);*/
  {
    Term t = MkVarTerm();
    Yap_UpdateTimedVar(LOCAL_GcGeneration, t);
  }
  Yap_UpdateTimedVar(LOCAL_GcPhase, MkIntegerTerm(LOCAL_GcCurrentPhase));
  c_time = Yap_cputime();
  if (gc_verbose) {
    fprintf(stderr, "%%   Compress: took %g sec\n",
            (double)(c_time - time_start) / 1000);
  }
  gc_time += (c_time - time_start);
  LOCAL_TotGcTime += gc_time;
  LOCAL_TotGcRecovered += heap_cells - tot;
  if (gc_verbose) {
    fprintf(stderr, "%% GC %lu took %g sec, total of %g sec doing GC so far.\n",
            (unsigned long int)LOCAL_GcCalls, (double)gc_time / 1000,
            (double)LOCAL_TotGcTime / 1000);
    fprintf(stderr, "%%  Left %ld cells free in stacks.\n",
            (unsigned long int)(ASP - HR));
  }
  check_global();
  return effectiveness;
}

static bool is_gc_verbose(void) {
  CACHE_REGS
  if (LOCAL_PrologMode == BootMode)
    return false;
#ifdef INSTRUMENT_GC
  /* always give info when we are debugging gc */
  return true;
#else
  Term t = gcTrace();
  return t == TermVerbose || t == TermVeryVerbose;
#endif
}

bool Yap_is_gc_verbose(void) { return is_gc_verbose(); }

static bool is_gc_very_verbose(void) {
  CACHE_REGS
  if (LOCAL_PrologMode == BootMode)
    return false;
  return gcTrace() == TermVeryVerbose;
}

Int Yap_total_gc_time(void) {
  CACHE_REGS
  return (LOCAL_TotGcTime);
}

static Int p_inform_gc(USES_REGS1) {
  Term tn = MkIntegerTerm(LOCAL_TotGcTime);
  Term tt = MkIntegerTerm(LOCAL_GcCalls);
  Term ts = Yap_Mk64IntegerTerm((LOCAL_TotGcRecovered * sizeof(CELL)));

  return (Yap_unify(tn, ARG2) && Yap_unify(tt, ARG1) && Yap_unify(ts, ARG3));
}

static int call_gc(UInt gc_lim, gc_entry_info_t *i USES_REGS) {
  arity_t predarity;
  CELL *current_env;
  yamop *nextop;
  UInt gc_margin = MinStackGap;
  Term Tgc_margin;
  Int effectiveness = 0;
  int gc_on = FALSE, gc_t = FALSE;
  predarity = i->a;
  current_env = i->env;
  nextop = i->p_env;

  if (trueGlobalPrologFlag(GC_FLAG) &&
      IsIntTerm(getAtomicGlobalPrologFlag(GC_MARGIN_FLAG)))
    gc_on = true;
  else
    gc_on = false;
  if (IsIntegerTerm(Tgc_margin = getAtomicGlobalPrologFlag(GC_MARGIN_FLAG)) &&
      gc_margin > 0) {
    gc_margin = (UInt)IntegerOfTerm(Tgc_margin);
    gc_t = true;
  } else {
    /* only go exponential for the first 6 calls, that would ask about 2MB
     * minimum */
    if (LOCAL_GcCalls < 8)
      gc_margin <<= LOCAL_GcCalls;
    else {
      /* next grow linearly */
      gc_margin <<= 8;
      /* don't do this: it forces the system to ask for ever more stack!!
         gc_margin *= LOCAL_GcCalls;
      */
    }
  }
  if (gc_margin < gc_lim)
    gc_margin = gc_lim;
  LOCAL_HGEN = VarOfTerm(Yap_ReadTimedVar(LOCAL_GcGeneration));
  if (gc_on && !(LOCAL_PrologMode & InErrorMode) &&
      /* make sure there is a point in collecting the heap */
      (ASP - H0) * sizeof(CELL) > gc_lim &&
      HR - LOCAL_HGEN > (LCL0 - ASP) / 2) {
    effectiveness = do_gc(predarity, current_env, nextop PASS_REGS);
    if (effectiveness < 0)
      return FALSE;
    if (effectiveness > 90 && !gc_t) {
      while (gc_margin < (HR - H0) / sizeof(CELL))
        gc_margin <<= 1;
    }
  } else {
    effectiveness = 0;
  }
  /* expand the stack if effectiveness is less than 20 % */
  if (ASP - HR < gc_margin / sizeof(CELL) || effectiveness < 20) {
    LeaveGCMode(PASS_REGS1);
#ifndef YAPOR
    CalculateStackGap(PASS_REGS1);
    if (gc_margin < 2 * EventFlag) {
      gc_margin = 2 * EventFlag;
    }
    return Yap_locked_growstack(gc_margin);
#endif
  }
  /*
   * debug for(save_total=1; save_total<=N; ++save_total)
   * plwrite(XREGS[save_total],NULL,30,0,0,0);
   */
  return TRUE;
}

static void LeaveGCMode(USES_REGS1) {
  if (LOCAL_PrologMode & GCMode)
    LOCAL_PrologMode &= ~GCMode;
  if (LOCAL_PrologMode & AbortMode) {
    LOCAL_PrologMode &= ~AbortMode;
    /* in case someone mangles the P register */
    Yap_Error(ABORT_EVENT, TermNil, "abort from console");
    Yap_RestartYap(1);
  }
}

int Yap_gc(void *v) {
  int rc;
  gc_entry_info_t *i = v;
  rc = Yap_locked_gc(i->a, i->env, i->p_env);
  return rc;
}

int Yap_locked_gc(Int predarity, CELL *current_env, yamop *nextop) {
  CACHE_REGS
  gc_entry_info_t info;
  int res;

  info.env = current_env;
  info.p_env = nextop;
  info.a = predarity;
  info.p = P;
#if YAPOR
  fprintf(stderr, "\n\n***** Trying to call the garbage collector in "
                  "YAPOR/copying ****\n\n\n");
  exit(1);
#endif
  LOCAL_PrologMode |= GCMode;
  res = call_gc(4096, &info PASS_REGS);
  LeaveGCMode(PASS_REGS1);
  if (LOCAL_PrologMode & GCMode)
    LOCAL_PrologMode &= ~GCMode;
  return res;
}

bool Yap_expand(size_t sz USES_REGS) {
  gc_entry_info_t info;
  if (sz == 0) {
    sz = LCL0-H0;
    if (sz < 4*K*K) sz *= 2; 
    else sz += 64*K*K;
  }
  Yap_track_cpred(&info);
  return call_gc(sz * sizeof(CELL), &info PASS_REGS);
}

int Yap_gcl(UInt gc_lim, Int predarity, CELL *current_env, yamop *nextop) {
  CACHE_REGS
  int res;
  UInt min;
  gc_entry_info_t info;

  info.env = current_env;
  info.p_env = nextop;
  info.a = predarity;
  info.p = P;

  CalculateStackGap(PASS_REGS1);
  min = EventFlag * sizeof(CELL);
  LOCAL_PrologMode |= GCMode;
  if (gc_lim < min)
    gc_lim = min;
  res = call_gc(gc_lim, &info PASS_REGS);
  LeaveGCMode(PASS_REGS1);
  return res;
}

int Yap_locked_gcl(UInt gc_lim, Int predarity, CELL *current_env,
                   yamop *nextop) {
  CACHE_REGS
  int res;
  UInt min;
  gc_entry_info_t info;

  info.env = current_env;
  info.p_env = nextop;
  info.a = predarity;
  info.p = P;

  CalculateStackGap(PASS_REGS1);
  min = EventFlag * sizeof(CELL);
  LOCAL_PrologMode |= GCMode;
  if (gc_lim < min)
    gc_lim = min;
  res = call_gc(gc_lim, &info PASS_REGS);
  LeaveGCMode(PASS_REGS1);
  return res;
}

static Int p_gc(USES_REGS1) {
  int res;
  LOCAL_PrologMode |= GCMode;
  if (P->opc == Yap_opcode(_execute_cpred))
    res = do_gc(0, ENV, CP PASS_REGS) >= 0;
  else
    res = do_gc(0, ENV, P PASS_REGS) >= 0;
  LeaveGCMode(PASS_REGS1);
  return res;
}

void Yap_init_gc(void) {
  Yap_InitCPred("$gc", 0, p_gc, 0);
  Yap_InitCPred("$inform_gc", 3, p_inform_gc, 0);
}

void Yap_inc_mark_variable() {
  CACHE_REGS
  LOCAL_total_marked++;
}
 
