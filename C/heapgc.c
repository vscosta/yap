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
* File:		heapgc.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Global Stack garbage collector                           *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif /* SCCS */

#include "absmi.h"
#include "yapio.h"
#include "alloc.h"
#include "attvar.h"


#define EARLY_RESET 1
#if !defined(TABLING)
#define EASY_SHUNTING 1
#endif /* !TABLING */
#define HYBRID_SCHEME 1


/* global variables for garbage collection */

#if !defined(YAPOR) && !defined(THREADS)
/* in a single gc */
static unsigned long int   total_marked, total_oldies;	/* number of heap objects marked */

#if DEBUG
#ifdef COROUTINING
static unsigned long int   total_smarked;
#endif
#endif
#endif /* !defined(YAPOR) && !defined(THREADS) */

#ifdef EASY_SHUNTING
static choiceptr current_B;

static tr_fr_ptr sTR, sTR0;

static CELL *prev_HB;
#endif

static tr_fr_ptr new_TR;

static CELL *HGEN;

STATIC_PROTO(Int  p_inform_gc, (void));
STATIC_PROTO(Int  p_gc, (void));
STATIC_PROTO(void push_registers, (Int, yamop *));
STATIC_PROTO(void marking_phase, (tr_fr_ptr, CELL *, yamop *, CELL *));
STATIC_PROTO(void compaction_phase, (tr_fr_ptr, CELL *, yamop *, CELL *));
STATIC_PROTO(void pop_registers, (Int, yamop *));
STATIC_PROTO(void init_dbtable, (tr_fr_ptr));
STATIC_PROTO(void mark_db_fixed, (CELL *));
STATIC_PROTO(void mark_regs, (tr_fr_ptr));
STATIC_PROTO(void mark_trail, (tr_fr_ptr, tr_fr_ptr, CELL *, choiceptr));
STATIC_PROTO(void mark_environments, (CELL *, OPREG, CELL *));
STATIC_PROTO(void mark_choicepoints, (choiceptr, tr_fr_ptr, int));
STATIC_PROTO(void into_relocation_chain, (CELL *, CELL *));
STATIC_PROTO(void sweep_trail, (choiceptr, tr_fr_ptr));
STATIC_PROTO(void sweep_environments, (CELL *, OPREG, CELL *));
STATIC_PROTO(void sweep_choicepoints, (choiceptr));
STATIC_PROTO(void compact_heap, (void));
STATIC_PROTO(void update_relocation_chain, (CELL *, CELL *));
STATIC_PROTO(int  is_gc_verbose, (void));
STATIC_PROTO(int  is_gc_very_verbose, (void));

#include "heapgc.h"

#if GC_NO_TAGS
char *bp;
#endif

static int discard_trail_entries = 0;

/* support for hybrid garbage collection scheme */

typedef struct {
  CELL *v;
  int nof;
} cont;

#ifdef EASY_SHUNTING
#define cont_top0 (cont *)sTR
#else
static cont *cont_top0;
#endif
static cont *cont_top;

static void
gc_growtrail(int committed)
{
#if THREADS
  longjmp(Yap_gc_restore, 2);
#endif
#if USE_SYSTEM_MALLOC
  TR = OldTR;
#endif
  if (!Yap_growtrail(64 * 1024L, TRUE)) {
    /* could not find more trail */
    longjmp(Yap_gc_restore, 2);
  }
#if USE_SYSTEM_MALLOC
#if !GC_NO_TAGS
  if (committed) {
    longjmp(Yap_gc_restore, 2);
  }
#endif
  longjmp(Yap_gc_restore, 1);
#endif
  
}

inline static void
PUSH_CONTINUATION(CELL *v, int nof) {
  cont *x;
  if (nof == 0) return;
  x = cont_top;
  x++;
  if ((ADDR)x > Yap_TrailTop-1024) {
    gc_growtrail(TRUE);
  }
  x->v = v;
  x->nof = nof;
  cont_top = x;
}

#define POP_CONTINUATION() {   \
  if (cont_top == cont_top0)   \
    return;                    \
  else {                       \
    int nof = cont_top->nof;   \
    cont *x = cont_top;        \
                               \
    current = x->v;            \
    if (nof == 1)              \
      cont_top = --x;          \
    else {                     \
      x->nof = nof-1;          \
      x->v = current+1;        \
    }                          \
  }                            \
  goto begin; }

#ifdef HYBRID_SCHEME

static CELL_PTR *iptop;

inline static void
PUSH_POINTER(CELL *v) {
  if (iptop >= (CELL_PTR *)ASP) return;
  *iptop++ = v;
}

inline static void
POP_POINTER(void) {
  if (iptop >= (CELL_PTR *)ASP) return;
  --iptop;
}

inline static void
POPSWAP_POINTER(CELL_PTR *vp, CELL_PTR v) {
  if (iptop >= (CELL_PTR *)ASP) return;
  if (*vp != v)
    return;
  --iptop;
  if (vp != iptop)
    *vp = *iptop;
}

/*
  original code from  In Hyuk Choi,
  found at http://userpages.umbc.edu/~ichoi1/project/cs441.htm
*/

static inline void
exchange(CELL_PTR * b, Int i, Int j)
{ 
  CELL *t = b[j];

  b[j] = b[i];
  b[i] = t;
}

static UInt
partition(CELL *a[], Int p, Int r)
{ 
  CELL *x;
  UInt i, j;

  x = a[p];
  i = p+1;
  j = r;

  while (a[j] > x && i < j) {
    j--;
  }
  while (a[i] < x && i < j) {
    i++;
  }
  while(i < j) {
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
  return(i);
}

static void
insort(CELL *a[], Int p, Int q)
{
  Int j;
    
  for (j = p+1; j <= q; j ++) {
    CELL *key;
    Int i;

    key = a[j];
    i = j;

    while (i > p && a[i-1] > key) {
      a[i] = a[i-1];
      i --;
    }
    a[i] = key;
  }
}


static void
quicksort(CELL *a[], Int p, Int r)
{ 
  Int q;
  if (p < r) {
    if (r - p < 100) {
      insort(a, p, r);
      return;
    }
    exchange(a, p, (p+r)/2);
    q = partition (a, p, r);  
    quicksort(a, p, q-1);
    quicksort(a, q + 1, r);
  }
}

#else

#define PUSH_POINTER(P)
#define POP_POINTER()
#define POPSWAP_POINTER(P)

#endif /* HYBRID_SCHEME */


#ifdef MULTI_ASSIGNMENT_VARIABLES
/* 
   Based in opt.mavar.h. This is a set of routines to find out if a
   ma trail entry has appeared before in the same trail segment. All ma
   entries for the same cell are then linked. At the end of mark_trail() only
   one will remain.
*/

#define GC_MAVARS_HASH_SIZE 512

typedef struct gc_ma_hash_entry_struct {
  UInt timestmp;
  CELL* addr;
  struct gc_ma_hash_entry_struct *next;
} gc_ma_hash_entry;

static gc_ma_hash_entry gc_ma_hash_table[GC_MAVARS_HASH_SIZE];

static UInt gc_timestamp;    /* an unsigned int */

static inline unsigned int
GC_MAVAR_HASH(CELL *addr) {
#if SIZEOF_INT_P==8
  return((((unsigned int)((CELL)(addr)))>>3)%GC_MAVARS_HASH_SIZE);
#else
  return((((unsigned int)((CELL)(addr)))>>2)%GC_MAVARS_HASH_SIZE); 
#endif
}

gc_ma_hash_entry *gc_ma_h_top;

static inline gc_ma_hash_entry *
GC_ALLOC_NEW_MASPACE(void)
{
  gc_ma_hash_entry *new = gc_ma_h_top;
  if ((char *)gc_ma_h_top > Yap_TrailTop-1024)
    gc_growtrail(FALSE);
  gc_ma_h_top++;
  cont_top = (cont *)gc_ma_h_top;
#ifdef EASY_SHUNTING
  sTR = (tr_fr_ptr)cont_top;
#else
  cont_top0 = cont_top;
#endif
  return new;
}

static inline gc_ma_hash_entry*
gc_lookup_ma_var(CELL *addr, tr_fr_ptr trp) {
  unsigned int i = GC_MAVAR_HASH(addr);
  gc_ma_hash_entry *nptr, *optr = NULL;

  if (gc_ma_hash_table[i].timestmp != gc_timestamp) {
    gc_ma_hash_table[i].timestmp = gc_timestamp;
    gc_ma_hash_table[i].addr = addr;
    gc_ma_hash_table[i].next = NULL;
    return NULL;
  }
  nptr = gc_ma_hash_table+i;
  while (nptr) {
    optr = nptr;
    if (nptr->addr == addr) {
      return nptr;
    }
    nptr = nptr->next;
  }
  nptr = GC_ALLOC_NEW_MASPACE();
  optr->next = nptr;
  nptr->addr = addr;
  nptr->next = NULL;
  return NULL;
}

static inline void
GC_NEW_MAHASH(gc_ma_hash_entry *top) {
  UInt time = ++gc_timestamp;
  if (time == 0) {
    unsigned int i;

    /* damn, we overflowed */
    for (i = 0; i < GC_MAVARS_HASH_SIZE; i++)
      gc_ma_hash_table[i].timestmp = 0L;
    time = ++gc_timestamp;
  }
  gc_ma_h_top = top;
  cont_top = (cont *)gc_ma_h_top;
#ifdef EASY_SHUNTING
  sTR = (tr_fr_ptr)cont_top;
#else
  cont_top0 = cont_top;
#endif
}

#endif

/* find all accessible objects on the heap and squeeze out all the rest */


/* push the active registers onto the trail for inclusion during gc */

static void 
push_registers(Int num_regs, yamop *nextop)
{
  int             i;
  StaticArrayEntry *sal = StaticArrays;

  /* push array entries first */
  ArrayEntry *al = DynamicArrays;
  while (al) {
    TrailTerm(TR++) = al->ValueOfVE;
    al = al->NextAE;
  }
  while (sal) {
    if (sal->ArrayType == array_of_nb_terms) {
      UInt arity = -sal->ArrayEArity, i;
      for (i=0; i < arity; i++) {
	Term tlive  = sal->ValueOfVE.lterms[i].tlive;
	if (!IsVarTerm(tlive) || !IsUnboundVar(&sal->ValueOfVE.lterms[i].tlive)) {
	    TrailTerm(TR++) = tlive;
	}
      }
    }
    sal = sal->NextAE;
  }
  TrailTerm(TR) = GcGeneration;
  TR++;
#ifdef COROUTINING
  TrailTerm(TR) = WokenGoals;
  TrailTerm(TR+1) = AttsMutableList;
  TrailTerm(TR+2) = DelayedVars;
  TR += 3;
#endif
  for (i = 1; i <= num_regs; i++)
    TrailTerm(TR++) = (CELL) XREGS[i];
  /* push any live registers we might have hanging around */
  if (nextop->opc == Yap_opcode(_move_back) ||
      nextop->opc == Yap_opcode(_skip)) {
    CELL *lab = (CELL *)(nextop->u.l.l);
    CELL max = lab[0];
    Int curr = lab[1];
    lab += 2;
    if (max) {
      CELL i;
      for (i=0L; i <= max; i++) {
	if (i == 8*CellSize) {
	  curr = lab[0];
	  lab++;
	}
	if (curr & 1) {
	  TrailTerm(TR++) = XREGS[i];
	}
	curr >>= 1;
      }
    }
  }
}



/* pop the corrected register values from the trail and update the registers */

static void 
pop_registers(Int num_regs, yamop *nextop)
{
  int             i;
  tr_fr_ptr ptr = TR;
  StaticArrayEntry *sal = StaticArrays;

  /* pop array entries first */
  ArrayEntry *al = DynamicArrays;
  while (al) {
    al->ValueOfVE = TrailTerm(ptr++);
    al = al->NextAE;
  }
  sal = StaticArrays;
  while (sal) {
    if (sal->ArrayType == array_of_nb_terms) {
      UInt arity = -sal->ArrayEArity;
      for (i=0; i < arity; i++) {
	Term tlive  = sal->ValueOfVE.lterms[i].tlive;
	if (!IsVarTerm(tlive) || !IsUnboundVar(&sal->ValueOfVE.lterms[i].tlive)) {
	  sal->ValueOfVE.lterms[i].tlive = TrailTerm(ptr++);
	}
      }
    }
    sal = sal->NextAE;
  }
  GcGeneration = TrailTerm(ptr++);
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
  WokenGoals = TrailTerm(ptr++);
  AttsMutableList = TrailTerm(ptr++);
  DelayedVars = TrailTerm(ptr++);
#endif
#endif
  for (i = 1; i <= num_regs; i++)
    XREGS[i] = TrailTerm(ptr++);
  /* pop any live registers we might have hanging around */
  if (nextop->opc == Yap_opcode(_move_back) ||
      nextop->opc == Yap_opcode(_skip)) {
    CELL *lab = (CELL *)(nextop->u.l.l);
    CELL max = lab[0];
    Int curr = lab[1];
    lab += 2;
    if (max) {
      CELL i;
      for (i=0L; i <= max; i++) {
	if (i == 8*CellSize) {
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
}

#if DEBUG && COUNT_CELLS_MARKED
static int 
count_cells_marked(void)
{
  CELL *current;
  int found_marked = 0;

  for (current = H - 1; current >= H0; current--) {
    if (MARKED_PTR(current)) {
      found_marked++;
    }
  }
  return(found_marked);
}
#endif


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
  struct db_entry *left;
  CODEADDR lim;
  struct db_entry *right;
} *dbentry;

static ADDR  db_vec, db_vec0;

typedef struct RB_red_blk_node {
  CODEADDR key;
  CODEADDR lim;
  db_entry_type db_type;
  int red; /* if red=0 then the node is black */
  struct RB_red_blk_node* left;
  struct RB_red_blk_node* right;
  struct RB_red_blk_node* parent;
} rb_red_blk_node;

static rb_red_blk_node *db_root, *db_nil;

static rb_red_blk_node *
RBMalloc(UInt size)
{
  ADDR new = db_vec;

  db_vec += size; 
  if ((ADDR)db_vec > Yap_TrailTop-1024) {
    gc_growtrail(FALSE);
  }
  return (rb_red_blk_node *)new;
}

static rb_red_blk_node *
RBTreeCreate(void) {
  rb_red_blk_node* temp;

  /*  see the comment in the rb_red_blk_tree structure in red_black_tree.h */
  /*  for information on nil and root */
  temp=db_nil= RBMalloc(sizeof(rb_red_blk_node));
  temp->parent=temp->left=temp->right=temp;
  temp->red=0;
  temp->key=NULL;
  temp = RBMalloc(sizeof(rb_red_blk_node));
  temp->parent=temp->left=temp->right=db_nil;
  temp->key=NULL;
  temp->red=0;
  return temp;
}

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

static void
LeftRotate(rb_red_blk_node* x) {
  rb_red_blk_node* y;
  rb_red_blk_node* nil=db_nil;

  /*  I originally wrote this function to use the sentinel for */
  /*  nil to avoid checking for nil.  However this introduces a */
  /*  very subtle bug because sometimes this function modifies */
  /*  the parent pointer of nil.  This can be a problem if a */
  /*  function which calls LeftRotate also uses the nil sentinel */
  /*  and expects the nil sentinel's parent pointer to be unchanged */
  /*  after calling this function.  For example, when RBDeleteFixUP */
  /*  calls LeftRotate it expects the parent pointer of nil to be */
  /*  unchanged. */

  y=x->right;
  x->right=y->left;

  if (y->left != nil) y->left->parent=x; /* used to use sentinel here */
  /* and do an unconditional assignment instead of testing for nil */
  
  y->parent=x->parent;   

  /* instead of checking if x->parent is the root as in the book, we */
  /* count on the root sentinel to implicitly take care of this case */
  if( x == x->parent->left) {
    x->parent->left=y;
  } else {
    x->parent->right=y;
  }
  y->left=x;
  x->parent=y;

#ifdef DEBUG_ASSERT
  Assert(!db_nil->red,"nil not red in LeftRotate");
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

static void
RightRotate(rb_red_blk_node* y) {
  rb_red_blk_node* x;
  rb_red_blk_node* nil=db_nil;

  /*  I originally wrote this function to use the sentinel for */
  /*  nil to avoid checking for nil.  However this introduces a */
  /*  very subtle bug because sometimes this function modifies */
  /*  the parent pointer of nil.  This can be a problem if a */
  /*  function which calls LeftRotate also uses the nil sentinel */
  /*  and expects the nil sentinel's parent pointer to be unchanged */
  /*  after calling this function.  For example, when RBDeleteFixUP */
  /*  calls LeftRotate it expects the parent pointer of nil to be */
  /*  unchanged. */

  x=y->left;
  y->left=x->right;

  if (nil != x->right)  x->right->parent=y; /*used to use sentinel here */
  /* and do an unconditional assignment instead of testing for nil */

  /* instead of checking if x->parent is the root as in the book, we */
  /* count on the root sentinel to implicitly take care of this case */
  x->parent=y->parent;
  if( y == y->parent->left) {
    y->parent->left=x;
  } else {
    y->parent->right=x;
  }
  x->right=y;
  y->parent=x;

#ifdef DEBUG_ASSERT
  Assert(!db_nil->red,"nil not red in RightRotate");
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

static void
TreeInsertHelp(rb_red_blk_node* z) {
  /*  This function should only be called by InsertRBTree (see above) */
  rb_red_blk_node* x;
  rb_red_blk_node* y;
  rb_red_blk_node* nil=db_nil;
  
  z->left=z->right=nil;
  y=db_root;
  x=db_root->left;
  while( x != nil) {
    y=x;
    if (x->key < z->key) { /* x.key > z.key */
      x=x->left;
    } else { /* x,key <= z.key */
      x=x->right;
    }
  }
  z->parent=y;
  if ( (y == db_root) ||
       (y->key < z->key)) { /* y.key > z.key */
    y->left=z;
  } else {
    y->right=z;
  }

#ifdef DEBUG_ASSERT
  Assert(!db_nil->red,"nil not red in TreeInsertHelp");
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

static rb_red_blk_node *
RBTreeInsert(CODEADDR key, CODEADDR end, db_entry_type db_type) {
  rb_red_blk_node * y;
  rb_red_blk_node * x;
  rb_red_blk_node * newNode;

  x=(rb_red_blk_node*) RBMalloc(sizeof(rb_red_blk_node));
  x->key=key;
  x->lim=end;
  x->db_type=db_type;

  TreeInsertHelp(x);
  newNode=x;
  x->red=1;
  while(x->parent->red) { /* use sentinel instead of checking for root */
    if (x->parent == x->parent->parent->left) {
      y=x->parent->parent->right;
      if (y->red) {
	x->parent->red=0;
	y->red=0;
	x->parent->parent->red=1;
	x=x->parent->parent;
      } else {
	if (x == x->parent->right) {
	  x=x->parent;
	  LeftRotate(x);
	}
	x->parent->red=0;
	x->parent->parent->red=1;
	RightRotate(x->parent->parent);
      } 
    } else { /* case for x->parent == x->parent->parent->right */
      y=x->parent->parent->left;
      if (y->red) {
	x->parent->red=0;
	y->red=0;
	x->parent->parent->red=1;
	x=x->parent->parent;
      } else {
	if (x == x->parent->left) {
	  x=x->parent;
	  RightRotate(x);
	}
	x->parent->red=0;
	x->parent->parent->red=1;
	LeftRotate(x->parent->parent);
      } 
    }
  }
  db_root->left->red=0;
  return newNode;

#ifdef DEBUG_ASSERT
  Assert(!db_nil->red,"nil not red in RBTreeInsert");
  Assert(!db_root->red,"root not red in RBTreeInsert");
#endif
}


/* init the table */
static void
store_in_dbtable(CODEADDR entry, CODEADDR end, db_entry_type db_type)
{
  RBTreeInsert(entry, end, db_type);
}

/* find an element in the dbentries table */
static rb_red_blk_node *
find_ref_in_dbtable(CODEADDR entry)
{
  rb_red_blk_node *current = db_root->left;

  while (current != db_nil) {
    if (current->key < entry && current->lim > entry) {
      return current;
    }
    if (entry < current->key)
      current = current->right;
    else
      current = current->left;
  }
  return current;
}

static void 
mark_db_fixed(CELL *ptr) {
  rb_red_blk_node *el;

  el = find_ref_in_dbtable((CODEADDR)ptr);
  if (el != db_nil) {
    switch (el->db_type) {
    case db_entry:
      ((DBRef)(el->key))->Flags |= GcFoundMask;
      break;
    case cl_entry:
      ((DynamicClause *)(el->key))->ClFlags |= GcFoundMask;
      break;
    case lcl_entry:
      ((LogUpdClause *)(el->key))->ClFlags |= GcFoundMask;
      break;
    case li_entry:
      ((LogUpdIndex *)(el->key))->ClFlags |= GcFoundMask;
      break;
    case dcl_entry:
      ((DeadClause *)(el->key))->ClFlags |= GcFoundMask;
      break;
    }
  }
}

static void 
init_dbtable(tr_fr_ptr trail_ptr) {
  DeadClause *cl = DeadClauses;

  db_vec0 = db_vec = (ADDR)TR;
  db_root = RBTreeCreate();
  while (trail_ptr > (tr_fr_ptr)Yap_TrailBase) {
    register CELL trail_cell;
    
    trail_ptr--;
    
    trail_cell = TrailTerm(trail_ptr);

    if (!IsVarTerm(trail_cell) && IsPairTerm(trail_cell)) {
      CELL *pt0 = RepPair(trail_cell);
      /* DB pointer */ 
      CELL flags;

#ifdef FROZEN_STACKS  /* TRAIL */
            /* avoid frozen segments */
      if (
#ifdef SBA
	  (ADDR) pt0 >= HeapTop
#else
	  (ADDR) pt0 >= Yap_TrailBase
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
			 (CODEADDR)dbr+sizeof(DBStruct)+sizeof(CELL)*dbr->DBT.NOfCells,
			 db_entry);
      } else if (flags & LogUpdMask) {
	if (flags & IndexMask) {
	  LogUpdIndex *li = ClauseFlagsToLogUpdIndex(pt0);
	  store_in_dbtable((CODEADDR)li, (CODEADDR)li+li->ClSize, li_entry);	  
	} else {
	  LogUpdClause *cli = ClauseFlagsToLogUpdClause(pt0);
	  store_in_dbtable((CODEADDR)cli, (CODEADDR)cli+cli->ClSize, lcl_entry);	  
	}
      } else {
	DynamicClause *dcl = ClauseFlagsToDynamicClause(pt0);
	store_in_dbtable((CODEADDR)dcl, (CODEADDR)dcl+dcl->ClSize, dcl_entry);
      }
    }
  }
  while (cl != NULL) {
    store_in_dbtable((CODEADDR)cl, (CODEADDR)cl+cl->ClSize, dcl_entry);
    cl = cl->NextCl;
  }
  if (db_vec == db_vec0) {
    /* could not find any entries: probably using LOG UPD semantics */
    db_vec0 = NULL;
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
unsigned long vars[gc_susp+1];

unsigned long num_bs;
unsigned long old_vars, new_vars;

static CELL *TrueHB;

static void
inc_vars_of_type(CELL *curr,gc_types val) {
  if (curr >= H0 && curr < TrueHB) {
    old_vars++;
  } else if (curr >= TrueHB && curr < H) {
    new_vars++;
  } else {
    return;
  }
  vars[val]++;
}

static void
put_type_info(unsigned long total)
{
  fprintf(Yap_stderr,"%%  type info for %lu cells\n", total);
  fprintf(Yap_stderr,"%%      %lu vars\n", vars[gc_var]);
  fprintf(Yap_stderr,"%%      %lu refs\n", vars[gc_ref]);
  fprintf(Yap_stderr,"%%      %lu references from env\n", env_vars);
  fprintf(Yap_stderr,"%%      %lu atoms\n", vars[gc_atom]);
  fprintf(Yap_stderr,"%%      %lu small ints\n", vars[gc_int]);
  fprintf(Yap_stderr,"%%      %lu other numbers\n", vars[gc_num]);
  fprintf(Yap_stderr,"%%      %lu lists\n", vars[gc_list]);
  fprintf(Yap_stderr,"%%      %lu compound terms\n", vars[gc_appl]);
  fprintf(Yap_stderr,"%%      %lu functors\n", vars[gc_func]);
  fprintf(Yap_stderr,"%%      %lu suspensions\n", vars[gc_susp]);
}

static void
inc_var(CELL *current, CELL *next)
{
  int len = 1;
  CELL *mynext=next;

  if (ONHEAP(current)) {
    if (next == current) {
      inc_vars_of_type(current,gc_var);
      chain[0]++;
    } else {
      inc_vars_of_type(current,gc_ref);
      while(ONHEAP(mynext) && IsVarTerm(*mynext)) {
	CELL *prox = GET_NEXT(*mynext);
	if (prox == mynext) {
	  chain[0]++;
	  break;
	}
	len++;
	mynext = prox;
      }
      if (len>=15)
	(chain[15])++;
      else
	(chain[len])++;
    }
  }
}
#endif /* INSTRUMENT_GC */

int	STD_PROTO(vsc_stop,(void));

int
vsc_stop(void) {
  return(1);
}

#endif

#ifdef CHECK_GLOBAL
static void
check_global(void) {
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
      if (
	  ccell < MAX_SPECIALS_TAG &&  /* two first pages */
	  ccell > EndSpecials && IsVarTerm(ccell)) {
	/* oops, we found a blob */
	int nofcells = (UNMARK_CELL(*current)-EndSpecials) / sizeof(CELL);
	CELL *ptr = current - nofcells ;
	current = ptr;
	ccurr = *current;
	/* process the functor next */
      }
      if (MARKED_PTR(current)) {
	printf("Oops, found marked cell at %p\n", current);
	break;
      }
    }
#if INSTRUMENT_GC
    if (IsVarTerm(ccurr)) {
      if (IsBlobFunctor((Functor)ccurr)) vars[gc_num]++;
      else if (ccurr != 0 && (ccurr < (CELL)Yap_GlobalBase || ccurr > (CELL)Yap_TrailTop)) {
	/*	printf("%p: %s/%d\n", current,
	       RepAtom(NameOfFunctor((Functor)ccurr))->StrOfAE,
	       ArityOfFunctor((Functor)ccurr));*/
	vars[gc_func]++;
      }
      else if (IsUnboundVar(current)) vars[gc_var]++;
      else vars[gc_ref]++;
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
  put_type_info(H-H0);
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

static void 
mark_variable(CELL_PTR current)
{
  CELL_PTR        next;
  register CELL	ccur;
  unsigned int    arity;

 begin:
  if (MARKED_PTR(current)) {
    POP_CONTINUATION();
  }
  MARK(current);
  if (current >= H0 && current < H) {
    total_marked++;
    if (current < HGEN) {
      total_oldies++;
    }
  }
  PUSH_POINTER(current);
  ccur = *current;
  next = GET_NEXT(ccur);

  if (IsVarTerm(ccur)) {
    if (ONHEAP(next)) {
#ifdef EASY_SHUNTING
      CELL cnext;
      /* do variable shunting between variables in the global */
      cnext = *next;

      if (!MARKED_PTR(next)) {
	if (IsVarTerm(cnext) && (CELL)next == cnext) {
	  /* new global variable to new global variable */
	  if (current < prev_HB && current >= HB && next >= HB && next < prev_HB) {
#ifdef INSTRUMENT_GC
	    inc_var(current, current);
#endif	      
	    *next = (CELL)current;
#if GC_NO_TAGS
	    UNMARK(next);
	    MARK(current);
	    *current = (CELL)current;
#else
	    *current = MARK_CELL((CELL)current);
#endif
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
#if GC_NO_TAGS
	    UNMARK(current);
#endif
	    *current = cnext;
	    if (current >= H0 && current < H) {
	      total_marked--;
	      if (current < HGEN) {
		total_oldies--;
	      }
	    }
	    POP_POINTER();
	  } else {
#ifdef INSTRUMENT_GC
	    inc_var(current, next);
#endif
	    current = next;
	  }
	}
      } else if (IsVarTerm(cnext) &&
		 UNMARK_CELL(cnext) != (CELL)next &&
		 current < LCL0) {
	/* This step is possible because we clean up the trail */
	*current = UNMARK_CELL(cnext);
#if GC_NO_TAGS
	UNMARK(current);
#endif
	if (current >= H0 && current < H) {
	  total_marked--;
	  if (current < HGEN) {
	    total_oldies--;
	  }
	}
	POP_POINTER();
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
    } else if (next < (CELL *)Yap_GlobalBase || next > (CELL *)Yap_TrailTop) {
      fprintf(Yap_stderr, "ooops while marking %lx, %p at %p\n", (unsigned long int)ccur, current, next);
#endif
    } else {
#ifdef COROUTING
      total_smarked++;
#endif      
#ifdef INSTRUMENT_GC
      inc_var(current, next);
#endif
    }
    POP_CONTINUATION();
  } else if (IsPairTerm(ccur)) {
#ifdef INSTRUMENT_GC
    inc_vars_of_type(current,gc_list);
#endif
    if (ONHEAP(next)) {
      PUSH_CONTINUATION(next+1,1);
      current = next;
      goto begin;
    } else if (ONCODE(next)) {
      mark_db_fixed(RepPair(ccur));
    }
    POP_CONTINUATION();
  } else if (IsApplTerm(ccur)) {
    register CELL cnext = *next;
    
#ifdef INSTRUMENT_GC
    if (!IsExtensionFunctor((Functor)cnext))
      inc_vars_of_type(current,gc_appl);
    else
      inc_vars_of_type(current,gc_num);
#endif
    if (ONCODE(next)) {
      if ((Functor)cnext == FunctorDBRef) {
	DBRef tref = DBRefOfTerm(ccur);
	/* make sure the reference is marked as in use */
	if ((tref->Flags & ErasedMask) &&
	    tref->Parent != NULL &&
	    tref->Parent->KindOfPE & LogUpdDBBit) {
	  *current = MkDBRefTerm(DBErasedMarker);
	  MARK(current);
	} else {
	  tref->Flags |= GcFoundMask;
	}
      } else {
	mark_db_fixed(next);
      }
      POP_CONTINUATION();
    }
    if ( MARKED_PTR(next) || !ONHEAP(next) )
      POP_CONTINUATION();
    
    if (next < H0) POP_CONTINUATION();
    if (IsExtensionFunctor((Functor)cnext)) {
      switch (cnext) {
      case (CELL)FunctorLongInt:
	MARK(next);
#if GC_NO_TAGS
	MARK(next+2);
#endif
	if (next < HGEN) {
	  total_oldies+=3;
	}
	total_marked += 3;
	PUSH_POINTER(next);
	PUSH_POINTER(next+1);
	PUSH_POINTER(next+2);
	POP_CONTINUATION();
      case (CELL)FunctorDouble:
	MARK(next);
	if (next < HGEN) {
	  total_oldies+=2+SIZEOF_DOUBLE/SIZEOF_LONG_INT;
	}
	total_marked += 2+SIZEOF_DOUBLE/SIZEOF_LONG_INT;
	PUSH_POINTER(next);
	PUSH_POINTER(next+1);
	PUSH_POINTER(next+2);
#if SIZEOF_DOUBLE==2*SIZEOF_LONG_INT
	PUSH_POINTER(next+3);
#if GC_NO_TAGS
	MARK(next+3);
#endif
#elif GC_NO_TAGS
	MARK(next+2);
#endif
	POP_CONTINUATION();
#ifdef USE_GMP
      case (CELL)FunctorBigInt:
	MARK(next);
	/* size is given by functor + friends */
	if (next < HGEN) {
	  total_oldies+=2+
	    (sizeof(MP_INT)+
	     (((MP_INT *)(next+1))->_mp_alloc*sizeof(mp_limb_t)))/CellSize;
	}
	total_marked += 2+
	  (sizeof(MP_INT)+
	   (((MP_INT *)(next+1))->_mp_alloc*sizeof(mp_limb_t)))/CellSize;
	{
	  int i;
	  PUSH_POINTER(next);
	  for (i = 1; i <= (sizeof(MP_INT)+
		 (((MP_INT *)(next+1))->_mp_alloc*sizeof(mp_limb_t)))/CellSize;
	       i++) {
	    PUSH_POINTER(next+i);
	  }
#if GC_NO_TAGS
	  MARK(next+i);
#endif
	  PUSH_POINTER(next+i);
	}
	POP_CONTINUATION();
#endif
      default:
	POP_CONTINUATION();
      }
    }
    if (next < H0) POP_CONTINUATION();
#ifdef INSTRUMENT_GC
    inc_vars_of_type(next,gc_func);
#endif
    arity = ArityOfFunctor((Functor)(cnext));
    MARK(next);
    ++total_marked;
    if (next < HGEN) {
      ++total_oldies;
    }
    PUSH_POINTER(next);
    current = next+1;
    PUSH_CONTINUATION(current+1,arity-1);
    goto begin;
  }
#ifdef INSTRUMENT_GC
  else if (IsAtomTerm(ccur))
    inc_vars_of_type(current,gc_atom);
  else 
    inc_vars_of_type(current, gc_int);
#endif
  POP_CONTINUATION();
}

void 
Yap_mark_variable(CELL_PTR current)
{
  mark_variable(current);
}

static void
mark_code(CELL_PTR ptr, CELL *next)
{
  if (ONCODE(next)) {
    CELL reg = *ptr;
    if (IsApplTerm(reg) && (Functor)(*next) == FunctorDBRef) {
      DBRef tref = DBRefOfTerm(reg);
      /* make sure the reference is marked as in use */
      if ((tref->Flags & ErasedMask) &&
	  tref->Parent != NULL &&
	  tref->Parent->KindOfPE & LogUpdDBBit) {
	*ptr = MkDBRefTerm(DBErasedMarker);
      } else {
	tref->Flags |= GcFoundMask;
      }
    } else {
      mark_db_fixed(next);
    }
  }
}

static void
mark_external_reference(CELL *ptr) {
  CELL *next = GET_NEXT(*ptr);

  if (ONHEAP(next)) {
#ifdef HYBRID_SCHEME
    CELL_PTR *old = iptop;
#endif     
    mark_variable(ptr);
    POPSWAP_POINTER(old, ptr);    
  } else {
    MARK(ptr);
    mark_code(ptr, next);
  }
}

static void inline
mark_external_reference2(CELL *ptr) {
  CELL *next = GET_NEXT(*ptr);

  if (ONHEAP(next)) {
#ifdef HYBRID_SCHEME
    CELL_PTR *old = iptop;
#endif      
    mark_variable(ptr);
    POPSWAP_POINTER(old, ptr);    
  } else {
    mark_code(ptr,next);
  }
}

/*
 * mark all heap objects accessible from the trail (which includes the active
 * general purpose registers) 
 */

void
Yap_mark_external_reference(CELL *ptr) {
  mark_external_reference(ptr);
}

static void 
mark_regs(tr_fr_ptr old_TR)
{
  tr_fr_ptr        trail_ptr;

	
  /* first, whatever we dumped on the trail. Easier just to do
     the registers separately?  */
  for (trail_ptr = old_TR; trail_ptr < TR; trail_ptr++)
    mark_external_reference(&TrailTerm(trail_ptr));
}

#ifdef COROUTINING
static void 
mark_delays(CELL *max)
{
  CELL *ptr = (CELL *)Yap_GlobalBase;
  for (; ptr < max; ptr++) {
    mark_external_reference2(ptr);
  }
}
#endif

/* mark all heap objects accessible from a chain of environments */

static void 
mark_environments(CELL_PTR gc_ENV, OPREG size, CELL *pvbmap)
{
  CELL_PTR        saved_var;

  while (gc_ENV != NULL) {	/* no more environments */
    Int bmap = 0;
    int currv = 0;

#ifdef DEBUG
    if (size <  0 || size > 512)
      fprintf(Yap_stderr,"Oops, env size for %p is %ld\n", gc_ENV, (unsigned long int)size);
#endif
    mark_db_fixed((CELL *)gc_ENV[E_CP]);
    /* for each saved variable */
    if (size > EnvSizeInCells) {
      int tsize = size - EnvSizeInCells;

      currv = sizeof(CELL)*8-tsize%(sizeof(CELL)*8);
      if (pvbmap != NULL) {
	pvbmap += tsize/(sizeof(CELL)*8);
	bmap = *pvbmap;
      } else {
	bmap = -1L;
      }
      bmap = (Int)(((CELL)bmap) << currv);
    }
	  
    for (saved_var = gc_ENV - size; saved_var < gc_ENV - EnvSizeInCells; saved_var++) {
      if (currv == sizeof(CELL)*8) {
	if (pvbmap) {
	  pvbmap--;
	  bmap = *pvbmap;
	} else {
	  bmap = -1L;
	}
	currv = 0;
      }
      /* we may have already been here */
      if (bmap < 0 && !MARKED_PTR(saved_var)) {
#ifdef INSTRUMENT_GC
	Term ccur = *saved_var;
	
	if (IsVarTerm(ccur)) {
	  int len = 1;
	  CELL *mynext= GET_NEXT(ccur);

	  if (ONHEAP(mynext)) {
	    env_vars++;
	    while(ONHEAP(mynext) && IsVarTerm(*mynext)) {
	      CELL *prox = GET_NEXT(*mynext);
	      if (prox == mynext) {
		chain[0]++;
		break;
	      }
	      len++;
	      mynext = prox;
	    }
	    if (len>=15) {
	      (chain[15])++;
	    } else {
	      (chain[len])++;
	    }
	  }
	}
#endif
	mark_external_reference(saved_var);
      }
      bmap <<= 1;
      currv++;
    }
    /* have we met this environment before?? */
    /* we use the B field in the environment to tell whether we have
       been here before or not.

       We do it at the end because we don't want to lose any variables
       that would have been trimmed at the first environment visit.
    */
    if (MARKED_PTR(gc_ENV+E_CB))
      return;
    MARK(gc_ENV+E_CB);
    
    size = EnvSize((CELL_PTR) (gc_ENV[E_CP]));	/* size = EnvSize(CP) */
    pvbmap = EnvBMap((CELL_PTR) (gc_ENV[E_CP]));
#if 0
      if (size < 0) {
	PredEntry *pe = EnvPreg(gc_ENV[E_CP]);
	op_numbers op = Yap_op_from_opcode(ENV_ToOp(gc_ENV[E_CP]));
#if defined(ANALYST) || defined(DEBUG)
	fprintf(Yap_stderr,"ENV %p-%p(%d) %s\n", gc_ENV, pvbmap, size-EnvSizeInCells, Yap_op_names[op]);
#else
	fprintf(Yap_stderr,"ENV %p-%p(%d) %d\n", gc_ENV, pvbmap, size-EnvSizeInCells, (int)op);
#endif
	if (pe->ArityOfPE)
	  fprintf(Yap_stderr,"   %s/%d\n", RepAtom(NameOfFunctor(pe->FunctorOfPred))->StrOfAE, pe->ArityOfPE);
	else
	  fprintf(Yap_stderr,"   %s\n", RepAtom((Atom)(pe->FunctorOfPred))->StrOfAE);
      }
#endif
    gc_ENV = (CELL_PTR) gc_ENV[E_E];	/* link to prev
					 * environment */
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

static void
mark_trail(tr_fr_ptr trail_ptr, tr_fr_ptr trail_base, CELL *gc_H, choiceptr gc_B)
{
#ifdef EASY_SHUNTING
  tr_fr_ptr begsTR = NULL, endsTR = NULL;
#endif
  cont *old_cont_top0 = cont_top0;

  GC_NEW_MAHASH((gc_ma_hash_entry *)cont_top0);
  while (trail_base < trail_ptr) {
    register CELL trail_cell;
    
    trail_cell = TrailTerm(trail_base);

    if (IsVarTerm(trail_cell)) {
      CELL *hp = (CELL *)trail_cell;
      /* if a variable older than the current CP has not been marked yet,
	 than its new binding is not accessible and we can reset it. Note
	 we must use gc_H to avoid trouble with dangling variables
	 in the heap */
      if (((hp < gc_H   && hp >= H0) || (hp > (CELL *)gc_B && hp < LCL0) ) && !MARKED_PTR(hp)) {
#ifdef EARLY_RESET
	/* reset to be a variable */
	RESET_VARIABLE(hp);
	discard_trail_entries++;
	RESET_VARIABLE(&TrailTerm(trail_base));
#ifdef FROZEN_STACKS
	RESET_VARIABLE(&TrailVal(trail_base));
#endif
#else
	/* if I have no early reset I have to follow the trail chain */
	mark_external_reference(&TrailTerm(trail_base));	
	UNMARK(&TrailTerm(trail_base));
#endif /* EARLY_RESET */
      } else if (hp < (CELL *)Yap_GlobalBase || hp > (CELL *)Yap_TrailTop) {
	  /*  pointers from the Heap back into the trail are process in mark_regs.  */
	/* do nothing !!! */
      } else if ((hp < (CELL *)gc_B && hp >= gc_H) || hp > (CELL *)Yap_TrailBase) {
	/* clean the trail, avoid dangling pointers! */
	RESET_VARIABLE(&TrailTerm(trail_base));
#ifdef FROZEN_STACKS
	RESET_VARIABLE(&TrailVal(trail_base));
#endif
	discard_trail_entries++;
      } else {
	if (trail_cell == (CELL)trail_base)
	  discard_trail_entries++;
#ifdef FROZEN_STACKS
	else
	  mark_external_reference(&TrailVal(trail_base));
#endif
#ifdef EASY_SHUNTING
	if (hp < gc_H   && hp >= H0) {
	  tr_fr_ptr nsTR = (tr_fr_ptr)cont_top0;
          CELL *cptr = (CELL *)trail_cell;

	  if ((ADDR)nsTR > Yap_TrailTop-1024)
	    Yap_growtrail(64 * 1024L, TRUE);
	  TrailTerm(nsTR) = (CELL)NULL;
	  TrailTerm(nsTR+1) = *hp;
	  TrailTerm(nsTR+2) = trail_cell;
	  if (begsTR == NULL)
	    begsTR = nsTR;
	  else
	    TrailTerm(endsTR) = (CELL)nsTR;
	  endsTR = nsTR;
	  cont_top = (cont *)(nsTR+3);
	  sTR = (tr_fr_ptr)cont_top;
	  gc_ma_h_top = (gc_ma_hash_entry *)(nsTR+3);
	  RESET_VARIABLE(cptr);
	  MARK(cptr);
	}
#endif
      }
    } else if (IsPairTerm(trail_cell)) {
      /* can safely ignore this */
    }
#if  MULTI_ASSIGNMENT_VARIABLES
    else {
      CELL *cptr = RepAppl(trail_cell);
      /* This is a bit complex. The idea is that we may have several
	 trailings for the same mavar in the same trail segment. Essentially,
	 the problem arises because of !. What we want is to ignore all but
	 the last entry, or in this case, all but the first entry with the last
	 value.

      */
      if (!gc_lookup_ma_var(cptr, trail_base)) {
	/* check whether this is the first time we see it*/
	if (HEAP_PTR(trail_cell)) {
	  /* fool the gc into thinking this is a variable */
	  TrailTerm(trail_base) = (CELL)cptr;
	  mark_external_reference(&(TrailTerm(trail_base)));
	  /* reset the gc to believe the original tag */
	  TrailTerm(trail_base) = AbsAppl((CELL *)TrailTerm(trail_base));
#ifdef TABLING
	  mark_external_reference(&(TrailVal(trail_base)));
#endif
	}
#ifndef TABLING
	trail_base++;
	mark_external_reference(&(TrailTerm(trail_base)));
#endif
	trail_base ++;
	if (HEAP_PTR(trail_cell)) {
	  /* fool the gc into thinking this is a variable */
	  TrailTerm(trail_base) = (CELL)cptr;
	  mark_external_reference(&(TrailTerm(trail_base)));
	  /* reset the gc to believe the original tag */
	  TrailTerm(trail_base) = AbsAppl((CELL *)TrailTerm(trail_base));
#ifdef TABLING
	  mark_external_reference(&(TrailVal(trail_base)));
#endif
	} 
      } else {
	/* we can safely ignore this little monster */
	discard_trail_entries += 3;
	RESET_VARIABLE(&TrailTerm(trail_base));
#ifdef FROZEN_STACKS
	RESET_VARIABLE(&TrailVal(trail_base));
#endif
#ifndef TABLING
	trail_base++;
	RESET_VARIABLE(&TrailTerm(trail_base));
#ifdef FROZEN_STACKS
	RESET_VARIABLE(&TrailVal(trail_base));
#endif
#endif /* TABLING */
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
#ifdef EASY_SHUNTING
  sTR = (tr_fr_ptr)old_cont_top0;
  while (begsTR != NULL) {
    tr_fr_ptr newsTR = (tr_fr_ptr)TrailTerm(begsTR);
    TrailTerm(sTR) = TrailTerm(begsTR+1);
    TrailTerm(sTR+1) = TrailTerm(begsTR+2);
    begsTR = newsTR;
    sTR += 2;
  } 
#else
  cont_top0 = old_cont_top0;
#endif
  cont_top = cont_top0;
}

/*
 * mark all heap objects accessible from each choicepoint & its chain of
 * environments 
 */

#ifdef TABLING
#define init_substitution_pointer(GCB, SUBS_PTR, DEP_FR)  \
        if (DepFr_leader_cp(DEP_FR) == GCB) {             \
          /* GCB is a generator-consumer node */          \
          /* never here if batched scheduling */          \
          SUBS_PTR = (CELL *) (GEN_CP(GCB) + 1);          \
          SUBS_PTR += SgFr_arity(GEN_CP(GCB)->cp_sg_fr);  \
        } else {                                          \
          SUBS_PTR = (CELL *) (CONS_CP(GCB) + 1);         \
        }
#endif /* TABLING */


static void
mark_slots(CELL *ptr)
{
  Int ns = IntOfTerm(*ptr);
  ptr++;
  while (ns > 0) {
    mark_external_reference(ptr);
    ptr++;
    ns--;
  }
}

static void 
mark_choicepoints(register choiceptr gc_B, tr_fr_ptr saved_TR, int very_verbose)
{

  yamop *lu_cl0 = NEXTOP(PredLogUpdClause0->CodeOfPred,ld),
    *lu_cl = NEXTOP(PredLogUpdClause->CodeOfPred,ld),
    *su_cl = NEXTOP(PredStaticClause->CodeOfPred,ld);
#ifdef TABLING
  dep_fr_ptr depfr = LOCAL_top_dep_fr;
#endif /* TABLING */

#ifdef EASY_SHUNTING
  HB = H;
#endif

#ifdef TABLING
  if (depfr != NULL && gc_B >= DepFr_cons_cp(depfr)) {
    gc_B = DepFr_cons_cp(depfr);
    depfr = DepFr_next(depfr);
  }
#endif
  while (gc_B != NULL) {
    op_numbers opnum;
    register OPCODE op;
    yamop *rtp = gc_B->cp_ap;

    mark_db_fixed((CELL *)rtp);
    mark_db_fixed((CELL *)(gc_B->cp_cp));
#ifdef EASY_SHUNTING
    current_B = gc_B;
    prev_HB = HB;
#endif
    HB = gc_B->cp_h;
#ifdef INSTRUMENT_GC
    num_bs++;
#endif
#ifdef TABLING
    /* include consumers */
    if (depfr != NULL && gc_B >= DepFr_cons_cp(depfr)) {
      gc_B = DepFr_cons_cp(depfr);
      depfr = DepFr_next(depfr);
      continue;
    }
    if (rtp == NULL) {
      opnum = _table_completion;
    } else
#endif /* TABLING */
      {
	op = rtp->opc;
	opnum = Yap_op_from_opcode(op);
      }
    if (very_verbose) {
      PredEntry *pe = Yap_PredForChoicePt(gc_B);

#if defined(ANALYST) || defined(DEBUG)
      if (pe == NULL) {
	fprintf(Yap_stderr,"%%       marked %ld (%s)\n", total_marked, Yap_op_names[opnum]);
      } else if (pe->ArityOfPE) {
	fprintf(Yap_stderr,"%%       %s/%d marked %ld (%s)\n", RepAtom(NameOfFunctor(pe->FunctorOfPred))->StrOfAE, pe->ArityOfPE, total_marked, Yap_op_names[opnum]);
      } else {
	fprintf(Yap_stderr,"%%       %s marked %ld (%s)\n", RepAtom((Atom)(pe->FunctorOfPred))->StrOfAE, total_marked, Yap_op_names[opnum]);
      }
#else
      if (pe == NULL) {
	fprintf(Yap_stderr,"%%       marked %ld (%u)\n", total_marked, (unsigned int)opnum);
      } else if (pe->ArityOfPE) {
	fprintf(Yap_stderr,"%%       %s/%d marked %ld (%u)\n", RepAtom(NameOfFunctor(pe->FunctorOfPred))->StrOfAE, pe->ArityOfPE, total_marked, (unsigned int)opnum);
      } else {
	fprintf(Yap_stderr,"%%       %s marked %ld (%u)\n", RepAtom((Atom)(pe->FunctorOfPred))->StrOfAE, total_marked, (unsigned int)opnum);
      }
#endif
    }
    {
      /* find out how many cells are still alive in the trail */
#ifndef FROZEN_STACKS
      UInt d0 = discard_trail_entries, diff, orig;
      orig = saved_TR-gc_B->cp_tr;
#endif
      mark_trail(saved_TR, gc_B->cp_tr, gc_B->cp_h, gc_B);
      saved_TR = gc_B->cp_tr;
#ifndef FROZEN_STACKS
      diff = discard_trail_entries-d0;
      gc_B->cp_tr = (tr_fr_ptr)(orig-diff);
#endif /* FROZEN_STACKS */
    }
    if (opnum == _or_else || opnum == _or_last) {
      /* ; choice point */
      mark_environments((CELL_PTR) (gc_B->cp_a1),
#ifdef YAPOR
			-gc_B->cp_cp->u.ldl.s / ((OPREG)sizeof(CELL)),
			(CELL *)(gc_B->cp_cp->u.ldl.bl)
#else
			-gc_B->cp_cp->u.sla.s / ((OPREG)sizeof(CELL)),
			gc_B->cp_cp->u.sla.bmap
#endif
			);
    } else {
      /* choicepoint with arguments */
      register CELL_PTR        saved_reg;
      OPREG nargs;
	  
      if (opnum == _Nstop)
	mark_environments((CELL_PTR) gc_B->cp_env,
			  EnvSizeInCells,
			  NULL);
      else
#ifdef TABLING
	if (opnum != _table_completion)
#endif /* TABLING */
	  mark_environments((CELL_PTR) gc_B->cp_env,
			    EnvSize((CELL_PTR) (gc_B->cp_cp)),
			    EnvBMap((CELL_PTR) (gc_B->cp_cp)));
      /* extended choice point */
    restart_cp:
      switch (opnum) {
      case _Nstop:
	mark_slots(gc_B->cp_env);
	if (gc_B->cp_b != NULL) {
	  nargs = 0;
	  break;
	} else {
	  /* this is the last choice point, the work is done  ;-) */
	  return;
	}
      case _retry_c:
      case _retry_userc:
	if (gc_B->cp_ap == RETRY_C_RECORDED_K_CODE 
	    || gc_B->cp_ap == RETRY_C_RECORDEDP_CODE) {
	  /* we have a reference from the choice-point stack to a term */
	  choiceptr old_b = B;
	  DBRef ref;
	  B = gc_B;
	  ref = (DBRef)EXTRA_CBACK_ARG(3,1);
	  if (IsVarTerm((CELL)ref))
	    ref->Flags |= GcFoundMask;
	  else {
	    if (ONCODE((CELL)ref)) {
	      mark_db_fixed(RepAppl((CELL)ref));
	    }
	  }
	  B = old_b;
	}
	nargs = rtp->u.lds.s+rtp->u.lds.extra;
	break;
      case _jump:
	rtp = rtp->u.l.l;
	op = rtp->opc;
	opnum = Yap_op_from_opcode(op);
	goto restart_cp;
      case _trust_logical_pred:
      case _retry_profiled:
      case _count_retry:
	rtp = NEXTOP(rtp,l); 
	op = rtp->opc;
	opnum = Yap_op_from_opcode(op);
	goto restart_cp;
      case _trust_fail:
	nargs = 0;
	break;
#ifdef TABLING
      case _table_load_answer:
	{
	  CELL *vars_ptr, vars;
	  vars_ptr = (CELL *) (LOAD_CP(gc_B) + 1);
	  vars = *vars_ptr++;
	  while (vars--) {	
	    mark_external_reference(vars_ptr);
	    vars_ptr++;
	  }
	}
	nargs = 0;
	break;
      case _table_try_answer:
      case _table_retry_me:
      case _table_trust_me:
      case _table_retry:
      case _table_trust:
	{
	  CELL *vars_ptr, vars;
	  vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
	  nargs = rtp->u.ld.s;
	  while (nargs--) {	
	    mark_external_reference(vars_ptr);
	    vars_ptr++;
	  }
	  vars = *vars_ptr++;
	  while (vars--) {	
	    mark_external_reference(vars_ptr);
	    vars_ptr++;
	  }
	}
	nargs = 0;
	break;
      case _table_completion:
	if (rtp) {
	  CELL *vars_ptr, vars;
	  vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
	  nargs = SgFr_arity(GEN_CP(gc_B)->cp_sg_fr);
	  while (nargs--) {	
	    mark_external_reference(vars_ptr);
	    vars_ptr++;
	  }
	  vars = *vars_ptr++;
	  while (vars--) {	
	    mark_external_reference(vars_ptr);
	    vars_ptr++;
	  }
	}
	nargs = 0;
	break;
      case _table_answer_resolution:
	{
	  CELL *vars_ptr, vars;
	  init_substitution_pointer(gc_B, vars_ptr, CONS_CP(gc_B)->cp_dep_fr);
	  vars = *vars_ptr++;
	  while (vars--) {	
	    mark_external_reference(vars_ptr);
	    vars_ptr++;
	  }
	}
	nargs = 0;
	break;
      case _trie_retry_null:
      case _trie_trust_null:
      case _trie_retry_var:
      case _trie_trust_var:
      case _trie_retry_val:
      case _trie_trust_val:
      case _trie_retry_atom:
      case _trie_trust_atom:
      case _trie_retry_list:
      case _trie_trust_list:
      case _trie_retry_struct:
      case _trie_trust_struct:
      case _trie_retry_extension:
      case _trie_trust_extension:
      case _trie_retry_float:
      case _trie_trust_float:
      case _trie_retry_long:
      case _trie_trust_long:
	{
	  CELL *vars_ptr;
	  int heap_arity, vars_arity, subs_arity;
	  vars_ptr = (CELL *)(gc_B + 1);
	  heap_arity = *vars_ptr;
	  vars_arity = *(vars_ptr + heap_arity + 1);
	  subs_arity = *(vars_ptr + heap_arity + 2);
	  vars_ptr += heap_arity + subs_arity + vars_arity + 2;
	  if (vars_arity) {
	    while (vars_arity--) {	
	      mark_external_reference(vars_ptr);
	      vars_ptr--;
	    }
	  }
	  if (subs_arity) {
	    while (subs_arity--) {	
	      mark_external_reference(vars_ptr);
	      vars_ptr--;
	    }
	  }
	  vars_ptr -= 2;
	  if (heap_arity) {
	    while (heap_arity--) {	
	      if (*vars_ptr == 0)
		break;  /* term extension mark: float/longint */
	      mark_external_reference(vars_ptr);
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
	ClauseCodeToDynamicClause(gc_B->cp_ap)->ClFlags |= GcFoundMask;
      case _retry2:
	nargs = 2;
	break;
      case _retry3:
	nargs = 3;
	break;
      case _retry4:
	nargs = 4;
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
	nargs = rtp->u.ld.s;
	break;
      default:
	fprintf(Yap_stderr, "OOps in GC: Unexpected opcode: %d\n", opnum);
	nargs = 0;
#else
      default:
	nargs = rtp->u.ld.s;
#endif
      }
	

      if (gc_B->cp_ap == lu_cl0 ||
	  gc_B->cp_ap == lu_cl ||
	  gc_B->cp_ap == su_cl) {
	CELL *pt = (CELL *)IntegerOfTerm(gc_B->cp_args[1]);
	mark_db_fixed(pt);
      }
      /* for each saved register */
      for (saved_reg = &gc_B->cp_a1;
	   /* assumes we can count registers in CP this
	      way */
	   saved_reg < &gc_B->cp_a1 + nargs;
	   saved_reg++) {
	mark_external_reference(saved_reg);
      }
    }	    
    gc_B = gc_B->cp_b;
  }
}




/*
 * insert a cell which points to a heap object into relocation chain of that
 * object 
 */

static inline void 
into_relocation_chain(CELL_PTR current, CELL_PTR next)
{
#if GC_NO_TAGS
  CELL             current_tag;

  current_tag = TAG(*current);
  if (RMARKED(next))
    RMARK(current);
  else {
    UNRMARK(current);
    RMARK(next);
  }
  *current = *next;
  *next = (CELL) current | current_tag;
#else
#ifdef TAGS_FAST_OPS
  register CELL ccur = *current, cnext = *next;

  if (IsVarTerm(ccur)) {
    *current = ( MARKED(ccur) ? MARK_CELL(UNMARKED(cnext)) :
		 UNMARKED(cnext) );
    *next = (MARKED(cnext) ? MBIT : 0) | RBIT | (Int) current;
  } else if (IsPairTerm(ccur)) {
    *current = ( MARKED(ccur) ? MARK_CELL(UNMARKED(cnext)) :
		 UNMARKED(cnext) );
    *next = AbsPair((CELL *)
		    ((MARKED(cnext) ? MBIT : 0) | RBIT | (Int) current));
  } else if (IsApplTerm(ccur)) {
    *current = ( MARKED(ccur) ? MARK_CELL(UNMARKED(cnext)) :
		 UNMARKED(cnext) );
    *next = AbsAppl((CELL *)
		    ((MARKED(cnext) ? MBIT : 0) | RBIT | (Int) current));
  } else {
    fprintf(Yap_stderr," OH MY GOD !!!!!!!!!!!!\n");
  }
#else
  CELL             current_tag;

  current_tag = TAG(*current);
  *current = (*current & MBIT) | (*next & ~MBIT);
#if INVERT_RBIT
  *next = ((*next & MBIT) | (CELL) current | current_tag) & ~RBIT;
#else
  *next = (*next & MBIT) | RBIT | (CELL) current | current_tag;
#endif
#endif
#endif /* GC_NO_TAGS */
}


/* insert trail cells which point to heap objects into relocation chains */

static void 
sweep_trail(choiceptr gc_B, tr_fr_ptr old_TR)
{
  tr_fr_ptr     trail_ptr, dest;
#if !USE_MALLOC
  Int OldHeapUsed = HeapUsed;
#endif
#ifdef DEBUG
  Int hp_entrs = 0, hp_erased = 0, hp_not_in_use = 0,
    hp_in_use_erased = 0, code_entries = 0;
#endif

#ifndef FROZEN_STACKS
  /* 
     adjust cp_tr pointers,
     we don't compress TR if we have freeze.
   */
 {
   Int size = old_TR-(tr_fr_ptr)Yap_TrailBase;
   size -= discard_trail_entries;
   while (gc_B != NULL) {
     size -= (UInt)(gc_B->cp_tr);
     gc_B->cp_tr = (tr_fr_ptr)Yap_TrailBase+size;
     gc_B = gc_B->cp_b;
   }
 }
#endif /* FROZEN_STACKS */

  /* first, whatever we dumped on the trail. Easier just to do
     the registers separately?  */
  for (trail_ptr = old_TR; trail_ptr < TR; trail_ptr++) {
    if (IN_BETWEEN(Yap_GlobalBase,TrailTerm(trail_ptr),Yap_TrailTop) &&
	MARKED_PTR(&TrailTerm(trail_ptr))) {
      UNMARK(&TrailTerm(trail_ptr));
      if (HEAP_PTR(TrailTerm(trail_ptr))) {
	into_relocation_chain(&TrailTerm(trail_ptr), GET_NEXT(TrailTerm(trail_ptr)));
      }
    }
  }
 
  /* next, follows the real trail entries */
  trail_ptr = (tr_fr_ptr)Yap_TrailBase;
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
	/* make sure it is a heap cell before we test whether it has been marked */
	if ((CELL *)trail_cell < H && (CELL *)trail_cell >= H0 && MARKED_PTR((CELL *)trail_cell)) {
	  if (HEAP_PTR(trail_cell)) {
	    into_relocation_chain(&TrailTerm(dest), GET_NEXT(trail_cell));
	  }
#ifdef FROZEN_STACKS
	  /* it is complex to recover cells with frozen segments */
	  TrailVal(dest) = TrailVal(trail_ptr);
	  if (MARKED_PTR(&TrailVal(dest))) {
	    UNMARK(&TrailVal(dest));
	    if (HEAP_PTR(TrailVal(dest))) {
	      into_relocation_chain(&TrailVal(dest), GET_NEXT(TrailVal(dest)));
	    }
	  }
#endif
	}
      } else if (IsPairTerm(trail_cell)) {
	CELL *pt0 = RepPair(trail_cell);
	CELL flags;


#ifdef FROZEN_STACKS  /* TRAIL */
	/* process all segments */
	if (
#ifdef SBA
	    (ADDR) pt0 >= Yap_GlobalBase
#else
	    (ADDR) pt0 >= Yap_TrailBase
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
	if (!FlagOn(GcFoundMask, flags)) {
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
      	if (!FlagOn(GcFoundMask, flags)) {
	  if (FlagOn(DBClMask, flags)) {
	    DBRef dbr = (DBRef) ((CELL)pt0 - (CELL) &(((DBRef) NIL)->Flags));
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
		DEC_CLREF_COUNT(indx);
		indx->ClFlags &= ~InUseMask;
		erase = (indx->ClFlags & ErasedMask
			 && !indx->ClRefCount);
		if (erase) {
		  /* at this point, 
		     no one is accessing the clause */
		  Yap_ErLogUpdIndex(indx, NULL);
		}
	      } else {
		LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt0);
		int erase;

		DEC_CLREF_COUNT(cl);
		cl->ClFlags &= ~InUseMask;
		erase = ((cl->ClFlags & ErasedMask) && !cl->ClRefCount);
		if (erase) {
		  /* at this point, 
		     no one is accessing the clause */
		  Yap_ErLogUpdCl(cl);
		}
	      }
	    } else {
	      DynamicClause *cl = ClauseFlagsToDynamicClause(pt0);
	      int erase;
	      DEC_CLREF_COUNT(cl);
	      cl->ClFlags &= ~InUseMask;
	      erase = (cl->ClFlags & ErasedMask)
#if  defined(YAPOR) || defined(THREADS)
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
	  discard_trail_entries++;
	} else {
	  *pt0 = ResetFlag(GcFoundMask, flags);
	}
#if  MULTI_ASSIGNMENT_VARIABLES
      } else {
#ifdef FROZEN_STACKS
	CELL trail_cell = TrailTerm(trail_ptr+1);
	CELL old = TrailVal(trail_ptr);
	CELL old1 = TrailVal(trail_ptr+1);
	Int marked_ptr = MARKED_PTR(&TrailTerm(trail_ptr+1));
	Int marked_val_old = MARKED_PTR(&TrailVal(trail_ptr));
	Int marked_val_ptr = MARKED_PTR(&TrailVal(trail_ptr+1));

	TrailTerm(dest+1) = TrailTerm(dest) = trail_cell;
	if (marked_ptr) {
	  UNMARK(&TrailTerm(dest));
	  UNMARK(&TrailTerm(dest+1));
	  if (HEAP_PTR(trail_cell)) {
	    into_relocation_chain(&TrailTerm(dest), GET_NEXT(trail_cell));
	    into_relocation_chain(&TrailTerm(dest+1), GET_NEXT(trail_cell));
	  }
	}
	if (marked_val_old) {
	  UNMARK(&TrailVal(dest));
	  if (HEAP_PTR(old)) {
	    into_relocation_chain(&TrailVal(dest), GET_NEXT(old));
	  }
	}
	if (marked_val_ptr) {
	  UNMARK(&TrailVal(dest+1));
	  if (HEAP_PTR(old1)) {
	    into_relocation_chain(&TrailVal(dest+1), GET_NEXT(old1));
	  }
	}
	trail_ptr ++;
	dest ++;
#else
	CELL trail_cell = TrailTerm(trail_ptr+2);
 	CELL old = TrailTerm(trail_ptr+1);
	Int marked_ptr = MARKED_PTR(&TrailTerm(trail_ptr+2));
	Int marked_old = MARKED_PTR(&TrailTerm(trail_ptr+1));
	CELL *ptr;
	/* be sure we don't overwrite before we read */

	if (marked_ptr) 
	  ptr = RepAppl(UNMARK_CELL(trail_cell));
	else
	  ptr = RepAppl(trail_cell);

	TrailTerm(dest+1) = old;
	if (marked_old) {
	  UNMARK(&TrailTerm(dest+1));
	  if (HEAP_PTR(old)) {
	    into_relocation_chain(&TrailTerm(dest+1), GET_NEXT(old));
	  }
	}
	TrailTerm(dest+2) = TrailTerm(dest) = trail_cell;
	if (marked_ptr) {
	  UNMARK(&TrailTerm(dest));
	  UNMARK(&TrailTerm(dest+2));
	  if (HEAP_PTR(trail_cell)) {
	    into_relocation_chain(&TrailTerm(dest), GET_NEXT(trail_cell));
	    into_relocation_chain(&TrailTerm(dest+2), GET_NEXT(trail_cell));
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
  new_TR = dest;
  if (is_gc_verbose()) {
    if (old_TR != (tr_fr_ptr)Yap_TrailBase)
      fprintf(Yap_stderr,
		 "%%       Trail: discarded %d (%ld%%) cells out of %ld\n",
		 discard_trail_entries,
		 (unsigned long int)(discard_trail_entries*100/(old_TR-(tr_fr_ptr)Yap_TrailBase)),
		 (unsigned long int)(old_TR-(tr_fr_ptr)Yap_TrailBase));
#ifdef DEBUG
    if (hp_entrs > 0)
      fprintf(Yap_stderr,
		 "%%       Trail: unmarked %ld dbentries (%ld%%) out of %ld\n",
		 (long int)hp_not_in_use,
		 (long int)(hp_not_in_use*100/hp_entrs),
		 (long int)hp_entrs);
    if (hp_in_use_erased > 0 && hp_erased > 0)
      fprintf(Yap_stderr,
		 "%%       Trail: deleted %ld dbentries (%ld%%) out of %ld\n",
		 (long int)hp_erased,
		 (long int)(hp_erased*100/(hp_erased+hp_in_use_erased)),
		 (long int)(hp_erased+hp_in_use_erased));
#endif
#if !USE_SYSTEM_MALLOC
    fprintf(Yap_stderr,
	    "%%       Heap: recovered %ld bytes (%ld%%) out of %ld\n",
	    (unsigned long int)(OldHeapUsed-HeapUsed),
	    (unsigned long int)((OldHeapUsed-HeapUsed)/(OldHeapUsed/100)),
	    (unsigned long int)OldHeapUsed);
#endif
  }
  {
    DeadClause **cptr;
    DeadClause *cl;

    cptr = &(DeadClauses);
    cl = DeadClauses;
    while (cl != NULL) {
      if (!(cl->ClFlags & GcFoundMask)) {
	char *ocl = (char *)cl;
	cl = cl->NextCl;
	*cptr = cl;
	Yap_FreeCodeSpace(ocl);
      } else {
	cl->ClFlags &= ~GcFoundMask;
	cptr = &(cl->NextCl);
	cl = cl->NextCl;
      }
    }
  }
}


/*
 * insert cells of a chain of environments which point to heap objects into
 * relocation chains 
 */

static void 
sweep_environments(CELL_PTR gc_ENV, OPREG size, CELL *pvbmap)
{
  CELL_PTR        saved_var;

  while (gc_ENV != NULL) {	/* no more environments */
    Int bmap = 0;
    int currv = 0;


    /* for each saved variable */

    if (size > EnvSizeInCells) {
      int tsize = size - EnvSizeInCells;

      
      currv = sizeof(CELL)*8-tsize%(sizeof(CELL)*8);
      if (pvbmap != NULL) {
	pvbmap += tsize/(sizeof(CELL)*8);
	bmap = *pvbmap;
      } else {
	bmap = -1L;
      }
      bmap = (Int)(((CELL)bmap) << currv);
    }
    for (saved_var = gc_ENV - size; saved_var < gc_ENV - EnvSizeInCells; saved_var++) {
      
      if (currv == sizeof(CELL)*8) {
	if (pvbmap != NULL) {
	  pvbmap--;
	  bmap = *pvbmap;
	} else {
	  bmap = -1L;
	}
	currv = 0;
      }
      if (bmap < 0) {
	CELL env_cell = *saved_var;
	if (MARKED_PTR(saved_var)) {
	  UNMARK(saved_var);
	  if (HEAP_PTR(env_cell)) {
	    into_relocation_chain(saved_var, GET_NEXT(env_cell));
	  }
	}
      }
      bmap <<= 1;
      currv++;
    }
    /* have we met this environment before?? */
    /* we use the B field in the environment to tell whether we have
       been here before or not 
    */
    if (!MARKED_PTR(gc_ENV+E_CB))
      return;
    UNMARK(gc_ENV+E_CB);

    size = EnvSize((CELL_PTR) (gc_ENV[E_CP]));	/* size = EnvSize(CP) */
    pvbmap = EnvBMap((CELL_PTR) (gc_ENV[E_CP]));
    gc_ENV = (CELL_PTR) gc_ENV[E_E];	/* link to prev
					 * environment */
  }
}

static void
sweep_slots(CELL *ptr)
{
  Int ns = IntOfTerm(*ptr);
  ptr++;
  while (ns > 0) {
    CELL cp_cell = *ptr;
    if (MARKED_PTR(ptr)) {
      UNMARK(ptr);
      if (HEAP_PTR(cp_cell)) {
	into_relocation_chain(ptr, GET_NEXT(cp_cell));
      }
    }
    ptr++;
    ns--;
  }
}

static void
sweep_b(choiceptr gc_B, UInt arity)
{
  register CELL_PTR saved_reg;

  sweep_environments(gc_B->cp_env,
		     EnvSize((CELL_PTR) (gc_B->cp_cp)),
		     EnvBMap((CELL_PTR) (gc_B->cp_cp)));

  /* for each saved register */
  for (saved_reg = &gc_B->cp_a1;
       saved_reg < &gc_B->cp_a1 + arity;
       saved_reg++) {
    CELL cp_cell = *saved_reg;
    if (MARKED_PTR(saved_reg)) {
      UNMARK(saved_reg);
      if (HEAP_PTR(cp_cell)) {
	into_relocation_chain(saved_reg, GET_NEXT(cp_cell));
      }
    }
  }
}


/*
 * insert cells of each choicepoint & its chain of environments which point
 * to heap objects into relocation chains 
 */
static void 
sweep_choicepoints(choiceptr gc_B)
{
#ifdef TABLING
   dep_fr_ptr depfr = LOCAL_top_dep_fr;
#endif /* TABLING */

#ifdef TABLING
  if (depfr != NULL && gc_B >= DepFr_cons_cp(depfr)) {
    gc_B = DepFr_cons_cp(depfr);
    depfr = DepFr_next(depfr);
  }
#endif
  while(gc_B != NULL) {
    yamop *rtp = gc_B->cp_ap;
    register OPCODE op;
    op_numbers opnum;

#ifdef TABLING
    /* include consumers */
    if (depfr != NULL && gc_B >= DepFr_cons_cp(depfr)) {
      gc_B = DepFr_cons_cp(depfr);
      depfr = DepFr_next(depfr);
      continue;
    }
    if (rtp == NULL) {
      opnum = _table_completion;
    } else
#endif /* TABLING */
      {
	op = rtp->opc;
	opnum = Yap_op_from_opcode(op);
      }

  restart_cp:
    /*
     * fprintf(Yap_stderr,"sweeping cps: %x, %x, %x\n",
     * *gc_B,CP_Extra(gc_B),CP_Nargs(gc_B)); 
     */
    /* any choice point */
    switch (opnum) {
    case _Nstop:
      /* end of the road, say bye bye! */
      sweep_environments(gc_B->cp_env,
			 EnvSizeInCells,
			 NULL);
      sweep_slots(gc_B->cp_env);
      if (gc_B->cp_b != NULL) {
	break;
      } else
	return;
    case _trust_fail:
      sweep_environments(gc_B->cp_env,
			 EnvSizeInCells,
			 NULL);
      break;
    case _or_else:
    case _or_last:

      sweep_environments((CELL_PTR)(gc_B->cp_a1),
#ifdef YAPOR
			 -gc_B->cp_cp->u.ldl.s / ((OPREG)sizeof(CELL)),
			 (CELL *)(gc_B->cp_cp->u.ldl.bl)
#else
			 -gc_B->cp_cp->u.sla.s / ((OPREG)sizeof(CELL)),
			 gc_B->cp_cp->u.sla.bmap
#endif
			 );
      break;
    case _trust_logical_pred:
    case _retry_profiled:
    case _count_retry:
      rtp = NEXTOP(rtp,l);
      op = rtp->opc;
      opnum = Yap_op_from_opcode(op);
      goto restart_cp;
    case _jump:
      rtp = rtp->u.l.l;
      op = rtp->opc;
      opnum = Yap_op_from_opcode(op);
      goto restart_cp;
#ifdef TABLING
    case _table_load_answer:
      {
	CELL *vars_ptr, vars;
	sweep_environments(gc_B->cp_env, EnvSize((CELL_PTR) (gc_B->cp_cp)), EnvBMap((CELL_PTR) (gc_B->cp_cp)));
	vars_ptr = (CELL *) (LOAD_CP(gc_B) + 1);
	vars = *vars_ptr++;
	while (vars--) {	
	  CELL cp_cell = *vars_ptr;
	  if (MARKED_PTR(vars_ptr)) {
	    UNMARK(vars_ptr);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
	    }
	  }
	  vars_ptr++;
	}
      }
      break;
    case _table_try_answer:
    case _table_retry_me:
    case _table_trust_me:
    case _table_retry:
    case _table_trust:
      {
	int nargs;
	CELL *vars_ptr, vars;
	sweep_environments(gc_B->cp_env, EnvSize((CELL_PTR) (gc_B->cp_cp)), EnvBMap((CELL_PTR) (gc_B->cp_cp)));
	vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
	nargs = rtp->u.ld.s;
	while(nargs--) {
	  CELL cp_cell = *vars_ptr;
	  if (MARKED_PTR(vars_ptr)) {
	    UNMARK(vars_ptr);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
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
	      into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
	    }
	  }
	  vars_ptr++;
	}
      }
      break;
    case _table_completion:
      if (rtp) {
	int nargs;
	CELL *vars_ptr, vars;
	sweep_environments(gc_B->cp_env, EnvSize((CELL_PTR) (gc_B->cp_cp)), EnvBMap((CELL_PTR) (gc_B->cp_cp)));
	vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
	nargs = SgFr_arity(GEN_CP(gc_B)->cp_sg_fr);	  
	while(nargs--) {
	  CELL cp_cell = *vars_ptr;
	  if (MARKED_PTR(vars_ptr)) {
	    UNMARK(vars_ptr);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
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
	      into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
	    }
	  }
	  vars_ptr++;
	}
      }
      break;
    case _table_answer_resolution:
      {
	CELL *vars_ptr, vars;
	sweep_environments(gc_B->cp_env, EnvSize((CELL_PTR) (gc_B->cp_cp)), EnvBMap((CELL_PTR) (gc_B->cp_cp)));
	init_substitution_pointer(gc_B, vars_ptr, CONS_CP(gc_B)->cp_dep_fr);
	vars = *vars_ptr++;
	while (vars--) {	
	  CELL cp_cell = *vars_ptr;
	  if (MARKED_PTR(vars_ptr)) {
	    UNMARK(vars_ptr);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
	    }
	  }
	  vars_ptr++;
	}
      }
      break;
    case _trie_retry_null:
    case _trie_trust_null:
    case _trie_retry_var:
    case _trie_trust_var:
    case _trie_retry_val:
    case _trie_trust_val:
    case _trie_retry_atom:
    case _trie_trust_atom:
    case _trie_retry_list:
    case _trie_trust_list:
    case _trie_retry_struct:
    case _trie_trust_struct:
    case _trie_retry_extension:
    case _trie_trust_extension:
    case _trie_retry_float:
    case _trie_trust_float:
    case _trie_retry_long:
    case _trie_trust_long:
      {
	CELL *vars_ptr;
	int heap_arity, vars_arity, subs_arity;
	sweep_environments(gc_B->cp_env, EnvSize((CELL_PTR) (gc_B->cp_cp)), EnvBMap((CELL_PTR) (gc_B->cp_cp)));
	vars_ptr = (CELL *)(gc_B + 1);
	heap_arity = *vars_ptr;
	vars_arity = *(vars_ptr + heap_arity + 1);
	subs_arity = *(vars_ptr + heap_arity + 2);
	vars_ptr += heap_arity + subs_arity + vars_arity + 2;
	if (vars_arity) {
	  while (vars_arity--) {	
	    CELL cp_cell = *vars_ptr;
	    if (MARKED_PTR(vars_ptr)) {
	      UNMARK(vars_ptr);
	      if (HEAP_PTR(cp_cell)) {
		into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
	      }
	    }
	    vars_ptr--;
	  }
	}
	if (subs_arity) {
	  while (subs_arity--) {	
	    CELL cp_cell = *vars_ptr;
	    if (MARKED_PTR(vars_ptr)) {
	      UNMARK(vars_ptr);
	      if (HEAP_PTR(cp_cell)) {
		into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
	      }
	    }
	    vars_ptr--;
	  }
	}
	vars_ptr -= 2;
	if (heap_arity) {
	  while (heap_arity--) {
	    CELL cp_cell = *vars_ptr;
	    if (*vars_ptr == 0)
	      break;  /* term extension mark: float/longint */
	    if (MARKED_PTR(vars_ptr)) {
	      UNMARK(vars_ptr);
	      if (HEAP_PTR(cp_cell)) {
		into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
	      }
	    }
	    vars_ptr--;
	  }
	}
      }
      break;
#endif /* TABLING */
    case _retry2:
      sweep_b(gc_B, 2);
      break;
    case _retry3:
      sweep_b(gc_B, 3);
      break;
    case _retry4:
      sweep_b(gc_B, 4);
      break;
    case _retry_c:
    case _retry_userc:
      {
	register CELL_PTR saved_reg;
	
	/* for each extra saved register */
	for (saved_reg = &(gc_B->cp_a1)+rtp->u.lds.s;
	     saved_reg < &(gc_B->cp_a1)+rtp->u.lds.s+rtp->u.lds.extra;
	     saved_reg++) {
	  CELL cp_cell = *saved_reg;
	  if (MARKED_PTR(saved_reg)) {
	    UNMARK(saved_reg);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(saved_reg, GET_NEXT(cp_cell));
	    }
	  }
	}
      }
      /* continue to clean environments and arguments */
    default:
      sweep_b(gc_B,rtp->u.ld.s);
    }

    /* link to prev choicepoint */
    gc_B = gc_B->cp_b;
  }
}




/* update a relocation chain to point all its cells to new location of object */
static void
update_relocation_chain(CELL_PTR current, CELL_PTR dest)
{
  CELL_PTR        next;
  CELL            ccur = *current;

#if GC_NO_TAGS
  int rmarked = RMARKED(current);

  UNRMARK(current);
  while (rmarked) {
    CELL             current_tag;
    next = GET_NEXT(ccur);
    current_tag = TAG(ccur);
    ccur = *next;
    rmarked = RMARKED(next);
    UNRMARK(next);
    *next = (CELL) dest | current_tag;
  }
  *current = ccur;
#elif TAGS_FAST_OPS
  while (RMARKED(current)) {
    register CELL cnext;

    next = GET_NEXT(ccur);
    cnext = *next;
    
    if (IsVarTerm(ccur)) {
      ccur = *current = (MARKED_VAR(ccur) ?
			 ENSURE_MARKED(cnext) : 
			 UNMARKED(cnext) );
      *next = (MARKED(cnext) ? MBIT : 0) | (Int) dest;
    } else if (IsPairTerm(ccur)) {
      ccur = *current = (MARKED_COMP(ccur) ?
			 ENSURE_MARKED(cnext) :
			 UNMARKED(cnext) );
      *next = AbsPair((CELL *)
		      ((MARKED(cnext) ? MBIT : 0) |
		       (Int) dest));
    } else if (IsApplTerm(ccur)) {
      ccur = *current = (MARKED_COMP(ccur) ?
			 ENSURE_MARKED(cnext) : 
			 UNMARKED(cnext) );
      *next = AbsAppl((CELL *)
		      ((MARKED(cnext) ? MBIT : 0) |
		       (Int) dest));	  
    }
#ifdef DEBUG
    else {
      Yap_Error(SYSTEM_ERROR, TermNil, "ATOMIC in a GC relocation chain");
    }
#endif
  }
#else /* !TAGS_FAST_OPS */
  while (RMARKED(current)) {
    CELL             current_tag;
    next = GET_NEXT(ccur);
    current_tag = TAG(ccur);
    ccur = *current = (ccur & MBIT) | (*next & ~MBIT);
#if INVERT_RBIT
    *next = (*next & MBIT) | (CELL) dest | current_tag | RBIT;
#else
    *next = (*next & MBIT) | (CELL) dest | current_tag;
#endif
  }
#endif
}

static inline choiceptr
update_B_H( choiceptr gc_B, CELL *current, CELL *dest, CELL *odest
#ifdef TABLING
	    , dep_fr_ptr *depfrp
#endif
	    ) {
  /* also make the value of H in a choicepoint
     coherent with the new global
     */
#ifdef TABLING
  dep_fr_ptr depfr = *depfrp;
#endif

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

static inline CELL *
set_next_hb(choiceptr gc_B)
{
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
static void 
compact_heap(void)
{
  CELL_PTR        dest, current, next;
#ifdef DEBUG
  Int             found_marked = 0;
#endif /* DEBUG */
  choiceptr        gc_B = B;
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
  if (depfr != NULL && gc_B >= DepFr_cons_cp(depfr)) {
    gc_B = DepFr_cons_cp(depfr);
    depfr = DepFr_next(depfr);
  }
#endif
  next_hb = set_next_hb(gc_B);
  dest = (CELL_PTR) H0 + total_marked - 1;

  for (current = H - 1; current >= start_from; current--) {
    if (MARKED_PTR(current)) {
      CELL ccell = UNMARK_CELL(*current);
      if (
	  IN_BETWEEN(EndSpecials, ccell, MAX_SPECIALS_TAG) /* two first pages */
	  && IsVarTerm(ccell)
	  ) {
	/* oops, we found a blob */
	int nofcells = (UNMARK_CELL(*current)-EndSpecials) / sizeof(CELL);
	CELL *ptr = current - nofcells ;

	if (MARKED_PTR(ptr)) {
#ifdef DEBUG
	  found_marked+=nofcells;
#endif /* DEBUG */
	  if (current <= next_hb) {
	    gc_B = update_B_H(gc_B, current, dest, dest+1
#ifdef TABLING
			      , &depfr
#endif
			      );
	    next_hb = set_next_hb(gc_B);
	  }
	  /* this one's being used */
	  /* first swap the tag so that it will be seen by the next step */
	  {
	    CELL tmp = current[0];
	    current[0] = ptr[1];
#if GC_NO_TAGS
	    MARK(ptr+1);
#endif
	    ptr[1] = tmp;
	  }
	  if (in_garbage > 0) {
	    current[1] = in_garbage;
	    in_garbage = 0;
	  }
	  dest -= nofcells;
	  current = ptr;
	  /* process the functor next */
	} else {
	  /* skip the term */
	  in_garbage += nofcells+1;
	  current = ptr;
	  continue;
	}
      } else{
	if (current <= next_hb) {
	  gc_B = update_B_H(gc_B, current, dest, dest+1
#ifdef TABLING
				, &depfr
#endif
			    );
	  next_hb = set_next_hb(gc_B);
	}
      }
      if (in_garbage > 0) {
	current[1] = in_garbage;
	in_garbage = 0;
      }
#ifdef DEBUG
      found_marked++;
#endif /* DEBUG */
      update_relocation_chain(current, dest);
      if (HEAP_PTR(*current)) {
	next = GET_NEXT(*current);
	if (next < current)	/* push into reloc.
				 * chain */
	  into_relocation_chain(current, next);
	else if (current == next)	{ /* cell pointing to
					 * itself */
#if GC_NO_TAGS
	  UNRMARK(current);
	  *current = (CELL) dest;	/* no tag */
#else
	  *current = (*current & MBIT) | (CELL) dest;	/* no tag */
#endif
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
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"%% Upward (%d): %ld total against %ld found\n",
	       GcCalls,
	       (unsigned long int)total_marked,
	       (unsigned long int)found_marked);
  found_marked = start_from-H0;
#endif


  /*
   * downward phase - scan heap from low to high, moving marked objects
   * to their new locations & setting downward pointers to pt to new
   * locations 
   */

  dest = (CELL_PTR) start_from;
  for (current = start_from; current < H; current++) {
    CELL ccur = *current;
    if (MARKED_PTR(current)) {
      CELL uccur = UNMARK_CELL(ccur);
      if (
	  uccur < MAX_SPECIALS_TAG &&  /* two first pages */
	  uccur > EndSpecials && IsVarTerm(uccur)) {
	/* oops, we found a blob */
	int nofcells = (uccur-EndSpecials) / sizeof(CELL) , i;

	*dest++ = current[nofcells-1];
	current ++;
	for (i = 0; i < nofcells-2; i++) {
	  *dest++ = *current++;
	}
	*dest++ = ccur;
#ifdef DEBUG
	found_marked += nofcells;
#endif
	continue;
      }
#ifdef DEBUG
      found_marked++;
#endif
      update_relocation_chain(current, dest);
      ccur = *current;
      next = GET_NEXT(ccur);
      if (HEAP_PTR(ccur) &&
	  (next = GET_NEXT(ccur)) < H && /* move current cell &
				 * push */
	  next > current) {	/* into relocation chain  */
	*dest = ccur;
	into_relocation_chain(dest, next);
	UNMARK(dest);
      } else {
	/* just move current cell */
	*dest = ccur = UNMARK_CELL(ccur);
      }
      /* next cell, please */
      dest++;
    } else {
      current += (ccur-1);
    }
  }
#ifdef DEBUG
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"%% Downward (%d): %ld total against %ld found\n",
	       GcCalls,
	       (unsigned long int)total_marked,
	       (unsigned long int)found_marked);
#endif

  H = dest;		/* reset H */
  HB = B->cp_h;
#ifdef TABLING
  if (B_FZ == (choiceptr)LCL0)
    H_FZ = H0;
  else
    H_FZ = B_FZ->cp_h;
#endif /* TABLING */

}

#ifdef HYBRID_SCHEME
static void
adjust_cp_hbs(void)
{
#ifdef TABLING
  dep_fr_ptr depfr = LOCAL_top_dep_fr;
#endif /* TABLING */
  choiceptr gc_B = B;
  CELL_PTR *top = iptop-1, *base = (CELL_PTR *)H;

#ifdef TABLING
  if (depfr != NULL && gc_B >= DepFr_cons_cp(depfr)) {
    gc_B = DepFr_cons_cp(depfr);
    depfr = DepFr_next(depfr);
  }
#endif
  while (gc_B != NULL) {
    CELL *gc_H = gc_B->cp_h;
    CELL_PTR *nbase = base;

#ifdef TABLING
    if (depfr && gc_B >= DepFr_cons_cp(depfr)) {
      gc_B = DepFr_cons_cp(depfr);
      depfr = DepFr_next(depfr);
      continue;
    }
#endif /* TABLING */
    if (top[0] <= gc_H) {
      if (top[0] == gc_H) {
	gc_B->cp_h = H0+(top-base);
      } else {
	gc_B->cp_h = H0+((top+1)-base);
      }
    } else while (TRUE) {
      CELL_PTR *nxt = nbase+(top-nbase)/2;

      if (nxt[0] > gc_H) {
	if (nbase == top) {
	  if (nbase == base) {
	    gc_B->cp_h = H0;
	    break;
	  } else {
	    Yap_Error(SYSTEM_ERROR,TermNil,"Bug in Garbage collector");
	    return;
	  }
	}
	top = nxt;
      } else if (nxt[0] < gc_H && nxt[1] < gc_H) {
	nbase = nxt+1;
      } else if (nxt[0] == gc_H) {
	  gc_B->cp_h = H0+(nxt-base);
	  top = nxt;
	  break;
      } else {
	  gc_B->cp_h = H0+((nxt-base)+1);
	  top = nxt+1;
	  break;
      } 
    }
    gc_B = gc_B->cp_b;
  }
}


/*
 * move marked objects on the heap upwards over unmarked objects, and reset
 * all pointers to point to new locations 
 */
static void 
icompact_heap(void)
{
  CELL_PTR *iptr, *ibase = (CELL_PTR *)H;
#ifdef DEBUG
  Int             found_marked = 0;
#endif /* DEBUG */

  /*
   * upward phase - scan heap from high to low, setting marked upward
   * ptrs to point to what will be the new locations of the
   * objects pointed to  
   */

  for (iptr = iptop - 1; iptr >= ibase; iptr--) {
    CELL ccell;
    CELL_PTR        current;

    current = *iptr;
    ccell = UNMARK_CELL(*current);
    if (ccell < MAX_SPECIALS_TAG &&  /* two first pages */
	ccell > EndSpecials && IsVarTerm(ccell)
	) {
      /* oops, we found a blob */
      int nofcells = (UNMARK_CELL(*current)-EndSpecials) / sizeof(CELL);
      CELL *ptr = current - nofcells ;

      iptr -= nofcells;
#ifdef DEBUG
      found_marked+=nofcells;
#endif /* DEBUG */
      /* this one's being used */
      /* first swap the tag so that it will be seen by the next step */
      {
	CELL tmp = current[0];
	current[0] = ptr[1];
#if GC_NO_TAGS
	MARK(ptr+1);
#endif
	ptr[1] = tmp;
      }
      current = ptr;
    }
#ifdef DEBUG
    found_marked++;
#endif /* DEBUG */
    update_relocation_chain(current, H0+(iptr-ibase));
    if (HEAP_PTR(*current)) {
      CELL_PTR next;
      next = GET_NEXT(*current);
      if (next < current)	/* push into reloc.
				 * chain */
	into_relocation_chain(current, next);
      else if (current == next)	{ /* cell pointing to
				   * itself */
#if GC_NO_TAGS
	*current = (CELL) (H0+(iptr-ibase));	/* no tag */
#else
	*current = (*current & MBIT) | (CELL) (H0+(iptr-ibase));	/* no tag */
#endif
      }
    }
  }

#ifdef DEBUG
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"%% Upward (%d): %ld total against %ld found\n",
	       GcCalls,
	       (unsigned long int)total_marked,
	       (unsigned long int)found_marked);
  found_marked = 0;
#endif


  /*
   * downward phase - scan heap from low to high, moving marked objects
   * to their new locations & setting downward pointers to pt to new
   * locations 
   */

  for (iptr = ibase; iptr < iptop; iptr++) {
    CELL_PTR next;
    CELL *current = *iptr;
    CELL ccur = *current;
    CELL_PTR dest = H0+(iptr-ibase);
    CELL uccur = UNMARK_CELL(ccur);
    if (uccur < MAX_SPECIALS_TAG && uccur > EndSpecials && IsVarTerm(uccur)) {
      /* oops, we found a blob */
      int nofcells = (uccur-EndSpecials) / sizeof(CELL) , i;

      *dest++ = current[nofcells-1];
      current ++;
      for (i = 0; i < nofcells-2; i++) {
	*dest++ = *current++;
      }
      *dest = ccur;
      iptr += nofcells-1;
#ifdef DEBUG
      found_marked += nofcells;
#endif
      continue;
    }
#ifdef DEBUG
    found_marked++;
#endif
    update_relocation_chain(current, dest);
    ccur = *current;
    next = GET_NEXT(ccur);
    if (HEAP_PTR(ccur) &&	/* move current cell &
				 * push */
	next > current) {	/* into relocation chain  */
      *dest = ccur;
      into_relocation_chain(dest, next);
      UNMARK(dest);
    } else {
      /* just move current cell */
      *dest = ccur = UNMARK_CELL(ccur);
    }
  }
#ifdef DEBUG
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"%% Downward (%d): %ld total against %ld found\n",
	       GcCalls,
	       (unsigned long int)total_marked,
	       (unsigned long int)found_marked);
#endif

  H = H0+(iptop-ibase);		/* reset H */
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
static void
set_conditionals(tr_fr_ptr sTR) {
  while (sTR != sTR0) {
    CELL *cptr;
    sTR -= 2;
    cptr = (CELL *)TrailTerm(sTR+1);
    *cptr = TrailTerm(sTR);
  } 
}
#endif


/*
 * mark all objects on the heap that are accessible from active registers,
 * the trail, environments, and choicepoints 
 */

static void 
marking_phase(tr_fr_ptr old_TR, CELL *current_env, yamop *curp, CELL *max)
{

#ifdef EASY_SHUNTING
  current_B = B;
#endif
  init_dbtable(old_TR);
#ifdef EASY_SHUNTING
  sTR0 = (tr_fr_ptr)db_vec;
  sTR = (tr_fr_ptr)db_vec;
#else
  cont_top0 = (cont *)db_vec;
#endif
  cont_top = (cont *)db_vec;
  /* These two must be marked first so that our trail optimisation won't lose
     values */
  mark_regs(old_TR);		/* active registers & trail */
#ifdef COROUTINING
  mark_delays(max);
#endif
  /* active environments */
  mark_environments(current_env, EnvSize(curp), EnvBMap((CELL *)curp));
  mark_choicepoints(B, old_TR, is_gc_very_verbose());	/* choicepoints, and environs  */
#ifdef EASY_SHUNTING
  set_conditionals(sTR);
#endif
}

static void
sweep_oldgen(CELL *max, CELL *base)
{
  CELL *ptr = base;
  long int nof = 0;
  while (ptr < max) {
    if (MARKED_PTR(ptr)) {
      nof++;
      UNMARK(ptr);
      if (HEAP_PTR(*ptr)) {
	into_relocation_chain(ptr, GET_NEXT(*ptr));
      }
    }
    ptr++;
  }
  /* fprintf(stderr,"found %d, %p-%p\n", nof, base, max); */
}

#ifdef COROUTINING
static void
sweep_delays(CELL *max)
{
  CELL *ptr = (CELL *)Yap_GlobalBase;
  while (ptr < max) {
    if (MARKED_PTR(ptr)) {
      UNMARK(ptr);
      if (HEAP_PTR(*ptr)) {
	into_relocation_chain(ptr, GET_NEXT(*ptr));
      }
    }
    ptr++;
  }
}
#endif


/*
 * move marked heap objects upwards over unmarked objects, and reset all
 * pointers to point to new locations 
 */

static void 
compaction_phase(tr_fr_ptr old_TR, CELL *current_env, yamop *curp, CELL *max)
{
  CELL *CurrentH0 = NULL;

  int icompact = (iptop < (CELL_PTR *)ASP && 10*total_marked < H-H0);

  if (icompact) {
    /* we are going to reuse the total space */
    if (HGEN != H0) {
      /* undo optimisation */
      total_marked += total_oldies;
    }
  } else {
    if (HGEN != H0) {
      CurrentH0 = H0;
      H0 = HGEN;
      sweep_oldgen(HGEN, CurrentH0);
    }
  }
#ifdef COROUTINING
  sweep_delays(max);
#endif
  sweep_environments(current_env, EnvSize(curp), EnvBMap((CELL *)curp));
  sweep_choicepoints(B);
  sweep_trail(B, old_TR);
#ifdef HYBRID_SCHEME
  if (icompact) {
#ifdef DEBUG
    if (total_marked
#ifdef COROUTINING
	-total_smarked
#endif
	!= iptop-(CELL_PTR *)H && iptop < (CELL_PTR *)ASP -1024)
      fprintf(Yap_stderr,"%% Oops on iptop-H (%ld) vs %ld\n", (unsigned long int)(iptop-(CELL_PTR *)H), total_marked);
#endif
#if DEBUGX
    int effectiveness = (((H-H0)-total_marked)*100)/(H-H0);
    fprintf(Yap_stderr,"%% using pointers (%d)\n", effectiveness);
#endif
    if (CurrentH0) {
      H0 = CurrentH0;
      HGEN = H0;
      total_marked += total_oldies;
      CurrentH0 = NULL; 
    }
    quicksort((CELL_PTR *)H, 0, (iptop-(CELL_PTR *)H)-1);
    adjust_cp_hbs();
    icompact_heap();
  } else
#endif /* HYBRID_SCHEME */
    {
#ifdef DEBUG
      /*
#ifdef HYBRID_SCHEME
      int effectiveness = (((H-H0)-total_marked)*100)/(H-H0);
      fprintf(stderr,"%% not using pointers (%d) ASP: %p, ip %p (expected %p) \n", effectiveness, ASP, iptop, H+total_marked);

#endif
      */
#endif
      compact_heap();
    }
  if (CurrentH0) {
    H0 = CurrentH0;
  }
}

static Int
do_gc(Int predarity, CELL *current_env, yamop *nextop)
{
  Int		heap_cells;
  int		gc_verbose;
  tr_fr_ptr     old_TR = NULL;
  UInt		m_time, c_time, time_start, gc_time;
  CELL *max;
  Int           effectiveness, tot;
  int           gc_trace;

  if (setjmp(Yap_gc_restore) == 2) {
    /* we cannot recover, fail system */
    Yap_Error(OUT_OF_TRAIL_ERROR,TermNil,"could not expand trail during garbage collection");
  }
  heap_cells = H-H0;
  gc_verbose = is_gc_verbose();
  effectiveness = 0;
  gc_trace = FALSE;
#if COROUTINING
  max = (CELL *)DelayTop();
  while (H0 - max < 1024+(2*NUM_OF_ATTS)) {
    if (!Yap_growglobal(&current_env)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return 0;
    }
    max = (CELL *)DelayTop();
  }
#else
  max = NULL;
#endif
#ifdef INSTRUMENT_GC
  {
    int i;
    for (i=0; i<16; i++)
      chain[i]=0;
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
  if (Yap_GetValue(AtomGcTrace) != TermNil)
    gc_trace = 1;
#if !GC_NO_TAGS
  /* sanity check: can we still do garbage_collection ? */
  if ((CELL)Yap_TrailTop & (MBIT|RBIT)) {
    /* oops, we can't */
    if (gc_verbose) {
      fprintf(Yap_stderr, "%% TrailTop at %p clashes with gc bits: %lx\n", Yap_TrailTop, (unsigned long int)(MBIT|RBIT));
      fprintf(Yap_stderr, "%% garbage collection disallowed\n");
    }
    return(0);
  }
#endif
  if (gc_trace) {
    fprintf(Yap_stderr, "%% gc\n");
  } else if (gc_verbose) {
    fprintf(Yap_stderr, "%% Start of garbage collection %d:\n", GcCalls);
#ifndef EARLY_RESET
    fprintf(Yap_stderr, "%% no early reset in trail\n");
#endif
    fprintf(Yap_stderr, "%%       Global: %8ld cells (%p-%p)\n", (long int)heap_cells,H0,H);
    fprintf(Yap_stderr, "%%       Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    fprintf(Yap_stderr, "%%       Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)Yap_TrailBase),Yap_TrailBase,TR);
  }
#if !USE_SYSTEM_MALLOC
  if (HeapTop >= Yap_GlobalBase - MinHeapGap) {
    *--ASP = (CELL)current_env;
    if (!Yap_growheap(FALSE, MinHeapGap, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return FALSE;
    }
    current_env = (CELL *)*ASP;
    ASP++;
#if COROUTINING
    max = (CELL *)DelayTop();
#endif
  }
#endif
  time_start = Yap_cputime();
  total_marked = 0;
  total_oldies = 0;
#ifdef COROUTING
  total_smarked = 0;
#endif
  discard_trail_entries = 0;
#if GC_NO_TAGS
  {
    UInt alloc_sz = (CELL *)Yap_TrailTop-(CELL*)Yap_GlobalBase;
    bp = Yap_PreAllocCodeSpace();
    while (bp+alloc_sz > (char *)AuxSp) {
      /* not enough space */
      *--ASP = (CELL)current_env;
      bp = (char *)Yap_ExpandPreAllocCodeSpace(alloc_sz, NULL);
      if (!bp)
	return 0;
      current_env = (CELL *)*ASP;
      ASP++;
#if COROUTINING
      max = (CELL *)DelayTop();
#endif
    }
    memset((void *)bp, 0, alloc_sz);
  }
#endif /* GC_NO_TAGS */
#ifdef HYBRID_SCHEME
  iptop = (CELL_PTR *)H;
#endif
  /* get the number of active registers */
  HGEN = H0+IntegerOfTerm(Yap_ReadTimedVar(GcGeneration));
  /* old HGEN are not very reliable, but still may have data to recover */
  if (HGEN < HB) {
    choiceptr b_ptr = B;
    /* cannot trust the data between HGEN and its current choice-point */
    while (b_ptr) {
      if (b_ptr->cp_h <= HGEN) {
	HGEN = b_ptr->cp_h;
	break;
      } else {
	b_ptr = b_ptr->cp_b;
      }
    }
    if (!b_ptr) HGEN = H0;
  }
  /*  fprintf(stderr,"HGEN is %ld, %p, %p/%p\n", IntegerOfTerm(Yap_ReadTimedVar(GcGeneration)), HGEN, H,H0);*/
  YAPEnterCriticalSection();
  OldTR = (tr_fr_ptr)(old_TR = TR);
  push_registers(predarity, nextop);
  marking_phase(old_TR, current_env, nextop, max);
  if (total_oldies > ((HGEN-H0)*8)/10) {
    total_marked -= total_oldies;
    tot = total_marked+(HGEN-H0);
  } else {
    HGEN = H0;
    tot = total_marked;
  }
  m_time = Yap_cputime();
  gc_time = m_time-time_start;
  if (heap_cells) {
    if (heap_cells > 1000000)
      effectiveness = (heap_cells-tot)/(heap_cells/100);
    else
      effectiveness = 100*(heap_cells-tot)/heap_cells;
  } else
    effectiveness = 0;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%   Mark: Marked %ld cells of %ld (efficiency: %ld%%) in %g sec\n",
	       (long int)tot, (long int)heap_cells, (long int)effectiveness, (double)(m_time-time_start)/1000);
    if (HGEN-H0)
      fprintf(Yap_stderr,"%%       previous generation has size %lu, with %lu (%lu%%) unmarked\n", (unsigned long)(HGEN-H0), (HGEN-H0)-total_oldies, 100*((HGEN-H0)-total_oldies)/(HGEN-H0));
#ifdef INSTRUMENT_GC
    {
      int i;
      for (i=0; i<16; i++) {
	if (chain[i]) {
	  fprintf(Yap_stderr, "%%     chain[%d]=%lu\n", i, chain[i]);
	}
      }
      put_type_info((unsigned long int)tot);
      fprintf(Yap_stderr,"%%  %lu/%ld before and %lu/%ld after\n", old_vars, (unsigned long int)(B->cp_h-H0), new_vars, (unsigned long int)(H-B->cp_h));
      fprintf(Yap_stderr,"%%  %ld choicepoints\n", num_bs);
    }
#endif
  }
  time_start = m_time;
  compaction_phase(old_TR, current_env, nextop, max);
  TR = old_TR;
  pop_registers(predarity, nextop);
  TR = new_TR;
  YAPLeaveCriticalSection();
  /*  fprintf(Yap_stderr,"NEW HGEN %ld (%ld)\n", H-H0, HGEN-H0);*/
  Yap_UpdateTimedVar(GcGeneration, MkIntegerTerm(H-H0));
  c_time = Yap_cputime();
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%   Compress: took %g sec\n", (double)(c_time-time_start)/1000);
  }
  gc_time += (c_time-time_start);
  TotGcTime += gc_time;
  TotGcRecovered += heap_cells-tot;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%% GC %d took %g sec, total of %g sec doing GC so far.\n", GcCalls, (double)gc_time/1000, (double)TotGcTime/1000);
    fprintf(Yap_stderr, "%%  Left %ld cells free in stacks.\n",
	       (unsigned long int)(ASP-H));
  }
  check_global();
  return effectiveness;
}

static int
is_gc_verbose(void)
{
#ifdef INSTRUMENT_GC
  /* always give info when we are debugging gc */
  return(TRUE);
#else
  return(Yap_GetValue(AtomGcVerbose) != TermNil ||
	 Yap_GetValue(AtomGcVeryVerbose) != TermNil);
#endif
}

int
Yap_is_gc_verbose(void)
{
  return is_gc_verbose();
}

static int
is_gc_very_verbose(void)
{
  return(Yap_GetValue(AtomGcVeryVerbose) != TermNil);
}

Int
Yap_total_gc_time(void)
{
  return(TotGcTime);
}

static Int
p_inform_gc(void)
{
  Term tn = MkIntegerTerm(TotGcTime);
  Term tt = MkIntegerTerm(GcCalls);
  Term ts = MkIntegerTerm((TotGcRecovered*sizeof(CELL)));
 
  return(Yap_unify(tn, ARG2) && Yap_unify(tt, ARG1) && Yap_unify(ts, ARG3));

}


static int
call_gc(UInt gc_lim, Int predarity, CELL *current_env, yamop *nextop)
{
  UInt   gc_margin = MinStackGap;
  Term   Tgc_margin;
  Int    effectiveness = 0;
  int    gc_on = FALSE;

#if defined(YAPOR) || defined(THREADS)
  if (NOfThreads != 1) {
    Yap_Error(SYSTEM_ERROR,TermNil,"cannot perform garbage collection: more than a worker/thread running");
    return(FALSE);
  }
#endif
  if (Yap_GetValue(AtomGc) != TermNil)
    gc_on = TRUE;
  if (IsIntegerTerm(Tgc_margin = Yap_GetValue(AtomGcMargin)) &&
      gc_margin > 0) {
    gc_margin = (UInt)IntegerOfTerm(Tgc_margin);
  } else {
    /* only go exponential for the first 8 calls */
    if (GcCalls < 8) 
      gc_margin <<= GcCalls;
    else {
      /* next grow linearly */
	 gc_margin <<= 8;
      /* don't do this: it forces the system to ask for ever more stack!!
	 gc_margin *= GcCalls;
      */
    }
  }
  if (gc_margin < gc_lim)
    gc_margin = gc_lim;
  GcCalls++;
  if (gc_on && !(Yap_PrologMode & InErrorMode) &&
      /* make sure there is a point in collecting the heap */
      H-H0 > (LCL0-ASP)/2) {
    effectiveness = do_gc(predarity, current_env, nextop);
    if (effectiveness > 90) {
      while (gc_margin < H-H0) 
	gc_margin <<= 1;
    }
  } else {
    effectiveness = 0;
  }
  /* expand the stack if effectiveness is less than 20 % */
  if (ASP - H < gc_margin/sizeof(CELL) ||
      effectiveness < 20) {
    return Yap_growstack(gc_margin);
  }
  /*
   * debug for(save_total=1; save_total<=N; ++save_total)
   * plwrite(XREGS[save_total],Yap_DebugPutc,0); 
   */
  return TRUE;
}

int 
Yap_gc(Int predarity, CELL *current_env, yamop *nextop)
{
  int res;
  Yap_PrologMode |= GCMode;
  res=call_gc(4096, predarity, current_env, nextop);
  Yap_PrologMode &= ~GCMode;
  return res;
}

int 
Yap_gcl(UInt gc_lim, Int predarity, CELL *current_env, yamop *nextop)
{
  return call_gc(gc_lim+CalculateStackGap()*sizeof(CELL), predarity, current_env, nextop);
}


static Int
p_gc(void)
{
  do_gc(0, ENV, P);
  return(TRUE);
}

void 
Yap_init_gc(void)
{
  Yap_InitCPred("$gc", 0, p_gc, HiddenPredFlag);
  Yap_InitCPred("$inform_gc", 3, p_inform_gc, HiddenPredFlag);
}

void
Yap_inc_mark_variable()
{
  total_marked++;
}
