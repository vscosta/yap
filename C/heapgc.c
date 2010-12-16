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

#if !defined(TABLING)
#define EASY_SHUNTING 1
#endif /* !TABLING */
#define HYBRID_SCHEME 1


/* global variables for garbage collection */

STATIC_PROTO(Int  p_inform_gc, (void));
STATIC_PROTO(Int  p_gc, (void));
STATIC_PROTO(void push_registers, (Int, yamop *));
STATIC_PROTO(void marking_phase, (tr_fr_ptr, CELL *, yamop *));
STATIC_PROTO(void compaction_phase, (tr_fr_ptr, CELL *, yamop *));
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
STATIC_PROTO(void  LeaveGCMode, (void));
#ifdef EASY_SHUNTING
STATIC_PROTO(void  set_conditionals, (tr_fr_ptr));
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
} *dbentry;

typedef struct RB_red_blk_node {
  CODEADDR key;
  CODEADDR lim;
  db_entry_type db_type;
  int in_use;
  int red; /* if red=0 then the node is black */
  struct RB_red_blk_node* left;
  struct RB_red_blk_node* right;
  struct RB_red_blk_node* parent;
} rb_red_blk_node;

#ifdef EASY_SHUNTING
#undef cont_top0
#define cont_top0 (cont *)sTR
#endif

#if !defined(YAPOR) && !defined(THREADS)
/* in a single gc */
static unsigned long int   total_marked, total_oldies;	/* number of heap objects marked */

#ifdef EASY_SHUNTING
static choiceptr current_B;

static tr_fr_ptr sTR, sTR0;

static CELL *prev_HB;
#endif

static tr_fr_ptr new_TR;

static CELL *HGEN;

char *Yap_bp;

static int discard_trail_entries = 0;

#ifdef HYBRID_SCHEME
static CELL_PTR *iptop;
#endif

#ifndef EASY_SHUNTING
static cont *cont_top0;
#endif
static cont *cont_top;

static gc_ma_hash_entry gc_ma_hash_table[GC_MAVARS_HASH_SIZE];

static gc_ma_hash_entry *gc_ma_h_top, *gc_ma_h_list;

static UInt gc_timestamp;    /* an unsigned int */

static ADDR  db_vec, db_vec0;

static rb_red_blk_node *db_root, *db_nil;

#endif /* !defined(YAPOR) && !defined(THREADS) */

/* support for hybrid garbage collection scheme */

static void
gc_growtrail(int committed, tr_fr_ptr begsTR, cont *old_cont_top0)
{
  UInt sz = Yap_TrailTop-(ADDR)OldTR;
  /* ask for double the size */
  sz = 2*sz;
  
  if (!Yap_growtrail(sz, TRUE)) {
#ifdef EASY_SHUNTING
    if (begsTR) {
      sTR = (tr_fr_ptr)old_cont_top0;
      while (begsTR != NULL) {
	tr_fr_ptr newsTR = (tr_fr_ptr)TrailTerm(begsTR);
	TrailTerm(sTR) = TrailTerm(begsTR+1);
	TrailTerm(sTR+1) = TrailTerm(begsTR+2);
	begsTR = newsTR;
	sTR += 2;
      } 
    }
    set_conditionals(sTR);
#endif
    /* could not find more trail */
    save_machine_regs();
    siglongjmp(Yap_gc_restore, 2);
  }
}

inline static void
PUSH_CONTINUATION(CELL *v, int nof) {
  cont *x;
  x = cont_top;
  x++;
  if ((ADDR)x > Yap_TrailTop-1024) {
    gc_growtrail(TRUE, NULL, NULL);
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
  if (iptop >= (CELL_PTR *)ASP || iptop == vp) return;
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

static inline unsigned int
GC_MAVAR_HASH(CELL *addr) {
#if SIZEOF_INT_P==8
  return((((unsigned int)((CELL)(addr)))>>3)%GC_MAVARS_HASH_SIZE);
#else
  return((((unsigned int)((CELL)(addr)))>>2)%GC_MAVARS_HASH_SIZE); 
#endif
}

static inline gc_ma_hash_entry *
GC_ALLOC_NEW_MASPACE(void)
{
  gc_ma_hash_entry *new = gc_ma_h_top;
  if ((char *)gc_ma_h_top > Yap_TrailTop-1024)
    gc_growtrail(FALSE, NULL, NULL);
  gc_ma_h_top++;
  cont_top = (cont *)gc_ma_h_top;
#ifdef EASY_SHUNTING
  sTR = sTR0 = (tr_fr_ptr)cont_top;
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
#if TABLING
    gc_ma_hash_table[i].loc = trp;
    gc_ma_hash_table[i].more = gc_ma_h_list;
    gc_ma_h_list = gc_ma_hash_table+i;
#endif /* TABLING */
    gc_ma_hash_table[i].next = NULL;
    return NULL;
  }
  nptr = gc_ma_hash_table+i;
  while (nptr) {
    optr = nptr;
    if (nptr->addr == addr) {
#if TABLING
      /*
	we're moving from oldest to more recent, so only a new entry
	has the correct new value
      */
      TrailVal(nptr->loc+1) = TrailVal(trp+1);
#endif /* TABLING */
      return nptr;
    }
    nptr = nptr->next;
  }
  nptr = GC_ALLOC_NEW_MASPACE();
  optr->next = nptr;
  nptr->addr = addr;
#if TABLING
  nptr->loc = trp;
  nptr->more = gc_ma_h_list;
#endif /* TABLING */
  nptr->next = NULL;
  gc_ma_h_list = nptr;
  return NULL;
}

static inline void
GC_NEW_MAHASH(gc_ma_hash_entry *top) {
  UInt time = ++gc_timestamp;

  gc_ma_h_list = NULL;
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

static void
check_pr_trail(tr_fr_ptr trp)
{
  if ((tr_fr_ptr)Yap_TrailTop-TR < 1024) {
    if (!Yap_growtrail(0, TRUE) || TRUE) {
      /* could not find more trail */
      save_machine_regs();
      siglongjmp(Yap_gc_restore, 2);
    }
  }
}

/* push the active registers onto the trail for inclusion during gc */

static void 
push_registers(Int num_regs, yamop *nextop)
{
  int             i;
  StaticArrayEntry *sal = StaticArrays;

  /* push array entries first */
  ArrayEntry *al = DynamicArrays;
  GlobalEntry *gl = GlobalVariables;
  TrailTerm(TR++) = GlobalArena;
  while (al) {
    check_pr_trail(TR);
    TrailTerm(TR++) = al->ValueOfVE;
    al = al->NextAE;
  }
  while (gl) {
    check_pr_trail(TR);
    TrailTerm(TR++) = gl->global;
    gl = gl->NextGE;
  }
  while (sal) {
    if (sal->ArrayType == array_of_nb_terms) {
      UInt arity = -sal->ArrayEArity, i;
      for (i=0; i < arity; i++) {
	Term tlive  = sal->ValueOfVE.lterms[i].tlive;
	if (!IsVarTerm(tlive) || !IsUnboundVar(&sal->ValueOfVE.lterms[i].tlive)) {
	  check_pr_trail(TR);
	  TrailTerm(TR++) = tlive;
	}
      }
    }
    sal = sal->NextAE;
  }
  check_pr_trail(TR);
  TrailTerm(TR) = GcGeneration;
  TR++;
  TrailTerm(TR) = GcPhase;
  TR++;
#ifdef COROUTINING
  TrailTerm(TR) = WokenGoals;
  TrailTerm(TR+1) = AttsMutableList;
  TR += 2;
#endif
  for (i = 1; i <= num_regs; i++) {
    check_pr_trail(TR);
    TrailTerm(TR++) = (CELL) XREGS[i];
  }
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
	  check_pr_trail(TR);
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
  GlobalEntry *gl = GlobalVariables;

  GlobalArena = TrailTerm(ptr++);
  while (al) {
    al->ValueOfVE = TrailTerm(ptr++);
    al = al->NextAE;
  }
  while (gl) {
    gl->global = TrailTerm(ptr++);
    gl = gl->NextGE;
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
  GcPhase = TrailTerm(ptr++);
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
  WokenGoals = TrailTerm(ptr++);
  AttsMutableList = TrailTerm(ptr++);
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


static rb_red_blk_node *
RBMalloc(UInt size)
{
  ADDR new = db_vec;

  db_vec += size; 
  if ((ADDR)db_vec > Yap_TrailTop-1024) {
    gc_growtrail(FALSE, NULL, NULL);
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
  x->in_use = FALSE;

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
static void
mark_ref_in_use(DBRef ref)
{
  rb_red_blk_node *el = find_ref_in_dbtable((CODEADDR)ref);
  el->in_use = TRUE;
}

static int
ref_in_use(DBRef ref)
{
  rb_red_blk_node *el = find_ref_in_dbtable((CODEADDR)ref);
  return el->in_use;
}

static void 
mark_db_fixed(CELL *ptr) {
  rb_red_blk_node *el;

  el = find_ref_in_dbtable((CODEADDR)ptr);
  if (el != db_nil) {
    el->in_use = TRUE;
  }
}

static void 
init_dbtable(tr_fr_ptr trail_ptr) {
  StaticClause *sc = DeadStaticClauses;
  MegaClause *mc = DeadMegaClauses;
  StaticIndex *si = DeadStaticIndices;

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
	  (ADDR) pt0 >= Yap_TrailBase && (ADDR) pt0 < Yap_TrailTop
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
  while (sc) {
    store_in_dbtable((CODEADDR)sc, (CODEADDR)sc+sc->ClSize, dcl_entry);
    sc = sc->ClNext;
  }
  while (si) {
    store_in_dbtable((CODEADDR)si, (CODEADDR)si+si->ClSize, dcl_entry);
    si = si->SiblingIndex;
  }
  while (mc) {
    store_in_dbtable((CODEADDR)mc, (CODEADDR)mc+mc->ClSize, dcl_entry);
    mc = mc->ClNext;
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
      if (ccell == EndSpecials) {
	/* oops, we found a blob */
	CELL *ptr = current-1;
	UInt nofcells;

	while (!MARKED_PTR(ptr)) ptr--;
	nofcells = current-ptr;
	current = ptr;
	ccurr = *current;
	/* process the functor next */
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
  char *local_bp = Yap_bp;

 begin:
  if (UNMARKED_MARK(current,local_bp)) {
    POP_CONTINUATION();
  }
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
    if (IN_BETWEEN(Yap_GlobalBase,current,H) && IsAttVar(current) && current==next) {
      if (next < H0) POP_CONTINUATION();
      if (!UNMARKED_MARK(next-1,local_bp)) {
	total_marked++;
	if (next-1 < HGEN) {
	  total_oldies++;
	}
	PUSH_POINTER(next-1);
      }
      PUSH_CONTINUATION(next+1,2);
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
	  if (next > current && current < prev_HB && current >= HB && next >= HB && next < prev_HB) {
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
	/* try to shorten chains if they go through the current CP */
      } else if (next > HB && 
		 IsVarTerm(cnext) &&
		 UNMARK_CELL(cnext) != (CELL)next &&
		 current < LCL0) {
	/* This step is possible because we clean up the trail */
	*current = UNMARK_CELL(cnext);
	UNMARK(current);
	if (current >= H0 && current < H ) {
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
      fprintf(Yap_stderr, "OOPS in GC: marking, current=%p, *current=" UInt_FORMAT " next=%p\n", current, ccur, next);
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
  } else if (IsAtomOrIntTerm(ccur)) {
#ifdef INSTRUMENT_GC
    if (IsAtomTerm(ccur))
      inc_vars_of_type(current,gc_atom);
    else 
      inc_vars_of_type(current, gc_int);
#endif
    POP_CONTINUATION();
  } else if (IsPairTerm(ccur)) {
#ifdef INSTRUMENT_GC
    inc_vars_of_type(current,gc_list);
#endif
    if (ONHEAP(next)) {
      /* speedup for strings */
      if (IsAtomOrIntTerm(*next)) {
	if (!UNMARKED_MARK(next,local_bp)) {
	  total_marked++;
	  if (next < HGEN) {
	    total_oldies++;
	  }
	  PUSH_POINTER(next);
	}
	current = next+1;
	goto begin;
      } else {
	PUSH_CONTINUATION(next+1,1);
	current = next;
	goto begin;
      }
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
	if ((tref->Flags & (ErasedMask|LogUpdMask)) == (ErasedMask|LogUpdMask)) {
	  *current = MkDBRefTerm((DBRef)LogDBErasedMarker);
	  MARK(current);
	} else {
	  mark_ref_in_use(tref);
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
	MARK(next+2);
	if (next < HGEN) {
	  total_oldies+=3;
	}
	total_marked += 3;
	PUSH_POINTER(next);
	PUSH_POINTER(next+2);
	POP_CONTINUATION();
      case (CELL)FunctorDouble:
	MARK(next);
	PUSH_POINTER(next);
	{
	  UInt sz = 1+SIZEOF_DOUBLE/SIZEOF_LONG_INT;
	  if (next < HGEN) {
	    total_oldies+= 1+sz;
	  }
	  total_marked += 1+sz;
	  PUSH_POINTER(next+sz);
	  MARK(next+sz);
	}
	POP_CONTINUATION();
      case (CELL)FunctorBigInt:
	{
	  UInt sz = (sizeof(MP_INT)+CellSize+
		     ((MP_INT *)(next+2))->_mp_alloc*sizeof(mp_limb_t))/CellSize;
	  MARK(next);
	  /* size is given by functor + friends */
	  if (next < HGEN)
	    total_oldies += 2+sz;
	  total_marked += 2+sz;
	  PUSH_POINTER(next);
	  sz++;
	  MARK(next+sz);
	  PUSH_POINTER(next+sz);
	}
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
    next++;
    /* speedup for leaves */
    while (arity && IsAtomOrIntTerm(*next)) {
      if (!UNMARKED_MARK(next,local_bp)) {
	total_marked++;
	if (next < HGEN) {
	  total_oldies++;
	}
	PUSH_POINTER(next);
      }
      next++;
      arity--;
    }
    if (!arity) POP_CONTINUATION();
    current = next;
    if (arity == 1)  goto begin;
    PUSH_CONTINUATION(current+1,arity-1);
    goto begin;
  }
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
      if ((tref->Flags & (LogUpdMask|ErasedMask)) == (LogUpdMask|ErasedMask)) {
	*ptr = MkDBRefTerm((DBRef)LogDBErasedMarker);
      } else {
	mark_ref_in_use(tref);
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
  for (trail_ptr = old_TR; trail_ptr < TR; trail_ptr++) {
    mark_external_reference(&TrailTerm(trail_ptr));
  }
}

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
      fprintf(Yap_stderr,"OOPS in GC: env size for %p is " UInt_FORMAT "\n", gc_ENV, (CELL)size);
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
	bmap = ((CELL)-1);
      }
      bmap = (Int)(((CELL)bmap) << currv);
    }

    for (saved_var = gc_ENV - size; saved_var < gc_ENV - EnvSizeInCells; saved_var++) {
      if (currv == sizeof(CELL)*8) {
	if (pvbmap) {
	  pvbmap--;
	  bmap = *pvbmap;
	} else {
	  bmap = ((CELL)-1);
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

    size = EnvSize((yamop *) (gc_ENV[E_CP]));	/* size = EnvSize(CP) */
    pvbmap = EnvBMap((yamop *) (gc_ENV[E_CP]));
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
  tr_fr_ptr OldsTR0 = sTR0;
#endif
#ifdef COROUTINING
  CELL *detatt = NULL;
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
	/* perform early reset */
	/* reset term to be a variable */
	RESET_VARIABLE(hp);
	discard_trail_entries++;
	RESET_VARIABLE(&TrailTerm(trail_base));
#ifdef FROZEN_STACKS
	RESET_VARIABLE(&TrailVal(trail_base));
#endif
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
	else {
	  /* This is a bit of a mess: when I find an attributed variable that was bound
	     nondeterministically, I know that after backtracking it will be back to be an unbound variable.
	     The ideal solution would be to unbind all variables. The current solution is to
	     remark it as an attributed variable */
	  if (IN_BETWEEN(Yap_GlobalBase,hp,H) && IsAttVar(hp) && !UNMARKED_MARK(hp-1,Yap_bp)) {
	    total_marked++;
	    PUSH_POINTER(hp-1);
	    if (hp-1 < HGEN) {
	      total_oldies++;
	    }
	    mark_variable(hp+1);
	    mark_variable(hp+2);
	  }
#ifdef FROZEN_STACKS
	  mark_external_reference(&TrailVal(trail_base));
#endif
	}
#ifdef EASY_SHUNTING
	if (hp < gc_H   && hp >= H0 && !MARKED_PTR(hp)) {
	  tr_fr_ptr nsTR = (tr_fr_ptr)cont_top0;
          CELL *cptr = (CELL *)trail_cell;

	  if ((ADDR)nsTR > Yap_TrailTop-1024) {
	    gc_growtrail(TRUE, begsTR, old_cont_top0);
	  }
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
      CELL *cptr = RepPair(trail_cell);
      if (IN_BETWEEN(Yap_GlobalBase,cptr,H) &&
	  IsAttVar(cptr)) {
	TrailTerm(trail_base) = (CELL)cptr;
	mark_external_reference(&TrailTerm(trail_base));
	TrailTerm(trail_base) = trail_cell;
      } 
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
      if (cptr < (CELL *)gc_B && cptr >= gc_H) {
	goto remove_trash_entry;
      } else if (IsAttVar(cptr)) {
	/* MABINDING that should be recovered */
	if (detatt && cptr < detatt) {
	  goto remove_trash_entry;
	} else {
	  /* This attributed variable is still in play */
	  mark_variable(cptr);
	}
      }
      if (!gc_lookup_ma_var(cptr, trail_base)) {
	/* check whether this is the first time we see it*/
	Term t0 = TrailTerm(trail_base+1);
     
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
	  mark_external_reference(&(TrailTerm(trail_base)));
	  /* reset the gc to believe the original tag */
	  TrailTerm(trail_base) = AbsAppl((CELL *)TrailTerm(trail_base));
	}
#ifdef FROZEN_STACKS
	mark_external_reference(&(TrailVal(trail_base)));
	trail_base++;
	if (HEAP_PTR(trail_cell)) {
	  TrailTerm(trail_base) = (CELL)cptr;
	  mark_external_reference(&(TrailTerm(trail_base)));
	  /* reset the gc to believe the original tag */
	  TrailTerm(trail_base) = AbsAppl((CELL *)TrailTerm(trail_base));
	}
	/* don't need to mark the next TrailVal, this is done at the end
	   of segment */
#else
	trail_base++;
	mark_external_reference(&(TrailTerm(trail_base)));
	trail_base ++;
	if (HEAP_PTR(trail_cell)) {
	  /* fool the gc into thinking this is a variable */
	  TrailTerm(trail_base) = (CELL)cptr;
	  mark_external_reference(&(TrailTerm(trail_base)));
	  /* reset the gc to believe the original tag */
	  TrailTerm(trail_base) = AbsAppl((CELL *)TrailTerm(trail_base));
	} 
#endif /* TABLING */
      } else {
      remove_trash_entry:
	/* we can safely ignore this little monster */
#ifdef FROZEN_STACKS
	discard_trail_entries += 2;
	RESET_VARIABLE(&TrailTerm(trail_base));
	RESET_VARIABLE(&TrailVal(trail_base));
#else
	discard_trail_entries += 3;
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
   gc_ma_hash_entry *gl = gc_ma_h_list;
   while (gl) {
     mark_external_reference(&(TrailVal(gl->loc+1)));
     gl = gl->more;
   }
 }
#endif /* TABLING */
#ifdef EASY_SHUNTING
  /* set back old variables */
  sTR = (tr_fr_ptr)old_cont_top0;
  while (begsTR != NULL) {
    tr_fr_ptr newsTR = (tr_fr_ptr)TrailTerm(begsTR);
    TrailTerm(sTR) = TrailTerm(begsTR+1);
    TrailTerm(sTR+1) = TrailTerm(begsTR+2);
    begsTR = newsTR;
    sTR += 2;
  } 
  sTR0 = OldsTR0;
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
mark_slots(void)
{
  Int curslot = CurSlot;
  while (curslot) {
    CELL *ptr = LCL0-curslot;
    Int ns = IntegerOfTerm(*ptr);

    ptr++;
    while (ns > 0) {
      //      Yap_DebugPlWrite(ptr);
      //fprintf(stderr,"\n");
      mark_external_reference(ptr);
      ptr++;
      ns--;
    }
    curslot = IntegerOfTerm(*ptr);
  }
}


#ifdef TABLING
static choiceptr
youngest_cp(choiceptr gc_B, dep_fr_ptr *depfrp)
{
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


static void 
mark_choicepoints(register choiceptr gc_B, tr_fr_ptr saved_TR, int very_verbose)
{
  OPCODE 
    trust_lu = Yap_opcode(_trust_logical),
    count_trust_lu = Yap_opcode(_count_trust_logical),
    profiled_trust_lu = Yap_opcode(_profiled_trust_logical);
  
  yamop *lu_cl0 = NEXTOP(PredLogUpdClause0->CodeOfPred,Otapl),
    *lu_cl = NEXTOP(PredLogUpdClause->CodeOfPred,Otapl),
    *lu_cle = NEXTOP(PredLogUpdClauseErase->CodeOfPred,Otapl),
    *su_cl = NEXTOP(PredStaticClause->CodeOfPred,Otapl);
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

    mark_db_fixed((CELL *)rtp);
#ifdef DETERMINISTIC_TABLING
    if (!IS_DET_GEN_CP(gc_B))
#endif /* DETERMINISTIC_TABLING */
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
      mark_trail(saved_TR, gc_B->cp_tr, gc_B->cp_h, gc_B);
      saved_TR = gc_B->cp_tr;
    }
    if (opnum == _or_else || opnum == _or_last) {
      /* ; choice point */
      mark_environments((CELL_PTR) (gc_B->cp_a1),
			-gc_B->cp_cp->u.Osblp.s / ((OPREG)sizeof(CELL)),
			gc_B->cp_cp->u.Osblp.bmap
			);
    } else {
      /* choicepoint with arguments */
      register CELL_PTR        saved_reg;
      OPREG nargs;
	  
      if (opnum == _Nstop)
	mark_environments((CELL_PTR) gc_B->cp_env,
			  EnvSizeInCells,
			  NULL);
      else if (opnum != _trust_fail)
#ifdef DETERMINISTIC_TABLING
	if (!IS_DET_GEN_CP(gc_B))
#endif /* DETERMINISTIC_TABLING */
	  mark_environments((CELL_PTR) gc_B->cp_env,
			    EnvSize((yamop *) (gc_B->cp_cp)),
			    EnvBMap((yamop *) (gc_B->cp_cp)));
      /* extended choice point */
    restart_cp:
      switch (opnum) {
      case _Nstop:
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
	  if (IsVarTerm((CELL)ref)) {
	    mark_ref_in_use(ref);
	  } else {
	    if (ONCODE((CELL)ref)) {
	      mark_db_fixed(RepAppl((CELL)ref));
	    }
	  }
	  B = old_b;
	}
	nargs = rtp->u.OtapFs.s+rtp->u.OtapFs.extra;
	break;
      case _jump:
	rtp = rtp->u.l.l;
	op = rtp->opc;
	opnum = Yap_op_from_opcode(op);
	goto restart_cp;
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
	  nargs = rtp->u.Otapl.s;
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
	      mark_external_reference(vars_ptr);
	      vars_ptr++;
	    }
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
      case _trie_retry_gterm:
	{
	  CELL *vars_ptr;
	  int heap_arity, vars_arity, subs_arity;
	  vars_ptr = (CELL *)(gc_B + 1);
	  heap_arity = vars_ptr[0];
	  vars_arity = vars_ptr[1 + heap_arity];
	  subs_arity = vars_ptr[2 + heap_arity + vars_arity];
	  vars_ptr += 2 + heap_arity + subs_arity + vars_arity;
	  if (subs_arity) {
	    while (subs_arity--) {	
	      mark_external_reference(vars_ptr);
	      vars_ptr--;
	    }
	  }
	  vars_ptr--;  /* skip subs_arity entry */
	  if (vars_arity) {
	    while (vars_arity--) {	
	      mark_external_reference(vars_ptr);
	      vars_ptr--;
	    }
	  }
	  vars_ptr--;  /* skip vars_arity entry */
	  if (heap_arity) {
	    while (heap_arity--) {	
	      if (*vars_ptr == 0)  /* double/longint extension mark */
		break;
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
	mark_ref_in_use((DBRef)ClauseCodeToDynamicClause(gc_B->cp_ap));
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
      case _retry_logical:
	{
	  /* find out who owns this sequence of try-retry-trust */
	  /* I don't like this code, it's a bad idea to do a linear scan,
	     on the other hand it's the only way we can be sure we can reclaim
	     space
	  */
	  yamop *end = rtp->u.OtaLl.n;
	  while (end->opc != trust_lu &&
		 end->opc != count_trust_lu &&
		 end->opc != profiled_trust_lu )
	    end = end->u.OtaLl.n;
	  mark_ref_in_use((DBRef)end->u.OtILl.block);
	}
	/* mark timestamp */
	nargs = rtp->u.OtaLl.s+1;
	break;
      case _count_retry_logical:
	{
	  /* find out who owns this sequence of try-retry-trust */
	  /* I don't like this code, it's a bad idea to do a linear scan,
	     on the other hand it's the only way we can be sure we can reclaim
	     space
	  */
	  yamop *end = rtp->u.OtaLl.n;
	  while (Yap_op_from_opcode(end->opc) != _count_trust_logical)
	    end = end->u.OtaLl.n;
	  mark_ref_in_use((DBRef)end->u.OtILl.block);
	}
	/* mark timestamp */
	nargs = rtp->u.OtaLl.s+1;
	break;
      case _profiled_retry_logical:
	{
	  /* find out who owns this sequence of try-retry-trust */
	  /* I don't like this code, it's a bad idea to do a linear scan,
	     on the other hand it's the only way we can be sure we can reclaim
	     space
	  */
	  yamop *end = rtp->u.OtaLl.n;
	  while (Yap_op_from_opcode(end->opc) != _profiled_trust_logical)
	    end = end->u.OtaLl.n;
	  mark_ref_in_use((DBRef)end->u.OtILl.block);
	}
	/* mark timestamp */
	nargs = rtp->u.OtaLl.s+1;
	break;
      case _trust_logical:
      case _count_trust_logical:
      case _profiled_trust_logical:
	/* mark timestamp */
	mark_ref_in_use((DBRef)rtp->u.OtILl.block);
	nargs = rtp->u.OtILl.d->ClPred->ArityOfPE+1;
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
	if (IN_BETWEEN(H0,(CELL *)(gc_B->cp_ap),H)) {
	  fprintf(stderr,"OOPS in GC: gc not supported in this case!!!\n");
	  exit(1);
	}
	nargs = rtp->u.Otapl.s;
	break;
      default:
	fprintf(Yap_stderr, "OOPS in GC: Unexpected opcode: %d\n", opnum);
	nargs = 0;
#else
      default:
	nargs = rtp->u.Otapl.s;
#endif
      }
	

      if (gc_B->cp_ap == lu_cl0 ||
	  gc_B->cp_ap == lu_cl ||
	  gc_B->cp_ap == lu_cle ||
	  gc_B->cp_ap == su_cl) {
	yamop *pt = (yamop *)IntegerOfTerm(gc_B->cp_args[1]);
	if (gc_B->cp_ap == su_cl) {
	  mark_db_fixed((CELL *)pt);
	} else {
	  while (pt->opc != trust_lu  &&
		 pt->opc != count_trust_lu &&
		 pt->opc != profiled_trust_lu
		 )
	    pt = pt->u.OtaLl.n;
	  mark_ref_in_use((DBRef)pt->u.OtILl.block);
	}
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

static inline void 
into_relocation_chain(CELL_PTR current, CELL_PTR next)
{
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
}


static void
CleanDeadClauses(void)
{
  {
    StaticClause **cptr;
    StaticClause *cl;

    cptr = &(DeadStaticClauses);
    cl = DeadStaticClauses;
    while (cl) {
      if (!ref_in_use((DBRef)cl)) {
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
      if (!ref_in_use((DBRef)cl)) {
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
      if (!ref_in_use((DBRef)cl)) {
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

static void 
sweep_trail(choiceptr gc_B, tr_fr_ptr old_TR)
{
  tr_fr_ptr     trail_ptr, dest;
  Int OldHeapUsed = HeapUsed;
#ifdef DEBUG
  Int hp_entrs = 0, hp_erased = 0, hp_not_in_use = 0,
    hp_in_use_erased = 0, code_entries = 0;
#endif

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
    source = dest = (tr_fr_ptr)Yap_TrailBase;
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
	}
#ifdef FROZEN_STACKS
	/* it is complex to recover cells with frozen segments */
	TrailVal(dest) = TrailVal(trail_ptr);
	if (MARKED_PTR(&TrailVal(dest))) {
	  if (HEAP_PTR(TrailVal(dest))) {
	    into_relocation_chain(&TrailVal(dest), GET_NEXT(TrailVal(dest)));
	  }
	}
#endif
      } else if (IsPairTerm(trail_cell)) {
	CELL *pt0 = RepPair(trail_cell);
	CELL flags;

	if (IN_BETWEEN(Yap_GlobalBase, pt0, H) && IsAttVar(pt0)) {
	  TrailTerm(dest) = trail_cell;
	  /* be careful with partial gc */
	  if (HEAP_PTR(TrailTerm(dest))) {
	    into_relocation_chain(&TrailTerm(dest), GET_NEXT(trail_cell));
	  }
	  dest++;
	  trail_ptr++;
	  continue;
	}
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
	if (!ref_in_use((DBRef)pt0)) {
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
      	if (!ref_in_use((DBRef)pt0)) {
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
#if  defined(YAPOR) || defined(THREADS)
		/*
		  gc may be called when executing a dynamic goal,
		  check PP to avoid deadlock
		*/
		PredEntry *ap = indx->ClPred;
		if (ap != PP)
		  PELOCK(85,ap);
#endif
		DEC_CLREF_COUNT(indx);
		indx->ClFlags &= ~InUseMask;
		erase = (indx->ClFlags & ErasedMask
			 && !indx->ClRefCount);
		if (erase) {
		  /* at this point, 
		     no one is accessing the clause */
		  Yap_ErLogUpdIndex(indx);
		}
#if  defined(YAPOR) || defined(THREADS)
		if (ap != PP)
		  UNLOCK(ap->PELock);
#endif
	      } else {
		LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt0);
#if  defined(YAPOR) || defined(THREADS)
		PredEntry *ap = cl->ClPred;
#endif
		int erase;

#if  defined(YAPOR) || defined(THREADS)
		if (ap != PP)
		  PELOCK(86,ap);
#endif
		DEC_CLREF_COUNT(cl);
		cl->ClFlags &= ~InUseMask;
		erase = ((cl->ClFlags & ErasedMask) && !cl->ClRefCount);
		if (erase) {
		  /* at this point, 
		     no one is accessing the clause */
		  Yap_ErLogUpdCl(cl);
		}
#if  defined(YAPOR) || defined(THREADS)
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
#ifdef FROZEN_STACKS
	  RESET_VARIABLE(&TrailVal(dest));
#endif
	  discard_trail_entries++;
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
	TrailVal(dest) = old;
	TrailVal(dest+1) = old1;
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
    if (OldHeapUsed) {
      fprintf(Yap_stderr,
	      "%%       Heap: recovered %ld bytes (%ld%%) out of %ld\n",
	      (unsigned long int)(OldHeapUsed-HeapUsed),
	      (unsigned long int)((OldHeapUsed-HeapUsed)/(OldHeapUsed/100)),
	      (unsigned long int)OldHeapUsed);
    }
  }
  CleanDeadClauses();
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
	bmap = ((CELL)-1);
      }
      bmap = (Int)(((CELL)bmap) << currv);
    }
    for (saved_var = gc_ENV - size; saved_var < gc_ENV - EnvSizeInCells; saved_var++) {
      
      if (currv == sizeof(CELL)*8) {
	if (pvbmap != NULL) {
	  pvbmap--;
	  bmap = *pvbmap;
	} else {
	  bmap = ((CELL)-1);
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

    size = EnvSize((yamop *) (gc_ENV[E_CP]));	/* size = EnvSize(CP) */
    pvbmap = EnvBMap((yamop *) (gc_ENV[E_CP]));
    gc_ENV = (CELL_PTR) gc_ENV[E_E];	/* link to prev
					 * environment */
  }
}

static void
sweep_slots(void)
{
  Int curslot = CurSlot;
  while (curslot) {
    CELL *ptr = LCL0-curslot;
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
    curslot = IntegerOfTerm(*ptr);
  }
}

static void
sweep_b(choiceptr gc_B, UInt arity)
{
  register CELL_PTR saved_reg;

  sweep_environments(gc_B->cp_env,
		     EnvSize((yamop *) (gc_B->cp_cp)),
		     EnvBMap((yamop *) (gc_B->cp_cp)));

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
      if (gc_B->cp_b != NULL) {
	break;
      } else
	return;
    case _trust_fail:
      break;
    case _or_else:
    case _or_last:

      sweep_environments((CELL_PTR)(gc_B->cp_a1),
			 -gc_B->cp_cp->u.Osblp.s / ((OPREG)sizeof(CELL)),
			 gc_B->cp_cp->u.Osblp.bmap
			 );
      break;
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
	sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp), EnvBMap(gc_B->cp_cp));
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
	sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp), EnvBMap(gc_B->cp_cp));
	vars_ptr = (CELL *)(GEN_CP(gc_B) + 1);
	nargs = rtp->u.Otapl.s;
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
      {
	int nargs;
	CELL *vars_ptr, vars;
#ifdef DETERMINISTIC_TABLING
	if (IS_DET_GEN_CP(gc_B))
	  vars_ptr = (CELL *)(DET_GEN_CP(gc_B) + 1);
	else
#endif /* DETERMINISTIC_TABLING */
	{
	  sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp), EnvBMap(gc_B->cp_cp));
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
	sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp), EnvBMap(gc_B->cp_cp));
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
    case _trie_retry_gterm:
      {
	CELL *vars_ptr;
	int heap_arity, vars_arity, subs_arity;
	sweep_environments(gc_B->cp_env, EnvSize(gc_B->cp_cp), EnvBMap(gc_B->cp_cp));
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
		into_relocation_chain(vars_ptr, GET_NEXT(cp_cell));
	      }
	    }
	    vars_ptr--;
	  }
	}
	vars_ptr--;  /* skip subs_arity entry */
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
	vars_ptr--;  /* skip vars_arity entry */
	if (heap_arity) {
	  while (heap_arity--) {
	    CELL cp_cell = *vars_ptr;
	    if (*vars_ptr == 0)  /* double/longint extension mark */
	      break;
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
    case _try_logical:
    case _retry_logical:
    case _count_retry_logical:
    case _profiled_retry_logical:
	/* sweep timestamp */
      sweep_b(gc_B, rtp->u.OtaLl.s+1);
      break;
    case _trust_logical:
    case _count_trust_logical:
    case _profiled_trust_logical:
      sweep_b(gc_B, rtp->u.OtILl.d->ClPred->ArityOfPE+1);
      break;
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
	for (saved_reg = &(gc_B->cp_a1)+rtp->u.OtapFs.s;
	     saved_reg < &(gc_B->cp_a1)+rtp->u.OtapFs.s+rtp->u.OtapFs.extra;
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
      sweep_b(gc_B,rtp->u.Otapl.s);
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
static void
update_relocation_chain(CELL_PTR current, CELL_PTR dest)
{
  CELL_PTR        next;
  CELL            ccur = *current;

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
}

static inline choiceptr
update_B_H( choiceptr gc_B, CELL *current, CELL *dest, CELL *odest
#ifdef TABLING
	    , dep_fr_ptr *depfrp
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
#endif /* TABLING */
  next_hb = set_next_hb(gc_B);
  dest = H0 + total_marked - 1;

  gc_B = update_B_H(gc_B, H, dest+1, dest+2
#ifdef TABLING
		    , &depfr
#endif /* TABLING */
		    );
  for (current = H - 1; current >= start_from; current--) {
    if (MARKED_PTR(current)) {
      CELL ccell = UNMARK_CELL(*current);

      if (in_garbage > 0) {
	current[1] = in_garbage;
	in_garbage = 0;
      }

      if (current <= next_hb) {
	gc_B = update_B_H(gc_B, current, dest, dest+1
#ifdef TABLING
			  , &depfr
#endif /* TABLING */
			  );
	next_hb = set_next_hb(gc_B);
      }

      if (ccell == EndSpecials) {
	/* oops, we found a blob */
	CELL *ptr = current-1;
	UInt nofcells;

	while (!MARKED_PTR(ptr)) ptr--;
	nofcells = current-ptr;
	ptr++;
	MARK(ptr);
#ifdef DEBUG
	found_marked+=nofcells;
#endif
	/* first swap the tag so that it will be seen by the next step */
	current[0] = ptr[0];
	ptr[0] = EndSpecials;
	dest -= nofcells;
	current = ptr;
	continue;
	/* process the functor on a separate cycle */
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
	  UNRMARK(current);
	  *current = (CELL) dest;	/* no tag */
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
  if (dest != start_from-1)
    fprintf(Yap_stderr,"%% Bad Dest (%lu): %p should be %p\n",
	    (unsigned long int)GcCalls,
	    dest,
	    start_from);
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"%% Upward (%lu): %lu total against %lu found\n",
	    (unsigned long int)GcCalls,
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
      if (uccur == EndSpecials) {
	CELL *old_dest = dest;

	dest++;
	current++;
	while (!MARKED_PTR(current)) {
	  *dest++ = *current++;
	}
	*old_dest = *current;
	*dest++ = EndSpecials;
#ifdef DEBUG
	found_marked += (dest-old_dest);
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
    fprintf(Yap_stderr,"%% Downward (%lu): %lu total against %lu found\n",
	    (unsigned long int)GcCalls,
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
/*
 * move marked objects on the heap upwards over unmarked objects, and reset
 * all pointers to point to new locations 
 */
static void 
icompact_heap(void)
{
  CELL_PTR *iptr, *ibase = (CELL_PTR *)H;
  CELL_PTR dest;
  CELL *next_hb;
#ifdef DEBUG
  Int             found_marked = 0;
#endif /* DEBUG */
#ifdef TABLING
  dep_fr_ptr depfr = LOCAL_top_dep_fr;
#endif /* TABLING */
  choiceptr        gc_B = B;

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
#endif /* TABLING */
  next_hb = set_next_hb(gc_B);
  dest = (CELL_PTR) H0 + total_marked - 1;
  gc_B = update_B_H(gc_B, H, dest+1, dest+2
#ifdef TABLING
		    , &depfr
#endif /* TABLING */
		    );
  for (iptr = iptop - 1; iptr >= ibase; iptr--) {
    CELL ccell;
    CELL_PTR        current;

    current = *iptr;
    ccell = UNMARK_CELL(*current);
    if (current <= next_hb) {
      gc_B = update_B_H(gc_B, current, dest, dest+1
#ifdef TABLING
			, &depfr
#endif /* TABLING */
			);
      next_hb = set_next_hb(gc_B);
    }
    if (ccell == EndSpecials) {
      /* oops, we found a blob */
      CELL_PTR ptr;
      UInt nofcells;

      /* use the first cell after the functor for all our dirty tricks  */
      ptr = iptr[-1]+1;
      nofcells = current-ptr;
#ifdef DEBUG
      found_marked+=(nofcells+1);
#endif /* DEBUG */
      dest -= nofcells+1;
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
    update_relocation_chain(current, dest);
    if (HEAP_PTR(*current)) {
      CELL_PTR next;
      next = GET_NEXT(*current);
      if (next < current)	/* push into reloc.
				 * chain */
	into_relocation_chain(current, next);
      else if (current == next)	{ /* cell pointing to
				   * itself */
	UNRMARK(current);
	*current = (CELL) dest;	/* no tag */
      }
    }
    dest--;
  }

#ifdef DEBUG
  if (dest != H0-1)
    fprintf(Yap_stderr,"%% Bad Dest (%lu): %p should be %p\n",
	    (unsigned long int)GcCalls,
	    dest,
	    H0-1);
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"%% Upward (%lu): %lu total against %lu found\n",
	    (unsigned long int)GcCalls,
	    (unsigned long int)total_marked,
	    (unsigned long int)found_marked);
  found_marked = 0;
#endif


  /*
   * downward phase - scan heap from low to high, moving marked objects
   * to their new locations & setting downward pointers to pt to new
   * locations 
   */

  dest = H0;
  for (iptr = ibase; iptr < iptop; iptr++) {
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
      found_marked += dest-old_dest;
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
      dest++;
    } else {
      /* just move current cell */
      *dest++ = ccur = UNMARK_CELL(ccur);
    }
  }
#ifdef DEBUG
  if (H0+total_marked != dest)
    fprintf(Yap_stderr,"%% Downward (%lu): %p total against %p found\n",
	    (unsigned long int)GcCalls,
	    H0+total_marked,
	    dest);
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"%% Downward (%lu): %lu total against %lu found\n",
	    (unsigned long int)GcCalls,
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
#endif /* HYBRID_SCHEME */


#ifdef EASY_SHUNTING
static void
set_conditionals(tr_fr_ptr str) {
  while (str != sTR0) {
    CELL *cptr;
    str -= 2;
    cptr = (CELL *)TrailTerm(str+1);
    *cptr = TrailTerm(str);
  } 
  sTR = sTR0 = NULL;
}
#endif


/*
 * mark all objects on the heap that are accessible from active registers,
 * the trail, environments, and choicepoints 
 */

static void 
marking_phase(tr_fr_ptr old_TR, CELL *current_env, yamop *curp)
{

#ifdef EASY_SHUNTING
  current_B = B;
  prev_HB = H;
#endif
  init_dbtable(old_TR);
#ifdef EASY_SHUNTING
  sTR0 = (tr_fr_ptr)db_vec;
  sTR = (tr_fr_ptr)db_vec;
  /* make sure we set HB before we do any variable shunting!!! */
#else
  cont_top0 = (cont *)db_vec;
#endif
  cont_top = (cont *)db_vec;
  /* These two must be marked first so that our trail optimisation won't lose
     values */
  mark_slots();
  mark_regs(old_TR);		/* active registers & trail */
  /* active environments */
  mark_environments(current_env, EnvSize(curp), EnvBMap(curp));
  mark_choicepoints(B, old_TR, is_gc_very_verbose());	/* choicepoints, and environs  */
#ifdef EASY_SHUNTING
  set_conditionals(sTR);
#endif
}

static void
sweep_oldgen(CELL *max, CELL *base)
{
  CELL *ptr = base;
  char *bpb = Yap_bp+(base-(CELL*)Yap_GlobalBase);

  while (ptr < max) {
    if (*bpb) {
      if (HEAP_PTR(*ptr)) {
	into_relocation_chain(ptr, GET_NEXT(*ptr));
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

static void 
compaction_phase(tr_fr_ptr old_TR, CELL *current_env, yamop *curp)
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
  sweep_slots();
  sweep_environments(current_env, EnvSize(curp), EnvBMap(curp));
  sweep_choicepoints(B);
  sweep_trail(B, old_TR);
#ifdef HYBRID_SCHEME
  if (icompact) {
#ifdef DEBUG
    /*
    if (total_marked
#ifdef COROUTINING
	-total_smarked
#endif
	!= iptop-(CELL_PTR *)H && iptop < (CELL_PTR *)ASP -1024)
      fprintf(Yap_stderr,"%% Oops on iptop-H (%ld) vs %ld\n", (unsigned long int)(iptop-(CELL_PTR *)H), total_marked);
    */
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

static int
do_gc(Int predarity, CELL *current_env, yamop *nextop)
{
  Int		heap_cells;
  int		gc_verbose;
  volatile tr_fr_ptr     old_TR = NULL;
  UInt		m_time, c_time, time_start, gc_time;
  Int           effectiveness, tot;
  int           gc_trace;
  UInt		gc_phase;
  UInt		alloc_sz;
  heap_cells = H-H0;
  gc_verbose = is_gc_verbose();
  effectiveness = 0;
  gc_trace = FALSE;
  GcCalls++;
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
  if (gc_trace) {
    fprintf(Yap_stderr, "%% gc\n");
  } else if (gc_verbose) {
#if  defined(YAPOR) || defined(THREADS)
    fprintf(Yap_stderr, "%% Worker Id %d:\n", worker_id);
#endif
    fprintf(Yap_stderr, "%% Start of garbage collection %lu:\n", (unsigned long int)GcCalls);
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
      return -1;
    }
    current_env = (CELL *)*ASP;
    ASP++;
  }
#endif
  time_start = Yap_cputime();
  if (sigsetjmp(Yap_gc_restore, 0) == 2) {
    UInt sz;

    /* we cannot recover, fail system */
    restore_machine_regs();    
    sz = Yap_TrailTop-(ADDR)OldTR;
    /* ask for double the size */
    sz = 2*sz;
    TR = OldTR;
  
    *--ASP = (CELL)current_env;
    if (
	!Yap_growtrail(sz, FALSE)
	) {
      Yap_Error(OUT_OF_TRAIL_ERROR,TermNil,"out of %lB during gc", sz);
      return -1;
    } else {
      total_marked = 0;
      total_oldies = 0;
#ifdef COROUTING
      total_smarked = 0;
#endif
      discard_trail_entries = 0;
      current_env = (CELL *)*ASP;
      ASP++;
    }
  }
#if EASY_SHUNTING
  sTR0 = sTR = NULL;
#endif
  total_marked = 0;
  total_oldies = 0;
#ifdef COROUTING
  total_smarked = 0;
#endif
  discard_trail_entries = 0;
  alloc_sz = (CELL *)Yap_TrailTop-(CELL*)Yap_GlobalBase;
  Yap_bp = Yap_PreAllocCodeSpace();
  while (Yap_bp+alloc_sz > (char *)AuxSp) {
    /* not enough space */
    *--ASP = (CELL)current_env;
    Yap_bp = (char *)Yap_ExpandPreAllocCodeSpace(alloc_sz, NULL, TRUE);
    if (!Yap_bp)
      return -1;
    current_env = (CELL *)*ASP;
    ASP++;
  }
  memset((void *)Yap_bp, 0, alloc_sz);
#ifdef HYBRID_SCHEME
  iptop = (CELL_PTR *)H;
#endif
  /* get the number of active registers */
  HGEN = VarOfTerm(Yap_ReadTimedVar(GcGeneration));

  gc_phase = (UInt)IntegerOfTerm(Yap_ReadTimedVar(GcPhase));
  /* old HGEN are not very reliable, but still may have data to recover */
  if (gc_phase != GcCurrentPhase) {
    HGEN = H0;
  }
  /*  fprintf(stderr,"HGEN is %ld, %p, %p/%p\n", IntegerOfTerm(Yap_ReadTimedVar(GcGeneration)), HGEN, H,H0);*/
  OldTR = (tr_fr_ptr)(old_TR = TR);
  push_registers(predarity, nextop);
  /* make sure we clean bits after a reset */
  marking_phase(old_TR, current_env, nextop);
  if (total_oldies > ((HGEN-H0)*8)/10) {
    total_marked -= total_oldies;
    tot = total_marked+(HGEN-H0);
  } else {
    if (HGEN != H0) {
      HGEN = H0;
      GcCurrentPhase++;
    }
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
      fprintf(Yap_stderr,"%%       previous generation has size " UInt_FORMAT ", with " UInt_FORMAT " (" UInt_FORMAT "%%) unmarked\n", (UInt)(HGEN-H0), (UInt)((HGEN-H0)-total_oldies), (UInt)(100*((HGEN-H0)-total_oldies)/(HGEN-H0)));
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
  compaction_phase(old_TR, current_env, nextop);
  TR = old_TR;
  pop_registers(predarity, nextop);
  TR = new_TR;
  /*  fprintf(Yap_stderr,"NEW HGEN %ld (%ld)\n", H-H0, HGEN-H0);*/
  {
    Term t = MkVarTerm();
    Yap_UpdateTimedVar(GcGeneration, t);
  }
  Yap_UpdateTimedVar(GcPhase, MkIntegerTerm(GcCurrentPhase));
  c_time = Yap_cputime();
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%   Compress: took %g sec\n", (double)(c_time-time_start)/1000);
  }
  gc_time += (c_time-time_start);
  TotGcTime += gc_time;
  TotGcRecovered += heap_cells-tot;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%% GC %lu took %g sec, total of %g sec doing GC so far.\n", (unsigned long int)GcCalls, (double)gc_time/1000, (double)TotGcTime/1000);
    fprintf(Yap_stderr, "%%  Left %ld cells free in stacks.\n",
	       (unsigned long int)(ASP-H));
  }
  check_global();
  return effectiveness;
}

static int
is_gc_verbose(void)
{
  if (Yap_PrologMode == BootMode)
    return FALSE;
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
  if (Yap_PrologMode == BootMode)
    return FALSE;
  return Yap_GetValue(AtomGcVeryVerbose) != TermNil;
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
  Term ts = Yap_Mk64IntegerTerm((TotGcRecovered*sizeof(CELL)));
 
  return(Yap_unify(tn, ARG2) && Yap_unify(tt, ARG1) && Yap_unify(ts, ARG3));

}


static int
call_gc(UInt gc_lim, Int predarity, CELL *current_env, yamop *nextop)
{
  UInt   gc_margin = MinStackGap;
  Term   Tgc_margin;
  Int    effectiveness = 0;
  int    gc_on = FALSE, gc_t = FALSE;

  if (Yap_GetValue(AtomGc) != TermNil)
    gc_on = TRUE;
  if (IsIntegerTerm(Tgc_margin = Yap_GetValue(AtomGcMargin)) &&
      gc_margin > 0) {
    gc_margin = (UInt)IntegerOfTerm(Tgc_margin);
    gc_t = TRUE;
  } else {
    /* only go exponential for the first 6 calls, that would ask about 2MB minimum */
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
  HGEN = VarOfTerm(Yap_ReadTimedVar(GcGeneration));
  if (gc_on && !(Yap_PrologMode & InErrorMode) &&
      /* make sure there is a point in collecting the heap */
      (ASP-H0)*sizeof(CELL) > gc_lim && 
      H-HGEN > (LCL0-ASP)/2) {
    effectiveness = do_gc(predarity, current_env, nextop);
    if (effectiveness < 0)
      return FALSE;
    if (effectiveness > 90 && !gc_t) {
      while (gc_margin < (H-H0)/sizeof(CELL)) 
	gc_margin <<= 1;
    }
  } else {
    effectiveness = 0;
  }
  /* expand the stack if effectiveness is less than 20 % */
  if (ASP - H < gc_margin/sizeof(CELL) ||
      effectiveness < 20) {
    LeaveGCMode();
    if (gc_margin < 2*CalculateStackGap())
      gc_margin = 2*CalculateStackGap();
    return Yap_growstack(gc_margin);
  }
  /*
   * debug for(save_total=1; save_total<=N; ++save_total)
   * plwrite(XREGS[save_total],Yap_DebugPutc,0); 
   */
  return TRUE;
}

static void
LeaveGCMode()
{
  if (Yap_PrologMode & GCMode)
    Yap_PrologMode &= ~GCMode;
  if (Yap_PrologMode & AbortMode) {
    Yap_PrologMode &= ~AbortMode;
    Yap_Error(PURE_ABORT, TermNil, "");
    P = FAILCODE;
  }
}

int 
Yap_gc(Int predarity, CELL *current_env, yamop *nextop)
{
  int res;
  Yap_PrologMode |= GCMode;
  res=call_gc(4096, predarity, current_env, nextop);
  LeaveGCMode();
  if (Yap_PrologMode & GCMode)
    Yap_PrologMode &= ~GCMode;
  return res;
}

int 
Yap_gcl(UInt gc_lim, Int predarity, CELL *current_env, yamop *nextop)
{
  int res;
  UInt min = CalculateStackGap()*sizeof(CELL);

  Yap_PrologMode |= GCMode;
  if (gc_lim < min)
    gc_lim = min;
  res = call_gc(gc_lim, predarity, current_env, nextop);
  LeaveGCMode();
  return res;
}


static Int
p_gc(void)
{
  int res;
  Yap_PrologMode |= GCMode;
  if (P->opc == Yap_opcode(_execute_cpred))
    res = do_gc(0, ENV, CP) >= 0;
  else
    res = do_gc(0, ENV, P) >= 0;
  LeaveGCMode();
  return res;
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
