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


#define EARLY_RESET 1
#if !defined(TABLING)
#define EASY_SHUNTING 1
#endif
#define HYBRID_SCHEME 1


/* global variables for garbage collection */

#ifndef DEBUG
static
#endif
unsigned int      gc_calls = 0;	/* number of times GC has been called */

static Int      tot_gc_time = 0; /* total time spent in GC */

static Int      tot_gc_recovered = 0; /* number of heap objects in all garbage collections */

/* in a single gc */
static unsigned long int   total_marked;	/* number of heap objects marked */

struct gc_ma_h_entry *live_list;

STATIC_PROTO(Int  p_inform_gc, (void));
STATIC_PROTO(Int  p_gc, (void));

#ifdef EASY_SHUNTING
static choiceptr current_B;

static tr_fr_ptr sTR, sTR0;

static CELL *prev_HB;
#endif

static tr_fr_ptr new_TR;

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
STATIC_PROTO(choiceptr update_B_H, (choiceptr, CELL *, CELL *, CELL *));
STATIC_PROTO(void compact_heap, (void));
STATIC_PROTO(void update_relocation_chain, (CELL *, CELL *));
STATIC_PROTO(int  is_gc_verbose, (void));
STATIC_PROTO(int  is_gc_very_verbose, (void));

#include "heapgc.h"

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

inline static void
PUSH_CONTINUATION(CELL *v, int nof) {
  cont *x;
  if (nof == 0) return;
  x = cont_top;
  x++;
  if ((ADDR)x > Yap_TrailTop-1024)
    Yap_growtrail(64 * 1024L);
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
POPSWAP_POINTER(CELL_PTR *vp) {
  if (iptop >= (CELL_PTR *)ASP) return;
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

typedef struct gc_ma_h_entry {
  CELL* addr;
  tr_fr_ptr trptr;
  struct gc_ma_h_entry* ma_list;
  struct gc_ma_h_entry *next;
} gc_ma_h_inner_struct;

extern struct gc_ma_h_entry *live_list;

typedef struct {
  UInt timestmp;
  struct gc_ma_h_entry val;
} gc_ma_hash_entry;

static gc_ma_hash_entry gc_ma_hash_table[GC_MAVARS_HASH_SIZE];

static UInt timestamp;    /* an unsigned int */

static inline unsigned int
GC_MAVAR_HASH(CELL *addr) {
#if SIZEOF_INT_P==8
  return((((unsigned int)((CELL)(addr)))>>3)%GC_MAVARS_HASH_SIZE);
#else
  return((((unsigned int)((CELL)(addr)))>>2)%GC_MAVARS_HASH_SIZE); 
#endif
}

gc_ma_h_inner_struct *gc_ma_h_top;

static inline struct gc_ma_h_entry *
GC_ALLOC_NEW_MASPACE(void)
{
  gc_ma_h_inner_struct *new = gc_ma_h_top;
  if ((char *)gc_ma_h_top > Yap_TrailTop-1024)
    Yap_growtrail(64 * 1024L);
  gc_ma_h_top++;
  cont_top = (cont *)gc_ma_h_top;
#ifdef EASY_SHUNTING
  sTR = (tr_fr_ptr)cont_top;
#else
  cont_top0 = cont_top;
#endif
  return(new);
}

static inline tr_fr_ptr*
gc_lookup_ma_var(CELL *addr, tr_fr_ptr trp) {
  unsigned int i = GC_MAVAR_HASH(addr);
  struct gc_ma_h_entry *nptr, *optr;

  if (gc_ma_hash_table[i].timestmp != timestamp) {
    gc_ma_hash_table[i].timestmp = timestamp;
    gc_ma_hash_table[i].val.addr = addr;
    gc_ma_hash_table[i].val.next = NULL;
    gc_ma_hash_table[i].val.trptr = trp;
    gc_ma_hash_table[i].val.ma_list = live_list;
    live_list = &(gc_ma_hash_table[i].val);
    return(NULL);
  }
  if (gc_ma_hash_table[i].val.addr == addr) {
    return(&(gc_ma_hash_table[i].val.trptr));
  }
  optr = &(gc_ma_hash_table[i].val);
  nptr = gc_ma_hash_table[i].val.next;
  while (nptr != NULL) {
    if (nptr->addr == addr) {
      return(&(nptr->trptr));
    }
    optr = nptr;
    nptr = nptr->next;
  }
  nptr = GC_ALLOC_NEW_MASPACE();
  optr->next = nptr;
  nptr->addr = addr;
  nptr->trptr = trp;
  nptr->ma_list = live_list;
  nptr->next = NULL;
  live_list = nptr;
  return(NULL);
}

static inline void
GC_NEW_MAHASH(gc_ma_h_inner_struct *top) {
  UInt time = ++timestamp;
  if (time == 0) {
    unsigned int i;
    /* damn, we overflowed */
    for (i = 0; i < GC_MAVARS_HASH_SIZE; i++)
      gc_ma_hash_table[i].timestmp = 0;
    time = ++timestamp;
  }
  gc_ma_h_top = top;
  cont_top = (cont *)gc_ma_h_top;
#ifdef EASY_SHUNTING
  sTR = (tr_fr_ptr)cont_top;
#else
  cont_top0 = cont_top;
#endif
  live_list = NULL;
}

#endif

/* find all accessible objects on the heap and squeeze out all the rest */


/* push the active registers onto the trail for inclusion during gc */

static void 
push_registers(Int num_regs, yamop *nextop)
{
  int             i;

  /* push array entries first */
  ArrayEntry *al = DynArrayList;
  while (al != NULL) {
    if (al->ArrayEArity > 0) {
      TrailTerm(TR++) = al->ValueOfVE;
    }
    al = al->NextArrayE;
  }
#ifdef COROUTINING
  TrailTerm(TR) = WokenGoals;
  TrailTerm(TR+1) = MutableList;
  TrailTerm(TR+2) = AttsMutableList;
  TrailTerm(TR+3) = DelayedVars;
  TR += 4;
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

  /* pop array entries first */
  ArrayEntry *al = DynArrayList;
  while (al != NULL) {
    if (al->ArrayEArity > 0) {
      al->ValueOfVE = TrailTerm(ptr++);
    }
    al = al->NextArrayE;
  }
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
  WokenGoals = TrailTerm(ptr++);
  MutableList = TrailTerm(ptr++);
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

#ifdef DEBUG
static int 
count_cells_marked(void)
{
  CELL *current;
  int found_marked = 0;

  for (current = H - 1; current >= H0; current--) {
    if (MARKED(*current)) {
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
  dcl_entry
} db_entry_type;

typedef struct db_entry {
  CODEADDR val;
  db_entry_type db_type;
  struct db_entry *left;
  CODEADDR lim;
  struct db_entry *right;
} *dbentry;

static dbentry  db_vec, db_vec0;


/* init the table */
static void
store_in_dbtable(CODEADDR entry, db_entry_type db_type)
{
  dbentry parent = db_vec0;
  dbentry new = db_vec;

  if ((ADDR)new > Yap_TrailTop-1024)
    Yap_growtrail(64 * 1024L);
  new->val = entry;
  new->db_type = db_type;
  new->lim = entry+Yap_SizeOfBlock((CODEADDR)entry);
  new->left = new->right = NULL;
  if (db_vec == db_vec0) {
    db_vec++;
    return;
  }
  db_vec++;
  parent = db_vec0;
 beg:
  if (entry < parent->val) {
    if (parent->right == NULL) {
      parent->right = new;
    } else {
      parent = parent->right;
      goto beg;
    }
  } else {
    if (parent->left == NULL) {
      parent->left = new;
    } else {
      parent = parent->left;
      goto beg;
    }
  }
}

/* find an element in the dbentries table */
static dbentry
find_ref_in_dbtable(CODEADDR entry)
{
  dbentry current = db_vec0;

  while (current != NULL) {
    if (current->val < entry && current->lim > entry) {
      return(current);
    }
    if (entry < current->val)
      current = current->right;
    else
      current = current->left;
  }
  return(NULL);
}

static void 
mark_db_fixed(CELL *ptr) {
  dbentry el;

  el = find_ref_in_dbtable((CODEADDR)ptr);
  if (el != NULL) {
    switch (el->db_type) {
    case db_entry:
      ((DBRef)(el->val))->Flags |= GcFoundMask;
      break;
    case cl_entry:
      ((DynamicClause *)(el->val))->ClFlags |= GcFoundMask;
      break;
    case lcl_entry:
      ((LogUpdClause *)(el->val))->ClFlags |= GcFoundMask;
      break;
    case dcl_entry:
      ((DeadClause *)(el->val))->ClFlags |= GcFoundMask;
      break;
    }
  }
}

static void 
init_dbtable(tr_fr_ptr trail_ptr) {
  DeadClause *cl = DeadClauses;

  db_vec0 = db_vec = (dbentry)TR;
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
	store_in_dbtable((CODEADDR)DBStructFlagsToDBStruct(pt0), db_entry);
      } else if (flags & LogUpdMask) {
	store_in_dbtable((CODEADDR)ClauseFlagsToLogUpdClause(pt0), lcl_entry);
      } else {
	store_in_dbtable((CODEADDR)ClauseFlagsToDynamicClause(pt0), cl_entry);
      }
    }
  }
  while (cl != NULL) {
    store_in_dbtable((CODEADDR)cl, dcl_entry);
    cl = cl->NextCl;
  }
  if (db_vec == db_vec0) {
    /* could not find any entries: probably using LOG UPD semantics */
    db_vec0 = NULL;
  }
}

#ifndef ANALYST

static char *op_names[_std_top + 1] =
{
#define OPCODE(OP,TYPE) #OP
#include "YapOpcodes.h"
#undef  OPCODE
};

#endif

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
  fprintf(Yap_stderr,"[GC]  type info for %lu cells\n", total);
  fprintf(Yap_stderr,"[GC]      %lu vars\n", vars[gc_var]);
  fprintf(Yap_stderr,"[GC]      %lu refs\n", vars[gc_ref]);
  fprintf(Yap_stderr,"[GC]      %lu references from env\n", env_vars);
  fprintf(Yap_stderr,"[GC]      %lu atoms\n", vars[gc_atom]);
  fprintf(Yap_stderr,"[GC]      %lu small ints\n", vars[gc_int]);
  fprintf(Yap_stderr,"[GC]      %lu other numbers\n", vars[gc_num]);
  fprintf(Yap_stderr,"[GC]      %lu lists\n", vars[gc_list]);
  fprintf(Yap_stderr,"[GC]      %lu compound terms\n", vars[gc_appl]);
  fprintf(Yap_stderr,"[GC]      %lu functors\n", vars[gc_func]);
  fprintf(Yap_stderr,"[GC]      %lu suspensions\n", vars[gc_susp]);
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

    if (MARKED(ccurr)) {
      CELL ccell = UNMARK_CELL(ccurr);
      if (ccell < (CELL)AtomBase && ccell > EndSpecials && IsVarTerm(ccell)) {
	/* oops, we found a blob */
	int nofcells = (UNMARK_CELL(*current)-EndSpecials) / sizeof(CELL);
	CELL *ptr = current - nofcells ;
	current = ptr;
	ccurr = *current;
	/* process the functor next */
      }
      if (MARKED(ccurr)) {
	printf("Oops, found marked cell at %p\n", current);
	break;
      }
    }
#if INSTRUMENT_GC
    if (IsVarTerm(ccurr)) {
      if (IsBlobFunctor((Functor)ccurr)) vars[gc_num]++;
      else if (ccurr != 0 && ccurr < (CELL)HeapTop) {
	/*	printf("%p: %s/%d\n", current,
	       RepAtom(NameOfFunctor((Functor)ccurr))->StrOfAE,
	       ArityOfFunctor((Functor)ccurr));*/
	vars[gc_func]++;
      }
      else if (IsUnboundVar((CELL)current)) vars[gc_var]++;
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
  ccur = *current;
  if (MARKED(ccur)) {
    POP_CONTINUATION();
  }
  MARK(current);
  total_marked++;
  PUSH_POINTER(current);
  next = GET_NEXT(ccur);

  if (IsVarTerm(ccur)) {
    if (ONHEAP(next)) {
#ifdef EASY_SHUNTING
      CELL cnext;
      /* do variable shunting between variables in the global */
      if (!MARKED((cnext = *next))) {
	if (IsVarTerm(cnext) && (CELL)next == cnext) {
	  /* new global variable to new global variable */
	  if (current < prev_HB && current >= HB && next >= HB && next < prev_HB) {
#ifdef INSTRUMENT_GC
	    inc_var(current, current);
#endif	      
	    *next = (CELL)current;
	    *current = MARK_CELL((CELL)current);
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
	    *current = cnext;
	    total_marked--;
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
	total_marked--;
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
    }
#ifdef DEBUG
    else if (next < (CELL *)AtomBase || next < (CELL *)HeapTop)
      fprintf(Yap_stderr, "ooops while marking %lx, %p at %p\n", (unsigned long int)ccur, current, next);
#endif
#ifdef INSTRUMENT_GC
    else
      inc_var(current, next);
#endif
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
    if ( MARKED(cnext) || !ONHEAP(next) )
      POP_CONTINUATION();
    
    if (next < H0) POP_CONTINUATION();
    if (IsExtensionFunctor((Functor)cnext)) {
      switch (cnext) {
      case (CELL)FunctorLongInt:
	MARK(next);
	total_marked += 3;
	PUSH_POINTER(next);
	PUSH_POINTER(next+1);
	PUSH_POINTER(next+2);
	POP_CONTINUATION();
      case (CELL)FunctorDouble:
	MARK(next);
	total_marked += 2+SIZEOF_DOUBLE/SIZEOF_LONG_INT;
	PUSH_POINTER(next);
	PUSH_POINTER(next+1);
	PUSH_POINTER(next+2);
#if SIZEOF_DOUBLE==2*SIZEOF_LONG_INT
	PUSH_POINTER(next+3);
#endif
	POP_CONTINUATION();
#ifdef USE_GMP
      case (CELL)FunctorBigInt:
	MARK(next);
	/* size is given by functor + friends */
	total_marked += 2+
	  (sizeof(MP_INT)+
	   (((MP_INT *)(next+1))->_mp_alloc*sizeof(mp_limb_t)))/CellSize;
	{
	  int i;
	  PUSH_POINTER(next);
	  for (i = 1; i <= (sizeof(MP_INT)+
		 (((MP_INT *)(next+1))->_mp_alloc*sizeof(mp_limb_t)))/CellSize;
	       i++)
	    PUSH_POINTER(next+i);
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
mark_external_reference(CELL *ptr) {
  CELL reg = *ptr;

  /* first, mark variables in environments */
  if (IsVarTerm(reg)) {
    if (ONHEAP(reg)) {
#ifdef HYBRID_SCHEME
      CELL_PTR *old = iptop;
#endif      
      mark_variable(ptr);
      total_marked--;
      POPSWAP_POINTER(old);
    } else {
      MARK(ptr);
    }
  } else if (IsApplTerm(reg)) {
    CELL *next = RepAppl(reg);

    if (ONHEAP(next)) {
#ifdef HYBRID_SCHEME
      CELL_PTR *old = iptop;
#endif      
      mark_variable(ptr);
      total_marked--;
      POPSWAP_POINTER(old);
    } else {
      MARK(ptr);
      if (ONCODE(next)) {
	if ((Functor)(*next) == FunctorDBRef) {
	  DBRef tref = DBRefOfTerm(reg);
	  /* make sure the reference is marked as in use */
	  if ((tref->Flags & ErasedMask) &&
	      tref->Parent != NULL &&
	      tref->Parent->KindOfPE & LogUpdDBBit) {
	    *ptr = MkDBRefTerm(DBErasedMarker);
	    MARK(ptr);
	  } else {
	    tref->Flags |= GcFoundMask;
	  }
	} else {
	  mark_db_fixed(next);
	}
      }
    }
  } else if (IsPairTerm(reg)) {
   CELL *next = RepPair(reg);
   
   if (ONHEAP(next)) {
#ifdef HYBRID_SCHEME
      CELL_PTR *old = iptop;
#endif      
      mark_variable(ptr);
      total_marked--;
      POPSWAP_POINTER(old);
   } else {
     MARK(ptr);
     if (ONCODE(next)) {
       mark_db_fixed(next);
     }
   }
  } else {
    /* atom or integer */
    MARK(ptr);
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
    mark_external_reference(ptr);
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
      if (bmap < 0 && !MARKED(*saved_var)) {
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
	    if (len>=15)
	      (chain[15])++;
	    else
	      (chain[len])++;
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
    if (MARKED(gc_ENV[E_CB]))
      return;
    MARK(gc_ENV+E_CB);
    
    size = EnvSize((CELL_PTR) (gc_ENV[E_CP]));	/* size = EnvSize(CP) */
    pvbmap = EnvBMap((CELL_PTR) (gc_ENV[E_CP]));
#if 0
      if (size < 0) {
	PredEntry *pe = EnvPreg(gc_ENV[E_CP]);
	op_numbers op = Yap_op_from_opcode(ENV_ToOp(gc_ENV[E_CP]));
	fprintf(Yap_stderr,"ENV %p-%p(%d) %s\n", gc_ENV, pvbmap, size-EnvSizeInCells, op_names[op]);
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
  GC_NEW_MAHASH((gc_ma_h_inner_struct *)cont_top0);
  while (trail_ptr > trail_base) {
    register CELL trail_cell;
    
    trail_ptr--;
    
    trail_cell = TrailTerm(trail_ptr);

    if (IsVarTerm(trail_cell)) {
      CELL *hp = (CELL *)trail_cell;
      /* if a variable older than the current CP has not been marked yet,
	 than its new binding is not accessible and we can reset it. Note
	 we must use gc_H to avoid trouble with dangling variables
	 in the heap */
      if (((hp < gc_H   && hp >= H0) || (hp > (CELL *)gc_B && hp < LCL0) ) && !MARKED(*hp)) {
#ifdef EARLY_RESET
	/* reset to be a variable */
	RESET_VARIABLE(hp);
	discard_trail_entries++;
	RESET_VARIABLE(&TrailTerm(trail_ptr));
#ifdef FROZEN_STACKS
	RESET_VARIABLE(&TrailVal(trail_ptr));
#endif
#else
	/* if I have no early reset I have to follow the trail chain */
	mark_external_reference(&TrailTerm(trail_ptr));	
	UNMARK(&TrailTerm(trail_ptr));
#endif /* EARLY_RESET */
      } else if (hp < (CELL *)HeapTop) {
	  /* I decided to allow pointers from the Heap back into the trail.
	   The point of doing so is to have dynamic arrays */
	mark_external_reference(hp);
      } else if ((hp < (CELL *)gc_B && hp >= gc_H) || hp > (CELL *)Yap_TrailBase) {
	/* clean the trail, avoid dangling pointers! */
	RESET_VARIABLE(&TrailTerm(trail_ptr));
#ifdef FROZEN_STACKS
	RESET_VARIABLE(&TrailVal(trail_ptr));
#endif
	discard_trail_entries++;
      } else {
	if (trail_cell == (CELL)trail_ptr)
	  discard_trail_entries++;
#ifdef FROZEN_STACKS
	else
	  mark_external_reference(&TrailVal(trail_ptr));
#endif
#ifdef EASY_SHUNTING
	if (hp < gc_H   && hp >= H0) {
	  tr_fr_ptr nsTR = (tr_fr_ptr)cont_top0;
          CELL *cptr = (CELL *)trail_cell;

	  if ((ADDR)nsTR > Yap_TrailTop-1024)
	    Yap_growtrail(64 * 1024L);
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
	  gc_ma_h_top = (gc_ma_h_inner_struct *)(nsTR+3);
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
      tr_fr_ptr *lkp;
      CELL *cptr = RepAppl(trail_cell);
      /* This is a bit complex. The idea is that we may have several
	 trailings for the same mavar in the same trail segment. Essentially,
	 the problem arises because of !. What we want is to ignore all but
	 the last entry, or in this case, all but the first entry with the last
	 value.

	 Problem: we can only mark when we know it is the *last*.

	 Solution: we keep a list of all found entries and search in the end
      */
      if (!(lkp = gc_lookup_ma_var(cptr, trail_ptr))) {
	if (HEAP_PTR(trail_cell)) {
	  /* fool the gc into thinking this is a variable */
	  TrailTerm(trail_ptr) = (CELL)cptr;
	  mark_external_reference(&(TrailTerm(trail_ptr)));
	  /* reset the gc to believe the original tag */
	  TrailTerm(trail_ptr) = AbsAppl((CELL *)TrailTerm(trail_ptr));
	} 
	trail_ptr --;
      } else {
	tr_fr_ptr trp = (*lkp)-1;
	TrailTerm(trp) = TrailTerm(trail_ptr-1);
	/* we can safely ignore this little monster */
	discard_trail_entries += 2;
	RESET_VARIABLE(&TrailTerm(trail_ptr));
#ifdef FROZEN_STACKS
	RESET_VARIABLE(&TrailVal(trail_ptr));
#endif
	trail_ptr--;
	RESET_VARIABLE(&TrailTerm(trail_ptr));
#ifdef FROZEN_STACKS
	RESET_VARIABLE(&TrailVal(trail_ptr));
#endif
      }
    }
#endif
  }
#if  MULTI_ASSIGNMENT_VARIABLES
  while (live_list != NULL) {
    CELL trail_cell = TrailTerm(live_list->trptr-1);
    CELL trail_cell2 = TrailTerm(live_list->trptr);
    if (HEAP_PTR(trail_cell)) {
      mark_external_reference(&TrailTerm(live_list->trptr-1));
    }
#ifdef FROZEN_STACKS
    if (HEAP_PTR(TrailVal(trail_ptr))) {
      mark_external_reference(&TrailVal(trail_ptr));
    }
#endif
    /*
      swap the two so that the sweep_trail() knows we have
      a multi-assignment binding
    */
    TrailTerm(live_list->trptr) = TrailTerm(live_list->trptr-1);
    TrailTerm(live_list->trptr-1) = trail_cell2;
#ifdef FROZEN_STACKS
    if (HEAP_PTR(TrailVal(trail_ptr-1))) {
      mark_external_reference(&TrailVal(trail_ptr-1));
    }
#endif
    live_list = live_list->ma_list;
  }
#endif
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
#ifdef TABLING_BATCHED_SCHEDULING
#define init_substitution_pointer(GCB, SUBS_PTR, DEP_FR)  \
        SUBS_PTR = (CELL *) (CONS_CP(GCB) + 1)
#else /* TABLING_LOCAL_SCHEDULING */
#define init_substitution_pointer(GCB, SUBS_PTR, DEP_FR)  \
        SUBS_PTR = (CELL *) (CONS_CP(GCB) + 1);        \
        if (DepFr_leader_cp(DEP_FR) == GCB)            \
          SUBS_PTR += SgFr_arity(GEN_CP_SG_FR(GCB))
#endif /* TABLING_SCHEDULING */
#endif


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

#ifdef TABLING
   dep_fr_ptr depfr = LOCAL_top_dep_fr;
#endif
#ifdef EASY_SHUNTING
    HB = H;
#endif
  while (gc_B != NULL) {
    op_numbers opnum;
    register OPCODE op;
    yamop *rtp = gc_B->cp_ap;

    mark_db_fixed((CELL *)rtp);
    mark_db_fixed((CELL *)(gc_B->cp_ap));
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
#endif
      {
	op = rtp->opc;
	opnum = Yap_op_from_opcode(op);
      }
    if (very_verbose) {
      switch (opnum) {
      case _retry_c:
      case _or_else:
      case _or_last:
      case _Nstop:
      case _retry_userc:
      case _trust_logical_pred:
      case _retry_profiled:
      case _count_retry:
	{
	  Atom at;
	  unsigned long int arity;
	  SMALLUNSGN mod;
	  if (Yap_PredForCode(gc_B->cp_ap, &at, (UInt *)(&arity), &mod)) {
	    if (arity) 
	      fprintf(Yap_stderr,"[GC]       %s/%ld marked %ld (%s)\n", RepAtom(at)->StrOfAE, arity, total_marked, op_names[opnum]);
	    else
	      fprintf(Yap_stderr,"[GC]       %s marked %ld (%s)\n", RepAtom(at)->StrOfAE, total_marked, op_names[opnum]);
	  } else
	    fprintf(Yap_stderr,"[GC]       marked %ld (%s)\n", total_marked, op_names[opnum]);
	}
	break;
#ifdef TABLING
      case _table_completion:
      case _table_answer_resolution:
	{
	  PredEntry *pe = ENV_ToP(gc_B->cp_cp);
	  op_numbers caller_op = Yap_op_from_opcode(ENV_ToOp(gc_B->cp_cp));
	  /* first condition  checks if this was a meta-call */
	  if ((caller_op != _call  && caller_op != _fcall) || pe == NULL) {
	    fprintf(Yap_stderr,"[GC]       marked %d (%s)\n", total_marked, op_names[opnum]);
	  } else
	    fprintf(Yap_stderr,"[GC]       %s/%d marked %d (%s)\n", RepAtom(NameOfFunctor(pe->FunctorOfPred))->StrOfAE, pe->ArityOfPE, total_marked, op_names[opnum]);
	}
	break;
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
	fprintf(Yap_stderr,"[GC]       marked %d (%s)\n", total_marked, op_names[opnum]);
	break;
#endif
      default:
	{
	  PredEntry *pe = (PredEntry *)gc_B->cp_ap->u.ld.p;
	  if (pe == NULL) {
	    fprintf(Yap_stderr,"[GC]       marked %ld (%s)\n", total_marked, op_names[opnum]);
	  } else
	    if (pe->ArityOfPE)
	      fprintf(Yap_stderr,"[GC]       %s/%d marked %ld (%s)\n", RepAtom(NameOfFunctor(pe->FunctorOfPred))->StrOfAE, pe->ArityOfPE, total_marked, op_names[opnum]);
	    else
	      fprintf(Yap_stderr,"[GC]       %s marked %ld (%s)\n", RepAtom((Atom)(pe->FunctorOfPred))->StrOfAE, total_marked, op_names[opnum]);
	}
      }
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
  restart_cp:
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
#endif
	mark_environments((CELL_PTR) gc_B->cp_env,
			  EnvSize((CELL_PTR) (gc_B->cp_cp)),
			  EnvBMap((CELL_PTR) (gc_B->cp_cp)));
      /* extended choice point */
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
      case _table_answer_resolution:
	{
	  CELL *answ_fr;
	  CELL vars;

	  /* fetch the solution */
	  init_substitution_pointer(gc_B, answ_fr, CONS_CP(gc_B)->ccp_dep_fr);
	  vars = *answ_fr++;
	  while (vars--) {	
	    mark_external_reference(answ_fr);
	    answ_fr++;
	  }
	  nargs = 0;
	}
	break;
      case _table_completion:
	{
	  register gen_cp_ptr gcp = GEN_CP(gc_B);
	  int nargs;
	  
#ifdef TABLING_BATCHED_SCHEDULING
	  nargs = gcp->gcp_sg_fr->subgoal_arity;
#else
	  nargs = gcp->gcp_dep_fr->subgoal_frame->subgoal_arity;
#endif
	  saved_reg = (CELL *)(gcp+1)+nargs;
	  nargs = *saved_reg++;
	  while (nargs--) {	
	    mark_external_reference(saved_reg);
	    saved_reg++;
	  }
	}
	nargs = 0;
	break;
      case _table_retry_me:
      case _table_trust_me:
	{
	  register gen_cp_ptr gcp = GEN_CP(gc_B);	  
	  int nargs = rtp->u.ld.s;
	  /* for each saved register */
	  for (saved_reg = (CELL *)(gcp+1);
	       /* assumes we can count registers in CP this
		  way */
	       saved_reg < (CELL *)(gcp+1) + nargs;
	       saved_reg++) {
	    mark_external_reference(saved_reg);
	  }
	  nargs = *saved_reg++;
	  while (nargs--) {	
	    mark_external_reference(saved_reg);
	    saved_reg++;
	  }
	}
	nargs = 0;
	break;
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
	{
	  CELL *aux_ptr;
	  int heap_arity;
	  int vars_arity;
	  int subs_arity;

	  /* fetch the solution */
	  aux_ptr = (CELL *)(gc_B+1);
	  heap_arity = *aux_ptr;
	  vars_arity = *(aux_ptr + heap_arity + 1);
	  subs_arity = *(aux_ptr + heap_arity + 2);
	  if (heap_arity) {
	    int i;
	    aux_ptr += heap_arity + subs_arity + vars_arity + 1;
	    for (i = 0; i < heap_arity + subs_arity + vars_arity + 1; i++) {
	      mark_external_reference(aux_ptr);
	      aux_ptr--;
	    }
	  } else {
	    int i;
	    aux_ptr += 2 + subs_arity + vars_arity;
	    for (i = 0; i < vars_arity; i++) {
	      mark_external_reference(aux_ptr);
	      aux_ptr--;
	    }
	    for (i = 1; i < subs_arity; i++) {
	      aux_ptr--;
	      mark_external_reference(aux_ptr);
	    }
	  }
	}
	nargs = 0;
	break;
#endif
      case _profiled_retry_and_mark:
      case _count_retry_and_mark:
      case _retry_and_mark:
	ClauseCodeToDynamicClause(gc_B->cp_ap)->ClFlags |= GcFoundMask;
#ifdef DEBUG
      case _retry_me:
      case _trust_me:
      case _profiled_retry_me:
      case _profiled_trust_me:
      case _count_retry_me:
      case _count_trust_me:
      case _retry_me0:
      case _trust_me0:
      case _retry_me1:
      case _trust_me1:
      case _retry_me2:
      case _trust_me2:
      case _retry_me3:
      case _trust_me3:
      case _retry_me4:
      case _trust_me4:
      case _retry:
      case _retry_killed:
      case _trust:
      case _trust_killed:
	nargs = rtp->u.ld.s;
	break;
      case _jump:
	rtp = rtp->u.l.l;
	op = rtp->opc;
	opnum = Yap_op_from_opcode(op);
	goto restart_cp;
      default:
	fprintf(Yap_stderr, "OOps in GC: Unexpected opcode: %d\n", opnum);
	nargs = 0;
#else
      default:
	nargs = rtp->u.ld.s;
#endif
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

static void 
into_relocation_chain(CELL_PTR current, CELL_PTR next)
{
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
    if (MARKED(TrailTerm(trail_ptr))) {
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
	if ((CELL *)trail_cell < H && MARKED(*(CELL *)trail_cell) && (CELL *)trail_cell >= H0) {
	  if (HEAP_PTR(trail_cell)) {
	    into_relocation_chain(&TrailTerm(dest), GET_NEXT(trail_cell));
	  }
#ifdef FROZEN_STACKS
	  /* it is complex to recover cells with frozen segments */
	  TrailVal(dest) = TrailVal(trail_ptr);
	  if (MARKED(TrailVal(dest))) {
	    UNMARK(&TrailVal(dest));
	    if (HEAP_PTR(TrailVal(dest))) {
	      into_relocation_chain(&TrailVal(dest), GET_NEXT(TrailVal(dest)));
	    }
	  }
#endif
	} else if ((CELL *)trail_cell < (CELL *)HeapTop) {
	  /* we may have pointers from the heap back into the cell */
	  CELL *next =  GET_NEXT(*CellPtr(trail_cell));
	  UNMARK(CellPtr(trail_cell));
	  if (HEAP_PTR(*CellPtr(trail_cell))) {
	    into_relocation_chain(CellPtr(trail_cell),next);
	  }
#ifdef FROZEN_STACKS
	  /* it is complex to recover cells with frozen segments */
	  TrailVal(dest) = TrailVal(trail_ptr);
	  if (MARKED(TrailVal(dest))) {
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
	    (ADDR) pt0 >= HeapTop
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
	      LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt0);
	      int erase;
	      DEC_CLREF_COUNT(cl);
	      cl->ClFlags &= ~InUseMask;
	      erase = (cl->ClFlags & ErasedMask)
#if  defined(YAPOR) || defined(THREADS)
		&& (cl->ref_count == 0)
#endif
	      ;
	      if (erase) {
		/* at this point, 
		   no one is accessing the clause */
		Yap_ErLogUpdCl(cl);
	      }
	    } else {
	      DynamicClause *cl = ClauseFlagsToDynamicClause(pt0);
	      int erase;
	      DEC_CLREF_COUNT(cl);
	      cl->ClFlags &= ~InUseMask;
	      erase = (cl->ClFlags & ErasedMask)
#if  defined(YAPOR) || defined(THREADS)
		&& (cl->ref_count == 0)
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
	CELL trail_cell = TrailTerm(trail_ptr);
	CELL *ptr;
	CELL old = TrailTerm(trail_ptr+1);

	if (MARKED(trail_cell)) 
	  ptr = RepAppl(UNMARK_CELL(trail_cell));
	else
	  ptr = RepAppl(trail_cell);

	TrailTerm(dest) = old;
	TrailTerm(dest+1) = trail_cell;
	if (MARKED(old)) {
	  UNMARK(&TrailTerm(dest));
	  if (HEAP_PTR(old)) {
	    into_relocation_chain(&TrailTerm(dest), GET_NEXT(old));
	  }
	}
#ifdef FROZEN_STACKS
	TrailVal(dest) = TrailVal(trail_ptr);
	if (MARKED(TrailVal(dest))) {
	  UNMARK(&TrailVal(dest));
	  if (HEAP_PTR(TrailVal(dest))) {
	    into_relocation_chain(&TrailVal(dest), GET_NEXT(TrailTerm(dest)));
	  }
	}
#endif
	dest++;
	if (MARKED(trail_cell)) {
	  UNMARK(&TrailTerm(dest));
	  if (HEAP_PTR(trail_cell)) {
	    into_relocation_chain(&TrailTerm(dest), GET_NEXT(trail_cell));
	  }
	}
	trail_ptr++;
#ifdef FROZEN_STACKS
	TrailVal(dest) = TrailVal(trail_ptr);
	if (MARKED(TrailVal(dest))) {
	  UNMARK(&TrailVal(dest));
	  if (HEAP_PTR(TrailVal(dest))) {
	    into_relocation_chain(&TrailVal(dest), GET_NEXT(TrailTerm(dest)));
	  }
	}
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
		 "[GC]       Trail: discarded %d (%ld%%) cells out of %ld\n",
		 discard_trail_entries,
		 (unsigned long int)(discard_trail_entries*100/(old_TR-(tr_fr_ptr)Yap_TrailBase)),
		 (unsigned long int)(old_TR-(tr_fr_ptr)Yap_TrailBase));
#ifdef DEBUG
    if (hp_entrs > 0)
      fprintf(Yap_stderr,
		 "[GC]       Trail: unmarked %ld dbentries (%ld%%) out of %ld\n",
		 (long int)hp_not_in_use,
		 (long int)(hp_not_in_use*100/hp_entrs),
		 (long int)hp_entrs);
    if (hp_in_use_erased > 0 && hp_erased > 0)
      fprintf(Yap_stderr,
		 "[GC]       Trail: deleted %ld dbentries (%ld%%) out of %ld\n",
		 (long int)hp_erased,
		 (long int)(hp_erased*100/(hp_erased+hp_in_use_erased)),
		 (long int)(hp_erased+hp_in_use_erased));
#endif
    fprintf(Yap_stderr,
	       "[GC]       Heap: recovered %ld bytes (%ld%%) out of %ld\n",
	       (unsigned long int)(OldHeapUsed-HeapUsed),
	       (unsigned long int)((OldHeapUsed-HeapUsed)/(OldHeapUsed/100)),
	       (unsigned long int)OldHeapUsed);
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
	if (MARKED(env_cell)) {
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
    if (!MARKED(gc_ENV[E_CB]))
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
    if (MARKED(cp_cell)) {
      UNMARK(ptr);
      if (HEAP_PTR(cp_cell)) {
	into_relocation_chain(ptr, GET_NEXT(cp_cell));
      }
    }
    ptr++;
    ns--;
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
#endif
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
    case _table_answer_resolution:
      {
	CELL *answ_fr;
	CELL vars;

	sweep_environments(gc_B->cp_env,
			   EnvSize((CELL_PTR) (gc_B->cp_cp)),
			   EnvBMap((CELL_PTR) (gc_B->cp_cp)));

	/* fetch the solution */
	init_substitution_pointer(gc_B, answ_fr, CONS_CP(gc_B)->ccp_dep_fr);
	vars = *answ_fr++;
	while (vars--) {	
	  CELL cp_cell = *answ_fr;
	  if (MARKED(cp_cell)) {
	    UNMARK(answ_fr);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(answ_fr, GET_NEXT(cp_cell));
	    }
	  }
	  answ_fr++;
	}
      }
      break;
      case _table_completion:
	{
	  register gen_cp_ptr gcp = GEN_CP(gc_B);
	  
#ifdef TABLING_BATCHED_SCHEDULING
	  int nargs = gcp->gcp_sg_fr->subgoal_arity;
#else
	  int nargs = gcp->gcp_dep_fr->subgoal_frame->subgoal_arity;
#endif
	  CELL *saved_reg;

	  saved_reg = (CELL *)(gcp+1)+nargs;
	  nargs = *saved_reg++;
	  while (nargs--) {	
	    CELL cp_cell = *saved_reg;
	    if (MARKED(cp_cell)) {
	      UNMARK(saved_reg);
	      if (HEAP_PTR(cp_cell)) {
		into_relocation_chain(saved_reg, GET_NEXT(cp_cell));
	      }
	    }
	    saved_reg++;
	  }
	}
	break;
    case _table_retry_me:
    case _table_trust_me:
      {
	register gen_cp_ptr gcp = GEN_CP(gc_B);
	int nargs;
	CELL *saved_reg;
	
	sweep_environments(gc_B->cp_env,
			   EnvSize((CELL_PTR) (gc_B->cp_cp)),
			   EnvBMap((CELL_PTR) (gc_B->cp_cp)));

	nargs = rtp->u.ld.s;
	/* for each saved register */
	for (saved_reg = (CELL *)(gcp+1);
	     /* assumes we can count registers in CP this
		way */
	     saved_reg < (CELL *)(gcp+1) + nargs;
	     saved_reg++) {
	  CELL cp_cell = *saved_reg;
	  if (MARKED(cp_cell)) {
	    UNMARK(saved_reg);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(saved_reg, GET_NEXT(cp_cell));
	    }
	  }
	}
	saved_reg = (CELL *)(gcp+1) + nargs;
	nargs = *saved_reg++;
	while (nargs--) {	
	  CELL cp_cell = *saved_reg;
	  if (MARKED(cp_cell)) {
	    UNMARK(saved_reg);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(saved_reg, GET_NEXT(cp_cell));
	    }
	  }
	  saved_reg++;
	}
      }
      break;
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
	{
	  CELL *aux_ptr;
	  int heap_arity;
	  int vars_arity;
	  int subs_arity;

	  sweep_environments(gc_B->cp_env,
			     EnvSize((CELL_PTR) (gc_B->cp_cp)),
			     EnvBMap((CELL_PTR) (gc_B->cp_cp)));

	  /* fetch the solution */
	  aux_ptr = (CELL *)(gc_B+1);
	  heap_arity = *aux_ptr;
	  vars_arity = *(aux_ptr + heap_arity + 1);
	  subs_arity = *(aux_ptr + heap_arity + 2);
	  if (heap_arity) {
	    int i;
	    aux_ptr += heap_arity + subs_arity + vars_arity + 1;
	    for (i = 0; i < heap_arity + subs_arity + vars_arity + 1; i++) {
	      CELL cp_cell = *aux_ptr;
	      if (MARKED(cp_cell)) {
		UNMARK(aux_ptr);
		if (HEAP_PTR(cp_cell)) {
		  into_relocation_chain(aux_ptr, GET_NEXT(cp_cell));
		}
	      }
	      aux_ptr--;
	    }
	  } else {
	    int i;
	    aux_ptr += 2 + subs_arity + vars_arity;
	    for (i = 0; i < vars_arity; i++) {
	      CELL cp_cell = *aux_ptr;
	      if (MARKED(cp_cell)) {
		UNMARK(aux_ptr);
		if (HEAP_PTR(cp_cell)) {
		  into_relocation_chain(aux_ptr, GET_NEXT(cp_cell));
		}
	      }
	      aux_ptr--;
	    }
	    for (i = 1; i < subs_arity; i++) {
	      CELL cp_cell = *--aux_ptr;
	      if (MARKED(cp_cell)) {
		UNMARK(aux_ptr);
		if (HEAP_PTR(cp_cell)) {
		  into_relocation_chain(aux_ptr, GET_NEXT(cp_cell));
		}
	      }
	    }
	  }
	}
	break;
#endif
    case _retry_c:
    case _retry_userc:
      {
	register CELL_PTR saved_reg;
	
	/* for each extra saved register */
	for (saved_reg = &(gc_B->cp_a1)+rtp->u.lds.s;
	     saved_reg < &(gc_B->cp_a1)+rtp->u.lds.s+rtp->u.lds.extra;
	     saved_reg++) {
	  CELL cp_cell = *saved_reg;
	  if (MARKED(cp_cell)) {
	    UNMARK(saved_reg);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(saved_reg, GET_NEXT(cp_cell));
	    }
	  }
	}
      }
      /* continue to clean environments and arguments */
    default:
      {
	register CELL_PTR saved_reg;

	sweep_environments(gc_B->cp_env,
			   EnvSize((CELL_PTR) (gc_B->cp_cp)),
			   EnvBMap((CELL_PTR) (gc_B->cp_cp)));

	/* for each saved register */
	for (saved_reg = &gc_B->cp_a1;
	     saved_reg < &gc_B->cp_a1 + rtp->u.ld.s;
	     saved_reg++) {
	  CELL cp_cell = *saved_reg;
	  if (MARKED(cp_cell)) {
	    UNMARK(saved_reg);
	    if (HEAP_PTR(cp_cell)) {
	      into_relocation_chain(saved_reg, GET_NEXT(cp_cell));
	    }
	  }
	}
      }
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

#ifdef TAGS_FAST_OPS
  while (RMARKED(ccur)) {
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
#else /* TAGS_FAST_OPS */
  while (RMARKED(ccur)) {
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
#endif /* TAGS_FAST_OPS */
}

#ifdef TABLING
static dep_fr_ptr gl_depfr;
#endif

static inline choiceptr
update_B_H( choiceptr gc_B, CELL *current, CELL *dest, CELL *odest) {
  /* also make the value of H in a choicepoint
     coherent with the new global
     */
  while (gc_B && current <= gc_B->cp_h) {
    if (gc_B->cp_h == current) {
      gc_B->cp_h = dest;
    } else {
      gc_B->cp_h = odest;
    }
    gc_B = gc_B->cp_b;
#ifdef TABLING
    if (gl_depfr != NULL && gc_B >= DepFr_cons_cp(gl_depfr)) {
      gc_B = DepFr_cons_cp(gl_depfr);
      gl_depfr = DepFr_next(gl_depfr);
    }
#endif
  }
    return(gc_B);
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

 

  /*
   * upward phase - scan heap from high to low, setting marked upward
   * ptrs to point to what will be the new locations of the
   * objects pointed to  
   */

#ifdef TABLING
  gl_depfr = LOCAL_top_dep_fr;
#endif
  dest = (CELL_PTR) H0 + total_marked - 1;
  for (current = H - 1; current >= H0; current--) {
    if (MARKED(*current)) {
      CELL ccell = UNMARK_CELL(*current);
      if (ccell < (CELL)AtomBase && ccell > EndSpecials && IsVarTerm(ccell)
	  ) {
	/* oops, we found a blob */
	int nofcells = (UNMARK_CELL(*current)-EndSpecials) / sizeof(CELL);
	CELL *ptr = current - nofcells ;
	CELL func = ptr[0];

	if (MARKED(func)) {
#ifdef DEBUG
	  found_marked+=nofcells;
#endif /* DEBUG */
	  gc_B = update_B_H(gc_B, current, dest, dest+1);
	  /* this one's being used */
	  /* first swap the tag so that it will be seen by the next step */
	  {
	    CELL tmp = current[0];
	    current[0] = ptr[1];
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
	gc_B = update_B_H(gc_B, current, dest, dest+1);
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
	else if (current == next)	/* cell pointing to
					 * itself */
	  *current = (*current & MBIT) | (CELL) dest;	/* no tag */
      }
      dest--;
    } else {
      in_garbage++;
    }
  }
  if (in_garbage)
    H0[0] = in_garbage;

#ifdef DEBUG
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"[GC] Upward (%d): %ld total against %ld found\n",
	       gc_calls,
	       (unsigned long int)total_marked,
	       (unsigned long int)found_marked);
  found_marked = 0;
#endif


  /*
   * downward phase - scan heap from low to high, moving marked objects
   * to their new locations & setting downward pointers to pt to new
   * locations 
   */

  dest = (CELL_PTR) H0;
  for (current = H0; current < H; current++) {
    CELL ccur = *current;
    if (MARKED(ccur)) {
      CELL uccur = UNMARK_CELL(ccur);
      if (uccur < (CELL)AtomBase && uccur > EndSpecials && IsVarTerm(uccur)) {
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
      /* next cell, please */
      dest++;
    } else {
      current += (ccur-1);
    }
  }
#ifdef DEBUG
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"[GC] Downward (%d): %ld total against %ld found\n",
	       gc_calls,
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
#endif

}

#ifdef HYBRID_SCHEME
static void
adjust_cp_hbs(void)
{
#ifdef TABLING
  dep_fr_ptr depfr = LOCAL_top_dep_fr;
#endif
  choiceptr gc_B = B;
  CELL_PTR *top = iptop-1, *base = (CELL_PTR *)H;

  while (gc_B != NULL) {
    CELL *gc_H = gc_B->cp_h;
    CELL_PTR *nbase = base;
    if (top[0] <= gc_H) {
      if (top[0] == gc_H)
	gc_B->cp_h = H0+(top-base);
      else
	gc_B->cp_h = H0+((top+1)-base);
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
      } else {
	if (nxt[0] == gc_H) {
	  gc_B->cp_h = H0+(nxt-base);
	  top = nxt;
	  break;
	} else {
	  gc_B->cp_h = H0+((nxt-base)+1);
	  top = nxt;
	  break;
	}
      } 
    }
#ifdef TABLING
    if (depfr != NULL && gc_B >= DepFr_cons_cp(depfr)) {
      gc_B = DepFr_cons_cp(depfr);
      depfr = DepFr_next(depfr);
    } else
#endif
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
    if (ccell < (CELL)AtomBase && ccell > EndSpecials && IsVarTerm(ccell)
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
      else if (current == next)	/* cell pointing to
				 * itself */
	*current = (*current & MBIT) | (CELL) (H0+(iptr-ibase));	/* no tag */
    }
  }

#ifdef DEBUG
  if (total_marked != found_marked)
    fprintf(Yap_stderr,"[GC] Upward (%d): %ld total against %ld found\n",
	       gc_calls,
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
    if (uccur < (CELL)AtomBase && uccur > EndSpecials && IsVarTerm(uccur)) {
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
    fprintf(Yap_stderr,"[GC] Downward (%d): %ld total against %ld found\n",
	       gc_calls,
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
#endif

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
#ifdef COROUTINING
  Yap_mark_all_suspended_goals();
#endif
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

#ifdef COROUTINING
static void
sweep_delays(CELL *max)
{
  CELL *ptr = (CELL *)Yap_GlobalBase;
  while (ptr < max) {
    if (MARKED(*ptr)) {
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
#ifdef COROUTINING
  sweep_delays(max);
#endif
  sweep_environments(current_env, EnvSize(curp), EnvBMap((CELL *)curp));
  sweep_choicepoints(B);
  sweep_trail(B, old_TR);
#ifdef HYBRID_SCHEME
#ifdef DEBUG
  if (total_marked != iptop-(CELL_PTR *)H && iptop < (CELL_PTR *)ASP -1024)
    fprintf(Yap_stderr,"[GC] Oops on iptop-H (%ld) vs %ld\n", (unsigned long int)(iptop-(CELL_PTR *)H), total_marked);
#endif
  if (iptop < (CELL_PTR *)ASP && 10*total_marked < H-H0) {
#ifdef INSTRUMENT_GC
    int effectiveness = (((H-H0)-total_marked)*100)/(H-H0);
    fprintf(Yap_stderr,"[GC] using pointers (%d)\n", effectiveness);
#endif
    quicksort((CELL_PTR *)H, 0, (iptop-(CELL_PTR *)H)-1);
    adjust_cp_hbs();
    icompact_heap();
  } else
#endif /* HYBRID_SCHEME */
    {
#ifdef DEBUG
#ifdef HYBID_SCHEME
      int effectiveness = (((H-H0)-total_marked)*100)/(H-H0);
      fprintf(stderr,"[GC] not using pointers (%d) ASP: %p, ip %p (expected %p) \n", effectiveness, ASP, iptop, H+total_marked);
#endif
#endif
      compact_heap();
    }
}

static Int
do_gc(Int predarity, CELL *current_env, yamop *nextop)
{
  Int		heap_cells = H-H0;
  int		gc_verbose = is_gc_verbose();
  tr_fr_ptr     old_TR;
  Int		m_time, c_time, time_start, gc_time;
#if COROUTINING
  CELL *max = (CELL *)Yap_ReadTimedVar(DelayedVars);
#else
  CELL *max = NULL;
#endif
  Int           effectiveness = 0;
  int           gc_trace = FALSE;

#if COROUTINING
  if (H0 - max < 1024+(2*NUM_OF_ATTS)) {
    if (!Yap_growglobal(&current_env)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return FALSE;
    }
  }
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
  /* sanity check: can we still do garbage_collection ? */
  if ((CELL)Yap_TrailTop & (MBIT|RBIT)) {
    /* oops, we can't */
    if (gc_verbose) {
      fprintf(Yap_stderr, "[GC] TrailTop at %p clashes with gc bits: %lx\n", Yap_TrailTop, (unsigned long int)(MBIT|RBIT));
      fprintf(Yap_stderr, "[GC] garbage collection disallowed\n");
    }
    return(0);
  }
  gc_calls++;
  if (gc_trace) {
    fprintf(Yap_stderr, "[gc]\n");
  } else if (gc_verbose) {
    fprintf(Yap_stderr, "[GC] Start of garbage collection %d:\n", gc_calls);
#ifndef EARLY_RESET
    fprintf(Yap_stderr, "[GC] no early reset in trail\n");
#endif
    fprintf(Yap_stderr, "[GC]       Global: %8ld cells (%p-%p)\n", (long int)heap_cells,H0,H);
    fprintf(Yap_stderr, "[GC]       Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    fprintf(Yap_stderr, "[GC]       Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)Yap_TrailBase),Yap_TrailBase,TR);
  }
  if (HeapTop >= Yap_GlobalBase - MinHeapGap) {
    *--ASP = (CELL)current_env;
    if (!Yap_growheap(FALSE, MinHeapGap)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    current_env = (CELL *)*ASP;
    ASP++;
  }
  time_start = Yap_cputime();
  total_marked = 0;
  discard_trail_entries = 0;
#ifdef HYBRID_SCHEME
  iptop = (CELL_PTR *)H;
#endif
  /* get the number of active registers */
  YAPEnterCriticalSection();
  old_TR = TR;
  push_registers(predarity, nextop);
  marking_phase(old_TR, current_env, nextop, max);
  m_time = Yap_cputime();
  gc_time = m_time-time_start;
  if (heap_cells)
    effectiveness = ((heap_cells-total_marked)*100)/heap_cells;
  else
    effectiveness = 0;
  if (gc_verbose) {
    fprintf(Yap_stderr, "[GC]   Mark: Recovered %ld cells of %ld (%ld%%) in %g sec\n",
	       (long int)(heap_cells-total_marked), (long int)heap_cells, (long int)effectiveness, (double)(m_time-time_start)/1000);
#ifdef INSTRUMENT_GC
    {
      int i;
      for (i=0; i<16; i++) {
	if (chain[i]) {
	  fprintf(Yap_stderr, "[GC]     chain[%d]=%lu\n", i, chain[i]);
	}
      }
      put_type_info((unsigned long int)total_marked);
      fprintf(Yap_stderr,"[GC]  %lu/%ld before and %lu/%ld after\n", old_vars, (unsigned long int)(B->cp_h-H0), new_vars, (unsigned long int)(H-B->cp_h));
      fprintf(Yap_stderr,"[GC]  %ld choicepoints\n", num_bs);
    }
#endif
  }
  time_start = m_time;
  compaction_phase(old_TR, current_env, nextop, max);
  TR = old_TR;
  pop_registers(predarity, nextop);
  TR = new_TR;
  YAPLeaveCriticalSection();
  c_time = Yap_cputime();
  if (gc_verbose) {
    fprintf(Yap_stderr, "[GC]   Compress: took %g sec\n", (double)(c_time-time_start)/1000);
  }
  gc_time += (c_time-time_start);
  tot_gc_time += gc_time;
  tot_gc_recovered += heap_cells-total_marked;
  if (gc_verbose) {
    fprintf(Yap_stderr, "[GC] GC %d took %g sec, total of %g sec doing GC so far.\n", gc_calls, (double)gc_time/1000, (double)tot_gc_time/1000);
    fprintf(Yap_stderr, "[GC]   Left %ld cells free in stacks.\n",
	       (unsigned long int)(ASP-H));
  }
  check_global();
  return(effectiveness);
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
  return(tot_gc_time);
}

static Int
p_inform_gc(void)
{
  Term tn = MkIntegerTerm(tot_gc_time);
  Term tt = MkIntegerTerm(gc_calls);
  Term ts = MkIntegerTerm((tot_gc_recovered*sizeof(CELL)));
 
  return(Yap_unify(tn, ARG2) && Yap_unify(tt, ARG1) && Yap_unify(ts, ARG3));

}


static int
call_gc(UInt gc_lim, Int predarity, CELL *current_env, yamop *nextop)
{
  UInt   gc_margin = 128;
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
    if (gc_calls < 8) 
      gc_margin <<= gc_calls;
    else {
      /* next grow linearly */
      gc_margin <<= 8;
      gc_margin *= gc_calls;
    }
  }
  if (gc_margin < gc_lim)
    gc_margin = gc_lim;
  if (gc_on) {
    effectiveness = do_gc(predarity, current_env, nextop);
    if (effectiveness > 90) {
      while (gc_margin < H-H0) 
	gc_margin <<= 1;
    }
  } else {
    effectiveness = 0;
  }
  /* expand the stack if effectiveness is less than 20 % */
  if (ASP - H < gc_margin ||
      effectiveness < 20) {
    return (Yap_growstack(gc_margin));
  }
  /*
   * debug for(save_total=1; save_total<=N; ++save_total)
   * plwrite(XREGS[save_total],Yap_DebugPutc,0); 
   */
  return ( TRUE );
}

int 
Yap_gc(Int predarity, CELL *current_env, yamop *nextop)
{
  return call_gc(128, predarity, current_env, nextop);
}

int 
Yap_gcl(UInt gc_lim, Int predarity, CELL *current_env, yamop *nextop)
{
  return call_gc(gc_lim, predarity, current_env, nextop);
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
  Yap_InitCPred("$gc", 0, p_gc, 0);
  Yap_InitCPred("$inform_gc", 3, p_inform_gc, 0);
}

void
Yap_inc_mark_variable()
{
  total_marked++;
}
