/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright R. Lopes,L.Damas, V. Santos Costa and Universidade do Porto 1985--	 *
*									 *
**************************************************************************
*									 *
* File:		gprof.c							 *
* comments:	Interrupt Driven Profiler				 *
*									 *
* Last rev:     $Date: 2008-03-26 14:37:07 $,$Author: vsc $						 *
* $Log: not supported by cvs2svn $
* Revision 1.9  2007/10/08 23:02:15  vsc
* minor fixes
*
* Revision 1.8  2007/04/10 22:13:20  vsc
* fix max modules limitation
*
* Revision 1.7  2006/08/22 16:12:45  vsc
* global variables
*
* Revision 1.6  2006/08/07 18:51:44  vsc
* fix garbage collector not to try to garbage collect when we ask for large
* chunks of stack in a single go.
*
* Revision 1.5  2006/04/27 20:58:59  rslopes
* fix do profiler offline.
*
* Revision 1.4  2006/02/01 13:28:56  vsc
* bignum support fixes
*
* Revision 1.3  2006/01/17 14:10:40  vsc
* YENV may be an HW register (breaks some tabling code)
* All YAAM instructions are now brackedted, so Op introduced an { and EndOp introduces an }. This is because Ricardo assumes that.
* Fix attvars 
*
* Revision 1.2  2005/12/23 00:20:13  vsc
* updates to gprof
* support for __POWER__
* Try to saveregs before longjmp.
*
* Revision 1.1  2005/12/17 03:26:38  vsc
* move event profiler outside from stdpreds.c
*									 *
*************************************************************************/

/// @file gprof.c

/** @addtogroup Tick_Profiler
 * @ingroup Profiling
 * @{
 * 
 * The tick profiler works by interrupting the Prolog code every so often
 * and checking at each point the code was. The pro/filer must be able to
 * retrace the state of the abstract machine at every moment. The major
 * advantage of this approach is that it gives the actual amount of time
 * being spent per procedure, or whether garbage collection dominates
 * execution time. The major drawback is that tracking down the state of
 * the abstract machine may take significant time, and in the worst case
 * may slow down the whole execution.
 * 
 * The following procedures are available:
 * 
 * + profinit/0 
 *   Initialise the data-structures for the profiler. Unnecessary for
 *   dynamic profiler.
 * 
 * + profon/0
 *   Start profiling.
 * 
 * +  profoff/0
 *    Stop profiling.
 * 
 * +  profoff/0
 *    Stop profiling.
 * 
 * +  showprofres/0 and showprofres/1
 *    Stop tick counts per predicate.
 * 
 *  
  */

#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#if defined(__x86_64__) && defined (__linux__)

#define __USE_GNU

#include <ucontext.h>

typedef greg_t context_reg;
#define CONTEXT_PC(scv) (((ucontext_t *)(scv))->uc_mcontext.gregs[14])
#define CONTEXT_BP(scv) (((ucontext_t *)(scv))->uc_mcontext.gregs[6])

#elif defined(__i386__) && defined (__linux__)


#include <ucontext.h>

typedef greg_t context_reg;
#define CONTEXT_PC(scv) (((ucontext_t *)(scv))->uc_mcontext.gregs[14])
#define CONTEXT_BP(scv) (((ucontext_t *)(scv))->uc_mcontext.gregs[6])

#elif defined(__APPLE__) && defined(__x86_64__)

#include <AvailabilityMacros.h>
#include <sys/ucontext.h>

#if !defined(MAC_OS_X_VERSION_10_5) || MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_5
#define CONTEXT_REG(r) r
#else
#define CONTEXT_REG(r) __##r
#endif

#define CONTEXT_STATE(scv) (((ucontext_t *)(scv))->uc_mcontext->CONTEXT_REG(ss))
#define CONTEXT_PC(scv) (CONTEXT_STATE(scv).CONTEXT_REG(rip))
#define CONTEXT_BP(scv) (CONTEXT_STATE(scv).CONTEXT_REG(rbp))

#elif defined(__APPLE__) && defined(__i386__)

#include <AvailabilityMacros.h>
#include <sys/ucontext.h>

#if !defined(MAC_OS_X_VERSION_10_5) || MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_5
#define CONTEXT_REG(r) r
#else
#define CONTEXT_REG(r) __##r
#endif

#define CONTEXT_STATE(scv) (((ucontext_t *)(scv))->uc_mcontext->CONTEXT_REG(ss))
#define CONTEXT_PC(scv) (CONTEXT_STATE(scv).CONTEXT_REG(eip))
#define CONTEXT_BP(scv) (CONTEXT_STATE(scv).CONTEXT_REG(ebp))
#define CONTEXT_FAULTING_ADDRESS ((char *) info->si_addr)

#else

#define CONTEXT_PC(scv) NULL
#define CONTEXT_BP(scv) NULL
#ifdef LOW_PROF
#undef LOW_PROF
#endif

#endif

#include "absmi.h"
#include <stdio.h>

#if HAVE_STRING_H
#include <string.h>
#endif

#ifdef LOW_PROF
#include <signal.h>
#include <unistd.h>
#include <sys/time.h>
#ifdef __APPLE__
#else
#ifdef UCONTEXT_H
#include <ucontext.h>
#endif
#endif



#define TIMER_DEFAULT 100
#define PROFILING_FILE 1
#define PROFPREDS_FILE 2

typedef struct {
  char tag;
  void *ptr;
}  __attribute__ ((packed)) buf_ptr;

typedef struct {
  gprof_info inf;
  void  *end;
  PredEntry *pe;
}  __attribute__ ((packed))  buf_extra;

typedef struct RB_red_blk_node {
  yamop *key; /* first address */
  yamop *lim; /* end address */
  PredEntry *pe; /* parent predicate */
  gprof_info source; /* how block was allocated */
  UInt pcs;  /* counter with total for each clause */
  int red; /* if red=0 then the node is black */
  struct RB_red_blk_node* left;
  struct RB_red_blk_node* right;
  struct RB_red_blk_node* parent;
} rb_red_blk_node;



static rb_red_blk_node *
RBMalloc(UInt size)
{
  return (rb_red_blk_node *)malloc(size);
}

static void
RBfree(rb_red_blk_node *ptr)
{
  free((char *)ptr);
}

static rb_red_blk_node *
RBTreeCreate(void) {
  rb_red_blk_node* temp;

  /*  see the comment in the rb_red_blk_tree structure in red_black_tree.h */
  /*  for information on nil and root */
  temp=GLOBAL_ProfilerNil= RBMalloc(sizeof(rb_red_blk_node));
  temp->parent=temp->left=temp->right=temp;
  temp->pcs=0;
  temp->red=0;
  temp->key=temp->lim=NULL;
  temp->pe=NULL;
  temp->source=GPROF_NO_EVENT;;
  temp = RBMalloc(sizeof(rb_red_blk_node));
  temp->parent=temp->left=temp->right=GLOBAL_ProfilerNil;
  temp->key=temp->lim=NULL;
  temp->pe=NULL;
  temp->source=GPROF_NO_EVENT;
  temp->pcs=0;
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
  rb_red_blk_node* rb_nil=GLOBAL_ProfilerNil;

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

  if (y->left != rb_nil) y->left->parent=x; /* used to use sentinel here */
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
  Assert(!GLOBAL_ProfilerNil->red,"nil not red in LeftRotate");
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
  rb_red_blk_node* rb_nil=GLOBAL_ProfilerNil;

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

  if (rb_nil != x->right)  x->right->parent=y; /*used to use sentinel here */
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
  Assert(!GLOBAL_ProfilerNil->red,"nil not red in RightRotate");
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
  rb_red_blk_node* rb_nil=GLOBAL_ProfilerNil;
  
  z->left=z->right=rb_nil;
  y=GLOBAL_ProfilerRoot;
  x=GLOBAL_ProfilerRoot->left;
  while( x != rb_nil) {
    y=x;
    if (x->key > z->key) { /* x.key > z.key */
      x=x->left;
    } else { /* x,key <= z.key */
      x=x->right;
    }
  }
  z->parent=y;
  if ( (y == GLOBAL_ProfilerRoot) ||
       (y->key > z->key)) { /* y.key > z.key */
    y->left=z;
  } else {
    y->right=z;
  }

#ifdef DEBUG_ASSERT
  Assert(!GLOBAL_ProfilerNil->red,"nil not red in TreeInsertHelp");
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
RBTreeInsert(yamop *key, yamop *lim) {
  rb_red_blk_node * y;
  rb_red_blk_node * x;
  rb_red_blk_node * newNode;

  x=(rb_red_blk_node*) RBMalloc(sizeof(rb_red_blk_node));
  x->key=key;
  x->lim=lim;

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
  GLOBAL_ProfilerRoot->left->red=0;
  return newNode;

#ifdef DEBUG_ASSERT
  Assert(!GLOBAL_ProfilerNil->red,"nil not red in RBTreeInsert");
  Assert(!GLOBAL_ProfilerRoot->red,"root not red in RBTreeInsert");
#endif
}

/***********************************************************************/
/*  FUNCTION:  RBExactQuery */
/**/
/*    INPUTS:  tree is the tree to print and q is a pointer to the key */
/*             we are searching for */
/**/
/*    OUTPUT:  returns the a node with key equal to q.  If there are */
/*             multiple nodes with key equal to q this function returns */
/*             the one highest in the tree */
/**/
/*    Modifies Input: none */
/**/
/***********************************************************************/
  
static rb_red_blk_node*
RBExactQuery(yamop* q) {
  rb_red_blk_node* x;
  rb_red_blk_node* rb_nil=GLOBAL_ProfilerNil;

  if (!GLOBAL_ProfilerRoot) return NULL;
  x=GLOBAL_ProfilerRoot->left;
  if (x == rb_nil) return NULL;
  while(x->key != q) {/*assignemnt*/
    if (x->key > q) { /* x->key > q */
      x=x->left;
    } else {
      x=x->right;
    }
    if ( x == rb_nil) return NULL;
  }
  return(x);
}


static rb_red_blk_node*
RBLookup(yamop *entry) {
  rb_red_blk_node *current;

  if (!GLOBAL_ProfilerRoot)
    return NULL;
  current = GLOBAL_ProfilerRoot->left;
  while (current != GLOBAL_ProfilerNil) {
    if (current->key <= entry && current->lim >= entry) {
      return current;
    }
    if (entry > current->key)
      current = current->right;
    else
      current = current->left;
  }
  return NULL;
}


/***********************************************************************/
/*  FUNCTION:  RBDeleteFixUp */
/**/
/*    INPUTS:  tree is the tree to fix and x is the child of the spliced */
/*             out node in RBTreeDelete. */
/**/
/*    OUTPUT:  none */
/**/
/*    EFFECT:  Performs rotations and changes colors to restore red-black */
/*             properties after a node is deleted */
/**/
/*    Modifies Input: tree, x */
/**/
/*    The algorithm from this function is from _Introduction_To_Algorithms_ */
/***********************************************************************/

static void RBDeleteFixUp(rb_red_blk_node* x) {
  rb_red_blk_node* root=GLOBAL_ProfilerRoot->left;
  rb_red_blk_node *w;

  while( (!x->red) && (root != x)) {
    if (x == x->parent->left) {
      w=x->parent->right;
      if (w->red) {
	w->red=0;
	x->parent->red=1;
	LeftRotate(x->parent);
	w=x->parent->right;
      }
      if ( (!w->right->red) && (!w->left->red) ) { 
	w->red=1;
	x=x->parent;
      } else {
	if (!w->right->red) {
	  w->left->red=0;
	  w->red=1;
	  RightRotate(w);
	  w=x->parent->right;
	}
	w->red=x->parent->red;
	x->parent->red=0;
	w->right->red=0;
	LeftRotate(x->parent);
	x=root; /* this is to exit while loop */
      }
    } else { /* the code below is has left and right switched from above */
      w=x->parent->left;
      if (w->red) {
	w->red=0;
	x->parent->red=1;
	RightRotate(x->parent);
	w=x->parent->left;
      }
      if ( (!w->right->red) && (!w->left->red) ) { 
	w->red=1;
	x=x->parent;
      } else {
	if (!w->left->red) {
	  w->right->red=0;
	  w->red=1;
	  LeftRotate(w);
	  w=x->parent->left;
	}
	w->red=x->parent->red;
	x->parent->red=0;
	w->left->red=0;
	RightRotate(x->parent);
	x=root; /* this is to exit while loop */
      }
    }
  }
  x->red=0;

#ifdef DEBUG_ASSERT
  Assert(!tree->nil->red,"nil not black in RBDeleteFixUp");
#endif
}



/***********************************************************************/
/*  FUNCTION:  TreeSuccessor  */
/**/
/*    INPUTS:  tree is the tree in question, and x is the node we want the */
/*             the successor of. */
/**/
/*    OUTPUT:  This function returns the successor of x or NULL if no */
/*             successor exists. */
/**/
/*    Modifies Input: none */
/**/
/*    Note:  uses the algorithm in _Introduction_To_Algorithms_ */
/***********************************************************************/
  
static rb_red_blk_node*
TreeSuccessor(rb_red_blk_node* x) { 
  rb_red_blk_node* y;
  rb_red_blk_node* rb_nil=GLOBAL_ProfilerNil;
  rb_red_blk_node* root=GLOBAL_ProfilerRoot;

  if (rb_nil != (y = x->right)) { /* assignment to y is intentional */
    while(y->left != rb_nil) { /* returns the minium of the right subtree of x */
      y=y->left;
    }
    return(y);
  } else {
    y=x->parent;
    while(x == y->right) { /* sentinel used instead of checking for nil */
      x=y;
      y=y->parent;
    }
    if (y == root) return(rb_nil);
    return(y);
  }
}

/***********************************************************************/
/*  FUNCTION:  RBDelete */
/**/
/*    INPUTS:  tree is the tree to delete node z from */
/**/
/*    OUTPUT:  none */
/**/
/*    EFFECT:  Deletes z from tree and frees the key and info of z */
/*             using DestoryKey and DestoryInfo.  Then calls */
/*             RBDeleteFixUp to restore red-black properties */
/**/
/*    Modifies Input: tree, z */
/**/
/*    The algorithm from this function is from _Introduction_To_Algorithms_ */
/***********************************************************************/

static void
RBDelete(rb_red_blk_node* z){
  rb_red_blk_node* y;
  rb_red_blk_node* x;
  rb_red_blk_node* rb_nil=GLOBAL_ProfilerNil;
  rb_red_blk_node* root=GLOBAL_ProfilerRoot;

  y= ((z->left == rb_nil) || (z->right == rb_nil)) ? z : TreeSuccessor(z);
  x= (y->left == rb_nil) ? y->right : y->left;
  if (root == (x->parent = y->parent)) { /* assignment of y->p to x->p is intentional */
    root->left=x;
  } else {
    if (y == y->parent->left) {
      y->parent->left=x;
    } else {
      y->parent->right=x;
    }
  }
  if (y != z) { /* y should not be nil in this case */

#ifdef DEBUG_ASSERT
    Assert( (y!=tree->nil),"y is nil in RBDelete\n");
#endif
    /* y is the node to splice out and x is its child */

    if (!(y->red)) RBDeleteFixUp(x);
  
    /* tree->DestroyKey(z->key);*/
    /*tree->DestroyInfo(z->info); */
    y->left=z->left;
    y->right=z->right;
    y->parent=z->parent;
    y->red=z->red;
    z->left->parent=z->right->parent=y;
    if (z == z->parent->left) {
      z->parent->left=y; 
    } else {
      z->parent->right=y;
    }
    RBfree(z); 
  } else {
    /*tree->DestroyKey(y->key);*/
    /*tree->DestroyInfo(y->info);*/
    if (!(y->red)) RBDeleteFixUp(x);
    RBfree(y);
  }
  
#ifdef DEBUG_ASSERT
  Assert(!tree->nil->red,"nil not black in RBDelete");
#endif
}

char *set_profile_dir(char *);
char *set_profile_dir(char *name){
    int size=0;

    if (name!=NULL) {
      size=strlen(name)+1;
      if (GLOBAL_DIRNAME!=NULL) free(GLOBAL_DIRNAME);
      GLOBAL_DIRNAME=malloc(size);
      if (GLOBAL_DIRNAME==NULL) { printf("Profiler Out of Mem\n"); exit(1); }
      strcpy(GLOBAL_DIRNAME,name);
    } 
    if (GLOBAL_DIRNAME==NULL) {
      do {
        if (GLOBAL_DIRNAME!=NULL) free(GLOBAL_DIRNAME);
        size+=20;
        GLOBAL_DIRNAME=malloc(size);
        if (GLOBAL_DIRNAME==NULL) { printf("Profiler Out of Mem\n"); exit(1); }
      } while (getcwd(GLOBAL_DIRNAME, size-15)==NULL); 
    }

return GLOBAL_DIRNAME;
}

char *profile_names(int);
char *profile_names(int k) {
  char *FNAME=NULL;
  int size=200;
   
  if (GLOBAL_DIRNAME==NULL) set_profile_dir(NULL);
  size=strlen(GLOBAL_DIRNAME)+40;
  FNAME=malloc(size);
  if (FNAME==NULL) { printf("Profiler Out of Mem\n"); exit(1); }

  if (k==PROFILING_FILE) {
    sprintf(FNAME,"%s/PROFILING_%d",GLOBAL_DIRNAME,getpid());
  } else { 
    sprintf(FNAME,"%s/PROFPREDS_%d",GLOBAL_DIRNAME,getpid());
  }

  //  printf("%s\n",FNAME);
  return FNAME;
}

void del_profile_files(void);
void del_profile_files() {
  if (GLOBAL_DIRNAME!=NULL) {
    remove(profile_names(PROFPREDS_FILE));
    remove(profile_names(PROFILING_FILE));
  }
}

void
Yap_inform_profiler_of_clause__(void *code_start, void *code_end, PredEntry *pe,gprof_info index_code) {
  buf_ptr b;
  buf_extra e;
  GLOBAL_ProfOn = TRUE;
  b.tag = '+';
  b.ptr= code_start;
  e.inf= index_code;
  e.end= code_end;
  e.pe= pe;
  fwrite(&b,sizeof(b),1,GLOBAL_FPreds);
  fwrite(&e,sizeof(e),1,GLOBAL_FPreds);
  GLOBAL_ProfOn = FALSE;
}

typedef struct clause_entry {
  yamop *beg, *end;
  PredEntry *pp;
  UInt pcs;  /* counter with total for each clause */
  UInt pca;  /* counter with total for each predicate (repeated for each clause)*/  
  int ts;    /* start end timestamp towards retracts, eventually */
} clauseentry;

static Int profend( USES_REGS1 ); 

static void
clean_tree(rb_red_blk_node* node) {
  if (node == GLOBAL_ProfilerNil)
    return;
  clean_tree(node->left);
  clean_tree(node->right);
  Yap_FreeCodeSpace((char *)node);
}

static void
reset_tree(void) {
  clean_tree(GLOBAL_ProfilerRoot);
  Yap_FreeCodeSpace((char *)GLOBAL_ProfilerNil);
  GLOBAL_ProfilerNil = GLOBAL_ProfilerRoot = NULL;
  GLOBAL_ProfCalls = GLOBAL_ProfGCs = GLOBAL_ProfHGrows = GLOBAL_ProfSGrows = GLOBAL_ProfMallocs = GLOBAL_ProfOns = 0L;
}

static int
InitProfTree(void)
{
  if (GLOBAL_ProfilerRoot) 
    reset_tree();
  while (!(GLOBAL_ProfilerRoot = RBTreeCreate())) {
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing profiler");
      return FALSE;
    }    
  }
  return TRUE;
}

static void RemoveCode(CODEADDR clau)
{
  rb_red_blk_node* x, *node;
  PredEntry *pp;
  UInt count;

  if (!GLOBAL_ProfilerRoot) return;
  if (!(x = RBExactQuery((yamop *)clau))) {
    /* send message */
    GLOBAL_ProfOn = FALSE;
    return;
  }
  pp = x->pe;
  count = x->pcs;
  RBDelete(x);
  /* use a single node to represent all deleted clauses */
  if (!(node = RBExactQuery((yamop *)(pp->OpcodeOfPred)))) {
    node = RBTreeInsert((yamop *)(pp->OpcodeOfPred), NEXTOP((yamop *)(pp->OpcodeOfPred),e));
    node->lim = (yamop *)pp;
    node->pe = pp;
    node->pcs = count;
    /* send message */
    GLOBAL_ProfOn = FALSE;
    return;
  } else {
    node->pcs += count;
  }  
}

static Int
showprofres( USES_REGS1 ) { 
  buf_ptr buf;

  profend( PASS_REGS1 ); /* Make sure profiler has ended */

  /* First part: Read information about predicates and store it on yap trail */

  InitProfTree();
  GLOBAL_ProfGCs=0;
  GLOBAL_ProfMallocs=0;
  GLOBAL_ProfHGrows=0;
  GLOBAL_ProfSGrows=0;
  GLOBAL_ProfIndexing=0;
  GLOBAL_FProf=fopen(profile_names(PROFILING_FILE),"r"); 
  if (GLOBAL_FProf==NULL) { fclose(GLOBAL_FProf); return FALSE; }
  while (fread(&buf, sizeof(buf), 1, GLOBAL_FProf)) {
    switch (buf.tag) {
    case '+':
      {
	rb_red_blk_node *node;
	buf_extra e;

	if (fread(&e,sizeof(buf_extra),1,GLOBAL_FProf) == 0)
	  return FALSE;;
	node = RBTreeInsert(buf.ptr, e.end);
	node->pe = e.pe;
	node->source = e.inf;
	node->pcs = 0;
      }
      break;
    case '?':
      {
	prolog_exec_mode md;

	md = (prolog_exec_mode)buf.ptr;
	if (md & GCMode) {
	  GLOBAL_ProfGCs++;
	} else if (md & MallocMode) {
	  GLOBAL_ProfMallocs++;
	} else if (md & GrowHeapMode) {
	  GLOBAL_ProfHGrows++;
	} else if (md & GrowStackMode) {
	  GLOBAL_ProfSGrows++;
	}
      }
      break;
    case '-':
      RemoveCode(buf.ptr);
      break;
    default:
      {
	rb_red_blk_node *node;

	node = RBLookup(buf.ptr);
	if (!node) {
#if DEBUG
	  fprintf(stderr,"Oops: %p\n", buf.ptr);
#endif
	} else {
	  switch(node->source) {
	  case GPROF_INDEX:
	  case GPROF_INDEX_EXPAND:
	  case GPROF_LU_INDEX:
	  case GPROF_STATIC_INDEX:
	  case GPROF_INIT_EXPAND:
	  case GPROF_INIT_LOG_UPD_CLAUSE:
	  case GPROF_NEW_LU_SWITCH:
	  case GPROF_NEW_STATIC_SWITCH:
	  case GPROF_NEW_EXPAND_BLOCK:
	    GLOBAL_ProfIndexing++;
	    break;
	  default:
	    break;
	  }
	  node->pcs++; 
	}
      }
    }
  }
  fclose(GLOBAL_FProf);
  if (GLOBAL_ProfCalls==0) 
    return TRUE;
  return TRUE;
}


#define TestMode (GCMode | GrowHeapMode | GrowStackMode | ErrorHandlingMode | InErrorMode | AbortMode | MallocMode)


static void
prof_alrm(int signo, siginfo_t *si, void *scv)
{ 
  CACHE_REGS
  void * oldpc;
  yamop *current_p;
  buf_ptr b;

  GLOBAL_ProfCalls++;
  /* skip an interrupt */
  if (GLOBAL_ProfOn) {
    GLOBAL_ProfOns++;
    return;
  }
  GLOBAL_ProfOn = TRUE;
  oldpc = (void *) CONTEXT_PC(scv);
  if (LOCAL_PrologMode & TestMode) {

    b.tag = '?';
    b.ptr= (void *)LOCAL_PrologMode;
    fwrite(&b,sizeof(b),1,GLOBAL_FPreds);
    GLOBAL_ProfOn = FALSE;
    return;
  }
  
  if (oldpc>(void *) &Yap_absmi && oldpc <= (void *) &Yap_absmiEND) { 
    CACHE_REGS
    /* we are running emulator code */
#if BP_FREE
    current_p =(yamop *) CONTEXT_BP(scv);
#else
    current_p = P;
#endif
  } else {
    CACHE_REGS
    op_numbers oop = Yap_op_from_opcode(PREVOP(P,Osbpp)->opc);
    
    if (oop == _call_cpred || oop == _call_usercpred) {
      /* doing C-code */
      current_p = PREVOP(P,Osbpp)->y_u.Osbpp.p->CodeOfPred;
    } else if ((oop = Yap_op_from_opcode(P->opc)) == _execute_cpred) {
      /* doing C-code */
      current_p = P->y_u.Osbpp.p->CodeOfPred;
    } else {
      current_p = P;
    }
  }

#if !USE_SYSTEM_MALLOC
  if (P < (yamop *)Yap_HeapBase || P > (yamop *)HeapTop) {
#if DEBUG
    fprintf(stderr,"Oops: %p, %p\n", oldpc, current_p);
#endif
    GLOBAL_ProfOn = FALSE;
    return;
  }
#endif

  b.tag = '.';
  b.ptr= current_p;
  fwrite(&b,sizeof(b),1,GLOBAL_FPreds);
  GLOBAL_ProfOn = FALSE;
}


void
Yap_InformOfRemoval(void *clau)
{
  GLOBAL_ProfOn = TRUE;
  if (GLOBAL_FPreds != NULL) {
    /* just store info about what is going on  */
    buf_ptr b;
    
    b.tag = '-';
    b.ptr= clau;
    fwrite(&b,sizeof(b),1,GLOBAL_FPreds);
    GLOBAL_ProfOn = FALSE;
    return;
  }
  GLOBAL_ProfOn = FALSE;
}

static Int profend( USES_REGS1 ); 

static Int
profnode( USES_REGS1 ) {
  Term t1 = Deref(ARG1), tleft, tright;
  rb_red_blk_node *node;

  if (!GLOBAL_ProfilerRoot)
    return FALSE;
  if (!(node = (rb_red_blk_node *)IntegerOfTerm(t1)))
    node = GLOBAL_ProfilerRoot;
  /*
    if (node->key)
    fprintf(stderr,"%p: %p,%p,%d,%p(%d),%p,%p\n",node,node->key,node->lim,node->pcs,node->pe,node->pe->ArityOfPE,node->right,node->left);
  */
  if (node->left == GLOBAL_ProfilerNil) {
    tleft = TermNil;
  } else {
    tleft = MkIntegerTerm((Int)node->left);
  }
  if (node->left == GLOBAL_ProfilerNil) {
    tleft = TermNil;
  } else {
    tleft = MkIntegerTerm((Int)node->left);
  }
  if (node->right == GLOBAL_ProfilerNil) {
    tright = TermNil;
  } else {
    tright = MkIntegerTerm((Int)node->right);
  }
  return 
    Yap_unify(ARG2,MkIntegerTerm((Int)node->key)) &&
    Yap_unify(ARG3,MkIntegerTerm((Int)node->pe)) &&
    Yap_unify(ARG4,MkIntegerTerm((Int)node->pcs)) &&
    Yap_unify(ARG5,tleft) &&
    Yap_unify(ARG6,tright);
}

static Int
profglobs( USES_REGS1 ) {
  return 
    Yap_unify(ARG1,MkIntegerTerm(GLOBAL_ProfCalls)) &&
    Yap_unify(ARG2,MkIntegerTerm(GLOBAL_ProfGCs)) &&
    Yap_unify(ARG3,MkIntegerTerm(GLOBAL_ProfHGrows)) &&
    Yap_unify(ARG4,MkIntegerTerm(GLOBAL_ProfSGrows)) &&
    Yap_unify(ARG5,MkIntegerTerm(GLOBAL_ProfMallocs)) &&
    Yap_unify(ARG6,MkIntegerTerm(GLOBAL_ProfIndexing)) &&
    Yap_unify(ARG7,MkIntegerTerm(GLOBAL_ProfOns)) ;
}

static Int
do_profinit( USES_REGS1 )
{
  //    GLOBAL_FPreds=fopen(profile_names(PROFPREDS_FILE),"w+"); 
  // if (GLOBAL_FPreds == NULL) return FALSE;
  GLOBAL_FProf=fopen(profile_names(PROFILING_FILE),"w+"); 
  if (GLOBAL_FProf==NULL) { fclose(GLOBAL_FProf); return FALSE; }
  GLOBAL_FPreds = GLOBAL_FProf;

  Yap_dump_code_area_for_profiler();
  return TRUE;
}

static Int profinit( USES_REGS1 )
{
  if (GLOBAL_ProfilerOn!=0) return (FALSE);
  
  if (!do_profinit( PASS_REGS1 ))
    return FALSE;

  GLOBAL_ProfilerOn = -1; /* Inited but not yet started */
  return(TRUE);
}

static Int start_profilers(int msec)
{
  struct itimerval t;
  struct sigaction sa;
  
  if (GLOBAL_ProfilerOn!=-1) {
    return FALSE; /* have to go through profinit */
  }
  sa.sa_sigaction=prof_alrm;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags=SA_SIGINFO;
  if (sigaction(SIGPROF,&sa,NULL)== -1) return FALSE;
//  if (signal(SIGPROF,prof_alrm) == SIG_ERR) return FALSE;

  t.it_interval.tv_sec=0;
  t.it_interval.tv_usec=msec;
  t.it_value.tv_sec=0;
  t.it_value.tv_usec=msec;
  setitimer(ITIMER_PROF,&t,NULL);

  GLOBAL_ProfilerOn = msec;
  return TRUE;
}


static Int profoff( USES_REGS1 ) {
  if (GLOBAL_ProfilerOn>0) {
    struct itimerval t;
    t.it_interval.tv_sec=0;
    t.it_interval.tv_usec=0;
    t.it_value.tv_sec=0;
    t.it_value.tv_usec=0;

    setitimer(ITIMER_PROF,&t,NULL);
    GLOBAL_ProfilerOn = -1;
    return TRUE;
  }
  return FALSE;
}

static Int ProfOn( USES_REGS1 ) { 
  Term p;
  profoff( PASS_REGS1 );
  p=Deref(ARG1);
  return(start_profilers(IntOfTerm(p)));
}

static Int ProfOn0( USES_REGS1 ) { 
  profoff( PASS_REGS1 );
  return(start_profilers(TIMER_DEFAULT));
}

static Int profison( USES_REGS1 ) {
  return (GLOBAL_ProfilerOn > 0);
}

static Int profalt( USES_REGS1 ) { 
  if (GLOBAL_ProfilerOn==0) return(FALSE);
  if (GLOBAL_ProfilerOn==-1) return ProfOn( PASS_REGS1 );
  return profoff( PASS_REGS1 );
}

static Int profend( USES_REGS1 ) 
{
  if (GLOBAL_ProfilerOn==0) return(FALSE);
  profoff( PASS_REGS1 );         /* Make sure profiler is off */
  GLOBAL_ProfilerOn=0;
  fclose(GLOBAL_FProf);
  GLOBAL_FPreds = NULL;
  return TRUE;
}

static Int getpredinfo( USES_REGS1 ) 
{
  PredEntry *pp = (PredEntry *)IntegerOfTerm(Deref(ARG1));
  Term mod, name;
  UInt arity;

  if (!pp)
    return FALSE;
  if (pp->ModuleOfPred == PROLOG_MODULE)
    mod = TermProlog;
  else
    mod = pp->ModuleOfPred;
  if (pp->ModuleOfPred == IDB_MODULE) {
    if (pp->PredFlags & NumberDBPredFlag) {
      arity = 0;
      name = MkIntegerTerm(pp->src.IndxId);
    } else  if (pp->PredFlags & AtomDBPredFlag) {
      arity = 0;
      name = MkAtomTerm((Atom)pp->FunctorOfPred);
    } else {
      name = MkAtomTerm(NameOfFunctor(pp->FunctorOfPred));
      arity = ArityOfFunctor(pp->FunctorOfPred);
    }
  } else {
    arity = pp->ArityOfPE;
    if (pp->ArityOfPE) {
      name = MkAtomTerm(NameOfFunctor(pp->FunctorOfPred));
    } else {
      name = MkAtomTerm((Atom)(pp->FunctorOfPred));
    }  
  }
  return Yap_unify(ARG2, mod) &&
    Yap_unify(ARG3, name) &&
    Yap_unify(ARG4, MkIntegerTerm(arity));
}

static Int profres0( USES_REGS1 ) { 
  return(showprofres( PASS_REGS1 ));
}

#endif /* LOW_PROF */

void
Yap_InitLowProf(void)
{
#if LOW_PROF
  GLOBAL_ProfCalls = 0;
  GLOBAL_ProfilerOn = FALSE;

  Yap_InitCPred("profinit",0, profinit, SafePredFlag);
  Yap_InitCPred("profend" ,0, profend, SafePredFlag);
  Yap_InitCPred("profon" , 0, ProfOn0, SafePredFlag);
  Yap_InitCPred("profoff", 0, profoff, SafePredFlag);
  Yap_InitCPred("profalt", 0, profalt, SafePredFlag);
  Yap_InitCPred("$offline_showprofres", 0, profres0, SafePredFlag);
  Yap_InitCPred("$profnode", 6, profnode, SafePredFlag);
  Yap_InitCPred("$profglobs", 7, profglobs, SafePredFlag);
  Yap_InitCPred("$profison",0 , profison, SafePredFlag);
  Yap_InitCPred("$get_pred_pinfo", 4, getpredinfo, SafePredFlag);
  Yap_InitCPred("showprofres", 4, getpredinfo, SafePredFlag);
#endif
}


/**
@}
*/
