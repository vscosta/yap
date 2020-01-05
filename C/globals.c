/**
   * @file   globals.c
   * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
   * @date   Tue Nov 17 23:16:17 2015
   *
   * @brief  support for backtrable and non-backtrackable variables in Prolog.
   *
   *
   */
#define DEB_DOOBIN(d0) (fprintf(stderr, "+++ %s ",__FUNCTION__) , Yap_DebugPlWriteln(d0))
#define DEB_DOOBOUT(d0) (fprintf(stderr, "--- ") , Yap_DebugPlWriteln(d0))
#define DEB_DOOB(S) (fprintf(stderr, "%s %ld %p=%p %p--%d\n ",S,to_visit-to_visit0,pt0,  ptf, *AbsAppl(pt0), arity) ) //, Yap_DebugPlWriteln(d0))
/**

   @defgroup Global_Variables Global Variables
   @ingroup builtins
   @{

   Global variables are associations between names (atoms) and
   terms. They differ in various ways from storing information using
   assert/1 or recorda/3.

   + The value lives on the Prolog (global) stack. This implies that
   lookup time is independent from the size of the term. This is
   particularly interesting for large data structures such as parsed XML
   documents or the CHR global constraint store.

   + They support both global assignment using nb_setval/2 and
   backtrackable assignment using b_setval/2.

    + Only one value (which can be an arbitrary complex Prolog term)
   can be associated to a variable at a time.

   + Their value cannot be shared among threads. Each thread has its own
   namespace and values for global variables.


   Currently global variables are scoped globally. We may consider module
   scoping in future versions.   Both b_setval/2 and
   nb_setval/2 implicitly create a variable if the referenced name
   does not already refer to a variable.

   Global variables may be initialized from directives to make them
   available during the program lifetime, but some considerations are
   necessary for saved-states and threads. Saved-states to not store
   global variables, which implies they have to be declared with
   initialization/1 to recreate them after loading the saved
   state. Each thread has its own set of global variables, starting with
   an empty set. Using `thread_initialization/1` to define a global
   variable it will be defined, restored after reloading a saved state
   and created in all threads that are created after the
   registration. Finally, global variables can be initialized using the
   exception hook called exception/3. The latter technique is used
   by CHR.

   SWI-Prolog global variables are associations between names (atoms) and
   terms.  They differ in various ways from storing information using
   assert/1 or recorda/3.

   + The value lives on the Prolog (global) stack.  This implies
   that lookup time is independent from the size of the term.
   This is particulary interesting for large data structures
   qqqsuch as parsed XML documents or the CHR global constraint
   store.

   They support both global assignment using nb_setval/2 and
   backtrackable assignment using b_setval/2.

   + Only one value (which can be an arbitrary complex Prolog
   term) can be associated to a variable at a time.

   + Their value cannot be shared among threads.  Each thread
   has its own namespace and values for global variables.

   + Currently global variables are scoped globally.  We may
   consider module scoping in future versions.


   Both b_setval/2 and nb_setval/2 implicitly create a variable if the
   referenced name does not already refer to a variable.

   Global variables may be initialized from directives to make them
   available during the program lifetime, but some considerations are
   necessary for saved-states and threads. Saved-states to not store global
   variables, which implies they have to be declared with initialization/1
   to recreate them after loading the saved state.  Each thread has
   its own set of global variables, starting with an empty set.  Using
   `thread_inititialization/1` to define a global variable it will be
   defined, restored after reloading a saved state and created in all
   threads that are created <em>after</em> the registration.


*/

#ifndef GLOBALS_C
#define GLOBALS_C 1

    #include "Yap.h"
#include "YapEval.h"
#include "YapHeap.h"
#include "Yatom.h"
#include "attvar.h"
#include "clause.h"
#include "heapgc.h"
#include "iopreds.h"
#include "yapio.h"
#include <math.h>

#include "heapgc.h"

#ifdef HAVE_STRING_H
#include "string.h"
#endif

  
/* Non-backtrackable terms will from now on be stored on arenas, a
   special term on the heap. Arenas automatically contract as we add terms to
   the front.

*/

#define QUEUE_FUNCTOR_ARITY 4

#define QUEUE_ARENA 0
#define QUEUE_HEAD 1
#define QUEUE_TAIL 2
#define QUEUE_SIZE 3
 
#define HEAP_FUNCTOR_MIN_ARITY

#define HEAP_SIZE 0
#define HEAP_MAX 1
#define HEAP_ARENA 2
#define HEAP_START 3

/// A cell_space is a short code region, where we want bindings to proceed
/// locally. It is used in copy_term
///
typedef struct cell_space {
  struct cell_space *parent; //`
  CELL *oASP, *oH, *oHB;
  CELL *arenaB, *arenaL;
} cell_space_t;


#define MIN_ARENA_SIZE (1048L)

#define MAX_ARENA_SIZE (2048 * 16)

#define Global_MkIntegerTerm(I) MkIntegerTerm(I)

static UInt big2arena_sz(CELL *arena_base) {
  return (((MP_INT *) (arena_base + 2))->_mp_alloc * sizeof(mp_limb_t) +
	  sizeof(MP_INT) + sizeof(Functor) + 2 * sizeof(CELL)) /
    sizeof(CELL);
}

static UInt arena2big_sz(UInt sz) {
  return sz -
    (sizeof(MP_INT) + sizeof(Functor) + 2 * sizeof(CELL)) / sizeof(CELL);
}

/* pointer to top of an arena */
static inline CELL *ArenaLimit(Term arena) {
  CELL *arena_base = RepAppl(arena);
  UInt sz = big2arena_sz(arena_base);
  return arena_base + sz;
}

/* pointer to top of an arena */
CELL *Yap_ArenaLimit(Term arena) { return ArenaLimit(arena); }

/* pointer to top of an arena */
static inline CELL *ArenaPt(Term arena) { return (CELL *) RepAppl(arena); }

static inline UInt ArenaSz(Term arena) { return big2arena_sz(RepAppl(arena)); }

static Term CreateNewArena(CELL *ptr, UInt size) {
  Term t = AbsAppl(ptr);
  MP_INT *dst;

  ptr[0] = (CELL) FunctorBigInt;
  ptr[1] = EMPTY_ARENA;
  dst = (MP_INT *) (ptr + 2);
  dst->_mp_size = 0L;
  dst->_mp_alloc = (sizeof(CELL) / sizeof(mp_limb_t)) * arena2big_sz(size);
  ptr[size - 1] = EndSpecials;
  return t;
}

static inline void enter_cell_space(cell_space_t *cs, Term *arenap) {
  cs->oH = HR;
  cs->oHB = HB;
  cs->oASP = ASP;
  if (!arenap || *arenap == TermNil) {
    HB = HR;
    cs->arenaL = cs->arenaB = NULL;
  } else {
    cs->arenaL = ASP = ArenaLimit(*arenap);
    cs->arenaB = HR = HB = ArenaPt(*arenap);
  }
}

static inline void exit_cell_space(cell_space_t *cs) {
  if (cs->arenaL) {
    HR = cs->oH;
  }
  HB = cs->oHB;
  ASP = cs->oASP;
}

static Term NewArena(UInt size, UInt arity, CELL *where, int wid) {
  Term t;
  UInt new_size;
  WORKER_REGS(wid)
    if (where == NULL || where == HR) {
      while (HR + size > ASP - 2 * MIN_ARENA_SIZE) {
	if (!Yap_dogc(0, NULL PASS_REGS)) {
	  Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	  return 0;
	}
      }

      t = CreateNewArena(HR, size);
      HR += size;
      new_size = size;
    } else {
      while (HR + size > ASP - 2 * MIN_ARENA_SIZE) {
	if ((new_size = Yap_InsertInGlobal(where, size * sizeof(CELL))) == 0) {
	  Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
			 "No Stack Space for Non-Backtrackable terms");
	  return 0;
	}
      }
      size = new_size / sizeof(CELL);
      t = CreateNewArena(where, size);
    }
  return t;
}

static Int p_allocate_arena(USES_REGS1) {
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "allocate_arena");
    return FALSE;
  } else if (!IsIntegerTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER, t, "allocate_arena");
    return FALSE;
  }

  return Yap_unify(ARG2, NewArena(IntegerOfTerm(t), 1, NULL, worker_id));
}

static Int p_default_arena_size(USES_REGS1) {
  return Yap_unify(ARG1, MkIntegerTerm(ArenaSz(LOCAL_GlobalArena)));
}

void Yap_AllocateDefaultArena(size_t gsize, int wid, void *cs) {
  REMOTE_GlobalArena(wid) = NewArena(gsize, 2, cs, wid);
}

#if 0
static void adjust_cps(UInt size USES_REGS) {
  /* adjust possible back pointers in choice-point stack */
  choiceptr b_ptr = B;
  while (b_ptr->cp_h == HR) {
    b_ptr->cp_h += size;
    b_ptr = b_ptr->cp_b;
  }
}
#endif

static Term GrowArena(Term arena, size_t size, UInt arity,
                      cell_space_t *cspace USES_REGS) {
  if (arena == TermNil) {
  }
  size_t old_size = ArenaSz(arena);
  LOCAL_ArenaOverflows++;
  if (size == 0) {
    if (old_size < 128 * 1024) {
      size = old_size;
    } else {
      size = old_size + 128 * 1024;
    }
  }
  if (size < 4 * MIN_ARENA_SIZE) {
    size = 4 * MIN_ARENA_SIZE;
  }
  CELL *pt;
  pt = ArenaLimit(arena);
  if (pt == HR) {
    choiceptr bp = B;
    while (bp && bp->cp_h == HR) {
      bp->cp_h += size;
      bp = bp->cp_b;
    }
    HR += size;
  } else {
    yhandle_t sla = Yap_PushHandle(arena);
    if ((size = Yap_InsertInGlobal(pt, size * sizeof(CELL)) / sizeof(CELL))
	== 0) {
      return 0;
    }
    arena = Yap_PopHandle(sla);
  }
  arena = CreateNewArena(RepAppl(arena), size + old_size);
  cspace->arenaB = ArenaPt(arena);
  cspace->arenaL = ArenaLimit(arena);

  return arena;
}

CELL *Yap_GetFromArena(Term *arenap, size_t cells, UInt arity) {
  CACHE_REGS
    Term arena = *arenap;
  CELL *max = ArenaLimit(arena);
  CELL *base = ArenaPt(arena);
  size_t old_sz = ArenaSz(arena), new_size;
  if (base + cells > max - MIN_ARENA_SIZE) {
    yhandle_t slt = Yap_PushHandle(arena);
    size_t extra_size = MIN_ARENA_SIZE * 8;
    if ((extra_size = Yap_InsertInGlobal(max, extra_size * 2 * sizeof(CELL))) ==
	0) {
      Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
		     "No Stack Space for Non-Backtrackable terms");
    }
    arena = Yap_PopHandle(slt);
    base = ArenaPt(arena);
    old_sz += extra_size;
  }
  if (arenap) {
    new_size = old_sz - cells;
    *arenap = CreateNewArena(base + cells, new_size);
  }
  return base;
}

static Term CloseArena(cell_space_t *region USES_REGS) {
  Term arena = TermNil;
  if (region->arenaB) {
    UInt new_size;
    new_size = ASP - HR;
    arena = CreateNewArena(HR, new_size);
  }
  exit_cell_space(region);
  return arena;
}

/**
 *
 * THis is the key function to copy term, parameters are:
 *
 * @param pt0 - pointer to start input - 1
 * @param pt0_end - pointer to end input -1
 * @param share - share duplicate code, instead of performing multiple copies
 * @param copy_att_vars - copy attributes on variables
 * @param ptf - poiter to destination
 * @param bindp - list of bindings introduce to break cycles in the original term.
 * If *bindp == TermFoundVar rewrite any loops as a singleconstant.
 * @param HLow
 * @return bool
 *
 * The routine works by marking compund terms the first time they are visited. If the term is
 * a listm it marks the head of the list, otherwise it marks the functor. The mderef routines then
 * do marker-aware dereferencing.
 */
static int copy_complex_term(CELL *pt0_,  CELL *pt0_end_,
                             bool share, bool copy_att_vars,
                             CELL **ptf_,
                             Term *bindp,
			     CELL *arenap,
			     cell_space_t *cspace USES_REGS) {
  // allocate space for internal stack, so that we do not have yo rely on
  //m C-stack.

  CELL *pt0=pt0_,*ptf=*ptf_,*pt0_end=pt0_end_;

  Term bind0 =  0;
  Ystack_t stt;
  volatile size_t sz = 1024*16;
  bool hack = bindp && IsIntTerm(*bindp);
  CELL *HB0;
  bool forest = bindp && (*bindp != TermFoundVar);
  bool singletons = hack;
  size_t restarts = 0;
  Int numb = 0;
  tr_fr_ptr TR0;
   CELL *HLow;
   bool ground;
   Term myt;
   if (bindp && forest) bind0 = *bindp;
   Term t = TermNil;
   //share |= hack;
init_stack(&stt, sz);
 loop:
 HLow = *ptf_;
 TR0 = TR;
 HB0 = HB;
   ground = true;
   myt = (CELL)HLow;
   HR=HLow+1;
  ptf--; // synch with pt0;
  do {
    while (pt0 < pt0_end) {
      CELL d0, dd0;
      CELL *ptd0;
      // next cell
      ++pt0;
      ++ptf;
      ptd0 = pt0;
      // notice that this is the true value of d0
      dd0 = *ptd0;
    list_loop:
      //	DEB_DOOB("enter");
      mderef_head(d0, dd0, copy_term_unk);
    copy_term_nvar :
      if (IsPairTerm(d0)) {
	CELL *ptd1 = RepPair(d0);

	///> found infinite loop.
	/// p0 is the original sub-term = ...,p0,....
	/// ptd0 is the derefereed version of d0
	/// 
	/// 
	if (IS_VISIT_MARKER(*ptd1)) {
      /* d0 has ance   */	 
	  if (hack) {
	    Term ups = MkIntTerm( to_visit-VISIT_ENTRY(*ptd1));
	    *ptf = Yap_MkApplTerm(FunctorUp,1, &ups);
	  } else if (forest) {
	    // set up a binding PTF=D0
     struct cp_frame *entry =  VISIT_ENTRY(*ptd1);
     Term val = entry->t;
     if (IsVarTerm(val)) {
       *ptf = val;
     } else {
	    // set up a binding PTF=D0
	    Term l = AbsAppl(HR);
            RESET_VARIABLE(ptf);
	    HR[0] = (CELL)FunctorEq;
	    entry->t = HR[1] = (CELL)ptf;
	    HR[2] = val;
	    HR+=3;
	    *bindp=MkPairTerm(l,*bindp);
     } }
	  else {
	    *ptf =  VISIT_TARGET(*ptd1);
     }
	  continue;
	      
	}
	   
	if (share && ptd0 >= HB) {
	  // d0 is from copy, so just use it. Note that this allows
	  // copying rational trees, even if we don break cycles.
	  *ptf = dd0;
	  continue;
	      
	}

	// first time we meet,
	// save state

	if (share) {
	  d0 = AbsPair(ptf);
	  mTrailedMaBind(ptd0,d0);
	}
	myt = AbsPair(HR);
    if (stt.pt + 32 >= stt.max) {
      goto aux_overflow;
    }
	to_visit->pt0 = pt0;
	to_visit->pt0_end = pt0_end;
	to_visit->ptf = ptf;
	to_visit->ground = ground;
	to_visit->oldp = ptd1;
	to_visit->oldv = VISIT_UNMARK(*ptd1);
	to_visit->t = myt;
	dd0 = (*ptd1);
	*ptd1 = VISIT_MARK();
	*ptf = AbsPair(HR);
	/*  the system into thinking we had a variable there */
	     
	ptf = HR;
	to_visit++;
	ground = true;
	pt0 = ptd1;
	pt0_end = ptd1 + 1;
	HR += 2;
	if (HR > ASP - MIN_ARENA_SIZE) {
	  //same as before
	  goto overflow;
	}
       goto list_loop;
      } else if (IsApplTerm(d0)) {
	CELL *ptd1 = RepAppl(d0);
	CELL tag = *ptd1;
	
	if (share && ptd1 > HB) {
	  /* If this is newer than the current term, just reuse */
	  *ptf = dd0;
	  continue;
	}

	if (IS_VISIT_MARKER(tag)) {
	  /* If this is newer than the current term, just reuse */
	  if (hack) {
	    char s[64];
	    snprintf(s,63,"__^%ld__", to_visit-VISIT_ENTRY(*ptd1));
	    *ptf = MkStringTerm(s);
	continue;
	  } else if (forest) {
	    // set up a binding PTF=D0
     struct cp_frame *entry =  VISIT_ENTRY(*ptd1);
     Term val = entry->t;
     if (IsVarTerm(val)) {
       *ptf = val;
     } else {
	    // set up a binding PTF=D0
	    Term l = AbsAppl(HR);
            RESET_VARIABLE(ptf);
	    HR[0] = (CELL)FunctorEq;
	    entry->t = HR[1] = (CELL)ptf;
	    HR[2] = val;
	    HR+=3;
	    *bindp = MkPairTerm(l, *bindp);
      }
	  } else {
	    //same as before
	    *ptf = (VISIT_TARGET(*ptd1));
	  }
	  continue;
	} else if (IsExtensionFunctor((Functor)tag) && tag!=(CELL)FunctorAttVar) {
	    switch (tag) {
	  case (CELL) FunctorDBRef:
	    *ptf = d0;
	  case (CELL) FunctorAttVar:
	    break;
	  case (CELL) FunctorLongInt:
	    if (HR > ASP - (MIN_ARENA_SIZE + 3)) {
	      goto overflow;
	    }
	    *ptf = AbsAppl(HR);
	    HR[0] = tag;
	    HR[1] = ptd1[1];
	    HR[2] = EndSpecials;
	    HR += 3;
	    if (HR > ASP - MIN_ARENA_SIZE) {
	      goto overflow;
	    }
	    break;
	  case (CELL) FunctorDouble:
	    if (HR >
		ASP - (MIN_ARENA_SIZE + (2 + SIZEOF_DOUBLE / sizeof(CELL)))) {
	      goto overflow;
	    }
	    *ptf = AbsAppl(HR);
	    HR[0] = tag;
	    HR[1] = ptd1[1];
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
	    HR[2] = ptd1[2];
	    HR[3] = EndSpecials;
	    HR += 4;
#else
	    HR[2] = EndSpecials;
	    HR += 3;
#endif
	    break;
	  case (CELL) FunctorString:
	    if (ASP - HR < MIN_ARENA_SIZE + 3 + ptd1[1]) {
	      goto overflow;
	    }
	    *ptf = AbsAppl(HR);
	    memmove(HR, ptd1, sizeof(CELL) * (3 + ptd1[1]));
	    HR += ptd1[1] + 3;
	    break;
	  default: {

	    /* big int */
	    UInt sz = (sizeof(MP_INT) + 3 * CellSize +
		       ((MP_INT *) (ptd1 + 2))->_mp_alloc * sizeof(mp_limb_t)) /
	      CellSize,
	      i;

	    if (HR > ASP - (MIN_ARENA_SIZE + sz)) {
	      goto overflow;
	    }
	    *ptf = AbsAppl(HR);
	    HR[0] = tag;
	    for (i = 1; i < sz; i++) {
	      HR[i] = ptd1[i];
	    }
	    HR += sz;
	  }
	  }
	  continue;
	} else {
	  myt = *ptf = AbsAppl(HR);
	  Functor f = (Functor)*ptd1;
	  arity_t arity;
	  if (f == FunctorDollarVar) {
	    *ptf = d0;
	    if (singletons) {
	      CELL *vt = RepAppl(d0);
	      if (vt[1]== MkIntTerm(-1))
	      vt[1] = MkIntTerm(numb++);
	    }
	      continue;
	  } else
	    arity = ArityOfFunctor(f);
	  if (f == FunctorAttVar)
	    arity = 3;
	  else
	    arity = ArityOfFunctor(f);
	  if (share) {
	    d0 = AbsAppl(ptf);
	    TrailedMaBind(ptd0, d0);
	  } 
	  /* store the terms to visit */
	  if (to_visit + 32 >= to_visit_end) {
	    goto aux_overflow;
	  }
	  to_visit->pt0 = pt0;
	  to_visit->pt0_end = pt0_end;
	  to_visit->ptf = ptf;
	  to_visit->t = myt;
	  to_visit->ground = ground;
	  to_visit->oldp = ptd1;
	  to_visit->oldv = VISIT_UNMARK(*ptd1);
	  *ptd1 = VISIT_MARK();
	  to_visit++;
	  ground = (f != FunctorMutable);
	  pt0 = ptd1;
	  pt0_end = ptd1 + arity;
	  /* store the functor for the new term */
	  HR[0] = (CELL) f;
	  ptf = HR;
	  HR += arity + 1;
	  if (HR > ASP - MIN_ARENA_SIZE) {
	    goto overflow;
	  }
        }
      } else {
	/* just copy atoms or integers */
	*ptf = d0;
      }
      continue;
      
      mderef_body(d0,dd0,ptd0,copy_term_unk, copy_term_nvar);
      ground = FALSE;
      /* don't need to copy variables if we want to share the global term */
      if (copy_att_vars && GlobalIsAttachedTerm((CELL) ptd0)) {
	/* if unbound, call the standard copy term routine */
	struct cp_frame *bp;

	if (true||! GLOBAL_attas[ExtFromCell(ptd0)].copy_term_op) {
	  d0 = AbsAppl(ptd0);
	  CELL *ptd1 = (CELL*)RepAttVar(ptd0);
        /* store the terms to visit */
        if (to_visit + 32 >= to_visit_end) {
            goto aux_overflow;
        }
        *HR++ = (CELL)FunctorAttVar;
                *ptf = (CELL)(HR+1);
        to_visit->pt0 = pt0;
        to_visit->pt0_end = pt0_end;
        to_visit->ptf = ptf;
        to_visit->t = myt;
        to_visit->ground = ground;
        to_visit->oldp = ptd1;
        to_visit->oldv = VISIT_UNMARK(*ptd1);
        *ptd1 = VISIT_MARK();
        to_visit++;
        ground = false;
        pt0 = ptd1+1;
        RESET_VARIABLE(HR+1);
        ptf = HR+2;
        HR+=4;
        pt0_end = ptd1 + 3;
        continue;
	} else {
	  bp = to_visit;
	  if (!GLOBAL_attas[ExtFromCell(ptd0)].copy_term_op(ptd0, &bp, ptf PASS_REGS)) {
	    goto overflow;
	  }
	  to_visit = bp;
	}
	if (TR > (tr_fr_ptr) LOCAL_TrailTop - 256) {
	  /* Trail overflow */
	  if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), TRUE)) {
	    goto trail_overflow;
	  }
	}
      } else {
	if (hack) {
	  Term d = AbsAppl(HR);
	    if (HR > ASP - MIN_ARENA_SIZE) {
	      goto overflow;
	    }
	  HR[0] = (CELL)FunctorDollarVar;
	  if (singletons) HR[1] = MkIntTerm(-1);
	  else HR[1] = MkIntTerm(numb++);
	  HR += 2;
	*ptf = d;
	mBind(ptd0,d);
	continue;
	}
	/* second time we met this term */
	if (ptd0 < ptf ||ptd0>= HR) {
	  RESET_VARIABLE(ptf);
	  mBind(ptd0, (CELL)ptf);
	} else if (ptd0 > ptf) {
	  mBind(ptf, d0);
	}
    }
    }
	  if (to_visit <= to_visit0) {
	    break;
	  }
      to_visit--;
      pt0 = to_visit->pt0;
      pt0_end = to_visit->pt0_end;
      ptf = to_visit->ptf;
      myt = to_visit->t;
      VUNMARK(to_visit->oldp, to_visit->oldv);
      ground = (ground && to_visit->ground);
  } while (true);

	  /* restore our nice, friendly, term to its original state */
    clean_tr(TR0 PASS_REGS);
  /* follow chain of multi-assigned variables */
 close_stack(&stt);
	return 0;

 aux_overflow:
  t = pt0_[1];
  while (to_visit > to_visit0) {
    to_visit--;
    VUNMARK(to_visit->oldp, to_visit->oldv);
  }
  clean_tr(TR0 PASS_REGS);

  if (arenap)
        *arenap = CloseArena(cspace PASS_REGS);
  sz += sz;
 retry:
  if (arenap) {
    enter_cell_space(cspace, arenap);
  }
  HLow =  HR;
  pt0_ = HR-1;
  pt0_end_ = HR;
  HR[0] = t;
  HR++;
    HB = HR;
  *ptf_ = HR++;
  reinit_stack(&stt, sz);
  pt0=pt0_;
  ptf=*ptf_;
  pt0_end=pt0_end_;
  if (bindp)
  *bindp =  bind0;
  goto loop;

  

overflow:
  { yhandle_t  slb,  sle;
    
  while (to_visit > to_visit0) {
    to_visit--;
     VUNMARK(to_visit->oldp, to_visit->oldv);
  }
  clean_tr(TR0);
  HB = HB0;
    if (&LOCAL_GlobalArena == arenap)
      LOCAL_GlobalArenaOverflows++;
    HR = *ptf_+1;
    slb = Yap_PushHandle(pt0_[1]);
    sle = Yap_PushHandle(bind0);
    if (arenap) {
    size_t min_grow =
      (restarts < 16 ? 16 * 1024 * restarts * restarts : 128 * 1024);
      *arenap = CloseArena(cspace PASS_REGS);
      restarts++;
      if ((*arenap =
	   GrowArena(*arenap, min_grow,  1, cspace PASS_REGS)) == 0) {
	Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	return 0L;
      }
    } else {
      if (!Yap_expand(0)) {
  close_stack(&stt);

	Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	return 0L;
      }
    }

        bind0 = Yap_PopHandle(sle);
        t = Yap_PopHandle(slb);
  }
    goto retry;

 trail_overflow:
    {
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
     VUNMARK(to_visit->oldp, to_visit->oldv);
  }
#endif
  clean_tr(TR0);
    HR = HB;
    if (arenap) {
     *arenap = CloseArena(cspace   PASS_REGS);
    }
      *arenap = CloseArena(cspace PASS_REGS);
        yhandle_t sla = Yap_PushHandle(*arenap);
        yhandle_t slb = Yap_PushHandle(pt0_[1]);
    yhandle_t sle = Yap_PushHandle(bind0);
    do {
      /* Trail overflow */
      if (!Yap_growtrail(256 * 256 * sizeof(struct trail_frame), false)) {
  close_stack(&stt);
	Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	return 0L;
      }
    } while (TR > (tr_fr_ptr) LOCAL_TrailTop - 256 * 256);
    bind0 = Yap_PopHandle(sle);
        t = Yap_PopHandle(slb);
    if (arenap) {
      *arenap = Yap_PopHandle(sla);
    }
    }
    goto retry;
  }

static Term CopyTermToArena(Term t, bool share, bool copy_att_vars, UInt arity,
			    Term *arenap, Term *listp, size_t min_grow USES_REGS) {
  cell_space_t cspace;
  int res = 0;
  Term tf;
  
  t = Deref(t);
  if ( !IsVarTerm(t) && IsAtomOrIntTerm(t)) {
    return t;
  }
  if (arenap) {
    enter_cell_space(&cspace, arenap);
  }
  {
      CELL *ap = &t;
    HB=HR;
    //   DEB_DOOBIN(t);
    CELL *pf = HR;
    RESET_VARIABLE(HR);
    HR++;

    if ((res = copy_complex_term(ap - 1, ap, share, copy_att_vars,& pf,
				 listp, arenap, &cspace PASS_REGS)) != 0) {
      //goto error_handler;
    }
    tf = Deref(*pf);
    //	          DEB_≈DOOBOUT(tf);
  }

  if (arenap) {
    Term ar = CloseArena(&cspace PASS_REGS);
    *arenap = ar;
  }
  return tf;

 }

Term Yap_CopyTerm(Term inp) {
  CACHE_REGS
    return CopyTermToArena(inp, false, true, 3, NULL, NULL, 0 PASS_REGS);
}

Term Yap_CopyTermNoShare(Term inp) {
  CACHE_REGS
    return CopyTermToArena(inp, FALSE, true, 3, NULL, NULL, 0 PASS_REGS);
}


Term Yap_HackCycles(Term inp USES_REGS) {
    return inp;
  Term l=MkIntTerm(0);
  if (IsVarTerm(inp)) {
      Term i = MkIntTerm(-1);
      return Yap_MkApplTerm(FunctorDollarVar,1,&i);
  }
  return CopyTermToArena(inp, FALSE, true, 3, NULL, &l, 0 PASS_REGS);
}

static Int p_copy_term(USES_REGS1) /* copy term t to a new instance  */
{
  Term t = CopyTermToArena(ARG1, false, TRUE, 2, NULL, NULL, 0 PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2, t);
}

static Int p_duplicate_term(USES_REGS1) /* copy term t to a new instance  */
{
  Term t = CopyTermToArena(ARG1, FALSE, TRUE, 2, NULL, NULL, 0 PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2, t);
}


static Int
p_rational_tree_to_forest(USES_REGS1) /* copy term t to a new instance  */
{
  Term t2 = ARG2;
  Term t3 = ARG3;
  Term list = Deref(ARG4);
  Term t = CopyTermToArena(ARG1, false, false, 2, NULL, &list, 0 PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(t2, t) && Yap_unify(t3, list);
}

Term
Yap_TermAsForest(Term t1, Term *list) /* copy term t to a new instance  */
{
  *list = TermNil;
  Term t = CopyTermToArena(t1, false, false, 2, NULL, list, 0 PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return t;
}


static Int
p_copy_term_no_delays(USES_REGS1) /* copy term t to a new instance  */
{
  Term t = CopyTermToArena(ARG1, false, false, 2, NULL, NULL, 0 PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2, t);
}

static Term CreateTermInArena(Atom Na, UInt Nar, UInt arity, Term *newarena,
			      Term init USES_REGS) {
  cell_space_t cells;
  Term tf;
  CELL *HB0;
  Functor f = Yap_MkFunctor(Na, Nar);
  UInt i;

 restart:
  enter_cell_space(&cells, newarena PASS_REGS);
  tf = AbsAppl(HR);
  HR[0] = (CELL) f;
  HR += 1 + ArityOfFunctor(f);
  if (HR > ASP - MIN_ARENA_SIZE) {
    /* overflow */
    HR = HB0 = HB;
    {
      //      CELL *old_top = ArenaLimit(*nsizeof(CELL)ewarena);
      if (*newarena == LOCAL_GlobalArena)
	LOCAL_GlobalArenaOverflows++;
      *newarena = CloseArena(&cells);

      if ((*newarena = GrowArena(*newarena, Nar * sizeof(CELL), arity + 1,
				 &cells PASS_REGS)) == 0) {
	Yap_Error(RESOURCE_ERROR_STACK, TermNil,
		  "while creating large global term");
	return 0L;
      }
    }
    goto restart;
  }
  if (init == 0L) {
    for (i = 1; i <= Nar; i++) {
      RESET_VARIABLE(HB0 + i);
    }
  } else {
    for (i = 1; i <= Nar; i++) {
      HB0[i] = init;
    }
  }
  Term ar;
  ar = CloseArena(&cells PASS_REGS);
  if (newarena)
    *newarena = ar;
  return tf;
}

inline static GlobalEntry *FindGlobalEntry(Atom at USES_REGS)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  READ_LOCK(ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0) {
    GlobalEntry *pe = RepGlobalProp(p0);
    if (pe->KindOfPE == GlobalProperty
#if THREADS
	&& pe->owner_id == worker_id
#endif
	) {
      READ_UNLOCK(ae->ARWLock);
      return pe;
    }
    p0 = pe->NextOfPE;
  }
  READ_UNLOCK(ae->ARWLock);
  return NULL;
}

inline static GlobalEntry *GetGlobalEntry(Atom at USES_REGS)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);
  GlobalEntry *new;

  WRITE_LOCK(ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0) {
    GlobalEntry *pe = RepGlobalProp(p0);
    if (pe->KindOfPE == GlobalProperty
#if THREADS
	&& pe->owner_id == worker_id
#endif
	) {
      WRITE_UNLOCK(ae->ARWLock);
      return pe;
    }
    p0 = pe->NextOfPE;
  }
  new = (GlobalEntry *) Yap_AllocAtomSpace(sizeof(*new));
  INIT_RWLOCK(new->GRWLock);
  new->KindOfPE = GlobalProperty;
#if THREADS
  new->owner_id = worker_id;
#endif
  new->NextGE = LOCAL_GlobalVariables;
  LOCAL_GlobalVariables = new;
  new->AtomOfGE = ae;
  AddPropToAtom(ae, (PropEntry *) new);
  RESET_VARIABLE(&new->global);
  WRITE_UNLOCK(ae->ARWLock);
  return new;
}

static UInt garena_overflow_size(CELL *arena USES_REGS) {
  UInt dup = (((CELL *) arena - H0) * sizeof(CELL)) >> 3;
  if (dup < 64 * 1024 * LOCAL_GlobalArenaOverflows)
    dup = 64 * 1024 * LOCAL_GlobalArenaOverflows;
  if (dup > 1024 * 1024)
    return 1024 * 1024;
  return dup;
}

static Int p_nb_setarg(USES_REGS1) {
  Term wheret = Deref(ARG1);
  Term dest;
  Term to;
  UInt arity, pos;
  CELL *destp;

  if (IsVarTerm(wheret)) {
    Yap_Error(INSTANTIATION_ERROR, wheret, "nb_setarg");
    return FALSE;
  }
  if (!IsIntegerTerm(wheret)) {
    Yap_Error(TYPE_ERROR_INTEGER, wheret, "nb_setarg");
    return FALSE;
  }
  pos = IntegerOfTerm(wheret);
  dest = Deref(ARG2);
  if (IsVarTerm(dest)) {
    Yap_Error(INSTANTIATION_ERROR, dest, "nb_setarg");
    return FALSE;
  } else if (IsPrimitiveTerm(dest)) {
    arity = 0;
  } else if (IsPairTerm(dest)) {
    arity = 2;
  } else {
    arity = ArityOfFunctor(FunctorOfTerm(dest));
  }
  if (pos < 1 || pos > arity)
    return FALSE;

  to = Deref(ARG3);
  to = CopyTermToArena(
		       ARG3, FALSE, TRUE, 3, &LOCAL_GlobalArena, NULL,
		       garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return FALSE;

  dest = Deref(ARG2);
  if (IsPairTerm(dest)) {
    destp = RepPair(dest) - 1;
  } else {
    destp = RepAppl(dest);
  }
  destp[pos] = to;
  return TRUE;
}

static Int p_nb_set_shared_arg(USES_REGS1) {
  Term wheret = Deref(ARG1);
  Term dest;
  Term to;
  UInt arity, pos;
  CELL *destp;

  if (IsVarTerm(wheret)) {
    Yap_Error(INSTANTIATION_ERROR, wheret, "nb_setarg");
    return FALSE;
  }
  if (!IsIntegerTerm(wheret)) {
    Yap_Error(TYPE_ERROR_INTEGER, wheret, "nb_setarg");
    return FALSE;
  }
  pos = IntegerOfTerm(wheret);
  dest = Deref(ARG2);
  if (IsVarTerm(dest)) {
    Yap_Error(INSTANTIATION_ERROR, dest, "nb_setarg");
    return FALSE;
  } else if (IsPrimitiveTerm(dest)) {
    arity = 0;
  } else if (IsPairTerm(dest)) {
    arity = 2;
  } else {
    arity = ArityOfFunctor(FunctorOfTerm(dest));
  }
  if (pos < 1 || pos > arity)
    return FALSE;
  to = CopyTermToArena(
		       ARG3, TRUE, TRUE, 3, &LOCAL_GlobalArena, NULL,
		       garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return FALSE;
  dest = Deref(ARG2);
  if (IsPairTerm(dest)) {
    destp = RepPair(dest) - 1;
  } else {
    destp = RepAppl(dest);
  }
  destp[pos] = to;
  return TRUE;
}

static Int p_nb_linkarg(USES_REGS1) {
  Term wheret = Deref(ARG1);
  Term dest;
  UInt arity, pos;
  CELL *destp;

  if (IsVarTerm(wheret)) {
    Yap_Error(INSTANTIATION_ERROR, wheret, "nb_setarg");
    return FALSE;
  }
  if (!IsIntegerTerm(wheret)) {
    Yap_Error(TYPE_ERROR_INTEGER, wheret, "nb_setarg");
    return FALSE;
  }
  pos = IntegerOfTerm(wheret);
  dest = Deref(ARG3);
  if (IsVarTerm(dest)) {
    Yap_Error(INSTANTIATION_ERROR, dest, "nb_setarg");
    return FALSE;
  } else if (IsPrimitiveTerm(dest)) {
    arity = 0;
    destp = NULL;
  } else if (IsPairTerm(dest)) {
    arity = 2;
    destp = RepPair(dest) - 1;
  } else {
    arity = ArityOfFunctor(FunctorOfTerm(dest));
    destp = RepAppl(dest);
  }
  if (pos < 1 || pos > arity)
    return FALSE;
  dest = Deref(ARG2);
  destp[pos] = Deref(ARG3);
  return TRUE;
}

static Int p_nb_linkval(USES_REGS1) {
  Term t = Deref(ARG1), to;
  GlobalEntry *ge;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_linkval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "nb_linkval");
    return (FALSE);
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  to = Deref(ARG2);
  WRITE_LOCK(ge->GRWLock);
  ge->global = to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}

static Int p_nb_create_accumulator(USES_REGS1) {
  Term t = Deref(ARG1), acct, to, t2;
  CELL *destp;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_create_accumulator");
    return FALSE;
  }
  if (!IsIntegerTerm(t) && !IsBigIntTerm(t) && !IsFloatTerm(t)) {
    Yap_Error(TYPE_ERROR_NUMBER, t, "nb_create_accumulator");
    return FALSE;
  }
  acct = Yap_MkApplTerm(FunctorGNumber, 1, &t);
  if (!Yap_unify(ARG2, acct)) {
    return FALSE;
  }
  to = CopyTermToArena(
		       t, TRUE, TRUE, 2, &LOCAL_GlobalArena, NULL,
		       garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return FALSE;
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    return Yap_unify(t2, Yap_MkApplTerm(FunctorGNumber, 1, &to));
  }
  destp = RepAppl(Deref(ARG2));
  destp[1] = to;
  return TRUE;
}

static Int p_nb_add_to_accumulator(USES_REGS1) {
  Term t = Deref(ARG1), t0, tadd;
  Functor f;
  CELL *destp;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_create_accumulator");
    return FALSE;
  }
  if (!IsApplTerm(t)) {
    Yap_Error(TYPE_ERROR_NUMBER, t, "nb_accumulator_value");
    return FALSE;
  }
  f = FunctorOfTerm(t);
  if (f != FunctorGNumber) {
    return FALSE;
  }
  destp = RepAppl(t);
  t0 = Deref(destp[1]);
  tadd = Deref(ARG2);
  if (IsVarTerm(tadd)) {
    Yap_Error(INSTANTIATION_ERROR, tadd, "nb_create_accumulator");
    return FALSE;
  }
  if (IsIntegerTerm(t0) && IsIntegerTerm(tadd)) {
    Int i0 = IntegerOfTerm(t0);
    Int i1 = IntegerOfTerm(tadd);
    Term new = MkIntegerTerm(i0 + i1);

    if (IsIntTerm(new)) {
      /* forget it if it was something else */
      destp[1] = new;
    } else {
      /* long, do we have space or not ?? */
      if (IsLongIntTerm(t0)) {
	CELL *target = RepAppl(t0);
	CELL *source = RepAppl(new);
	target[1] = source[1];
      } else {
	/* we need to create a new long int */
	new = CopyTermToArena(new, TRUE, TRUE, 2, &LOCAL_GlobalArena, NULL,
			      garena_overflow_size(ArenaPt(LOCAL_GlobalArena)
						   PASS_REGS) PASS_REGS);
	destp = RepAppl(Deref(ARG1));
	destp[1] = new;
      }
    }
    return TRUE;
  }
  if (IsFloatTerm(t0) && IsFloatTerm(tadd)) {
    Float f0 = FloatOfTerm(t0);
    Float f1 = FloatOfTerm(tadd);
    Term new = MkFloatTerm(f0 + f1);
    CELL *target = RepAppl(t0);
    CELL *source = RepAppl(new);

#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
    target[2] = source[2];
#endif
    target[1] = source[1];
    return TRUE;
  }
  if (IsNumTerm(t0) && IsNumTerm(tadd)) {
    Term t2[2], new;
    t2[0] = t0;
    t2[1] = tadd;
    new = Yap_MkApplTerm(FunctorPlus, 2, t2);

    new = Yap_Eval(new);
    new = CopyTermToArena(
			  new, TRUE, TRUE, 2, &LOCAL_GlobalArena, NULL,
			  garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
    destp = RepAppl(Deref(ARG1));
    destp[1] = new;

    return TRUE;
  }
  return FALSE;
}

static Int p_nb_accumulator_value(USES_REGS1) {
  Term t = Deref(ARG1);
  Functor f;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_accumulator_value");
    return FALSE;
  }
  if (!IsApplTerm(t)) {
    Yap_Error(TYPE_ERROR_NUMBER, t, "nb_accumulator_value");
    return FALSE;
  }
  f = FunctorOfTerm(t);
  if (f != FunctorGNumber) {
    return FALSE;
  }
  return Yap_unify(ArgOfTerm(1, t), ARG2);
}

Term Yap_SetGlobalVal(Atom at, Term t0) {
  CACHE_REGS
    Term to;
  GlobalEntry *ge;
  ge = GetGlobalEntry(at PASS_REGS);
  to = CopyTermToArena(
		       t0, FALSE, TRUE, 2, &LOCAL_GlobalArena, NULL,
		       garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return to;
  WRITE_LOCK(ge->GRWLock);
  ge->global = to;
  WRITE_UNLOCK(ge->GRWLock);
  return to;
}

Term Yap_SaveTerm(Term t0) {
  CACHE_REGS
    Term to;
  to = CopyTermToArena(
		       Deref(t0), FALSE, TRUE, 2, &LOCAL_GlobalArena, NULL,
		       garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return to;
  return to;
}

static Int p_nb_setval(USES_REGS1) {
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_setval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "nb_setval");
    return (FALSE);
  }
  return Yap_SetGlobalVal(AtomOfTerm(t), ARG2);
}

static Int p_nb_set_shared_val(USES_REGS1) {
  Term t = Deref(ARG1), to;
  GlobalEntry *ge;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_setval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "nb_setval");
    return (FALSE);
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  to = CopyTermToArena(
		       ARG2, TRUE, TRUE, 2, &LOCAL_GlobalArena, NULL,
		       garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return FALSE;
  WRITE_LOCK(ge->GRWLock);
  ge->global = to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}

static Int p_b_setval(USES_REGS1) {
  Term t = Deref(ARG1);
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "b_setval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "b_setval");
    return (FALSE);
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  WRITE_LOCK(ge->GRWLock);
#ifdef MULTI_ASSIGNMENT_VARIABLES
  /* the evil deed is to be done now */
  {
    /* but first make sure we are doing on a global object, or a constant! */
    Term t = Deref(ARG2);
    if (IsVarTerm(t) && VarOfTerm(t) > HR && VarOfTerm(t) < LCL0) {
      Term tn = MkVarTerm();
      Bind_Local(VarOfTerm(t), tn);
      t = tn;
    }
    MaBind(&ge->global, t);
  }
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
#else
  WRITE_UNLOCK(ge->GRWLock);
  Yap_Error(SYSTEM_ERROR_INTERNAL, t, "update_array");
  return FALSE;
#endif
}

static int undefined_global(USES_REGS1) {
  Term t3 = Deref(ARG3);

  if (IsApplTerm(t3)) {
    if (FunctorOfTerm(t3) == FunctorEq)
      return Yap_unify(ArgOfTerm(1, t3), ArgOfTerm(2, t3));
    return FALSE;
  }
  return Yap_unify(t3, TermNil);
}

static Int p_nb_getval(USES_REGS1) {
  Term t = Deref(ARG1), to;
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_getval");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "nb_getval");
    return FALSE;
  }
  ge = FindGlobalEntry(AtomOfTerm(t) PASS_REGS);
  if (!ge)
    return undefined_global(PASS_REGS1);
  READ_LOCK(ge->GRWLock);
  to = ge->global;
  if (IsVarTerm(to) && IsUnboundVar(VarOfTerm(to))) {
    Term t = MkVarTerm();
    Bind_and_Trail(VarOfTerm(to), t);
    to = t;
  }
  READ_UNLOCK(ge->GRWLock);
  if (to == TermFoundVar) {
    return FALSE;
  }
  return Yap_unify(ARG2, to);
}

Term Yap_GetGlobal(Atom at) {
  CACHE_REGS
    GlobalEntry *ge;
  Term to;

  ge = FindGlobalEntry(at PASS_REGS);
  if (!ge)
    return 0L;
  READ_LOCK(ge->GRWLock);
  to = ge->global;
  if (IsVarTerm(to) && IsUnboundVar(VarOfTerm(to))) {
    Term t = MkVarTerm();
    Bind_and_Trail(VarOfTerm(to), t);
    to = t;
  }
  READ_UNLOCK(ge->GRWLock);
  if (to == TermFoundVar) {
    return 0;
  }
  return to;
}

static Int nbdelete(Atom at USES_REGS) {
  GlobalEntry *ge, *g;
  AtomEntry *ae;
  Prop gp, g0;

  ge = FindGlobalEntry(at PASS_REGS);
  if (!ge) {
    Yap_Error(EXISTENCE_ERROR_VARIABLE, MkAtomTerm(at), "nb_delete");
    return FALSE;
  }
  WRITE_LOCK(ge->GRWLock);
  ae = ge->AtomOfGE;
  if (LOCAL_GlobalVariables == ge) {
    LOCAL_GlobalVariables = ge->NextGE;
  } else {
    g = LOCAL_GlobalVariables;
    while (g->NextGE != ge)
      g = g->NextGE;
    g->NextGE = ge->NextGE;
  }
  gp = AbsGlobalProp(ge);
  WRITE_LOCK(ae->ARWLock);
  if (ae->PropsOfAE == gp) {
    ae->PropsOfAE = ge->NextOfPE;
  } else {
    g0 = ae->PropsOfAE;
    while (g0->NextOfPE != gp)
      g0 = g0->NextOfPE;
    g0->NextOfPE = ge->NextOfPE;
  }
  WRITE_UNLOCK(ae->ARWLock);
  WRITE_UNLOCK(ge->GRWLock);
  Yap_FreeCodeSpace((char *) ge);
  return TRUE;
}

Int Yap_DeleteGlobal(Atom at) {
  CACHE_REGS
    return nbdelete(at PASS_REGS);
}

static Int p_nb_delete(USES_REGS1) {
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_delete");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "nb_delete");
    return FALSE;
  }
  return nbdelete(AtomOfTerm(t) PASS_REGS);
}

static Int p_nb_create(USES_REGS1) {
  Term t = Deref(ARG1);
  Term tname = Deref(ARG2);
  Term tarity = Deref(ARG3);
  Term to;
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "nb_create");
    return FALSE;
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  if (!ge) {
    Yap_Error(EXISTENCE_ERROR_VARIABLE, t, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tarity)) {
    Yap_Error(INSTANTIATION_ERROR, tarity, "nb_create");
    return FALSE;
  } else if (!IsIntegerTerm(tarity)) {
    Yap_Error(TYPE_ERROR_INTEGER, tarity, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "nb_create");
    return FALSE;
  }
  to = CreateTermInArena(AtomOfTerm(tname), IntegerOfTerm(tarity), 3,
			 &LOCAL_GlobalArena, 0L PASS_REGS);
  if (!to)
    return FALSE;
  WRITE_LOCK(ge->GRWLock);
  ge->global = to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}

static Int p_nb_create2(USES_REGS1) {
  Term t = Deref(ARG1);
  Term tname = Deref(ARG2);
  Term tarity = Deref(ARG3);
  Term tinit = Deref(ARG4);
  Term to;
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "nb_create");
    return FALSE;
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  if (!ge) {
    Yap_Error(EXISTENCE_ERROR_VARIABLE, t, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tarity)) {
    Yap_Error(INSTANTIATION_ERROR, tarity, "nb_create");
    return FALSE;
  } else if (!IsIntegerTerm(tarity)) {
    Yap_Error(TYPE_ERROR_INTEGER, tarity, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tinit)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(tinit)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "nb_create");
    return FALSE;
  }
  to = CreateTermInArena(AtomOfTerm(tname), IntegerOfTerm(tarity), 4,
			 &LOCAL_GlobalArena, tinit PASS_REGS);
  if (!to)
    return FALSE;
  WRITE_LOCK(ge->GRWLock);
  ge->global = to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}

/* a non-backtrackable queue is a term of the form $array(Arena,Start,End,Size)
 * plus an Arena. */

static Int nb_queue(UInt arena_sz USES_REGS) {
  Term queue_arena, queue, ar[QUEUE_FUNCTOR_ARITY], *nar;
  Term t = Deref(ARG1);
  LOCAL_DepthArenas++;
  if (!IsVarTerm(t)) {
    if (!IsApplTerm(t)) {
      return FALSE;
    }
    return (FunctorOfTerm(t) == FunctorNBQueue);
  }
  ar[QUEUE_ARENA] = ar[QUEUE_HEAD] = ar[QUEUE_TAIL] = ar[QUEUE_SIZE] =
    MkIntTerm(0);
  queue = Yap_MkApplTerm(FunctorNBQueue, QUEUE_FUNCTOR_ARITY, ar);
  if (!Yap_unify(queue, ARG1))
    return FALSE;
  if (arena_sz < 32 * 1024)
    arena_sz = 32 * 1024;
  queue_arena = NewArena(arena_sz, 1, NULL, worker_id);
  if (queue_arena == 0L) {
    return FALSE;
  }
  nar = RepAppl(Deref(ARG1)) + 1;
  nar[QUEUE_ARENA] = queue_arena;
  return true;
}

static Int p_nb_queue(USES_REGS1) {
  UInt arena_sz = (ASP - HR) / 16;
  if (LOCAL_DepthArenas > 1)
    arena_sz /= LOCAL_DepthArenas;
  if (arena_sz < MIN_ARENA_SIZE)
    arena_sz = MIN_ARENA_SIZE;
  if (arena_sz > MAX_ARENA_SIZE)
    arena_sz = MAX_ARENA_SIZE;
  return nb_queue(arena_sz PASS_REGS);
}

static Int p_nb_queue_sized(USES_REGS1) {
  Term t = Deref(ARG2);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "nb_queue");
    return FALSE;
  }
  if (!IsIntegerTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER, t, "nb_queue");
    return FALSE;
  }
  return nb_queue((UInt) IntegerOfTerm(t) PASS_REGS);
}

static CELL *GetQueue(Term t, char *caller) {
  t = Deref(t);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, caller);
    return NULL;
  }
  if (!IsApplTerm(t)) {
    Yap_Error(TYPE_ERROR_COMPOUND, t, caller);
    return NULL;
  }
  if (FunctorOfTerm(t) != FunctorNBQueue) {
    Yap_Error(DOMAIN_ERROR_ARRAY_TYPE, t, caller);
    return NULL;
  }
  return RepAppl(t) + 1;
}

static Term GetQueueArena(CELL *qd, char *caller) {
  Term t = Deref(qd[QUEUE_ARENA]);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, caller);
    return 0L;
  }
  if (!IsApplTerm(t)) {
    Yap_Error(TYPE_ERROR_COMPOUND, t, caller);
    return 0L;
  }
  if (FunctorOfTerm(t) != FunctorBigInt) {
    Yap_Error(DOMAIN_ERROR_ARRAY_TYPE, t, caller);
    return 0L;
  }
  return t;
}

static void RecoverArena(Term arena USES_REGS) {
  CELL *pt = ArenaPt(arena), *max = ArenaLimit(arena);

  if (max == HR) {
    HR = pt;
  }
}

static Int p_nb_queue_close(USES_REGS1) {
  Term t = Deref(ARG1);
  Int out;

  LOCAL_DepthArenas--;
  if (!IsVarTerm(t)) {
    CELL *qp;

    qp = GetQueue(t, "queue/3");
    if (qp == NULL) {
      return Yap_unify(ARG3, ARG2);
    }
    if (qp[QUEUE_ARENA] != MkIntTerm(0))
      RecoverArena(qp[QUEUE_ARENA] PASS_REGS);
    if (qp[QUEUE_SIZE] == MkIntTerm(0)) {
      return Yap_unify(ARG3, ARG2);
    }
    out = Yap_unify(ARG3, qp[QUEUE_TAIL]) && Yap_unify(ARG2, qp[QUEUE_HEAD]);
    RESET_VARIABLE(qp + QUEUE_TAIL);
    qp[QUEUE_HEAD] = qp[QUEUE_TAIL] = (CELL) (qp + QUEUE_TAIL);
    qp[QUEUE_SIZE] = MkIntTerm(0);
    return out;
  }
  Yap_Error(INSTANTIATION_ERROR, t, "queue/3");
  return FALSE;
}

static Int p_nb_queue_enqueue(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "enqueue");
  Yap_RebootHandles(worker_id);
  Term arena, qsize, to;
  UInt min_size;
  if (!qd)
    return FALSE;
  arena = GetQueueArena(qd, "enqueue");

  if (arena == 0L) {
    return FALSE;
  }

  if (IsPairTerm(qd[QUEUE_HEAD])) {
    min_size = ArenaPt(arena) - RepPair(qd[QUEUE_HEAD]);
  } else {
    min_size = 0L;
  }
  to = CopyTermToArena(Deref(ARG2), false, true, 2, &arena, NULL, min_size PASS_REGS);
  size_t slto = Yap_PushHandle(to);
  CELL *cell = Yap_GetFromArena(&arena, 2, 2);
  cell[0] = Yap_PopHandle(slto);
  RESET_VARIABLE(cell + 1);
  to = AbsPair(cell);
  /* garbage collection ? */
  qd = GetQueue(Deref(ARG1), "queue");
  qsize = IntegerOfTerm(qd[QUEUE_SIZE]);
  qd[QUEUE_SIZE] = Global_MkIntegerTerm(qsize + 1);
  if (qsize == 0) {
    qd[QUEUE_HEAD] = to;
  } else {
    *VarOfTerm(qd[QUEUE_TAIL]) = to;
    //    fprintf(stderr,"%p = ", qd+QUEUE_TAIL); Yap_DebugPlWriteln(to);
  }
  qd[QUEUE_TAIL] = TailOfTerm(to);
  qd[QUEUE_ARENA] = arena;
  return TRUE;
}

static Int p_nb_queue_dequeue(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "dequeue");
  UInt qsz;
  Term arena, out;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[QUEUE_SIZE]);
  if (qsz == 0)
    return FALSE;
  arena = GetQueueArena(qd, "dequeue");
  if (arena == 0L)
    return FALSE;
  out = HeadOfTerm(qd[QUEUE_HEAD]);
  qd[QUEUE_HEAD] = TailOfTerm(qd[QUEUE_HEAD]);
  qd[QUEUE_SIZE] = Global_MkIntegerTerm(qsz - 1);
  qd[QUEUE_ARENA] = arena;
  return Yap_unify(out, ARG2);
}

/* purge an entry from the queue, replacing it by [] */
static Int p_nb_queue_replace(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "dequeue");
  UInt qsz;
  Term queue, t = Deref(ARG2);

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[QUEUE_SIZE]);
  if (qsz == 0)
    return FALSE;

  queue = qd[QUEUE_HEAD];
  for (; qsz > 0; qsz--) {
    if (Yap_eq(HeadOfTerm(queue), t)) {
      *RepPair(Deref(queue)) = Deref(ARG3);
      return TRUE;
    }
    queue = TailOfTerm(queue);
  }
  return FALSE;
}

static Int p_nb_queue_peek(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "queue_peek");
  UInt qsz;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[QUEUE_SIZE]);
  if (qsz == 0)
    return FALSE;
  return Yap_unify(HeadOfTerm(qd[QUEUE_HEAD]), ARG2);
}

static Int p_nb_queue_empty(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "queue_empty");

  if (!qd)
    return FALSE;
  return (IntegerOfTerm(qd[QUEUE_SIZE]) == 0);
}

static Int p_nb_queue_size(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "queue_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2, qd[QUEUE_SIZE]);
}

static Int p_nb_queue_show(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "queue_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2, qd[QUEUE_HEAD]);
}

static CELL *GetHeap(Term t, char *caller) {
  t = Deref(t);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, caller);
    return NULL;
  }
  if (!IsApplTerm(t)) {
    Yap_Error(TYPE_ERROR_COMPOUND, t, caller);
    return NULL;
  }
  return RepAppl(t) + 1;
}

static Term MkZeroApplTerm(Functor f, UInt sz USES_REGS) {
  Term t0, tf;
  CELL *pt;

  if (HR + (sz + 1) > ASP - 1024)
    return TermNil;
  tf = AbsAppl(HR);
  *HR = (CELL) f;
  t0 = MkIntTerm(0);
  pt = HR + 1;
  while (sz--) {
    *pt++ = t0;
  }
  HR = pt;
  return tf;
}

static Int p_nb_heap(USES_REGS1) {
  Term heap_arena, heap, *ar, *nar;
  UInt hsize;
  Term tsize = Deref(ARG1);
  UInt arena_sz = (ASP - HR) / 16;

  if (IsVarTerm(tsize)) {
    Yap_Error(INSTANTIATION_ERROR, tsize, "nb_heap");
    return FALSE;
  } else {
    if (!IsIntegerTerm(tsize)) {
      Yap_Error(TYPE_ERROR_INTEGER, tsize, "nb_heap");
      return FALSE;
    }
    hsize = IntegerOfTerm(tsize);
  }
  if (arena_sz < hsize)
    arena_sz = hsize;
  while ((heap = MkZeroApplTerm(
				Yap_MkFunctor(AtomHeapData, 2 * hsize + HEAP_START + 1),
				2 * hsize + HEAP_START + 1 PASS_REGS)) == TermNil) {
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
      return 0;
    }
  }
  if (!Yap_unify(heap, ARG2))
    return FALSE;
  ar = RepAppl(heap) + 1;
  ar[HEAP_ARENA] = ar[HEAP_SIZE] = MkIntTerm(0);
  ar[HEAP_MAX] = tsize;
  if (arena_sz < 1024) {
    arena_sz = 1024;
  }
  heap_arena = NewArena(arena_sz, 1, NULL, worker_id);
  if (heap_arena == 0L) {
    return FALSE;
  }
  nar = RepAppl(Deref(ARG2)) + 1;
  nar[HEAP_ARENA] = heap_arena;
  return true;
}

static Int p_nb_heap_close(USES_REGS1) {
  Term t = Deref(ARG1);
  if (!IsVarTerm(t)) {
    CELL *qp;

    qp = RepAppl(t) + 1;
    if (qp[HEAP_ARENA] != MkIntTerm(0))
      RecoverArena(qp[HEAP_ARENA] PASS_REGS);
    qp[-1] = (CELL) Yap_MkFunctor(AtomHeapData, 1);
    qp[0] = MkIntegerTerm(0);
    return TRUE;
  }
  Yap_Error(INSTANTIATION_ERROR, t, "heap_close/1");
  return FALSE;
}

/*
 * static void PushHeap(CELL *pt, UInt off) {
 while (off) {
 UInt noff = (off + 1) / 2 - 1;
 if (Yap_compare_terms(pt[2 * off], pt[2 * noff]) < 0) {
 Term tk = pt[2 * noff];
 Term tv = pt[2 * noff + 1];
 pt[2 * noff] = pt[2 * off];
 pt[2 * noff + 1] = pt[2 * off + 1];
 pt[2 * off] = tk;
 pt[2 * off + 1] = tv;
 off = noff;
 } else {
 return;
 }
 }
 }
*/
static void DelHeapRoot(CELL *pt, UInt sz) {
  UInt indx = 0;
  Term tk, tv;

  sz--;
  tk = pt[2 * sz];
  tv = pt[2 * sz + 1];
  pt[2 * sz] = TermNil;
  pt[2 * sz + 1] = TermNil;
  while (TRUE) {
    if (sz < 2 * indx + 3 ||
	Yap_compare_terms(pt[4 * indx + 2], pt[4 * indx + 4]) < 0) {
      if (sz < 2 * indx + 2 || Yap_compare_terms(tk, pt[4 * indx + 2]) < 0) {
	pt[2 * indx] = tk;
	pt[2 * indx + 1] = tv;
	return;
      } else {
	pt[2 * indx] = pt[4 * indx + 2];
	pt[2 * indx + 1] = pt[4 * indx + 3];
	indx = 2 * indx + 1;
      }
    } else {
      if (Yap_compare_terms(tk, pt[4 * indx + 4]) < 0) {
	pt[2 * indx] = tk;
	pt[2 * indx + 1] = tv;
	return;
      } else {
	pt[2 * indx] = pt[4 * indx + 4];
	pt[2 * indx + 1] = pt[4 * indx + 5];
	indx = 2 * indx + 2;
      }
    }
  }
}

static CELL *new_heap_entry(CELL *qd) {
  size_t hsize, hmsize;
  if (!qd)
    return FALSE;
 restart:
  hsize = IntegerOfTerm(qd[HEAP_SIZE]);
  hmsize = IntegerOfTerm(qd[HEAP_MAX]);
  if (hsize == hmsize - 1) {
    CELL *top = qd + (HEAP_START + 2 * hmsize);
    size_t extra_size;

    if (hmsize <= 64 * 1024) {
      extra_size = 64 * 1024;
    } else {
      extra_size = hmsize;
    }
    if ((extra_size = Yap_InsertInGlobal(top, extra_size * 2 * sizeof(CELL))) ==
	0) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil,
		"No Stack Space for Non-Backtrackable terms");
      return FALSE;
    }
    extra_size = extra_size / (2 * sizeof(CELL));
    qd = GetHeap(ARG1, "add_to_heap");
    hmsize += extra_size;
    if (!qd)
      return FALSE;
    qd[-1] = (CELL) Yap_MkFunctor(AtomHeapData, 2 * hmsize + HEAP_START);
    top = qd + (HEAP_START + 2 * (hmsize - extra_size));
    while (extra_size) {
      RESET_VARIABLE(top);
      RESET_VARIABLE(top + 1);
      top += 2;
      extra_size--;
    }
    qd[HEAP_MAX] = Global_MkIntegerTerm(hmsize);
    goto restart;
  }
  return qd;
}

static Int p_nb_heap_add_to_heap(USES_REGS1) {
  CELL *qd, *pt;
  Term arena, to;
  UInt mingrow;
  size_t hsize;

  qd = new_heap_entry(GetHeap(ARG1, "add_to_heap"));
  arena = qd[HEAP_ARENA];
  if (arena == 0L)
    return false;
  mingrow = garena_overflow_size(ArenaPt(arena) PASS_REGS);
  to = CopyTermToArena(ARG2, FALSE, TRUE, 3, &arena, NULL, mingrow PASS_REGS);
  qd = GetHeap(ARG1, "add_to_heap");
  hsize = IntegerOfTerm(qd[HEAP_SIZE]);
  pt = qd + HEAP_START;
  pt[2 * hsize] = to;
  to = CopyTermToArena(ARG3, FALSE, TRUE, 3, &arena, NULL, mingrow PASS_REGS);
  /* protect key in ARG2 in case there is an overflow while copying to */
  qd = GetHeap(ARG1, "add_to_heap");
  pt = qd + HEAP_START;
  pt[2 * hsize + 1] = to;
  to = Global_MkIntegerTerm(hsize + 1);

  qd[HEAP_ARENA] = arena;
  qd[HEAP_SIZE] = to;
  return TRUE;
}

static Int p_nb_heap_del(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "deheap");
  UInt qsz;
  Term arena;
  Term tk, tv;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  if (qsz == 0)
    return FALSE;
  arena = qd[HEAP_ARENA];
  if (arena == 0L)
    return FALSE;
  /* garbage collection ? */
  qd[HEAP_SIZE] = MkIntTerm(qsz - 1);
  tk = qd[HEAP_START];
  tv = qd[HEAP_START + 1];
  DelHeapRoot(qd + HEAP_START, qsz);
  return Yap_unify(tk, ARG2) && Yap_unify(tv, ARG3);
}

static Int p_nb_heap_peek(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "heap_peek");
  UInt qsz;
  Term tk, tv;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  if (qsz == 0)
    return FALSE;
  tk = qd[HEAP_START];
  tv = qd[HEAP_START + 1];
  return Yap_unify(tk, ARG2) && Yap_unify(tv, ARG3);
}

static Int p_nb_heap_empty(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "heap_empty");

  if (!qd)
    return FALSE;
  return (IntegerOfTerm(qd[HEAP_SIZE]) == 0);
}

static Int p_nb_heap_size(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "heap_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2, qd[HEAP_SIZE]);
}

static Int p_nb_beam(USES_REGS1) {
  Term beam_arena, beam, *ar, *nar;
  UInt hsize;
  Term tsize = Deref(ARG1);
  UInt arena_sz = (HR - H0) / 16;

  if (IsVarTerm(tsize)) {
    Yap_Error(INSTANTIATION_ERROR, tsize, "nb_beam");
    return FALSE;
  } else {
    if (!IsIntegerTerm(tsize)) {
      Yap_Error(TYPE_ERROR_INTEGER, tsize, "nb_beam");
      return FALSE;
    }
    hsize = IntegerOfTerm(tsize);
  }
  while ((beam = MkZeroApplTerm(
				Yap_MkFunctor(AtomHeapData, 5 * hsize + HEAP_START + 1),
				5 * hsize + HEAP_START + 1 PASS_REGS)) == TermNil) {
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
      return 0;
    }
  }
  if (!Yap_unify(beam, ARG2))
    return FALSE;
  ar = RepAppl(beam) + 1;
  ar[HEAP_ARENA] = ar[HEAP_SIZE] = MkIntTerm(0);
  ar[HEAP_MAX] = tsize;
  if (arena_sz < 1024)
    arena_sz = 1024;
  beam_arena = NewArena(arena_sz, 1, NULL, worker_id);
  if (beam_arena == 0L) {
    return FALSE;
  }
  nar = RepAppl(Deref(ARG2)) + 1;
  nar[HEAP_ARENA] = beam_arena;
  return TRUE;
}

static Int p_nb_beam_close(USES_REGS1) { return p_nb_heap_close(PASS_REGS1); }

/* we have two queues, one with
   Key, IndxQueue2
   the other with
   Key, IndxQueue1, Val
*/
static void PushBeam(CELL *pt, CELL *npt, UInt hsize, Term key, Term to) {
  CACHE_REGS
    UInt off = hsize, off2 = hsize;
  Term toff, toff2;

  /* push into first queue */
  while (off) {
    UInt noff = (off + 1) / 2 - 1;
    if (Yap_compare_terms(key, pt[2 * noff]) < 0) {
      UInt i2 = IntegerOfTerm(pt[2 * noff + 1]);

      pt[2 * off] = pt[2 * noff];
      pt[2 * off + 1] = pt[2 * noff + 1];
      npt[3 * i2 + 1] = Global_MkIntegerTerm(off);
      off = noff;
    } else {
      break;
    }
  }
  toff = Global_MkIntegerTerm(off);
  /* off says where we are in first queue */
  /* push into second queue */
  while (off2) {
    UInt noff = (off2 + 1) / 2 - 1;
    if (Yap_compare_terms(key, npt[3 * noff]) > 0) {
      UInt i1 = IntegerOfTerm(npt[3 * noff + 1]);

      npt[3 * off2] = npt[3 * noff];
      npt[3 * off2 + 1] = npt[3 * noff + 1];
      npt[3 * off2 + 2] = npt[3 * noff + 2];
      pt[2 * i1 + 1] = Global_MkIntegerTerm(off2);
      off2 = noff;
    } else {
      break;
    }
  }
  toff2 = Global_MkIntegerTerm(off2);
  /* store elements in their rightful place */
  npt[3 * off2] = pt[2 * off] = key;
  pt[2 * off + 1] = toff2;
  npt[3 * off2 + 1] = toff;
  npt[3 * off2 + 2] = to;
}

static void DelBeamMax(CELL *pt, CELL *pt2, UInt sz) {
  CACHE_REGS
    UInt off = IntegerOfTerm(pt2[1]);
  UInt indx = 0;
  Term tk, ti, tv;

  sz--;
  /* first, fix the reverse queue */
  tk = pt2[3 * sz];
  ti = pt2[3 * sz + 1];
  tv = pt2[3 * sz + 2];
  while (TRUE) {
    if (sz < 2 * indx + 3 ||
	Yap_compare_terms(pt2[6 * indx + 3], pt2[6 * indx + 6]) > 0) {
      if (sz < 2 * indx + 2 || Yap_compare_terms(tk, pt2[6 * indx + 3]) > 0) {
	break;
      } else {
	UInt off = IntegerOfTerm(pt2[6 * indx + 4]);

	pt2[3 * indx] = pt2[6 * indx + 3];
	pt2[3 * indx + 1] = pt2[6 * indx + 4];
	pt2[3 * indx + 2] = pt2[6 * indx + 5];
	pt[2 * off + 1] = Global_MkIntegerTerm(indx);
	indx = 2 * indx + 1;
      }
    } else {
      if (Yap_compare_terms(tk, pt2[6 * indx + 6]) > 0) {
	break;
      } else {
	UInt off = IntegerOfTerm(pt2[6 * indx + 7]);

	pt2[3 * indx] = pt2[6 * indx + 6];
	pt2[3 * indx + 1] = pt2[6 * indx + 7];
	pt2[3 * indx + 2] = pt2[6 * indx + 8];
	pt[2 * off + 1] = Global_MkIntegerTerm(indx);
	indx = 2 * indx + 2;
      }
    }
  }
  pt[2 * IntegerOfTerm(ti) + 1] = Global_MkIntegerTerm(indx);
  pt2[3 * indx] = tk;
  pt2[3 * indx + 1] = ti;
  pt2[3 * indx + 2] = tv;
  /* now, fix the standard queue */
  if (off != sz) {
    Term toff, toff2, key;
    UInt off2;

    key = pt[2 * sz];
    toff2 = pt[2 * sz + 1];
    off2 = IntegerOfTerm(toff2);
    /* off says where we are in first queue */
    /* push into second queue */
    while (off) {
      UInt noff = (off + 1) / 2 - 1;
      if (Yap_compare_terms(key, pt[2 * noff]) < 0) {
	UInt i1 = IntegerOfTerm(pt[2 * noff + 1]);

	pt[2 * off] = pt[2 * noff];
	pt[2 * off + 1] = pt[2 * noff + 1];
	pt2[3 * i1 + 1] = Global_MkIntegerTerm(off);
	off = noff;
      } else {
	break;
      }
    }
    toff = Global_MkIntegerTerm(off);
    /* store elements in their rightful place */
    pt[2 * off] = key;
    pt2[3 * off2 + 1] = toff;
    pt[2 * off + 1] = toff2;
  }
}

static Term DelBeamMin(CELL *pt, CELL *pt2, UInt sz) {
  CACHE_REGS
    UInt off2 = IntegerOfTerm(pt[1]);
  Term ov = pt2[3 * off2 + 2]; /* return value */
  UInt indx = 0;
  Term tk, tv;

  sz--;
  /* first, fix the standard queue */
  tk = pt[2 * sz];
  tv = pt[2 * sz + 1];
  while (TRUE) {
    if (sz < 2 * indx + 3 ||
	Yap_compare_terms(pt[4 * indx + 2], pt[4 * indx + 4]) < 0) {
      if (sz < 2 * indx + 2 || Yap_compare_terms(tk, pt[4 * indx + 2]) < 0) {
	break;
      } else {
	UInt off2 = IntegerOfTerm(pt[4 * indx + 3]);
	pt[2 * indx] = pt[4 * indx + 2];
	pt[2 * indx + 1] = pt[4 * indx + 3];
	pt2[3 * off2 + 1] = Global_MkIntegerTerm(indx);
	indx = 2 * indx + 1;
      }
    } else {
      if (Yap_compare_terms(tk, pt[4 * indx + 4]) < 0) {
	break;
      } else {
	UInt off2 = IntegerOfTerm(pt[4 * indx + 5]);

	pt[2 * indx] = pt[4 * indx + 4];
	pt[2 * indx + 1] = pt[4 * indx + 5];
	pt2[3 * off2 + 1] = Global_MkIntegerTerm(indx);
	indx = 2 * indx + 2;
      }
    }
  }
  pt[2 * indx] = tk;
  pt[2 * indx + 1] = tv;
  pt2[3 * IntegerOfTerm(tv) + 1] = Global_MkIntegerTerm(indx);
  /* now, fix the reverse queue */
  if (off2 != sz) {
    Term to, toff, toff2, key;
    UInt off;

    key = pt2[3 * sz];
    toff = pt2[3 * sz + 1];
    to = pt2[3 * sz + 2];
    off = IntegerOfTerm(toff);
    /* off says where we are in first queue */
    /* push into second queue */
    while (off2) {
      UInt noff = (off2 + 1) / 2 - 1;
      if (Yap_compare_terms(key, pt2[3 * noff]) > 0) {
	UInt i1 = IntegerOfTerm(pt2[3 * noff + 1]);

	pt2[3 * off2] = pt2[3 * noff];
	pt2[3 * off2 + 1] = pt2[3 * noff + 1];
	pt2[3 * off2 + 2] = pt2[3 * noff + 2];
	pt[2 * i1 + 1] = Global_MkIntegerTerm(off2);
	off2 = noff;
      } else {
	break;
      }
    }
    toff2 = Global_MkIntegerTerm(off2);
    /* store elements in their rightful place */
    pt2[3 * off2] = key;
    pt[2 * off + 1] = toff2;
    pt2[3 * off2 + 1] = toff;
    pt2[3 * off2 + 2] = to;
  }
  return ov;
}

static size_t new_beam_entry(CELL *qd) {
  size_t hsize, hmsize;
  hsize = IntegerOfTerm(qd[HEAP_SIZE]);
  hmsize = IntegerOfTerm(qd[HEAP_MAX]);

  if (!qd)
    return 0;
  Term *pt;
  if (hsize == hmsize) {
    pt = qd + HEAP_START;
    if (Yap_compare_terms(pt[2 * hmsize], Deref(ARG2)) > 0) {
      /* smaller than current max, we need to drop current max */
      DelBeamMax(pt, pt + 2 * hmsize, hmsize);
      hsize--;
    }
  }
  return hsize;
}

static Int p_nb_beam_add_to_beam(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "add_to_beam"), *pt;
  size_t hsize, hmsize = qd[HEAP_SIZE];
  Term arena, to, key;
  UInt mingrow;

  hsize = new_beam_entry(qd);
  arena = qd[HEAP_ARENA];
  if (arena == 0L)
    return FALSE;
  mingrow = garena_overflow_size(ArenaPt(arena) PASS_REGS);
  key = CopyTermToArena(ARG2, FALSE, TRUE, 3, &arena, NULL, mingrow PASS_REGS);
  to = CopyTermToArena(ARG3, FALSE, TRUE, 3, &arena, NULL, mingrow PASS_REGS);
  if (key == 0 || to == 0L)
    return FALSE;
  qd = GetHeap(ARG1, "add_to_beam");
  qd[HEAP_ARENA] = arena;
  pt = qd + HEAP_START;
  PushBeam(pt, pt + 2 * hmsize, hsize, key, to);
  qd[HEAP_SIZE] = Global_MkIntegerTerm(hsize + 1);
  return TRUE;
}

static Int p_nb_beam_del(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "debeam");
  UInt qsz;
  Term tk, tv;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  if (qsz == 0)
    return FALSE;
  /* garbage collection ? */
  qd[HEAP_SIZE] = Global_MkIntegerTerm(qsz - 1);
  tk = qd[HEAP_START];
  tv = DelBeamMin(qd + HEAP_START,
		  qd + (HEAP_START + 2 * IntegerOfTerm(qd[HEAP_MAX])), qsz);
  return Yap_unify(tk, ARG2) && Yap_unify(tv, ARG3);
}

#ifdef DEBUG

static Int p_nb_beam_check(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "debeam");
  UInt qsz, qmax;
  CELL *pt, *pt2;
  UInt i;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  qmax = IntegerOfTerm(qd[HEAP_MAX]);
  if (qsz == 0)
    return TRUE;
  pt = qd + HEAP_START;
  pt2 = pt + 2 * qmax;
  for (i = 1; i < qsz; i++) {
    UInt back;
    if (Yap_compare_terms(pt[2 * ((i + 1) / 2 - 1)], pt[2 * i]) > 0) {
            Yap_DebugPlWrite(pt[2 * ((i + 1) / 2 - 1)]);
      fprintf(stderr, "\n");
      Yap_DebugPlWrite(pt[2 * i]);
      fprintf(stderr, "\n");
      fprintf(stderr, "Error at %ld\n", (unsigned long int) i);
      return FALSE;
    }
    back = IntegerOfTerm(pt[2 * i + 1]);
    if (IntegerOfTerm(pt2[3 * back + 1]) != i) {
      fprintf(stderr, "Link error at %ld\n", (unsigned long int) i);
      return FALSE;
    }
  }
  for (i = 1; i < qsz; i++) {
    if (Yap_compare_terms(pt2[3 * ((i + 1) / 2 - 1)], pt2[3 * i]) < 0) {
      fprintf(stderr, "Error at sec %ld\n", (unsigned long int) i);
      Yap_DebugPlWrite(pt2[3 * ((i + 1) / 2 - 1)]);
      fprintf(stderr, "\n");
      Yap_DebugPlWrite(pt2[3 * i]);
      fprintf(stderr, "\n");
      return FALSE;
    }
  }
  return TRUE;
}

#endif

static Int p_nb_beam_keys(USES_REGS1) {
  CELL *qd;
  UInt qsz;
  CELL *pt, *ho;
  UInt i;

 restart:
  qd = GetHeap(ARG1, "beam_keys");
  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  ho = HR;
  pt = qd + HEAP_START;
  if (qsz == 0)
    return Yap_unify(ARG2, TermNil);
  for (i = 0; i < qsz; i++) {
    if (HR > ASP - 1024) {
      if (!Yap_dogc(0, NULL PASS_REGS)) {
	Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	return 0;
      }

      goto restart;
    }
    *HR++ = pt[0];
    *HR = AbsPair(HR + 1);
    HR++;
    pt += 2;
  }
  HR[-1] = TermNil;
  return Yap_unify(ARG2, AbsPair(ho));
}

static Int p_nb_beam_peek(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "beam_peek"), *pt, *pt2;
  UInt qsz, qbsize;
  Term tk, tv;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  qbsize = IntegerOfTerm(qd[HEAP_MAX]);
  if (qsz == 0)
    return FALSE;
  pt = qd + HEAP_START;
  pt2 = pt + 2 * qbsize;
  tk = pt[0];
  tv = pt2[2];
  return Yap_unify(tk, ARG2) && Yap_unify(tv, ARG3);
}

static Int p_nb_beam_empty(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "beam_empty");

  if (!qd)
    return FALSE;
  return (IntegerOfTerm(qd[HEAP_SIZE]) == 0);
}

static Int p_nb_beam_size(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "beam_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2, qd[HEAP_SIZE]);
}

static Int cont_current_nb(USES_REGS1) {
  Int unif;
  GlobalEntry *ge = (GlobalEntry *) IntegerOfTerm(EXTRA_CBACK_ARG(1, 1));

  unif = Yap_unify(MkAtomTerm(AbsAtom(ge->AtomOfGE)), ARG1);
  ge = ge->NextGE;
  if (!ge) {
    if (unif)
      cut_succeed();
    else
      cut_fail();
  } else {
    EXTRA_CBACK_ARG(1, 1) = MkIntegerTerm((Int) ge);
    return unif;
  }
}

static Int init_current_nb(USES_REGS1) { /* current_atom(?Atom)		 */
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    if (IsAtomTerm(t1)) {
      if (!FindGlobalEntry(AtomOfTerm(t1) PASS_REGS)) {
	cut_fail();
      } else {
	cut_succeed();
      }
    } else {
      Yap_Error(TYPE_ERROR_ATOM, t1, "nb_current");
      cut_fail();
    }
  }
  READ_LOCK(HashChain[0].AERWLock);
  EXTRA_CBACK_ARG(1, 1) = MkIntegerTerm((Int) LOCAL_GlobalVariables);
  return cont_current_nb(PASS_REGS1);
}

void Yap_InitGlobals(void) {
  CACHE_REGS
    Term cm = CurrentModule;
  Yap_InitCPred("$allocate_arena", 2, p_allocate_arena, 0);
  Yap_InitCPred("arena_size", 1, p_default_arena_size, 0);
  Yap_InitCPred("b_setval", 2, p_b_setval, SafePredFlag);
  Yap_InitCPred("__B_setval__", 2, p_b_setval, HiddenPredFlag | SafePredFlag);
  /** @pred  b_setval(+ _Name_, + _Value_)


      Associate the term  _Value_ with the atom  _Name_ or replaces
      the currently associated value with  _Value_. If  _Name_ does
      not refer to an existing global variable a variable with initial value
      [] is created (the empty list). On backtracking the assignment is
      reversed.


  */
  /** @pred b_setval(+ _Name_,+ _Value_)


      Associate the term  _Value_ with the atom  _Name_ or replaces
      the currently associated value with  _Value_.  If  _Name_ does
      not refer to an existing global variable a variable with initial value
      `[]` is created (the empty list).  On backtracking the
      assignment is reversed.


  */
  Yap_InitCPred("nb_setval", 2, p_nb_setval, 0L);
  Yap_InitCPred("__NB_setval__", 2, p_nb_setval, HiddenPredFlag);
  /** @pred  nb_setval(+ _Name_, + _Value_)


      Associates a copy of  _Value_ created with duplicate_term/2 with
      the atom  _Name_. Note that this can be used to set an initial
      value other than `[]` prior to backtrackable assignment.


  */
  /** @pred nb_setval(+ _Name_,+ _Value_)


      Associates a copy of  _Value_ created with duplicate_term/2
      with the atom  _Name_.  Note that this can be used to set an
      initial value other than `[]` prior to backtrackable assignment.


  */
  Yap_InitCPred("nb_set_shared_val", 2, p_nb_set_shared_val, 0L);
  /** @pred  nb_set_shared_val(+ _Name_, + _Value_)


      Associates the term  _Value_ with the atom  _Name_, but sharing
      non-backtrackable terms. This may be useful if you want to rewrite a
      global variable so that the new copy will survive backtracking, but
      you want to share structure with the previous term.

      The next example shows the differences between the three built-ins:

      ~~~~~
      ?- nb_setval(a,a(_)),nb_getval(a,A),nb_setval(b,t(C,A)),nb_getval(b,B).
      A = a(_A),
      B = t(_B,a(_C)) ?

      ?-
      nb_setval(a,a(_)),nb_getval(a,A),nb_set_shared_val(b,t(C,A)),nb_getval(b,B).

      ?- nb_setval(a,a(_)),nb_getval(a,A),nb_linkval(b,t(C,A)),nb_getval(b,B).
      A = a(_A),
      B = t(C,a(_A)) ?
      ~~~~~


  */
  Yap_InitCPred("nb_linkval", 2, p_nb_linkval, 0L);
  /** @pred  nb_linkval(+ _Name_, + _Value_)


      Associates the term  _Value_ with the atom  _Name_ without
      copying it. This is a fast special-purpose variation of nb_setval/2
      intended for expert users only because the semantics on backtracking
      to a point before creating the link are poorly defined for compound
      terms. The principal term is always left untouched, but backtracking
      behaviour on arguments is undone if the original assignment was
      trailed and left alone otherwise, which implies that the history that
      created the term affects the behaviour on backtracking. Please
      consider the following example:

      ~~~~~
      demo_nb_linkval :-
      T = nice(N),
      (   N = world,
      nb_linkval(myvar, T),
      fail
      ;   nb_getval(myvar, V),
      writeln(V)
      ).
      ~~~~~


  */
  Yap_InitCPred("$nb_getval", 3, p_nb_getval, SafePredFlag);
  Yap_InitCPred("__NB_getval__", 3, p_nb_getval, HiddenPredFlag);
  Yap_InitCPred("__B_getval__", 3, p_nb_getval, HiddenPredFlag);
  Yap_InitCPred("nb_setarg", 3, p_nb_setarg, 0L);
  /** @pred  nb_setarg(+{Arg], + _Term_, + _Value_)



      Assigns the  _Arg_-th argument of the compound term  _Term_ with
      the given  _Value_ as setarg/3, but on backtracking the assignment
      is not reversed. If  _Term_ is not atomic, it is duplicated using
      duplicate_term/2. This predicate uses the same technique as
      nb_setval/2. We therefore refer to the description of
      nb_setval/2 for details on non-backtrackable assignment of
      terms. This predicate is compatible to GNU-Prolog
      `setarg(A,T,V,false)`, removing the type-restriction on
      _Value_. See also nb_linkarg/3. Below is an example for
      counting the number of solutions of a goal. Note that this
      implementation is thread-safe, reentrant and capable of handling
      exceptions. Realising these features with a traditional implementation
      based on assert/retract or flag/3 is much more complicated.

      ~~~~~
      succeeds_n_times(Goal, Times) :-
      Counter = counter(0),
      (   Goal,
      arg(1, Counter, N0),
      N is N0 + 1,
      nb_setarg(1, Counter, N),
      fail
      ;   arg(1, Counter, Times)
      ).
      ~~~~~


  */
  Yap_InitCPred("nb_set_shared_arg", 3, p_nb_set_shared_arg, 0L);
  /** @pred  nb_set_shared_arg(+ _Arg_, + _Term_, + _Value_)



      As nb_setarg/3, but like nb_linkval/2 it does not
      duplicate the global sub-terms in  _Value_. Use with extreme care
      and consult the documentation of nb_linkval/2 before use.


  */
  Yap_InitCPred("nb_linkarg", 3, p_nb_linkarg, 0L);
  /** @pred  nb_linkarg(+ _Arg_, + _Term_, + _Value_)



      As nb_setarg/3, but like nb_linkval/2 it does not
      duplicate  _Value_. Use with extreme care and consult the
      documentation of nb_linkval/2 before use.


  */
  Yap_InitCPred("nb_delete", 1, p_nb_delete, 0L);
  /** @pred  nb_delete(+ _Name_)


      Delete the named global variable.


      Global variables have been introduced by various Prolog
      implementations recently. We follow the implementation of them in
      SWI-Prolog, itself based on hProlog by Bart Demoen.

      GNU-Prolog provides a rich set of global variables, including
      arrays. Arrays can be implemented easily in YAP and SWI-Prolog using
      functor/3 and `setarg/3` due to the unrestricted arity of
      compound terms.


  */
  Yap_InitCPred("nb_create", 3, p_nb_create, 0L);
  Yap_InitCPred("nb_create", 4, p_nb_create2, 0L);
  Yap_InitCPredBack("$nb_current", 1, 1, init_current_nb, cont_current_nb,
		    SafePredFlag);
  Yap_InitCPred("copy_term", 2, p_copy_term, 0);
  /** @pred  copy_term(? _TI_,- _TF_) is iso


      Term  _TF_ is a variant of the original term  _TI_, such that for
      each variable  _V_ in the term  _TI_ there is a new variable  _V'_
      in term  _TF_. Notice that:

      + suspended goals and attributes for attributed variables in _TI_ are also
      duplicated;
      + ground terms are shared between the new and the old term.

      If you do not want any sharing to occur please use
      duplicate_term/2.


  */
  Yap_InitCPred("duplicate_term", 2, p_duplicate_term, 0);
  /** @pred  duplicate_term(? _TI_,- _TF_)


      Term  _TF_ is a variant of the original term  _TI_, such that
      for each variable  _V_ in the term  _TI_ there is a new variable
      _V'_ in term  _TF_, and the two terms do not share any
      structure. All suspended goals and attributes for attributed variables
      in  _TI_ are also duplicated.

      Also refer to copy_term/2.


  */
  Yap_InitCPred("copy_term_nat", 2, p_copy_term_no_delays, 0);
  Yap_InitCPred("rational_term_to_forest", 4, p_rational_tree_to_forest, 0);
  /** @pred copy_term_nat(? _TI_,- _TF_)


      As copy_term/2.  Attributes however, are <em>not</em> copied but replaced
      by fresh variables.




  */
  CurrentModule = GLOBALS_MODULE;
  Yap_InitCPred("nb_queue", 1, p_nb_queue, 0L);
  Yap_InitCPred("nb_queue", 2, p_nb_queue_sized, 0L);
  Yap_InitCPred("nb_queue_close", 3, p_nb_queue_close, SafePredFlag);
  Yap_InitCPred("nb_queue_enqueue", 2, p_nb_queue_enqueue, 0L);
  Yap_InitCPred("nb_queue_dequeue", 2, p_nb_queue_dequeue, SafePredFlag);
  Yap_InitCPred("nb_queue_peek", 2, p_nb_queue_peek, SafePredFlag);
  Yap_InitCPred("nb_queue_empty", 1, p_nb_queue_empty, SafePredFlag);
  Yap_InitCPred("nb_queue_replace", 3, p_nb_queue_replace, SafePredFlag);
  Yap_InitCPred("nb_queue_size", 2, p_nb_queue_size, SafePredFlag);
  Yap_InitCPred("nb_queue_show", 2, p_nb_queue_show, SafePredFlag);
  Yap_InitCPred("nb_heap", 2, p_nb_heap, 0L);
  Yap_InitCPred("nb_heap_close", 1, p_nb_heap_close, SafePredFlag);
  Yap_InitCPred("nb_heap_add", 3, p_nb_heap_add_to_heap, 0L);
  Yap_InitCPred("nb_heap_del", 3, p_nb_heap_del, SafePredFlag);
  Yap_InitCPred("nb_heap_peek", 3, p_nb_heap_peek, SafePredFlag);
  Yap_InitCPred("nb_heap_empty", 1, p_nb_heap_empty, SafePredFlag);
  Yap_InitCPred("nb_heap_size", 2, p_nb_heap_size, SafePredFlag);
  Yap_InitCPred("nb_beam", 2, p_nb_beam, 0L);
  Yap_InitCPred("nb_beam_close", 1, p_nb_beam_close, SafePredFlag);
  Yap_InitCPred("nb_beam_add", 3, p_nb_beam_add_to_beam, 0L);
  Yap_InitCPred("nb_beam_del", 3, p_nb_beam_del, SafePredFlag);
  Yap_InitCPred("nb_beam_peek", 3, p_nb_beam_peek, SafePredFlag);
  Yap_InitCPred("nb_beam_empty", 1, p_nb_beam_empty, SafePredFlag);
  Yap_InitCPred("nb_beam_keys", 2, p_nb_beam_keys, 0L);
  Yap_InitCPred("nb_create_accumulator", 2, p_nb_create_accumulator, 0L);
  Yap_InitCPred("nb_add_to_accumulator", 2, p_nb_add_to_accumulator, 0L);
  Yap_InitCPred("nb_accumulator_value", 2, p_nb_accumulator_value, 0L);
#ifdef DEBUG
  Yap_InitCPred("nb_beam_check", 1, p_nb_beam_check, SafePredFlag);
#endif
  Yap_InitCPred("nb_beam_size", 2, p_nb_beam_size, SafePredFlag);
  CurrentModule = cm;
}

#endif

/**
   @}
*/
