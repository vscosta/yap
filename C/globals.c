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
* File:		non backtrackable term support				 *
* Last rev:	2/8/06							 *
* mods:									 *
* comments:	non-backtrackable term support				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif


/**

@file globals.c

 @defgroup Global_Variables Global Variables
@ingroup YAPBuiltins
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

Global variables may be initialised from directives to make them
available during the program lifetime, but some considerations are
necessary for saved-states and threads. Saved-states to not store
global variables, which implies they have to be declared with
initialization/1 to recreate them after loading the saved
state. Each thread has its own set of global variables, starting with
an empty set. Using `thread_initialization/1` to define a global
variable it will be defined, restored after reloading a saved state
and created in all threads that are created after the
registration. Finally, global variables can be initialised using the
exception hook called exception/3. The latter technique is used
by CHR.

SWI-Prolog global variables are associations between names (atoms) and
terms.  They differ in various ways from storing information using
assert/1 or recorda/3.

+ The value lives on the Prolog (global) stack.  This implies 
that lookup time is independent from the size of the term.
This is particulary interesting for large data structures
such as parsed XML documents or the CHR global constraint
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

Global variables may be initialised from directives to make them
available during the program lifetime, but some considerations are
necessary for saved-states and threads. Saved-states to not store global
variables, which implies they have to be declared with initialization/1
to recreate them after loading the saved state.  Each thread has
its own set of global variables, starting with an empty set.  Using
`thread_inititialization/1` to define a global variable it will be
defined, restored after reloading a saved state and created in all
threads that are created <em>after</em> the registration.

 
*/

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "iopreds.h"
#include "eval.h"
#include "attvar.h"
#include <math.h>

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

#define HEAP_SIZE  0
#define HEAP_MAX   1
#define HEAP_ARENA 2
#define HEAP_START 3

#define MIN_ARENA_SIZE (1048L)


#define MAX_ARENA_SIZE (2048*16)

#define Global_MkIntegerTerm(I) MkIntegerTerm(I)

static UInt
big2arena_sz(CELL *arena_base)
{
  return (((MP_INT*)(arena_base+2))->_mp_alloc*sizeof(mp_limb_t) + sizeof(MP_INT) + sizeof(Functor)+2*sizeof(CELL))/sizeof(CELL);
}

static UInt
arena2big_sz(UInt sz)
{
  return sz - (sizeof(MP_INT) + sizeof(Functor) + 2*sizeof(CELL))/sizeof(CELL);
}


/* pointer to top of an arena */
static inline CELL *
ArenaLimit(Term arena)
{
  CELL *arena_base = RepAppl(arena);
  UInt sz = big2arena_sz(arena_base);
  return arena_base+sz;
}

/* pointer to top of an arena */
static inline CELL *
ArenaPt(Term arena)
{
  return (CELL *)RepAppl(arena);
}

static inline UInt
ArenaSz(Term arena)
{
  return big2arena_sz(RepAppl(arena));
}

static Term
CreateNewArena(CELL *ptr, UInt size)
{
  Term t = AbsAppl(ptr);
  MP_INT *dst;

  ptr[0] = (CELL)FunctorBigInt;
  ptr[1] = EMPTY_ARENA;
  dst = (MP_INT *)(ptr+2);
  dst->_mp_size = 0L;
  dst->_mp_alloc = (sizeof(CELL)/sizeof(mp_limb_t))*arena2big_sz(size);
  ptr[size-1] = EndSpecials;
  return t;
}

static Term
NewArena(UInt size, int wid, UInt arity, CELL *where)
{
  Term t;
  UInt new_size;
  WORKER_REGS(wid)

  if (where == NULL || where == HR) {
    while (HR+size > ASP-1024) {
      if (!Yap_gcl(size*sizeof(CELL), arity, ENV, P)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	return TermNil;
      }
    }
    t = CreateNewArena(HR, size);
    HR += size;
  } else {
    if ((new_size=Yap_InsertInGlobal(where, size*sizeof(CELL)))==0) {
      Yap_Error(OUT_OF_STACK_ERROR,TermNil,"No Stack Space for Non-Backtrackable terms");
      return TermNil;
    }
    size = new_size/sizeof(CELL);
    t = CreateNewArena(where, size);
  }
  return t;
}

static Int
p_allocate_arena( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"allocate_arena");
    return FALSE;
  } else if (!IsIntegerTerm(t)) {
      Yap_Error(TYPE_ERROR_INTEGER,t,"allocate_arena");
      return FALSE;
  }
  return Yap_unify(ARG2,NewArena(IntegerOfTerm(t), worker_id, 1, NULL));
}


static Int
p_default_arena_size( USES_REGS1 )
{
  return Yap_unify(ARG1,MkIntegerTerm(ArenaSz(LOCAL_GlobalArena)));
}


void
Yap_AllocateDefaultArena(Int gsize, Int attsize, int wid)
{
  REMOTE_GlobalArena(wid) = NewArena(gsize, wid, 2, NULL);
}

static void
adjust_cps(UInt size USES_REGS)
{
  /* adjust possible back pointers in choice-point stack */
  choiceptr b_ptr = B;
  while (b_ptr->cp_h == HR) {
    b_ptr->cp_h += size;
    b_ptr = b_ptr->cp_b;
  }
}


static int
GrowArena(Term arena, CELL *pt, size_t old_size, size_t size, UInt arity USES_REGS)
{
  LOCAL_ArenaOverflows++;
  if (size == 0) {
    if (old_size < 128*1024) {
      size = old_size;
    } else {
      size = old_size+128*1024;
    }
  }
  if (size < 4096) {
    size = 4096;
  }
  if (pt == HR) {
    if (HR+size > ASP-1024) {

      XREGS[arity+1] = arena;
      if (!Yap_gcl(size*sizeof(CELL), arity+1, ENV, gc_P(P,CP))) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	return FALSE;
      }
      arena = XREGS[arity+1];
      /* we don't know if the GC added junk on top of the global */
      pt = ArenaLimit(arena);
      return GrowArena(arena, pt, old_size, size, arity PASS_REGS);
    }
    adjust_cps(size PASS_REGS);
    HR += size;
  } else {
    XREGS[arity+1] = arena;
    /* try to recover some room  */
    if (arena == LOCAL_GlobalArena && 10*(pt-H0) > 8*(HR-H0)) {
      if (!Yap_gcl(size*sizeof(CELL), arity+1, ENV, gc_P(P,CP))) {
	Yap_Error(OUT_OF_STACK_ERROR,TermNil,LOCAL_ErrorMessage);
	return FALSE;
      }
    }
    arena = XREGS[arity+1];
    pt = ArenaLimit(arena);
    if ((size=Yap_InsertInGlobal(pt, size*sizeof(CELL)))==0) {
      return FALSE;
    }
    size = size/sizeof(CELL);
    arena = XREGS[arity+1];
  }
  CreateNewArena(ArenaPt(arena), size+old_size);
  return TRUE;
}

CELL *
Yap_GetFromArena(Term *arenap, UInt cells, UInt arity)
{
  CACHE_REGS
 restart:
  {
    Term arena = *arenap;
    CELL *max = ArenaLimit(arena);
    CELL *base = ArenaPt(arena);
    CELL *newH;
    UInt old_sz = ArenaSz(arena), new_size;

    if (IN_BETWEEN(base, HR, max)) {
      base = HR;
      HR += cells;
      return base;
    }
    if (base+cells > max-1024) {
      if (!GrowArena(arena, max, old_sz, old_sz+sizeof(CELL)*1024, arity PASS_REGS))
	return NULL;
      goto restart;
    }

    newH = base+cells;
    new_size = old_sz - cells;
    *arenap = CreateNewArena(newH, new_size);
    return base;
  }
}

static void
CloseArena(CELL *oldH, CELL *oldHB, CELL *oldASP, Term *oldArenaP, UInt old_size USES_REGS)
{
  UInt new_size;

  if (HR == oldH)
    return;
  new_size = old_size - (HR-RepAppl(*oldArenaP));
  *oldArenaP = CreateNewArena(HR, new_size);
  HR = oldH;
  HB = oldHB;
  ASP = oldASP;
}

static inline void
clean_dirty_tr(tr_fr_ptr TR0 USES_REGS) {
  if (TR != TR0) {
    tr_fr_ptr pt = TR0;

    do {
      Term p = TrailTerm(pt++);
      if (IsVarTerm(p)) {
	RESET_VARIABLE(p);
      } else {
	/* copy downwards */
	TrailTerm(TR0+1) = TrailTerm(pt);
	TrailTerm(TR0) = TrailTerm(TR0+2) = p;
	pt+=2;
	TR0 += 3;
      }
    } while (pt != TR);
    TR = TR0;
  }
}

static int
copy_complex_term(register CELL *pt0, register CELL *pt0_end, int share, int copy_att_vars, CELL *ptf, CELL *HLow USES_REGS)
{

  struct cp_frame *to_visit0, *to_visit = (struct cp_frame *)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;
  int ground = TRUE;

  HB = HLow;
  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, copy_term_unk);
  copy_term_nvar:
    {
      if (IsPairTerm(d0)) {
	CELL *ap2 = RepPair(d0);
	if ((share && ap2 < HB) ||
	    (ap2 >= HB && ap2 < HR)) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	*ptf = AbsPair(HR);
	ptf++;
#ifdef RATIONAL_TREES
	if (to_visit+1 >= (struct cp_frame *)AuxSp) {
	  goto heap_overflow;
	}
	to_visit->start_cp = pt0;
	to_visit->end_cp = pt0_end;
	to_visit->to = ptf;
	to_visit->oldv = *pt0;
	to_visit->ground = ground;
	/* fool the system into thinking we had a variable there */
	*pt0 = AbsPair(HR);
	to_visit ++;
#else
	if (pt0 < pt0_end) {
	  if (to_visit + 1 >= (CELL **)AuxSp) {
	    goto heap_overflow;
	  }
	  to_visit->start_cp = pt0;
	  to_visit->end_cp = pt0_end;
	  to_visit->to = ptf;
	  to_visit->ground = ground;
	  to_visit ++;
	}
#endif
	ground = TRUE;
	pt0 = ap2 - 1;
	pt0_end = ap2 + 1;
	ptf = HR;
	HR += 2;
	if (HR > ASP - MIN_ARENA_SIZE) {
	  goto overflow;
	}
      } else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2;
	/* store the terms to visit */
	ap2 = RepAppl(d0);
	if ((share && ap2 < HB) ||
	    (ap2 >= HB && ap2 < HR)) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {
	  switch((CELL)f) {
	  case (CELL)FunctorDBRef:
	  case (CELL)FunctorAttVar:
	    *ptf++ = d0;
	    break;
	  case (CELL)FunctorLongInt:
	    if (HR > ASP - (MIN_ARENA_SIZE+3)) {
	      goto overflow;
	    }
	    *ptf++ = AbsAppl(HR);
	    HR[0] = (CELL)f;
	    HR[1] = ap2[1];
	    HR[2] = EndSpecials;
	    HR += 3;
	    if (HR > ASP - MIN_ARENA_SIZE) {
	      goto overflow;
	    }
	    break;
	  case (CELL)FunctorDouble:
	    if (HR > ASP - (MIN_ARENA_SIZE+(2+SIZEOF_DOUBLE/sizeof(CELL)))) {
	      goto overflow;
	    }
	    *ptf++ = AbsAppl(HR);
	    HR[0] = (CELL)f;
	    HR[1] = ap2[1];
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	    HR[2] = ap2[2];
	    HR[3] = EndSpecials;
	    HR += 4;
#else
	    HR[2] = EndSpecials;
	    HR += 3;
#endif
	    break;
	  case (CELL)FunctorString:
	    if (ASP - HR < MIN_ARENA_SIZE+3+ap2[1]) {
		goto overflow;
	    }
	    *ptf++ = AbsAppl(HR);
	    memcpy(HR, ap2, sizeof(CELL)*(3+ap2[1]));
	    HR+=ap2[1]+3;
	    break;
	  default:
	    {
	      /* big int */
	      UInt sz = (sizeof(MP_INT)+3*CellSize+
			 ((MP_INT *)(ap2+2))->_mp_alloc*sizeof(mp_limb_t))/CellSize, i;

	      if (HR > ASP - (MIN_ARENA_SIZE+sz)) {
		goto overflow;
	      }
	      *ptf++ = AbsAppl(HR);
	      HR[0] = (CELL)f;
	      for (i = 1; i < sz; i++) {
		HR[i] = ap2[i];
	      }
	      HR += sz;
	    }
	  }
	  continue;
	}
	*ptf = AbsAppl(HR);
	ptf++;
	/* store the terms to visit */
#ifdef RATIONAL_TREES
	if (to_visit+1 >= (struct cp_frame *)AuxSp) {
	  goto heap_overflow;
	}
	to_visit->start_cp = pt0;
	to_visit->end_cp = pt0_end;
	to_visit->to = ptf;
	to_visit->oldv = *pt0;
	to_visit->ground = ground;
	/* fool the system into thinking we had a variable there */
	*pt0 = AbsAppl(HR);
	to_visit ++;
#else
	if (pt0 < pt0_end) {
	  if (to_visit ++ >= (CELL **)AuxSp) {
	    goto heap_overflow;
	  }
	  to_visit->start_cp = pt0;
	  to_visit->end_cp = pt0_end;
	  to_visit->to = ptf;
	  to_visit->ground = ground;
	  to_visit ++;
	}
#endif
	ground = (f != FunctorMutable);
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	/* store the functor for the new term */
	HR[0] = (CELL)f;
	ptf = HR+1;
	HR += 1+d0;
	if (HR > ASP - MIN_ARENA_SIZE) {
	  goto overflow;
	}
      } else {
	/* just copy atoms or integers */
	*ptf++ = d0;
      }
      continue;
    }

    derefa_body(d0, ptd0, copy_term_unk, copy_term_nvar);
    ground = FALSE;
    /* don't need to copy variables if we want to share the global term */
    if ((share && ptd0 < HB && ptd0 > H0) ||
	(ptd0 >= HLow && ptd0 < HR)) { 
      /* we have already found this cell */
      *ptf++ = (CELL) ptd0;
    } else {
#if COROUTINING
      if (copy_att_vars && GlobalIsAttachedTerm((CELL)ptd0)) {
	/* if unbound, call the standard copy term routine */
	struct cp_frame *bp;
	CELL new;

	bp = to_visit;
	if (!GLOBAL_attas[ExtFromCell(ptd0)].copy_term_op(ptd0, &bp, ptf PASS_REGS)) {
	  goto overflow;
	}
	to_visit = bp;
	new = *ptf;
	if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
	  /* Trail overflow */
	  if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	    goto trail_overflow;
	  }
	}
	Bind_and_Trail(ptd0, new);
	ptf++;
      } else {
#endif
	/* first time we met this term */
	RESET_VARIABLE(ptf);
	if ((ADDR)TR > LOCAL_TrailTop-MIN_ARENA_SIZE)
	  goto trail_overflow;
	Bind_and_Trail(ptd0, (CELL)ptf);
	ptf++;
#ifdef COROUTINING
      }
#endif
    }
  }

  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
#ifdef RATIONAL_TREES
    *pt0 = to_visit->oldv;
#endif
    ground = (ground && to_visit->ground);
    goto loop;
  }

  /* restore our nice, friendly, term to its original state */
  HB = HB0;
  clean_dirty_tr(TR0 PASS_REGS);
  /* follow chain of multi-assigned variables */
  return 0;

 overflow:
  /* oops, we're in trouble */
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  return -1;

 heap_overflow:
  /* oops, we're in trouble */
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  return -2;

 trail_overflow:
  /* oops, we're in trouble */
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  return -4;
}

static Term
CopyTermToArena(Term t, Term arena, bool share, bool copy_att_vars, UInt arity, Term *newarena, size_t min_grow USES_REGS)
{
  size_t old_size = ArenaSz(arena);
  CELL *oldH = HR;
  CELL *oldHB = HB;
  CELL *oldASP = ASP;
  int res = 0;
  Term tn;

 restart:
  t = Deref(t);
  if (IsVarTerm(t)) {
    ASP = ArenaLimit(arena);
    HR = HB = ArenaPt(arena);
#if COROUTINING
    if (GlobalIsAttachedTerm(t)) {
      CELL *Hi;

      *HR = t;
      Hi = HR+1;
      HR += 2;
      if ((res = copy_complex_term(Hi-2, Hi-1, share, copy_att_vars, Hi, Hi PASS_REGS)) < 0) 
	goto error_handler;
      CloseArena(oldH, oldHB, oldASP, newarena, old_size PASS_REGS);
      return Hi[0];
    }
#endif
    if (share && VarOfTerm(t) > ArenaPt(arena)) {
      CloseArena(oldH, oldHB, oldASP, newarena, old_size PASS_REGS);
      return t;
    }
    tn = MkVarTerm();
    if (HR > ASP - MIN_ARENA_SIZE) {
      res = -1;
      goto error_handler;
    }
    CloseArena(oldH, oldHB, oldASP, newarena, old_size PASS_REGS);
    return tn;
  } else if (IsAtomOrIntTerm(t)) {
    return t;
  } else if (IsPairTerm(t)) {
    Term tf;
    CELL *ap;
    CELL *Hi;

    if (share && ArenaPt(arena) > RepPair(t)) {
      return t;
    }
    HR = HB = ArenaPt(arena);
    ASP = ArenaLimit(arena);
    ap = RepPair(t);
    Hi = HR;
    tf = AbsPair(HR);
    HR += 2;
    if ((res = copy_complex_term(ap-1, ap+1, share, copy_att_vars, Hi, Hi PASS_REGS)) < 0) {
	goto error_handler;
    }
    CloseArena(oldH, oldHB, oldASP, newarena, old_size PASS_REGS);
    return tf;
  } else {
    Functor f;
    Term tf;
    CELL *HB0;
    CELL *ap;

    if (share && ArenaPt(arena) > RepAppl(t)) {
      return t;
    }
    HR = HB = ArenaPt(arena);
    ASP = ArenaLimit(arena);
    f = FunctorOfTerm(t);
    HB0 = HR;
    ap = RepAppl(t);
    tf = AbsAppl(HR);
    HR[0] = (CELL)f;
    if (IsExtensionFunctor(f)) {
      switch((CELL)f) {
      case (CELL)FunctorDBRef:
	CloseArena(oldH, oldHB, oldASP, newarena, old_size PASS_REGS);
	return t;
      case (CELL)FunctorLongInt:
	if (HR > ASP - (MIN_ARENA_SIZE+3)) {
	  res = -1;
	  goto error_handler;
	}
	HR[1] = ap[1];
	HR[2] = EndSpecials;
	HR += 3;
	break;
      case (CELL)FunctorDouble:
	if (HR > ASP - (MIN_ARENA_SIZE+(2+SIZEOF_DOUBLE/sizeof(CELL)))) {
	  res = -1;
	  goto error_handler;
	}
	HR[1] = ap[1];
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	HR[2] = ap[2];
	HR[3] = EndSpecials;
	HR += 4;
#else
	HR[2] = EndSpecials;
	HR += 3;
#endif
	break;
      case (CELL)FunctorString:
	if (HR > ASP - (MIN_ARENA_SIZE+3+ap[1])) {
	  res = -1;
	  goto error_handler;
	}
	memcpy(HR, ap, sizeof(CELL)*(3+ap[1]));
	HR += ap[1]+3;
	break;
      default:
	{
	  UInt sz = ArenaSz(t), i;

	  if (HR > ASP - (MIN_ARENA_SIZE+sz)) {
	    res = -1;
	    goto error_handler;
	  }
	  for (i = 1; i < sz; i++) {
	    HR[i] = ap[i];
	  }
	  HR += sz;
	}
      }
    } else {
      HR += 1+ArityOfFunctor(f);
      if (HR > ASP-MIN_ARENA_SIZE) {
	res = -1;
	goto error_handler;
      } 
      if ((res = copy_complex_term(ap, ap+ArityOfFunctor(f), share, copy_att_vars, HB0+1, HB0 PASS_REGS)) < 0) {
	goto error_handler;
      }
    }
    CloseArena(oldH, oldHB, oldASP, newarena, old_size PASS_REGS);
    return tf;
  }
 error_handler:
  HR = HB;
  CloseArena(oldH, oldHB, oldASP, newarena, old_size PASS_REGS);
  XREGS[arity+1] = t;
  XREGS[arity+2] = arena;
  XREGS[arity+3] = (CELL)newarena;
  {
    CELL *old_top = ArenaLimit(*newarena);
    ASP = oldASP;
    HR = oldH;
    HB = oldHB;
    switch (res) {
    case -1:
      if (arena == LOCAL_GlobalArena)
	LOCAL_GlobalArenaOverflows++;
      if (!GrowArena(arena, old_top, old_size, min_grow, arity+3 PASS_REGS)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	return 0L;
      }
      break;
    default: /* temporary space overflow */
      if (!Yap_ExpandPreAllocCodeSpace(0,NULL,TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, TermNil, LOCAL_ErrorMessage);
	return 0L;
      }
    }
  }
  oldH = HR;
  oldHB = HB;
  oldASP = ASP;
  newarena = (CELL *)XREGS[arity+3];
  arena = Deref(XREGS[arity+2]);
  t = XREGS[arity+1];
  old_size = ArenaSz(arena);
  goto restart;
}

static Term
CreateTermInArena(Term arena, Atom Na, UInt Nar, UInt arity, Term *newarena, Term init USES_REGS)
{
  UInt old_size = ArenaSz(arena);
  CELL *oldH = HR;
  CELL *oldHB = HB;
  CELL *oldASP = ASP;
  Term tf;
  CELL *HB0;
  Functor f = Yap_MkFunctor(Na, Nar);
  UInt i;

 restart:
  HR = HB = ArenaPt(arena);
  ASP = ArenaLimit(arena);
  HB0 = HR;
  tf = AbsAppl(HR);
  HR[0] = (CELL)f;
  HR += 1+ArityOfFunctor(f);
  if (HR > ASP-MIN_ARENA_SIZE) {
    /* overflow */
    HR = HB;
    CloseArena(oldH, oldHB, oldASP, newarena, old_size PASS_REGS);
    XREGS[arity+1] = arena;
    XREGS[arity+2] = (CELL)newarena;
    {
      CELL *old_top = ArenaLimit(*newarena);
      ASP = oldASP;
      HR = oldH;
      HB = oldHB;
      if (arena == LOCAL_GlobalArena)
	LOCAL_GlobalArenaOverflows++;
      if (!GrowArena(arena, old_top, old_size, Nar*sizeof(CELL), arity+2 PASS_REGS)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, "while creating large global term");
	return 0L;
      }
    }
    oldH = HR;
    oldHB = HB;
    oldASP = ASP;
    newarena = (CELL *)XREGS[arity+2];
    arena = Deref(XREGS[arity+1]);
    old_size = ArenaSz(arena);
    goto restart;
  } 
  if (init == 0L) {
    for (i=1; i<=Nar; i++) {
      RESET_VARIABLE(HB0+i);
    }
  } else {
    for (i=1; i<=Nar; i++) {
      HB0[i] = init;
    }
  }
  CloseArena(oldH, oldHB, oldASP, newarena, old_size PASS_REGS);
  return tf;
}

inline static GlobalEntry *
FindGlobalEntry(Atom at USES_REGS)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  READ_LOCK(ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0) {
    GlobalEntry *pe = RepGlobalProp(p0);
    if ( pe->KindOfPE == GlobalProperty
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

inline static GlobalEntry *
GetGlobalEntry(Atom at USES_REGS)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);
  GlobalEntry *new;

  WRITE_LOCK(ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0) {
    GlobalEntry *pe = RepGlobalProp(p0);
    if ( pe->KindOfPE == GlobalProperty
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
  AddPropToAtom(ae, (PropEntry *)new);
  RESET_VARIABLE(&new->global);
  WRITE_UNLOCK(ae->ARWLock);
  return new;
}

static UInt
garena_overflow_size(CELL *arena USES_REGS)
{
  UInt dup = (((CELL *)arena-H0)*sizeof(CELL))>>3;
  if (dup < 64*1024*LOCAL_GlobalArenaOverflows)
    dup = 64*1024*LOCAL_GlobalArenaOverflows;
  if (dup > 1024*1024)
    return 1024*1024;
  return dup;
}

static Int
p_nb_setarg( USES_REGS1 )
{
  Term wheret = Deref(ARG1);
  Term dest;
  Term to;
  UInt arity, pos;
  CELL *destp;

  if (IsVarTerm(wheret)) {
    Yap_Error(INSTANTIATION_ERROR,wheret,"nb_setarg");
    return FALSE; 
  }
  if (!IsIntegerTerm(wheret)) {
    Yap_Error(TYPE_ERROR_INTEGER,wheret,"nb_setarg");
    return FALSE; 
  }
  pos = IntegerOfTerm(wheret);
  dest = Deref(ARG2);
  if (IsVarTerm(dest)) {
    Yap_Error(INSTANTIATION_ERROR,dest,"nb_setarg");
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
  to = CopyTermToArena(ARG3, LOCAL_GlobalArena, FALSE, TRUE, 3, &LOCAL_GlobalArena, garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return FALSE;

  dest = Deref(ARG2);
  if (IsPairTerm(dest)) {
    destp = RepPair(dest)-1;
  } else {
    destp = RepAppl(dest);
  }
  destp[pos] = to;
  return TRUE;
}

static Int
p_nb_set_shared_arg( USES_REGS1 )
{
  Term wheret = Deref(ARG1);
  Term dest = Deref(ARG2);
  Term to;
  UInt arity, pos;
  CELL *destp;

  if (IsVarTerm(wheret)) {
    Yap_Error(INSTANTIATION_ERROR,wheret,"nb_setarg");
    return FALSE; 
  }
  if (!IsIntegerTerm(wheret)) {
    Yap_Error(TYPE_ERROR_INTEGER,wheret,"nb_setarg");
    return FALSE; 
  }
  pos = IntegerOfTerm(wheret);
  if (IsVarTerm(dest)) {
    Yap_Error(INSTANTIATION_ERROR,dest,"nb_setarg");
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
  to = CopyTermToArena(ARG3, LOCAL_GlobalArena, TRUE, TRUE, 3, &LOCAL_GlobalArena, garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return FALSE;
  if (IsPairTerm(dest)) {
    destp = RepPair(dest)-1;
  } else {
    destp = RepAppl(dest);
  }
  destp[pos] = to;
  return TRUE;
}

static Int
p_nb_linkarg( USES_REGS1 )
{
  Term wheret = Deref(ARG1);
  Term dest = Deref(ARG2);
  UInt arity, pos;
  CELL *destp;

  if (IsVarTerm(wheret)) {
    Yap_Error(INSTANTIATION_ERROR,wheret,"nb_setarg");
    return FALSE; 
  }
  if (!IsIntegerTerm(wheret)) {
    Yap_Error(TYPE_ERROR_INTEGER,wheret,"nb_setarg");
    return FALSE; 
  }
  pos = IntegerOfTerm(wheret);
  if (IsVarTerm(dest)) {
    Yap_Error(INSTANTIATION_ERROR,dest,"nb_setarg");
    return FALSE; 
  } else if (IsPrimitiveTerm(dest)) {
    arity = 0;
    destp = NULL;
  } else if (IsPairTerm(dest)) {
    arity = 2;
    destp = RepPair(dest)-1;
  } else {
    arity = ArityOfFunctor(FunctorOfTerm(dest));
    destp = RepAppl(dest);
  }
  if (pos < 1 || pos > arity)
    return FALSE;
  destp[pos] = Deref(ARG3);
  return TRUE;
}

static Int
p_nb_linkval( USES_REGS1 )
{
  Term t = Deref(ARG1), to;
  GlobalEntry *ge;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_linkval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
      Yap_Error(TYPE_ERROR_ATOM,t,"nb_linkval");
      return (FALSE);
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  to = Deref(ARG2);
  WRITE_LOCK(ge->GRWLock);
  ge->global=to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}



static Int
p_nb_create_accumulator( USES_REGS1 )
{
  Term t = Deref(ARG1), acct, to, t2;
  CELL *destp;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_create_accumulator");
    return FALSE; 
  }
  if (!IsIntegerTerm(t) && !IsBigIntTerm(t) && !IsFloatTerm(t)) {
    Yap_Error(TYPE_ERROR_NUMBER,t,"nb_create_accumulator");
    return FALSE; 
  }
  acct = Yap_MkApplTerm(FunctorGNumber,1,&t);
  if (!Yap_unify(ARG2, acct)) {
    return FALSE;
  }
  to = CopyTermToArena(t, LOCAL_GlobalArena, TRUE, TRUE, 2, &LOCAL_GlobalArena, garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return FALSE;
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    return Yap_unify(t2, Yap_MkApplTerm(FunctorGNumber,1,&to) );
  }
  destp = RepAppl(Deref(ARG2));
  destp[1] = to;
  return TRUE;
}

static Int
p_nb_add_to_accumulator( USES_REGS1 )
{
  Term t = Deref(ARG1), t0, tadd;
  Functor f;
  CELL *destp;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_create_accumulator");
    return FALSE; 
  }
  if (!IsApplTerm(t)) {
    Yap_Error(TYPE_ERROR_NUMBER,t,"nb_accumulator_value");
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
    Yap_Error(INSTANTIATION_ERROR,tadd,"nb_create_accumulator");
    return FALSE; 
  }
  if (IsIntegerTerm(t0) && IsIntegerTerm(tadd)) {
    Int i0 = IntegerOfTerm(t0);
    Int i1 = IntegerOfTerm(tadd);
    Term new = MkIntegerTerm(i0+i1);

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
	new = CopyTermToArena(new, LOCAL_GlobalArena, TRUE, TRUE, 2, &LOCAL_GlobalArena, garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
	destp = RepAppl(Deref(ARG1));
	destp[1] = new;
      }
    }
    return TRUE;
  }
  if (IsFloatTerm(t0) && IsFloatTerm(tadd)) {
    Float f0 = FloatOfTerm(t0);
    Float f1 = FloatOfTerm(tadd);
    Term new = MkFloatTerm(f0+f1);
    CELL *target = RepAppl(t0);
    CELL *source = RepAppl(new);

#if  SIZEOF_DOUBLE == 2*SIZEOF_INT_P
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
    new = CopyTermToArena(new, LOCAL_GlobalArena, TRUE, TRUE, 2, &LOCAL_GlobalArena, garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS)  PASS_REGS);
    destp = RepAppl(Deref(ARG1));
    destp[1] = new;    

    return TRUE;
  }
  return FALSE;
}


static Int
p_nb_accumulator_value( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Functor f;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_accumulator_value");
    return FALSE; 
  }
  if (!IsApplTerm(t)) {
    Yap_Error(TYPE_ERROR_NUMBER,t,"nb_accumulator_value");
    return FALSE; 
  }
  f = FunctorOfTerm(t);
  if (f != FunctorGNumber) {
    return FALSE;
  }
  return Yap_unify(ArgOfTerm(1,t), ARG2);
}


Term
Yap_SetGlobalVal(Atom at, Term t0)
{
  CACHE_REGS
  Term to;
  GlobalEntry *ge;
  ge = GetGlobalEntry(at PASS_REGS);
  to = CopyTermToArena(t0, LOCAL_GlobalArena, FALSE, TRUE, 2, &LOCAL_GlobalArena, garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return to;
  WRITE_LOCK(ge->GRWLock);
  ge->global=to;
  WRITE_UNLOCK(ge->GRWLock);
  return to;
}

Term
Yap_SaveTerm(Term t0)
{
  CACHE_REGS
  Term to;
  to = CopyTermToArena(t0, LOCAL_GlobalArena, FALSE, TRUE, 2, &LOCAL_GlobalArena, garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return to;
  return to;
}

static Int
p_nb_setval( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_setval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
      Yap_Error(TYPE_ERROR_ATOM,t,"nb_setval");
      return (FALSE);
  }
  return Yap_SetGlobalVal(AtomOfTerm(t), ARG2);
}

static Int
p_nb_set_shared_val( USES_REGS1 )
{
  Term t = Deref(ARG1), to;
  GlobalEntry *ge;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_setval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
      Yap_Error(TYPE_ERROR_ATOM,t,"nb_setval");
      return (FALSE);
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  to = CopyTermToArena(ARG2, LOCAL_GlobalArena, TRUE, TRUE, 2, &LOCAL_GlobalArena, garena_overflow_size(ArenaPt(LOCAL_GlobalArena) PASS_REGS) PASS_REGS);
  if (to == 0L)
    return FALSE;
  WRITE_LOCK(ge->GRWLock);
  ge->global=to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}

static Int
p_b_setval( USES_REGS1 )
{
  Term t = Deref(ARG1);
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"b_setval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
      Yap_Error(TYPE_ERROR_ATOM,t,"b_setval");
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
  Yap_Error(SYSTEM_ERROR,t,"update_array");
  return FALSE;
#endif
}

static int
undefined_global( USES_REGS1 ) {
  Term t3 = Deref(ARG3);

  if (IsApplTerm(t3)) {
    if (FunctorOfTerm(t3) == FunctorEq)
      return Yap_unify( ArgOfTerm(1, t3) , ArgOfTerm(2, t3) );
    return FALSE;
  }
  return Yap_unify(t3, TermNil);
}

static Int
p_nb_getval( USES_REGS1 )
{
  Term t = Deref(ARG1), to;
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_getval");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM,t,"nb_getval");
    return FALSE;
  }
  ge = FindGlobalEntry(AtomOfTerm(t) PASS_REGS);
  if (!ge) 
    return undefined_global( PASS_REGS1 );
  READ_LOCK(ge->GRWLock);
  to = ge->global;
  if (IsVarTerm(to) && IsUnboundVar(VarOfTerm(to))) {
    Term t = MkVarTerm();
    YapBind(VarOfTerm(to), t);
    to = t;
  }
  READ_UNLOCK(ge->GRWLock);
  if (to == TermFoundVar) {
    return FALSE;
  }
  return Yap_unify(ARG2, to);
}


static Int 
nbdelete(Atom at USES_REGS)
{
  GlobalEntry *ge, *g;
  AtomEntry *ae;
  Prop gp, g0;

  ge = FindGlobalEntry(at PASS_REGS);
  if (!ge) {
    Yap_Error(EXISTENCE_ERROR_VARIABLE,MkAtomTerm(at),"nb_delete");
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
  Yap_FreeCodeSpace((char *)ge);
  return TRUE;
}

Int
Yap_DeleteGlobal(Atom at)
{
  CACHE_REGS
  return nbdelete(at PASS_REGS);
}

static Int
p_nb_delete( USES_REGS1 )
{
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_delete");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM,t,"nb_delete");
    return FALSE;
  }
  return nbdelete(AtomOfTerm(t) PASS_REGS);
}

static Int
p_nb_create( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Term tname = Deref(ARG2);
  Term tarity = Deref(ARG3);
  Term to;
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_create");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM,t,"nb_create");
    return FALSE;
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  if (!ge) {
    Yap_Error(EXISTENCE_ERROR_VARIABLE,t,"nb_create");
    return FALSE;
  }
  if (IsVarTerm(tarity)) {
    Yap_Error(INSTANTIATION_ERROR,tarity,"nb_create");
    return FALSE;
  } else if (!IsIntegerTerm(tarity)) {
    Yap_Error(TYPE_ERROR_INTEGER,tarity,"nb_create");
    return FALSE;
  }
  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR,tname,"nb_create");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM,tname,"nb_create");
    return FALSE;
  }
  to = CreateTermInArena(LOCAL_GlobalArena, AtomOfTerm(tname), IntegerOfTerm(tarity),  3, &LOCAL_GlobalArena, 0L PASS_REGS);
  if (!to)
    return FALSE;
  WRITE_LOCK(ge->GRWLock);
  ge->global=to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}

static Int
p_nb_create2( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Term tname = Deref(ARG2);
  Term tarity = Deref(ARG3);
  Term tinit = Deref(ARG4);
  Term to;
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_create");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM,t,"nb_create");
    return FALSE;
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  if (!ge) {
    Yap_Error(EXISTENCE_ERROR_VARIABLE,t,"nb_create");
    return FALSE;
  }
  if (IsVarTerm(tarity)) {
    Yap_Error(INSTANTIATION_ERROR,tarity,"nb_create");
    return FALSE;
  } else if (!IsIntegerTerm(tarity)) {
    Yap_Error(TYPE_ERROR_INTEGER,tarity,"nb_create");
    return FALSE;
  }
  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR,tname,"nb_create");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM,tname,"nb_create");
    return FALSE;
  }
  if (IsVarTerm(tinit)) {
    Yap_Error(INSTANTIATION_ERROR,tname,"nb_create");
    return FALSE;
  } else if (!IsAtomTerm(tinit)) {
    Yap_Error(TYPE_ERROR_ATOM,tname,"nb_create");
    return FALSE;
  }
  to = CreateTermInArena(LOCAL_GlobalArena, AtomOfTerm(tname), IntegerOfTerm(tarity), 4, &LOCAL_GlobalArena, tinit PASS_REGS);
  if (!to)
    return FALSE;
  WRITE_LOCK(ge->GRWLock);
  ge->global=to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}

/* a non-backtrackable queue is a term of the form $array(Arena,Start,End,Size) plus an Arena. */

static Int 
nb_queue(UInt arena_sz USES_REGS)
{
  Term queue_arena, queue, ar[QUEUE_FUNCTOR_ARITY], *nar;
  Term t = Deref(ARG1);

  LOCAL_DepthArenas++;
  if (!IsVarTerm(t)) {
    if (!IsApplTerm(t)) {
      return FALSE;
    }
    return (FunctorOfTerm(t) == FunctorNBQueue);
  }
  ar[QUEUE_ARENA] = 
    ar[QUEUE_HEAD] =
    ar[QUEUE_TAIL] =
    ar[QUEUE_SIZE] =
    MkIntTerm(0);
  queue = Yap_MkApplTerm(FunctorNBQueue,QUEUE_FUNCTOR_ARITY,ar);
  if (!Yap_unify(queue,ARG1))
    return FALSE;
  if (arena_sz < 4*1024)
    arena_sz = 4*1024;
  queue_arena = NewArena(arena_sz, worker_id, 1, NULL);
  if (queue_arena == 0L) {
    return FALSE;
  }
  nar = RepAppl(Deref(ARG1))+1;
  nar[QUEUE_ARENA] = queue_arena;
  return TRUE;
}

static Int
p_nb_queue( USES_REGS1 )
{
  UInt arena_sz = (ASP-HR)/16;
  if (LOCAL_DepthArenas > 1)
    arena_sz /= LOCAL_DepthArenas;
  if (arena_sz < MIN_ARENA_SIZE)
    arena_sz = MIN_ARENA_SIZE;
  if (arena_sz > MAX_ARENA_SIZE)
    arena_sz = MAX_ARENA_SIZE;
  return nb_queue(arena_sz PASS_REGS);
}

static Int
p_nb_queue_sized( USES_REGS1 )
{
  Term t = Deref(ARG2);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"nb_queue");
    return FALSE;
  }
  if (!IsIntegerTerm(t)) {
      Yap_Error(TYPE_ERROR_INTEGER,t,"nb_queue");
      return FALSE;
  }
  return nb_queue((UInt)IntegerOfTerm(t) PASS_REGS);
}

static CELL *
GetQueue(Term t, char* caller)
{
  t = Deref(t);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,caller);
    return NULL;
  } 
  if (!IsApplTerm(t)) {
      Yap_Error(TYPE_ERROR_COMPOUND,t,caller);
      return NULL;
  }
  if (FunctorOfTerm(t) != FunctorNBQueue) {
      Yap_Error(DOMAIN_ERROR_ARRAY_TYPE,t,caller);
      return NULL;
  }
  return RepAppl(t)+1;
}

static Term
GetQueueArena(CELL *qd, char* caller)
{
  Term t = Deref(qd[QUEUE_ARENA]);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,caller);
    return 0L;
  } 
  if (!IsApplTerm(t)) {
      Yap_Error(TYPE_ERROR_COMPOUND,t,caller);
      return 0L;
  }
  if (FunctorOfTerm(t) != FunctorBigInt) {
      Yap_Error(DOMAIN_ERROR_ARRAY_TYPE,t,caller);
      return 0L;
  }
  return t;
}

static void
RecoverArena(Term arena USES_REGS)
{
  CELL *pt = ArenaPt(arena),
    *max = ArenaLimit(arena);
  
  if (max == HR) {
    HR = pt;
  }
}

static Int
p_nb_queue_close( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Int out;

  LOCAL_DepthArenas--;
  if (!IsVarTerm(t)) {
    CELL *qp;

    qp = GetQueue(t, "queue/3");
    if (qp == NULL) {
      return
	Yap_unify(ARG3, ARG2);
    }
    if (qp[QUEUE_ARENA] != MkIntTerm(0))
      RecoverArena(qp[QUEUE_ARENA] PASS_REGS);
    if (qp[QUEUE_SIZE] == MkIntTerm(0)) {
      return 
	Yap_unify(ARG3, ARG2);
    }
    out = 
      Yap_unify(ARG3, qp[QUEUE_TAIL]) &&
      Yap_unify(ARG2, qp[QUEUE_HEAD]);
    qp[-1] = (CELL)Yap_MkFunctor(AtomHeap,1);
    qp[QUEUE_ARENA] =
    qp[QUEUE_HEAD] = 
    qp[QUEUE_TAIL] = MkIntegerTerm(0);
    return out;
  }
  Yap_Error(INSTANTIATION_ERROR,t,"queue/3");
  return FALSE;
}

static Int
p_nb_queue_enqueue( USES_REGS1 )
{
  CELL *qd = GetQueue(ARG1,"enqueue"), *oldH, *oldHB;
  UInt old_sz;
  Term arena, qsize, to;
  UInt min_size;

  if (!qd)
    return FALSE;
  arena = GetQueueArena(qd,"enqueue");
  if (arena == 0L)
    return FALSE;
  if (IsPairTerm(qd[QUEUE_HEAD])) {
    min_size = ArenaPt(arena)-RepPair(qd[QUEUE_HEAD]);
  } else {
    min_size = 0L;
  }
  to = CopyTermToArena(ARG2, arena, FALSE, TRUE, 2, qd+QUEUE_ARENA, min_size PASS_REGS);
  if (to == 0L)
    return FALSE;
  qd = GetQueue(ARG1,"enqueue");
  arena = GetQueueArena(qd,"enqueue");
  /* garbage collection ? */
  oldH = HR;
  oldHB = HB;
  HR = HB = ArenaPt(arena);
  old_sz = ArenaSz(arena);
  qsize = IntegerOfTerm(qd[QUEUE_SIZE]);
  while (old_sz < MIN_ARENA_SIZE) {
    UInt gsiz = HR-RepPair(qd[QUEUE_HEAD]);
    HR = oldH;
    HB = oldHB;
    if (gsiz > 1024*1024) {
      gsiz = 1024*1024;
    } else if (gsiz < 1024) {
      gsiz = 1024;
    }
    ARG3 = to;
    /*    fprintf(stderr,"growing %ld cells\n",(unsigned long int)gsiz);*/
    if (!GrowArena(arena, ArenaLimit(arena), old_sz, gsiz, 3 PASS_REGS)) {
      Yap_Error(OUT_OF_STACK_ERROR, arena, LOCAL_ErrorMessage);
      return 0L;
    }    
    to = ARG3;
    qd = RepAppl(Deref(ARG1))+1;
    arena = GetQueueArena(qd,"enqueue");
    oldH = HR;
    oldHB = HB;
    HR = HB = ArenaPt(arena);
    old_sz = ArenaSz(arena);    
  }
  qd[QUEUE_SIZE] = Global_MkIntegerTerm(qsize+1);
  if (qsize == 0) {
    qd[QUEUE_HEAD] = AbsPair(HR);
  } else {
    *VarOfTerm(qd[QUEUE_TAIL]) = AbsPair(HR);
  }
  *HR++ = to;
  RESET_VARIABLE(HR);
  qd[QUEUE_TAIL] = (CELL)HR;
  HR++;
  CloseArena(oldH, oldHB, ASP, qd+QUEUE_ARENA, old_sz PASS_REGS);
  return TRUE;
}

static Int
p_nb_queue_dequeue( USES_REGS1 )
{
  CELL *qd = GetQueue(ARG1,"dequeue");
  UInt old_sz, qsz;
  Term arena, out;
  CELL *oldH, *oldHB;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[QUEUE_SIZE]);
  if (qsz == 0)
    return FALSE;
  arena = GetQueueArena(qd,"dequeue");
  if (arena == 0L)
    return FALSE;
  old_sz = ArenaSz(arena);
  out = HeadOfTerm(qd[QUEUE_HEAD]);
  qd[QUEUE_HEAD] = TailOfTerm(qd[QUEUE_HEAD]);
  /* garbage collection ? */
  oldH = HR;
  oldHB = HB;
  qd[QUEUE_SIZE] = Global_MkIntegerTerm(qsz-1);
  CloseArena(oldH, oldHB, ASP, &arena, old_sz PASS_REGS);
  return Yap_unify(out, ARG2);
}

/* purge an entry from the queue, replacing it by [] */
static Int
p_nb_queue_replace( USES_REGS1 )
{
  CELL *qd = GetQueue(ARG1,"dequeue");
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

static Int
p_nb_queue_peek( USES_REGS1 )
{
  CELL *qd = GetQueue(ARG1,"queue_peek");
  UInt qsz;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[QUEUE_SIZE]);
  if (qsz == 0)
    return FALSE;
  return Yap_unify(HeadOfTerm(qd[QUEUE_HEAD]), ARG2);
}

static Int
p_nb_queue_empty( USES_REGS1 )
{
  CELL *qd = GetQueue(ARG1,"queue_empty");

  if (!qd)
    return FALSE;
  return (IntegerOfTerm(qd[QUEUE_SIZE]) == 0);
}

static Int
p_nb_queue_size( USES_REGS1 )
{
  CELL *qd = GetQueue(ARG1,"queue_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2,qd[QUEUE_SIZE]);
}

static Int
p_nb_queue_show( USES_REGS1 )
{
  CELL *qd = GetQueue(ARG1,"queue_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2,qd[QUEUE_HEAD]);
}


static CELL *
GetHeap(Term t, char* caller)
{
  t = Deref(t);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,caller);
    return NULL;
  } 
  if (!IsApplTerm(t)) {
      Yap_Error(TYPE_ERROR_COMPOUND,t,caller);
      return NULL;
  }
  return RepAppl(t)+1;
}

static Term
MkZeroApplTerm(Functor f, UInt sz USES_REGS)
{
  Term t0, tf;
  CELL *pt;

  if (HR+(sz+1) > ASP-1024)
    return TermNil;
  tf = AbsAppl(HR);
  *HR = (CELL)f;
  t0 = MkIntTerm(0);
  pt = HR+1;
  while (sz--) {
    *pt++ = t0;
  }
  HR = pt;
  return tf;
}

static Int
p_nb_heap( USES_REGS1 )
{
  Term heap_arena, heap, *ar, *nar;
  UInt hsize;
  Term tsize = Deref(ARG1);
  UInt arena_sz = (HR-H0)/16;

  if (IsVarTerm(tsize)) {
    Yap_Error(INSTANTIATION_ERROR,tsize,"nb_heap");
    return FALSE;
  } else {
    if (!IsIntegerTerm(tsize)) {
      Yap_Error(TYPE_ERROR_INTEGER,tsize,"nb_heap");
      return FALSE;
    }
    hsize = IntegerOfTerm(tsize);
  }

  while ((heap = MkZeroApplTerm(Yap_MkFunctor(AtomHeap,2*hsize+HEAP_START+1),2*hsize+HEAP_START+1 PASS_REGS)) == TermNil) {
    if (!Yap_gcl((2*hsize+HEAP_START+1)*sizeof(CELL), 2, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  if (!Yap_unify(heap,ARG2))
    return FALSE;
  ar = RepAppl(heap)+1;
  ar[HEAP_ARENA] = 
    ar[HEAP_SIZE] = 
    MkIntTerm(0);
  ar[HEAP_MAX] = tsize;
  if (arena_sz < 1024)
    arena_sz = 1024;
  heap_arena = NewArena(arena_sz,worker_id,1,NULL);
  if (heap_arena == 0L) {
    return FALSE;
  }
  nar = RepAppl(Deref(ARG2))+1;
  nar[HEAP_ARENA] = heap_arena;
  return TRUE;
}

static Int
p_nb_heap_close( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (!IsVarTerm(t)) {
    CELL *qp;

    qp = RepAppl(t)+1;
    if (qp[HEAP_ARENA] != MkIntTerm(0))
      RecoverArena(qp[HEAP_ARENA] PASS_REGS);
    qp[-1] = (CELL)Yap_MkFunctor(AtomHeap,1);
    qp[0] = MkIntegerTerm(0);
    return TRUE;
  }
  Yap_Error(INSTANTIATION_ERROR,t,"heap_close/1");
  return FALSE;
}

static void
PushHeap(CELL *pt, UInt off)
{
  while (off) {
    UInt noff = (off+1)/2-1;
    if (Yap_compare_terms(pt[2*off], pt[2*noff]) < 0) {
      Term tk = pt[2*noff];
      Term tv = pt[2*noff+1];
      pt[2*noff] = pt[2*off];
      pt[2*noff+1] = pt[2*off+1];
      pt[2*off] = tk;
      pt[2*off+1] = tv;
      off = noff;
    } else {
      return;
    }
  }
}

static void
DelHeapRoot(CELL *pt, UInt sz)
{
  UInt indx = 0;
  Term tk, tv;

  sz--;
  tk = pt[2*sz];
  tv = pt[2*sz+1];
  pt[2*sz] = TermNil;
  pt[2*sz+1] = TermNil;
  while (TRUE) {
    if (sz < 2*indx+3 || Yap_compare_terms(pt[4*indx+2],pt[4*indx+4]) < 0) {
      if (sz < 2*indx+2 || Yap_compare_terms(tk, pt[4*indx+2]) < 0) {
	pt[2*indx] = tk;
	pt[2*indx+1] = tv;
	return;
      } else {
	pt[2*indx] = pt[4*indx+2];
	pt[2*indx+1] = pt[4*indx+3];
	indx = 2*indx+1;
      }
    } else {
      if (Yap_compare_terms(tk, pt[4*indx+4]) < 0) {
	pt[2*indx] = tk;
	pt[2*indx+1] = tv;
	return;
      } else {
	pt[2*indx] = pt[4*indx+4];
	pt[2*indx+1] = pt[4*indx+5];
	indx = 2*indx+2;
      }
    }
  }
}

static Int
p_nb_heap_add_to_heap( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"add_to_heap"), *oldH, *oldHB, *pt;
  UInt hsize, hmsize, old_sz;
  Term arena, to, key;
  UInt mingrow;

  if (!qd)
    return FALSE;
 restart:
  hsize = IntegerOfTerm(qd[HEAP_SIZE]);
  hmsize = IntegerOfTerm(qd[HEAP_MAX]);
  if (hsize == hmsize) {
    CELL *top = qd+(HEAP_START+2*hmsize);
    UInt extra_size;

    if (hmsize <= 64*1024) {
      extra_size = 64*1024;
    } else {
      extra_size = hmsize;
    }
    if ((extra_size=Yap_InsertInGlobal(top, extra_size*2*sizeof(CELL)))==0) {
      Yap_Error(OUT_OF_STACK_ERROR,TermNil,"No Stack Space for Non-Backtrackable terms");
      return FALSE;
    }
    extra_size = extra_size/(2*sizeof(CELL));
    qd = GetHeap(ARG1,"add_to_heap");
    hmsize += extra_size;
    if (!qd)
      return FALSE;
    qd[-1] = (CELL)Yap_MkFunctor(AtomHeap,2*hmsize+HEAP_START);
    top = qd+(HEAP_START+2*(hmsize-extra_size));
    while (extra_size) {
      RESET_VARIABLE(top);
      RESET_VARIABLE(top+1);
      top+=2;
      extra_size--;
    }
    arena = qd[HEAP_ARENA];
    old_sz = ArenaSz(arena);
    oldH = HR;
    oldHB = HB;
    HR = HB = ArenaPt(arena);
    qd[HEAP_MAX] = Global_MkIntegerTerm(hmsize);
    CloseArena(oldH, oldHB, ASP, qd+HEAP_ARENA, old_sz PASS_REGS);
    goto restart;
  }
  arena = qd[HEAP_ARENA];
  if (arena == 0L)
    return FALSE;
  mingrow = garena_overflow_size(ArenaPt(arena) PASS_REGS);
  ARG2 = CopyTermToArena(ARG2, arena, FALSE, TRUE, 3, qd+HEAP_ARENA, mingrow PASS_REGS);
  qd = GetHeap(ARG1,"add_to_heap");
  arena = qd[HEAP_ARENA];
  to = CopyTermToArena(ARG3, arena, FALSE, TRUE, 3, qd+HEAP_ARENA, mingrow PASS_REGS);
  /* protect key in ARG2 in case there is an overflow while copying to */
  key = ARG2;
  if (key == 0 || to == 0L)
    return FALSE;
  qd = GetHeap(ARG1,"add_to_heap");
  arena = qd[HEAP_ARENA];
  /* garbage collection ? */
  oldH = HR;
  oldHB = HB;
  HR = HB = ArenaPt(arena);
  old_sz = ArenaSz(arena);
  while (old_sz < MIN_ARENA_SIZE) {
    UInt gsiz = hsize*2;

    HR = oldH;
    HB = oldHB;
    if (gsiz > 1024*1024) {
      gsiz = 1024*1024;
    } else if (gsiz < 1024) {
      gsiz = 1024;
    }
    ARG3 = to;
    if (!GrowArena(arena, ArenaLimit(arena), old_sz, gsiz, 3 PASS_REGS)) {
      Yap_Error(OUT_OF_STACK_ERROR, arena, LOCAL_ErrorMessage);
      return 0L;
    }    
    to = ARG3;
    qd = RepAppl(Deref(ARG1))+1;
    arena = qd[HEAP_ARENA];
    oldH = HR;
    oldHB = HB;
    HR = HB = ArenaPt(arena);
    old_sz = ArenaSz(arena);    
  }
  pt = qd+HEAP_START;
  pt[2*hsize] = key;
  pt[2*hsize+1] = to;
  PushHeap(pt, hsize);
  qd[HEAP_SIZE] = Global_MkIntegerTerm(hsize+1);
  CloseArena(oldH, oldHB, ASP, qd+HEAP_ARENA, old_sz PASS_REGS);
  return TRUE;
}

static Int
p_nb_heap_del( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"deheap");
  UInt old_sz, qsz;
  Term arena;
  CELL *oldH, *oldHB;
  Term tk, tv;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  if (qsz == 0)
    return FALSE;
  arena = qd[HEAP_ARENA];
  if (arena == 0L)
    return FALSE;
  old_sz = ArenaSz(arena);
  /* garbage collection ? */
  oldH = HR;
  oldHB = HB;
  qd[HEAP_SIZE] = Global_MkIntegerTerm(qsz-1);
  CloseArena(oldH, oldHB, ASP, &arena, old_sz PASS_REGS);
  tk = qd[HEAP_START];
  tv = qd[HEAP_START+1];
  DelHeapRoot(qd+HEAP_START, qsz);
  return Yap_unify(tk, ARG2) &&
    Yap_unify(tv, ARG3);
}

static Int
p_nb_heap_peek( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"heap_peek");
  UInt qsz;
  Term tk, tv;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  if (qsz == 0)
    return FALSE;
  tk = qd[HEAP_START];
  tv = qd[HEAP_START+1];
  return Yap_unify(tk, ARG2) &&
    Yap_unify(tv, ARG3);
}

static Int
p_nb_heap_empty( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"heap_empty");

  if (!qd)
    return FALSE;
  return (IntegerOfTerm(qd[HEAP_SIZE]) == 0);
}

static Int
p_nb_heap_size( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"heap_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2,qd[HEAP_SIZE]);
}

static Int
p_nb_beam( USES_REGS1 )
{
  Term beam_arena, beam, *ar, *nar;
  UInt hsize;
  Term tsize = Deref(ARG1);
  UInt arena_sz = (HR-H0)/16;

  if (IsVarTerm(tsize)) {
    Yap_Error(INSTANTIATION_ERROR,tsize,"nb_beam");
    return FALSE;
  } else {
    if (!IsIntegerTerm(tsize)) {
      Yap_Error(TYPE_ERROR_INTEGER,tsize,"nb_beam");
      return FALSE;
    }
    hsize = IntegerOfTerm(tsize);
  }
  while ((beam = MkZeroApplTerm(Yap_MkFunctor(AtomHeap,5*hsize+HEAP_START+1),5*hsize+HEAP_START+1 PASS_REGS)) == TermNil) {
    if (!Yap_gcl((4*hsize+HEAP_START+1)*sizeof(CELL), 2, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  if (!Yap_unify(beam,ARG2))
    return FALSE;
  ar = RepAppl(beam)+1;
  ar[HEAP_ARENA] = 
    ar[HEAP_SIZE] = 
    MkIntTerm(0);
  ar[HEAP_MAX] = tsize;
  if (arena_sz < 1024)
    arena_sz = 1024;
  beam_arena = NewArena(arena_sz,worker_id,1,NULL);
  if (beam_arena == 0L) {
    return FALSE;
  }
  nar = RepAppl(Deref(ARG2))+1;
  nar[HEAP_ARENA] = beam_arena;
  return TRUE;
}

static Int
p_nb_beam_close( USES_REGS1 )
{
  return p_nb_heap_close( PASS_REGS1 );
}


/* we have two queues, one with
   Key, IndxQueue2
   the other with
   Key, IndxQueue1, Val
*/
static void
PushBeam(CELL *pt, CELL *npt, UInt hsize, Term key, Term to)
{
  CACHE_REGS
  UInt off = hsize, off2 = hsize;
  Term toff, toff2;

  /* push into first queue */
  while (off) {
    UInt noff = (off+1)/2-1;
    if (Yap_compare_terms(key, pt[2*noff]) < 0) {
      UInt i2 = IntegerOfTerm(pt[2*noff+1]);

      pt[2*off] = pt[2*noff];
      pt[2*off+1] = pt[2*noff+1];
      npt[3*i2+1] = Global_MkIntegerTerm(off);
      off = noff;
    } else {
      break;
    }
  }
  toff = Global_MkIntegerTerm(off);
  /* off says where we are in first queue */
  /* push into second queue */
  while (off2) {
    UInt noff = (off2+1)/2-1;
    if (Yap_compare_terms(key, npt[3*noff]) > 0) {
      UInt i1 = IntegerOfTerm(npt[3*noff+1]);

      npt[3*off2] = npt[3*noff];
      npt[3*off2+1] = npt[3*noff+1];
      npt[3*off2+2] = npt[3*noff+2];
      pt[2*i1+1] = Global_MkIntegerTerm(off2);
      off2 = noff;
    } else {
      break;
    }
  }
  toff2 = Global_MkIntegerTerm(off2);
  /* store elements in their rightful place */
  npt[3*off2] = pt[2*off] = key;
  pt[2*off+1] = toff2;
  npt[3*off2+1] = toff;
  npt[3*off2+2] = to;
}

static void
DelBeamMax(CELL *pt, CELL *pt2, UInt sz)
{
  CACHE_REGS
  UInt off = IntegerOfTerm(pt2[1]);
  UInt indx = 0;
  Term tk, ti, tv;

  sz--;
  /* first, fix the reverse queue */
  tk = pt2[3*sz];
  ti = pt2[3*sz+1];
  tv = pt2[3*sz+2];
  while (TRUE) {
    if (sz < 2*indx+3 || Yap_compare_terms(pt2[6*indx+3],pt2[6*indx+6]) > 0) {
      if (sz < 2*indx+2 || Yap_compare_terms(tk, pt2[6*indx+3]) > 0) {
	break;
      } else {
	UInt off = IntegerOfTerm(pt2[6*indx+4]);

	pt2[3*indx] = pt2[6*indx+3];
	pt2[3*indx+1] = pt2[6*indx+4];
	pt2[3*indx+2] = pt2[6*indx+5];
	pt[2*off+1] = Global_MkIntegerTerm(indx);
	indx = 2*indx+1;
      }
    } else {
      if (Yap_compare_terms(tk, pt2[6*indx+6]) > 0) {
	break;
      } else {
	UInt off = IntegerOfTerm(pt2[6*indx+7]);

	pt2[3*indx] = pt2[6*indx+6];
	pt2[3*indx+1] = pt2[6*indx+7];
	pt2[3*indx+2] = pt2[6*indx+8];
	pt[2*off+1] = Global_MkIntegerTerm(indx);
	indx = 2*indx+2;
      }
    }
  }
  pt[2*IntegerOfTerm(ti)+1] = Global_MkIntegerTerm(indx);
  pt2[3*indx] = tk;
  pt2[3*indx+1] = ti;
  pt2[3*indx+2] = tv;
  /* now, fix the standard queue */
  if (off != sz) {
    Term toff, toff2, key;
    UInt off2;

    key = pt[2*sz];
    toff2 = pt[2*sz+1];
    off2 = IntegerOfTerm(toff2);
    /* off says where we are in first queue */
    /* push into second queue */
    while (off) {
      UInt noff = (off+1)/2-1;
      if (Yap_compare_terms(key, pt[2*noff]) < 0) {
	UInt i1 = IntegerOfTerm(pt[2*noff+1]);

	pt[2*off] = pt[2*noff];
	pt[2*off+1] = pt[2*noff+1];
	pt2[3*i1+1] = Global_MkIntegerTerm(off);
	off = noff;
      } else {
	break;
      }
    }
    toff = Global_MkIntegerTerm(off);
    /* store elements in their rightful place */
    pt[2*off] = key;
    pt2[3*off2+1] = toff;
    pt[2*off+1] = toff2;
  }
}

static Term
DelBeamMin(CELL *pt, CELL *pt2, UInt sz)
{
  CACHE_REGS
  UInt off2 = IntegerOfTerm(pt[1]);
  Term ov = pt2[3*off2+2]; /* return value */
  UInt indx = 0;
  Term tk, tv;

  sz--;
  /* first, fix the standard queue */
  tk = pt[2*sz];
  tv = pt[2*sz+1];
  while (TRUE) {
    if (sz < 2*indx+3 || Yap_compare_terms(pt[4*indx+2],pt[4*indx+4]) < 0) {
      if (sz < 2*indx+2 || Yap_compare_terms(tk, pt[4*indx+2]) < 0) {
	break;
      } else {
	UInt off2 = IntegerOfTerm(pt[4*indx+3]);
	pt[2*indx] = pt[4*indx+2];
	pt[2*indx+1] = pt[4*indx+3];
	pt2[3*off2+1] = Global_MkIntegerTerm(indx);
	indx = 2*indx+1;
      }
    } else {
      if (Yap_compare_terms(tk, pt[4*indx+4]) < 0) {
	break;
      } else {
	UInt off2 = IntegerOfTerm(pt[4*indx+5]);

	pt[2*indx] = pt[4*indx+4];
	pt[2*indx+1] = pt[4*indx+5];
	pt2[3*off2+1] = Global_MkIntegerTerm(indx);
	indx = 2*indx+2;
      }
    }
  }
  pt[2*indx] = tk;
  pt[2*indx+1] = tv;
  pt2[3*IntegerOfTerm(tv)+1] = Global_MkIntegerTerm(indx);
  /* now, fix the reverse queue */
  if (off2 != sz) {
    Term to, toff, toff2, key;
    UInt off;

    key = pt2[3*sz];
    toff = pt2[3*sz+1];
    to = pt2[3*sz+2];
    off = IntegerOfTerm(toff);
    /* off says where we are in first queue */
    /* push into second queue */
    while (off2) {
      UInt noff = (off2+1)/2-1;
      if (Yap_compare_terms(key, pt2[3*noff]) > 0) {
	UInt i1 = IntegerOfTerm(pt2[3*noff+1]);

	pt2[3*off2] = pt2[3*noff];
	pt2[3*off2+1] = pt2[3*noff+1];
	pt2[3*off2+2] = pt2[3*noff+2];
	pt[2*i1+1] = Global_MkIntegerTerm(off2);
	off2 = noff;
      } else {
	break;
      }
    }
    toff2 = Global_MkIntegerTerm(off2);
    /* store elements in their rightful place */
    pt2[3*off2] = key;
    pt[2*off+1] = toff2;
    pt2[3*off2+1] = toff;
    pt2[3*off2+2] = to;
  }
  return ov;
}

static Int
p_nb_beam_add_to_beam( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"add_to_beam"), *oldH, *oldHB, *pt;
  UInt hsize, hmsize, old_sz;
  Term arena, to, key;
  UInt mingrow;

  if (!qd)
    return FALSE;
  hsize = IntegerOfTerm(qd[HEAP_SIZE]);
  hmsize = IntegerOfTerm(qd[HEAP_MAX]);
  key = Deref(ARG2);
  if (hsize == hmsize) {
    pt = qd+HEAP_START;
    if (Yap_compare_terms(pt[2*hmsize],Deref(ARG2)) > 0) {
      /* smaller than current max, we need to drop current max */
      DelBeamMax(pt, pt+2*hmsize, hmsize);
      hsize--;
    } else {
      return TRUE;
    }
  }
  arena = qd[HEAP_ARENA];
  if (arena == 0L)
    return FALSE;
  mingrow = garena_overflow_size(ArenaPt(arena) PASS_REGS);
  key = CopyTermToArena(ARG2, qd[HEAP_ARENA], FALSE, TRUE, 3, qd+HEAP_ARENA, mingrow PASS_REGS);
  arena = qd[HEAP_ARENA];
  to = CopyTermToArena(ARG3, arena, FALSE, TRUE, 3, qd+HEAP_ARENA, mingrow PASS_REGS);
  if (key == 0 || to == 0L)
    return FALSE;
  qd = GetHeap(ARG1,"add_to_beam");
  arena = qd[HEAP_ARENA];
  /* garbage collection ? */
  oldH = HR;
  oldHB = HB;
  HR = HB = ArenaPt(arena);
  old_sz = ArenaSz(arena);
  while (old_sz < MIN_ARENA_SIZE) {
    UInt gsiz = hsize*2;

    HR = oldH;
    HB = oldHB;
    if (gsiz > 1024*1024) {
      gsiz = 1024*1024;
    } else if (gsiz < 1024) {
      gsiz = 1024;
    }
    ARG3 = to;
    if (!GrowArena(arena, ArenaLimit(arena), old_sz, gsiz, 3 PASS_REGS)) {
      Yap_Error(OUT_OF_STACK_ERROR, arena, LOCAL_ErrorMessage);
      return 0L;
    }    
    to = ARG3;
    qd = RepAppl(Deref(ARG1))+1;
    arena = qd[HEAP_ARENA];
    oldH = HR;
    oldHB = HB;
    HR = HB = ArenaPt(arena);
    old_sz = ArenaSz(arena);    
  }
  pt = qd+HEAP_START;
  PushBeam(pt, pt+2*hmsize, hsize, key, to);
  qd[HEAP_SIZE] = Global_MkIntegerTerm(hsize+1);
  CloseArena(oldH, oldHB, ASP, qd+HEAP_ARENA, old_sz PASS_REGS);
  return TRUE;
}

static Int
p_nb_beam_del( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"debeam");
  UInt old_sz, qsz;
  Term arena;
  CELL *oldH, *oldHB;
  Term tk, tv;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  if (qsz == 0)
    return FALSE;
  arena = qd[HEAP_ARENA];
  if (arena == 0L)
    return FALSE;
  old_sz = ArenaSz(arena);
  /* garbage collection ? */
  oldH = HR;
  oldHB = HB;
  qd[HEAP_SIZE] = Global_MkIntegerTerm(qsz-1);
  CloseArena(oldH, oldHB, ASP, &arena, old_sz PASS_REGS);
  tk = qd[HEAP_START];
  tv = DelBeamMin(qd+HEAP_START, qd+(HEAP_START+2*IntegerOfTerm(qd[HEAP_MAX])), qsz);
  return Yap_unify(tk, ARG2) &&
    Yap_unify(tv, ARG3);
}

#ifdef DEBUG
static Int
p_nb_beam_check( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"debeam");
  UInt qsz, qmax;
  CELL *pt, *pt2;
  UInt i;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  qmax = IntegerOfTerm(qd[HEAP_MAX]);
  if (qsz == 0)
    return TRUE;
  pt = qd+HEAP_START;
  pt2 = pt+2*qmax;
  for (i = 1; i < qsz; i++) {
    UInt back;
    if (Yap_compare_terms(pt[2*((i+1)/2-1)],pt[2*i]) > 0) {
      Yap_DebugPlWrite(pt[2*((i+1)/2-1)]); fprintf(stderr,"\n");
      Yap_DebugPlWrite(pt[2*i]); fprintf(stderr,"\n");
      fprintf(stderr,"Error at %ld\n",(unsigned long int)i);
      return FALSE;
    }
    back = IntegerOfTerm(pt[2*i+1]);
    if (IntegerOfTerm(pt2[3*back+1]) != i) {
      fprintf(stderr,"Link error at %ld\n",(unsigned long int)i);
      return FALSE;
    }
  }
  for (i = 1; i < qsz; i++) {
    if (Yap_compare_terms(pt2[3*((i+1)/2-1)],pt2[3*i]) < 0) {
      fprintf(stderr,"Error at sec %ld\n",(unsigned long int)i);
      Yap_DebugPlWrite(pt2[3*((i+1)/2-1)]); fprintf(stderr,"\n");
      Yap_DebugPlWrite(pt2[3*i]); fprintf(stderr,"\n");
      return FALSE;
    }
  }
  return TRUE;
}

#endif

static Int
p_nb_beam_keys( USES_REGS1 )
{
  CELL *qd;
  UInt qsz;
  CELL *pt, *ho;
  UInt i;

 restart:
  qd = GetHeap(ARG1,"beam_keys");
  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  ho = HR;
  pt = qd+HEAP_START;
  if (qsz == 0)
    return Yap_unify(ARG2, TermNil);
  for (i=0; i < qsz; i++) {
    if (HR > ASP-1024) {
      HR = ho;
      if (!Yap_gcl(((ASP-HR)-1024)*sizeof(CELL), 2, ENV, P)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	return TermNil;
      }
      goto restart;
    }
    *HR++ = pt[0];
    *HR = AbsPair(HR+1);
    HR++;
    pt += 2;
  }
  HR[-1] = TermNil;
  return Yap_unify(ARG2, AbsPair(ho));
}

static Int
p_nb_beam_peek( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"beam_peek"), *pt, *pt2;
  UInt qsz, qbsize;
  Term tk, tv;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[HEAP_SIZE]);
  qbsize = IntegerOfTerm(qd[HEAP_MAX]);
  if (qsz == 0)
    return FALSE;
  pt = qd+HEAP_START;
  pt2 = pt+2*qbsize;
  tk = pt[0];
  tv = pt2[2];
  return Yap_unify(tk, ARG2) &&
    Yap_unify(tv, ARG3);
}

static Int
p_nb_beam_empty( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"beam_empty");

  if (!qd)
    return FALSE;
  return (IntegerOfTerm(qd[HEAP_SIZE]) == 0);
}

static Int
p_nb_beam_size( USES_REGS1 )
{
  CELL *qd = GetHeap(ARG1,"beam_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2,qd[HEAP_SIZE]);
}

static Int 
cont_current_nb( USES_REGS1 )
{
  Int unif;
  GlobalEntry *ge = (GlobalEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(1,1));

  unif = Yap_unify(MkAtomTerm(AbsAtom(ge->AtomOfGE)), ARG1);
  ge = ge->NextGE;
  if (!ge) {
    if (unif)
      cut_succeed();
    else
      cut_fail();
  } else {
    EXTRA_CBACK_ARG(1,1) =  MkIntegerTerm((Int)ge);
    return unif;
  }
}

static Int 
init_current_nb( USES_REGS1 )
{				/* current_atom(?Atom)		 */
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    if (IsAtomTerm(t1)) {
      if (!FindGlobalEntry(AtomOfTerm(t1) PASS_REGS)) {
	  cut_fail();
      } else {
	cut_succeed();
      }
    } else {
      Yap_Error(TYPE_ERROR_ATOM,t1,"nb_current");
      cut_fail();
    }
  }
  READ_LOCK(HashChain[0].AERWLock);
  EXTRA_CBACK_ARG(1,1) =  MkIntegerTerm((Int)LOCAL_GlobalVariables);
  return cont_current_nb( PASS_REGS1 );
}


void Yap_InitGlobals(void)
{
  CACHE_REGS
  Term cm = CurrentModule;
  Yap_InitCPred("$allocate_arena", 2, p_allocate_arena, 0);
  Yap_InitCPred("arena_size", 1, p_default_arena_size, 0);
  Yap_InitCPred("b_setval", 2, p_b_setval, SafePredFlag);
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

?- nb_setval(a,a(_)),nb_getval(a,A),nb_set_shared_val(b,t(C,A)),nb_getval(b,B).

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


@}  */
  Yap_InitCPred("nb_create", 3, p_nb_create, 0L);
  Yap_InitCPred("nb_create", 4, p_nb_create2, 0L);
  Yap_InitCPredBack("$nb_current", 1, 1, init_current_nb, cont_current_nb, SafePredFlag);
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

/**
@}
*/
