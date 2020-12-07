/**
 * @file   globals.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 23:16:17 2015
 *
 * @brief  support for backtrable and non-backtrackable variables in Prolog.
 *
 *
 */

#define DEB_DOOBIN(d0)                                                         \
  (fprintf(stderr, "+++ %s ", __FUNCTION__), Yap_DebugPlWriteln(d0))
#define DEB_DOOBOUT(d0) (fprintf(stderr, "--- "), Yap_DebugPlWriteln(d0))
#define DEB_DOOB(S)                                                            \
  (fprintf(stderr, "%s %ld %p=%p %p--%d\n ", S, to_visit - to_visit0, pt0,     \
           ptf, *AbsAppl(pt0), arity)) //, Yap_DebugPlWriteln(d0))
/**

   @defgroup Global_Variables Global Variables
   @ingroup builtins
   @{

   Global variables are associations between names (atoms) and
   terms. They differ in various ways from storing information using
   assert/1 or recorda/3.
   4
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

#include "YapError.h"

#include "terms.h"

#ifdef HAVE_STRING_H

#include "string.h"

#endif
/* Non-backtrackable terms will from now on be stored on arenas, a
   special term on the heap. Arenas automatically contract as we add terms to
   the front.

*/

static int copy_complex_term(CELL *pt0_, CELL *pt0_end_, bool share,
                             bool copy_att_vars, CELL *ptf_, 
                             Term *bindp,
			     Ystack_t *stt USES_REGS);

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

#define MIN_ARENA_SIZE (1024L)

#define MAX_ARENA_SIZE (2048 * MIN_ARENA_SIZE)

#define Global_MkIntegerTerm(I) MkIntegerTerm(I)

static size_t big2arena_szW(CELL *arena_base) {
  return (((MP_INT *)(arena_base + 2))->_mp_alloc * sizeof(mp_limb_t) +
          sizeof(MP_INT) + sizeof(Functor) + 2 * sizeof(CELL)) /
         sizeof(CELL);
}

#if 0
static size_t arena2big_szW   (size_t sz) {
  return sz -
    (sizeof(MP_INT) + sizeof(Functor) + 2 * sizeof(CELL)) / sizeof(CELL);
}
#endif

/* pointer to top of an arena */
static inline CELL *ArenaLimit(Term arena) {
  CELL *arena_base = RepAppl(arena);
  size_t szW = big2arena_szW(arena_base);
  return arena_base + szW;
}

/* pointer to top of an arena */
CELL *Yap_ArenaLimit(Term arena) { return ArenaLimit(arena); }

/* pointer to top of an arena */
static inline CELL *ArenaPt(Term arena) { return RepAppl(arena); }

CELL *Yap_ArenaPt(Term arena) { return ArenaPt(arena); }

static inline UInt ArenaSzW(Term arena) {
  return big2arena_szW(RepAppl(arena));
}

UInt Yap_ArenaSz(Term arena) { return ArenaSzW(arena); }

/// A cell_space is a short code region, where we want bindings to proceed
/// locally. It is used in copy_term
///
typedef struct cell_space {
  struct cell_space *parent; //> the ancestor
  ssize_t oASP;
  CELL *oH, *oHB;        //> stacks above
  CELL *arenaB, *arenaL; //> work area
  size_t szW;
} cell_space_t;

static Term CreateNewArena(CELL *ptr, CELL *max) {
  Term t = AbsAppl(ptr);
  MP_INT *dst;

  //  printf("<< %p %p %ld     \n", ptr, max, max - ptr);
  ptr[0] = (CELL)FunctorBigInt;
  ptr[1] = EMPTY_ARENA;
  dst = (MP_INT *)(ptr + 2);
  size_t size = max - 1 - (CELL *)(dst + 1);
  dst->_mp_alloc = (sizeof(CELL) / sizeof(mp_limb_t)) * size;
  dst->_mp_size = 0;
  max[-1] = CloseExtension(ptr);
  if (ptr >= HR)
    HR = max;
  return t;
}

static bool expand( size_t sz, CELL *arenap) {

    sz += MinStackGap;
  if (!arenap) {

    if (HR + sz > ASP ) {
      if (!Yap_dogcl(sz * CellSize)) {
        Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
                       "No Stack Space for Non-Backtrackable terms");
      }
    }
    return true;
  } else {
    size_t nsz;
    size_t sz0 = ArenaSzW(*arenap);
    yhandle_t yh = Yap_PushHandle(*arenap);
    while (true) {
      CELL *shifted_max;
      CELL *a_max = ArenaLimit(*arenap);
      nsz = Yap_InsertInGlobal(a_max-1, sz * CellSize, &shifted_max) /
	CellSize;
      if (nsz >= sz ) {
	shifted_max += 1;
	CELL* ar_max = shifted_max+nsz;
      CELL *ar_min = shifted_max-sz0;
      Yap_PopHandle(yh);
      *arenap = CreateNewArena( ar_min, ar_max);
      return true;
      }
    }
      
      if(!Yap_dogcl(sz * CellSize)) {
	Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
                     "No Stack Space for Non-Backtrackable terms");
      }
      *arenap = Yap_GetFromHandle(yh);
    }
}

static Int p_allocate_arena(USES_REGS1) {
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "allocate_arena");
    return FALSE;
  } else if (!IsIntegerTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, t, "allocate_arena");
    return FALSE;
  }
  size_t sz = IntegerOfTerm(t);
  Term a = CreateNewArena(HR, HR + sz);
  return Yap_unify(ARG2, a);
}

static Int arena_size(USES_REGS1) {
  return Yap_unify(ARG1, MkIntegerTerm(ArenaSzW(LOCAL_GlobalArena)));
}

void Yap_AllocateDefaultArena(size_t gsizeW, int wid, void *cs) {
  REMOTE_GlobalArena(wid) = CreateNewArena(H0, H0 + gsizeW);
  HR = H0 + gsizeW;
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

static Term visitor_error_handler(yap_error_number res, CELL *hb, CELL *asp,
                                  size_t min_grow, Term *arenap) {
  if (res == RESOURCE_ERROR_AUXILIARY_STACK) {
    return true;

  } else if (res == RESOURCE_ERROR_TRAIL) {
    if (!Yap_growtrail(0, false)) {
      Yap_ThrowError(RESOURCE_ERROR_TRAIL, TermNil, "while visiting terms");
    }
  } else if (res == RESOURCE_ERROR_STACK) {
    return  expand(min_grow, arenap);
  }
  return true;
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
 * @param bindp - list of bindings introduce to break cycles in the original
 * term. If *bindp == TermFoundVar rewrite any loops as a singleconstant.
 * @param stt->hlow
 * @return bool
 *
 * The routine works by marking compund terms the first time they are visited.
 * If the term is a listm it marks the head of the list, otherwise it marks the
 * functor. The mderef routines then do marker-aware dereferencing.
 */
static int copy_complex_term(CELL *pt0_, CELL *pt0_end_, bool share,
                             bool copy_att_vars, CELL *ptf_,
                 			      Term *bindp, Ystack_t *stt USES_REGS) {
  // allocate space for internal stack, so that we do not have yo rely on
  // m C-stack.
  CELL *pt0 = pt0_, *ptf = ptf_, *pt0_end = pt0_end_;
  Term myt;
  bool forest = bindp;
  bool ground;
  ground = true;
  share = false;

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
      //	DEB_DOOB("enter");
      mderef_head(d0, dd0, copy_term_unk);
    copy_term_nvar:
      if (IsPairTerm(d0)) {
        CELL *ptd1 = RepPair(d0);

        ///> found infinite loop.
        /// p0 is the original sub-term = ...,p0,....
        /// ptd0 is the derefereed version of d0
        ///
        ///
        if (share && ptd1 >= HB && ptd1 < ASP) {
          *ptf = AbsPair(ptd1);
        } else if (IS_VISIT_MARKER(*ptd1)) {
          /* d0 has ance   */
          struct cp_frame *entry = VISIT_ENTRY(*ptd1);
          Term val = entry->t;
          if (IsVarTerm(val)) {
            *ptf = val;
          } else if (forest) {
            // set up a binding PTF=D0

            // set up a binding PTF=D0
            Term l = AbsAppl(HR);
            RESET_VARIABLE(ptf);
            HR[0] = (CELL)FunctorEq;
            entry->t = HR[1] = (CELL)ptf;
            HR[2] = val;
        if (HR + 3 > ASP - MIN_ARENA_SIZE) {
          // same as before
          return RESOURCE_ERROR_STACK;
        }
            HR += 3;
            if (bindp)
              *bindp = MkPairTerm(l, *bindp);
          } else {
            *ptf = val;
            TrailedMaBind(ptf, (CELL)ptf);
          }
          continue;
        }

        // first time we meet,
        // save state

        if (share) {
          d0 = AbsPair(ptf);
          mTrailedMaBind(ptd0, d0);
          if (TR + 32 >= (tr_fr_ptr)LOCAL_TrailTop) {
            return RESOURCE_ERROR_TRAIL;
          }
        }
        myt = AbsPair(HR);
        if (stt->pt + 32 >= stt->max) {
          return RESOURCE_ERROR_AUXILIARY_STACK;
        }
        to_visit->tr = TR;
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

        ptf = HR - 1;
        to_visit++;
        ground = true;
        pt0 = ptd1 - 1;
        pt0_end = ptd1 + 1;
        if (HR + 2 > ASP - MIN_ARENA_SIZE) {
          // same as before
          return RESOURCE_ERROR_STACK;
        }
        HR += 2;
        continue;
      } else if (IsApplTerm(d0)) {
        CELL *ptd1 = RepAppl(d0);
        CELL dd1 = *ptd1;
        if (share && ptd1 >= HB && ptd1 < ASP) {
          /* If this is newer than the current term, just reuse */
          *ptf = d0;
          continue;
        }

        if (IsExtensionFunctor((Functor)dd1)) {
          switch (dd1) {
          case (CELL)FunctorDBRef:
            *ptf = d0;
          case (CELL)FunctorLongInt:
            if (HR > ASP - (MIN_ARENA_SIZE + 3)) {
              return RESOURCE_ERROR_STACK;
            }
            *ptf = AbsAppl(HR);
            HR[0] = dd1;
            HR[1] = ptd1[1];
            HR[2] = CloseExtension(HR);
            HR += 3;
            break;
          case (CELL)FunctorDouble:
            if (HR >
                ASP - (MIN_ARENA_SIZE + (2 + SIZEOF_DOUBLE / sizeof(CELL)))) {
              return RESOURCE_ERROR_STACK;
            }
            *ptf = AbsAppl(HR);
            HR[0] = dd1;
            HR[1] = ptd1[1];
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
            HR[2] = ptd1[2];
            HR[3] = CloseExtension(HR);
            HR += 4;
#else
            HR[2] = CloseExtension(HR);
            HR += 3;
#endif
            break;
          case (CELL)FunctorString:
            if (ASP - HR < MIN_ARENA_SIZE + 3 + ptd1[1]) {
              return RESOURCE_ERROR_STACK;
            }
            *ptf = AbsAppl(HR);
            memmove(HR, ptd1, sizeof(CELL) * (2 + ptd1[1]));
            HR += ptd1[1] + 3;
            HR[-1] = CloseExtension(RepAppl(*ptf));
            break;
          default: {
            size_t szW;

            /* big int */
            if (ptd1[1] == BIG_RATIONAL) {

              szW =
                  (2 * sizeof(MP_INT) + 3 * CellSize +
                   ((MP_INT *)(ptd1 + 2))->_mp_alloc * sizeof(mp_limb_t)) /
                      CellSize +
                  (((MP_INT *)(ptd1 + 2) + 1)->_mp_alloc * sizeof(mp_limb_t)) /
                      CellSize;
            } else {
              szW = (sizeof(MP_INT) + 3 * CellSize +
                     ((MP_INT *)(ptd1 + 2))->_mp_alloc * sizeof(mp_limb_t)) /
                    CellSize;
            }
            if (HR > ASP - (MIN_ARENA_SIZE + szW)) {
              return RESOURCE_ERROR_STACK;
            }
            *ptf = AbsAppl(HR);
            HR[0] = dd1;
            size_t i;
            for (i = 1; i < szW - 1; i++) {
              HR[i] = ptd1[i];
            }
            HR[szW - 1] =  CloseExtension(HR);
            HR += szW;
          }
          }
          continue;
        } else if (IS_VISIT_MARKER(dd1)) {
          /* If this is newer than the current term, just reuse */
          // set up a binding PTF=D0
          struct cp_frame *entry = VISIT_ENTRY(dd1);
          Term val = entry->t;
          if (IsVarTerm(val)) {
            mBind_And_Trail(ptf, val);
          } else if (forest) {
            // set up a binding PTF=D0
            Term l = AbsAppl(HR);
            RESET_VARIABLE(ptf);
            HR[0] = (CELL)FunctorEq;
            entry->t = HR[1] = (CELL)ptf;
            HR[2] = val;
            HR += 3;
            if (bindp)
              *bindp = MkPairTerm(l, *bindp);
          } else {
            // same as before
            *ptf = val;
            TrailedMaBind(ptf, (CELL)ptf);
            if (TR + 32 >= (tr_fr_ptr)LOCAL_TrailTop) {
              return RESOURCE_ERROR_TRAIL;
            }
          }
        } else {
          Term d1 = dd1;
          myt = *ptf = AbsAppl(HR);
          Functor f = (Functor)d1;
          arity_t arity;
          if (f == FunctorAttVar)
            arity = 3;
          else
            arity = ArityOfFunctor(f);
          if (share) {
            d0 = AbsAppl(ptf);
            TrailedMaBind(ptd0, d0);
            if (TR + 32 >= (tr_fr_ptr)LOCAL_TrailTop) {
              return RESOURCE_ERROR_TRAIL;
            }
          }
          /* store the terms to visit */
          if (to_visit + 32 >= to_visit_end) {
            return RESOURCE_ERROR_AUXILIARY_STACK;
          }
          to_visit->tr = TR;
          to_visit->pt0 = pt0;
          to_visit->pt0_end = pt0_end;
          to_visit->ptf = ptf;
          to_visit->t = myt;
          to_visit->ground = ground;
          to_visit->oldp = ptd1;
          to_visit->oldv = (CELL)f;
          *ptd1 = VISIT_MARK();
          to_visit++;
          ground = (f != FunctorMutable);
          pt0 = ptd1;
          pt0_end = ptd1 + arity;
          /* store the functor for the new term */
          HR[0] = (CELL)f;
          ptf = HR;
          if (HR > ASP - (arity + MIN_ARENA_SIZE)) {
            return RESOURCE_ERROR_STACK;
          }
          HR += arity + 1;
        }
      } else {
        /* just copy atoms or integers */
        *ptf = d0;
      }
      continue;
      mderef_body(d0, dd0, ptd0, copy_term_unk, copy_term_nvar);
      ground = FALSE;
      /* don't need to copy variables if we want to share the global term */
      if (HB <= ptd0 && ptd0 < HR) {
        if (ptd0 != ptf) {
          *ptf = (CELL)ptd0;
        }
        continue;
      }
      if (copy_att_vars && GlobalIsAttVar(ptd0)) {
        /* if unbound, call the standard copy term routine */
        //	  if (true) { //||!GLOBAL_atd0)].copy_term_op) {
        /* store the terms to visit */
        if (to_visit + 32 >= to_visit_end) {
          return RESOURCE_ERROR_AUXILIARY_STACK;
        }
        *ptf = (CELL)(HR + 1);
        to_visit->pt0 = pt0;
        to_visit->pt0_end = pt0_end;
        to_visit->ptf = ptf;
        to_visit->t = AbsAppl(HR);
        to_visit->ground = false;
        to_visit->tr = TR;
        to_visit->oldp = ptd0 - 1;
        to_visit->oldv = (CELL)FunctorAttVar;
        ptd0[-1] = VISIT_MARK();
        to_visit++;
        ground = false;
        pt0 = ptd0 - 1;
        pt0_end = ptd0 + 2;
        /* store the functor for the new term */
        HR[0] = (CELL)FunctorAttVar;
        ptf = HR;
        if (HR > ASP - (3 + MIN_ARENA_SIZE)) {
          return RESOURCE_ERROR_STACK;
        }
        HR += 3 + 1;
      } else {
        RESET_VARIABLE(ptf);
        mBind_And_Trail(ptd0, (CELL)ptf);
        if (TR + 32 >= (tr_fr_ptr)LOCAL_TrailTop) {
          return RESOURCE_ERROR_TRAIL;
        }
      }
    }
    if (to_visit <= to_visit0) {
      return 0;
    }
    to_visit--;
    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    ptf = to_visit->ptf;
    myt = to_visit->t;
    VUNMARK(to_visit->oldp, to_visit->oldv);
    ground = (ground && to_visit->ground);
  } while (true);
  return 0;
}

static Term CopyTermToArena(Term t,
			    bool share, bool copy_att_vars,
                            Term *arenap, Term *bindp USES_REGS)
{  Ystack_t ystk, *stt = &ystk;
  size_t expand_stack, sz0;
      yap_error_number res = 0;
  t = Deref(t);
  if (!IsVarTerm(t) && IsAtomOrIntTerm(t))
    return t;
      if (arenap) {
      if (*arenap) {
          sz0 = ArenaSzW(*arenap);
      } else {
	sz0 = ASP - HR;
      }
  }
  expand_stack = 4 * K;
      if (expand_stack < 4* MIN_ARENA_SIZE)
          expand_stack =  4* MIN_ARENA_SIZE;
      if (expand_stack > 2 * K * K)
          expand_stack = 2 * K * K;
      Yap_RebootHandles();
      size_t sz_stack = 1024;
      while (true) {
          CELL *ap = &t;
	  CELL *pf;
          CELL *hr, *hb, *asp;
	  
          hr = HR; 
          hb = HB;
          asp = ASP;
          init_stack(stt, sz_stack);
          if (arenap && *arenap) {
              CELL *start = ArenaPt(*arenap);
              CELL *end = ArenaLimit(*arenap);
                   HR = start;
                  ASP = end;
              }
          HB = HR;
           pf = HR;
          res = copy_complex_term(ap - 1, ap, share, copy_att_vars, pf, bindp,
                                  stt PASS_REGS);
                  if (arenap && *arenap) {
	CELL *start = res ? HB : HR;
		      HR = hr;
                      *arenap = CreateNewArena(start, ASP);
		      ASP = asp;
                  }     
              HB = B->cp_h;
    while (to_visit > to_visit0) {
      to_visit--;

      clean_tr(to_visit->tr PASS_REGS);
      VUNMARK(to_visit->oldp, to_visit->oldv);
    } /* restore our nice, friendly, term to its original state */
    clean_tr(stt->tr0 PASS_REGS);
    // done, exit!
  if (res == YAP_NO_ERROR) {
		
	      if (IsVarTerm(t))
		return (CELL) pf;
	      if (IsApplTerm(t))
		return AbsAppl(pf);
	      return AbsPair(pf);
  } else {
    yhandle_t yt1,yt;
                      yt = Yap_InitHandle(t);
    if (bindp)
                  yt1 = Yap_InitHandle(*bindp);
              if (res == RESOURCE_ERROR_AUXILIARY_STACK) {
                  sz_stack *= 2;
                  if (sz_stack >= 16 * K * K)
                      sz_stack = 16 * K * K;
              } else if (res == RESOURCE_ERROR_STACK) {
                  expand_stack *= 2;
                  if (expand_stack > 2 * K * K)
                      expand_stack = 2 * K * K;
              }
              visitor_error_handler(res, HB, ASP, expand_stack, arenap);
              if (bindp)
                  *bindp = Yap_PopHandle(yt1);
	      t = Yap_PopHandle(yt);
          }
      }
}


Term Yap_CopyTerm(Term inp) {
  CACHE_REGS
  return CopyTermToArena(inp, false, true, NULL, NULL PASS_REGS);
}

Term Yap_CopyTermNoShare(Term inp) {
  CACHE_REGS

  COPY(inp);
  return CopyTermToArena(inp, FALSE, true, NULL, NULL PASS_REGS);
}

static Int p_copy_term(USES_REGS1) /* copy term t to a new instance  */
{
  Term inp = MkGlobal(Deref(ARG1));
  Term t = CopyTermToArena(inp, false, TRUE, NULL, NULL PASS_REGS);
  COPY(t);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2, t);
}

static Int p_duplicate_term(USES_REGS1) /* copy term t to a new instance  */
{
  Term inp = MkGlobal(Deref(ARG1));
  Term t = CopyTermToArena(inp, FALSE, TRUE, NULL, NULL PASS_REGS);
  if (t == 0L)
    return FALSE;
  COPY(t);
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2, t);
}

static Int
p_rational_tree_to_forest(USES_REGS1) /* copy term t to a new instance  */
{
  Term t2 = ARG2;
  Term t3 = ARG3;
  Term list = Deref(ARG4);
  COPY(ARG1);
  Term t = CopyTermToArena(Deref(ARG1), false, false, NULL, &list PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(t2, t) && Yap_unify(t3, list);
}

Term Yap_TermAsForest(Term t1) /* copy term t to a new instance  */
{
  Term list = TermNil;
  Term t = CopyTermToArena(t1, false, false, NULL, &list PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  Term ts[2];
  ts[0] = t;
  ts[1] = list;
  return Yap_MkApplTerm(FunctorAtSymbol, 2, ts);
}

static Int
p_copy_term_no_delays(USES_REGS1) /* copy term t to a new instance  */
{
  COPY(ARG1);
  Term inp = MkGlobal(Deref(ARG1));
  Term t = CopyTermToArena(inp, false, false, NULL, NULL PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2, t);
}

inline static GlobalEntry *FindGlobalEntry(Atom at USES_REGS)
/* get predicate entry for ap/arity; create it if neccessary. */
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
/* get predicate entry for ap/arity; create it if neccessary. */
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
  new = (GlobalEntry *)Yap_AllocAtomSpace(sizeof(*new));
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

static Int nb_setarg(USES_REGS1) {
  Term wheret = Deref(ARG1);
  Term dest;
  Term to;
  UInt arity, pos;
  CELL *destp;

  if (IsVarTerm(wheret)) {
    Yap_ThrowError(INSTANTIATION_ERROR, wheret, "nb_setarg");
    return FALSE;
  }
  if (!IsIntegerTerm(wheret)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, wheret, "nb_setarg");
    return FALSE;
  }
  pos = IntegerOfTerm(wheret);
  dest = Deref(ARG2);
  if (IsVarTerm(dest)) {
    Yap_ThrowError(INSTANTIATION_ERROR, dest, "nb_setarg");
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
  COPY(ARG3);

  to = Deref(ARG3);
  to = CopyTermToArena(Deref(ARG3), FALSE, TRUE, &LOCAL_GlobalArena, NULL
                           PASS_REGS);
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

static Int nb_set_shared_arg(USES_REGS1) {
  Term wheret = Deref(ARG1);
  Term dest;
  Term to;
  UInt arity, pos;
  CELL *destp;

  if (IsVarTerm(wheret)) {
    Yap_ThrowError(INSTANTIATION_ERROR, wheret, "nb_setarg");
    return FALSE;
  }
  if (!IsIntegerTerm(wheret)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, wheret, "nb_setarg");
    return FALSE;
  }
  pos = IntegerOfTerm(wheret);
  dest = Deref(ARG2);
  if (IsVarTerm(dest)) {
    Yap_ThrowError(INSTANTIATION_ERROR, dest, "nb_setarg");
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
  COPY(ARG3);
  to = CopyTermToArena(Deref(ARG3), TRUE, TRUE, &LOCAL_GlobalArena, NULL PASS_REGS);
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

static Int nb_linkarg(USES_REGS1) {
  Term wheret = Deref(ARG1);
  Term dest;
  UInt arity, pos;
  CELL *destp;

  if (IsVarTerm(wheret)) {
    Yap_ThrowError(INSTANTIATION_ERROR, wheret, "nb_setarg");
    return FALSE;
  }
  if (!IsIntegerTerm(wheret)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, wheret, "nb_setarg");
    return FALSE;
  }
  pos = IntegerOfTerm(wheret);
  dest = Deref(ARG3);
  if (IsVarTerm(dest)) {
    Yap_ThrowError(INSTANTIATION_ERROR, dest, "nb_setarg");
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

static Int nb_linkval(USES_REGS1) {
  Term t = Deref(ARG1), to;
  GlobalEntry *ge;
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_linkval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "nb_linkval");
    return (FALSE);
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  to = Deref(ARG2);
  WRITE_LOCK(ge->GRWLock);
  ge->global = to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}

static Int nb_create_accumulator(USES_REGS1) {
  Term t = Deref(ARG1), acct, to, t2;
  CELL *destp;

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_create_accumulator");
    return FALSE;
  }
  if (!IsIntegerTerm(t) && !IsBigIntTerm(t) && !IsFloatTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_NUMBER, t, "nb_create_accumulator");
    return FALSE;
  }
  acct = Yap_MkApplTerm(FunctorGNumber, 1, &t);
  if (!Yap_unify(ARG2, acct)) {
    return FALSE;
  }
  COPY(t);
  to = CopyTermToArena(t, TRUE, TRUE, &LOCAL_GlobalArena, NULL PASS_REGS);
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

static Int nb_add_to_accumulator(USES_REGS1) {
  Term t = Deref(ARG1), t0, tadd;
  Functor f;
  CELL *destp;

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_crate_accumulator");
    return FALSE;
  }
  if (!IsApplTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_NUMBER, t, "nb_accumulator_value");
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
    Yap_ThrowError(INSTANTIATION_ERROR, tadd, "nb_create_accumulator");
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
        COPY(new);
        new = CopyTermToArena(new, TRUE, TRUE, &LOCAL_GlobalArena, NULL  PASS_REGS);
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
    COPY(new);
    new = CopyTermToArena(new, TRUE, TRUE, &(LOCAL_GlobalArena), NULL PASS_REGS);
    destp = RepAppl(Deref(ARG1));
    destp[1] = new;

    return TRUE;
  }
  return FALSE;
}

static Int nb_accumulator_value(USES_REGS1) {
  Term t = Deref(ARG1);
  Functor f;

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_accumulator_value");
    return FALSE;
  }
  if (!IsApplTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_NUMBER, t, "nb_accumulator_value");
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
  COPY(t0);
  to = CopyTermToArena(t0, FALSE, TRUE, &(LOCAL_GlobalArena), NULL PASS_REGS);
  if (to == 0L)
    return to;
  WRITE_LOCK(ge->GRWLock);
  ge->global = to;
  WRITE_UNLOCK(ge->GRWLock);
  return to;
}

Term Yap_CopyTermToArena(Term inp, Term *arenap) {
  CACHE_REGS
  return CopyTermToArena(inp, false, true, arenap, NULL PASS_REGS);
}

Term Yap_SaveTerm(Term t0) {
  CACHE_REGS
  Term to;
  to = CopyTermToArena(Deref(t0), FALSE, TRUE, &LOCAL_GlobalArena, NULL PASS_REGS);
  if (to == 0L)
    return to;
  return to;
}

static Int nb_setval(USES_REGS1) {
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_setval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "nb_setval");
    return (FALSE);
  }
  return Yap_SetGlobalVal(AtomOfTerm(t), ARG2);
}

static Int nb_set_shared_val(USES_REGS1) {
  Term t = Deref(ARG1), to;
  GlobalEntry *ge;
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_setval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "nb_setval");
    return (FALSE);
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  COPY(ARG2);
  to = CopyTermToArena(ARG2, TRUE, TRUE, &LOCAL_GlobalArena, NULL PASS_REGS);
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
    Yap_ThrowError(INSTANTIATION_ERROR, t, "b_setval");
    return (TermNil);
  } else if (!IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "b_setval");
    return (FALSE);
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  WRITE_LOCK(ge->GRWLock);
#ifdef MULTI_ASSIGNMENT_VARIABLES
  /* the evil deed is to be done now */
  {
    /* but first make sure we are doing on a global object, or a constant!
     */
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
  Yap_ThrowError(SYSTEM_ERROR_INTERNAL, t, "update_array");
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

static Int nb_getval(USES_REGS1) {
  Term t = Deref(ARG1), to;
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_getval");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "nb_getval");
    return FALSE;
  }
  ge = FindGlobalEntry(AtomOfTerm(t) PASS_REGS);
  if (!ge)
    return undefined_global(PASS_REGS1);
  READ_LOCK(ge->GRWLock);
  to = ge->global;
  if (!to)
        Yap_ThrowError(INSTANTIATION_ERROR, ARG1, "nb_getval");
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
    Yap_ThrowError(EXISTENCE_ERROR_VARIABLE, MkAtomTerm(at), "nb_delete");
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

Int Yap_DeleteGlobal(Atom at) {
  CACHE_REGS
  return nbdelete(at PASS_REGS);
}

static Int nb_delete(USES_REGS1) {
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_delete");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "nb_delete");
    return FALSE;
  }
  return nbdelete(AtomOfTerm(t) PASS_REGS);
}

static Int nb_create(USES_REGS1) {
  Term t = Deref(ARG1);
  Term tname = Deref(ARG2);
  Term tarity = Deref(ARG3);
  Term to;
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "nb_create");
    return FALSE;
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  if (!ge) {
    Yap_ThrowError(EXISTENCE_ERROR_VARIABLE, t, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tarity)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tarity, "nb_create");
    return FALSE;
  } else if (!IsIntegerTerm(tarity)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, tarity, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tname)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tname, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, tname, "nb_create");
    return FALSE;
  }
  to = CopyTermToArena(t, false, TRUE, &LOCAL_GlobalArena, NULL PASS_REGS);
  COPY(t);
  if (!to) {
    return false;
  }

  WRITE_LOCK(ge->GRWLock);
  ge->global = to;
  WRITE_UNLOCK(ge->GRWLock);
  return TRUE;
}

static Int nb_create2(USES_REGS1) {
  Term t = Deref(ARG1);
  Term tname = Deref(ARG2);
  Term tarity = Deref(ARG3);
  Term tinit = Deref(ARG4);
  Term to;
  GlobalEntry *ge;

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t, "nb_create");
    return FALSE;
  }
  ge = GetGlobalEntry(AtomOfTerm(t) PASS_REGS);
  if (!ge) {
    Yap_ThrowError(EXISTENCE_ERROR_VARIABLE, t, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tarity)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tarity, "nb_create");
    return FALSE;
  } else if (!IsIntegerTerm(tarity)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, tarity, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tname)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tname, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, tname, "nb_create");
    return FALSE;
  }
  if (IsVarTerm(tinit)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tname, "nb_create");
    return FALSE;
  } else if (!IsAtomTerm(tinit)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, tname, "nb_create");
    return FALSE;
  }
  to = CopyTermToArena(tinit, false, false, &LOCAL_GlobalArena, NULL PASS_REGS);
  if (to == 0)
    return false;
  WRITE_LOCK(ge->GRWLock);
  ge->global = to;
  WRITE_UNLOCK(ge->GRWLock);
  return true;
}

/* a non-backtrackable queue is a term of the form
 * $array(Arena,Start,End,Size) plus an Arena. */

static Int nb_queue_sized(size_t arena_sz USES_REGS) {
  Term queue, *ar;
  Term t = Deref(ARG1);
  LOCAL_DepthArenas++;
  if (!IsVarTerm(t)) {
    if (!IsApplTerm(t)) {
      return FALSE;
    }
    return (FunctorOfTerm(t) == FunctorNBQueue);
  }
  if (arena_sz < 32 * MIN_ARENA_SIZE)
    arena_sz = 32 * MIN_ARENA_SIZE;
  ar = HR;
  queue = AbsAppl(HR);
  HR += QUEUE_FUNCTOR_ARITY + 1;
  if (queue == 0L) {
    return FALSE;
  }
  ar[0] = (CELL)FunctorNBQueue;
  ar += 1;
  RESET_VARIABLE(ar + QUEUE_TAIL);
  ar[QUEUE_HEAD] = ar[QUEUE_TAIL];
  ar[QUEUE_SIZE] = MkIntTerm(0);
  ar[QUEUE_ARENA] = CreateNewArena(HR, HR + arena_sz);
  HR = ArenaLimit(ar[QUEUE_ARENA]);
  return Yap_unify(queue, ARG1);
}

static Int nb_queue(USES_REGS1) {
  UInt arena_sz = (ASP - HR) / 16;
  if (LOCAL_DepthArenas > 1)
    arena_sz /= LOCAL_DepthArenas;
  if (arena_sz < MIN_ARENA_SIZE)
    arena_sz = MIN_ARENA_SIZE;
  if (arena_sz > MAX_ARENA_SIZE)
    arena_sz = MAX_ARENA_SIZE;
  return nb_queue_sized(arena_sz PASS_REGS);
}

static Int nb_queue2(USES_REGS1) {
  Term t = Deref(ARG2);
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "nb_queue");
    return FALSE;
  }
  if (!IsIntegerTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, t, "nb_queue");
    return FALSE;
  }
  return nb_queue_sized((UInt)IntegerOfTerm(t) PASS_REGS);
}

static CELL *GetQueue(Term t, char *caller) {
  t = Deref(t);

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, caller);
    return NULL;
  }
  if (!IsApplTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_COMPOUND, t, caller);
    return NULL;
  }
  if (FunctorOfTerm(t) != FunctorNBQueue) {
    Yap_ThrowError(DOMAIN_ERROR_ARRAY_TYPE, t, caller);
    return NULL;
  }
  return RepAppl(t) + 1;
}

static Term GetQueueArena(CELL *qd, char *caller) {
  Term t = Deref(qd[QUEUE_ARENA]);

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, caller);
    return 0L;
  }
  if (!IsApplTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_COMPOUND, t, caller);
    return 0L;
  }
  if (FunctorOfTerm(t) != FunctorBigInt) {
    Yap_ThrowError(DOMAIN_ERROR_ARRAY_TYPE, t, caller);
    return 0L;
  }
  return t;
}

static void RecoverArena(Term arena USES_REGS) {
  CELL *pt = ArenaPt(arena), *a_max = ArenaLimit(arena);

  //  printf("%p/%p %p %lx %lx\n", pt,HR, a_max, pt[0], a_max[-1]);
  if (a_max == HR) {
    HR = pt;
          choiceptr bb = B;
      while (bb && bb->cp_h > HR) {
        bb->cp_h = HR;

        bb = bb->cp_b;
      }

  } else {
    while (pt < a_max)
      *pt++ = TermNil;
  }
}

static void RecoverQueue(Term *qp USES_REGS) {
  Term arena = qp[QUEUE_ARENA];
  RecoverArena(arena PASS_REGS);
  qp[QUEUE_ARENA] = MkIntTerm(0);
}

static Int nb_queue_close(USES_REGS1) {
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
      RecoverQueue(qp PASS_REGS);
    if (qp[QUEUE_SIZE] == MkIntTerm(0)) {
      return Yap_unify(ARG3, ARG2);
    }
    out = Yap_unify(ARG3, qp[QUEUE_TAIL]) && Yap_unify(ARG2, qp[QUEUE_HEAD]);
    RESET_VARIABLE(qp + QUEUE_TAIL);
    qp[QUEUE_HEAD] = qp[QUEUE_TAIL];
    RESET_VARIABLE(qp+QUEUE_TAIL);
    qp[QUEUE_SIZE] = MkIntTerm(0);
    return out;
  }
  Yap_ThrowError(INSTANTIATION_ERROR, t, "queue/3");
  return FALSE;
}

static Int nb_queue_enqueue(USES_REGS1) {
  size_t sz = 4*MIN_ARENA_SIZE, qsize;
  Term arena, to;
  CELL *qd;
  while (true) {
    qd = GetQueue(ARG1, "enqueue");
    if (!qd)
      return FALSE;
    arena = GetQueueArena(qd, "enqueue");
    if (arena == 0L) {
      return FALSE;
    }
    if (ArenaSzW(arena) < MIN_ARENA_SIZE+2 ||
		HR + MIN_ARENA_SIZE+2 > ASP) {
	    sz = sz*2;
      expand(sz, &arena);
    } else {
      break;
    }
  }
  CELL *ar = ArenaPt(arena),
  *arl = ArenaLimit(arena);
  
  RESET_VARIABLE(ar);
  RESET_VARIABLE(ar+1);
  qd[QUEUE_ARENA] = arena = CreateNewArena(ar+2,arl);
  to = AbsPair(ar);
  qsize = IntegerOfTerm(qd[QUEUE_SIZE]);
  if (qsize == 0) {
    qd[QUEUE_HEAD] = to;
  } else {
    VarOfTerm(qd[QUEUE_TAIL])[0] = to;
  }
  qd[QUEUE_SIZE] = MkIntTerm(qsize + 1);
  qd[QUEUE_TAIL] = RepPair(to)[1];
  Term tf = CopyTermToArena(Deref(ARG2), false, true, &arena, NULL PASS_REGS);
  qd = GetQueue(ARG1, "enqueue");
  qd[QUEUE_ARENA] = arena;
  /* garbage collection ? */
    VarOfTerm(qd[QUEUE_TAIL])[-1] = tf;
  return true;
}

static Int nb_queue_dequeue(USES_REGS1) {
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
static Int nb_queue_replace(USES_REGS1) {
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

static Int nb_queue_peek(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "queue_peek");
  UInt qsz;

  if (!qd)
    return FALSE;
  qsz = IntegerOfTerm(qd[QUEUE_SIZE]);
  if (qsz == 0)
    return FALSE;
  return Yap_unify(HeadOfTerm(qd[QUEUE_HEAD]), ARG2);
}

static Int nb_queue_empty(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "queue_empty");

  if (!qd)
    return FALSE;
  return (IntegerOfTerm(qd[QUEUE_SIZE]) == 0);
}

static Int nb_queue_size(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "queue_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2, qd[QUEUE_SIZE]);
}

static Int nb_queue_show(USES_REGS1) {
  CELL *qd = GetQueue(ARG1, "queue_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2, qd[QUEUE_HEAD]);
}

static CELL *GetHeap(Term t, char *caller) {
  t = Deref(t);

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, caller);
    return NULL;
  }
  if (!IsApplTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_COMPOUND, t, caller);
    return NULL;
  }
  return RepAppl(t) + 1;
}

static Term MkZeroApplTerm(Atom f, UInt sz) {
  Term t0;
  CELL *pt, *pt0;

  pt0 = HR;
  Functor fsz = Yap_MkFunctor(f, sz);
  *HR = (CELL)fsz;
  t0 = MkIntTerm(0);
  pt = HR + 1;
  while (sz--) {
    *pt++ = t0;
  }
  HR = pt;
  return CreateNewArena(pt0, HR);
}

static Int nb_heap(USES_REGS1) {
  UInt hsize;
  Term tsize;
  UInt arena_sz;
 restart:
  tsize = Deref(ARG1);
    if (IsVarTerm(tsize)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tsize, "nb_heap");
    return FALSE;
  } else {
    if (!IsIntegerTerm(tsize)) {
      Yap_ThrowError(TYPE_ERROR_INTEGER, tsize, "nb_heap");
      return FALSE;
    }
    hsize = IntegerOfTerm(tsize);
  }
  if (arena_sz < hsize)
    arena_sz = hsize;
  if (arena_sz < 1024) {
    arena_sz = 1024;
  }
  size_t sz = (8 * hsize * 2 + 16);
  if (HR + sz > ASP - 1024) {
    if (sz > HR-H0) {
      Yap_growstack(sz * CellSize);
    } else if (!Yap_dogcl(sz * CellSize)) {
        Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
                       "No Stack Space for Non-Backtrackable terms");
      }
    sz *= 2;
      goto restart;
  }
  Term heap = MkZeroApplTerm(AtomHeapData, 8 * hsize + HEAP_START + 1);
  if (heap != TermNil) {

    CELL *ar = RepAppl(heap) + 1;
    ar[HEAP_ARENA] = CreateNewArena(ar + 2 * hsize + HEAP_START,
                                    ar + 8 * hsize + HEAP_START);
    ar[HEAP_SIZE] = MkIntTerm(0);
    ar[HEAP_MAX] = MkIntegerTerm(hsize);
    if (heap == 0L) {
      return false;
    }

    return Yap_unify(heap, ARG2);
  }
  return true;
}

static Int nb_heap_close(USES_REGS1) {
  Term t = Deref(ARG1);
  if (!IsVarTerm(t)) {
    CELL *qp;

    qp = RepAppl(t) + 1;
    if (qp[HEAP_ARENA] != MkIntTerm(0))
      RecoverArena(qp[HEAP_ARENA] PASS_REGS);
    qp[-1] = (CELL)Yap_MkFunctor(AtomHeapData, 1);
    qp[0] = MkIntegerTerm(0);
    return TRUE;
  }
  Yap_ThrowError(INSTANTIATION_ERROR, t, "heap_close/1");
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

CELL *new_heap_entry(CELL *qd) {
  size_t size = HEAP_START + 2 * IntOfTerm(qd[HEAP_MAX]);
  size_t indx = HEAP_START + 2 * IntOfTerm(qd[HEAP_SIZE]);
  size_t extra = 8*MIN_ARENA_SIZE + 2 * size; // in double cells
  size_t howmany =extra;
  CELL *a_max = qd + size;
  if (size < indx + 10) {
    while (true) {
      CELL *new_max = a_max;
      size_t nsize;
      if ((nsize = Yap_InsertInGlobal(a_max, howmany * CellSize, &new_max) /
                  CellSize) >= howmany) {
      a_max = new_max;
      extra = nsize;
      break;
      }
      if(!Yap_dogcl(extra * CellSize)) {
      Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
                     "No Stack Space for Non-Backtrackable terms");
      }
  }
    qd = a_max - size;
    qd[HEAP_MAX] = MkIntTerm((size + extra) / 2 - HEAP_START);
    if (!qd)
      return NULL;
    CELL *top = a_max + extra;
    qd[-1] = (CELL)Yap_MkFunctor(AtomHeapData, size + extra);
    while (a_max < top) {
      a_max[0] = a_max[1] = TermNil;
      a_max += 2;
    }
  }
  return qd;
}

static Int nb_heap_add_to_heap(USES_REGS1) {
  CELL *qd, *pt;
  Term arena = 0, to;
  size_t hsize;

  qd = new_heap_entry(GetHeap(ARG1, "add_to_heap"));
  if (qd) {
    arena = qd[HEAP_ARENA];
    if (arena == 0L)
      return false;
  } else {
    return false;
  }
  Term l = MkPairTerm(ARG2, ARG3);
  to = CopyTermToArena(l, true, true, &arena, NULL PASS_REGS);
  qd = GetHeap(Deref(ARG1), "add_to_heap)");
  hsize = IntegerOfTerm(qd[HEAP_SIZE]);
  pt = qd + HEAP_START;
  pt[2 * hsize] = HeadOfTerm(to);
  /* protect key in ARG2 in case there is an overflow while copying to */
  pt[2 * hsize + 1] = TailOfTerm(to);
  Term thsz = Global_MkIntegerTerm(hsize + 1);

  qd[HEAP_ARENA] = arena;
  qd[HEAP_SIZE] = thsz;
  return TRUE;
}

static Int nb_heap_del(USES_REGS1) {
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

static Int nb_heap_peek(USES_REGS1) {
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

static Int nb_heap_empty(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "heap_empty");

  if (!qd)
    return FALSE;
  return (IntegerOfTerm(qd[HEAP_SIZE]) == 0);
}

static Int nb_heap_size(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "heap_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2, qd[HEAP_SIZE]);
}

static Int nb_beam(USES_REGS1) {
  Term beam, *ar;
  UInt hsize;
  Term tsize = Deref(ARG1);
  UInt arena_sz = (HR - H0) / 16;

  if (IsVarTerm(tsize)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tsize, "nb_beam");
    return FALSE;
  } else {
    if (!IsIntegerTerm(tsize)) {
      Yap_ThrowError(TYPE_ERROR_INTEGER, tsize, "nb_beam");
      return FALSE;
    }
    hsize = IntegerOfTerm(tsize);
  }
  if (arena_sz < 1024)
    arena_sz = 1024;
  while (HR + (3 * hsize + arena_sz + 16) > ASP - 1024) {
    CreepFlag = EventFlag =
        StackGap(PASS_REGS1) + (5 * hsize + HEAP_START + 1 + arena_sz);

    gc_entry_info_t info;
    Yap_track_cpred(0, P, 0, &info);
    // p should be past the environment minus Obpp
    if (!Yap_gc(&info)) {
      Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
                     "stack overflow: gc failed");
    }
  }
  beam = MkZeroApplTerm(AtomHeapData, 5 * hsize + HEAP_START + 1);
  if (beam == TermNil) {
    return false;
  }
  if (!Yap_unify(beam, ARG2))
    return FALSE;
  ar = RepAppl(beam) + 1;
  ar[HEAP_ARENA] = ar[HEAP_SIZE] = MkIntTerm(0);
  ar[HEAP_MAX] = tsize;

  return TRUE;
}

static Int nb_beam_close(USES_REGS1) { return nb_heap_close(PASS_REGS1); }

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

static size_t new_beam_entry(CELL **qdp) {
  size_t hsize, hmsize;
  CELL *qd = *qdp;
  hsize = IntegerOfTerm(qd[HEAP_SIZE]);
  hmsize = IntegerOfTerm(qd[HEAP_MAX]);
  if (!qd)
    return 0;
  Term *pt;
  if (hsize >= hmsize - 10) {
    size_t nsize;
    
    size_t sz = 2 * hsize + HEAP_START, ex = 2 * sz;
    while (true) {
      CELL *new_max = qd+sz, *a_max= qd+sz;
    if ((nsize = Yap_InsertInGlobal(a_max, ex * CellSize, &new_max) /
                  CellSize) >= ex) {
      a_max = new_max;
      ex = nsize;
      qd = a_max-ex;
      break;
      }
      if(!Yap_dogcl(ex * CellSize)) {
      Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
                     "No Stack Space for Non-Backtrackable terms");
      }
  }
    qd[HEAP_MAX] = MkIntTerm(3 * hmsize);
    *qdp = qd;
  }
  pt = qd + HEAP_START;

  if (Yap_compare_terms(pt[2 * hmsize], Deref(ARG2)) > 0) {
    /* smaller than current max, we need to drop current max */
    DelBeamMax(pt, pt + 2 * hmsize, hmsize);
    hsize--;
  }

  return hsize;
}

static Int nb_beam_add_to_beam(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "add_to_beam"), *pt;
  size_t hsize, hmsize = qd[HEAP_SIZE];
  Term arena, to;

  hsize = new_beam_entry(&qd);
  arena = qd[HEAP_ARENA];
  if (arena == 0L)
    return FALSE;
  CELL *arenap = &arena;
  Term l = MkPairTerm(ARG2, ARG3);
  to = CopyTermToArena(l, FALSE, TRUE, &arena, NULL PASS_REGS);
  if (to == 0)
    return FALSE;
  qd = GetHeap(ARG1, "add_to_beam");
  arena = *arenap;
  qd[HEAP_ARENA] = arena;
  pt = qd + HEAP_START;
  PushBeam(pt, pt + 2 * hmsize, hsize, HeadOfTerm(to), TailOfTerm(to));
  qd[HEAP_SIZE] = Global_MkIntegerTerm(hsize + 1);
  return TRUE;
}

static Int nb_beam_del(USES_REGS1) {
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

static Int nb_beam_check(USES_REGS1) {
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
      fprintf(stderr, "Error at %ld\n", (unsigned long int)i);
      return FALSE;
    }
    back = IntegerOfTerm(pt[2 * i + 1]);
    if (IntegerOfTerm(pt2[3 * back + 1]) != i) {
      fprintf(stderr, "Link error at %ld\n", (unsigned long int)i);
      return FALSE;
    }
  }
  for (i = 1; i < qsz; i++) {
    if (Yap_compare_terms(pt2[3 * ((i + 1) / 2 - 1)], pt2[3 * i]) < 0) {
      fprintf(stderr, "Error at sec %ld\n", (unsigned long int)i);
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

static Int nb_beam_keys(USES_REGS1) {
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
      if (!Yap_dogc()) {
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

static Int nb_beam_peek(USES_REGS1) {
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

static Int nb_beam_empty(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "beam_empty");

  if (!qd)
    return FALSE;
  return (IntegerOfTerm(qd[HEAP_SIZE]) == 0);
}

static Int nb_beam_size(USES_REGS1) {
  CELL *qd = GetHeap(ARG1, "beam_size");

  if (!qd)
    return FALSE;
  return Yap_unify(ARG2, qd[HEAP_SIZE]);
}

static Int cont_current_nb(USES_REGS1) {
  Int unif;
  GlobalEntry *ge = (GlobalEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(1, 1));

  unif = Yap_unify(MkAtomTerm(AbsAtom(ge->AtomOfGE)), ARG1);
  ge = ge->NextGE;
  if (!ge) {
    if (unif)
      cut_succeed();
    else
      cut_fail();
  } else {
    EXTRA_CBACK_ARG(1, 1) = MkIntegerTerm((Int)ge);
    return unif;
  }
}

static Int init_current_nb(USES_REGS1) { /* current_atom(?Atom)
                                          */
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    if (IsAtomTerm(t1)) {
      if (!FindGlobalEntry(AtomOfTerm(t1) PASS_REGS)) {
        cut_fail();
      } else {
        cut_succeed();
      }
    } else {
      Yap_ThrowError(TYPE_ERROR_ATOM, t1, "nb_current");
      cut_fail();
    }
  }
  READ_LOCK(HashChain[0].AERWLock);
  EXTRA_CBACK_ARG(1, 1) = MkIntegerTerm((Int)LOCAL_GlobalVariables);
  return cont_current_nb(PASS_REGS1);
}


void Yap_InitGlobals(void) {
  CACHE_REGS
  Term cm = CurrentModule;
  Yap_InitCPred("$allocate_arena", 2, p_allocate_arena, 0);
  Yap_InitCPred("arena_size", 1, arena_size, 0);
  Yap_InitCPred("b_setval", 2, p_b_setval, SafePredFlag);
  Yap_InitCPred("__B_setval__", 2, p_b_setval, HiddenPredFlag | SafePredFlag);
  /** @pred  b_setval(+ _Name_, + _Value_)


      Associate the term  _Value_ with the atom  _Name_ or replaces
      the currently associated value with  _Value_. If  _Name_ does
      not refer to an existing global variable a variable with initial
      value
      [] is created (the empty list). On backtracking the assignment is
      reversed.


  */
  /** @pred b_setval(+ _Name_,+ _Value_)


      Associate the term  _Value_ with the atom  _Name_ or replaces
      the currently associated value with  _Value_.  If  _Name_ does
      not refer to an existing global variable a variable with initial
      value
      `[]` is created (the empty list).  On backtracking the
      assignment is reversed.


  */
  Yap_InitCPred("nb_setval", 2, nb_setval, 0L);
  Yap_InitCPred("__NB_setval__", 2, nb_setval, HiddenPredFlag);
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
  Yap_InitCPred("nb_set_shared_val", 2, nb_set_shared_val, 0L);
  /** @pred  nb_set_shared_val(+ _Name_, + _Value_)


      Associates the term  _Value_ with the atom  _Name_, but sharing
      non-backtrackable terms. This may be useful if you want to rewrite a
      global variable so that the new copy will survive backtracking, but
      you want to share structure with the previous term.

      The next example shows the differences between the three built-ins:

      ~~~~~
      ?-
      nb_setval(a,a(_)),nb_getval(a,A),nb_setval(b,t(C,A)),nb_getval(b,B).
      A = a(_A), B = t(_B,a(_C)) ?

      ?-
      nb_setval(a,a(_)),nb_getval(a,A),nb_set_shared_val(b,t(C,A)),nb_getval(b,B).

      ?-
      nb_setval(a,a(_)),nb_getval(a,A),nb_linkval(b,t(C,A)),nb_getval(b,B).
      A = a(_A),
      B = t(C,a(_A)) ?
      ~~~~~


  */
  Yap_InitCPred("nb_linkval", 2, nb_linkval, 0L);
  /** @pred  nb_linkval(+ _Name_, + _Value_)


      Associates the term  _Value_ with the atom  _Name_ without
      copying it. This is a fast special-purpose variation of nb_setval/2
      intended for expert users only because the semantics on backtracking
      to a point before creating the link are poorly defined for compound
      terms. The principal term is always left untouched, but backtracking
      behaviour on arguments is undone if the original assignment was
      trailed and left alone otherwise, which implies that the history
      that created the term affects the behaviour on backtracking. Please
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
  Yap_InitCPred("$nb_getval", 3, nb_getval, SafePredFlag);
  Yap_InitCPred("__NB_getval__", 3, nb_getval, HiddenPredFlag);
  Yap_InitCPred("__B_getval__", 3, nb_getval, HiddenPredFlag);
  Yap_InitCPred("nb_setarg", 3, nb_setarg, 0L);
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
      exceptions. Realising these features with a traditional
      implementation based on assert/retract or flag/3 is much more
      complicated.

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
  Yap_InitCPred("nb_set_shared_arg", 3, nb_set_shared_arg, 0L);
  /** @pred  nb_set_shared_arg(+ _Arg_, + _Term_, + _Value_)



      As nb_setarg/3, but like nb_linkval/2 it does not
      duplicate the global sub-terms in  _Value_. Use with extreme care
      and consult the documentation of nb_linkval/2 before use.


  */
  Yap_InitCPred("nb_linkarg", 3, nb_linkarg, 0L);
  /** @pred  nb_linkarg(+ _Arg_, + _Term_, + _Value_)



      As nb_setarg/3, but like nb_linkval/2 it does not
      duplicate  _Value_. Use with extreme care and consult the
      documentation of nb_linkval/2 before use.


  */
  Yap_InitCPred("nb_delete", 1, nb_delete, 0L);
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
  Yap_InitCPred("nb_create", 3, nb_create, 0L);
  Yap_InitCPred("nb_create", 4, nb_create2, 0L);
  Yap_InitCPredBack("$nb_current", 1, 1, init_current_nb, cont_current_nb,
                    SafePredFlag);
  Yap_InitCPred("copy_term", 2, p_copy_term, 0);
  /** @pred  copy_term(? _TI_,- _TF_) is iso


      Term  _TF_ is a variant of the original term  _TI_, such that for
      each variable  _V_ in the term  _TI_ there is a new variable  _V'_
      in term  _TF_. Notice that:

      + suspended goals and attributes for attributed variables in _TI_
      are also duplicated;
      + ground terms are shared between the new and the old term.

      If you do not want any sharing to occur please use
      duplicate_term/2.


  */
  Yap_InitCPred("duplicate_term", 2, p_duplicate_term, 0);
  /** @pred  duplicate_term(? _TI_,- _TF_)


      Term  _TF_ is a variant of the original term  _TI_, such that
      for each variable  _V_ in the term  _TI_ there is a new variable
      _V'_ in term  _TF_, and the two terms do not share any
      structure. All suspended goals and attributes for attributed
      variables in  _TI_ are also duplicated.

      Also refer to copy_term/2.


  */
  Yap_InitCPred("copy_term_nat", 2, p_copy_term_no_delays, 0);
  Yap_InitCPred("rational_term_to_forest", 4, p_rational_tree_to_forest, 0);
  /** @pred copy_term_nat(? _TI_,- _TF_)


      As copy_term/2.  Attributes however, are <em>not</em> copied but
      replaced by fresh variables.




  */
  CurrentModule = GLOBALS_MODULE;
  Yap_InitCPred("nb_queue", 1, nb_queue, 0L);
  Yap_InitCPred("nb_queue", 2, nb_queue2, 0L);
  Yap_InitCPred("nb_queue_close", 3, nb_queue_close, SafePredFlag);
  Yap_InitCPred("nb_queue_enqueue", 2, nb_queue_enqueue, 0L);
  Yap_InitCPred("nb_queue_dequeue", 2, nb_queue_dequeue, SafePredFlag);
  Yap_InitCPred("nb_queue_peek", 2, nb_queue_peek, SafePredFlag);
  Yap_InitCPred("nb_queue_empty", 1, nb_queue_empty, SafePredFlag);
  Yap_InitCPred("nb_queue_replace", 3, nb_queue_replace, SafePredFlag);
  Yap_InitCPred("nb_queue_size", 2, nb_queue_size, SafePredFlag);
  Yap_InitCPred("nb_queue_show", 2, nb_queue_show, SafePredFlag);
  Yap_InitCPred("nb_heap", 2, nb_heap, 0L);
  Yap_InitCPred("nb_heap_close", 1, nb_heap_close, SafePredFlag);
  Yap_InitCPred("nb_heap_add", 3, nb_heap_add_to_heap, 0L);
  Yap_InitCPred("nb_heap_del", 3, nb_heap_del, SafePredFlag);
  Yap_InitCPred("nb_heap_peek", 3, nb_heap_peek, SafePredFlag);
  Yap_InitCPred("nb_heap_empty", 1, nb_heap_empty, SafePredFlag);
  Yap_InitCPred("nb_heap_size", 2, nb_heap_size, SafePredFlag);
  Yap_InitCPred("nb_beam", 2, nb_beam, 0L);
  Yap_InitCPred("nb_beam_close", 1, nb_beam_close, SafePredFlag);
  Yap_InitCPred("nb_beam_add", 3, nb_beam_add_to_beam, 0L);
  Yap_InitCPred("nb_beam_del", 3, nb_beam_del, SafePredFlag);
  Yap_InitCPred("nb_beam_peek", 3, nb_beam_peek, SafePredFlag);
  Yap_InitCPred("nb_beam_empty", 1, nb_beam_empty, SafePredFlag);
  Yap_InitCPred("nb_beam_keys", 2, nb_beam_keys, 0L);
  Yap_InitCPred("nb_create_accumulator", 2, nb_create_accumulator, 0L);
  Yap_InitCPred("nb_add_to_accumulator", 2, nb_add_to_accumulator, 0L);
  Yap_InitCPred("nb_accumulator_value", 2, nb_accumulator_value, 0L);
#ifdef DEBUG
  Yap_InitCPred("nb_beam_check", 1, nb_beam_check, SafePredFlag);
#endif
  Yap_InitCPred("nb_beam_size", 2, nb_beam_size, SafePredFlag);
  CurrentModule = cm;
}

#endif

/**
   @}
*/
