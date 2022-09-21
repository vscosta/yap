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
* File:		copy.c   						 *
* Last rev:								 *
* mods:									 *
* comments:	support from multiple assignment variables in YAP	 *
*									 *
*************************************************************************/


/** 
 * @file copy.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 23:16:17 2015
 *
 * @brief  support for backtrable and non-backtrackable variables in Prolog.
 *
 *
 */


/**
   @defgroup TermCopy Copying Terms
   @ingroup Builtins
   @{
*/



#include <stdbool.h>
#define DEB_DOOBIN(d0)                                                         \
(fprintf(stderr, "+++ %s ", __FUNCTION__), Yap_DebugPlWriteln(d0))
#define DEB_DOOBOUT(d0) (fprintf(stderr, "--- "), Yap_DebugPlWriteln(d0))
#define DEB_DOOB(S)                                                            \
(fprintf(stderr, "%s %ld %p=%p %p--%d\n ", S, to_visit - to_visit0, pt0,     \
 ptf, *AbsAppl(pt0), arity)) //, Yap_DebugPlWriteln(d0))

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

#include "YapArenas.h"
#include "YapError.h"

#include "terms.h"

#ifdef HAVE_STRING_H

#include "string.h"

#endif

/* pointer to top of an arena */
CELL *Yap_ArenaLimit(Term arena) { return ArenaLimit(arena); }

CELL *Yap_ArenaPt(Term arena) { return ArenaPt(arena); }

UInt Yap_ArenaSz(Term arena) { return ArenaSzW(arena); }

/* Non-backtrackable terms will from now on be stored on arenas, a
   special term on the heap. Arenas automatically contract as we add terms to
   the front.

*/

static int copy_complex_term(CELL *pt0_, CELL *pt0_end_, bool share,
                             bool copy_att_vars, CELL *ptf_,
                             Term *bindp,
                             Ystack_t *stt USES_REGS);



Term Yap_MkArena(CELL *ptr, CELL *max) {
  CACHE_REGS
    Term t = AbsAppl(ptr);


    //  printf("<< %p %p %ld     \n", ptr, max, max - ptr);
    ptr[0] = (CELL) FunctorBlob;
    ptr[1] = EMPTY_ARENA;
    size_t size = (max - 1) - (ptr + 3);
    ptr[2] = size;
    max[-1] = CloseExtension(ptr);
    if (max >= HR)
        HR = max;
    return t;
}

bool Yap_ArenaExpand(size_t sz, CELL *arenap, bool first_try) {
  CACHE_REGS

    if (!arenap) {
    sz += MinStackGap;
       if (first_try) {
       
         if (!Yap_dogcl(sz * CellSize PASS_REGS)) {
                Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
                               "No Stack Space for Non-Backtrackable terms");
            }
            }

else if (!Yap_growstack(sz*sizeof(CELL))){
                Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
                               "No Stack Space for Non-Backtrackable terms");
            }
        
        return true;
    } else 
    {
        size_t nsz;
        size_t sz0 = ArenaSzW(*arenap);
        yhandle_t ys = Yap_PushHandle(*arenap);
        while (true) {
            CELL *shifted_max;
                    sz += 3*MIN_ARENA_SIZE;
             if (first_try &&  !Yap_dogcl(sz * CellSize PASS_REGS)) {
            Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
                           "No Stack Space for Non-Backtrackable terms");
        }
       CELL *a_max = ArenaLimit(*arenap);
            nsz = Yap_InsertInGlobal(a_max-1 ,
				     sz * CellSize, &shifted_max) /
	      CellSize;
            if (nsz >= sz) {
	      shifted_max+=1;
                CELL *ar_max = shifted_max + nsz;
                CELL *ar_min = shifted_max - sz0;
                Yap_PopHandle(ys);
                *arenap = Yap_MkArena(ar_min, ar_max);
                return true;
            }
   first_try = false;
        }

        *arenap = Yap_PopHandle(ys);
    }
  return true;
}

static Int allocate_arena(USES_REGS1) {
    Term t = Deref(ARG1);
    if (IsVarTerm(t)) {
        Yap_ThrowError(INSTANTIATION_ERROR, t, "allocate_arena");
        return FALSE;
    } else if (!IsIntegerTerm(t)) {
        Yap_ThrowError(TYPE_ERROR_INTEGER, t, "allocate_arena");
        return FALSE;
    }
    size_t sz = IntegerOfTerm(t);
    Term a = Yap_MkArena(HR, HR + sz);
    return Yap_unify(ARG2, a);
}

static Int arena_size(USES_REGS1) {
    return Yap_unify(ARG1, MkIntegerTerm(ArenaSzW(LOCAL_GlobalArena)));
}

void Yap_AllocateDefaultArena(size_t gsizeW, int wid, void *cs) {
 CACHE_REGS
    REMOTE_GlobalArena(wid) = Yap_MkArena(H0, H0 + gsizeW);
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

static bool visitor_error_handler( yap_error_number err, Ystack_t *stt,
				   size_t min_grow, Term *arenap) {
  bool first_try = false;
  if (err == RESOURCE_ERROR_TRAIL) {
    
        if (!Yap_growtrail(0, false)) {
            Yap_ThrowError(RESOURCE_ERROR_TRAIL, TermNil, "while visiting terms");

        }
        stt->err = YAP_NO_ERROR;
  }else if (err == RESOURCE_ERROR_AUXILIARY_STACK) {
           
               if (!realloc_stack(stt)) {
                   Yap_ThrowError(RESOURCE_ERROR_TRAIL, TermNil, "while visiting terms");
               }
        stt->err = YAP_NO_ERROR;
} else if (err == RESOURCE_ERROR_STACK) {
    return Yap_ArenaExpand(min_grow, arenap, first_try);
	first_try = false;
    }
        stt->err = YAP_NO_ERROR;

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
static int  copy_complex_term(CELL *pt0_, CELL *pt0_end_, bool share,
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
	    //	    extern long long vsc_count;
	    //extern void jmp_deb(int);
	    //if (vsc_count == 66) jmp_deb(1);
            //printf("%d %d %p %p-%p\n",vsc_count++,pt0-HB,pt0,HR,ptf);
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
                /// ptd0 is the dereferenced version of d0
                ///
                ///
                if (ptd1 >= HB && ptd1 < ASP) {
                    *ptf = AbsPair(ptd1);
                    continue;                } else if (IS_VISIT_MARKER(*ptd1)) {
                    /* d0 has ance   */
                    struct cp_frame *entry = VISIT_ENTRY(*ptd1);
                    Term val = entry->t;
                    if (IsVarTerm(val)) {
                        *ptf = val;
                    } else if (forest) {
                        // break represent a loopy list by introducing explicit unification
                        //

                        // set up a binding PTF=D0
                        Term l = AbsAppl(HR);
                        RESET_VARIABLE(ptf);
                        HR[0] = (CELL) FunctorEq;
                        *ptf = entry->t = HR[1] = (CELL) ptf;
                        HR[2] = val;
                        if (HR + 3 > ASP - MIN_ARENA_SIZE) {
                            // same as before
                            return stt->err = RESOURCE_ERROR_STACK;
                        }
                                                                                                                                                                                                                                                                                          HR += 3;
                        if (bindp)
                           *bindp = MkPairTerm(l, *bindp);
                    } else {
                        *ptf = val;
                        TrailedMaBind(ptd0, (CELL) ptf);
                    }
                    continue;
                }

                // first time we meet,
                // save state
                myt = AbsPair(HR);
                if (stt->pt + 2 >= stt->max && !realloc_stack(stt)) {
                    return stt->err = RESOURCE_ERROR_AUXILIARY_STACK;
                }

                if (share) {
                    d0 = AbsPair(ptf);
                    mTrailedMaBind(ptd0, d0);
                    if (TR + 32 >= (tr_fr_ptr) LOCAL_TrailTop) {
                        return stt->err = RESOURCE_ERROR_TRAIL;
                    }
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
                       return stt->err = RESOURCE_ERROR_STACK;
                }
                HR += 2;
                continue;
            } else if (IsApplTerm(d0)) 
	      {
		CELL *ptd1 = RepAppl(d0);
                CELL dd1 = *ptd1;
                if (ptd1 >= HB && ptd1 < ASP) {
                    /* If this is newer than the current term, just reuse */
                    *ptf = d0;
                    continue;
                }

                if (IsExtensionFunctor((Functor) dd1)) {
		  Functor f = (Functor) dd1;
         if (f == FunctorDBRef) {
                    *ptf = d0;
        } else {
	    Term t = d0;
	    size_t szop = SizeOfOpaqueTerm(RepAppl(t), (CELL) f);
            	      if (ASP-HR < szop+MIN_ARENA_SIZE) {
		return stt->err = RESOURCE_ERROR_STACK;
	      }
	      *ptf = AbsAppl(HR);
	      memmove(HR, RepAppl(t), (szop-1) * CellSize);
	      HR[szop - 1] = CloseExtension(HR);
	      HR+=szop;
                  continue;
                }
		}else if (IS_VISIT_MARKER(dd1)) {
                    /* If this is newer than the current term, just reuse */
                    // set up a binding P1TF=D
                    struct cp_frame *entry = VISIT_ENTRY(dd1);
                    Term val = entry->t;
                    if (IsVarTerm(val)) {
                        *ptf = val;
                    } else if (forest) {
		      // set up a binding PTF=D0
		      Term l = AbsAppl(HR);
		      RESET_VARIABLE(ptf);
		      HR[0] = (CELL) FunctorEq;
		      *ptf = entry->t = HR[1] = (CELL) ptf;
		      HR[2] = val;
		      HR += 3;
		      if (bindp)
			*bindp = MkPairTerm(l, *bindp);
                    } else {
		      // same as before
		      *ptf = val;
		      TrailedMaBind(ptd0, (CELL) ptf);
		      if (TR + 32 >= (tr_fr_ptr) LOCAL_TrailTop) {
			return stt->err = RESOURCE_ERROR_TRAIL;
		      }
                    }
                } else {
		  Term d1 = dd1;
		  Functor f = (Functor) d1;
		  arity_t arity;
                  if (f == FunctorAttVar) {
                    myt = *ptf = (CELL)(HR+1);
                    arity = 3;
		  } else {
		    myt = *ptf = AbsAppl(HR);
		    arity = ArityOfFunctor(f);
		  }
		  if (share) {
		    d0 = AbsAppl(ptf);
		    TrailedMaBind(ptd0, d0);
		    if (TR + 32 >= (tr_fr_ptr) LOCAL_TrailTop) {
		      return stt->err = RESOURCE_ERROR_TRAIL;
		    }
		  }
		  /* store the terms to visit */
		  if (to_visit + 2  >= to_visit_end && !realloc_stack(stt)) {
		    return stt->err = RESOURCE_ERROR_AUXILIARY_STACK;
                    }
                    to_visit->tr = TR;
                    to_visit->pt0 = pt0;
                    to_visit->pt0_end = pt0_end;
                    to_visit->ptf = ptf;
                    to_visit->t = myt;
                    to_visit->ground = ground;
                    to_visit->oldp = ptd1;
                    to_visit->oldv = (CELL) f;
                    *ptd1 = VISIT_MARK();
                    to_visit++;
                    ground = (f != FunctorMutable);
                    pt0 = ptd1;
                    pt0_end = ptd1 + arity;
                    /* store the functor for the new term */
                    HR[0] = (CELL) f;
                    ptf = HR;
                    if (HR > ASP - (arity + MIN_ARENA_SIZE)) {
                        return stt->err  = RESOURCE_ERROR_STACK;
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
                    *ptf = (CELL) ptd0;
                }
                continue;
            }
	    bool attv = GlobalIsAttVar(ptd0);
            if (copy_att_vars && attv) {
                /* if unbound, call the standard copy term routine */
                //	  if (true) { //||!GLOBAL_atd0)].copy_term_op) {
                /* store the terms to visit */
	      if (to_visit + 8 >= to_visit_end && !realloc_stack(stt)) {
		return stt->err = RESOURCE_ERROR_AUXILIARY_STACK;
                }
	      *ptf = (CELL)(HR+1);
		mBind_And_Trail(ptd0, (CELL)(HR+1));
		to_visit->pt0 = pt0;
                to_visit->pt0_end = pt0_end;
                to_visit->ptf = ptf;
                to_visit->t = AbsAppl(HR);
                to_visit->ground = false;
                to_visit->tr = TR;
                to_visit->oldp = ptd0 - 1;
                to_visit->oldv = (CELL) FunctorAttVar;
                ptd0[-1] = VISIT_MARK();
                to_visit++;
                ground = false;
                pt0 = ptd0;
                pt0_end = ptd0 + 2;
                /* store the functor for the new term */
                HR[0] = (CELL) FunctorAttVar;
		RESET_VARIABLE(HR+1);
                ptf = HR+1;
                if (HR > ASP - (3 + MIN_ARENA_SIZE)) {
                    return stt->err = RESOURCE_ERROR_STACK;
                }
                HR += 3 + 1;
            } else {
                RESET_VARIABLE(ptf);
                mBind_And_Trail(ptd0, (CELL) ptf);
                if (TR + 32 >= (tr_fr_ptr) LOCAL_TrailTop) {
                    return stt->err = RESOURCE_ERROR_TRAIL;
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

Term CopyTermToArena(Term t,
                            bool share, bool copy_att_vars,
		     yap_error_number *errp,
                            Term *arenap, Term *bindp USES_REGS) {
    Ystack_t ystk, *stt = &ystk;
    size_t expand_stack;
    Functor f;
    CELL *base;
    t = Deref(t);
    if (IsVarTerm(t)) {
            if (arenap && *arenap) {
                CELL *base = ArenaPt(*arenap);
                CELL *end = ArenaLimit(*arenap);
		RESET_VARIABLE(base);
		base++;
		*arenap = Yap_MkArena(base,end);
		return (CELL)(base-1);
		  }
        if (!IsAttVar(VarOfTerm(t)) || !copy_att_vars) {
            HR++;
            RESET_VARIABLE(HR - 1);
            return (CELL) (HR - 1);
        }
    } else if (IsAtomOrIntTerm(t)) {
        return t;
    } else if (IsApplTerm(t) && IsExtensionFunctor((f = FunctorOfTerm(t)))) {
        if (f == FunctorDBRef) {
	  return t;
        } else {
	    CELL *end;
	  
	    size_t szop = SizeOfOpaqueTerm(RepAppl(t), (CELL) f);
            if (arenap && *arenap) {
	      if (ArenaSzW(*arenap) < szop+MIN_ARENA_SIZE) {
		yhandle_t yt1, yt;
		yt = Yap_InitHandle(t);
		if (bindp)
		  yt1 = Yap_InitHandle(*bindp);
		Yap_ArenaExpand(szop+MIN_ARENA_SIZE,
				arenap, true);
		if (bindp)
		  *bindp = Yap_PopHandle(yt1);
		t = Yap_PopHandle(yt);
	      }
	      base = ArenaPt(*arenap);
	      CELL *limit =  ArenaLimit(*arenap);
	      end = base+szop;
	      memmove(base, RepAppl(t), (szop) * CellSize);
	      end[ - 1] = CloseExtension(end-szop);
	      Term tf = AbsAppl(base);
	      *arenap = Yap_MkArena(end, limit);
	      return tf;
	    } else {
	      while (HR + (MIN_ARENA_SIZE + szop) > ASP) {
		yhandle_t yt1, yt;
		yt = Yap_InitHandle(t);
		if (bindp)
		  yt1 = Yap_InitHandle(*bindp);
		if (!Yap_dogcl(2 * MIN_ARENA_SIZE + szop PASS_REGS)) {
		  stt->err = RESOURCE_ERROR_STACK;
		  break;
		}
		if (bindp)
		  *bindp = Yap_PopHandle(yt1);
		t = Yap_PopHandle(yt);
	      }
	      memmove(HR, RepAppl(t), (szop - 1) * CellSize);
	      Term tf = AbsAppl(HR);
	      HR[szop - 1] = CloseExtension(HR);
	      HR += szop;
	      return tf;
	    }
	}
    }

    CELL *start, *end;
    int i = push_text_stack();
    expand_stack =(HR-H0)/8;
    if (expand_stack < 4 * MIN_ARENA_SIZE)
        expand_stack = 4 * MIN_ARENA_SIZE;
    if (expand_stack > 2 * K * K)
        expand_stack = 2 * K * K;
    stt->pt0 = NULL;
    init_stack(stt);
    while (true) {
        CELL *ap = &t;
        CELL *pf;
        CELL *hr, *asp;
        hr = HR;
        asp = ASP;
        if (arenap && *arenap) {
            start = ArenaPt(*arenap);
            end = ArenaLimit(*arenap);
            HR = start;
            ASP = end;
        }
        HB = HR;
        pf = HR;
	stt->err = YAP_NO_ERROR;
        stt->err = copy_complex_term(ap - 1, ap, share, copy_att_vars, pf, bindp,
                                stt PASS_REGS);
        if (arenap && *arenap) {
         if (stt->err == YAP_NO_ERROR) {
            *arenap = Yap_MkArena(HR, ASP);  
            } else {      
            *arenap = Yap_MkArena(start, end);
            }
            HR = hr;
            ASP = asp;
        } else if (stt->err!=YAP_NO_ERROR) {
            HR=HB;
        }

        HB = B->cp_h;
        // done, exit!
        if (stt->err == YAP_NO_ERROR) {
	if (!share) {
	  clean_tr(B->cp_tr+stt->tr0 PASS_REGS);
	  TR = B->cp_tr+stt->tr0;
	}
            pop_text_stack(i);
            if (IsVarTerm(t))
                return (CELL) pf;
            if (IsApplTerm(t))
                return AbsAppl(pf);
            return AbsPair(pf);
        } else {
		      while (to_visit > to_visit0) {
			to_visit--;
			
			VUNMARK(to_visit->oldp, to_visit->oldv);
		      } /* restore our nice, friendly, term to its original state */
	  clean_tr(B->cp_tr+stt->tr0 PASS_REGS);
	  TR = B->cp_tr+stt->tr0;
	  if (errp && *errp) {
        stt->err = *errp;
	    return  0;
	  }
            yhandle_t yt1, yt;
	      yt = Yap_InitHandle(t);
	      if (bindp)
                yt1 = Yap_InitHandle(*bindp);
	      visitor_error_handler(stt->err, stt, expand_stack, arenap);
	      if (bindp)
                *bindp = Yap_PopHandle(yt1);
	      stt->t = t = Yap_PopHandle(yt);
	      stt->err = YAP_NO_ERROR;
        }
    }
}


Term Yap_CopyTerm(Term inp) {
    CACHE_REGS
      return CopyTermToArena(inp, false, true, NULL, NULL, NULL PASS_REGS);
}

Term Yap_CopyTermNoShare(Term inp) {
    CACHE_REGS

    COPY(inp);
    return CopyTermToArena(inp, false, true  , NULL, NULL, NULL PASS_REGS);
}

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
static Int copy_term(USES_REGS1) /* copy term t to a new instance  */
{
   COPY(ARG1);
  Term t;
  yap_error_number err = YAP_NO_ERROR;
    Term inp = MkGlobal(Deref(ARG1));
    t = CopyTermToArena(inp, false, true  , &err, NULL, NULL PASS_REGS);
    /* be careful, there may be a stack shift here */
    return Yap_unify(ARG2, t);
 }

/** @pred  duplicate_term(? _TI_,- _TF_)


        Term  _TF_ is a variant of the original term  _TI_, such that
        for each variable  _V_ in the term  _TI_ there is a new variable
        _V'_ in term  _TF_, and the two terms do not share any
        structure. All suspended goals and attributes for attributed
        variables in  _TI_ are also duplicated.

        Also refer to copy_term/2.


*/
static Int duplicate_term(USES_REGS1) /* copy term t to a new instance  */
{
  COPY(ARG1);
  Term t;
  yap_error_number err = YAP_NO_ERROR;
    Term inp = MkGlobal(Deref(ARG1));
    t = CopyTermToArena(inp, false, true  , &err, NULL, NULL PASS_REGS);
    if (t == 0L)
      return false;
    /* be careful, there may be a stack shift here */
    return Yap_unify(ARG2, t);
}

/**
 *  @pred  rational_term_to_forest(? _TI_,- _TF_, -Extras, ?More)
 *
 * Copy the term, but loops within the term are broken up and added to a list
 * of loopy assignments 
 *
 * The term _TF_ is a forest representation (without cycles and repeated
 * terms) for the Prolog term _TI_. The term _TF_ is the main term.  The
 * difference list _SubTerms_-_MoreSubterms_ stores terms of the form
 * _V=T_, where _V_ is a new variable occuring in _TF_, and _T_ is a copy
 * of a sub-term from _TI_.

 */
static Int
rational_term_to_forest(USES_REGS1) /* copy term t to a new instance  */
{
  COPY(ARG1);
  Term list = MkGlobal(Deref(ARG4));
  Term t;
    Term inp = MkGlobal(Deref(ARG1));
    COPY(ARG1);
    t = CopyTermToArena(inp, false, false ,NULL, NULL, &list PASS_REGS);
    /* be careful, there may be a stack shift here */
    return Yap_unify(ARG2, t) && Yap_unify(ARG3, list);
}

Term Yap_TermAsForest(Term t1) /* copy term t to a new instance  */
{
  CACHE_REGS
    Term list = TermNil;
    Term t = CopyTermToArena(t1, true, false  , NULL, NULL, &list PASS_REGS);
    if (t == 0L)
        return FALSE;
    if (list != TermNil) {
    /* be careful, there may be a stack shift here */
    Term ts[2];
    ts[0] = t;
    ts[1] = list;
    return Yap_MkApplTerm(FunctorAtSymbol, 2, ts);
    }
    return t;
}
      

    /** @pred copy_term_nat(? _TI_,- _TF_)


        As copy_term/2.  Attributes however, are <em>not</em> copied but
        replaced by fresh variables.
    */
static Int
copy_term_no_delays(USES_REGS1) /* copy term t to a new instance  */
{
  COPY(ARG1);
  Term t;
    Term inp = MkGlobal(Deref(ARG1));
    t = CopyTermToArena(inp, false, false  ,NULL, NULL, NULL PASS_REGS);
	
    /* be careful, there may be a stack shift here */
    return Yap_unify(ARG2, t);
}



void Yap_InitCopyTerm(void) {
    CACHE_REGS
    Term cm = PROLOG_MODULE;
    Yap_InitCPred("$allocate_arena", 2, allocate_arena, 0);
    Yap_InitCPred("arena_size", 1, arena_size, 0);
    Yap_InitCPred("copy_term", 2, copy_term, 0);
    Yap_InitCPred("duplicate_term", 2, duplicate_term, 0);
    Yap_InitCPred("copy_term_nat", 2, copy_term_no_delays, 0);
    Yap_InitCPred("rational_term_to_forest", 4, rational_term_to_forest, 0);
       CurrentModule = cm;
}

/**
   @}
*/

