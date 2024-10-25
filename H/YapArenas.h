/**********************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		YapArenas.h * Last rev:	4/03/88
 *
 * mods:
 * comments:	Memory allocation for global variables *
 *									 *
 *************************************************************************/

#ifndef YAPARENAS_H
#define YAPARENAS_H 1

#include "terms.h"

extern Term Yap_MkArena(CELL *ptr, CELL *max);
extern bool Yap_ArenaExpand(size_t sz, CELL *arenap,bool);

#define MIN_ARENA_SIZE (1024L)

#define MAX_ARENA_SIZE (2048 * MIN_ARENA_SIZE)

#define Global_MkIntegerTerm(I) MkIntegerTerm(I)

static size_t big2arena_szW(CELL *arena_base) {
    return arena_base[2] + 4;
}

#if 0
static size_t arena2big_szW   (size_t sz) {
  return sz - 4;
}
#endif

/* pointer to top of an arena */
static inline CELL *ArenaLimit(Term arena) {
    CELL *arena_base = RepAppl(arena);
    size_t szW = big2arena_szW(arena_base);
    return arena_base + szW;
}

/* pointer to top of an arena */
static inline CELL *ArenaPt(Term arena) { return RepAppl(arena); }

static inline UInt ArenaSzW(Term arena) {
    return big2arena_szW(RepAppl(arena));
}

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

inline static GlobalEntry *GetGlobalEntry(Atom at USES_REGS)
/* get predicate entry for ap/arity; create it if neccessary. */
{
    GlobalEntry *newe;
    AtomEntry *ae = RepAtom(at);
	  WRITE_LOCK(ae->ARWLock);
    Prop p0;
#if THREADS
    if (worker_id > 0) {
      p0 = LOCAL_ThreadHandle.ge;
    } else
#endif
      {
	p0 = ae->PropsOfAE;
      }
    while (p0) {
        GlobalEntry *pe = RepGlobalProp(p0);
        if (
#if THREADS
	    worker_id > 0 ?
	    pe->AtomOfGE == ae
	    :
#endif
	    pe->KindOfPE == GlobalProperty
                ) {
	  WRITE_UNLOCK(ae->ARWLock);
            return pe;
        }
        p0 = pe->NextOfPE;
    }
    newe = (GlobalEntry *) Yap_AllocAtomSpace(sizeof(*newe));
	  newe->NextGE = LOCAL_GlobalVariables;
	  LOCAL_GlobalVariables = newe;
    newe->KindOfPE = GlobalProperty;
    newe->AtomOfGE = ae;
    RESET_VARIABLE(&newe->global);
#if THREADS
     if (worker_id > 0) {
      newe->NextOfPE   = LOCAL_ThreadHandle.ge;
      LOCAL_ThreadHandle.ge = AbsGlobalProp(newe);
    } else
#endif
	{
	  AddPropToAtom(ae, (PropEntry *) newe);
	}
    WRITE_UNLOCK(ae->ARWLock);
    return newe;
}


#endif
