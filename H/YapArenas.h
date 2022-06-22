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
    Prop p0;
    AtomEntry *ae = RepAtom(at);
    GlobalEntry *newe;

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
    newe = (GlobalEntry *) Yap_AllocAtomSpace(sizeof(*newe));
    INIT_RWLOCK(newe->GRWLock);
    newe->KindOfPE = GlobalProperty;
#if THREADS
    newe->owner_id = worker_id;
#endif
    newe->NextGE = LOCAL_GlobalVariables;
    LOCAL_GlobalVariables = newe;
    newe->AtomOfGE = ae;
    AddPropToAtom(ae, (PropEntry *) newe);
    RESET_VARIABLE(&newe->global);
    WRITE_UNLOCK(ae->ARWLock);
    return newe;
}


#endif
