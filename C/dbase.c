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
* File:		dbase.c							 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	YAP's internal data base				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
 * @file   dbase.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
 * @date   Mon Apr 30 09:36:46 2018
 * 
 * @brief  record and other forms of storing terms.
 *
 */


/** @defgroup Internal_Database Internal Data Base
 * 
 *     @ingroup builtins
 *     @{
 * 
 * Some programs need global information for, e.g. counting or collecting
 * data obtained by backtracking. As a rule, to keep this information, the
 * internal data base should be used instead of asserting and retracting
 * clauses (as most novice programmers  do), .
 * In YAP (as in some other Prolog systems) the internal data base (i.d.b.
 * for short) is faster, needs less space and provides a better insulation of
 * program and data than using asserted/retracted clauses.
 * The i.d.b. is implemented as a set of terms, accessed by keys that
 * unlikely what happens in (non-Prolog) data bases are not part of the
 * term. Under each key a list of terms is kept. References are provided so that
 * terms can be identified: each term in the i.d.b. has a unique reference
 * (references are also available for clauses of dynamic predicates).
 * 
 * There is a strong analogy between the i.d.b. and the way dynamic
 * predicates are stored. In fact, the main i.d.b. predicates might be
 * implemented using dynamic predicates:
 * 
 * ~~~~~
 * recorda(X,T,R) :- asserta(idb(X,T),R).
 * recordz(X,T,R) :- assertz(idb(X,T),R).
 * recorded(X,T,R) :- clause(idb(X,T),R).
 * ~~~~~
 * We can take advantage of this, the other way around, as it is quite
 * easy to write a simple Prolog interpreter, using the i.d.b.:
 * 
 * ~~~~~
 * asserta(G) :- recorda(interpreter,G,_).
 * assertz(G) :- recordz(interpreter,G,_).
 * retract(G) :- recorded(interpreter,G,R), !, erase(R).
 * call(V) :- var(V), !, fail.
 * call((H :- B)) :- !, recorded(interpreter,(H :- B),_), call(B).
 * call(G) :- recorded(interpreter,G,_).
 * ~~~~~
 * In YAP, much attention has been given to the implementation of the
 * i.d.b., especially to the problem of accelerating the access to terms kept in
 * a large list under the same key. Besides using the key, YAP uses an internal
 * lookup function, transparent to the user, to find only the terms that might
 * unify. For instance, in a data base containing the terms
 * 
 * ~~~~~
 * b
 * b(a)
 * c(d)
 * e(g)
 * b(X)
 * e(h)
 * ~~~~~
 * 
 * stored under the key k/1, when executing the query
 * 
 * ~~~~~
 * :- recorded(k(_),c(_),R).
 * ~~~~~
 * 
 * `recorded` would proceed directly to the third term, spending almost the
 * time as if `a(X)` or `b(X)` was being searched.
 * The lookup function uses the functor of the term, and its first three
 * arguments (when they exist). So, `recorded(k(_),e(h),_)` would go
 * directly to the last term, while `recorded(k(_),e(_),_)` would find
 * first the fourth term, and then, after backtracking, the last one.
 * 
 * This mechanism may be useful to implement a sort of hierarchy, where
 * the functors of the terms (and eventually the first arguments) work as
 * secondary keys.
 * 
 * In the YAP's i.d.b. an optimized representation is used for
 * terms without free variables. This results in a faster retrieval of terms
 * and better space usage. Whenever possible, avoid variables in terms in terms
 * stored in the  i.d.b.
 * 
 * 
 * 
 */

#include "Yap.h"
#include "attvar.h"
#include "clause.h"
#include "heapgc.h"
#include "yapio.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#include <stdlib.h>

/* There are two options to implement traditional immediate update semantics.

   - In the first option, we only remove an element of the chain when
   it is physically disposed of. This simplifies things, because
   pointers are always valid, but it complicates some stuff a bit:

   o You may have go through long lines of deleted db entries before you
   actually reach the one you want.

   o Deleted clauses are also not removed of the chain. The solution
   was to place a fail in every clause, but you still have to
   backtrack through failed clauses.

   An alternative solution is to remove clauses from the chain, even
   if they are still phisically present. Unfortunately this creates
   problems because immediate update semantics means you have to
   backtrack clauses or see the db entries stored later.

   There are several solutions. One of the simplest is to use an age
   counter. When you backtrack to a removed clause or to a deleted db
   entry you use the age to find newly entered clauses in the DB.

   This still causes a problem when you backtrack to a deleted
   clause, because clauses are supposed to point to the next
   alternative, and having been removed from the chain you cannot
   point there directly. One solution is to have a predicate in C that
   recovers the place where to go to and then gets rid of the clause.

*/

#define DISCONNECT_OLD_ENTRIES 1

#ifdef MACYAPBUG
#define Register
#else
#define Register register
#endif

/* Flags for recorda or recordz				 */
/* MkCode should be the same as CodeDBProperty */
#define MkFirst 1
#define MkCode CodeDBBit
#define MkLast 4
#define WithRef 8
#define MkIfNot 16
#define InQueue 32

#define FrstDBRef(V) ((V)->First)
#define NextDBRef(V) ((V)->Next)

#define DBLength(V) (sizeof(DBStruct) + (Int)(V) + CellSize)
#define AllocDBSpace(V) ((DBRef)Yap_AllocCodeSpace(V))
#define FreeDBSpace(V) Yap_FreeCodeSpace(V)

#if SIZEOF_INT_P == 4
#define ToSmall(V) ((link_entry)(Unsigned(V) >> 2))
#else
#define ToSmall(V) ((link_entry)(Unsigned(V) >> 3))
#endif

#ifdef SFUNC

#define MaxSFs 256

typedef struct {
  Term SName;    /* The culprit */
  CELL *SFather; /* and his father's position */
} SFKeep;
#endif

#define HashFieldMask ((CELL)0xffL)
#define DualHashFieldMask ((CELL)0xffffL)
#define TripleHashFieldMask ((CELL)0xffffffL)
#define FourHashFieldMask ((CELL)0xffffffffL)

#define ONE_FIELD_SHIFT 8
#define TWO_FIELDS_SHIFT 16
#define THREE_FIELDS_SHIFT 24

#define AtomHash(t) (Unsigned(t) >> 4)
#define FunctorHash(t) (Unsigned(t) >> 4)
#define NumberHash(t) (Unsigned(IntOfTerm(t)))

#define LARGE_IDB_LINK_TABLE 1

/* traditionally, YAP used a link table to recover IDB terms*/
#if LARGE_IDB_LINK_TABLE
typedef BITS32 link_entry;
#define SIZEOF_LINK_ENTRY 4
#else
typedef BITS16 link_entry;
#define SIZEOF_LINK_ENTRY 2
#endif

/* These global variables are necessary to build the data base
   structure */
typedef struct db_globs {
  link_entry *lr, *LinkAr;
  /* we cannot call Error directly from within recorded(). These flags are used
     to delay for a while
  */
  DBRef *tofref; /* place the refs also up	 */
#ifdef SFUNC
  CELL *FathersPlace;   /* Where the father was going when the term
                         * was reached */
  SFKeep *SFAr, *TopSF; /* Where are we putting our SFunctors */
#endif
  DBRef found_one; /* Place where we started recording */
  UInt sz;         /* total size */
} dbglobs;

#ifdef SUPPORT_HASH_TABLES
typedef struct {
  CELL key;
  DBRef entry;
} hash_db_entry;

typedef table {
  Int NOfEntries;
  Int HashArg;
  hash_db_entry *table;
}
hash_db_table;
#endif

static CELL *cpcells(CELL *, CELL *, Int);
static void linkblk(link_entry *, CELL *, CELL);
static Int cmpclls(CELL *, CELL *, Int);
static Prop FindDBProp(AtomEntry *, int, unsigned int, Term);
static CELL CalcKey(Term);
#ifdef COROUTINING
static CELL *MkDBTerm(CELL *, CELL *, CELL *, CELL *, CELL *, CELL *, int *,
                      struct db_globs *);
#else
static CELL *MkDBTerm(CELL *, CELL *, CELL *, CELL *, CELL *, int *,
                      struct db_globs *);
#endif
static DBRef CreateDBStruct(Term, DBProp, int, int *, UInt, struct db_globs *);
static DBRef record(int, Term, Term, Term CACHE_TYPE);
static DBRef check_if_cons(DBRef, Term);
static DBRef check_if_var(DBRef);
static DBRef check_if_wvars(DBRef, unsigned int, CELL *);
static int scheckcells(int, CELL *, CELL *, link_entry *, CELL);
static DBRef check_if_nvars(DBRef, unsigned int, CELL *, struct db_globs *);
static Int p_rcda(USES_REGS1);
static Int p_rcdap(USES_REGS1);
static Int p_rcdz(USES_REGS1);
static Int p_rcdzp(USES_REGS1);
static Int p_drcdap(USES_REGS1);
static Int p_drcdzp(USES_REGS1);
static Term GetDBTerm(const DBTerm *, int src CACHE_TYPE);
static DBProp FetchDBPropFromKey(Term, int, int, char *);
static Int i_recorded(DBProp, Term CACHE_TYPE);
static Int c_recorded(int CACHE_TYPE);
static Int co_rded(USES_REGS1);
static Int in_rdedp(USES_REGS1);
static Int co_rdedp(USES_REGS1);
static Int p_first_instance(USES_REGS1);
static void ErasePendingRefs(const DBTerm *CACHE_TYPE);
static void RemoveDBEntry(const DBRef CACHE_TYPE);
static void EraseLogUpdCl(LogUpdClause *);
static void MyEraseClause(DynamicClause *CACHE_TYPE);
static void PrepareToEraseClause(DynamicClause *, DBRef);
static void EraseEntry(DBRef);
static Int p_erase(USES_REGS1);
static Int p_eraseall(USES_REGS1);
static Int p_erased(USES_REGS1);
static Int p_instance(USES_REGS1);
static int NotActiveDB(DBRef);
static DBEntry *NextDBProp(PropEntry *);
static Int init_current_key(USES_REGS1);
static Int cont_current_key(USES_REGS1);
static Int cont_current_key_integer(USES_REGS1);
static Int p_rcdstatp(USES_REGS1);
static Int p_somercdedp(USES_REGS1);
static yamop *find_next_clause(DBRef USES_REGS);
static Int p_jump_to_next_dynamic_clause(USES_REGS1);
#ifdef SFUNC
static void SFVarIn(Term);
static void sf_include(SFKeep *);
#endif
static Int p_init_queue(USES_REGS1);
static Int p_enqueue(USES_REGS1);
static void keepdbrefs(const DBTerm *ref USES_REGS);
static Int p_dequeue(USES_REGS1);
static void ErDBE(DBRef CACHE_TYPE);
static void ReleaseTermFromDB(const DBTerm *ref USES_REGS);
static PredEntry *new_lu_entry(Term);
static PredEntry *new_lu_int_key(Int);
static PredEntry *find_lu_entry(Term);
static DBProp find_int_key(Int);

#define db_check_trail(x)                                                      \
  {                                                                            \
    if (Unsigned(dbg->tofref) == Unsigned(x)) {                                \
      goto error_tr_overflow;                                                  \
    }                                                                          \
  }

static UInt new_trail_size(void) {
  CACHE_REGS
  UInt sz = (LOCAL_TrailTop - (ADDR)TR) / 2;
  if (sz < K64)
    return K64;
  if (sz > M1)
    return M1;
  return sz;
}

static int recover_from_record_error(int nargs) {
  CACHE_REGS
  switch (LOCAL_Error_TYPE) {
  case RESOURCE_ERROR_STACK:
    if (!Yap_gcl(LOCAL_Error_Size, nargs, ENV, gc_P(P, CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    }
    goto recover_record;
  case RESOURCE_ERROR_TRAIL:
    if (!Yap_growtrail(new_trail_size(), FALSE)) {
      Yap_Error(RESOURCE_ERROR_TRAIL, TermNil,
                "YAP could not grow trail in recorda/3");
      return FALSE;
    }
    goto recover_record;
  case RESOURCE_ERROR_HEAP:
    if (!Yap_growheap(FALSE, LOCAL_Error_Size, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    }
    goto recover_record;
  case RESOURCE_ERROR_AUXILIARY_STACK:
    if (!Yap_ExpandPreAllocCodeSpace(LOCAL_Error_Size, NULL, TRUE)) {
      Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    }
    goto recover_record;
  default:
    Yap_Error(LOCAL_Error_TYPE, TermNil, LOCAL_ErrorMessage);
    return FALSE;
  }
recover_record:
  LOCAL_Error_Size = 0;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  return TRUE;
}

#ifdef SUPPORT_HASH_TABLES
/* related property and hint on number of entries */
static void create_hash_table(DBProp p, Int hint) {
  int off = sizeof(CELL) * 4, out;
  Int size;

  if (hint < p->NOfEntries)
    hint = p->NOfEntries;
  while (off) {
    Int limit = ((CELL)1) << (off);
    if (inp >= limit) {
      out += off;
      inp >>= off;
    }
    off >>= 1;
  }
  if ((size = ((CELL)1) << out) < hint)
    hint <<= 1;
  /* clean up the table */
  pt = tbl = (hash_db_entry *)AllocDBSpace(hint * sizeof(hash_db_entry));
  Yap_LUClauseSpace += hint * sizeof(hash_db_entry);
  for (i = 0; i < hint; i++) {
    pt->key = NULL;
    pt++;
  }
  /* next insert the entries */
}

static void insert_in_table() {}

static void remove_from_table() {}
#endif

inline static CELL *cpcells(CELL *to, CELL *from, Int n) {
#if HAVE_MEMMOVE
  memmove((void *)to, (void *)from, (size_t)(n * sizeof(CELL)));
  return (to + n);
#else
  while (n-- >= 0) {
    *to++ = *from++;
  }
  return (to);
#endif
}

static void linkblk(link_entry *r, CELL *c, CELL offs) {
  CELL p;
  while ((p = (CELL)*r) != 0) {
    Term t = c[p];
    r++;
    c[p] = AdjustIDBPtr(t, offs);
  }
}

static Int cmpclls(CELL *a, CELL *b, Int n) {
  while (n-- > 0) {
    if (*a++ != *b++)
      return FALSE;
  }
  return TRUE;
}

/* get DB entry for ap/arity; */
static Prop FindDBPropHavingLock(AtomEntry *ae, int CodeDB, unsigned int arity,
                                 Term dbmod) {
  Prop p0;
  DBProp p;

  p = RepDBProp(p0 = ae->PropsOfAE);
  while (p0 &&
         (((p->KindOfPE & ~0x1) != (CodeDB | DBProperty)) ||
          (p->ArityOfDB != arity) ||
          ((CodeDB & MkCode) && p->ModuleOfDB && p->ModuleOfDB != dbmod))) {
    p = RepDBProp(p0 = p->NextOfPE);
  }
  return p0;
}

/* get DB entry for ap/arity; */
static Prop FindDBProp(AtomEntry *ae, int CodeDB, unsigned int arity,
                       Term dbmod) {
  Prop out;

  READ_LOCK(ae->ARWLock);
  out = FindDBPropHavingLock(ae, CodeDB, arity, dbmod);
  READ_UNLOCK(ae->ARWLock);
  return (out);
}

/* These two functions allow us a fast lookup method in the data base */
/* PutMasks builds the mask and hash for a single argument	 */
inline static CELL CalcKey(Term tw) {
  /* The first argument is known to be instantiated */
  if (IsApplTerm(tw)) {
    Functor f = FunctorOfTerm(tw);
    if (IsExtensionFunctor(f)) {
      if (f == FunctorDBRef) {
        return (FunctorHash(tw)); /* Ref */
      } /* if (f == FunctorLongInt || f == FunctorDouble) */
      return (NumberHash(RepAppl(tw)[1]));
    }
    return (FunctorHash(f));
  } else if (IsAtomOrIntTerm(tw)) {
    if (IsAtomTerm(tw)) {
      return (AtomHash(tw));
    }
    return (NumberHash(tw));
  }
  return (FunctorHash(FunctorList));
}

/* EvalMasks builds the mask and hash for up to three arguments of a term */
static CELL EvalMasks(register Term tm, CELL *keyp) {

  if (IsVarTerm(tm)) {
    *keyp = 0L;
    return (0L);
  } else if (IsApplTerm(tm)) {
    Functor fun = FunctorOfTerm(tm);

    if (IsExtensionFunctor(fun)) {
      if (fun == FunctorDBRef) {
        *keyp = FunctorHash(tm); /* Ref */
      } else /* if (f == FunctorLongInt || f == FunctorDouble) */ {
        *keyp = NumberHash(RepAppl(tm)[1]);
      }
      return (FourHashFieldMask);
    } else {
      unsigned int arity;

      arity = ArityOfFunctor(fun);
#ifdef SFUNC
      if (arity == SFArity) { /* do not even try to calculate masks */
        *keyp = key;
        return (FourHashFieldMask);
      }
#endif
      switch (arity) {
      case 1: {
        Term tw = ArgOfTerm(1, tm);

        if (IsNonVarTerm(tw)) {
          *keyp = (FunctorHash(fun) & DualHashFieldMask) |
                  (CalcKey(tw) << TWO_FIELDS_SHIFT);
          return (FourHashFieldMask);
        } else {
          *keyp = (FunctorHash(fun) & DualHashFieldMask);
          return (DualHashFieldMask);
        }
      }
      case 2: {
        Term tw1, tw2;
        CELL key, mask;

        key = FunctorHash(fun) & DualHashFieldMask;
        mask = DualHashFieldMask;

        tw1 = ArgOfTerm(1, tm);
        if (IsNonVarTerm(tw1)) {
          key |= ((CalcKey(tw1) & HashFieldMask) << TWO_FIELDS_SHIFT);
          mask |= (HashFieldMask << TWO_FIELDS_SHIFT);
        }
        tw2 = ArgOfTerm(2, tm);
        if (IsNonVarTerm(tw2)) {
          *keyp = key | (CalcKey(tw2) << THREE_FIELDS_SHIFT);
          return (mask | (HashFieldMask << THREE_FIELDS_SHIFT));
        } else {
          *keyp = key;
          return (mask);
        }
      }
      default: {
        Term tw1, tw2, tw3;
        CELL key, mask;

        key = FunctorHash(fun) & HashFieldMask;
        mask = HashFieldMask;

        tw1 = ArgOfTerm(1, tm);
        if (IsNonVarTerm(tw1)) {
          key |= (CalcKey(tw1) & HashFieldMask) << ONE_FIELD_SHIFT;
          mask |= HashFieldMask << ONE_FIELD_SHIFT;
        }
        tw2 = ArgOfTerm(2, tm);
        if (IsNonVarTerm(tw2)) {
          key |= (CalcKey(tw2) & HashFieldMask) << TWO_FIELDS_SHIFT;
          mask |= HashFieldMask << TWO_FIELDS_SHIFT;
        }
        tw3 = ArgOfTerm(3, tm);
        if (IsNonVarTerm(tw3)) {
          *keyp = key | (CalcKey(tw3) << THREE_FIELDS_SHIFT);
          return (mask | (HashFieldMask << THREE_FIELDS_SHIFT));
        } else {
          *keyp = key;
          return (mask);
        }
      }
      }
    }
  } else {
    CELL key = (FunctorHash(FunctorList) & DualHashFieldMask);
    CELL mask = DualHashFieldMask;
    Term th = HeadOfTerm(tm), tt;

    if (IsNonVarTerm(th)) {
      mask |= (HashFieldMask << TWO_FIELDS_SHIFT);
      key |= (CalcKey(th) << TWO_FIELDS_SHIFT);
    }
    tt = TailOfTerm(tm);
    if (IsNonVarTerm(tt)) {
      *keyp = key | (CalcKey(tt) << THREE_FIELDS_SHIFT);
      return (mask | (HashFieldMask << THREE_FIELDS_SHIFT));
    }
    *keyp = key;
    return (mask);
  }
}

CELL Yap_EvalMasks(register Term tm, CELL *keyp) { return EvalMasks(tm, keyp); }

/* Called to inform that a new pointer to a data base entry has been added */
#define MarkThisRef(Ref) ((Ref)->NOfRefsTo++)

/* From a term, builds its representation in the data base */

/* otherwise, we just need to restore variables*/
typedef struct { CELL *addr; } visitel;
#define DB_UNWIND_CUNIF()                                                      \
  while (visited < (visitel *)AuxSp) {                                         \
    RESET_VARIABLE(visited->addr);                                             \
    visited++;                                                                 \
  }

/* no checking for overflow while building DB terms yet */
#define CheckDBOverflow(X)                                                     \
  if (CodeMax + X >= (CELL *)visited - 1024) {                                 \
    goto error;                                                                \
  }

/* no checking for overflow while building DB terms yet */
#define CheckVisitOverflow()                                                   \
  if ((CELL *)tovisit + 1024 >= ASP) {                                        \
    goto error2;                                                               \
  }

static CELL *copy_long_int(CELL *st, CELL *pt) {
  /* first thing, store a link to the list before we move on */
  st[0] = (CELL)FunctorLongInt;
  st[1] = pt[1];
  st[2] = EndSpecials;
  /* now reserve space */
  return st + 3;
}

static CELL *copy_double(CELL *st, CELL *pt) {
  /* first thing, store a link to the list before we move on */
  st[0] = (CELL)FunctorDouble;
  st[1] = pt[1];
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
  st[2] = pt[2];
  st[3] = EndSpecials;
#else
  st[2] = EndSpecials;
#endif
  /* now reserve space */
  return st + (2 + SIZEOF_DOUBLE / SIZEOF_INT_P);
}

static CELL *copy_string(CELL *st, CELL *pt) {
  UInt sz = pt[1] + 3;
  /* first thing, store a link to the list before we move on */
  memmove(st, pt, sizeof(CELL) * sz);
  /* now reserve space */
  return st + sz;
}

#ifdef USE_GMP
static CELL *copy_big_int(CELL *st, CELL *pt) {
  Int sz =
      sizeof(MP_INT) + (((MP_INT *)(pt + 2))->_mp_alloc * sizeof(mp_limb_t));

  /* first functor */
  st[0] = (CELL)FunctorBigInt;
  st[1] = pt[1];
  /* then the actual number */
  memmove((void *)(st + 2), (void *)(pt + 2), sz);
  st = st + 2 + sz / CellSize;
  /* then the tail for gc */
  st[0] = EndSpecials;
  return st + 1;
}
#endif /* BIG_INT */

#define DB_MARKED(d0) ((CELL *)(d0) < CodeMax && (CELL *)(d0) >= tbase)

/* This routine creates a complex term in the heap. */
static CELL *MkDBTerm(register CELL *pt0, register CELL *pt0_end,
                      register CELL *StoPoint, CELL *CodeMax, CELL *tbase,
#ifdef COROUTINING
                      CELL *attachmentsp,
#endif
                      int *vars_foundp, struct db_globs *dbg) {
  CACHE_REGS
#if THREADS
#undef Yap_REGS
  register REGSTORE *regp = Yap_regp;
#define Yap_REGS (*regp)
#endif
  register visitel *visited = (visitel *)AuxSp;
  /* store this in H */
  register CELL **tovisit = (CELL **)HR;
  CELL **tovisit_base = tovisit;
  /* where we are going to add a new pair */
  int vars_found = 0;
#ifdef COROUTINING
  Term ConstraintsTerm = TermNil;
  CELL *origH = HR;
#endif
  CELL *CodeMaxBase = CodeMax;

loop:
  while (pt0 <= pt0_end) {

    CELL *ptd0 = pt0;
    CELL d0 = *ptd0;
  restart:
    if (IsVarTerm(d0))
      goto deref_var;

    if (IsApplTerm(d0)) {
      register Functor f;
      register CELL *ap2;

      /* we will need to link afterwards */
      ap2 = RepAppl(d0);
#ifdef RATIONAL_TREES
      if (ap2 >= tbase && ap2 <= StoPoint) {
        db_check_trail(dbg->lr + 1);
        *dbg->lr++ = ToSmall((CELL)(StoPoint) - (CELL)(tbase));
        *StoPoint++ = d0;
        ++pt0;
        continue;
      }
#endif
      db_check_trail(dbg->lr + 1);
      *dbg->lr++ = ToSmall((CELL)(StoPoint) - (CELL)(tbase));
      f = (Functor)(*ap2);
      if (IsExtensionFunctor(f)) {
        switch ((CELL)f) {
        case (CELL)FunctorDBRef: {
          DBRef dbentry;

          dbentry = DBRefOfTerm(d0);
          *StoPoint++ = d0;
          dbg->lr--;
          if (dbentry->Flags & LogUpdMask) {
            LogUpdClause *cl = (LogUpdClause *)dbentry;
/* store now the correct entry */
#if DEBUG
            if (GLOBAL_Option['i' - 'a' + 1]) {
              Yap_DebugPlWriteln(d0);
              fprintf(stderr, "+%p@%p %s\n", cl, cl->ClPred,
                      IndicatorOfPred(cl->ClPred));
            }
#endif
            cl->ClRefCount++;
          } else {
            dbentry->NOfRefsTo++;
          }
          *--dbg->tofref = dbentry;
          db_check_trail(dbg->lr);
          /* just continue the loop */
          ++pt0;
          continue;
        }
        case (CELL)FunctorLongInt:
          CheckDBOverflow(3);
          *StoPoint++ = AbsAppl(CodeMax);
          CodeMax = copy_long_int(CodeMax, ap2);
          ++pt0;
          continue;
#ifdef USE_GMP
        case (CELL)FunctorBigInt:
          CheckDBOverflow(3 + Yap_SizeOfBigInt(d0));
          /* first thing, store a link to the list before we move on */
          *StoPoint++ = AbsAppl(CodeMax);
          CodeMax = copy_big_int(CodeMax, ap2);
          ++pt0;
          continue;
#endif
        case (CELL)FunctorString: {
          CELL *st = CodeMax;

          CheckDBOverflow(3 + ap2[1]);
          /* first thing, store a link to the list before we move on */
          *StoPoint++ = AbsAppl(st);
          CodeMax = copy_string(CodeMax, ap2);
          ++pt0;
          continue;
        }
        case (CELL)FunctorDouble: {
          CELL *st = CodeMax;

          CheckDBOverflow(4);
          /* first thing, store a link to the list before we move on */
          *StoPoint++ = AbsAppl(st);
          CodeMax = copy_double(CodeMax, ap2);
          ++pt0;
          continue;
        }
        }
      }
      /* first thing, store a link to the list before we move on */
      *StoPoint++ = AbsAppl(CodeMax);
      /* next, postpone analysis to the rest of the current list */
      CheckVisitOverflow();
#ifdef RATIONAL_TREES
      tovisit[0] = pt0 + 1;
      tovisit[1] = pt0_end;
      tovisit[2] = StoPoint;
      tovisit[3] = (CELL *)*pt0;
      tovisit += 4;
      *pt0 = StoPoint[-1];
#else
      if (pt0 < pt0_end) {
        tovisit[0] = pt0 + 1;
        tovisit[1] = pt0_end;
        tovisit[2] = StoPoint;
        tovisit += 3;
      }
#endif
      d0 = ArityOfFunctor(f);
      pt0 = ap2 + 1;
      pt0_end = ap2 + d0;
      CheckDBOverflow(d0 + 1);
      /* prepare for our new compound term */
      /* first the functor */
      *CodeMax++ = (CELL)f;
      /* we'll be working here */
      StoPoint = CodeMax;
      /* now reserve space */
      CodeMax += d0;
      continue;
    } else if (IsPairTerm(d0)) {
      /* we will need to link afterwards */
      CELL *ap2 = RepPair(d0);
      if (ap2 >= tbase && ap2 <= StoPoint) {
        db_check_trail(dbg->lr + 1);
        *dbg->lr++ = ToSmall((CELL)(StoPoint) - (CELL)(tbase));
        *StoPoint++ = d0;
        ++pt0;
        continue;
      }

      if (IsAtomOrIntTerm(Deref(ap2[0])) && IsPairTerm(Deref(ap2[1]))) {
        /* shortcut for [1,2,3,4,5] */
        Term tt = Deref(ap2[1]);
        Term th = Deref(ap2[0]);
        Int direction = RepPair(tt) - ap2;
        CELL *OldStoPoint;
        CELL *lp;

        if (direction < 0)
          direction = -1;
        else
          direction = 1;
        db_check_trail(dbg->lr + 1);
        *dbg->lr++ = ToSmall((CELL)(StoPoint) - (CELL)(tbase));
        *StoPoint++ = AbsPair(CodeMax);
        OldStoPoint = StoPoint;
        do {
          lp = RepPair(tt);

          if (lp >= tbase && lp <= StoPoint) {
            break;
          }
          CheckDBOverflow(2);
          CodeMax[0] = th;
          db_check_trail(dbg->lr + 1);
          *dbg->lr++ = ToSmall((CELL)(CodeMax + 1) - (CELL)(tbase));
          CodeMax[1] = AbsPair(CodeMax + 2);
          CodeMax += 2;
          th = Deref(lp[0]);
          tt = Deref(lp[1]);
        } while (IsAtomOrIntTerm(th) && IsPairTerm(tt) &&
                 /* have same direction to avoid infinite terms X = [a|X] */
                 (RepPair(tt) - lp) * direction > 0);
        if (lp >= tbase && lp <= StoPoint) {
          CodeMax[-1] = tt;
          break;
        }
        if (IsAtomOrIntTerm(th) && IsAtomOrIntTerm(tt)) {
          CheckDBOverflow(2);
          CodeMax[0] = th;
          CodeMax[1] = tt;
          CodeMax += 2;
          ++pt0;
          continue;
        }
        d0 = AbsPair(lp);
        StoPoint = OldStoPoint;
      } else {
        db_check_trail(dbg->lr + 1);
        *dbg->lr++ = ToSmall((CELL)(StoPoint) - (CELL)(tbase));
        *StoPoint++ = AbsPair(CodeMax);
      }
/* next, postpone analysis to the rest of the current list */
#ifdef RATIONAL_TREES
      tovisit[0] = pt0 + 1;
      tovisit[1] = pt0_end;
      tovisit[2] = StoPoint;
      tovisit[3] = (CELL *)*pt0;
      tovisit += 4;
      *pt0 = StoPoint[-1];
#else
      if (pt0 < pt0_end) {
        tovisit[0] = pt0 + 1;
        tovisit[1] = pt0_end;
        tovisit[2] = StoPoint;
        tovisit += 3;
      }
#endif
      CheckVisitOverflow();
      /* new list */
      /* we are working at CodeMax */
      StoPoint = CodeMax;
      /* set ptr to new term being analysed */
      pt0 = RepPair(d0);
      pt0_end = RepPair(d0) + 1;
      /* reserve space for our new list */
      CodeMax += 2;
      CheckDBOverflow(2);
      continue;
    } else if (IsAtomOrIntTerm(d0)) {
      *StoPoint++ = d0;
      ++pt0;
      continue;
    }

  /* the code to dereference a  variable */
  deref_var:
    if (!DB_MARKED(d0)) {
      if (
#if YAPOR_SBA
          d0 != 0
#else
          d0 != (CELL)ptd0
#endif
          ) {
        ptd0 = (Term *)d0;
        d0 = *ptd0;
        goto restart; /* continue dereferencing */
      }
      /* else just drop to found_var */
    }
    /* else just drop to found_var */
    {
      CELL displacement = (CELL)(StoPoint) - (CELL)(tbase);

      pt0++;
      /* first time we found this variable! */
      if (!DB_MARKED(d0)) {

        /* store previous value */
        visited--;
        visited->addr = ptd0;
        CheckDBOverflow(1);
        /* variables need to be offset at read time */
        *ptd0 = (CELL)StoPoint;
#if YAPOR_SBA
        /* the copy we keep will be an empty variable   */
        *StoPoint++ = 0;
#else
        /* the copy we keep will be the current displacement   */
        *StoPoint = (CELL)StoPoint;
        StoPoint++;
        db_check_trail(dbg->lr + 1);
        *dbg->lr++ = ToSmall(displacement);
#endif
        /* indicate we found variables */
        vars_found++;
#ifdef COROUTINING
        if (SafeIsAttachedTerm((CELL)ptd0)) {
          Term t[4];
          int sz = tovisit - tovisit_base;

          HR = (CELL *)tovisit;
          /* store the constraint away for: we need a back pointer to
             the variable, the constraint in some cannonical form, what type
             of constraint, and a list pointer */
          t[0] = (CELL)ptd0;
          t[1] = GLOBAL_attas[ExtFromCell(ptd0)].to_term_op(ptd0);
          t[2] = MkIntegerTerm(ExtFromCell(ptd0));
          t[3] = ConstraintsTerm;
          ConstraintsTerm = Yap_MkApplTerm(FunctorClist, 4, t);
          if (HR + sz >= ASP) {
            goto error2;
          }
          memmove((void *)HR, (void *)(tovisit_base), sz * sizeof(CELL *));
          tovisit_base = (CELL **)HR;
          tovisit = tovisit_base + sz;
        }
#endif
        continue;
      } else {
        /* references need to be offset at read time */
        db_check_trail(dbg->lr + 1);
        *dbg->lr++ = ToSmall(displacement);
        /* store the offset */
        *StoPoint = d0;
        StoPoint++;
        continue;
      }
    }
  }

  /* Do we still have compound terms to visit */
  if (tovisit > tovisit_base) {
#ifdef RATIONAL_TREES
    tovisit -= 4;
    pt0 = tovisit[0];
    pt0_end = tovisit[1];
    StoPoint = tovisit[2];
    pt0[-1] = (CELL)tovisit[3];
#else
    tovisit -= 3;
    pt0 = tovisit[0];
    pt0_end = tovisit[1];
    CheckDBOverflow(1);
    StoPoint = tovisit[2];
#endif
    goto loop;
  }

#ifdef COROUTINING
  /* we still may have constraints to do */
  if (ConstraintsTerm != TermNil &&
      !IN_BETWEEN(tbase, RepAppl(ConstraintsTerm), CodeMax)) {
    *attachmentsp = (CELL)(CodeMax + 1);
    pt0 = RepAppl(ConstraintsTerm) + 1;
    pt0_end = RepAppl(ConstraintsTerm) + 4;
    StoPoint = CodeMax;
    *StoPoint++ = RepAppl(ConstraintsTerm)[0];
    ConstraintsTerm = AbsAppl(CodeMax);
    CheckDBOverflow(1);
    CodeMax += 5;
    goto loop;
  }
#endif
  /* we're done */
  *vars_foundp = vars_found;
  DB_UNWIND_CUNIF();
#ifdef COROUTINING
  HR = origH;
#endif
  return CodeMax;

error:
  LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
  LOCAL_Error_Size = 1024 + ((char *)AuxSp - (char *)CodeMaxBase);
  *vars_foundp = vars_found;
#ifdef RATIONAL_TREES
  while (tovisit > tovisit_base) {
    tovisit -= 4;
    pt0 = tovisit[0];
    pt0_end = tovisit[1];
    StoPoint = tovisit[2];
    pt0[-1] = (CELL)tovisit[3];
  }
#endif
  DB_UNWIND_CUNIF();
#ifdef COROUTINING
  HR = origH;
#endif
  return NULL;

error2:
  LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
  *vars_foundp = vars_found;
#ifdef RATIONAL_TREES
  while (tovisit > tovisit_base) {
    tovisit -= 4;
    pt0 = tovisit[0];
    pt0_end = tovisit[1];
    StoPoint = tovisit[2];
    pt0[-1] = (CELL)tovisit[3];
  }
#endif
  DB_UNWIND_CUNIF();
#ifdef COROUTINING
  HR = origH;
#endif
  return NULL;

error_tr_overflow:
  LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
  *vars_foundp = vars_found;
#ifdef RATIONAL_TREES
  while (tovisit > tovisit_base) {
    tovisit -= 4;
    pt0 = tovisit[0];
    pt0_end = tovisit[1];
    StoPoint = tovisit[2];
    pt0[-1] = (CELL)tovisit[3];
  }
#endif
  DB_UNWIND_CUNIF();
#ifdef COROUTINING
  HR = origH;
#endif
  return NULL;
#if THREADS
#undef Yap_REGS
#define Yap_REGS (*Yap_regp)
#endif /* THREADS */
}

#ifdef SFUNC
/*
 * The sparse terms existing in the structure are to be included now. This
 * means simple copy for constant terms but, some care about variables If
 * they have appeared before, we will know by their position number
 */
static void sf_include(SFKeep *sfp, struct db_globs *dbg) SFKeep *sfp;
{
  Term Tm = sfp->SName;
  CELL *tp = ArgsOfSFTerm(Tm);
  Register Term *StoPoint = ntp;
  CELL *displacement = CodeAbs;
  CELL arg_no;
  Term tvalue;
  int j = 3;

  if (sfp->SFather != NIL)
    *(sfp->SFather) = AbsAppl(displacement);
  *StoPoint++ = FunctorOfTerm(Tm);
  db_check_trail(dbg->lr + 1);
  *dbg->lr++ = ToSmall(displacement + 1);
  *StoPoint++ = (Term)(displacement + 1);
  while (*tp) {
    arg_no = *tp++;
    tvalue = Derefa(tp++);
    if (IsVarTerm(tvalue)) {
      if (((VarKeep *)tvalue)->NOfVars != 0) {
        *StoPoint++ = arg_no;
        db_check_trail(dbg->lr + 1);
        *dbg->lr++ = ToSmall(displacement + j);
        if (((VarKeep *)tvalue)->New == 0)
          *StoPoint++ = ((VarKeep *)tvalue)->New = Unsigned(displacement + j);
        else
          *StoPoint++ = ((VarKeep *)tvalue)->New;
        j += 2;
      }
    } else if (IsAtomicTerm(tvalue)) {
      *StoPoint++ = arg_no;
      *StoPoint++ = tvalue;
      j += 2;
    } else {
      LOCAL_Error_TYPE = TYPE_ERROR_DBTERM;
      LOCAL_ErrorMessage = "wrong term in SF";
      return (NULL);
    }
  }
  *StoPoint++ = 0;
  ntp = StoPoint;
  CodeAbs = displacement + j;
}
#endif

/*
 * This function is used to check if one of the terms in the idb is the
 * constant to_compare
 */
inline static DBRef check_if_cons(DBRef p, Term to_compare) {
  while (p != NIL &&
         (p->Flags & (DBCode | ErasedMask | DBVar | DBNoVars | DBComplex) ||
          p->DBT.Entry != Unsigned(to_compare)))
    p = NextDBRef(p);
  return p;
}

/*
 * This function is used to check if one of the terms in the idb is a prolog
 * variable
 */
static DBRef check_if_var(DBRef p) {
  while (p != NIL &&
         p->Flags & (DBCode | ErasedMask | DBAtomic | DBNoVars | DBComplex))
    p = NextDBRef(p);
  return p;
}

/*
 * This function is used to check if a Prolog complex term with variables
 * already exists in the idb for that key. The comparison is alike ==, but
 * only the relative binding of variables, not their position is used. The
 * comparison is done using the function cmpclls only. The function could
 * only fail if a functor was matched to a Prolog term, but then, it should
 * have failed before because the structure of term would have been very
 * different
 */
static DBRef check_if_wvars(DBRef p, unsigned int NOfCells, CELL *BTptr) {
  CELL *memptr;

  do {
    while (p != NIL &&
           p->Flags & (DBCode | ErasedMask | DBAtomic | DBNoVars | DBVar))
      p = NextDBRef(p);
    if (p == NIL)
      return p;
    memptr = CellPtr(&(p->DBT.Contents));
    if (NOfCells == p->DBT.NOfCells && cmpclls(memptr, BTptr, NOfCells))
      return p;
    else
      p = NextDBRef(p);
  } while (TRUE);
  return NIL;
}

static int scheckcells(int NOfCells, register CELL *m1, register CELL *m2,
                       link_entry *lp, register CELL bp) {
  CELL base = Unsigned(m1);
  link_entry *lp1;

  while (NOfCells-- > 0) {
    Register CELL r1, r2;

    r1 = *m1++;
    r2 = *m2++;
    if (r1 == r2)
      continue;
    else if (r2 + bp == r1) {
      /* link pointers may not have been generated in the */
      /* same order */
      /* make sure r1 is really an offset. */
      lp1 = lp;
      r1 = m1 - (CELL *)base;
      while (*lp1 != r1 && *lp1)
        lp1++;
      if (!(*lp1))
        return FALSE;
      /* keep the old link pointer for future search. */
      /* vsc: this looks like a bug!!!! */
      /* *lp1 = *lp++; */
    } else {
      return FALSE;
    }
  }
  return TRUE;
}

/*
 * the cousin of the previous, but with things a bit more sophisticated.
 * mtchcells, if an error was an found, needs to test ........
 */
static DBRef check_if_nvars(DBRef p, unsigned int NOfCells, CELL *BTptr,
                            struct db_globs *dbg) {
  CELL *memptr;

  do {
    while (p != NIL &&
           p->Flags & (DBCode | ErasedMask | DBAtomic | DBComplex | DBVar))
      p = NextDBRef(p);
    if (p == NIL)
      return p;
    memptr = CellPtr(p->DBT.Contents);
    if (scheckcells(NOfCells, memptr, BTptr, dbg->LinkAr,
                    Unsigned(p->DBT.Contents - 1)))
      return p;
    else
      p = NextDBRef(p);
  } while (TRUE);
  return NIL;
}

static DBRef generate_dberror_msg(int errnumb, UInt sz, char *msg) {
  CACHE_REGS
  LOCAL_Error_Size = sz;
  LOCAL_Error_TYPE = errnumb;
  LOCAL_ErrorMessage = msg;
  return NULL;
}

static DBRef CreateDBWithDBRef(Term Tm, DBProp p, struct db_globs *dbg) {
  DBRef pp, dbr = DBRefOfTerm(Tm);
  DBTerm *ppt;

  if (p == NULL) {
    UInt sz = sizeof(DBTerm) + 2 * sizeof(CELL);
    ppt = (DBTerm *)AllocDBSpace(sz);
    if (ppt == NULL) {
      return generate_dberror_msg(RESOURCE_ERROR_HEAP, TermNil,
                                  "could not allocate heap");
    }
    dbg->sz = sz;
    Yap_LUClauseSpace += sz;
    pp = (DBRef)ppt;
  } else {
    UInt sz = DBLength(2 * sizeof(DBRef));
    pp = AllocDBSpace(sz);
    if (pp == NULL) {
      return generate_dberror_msg(RESOURCE_ERROR_HEAP, 0,
                                  "could not allocate space");
    }
    Yap_LUClauseSpace += sz;
    dbg->sz = sz;
    pp->id = FunctorDBRef;
    pp->Flags = DBNoVars | DBComplex | DBWithRefs;
    INIT_LOCK(pp->lock);
    INIT_DBREF_COUNT(pp);
    ppt = &(pp->DBT);
  }
  if (dbr->Flags & LogUpdMask) {
    LogUpdClause *cl = (LogUpdClause *)dbr;
    cl->ClRefCount++;
  } else {
    dbr->NOfRefsTo++;
  }
  ppt->Entry = Tm;
  ppt->NOfCells = 0;
  ppt->Contents[0] = (CELL)NULL;
  ppt->Contents[1] = (CELL)dbr;
  ppt->DBRefs = (DBRef *)(ppt->Contents + 2);
#ifdef COROUTINING
  ppt->ag.attachments = 0L;
#endif
  return pp;
}

static DBTerm *CreateDBTermForAtom(Term Tm, UInt extra_size,
                                   struct db_globs *dbg) {
  DBTerm *ppt;
  ADDR ptr;
  UInt sz = extra_size + sizeof(DBTerm);

  ptr = (ADDR)AllocDBSpace(sz);
  if (ptr == NULL) {
    return (DBTerm *)generate_dberror_msg(RESOURCE_ERROR_HEAP, 0,
                                          "could not allocate space");
  }
  Yap_LUClauseSpace += sz;
  dbg->sz = sz;
  ppt = (DBTerm *)(ptr + extra_size);
  ppt->NOfCells = 0;
  ppt->DBRefs = NULL;
#ifdef COROUTINING
  ppt->ag.attachments = 0;
#endif
  ppt->DBRefs = NULL;
  ppt->Entry = Tm;
  return ppt;
}

static DBTerm *CreateDBTermForVar(UInt extra_size, struct db_globs *dbg) {
  DBTerm *ppt;
  ADDR ptr;
  UInt sz = extra_size + sizeof(DBTerm);

  ptr = (ADDR)AllocDBSpace(sz);
  if (ptr == NULL) {
    return (DBTerm *)generate_dberror_msg(RESOURCE_ERROR_HEAP, 0,
                                          "could not allocate space");
  }
  Yap_LUClauseSpace += sz;
  dbg->sz = sz;
  ppt = (DBTerm *)(ptr + extra_size);
  ppt->NOfCells = 0;
  ppt->DBRefs = NULL;
#ifdef COROUTINING
  ppt->ag.attachments = 0;
#endif
  ppt->DBRefs = NULL;
  ppt->Entry = (CELL)(&(ppt->Entry));
  return ppt;
}

static DBRef CreateDBRefForAtom(Term Tm, DBProp p, int InFlag,
                                struct db_globs *dbg) {
  Register DBRef pp;
  SMALLUNSGN flag;
  UInt sz = DBLength(NIL);

  flag = DBAtomic;
  if (InFlag & MkIfNot && (dbg->found_one = check_if_cons(p->First, Tm)))
    return dbg->found_one;
  pp = AllocDBSpace(sz);
  if (pp == NIL) {
    return generate_dberror_msg(RESOURCE_ERROR_HEAP, 0,
                                "could not allocate space");
  }
  Yap_LUClauseSpace += sz;
  dbg->sz = sz;
  pp->id = FunctorDBRef;
  INIT_LOCK(pp->lock);
  INIT_DBREF_COUNT(pp);
  pp->Flags = flag;
  pp->Code = NULL;
  pp->DBT.Entry = Tm;
  pp->DBT.DBRefs = NULL;
  pp->DBT.NOfCells = 0;
#ifdef COROUTINING
  pp->DBT.ag.attachments = 0;
#endif
  return (pp);
}

static DBRef CreateDBRefForVar(Term Tm, DBProp p, int InFlag,
                               struct db_globs *dbg) {
  Register DBRef pp;
  UInt sz = DBLength(NULL);

  if (InFlag & MkIfNot && (dbg->found_one = check_if_var(p->First)))
    return dbg->found_one;
  pp = AllocDBSpace(sz);
  if (pp == NULL) {
    return generate_dberror_msg(RESOURCE_ERROR_HEAP, 0,
                                "could not allocate space");
  }
  Yap_LUClauseSpace += sz;
  dbg->sz = sz;
  pp->id = FunctorDBRef;
  pp->Flags = DBVar;
  pp->DBT.Entry = (CELL)Tm;
  pp->Code = NULL;
  pp->DBT.NOfCells = 0;
  pp->DBT.DBRefs = NULL;
#ifdef COROUTINING
  pp->DBT.ag.attachments = 0;
#endif
  INIT_LOCK(pp->lock);
  INIT_DBREF_COUNT(pp);
  return pp;
}

static DBRef CreateDBStruct(Term Tm, DBProp p, int InFlag, int *pstat,
                            UInt extra_size, struct db_globs *dbg) {
  CACHE_REGS
  Register Term tt, *nar = NIL;
  SMALLUNSGN flag;
  int NOfLinks = 0;
  /* place DBRefs in ConsultStack */
  DBRef *TmpRefBase;
  CELL *CodeAbs; /* how much code did we find	 */
  int vars_found = FALSE;
  yap_error_number oerr = LOCAL_Error_TYPE;

 retry_record:
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  TmpRefBase = (DBRef *)LOCAL_TrailTop;
  if (p == NULL) {
    if (IsVarTerm(Tm)) {
#ifdef COROUTINING
      if (!SafeIsAttachedTerm(Tm)) {
#endif
        DBRef out = (DBRef)CreateDBTermForVar(extra_size, dbg);
        *pstat = TRUE;
        LOCAL_Error_TYPE = oerr;
        return out;
#ifdef COROUTINING
      }
#endif
    } else if (IsAtomOrIntTerm(Tm)) {
      DBRef out = (DBRef)CreateDBTermForAtom(Tm, extra_size, dbg);
      *pstat = FALSE;
      LOCAL_Error_TYPE = oerr;
      return out;
    }
  } else {
    if (IsVarTerm(Tm)
#ifdef COROUTINING
        && !SafeIsAttachedTerm(Tm)
#endif
            ) {
      *pstat = TRUE;
      LOCAL_Error_TYPE = oerr;
      return CreateDBRefForVar(Tm, p, InFlag, dbg);
    } else if (IsAtomOrIntTerm(Tm)) {
      LOCAL_Error_TYPE = oerr;
      return CreateDBRefForAtom(Tm, p, InFlag, dbg);
    }
  }
  /* next, let's process a compound term */
  {
    DBTerm *ppt, *ppt0;
    DBRef pp, pp0;
    Term *ntp0, *ntp;
    unsigned int NOfCells = 0;
#ifdef COROUTINING
    CELL attachments = 0;
#endif

    dbg->tofref = TmpRefBase;

    if (p == NULL) {
      ADDR ptr = Yap_PreAllocCodeSpace();
      ppt0 = (DBTerm *)(ptr + extra_size);
      pp0 = (DBRef)ppt0;
    } else {
      pp0 = (DBRef)Yap_PreAllocCodeSpace();
      ppt0 = &(pp0->DBT);
    }
    if ((ADDR)ppt0 >= (ADDR)AuxSp - 1024) {
      LOCAL_Error_Size = (UInt)(extra_size + sizeof(ppt0));
      LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
      Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
      LOCAL_Error_TYPE = oerr;
      return NULL;
    }
    ntp0 = ppt0->Contents;
    if ((ADDR)TR >= LOCAL_TrailTop - 1024) {
      LOCAL_Error_Size = 0;
      LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
      Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
      LOCAL_Error_TYPE = oerr;

      return NULL;
    }
    dbg->lr = dbg->LinkAr = (link_entry *)TR;
#ifdef COROUTINING
    /* attachment */
    if (IsVarTerm(Tm)) {
      tt = (CELL)(ppt0->Contents);
       ntp = MkDBTerm(VarOfTerm(Tm), VarOfTerm(Tm), ntp0, ntp0 + 1, ntp0 - 1,
                     &attachments, &vars_found, dbg);
      if (ntp == NULL) {
        Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
        LOCAL_Error_TYPE = oerr;
        return NULL;
      }
    } else
#endif
        if (IsPairTerm(Tm)) {
      /* avoid null pointers!! */
      tt = AbsPair(ppt0->Contents);
      ntp = MkDBTerm(RepPair(Tm), RepPair(Tm) + 1, ntp0, ntp0 + 2, ntp0 - 1,
#ifdef COROUTINING
                     &attachments,
#endif
                     &vars_found, dbg);
      if (ntp == NULL) {
        Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
        LOCAL_Error_TYPE = oerr;
        return NULL;
      }
    } else {
      unsigned int arity;
      Functor fun;
      vars_found = true;
      tt = AbsAppl(ppt0->Contents);
      /* we need to store the functor manually */
      fun = FunctorOfTerm(Tm);
      if (IsExtensionFunctor(fun)) {
        switch ((CELL)fun) {
        case (CELL)FunctorDouble:
          ntp = copy_double(ntp0, RepAppl(Tm));
          break;
        case (CELL)FunctorString:
	  {
	    UInt sz = 1024+sizeof(CELL)*(3 + RepAppl(Tm)[1]);
	    if (sz >
		(char*)AuxSp-(char*)ppt0) {
	      LOCAL_Error_Size = sz;
	      if (!Yap_ExpandPreAllocCodeSpace(LOCAL_Error_Size, NULL, TRUE)) {
		Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK, TermNil, LOCAL_ErrorMessage);
		return NULL;
	      }
	      goto retry_record;
	    }
	  }
	    ntp = copy_string(ntp0, RepAppl(Tm));
          break;
        case (CELL)FunctorDBRef:
          Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
          return CreateDBWithDBRef(Tm, p, dbg);
#ifdef USE_GMP
        case (CELL)FunctorBigInt:
	  {
	    UInt sz = 1024+sizeof(CELL)*Yap_SizeOfBigInt(Tm);
	    if (sz >
		(char*)AuxSp-(char*)ppt0) {
	      LOCAL_Error_Size = sizeof(CELL)*(3 + RepAppl(Tm)[1]);
	      if (!Yap_ExpandPreAllocCodeSpace(LOCAL_Error_Size, NULL, TRUE)) {
		Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK, TermNil, LOCAL_ErrorMessage);
		return NULL;
	      }
	      goto retry_record;
	    }
	  }
          ntp = copy_big_int(ntp0, RepAppl(Tm));
          break;
#endif
        default: /* LongInt */
          ntp = copy_long_int(ntp0, RepAppl(Tm));
          break;
        }
      } else {
        *ntp0 = (CELL)fun;
        arity = ArityOfFunctor(fun);
        ntp = MkDBTerm(RepAppl(Tm) + 1, RepAppl(Tm) + arity, ntp0 + 1,
                       ntp0 + 1 + arity, ntp0 - 1,
#ifdef COROUTINING
                       &attachments,
#endif
                       &vars_found, dbg);
        if (ntp == NULL) {
          Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
          LOCAL_Error_TYPE = oerr;
          return NULL;
        }
      }
    }
    CodeAbs = (CELL *)((CELL)ntp - (CELL)ntp0);
    if (LOCAL_Error_TYPE) {
      Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
      LOCAL_Error_TYPE = oerr;
      return NULL; /* Error Situation */
    }
    NOfCells = ntp - ntp0; /* End Of Code Info */
    *dbg->lr++ = 0;
    NOfLinks = (dbg->lr - dbg->LinkAr);
    if (vars_found || InFlag & InQueue) {

      /*
       * Take into account the fact that one needs an entry
       * for the number of links
       */
      flag = DBComplex;
      CodeAbs += (NOfLinks + (sizeof(CELL) / sizeof(BITS32) - 1)) /
                 (sizeof(CELL) / sizeof(BITS32));
      if ((CELL *)((char *)ntp0 + (CELL)CodeAbs) > AuxSp) {
        LOCAL_Error_Size = (UInt)DBLength(CodeAbs);
        LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
        Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
        LOCAL_Error_TYPE = oerr;
        return NULL;
      }
      if ((InFlag & MkIfNot) &&
          (dbg->found_one = check_if_wvars(p->First, NOfCells, ntp0))) {
        Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
        LOCAL_Error_TYPE = oerr;
        return dbg->found_one;
      }
    } else {
      flag = DBNoVars;
      if ((InFlag & MkIfNot) &&
          (dbg->found_one = check_if_nvars(p->First, NOfCells, ntp0, dbg))) {
        Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
        LOCAL_Error_TYPE = oerr;
        return dbg->found_one;
      }
    }
    if (dbg->tofref != TmpRefBase) {
      CodeAbs += (TmpRefBase - dbg->tofref) + 1;
      if ((CELL *)((char *)ntp0 + (CELL)CodeAbs) > AuxSp) {
        LOCAL_Error_Size = (UInt)DBLength(CodeAbs);
        LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
        Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
        LOCAL_Error_TYPE = oerr;
        return NULL;
      }
      flag |= DBWithRefs;
    }
#if SIZEOF_LINK_ENTRY == 2
    if (Unsigned(CodeAbs) >= 0x40000) {
      Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
      LOCAL_Error_TYPE = oerr;
      return generate_dberror_msg(SYSTEM_ERROR_INTERNAL, 0,
                                  "trying to store term larger than 256KB");
    }
#endif
    if (p == NULL) {
      UInt sz = (CELL)CodeAbs + extra_size + sizeof(DBTerm);
      ADDR ptr = Yap_AllocCodeSpace(sz);
      ppt = (DBTerm *)(ptr + extra_size);
      if (ptr == NULL) {
        Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
        LOCAL_Error_TYPE = oerr;
        return generate_dberror_msg(RESOURCE_ERROR_HEAP, sz,
                                    "heap crashed against stacks");
      }
      Yap_LUClauseSpace += sz;
      dbg->sz = sz;
      pp = (DBRef)ppt;
    } else {
      UInt sz = DBLength(CodeAbs);
      pp = AllocDBSpace(sz);
      if (pp == NULL) {
        Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
        LOCAL_Error_TYPE = oerr;
        return generate_dberror_msg(RESOURCE_ERROR_HEAP, sz,
                                    "heap crashed against stacks");
      }
      Yap_LUClauseSpace += sz;
      dbg->sz = sz;
      pp->id = FunctorDBRef;
      pp->Flags = flag;
      INIT_LOCK(pp->lock);
      INIT_DBREF_COUNT(pp);
      ppt = &(pp->DBT);
    }
    if (flag & DBComplex) {
      link_entry *woar;

      ppt->NOfCells = NOfCells;
#ifdef COROUTINING
      ppt->ag.attachments = attachments;
#endif
      if (pp0 != pp) {
        nar = ppt->Contents;
        nar = (Term *)cpcells(CellPtr(nar), ntp0, Unsigned(NOfCells));
      } else {
        nar = ppt->Contents + Unsigned(NOfCells);
      }
      woar = (link_entry *)nar;
      memmove((void *)woar, (const void *)dbg->LinkAr,
             (size_t)(NOfLinks * sizeof(link_entry)));
      woar += NOfLinks;
#ifdef ALIGN_LONGS
#if SIZEOF_INT_P == 8
      while ((Unsigned(woar) & 7) != 0)
        woar++;
#else
      if ((Unsigned(woar) & 3) != 0)
        woar++;
#endif
#endif
      nar = (Term *)(woar);
      *pstat = TRUE;
    } else if (flag & DBNoVars) {
      if (pp0 != pp) {
        nar = (Term *)cpcells(CellPtr(ppt->Contents), ntp0, Unsigned(NOfCells));
      } else {
        nar = ppt->Contents + Unsigned(NOfCells);
      }
      ppt->NOfCells = NOfCells;
    }
    if (ppt != ppt0) {
      linkblk(dbg->LinkAr, CellPtr(ppt->Contents - 1), (CELL)ppt - (CELL)ppt0);
      ppt->Entry = AdjustIDBPtr(tt, (CELL)ppt - (CELL)ppt0);
#ifdef COROUTINING
      if (attachments)
        ppt->ag.attachments = AdjustIDBPtr(attachments, (CELL)ppt - (CELL)ppt0);
      else
        ppt->ag.attachments = 0L;
#endif
    } else {
      ppt->Entry = tt;
#ifdef COROUTINING
      ppt->ag.attachments = attachments;
#endif
    }
    if (flag & DBWithRefs) {
      DBRef *ptr = TmpRefBase, *rfnar = (DBRef *)nar;

      *rfnar++ = NULL;
      while (ptr != dbg->tofref)
        *rfnar++ = *--ptr;
      ppt->DBRefs = rfnar;
    } else {
      ppt->DBRefs = NULL;
    }
    Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
    LOCAL_Error_TYPE = oerr;
    return pp;
  }
}

static DBRef record(int Flag, Term key, Term t_data, Term t_code USES_REGS) {
  Register Term twork = key;
  Register DBProp p;
  Register DBRef x;
  int needs_vars;
  struct db_globs dbg;

  dbg.found_one = NULL;
#ifdef SFUNC
  FathersPlace = NIL;
#endif
  if (EndOfPAEntr(
          p = FetchDBPropFromKey(twork, Flag & MkCode, TRUE, "record/3"))) {
    return NULL;
  }
  if ((x = CreateDBStruct(t_data, p, Flag, &needs_vars, 0, &dbg)) == NULL) {
    return NULL;
  }
  if ((Flag & MkIfNot) && dbg.found_one)
    return NULL;
  TRAIL_REF(x);
  if (x->Flags & (DBNoVars | DBComplex))
    x->Mask = EvalMasks(t_data, &x->Key);
  else
    x->Mask = x->Key = 0;
  if (Flag & MkCode)
    x->Flags |= DBCode;
  else
    x->Flags |= DBNoCode;
  x->Parent = p;
#if MULTIPLE_STACKS
  x->Flags |= DBClMask;
  x->ref_count = 1;
#else
  x->Flags |= (InUseMask | DBClMask);
#endif
  x->NOfRefsTo = 0;
  WRITE_LOCK(p->DBRWLock);
  if (p->F0 == NULL) {
    p->F0 = p->L0 = x;
    x->p = x->n = NULL;
  } else {
    if (Flag & MkFirst) {
      x->n = p->F0;
      p->F0->p = x;
      p->F0 = x;
      x->p = NULL;
    } else {
      x->p = p->L0;
      p->L0->n = x;
      p->L0 = x;
      x->n = NULL;
    }
  }
  if (p->First == NIL) {
    p->First = p->Last = x;
    x->Prev = x->Next = NIL;
  } else if (Flag & MkFirst) {
    x->Prev = NIL;
    (p->First)->Prev = x;
    x->Next = p->First;
    p->First = x;
  } else {
    x->Next = NIL;
    (p->Last)->Next = x;
    x->Prev = p->Last;
    p->Last = x;
  }
  if (Flag & MkCode) {
    x->Code = (yamop *)IntegerOfTerm(t_code);
  }
  WRITE_UNLOCK(p->DBRWLock);
  return x;
}

/* add a new entry next to an old one */
static DBRef record_at(int Flag, DBRef r0, Term t_data, Term t_code USES_REGS) {
  Register DBProp p;
  Register DBRef x;
  int needs_vars;
  struct db_globs dbg;

#ifdef SFUNC
  FathersPlace = NIL;
#endif
  p = r0->Parent;
  if ((x = CreateDBStruct(t_data, p, Flag, &needs_vars, 0, &dbg)) == NULL) {
    return NULL;
  }
  TRAIL_REF(x);
  if (x->Flags & (DBNoVars | DBComplex))
    x->Mask = EvalMasks(t_data, &x->Key);
  else
    x->Mask = x->Key = 0;
  if (Flag & MkCode)
    x->Flags |= DBCode;
  else
    x->Flags |= DBNoCode;
  x->Parent = p;
#if MULTIPLE_STACKS
  x->Flags |= DBClMask;
  x->ref_count = 1;
#else
  x->Flags |= (InUseMask | DBClMask);
#endif
  x->NOfRefsTo = 0;
  WRITE_LOCK(p->DBRWLock);
  if (Flag & MkFirst) {
    x->n = r0;
    x->p = r0->p;
    if (p->F0 == r0) {
      p->F0 = x;
    } else {
      r0->p->n = x;
    }
    r0->p = x;
  } else {
    x->p = r0;
    x->n = r0->n;
    if (p->L0 == r0) {
      p->L0 = x;
    } else {
      r0->n->p = x;
    }
    r0->n = x;
  }
  if (Flag & MkFirst) {
    x->Prev = r0->Prev;
    x->Next = r0;
    if (p->First == r0) {
      p->First = x;
    } else {
      r0->Prev->Next = x;
    }
    r0->Prev = x;
  } else {
    x->Next = r0->Next;
    x->Prev = r0;
    if (p->Last == r0) {
      p->Last = x;
    } else {
      r0->Next->Prev = x;
    }
    r0->Next = x;
  }
  if (Flag & WithRef) {
    x->Code = (yamop *)IntegerOfTerm(t_code);
  }
  WRITE_UNLOCK(p->DBRWLock);
  return x;
}

static LogUpdClause *new_lu_db_entry(Term t, PredEntry *pe) {
  CACHE_REGS
  DBTerm *x;
  LogUpdClause *cl;
  yamop *ipc;
  int needs_vars = FALSE;
  struct db_globs dbg;
  int d_flag = 0;

#if MULTIPLE_STACKS
  /* we cannot allow sharing between threads (for now) */
  if (!pe || !(pe->PredFlags & ThreadLocalPredFlag))
    d_flag |= InQueue;
#endif
  ipc = NEXTOP(((LogUpdClause *)NULL)->ClCode, e);
  if ((x = (DBTerm *)CreateDBStruct(t, NULL, d_flag, &needs_vars, (UInt)ipc,
                                    &dbg)) == NULL) {
    return NULL; /* crash */
  }
  cl = (LogUpdClause *)((ADDR)x - (UInt)ipc);
  ipc = cl->ClCode;
  cl->Id = FunctorDBRef;
  cl->ClFlags = LogUpdMask;
  cl->lusl.ClSource = x;
  cl->ClRefCount = 0;
  cl->ClPred = pe;
  cl->ClExt = NULL;
  cl->ClPrev = cl->ClNext = NULL;
  cl->ClSize = dbg.sz;
  /* Support for timestamps */
  if (pe && pe->LastCallOfPred != LUCALL_ASSERT) {
    if (pe->TimeStampOfPred >= TIMESTAMP_RESET)
      Yap_UpdateTimestamps(pe);
    ++pe->TimeStampOfPred;
    /*  fprintf(stderr,"+
     * %x--%d--%ul\n",pe,pe->TimeStampOfPred,pe->ArityOfPE);*/
    pe->LastCallOfPred = LUCALL_ASSERT;
    cl->ClTimeStart = pe->TimeStampOfPred;
  } else {
    cl->ClTimeStart = 0L;
  }
  cl->ClTimeEnd = TIMESTAMP_EOT;

#if MULTIPLE_STACKS
  //  INIT_LOCK(cl->ClLock);
  INIT_CLREF_COUNT(cl);
  ipc->opc = Yap_opcode(_copy_idb_term);
#else
  if (needs_vars)
    ipc->opc = Yap_opcode(_copy_idb_term);
  else
    ipc->opc = Yap_opcode(_unify_idb_term);
#endif

  return cl;
}

LogUpdClause *Yap_new_ludbe(Term t, PredEntry *pe, UInt nargs) {
  CACHE_REGS
  LogUpdClause *x;

  LOCAL_Error_Size = 0;
  while ((x = new_lu_db_entry(t, pe)) == NULL) {
    if (LOCAL_Error_TYPE == YAP_NO_ERROR) {
      break;
    } else {
      XREGS[nargs + 1] = t;
      if (recover_from_record_error(nargs + 1)) {
        t = Deref(XREGS[nargs + 1]);
      } else {
        return FALSE;
      }
    }
  }
  return x;
}

static LogUpdClause *record_lu(PredEntry *pe, Term t, int position) {
  LogUpdClause *cl;

  if ((cl = new_lu_db_entry(t, pe)) == NULL) {
    return NULL;
  }
  {
    Yap_inform_profiler_of_clause(cl, (char *)cl + cl->ClSize, pe,
                                  GPROF_NEW_LU_CLAUSE);
  }
  Yap_add_logupd_clause(pe, cl, (position == MkFirst ? 2 : 0));
  return cl;
}

static LogUpdClause *record_lu_at(int position, LogUpdClause *ocl, Term t) {
  LogUpdClause *cl;
  PredEntry *pe;

  pe = ocl->ClPred;
  PELOCK(62, pe);
  if ((cl = new_lu_db_entry(t, pe)) == NULL) {
    UNLOCK(pe->PELock);
    return NULL;
  }
  if (pe->NOfClauses > 1)
    Yap_RemoveIndexation(pe);
  if (position == MkFirst) {
    /* add before current clause */
    cl->ClNext = ocl;
    if (ocl->ClCode == pe->FirstClause) {
      cl->ClPrev = NULL;
      pe->FirstClause = cl->ClCode;
    } else {
      cl->ClPrev = ocl->ClPrev;
      ocl->ClPrev->ClNext = cl;
    }
    ocl->ClPrev = cl;
  } else {
    /* add after current clause */
    cl->ClPrev = ocl;
    if (ocl->ClCode == pe->LastClause) {
      cl->ClNext = NULL;
      pe->LastClause = cl->ClCode;
    } else {
      cl->ClNext = ocl->ClNext;
      ocl->ClNext->ClPrev = cl;
    }
    ocl->ClNext = cl;
  }
  pe->NOfClauses++;
  if (pe->NOfClauses > 1) {
    pe->OpcodeOfPred = INDEX_OPCODE;
    pe->CodeOfPred = (yamop *)(&(pe->OpcodeOfPred));
  }
  UNLOCK(pe->PELock);
  return cl;
}

/* recorda(+Functor,+Term,-Ref) */
static Int p_rcda(USES_REGS1) {
  /* Idiotic xlc's cpp does not work with ARG1 within MkDBRefTerm */
  Term TRef, t1 = Deref(ARG1);
  PredEntry *pe = NULL;

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  pe = find_lu_entry(t1);
  LOCAL_Error_Size = 0;
restart_record:
  if (pe) {
    LogUpdClause *cl;

    PELOCK(61, pe);
    cl = record_lu(pe, Deref(ARG2), MkFirst);
    if (cl != NULL) {
      TRAIL_CLREF(cl);
#if MULTIPLE_STACKS
      INC_CLREF_COUNT(cl);
#else
      cl->ClFlags |= InUseMask;
#endif
      TRef = MkDBRefTerm((DBRef)cl);
    } else {
      TRef = TermNil;
    }
    UNLOCK(pe->PELock);
  } else {
    TRef = MkDBRefTerm(record(MkFirst, t1, Deref(ARG2), Unsigned(0) PASS_REGS));
  }
  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    if (recover_from_record_error(3)) {
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  if (!pe)
    return FALSE;
  return Yap_unify(ARG3, TRef);
}

/* '$recordap'(+Functor,+Term,-Ref) */
static Int p_rcdap(USES_REGS1) {
  Term TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);

  if (!IsVarTerm(Deref(ARG3)))
    return FALSE;
  LOCAL_Error_Size = 0;
restart_record:
  TRef = MkDBRefTerm(record(MkFirst | MkCode, t1, t2, Unsigned(0) PASS_REGS));

  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    if (recover_from_record_error(3)) {
      t1 = Deref(ARG1);
      t2 = Deref(ARG2);
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  return Yap_unify(ARG3, TRef);
}

/* recorda_at(+DBRef,+Term,-Ref) */
/** @pred  recorda_at(+ _R0_, _T_,- _R_)


Makes term  _T_ the record preceding record with reference
 _R0_, and unifies  _R_ with its reference.


*/
static Int p_rcda_at(USES_REGS1) {
  /* Idiotic xlc's cpp does not work with ARG1 within MkDBRefTerm */
  Term TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);
  DBRef dbr;

  if (!IsVarTerm(Deref(ARG3)))
    return FALSE;
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "recorda_at/3");
    return FALSE;
  }
  if (!IsDBRefTerm(t1)) {
    Yap_Error(TYPE_ERROR_DBREF, t1, "recorda_at/3");
    return FALSE;
  }
  LOCAL_Error_Size = 0;
restart_record:
  dbr = DBRefOfTerm(t1);
  if (dbr->Flags & ErasedMask) {
    /* doesn't make sense */
    return FALSE;
  }
  if (dbr->Flags & LogUpdMask) {
    TRef = MkDBRefTerm((DBRef)record_lu_at(MkFirst, (LogUpdClause *)dbr, t2));
  } else {
    TRef = MkDBRefTerm(
        record_at(MkFirst, DBRefOfTerm(t1), t2, Unsigned(0) PASS_REGS));
  }
  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    if (recover_from_record_error(3)) {
      t1 = Deref(ARG1);
      t2 = Deref(ARG2);
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  return Yap_unify(ARG3, TRef);
}

/* recordz(+Functor,+Term,-Ref) */
/** @pred  recordz(+ _K_, _T_,- _R_)

Makes term  _T_ the last record under key  _K_ and unifies  _R_
with its reference.

*/
static Int p_rcdz(USES_REGS1) {
  Term TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);
  PredEntry *pe;

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  pe = find_lu_entry(t1);
  LOCAL_Error_Size = 0;
restart_record:
  if (pe) {
    LogUpdClause *cl;

    PELOCK(62, pe);
    cl = record_lu(pe, t2, MkLast);
    if (cl != NULL) {
      TRAIL_CLREF(cl);
#if MULTIPLE_STACKS
      INC_CLREF_COUNT(cl);
#else
      cl->ClFlags |= InUseMask;
#endif
      TRef = MkDBRefTerm((DBRef)cl);
    } else {
      TRef = TermNil;
    }
    UNLOCK(pe->PELock);
  } else {
    TRef = MkDBRefTerm(record(MkLast, t1, t2, Unsigned(0) PASS_REGS));
  }
  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    if (recover_from_record_error(3)) {
      t1 = Deref(ARG1);
      t2 = Deref(ARG2);
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  if (!pe)
    return FALSE;
  return Yap_unify(ARG3, TRef);
}

/* recordz(+Functor,+Term,-Ref) */
Int Yap_Recordz(Atom at, Term t2) {
  CACHE_REGS
  PredEntry *pe;

  pe = find_lu_entry(MkAtomTerm(at));
  LOCAL_Error_Size = 0;
restart_record:
  if (pe) {
    record_lu(pe, t2, MkLast);
  } else {
    record(MkLast, MkAtomTerm(at), t2, Unsigned(0) PASS_REGS);
  }
  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    ARG1 = t2;
    if (recover_from_record_error(1)) {
      t2 = ARG1;
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  return TRUE;
}

/* '$recordzp'(+Functor,+Term,-Ref) */
static Int p_rcdzp(USES_REGS1) {
  Term TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  LOCAL_Error_Size = 0;
restart_record:
  TRef = MkDBRefTerm(record(MkLast | MkCode, t1, t2, Unsigned(0) PASS_REGS));
  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    if (recover_from_record_error(3)) {
      t1 = Deref(ARG1);
      t2 = Deref(ARG2);
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  return Yap_unify(ARG3, TRef);
}

/* recordz_at(+Functor,+Term,-Ref) */
/** @pred  recordz_at(+ _R0_, _T_,- _R_)


Makes term  _T_ the record following record with reference
 _R0_, and unifies  _R_ with its reference.


*/
static Int p_rcdz_at(USES_REGS1) {
  /* Idiotic xlc's cpp does not work with ARG1 within MkDBRefTerm */
  Term TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);
  DBRef dbr;

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "recordz_at/3");
    return FALSE;
  }
  if (!IsDBRefTerm(t1)) {
    Yap_Error(TYPE_ERROR_DBREF, t1, "recordz_at/3");
    return FALSE;
  }
  LOCAL_Error_Size = 0;
restart_record:
  dbr = DBRefOfTerm(t1);
  if (dbr->Flags & ErasedMask) {
    /* doesn't make sense */
    return FALSE;
  }
  if (dbr->Flags & LogUpdMask) {
    TRef = MkDBRefTerm((DBRef)record_lu_at(MkLast, (LogUpdClause *)dbr, t2));
  } else {
    TRef = MkDBRefTerm(record_at(MkLast, dbr, t2, Unsigned(0) PASS_REGS));
  }
  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    if (recover_from_record_error(3)) {
      t1 = Deref(ARG1);
      t2 = Deref(ARG2);
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  return Yap_unify(ARG3, TRef);
}

/* '$record_stat_source'(+Functor,+Term) */
static Int p_rcdstatp(USES_REGS1) {
  Term t1 = Deref(ARG1), t2 = Deref(ARG2), t3 = Deref(ARG3);
  int mk_first;
  Term TRef;

  if (IsVarTerm(t3) || !IsIntTerm(t3))
    return (FALSE);
  if (IsVarTerm(t3) || !IsIntTerm(t3))
    return (FALSE);
  mk_first = ((IntOfTerm(t3) % 4) == 2);
  LOCAL_Error_Size = 0;
restart_record:
  if (mk_first)
    TRef =
        MkDBRefTerm(record(MkFirst | MkCode, t1, t2, MkIntTerm(0) PASS_REGS));
  else
    TRef = MkDBRefTerm(record(MkLast | MkCode, t1, t2, MkIntTerm(0) PASS_REGS));
  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    if (recover_from_record_error(4)) {
      t1 = Deref(ARG1);
      t2 = Deref(ARG2);
      t3 = Deref(ARG3);
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  return Yap_unify(ARG4, TRef);
}

/* '$recordap'(+Functor,+Term,-Ref,+CRef) */
static Int p_drcdap(USES_REGS1) {
  Term TRef, t1 = Deref(ARG1), t2 = Deref(ARG2), t4 = Deref(ARG4);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  if (IsVarTerm(t4) || !IsIntegerTerm(t4))
    return (FALSE);
  LOCAL_Error_Size = 0;
restart_record:
  TRef = MkDBRefTerm(record(MkFirst | MkCode | WithRef, t1, t2, t4 PASS_REGS));
  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    if (recover_from_record_error(4)) {
      t1 = Deref(ARG1);
      t2 = Deref(ARG2);
      t4 = Deref(ARG4);
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  return Yap_unify(ARG3, TRef);
}

/* '$recordzp'(+Functor,+Term,-Ref,+CRef) */
static Int p_drcdzp(USES_REGS1) {
  Term TRef, t1 = Deref(ARG1), t2 = Deref(ARG2), t4 = Deref(ARG4);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  if (IsVarTerm(t4) || !IsIntegerTerm(t4))
    return (FALSE);
restart_record:
  LOCAL_Error_Size = 0;
  TRef = MkDBRefTerm(record(MkLast | MkCode | WithRef, t1, t2, t4 PASS_REGS));
  if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
    if (recover_from_record_error(4)) {
      t1 = Deref(ARG1);
      t2 = Deref(ARG2);
      t4 = Deref(ARG4);
      goto restart_record;
    } else {
      return FALSE;
    }
  }
  return Yap_unify(ARG3, TRef);
}

static Int p_still_variant(USES_REGS1) {
  CELL *old_h = B->cp_h;
  tr_fr_ptr old_tr = B->cp_tr;
  Term t1 = Deref(ARG1), t2 = Deref(ARG2);
  DBTerm *dbt;
  DBRef dbr;

  if (IsVarTerm(t1) || !IsDBRefTerm(t1)) {
    return (FALSE);
    /* limited sanity checking */
    if (dbr->id != FunctorDBRef) {
      return FALSE;
    }
  } else {
    dbr = DBRefOfTerm(t1);
  }
  /* ok, we assume there was a choicepoint before we copied the term */

  /* skip binding for argument variable */
  old_tr++;
  if (dbr->Flags & LogUpdMask) {
    LogUpdClause *cl = (LogUpdClause *)dbr;

    if (old_tr == TR - 1) {
      if (TrailTerm(old_tr) != CLREF_TO_TRENTRY(cl))
        return FALSE;
    } else if (old_tr != TR)
      return FALSE;
    if (Yap_op_from_opcode(cl->ClCode->opc) == _unify_idb_term) {
      return TRUE;
    } else {
      dbt = cl->lusl.ClSource;
    }
  } else {
    if (old_tr == TR - 1) {
      if (TrailTerm(old_tr) != REF_TO_TRENTRY(dbr))
        return FALSE;
    } else if (old_tr != TR)
      return FALSE;
    if (dbr->Flags & (DBNoVars | DBAtomic))
      return TRUE;
    if (dbr->Flags & DBVar)
      return IsVarTerm(t2);
    dbt = &(dbr->DBT);
  }
  /*
    we checked the trail, so we are sure only variables in the new term
    were bound
  */
  {
    link_entry *lp = (link_entry *)(dbt->Contents + dbt->NOfCells);
    link_entry link;

    if (!dbt->NOfCells) {
      return IsVarTerm(t2);
    }
    while ((link = *lp++)) {
      Term t2 = Deref(old_h[link - 1]);
      if (IsUnboundVar(dbt->Contents + (link - 1))) {
        if (IsVarTerm(t2)) {
          Yap_unify(t2, MkAtomTerm(AtomFoundVar));
        } else {
          return FALSE;
        }
      }
    }
  }
  return TRUE;
}

#ifdef COROUTINING
static int copy_attachments(CELL *ts USES_REGS) {
  /* we will change delayed vars, and that also means the trail */
  tr_fr_ptr tr0 = TR;

  while (TRUE) {
    /* store away in case there is an overflow */

    if (GLOBAL_attas[IntegerOfTerm(ts[2])].term_to_op(ts[1], ts[0] PASS_REGS) ==
        FALSE) {
      /* oops, we did not have enough space to copy the elements */
      /* reset queue of woken up goals */
      TR = tr0;
      return FALSE;
    }
    if (ts[3] == TermNil)
      return TRUE;
    ts = RepAppl(ts[3]) + 1;
  }
}
#endif

static Term GetDBLUKey(PredEntry *ap) {
  PELOCK(63, ap);
  if (ap->PredFlags & NumberDBPredFlag) {
    CACHE_REGS
    Int id = ap->src.IndxId;
    UNLOCK(ap->PELock);
    return MkIntegerTerm(id);
  } else if (ap->PredFlags & AtomDBPredFlag ||
             (ap->ModuleOfPred != IDB_MODULE && ap->ArityOfPE == 0)) {
    Atom at = (Atom)ap->FunctorOfPred;
    UNLOCK(ap->PELock);
    return MkAtomTerm(at);
  } else {
    Functor f = ap->FunctorOfPred;
    UNLOCK(ap->PELock);
    return Yap_MkNewApplTerm(f, ArityOfFunctor(f));
  }
}

static int UnifyDBKey(DBRef DBSP, PropFlags flags, Term t) {
  DBProp p = DBSP->Parent;
  Term t1, tf;

  READ_LOCK(p->DBRWLock);
  /* get the key */
  if (p->ArityOfDB == 0) {
    t1 = MkAtomTerm((Atom)(p->FunctorOfDB));
  } else {
    t1 = Yap_MkNewApplTerm(p->FunctorOfDB, p->ArityOfDB);
  }
  if ((p->KindOfPE & CodeDBBit) && (flags & CodeDBBit)) {
    Term t[2];
    if (p->ModuleOfDB)
      t[0] = p->ModuleOfDB;
    else
      t[0] = TermProlog;
    t[1] = t1;
    tf = Yap_MkApplTerm(FunctorModule, 2, t);
  } else if (!(flags & CodeDBBit)) {
    tf = t1;
  } else {
    return FALSE;
  }
  READ_UNLOCK(p->DBRWLock);
  return Yap_unify(tf, t);
}

static int UnifyDBNumber(DBRef DBSP, Term t) {
  CACHE_REGS
  DBProp p = DBSP->Parent;
  DBRef ref;
  Int i = 1;

  READ_LOCK(p->DBRWLock);
  ref = p->First;
  while (ref != NIL) {
    if (ref == DBSP)
      break;
    if (!DEAD_REF(ref))
      i++;
    ref = ref->Next;
  }
  if (ref == NIL)
    return FALSE;
  READ_UNLOCK(p->DBRWLock);
  return Yap_unify(MkIntegerTerm(i), t);
}

Int Yap_unify_immediate_ref(DBRef ref USES_REGS) {
  // old immediate semantics style
  LOCK(ref->lock);
  if (ref == NULL || DEAD_REF(ref) || !UnifyDBKey(ref, 0, ARG1) ||
      !UnifyDBNumber(ref, ARG2)) {
    UNLOCK(ref->lock);
    return FALSE;
  } else {
    UNLOCK(ref->lock);
    return TRUE;
  }
}

static Term GetDBTerm(const DBTerm *DBSP, int src USES_REGS) {
  Term t = DBSP->Entry;

  if (IsVarTerm(t)
#if COROUTINING
      && !DBSP->ag.attachments
#endif
      ) {
    return MkVarTerm();
  } else if (IsAtomOrIntTerm(t)) {
    return t;
  } else {
    CELL *HOld = HR;
    CELL *HeapPtr;
    CELL *pt;
    CELL NOf;

    if (!(NOf = DBSP->NOfCells)) {
      return t;
    }
    pt = CellPtr(DBSP->Contents);
    CalculateStackGap(PASS_REGS1);
    if (HR + NOf > ASP - EventFlag / sizeof(CELL)) {
      if (LOCAL_PrologMode & InErrorMode) {
        LOCAL_PrologMode &= ~InErrorMode;
        if (HR + NOf > ASP)
          fprintf(stderr,
                  "\n\n [ FATAL ERROR: No Stack for Error Handling ]\n");
        Yap_exit(1);
      } else {
        LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
        LOCAL_Error_Size = NOf * sizeof(CELL);
        return (Term)0;
      }
    }
    HeapPtr = cpcells(HOld, pt, NOf);
    pt += HeapPtr - HOld;
    HR = HeapPtr;
    {
      link_entry *lp = (link_entry *)pt;
      linkblk(lp, HOld - 1, (CELL)HOld - (CELL)(DBSP->Contents));
    }
#ifdef COROUTINING
    if (DBSP->ag.attachments != 0L && !src) {
      if (!copy_attachments((CELL *)AdjustIDBPtr(
              DBSP->ag.attachments, (CELL)HOld - (CELL)(DBSP->Contents))
                                PASS_REGS)) {
        HR = HOld;
        LOCAL_Error_TYPE = RESOURCE_ERROR_ATTRIBUTED_VARIABLES;
        LOCAL_Error_Size = 0;
        return (Term)0;
      }
    }
#endif
    return AdjustIDBPtr(t, Unsigned(HOld) - (CELL)(DBSP->Contents));
  }
}

static Term GetDBTermFromDBEntry(DBRef DBSP USES_REGS) {
  if (DBSP->Flags & (DBNoVars | DBAtomic))
    return DBSP->DBT.Entry;
  return GetDBTerm(&(DBSP->DBT), FALSE PASS_REGS);
}

static void init_int_keys(void) {
  INT_KEYS = (Prop *)Yap_AllocCodeSpace(sizeof(Prop) * INT_KEYS_SIZE);
  if (INT_KEYS != NULL) {
    UInt i = 0;
    Prop *p = INT_KEYS;
    for (i = 0; i < INT_KEYS_SIZE; i++) {
      p[0] = NIL;
      p++;
    }
    Yap_LUClauseSpace += sizeof(Prop) * INT_KEYS_SIZE;
  }
}

static void init_int_lu_keys(void) {
  INT_LU_KEYS = (Prop *)Yap_AllocCodeSpace(sizeof(Prop) * INT_KEYS_SIZE);
  if (INT_LU_KEYS != NULL) {
    UInt i = 0;
    Prop *p = INT_LU_KEYS;
    for (i = 0; i < INT_KEYS_SIZE; i++) {
      p[0] = NULL;
      p++;
    }
    Yap_LUClauseSpace += sizeof(Prop) * INT_KEYS_SIZE;
  }
}

static int resize_int_keys(UInt new_size) {
  CACHE_REGS
  Prop *new;
  UInt i;
  UInt old_size = INT_KEYS_SIZE;

  YAPEnterCriticalSection();
  if (INT_KEYS == NULL) {
    INT_KEYS_SIZE = new_size;
    YAPLeaveCriticalSection();
    return TRUE;
  }
  new = (Prop *)Yap_AllocCodeSpace(sizeof(Prop) * new_size);
  if (new == NULL) {
    YAPLeaveCriticalSection();
    LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
    LOCAL_ErrorMessage = "could not allocate space";
    return FALSE;
  }
  Yap_LUClauseSpace += sizeof(Prop) * new_size;
  for (i = 0; i < new_size; i++) {
    new[i] = NIL;
  }
  for (i = 0; i < INT_KEYS_SIZE; i++) {
    if (INT_KEYS[i] != NIL) {
      Prop p0 = INT_KEYS[i];
      while (p0 != NIL) {
        DBProp p = RepDBProp(p0);
        CELL key = (CELL)(p->FunctorOfDB);
        UInt hash_key = (CELL)key % new_size;
        p0 = p->NextOfPE;
        p->NextOfPE = new[hash_key];
        new[hash_key] = AbsDBProp(p);
      }
    }
  }
  Yap_LUClauseSpace -= sizeof(Prop) * old_size;
  Yap_FreeCodeSpace((char *)INT_KEYS);
  INT_KEYS = new;
  INT_KEYS_SIZE = new_size;
  INT_KEYS_TIMESTAMP++;
  if (INT_KEYS_TIMESTAMP == MAX_ABS_INT)
    INT_KEYS_TIMESTAMP = 0;
  YAPLeaveCriticalSection();
  return TRUE;
}

static PredEntry *find_lu_int_key(Int key) {
  UInt hash_key = (CELL)key % INT_KEYS_SIZE;
  Prop p0;

  if (INT_LU_KEYS != NULL) {
    p0 = INT_LU_KEYS[hash_key];
    while (p0) {
      PredEntry *pe = RepPredProp(p0);
      if (pe->src.IndxId == key) {
        return pe;
      }
      p0 = pe->NextOfPE;
    }
  }
  if (UPDATE_MODE == UPDATE_MODE_LOGICAL && find_int_key(key) == NULL) {
    return new_lu_int_key(key);
  }
  return NULL;
}

PredEntry *Yap_FindLUIntKey(Int key) { return find_lu_int_key(key); }

static DBProp find_int_key(Int key) {
  UInt hash_key = (CELL)key % INT_KEYS_SIZE;
  Prop p0;

  if (INT_KEYS == NULL) {
    return NULL;
  }
  p0 = INT_KEYS[hash_key];
  while (p0) {
    DBProp p = RepDBProp(p0);
    if (p->FunctorOfDB == (Functor)key)
      return p;
    p0 = p->NextOfPE;
  }
  return NULL;
}

static PredEntry *new_lu_int_key(Int key) {
  UInt hash_key = (CELL)key % INT_KEYS_SIZE;
  PredEntry *p;
  Prop p0;
  Atom ae;

  if (INT_LU_KEYS == NULL) {
    init_int_lu_keys();
    if (INT_LU_KEYS == NULL) {
      CACHE_REGS
      LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
      LOCAL_ErrorMessage = "could not allocate space";
      return NULL;
    }
  }
  ae = AtomDInteger;
  WRITE_LOCK(ae->ARWLock);
  p0 = Yap_NewPredPropByAtom(ae, IDB_MODULE);
  p = RepPredProp(p0);
  p->NextOfPE = INT_LU_KEYS[hash_key];
  p->src.IndxId = key;
  p->PredFlags |= LogUpdatePredFlag | NumberDBPredFlag;
  p->ArityOfPE = 3;
  p->OpcodeOfPred = Yap_opcode(_op_fail);
  p->TrueCodeOfPred = p->CodeOfPred = FAILCODE;
  if (p->PredFlags & ProfiledPredFlag) {
    if (!Yap_initProfiler(p)) {
      return NULL;
    }
  }
  INT_LU_KEYS[hash_key] = p0;
  return p;
}

static PredEntry *new_lu_entry(Term t) {
  CACHE_REGS
  Prop p0;
  PredEntry *pe;

  if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);

    FUNC_WRITE_LOCK(f);
    p0 = Yap_NewPredPropByFunctor(f, IDB_MODULE);
  } else if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);

    WRITE_LOCK(RepAtom(at)->ARWLock);
    p0 = Yap_NewPredPropByAtom(at, IDB_MODULE);
  } else {
    FUNC_WRITE_LOCK(FunctorList);
    p0 = Yap_NewPredPropByFunctor(FunctorList, IDB_MODULE);
  }
  pe = RepPredProp(p0);
  pe->PredFlags |= LogUpdatePredFlag;
  if (IsAtomTerm(t)) {
    pe->PredFlags |= AtomDBPredFlag;
    pe->FunctorOfPred = (Functor)AtomOfTerm(t);
  } else {
    pe->FunctorOfPred = FunctorOfTerm(t);
  }
  pe->ArityOfPE = 3;
  pe->OpcodeOfPred = Yap_opcode(_op_fail);
  if (CurrentModule == PROLOG_MODULE)
    pe->PredFlags |= StandardPredFlag;
  pe->TrueCodeOfPred = pe->CodeOfPred = FAILCODE;
  if (pe->PredFlags & ProfiledPredFlag) {
    if (!Yap_initProfiler(pe)) {
      return NULL;
    }
  }
  return pe;
}

static DBProp find_entry(Term t) {
  Atom at;
  UInt arity;

  if (IsVarTerm(t)) {
    return RepDBProp(NIL);
  } else if (IsAtomTerm(t)) {
    at = AtomOfTerm(t);
    arity = 0;

  } else if (IsIntegerTerm(t)) {
    return find_int_key(IntegerOfTerm(t));
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);

    at = NameOfFunctor(f);
    arity = ArityOfFunctor(f);
  } else {
    at = AtomDot;
    arity = 2;
  }
  DBProp rc = RepDBProp(FindDBProp(RepAtom(at), 0, arity, 0));
  return rc;
}

static PredEntry *find_lu_entry(Term t) {
  Prop p;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "while accessing database key");
    return NULL;
  }
  if (IsIntegerTerm(t)) {
    return find_lu_int_key(IntegerOfTerm(t));
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_KEY, t, "while accessing database key");
      return NULL;
    }
    p = Yap_GetPredPropByFuncInThisModule(FunctorOfTerm(t), IDB_MODULE);
  } else if (IsAtomTerm(t)) {
    p = Yap_GetPredPropByAtomInThisModule(AtomOfTerm(t), IDB_MODULE);
  } else {
    p = Yap_GetPredPropByFuncInThisModule(FunctorList, IDB_MODULE);
  }
  if (p == NIL) {
    if (UPDATE_MODE == UPDATE_MODE_LOGICAL && !find_entry(t)) {
      return new_lu_entry(t);
    } else {
      return NULL;
    }
  }
  return RepPredProp(p);
}

static DBProp FetchIntDBPropFromKey(Int key, int flag, int new,
                                    char *error_mssg) {
  Functor fun = (Functor)key;
  UInt hash_key = (CELL)key % INT_KEYS_SIZE;
  Prop p0;

  if (INT_KEYS == NULL) {
    init_int_keys();
    if (INT_KEYS == NULL) {
      CACHE_REGS
      LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
      LOCAL_ErrorMessage = "could not allocate space";
      return NULL;
    }
  }
  p0 = INT_KEYS[hash_key];
  while (p0 != NIL) {
    DBProp p = RepDBProp(p0);
    if (p->FunctorOfDB == fun)
      return p;
    p0 = p->NextOfPE;
  }
  /* p is NULL, meaning we did not find the functor */
  if (new) {
    DBProp p;
    /* create a new DBProp				 */
    p = (DBProp)Yap_AllocAtomSpace(sizeof(*p));
    p->KindOfPE = DBProperty | flag;
    p->F0 = p->L0 = NULL;
    p->ArityOfDB = 0;
    p->First = p->Last = NULL;
    p->ModuleOfDB = 0;
    p->FunctorOfDB = fun;
    p->NextOfPE = INT_KEYS[hash_key];
    INIT_RWLOCK(p->DBRWLock);
    INT_KEYS[hash_key] = AbsDBProp(p);
    return p;
  } else {
    return RepDBProp(NULL);
  }
}

static DBProp FetchDBPropFromKey(Term twork, int flag, int new,
                                 char *error_mssg) {
  Atom At;
  Int arity;
  Term dbmod;

  if (flag & MkCode) {
    if (IsVarTerm(twork)) {
      Yap_Error(INSTANTIATION_ERROR, twork, error_mssg);
      return RepDBProp(NULL);
    }
    if (!IsApplTerm(twork)) {
      Yap_Error(SYSTEM_ERROR_INTERNAL, twork, "missing module");
      return RepDBProp(NULL);
    } else {
      Functor f = FunctorOfTerm(twork);
      if (f != FunctorModule) {
        Yap_Error(SYSTEM_ERROR_INTERNAL, twork, "missing module");
        return RepDBProp(NULL);
      }
      dbmod = ArgOfTerm(1, twork);
      if (IsVarTerm(dbmod)) {
        Yap_Error(INSTANTIATION_ERROR, twork, "var in module");
        return RepDBProp(NIL);
      }
      if (!IsAtomTerm(dbmod)) {
        Yap_Error(TYPE_ERROR_ATOM, twork, "not atom in module");
        return RepDBProp(NIL);
      }
      twork = ArgOfTerm(2, twork);
    }
  } else {
    dbmod = 0;
  }
  if (IsVarTerm(twork)) {
    Yap_Error(INSTANTIATION_ERROR, twork, error_mssg);
    return RepDBProp(NIL);
  } else if (IsAtomTerm(twork)) {
    arity = 0, At = AtomOfTerm(twork);
  } else if (IsIntegerTerm(twork)) {
    return FetchIntDBPropFromKey(IntegerOfTerm(twork), flag, new, error_mssg);
  } else if (IsApplTerm(twork)) {
    Register Functor f = FunctorOfTerm(twork);
    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_KEY, twork, error_mssg);
      return RepDBProp(NIL);
    }
    At = NameOfFunctor(f);
    arity = ArityOfFunctor(f);
  } else if (IsPairTerm(twork)) {
    At = AtomDot;
    arity = 2;
  } else {
    Yap_Error(TYPE_ERROR_KEY, twork, error_mssg);
    return RepDBProp(NIL);
  }
  if (new) {
    DBProp p;
    AtomEntry *ae = RepAtom(At);

    WRITE_LOCK(ae->ARWLock);
    if (EndOfPAEntr(
            p = RepDBProp(FindDBPropHavingLock(ae, flag, arity, dbmod)))) {
      /* create a new DBProp				 */
      int OLD_UPDATE_MODE = UPDATE_MODE;
      if (flag & MkCode) {
        PredEntry *pp;
        pp = RepPredProp(Yap_GetPredPropHavingLock(At, arity, dbmod));

        if (!EndOfPAEntr(pp)) {
          PELOCK(64, pp);
          if (pp->PredFlags & LogUpdatePredFlag)
            UPDATE_MODE = UPDATE_MODE_LOGICAL;
          UNLOCK(pp->PELock);
        }
      }
      p = (DBProp)Yap_AllocAtomSpace(sizeof(*p));
      p->KindOfPE = DBProperty | flag;
      p->F0 = p->L0 = NULL;
      UPDATE_MODE = OLD_UPDATE_MODE;
      p->ArityOfDB = arity;
      p->First = p->Last = NIL;
      p->ModuleOfDB = dbmod;
      /* This is NOT standard but is QUITE convenient */
      INIT_RWLOCK(p->DBRWLock);
      if (arity == 0)
        p->FunctorOfDB = (Functor)At;
      else
        p->FunctorOfDB = Yap_UnlockedMkFunctor(ae, arity);
      AddPropToAtom(ae, (PropEntry *)p);
    }
    WRITE_UNLOCK(ae->ARWLock);
    return p;
  } else
    return RepDBProp(FindDBProp(RepAtom(At), flag, arity, dbmod));
}

static Int lu_nth_recorded(PredEntry *pe, Int Count USES_REGS) {
  LogUpdClause *cl;

  XREGS[2] = MkVarTerm();
  cl = Yap_NthClause(pe, Count);
  if (cl == NULL)
    return FALSE;
#if MULTIPLE_STACKS
  TRAIL_CLREF(cl); /* So that fail will erase it */
  INC_CLREF_COUNT(cl);
#else
  if (!(cl->ClFlags & InUseMask)) {
    cl->ClFlags |= InUseMask;
    TRAIL_CLREF(cl); /* So that fail will erase it */
  }
#endif
  UNLOCK(pe->PELock);
  return Yap_unify(MkDBRefTerm((DBRef)cl), ARG4);
}

/* Finds a term recorded under the key ARG1			 */
static Int nth_recorded(DBProp AtProp, Int Count USES_REGS) {
  Register DBRef ref;

  READ_LOCK(AtProp->DBRWLock);
  ref = AtProp->First;
  Count--;
  while (ref != NULL && DEAD_REF(ref))
    ref = NextDBRef(ref);
  if (ref == NULL) {
    READ_UNLOCK(AtProp->DBRWLock);
    return FALSE;
  }
  while (Count) {
    Count--;
    ref = NextDBRef(ref);
    while (ref != NULL && DEAD_REF(ref))
      ref = NextDBRef(ref);
    if (ref == NULL) {
      READ_UNLOCK(AtProp->DBRWLock);
      return FALSE;
    }
  }
#if MULTIPLE_STACKS
  LOCK(ref->lock);
  READ_UNLOCK(AtProp->DBRWLock);
  TRAIL_REF(ref); /* So that fail will erase it */
  INC_DBREF_COUNT(ref);
  UNLOCK(ref->lock);
#else
  if (!(ref->Flags & InUseMask)) {
    ref->Flags |= InUseMask;
    TRAIL_REF(ref); /* So that fail will erase it */
  }
  READ_UNLOCK(AtProp->DBRWLock);
#endif
  return Yap_unify(MkDBRefTerm(ref), ARG4);
}

Int Yap_db_nth_recorded(PredEntry *pe, Int Count USES_REGS) {
  DBProp AtProp;

  if (pe == NULL) {
    return lu_nth_recorded(pe, Count PASS_REGS);
  }
  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(Deref(ARG1), 0, FALSE,
                                              "nth_instance/3"))) {
    UNLOCK(pe->PELock);
    return FALSE;
  }
  return nth_recorded(AtProp, Count PASS_REGS);
}

static Int p_db_key(USES_REGS1) {
  Register Term twork = Deref(ARG1); /* fetch the key */
  DBProp AtProp;

  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(twork, 0, TRUE, "db_key/3"))) {
    /* should never happen */
    return FALSE;
  }
  return Yap_unify(ARG2, MkIntegerTerm((Int)AtProp));
}

/* Finds a term recorded under the key ARG1			 */
static Int i_recorded(DBProp AtProp, Term t3 USES_REGS) {
  Term TermDB, TRef;
  Register DBRef ref;
  Term twork;

  READ_LOCK(AtProp->DBRWLock);
  ref = AtProp->First;
  while (ref != NULL && DEAD_REF(ref))
    ref = NextDBRef(ref);
  READ_UNLOCK(AtProp->DBRWLock);
  if (ref == NULL) {
    cut_fail();
  }
  twork = Deref(ARG2); /* now working with ARG2 */
  if (IsVarTerm(twork)) {
    EXTRA_CBACK_ARG(3, 2) = MkIntegerTerm(0);
    EXTRA_CBACK_ARG(3, 3) = MkIntegerTerm(0);
    B->cp_h = HR;
    while ((TermDB = GetDBTermFromDBEntry(ref PASS_REGS)) == (CELL)0) {
      /* make sure the garbage collector sees what we want it to see! */
      EXTRA_CBACK_ARG(3, 1) = (CELL)ref;
      /* oops, we are in trouble, not enough stack space */
      if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_growglobal(NULL)) {
          Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                    LOCAL_ErrorMessage);
          return FALSE;
        }
      } else {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_gcl(LOCAL_Error_Size, 3, ENV, CP)) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
      LOCAL_Error_Size = 0;
      twork = Deref(ARG2);
      t3 = Deref(ARG3);
    }
    if (!Yap_unify(twork, TermDB)) {
      cut_fail();
    }
  } else if (IsAtomOrIntTerm(twork)) {
    EXTRA_CBACK_ARG(3, 2) = MkIntegerTerm(0);
    EXTRA_CBACK_ARG(3, 3) = MkIntegerTerm((Int)twork);
    B->cp_h = HR;
    READ_LOCK(AtProp->DBRWLock);
    do {
      if (((twork == ref->DBT.Entry) || IsVarTerm(ref->DBT.Entry)) &&
          !DEAD_REF(ref))
        break;
      ref = NextDBRef(ref);
      if (ref == NIL) {
        READ_UNLOCK(AtProp->DBRWLock);
        cut_fail();
      }
    } while (TRUE);
    READ_UNLOCK(AtProp->DBRWLock);
  } else {
    CELL key;
    CELL mask = EvalMasks(twork, &key);

    B->cp_h = HR;
    READ_LOCK(AtProp->DBRWLock);
    do {
      while ((mask & ref->Key) != (key & ref->Mask) && !DEAD_REF(ref)) {
        ref = NextDBRef(ref);
        if (ref == NULL) {
          READ_UNLOCK(AtProp->DBRWLock);
          cut_fail();
        }
      }
      if ((TermDB = GetDBTermFromDBEntry(ref PASS_REGS)) != (CELL)0) {
        if (Yap_unify(TermDB, ARG2)) {
          /* success */
          EXTRA_CBACK_ARG(3, 2) = MkIntegerTerm(((Int)mask));
          EXTRA_CBACK_ARG(3, 3) = MkIntegerTerm(((Int)key));
          B->cp_h = HR;
          break;
        } else {
          while ((ref = NextDBRef(ref)) != NULL && DEAD_REF(ref))
            ;
          if (ref == NULL) {
            READ_UNLOCK(AtProp->DBRWLock);
            cut_fail();
          }
        }
      } else {
        /* make sure the garbage collector sees what we want it to see! */
        EXTRA_CBACK_ARG(3, 1) = (CELL)ref;
        READ_UNLOCK(AtProp->DBRWLock);
        EXTRA_CBACK_ARG(3, 2) = MkIntegerTerm(((Int)mask));
        EXTRA_CBACK_ARG(3, 3) = MkIntegerTerm(((Int)key));
        /* oops, we are in trouble, not enough stack space */
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_growglobal(NULL)) {
            Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                      LOCAL_ErrorMessage);
            return FALSE;
          }
        } else {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_gcl(LOCAL_Error_Size, 3, ENV, CP)) {
            Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            return FALSE;
          }
        }
        READ_LOCK(AtProp->DBRWLock);
      }
    } while (TRUE);
    READ_UNLOCK(AtProp->DBRWLock);
  }
  EXTRA_CBACK_ARG(3, 1) = (CELL)ref;
  /* This should be after any non-tagged terms, because the routines in grow.c
     go from upper to lower addresses */
  TRef = MkDBRefTerm(ref);
#if MULTIPLE_STACKS
  LOCK(ref->lock);
  TRAIL_REF(ref); /* So that fail will erase it */
  INC_DBREF_COUNT(ref);
  UNLOCK(ref->lock);
#else
  if (!(ref->Flags & InUseMask)) {
    ref->Flags |= InUseMask;
    TRAIL_REF(ref); /* So that fail will erase it */
  }
#endif
  return (Yap_unify(ARG3, TRef));
}

static Int c_recorded(int flags USES_REGS) {
  Term TermDB, TRef;
  Register DBRef ref, ref0;
  CELL *PreviousHeap = HR;
  CELL mask, key;
  Term t1;

  t1 = EXTRA_CBACK_ARG(3, 1);
  ref0 = (DBRef)t1;
  READ_LOCK(ref0->Parent->DBRWLock);
  ref = NextDBRef(ref0);
  if (ref == NIL) {
    if (ref0->Flags & ErasedMask) {
      ref = ref0;
      while ((ref = ref->n) != NULL) {
        if (!(ref->Flags & ErasedMask))
          break;
      }
      /* we have used the DB entry, so we can remove it now, although
         first we have to make sure noone is pointing to it */
      if (ref == NULL) {
        READ_UNLOCK(ref0->Parent->DBRWLock);
        cut_fail();
      }
    } else {
      READ_UNLOCK(ref0->Parent->DBRWLock);
      cut_fail();
    }
  }

  {
    Term ttmp = EXTRA_CBACK_ARG(3, 2);
    if (IsLongIntTerm(ttmp))
      mask = (CELL)LongIntOfTerm(ttmp);
    else
      mask = (CELL)IntOfTerm(ttmp);
  }
  {
    Term ttmp = EXTRA_CBACK_ARG(3, 3);
    if (IsLongIntTerm(ttmp))
      key = (CELL)LongIntOfTerm(ttmp);
    else
      key = (CELL)IntOfTerm(ttmp);
  }
  while (ref != NIL && DEAD_REF(ref))
    ref = NextDBRef(ref);
  if (ref == NIL) {
    READ_UNLOCK(ref0->Parent->DBRWLock);
    cut_fail();
  }
  if (mask == 0 && key == 0) { /* ARG2 is a variable */
    while ((TermDB = GetDBTermFromDBEntry(ref PASS_REGS)) == (CELL)0) {
      /* make sure the garbage collector sees what we want it to see! */
      EXTRA_CBACK_ARG(3, 1) = (CELL)ref;
      /* oops, we are in trouble, not enough stack space */
      if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_growglobal(NULL)) {
          Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                    LOCAL_ErrorMessage);
          return FALSE;
        }
      } else {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_gcl(LOCAL_Error_Size, 3, ENV, CP)) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
      LOCAL_Error_Size = 0;
      PreviousHeap = HR;
    }
    Yap_unify(ARG2, TermDB);
  } else if (mask == 0) { /* ARG2 is a constant */
    do {
      if (((key == Unsigned(ref->DBT.Entry)) || (ref->Flags & DBVar)) &&
          !DEAD_REF(ref))
        break;
      ref = NextDBRef(ref);
    } while (ref != NIL);
    if (ref == NIL) {
      READ_UNLOCK(ref0->Parent->DBRWLock);
      cut_fail();
    }
  } else
    do { /* ARG2 is a structure */
      HR = PreviousHeap;
      while ((mask & ref->Key) != (key & ref->Mask)) {
        while ((ref = NextDBRef(ref)) != NIL && DEAD_REF(ref))
          ;
        if (ref == NIL) {
          READ_UNLOCK(ref0->Parent->DBRWLock);
          cut_fail();
        }
      }
      while ((TermDB = GetDBTermFromDBEntry(ref PASS_REGS)) == (CELL)0) {
        /* make sure the garbage collector sees what we want it to see! */
        EXTRA_CBACK_ARG(3, 1) = (CELL)ref;
        /* oops, we are in trouble, not enough stack space */
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_growglobal(NULL)) {
            Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                      LOCAL_ErrorMessage);
            return FALSE;
          }
        } else {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_gcl(LOCAL_Error_Size, 3, ENV, CP)) {
            Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            return FALSE;
          }
        }
        LOCAL_Error_Size = 0;
        PreviousHeap = HR;
      }
      if (Yap_unify(ARG2, TermDB))
        break;
      while ((ref = NextDBRef(ref)) != NIL && DEAD_REF(ref))
        ;
      if (ref == NIL) {
        READ_UNLOCK(ref0->Parent->DBRWLock);
        cut_fail();
      }
    } while (1);
  READ_UNLOCK(ref0->Parent->DBRWLock);
  TRef = MkDBRefTerm(ref);
  EXTRA_CBACK_ARG(3, 1) = (CELL)ref;
#if MULTIPLE_STACKS
  LOCK(ref->lock);
  TRAIL_REF(ref); /* So that fail will erase it */
  INC_DBREF_COUNT(ref);
  UNLOCK(ref->lock);
#else
  if (!(ref->Flags & InUseMask)) {
    ref->Flags |= InUseMask;
    TRAIL_REF(ref); /* So that fail will erase it */
  }
#endif
  return (Yap_unify(ARG3, TRef));
}

/*
 * The arguments for this 4 functions are the flags for terms which should be
 * skipped
 */

static Int lu_recorded(PredEntry *pe USES_REGS) {
  op_numbers opc = Yap_op_from_opcode(P->opc);

#if defined(YAPOR) || defined(THREADS)
  PELOCK(66, pe);
  PP = pe;
#endif
  if (opc == _procceed) {
    P = pe->CodeOfPred;
  } else {
    if (P->opc != Yap_opcode(_execute_cpred)) {
      CP = P;
      ENV = YENV;
      YENV = ASP;
      YENV[E_CB] = (CELL)B;
    }
    P = pe->CodeOfPred;
#if defined(YAPOR) || defined(THREADS)
    /* avoid holding a lock if we don't have anything in the database */
    if (P == FAILCODE) {
      UNLOCK(pe->PELock);
      PP = NULL;
    }
#endif
  }
  if (pe->PredFlags & ProfiledPredFlag) {
    LOCK(pe->StatisticsForPred->lock);

    pe->StatisticsForPred->NOfEntries++;
    UNLOCK(pe->StatisticsForPred->lock);
  }
  return TRUE;
}

/* recorded(+Functor,+Term,-Ref) */
static Int in_rded_with_key(USES_REGS1) {
  DBProp AtProp = (DBProp)IntegerOfTerm(Deref(ARG1));

  return (i_recorded(AtProp, Deref(ARG3) PASS_REGS));
}

/* recorded(+Functor,+Term,-Ref) */
static Int p_recorded(USES_REGS1) {
  DBProp AtProp;
  Register Term twork = Deref(ARG1); /* initially working with
                                      * ARG1 */
  Term t3 = Deref(ARG3);
  PredEntry *pe;

  if (!IsVarTerm(t3)) {
    DBRef ref = DBRefOfTerm(t3);
    if (!IsDBRefTerm(t3)) {
      return FALSE;
    } else {
      ref = DBRefOfTerm(t3);
    }
    ref = DBRefOfTerm(t3);
    if (ref == NULL)
      return FALSE;
    if (DEAD_REF(ref)) {
      return FALSE;
    }
    if (ref->Flags & LogUpdMask) {
      LogUpdClause *cl = (LogUpdClause *)ref;
      PredEntry *ap = cl->ClPred;
      op_numbers opc = Yap_op_from_opcode(P->opc);

      if (!Yap_unify(GetDBLUKey(ap), ARG1))
        return FALSE;

      if (opc == _procceed) {
        P = cl->ClCode;
      } else {
        CP = P;
#if defined(YAPOR) || defined(THREADS)
        PP = cl->ClPred;
#endif
        P = cl->ClCode;
        ENV = YENV;
        YENV = ASP;
        YENV[E_CB] = (CELL)B;
      }
      return TRUE;
    } else {
      Term TermDB;
      while ((TermDB = GetDBTermFromDBEntry(ref PASS_REGS)) == (CELL)0) {
        /* oops, we are in trouble, not enough stack space */
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_growglobal(NULL)) {
            Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                      LOCAL_ErrorMessage);
            return FALSE;
          }
        } else {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_gcl(LOCAL_Error_Size, 3, ENV, gc_P(P, CP))) {
            Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            return FALSE;
          }
        }
      }
      if (!Yap_unify(ARG2, TermDB) || !UnifyDBKey(ref, 0, ARG1)) {
        return FALSE;
      } else {
        return TRUE;
      }
    }
  }
  if ((pe = find_lu_entry(twork)) != NULL) {
    return lu_recorded(pe PASS_REGS);
  }
  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(twork, 0, FALSE, "recorded/3"))) {
    return FALSE;
  }
  ARG1 = MkIntegerTerm((Int)AtProp);
  P = PredRecordedWithKey->CodeOfPred;
  return (i_recorded(AtProp, t3 PASS_REGS));
}

static Int co_rded(USES_REGS1) { return (c_recorded(0 PASS_REGS)); }

/* '$recordedp'(+Functor,+Term,-Ref) */
static Int in_rdedp(USES_REGS1) {
  DBProp AtProp;
  register choiceptr b0 = B;
  Register Term twork = Deref(ARG1); /* initially working with
                                      * ARG1 */

  Term t3 = Deref(ARG3);
  if (!IsVarTerm(t3)) {
    if (!IsDBRefTerm(t3)) {
      cut_fail();
    } else {
      DBRef ref = DBRefOfTerm(t3);
      LOCK(ref->lock);
      if (ref == NULL || DEAD_REF(ref) ||
          !Yap_unify(ARG2, GetDBTermFromDBEntry(ref PASS_REGS)) ||
          !UnifyDBKey(ref, CodeDBBit, ARG1)) {
        UNLOCK(ref->lock);
        cut_fail();
      } else {
        UNLOCK(ref->lock);
        cut_succeed();
      }
    }
  }
  if (EndOfPAEntr(AtProp =
                      FetchDBPropFromKey(twork, MkCode, FALSE, "recorded/3"))) {
    if (b0 == B)
      cut_fail();
    else
      return FALSE;
  }
  return (i_recorded(AtProp, t3 PASS_REGS));
}

static Int co_rdedp(USES_REGS1) { return (c_recorded(MkCode PASS_REGS)); }

/* '$some_recordedp'(Functor)				 */
static Int p_somercdedp(USES_REGS1) {
  Register DBRef ref;
  DBProp AtProp;
  Register Term twork = Deref(ARG1); /* initially working with
                                              * ARG1 */
  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(twork, MkCode, FALSE,
                                              "some_recorded/3"))) {
    return FALSE;
  }
  READ_LOCK(AtProp->DBRWLock);
  ref = FrstDBRef(AtProp);
  while (ref != NIL && (ref->Flags & (DBNoCode | ErasedMask)))
    ref = NextDBRef(ref);
  READ_UNLOCK(AtProp->DBRWLock);
  if (ref == NIL)
    return (FALSE);
  else
    return (TRUE);
}

/* Finds the first instance recorded under key ARG1			 */
static Int p_first_instance(USES_REGS1) {
  Term TRef;
  Register DBRef ref;
  DBProp AtProp;
  Register Term twork = Deref(ARG1); /* initially working with
                                      * ARG1 */
  Term TermDB;

  ARG3 = Deref(ARG3);
  if (!IsVarTerm(ARG3)) {
    cut_fail();
  }
  if (EndOfPAEntr(
          AtProp = FetchDBPropFromKey(twork, 0, FALSE, "first_instance/3"))) {
    return FALSE;
  }
  READ_LOCK(AtProp->DBRWLock);
  ref = AtProp->First;
  while (ref != NIL && (ref->Flags & (DBCode | ErasedMask)))
    ref = NextDBRef(ref);
  READ_UNLOCK(AtProp->DBRWLock);
  if (ref == NIL) {
    cut_fail();
  }
  TRef = MkDBRefTerm(ref);
  /* we have a pointer to the term available */
  LOCK(ref->lock);
#if MULTIPLE_STACKS
  TRAIL_REF(ref); /* So that fail will erase it */
  INC_DBREF_COUNT(ref);
#else
  if (!(ref->Flags & InUseMask)) {
    ref->Flags |= InUseMask;
    TRAIL_REF(ref); /* So that fail will erase it */
  }
#endif
  UNLOCK(ref->lock);
  while ((TermDB = GetDBTermFromDBEntry(ref PASS_REGS)) == (CELL)0) {
    /* oops, we are in trouble, not enough stack space */
    if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_growglobal(NULL)) {
        Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                  LOCAL_ErrorMessage);
        return FALSE;
      }
    } else {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_gcl(LOCAL_Error_Size, 3, ENV, gc_P(P, CP))) {
        Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        return FALSE;
      }
    }
  }
  if (IsVarTerm(TermDB)) {
    Yap_unify(TermDB, ARG2);
  } else {
    return Yap_unify(ARG2, TermDB);
  }
  return Yap_unify(ARG3, TRef);
}

static UInt index_sz(LogUpdIndex *x) {
  UInt sz = x->ClSize;
  yamop *start = x->ClCode;
  op_numbers op = Yap_op_from_opcode(start->opc);

  /* add try-retry-trust children */
  while (op == _jump_if_nonvar) {
    start = NEXTOP(start, xll);
    op = Yap_op_from_opcode(start->opc);
  }
  if (op == _enter_lu_pred) {
    PredEntry *ap = x->ClPred;
    OPCODE endop, op1;
    UInt count = 0, dead = 0;

    if (ap->PredFlags & CountPredFlag)
      endop = Yap_opcode(_count_trust_logical);
    else if (ap->PredFlags & ProfiledPredFlag)
      endop = Yap_opcode(_profiled_trust_logical);
    else
      endop = Yap_opcode(_trust_logical);
    start = start->y_u.Illss.l1;
    if (start->y_u.Illss.s)
      do {
        sz += (UInt)NEXTOP((yamop *)NULL, OtaLl);
        op1 = start->opc;
        count++;
        if (start->y_u.OtaLl.d->ClFlags & ErasedMask)
          dead++;
        start = start->y_u.OtaLl.n;
      } while (op1 != endop);
  }
  x = x->ChildIndex;
  while (x != NULL) {
    sz += index_sz(x);
    x = x->SiblingIndex;
  }
  return sz;
}

static Int lu_statistics(PredEntry *pe USES_REGS) {
  UInt sz = sizeof(PredEntry), cls = 0, isz = 0;

  /* count number of clauses and size */
  LogUpdClause *x;

  if (pe->FirstClause == NULL) {
    cls = 0;
    sz = 0;
  } else {
    x = ClauseCodeToLogUpdClause(pe->FirstClause);
    while (x != NULL) {
      cls++;
      sz += x->ClSize;
      x = x->ClNext;
    }
  }
  isz = 0;
  if (pe->PredFlags & IndexedPredFlag) {
    /* expand clause blocks */
    yamop *ep = ExpandClausesFirst;
    while (ep) {
      if (ep->y_u.sssllp.p == pe)
        isz += (UInt)NEXTOP((yamop *)NULL, sssllp) +
               ep->y_u.sssllp.s1 * sizeof(yamop *);
      ep = ep->y_u.sssllp.snext;
    }
    isz += index_sz(ClauseCodeToLogUpdIndex(pe->TrueCodeOfPred));
  }
  return Yap_unify(ARG2, MkIntegerTerm(cls)) &&
         Yap_unify(ARG3, MkIntegerTerm(sz)) &&
         Yap_unify(ARG4, MkIntegerTerm(isz));
}

/** @pred  key_statistics(+ _K_,- _Entries_,- _Size_,- _IndexSize_)


Returns several statistics for a key  _K_. Currently, it says how
many entries we have for that key,  _Entries_, what is the
total size spent on entries,  _Size_, and what is the amount of
space spent in indices.


*/
static Int p_key_statistics(USES_REGS1) {
  Register DBProp p;
  Register DBRef x;
  UInt sz = 0, cls = 0;
  Term twork = Deref(ARG1);
  PredEntry *pe;

  if ((pe = find_lu_entry(twork)) != NULL) {
    return lu_statistics(pe PASS_REGS);
  }
  if (EndOfPAEntr(p = FetchDBPropFromKey(twork, 0, TRUE, "key_statistics/4"))) {
    /* This is not a key property */
    return FALSE;
  }
  /* count number of clauses and size */
  x = p->First;
  while (x != NULL) {
    cls++;
    sz += sizeof(DBStruct) + sizeof(CELL) * x->DBT.NOfCells;
    if (x->Code) {
      DynamicClause *cl = ClauseCodeToDynamicClause(x->Code);
      sz += cl->ClSize;
    }
    x = NextDBRef(x);
  }
  return Yap_unify(ARG2, MkIntegerTerm(cls)) &&
         Yap_unify(ARG3, MkIntegerTerm(sz)) && Yap_unify(ARG4, MkIntTerm(0));
}

static Int p_lu_statistics(USES_REGS1) {
  Term t = Deref(ARG1);
  Term mod = Deref(ARG5);
  PredEntry *pe;
  if (IsVarTerm(t)) {
    return (FALSE);
  } else if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(Yap_GetPredPropByAtom(at, mod));
  } else if (IsIntegerTerm(t) && mod == IDB_MODULE) {
    pe = find_lu_int_key(IntegerOfTerm(t));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pe = RepPredProp(Yap_GetPredPropByFunc(fun, mod));
  } else
    return FALSE;
  if (pe == NIL)
    return FALSE;
  if (!(pe->PredFlags & LogUpdatePredFlag)) {
    /* should use '$recordedp' in this case */
    return FALSE;
  }
  return lu_statistics(pe PASS_REGS);
}

static Int p_total_erased(USES_REGS1) {
  UInt sz = 0, cls = 0;
  UInt isz = 0, icls = 0;
  LogUpdClause *cl = DBErasedList;
  LogUpdIndex *icl = DBErasedIList;

  /* only for log upds */
  while (cl) {
    cls++;
    sz += cl->ClSize;
    cl = cl->ClNext;
  }
  while (icl) {
    icls++;
    isz += icl->ClSize;
    icl = icl->SiblingIndex;
  }
  return Yap_unify(ARG1, MkIntegerTerm(cls)) &&
         Yap_unify(ARG2, MkIntegerTerm(sz)) &&
         Yap_unify(ARG3, MkIntegerTerm(icls)) &&
         Yap_unify(ARG4, MkIntegerTerm(isz));
}

static Int lu_erased_statistics(PredEntry *pe USES_REGS) {
  UInt sz = 0, cls = 0;
  UInt isz = 0, icls = 0;
  LogUpdClause *cl = DBErasedList;
  LogUpdIndex *icl = DBErasedIList;

  while (cl) {
    if (cl->ClPred == pe) {
      cls++;
      sz += cl->ClSize;
    }
    cl = cl->ClNext;
  }
  while (icl) {
    if (pe == icl->ClPred) {
      icls++;
      isz += icl->ClSize;
    }
    icl = icl->SiblingIndex;
  }
  return Yap_unify(ARG2, MkIntegerTerm(cls)) &&
         Yap_unify(ARG3, MkIntegerTerm(sz)) &&
         Yap_unify(ARG4, MkIntegerTerm(icls)) &&
         Yap_unify(ARG5, MkIntegerTerm(isz));
}

static Int p_key_erased_statistics(USES_REGS1) {
  Term twork = Deref(ARG1);
  PredEntry *pe;

  /* only for log upds */
  if ((pe = find_lu_entry(twork)) == NULL)
    return FALSE;
  return lu_erased_statistics(pe PASS_REGS);
}

static Int p_heap_space_info(USES_REGS1) {
  return Yap_unify(ARG1, MkIntegerTerm(HeapUsed)) &&
         Yap_unify(ARG2, MkIntegerTerm(HeapMax - HeapUsed)) &&
         Yap_unify(ARG3, MkIntegerTerm(Yap_expand_clauses_sz));
}

/*
 * This is called when we are erasing a data base clause, because we may have
 * pending references
 */
static void ErasePendingRefs(const DBTerm *entryref USES_REGS) {
  DBRef *cp;
  DBRef ref;

  cp = entryref->DBRefs;
  if (entryref->DBRefs == NULL)
    return;
  while ((ref = *--cp) != NULL) {
    if ((ref->Flags & DBClMask) && (--(ref->NOfRefsTo) == 0) &&
        (ref->Flags & ErasedMask))
      ErDBE(ref PASS_REGS);
  }
}

inline static void RemoveDBEntry(DBRef entryref USES_REGS) {

  ErasePendingRefs(&(entryref->DBT)PASS_REGS);
  /* We may be backtracking back to a deleted entry. If we just remove
     the space then the info on the entry may be corrupt.  */
  if ((B->cp_ap == RETRY_C_RECORDED_K_CODE ||
       B->cp_ap == RETRY_C_RECORDEDP_CODE) &&
      EXTRA_CBACK_ARG(3, 1) == (CELL)entryref) {
/* make it clear the entry has been released */
#if MULTIPLE_STACKS
    DEC_DBREF_COUNT(entryref);
#else
    entryref->Flags &= ~InUseMask;
#endif
    DBErasedMarker->Next = NULL;
    DBErasedMarker->Parent = entryref->Parent;
    DBErasedMarker->n = entryref->n;
    EXTRA_CBACK_ARG(3, 1) = (CELL)DBErasedMarker;
  }
  if (entryref->p != NULL)
    entryref->p->n = entryref->n;
  else
    entryref->Parent->F0 = entryref->n;
  if (entryref->n != NULL)
    entryref->n->p = entryref->p;
  else
    entryref->Parent->L0 = entryref->p;
  /*  Yap_LUClauseSpace -= entryref->Size; */
  FreeDBSpace((char *)entryref);
}

static yamop *find_next_clause(DBRef ref0 USES_REGS) {
  Register DBRef ref;
  yamop *newp;

/* fetch ref0 from the instruction we just started executing */
#ifdef DEBUG
  if (!(ref0->Flags & ErasedMask)) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "find_next_clause (dead clause %x)", ref0);
    return NULL;
  }
#endif
  /* search for an newer entry that is to the left and points to code */
  ref = ref0;
  while ((ref = ref->n) != NULL) {
    if (!(ref->Flags & ErasedMask))
      break;
  }
  /* no extra alternatives to try, let us leave gracefully */
  if (ref == NULL) {
    return NULL;
  } else {
    /* OK, we found a clause we can jump to, do a bit of hanky pancking with
       the choice-point, so that it believes we are actually working from that
       clause */
    newp = ref->Code;
/* and next let's tell the world this clause is being used, just
   like if we were executing a standard retry_and_mark */
#if MULTIPLE_STACKS
    {
      DynamicClause *cl = ClauseCodeToDynamicClause(newp);

      LOCK(cl->ClLock);
      TRAIL_CLREF(cl);
      INC_CLREF_COUNT(cl);
      UNLOCK(cl->ClLock);
    }
#else
    if (!(DynamicFlags(newp) & InUseMask)) {
      DynamicFlags(newp) |= InUseMask;
      TRAIL_CLREF(ClauseCodeToDynamicClause(newp));
    }
#endif
    return newp;
  }
}

/* This procedure is called when a clause is officialy deleted. Its job
   is to find out where the code can go next, if it can go anywhere */
static Int p_jump_to_next_dynamic_clause(USES_REGS1) {
  DBRef ref =
      (DBRef)(((yamop *)((CODEADDR)P - (CELL)NEXTOP((yamop *)NULL, Osbpp)))
                  ->y_u.Osbpp.bmap);
  yamop *newp = find_next_clause(ref PASS_REGS);

  if (newp == NULL) {
    cut_fail();
  }
  /* the next alternative to try must be obtained from this clause */
  B->cp_ap = newp;
  /* and next, enter the clause */
  P = NEXTOP(newp, Otapl);
  /* and return like if nothing had happened. */
  return TRUE;
}

static void complete_lu_erase(LogUpdClause *clau) {
  DBRef *cp;

  if (clau->ClFlags & FactMask)
    cp = NULL;
  else
    cp = clau->lusl.ClSource->DBRefs;
  if (CL_IN_USE(clau)) {
    return;
  }
#ifndef THREADS
  if (clau->ClNext)
    clau->ClNext->ClPrev = clau->ClPrev;
  if (clau->ClPrev) {
    clau->ClPrev->ClNext = clau->ClNext;
  } else {
    DBErasedList = clau->ClNext;
  }
#endif
  if (cp != NULL) {
    DBRef ref;
    while ((ref = *--cp) != NIL) {
      if (ref->Flags & LogUpdMask) {
        LogUpdClause *cl = (LogUpdClause *)ref;
        cl->ClRefCount--;
        if (cl->ClFlags & ErasedMask && !(cl->ClFlags & InUseMask) &&
            !(cl->ClRefCount)) {
          EraseLogUpdCl(cl);
        }
      } else {
        LOCK(ref->lock);
        ref->NOfRefsTo--;
        if (ref->Flags & ErasedMask && !(ref->Flags & InUseMask) &&
            ref->NOfRefsTo) {
          CACHE_REGS
          UNLOCK(ref->lock);
          ErDBE(ref PASS_REGS);
        } else {
          UNLOCK(ref->lock);
        }
      }
    }
  }
  Yap_InformOfRemoval(clau);
  Yap_LUClauseSpace -= clau->ClSize;
  Yap_FreeCodeSpace((char *)clau);
}

static void EraseLogUpdCl(LogUpdClause *clau) {
  PredEntry *ap;
  ap = clau->ClPred;
  /* no need to erase what has been erased */
  if (!(clau->ClFlags & ErasedMask)) {
    clau->ClFlags |= ErasedMask;
/* get ourselves out of the list */
    if (clau->ClNext != NULL) {
      clau->ClNext->ClPrev = clau->ClPrev;
    }
    if (clau->ClPrev != NULL) {
      clau->ClPrev->ClNext = clau->ClNext;
    }
    if (ap) {
      if (clau->ClCode == ap->FirstClause) {
        if (clau->ClNext == NULL) {
          ap->FirstClause = NULL;
        } else {
          ap->FirstClause = clau->ClNext->ClCode;
        }
      }
      if (clau->ClCode == ap->LastClause) {
        if (clau->ClPrev == NULL) {
          ap->LastClause = NULL;
        } else {
          ap->LastClause = clau->ClPrev->ClCode;
        }
      }
      clau->ClTimeEnd = ap->TimeStampOfPred;
      ap->NOfClauses--;
    }
#ifndef THREADS
    {
      LogUpdClause *er_head = DBErasedList;
      if (er_head == NULL) {
        clau->ClPrev = clau->ClNext = NULL;
      } else {
        clau->ClNext = er_head;
        er_head->ClPrev = clau;
        clau->ClPrev = NULL;
      }
      DBErasedList = clau;
    }
#endif
    /* we are holding a reference to the clause */
    clau->ClRefCount++;
    if (ap) {
      /* mark it as erased */
      if (ap->LastCallOfPred != LUCALL_RETRACT) {
        if (ap->NOfClauses > 1) {
          if (ap->TimeStampOfPred >= TIMESTAMP_RESET)
            Yap_UpdateTimestamps(ap);
          ++(ap->TimeStampOfPred);
          /*	  fprintf(stderr,"-
           * %x--%d--%ul\n",ap,ap->TimeStampOfPred,ap->ArityOfPE);*/
          ap->LastCallOfPred = LUCALL_RETRACT;
        } else {
/* OK, there's noone left */
#ifndef THREADS
          if (ap->NOfClauses == 0) {
            /* Other threads may hold refs to clauses */
            ap->TimeStampOfPred = 0L;
          }
#endif
          /*	  fprintf(stderr,"-
           * %x--%d--%ul\n",ap,ap->TimeStampOfPred,ap->ArityOfPE);*/
          ap->LastCallOfPred = LUCALL_ASSERT;
        }
      }
      //clau->ClTimeEnd = ap->TimeStampOfPred;
      Yap_RemoveClauseFromIndex(ap, clau->ClCode);
      /* release the extra reference */
    }
    clau->ClRefCount--;
  }
  complete_lu_erase(clau);
}

static void MyEraseClause(DynamicClause *clau USES_REGS) {
  DBRef ref;

  if (CL_IN_USE(clau))
    return;
  /*
    I don't need to lock the clause at this point because
    I am the last one using it anyway.
  */
  ref = (DBRef)NEXTOP(clau->ClCode, Otapl)->y_u.Osbpp.bmap;
  /* don't do nothing if the reference is still in use */
  if (DBREF_IN_USE(ref))
    return;
  if (P == clau->ClCode) {
    yamop *np = RTRYCODE;
    /* make it the next alternative */
    np->y_u.Otapl.d =
        find_next_clause((DBRef)(NEXTOP(P, Otapl)->y_u.Osbpp.bmap)PASS_REGS);
    if (np->y_u.Otapl.d == NULL)
      P = (yamop *)FAILCODE;
    else {
      /* with same arity as before */
      np->y_u.Otapl.s = P->y_u.Otapl.s;
      np->y_u.Otapl.p = P->y_u.Otapl.p;
      /* go ahead and try this code */
      P = np;
    }
  } else {
    Yap_InformOfRemoval(clau);
    Yap_LUClauseSpace -= clau->ClSize;
    Yap_FreeCodeSpace((char *)clau);
#ifdef DEBUG
    if (ref->NOfRefsTo)
      fprintf(stderr, "Error: references to dynamic clause\n");
#endif
    RemoveDBEntry(ref PASS_REGS);
  }
}

/*
  This predicate is supposed to be called with a
  lock on the current predicate
*/
void Yap_ErLogUpdCl(LogUpdClause *clau) { EraseLogUpdCl(clau); }

/*
  This predicate is supposed to be called with a
  lock on the current predicate
*/
void Yap_ErCl(DynamicClause *clau) {
  CACHE_REGS
  MyEraseClause(clau PASS_REGS);
}

static void PrepareToEraseLogUpdClause(LogUpdClause *clau, DBRef dbr) {
  yamop *code_p = clau->ClCode;
  PredEntry *p = clau->ClPred;
  yamop *cl = code_p;

  if (clau->ClFlags & ErasedMask) {
    return;
  }
  clau->ClFlags |= ErasedMask;
  if (p->FirstClause != cl) {
    /* we are not the first clause... */
    yamop *prev_code_p = (yamop *)(dbr->Prev->Code);
    prev_code_p->y_u.Otapl.d = code_p->y_u.Otapl.d;
    /* are we the last? */
    if (p->LastClause == cl)
      p->LastClause = prev_code_p;
  } else {
    /* we are the first clause, what about the last ? */
    if (p->LastClause == p->FirstClause) {
      p->LastClause = p->FirstClause = NULL;
    } else {
      p->FirstClause = code_p->y_u.Otapl.d;
      p->FirstClause->opc = Yap_opcode(_try_me);
    }
  }
  dbr->Code = NULL; /* unlink the two now */
  if (p->PredFlags & IndexedPredFlag) {
    p->NOfClauses--;
    Yap_RemoveIndexation(p);
  } else {
    EraseLogUpdCl(clau);
  }
  if (p->FirstClause == p->LastClause) {
    if (p->FirstClause != NULL) {
      code_p = p->FirstClause;
      code_p->y_u.Otapl.d = p->FirstClause;
      p->TrueCodeOfPred = NEXTOP(code_p, Otapl);
      if (p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
        p->OpcodeOfPred = Yap_opcode(_spy_pred);
        p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
#if defined(YAPOR) || defined(THREADS)
      } else if (p->ModuleOfPred != IDB_MODULE &&
                 !(p->PredFlags & ThreadLocalPredFlag)) {
        p->OpcodeOfPred = LOCKPRED_OPCODE;
        p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
#endif
      } else {
        p->CodeOfPred = p->TrueCodeOfPred;
        p->OpcodeOfPred = p->TrueCodeOfPred->opc;
      }
#if defined(YAPOR) || defined(THREADS)
    } else if (p->ModuleOfPred != IDB_MODULE &&
               !(p->PredFlags & ThreadLocalPredFlag)) {
      p->OpcodeOfPred = LOCKPRED_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
#endif
    } else {
      p->OpcodeOfPred = FAIL_OPCODE;
      p->TrueCodeOfPred = p->CodeOfPred =
          (yamop *)(&(p->OpcodeOfPred));
    }
  } else {
    if (p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
      p->OpcodeOfPred = Yap_opcode(_spy_pred);
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
#if defined(YAPOR) || defined(THREADS)
    } else if (p->ModuleOfPred != IDB_MODULE &&
               !(p->PredFlags & ThreadLocalPredFlag)) {
      p->OpcodeOfPred = LOCKPRED_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
#endif
    } else {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
  }
}

static void PrepareToEraseClause(DynamicClause *clau, DBRef dbr) {}

static void ErDBE(DBRef entryref USES_REGS) {

  if ((entryref->Flags & DBCode) && entryref->Code) {
    if (entryref->Flags & LogUpdMask) {
      LogUpdClause *clau = ClauseCodeToLogUpdClause(entryref->Code);
      if (CL_IN_USE(clau) || entryref->NOfRefsTo != 0) {
        PrepareToEraseLogUpdClause(clau, entryref);
      } else {
        if (!(clau->ClFlags & ErasedMask))
          PrepareToEraseLogUpdClause(clau, entryref);
        /* the clause must have left the chain */
        EraseLogUpdCl(clau);
      }
    } else {
      DynamicClause *clau = ClauseCodeToDynamicClause(entryref->Code);
      if (CL_IN_USE(clau) || entryref->NOfRefsTo != 0) {
        PrepareToEraseClause(clau, entryref);
      } else {
        if (!(clau->ClFlags & ErasedMask))
          PrepareToEraseClause(clau, entryref);
        /* the clause must have left the chain */
        MyEraseClause(clau PASS_REGS);
      }
    }
  } else if (!(DBREF_IN_USE(entryref))) {
    if (entryref->NOfRefsTo == 0)
      RemoveDBEntry(entryref PASS_REGS);
    else if (!(entryref->Flags & ErasedMask)) {
      /* oops, I cannot remove it, but I at least have to tell
         the world what's going on */
      entryref->Flags |= ErasedMask;
      entryref->Next = entryref->Prev = NIL;
    }
  }
}

void Yap_ErDBE(DBRef entryref) {
  CACHE_REGS
  ErDBE(entryref PASS_REGS);
}

static void EraseEntry(DBRef entryref) {
  DBProp p;

  if (entryref->Flags & ErasedMask)
    return;
  if (entryref->Flags & LogUpdMask && !(entryref->Flags & DBClMask)) {
    LogUpdClause *luclause = (LogUpdClause *)entryref;
    PELOCK(67, luclause->ClPred);
    EraseLogUpdCl(luclause);
    UNLOCK(luclause->ClPred->PELock);
    return;
  }
  entryref->Flags |= ErasedMask;
  /* update FirstNEr */
  p = entryref->Parent;
  /* exit the db chain */
  if (entryref->Next != NIL) {
    entryref->Next->Prev = entryref->Prev;
  } else {
    p->Last = entryref->Prev;
  }
  if (entryref->Prev != NIL)
    entryref->Prev->Next = entryref->Next;
  else
    p->First = entryref->Next;
  /* make sure we know the entry has been removed from the list */
  entryref->Next = NIL;
  if (!DBREF_IN_USE(entryref)) {
    CACHE_REGS
    ErDBE(entryref PASS_REGS);
  } else if ((entryref->Flags & DBCode) && entryref->Code) {
    PrepareToEraseClause(ClauseCodeToDynamicClause(entryref->Code), entryref);
  }
}

/* erase(+Ref)	 */
static Int p_erase(USES_REGS1) {
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "erase");
    return FALSE;
  }
  if (!IsDBRefTerm(t1)) {
    Yap_Error(TYPE_ERROR_DBREF, t1, "erase");
    return FALSE;
  }
  EraseEntry(DBRefOfTerm(t1));
  return TRUE;
}

/* increase_reference_counter(+Ref)	 */
static Int p_increase_reference_counter(USES_REGS1) {
  Term t1 = Deref(ARG1);
  LogUpdClause *cl;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "increase_reference_counter/1");
    return FALSE;
  }
  if (!IsDBRefTerm(t1)) {
    Yap_Error(TYPE_ERROR_DBREF, t1, "increase_reference_counter");
    return FALSE;
  }
  cl = (LogUpdClause *)DBRefOfTerm(t1);
  PELOCK(67, cl->ClPred);
  cl->ClRefCount++;
  UNLOCK(cl->ClPred->PELock);
  return TRUE;
}

/* increase_reference_counter(+Ref)	 */
static Int p_decrease_reference_counter(USES_REGS1) {
  Term t1 = Deref(ARG1);
  LogUpdClause *cl;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "increase_reference_counter/1");
    return FALSE;
  }
  if (!IsDBRefTerm(t1)) {
    Yap_Error(TYPE_ERROR_DBREF, t1, "increase_reference_counter");
    return FALSE;
  }
  cl = (LogUpdClause *)DBRefOfTerm(t1);
  PELOCK(67, cl->ClPred);
  if (cl->ClRefCount) {
    cl->ClRefCount--;
    UNLOCK(cl->ClPred->PELock);
    return TRUE;
  }
  UNLOCK(cl->ClPred->PELock);
  return FALSE;
}

/* erase(+Ref)	 */
/** @pred  erase(+ _R_)


The term referred to by  _R_ is erased from the internal database. If
reference  _R_ does not exist in the database, `erase` just fails.


*/
static Int p_current_reference_counter(USES_REGS1) {
  Term t1 = Deref(ARG1);
  LogUpdClause *cl;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "increase_reference_counter/1");
    return FALSE;
  }
  if (!IsDBRefTerm(t1)) {
    Yap_Error(TYPE_ERROR_DBREF, t1, "increase_reference_counter");
    return FALSE;
  }
  cl = (LogUpdClause *)DBRefOfTerm(t1);
  return Yap_unify(ARG2, MkIntegerTerm(cl->ClRefCount));
}

static Int p_erase_clause(USES_REGS1) {
  Term t1 = Deref(ARG1);
  DBRef entryref;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "erase");
    return FALSE;
  }
  if (!IsDBRefTerm(t1)) {
    if (IsApplTerm(t1)) {
      if (FunctorOfTerm(t1) == FunctorStaticClause) {
        Yap_EraseStaticClause(Yap_ClauseFromTerm(t1),
                              (PredEntry *)IntegerOfTerm(ArgOfTerm(2, t1)),
                              Deref(ARG2));
        return TRUE;
      }
      if (FunctorOfTerm(t1) == FunctorMegaClause) {
        Yap_EraseMegaClause(Yap_MegaClauseFromTerm(t1),
                            Yap_MegaClausePredicateFromTerm(t1));
        return TRUE;
      }
      if (FunctorOfTerm(t1) == FunctorExoClause) {
        Yap_Error(TYPE_ERROR_DBREF, t1, "erase exo clause");
        return FALSE;
      }
    }
    Yap_Error(TYPE_ERROR_DBREF, t1, "erase");
    return FALSE;
  } else {
    entryref = DBRefOfTerm(t1);
  }
  EraseEntry(entryref);
  return TRUE;
}

/* eraseall(+Key)	 */
/** @pred  eraseall(+ _K_)

All terms belonging to the key `K` are erased from the internal
database. The predicate always succeeds.

*/
static Int p_eraseall(USES_REGS1) {
  Register Term twork = Deref(ARG1);
  Register DBRef entryref;
  DBProp p;
  PredEntry *pe;

  if ((pe = find_lu_entry(twork)) != NULL) {
    LogUpdClause *cl;

    if (!pe->NOfClauses)
      return TRUE;
    if (pe->PredFlags & IndexedPredFlag)
      Yap_RemoveIndexation(pe);
    cl = ClauseCodeToLogUpdClause(pe->FirstClause);
    do {
      LogUpdClause *ncl = cl->ClNext;
      Yap_ErLogUpdCl(cl);
      cl = ncl;
    } while (cl != NULL);
    return TRUE;
  }
  if (EndOfPAEntr(p = FetchDBPropFromKey(twork, 0, FALSE, "eraseall/3"))) {
    return TRUE;
  }
  WRITE_LOCK(p->DBRWLock);
  entryref = FrstDBRef(p);
  do {
    DBRef next_entryref;

    while (entryref != NIL && (entryref->Flags & (DBCode | ErasedMask)))
      entryref = NextDBRef(entryref);
    if (entryref == NIL)
      break;
    next_entryref = NextDBRef(entryref);
    /* exit the db chain */
    if (entryref->Next != NIL) {
      entryref->Next->Prev = entryref->Prev;
    } else {
      p->Last = entryref->Prev;
    }
    if (entryref->Prev != NIL)
      entryref->Prev->Next = entryref->Next;
    else
      p->First = entryref->Next;
    /* make sure we know the entry has been removed from the list */
    entryref->Next = entryref->Prev = NIL;
    if (!DBREF_IN_USE(entryref))
      ErDBE(entryref PASS_REGS);
    else {
      entryref->Flags |= ErasedMask;
    }
    entryref = next_entryref;
  } while (entryref != NIL);
  WRITE_UNLOCK(p->DBRWLock);
  return (TRUE);
}

/* erased(+Ref) */
/** @pred  erased(+ _R_)


Succeeds if the object whose database reference is  _R_ has been
erased.


*/
static Int p_erased(USES_REGS1) {
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "erased");
    return (FALSE);
  }
  if (!IsDBRefTerm(t)) {
    Yap_Error(TYPE_ERROR_DBREF, t, "erased");
    return (FALSE);
  }
  return (DBRefOfTerm(t)->Flags & ErasedMask);
}

static Int static_instance(StaticClause *cl, PredEntry *ap USES_REGS) {
  if (cl->ClFlags & ErasedMask) {
    return FALSE;
  }
  if (cl->ClFlags & FactMask) {
    if (ap->ArityOfPE == 0) {
      return Yap_unify(ARG2, MkAtomTerm((Atom)ap->FunctorOfPred));
    } else {
      Functor f = ap->FunctorOfPred;
      UInt arity = ArityOfFunctor(ap->FunctorOfPred), i;
      Term t2 = Deref(ARG2);
      CELL *ptr;

      if (IsVarTerm(t2)) {
        Yap_unify(ARG2, (t2 = Yap_MkNewApplTerm(f, arity)));
      } else if (!IsApplTerm(t2) || FunctorOfTerm(t2) != f) {
        return FALSE;
      }
      ptr = RepAppl(t2) + 1;
      for (i = 0; i < arity; i++) {
        XREGS[i + 1] = ptr[i];
      }
      CP = P;
      YENV = ASP;
      YENV[E_CB] = (CELL)B;
      P = cl->ClCode;
      return TRUE;
    }
  } else {
    Term TermDB;

    while ((TermDB = GetDBTerm(cl->usc.ClSource, TRUE PASS_REGS)) == 0L) {
      /* oops, we are in trouble, not enough stack space */
      if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_growglobal(NULL)) {
          Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                    LOCAL_ErrorMessage);
          return FALSE;
        }
      } else {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_gcl(LOCAL_Error_Size, 2, ENV, gc_P(P, CP))) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
    }
    return Yap_unify(ARG2, TermDB);
  }
}

static Int exo_instance(Int i, PredEntry *ap USES_REGS) {
  if (ap->ArityOfPE == 0) {
    return Yap_unify(ARG2, MkAtomTerm((Atom)ap->FunctorOfPred));
  } else {
    MegaClause *mcl = ClauseCodeToMegaClause(ap->FirstClause);
    Functor f = ap->FunctorOfPred;
    UInt arity = ArityOfFunctor(ap->FunctorOfPred);
    Term t2 = Deref(ARG2);
    CELL *ptr = (CELL *)((ADDR)mcl->ClCode + 2 * sizeof(struct index_t *) +
                         i * (mcl->ClItemSize));
    if (IsVarTerm(t2)) {
      // fresh slate
      t2 = Yap_MkApplTerm(f, arity, ptr);
      Yap_unify(ARG2, t2);
    } else if (!IsApplTerm(t2) || FunctorOfTerm(t2) != f) {
      return FALSE;
    }
    for (i = 0; i < arity; i++) {
      XREGS[i + 1] = ptr[i];
    }
    S = ptr;
    CP = P;
    YENV = ASP;
    YENV[E_CB] = (CELL)B;
    P = mcl->ClCode;
    return TRUE;
  }
}

static Int mega_instance(yamop *code, PredEntry *ap USES_REGS) {
  if (ap->ArityOfPE == 0) {
    return Yap_unify(ARG2, MkAtomTerm((Atom)ap->FunctorOfPred));
  } else {
    Functor f = ap->FunctorOfPred;
    UInt arity = ArityOfFunctor(ap->FunctorOfPred), i;
    Term t2 = Deref(ARG2);
    CELL *ptr;

    if (IsVarTerm(t2)) {
      t2 = Yap_MkNewApplTerm(f, arity);
      Yap_unify(ARG2, t2);
    } else if (!IsApplTerm(t2) || FunctorOfTerm(t2) != f) {
      return FALSE;
    }
    ptr = RepAppl(t2) + 1;
    for (i = 0; i < arity; i++) {
      XREGS[i + 1] = ptr[i];
    }
    CP = P;
    YENV = ASP;
    YENV[E_CB] = (CELL)B;
    P = code;
    return TRUE;
  }
}

/* instance(+Ref,?Term) */
/** @pred  instance(+ _R_,- _T_)


If  _R_ refers to a clause or a recorded term,  _T_ is unified
with its most general instance. If  _R_ refers to an unit clause
 _C_, then  _T_ is unified with ` _C_ :- true`. When
 _R_ is not a reference to an existing clause or to a recorded term,
this goal fails.


*/
static Int p_instance(USES_REGS1) {
  Term t1 = Deref(ARG1);
  DBRef dbr;

  if (IsVarTerm(t1) || !IsDBRefTerm(t1)) {
    if (IsApplTerm(t1)) {
      if (FunctorOfTerm(t1) == FunctorStaticClause) {
        return static_instance(Yap_ClauseFromTerm(t1),
                               (PredEntry *)IntegerOfTerm(ArgOfTerm(2, t1))
                                   PASS_REGS);
      }
      if (FunctorOfTerm(t1) == FunctorMegaClause) {
        return mega_instance(Yap_MegaClauseFromTerm(t1),
                             Yap_MegaClausePredicateFromTerm(t1) PASS_REGS);
      }
      if (FunctorOfTerm(t1) == FunctorExoClause) {
        return exo_instance(Yap_ExoClauseFromTerm(t1),
                            Yap_ExoClausePredicateFromTerm(t1) PASS_REGS);
      }
    }
    return FALSE;
  } else {
    dbr = DBRefOfTerm(t1);
  }
  if (dbr->Flags & LogUpdMask) {
    op_numbers opc;
    LogUpdClause *cl = (LogUpdClause *)dbr;
    PredEntry *ap = cl->ClPred;

    PELOCK(68, ap);
    if (cl->ClFlags & ErasedMask) {
      UNLOCK(ap->PELock);
      return FALSE;
    }
    if (cl->ClFlags & FactMask) {
      if (ap->ArityOfPE == 0) {
        UNLOCK(ap->PELock);
        return Yap_unify(ARG2, MkAtomTerm((Atom)ap->FunctorOfPred));
      } else {
        Functor f = ap->FunctorOfPred;
        UInt arity = ArityOfFunctor(ap->FunctorOfPred), i;
        Term t2 = Deref(ARG2);
        CELL *ptr;

        if (IsVarTerm(t2)) {
          Yap_unify(ARG2, (t2 = Yap_MkNewApplTerm(f, arity)));
        } else if (!IsApplTerm(t2) || FunctorOfTerm(t2) != f) {
          UNLOCK(ap->PELock);
          return FALSE;
        }
        ptr = RepAppl(t2) + 1;
        for (i = 0; i < arity; i++) {
          XREGS[i + 1] = ptr[i];
        }
        CP = P;
        YENV = ASP;
        YENV[E_CB] = (CELL)B;
        P = cl->ClCode;
#if defined(YAPOR) || defined(THREADS)
        if (ap->PredFlags & ThreadLocalPredFlag) {
          UNLOCK(ap->PELock);
        } else {
          PP = ap;
        }
#endif
        return TRUE;
      }
    }
    opc = Yap_op_from_opcode(cl->ClCode->opc);
    if (opc == _unify_idb_term) {
      UNLOCK(ap->PELock);
      return Yap_unify(ARG2, cl->lusl.ClSource->Entry);
    } else {
      Term TermDB;
      int in_cl = (opc != _copy_idb_term);

      while ((TermDB = GetDBTerm(cl->lusl.ClSource, in_cl PASS_REGS)) == 0L) {
        /* oops, we are in trouble, not enough stack space */
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_growglobal(NULL)) {
            Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                      LOCAL_ErrorMessage);
            UNLOCK(ap->PELock);
            return FALSE;
          }
        } else {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_gcl(LOCAL_Error_Size, 2, ENV, gc_P(P, CP))) {
            Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            UNLOCK(ap->PELock);
            return FALSE;
          }
        }
      }
      UNLOCK(ap->PELock);
      return Yap_unify(ARG2, TermDB);
    }
  } else {
    Term TermDB;
    while ((TermDB = GetDBTermFromDBEntry(dbr PASS_REGS)) == 0L) {
      /* oops, we are in trouble, not enough stack space */
      if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_growglobal(NULL)) {
          Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                    LOCAL_ErrorMessage);
          return FALSE;
        }
      } else {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_gcl(LOCAL_Error_Size, 2, ENV, gc_P(P, CP))) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
      t1 = Deref(ARG1);
    }
    return Yap_unify(ARG2, TermDB);
  }
}

Term Yap_LUInstance(LogUpdClause *cl, UInt arity) {
  CACHE_REGS
  Term TermDB;
  op_numbers opc = Yap_op_from_opcode(cl->ClCode->opc);

  if (opc == _unify_idb_term) {
    TermDB = cl->lusl.ClSource->Entry;
  } else {
    CACHE_REGS
    int in_src;

    in_src = (opc != _copy_idb_term);
    while ((TermDB = GetDBTerm(cl->lusl.ClSource, in_src PASS_REGS)) == 0L) {
      /* oops, we are in trouble, not enough stack space */
      if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_growglobal(NULL)) {
          Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                    LOCAL_ErrorMessage);
          return 0L;
        }
      } else {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_gcl(LOCAL_Error_Size, arity, ENV, gc_P(P, CP))) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return 0L;
        }
      }
    }
  }
#if MULTIPLE_STACKS
  cl->ClRefCount++;
  TRAIL_CLREF(cl); /* So that fail will erase it */
#else
  if (!(cl->ClFlags & InUseMask)) {
    cl->ClFlags |= InUseMask;
    TRAIL_CLREF(cl);
  }
#endif
  return TermDB;
}

/* instance(+Ref,?Term) */
static Int p_instance_module(USES_REGS1) {
  Term t1 = Deref(ARG1);
  DBRef dbr;

  if (IsVarTerm(t1)) {
    return FALSE;
  }
  if (IsDBRefTerm(t1)) {
    dbr = DBRefOfTerm(t1);
  } else {
    return FALSE;
  }
  if (dbr->Flags & LogUpdMask) {
    LogUpdClause *cl = (LogUpdClause *)dbr;

    if (cl->ClFlags & ErasedMask) {
      return FALSE;
    }
    if (cl->ClPred->ModuleOfPred)
      return Yap_unify(ARG2, cl->ClPred->ModuleOfPred);
    else
      return Yap_unify(ARG2, TermProlog);
  } else {
    return Yap_unify(ARG2, dbr->Parent->ModuleOfDB);
  }
}

inline static int NotActiveDB(DBRef my_dbref) {
  while (my_dbref && (my_dbref->Flags & (DBCode | ErasedMask)))
    my_dbref = my_dbref->Next;
  return (my_dbref == NIL);
}

inline static DBEntry *NextDBProp(PropEntry *pp) {
  while (!EndOfPAEntr(pp) && (((pp->KindOfPE & ~0x1) != DBProperty) ||
                              NotActiveDB(((DBProp)pp)->First)))
    pp = RepProp(pp->NextOfPE);
  return ((DBEntry *)pp);
}

static Int init_current_key(USES_REGS1) { /* current_key(+Atom,?key)	 */
  Int i = 0;
  DBEntry *pp;
  Atom a;
  Term t1 = ARG1;

  t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    if (IsAtomTerm(t1))
      a = AtomOfTerm(t1);
    else {
      cut_fail();
    }
  } else {
    /* ask for the first hash line */
    while (TRUE) {
      READ_LOCK(HashChain[i].AERWLock);
      a = HashChain[i].Entry;
      if (a != NIL) {
        break;
      }
      READ_UNLOCK(HashChain[i].AERWLock);
      i++;
    }
    READ_UNLOCK(HashChain[i].AERWLock);
  }
  READ_LOCK(RepAtom(a)->ARWLock);
  pp = NextDBProp(RepProp(RepAtom(a)->PropsOfAE));
  READ_UNLOCK(RepAtom(a)->ARWLock);
  EXTRA_CBACK_ARG(2, 3) = MkAtomTerm(a);
  EXTRA_CBACK_ARG(2, 2) = MkIntTerm(i);
  EXTRA_CBACK_ARG(2, 1) = MkIntegerTerm((Int)pp);
  return cont_current_key(PASS_REGS1);
}

static Int cont_current_key(USES_REGS1) {
  unsigned int arity;
  Functor functor;
  Term term, AtT;
  Atom a;
  Int i = IntegerOfTerm(EXTRA_CBACK_ARG(2, 2));
  Term first = Deref(ARG1);
  DBEntry *pp = (DBEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(2, 1));

  if (IsIntTerm(term = EXTRA_CBACK_ARG(2, 3)))
    return cont_current_key_integer(PASS_REGS1);
  a = AtomOfTerm(term);
  if (EndOfPAEntr(pp) && IsAtomTerm(first)) {
    cut_fail();
  }
  while (EndOfPAEntr(pp)) {
    UInt j;

    if ((a = RepAtom(a)->NextOfAE) == NIL) {
      i++;
      while (i < AtomHashTableSize) {
        /* protect current hash table line, notice that the current
           LOCK/UNLOCK algorithm assumes new entries are added to
           the *front* of the list, otherwise I should have locked
           earlier.
        */
        READ_LOCK(HashChain[i].AERWLock);
        a = HashChain[i].Entry;
        if (a != NIL) {
          break;
        }
        /* move to next entry */
        READ_UNLOCK(HashChain[i].AERWLock);
        i++;
      }
      if (i == AtomHashTableSize) {
        /* we have left the atom hash table */
        /* we don't have a lock over the hash table any longer */
        if (IsAtomTerm(first)) {
          cut_fail();
        }
        j = 0;
        if (INT_KEYS == NULL) {
          cut_fail();
        }
        for (j = 0; j < INT_KEYS_SIZE; j++) {
          if (INT_KEYS[j] != NIL) {
            DBProp pptr = RepDBProp(INT_KEYS[j]);
            EXTRA_CBACK_ARG(2, 1) = MkIntegerTerm((Int)(pptr->NextOfPE));
            EXTRA_CBACK_ARG(2, 2) = MkIntegerTerm(j + 1);
            EXTRA_CBACK_ARG(2, 3) = MkIntTerm(INT_KEYS_TIMESTAMP);
            term = MkIntegerTerm((Int)(pptr->FunctorOfDB));
            return Yap_unify(term, ARG1) && Yap_unify(term, ARG2);
          }
        }
        if (j == INT_KEYS_SIZE) {
          cut_fail();
        }
        return cont_current_key_integer(PASS_REGS1);
      } else {
        /* release our lock over the hash table */
        READ_UNLOCK(HashChain[i].AERWLock);
        EXTRA_CBACK_ARG(2, 2) = MkIntTerm(i);
      }
    }
    READ_LOCK(RepAtom(a)->ARWLock);
    if (!EndOfPAEntr(pp = NextDBProp(RepProp(RepAtom(a)->PropsOfAE))))
      EXTRA_CBACK_ARG(2, 3) = (CELL)MkAtomTerm(a);
    READ_UNLOCK(RepAtom(a)->ARWLock);
  }
  READ_LOCK(RepAtom(a)->ARWLock);
  EXTRA_CBACK_ARG(2, 1) = MkIntegerTerm((Int)NextDBProp(RepProp(pp->NextOfPE)));
  READ_UNLOCK(RepAtom(a)->ARWLock);
  arity = (unsigned int)(pp->ArityOfDB);
  if (arity == 0) {
    term = AtT = MkAtomTerm(a);
  } else {
    unsigned int j;
    CELL *p = HR;

    for (j = 0; j < arity; j++) {
      p[j] = MkVarTerm();
    }
    functor = Yap_MkFunctor(a, arity);
    term = Yap_MkApplTerm(functor, arity, p);
    AtT = MkAtomTerm(a);
  }
  return (Yap_unify_constant(ARG1, AtT) && Yap_unify(ARG2, term));
}

static Int cont_current_key_integer(USES_REGS1) {
  Term term;
  UInt i = IntOfTerm(EXTRA_CBACK_ARG(2, 2));
  Prop pp = (Prop)IntegerOfTerm(EXTRA_CBACK_ARG(2, 1));
  UInt tstamp = (UInt)IntOfTerm(EXTRA_CBACK_ARG(2, 3));
  DBProp pptr;

  if (tstamp != INT_KEYS_TIMESTAMP) {
    cut_fail();
  }
  while (pp == NIL) {
    for (; i < INT_KEYS_SIZE; i++) {
      if (INT_KEYS[i] != NIL) {
        EXTRA_CBACK_ARG(2, 2) = MkIntTerm(i + 1);
        pp = INT_KEYS[i];
        break;
      }
    }
    if (i == INT_KEYS_SIZE) {
      cut_fail();
    }
  }
  pptr = RepDBProp(pp);
  EXTRA_CBACK_ARG(2, 1) = MkIntegerTerm((Int)(pptr->NextOfPE));
  term = MkIntegerTerm((Int)(pptr->FunctorOfDB));
  return Yap_unify(term, ARG1) && Yap_unify(term, ARG2);
}

Term Yap_FetchTermFromDB(const void *ref) {
  CACHE_REGS
    if (ref == NULL)
      return 0;
  return GetDBTerm(ref, FALSE PASS_REGS);
}

Term Yap_FetchClauseTermFromDB(const void *ref) {
  CACHE_REGS
    if (ref == NULL)
      return 0;
  return GetDBTerm(ref, TRUE PASS_REGS);
}

Term Yap_PopTermFromDB(const void *ref) {
  CACHE_REGS

  Term t = GetDBTerm(ref, FALSE PASS_REGS);
  if (t != 0L)
    ReleaseTermFromDB(ref PASS_REGS);
  return t;
}

static DBTerm *StoreTermInDB(Term t, int nargs USES_REGS) {
  DBTerm *x;
  int needs_vars;
  struct db_globs dbg;

  LOCAL_Error_Size = 0;
  while ((x = (DBTerm *)CreateDBStruct(t, (DBProp)NULL, InQueue, &needs_vars, 0,
                                       &dbg)) == NULL) {
    if (LOCAL_Error_TYPE == YAP_NO_ERROR) {
      break;
    } else if (nargs == -1) {
      return NULL;
    } else {
      XREGS[nargs + 1] = t;
      if (recover_from_record_error(nargs + 1)) {
        t = Deref(XREGS[nargs + 1]);
      } else {
        return NULL;
      }
    }
  }
  return x;
}

DBTerm *Yap_StoreTermInDB(Term t, int nargs) {
  CACHE_REGS
  return StoreTermInDB(t, nargs PASS_REGS);
}

DBTerm *Yap_StoreTermInDBPlusExtraSpace(Term t, UInt extra_size, UInt *sz) {
  CACHE_REGS
  int needs_vars;
  struct db_globs dbg;
  DBTerm *o;

  o = (DBTerm *)CreateDBStruct(t, (DBProp)NULL, InQueue, &needs_vars,
                               extra_size, &dbg);
  *sz = dbg.sz;
  return o;
}

void Yap_init_tqueue(db_queue *dbq) {
  dbq->id = FunctorDBRef;
  dbq->Flags = DBClMask;
  dbq->FirstInQueue = dbq->LastInQueue = NULL;
  INIT_RWLOCK(dbq->QRWLock);
}

void Yap_destroy_tqueue(db_queue *dbq USES_REGS) {
  QueueEntry *cur_instance = dbq->FirstInQueue;
  while (cur_instance) {
    /* release space for cur_instance */
    keepdbrefs(cur_instance->DBT PASS_REGS);
    ErasePendingRefs(cur_instance->DBT PASS_REGS);
    FreeDBSpace((char *)cur_instance->DBT);
    FreeDBSpace((char *)cur_instance);
  }
  dbq->FirstInQueue = dbq->LastInQueue = NULL;
}

bool Yap_enqueue_tqueue(db_queue *father_key, Term t USES_REGS) {
  QueueEntry *x;
  while ((x = (QueueEntry *)AllocDBSpace(sizeof(QueueEntry))) == NULL) {
    if (!Yap_growheap(FALSE, sizeof(QueueEntry), NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "in findall");
      return false;
    }
  }
  /* Yap_LUClauseSpace += sizeof(QueueEntry); */
  x->DBT = StoreTermInDB(Deref(t), 2 PASS_REGS);
  if (x->DBT == NULL) {
    return false;
  }
  x->next = NULL;
  if (father_key->LastInQueue != NULL)
    father_key->LastInQueue->next = x;
  father_key->LastInQueue = x;
  if (father_key->FirstInQueue == NULL) {
    father_key->FirstInQueue = x;
  }
  return true;
}

bool Yap_dequeue_tqueue(db_queue *father_key, Term t, bool first,
                        bool release USES_REGS) {
  Term TDB;
  CELL *oldH = HR;
  tr_fr_ptr oldTR = TR;
  QueueEntry *cur_instance = father_key->FirstInQueue, *prev = NULL;
  while (cur_instance) {
    HR = oldH;
    HB = LCL0;
    while ((TDB = GetDBTerm(cur_instance->DBT, false PASS_REGS)) == 0L) {
      if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_growglobal(NULL)) {
          Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                    LOCAL_ErrorMessage);
          return false;
        }
      } else {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_gcl(LOCAL_Error_Size, 2, ENV, gc_P(P, CP))) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return false;
        }
      }
      oldTR = TR;
      oldH = HR;
    }
    if (Yap_unify(t, TDB)) {
      if (release) {
        if (cur_instance == father_key->FirstInQueue) {
          father_key->FirstInQueue = cur_instance->next;
        }
        if (cur_instance == father_key->LastInQueue) {
          father_key->LastInQueue = prev;
        }
        if (prev) {
          prev->next = cur_instance->next;
        }
        /* release space for cur_instance */
        keepdbrefs(cur_instance->DBT PASS_REGS);
        ErasePendingRefs(cur_instance->DBT PASS_REGS);
        FreeDBSpace((char *)cur_instance->DBT);
        FreeDBSpace((char *)cur_instance);
      } else {
        // undo if you'rejust peeking
        while (oldTR < TR) {
          CELL d1 = TrailTerm(TR - 1);
          TR--;
          /* normal variable */
          RESET_VARIABLE(d1);
        }
      }
      return true;
    } else {
      // just getting the first
      if (first)
        return false;
      // but keep on going, if we want to check everything.
      prev = cur_instance;
      cur_instance = cur_instance->next;
    }
  }
  return false;
}

static Int p_init_queue(USES_REGS1) {
  db_queue *dbq;
  Term t;

  while ((dbq = (db_queue *)AllocDBSpace(sizeof(db_queue))) == NULL) {
    if (!Yap_growheap(FALSE, sizeof(db_queue), NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "in findall");
      return FALSE;
    }
  }
  /* Yap_LUClauseSpace += sizeof(db_queue); */
  Yap_init_tqueue(dbq);
  t = MkIntegerTerm((Int)dbq);
  return Yap_unify(ARG1, t);
}

static Int p_enqueue(USES_REGS1) {
  Term Father = Deref(ARG1);
  db_queue *father_key;
  bool rc;

  if (IsVarTerm(Father)) {
    Yap_Error(INSTANTIATION_ERROR, Father, "enqueue");
    return FALSE;
  } else if (!IsIntegerTerm(Father)) {
    Yap_Error(TYPE_ERROR_INTEGER, Father, "enqueue");
    return FALSE;
  } else
    father_key = (db_queue *)IntegerOfTerm(Father);
  WRITE_LOCK(father_key->QRWLock);
  rc = Yap_enqueue_tqueue(father_key, Deref(ARG2) PASS_REGS);
  WRITE_UNLOCK(father_key->QRWLock);
  return rc;
}

static Int p_enqueue_unlocked(USES_REGS1) {
  Term Father = Deref(ARG1);
  db_queue *father_key;

  if (IsVarTerm(Father)) {
    Yap_Error(INSTANTIATION_ERROR, Father, "enqueue");
    return FALSE;
  } else if (!IsIntegerTerm(Father)) {
    Yap_Error(TYPE_ERROR_INTEGER, Father, "enqueue");
    return FALSE;
  } else
    father_key = (db_queue *)IntegerOfTerm(Father);
  return Yap_enqueue_tqueue(father_key, Deref(ARG2) PASS_REGS);
}

/* when reading an entry in the data base we are making it accessible from
   the outside. If the entry was removed, and this was the last pointer, the
   target entry would be immediately removed, leading to dangling pointers.
   We avoid this problem by making every entry accessible.

   Note that this could not happen with recorded, because the original db
   entry itself is still accessible from a trail entry, so we could not remove
   the target entry,
 */
static void keepdbrefs (const DBTerm *entryref USES_REGS) {
  DBRef *cp;
  DBRef ref;

  cp = entryref->DBRefs;
  if (cp == NULL) {
    return;
  }
  while ((ref = *--cp) != NIL) {
    if (!(ref->Flags & LogUpdMask)) {
      LOCK(ref->lock);
      if (!(ref->Flags & InUseMask)) {
        ref->Flags |= InUseMask;
        TRAIL_REF(ref); /* So that fail will erase it */
      }
      UNLOCK(ref->lock);
    }
  }
}

static Int p_dequeue(USES_REGS1) {
  db_queue *father_key;
  QueueEntry *cur_instance;
  Term Father = Deref(ARG1);
  Int rc;

  if (IsVarTerm(Father)) {
    Yap_Error(INSTANTIATION_ERROR, Father, "dequeue");
    return FALSE;
  } else if (!IsIntegerTerm(Father)) {
    Yap_Error(TYPE_ERROR_INTEGER, Father, "dequeue");
    return FALSE;
  } else {
    father_key = (db_queue *)IntegerOfTerm(Father);
    WRITE_LOCK(father_key->QRWLock);
    if ((cur_instance = father_key->FirstInQueue) == NULL) {
      /* an empty queue automatically goes away */
      WRITE_UNLOCK(father_key->QRWLock);
      FreeDBSpace((char *)father_key);
      return false;
    }
    rc = Yap_dequeue_tqueue(father_key, ARG2, true, true PASS_REGS);
    WRITE_UNLOCK(father_key->QRWLock);
    return rc;
  }
}

static Int p_dequeue_unlocked(USES_REGS1) {
  db_queue *father_key;
  QueueEntry *cur_instance;
  Term Father = Deref(ARG1);

  if (IsVarTerm(Father)) {
    Yap_Error(INSTANTIATION_ERROR, Father, "dequeue");
    return FALSE;
  } else if (!IsIntegerTerm(Father)) {
    Yap_Error(TYPE_ERROR_INTEGER, Father, "dequeue");
    return FALSE;
  } else {
    father_key = (db_queue *)IntegerOfTerm(Father);
    if ((cur_instance = father_key->FirstInQueue) == NULL) {
      /* an empty queue automatically goes away */
      FreeDBSpace((char *)father_key);
      return FALSE;
    }
    return Yap_dequeue_tqueue(father_key, ARG2, true, true PASS_REGS);
  }
}

static Int p_peek_queue(USES_REGS1) {
  db_queue *father_key;
  QueueEntry *cur_instance;
  Term Father = Deref(ARG1);

  if (IsVarTerm(Father)) {
    Yap_Error(INSTANTIATION_ERROR, Father, "dequeue");
    return FALSE;
  } else if (!IsIntegerTerm(Father)) {
    Yap_Error(TYPE_ERROR_INTEGER, Father, "dequeue");
    return FALSE;
  } else {
    father_key = (db_queue *)IntegerOfTerm(Father);
    if ((cur_instance = father_key->FirstInQueue) == NULL) {
      /* an empty queue automatically goes away */
      FreeDBSpace((char *)father_key);
      return FALSE;
    }
    if (!Yap_dequeue_tqueue(father_key, ARG2, true, false PASS_REGS))
      return FALSE;
    if (cur_instance == father_key->LastInQueue)
      father_key->FirstInQueue = father_key->LastInQueue = NULL;
    else
      father_key->FirstInQueue = cur_instance->next;
    return TRUE;
  }
}

static Int p_clean_queues(USES_REGS1) { return TRUE; }

/* set the logical updates flag */
static Int p_slu(USES_REGS1) {
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "switch_logical_updates/1");
    return FALSE;
  }
  if (!IsIntTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER, t, "switch_logical_updates/1");
    return FALSE;
  }
  UPDATE_MODE = IntOfTerm(t);
  return TRUE;
}

/* get a hold over the index table for logical update predicates */
static Int p_hold_index(USES_REGS1) {
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "hold_index in debugger");
  return FALSE;
}

static Int p_fetch_reference_from_index(USES_REGS1) {
  Term t1 = Deref(ARG1), t2 = Deref(ARG2);
  DBRef table, el;
  Int pos;

  if (IsVarTerm(t1) || !IsDBRefTerm(t1))
    return FALSE;
  table = DBRefOfTerm(t1);

  if (IsVarTerm(t2) || !IsIntTerm(t2))
    return FALSE;
  pos = IntOfTerm(t2);
  el = (DBRef)(table->DBT.Contents[pos]);
  LOCK(el->lock);
#if MULTIPLE_STACKS
  TRAIL_REF(el); /* So that fail will erase it */
  INC_DBREF_COUNT(el);
#else
  if (!(el->Flags & InUseMask)) {
    el->Flags |= InUseMask;
    TRAIL_REF(el);
  }
#endif
  UNLOCK(el->lock);
  return Yap_unify(ARG3, MkDBRefTerm(el));
}

static Int p_resize_int_keys(USES_REGS1) {
  Term t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    return Yap_unify(ARG1, MkIntegerTerm((Int)INT_KEYS_SIZE));
  }
  if (!IsIntegerTerm(t1)) {
    Yap_Error(TYPE_ERROR_INTEGER, t1, "yap_flag(resize_db_int_keys,T)");
    return FALSE;
  }
  return resize_int_keys(IntegerOfTerm(t1));
}

static void ReleaseTermFromDB(const DBTerm *ref USES_REGS) {
  if (!ref)
    return;
  keepdbrefs(ref PASS_REGS);
  ErasePendingRefs(ref PASS_REGS);
  FreeDBSpace((char *)ref);
}

void Yap_ReleaseTermFromDB(const void *ref) {
  CACHE_REGS
  ReleaseTermFromDB(ref PASS_REGS);
}

static Int p_install_thread_local(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);

  if (IsVarTerm(t)) {
    return (FALSE);
  }
  if (mod == IDB_MODULE) {
    pe = find_lu_entry(t);
    if (!pe->NOfClauses) {
      if (IsIntegerTerm(t))
        pe->PredFlags |= LogUpdatePredFlag | NumberDBPredFlag;
      else if (IsAtomTerm(t))
        pe->PredFlags |= LogUpdatePredFlag | AtomDBPredFlag;
      else
        pe->PredFlags |= LogUpdatePredFlag;
    }
  } else if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pe = RepPredProp(PredPropByFunc(fun, mod));
  } else {
    return FALSE;
  }
  PELOCK(69, pe);
  if (pe->PredFlags & (ThreadLocalPredFlag | LogUpdatePredFlag)) {
    // second declaration, just ignore
    UNLOCK(pe->PELock);
    return TRUE;
  }
  if (pe->PredFlags &
          (UserCPredFlag | HiddenPredFlag | CArgsPredFlag | SyncPredFlag |
           TestPredFlag | AsmPredFlag | StandardPredFlag | CPredFlag |
           SafePredFlag | IndexedPredFlag | BinaryPredFlag) ||
      pe->NOfClauses) {
    UNLOCK(pe->PELock);
    return FALSE;
  }
#if THREADS
  pe->PredFlags |= ThreadLocalPredFlag | LogUpdatePredFlag;
  pe->OpcodeOfPred = Yap_opcode(_thread_local);
  pe->CodeOfPred = (yamop *)&pe->OpcodeOfPred;
#else
  pe->PredFlags |= LogUpdatePredFlag;
#endif
  UNLOCK(pe->PELock);
  return TRUE;
}

void Yap_InitDBPreds(void) {
  Yap_InitCPred("$set_pred_flags", 2, p_rcdz, SyncPredFlag);
  /** @pred  recorded(+ _K_, _T_, _R_)


  Searches in the internal database under the key  _K_, a term that
  unifies with  _T_ and whose reference matches  _R_. This
  built-in may be used in one of two ways:

  + _K_ may be given, in this case the built-in will return all
  elements of the internal data-base that match the key.
  + _R_ may be given, if so returning the key and element that
  match the reference.



  */
  Yap_InitCPred("recorded", 3, p_recorded, SyncPredFlag);
  Yap_InitCPred("recorda", 3, p_rcda, SyncPredFlag);
  /** @pred  recorda(+ _K_, _T_,- _R_)


  Makes term  _T_ the first record under key  _K_ and  unifies  _R_
  with its reference.


  */
  Yap_InitCPred("recordz", 3, p_rcdz, SyncPredFlag);
  Yap_InitCPred("$still_variant", 2, p_still_variant, SyncPredFlag);
  Yap_InitCPred("recorda_at", 3, p_rcda_at, SyncPredFlag);
  Yap_InitCPred("recordz_at", 3, p_rcdz_at, SyncPredFlag);
  Yap_InitCPred("$recordap", 3, p_rcdap, SyncPredFlag);
  Yap_InitCPred("$recordzp", 3, p_rcdzp, SyncPredFlag);
  Yap_InitCPred("$recordap", 4, p_drcdap, SyncPredFlag);
  Yap_InitCPred("$recordzp", 4, p_drcdzp, SyncPredFlag);
  Yap_InitCPred("erase", 1, p_erase, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$erase_clause", 2, p_erase_clause,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("increase_reference_count", 1, p_increase_reference_counter,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("decrease_reference_count", 1, p_decrease_reference_counter,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("current_reference_count", 2, p_current_reference_counter,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("erased", 1, p_erased,
                TestPredFlag | SafePredFlag | SyncPredFlag);
  Yap_InitCPred("instance", 2, p_instance, SyncPredFlag);
  Yap_InitCPred("$instance_module", 2, p_instance_module, SyncPredFlag);
  Yap_InitCPred("eraseall", 1, p_eraseall, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$record_stat_source", 4, p_rcdstatp,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$some_recordedp", 1, p_somercdedp,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$first_instance", 3, p_first_instance,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$init_db_queue", 1, p_init_queue, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$db_key", 2, p_db_key, 0L);
  Yap_InitCPred("$db_enqueue", 2, p_enqueue, SyncPredFlag);
  Yap_InitCPred("$db_enqueue_unlocked", 2, p_enqueue_unlocked, SyncPredFlag);
  Yap_InitCPred("$db_dequeue", 2, p_dequeue, SyncPredFlag);
  Yap_InitCPred("$db_dequeue_unlocked", 2, p_dequeue_unlocked, SyncPredFlag);
  Yap_InitCPred("$db_peek_queue", 2, p_peek_queue, SyncPredFlag);
  Yap_InitCPred("$db_clean_queues", 1, p_clean_queues, SyncPredFlag);
  Yap_InitCPred("$switch_log_upd", 1, p_slu, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$hold_index", 3, p_hold_index, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$fetch_reference_from_index", 3, p_fetch_reference_from_index,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$resize_int_keys", 1, p_resize_int_keys,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("key_statistics", 4, p_key_statistics, SyncPredFlag);
  Yap_InitCPred("$lu_statistics", 5, p_lu_statistics, SyncPredFlag);
  Yap_InitCPred("total_erased", 4, p_total_erased, SyncPredFlag);
  Yap_InitCPred("key_erased_statistics", 5, p_key_erased_statistics,
                SyncPredFlag);
  Yap_InitCPred("heap_space_info", 3, p_heap_space_info, SyncPredFlag);
  Yap_InitCPred("$jump_to_next_dynamic_clause", 0,
                p_jump_to_next_dynamic_clause, SyncPredFlag);
  Yap_InitCPred("$install_thread_local", 2, p_install_thread_local,
                SafePredFlag);
}

void Yap_InitBackDB(void) {
  Yap_InitCPredBack("$recorded_with_key", 3, 3, in_rded_with_key, co_rded,
                    SyncPredFlag);
  RETRY_C_RECORDED_K_CODE =
      NEXTOP(PredRecordedWithKey->FirstClause, OtapFs);
  Yap_InitCPredBack("$recordedp", 3, 3, in_rdedp, co_rdedp, SyncPredFlag);
  RETRY_C_RECORDEDP_CODE =
      NEXTOP(RepPredProp(PredPropByFunc(Yap_MkFunctor(AtomRecordedP, 3), 0))
                 ->FirstClause,
             OtapFs);
  Yap_InitCPredBack("$current_immediate_key", 2, 4, init_current_key,
                    cont_current_key, SyncPredFlag);
}

/**
@}
*/
