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
static char     SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "clause.h"
#include "heapgc.h"
#include "yapio.h"
#if HAVE_STRING_H
#include <string.h>
#endif

/* There are two options to implement traditional immediate update semantics.

   - In the first option, we only remove an element of the chain when
   it is phisically disposed of. This simplifies things, because
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
#define Register	register
#endif

/* Flags for recorda or recordz				 */
/* MkCode should be the same as CodeDBProperty */
#define MkFirst	1
#define MkCode  CodeDBBit
#define MkLast	4
#define WithRef	8
#define MkIfNot	16
#define InQueue	32

#define FrstDBRef(V)	( (V) -> First )
#define NextDBRef(V)	( (V) -> Next )

#define DBLength(V)	(sizeof(DBStruct) + (Int)(V) + CellSize)
#define AllocDBSpace(V)	((DBRef)Yap_AllocCodeSpace(V))
#define FreeDBSpace(V)	Yap_FreeCodeSpace(V)

#define NO_ERROR_IN_DB 0
#define OVF_ERROR_IN_DB 1
#define SOVF_ERROR_IN_DB 2
#define TOVF_ERROR_IN_DB 3
#define OTHER_ERROR_IN_DB 4

#if SIZEOF_INT_P==4
#define ToSmall(V)	((link_entry)(Unsigned(V)>>2))
#else
#define ToSmall(V)	((link_entry)(Unsigned(V)>>3))
#endif

#define DEAD_REF(ref) FALSE

#ifdef SFUNC

#define MaxSFs		256

typedef struct {
  Term            SName;	/* The culprit */
  CELL           *SFather;      /* and his father's position */
}               SFKeep;
#endif

typedef struct idb_queue
{
  Functor id;		/* identify this as being pointed to by a DBRef */
  SMALLUNSGN    Flags;  /* always required */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t    QRWLock;         /* a simple lock to protect this entry */
#endif
  DBRef FirstInQueue, LastInQueue;
  Int age;        /* the number of catches when we created the queue */
  struct idb_queue *next, *prev;
}  db_queue;

#define HashFieldMask		((CELL)0xffL)
#define DualHashFieldMask	((CELL)0xffffL)
#define TripleHashFieldMask	((CELL)0xffffffL)
#define FourHashFieldMask	((CELL)0xffffffffL)

#define    ONE_FIELD_SHIFT         8
#define   TWO_FIELDS_SHIFT        16
#define THREE_FIELDS_SHIFT        24

#define AtomHash(t)	(Unsigned(t)>>4)
#define FunctorHash(t)  (Unsigned(t)>>4)
#define NumberHash(t)   (Unsigned(IntOfTerm(t)))

#define LARGE_IDB_LINK_TABLE 1

/* traditionally, YAP used a link table to recover IDB terms*/
#define IDB_LINK_TABLE 1
#if LARGE_IDB_LINK_TABLE
typedef BITS32 link_entry; 
#define SIZEOF_LINK_ENTRY 4
#else
typedef BITS16 link_entry; 
#define SIZEOF_LINK_ENTRY 2
#endif
/* a second alternative is to just use a tag */
/*#define IDB_USE_MBIT 1*/

/* These global variables are necessary to build the data base
   structure */
#ifdef IDB_LINK_TABLE
static link_entry  *lr = NULL, *LinkAr;
#endif
static int      DBErrorFlag = FALSE; /* error while recording */
/* we cannot call Error directly from within recorded(). These flags are used
   to delay for a while
*/
static yap_error_number DBErrorNumber; /* error number */
static Term      DBErrorTerm; /* error term */
static char     *DBErrorMsg;       /* Error Message */
static DBRef    *tofref;	/* place the refs also up	 */

CELL *next_float = NULL;

#ifdef SFUNC
static CELL    *FathersPlace;	/* Where the father was going when the term
				 * was reached */
static SFKeep  *SFAr, *TopSF;	/* Where are we putting our SFunctors */
#endif

static DBRef    found_one;	/* Place where we started recording */

#ifdef SUPPORT_HASH_TABLES
typedef struct {
  CELL  key;
  DBRef entry;
} hash_db_entry;

typedef table {
  Int NOfEntries;
  Int HashArg;
  hash_db_entry *table;
} hash_db_table;
#endif

STATIC_PROTO(CELL *cpcells,(CELL *,CELL*,Int));
#ifdef IDB_LINK_TABLE
STATIC_PROTO(void linkblk,(link_entry *,CELL *));
#endif
#ifdef IDB_USE_MBIT
STATIC_PROTO(CELL *linkcells,(CELL *,Int));
#endif
STATIC_PROTO(Int cmpclls,(CELL *,CELL *,Int));
STATIC_PROTO(Prop FindDBProp,(AtomEntry *, int, unsigned int, SMALLUNSGN));
STATIC_PROTO(CELL  CalcKey, (Term));
#ifdef COROUTINING
STATIC_PROTO(CELL  *MkDBTerm, (CELL *, CELL *, CELL *, CELL *, CELL *, CELL *,int *));
#else
STATIC_PROTO(CELL  *MkDBTerm, (CELL *, CELL *, CELL *, CELL *, CELL *, int *));
#endif
STATIC_PROTO(DBRef  CreateDBStruct, (Term, DBProp, int));
STATIC_PROTO(DBRef  new_lu_index, (LogUpdDBProp));
STATIC_PROTO(void   clean_lu_index, (DBRef));
STATIC_PROTO(DBRef  record, (int, Term, Term, Term));
STATIC_PROTO(DBRef  check_if_cons, (DBRef, Term));
STATIC_PROTO(DBRef  check_if_var, (DBRef));
STATIC_PROTO(DBRef  check_if_wvars, (DBRef, unsigned int, CELL *));
#ifdef IDB_LINK_TABLE
STATIC_PROTO(int  scheckcells, (int, CELL *, CELL *, link_entry *, CELL));
#endif
STATIC_PROTO(DBRef  check_if_nvars, (DBRef, unsigned int, CELL *));
STATIC_PROTO(Int  p_rcda, (void));
STATIC_PROTO(Int  p_rcdap, (void));
STATIC_PROTO(Int  p_rcdz, (void));
STATIC_PROTO(Int  p_rcdzp, (void));
STATIC_PROTO(Int  p_drcdap, (void));
STATIC_PROTO(Int  p_drcdzp, (void));
STATIC_PROTO(Int  p_rcdaifnot, (void));
STATIC_PROTO(Int  p_rcdzifnot, (void));
STATIC_PROTO(Term  GetDBTerm, (DBRef));
STATIC_PROTO(DBProp  FetchDBPropFromKey, (Term, int, int, char *));
STATIC_PROTO(Int  i_log_upd_recorded, (LogUpdDBProp));
STATIC_PROTO(Int  i_recorded, (DBProp,Term));
STATIC_PROTO(Int  c_log_upd_recorded, (DBRef *, int));
STATIC_PROTO(Int  c_recorded, (int));
STATIC_PROTO(Int  in_rded, (void));
STATIC_PROTO(Int  co_rded, (void));
STATIC_PROTO(Int  in_rdedp, (void));
STATIC_PROTO(Int  co_rdedp, (void));
STATIC_PROTO(Int  p_first_instance, (void));
STATIC_PROTO(void  ErasePendingRefs, (DBRef));
STATIC_PROTO(void  RemoveDBEntry, (DBRef));
STATIC_PROTO(void  EraseLogUpdCl, (Clause *));
STATIC_PROTO(void  MyEraseClause, (Clause *));
STATIC_PROTO(void  PrepareToEraseClause, (Clause *, DBRef));
STATIC_PROTO(void  EraseEntry, (DBRef));
STATIC_PROTO(Int  p_erase, (void));
STATIC_PROTO(Int  p_eraseall, (void));
STATIC_PROTO(Int  p_erased, (void));
STATIC_PROTO(Int  p_instance, (void));
STATIC_PROTO(int  NotActiveDB, (DBRef));
STATIC_PROTO(DBEntry  *NextDBProp, (PropEntry *));
STATIC_PROTO(Int  init_current_key, (void));
STATIC_PROTO(Int  cont_current_key, (void));
STATIC_PROTO(Int  cont_current_key_integer, (void));
STATIC_PROTO(Int  p_rcdstatp, (void));
STATIC_PROTO(Int  p_somercdedp, (void));
STATIC_PROTO(yamop * find_next_clause, (DBRef));
STATIC_PROTO(Int  p_jump_to_next_dynamic_clause, (void));
#ifdef SFUNC
STATIC_PROTO(void  SFVarIn, (Term));
STATIC_PROTO(void  sf_include, (SFKeep *));
#endif
STATIC_PROTO(Int  p_init_queue, (void));
STATIC_PROTO(Int  p_enqueue, (void));
STATIC_PROTO(void keepdbrefs, (DBRef));
STATIC_PROTO(Int  p_dequeue, (void));
STATIC_PROTO(void ErDBE, (DBRef));

#if OS_HANDLES_TR_OVERFLOW
#define check_trail(x)
#else
#define check_trail(x) {                           \
  if (Unsigned(Yap_TrailTop) == Unsigned(x)) {         \
    if(!Yap_growtrail (sizeof(CELL) * 16 * 1024L)) {   \
      goto error_tr_overflow;                      \
    }                                              \
  }                                                \
}
			
#endif


#ifdef SUPPORT_HASH_TABLES
/* related property and hint on number of entries */
static void create_hash_table(DBProp p, Int hint) {
  int off = sizeof(CELL)*4, out;
  Int size;

  if (hint < p->NOfEntries)
    hint = p->NOfEntries;
  while (off) {
    int limit = 1 << (off);
    if (inp >= limit) {
      out += off;
      inp >>= off;
    }
    off >>= 1;
  }
  if ((size = 1 << out) < hint)
    hint <<= 1;
  /* clean up the table */
  pt = tbl = (hash_db_entry *)AllocDBSpace(hint*sizeof(hash_db_entry));
  for (i=0; i< hint; i++) {
    pt->key = NULL;
    pt++;
  }
  /* next insert the entries */
}

static void insert_in_table() {
  
}

static void remove_from_table() {
  
}
#endif

#ifdef IDB_LINK_TABLE
inline static CELL *cpcells(CELL *to, CELL *from, Int n)
{
#if HAVE_MEMMOVE
  memmove((void *)to, (void *)from, (size_t)(n*sizeof(CELL)));
  return(to+n);
#else
  while (n-- >= 0) {
    *to++ = *from++;
  }
  return(to);
#endif
}

static void linkblk(link_entry *r, CELL *c)
{
  CELL p;

  while ((p = (CELL)*r) != 0) {
    Term t = c[p];
    r++;
    c[p] = AdjustIDBPtr(t, (CELL)c);
  }
}
#endif

#ifdef IDB_USE_MBIT
inline static CELL *cpcells(register CELL *to, register CELL *from, Int n)
{
  CELL *last = to + n;
  register CELL off = ((CELL)to)-MBIT;
  while (to <= last) {
    register d0 = *from++;
    if (MARKED(d0)) 
      *to++ = AdjustIDBPtr(d0, off);
    else
      *to++ = d0;
  }
  return(to);
}

static CELL *linkcells(register CELL *to, Int n)
{
  CELL *last = to + n;
  register CELL off = ((CELL)to)-MBIT;
  while(to <= last) {
    register d0 = *to++;
    if (MARKED(d0)) 
      to[-1] = AdjustIDBPtr(d0, off);
  }
  return(to);
}


#endif

static Int cmpclls(CELL *a,CELL *b,Int n)
{
  while (n-- > 0) {
    if(*a++ != *b++) return FALSE;
  }
  return TRUE;
}

int Yap_DBTrailOverflow(void)
{
#ifdef IDB_USE_MBIT
  return(FALSE);
#endif
#ifdef IDB_LINK_TABLE
  return((CELL *)lr > (CELL *)Yap_TrailTop - 1024);
#endif
}

/* get DB entry for ap/arity; */
static Prop 
FindDBPropHavingLock(AtomEntry *ae, int CodeDB, unsigned int arity, SMALLUNSGN dbmod)
{
  Prop          p0;
  DBProp        p;

  p = RepDBProp(p0 = ae->PropsOfAE);
  while (p0 && (((p->KindOfPE & ~0x1) != (CodeDB|DBProperty)) ||
		(p->ArityOfDB != arity) ||
		((CodeDB & MkCode) && p->ModuleOfDB && p->ModuleOfDB != dbmod))) {
    p = RepDBProp(p0 = p->NextOfPE);
  }
  return (p0);
}


/* get DB entry for ap/arity; */
static Prop 
FindDBProp(AtomEntry *ae, int CodeDB, unsigned int arity, SMALLUNSGN dbmod)
{
  Prop out;

  READ_LOCK(ae->ARWLock);
  out = FindDBPropHavingLock(ae, CodeDB, arity, dbmod);
  READ_UNLOCK(ae->ARWLock);
  return(out);
}



/* These two functions allow us a fast lookup method in the data base */
/* PutMasks builds the mask and hash for a single argument	 */
inline static CELL 
CalcKey(Term tw)
{
  /* The first argument is known to be instantiated */
  if (IsApplTerm(tw)) {
    Functor f = FunctorOfTerm(tw);
    if (IsExtensionFunctor(f)) {
      if (f == FunctorDBRef) {
	return(FunctorHash(tw));	/* Ref */
      } /* if (f == FunctorLongInt || f == FunctorDouble) */
      return(NumberHash(RepAppl(tw)[1]));
    }
    return(FunctorHash(f));
  } else if (IsAtomOrIntTerm(tw)) {
    if (IsAtomTerm(tw)) {
      return(AtomHash(tw));
    }
    return(NumberHash(tw));
  }
  return(FunctorHash(FunctorList));
}

/* EvalMasks builds the mask and hash for up to three arguments of a term */
static CELL 
EvalMasks(register Term tm, CELL *keyp)
{

  if (IsVarTerm(tm)) {
    *keyp = 0L;
    return(0L);
  } else if (IsApplTerm(tm)) {
    Functor         fun = FunctorOfTerm(tm);

    if (IsExtensionFunctor(fun)) {
      if (fun == FunctorDBRef) {
	*keyp = FunctorHash(tm);	/* Ref */
      } else /* if (f == FunctorLongInt || f == FunctorDouble) */ {
	*keyp = NumberHash(RepAppl(tm)[1]);
      }
      return(FourHashFieldMask);
    } else {
      unsigned int    arity;

      arity = ArityOfFunctor(fun);
#ifdef SFUNC
      if (arity == SFArity) {	/* do not even try to calculate masks */
	*keyp = key;
	return(FourHashFieldMask);
      }
#endif
      switch (arity) {
      case 1:
	{
	  Term tw = ArgOfTerm(1, tm);

	  if (IsNonVarTerm(tw)) {
	    *keyp = (FunctorHash(fun) & DualHashFieldMask) | (CalcKey(tw) << TWO_FIELDS_SHIFT);
	    return(FourHashFieldMask);
	  } else {
	    *keyp = (FunctorHash(fun) & DualHashFieldMask);
	    return(DualHashFieldMask);
	  }
	}
      case 2:
	{
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
	    return(mask | (HashFieldMask << THREE_FIELDS_SHIFT));
	  } else {
	    *keyp = key;
	    return(mask);
	  }
	}
      default:
	{
	  Term tw1, tw2, tw3;
	  CELL key, mask;

	  key = FunctorHash(fun)  & HashFieldMask;
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
	    return(mask | (HashFieldMask << THREE_FIELDS_SHIFT));
	  } else {
	    *keyp = key;
	    return(mask);
	  }
	}
      }
    }
  } else {
    CELL key  = (FunctorHash(FunctorList) & DualHashFieldMask);
    CELL mask = DualHashFieldMask;
    Term th = HeadOfTerm(tm), tt;

    if (IsNonVarTerm(th)) {
      mask |= (HashFieldMask << TWO_FIELDS_SHIFT);
      key |= (CalcKey(th) << TWO_FIELDS_SHIFT);
    }
    tt = TailOfTerm(tm);
    if (IsNonVarTerm(tt)) {
      *keyp = key | (CalcKey(tt) << THREE_FIELDS_SHIFT);
      return( mask|(HashFieldMask << THREE_FIELDS_SHIFT));
    }
    *keyp = key;
    return(mask);
  }
}

CELL 
Yap_EvalMasks(register Term tm, CELL *keyp)
{
  return EvalMasks(tm, keyp);
}


/* Called to inform that a new pointer to a data base entry has been added */
#define MarkThisRef(Ref)	((Ref)->NOfRefsTo ++ )

/* From a term, builds its representation in the data base */

/* otherwise, we just need to restore variables*/
typedef struct {
  CELL *addr;
} visitel;
#define UNWIND_CUNIF()                                        \
         while (visited < (visitel *)AuxSp) {                 \
            RESET_VARIABLE(visited->addr);                    \
            visited ++;                                       \
         }

/* no checking for overflow while building DB terms yet */
#define  CheckDBOverflow() if (CodeMax+1024 >= (CELL *)visited) {     \
    goto error;					                      \
   }
    
/* no checking for overflow while building DB terms yet */
#define  CheckVisitOverflow() if ((CELL *)to_visit+1024 >= ASP) {     \
    goto error2;					              \
   }
    

/* This routine creates a complex term in the heap. */
static CELL *MkDBTerm(register CELL *pt0, register CELL *pt0_end,
		     register CELL *StoPoint,
		     CELL *CodeMax, CELL *tbase,
#ifdef COROUTINING
		     CELL *attachmentsp,
#endif
		     int *vars_foundp)
{

  register visitel *visited = (visitel *)AuxSp;
  /* store this in H */
  register CELL **to_visit = (CELL **)H;
  CELL **to_visit_base = to_visit;
  /* where we are going to add a new pair */
  int vars_found = 0;
#ifdef COROUTINING
  Term ConstraintsTerm = TermNil;
  CELL *ConstraintsBottom = NULL;
  CELL *origH = H;
#endif

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
      if (ap2 < (CELL *)((CELL)CodeMax-(CELL)tbase)) {
	*lr++ = ToSmall((CELL)(StoPoint)-(CELL)(tbase));
	check_trail(lr);
	*StoPoint++ = d0;
	++pt0;
	continue;
      }
#endif
#ifdef IDB_LINK_TABLE
      *lr++ = ToSmall((CELL)(StoPoint)-(CELL)(tbase));
      check_trail(lr);
#endif
      f = (Functor)(*ap2);
      if (IsExtensionFunctor(f)) {
	switch((CELL)f) {
	case (CELL)FunctorDBRef:
	  {
	    DBRef dbentry;
	    /* store now the correct entry */
	    dbentry = DBRefOfTerm(d0);
	    *StoPoint++ = d0;
#ifdef IDB_LINK_TABLE
	    lr--;
#endif
	    dbentry->NOfRefsTo++;
	    *--tofref = dbentry;
	    /* just continue the loop */
	    ++ pt0;
	    continue;
	  }
	case (CELL)FunctorLongInt:
	  {
	    CELL *st = CodeMax;

	    CheckDBOverflow();
	/* first thing, store a link to the list before we move on */
#ifdef IDB_USE_MBIT
	    *StoPoint++ = AbsAppl((CELL *)(((CELL)st-(CELL)tbase)|MBIT));
#else
	    *StoPoint++ = AbsAppl((CELL *)((CELL)st-(CELL)tbase));
#endif
	    st[0] = (CELL)f;
	    st[1] = ap2[1];
	    st[2] = ((2*sizeof(CELL)+EndSpecials)|MBIT);
	    /* now reserve space */
	    CodeMax = st+3;
	    ++pt0;
	    continue;
	  }
#ifdef USE_GMP
	case (CELL)FunctorBigInt:
	  {
	    CELL *st = CodeMax;

	    CheckDBOverflow();
	    /* first thing, store a link to the list before we move on */
#ifdef IDB_USE_MBIT
	    *StoPoint++ = AbsAppl((CELL *)(((CELL)st-(CELL)tbase)|MBIT));
#else
	    *StoPoint++ = AbsAppl((CELL *)((CELL)st-(CELL)tbase));
#endif
	    st[0] = (CELL)f;
	    {
	      Int sz = 
		sizeof(MP_INT)+
		(((MP_INT *)(ap2+1))->_mp_alloc*sizeof(mp_limb_t));
	      memcpy((void *)(st+1), (void *)(ap2+1), sz);
	      CodeMax = st+1+sz/CellSize;
	      *CodeMax++ = (sz+CellSize+EndSpecials)|MBIT;
	    }
	    ++pt0;
	    continue;
	  }
#endif
	case (CELL)FunctorDouble:
	  {
	    CELL *st = CodeMax;

	    CheckDBOverflow();
	    /* first thing, store a link to the list before we move on */
#ifdef IDB_USE_MBIT
	    *StoPoint++ = AbsAppl((CELL *)(((CELL)st-(CELL)tbase)|MBIT));
#else
	    *StoPoint++ = AbsAppl((CELL *)((CELL)st-(CELL)tbase));
#endif
	    st[0] = (CELL)f;
	    st[1] = ap2[1];
#if  SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT
	    st[2] = ap2[2];
	    st[3] = ((3*sizeof(CELL)+EndSpecials)|MBIT);
#else
	    st[2] = ((2*sizeof(CELL)+EndSpecials)|MBIT);
#endif
	    /* now reserve space */
	    CodeMax = st+(2+SIZEOF_DOUBLE/SIZEOF_LONG_INT);
	    ++pt0;
	    continue;
	  }
	}
      }
      /* first thing, store a link to the list before we move on */
#ifdef IDB_USE_MBIT
      *StoPoint++ = AbsAppl((CELL *)(((CELL)CodeMax-(CELL)tbase)|MBIT));
#else
      *StoPoint++ = AbsAppl((CELL *)((CELL)CodeMax-(CELL)tbase));
#endif
      /* next, postpone analysis to the rest of the current list */
#ifdef RATIONAL_TREES
      to_visit[0] = pt0+1;
      to_visit[1] = pt0_end;
      to_visit[2] = StoPoint;
      to_visit[3] = (CELL *)*pt0;
      to_visit += 4;
      *pt0 = StoPoint[-1];
#else
      if (pt0 < pt0_end) {
	to_visit[0] = pt0+1;
	to_visit[1] = pt0_end;
	to_visit[2] = StoPoint;
	to_visit += 3;
      }
#endif
      CheckVisitOverflow();
      d0 = ArityOfFunctor(f);
      pt0 = ap2+1;
      pt0_end = ap2 + d0;
      /* prepare for our new compound term */
      /* first the functor */
      CheckDBOverflow();
      *CodeMax++ = (CELL)f;
      /* we'll be working here */
      StoPoint = CodeMax;
      /* now reserve space */
      CodeMax += d0;
      continue;
    }
    else if (IsPairTerm(d0)) {
      /* we will need to link afterwards */
#ifdef RATIONAL_TREES
      if (RepPair(d0) < (CELL *)((CELL)CodeMax-(CELL)tbase)) {
	*StoPoint++ = d0;
	*lr++ = ToSmall((CELL)(StoPoint)-(CELL)(tbase));
	check_trail(lr);
	++pt0;
	continue;
      }
#endif
#ifdef IDB_LINK_TABLE
      *lr++ = ToSmall((CELL)(StoPoint)-(CELL)(tbase));
      check_trail(lr);
#endif
#ifdef IDB_USE_MBIT
      *StoPoint++ =
	AbsPair((CELL *)(((CELL)CodeMax-(CELL)tbase)|MBIT));
#else
      *StoPoint++ = AbsPair((CELL *)(((CELL)CodeMax-(CELL)tbase)));
#endif
      /* next, postpone analysis to the rest of the current list */
#ifdef RATIONAL_TREES
      to_visit[0] = pt0+1;
      to_visit[1] = pt0_end;
      to_visit[2] = StoPoint;
      to_visit[3] = (CELL *)*pt0;
      to_visit += 4;
      *pt0 = StoPoint[-1];
#else
      if (pt0 < pt0_end) {
	to_visit[0] = pt0+1;
	to_visit[1] = pt0_end;
	to_visit[2] = StoPoint;
	to_visit += 3;
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
      CheckDBOverflow();
      continue;
    } else if (IsAtomOrIntTerm(d0)) {
      *StoPoint++ = d0;
      ++pt0;
      continue;
    }
    
    /* the code to dereference a  variable */
  deref_var:
    if (!MARKED(d0)) {
      if ( 
#if SBA
	  d0 != 0
#else
	  d0 != (CELL)ptd0
#endif
	  ) {
	ptd0 = (Term *) d0;
	d0 = *ptd0;
	goto restart; /* continue dereferencing */
      }
      /* else just drop to found_var */
    }
    /* else just drop to found_var */
    {
      CELL displacement = (CELL)(StoPoint)-(CELL)(tbase);
      
      pt0++;
      /* first time we found this variable! */
      if (!MARKED(d0)) {
	
	/* store previous value */ 
	visited --;
	visited->addr = ptd0;
	CheckDBOverflow();
	/* variables need to be offset at read time */
	*ptd0 = (displacement | MBIT);
#if SBA
	/* the copy we keep will be an empty variable   */
	*StoPoint++ = 0;
#else
#ifdef IDB_USE_MBIT
	/* say we've seen the variable, and make it point to its
	   offset */
	/* the copy we keep will be the current displacement   */
	*StoPoint++ = (displacement | MBIT);
#else
	/* the copy we keep will be the current displacement   */
	*StoPoint++ = displacement;
	*lr++ = ToSmall(displacement);
	check_trail(lr);
#endif
#endif
	/* indicate we found variables */
	vars_found++;
#ifdef COROUTINING
	if (IsAttachedTerm((CELL)ptd0)) {
	  Term t[4];
	  int sz = to_visit-to_visit_base;

	  H = (CELL *)to_visit;
	  /* store the constraint away for now */
	  t[0] = (CELL)ptd0;
	  t[1] = attas[ExtFromCell(ptd0)].to_term_op(ptd0);
	  t[2] = MkIntegerTerm(ExtFromCell(ptd0));
	  t[3] = TermNil;
	  if (ConstraintsBottom == NULL) {
	    ConstraintsTerm = Yap_MkApplTerm(FunctorClist, 4, t);
	    ConstraintsBottom = RepAppl(ConstraintsTerm)+4;
	  } else {
	    Term new = Yap_MkApplTerm(FunctorClist, 4, t);
	    *ConstraintsBottom = new;
	    ConstraintsBottom = RepAppl(new)+4;
	  }
	  if (H+sz >= ASP) {
	    goto error2;
	  }
	  memcpy((void *)H, (void *)(to_visit_base), sz*sizeof(CELL *));
	  to_visit_base = (CELL **)H;
	  to_visit = to_visit_base+sz;
	}
#endif
	continue;
      } else  {
	/* references need to be offset at read time */
#ifdef IDB_LINK_TABLE
	*lr++ = ToSmall(displacement);
	check_trail(lr);
#endif
	/* store the offset */
#ifdef IDB_USE_MBIT
	*StoPoint = d0;
#else
	*StoPoint = d0 ^ MBIT;
#endif
	StoPoint++;
	continue;
      }

    }

  }

  /* Do we still have compound terms to visit */
  if (to_visit > to_visit_base) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    StoPoint = to_visit[2];
    pt0[-1] = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    CheckDBOverflow();
    StoPoint = to_visit[2];
#endif
    goto loop;
  }

#ifdef COROUTINING
  /* we still may have constraints to do */
  if (ConstraintsTerm != TermNil) {
    *attachmentsp = (CELL)(CodeMax)-(CELL)(tbase);
    pt0 = RepAppl(ConstraintsTerm)+1;
    pt0_end = RepAppl(ConstraintsTerm)+4;
    ConstraintsTerm = TermNil; 
    StoPoint = CodeMax;
    CheckDBOverflow();
    CodeMax += 4;
    goto loop;
  }
#endif
  /* we're done */
  *vars_foundp = vars_found;
  UNWIND_CUNIF();
#ifdef COROUTINING
  H = origH;
#endif
  return(CodeMax);

 error:
  DBErrorFlag = OVF_ERROR_IN_DB;
  *vars_foundp = vars_found;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit_base) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    StoPoint = to_visit[2];
    pt0[-1] = (CELL)to_visit[3];
  }
#endif
  UNWIND_CUNIF();
#ifdef COROUTINING
  H = origH;
#endif
  return(NULL);

 error2:
  DBErrorFlag = SOVF_ERROR_IN_DB;
  *vars_foundp = vars_found;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit_base) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    StoPoint = to_visit[2];
    pt0[-1] = (CELL)to_visit[3];
  }
#endif
  UNWIND_CUNIF();
#ifdef COROUTINING
  H = origH;
#endif
  return(NULL);

#if !OS_HANDLES_TR_OVERFLOW
 error_tr_overflow:
  DBErrorFlag = TOVF_ERROR_IN_DB;              \
  *vars_foundp = vars_found;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit_base) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    StoPoint = to_visit[2];
    pt0[-1] = (CELL)to_visit[3];
  }
#endif
  UNWIND_CUNIF();
#ifdef COROUTINING
  H = origH;
#endif
  return(NULL);
#endif
}


#ifdef SFUNC
/*
 * The sparse terms existing in the structure are to be included now. This
 * means simple copy for constant terms but, some care about variables If
 * they have appeared before, we will know by their position number 
 */
static void 
sf_include(sfp)
	SFKeep         *sfp;
{
  Term            Tm = sfp->SName;
  CELL           *tp = ArgsOfSFTerm(Tm);
  Register Term  *StoPoint = ntp;
  CELL           *displacement = CodeAbs;
  CELL            arg_no;
  Term            tvalue;
  int             j = 3;

  if (sfp->SFather != NIL)
    *(sfp->SFather) = AbsAppl(displacement);
  *StoPoint++ = FunctorOfTerm(Tm);
  *lr++ = ToSmall(displacement + 1);
  check_trail(lr);
  *StoPoint++ = (Term) (displacement + 1);
  while (*tp) {
    arg_no = *tp++;
    tvalue = Derefa(tp++);
    if (IsVarTerm(tvalue)) {
      if (((VarKeep *) tvalue)->NOfVars != 0) {
	*StoPoint++ = arg_no;
	*lr++ = ToSmall(displacement + j);
	check_trail(lr);
	if (((VarKeep *) tvalue)->New == 0)
	  *StoPoint++ = ((VarKeep *) tvalue)->New = Unsigned(displacement + j);
	else
	  *StoPoint++ = ((VarKeep *) tvalue)->New;
	j += 2;
      }
    } else if (IsAtomicTerm(tvalue)) {
      *StoPoint++ = arg_no;
      *StoPoint++ = tvalue;
      j += 2;
    } else {
      DBErrorFlag = OTHER_ERROR_IN_DB;
      DBErrorNumber = TYPE_ERROR_DBTERM;
      DBErrorTerm = d0;
      DBErrorMsg = "wrong term in SF";
      return(NULL);
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
inline static DBRef 
check_if_cons(DBRef p, Term to_compare)
{
  while (p != NIL
	 && (p->Flags & (DBCode | ErasedMask | DBVar | DBNoVars | DBComplex)
	     || p->Entry != Unsigned(to_compare)))
    p = NextDBRef(p);
  return (p);
}

/*
 * This function is used to check if one of the terms in the idb is a prolog
 * variable 
 */
static DBRef 
check_if_var(DBRef p)
{
  while (p != NIL &&
	 p->Flags & (DBCode | ErasedMask | DBAtomic | DBNoVars | DBComplex ))
    p = NextDBRef(p);
  return (p);
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
static DBRef 
check_if_wvars(DBRef p, unsigned int NOfCells, CELL *BTptr)
{
  CELL           *memptr;

  do {
    while (p != NIL &&
	   p->Flags & (DBCode | ErasedMask | DBAtomic | DBNoVars | DBVar))
      p = NextDBRef(p);
    if (p == NIL)
      return (p);
    memptr = CellPtr(&(p->Contents));
    if (NOfCells == p->NOfCells
	&& cmpclls(memptr, BTptr, NOfCells))
      return (p);
    else
      p = NextDBRef(p);
  } while (TRUE);
  return (NIL);
}

#ifdef IDB_LINK_TABLE
static int 
scheckcells(int NOfCells, register CELL *m1, register CELL *m2, link_entry *lp, register CELL bp)
{
  CELL            base = Unsigned(m1);
  link_entry         *lp1;

  while (NOfCells-- > 0) {
    Register CELL   r1, r2;

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
	return (FALSE);
      /* keep the old link pointer for future search. */
      /* vsc: this looks like a bug!!!! */
      /* *lp1 = *lp++; */
    } else {
      return (FALSE);
    }
  }
  return (TRUE);
}
#endif

/*
 * the cousin of the previous, but with things a bit more sophisticated.
 * mtchcells, if an error was an found, needs to test ........ 
 */
static DBRef 
check_if_nvars(DBRef p, unsigned int NOfCells, CELL *BTptr)
{
  CELL           *memptr;

  do {
    while (p != NIL &&
	   p->Flags & (DBCode | ErasedMask | DBAtomic | DBComplex | DBVar))
      p = NextDBRef(p);
    if (p == NIL)
      return (p);
    memptr = CellPtr(p->Contents);
#ifdef IDB_LINK_TABLE
    if (scheckcells(NOfCells, memptr, BTptr, LinkAr, Unsigned(p->Contents-1)))
#else
      if (NOfCells == *memptr++
	  && cmpclls(memptr, BTptr, NOfCells))
#endif
	return (p);
      else
	p = NextDBRef(p);
  } while (TRUE);
  return (NIL);
}

static DBRef 
CreateDBStruct(Term Tm, DBProp p, int InFlag)
{
  Register Term   tt, *nar = NIL;
  Register DBRef  pp0, pp;
  SMALLUNSGN      flag;
  unsigned int    NOfCells = 0;
#ifdef IDB_LINK_TABLE
  int NOfLinks = 0;
#endif
  Term           *ntp0, *ntp;
  /* place DBRefs in ConsultStack */
  DBRef    *TmpRefBase = (DBRef *)ConsultSp;
  CELL	   *CodeAbs;	/* how much code did we find	 */
  int vars_found;
#ifdef COROUTINING
  CELL attachments = 0;
#endif

  DBErrorFlag = NO_ERROR_IN_DB;

  if (IsVarTerm(Tm)
#ifdef COROUTINING
      && !IsAttachedTerm(Tm)
#endif
      ) {
    Register DBRef  pp;

    tt = Tm;
    if (InFlag & MkIfNot && (found_one = check_if_var(p->First)))
      return (found_one);
    pp = AllocDBSpace(DBLength(NIL));
    if (pp == NIL) {
      DBErrorFlag = OTHER_ERROR_IN_DB;
      DBErrorNumber = SYSTEM_ERROR;
      DBErrorTerm = TermNil;
      DBErrorMsg = "could not allocate space";
      return(NULL);
    }
    pp->id = FunctorDBRef;
    pp->Flags = DBVar;
    pp->Entry = (CELL) Tm;
    pp->Code = NULL;
    pp->NOfCells = 1;
    INIT_LOCK(pp->lock);
    INIT_DBREF_COUNT(pp);
    return(pp);
  } else if (IsAtomOrIntTerm(Tm)) {
    Register DBRef  pp;
    SMALLUNSGN      flag;

    tt = Tm;
    flag = DBAtomic;
    if (IsAtomOrIntTerm(tt))
    if (InFlag & MkIfNot && (found_one = check_if_cons(p->First, tt)))
      return (found_one);
    pp = AllocDBSpace(DBLength(NIL));
    if (pp == NIL) {
      DBErrorFlag = OTHER_ERROR_IN_DB;
      DBErrorNumber = SYSTEM_ERROR;
      DBErrorTerm = TermNil;
      DBErrorMsg = "could not allocate space";
      return(NULL);
    }
    pp->id = FunctorDBRef;
    pp->Flags = flag;
    pp->Entry = (CELL) Tm;
    pp->Code = NULL;
    pp->NOfCells = 1;
    INIT_LOCK(pp->lock);
    INIT_DBREF_COUNT(pp);
    return(pp);
  } else {

    tofref = TmpRefBase;
    /* compound term */
    pp0 = (DBRef)Yap_PreAllocCodeSpace();
    ntp0 = pp0->Contents;
#ifdef IDB_LINK_TABLE
    lr = LinkAr = (link_entry *)TR;
#endif
#ifdef COROUTINING
    /* attachment */ 
    if (IsVarTerm(Tm)) {
      tt = sizeof(CELL);
      ntp = MkDBTerm(VarOfTerm(Tm), VarOfTerm(Tm), ntp0, ntp0+1, ntp0-1,
		     &attachments,
		     &vars_found);
      if (ntp == NULL) {
	Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
	return(NULL);
      }
    } else
#endif
    if (IsPairTerm(Tm)) {
      /* avoid null pointers!! */
      tt = AbsPair((CELL *)sizeof(CELL));
      ntp = MkDBTerm(RepPair(Tm), RepPair(Tm)+1, ntp0, ntp0+2, ntp0-1,
#ifdef COROUTINING
		     &attachments,
#endif
		     &vars_found);
      if (ntp == NULL) {
	Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
	return(NULL);
      }
    }
    else
    {
      unsigned int arity;
      Functor fun;

      tt = AbsAppl((CELL *)sizeof(CELL));
      /* we need to store the functor manually */
      fun = (Functor)(*ntp0 = (CELL)FunctorOfTerm(Tm));
      if (IsExtensionFunctor(fun)) {
	switch((CELL)fun) {
	case (CELL)FunctorDouble:
	  {
	    CELL *fp = RepAppl(Tm);

	    ntp0[1] = fp[1];
#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT
	    ntp0[2] = fp[2];
	    ntp0[3] = ((3*sizeof(CELL)+EndSpecials)|MBIT);
	    ntp = ntp0+4;
#else
	    ntp0[2] = ((2*sizeof(CELL)+EndSpecials)|MBIT);
	    ntp = ntp0+3;
#endif
	  }
	  break;
	case (CELL)FunctorDBRef:
	  {
	    DBRef dbr;

	    pp = AllocDBSpace(DBLength(2*sizeof(DBRef)));
	    if (pp == NIL) {
	      DBErrorFlag = OTHER_ERROR_IN_DB;
	      DBErrorNumber = SYSTEM_ERROR;
	      DBErrorTerm = TermNil;
	      DBErrorMsg = "could not allocate space";
	      return(NULL);
	    }
	    pp->id = FunctorDBRef;
	    pp->Flags = DBNoVars|DBComplex|DBWithRefs;
	    pp->Entry = Tm;
	    pp->NOfCells = 2;
	    dbr = DBRefOfTerm(Tm);
	    dbr->NOfRefsTo++;
	    pp->Contents[0] = (CELL)NIL;
	    pp->Contents[1] = (CELL)dbr;
	    pp->DBRefs = (DBRef *)(pp->Contents+2);
	    INIT_LOCK(pp->lock);
	    INIT_DBREF_COUNT(pp);
	    Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
	    return(pp);
	  }
#ifdef USE_GMP
	case (CELL)FunctorBigInt:
	  {
	    CELL *pt = RepAppl(Tm);
	    Int sz =
	      sizeof(MP_INT)+
	      (((MP_INT *)(pt+1))->_mp_alloc*sizeof(mp_limb_t));

	    memcpy((void *)(ntp0+1), (void *)(pt+1), sz);
	    ntp = ntp0+sz/sizeof(CELL)+1;
	    *ntp++ = (sz+CellSize+EndSpecials)|MBIT;
	  }
	  break;
#endif
	default: /* LongInt */
	  {
	    CELL *pt = RepAppl(Tm);

	    ntp0[1] = pt[1];
	    ntp0[2] = ((2*sizeof(CELL)+EndSpecials)|MBIT);
	    ntp = ntp0+3;
	  }
	  break;
	}
      } else {
	arity = ArityOfFunctor(fun);
	ntp = MkDBTerm(RepAppl(Tm)+1,
		       RepAppl(Tm)+arity,
		       ntp0+1, ntp0+1+arity, ntp0-1,
#ifdef COROUTINING
		       &attachments,
#endif
		       &vars_found);
	if (ntp == NULL) {
	  Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
	  return(NULL);
	}
      }
    } 
    CodeAbs = (CELL *)((CELL)ntp-(CELL)ntp0);
    if (DBErrorFlag) {
      Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
      return (NULL);	/* Error Situation */
    }
    NOfCells = ntp - ntp0;	/* End Of Code Info */
#ifdef IDB_LINK_TABLE
    *lr++ = 0;
    NOfLinks = (lr - LinkAr);
#endif
    if (vars_found || InFlag & InQueue) {
      /*
       * Take into account the fact that one needs an entry
       * for the number of links 
       */
      flag = DBComplex;
#ifdef IDB_LINK_TABLE
      CodeAbs++;	/* We have one more cell */
      CodeAbs += CellPtr(lr) - CellPtr(LinkAr);
      if ((CELL *)((char *)ntp0+(CELL)CodeAbs) > AuxSp) {
	DBErrorFlag = OVF_ERROR_IN_DB;
	Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
	return(NULL);
      }
      /* restore lr to NULL in case there is a TR overflow */
      lr = NULL;
#endif
      if ((InFlag & MkIfNot) && (found_one = check_if_wvars(p->First, NOfCells, ntp0))) {
	Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
	return (found_one);
      }
    } else {
#ifdef IDB_LINK_TABLE
      /* make sure lr ends in 0 for check_if_nvars */  
      /* restore lr to NULL in case there is a TR overflow */
      lr = NULL;
#endif
      flag = DBNoVars;
      if ((InFlag & MkIfNot) && (found_one = check_if_nvars(p->First, NOfCells, ntp0))) {
	Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
	return (found_one);
      }
    }
    if (tofref != TmpRefBase) {
      CodeAbs += TmpRefBase - tofref + 1;
      if ((CELL *)((char *)ntp0+(CELL)CodeAbs) > AuxSp) {
	DBErrorFlag = OVF_ERROR_IN_DB;
	Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
	return(NULL);
      }
      flag |= DBWithRefs;
    }
#ifdef IDB_LINK_TABLE
#if SIZEOF_LINK_ENTRY==2
    if (Unsigned(CodeAbs) >= 0x40000) {
      DBErrorFlag = OTHER_ERROR_IN_DB;
      DBErrorNumber = SYSTEM_ERROR;
      DBErrorTerm = TermNil;
      DBErrorMsg = "trying to store term larger than 256KB";
      Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
      return(NULL);
    }
#endif
#endif
    pp = AllocDBSpace(DBLength(CodeAbs));
    if (pp == NIL) {
      DBErrorFlag = OVF_ERROR_IN_DB;
      DBErrorNumber = SYSTEM_ERROR;
      DBErrorTerm = TermNil;
      DBErrorMsg = "heap crashed against stacks";
      Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
      return(NULL);
    }
    pp->id = FunctorDBRef;
    INIT_LOCK(pp->lock);
    INIT_DBREF_COUNT(pp);
    pp->Flags = flag;
    if (flag & DBComplex) {
#ifdef IDB_LINK_TABLE
      link_entry         *woar;
#endif /* IDB_LINK_TABLE */

      pp->NOfCells = NOfCells;
#ifdef COROUTINING
      pp->attachments = attachments;
#endif
      if (pp0 != pp) {
	nar = pp->Contents;
#ifdef IDB_LINK_TABLE
	nar = (Term *) cpcells(CellPtr(nar), ntp0, Unsigned(NOfCells));
#endif
#ifdef IDB_USE_MBIT
	memcpy((void *)nar, (const void *)ntp0,
	       (size_t)((NOfCells+1)*sizeof(CELL)));
	nar += NOfCells+1;
#endif
      } else {
	nar = pp->Contents + Unsigned(NOfCells);
      }
#ifdef IDB_LINK_TABLE
      woar = (link_entry *)nar;
      memcpy((void *)woar,(const void *)LinkAr,(size_t)(NOfLinks*sizeof(link_entry)));
      woar += NOfLinks;
#ifdef ALIGN_LONGS
#if SIZEOF_INT_P==8
      while ((Unsigned(woar) & 7) != 0)
	woar++;		
#else
      if ((Unsigned(woar) & 3) != 0)
	woar++;
#endif
#endif
      nar = (Term *) (woar);
#endif
      pp->Entry = (CELL) tt;
    } else if (flag & DBNoVars) {
      if (pp0 != pp) {
	nar = (Term *) cpcells(CellPtr(pp->Contents), ntp0, Unsigned(NOfCells));
      } else {
#ifdef IDB_LINK_TABLE
	nar = pp->Contents + Unsigned(NOfCells)+1;
#endif
#ifdef IDB_USE_MBIT
	/* we still need to link */
	nar = (Term *) linkcells(ntp0, NOfCells);
#endif
      }
#ifdef IDB_LINK_TABLE
      linkblk(LinkAr, CellPtr(pp->Contents-1));
#endif
      pp->Entry = AdjustIDBPtr(tt,Unsigned(pp->Contents)-sizeof(CELL));
      pp->NOfCells = NOfCells;
    }
    if (flag & DBWithRefs) {
      DBRef *ptr = TmpRefBase, *rfnar = (DBRef *)nar;

      *rfnar++ = NULL;
      while (ptr != tofref)
	*rfnar++ = *--ptr;
      pp->DBRefs = rfnar;
    } else {
      pp->DBRefs = NULL;
    }      
    Yap_ReleasePreAllocCodeSpace((ADDR)pp0);
    return (pp);
  }
}

static DBRef
new_lu_index(LogUpdDBProp AtProp) {
  Int cnt = AtProp->NOfEntries, i;
  DBRef index = AllocDBSpace(DBLength((cnt+1)*sizeof(DBRef *)));
  DBRef ref = AtProp->First;
  DBRef *te;

  if (index == NULL) {
    DBErrorFlag = OTHER_ERROR_IN_DB;
    DBErrorNumber = SYSTEM_ERROR;
    DBErrorTerm = TermNil;
    DBErrorMsg = "could not allocate space";
    return(NULL);
  }
  te = (DBRef *)(index->Contents);
  for (i=0; i < cnt; i++) {
    *te++ = ref;
    ref->NOfRefsTo++;
    ref = ref->Next;
  }
  *te = NULL;
  index->id = FunctorDBRef;
  index->NOfRefsTo = 0;
  index->Prev = index->Next = NIL;
  index->Parent = (DBProp)AtProp;
  index->Flags = DBClMask|IndexMask|LogUpdMask;
  index->NOfCells = cnt;
  INIT_LOCK(index->lock);
  INIT_DBREF_COUNT(index);
  return(index);
}

static DBRef 
record(int Flag, Term key, Term t_data, Term t_code)
{
  Register Term   twork = key;
  Register DBProp p;
  Register DBRef  x;


#ifdef SFUNC
  FathersPlace = NIL;
#endif
  if (EndOfPAEntr(p = FetchDBPropFromKey(twork, Flag & MkCode, TRUE, "record/3"))) {
    return(NULL);
  }
  if ((x = CreateDBStruct(t_data, p, Flag)) == NULL) {
    return (NULL);
  }
  if ((Flag & MkIfNot) && found_one)
    return (NULL);
  TRAIL_REF(x);
  if (x->Flags & (DBNoVars|DBComplex))
    x->Mask = EvalMasks(t_data, &x->Key);
  else
    x->Mask = x->Key = 0;
  if (Flag & MkCode)
    x->Flags |= DBCode;
  else
    x->Flags |= DBNoCode;
  x->Parent = p;
#if defined(YAPOR) || defined(THREADS)
  x->Flags |= DBClMask;
  x->ref_count = 1;
#else
  x->Flags |= (InUseMask | DBClMask);
#endif
  x->NOfRefsTo = 0;
  WRITE_LOCK(p->DBRWLock);
  if (p->KindOfPE & LogUpdDBBit) {
    LogUpdDBProp lup = (LogUpdDBProp)p;
    x->Flags |= LogUpdMask;
    /* index stops being valid */
    if (lup->Index != NULL) {
      clean_lu_index(lup->Index);
      lup->Index = NULL;
    }
    lup->NOfEntries++;
  } else {
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
    x->Code = (yamop *) IntegerOfTerm(t_code);
  }
  WRITE_UNLOCK(p->DBRWLock);
  return (x);
}

/* add a new entry next to an old one */
static DBRef 
record_at(int Flag, DBRef r0, Term t_data, Term t_code)
{
  Register DBProp p;
  Register DBRef  x;


#ifdef SFUNC
  FathersPlace = NIL;
#endif
  p = r0->Parent;
  if ((x = CreateDBStruct(t_data, p, Flag)) == NULL) {
    return (NULL);
  }
  TRAIL_REF(x);
  if (x->Flags & (DBNoVars|DBComplex))
    x->Mask = EvalMasks(t_data, &x->Key);
  else
    x->Mask = x->Key = 0;
  if (Flag & MkCode)
    x->Flags |= DBCode;
  else
    x->Flags |= DBNoCode;
  x->Parent = p;
#if defined(YAPOR) || defined(THREADS)
  x->Flags |= DBClMask;
  x->ref_count = 1;
#else
  x->Flags |= (InUseMask | DBClMask);
#endif
  x->NOfRefsTo = 0;
  WRITE_LOCK(p->DBRWLock);
  if (p->KindOfPE & LogUpdDBBit) {
    LogUpdDBProp lup = (LogUpdDBProp)p;
    x->Flags |= LogUpdMask;
    /* index stops being valid */
    if (lup->Index != NULL) {
      clean_lu_index(lup->Index);
      lup->Index = NULL;
    }
    lup->NOfEntries++;
  } else {
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
    x->Code = (yamop *) IntegerOfTerm(t_code);
  }
  WRITE_UNLOCK(p->DBRWLock);
  return (x);
}

/* recorda(+Functor,+Term,-Ref) */
static Int 
p_rcda(void)
{
  /* Idiotic xlc's cpp does not work with ARG1 within MkDBRefTerm */
  Term            TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
 restart_record:
  TRef = MkDBRefTerm(record(MkFirst, t1, t2, Unsigned(0)));
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    return (Yap_unify(ARG3, TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  goto restart_record;
}

/* '$recordap'(+Functor,+Term,-Ref) */
static Int 
p_rcdap(void)
{
  Term            TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);

  if (!IsVarTerm(Deref(ARG3)))
    return FALSE;
 restart_record:
  TRef = MkDBRefTerm(record(MkFirst | MkCode, t1, t2, Unsigned(0)));
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    return Yap_unify(ARG3, TRef);
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return FALSE;
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
    return FALSE;
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return FALSE;
    } else {
      goto recover_record;
    }
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return FALSE;
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  goto restart_record;
}

/* recorda_at(+Functor,+Term,-Ref) */
static Int 
p_rcda_at(void)
{
  /* Idiotic xlc's cpp does not work with ARG1 within MkDBRefTerm */
  Term            TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  if (IsVarTerm(t1)) {
      Yap_Error(INSTANTIATION_ERROR, t1, "recorda_at/3");
      return(FALSE);
  }
  if (!IsDBRefTerm(t1)) {
      Yap_Error(TYPE_ERROR_DBREF, t1, "recorda_at/3");
      return(FALSE);
  }
 restart_record:
  TRef = MkDBRefTerm(record_at(MkFirst, DBRefOfTerm(t1), t2, Unsigned(0)));
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    return (Yap_unify(ARG3, TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  goto restart_record;
}

/* recordz(+Functor,+Term,-Ref) */
static Int 
p_rcdz(void)
{
  Term            TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
 restart_record:
  TRef = MkDBRefTerm(record(MkLast, t1, t2, Unsigned(0)));
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    return (Yap_unify(ARG3, TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  goto restart_record;
}

/* '$recordzp'(+Functor,+Term,-Ref) */
static Int 
p_rcdzp(void)
{
  Term            TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
 restart_record:
  TRef = MkDBRefTerm(record(MkLast | MkCode, t1, t2, Unsigned(0)));
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    return (Yap_unify(ARG3, TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  goto restart_record;
}

/* recordz_at(+Functor,+Term,-Ref) */
static Int 
p_rcdz_at(void)
{
  /* Idiotic xlc's cpp does not work with ARG1 within MkDBRefTerm */
  Term            TRef, t1 = Deref(ARG1), t2 = Deref(ARG2);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  if (IsVarTerm(t1)) {
      Yap_Error(INSTANTIATION_ERROR, t1, "recordz_at/3");
      return(FALSE);
  }
  if (!IsDBRefTerm(t1)) {
      Yap_Error(TYPE_ERROR_DBREF, t1, "recordz_at/3");
      return(FALSE);
  }
 restart_record:
  TRef = MkDBRefTerm(record_at(MkLast, DBRefOfTerm(t1), t2, Unsigned(0)));
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    return (Yap_unify(ARG3, TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recordz_at/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  goto restart_record;
}

/* '$record_stat_source'(+Functor,+Term) */
static Int 
p_rcdstatp(void)
{
  Term t1 = Deref(ARG1), t2 = Deref(ARG2), t3 = Deref(ARG3);
  int mk_first;
  Term TRef;

  if (IsVarTerm(t3) || !IsIntTerm(t3))
    return (FALSE);
  if (IsVarTerm(t3) || !IsIntTerm(t3))
    return (FALSE);
  mk_first = ((IntOfTerm(t3) % 4) == 2);
 restart_record:
  if (mk_first)
    TRef = MkDBRefTerm(record(MkFirst | MkCode, t1, t2, MkIntTerm(0)));
  else
    TRef = MkDBRefTerm(record(MkLast | MkCode, t1, t2, MkIntTerm(0)));
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    return (Yap_unify(ARG4,TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in record_stat_source/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  goto restart_record;
}

/* '$recordap'(+Functor,+Term,-Ref,+CRef) */
static Int 
p_drcdap(void)
{
  Term            TRef, t1 = Deref(ARG1), t2 = Deref(ARG2), t4 = Deref(ARG4);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  if (IsVarTerm(t4) || !IsIntegerTerm(t4))
    return (FALSE);
 restart_record:
  TRef = MkDBRefTerm(record(MkFirst | MkCode | WithRef,
			    t1, t2, t4));
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    return (Yap_unify(ARG3, TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(4, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  t4 = Deref(ARG4);
  goto restart_record;
}

/* '$recordzp'(+Functor,+Term,-Ref,+CRef) */
static Int 
p_drcdzp(void)
{
  Term            TRef, t1 = Deref(ARG1), t2 = Deref(ARG2), t4 =  Deref(ARG4);

  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  if (IsVarTerm(t4) || !IsIntegerTerm(t4))
    return (FALSE);
 restart_record:
  TRef = MkDBRefTerm(record(MkLast | MkCode | WithRef,
			    t1, t2, t4));
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    return (Yap_unify(ARG3, TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(4, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  t4 = Deref(ARG4);
  goto restart_record;
}

/* '$recordaifnot'(+Functor,+Term,-Ref) */
static Int 
p_rcdaifnot(void)
{
  Term            TRef;
  DBRef           db_ref;

 restart_record:
  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  found_one = NIL;
  db_ref = record(MkFirst | MkIfNot, Deref(ARG1), Deref(ARG2), Unsigned(0));
  if (db_ref == NULL)
    return(FALSE);
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    TRef = MkDBRefTerm(db_ref);
    return (Yap_unify(ARG3, TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  goto restart_record;
}

/* '$recordzifnot'(+Functor,+Term,-Ref) */
static Int 
p_rcdzifnot(void)
{
  Term            TRef;
  DBRef           db_ref;

 restart_record:
  if (!IsVarTerm(Deref(ARG3)))
    return (FALSE);
  found_one = NIL;
  db_ref = record(MkLast | MkIfNot, Deref(ARG1), Deref(ARG2), Unsigned(0));
  if (db_ref == NULL)
    return(FALSE);
  switch(DBErrorFlag) {
  case NO_ERROR_IN_DB:
    TRef = MkDBRefTerm(db_ref);
    return (Yap_unify(ARG3, TRef));
  case SOVF_ERROR_IN_DB:
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    goto recover_record;
  case TOVF_ERROR_IN_DB:
    Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
    return(FALSE);
  case OVF_ERROR_IN_DB:
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    } else
      goto recover_record;
  default:
    Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
    return(FALSE);
  }
 recover_record:
  DBErrorFlag = NO_ERROR_IN_DB;
  goto restart_record;
}

#ifdef COROUTINING
static void
copy_attachments(CELL *ts)
{
  while (TRUE) {
    Term t;
    /* store away in case there is an overflow */
    *--ASP = ts[3];
    attas[IntegerOfTerm(ts[2])].term_to_op(ts[1], ts[0]);
    t = *ASP;
    ASP++;
    if (t == TermNil) return;
    ts = RepAppl(t)+1;
  }
}
#endif

static int 
UnifyDBKey(DBRef DBSP, PropFlags flags, Term t)
{
  DBProp p = DBSP->Parent;
  Term t1, tf;

  READ_LOCK(p->DBRWLock);
  /* get the key */
  if (p->ArityOfDB == 0) {
    t1 = MkAtomTerm((Atom)(p->FunctorOfDB));
  } else {
    t1 = Yap_MkNewApplTerm(p->FunctorOfDB,p->ArityOfDB);
  }
  if ((p->KindOfPE & CodeDBBit) && (flags & CodeDBBit)) {
    Term t[2];
    t[0] = ModuleName[p->ModuleOfDB];
    t[1] = t1;
    tf = Yap_MkApplTerm(FunctorModule, 2, t);
  } else if (!(flags & CodeDBBit)) {
    tf = t1;
  } else {
    return FALSE;
  }
  READ_UNLOCK(p->DBRWLock);
  return(Yap_unify(tf,t));
}


static int 
UnifyDBNumber(DBRef DBSP, Term t)
{
  DBProp p = DBSP->Parent;
  DBRef ref;
  Int i = 1;

  READ_LOCK(p->DBRWLock);
  ref = p->First;
  while (ref != NIL) {
    if (ref == DBSP) break;
    if (!DEAD_REF(ref)) i++;
    ref = ref->Next;
  }
  if (ref == NIL)
    return FALSE;
  READ_UNLOCK(p->DBRWLock);
  return(Yap_unify(MkIntegerTerm(i),t));
}


static Term 
GetDBTerm(DBRef DBSP)
{
  if (DBSP->Flags & (DBNoVars | DBAtomic))
    return ((Term) DBSP->Entry);
  if (DBSP->Flags & DBComplex) {
    CELL           *HOld = H;
    CELL           *HeapPtr;
    CELL           *pt;
    CELL            NOf;

    pt = CellPtr(DBSP->Contents);
    NOf = DBSP->NOfCells;
    if (H+NOf > ASP-CalculateStackGap()) {
      if (Yap_PrologMode & InErrorMode) {
	if (H+NOf > ASP)
	  fprintf(Yap_stderr, "\n\n [ FATAL ERROR: No Stack for Error Handling ]\n");
	  Yap_exit( 1);
      } else {
	return((Term)0);
      }
    }
    HeapPtr = cpcells(HOld, pt, NOf);
    pt += HeapPtr - HOld;
    H = HeapPtr;
#ifdef IDB_LINK_TABLE
    {
      link_entry *lp = (link_entry *)pt;
      linkblk(lp, HOld-1);
    }
#endif
#ifdef COROUTINING
    if (DBSP->attachments != 0L)  {
      *--ASP = (CELL)HOld;
      copy_attachments((CELL *)AdjustIDBPtr(DBSP->attachments,(CELL)(HOld-1)));
      HOld = CellPtr(*ASP++);
    }
#endif
    return (AdjustIDBPtr((Term)(DBSP->Entry),Unsigned(HOld)-sizeof(CELL)));
  }
  return (MkVarTerm());
}

static void
init_int_keys(void) {
  INT_KEYS = (Prop *)Yap_AllocCodeSpace(sizeof(Prop)*INT_KEYS_SIZE);
  if (INT_KEYS != NULL) {
    UInt i = 0;
    Prop *p = INT_KEYS;
    for (i = 0; i < INT_KEYS_SIZE; i++) {
      p[0] = NIL;
      p++;
    }
  }
}

static int
resize_int_keys(UInt new_size) {
  Prop *new;
  UInt i;

  YAPEnterCriticalSection();
  if (INT_KEYS == NULL) {
    INT_KEYS_SIZE = new_size;
    YAPLeaveCriticalSection();
    return(TRUE);
  }
  new = (Prop *)Yap_AllocCodeSpace(sizeof(Prop)*new_size);
  if (new == NULL) {
    YAPLeaveCriticalSection();
    DBErrorFlag = OTHER_ERROR_IN_DB;
    DBErrorNumber = SYSTEM_ERROR;
    DBErrorTerm = TermNil;
    DBErrorMsg = "could not allocate space";
    return(FALSE);
  }
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
  Yap_FreeCodeSpace((char *)INT_KEYS);
  INT_KEYS = new;
  INT_KEYS_SIZE = new_size;
  INT_KEYS_TIMESTAMP++;
  if (INT_KEYS_TIMESTAMP == MAX_ABS_INT)
    INT_KEYS_TIMESTAMP = 0;
  YAPLeaveCriticalSection();
  return(TRUE);
}

static DBProp
FetchIntDBPropFromKey(Int key, int flag, int new, char *error_mssg)
{
  Functor fun = (Functor)key;
  UInt hash_key = (CELL)key % INT_KEYS_SIZE;
  Prop p0;

  if (INT_KEYS == NULL) {
    init_int_keys();
    if (INT_KEYS == NULL) {
      DBErrorFlag = OTHER_ERROR_IN_DB;
      DBErrorNumber = SYSTEM_ERROR;
      DBErrorTerm = TermNil;
      DBErrorMsg = "could not allocate space";
      return(NULL);
    }
  }
  p0 = INT_KEYS[hash_key];
  while (p0 != NIL) {
    DBProp p = RepDBProp(p0);
    if (p->FunctorOfDB == fun) return(p);
    p0 = p->NextOfPE;
  }
  /* p is NULL, meaning we did not find the functor */
  if (new) {
    DBProp p;
    /* create a new DBProp				 */
    if (UPDATE_MODE == UPDATE_MODE_LOGICAL
	|| (UPDATE_MODE == UPDATE_MODE_LOGICAL_ASSERT && (flag & MkCode))) {
      LogUpdDBProp lup = (LogUpdDBProp) Yap_AllocAtomSpace(sizeof(*lup));
      lup->KindOfPE = LogUpdDBProperty|flag;
      lup->NOfEntries = 0;
      lup->Index = NULL;
      p = (DBProp)lup;
    } else {
      p = (DBProp) Yap_AllocAtomSpace(sizeof(*p));
      p->KindOfPE = DBProperty|flag;
      p->F0 = p->L0 = NULL;
    }
    p->ArityOfDB = 0;
    p->First = p->Last = NULL;
    p->ModuleOfDB = 0;
    p->FunctorOfDB = fun;
    p->NextOfPE = INT_KEYS[hash_key];
    INIT_RWLOCK(p->DBRWLock);
    INT_KEYS[hash_key] = AbsDBProp(p);
    return(p);
  } else {
    return(RepDBProp(NULL));
  }
}

static DBProp
FetchDBPropFromKey(Term twork, int flag, int new, char *error_mssg)
{
  Atom At;
  Int arity;
  SMALLUNSGN dbmod;

  if (flag & MkCode) {
    if (IsVarTerm(twork)) {
      Yap_Error(INSTANTIATION_ERROR, twork, error_mssg);
      return(RepDBProp(NIL));
    }
    if (!IsApplTerm(twork)) {
      Yap_Error(SYSTEM_ERROR, twork, "missing module");
      return(RepDBProp(NIL));
    } else {
      Functor f = FunctorOfTerm(twork);
      Term tmod;
      if (f != FunctorModule) {
	Yap_Error(SYSTEM_ERROR, twork, "missing module");
	return(RepDBProp(NIL));
      }
      tmod = ArgOfTerm(1, twork);
      if (IsVarTerm(tmod)) {
	Yap_Error(INSTANTIATION_ERROR, twork, "var in module");
	return(RepDBProp(NIL));
      }
      if (!IsAtomTerm(tmod)) {
	Yap_Error(TYPE_ERROR_ATOM, twork, "not atom in module");
	return(RepDBProp(NIL));
      }
      dbmod = Yap_LookupModule(tmod);
      twork = ArgOfTerm(2, twork);
    }
  } else
    dbmod = 0;
  if (IsVarTerm(twork)) {
    Yap_Error(INSTANTIATION_ERROR, twork, error_mssg);
    return(RepDBProp(NIL));
  } else if (IsAtomTerm(twork)) {
    arity = 0, At = AtomOfTerm(twork);
  } else if (IsIntegerTerm(twork)) {
    return(FetchIntDBPropFromKey(IntegerOfTerm(twork), flag, new, error_mssg));
  } else if (IsApplTerm(twork)) {
    Register Functor f = FunctorOfTerm(twork);
    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_KEY, twork, error_mssg);
      return(RepDBProp(NIL));
    }
    At = NameOfFunctor(f);
    arity = ArityOfFunctor(f);
  } else if (IsPairTerm(twork)) {
    At = AtomDot;
    arity = 2;
  } else {
    Yap_Error(TYPE_ERROR_KEY, twork,error_mssg);
    return(RepDBProp(NIL));
  }
  if (new) {
    DBProp p;
    AtomEntry *ae = RepAtom(At);

    WRITE_LOCK(ae->ARWLock);
    if (EndOfPAEntr(p = RepDBProp(FindDBPropHavingLock(ae, flag, arity, dbmod)))) {
     /* create a new DBProp				 */
      int OLD_UPDATE_MODE = UPDATE_MODE;
      if (flag & MkCode) {
	PredEntry *pp;
	pp = RepPredProp(Yap_GetPredPropHavingLock(At, arity, dbmod));

	if (!EndOfPAEntr(pp)) {
	  READ_LOCK(pp->PRWLock);
	  if(pp->PredFlags & LogUpdatePredFlag)
	    UPDATE_MODE = UPDATE_MODE_LOGICAL;
	  READ_UNLOCK(pp->PRWLock);
	}

      }
      if (UPDATE_MODE == UPDATE_MODE_LOGICAL
	  || (UPDATE_MODE == UPDATE_MODE_LOGICAL_ASSERT && (flag & MkCode))) {
	LogUpdDBProp lup = (LogUpdDBProp) Yap_AllocAtomSpace(sizeof(*lup));
	lup->KindOfPE = LogUpdDBProperty|flag;
	lup->NOfEntries = 0;
	lup->Index = NULL;
	p = (DBProp)lup;
      } else {
	p = (DBProp) Yap_AllocAtomSpace(sizeof(*p));
	p->KindOfPE = DBProperty|flag;
	p->F0 = p->L0 = NULL;
      }
      UPDATE_MODE = OLD_UPDATE_MODE;
      p->ArityOfDB = arity;
      p->First = p->Last = NIL;
      p->ModuleOfDB = dbmod;
      /* This is NOT standard but is QUITE convenient */
      INIT_RWLOCK(p->DBRWLock);
      if (arity == 0)
	p->FunctorOfDB = (Functor) At;
      else
	p->FunctorOfDB = Yap_UnlockedMkFunctor(ae,arity);
      p->NextOfPE = ae->PropsOfAE;
      ae->PropsOfAE = AbsDBProp(p);
    }
    WRITE_UNLOCK(ae->ARWLock);
    return(p);
  } else
    return(RepDBProp(FindDBProp(RepAtom(At), flag, arity, dbmod)));
}


static DBRef 
nth_recorded_log(LogUpdDBProp AtProp, Int Count)
{
  DBRef ref;

  if (AtProp->NOfEntries == 0) {
    READ_UNLOCK(AtProp->DBRWLock);
    return FALSE;
  }
  if (Count > AtProp->NOfEntries) {
    READ_UNLOCK(AtProp->DBRWLock);
    return FALSE;
  }
  if (AtProp->NOfEntries == 1) {
    ref = AtProp->First;
  } else {
    if (AtProp->Index == NULL) {
      while((AtProp->Index = new_lu_index(AtProp)) == NULL) {
	if (!Yap_growheap(FALSE)) {
	  Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	  READ_UNLOCK(AtProp->DBRWLock);
	  return NULL;
	}
      }
    }
    ref = ((DBRef *)(AtProp->Index->Contents))[Count-1];
  }
  return ref;
}


/* Finds a term recorded under the key ARG1			 */
static Int 
nth_recorded(DBProp AtProp, Int Count)
{
  Register DBRef  ref;

  READ_LOCK(AtProp->DBRWLock);
  if (AtProp->KindOfPE & 0x1) {
    ref = nth_recorded_log((LogUpdDBProp)AtProp, Count);
    if (ref == NULL) {
      READ_UNLOCK(AtProp->DBRWLock);
      return FALSE;
    }
  } else {
    ref = AtProp->First;
    Count--;
    while (ref != NULL
	   && DEAD_REF(ref))
      ref = NextDBRef(ref);
    if (ref == NULL) {
      READ_UNLOCK(AtProp->DBRWLock);
      return FALSE;
    }
    while (Count) {
      Count--;
      ref = NextDBRef(ref);
      while (ref != NULL
	     && DEAD_REF(ref))
	ref = NextDBRef(ref);
      if (ref == NULL) {
	READ_UNLOCK(AtProp->DBRWLock);
	return FALSE;
      }
    }
  }
#if defined(YAPOR) || defined(THREADS)
  LOCK(ref->lock);
  READ_UNLOCK(AtProp->DBRWLock);
  TRAIL_REF(ref);		/* So that fail will erase it */
  INC_DBREF_COUNT(ref);
  UNLOCK(ref->lock);
#else
  if (!(ref->Flags & InUseMask)) {
    ref->Flags |= InUseMask;
    TRAIL_REF(ref);	/* So that fail will erase it */
  }
  READ_UNLOCK(AtProp->DBRWLock);
#endif
  return Yap_unify(MkDBRefTerm(ref),ARG3);
}

static Int
p_nth_instance(void)
{
  DBProp          AtProp;
  Term            TCount;
  Int             Count;
  Term t3 = Deref(ARG3);

  if (!IsVarTerm(t3)) {
    if (!IsDBRefTerm(t3)) {
      Yap_Error(TYPE_ERROR_DBREF,t3,"nth_instance/3");
      return FALSE;
    } else {
      DBRef ref = DBRefOfTerm(t3);
      LOCK(ref->lock);
      if (ref == NULL
	  || DEAD_REF(ref)
	  || !UnifyDBKey(ref,0,ARG1)
	  || !UnifyDBNumber(ref,ARG2)) {
	UNLOCK(ref->lock);
	return(FALSE);
      } else {
	UNLOCK(ref->lock);
	return(TRUE);
      }
    }
  }
  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(Deref(ARG1), 0, FALSE, "nth_instance/3"))) {
    return(FALSE);
  }
  TCount = Deref(ARG2);
  if (IsVarTerm(TCount)) {
    Yap_Error(INSTANTIATION_ERROR, TCount, "nth_instance/3");
    return (FALSE);
  }
  if (!IsIntegerTerm(TCount)) {
    Yap_Error(TYPE_ERROR_INTEGER, TCount, "nth_instance/3");
    return (FALSE);
  }
  Count = IntegerOfTerm(TCount);
  if (Count <= 0) {
    if (Count) 
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, TCount, "nth_instance/3");
    else
      Yap_Error(DOMAIN_ERROR_NOT_ZERO, TCount, "nth_instance/3");
    return (FALSE);
  }
  return nth_recorded(AtProp,Count);
}

static Int
p_nth_instancep(void)
{
  DBProp          AtProp;
  Term            TCount;
  Int             Count;
  Term            t3 = Deref(ARG3);

  if (!IsVarTerm(t3)) {
    if (!IsDBRefTerm(t3)) {
      Yap_Error(TYPE_ERROR_DBREF,t3,"nth_instance/3");
      return FALSE;
    } else {
      DBRef ref = DBRefOfTerm(t3);
      LOCK(ref->lock);
      if (ref == NULL
	  || DEAD_REF(ref)
	  || !UnifyDBKey(ref,CodeDBBit,ARG1)
	  || !UnifyDBNumber(ref,ARG2)) {
	UNLOCK(ref->lock);
	return(FALSE);
      } else {
	UNLOCK(ref->lock);
	return(TRUE);
      }
    }
  }
  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(Deref(ARG1), MkCode, FALSE, "nth_instance/3"))) {
    return(FALSE);
  }
  TCount = Deref(ARG2);
  if (IsVarTerm(TCount)) {
    Yap_Error(INSTANTIATION_ERROR, TCount, "recorded_at/4");
    return (FALSE);
  }
  if (!IsIntegerTerm(TCount)) {
    Yap_Error(TYPE_ERROR_INTEGER, TCount, "recorded_at/4");
    return (FALSE);
  }
  Count = IntegerOfTerm(TCount);
  if (Count <= 0) {
    if (Count) 
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, TCount, "recorded_at/4");
    else
      Yap_Error(DOMAIN_ERROR_NOT_ZERO, TCount, "recorded_at/4");
    return (FALSE);
  }
  return nth_recorded(AtProp,Count);
}

/* Finds a term recorded under the key ARG1			 */
static Int 
i_log_upd_recorded(LogUpdDBProp AtProp)
{
  Term            TermDB, TRef;
  Register DBRef  ref;
  CELL           *PreviousHeap;
  CELL            mask = 0, key = 0;
  DBRef          *ep;
  DBRef table, rtable[1];
  Term twork;

  
  if (AtProp->NOfEntries == 0) {
    READ_UNLOCK(AtProp->DBRWLock);
    cut_fail();
  }
  else if (AtProp->NOfEntries == 1) {
    ep = rtable;
    ref = AtProp->First;
    rtable[0] = NIL;
  } else {
    if (AtProp->Index == NULL) {
      while((AtProp->Index = new_lu_index(AtProp)) == NULL) {
	if (!Yap_growheap(FALSE)) {
	  Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	  return FALSE;
	}
	twork = Deref(ARG2);
      }
    }
    table = AtProp->Index;
    ep = (DBRef *)(table->Contents);
    ref = *ep++;
  }
  READ_UNLOCK(AtProp->DBRWLock);

  while (ref != NIL && DEAD_REF(ref))
    ref = *ep++;
  if (ref == NIL) {
    cut_fail();
  }
  twork = Deref(ARG2);	/* now working with ARG2 */
  if (IsVarTerm(twork)) {
    mask = key = 0;
    /* EXTRA_CBACK_ARG(3,2) is being assigned the value MkIntegerTerm(((Int)mask)),
       and EXTRA_CBACK_ARG(3,3) the value MkIntegerTerm(((Int)key)) which is here 0.
       This is working around a bug in the O2 optimiser of the compiler that ships
       with HP-UX 10.20: cc Rel. 10.32.30, April 1999. */
    EXTRA_CBACK_ARG(3,2) = MkIntegerTerm(((Int)0));
    EXTRA_CBACK_ARG(3,3) = MkIntegerTerm(((Int)0));
    B->cp_h = H;
    while ((TermDB = GetDBTerm(ref)) == (CELL)0) {
      /* make sure the garbage collector sees what we want it to see! */
      EXTRA_CBACK_ARG(3,1) = AbsAppl((CELL *)ep);
      /* oops, we are in trouble, not enough stack space */
      if (!Yap_gc(3, ENV, CP)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      }
      PreviousHeap = H;
      twork = Deref(ARG2);
    }
    if (!Yap_unify(twork, TermDB)) {
      cut_fail();
    }
  } else if (IsAtomOrIntTerm(twork)) {
    /* EXTRA_CBACK_ARG(3,2) is being assigned the value MkIntegerTerm(((Int)mask)),
       which is here 0. This is working around a bug in the O2 optimiser of the
       compiler that ships with HP-UX 10.20: cc Rel. 10.32.30, April 1999. */
    mask = 0;
    EXTRA_CBACK_ARG(3,2) = MkIntegerTerm(((Int)0));
    key = Unsigned(twork);
    EXTRA_CBACK_ARG(3,3) = MkIntegerTerm(((Int)key));
    B->cp_h = H;
    do {
      if (((twork == ref->Entry) || (ref->Flags & DBVar)) &&
	  !DEAD_REF(ref))
	break;
      ref =*ep++ ;
      if (ref == NIL) {
	cut_fail();
      }
    } while (TRUE);
  } else {
    mask = EvalMasks(twork, &key);
    EXTRA_CBACK_ARG(3,2) = MkIntegerTerm(((Int)mask));
    EXTRA_CBACK_ARG(3,3) = MkIntegerTerm(((Int)key));
    PreviousHeap = H;
    B->cp_h = H;
    do {
      H = PreviousHeap;
      while ((mask & ref->Key) != (key & ref->Mask)) {
	while ((ref = *ep++) != NIL
	       && DEAD_REF(ref));
	if (ref == NIL) {
	  cut_fail();
	}
      }
      while ((TermDB = GetDBTerm(ref)) == (CELL)0) {
	/* make sure the garbage collector sees what we want it to see! */
	EXTRA_CBACK_ARG(3,1) = AbsAppl((CELL *)ep);
	/* oops, we are in trouble, not enough stack space */
	if(!Yap_gc(3, ENV, CP)) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	  return(FALSE);
	}
	PreviousHeap = H;
      }
      if (Yap_unify(ARG2, TermDB))
	break;
      while ((ref = *ep++) != NIL
	     && DEAD_REF(ref));
      if (ref == NIL) {
	cut_fail();
      }
    } while (TRUE);
  }
  /* This should be after any non-tagged terms, because the routines in grow.c
     go from upper to lower addresses */
  TRef = MkDBRefTerm(ref);
  if (*ep == NULL) {
    if (Yap_unify(ARG3, TRef)) {
#if defined(OR) || defined(THREADS)
      LOCK(ref->lock);
      TRAIL_REF(ref);		/* So that fail will erase it */
      INC_DBREF_COUNT(ref);
      UNLOCK(ref->lock);
#else
      if (!(ref->Flags & InUseMask)) {
	ref->Flags |= InUseMask;
	TRAIL_REF(ref);	/* So that fail will erase it */
      }
#endif
      cut_succeed();
    } else {
      cut_fail();
    }
  } else {
    DBRef table = AtProp->Index;

    EXTRA_CBACK_ARG(3,1) = AbsAppl((CELL *)ep);
#if defined(YAPOR) || defined(THREADS)
    LOCK(table->lock);
    TRAIL_REF(table);		/* So that fail will erase it */
    INC_DBREF_COUNT(table);
    UNLOCK(table->lock);
    LOCK(ref->lock);
    TRAIL_REF(ref);		/* So that fail will erase it */
    INC_DBREF_COUNT(ref);
    UNLOCK(ref->lock);
#else
    if (!(table->Flags & InUseMask)) {
      table->Flags |= InUseMask;
      if (B->cp_tr == TR) {
	TRAIL_REF(table);		/* So that fail will erase it */
	
	B->cp_tr = TR;    /* protect this entry so that it will not be
			     undone by backtracking */
	
      } else {
	Term new;
	TRAIL_REF(table);		/* So that fail will erase it */
	/* swap this with what was pointed by the choicepoint */
	new = TrailTerm(TR-1);
	TrailTerm(TR-1) = TrailTerm(B->cp_tr);
	TrailTerm(B->cp_tr) = new;
#if defined(TABLING) || defined(SBA)
	{
	  CELL val;
	  val = TrailVal(TR-1);
	  TrailVal(TR-1) = TrailVal(B->cp_tr);
	  TrailVal(B->cp_tr) = val;
	}
#endif
	B->cp_tr++;
      }
    }
    if (!(ref->Flags & InUseMask)) {
      ref->Flags |= InUseMask;
      TRAIL_REF(ref);		/* So that fail will erase it */
    }
#endif
    return (Yap_unify(ARG3, TRef));
  }
}

static Int
p_db_key(void)
{
  Register Term   twork = Deref(ARG1);	/* fetch the key */
  DBProp          AtProp;

  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(twork, 0, TRUE, "db_key/3"))) {
    /* should never happen */
    return(FALSE);
  }
  return(Yap_unify(ARG2,MkIntegerTerm((Int)AtProp)));
}

/* Finds a term recorded under the key ARG1			 */
static Int 
i_recorded(DBProp AtProp, Term t3)
{
  Term            TermDB, TRef;
  Register DBRef  ref;
  Term twork;

  READ_LOCK(AtProp->DBRWLock);
  if (AtProp->KindOfPE & 0x1)
    return(i_log_upd_recorded((LogUpdDBProp)AtProp));
  ref = AtProp->First;
  while (ref != NULL
	 && DEAD_REF(ref))
    ref = NextDBRef(ref);
  READ_UNLOCK(AtProp->DBRWLock);
  if (ref == NULL) {
    cut_fail();
  }
  twork = Deref(ARG2);	/* now working with ARG2 */
  if (IsVarTerm(twork)) {
    EXTRA_CBACK_ARG(3,2) = MkIntegerTerm(0);
    EXTRA_CBACK_ARG(3,3) = MkIntegerTerm(0);
    B->cp_h = H;
    while ((TermDB = GetDBTerm(ref)) == (CELL)0) {
      /* make sure the garbage collector sees what we want it to see! */
      EXTRA_CBACK_ARG(3,1) = (CELL)ref;
      /* oops, we are in trouble, not enough stack space */
      if (!Yap_gc(3, ENV, CP)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      }
      twork = Deref(ARG2);
      t3 = Deref(ARG3);
    }
    if (!Yap_unify(twork, TermDB)) {
      cut_fail();
    }
  } else if (IsAtomOrIntTerm(twork)) {
    EXTRA_CBACK_ARG(3,2) = MkIntegerTerm(0);
    EXTRA_CBACK_ARG(3,3) = MkIntegerTerm((Int)twork);
    B->cp_h = H;
    READ_LOCK(AtProp->DBRWLock);
    do {
      if (((twork == ref->Entry) || IsVarTerm(ref->Entry)) &&
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

    B->cp_h = H;
    READ_LOCK(AtProp->DBRWLock);
    do {
      while ((mask & ref->Key) != (key & ref->Mask) && !DEAD_REF(ref)) {
	ref = NextDBRef(ref);
	if (ref == NULL) {
	  READ_UNLOCK(AtProp->DBRWLock);
	  cut_fail();
	}
      }
      if ((TermDB = GetDBTerm(ref)) != (CELL)0) {
	if (Yap_unify(TermDB, ARG2)) {
	  /* success */
	  EXTRA_CBACK_ARG(3,2) = MkIntegerTerm(((Int)mask));
	  EXTRA_CBACK_ARG(3,3) = MkIntegerTerm(((Int)key));
	  B->cp_h = H;
	  break;
	} else {
	  while ((ref = NextDBRef(ref)) != NULL
		 && DEAD_REF(ref));
	  if (ref == NULL) {
	    READ_UNLOCK(AtProp->DBRWLock);
	    cut_fail();
	  }
	}
      } else {
	/* make sure the garbage collector sees what we want it to see! */
	EXTRA_CBACK_ARG(3,1) = (CELL)ref;
	READ_UNLOCK(AtProp->DBRWLock);
	EXTRA_CBACK_ARG(3,2) = MkIntegerTerm(((Int)mask));
	EXTRA_CBACK_ARG(3,3) = MkIntegerTerm(((Int)key));
	/* oops, we are in trouble, not enough stack space */
	if (!Yap_gc(3, ENV, CP)) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	  return(FALSE);
	}
	READ_LOCK(AtProp->DBRWLock);
      }
    } while (TRUE);
    READ_UNLOCK(AtProp->DBRWLock);
  }
  EXTRA_CBACK_ARG(3,1) = (CELL)ref;
  /* This should be after any non-tagged terms, because the routines in grow.c
     go from upper to lower addresses */
  TRef = MkDBRefTerm(ref);
#if defined(YAPOR) || defined(THREADS)
  LOCK(ref->lock);
  TRAIL_REF(ref);		/* So that fail will erase it */
  INC_DBREF_COUNT(ref);
  UNLOCK(ref->lock);
#else
  if (!(ref->Flags & InUseMask)) {
    ref->Flags |= InUseMask;
    TRAIL_REF(ref);		/* So that fail will erase it */
  }
#endif
  return (Yap_unify(ARG3, TRef));
}

static Int 
c_log_upd_recorded(DBRef *ep, int flags)
{
  Term            TermDB, TRef;
  Register DBRef  ref;
  CELL           *PreviousHeap = H;
  CELL            mask, key;


  ref = *ep++;
  if (ref == NIL) {
    cut_fail();
  }
  {
    Term ttmp = EXTRA_CBACK_ARG(3,2);
    if (IsLongIntTerm(ttmp))
      mask = (CELL)LongIntOfTerm(ttmp);
    else
      mask = (CELL)IntOfTerm(ttmp);
  }
  {
    Term ttmp = EXTRA_CBACK_ARG(3,3);
    if (IsLongIntTerm(ttmp))
      key = (CELL)LongIntOfTerm(ttmp);
    else
      key = (CELL)IntOfTerm(ttmp);
  }
  while (ref != NIL
	 && DEAD_REF(ref))
    ref = *ep++;
  if (ref == NIL) {
    cut_fail();
  }
  if (mask == 0 && key == 0) {	/* ARG2 is a variable */
    while ((TermDB = GetDBTerm(ref)) == (CELL)0) {
      /* make sure the garbage collector sees what we want it to see! */
      EXTRA_CBACK_ARG(3,1) = AbsAppl((CELL *)ep);
      /* oops, we are in trouble, not enough stack space */
      if (!Yap_gc(3, ENV, CP)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      }
      PreviousHeap = H;
    }
    Yap_unify(ARG2, TermDB);
  } else if (mask == 0) {	/* ARG2 is a constant */
    do {
      if (((key == Unsigned(ref->Entry)) || (ref->Flags & DBVar)) &&
	  !DEAD_REF(ref))
	break;
      ref = *ep++;
    } while (ref != NIL);
    if (ref == NIL) {
      cut_fail();
    }
  } else
    do {		/* ARG2 is a structure */
      H = PreviousHeap;
      while ((mask & ref->Key) != (key & ref->Mask)) {
	while ((ref = *ep++) != NIL
	       && DEAD_REF(ref));
	if (ref == NIL) {
	  cut_fail();
	}
      }
      while ((TermDB = GetDBTerm(ref)) == (CELL)0) {
	/* make sure the garbage collector sees what we want it to see! */
	EXTRA_CBACK_ARG(3,1) = AbsAppl((CELL *)ep);
	/* oops, we are in trouble, not enough stack space */
	if (!Yap_gc(3, ENV, CP)) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	  return(FALSE);
	}
	PreviousHeap = H;
      }
      if (Yap_unify(ARG2, TermDB))
	break;
      while ((ref = *ep++) != NIL
	     && DEAD_REF(ref));
      if (ref == NIL) {
	cut_fail();
      }
    } while (1);
  TRef = MkDBRefTerm(ref);
  EXTRA_CBACK_ARG(3,1) = AbsAppl((CELL *)ep);
#if defined(YAPOR) || defined(THREADS)
  LOCK(ref->lock);
  TRAIL_REF(ref);	/* So that fail will erase it */
  INC_DBREF_COUNT(ref);
  UNLOCK(ref->lock);
#else 
 if (!(ref->Flags & InUseMask)) {
    ref->Flags |= InUseMask;
    TRAIL_REF(ref);	/* So that fail will erase it */
  }
#endif
  if (!Yap_unify(ARG3, TRef))
    return(FALSE); /* should never happen */
  if (*ep == NULL) {
    cut_succeed();
  } else {
    return(TRUE);
  }
}

static Int 
c_recorded(int flags)
{
  Term            TermDB, TRef;
  Register DBRef  ref, ref0;
  CELL           *PreviousHeap = H;
  CELL            mask, key;
  Term t1;

  t1 = EXTRA_CBACK_ARG(3,1);
  if (!IsVarTerm(t1)) {
    return(c_log_upd_recorded((DBRef *)RepAppl(t1), flags));
  }
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
    }
    else
      {
	READ_UNLOCK(ref0->Parent->DBRWLock);
	cut_fail();
      }
  }
	
  {
    Term ttmp = EXTRA_CBACK_ARG(3,2);
    if (IsLongIntTerm(ttmp))
      mask = (CELL)LongIntOfTerm(ttmp);
    else
      mask = (CELL)IntOfTerm(ttmp);
  }
  {
    Term ttmp = EXTRA_CBACK_ARG(3,3);
    if (IsLongIntTerm(ttmp))
      key = (CELL)LongIntOfTerm(ttmp);
    else
      key = (CELL)IntOfTerm(ttmp);
  }
  while (ref != NIL
	 && DEAD_REF(ref))
    ref = NextDBRef(ref);
  if (ref == NIL) {
    READ_UNLOCK(ref0->Parent->DBRWLock);
    cut_fail();
  }
  if (mask == 0 && key == 0) {	/* ARG2 is a variable */
    while ((TermDB = GetDBTerm(ref)) == (CELL)0) {
      /* make sure the garbage collector sees what we want it to see! */
      EXTRA_CBACK_ARG(3,1) = (CELL)ref;
      /* oops, we are in trouble, not enough stack space */
      if (!Yap_gc(3, ENV, CP)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      }
      PreviousHeap = H;
    }
    Yap_unify(ARG2, TermDB);
  } else if (mask == 0) {	/* ARG2 is a constant */
    do {
      if (((key == Unsigned(ref->Entry)) || (ref->Flags & DBVar)) &&
	  !DEAD_REF(ref))
	break;
      ref = NextDBRef(ref);
    } while (ref != NIL);
    if (ref == NIL) {
      READ_UNLOCK(ref0->Parent->DBRWLock);
      cut_fail();
    }
  } else
    do {		/* ARG2 is a structure */
      H = PreviousHeap;
      while ((mask & ref->Key) != (key & ref->Mask)) {
	while ((ref = NextDBRef(ref)) != NIL
	       && DEAD_REF(ref));
	if (ref == NIL) {
	  READ_UNLOCK(ref0->Parent->DBRWLock);
	  cut_fail();
	}
      }
      while ((TermDB = GetDBTerm(ref)) == (CELL)0) {
	/* make sure the garbage collector sees what we want it to see! */
	EXTRA_CBACK_ARG(3,1) = (CELL)ref;
	/* oops, we are in trouble, not enough stack space */
	if (!Yap_gc(3, ENV, CP)) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	  return(FALSE);
	}
	PreviousHeap = H;
      }
      if (Yap_unify(ARG2, TermDB))
	break;
      while ((ref = NextDBRef(ref)) != NIL
	     && DEAD_REF(ref));
      if (ref == NIL) {
	READ_UNLOCK(ref0->Parent->DBRWLock);
	cut_fail();
      }
    } while (1);
  READ_UNLOCK(ref0->Parent->DBRWLock);
  TRef = MkDBRefTerm(ref);
  EXTRA_CBACK_ARG(3,1) = (CELL)ref;
#if defined(YAPOR) || defined(THREADS)
  LOCK(ref->lock);
  TRAIL_REF(ref);	/* So that fail will erase it */
  INC_DBREF_COUNT(ref);
  UNLOCK(ref->lock);
#else 
  if (!(ref->Flags & InUseMask)) {
    ref->Flags |= InUseMask;
    TRAIL_REF(ref);	/* So that fail will erase it */
  }
#endif
  return (Yap_unify(ARG3, TRef));
}

/*
 * The arguments for this 4 functions are the flags for terms which should be
 * skipped 
 */

/* recorded(+Functor,+Term,-Ref) */
static Int 
in_rded(void)
{
  DBProp          AtProp;
  register choiceptr b0=B;
  Register Term   twork = Deref(ARG1);	/* initially working with
					 * ARG1 */
  Term t3 = Deref(ARG3);

  if (!IsVarTerm(t3)) {
    if (!IsDBRefTerm(t3)) {
      cut_fail();
    } else {
      DBRef ref = DBRefOfTerm(t3);
      LOCK(ref->lock);
      if (ref == NULL
	  || DEAD_REF(ref)
	  || !Yap_unify(ARG2,GetDBTerm(ref))
	  || !UnifyDBKey(ref,0,ARG1)) {
	UNLOCK(ref->lock);
	cut_fail();
      } else {
	UNLOCK(ref->lock);
	cut_succeed();
      }
    }
  }
  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(twork, 0, FALSE, "recorded/3"))) {
    if (b0 == B)
      cut_fail();
    else
      return(FALSE);
  }
  return (i_recorded(AtProp, t3));
}

/* recorded(+Functor,+Term,-Ref) */
static Int 
in_rded_with_key(void)
{
  DBProp AtProp = (DBProp)IntegerOfTerm(Deref(ARG1));

  return (i_recorded(AtProp,Deref(ARG3)));
}

static Int 
co_rded(void)
{
  return (c_recorded(0));
}

/* '$recordedp'(+Functor,+Term,-Ref) */
static Int 
in_rdedp(void)
{
  DBProp          AtProp;
  register choiceptr b0=B;
  Register Term   twork = Deref(ARG1);	/* initially working with
					 * ARG1 */

  Term t3 = Deref(ARG3);
  if (!IsVarTerm(t3)) {
    if (!IsDBRefTerm(t3)) {
      cut_fail();
    } else {
      DBRef ref = DBRefOfTerm(t3);
      LOCK(ref->lock);
      if (ref == NULL
	  || DEAD_REF(ref)
	  || !Yap_unify(ARG2,GetDBTerm(ref))
	  || !UnifyDBKey(ref,CodeDBBit,ARG1)) {
	UNLOCK(ref->lock);
	cut_fail();
      } else {
	UNLOCK(ref->lock);
	cut_succeed();
      }
    }
  }
  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(twork, MkCode, FALSE, "recorded/3"))) {
    if (b0 == B)
      cut_fail();
    else
      return(FALSE);
  }
  return (i_recorded(AtProp,t3));
}


static Int 
co_rdedp(void)
{
  return (c_recorded(MkCode));
}

/* '$some_recordedp'(Functor)				 */
static Int 
p_somercdedp(void)
{
  Register DBRef  ref;
  DBProp            AtProp;
  Register Term   twork = Deref(ARG1);	/* initially working with
						 * ARG1 */
  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(twork, MkCode, FALSE, "some_recorded/3"))) {
    return(FALSE);
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
static Int 
p_first_instance(void)
{
  Term            TRef;
  Register DBRef  ref;
  DBProp          AtProp;
  Register Term   twork = Deref(ARG1);	/* initially working with
					 * ARG1 */
  Term TermDB;

  ARG3 = Deref(ARG3);
  if (!IsVarTerm(ARG3)) {
    cut_fail();
  }
  if (EndOfPAEntr(AtProp = FetchDBPropFromKey(twork, 0, FALSE, "first_instance/3"))) {
    return(FALSE);
  }
  READ_LOCK(AtProp->DBRWLock);
  ref = AtProp->First;
  while (ref != NIL
	 && (ref->Flags & (DBCode | ErasedMask)))
    ref = NextDBRef(ref);
  READ_UNLOCK(AtProp->DBRWLock);
  if (ref == NIL) {
    cut_fail();
  }
  TRef = MkDBRefTerm(ref);
  /* we have a pointer to the term available */
#if defined(YAPOR) || defined(THREADS)
  LOCK(ref->lock);
  TRAIL_REF(ref);	/* So that fail will erase it */
  INC_DBREF_COUNT(ref);
  UNLOCK(ref->lock);
#else 
  if (!(ref->Flags & InUseMask)) {
    ref->Flags |= InUseMask;
    TRAIL_REF(ref);	/* So that fail will erase it */
  }
#endif
  while ((TermDB = GetDBTerm(ref)) == (CELL)0) {
    /* oops, we are in trouble, not enough stack space */
    if (!Yap_gc(3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
  }
  if (IsVarTerm(TermDB)) {
    Yap_unify(TermDB, ARG2);
  } else {
    return(Yap_unify(ARG2, TermDB));
  }
  return(Yap_unify(ARG3, TRef));
}

static Int
p_key_statistics(void)
{
  Register DBProp p;
  Register DBRef  x;
  UInt sz = 0, cls = 0;
  if (EndOfPAEntr(p = FetchDBPropFromKey(Deref(ARG1), 0, TRUE, "key_statistics/3"))) {
    /* This is not a key property */
    return(FALSE);
  }
  /* count number of clauses and size */
  x = p->First;
  while (x != NULL) {
    cls++;
    sz += Yap_SizeOfBlock((CODEADDR)x);
    x = NextDBRef(x);
  }
  return(Yap_unify(ARG2,MkIntegerTerm(cls)) &&
	 Yap_unify(ARG3,MkIntegerTerm(sz)));
}


/*
 * This is called when we are erasing a data base clause, because we may have
 * pending references 
 */
static void 
ErasePendingRefs(DBRef entryref)
{
  CELL           *cp;
  DBRef           ref;

  if (!(entryref->Flags & DBWithRefs))
    return;
  cp = CellPtr(entryref->DBRefs);
  while ((ref = (DBRef)(*--cp)) != NULL) {
    if ((ref->Flags & DBClMask) && (--(ref->NOfRefsTo) == 0)
	&& (ref->Flags & ErasedMask))
      ErDBE(ref);
  }
}


inline static void 
RemoveDBEntry(DBRef entryref)
{

  ErasePendingRefs(entryref);
  if (entryref->Flags & LogUpdMask) {
    if (entryref->Flags & IndexMask)
      clean_lu_index(entryref);
    else
      FreeDBSpace((char *) entryref);
  } else {
    /* We may be backtracking back to a deleted entry. If we just remove
       the space then the info on the entry may be corrupt.  */
    if ((B->cp_ap == RETRY_C_RECORDED_CODE 
	 || B->cp_ap == RETRY_C_RECORDED_K_CODE 
	 || B->cp_ap == RETRY_C_DRECORDED_CODE 
	 || B->cp_ap == RETRY_C_RECORDEDP_CODE) &&
	EXTRA_CBACK_ARG(3,1) == (CELL)entryref) {
      /* make it clear the entry has been released */
#if defined(YAPOR) || defined(THREADS)
      DEC_DBREF_COUNT(entryref);
#else 
      entryref->Flags &= ~InUseMask;
#endif
      DBErasedMarker->Next = NULL;
      DBErasedMarker->Parent = entryref->Parent;
      DBErasedMarker->n = entryref->n;
      EXTRA_CBACK_ARG(3,1) = (CELL)DBErasedMarker;
    }
    if (entryref->p != NULL)
      entryref->p->n = entryref->n;
    else 
      entryref->Parent->F0 = entryref->n;
    if (entryref->n != NULL)
      entryref->n->p = entryref->p;
    else 
      entryref->Parent->L0 = entryref->p;
    FreeDBSpace((char *) entryref);
  }
}

static void
clean_lu_index(DBRef index) {
  DBRef *te = (DBRef *)(index->Contents);
  DBRef ref;

  LOCK(index->lock);
  if (DBREF_IN_USE(index)) {
    index->Flags |= ErasedMask;
    UNLOCK(index->lock);
    return;
  }
  while ((ref = *te++) != NULL) {
    LOCK(ref->lock);
    /* note that the first element of the conditional generates a
       side-effect, and should never be swapped around with the other */
    if ( --(ref->NOfRefsTo) == 0 && (ref->Flags & ErasedMask)) {
      if (!DBREF_IN_USE(ref)) {
	UNLOCK(ref->lock);
	RemoveDBEntry(ref);
      } else {
	UNLOCK(ref->lock);
      }
    } else {
      UNLOCK(ref->lock);
    }
  }
  UNLOCK(index->lock);
  /* can I get rid of this index? */
  FreeDBSpace((char *)index);
}



static yamop *
find_next_clause(DBRef ref0)
{
  Register DBRef  ref;
  yamop *newp;

  /* fetch ref0 from the instruction we just started executing */
#ifdef DEBUG
  if (!(ref0->Flags & ErasedMask)) {
    Yap_Error(SYSTEM_ERROR, TermNil, "find_next_clause (dead clause %x)", ref0);
    return(NIL);
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
    return(NULL);
  } else {
    /* OK, we found a clause we can jump to, do a bit of hanky pancking with
       the choice-point, so that it believes we are actually working from that
       clause */
    newp = ref->Code;
    /* and next let's tell the world this clause is being used, just
       like if we were executing a standard retry_and_mark */
#if defined(YAPOR) || defined(THREADS)
    {
      Clause *cl = ClauseCodeToClause(newp);

      LOCK(cl->ClLock);
      TRAIL_CLREF(cl);
      INC_DBREF_COUNT(cl);
      UNLOCK(cl->ClLock);
    }
#else 
    if (!DynamicFlags(newp) & InUseMask) {
      DynamicFlags(newp) |= InUseMask;
      TRAIL_CLREF(ClauseCodeToClause(newp));
    }
#endif
    return(newp);
  }
}

/* This procedure is called when a clause is officialy deleted. Its job
   is to find out where the code can go next, if it can go anywhere */
static Int
p_jump_to_next_dynamic_clause(void)
{
  DBRef ref = (DBRef)(((yamop *)((CODEADDR)P-(CELL)NEXTOP((yamop *)NULL,sla)))->u.sla.bmap);
  yamop *newp = find_next_clause(ref);
  
  if (newp == NULL) {
    cut_fail();
  }
  /* the next alternative to try must be obtained from this clause */
  B->cp_ap = newp;
  /* and next, enter the clause */
  P = NEXTOP(newp,ld);
  /* and return like if nothing had happened. */
  return(TRUE);
}

static void
EraseLogUpdCl(Clause *clau)
{
  if (clau->ClFlags & IndexMask) {
    Yap_RemoveLogUpdIndex(clau);
  } else {
    if (clau->ClFlags & LogUpdRuleMask) {
      if (clau->u2.ClExt->u.EC.ClRefs > 0)
	return;
    } else if (clau->u2.ClUse > 0)
      return;
    Yap_FreeCodeSpace((char *)clau);
  }
}

static void
MyEraseClause(Clause *clau)
{
  DBRef           ref;
  SMALLUNSGN      clmask;

  if (CL_IN_USE(clau))
    return;
  clmask = clau->ClFlags;
  if (clmask & LogUpdMask) {
    EraseLogUpdCl(clau);
    return;
  }
  /*
    I don't need to lock the clause at this point because 
    I am the last one using it anyway.
  */
  ref = (DBRef) NEXTOP(clau->ClCode,ld)->u.sla.bmap;
  /* don't do nothing if the reference is still in use */
  if (DBREF_IN_USE(ref))
    return;
  if ( P == clau->ClCode ) {
    yamop *np = RTRYCODE;
    /* make it the next alternative */
    np->u.ld.d = find_next_clause((DBRef)(NEXTOP(P,ld)->u.sla.bmap));
    if (np->u.ld.d == NULL)
      P = (yamop *)FAILCODE;
    else {
      /* with same arity as before */
      np->u.ld.s = P->u.ld.s;
      np->u.ld.p = P->u.ld.p;
      /* go ahead and try this code */
      P = np;
    }
  } else {
    Yap_FreeCodeSpace((char *)clau);
#ifdef DEBUG
    if (ref->NOfRefsTo)
      fprintf(Yap_stderr, "Error: references to dynamic clause\n");
#endif
    RemoveDBEntry(ref);
  }
}

/*
  This predicate is supposed to be called with a
  lock on the current predicate
*/
void 
Yap_ErCl(Clause *clau)
{
  MyEraseClause(clau);
}

#define TRYCODE(G,F,N) ( (N)<5 ? (op_numbers)((int)(F)+(N)*3) : G)

static void 
PrepareToEraseLogUpdClause(Clause *clau, DBRef dbr)
{
  yamop          *code_p = clau->ClCode;
  PredEntry *p = (PredEntry *)(code_p->u.ld.p);
  yamop *cl = code_p;

  WRITE_LOCK(p->PRWLock);
  if (p->cs.p_code.FirstClause != cl) {
    /* we are not the first clause... */
    yamop *prev_code_p = (yamop *)(dbr->Prev->Code);
    prev_code_p->u.ld.d = code_p->u.ld.d; 
    /* are we the last? */
    if (p->cs.p_code.LastClause == cl)
      p->cs.p_code.LastClause = prev_code_p;
  } else {
    /* we are the first clause, what about the last ? */
    if (p->cs.p_code.LastClause == p->cs.p_code.FirstClause) {
      p->cs.p_code.LastClause = p->cs.p_code.FirstClause = NULL;
    } else {
      p->cs.p_code.FirstClause = code_p->u.ld.d;
      p->cs.p_code.FirstClause->opc =
       Yap_opcode(TRYCODE(_try_me, _try_me0, p->ArityOfPE));
    }
  }
  dbr->Code = NULL;   /* unlink the two now */
  if (p->PredFlags & IndexedPredFlag) {
    Yap_RemoveIndexation(p);
  } else {
    if (!(clau->ClFlags & InUseMask))
      EraseLogUpdCl(clau);
  }
  if (p->cs.p_code.FirstClause == p->cs.p_code.LastClause) {
    if (p->cs.p_code.FirstClause != NULL) {
      code_p = p->cs.p_code.FirstClause;
      code_p->u.ld.d = p->cs.p_code.FirstClause;
      p->cs.p_code.TrueCodeOfPred = NEXTOP(code_p, ld);
      if (p->PredFlags & SpiedPredFlag) {
	p->OpcodeOfPred = Yap_opcode(_spy_pred);
	p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
	p->StateOfPred = StaticMask | SpiedMask;
      } else {
	p->CodeOfPred = p->cs.p_code.TrueCodeOfPred;
	p->OpcodeOfPred = p->cs.p_code.TrueCodeOfPred->opc;
	p->StateOfPred = StaticMask;
      }
    } else {
      p->OpcodeOfPred = FAIL_OPCODE;
      p->cs.p_code.TrueCodeOfPred = p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
    }
  } else {
    if (p->PredFlags & SpiedPredFlag) {
      p->OpcodeOfPred = Yap_opcode(_spy_pred);
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
    } else {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
    }
  }
  WRITE_UNLOCK(p->PRWLock);
}

static void 
PrepareToEraseClause(Clause *clau, DBRef dbr)
{
  yamop          *code_p;

  /* no need to erase what has been erased */ 
  if (clau->ClFlags & ErasedMask)
    return;
  clau->ClFlags |= ErasedMask;
  if (clau->ClFlags & LogUpdMask) {
    PrepareToEraseLogUpdClause(clau, dbr);
    return;
  }    
  /* skip mask */
  code_p = clau->ClCode;
  /* skip retry instruction */
  /* we can remove the entry from the list of alternatives for the
     goal immediately */
  {
    DBProp father;
    PredEntry *pred;
    /* first we get the next clause */
    yamop *next = code_p->u.ld.d;
    /* then we get the previous clause */
    yamop *previous =  clau->u.ClPrevious;
    yamop *clau_code;

    /* next we check if we still have clauses left in the chain */
    if (previous != next) {
      yamop  *previous_code = (yamop *)previous;
      Clause *next_cl = ClauseCodeToClause(next);
      /* we do, let's say the previous now backtracks to the next */
      previous_code->u.ld.d = next;
      /* and tell next who it is the previous element */
      next_cl->u.ClPrevious = previous_code;
    }
    /* that's it about setting up the code, now let's tell the
       predicate entry that a clause left. */
    father = dbr->Parent;
    /* inefficient, but that will do for the moment, sir. */
    if (father->ArityOfDB == 0) {
      Atom name = (Atom) father->FunctorOfDB;
      pred = RepPredProp(PredPropByAtom(name, father->ModuleOfDB));
    } else {
      pred = RepPredProp(PredPropByFunc(father->FunctorOfDB, father->ModuleOfDB));
    }
    WRITE_LOCK(pred->PRWLock);
    /* got my pred entry, let's have some fun! */
    clau_code = clau->ClCode;
    if (pred->cs.p_code.FirstClause == pred->cs.p_code.LastClause &&
	pred->cs.p_code.FirstClause != NULL) {
#ifdef DEBUG
      if (pred->cs.p_code.FirstClause != clau_code) {
	/* sanity check */
	if (father->ArityOfDB == 0) {
	  Yap_Error(SYSTEM_ERROR, TermNil, "Prepare to erase clause for %s/%d",RepAtom((Atom)father->FunctorOfDB)->StrOfAE,0);
	} else {
	  Functor f = father->FunctorOfDB;
	  Yap_Error(SYSTEM_ERROR, TermNil, "Prepare to erase clause for %s/%d",RepAtom(NameOfFunctor(f))->StrOfAE,ArityOfFunctor(f));
	}
	return;
      }
#endif
      /* nothing left here, let's clean the shop */
      Yap_FreeCodeSpace(((char *) ClauseCodeToClause(pred->CodeOfPred)));
      pred->cs.p_code.LastClause = pred->cs.p_code.FirstClause = NULL;
      pred->OpcodeOfPred = FAIL_OPCODE;
      pred->cs.p_code.TrueCodeOfPred = pred->CodeOfPred =
	(yamop *)(&(pred->OpcodeOfPred)); 
    } else if (clau_code == pred->cs.p_code.FirstClause) {
      pred->cs.p_code.FirstClause = next;
    } else if (clau_code == pred->cs.p_code.LastClause) {
      pred->cs.p_code.LastClause = previous;
    }
    WRITE_UNLOCK(pred->PRWLock);
  }
  /* make sure we don't directly point to anyone else */
  code_p->u.ld.d = code_p;
  /* now, put some code so that backtracks to here will survive */
  code_p = NEXTOP(code_p, ld);
  /* in this case, a failed clause should go to the data base and find
     out  what is the next clause, if there is one */
  code_p->opc = Yap_opcode(_call_cpred);
  code_p->u.sla.sla_u.p = RepPredProp(Yap_GetPredPropByAtom(Yap_FullLookupAtom("$jump_to_next_dynamic_clause"),0));
  code_p->u.sla.bmap = (CELL *)(dbr);
}

static void 
ErDBE(DBRef entryref)
{

  if ((entryref->Flags & DBCode) && entryref->Code) {
    Clause *clau = ClauseCodeToClause(entryref->Code);
    LOCK(clau->ClLock);
    if (CL_IN_USE(clau) || entryref->NOfRefsTo != 0) {
      PrepareToEraseClause(clau, entryref);
      UNLOCK(clau->ClLock);
    } else {
      if (!(clau->ClFlags & ErasedMask))
	PrepareToEraseClause(clau, entryref);
      UNLOCK(clau->ClLock);
      /* the clause must have left the chain */
      MyEraseClause(clau);
    }
  } else if (!(DBREF_IN_USE(entryref))) {
    if (entryref->NOfRefsTo == 0) 
      RemoveDBEntry(entryref);
    else if (!(entryref->Flags & ErasedMask)) {
      /* oops, I cannot remove it, but I at least have to tell
	 the world what's going on */
      entryref->Flags |= ErasedMask;
      entryref->Next = entryref->Prev = NIL;
    }
  }
}

void 
Yap_ErDBE(DBRef entryref)
{
  ErDBE(entryref);
}

static void
EraseEntry(DBRef entryref)
{
  DBProp          p;

  if (entryref->Flags & ErasedMask)
    return;
  entryref->Flags |= ErasedMask;
  /* update FirstNEr */
  p = entryref->Parent;
  if (p->KindOfPE & LogUpdDBBit) {
    LogUpdDBProp lup = (LogUpdDBProp)p;
    lup->NOfEntries--;
    if (lup->Index != NULL) {
      clean_lu_index(lup->Index);
      lup->Index = NULL;
    }
  }
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
    ErDBE(entryref);
  } else if ((entryref->Flags & DBCode) && entryref->Code) {
    PrepareToEraseClause(ClauseCodeToClause(entryref->Code), entryref);
  }
}

/* erase(+Ref)	 */
static Int 
p_erase(void)
{
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "erase");
    return (FALSE);
  }
  if (!IsDBRefTerm(t1)) {
    Yap_Error(TYPE_ERROR_DBREF, t1, "erase");
    return (FALSE);
  }
  EraseEntry(DBRefOfTerm(t1));
  return (TRUE);
}

/* eraseall(+Key)	 */
static Int 
p_eraseall(void)
{
  Register Term   twork = Deref(ARG1);
  Register DBRef  entryref;
  DBProp          p;

  if (EndOfPAEntr(p = FetchDBPropFromKey(twork, 0, FALSE, "eraseall/3"))) {
    return(TRUE);
  }
  WRITE_LOCK(p->DBRWLock);
  if (p->KindOfPE & LogUpdDBBit) {
    LogUpdDBProp lup = (LogUpdDBProp)p;
    lup->NOfEntries = 0;
    if (lup->Index != NULL) {
      clean_lu_index(lup->Index);
      lup->Index = NULL;
    }
  }
  entryref = FrstDBRef(p);
  do {
    DBRef next_entryref;

    while (entryref != NIL &&
	   (entryref->Flags & (DBCode | ErasedMask)))
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
      ErDBE(entryref);
    else {
      entryref->Flags |= ErasedMask;
    }
    entryref = next_entryref;
  } while (entryref != NIL);
  WRITE_UNLOCK(p->DBRWLock);
  return (TRUE);
}


/* erased(+Ref) */
static Int 
p_erased(void)
{
  Term            t = Deref(ARG1);

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


/* instance(+Ref,?Term) */
static Int 
p_instance(void)
{
  Term            TermDB;
  Term t1 = Deref(ARG1);
  DBRef dbr;

  if (IsVarTerm(t1) || !IsDBRefTerm(t1))
    return (FALSE);
  dbr = DBRefOfTerm(t1);
  if (dbr->Flags & ErasedMask) {
    /* instance/2 of erased terms should fail under log update
       semantics */
    if (dbr->Parent != NULL && dbr->Parent->KindOfPE & LogUpdDBBit)
      return(FALSE);
  }
  while ((TermDB = GetDBTerm(dbr)) == (CELL)0) {
    /* oops, we are in trouble, not enough stack space */
    if (!Yap_gc(2, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
    t1 = Deref(ARG1);
  }
  return (Yap_unify(ARG2, TermDB));
}

inline static int 
NotActiveDB(DBRef my_dbref)
{
  while (my_dbref && (my_dbref->Flags & (DBCode | ErasedMask)))
    my_dbref = my_dbref->Next;
  return (my_dbref == NIL);
}

inline static DBEntry *
NextDBProp(PropEntry *pp)
{
  while (!EndOfPAEntr(pp) && (((pp->KindOfPE & ~ 0x1) != DBProperty) ||
			      NotActiveDB(((DBProp) pp)->First)))
    pp = RepProp(pp->NextOfPE);
  return ((DBEntry *)pp);
}

static Int 
init_current_key(void)
{				/* current_key(+Atom,?key)	 */
  Int             i = 0;
  DBEntry        *pp;
  Atom            a;
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
  EXTRA_CBACK_ARG(2,3) = MkAtomTerm(a);
  EXTRA_CBACK_ARG(2,2) = MkIntTerm(i);
  EXTRA_CBACK_ARG(2,1) = MkIntegerTerm((Int)pp);
  return (cont_current_key());
}

static Int 
cont_current_key(void)
{
  unsigned int    arity;
  Functor         functor;
  Term            term, AtT;
  Atom            a;
  Int             i = IntegerOfTerm(EXTRA_CBACK_ARG(2,2));
  Term            first = Deref(ARG1);
  DBEntry        *pp = (DBEntry *) IntegerOfTerm(EXTRA_CBACK_ARG(2,1));

  if (IsIntTerm(term = EXTRA_CBACK_ARG(2,3)))
    return(cont_current_key_integer());
  a = AtomOfTerm(term);
  if (EndOfPAEntr(pp) && IsAtomTerm(first)) {
    cut_fail();
  }
  while (EndOfPAEntr(pp)) {
    UInt j;

    if ((a = RepAtom(a)->NextOfAE) == NIL) {
      i++;
      while (i < MaxHash) {
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
      if (i == MaxHash) {
	/* we have left the atom hash table */
	/* we don't have a lock over the hash table aany longer */
	if (IsAtomTerm(first)) {
	  cut_fail();
	}
	j = 0;
	if (INT_KEYS == NULL) {
	  cut_fail();
	}
	for(j = 0; j < INT_KEYS_SIZE; j++) {
	  if (INT_KEYS[j] != NIL) {
	    DBProp          pptr = RepDBProp(INT_KEYS[j]);
	    EXTRA_CBACK_ARG(2,1) = MkIntegerTerm((Int)(pptr->NextOfPE));
	    EXTRA_CBACK_ARG(2,2) = MkIntegerTerm(j+1);
	    EXTRA_CBACK_ARG(2,3) = MkIntTerm(INT_KEYS_TIMESTAMP);
	    term = MkIntegerTerm((Int)(pptr->FunctorOfDB));
	    return(Yap_unify(term,ARG1) && Yap_unify(term,ARG2));
	  }
	}
	if (j == INT_KEYS_SIZE) {
	  cut_fail();
	}	  
	return(cont_current_key_integer());
      } else {
	/* release our lock over the hash table */
	READ_UNLOCK(HashChain[i].AERWLock);
	EXTRA_CBACK_ARG(2,2) = MkIntTerm(i);
      }
    }
    READ_LOCK(RepAtom(a)->ARWLock);
    if (!EndOfPAEntr(pp = NextDBProp(RepProp(RepAtom(a)->PropsOfAE))))
      EXTRA_CBACK_ARG(2,3)  = (CELL) MkAtomTerm(a);
    READ_UNLOCK(RepAtom(a)->ARWLock);
  }
  READ_LOCK(RepAtom(a)->ARWLock);
  EXTRA_CBACK_ARG(2,1) = MkIntegerTerm((Int)NextDBProp(RepProp(pp->NextOfPE)));
  READ_UNLOCK(RepAtom(a)->ARWLock);
  arity = (unsigned int)(pp->ArityOfDB);
  if (arity == 0) {
    term = AtT = MkAtomTerm(a);
  } else {
    unsigned int j;
    CELL *p = H;

    for (j = 0; j < arity; j++) {
      p[j] = MkVarTerm();
    }
    functor = Yap_MkFunctor(a, arity);
    term = Yap_MkApplTerm(functor, arity, p);
    AtT = MkAtomTerm(a);
  }
  return (Yap_unify_constant(ARG1, AtT) && Yap_unify(ARG2, term));
}

static Int 
cont_current_key_integer(void)
{
  Term            term;
  UInt             i = IntOfTerm(EXTRA_CBACK_ARG(2,2));
  Prop            pp = (Prop)IntegerOfTerm(EXTRA_CBACK_ARG(2,1));
  UInt            tstamp = (UInt)IntOfTerm(EXTRA_CBACK_ARG(2,3));
  DBProp          pptr;

  if (tstamp != INT_KEYS_TIMESTAMP) {
    cut_fail();
  }
  while (pp == NIL) {
    for(;i < INT_KEYS_SIZE; i++) {
      if (INT_KEYS[i] != NIL) {
	EXTRA_CBACK_ARG(2,2) = MkIntTerm(i+1);
	pp = INT_KEYS[i];
	break;
      }
    }
    if (i == INT_KEYS_SIZE) {
      cut_fail();
    }
  }
  pptr = RepDBProp(pp);
  EXTRA_CBACK_ARG(2,1) = MkIntegerTerm((Int)(pptr->NextOfPE));
  term = MkIntegerTerm((Int)(pptr->FunctorOfDB));
  return(Yap_unify(term,ARG1) && Yap_unify(term,ARG2));
}

static Term 
FetchTermFromDB(DBRef ref, int args)
{
  Term TDB;
  while ((TDB = GetDBTerm(ref)) == (CELL)0) {
    /* oops, we are in trouble, not enough stack space */
    if (!Yap_gc(args, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return(TermNil);
    }
  }
  return(TDB);
}

Term 
Yap_FetchTermFromDB(DBRef ref, int args)
{
  return FetchTermFromDB(ref,args);
}

void 
Yap_ReleaseTermFromDB(DBRef ref)
{
  FreeDBSpace((char *)ref);
}

static DBRef
StoreTermInDB(int arg, int nargs)
{
  DBRef x;
  Term t = Deref(XREGS[arg]);

  while ((x = CreateDBStruct(t, (DBProp)NIL,
			  InQueue)) == NULL) {
    switch(DBErrorFlag) {
    case NO_ERROR_IN_DB:
#ifdef DEBUG
      Yap_Error(SYSTEM_ERROR, TermNil, "no error but null return in enqueue/2");
#endif
      break;
    case SOVF_ERROR_IN_DB:
      if (!Yap_gc(nargs, ENV, P)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      } else {
	t = Deref(XREGS[arg]);
	break;
      }
    case TOVF_ERROR_IN_DB:
      Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow trail in recorda/3");
      return(FALSE);
    case OVF_ERROR_IN_DB:
      if (!Yap_growheap(FALSE)) {
	Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      } else {
	t = Deref(XREGS[arg]);
	break;
      }
    default:
      Yap_Error(DBErrorNumber, DBErrorTerm, DBErrorMsg);
      return(FALSE);
    }
  }
  return(x);
}

DBRef
Yap_StoreTermInDB(int arg, int nargs) {
  return StoreTermInDB(arg, nargs);
}


static Int 
p_init_queue(void)
{
  db_queue *dbq;
  Term t;

  if (DBQueuesCache) {
    dbq = DBQueuesCache;
    DBQueuesCache = dbq->next;
  } else {
    while ((dbq = (db_queue *)AllocDBSpace(sizeof(db_queue))) == NULL) {
      if (!Yap_growheap(FALSE)) {
	Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      }
    }
    dbq->id = FunctorDBRef;
    dbq->Flags = DBClMask;
    dbq->FirstInQueue = dbq->LastInQueue = NULL;
    dbq->prev = NULL;
  }
  dbq->next = DBQueues;
  DBQueues = dbq;
  dbq->age = IntOfTerm(Yap_GetValue(AtomCatch));
  INIT_RWLOCK(dbq->QRWLock);
  t = MkDBRefTerm((DBRef)dbq);
  return(Yap_unify(ARG1, t));
}


static Int 
p_enqueue(void)
{
  Term Father = Deref(ARG1);
  DBRef x;
  db_queue *father_key;

  if (IsVarTerm(Father)) {
    Yap_Error(INSTANTIATION_ERROR, Father, "enqueue");
    return(FALSE);
  } else if (!IsDBRefTerm(Father)) {
    Yap_Error(TYPE_ERROR_DBREF, Father, "enqueue");
    return(FALSE);
  } else
    father_key = (db_queue *)DBRefOfTerm(Father);
  x = StoreTermInDB(2, 2);
  if (x == NULL)
    return(FALSE);
  x->Parent = NULL;
  WRITE_LOCK(father_key->QRWLock);
  if (father_key->LastInQueue != NULL)
    father_key->LastInQueue->Parent = (DBProp)x;
  father_key->LastInQueue = x;
  if (father_key->FirstInQueue == NULL) {
    father_key->FirstInQueue = x;
  }
  WRITE_UNLOCK(father_key->QRWLock);
  return(TRUE);
}

/* when reading an entry in the data base we are making it accessible from
   the outside. If the entry was removed, and this was the last pointer, the
   target entry would be immediately removed, leading to dangling pointers.
   We avoid this problem by making every entry accessible. 

   Note that this could not happen with recorded, because the original db
   entry itself is still accessible from a trail entry, so we could not remove
   the target entry,
 */
static void
keepdbrefs(DBRef entryref)
{
  DBRef           *cp;
  DBRef           ref;

  if (!(entryref->Flags & DBWithRefs))
    return;
  cp = entryref->DBRefs;
  while ((ref = *--cp) != NIL) {
    LOCK(ref->lock);
    if(!(ref->Flags & InUseMask)) {
      ref->Flags |= InUseMask;
      TRAIL_REF(ref);	/* So that fail will erase it */
    }
    UNLOCK(ref->lock);
  }

}

static Int 
p_dequeue(void)
{
  db_queue *father_key;
  DBRef cur_instance;
  Term Father = Deref(ARG1);

  if (IsVarTerm(Father)) {
    Yap_Error(INSTANTIATION_ERROR, Father, "dequeue");
    return(FALSE);
  } else if (!IsDBRefTerm(Father)) {
    Yap_Error(TYPE_ERROR_DBREF, Father, "dequeue");
    return(FALSE);
  } else
    father_key = (db_queue *)DBRefOfTerm(Father);
  WRITE_LOCK(father_key->QRWLock);
  if ((cur_instance = father_key->FirstInQueue) == NULL) {
    /* an empty queue automatically goes away */
    if (father_key == DBQueues)
      DBQueues = father_key->next;
    else 
      father_key->prev->next = father_key->next;
    if (father_key->next != NULL)
      father_key->next->prev = father_key->prev;
    father_key->next = DBQueuesCache;
    DBQueuesCache = father_key;
    WRITE_UNLOCK(father_key->QRWLock);
    return(FALSE);
  } else {
    Term TDB;
    DBRef tref = father_key->FirstInQueue;
    if (cur_instance == father_key->LastInQueue)
      father_key->FirstInQueue = father_key->LastInQueue = NIL;
    else
      father_key->FirstInQueue = (DBRef)(cur_instance->Parent);
    WRITE_UNLOCK(father_key->QRWLock);
    TDB = FetchTermFromDB(tref, 2);
    /* release space for cur_instance */
    keepdbrefs(cur_instance);
    ErasePendingRefs(cur_instance);
    FreeDBSpace((char *) cur_instance);
    return(Yap_unify(ARG2, TDB));
  }
}

static Int
p_clean_queues(void)
{
  Int myage = IntOfTerm(ARG1);
  db_queue *ptr;
  YAPEnterCriticalSection();
  ptr = DBQueues;
  while (ptr) {
    if (ptr->age >= myage) {
      DBRef cur_instance;
      db_queue *optr = ptr;

      while ((cur_instance = ptr->FirstInQueue)) {
	/* release space for cur_instance */
	ptr->FirstInQueue = (DBRef)(cur_instance->Parent);	
	ErasePendingRefs(cur_instance);
	FreeDBSpace((char *) cur_instance);
      }
      ptr = ptr->next;
      FreeDBSpace((char *) optr);
    } else
      break;
  }
  if (ptr)
    ptr->prev = NULL;
  DBQueues = ptr;
  YAPLeaveCriticalSection();
  return(TRUE);
}

/* set the logical updates flag */
static Int
p_slu(void)
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) { 
    Yap_Error(INSTANTIATION_ERROR, t, "switch_logical_updates/1");
    return(FALSE);
  } 
  if (!IsIntTerm(t)) { 
    Yap_Error(TYPE_ERROR_INTEGER, t, "switch_logical_updates/1");
    return(FALSE);
  }
  UPDATE_MODE = IntOfTerm(t);
  return(TRUE);
}

/* check current status for logical updates */
static Int
p_lu(void)
{
  return(Yap_unify(ARG1,MkIntTerm(UPDATE_MODE)));
}

/* get a hold over the index table for logical update predicates */ 
static Int
p_hold_index(void)
{
  LogUpdDBProp AtProp;
  DBRef index;


  if (EndOfPAEntr(AtProp = (LogUpdDBProp)FetchDBPropFromKey(Deref(ARG1), MkCode, FALSE, "recorded/3"))) {
    return(FALSE);
  }
  if ((index = AtProp->Index) == NULL) {
    if (AtProp->NOfEntries < 2) {
      return(Yap_unify(ARG2, TermNil) && Yap_unify(ARG3,MkIntTerm(AtProp->NOfEntries)));
    } else
      index = AtProp->Index = new_lu_index(AtProp);
  }
  /* now, stash the index */ 
#if defined(YAPOR) || defined(THREADS)
  LOCK(index->lock);
  TRAIL_REF(index);	/* So that fail will erase it */
  INC_DBREF_COUNT(index);
  UNLOCK(index->lock);
#else
  if (!(index->Flags & InUseMask)) {
    index->Flags |= InUseMask;
    TRAIL_REF(index);
  }
#endif
  return(Yap_unify(ARG2, MkDBRefTerm(index)) &&
	 Yap_unify(ARG3,MkIntTerm(AtProp->NOfEntries)));
}

static Int
p_fetch_reference_from_index(void)
{
  Term t1 = Deref(ARG1), t2 = Deref(ARG2);
  DBRef table, el;
  Int pos;

  if (IsVarTerm(t1) || !IsDBRefTerm(t1))
    return(FALSE);
  table = DBRefOfTerm(t1);

  if (IsVarTerm(t2) || !IsIntTerm(t2))
    return(FALSE);
  pos = IntOfTerm(t2);
  el = (DBRef)(table->Contents[pos]);
#if defined(YAPOR) || defined(THREADS)
  LOCK(el->lock);
  TRAIL_REF(el);	/* So that fail will erase it */
  INC_DBREF_COUNT(el);
  UNLOCK(el->lock);
#else
  if (!(el->Flags & InUseMask)) {
    el->Flags |= InUseMask;
    TRAIL_REF(el);
  }
#endif
  return(Yap_unify(ARG3, MkDBRefTerm(el)));
}

static Int
p_resize_int_keys(void)
{
  Term t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    return(Yap_unify(ARG1,MkIntegerTerm((Int)INT_KEYS_SIZE)));
  }
  if (!IsIntegerTerm(t1)) {
    Yap_Error(TYPE_ERROR_INTEGER, t1, "yap_flag(resize_db_int_keys,T)");
    return(FALSE);
  }
  return(resize_int_keys(IntegerOfTerm(t1)));
}

void 
Yap_InitDBPreds(void)
{
  Yap_InitCPred("$recorda", 3, p_rcda, SyncPredFlag);
  Yap_InitCPred("$recordz", 3, p_rcdz, SyncPredFlag);
  Yap_InitCPred("recorda_at", 3, p_rcda_at, SyncPredFlag);
  Yap_InitCPred("recordz_at", 3, p_rcdz_at, SyncPredFlag);
  Yap_InitCPred("$recordap", 3, p_rcdap, SyncPredFlag);
  Yap_InitCPred("$recordzp", 3, p_rcdzp, SyncPredFlag);
  Yap_InitCPred("$recordap", 4, p_drcdap, SyncPredFlag);
  Yap_InitCPred("$recordzp", 4, p_drcdzp, SyncPredFlag);
  Yap_InitCPred("$recordaifnot", 3, p_rcdaifnot, SyncPredFlag);
  Yap_InitCPred("$recordzifnot", 3, p_rcdzifnot, SyncPredFlag);
  Yap_InitCPred("erase", 1, p_erase, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("erased", 1, p_erased, TestPredFlag | SafePredFlag|SyncPredFlag);
  Yap_InitCPred("instance", 2, p_instance, SyncPredFlag);
  Yap_InitCPred("eraseall", 1, p_eraseall, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$record_stat_source", 4, p_rcdstatp, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$some_recordedp", 1, p_somercdedp, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$first_instance", 3, p_first_instance, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$init_db_queue", 1, p_init_queue, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$db_key", 2, p_db_key, 0);
  Yap_InitCPred("$db_enqueue", 2, p_enqueue, SyncPredFlag);
  Yap_InitCPred("$db_dequeue", 2, p_dequeue, SyncPredFlag);
  Yap_InitCPred("$db_clean_queues", 1, p_clean_queues, SyncPredFlag);
  Yap_InitCPred("$switch_log_upd", 1, p_slu, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$log_upd", 1, p_lu, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$hold_index", 3, p_hold_index, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$fetch_reference_from_index", 3, p_fetch_reference_from_index, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$resize_int_keys", 1, p_resize_int_keys, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("key_statistics", 3, p_key_statistics, SyncPredFlag);
  Yap_InitCPred("nth_instance", 3, p_nth_instance, SyncPredFlag);
  Yap_InitCPred("$nth_instancep", 3, p_nth_instancep, SyncPredFlag);
  Yap_InitCPred("$jump_to_next_dynamic_clause", 0, p_jump_to_next_dynamic_clause, SyncPredFlag);
}

void 
Yap_InitBackDB(void)
{
  Yap_InitCPredBack("recorded", 3, 3, in_rded, co_rded, SyncPredFlag);
  /* internal version, just to prevent the debugger from nosying around */
  RETRY_C_RECORDED_CODE = NEXTOP((yamop *)
    (RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom("recorded"), 3),0))->cs.p_code.FirstClause),lds);
  Yap_InitCPredBack("$recorded_with_key", 3, 3, in_rded_with_key, co_rded, SyncPredFlag);
  RETRY_C_RECORDED_K_CODE = NEXTOP((yamop *)
    (RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom("$recorded_with_key"), 3),0))->cs.p_code.FirstClause),lds);
  Yap_InitCPredBack("$recorded", 3, 3, in_rded, co_rded, SyncPredFlag);
  RETRY_C_DRECORDED_CODE = NEXTOP((yamop *)
    (RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom("$recorded"), 3),0))->cs.p_code.FirstClause),lds);
  Yap_InitCPredBack("$recordedp", 3, 3, in_rdedp, co_rdedp, SyncPredFlag);
  RETRY_C_RECORDEDP_CODE = NEXTOP((yamop *)
    (RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom("$recordedp"), 3),0))->cs.p_code.FirstClause),lds);
  Yap_InitCPredBack("current_key", 2, 4, init_current_key, cont_current_key,
		SyncPredFlag);
}

