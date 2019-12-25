
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
* File:		exo.c							 *
* comments:	Exo compilation						 *
*									 *
* Last rev:     $Date: 2008-07-22 23:34:44 $,$Author: vsc $		 *				 *
* $Log: not supported by cvs2svn $				 	 *
*                                                                        *
*									 *
*************************************************************************/

#include "Yap.h"
#include "clause.h"
#include "yapio.h"
#include "YapEval.h"
#include "tracer.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_STDBOOL_H
#include <stdbool.h>
#endif

bool YAP_NewExo( PredEntry *ap, size_t data, struct udi_info *udi);
bool YAP_AssertTuples( PredEntry *pe, const Term *ts, size_t offset, size_t m);

//static int exo_write=FALSE;

//void do_write(void) { exo_write=TRUE;}

#define MAX_ARITY 256

#if SIZEOF_INT_P==4
#define FNV32_PRIME (16777619UL)
#define FNV32_OFFSET (0x811c9dc5UL)
#define FNV_PRIME FNV32_PRIME
#define FNV_OFFSET FNV32_OFFSET
#elif SIZEOF_INT_P==8
#define FNV64_PRIME (1099511628211)
#if SIZEOF_LONG_INT==4
#define FNV64_OFFSET (14695981039346656037ULL)
#else
#define FNV64_OFFSET (14695981039346656037UL)
#endif
#define FNV_PRIME FNV64_PRIME
#define FNV_OFFSET FNV64_OFFSET
#endif

/*MurmurHash3 from: https://code.google.com/p/smhasher/wiki/MurmurHash3*/
BITS32 rotl32 ( BITS32, int8_t);

inline BITS32 rotl32 ( BITS32 x, int8_t r )
{
  return (x << r) | (x >> (32 - r));
}
#define ROTL32(x,y)     rotl32(x,y)
//-----------------------------------------------------------------------------
// Finalization mix - force all bits of a hash block to avalanche

BITS32 fmix32 ( BITS32 );
inline BITS32 fmix32 ( BITS32 h )
{
  h ^= h >> 16;
  h *= 0x85ebca6b;
  h ^= h >> 13;
  h *= 0xc2b2ae35;
  h ^= h >> 16;

  return h;
}
//-----------------------------------------------------------------------------
INLINE_ONLY BITS32
HASH_MURMUR3_32 (UInt arity, CELL *cl, UInt bnds[], UInt sz);

INLINE_ONLY BITS32
HASH_MURMUR3_32 (UInt arity, CELL *cl, UInt bnds[], UInt sz)
{
  UInt hash;
  UInt  j=0;
  int len = 0;
  const BITS32 c1 = 0xcc9e2d51;
  const BITS32 c2 = 0x1b873593;

  hash = FNV_OFFSET; /*did not find what seed to use yet*/

  while (j < arity) {
    if (bnds[j]) {
      unsigned char *i=(unsigned char*)(cl+j);
      unsigned char *m=(unsigned char*)(cl+(j+1));

      while (i < m) {
	BITS32 k1 = i[0];

	k1 *= c1;
	k1 = ROTL32(k1,15);
	k1 *= c2;

	hash ^= k1;
	hash = ROTL32(hash,13);
	hash = hash*5+0xe6546b64;
        i++;
	len++;
      }
    }
    j++;
  }

  //----------
  // tail not used becouse len is block multiple

  //----------
  // finalization

  hash ^= len;

  hash = fmix32(hash);

  return hash;
}

/*DJB2*/
#define DJB2_OFFSET 5381

INLINE_ONLY BITS32
HASH_DJB2(UInt arity, CELL *cl, UInt bnds[], UInt sz);

INLINE_ONLY BITS32
HASH_DJB2(UInt arity, CELL *cl, UInt bnds[], UInt sz)
{
  BITS32 hash;
  UInt  j=0;

  hash = DJB2_OFFSET;
  while (j < arity) {
    if (bnds[j]) {
      unsigned char *i=(unsigned char*)(cl+j);
      unsigned char *m=(unsigned char*)(cl+(j+1));

      while (i < m) {
	BITS32 h5 = hash << 5;
	hash += h5 + i[0]; /* hash * 33 + i[0] */
	i++;
      }
    }
    j++;
  }
  return hash;
}

INLINE_ONLY BITS32
HASH_RS(UInt arity, CELL *cl, UInt bnds[], UInt sz);

/* RS Hash Function */
INLINE_ONLY BITS32
HASH_RS(UInt arity, CELL *cl, UInt bnds[], UInt sz)
{
  UInt hash=0;
  UInt j=0;

  UInt b = 378551;
  UInt a = 63689;

  while (j < arity) {
    if (bnds[j]) {
      unsigned char *i=(unsigned char*)(cl+j);
      unsigned char *m=(unsigned char*)(cl+(j+1));

      while (i < m) {
	hash = hash * a + i[0];
	a    = a * b;
	i++;
      }
    }
    j++;
  }
  return hash;
}

INLINE_ONLY BITS32
HASH_FVN_1A(UInt arity, CELL *cl, UInt bnds[], UInt sz);

/* Simple hash function:
   FVN-1A
   first component is the base key.
   hash0 spreads extensions coming from different elements.
   spread over j quadrants.
 */
INLINE_ONLY BITS32
HASH_FVN_1A(UInt arity, CELL *cl, UInt bnds[], UInt sz)
{
  UInt hash;
  UInt  j=0;

  hash = FNV_OFFSET;
  while (j < arity) {
    if (bnds[j]) {
      unsigned char *i=(unsigned char*)(cl+j);
      unsigned char *m=(unsigned char*)(cl+(j+1));

      while (i < m) {
	hash = hash ^ i[0];
	hash = hash * FNV_PRIME;
	i++;
      }
    }
    j++;
  }
  return hash;
}

//#define TEST_HASH_DJB 1

#if defined TEST_HASH_MURMUR
# define HASH(...) HASH_MURMUR3_32(__VA_ARGS__)
#elif defined TEST_HASH_DJB
# define HASH(...)    HASH_DJB2(__VA_ARGS__)
#elif defined TEST_HASH_RS
# define HASH(...) HASH_RS(__VA_ARGS__)
#else
/* Default: TEST_HASH_FVN */
# define HASH(...) HASH_FVN_1A(__VA_ARGS__)
# define HASH1(...) HASH_MURMUR3_32(__VA_ARGS__)
#endif

static BITS32
NEXT(UInt arity, CELL *cl, UInt bnds[], UInt sz, BITS32 hash)
{
  int i = 0;
  BITS32 hash1;

  while (bnds[i]==0) i++;
  hash1 = HASH1(arity, cl, bnds, sz);
  return (hash +  hash1 +cl[i]);
}

/* search for matching elements */
static int
MATCH(CELL *clp, CELL *kvp, UInt arity, UInt bnds[])
{
  UInt j = 0;
  while (j< arity) {
    if ( bnds[j] && clp[j] != kvp[j])
      return FALSE;
    j++;
  }
  return TRUE;
}

static void
ADD_TO_TRY_CHAIN(CELL *kvp, CELL *cl, struct index_t *it)
{
  BITS32 old = EXO_ADDRESS_TO_OFFSET(it, kvp);
  BITS32 new = EXO_ADDRESS_TO_OFFSET(it, cl);
  BITS32 *links = it->links;
  BITS32 tmp = links[old]; /* points to the end of the chain */

  if (!tmp) {
    links[old] = links[new] = new;
  } else {
    links[new] = links[tmp];
    links[tmp] = new;
    links[old] = new;
  }
}

/* This is the critical routine, it builds the hash table *
 * each HT field stores a key pointer which is actually
 * a pointer to the point in the clause where one can find the element.
 *
 * The cls table indexes all elements that can be reached using that key.
 *
 * Insert:
 * j = first
 * not match cij -> insert, open new chain
 * match ci..j ck..j -> find j = minarg(cij \= c2j),
 * else j = +inf -> c2+ci
 * Lookup:
 * j= first
 * not match cij -> fail
 * match ci..j ck..j -> find j = minarg(cij \= c2j)
 * else
 */
static int
INSERT(CELL *cl, struct index_t *it, UInt arity, UInt base, UInt bnds[])
{
  CELL *kvp;
  BITS32 hash;
  int coll_count = 0;


  hash = HASH(arity, cl, bnds, it->hsize);
 next:
  kvp = EXO_OFFSET_TO_ADDRESS(it, it->key [hash % it->hsize]);
  if (kvp == NULL) {
    /* simple case, new entry */
    it->nentries++;
    it->key[hash % it->hsize ] = EXO_ADDRESS_TO_OFFSET(it, cl);
    if (coll_count > it -> max_col_count)
      it->max_col_count = coll_count;
    return TRUE;
  } else if (MATCH(kvp, cl, arity, bnds))  {
    it->ntrys++;
    ADD_TO_TRY_CHAIN(kvp, cl, it);
    return TRUE;
  } else {
    coll_count++;
    it->ncollisions++;
    //  printf("#");
    hash =  NEXT(arity, cl, bnds, it->hsize, hash);
    //if (exo_write) printf("N=%ld\n", hash);
    goto next;
  }
}

static yamop *
LOOKUP(struct index_t *it, UInt arity, UInt j, UInt bnds[])
{
  CACHE_REGS
  CELL *kvp;
  BITS32 hash;

  /* j is the firs bound element */
  /* check if we match */
  hash = HASH(arity, XREGS+1, bnds, it->hsize);
 next:
  /* loop to insert element */
  kvp = EXO_OFFSET_TO_ADDRESS(it, it->key[hash % it->hsize]);
  if (kvp == NULL) {
    /* simple case, no element */
    return FAILCODE;
  } else if (MATCH(kvp, XREGS+1, arity, bnds))  {
    S = kvp;
    if (!it->is_key && it->links[EXO_ADDRESS_TO_OFFSET(it, S)])
      return it->code;
    else
      return NEXTOP(NEXTOP(it->code,lp),lp);
  } else {
    /* collision */
    hash =  NEXT(arity, XREGS+1, bnds, it->hsize, hash);
    goto next;
  }
}

static int
fill_hash(UInt bmap, struct index_t *it, UInt bnds[])
{
  UInt i;
  UInt arity = it->arity;
  CELL *cl = it->cls;

  for (i=0; i < it->nels; i++) {
    if (!INSERT(cl, it, arity, 0, bnds))
      return FALSE;
    cl += arity;
  }
  for (i=0; i < it->hsize; i++) {
    if (it->key[i]) {
      BITS32 offset = it->key[i];
      BITS32 last = it->links[offset];
      if (last) {
      /* the chain used to point straight to the last, and the last back to the original first */
	it->links[offset] = it->links[last];
	it->links[last] = 0;
      }
    }
  }
  return TRUE;
}

static struct index_t *
add_index(struct index_t **ip, UInt bmap, PredEntry *ap, UInt count)
{
  CACHE_REGS
  UInt ncls = ap->NOfClauses, j;
  CELL *base = NULL;
  struct index_t *i;
  size_t sz, dsz;
  yamop *ptr;
  UInt *bnds = LOCAL_ibnds;

  sz =   (CELL)NEXTOP(NEXTOP((yamop*)NULL,lp),lp)+ap->ArityOfPE*(CELL)NEXTOP((yamop *)NULL,x) +(CELL)NEXTOP(NEXTOP((yamop *)NULL,p),l);
  if (!(i = (struct index_t *)Yap_AllocCodeSpace(sizeof(struct index_t)+sz))) {
    CACHE_REGS
    save_machine_regs();
    LOCAL_Error_Size = 3*ncls*sizeof(CELL);
    LOCAL_ErrorMessage = "not enough space to index";
    Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
    return NULL;
  }
  i->is_key = FALSE;
  i->next = *ip;
  i->prev = NULL;
  i->nels = ncls;
  i->arity = ap->ArityOfPE;
  i->ap = ap;
  i->bmap = bmap;
  i->is_key = FALSE;
  i->hsize = 2*ncls;
  dsz = sizeof(BITS32)*(ncls+1+i->hsize);
  if (count) {
    if (!(base = (CELL *)Yap_AllocCodeSpace(dsz))) {
      CACHE_REGS
      save_machine_regs();
      LOCAL_Error_Size = dsz;
      LOCAL_ErrorMessage = "not enough space to generate indices";
      Yap_FreeCodeSpace((void *)i);
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
      return NULL;
    }
    memset(base, 0, dsz);
  }
  i->size = sz+dsz+sizeof(struct index_t);
  i->key = (BITS32 *)base;
  i->links = (BITS32 *)base+i->hsize;
  i->ncollisions = i->nentries = i->ntrys = 0;
  i->cls = (CELL *)((ADDR)ap->FirstClause+2*sizeof(struct index_t *));
  i->bcls= i->cls-i->arity;
  i->udi_free_args = 0;
  i->is_udi = FALSE;
  i->udi_arg = 0;
  *ip = i;
  while (count) {
    if (!fill_hash(bmap, i, bnds)) {
      size_t sz;
      i->hsize += ncls;
      if (i->is_key) {
	sz = i->hsize*sizeof(BITS32);
      } else {
	sz = (ncls+1+i->hsize)*sizeof(BITS32);
      }
      if (base != (CELL *)Yap_ReallocCodeSpace((char *)base, sz))
	return FALSE;
      memset(base, 0, sz);
      i->key = (BITS32 *)base;
      i->links = (BITS32 *)(base+i->hsize);
      i->ncollisions = i->nentries = i->ntrys = 0;
      continue;
    }
#if DEBUG
  fprintf(stderr, "entries=" UInt_FORMAT " collisions=" UInt_FORMAT" (max="  UInt_FORMAT ") trys=" UInt_FORMAT "\n", i->nentries, i->ncollisions,  i->max_col_count, i->ntrys);
#endif
    if (!i->ntrys && !i->is_key) {
      i->is_key = TRUE;
      if (base != (CELL *)Yap_ReallocCodeSpace((char *)base, i->hsize*sizeof(BITS32)))
	return FALSE;
    }
    /* our hash table is just too large */
    if (( i->nentries+i->ncollisions  )*10 < i->hsize) {
      size_t sz;
      i->hsize = ( i->nentries+i->ncollisions  )*10;
      if (i->is_key) {
	sz = i->hsize*sizeof(BITS32);
      } else {
	sz = (ncls+1+i->hsize)*sizeof(BITS32);
      }
      if (base != (CELL *)Yap_ReallocCodeSpace((char *)base, sz))
	return FALSE;
      memset(base, 0, sz);
      i->key = (BITS32 *)base;
      i->links = (BITS32 *)base+i->hsize;
      i->ncollisions = i->nentries = i->ntrys = 0;
    } else {
      break;
    }
  }
  ptr = (yamop *)(i+1);
  i->code = ptr;
  if (count)
    ptr->opc = Yap_opcode(_try_exo);
  else
    ptr->opc = Yap_opcode(_try_all_exo);
  ptr->y_u.lp.l = (yamop *)i;
  ptr->y_u.lp.p = ap;
  ptr = NEXTOP(ptr, lp);
  if (count)
    ptr->opc = Yap_opcode(_retry_exo);
  else
    ptr->opc = Yap_opcode(_retry_all_exo);
  ptr->y_u.lp.p = ap;
  ptr->y_u.lp.l = (yamop *)i;
  ptr = NEXTOP(ptr, lp);
  for (j = 0; j < i->arity; j++) {
    ptr->opc = Yap_opcode(_get_atom_exo);
#if PRECOMPUTE_REGADDRESS
    ptr->y_u.x.x = (CELL) (XREGS + (j+1));
#else
    ptr->y_u.x.x = j+1;
#endif
    ptr = NEXTOP(ptr, x);
  }
  ptr->opc = Yap_opcode(_procceed);
  ptr->y_u.p.p = ap;
  ptr = NEXTOP(ptr, p);
  ptr->opc = Yap_opcode(_Ystop);
  ptr->y_u.l.l = i->code;
  Yap_inform_profiler_of_clause((char *)(i->code), (char *)NEXTOP(ptr,l), ap, GPROF_INDEX);
  if (ap->PredFlags & UDIPredFlag) {
    Yap_new_udi_clause( ap, NULL, (Term)ip);
  } else {
    i->is_udi = FALSE;
  }
  return i;
}

yamop  *
Yap_ExoLookup(PredEntry *ap USES_REGS)
{
  UInt arity = ap->ArityOfPE;
  UInt bmap = 0L, bit = 1, count = 0, j, j0 = 0;
  struct index_t **ip = (struct index_t **)(ap->FirstClause);
  struct index_t *i = *ip;

  for (j=0; j< arity; j++, bit<<=1) {
    Term t = Deref(XREGS[j+1]);
    if (!IsVarTerm(t)) {
      bmap += bit;
      LOCAL_ibnds[j] = TRUE;
      if (!count) j0= j;
      count++;
    } else {
      LOCAL_ibnds[j] = FALSE;
    }
    XREGS[j+1] = t;
  }

  while (i) {
    //    if (i->is_key && (i->bmap & bmap)  == i->bmap) {
    //  break;
    // }
    if (i->bmap == bmap) {
      break;
    }
    ip = &i->next;
    i = i->next;
  }
  if (!i) {
    i = add_index(ip, bmap, ap, count);
  }
  if (count) {
    yamop *code = LOOKUP(i, arity, j0, LOCAL_ibnds);
    if (code == FAILCODE)
      return code;
    if (i->is_udi)
      return ((CEnterExoIndex)i->udi_first)(i PASS_REGS);
    else
      return code;
  } else if(i->is_udi) {
    return ((CEnterExoIndex)i->udi_first)(i PASS_REGS);
  } else {
    return i->code;
  }
}

CELL
Yap_NextExo(choiceptr cptr, struct index_t *it)
{
  CACHE_REGS
  BITS32 offset = ADDRESS_TO_LINK(it,(BITS32 *)((CELL *)(B+1))[it->arity]);
  BITS32 next = it->links[offset];
  ((CELL *)(B+1))[it->arity] = (CELL)LINK_TO_ADDRESS(it, next);
  S = it->cls+it->arity*offset;
  return next;
}

static MegaClause *
exodb_get_space( Term t, Term mod, Term tn )
{
  UInt            arity;
  Prop            pe;
  PredEntry      *ap;
  MegaClause *mcl;
  UInt ncls;
  UInt required;
  struct index_t **li;


  if (IsVarTerm(mod)  || !IsAtomTerm(mod)) {
    return NULL;
  }
  if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    arity = 0;
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    arity = ArityOfFunctor(f);
    pe = PredPropByFunc(f, mod);
  } else {
    return NULL;
  }
  if (EndOfPAEntr(pe)) 
    return NULL;
  ap = RepPredProp(pe);
  if (ap->PredFlags & (DynamicPredFlag|LogUpdatePredFlag
#ifdef TABLING
                       |TabledPredFlag
#endif /* TABLING */
                       )) {
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE,t,"dbload_get_space/4");
    return NULL;
  }
  if (IsVarTerm(tn)  || !IsIntegerTerm(tn)) {
    return NULL;
  }
  ncls = IntegerOfTerm(tn);
  if (ncls <= 1) {
    return NULL;
  }

  required = ncls*arity*sizeof(CELL)+sizeof(MegaClause)+2*sizeof(struct index_t *);
  while (!(mcl = (MegaClause *)Yap_AllocCodeSpace(required))) {
    if (!Yap_growheap(FALSE, required, NULL)) {
      /* just fail, the system will keep on going */
      return NULL;
    }
  }
  Yap_ClauseSpace += required;
  /* cool, it's our turn to do the conversion */
  mcl->ClFlags = MegaMask|ExoMask;
  mcl->ClSize = required;
  mcl->ClPred = ap;
  mcl->ClItemSize = arity*sizeof(CELL);
  mcl->ClNext = NULL;
  li = (struct index_t **)(mcl->ClCode);
  li[0] = li[1] = NULL;
  ap->FirstClause =
    ap->LastClause =
    mcl->ClCode;
  ap->PredFlags |= MegaClausePredFlag;
  ap->NOfClauses = ncls;
  if (ap->PredFlags & (SpiedPredFlag|CountPredFlag|ProfiledPredFlag)) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
  } else {
    ap->OpcodeOfPred = Yap_opcode(_enter_exo);
  }
  ap->CodeOfPred = ap->TrueCodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
  return mcl;
}

bool
YAP_NewExo( PredEntry *ap, size_t data, struct udi_info *udi)
{
  MegaClause *mcl;
  size_t required;
  struct index_t **li;

  if (data <= ap->ArityOfPE*sizeof(CELL)) {
    return false;
  }
  // data = ncls*arity*sizeof(CELL);
  required = data+sizeof(MegaClause)+2*sizeof(struct index_t *);
  while (!(mcl = (MegaClause *)Yap_AllocCodeSpace(required))) {
    if (!Yap_growheap(FALSE, required, NULL)) {
      /* just fail, the system will keep on going */
      return false;
    }
  }
  Yap_ClauseSpace += required;
  /* cool, it's our turn to do the conversion */
  mcl->ClFlags = MegaMask|ExoMask;
  mcl->ClSize = required;
  mcl->ClPred = ap;
  mcl->ClItemSize = ap->ArityOfPE*sizeof(CELL);
  mcl->ClNext = NULL;
  li = (struct index_t **)(mcl->ClCode);
  li[0] = li[1] = NULL;
  ap->FirstClause =
    ap->LastClause =
    mcl->ClCode;
  ap->PredFlags |= MegaClausePredFlag;
  ap->NOfClauses = 0;
  if (ap->PredFlags & (SpiedPredFlag|CountPredFlag|ProfiledPredFlag)) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
  } else {
    ap->OpcodeOfPred = Yap_opcode(_enter_exo);
  }
  ap->CodeOfPred = ap->TrueCodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
  return true;
}

static Int
p_exodb_get_space( USES_REGS1 )
{				/* '$number_of_clauses'(Predicate,M,N) */
  void *mcl;

  if ((mcl = exodb_get_space(Deref(ARG1), Deref(ARG2), Deref(ARG3))) == NULL)
    return FALSE;

  return Yap_unify(ARG4, MkIntegerTerm((Int)mcl));
}

#define DerefAndCheck(t, V)			\
  t = Deref(V); if(IsVarTerm(t) || !(IsAtomOrIntTerm(t))) Yap_Error(TYPE_ERROR_ATOMIC, t0, "load_db");

static Int
store_exo(yamop *pc, UInt arity, Term t0)
{
  Term t;
  CELL *tp = RepAppl(t0)+1,
    *cpc = (CELL *)pc;
  UInt i;
  for (i = 0; i< arity; i++) {
    DerefAndCheck(t, tp[0]);
    *cpc = t;
    //    Yap_DebugPlWrite(t); fprintf(stderr,"\n");
    tp++;
    cpc++;
  }
  //fprintf(stderr,"\n");
  return TRUE;
}

bool
YAP_AssertTuples( PredEntry *pe, const Term *ts, size_t offset, size_t m)
{
  MegaClause *mcl = ClauseCodeToMegaClause(pe->FirstClause);
  size_t           i;
  ADDR   base = (ADDR)mcl->ClCode+2*sizeof(struct index_t *);
  for (i=0; i<m; i++) {
    yamop *ptr = (yamop *)(base+offset*(mcl->ClItemSize));
    store_exo( ptr, pe->ArityOfPE, ts[i]);
  }
  return true;
}

static void
exoassert( void *handle, Int n, Term term )
{                               /* '$number_of_clauses'(Predicate,M,N) */
  PredEntry       *pe;
  MegaClause      *mcl;


  mcl = (MegaClause *) handle;
  pe = mcl->ClPred;
  store_exo((yamop *)((ADDR)mcl->ClCode+2*sizeof(struct index_t *)+n*(mcl->ClItemSize)),pe->ArityOfPE, term);
}

static Int
p_exoassert( USES_REGS1 )
{				/* '$number_of_clauses'(Predicate,M,N) */
  Term            thandle = Deref(ARG2);
  Term            tn = Deref(ARG3);
  MegaClause      *mcl;
  Int              n;


  if (IsVarTerm(thandle)  || !IsIntegerTerm(thandle)) {
    return FALSE;
  }
  mcl = (MegaClause *)IntegerOfTerm(thandle);
  if (IsVarTerm(tn)  || !IsIntegerTerm(tn)) {
    return FALSE;
  }
  n = IntegerOfTerm(tn);
  exoassert(mcl,n,Deref(ARG1));
  return TRUE;
}

void
Yap_InitExoPreds(void)
{
  CACHE_REGS
  Term cm = CurrentModule;

  CurrentModule = DBLOAD_MODULE;
  Yap_InitCPred("exo_db_get_space", 4, p_exodb_get_space, 0L);
  Yap_InitCPred("exoassert", 3, p_exoassert, 0L);
  CurrentModule = cm;
}
