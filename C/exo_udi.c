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
#include "eval.h"
#include "tracer.h"
#include "attvar.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#if HAVE_STRING_H
#include <string.h>
#endif
#define YAP_Term Term
#define YAP_Atom Atom
#include <udi.h>


static int
compar(const void *ip0, const void *jp0) {
  CACHE_REGS
  BITS32 *ip = (BITS32 *)ip0, *jp = (BITS32 *)jp0;
  Term i = EXO_OFFSET_TO_ADDRESS(LOCAL_exo_it, *ip)[LOCAL_exo_arg];
  Term j = EXO_OFFSET_TO_ADDRESS(LOCAL_exo_it, *jp)[LOCAL_exo_arg];
  //fprintf(stderr, "%ld-%ld\n", IntOfTerm(i), IntOfTerm(j)); 
  return IntOfTerm(i)-IntOfTerm(j);
}

static int
compare(const BITS32 *ip, Int j USES_REGS) {
  Term i = EXO_OFFSET_TO_ADDRESS(LOCAL_exo_it, *ip)[LOCAL_exo_arg];
  //fprintf(stderr, "%ld-%ld\n", IntOfTerm(i), j); 
  return IntOfTerm(i)-j;
 }

static int
same_free(BITS32 i, BITS32 j, struct index_t *it) {
  CELL *ip = EXO_OFFSET_TO_ADDRESS(it, i);
  CELL *jp = EXO_OFFSET_TO_ADDRESS(it, j);
  UInt m = it->udi_free_args, m0 = 1, x;
  for (x=0; x< it->arity; x++) {
    if (m0 & m) {
      if (ip[x] != jp[x])
	return FALSE;
      m -= m0;
      if (!m)
	return TRUE;
    }
    m0 <<= 1;
  }
  return TRUE;
 }

static UInt free_args(UInt b[], UInt arity, UInt i) {
  UInt j;
  UInt rc = 0;

  for (j=0; j<arity; j++) {
    if (i !=j && b[j] == 0)
      rc |= 1<<j;
  }
  return rc;
}

static void
chain(BITS32 *p0, BITS32 *el, BITS32 n, struct index_t *it) {
  UInt i;

  for (i=0; i<n; i++) {
    UInt j, k = i;
    if (p0[i])
      continue;
    p0[i] = i;
    for (j=i+1; j<n; j++) {
      if (same_free(el[i], el[j], it)) {
	p0[j] = k;
	k = j;
      }
    }
  }
}

static Int
NEXT_DIFFERENT(Int x0, Int x, BITS32 *p, Int xe)
{
  while (x <= xe) {
    x++;
    if (p[x] < x0 || p[x] >= x) 
      return x;
  }
  return x;
}

static Int
BIGGEST_EL(Int x0, BITS32 *p, Int xe)
{
  Int x = x0; 

  while (x <= xe) {
    if (p[x] == x0) 
      x0 = x;
    x++;
  }
  return x0;
}

static void
IntervalUDIRefitIndex(struct index_t **ip, UInt b[] USES_REGS)
{
  size_t sz;
  struct index_t *it = *ip;
  yamop *code;

  /* hard-wired implementation for the Interval case */
  Int i = it->udi_arg;
  /* it is bound, use hash */
  if (it->bmap & b[i]) return;
  /* no constraints, nothing to gain */
  if (!IsAttVar(VarOfTerm(XREGS[i+1]))) return;
  LOCAL_exo_it = it;
  LOCAL_exo_base = it->bcls;
  LOCAL_exo_arity = it->arity;
  LOCAL_exo_arg = i;
  it->udi_free_args = free_args(b, it->arity, i);
  if (!it->key) {
    UInt ncls = it->ap->cs.p_code.NOfClauses, i;
    BITS32 *sorted;
    /* handle ll variables */
    sz = sizeof(BITS32)*(ncls);
    /* allocate space */
    if (!(it->udi_data = (BITS32*)Yap_AllocCodeSpace(sz)))
      return;
    sorted = (BITS32*)it->udi_data;
    for (i=0; i< ncls; i++)
      sorted[i] = i;
    qsort(sorted, (size_t)ncls, sizeof(BITS32), compar); 
    it->links = NULL;
  } else {
    BITS32 *sorted0, *sorted;
    
    /* be conservative */
    if (it->udi_free_args)
      sz = sizeof(BITS32)*(3*it->ntrys+2*it->nentries);
    else
      sz = sizeof(BITS32)*(2*it->ntrys+it->nentries);
    /* allocate space */
    if (!(it->udi_data = (BITS32*)Yap_AllocCodeSpace(sz)))
      return;
    sorted0 = sorted = (BITS32 *)it->udi_data;
    sorted++; /* leave an initial hole */
    for (i=0; i < it->hsize; i++) {
      if (it->key[i]) {
	BITS32 *s0 = sorted;
	BITS32 offset = it->key[i], offset0 = offset;
	
	*sorted++ = 0;
	do {
	  *sorted++ = offset;
	  offset = it->links[offset];
	} while (offset);
	// S = EXO_OFFSET_TO_ADDRESS(it, offset0); Yap_DebugPlWrite(S[0]);
	// fprintf(stderr, " key[i]=%d offset=%d  %d\n", it->key[i], offset0, (sorted-s0)-1);
	if (sorted-s0 == 2) {
	  it->links[offset0] = 0;
	  sorted = s0;
	} else {
	  /* number of elements comes first */
	  *s0 = sorted - (s0+1);
	  qsort(s0+1, (size_t)*s0, sizeof(BITS32), compar); 
	  it->links[offset0] = s0-sorted0;
	  // fprintf(stderr," %d links %d=%d \n", offset0, s0-sorted0, s0[0]);
	  if (it->udi_free_args) {
	    bzero(sorted, sizeof(BITS32)*(*s0));
	    /* chain elements with same unbound vars together */
	    chain(sorted, s0+1, *s0, it);
	    sorted += *s0;
	  }
	}
      }
    }
    sz = sizeof(BITS32)*(sorted-sorted0);
    it->udi_data = (BITS32 *)Yap_ReallocCodeSpace((char *)it->udi_data, sz);
  }
  it->is_udi = i+1;
  code = it->code;
  code->opc = Yap_opcode(_try_exo_udi);
  code = NEXTOP(code, lp);
  code->opc = Yap_opcode(_retry_exo_udi);
}

 static BITS32 *
 binary_search(BITS32 *start, BITS32 *end, Int x USES_REGS)
 {
   BITS32 *mid;
   while (start < end) {
     int cmp;
     mid = start + (end-start)/2;
     cmp = compare(mid, x PASS_REGS);
     if (!cmp)
       return mid;
     if (cmp > 0) {
       end = mid-1;
     } else
       start = mid+1;
   }
   return start;
 }

 static yamop *
Interval(struct index_t *it, Term min, Term max, Term op, BITS32 off USES_REGS)
 {
   BITS32 *c;
   BITS32 n;
   BITS32 *pt;
   BITS32 *end;
   BITS32 *pt0, *end0; 
   Atom at;

   LOCAL_exo_it = it;
   LOCAL_exo_base = it->bcls;
   LOCAL_exo_arity = it->arity;
   LOCAL_exo_arg = it->udi_arg;
   if (!it->links) {
     c = (BITS32 *)it->udi_data;
     n = it->nels;
     pt  = c;
     end  = c+(n-1);
     pt0 = pt;
     end0 = end+1;
   } else if (it->links[off]) {
     c = (BITS32 *)it->udi_data;
     n = c[it->links[off]];
     pt0 = pt  = c+(it->links[off]+1);
     end  = c+(it->links[off]+n);
     end0 = end+1;
     // fprintf(stderr," %d links %d=%d \n", off, it->links[off], n);
   } else {
     if (!IsVarTerm(min)) {
       Int x;
       if (!IsIntegerTerm(min)) {
	 min = Yap_Eval(min);
	 if (!IsIntegerTerm(min)) {
	   Yap_Error(TYPE_ERROR_INTEGER, min, "data-base constraint");
	   return FAILCODE;
	 }
       }
       x = IntegerOfTerm(min);
       if (x >= IntegerOfTerm(S[LOCAL_exo_arg])) {
	 return FAILCODE;
       }     
     }
     if (!IsVarTerm(max)) {
       Int x;
       if (!IsIntegerTerm(max)) {
	 max = Yap_Eval(max);
	 if (!IsIntegerTerm(max)) {
	   Yap_Error(TYPE_ERROR_INTEGER, max, "data-base constraint");
	   return FAILCODE;
	 }
       }
       x = IntegerOfTerm(max);
       if (x <= IntegerOfTerm(S[LOCAL_exo_arg])) {
	 return FAILCODE;
       }     
     }
     return NEXTOP(NEXTOP(it->code,lp),lp);
   }

   if (!IsVarTerm(min)) {
     Int x;
     if (!IsIntegerTerm(min)) {
      min = Yap_Eval(min);
      if (!IsIntegerTerm(min)) {
	Yap_Error(TYPE_ERROR_INTEGER, min, "data-base constraint");
	return FAILCODE;
      }
     }
     x = IntegerOfTerm(min);
     if (n > 8) {
       int cmp;
       pt = binary_search(pt, end, x PASS_REGS);
       while ( pt < end+1 && (cmp = compare(pt, x PASS_REGS)) <= 0 ) {
	 if (cmp > 0) break;
	 pt++;
       }
     } else {
       while ( pt < end+1 && compare(pt, x PASS_REGS) <= 0 ) {
	 pt++;
       }
     }
     if (pt > end) 
       return FAILCODE;
   }
   if (!IsVarTerm(max)) {
     Int x;
     BITS32 *pt1;
     Int n = end-pt;

     if (!IsIntegerTerm(max)) {
      max = Yap_Eval(max);
      if (!IsIntegerTerm(max)) {
	Yap_Error(TYPE_ERROR_INTEGER, max, "data-base constraint");
	return FAILCODE;
      }
     }
     x = IntegerOfTerm(max);
     if (n > 8) {
       int cmp;
       pt1 = binary_search(pt, end, x PASS_REGS);
       while ( pt1 >= pt && (cmp = compare(pt1, x PASS_REGS)) >= 0 ) {
	 if (cmp < 0) break;
	 pt1--;
       }
     } else {
       pt1 = end;
       while ( pt1 >= pt && compare(pt1, x PASS_REGS) >= 0 ) {
	 pt1--;
       }
     }
     if (pt1 < pt) 
       return FAILCODE;
     end = pt1;
   }
   if (IsVarTerm(op)) {
     S = EXO_OFFSET_TO_ADDRESS(it, pt[0]);
     if (pt < end ) {
       YENV[-1] = (CELL)( end );
       YENV[-2] = (CELL)( pt+1 );
       YENV -= 2;
       return it->code;
     }
     return NEXTOP(NEXTOP(it->code,lp),lp);
   }
   at = AtomOfTerm(op);
   if (at == AtomAny || at == AtomMinimum) {
     S = EXO_OFFSET_TO_ADDRESS(it, pt[0]);
   } else if (at == AtomMaximum) {
     S = EXO_OFFSET_TO_ADDRESS(it, end[0]);
   } else if (at == AtomUnique) {
     if (end-2 > pt)
       return FAILCODE;
     S = EXO_OFFSET_TO_ADDRESS(it, pt[0]);
   } else if (at == AtomMin) {
     Int x0, xe, x;

     x0 = pt-pt0;
     xe = end-pt0;
     S = EXO_OFFSET_TO_ADDRESS(it, pt[0]);
     x = NEXT_DIFFERENT(x0, x0, end0, xe);
     if (x < xe ) {
       YENV[-5] = (CELL)( pt0 );  // base for array of pointed pointers
       YENV[-4] = MkIntegerTerm( x );    // where we are in pt0 array
       YENV[-3] = MkIntegerTerm( xe );   // our visit will end here 
       YENV[-2] = MkIntegerTerm( x0 );   // our visit started here
       YENV[-1] = (CELL)( end0 ); // base for array into pt
       YENV -= 5;
       return it->code;
     }
     return NEXTOP(NEXTOP(it->code,lp),lp);
   } else if (at == AtomMax) {
     Int x0, xe, x, y;

     x0 = pt-pt0;
     xe = end-pt0;
     y = BIGGEST_EL( x0, end0, xe );
     S = EXO_OFFSET_TO_ADDRESS(it, pt[0]);
     x = NEXT_DIFFERENT(x0, x0, end0, xe);
     if (x < xe ) {
       YENV[-5] = (CELL)( pt0 );  // base for array of pointed pointers
       YENV[-4] = MkIntegerTerm( -x );    // where we are in pt0 array
       YENV[-3] = MkIntegerTerm( xe );   // our visit will end here 
       YENV[-2] = MkIntegerTerm( x0 );   // our visit started here
       YENV[-1] = (CELL)( end0 ); // base for array into pt
       YENV -= 5;
       return it->code;
     }
     return NEXTOP(NEXTOP(it->code,lp),lp);
   }
   return NEXTOP(NEXTOP(it->code,lp),lp);
 }

static yamop *
IntervalEnterUDIIndex(struct index_t *it USES_REGS)
{
  Int i = it->udi_arg;
  Term  t = XREGS[i+1], a1;
  BITS32 off = EXO_ADDRESS_TO_OFFSET(it, S);
  //  printf("off=%d it=%p %p---%p\n", off, it, it->cls, S);
  attvar_record *attv;

  t = Deref(t);
  if (!IsVarTerm(t))
    return FALSE;
  if(!IsAttVar(VarOfTerm(t)))
    return Interval(it, MkVarTerm(), MkVarTerm(), MkVarTerm(), off PASS_REGS);
  attv = RepAttVar(VarOfTerm(t));
  t =  attv->Atts;
  a1 = ArgOfTerm(2,t);
  if (IsVarTerm(a1)) {
    Yap_Error(INSTANTIATION_ERROR, t, "executing exo_interval constraints");
    return FAILCODE;
  } else if (!IsApplTerm(a1)) {
    Yap_Error(TYPE_ERROR_COMPOUND, a1, "executing exo_interval constraints");
    return FAILCODE;
  } else {
    return Interval(it, ArgOfTerm(1,a1), ArgOfTerm(2,a1), ArgOfTerm(3,a1), off PASS_REGS);
  }
}

static int
IntervalRetryUDIIndex(struct index_t *it USES_REGS)
{
  CELL *w = (CELL*)(B+1)+it->arity;
  if (IsVarTerm(w[2])) {
    BITS32 *end = (BITS32 *) w[2],
      *pt = (BITS32 *) w[1];
    BITS32 f = *pt;

    S = EXO_OFFSET_TO_ADDRESS(it, f);
    if (pt++ == end) return FALSE;
    w[1] = (CELL)pt;
  } else {
    BITS32 *pt0 = (BITS32 *)w[1];
    Int x = IntegerOfTerm( w[2] );
    Int xe = IntegerOfTerm( w[3] );
    Int x0 = IntegerOfTerm( w[4] );
    BITS32 *base = (BITS32 *)w[5];
    if ( x > 0) {
      //Yap_DebugPlWrite( EXO_OFFSET_TO_ADDRESS(it, el[i])[1] ); fprintf(stderr,"\n");
      S = EXO_OFFSET_TO_ADDRESS(it, pt0[x]);
      //fprintf(stderr,"S=%p x=%d/%d %d %d %p %p \n", S, x, base[x], x0, xe, pt0, base);
      x = NEXT_DIFFERENT(x0, x, base, xe);
      if (x > xe) return FALSE;
      w[2] = MkIntegerTerm(x);
    } else {
      x = -x;
      //Yap_DebugPlWrite( EXO_OFFSET_TO_ADDRESS(it, el[i])[1] ); fprintf(stderr,"\n");
      S = EXO_OFFSET_TO_ADDRESS(it, pt0[BIGGEST_EL(x, base, xe) ]);
      x = NEXT_DIFFERENT(x0, x, base, xe);
      // fprintf(stderr,"S=%p x=%d/%d %d %d %p %p \n", S, x, base[x], x0, xe, pt0, base);
      if (x > xe) {
	return FALSE;
      }
      w[2] = MkIntegerTerm(-x);
    }
  }
  return TRUE;
}


static struct udi_control_block IntervalCB;

typedef struct exo_udi_access_t {
  CRefitExoIndex refit;
} exo_udi_encaps_t;

static struct exo_udi_access_t ExoCB;

static void *
IntervalUdiInit (Term spec, int arg, int arity) {
  ExoCB.refit = IntervalUDIRefitIndex;
  return (void *)&ExoCB;
}

static void *
IntervalUdiInsert (void *control,
                Term term, int arg, void *data)
{
  CACHE_REGS

  struct index_t **ip = (struct index_t **)term;
  (*ip)->udi_arg = arg-1;
  (ExoCB.refit)(ip, LOCAL_ibnds PASS_REGS);
  (*ip)->udi_first = (void *)IntervalEnterUDIIndex;
  (*ip)->udi_next = (void *)IntervalRetryUDIIndex;
  return control;
}

static int IntervalUdiDestroy(void *control)
{
  return TRUE;
}



void Yap_udi_Interval_init(void) {
  UdiControlBlock cb = &IntervalCB;
  Atom name = Yap_LookupAtom("exo_interval");
  memset((void *) cb,0, sizeof(*cb));

  /*TODO: ask vitor why this gives a warning*/
  cb->decl= name;
  Yap_MkEmptyWakeUp(name);
  cb->init= IntervalUdiInit;
  cb->insert=IntervalUdiInsert;
  cb->search=NULL;
  cb->destroy=IntervalUdiDestroy;

  Yap_UdiRegister(cb);
}
