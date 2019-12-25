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

static Int
cmp_extra_args(CELL *si, CELL *sj, struct index_t *it)
{
  UInt m = it->udi_free_args;
  UInt m0 = 1, x;
  
  for (x=0; x< it->arity; x++) {
    if (m0 & m) {
      if (si[x] != sj[x]) {
	if (IsIntTerm(si[x]))
	  return IntOfTerm(si[x])-IntOfTerm(sj[x]);
	return AtomOfTerm(si[x])-AtomOfTerm(sj[x]);
      }
      m -= m0;
      if (m == 0)
	return 0;
    }
    m0 <<= 1;
  }
  return 0;
}

static int
compar2(const void *ip0, const void *jp0) {
  CACHE_REGS
  BITS32 *ip = (BITS32 *)ip0, *jp = (BITS32 *)jp0;
  struct index_t *it = LOCAL_exo_it;
  Term* si = EXO_OFFSET_TO_ADDRESS(it, *ip);
  Term* sj = EXO_OFFSET_TO_ADDRESS(it, *jp);
  int cmp = cmp_extra_args(si, sj, it);
  if (cmp)
    return cmp;
  return IntOfTerm(si[LOCAL_exo_arg])-IntOfTerm(sj[LOCAL_exo_arg]);
}

static int
compare(const BITS32 *ip, Int j USES_REGS) {
  Term i = EXO_OFFSET_TO_ADDRESS(LOCAL_exo_it, *ip)[LOCAL_exo_arg];
  //fprintf(stderr, "%ld-%ld\n", IntOfTerm(i), j); 
  return IntOfTerm(i)-j;
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

static BITS32*
NEXT_DIFFERENT(BITS32 *pt0, BITS32 *pte, struct index_t *it)
{
  Term* si = EXO_OFFSET_TO_ADDRESS(it, pt0[0]);
  Term* sj;

  do {
    pt0++;
    if (pt0 == pte)
      return NULL;
    sj = EXO_OFFSET_TO_ADDRESS(it, *pt0);
  } while (!cmp_extra_args(si, sj, it));
  return pt0;
}

static BITS32*
PREV_DIFFERENT(BITS32 *pt0, BITS32 *pte, struct index_t *it)
{
  Term* si = EXO_OFFSET_TO_ADDRESS(it, pt0[0]);
  Term* sj;

  do {
    pt0--;
    if (pt0 == pte)
      return NULL;
    sj = EXO_OFFSET_TO_ADDRESS(it, *pt0);
  } while (!cmp_extra_args(si, sj, it));
  return pt0;
}

static BITS32*
NEXT_MIN(BITS32 *pt0, BITS32 *pte, Term tmin, Term tmax, struct index_t *it)
{
  Term* si = EXO_OFFSET_TO_ADDRESS(it, pt0[0]);
  int do_min, do_max;
  Int min = 0, max = 0;

  if (IsVarTerm(tmin)) {
    do_min = FALSE;
  } else {
    do_min = TRUE;
    min = IntOfTerm(tmin);
  }
  if (IsVarTerm(tmax)) {
    do_max = FALSE;
  } else {
    do_max = TRUE;
    max = IntOfTerm(tmax);
  }

  while ((do_min && IntOfTerm(si[it->udi_arg]) < min) ||
	 (do_max && IntOfTerm(si[it->udi_arg]) > max)) {
    pt0++;
    if (pt0 == pte)
      return NULL;
    si = EXO_OFFSET_TO_ADDRESS(it, *pt0);
  }
  return pt0;
}

static BITS32*
NEXT_MAX(BITS32 *pt0, BITS32 *pte, Term tmin, Term tmax, struct index_t *it)
{
  Term* si = EXO_OFFSET_TO_ADDRESS(it, pt0[0]);
  int do_min, do_max;
  Int min = 0, max = 0;

  if (IsVarTerm(tmin)) {
    do_min = FALSE;
  } else {
    do_min = TRUE;
    min = IntOfTerm(tmin);
  }
  if (IsVarTerm(tmax)) {
    do_max = FALSE;
  } else {
    do_max = TRUE;
    max = IntOfTerm(tmax);
  }

  while ((do_min && IntOfTerm(si[it->udi_arg]) < min) ||
	 (do_max && IntOfTerm(si[it->udi_arg]) > max)) {
    pt0--;
    if (pt0 == pte)
      return NULL;
    si = EXO_OFFSET_TO_ADDRESS(it, *pt0);
  }
  return pt0;
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
  //if (!IsAttVar(VarOfTerm(Deref(XREGS[i+1])))) return;
  LOCAL_exo_it = it;
  LOCAL_exo_base = it->bcls;
  LOCAL_exo_arity = it->arity;
  LOCAL_exo_arg = i;
  it->udi_free_args = free_args(b, it->arity, i);
  if (!it->key) {
    UInt ncls = it->ap->NOfClauses, i;
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
      sz = sizeof(BITS32)*(2*it->ntrys+3*it->nentries);
    else
      sz = sizeof(BITS32)*(it->ntrys+2*it->nentries);
    /* allocate space */
    if (!(it->udi_data = (BITS32*)malloc(sz)))
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
	  if (it->udi_free_args) {
	    memmove(sorted, s0+1, sizeof(BITS32)*(*s0));
	    qsort(sorted, (size_t)*s0, sizeof(BITS32), compar2); 
	    sorted += *s0;
	  }
	}
      }
    }
    sz = sizeof(BITS32)*(sorted-sorted0);
    it->udi_data = (BITS32 *)realloc((char *)it->udi_data, sz);
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
   } else if (it->links[off]) {
     c = (BITS32 *)it->udi_data;
     n = c[it->links[off]];
     pt = c;
     end  = c+(it->links[off]+n);
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
     S = EXO_OFFSET_TO_ADDRESS(it, pt[0]);
     if (it->udi_free_args) {
       BITS32 *ptn;
       pt  = c+(it->links[off]+n+1);
       end = pt+n;
       pt = NEXT_MIN(pt, end, min, max, it);
       if (!pt)
	 return FAILCODE;
       S = EXO_OFFSET_TO_ADDRESS(it, pt[0]);
       ptn = NEXT_DIFFERENT(pt, end, it);
       if (ptn) 
	 ptn = NEXT_MIN(ptn, end, min, max, it);
       if ( ptn ) {
	 YENV[-1] = min;    // what we are doing
	 YENV[-2] = max;    // what we are doing
	 YENV[-3] = (CELL) end;    // what we are doing
	 YENV[-4] = MkAtomTerm(AtomMin);    // what we are doing
	 YENV[-5] = (CELL)( ptn );    // where we are in pt0 array
	 YENV -= 5;
	 return it->code;
       }
     }
     return NEXTOP(NEXTOP(it->code,lp),lp);
   } else if (at == AtomMax) {
     S = EXO_OFFSET_TO_ADDRESS(it, pt[0]);
     if (it->udi_free_args) {
       BITS32 *ptn;
       end  = c+(it->links[off]+n);
       pt = end+n;
       pt = NEXT_MAX(pt, end, min, max, it);
       if (!pt)
	 return FAILCODE;
       S = EXO_OFFSET_TO_ADDRESS(it, pt[0]);
       ptn = PREV_DIFFERENT(pt, end, it);
       if (ptn) 
	 ptn = NEXT_MAX(ptn, end, min, max, it);
       if ( ptn ) {
	 YENV[-1] = min;    // what we are doing
	 YENV[-2] = max;    // what we are doing
	 YENV[-3] = (CELL) end;    // what we are doing
	 YENV[-4] = MkAtomTerm(AtomMax);    // what we are doing
	 YENV[-5] = (CELL)( ptn );    // where we are in pt0 array
	 YENV -= 5;
	 return it->code;
       }
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
    BITS32 *pte = (BITS32 *)w[3];
    Atom what = AtomOfTerm(w[2]);
    Term min = w[5];
    Term max = w[4];

    S = EXO_OFFSET_TO_ADDRESS(it, pt0[0]);
    if ( what == AtomMin ) {
      pt0 = NEXT_DIFFERENT(pt0, pte, it);
      if (pt0) 
	pt0 = NEXT_MIN(pt0, pte, min, max, it); 
    } else {
      pt0 = PREV_DIFFERENT(pt0, pte, it);
      if (pt0) 
	pt0 = NEXT_MAX(pt0, pte, min, max, it); 
    }
    if (!pt0) {
      return FALSE;
    }
    w[1] = (CELL)pt0;
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
  cb->decl= (YAP_Atom)name;
  Yap_MkEmptyWakeUp(name);
  cb->init= IntervalUdiInit;
  cb->insert=IntervalUdiInsert;
  cb->search=NULL;
  cb->destroy=IntervalUdiDestroy;

  Yap_UdiRegister(cb);
}
