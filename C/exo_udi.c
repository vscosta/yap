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
  BITS32 *bs = LOCAL_exo_base;
  Int i = bs[LOCAL_exo_arity*(*ip)+LOCAL_exo_arg];
  Int j = bs[LOCAL_exo_arity*(*jp)+LOCAL_exo_arg];
  return IntOfTerm(i)-IntOfTerm(j);
}

static int
compare(const BITS32 *ip, Int j USES_REGS) {
  BITS32 *bs = LOCAL_exo_base;
  Int i = bs[LOCAL_exo_arity*(*ip)+LOCAL_exo_arg];
  //fprintf(stderr, "%ld-%ld\n", IntOfTerm(i), j); 
  return IntOfTerm(i)-j;
 }


 static void
 IntervalUDIRefitIndex(struct index_t **ip, UInt b[] USES_REGS)
 {
   size_t sz;
   struct index_t *it = *ip;
   UInt arity = it->arity;
   yamop *code;

   /* hard-wired implementation for the Interval case */
   Int i = it->udi_arg;
   /* it is bound, use hash */
   if (it->bmap & b[i]) return;
   /* no constraints, nothing to gain */
   if (!IsAttVar(VarOfTerm(XREGS[i+1]))) return;
   LOCAL_exo_base = it->cls;
   LOCAL_exo_arity = it->arity;
   LOCAL_exo_arg = i;
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
     sz = sizeof(BITS32)*(2*it->ntrys+it->nentries);
     /* allocate space */
     if (!(it->udi_data = (BITS32*)Yap_AllocCodeSpace(sz)))
       return;
     sorted0 = sorted = (BITS32 *)it->udi_data;
     sorted++; /* leave an initial hole */
     for (i=0; i < it->hsize; i++) {
       if (it->key[i]) {
	 BITS32 *s0 = sorted;
	 BITS32 offset = it->key[i]/arity, offset0 = offset;

	 *sorted++ = 0;
	 do {
	   *sorted++ = offset;
	   offset = it->links[offset];
	 } while (offset);
	 //      S = it->cls+it->arity*offset0; Yap_DebugPlWrite(S[1]);
	 // fprintf(stderr, " key[i]=%d offset=%d  %d\n", it->key[i], offset0, (sorted-s0)-1);
	 if (sorted-s0 == 2) {
	   it->links[offset0] = 0;
	   sorted = s0;
	 } else {
	   /* number of elements comes first */
	   *s0 = sorted - (s0+1);
	   qsort(s0+1, (size_t)*s0, sizeof(BITS32), compar); 
	   it->links[offset0] = s0-sorted0;
	 }
       }
     }
     sz = sizeof(BITS32)*(sorted-sorted0);
     it->udi_data = (BITS32 *)Yap_ReallocCodeSpace((char *)it->udi_data, sz);
   }
   it->is_udi = TRUE;
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

   LOCAL_exo_base = it->cls;
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
     pt  = c+(it->links[off]+1);
     end  = c+(it->links[off]+n);
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
     S = it->cls+it->arity*pt[0];
     if (pt < end ) {
       YENV[-2] = (CELL)( pt+1 );
       YENV[-1] = (CELL)( end );
       YENV -= 2;
       return it->code;
     }
     return NEXTOP(NEXTOP(it->code,lp),lp);
   }
   at = AtomOfTerm(op);
   if (at == AtomAny || at == AtomMin) {
     S = it->cls+it->arity*pt[0];
   } else if (at == AtomMax) {
     S = it->cls+it->arity*end[0];
   } else if (at == AtomUnique) {
     if (end-2 > pt)
       return FAILCODE;
     S = it->cls+it->arity*pt[0];
   }
   return NEXTOP(NEXTOP(it->code,lp),lp);
 }

static yamop *
IntervalEnterUDIIndex(struct index_t *it USES_REGS)
{
  Int i = it->udi_arg;
  Term  t = XREGS[i+1], a1;
  BITS32 off = EXO_ADDRESS_TO_OFFSET(it, S)/it->arity;
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
  CELL *w = (CELL*)(B+1);
  BITS32 *end = (BITS32 *) w[it->arity+2],
    *pt = (BITS32 *) w[it->arity+1];
  BITS32 f = *pt;

  S = it->cls+it->arity*f;
  if (pt++ == end) return FALSE;
  w[it->arity+1] = (CELL)pt;
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
