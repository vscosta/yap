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
  /* fprintf(stderr, "%ld-%ld\n", IntOfTerm(i), j); */
  return IntOfTerm(i)-j;
}


static void
RangeUDIRefitIndex(struct index_t **ip, UInt b[] USES_REGS)
{
  size_t sz;
  struct index_t *it = *ip;
  BITS32 *sorted0, *sorted;
  UInt arity = it->arity;
  yamop *code;

  /* hard-wired implementation for the range case */
  Int i = it->udi_arg;
  /* it is bound, use hash */
  if (it->bmap & b[i]) return;
  /* no constraints, nothing to gain */
  if (!IsAttVar(VarOfTerm(XREGS[i+1]))) return;
  /* be conservative */
  sz = sizeof(BITS32)*(it->ntrys+it->nentries*2);
  /* allocate space */
  if (!(it->udi_data = malloc(sz)))
    return;
  sorted0 = sorted = (BITS32 *)it->udi_data;
  sorted++; /* leave an initial hole */
  LOCAL_exo_base = it->cls;
  LOCAL_exo_arity = it->arity;
  LOCAL_exo_arg = i;
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
  it->is_udi = i+1;
  code = it->code;
  code->opc = Yap_opcode(_try_exo_udi);
  code = NEXTOP(code, lp);
  code->opc = Yap_opcode(_retry_exo_udi);
}

static yamop *
Min(struct index_t *it, BITS32 off USES_REGS)
{
  if (it->links[off]) {
    BITS32 *c = (BITS32 *)it->udi_data;
    BITS32 f = c[it->links[off]+1];
    S = it->cls+it->arity*f;
  }
  return NEXTOP(NEXTOP(it->code,lp),lp);
}

static yamop *
Max(struct index_t *it, BITS32 off USES_REGS)
{
  if (it->links[off]) {
    BITS32 *c = (BITS32 *)it->udi_data;
    BITS32 n = c[it->links[off]];
    BITS32 f = c[it->links[off]+n];
    S = it->cls+it->arity*f;
  }
  return NEXTOP(NEXTOP(it->code,lp),lp);
}

static yamop *
Gt(struct index_t *it, Int x, BITS32 off USES_REGS)
{
  if (it->links[off]) {
    BITS32 *c = (BITS32 *)it->udi_data;
    BITS32 n = c[it->links[off]];

    LOCAL_exo_base = it->cls;
    LOCAL_exo_arity = it->arity;
    LOCAL_exo_arg = it->udi_arg;
    BITS32 *pt  = c+(it->links[off]+1);
    BITS32 *end  = c+(it->links[off]+(n+2));
    if (n > 8 && FALSE) {
      //      start = binary_search(start,end, x, it);
    } else {
      while ( pt < end && compare(pt, x PASS_REGS) <= 0 ) {
	pt++;
      }
    }
    if (pt == end) 
      return FAILCODE;
    S = it->cls+it->arity*pt[0];
    end --;
    if (pt < end ) {
      YENV[-2] = (CELL)( pt+1 );
      YENV[-1] = (CELL)( end );
      YENV -= 2;
      return it->code;
    }
  }
  return NEXTOP(NEXTOP(it->code,lp),lp);
}

static yamop *
Lt(struct index_t *it, Int x, BITS32 off USES_REGS)
{
  if (it->links[off]) {
    BITS32 *c = (BITS32 *)it->udi_data;
    BITS32 n = c[it->links[off]];

    LOCAL_exo_base = it->cls;
    LOCAL_exo_arity = it->arity;
    LOCAL_exo_arg = it->udi_arg;
    BITS32 *start  = c+(it->links[off]+1), *pt = start+1;
    BITS32 *end  = c+(it->links[off]+(n+2));
    if (n > 8 && FALSE) {
      //      start = binary_search(start,end, x, it);
    } else {
      if (compare(start, x PASS_REGS) >= 0)
	return FAILCODE;
      while ( pt < end && compare(pt, x PASS_REGS) < 0 ) {
	pt++;
      }
    }
    S = it->cls+it->arity*start[0];
    pt --;
    if ( pt > start ) {
      YENV[-2] = (CELL)( start+1 );
      YENV[-1] = (CELL)( pt  );
      YENV -= 2;
      return it->code;
    }
  }
  return NEXTOP(NEXTOP(it->code,lp),lp);
}

static yamop *
Eq(struct index_t *it, Int x, BITS32 off USES_REGS)
{
  if (it->links[off]) {
    BITS32 *c = (BITS32 *)it->udi_data;
    BITS32 n = c[it->links[off]];

    LOCAL_exo_base = it->cls;
    LOCAL_exo_arity = it->arity;
    LOCAL_exo_arg = it->udi_arg;
    BITS32 *end  = c+(it->links[off]+(n+2));
    BITS32 *start, *pt = c+(it->links[off]+1);
    if (n > 8 && FALSE) {
      //      start = binary_search(start,end, x, it);
    } else {
      Int c = 0;
      while ( pt < end && (c = compare(pt, x PASS_REGS)) < 0 ) {
	pt++;
      }
      if (pt == end || c) 
	return FAILCODE;
      start = pt;
      pt ++;
      while ( pt < end && (c = compare(pt, x PASS_REGS)) == 0 ) {
	pt++;
      }
    }
    S = it->cls+it->arity*start[0];
    pt --;
    if ( pt > start ) {
      YENV[-2] = (CELL)( start+1 );
      YENV[-1] = (CELL)( pt  );
      YENV -= 2;
      return it->code;
    }
  }
  return NEXTOP(NEXTOP(it->code,lp),lp);
}

static yamop *
All(struct index_t *it, BITS32 off USES_REGS)
{
  if (it->links[off]) {
    BITS32 *c = (BITS32 *)it->udi_data;
    BITS32 n = c[it->links[off]];

    LOCAL_exo_base = it->cls;
    LOCAL_exo_arity = it->arity;
    LOCAL_exo_arg = it->udi_arg;
    BITS32 *start  = c+(it->links[off]+1);
    BITS32 *end  = c+(it->links[off]+(n+1));
    S = it->cls+it->arity*start[0];
    if ( end > start ) {
      YENV[-2] = (CELL)( start+1 );
      YENV[-1] = (CELL)( end  );
      YENV -= 2;
      return it->code;
    }
  }
  return NEXTOP(NEXTOP(it->code,lp),lp);
}

static yamop *
RangeEnterUDIIndex(struct index_t *it USES_REGS)
{
  Int i = it->udi_arg;
  Term  t = XREGS[i+1], a1;
  BITS32 off = EXO_ADDRESS_TO_OFFSET(it, S)/it->arity;
  //  printf("off=%d it=%p %p---%p\n", off, it, it->cls, S);
  attvar_record *attv;
  Atom at;

  t = Deref(t);
  if (!IsVarTerm(t))
    return FALSE;
  if(!IsAttVar(VarOfTerm(t)))
    return FALSE;
  attv = RepAttVar(VarOfTerm(t));
  t =  attv->Atts;
  a1 = ArgOfTerm(2,t);
  if (IsAtomTerm(a1)) {
    at = AtomOfTerm(a1);
  } else {
    Functor f = FunctorOfTerm(a1);
    at  = NameOfFunctor(f);
  }
  if (at == AtomMax) {
    return Max(it, off PASS_REGS);
  } else if (at == AtomMin) {
    return Min(it, off PASS_REGS);
  } else if (at == AtomGT) {
    Term arg = ArgOfTerm(1, a1);
    if (IsVarTerm(arg))
      return All(it, off PASS_REGS);
    else if (!IsIntTerm(arg)) {
      Yap_Error(TYPE_ERROR_INTEGER, arg, "data-base constraint");
      return FAILCODE;
    }
    return Gt(it, IntOfTerm(arg), off PASS_REGS);
  } else if (at == AtomLT) {
    Term arg = ArgOfTerm(1, a1);

    if (IsVarTerm(arg))
      return All(it, off PASS_REGS);
    else if (!IsIntTerm(arg)) {
      Yap_Error(TYPE_ERROR_INTEGER, t, "data-base constraint");
      return FAILCODE;
    }
    return Lt(it, IntOfTerm(arg), off PASS_REGS);
  } else if (at == AtomEQ) {
    Term arg = ArgOfTerm(1, a1);

    if (IsVarTerm(arg))
      return All(it, off PASS_REGS);
    else if (!IsIntTerm(arg)) {
      Yap_Error(TYPE_ERROR_INTEGER, t, "data-base constraint");
      return FAILCODE;
    }
    return Eq(it, IntOfTerm(arg), off PASS_REGS);
  }
  return FAILCODE;
}

static int
RangeRetryUDIIndex(struct index_t *it USES_REGS)
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


static struct udi_control_block RangeCB;

typedef struct exo_udi_access_t {
  CRefitExoIndex refit;
} exo_udi_encaps_t;

static struct exo_udi_access_t ExoCB;

static void *
RangeUdiInit (Term spec, int arg, int arity) {
  ExoCB.refit = RangeUDIRefitIndex;
  return (void *)&ExoCB;
}

static void *
RangeUdiInsert (void *control,
                Term term, int arg, void *data)
{
  CACHE_REGS

  struct index_t **ip = (struct index_t **)term;
  (*ip)->udi_arg = arg-1;
  (ExoCB.refit)(ip, LOCAL_ibnds PASS_REGS);
  (*ip)->udi_first = (void *)RangeEnterUDIIndex;
  (*ip)->udi_next = (void *)RangeRetryUDIIndex;
  return control;
}

static int RangeUdiDestroy(void *control)
{
  return TRUE;
}



void Yap_udi_range_init(void) {
  UdiControlBlock cb = &RangeCB;

  memset((void *) cb,0, sizeof(*cb));

  /*TODO: ask vitor why this gives a warning*/
  cb->decl=Yap_LookupAtom("range");

  cb->init= RangeUdiInit;
  cb->insert=RangeUdiInsert;
  cb->search=NULL;
  cb->destroy=RangeUdiDestroy;

  Yap_UdiRegister(cb);
}
