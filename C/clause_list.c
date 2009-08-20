#include "Yap.h"
#include "clause.h"
#include "tracer.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#include "clause_list.h"

/* need to fix overflow handling */

static void
mk_blob(int sz)
{
  MP_INT *dst;
 
  H[0] = (CELL)FunctorBigInt;
  H[1] = CLAUSE_LIST;
  dst = (MP_INT *)(H+2);
  dst->_mp_size = 0L;
  dst->_mp_alloc = sz;
  H += (1+sizeof(MP_INT)/sizeof(CELL));
  H[sz] = EndSpecials;
  H += sz+1;
}

static CELL *
extend_blob(CELL *start, int sz)
{
  UInt osize;
  MP_INT *dst;
  
  if (H + sz > ASP)
    return NULL;
  dst = (MP_INT *)(start+2);
  osize = dst->_mp_alloc;
  start += (1+sizeof(MP_INT)/sizeof(CELL));
  start[sz+osize] = EndSpecials;
  dst->_mp_alloc += sz;
  H += sz;
  return start+osize;
}

/*init of ClasuseList*/
X_API clause_list_t
Yap_ClauseListInit(clause_list_t in)
{
  in->n = 0;
  in->start = H;
  mk_blob(0);
  in->end = H;
  return in;
}

/*add clause to ClauseList
 returns FALSE on error*/
X_API int
Yap_ClauseListExtend(clause_list_t cl, void * clause, void *pred)
{
  PredEntry *ap = (PredEntry *)pred;

  /*  fprintf(stderr,"cl=%p\n",clause); */
  if (cl->end != H)
    return FALSE;
  if (cl->n == 0) {
    void **ptr;
    if (!(ptr = (void **)extend_blob(cl->start,1))) return FALSE;
    ptr[0] = clause;
  } else if (cl->n == 1)  {
    yamop **ptr;
    yamop *code_p, *fclause;
    
    if (!(ptr = (yamop **)extend_blob(cl->start,2*(CELL)NEXTOP((yamop *)NULL,Otapl)/sizeof(CELL)-1))) return FALSE;
    fclause = ptr[-1];
    code_p = (yamop *)(ptr-1);
    code_p->opc = Yap_opcode(_try_clause);
    code_p->u.Otapl.d = fclause;
    code_p->u.Otapl.s = ap->ArityOfPE;
    code_p->u.Otapl.p = ap;
#ifdef TABLING
    code_p->u.Otapl.te = ap->TableOfPred;
#endif
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, 0);
#endif /* YAPOR */
    code_p = NEXTOP(code_p,Otapl);
    code_p->opc = Yap_opcode(_trust);
    code_p->u.Otapl.d = clause;
    code_p->u.Otapl.s = ap->ArityOfPE;
    code_p->u.Otapl.p = ap;
#ifdef TABLING
    code_p->u.Otapl.te = ap->TableOfPred;
#endif
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, 0);
#endif /* YAPOR */
  } else {
    yamop *code_p;

    if (!(code_p = (yamop *)extend_blob(cl->start,((CELL)NEXTOP((yamop *)NULL,Otapl))/sizeof(CELL)))) return FALSE;
    code_p->opc = Yap_opcode(_trust);
    code_p->u.Otapl.d = clause;
    code_p->u.Otapl.s = ap->ArityOfPE;
    code_p->u.Otapl.p = ap;
#ifdef TABLING
    code_p->u.Otapl.te = ap->TableOfPred;
#endif
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, 0);
#endif /* YAPOR */
    code_p = PREVOP(code_p,Otapl);
    code_p->opc = Yap_opcode(_retry);
  }
  cl->end = H;
  cl->n++;
  return TRUE;
}

/*closes the clause list*/
X_API void
Yap_ClauseListClose(clause_list_t cl)
{
  /* no need to do nothing */
}

/*destroys the clause list freeing memory*/
X_API int
Yap_ClauseListDestroy(clause_list_t cl)
{
  if (cl->end != H)
    return FALSE;
  H = cl->start;
  return TRUE;
}

/*destroys clause list and returns only first clause*/
X_API void *
Yap_ClauseListToClause(clause_list_t cl)
{
  void **ptr;
  if (cl->end != H)
    return NULL;
  if (cl->n != 1)
    return NULL;
  if (!(ptr = (void **)extend_blob(cl->start,0))) return NULL;
  return ptr[-1];
}

/*return pointer to start of try-retry-trust sequence*/
X_API void *
Yap_ClauseListCode(clause_list_t cl)
{
  CELL *ptr;
  ptr = (CELL *)cl->start;
  ptr += (1+sizeof(MP_INT)/sizeof(CELL));
  return (void *)ptr;
}

/* where to fail */
X_API void *
Yap_FAILCODE(void)
{
  return (void *)FAILCODE;
}


