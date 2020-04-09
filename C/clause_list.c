#include "Yap.h"
#include "clause.h"
#include "tracer.h"
#ifdef YAPOR
#include "or.macros.h"
#endif /* YAPOR */
#include "clause_list.h"

/* need to fix overflow handling */

static void mk_blob(int sz USES_REGS) {
  MP_INT *dst;

  HR[0] = (CELL)FunctorBigInt;
  HR[1] = CLAUSE_LIST;
  dst = (MP_INT *)(HR + 2);
  dst->_mp_size = 0L;
  dst->_mp_alloc = sz;
  HR += (1 + sizeof(MP_INT) / sizeof(CELL));
  HR[sz] = EndSpecials;
  HR += sz + 1;
}

static CELL *extend_blob(CELL *start, int sz USES_REGS) {
  UInt osize;
  MP_INT *dst;

  if (HR + sz > ASP)
    return NULL;
  dst = (MP_INT *)(start + 2);
  osize = dst->_mp_alloc;
  start += (1 + sizeof(MP_INT) / sizeof(CELL));
  start[sz + osize] = EndSpecials;
  dst->_mp_alloc += sz;
  HR += sz;
  return start + osize;
}

/*init of ClasuseList*/
 clause_list_t Yap_ClauseListInit(clause_list_t in) {
  CACHE_REGS in->n = 0;
  in->start = HR;
  mk_blob(0 PASS_REGS);
  in->end = HR;
  return in;
}

/*add clause to ClauseList
 returns FALSE on error*/
  int Yap_ClauseListExtend(clause_list_t cl, void *clause, void *pred) {
  CACHE_REGS
  PredEntry *ap = (PredEntry *)pred;

  /*  fprintf(stderr,"cl=%p\n",clause); */
  if (cl->end != HR)
    return FALSE;
  if (cl->n == 0) {
    void **ptr;
    if (!(ptr = (void **)extend_blob(cl->start, 1 PASS_REGS)))
      return FALSE;
    ptr[0] = clause;
  } else if (cl->n == 1) {
    yamop **ptr;
    yamop *code_p, *fclause;

    if (!(ptr = (yamop **)extend_blob(
              cl->start, 2 * (CELL)NEXTOP((yamop *)NULL, Otapl) / sizeof(CELL) -
                             1 PASS_REGS)))
      return FALSE;
    fclause = ptr[-1];
    code_p = (yamop *)(ptr - 1);
    code_p->opc = Yap_opcode(_try_clause);
    code_p->y_u.Otapl.d = fclause;
    code_p->y_u.Otapl.s = ap->ArityOfPE;
    code_p->y_u.Otapl.p = ap;
#ifdef TABLING
    code_p->y_u.Otapl.te = ap->TableOfPred;
#endif
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, 0);
#endif /* YAPOR */
    code_p = NEXTOP(code_p, Otapl);
    code_p->opc = Yap_opcode(_trust);
    code_p->y_u.Otapl.d = clause;
    code_p->y_u.Otapl.s = ap->ArityOfPE;
    code_p->y_u.Otapl.p = ap;
#ifdef TABLING
    code_p->y_u.Otapl.te = ap->TableOfPred;
#endif
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, 0);
#endif /* YAPOR */
  } else {
    yamop *code_p;

    if (!(code_p = (yamop *)extend_blob(cl->start,
                                        ((CELL)NEXTOP((yamop *)NULL, Otapl)) /
                                            sizeof(CELL) PASS_REGS)))
      return FALSE;
    code_p->opc = Yap_opcode(_trust);
    code_p->y_u.Otapl.d = clause;
    code_p->y_u.Otapl.s = ap->ArityOfPE;
    code_p->y_u.Otapl.p = ap;
#ifdef TABLING
    code_p->y_u.Otapl.te = ap->TableOfPred;
#endif
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, 0);
#endif /* YAPOR */
    code_p = PREVOP(code_p, Otapl);
    code_p->opc = Yap_opcode(_retry);
  }
  cl->end = HR;
  cl->n++;
  return TRUE;
}

/*closes the clause list*/
 void Yap_ClauseListClose(clause_list_t cl) { /* no need to do nothing */
}

/*destroys the clause list freeing memory*/
 int Yap_ClauseListDestroy(clause_list_t cl) {
  CACHE_REGS
  if (cl->end != HR)
    return FALSE;
  HR = cl->start;
  return TRUE;
}

/*destroys clause list and returns only first clause*/
 void *Yap_ClauseListToClause(clause_list_t cl) {
  CACHE_REGS
  void **ptr;
  if (cl->end != HR)
    return NULL;
  if (cl->n != 1)
    return NULL;
  if (!(ptr = (void **)extend_blob(cl->start, 0 PASS_REGS)))
    return NULL;
  return ptr[-1];
}

/*return pointer to start of try-retry-trust sequence*/
 void *Yap_ClauseListCode(clause_list_t cl) {
  CELL *ptr;
  ptr = (CELL *)cl->start;
  ptr += (1 + sizeof(MP_INT) / sizeof(CELL));
  return (void *)ptr;
}

/* where to fail */
 void *Yap_FAILCODE(void) { return (void *)FAILCODE; }
