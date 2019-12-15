/****** start of top macro */
CELL *pt0, *pt0_end;
tstack_t stt;
size_t sz = 1024;
reset :
HB = HR;
init_stack(&stt, sz);
if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
  /* Trail overflow */
  goto trail_overflow;
}
if (HR + 1024 > ASP) {
  goto global_overflow;
}
pt0 = pt0_;
pt0_end = pt0_end_;

while (true) {
loop:

  while (pt0 == pt0_end) {
    if (!pop_sub_term(&stt, &pt0, &pt0_end)) {
    goto nomore;
    }
  }
  CELL d0;
  CELL *ptd0;
  pt0++;
  ptd0 = pt0;
  d0 = VISIT_UNMARK(*ptd0);
 list_loop:
  mderef_head(d0, var_in_term_unk); /*DEB_DOOB();*/
var_in_term_nvar:
  if (IsPairTerm(d0)) {
    CELL *ptd1 = RepPair(d0);
    CELL d1 = VISIT_UNMARK(ptd1[0]);
    if (IS_VISIT_MARKER(ptd1[0]))
      goto loop;
    LIST_HOOK_CODE;
    *ptd1 = VISIT_MARK();
    push_sub_term(&stt, d1, ptd1, pt0, pt0_end);
    pt0 = ptd1;
    pt0_end = ptd1 + 1;
    d0 = d1;
    //    fprintf(stderr, "%ld at %s %ld@%ld-%ld %lx\n", stt.pt - stt.pt0,
    //       __FUNCTION__, ptd1 - H0, pt0 - H0, pt0_end - H0, *ptd1);
    goto list_loop;
  } else if (IsApplTerm(d0)) {
    register Functor f;
    /* store the terms to visit */
    CELL *ptd1 = RepAppl(d0), d1;
    f = (Functor)(d1 = VISIT_UNMARK(*ptd1));
    if (IsExtensionFunctor(f)) {
      goto loop;
    }

    if (stt.pt + 32 >= stt.max) {
      goto aux_overflow;
    }
    COMPOUND_HOOK_CODE;
    if (IS_VISIT_MARKER(*ptd1)) {

      goto loop;
    }
    *ptd1 = VISIT_MARK();
    push_sub_term(&stt, d1, ptd1, pt0, pt0_end);
    arity_t a = ArityOfFunctor(f);
    pt0 = ptd1;
    pt0_end = ptd1 + a;
    //   fprintf(stderr, "%ld at %s %ld@%ld-%ld %lx\n", stt.pt -
    //	    stt.pt0, __FUNCTION__,ptd1-H0,pt0-H0,pt0_end-H0, *(CELL*)f);
    goto loop;
  } else {
    ATOMIC_HOOK_CODE;
    goto loop;
  }
  mderef_body(d0, ptd0, var_in_term_unk, var_in_term_nvar);
/*enter variable processing */
 
