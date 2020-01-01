/****** start of top macro */
CELL *pt0, *pt0_end, *HStart = HR;
Ystack_t stt;
size_t sz = 1024;
if (!init_stack(&stt, sz)) {

  aux_overflow : {
    while (pop_sub_term(&stt, NULL, NULL)) {};
    HR = HStart;
    clean_tr(TR0 PASS_REGS);
    if (reinit_stack(&stt,0))
    {
      HStart = HR;
      goto reset;
    }
     close_stack(&stt); return false;
}

  trail_overflow : {
    while (pop_sub_term(&stt, NULL, NULL)) {
    };
    HR = HStart;
    LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
    ssize_t expand = 0L;
    if (!Yap_gcl(expand, 3, ENV, gc_P(P, CP))) {

       close_stack(&stt); return false;
    }
      HStart = HR;
    goto reset;
  }
  global_overflow : {
    while (pop_sub_term(&stt, NULL, NULL)) {
    };
    HR = HStart;
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
    ssize_t expand = 0L;
    if (!Yap_gcl(expand, 3, ENV, gc_P(P, CP))) {

      close_stack(&stt); return false;
    }
      HStart = HR;
  }
 }

if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
  /* Trail overflow */
  goto trail_overflow;
}
if (HR + 1024 > ASP) {
  goto global_overflow;
}
reset:
pt0 = pt0_;
pt0_end = pt0_end_;

while (true) {
loop:

  while (pt0 == pt0_end) {
    if (!pop_sub_term(&stt, &pt0, &pt0_end)) {
    goto nomore;
    }
  }
  CELL d0, dd0;
  CELL *ptd0;
  pt0++;
  ptd0 = pt0;
  dd0 = (*ptd0);
 list_loop:
  mderef_head(d0, dd0, var_in_term_unk); /*DEB_DOOB();*/
var_in_term_nvar:
  if (IsPairTerm(d0)) {
    CELL *ptd1 = RepPair(d0);
    CELL d1 = VISIT_UNMARK(ptd1[0]);
    if (IS_VISIT_MARKER(ptd1[0]))
      goto loop;
    LIST_HOOK_CODE;
    *ptd1 = VISIT_MARK();
    push_sub_term(&stt, d1, ptd1, pt0, pt0_end);
    if (stt.pt + 32 >= stt.max) {
      goto aux_overflow;
    }
    pt0 = ptd1;
    pt0_end = ptd1 + 1;
    dd0 = d1;
    //    fprintf(stderr, "%ld at %s %ld@%ld-%ld %lx\n", stt.pt - stt.pt0,
    //       __FUNCTION__, ptd1 - H0, pt0 - H0, pt0_end - H0, *ptd1);
    goto list_loop;
  } else if (IsApplTerm(d0)) {
   Functor f;
   arity_t a;
    /* store the terms to visit */
    CELL *ptd1 = RepAppl(d0), d1;
    f = (Functor)(d1 = VISIT_UNMARK(*ptd1));
    if (IsExtensionFunctor(f)) {
      if (f == FunctorAttVar)
	a =3;
      else
	goto loop;
    } else {
      a = ArityOfFunctor(f);
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
    pt0 = ptd1;
    pt0_end = ptd1 + a;
    //   fprintf(stderr, "%ld at %s %ld@%ld-%ld %lx\n", stt.pt -
    //	    stt.pt0, __FUNCTION__,ptd1-H0,pt0-H0,pt0_end-H0, *(CELL*)f);
    goto loop;
  } else {
    ATOMIC_HOOK_CODE;
    goto loop;
  }

  mderef_body(d0,dd0, ptd0, var_in_term_unk, var_in_term_nvar);
/*enter variable processing */ {
 
