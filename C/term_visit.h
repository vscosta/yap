/**
 * @file term_visit.h
 *
 * start of top macro */
 DEB_DOOBIN(*pt0_);
CELL *pt0=&t-1, *pt0_end=&t;

Ystack_t stt_, *stt = &stt_;
//Int rc;<

init_stack(stt);
do {
    yhandle_t y0 = Yap_StartHandles();
 RESET_TERM_VISITOR;
Yap_CloseHandles(y0)
;
 loop:
  while (pt0 == pt0_end) {
    if (!pop_sub_term(stt, &pt0, &pt0_end)) {

      goto nomore;
    }
  }
  CELL d0, dd0;
  CELL *ptd0;
  pt0++;
  ptd0 = pt0;
  dd0 = (*ptd0);
  mderef_head(d0, dd0, var_in_term_unk); /*DEB_DOOB();*/
 var_in_term_nvar:
  if (IsPairTerm(d0)) {
    CELL *ptd1 = RepPair(d0);
#if  defined(LIST_HOOK_CODE)
    LIST_HOOK_CODE;
#endif
    if (IS_VISIT_MARKER(ptd1[0]))
      goto loop;
    CELL d1 = VISIT_UNMARK(ptd1[0]);
    push_sub_term(stt, d1, ptd1, pt0, pt0_end);
    pt0 = ptd1-1;
    pt0_end = ptd1 + 1;
    //    fprintf(stderr, "%ld at %s %ld@%ld-%ld %lx\n", stt->pt - stt->pt0,
    //       __FUNCTION__, ptd1 - H0, pt0 - H0, pt0_end - H0, *ptd1);
    goto loop;
  } else if (IsApplTerm(d0)) {
    Functor f;
    arity_t a;
    /* store the terms to visit */
    CELL *ptd1 = RepAppl(d0), d1;
    f = (Functor)(d1 = VISIT_UNMARK(*ptd1));
#if  defined(COMPOUND_HOOK_CODE)
    COMPOUND_HOOK_CODE;
#endif
    if (IsExtensionFunctor(f)) {
      if (f == FunctorAttVar)
	a =3;
      else
	goto loop;
    } else {
      a = ArityOfFunctor(f);
    }

    if (IS_VISIT_MARKER(*ptd1)) {

      goto loop;
    }
    push_sub_term(stt, d1, ptd1, pt0, pt0_end);
    pt0 = ptd1;
    pt0_end = ptd1 + a;
    //   fprintf(stderr, "%ld at %s %ld@%ld-%ld %lx\n", stt->pt -
    //	    stt->pt0, __FUNCTION__,ptd1-H0,pt0-H0,pt0_end-H0, *(CELL*)f);
    goto loop;
  } else {
#if  defined(ATOMIC_HOOK_CODE)
    ATOMIC_HOOK_CODE;
#endif
    goto loop;
  }

  mderef_body(d0,dd0, ptd0, var_in_term_unk, var_in_term_nvar);
  /*enter variable processing */ {
#if  defined(VAR_HOOK_CODE)
    VAR_HOOK_CODE;
#endif
    goto loop;
  }
 } while (stt->err != YAP_NO_ERROR);
nomore:

#undef  RESET_TERM_VISITOR


