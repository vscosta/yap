/****** start of top macro */

CELL *pt0, *pt0_end;

bzero(stt, sizeof(Ystack_t));
if (!init_stack(stt, 0, pt0_[1] )) {
  LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
return 0;
 }
pt0 = pt0_;
pt0_end = pt0_end_;

while (true) {
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
 list_loop:
  mderef_head(d0, dd0, var_in_term_unk); /*DEB_DOOB();*/
var_in_term_nvar:
  if (IsPairTerm(d0)) {
    CELL *ptd1 = RepPair(d0);
    CELL d1 = VISIT_UNMARK(ptd1[0]);
LIST_HOOK_CODE;
if (IS_VISIT_MARKER(ptd1[0]))
      goto loop;
    *ptd1 = VISIT_MARK();
    push_sub_term(stt, d1, ptd1, pt0, pt0_end);
    if (stt->pt + 32 >= stt->max) {
LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
return 0;
    }
    pt0 = ptd1;
    pt0_end = ptd1 + 1;
    dd0 = d1;
    //    fprintf(stderr, "%ld at %s %ld@%ld-%ld %lx\n", stt->pt - stt->pt0,
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

    if (stt->pt + 32 >= stt->max) {
LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
return 0;
    }
    COMPOUND_HOOK_CODE;
    if (IS_VISIT_MARKER(*ptd1)) {

      goto loop;
    }
    *ptd1 = VISIT_MARK();
    push_sub_term(stt, d1, ptd1, pt0, pt0_end);
    pt0 = ptd1;
    pt0_end = ptd1 + a;
    //   fprintf(stderr, "%ld at %s %ld@%ld-%ld %lx\n", stt->pt -
    //	    stt->pt0, __FUNCTION__,ptd1-H0,pt0-H0,pt0_end-H0, *(CELL*)f);
    goto loop;
  } else {
    ATOMIC_HOOK_CODE;
    goto loop;
  }

  mderef_body(d0,dd0, ptd0, var_in_term_unk, var_in_term_nvar);
/*enter variable processing */ {
 
