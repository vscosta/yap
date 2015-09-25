#define _p_integer_x_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xl.x); \
      deref_head(d0, integer_x_unk); \
    integer_x_nvar: \
      if (IsIntTerm(d0)) { \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONEXT(); \
      } \
      if (IsApplTerm(d0)) { \
	Functor f0 = FunctorOfTerm(d0); \
	if (IsExtensionFunctor(f0)) { \
	  switch ((CELL)f0) { \
	  case (CELL)FunctorLongInt: \
	  case (CELL)FunctorBigInt: \
	    (*_PREG) = NEXTOP((*_PREG), xl); \
	    GONEXT(); \
	  default: \
	    (*_PREG) = (*_PREG)->u.xl.F; \
	    BACK(); \
	  } \
	} \
      } \
      (*_PREG) = (*_PREG)->u.xl.F; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, integer_x_unk, integer_x_nvar); \
      (*_PREG) = (*_PREG)->u.xl.F; \
      BACK(); \
      ENDP(pt0); \
      ENDD(d0);
     
#define _p_plus_vv_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = XREG((*_PREG)->u.xxx.x1); \
      deref_head(d0, plus_vv_unk); \
    plus_vv_nvar: \
      d1 = XREG((*_PREG)->u.xxx.x2); \
      deref_head(d1, plus_vv_nvar_unk); \
    plus_vv_nvar_nvar: \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1)); \
      } \
      else { \
	saveregs(); \
	d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
	if (d0 == 0L) { \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL(); \
	} \
      } \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, plus_vv_unk, plus_vv_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is _+B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, plus_vv_nvar_unk, plus_vv_nvar_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A+B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);
      
#define _p_plus_vc_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xxn.xi); \
      deref_head(d0, plus_vc_unk); \
    plus_vc_nvar: \
      { \
	Int d1 = (*_PREG)->u.xxn.c; \
	if (IsIntTerm(d0)) { \
	  d0 = MkIntegerTerm(IntOfTerm(d0) + d1); \
	} \
	else { \
	  saveregs(); \
	  d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs(); \
	  if (d0 == 0L) { \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL(); \
	  } \
	} \
      } \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, plus_vc_unk, plus_vc_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A + " Int_FORMAT, (*_PREG)->u.xxn.c); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d0);
      
#define _p_plus_y_vv_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = XREG((*_PREG)->u.yxx.x1); \
      deref_head(d0, plus_y_vv_unk); \
    plus_y_vv_nvar: \
      d1 = XREG((*_PREG)->u.xxx.x2); \
      deref_head(d1, plus_y_vv_nvar_unk); \
    plus_y_vv_nvar_nvar: \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1)); \
      } \
      else { \
	saveregs(); \
	d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
	if (d0 == 0L) { \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL(); \
	} \
      } \
      BEGP(pt0); \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      ENDP(pt0); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, plus_y_vv_unk, plus_y_vv_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A+B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, plus_y_vv_nvar_unk, plus_y_vv_nvar_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A+B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define _p_plus_y_vc_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.yxn.xi); \
      deref_head(d0, plus_y_vc_unk); \
    plus_y_vc_nvar: \
      { \
	Int d1 = (*_PREG)->u.yxn.c; \
	if (IsIntTerm(d0)) { \
	  d0 = MkIntegerTerm(IntOfTerm(d0) + d1); \
	} \
	else { \
	  saveregs(); \
	  d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs(); \
	  if (d0 == 0L) { \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL(); \
	  } \
	} \
      } \
      BEGP(pt0); \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      ENDP(pt0); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, plus_y_vc_unk, plus_y_vc_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A + " Int_FORMAT, (*_PREG)->u.yxn.c); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d0);
      
#define _p_minus_vv_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = XREG((*_PREG)->u.xxx.x1); \
      deref_head(d0, minus_vv_unk); \
    minus_vv_nvar: \
      d1 = XREG((*_PREG)->u.xxx.x2); \
      deref_head(d1, minus_vv_nvar_unk); \
    minus_vv_nvar_nvar: \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1)); \
      } \
      else { \
	saveregs(); \
	d0 = p_minus(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
	if (d0 == 0L) { \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL(); \
	} \
      } \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, minus_vv_unk, minus_vv_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, minus_vv_nvar_unk, minus_vv_nvar_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);
      
#define _p_times_vv_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = XREG((*_PREG)->u.xxx.x1); \
      deref_head(d0, times_vv_unk); \
    times_vv_nvar: \
      d1 = XREG((*_PREG)->u.xxx.x2); \
      deref_head(d1, times_vv_nvar_unk); \
    times_vv_nvar_nvar: \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	d0 = times_int(IntOfTerm(d0), IntOfTerm(d1)); \
      } \
      else { \
	saveregs(); \
	d0 = p_times(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
	if (d0 == 0L) { \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL(); \
	} \
      } \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, times_vv_unk, times_vv_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, times_vv_nvar_unk, times_vv_nvar_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define _p_times_vc_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xxn.xi); \
      deref_head(d0, times_vc_unk); \
    times_vc_nvar: \
      { \
	Int d1 = (*_PREG)->u.xxn.c; \
	if (IsIntTerm(d0)) { \
	  d0 = times_int(IntOfTerm(d0), d1); \
	} \
	else { \
	  saveregs(); \
	  d0 = p_times(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs(); \
	  if (d0 == 0L) { \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL(); \
	  } \
	} \
      } \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, times_vc_unk, times_vc_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A* " Int_FORMAT, (*_PREG)->u.xxn.c); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d0);
      
#define _p_div_vv_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = XREG((*_PREG)->u.xxx.x1); \
      deref_head(d0, div_vv_unk); \
    div_vv_nvar: \
      d1 = XREG((*_PREG)->u.xxx.x2); \
      deref_head(d1, div_vv_nvar_unk); \
    div_vv_nvar_nvar: \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	Int div = IntOfTerm(d1); \
	if (div == 0) { \
	  saveregs(); \
	  Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2"); \
	  setregs(); \
	  FAIL(); \
	} \
	d0 = MkIntTerm(IntOfTerm(d0) / div); \
      } \
      else { \
	saveregs(); \
	d0 = p_div(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
	if (d0 == 0L) { \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL(); \
	} \
      } \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, div_vv_unk, div_vv_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, div_vv_nvar_unk, div_vv_nvar_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);
      
#define _p_and_vv_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = XREG((*_PREG)->u.xxx.x1); \
      deref_head(d0, and_vv_unk); \
    and_vv_nvar: \
      d1 = XREG((*_PREG)->u.xxx.x2); \
      deref_head(d1, and_vv_nvar_unk); \
    and_vv_nvar_nvar: \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1)); \
      } \
      else { \
	saveregs(); \
	d0 = p_and(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
	if (d0 == 0L) { \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL(); \
	} \
      } \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, and_vv_unk, and_vv_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, and_vv_nvar_unk, and_vv_nvar_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);
      
#define _p_and_vc_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xxn.xi); \
      deref_head(d0, and_vc_unk); \
    and_vc_nvar: \
      { \
	Int d1 = (*_PREG)->u.xxn.c; \
	if (IsIntTerm(d0)) { \
	  d0 = MkIntegerTerm(IntOfTerm(d0) & d1); \
	} \
	else { \
	  saveregs(); \
	  d0 = p_and(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs(); \
	  if (d0 == 0L) { \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL(); \
	  } \
	} \
      } \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, and_vc_unk, and_vc_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A /\\ " Int_FORMAT , (*_PREG)->u.xxn.c); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d0);
      
#define _p_sll_cv_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xxn.xi); \
      deref_head(d0, sll_cv_unk); \
    sll_cv_nvar: \
      { \
	Int d1 = (*_PREG)->u.xxn.c; \
	if (IsIntTerm(d0)) { \
	  Int i2 = IntOfTerm(d0); \
	  if (i2 < 0) { \
	    d0 = MkIntegerTerm(SLR(d1, -i2)); \
	  } else { \
	    d0 = do_sll(d1,i2); \
	} \
	} else { \
	  saveregs(); \
	  d0 = p_sll(MkIntegerTerm(d1), Yap_Eval(d0)); \
	  setregs(); \
	} \
      } \
      if (d0 == 0L) { \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL(); \
      } \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, sll_cv_unk, sll_cv_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d0);
      
#define _p_slr_vc_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xxn.xi); \
      deref_head(d0, slr_vc_unk); \
    slr_vc_nvar: \
      { \
	Int d1 = (*_PREG)->u.xxn.c; \
	if (IsIntTerm(d0)) { \
	  d0 = MkIntTerm(SLR(IntOfTerm(d0), d1)); \
	} \
	else { \
	  saveregs(); \
	  d0 = p_slr(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs(); \
	  if (d0 == 0L) { \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL(); \
	  } \
	} \
      } \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, slr_vc_unk, slr_vc_nvar); \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B"); \
      setregs(); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d0);

#define _call_bfunc_xx_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = XREG((*_PREG)->u.plxxs.x1); \
    call_bfunc_xx_nvar: \
      d1 = XREG((*_PREG)->u.plxxs.x2); \
    call_bfunc_xx2_nvar: \
      deref_head(d0, call_bfunc_xx_unk); \
      deref_head(d1, call_bfunc_xx2_unk); \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	COUNT flags; \
	Int v = IntOfTerm(d0) - IntOfTerm(d1); \
	flags = (*_PREG)->u.plxxs.flags; \
	if (v > 0) { \
	  if (flags & GT_OK_IN_CMP) { \
	    yamop *nextp = NEXTOP((*_PREG), plxxs); \
	    (*_PREG) = nextp; \
	    GONEXT(); \
      } \
	  else { \
	    yamop *nextp = (*_PREG)->u.plxxs.f; \
	    (*_PREG) = nextp; \
	    BACK(); \
      } \
    } \
	else if (v < 0) { \
	  if (flags & LT_OK_IN_CMP) { \
	    yamop *nextp = NEXTOP((*_PREG), plxxs); \
	    (*_PREG) = nextp; \
	    GONEXT(); \
      } \
	  else { \
	    yamop *nextp = (*_PREG)->u.plxxs.f; \
	    (*_PREG) = nextp; \
	    BACK(); \
      } \
	} \
	else { \
	  if (flags & EQ_OK_IN_CMP) { \
	    yamop *nextp = NEXTOP((*_PREG), plxxs); \
	    (*_PREG) = nextp; \
	    GONEXT(); \
	  } \
	  else { \
	    yamop *nextp = (*_PREG)->u.plxxs.f; \
	    (*_PREG) = nextp; \
	    BACK(); \
	  } \
	} \
      } \
    exec_bin_cmp_xx: \
      { \
	 CmpPredicate f = (*_PREG)->u.plxxs.p->cs.d_code; \
	 saveregs(); \
	 d0 = (CELL) (f) (d0,d1); \
	 setregs(); \
      } \
      if ((*_PREG) == FAILCODE) { \
	BACK(); \
      } \
      if (!d0) { \
	  (*_PREG) = (*_PREG)->u.plxxs.f; \
	BACK(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), plxxs); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, call_bfunc_xx_unk, call_bfunc_xx_nvar); \
      d1 = Deref(d1); \
      goto exec_bin_cmp_xx; \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, call_bfunc_xx2_unk, call_bfunc_xx2_nvar); \
      goto exec_bin_cmp_xx; \
      ENDP(pt0); \
 \
      ENDD(d1); \
      ENDD(d0);
      
#define _call_bfunc_yx_instinit \
      BEGD(d0); \
      BEGD(d1); \
      BEGP(pt0); \
      pt0 = YREG + (*_PREG)->u.plxys.y; \
      d1 = XREG((*_PREG)->u.plxys.x); \
      d0 = *pt0; \
      ENDP(pt0); \
      deref_head(d0, call_bfunc_yx_unk); \
    call_bfunc_yx_nvar: \
      deref_head(d1, call_bfunc_yx2_unk); \
    call_bfunc_yx2_nvar: \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	int flags; \
	Int v = IntOfTerm(d0) - IntOfTerm(d1); \
	flags = (*_PREG)->u.plxys.flags; \
	if (v > 0) { \
	  if (flags & GT_OK_IN_CMP) { \
	    (*_PREG) = NEXTOP((*_PREG), plxys); \
	    GONEXT(); \
	  } else { \
	    (*_PREG) = (*_PREG)->u.plxys.f; \
	    BACK(); \
	  } \
	} else if (v < 0) { \
	  if (flags & LT_OK_IN_CMP) { \
	    (*_PREG) = NEXTOP((*_PREG), plxys); \
	    GONEXT(); \
	  } else { \
	    (*_PREG) = (*_PREG)->u.plxys.f; \
	    BACK(); \
	  } \
	} else { \
	  if (flags & EQ_OK_IN_CMP) { \
	    (*_PREG) = NEXTOP((*_PREG), plxys); \
	    GONEXT(); \
	  } else { \
	    (*_PREG) = (*_PREG)->u.plxys.f; \
	    BACK(); \
	  } \
	} \
      } \
    exec_bin_cmp_yx: \
      { \
	CmpPredicate f = (*_PREG)->u.plxys.p->cs.d_code; \
	saveregs(); \
	d0 = (CELL) (f) (d0,d1); \
	setregs(); \
      } \
      if (!d0 || (*_PREG) == FAILCODE) { \
	if ((*_PREG) != FAILCODE) { \
	  (*_PREG) = (*_PREG)->u.plxys.f; \
	} \
	BACK(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), plxys); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, call_bfunc_yx_unk, call_bfunc_yx_nvar); \
      d1 = Deref(d1); \
      goto exec_bin_cmp_yx; \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, call_bfunc_yx2_unk, call_bfunc_yx2_nvar); \
      goto exec_bin_cmp_yx; \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);
      
#define _call_bfunc_xy_instinit \
      BEGD(d0); \
      BEGD(d1); \
      BEGP(pt0); \
      pt0 = YREG + (*_PREG)->u.plxys.y; \
      d0 = XREG((*_PREG)->u.plxys.x); \
      d1 = *pt0; \
      ENDP(pt0); \
      deref_head(d0, call_bfunc_xy_unk); \
    call_bfunc_xy_nvar: \
      deref_head(d1, call_bfunc_xy2_unk); \
    call_bfunc_xy2_nvar: \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	int flags; \
	Int v = IntOfTerm(d0) - IntOfTerm(d1); \
	flags = (*_PREG)->u.plxys.flags; \
	if (v > 0) { \
	  if (flags & GT_OK_IN_CMP) { \
	    (*_PREG) = NEXTOP((*_PREG), plxys); \
	    GONEXT(); \
	  } else { \
	    (*_PREG) = (*_PREG)->u.plxys.f; \
	    BACK(); \
	  } \
	} else if (v < 0) { \
	  if (flags & LT_OK_IN_CMP) { \
	    (*_PREG) = NEXTOP((*_PREG), plxys); \
	    GONEXT(); \
	  } else { \
	    (*_PREG) = (*_PREG)->u.plxys.f; \
	    BACK(); \
	  } \
	} else { \
	  if (flags & EQ_OK_IN_CMP) { \
	    (*_PREG) = NEXTOP((*_PREG), plxys); \
	    GONEXT(); \
	  } else { \
	    (*_PREG) = (*_PREG)->u.plxys.f; \
	    BACK(); \
	  } \
	} \
      } \
    exec_bin_cmp_xy: \
      { \
	CmpPredicate f = (*_PREG)->u.plxys.p->cs.d_code; \
	saveregs(); \
	d0 = (CELL) (f) (d0,d1); \
	setregs(); \
      } \
      if (!d0 || (*_PREG) == FAILCODE) { \
	if ((*_PREG) != FAILCODE) { \
	  (*_PREG) = (*_PREG)->u.plxys.f; \
	} \
	BACK(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), plxys); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, call_bfunc_xy_unk, call_bfunc_xy_nvar); \
      d1 = Deref(d1); \
      goto exec_bin_cmp_xy; \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, call_bfunc_xy2_unk, call_bfunc_xy2_nvar); \
      goto exec_bin_cmp_xy; \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);
      
#define _call_bfunc_yy_instinit \
      BEGD(d0); \
      BEGD(d1); \
      BEGP(pt0); \
      pt0 = YREG + (*_PREG)->u.plyys.y1; \
      BEGP(pt1); \
      pt1 = YREG + (*_PREG)->u.plyys.y2; \
      d0 = *pt0; \
      d1 = *pt1; \
      ENDP(pt1); \
      ENDP(pt0); \
      deref_head(d0, call_bfunc_yy_unk); \
    call_bfunc_yy_nvar: \
      deref_head(d1, call_bfunc_yy2_unk); \
    call_bfunc_yy2_nvar: \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	int flags; \
	Int v = IntOfTerm(d0) - IntOfTerm(d1); \
	flags = (*_PREG)->u.plyys.flags; \
	if (v > 0) { \
	  if (flags & GT_OK_IN_CMP) { \
	    (*_PREG) = NEXTOP((*_PREG), plyys); \
	    GONEXT(); \
	  } else { \
	    (*_PREG) = (*_PREG)->u.plyys.f; \
	    BACK(); \
	  } \
	} else if (v < 0) { \
	  if (flags & LT_OK_IN_CMP) { \
	    (*_PREG) = NEXTOP((*_PREG), plyys); \
	    GONEXT(); \
	  } else { \
	    (*_PREG) = (*_PREG)->u.plyys.f; \
	    BACK(); \
	  } \
	} else { \
	  if (flags & EQ_OK_IN_CMP) { \
	    (*_PREG) = NEXTOP((*_PREG), plyys); \
	    GONEXT(); \
	  } else { \
	    (*_PREG) = (*_PREG)->u.plyys.f; \
	    BACK(); \
	  } \
	} \
      } \
    exec_bin_cmp_yy: \
      { \
	CmpPredicate f = (*_PREG)->u.plyys.p->cs.d_code; \
	saveregs(); \
	d0 = (CELL) (f) (d0,d1); \
	setregs(); \
      } \
      if (!d0 || (*_PREG) == FAILCODE) { \
	if ((*_PREG) != FAILCODE) { \
	  (*_PREG) = (*_PREG)->u.plyys.f; \
	} \
	BACK(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), plyys); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, call_bfunc_yy_unk, call_bfunc_yy_nvar); \
      d1 = Deref(d1); \
      goto exec_bin_cmp_yy; \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, call_bfunc_yy2_unk, call_bfunc_yy2_nvar); \
      goto exec_bin_cmp_yy; \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);
      
#ifdef LOW_LEVEL_TRACER
#if defined(YAPOR_SBA) && defined(YAPOR)
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailVal(--TR); \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailTerm(--TR); \
	    TR--; \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#else /* COROUTINING */
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailVal(--TR); \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailTerm(--TR); \
	    TR--; \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#endif /* COROUTINING */
#else /* defined(YAPOR_SBA) && defined(YAPOR) */
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailVal(--TR); \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailTerm(--TR); \
	    TR--; \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#else /* COROUTINING */
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailVal(--TR); \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailTerm(--TR); \
	    TR--; \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define _p_dif_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1); \
	} \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#endif /* COROUTINING */
#endif /* defined(YAPOR_SBA) && defined(YAPOR) */
#else /* LOW_LEVEL_TRACER */
#if defined(YAPOR_SBA) && defined(YAPOR)
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailVal(--TR); \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailTerm(--TR); \
	    TR--; \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#else /* COROUTINING */
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailVal(--TR); \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailTerm(--TR); \
	    TR--; \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
	    if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	      RESET_VARIABLE(STACK_TO_SBA(d1)); \
	    } else \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#endif /* COROUTINING */
#else /* defined(YAPOR_SBA) && defined(YAPOR) */
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailVal(--TR); \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailTerm(--TR); \
	    TR--; \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	if (OldWokenGoals == TermNil) { \
	  Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	} \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	H = HRBREG; \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#else /* COROUTINING */
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailVal(--TR); \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } else { \
	    CELL *pt = RepAppl(d1); \
	    pt[0] = TrailTerm(--TR); \
	    TR--; \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* MULTI_ASSIGNMENT_VARIABLES */
#define _p_dif_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
      deref_head(d0, dif_unk1); \
    dif_nvar1: \
      d1 = ARG2; \
      deref_head(d1, dif_nvar1_unk2); \
    dif_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      { \
	Int opresult; \
	register tr_fr_ptr pt0; \
	pt0 = TR; \
	BEGCHO(pt1); \
	pt1 = B; \
	HBREG = HR; \
	B = (choiceptr) HR; \
	B->cp_h = HR; \
	SET_BB(B); \
	save_hb(); \
	opresult = Yap_IUnify(d0, d1); \
	B = pt1; \
	SET_BB(PROTECT_FROZEN_B(pt1)); \
	HBREG = B->cp_h; \
	while (TR != pt0) { \
	  BEGD(d1); \
	  d1 = TrailTerm(--TR); \
	  if (IsVarTerm(d1)) { \
        { \
	      RESET_VARIABLE(d1); \
		} \
	  } \
	  ENDD(d1); \
	} \
	if (opresult) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	(*_PREG) = NEXTOP((*_PREG), l); \
	ENDCHO(pt1); \
      } \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, dif_unk1, dif_nvar1); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2); \
      ENDP(pt0); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#endif /* COROUTINING */
#endif /* defined(YAPOR_SBA) && defined(YAPOR) */
#endif /* LOW_LEVEL_TRACER */
      
#ifdef LOW_LEVEL_TRACER
#ifdef USE_GMP
#define _p_eq_instinit \
    register CELL d0, d1, d2; \
    register CELL *pt0, *pt1; \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorSame,0)),XREGS+1); \
    } \
      d0 = ARG1; \
      deref_head(d0, p_eq_unk1); \
    p_eq_nvar1: \
      d1 = ARG2; \
      deref_head(d1, p_eq_nvar1_unk2); \
    p_eq_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      if (IsPairTerm(d0)) { \
	if (!IsPairTerm(d1)) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_save_pc(); \
	d2 = iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1); \
	if (d2 == FALSE) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_set_pc(); \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      if (IsApplTerm(d0)) { \
	Functor f0 = FunctorOfTerm(d0); \
	Functor f1; \
 \
	if (!IsApplTerm(d1)) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	f1 = FunctorOfTerm(d1); \
 \
	if (IsExtensionFunctor(f0)) { \
	  switch ((CELL)f0) { \
	  case (CELL)FunctorDBRef: \
	    if (d0 == d1) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorLongInt: \
	    if (f1 != FunctorLongInt) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (LongIntOfTerm(d0) == LongIntOfTerm(d1)) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorBigInt: \
	    if (f1 != FunctorBigInt) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (Yap_gmp_tcmp_big_big(d0,d1) == 0) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorDouble: \
	    if (f1 != FunctorDouble) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (FloatOfTerm(d0) == FloatOfTerm(d1)) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	  default: \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  } \
	} \
	if (f0 != f1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_save_pc(); \
	d2 = iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1)); \
	if (d2 == FALSE) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_set_pc(); \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1); \
      d1 = ARG2; \
      deref_head(d1, p_eq_var1_unk2); \
    p_eq_var1_nvar2: \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2); \
      if (pt1 != pt0) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), l); \
      GONEXT();
#else /* USE_GMP */
#define _p_eq_instinit \
    register CELL d0, d1, d2; \
    register CELL *pt0, *pt1; \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorSame,0)),XREGS+1); \
    } \
      d0 = ARG1; \
      deref_head(d0, p_eq_unk1); \
    p_eq_nvar1: \
      d1 = ARG2; \
      deref_head(d1, p_eq_nvar1_unk2); \
    p_eq_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      if (IsPairTerm(d0)) { \
	if (!IsPairTerm(d1)) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_save_pc(); \
	d2 = iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1); \
	if (d2 == FALSE) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_set_pc(); \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      if (IsApplTerm(d0)) { \
	Functor f0 = FunctorOfTerm(d0); \
	Functor f1; \
 \
	if (!IsApplTerm(d1)) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	f1 = FunctorOfTerm(d1); \
 \
	if (IsExtensionFunctor(f0)) { \
	  switch ((CELL)f0) { \
	  case (CELL)FunctorDBRef: \
	    if (d0 == d1) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorLongInt: \
	    if (f1 != FunctorLongInt) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (LongIntOfTerm(d0) == LongIntOfTerm(d1)) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorDouble: \
	    if (f1 != FunctorDouble) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (FloatOfTerm(d0) == FloatOfTerm(d1)) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	  default: \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  } \
	} \
	if (f0 != f1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_save_pc(); \
	d2 = iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1)); \
	if (d2 == FALSE) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_set_pc(); \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1); \
      d1 = ARG2; \
      deref_head(d1, p_eq_var1_unk2); \
    p_eq_var1_nvar2: \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2); \
      if (pt1 != pt0) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), l); \
      GONEXT();
#endif /* USE_GMP */
#else /* LOW_LEVEL_TRACER */
#ifdef USE_GMP
#define _p_eq_instinit \
    register CELL d0, d1, d2; \
    register CELL *pt0, *pt1; \
      d0 = ARG1; \
      deref_head(d0, p_eq_unk1); \
    p_eq_nvar1: \
      d1 = ARG2; \
      deref_head(d1, p_eq_nvar1_unk2); \
    p_eq_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      if (IsPairTerm(d0)) { \
	if (!IsPairTerm(d1)) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_save_pc(); \
	d2 = iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1); \
	if (d2 == FALSE) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_set_pc(); \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      if (IsApplTerm(d0)) { \
	Functor f0 = FunctorOfTerm(d0); \
	Functor f1; \
 \
	if (!IsApplTerm(d1)) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	f1 = FunctorOfTerm(d1); \
 \
	if (IsExtensionFunctor(f0)) { \
	  switch ((CELL)f0) { \
	  case (CELL)FunctorDBRef: \
	    if (d0 == d1) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorLongInt: \
	    if (f1 != FunctorLongInt) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (LongIntOfTerm(d0) == LongIntOfTerm(d1)) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorBigInt: \
	    if (f1 != FunctorBigInt) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (Yap_gmp_tcmp_big_big(d0,d1) == 0) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorDouble: \
	    if (f1 != FunctorDouble) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (FloatOfTerm(d0) == FloatOfTerm(d1)) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	  default: \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  } \
	} \
	if (f0 != f1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_save_pc(); \
	d2 = iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1)); \
	if (d2 == FALSE) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_set_pc(); \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1); \
      d1 = ARG2; \
      deref_head(d1, p_eq_var1_unk2); \
    p_eq_var1_nvar2: \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2); \
      if (pt1 != pt0) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), l); \
      GONEXT();
#else /* USE_GMP */
#define _p_eq_instinit \
    register CELL d0, d1, d2; \
    register CELL *pt0, *pt1; \
      d0 = ARG1; \
      deref_head(d0, p_eq_unk1); \
    p_eq_nvar1: \
      d1 = ARG2; \
      deref_head(d1, p_eq_nvar1_unk2); \
    p_eq_nvar1_nvar2: \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      if (IsPairTerm(d0)) { \
	if (!IsPairTerm(d1)) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_save_pc(); \
	d2 = iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1); \
	if (d2 == FALSE) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_set_pc(); \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      if (IsApplTerm(d0)) { \
	Functor f0 = FunctorOfTerm(d0); \
	Functor f1; \
 \
	if (!IsApplTerm(d1)) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	f1 = FunctorOfTerm(d1); \
 \
	if (IsExtensionFunctor(f0)) { \
	  switch ((CELL)f0) { \
	  case (CELL)FunctorDBRef: \
	    if (d0 == d1) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorLongInt: \
	    if (f1 != FunctorLongInt) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (LongIntOfTerm(d0) == LongIntOfTerm(d1)) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  case (CELL)FunctorDouble: \
	    if (f1 != FunctorDouble) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      BACK(); \
	    } \
	    if (FloatOfTerm(d0) == FloatOfTerm(d1)) { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
	      GONEXT(); \
	    } \
	  default: \
	    (*_PREG) = (*_PREG)->u.l.l; \
	    BACK(); \
	  } \
	} \
	if (f0 != f1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_save_pc(); \
	d2 = iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1)); \
	if (d2 == FALSE) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  BACK(); \
	} \
	always_set_pc(); \
	(*_PREG) = NEXTOP((*_PREG), l); \
	GONEXT(); \
      } \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2); \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1); \
      d1 = ARG2; \
      deref_head(d1, p_eq_var1_unk2); \
    p_eq_var1_nvar2: \
      (*_PREG) = (*_PREG)->u.l.l; \
      BACK(); \
 \
      deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2); \
      if (pt1 != pt0) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	BACK(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), l); \
      GONEXT();
#endif /* USE_GMP */
#endif /* LOW_LEVEL_TRACER */

#ifdef LOW_LEVEL_TRACER
#define _p_arg_vv_instinit \
      if (Yap_do_low_level_trace) { \
	H[0] = XREG((*_PREG)->u.xxx.x1); \
	H[1] = XREG((*_PREG)->u.xxx.x2); \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),H); \
      } \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xxx.x1); \
      deref_head(d0, arg_arg1_unk); \
    arg_arg1_nvar: \
      if (IsIntTerm(d0)) { \
	d0 = IntOfTerm(d0); \
      } else if (IsLongIntTerm(d0)) { \
	d0 = LongIntOfTerm(d0); \
      } else { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3"); \
	setregs(); \
	FAIL(); \
      } \
 \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.xxx.x2); \
      deref_head(d1, arg_arg2_unk); \
    arg_arg2_nvar: \
      if (IsApplTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepAppl(d1); \
	d1 = *pt0; \
	if (IsExtensionFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	if ((Int)d0 <= 0 || \
	    (Int)d0 > ArityOfFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	XREG((*_PREG)->u.xxx.x) = pt0[d0]; \
	(*_PREG) = NEXTOP((*_PREG), xxx); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else if (IsPairTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepPair(d1); \
	if (d0 != 1 && d0 != 2) { \
	  if ((Int)d0 < 0) { \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs(); \
	  } \
	  FAIL(); \
	} \
	XREG((*_PREG)->u.xxx.x) = pt0[d0-1]; \
	(*_PREG) = NEXTOP((*_PREG), xxx); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else { \
	FAIL(); \
      } \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, arg_arg2_unk, arg_arg2_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d1); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, arg_arg1_unk, arg_arg1_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d0);
#else /* LOW_LEVEL_TRACER */
#define _p_arg_vv_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xxx.x1); \
      deref_head(d0, arg_arg1_unk); \
    arg_arg1_nvar: \
      if (IsIntTerm(d0)) { \
	d0 = IntOfTerm(d0); \
      } else if (IsLongIntTerm(d0)) { \
	d0 = LongIntOfTerm(d0); \
      } else { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3"); \
	setregs(); \
	FAIL(); \
      } \
 \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.xxx.x2); \
      deref_head(d1, arg_arg2_unk); \
    arg_arg2_nvar: \
      if (IsApplTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepAppl(d1); \
	d1 = *pt0; \
	if (IsExtensionFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	if ((Int)d0 <= 0 || \
	    (Int)d0 > ArityOfFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	XREG((*_PREG)->u.xxx.x) = pt0[d0]; \
	(*_PREG) = NEXTOP((*_PREG), xxx); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else if (IsPairTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepPair(d1); \
	if (d0 != 1 && d0 != 2) { \
	  if ((Int)d0 < 0) { \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs(); \
	  } \
	  FAIL(); \
	} \
	XREG((*_PREG)->u.xxx.x) = pt0[d0-1]; \
	(*_PREG) = NEXTOP((*_PREG), xxx); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else { \
	FAIL(); \
      } \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, arg_arg2_unk, arg_arg2_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d1); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, arg_arg1_unk, arg_arg1_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d0);
#endif /* LOW_LEVEL_TRACER */

#ifdef LOW_LEVEL_TRACER
#define _p_arg_cv_instinit \
      if (Yap_do_low_level_trace) { \
	CELL *Ho = HR; \
	Term t = MkIntegerTerm((*_PREG)->u.xxn.c); \
	H[0] =  t; \
	H[1] = XREG((*_PREG)->u.xxn.xi); \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),H); \
	H = HRo; \
      } \
      BEGD(d0); \
      d0 = (*_PREG)->u.xxn.c; \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.xxn.xi); \
      deref_head(d1, arg_arg2_vc_unk); \
    arg_arg2_vc_nvar: \
      if (IsApplTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepAppl(d1); \
	d1 = *pt0; \
	if (IsExtensionFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	if ((Int)d0 <= 0 || \
	    (Int)d0 > ArityOfFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	XREG((*_PREG)->u.xxn.x) = pt0[d0]; \
	(*_PREG) = NEXTOP((*_PREG), xxn); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else if (IsPairTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepPair(d1); \
	if (d0 != 1 && d0 != 2) { \
	  if ((Int)d0 < 0) { \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs(); \
	  } \
	  FAIL(); \
	} \
	XREG((*_PREG)->u.xxn.x) = pt0[d0-1]; \
	(*_PREG) = NEXTOP((*_PREG), xxn); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else { \
	FAIL(); \
      } \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, arg_arg2_vc_unk, arg_arg2_vc_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d1); \
      ENDD(d0);
#else /* LOW_LEVEL_TRACER */
#define _p_arg_cv_instinit \
      BEGD(d0); \
      d0 = (*_PREG)->u.xxn.c; \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.xxn.xi); \
      deref_head(d1, arg_arg2_vc_unk); \
    arg_arg2_vc_nvar: \
      if (IsApplTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepAppl(d1); \
	d1 = *pt0; \
	if (IsExtensionFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	if ((Int)d0 <= 0 || \
	    (Int)d0 > ArityOfFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	XREG((*_PREG)->u.xxn.x) = pt0[d0]; \
	(*_PREG) = NEXTOP((*_PREG), xxn); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else if (IsPairTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepPair(d1); \
	if (d0 != 1 && d0 != 2) { \
	  if ((Int)d0 < 0) { \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs(); \
	  } \
	  FAIL(); \
	} \
	XREG((*_PREG)->u.xxn.x) = pt0[d0-1]; \
	(*_PREG) = NEXTOP((*_PREG), xxn); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else { \
	FAIL(); \
      } \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, arg_arg2_vc_unk, arg_arg2_vc_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d1); \
      ENDD(d0);
#endif /* LOW_LEVEL_TRACER */

#ifdef LOW_LEVEL_TRACER
#define _p_arg_y_vv_instinit \
      if (Yap_do_low_level_trace) { \
	H[0] = XREG((*_PREG)->u.yxx.x1); \
	H[1] = XREG((*_PREG)->u.yxx.x2); \
	H[2] = YREG[(*_PREG)->u.yxx.y]; \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),H); \
      } \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.yxx.x1); \
      deref_head(d0, arg_y_arg1_unk); \
    arg_y_arg1_nvar: \
      if (IsIntTerm(d0)) { \
	d0 = IntOfTerm(d0); \
      } else if (IsLongIntTerm(d0)) { \
	d0 = LongIntOfTerm(d0); \
      } else { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3"); \
	setregs(); \
	FAIL(); \
      } \
 \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.yxx.x2); \
      deref_head(d1, arg_y_arg2_unk); \
    arg_y_arg2_nvar: \
      if (IsApplTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepAppl(d1); \
	d1 = *pt0; \
	if (IsExtensionFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	if ((Int)d0 <= 0 || \
	    (Int)d0 > ArityOfFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	BEGP(pt1); \
	pt1 = YREG + (*_PREG)->u.yxx.y; \
	(*_PREG) = NEXTOP((*_PREG), yxx); \
	INITIALIZE_PERMVAR(pt1,pt0[d0]); \
	ENDP(pt1); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else if (IsPairTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepPair(d1); \
	if (d0 != 1 && d0 != 2) { \
	  if ((Int)d0 < 0) { \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs(); \
	  } \
	  FAIL(); \
	} \
	BEGP(pt1); \
	pt1 = YREG + (*_PREG)->u.yxx.y; \
	(*_PREG) = NEXTOP((*_PREG), yxx); \
	INITIALIZE_PERMVAR(pt1,pt0[d0-1]); \
	GONEXT(); \
	ENDP(pt1); \
	ENDP(pt0); \
      } \
      else { \
	FAIL(); \
      } \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, arg_y_arg2_unk, arg_y_arg2_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d1); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, arg_y_arg1_unk, arg_y_arg1_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d0);
#else /* LOW_LEVEL_TRACER */
#define _p_arg_y_vv_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.yxx.x1); \
      deref_head(d0, arg_y_arg1_unk); \
    arg_y_arg1_nvar: \
      if (IsIntTerm(d0)) { \
	d0 = IntOfTerm(d0); \
      } else if (IsLongIntTerm(d0)) { \
	d0 = LongIntOfTerm(d0); \
      } else { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3"); \
	setregs(); \
	FAIL(); \
      } \
 \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.yxx.x2); \
      deref_head(d1, arg_y_arg2_unk); \
    arg_y_arg2_nvar: \
      if (IsApplTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepAppl(d1); \
	d1 = *pt0; \
	if (IsExtensionFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	if ((Int)d0 <= 0 || \
	    (Int)d0 > ArityOfFunctor((Functor) d1)) { \
	  FAIL(); \
	} \
	BEGP(pt1); \
	pt1 = YREG + (*_PREG)->u.yxx.y; \
	(*_PREG) = NEXTOP((*_PREG), yxx); \
	INITIALIZE_PERMVAR(pt1,pt0[d0]); \
	ENDP(pt1); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      else if (IsPairTerm(d1)) { \
	BEGP(pt0); \
	pt0 = RepPair(d1); \
	if (d0 != 1 && d0 != 2) { \
	  if ((Int)d0 < 0) { \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs(); \
	  } \
	  FAIL(); \
	} \
	BEGP(pt1); \
	pt1 = YREG + (*_PREG)->u.yxx.y; \
	(*_PREG) = NEXTOP((*_PREG), yxx); \
	INITIALIZE_PERMVAR(pt1,pt0[d0-1]); \
	GONEXT(); \
	ENDP(pt1); \
	ENDP(pt0); \
      } \
      else { \
	FAIL(); \
      } \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, arg_y_arg2_unk, arg_y_arg2_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d1); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, arg_y_arg1_unk, arg_y_arg1_nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3"); \
      setregs(); \
      ENDP(pt0); \
      FAIL(); \
      ENDD(d0);
#endif /* LOW_LEVEL_TRACER */


#ifdef LOW_LEVEL_TRACER
#define _p_functor_instinit \
      if (Yap_do_low_level_trace) \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),XREGS+1); \
      restart_functor: \
      BEGD(d0); \
      d0 = ARG1; \
      deref_head(d0, func_unk); \
    func_nvar: \
      BEGD(d1); \
      if (IsApplTerm(d0)) { \
	d1 = *RepAppl(d0); \
	if (IsExtensionFunctor((Functor) d1)) { \
	  if (d1 <= (CELL)FunctorDouble && d1 >= (CELL)FunctorLongInt ) { \
	    d1 = MkIntTerm(0); \
	  } else \
	    FAIL(); \
	} else { \
	    d0 = MkAtomTerm(NameOfFunctor((Functor) d1)); \
	    d1 = MkIntTerm(ArityOfFunctor((Functor) d1)); \
	} \
      } \
      else if (IsPairTerm(d0)) { \
	d0 = TermDot; \
	d1 = MkIntTerm(2); \
      } \
      else { \
	d1 = MkIntTerm(0); \
      } \
      { \
	register CELL arity = d1; \
 \
	d1 = ARG2; \
	deref_head(d1, func_nvar_unk); \
      func_nvar_nvar: \
	if (d0 != d1) { \
	  FAIL(); \
	} \
	d0 = arity; \
	goto func_bind_x3; \
 \
	BEGP(pt0); \
	deref_body(d1, pt0, func_nvar_unk, func_nvar_nvar); \
	Bind(pt0, d0); \
	d0 = arity; \
	ENDP(pt0); \
      func_bind_x3: \
	d1 = ARG3; \
	deref_head(d1, func_nvar3_unk); \
      func_nvar3_nvar: \
	if (d0 != d1) { \
	  FAIL(); \
	} \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	GONEXT(); \
 \
	BEGP(pt0); \
	deref_body(d1, pt0, func_nvar3_unk, func_nvar3_nvar); \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	Bind(pt0, d0); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      ENDD(d1); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, func_unk, func_nvar); \
      d0 = ARG2; \
      deref_head(d0, func_var_2unk); \
    func_var_2nvar: \
      BEGD(d1); \
      d1 = ARG3; \
      deref_head(d1, func_var_3unk); \
    func_var_3nvar: \
      if (IsIntTerm(d1)) \
	d1 = IntOfTerm(d1); \
      else { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	setregs(); \
	FAIL(); \
      } \
      if (!IsAtomicTerm(d0)) { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	setregs(); \
	FAIL(); \
      } \
      if (d0 == TermDot && d1 == 2) { \
	RESET_VARIABLE(HR); \
	RESET_VARIABLE(HR+1); \
	d0 = AbsPair(HR); \
	H += 2; \
      } \
      else if ((Int)d1 > 0) { \
	if (!IsAtomTerm(d0)) { \
	  saveregs(); \
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	  setregs(); \
	  FAIL(); \
	} \
	BEGP(pt1); \
	if (!IsAtomTerm(d0)) { \
	  FAIL(); \
	} \
	else \
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
	pt1 = HR; \
	*pt1++ = d0; \
	d0 = AbsAppl(HR); \
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	  saveregs(); \
	  if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	    setregs(); \
	    BACK(); \
	  } else { \
	    setregs(); \
	  } \
	  goto restart_functor; \
	} \
	while ((Int)d1--) { \
	  RESET_VARIABLE(pt1); \
	  pt1++; \
	} \
	H = pt1; \
	ENDP(pt1); \
      }	else if ((Int)d1  < 0) { \
	saveregs(); \
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	setregs(); \
	FAIL(); \
      } \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
      Bind(pt0, d0); \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, func_var_3unk, func_var_3nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
      setregs(); \
      ENDP(pt1); \
      FAIL(); \
      ENDD(d1); \
 \
      BEGP(pt1); \
      deref_body(d0, pt1, func_var_2unk, func_var_2nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      ENDP(pt1); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d0);
#else /* LOW_LEVEL_TRACER */
#define _p_functor_instinit \
      restart_functor: \
      BEGD(d0); \
      d0 = ARG1; \
      deref_head(d0, func_unk); \
    func_nvar: \
      BEGD(d1); \
      if (IsApplTerm(d0)) { \
	d1 = *RepAppl(d0); \
	if (IsExtensionFunctor((Functor) d1)) { \
	  if (d1 <= (CELL)FunctorDouble && d1 >= (CELL)FunctorLongInt ) { \
	    d1 = MkIntTerm(0); \
	  } else \
	    FAIL(); \
	} else { \
	    d0 = MkAtomTerm(NameOfFunctor((Functor) d1)); \
	    d1 = MkIntTerm(ArityOfFunctor((Functor) d1)); \
	} \
      } \
      else if (IsPairTerm(d0)) { \
	d0 = TermDot; \
	d1 = MkIntTerm(2); \
      } \
      else { \
	d1 = MkIntTerm(0); \
      } \
      { \
	register CELL arity = d1; \
 \
	d1 = ARG2; \
	deref_head(d1, func_nvar_unk); \
      func_nvar_nvar: \
	if (d0 != d1) { \
	  FAIL(); \
	} \
	d0 = arity; \
	goto func_bind_x3; \
 \
	BEGP(pt0); \
	deref_body(d1, pt0, func_nvar_unk, func_nvar_nvar); \
	Bind(pt0, d0); \
	d0 = arity; \
	ENDP(pt0); \
      func_bind_x3: \
	d1 = ARG3; \
	deref_head(d1, func_nvar3_unk); \
      func_nvar3_nvar: \
	if (d0 != d1) { \
	  FAIL(); \
	} \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	GONEXT(); \
 \
	BEGP(pt0); \
	deref_body(d1, pt0, func_nvar3_unk, func_nvar3_nvar); \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	Bind(pt0, d0); \
	GONEXT(); \
	ENDP(pt0); \
      } \
      ENDD(d1); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, func_unk, func_nvar); \
      d0 = ARG2; \
      deref_head(d0, func_var_2unk); \
    func_var_2nvar: \
      BEGD(d1); \
      d1 = ARG3; \
      deref_head(d1, func_var_3unk); \
    func_var_3nvar: \
      if (IsIntTerm(d1)) \
	d1 = IntOfTerm(d1); \
      else { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	setregs(); \
	FAIL(); \
      } \
      if (!IsAtomicTerm(d0)) { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	setregs(); \
	FAIL(); \
      } \
      if (d0 == TermDot && d1 == 2) { \
	RESET_VARIABLE(HR); \
	RESET_VARIABLE(HR+1); \
	d0 = AbsPair(HR); \
	H += 2; \
      } \
      else if ((Int)d1 > 0) { \
	if (!IsAtomTerm(d0)) { \
	  saveregs(); \
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	  setregs(); \
	  FAIL(); \
	} \
	BEGP(pt1); \
	if (!IsAtomTerm(d0)) { \
	  FAIL(); \
	} \
	else \
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
	pt1 = HR; \
	*pt1++ = d0; \
	d0 = AbsAppl(HR); \
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	  saveregs(); \
	  if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	    setregs(); \
	    BACK(); \
	  } else { \
	    setregs(); \
	  } \
	  goto restart_functor; \
	} \
	while ((Int)d1--) { \
	  RESET_VARIABLE(pt1); \
	  pt1++; \
	} \
	H = pt1; \
	ENDP(pt1); \
      }	else if ((Int)d1  < 0) { \
	saveregs(); \
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	setregs(); \
	FAIL(); \
      } \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
      Bind(pt0, d0); \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, func_var_3unk, func_var_3nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
      setregs(); \
      ENDP(pt1); \
      FAIL(); \
      ENDD(d1); \
 \
      BEGP(pt1); \
      deref_body(d0, pt1, func_var_2unk, func_var_2nvar); \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      ENDP(pt1); \
      FAIL(); \
      ENDP(pt0); \
      ENDD(d0);
#endif /* LOW_LEVEL_TRACER */
