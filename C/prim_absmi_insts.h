/************************************************************************ \
 *    Basic Primitive Predicates                                       *
\************************************************************************/

#ifdef INDENT_CODE
{
  {
    {
#endif /* INDENT_CODE */

      Op(p_plus_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, plus_vv_unk);
    plus_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, plus_vv_nvar_unk);
    plus_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1));
      } else {
        saveregs();
        d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, plus_vv_unk, plus_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, plus_vv_nvar_unk, plus_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_plus_vc, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, plus_vc_unk);
    plus_vc_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) + d1);
      } else {
        saveregs();
        d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, plus_vc_unk, plus_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_plus_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, plus_y_vv_unk);
    plus_y_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, plus_y_vv_nvar_unk);
    plus_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1));
      } else {
        saveregs();
        d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, plus_y_vv_unk, plus_y_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, plus_y_vv_nvar_unk, plus_y_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_plus_y_vc, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, plus_y_vc_unk);
    plus_y_vc_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) + d1);
      } else {
        saveregs();
        d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, plus_y_vc_unk, plus_y_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_minus_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, minus_vv_unk);
    minus_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, minus_vv_nvar_unk);
    minus_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1));
      } else {
        saveregs();
        d0 = p_minus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_vv_unk, minus_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, minus_vv_nvar_unk, minus_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d0);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_minus_cv, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, minus_cv_unk);
    minus_cv_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntegerTerm(d1 - IntOfTerm(d0));
      } else {
        saveregs();
        d0 = p_minus(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
        setregs();
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_cv_unk, minus_cv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_minus_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, minus_y_vv_unk);
    minus_y_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, minus_y_vv_nvar_unk);
    minus_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1));
      } else {
        saveregs();
        d0 = p_minus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_y_vv_unk, minus_y_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, minus_y_vv_nvar_unk, minus_y_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_minus_y_cv, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, minus_y_cv_unk);
    minus_y_cv_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntegerTerm(d1 - IntOfTerm(d0));
      } else {
        saveregs();
        d0 = p_minus(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_y_cv_unk, minus_y_cv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_times_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, times_vv_unk);
    times_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, times_vv_nvar_unk);
    times_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = times_int(IntOfTerm(d0), IntOfTerm(d1) PASS_REGS);
      } else {
        saveregs();
        d0 = p_times(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, times_vv_unk, times_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, times_vv_nvar_unk, times_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_times_vc, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, times_vc_unk);
    times_vc_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        d0 = times_int(IntOfTerm(d0), d1 PASS_REGS);
      } else {
        saveregs();
        d0 = p_times(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, times_vc_unk, times_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_times_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, times_y_vv_unk);
    times_y_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, times_y_vv_nvar_unk);
    times_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = times_int(IntOfTerm(d0), IntOfTerm(d1) PASS_REGS);
      } else {
        saveregs();
        d0 = p_times(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, times_y_vv_unk, times_y_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, times_y_vv_nvar_unk, times_y_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_times_y_vc, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, times_y_vc_unk);
    times_y_vc_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        d0 = times_int(IntOfTerm(d0), d1 PASS_REGS);
      } else {
        saveregs();
        d0 = p_times(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, times_y_vc_unk, times_y_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, div_vv_unk);
    div_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, div_vv_nvar_unk);
    div_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        Int div = IntOfTerm(d1);
        if (div == 0) {
          Yap_AsmError(EVALUATION_ERROR_ZERO_DIVISOR,d1);
        }
        d0 = MkIntTerm(IntOfTerm(d0) / div);
      } else {
        saveregs();
        d0 = p_div(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_vv_unk, div_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, div_vv_nvar_unk, div_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_div_vc, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, div_vc_unk);
    div_vc_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntTerm(IntOfTerm(d0) / d1);
      } else {
        saveregs();
        d0 = p_div(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_vc_unk, div_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_cv, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, div_cv_unk);
    div_cv_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        Int div = IntOfTerm(d0);
        if (div == 0) {
          Yap_AsmError(EVALUATION_ERROR_ZERO_DIVISOR,d0);
          FAIL();
        }
        d0 = MkIntegerTerm(d1 / div);
      } else {
        saveregs();
        d0 = p_div(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_cv_unk, div_cv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, div_y_vv_unk);
    div_y_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, div_y_vv_nvar_unk);
    div_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        Int div = IntOfTerm(d1);
        if (div == 0) {
          Yap_AsmError(EVALUATION_ERROR_ZERO_DIVISOR, d0);
          FAIL();
        }
        d0 = MkIntTerm(IntOfTerm(d0) / div);
      } else {
        saveregs();
        d0 = p_div(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_y_vv_unk, div_y_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, div_y_vv_nvar_unk, div_y_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_div_y_vc, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, div_y_vc_unk);
    div_y_vc_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntTerm(IntOfTerm(d0) / d1);
      } else {
        saveregs();
        d0 = p_div(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_y_vc_unk, div_y_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_y_cv, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, div_y_cv_unk);
    div_y_cv_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        Int div = IntOfTerm(d0);
        if (div == 0) {
          Yap_AsmError(EVALUATION_ERROR_ZERO_DIVISOR,d0);
          FAIL();
        }
        d0 = MkIntegerTerm(d1 / div);
      } else {
        saveregs();
        d0 = p_div(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_y_cv_unk, div_y_cv_nvar);
      Yap_AbsmiError(INSTANTIATION_ERROR);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_and_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, and_vv_unk);
    and_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, and_vv_nvar_unk);
    and_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1));
      } else {
        saveregs();
        d0 = p_and(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_vv_unk, and_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, and_vv_nvar_unk, and_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_and_vc, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, and_vc_unk);
    and_vc_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) & d1);
      } else {
        saveregs();
        d0 = p_and(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_vc_unk, and_vc_nvar);
      Yap_AbsmiError(INSTANTIATION_ERROR);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_and_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, and_y_vv_unk);
    and_y_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, and_y_vv_nvar_unk);
    and_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1));
      } else {
        saveregs();
        d0 = p_and(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_y_vv_unk, and_y_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, and_y_vv_nvar_unk, and_y_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_and_y_vc, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, and_y_vc_unk);
    and_y_vc_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) & d1);
      } else {
        saveregs();
        d0 = p_and(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_y_vc_unk, and_y_vc_nvar);
      Yap_AbsmiError(INSTANTIATION_ERROR);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_or_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, or_vv_unk);
    or_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, or_vv_nvar_unk);
    or_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));
      } else {
        saveregs();
        d0 = p_or(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_vv_unk, or_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, or_vv_nvar_unk, or_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_or_vc, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, or_vc_unk);
    or_vc_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) | d1);
      } else {
        saveregs();
        d0 = p_or(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_vc_unk, or_vc_nvar);
      Yap_AbsmiError(INSTANTIATION_ERROR);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_or_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, or_y_vv_unk);
    or_y_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, or_y_vv_nvar_unk);
    or_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));
      } else {
        saveregs();
        d0 = p_or(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_y_vv_unk, or_y_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, or_y_vv_nvar_unk, or_y_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_or_y_vc, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, or_y_vc_unk);
    or_y_vc_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntegerTerm(IntOfTerm(d0) | d1);
      } else {
        saveregs();
        d0 = p_or(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_y_vc_unk, or_y_vc_nvar);
      Yap_AbsmiError(INSTANTIATION_ERROR);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, sll_vv_unk);
    sll_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, sll_vv_nvar_unk);
    sll_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        Int i2 = IntOfTerm(d1);
        if (i2 < 0)
          d0 = MkIntegerTerm(SLR(IntOfTerm(d0), -i2));
        else
          d0 = do_sll(IntOfTerm(d0), i2 PASS_REGS);
      } else {
        saveregs();
        d0 = p_sll(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_vv_unk, sll_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, sll_vv_nvar_unk, sll_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_sll_vc, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, sll_vc_unk);
    sll_vc_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        d0 = do_sll(IntOfTerm(d0), (Int)d1 PASS_REGS);
      } else {
        saveregs();
        d0 = p_sll(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_vc_unk, sll_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_cv, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, sll_cv_unk);
    sll_cv_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        Int i2 = IntOfTerm(d0);
        if (i2 < 0)
          d0 = MkIntegerTerm(SLR(d1, -i2));
        else
          d0 = do_sll(d1, i2 PASS_REGS);
      } else {
        saveregs();
        d0 = p_sll(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
        setregs();
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_cv_unk, sll_cv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, sll_y_vv_unk);
    sll_y_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, sll_y_vv_nvar_unk);
    sll_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        Int i2 = IntOfTerm(d1);
        if (i2 < 0)
          d0 = MkIntegerTerm(SLR(IntOfTerm(d0), -i2));
        else
          d0 = do_sll(IntOfTerm(d0), i2 PASS_REGS);
      } else {
        saveregs();
        d0 = p_sll(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_y_vv_unk, sll_y_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, sll_y_vv_nvar_unk, sll_y_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_sll_y_vc, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, sll_y_vc_unk);
    sll_y_vc_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        d0 = do_sll(IntOfTerm(d0), Yap_Eval(d1) PASS_REGS);
      } else {
        saveregs();
        d0 = p_sll(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_y_vc_unk, sll_y_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_y_cv, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, sll_y_cv_unk);
    sll_y_cv_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        Int i2 = IntOfTerm(d0);
        if (i2 < 0)
          d0 = MkIntegerTerm(SLR(d1, -i2));
        else
          d0 = do_sll(d1, i2 PASS_REGS);
      } else {
        saveregs();
        d0 = p_sll(MkIntegerTerm(d1), Yap_Eval(0) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_y_cv_unk, sll_y_cv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, slr_vv_unk);
    slr_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, slr_vv_nvar_unk);
    slr_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        Int i2 = IntOfTerm(d1);
        if (i2 < 0)
          d0 = do_sll(IntOfTerm(d0), -i2 PASS_REGS);
        else
          d0 = MkIntTerm(SLR(IntOfTerm(d0), i2));
      } else {
        saveregs();
        d0 = p_slr(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_vv_unk, slr_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, slr_vv_nvar_unk, slr_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_slr_vc, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, slr_vc_unk);
    slr_vc_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntTerm(SLR(IntOfTerm(d0), d1));
      } else {
        saveregs();
        d0 = p_slr(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_vc_unk, slr_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_cv, xxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, slr_cv_unk);
    slr_cv_nvar : {
      Int d1 = PREG->y_u.xxn.c;
      if (IsIntTerm(d0)) {
        Int i2 = IntOfTerm(d0);
        if (i2 < 0)
          d0 = do_sll(d1, -i2 PASS_REGS);
        else
          d0 = MkIntegerTerm(SLR(d1, i2));
      } else {
        saveregs();
        d0 = p_slr(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
        setregs();
      }
    }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_cv_unk, slr_cv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, slr_y_vv_unk);
    slr_y_vv_nvar:
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, slr_y_vv_nvar_unk);
    slr_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        Int i2 = IntOfTerm(d1);
        if (i2 < 0)
          d0 = do_sll(IntOfTerm(d0), -i2 PASS_REGS);
        else
          d0 = MkIntTerm(SLR(IntOfTerm(d0), i2));
      } else {
        saveregs();
        d0 = p_slr(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
        setregs();
      }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_y_vv_unk, slr_y_vv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, slr_y_vv_nvar_unk, slr_y_vv_nvar_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_slr_y_vc, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, slr_y_vc_unk);
    slr_y_vc_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        d0 = MkIntTerm(SLR(IntOfTerm(d0), d1));
      } else {
        saveregs();
        d0 = p_slr(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
        setregs();
      }
    }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_y_vc_unk, slr_y_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_y_cv, yxn);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, slr_y_cv_unk);
    slr_y_cv_nvar : {
      Int d1 = PREG->y_u.yxn.c;
      if (IsIntTerm(d0)) {
        Int i2 = IntOfTerm(d0);
        if (i2 < 0)
          d0 = do_sll(d1, -i2 PASS_REGS);
        else
          d0 = MkIntegerTerm(SLR(d1, i2));
      } else {
        saveregs();
        d0 = p_slr(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
        setregs();
      }
    }
      if (d0 == 0L) {
        Yap_AsmError(LOCAL_Error_TYPE,d0);
        FAIL();
      }
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0, d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_y_cv_unk, slr_y_cv_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      BOp(call_bfunc_xx, plxxs);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.plxxs.x1);
    call_bfunc_xx_nvar:
      d1 = XREG(PREG->y_u.plxxs.x2);
    call_bfunc_xx2_nvar:
      deref_head(d0, call_bfunc_xx_unk);
      deref_head(d1, call_bfunc_xx2_unk);
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        COUNT flags;

        Int v = IntOfTerm(d0) - IntOfTerm(d1);
        flags = PREG->y_u.plxxs.flags;
        if (v > 0) {
          if (flags & GT_OK_IN_CMP) {
            yamop *nextp = NEXTOP(PREG, plxxs);
            ALWAYS_LOOKAHEAD(nextp->opc);
            PREG = nextp;
            ALWAYS_GONext();
            ALWAYS_END_PREFETCH();
          } else {
            yamop *nextp = PREG->y_u.plxxs.f;
            ALWAYS_LOOKAHEAD(nextp->opc);
            PREG = nextp;
            ALWAYS_GONext();
            ALWAYS_END_PREFETCH();
          }
        } else if (v < 0) {
          if (flags & LT_OK_IN_CMP) {
            yamop *nextp = NEXTOP(PREG, plxxs);
            ALWAYS_LOOKAHEAD(nextp->opc);
            PREG = nextp;
            ALWAYS_GONext();
            ALWAYS_END_PREFETCH();
          } else {
            yamop *nextp = PREG->y_u.plxxs.f;
            ALWAYS_LOOKAHEAD(nextp->opc);
            PREG = nextp;
            ALWAYS_GONext();
            ALWAYS_END_PREFETCH();
          }
        } else /* if (v == 0) */ {
          if (flags & EQ_OK_IN_CMP) {
            yamop *nextp = NEXTOP(PREG, plxxs);
            ALWAYS_LOOKAHEAD(nextp->opc);
            PREG = nextp;
            ALWAYS_GONext();
            ALWAYS_END_PREFETCH();
          } else {
            yamop *nextp = PREG->y_u.plxxs.f;
            ALWAYS_LOOKAHEAD(nextp->opc);
            PREG = nextp;
            ALWAYS_GONext();
            ALWAYS_END_PREFETCH();
          }
        }
      }
    exec_bin_cmp_xx : {
      CmpPredicate f = PREG->y_u.plxxs.p->cs.d_code;
      saveregs();
      d0 = (CELL)(f)(d0, d1);
      setregs();
    }
      if (PREG == FAILCODE) {
        JMPNext();
      }
      if (!d0) {
        PREG = PREG->y_u.plxxs.f;
        JMPNext();
      }
      PREG = NEXTOP(PREG, plxxs);
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, call_bfunc_xx_unk, call_bfunc_xx_nvar);
      d1 = Deref(d1);
      goto exec_bin_cmp_xx;
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, call_bfunc_xx2_unk, call_bfunc_xx2_nvar);
      goto exec_bin_cmp_xx;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      BOp(call_bfunc_yx, plxys);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.plxys.y;
      d1 = XREG(PREG->y_u.plxys.x);
      d0 = *pt0;
      ENDP(pt0);
      deref_head(d0, call_bfunc_yx_unk);
    call_bfunc_yx_nvar:
      deref_head(d1, call_bfunc_yx2_unk);
    call_bfunc_yx2_nvar:
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        int flags;

        Int v = IntOfTerm(d0) - IntOfTerm(d1);
        flags = PREG->y_u.plxys.flags;
        if (v > 0) {
          if (flags & GT_OK_IN_CMP) {
            PREG = NEXTOP(PREG, plxys);
            JMPNext();
          } else {
            PREG = PREG->y_u.plxys.f;
            JMPNext();
          }
        } else if (v < 0) {
          if (flags & LT_OK_IN_CMP) {
            PREG = NEXTOP(PREG, plxys);
            JMPNext();
          } else {
            PREG = PREG->y_u.plxys.f;
            JMPNext();
          }
        } else /* if (v == 0) */ {
          if (flags & EQ_OK_IN_CMP) {
            PREG = NEXTOP(PREG, plxys);
            JMPNext();
          } else {
            PREG = PREG->y_u.plxys.f;
            JMPNext();
          }
        }
      }
    exec_bin_cmp_yx : {
      CmpPredicate f = PREG->y_u.plxys.p->cs.d_code;
      saveregs();
      d0 = (CELL)(f)(d0, d1);
      setregs();
    }
      if (!d0 || PREG == FAILCODE) {
        if (PREG != FAILCODE)
          PREG = PREG->y_u.plxys.f;
        JMPNext();
      }
      PREG = NEXTOP(PREG, plxys);
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, call_bfunc_yx_unk, call_bfunc_yx_nvar);
      d1 = Deref(d1);
      goto exec_bin_cmp_yx;
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, call_bfunc_yx2_unk, call_bfunc_yx2_nvar);
      goto exec_bin_cmp_yx;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      BOp(call_bfunc_xy, plxys);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.plxys.y;
      d0 = XREG(PREG->y_u.plxys.x);
      d1 = *pt0;
      ENDP(pt0);
      deref_head(d0, call_bfunc_xy_unk);
    call_bfunc_xy_nvar:
      deref_head(d1, call_bfunc_xy2_unk);
    call_bfunc_xy2_nvar:
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        int flags;

        Int v = IntOfTerm(d0) - IntOfTerm(d1);
        flags = PREG->y_u.plxys.flags;
        if (v > 0) {
          if (flags & GT_OK_IN_CMP) {
            PREG = NEXTOP(PREG, plxys);
            JMPNext();
          } else {
            PREG = PREG->y_u.plxys.f;
            JMPNext();
          }
        } else if (v < 0) {
          if (flags & LT_OK_IN_CMP) {
            PREG = NEXTOP(PREG, plxys);
            JMPNext();
          } else {
            PREG = PREG->y_u.plxys.f;
            JMPNext();
          }
        } else /* if (v == 0) */ {
          if (flags & EQ_OK_IN_CMP) {
            PREG = NEXTOP(PREG, plxys);
            JMPNext();
          } else {
            PREG = PREG->y_u.plxys.f;
            JMPNext();
          }
        }
      }
    exec_bin_cmp_xy : {
      CmpPredicate f = PREG->y_u.plxys.p->cs.d_code;
      saveregs();
      d0 = (CELL)(f)(d0, d1);
      setregs();
    }
      if (!d0 || PREG == FAILCODE) {
        if (PREG != FAILCODE)
          PREG = PREG->y_u.plxys.f;
        JMPNext();
      }
      PREG = NEXTOP(PREG, plxys);
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, call_bfunc_xy_unk, call_bfunc_xy_nvar);
      d1 = Deref(d1);
      goto exec_bin_cmp_xy;
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, call_bfunc_xy2_unk, call_bfunc_xy2_nvar);
      goto exec_bin_cmp_xy;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      BOp(call_bfunc_yy, plyys);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.plyys.y1;
      BEGP(pt1);
      pt1 = YREG + PREG->y_u.plyys.y2;
      d0 = *pt0;
      d1 = *pt1;
      ENDP(pt1);
      ENDP(pt0);
      deref_head(d0, call_bfunc_yy_unk);
    call_bfunc_yy_nvar:
      deref_head(d1, call_bfunc_yy2_unk);
    call_bfunc_yy2_nvar:
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        int flags;

        Int v = IntOfTerm(d0) - IntOfTerm(d1);
        flags = PREG->y_u.plyys.flags;
        if (v > 0) {
          if (flags & GT_OK_IN_CMP) {
            PREG = NEXTOP(PREG, plyys);
            JMPNext();
          } else {
            PREG = PREG->y_u.plyys.f;
            JMPNext();
          }
        } else if (v < 0) {
          if (flags & LT_OK_IN_CMP) {
            PREG = NEXTOP(PREG, plyys);
            JMPNext();
          } else {
            PREG = PREG->y_u.plyys.f;
            JMPNext();
          }
        } else /* if (v == 0) */ {
          if (flags & EQ_OK_IN_CMP) {
            PREG = NEXTOP(PREG, plyys);
            JMPNext();
          } else {
            PREG = PREG->y_u.plyys.f;
            JMPNext();
          }
        }
      }
    exec_bin_cmp_yy : {
      CmpPredicate f = PREG->y_u.plyys.p->cs.d_code;
      saveregs();
      d0 = (CELL)(f)(d0, d1);
      setregs();
    }
      if (!d0 || PREG == FAILCODE) {
        if (PREG != FAILCODE)
          PREG = PREG->y_u.plyys.f;
        JMPNext();
      }
      PREG = NEXTOP(PREG, plyys);
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, call_bfunc_yy_unk, call_bfunc_yy_nvar);
      d1 = Deref(d1);
      goto exec_bin_cmp_yy;
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, call_bfunc_yy2_unk, call_bfunc_yy2_nvar);
      goto exec_bin_cmp_yy;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      Op(p_equal, e);
      save_hb();
      if (Yap_IUnify(ARG1, ARG2) == false) {
        FAIL();
      }
      PREG = NEXTOP(PREG, e);
      GONext();
      ENDOp();

#if INLINE_BIG_COMPARISONS
      Op(p_dif, l);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace)
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorDiff, 0)),
                        XREGS + 1);
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      BEGD(d1);
      d0 = ARG1;
      deref_head(d0, dif_unk1);
    dif_nvar1:
      /* first argument is bound */
      d1 = ARG2;
      deref_head(d1, dif_nvar1_unk2);
    dif_nvar1_nvar2:
      /* both arguments are bound */
      if (d0 == d1) {
        PREG = PREG->y_u.l.l;
        GONext();
      }
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) {
        PREG = NEXTOP(PREG, l);
        GONext();
      }
      {
        Int opresult;
#ifdef COROUTINING
        /*
         * We may wake up goals during our attempt to unify the
         * two terms. If we are adding to the tail of a list of
         * woken goals that should be ok, but otherwise we need
         * to restore LOCAL_WokenGoals to its previous value.
         */
        CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals);

#endif
        /* We will have to look inside compound terms */
        register tr_fr_ptr pt0;
        /* store the old value of TR for clearing bindings */
        pt0 = TR;
        BEGCHO(pt1);
        pt1 = B;
        /* make B and HB point to H to guarantee all bindings will
         * be trailed
         */
        HBREG = HR;
        B = (choiceptr)HR;
        B->cp_h = HR;
        SET_BB(B);
        save_hb();
        opresult = Yap_IUnify(d0, d1);
#ifdef COROUTINING
        /* now restore Woken Goals to its old value */
        Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals);
        if (OldWokenGoals == TermNil) {
          Yap_get_signal(YAP_WAKEUP_SIGNAL);
        }
#endif
        /* restore B */
        B = pt1;
        SET_BB(PROTECT_FROZEN_B(pt1));
#ifdef COROUTINING
        HR = HBREG;
#endif
        HBREG = B->cp_h;
        /* untrail all bindings made by Yap_IUnify */
        while (TR != pt0) {
          BEGD(d1);
          d1 = TrailTerm(--TR);
          if (IsVarTerm(d1)) {
#if defined(YAPOR_SBA) && defined(YAPOR)
            /* clean up the trail when we backtrack */
            if (Unsigned((Int)(d1) - (Int)(H_FZ)) >
                Unsigned((Int)(B_FZ) - (Int)(H_FZ))) {
              RESET_VARIABLE(STACK_TO_SBA(d1));
            } else
#endif
              /* normal variable */
              RESET_VARIABLE(d1);
#ifdef MULTI_ASSIGNMENT_VARIABLES
          } else /* if (IsApplTerm(d1)) */ {
            CELL *pt = RepAppl(d1);
/* AbsAppl means */
/* multi-assignment variable */
/* so the next cell is the old value */
#ifdef FROZEN_STACKS
            pt[0] = TrailVal(--TR);
#else
            pt[0] = TrailTerm(--TR);
            TR--;
#endif /* FROZEN_STACKS */
#endif /* MULTI_ASSIGNMENT_VARIABLES */
          }
          ENDD(d1);
        }
        if (opresult) {
          /* restore B, no need to restore HB */
          PREG = PREG->y_u.l.l;
          GONext();
        }
        /* restore B, and later HB */
        PREG = NEXTOP(PREG, l);
        ENDCHO(pt1);
      }
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, dif_unk1, dif_nvar1);
      ENDP(pt0);
      /* first argument is unbound */
      PREG = PREG->y_u.l.l;
      GONext();

      BEGP(pt0);
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2);
      ENDP(pt0);
      /* second argument is unbound */
      PREG = PREG->y_u.l.l;
      GONext();
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_eq, l);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackEq, HR);
      ENDCACHE_Y_AS_ENV();
    do_eq:
#endif
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace)
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorSame, 0)),
                        XREGS + 1);
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      BEGD(d1);
      d0 = ARG1;
      deref_head(d0, p_eq_unk1);
    p_eq_nvar1:
      /* first argument is bound */
      d1 = ARG2;
      deref_head(d1, p_eq_nvar1_unk2);
    p_eq_nvar1_nvar2:
      /* both arguments are bound */
      if (d0 == d1) {
        PREG = NEXTOP(PREG, l);
        GONext();
      }
      if (IsPairTerm(d0)) {
        if (!IsPairTerm(d1)) {
          PREG = PREG->y_u.l.l;
          GONext();
        }
        BEGD(d2);
        always_save_pc();
        d2 = iequ_complex(RepPair(d0) - 1, RepPair(d0) + 1, RepPair(d1) - 1);
        if (d2 == false) {
          PREG = PREG->y_u.l.l;
          GONext();
        }
        ENDD(d2);
        always_set_pc();
        PREG = NEXTOP(PREG, l);
        GONext();
      }
      if (IsApplTerm(d0)) {
        Functor f0 = FunctorOfTerm(d0);
        Functor f1;

        /* f1 must be a compound term, even if it is a suspension */
        if (!IsApplTerm(d1)) {
          PREG = PREG->y_u.l.l;
          GONext();
        }
        f1 = FunctorOfTerm(d1);

        /* we now know f1 is true */
        /* deref if a compound term */
        if (IsExtensionFunctor(f0)) {
          switch ((CELL)f0) {
          case (CELL)FunctorDBRef:
            if (d0 == d1) {
              PREG = NEXTOP(PREG, l);
              GONext();
            }
            PREG = PREG->y_u.l.l;
            GONext();
          case (CELL)FunctorLongInt:
            if (f1 != FunctorLongInt) {
              PREG = PREG->y_u.l.l;
              GONext();
            }
            if (LongIntOfTerm(d0) == LongIntOfTerm(d1)) {
              PREG = NEXTOP(PREG, l);
              GONext();
            }
            PREG = PREG->y_u.l.l;
            GONext();
          case (CELL)FunctorString:
            if (f1 != FunctorString) {
              PREG = PREG->y_u.l.l;
              GONext();
            }
            if (strcmp((char *)(RepAppl(d0) + 2), (char *)(RepAppl(d1) + 2)) ==
                0) {
              PREG = NEXTOP(PREG, l);
              GONext();
            }
            PREG = PREG->y_u.l.l;
            GONext();
            break;
#ifdef USE_GMP
          case (CELL)FunctorBigInt:
            if (f1 != FunctorBigInt) {
              PREG = PREG->y_u.l.l;
              GONext();
            }
            if (Yap_gmp_tcmp_big_big(d0, d1) == 0) {
              PREG = NEXTOP(PREG, l);
              GONext();
            }
            PREG = PREG->y_u.l.l;
            GONext();
#endif
          case (CELL)FunctorDouble:
            if (f1 != FunctorDouble) {
              PREG = PREG->y_u.l.l;
              GONext();
            }
            if (FloatOfTerm(d0) == FloatOfTerm(d1)) {
              PREG = NEXTOP(PREG, l);
              GONext();
            }
            PREG = PREG->y_u.l.l;
            GONext();
            break;
          default:
            PREG = PREG->y_u.l.l;
            GONext();
          }
        }
        if (f0 != f1) {
          PREG = PREG->y_u.l.l;
          GONext();
        }
        always_save_pc();
        BEGD(d2);
        d2 = iequ_complex(RepAppl(d0), RepAppl(d0) + ArityOfFunctor(f0),
                          RepAppl(d1));
        if (d2 == false) {
          PREG = PREG->y_u.l.l;
          GONext();
        }
        ENDD(d2);
        always_set_pc();
        PREG = NEXTOP(PREG, l);
        GONext();
      }
      PREG = PREG->y_u.l.l;
      GONext();

      BEGP(pt0);
      deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2);
      ENDP(pt0);
      /* first argument is bound */
      /* second argument is unbound */
      /* I don't need to worry about co-routining because an
         unbound variable may never be == to a constrained variable!! */
      PREG = PREG->y_u.l.l;
      GONext();
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1);
      BEGD(d1);
      d1 = ARG2;
      deref_head(d1, p_eq_var1_unk2);
    p_eq_var1_nvar2:
      /* I don't need to worry about co-routining because an
         unbound variable may never be == to a constrained variable!! */
      PREG = PREG->y_u.l.l;
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2);
      /* first argument is unbound */
      /* second argument is unbound */
      if (pt1 != pt0) {
        PREG = PREG->y_u.l.l;
        GONext();
      }
      PREG = NEXTOP(PREG, l);
      GONext();
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      ENDD(d0);
#ifdef COROUTINING
    /* Problem: have I got an environment or not? */
    NoStackEq:
      PROCESS_INTERRUPT(interrupt_eq, do_eq, E_CB*EnvSize);
#endif

      ENDOp();
#endif /* INLINE_BIG_COMPARISONS */

      Op(p_arg_vv, xxx);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
        ts[0] = XREG(PREG->y_u.xxx.x1);
        ts[1] = XREG(PREG->y_u.xxx.x2);
        RESET_VARIABLE(ts + 2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorArg, 0)), ts);
      }
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxx.x1);
      deref_head(d0, arg_arg1_unk);
    arg_arg1_nvar:
      /* ARG1 is ok! */
      if (IsIntTerm(d0))
        d0 = IntOfTerm(d0);
      else if (IsLongIntTerm(d0)) {
        d0 = LongIntOfTerm(d0);
      } else {
        if (IsBigIntTerm(d0))
          FAIL();
        Yap_AsmError(TYPE_ERROR_INTEGER, d0);
        FAIL();
      }

      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xxx.x2);
      deref_head(d1, arg_arg2_unk);
    arg_arg2_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
        BEGP(pt0);
        pt0 = RepAppl(d1);
        d1 = *pt0;
        if (IsExtensionFunctor((Functor)d1)) {
          Yap_AsmError(TYPE_ERROR_COMPOUND, AbsAppl(pt0));
          FAIL();
        }
        if ((Int)d0 <= 0 || (Int)d0 > ArityOfFunctor((Functor)d1)) {
          /* don't complain here for Prolog compatibility
             if ((Int)d0 <= 0) {
             saveregs();
Yap_AsmError( DOMAIN_ERROR_NOT_LESS_THAN_ZERO );
             MkIntegerTerm(d0),"arg 1 of arg/3");
             setregs();
             }
          */
          FAIL();
        }
        XREG(PREG->y_u.xxx.x) = pt0[d0];
        PREG = NEXTOP(PREG, xxx);
        GONext();
        ENDP(pt0);
      } else if (IsPairTerm(d1)) {
        BEGP(pt0);
        pt0 = RepPair(d1);
        if (d0 != 1 && d0 != 2) {
          if ((Int)d0 < 0) {
              Yap_AsmError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(d0)                                                                                                                 );
          }
          FAIL();
        }
        XREG(PREG->y_u.xxx.x) = pt0[d0 - 1];
        PREG = NEXTOP(PREG, xxx);
        GONext();
        ENDP(pt0);
      } else {
        Yap_AsmError(TYPE_ERROR_COMPOUND, d1);
        FAIL();
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_arg2_unk, arg_arg2_nvar);
      saveregs();
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      ;
      setregs();
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, arg_arg1_unk, arg_arg1_nvar);
      saveregs();
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ;
      setregs();
      ENDP(pt0);
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_arg_cv, xxn);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        CELL *Ho = HR;
        Term ts[3];
        Term t = MkIntegerTerm(PREG->y_u.xxn.c);
        ts[0] = t;
        ts[1] = XREG(PREG->y_u.xxn.xi);
        RESET_VARIABLE(ts + 2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorArg, 0)), ts);
        HR = Ho;
      }
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = PREG->y_u.xxn.c;
      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xxn.xi);
      deref_head(d1, arg_arg2_vc_unk);
    arg_arg2_vc_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
        BEGP(pt0);
        pt0 = RepAppl(d1);
        d1 = *pt0;
        if (IsExtensionFunctor((Functor)d1)) {
          Yap_AsmError(TYPE_ERROR_COMPOUND,XREG(PREG->y_u.xxn.xi));
          FAIL();
        }
        if ((Int)d0 <= 0 || (Int)d0 > ArityOfFunctor((Functor)d1)) {
          /* don't complain here for Prolog compatibility
             if ((Int)d0 <= 0) {
             saveregs();
Yap_AsmError( DOMAIN_ERROR_NOT_LESS_THAN_ZERO );
             MkIntegerTerm(d0),"arg 1 of arg/3");
             setregs();
             }
          */
          FAIL();
        }
        XREG(PREG->y_u.xxn.x) = pt0[d0];
        PREG = NEXTOP(PREG, xxn);
        GONext();
        ENDP(pt0);
      } else if (IsPairTerm(d1)) {
        BEGP(pt0);
        pt0 = RepPair(d1);
        if (d0 != 1 && d0 != 2) {
          if ((Int)d0 < 0) {
            saveregs();
            Yap_AsmError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, XREG(PREG->y_u.xxn.xi));
          }
          FAIL();
        }
        XREG(PREG->y_u.xxn.x) = pt0[d0 - 1];
        PREG = NEXTOP(PREG, xxn);
        GONext();
        ENDP(pt0);
      } else {
        Yap_AsmError(TYPE_ERROR_COMPOUND, XREG(PREG->y_u.xxn.xi));
        FAIL();
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_arg2_vc_unk, arg_arg2_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR, XREG(PREG->y_u.xxn.xi));
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      ENDD(d0);
      ENDOp();

      Op(p_arg_y_vv, yxx);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
        ts[0] = XREG(PREG->y_u.yxx.x1);
        ts[1] = XREG(PREG->y_u.yxx.x2);
        ts[2] = YREG[PREG->y_u.yxx.y];
        RESET_VARIABLE(ts + 2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorArg, 0)), ts);
      }
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxx.x1);
      deref_head(d0, arg_y_arg1_unk);
    arg_y_arg1_nvar:
      /* ARG1 is ok! */
      if (IsIntTerm(d0))
        d0 = IntOfTerm(d0);
      else if (IsLongIntTerm(d0)) {
        d0 = LongIntOfTerm(d0);
      } else {
        if (IsBigIntTerm(d0))
          FAIL();
        Yap_AsmError(TYPE_ERROR_INTEGER, XREG(PREG->y_u.yxx.x1));
        FAIL();
      }

      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = XREG(PREG->y_u.yxx.x2);
      deref_head(d1, arg_y_arg2_unk);
    arg_y_arg2_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
        BEGP(pt0);
        pt0 = RepAppl(d1);
        d1 = *pt0;
        if (IsExtensionFunctor((Functor)d1)) {
          Yap_AsmError(TYPE_ERROR_COMPOUND,XREG(PREG->y_u.yxx.x2));
          FAIL();
        }
        if ((Int)d0 <= 0 || (Int)d0 > ArityOfFunctor((Functor)d1)) {
          /* don't complain here for Prolog compatibility
             if ((Int)d0 <= 0) {
             saveregs();
Yap_AsmError( DOMAIN_ERROR_NOT_LESS_THAN_ZERO );
             MkIntegerTerm(d0),"arg 1 of arg/3");
             saveregs();
             }
          */
          FAIL();
        }
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxx.y;
        PREG = NEXTOP(PREG, yxx);
        INITIALIZE_PERMVAR(pt1, pt0[d0]);
        ENDP(pt1);
        GONext();
        ENDP(pt0);
      } else if (IsPairTerm(d1)) {
        BEGP(pt0);
        pt0 = RepPair(d1);
        if (d0 != 1 && d0 != 2) {
          if ((Int)d0 < 0) {
            saveregs();
            Yap_AsmError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, YREG[PREG->y_u.yxx.y]);
          }
          FAIL();
        }
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxx.y;
        PREG = NEXTOP(PREG, yxx);
        INITIALIZE_PERMVAR(pt1, pt0[d0 - 1]);
        GONext();
        ENDP(pt1);
        ENDP(pt0);
      } else {
          Yap_AsmError(TYPE_ERROR_COMPOUND, d1);
        FAIL();
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_y_arg2_unk, arg_y_arg2_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, arg_y_arg1_unk, arg_y_arg1_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt0);
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_arg_y_cv, yxn);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        CELL *Ho = HR;
        Term ts[3];
        Term t = MkIntegerTerm(PREG->y_u.yxn.c);
        ts[0] = t;
        ts[1] = XREG(PREG->y_u.yxn.xi);
        ts[2] = YREG[PREG->y_u.yxn.y];
        RESET_VARIABLE(ts + 2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorArg, 0)), ts);
        HR = Ho;
      }
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = PREG->y_u.yxn.c;
      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = XREG(PREG->y_u.yxn.xi);
      deref_head(d1, arg_y_arg2_vc_unk);
    arg_y_arg2_vc_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
        BEGP(pt0);
        pt0 = RepAppl(d1);
        d1 = *pt0;
        if (IsExtensionFunctor((Functor)d1)) {
          Yap_AsmError(TYPE_ERROR_COMPOUND, XREG(PREG->y_u.yxn.xi));
          FAIL();
        }
        if ((Int)d0 <= 0 || (Int)d0 > ArityOfFunctor((Functor)d1)) {
          /* don't complain here for Prolog compatibility
             if ((Int)d0 <= 0) {
Yap_AsmError( DOMAIN_ERROR_NOT_LESS_THAN_ZERO );
             }
          */
          FAIL();
        }
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxn.y;
        PREG = NEXTOP(PREG, yxn);
        INITIALIZE_PERMVAR(pt1, pt0[d0]);
        ENDP(pt1);
        GONext();
        ENDP(pt0);
      } else if (IsPairTerm(d1)) {
        BEGP(pt0);
        pt0 = RepPair(d1);
        if (d0 != 1 && d0 != 2) {
          if ((Int)d0 < 0) {
            saveregs();
            Yap_AsmError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, YREG[PREG->y_u.yxn.y]);
          }
          FAIL();
        }
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxn.y;
        PREG = NEXTOP(PREG, yxn);
        INITIALIZE_PERMVAR(pt1, pt0[d0 - 1]);
        ENDP(pt1);
        GONext();
        ENDP(pt0);
      } else {
        Yap_AsmError(TYPE_ERROR_COMPOUND,YREG[PREG->y_u.yxn.y]);
        FAIL();
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_y_arg2_vc_unk, arg_y_arg2_vc_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      ENDD(d0);
      ENDOp();

      Op(p_func2s_vv, xxx);
    /* A1 is a variable */
    restart_func2s:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
        RESET_VARIABLE(ts);
        ts[1] = XREG(PREG->y_u.xxx.x1);
        ts[2] = XREG(PREG->y_u.xxx.x2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
      }
#endif /* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxx.x1);
      deref_head(d0, func2s_unk);
    func2s_nvar:
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xxx.x2);
      deref_head(d1, func2s_unk2);
    func2s_nvar2:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1))
        d1 = IntegerOfTerm(d1);
      else {
        saveregs();
        if (IsBigIntTerm(d1)) {
            setregs();
          Yap_AsmError(RESOURCE_ERROR_STACK,d1);
        } else {
            setregs();
          Yap_AsmError(TYPE_ERROR_INTEGER,d1);
        }
        FAIL();
      }
      if (!IsAtomicTerm(d0)) {
        Yap_AsmError(TYPE_ERROR_ATOM, d0);
        FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
        RESET_VARIABLE(HR);
        RESET_VARIABLE(HR + 1);
        d0 = AbsPair(HR);
        HR += 2;
        /* else if arity is 0 just pass d0 through */
        /* Ding, ding, we made it */
        XREG(PREG->y_u.xxx.x) = d0;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx), Osbpp), l);
        GONext();
      } else if ((Int)d1 > 0) {
        /* now let's build a compound term */
        if (!IsAtomTerm(d0)) {
          Yap_AsmError(TYPE_ERROR_ATOM, d0);
          FAIL();
        }
        BEGP(pt1);
        if (!IsAtomTerm(d0)) {
          FAIL();
        } else
          d0 = (CELL)Yap_MkFunctor(AtomOfTerm(d0), (Int)d1);
        pt1 = HR;
        *pt1++ = d0;
        d0 = AbsAppl(HR);
        if (pt1 + d1 > ENV || pt1 + d1 > (CELL *)B) {
          /* make sure we have something to show for our trouble */
          saveregs();
          if (!Yap_gcl((1 + d1) * sizeof(CELL), 0, YREG,
                       NEXTOP(NEXTOP(PREG, xxx), Osbpp))) {
                           setregs();
            Yap_AsmError(RESOURCE_ERROR_STACK,   d1                                                                                                                                                                                                                                                                       );
            JMPNext();
          } else {
            setregs();
          }
          goto restart_func2s;
        }
        while ((Int)d1--) {
          RESET_VARIABLE(pt1);
          pt1++;
        }
        HR = pt1;
        /* done building the term */
        ENDP(pt1);
        /* else if arity is 0 just pass d0 through */
        /* Ding, ding, we made it */
        XREG(PREG->y_u.xxx.x) = d0;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx), Osbpp), l);
        GONext();
      } else if ((Int)d1 == 0) {
        XREG(PREG->y_u.xxx.x) = d0;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx), Osbpp), l);
        GONext();
      } else {
        Yap_AsmError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(d1));
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_unk2, func2s_nvar2);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);

      BEGP(pt1);
      deref_body(d0, pt1, func2s_unk, func2s_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2s_cv, xxc);
    /* A1 is a variable */
    restart_func2s_cv:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
        RESET_VARIABLE(ts);
        ts[1] = PREG->y_u.xxc.c;
        ts[2] = XREG(PREG->y_u.xxc.xi);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
      }
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      /* We have to build the structure */
      d0 = PREG->y_u.xxc.c;
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xxc.xi);
      deref_head(d1, func2s_unk2_cv);
    func2s_nvar2_cv:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1))
        d1 = IntegerOfTerm(d1);
      else {
        saveregs();
        if (IsBigIntTerm(d1)) {
            setregs();
          Yap_AsmError(RESOURCE_ERROR_STACK, d1);
        } else {
            setregs();
          Yap_AsmError(TYPE_ERROR_INTEGER, d1);
        }
        FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
        RESET_VARIABLE(HR);
        RESET_VARIABLE(HR + 1);
        d0 = AbsPair(HR);
        HR += 2;
        /* else if arity is 0 just pass d0 through */
        /* Ding, ding, we made it */
        XREG(PREG->y_u.xxc.x) = d0;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc), Osbpp), l);
        GONext();
      } else if ((Int)d1 > 0) {
        /* now let's build a compound term */
        if (!IsAtomTerm(d0)) {
          Yap_AsmError(TYPE_ERROR_ATOM, d0);
          FAIL();
        }
        BEGP(pt1);
        if (!IsAtomTerm(d0)) {
          FAIL();
        } else
          d0 = (CELL)Yap_MkFunctor(AtomOfTerm(d0), (Int)d1);
        pt1 = HR;
        *pt1++ = d0;
        d0 = AbsAppl(HR);
        if (pt1 + d1 > ENV || pt1 + d1 > (CELL *)B) {
          /* make sure we have something to show for our trouble */
          saveregs();
          if (!Yap_gcl((1 + d1) * sizeof(CELL), 0, YREG,
                       NEXTOP(NEXTOP(PREG, xxc), Osbpp))) {
                           setregs();
            Yap_AsmError(RESOURCE_ERROR_STACK, d1);
            JMPNext();
          } else {
            setregs();
          }
          goto restart_func2s_cv;
        }
        while ((Int)d1--) {
          RESET_VARIABLE(pt1);
          pt1++;
        }
        /* done building the term */
        HR = pt1;
        ENDP(pt1);
        /* else if arity is 0 just pass d0 through */
        /* Ding, ding, we made it */
        XREG(PREG->y_u.xxc.x) = d0;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc), Osbpp), l);
        GONext();
      } else if (d1 == 0) {
        XREG(PREG->y_u.xxc.x) = d0;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc), Osbpp), l);
        GONext();
      } else {
        Yap_AsmError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,d1);
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_unk2_cv, func2s_nvar2_cv);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_func2s_vc, xxn);
    /* A1 is a variable */
    restart_func2s_vc:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        Term ti;
        Term ts[3];
        CELL *hi = HR;

        ti = MkIntegerTerm(PREG->y_u.xxn.c);
        RESET_VARIABLE(ts);
        ts[1] = XREG(PREG->y_u.xxn.xi);
        ts[2] = ti;
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
        HR = hi;
      }
#endif /* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      deref_head(d0, func2s_unk_vc);
    func2s_nvar_vc:
      BEGD(d1);
      d1 = PREG->y_u.xxn.c;
      if (!IsAtomicTerm(d0)) {
        Yap_AsmError(TYPE_ERROR_ATOM,d0);
        FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
        RESET_VARIABLE(HR);
        RESET_VARIABLE(HR + 1);
        d0 = AbsPair(HR);
        HR += 2;
        /* else if arity is 0 just pass d0 through */
        /* Ding, ding, we made it */
        XREG(PREG->y_u.xxn.x) = d0;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxn), Osbpp), l);
        GONext();
      }
      /* now let's build a compound term */
      if (d1 == 0) {
        XREG(PREG->y_u.xxn.x) = d0;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxn), Osbpp), l);
        GONext();
      }
      if (!IsAtomTerm(d0)) {
        Yap_AsmError(TYPE_ERROR_ATOM, d0);
        FAIL();
      }
      BEGP(pt1);
      if (!IsAtomTerm(d0)) {
        FAIL();
      } else
        d0 = (CELL)Yap_MkFunctor(AtomOfTerm(d0), (Int)d1);
      pt1 = HR;
      *pt1++ = d0;
      d0 = AbsAppl(HR);
      if (pt1 + d1 > ENV || pt1 + d1 > (CELL *)B) {
        /* make sure we have something to show for our trouble */
        saveregs();
        if (!Yap_gcl(0, 0, YREG, NEXTOP(NEXTOP(PREG, xxn), Osbpp))) {
            setregs();
          Yap_AsmError(INSTANTIATION_ERROR,d1);
          JMPNext();
        } else {
          setregs();
        }
        goto restart_func2s_vc;
      }
      while ((Int)d1--) {
        RESET_VARIABLE(pt1);
        pt1++;
      }
      /* done building the term */
      HR = pt1;
      ENDP(pt1);
      ENDD(d1);
      /* else if arity is 0 just pass d0 through */
      /* Ding, ding, we made it */
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxn), Osbpp), l);
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, func2s_unk_vc, func2s_nvar_vc);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2s_y_vv, yxx);
    /* A1 is a variable */
    restart_func2s_y:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
        RESET_VARIABLE(ts);
        ts[1] = XREG(PREG->y_u.yxx.x1);
        ts[2] = XREG(PREG->y_u.yxx.x2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
      }
#endif /* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxx.x1);
      deref_head(d0, func2s_y_unk);
    func2s_y_nvar:
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->y_u.yxx.x2);
      deref_head(d1, func2s_y_unk2);
    func2s_y_nvar2:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1))
        d1 = IntegerOfTerm(d1);
      else {
        saveregs();
        if (IsBigIntTerm(d1)) {
            setregs();
          Yap_AsmError(RESOURCE_ERROR_STACK, d1);
        } else {
            setregs();
          Yap_AsmError(TYPE_ERROR_INTEGER, d1);
        }
        FAIL();
      }
      if (!IsAtomicTerm(d0)) {
        Yap_AsmError(TYPE_ERROR_ATOM, d0);
        FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
        RESET_VARIABLE(HR);
        RESET_VARIABLE(HR + 1);
        d0 = AbsPair(HR);
        HR += 2;
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxx.y;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx), Osbpp), l);
        INITIALIZE_PERMVAR(pt1, d0);
        ENDP(pt1);
        GONext();
      } else if ((Int)d1 > 0) {
        /* now let's build a compound term */
        if (!IsAtomTerm(d0)) {
          Yap_AsmError(TYPE_ERROR_ATOM, d0);
          FAIL();
        }
        BEGP(pt1);
        if (!IsAtomTerm(d0)) {
          FAIL();
        } else
          d0 = (CELL)Yap_MkFunctor(AtomOfTerm(d0), (Int)d1);
        pt1 = HR;
        *pt1++ = d0;
        d0 = AbsAppl(HR);
        if (pt1 + d1 > ENV || pt1 + d1 > (CELL *)B) {
          /* make sure we have something to show for our trouble */
          saveregs();
          if (!Yap_gcl((1 + d1) * sizeof(CELL), 0, YREG,
                       NEXTOP(NEXTOP(PREG, yxx), Osbpp))) {
            setregs();
            Yap_AsmError(RESOURCE_ERROR_STACK, d1);
            JMPNext();
          } else {
            setregs();
          }
          goto restart_func2s_y;
        }
        while ((Int)d1--) {
          RESET_VARIABLE(pt1);
          pt1++;
        }
        /* done building the term */
        HR = pt1;
        ENDP(pt1);
        /* else if arity is 0 just pass d0 through */
        /* Ding, ding, we made it */
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxx.y;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx), Osbpp), l);
        INITIALIZE_PERMVAR(pt1, d0);
        ENDP(pt1);
        GONext();
      } else if (d1 == 0) {
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxx.y;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx), Osbpp), l);
        INITIALIZE_PERMVAR(pt1, d0);
        ENDP(pt1);
        GONext();
      } else {
        Yap_AsmError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(d1));
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_y_unk2, func2s_y_nvar2);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);

      BEGP(pt1);
      deref_body(d0, pt1, func2s_y_unk, func2s_y_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2s_y_cv, yxc);
    /* A1 is a variable */
    restart_func2s_y_cv:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
        RESET_VARIABLE(ts);
        ts[1] = PREG->y_u.yxc.c;
        ts[2] = XREG(PREG->y_u.yxc.xi);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
      }
#endif /* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = PREG->y_u.yxc.c;
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->y_u.yxc.xi);
      deref_head(d1, func2s_y_unk_cv);
    func2s_y_nvar_cv:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1)) {
        d1 = IntegerOfTerm(d1);
      } else {
        if (IsBigIntTerm(d1)) {
          Yap_AsmError(RESOURCE_ERROR_STACK, d1);
        } else {
          Yap_AsmError(TYPE_ERROR_INTEGER, d1);
        }
        FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
        RESET_VARIABLE(HR);
        RESET_VARIABLE(HR + 1);
        d0 = AbsPair(HR);
        HR += 2;
        /* else if arity is 0 just pass d0 through */
        /* Ding, ding, we made it */
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxc.y;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxc), Osbpp), l);
        INITIALIZE_PERMVAR(pt1, d0);
        ENDP(pt1);
        GONext();
      } else if ((Int)d1 > 0) {
        /* now let's build a compound term */
        if (!IsAtomTerm(d0)) {
          Yap_AsmError(TYPE_ERROR_ATOM,d0);
          FAIL();
        }
        if (!IsAtomTerm(d0)) {
          FAIL();
        } else
          d0 = (CELL)Yap_MkFunctor(AtomOfTerm(d0), (Int)d1);
        BEGP(pt1);
        pt1 = HR;
        *pt1++ = d0;
        d0 = AbsAppl(HR);
        if (pt1 + d1 > ENV || pt1 + d1 > (CELL *)B) {
          /* make sure we have something to show for our trouble */
          saveregs();
          if (!Yap_gcl((1 + d1) * sizeof(CELL), 0, YREG,
                       NEXTOP(NEXTOP(PREG, yxc), Osbpp))) {
            setregs();
            Yap_AsmError(RESOURCE_ERROR_STACK, d1);
            JMPNext();
          } else {
            setregs();
          }
          goto restart_func2s_y_cv;
        }
        while ((Int)d1--) {
          RESET_VARIABLE(pt1);
          pt1++;
        }
        /* done building the term */
        HR = pt1;
        ENDP(pt1);
        /* else if arity is 0 just pass d0 through */
        /* Ding, ding, we made it */
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxc.y;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxc), Osbpp), l);
        INITIALIZE_PERMVAR(pt1, d0);
        ENDP(pt1);
        GONext();
      } else if (d1 == 0) {
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxc.y;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxc), Osbpp), l);
        INITIALIZE_PERMVAR(pt1, d0);
        ENDP(pt1);
        GONext();
      } else {
        Yap_AsmError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(d1));
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_y_unk_cv, func2s_y_nvar_cv);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_func2s_y_vc, yxn);
    /* A1 is a variable */
    restart_func2s_y_vc:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        Term ti;
        CELL *hi = HR;
        Term ts[3];

        ti = MkIntegerTerm((Int)(PREG->y_u.yxn.c));
        RESET_VARIABLE(ts);
        ts[1] = XREG(PREG->y_u.yxn.xi);
        ts[2] = ti;
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
        HR = hi;
      }
#endif /* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      deref_head(d0, func2s_y_unk_vc);
    func2s_y_nvar_vc:
      BEGD(d1);
      d1 = PREG->y_u.yxn.c;
      if (!IsAtomicTerm(d0)) {
        Yap_AsmError(TYPE_ERROR_ATOM,d0);
        FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
        RESET_VARIABLE(HR);
        RESET_VARIABLE(HR + 1);
        d0 = AbsPair(HR);
        HR += 2;
        /* else if arity is 0 just pass d0 through */
        /* Ding, ding, we made it */
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxn.y;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxn), Osbpp), l);
        INITIALIZE_PERMVAR(pt1, d0);
        ENDP(pt1);
        GONext();
      }
      if (d1 == 0) {
        BEGP(pt1);
        pt1 = YREG + PREG->y_u.yxn.y;
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxn), Osbpp), l);
        INITIALIZE_PERMVAR(pt1, d0);
        ENDP(pt1);
        GONext();
      }
      if (!IsAtomTerm(d0)) {
        Yap_AsmError(TYPE_ERROR_ATOM,d0);
        FAIL();
      }
      /* now let's build a compound term */
      if (!IsAtomTerm(d0)) {
        Yap_AsmError(TYPE_ERROR_ATOM,d0);
        FAIL();
      }
      BEGP(pt1);
      if (!IsAtomTerm(d0)) {
        FAIL();
      } else
        d0 = (CELL)Yap_MkFunctor(AtomOfTerm(d0), (Int)d1);
      pt1 = HR;
      *pt1++ = d0;
      d0 = AbsAppl(HR);
      if (pt1 + d1 > ENV || pt1 + d1 > (CELL *)B) {
        /* make sure we have something to show for our trouble */
        saveregs();
        if (!Yap_gcl((1 + d1) * sizeof(CELL), 0, YREG,
                     NEXTOP(NEXTOP(PREG, yxn), Osbpp))) {
                         setregs();
          Yap_AsmError(RESOURCE_ERROR_STACK, d1);
          JMPNext();
        } else {
          setregs();
        }
        goto restart_func2s_y_vc;
      }
      while ((Int)d1--) {
        RESET_VARIABLE(pt1);
        pt1++;
      }
      /* done building the term */
      HR = pt1;
      ENDP(pt1);
      /* else if arity is 0 just pass d0 through */
      /* Ding, ding, we made it */
      BEGP(pt1);
      pt1 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxn), Osbpp), l);
      INITIALIZE_PERMVAR(pt1, d0);
      ENDP(pt1);
      ENDD(d1);
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, func2s_y_unk_vc, func2s_y_nvar_vc);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_xx, xxx);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
        ts[0] = XREG(PREG->y_u.xxx.x);
        RESET_VARIABLE(ts + 1);
        RESET_VARIABLE(ts + 2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
      }
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxx.x);
      deref_head(d0, func2f_xx_unk);
    func2f_xx_nvar:
      if (IsApplTerm(d0)) {
        Functor d1 = FunctorOfTerm(d0);
        if (IsExtensionFunctor(d1)) {
          XREG(PREG->y_u.xxx.x1) = d0;
          XREG(PREG->y_u.xxx.x2) = MkIntTerm(0);
          PREG = NEXTOP(PREG, xxx);
          GONext();
        }
        XREG(PREG->y_u.xxx.x1) = MkAtomTerm(NameOfFunctor(d1));
        XREG(PREG->y_u.xxx.x2) = MkIntegerTerm(ArityOfFunctor(d1));
        PREG = NEXTOP(PREG, xxx);
        GONext();
      } else if (IsPairTerm(d0)) {
        XREG(PREG->y_u.xxx.x1) = TermDot;
        XREG(PREG->y_u.xxx.x2) = MkIntTerm(2);
        PREG = NEXTOP(PREG, xxx);
        GONext();
      } else {
        XREG(PREG->y_u.xxx.x1) = d0;
        XREG(PREG->y_u.xxx.x2) = MkIntTerm(0);
        PREG = NEXTOP(PREG, xxx);
        GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_xx_unk, func2f_xx_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_xy, xxy);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
        ts[0] = XREG(PREG->y_u.xxy.x);
        RESET_VARIABLE(ts + 1);
        RESET_VARIABLE(ts + 2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
      }
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxy.x);
      deref_head(d0, func2f_xy_unk);
    func2f_xy_nvar:
      if (IsApplTerm(d0)) {
        Functor d1 = FunctorOfTerm(d0);
        CELL *pt0 = YREG + PREG->y_u.xxy.y2;
        if (IsExtensionFunctor(d1)) {
          XREG(PREG->y_u.xxy.x1) = d0;
          PREG = NEXTOP(PREG, xxy);
          INITIALIZE_PERMVAR(pt0, MkIntTerm(0));
          GONext();
        }
        XREG(PREG->y_u.xxy.x1) = MkAtomTerm(NameOfFunctor(d1));
        PREG = NEXTOP(PREG, xxy);
        INITIALIZE_PERMVAR(pt0, MkIntegerTerm(ArityOfFunctor(d1)));
        GONext();
      } else if (IsPairTerm(d0)) {
        CELL *pt0 = YREG + PREG->y_u.xxy.y2;
        XREG(PREG->y_u.xxy.x1) = TermDot;
        PREG = NEXTOP(PREG, xxy);
        INITIALIZE_PERMVAR(pt0, MkIntTerm(2));
        GONext();
      } else {
        CELL *pt0 = YREG + PREG->y_u.xxy.y2;
        XREG(PREG->y_u.xxy.x1) = d0;
        PREG = NEXTOP(PREG, xxy);
        INITIALIZE_PERMVAR(pt0, MkIntTerm(0));
        GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_xy_unk, func2f_xy_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_yx, yxx);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
        ts[0] = XREG(PREG->y_u.yxx.x2);
        RESET_VARIABLE(ts + 1);
        RESET_VARIABLE(ts + 2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
      }
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxx.x2);
      deref_head(d0, func2f_yx_unk);
    func2f_yx_nvar:
      if (IsApplTerm(d0)) {
        Functor d1 = FunctorOfTerm(d0);
        CELL *pt0 = YREG + PREG->y_u.yxx.y;
        if (IsExtensionFunctor(d1)) {
          XREG(PREG->y_u.yxx.x1) = MkIntTerm(0);
          PREG = NEXTOP(PREG, yxx);
          INITIALIZE_PERMVAR(pt0, d0);
          GONext();
        }
        XREG(PREG->y_u.yxx.x1) = MkIntegerTerm(ArityOfFunctor(d1));
        PREG = NEXTOP(PREG, yxx);
        INITIALIZE_PERMVAR(pt0, MkAtomTerm(NameOfFunctor(d1)));
        GONext();
      } else if (IsPairTerm(d0)) {
        CELL *pt0 = YREG + PREG->y_u.yxx.y;
        XREG(PREG->y_u.yxx.x1) = MkIntTerm(2);
        PREG = NEXTOP(PREG, yxx);
        INITIALIZE_PERMVAR(pt0, TermDot);
        GONext();
      } else {
        CELL *pt0 = YREG + PREG->y_u.yxx.y;
        XREG(PREG->y_u.yxx.x1) = MkIntTerm(0);
        PREG = NEXTOP(PREG, yxx);
        INITIALIZE_PERMVAR(pt0, d0);
        GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_yx_unk, func2f_yx_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_yy, yyx);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
          Term ts[3];
          ts[0] = XREG(PREG->y_u.yyx.x);
        RESET_VARIABLE(ts + 1);
        RESET_VARIABLE(ts + 2);
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        ts);
      }
#endif /* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->y_u.yyx.x);
      deref_head(d0, func2f_yy_unk);
    func2f_yy_nvar:
      if (IsApplTerm(d0)) {
        Functor d1 = FunctorOfTerm(d0);
        CELL *pt0 = YREG + PREG->y_u.yyx.y1;
        CELL *pt1 = YREG + PREG->y_u.yyx.y2;
        if (IsExtensionFunctor(d1)) {
          PREG = NEXTOP(PREG, yyx);
          INITIALIZE_PERMVAR(pt0, d0);
          INITIALIZE_PERMVAR(pt1, MkIntTerm(0));
          GONext();
        }
        PREG = NEXTOP(PREG, yyx);
        INITIALIZE_PERMVAR(pt0, MkAtomTerm(NameOfFunctor(d1)));
        INITIALIZE_PERMVAR(pt1, MkIntegerTerm(ArityOfFunctor(d1)));
        GONext();
      } else if (IsPairTerm(d0)) {
        CELL *pt0 = YREG + PREG->y_u.yyx.y1;
        CELL *pt1 = YREG + PREG->y_u.yyx.y2;
        PREG = NEXTOP(PREG, yyx);
        INITIALIZE_PERMVAR(pt0, TermDot);
        INITIALIZE_PERMVAR(pt1, MkIntTerm(2));
        GONext();
      } else {
        CELL *pt0 = YREG + PREG->y_u.yyx.y1;
        CELL *pt1 = YREG + PREG->y_u.yyx.y2;
        PREG = NEXTOP(PREG, yyx);
        INITIALIZE_PERMVAR(pt0, d0);
        INITIALIZE_PERMVAR(pt1, MkIntTerm(0));
        GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_yy_unk, func2f_yy_nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_functor, e);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace)
        low_level_trace(enter_pred,
                        RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor, 0)),
                        XREGS + 1);
#endif /* LOW_LEVEL_TRACE */
    restart_functor:
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, func_unk);
    func_nvar:
      /* A1 is bound */
      BEGD(d1);
      if (IsApplTerm(d0)) {
        d1 = *RepAppl(d0);
        if (IsExtensionFunctor((Functor)d1)) {
          if (d1 <= (CELL)FunctorDouble && d1 >= (CELL)FunctorLongInt) {
            d1 = MkIntTerm(0);
          } else
            FAIL();
        } else {
          d0 = MkAtomTerm(NameOfFunctor((Functor)d1));
          d1 = MkIntTerm(ArityOfFunctor((Functor)d1));
        }
      } else if (IsPairTerm(d0)) {
        d0 = TermDot;
        d1 = MkIntTerm(2);
      } else {
        d1 = MkIntTerm(0);
      }
      /* d1 and d0 now have the two arguments */
      /* let's go and bind them */
      {
        register CELL arity = d1;

        d1 = ARG2;
        deref_head(d1, func_nvar_unk);
      func_nvar_nvar:
        /* A2 was bound */
        if (d0 != d1) {
          FAIL();
        }
        /* I have to this here so that I don't have a jump to a closing bracket
         */
        d0 = arity;
        goto func_bind_x3;

        BEGP(pt0);
        deref_body(d1, pt0, func_nvar_unk, func_nvar_nvar);
        /* A2 is a variable, go and bind it */
        YapBind(pt0, d0);
        /* I have to this here so that I don't have a jump to a closing bracket
         */
        d0 = arity;
        ENDP(pt0);
      func_bind_x3:
        /* now let's process A3 */
        d1 = ARG3;
        deref_head(d1, func_nvar3_unk);
      func_nvar3_nvar:
        /* A3 was bound */
        if (d0 != d1) {
          FAIL();
        }
        /* Done */
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e), Osbmp), l);
        GONext();

        BEGP(pt0);
        deref_body(d1, pt0, func_nvar3_unk, func_nvar3_nvar);
        /* A3 is a variable, go and bind it */
        PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e), Osbmp), l);
        YapBind(pt0, d0);
        /* Done */
        GONext();

        ENDP(pt0);
      }
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, func_unk, func_nvar);
      /* A1 is a variable */
      /* We have to build the structure */
      d0 = ARG2;
      deref_head(d0, func_var_2unk);
    func_var_2nvar:
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = ARG3;
      deref_head(d1, func_var_3unk);
    func_var_3nvar:
      /* Uuuff, the second and third argument are bound */
      if (IsIntTerm(d1))
        d1 = IntOfTerm(d1);
      else {
        Yap_AsmError(TYPE_ERROR_INTEGER, d1);
        FAIL();
      }
      if (!IsAtomicTerm(d0)) {
        Yap_AsmError(TYPE_ERROR_ATOM,d0);
        FAIL();
      } /* We made it!!!!! we got in d0 the name, in d1 the arity and
         * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
        RESET_VARIABLE(HR);
        RESET_VARIABLE(HR + 1);
        d0 = AbsPair(HR);
        HR += 2;
      } else if ((Int)d1 > 0) {
        /* now let's build a compound term */
        if (!IsAtomTerm(d0)) {
          Yap_AsmError(TYPE_ERROR_ATOM,d0);
          FAIL();
        }
        BEGP(pt1);
        if (!IsAtomTerm(d0)) {
          FAIL();
        } else
          d0 = (CELL)Yap_MkFunctor(AtomOfTerm(d0), (Int)d1);
        pt1 = HR;
        *pt1++ = d0;
        d0 = AbsAppl(HR);
        if (pt1 + d1 > ENV || pt1 + d1 > (CELL *)B) {
          /* make sure we have something to show for our trouble */
          saveregs();
          if (!Yap_gcl((1 + d1) * sizeof(CELL), 3, YREG,
                       NEXTOP(NEXTOP(PREG, e), Osbmp))) {
                           setregs();
            Yap_AsmError(INSTANTIATION_ERROR,d1);
          } else {
            setregs();
          }
          goto restart_functor; /*  */
        }
        while ((Int)d1--) {
          RESET_VARIABLE(pt1);
          pt1++;
        }
        /* done building the term */
        HR = pt1;
        ENDP(pt1);
      } else if ((Int)d1 < 0) {
        Yap_AsmError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(d1));
        FAIL();
      }
      /* else if arity is 0 just pass d0 through */
      /* Ding, ding, we made it */
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e), Osbpp), l);
      YapBind(pt0, d0);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, func_var_3unk, func_var_3nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d1);
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);

      BEGP(pt1);
      deref_body(d0, pt1, func_var_2unk, func_var_2nvar);
      Yap_AsmError(INSTANTIATION_ERROR,d0);
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();
