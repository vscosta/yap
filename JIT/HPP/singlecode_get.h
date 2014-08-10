#define _get_x_var_instinit \
      register CELL d0; \
      d0 = XREG((*_PREG)->u.xx.xr); \
      XREG((*_PREG)->u.xx.xl) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      GONEXT();

#define _get_y_var_instinit \
      register CELL d0; \
      register CELL *pt0; \
      pt0 = YREG + (*_PREG)->u.yx.y; \
      d0 = XREG((*_PREG)->u.yx.x); \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONEXT();

#define _get_yy_var_instinit \
      register CELL d0, d1; \
      register CELL *pt0, *pt1; \
      CACHE_Y(YREG); \
      pt0 = S_YREG + (*_PREG)->u.yyxx.y1; \
      d0 = XREG((*_PREG)->u.yyxx.x1); \
      pt1 = S_YREG + (*_PREG)->u.yyx.y2; \
      d1 = XREG((*_PREG)->u.yyxx.x2); \
      (*_PREG) = NEXTOP((*_PREG), yyxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      INITIALIZE_PERMVAR(pt1,d1); \
      ENDCACHE_Y(); \
      GONEXT();

#define _get_x_val_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xx.xl); \
      deref_head(d0, gvalx_unk); \
 \
    gvalx_nonvar: \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.xx.xr); \
      deref_head(d1, gvalx_nonvar_unk); \
 \
    gvalx_nonvar_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      YAAM_UnifyBound(d0, d1); \
 \
      BEGP(pt0); \
      deref_body(d1, pt0, gvalx_nonvar_unk, gvalx_nonvar_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind(pt0, d0); \
      GONEXT(); \
      ENDP(pt0); \
      ENDD(d1); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, gvalx_unk, gvalx_nonvar); \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.xx.xr); \
      deref_head(d1, gvalx_var_unk); \
 \
    gvalx_var_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind(pt0, d1); \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, gvalx_var_unk, gvalx_var_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      UnifyCells(pt0, pt1); \
      GONEXT(); \
      ENDP(pt1); \
      ENDD(d1); \
      ENDP(pt0); \
      ENDD(d0);

#define _get_y_val_instinit \
      BEGD(d0); \
      BEGD(d1); \
      BEGP(pt0); \
      pt0 = YREG + (*_PREG)->u.yx.y; \
      d0 = *pt0; \
 \
      deref_head(d0, gvaly_unk); \
    gvaly_nonvar: \
      d1 = XREG((*_PREG)->u.yx.x); \
      deref_head(d1, gvaly_nonvar_unk); \
 \
    gvaly_nonvar_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      YAAM_UnifyBound(d0, d1); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, gvaly_nonvar_unk, gvaly_nonvar_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      Bind(pt1, d0); \
      GONEXT(); \
      ENDP(pt1); \
 \
      derefa_body(d0, pt0, gvaly_unk, gvaly_nonvar); \
      d1 = XREG((*_PREG)->u.yx.x); \
      deref_head(d1, gvaly_var_unk); \
    gvaly_var_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      Bind(pt0, d1); \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, gvaly_var_unk, gvaly_var_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      UnifyCells(pt0, pt1); \
      GONEXT(); \
      ENDP(pt1); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define _get_atom_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = XREG((*_PREG)->u.xc.x); \
      d1 = (*_PREG)->u.xc.c; \
 \
      BEGP(pt0); \
      deref_head(d0, gatom_unk); \
    gatom_nonvar: \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), xc); \
	GONEXT(); \
      } \
      else { \
	FAIL(); \
      } \
 \
      deref_body(d0, pt0, gatom_unk, gatom_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), xc); \
      Bind(pt0, d1); \
      GONEXT(); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define _get_2atoms_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
 \
      BEGP(pt0); \
      deref_head(d0, gatom_2unk); \
    gatom_2nonvar: \
      if (d0 == (*_PREG)->u.cc.c1) { \
	goto gatom_2b; \
      } \
      else { \
	FAIL(); \
      } \
 \
      deref_body(d0, pt0, gatom_2unk, gatom_2nonvar); \
      Bind(pt0, (*_PREG)->u.cc.c1); \
      ENDP(pt0); \
    gatom_2b: \
      d0 = ARG2; \
      d1 = (*_PREG)->u.cc.c2; \
 \
      BEGP(pt0); \
      deref_head(d0, gatom_2bunk); \
    gatom_2bnonvar: \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), cc); \
	GONEXT(); \
      } \
      else { \
	FAIL(); \
      } \
 \
      deref_body(d0, pt0, gatom_2bunk, gatom_2bnonvar); \
      (*_PREG) = NEXTOP((*_PREG), cc); \
      Bind(pt0, d1); \
      GONEXT(); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define _get_3atoms_instinit \
      BEGD(d0); \
      BEGD(d1); \
      d0 = ARG1; \
 \
      BEGP(pt0); \
      deref_head(d0, gatom_3unk); \
    gatom_3nonvar: \
      if (d0 == (*_PREG)->u.ccc.c1) { \
	goto gatom_3b; \
      } \
      else { \
	FAIL(); \
      } \
 \
      deref_body(d0, pt0, gatom_3unk, gatom_3nonvar); \
      Bind(pt0, (*_PREG)->u.ccc.c1); \
      ENDP(pt0); \
    gatom_3b: \
      d0 = ARG2; \
 \
      BEGP(pt0); \
      deref_head(d0, gatom_3bunk); \
    gatom_3bnonvar: \
      if (d0 == (*_PREG)->u.ccc.c2) { \
	goto gatom_3c; \
      } \
      else { \
	FAIL(); \
      } \
 \
      deref_body(d0, pt0, gatom_3bunk, gatom_3bnonvar); \
      Bind(pt0, (*_PREG)->u.ccc.c2); \
      ENDP(pt0); \
    gatom_3c: \
      d0 = ARG3; \
      d1 = (*_PREG)->u.ccc.c3; \
 \
      BEGP(pt0); \
      deref_head(d0, gatom_3cunk); \
    gatom_3cnonvar: \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), ccc); \
	GONEXT(); \
      } \
      else { \
	FAIL(); \
      } \
 \
      deref_body(d0, pt0, gatom_3cunk, gatom_3cnonvar); \
      (*_PREG) = NEXTOP((*_PREG), ccc); \
      Bind(pt0, d1); \
      GONEXT(); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define GET_4ATOMS_INSTINIT \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = ARG1;

#define GET_4ATOMS_GATOM_4UNK \
      Bind(pt0, (*_PREG)->u.cccc.c1);

#define GET_4ATOMS_GATOM_4B \
      d0 = ARG2;

#define GET_4ATOMS_GATOM_4BUNK \
      Bind(pt0, (*_PREG)->u.cccc.c2);

#define GET_4ATOMS_GATOM_4C \
      d0 = ARG3;

#define GET_4ATOMS_GATOM_4CUNK \
      Bind(pt0, (*_PREG)->u.cccc.c3);

#define GET_4ATOMS_GATOM_4D \
      d0 = ARG4; \
      d1 = (*_PREG)->u.cccc.c4;

#define GET_4ATOMS_EQUALS \
	(*_PREG) = NEXTOP((*_PREG), cccc); \
	GONext();

#define GET_4ATOMS_GATOM_4DUNK \
      (*_PREG) = NEXTOP((*_PREG), cccc); \
      Bind(pt0, d1); \
      GONext();

#define GET_5ATOMS_INSTINIT \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = ARG1;

#define GET_5ATOMS_GATOM_5UNK \
      Bind(pt0, (*_PREG)->u.ccccc.c1);

#define GET_5ATOMS_GATOM_5B \
      d0 = ARG2;

#define GET_5ATOMS_GATOM_5BUNK \
      Bind(pt0, (*_PREG)->u.ccccc.c2);

#define GET_5ATOMS_GATOM_5C \
      d0 = ARG3;

#define GET_5ATOMS_GATOM_5CUNK \
      Bind(pt0, (*_PREG)->u.ccccc.c3);

#define GET_5ATOMS_GATOM_5D \
      d0 = ARG4;

#define GET_5ATOMS_GATOM_5DUNK \
      Bind(pt0, (*_PREG)->u.ccccc.c4);

#define GET_5ATOMS_GATOM_5E \
      d0 = ARG5; \
      d1 = (*_PREG)->u.ccccc.c5;

#define GET_5ATOMS_EQUALS \
	(*_PREG) = NEXTOP((*_PREG), ccccc); \
	GONext();

#define GET_5ATOMS_GATOM_5EUNK \
      (*_PREG) = NEXTOP((*_PREG), ccccc); \
      Bind(pt0, d1); \
      GONext();

#define GET_6ATOMS_INSTINIT \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = ARG1;

#define GET_6ATOMS_GATOM_6UNK \
      Bind(pt0, (*_PREG)->u.cccccc.c1);

#define GET_6ATOMS_GATOM_6B \
      d0 = ARG2;

#define GET_6ATOMS_GATOM_6BUNK \
      Bind(pt0, (*_PREG)->u.cccccc.c2);

#define GET_6ATOMS_GATOM_6C \
      d0 = ARG3;

#define GET_6ATOMS_GATOM_6CUNK \
      Bind(pt0, (*_PREG)->u.cccccc.c3);

#define GET_6ATOMS_GATOM_6D \
      d0 = ARG4;

#define GET_6ATOMS_GATOM_6DUNK \
      Bind(pt0, (*_PREG)->u.cccccc.c4);

#define GET_6ATOMS_GATOM_6E \
      d0 = ARG5;

#define GET_6ATOMS_GATOM_6EUNK \
      Bind(pt0, (*_PREG)->u.cccccc.c5);

#define GET_6ATOMS_GATOM_6F \
      d0 = ARG6; \
      d1 = (*_PREG)->u.cccccc.c6;

#define GET_6ATOMS_EQUALS \
	(*_PREG) = NEXTOP((*_PREG), cccccc); \
	GONext();

#define GET_6ATOMS_GATOM_6FUNK \
      (*_PREG) = NEXTOP((*_PREG), cccccc); \
      Bind(pt0, d1); \
      GONext();

#define _get_list_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.x.x); \
      deref_head(d0, glist_unk); \
 \
    glist_nonvar: \
      if (!IsPairTerm(d0)) { \
	    FAIL(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), x); \
      (*_SREG) = RepPair(d0); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, glist_unk, glist_nonvar); \
      CACHE_S(); \
      S_SREG = HR; \
      (*_PREG) = NEXTOP((*_PREG), x); \
      BEGD(d0); \
      d0 = AbsPair(S_SREG); \
      Bind(pt0, d0); \
      S_SREG = HR; \
      HR = S_SREG + 2; \
      ENDD(d0); \
      WRITEBACK_S(S_SREG); \
      GONEXTW(); \
      ENDCACHE_S(); \
      ENDP(pt0); \
      ENDD(d0);

#define _get_struct_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xfa.x); \
      deref_head(d0, gstruct_unk); \
 \
    gstruct_nonvar: \
      if (!IsApplTerm(d0)) { \
	FAIL(); \
      } \
      CACHE_S(); \
      S_SREG = RepAppl(d0); \
      d0 = (CELL) ((*_PREG)->u.xfa.f); \
      if (*S_SREG != d0) { \
	FAIL(); \
      } \
      WRITEBACK_S(S_SREG+1); \
      ENDCACHE_S(); \
      (*_PREG) = NEXTOP((*_PREG), xfa); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, gstruct_unk, gstruct_nonvar); \
      BEGD(d1); \
      d1 = AbsAppl(HR); \
      Bind(pt0, d1); \
      pt0 = HR; \
      ENDD(d1); \
      d0 = (CELL) ((*_PREG)->u.xfa.f); \
      *pt0++ = d0; \
      HR = pt0 + (*_PREG)->u.xfa.a; \
      (*_PREG) = NEXTOP((*_PREG), xfa); \
      (*_SREG) = pt0; \
      GONEXTW(); \
      ENDP(pt0); \
      ENDD(d0);

#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
#define _get_float_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xd.x); \
      deref_head(d0, gfloat_unk); \
 \
    gfloat_nonvar: \
      if (!IsApplTerm(d0)) { \
	FAIL(); \
	  } \
      BEGP(pt0); \
      pt0 = RepAppl(d0); \
      if (*pt0 != (CELL)FunctorDouble) { \
	FAIL(); \
      } \
      BEGP(pt1); \
      pt1 = (*_PREG)->u.xd.d; \
      (*_PREG) = NEXTOP((*_PREG), xd); \
      if ( \
	  pt1[1] != pt0[1] \
	  || pt1[2] != pt0[2] \
	  ) { \
	    FAIL(); \
	    } \
      ENDP(pt1); \
      ENDP(pt0); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, gfloat_unk, gfloat_nonvar); \
      BEGD(d1); \
      d1 = AbsAppl((*_PREG)->u.xd.d); \
      (*_PREG) = NEXTOP((*_PREG), xd); \
      Bind(pt0, d1); \
      GONEXT(); \
      ENDD(d1); \
      ENDP(pt0); \
      ENDD(d0);
#else /* SIZEOF_DOUBLE == 2*SIZEOF_INT_P */
#define _get_float_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xd.x); \
      deref_head(d0, gfloat_unk); \
 \
    gfloat_nonvar: \
      if (!IsApplTerm(d0)) { \
	FAIL(); \
	  } \
      BEGP(pt0); \
      pt0 = RepAppl(d0); \
      if (*pt0 != (CELL)FunctorDouble) { \
	FAIL(); \
      } \
      BEGP(pt1); \
      pt1 = (*_PREG)->u.xd.d; \
      (*_PREG) = NEXTOP((*_PREG), xd); \
      if ( \
	  pt1[1] != pt0[1] \
	  ) { \
	    FAIL(); \
	    } \
      ENDP(pt1); \
      ENDP(pt0); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, gfloat_unk, gfloat_nonvar); \
      BEGD(d1); \
      d1 = AbsAppl((*_PREG)->u.xd.d); \
      (*_PREG) = NEXTOP((*_PREG), xd); \
      Bind(pt0, d1); \
      GONEXT(); \
      ENDD(d1); \
      ENDP(pt0); \
      ENDD(d0);
#endif /* SIZEOF_DOUBLE == 2*SIZEOF_INT_P */

#define GET_LONGINT_INSTINIT \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xi.x);

#define GET_LONGINT_GLONGINT_NONVAR_INIT \
      START_PREFETCH(xi); \
      pt0 = RepAppl(d0);

#define GET_LONGINT_GLONGINT_NONVAR_END \
      (*_PREG) = NEXTOP((*_PREG), xi); \
      GONext(); \
      END_PREFETCH();

#define GET_LONGINT_GLONGINT_UNK \
      START_PREFETCH(xi); \
      d1 = AbsAppl((*_PREG)->u.xi.i); \
      (*_PREG) = NEXTOP((*_PREG), xi); \
      Bind(pt0, d1); \
      GONext(); \
      END_PREFETCH();

#ifdef USE_GMP
#define GET_BIGINT_INSTINIT \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xN.x);

#define GET_BIGINT_GBIGINT_NONVAR_INIT \
      START_PREFETCH(xN); \
      pt0 = RepAppl(d0);

#define GET_BIGINT_GBIGINT_NONVAR_END \
      (*_PREG) = NEXTOP((*_PREG), xN); \
      GONext(); \
      END_PREFETCH();

#define GET_BIGINT_GBIGINT_UNK \
      START_PREFETCH(xN); \
      d1 = (*_PREG)->u.xN.b; \
      (*_PREG) = NEXTOP((*_PREG), xN); \
      Bind(pt0, d1); \
      GONext(); \
      END_PREFETCH();
#endif

#define GET_DBTERM_INSTINIT \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xD.x);

#define GET_DBTERM_GDBTERM_NONVAR \
      BLOCK = (CELL)GET_DBTERM_GDBTERM_NONVAR; \
      d1 = (*_PREG)->u.xD.D; \
      (*_PREG) = NEXTOP((*_PREG), xD); \
      YAAM_UNIFYBOUND;

#define GET_DBTERM_GDBTERM_UNK \
      START_PREFETCH(xD); \
      d1 = (*_PREG)->u.xD.D; \
      (*_PREG) = NEXTOP((*_PREG), xD); \
      Bind(pt0, d1); \
      GONext(); \
      END_PREFETCH();

#define _glist_valx_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xx.xl); \
      deref_head(d0, glist_valx_write); \
    glist_valx_read: \
      BEGP(pt0); \
      if (!IsPairTerm(d0)) { \
        FAIL(); \
      } \
      pt0 = RepPair(d0); \
      (*_SREG) = pt0 + 1; \
      d0 = *pt0; \
      deref_head(d0, glist_valx_unk); \
 \
    glist_valx_nonvar: \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.xx.xr); \
      deref_head(d1, glist_valx_nonvar_unk); \
 \
    glist_valx_nonvar_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      YAAM_UnifyBound(d0, d1); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, glist_valx_nonvar_unk, glist_valx_nonvar_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind(pt1, d0); \
      GONEXT(); \
      ENDP(pt1); \
      ENDD(d1); \
 \
      derefa_body(d0, pt0, glist_valx_unk, glist_valx_nonvar); \
      d0 = XREG((*_PREG)->u.xx.xr); \
      deref_head(d0, glist_valx_var_unk); \
 \
    glist_valx_var_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind_Global(pt0, d0); \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d0, pt1, glist_valx_var_unk, glist_valx_var_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONEXT(); \
      ENDP(pt1); \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, glist_valx_write, glist_valx_read); \
      CACHE_S(); \
      S_SREG = HR; \
      BEGD(d1); \
      d1 = XREG((*_PREG)->u.xx.xr); \
      d0 = AbsPair(S_SREG); \
      S_SREG[0] = d1; \
      ENDD(d1); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      HR = S_SREG + 2; \
      WRITEBACK_S(S_SREG+1); \
      Bind(pt0, d0); \
      GONEXTW(); \
      ENDCACHE_S(); \
      ENDP(pt0); \
      ENDD(d0);

#define GLIST_VALY_INSTINIT \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.yx.x);

#define GLIST_VALY_GLIST_VALY_READ \
      if (!IsPairTerm(d0)) { \
	    YAAM_FAIL; \
	  } \
      START_PREFETCH(yx); \
      pt0 = RepPair(d0); \
      (*_SREG) = pt0 + 1; \
      d0 = *pt0;

#define GLIST_VALY_GLIST_VALY_NONVAR \
      pt1 = YREG + (*_PREG)->u.yx.y; \
      d1 = *pt1; \
      (*_PREG) = NEXTOP((*_PREG), yx);

#define GLIST_VALY_GLIST_VALY_NONVAR_NONVAR \
      BLOCK = (CELL)GLIST_VALY_GLIST_VALY_NONVAR_NONVAR; \
      (*_SREG) = pt0 + 1; \
      YAAM_UNIFYBOUND;

#define GLIST_VALY_GLIST_VALY_NONVAR_UNK \
      Bind(pt1, d0); \
      GONext();

#define GLIST_VALY_GLIST_VALY_UNK \
      pt1 = YREG+(*_PREG)->u.yx.y; \
      d1 = *pt1;

#define GLIST_VALY_GLIST_VALY_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      Bind_Global(pt0, d1); \
      GONext();

#define GLIST_VALY_GLIST_VALY_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext(); \
      END_PREFETCH();

#define GLIST_VALY_GLIST_VALY_WRITE \
      START_PREFETCH_W(yx); \
      pt1 = HR; \
      d0 = AbsPair(pt1); \
      Bind(pt0, d0); \
      d0 = YREG[(*_PREG)->u.yx.y]; \
      pt1[0] = d0; \
      HR = pt1 + 2; \
      (*_SREG) = pt1 + 1; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      GONextW(); \
      END_PREFETCH_W();

#define _gl_void_varx_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xx.xl); \
      deref_head(d0, glist_void_varx_write); \
    glist_void_varx_read: \
      if (!IsPairTerm(d0)) { \
	FAIL(); \
	  } \
      BEGP(pt0); \
      pt0 = RepPair(d0); \
      d0 = pt0[1]; \
      XREG((*_PREG)->u.xx.xr) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      GONEXT(); \
      ENDP(pt0); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, glist_void_varx_write, glist_void_varx_read); \
      BEGP(pt1); \
      pt1 = HR; \
      XREG((*_PREG)->u.xx.xr) = \
	Unsigned(pt1 + 1); \
      RESET_VARIABLE(pt1); \
      RESET_VARIABLE(pt1+1); \
      HR = pt1 + 2; \
      BEGD(d0); \
      d0 = AbsPair(pt1); \
      Bind(pt0, d0); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      ENDD(d0); \
      ENDP(pt1); \
      GONEXT(); \
      ENDP(pt0); \
      ENDD(d0);

#define GL_VOID_VARY_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.yx.x);

#define GL_VOID_VARY_GLIST_VOID_VARY_READ \
      BLOCK = (CELL)GL_VOID_VARY_GLIST_VOID_VARY_READ; \
      FAILED = 0; \
      if (!IsPairTerm(d0)) { \
	    YAAM_FAIL; \
	  } \
	  else { \
        pt0 = RepPair(d0); \
        d0 = pt0[1]; \
        INITIALIZE_PERMVAR(YREG+(*_PREG)->u.yx.y,d0); \
        (*_PREG) = NEXTOP((*_PREG), yx); \
        GONext(); \
      }

#define GL_VOID_VARY_GLIST_VOID_VARY_WRITE \
      pt1 = HR; \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.yx.y,Unsigned(pt1 + 1)); \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      RESET_VARIABLE(pt1); \
      RESET_VARIABLE(pt1+1); \
      d0 = AbsPair(pt1); \
      HR = pt1 + 2; \
      Bind(pt0, d0); \
      GONext();

#define GL_VOID_VALX_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.xx.xl);

#define GL_VOID_VALX_GLIST_VOID_VALX_READ \
      BLOCK = (CELL)GL_VOID_VALX_GLIST_VOID_VALX_READ; \
      FAILED = 0; \
      if (!IsPairTerm(d0)) { \
	YAAM_FAIL; \
	  } \
	  else { \
    pt0 = RepPair(d0)+1; \
    d0 = *pt0; \
      }

#define GL_VOID_VALX_GLIST_VOID_VALX_NONVAR \
      d1 = XREG((*_PREG)->u.xx.xr);

#define GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_NONVAR \
      BLOCK = (CELL)GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      YAAM_UNIFYBOUND;

#define GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind(pt1, d0); \
      GONext();

#define GL_VOID_VALX_GLIST_VOID_VALX_UNK \
      d1 = XREG((*_PREG)->u.xx.xr);

#define GL_VOID_VALX_GLIST_VOID_VALX_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind_Global(pt0, d1); \
      GONext();

#define GL_VOID_VALX_GLIST_VOID_VALX_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext();

#define GL_VOID_VALX_GLIST_VOID_VALX_WRITE \
      pt1 = HR; \
      d0 = AbsPair(pt1); \
      Bind(pt0, d0); \
      pt1 = HR; \
      d0 = XREG((*_PREG)->u.xx.xr); \
      RESET_VARIABLE(pt1); \
      pt1[1] = d0; \
      HR = pt1 + 2; \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      GONext();

#define GL_VOID_VALY_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.yx.x);

#define GL_VOID_VALY_GLIST_VOID_VALY_READ \
      BLOCK = (CELL)GL_VOID_VALY_GLIST_VOID_VALY_READ; \
      FAILED = 0; \
      if (!IsPairTerm(d0)) { \
	YAAM_FAIL; \
	  } \
	  else { \
    pt0 = RepPair(d0)+1; \
    d0 = *pt0; \
      }

#define GL_VOID_VALY_GLIST_VOID_VALY_NONVAR \
      pt1 = YREG+(*_PREG)->u.yx.y; \
      d1 = *pt1;

#define GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_NONVAR \
      BLOCK = (CELL)GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      YAAM_UNIFYBOUND;
	  
#define GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      Bind(pt1, d0); \
      GONext();

#define GL_VOID_VALY_GLIST_VOID_VALY_UNK \
      pt1 = YREG+(*_PREG)->u.yx.y; \
      d1 = *pt1;

#define GL_VOID_VALY_GLIST_VOID_VALY_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      Bind_Global(pt0, d1); \
      GONext();
	  
#define GL_VOID_VALY_GLIST_VOID_VALY_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext();

#define GL_VOID_VALY_GLIST_VOID_VALY_WRITE \
      CACHE_S(); \
      S_SREG = HR; \
      d0 = AbsPair(S_SREG); \
      Bind(pt0, d0); \
      S_SREG = HR; \
      d1 = YREG[(*_PREG)->u.yx.y]; \
      RESET_VARIABLE(S_SREG); \
      S_SREG[1] = d1; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      HR = S_SREG + 2; \
      ENDCACHE_S(); \
      GONext();

