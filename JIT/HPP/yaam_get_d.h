#define GET_X_VAR_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = XREG((*_PREG)->u.xx.xr); \
      XREG((*_PREG)->u.xx.xl) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      GONext();

#define GET_Y_VAR_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      register CELL *pt0; \
      pt0 = YREG + (*_PREG)->u.yx.y; \
      d0 = XREG((*_PREG)->u.yx.x); \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define GET_YY_VAR_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
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
      GONext();

#define GET_X_VAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.xx.xl);

#define GET_X_VAL_GVALX_NONVAR \
      d1 = XREG((*_PREG)->u.xx.xr);

#define GET_X_VAL_GVALX_NONVAR_NONVAR \
      BLOCK = (CELL)GET_X_VAL_GVALX_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      YAAM_UNIFYBOUND;

#define GET_X_VAL_GVALX_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind(pt0, d0); \
      GONext();

#define GET_X_VAL_GVALX_UNK \
      d1 = XREG((*_PREG)->u.xx.xr);

#define GET_X_VAL_GVALX_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind(pt0, d1); \
      GONext();

#define GET_X_VAL_GVALX_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      UnifyCells(pt0, pt1); \
      GONext();

#define GET_Y_VAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = YREG + (*_PREG)->u.yx.y; \
      d0 = *pt0;

#define GET_Y_VAL_GVALY_NONVAR \
      d1 = XREG((*_PREG)->u.yx.x);

#define GET_Y_VAL_GVALY_NONVAR_NONVAR \
      BLOCK = (CELL)GET_Y_VAL_GVALY_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      YAAM_UNIFYBOUND;

#define GET_Y_VAL_GVALY_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      Bind(pt1, d0); \
      GONext();

#define GET_Y_VAL_GVALY_UNK \
      d1 = XREG((*_PREG)->u.yx.x);

#define GET_Y_VAL_GVALY_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      Bind(pt0, d1); \
      GONext();

#define GET_Y_VAL_GVALY_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      UnifyCells(pt0, pt1); \
      GONext();

#define GET_ATOM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xc.x); \
      d1 = (*_PREG)->u.xc.c;

#define GET_ATOM_GATOM_NONVAR \
      BLOCK = (CELL)GET_ATOM_GATOM_NONVAR; \
      FAILED = 0; \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), xc); \
	GONext(); \
      } \
      else { \
	YAAM_FAIL; \
      }

#define GET_ATOM_GATOM_UNK \
      (*_PREG) = NEXTOP((*_PREG), xc); \
      Bind(pt0, d1); \
      GONext();

#define GET_2ATOMS_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = ARG1;

#define GET_2ATOMS_GATOM_2UNK \
      Bind(pt0, (*_PREG)->u.cc.c1);

#define GET_2ATOMS_GATOM_2B \
      d0 = ARG2; \
      d1 = (*_PREG)->u.cc.c2;

#define GET_2ATOMS_GATOM_2BNONVAR \
      BLOCK = (CELL)GET_2ATOMS_GATOM_2BNONVAR; \
      FAILED = 0; \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), cc); \
	GONext(); \
      } \
      else { \
	YAAM_FAIL; \
      }

#define GET_2ATOMS_GATOM_2BUNK \
      (*_PREG) = NEXTOP((*_PREG), cc); \
      Bind(pt0, d1); \
      GONext();

#define GET_3ATOMS_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = ARG1;

#define GET_3ATOMS_GATOM_3UNK \
      Bind(pt0, (*_PREG)->u.ccc.c1);

#define GET_3ATOMS_GATOM_3B \
      d0 = ARG2;

#define GET_3ATOMS_GATOM_3BUNK \
      Bind(pt0, (*_PREG)->u.ccc.c2);

#define GET_3ATOMS_GATOM_3C \
      d0 = ARG3; \
      d1 = (*_PREG)->u.ccc.c3;

#define GET_3ATOMS_GATOM_3CNONVAR \
      BLOCK = (CELL)GET_3ATOMS_GATOM_3CNONVAR; \
      FAILED = 0; \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), ccc); \
	GONext(); \
      } \
      else { \
	YAAM_FAIL; \
      }

#define GET_3ATOMS_GATOM_3CUNK \
      (*_PREG) = NEXTOP((*_PREG), ccc); \
      Bind(pt0, d1); \
      GONext();

#define GET_4ATOMS_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
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

#define GET_4ATOMS_GATOM_4DNONVAR \
      BLOCK = (CELL)GET_4ATOMS_GATOM_4DNONVAR; \
      FAILED = 0; \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), cccc); \
	GONext(); \
      } \
      else { \
	YAAM_FAIL; \
      }

#define GET_4ATOMS_GATOM_4DUNK \
      (*_PREG) = NEXTOP((*_PREG), cccc); \
      Bind(pt0, d1); \
      GONext();

#define GET_5ATOMS_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
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

#define GET_5ATOMS_GATOM_5ENONVAR \
      BLOCK = (CELL)GET_5ATOMS_GATOM_5ENONVAR; \
      FAILED = 0; \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), ccccc); \
	GONext(); \
      } \
      else { \
	YAAM_FAIL; \
      }

#define GET_5ATOMS_GATOM_5EUNK \
      (*_PREG) = NEXTOP((*_PREG), ccccc); \
      Bind(pt0, d1); \
      GONext();

#define GET_6ATOMS_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
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

#define GET_6ATOMS_GATOM_6FNONVAR \
      BLOCK = (CELL)GET_6ATOMS_GATOM_6FNONVAR; \
      FAILED = 0; \
      if (d0 == d1) { \
	(*_PREG) = NEXTOP((*_PREG), cccccc); \
	GONext(); \
      } \
      else { \
	YAAM_FAIL; \
      }

#define GET_6ATOMS_GATOM_6FUNK \
      (*_PREG) = NEXTOP((*_PREG), cccccc); \
      Bind(pt0, d1); \
      GONext();

#define GET_LIST_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.x.x);

#define GET_LIST_GLIST_NONVAR \
      BLOCK = (CELL)GET_LIST_GLIST_NONVAR; \
      FAILED = 0; \
      if (!IsPairTerm(d0)) { \
	    YAAM_FAIL; \
      } \
	  else { \
        (*_PREG) = NEXTOP((*_PREG), x); \
        (*_SREG) = RepPair(d0); \
        GONext(); \
	  }

#define GET_LIST_GLIST_UNK \
      CACHE_S(); \
      S_SREG = HR; \
      START_PREFETCH_W(x); \
      (*_PREG) = NEXTOP((*_PREG), x); \
      d0 = AbsPair(S_SREG); \
      Bind(pt0, d0); \
      S_SREG = HR; \
      HR = S_SREG + 2; \
      WRITEBACK_S(S_SREG); \
      GONextW(); \
      END_PREFETCH_W(); \
      ENDCACHE_S();

#define GET_STRUCT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xfa.x);

#define GET_STRUCT_GSTRUCT_NONVAR \
      BLOCK = (CELL)GET_STRUCT_GSTRUCT_NONVAR; \
      FAILED = 0; \
      if (!IsApplTerm(d0)) { \
	YAAM_FAIL; \
      } \
	  else { \
        register CELL * S_SREG; \
        S_SREG = RepAppl(d0); \
        d0 = (CELL) ((*_PREG)->u.xfa.f); \
        if (*S_SREG != d0) { \
	  YAAM_FAIL; \
        } \
		else { \
          WRITEBACK_S(S_SREG+1); \
          (*_PREG) = NEXTOP((*_PREG), xfa); \
          GONext(); \
		} \
	  }

#define GET_STRUCT_GSTRUCT_UNK \
      START_PREFETCH_W(xfa); \
      d1 = AbsAppl(HR); \
      Bind(pt0, d1); \
      pt0 = HR; \
      d0 = (CELL) ((*_PREG)->u.xfa.f); \
      *pt0++ = d0; \
      HR = pt0 + (*_PREG)->u.xfa.a; \
      (*_PREG) = NEXTOP((*_PREG), xfa); \
      (*_SREG) = pt0; \
      GONextW(); \
      END_PREFETCH_W();

#define GET_FLOAT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.xd.x);

#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
#define GET_FLOAT_GFLOAT_NONVAR \
      BLOCK = (CELL)GET_FLOAT_GFLOAT_NONVAR; \
      FAILED = 0; \
      if (!IsApplTerm(d0)) { \
	YAAM_FAIL; \
	  } \
	  else { \
        pt0 = RepAppl(d0); \
        if (*pt0 != (CELL)FunctorDouble) { \
	  YAAM_FAIL; \
        } \
		else { \
          pt1 = (*_PREG)->u.xd.d; \
          (*_PREG) = NEXTOP((*_PREG), xd); \
          if ( \
	      pt1[1] != pt0[1] \
		  || pt1[2] != pt0[2] \
	      ) { \
	        YAAM_FAIL; \
	        } \
		  else { \
            GONext(); \
		  } \
		} \
	  }
#else /* SIZEOF_DOUBLE == 2*SIZEOF_INT_P */
#define GET_FLOAT_GFLOAT_NONVAR \
      BLOCK = (CELL)GET_FLOAT_GFLOAT_NONVAR; \
      FAILED = 0; \
      if (!IsApplTerm(d0)) { \
	YAAM_FAIL; \
	  } \
	  else { \
        pt0 = RepAppl(d0); \
        if (*pt0 != (CELL)FunctorDouble) { \
	  YAAM_FAIL; \
        } \
		else { \
          pt1 = (*_PREG)->u.xd.d; \
          (*_PREG) = NEXTOP((*_PREG), xd); \
          if ( \
	      pt1[1] != pt0[1] \
	      ) { \
	        YAAM_FAIL; \
	        } \
		  else { \
            GONext(); \
		  } \
		} \
	  }
#endif /* SIZEOF_DOUBLE == 2*SIZEOF_INT_P */

#define GET_FLOAT_GFLOAT_UNK \
      START_PREFETCH(xc); \
      d1 = AbsAppl((*_PREG)->u.xd.d); \
      (*_PREG) = NEXTOP((*_PREG), xd); \
      Bind(pt0, d1); \
      GONext(); \
      END_PREFETCH();

#define GET_LONGINT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xi.x);

#define GET_LONGINT_GLONGINT_NONVAR \
      BLOCK = (CELL)GET_LONGINT_GLONGINT_NONVAR; \
      FAILED = 0; \
      if (!IsApplTerm(d0)) { \
	YAAM_FAIL; \
	  } \
	  else { \
        pt0 = RepAppl(d0); \
        if (*pt0 != (CELL)FunctorLongInt) { \
	      YAAM_FAIL; \
        } \
        if ((*_PREG)->u.xi.i[1] != (CELL)pt0[1]) { \
	      YAAM_FAIL; \
	    } \
	  } \
	  if (!FAILED) { \
        (*_PREG) = NEXTOP((*_PREG), xi); \
        GONext(); \
	  }

#define GET_LONGINT_GLONGINT_UNK \
      START_PREFETCH(xi); \
      d1 = AbsAppl((*_PREG)->u.xi.i); \
      (*_PREG) = NEXTOP((*_PREG), xi); \
      Bind(pt0, d1); \
      GONext(); \
      END_PREFETCH();

#ifdef USE_GMP
#define GET_BIGINT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xN.x);

#define GET_BIGINT_GBIGINT_NONVAR \
      BLOCK = (CELL)GET_BIGINT_GBIGINT_NONVAR; \
      FAILED = 0; \
      if (!IsApplTerm(d0)) { \
	YAAM_FAIL; \
	  } \
	  else { \
        pt0 = RepAppl(d0); \
        if (*pt0 != (CELL)FunctorBigInt) { \
	      YAAM_FAIL; \
	    } \
        if (Yap_gmp_tcmp_big_big(d0,(*_PREG)->u.xN.b)) { \
	      YAAM_FAIL; \
	    } \
	  } \
	  if (!FAILED) { \
        (*_PREG) = NEXTOP((*_PREG), xN); \
        GONext(); \
	  }

#define GET_BIGINT_GBIGINT_UNK \
      START_PREFETCH(xN); \
      d1 = (*_PREG)->u.xN.b; \
      (*_PREG) = NEXTOP((*_PREG), xN); \
      Bind(pt0, d1); \
      GONext(); \
      END_PREFETCH();
#endif

#define GET_DBTERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
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

#define GLIST_VALX_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.xx.xl); \
      if (!IsVarTerm(d0)) { \
        printf("Oops!\n"); \
        exit(1); \
      }

#define GLIST_VALX_GLIST_VALX_READ \
      BLOCK = (CELL)GLIST_VALX_GLIST_VALX_READ; \
      FAILED = 0; \
      if (!IsPairTerm(d0)) { \
        YAAM_FAIL; \
      } \
      else { \
         pt0 = RepPair(d0); \
        (*_SREG) = pt0 + 1; \
        d0 = *pt0; \
      }

#define GLIST_VALX_GLIST_VALX_NONVAR \
      d1 = XREG((*_PREG)->u.xx.xr);

#define GLIST_VALX_GLIST_VALX_NONVAR_NONVAR \
      BLOCK = (CELL)GLIST_VALX_GLIST_VALX_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      YAAM_UNIFYBOUND;

#define GLIST_VALX_GLIST_VALX_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind(pt1, d0); \
      GONext();

#define GLIST_VALX_GLIST_VALX_UNK \
      d0 = XREG((*_PREG)->u.xx.xr);

#define GLIST_VALX_GLIST_VALX_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      Bind_Global(pt0, d0); \
      GONext();

#define GLIST_VALX_GLIST_VALX_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext(); \

#define GLIST_VALX_GLIST_VALX_WRITE \
      CACHE_S(); \
      S_SREG = HR; \
      d1 = XREG((*_PREG)->u.xx.xr); \
      d0 = AbsPair(S_SREG); \
      S_SREG[0] = d1; \
      ALWAYS_START_PREFETCH_W(xx); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      HR = S_SREG + 2; \
      WRITEBACK_S(S_SREG+1); \
      Bind(pt0, d0); \
      ALWAYS_GONextW(); \
      ALWAYS_END_PREFETCH_W(); \
      ENDCACHE_S();

#define GLIST_VALY_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
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

#define GL_VOID_VARX_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.xx.xl);

#define GL_VOID_VARX_GLIST_VOID_VARX_READ \
      BLOCK = (CELL)GL_VOID_VARX_GLIST_VOID_VARX_READ; \
      FAILED = 0; \
      if (!IsPairTerm(d0)) { \
	YAAM_FAIL; \
	  } \
	  else { \
    pt0 = RepPair(d0); \
    d0 = pt0[1]; \
    XREG((*_PREG)->u.xx.xr) = d0; \
    (*_PREG) = NEXTOP((*_PREG), xx); \
    ALWAYS_GONext(); \
	  }

#define GL_VOID_VARX_GLIST_VOID_VAR_WRITE \
      pt1 = HR; \
      XREG((*_PREG)->u.xx.xr) = \
	Unsigned(pt1 + 1); \
      RESET_VARIABLE(pt1); \
      RESET_VARIABLE(pt1+1); \
      HR = pt1 + 2; \
      d0 = AbsPair(pt1); \
      Bind(pt0, d0); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      GONext();

#define GL_VOID_VARY_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
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
      print_instruction((*_PREG), ON_NATIVE); \
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
      print_instruction((*_PREG), ON_NATIVE); \
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

