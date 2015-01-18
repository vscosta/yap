#define UNIFY_X_VAR_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
      CACHE_S(); \
      READ_IN_S(); \
      d0 = *S_SREG;
	  
#ifdef YAPOR_SBA
#define UNIFY_X_VAR_YAPOR_SBA \
      if (d0 == 0) { \
	d0 = (CELL)S_SREG; \
	}
#endif

#define UNIFY_X_VAR_END \
      WRITEBACK_S(S_SREG+1); \
      ALWAYS_START_PREFETCH(ox); \
      XREG((*_PREG)->u.ox.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      ALWAYS_GONext(); \
      ALWAYS_END_PREFETCH(); \
      ENDCACHE_S();

#define UNIFY_X_VAR_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL *pt0; \
      CACHE_S(); \
      READ_IN_S(); \
      pt0 = &XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      RESET_VARIABLE(S_SREG); \
      *pt0 = (CELL) S_SREG; \
      WRITEBACK_S(S_SREG+1); \
      ENDCACHE_S(); \
      GONextW();

#define UNIFY_L_X_VAR_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL *pt0; \
      ALWAYS_START_PREFETCH(ox); \
      d0 = (*_SREG)[0]; \
      pt0 = &XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox);
	  
#ifdef YAPOR_SBA
#define UNIFY_L_X_VAR_YAPOR_SBA \
      if (d0 == 0) { \
	d0 = (CELL)(*_SREG); \
      }
#endif

#define UNIFY_L_X_VAR_END \
      *pt0 = d0; \
      ALWAYS_GONext(); \
      ALWAYS_END_PREFETCH();

#define UNIFY_L_X_VAR_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL *pt0; \
      ALWAYS_START_PREFETCH(ox); \
      CACHE_S(); \
      READ_IN_S(); \
      pt0 = &XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      RESET_VARIABLE(S_SREG); \
      *pt0 = (CELL)S_SREG; \
      ENDCACHE_S(); \
      ALWAYS_GONext(); \
      ALWAYS_END_PREFETCH();

#define UNIFY_X_VAR2_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      CACHE_S(); \
      ALWAYS_START_PREFETCH(oxx); \
      READ_IN_S(); \
      pt0 = &XREG((*_PREG)->u.oxx.xr); \
      d0 = S_SREG[0]; \
      d1 = S_SREG[1];
	  
#ifdef YAPOR_SBA
#define UNIFY_X_VAR2_YAPOR_SBA \
      if (d0 == 0) { \
	d0 = (CELL)S_SREG; \
	} \
      if (d1 == 0) { \
	d1 = (CELL)(S_SREG+1); \
	}
#endif

#define UNIFY_X_VAR2_END \
      WRITEBACK_S(S_SREG+2); \
      XREG((*_PREG)->u.oxx.xl) = d0; \
      (*_PREG) = NEXTOP((*_PREG), oxx); \
      *pt0 = d1; \
      ALWAYS_GONext(); \
      ALWAYS_END_PREFETCH(); \
      ENDCACHE_S();

#define UNIFY_X_VAR2_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL *pt0; \
	  CACHE_S(); \
      READ_IN_S(); \
      pt0 = &XREG((*_PREG)->u.oxx.xr); \
      RESET_VARIABLE(S_SREG); \
      XREG((*_PREG)->u.oxx.xl) = (CELL) S_SREG; \
      S_SREG++; \
      (*_PREG) = NEXTOP((*_PREG), oxx); \
      RESET_VARIABLE(S_SREG); \
      *pt0 = (CELL) S_SREG; \
      WRITEBACK_S(S_SREG+1); \
      ENDCACHE_S(); \
      GONextW();

#ifdef YAPOR_SBA
#define UNIFY_L_X_VAR2_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0, d1; \
      register CELL *pt0; \
      CACHE_S(); \
      READ_IN_S(); \
      pt0 = &XREG((*_PREG)->u.oxx.xr); \
      d0 = S_SREG[0]; \
      d1 = S_SREG[1]; \
      if (d0 == 0) \
	XREG((*_PREG)->u.oxx.xl) = (CELL)S_SREG; \
      else \
	XREG((*_PREG)->u.oxx.xl) = d0; \
      (*_PREG) = NEXTOP((*_PREG), oxx); \
      if (d1 == 0) \
	*pt0 = (CELL)(S_SREG+1); \
      else \
	*pt0 = d1; \
      ENDCACHE_S(); \
      ALWAYS_GONext();
#else /* YAPOR_SBA */
#define UNIFY_L_X_VAR2_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
     register CELL d0, d1; \
      register CELL *pt0; \
      CACHE_S(); \
      READ_IN_S(); \
      pt0 = &XREG((*_PREG)->u.oxx.xr); \
      d0 = S_SREG[0]; \
      d1 = S_SREG[1]; \
      { \
	XREG((*_PREG)->u.oxx.xl) = d0; \
	  } \
      (*_PREG) = NEXTOP((*_PREG), oxx); \
      { \
	*pt0 = d1; \
	  } \
      ENDCACHE_S(); \
      ALWAYS_GONext();
#endif /* YAPOR_SBA */

#define UNIFY_L_X_VAR2_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL *pt0; \
      CACHE_S(); \
      READ_IN_S(); \
      pt0 = &XREG((*_PREG)->u.oxx.xr); \
      XREG((*_PREG)->u.oxx.xl) = (CELL) S_SREG; \
      RESET_VARIABLE(S_SREG); \
      S_SREG++; \
      *pt0 = (CELL) S_SREG; \
      (*_PREG) = NEXTOP((*_PREG), oxx); \
      RESET_VARIABLE(S_SREG); \
      ENDCACHE_S(); \
      GONext();

#ifdef YAPOR_SBA
#define UNIFY_Y_VAR_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = *(*_SREG)++; \
      if (d0 == 0) { \
	INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,(CELL)((*_SREG)-1)); \
      } else \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();
#else /* YAPOR_SBA */
#define UNIFY_Y_VAR_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = *(*_SREG)++; \
	INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,d0); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();
#endif /* YAPOR_SBA */

#define UNIFY_Y_VAR_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      CACHE_S(); \
      READ_IN_S(); \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,(CELL) S_SREG); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      RESET_VARIABLE(S_SREG); \
      WRITEBACK_S(S_SREG+1); \
      ENDCACHE_S(); \
      GONextW();

#ifdef YAPOR_SBA
#define UNIFY_L_Y_VAR_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = (*_SREG)[0]; \
      if (d0 == 0) { \
	INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,(CELL)(*_SREG)); \
      } else \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();
#else /* YAPOR_SBA */
#define UNIFY_L_Y_VAR_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = (*_SREG)[0]; \
	INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,d0); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();
#endif /* YAPOR_SBA */

#define UNIFY_L_Y_VAR_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      CACHE_S(); \
      READ_IN_S(); \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,(CELL) S_SREG); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      RESET_VARIABLE(S_SREG); \
      ENDCACHE_S(); \
      GONext();

#define UNIFY_X_VAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_X_VAL_UVALX_NONVAR \
      d1 = XREG((*_PREG)->u.ox.x);

#define UNIFY_X_VAL_UVALX_NONVAR_NONVAR \
      BLOCK = (CELL)UNIFY_X_VAL_UVALX_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      YAAM_UNIFYBOUND;

#define UNIFY_X_VAL_UVALX_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      Bind(pt1, d0); \
      GONext();

#define UNIFY_X_VAL_UVALX_UNK \
      d1 = XREG((*_PREG)->u.ox.x);

#define UNIFY_X_VAL_UVALX_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_X_VAL_UVALX_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext();

#define UNIFY_X_VAL_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      *(*_SREG)++ = XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONextW();

#define UNIFY_L_X_VAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_L_X_VAL_ULVALX_NONVAR \
      d1 = XREG((*_PREG)->u.ox.x);

#define UNIFY_L_X_VAL_ULVALX_NONVAR_NONVAR \
      BLOCK = (CELL)UNIFY_L_X_VAL_ULVALX_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      YAAM_UNIFYBOUND;

#define UNIFY_L_X_VAL_ULVALX_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      Bind(pt1, d0); \
      GONext();

#define UNIFY_L_X_VAL_ULVALX_UNK \
      d1 = XREG((*_PREG)->u.ox.x);

#define UNIFY_L_X_VAL_ULVALX_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_L_X_VAL_ULVALX_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext();

#define UNIFY_L_X_VAL_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_SREG)[0] = XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONext();

#define UNIFY_Y_VAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG); \
      d0 = *pt0;
	
#define UNIFY_Y_VAL_UVALY_NONVAR \
      pt1 = YREG+(*_PREG)->u.oy.y; \
      d1 = *pt1;

#define UNIFY_Y_VAL_UVALY_NONVAR_NONVAR \
      BLOCK = (CELL)UNIFY_Y_VAL_UVALY_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      (*_SREG)++; \
      YAAM_UNIFYBOUND;

#define UNIFY_Y_VAL_UVALY_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      (*_SREG)++; \
      Bind(pt1, d0); \
      GONext();
	  
#define UNIFY_Y_VAL_UVALY_UNK \
      pt1 = YREG+(*_PREG)->u.oy.y; \
      d1 = *pt1;

#define UNIFY_Y_VAL_UVALY_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      (*_SREG)++; \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_Y_VAL_UVALY_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      (*_SREG)++; \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext();

#ifdef YAPOR_SBA
#define UNIFY_Y_VAL_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.oy.y]; \
      if (d0 == 0) \
	*(*_SREG)++ = (CELL)(YREG+(*_PREG)->u.oy.y); \
      else \
	*(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONextW();
#else /* YAPOR_SBA */
#define UNIFY_Y_VAL_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0;  \
      d0 = YREG[(*_PREG)->u.oy.y]; \
	*(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONextW();
#endif /* YAPOR_SBA */

#define UNIFY_L_Y_VAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_L_Y_VAL_ULVALY_NONVAR \
      pt1 = YREG+(*_PREG)->u.oy.y; \
      d1 = *pt1;

#define UNIFY_L_Y_VAL_ULVALY_NONVAR_NONVAR \
      BLOCK = (CELL)UNIFY_L_Y_VAL_ULVALY_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      YAAM_UNIFYBOUND;

#define UNIFY_L_Y_VAL_ULVALY_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      Bind(pt1, d0); \
      GONext();

#define UNIFY_L_Y_VAL_ULVALY_UNK \
      pt1 = YREG+(*_PREG)->u.oy.y; \
      d1 = *pt1;

#define UNIFY_L_Y_VAL_ULVALY_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_L_Y_VAL_ULVALY_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext();

#ifdef YAPOR_SBA
#define UNIFY_L_Y_VAL_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.oy.y]; \
      if (d0 == 0) \
	(*_SREG)[0] = (CELL)(YREG+(*_PREG)->u.oy.y); \
      else \
	(*_SREG)[0] = d0; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();
#else /* YAPOR_SBA */
#define UNIFY_L_Y_VAL_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.oy.y]; \
	(*_SREG)[0] = d0; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();
#endif /* YAPOR_SBA */
	  
#define UNIFY_X_LOC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_X_LOC_UVALX_LOC_NONVAR \
      d1 = XREG((*_PREG)->u.ox.x);

#define UNIFY_X_LOC_UVALX_LOC_NONVAR_NONVAR \
      BLOCK = (CELL)UNIFY_X_LOC_UVALX_LOC_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      YAAM_UNIFYBOUND;

#define UNIFY_X_LOC_UVALX_LOC_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      Bind(pt1, d0); \
      GONext();

#define UNIFY_X_LOC_UVALX_LOC_UNK \
      d1 = XREG((*_PREG)->u.ox.x);

#define UNIFY_X_LOC_UVALX_LOC_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_X_LOC_UVALX_LOC_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      UnifyCells(pt0, pt1); \
      GONext();

#define UNIFY_X_LOC_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.ox.x);

#define UNIFY_X_LOC_WRITE_UNIFY_X_LOC_NONVAR \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONextW();

#define UNIFY_X_LOC_WRITE_UNIFY_X_LOC_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      if (pt0 < HR) { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONextW(); \
      } \
      else { \
	CACHE_S(); \
	READ_IN_S(); \
	Bind_Local(pt0, Unsigned(S_SREG)); \
	RESET_VARIABLE(S_SREG); \
	WRITEBACK_S(S_SREG+1); \
	ENDCACHE_S(); \
	GONextW(); \
      }

#define UNIFY_L_X_LOC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_L_X_LOC_ULVALX_LOC_NONVAR \
      d1 = XREG((*_PREG)->u.ox.x);

#define UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_NONVAR \
      BLOCK = (CELL)UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      YAAM_UNIFYBOUND;

#define UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      Bind(pt0, d0); \
      GONext();

#define UNIFY_L_X_LOC_ULVALX_LOC_UNK \
      d1 = XREG((*_PREG)->u.ox.x);

#define UNIFY_L_X_LOC_ULVALX_LOC_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_L_X_LOC_ULVALX_LOC_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext();

#define UNIFY_L_X_LOC_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.ox.x);

#define UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_NONVAR \
      (*_SREG)[0] = d0; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONext();

#define UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_UNK \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      if (pt0 < HR) { \
	(*_SREG)[0] = Unsigned(pt0); \
	GONext(); \
      } \
      else { \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	GONext(); \
      }

#define UNIFY_Y_LOC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_Y_LOC_UVALY_LOC_NONVAR \
      pt1 =  YREG+(*_PREG)->u.oy.y; \
      d1 = *pt1;

#define UNIFY_Y_LOC_UVALY_LOC_NONVAR_NONVAR \
      BLOCK = (CELL)UNIFY_Y_LOC_UVALY_LOC_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      (*_SREG)++; \
      YAAM_UNIFYBOUND;

#define UNIFY_Y_LOC_UVALY_LOC_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      (*_SREG)++; \
      Bind(pt1, d0); \
      GONext();

#define UNIFY_Y_LOC_UVALY_LOC_UNK \
      pt1 = YREG+(*_PREG)->u.oy.y; \
      d1 = *pt1;

#define UNIFY_Y_LOC_UVALY_LOC_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      (*_SREG)++; \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_Y_LOC_UVALY_LOC_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      (*_SREG)++; \
      UnifyCells(pt0, pt1); \
      GONext();

#define UNIFY_Y_LOC_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL *pt0; \
      pt0 = YREG+(*_PREG)->u.oy.y; \
      d0 = *pt0;

#define UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_NONVAR \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONextW();

#define UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      if (pt0 < HR) { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONextW(); \
      } \
      else { \
	CACHE_S(); \
	READ_IN_S(); \
	Bind_Local(pt0, Unsigned(S_SREG)); \
	RESET_VARIABLE(S_SREG); \
	WRITEBACK_S(S_SREG+1); \
	ENDCACHE_S(); \
	GONextW(); \
      }

#define UNIFY_L_Y_LOC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR \
      pt1 = YREG+(*_PREG)->u.oy.y; \
      d1 = *pt1;

#define UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_NONVAR \
      BLOCK = (CELL)UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_NONVAR; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      YAAM_UNIFYBOUND;
	  
#define UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      Bind(pt1, d0); \
      GONext();

#define UNIFY_L_Y_LOC_ULVALY_LOC_UNK \
      pt1 = YREG+(*_PREG)->u.oy.y; \
      d1 = *pt1;

#define UNIFY_L_Y_LOC_ULVALY_LOC_VAR_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      Bind_Global(pt0, d1); \
      GONext();
	  
#define UNIFY_L_Y_LOC_ULVALY_LOC_VAR_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONext();

#define UNIFY_L_Y_LOC_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL *pt0; \
      pt0 = YREG+(*_PREG)->u.oy.y; \
      d0 = *pt0;

#define UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_NONVAR \
      (*_SREG)[0] = d0; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();

#define UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_UNK \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      if (pt0 < HR) { \
	(*_SREG)[0] = Unsigned(pt0); \
	GONext(); \
      } \
      else { \
	CACHE_S(); \
	READ_IN_S(); \
	Bind_Local(pt0, Unsigned(S_SREG)); \
	RESET_VARIABLE(S_SREG); \
	ENDCACHE_S(); \
	GONext(); \
      }

#define UNIFY_VOID_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      START_PREFETCH(o); \
      (*_PREG) = NEXTOP((*_PREG), o); \
      (*_SREG)++; \
      GONext(); \
      END_PREFETCH();

#define UNIFY_VOID_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      CACHE_S(); \
      READ_IN_S(); \
      (*_PREG) = NEXTOP((*_PREG), o); \
      RESET_VARIABLE(S_SREG); \
      WRITEBACK_S(S_SREG+1); \
      ENDCACHE_S(); \
      GONextW();

#define UNIFY_L_VOID_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_PREG) = NEXTOP((*_PREG), o); \
      GONext();

#define UNIFY_L_VOID_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_PREG) = NEXTOP((*_PREG), o); \
      RESET_VARIABLE((*_SREG)); \
      GONext();

#define UNIFY_N_VOIDS_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_SREG) += (*_PREG)->u.os.s; \
      (*_PREG) = NEXTOP((*_PREG), os); \
      GONext();

#define UNIFY_N_VOIDS_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      CACHE_S(); \
      d0 = (*_PREG)->u.os.s; \
      READ_IN_S(); \
      (*_PREG) = NEXTOP((*_PREG), os); \
      for (; d0 > 0; d0--) { \
	RESET_VARIABLE(S_SREG); \
	S_SREG++; \
      } \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      GONextW();

#define UNIFY_L_N_VOIDS_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_PREG) = NEXTOP((*_PREG), os); \
      GONext();

#define UNIFY_L_N_VOIDS_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = (*_PREG)->u.os.s; \
      (*_PREG) = NEXTOP((*_PREG), os); \
      CACHE_S(); \
      READ_IN_S(); \
      for (; d0 > 0; d0--) { \
	RESET_VARIABLE(S_SREG); \
	S_SREG++; \
      } \
      ENDCACHE_S(); \
      GONext();

#define UNIFY_ATOM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
      register CELL d0; \
	  register CELL *pt0; \
      pt0 = (*_SREG)++; \
      d0 = *pt0;

#define UNIFY_ATOM_UATOM_NONVAR \
      BLOCK = (CELL)UNIFY_ATOM_UATOM_NONVAR; \
      FAILED = 0; \
      if (d0 != (*_PREG)->u.oc.c) { \
	YAAM_FAIL; \
      } \
	  else { \
        (*_PREG) = NEXTOP((*_PREG), oc); \
        GONext(); \
	  }

#define UNIFY_ATOM_UATOM_UNK \
      d0 = (*_PREG)->u.oc.c; \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      Bind_Global(pt0, d0); \
      GONext();

#define UNIFY_ATOM_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      * (*_SREG)++ = (*_PREG)->u.oc.c; \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      GONextW();

#define UNIFY_L_ATOM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
      register CELL d0; \
	  register CELL *pt0; \
      pt0 = (*_SREG); \
      d0 = *(*_SREG);

#define UNIFY_L_ATOM_ULATOM_NONVAR \
      BLOCK = (CELL)UNIFY_L_ATOM_ULATOM_NONVAR; \
      FAILED = 0; \
      if (d0 != (*_PREG)->u.oc.c) { \
	YAAM_FAIL; \
      } \
	  else { \
    (*_PREG) = NEXTOP((*_PREG), oc); \
    GONext(); \
	  }
	  
#define UNIFY_L_ATOM_ULATOM_UNK \
      d0 = (*_PREG)->u.oc.c; \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      Bind_Global(pt0, d0); \
      GONext();

#define UNIFY_L_ATOM_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_SREG)[0] = (*_PREG)->u.oc.c; \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      GONext();

#define UNIFY_N_ATOMS_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	register Int i = (*_PREG)->u.osc.s; \
	register CELL d0, d1;  \
	register CELL *pt0; \
	d1 = (*_PREG)->u.osc.c; \
	for (; i > 0; i--) { \
	  pt0 = (*_SREG)++; \
	  d0 = *pt0; \
	  Int nonvar = 1; \
	  if(IsVarTerm(d0)) { \
	    nonvar = 0; \
	    while (Unsigned(pt0) != (d0)) { \
	      (pt0) = (CELL *)(d0); \
          (d0) = *(CELL *)(d0); \
          if(!IsVarTerm(d0)) { \
            nonvar = 1; \
            break; \
          } \
	    } \
	    if (!nonvar) { \
	      Bind_Global(pt0, d1); \
	      continue; \
	    } \
	  } \
	  if (nonvar) { \
	    if (d0 != d1) { \
	      break; \
	    } \
	    continue; \
	  } \
	} \
	(*_PREG) = NEXTOP((*_PREG), osc); \
    GONext();

#define UNIFY_N_ATOMS_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0, d1; \
      d0 = (*_PREG)->u.osc.s; \
      d1 = (*_PREG)->u.osc.c; \
      CACHE_S(); \
      READ_IN_S(); \
      (*_PREG) = NEXTOP((*_PREG), osc); \
      for (; d0 > 0; d0--) { \
	*S_SREG++ = d1; \
	  } \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      GONextW();

#define UNIFY_FLOAT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG)++; \
      d0 = *pt0;

#define UNIFY_FLOAT_UFLOAT_NONVAR_INIT \
      pt0 = RepAppl(d0); \
      d0 = *pt0;

#define UNIFY_FLOAT_UFLOAT_NONVAR_D0ISFUNCTOR \
      pt1 = (*_PREG)->u.od.d; \
      (*_PREG) = NEXTOP((*_PREG), od);

#define UNIFY_FLOAT_UFLOAT_NONVAR_END \
      GONext();

#define UNIFY_FLOAT_UFLOAT_UNK \
      d1 = AbsAppl((*_PREG)->u.od.d); \
      (*_PREG) = NEXTOP((*_PREG), od); \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_FLOAT_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      * (*_SREG)++ = AbsAppl((*_PREG)->u.od.d); \
      (*_PREG) = NEXTOP((*_PREG), od); \
      GONextW();

#define UNIFY_L_FLOAT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, pt1; \
      CACHE_S(); \
      READ_IN_S(); \
      d0 = *S_SREG;

#define UNIFY_L_FLOAT_D0ISAPPL \
      pt0 = RepAppl(d0); \
      d0 = *pt0;

#define UNIFY_L_FLOAT_D0ISFUNC \
      pt1 = (*_PREG)->u.od.d; \
      (*_PREG) = NEXTOP((*_PREG), od);

#define UNIFY_L_FLOAT_EQUALS \
      GONext();
	  
#define UNIFY_L_FLOAT_ULFLOAT_UNK \
      d1 = AbsAppl((*_PREG)->u.od.d); \
      (*_PREG) = NEXTOP((*_PREG), od); \
      Bind_Global(S_SREG, d1); \
      GONext(); \
      ENDCACHE_S();

#define UNIFY_L_FLOAT_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_SREG)[0] = AbsAppl((*_PREG)->u.od.d); \
      (*_PREG) = NEXTOP((*_PREG), od); \
      GONext();

#define UNIFY_LONGINT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = (*_SREG)++; \
      d0 = *pt0;

#define UNIFY_LONGINT_D0ISAPPL \
      pt0 = RepAppl(d0); \
      d0 = *pt0;

#define UNIFY_LONGINT_D0ISFUNC \
      pt1 = (*_PREG)->u.oi.i; \
      (*_PREG) = NEXTOP((*_PREG), oi);

#define UNIFY_LONGINT_EQUALS \
      GONext();

#define UNIFY_LONGINT_ULONGINT_UNK \
      d1 = AbsAppl((*_PREG)->u.oi.i); \
      (*_PREG) = NEXTOP((*_PREG), oi); \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_LONGINT_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      * (*_SREG)++ = AbsAppl((*_PREG)->u.oi.i); \
      (*_PREG) = NEXTOP((*_PREG), oi); \
      GONextW();

#define UNIFY_L_LONGINT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      CACHE_S(); \
      READ_IN_S(); \
      d0 = *S_SREG;

#define UNIFY_L_LONGINT_D0ISAPPL \
      pt0 = RepAppl(d0); \
      d0 = *pt0;

#define UNIFY_L_LONGINT_D0ISFUNC \
      pt1 = (*_PREG)->u.oi.i; \
      (*_PREG) = NEXTOP((*_PREG), oi);

#define UNIFY_L_LONGINT_EQUALS \
      GONext();

#define UNIFY_L_LONGINT_ULLONGINT_UNK \
      d1 = AbsAppl((*_PREG)->u.oi.i); \
      (*_PREG) = NEXTOP((*_PREG), oi); \
      Bind_Global(S_SREG, d1); \
      GONext();

#define UNIFY_L_LONGINT_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_SREG)[0] = AbsAppl((*_PREG)->u.oi.i); \
      (*_PREG) = NEXTOP((*_PREG), oi); \
      GONext();

#ifdef USE_GMP
#define UNIFY_BIGINT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      pt0 = (*_SREG)++; \
      d0 = *pt0;

#define UNIFY_BIGINT_D0ISAPPL \
      pt0 = RepAppl(d0); \
      d1 = *pt0;

#define UNIFY_BIGINT_D1ISFUNC_GMP \
      (*_PREG) = NEXTOP((*_PREG), oN); \
      GONext();

#define UNIFY_BIGINT_UBIGINT_UNK \
      d1 = (*_PREG)->u.oN.b; \
      (*_PREG) = NEXTOP((*_PREG), oN); \
      Bind_Global(pt0, d1); \
      GONext();
#endif

#ifdef USE_GMP
#define UNIFY_L_BIGINT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      CACHE_S(); \
      READ_IN_S(); \
      d0 = *S_SREG;

#define UNIFY_L_BIGINT_D0ISAPPL \
      pt0 = RepAppl(d0); \
      d0 = *pt0;

#define UNIFY_L_BIGINT_D0ISFUNC_GMP \
      (*_PREG) = NEXTOP((*_PREG), oN); \
      GONext();

#define UNIFY_L_BIGINT_ULBIGINT_UNK \
      d1 = (*_PREG)->u.oN.b; \
      (*_PREG) = NEXTOP((*_PREG), oN); \
      Bind_Global(S_SREG, d1); \
      GONext(); \
	  ENDCACHE_S();
#endif

#define UNIFY_DBTERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      pt0 = (*_SREG)++; \
      d0 = *pt0;

#define UNIFY_DBTERM_UDBTERM_NONVAR \
      BLOCK = (CELL)UNIFY_DBTERM_UDBTERM_NONVAR; \
      d1 = (*_PREG)->u.oD.D; \
      (*_PREG) = NEXTOP((*_PREG), oD); \
      YAAM_UNIFYBOUND;
	  
#define UNIFY_DBTERM_UDBTERM_UNK \
      d1 = (*_PREG)->u.oD.D; \
      (*_PREG) = NEXTOP((*_PREG), oD); \
      Bind_Global(pt0, d1); \
      GONext();

#define UNIFY_L_DBTERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0, d1; \
	  register CELL *pt0; \
      CACHE_S(); \
      READ_IN_S(); \
      d0 = *S_SREG;

#define UNIFY_L_DBTERM_ULDBTERM_NONVAR \
      BLOCK = (CELL)UNIFY_L_DBTERM_ULDBTERM_NONVAR; \
      d1 = (*_PREG)->u.oD.D; \
      (*_PREG) = NEXTOP((*_PREG), oD); \
      YAAM_UNIFYBOUND;

#define UNIFY_L_DBTERM_ULDBTERM_UNK \
      d1 = (*_PREG)->u.oD.D; \
      (*_PREG) = NEXTOP((*_PREG), oD); \
      Bind_Global(S_SREG, d1); \
      GONext(); \
      ENDCACHE_S();

#define UNIFY_LIST_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL *pt0; \
      *--SP = Unsigned((*_SREG) + 1); \
      *--SP = READ_MODE; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_LIST_READMODE \
      BLOCK = (CELL)UNIFY_LIST_READMODE; \
      FAILED = 0; \
      if (!IsPairTerm(d0)) { \
	YAAM_FAIL; \
      } \
      else { \
    (*_SREG) = RepPair(d0); \
    (*_PREG) = NEXTOP((*_PREG), o); \
    GONext(); \
      }
	  
#define UNIFY_LIST_WRITEMODE \
      START_PREFETCH_W(o); \
      CACHE_S(); \
      READ_IN_S(); \
      S_SREG = HR; \
      (*_PREG) = NEXTOP((*_PREG), o); \
      HR = S_SREG + 2; \
      d0 = AbsPair(S_SREG); \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      Bind_Global(pt0, d0); \
      GONextW(); \
      END_PREFETCH_W();

#define UNIFY_LIST_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      (*_PREG) = NEXTOP((*_PREG), o); \
      d0 = AbsPair(HR); \
      CACHE_S(); \
      READ_IN_S(); \
      SP -= 2; \
      SP[0] = WRITE_MODE; \
      SP[1] = Unsigned(S_SREG + 1); \
      S_SREG[0] = d0; \
      S_SREG = HR; \
      HR = S_SREG + 2; \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      GONextW();

#define UNIFY_L_LIST_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
      register CELL d0; \
	  register CELL *pt0; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_L_LIST_READMODE \
      BLOCK = (CELL)UNIFY_L_LIST_READMODE; \
      FAILED = 0; \
      if (!IsPairTerm(d0)) { \
	YAAM_FAIL; \
      } \
      else { \
    (*_PREG) = NEXTOP((*_PREG), o); \
    (*_SREG) = RepPair(d0); \
    GONext(); \
      } \
	  
#define UNIFY_L_LIST_WRITEMODE \
      START_PREFETCH_W(o); \
      (*_PREG) = NEXTOP((*_PREG), o); \
      CACHE_S(); \
      READ_IN_S(); \
      S_SREG = HR; \
      HR = S_SREG + 2; \
      d0 = AbsPair(S_SREG); \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      Bind_Global(pt0, d0); \
      GONextW(); \
      END_PREFETCH_W();

#define UNIFY_L_LIST_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = AbsPair(HR); \
      (*_PREG) = NEXTOP((*_PREG), o); \
      CACHE_S(); \
      READ_IN_S(); \
      S_SREG[0] = d0; \
      S_SREG = HR; \
      HR = S_SREG + 2; \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      GONextW();

#define UNIFY_STRUCT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      *--SP = Unsigned((*_SREG) + 1); \
      *--SP = READ_MODE; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_STRUCT_READMODE \
      BLOCK = (CELL)UNIFY_STRUCT_READMODE; \
      FAILED = 0; \
      if (!IsApplTerm(d0)) { \
	YAAM_FAIL; \
      } \
	  else { \
        CACHE_S(); \
        READ_IN_S(); \
        S_SREG = RepAppl(d0); \
        d0 = (CELL) ((*_PREG)->u.ofa.f); \
        if (*S_SREG != d0) { \
	  YAAM_FAIL; \
        } \
		else { \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      WRITEBACK_S(S_SREG+1); \
	  GONext(); \
		} \
        ENDCACHE_S(); \
	  }

#define UNIFY_STRUCT_WRITEMODE \
      START_PREFETCH_W(ofa); \
      d1 = AbsAppl(HR); \
      Bind_Global(pt0, d1); \
      pt0 = HR; \
      d0 = (CELL) ((*_PREG)->u.ofa.f); \
      *pt0++ = d0; \
      HR = pt0 + (*_PREG)->u.ofa.a; \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      (*_SREG) = pt0; \
      GONextW(); \
      END_PREFETCH_W();

#define UNIFY_STRUCT_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      CACHE_S(); \
      READ_IN_S(); \
      *--SP = Unsigned(S_SREG + 1); \
      *--SP = WRITE_MODE; \
      d0 = AbsAppl(HR); \
      S_SREG[0] = d0; \
      S_SREG = HR; \
      d0 = (CELL) ((*_PREG)->u.ofa.f); \
      *S_SREG++ = d0; \
      HR = S_SREG + (*_PREG)->u.ofa.a; \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      GONextW();

#define UNIFY_L_STRUC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      pt0 = (*_SREG); \
      d0 = *pt0;

#define UNIFY_L_STRUC_READMODE \
      BLOCK = (CELL)UNIFY_L_STRUC_READMODE; \
      FAILED = 0; \
      if (!IsApplTerm(d0)) { \
	YAAM_FAIL; \
      } \
	  else { \
        (*_SREG) = RepAppl(d0); \
        d0 = (CELL) ((*_PREG)->u.ofa.f); \
        if (*(*_SREG)++ != d0) { \
	  YAAM_FAIL; \
        } \
		else { \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      GONext(); \
		} \
	  }

#define UNIFY_L_STRUC_WRITEMODE \
      START_PREFETCH_W(ofa); \
      d1 = AbsAppl(HR); \
      Bind_Global(pt0, d1); \
      pt0 = HR; \
      d0 = (CELL) ((*_PREG)->u.ofa.f); \
      *pt0++ = d0; \
      HR = pt0 + (*_PREG)->u.ofa.a; \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      (*_SREG) = pt0; \
      GONextW(); \
      END_PREFETCH_W();

#define UNIFY_L_STRUC_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = AbsAppl(HR); \
      CACHE_S(); \
      READ_IN_S(); \
      S_SREG[0] = d0; \
      S_SREG = HR; \
      d0 = (CELL) ((*_PREG)->u.ofa.f); \
      *S_SREG++ = d0; \
      HR = S_SREG + (*_PREG)->u.ofa.a; \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      GONextW();

#define SAVE_PAIR_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      XREG((*_PREG)->u.ox.x) = AbsPair((*_SREG)); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONext();

#define SAVE_PAIR_X_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      XREG((*_PREG)->u.ox.x) = AbsPair((*_SREG)); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONextW();

#define SAVE_PAIR_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,AbsPair((*_SREG))); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();

#define SAVE_PAIR_Y_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,AbsPair((*_SREG))); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONextW();

#define SAVE_APPL_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      XREG((*_PREG)->u.ox.x) = AbsAppl((*_SREG) - 1); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONext();
	  
#define SAVE_APPL_X_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      XREG((*_PREG)->u.ox.x) = AbsAppl((*_SREG) - 1); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONextW();

#define SAVE_APPL_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,AbsAppl((*_SREG)-1)); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();

#define SAVE_APPL_Y_WRITE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,AbsAppl((*_SREG)-1)); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONextW();

