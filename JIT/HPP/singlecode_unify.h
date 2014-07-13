#ifdef YAPOR_SBA
#define _unify_x_var_instinit \
      CACHE_S(); \
      READ_IN_S(); \
      BEGD(d0); \
      d0 = *S_SREG; \
      if (d0 == 0) { \
	d0 = (CELL)S_SREG; \
	} \
      WRITEBACK_S(S_SREG+1); \
      XREG((*_PREG)->u.ox.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXT(); \
      ENDD(d0); \
      ENDCACHE_S();
#else /* YAPOR_SBA */
#define _unify_x_var_instinit \
      CACHE_S(); \
      READ_IN_S(); \
      BEGD(d0); \
      d0 = *S_SREG; \
      WRITEBACK_S(S_SREG+1); \
      XREG((*_PREG)->u.ox.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXT(); \
      ENDD(d0); \
      ENDCACHE_S();
#endif /* YAPOR_SBA */

#define _unify_x_var_write_instinit \
      register CELL *pt0; \
      CACHE_S(); \
      READ_IN_S(); \
      pt0 = &XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      RESET_VARIABLE(S_SREG); \
      *pt0 = (CELL) S_SREG; \
      WRITEBACK_S(S_SREG+1); \
      ENDCACHE_S(); \
      GONEXTW();

#ifdef YAPOR_SBA
#define _unify_l_x_var_instinit \
      BEGP(pt0); \
      BEGD(d0); \
      d0 = SREG[0]; \
      pt0 = &XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      if (d0 == 0) { \
	d0 = (CELL)SREG; \
      } \
      *pt0 = d0; \
      GONEXT(); \
      ENDD(d0); \
      ENDP(pt0);
#else /* YAPOR_SBA */
#define _unify_l_x_var_instinit \
      BEGP(pt0); \
      BEGD(d0); \
      d0 = (*_SREG)[0]; \
      pt0 = &XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      *pt0 = d0; \
      GONEXT(); \
      ENDD(d0); \
      ENDP(pt0);
#endif /* YAPOR_SBA */

#define _unify_l_x_var_write_instinit \
      register CELL *pt0; \
      CACHE_S(); \
      READ_IN_S(); \
      pt0 = &XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      RESET_VARIABLE(S_SREG); \
      *pt0 = (CELL)S_SREG; \
      ENDCACHE_S(); \
      GONEXT();

#ifdef YAPOR_SBA
#define _unify_x_var2_instinit \
      CACHE_S(); \
      READ_IN_S(); \
      BEGP(pt0); \
      pt0 = &XREG((*_PREG)->u.oxx.xr); \
      BEGD(d0); \
      d0 = S_SREG[0]; \
      BEGD(d1); \
      d1 = S_SREG[1]; \
      if (d0 == 0) { \
	d0 = (CELL)S_SREG; \
	} \
      if (d1 == 0) { \
	d1 = (CELL)(S_SREG+1); \
	} \
      WRITEBACK_S(S_SREG+2); \
      XREG((*_PREG)->u.oxx.xl) = d0; \
      (*_PREG) = NEXTOP((*_PREG), oxx); \
      *pt0 = d1; \
      ENDD(d0); \
      ENDD(d1); \
      ENDP(pt0); \
      GONEXT(); \
      ENDCACHE_S();
#else /* YAPOR_SBA */
#define _unify_x_var2_instinit \
      CACHE_S(); \
      READ_IN_S(); \
      BEGP(pt0); \
      pt0 = &XREG((*_PREG)->u.oxx.xr); \
      BEGD(d0); \
      d0 = S_SREG[0]; \
      BEGD(d1); \
      d1 = S_SREG[1]; \
      WRITEBACK_S(S_SREG+2); \
      XREG((*_PREG)->u.oxx.xl) = d0; \
      (*_PREG) = NEXTOP((*_PREG), oxx); \
      *pt0 = d1; \
      ENDD(d0); \
      ENDD(d1); \
      ENDP(pt0); \
      GONEXT(); \
      ENDCACHE_S();
#endif /* YAPOR_SBA */

#define _unify_x_var2_write_instinit \
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
      GONEXTW();

#ifdef YAPOR_SBA
#define _unify_l_x_var2_instinit \
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
      GONEXT();
#else /* YAPOR_SBA */
#define _unify_l_x_var2_instinit \
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
      GONEXT();
#endif /* YAPOR_SBA */

#define _unify_l_x_var2_write_instinit \
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
      GONEXT();

#ifdef YAPOR_SBA
#define _unify_y_var_instinit \
      register CELL d0; \
      d0 = *(*_SREG)++; \
      if (d0 == 0) { \
	INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,(CELL)((*_SREG)-1)); \
      } else \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONEXT();
#else /* YAPOR_SBA */
#define _unify_y_var_instinit \
      register CELL d0; \
      d0 = *(*_SREG)++; \
	INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,d0); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONEXT();
#endif /* YAPOR_SBA */

#define _unify_y_var_write_instinit \
      CACHE_S(); \
      READ_IN_S(); \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,(CELL) S_SREG); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      RESET_VARIABLE(S_SREG); \
      WRITEBACK_S(S_SREG+1); \
      ENDCACHE_S(); \
      GONEXTW();

#ifdef YAPOR_SBA
#define _unify_l_y_var_instinit \
      register CELL d0; \
      d0 = (*_SREG)[0]; \
      if (d0 == 0) { \
	INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,(CELL)(*_SREG)); \
      } else \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONEXT();
#else /* YAPOR_SBA */
#define _unify_l_y_var_instinit \
      register CELL d0; \
      d0 = (*_SREG)[0]; \
	INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,d0); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONEXT();
#endif /* YAPOR_SBA */

#define _unify_l_y_var_write_instinit \
      CACHE_S(); \
      READ_IN_S(); \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,(CELL) S_SREG); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      RESET_VARIABLE(S_SREG); \
      ENDCACHE_S(); \
      GONEXT();

#define _unify_x_val_instinit \
      BEGD(d0); \
      BEGD(d1); \
      BEGP(pt0); \
      pt0 = (*_SREG); \
      d0 = *pt0; \
      deref_head(d0, uvalx_unk); \
 \
    uvalx_nonvar: \
      d1 = XREG((*_PREG)->u.ox.x); \
      deref_head(d1, uvalx_nonvar_unk); \
 \
    uvalx_nonvar_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      YAAM_UnifyBound(d0, d1); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, uvalx_nonvar_unk, uvalx_nonvar_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      Bind(pt1, d0); \
      GONEXT(); \
      ENDP(pt1); \
 \
      derefa_body(d0, pt0, uvalx_unk, uvalx_nonvar); \
      d1 = XREG((*_PREG)->u.ox.x); \
      deref_head(d1, uvalx_var_unk); \
 \
    uvalx_var_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      Bind_Global(pt0, d1); \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, uvalx_var_unk, uvalx_var_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONEXT(); \
      ENDP(pt1); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define _unify_x_val_write_instinit \
      *(*_SREG)++ = XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXTW();

#define _unify_l_x_val_instinit \
      BEGD(d0); \
      BEGD(d1); \
      BEGP(pt0); \
      pt0 = (*_SREG); \
      d0 = *pt0; \
      deref_head(d0, ulvalx_unk); \
 \
    ulvalx_nonvar: \
      d1 = XREG((*_PREG)->u.ox.x); \
      deref_head(d1, ulvalx_nonvar_unk); \
 \
    ulvalx_nonvar_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      YAAM_UnifyBound(d0, d1); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, ulvalx_nonvar_unk, ulvalx_nonvar_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      Bind(pt1, d0); \
      GONEXT(); \
      ENDP(pt1); \
 \
      derefa_body(d0, pt0, ulvalx_unk, ulvalx_nonvar); \
      d1 = XREG((*_PREG)->u.ox.x); \
      deref_head(d1, ulvalx_var_unk); \
 \
    ulvalx_var_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      Bind_Global(pt0, d1); \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, ulvalx_var_unk, ulvalx_var_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONEXT(); \
      ENDP(pt1); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define _unify_l_x_val_write_instinit \
      (*_SREG)[0] = XREG((*_PREG)->u.ox.x); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXT();

#define UNIFY_Y_VAL_INSTINIT \
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
      register CELL d0;  \
      d0 = YREG[(*_PREG)->u.oy.y]; \
	*(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONextW();
#endif /* YAPOR_SBA */

#define UNIFY_L_Y_VAL_INSTINIT \
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
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.oy.y]; \
	(*_SREG)[0] = d0; \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();
#endif /* YAPOR_SBA */
	  
#define _unify_x_loc_instinit \
      BEGD(d0); \
      BEGD(d1); \
      BEGP(pt0); \
      pt0 = (*_SREG); \
      d0 = *pt0; \
      deref_head(d0, uvalx_loc_unk); \
 \
    uvalx_loc_nonvar: \
      d1 = XREG((*_PREG)->u.ox.x); \
      deref_head(d1, uvalx_loc_nonvar_unk); \
 \
    uvalx_loc_nonvar_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      YAAM_UnifyBound(d0, d1); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, uvalx_loc_nonvar_unk, uvalx_loc_nonvar_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      Bind(pt1, d0); \
      GONEXT(); \
      ENDP(pt1); \
 \
      derefa_body(d0, pt0, uvalx_loc_unk, uvalx_loc_nonvar); \
      d1 = XREG((*_PREG)->u.ox.x); \
      deref_head(d1, uvalx_loc_var_unk); \
 \
    uvalx_loc_var_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      Bind_Global(pt0, d1); \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, uvalx_loc_var_unk, uvalx_loc_var_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      (*_SREG)++; \
      UnifyCells(pt0, pt1); \
      GONEXT(); \
      ENDP(pt1); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define _unify_x_loc_write_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.ox.x); \
      deref_head(d0, unify_x_loc_unk); \
    unify_x_loc_nonvar: \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXTW(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, unify_x_loc_unk, unify_x_loc_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      if (pt0 < HR) { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONEXTW(); \
      } \
      else { \
	CACHE_S(); \
	READ_IN_S(); \
	Bind_Local(pt0, Unsigned(S_SREG)); \
	RESET_VARIABLE(S_SREG); \
	WRITEBACK_S(S_SREG+1); \
	ENDCACHE_S(); \
	GONEXTW(); \
      } \
      ENDP(pt0); \
      ENDD(d0);

#define _unify_l_x_loc_instinit \
      BEGD(d0); \
      BEGD(d1); \
      BEGP(pt0); \
      pt0 = (*_SREG); \
      d0 = *pt0; \
      deref_head(d0, ulvalx_loc_unk); \
 \
    ulvalx_loc_nonvar: \
      d1 = XREG((*_PREG)->u.ox.x); \
      deref_head(d1, ulvalx_loc_nonvar_unk); \
 \
    ulvalx_loc_nonvar_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      YAAM_UnifyBound(d0, d1); \
 \
      deref_body(d1, pt0, ulvalx_loc_nonvar_unk, ulvalx_loc_nonvar_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      Bind(pt0, d0); \
      GONEXT(); \
 \
      derefa_body(d0, pt0, ulvalx_loc_unk, ulvalx_loc_nonvar); \
      d1 = XREG((*_PREG)->u.ox.x); \
      deref_head(d1, ulvalx_loc_var_unk); \
 \
    ulvalx_loc_var_nonvar: \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      Bind_Global(pt0, d1); \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d1, pt1, ulvalx_loc_var_unk, ulvalx_loc_var_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      UnifyGlobalCellToCell(pt0, pt1); \
      GONEXT(); \
      ENDP(pt1); \
      ENDP(pt0); \
      ENDD(d1); \
      ENDD(d0);

#define _unify_l_x_loc_write_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.ox.x); \
      deref_head(d0, ulnify_x_loc_unk); \
    ulnify_x_loc_nonvar: \
      (*_SREG)[0] = d0; \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, ulnify_x_loc_unk, ulnify_x_loc_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      if (pt0 < HR) { \
	(*_SREG)[0] = Unsigned(pt0); \
	GONEXT(); \
      } \
      else { \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);

#define UNIFY_Y_LOC_INSTINIT \
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

#define _unify_void_instinit \
      (*_PREG) = NEXTOP((*_PREG), o); \
      (*_SREG)++; \
      GONEXT();
      
#define _unify_void_write_instinit \
      CACHE_S(); \
      READ_IN_S(); \
      (*_PREG) = NEXTOP((*_PREG), o); \
      RESET_VARIABLE(S_SREG); \
      WRITEBACK_S(S_SREG+1); \
      ENDCACHE_S(); \
      GONEXTW();

#define UNIFY_L_VOID_INSTINIT \
      (*_PREG) = NEXTOP((*_PREG), o); \
      GONext();

#define UNIFY_L_VOID_WRITE_INSTINIT \
      (*_PREG) = NEXTOP((*_PREG), o); \
      RESET_VARIABLE((*_SREG)); \
      GONext();

#define _unify_n_voids_instinit \
      (*_SREG) += (*_PREG)->u.os.s; \
      (*_PREG) = NEXTOP((*_PREG), os); \
      GONEXT();

#define _unify_n_voids_write_instinit \
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
      GONEXTW();

#define _unify_l_n_voids_instinit \
      (*_PREG) = NEXTOP((*_PREG), os); \
      GONEXT();

#define _unify_l_n_voids_write_instinit \
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
      GONEXT();

#define _unify_atom_instinit \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = (*_SREG)++; \
      d0 = *pt0; \
      deref_head(d0, uatom_unk); \
    uatom_nonvar: \
      if (d0 != (*_PREG)->u.oc.c) { \
	FAIL(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      GONEXT(); \
 \
      derefa_body(d0, pt0, uatom_unk, uatom_nonvar); \
      d0 = (*_PREG)->u.oc.c; \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      Bind_Global(pt0, d0); \
      GONEXT(); \
      ENDP(pt0); \
      ENDD(d0);

#define _unify_atom_write_instinit \
      * (*_SREG)++ = (*_PREG)->u.oc.c; \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      GONEXTW();

#define _unify_l_atom_instinit \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = (*_SREG); \
      d0 = *(*_SREG); \
      deref_head(d0, ulatom_unk); \
    ulatom_nonvar: \
      if (d0 != (*_PREG)->u.oc.c) { \
	FAIL(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      GONEXT(); \
 \
      derefa_body(d0, pt0, ulatom_unk, ulatom_nonvar); \
      d0 = (*_PREG)->u.oc.c; \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      Bind_Global(pt0, d0); \
      GONEXT(); \
      ENDP(pt0); \
      ENDD(d0);

#define _unify_l_atom_write_instinit \
      (*_SREG)[0] = (*_PREG)->u.oc.c; \
      (*_PREG) = NEXTOP((*_PREG), oc); \
      GONEXT();

#define UNIFY_N_ATOMS_INSTINIT \
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
      * (*_SREG)++ = AbsAppl((*_PREG)->u.od.d); \
      (*_PREG) = NEXTOP((*_PREG), od); \
      GONextW();

#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
#define _unify_l_float_instinit \
      BEGD(d0); \
      CACHE_S(); \
      READ_IN_S(); \
      d0 = *S_SREG; \
      deref_head(d0, ulfloat_unk); \
    ulfloat_nonvar: \
      if (!IsApplTerm(d0)) { \
	FAIL(); \
      } \
      BEGP(pt0); \
      pt0 = RepAppl(d0); \
      BEGD(d0); \
      d0 = *pt0; \
      if (d0 != (CELL)FunctorDouble) { \
	FAIL(); \
      } \
      ENDD(d0); \
      BEGP(pt1); \
      pt1 = (*_PREG)->u.od.d; \
      (*_PREG) = NEXTOP((*_PREG), od); \
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
      derefa_body(d0, S_SREG, ulfloat_unk, ulfloat_nonvar); \
      BEGD(d1); \
      d1 = AbsAppl((*_PREG)->u.od.d); \
      (*_PREG) = NEXTOP((*_PREG), od); \
      Bind_Global(S_SREG, d1); \
      GONEXT(); \
      ENDD(d1); \
      ENDCACHE_S(); \
      ENDD(d0);
#else /* SIZEOF_DOUBLE == 2*SIZEOF_INT_P */
#define _unify_l_float_instinit \
      BEGD(d0); \
      CACHE_S(); \
      READ_IN_S(); \
      d0 = *S_SREG; \
      deref_head(d0, ulfloat_unk); \
    ulfloat_nonvar: \
      if (!IsApplTerm(d0)) { \
	FAIL(); \
      } \
      BEGP(pt0); \
      pt0 = RepAppl(d0); \
      BEGD(d0); \
      d0 = *pt0; \
      if (d0 != (CELL)FunctorDouble) { \
	FAIL(); \
      } \
      ENDD(d0); \
      BEGP(pt1); \
      pt1 = (*_PREG)->u.od.d; \
      (*_PREG) = NEXTOP((*_PREG), od); \
      if ( \
	  pt1[1] != pt0[1] \
	  ) { \
	   FAIL(); \
	  } \
      ENDP(pt1); \
      ENDP(pt0); \
      GONEXT(); \
 \
      derefa_body(d0, S_SREG, ulfloat_unk, ulfloat_nonvar); \
      BEGD(d1); \
      d1 = AbsAppl((*_PREG)->u.od.d); \
      (*_PREG) = NEXTOP((*_PREG), od); \
      Bind_Global(S_SREG, d1); \
      GONEXT(); \
      ENDD(d1); \
      ENDCACHE_S(); \
      ENDD(d0);
#endif /* SIZEOF_DOUBLE == 2*SIZEOF_INT_P */

#define _unify_l_float_write_instinit \
      (*_SREG)[0] = AbsAppl((*_PREG)->u.od.d); \
      (*_PREG) = NEXTOP((*_PREG), od); \
      GONEXT();

#define UNIFY_LONGINT_INSTINIT \
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
      * (*_SREG)++ = AbsAppl((*_PREG)->u.oi.i); \
      (*_PREG) = NEXTOP((*_PREG), oi); \
      GONextW();

#define UNIFY_L_LONGINT_INSTINIT \
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
      (*_SREG)[0] = AbsAppl((*_PREG)->u.oi.i); \
      (*_PREG) = NEXTOP((*_PREG), oi); \
      GONext();

#ifdef USE_GMP
#define UNIFY_BIGINT_INSTINIT \
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

#define _unify_l_list_instinit \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = (*_SREG); \
      d0 = *pt0; \
      deref_head(d0, ullist_unk); \
    ullist_nonvar: \
      if (!IsPairTerm(d0)) { \
	FAIL(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), o); \
      (*_SREG) = RepPair(d0); \
      GONEXT(); \
 \
      derefa_body(d0, pt0, ullist_unk, ullist_nonvar); \
      (*_PREG) = NEXTOP((*_PREG), o); \
      CACHE_S(); \
      READ_IN_S(); \
      S_SREG = HR; \
      HR = S_SREG + 2; \
      d0 = AbsPair(S_SREG); \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      Bind_Global(pt0, d0); \
      GONEXTW(); \
      ENDP(pt0); \
      ENDD(d0);

#define _unify_l_list_write_instinit \
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
      GONEXTW();

#define _unify_struct_instinit \
      *--SP = Unsigned((*_SREG) + 1); \
      *--SP = READ_MODE; \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = (*_SREG); \
      d0 = *pt0; \
      deref_head(d0, ustruct_unk); \
    ustruct_nonvar: \
      if (!IsApplTerm(d0)) { \
	FAIL(); \
      } \
      CACHE_S(); \
      READ_IN_S(); \
      S_SREG = RepAppl(d0); \
      d0 = (CELL) ((*_PREG)->u.ofa.f); \
      if (*S_SREG != d0) { \
	FAIL(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      WRITEBACK_S(S_SREG+1); \
      ENDCACHE_S(); \
      GONEXT(); \
 \
      derefa_body(d0, pt0, ustruct_unk, ustruct_nonvar); \
      BEGD(d1); \
      d1 = AbsAppl(HR); \
      Bind_Global(pt0, d1); \
      pt0 = HR; \
      ENDD(d1); \
      d0 = (CELL) ((*_PREG)->u.ofa.f); \
      *pt0++ = d0; \
      HR = pt0 + (*_PREG)->u.ofa.a; \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      (*_SREG) = pt0; \
      GONEXTW(); \
      ENDP(pt0); \
      ENDD(d0);

#define _unify_struct_write_instinit \
      CACHE_S(); \
      READ_IN_S(); \
      *--SP = Unsigned(S_SREG + 1); \
      *--SP = WRITE_MODE; \
      BEGD(d0); \
      d0 = AbsAppl(HR); \
      S_SREG[0] = d0; \
      S_SREG = HR; \
      d0 = (CELL) ((*_PREG)->u.ofa.f); \
      *S_SREG++ = d0; \
      HR = S_SREG + (*_PREG)->u.ofa.a; \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      ENDD(d0); \
      GONEXTW();

#define _unify_l_struc_instinit \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = (*_SREG); \
      d0 = *pt0; \
      deref_head(d0, ulstruct_unk); \
    ulstruct_nonvar: \
      if (!IsApplTerm(d0)) { \
	FAIL(); \
      } \
      (*_SREG) = RepAppl(d0); \
      d0 = (CELL) ((*_PREG)->u.ofa.f); \
      if (*(*_SREG)++ != d0) { \
	FAIL(); \
      } \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      GONEXT(); \
 \
      derefa_body(d0, pt0, ulstruct_unk, ulstruct_nonvar); \
      BEGD(d1); \
      d1 = AbsAppl(HR); \
      Bind_Global(pt0, d1); \
      pt0 = HR; \
      ENDD(d1); \
      d0 = (CELL) ((*_PREG)->u.ofa.f); \
      *pt0++ = d0; \
      HR = pt0 + (*_PREG)->u.ofa.a; \
      (*_PREG) = NEXTOP((*_PREG), ofa); \
      (*_SREG) = pt0; \
      GONEXTW(); \
      ENDP(pt0); \
      ENDD(d0);

#define _unify_l_struc_write_instinit \
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
      GONEXTW();

#define _save_pair_x_instinit \
      XREG((*_PREG)->u.ox.x) = AbsPair((*_SREG)); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXT();

#define _save_pair_x_write_instinit \
      XREG((*_PREG)->u.ox.x) = AbsPair((*_SREG)); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXTW();

#define SAVE_PAIR_Y_INSTINIT \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,AbsPair((*_SREG))); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();

#define SAVE_PAIR_Y_WRITE_INSTINIT \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,AbsPair((*_SREG))); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONextW();

#define _save_appl_x_instinit \
      XREG((*_PREG)->u.ox.x) = AbsAppl((*_SREG) - 1); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXT();
	  
#define _save_appl_x_write_instinit \
      XREG((*_PREG)->u.ox.x) = AbsAppl((*_SREG) - 1); \
      (*_PREG) = NEXTOP((*_PREG), ox); \
      GONEXTW();

#define SAVE_APPL_Y_INSTINIT \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,AbsAppl((*_SREG)-1)); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONext();

#define SAVE_APPL_Y_WRITE_INSTINIT \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.oy.y,AbsAppl((*_SREG)-1)); \
      (*_PREG) = NEXTOP((*_PREG), oy); \
      GONextW();

