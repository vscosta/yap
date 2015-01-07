#define _put_x_var_instinit \
      register CELL *pt0; \
      pt0 = HR; \
      XREG((*_PREG)->y_u.xx.xl) = Unsigned(pt0); \
      HR = pt0 + 1; \
      XREG((*_PREG)->y_u.xx.xr) = Unsigned(pt0); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      RESET_VARIABLE(pt0); \
      GONEXT();

#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#define _put_y_var_instinit \
      register CELL *pt0; \
      pt0 = YREG + (*_PREG)->y_u.yx.y; \
      XREG((*_PREG)->y_u.yx.x) = (CELL) pt0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      if (Unsigned((Int)(pt0)-(Int)(H_FZ)) > \
	  Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	*pt0 =  (CELL)STACK_TO_SBA(pt0); \
      } else \
	INITIALIZE_PERMVAR(pt0, (CELL)pt0); \
      GONEXT();
#else /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#define _put_y_var_instinit \
      register CELL *pt0; \
      pt0 = YREG + (*_PREG)->y_u.yx.y; \
      XREG((*_PREG)->y_u.yx.x) = (CELL) pt0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
	INITIALIZE_PERMVAR(pt0, (CELL)pt0); \
      GONEXT();
#endif /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */

#define _put_x_val_instinit \
      register CELL d0; \
      d0 = XREG((*_PREG)->y_u.xx.xl); \
      XREG((*_PREG)->y_u.xx.xr) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      GONEXT();

#define _put_xx_val_instinit \
      register CELL d0, d1; \
      d0 = XREG((*_PREG)->y_u.xxxx.xl1); \
      d1 = XREG((*_PREG)->y_u.xxxx.xl2); \
      XREG((*_PREG)->y_u.xxxx.xr1) = d0; \
      XREG((*_PREG)->y_u.xxxx.xr2) = d1; \
      (*_PREG) = NEXTOP((*_PREG), xxxx); \
      GONEXT();

#ifdef YAPOR_SBA
#define _put_y_val_instinit \
      register CELL d0; \
      d0 = YREG[(*_PREG)->y_u.yx.y]; \
      if (d0 == 0) { \
	XREG((*_PREG)->y_u.yx.x) = (CELL)(YREG+(*_PREG)->y_u.yx.y); \
      } else \
	XREG((*_PREG)->y_u.yx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      GONEXT();
#else /* YAPOR_SBA */
#define _put_y_val_instinit \
      register CELL d0; \
      d0 = YREG[(*_PREG)->y_u.yx.y]; \
	XREG((*_PREG)->y_u.yx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      GONEXT();
#endif /* YAPOR_SBA */

#ifdef YAPOR_SBA
#define _put_y_vals_instinit \
      register CELL d0, d1; \
      d0 = YREG[(*_PREG)->y_u.yyxx.y1]; \
      if (d0 == 0) \
	XREG((*_PREG)->y_u.yyxx.x1) = (CELL)(YREG+(*_PREG)->y_u.yyxx.y1); \
      else \
	XREG((*_PREG)->y_u.yyxx.x1) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yyxx); \
      d1 = YREG[PREVOP((*_PREG),yyxx)->y_u.yyxx.y2]; \
      if (d1 == 0) \
	XREG(PREVOP((*_PREG)->y_u.yyxx,yyxx).x2) = (CELL)(YREG+(*_PREG)->y_u.yyxx.y2); \
      else \
	XREG(PREVOP((*_PREG),yyxx)->y_u.yyxx.x2) = d1; \
      GONEXT();
#else /* YAPOR_SBA */
#define _put_y_vals_instinit \
      register CELL d0, d1; \
      d0 = YREG[(*_PREG)->y_u.yyxx.y1]; \
	XREG((*_PREG)->y_u.yyxx.x1) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yyxx); \
      d1 = YREG[PREVOP((*_PREG),yyxx)->y_u.yyxx.y2]; \
	XREG(PREVOP((*_PREG),yyxx)->y_u.yyxx.x2) = d1; \
      GONEXT();
#endif /* YAPOR_SBA */

#define _put_unsafe_instinit \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = YREG+(*_PREG)->y_u.yx.y; \
      d0 = *pt0; \
      deref_head(d0, punsafe_unk); \
    punsafe_nonvar: \
      XREG((*_PREG)->y_u.yx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      GONEXT(); \
 \
      derefa_body(d0, pt0, punsafe_unk, punsafe_nonvar); \
      if (pt0 <= HR || pt0 >= YREG) { \
	XREG((*_PREG)->y_u.yx.x) = Unsigned(pt0); \
	(*_PREG) = NEXTOP((*_PREG), yx); \
	GONEXT(); \
      } \
      else { \
	Bind_Local(pt0, Unsigned(HR)); \
	XREG((*_PREG)->y_u.yx.x) = (CELL) HR; \
	RESET_VARIABLE(HR); \
	HR++; \
	(*_PREG) = NEXTOP((*_PREG), yx); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);

#define _put_atom_instinit \
      register CELL d0; \
      d0 = (*_PREG)->y_u.xc.c; \
      XREG((*_PREG)->y_u.xc.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xc); \
      GONEXT();

#define PUT_DBTERM_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->y_u.xD.D; \
      XREG((*_PREG)->y_u.xD.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xD); \
      GONext();

#define PUT_BIGINT_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->y_u.xN.b; \
      XREG((*_PREG)->y_u.xN.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xN); \
      GONext();

#define _put_float_instinit \
      register CELL d0; \
      d0 = AbsAppl((*_PREG)->y_u.xd.d); \
      XREG((*_PREG)->y_u.xd.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xd); \
      GONEXT();

#define PUT_LONGINT_INSTINIT \
      register CELL d0; \
      d0 = AbsAppl((*_PREG)->y_u.xi.i); \
      XREG((*_PREG)->y_u.xi.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xi); \
      GONext();

#define _put_list_instinit \
      register CELL d0; \
      CACHE_S(); \
      READ_IN_S(); \
      S_SREG = HR; \
      HR += 2; \
      d0 = AbsPair(S_SREG); \
      XREG((*_PREG)->y_u.x.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), x); \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      GONEXT();

#define _put_struct_instinit \
      register CELL d0; \
      d0 = AbsAppl(HR); \
      XREG((*_PREG)->y_u.xfa.x) = d0; \
      d0 = (CELL) ((*_PREG)->y_u.xfa.f); \
      *HR++ = d0; \
      (*_SREG) = HR; \
      HR += (*_PREG)->y_u.xfa.a; \
      (*_PREG) = NEXTOP((*_PREG), xfa); \
      GONEXT();

