#define PUT_X_VAR_INSTINIT \
      register CELL *pt0; \
      pt0 = HR; \
      XREG((*_PREG)->u.xx.xl) = Unsigned(pt0); \
      HR = pt0 + 1; \
      XREG((*_PREG)->u.xx.xr) = Unsigned(pt0); \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      RESET_VARIABLE(pt0); \
      GONext();

#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#define PUT_Y_VAR_INSTINIT \
      register CELL *pt0; \
      pt0 = YREG + (*_PREG)->u.yx.y; \
      XREG((*_PREG)->u.yx.x) = (CELL) pt0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      if (Unsigned((Int)(pt0)-(Int)(H_FZ)) > \
	  Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	*pt0 =  (CELL)STACK_TO_SBA(pt0); \
      } else \
	INITIALIZE_PERMVAR(pt0, (CELL)pt0); \
      GONext();
#else /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#define PUT_Y_VAR_INSTINIT \
      register CELL *pt0; \
      pt0 = YREG + (*_PREG)->u.yx.y; \
      XREG((*_PREG)->u.yx.x) = (CELL) pt0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
	INITIALIZE_PERMVAR(pt0, (CELL)pt0); \
      GONext();
#endif /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */

#define PUT_X_VAL_INSTINIT \
      register CELL d0; \
      d0 = XREG((*_PREG)->u.xx.xl); \
      XREG((*_PREG)->u.xx.xr) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xx); \
      GONext();

#define PUT_XX_VAL_INSTINIT \
      register CELL d0, d1; \
      d0 = XREG((*_PREG)->u.xxxx.xl1); \
      d1 = XREG((*_PREG)->u.xxxx.xl2); \
      XREG((*_PREG)->u.xxxx.xr1) = d0; \
      XREG((*_PREG)->u.xxxx.xr2) = d1; \
      (*_PREG) = NEXTOP((*_PREG), xxxx); \
      GONext();

#ifdef YAPOR_SBA
#define PUT_Y_VAL_INSTINIT \
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.yx.y]; \
      if (d0 == 0) { \
	XREG((*_PREG)->u.yx.x) = (CELL)(YREG+(*_PREG)->u.yx.y); \
      } else \
	XREG((*_PREG)->u.yx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      GONext();
#else /* YAPOR_SBA */
#define PUT_Y_VAL_INSTINIT \
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.yx.y]; \
	XREG((*_PREG)->u.yx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      GONext();
#endif /* YAPOR_SBA */

#ifdef YAPOR_SBA
#define PUT_Y_VALS_INSTINIT \
      register CELL d0, d1; \
      ALWAYS_START_PREFETCH(yyxx); \
      d0 = YREG[(*_PREG)->u.yyxx.y1]; \
      if (d0 == 0) \
	XREG((*_PREG)->u.yyxx.x1) = (CELL)(YREG+(*_PREG)->u.yyxx.y1); \
      else \
	XREG((*_PREG)->u.yyxx.x1) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yyxx); \
      d1 = YREG[PREVOP((*_PREG),yyxx)->u.yyxx.y2]; \
      if (d1 == 0) \
	XREG(PREVOP((*_PREG)->u.yyxx,yyxx).x2) = (CELL)(YREG+(*_PREG)->u.yyxx.y2); \
      else \
	XREG(PREVOP((*_PREG),yyxx)->u.yyxx.x2) = d1; \
      ALWAYS_GONext(); \
      ALWAYS_END_PREFETCH();
#else /* YAPOR_SBA */
#define PUT_Y_VALS_INSTINIT \
      register CELL d0, d1; \
      ALWAYS_START_PREFETCH(yyxx); \
      d0 = YREG[(*_PREG)->u.yyxx.y1]; \
	XREG((*_PREG)->u.yyxx.x1) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yyxx); \
      d1 = YREG[PREVOP((*_PREG),yyxx)->u.yyxx.y2]; \
	XREG(PREVOP((*_PREG),yyxx)->u.yyxx.x2) = d1; \
      ALWAYS_GONext(); \
      ALWAYS_END_PREFETCH();
#endif /* YAPOR_SBA */

#define PUT_UNSAFE_INSTINIT \
      register CELL d0; \
	  register CELL *pt0; \
      pt0 = YREG+(*_PREG)->u.yx.y; \
      d0 = *pt0;

#define PUT_UNSAFE_PUNSAFE_NONVAR \
      XREG((*_PREG)->u.yx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), yx); \
      GONext();
      
#define PUT_UNSAFE_PUNSAFE_UNK \
      if (pt0 <= HR || pt0 >= YREG) { \
	XREG((*_PREG)->u.yx.x) = Unsigned(pt0); \
	(*_PREG) = NEXTOP((*_PREG), yx); \
	GONext(); \
      } \
      else { \
	Bind_Local(pt0, Unsigned(HR)); \
	XREG((*_PREG)->u.yx.x) = (CELL) HR; \
	RESET_VARIABLE(HR); \
	H++; \
	(*_PREG) = NEXTOP((*_PREG), yx); \
	GONext(); \
      }

#define PUT_ATOM_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->u.xc.c; \
      XREG((*_PREG)->u.xc.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xc); \
      GONext();

#define PUT_DBTERM_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->u.xD.D; \
      XREG((*_PREG)->u.xD.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xD); \
      GONext();

#define PUT_BIGINT_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->u.xN.b; \
      XREG((*_PREG)->u.xN.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xN); \
      GONext();

#define PUT_FLOAT_INSTINIT \
      register CELL d0; \
      d0 = AbsAppl((*_PREG)->u.xd.d); \
      XREG((*_PREG)->u.xd.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xd); \
      GONext();

#define PUT_LONGINT_INSTINIT \
      register CELL d0; \
      d0 = AbsAppl((*_PREG)->u.xi.i); \
      XREG((*_PREG)->u.xi.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xi); \
      GONext();

#define PUT_LIST_INSTINIT \
      register CELL d0; \
      CACHE_S(); \
      READ_IN_S(); \
      S_SREG = HR; \
      HR += 2; \
      d0 = AbsPair(S_SREG); \
      XREG((*_PREG)->u.x.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), x); \
      WRITEBACK_S(S_SREG); \
      ENDCACHE_S(); \
      GONext();

#define PUT_STRUCT_INSTINIT \
      register CELL d0; \
      d0 = AbsAppl(HR); \
      XREG((*_PREG)->u.xfa.x) = d0; \
      d0 = (CELL) ((*_PREG)->u.xfa.f); \
      *H++ = d0; \
      (*_SREG) = HR; \
      HR += (*_PREG)->u.xfa.a; \
      (*_PREG) = NEXTOP((*_PREG), xfa); \
      GONext();

