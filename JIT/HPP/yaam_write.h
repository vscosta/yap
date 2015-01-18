#define WRITE_X_VAR_INSTINIT \
      XREG((*_PREG)->u.x.x) = Unsigned((*_SREG)); \
      (*_PREG) = NEXTOP((*_PREG), x); \
      RESET_VARIABLE((*_SREG)); \
      (*_SREG)++; \
      GONext();

#define WRITE_VOID_INSTINIT \
      (*_PREG) = NEXTOP((*_PREG), e); \
      RESET_VARIABLE((*_SREG)); \
      (*_SREG)++; \
      GONext();

#define WRITE_N_VOIDS_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->u.s.s; \
      (*_PREG) = NEXTOP((*_PREG), s); \
      for (; d0 > 0; d0--) { \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
      } \
      GONext();

#define WRITE_Y_VAR_INSTINIT \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.y.y,Unsigned((*_SREG))); \
      (*_PREG) = NEXTOP((*_PREG), y); \
      RESET_VARIABLE((*_SREG)); \
      (*_SREG)++; \
      GONext();

#define WRITE_X_VAL_INSTINIT \
      register CELL d0; \
      d0 = XREG((*_PREG)->u.x.x); \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), x); \
      GONext();

#define WRITE_X_LOC_INSTINIT \
      register CELL d0; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.x.x); \
      (*_PREG) = NEXTOP((*_PREG), x);

#define WRITE_X_LOC_W_X_BOUND \
      *(*_SREG)++ = d0; \
      GONext();

#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#ifdef FROZEN_STACKS
#define WRITE_X_LOC_W_X_UNK \
      if (pt0 > HR && pt0<(CELL *)B_FZ) { \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONext(); \
      } \
      else { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONext(); \
      }
#else /* FROZEN_STACKS */
#define WRITE_X_LOC_W_X_UNK \
      if (pt0 > HR && pt0<(CELL *)B_FZ) { \
	TRAIL_LOCAL(pt0, Unsigned((*_SREG))); \
	*pt0 = Unsigned((*_SREG)); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONext(); \
      } \
      else { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONext(); \
      }
#endif /* FROZEN_STACKS */
#else /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#ifdef FROZEN_STACKS
#define WRITE_X_LOC_W_X_UNK \
      if (pt0 > HR) { \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONext(); \
      } \
      else { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONext(); \
      }
#else /* FROZEN_STACKS */
#define WRITE_X_LOC_W_X_UNK \
      if (pt0 > HR) { \
	TRAIL_LOCAL(pt0, Unsigned((*_SREG))); \
	*pt0 = Unsigned((*_SREG)); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONext(); \
      } \
      else { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONext(); \
      }
#endif /* FROZEN_STACKS */
#endif /*defined(YAPOR_SBA) && defined(FROZEN_STACKS) */

#ifdef YAPOR_SBA
#define WRITE_Y_VAL_INSTINIT \
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.y.y]; \
      if (d0 == 0) \
	*(*_SREG)++ = (CELL)(YREG+(*_PREG)->u.y.y); \
      else \
	*(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), y); \
      GONext();
#else /* YAPOR_SBA */
#define WRITE_Y_VAL_INSTINIT \
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.y.y]; \
	*(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), y); \
      GONext();
#endif /* YAPOR_SBA */

#define WRITE_Y_LOC_INSTINIT \
      register CELL d0; \
	  register CELL *pt0; \
      pt0 = YREG+(*_PREG)->u.y.y; \
      d0 = *pt0;

#define WRITE_Y_LOC_W_Y_BOUND \
      (*_PREG) = NEXTOP((*_PREG), y); \
      *(*_SREG)++ = d0; \
      GONext();

#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#ifdef FROZEN_STACKS
#define WRITE_Y_LOC_W_Y_UNK \
      if (pt0 > HR \
	  && pt0<(CELL *)B_FZ \
	  ) { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONext(); \
      } else { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*(*_SREG)++ = Unsigned(pt0); \
	GONext(); \
      }
#else /* FROZEN_STACKS */
#define WRITE_Y_LOC_W_Y_UNK \
      if (pt0 > HR \
	  && pt0<(CELL *)B_FZ \
	  ) { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*pt0 = Unsigned((*_SREG)); \
	TRAIL_LOCAL(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONext(); \
      } else { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*(*_SREG)++ = Unsigned(pt0); \
	GONext(); \
      }
#endif /* FROZEN_STACKS */
#else /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#ifdef FROZEN_STACKS
#define WRITE_Y_LOC_W_Y_UNK \
      if (pt0 > HR \
	  ) { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONext(); \
      } else { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*(*_SREG)++ = Unsigned(pt0); \
	GONext(); \
      }
#else /* FROZEN_STACKS */
#define WRITE_Y_LOC_W_Y_UNK \
      if (pt0 > HR \
	  ) { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*pt0 = Unsigned((*_SREG)); \
	TRAIL_LOCAL(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONext(); \
      } else { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*(*_SREG)++ = Unsigned(pt0); \
	GONext(); \
      }
#endif /* FROZEN_STACKS */
#endif /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */

#define WRITE_ATOM_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->u.c.c; \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), c); \
      GONext();

#define WRITE_BIGINT_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->u.N.b; \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), N); \
      GONext();

#define WRITE_DBTERM_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->u.D.D; \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), D); \
      GONext();

#define WRITE_FLOAT_INSTINIT \
      register CELL d0; \
      d0 = AbsAppl((*_PREG)->u.d.d); \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), d); \
      GONext();

#define WRITE_LONGIT_INSTINIT \
      register CELL d0; \
      d0 = AbsAppl((*_PREG)->u.i.i); \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), i); \
      GONext();

#define WRITE_N_ATOMS_INSTINIT \
      register CELL d0, d1; \
      d0 = (*_PREG)->u.sc.s; \
      d1 = (*_PREG)->u.sc.c; \
      for (; d0 > 0; d0--) { \
	*(*_SREG)++ = d1; \
	  } \
      (*_PREG) = NEXTOP((*_PREG), sc); \
      GONext();

#define WRITE_LIST_INSTINIT \
      register CELL d0; \
      d0 = AbsPair(HR); \
      *(*_SREG)++ = d0; \
      SP[-1] = Unsigned((*_SREG)); \
      SP[-2] = 1; \
      SP -= 2; \
      (*_SREG) = HR; \
      HR += 2; \
      (*_PREG) = NEXTOP((*_PREG), e); \
      GONext();

#define WRITE_L_LIST_INSTINIT \
      register CELL d0; \
      ALWAYS_START_PREFETCH(e); \
      (*_PREG) = NEXTOP((*_PREG), e); \
      CACHE_S(); \
      READ_IN_S(); \
      d0 = AbsPair(HR); \
      *S_SREG = d0; \
      WRITEBACK_S(HR); \
      HR += 2; \
      ENDCACHE_S(); \
      ALWAYS_GONext(); \
      ALWAYS_END_PREFETCH();

#define WRITE_STRUCT_INSTINIT \
      register CELL d0; \
      d0 = AbsAppl(HR); \
      *(*_SREG)++ = d0; \
      SP[-1] = Unsigned((*_SREG)); \
      SP[-2] = 1; \
      SP -= 2; \
      d0 = (CELL) ((*_PREG)->u.fa.f); \
      *H++ = d0; \
      d0 = (*_PREG)->u.fa.a; \
      (*_PREG) = NEXTOP((*_PREG), fa); \
      (*_SREG) = HR; \
      HR += d0; \
      GONext();

#define WRITE_L_STRUC_INSTINIT \
      register CELL d0; \
      d0 = AbsAppl(HR); \
      *(*_SREG) = d0; \
      d0 = (CELL) ((*_PREG)->u.fa.f); \
      *H++ = d0; \
      (*_SREG) = HR; \
      d0 = (*_PREG)->u.fa.a; \
      (*_PREG) = NEXTOP((*_PREG), fa); \
      HR += d0; \
      GONext();

