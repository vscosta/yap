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

#define _write_y_var_instinit \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.y.y,Unsigned((*_SREG))); \
      (*_PREG) = NEXTOP((*_PREG), y); \
      RESET_VARIABLE((*_SREG)); \
      (*_SREG)++; \
      GONEXT();

#define _write_x_val_instinit \
      register CELL d0; \
      d0 = XREG((*_PREG)->u.x.x); \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), x); \
      GONEXT();

#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#ifdef FROZEN_STACKS
#define _write_x_loc_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.x.x); \
      (*_PREG) = NEXTOP((*_PREG), x); \
      deref_head(d0, w_x_unk); \
    w_x_bound: \
      *(*_SREG)++ = d0; \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, w_x_unk, w_x_bound); \
      if (pt0 > HR && pt0<(CELL *)B_FZ) { \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONEXT(); \
      } \
      else { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _write_x_loc_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.x.x); \
      (*_PREG) = NEXTOP((*_PREG), x); \
      deref_head(d0, w_x_unk); \
    w_x_bound: \
      *(*_SREG)++ = d0; \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, w_x_unk, w_x_bound); \
      if (pt0 > HR && pt0<(CELL *)B_FZ) { \
	TRAIL_LOCAL(pt0, Unsigned((*_SREG))); \
	*pt0 = Unsigned((*_SREG)); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONEXT(); \
      } \
      else { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#ifdef FROZEN_STACKS
#define _write_x_loc_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.x.x); \
      (*_PREG) = NEXTOP((*_PREG), x); \
      deref_head(d0, w_x_unk); \
    w_x_bound: \
      *(*_SREG)++ = d0; \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, w_x_unk, w_x_bound); \
      if (pt0 > HR) { \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONEXT(); \
      } \
      else { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _write_x_loc_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.x.x); \
      (*_PREG) = NEXTOP((*_PREG), x); \
      deref_head(d0, w_x_unk); \
    w_x_bound: \
      *(*_SREG)++ = d0; \
      GONEXT(); \
 \
      BEGP(pt0); \
      deref_body(d0, pt0, w_x_unk, w_x_bound); \
      if (pt0 > HR) { \
	TRAIL_LOCAL(pt0, Unsigned((*_SREG))); \
	*pt0 = Unsigned((*_SREG)); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONEXT(); \
      } \
      else { \
	*(*_SREG)++ = Unsigned(pt0); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#endif /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */

#ifdef YAPOR_SBA
#define _write_y_val_instinit \
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.y.y]; \
      if (d0 == 0) \
	*(*_SREG)++ = (CELL)(YREG+(*_PREG)->u.y.y); \
      else \
	*(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), y); \
      GONEXT();
#else /* YAPOR_SBA */
#define _write_y_val_instinit \
      register CELL d0; \
      d0 = YREG[(*_PREG)->u.y.y]; \
	*(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), y); \
      GONEXT();
#endif /* YAPOR_SBA */

#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#ifdef FROZEN_STACKS
#define _write_y_loc_instinit \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = YREG+(*_PREG)->u.y.y; \
      d0 = *pt0; \
      deref_head(d0, w_y_unk); \
    w_y_bound: \
      (*_PREG) = NEXTOP((*_PREG), y); \
      *(*_SREG)++ = d0; \
      GONEXT(); \
 \
      derefa_body(d0, pt0, w_y_unk, w_y_bound); \
      if (pt0 > HR \
	  && pt0<(CELL *)B_FZ \
	  ) { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONEXT(); \
      } else { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*(*_SREG)++ = Unsigned(pt0); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _write_y_loc_instinit \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = YREG+(*_PREG)->u.y.y; \
      d0 = *pt0; \
      deref_head(d0, w_y_unk); \
    w_y_bound: \
      (*_PREG) = NEXTOP((*_PREG), y); \
      *(*_SREG)++ = d0; \
      GONEXT(); \
 \
      derefa_body(d0, pt0, w_y_unk, w_y_bound); \
      if (pt0 > HR \
	  && pt0<(CELL *)B_FZ \
	  ) { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*pt0 = Unsigned((*_SREG)); \
	TRAIL_LOCAL(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONEXT(); \
      } else { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*(*_SREG)++ = Unsigned(pt0); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#else /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#ifdef FROZEN_STACKS
#define _write_y_loc_instinit \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = YREG+(*_PREG)->u.y.y; \
      d0 = *pt0; \
      deref_head(d0, w_y_unk); \
    w_y_bound: \
      (*_PREG) = NEXTOP((*_PREG), y); \
      *(*_SREG)++ = d0; \
      GONEXT(); \
 \
      derefa_body(d0, pt0, w_y_unk, w_y_bound); \
      if (pt0 > HR \
	  ) { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	Bind_Local(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONEXT(); \
      } else { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*(*_SREG)++ = Unsigned(pt0); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);
#else /* FROZEN_STACKS */
#define _write_y_loc_instinit \
      BEGD(d0); \
      BEGP(pt0); \
      pt0 = YREG+(*_PREG)->u.y.y; \
      d0 = *pt0; \
      deref_head(d0, w_y_unk); \
    w_y_bound: \
      (*_PREG) = NEXTOP((*_PREG), y); \
      *(*_SREG)++ = d0; \
      GONEXT(); \
 \
      derefa_body(d0, pt0, w_y_unk, w_y_bound); \
      if (pt0 > HR \
	  ) { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*pt0 = Unsigned((*_SREG)); \
	TRAIL_LOCAL(pt0, Unsigned((*_SREG))); \
	RESET_VARIABLE((*_SREG)); \
	(*_SREG)++; \
	GONEXT(); \
      } else { \
	(*_PREG) = NEXTOP((*_PREG), y); \
	*(*_SREG)++ = Unsigned(pt0); \
	GONEXT(); \
      } \
      ENDP(pt0); \
      ENDD(d0);
#endif /* FROZEN_STACKS */
#endif /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */

#define _write_atom_instinit \
      register CELL d0; \
      d0 = (*_PREG)->u.c.c; \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), c); \
      GONEXT();

#define WRITE_BIGINT_INSTINIT \
      register CELL d0; \
      d0 = (*_PREG)->u.N.b; \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), N); \
      GONext();

#define _write_dbterm_instinit \
      register CELL d0; \
      d0 = (*_PREG)->u.D.D; \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), D); \
      GONEXT();

#define _write_float_instinit \
      register CELL d0; \
      d0 = AbsAppl((*_PREG)->u.d.d); \
      *(*_SREG)++ = d0; \
      (*_PREG) = NEXTOP((*_PREG), d); \
      GONEXT();

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

#define _write_l_list_instinit \
      register CELL d0; \
      (*_PREG) = NEXTOP((*_PREG), e); \
      CACHE_S(); \
      READ_IN_S(); \
      d0 = AbsPair(HR); \
      *S_SREG = d0; \
      WRITEBACK_S(HR); \
      HR += 2; \
      ENDCACHE_S(); \
      GONEXT(); \

#define _write_struct_instinit \
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
      GONEXT();

#define _write_l_struc_instinit \
      register CELL d0; \
      d0 = AbsAppl(HR); \
      *(*_SREG) = d0; \
      d0 = (CELL) ((*_PREG)->u.fa.f); \
      *H++ = d0; \
      (*_SREG) = HR; \
      d0 = (*_PREG)->u.fa.a; \
      (*_PREG) = NEXTOP((*_PREG), fa); \
      HR += d0; \
      GONEXT();

