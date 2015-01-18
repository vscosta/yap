#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack_on_cut \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) return external_labels[1];
#else
#define check_stack_on_cut \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) return external_labels[1];
#endif /* YAPOR_SBA && YAPOR */

#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack_on_cutt \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) return external_labels[3];
#else
#define check_stack_on_cutt \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) return external_labels[3];
#endif /* YAPOR_SBA && YAPOR */

#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack_on_commitx \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) return external_labels[11];
#else
#define check_stack_on_commitx \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) return external_labels[11];
#endif /* YAPOR_SBA && YAPOR */

#ifdef COROUTINING
#define _cut_instinit \
      if (FALSE) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_cut; \
	ENDCACHE_Y_AS_ENV(); \
      } \
    do_cut: \
      SET_ASP(YREG, (*_PREG)->u.s.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), s),Osbpp),l); \
      saveregs(); \
      prune((choiceptr)YREG[E_CB]); \
      setregs(); \
      GONEXT();
#else /* COROUTINING */
#define _cut_instinit \
      SET_ASP(YREG, (*_PREG)->u.s.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), s),Osbpp),l); \
      saveregs(); \
      prune((choiceptr)YREG[E_CB]); \
      setregs(); \
      GONEXT();
#endif /* COROUTINING */

#ifdef COROUTINING
#define _cut_t_instinit \
      if (FALSE) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_cutt; \
	ENDCACHE_Y_AS_ENV(); \
      } \
    do_cut_t: \
      SET_ASP(YREG, (*_PREG)->u.s.s); \
      saveregs(); \
      prune((choiceptr)YREG[E_CB]); \
      setregs(); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), s),Osbpp),l); \
      GONEXT();
#else /* COROUTINING */
#define _cut_t_instinit \
      SET_ASP(YREG, (*_PREG)->u.s.s); \
      saveregs(); \
      prune((choiceptr)YREG[E_CB]); \
      setregs(); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), s),Osbpp),l); \
      GONEXT();
#endif /* COROUTINING */

#define CUT_E_INSTINIT

#ifdef COROUTINING
#define CUT_E_COROUTINING \
	CACHE_Y_AS_ENV(YREG); \
	check_stack(NoStackCutE, HR); \
	ENDCACHE_Y_AS_ENV();
#endif

#define CUT_E_NOCOROUTINING \
      SET_ASP(YREG, (*_PREG)->u.s.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), s),Osbpp),l); \
      saveregs(); \
      prune((choiceptr)(*_SREG)[E_CB]); \
      setregs(); \
      GONext();

#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#define _save_b_x_instinit \
      BEGD(d0); \
      d0 = (*_PREG)->u.x.x; \
      XREG(d0) = MkIntegerTerm((Int)B); \
      (*_PREG) = NEXTOP((*_PREG), x); \
      ENDD(d0); \
      GONEXT();
#else /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#define _save_b_x_instinit \
      BEGD(d0); \
      d0 = (*_PREG)->u.x.x; \
      XREG(d0) = MkIntegerTerm(LCL0-(CELL *) (B)); \
      (*_PREG) = NEXTOP((*_PREG), x); \
      ENDD(d0); \
      GONEXT();
#endif /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */

#if defined(YAPOR_SBA)
#define _save_b_y_instinit \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.y.y,MkIntegerTerm((Int)B)); \
      (*_PREG) = NEXTOP((*_PREG), y); \
      GONEXT();
#else /* defined(YAPOR_SBA) */
#define _save_b_y_instinit \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.y.y,MkIntegerTerm(LCL0-(CELL *)(B))); \
      (*_PREG) = NEXTOP((*_PREG), y); \
      GONEXT();
#endif /* defined(YAPOR_SBA) */

#ifdef COROUTINING
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#define _commit_b_x_instinit \
      CACHE_Y_AS_ENV(YREG); \
      check_stack_on_commitx; \
      ENDCACHE_Y_AS_ENV(); \
    do_commit_b_x: \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xps.x); \
      deref_head(d0, commit_b_x_unk); \
    commit_b_x_nvar: \
      SET_ASP(YREG, (*_PREG)->u.xps.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xps),Osbpp),l); \
      { \
	choiceptr pt0; \
	pt0 = (choiceptr)IntegerOfTerm(d0); \
	saveregs(); \
	prune(pt0); \
	setregs(); \
      } \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d0, pt1, commit_b_x_unk, commit_b_x_nvar); \
      ENDP(pt1); \
      FAIL(); \
      ENDD(d0);
#else /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#define _commit_b_x_instinit \
      CACHE_Y_AS_ENV(YREG); \
      check_stack_on_commitx; \
      ENDCACHE_Y_AS_ENV(); \
    do_commit_b_x: \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xps.x); \
      deref_head(d0, commit_b_x_unk); \
    commit_b_x_nvar: \
      SET_ASP(YREG, (*_PREG)->u.xps.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xps),Osbpp),l); \
      { \
	choiceptr pt0; \
	pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0)); \
	saveregs(); \
	prune(pt0); \
	setregs(); \
      } \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d0, pt1, commit_b_x_unk, commit_b_x_nvar); \
      ENDP(pt1); \
      FAIL(); \
      ENDD(d0);
#endif /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#else /* COROUTINING */
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#define _commit_b_x_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xps.x); \
      deref_head(d0, commit_b_x_unk); \
    commit_b_x_nvar: \
      SET_ASP(YREG, (*_PREG)->u.xps.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xps),Osbpp),l); \
      { \
	choiceptr pt0; \
	pt0 = (choiceptr)IntegerOfTerm(d0); \
	saveregs(); \
	prune(pt0); \
	setregs(); \
      } \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d0, pt1, commit_b_x_unk, commit_b_x_nvar); \
      ENDP(pt1); \
      FAIL(); \
      ENDD(d0);
#else /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#define _commit_b_x_instinit \
      BEGD(d0); \
      d0 = XREG((*_PREG)->u.xps.x); \
      deref_head(d0, commit_b_x_unk); \
    commit_b_x_nvar: \
      SET_ASP(YREG, (*_PREG)->u.xps.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xps),Osbpp),l); \
      { \
	choiceptr pt0; \
	pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0)); \
	saveregs(); \
	prune(pt0); \
	setregs(); \
      } \
      GONEXT(); \
 \
      BEGP(pt1); \
      deref_body(d0, pt1, commit_b_x_unk, commit_b_x_nvar); \
      ENDP(pt1); \
      FAIL(); \
      ENDD(d0);
#endif /* defined(YAPOR_SBA) && defined(FROZEN_STACKS) */
#endif /* COROUTINING */

#define COMMIT_B_Y_INSTINIT \
      register CELL d0; \
      register CELL *pt1;

#define COMMIT_B_Y_DO_COMMIT_B_Y \
      d0 = YREG[(*_PREG)->u.yps.y];

#define COMMIT_B_Y_COMMIT_B_Y_NVAR \
      SET_ASP(YREG, (*_PREG)->u.yps.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yps),Osbpp),l); \
	choiceptr pt0;
	
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#define COMMIT_B_Y_YSBA_FROZEN \
	pt0 = (choiceptr)IntegerOfTerm(d0);
#else
#define COMMIT_B_Y_NOYSBA_NOFROZEN \
	pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0));
#endif

#define COMMIT_B_Y_POST_YSBA_FROZEN \
	saveregs(); \
	prune(pt0); \
	setregs(); \
      GONext();

