#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack_on_fail \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) return external_labels[10];
#else
#define check_stack_on_fail \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) return external_labels[10];
#endif /* YAPOR_SBA && YAPOR */

#define GONEXT() \
  if (idx != -1) goto *NOp_Address_R[idx]; \
  else BACK();
  
#define GONEXTW() \
  if (idx != -1) goto *NOp_Address_W[idx]; \
  else BACK();
  
#define YAAM_UnifyBound_TEST_ATTACHED(f,d0,pt0,d1)                          \
 if (IsExtensionFunctor(f)) {                                          \
   if (unify_extension(f, d0, RepAppl(d0), d1))                        \
        { GONEXT(); }                                                  \
      else                                                             \
        { FAIL(); }                                                    \
    }

#define YAAM_UnifyBound(d0,d1)                                         \
  if (d0 == d1) { GONEXT(); }                                          \
  if (IsPairTerm(d0)) {                                                \
    register CELL *ipt0, *ipt1;                                        \
    if (!IsPairTerm(d1)) { FAIL(); }                                   \
    ipt0 = RepPair(d0);                                                \
    ipt1 = RepPair(d1);                                                \
    save_hb();							       \
    always_save_pc();						       \
    if (IUnify_complex(ipt0-1,ipt0+1,ipt1-1)) {always_set_pc(); GONEXT();}\
    else { FAIL(); }                                                   \
  } else if (IsApplTerm(d0)) {                                         \
    register CELL *ipt0, *ipt1;                                        \
    register Functor f;                                                \
    if (!IsApplTerm(d1)) { FAIL(); }                                   \
    ipt0 = RepAppl(d0);                                                \
    ipt1 = RepAppl(d1);                                                \
    f = (Functor)*ipt0;                                                \
    if (f != (Functor)*ipt1) { FAIL(); }                               \
    YAAM_UnifyBound_TEST_ATTACHED(f,d0,ipt0,d1);                       \
    d0 = ArityOfFunctor(f);                                            \
    always_save_pc();						       \
    save_hb();							       \
    if (IUnify_complex(ipt0, ipt0+d0, ipt1)) {always_set_pc(); GONEXT();} \
    else { FAIL(); }                                                   \
  }                                                                    \
  else { FAIL(); }

#define _native_me_instinit \
 (*_PREG) = NEXTOP((*_PREG), aFlp); \
 GONEXT();
 
#ifdef COROUTINING
#define _op_fail_instinit \
      if (PP) { \
	UNLOCK(PP->PELock); \
	PP = NULL; \
      } \
      CACHE_Y_AS_ENV(YREG); \
      check_stack_on_fail; \
      ENDCACHE_Y_AS_ENV(); \
      FAIL();
#else /* COROUTINING */
#define _op_fail_instinit \
      if (PP) { \
	UNLOCK(PP->PELock); \
	PP = NULL; \
      } \
      FAIL();
#endif /* COROUTINING */

#define I_R (XREGS[0])
