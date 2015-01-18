#define JIT_HANDLER_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      print_main_when_head((*_PREG), ON_NATIVE); \
      (*_PREG) = NEXTOP((*_PREG), jhc);

#define I_R (XREGS[0])

#define YAAM_CHECK_TRAIL_TR  { check_trail(TR); }

#define YAAM_DEREF_BODY_D0PT0 \
    if (!IsVarTerm(d0)) { BACK(); } \
      pt0 = (CELL *)d0; \
      d0 = *(CELL *)d0;

#define YAAM_DEREF_BODY_D0PT1 \
    if (!IsVarTerm(d0)) { BACK(); } \
      pt1 = (CELL *)d0; \
      d0 = *(CELL *)d0;

#define YAAM_DEREF_BODY_D1PT0 \
    if (!IsVarTerm(d1)) { BACK(); } \
      pt0 = (CELL *)d1; \
      d1 = *(CELL *)d1;

#define YAAM_DEREF_BODY_D1PT1 \
    if (!IsVarTerm(d1)) { BACK(); } \
      pt1 = (CELL *)d1; \
      d1 = *(CELL *)d1;

#define YAAM_UNIFYBOUND \
  if (d0 == d1) { \
    GONext(); \
  } \
  else if (IsPairTerm(d0)) { \
    if (!IsPairTerm(d1)) { \
      YAAM_FAIL; \
    } \
    else { \
      register CELL *ipt0, *ipt1; \
      ipt0 = RepPair(d0); \
      ipt1 = RepPair(d1); \
      save_hb(); \
      always_save_pc(); \
      if (IUnify_complex(ipt0-1,ipt0+1,ipt1-1)) { \
        always_set_pc(); \
        GONext(); \
      } \
      else { \
        YAAM_FAIL; \
      } \
    } \
  } \
  else if (IsApplTerm(d0)) { \
    if (!IsApplTerm(d1)) { \
      YAAM_FAIL; \
    } \
    else { \
      register CELL *ipt0, *ipt1; \
      register Functor f; \
      ipt0 = RepAppl(d0); \
      ipt1 = RepAppl(d1); \
      f = (Functor)*ipt0; \
      if (f != (Functor)*ipt1) { \
        YAAM_FAIL; \
      } \
      else if (IsExtensionFunctor(f)) { \
        if (unify_extension(f, d0, RepAppl(d0), d1)) { \
          GONext(); \
        } \
        else { \
          YAAM_FAIL; \
        } \
      } \
      else { \
        d0 = ArityOfFunctor(f); \
        always_save_pc(); \
        save_hb(); \
        if (IUnify_complex(ipt0, ipt0+d0, ipt1)) { \
          always_set_pc(); \
          GONext(); \
        } \
        else { \
          YAAM_FAIL; \
        } \
      } \
    } \
  } \
  else { \
    YAAM_FAIL; \
  }

#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define NoStackDeallocate_Exception                            \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[8]; \
  }

#define NoStackCall_Exception                            \
  CACHE_Y_AS_ENV(YREG); \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[6]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackDExecute_Exception                            \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[7]; \
  }

#define NoStackExecute_Exception                            \
  CACHE_Y_AS_ENV(YREG); \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[5]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackFail_Exception                            \
  CACHE_Y_AS_ENV(YREG); \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[10]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackEither_Exception                            \
  CACHE_Y_AS_ENV(YREG); \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[4]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackCommitY_Exception                            \
  CACHE_Y_AS_ENV(YREG); \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[2]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackCommitX_Exception                            \
  CACHE_Y_AS_ENV(YREG); \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[11]; \
  } \
  ENDCACHE_Y_AS_ENV();

#else
#define NoStackDeallocate_Exception                             \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[8]; \
  }

#define NoStackCall_Exception                             \
  CACHE_Y_AS_ENV(YREG); \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[6]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackDExecute_Exception                             \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[7]; \
  }

#define NoStackExecute_Exception                             \
  CACHE_Y_AS_ENV(YREG); \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[5]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackFail_Exception                             \
  CACHE_Y_AS_ENV(YREG); \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[10]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackEither_Exception                             \
  CACHE_Y_AS_ENV(YREG); \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[4]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackCommitY_Exception                             \
  CACHE_Y_AS_ENV(YREG); \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[2]; \
  } \
  ENDCACHE_Y_AS_ENV();

#define NoStackCommitX_Exception                             \
  CACHE_Y_AS_ENV(YREG); \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { \
	  if ((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != 0 && (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap != (char*)0x1) { \
	    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
	    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.native_treat_heap); \
	  } \
      return external_labels[11]; \
  } \
  ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA && YAPOR */

#define YAAM_FAIL \
      FAILED = 1;
