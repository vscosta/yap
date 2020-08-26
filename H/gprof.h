/* gprof.c */


typedef enum {
  GPROF_NO_EVENT,
  GPROF_NEW_PRED_FUNC,
  GPROF_NEW_PRED_THREAD,
  GPROF_NEW_PRED_ATOM,
  GPROF_INDEX,
  GPROF_INDEX_EXPAND,
  GPROF_CLAUSE,
  GPROF_MEGA,
  GPROF_LU_INDEX,
  GPROF_STATIC_INDEX,
  GPROF_INIT_OPCODE,
  GPROF_INIT_SYSTEM_CODE,
  GPROF_INIT_EXPAND,
  GPROF_INIT_LOG_UPD_CLAUSE,
  GPROF_INIT_DYNAMIC_CLAUSE,
  GPROF_INIT_STATIC_CLAUSE,
  GPROF_INIT_COMMA,
  GPROF_INIT_FAIL,
  GPROF_NEW_LU_CLAUSE,
  GPROF_NEW_LU_SWITCH,
  GPROF_NEW_STATIC_SWITCH,
  GPROF_NEW_EXPAND_BLOCK
} gprof_info;

extern void Yap_InitLowProf(void);
#if LOW_PROF
extern void Yap_inform_profiler_of_clause__(void *, void *, struct pred_entry *,
                                     gprof_info);
#define Yap_inform_profiler_of_clause(CODE0, CODEF, AP, MODE)                  \
  {                                                                            \
    if (GLOBAL_FPreds)                                                         \
      Yap_inform_profiler_of_clause__(CODE0, CODEF, AP, MODE);                 \
  }
#else
#define Yap_inform_profiler_of_clause(CODE0, CODEF, AP, MODE)
#endif
extern void Yap_tell_gprof(yamop *);
