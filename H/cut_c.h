#ifndef __CUT_C_H__
#define __CUT_C_H__

/* Some definitions */
#define Choice_Point_Type void *

/* necessary for not redefine NULL*/
#ifndef NULL
#define NULL nil
#endif

typedef struct cut_c_str *cut_c_str_ptr;
struct cut_c_str {
  cut_c_str_ptr before;
  void *try_userc_cut_yamop;
};

#define CUT_C_STR_SIZE ((sizeof(struct cut_c_str)) / (sizeof(CELL)))

#define EXTRA_CBACK_CUT_ARG(Type, Offset) EXTRA_CBACK_ARG(PP->ArityOfPE, Offset)

#define CBACK_CUT_ARG(Offset) B->cp_args[(Offset)-1]

#define CUT_C_PUSH(YAMOP, S_YREG)                                              \
  {                                                                            \
    if ((YAMOP)->y_u.OtapFs.f) {                                               \
      S_YREG = S_YREG - CUT_C_STR_SIZE;                                        \
      cut_c_str_ptr new_top = (cut_c_str_ptr)S_YREG;                           \
      new_top->try_userc_cut_yamop = YAMOP;                                    \
      cut_c_push(new_top);                                                     \
    }                                                                          \
  }

#define POP_CHOICE_POINT(cp)                                                   \
  (((CELL *)Yap_REGS.CUT_C_TOP != (CELL *)LOCAL_LocalBase) &&                  \
   ((CELL *)(cp) > (CELL *)Yap_REGS.CUT_C_TOP))

#define POP_EXECUTE()                                                          \
  cut_c_str_ptr TOP = Yap_REGS.CUT_C_TOP;                                      \
  CPredicate func =                                                            \
      (CPredicate)((yamop *)TOP->try_userc_cut_yamop)->y_u.OtapFs.f;           \
  PredEntry *pred =                                                            \
      (PredEntry *)((yamop *)TOP->try_userc_cut_yamop)->y_u.OtapFs.p;          \
  YAP_ExecuteOnCut(pred, func, TOP);                                           \
  cut_c_pop();

#define POP_FAIL(handler)                                                      \
  if (handler) { yamop *oap = handler->cp_ap;                                               \
handler->cp_ap = NOCODE;                                                       \
  P = (yamop *)FAILCODE;                                                       \
  choiceptr olB = B; B = handler;                                                                 \
  HR = handler->cp_h;                                                          \
  /*  DBTerm *ref = Yap_RefToException();           */                         \
  Yap_exec_absmi(true, false);                                                 \
  B = olB; handler->cp_ap = oap; }

#define POP_FAIL_EXECUTE(handler)                                              \
  POP_FAIL(handler);                                                           \
  cut_c_str_ptr TOP = Yap_REGS.CUT_C_TOP;                                      \
  CPredicate func =                                                            \
      (CPredicate)((yamop *)TOP->try_userc_cut_yamop)->y_u.OtapFs.f;           \
  PredEntry *pred =                                                            \
      (PredEntry *)((yamop *)TOP->try_userc_cut_yamop)->y_u.OtapFs.p;          \
  YAP_ExecuteOnCut(pred, func, TOP);                                           \
  cut_c_pop();

/*Initializes CUT_C_TOP*/
void cut_c_initialize(int wid);

/*Removes a choice_point from the stack*/
void cut_c_pop(void);

/*Insert a choice_point in the stack*/
void cut_c_push(cut_c_str_ptr);

#endif /*_CUT_C_H__*/
