 
/**
 * @file   real.h
 * @date   Sat May 19 13:44:04 2018
 * 
 * @brief  Prolog  to R interface
 * 
 * 
 */

#ifdef __cplusplus
extern "C"{
#endif
  
bool R_isNull(SEXP sexp);

#if DEBUG_MEMORY
#define PROTECT_AND_COUNT(EXP)                                                 \
  {                                                                            \
    extern int R_PPStackTop;                                                   \
    PROTECT(EXP);                                                              \
    nprotect++;                                                                \
    printf("%s:%d +%d=%d\n", __FUNCTION__, __LINE__, nprotect, R_PPStackTop);  \
  }
#define Ureturn                                                                \
  {                                                                            \
    extern int R_PPStackTop;                                                   \
    printf("%s:%d -%d=%d\n", __FUNCTION__, __LINE__, nprotect,                 \
           R_PPStackTop - nprotect);                                           \
  }                                                                            \
  unprotect(nprotect);                                                         \
  return
#else
#define PROTECT_AND_COUNT(EXP)                                                 \
  {                                                                            \
    PROTECT(EXP);                                                              \
    nprotect++;                                                                \
  }
#define Ureturn                                                                \
  unprotect(nprotect);                                                         \
  return
#endif

// #define PL_free(v)

static inline SEXP protected_tryEval(SEXP expr, SEXP env, int *errp) {
  SEXP o;
  o = R_tryEval(expr, env, errp);
  return o ? o : expr;
}


  #ifndef term_t
#define term_t YAP_Int
  #endif

  
extern bool  sexp_to_pl(term_t t, SEXP s);
extern SEXP term_to_sexp(term_t t, bool eval);

#ifdef __cplusplus
}
#endif

