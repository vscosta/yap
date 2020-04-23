 
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
  

#define PL_R_BOOL (1)      /* const char * */
#define PL_R_CHARS (2)     /* const char * */
#define PL_R_INTEGER (3)   /* int */
#define PL_R_FLOAT (4)     /* double */
#define PL_R_COMPLEX (5)   /* x + yi * */
#define PL_R_SYMBOL (6)    /* A * */
#define PL_R_CALL (7)      /* A(F) * */
#define PL_R_LISTEL (8)    /* X$listEl * */
#define PL_R_SLOT (9)      /* X@slot * */
#define PL_R_NAME (10)     /* name = X, just within a list * */
#define PL_R_PLUS (11)     /* +X * */
#define PL_R_PSYMBOL (12)  /* -X * */
#define PL_R_ATBOOL (13)   /* @X * */
#define PL_R_VARIABLE (14) /* _ */
#define PL_R_SUBSET (15)   /* [] */
#define PL_R_DOT (16)      /* . */
#define PL_R_DEFUN (17)    /* function(_,_,_) -> ... */
#define PL_R_QUOTE (18)    /* quote(_) */
#define PL_R_INNER (19)    /* %i% */
#define PL_R_OUTER (20)    /* %o% */
#define PL_R_FORMULA (21)  /* At ~ Exp */
#define PL_R_IF (22)       /* if(Cond, Then)  */
#define PL_R_IF_ELSE (23)  /* if(Cond, Then, Else)  */
#define PL_R_FOR (26)      /* for(I in Cond, Expr)  */
#define PL_R_WHILE (27)    /* while(Cond, Expr)  */
#define PL_R_REPEAT (28)   /* repeat(Expr)  */
#define PL_R_NEXT (29)     /* next  */
#define PL_R_BREAK (30)    /* break  */
#define PL_R_IN (31)       /* break  */
#define PL_R_RFORMULA (32) /* ~ Exp */
#define PL_R_EQUAL (33)    /* ~ Exp */
#define PL_R_VECTOR (256)  /* [.....] * */

#define REAL_Error(s, t) REAL_Error__(__LINE__, __FUNCTION__, s, t)

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

