/*************************************************************************
 *									 *
 *	 Yap Prolog 							 *
 *									 *
 *	Yap Prolog Was Developed At Nccup - Universidade Do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa And Universidade Do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		errors.c *
 * Last Rev:								 *
 * Mods: *
 * Comments:	Yap's error handlers					 *
 *									 *
 *************************************************************************/

#include "absmi.h"
#include "yapio.h"
#include "YapStreams.h"
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_EXECINFO_H
#include <execinfo.h>
#endif
#include "Foreign.h"

#define set_key_b(k, ks, q, i, t)                                              \
  if (strcmp(ks, q) == 0) {                                                    \
    i->k = ( t == TermTrue ? true : false);				\
    return i->k || t == TermFalse;                                             \
  }

#define set_key_i(k, ks, q, i, t)                                              \
  if (strcmp(ks, q) == 0) {                                                    \
    i->k = IsIntegerTerm(t) ? IntegerOfTerm(t) : 0;   \
    return IsIntegerTerm(t);			      \
  }

#define set_key_s(k, ks, q, i, t)                                              \
  if (strcmp(ks, q) == 0) {                                                    \
    const char *s = IsAtomTerm(t) ? RepAtom(AtomOfTerm(t))->StrOfAE            \
                                  : IsStringTerm(t) ? StringOfTerm(t) : NULL;  \
    if (s) {                                                                   \
      char *tmp = calloc(1,strlen(s) + 1);                                       \
      strcpy(tmp, s);                                                          \
      i->k = tmp;                                                              \
    }                                                                          \
    return i->k != NULL;                                                       \
  }

static bool setErr(const char *q, yap_error_descriptor_t *i, Term t) {
  set_key_i(errorNo, "errorNo", q, i, t);
  set_key_i(errorClass, "errorClass", q, i, t);
  set_key_s(errorAsText, "errorAsText", q, i, t);
  set_key_s(errorGoal, "errorGoal", q, i, t);
  set_key_s(classAsText, "classAsText", q, i, t);
  set_key_i(errorLine, "errorLine", q, i, t);
  set_key_s(errorFunction, "errorFunction", q, i, t);
  set_key_s(errorFile, "errorFile", q, i, t);
  set_key_i(prologPredLine, "prologPredLine", q, i, t);
  set_key_s(prologPredName, "prologPredName", q, i, t);
  set_key_i(prologPredArity, "prologPredArity", q, i, t);
  set_key_s(prologPredModule, "prologPredModule", q, i, t);
  set_key_s(prologPredFile, "prologPredFile", q, i, t);
  set_key_i(parserPos, "parserPos", q, i, t);
  set_key_i(parserLine, "parserLine", q, i, t);
  set_key_i(parserFirstLine, "parserFirstLine", q, i, t);
  set_key_i(parserLastLine, "parserLastLine", q, i, t);
  set_key_s(parserTextA, "parserTextA", q, i, t);
  set_key_s(parserTextB, "parserTextB", q, i, t);
  set_key_s(parserFile, "parserFile", q, i, t);
  set_key_b(parserReadingCode, "parserReadingcode", q, i, t);
  set_key_b(prologConsulting, "prologConsulting", q, i, t);
  set_key_s(culprit, "culprit", q, i, t);
  set_key_s(prologStack, "prologStack", q, i, t);
  set_key_s(errorMsg, "errorMsg", q, i, t);
  set_key_i(errorMsgLen, "errorMsgLen", q, i, t);
  return false;
}

#define query_key_b(k, ks, q, i)  \
if (strcmp(ks, q) == 0) { \
    return i->k ? TermTrue : TermFalse;                                        \
  }

#define query_key_i(k, ks, q, i) \
if (strcmp(ks, q) == 0) {                                                    \
    return MkIntegerTerm(i->k);                                                \
  }

#define query_key_s(k, ks, q, i)  \
if (strcmp(ks, q) == 0 ) \
{  if (i->k) return MkAtomTerm(Yap_LookupAtom(i->k)); else return TermEmptyAtom; }


#define query_key_t(k, ks, q, i)     \
  if (strcmp(ks, q) == 0) {                                                    \
  if (i->k == NULL) return TermNil; \
 Term t; if((t = Yap_BufferToTerm(i->k, TermNil) ) == 0 ) return TermNil; return t; }

static yap_error_descriptor_t *CopyException(yap_error_descriptor_t *t);
  

static Term queryErr(const char *q, yap_error_descriptor_t *i) {
  query_key_i(errorNo, "errorNo", q, i);
  query_key_i(errorClass, "errorClass", q, i);
  query_key_s(errorAsText, "errorAsText", q, i);
  query_key_t(errorGoal, "errorGoal", q, i);
  query_key_s(classAsText, "classAsText", q, i);
  query_key_i(errorLine, "errorLine", q, i);
  query_key_s(errorFunction, "errorFunction", q, i);
  query_key_s(errorFile, "errorFile", q, i);
  query_key_i(prologPredLine, "prologPredLine", q, i);
  query_key_s(prologPredName, "prologPredName", q, i);
  query_key_i(prologPredArity, "prologPredArity", q, i);
  query_key_s(prologPredModule, "prologPredModule", q, i);
  query_key_s(prologPredFile, "prologPredFile", q, i);
  query_key_i(parserPos, "parserPos", q, i);
  query_key_i(parserLine, "parserLine", q, i);
  query_key_i(parserFirstLine, "parserFirstLine", q, i);
  query_key_i(parserLastLine, "parserLastLine", q, i);
  query_key_s(parserTextA, "parserTextA", q, i);
  query_key_s(parserTextB, "parserTextB", q, i);
  query_key_s(parserFile, "parserFile", q, i);
  query_key_b(parserReadingCode, "parserReadingCode", q, i);
  query_key_b(prologConsulting, "prologConsulting", q, i);
  query_key_s(prologStack, "prologStack", q, i);
  query_key_s(culprit, "culprit", q, i);
  query_key_s(errorMsg, "errorMsg", q, i);
  query_key_i(errorMsgLen, "errorMsgLen", q, i);
  return TermNil;
}

static void print_key_b(const char *key, bool v) {
  const char *b = v ? "true" : "false";
  fprintf(stderr, "%s: %s\n", key, b);
}

static void print_key_i(const char *key, YAP_Int v) {
  fprintf(stderr, "%s: " Int_FORMAT "\n", key, v);
}

static void print_key_s(const char *key, const char *v) {
  fprintf(stderr, "%s: %s\n", key, v);
}

static void printErr(yap_error_descriptor_t *i) {

  if (i->errorNo == YAP_NO_ERROR) {
    return;
  }
  print_key_i("errorNo", i->errorNo);
  print_key_i("errorClass", (i->errorClass = Yap_errorClass(i->errorNo)));
  print_key_s("errorAsText", (i->errorAsText=Yap_errorName(i->errorNo) ) );
  print_key_s("errorGoal", i->errorGoal);
  print_key_s("classAsText", (i->classAsText=Yap_errorClassName(i->errorClass)));
  print_key_i("errorLine", i->errorLine);
  print_key_s("errorFunction", i->errorFunction);
  print_key_s("errorFile", i->errorFile);
  print_key_i("prologPredLine", i->prologPredLine);
  print_key_s("prologPredName", i->prologPredName);
  print_key_i("prologPredArity", i->prologPredArity);
  print_key_s("prologPredModule", i->prologPredModule);
  print_key_s("prologPredFile", i->prologPredFile);
  print_key_i("parserPos", i->parserPos);
  print_key_i("parserLine", i->parserLine);
  print_key_i("parserFirstLine", i->parserFirstLine);
  print_key_i("parserLastLine", i->parserLastLine);
  print_key_s("parserTextA", i->parserTextA);
  print_key_s("parserTextB", i->parserTextB);
  print_key_s("parserFile", i->parserFile);
  print_key_b("parserReadingCode", i->parserReadingCode);
  print_key_b("prologConsulting", i->prologConsulting);
  print_key_s("culprit", i->culprit);
  print_key_s("prologStack", i->prologStack);
  if (i->errorMsgLen) {
    print_key_s("errorMsg", i->errorMsg);
    print_key_i("errorMsgLen", i->errorMsgLen);
  }
}

static YAP_Term add_key_b(const char *key, bool v, YAP_Term o0) {
  YAP_Term tkv[2];
  tkv[1] = v ? TermTrue : TermFalse;
  tkv[0] = MkStringTerm(key);
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static YAP_Term add_key_i(const char *key, YAP_Int v, YAP_Term o0) {
  YAP_Term tkv[2];
  tkv[1] = MkIntegerTerm(v), tkv[0] = MkStringTerm(key);
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static YAP_Term add_key_s(const char *key, const char *v, YAP_Term o0) {
  Term tkv[2];
  if (!v || v[0] == '\0')
    return o0;
  tkv[1] = MkStringTerm(v), tkv[0] = MkStringTerm(key);
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static Term err2list(yap_error_descriptor_t *i) {
  Term o = TermNil;
  if (i->errorNo == YAP_NO_ERROR) {
    return o;
  }
  o = add_key_i("errorNo", i->errorNo, o);
  o = add_key_i("errorClass", i->errorClass, o);
  o = add_key_s("errorAsText", i->errorAsText, o);
  o = add_key_s("errorGoal", i->errorGoal, o);
  o = add_key_s("classAsText", i->classAsText, o);
  o = add_key_i("errorLineq", i->errorLine, o);
  o = add_key_s("errorFunction", i->errorFunction, o);
  o = add_key_s("errorFile", i->errorFile, o);
  o = add_key_i("prologPredLine", i->prologPredLine, o);
  o = add_key_s("prologPredName", i->prologPredName, o);
  o = add_key_i("prologPredArity", i->prologPredArity, o);
  o = add_key_s("prologPredModule", i->prologPredModule, o);
  o = add_key_s("prologPredFile", i->prologPredFile, o);
  o = add_key_i("parserPos", i->parserPos, o);
  o = add_key_i("parserLine", i->parserLine, o);
  o = add_key_i("parserFirstLine", i->parserFirstLine, o);
  o = add_key_i("parserLastLine", i->parserLastLine, o);
  o = add_key_s("parserTextA", i->parserTextA, o);
  o = add_key_s("parserTextB", i->parserTextB, o);
  o = add_key_s("parserFile", i->parserFile, o);
  o = add_key_b("parserReadingCode", i->parserReadingCode, o);
  o = add_key_b("prologConsulting", i->prologConsulting, o);
  o = add_key_s("culprit", i->culprit, o);
  o = add_key_s("prologStack", i->prologStack, o);
  if (i->errorMsgLen) {
    o = add_key_s("errorMsg", i->errorMsg, o);
    o = add_key_i("errorMsgLen", i->errorMsgLen, o);
  }
  return o;
}

bool Yap_Warning(const char *s, ...) {
  CACHE_REGS
  va_list ap;
  PredEntry *pred;
  bool rc;
  Term ts[2];
  const char *fmt;
  char tmpbuf[MAXPATHLEN];
  yap_error_number err;

  LOCAL_DoingUndefp = true;
  if (LOCAL_PrologMode & InErrorMode && (err = LOCAL_ActiveError->errorNo)) {
    fprintf(stderr, "%% Warning %s WITHIN ERROR %s %s\n", s,
            Yap_errorClassName(Yap_errorClass(err)), Yap_errorName(err));
    Yap_RestartYap(1);
  }
  LOCAL_PrologMode |= InErrorMode;
  pred = RepPredProp(PredPropByFunc(FunctorPrintMessage,
                                    PROLOG_MODULE)); // PROCEDURE_print_message2
  va_start(ap, s);
  fmt = va_arg(ap, char *);
  if (fmt != NULL) {
#if HAVE_VSNPRINTF
    vsnprintf(tmpbuf, MAXPATHLEN - 1, fmt, ap);
#else
    (void)vsprintf(tmpbuf, fmt, ap);
#endif
  } else {
    return false;
  }
  va_end(ap);
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    fprintf(stderr, "warning message: %s\n", tmpbuf);
    LOCAL_DoingUndefp = false;
    LOCAL_PrologMode &= ~InErrorMode;
    return false;
  }

  ts[1] = MkAtomTerm(AtomWarning);
  ts[0] = MkAtomTerm(Yap_LookupAtom(tmpbuf));
  rc = Yap_execute_pred(pred, ts, true PASS_REGS);
  LOCAL_PrologMode &= ~InErrorMode;
  return rc;
}

void Yap_InitError__(const char *file, const char *function, int lineno,
                     yap_error_number e, Term t, ...) {
  CACHE_REGS
  va_list ap;
  va_start(ap, t);
  const char *fmt;
  char *tmpbuf=NULL;

  fmt = va_arg(ap, char *);
  if (fmt != NULL) {
    tmpbuf = malloc(MAXPATHLEN);
#if HAVE_VSNPRINTF
    vsnprintf(tmpbuf, MAXPATHLEN - 1, fmt, ap);
#else
    (void)vsprintf(tmpbuf, fmt, ap);
#endif
  } else
    return;
  va_end(ap);
  if (LOCAL_ActiveError->errorNo != YAP_NO_ERROR) {
    yap_error_number err = LOCAL_ActiveError->errorNo;
    fprintf(stderr, "%% Warning %s WITHIN ERROR %s %s\n", Yap_errorName(e),
            Yap_errorClassName(Yap_errorClass(err)), Yap_errorName(err));
    return;
  }
  LOCAL_ActiveError->errorNo = e;
  LOCAL_ActiveError->errorFile = NULL;
  LOCAL_ActiveError->errorFunction = NULL;
  LOCAL_ActiveError->errorLine = 0;
  if (fmt && tmpbuf) {
    LOCAL_Error_Size = strlen(tmpbuf);
    LOCAL_ActiveError->errorMsg = malloc(LOCAL_Error_Size + 1);
    strcpy((char *)LOCAL_ActiveError->errorMsg, tmpbuf);
  } else {
    LOCAL_Error_Size = 0;
  }
}

bool Yap_PrintWarning(Term twarning) {
  CACHE_REGS
  PredEntry *pred = RepPredProp(PredPropByFunc(
      FunctorPrintMessage, PROLOG_MODULE)); // PROCEDURE_print_message2;
  if (twarning)
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid ", " warning(%s)",
                        Yap_TermToBuffer(twarning, Quote_illegal_f | Ignore_ops_f | Handle_cyclics_f));
  Term cmod = (CurrentModule == PROLOG_MODULE ? TermProlog : CurrentModule);
  bool rc;
  Term ts[2], err;

  
  if (twarning && LOCAL_PrologMode & InErrorMode &&
      LOCAL_ActiveError->errorClass != WARNING &&
      (err = LOCAL_ActiveError->errorNo)  ) {
    fprintf(stderr, "%% Warning %s while processing error: %s %s\n",
            Yap_TermToBuffer(twarning,
                             Quote_illegal_f | Ignore_ops_f),
            Yap_errorClassName(Yap_errorClass(err)), Yap_errorName(err));
    return false;
  }
  LOCAL_PrologMode |= InErrorMode;
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    fprintf(stderr, "%s:%ld/* d:%d warning */:\n",
	    LOCAL_ActiveError->errorFile,
	    LOCAL_ActiveError->errorLine, 0 );
    if (!twarning)
      twarning =  Yap_MkFullError();
    Yap_DebugPlWriteln(twarning);
    LOCAL_DoingUndefp = false;
    LOCAL_PrologMode &= ~InErrorMode;
    CurrentModule = cmod;
    return false;
  }
    if (!twarning)
      twarning =  Yap_MkFullError();
  ts[1] = twarning;
  ts[0] = MkAtomTerm(AtomWarning);
  rc = Yap_execute_pred(pred, ts, true PASS_REGS);
  LOCAL_within_print_message = false;
  LOCAL_PrologMode &= ~InErrorMode;
  return rc;
   
}

bool Yap_HandleError__(const char *file, const char *function, int lineno,
                       const char *s, ...) {
  CACHE_REGS
  yap_error_number err = LOCAL_Error_TYPE;
  const char *serr;

  arity_t arity = 2;

  if (LOCAL_ErrorMessage) {
    serr = LOCAL_ErrorMessage;
  } else {
    serr = s;
  }
  if (P != FAILCODE) {
    if (P->opc == Yap_opcode(_try_c) || P->opc == Yap_opcode(_try_userc) ||
        P->opc == Yap_opcode(_retry_c) || P->opc == Yap_opcode(_retry_userc)) {

      arity = P->y_u.OtapFs.p->ArityOfPE;
    } else {
      arity = PREVOP(P, Osbpp)->y_u.Osbpp.p->ArityOfPE;
    }
  }
  switch (err) {
  case RESOURCE_ERROR_STACK:
    if (!Yap_gcl(0, arity, ENV, gc_P(P, CP))) {
      Yap_Error__(false, file, function, lineno, RESOURCE_ERROR_STACK, ARG1,
                  serr);
      return false;
    }
    LOCAL_PrologMode = UserMode;
    return true;
  case RESOURCE_ERROR_AUXILIARY_STACK:
    if (LOCAL_MAX_SIZE < (char *)AuxSp - AuxBase) {
      LOCAL_MAX_SIZE += 1024;
    }
    if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
      /* crash in flames */
      Yap_Error__(false, file, function, lineno, RESOURCE_ERROR_AUXILIARY_STACK,
                  ARG1, serr);
      return false;
    }
    LOCAL_PrologMode = UserMode;
    return true;
  case RESOURCE_ERROR_HEAP:
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error__(false, file, function, lineno, RESOURCE_ERROR_HEAP, ARG2,
                  serr);
      return false;
    }
  default:
  
    if (LOCAL_PrologMode == UserMode)
      Yap_ThrowError__(file, function, lineno, err, LOCAL_RawTerm, serr);
    else
      LOCAL_PrologMode &= ~InErrorMode;
    return false;
  }
}

int Yap_SWIHandleError(const char *s, ...) {
  CACHE_REGS
  yap_error_number err = LOCAL_Error_TYPE;
  char *serr;

  if (s) {
    serr = (char *)s;
  }
  switch (err) {
  case RESOURCE_ERROR_STACK:
    if (!Yap_gcl(0, 2, ENV, gc_P(P, CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, serr);
      return (FALSE);
    }
    return TRUE;
  case RESOURCE_ERROR_AUXILIARY_STACK:
    if (LOCAL_MAX_SIZE < (char *)AuxSp - AuxBase) {
      LOCAL_MAX_SIZE += 1024;
    }
    if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
      /* crash in flames */
      Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK, ARG1, serr);
      return FALSE;
    }
    return true;
  case RESOURCE_ERROR_HEAP:
    if (!Yap_growheap(false, 0, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, ARG2, serr);
      return false;
    }
  default:
    Yap_Error(err, TermNil, serr);
    return false;
  }
}

void Yap_RestartYap(int flag) {
  CACHE_REGS
#if PUSH_REGS
  restore_absmi_regs(&Yap_standard_regs);
#endif
  siglongjmp(*LOCAL_RestartEnv, flag);
}

static void error_exit_yap(int value) {
  CACHE_REGS
  if (!(LOCAL_PrologMode & BootMode)) {

#if DEBUG
#endif
  }
  fprintf(stderr, "\n   Exiting ....\n");
#if HAVE_BACKTRACE
  void *callstack[256];
  int i;
  int frames = backtrace(callstack, 256);
  char **strs = backtrace_symbols(callstack, frames);
  fprintf(stderr, "Execution stack:\n");
  for (i = 0; i < frames; ++i) {
    fprintf(stderr, "       %s\n", strs[i]);
  }
  free(strs);
#endif
  Yap_exit(value);
}

/* This needs to be a static because I can't trust the stack (WIN32), and
   I can't trust the Yap stacks  (error) */
#define YAP_BUF_SIZE 512

static char tmpbuf[YAP_BUF_SIZE];

// error classes: based on OSI errors.
//
//     - The extra argument says whether there different instances
//
//     - Events are treated within the same pipeline as errors.
//

#undef BEGIN_ERROR_CLASSES
#undef ECLASS
#undef END_ERROR_CLASSES
#undef BEGIN_ERRORS
#undef E0
#undef E
#undef E1
#undef E2
#undef END_ERRORS

#define BEGIN_ERROR_CLASSES()                                                  \
  static Atom mkerrorct(yap_error_class_number c) {                            \
    switch (c) {

#define ECLASS(CL, A, B)                                                       \
  case CL:                                                                     \
    return Yap_LookupAtom(A);

#define END_ERROR_CLASSES()                                                    \
  }                                                                            \
  return NULL;                                                                 \
  }

#define BEGIN_ERRORS()                                                         \
  static Term mkerrort(yap_error_number e, Term culprit, Term info) {          \
    if (!e || !info) return TermNil; \
    switch (e) {

#define E0(A, B)                                                               \
  case A: {                                                                    \
    Term ft[2];                                                                \
    ft[0] = MkAtomTerm(mkerrorct(B));                                          \
    ft[1] = info;                                                              \
    return Yap_MkApplTerm(FunctorError, 2, ft);                                \
  }

#define E(A, B, C)                                                             \
  case A: {                                                                    \
    Term ft[2], nt[2];                                                         \
    nt[0] = MkAtomTerm(Yap_LookupAtom(C));                                     \
    nt[1] = MkVarTerm();                                                       \
    Yap_unify(nt[1], culprit);                                                 \
    ft[0] = Yap_MkApplTerm(Yap_MkFunctor(mkerrorct(B), 2), 2, nt);             \
    ft[1] = info;                                                              \
    return Yap_MkApplTerm(FunctorError, 2, ft);                                \
  }

#define E1(A, B, C)                                                            \
  case A: {                                                                    \
    Term ft[2], nt[1];                                                         \
    nt[0] = MkVarTerm();                                                       \
    Yap_unify(nt[0], culprit);                                                 \
    ft[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(C), 1), 1, nt);        \
    ft[1] = info;                                                              \
    return Yap_MkApplTerm(FunctorError, 2, ft);                                \
  }

#define E2(A, B, C, D)                                                         \
  case A: {                                                                    \
    Term ft[2], nt[3];                                                         \
    nt[0] = MkAtomTerm(Yap_LookupAtom(C));                                     \
    nt[1] = MkAtomTerm(Yap_LookupAtom(D));                                     \
    nt[2] = MkVarTerm();                                                       \
    Yap_unify(nt[2], culprit);                                                 \
    ft[0] = Yap_MkApplTerm(Yap_MkFunctor(mkerrorct(B), 3), 3, nt);             \
    ft[1] = info;                                                              \
    return Yap_MkApplTerm(FunctorError, 2, ft);                                \
  }

#define END_ERRORS()                                                           \
  }                                                                            \
  return TermNil;                                                              \
  }

#include "YapErrors.h"

/// add a new error descriptor, either to the top of the  stack,
/// or replacing the top;
bool Yap_pushErrorContext(bool link , yap_error_descriptor_t *new_error) {
  memset(new_error, 0, sizeof(yap_error_descriptor_t));
  if (link)
    new_error->top_error = LOCAL_ActiveError;
  LOCAL_ActiveError = new_error;
  return true;
}

/* static void */
/* reset_error_description(void) { */
/*   yap_error_descriptor_t *bf = LOCAL_ActiveError->top_error; */
/*   if (Yap_HasException()) */
/*   memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError)); */
/*   LOCAL_ActiveError->top_error = bf; */
/* } */
yap_error_descriptor_t *Yap_popErrorContext(bool mdnew, bool pass) {
  yap_error_descriptor_t *e = LOCAL_ActiveError, *ep = LOCAL_ActiveError->top_error;
  // last block
  LOCAL_ActiveError = ep;
  if (e->errorNo && !ep->errorNo && pass) {
    yap_error_descriptor_t *epp = ep->top_error;
    memmove(ep, e, sizeof(*e));
    ep->top_error = epp;
  }
  return LOCAL_ActiveError;
}
/**
 * Throw an error directly to the error handler
 *
 * @param file      where
 * @param function  who
 * @param lineno    when
 * @param type      what, error code
 * @param where     how, user information
 */
void Yap_ThrowError__(const char *file, const char *function, int lineno,
                      yap_error_number type, Term where, ...) {
  va_list ap;
  char tmpbuf[MAXPATHLEN];

  va_start(ap, where);
  char *fmt = va_arg(ap, char *);
  if (fmt != NULL) {
#if HAVE_VSNPRINTF
    (void)vsnprintf(tmpbuf, MAXPATHLEN - 1, fmt, ap);
#else
    (void)vsprintf(tnpbuf, fmt, ap);
#endif
    // fprintf(stderr, "warning: ");
    Yap_Error__(true, file, function, lineno, type, where, tmpbuf);
  } else {
    Yap_Error__(true, file, function, lineno, type, where, NULL);
  }
  if (LOCAL_RestartEnv && !LOCAL_delay) {
    Yap_RestartYap(5);
  }
  Yap_exit(5);
}

/**
 * complete delayed error.
 *
 */
void Yap_ThrowExistingError(void) {
  if (LOCAL_RestartEnv) {
    Yap_RestartYap(5);
  }
  Yap_exit(5);
}

Term Yap_MkFullError(void)
{
  yap_error_descriptor_t *i =  CopyException(Yap_local.ActiveError);
  i->errorAsText = Yap_errorName( i->errorNo );
  i->errorClass = Yap_errorClass( i-> errorNo );
  i->classAsText = Yap_errorClassName(i->errorClass);
    return mkerrort(i->errorNo, TermNil , MkSysError(i) );
}


bool Yap_MkErrorRecord(yap_error_descriptor_t *r, const char *file,
                       const char *function, int lineno, yap_error_number type,
  Term where, const char *s) {
  if (!Yap_pc_add_location(r, P, B, ENV))
    Yap_env_add_location(r, CP, B, ENV, 0);
  if (where == 0L || where == TermNil) {
    r->culprit = NULL;
  } else {
    r->culprit = Yap_TermToBuffer(
        where, Quote_illegal_f | Ignore_ops_f);
  }
  if (type != SYNTAX_ERROR && LOCAL_consult_level > 0) {
    r->parserFile = Yap_ConsultingFile(PASS_REGS1)->StrOfAE;
    r->parserLine = Yap_source_line_no();
  }
  r->errorNo = type;
  r->errorAsText = Yap_errorName(type);
  r->errorClass = Yap_errorClass(type);
  r->classAsText = Yap_errorClassName(r->errorClass);
  r->errorLine = lineno;
  r->errorFunction = function;
  r->errorFile = file;
  r->prologConsulting = Yap_Consulting();
  LOCAL_PrologMode |= InErrorMode;
  Yap_ClearExs();
  // first, obtain current location

  // sprintf(LOCAL_FileNameBuf, "%s:%d in C-function %s ", file, lineno,
  // function);
  //  tf = MkAtomTerm(Yap_LookupAtom(LOCAL_FileNameBuf));
#if DEBUG_STRICT
  if (Yap_heap_regs && !(LOCAL_PrologMode & BootMode))
    fprintf(stderr, "***** Processing Error %d (%lx,%x) %s***\n", type,
            (unsigned long int)LOCAL_Signals, LOCAL_PrologMode, fmt);
  else
    fprintf(stderr, "***** Processing Error %d (%x) %s***\n", type,
            LOCAL_PrologMode, fmt);
#endif
  if (r->errorNo == SYNTAX_ERROR) {
    r->errorClass = SYNTAX_ERROR_CLASS;
  } else if (r->errorNo == SYNTAX_ERROR_NUMBER) {
    r->errorClass = SYNTAX_ERROR_CLASS;
    r->errorNo = SYNTAX_ERROR;
  }
  if (type == INTERRUPT_EVENT) {
    fprintf(stderr, "%% YAP exiting: cannot handle signal %d\n",
            (int)IntOfTerm(where));
    Yap_exit(1);
  }
  // fprintf(stderr, "warning: ");
  if (s && s[0]) {
    char *ns;
    r->errorMsgLen = strlen(s) + 1;
    ns = malloc(r->errorMsgLen);
    strcpy(ns, s);
    r->errorMsg = ns;
  } else {
    r->errorMsgLen = 0;
    r->errorMsg = 0;
  }
  return true;
}

/**
 * @brief Yap_Error
 *   This function handles errors in the C code. Check errors.yap for the
 *corresponding Prolog code.
 *
 * @param file      C source
 * @param function  C function
 * @param lineno    C exact line
 * @param type      the error ID (in YAP this is a single integer)
 * @param where     the culprit
 * @return usually FAILCODE
 *
 * In a good day, the error handler's job is to generate a throw. This includes:
 *   - constructing an ISO style error term;
 *   - constructing a list with all available info on the bug
 *   - generating the throw
 *   - forcing backtracking in order to restart.
 *
 * In a bad day, it has to deal with OOM, abort, and errors within errorts.
 *
 */
yamop *Yap_Error__(bool throw, const char *file, const char *function,
                   int lineno, yap_error_number type, Term where, ...) {
  CACHE_REGS
  va_list ap;
  char *fmt;
  char *s = NULL;


  switch (type) {
  case SYSTEM_ERROR_INTERNAL: {
    fprintf(stderr, "%% Internal YAP Error: %s exiting....\n", tmpbuf);
    //    serious = true;
    if (LOCAL_PrologMode & BootMode) {
      fprintf(stderr, "%% YAP crashed while booting %s\n", tmpbuf);
    } else {
      Yap_output_bug_location(P, FIND_PRED_FROM_ANYWHERE, YAP_BUF_SIZE);
      if (tmpbuf[0]) {
        fprintf(stderr, "%% Bug found while executing %s\n", tmpbuf);
      }
#if HAVE_BACKTRACE
      void *callstack[256];
      int i;
      int frames = backtrace(callstack, 256);
      char **strs = backtrace_symbols(callstack, frames);
      fprintf(stderr, "Execution stack:\n");
      for (i = 0; i < frames; ++i) {
        fprintf(stderr, "       %s\n", strs[i]);
      }
      free(strs);
#endif
    }
    error_exit_yap(1);
  }
  case SYSTEM_ERROR_FATAL: {
    fprintf(stderr, "%% Fatal YAP Error: %s exiting....\n", tmpbuf);
    error_exit_yap(1);
  }
  case INTERRUPT_EVENT: {
    error_exit_yap(1);
  }
  case USER_EVENT: {
    LOCAL_ActiveError = Yap_UserError(where,LOCAL_ActiveError);
    break;
  }
  case ABORT_EVENT:
    //      fun = FunctorDollarVar;
    //    serious = true;
    LOCAL_ActiveError->errorNo = ABORT_EVENT;
    Yap_JumpToEnv();
    P = FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  case CALL_COUNTER_UNDERFLOW_EVENT:
    /* Do a long jump */
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    LOCAL_ActiveError->errorNo = CALL_COUNTER_UNDERFLOW_EVENT;
    Yap_JumpToEnv();
    P = FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  case PRED_ENTRY_COUNTER_UNDERFLOW_EVENT:
    /* Do a long jump */
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    LOCAL_ActiveError->errorNo = PRED_ENTRY_COUNTER_UNDERFLOW_EVENT;
    Yap_JumpToEnv();
    P = FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  case RETRY_COUNTER_UNDERFLOW_EVENT:
    /* Do a long jump */
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    LOCAL_ActiveError->errorNo = RETRY_COUNTER_UNDERFLOW_EVENT;
    Yap_JumpToEnv();
    P = FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  default:
    va_start(ap, where);
    fmt = va_arg(ap, char *);
    if (fmt != NULL) {
    s = malloc(MAXPATHLEN);
#if HAVE_VSNPRINTF
      (void)vsnprintf(s, MAXPATHLEN - 1, fmt, ap);
#else
      (void)vsprintf(s, fmt, ap);
#endif
      va_end(ap);
      break;
    }
  }
  Yap_MkErrorRecord(LOCAL_ActiveError, file, function, lineno, type, where, s);
  if (where == 0 || where == TermNil) {
    LOCAL_ActiveError->culprit = 0;
  }
  if (P == (yamop *)(FAILCODE)) {
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  }
  /* PURE_ABORT may not have set where correctly, BootMode may not have the data
   * terms ready */
  if (type == ABORT_EVENT || LOCAL_PrologMode & BootMode) {
    LOCAL_PrologMode &= ~AbortMode;
    LOCAL_PrologMode &= ~InErrorMode;
    /* make sure failure will be seen at next port */
    // no need to lock & unlock
    if (LOCAL_PrologMode & AsyncIntMode)
      Yap_signal(YAP_FAIL_SIGNAL);
    P = FAILCODE;
  } else {
    /* Exit Abort Mode, if we were there */
    LOCAL_PrologMode &= ~AbortMode;
    LOCAL_PrologMode |= InErrorMode;
  }

#ifdef DEBUG
  // DumpActiveGoals( USES_REGS1 );
#endif /* DEBUG */
  if (LOCAL_ActiveError->errorNo!= SYNTAX_ERROR)
    LOCAL_ActiveError->prologStack=Yap_dump_stack();
  CalculateStackGap(PASS_REGS1);
#if DEBUG
  //    DumpActiveGoals( PASS_REGS1 );
#endif
  /* wait if we we are in user code,
     it's up to her to decide */
  if (LOCAL_delay)
    return P;
  if (LOCAL_DoingUndefp) {
      LOCAL_DoingUndefp = false;
    LOCAL_Signals = 0;
    yap_error_descriptor_t *co = CopyException( LOCAL_ActiveError );
    Yap_PrintWarning(MkErrorTerm(Yap_GetException( co )));
    return P;
  }
  // LOCAL_ActiveError = Yap_GetException();
  // reset_error_description();
  if (!throw) {
    Yap_JumpToEnv();
    pop_text_stack(LOCAL_MallocDepth+1);
  }
  LOCAL_PrologMode = UserMode;
  return P;
}

static Int close_error(USES_REGS1) {
  if (!LOCAL_CommittedError)
    return true;
  LOCAL_CommittedError->errorNo = YAP_NO_ERROR;
  LOCAL_ErrorMessage = NULL;
  free(LOCAL_CommittedError);
  LOCAL_CommittedError = NULL;
  return true;
}

#undef BEGIN_ERROR_CLASSES
#undef ECLASS
#undef END_ERROR_CLASSES
#undef BEGIN_ERRORS
#undef E0
#undef E
#undef E1
#undef E2
#undef END_ERRORS

#define BEGIN_ERROR_CLASSES() typedef enum aux_class {

#define ECLASS(CL, A, B) CL##__,

#define END_ERROR_CLASSES()                                                    \
  }                                                                            \
  aux_class_t;

#define BEGIN_ERRORS()
#define E0(X, Y)
#define E(X, Y, Z)
#define E1(X, Y, Z)
#define E2(X, Y, Z, W)
#define END_ERRORS()

#include <YapErrors.h>

#undef BEGIN_ERROR_CLASSES
#undef ECLASS
#undef END_ERROR_CLASSES
#undef BEGIN_ERRORS
#undef E0
#undef E
#undef E1
#undef E2
#undef END_ERRORS

#define BEGIN_ERROR_CLASSES() static const char *c_error_class_name[] = {

#define ECLASS(CL, A, B) A,

#define END_ERROR_CLASSES()                                                    \
  NULL                                                                         \
  }

typedef struct c_error_info {
  int class;
  const char *name;
} c_error_t;

#define BEGIN_ERRORS() static struct c_error_info c_error_list[] = {
#define E0(X, Y) {Y##__, ""},
#define E(X, Y, Z) {Y##__, Z},
#define E1(X, Y, Z) {Y##__, Z},
#define E2(X, Y, Z, W) {Y##__, Z " " W},
#define END_ERRORS()                                                           \
  { YAPC_NO_ERROR, "" }                                                        \
  }                                                                            \
  ;

#include <YapErrors.h>
#include <iopreds.h>

yap_error_class_number Yap_errorClass(yap_error_number e) {
  return c_error_list[e].class;
}

const char *Yap_errorName(yap_error_number e) { return c_error_list[e].name; }

const char *Yap_errorClassName(yap_error_class_number e) {
  return c_error_class_name[e];
}

yap_error_descriptor_t *Yap_GetException(yap_error_descriptor_t *i) {
  CACHE_REGS
  if (i->errorNo != YAP_NO_ERROR) {
    yap_error_descriptor_t *t = LOCAL_ActiveError,
                           *nt = calloc(1,sizeof(yap_error_descriptor_t));
    memmove(nt, t, sizeof(yap_error_descriptor_t));
    return nt;
  }
  return 0;
}

void Yap_PrintException(yap_error_descriptor_t *i) {
  printErr(LOCAL_ActiveError);
}

bool Yap_RaiseException(void) {
  if (LOCAL_ActiveError == NULL ||
      LOCAL_ActiveError->errorNo == YAP_NO_ERROR)
    return false;
  Yap_RestartYap(5);
  return false;
  //return Yap_JumpToEnv();
}

bool Yap_ResetException(yap_error_descriptor_t *i) {
  // reset error descriptor
  if (!i)
    i = LOCAL_ActiveError;
  yap_error_descriptor_t *bf = i->top_error;
  memset(i, 0, sizeof(*i));
  i->top_error = bf;
  return true;
}

static Int reset_exception(USES_REGS1) { return Yap_ResetException(worker_id); }


Term MkErrorTerm(yap_error_descriptor_t *t) {
  if (t->errorClass == EVENT)
    return t->errorRawTerm;
  Term tc = t->culprit ? Yap_BufferToTerm(t->culprit, TermNil) : TermNil;
  if (tc == 0)
    tc = MkAtomTerm(Yap_LookupAtom(t->culprit));
  return mkerrort(t->errorNo,
                  tc,
                  err2list(t));
}


static yap_error_descriptor_t *CopyException(yap_error_descriptor_t *t) {
  yap_error_descriptor_t *n = malloc( sizeof( yap_error_descriptor_t ));
  memcpy(n, t, sizeof( yap_error_descriptor_t ) );
  return n;
}

static Int read_exception(USES_REGS1) {
  yap_error_descriptor_t *t = AddressOfTerm(Deref(ARG1));
  Term rc = MkErrorTerm(t);
  //      Yap_DebugPlWriteln(rc);
  return Yap_unify(ARG2, rc);
}

static Int print_exception(USES_REGS1) {
    Term t1 = Deref(ARG1);
    if (IsAddressTerm(t1)) {
        yap_error_descriptor_t *t = AddressOfTerm(t1);
	if (t->parserFile && t->parserLine) {
	  fprintf(stderr,"\n%s:%ld:0 error: while parsing %s\n\n", t->parserFile,  t->parserLine,t->errorAsText);
	} else if (t->prologPredFile && t->prologPredLine) {
	  fprintf(stderr,"\n%s:%ld:0 error: while running %s\n\n", t->prologPredFile,  t->prologPredLine,t->errorAsText);
	} else if (t->errorFile && t->errorLine) {
	  fprintf(stderr,"\n%s:%ld:0 error: while executing %s\n\n", t->errorFile,  t->errorLine,t->errorAsText);
	}
        printErr(t);
    } else {
           return Yap_WriteTerm(LOCAL_c_error_stream,t1,TermNil PASS_REGS);
        }
  //      Yap_DebugPlWriteln(rc);
  return true;
}



static Int query_exception(USES_REGS1) {
  const char *query = NULL;
  Term t;

  if (IsAtomTerm((t = Deref(ARG1))))
    query = RepAtom(AtomOfTerm(t))->StrOfAE;
  if (IsStringTerm(t))
    query = StringOfTerm(t);
  if (!IsAddressTerm(Deref(ARG2)))
    return false;
  yap_error_descriptor_t *y = AddressOfTerm(Deref(ARG2));
  // if (IsVarTerm(t3)) {
    Term rc = queryErr(query, y);
    //      Yap_DebugPlWriteln(rc);
    return Yap_unify(ARG3, rc);
    // } else {
    // return setErr(query, y, t3);
    // }
}

static Int set_exception(USES_REGS1) {
  const char *query = NULL;
  Term t;

  if (IsAtomTerm((t = Deref(ARG1))))
    query = RepAtom(AtomOfTerm(t))->StrOfAE;
  if (IsStringTerm(t))
    query = StringOfTerm(t);
  if (!IsAddressTerm(Deref(ARG2)))
    return false;
  yap_error_descriptor_t *y = AddressOfTerm(Deref(ARG2));
  Term t3 = Deref(ARG3);
  if (IsVarTerm(t3)) {
    return false;
  } else {
    return setErr(query, y, t3);
  }
}



static Int drop_exception(USES_REGS1) {
  yap_error_descriptor_t *t = AddressOfTerm(Deref(ARG1));
  free(t);
  return true;
}

static Int new_exception(USES_REGS1) {
  Term t = MkSysError(calloc(1, sizeof(yap_error_descriptor_t)));
  return Yap_unify(ARG1, t);
}

static Int get_exception(USES_REGS1) {
  yap_error_descriptor_t *i;
  Term t;

  if (LOCAL_ActiveError->errorNo != YAP_NO_ERROR) {
    i = Yap_GetException(LOCAL_ActiveError);
    Yap_ResetException(LOCAL_ActiveError);
    LOCAL_PrologMode = UserMode;
    if (i->errorRawTerm &&
        (i->errorClass == EVENT || i->errorNo == SYNTAX_ERROR)) {
      t = i->errorRawTerm;
    } else if (i->culprit != NULL) {
      Term culprit = Yap_BufferToTerm(i->culprit, TermNil);
      if (culprit == 0) culprit = TermNil;
      t = mkerrort(i->errorNo,culprit ,
                   MkSysError(i));
    } else {
      t = mkerrort(i->errorNo, TermNil, MkSysError(i));
    }
    return Yap_unify(ARG1, t);
  }
  return false;
}

yap_error_descriptor_t *event(Term t, yap_error_descriptor_t *i) {
  i->errorNo = ERROR_EVENT;
  i->errorClass = EVENT;
  i->errorRawTerm = Yap_SaveTerm(t);
  return i;
}

yap_error_descriptor_t *Yap_UserError(Term t, yap_error_descriptor_t *i) {
  Term n = t;
  bool found = false, wellformed = true;
    if (!IsApplTerm(t) || FunctorOfTerm(t) != FunctorError) {
        LOCAL_Error_TYPE = THROW_EVENT;
        LOCAL_ActiveError->errorClass = EVENT;
        LOCAL_ActiveError->errorAsText = Yap_errorName(THROW_EVENT);
        LOCAL_ActiveError->classAsText =
                Yap_errorClassName(Yap_errorClass(THROW_EVENT));
        LOCAL_ActiveError->errorRawTerm = Yap_SaveTerm(t);
        LOCAL_ActiveError->culprit = NULL;
    } else     if (i->errorNo != YAP_NO_ERROR && i->errorNo != ERROR_EVENT) {
            LOCAL_Error_TYPE = i->errorNo;
            LOCAL_ActiveError->errorClass = Yap_errorClass(i->errorNo);
            LOCAL_ActiveError->errorAsText = Yap_errorName(i->errorNo);
            LOCAL_ActiveError->classAsText =
                    Yap_errorClassName(Yap_errorClass(i->errorNo));
            LOCAL_ActiveError->errorRawTerm = Yap_SaveTerm(t);
            LOCAL_ActiveError->culprit = NULL;
  } else {
    Term t1, t2;
    t1 = ArgOfTerm(1, t);
    t2 = ArgOfTerm(2, t);
    //    LOCAL_Error_TYPE = ERROR_EVENT;
    wellformed = wellformed && (i->errorAsText != NULL);
    if (wellformed) {
      int j;
      for (j = 0; j < sizeof(c_error_list) / sizeof(struct c_error_info); j++) {
        if (!strcmp(c_error_list[j].name, i->errorAsText) &&
            (c_error_list[j].class == 0 ||
             !strcmp(i->classAsText,
                     c_error_class_name[c_error_list[j].class]))) {
          if (c_error_list[j].class != PERMISSION_ERROR ||
              (t1 = ArgOfTerm(2, t1) && IsAtomTerm(t1) &&
                    !strcmp(c_error_list[j].name,
                            RepAtom(AtomOfTerm(t1))->StrOfAE) &&
                    c_error_list[j].class != EVENT)) {
            i->errorNo = j;
            i->errorClass = c_error_list[j].class;
            found = true;
            break;
          }
        }
      }
    } else if (IsAtomTerm(t1)) {
      const char *err = RepAtom(AtomOfTerm(t1))->StrOfAE;
      if (!strcmp(err, "instantiation_error")) {
        i->errorClass = INSTANTIATION_ERROR_CLASS;
        i->classAsText = "instantiation_error";
        i->errorAsText = "instantiation_error";
        i->errorNo = INSTANTIATION_ERROR;
        found = true;
      } else if (!strcmp(err, "uninstantiation_error")) {
        i->errorClass = UNINSTANTIATION_ERROR_CLASS;
        i->classAsText = "uninstantiation_error";
        i->errorAsText = "uninstantiation_error";
        i->errorNo = UNINSTANTIATION_ERROR;
        found = true;
      }
    }
    if (i->errorAsText && i->errorAsText[0]) {
      char *errs = malloc(strlen(i->errorAsText) + 1);
      strcpy(errs, i->errorAsText);
      i->errorAsText = errs;
    }
    if (!found) {
      return event(t, i);
    }
    if (found) {
      n = t2;
    }
    i->errorGoal = Yap_TermToBuffer(
        n, Quote_illegal_f | Ignore_ops_f );
  }
  Yap_prolog_add_culprit(i PASS_REGS);
  return i;
}

static Int is_boolean(USES_REGS1) {
  Term t = Deref(ARG1);
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  return t == TermTrue || t == TermFalse;
}

static Int is_atom(USES_REGS1) {
  Term t = Deref(ARG1);
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  return IsAtomTerm(t);
}

/** @pred must_be_callable( ?_Goal_ )
 *
 *  _Goal must be callable, that is, it must be bound and also must be
 *  either a compound term or an atom.
 */
static Int  must_be_callable(USES_REGS1) {
    Term mod = CurrentModule;
    if (mod == 0) mod = TermProlog;
    Term G = Yap_StripModule(Deref(ARG1), &mod);
    // Term Context = Deref(ARG2);
    if (IsVarTerm(mod)) {
        Yap_ThrowError(INSTANTIATION_ERROR, G, NULL);
        return false;
    } else if (!IsAtomTerm(mod)) {
          Yap_ThrowError(TYPE_ERROR_ATOM, mod, NULL);
        return false;
    }
    if (IsVarTerm(G)) {
        Yap_ThrowError(INSTANTIATION_ERROR, G, NULL);
        return false;
    }
    if (IsApplTerm(G)) {
        Functor f = FunctorOfTerm(G);
        if (IsExtensionFunctor(f)) {
            Yap_ThrowError(TYPE_ERROR_CALLABLE, G, NULL);
        }else {
                return true;
            }
        } else if (IsPairTerm(G) || IsAtomTerm(G)) {
            return true;
        } else {
            Yap_ThrowError(TYPE_ERROR_CALLABLE, G, NULL);
            return false;
        }
    return true;
}

/** @pred must_be_list( ?_Goal_ )
 *
 *  _Goal_ must be a list, that is, it must be bound and also must be
 *  a true list.
 */
static Int  must_be_list(USES_REGS1) {
  Term list = Deref(ARG1), *tailp;
    // Term Context = Deref(ARG2);
  Int n = Yap_SkipList(&list, &tailp);
  if (IsVarTerm (*tailp))
    Yap_ThrowError(INSTANTIATION_ERROR, ARG1, NULL);
  if (*tailp != TermNil || n < 0) {
    Yap_ThrowError(TYPE_ERROR_LIST, ARG1, NULL);
    return false;
  }
    return true;
}

/** @pred is_list( ?_Goal_ )
 *
 *  _Goal_ must be a list, that is, it must be bound and also must be
 *  a true list.
 */
static Int  is_list(USES_REGS1) {
  Term list = Deref(ARG1), *tailp;
    // Term Context = Deref(ARG2);
  Int n = Yap_SkipList(&list, &tailp);
  if (IsVarTerm (*tailp))
    Yap_ThrowError(INSTANTIATION_ERROR, ARG1, NULL);
  if (*tailp != TermNil || n < 0) {
    return false;
  }
    return true;
}

/** @pred must_be_bound( ?_T_ )
 *
 *  _T_ must be instantiated.
 */
static Int  must_be_bound(USES_REGS1) {
  Term t = Deref(ARG1);
    // Term Context = Deref(ARG2);
  if (IsVarTerm (t))
    Yap_ThrowError(INSTANTIATION_ERROR, ARG1, NULL);
  return true;
}

/**
 * @pred  is_predicate_indicator( Term, Module, Name, Arity )
 *
 *   This predicates can be used to verify if Term is a predicate indicator, that is of the form:
 *   + Name/Arity
 *   + Name//Arity-2
 *   + Module:Name/Arity
 *   + Module:Name//Arity-2
 *
 *   if it is, it will extract the predicate's module, name, and arity.
 *
 * Note: this will now accept both mod:(a/n) and
 * (mod:a)/n as valid.
 */
static Int get_predicate_indicator(USES_REGS1) {
    Term G = Deref(ARG1);
    // Term Context = Deref(ARG2);
    Term mod = CurrentModule;

    G = Yap_YapStripModule(G, &mod);
    if (IsVarTerm(G)) {
        Yap_ThrowError(INSTANTIATION_ERROR, G, NULL);
    }
    if (!IsVarTerm(mod) && !IsAtomTerm(mod)) {
        Yap_Error(TYPE_ERROR_ATOM, G, NULL);
        return false;
    }
    if (IsApplTerm(G)) {
        Functor f = FunctorOfTerm(G);
        if (IsExtensionFunctor(f)) {
            Yap_ThrowError(TYPE_ERROR_PREDICATE_INDICATOR, G, NULL);
        }
        if (f == FunctorSlash || f == FunctorDoubleSlash) {
            Term name = ArgOfTerm(1, G), arity = ArgOfTerm(2, G);
            name = Yap_YapStripModule(name, &mod);
            if (IsVarTerm(name)) {
                Yap_ThrowError(INSTANTIATION_ERROR, name, NULL);
            } else if (!IsAtomTerm(name)) {
                Yap_ThrowError(TYPE_ERROR_ATOM, name, NULL);
            }
            if (IsVarTerm(arity)) {
                Yap_ThrowError(INSTANTIATION_ERROR, arity, NULL);
            } else if (!IsIntegerTerm(arity)) {
                Yap_ThrowError(TYPE_ERROR_INTEGER, arity, NULL);
            } else {
                Int ar = IntegerOfTerm(arity);
                if (ar < 0) {
                    Yap_ThrowError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, arity, NULL);
                }
                if (f == FunctorDoubleSlash) {
                    arity = MkIntegerTerm(ar + 2);
                }
                return Yap_unify(mod, ARG2) &&
                       Yap_unify(name, ARG3) &&
                       Yap_unify(arity, ARG4);
            }
        }
    }
    Yap_ThrowError(TYPE_ERROR_PREDICATE_INDICATOR, G, NULL);
    return false;
}

void Yap_InitErrorPreds(void) {
  CACHE_REGS
  Yap_InitCPred("$print_exception", 1, print_exception, 0);
  Yap_InitCPred("$reset_exception", 1, reset_exception, 0);

  Yap_InitCPred("$new_exception", 1, new_exception, 0);
  Yap_InitCPred("$get_exception", 1, get_exception, 0);
  Yap_InitCPred("$set_exception", 3, set_exception, 0);
  Yap_InitCPred("$read_exception", 2, read_exception, 0);
  Yap_InitCPred("$query_exception", 3, query_exception, 0);
  Yap_InitCPred("$drop_exception", 1, drop_exception, 0);
  Yap_InitCPred("$close_error", 0, close_error, HiddenPredFlag);
  Yap_InitCPred("is_boolean", 1, is_boolean, TestPredFlag);
  Yap_InitCPred("must_be_callable", 1, must_be_callable, TestPredFlag);
  Yap_InitCPred("must_be_list", 1, must_be_list, TestPredFlag);
  Yap_InitCPred("must_be_bound", 1, must_be_bound, TestPredFlag);
  Yap_InitCPred("is_atom", 1, is_atom, TestPredFlag);
  Yap_InitCPred("is_list", 1, is_list, TestPredFlag);
  Yap_InitCPred("get_predicate_indicator", 4, get_predicate_indicator, 0);
}

