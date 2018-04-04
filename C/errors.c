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


#define query_key_b(k, ks, q, i)		\
  if (strcmp(ks,q) == 0) \
    { return i->k ? TermTrue : TermFalse; }	\

#define query_key_i(k, ks, q, i)		\
  if (strcmp(ks,q) == 0) \
    { return MkIntegerTerm(i->k); }

#define query_key_s(k, ks, q, i)			\
  if (strcmp(ks,q) == 0) \
    { return i->k ? MkStringTerm(i->k) : TermNil; }

static Term queryErr(const char *q, yap_error_descriptor_t *i) {
  query_key_i( errorNo, "errorNo", q, i );
  query_key_i(errorClass, "errorClass", q, i);
  query_key_s(errorAsText, "errorAsText", q, i);
  query_key_s( errorGoal, "errorGoal", q, i);
  query_key_s( classAsText, "classAsText", q, i);
  query_key_i( errorLine, "errorLine", q, i );
  query_key_s( errorFunction, "errorFunction", q, i);
  query_key_s( errorFile, "errorFile", q, i);
  query_key_i(  prologPredLine, "prologPredLine", q, i);
  query_key_i(  prologPredFirstLine, "prologPredFirstLine", q, i);
  query_key_i(  prologPredLastLine, "prologPredLastLine", q, i);
  query_key_s( prologPredName, "prologPredName", q, i);
  query_key_i(  prologPredArity, "prologPredArity", q, i);
  query_key_s( prologPredModule, "prologPredModule", q, i);
  query_key_s( prologPredFile, "prologPredFile", q, i);
  query_key_i(  prologParserPos, "prologParserPos", q, i);
  query_key_i(  prologParserLine, "prologParserLine", q, i);
  query_key_i(  prologParserFirstLine, "prologParserFirstLine", q, i);
  query_key_i(  prologParserLastLine, "prologParserLastLine", q, i);
  query_key_s( prologParserText, "prologParserText", q, i);
  query_key_s( prologParserFile, "prologParserFile", q, i);
  query_key_b( prologConsulting, "prologConsulting", q, i);
  query_key_s( culprit, "culprit", q, i);
  query_key_s( errorMsg, "errorMsg", q, i);
  query_key_i( errorMsgLen, "errorMsgLen", q, i);
      return TermNil;
}

static void print_key_b(const char *key, bool v)
{
  const char *b = v ? "true" : "false";
  fprintf(stderr,"%s: %s\n", key, b);
}

static void print_key_i(const char *key, YAP_Int v)
{
    fprintf(stderr,"%s: " Int_FORMAT "\n", key, v);
}



static void print_key_s(const char *key, const char *v)
{
    fprintf(stderr,"%s: %s\n", key, v);
}

  static void printErr(yap_error_descriptor_t *i) {

  if (i->errorNo == YAP_NO_ERROR) {
    return;
  }
   print_key_i( "errorNo", i->errorNo );
   print_key_i("errorClass", i->errorClass);
   print_key_s("errorAsText", i->errorAsText);
   print_key_s( "errorGoal", i->errorGoal);
   print_key_s( "classAsText", i->classAsText);
   print_key_i( "errorLineq", i->errorLine );
   print_key_s( "errorFunction", i->errorFunction);
   print_key_s( "errorFile", i->errorFile);
   print_key_i(  "prologPredLine", i->prologPredLine);
   print_key_i(  "prologPredFirstLine", i->prologPredFirstLine);
     print_key_i(  "prologPredLastLine", i->prologPredLastLine);
     print_key_s( "prologPredName", i->prologPredName);
     print_key_i(  "prologPredArity", i->prologPredArity);
     print_key_s( "prologPredModule", i->prologPredModule);
     print_key_s( "prologPredFile", i->prologPredFile);
     print_key_i(  "prologParserPos", i->prologParserPos);
     print_key_i(  "prologParserLine", i->prologParserLine);
     print_key_i(  "prologParserFirstLine", i->prologParserFirstLine);
     print_key_i(  "prologParserLastLine", i->prologParserLastLine);
     print_key_s( "prologParserText", i->prologParserText);
     print_key_s( "prologParserFile", i->prologParserFile);
     print_key_b( "prologConsulting", i->prologConsulting);
     print_key_s( "culprit", i->culprit);
    if (i->errorMsgLen) {
       print_key_s( "errorMsg", i->errorMsg);
       print_key_i( "errorMsgLen", i->errorMsgLen);
    }
}


static YAP_Term add_key_b(const char *key, bool v, YAP_Term o0)
{
  YAP_Term tkv[2];
  tkv[1] = v ? TermTrue : TermFalse;
  tkv[0] = MkStringTerm(key);
  Term node = Yap_MkApplTerm( FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static YAP_Term add_key_i(const char *key, YAP_Int v, YAP_Term o0)
{
  YAP_Term tkv[2];
  tkv[1] = MkIntegerTerm(v), tkv[0] = MkStringTerm(key);
  Term node = Yap_MkApplTerm( FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}



static YAP_Term add_key_s(const char *key, const char *v, YAP_Term o0)
{
  Term tkv[2];
  if (!v || v[0] ==  '\0')
    return o0;
  tkv[1] = MkStringTerm(v), tkv[0] = MkStringTerm(key);
  Term node = Yap_MkApplTerm( FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

  static Term err2list(yap_error_descriptor_t *i) {
  Term o = TermNil;
  if (i->errorNo == YAP_NO_ERROR) {
    return o;
  }
  o = add_key_i( "errorNo", i->errorNo, o );
  o = add_key_i("errorClass", i->errorClass, o);
  o = add_key_s("errorAsText", i->errorAsText, o);
  o = add_key_s( "errorGoal", i->errorGoal, o);
  o = add_key_s( "classAsText", i->classAsText, o);
  o = add_key_i( "errorLineq", i->errorLine, o );
  o = add_key_s( "errorFunction", i->errorFunction, o);
  o = add_key_s( "errorFile", i->errorFile, o);
  o = add_key_i(  "prologPredLine", i->prologPredLine, o);
  o = add_key_i(  "prologPredFirstLine", i->prologPredFirstLine, o);
    o = add_key_i(  "prologPredLastLine", i->prologPredLastLine, o);
    o = add_key_s( "prologPredName", i->prologPredName, o);
    o = add_key_i(  "prologPredArity", i->prologPredArity, o);
    o = add_key_s( "prologPredModule", i->prologPredModule, o);
    o = add_key_s( "prologPredFile", i->prologPredFile, o);
    o = add_key_i(  "prologParserPos", i->prologParserPos, o);
    o = add_key_i(  "prologParserLine", i->prologParserLine, o);
    o = add_key_i(  "prologParserFirstLine", i->prologParserFirstLine, o);
    o = add_key_i(  "prologParserLastLine", i->prologParserLastLine, o);
    o = add_key_s( "prologParserText", i->prologParserText, o);
    o = add_key_s( "prologParserFile", i->prologParserFile, o);
    o = add_key_b( "prologConsulting", i->prologConsulting, o);
    o = add_key_s( "culprit", i->culprit, o);
    if (i->errorMsgLen) {
      o = add_key_s( "errorMsg", i->errorMsg, o);
      o = add_key_i( "errorMsgLen", i->errorMsgLen, o);
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

  LOCAL_DoingUndefp = true;
  if (LOCAL_PrologMode & InErrorMode) {
    fprintf(stderr, "%% ERROR WITHIN ERROR %d: %s\n", LOCAL_Error_TYPE, tmpbuf);
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
    Yap_popErrorContext(false);
    return false;
  }
  va_end(ap);
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    fprintf(stderr, "warning message: %s\n", tmpbuf);
    LOCAL_DoingUndefp = false;
LOCAL_PrologMode &= ~InErrorMode;
    Yap_popErrorContext(false);
    return false;
  }

  ts[1] = MkAtomTerm(AtomWarning);
  ts[0] = MkAtomTerm(Yap_LookupAtom(tmpbuf));
  rc = Yap_execute_pred(pred, ts, true PASS_REGS);
  Yap_popErrorContext(false);
  LOCAL_PrologMode &= ~InErrorMode;
  return rc;
}

void Yap_InitError__(const char *file, const char *function, int lineno, yap_error_number e, Term t, ...) {
  CACHE_REGS
  va_list ap;
  va_start(ap, t);
  const char *fmt;
  char tmpbuf[MAXPATHLEN];

  fmt = va_arg(ap, char *);
  if (fmt != NULL) {
#if HAVE_VSNPRINTF
    vsnprintf(tmpbuf, MAXPATHLEN - 1, fmt, ap);
#else
    (void)vsprintf(tmpbuf, fmt, ap);
#endif
  } else
    return;
  va_end(ap);
  if (LOCAL_ActiveError->errorNo != YAP_NO_ERROR) {
    Yap_exit(1);
  }
  LOCAL_ActiveError->errorNo = e;
  LOCAL_ActiveError->errorFile = NULL;
  LOCAL_ActiveError->errorFunction = NULL;
  LOCAL_ActiveError->errorLine = 0;
  if (fmt) {
    LOCAL_Error_Size = strlen(tmpbuf);
    LOCAL_ActiveError->errorMsg = malloc(LOCAL_Error_Size + 1);
    strcpy(LOCAL_ActiveError->errorMsg, tmpbuf);
  } else {
    LOCAL_Error_Size = 0;
  }
}

bool Yap_PrintWarning(Term twarning) {
  CACHE_REGS
  PredEntry *pred = RepPredProp(PredPropByFunc(
      FunctorPrintMessage, PROLOG_MODULE)); // PROCEDURE_print_message2;
  Term cmod = (CurrentModule == PROLOG_MODULE ? TermProlog : CurrentModule);
  bool rc;
  Term ts[2];

  if (LOCAL_PrologMode & InErrorMode) {
    fprintf(stderr, "%% ERROR WITHIN ERROR while processing warning: %s\n", Yap_TermToBuffer(twarning, LOCAL_encoding, 0));
    Yap_RestartYap(1);
  }
  LOCAL_PrologMode |= InErrorMode;
  LOCAL_DoingUndefp = true;
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    fprintf(stderr, "warning message:\n");
    Yap_DebugPlWrite(twarning);
    fprintf(stderr, "\n");
    LOCAL_DoingUndefp = false;
  LOCAL_PrologMode &= ~InErrorMode;
    CurrentModule = cmod;
    return false;
  }
  ts[1] = twarning;
  ts[0] = MkAtomTerm(AtomWarning);
  HB = B->cp_h = HR;
  B->cp_tr = TR;
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
  if (P->opc == Yap_opcode(_try_c) || P->opc == Yap_opcode(_try_userc) ||
      P->opc == Yap_opcode(_retry_c) || P->opc == Yap_opcode(_retry_userc)) {

    arity = P->y_u.OtapFs.p->ArityOfPE;
  } else {
    arity = PREVOP(P, Osbpp)->y_u.Osbpp.p->ArityOfPE;
  }

  switch (err) {
  case RESOURCE_ERROR_STACK:
    if (!Yap_gc(arity, ENV, gc_P(P, CP))) {
      Yap_Error__(false, file, function, lineno, RESOURCE_ERROR_STACK, ARG1, serr);
      return false;
    }
    return true;
  case RESOURCE_ERROR_AUXILIARY_STACK:
    if (LOCAL_MAX_SIZE < (char *)AuxSp - AuxBase) {
      LOCAL_MAX_SIZE += 1024;
    }
    if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
      /* crash in flames */
      Yap_Error__(false, file, function, lineno, RESOURCE_ERROR_AUXILIARY_STACK, ARG1,
                  serr);
      return false;
    }
    return true;
  case RESOURCE_ERROR_HEAP:
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error__(false, file, function, lineno, RESOURCE_ERROR_HEAP, ARG2, serr);
      return false;
    }
  default:
    Yap_Error__(false, file, function, lineno, err, TermNil, serr);
    return false;
  }
}

int Yap_SWIHandleError(const char *s, ...) {
  CACHE_REGS
  yap_error_number err = LOCAL_Error_TYPE;
  char *serr;

  if (LOCAL_ErrorMessage) {
    serr = LOCAL_ErrorMessage;
  } else {
    serr = (char *)s;
  }
  switch (err) {
  case RESOURCE_ERROR_STACK:
    if (!Yap_gc(2, ENV, gc_P(P, CP))) {
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
    fprintf(stderr,"call siglongjmp HR=%p B=%p\n", HR, B);
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
#undef E2
#undef END_ERRORS

#define BEGIN_ERROR_CLASSES()					     \
  static Atom mkerrorct(yap_error_class_number c) {                  \
  switch (c) {

#define ECLASS(CL, A, B)						\
  case CL:								\
  return Yap_LookupAtom(A);						\

#define END_ERROR_CLASSES()						\
  }									\
    return NULL;							\
  }

#define BEGIN_ERRORS()							\
  static Term mkerrort(yap_error_number e, Term culprit, Term info) {	\
  switch (e) {

#define E0(A, B)							\
  case A: {								\
    Term ft[2];								\
    ft[0] = MkAtomTerm(mkerrorct(B));					\
    ft[1] = info;							\
    return Yap_MkApplTerm(FunctorError,2,ft); }

#define E(A, B, C)							\
  case A:								\
  { Term ft[2], nt[2];							\
    nt[0] = MkAtomTerm(Yap_LookupAtom(C));				\
    nt[1] = MkVarTerm(); \
    Yap_unify(nt[1], culprit);			\
    ft[0] = Yap_MkApplTerm(Yap_MkFunctor(mkerrorct(B),2), 2, nt);	\
    ft[1] = info;							\
    return Yap_MkApplTerm(FunctorError,2,ft); }

#define E2(A, B, C, D)							\
  case A:								\
  {									\
  Term ft[2], nt[3];							\
     nt[0] = MkAtomTerm(Yap_LookupAtom(C));				\
     nt[1] = MkAtomTerm(Yap_LookupAtom(D));				\
     nt[2] = MkVarTerm(); \
     Yap_unify(nt[2], culprit);			\
     ft[0] = Yap_MkApplTerm(Yap_MkFunctor(mkerrorct(B),3), 3, nt);	\
     ft[1] = info;							\
     return Yap_MkApplTerm(FunctorError,2,ft);				\
  }

#define END_ERRORS()							\
  }									\
    return TermNil;							\
  }

#include "YapErrors.h"

void Yap_pushErrorContext(yap_error_descriptor_t *new_error) {
  memset(new_error, 0, sizeof(yap_error_descriptor_t));
  new_error->top_error = LOCAL_ActiveError;
  LOCAL_ActiveError = new_error;
}

/* static void */
/* reset_error_description(void) { */
/*   yap_error_descriptor_t *bf = LOCAL_ActiveError->top_error; */
/*   if (Yap_HasException()) */
/*   memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError)); */
/*   LOCAL_ActiveError->top_error = bf; */

/* } */



yap_error_descriptor_t *Yap_popErrorContext(bool pass) {
  if (pass && LOCAL_ActiveError->top_error->errorNo == YAP_NO_ERROR &&
      LOCAL_ActiveError->errorNo != YAP_NO_ERROR)
    memcpy(LOCAL_ActiveError->top_error, LOCAL_ActiveError,
           sizeof(yap_error_descriptor_t));
  yap_error_descriptor_t *new_error = LOCAL_ActiveError;
  LOCAL_ActiveError = LOCAL_ActiveError->top_error;
  return new_error;
}

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
    Yap_Error__(true, file, function, lineno, type, where);
  }
  if (LOCAL_RestartEnv) {
    Yap_RestartYap(5);
  } 
    Yap_exit(5);
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
 * The list includes the following options:
 *   + c=c(file, line, function): where the bug was detected;
 *
 *   + e=p(mod, name, arity, cl, file, lin): where the code was entered;
 *
 *   + p=p(mod, name, arity, cl, file, line): the prolog procedure that caused
 *the bug,
 *and optionally,
 *
 *   + g=g(Goal):   the goal that created this mess
 *
 *   + i=i(Comment): an user-written comment on this bug.
 */
yamop *Yap_Error__(bool throw, const char *file, const char *function, int lineno,
                   yap_error_number type, Term where, ...) {
    CACHE_REGS
    va_list ap;
    char *fmt;
    char s[MAXPATHLEN];

    /* disallow recursive error handling */
    if (LOCAL_PrologMode & InErrorMode) {
        fprintf(stderr, "%% ERROR WITHIN ERROR %d: %s\n", LOCAL_Error_TYPE, tmpbuf);
        Yap_RestartYap(1);
    }
    if (LOCAL_DoingUndefp && type == EVALUATION_ERROR_UNDEFINED) {
        P = FAILCODE;
        CalculateStackGap(PASS_REGS1);
        return P;
    }
    LOCAL_ActiveError->errorNo = type;
    LOCAL_ActiveError->errorAsText = Yap_errorName(type);
    LOCAL_ActiveError->errorClass = Yap_errorClass(type);
    LOCAL_ActiveError->classAsText =
            Yap_errorClassName(LOCAL_ActiveError->errorClass);
    LOCAL_ActiveError->errorLine = lineno;
    LOCAL_ActiveError->errorFunction = function;
    LOCAL_ActiveError->errorFile = file;
    Yap_prolog_add_culprit(LOCAL_ActiveError PASS_REGS1);
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
    if (type == INTERRUPT_EVENT) {
        fprintf(stderr, "%% YAP exiting: cannot handle signal %d\n",
                (int) IntOfTerm(where));
        LOCAL_PrologMode &= ~InErrorMode;
        Yap_exit(1);
    }
    if (where == 0L || where == TermNil) {
        LOCAL_ActiveError->culprit = NULL;
    } else {
        LOCAL_ActiveError->culprit = Yap_TermToBuffer(where, LOCAL_encoding, Quote_illegal_f | Handle_vars_f);

    }
    va_start(ap, where);
    fmt = va_arg(ap, char *);
    if (fmt != NULL) {
#if HAVE_VSNPRINTF
        (void) vsnprintf(s, MAXPATHLEN - 1, fmt, ap);
#else
        (void)vsprintf(s, fmt, ap);
#endif
        // fprintf(stderr, "warning: ");
        if (s[0]) {
            LOCAL_ActiveError->errorMsgLen = strlen(s) + 1;
            LOCAL_ActiveError->errorMsg = malloc(LOCAL_ActiveError->errorMsgLen);
            strcpy(LOCAL_ActiveError->errorMsg, s);
        } else if (LOCAL_ErrorMessage && LOCAL_ErrorMessage[0]) {
            LOCAL_ActiveError->errorMsgLen = strlen(LOCAL_ErrorMessage) + 1;
            LOCAL_ActiveError->errorMsg = malloc(LOCAL_ActiveError->errorMsgLen);
            strcpy(LOCAL_ActiveError->errorMsg, LOCAL_ErrorMessage);
        } else {
            LOCAL_ActiveError->errorMsgLen = 0;
            LOCAL_ActiveError->errorMsg = 0;
        }
    }
    va_end(ap);
    if (where == 0 || where == TermNil) {
        LOCAL_ActiveError->culprit = 0;
    }
    if (P == (yamop *) (FAILCODE)) {
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

    if (LOCAL_PrologMode & BootMode) {
        /* crash in flames! */
        fprintf(stderr,
                "%s:%d:0 YAP Fatal Error %d in function %s:\n  %s exiting....\n",
                file, lineno, type, function, s);
        error_exit_yap(1);
    }
#ifdef DEBUG
    // DumpActiveGoals( USES_REGS1 );
#endif /* DEBUG */

    switch (type) {
        case SYSTEM_ERROR_INTERNAL: {
            fprintf(stderr, "%% Internal YAP Error: %s exiting....\n", tmpbuf);
            //    serious = true;
            if (LOCAL_PrologMode & BootMode) {
                fprintf(stderr, "%% YAP crashed while booting %s\n", tmpbuf);
            } else {
                Yap_detect_bug_location(P, FIND_PRED_FROM_ANYWHERE, YAP_BUF_SIZE);
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
           if (!Yap_pc_add_location(LOCAL_ActiveError, CP, B, ENV))
                Yap_env_add_location(LOCAL_ActiveError, CP, B, ENV, 0);
            break;
    }

  CalculateStackGap(PASS_REGS1);
#if DEBUG
  //    DumpActiveGoals( PASS_REGS1 );
#endif
  /* wait if we we are in user code,
     it's up to her to decide */

  if (LOCAL_DoingUndefp) {
    LOCAL_Signals = 0;    
    Yap_PrintWarning(Yap_GetException());
    return P;
  }
  //reset_error_description();
  fprintf(stderr,"HR before jmp=%p\n", HR);
  //  if (!throw) {
    Yap_JumpToEnv();
    //  }
  return P;
}

static Int is_boolean(USES_REGS1) {
  Term t = Deref(ARG1);
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  return t == TermTrue || t == TermFalse;
}

static Int is_atom(USES_REGS1) {
  Term t = Deref(ARG1);
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  return IsAtomTerm(t);
}

static Int is_callable(USES_REGS1) {
  Term G = Deref(ARG1);
  // Term Context = Deref(ARG2);
  while (true) {
    if (IsVarTerm(G)) {
      Yap_Error(INSTANTIATION_ERROR, G, NULL);
      return false;
    }
    if (IsApplTerm(G)) {
      Functor f = FunctorOfTerm(G);
      if (IsExtensionFunctor(f)) {
        Yap_Error(TYPE_ERROR_CALLABLE, G, NULL);
      }
      if (f == FunctorModule) {
        Term tm = ArgOfTerm(1, G);
        if (IsVarTerm(tm)) {
          Yap_Error(INSTANTIATION_ERROR, G, NULL);
          return false;
        }
        if (!IsAtomTerm(tm)) {
          Yap_Error(TYPE_ERROR_CALLABLE, G, NULL);
          return false;
        }
        G = ArgOfTerm(2, G);
      } else {
        return true;
      }
    } else if (IsPairTerm(G) || IsAtomTerm(G)) {
      return true;
    } else {
      Yap_Error(TYPE_ERROR_CALLABLE, G, NULL);
      return false;
    }
  }
  return false;
}

static Int is_predicate_indicator(USES_REGS1) {
  Term G = Deref(ARG1);
  // Term Context = Deref(ARG2);
  Term mod = CurrentModule;

  G = Yap_YapStripModule(G, &mod);
  if (IsVarTerm(G)) {
    Yap_Error(INSTANTIATION_ERROR, G, NULL);
    return false;
  }
  if (!IsVarTerm(mod) && !IsAtomTerm(mod)) {
    Yap_Error(TYPE_ERROR_ATOM, G, NULL);
    return false;
  }
  if (IsApplTerm(G)) {
    Functor f = FunctorOfTerm(G);
    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_PREDICATE_INDICATOR, G, NULL);
    }
    if (f == FunctorSlash || f == FunctorDoubleSlash) {
      return true;
    }
  }
  Yap_Error(TYPE_ERROR_PREDICATE_INDICATOR, G, NULL);
  return false;
}

static Int close_error(USES_REGS1) {
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  return true;
}

#undef BEGIN_ERROR_CLASSES
#undef ECLASS
#undef END_ERROR_CLASSES
#undef BEGIN_ERRORS
#undef E0
#undef E
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
#define E2(X, Y, Z, W)
#define END_ERRORS()

#include <YapErrors.h>

#undef BEGIN_ERROR_CLASSES
#undef ECLASS
#undef END_ERROR_CLASSES
#undef BEGIN_ERRORS
#undef E0
#undef E
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
#define E2(X, Y, Z, W) {Y##__, Z " " W},
#define END_ERRORS()                                                           \
  { YAPC_NO_ERROR, "" }                                                        \
  }                                                                            \
  ;

#include <YapErrors.h>

yap_error_class_number Yap_errorClass(yap_error_number e) {
  return c_error_list[e].class;
}

const char *Yap_errorName(yap_error_number e) { return c_error_list[e].name; }

const char *Yap_errorClassName(yap_error_class_number e) {
  return c_error_class_name[e];
}

  Term Yap_GetException(void) {
    CACHE_REGS
    if (LOCAL_ActiveError->errorNo != YAP_NO_ERROR) {
      yap_error_descriptor_t *t =  LOCAL_ActiveError, *nt = malloc(sizeof(yap_error_descriptor_t));
      memcpy(nt,t,sizeof(yap_error_descriptor_t));
      Term rc = mkerrort(t->errorNo, Yap_BufferToTerm(t->culprit, TermNil), MkAddressTerm(nt));
     Yap_ResetException(worker_id);
      save_H();
      return rc;
    }
    return 0;
  }

  void Yap_PrintException(void) {
    printErr(LOCAL_ActiveError);
  }

  bool Yap_RaiseException(void) {
    if (LOCAL_ActiveError->errorNo == YAP_NO_ERROR)
      return false;
    return Yap_JumpToEnv();
  }

  bool Yap_ResetException(int wid) {
    // reset error descriptor
    yap_error_descriptor_t *bf = REMOTE_ActiveError(wid)->top_error;
    memset(REMOTE_ActiveError(wid), 0, sizeof(*LOCAL_ActiveError));
    REMOTE_ActiveError(wid)->top_error = bf;
    LOCAL_PrologMode &= ~InErrorMode;
    return true;
  }

  static Int reset_exception(USES_REGS1) { return Yap_ResetException(worker_id); }

 static Int read_exception(USES_REGS1) {
   yap_error_descriptor_t *t = AddressOfTerm(Deref(ARG1));
      Term rc = mkerrort(t->errorNo, Yap_BufferToTerm(t->culprit, TermNil), err2list(t));
      Yap_DebugPlWriteln(rc);
      return Yap_unify(ARG2, rc);
 }

 static Int query_exception(USES_REGS1) {
   const char *query;
   Term t;
   
   if (IsAtomTerm((t = Deref(ARG1))))
     query = RepAtom(AtomOfTerm(t))->StrOfAE;
   if (IsStringTerm(t))
     query = StringOfTerm(t);
   yap_error_descriptor_t *y = AddressOfTerm(Deref(ARG2));
       Term rc = queryErr(query, y);
      Yap_DebugPlWriteln(rc);
      return Yap_unify(ARG3, rc);
 }


 static Int drop_exception(USES_REGS1) {
   yap_error_descriptor_t *t = AddressOfTerm(Deref(ARG1));
   free(t);
   return true;
 }


  static Int get_exception(USES_REGS1) {
    Term t;
    if (Yap_HasException() && (t = Yap_GetException()) != 0) {
	Int rc= Yap_unify(t, ARG1);
	return rc;
      }
      return false;
  }

void Yap_InitErrorPreds(void) {
  CACHE_REGS
    Yap_InitCPred("$reset_exception", 1, reset_exception, 0);
    Yap_InitCPred("$get_exception", 1, get_exception, 0);
    Yap_InitCPred("$drop_exception", 1, get_exception, 0);
    Yap_InitCPred("$read_exception", 2, read_exception, 0);
    Yap_InitCPred("$query_exception", 3, query_exception, 0);
    Yap_InitCPred("$drop_exception", 1, drop_exception, 0);
  Yap_InitCPred("$close_error", 0, close_error, HiddenPredFlag);
  Yap_InitCPred("is_boolean", 2, is_boolean, TestPredFlag);
  Yap_InitCPred("is_callable", 2, is_callable, TestPredFlag);
  Yap_InitCPred("is_atom", 2, is_atom, TestPredFlag);
  Yap_InitCPred("is_predicate_indicator", 2, is_predicate_indicator,
                TestPredFlag);
}
