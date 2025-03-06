/*************************************************************************
 *									 *
 *	 Yap Prolog 							 *
 *									 *
 *	Yap Prolog Was Deeloped At Nccup - Universidade Do Porto	 *
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

/**

   @file errors.c

   @brief low-level error handling..

   
@defgroup YAPErrors Error Handling

@ingroup YAPControl

@{

@brief Error Implementation through error descriptors. 

+ The process is as
   follows:
   - Input the error from Prolog or C-code
   - Complete the descriptor with state information
   - Export the result either as a term or as a C-structure.
+   The key routines are:
   - Yap_ThrowError(): create an error descriptor and jump to the closest C
   longjmp handler.

   - ErrorFromProlog(): try to discriminate between a throw and an system error;
   if an error doThrowError().

   Also included are exception manipulation routines and higher level
   error handlers, based on pl/error.yap. See include/YapError.h for a
   definition of data-strEuctures, and include/YapErrors.h for a list of
   implementation supported exceptions.

+ Exception LifeCycle

   Exceptions are a dictionary that includee exception type, location, culprit,
   and user-defined. They start in the bottom stage, so that they:

   1. complete the dictionary;
   2. force a throw to the exception handler;
   3. close the bottom-half and move to the top-half.

   The top-half is Prolog code

*/
#include "Yap.h"


#include "Atoms.h"
#include "YapDefs.h"
#include "YapError.h"
#include "YapInterface.h"
#include "YapStreams.h"
#include "YapTags.h"
#include "Yapproto.h"
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
#include <iopreds.h>

void Yap_RestartYap(int flag) {
  CACHE_REGS
    if (!LOCAL_RestartEnv)
      return;
#if PUSH_REGS
  restore_absmi_regs(&Yap_standard_regs);
#endif
  siglongjmp(*LOCAL_RestartEnv, flag);
}

#define set_key_b(k, ks, q, i, t)                                              \
if (strcmp(ks, q) == 0) {						\
   if (IsVarTerm(t)) { Yap_ThrowError(INSTANTIATION_ERROR,t,"should be a boolean");}; \
    i->k = (t == TermTrue ? true : false);                                     \
    return t == TermTrue || t == TermFalse;                                             \
  }

#define set_key_i(k, ks, q, i, t)                                              \
  if (strcmp(ks, q) == 0) {                                                    \
    if (IsVarTerm(t)) { Yap_ThrowError(INSTANTIATION_ERROR,t,"should be a boolean");}; \
    i->k = IsIntegerTerm(t) ? IntegerOfTerm(t) : 0;                            \
    return IsIntegerTerm(t);                                                   \
  }

#define set_key_s(k, ks, q, i, t)                                              \
  if (strcmp(ks, q) == 0) {                                                    \
    if (IsVarTerm(t)) { Yap_ThrowError(INSTANTIATION_ERROR,t,"should be a text");}; \
    const char *s = IsAtomTerm(t) ? RepAtom(AtomOfTerm(t))->StrOfAE            \
: IsStringTerm(t) ? StringOfTerm(t) : NULL;  \
     if (s && s[0]) {                                                           \
      char *tmp = calloc(1, strlen(s) + 1);                                    \
      strcpy(tmp, s);                                                          \
      i->k = tmp;                                                              \
    }                                                                          \
    return i->k != NULL;                                                       \
  }

#define set_key_t(k, ks, q, i, t)                                              \
  if (strcmp(ks, q) == 0) {                                                    \
    i->k = Yap_SaveTerm(t);                                                    \
    return i->k != 0L;                                                         \
  }

static bool setErr(const char *q, yap_error_descriptor_t *i, Term t) {
  set_key_i(errorNo, "errorNo", q, i, t);
  set_key_i(errorClass, "errorClass", q, i, t);
  set_key_s(errorAsText, "errorAsText", q, i, t);
  set_key_s(errorAsText2, "errorAsText2", q, i, t);
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
  set_key_i(parserSize, "parserSize", q, i, t);
  set_key_i(parserLine, "parserLine", q, i, t);
  set_key_i(parserLinePos, "parserLinePos", q, i, t);
  set_key_i(parserFirstLine, "parserFirstLine", q, i, t);
  set_key_i(parserFirstLinePos, "parserFirstLinePos", q, i, t);
  set_key_i(parserLastLine, "parserLastLine", q, i, t);
  set_key_i(parserLastLinePos, "parserLastLinePos", q, i, t);
  set_key_s(parserTextA, "parserTextA", q, i, t);
  set_key_s(parserTextB, "parserTextB", q, i, t);
  set_key_s(parserFile, "parserFile", q, i, t);
  set_key_b(parserReadingCode, "parserReadingCode", q, i, t);
  set_key_b(prologConsulting, "prologConsulting", q, i, t);
  set_key_t(culprit_t, "culprit_t", q, i, t);
  set_key_t(errorUserTerm, "errorUserTerm", q, i, t);
  set_key_s(prologStack, "prologStack", q, i, t);
  set_key_s(errorMsg, "errorMsg", q, i, t);
  set_key_i(errorMsgLen, "errorMsgLen", q, i, t);
  return false;
}

#define query_key_b(k, ks, q, i)                                               \
  if (strcmp(ks, q) == 0) {                                                    \
    return i->k ? TermTrue : TermFalse;                                        \
  }

#define query_key_i(k, ks, q, i)                                               \
  if (strcmp(ks, q) == 0) {                                                    \
    return MkIntegerTerm(i->k);                                                \
  }

#define query_key_s(k, ks, q, i)                                               \
  if (strcmp(ks, q) == 0) {                                                    \
    if (i->k && i->k[0] != '\0')                                               \
      return MkAtomTerm(Yap_LookupAtom(i->k));                                 \
    else                                                                       \
      return TermEmpty;                                                    \
  }

#define query_key_t(k, ks, q, i)                                               \
  if (strcmp(ks, q) == 0) {                                                    \
    Term t;                                                                    \
    if ((t = i->k) == 0)                                                       \
      return TermNil;                                                          \
    return t;                                                                  \
  }

static Term queryErr(const char *q, yap_error_descriptor_t *i) {
  CACHE_REGS
    i->errorAsText =    (i->errorAsText && strlen(i->errorAsText) ? i->errorAsText : Yap_errorName(i->errorNo));
  i->errorAsText2 =  (i->errorAsText2  && strlen(i->errorAsText2) ? i->errorAsText2 : NULL);
  i->classAsText =  (i->classAsText && strlen(i->classAsText)? i->classAsText : Yap_errorClassName(i->errorClass)
   );
 query_key_i(errorNo, "errorNo", q, i);
  query_key_i(errorClass, "errorClass", q, i);
  query_key_s(errorAsText, "errorAsText", q, i);
  query_key_s(errorAsText2, "errorAsText2", q, i);
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
  query_key_i(parserSize, "parserSize", q, i);
  query_key_i(parserFirstLine, "parserFirstLine", q, i);
  query_key_i(parserLastLine, "parserLastLine", q, i);
  query_key_i(parserLinePos, "parserLinePos", q, i);
  query_key_i(parserFirstLinePos, "parserFirstLinePos", q, i);
  query_key_i(parserLastLinePos, "parserLastLinePos", q, i);
  query_key_s(parserTextA, "parserTextA", q, i);
  query_key_s(parserTextB, "parserTextB", q, i);
  query_key_s(parserFile, "parserFile", q, i);
  query_key_b(parserReadingCode, "parserReadingCode", q, i);
  query_key_b(prologConsulting, "prologConsulting", q, i);
  query_key_s(prologStack, "prologStack", q, i);
  query_key_t(culprit_t, "culprit_t", q, i);
  query_key_t(errorUserTerm, "errorUserTerm", q, i);
  Yap_ThrowError(DOMAIN_ERROR_FLAG_VALUE, MkAtomTerm(Yap_LookupAtom(q)), "Bad error parameter  %s in error query", q);
  return false;
}

static void print_key_b(FILE *of, const char *key, bool v) {
  const char *b = v ? "true" : "false";
  fprintf(of, "%s: %s\n", key, b);
}

static void print_key_i(FILE *of, const char *key, YAP_Int v) {
  fprintf(of, "%s: " Int_FORMAT "\n", key, v);
}

static void print_key_s(FILE *of, const char *key, const char *v) {
  if (v && v[0])
    fprintf(of, "%s: %s\n", key, v);
}

static void print_key_t(FILE *of, const char *key, YAP_Term v) {
  if (v)
    fprintf(
        of, "%s: %s\n", key,
        Yap_TermToBuffer(v,  YAP_WRITE_QUOTED| YAP_WRITE_IGNORE_OPS | YAP_WRITE_HANDLE_CYCLES));
}

static void printErr(yap_error_descriptor_t *i, FILE *out) {

  if (i->errorNo == YAP_NO_ERROR) {
    return;
  }
  print_key_s(out, "errorAsText",
              (i->errorAsText ? i->errorAsText : Yap_errorName(i->errorNo)));
  print_key_s(out, "errorAsText2",
              (i->errorAsText2 ? i->errorAsText2 : NULL));
  print_key_s(
      out, "classAsText",
      (i->classAsText ? i->classAsText : Yap_errorClassName(i->errorClass)));
  print_key_i(out, "parserPos", i->parserPos);
  print_key_i(out, "parserSize", i->parserSize);
  print_key_i(out, "parserLinePos", i->parserLinePos);
  print_key_i(out, "parserLine", i->parserLine);
  print_key_i(out, "parserFirstLine", i->parserFirstLine);
  print_key_i(out, "parserFirstLinePos", i->parserFirstLinePos);
  print_key_i(out, "parserLastLine", i->parserLastLine);
  print_key_i(out, "parserLastLinePos", i->parserLastLinePos);
  print_key_s(out, "parserTextA", i->parserTextA);
  print_key_s(out, "parserTextB", i->parserTextB);
  print_key_s(out, "parserFile", i->parserFile);
  print_key_i(out, "errorNo", i->errorNo);
  print_key_s(
      out, "errorClass",
      (i->classAsText ? i->classAsText : Yap_errorClassName(i->errorClass)));
  print_key_i(out, "errorLine", i->errorLine);
  print_key_s(out, "errorFunction", i->errorFunction);
  print_key_s(out, "errorFile", i->errorFile);
  print_key_i(out, "prologPredLine", i->prologPredLine);
  print_key_s(out, "prologPredName", i->prologPredName);
  print_key_i(out, "prologPredArity", i->prologPredArity);
  print_key_s(out, "prologPredModule", i->prologPredModule);
  print_key_s(out, "prologPredFile", i->prologPredFile);
  print_key_b(out, "parserReadingCode", i->parserReadingCode);
  print_key_b(out, "prologConsulting", i->prologConsulting);
  print_key_t(out, "culprit_t", i->culprit_t);
  print_key_s(out, "prologStack", i->prologStack);
  print_key_t(out, "errorUserterm", i->errorUserTerm);
  print_key_s(out, "errorMsg", (i->errorMsgLen == 0 ? "no message" : i->errorMsg));
  print_key_i(out, "errorMsgLen", i->errorMsgLen);
}

static YAP_Term add_key_b(const char *key, bool v, YAP_Term o0) {
  CACHE_REGS
  YAP_Term tkv[2];
  tkv[1] = v ? TermTrue : TermFalse;
  tkv[0] = MkAtomTerm(Yap_LookupAtom(key));
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static YAP_Term add_key_i(const char *key, YAP_Int v, YAP_Term o0) {
  CACHE_REGS
  YAP_Term tkv[2];
  tkv[1] = MkIntegerTerm(v), tkv[0] = MkAtomTerm(Yap_LookupAtom(key));
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static YAP_Term add_key_s(const char *key, const char *v, YAP_Term o0) {
  CACHE_REGS
  Term tkv[2];
  if (!v || v[0] == '\0')
    return o0;
  tkv[1] = MkStringTerm(v);
  tkv[0] = MkAtomTerm(Yap_LookupAtom(key));
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static YAP_Term add_key_t(const char *key, YAP_Term v, YAP_Term o0) {
  CACHE_REGS
  if (!v)
    return o0;
  Term tkv[2];
  tkv[0]= MkAtomTerm(Yap_LookupAtom(key));
  tkv[1] = v;
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
  return v;
}

static Term err2list(yap_error_descriptor_t *i) {
  Term o = TermNil;
  if (i->errorNo == YAP_NO_ERROR) {
    return o;
  }
  o = add_key_i("errorNo", i->errorNo, o);
  o = add_key_i("errorClass", i->errorClass, o);
  o = add_key_s("errorAsText",
		(i->errorAsText ? i->errorAsText : Yap_errorName(i->errorNo)), o);
  o = add_key_s( "errorAsText2",
		 (i->errorAsText2 ? i->errorAsText2 : NULL), o);
  o = add_key_s( "classAsText",
		 (i->classAsText ? i->classAsText : Yap_errorClassName(i->errorClass)), o);
  o = add_key_i("errorLine", i->errorLine, o);
  o = add_key_s("errorFunction", i->errorFunction, o);
  o = add_key_s("errorFile", i->errorFile, o);
  o = add_key_i("prologPredLine", i->prologPredLine, o);
  o = add_key_s("prologPredName", i->prologPredName, o);
  o = add_key_i("prologPredArity", i->prologPredArity, o);
  o = add_key_s("prologPredModule", i->prologPredModule, o);
  o = add_key_s("prologPredFile", i->prologPredFile, o);
  o = add_key_i("parserPos", i->parserPos, o);
  o = add_key_i("parserSize", i->parserSize, o);
  o = add_key_i("parserLinePos", i->parserLinePos, o);
  o = add_key_i("parserLine", i->parserLine, o);
  o = add_key_i("parserFirstLine", i->parserFirstLine, o);
  o = add_key_i("parserLastLine", i->parserLastLine, o);
  o = add_key_i("parserFirstLinePos", i->parserFirstLinePos, o);
  o = add_key_i("parserLastLinePos", i->parserLastLinePos, o);
  o = add_key_s("parserTextA", i->parserTextA, o);
  o = add_key_s("parserTextB", i->parserTextB, o);
  o = add_key_s("parserFile", i->parserFile, o);
  o = add_key_b("parserReadingCode", i->parserReadingCode, o);
  o = add_key_b("prologConsulting", i->prologConsulting, o);
  o = add_key_t("culprit_t", i->culprit_t, o);
  o = add_key_s("prologStack", i->prologStack, o);
  o = add_key_t("errorUserTerm", i->errorUserTerm, o);
  o = add_key_s("errorMsg", i->errorMsg, o);
  o = add_key_i("errorMsgLen", i->errorMsgLen, o);
  return o;
}

void Yap_do_warning__(const char *file, const char *function, int line,
                      yap_error_number type, Term t, const char *fmt, ...) {
  CACHE_REGS
  char tmpbuf[PATH_MAX];
  va_list ap;
  PredEntry *p;
  Term ts[2];
  return;
  yap_error_descriptor_t *e = calloc(1, sizeof(yap_error_descriptor_t));
  Yap_MkErrorRecord(e, file, function, line, type,ArgOfTerm(1,t),ArgOfTerm(2,t), "discontiguous warning");
  p = RepPredProp(PredPropByFunc(FunctorPrintMessage,
                                 PROLOG_MODULE)); // PROCEDURE_print_message2
  if (p->ArityOfPE) {

    // sc[0] = t;
    //    sc[1] = MkSysError(e);
    va_start(ap, fmt);
    fmt = va_arg(ap, char *);
    if (fmt != NULL) {
#if HAVE_VSNPRINTF
      vsnprintf(tmpbuf, PATH_MAX - 1, fmt, ap);
#else
      (void)vsprintf(tmpbuf, fmt, ap);
#endif
      va_end(ap);
      ts[1] = MkAtomTerm(AtomWarning);
      ts[0] = MkAtomTerm(Yap_LookupAtom(tmpbuf));
      Yap_execute_pred(p, ts, true PASS_REGS);
      Yap_ResetException(NULL);
      LOCAL_PrologMode &= ~InErrorMode;
    }
  }
}

bool Yap_Warning(const char *s, ...) {
  CACHE_REGS
  va_list ap;
  PredEntry *pred;
  bool rc;
  Term ts[2];
  char tmpbuf[PATH_MAX];
  yap_error_number err;

  if (LOCAL_PrologMode & InErrorMode && (err = LOCAL_ActiveError->errorNo)) {
    fprintf(stderr, "%% Warning %s WITHIN ERROR %s %s\n", s,
            LOCAL_ActiveError->classAsText, LOCAL_ActiveError->errorAsText);
    return false;
  }
  LOCAL_PrologMode |= InErrorMode;
  pred = RepPredProp(PredPropByFunc(FunctorPrintMessage,
                                    PROLOG_MODULE)); // PROCEDURE_print_message2
  va_start(ap, s);
#if HAVE_VSNPRINTF
    vsnprintf(tmpbuf, PATH_MAX - 1, s, ap);
#else
    (void)vsprintf(tmpbuf, s, ap);
#endif

  va_end(ap);
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    LOCAL_PrologMode &= ~InErrorMode;
    return false;
  }

    ts[1] = MkAtomTerm(AtomWarning);
  ts[0] = MkAtomTerm(Yap_LookupAtom(tmpbuf));
  rc = Yap_execute_pred(pred, ts, true PASS_REGS); 
  Yap_ResetException(NULL);
  LOCAL_PrologMode &= ~InErrorMode;
  return rc;
}


bool Yap_PrintWarning(Term twarning, Term level) {
  CACHE_REGS

  PredEntry *pred = RepPredProp(PredPropByFunc(
      FunctorPrintMessage, PROLOG_MODULE)); // PROCEDURE_print_message2;
  if (twarning)
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid ", " warning(%s)",
                        Yap_TermToBuffer(twarning,  |
                                                       YAP_WRITE_IGNORE_OPS |
                                                       YAP_WRITE_HANDLE_CYCLES));

  bool rc;
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    fprintf(stderr, "%s:" UInt_FORMAT "/* %s */:\n", LOCAL_ActiveError->parserFile,
            LOCAL_ActiveError->parserLine,
	    Yap_TermToBuffer(twarning,  YAP_WRITE_QUOTED|
			     YAP_WRITE_IGNORE_OPS |YAP_WRITE_HANDLE_CYCLES));
  }
  ARG2 = twarning;
  ARG1 = level;
  LOCAL_PrologMode &= ~InErrorMode;
  rc = Yap_execute_pred(pred, NULL, true PASS_REGS);
  LOCAL_within_print_message = false;

  return rc;
}

bool Yap_HandleError__(const char *file, const char *function, int lineno,
                       const char *s, ...) {
  CACHE_REGS
  yap_error_number err = LOCAL_Error_TYPE;
  const char *serr;
   if (LOCAL_PrologMode & InErrorMode) {
     LOCAL_ActiveError->errorNo = ABORT_EVENT;
     Yap_JumpToEnv();
   }
  if (LOCAL_ErrorMessage) {
    serr = LOCAL_ErrorMessage;
  } else {
    serr = s;
  }
  switch (err) {
  case RESOURCE_ERROR_STACK:
    if (!Yap_dogc(PASS_REGS1)) {
      Yap_ThrowError__(file, function, lineno, RESOURCE_ERROR_STACK, ARG1,
                  serr);
      return false;
    }
    LOCAL_PrologMode = UserMode;
    return true;
  case RESOURCE_ERROR_AUXILIARY_STACK:
    {
      size_t sz = (char *)AuxSp - AuxBase;

      if (!Yap_ExpandPreAllocCodeSpace(sz+1024, NULL, TRUE)) {
	/* crash in flames */
	Yap_ThrowError(RESOURCE_ERROR_AUXILIARY_STACK,
		    ARG1, serr);
	return false;
      }
      LOCAL_PrologMode = UserMode;
    }
    return true;
  case RESOURCE_ERROR_HEAP:
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_ThrowError__( file, function, lineno, RESOURCE_ERROR_HEAP, ARG2,
                  serr);
      return false;
    }
  default:

    if (LOCAL_PrologMode == UserMode) {
      Yap_ThrowError__(file, function, lineno, err, 0, NULL);
    } else
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
    if (!Yap_dogc(PASS_REGS1)) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, serr);
      return (FALSE);
    }
    return TRUE;
  case RESOURCE_ERROR_AUXILIARY_STACK:
    {
      size_t sz = (char *)AuxSp - AuxBase;

      if (!Yap_ExpandPreAllocCodeSpace(sz+1024, NULL, TRUE)) {
	/* crash in flames */
	Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK,
		    ARG1, serr);
	return false;
      }
      LOCAL_PrologMode = UserMode;
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

static void error_exit_yap(int value) {
  CACHE_REGS
  if (!(LOCAL_PrologMode & BootMode)) {

#if DEBUG
#endif
  }
  fprintf(stderr, "\n   Exiting ....\n");
  Yap_dump_stack(stderr);
#if HAVE_BACKTRACE
  void *callstack = malloc(64 * K);
  int i;
  int frames = backtrace(callstack, 64 * K - 1);
  char **strs = backtrace_symbols(callstack, frames);
 fprintf(stderr, "%% C-Execution stack:\n");
  for (i = 0; i < frames; ++i) {
    fprintf(stderr, "%%       %s\n", strs[i]);
    //  free(strs[i]);
  }
  free(callstack);
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
#undef ES
#undef E2
#undef END_ERRORS



#define BEGIN_ERROR_CLASSES()                                                         
#define END_ERROR_CLASSES()                                                         
#define ECLASS(CL, A, B)

#define BEGIN_ERRORS()                                                         
#define END_ERRORS()                                              
    
#define E0(A, B, C)                                                            \
  case A: {                                                                    \
    ft0 = MkAtomTerm(Yap_LookupAtom(C "_error"));                            \
  } break;

#define E(A, B, C)                                                             \
  case A: {                                                                    \
    Term nt[2];                                                         \
    nt[0] = MkAtomTerm(Yap_LookupAtom(C ));                                     \
    if (culprit) nt[1] = culprit; else nt[1] = MkVarTerm();		\
    ft0 = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(i->classAsText), 2), 2, nt);             \
  }break;

#define E1(A, B, C)                                                            \
  case A: {                                                                    \
    Term nt = MkAtomTerm(Yap_LookupAtom(C));				\
    ft0 = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(i->classAsText), 1), 1, &nt);        \
  }break;

#define ES(A, B, C)                                                            \
  case A: {                                                                    \
    Term nt = MkAtomTerm(Yap_LookupAtom(i->culprit));			\
    ft0 = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(i->classAsText), 1), 1, &nt);        \
  }break;

#define E2(A, B, C, D)                                                         \
  case A: {                                                                    \
    Term nt[3];                                                         \
    nt[0] = MkAtomTerm(Yap_LookupAtom(C));                                     \
    nt[1] = MkAtomTerm(Yap_LookupAtom(D));                                     \
    if (culprit) nt[2] = culprit; else nt[2] = MkVarTerm();                                                       \
    ft0 = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(i->classAsText), 3), 3, nt);             \
  } break;

Term Yap_MkErrorTerm(yap_error_descriptor_t *i) {
  CACHE_REGS
  if (i == NULL) {
    i = LOCAL_ActiveError;
  }
  if (i->errorUserTerm)
    { return i->errorUserTerm;}

  yap_error_number type = i->errorNo;
    Term culprit = i->culprit_t, ft0;
    i->errorAsText = Yap_errorName(type);
    i->errorAsText2 = Yap_errorName2(type);
    i->errorClass = Yap_errorClass(type);
    i->classAsText = Yap_errorClassName(LOCAL_ActiveError->errorClass);    switch (i->errorNo) {
#include "YapErrors.h"
  }
    
    Term *o = (HR);
    HR += 3;
    o[0] = (CELL)FunctorError;
    o[1] = ft0;
    o[2] = TermNil;
    return AbsAppl(o);
  }


/// add a new error descriptor, either to the top of the  stack,
/// or as the top;
yap_error_descriptor_t *
Yap_pushErrorContext(bool link, yap_error_descriptor_t *new_error,
                     yap_error_descriptor_t *old_error) {

  CACHE_REGS
  memset(new_error, 0, sizeof(yap_error_descriptor_t));
  if (link)
    new_error->top_error = LOCAL_ActiveError;
  LOCAL_ActiveError = new_error;
  return old_error;
}

/* static void */
/* reset_error_description(void) { */
/*   yap_error_descriptor_t *bf = LOCAL_ActiveError->top_error; */
/*   if (Yap_HasException()) */
/*   memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError)); */
/*   LOCAL_ActiveError->top_error = bf; */
/* } */
yap_error_descriptor_t *Yap_popErrorContext(bool mdnew, bool pass,
                                            yap_error_descriptor_t *old_error) {
  CACHE_REGS
  yap_error_descriptor_t *e = LOCAL_ActiveError,
                         *ep = LOCAL_ActiveError->top_error;
  // last block
  LOCAL_ActiveError = ep;
  if (e->errorNo && !ep->errorNo && pass) {
    yap_error_descriptor_t *epp = ep->top_error;
    memmove(ep, e, sizeof(*e));
    ep->top_error = epp;
  }
  return old_error;
}

/**
 * Thow an error directly to the error handler
 *
 * @param file      where
 * @param function  who
 * @param lineno    when
 * @param type      what, error code
 * @param where     how, user information
 */
void Yap_ThrowError__(const char *file, const char *function, int lineno,
                      yap_error_number type, Term where, const char *msg, ...) {
  CACHE_REGS
  va_list ap;
  if (LOCAL_ActiveError->errorNo != YAP_NO_ERROR) {
    return;
  }
  char tmp[4096];
  va_start(ap,msg);
  if (msg) {
    vsnprintf(tmp, 4095,msg,ap);
  }
  va_end(ap);
  pop_text_stack(1);
  Yap_Error__(true, file, function, lineno, type, where, tmp, NULL);
  if (LOCAL_ActiveError->errorUserTerm ) {
      LOCAL_ActiveError->errorUserTerm = Yap_SaveTerm(LOCAL_ActiveError->errorUserTerm);
  } else {
    LOCAL_ActiveError->culprit = NULL;

    LOCAL_PrologMode = UserMode;
  }
  Yap_RestartYap(5);
}

/// complete delayed error.

void Yap_ThrowExistingError(void) {
  CACHE_REGS
  P=FAILCODE;
  Yap_RestartYap(5);
}

/// Wrap the error descriptor as exception/2

Term MkSysError(yap_error_descriptor_t *i) {
  CACHE_REGS
  Term et = MkAddressTerm(i);
  return Yap_MkApplTerm(FunctorException, 1, &et);
}

#if 0
static void
termToError(Term t1, Term user_info, yap_error_descriptor_t *i) {
CACHE_REGS
  if (!i) i = LOCAL_ActiveError;

 if (IsAtomTerm(t1)) {
   i->classAsText = i->errorAsText = RepAtom(AtomOfTerm(t1))->StrOfAE;
   i->errorClass = Yap_errorClassNumber(i->classAsText);
   i->culprit_t = TermNil;
   if (i->errorClass == INSTANTIATION_ERROR_CLASS)
     i->errorNo = INSTANTIATION_ERROR;
 } else if (IsApplTerm(t1)) {
   char *s1 = NULL;
      Term t11 = ArgOfTerm(1, t1);
      Functor f = FunctorOfTerm(t1);
      arity_t a = ArityOfFunctor(f);
      i->classAsText = RepAtom(NameOfFunctor(f))->StrOfAE;
      i->errorClass = Yap_errorClassNumber(i->classAsText);
      if (IsAtomTerm(t11)) {
         s1 = RepAtom(AtomOfTerm(t11))->StrOfAE;
      } else {
	return;
      }
      if (a == 1) {
        i->errorAsText = i->classAsText;
       i->culprit_t = TermNil;
         i->errorAsText = NULL;
         i->errorAsText2 = NULL;
      } else if (a == 3) {
       Term t12 = ArgOfTerm(2,t1);
        if (IsAtomTerm(t12)) {
          char *s2 = RepAtom(AtomOfTerm(t12))->StrOfAE;
         i->errorAsText = s1;
         i->errorAsText2 = s2;
        }
	i->culprit_t = Yap_SaveTerm(ArgOfTerm(3,t1));
      } else { // a ==2b..............................
	i->culprit_t = Yap_SaveTerm(ArgOfTerm(2,t1));
	
	i->errorAsText = s1;
	i->errorAsText2 = NULL;
      } 

    }
 i->errorNo = Yap_errorNumber(i->errorClass,i->classAsText, i->errorAsText2);


    char *buf, *msg;
    if (IsStringTerm(user_info)) {
     buf = StringOfTerm(user_info);
      msg = "user text";
  } else if (IsPairTerm(user_info)) {
      buf = Yap_TextTermToText(user_info PASS_REGS);
      msg = "user text";
  } else { 
      buf =    Yap_TermToBuffer(user_info,  YAP_WRITE_QUOTED|
                    YAP_WRITE_IGNORE_OPS |YAP_WRITE_HANDLE_CYCLES);
      msg = "user goal";
   }
    size_t bsize = 32;
    if (buf) bsize += strlen(buf)+1;
    if (msg) bsize += strlen(msg)+1;
      i->errorMsg = malloc(bsize);
      snprintf(i->errorMsg, bsize-1,  "%% %s:  text: %s...", msg, buf);
      i->errorMsgLen = strlen(i->errorMsg);
  //  return Yap_SaveTerm(Yap_MkErrorTerm(i));
  // return MkStringTerm(i->errorMsg);

}
#endif

bool Yap_MkErrorRecord(yap_error_descriptor_t *r, const char *file,
                       const char *function, int lineno, yap_error_number type,
                       Term where, Term extra, const char *s) {
  CACHE_REGS
    if (type!= EVALUATION_ERROR_UNDEFINED ) { //&& LOCAL_Undef_CP == NULL) {
    if (!Yap_pc_add_location(r, P, B, ENV))
      Yap_env_add_location(r, CP, B, ENV, 0);
  } 
 r->errorNo = type;
 if (type != USER_DEFINED_EVENT &&
     type != USER_DEFINED_ERROR ) {

   if (where)
     r->culprit_t = Yap_SaveTerm(where);
   r->culprit = NULL;
   r->errorAsText = Yap_errorName(type);
   r->errorAsText2 = Yap_errorName2(type);
   r->errorClass = Yap_errorClass(type);
   r->classAsText = Yap_errorClassName(r->errorClass);
 }
   
 type = LOCAL_ActiveError ->errorNo;

 
  if (s && s[0]) {
    size_t sz = r->errorMsgLen = strlen(s);
    char *ns = malloc(sz+1);
    memcpy(ns, s, sz+1);
    r->errorMsg = ns;
    r->errorMsgLen = sz;
  } else {
    r->errorMsgLen = 0;
    r->errorMsg = NULL;
}
  if (type != SYNTAX_ERROR) {
    const char *s =  RepAtom(Yap_source_file_name())->StrOfAE;
     r->parserFile = strcpy(malloc(strlen(s)+1),s);
     r->parserLine = r->parserFirstLine = LOCAL_StartLineCount;
    r->parserLastLine = Yap_source_line_no();
    r->parserPos = Yap_source_pos();
    r->parserSize = 0;
    r->parserLinePos = Yap_source_line_pos();
  }


 r->errorLine = r->parserLine;

  return r; 
}



/** convert  a C-error to a Prolog term:
 *
 - fill text fields
 - wrap descriptor1240
*/
Term Yap_MkFullError(yap_error_descriptor_t *i, yap_error_number type) {
  CACHE_REGS
  if (i == NULL)
    i = (LOCAL_ActiveError);

  memset(i, 0, sizeof(*LOCAL_ActiveError));
  if (type != USER_DEFINED_ERROR) {
    i->errorAsText = Yap_errorName(type);
    i->errorAsText2 = Yap_errorName2(type);
    i->errorClass = Yap_errorClass(type);
    i->classAsText = Yap_errorClassName(i->errorClass);
  }


 if (type == SYNTAX_ERROR) {
    i->errorClass = SYNTAX_ERROR_CLASS;
    //      Yap_syntax_error(r, LOCAL_tokptr, LOCAL_toktide);
  } else if (type == SYNTAX_ERROR_NUMBER) { // Yap_syntax_error(r, LOCAL_tokptr,
                                            // LOCAL_toktide);
    i->errorClass = SYNTAX_ERROR_CLASS;
    type = SYNTAX_ERROR;
  }
  if (type == INTERRUPT_EVENT) {
    fprintf(stderr, "%% YAP exiting: cannot handle signal\n");
    Yap_exit(1);
  }
  return true;
}



/**
   @brief Yap_Error()
   This function handles errors in the C code. Check errors.yap for the
   corresponding Prolog code.

   @param file      C source
   @param function  C function
   @param lineno    C exact line
   @param type      the error ID (in YAP this is a single integer)
   @param where     the culprit
   @return usually FAILCODE

   In a good day, the error handler's job is to generate a throw. This includes:
   - constructing an ISO style error term;
   - constructing a list with all available info on the bug
   - generating the throw
   - forcing backtracking in order to restart.
4
   In a bad day, it has to deal with OOM, abort, and errors within errorts.
*/
yamop *Yap_Error__(bool throw, const char *file, const char *function,
                   int lineno, yap_error_number type, Term where, const char *fmt, ...) {
  CACHE_REGS

    
    char  s[PATH_MAX];
    LOCAL_OldP = P;
    LOCAL_OldCP = CP;
    if (where == 0)
      where = TermNil;
    s[0]='\0';
  if (!LOCAL_ActiveError) {
    LOCAL_ActiveError = Yap_GetException();
  }
   switch (type) {
  case SYSTEM_ERROR_INTERNAL: {
    Yap_flush_all();
    fprintf(stderr, "%% Internal YAP Error: %s exiting....\n", tmpbuf);
    if (LOCAL_PrologMode & BootMode) {
      fprintf(stderr, "%% YAP crashed while booting %s\n", tmpbuf);
    } else {
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
    return NULL;
  }
  case INTERRUPT_EVENT: {
    error_exit_yap(1);
    return NULL;
  }
  default:
    if (LOCAL_ActiveError->errorNo != YAP_NO_ERROR) {
      Yap_flush_all();
      yap_error_number err = LOCAL_ActiveError->errorNo;
      fprintf(stderr, "%% Warning %d WITHIN ERROR\n", (err));
      return FAILCODE;
    }
  }
  switch (type) {
  case USER_DEFINED_ERROR:
    {
      LOCAL_ActiveError->errorNo = type;     
      LOCAL_ActiveError->errorUserTerm = (where);
    }
    break;
  case EVALUATION_ERROR_UNDEFINED:
  case PERMISSION_ERROR_ACCESS_PRIVATE_PROCEDURE:
  case PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE:
  case TYPE_ERROR_CALLABLE:
  case TYPE_ERROR_PREDICATE_INDICATOR:
    {
      Functor f;
      Term t;
      if (IsApplTerm(where) &&
	  (f=FunctorOfTerm(where)) ==FunctorModule &&
	  (((t = ArgOfTerm(1,where))  == CurrentModule) ||
	   t == TermProlog)) {
	where = ArgOfTerm(2,where);
      }
      if(IsApplTerm(where) &&
	 FunctorOfTerm(where) ==FunctorSlash &&
	 ArgOfTerm(2,where)  == MkIntTerm(0) &&
	 !IsAtomTerm((t=ArgOfTerm(1,where)))) {
	where = t;
      } 
    }
    break;
  case USER_DEFINED_EVENT:    
      LOCAL_ActiveError->errorUserTerm = (where);
  case THROW_EVENT:
    {
      LOCAL_ActiveError->errorNo = type;
      LOCAL_PrologMode &= ~InErrorMode;
    }
    break;
  case ABORT_EVENT:
    {
      //	fun = FunctorDollarVar;
      //  serious = true;
      LOCAL_ActiveError->errorNo = ABORT_EVENT;
      P = FAILCODE;
      LOCAL_PrologMode &= ~InErrorMode;
      LOCAL_PrologMode |= AbortMode;
    }
    break;
  case CALL_COUNTER_UNDERFLOW_EVENT:
    // Do a long jump
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    LOCAL_ActiveError->errorNo = CALL_COUNTER_UNDERFLOW_EVENT;
    //_JumpToEnv();
    P = FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  case PRED_ENTRY_COUNTER_UNDERFLOW_EVENT:
    // Do a long jump
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    LOCAL_ActiveError->errorNo = PRED_ENTRY_COUNTER_UNDERFLOW_EVENT;
    Yap_JumpToEnv();
    P = FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  case RETRY_COUNTER_UNDERFLOW_EVENT:
    // Do a long jump
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    LOCAL_ActiveError->errorNo = RETRY_COUNTER_UNDERFLOW_EVENT;
    Yap_JumpToEnv();
    P = FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  default:
    if (fmt != NULL) {
      if (throw) {

	strncpy(s,fmt,PATH_MAX);
      } else {
      
  va_list ap;
  va_start(ap, fmt);
#if HAVE_VSNPRINTF
      (void)vsnprintf(s, PATH_MAX - 1, fmt, ap);
      #else
      (void)vsprintf(s, fmt, ap);
#endif
      va_end(ap);
      }
      break;
    }
  }
  bool is_error = false;
  Term user_info = TermNil;
  if (LOCAL_ActiveError->errorNo <= USER_DEFINED_ERROR)  {
    if (LOCAL_ActiveError->errorNo == USER_DEFINED_ERROR) {
      user_info = ArgOfTerm(2,where);
  }
    is_error = true;
  }
  

  
  if (is_error) {
    Yap_MkErrorRecord(LOCAL_ActiveError, file, function, lineno, type, 
		    where, user_info, s);
       
  }


  // PURE_ABORT may not have set where correctly, BootMode may not have the data
  // terms ready
  if (type == ABORT_EVENT || LOCAL_PrologMode & BootMode) {
    LOCAL_PrologMode &= ~AbortMode;
    LOCAL_PrologMode &= ~InErrorMode;
    P = FAILCODE;
  } else {
    // Exit Abort Mode, if we were there
    LOCAL_PrologMode &= ~AbortMode;
    LOCAL_PrologMode |= InErrorMode;
  }

    // make sure failure will be seen at next port
    // no need to lock & unlock
    if (LOCAL_PrologMode & AsyncIntMode)
      Yap_signal(YAP_FAIL_SIGNAL);

#ifdef DEBUG
  // DumpActiveGoals( USES_REGS1 );
#endif // DEBUG
  if (
      trueGlobalPrologFlag(STACK_DUMP_ON_ERROR_FLAG)	 ) {
    LOCAL_ActiveError->prologStack =  Yap_dump_stack(NULL);
  } else  {
    LOCAL_ActiveError->prologStack = NULL;
  }
  CalculateStackGap(PASS_REGS1);
#if DEBUG
  // DumpActiveGoals( PASS_REGS1 );
#endif
  // wait if we we are in user code,
  // it's up to her to decide
  if (LOCAL_delay)
    return P;

  

   if (P == (yamop *)(FAILCODE)) {
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  }
  //  reset_error_description();
  pop_text_stack(LOCAL_MallocDepth + 1);
  return P;
}


static Int close_error(USES_REGS1) {
  if (!LOCAL_CommittedError)
   return true;
  LOCAL_CommittedError->errorNo = YAP_NO_ERROR;
  memset(LOCAL_CommittedError, 0, sizeof(yap_error_descriptor_t));
  LOCAL_ErrorMessage = NULL;
  free(LOCAL_CommittedError);
  LOCAL_CommittedError = NULL;
  return true;
}

typedef struct c_error_info {
  yap_error_number errnb;
  yap_error_class_number class;
  const char *name, *name2;
} c_error_t;


#undef BEGIN_ERROR_CLASSES
#undef ECLASS
#undef END_ERROR_CLASSES
#undef BEGIN_ERRORS
#undef E0
#undef E
#undef E1
#undef ES
#undef E2
#undef END_ERRORS

#define BEGIN_ERROR_CLASSES() typedef enum aux_class {

#define ECLASS(CL, A, B) CL##__,

#define END_ERROR_CLASSES()                                                    \
  }                                                                            \
  aux_class_t;

#define BEGIN_ERRORS()
#define E0(X, Y, Z)
#define E(X, Y, Z)
#define E1(X, Y, Z)
#define ES(X, Y, Z)
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
#undef ES
#undef E2
#undef END_ERRORS

#define BEGIN_ERROR_CLASSES() static const char *c_error_class_name[] = {

#define ECLASS(CL, A, B) A,

#define END_ERROR_CLASSES()                                                    \
  NULL                                                                         \
  }

#define BEGIN_ERRORS() static struct c_error_info c_error_list[] = {
#define E0(X, Y, Z) {X, Y, Z, NULL},
#define E(X, Y, Z) {X, Y, Z, NULL},
#define E1(X, Y, Z) {X, Y, NULL, NULL},
#define ES(X, Y, Z) {X, Y, NULL, NULL},
#define E2(X, Y, Z, W) {X, Y, Z , W},
#define END_ERRORS()                                                           \
  { 0, YAPC_NO_ERROR, "" }                                                     \
  }                                                                            \
  ;

#include <YapErrors.h>

char *Yap_errorName(yap_error_number e) {
  if (e != USER_DEFINED_ERROR)
    return (char *)c_error_list[e].name;
  return NULL;
}

 char *Yap_errorName2(yap_error_number e) {
  if (e != USER_DEFINED_ERROR)
    return (char *)c_error_list[e].name2;
  return NULL;
}

yap_error_class_number Yap_errorClass(yap_error_number e) {
  return c_error_list[e].class;
}

yap_error_class_number Yap_errorClassNumber(const char *s) {
  Int i = 1;
  while (c_error_class_name[i] && strcmp(c_error_class_name[i], s) != 0) {
    i++;
  }
  if (!c_error_class_name[i])
    return USER_DEFINED_ERROR_CLASS;
  return i;
}
char *Yap_errorClassName(yap_error_class_number e) {
  return (char *)c_error_class_name[e];
}

static Int reset_exception(USES_REGS1) { return Yap_ResetException(NULL); }

/**

Notice that if
the argument is an error descriptor, and you pass NULL, they always
expect it to refer the current Active error descriptor.

*/

/**
 * is an error active?
 */
yap_error_descriptor_t *Yap_PeekException(void) {
  CACHE_REGS
  if (LOCAL_ActiveError->errorNo)
    return LOCAL_ActiveError;
  else
    return NULL;
}

/**
 * clone Active Error
 */
yap_error_descriptor_t *Yap_GetException(void) {
  CACHE_REGS
  yap_error_descriptor_t *t = LOCAL_ActiveError,
                         *nt = calloc(1, sizeof(yap_error_descriptor_t));
  memmove(nt, t, sizeof(yap_error_descriptor_t));
  return nt;
}

/**
 * print descriptor to user_output/stdout
 */
void Yap_PrintException(yap_error_descriptor_t *i) {
  CACHE_REGS
  FILE *of = GLOBAL_Stream[LOCAL_c_error_stream].file
                 ? GLOBAL_Stream[LOCAL_c_error_stream].file
                 : stderr;
  if (!i)
    i  = LOCAL_ActiveError;
  printErr(i, of);
}

/**
 * let's go.
 */
bool Yap_RaiseException() {
  CACHE_REGS
  if (LOCAL_ActiveError->errorNo) {
    Yap_ThrowExistingError();
    return true;
  }
  return false;
}

/**
v * clean up (notice that the code ensures  ActiveError exists on exit.
 */
bool Yap_ResetException(yap_error_descriptor_t *i) {
  CACHE_REGS
    // reset error descriptor
    if (!i)
      i = LOCAL_ActiveError;
  i = memset(i, 0, sizeof(yap_error_descriptor_t));
  LOCAL_PrologMode &= ~InErrorMode;
  return true;
}

/**
 * clean up (notice that the code ensures  ActiveError exists on exit.
 */
bool Yap_RestartException(yap_error_descriptor_t *i) {
  CACHE_REGS
    // reset error descriptor
    if (i)
      memcpy(LOCAL_ActiveError, i, sizeof(yap_error_descriptor_t));
  LOCAL_PrologMode |= InErrorMode;
  return true; Yap_JumpToEnv();
}

/// transform an exception into Prolog shape (dictionary or list)
/// 
static Int read_exception(USES_REGS1) {
  yap_error_descriptor_t *e;
  Term t = Deref(ARG1), rc;
  if (IsAddressTerm(t)) {
    e = AddressOfTerm(t);
    if (e == nullptr)
      return false;
    rc = err2list(e);
  } else {
    rc = t;
  }
  //      Yap_DebugPlWriteln(rc);
  return Yap_unify(ARG2, rc);
}

static Int print_exception(USES_REGS1) {
  Term t1 = Deref(ARG1);
  if (IsAddressTerm(t1)) {
    FILE *of = GLOBAL_Stream[LOCAL_c_error_stream].file ?
      GLOBAL_Stream[LOCAL_c_error_stream].file
      : stderr;
    yap_error_descriptor_t *t = AddressOfTerm(t1);
    if (t->parserFile && t->parserLine) {
      fprintf(of, "\n%s:%ld:0 error: while parsing %s\n\n", t->parserFile,
              t->parserLine, t->errorAsText);
    } else if (t->prologPredFile && t->prologPredLine) {
      fprintf(of, "\n%s:%ld:0 error: while running %s\n\n", t->prologPredFile,
              t->prologPredLine, t->errorAsText);
    } else if (t->errorFile && t->errorLine) {
      fprintf(of, "\n%s:%ld:0 error: while executing %s\n\n", t->errorFile,
              t->errorLine, t->errorAsText);
    }
    printErr(t, of);
  } else {
    return Yap_WriteTerm(LOCAL_c_error_stream, t1, TermNil PASS_REGS);
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
  else if (IsStringTerm(t))
    query = StringOfTerm(t);
  else
    return false;
  if (!IsAddressTerm(Deref(ARG2)))
    return false;
  yap_error_descriptor_t *y = AddressOfTerm(Deref(ARG2));
  Term t3 = Deref(ARG3);
    return setErr(query, y, t3);
}

static Int drop_exception(USES_REGS1) {
  Term tn;
  bool rc = false;
  if (LOCAL_Error_TYPE) {
    if (LOCAL_Error_TYPE == USER_DEFINED_EVENT) {
      rc = Yap_unify(LOCAL_ActiveError->errorUserTerm, ARG1) &&
	Yap_unify( TermNil, ARG2);
    } else if (LOCAL_Error_TYPE == USER_DEFINED_ERROR ) {
      rc = Yap_unify(LOCAL_ActiveError->errorUserTerm, ARG1) &&
	Yap_unify( ( err2list(LOCAL_ActiveError)), ARG2);
      if (rc) {
      Term t1 = Deref(ARG1);
      Functor f;
      if (IsApplTerm(t1) && (f=FunctorOfTerm(t1)) &&
	  !IsExtensionFunctor(f) &&
	  ArityOfFunctor(f)==2&&
			 IsVarTerm(ArgOfTerm(2,t1))) {
	Yap_unify(ArgOfTerm(2,t1), ARG2);
      }
      }
    } else {
      tn = ( Yap_MkErrorTerm(LOCAL_ActiveError));
      rc = Yap_unify(tn, ARG1) && Yap_unify( ( err2list(LOCAL_ActiveError)), ARG2);
    }
    memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError));
  } else {
    rc = false;
  }
  LOCAL_PrologMode &= ~InErrorMode;
  return rc;
}

static Int new_exception(USES_REGS1) {
  Term t = MkSysError(calloc(1, sizeof(yap_error_descriptor_t)));
  return Yap_unify(ARG1, t);
}


static Int user_exception(USES_REGS1) {
  if (!Yap_MkErrorRecord(LOCAL_ActiveError,NULL,NULL,-1,USER_DEFINED_ERROR,
			 Deref(ARG1), Deref(ARG2), NULL)) {
    return false;
  }
  LOCAL_ActiveError->prologPredFile=RepAtom(AtomOfTerm(Deref(ARG3 )))->StrOfAE;
  LOCAL_ActiveError->prologPredLine=IntegerOfTerm(Deref(ARG4 ));
  LOCAL_ActiveError->prologPredModule=RepAtom(AtomOfTerm(Deref(ARG5 )))->StrOfAE;
  LOCAL_ActiveError->prologPredName=RepAtom(AtomOfTerm(Deref(ARG6 )))->StrOfAE;
  LOCAL_ActiveError->prologPredArity=IntegerOfTerm(Deref(ARG7 ));
      Term *o = (HR);
    HR += 3;
    o[0] = (CELL)FunctorError;
    o[1] = Deref(ARG1);
    o[2] = Deref(ARG2);
    LOCAL_ActiveError->errorUserTerm = Yap_SaveTerm( AbsAppl(o) );
    P = FAILCODE;
  Yap_ThrowExistingError();
  return true;
  }

bool Yap_get_exception(void) {
  Term tn = Yap_GetGlobal(AtomZip);
  if (!IsVarTerm(tn) && tn != TermNil)
    return true;
  return false;
}

/** given a string(s, lookup for a corresponding error class
    r   numbe */
/** given a string(s) and class context, lookup for a corresponding error
    number */
yap_error_number Yap_errorNumber(yap_error_class_number c, const char *s, const char *s2) {
  Int i = 1;
  if (s==NULL || !s)
    
  while (c_error_list[i].class &&
	 c_error_list[i].class !=  c) {
    i++;
  }
  while (c_error_list[i].name) {
      if (c_error_list[i].name == NULL)
	return c_error_list[i].errnb;
      else if (strcmp(c_error_list[i].name, s) == 0) {
	if (!s2 || !c_error_list[i].name2 || (s2 && strcmp(c_error_list[i].name2, s2) == 0))
	// found it!
	return c_error_list[i].errnb;}

  i++;
  }
  return USER_DEFINED_ERROR ;
}

yap_error_descriptor_t *event(Term t, yap_error_descriptor_t *i) {
  i->errorNo = ERROR_EVENT;
  i->errorClass = EVENT;

  return i;
}

/**

 */

Int is_nonvar__(const char *file, const char *function, int lineno,
                Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  return t == TermTrue || t == TermFalse;
}

/**
 * @pred is_nonvar(T)
 *
 * True if the term _T_ has been instantiated, otherwise causs an exception. The
 * same as
 * must_be_bound/1.
*/
static Int is_nonvar1(USES_REGS1) {
  Term t = Deref(ARG1);
  return is_nonvar__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}

bool is_boolean__(const char *file, const char *function, int lineno,
                  Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  return t == TermTrue || t == TermFalse;
}

bool is_codes__(const char *file, const char *function, int lineno,
                  Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, NULL);
    return false;
  }
    Term *tailp;
  Yap_SkipList(&t, &tailp);
  return *tailp == TermNil;

  return t == TermTrue || t == TermFalse;
}

static Int is_boolean1(USES_REGS1) {
  Term t = Deref(ARG1);
  return is_boolean__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}

bool must_be_boolean__(const char *file, const char *function, int lineno,
                       Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  if (t == TermTrue || t == TermFalse)
    return true;
  else {
    Yap_ThrowError__(file, function, lineno, TYPE_ERROR_BOOLEAN, t, NULL);
    return false;
  }
}

static Int must_be_boolean1(USES_REGS1) {
  Term t = Deref(ARG1);
  return must_be_boolean__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}

bool is_atom__(const char *file, const char *function, int lineno,
               Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  return IsAtomTerm(t);
}

static Int is_atom1(USES_REGS1) {
  Term t = Deref(ARG1);
  return is_atom__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}

bool must_be_arity__(const char *file, const char *function, int lineno,
                    Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, "is atom");
    return false;
  }
  if (!IsIntegerTerm(t)) {
    Yap_ThrowError__(file, function, lineno, TYPE_ERROR_INTEGER, t, "is atom");
    return false;
  }
  Int i = IntegerOfTerm(t);
  if (i < 0) { 
    Yap_ThrowError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, NULL);
      return false;
  }
  return true;
}
static Int must_be_arity1(USES_REGS1) {
  Term t = Deref(ARG1);
  return must_be_arity__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}
bool must_be_atom__(const char *file, const char *function, int lineno,
                    Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, "is atom");
    return false;
  }
  if (IsAtomTerm(t))
    return true;
  else {
    Yap_ThrowError__(file, function, lineno, TYPE_ERROR_ATOM, t, "is atom");
    return false;
  }
}

static Int must_be_atom1(USES_REGS1) {
  Term t = Deref(ARG1);
  return must_be_atom__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}


bool must_be_string__(const char *file, const char *function, int lineno,
                    Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, "is atom");
    return false;
  }
  if (!IsStringTerm(t)) {
    Yap_ThrowError__(file, function, lineno, TYPE_ERROR_STRING, t, "is atom");
    return false;
  }
  return true;
}

static Int must_be_string1(USES_REGS1) {
  Term t = Deref(ARG1);
  return must_be_string__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}


bool must_be_char__(const char *file, const char *function, int lineno,
                    Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, "is atom");
    return false;
  }
  if (!IsAtomTerm(t)) {
    Yap_ThrowError__(file, function, lineno, TYPE_ERROR_IN_CHARACTER, t, "is atom");
    return false;
  }
  if (t==TermEof)
    return true;
  u_char *a=RepAtom(AtomOfTerm(t))->UStrOfAE;
  utf8proc_int32_t  v;
  int off = 0;
  if(a[0] == '\0' ||(off=__get_utf8(a,1,&v PASS_REGS))<=0 || a[off] != '\0') {
      Yap_ThrowError(TYPE_ERROR_CHARACTER_CODE, t, NULL);
      return false;
  }
  return true;
}

static Int must_be_char1(USES_REGS1) {
  Term t = Deref(ARG1);
  return must_be_char__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}


bool must_be_code__(const char *file, const char *function, int lineno,
                    Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, "is atom");
    return false;
  }
  if (!IsIntegerTerm(t)) {
    Yap_ThrowError__(file, function, lineno, TYPE_ERROR_INTEGER, t, "is atom");
    return false;
  }
  Int i = IntegerOfTerm(t);
  if (i < -1 || i > CHARCODE_MAX) { 
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t, NULL);
      return false;
  }
  return true;
}

static Int must_be_code1(USES_REGS1) {
  Term t = Deref(ARG1);
  return must_be_code__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}



/** @pred is_list( ?_List_ )
 *
 *  _List_ must be a list, that is, it must be bound  to a true list.
 *
 * The predicate causes an instantiation  exception if _List_ is unbound, and
 * fails if _Term_ is bound but not to a list.
 */
bool is_list__(const char *file, const char *function, int lineno,
               Term list USES_REGS) {
  Term *tailp;
  // Term Context = Deref(ARG2);
  Int n = Yap_SkipList(&list, &tailp);
  if (IsVarTerm(*tailp))
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, list, "is list");
  if (*tailp != TermNil || n < 0) {
    return false;
  }
  return true;
}

static Int is_list1(USES_REGS1) {
  Term t = Deref(ARG1);
  return is_list__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}

bool must_be_list__(const char *file, const char *function, int lineno,
                    Term list USES_REGS) {
  Term *tailp;
  // Term Context = Deref(ARG2);
  Int n = Yap_SkipList(&list, &tailp);
  if (IsVarTerm(*tailp))
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, list, "must be list");
  if (*tailp != TermNil || n < 0) {
    return false;
  }
  return true;
}

static Int must_be_list1(USES_REGS1) {
  Term t = Deref(ARG1);
  return must_be_list__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}

/** @pred callable( ?_Goal_ )
 *
 *  _Goal must be callable, that is, it must be bound and also must be
 *  either a compound term or an atom.
 *
 *  Either succeeds or fails.
 */
Int callable(USES_REGS1) {
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {

    return false;
  }
  Term mod = CurrentModule;
  if (mod == 0)
    mod = TermProlog;
  Term G = Yap_StripModule(Deref(ARG1), &mod);
  // Term Context = Deref(ARG2);
  if (IsVarTerm(mod)) {
    return false;
  } else if (!IsAtomTerm(mod)) {
    return false;
  }
  if (IsVarTerm(G)) {
    return false;
  }
  if (IsApplTerm(G)) {
    Functor f = FunctorOfTerm(G);
    if (IsExtensionFunctor(f)) {
      return false;
    } else {
      return true;
    }
  } else if (IsPairTerm(G) || IsAtomTerm(G)) {
    return true;
  } else {
    return false;
  }
}

/** @pred is_callable( ?_Goal_ )
 *
 *  _Goal must be callable, that is, it must be bound and also must be
 *  either a compound term or an atom.
 *
 *  Throws an instantiantion error if _Goal_ is unbound.
 */
bool is_callable__(const char *file, const char *function, int lineno,
                   Term t USES_REGS) {
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, "is callable");
    return false;;
  }
  Term mod = CurrentModule;
  Term G = Yap_StripModule(Deref(ARG1), &mod);
  if (mod == 0)
    mod = TermProlog;
  // Term Context = Deref(ARG2);
  if (IsVarTerm(mod)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, mod, "error in is_callable");
    return false;
  } else if (!IsAtomTerm(mod)) {
    return false;
  }
  if (IsVarTerm(G)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, G, "is callable");
    return false;
  }
  if (IsApplTerm(G)) {
    Functor f = FunctorOfTerm(G);
    if (IsExtensionFunctor(f)) {
      return false;
    } else {
      return true;
    }
  } else if (IsPairTerm(G) || IsAtomTerm(G)) {
    return true;
  } else {
    return false;
  }
}

static Int is_callable1(USES_REGS1) {
  Term t = Deref(ARG1);
  return is_callable__(__FILE__, __FUNCTION__, __LINE__, t PASS_REGS);
}

bool Yap_callable(Term t) {
  CACHE_REGS
  Term mod = CurrentModule;
  if (mod == 0)
    mod = TermProlog;
  Term G = Yap_StripModule(Deref(t), &mod); // Term Context = Deref(ARG2);
  if (IsVarTerm(mod)) {
    return false;
  } else if (!IsAtomTerm(mod)) {
    return false;
  }
  if (IsVarTerm(G)) {
    return false;
  }
  if (IsApplTerm(G)) {
    Functor f = FunctorOfTerm(G);
    if (IsExtensionFunctor(f)) {
      return false;
    } else {
      return true;
    }
  } else if (IsPairTerm(G) || IsAtomTerm(G)) {
    return true;
  } else {
    return false;
  }
}

bool Yap_must_be_callable(Term G, Term mod)
{
  if (mod == 0)
    mod = TermProlog;
  // Term Context = Deref(ARG2);
  if (IsVarTerm(mod)) {
    Yap_ThrowError(INSTANTIATION_ERROR, mod, "must be callable");
    return false;
  } else if (!IsAtomTerm(mod)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, mod, "must be callable");
    return false;
  }
  if (IsVarTerm(G)) {
    Yap_ThrowError(INSTANTIATION_ERROR, G, "must be callable");
    return false;
  }
  if (IsApplTerm(G)) {
    Functor f = FunctorOfTerm(G);
    if (IsExtensionFunctor(f)) {
      Yap_ThrowError(TYPE_ERROR_CALLABLE, G, "must be callable");
    } else {
      return true;
    }
  } else if (IsPairTerm(G) || IsAtomTerm(G)) {
    return true;
  } else {
    Yap_ThrowError(TYPE_ERROR_CALLABLE, G, "must be callable");
    return false;
  }
  return true;
  
}


/** @pred must_be_callable( ?_Goal_ )
 *
 *  _Goal must be callable, that is, it must be bound and also must be
 *  either a compound term or an atom.
 */
static Int must_be_callable1(USES_REGS1) {
  Term mod = CurrentModule;
  Term G = Yap_StripModule(Deref(ARG1), &mod);  
return Yap_must_be_callable(G, mod);
}

/// Dereferenced term t must start as a list:
///   - `[]`
///   -  `[_|_]`
///
/// no effort is made to verify if a true list
void Yap_must_be_list(Term t) {
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "must be list");
  }
  if (t == TermNil || IsPairTerm(t)) {
    return;
  }
  Yap_ThrowError(TYPE_ERROR_LIST, t, "must be list");
}

/** @pred must_be_bound( ?_T_ )
 *
 *  _T_ must be instantiated.
 */
static Int must_be_bound1(USES_REGS1) {
  Term t = Deref(ARG1);
  // Term Context = Deref(ARG2);
  if (IsVarTerm(t))
    Yap_ThrowError(INSTANTIATION_ERROR, ARG1, "must be bound");
  return true;
}

/** @pred must_be_ground( ?_T_ )
 *
 *  _T_ must be fully instantiated.
 */
static Int must_be_ground1(USES_REGS1) {
  Term t = Deref(ARG1);
  // Term Context = Deref(ARG2);
  if (!Yap_IsGroundTerm(t))
    Yap_ThrowError(INSTANTIATION_ERROR, ARG1, "must be ground");
  return true;
}

/**
 * @pred must_be_predicate_indicator( Term, Module, Name, Arity )
 *
 *   This predicates can be used to verify if Term is a predicate indicator,
 * that is of the form:
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
static Int must_be_predicate_indicator1(USES_REGS1) {
  Term G = Deref(ARG1);
  // Term Context = Deref(ARG2);
  Term mod = CurrentModule;

  if (IsVarTerm(G)) {
    Yap_ThrowError(INSTANTIATION_ERROR, G, "must be predicate indicator");
  }
  G = Yap_YapStripModule(G, &mod);
  if (!mod)
    mod = TermProlog;
  if (IsVarTerm(G)) {
    Yap_ThrowError(INSTANTIATION_ERROR, G, "must be predicate indicator");
  }
  if (IsVarTerm(mod)) {
    Yap_ThrowError(INSTANTIATION_ERROR,mod, "must be predicate indicator");
  }
  if (!IsVarTerm(mod) && !IsAtomTerm(mod)) {
    Yap_Error(TYPE_ERROR_ATOM, G,  "must be predicate indicator");
    return false;
  }
  if (IsApplTerm(G)) {
    Functor f = FunctorOfTerm(G);
    if (IsExtensionFunctor(f)) {
      Yap_ThrowError(TYPE_ERROR_PREDICATE_INDICATOR, G,  "must be predicate indicator");
    }
    if (f == FunctorSlash || f == FunctorDoubleSlash) {
      Term name = ArgOfTerm(1, G), arity = ArgOfTerm(2, G);
      name = Yap_YapStripModule(name, &mod);
      if (!mod)
        mod = TermProlog;
      if (IsVarTerm(name)) {
        Yap_ThrowError(INSTANTIATION_ERROR, name,  "must be predicate indicator");
      } else if (!IsAtomTerm(name)) {
        Yap_ThrowError(TYPE_ERROR_ATOM, name,  "must be predicate indicator");
      }
      if (IsVarTerm(arity)) {
        Yap_ThrowError(INSTANTIATION_ERROR, arity,  "must be predicate indicator");
      } else if (!IsIntegerTerm(arity)) {
        Yap_ThrowError(TYPE_ERROR_INTEGER, arity,  "must be predicate indicator");
      } else {
        Int ar = IntegerOfTerm(arity);
        if (ar < 0) {
          Yap_ThrowError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, arity, "must be predicate indicator");
        }
        if (f == FunctorDoubleSlash) {
          arity = MkIntegerTerm(ar + 2);
        }
        return Yap_unify(mod, ARG2) && Yap_unify(name, ARG3) &&
               Yap_unify(arity, ARG4);
      }
    }
  }
  Yap_ThrowError(TYPE_ERROR_PREDICATE_INDICATOR, G,  "must be predicate indicator");
  return false;
}

void Yap_InitErrorPreds(void) {

  Yap_InitCPred("$print_exception", 1, print_exception, 0);
  Yap_InitCPred("print_exception", 1, print_exception, 0);
  Yap_InitCPred("$reset_exception", 1, reset_exception, 0);

  Yap_InitCPred("$new_exception", 1, new_exception, 0);
  Yap_InitCPred("$user_exception",7, user_exception, 0);
  Yap_InitCPred("$set_exception", 3, set_exception, 0);
  Yap_InitCPred("$read_exception", 2, read_exception, 0);
  Yap_InitCPred("$query_exception", 3, query_exception, 0);
  Yap_InitCPred("$drop_exception", 2, drop_exception, 0);
  Yap_InitCPred("$close_error", 1, close_error, HiddenPredFlag);

  /* Test predicates */
  Yap_InitCPred("callable", 1, callable, TestPredFlag);
  Yap_InitCPred("is_bound", 1, is_nonvar1, TestPredFlag);
  Yap_InitCPred("is_boolean", 1, is_boolean1, TestPredFlag);
  Yap_InitCPred("is_atom", 1, is_atom1, TestPredFlag);
  Yap_InitCPred("is_boolean", 1, is_boolean1, TestPredFlag);
  Yap_InitCPred("is_callable", 1, is_callable1, TestPredFlag);
  Yap_InitCPred("is_list", 1, is_list1, TestPredFlag);
  Yap_InitCPred("is_nonvar", 1, is_nonvar1, TestPredFlag);

  Yap_InitCPred("must_be_arity", 1, must_be_arity1, TestPredFlag);
  Yap_InitCPred("must_be_atom", 1, must_be_atom1, TestPredFlag);
  Yap_InitCPred("must_be_boolean", 1, must_be_boolean1, TestPredFlag);
  Yap_InitCPred("must_be_bound", 1, must_be_bound1, TestPredFlag);
  Yap_InitCPred("must_be_callable", 1, must_be_callable1, TestPredFlag);
  Yap_InitCPred("must_be_char", 1, must_be_char1, TestPredFlag);
  Yap_InitCPred("must_be_code", 1, must_be_code1, TestPredFlag);
  Yap_InitCPred("must_be_string", 1, must_be_string1, TestPredFlag);
  Yap_InitCPred("must_be_ground", 1, must_be_ground1, TestPredFlag);
  Yap_InitCPred("must_be_list", 1, must_be_list1, TestPredFlag);

  Yap_InitCPred("must_be_predicate_indicator", 4, must_be_predicate_indicator1,
                0);
 Yap_InitCPred("is_list", 1, is_list1, TestPredFlag);
}  

/**
@}
*/
