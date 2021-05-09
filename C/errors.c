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
   @ingroup Implementation

   @{

   ## Error Implementation through error descriptors. The process is as follows:

   - Input the error from Prolog or C-code
   - Complete the descriptor with state information
   - Export the result either as a term or as a C-structure.

   ## API

   The key routines are:

   - Yap_ThrowError(): create an error descriptor and jump to the closest C longjmp handler.

   - Yap_UserError(): try to discriminate between a throw and an system error; if an
   error doThrowError().

   Also included are exception manipulation routines and higher level
   error handlers, based on pl/error.yap. See include/YapError.h for a
   definition of data-strEuctures, and include/YapErrors.h for a list of
   implementation supported exceptions.

   ## Exception LifeCycle

   Exceptions are a dictionary that includee exception type, location, culprit,
   and user-defined. They start in the bottom stage, so that they:
   
   1. complete the dictionary;
   2. force a throw to the exception handler;
   3. close the bottom-half and move to the top-half.

   The top-half is Prolog code

*/

#include "Yap.h"
#include "YapDefs.h"
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

void Yap_RestartYap(int flag)
{
  CACHE_REGS
#if PUSH_REGS
    restore_absmi_regs(&Yap_standard_regs);
#endif
  siglongjmp(*LOCAL_RestartEnv, flag);
}

extern Term MkErrorTerm(yap_error_descriptor_t *t);

#define set_key_b(k, ks, q, i, t)		\
  if (strcmp(ks, q) == 0)			\
    {						\
      i->k = (t == TermTrue ? true : false);	\
      return i->k || t == TermFalse;		\
    }

#define set_key_i(k, ks, q, i, t)			\
  if (strcmp(ks, q) == 0)				\
    {							\
      i->k = IsIntegerTerm(t) ? IntegerOfTerm(t) : 0;	\
      return IsIntegerTerm(t);				\
    }

#define set_key_s(k, ks, q, i, t)					\
  if (strcmp(ks, q) == 0)						\
    {									\
      const char *s = IsAtomTerm(t) ? RepAtom(AtomOfTerm(t))->StrOfAE	\
	: IsStringTerm(t) ? StringOfTerm(t) : NULL;			\
      if (s && s[0])							\
	{								\
	  char *tmp = calloc(1, strlen(s) + 1);				\
	  strcpy(tmp, s);						\
	  i->k = tmp;							\
	}								\
      return i->k != NULL;						\
    }

#define set_key_t(k, ks, q, i, t)		\
  if (strcmp(ks, q) == 0)			\
    {						\
      i->k = Yap_SaveTerm(t);			\
      return i->k != 0L;			\
    }

static bool setErr(const char *q, yap_error_descriptor_t *i, Term t)
{
  set_key_i(errorNo, "errorNo", q, i, t);
  set_key_i(errorClass, "errorClass", q, i, t);
  set_key_s(errorAsText, "errorAsText", q, i, t);
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
  set_key_i(parserTextB, "parserTextB", q, i, t);
  set_key_s(parserFile, "parserFile", q, i, t);
  set_key_b(parserReadingCode, "parserReadingcode", q, i, t);
  set_key_b(prologConsulting, "prologConsulting", q, i, t);
  set_key_s(culprit, "culprit", q, i, t);

  set_key_s(prologStack, "prologStack", q, i, t);
  set_key_s(errorMsg, "errorMsg", q, i, t);
  set_key_i(errorMsgLen, "errorMsgLen", q, i, t);
  return false;
}

#define query_key_b(k, ks, q, i)		\
  if (strcmp(ks, q) == 0)			\
    {						\
      return i->k ? TermTrue : TermFalse;	\
    }

#define query_key_i(k, ks, q, i)		\
  if (strcmp(ks, q) == 0)			\
    {						\
      return MkIntegerTerm(i->k);		\
    }

#define query_key_s(k, ks, q, i)			\
  if (strcmp(ks, q) == 0)				\
    {							\
      if (i->k && i->k[0] != '\0')			\
	return MkAtomTerm(Yap_LookupAtom(i->k));	\
      else						\
	return TermEmptyAtom;				\
    }

#define query_key_t(k, ks, q, i)		\
  if (strcmp(ks, q) == 0)			\
    {						\
      Term t;					\
      if ((t = i->k) == 0)			\
	return TermNil;				\
      return t;					\
    }

static Term queryErr(const char *q, yap_error_descriptor_t *i)
{
  query_key_i(errorNo, "errorNo", q, i);
  query_key_i(errorClass, "errorClass", q, i);
  query_key_s(errorAsText, "errorAsText", q, i);
  query_key_s(classAsText, "classAsText", q, i);
  query_key_i(errorLine, "errorLine", q, i);
  query_key_s(errorFunction, "errorFunction", q, i);
  query_key_s(errorFile, "errorFile", q, i);
  query_key_i(prologPredLine, "prologPredLine", q, i);
  query_key_s(prologPredName, "prologPredName", q, i);
  query_key_i(prologPredArity, "prologPredArity", q, i);
  query_key_s(prologPredModule, "prologPredModule", q, i);
  query_key_s(prologPredFile, "prologPredFile", q, i);
  query_key_i(parserLine, "parserLine", q, i);
  query_key_i(parserFirstLine, "parserFirstLine", q, i);
  query_key_i(parserLastLine, "parserLastLine", q, i);
  query_key_s(parserTextA, "parserTextA", q, i);
  query_key_i(parserTextB, "parserTextB", q, i);
  query_key_s(parserFile, "parserFile", q, i);
  query_key_b(parserReadingCode, "parserReadingCode", q, i);
  query_key_b(prologConsulting, "prologConsulting", q, i);
  query_key_s(prologStack, "prologStack", q, i);
  query_key_s(culprit, "culprit", q, i);
  query_key_s(errorMsg, "errorMsg", q, i);
  query_key_i(errorMsgLen, "errorMsgLen", q, i);
  return TermNil;
}

static void print_key_b(FILE *of, const char *key, bool v)
{
  const char *b = v ? "true" : "false";
  fprintf(of, "%s: %s\n", key, b);
}

static void print_key_i(FILE *of, const char *key, YAP_Int v)
{
  fprintf(of, "%s: " Int_FORMAT "\n", key, v);
}

static void print_key_s(FILE *of, const char *key, const char *v)
{
  if (v && v[0])
    fprintf(of, "%s: %s\n", key, v);
}

static void print_key_t(FILE *of, const char *key, YAP_Term v)
{
  if (v)
    fprintf(of, "%s: %s\n", key, Yap_TermToBuffer(v, Quote_illegal_f | Ignore_ops_f | Handle_cyclics_f));
}

static void printErr(yap_error_descriptor_t *i, FILE *out)
{

  if (i->errorNo == YAP_NO_ERROR)
    {
      return;
    }
  print_key_s(out, "errorAsText", (i->errorAsText ? i->errorAsText : Yap_errorName(i->errorNo) ));
  print_key_s(out, "classAsText",
              (i->classAsText ?  i->classAsText: Yap_errorClassName(i->errorClass)));
  print_key_i(out, "parserPos", i->parserPos);
  print_key_i(out, "parserLine", i->parserLine);
  print_key_i(out, "parserFirstLine", i->parserFirstLine);
  print_key_i(out, "parserLastLine", i->parserLastLine);
  print_key_s(out, "parserTextA", i->parserTextA);
  print_key_i(out, "parserTextB", i->parserTextB);
  print_key_s(out, "parserFile", i->parserFile);
  print_key_i(out, "errorNo", i->errorNo);
  print_key_s(out, "errorClass", (i->classAsText ? i->classAsText: Yap_errorClassName(i->errorClass))); 
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
  print_key_s(out, "culprit", i->culprit);
  print_key_s(out, "prologStack", i->prologStack);
  print_key_t(out, "errorUserterm", i->errorUserTerm);
  if (i->errorMsgLen)
    {
      print_key_s(out, "errorMsg", i->errorMsg);
      print_key_i(out, "errorMsgLen", i->errorMsgLen);
    }
}

static YAP_Term add_key_b(const char *key, bool v, YAP_Term o0)
{
  YAP_Term tkv[2];
  tkv[1] = v ? TermTrue : TermFalse;
  tkv[0] = MkAtomTerm(Yap_LookupAtom(key));
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static YAP_Term add_key_i(const char *key, YAP_Int v, YAP_Term o0)
{
  YAP_Term tkv[2];
  tkv[1] = MkIntegerTerm(v), tkv[0] = MkAtomTerm(Yap_LookupAtom(key));
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static YAP_Term add_key_s(const char *key, const char *v, YAP_Term o0)
{
  Term tkv[2];
  if (!v)
    return o0;
  tkv[1] = MkStringTerm(v);
  tkv[0] = MkAtomTerm(Yap_LookupAtom(key));
  Term node = Yap_MkApplTerm(FunctorEq, 2, tkv);
  return MkPairTerm(node, o0);
}

static YAP_Term add_key_t(const char *key, YAP_Term v, YAP_Term o0)
{
  if (!v)
    return o0;
  return v;
}

static Term err2list(yap_error_descriptor_t *i)
{
  Term o = TermNil;
  if (i->errorNo == YAP_NO_ERROR)
    {
      return o;
    }
  o = add_key_i("errorNo", i->errorNo, o);
  o = add_key_i("errorClass", i->errorClass, o);
  o = add_key_s("errorAsText", i->errorAsText, o);
  o = add_key_s("classAsText", i->classAsText, o);
  o = add_key_i("errorLine", i->errorLine, o);
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
  o = add_key_i("parserTextB", i->parserTextB, o);
  o = add_key_s("parserFile", i->parserFile, o);
  o = add_key_b("parserReadingCode", i->parserReadingCode, o);
  o = add_key_b("prologConsulting", i->prologConsulting, o);
  o = add_key_s("culprit", i->culprit, o);
  o = add_key_s("prologStack", i->prologStack, o);
  o = add_key_t("errorUserterm", i->errorUserTerm, o);
  if (i->errorMsgLen)
    {
      o = add_key_s("errorMsg", i->errorMsg, o);
      o = add_key_i("errorMsgLen", i->errorMsgLen, o);
    }
  return o;
}

void Yap_do_warning__(const char *file, const char *function, int line,
                      yap_error_number type, Term t, ...)
{
  CACHE_REGS
    char tmpbuf[PATH_MAX];
  va_list ap;
  PredEntry *p;
  Term ts[2];
  const char *fmt;
  return;
  yap_error_descriptor_t *e = calloc(1, sizeof(yap_error_descriptor_t));
  Yap_MkErrorRecord(e, file, function, line, type, t, "discontiguous warning");
  p = RepPredProp(PredPropByFunc(FunctorPrintMessage,
                                 PROLOG_MODULE)); // PROCEDURE_print_message2
  if (p->ArityOfPE)
    {
      //sc[0] = t;
      //    sc[1] = MkSysError(e);
      va_start(ap, t);
      fmt = va_arg(ap, char *);
      if (fmt != NULL)
	{
#if HAVE_VSNPRINTF
	  vsnprintf(tmpbuf, PATH_MAX - 1, fmt, ap);
#else
	  (void)vsprintf(tmpbuf, fmt, ap);
#endif
	  va_end(ap);
	  ts[1] = MkAtomTerm(AtomWarning);
	  ts[0] = MkAtomTerm(Yap_LookupAtom(tmpbuf));
	  Yap_execute_pred(p, ts, true PASS_REGS);
	  LOCAL_PrologMode &= ~InErrorMode;
	}
    }
}

bool Yap_Warning(const char *s, ...)
{
  CACHE_REGS
    va_list ap;
  PredEntry *pred;
  bool rc;
  Term ts[2];
  const char *fmt;
  char tmpbuf[PATH_MAX];
  yap_error_number err;

  if (LOCAL_DoingUndefp)
    return false;
  LOCAL_DoingUndefp = true;

  if (LOCAL_PrologMode & InErrorMode && (err = LOCAL_ActiveError->errorNo))
    {
      fprintf(stderr, "%% Warning %s WITHIN ERROR %s %s\n", s,
	      LOCAL_ActiveError->classAsText,
	      LOCAL_ActiveError->errorAsText);
	      return false;

    }
  LOCAL_PrologMode |= InErrorMode;
  pred = RepPredProp(PredPropByFunc(FunctorPrintMessage,
                                    PROLOG_MODULE)); // PROCEDURE_print_message2
  va_start(ap, s);
  fmt = va_arg(ap, char *);
  if (fmt != NULL)
    {
#if HAVE_VSNPRINTF
      vsnprintf(tmpbuf, PATH_MAX - 1, fmt, ap);
#else
      (void)vsprintf(tmpbuf, fmt, ap);
#endif
    }
  else
    {
      LOCAL_DoingUndefp = false;
      LOCAL_PrologMode &= ~InErrorMode;
      return false;
    }
  va_end(ap);
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE)
    {
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
                     yap_error_number e, Term t, ...)
{
  CACHE_REGS
    va_list ap;
  va_start(ap, t);
  const char *fmt;
  char *tmpbuf = NULL;

  fmt = va_arg(ap, char *);
  if (fmt != NULL)
    {
      tmpbuf = malloc(PATH_MAX);
#if HAVE_VSNPRINTF
      vsnprintf(tmpbuf, PATH_MAX - 1, fmt, ap);
#else
      (void)vsprintf(tmpbuf, fmt, ap);
#endif
    }
  else
    return;
  va_end(ap);
  if (LOCAL_ActiveError->errorNo != YAP_NO_ERROR)
    {
      yap_error_number err = LOCAL_ActiveError->errorNo;
      fprintf(stderr, "%% Warning %s WITHIN ERRORR\n", Yap_errorName(err));
      return;
    }
  LOCAL_ActiveError->errorNo = e;
  LOCAL_ActiveError->errorFile = NULL;
  LOCAL_ActiveError->errorFunction = NULL;
  LOCAL_ActiveError->errorLine = 0;
  if (fmt && tmpbuf)
    {
      LOCAL_Error_Size = strlen(tmpbuf);
      LOCAL_ActiveError->errorMsg = malloc(LOCAL_Error_Size + 1);
      strcpy((char *)LOCAL_ActiveError->errorMsg, tmpbuf);
    }
  else
    {
      LOCAL_Error_Size = 0;
    }
  ARG1 = TermNil;
}

bool Yap_PrintWarning(Term twarning)
{
  CACHE_REGS
    if (LOCAL_DoingUndefp)
      {
	P = FAILCODE;
	return false;
      }

  PredEntry *pred = RepPredProp(PredPropByFunc(
					       FunctorPrintMessage, PROLOG_MODULE)); // PROCEDURE_print_message2;
  if (twarning)
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid ", " warning(%s)",
                        Yap_TermToBuffer(twarning, Quote_illegal_f |
					 Ignore_ops_f |
					 Handle_cyclics_f));

  bool rc;
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE)
    {
      fprintf(stderr, "%s:%ld/* d:%d warning */:\n", LOCAL_ActiveError->errorFile,
	      LOCAL_ActiveError->errorLine, 0);
    }
  ARG2 = twarning;
  ARG1 = MkAtomTerm(AtomWarning);
  LOCAL_PrologMode &= ~InErrorMode;
  rc = Yap_execute_pred(pred, NULL, true PASS_REGS);
  LOCAL_within_print_message = false;
  return rc;
}

bool Yap_HandleError__(const char *file, const char *function, int lineno,
                       const char *s, ...)
{
  CACHE_REGS
    yap_error_number err = LOCAL_Error_TYPE;
  const char *serr;

  if (LOCAL_ErrorMessage)
    {
      serr = LOCAL_ErrorMessage;
    }
  else
    {
      serr = s;
    }
  switch (err)
    {
    case RESOURCE_ERROR_STACK:
      if (!Yap_dogc(PASS_REGS1))
	{
	  Yap_Error__(false, file, function, lineno, RESOURCE_ERROR_STACK, ARG1,
		      serr);
	  return false;
	}
      LOCAL_PrologMode = UserMode;
      return true;
    case RESOURCE_ERROR_AUXILIARY_STACK:
      if (LOCAL_MAX_SIZE < (char *)AuxSp - AuxBase)
	{
	  LOCAL_MAX_SIZE += 1024;
	}
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE))
	{
	  /* crash in flames */
	  Yap_Error__(false, file, function, lineno, RESOURCE_ERROR_AUXILIARY_STACK,
		      ARG1, serr);
	  return false;
	}
      LOCAL_PrologMode = UserMode;
      return true;
    case RESOURCE_ERROR_HEAP:
      if (!Yap_growheap(FALSE, 0, NULL))
	{
	  Yap_Error__(false, file, function, lineno, RESOURCE_ERROR_HEAP, ARG2,
		      serr);
	  return false;
	}
    default:

      if (LOCAL_PrologMode == UserMode)
	{
	  Yap_ThrowError__(file, function, lineno, err, 0, NULL);
	}
      else
	LOCAL_PrologMode &= ~InErrorMode;
      return false;
    }
}

int Yap_SWIHandleError(const char *s, ...)
{
  CACHE_REGS
    yap_error_number err = LOCAL_Error_TYPE;
  char *serr;

  if (s)
    {
      serr = (char *)s;
    }
  switch (err)
    {
    case RESOURCE_ERROR_STACK:
      if (!Yap_dogc(PASS_REGS1))
	{
	  Yap_Error(RESOURCE_ERROR_STACK, TermNil, serr);
	  return (FALSE);
	}
      return TRUE;
    case RESOURCE_ERROR_AUXILIARY_STACK:
      if (LOCAL_MAX_SIZE < (char *)AuxSp - AuxBase)
	{
	  LOCAL_MAX_SIZE += 1024;
	}
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE))
	{
	  /* crash in flames */
	  Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK, ARG1, serr);
	  return FALSE;
	}
      return true;
    case RESOURCE_ERROR_HEAP:
      if (!Yap_growheap(false, 0, NULL))
	{
	  Yap_Error(RESOURCE_ERROR_HEAP, ARG2, serr);
	  return false;
	}
    default:
      Yap_Error(err, TermNil, serr);
      return false;
    }
}

static void error_exit_yap(int value)
{
  CACHE_REGS
    if (!(LOCAL_PrologMode & BootMode))
      {

#if DEBUG
#endif
      }
  fprintf(stderr, "\n   Exiting ....\n");
#if HAVE_BACKTRACE
  void *callstack = malloc(64 * K);
  int i;
  int frames = backtrace(callstack, 64 * K - 1);
  char **strs = backtrace_symbols(callstack, frames);
  fprintf(stderr, "%% C-Execution stack:\n");
  for (i = 0; i < frames; ++i)
    {
      fprintf(stderr, "%%       %s\n", strs[i]);
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

#define BEGIN_ERROR_CLASSES()				\
  static Atom mkerrorct(yap_error_class_number c)	\
  {							\
  switch (c)						\
    {

#define ECLASS(CL, A, B)			\
  case CL:					\
  return Yap_LookupAtom(A);

#define END_ERROR_CLASSES()			\
  }						\
  return NULL;					\
  }

#define BEGIN_ERRORS()							\
  static Term mkerrort(yap_error_number e, Term culprit, Term info)	\
  {									\
  if (!e)      e= USER_DEFINED_ERROR;					\
  if (!info) info = TermNil;						\
  switch (e)								\
    {

#define E0(A, B, C)					\
  case A:						\
  {							\
    Term ft[2];						\
    ft[0] = MkAtomTerm(Yap_LookupAtom(C "_error"));	\
    ft[1] = info;					\
    return Yap_MkApplTerm(FunctorError, 2, ft);		\
  }

#define E(A, B, C)							\
  case A:								\
  {									\
    Term ft[2], nt[2];							\
    nt[0] = MkAtomTerm(Yap_LookupAtom(C));				\
    nt[1] = MkVarTerm();						\
    if (culprit) Yap_unify(nt[1], culprit);				\
    ft[0] = Yap_MkApplTerm(Yap_MkFunctor(mkerrorct(B), 2), 2, nt);	\
    ft[1] = info;							\
    return Yap_MkApplTerm(FunctorError, 2, ft);				\
  }

#define E1(A, B, C)                                                     \
  case A:                                                               \
  {                                                                     \
    Term ft[2], nt[1];                                                  \
    nt[0] = MkVarTerm();                                                \
    if (culprit) Yap_unify(nt[0], culprit);				\
    ft[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(C), 1), 1, nt); \
    ft[1] = info;                                                       \
    return Yap_MkApplTerm(FunctorError, 2, ft);                         \
  }

#define E2(A, B, C, D)							\
  case A:								\
  {									\
    Term ft[2], nt[3];							\
    nt[0] = MkAtomTerm(Yap_LookupAtom(C));				\
    nt[1] = MkAtomTerm(Yap_LookupAtom(D));				\
    nt[2] = MkVarTerm();						\
    if (culprit) Yap_unify(nt[2], culprit);				\
    ft[0] = Yap_MkApplTerm(Yap_MkFunctor(mkerrorct(B), 3), 3, nt);	\
    ft[1] = info;							\
    return Yap_MkApplTerm(FunctorError, 2, ft);				\
  }

#define END_ERRORS()				\
  }						\
  return TermNil;				\
  }

#include "YapErrors.h"

/// add a new error descriptor, either to the top of the  stack,
/// or as the top;
yap_error_descriptor_t *Yap_pushErrorContext(bool link, yap_error_descriptor_t *new_error, yap_error_descriptor_t *old_error)
{

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
yap_error_descriptor_t *Yap_popErrorContext(bool mdnew, bool pass, yap_error_descriptor_t *old_error)
{
  yap_error_descriptor_t *e = LOCAL_ActiveError,
    *ep = LOCAL_ActiveError->top_error;
  // last block
  LOCAL_ActiveError = ep;
  if (e->errorNo && !ep->errorNo && pass)
    {
      yap_error_descriptor_t *epp = ep->top_error;
      memmove(ep, e, sizeof(*e));
      ep->top_error = epp;
    }
  return old_error;
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
                      yap_error_number type, Term where, ...)
{
  va_list ap;
  char tmpbuf[PATH_MAX];

  if ( LOCAL_ActiveError->errorNo)
    {

      fprintf(stderr, "%s:%d:0 %s() caused a %s while processing error or warning!!!!!\n\n", file, lineno, function, Yap_errorName(type));
    }
  else
    {
      va_start(ap, where);      char *fmt = va_arg(ap, char *);
      if (fmt != NULL)
	{
#if HAVE_VSNPRINTF
	  (void)vsnprintf(tmpbuf, PATH_MAX - 1, fmt, ap);
#else
	  (void)vsprintf(tmpbuf, fmt, ap);
#endif

	}
      else
	{
	  tmpbuf[0] = '\0';
	  
	}
	  Yap_Error__(true, file, function, lineno, type, where, tmpbuf);
    }

  Yap_ThrowExistingError();

}

// complete delayed error.

void Yap_ThrowExistingError(void)
{
  if (LOCAL_RestartEnv || IsNonVarTerm(Yap_GetGlobal(AtomZip)))
    {
      P = FAILCODE;
      Yap_RestartYap(5);
    }
  Yap_exit(5);
}

/// Wrap the error descriptor as exception/2

Term MkSysError(yap_error_descriptor_t *i)
{
  Term et = MkAddressTerm(i);
  return Yap_MkApplTerm(FunctorException, 1, &et);
}


/** convert  a C-error to a Prolog term:
 *
 - fill text fields
 - wrap descriptor
 */
Term Yap_MkFullError(yap_error_descriptor_t *i)
{
  if (i == NULL)
    i = (Yap_local.ActiveError);

  memset(i,0, sizeof(*Yap_local.ActiveError));
  if (i->errorNo != USER_DEFINED_ERROR) {
    i->errorAsText = Yap_errorName(i->errorNo);
    i->errorClass = Yap_errorClass(i->errorNo);
    i->classAsText = Yap_errorClassName(i->errorClass);
  }	
  Term culprit = TermNil;
  
  if (i->culprit)
      culprit = Yap_BufferToTerm(i->culprit, TermNil);
    else if (i->errorMsg)
      culprit = MkStringTerm(i->errorMsg);

return    mkerrort(i->errorNo, culprit, MkSysError(i));
   
}

/**
complete an error descriptor:
 - pc
 - culprit
*/
bool Yap_MkErrorRecord(yap_error_descriptor_t *r, const char *file,
                       const char *function, int lineno, yap_error_number type,
                       Term where, const char *s)
{
  if (!Yap_pc_add_location(r, P, B, ENV))
    Yap_env_add_location(r, CP, B, ENV, 0);
  if (where == 0L || where == TermNil)
    {
      r->culprit = NULL;
    }
  else
    {
      if (where)
	{
	  r->culprit = Yap_TermToBuffer(where, Quote_illegal_f | Handle_vars_f | Handle_cyclics_f);
	}
    }
  if (type != SYNTAX_ERROR && LOCAL_consult_level > 0)
    {
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
  r->prologConsulting = Yap_ConsultingFile();
  LOCAL_PrologMode |= InErrorMode;
  Yap_ClearExs();
  // first, obtain current location

#if 0
  sprintf(LOCAL_FileNameBuf, "%s:%d in C-function %s ", file, lineno,
  function);
   tf = MkAtomTerm(Yap_LookupAtom(LOCAL_FileNameBuf));
#if DEBUG_STRICT
  if (Yap_heap_regs && !(LOCAL_PrologMode & BootMode))
    fprintf(stderr, "***** Processing Error %d (%lx,%x) %s***\n", type,
            (unsigned long int)LOCAL_Signals, LOCAL_PrologMode, fmt);
  else
    fprintf(stderr, "***** Processing Error %d (%x) %s***\n", type,
            LOCAL_PrologMode, fmt);
#endif
#endif
  if (r->errorNo == SYNTAX_ERROR)
    {
      r->errorClass = SYNTAX_ERROR_CLASS;
    }
  else if (r->errorNo == SYNTAX_ERROR_NUMBER)
    {
      r->errorClass = SYNTAX_ERROR_CLASS;
      r->errorNo = SYNTAX_ERROR;
    }
  if (type == INTERRUPT_EVENT)
    {
      fprintf(stderr, "%% YAP exiting: cannot handle signal %d\n",
	      (int)IntOfTerm(where));
      Yap_exit(1);
    }
  if (s && s[0])
    {
      char *ns;
      r->errorMsgLen = strlen(s) + 1;
      ns = malloc(r->errorMsgLen);
      strcpy(ns, s);
      r->errorMsg = ns;
    }
  else
    {
      r->errorMsgLen = 0;
      r->errorMsg = 0;
    }
  return true;
}
/**
@brief Yap_Error
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

In a bad day, it has to deal with OOM, abort, and errors within errorts.
*/
yamop *Yap_Error__(bool throw, const char *file, const char *function,
                   int lineno, yap_error_number type, Term where, ...)
{
  CACHE_REGS
    va_list ap;
  char *fmt;
  char *s = NULL;

  switch (type)
    {
    case SYSTEM_ERROR_INTERNAL:
      {
	fprintf(stderr, "%% Internal YAP Error: %s exiting....\n", tmpbuf);
	if (LOCAL_PrologMode & BootMode)
	  {
	    fprintf(stderr, "%% YAP crashed while booting %s\n", tmpbuf);
	  }
	else
	  {
	    if (tmpbuf[0])
	      {
		fprintf(stderr, "%% Bug found while executing %s\n", tmpbuf);
	      }
#if HAVE_BACKTRACE
	    void *callstack[256];
	    int i;
	    int frames = backtrace(callstack, 256);
                                                            	    char **strs = backtrace_symbols(callstack, frames);
	    fprintf(stderr, "Execution stack:\n");
	    for (i = 0; i < frames; ++i)
	      {
		fprintf(stderr, "       %s\n", strs[i]);
	      }
	    free(strs);
#endif
	  }
	error_exit_yap(1);
      }
    case SYSTEM_ERROR_FATAL:
      {
	fprintf(stderr, "%% Fatal YAP Error: %s exiting....\n", tmpbuf);
	error_exit_yap(1);
      }
    case INTERRUPT_EVENT:
      {
	error_exit_yap(1);
      }
    case USER_DEFINED_EVENT:
    case THROW_EVENT:
      {
	LOCAL_ActiveError->errorUserTerm = TermNil;
      }
      break;
    case ABORT_EVENT:
      {
	//	fun = FunctorDollarVar;
	//  serious = true;
      LOCAL_ActiveError->errorNo = ABORT_EVENT;
      P = FAILCODE;
      LOCAL_PrologMode &= ~InErrorMode;
      }
      break;
    case CALL_COUNTER_UNDERFLOW_EVENT:
      // Do a long jump
      LOCAL_ReductionsCounterOn = FALSE;
      LOCAL_PredEntriesCounterOn = FALSE;
      LOCAL_RetriesCounterOn = FALSE;
      LOCAL_ActiveError->errorNo = CALL_COUNTER_UNDERFLOW_EVENT;
      Yap_JumpToEnv(TermNil);
      P = FAILCODE;
      LOCAL_PrologMode &= ~InErrorMode;
      return P;
    case PRED_ENTRY_COUNTER_UNDERFLOW_EVENT:
      // Do a long jump
      LOCAL_ReductionsCounterOn = FALSE;
      LOCAL_PredEntriesCounterOn = FALSE;
      LOCAL_RetriesCounterOn = FALSE;
      LOCAL_ActiveError->errorNo = PRED_ENTRY_COUNTER_UNDERFLOW_EVENT;
      Yap_JumpToEnv(TermNil);
      P = FAILCODE;
      LOCAL_PrologMode &= ~InErrorMode;
      return P;
    case RETRY_COUNTER_UNDERFLOW_EVENT:
      // Do a long jump
      LOCAL_ReductionsCounterOn = FALSE;
      LOCAL_PredEntriesCounterOn = FALSE;
      LOCAL_RetriesCounterOn = FALSE;
      LOCAL_ActiveError->errorNo = RETRY_COUNTER_UNDERFLOW_EVENT;
      Yap_JumpToEnv(TermNil);
      P = FAILCODE;
      LOCAL_PrologMode &= ~InErrorMode;
      return P;
    default:
      va_start(ap, where);
      fmt = va_arg(ap, char *);
      if (fmt != NULL)
	{
	  s = malloc(PATH_MAX);
#if HAVE_VSNPRINTF
	  (void)vsnprintf(s, PATH_MAX - 1, fmt, ap);
#else
	  (void)vsprintf(s, fmt, ap);
#endif
	  va_end(ap);
	  break;
	}
    }
  Yap_MkErrorRecord(LOCAL_ActiveError, file, function, lineno, type, where, s);
  if (where == 0 || where == TermNil)
    {
      LOCAL_ActiveError->culprit = NULL;
    }
  if (P == (yamop *)(FAILCODE))
    {
      LOCAL_PrologMode &= ~InErrorMode;
      return P;
    }
  // PURE_ABORT may not have set where correctly, BootMode may not have the data
  // terms ready
  if (type == ABORT_EVENT || LOCAL_PrologMode & BootMode)
    {
      LOCAL_PrologMode &= ~AbortMode;
      LOCAL_PrologMode &= ~InErrorMode;
      // make sure failure will be seen at next port
      // no need to lock & unlock
      if (LOCAL_PrologMode & AsyncIntMode)
	Yap_signal(YAP_FAIL_SIGNAL);
      P = FAILCODE;
    }
  else
    {
      // Exit Abort Mode, if we were there
      LOCAL_PrologMode &= ~AbortMode;
      LOCAL_PrologMode |= InErrorMode;
    }

#ifdef DEBUG
  //DumpActiveGoals( USES_REGS1 );
#endif // DEBUG
#if 0
 if (LOCAL_ActiveError->errorNo != SYNTAX_ERROR &&
     trueLocalPrologFlag(STACK_DUMP_ON_ERROR_FLAG)	 )
      LOCAL_ActiveError->prologStack = Yap_dump_stack();
  #else
 LOCAL_ActiveError->prologStack = NULL;  
#endif
CalculateStackGap(PASS_REGS1);
#if DEBUG
  //DumpActiveGoals( PASS_REGS1 );
#endif
     //wait if we we are in user code,
     // it's up to her to decide
  if (LOCAL_delay)
    return P;
  if (LOCAL_DoingUndefp)
    {
      LOCAL_DoingUndefp = false;
      LOCAL_Signals = 0;
      //      yap_error_descriptor_t *co = CopyException(LOCAL_ActiveError);
      Yap_PrintWarning(Yap_MkFullError(NULL));
      return P;
    }
  if (!LOCAL_ActiveError) {
   LOCAL_ActiveError = Yap_GetException();
  }
  Yap_JumpToEnv(where);
//  reset_error_description();
      pop_text_stack(LOCAL_MallocDepth + 1);
  if (throw)
    {
      LOCAL_Error_TYPE = type;
      Yap_RaiseException();
    } else {
  LOCAL_Error_TYPE = YAP_NO_ERROR;



    
  LOCAL_PrologMode = UserMode;
  }
  return P;
}

static Int close_error(USES_REGS1)
{
  if (!LOCAL_CommittedError)
    return true;
  LOCAL_CommittedError->errorNo = YAP_NO_ERROR;
  memset(LOCAL_CommittedError, 0, sizeof(yap_error_descriptor_t));
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

#define BEGIN_ERROR_CLASSES() typedef enum aux_class	\
    {

#define ECLASS(CL, A, B) CL##__,

#define END_ERROR_CLASSES()			\
  }						\
  aux_class_t;

#define BEGIN_ERRORS()
#define E0(X, Y, Z)
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

#define END_ERROR_CLASSES()			\
  NULL						\
  }

typedef struct c_error_info
{
  int errnb;
  int class;
  const char *name;
} c_error_t;

#define BEGIN_ERRORS() static struct c_error_info c_error_list[] = {
#define E0(X, Y, Z) {X, Y##__, Z},
#define E(X, Y, Z) {X, Y##__, Z},
#define E1(X, Y, Z) {X, Y##__, Z},
#define E2(X, Y, Z, W) {X, Y##__, Z " " W},
#define END_ERRORS()				\
  {						\
    0, YAPC_NO_ERROR, ""			\
      }						\
  }						\
  ;

#include <YapErrors.h>
#include <iopreds.h>

yap_error_class_number Yap_errorClass(yap_error_number e)
{
  return c_error_list[e].class;
}

yap_error_class_number Yap_errorClassNumber(const char *s)
{
  Int i = 1;
  while (c_error_class_name[i] &&
         strcmp(c_error_class_name[i], s) != 0)
    {
      i++;
    }
  if (!c_error_class_name[i])
    return USER_DEFINED_ERROR_CLASS;
  return i;
}
char *Yap_errorClassName(yap_error_class_number e)
{
  return (char *)c_error_class_name[e];
}

static Int reset_exception(USES_REGS1) { return Yap_ResetException(NULL); }

Term MkErrorTerm(yap_error_descriptor_t *t)
{
    if (t==NULL) {
        t = LOCAL_ActiveError;
    }
  if (t->errorUserTerm)
    return t->errorUserTerm;
  if (t->errorNo == THROW_EVENT || t->errorNo == ERROR_EVENT)
    {
      if (t->errorMsg)
	return Yap_BufferToTerm(t->errorMsg, TermNil);
    }

  Term tc = t->culprit ? Yap_BufferToTerm(t->culprit, TermNil) : 0;
  if (IsVarTerm(tc)) tc=TermNil;
  Term o;
  if (t->errorNo == USER_DEFINED_ERROR) {
    Term ft[2];
    ft[0] = Yap_BufferToTerm(t->culprit, TermNil);
    ft[1] = err2list(t);						
    o =  Yap_MkApplTerm(FunctorError, 2, ft);
  } else {
    o = mkerrort(t->errorNo, tc, err2list(t));
  }
  return  o;
}


/// Convert an user term into a Prolog error term.

static yap_error_descriptor_t *mkUserError(Term t, Term *tp, yap_error_descriptor_t *i)
{
  if (i == NULL)
    i = LOCAL_ActiveError;
  memset(i,0, sizeof(*Yap_local.ActiveError));
  i->errorUserTerm = Yap_SaveTerm(t);
  i->errorNo = THROW_EVENT;
  /* just allow the easy way out, if needed */
  if (IsApplTerm(t) && FunctorOfTerm(t) == FunctorError){
    i->errorNo = USER_DEFINED_ERROR;;
    i->errorClass = USER_DEFINED_ERROR_CLASS;
    Term t1 = ArgOfTerm(1,t);
    i->errorMsg = Yap_TermToBuffer(ArgOfTerm(2,t),0);
    if (IsAtomTerm(t1)) {
      i->classAsText=i->errorAsText=RepAtom(AtomOfTerm(t1))->StrOfAE;
      i->errorClass = Yap_errorClassNumber( i->classAsText);
      if (i->errorClass == INSTANTIATION_ERROR_CLASS)
	i->errorNo = INSTANTIATION_ERROR;
      return i;
    }
    
    if (IsApplTerm(t1)) {
      Functor f = FunctorOfTerm(t1);
      arity_t a = ArityOfFunctor(f);
      i->classAsText=RepAtom(NameOfFunctor(f))->StrOfAE;
      i->errorAsText=RepAtom(AtomOfTerm(ArgOfTerm(1,t1)))->StrOfAE;
	i->errorClass = Yap_errorClassNumber( i->classAsText);
	if (i->errorClass != USER_DEFINED_ERROR_CLASS) {
      if (a<3) {
	i->errorNo = Yap_errorNumber(i->errorClass, i->errorAsText);
      } else if (a==3) {
	const char *fname = RepAtom(AtomOfTerm(ArgOfTerm(2,t1)))->StrOfAE;
	char *buf = malloc(strlen(i->classAsText)+strlen(fname)+2);
	strcpy(buf,i->classAsText);
	strcat(buf," ");
	strcat(buf,fname);
	i->errorNo = Yap_errorNumber(i->errorClass, i->errorAsText);
      }
	}

    if ( i->errorNo != USER_DEFINED_ERROR) {
	if (a>1)
	  i->culprit = Yap_TermToBuffer(ArgOfTerm(a,t1),0);
	return i;
      }
    }
   i->culprit = Yap_TermToBuffer(t1,0);
  }
  return i;
}

Term Yap_UserError(Term t, yap_error_descriptor_t * i)
{

 Term tc;

  if (i == NULL)
    i = LOCAL_ActiveError;
  if (!IsApplTerm(t) || FunctorOfTerm(t) != FunctorError)
    return t;
  if (!Yap_pc_add_location(i, P, B, ENV))
    Yap_env_add_location(i, CP, B, ENV, 0);
  i = mkUserError(t, &tc, i);
  if ( LOCAL_consult_level > 0)
    {
      i->parserFile = Yap_ConsultingFile(PASS_REGS1)->StrOfAE;
i->culprit = 0;
    i->errorMsg = 0;
  }

    if (i->errorNo ==  USER_DEFINED_ERROR) {
      Term ts[2];
    ts[0] = Yap_MkApplTerm(FunctorUserDefinedError,1,&t);
    ts[1] = MkSysError(i);
    return Yap_MkApplTerm(FunctorError,2,ts);
  }
    Term t2=ArgOfTerm(2,t);
  if (IsVarTerm(t2) || t2 == TermNil) {
        Term ts[2];
	ts[0] = ArgOfTerm(1,t);
	ts[1] = MkSysError(i);
	return Yap_MkApplTerm(FunctorError,2,ts);
  }
  return t;
}


/** @}

    @brief C-ErrorHandler Low-level error handling..

    @defgroup  ExceptionDescriptors Exception Descriptor Manipulation
    @ingroup C-ErrorHandler

    @ brief Manipulate error/throw descriptors

    @{

    These routines do useful stuff on error descriptors.

    Notice that if
    the argument is an error descriptor, and you pass NULL, they always
    expect it to refer the current Active error descriptor.


*/

/**
 * is an error active?
 */
yap_error_descriptor_t *Yap_PeekException(void)
{
  CACHE_REGS
    if (LOCAL_ActiveError->errorNo)
      return LOCAL_ActiveError;
    else
      return NULL;
}

/**
 * clone Active Error
 */
yap_error_descriptor_t *Yap_GetException(void)
{
  CACHE_REGS
    yap_error_descriptor_t *t = LOCAL_ActiveError,
    *nt = calloc(1, sizeof(yap_error_descriptor_t));
  memmove(nt, t, sizeof(yap_error_descriptor_t));
  return nt;
}

/**
 * print descriptor to user_output/stdout
 */
void Yap_PrintException(yap_error_descriptor_t * i)
{
  FILE *of = GLOBAL_Stream[LOCAL_c_output_stream].file ? GLOBAL_Stream[LOCAL_c_output_stream].file : stderr;
  printErr(LOCAL_ActiveError, of);
}

/** 
 * let's go.
 */
bool Yap_RaiseException(void)
{
  if (LOCAL_ActiveError->errorNo) {
    P=FAILCODE;
    Yap_JumpToEnv(TermNil);
    // DsBTerm *dbt = Yap_RefToException();
    return true;
  }
  return false;
}

/**
 * clean up (notice that the code ensures  ActiveError exists on exit.
 */
bool Yap_ResetException(yap_error_descriptor_t * i)
{
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
bool Yap_RestartException(yap_error_descriptor_t *  i)
{
  // reset error descriptor
  memcpy(LOCAL_ActiveError, i, sizeof(yap_error_descriptor_t));
  LOCAL_PrologMode |=  InErrorMode;
  return true;
}

/** C-predicates that export the interface */

static Int read_exception(USES_REGS1)
{
  yap_error_descriptor_t *t = AddressOfTerm(Deref(ARG1));
  Term rc = err2list(t);
  //      Yap_DebugPlWriteln(rc);
  return Yap_unify(ARG2, rc);
}

static Int print_exception(USES_REGS1)
{
  Term t1 = Deref(ARG1);
  if (IsAddressTerm(t1))
    {
      FILE *of = GLOBAL_Stream[LOCAL_c_error_stream].file ? GLOBAL_Stream[LOCAL_c_error_stream].file : stderr;
      yap_error_descriptor_t *t = AddressOfTerm(t1);
      if (t->parserFile && t->parserLine)
	{
	  fprintf(of, "\n%s:%ld:0 error: while parsing %s\n\n", t->parserFile,
		  t->parserLine, t->errorAsText);
	}
      else if (t->prologPredFile && t->prologPredLine)
	{
	  fprintf(of, "\n%s:%ld:0 error: while running %s\n\n",
		  t->prologPredFile, t->prologPredLine, t->errorAsText);
	}
      else if (t->errorFile && t->errorLine)
	{
	  fprintf(of, "\n%s:%ld:0 error: while executing %s\n\n", t->errorFile,
		  t->errorLine, t->errorAsText);
	}
      printErr(t, of);
    }
  else
    {
      return Yap_WriteTerm(LOCAL_c_error_stream, t1, TermNil PASS_REGS);
    }
  //      Yap_DebugPlWriteln(rc);
  return true;
}

static Int query_exception(USES_REGS1)
{
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

static Int set_exception(USES_REGS1)
{
  const char *query = NULL;
  Term t;

  if (IsAtomTerm((t = Deref(ARG1))))
    query = RepAtom(AtomOfTerm(t))->StrOfAE;
  if (IsStringTerm(t))
    query = StringOfTerm(t);
  if (!IsAddressTerm(Deref(ARG1)))
    return false;
  yap_error_descriptor_t *y = AddressOfTerm(Deref(ARG2));
  Term t3 = Deref(ARG3);
  if (IsVarTerm(t3))
    {
      return false;
    }
  else
    {
      return setErr(query, y, t3);
    }
}

static Int
drop_exception(USES_REGS1)
{
  Term tn;
  bool rc=false;
  if (LOCAL_Error_TYPE) {
    tn = MkErrorTerm(LOCAL_ActiveError);
    if (LOCAL_ActiveError->errorNo == USER_DEFINED_EVENT) {
      rc = 
	Yap_unify(LOCAL_ActiveError->errorUserTerm, ARG1);
    } else {
      if (LOCAL_ActiveError->errorNo == USER_DEFINED_ERROR &&
	  LOCAL_ActiveError->errorUserTerm) {
	rc = Yap_unify(LOCAL_ActiveError->errorUserTerm, ARG1);
      } else {
	rc = Yap_unify(tn, ARG1);
      }
    }
  }
  LOCAL_PrologMode &= ~InErrorMode;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  LOCAL_ActiveError->errorUserTerm = 0;
   return rc;
}

static Int new_exception(USES_REGS1)
{
  Term t =MkSysError(calloc(1, sizeof(yap_error_descriptor_t)));
  return Yap_unify(ARG1, t);
}

bool Yap_get_exception(USES_REGS1)
{
  Term tn = Yap_GetGlobal(AtomZip);
  if (!IsVarTerm(tn) && tn != TermNil)
    return true;
  return false;
}

/** given a string(s, lookup for a corresponding error class
    r   numbe */
/** given a string(s) and class context, lookup for a corresponding error
    numbe */
yap_error_number Yap_errorNumber(yap_error_class_number c, const char *s)
{
  Int i = 1;

  while (c_error_list[i].name)
    {
      if (c_error_list[i].class == USER_DEFINED_ERROR_CLASS)
        // we will never find it, but we found it!
        return USER_DEFINED_ERROR;
      if (c_error_list[i].class != c)
	{
	  i++;
	  continue;
	}
      // same class
      if (strcmp(c_error_list[i].name, s) == 0)
        // found it!
        break;
      i++;
    }
  return i;
}

char *Yap_errorName(yap_error_number e) {if (e != USER_DEFINED_ERROR)
    return (char*)c_error_list[e].name;
  return NULL;}

yap_error_descriptor_t *event(Term t, yap_error_descriptor_t * i)
{
  i->errorNo = ERROR_EVENT;
  i->errorClass = EVENT;

  return i;
}


Int is_nonvar__(const char *file, const char *function, int lineno, Term t USES_REGS)
{
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t))
    {
      Yap_ThrowError(INSTANTIATION_ERROR, t, NULL);
      return false;
    }
  return t == TermTrue || t == TermFalse;
}

/**
 * @pred is_nonvar(T)
 *
 * True if the term _T_ has been instantiated, otherwise causs an exception. The same as
` * must_be_bound/1.
 */
static Int is_nonvar1(USES_REGS1)
{
    Term t = Deref(ARG1);
    return is_nonvar__(__FILE__,__FUNCTION__,__LINE__,t PASS_REGS);
}

bool is_boolean__(const char *file, const char *function, int lineno, Term t USES_REGS)
{
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t))
    {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, NULL);
      return false;
    }
  return t == TermTrue || t == TermFalse;
}

static Int is_boolean1(USES_REGS1)
{
    Term t = Deref(ARG1);
    return is_boolean__(__FILE__,__FUNCTION__,__LINE__,t PASS_REGS);
}

bool must_be_boolean__(const char *file, const char *function, int lineno, Term t USES_REGS)
{
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t))
    {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, t, NULL);
      return false;
    }
  if ( t == TermTrue || t == TermFalse )
    return true;
  else     {
      Yap_ThrowError__(file, function, lineno, TYPE_ERROR_BOOLEAN, t, NULL);
      return false;
    }
  
}

static Int must_be_boolean1(USES_REGS1)
{
    Term t = Deref(ARG1);
    return must_be_boolean__(__FILE__,__FUNCTION__,__LINE__,t PASS_REGS);
}


bool is_atom__(const char *file, const char *function, int lineno, Term t USES_REGS)
{
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t))
    {
      Yap_ThrowError__( file, function, lineno, INSTANTIATION_ERROR, t, NULL);
      return false;
    }
  return IsAtomTerm(t);
}

static Int  is_atom1( USES_REGS1 )
{
    Term t = Deref(ARG1);
    return is_atom__(__FILE__,__FUNCTION__,__LINE__,t PASS_REGS);
}


bool must_be_atom__(const char *file, const char *function, int lineno, Term t USES_REGS)
{
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t))
    {
      Yap_ThrowError__( file, function, lineno, INSTANTIATION_ERROR, t, NULL);
      return false;
    }
  if (IsAtomTerm(t))
    return true;
  else     {
      Yap_ThrowError__(file, function, lineno, TYPE_ERROR_ATOM, t, NULL);
      return false;
    }
}

static Int  must_be_atom1( USES_REGS1 )
{
    Term t = Deref(ARG1);
    return must_be_atom__(__FILE__,__FUNCTION__,__LINE__,t PASS_REGS);
}

/** @pred is_list( ?_List_ )
 *
 *  _List_ must be a list, that is, it must be bound  to a true list.
 *
 * The predicate causes an instantiation  exception if _List_ is unbound, and fails if _Term_ is bound but not to a list.
 */
bool is_list__(const char *file, const char *function, int lineno, Term list USES_REGS)
{
  Term  *tailp;
  // Term Context = Deref(ARG2);
  Int n = Yap_SkipList(&list, &tailp);
  if (IsVarTerm(*tailp))
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, list, NULL);
  if (*tailp != TermNil || n < 0)
    {
      return false;
    }
  return true;
}

static Int  is_list1( USES_REGS1 )
{
    Term t = Deref(ARG1);
    return is_list__(__FILE__,__FUNCTION__,__LINE__,t PASS_REGS);
}

bool must_be_list__(const char *file, const char *function, int lineno, Term list USES_REGS)
{
  Term  *tailp;
  // Term Context = Deref(ARG2);
  Int n = Yap_SkipList(&list, &tailp);
  if (IsVarTerm(*tailp))
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, list, NULL);
  if (*tailp != TermNil || n < 0)
    {
      return false;
    }
  return true;
}

static Int  must_be_list1( USES_REGS1 )
{
    Term t = Deref(ARG1);
    return must_be_list__(__FILE__,__FUNCTION__,__LINE__,t PASS_REGS);
}

/** @pred is_callable( ?_Goal_ )
 *
 *  _Goal must be callable, that is, it must be bound and also must be
 *  either a compound term or an atom.
 *
 *  Throws an instantiantion error if _Goal_ is unbound.
 */
bool is_callable__(const char *file, const char *function, int lineno, Term t USES_REGS)
{
  // Term Context = Deref(ARG2)Yap_Error(INSTANTIATION_ERROR, t, NULL);;
  if (IsVarTerm(t))
    {
      Yap_ThrowError__( file, function, lineno, INSTANTIATION_ERROR, t, NULL);
      return false;
    }
  Term mod = CurrentModule;
  if (mod == 0)
    mod = TermProlog;
  Term G = Yap_StripModule(Deref(ARG1), &mod);
  // Term Context = Deref(ARG2);
  if (IsVarTerm(mod))
    {
      Yap_ThrowError__( file, function, lineno, INSTANTIATION_ERROR, mod, NULL);
      return false;
    }
  else if (!IsAtomTerm(mod))
    {
      return false;
    }
  if (IsVarTerm(G))
    {
      Yap_ThrowError__( file, function, lineno, INSTANTIATION_ERROR, G, NULL);
      return false;
    }
  if (IsApplTerm(G))
    {
      Functor f = FunctorOfTerm(G);
      if (IsExtensionFunctor(f))
	{
	  return false;
	}
      else
	{
	  return true;
	}
    }
  else if (IsPairTerm(G) || IsAtomTerm(G))
    {
      return true;
    }
  else
    {
      return false;
    }
}

static Int  is_callable1( USES_REGS1 )
{
    Term t = Deref(ARG1);
    return is_callable__(__FILE__,__FUNCTION__,__LINE__,t PASS_REGS);
}

bool Yap_callable(Term t)
{
  Term mod = CurrentModule;
  if (mod == 0)
    mod = TermProlog;
  Term G = Yap_StripModule(Deref(t), &mod); // Term Context = Deref(ARG2);
  if (IsVarTerm(mod))
    {
      return false;
    }
  else if (!IsAtomTerm(mod))
    {
      return false;
    }
  if (IsVarTerm(G))
    {
      return false;
    }
  if (IsApplTerm(G))
    {
      Functor f = FunctorOfTerm(G);
      if (IsExtensionFunctor(f))
	{
	  return false;
	}
      else
	{
	  return true;
	}
    }
  else if (IsPairTerm(G) || IsAtomTerm(G))
    {
      return true;
    }
  else
    {
      return false;
    }
}

/** @pred must_be_callable( ?_Goal_ )
 *
 *  _Goal must be callable, that is, it must be bound and also must be
 *  either a compound term or an atom.
 */
static Int must_be_callable1(USES_REGS1)
{
  Term mod = CurrentModule;
  if (mod == 0)
    mod = TermProlog;
  Term G = Yap_StripModule(Deref(ARG1), &mod);
  // Term Context = Deref(ARG2);
  if (IsVarTerm(mod))
    {
      Yap_ThrowError(INSTANTIATION_ERROR, G, NULL);
      return false;
    }
  else if (!IsAtomTerm(mod))
    {
      Yap_ThrowError(TYPE_ERROR_ATOM, mod, NULL);
      return false;
    }
  if (IsVarTerm(G))
    {
      Yap_ThrowError(INSTANTIATION_ERROR, G, NULL);
      return false;
    }
  if (IsApplTerm(G))
    {
      Functor f = FunctorOfTerm(G);
      if (IsExtensionFunctor(f))
	{
	  Yap_ThrowError(TYPE_ERROR_CALLABLE, G, NULL);
	}
      else
	{
	  return true;
	}
    }
  else if (IsPairTerm(G) || IsAtomTerm(G))
    {
      return true;
    }
  else
    {
      Yap_ThrowError(TYPE_ERROR_CALLABLE, G, NULL);
      return false;
    }
  return true;
}


/// Dereferenced term t must start as a list:
///   - `[]`
///   -  `[_|_]`
///
/// no effort is made to verify if a true list
void Yap_must_be_list(Term t)
{
  if (IsVarTerm(t))
    {
      Yap_ThrowError(INSTANTIATION_ERROR, t, "");
    }
  if (t == TermNil || IsPairTerm(t))
    {
      return;
    }
  Yap_ThrowError(TYPE_ERROR_LIST, t, "");
}

/** @pred must_be_bound( ?_T_ )
 *
 *  _T_ must be instantiated.
 */
static Int must_be_bound1(USES_REGS1)
{
  Term t = Deref(ARG1);
  // Term Context = Deref(ARG2);
  if (IsVarTerm(t))
    Yap_ThrowError(INSTANTIATION_ERROR, ARG1, NULL);
  return true;
}	

/** @pred must_be_ground( ?_T_ )
 *
 *  _T_ must be fully instantiated.
 */
static Int must_be_ground1(USES_REGS1)
{
  Term t = Deref(ARG1);
  // Term Context = Deref(ARG2);
  if (!Yap_IsGroundTerm(t))
    Yap_ThrowError(INSTANTIATION_ERROR, ARG1, NULL);
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
static Int must_be_predicate_indicator1(USES_REGS1)
{
  Term G = Deref(ARG1);
  // Term Context = Deref(ARG2);
  Term mod = CurrentModule;

  G = Yap_YapStripModule(G, &mod);
if (!mod) mod = TermProlog;
if (IsVarTerm(G))
    {
      Yap_ThrowError(INSTANTIATION_ERROR, G, NULL);
    }
  if (!IsVarTerm(mod) && !IsAtomTerm(mod))
    {
      Yap_Error(TYPE_ERROR_ATOM, G, NULL);
      return false;
    }
  if (IsApplTerm(G))
    {
      Functor f = FunctorOfTerm(G);
      if (IsExtensionFunctor(f))
	{
	  Yap_ThrowError(TYPE_ERROR_PREDICATE_INDICATOR, G, NULL);
	}
      if (f == FunctorSlash || f == FunctorDoubleSlash)
	{
	  Term name = ArgOfTerm(1, G), arity = ArgOfTerm(2, G);
	  name = Yap_YapStripModule(name, &mod);
	  if (!mod) mod = TermProlog;
if (IsVarTerm(name))
	    {
	      Yap_ThrowError(INSTANTIATION_ERROR, name, NULL);
	    }
	  else if (!IsAtomTerm(name))
	    {
	      Yap_ThrowError(TYPE_ERROR_ATOM, name, NULL);
	    }
	  if (IsVarTerm(arity))
	    {
	      Yap_ThrowError(INSTANTIATION_ERROR, arity, NULL);
	    }
	  else if (!IsIntegerTerm(arity))
	    {
	      Yap_ThrowError(TYPE_ERROR_INTEGER, arity, NULL);
	    }
	  else
	    {
	      Int ar = IntegerOfTerm(arity);
	      if (ar < 0)
		{
		  Yap_ThrowError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, arity, NULL);
		}
	      if (f == FunctorDoubleSlash)
		{
		  arity = MkIntegerTerm(ar + 2);
		}
	      return Yap_unify(mod, ARG2) && Yap_unify(name, ARG3) &&
		Yap_unify(arity, ARG4);
	    }
	}
    }
  Yap_ThrowError(TYPE_ERROR_PREDICATE_INDICATOR, G, NULL);
  return false;
}

void Yap_InitErrorPreds(void)
{
  CACHE_REGS
    Yap_InitCPred("$print_exception", 1, print_exception, 0);
  Yap_InitCPred("print_exception", 1, print_exception, 0);
  Yap_InitCPred("$reset_exception", 1, reset_exception, 0);

  Yap_InitCPred("$new_exception", 1, new_exception, 0);
  Yap_InitCPred("$set_exception", 3, set_exception, 0);
  Yap_InitCPred("$read_exception", 2, read_exception, 0);
  Yap_InitCPred("$query_exception", 3, query_exception, 0);
  Yap_InitCPred("$drop_exception", 1, drop_exception, 0);
  Yap_InitCPred("$close_error", 1, close_error, HiddenPredFlag);

  /* Test predicates */
  Yap_InitCPred("is_bound", 1, is_nonvar1, TestPredFlag);
  Yap_InitCPred("is_boolean", 1, is_boolean1, TestPredFlag);
  Yap_InitCPred("is_atom", 1, is_atom1, TestPredFlag);
  Yap_InitCPred("is_boolean", 1, is_boolean1, TestPredFlag);
  Yap_InitCPred("is_callable", 1, is_callable1, TestPredFlag);
  Yap_InitCPred("is_list", 1, is_list1, TestPredFlag);
  Yap_InitCPred("is_nonvar", 1, is_nonvar1, TestPredFlag);

  Yap_InitCPred("must_be_atom", 1, must_be_atom1, TestPredFlag);
  Yap_InitCPred("must_be_boolean", 1, must_be_boolean1, TestPredFlag);
  Yap_InitCPred("must_be_bound", 1, must_be_bound1, TestPredFlag);
  Yap_InitCPred("must_be_callable", 1, must_be_callable1, TestPredFlag);
  Yap_InitCPred("must_be_ground", 1, must_be_ground1, TestPredFlag);
  Yap_InitCPred("must_be_list", 1, must_be_list1, TestPredFlag);

  Yap_InitCPred("is_list", 1, is_list1, TestPredFlag);
  Yap_InitCPred("must_be_predicate_indicator", 4, must_be_predicate_indicator1, 0);
}

/**
   @}
*/
					
