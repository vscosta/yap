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

bool Yap_Warning(const char *s, ...) {
  CACHE_REGS
  va_list ap;
  PredEntry *pred;
  bool rc;
  Term ts[2];
  const char *fmt;
  char tmpbuf[MAXPATHLEN];

  LOCAL_DoingUndefp = true;
  LOCAL_within_print_message = true;
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
  } else
    return false;
  va_end(ap);
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    fprintf(stderr, "warning message: %s\n", tmpbuf);
    LOCAL_DoingUndefp = false;
    LOCAL_within_print_message = false;
    return false;
  }

  ts[1] = MkAtomTerm(AtomWarning);
  ts[0] = MkAtomTerm(Yap_LookupAtom(tmpbuf));
  rc = Yap_execute_pred(pred, ts, true PASS_REGS);
  return rc;
}
void Yap_InitError(yap_error_number e, Term t, const char *msg) {
  if (LOCAL_ActiveError->status) {
    Yap_exit(1);
  }
  LOCAL_ActiveError->errorNo = e;
  LOCAL_ActiveError->errorFile = NULL;
  LOCAL_ActiveError->errorFunction = NULL;
  LOCAL_ActiveError->errorLine = 0;
  if (msg) {
    LOCAL_Error_Size = strlen(msg);
    LOCAL_ActiveError->errorMsg = malloc(LOCAL_Error_Size + 1);
    strcpy(LOCAL_ActiveError->errorMsg, msg);
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

  if (LOCAL_within_print_message) {
    /* error within error */
    fprintf(stderr, "%% WARNING WITHIN WARNING\n");
    Yap_RestartYap(1);
  }
  LOCAL_DoingUndefp = true;
  LOCAL_within_print_message = true;
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    fprintf(stderr, "warning message:\n");
    Yap_DebugPlWrite(twarning);
    fprintf(stderr, "\n");
    LOCAL_DoingUndefp = false;
    LOCAL_within_print_message = false;
    CurrentModule = cmod;
    return false;
  }
  ts[1] = twarning;
  ts[0] = MkAtomTerm(AtomWarning);
  HB = B->cp_h = HR;
  B->cp_tr = TR;
  rc = Yap_execute_pred(pred, ts, true PASS_REGS);
  LOCAL_within_print_message = false;
  LOCAL_DoingUndefp = false;
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
      Yap_Error__(file, function, lineno, RESOURCE_ERROR_STACK, ARG1, serr);
      return false;
    }
    return true;
  case RESOURCE_ERROR_AUXILIARY_STACK:
    if (LOCAL_MAX_SIZE < (char *)AuxSp - AuxBase) {
      LOCAL_MAX_SIZE += 1024;
    }
    if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
      /* crash in flames */
      Yap_Error__(file, function, lineno, RESOURCE_ERROR_AUXILIARY_STACK, ARG1,
                  serr);
      return false;
    }
    return true;
  case RESOURCE_ERROR_HEAP:
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error__(file, function, lineno, RESOURCE_ERROR_HEAP, ARG2, serr);
      return false;
    }
  default:
    Yap_Error__(file, function, lineno, err, TermNil, serr);
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
    return TRUE;
  case RESOURCE_ERROR_HEAP:
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, ARG2, serr);
      return FALSE;
    }
  default:
    Yap_Error(err, TermNil, serr);
    return (FALSE);
  }
}

void Yap_RestartYap(int flag) {
  CACHE_REGS
#if PUSH_REGS
  restore_absmi_regs(&Yap_standard_regs);
#endif
  siglongjmp(*LOCAL_RestartEnv, 1);
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

#define BEGIN_ERROR_CLASSES()                                                  \
  static Term mkerrorct(yap_error_class_number c, Term *ts) {                  \
    switch (c) {

#define ECLASS(CL, A, B)                                                       \
  case CL:                                                                     \
    if (A == 0)                                                                \
      return MkAtomTerm(Yap_LookupAtom(A));                                    \
    else {                                                                     \
      return Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(A), B), B, ts);       \
    }

#define END_ERROR_CLASSES()                                                    \
  }                                                                            \
  return TermNil;                                                              \
  }

#define BEGIN_ERRORS()                                                         \
  static Term mkerrort(yap_error_number e, Term *ts) {                         \
    switch (e) {

#define E0(A, B)                                                               \
  case A:                                                                      \
    return mkerrorct(B, ts);

#define E(A, B, C)                                                             \
  case A:                                                                      \
    ts -= 1;                                                                   \
    ts[0] = MkAtomTerm(Yap_LookupAtom(C));                                     \
    return mkerrorct(B, ts);

#define E2(A, B, C, D)                                                         \
  case A:                                                                      \
    ts -= 2;                                                                   \
    ts[0] = MkAtomTerm(Yap_LookupAtom(C));                                     \
    ts[1] = MkAtomTerm(Yap_LookupAtom(D));                                     \
    return mkerrorct(B, ts);

#define END_ERRORS()                                                           \
  }                                                                            \
  return TermNil;                                                              \
  }

#include "YapErrors.h"

void Yap_pushErrorContext(yap_error_descriptor_t *new_error) {
  new_error->top_error = LOCAL_ActiveError;
  LOCAL_ActiveError = new_error;
}

yap_error_descriptor_t *Yap_popErrorContext(void) {
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
    Yap_Error__(file, function, lineno, type, where, tmpbuf);
  } else {
    Yap_Error__(file, function, lineno, type, where);
  }
  siglongjmp(*LOCAL_RestartEnv, 5);
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
yamop *Yap_Error__(const char *file, const char *function, int lineno,
                   yap_error_number type, Term where, ...) {
  CACHE_REGS
  va_list ap;
  CELL nt[3];
  Functor fun;
  Term error_t;
  Term comment;
  char *fmt;
  char s[MAXPATHLEN];

  /* disallow recursive error handling */
  if (LOCAL_PrologMode & InErrorMode) {
    fprintf(stderr, "%% ERROR WITHIN ERROR %d: %s\n", LOCAL_Error_TYPE, tmpbuf);
    Yap_RestartYap(1);
  }
  LOCAL_ActiveError->errorNo = type;
  LOCAL_ActiveError->errorAsText = Yap_errorName(type);
  LOCAL_ActiveError->errorClass = Yap_errorClass(type);
  LOCAL_ActiveError->classAsText =
    Yap_errorClassName(LOCAL_ActiveError->errorClass);
  LOCAL_ActiveError->errorLine = lineno;
  LOCAL_ActiveError->errorFunction = function;
  LOCAL_ActiveError->errorFile = file;
  Yap_find_prolog_culprit(PASS_REGS1);
  LOCAL_PrologMode |= InErrorMode;
  Yap_ClearExs();
  if (where == 0L) {
    where = TermNil;
  }
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
            (int)IntOfTerm(where));
    LOCAL_PrologMode &= ~InErrorMode;
    Yap_exit(1);
  }
  if (LOCAL_within_print_message) {
    /* error within error */
    fprintf(stderr, "%% ERROR WITHIN WARNING %d: %s\n", LOCAL_Error_TYPE,
            tmpbuf);
    LOCAL_PrologMode &= ~InErrorMode;
    Yap_exit(1);
  }
  va_start(ap, where);
  fmt = va_arg(ap, char *);
  if (fmt != NULL) {
#if HAVE_VSNPRINTF
    (void)vsnprintf(s, MAXPATHLEN - 1, fmt, ap);
#else
    (void)vsprintf(s, fmt, ap);
#endif
    // fprintf(stderr, "warning: ");
    comment = MkAtomTerm(Yap_LookupAtom(s));
  } else if (LOCAL_ErrorMessage && LOCAL_ErrorMessage[0]) {
    comment = MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage));
  } else {
    comment = TermNil;
  }
  va_end(ap);
  if (P == (yamop *)(FAILCODE)) {
    memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError));
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  }
  /* PURE_ABORT may not have set where correctly, BootMode may not have the data
   * terms ready */
  if (type == ABORT_EVENT || LOCAL_PrologMode & BootMode) {
    where = TermNil;
    LOCAL_PrologMode &= ~AbortMode;
    LOCAL_PrologMode &= ~InErrorMode;
    /* make sure failure will be seen at next port */
    // no need to lock & unlock
    if (LOCAL_PrologMode & AsyncIntMode)
      Yap_signal(YAP_FAIL_SIGNAL);
    P = FAILCODE;
  } else {
    if (IsVarTerm(where)) {
      /* we must be careful someone gave us a copy to a local variable */
      Term t = MkVarTerm();
      Yap_unify(t, where);
      where = Deref(where);
    }
    /* Exit Abort Mode, if we were there */
    LOCAL_PrologMode &= ~AbortMode;
    LOCAL_PrologMode |= InErrorMode;
    if (!(where = Yap_CopyTerm(where))) {
      where = TermNil;
    }
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
  if (!IsVarTerm(where) && IsApplTerm(where) &&
      FunctorOfTerm(where) == FunctorError) {
    error_t = where;
    P = (yamop *)FAILCODE;
    Yap_JumpToEnv(error_t);
    LOCAL_PrologMode &= ~InErrorMode;
    return P;
  }
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
    nt[0] = MkAtomTerm(AtomDAbort);
    fun = FunctorDollarVar;
    //    serious = true;
    break;
  case CALL_COUNTER_UNDERFLOW_EVENT:
    /* Do a long jump */
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    Yap_JumpToEnv(MkAtomTerm(AtomCallCounter));
    P = (yamop *)FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return (P);
  case PRED_ENTRY_COUNTER_UNDERFLOW_EVENT:
    /* Do a long jump */
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    Yap_JumpToEnv(MkAtomTerm(AtomCallAndRetryCounter));
    P = (yamop *)FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return (P);
  case RETRY_COUNTER_UNDERFLOW_EVENT:
    /* Do a long jump */
    LOCAL_ReductionsCounterOn = FALSE;
    LOCAL_PredEntriesCounterOn = FALSE;
    LOCAL_RetriesCounterOn = FALSE;
    Yap_JumpToEnv(MkAtomTerm(AtomRetryCounter));
    P = (yamop *)FAILCODE;
    LOCAL_PrologMode &= ~InErrorMode;
    return (P);
  default: {
    LOCAL_PrologMode &= ~InErrorMode;
    Term ts[3];
    ts[2] = where;
    nt[0] = mkerrort(type, ts + 2);
  }
  }
  LOCAL_PrologMode &= ~InErrorMode;
  if (type != ABORT_EVENT) {
    Term location;

    /* This is used by some complex procedures to detect there was an error */
    if (IsAtomTerm(nt[0])) {
      LOCAL_ErrorMessage = RepAtom(AtomOfTerm(nt[0]))->StrOfAE;
    } else {
      LOCAL_ErrorMessage =
	(char *)RepAtom(NameOfFunctor(FunctorOfTerm(nt[0])))->StrOfAE;
    }
    nt[1] = TermNil;
    switch (type) {
    case RESOURCE_ERROR_HEAP:
    case RESOURCE_ERROR_STACK:
    case RESOURCE_ERROR_TRAIL:
      comment = MkAtomTerm(Yap_LookupAtom(tmpbuf));
    default:
      if (comment != TermNil)
        nt[1] = MkPairTerm(MkPairTerm(MkAtomTerm(Yap_LookupAtom("i")), comment),
                           nt[1]);
      if (file && function) {
        Term ts[3], t3;
        ts[0] = MkAtomTerm(Yap_LookupAtom(file));
        ts[1] = MkIntegerTerm(lineno);
        ts[2] = MkAtomTerm(Yap_LookupAtom(function));
        t3 = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("c"), 3), 3, ts);
        nt[1] =
	  MkPairTerm(MkPairTerm(MkAtomTerm(Yap_LookupAtom("c")), t3), nt[1]);
      }
      if ((location = Yap_pc_location(P, B, ENV)) != TermNil) {
        nt[1] = MkPairTerm(
			   MkPairTerm(MkAtomTerm(Yap_LookupAtom("p")), location), nt[1]);
      }
      if ((location = Yap_env_location(CP, B, ENV, 0)) != TermNil) {
        nt[1] = MkPairTerm(
			   MkPairTerm(MkAtomTerm(Yap_LookupAtom("e")), location), nt[1]);
      }
    }
  }
  /* disable active signals at this point */
  LOCAL_Signals = 0;
  CalculateStackGap(PASS_REGS1);
#if DEBUG
  //    DumpActiveGoals( PASS_REGS1 );
#endif
  /* wait if we we are in user code,
     it's up to her to decide */
  fun = FunctorError;
  error_t = Yap_MkApplTerm(fun, 2, nt);

  if (type == ABORT_EVENT) {
    error_t = MkAtomTerm(AtomDAbort);
  } else {
    error_t = Yap_MkApplTerm(fun, 2, nt);
  }
  memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError));
  Yap_JumpToEnv(error_t);
  P = (yamop *)FAILCODE;
  LOCAL_PrologMode &= ~InErrorMode;
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

void Yap_InitErrorPreds(void) {
  CACHE_REGS
  Yap_InitCPred("$close_error", 0, close_error, HiddenPredFlag);
  Yap_InitCPred("is_boolean", 2, is_boolean, TestPredFlag);
  Yap_InitCPred("is_callable", 2, is_callable, TestPredFlag);
  Yap_InitCPred("is_atom", 2, is_atom, TestPredFlag);
  Yap_InitCPred("is_predicate_indicator", 2, is_predicate_indicator,
                TestPredFlag);
}
