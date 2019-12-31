/*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		iopreds.c *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Input/Output C implemented predicates			 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes the definition of a miscellania of standard predicates
 * for yap refering to: Files and GLOBAL_Streams, Simple Input/Output,
 *
 */

#include "Yap.h"
#include "YapEval.h"
#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
#include "yapio.h"
#include <stdlib.h>
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_SYS_SELECT_H && !_MSC_VER && !defined(__MINGW32__)
#include <sys/select.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_SIGNAL_H
#include <signal.h>
#endif
#if HAVE_FCNTL_H
/* for O_BINARY and O_TEXT in WIN32 */
#include <fcntl.h>
#endif
#ifdef _WIN32
#if HAVE_IO_H
/* Windows */
#include <io.h>
#endif
#endif
#if !HAVE_STRNCAT
#define strncat(X, Y, Z) strcat(X, Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X, Y, Z) strcpy(X, Y)
#endif
#if _MSC_VER || defined(__MINGW32__)
#if HAVE_SOCKET
#include <winsock2.h>
#endif
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR) == _S_IFDIR)
#endif
#endif
#include "iopreds.h"
#include "clause.h"

static Term readFromBuffer(const char *s, Term opts) {
  Term rval;
  int sno;
  encoding_t enc = ENC_ISO_UTF8;
  sno = Yap_open_buf_read_stream(
      (char *)s, strlen_utf8((unsigned char *)s), &enc, MEM_BUF_USER,
      Yap_LookupAtom(Yap_StrPrefix((char *)s, 16)), TermNone);

  rval = Yap_read_term(sno, opts, 3);
  Yap_CloseStream(sno);
  return rval;
}

#if _MSC_VER || defined(__MINGW32__)
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif

static bool write_term(int output_stream, Term t, bool b, xarg *args USES_REGS) {
  bool rc;
  Term cm = CurrentModule;
  yhandle_t yh = Yap_CurrentHandle();
  int depth, prio, flags = 0;
  Int numv;
	Int n=0;

  if (t==0)
    return false;
  t = Deref(t);
  
    if (args[WRITE_ATTRIBUTES].used) {
    Term ctl = args[WRITE_ATTRIBUTES].tvalue;
    if (ctl == TermWrite) {
      flags |= AttVar_None_f;
    } else if (ctl == TermPortray) {
      flags |= AttVar_None_f | AttVar_Portray_f;
    } else if (ctl == TermDots) {
      flags |= AttVar_Dots_f;
    } else if (ctl != TermIgnore) {
      Yap_Error(
          DOMAIN_ERROR_WRITE_OPTION, ctl,
          "write attributes should be one of {dots,ignore,portray,write}");
      rc = false;
      goto end;
    }
  }
  if (!args[WRITE_CYCLES].used || (args[WRITE_CYCLES].used
				   && args[WRITE_CYCLES].tvalue == TermTrue)) {
    flags |= Handle_cyclics_f;
  }
  if (args[WRITE_QUOTED].used && args[WRITE_QUOTED].tvalue == TermTrue) {
    flags |= Quote_illegal_f;
  }
  if (args[WRITE_IGNORE_OPS].used &&
      args[WRITE_IGNORE_OPS].tvalue == TermTrue) {
    flags |= Ignore_ops_f;
  }
  if (args[WRITE_PORTRAY].used && args[WRITE_IGNORE_OPS].tvalue == TermTrue) {
    flags |= Ignore_ops_f;
  }
  if (args[WRITE_PORTRAYED].used && args[WRITE_PORTRAYED].tvalue == TermTrue) {
    flags |= Use_portray_f;
  }
  if (args[WRITE_CHARACTER_ESCAPES].used &&
      args[WRITE_CHARACTER_ESCAPES].tvalue == TermFalse) {
    flags |= No_Escapes_f;
  }
  if (args[WRITE_BACKQUOTES].used &&
      args[WRITE_BACKQUOTES].tvalue == TermTrue) {
    flags |= BackQuote_String_f;
  }
  if (args[WRITE_BRACE_TERMS].used &&
      args[WRITE_BRACE_TERMS].tvalue == TermFalse) {
    flags |= No_Brace_Terms_f;
  }
  if (args[WRITE_FULLSTOP].used && args[WRITE_FULLSTOP].tvalue == TermTrue) {
    flags |= Fullstop_f;
  }
  if (args[WRITE_NL].used && args[WRITE_NL].tvalue == TermTrue) {
    flags |= New_Line_f;
  }

  if (args[WRITE_MAX_DEPTH].used) {
    depth = IntegerOfTerm(args[WRITE_MAX_DEPTH].tvalue);
  } else
    depth = LOCAL_max_depth;
  Yap_plwrite(t, GLOBAL_Stream + output_stream, depth, flags, args);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  rc = true;

end:
     CurrentModule = cm;
  Yap_RecoverHandles(0, yh);
  return rc;
}


#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t write_defs[] = {WRITE_DEFS()};
#undef PAR

/**
 *
 */
bool Yap_WriteTerm(int output_stream, Term t, Term opts USES_REGS) {
  xarg *args = Yap_ArgListToVector(opts, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, opts, NULL);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  LOCK(GLOBAL_Stream[output_stream].streamlock);
  write_term(output_stream, t, false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  return (TRUE);
}

static Int write_term2(USES_REGS1) {

  /* '$write'(+Flags,?Term) */
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  return Yap_WriteTerm(LOCAL_c_output_stream, ARG1, ARG2 PASS_REGS);
}

static Int write_term3(USES_REGS1) {

  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0) {
    return false;
  }
  return Yap_WriteTerm(output_stream, ARG2, ARG3 PASS_REGS);
}

static Int write2(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */

  xarg *args;
  yhandle_t mySlots;
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0)
    return false;
  args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                             DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  mySlots = Yap_StartSlots();
  args[WRITE_SINGLETONS].used = true;
  args[WRITE_SINGLETONS].tvalue = TermTrue;
  write_term(output_stream, ARG2, false , args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int write1(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1)
    output_stream = 1;
  xarg *args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  args[WRITE_SINGLETONS].used = true;
  args[WRITE_SINGLETONS].tvalue = TermTrue;
  LOCK(GLOBAL_Stream[output_stream].streamlock);
  write_term(output_stream, ARG1, false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int write_canonical1(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1)
    output_stream = 1;
  xarg *args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  args[WRITE_SINGLETONS].used = true;
  args[WRITE_SINGLETONS].tvalue = TermTrue;
  args[WRITE_IGNORE_OPS].used = true;
  args[WRITE_IGNORE_OPS].tvalue = TermTrue;
  args[WRITE_QUOTED].used = true;
  args[WRITE_QUOTED].tvalue = TermTrue;
  args[WRITE_CYCLES].used = true;
  args[WRITE_CYCLES].tvalue = TermTrue;
  LOCK(GLOBAL_Stream[output_stream].streamlock);
  write_term(output_stream, ARG1, false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int write_canonical(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  xarg *args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0) {
    free(args);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  args[WRITE_SINGLETONS].used = true;
  args[WRITE_SINGLETONS].tvalue = TermTrue;
  args[WRITE_IGNORE_OPS].used = true;
  args[WRITE_IGNORE_OPS].tvalue = TermTrue;
  args[WRITE_CYCLES].used = true;
  args[WRITE_CYCLES].tvalue = TermTrue;
  args[WRITE_QUOTED].used = true;
  args[WRITE_QUOTED].tvalue = TermTrue;
  write_term(output_stream, ARG2, false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int writeq1(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  xarg *args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1) {
    free(args);
    output_stream = 1;
  }
  args[WRITE_SINGLETONS].used = true;
    args[WRITE_SINGLETONS].tvalue = TermTrue;
  args[WRITE_QUOTED].used = true;
  args[WRITE_QUOTED].tvalue = TermTrue;
  write_term(output_stream, ARG1, false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int writeq(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  xarg *args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0) {
    free(args);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  args[WRITE_SINGLETONS].used = true;
  args[WRITE_SINGLETONS].tvalue = TermTrue;
  args[WRITE_QUOTED].used = true;
  args[WRITE_QUOTED].tvalue = TermTrue;
  write_term(output_stream, ARG2, false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int print1(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  xarg *args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1) {
    free(args);
    output_stream = 1;
  }
  args[WRITE_PORTRAY].used = true;
  args[WRITE_PORTRAY].tvalue = TermTrue;
  args[WRITE_SINGLETONS].used = true;
  args[WRITE_SINGLETONS].tvalue = TermTrue;
  LOCK(GLOBAL_Stream[output_stream].streamlock);
  write_term(output_stream, ARG1, false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int print(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  xarg *args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0) {
    free(args);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  args[WRITE_PORTRAY].used = true;
  args[WRITE_PORTRAY].tvalue = TermTrue;
    args[WRITE_SINGLETONS].used = true;
    args[WRITE_SINGLETONS].tvalue = TermTrue;
  write_term(output_stream, ARG2, false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int writeln1(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1)
    output_stream = 1;
  xarg *args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  args[WRITE_NL].used = true;
  args[WRITE_NL].tvalue = TermTrue;
    args[WRITE_SINGLETONS].used = true;
    args[WRITE_SINGLETONS].tvalue = TermTrue;
  args[WRITE_CYCLES].used = true;
  args[WRITE_CYCLES].tvalue = TermTrue;
  LOCK(GLOBAL_Stream[output_stream].streamlock);
  write_term(output_stream, ARG1, false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int writeln(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  xarg *args = Yap_ArgListToVector(TermNil, write_defs, WRITE_END,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, TermNil, NULL);
    return false;
  }
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "writeln/2");
  if (output_stream < 0) {
    free(args);
    return false;
  }
  yhandle_t mySlots = Yap_StartSlots();
  args[WRITE_NL].used = true;
  args[WRITE_NL].tvalue = TermTrue;
  args[WRITE_SINGLETONS].used = true;
  args[WRITE_SINGLETONS].tvalue = TermTrue;
  args[WRITE_CYCLES].used = true;
  args[WRITE_CYCLES].tvalue = TermTrue;
  write_term(output_stream, ARG2,  false, args PASS_REGS);
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  free(args);
  Yap_CloseSlots(mySlots);
  Yap_RaiseException();
  return (TRUE);
}

static Int p_write_depth(USES_REGS1) { /* write_depth(Old,New)          */
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Term t3 = Deref(ARG3);

  if (!IsVarTerm(t1) && !IsIntegerTerm(t1)) {
    Yap_Error(TYPE_ERROR_INTEGER, t1, "write_depth/3");
    return FALSE;
  }
  if (!IsVarTerm(t2) && !IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "write_depth/3");
    return FALSE;
  }
  if (!IsVarTerm(t3) && !IsIntegerTerm(t3)) {
    Yap_Error(TYPE_ERROR_INTEGER, t3, "write_depth/3");
    return FALSE;
  }
  if (IsVarTerm(t1)) {
    Term t = MkIntegerTerm(LOCAL_max_depth);
    if (!Yap_unify_constant(t1, t))
      return FALSE;
  } else
    LOCAL_max_depth = IntegerOfTerm(t1);
  if (IsVarTerm(t2)) {
    Term t = MkIntegerTerm(LOCAL_max_list);
    if (!Yap_unify_constant(t2, t))
      return FALSE;
  } else
    LOCAL_max_list = IntegerOfTerm(t2);
  if (IsVarTerm(t3)) {
    Term t = MkIntegerTerm(LOCAL_max_write_args);
    if (!Yap_unify_constant(t3, t))
      return FALSE;
  } else
    LOCAL_max_write_args = IntegerOfTerm(t3);
  return TRUE;
}

static Int dollar_var(USES_REGS1) {
  Term in = Deref(ARG1);
  if (IsVarTerm(in)) {
    Term t2;
    if (!IsVarTerm(t2 = Deref(ARG2))) {
      if (IsApplTerm(t2) && FunctorOfTerm(t2) == FunctorDollarVar) {
        return Yap_unify(ArgOfTerm(1, t2), ARG1);
      }
      Yap_Error(TYPE_ERROR_COMPOUND, ARG2, "");
      return false;
    } else {
      Yap_Error(INSTANTIATION_ERROR, ARG2, "");
    }
  }
  Term t2 = Yap_unify(MkVarTerm(), ARG1);
  Term tv = Yap_MkApplTerm(FunctorDollarVar, 1, &t2);
  return Yap_unify(tv, ARG2);
}



static Int term_to_string(USES_REGS1) {
  Term t2 = Deref(ARG2), rc = false, t1 = Deref(ARG1);
  const char *s;
  if (IsVarTerm(t2)) {
    s = Yap_TermToBuffer(t1, Quote_illegal_f | Handle_vars_f);
    if (!s || !MkStringTerm(s)) {
      Yap_Error(RESOURCE_ERROR_HEAP, t1,
                "Could not get memory from the operating system");
      return false;
    }
    return Yap_unify(ARG2, MkStringTerm(s));
  } else if (!IsStringTerm(t2)) {
    Yap_Error(TYPE_ERROR_STRING, t2, "term_to_string/3");
    return false;
  } else {
    s = StringOfTerm(t2);
  }
  yhandle_t y1 = Yap_InitHandle( t1 );
  return (rc = readFromBuffer(s, TermNil)) != 0L && Yap_unify(rc, Yap_PopHandle(y1));
}

/**
 *
 * @return
 */
static Int term_to_atom(USES_REGS1) {
  Term t2 = Deref(ARG2), ctl, rc = false;
  Atom at;
  if (IsVarTerm(t2)) {
    const char *s =
        Yap_TermToBuffer(Deref(ARG1), Quote_illegal_f | Handle_vars_f);
    if (!s || !(at = Yap_UTF8ToAtom((const unsigned char *)s))) {
      Yap_Error(RESOURCE_ERROR_HEAP, t2,
                "Could not get memory from the operating system");
      return false;
    }
    return Yap_unify(ARG2, MkAtomTerm(at));
  } else if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_ATOM, t2, "atom_to_term/2");
    return (FALSE);
  } else {
    at = AtomOfTerm(t2);
  }
  ctl = TermNil;
  return ((rc = Yap_UBufferToTerm(RepAtom(at)->UStrOfAE, ctl))) &&
         Yap_unify(rc, ARG1);
}



char *Yap_TermToBuffer(Term t, int flags) {
  CACHE_REGS
  int sno = Yap_open_buf_write_stream(LOCAL_encoding, flags);

  if (sno < 0)
    return NULL;
  if (t == 0)
    return NULL;
  else
    t = Deref(t);
  GLOBAL_Stream[sno].encoding = LOCAL_encoding;
  GLOBAL_Stream[sno].status |= CloseOnException_Stream_f;
  GLOBAL_Stream[sno].status &= ~FreeOnClose_Stream_f;
  Yap_plwrite(t, GLOBAL_Stream + sno, LOCAL_max_depth, flags, NULL);
  char *new = Yap_MemExportStreamPtr(sno);
  Yap_CloseStream(sno);
  return new;
}

void Yap_InitWriteTPreds(void) {
  Yap_InitCPred("write_term", 2, write_term2, SyncPredFlag);
  Yap_InitCPred("write_term", 3, write_term3, SyncPredFlag);
  Yap_InitCPred("write", 1, write1, SyncPredFlag);
  Yap_InitCPred("write", 2, write2, SyncPredFlag);
  Yap_InitCPred("writeq", 1, writeq1, SyncPredFlag);
  Yap_InitCPred("writeq", 2, writeq, SyncPredFlag);
  Yap_InitCPred("writeln", 1, writeln1, SyncPredFlag);
  Yap_InitCPred("writeln", 2, writeln, SyncPredFlag);
  Yap_InitCPred("write_canonical", 1, write_canonical1, SyncPredFlag);
  Yap_InitCPred("write_canonical", 2, write_canonical, SyncPredFlag);
  Yap_InitCPred("print", 1, print1, SyncPredFlag);
  Yap_InitCPred("print", 2, print, SyncPredFlag);
  Yap_InitCPred("write_depth", 3, p_write_depth, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("term_to_string", 2, term_to_string, 0);
  Yap_InitCPred("term_to_atom", 2, term_to_atom, 0);
  Yap_InitCPred("write_depth", 3, p_write_depth, SafePredFlag | SyncPredFlag);
  ;
  Yap_InitCPred("$VAR", 2, dollar_var, SafePredFlag);
  ;
}
