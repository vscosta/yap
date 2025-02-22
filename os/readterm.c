/*************************************************************************
 *									 *
 *	 YAP Prolog							 *
 *									 *
 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		iopreds.c *
 * Last rev:	5/2/88							 *
 * mods: *
 * comemnts:	Input/Output C implemented predicates			 *
 *

 *
 *************************************************************************/



#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
 * @file readterm.c
 * @brief glue code between parser and streams.
 * for yap refering to: Files and GLOBAL_Streams, Simple Input/Output,
 *
 * @defgroup ReadTerm Read Term From A Stream
 * @ingroup InputOutput
 *  @{
 */

#include "Yap.h"

#include "YapError.h"

#include "YapEval.h"

#include "YapFlags.h"

#include "YapHeap.h"

#include "YapText.h"

#include "Yatom.h"

#include "yapio.h"

#include <stdlib.h>

#include "YapArenas.h"

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
/* priows */
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

#if _MSC_VER || defined(__MINGW32__)
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif

static Term is_output_list(Term t)
{
    Term *tailp;
  Yap_SkipList(&t, &tailp);
  if (IsVarTerm(*tailp) || *tailp == TermNil)
      return t;
  Yap_ThrowError(DOMAIN_ERROR_READ_OPTION,t,NULL);
  return TermZERO;
}

static void clean_vars(VarEntry *p) {
  if (p == NULL)
    return;
  p->VarAdr = TermNil;
  clean_vars(p->VarLeft);
  clean_vars(p->VarRight);
}

#undef PAR

#ifdef O_QUASIQUOTATIONS
/** '$qq_open'(+QQRange, -Stream) is det.

    Opens a quasi-quoted memory range.

    @arg QQRange is a term '$quasi_quotation'(ReadData, Start, Length)
    @arg Stream  is a UTF-8 encoded string, whose position indication
    reflects the location in the real file.
*/

static Int qq_open(USES_REGS1) {
  PRED_LD

  Term t = Deref(ARG1);

  if (!IsVarTerm(t) && IsApplTerm(t) && FunctorOfTerm(t) =
          FunctorDQuasiQuotation) {
    void *ptr;
    char *start;
    size_t l int s;
    Term t0, t1, t2;

    if (IsPointerTerm((t0 = ArgOfTerm(1, t))) &&
        IsPointerTerm((t1 = ArgOfTerm(2, t))) &&
        IsIntegerTerm((t2 = ArgOfTerm(3, t)))) {
      ptr = PointerOfTerm(t0);
      start = PointerOfTerm(t1);
      len = IntegerOfTerm(t2);
      if ((s = Yap_open_buf_read_stream(start, len, ENC_UTF8, MEM_BUF_USER)) <
          0)
        return false;
      return Yap_unify(ARG2, Yap_CbMkStream(s));
    } else {
      Yap_ThrowError(TYPE_ERROR_READ_CONTEXT, t);
    }

    return FALSE;
  }
}

static int parse_quasi_quotations(ReadData _PL_rd ARG_LD) {
  if (_PL_rd->qq_tail) {
    term_t av;
    int rc;

    if (!PL_unify_nil(_PL_rd->qq_tail))
      return FALSE;

    if (!_PL_rd->quasi_quotations) {
      if ((av = PL_new_term_refs(2)) && PL_put_term(av + 0, _PL_rd->qq) &&
#if __YAP_PROLOG__
          PL_put_atom(av + 1, YAP_SWIAtomFromAtom(_PL_rd->module->AtomOfME)) &&
#else
          PL_put_atom(av + 1, _PL_rd->module->name) &&
#endif
          PL_cons_functor_v(av, FUNCTOR_dparse_quasi_quotations2, av)) {
        term_t ex;
        rc = callProlog(MODULE_system, av + 0, PL_Q_CATCH_EXCEPTION, &ex);
        if (rc)
          return TRUE;
        _PL_rd->exception = ex;
        _PL_rd->has_exception = TRUE;
      }
      return FALSE;
    } else
      return TRUE;
  } else if (_PL_rd->quasi_quotations) /* user option, but no quotes */
  {
    return PL_unify_nil(_PL_rd->quasi_quotations);
  } else
    return TRUE;
}

#endif /*O_QUASIQUOTATIONS*/



#undef PAR



 #define READ_DEFS()                                                            \
  PAR("comments", is_output_list, READ_COMMENTS),                                 \
      PAR("module", isatom, READ_MODULE), PAR("priority", nat, READ_PRIORITY), \
      PAR("output", filler, READ_OUTPUT),                                      \
      PAR("quasi_quotations", filler, READ_QUASI_QUOTATIONS),                  \
      PAR("term_position", filler, READ_TERM_POSITION),                        \
      PAR("syntax_errors", synerr, READ_SYNTAX_ERRORS),                        \
          PAR("scan", is_output_list, READ_SCAN),                                \
          PAR("singletons", is_output_list, READ_SINGLETONS),                                \
          PAR("variables", is_output_list, READ_VARIABLES),                                \
      PAR("variable_names", is_output_list, READ_VARIABLE_NAMES),                      \
      PAR("character_escapes", booleanFlag, READ_CHARACTER_ESCAPES),           \
      PAR("input_closing_blank", booleanFlag, READ_INPUT_CLOSING_BLANK),       \
      PAR("backquoted_string", isatom, READ_BACKQUOTED_STRING),                \
      PAR("singlequoted_string", isatom, READ_SINGLEQUOTED_STRING),            \
      PAR("doublequoted_string", isatom, READ_DOUBLEQUOTED_STRING),            \
      PAR("var_prefix", isatom, READ_VAR_PREFIX),                              \
      PAR("allow_variable_name_as_functor", isatom,                            \
          READ_ALLOW_VARIABLE_NAME_AS_FUNCTOR),                                \
      PAR("cycles", booleanFlag, READ_CYCLES), PAR(NULL, ok, READ_END)

#define PAR(x, y, z) z

typedef enum read_enum_choices { READ_DEFS() } read_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t read_defs[] = {READ_DEFS()};
#undef PAR

static Term add_output(Term t, Term tail) {
  CACHE_REGS
    
  Term topt = Yap_MkApplTerm(Yap_MkFunctor(AtomOutput, 1), 1, &t);

  tail = Deref(tail);
  if (IsVarTerm(tail)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tail, "unbound list of options");
  } else if (IsPairTerm(tail) || tail == TermNil) {
    return MkPairTerm(topt, tail);
  } else {
    Yap_ThrowError(TYPE_ERROR_LIST, tail, "list of options");
  }
  return false;
}

static Term add_names(Term t, Term tail) {
  CACHE_REGS
    
  Term topt = Yap_MkApplTerm(Yap_MkFunctor(AtomVariableNames, 1), 1, &t);

  if (IsVarTerm(tail)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tail, "unbound list of options");
  } else if (IsPairTerm(tail) || tail == TermNil) {
    return MkPairTerm(topt, tail);
  } else {
    Yap_ThrowError(TYPE_ERROR_LIST, tail, "list of options");
  }
  return false;
}

static Term add_priority(Term t, Term tail) {
  CACHE_REGS
  Term topt = Yap_MkNewApplTerm(Yap_MkFunctor(AtomPriority, 1), 1);

  Yap_unify(t, ArgOfTerm(1, topt));
  if (IsVarTerm(tail)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tail, "unbound list of options");
  } else if (IsPairTerm(tail) || tail == TermNil) {
    return MkPairTerm(topt, tail);
  } else {
    Yap_ThrowError(TYPE_ERROR_LIST, tail, "list of options");
  }
  return false;
}

#if 0
static Term scanToList(TokEntry *tok, TokEntry *errtok) {
  CACHE_REGS
    
  TokEntry *tok0 = tok;
  CELL *Hi = HR;
  Term ts[1];

  ts[0] = TermNil;
  Term *tailp = ts;

  while (tok) {
    if (HR > ASP - 1024) {
      /* for some reason moving this earlier confuses gcc on solaris */
      HR = Hi;
      tok = tok0;
      if (!Yap_dogc(PASS_REGS1)) {
        return 0;
      }
      continue;
    }
    if (tok == errtok && tok->Tok != Error_tok) {
      *tailp = MkPairTerm(MkAtomTerm(AtomError), TermNil);
      tailp = RepPair(*tailp) + 1;
    }
    Term rep = Yap_tokRep(tok);
    *tailp = MkPairTerm(rep, TermNil);
    tailp = RepPair(*tailp) + 1;
    if (tok->TokNext == NULL) {
      break;
    }
    tok = tok->TokNext;
  }
  return ts[0];
}
#endif

static Term tokToPair( TokEntry *tok) {
  CACHE_REGS

  Term ttok =  Yap_tokFullRep(tok);
HR[0] = ttok;
RESET_VARIABLE(HR+1);
  HR += 2;
  return AbsPair(HR-2);
}
/**
   @pred scan_stream( +Stream, -Tokens )
   Generate a list of tokens from a scan of the (input) stream, Tokens are of
   the form:

   + `atom`(Atom)
   + `<QQ>`(Text)
   + `number`(Number)
   + `var`(VarName)
   + `string`(String)
   + 'EOF''
   + symbols, including `(`, `)`, `,`, `;`

*/
static Int scan_stream(USES_REGS1) {
  int inp_stream;
  Term tout= TermNil;
  scanner_params params;
  StreamDesc *st;
  memset(&params, 0, sizeof(scanner_params));
  params.store_comments = true;
  /* needs to change LOCAL_output_stream for write */
  inp_stream = Yap_CheckTextStream(ARG1, Input_Stream_f, "read/3");
  if (inp_stream == -1) {
    return false;
  }
  st = GLOBAL_Stream+inp_stream;
  Term end = TermNil;
  LOCAL_tokptr = NULL;
  while (true) {
  LOCAL_ErrorMessage = NULL;
    if (st->status & (Past_Eof_Stream_f)) {
      post_process_eof(st);
      break;
    }
      TokEntry * t = 
      Yap_tokenizer(GLOBAL_Stream + inp_stream, &params);
    while (t) {
      if (t->TokSize==0) {
	t = t->TokNext;
	continue;
      }
      Term tt = tokToPair(t);
      if (tout == TermNil) {
	tout = tt;
	end = TailOfTerm(tout);
      } else {
	*VarOfTerm(end) = tt;
	end = TailOfTerm(tt);
      }
      t = t->TokNext;
    }
  Yap_clean_tokenizer();
  }
  if (end != TermNil)
    *VarOfTerm(end) = TermNil;

  LOCAL_tokptr = NULL;
  
  return Yap_unify(ARG2, tout);
}



#define MSG_SIZE 1024

/**
 * Syntax Error Handler
 *
 * @par tokptr: the sequence of tokens
 * @par sno: the stream numbet
 *
 * Implicit arguments:
 *    +
 */
char * Yap_syntax_error__(const char *file, const char *function, int lineno,Term t, int sno, TokEntry *start,
                       TokEntry *err, char *msg,  ...) {
  CACHE_REGS
#if HAVE_FTELLO
  offset_t opos;
#else
  long opos;
#endif

  va_list ap;
  char  o[MSG_SIZE+1];
  o[ 0] = '\0';
  va_start(ap,msg);
  if (msg) {
    vsnprintf(o, MSG_SIZE,msg,ap);
  }
  va_end(ap);
  TokEntry *tok = start, *end = err;
  StreamDesc
    *st = GLOBAL_Stream+sno;
  yap_error_descriptor_t *e;
  if (LOCAL_ActiveError) {
    e = LOCAL_ActiveError;
  } else {
    LOCAL_ActiveError = e = calloc(1,  sizeof(yap_error_descriptor_t));
  }
  if (sno<0) {
    //    Yap_JumpToEnv();   if (sno < 0 || start == NULL) {
    e->parserPos = 0;
    e->parserSize = 0;
    e->parserFile = "Prolog term";
    Yap_MkErrorRecord(e, file, function, lineno,SYNTAX_ERROR,t,TermEmpty,o   );
    return NULL;
  }
  if (st->stream_getc ==  Yap_popChar)
    st->stream_wgetc(sno);
  if (!err) {
    //    Yap_JumpToEnv();   if (sno < 0 || start == NULL) {
    e->parserPos = 0;
     e->prologConsulting = LOCAL_consult_level > 0;
     e->parserReadingCode = true;
     e->parserFirstLine =
       e->parserLine =  e->parserLastLine =st->linecount;
  e->parserSize = 1;
  e->parserFirstPos = 
    e->parserPos = st->charcount- 1; 
  e->parserLastPos = st->charcount;
  e->parserFirstLinePos = 
    e->parserLinePos = e->parserPos-st->linestart;
  e->parserLastLinePos = e->parserLastPos-st->linestart;
      const char *s =  RepAtom(AtomOfTerm(st->user_name))->StrOfAE;
    e->parserFile = strcpy(malloc(strlen(s)+1),s);  
    Yap_MkErrorRecord(e, file, function, lineno,SYNTAX_ERROR,t,TermEmpty,o);
   } else {
     const char *s =  RepAtom(AtomOfTerm(st->user_name))->StrOfAE;
     e->parserFile = strcpy(malloc(strlen(s)+1),s);  
     if (err  && err->TokNext) {
       while (end->TokNext && end->Tok != eot_tok) {
	 end = end->TokNext;
       }
     }else {
       end = err;
     }
     Int start_line = tok->TokLine;
     Int err_line = err->TokLine;
     Int end_line = end->TokLine;
     Int startpos = tok_pos(tok);
     Int errpos = tok_pos(err);
     Int errsize = err->TokSize;
     Int endpos = tok_pos(end);
     Int startlpos = tok->TokOffset;
     Int errlpos = err->TokOffset;
     Int endlpos = end->TokOffset+end->TokSize;
     if (endpos < errpos && st > 0) {
       endpos = errpos;
       endlpos = errlpos;
       end_line = endlpos;
     }
     o[0] = '\0';
     Yap_MkErrorRecord(LOCAL_ActiveError, file, function, lineno,SYNTAX_ERROR, MkIntTerm(err_line), TermNil, NULL);
     // const char *p1 =
     e->prologConsulting = LOCAL_consult_level > 0;
     e->parserReadingCode = true;
     e->parserFirstLine = start_line;
     e->parserLine = err_line;
  e->parserLastLine = end_line;
  e->parserSize = errsize;
  e->parserPos = errpos;
  e->parserSize = errsize;
  e->parserFirstPos = startpos;
  e->parserLastPos = endpos;
  e->parserLinePos = errlpos;
  e->parserFirstLinePos = startlpos;
  e->parserLastLinePos = endlpos;
  e->parserTextA = NULL;
  e->parserTextB = NULL;
  {
    Term nt;
    if ((nt = Yap_StreamUserName(sno))==0) {
      e->parserFile = "<<<"; //
    } else {
    const char *s =  RepAtom(Yap_source_file_name())->StrOfAE;
     e->parserFile = strcpy(malloc(strlen(s)+1),s);


     
    }
  }
  e->culprit = s;
  if (GLOBAL_Stream[sno].status & Seekable_Stream_f &&
            sno >= 0) {
    char *bufb, *bufa;
#if HAVE_FTELLO
opos = ftello(GLOBAL_Stream[sno].file);
#else
opos=	ftell(GLOBAL_Stream[sno].file);
#endif
    err_line = e->parserLine;
      Int msgstartpos = Yap_Max(startpos, errpos - 200);
      if (msgstartpos >= errpos) {
	bufb = NULL;
      } else {
#if HAVE_FTELLO
	fseeko(GLOBAL_Stream[sno].file, msgstartpos, SEEK_SET);
#else
	fseek(GLOBAL_Stream[sno].file, msgstartpos, SEEK_SET);
#endif
	fflush(GLOBAL_Stream[sno].file);
	bufb = malloc(1+errpos-msgstartpos);
	fread(bufb, errpos-msgstartpos, 1, GLOBAL_Stream[sno].file);
	bufb[errpos-msgstartpos] = '\0';
      }
      e->parserTextA = bufb;
      Int msgendpos = Yap_Min(opos,errpos+200);
      if (msgstartpos >= errpos) {
	bufa = NULL;
      } else {
#if HAVE_FTELLO
	fseeko(GLOBAL_Stream[sno].file, errpos, SEEK_SET);
#else
	fseek(GLOBAL_Stream[sno].file, errpos, SEEK_SET);
#endif
	fflush(GLOBAL_Stream[sno].file);
	bufa = malloc(1+msgendpos-errpos);
	fread(bufa, msgendpos-errpos, 1, GLOBAL_Stream[sno].file);
	bufa[msgendpos-errpos] = '\0';
      }
      e->parserTextB = bufa;
#if HAVE_FTELLO
	fseeko(GLOBAL_Stream[sno].file, opos, SEEK_SET);
#else
	fseek(GLOBAL_Stream[sno].file, opos, SEEK_SET);
#endif
  } else {
    char * buf = malloc(MSG_SIZE);
    buf[0]='\0';
    TokEntry *tok = start;
    while (tok &&  tok->Tok != eot_tok && tok != LOCAL_toktide) {
      const char *ns = Yap_tokText(tok);
      size_t esz = strlen(ns);
      if (ns && ns[0]) {
        if (esz + strlen(o) + 1 > MSG_SIZE - 256) {
	  break;

        }
        strcat(buf, ns);
        strcat(buf, " ");
	if (tok->TokNext && tok->TokNext->TokLine > tok->TokLine) {
          strcat(o, "\n");
        }
      }
      tok =  tok->TokNext;
    }
    size_t bufs = MSG_SIZE;
    e->parserTextA = realloc(buf, strlen(buf)+1);
    buf = malloc(bufs);
    buf[0]='\0';
  while (tok &&  tok->Tok != eot_tok) {
      const char *ns = Yap_tokText(tok);
      size_t esz = strlen(ns);
      if (ns && ns[0]) {
        while (esz + strlen(buf) + 1 > bufs - 256) {
	  buf = realloc(buf,bufs+MSG_SIZE);
	  bufs += MSG_SIZE;
        }
        strcat(buf, ns);
        strcat(buf, " ");
        if (tok->TokNext && tok->TokNext->TokLine > tok->TokLine) {
          strcat(o, "\n");
        }
      }
      tok = tok->TokNext;
      }
  e->parserTextB = realloc(buf, strlen(buf)+1);
}
  }

    if (st>= 0 && st->status & Past_Eof_Stream_f) {
      Yap_CloseStream(sno);
    }
      /* 0:  strat, error, end line */
  /*2 msg */
  /* 1: file */
  clean_vars(LOCAL_VarTable);
  clean_vars(LOCAL_AnonVarTable);
  LOCAL_VarTable = LOCAL_VarList = LOCAL_VarTail = LOCAL_AnonVarTable = NULL;
  return NULL;
}

typedef struct FEnv {
  scanner_params scanner; /// scanner interface
  Term qq, tp, sp, np, vprefix, scan;
  Term cmod;           /// initial position of the term to be read.
  Term t, t0;          /// the output term
  TokEntry *tokstart;  /// the token list
  TokEntry *toklast;   /// the last token
  CELL *old_H;         /// initial H, will be reset on stack overflow.
  xarg *args;          /// input args
  bool reading_clause; /// read_clause
  size_t nargs;        /// arity of current procedure
  encoding_t enc;      /// encoding of the stream being read
  char * msg;          /// Error  Messagge
  Term user_file_name; ///stream name inn case it closes
  int top_stream;      /// last open stream
} FEnv;

typedef struct renv {
  Term bq;
  bool ce, sw;
  Term sy;
  UInt cpos;
  int prio;
  int ungetc_oldc;
  int had_ungetc;
  bool seekable;
} REnv;

static xarg *setClauseReadEnv(Term opts, FEnv *fe, struct renv *re,
                              int inp_stream);
static xarg *setReadEnv(Term opts, FEnv *fe, struct renv *re, int inp_stream) {
  CACHE_REGS
  LOCAL_VarTable = LOCAL_VarList = LOCAL_VarTail = LOCAL_AnonVarTable = NULL;
  fe->enc = GLOBAL_Stream[inp_stream].encoding;
  xarg *args = Malloc(sizeof(xarg) * READ_END);
  memset(args, 0, sizeof(xarg) * READ_END);
  args = Yap_ArgListToVector(opts, read_defs, READ_END, args,
                             DOMAIN_ERROR_READ_OPTION);
  fe->top_stream = Yap_FirstFreeStreamD();
  fe->msg = NULL;
  if (args && args[READ_OUTPUT].used) {
    fe->t0 = args[READ_OUTPUT].tvalue;
  } else {
    fe->t0 = 0;
  }
  if (args && args[READ_MODULE].used) {
    fe->cmod = args[READ_MODULE].tvalue;
  } else {
    fe->cmod = CurrentModule;
    if (fe->cmod == TermProlog)
      fe->cmod = PROLOG_MODULE;
  }
  if (args && args[READ_BACKQUOTED_STRING].used) {
    fe->scanner.backquotes = args[READ_BACKQUOTED_STRING].tvalue;
  } else {
    fe->scanner.backquotes = getBackQuotesFlag(fe->cmod);
  }
  if (args && args[READ_DOUBLEQUOTED_STRING].used) {
    fe->scanner.doublequotes = args[READ_DOUBLEQUOTED_STRING].tvalue;
  } else {
    fe->scanner.doublequotes = getDoubleQuotesFlag(fe->cmod);
  }
  if (args && args[READ_SINGLEQUOTED_STRING].used) {
    fe->scanner.singlequotes = args[READ_SINGLEQUOTED_STRING].tvalue;
  } else {
    fe->scanner.singlequotes = getSingleQuotesFlag(fe->cmod);
  }
  if (args && args[READ_CHARACTER_ESCAPES].used) {
    fe->scanner.ce = args[READ_CHARACTER_ESCAPES].tvalue == TermTrue;
  } else {
    fe->scanner.ce = Yap_CharacterEscapes(fe->cmod) == TermTrue;
  }
  if (args && args[READ_VAR_PREFIX].used) {
    fe->scanner.vprefix = args[READ_VAR_PREFIX].tvalue == TermTrue;
  } else {
    fe->scanner.vprefix = false;
  }
  if (args && args[READ_INPUT_CLOSING_BLANK].used) {
    fe->scanner.get_eot_blank =
        args[READ_INPUT_CLOSING_BLANK].tvalue == TermTrue;
  } else {
    fe->scanner.get_eot_blank = false;
  }
  if (args && args[READ_ALLOW_VARIABLE_NAME_AS_FUNCTOR].used) {
    fe->scanner.vn_asfl =
        args[READ_ALLOW_VARIABLE_NAME_AS_FUNCTOR].tvalue == TermTrue;
  } else {
    fe->scanner.vn_asfl =
      trueGlobalPrologFlag(ALLOW_VARIABLE_NAME_AS_FUNCTOR_FLAG) == TermTrue;
  }
  if (args && args[READ_COMMENTS].used) {
    fe->scanner.store_comments = true;
    fe->scanner.ecomms = args[READ_COMMENTS].tvalue;
  } else {
    fe->scanner.store_comments = false;
    fe->scanner.ecomms = TermNil;
  }
  if (args && args[READ_QUASI_QUOTATIONS].used) {
    fe->qq = args[READ_QUASI_QUOTATIONS].tvalue;
  } else {
    fe->qq = 0;
  }
  if (args && args[READ_TERM_POSITION].used) {
    fe->tp = args[READ_TERM_POSITION].tvalue;
  } else {
    fe->tp = 0;
  }
  if (args && args[READ_SINGLETONS].used) {
    fe->sp = args[READ_SINGLETONS].tvalue;
    Term *tp;
    if (!IsVarTerm(fe->sp) &&
	(Yap_SkipList(&fe->sp, &tp)<1 || *tp != TermNil)) {
      Yap_ThrowError(DOMAIN_ERROR_READ_OPTION, Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("singletons"),1),1,&fe->sp),
                     "type error in singletons/1");
    }
 } else {
    fe->sp = 0;
  }
  if (args && args[READ_SYNTAX_ERRORS].used) {
    re->sy = args[READ_SYNTAX_ERRORS].tvalue;
  } else {
    re->sy = getYapFlag( MkAtomTerm(AtomSyntaxErrors) );
  }
  if (args && args[READ_VARIABLES].used) {
    fe->vprefix = args[READ_VARIABLES].tvalue;
    Term *tp;
    if (!IsVarTerm(fe->vprefix) &&
	(Yap_SkipList(&fe->vprefix, &tp)<1 || *tp != TermNil)) {
      Yap_ThrowError(DOMAIN_ERROR_READ_OPTION, Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("variables"),1),1,&fe->vprefix),
                     "type error in variables/1");
    }
  } else {
    fe->vprefix = 0;
  }
  if (args && args[READ_VARIABLE_NAMES].used) {
    fe->np = args[READ_VARIABLE_NAMES].tvalue;
    Term *tp;
    if (!IsVarTerm(fe->np) &&
	(Yap_SkipList(&fe->np,&tp)<1 || *tp != TermNil)) {
      Yap_ThrowError(DOMAIN_ERROR_READ_OPTION, Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("variable_names"),1),1,&fe->np),
                     "type error in variable_names/1");
      }
  } else {
    fe->np = 0;
  }
  if (args && args[READ_SCAN].used) {
    fe->scan = args[READ_SCAN].tvalue;
  } else {
    fe->scan = 0;
  }
  re->seekable = (GLOBAL_Stream[inp_stream].status & Seekable_Stream_f) != 0;
  if (re->seekable) {
    re->cpos = GLOBAL_Stream[inp_stream].charcount;
  }
  if (args && args[READ_PRIORITY].used) {
    re->prio = IntegerOfTerm(args[READ_PRIORITY].tvalue);
    if (re->prio > GLOBAL_MaxPriority) {
      Yap_ThrowError(DOMAIN_ERROR_OPERATOR_PRIORITY, opts,
                     "max priority in Prolog is %d, not %ld",
                     GLOBAL_MaxPriority, re->prio);
    }
  } else {
    re->prio = LOCAL_default_priority;
  }
  return args;
}

typedef enum {
  YAP_START_PARSING,  /// initialization
  YAP_SCANNING,       /// input to list of tokens
  YAP_SCANNING_ERROR, /// serious error (eg oom); trying error handling, followd
  /// by throw, restart or failure
  YAP_PARSING,         /// list of tokens to term
  YAP_PARSING_ERROR,   /// oom or syntax error
  YAP_PARSING_FINISHED /// exit parser
} parser_state_t;

Int Yap_FirstLineInParse(void) {
  CACHE_REGS
  return LOCAL_StartLineCount;
}

#define PUSHFET(X) *HR++ = fe->X
#define POPFET(X) fe->X = *--HR

static void reset_regs(TokEntry *tokstart, FEnv *fe) {
  CACHE_REGS

  restore_machine_regs();

  /* restart global */
  PUSHFET(qq);
  PUSHFET(tp);
  PUSHFET(sp);
  PUSHFET(np);
  PUSHFET(vprefix);
  PUSHFET(t);
  HR = fe->old_H;
  memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError));
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  //    Yap_growstack_in_parser();
  POPFET(t);
  POPFET(vprefix);
  POPFET(np);
  POPFET(sp);
  POPFET(qq);
}

static Term get_variables(FEnv *fe, TokEntry *tokstart) {
  CACHE_REGS
  Term v;

  if (fe->vprefix) {
    while (true) {
      fe->old_H = HR;
      if (setjmp(LOCAL_IOBotch) == 0) {
        if ((v = Yap_Variables(LOCAL_VarList, TermNil))) {
          fe->old_H = HR;
          return v;
        }
      } else {
        reset_regs(tokstart, fe);
      }
    }
  }
  return 0;
}

static Term get_varnames(FEnv *fe, TokEntry *tokstart) {
  CACHE_REGS
  Term v;

  if (fe->np) {
    while (true) {
      fe->old_H = HR;

      if (setjmp(LOCAL_IOBotch) == 0) {
        if ((v = Yap_VarNames(LOCAL_VarList, TermNil))) {
          fe->old_H = HR;
	  //	  GlobalEntry *ge = GetGlobalEntry(AtomNameVariables PASS_REGS);
	  //MaBind(&ge->global, v);
          return v;
        }
      } else {
        reset_regs(tokstart, fe);
      }
    }
  }
  return 0;
}

static Term get_singletons(FEnv *fe, bool var_only, TokEntry *tokstart) {
  CACHE_REGS
  Term v;

  if (fe->sp) {
    while (TRUE) {
      fe->old_H = HR;

      if (setjmp(LOCAL_IOBotch) == 0) {
        if ((v = Yap_Singletons(LOCAL_VarList, var_only, TermNil))) {
          return v;
        }
      } else {
         reset_regs(tokstart, fe);
      }
    }
  }
  return 0;
}


static void warn_singletons(FEnv *fe, int sno, TokEntry *tokstart) {
  CACHE_REGS
  fe->sp = TermNil;
  Term vn = get_singletons(fe, false, tokstart);
  while (vn && vn != TermNil) {
    int line,col;
    char *fil;
    Term myv = ArgOfTerm(1,HeadOfTerm(vn));
	line = IntOfTerm(HeadOfTerm(TailOfTerm(myv)));
     col = IntOfTerm(HeadOfTerm(TailOfTerm(TailOfTerm(myv))));
     fil  = RepAtom(AtomOfTerm(HeadOfTerm(TailOfTerm(TailOfTerm(TailOfTerm(myv))))))->StrOfAE;
    yap_error_descriptor_t *e = LOCAL_ActiveError;
    Yap_MkErrorRecord(e, __FILE__, __FUNCTION__, __LINE__, WARNING_SINGLETONS,
                      myv, TermNil, "singletons warning");
			    Term ts[3], sc[2];
    ts[0] = MkAtomTerm(Yap_LookupAtom("singletons"));
    ts[1] = myv;
    ts[2] = fe->t;
    sc[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomStyleCheck,3),3,ts);
    sc[1] = MkSysError(e);
   if (sno < 0) {
     
    e->parserPos = 0;
    //    Yap_JumpToEnv();
   } else {
     e->parserLine = line;
     e->parserPos = col;
     e->parserSize = strlen(RepAtom(AtomOfTerm(myv))->StrOfAE);
     e->parserFile = fil;
     }
   Yap_PrintWarning(Yap_MkApplTerm(FunctorError, 2, sc),TermWarning);
        vn = TailOfTerm(vn);
}
}


		      

static Term get_stream_position(FEnv *fe, TokEntry *tokstart) {
  CACHE_REGS
  Term v;

    

  if (fe->tp) {
    while (true) {
      fe->old_H = HR;

      if (setjmp(LOCAL_IOBotch) == 0) {
        if ((v = CurrentPositionToTerm())) {
          return v;
        }
      } else {
        reset_regs( tokstart, fe);
      }
    }
  }
  return 0;
}
static bool is_goal(Term t)
{
  if (!IsApplTerm(t))
    return false;
  Functor f = FunctorOfTerm(t);
return f == FunctorQuery || f == FunctorAssert1;
}

static Term scan_to_list(TokEntry * t)
{
  Term   vs = TermNil;
  Term end;
  while (t) {
    Term tt = tokToPair(t);
    if (vs == TermNil) {
      vs = tt;
      end = TailOfTerm(vs);
    } else {
      *VarOfTerm(end) = tt;
      end = TailOfTerm(tt);
    }
    t = t->TokNext;
  }
  return vs;
}


 static bool complete_processing(FEnv *fe, int sno, TokEntry *tokstart) {
  CACHE_REGS
    Term v1=0, v2=0, v3=0, vs =  0;

  if (fe->t0 && fe->t && !(Yap_unify(fe->t, fe->t0)))
    return false;

  if (fe->t && fe->vprefix)
    v1 = get_variables(fe, tokstart);
  else
    v1 = 0L;
  if (fe->t && fe->np)
    v2 = get_varnames(fe, tokstart);
  else
    v2 = 0L;
  if (fe->t && fe->sp)
    v3 = get_singletons(fe, true, tokstart);
  else
    v3 = 0L;
  vs = fe->scanner.stored_scan;
  Term tpos = get_stream_position(fe, tokstart);
  Yap_clean_tokenizer();

  if (LOCAL_ParserAuxBase) {

    LOCAL_ParserAuxBase = NULL;
  }
  // trail must be ok by now.]
  if (fe->t) {
    return (!v1 || Yap_unify(v1, fe->vprefix)) &&
           (!v2 || Yap_unify(v2, fe->np)) &&
      (!v3 || Yap_unify(v3, fe->sp)) &&
(! vs ||  !fe->scan || Yap_unify(vs, fe->scan)) &&
                 (!fe->tp || Yap_unify(fe->tp, tpos));
  }
  return true;
}

 static bool complete_clause_processing(FEnv *fe, int sno, TokEntry *tokstart) {
  CACHE_REGS
    Term v_vprefix, v_vnames, v_pos, vs=0;

  if (fe->t) {
    if (fe->vprefix)
      v_vprefix = get_variables(fe, tokstart);
    else
      v_vprefix = 0L;
    v_vnames = get_varnames(fe, tokstart);
    vs = fe->scanner.stored_scan;
    v_pos =get_stream_position(fe, tokstart);
      Term t,state = MkPairTerm(
        fe->t,
        MkPairTerm(v_vnames,
                   MkPairTerm(StreamName(sno),
                              MkPairTerm(v_pos, TermNil))));
    Yap_SetBacktrackableGlobalVal(AtomCurrentClause,(t=MkVarTerm()) PASS_REGS);
    YapBind(VarOfTerm(t),state);
    //    Yap_DebugPlWriteln(state);
  } else {
    v_pos = 0;
    v_vnames = 0;
  }
   Yap_clean_tokenizer();

  // trail must be ok by now.]
  if (fe->t) {
    return (!fe->vprefix || Yap_unify(v_vprefix, fe->vprefix)) &&
      (!fe->scanner.store_comments
       || Yap_unify(fe->scanner.tcomms, fe->scanner.ecomms)) &&
      (!fe->np || Yap_unify(v_vnames, fe->np)) &&
      (!fe->scan ||   Yap_unify(vs, fe->scan)) &&
      (!fe->tp || Yap_unify(v_pos, fe->tp)) ;
  }
  return true;
}

static parser_state_t initparser(Term opts, FEnv *fe, REnv *re, int inp_stream,
                                 bool clause);

static parser_state_t parse(REnv *re, FEnv *fe, int inp_stream);

static parser_state_t scanError(REnv *re, FEnv *fe, int inp_stream);


static parser_state_t scan(REnv *re, FEnv *fe, int inp_stream);

static parser_state_t scanEOF(FEnv *fe, int inp_stream, TokEntry *tokstart) {
  CACHE_REGS
  // bool store_comments = false;

  // check for an user abort
  if (tokstart != NULL && tokstart->Tok != Ord(eot_tok)) {
    /* we got the end of file from an abort */
    if (fe->msg && fe->msg[0] && !strcmp(fe->msg, "Abort")) {
      fe->t = 0L;
      Yap_clean_tokenizer();
      return YAP_PARSING_FINISHED;
    }
    // a :- <eof>
    
  /* if (LOCAL_tokptr->Tok == eot_tok && LOCAL_tokptr->TokNext == NULL && */
  /*     LOCAL_tokptr->TokInfo != TermEof) { */
  /*   fe->msg = ". is end-of-term?"; */
  /*   return YAP_PARSING_ERROR; */
  /* } */
    if (GLOBAL_Stream[inp_stream].status & Past_Eof_Stream_f) {
      fe->msg = "parsing stopped at a end-of-file";
      Yap_CloseStream(inp_stream);
      Yap_ThrowError(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,Yap_MkStream(inp_stream),NULL); 
    }
    /* we need to force the next read to also give end of file.*/
    GLOBAL_Stream[inp_stream].status |= Push_Eof_Stream_f;
    fe->msg = "end of file found before end of term";
    return YAP_PARSING_FINISHED;
  } else {
    // <eof>
    // return end_of_file
    Yap_clean_tokenizer();
    fe->t = MkAtomTerm(AtomEof);
    if (fe->np && !Yap_unify(TermNil, fe->np))
      fe->t = 0;
    if (fe->sp && !Yap_unify(TermNil, fe->sp))
      fe->t = 0;
    if (fe->vprefix && !Yap_unify(TermNil, fe->vprefix))
      fe->t = 0;
    if (fe->tp && !Yap_unify(fe->tp, StreamPosition(inp_stream)))
      fe->t = 0;
#if DEBUG
    if (GLOBAL_Option['p' - 'a' + 1]) {
      fprintf(stderr, "[ end_of_file %p ]\n", GLOBAL_Stream[inp_stream].name);
    }
#endif
    return YAP_PARSING_FINISHED;
  }
}

static parser_state_t initparser(Term opts, FEnv *fe, REnv *re, int inp_stream,
                                 bool clause) {
  CACHE_REGS
    fe->t0=fe->t=0;
  fe->user_file_name = Yap_StreamUserName(inp_stream);
  fe->tp = StreamPosition(inp_stream);
  fe->reading_clause = clause;

  fe->top_stream = inp_stream+1;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  LOCAL_SourceFileName = AtomOfTerm(fe->user_file_name);
  LOCAL_eot_before_eof = false;
  if (clause) {
    fe->args = setClauseReadEnv(opts, fe, re, inp_stream);
  } else {
    fe->args = setReadEnv(opts, fe, re, inp_stream);
  }
  if (fe->args == NULL) {
    if (LOCAL_Error_TYPE == DOMAIN_ERROR_OUT_OF_RANGE)
      LOCAL_Error_TYPE = TYPE_ERROR_READ_TERM;
    if (LOCAL_Error_TYPE)
      Yap_ThrowError(LOCAL_Error_TYPE, opts, NULL);
    fe->t = 0;
    return YAP_PARSING_FINISHED;
    ;
  }
  if (!fe->args) {
    return YAP_PARSING_FINISHED;
  }
  fe->old_H = HR;
  fe->msg = NULL;
  return YAP_SCANNING;
}

static Term add_comment(FEnv *fe,Term comms, Term v){
if (fe->scanner.store_comments) {
    Term new =  Yap_MkNewPairTerm();
       if (comms == TermNil)
	 fe->scanner.tcomms = new;
       else
	 *VarOfTerm((comms))= new;
       RepPair(new)[0] = v;
       return TailOfTerm(new);
     }
return comms;
   }

static parser_state_t scan(REnv *re, FEnv *fe, int sno) {
  CACHE_REGS
  /* preserve   value of H after scanning: otherwise we may lose strings
     and floats */
    if (GLOBAL_Stream[sno].status & Free_Stream_f) {
      fe->t = 0;
      LOCAL_tokptr = NULL;
      Yap_ThrowError(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,MkIntTerm(sno),"trying to scan a closed file");
      return  YAP_PARSING_FINISHED;
    }
    LOCAL_tokptr = 
      Yap_tokenizer(GLOBAL_Stream + sno, &fe->scanner);
#if DEBUG
  if (GLOBAL_Option[2]) {
    TokEntry *t = LOCAL_tokptr;
    int n = 0;
    while (t) {
      fprintf(stderr, "[Token %d %s %d]", Ord(t->Tok), Yap_tokText(t), n++);
      t = t->TokNext;
    }
    fprintf(stderr, "\n");
  }
#endif
  if (fe->msg)
    return YAP_SCANNING_ERROR;
  if (fe->scan)
    fe->scanner.stored_scan = scan_to_list(LOCAL_tokptr);
  else
    fe->scanner.stored_scan = TermNil;
   Term comms = fe->scanner.tcomms = TermNil;



   while (LOCAL_tokptr && LOCAL_tokptr->Tok == Ord(Comment_tok)) {
    comms = add_comment(fe,comms, LOCAL_tokptr->TokInfo);
    LOCAL_tokptr=LOCAL_tokptr->TokNext;
  }
   TokEntry *tokstart = LOCAL_tokptr;

 while (tokstart->TokNext) {
     if (tokstart->TokNext->Tok == Ord(Comment_tok)) {
           comms = add_comment(fe,comms, tokstart->TokNext->TokInfo);
    tokstart->TokNext=tokstart->TokNext->TokNext;
  } else
     tokstart=tokstart->TokNext;
 }
 
 if (comms != TermNil) {
   *(CELL*)((comms))= TermNil;
 }
 // LOCAL_tokptr=tokstart;
 
     if (LOCAL_tokptr->Tok != Ord(eot_tok)) {
    // next step
    return YAP_PARSING;
  }
  return scanEOF(fe, sno, tokstart);
}


static parser_state_t parseError(REnv *re, FEnv *fe, int inp_stream) {
  CACHE_REGS
  fe->t = 0;

  HR = fe->old_H;
  yap_error_number err = LOCAL_Error_TYPE;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  LOCAL_ErrorMessage = NULL;
  if (err == RESOURCE_ERROR_STACK) {
    while (!Yap_dogc(PASS_REGS1)) {
      Yap_ThrowError(RESOURCE_ERROR_STACK, MkStringTerm("read_term"), NULL);
      RECOVER_H();
      return 0L;
    }
    return YAP_START_PARSING;
  } else if (err == RESOURCE_ERROR_HEAP)
    {
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_ThrowError(RESOURCE_ERROR_HEAP, MkStringTerm("read_term"), NULL);
      RECOVER_H();
      return 0L;
    }
    return YAP_START_PARSING;
  } else if (err == RESOURCE_ERROR_TRAIL) {
    if (!Yap_growtrail(0, FALSE)) {
      Yap_ThrowError(RESOURCE_ERROR_HEAP, MkStringTerm("read_term"), NULL);
      RECOVER_H();
      return 0L;
    }
    return YAP_START_PARSING;
  }

  if (err != SYNTAX_ERROR && err != YAP_NO_ERROR) {

    Yap_ThrowError(err, MkStringTerm("read_term"), NULL);
      RECOVER_H();
      return 0L;
  }
  Term cause;
  if (LOCAL_ErrorMessage && LOCAL_ErrorMessage[0]) {
    size_t len = strlen(LOCAL_ErrorMessage);
    fe->msg = malloc(len+1);
    strncpy(fe->msg, LOCAL_ErrorMessage, len);
    cause = MkAtomTerm(Yap_LookupAtom(fe->msg));
  } else {
    cause = TermEmpty;
    fe->msg = "";
  }
  Yap_syntax_error__(__FILE__, __FUNCTION__, __LINE__,
                     cause, inp_stream, (LOCAL_tokptr), (LOCAL_toktide),
                     fe->msg);
  Term action = re->sy;
  if (action == TermFail || action == TermDec10) {
    Term sc[2];
    sc[0] = MkAtomTerm(Yap_LookupAtom(LOCAL_ActiveError->culprit));
    sc[0] = Yap_MkApplTerm(FunctorShortSyntaxError,1,sc);
    sc[1] = MkSysError(LOCAL_ActiveError);
    Yap_PrintWarning(Yap_MkApplTerm(Yap_MkFunctor(AtomError, 2), 2, sc),TermError);
   if (action == TermFail)
     return  YAP_PARSING_FINISHED;   else
     return  YAP_START_PARSING;
  } else {
    Yap_RaiseException();
  } 
  return YAP_PARSING_FINISHED;
}

static parser_state_t scanError(REnv *re, FEnv *fe, int inp_stream) {
    return parseError(re,fe,inp_stream);
  CACHE_REGS
  fe->t = 0;
  HR = fe->old_H;

  fflush(NULL);
  Yap_clearInput(inp_stream);
  // running out of memory
  yap_error_number err = LOCAL_Error_TYPE;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  if (err == RESOURCE_ERROR_TRAIL) {
    if (!Yap_growtrail(sizeof(CELL) * K16, FALSE)) {
      Yap_CloseTemporaryStreams(fe->top_stream);
      return YAP_PARSING_FINISHED;
    }
  } else if (err == RESOURCE_ERROR_AUXILIARY_STACK) {
    if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
      Yap_CloseTemporaryStreams(fe->top_stream);
      return YAP_PARSING_FINISHED;
    }
  } else if (err == RESOURCE_ERROR_HEAP) {
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_CloseTemporaryStreams(fe->top_stream);
      return YAP_PARSING_FINISHED;
    }
  } else if (err == RESOURCE_ERROR_STACK) {
    if (!Yap_dogc(PASS_REGS1)) {
      Yap_CloseTemporaryStreams(fe->top_stream);
      return YAP_PARSING_FINISHED;
    }
  }
  // go back to the start
  if (err == SYNTAX_ERROR) {
    return YAP_PARSING_ERROR;
  }
  if (re->seekable) {
    if (GLOBAL_Stream[inp_stream].status & InMemory_Stream_f) {
      GLOBAL_Stream[inp_stream].u.mem_string.pos = re->cpos;
    } else if (GLOBAL_Stream[inp_stream].status) {
#if HAVE_FTELLO
      fseeko(GLOBAL_Stream[inp_stream].file, re->cpos, 0L);
#else
      fseek(GLOBAL_Stream[inp_stream].file, re->cpos, 0L);
#endif
    }
  }
  return YAP_SCANNING;
}

static parser_state_t parse(REnv *re, FEnv *fe, int inp_stream) {
  CACHE_REGS
    TokEntry *tokstart;

LOCAL_toktide= tokstart = LOCAL_tokptr;
    LOCAL_StartLineCount = LOCAL_tokptr->TokLine;
 fe->tokstart = tokstart;
  fe->t = Yap_Parse(re->prio, fe->enc, fe->cmod);
  fe->toklast = LOCAL_tokptr;
  LOCAL_tokptr = tokstart;
#if EMACS
  first_char = tokstart->TokPos;
#endif /* EMACS */
  if (LOCAL_Error_TYPE != YAP_NO_ERROR || fe->t == 0)
    return YAP_PARSING_ERROR;
  return YAP_PARSING_FINISHED;
}

/** dome with the parser, off we go...
 */
static Term exit_parser(int sno, yhandle_t y0, yap_error_descriptor_t *new,
                        yap_error_descriptor_t *old, Term rc) {
  CACHE_REGS
  Yap_CloseHandles(y0);

  if (old) {
    LOCAL_ActiveError = old;
    if (old->errorNo != YAP_NO_ERROR)
      LOCAL_PrologMode |= InErrorMode;
  }
  return rc;
}

/**
 * @brief generic routine to read terms from a stream
 *
 *
 * @arg inp_stream: where we read from
 * @arg: opts, a list with options
 * @arg: if called from read_term, arity
 *  called from read_clause, -arity
 *
 * @return the term or 0 in case of error.
 *
 * Implementation uses a state machine: default is init, scan, parse, complete.
 *
 *
 */
Term Yap_read_term(int sno, Term opts, bool clause) {
    CACHE_REGS

      yap_error_descriptor_t new0, *new =&new0, *old = NULL;
#if EMACS
  int emacs_cares = FALSE;
#endif
  Term rc;
  parser_state_t state = YAP_START_PARSING;
  yhandle_t yopts = Yap_PushHandle(opts);
  yhandle_t y0 = yopts;
  FEnv fe0, *fe = &fe0;
  REnv re0, *re = &re0;
  while (true) {
    switch (state) {
    case YAP_START_PARSING:
      opts = Yap_GetFromHandle(yopts);
      state = initparser(opts, fe, re, sno, clause);
      if (state == YAP_PARSING_FINISHED)
        return exit_parser(sno, y0, new, old, 0);
      break;

    case YAP_SCANNING:
      state = scan(re, fe, sno);
      break;

    case YAP_SCANNING_ERROR:
      state = scanError(re, fe, sno);
      break;

    case YAP_PARSING:
      state = parse(re, fe, sno);
      break;

    case YAP_PARSING_ERROR:
      state = parseError(re, fe, sno);
      break;

    case YAP_PARSING_FINISHED: {
      CACHE_REGS
      bool done;
      if (clause) {
	if (fe->t && fe->reading_clause &&
      !is_goal(fe->t)  &&
      trueGlobalPrologFlag(SINGLE_VAR_WARNINGS_FLAG)) {
  }
  warn_singletons(fe, sno, LOCAL_tokptr);
  if (fe->t0 && fe->t && !Yap_unify(fe->t, fe->t0))
    return false;
        if (fe->t0 && fe->t && !Yap_unify(fe->t, fe->t0))
	  return false;
      done = complete_clause_processing(fe,sno, LOCAL_tokptr);
      }
      else
        done = complete_processing(fe, sno, LOCAL_tokptr);
      if (!done) {
        rc = fe->t = 0;
      }
#if EMACS
      first_char = tokstart->TokPos;
#endif /* EMACS */
      rc = fe->t;
      rc = exit_parser(sno, y0, new, old, rc);
      return rc;
    }
    }
  }

  return exit_parser(sno, y0, new, old, rc);
}

static Int
read_term2(USES_REGS1) /* '$read'(+Flag,?Term,?Module,?Vars,-Pos,-Err) */
{
  return Yap_read_term(LOCAL_c_input_stream, add_output(ARG1, ARG2), false) !=
         0;
}

/**
* @pred read_term(+Stream, -Term, +Flags)

* Reads term T from the  input stream Stream with execution
controlled by the following options:


 term_position(-@var{Position})
Unify @var{Position} with a term describing the position of the stream
at the start of parse. Use stream_position_data/3 to obtain extra
information.

+ singletons(_Names_)
Unify Names with a list of the form `Name=Var`, where
_Name_ is the name of a non-anonymous singleton variable in the
original term, and _Var_ is the variable's representation in
YAP.

+ syntax_errors(+_Val_)
Control action to be taken after syntax errors. See set_prolog_flag/2
for detailed information.

+ variable_names(-_Names_)
Unify Names with a list of the form `Name=Var` , where Name is
the name of a non-anonymous variable in the original term, and Var
is the variable's representation in Y

+  variables(-Names)
Unify Names with a list of the variables in term @var{T}.

*
*/
static Int read_term(
    USES_REGS1) /* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
{
  int sno;
  Term out;

  /* needs to change LOCAL_output_stream for write */

  sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "read/3");
  if (sno == -1) {
    return (FALSE);
  }
  out = Yap_read_term(sno, add_output(ARG2, ARG3), false);
  return out != 0L;
}

static xarg *setClauseReadEnv(Term opts, FEnv *fe, struct renv *re, int sno) {
  CACHE_REGS

  LOCAL_VarTable = LOCAL_VarList = LOCAL_VarTail = LOCAL_AnonVarTable = NULL;
  xarg *args = Malloc(sizeof(xarg) * READ_END);
  memset(args, 0, sizeof(xarg) * READ_END);
  args = Yap_ArgListToVector(opts, read_defs, READ_END, args,
                             DOMAIN_ERROR_READ_OPTION);
  memset(fe, 0, sizeof(*fe));
  fe->reading_clause = true;
  if (args && args[READ_OUTPUT].used) {
    fe->t0 = args[READ_OUTPUT].tvalue;
  } else {
    fe->t0 = 0;
  }
  if (args && args[READ_MODULE].used) {
    fe->cmod = args[READ_MODULE].tvalue;
  } else {
    fe->cmod = LOCAL_SourceModule;
    if (fe->cmod == TermProlog)
      fe->cmod = PROLOG_MODULE;
  }
  fe->scanner.backquotes = getBackQuotesFlag(fe->cmod);
  fe->scanner.singlequotes = getSingleQuotesFlag(fe->cmod);
  fe->scanner.doublequotes = getDoubleQuotesFlag(fe->cmod);
  fe->enc = GLOBAL_Stream[sno].encoding;
  fe->sp = 0;
  fe->qq = 0;
  if (args && args[READ_TERM_POSITION].used) {
    fe->tp = args[READ_TERM_POSITION].tvalue;
  } else {
    fe->tp = 0;
  }
  fe->sp = 0;
  if (args && args[READ_COMMENTS].used) {
    fe->scanner.store_comments = (args[READ_COMMENTS].used);
    fe->scanner.ecomms = args[READ_COMMENTS].tvalue;
  } else {
    fe->scanner.store_comments = false;
     fe->scanner.ecomms = TermNil;
  }
  if (args && args[READ_SYNTAX_ERRORS].used) {
    re->sy = args[READ_SYNTAX_ERRORS].tvalue;
  } else {
    re->sy = TermDec10;
  }
  fe->vprefix = 0;
  if (args && args[READ_VARIABLE_NAMES].used) {
    fe->np = args[READ_VARIABLE_NAMES].tvalue;
  } else {
    fe->np = 0;
  }
  if (args && args[READ_VARIABLES].used) {
    fe->vprefix = args[READ_VARIABLES].tvalue;
  } else {
    fe->vprefix = 0;
  }
  if (args && args[READ_SCAN].used) {
    fe->scan = args[READ_SCAN].tvalue;
  } else {
    fe->scan = 0;
  }
  fe->scanner.ce = Yap_CharacterEscapes(fe->cmod);
  re->seekable = (GLOBAL_Stream[sno].status & Seekable_Stream_f) != 0;
  if (re->seekable) {
    re->cpos = GLOBAL_Stream[sno].charcount;
  }
  re->prio = LOCAL_default_priority;
  LOCAL_ErrorMessage = NULL;
  return args;
}

/**
 * @pred read_clause( +Stream, -Clause, ?Opts) is det
 *
 x * Same as read_clause/3, but from the standard input stream.
 *
 */
static Int read_clause2(USES_REGS1) {
  Term ctl = add_output(ARG1, ARG2);

  return Yap_read_term(LOCAL_c_input_stream, ctl, true);
}

/**
 * @pred read_clause( +Stream, -Clause, ?Opts) is det
 *
 * This predicate receives a set of options _OPts_ based on read_term/3, but
 * specific
 * to readin clauses. The following options are considered:
 *
 *   + The `comments` option unifies its argument with the comments in the
 * term,
 *     represented as strings
 *   + The `process_comments` option calls a hook, it is current ignored by
 * YAP.
 *   + The `term_position` unifies its argument with a term describing the
 *     position of the term.
 *   + The `syntax_errors` flag controls response to syntactic errors, the
 * default is `dec10`.
 *
 * The next two options are called implicitly:
 *
 *   + The `module` option is initialized to the current source module, by
 * default.
 *   + The `singletons` option is set from the single var flag
 */
static Int read_clause(
    USES_REGS1) /* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
{
  int sno;
  Term out;

  /* needs to change LOCAL_output_stream for write */
  sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "read/3");
  if (sno < 0)
    return false;
  out = Yap_read_term(sno, add_output(ARG2, ARG3), true);
  return out != 0;
}

/**
 * start input for a meta-clause. Should obtain:
 *   - predicate name
 *   - predicate arity
 *   - address for 256 cluses.
 *
 * @param  ARG1 input stream
 * @param  ARG2 Adress of predicate.
 * @param  ARG3 Term read.
 * @return            [description]
 */
#if 0
static Int start_mega(USES_REGS1)
{
  int sno;
  Term out;
  Term t3 = Deref(ARG3);
  /* needs to change LOCAL_output_stream for write */
  sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "read_exo/3");
  if (sno < 0)
    return false;
  yhandle_t y0 = Yap_StartHandles();
  yhandle_t h = Yap_InitSlot(ARG2);
  TokENtry *tok;
  arity_t srity = 0;


  /* preserve   value of H after scanning: otherwise we may lose strings
     and floats */
  LOCAL_tokptr = LOCAL_toktide =
    Yap_tokenizer(GLOBAL_Stream + sno, fe->scanner);
  if (fe->scan) {
    fe->scanner.stored_scan = scan_to_list(LOCAL_tokptr);
  }
    LOCAL_StartLineCount = tokptr->TokLine;
  if (tokptr->Tok == Name_tok && (next = tokptr->TokNext) != NULL &&
      next->Tok == Ponctuation_tok && next->TokInfo == TermOpenBracket)
    {
      bool start = true;
      while ((tokptr = next->TokNext))
	{
	  if (IsAtomOrIntTerm(t = fe->tp))
\     	    {
	      ip->opc = Yap_opcode(get_atom);
	      ip->y_u.x_c.c = t.
		ip->y_u.x_c.x = fe->tp++; / ()c * /
					      }
	  else if (IsAtomOrIntTerm(t = *tp))
	    {
	      (IsAtom(tok->Tokt) || IsIntTerm(XREGS + (i + 1)))extra[arity]
                ]
	    }
    }
}
Yap_CloseHandles(y0);
}
#endif

/**
 * @pred source_location( - _File_ , _Line_, _Offset_ )
 *
 * unify  _File_ and  _Line_ wuth the position of the last term read, if the
 * term
 * comes from a stream created by opening a file-system path with open/3 and
 * friends.
 * It ignores user_input or
 * sockets.
 *
 * @param - _File_
 * @param - _Line_
 *
 *
 *
 * @note SWI-Prolog built-in.
 */
static Int source_location(USES_REGS1) {
  return Yap_unify(ARG1, MkAtomTerm(LOCAL_SourceFileName)) &&
         Yap_unify(ARG2, MkIntegerTerm(LOCAL_SourceFileLineno));
}

/**
 * @pred read(+ Stream, -Term ) is iso
 *
 * Reads term  _T_ from the stream  _S_ instead of from the current input
 * stream.
 *
 * @param - _Stream_
 * @param - _Term_
 *
 */
static Int
read2(USES_REGS1) /* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
{
  int sno;
  Int out;

  /* needs to change LOCAL_output_stream for write */
  sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "read/3");
  if (sno == -1) {
    return (FALSE);
  }
  out = Yap_read_term(sno, add_output(ARG2, TermNil), false);
  Yap_RaiseException();
  return out;
}

/** @pred  read(- T) is iso

    Reads the next term from the current input stream, and unifies it with
    _T_. The term must be followed by a dot (`.`) and any blank-character
    as previously defined. The syntax of the term must match the current
    declarations for operators (see op). If the end-of-stream is reached,
    _T_ is unified with the atom `end_of_file`. Further reads from of
    the same stream may cause an error failure (see open/3).

*/
static Int
read1(USES_REGS1) /* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
{
  Int out =
      Yap_read_term(LOCAL_c_input_stream, add_output(ARG1, TermNil), false);
  Yap_RaiseException();
  return out;
}

static Int style_checker(USES_REGS1) {
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Term t = TermNil;
    if (getYapFlag(MkAtomTerm(AtomSingleVarWarnings)) == TermTrue) {
      t = MkPairTerm(MkAtomTerm(AtomSingleVarWarnings), t);
    }
    if (getYapFlag(MkAtomTerm(AtomDiscontiguousWarnings)) == TermTrue) {
      t = MkPairTerm(MkAtomTerm(AtomDiscontiguousWarnings), t);
    }
    if (getYapFlag(MkAtomTerm(AtomRedefineWarnings)) == TermTrue) {
      t = MkPairTerm(MkAtomTerm(AtomRedefineWarnings), t);
    }
  } else {
    while (IsPairTerm(t)) {
      Term h = HeadOfTerm(t);
      t = TailOfTerm(t);

      if (IsVarTerm(h)) {
        Yap_ThrowError(INSTANTIATION_ERROR, t, "style_check/1");
        return (FALSE);
      } else if (IsAtomTerm(h)) {
        Atom at = AtomOfTerm(h);
        if (at == AtomSingleVarWarnings)
          Yap_set_flag(MkAtomTerm(AtomSingleVarWarnings), TermTrue);
        else if (at == AtomDiscontiguousWarnings)
          Yap_set_flag(MkAtomTerm(AtomDiscontiguousWarnings), TermTrue);
        else if (at == AtomRedefineWarnings)
          Yap_set_flag(MkAtomTerm(AtomRedefineWarnings), TermTrue);
      } else {
        Atom at = AtomOfTerm(ArgOfTerm(1, h));
        if (at == AtomSingleVarWarnings)
          Yap_set_flag(MkAtomTerm(AtomSingleVarWarnings), TermFalse);
        else if (at == AtomDiscontiguousWarnings)
          Yap_set_flag(MkAtomTerm(AtomDiscontiguousWarnings), TermFalse);
        else if (at == AtomRedefineWarnings)
          Yap_set_flag(MkAtomTerm(AtomRedefineWarnings), TermFalse);
      }
    }
  }
  return TRUE;
}

Term Yap_BufferToTerm(const char *s, Term opts) {
  Term rval;
  int sno;
  encoding_t l = ENC_ISO_UTF8;

  sno =
      Yap_open_buf_read_stream(NULL, (char *)s, strlen(s), &l, MEM_BUF_USER,
                               Yap_LookupAtom(Yap_StrPrefix(s, 16)), TermNone);

  GLOBAL_Stream[sno].status |= CloseOnException_Stream_f;
  rval = Yap_read_term(sno, opts, false);
  Yap_CloseStream(sno);
  Yap_RaiseException();
  return rval;
}

Term Yap_UBufferToTerm(const unsigned char *s, Term opts) {
  Term rval;
  int sno;
  encoding_t l = ENC_ISO_UTF8;

  sno = Yap_open_buf_read_stream(
      NULL, (char *)s, strlen((const char *)s),  &l, MEM_BUF_USER,
      Yap_LookupAtom(Yap_StrPrefix((char *)s, 16)), TermNone);
  GLOBAL_Stream[sno].status |= CloseOnException_Stream_f;
  rval = Yap_read_term(sno, opts, false);
  Yap_CloseStream(sno);
    Yap_RaiseException();
  return rval;
}

X_API Term Yap_BufferToTermWithPrioBindings(const char *s, Term ctl,
                                            Term bindings, size_t len,
                                            int prio) {
  if (bindings) {
    ctl = add_names(bindings, ctl);
  }
  if (prio != 1200) {
    ctl = add_priority(prio, ctl);
  }
  Term o = Yap_BufferToTerm(s, ctl);
    Yap_RaiseException();
  return o;
}

/**
 * @pred read_term_from_atom( +Atom , -T , +Options )
 *
 * read a term _T_ stored in constant _Atom_ according to  _Options_
 *
 * @param _Atom_ the source _Atom_
 * @param _T_ the output term _T_, may be any term
 * @param _Options_ read_term/3 options.
 *
 * @note Originally from SWI-Prolog, in YAP only works with internalised
 * atoms
 * Check  read_term_from_atomic/3 for the general version. Also, the built-in
 * is
 * supposed to
 * use YAP's internal encoding, so please avoid the encoding/1 option.
 */
static Int read_term_from_atom(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Atom at;
  const unsigned char *s;

  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "style_check/1");
    return false;
  } else if (!IsAtomTerm(t1)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t1, "style_check/1");
    return false;
  } else {
    at = AtomOfTerm(t1);
    s = at->UStrOfAE;
  }
  Term ctl = add_output(ARG2, ARG3);
  
  Int rc = Yap_UBufferToTerm(s, ctl);
  if (Yap_RaiseException()) {
    return false;
  }
  return rc;
}

/**
 *  @pred read_term_from_atomic( +Atomic , - T , +Options )
 *
 * read a term _T_ stored in text _Atomic_ according to  _Options_
 *
 * @param _Atomic_ the source may be an atom, string, list of codes, or list
 * of
 * chars.
 * @param _T_ the output term _T_, may be any term
 * @param _Options_ read_term/3 options.
 *
 * @note Idea originally from SWI-Prolog, but in YAP we separate atomic and
 * atom.
 * Encoding is fixed in atoms and strings.
 */
static Int read_term_from_atomic(USES_REGS1) {
  Term t1 = Deref(ARG1);
  const unsigned char *s;

  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "read_term_from_atomic/3");
    return (FALSE);
  } else if (!IsAtomicTerm(t1)) {
    Yap_ThrowError(TYPE_ERROR_ATOMIC, t1, "read_term_from_atomic/3");
    return (FALSE);
  } else {
    Term t = Yap_AtomicToString(t1 PASS_REGS);
    s = UStringOfTerm(t);
  }
  Term ctl = add_output(ARG2, ARG3);

  Int rc = Yap_UBufferToTerm(s, ctl);
  if (Yap_RaiseException()) {
    return false;
  }
  return rc;
}

/**
 * @pred read_term_from_string( +String , - T , + Options )
 *
 * read a term _T_ stored in constant _String_ according to  _Options_
 *
 * @param _String_ the source _String_
 * @param _T_ the output term _T_, may be any term
 * @param _Options_ read_term/3 options.
 *
 *  Idea from SWI-Prolog, in YAP only works with strings
 * Check  read_term_from_atomic/3 for the general version.
 */
static Int read_term_from_string(USES_REGS1) {
  Term t1 = Deref(ARG1), rc;
  const unsigned char *s;
  size_t len;

  BACKUP_H()
  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "read_term_from_string/3");
    return (FALSE);
  } else if (!IsStringTerm(t1)) {
    Yap_ThrowError(TYPE_ERROR_STRING, t1, "read_term_from_string/3");
    return (FALSE);
  } else {
    s = UStringOfTerm(t1);
    len = strlen_utf8(s);
  }
  char *ss = (char *)s;
  encoding_t enc = ENC_ISO_UTF8;
  int sno = Yap_open_buf_read_stream(NULL, ss, len, &enc, MEM_BUF_USER,
                                     Yap_LookupAtom(Yap_StrPrefix(ss, 16)),
                                     TermString);
  GLOBAL_Stream[sno].status |= CloseOnException_Stream_f;
  rc = Yap_read_term(sno, Deref(ARG3), 3);
  Yap_CloseStream(sno);
  if (Yap_RaiseException()) {
    return false;
  }
  RECOVER_H();
  if (!rc)
    return false;
  return Yap_unify(rc, ARG2);
}

/**
 * @pred read_term_from_chars( +String , - T , + Options )
 *
 * read a term _T_ stored in a list of characters _String_ according to  _Options_
 *
 * @param _String_ the source _String_
 * @param _T_ the output term _T_, may be any term
 * @param _Options_ read_term/3 options.
 *
 *  Idea from SWI-Prolog, in YAP only works with strings
 * Check  read_term_from_atomic/3 for the general version.
 */
static Int read_term_from_chars(USES_REGS1) {
  Term t1 = Deref(ARG1);
  const unsigned char *s;
  seq_tv_t inp;
  inp.val.t = t1;
  inp.type = YAP_STRING_ATOMS;
  BACKUP_H()
  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "read_term_from_string/3");
    return (FALSE);
  }
  s = Yap_ListOfCharsToBuffer(NULL, t1 , &inp PASS_REGS);
  Term ctl = add_output(ARG2, ARG3);

  Int rc = Yap_UBufferToTerm(s, ctl);
  if (Yap_RaiseException()) {
    return false;
  }
  return rc;
}

/**
 * @pred read_term_from_chars( +String , - T , + Options )
 *
 * read a term _T_ stored in a list of characters _String_ according to  _Options_
 *
 * @param _String_ the source _String_
 * @param _T_ the output term _T_, may be any term
 * @param _Options_ read_term/3 options.
 *
 *  Idea from SWI-Prolog, in YAP only works with strings
 * Check  read_term_from_atomic/3 for the general version.
 */
static Int read_term_from_codes(USES_REGS1) {
  Term t1 = Deref(ARG1);
  seq_tv_t inp;
  inp.val.t = t1;
  inp.type = YAP_STRING_ATOMS;
  BACKUP_H()
  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "read_term_from_string/3");
    return (FALSE);
  }
  const unsigned char *s = Yap_ListOfCodesToBuffer(NULL, t1, &inp PASS_REGS);
  Term ctl = add_output(ARG2, ARG3);

  Int rc = Yap_UBufferToTerm(s, ctl);
  if (Yap_RaiseException()) {
    return false;
  }
  return rc;
}



static Int atom_to_term(USES_REGS1) {
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "read_term_from_atom/3");
    return (FALSE);
  } else if (!IsAtomTerm(t1)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t1, "read_term_from_atomic/3");
    return (FALSE);
  } else {
    Term t = Yap_AtomToString(t1 PASS_REGS);
    const unsigned char *us = UStringOfTerm(t);
    return Yap_UBufferToTerm(us, add_output(ARG2, add_names(ARG3, TermNil)));
  }
}

static Int atomic_to_term(USES_REGS1) {
  Term t1 = Deref(ARG1);
  int l = push_text_stack();
  Term cm = CurrentModule;
  if (IsApplTerm(t1)) {
    Term tmod = LOCAL_SourceModule;
    t1 = Yap_YapStripModule(t1, &tmod);
    CurrentModule = tmod;
  }
  if (Yap_RaiseException()) {
    return false;
  }
  const unsigned char *s = Yap_TextToUTF8Buffer(t1 PASS_REGS);
  Int rc = Yap_UBufferToTerm(s, add_output(ARG2, add_names(ARG3, TermNil)));
  CurrentModule = cm;
  pop_text_stack(l);
  return rc;
}

static Int string_to_term(USES_REGS1) {
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "read_term_from_string/3");
    return (FALSE);
  } else if (!IsStringTerm(t1)) {
    Yap_ThrowError(TYPE_ERROR_STRING, t1, "read_term_from_string/3");
    return (FALSE);
  } else {
    const unsigned char *us = UStringOfTerm(t1);
    return Yap_UBufferToTerm(us, add_output(ARG2, add_names(ARG3, TermNil)));
  }
}



void Yap_InitReadTPreds(void) {
  Yap_InitCPred("read_term", 2, read_term2, SyncPredFlag);
  Yap_InitCPred("read_term", 3, read_term, SyncPredFlag);

  Yap_InitCPred("atom_to_term", 3, atom_to_term, 0);
  Yap_InitCPred("atomic_to_term", 3, atomic_to_term, 0);
  Yap_InitCPred("string_to_term", 3, string_to_term, 0);

  Yap_InitCPred("scan_stream", 2, scan_stream, SyncPredFlag);
  Yap_InitCPred("read", 1, read1, SyncPredFlag);
  Yap_InitCPred("read", 2, read2, SyncPredFlag);
  Yap_InitCPred("read_clause", 2, read_clause2, SyncPredFlag);
  Yap_InitCPred("read_clause", 3, read_clause, 0);
  Yap_InitCPred("read_term_from_atom", 3, read_term_from_atom, 0);
  Yap_InitCPred("read_term_from_atomic", 3, read_term_from_atomic, 0);
  Yap_InitCPred("read_term_from_string", 3, read_term_from_string, 0);
  Yap_InitCPred("read_term_from_chars", 3, read_term_from_chars, 0);
  Yap_InitCPred("read_term_from_codes", 3, read_term_from_codes, 0);
  Yap_InitCPred("source_location", 2, source_location, SyncPredFlag);
  Yap_InitCPred("$style_checker", 1, style_checker,
                SyncPredFlag | HiddenPredFlag);
}

/// @}
