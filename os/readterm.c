
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
* File:		iopreds.c						 *
* Last rev:	5/2/88							 *
* mods:									 *
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
#include "Yatom.h"
#include "YapHeap.h"
#include "YapFlags.h"
#include "yapio.h"
#include "eval.h"
#include "YapText.h"
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
#define strncat(X,Y,Z) strcat(X,Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X,Y,Z) strcpy(X,Y)
#endif
#if _MSC_VER || defined(__MINGW32__)
#if HAVE_SOCKET
#include <winsock2.h>
#endif
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR)==_S_IFDIR)
#endif
#endif
#include "iopreds.h"

#if _MSC_VER || defined(__MINGW32__)
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif

static void
clean_vars(VarEntry *p)
{
  if (p == NULL) return;
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

static Int
qq_open( USES_REGS1)
{ PRED_LD

      Term t = Deref(ARG1);
  if ( !IsVarTerm(t) &&
       IsApplTerm(t) &&
       FunctorOfTerm(t) = FunctorDQuasiQuotation)
    { void *ptr;
      char *  start;
      size_t l                                                                                                                                                                    int s;
      Term t0, t1, t2;

      if ( IsPointerTerm((t0 = ArgOfTerm(1, t))) &&
           IsPointerTerm((t1 = ArgOfTerm(2, t))) &&
           IsIntegerTerm((t2 = ArgOfTerm(3, t))))
        {
          ptr = PointerOfTerm(t0);
          start = PointerOfTerm(t1);
          len = IntegerOfTerm(t2);
          if ((s = Yap_open_buf_read_stream( start, len, ENC_UTF8, MEM_BUF_USER)) < 0)
            return false;
          return Yap_unify(ARG2, Yap_MkStream(s));
        } else {
          Yap_Error(TYPE_ERROR_READ_CONTEXT, t);
        }

      return FALSE;
    }
}

static int
parse_quasi_quotations(ReadData _PL_rd ARG_LD)
{ if ( _PL_rd->qq_tail )
    { term_t av;
      int rc;

      if ( !PL_unify_nil(_PL_rd->qq_tail) )
        return FALSE;

      if ( !_PL_rd->quasi_quotations )
        { if ( (av = PL_new_term_refs(2)) &&
               PL_put_term(av+0, _PL_rd->qq) &&
     #if __YAP_PROLOG__
               PL_put_atom(av+1, YAP_SWIAtomFromAtom(_PL_rd->module->AtomOfME)) &&
     #else
               PL_put_atom(av+1, _PL_rd->module->name) &&
     #endif
               PL_cons_functor_v(av, FUNCTOR_dparse_quasi_quotations2, av) )
            { term_t ex;
              rc = callProlog(MODULE_system, av+0, PL_Q_CATCH_EXCEPTION, &ex);
              if ( rc )
                return TRUE;
              _PL_rd->exception = ex;
              _PL_rd->has_exception = TRUE;
            }
          return FALSE;
        } else
        return TRUE;
    } else if ( _PL_rd->quasi_quotations )	/* user option, but no quotes */
    { return PL_unify_nil(_PL_rd->quasi_quotations);
    } else
    return TRUE;
}


#endif /*O_QUASIQUOTATIONS*/

#define    READ_DEFS()							\
  PAR( "comments", filler, READ_COMMENTS),				\
  PAR( "module", isatom, READ_MODULE ),				\
  PAR( "priority", nat, READ_PRIORITY ),				\
  PAR( "quasi_quotations", filler, READ_QUASI_QUOTATIONS ),		\
  PAR( "term_position", filler, READ_TERM_POSITION  ),		\
  PAR( "syntax_errors", isatom, READ_SYNTAX_ERRORS ),			\
  PAR( "singletons", filler, READ_SINGLETONS ),			\
  PAR( "variables", filler, READ_VARIABLES ),				\
  PAR( "variable_names", filler, READ_VARIABLE_NAMES ),		\
  PAR( "character_escapes", boolean, READ_CHARACTER_ESCAPES ),	\
  PAR( "backquoted_string", isatom, READ_BACKQUOTED_STRING ),		\
  PAR( "cycles", ok, READ_CYCLES ),					\
  PAR( NULL, ok, READ_END )

#define PAR(x,y,z) z

typedef enum open_enum_choices
{
  READ_DEFS()
} read_choices_t;

#undef PAR

#define PAR(x,y,z) { x , y, z }


static const param_t read_defs[] =
{
  READ_DEFS()
};
#undef PAR


/**
* Syntax Error Handler
*
* @par tokptr: the sequence of tokens
* @par sno: the stream numbet
*
* Implicit arguments:
*    +
*/
Term
Yap_syntax_error (TokEntry * tokptr, int sno)
{
  CACHE_REGS
      Term info;
  Term out = MkIntTerm(0);
  Term startline, errline, endline;
  Term tf[7];
  Term *error = tf+3;
  CELL *Hi = HR;
  UInt count = 0;

  startline = MkIntegerTerm(tokptr->TokPos);
  clean_vars(LOCAL_VarTable);
  clean_vars(LOCAL_AnonVarTable);
  while (1) {
      Term ts[2];

      if (HR > ASP-1024) {
          tf[3] = TermNil;
          errline = MkIntegerTerm(0);
          endline = MkIntegerTerm( 0 );
          count = 0;
          /* for some reason moving this earlier confuses gcc on solaris */
          HR = Hi;
          break;
        }
      if (tokptr == LOCAL_toktide) {
          errline = MkIntegerTerm( tokptr->TokPos );
          out = MkIntegerTerm(count);
        }
      info = tokptr->TokInfo;
      switch (tokptr->Tok) {
        case Name_tok:
          {
            Term t0[1];
            t0[0] = MkAtomTerm((Atom)info);
            ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomAtom,1),1,t0);
          }
          break;
        case Number_tok:
          ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomNumber,1),1,&(tokptr->TokInfo));
          break;
        case Var_tok:
          {
            Term t[3];
            VarEntry *varinfo = (VarEntry *)info;

            t[0] = MkIntTerm(0);
            t[1] = Yap_CharsToListOfCodes(varinfo->VarRep PASS_REGS);
            if (varinfo->VarAdr == TermNil) {
                t[2] = varinfo->VarAdr = MkVarTerm();
              } else {
                t[2] = varinfo->VarAdr;
              }
            ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomGVar,3),3,t);
          }
          break;
        case String_tok:
        {
          Term t0 = Yap_CharsToTDQ((char *)info, CurrentModule PASS_REGS);
          ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomString,1),1,&t0);
        }
          break;
        case WString_tok:
        {
          Term t0 = Yap_WCharsToTDQ((wchar_t *)info, CurrentModule PASS_REGS);
          ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomString,1),1,&t0);
        }
          break;
        case BQString_tok:
        {
          Term t0 = Yap_CharsToTBQ((char *)info, CurrentModule PASS_REGS);
          ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomString,1),1,&t0);
        }
          break;
        case WBQString_tok:
        {
          Term t0 = Yap_WCharsToTBQ((wchar_t *)info, CurrentModule PASS_REGS);
          ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomString,1),1,&t0);
        }
          break;
        case Error_tok:
        case eot_tok:
          break;
        case Ponctuation_tok:
          {
            char s[2];
            s[1] = '\0';
            if ((info) == 'l') {
                s[0] = '(';
              } else  {
                s[0] = (char)info;
              }
            ts[0] = MkAtomTerm(Yap_LookupAtom(s));
          }
        }
      if (tokptr->Tok == Ord (eot_tok)) {
          *error = TermNil;
          endline = MkIntegerTerm(tokptr->TokPos);
          break;
        } else if (tokptr->Tok != Ord (Error_tok)) {
          ts[1] = MkIntegerTerm(tokptr->TokPos);
          *error = MkPairTerm(Yap_MkApplTerm(FunctorMinus,2,ts),TermNil);
          error = RepPair(*error)+1;
          count++;
        }
      tokptr = tokptr->TokNext;
    }
  {
    Term tcount = MkIntegerTerm(count);
    Term t[3];
    tf[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomRead,1),1,&tcount);

    t[0] = startline;
    t[1] = errline;
    t[2] = endline;
    tf[1] = Yap_MkApplTerm(Yap_MkFunctor(AtomBetween,3),3,t);
  }
  tf[2] = TermDot;
  tf[4] = MkIntegerTerm(count);
  tf[5] = out;
  tf[6] = Yap_StreamUserName(sno);
  return(Yap_MkApplTerm(FunctorSyntaxError,7,tf));
}

typedef struct FEnv  {
  Term qq, tp, sp, np, vp, ce;
  Term tpos;                /// initial position of the term to be read.
  Term t;                   /// the output term
  TokEntry *tokstart;       /// the token list
  CELL *old_H;              /// initial H, will be reset on stack overflow.
  tr_fr_ptr old_TR;         /// initial TR
  xarg *args;               /// input args
  bool reading_clause;      /// read_clause
  size_t nargs;             /// arity of current procedure
} FEnv;

typedef struct renv {
  Term cm, bq;
  bool ce, sw;\
  Term sy;
  UInt cpos;
#if HAVE_FGETPOS
  fpos_t rpos;
#endif
  int prio;
  int ungetc_oldc;
  int had_ungetc;
  bool seekable;
} REnv;

static xarg * setClauseReadEnv(Term opts, FEnv *fe, struct renv *re, int inp_stream);
static xarg * setReadEnv(Term opts, FEnv *fe, struct renv *re, int inp_stream)
{
  CACHE_REGS
      re->cm = CurrentModule;
  xarg * args = Yap_ArgListToVector ( opts, read_defs, READ_END  );
  if (args == NULL) {
      return NULL;
    }
  re->bq = getBackQuotesFlag();
  if (args[READ_MODULE].used) {
      CurrentModule = args[READ_MODULE].tvalue;
    }
  if (args[READ_BACKQUOTED_STRING].used) {
      if (!setBackQuotesFlag(args[READ_BACKQUOTED_STRING].tvalue))
        return false;
    }
  if (args[READ_QUASI_QUOTATIONS].used) {
      fe->qq = args[READ_QUASI_QUOTATIONS].tvalue;
    } else {
      fe->qq = 0;
    }
  if (args[READ_TERM_POSITION].used) {
      fe->tp = args[READ_TERM_POSITION].tvalue;
    } else {
      fe->tp = 0;
    }
  if (args[READ_SINGLETONS].used) {
      fe->sp = args[READ_SINGLETONS].tvalue;
    } else if (args[READ_SINGLETONS].used) {
      fe->sp = MkVarTerm();
    } else {
      fe->sp = 0;
    }
  if (args[READ_SYNTAX_ERRORS].used) {
      re->sy = args[READ_SYNTAX_ERRORS].tvalue;
    } else {
    re->sy = TermError; // getYapFlag( MkAtomTerm(AtomSyntaxErrors) );
    }
  if (args[READ_VARIABLES].used) {
      fe->vp = args[READ_VARIABLES].tvalue;
    } else {
      fe->vp = 0;
    }
  if (args[READ_VARIABLE_NAMES].used) {
      fe->np = args[READ_VARIABLE_NAMES].tvalue;
    } else {
      fe->np = 0;
    }
  if (args[READ_CHARACTER_ESCAPES].used || Yap_CharacterEscapes( CurrentModule )) {
      fe->ce = true;
    } else {
      fe->ce = false;
    }
  re->seekable = (GLOBAL_Stream[inp_stream].status & Seekable_Stream_f) != 0;
  if (re->seekable) {
      if (GLOBAL_Stream[inp_stream].stream_getc == PlUnGetc) {
          re->had_ungetc = TRUE;
          re->ungetc_oldc = GLOBAL_Stream[inp_stream].och;
        }
#if HAVE_FGETPOS
      fgetpos(GLOBAL_Stream[inp_stream].file, &re->rpos);
#else
      re->cpos = GLOBAL_Stream[inp_stream].charcount;
#endif
    }
  if (args[READ_PRIORITY].used) {
      re->prio = IntegerOfTerm(args[READ_PRIORITY].tvalue);
      if (re->prio > 1200) {
          Yap_Error(DOMAIN_ERROR_OPERATOR_PRIORITY, opts, "max priority in Prolog is 1200, not %ld", re->prio);
        }
    }else {
      re->prio = LOCAL_default_priority;
    }
  return args;
}



typedef enum {
  YAP_START_PARSING, /// initialization
  YAP_SCANNING,       /// input to list of tokens
  YAP_SCANNING_ERROR, /// serious error (eg oom); trying error handling, followd by either restart or failure
  YAP_PARSING,         /// list of tokens to term
  YAP_PARSING_ERROR,   /// oom or syntax error
  YAP_PARSING_FINISHED /// exit parser
} parser_state_t;



Int
Yap_FirstLineInParse ( void )
{
  CACHE_REGS
      return LOCAL_StartLine;
}

static void
reset_regs( TokEntry *tokstart, FEnv *fe)
{
  CACHE_REGS

  restore_machine_regs();

  /* restart global */
  HR = fe->old_H;
  TR = (tr_fr_ptr)LOCAL_ScannerStack;
  Yap_growstack_in_parser(&fe->old_TR, &tokstart, &LOCAL_VarTable);
  LOCAL_ScannerStack = (char *)TR;
  TR = fe->old_TR;
}

static bool
complete_clause_processing( FEnv *fe, TokEntry *tokstarts, Term t );

static bool
complete_processing( FEnv *fe, TokEntry *tokstart )
{
  CACHE_REGS
      Term v;
  if (fe->vp) {
      while (TRUE) {
          fe->old_H = HR;

          if (setjmp(LOCAL_IOBotch) == 0) {
              v = Yap_Variables(LOCAL_VarTable, TermNil);
              break;
            } else {
              reset_regs(tokstart,  fe);
            }
        }
      if (!Yap_unify(v, fe->vp))
        return false;
    }
  if (fe->np) {
      while (TRUE) {
          fe->old_H = HR;

          if (setjmp(LOCAL_IOBotch) == 0) {
              v = Yap_VarNames(LOCAL_VarTable, TermNil);
              break;
            } else {
              reset_regs(tokstart,  fe);
            }
        }
      if (!Yap_unify(v, fe->np))
        return false;
    }
  if (fe->sp) {
      while (TRUE) {
          fe->old_H = HR;

          if (setjmp(LOCAL_IOBotch) == 0) {
              v = Yap_Singletons(LOCAL_VarTable, TermNil);
              break;
            } else {
              reset_regs(tokstart,  fe);
            }
        }
      if (!Yap_unify(v, fe->np))
        return false;
    }
  if (fe->tp) {
      while (TRUE) {
          fe->old_H = HR;

          if (setjmp(LOCAL_IOBotch) == 0) {
              v = fe->tpos;
              break;
            } else {
              reset_regs( tokstart,  fe);
            }
        }
      if (!Yap_unify(v, fe->tp))
        return false;
    }

  return true;
}

static parser_state_t
initParser(Term opts, FEnv *fe, REnv *re, int inp_stream, int nargs);

static parser_state_t
parse(REnv *re, FEnv *fe, int inp_stream);

static parser_state_t
scanError(REnv *re, FEnv  *fe, int inp_stream);

static parser_state_t
scanEOF( FEnv   *fe, int inp_stream);

static parser_state_t
scan(REnv *re, FEnv  *fe, int inp_stream);


static parser_state_t
scanEOF( FEnv  *fe, int inp_stream)
{
  CACHE_REGS
      //bool store_comments = false;
      TokEntry * tokstart =
      LOCAL_tokptr;
       // check for an user abort
      if (tokstart != NULL && tokstart->Tok != Ord (eot_tok)) {
          /* we got the end of file from an abort */
          if (LOCAL_ErrorMessage &&
              !strcmp(LOCAL_ErrorMessage,"Abort")) {
              fe->t = 0L;
              Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable);
              return YAP_PARSING_FINISHED;
            }
          // a :- <eof>
          /* we need to force the next read to also give end of file.*/
          GLOBAL_Stream[inp_stream].status |= Push_Eof_Stream_f;
          LOCAL_ErrorMessage = "end of file found before end of term";
          return YAP_PARSING;
        } else {
          // <eof>
          // return end_of_file
          Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable);
          fe->t = MkAtomTerm (AtomEof);
          post_process_eof(GLOBAL_Stream + inp_stream);
#if DEBUG
  if (GLOBAL_Option['p' - 'a' + 1] || true) {
      fprintf(stderr, "[ end_of_file %p ]", GLOBAL_Stream[inp_stream].name);
    Yap_DebugPlWrite(fe->t);
    Yap_DebugPutc(stderr, ' ]\n');
  }
#endif
          return YAP_PARSING_FINISHED;

        }
}

static parser_state_t
initParser(Term opts, FEnv  *fe, REnv *re, int inp_stream, int nargs)
{
  CACHE_REGS
      fe->old_H = HR;
  LOCAL_ErrorMessage = NULL;
  fe->old_TR = TR;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  fe->tpos = StreamPosition(inp_stream);
  LOCAL_SourceFileName = GLOBAL_Stream[inp_stream].name;
  LOCAL_eot_before_eof = false;
  fe->tpos = StreamPosition(inp_stream);
  fe->reading_clause = nargs < 0;
  if (fe->reading_clause) {
      fe->nargs = -nargs;
      fe->args = setClauseReadEnv( opts, fe, re, inp_stream );
    } else {
      fe->nargs = nargs;
      fe->args = setReadEnv( opts, fe, re, inp_stream );
    }
  if (GLOBAL_Stream[inp_stream].status & Push_Eof_Stream_f) {
      fe->t = MkAtomTerm(AtomEof);
      GLOBAL_Stream[inp_stream].status &= ~Push_Eof_Stream_f;
      return YAP_PARSING_FINISHED;
    }
  if (!fe->args)
    return YAP_PARSING_FINISHED;
  return YAP_SCANNING;
}


static parser_state_t
scan(REnv *re, FEnv  *fe, int inp_stream)
{
  CACHE_REGS
  /* preserve   value of H after scanning: otherwise we may lose strings
      and floats */
  LOCAL_tokptr = LOCAL_toktide =
      Yap_tokenizer(GLOBAL_Stream+inp_stream, false, &fe->tpos);
  if (LOCAL_ErrorMessage)
    return YAP_SCANNING_ERROR;
  if (LOCAL_tokptr->Tok != Ord (eot_tok)) {
      // next step
      return YAP_PARSING;
    }
  return scanEOF(fe, inp_stream);
}


static parser_state_t
scanError(REnv *re, FEnv  *fe, int inp_stream)
{
  CACHE_REGS

  fe->t = 0;
  // running out of memory
  if (LOCAL_Error_TYPE == OUT_OF_TRAIL_ERROR) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_growtrail (sizeof(CELL) * K16, FALSE)) {
          return YAP_PARSING_FINISHED;
        }
    } else if (LOCAL_Error_TYPE == OUT_OF_AUXSPACE_ERROR) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
          return YAP_PARSING_FINISHED;
        }
    } else if (LOCAL_Error_TYPE == OUT_OF_HEAP_ERROR) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_growheap(FALSE, 0, NULL)) {
          return YAP_PARSING_FINISHED;
        }
    } else if (LOCAL_Error_TYPE == OUT_OF_STACK_ERROR) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_gcl(LOCAL_Error_Size, fe->nargs, ENV, CP)) {
          return YAP_PARSING_FINISHED;
        }
    }
  // go back to the start
  if (re->had_ungetc) {
      GLOBAL_Stream[inp_stream].stream_getc = PlUnGetc;
      GLOBAL_Stream[inp_stream].och = re->ungetc_oldc;
    }
  if (re->seekable) {
      if (GLOBAL_Stream[inp_stream].status & InMemory_Stream_f) {
          GLOBAL_Stream[inp_stream].u.mem_string.pos = re->cpos;
        } else if (GLOBAL_Stream[inp_stream].status) {
#if HAVE_FGETPOS
          fsetpos(GLOBAL_Stream[inp_stream].file, &re->rpos);
#else
          fseek(GLOBAL_Stream[inp_stream].file, re->cpos, 0L);
#endif
        }
    }
  return YAP_SCANNING;
}

static parser_state_t
parseError(REnv *re, FEnv  *fe, int inp_stream)
{
  CACHE_REGS

      fe->t = 0;
  TokEntry * tokstart =
      LOCAL_tokptr;
  if (LOCAL_Error_TYPE == OUT_OF_TRAIL_ERROR ||
      LOCAL_Error_TYPE == OUT_OF_AUXSPACE_ERROR ||
      LOCAL_Error_TYPE == OUT_OF_HEAP_ERROR ||
      LOCAL_Error_TYPE == OUT_OF_STACK_ERROR) {
      return YAP_SCANNING_ERROR;
    }
  Term ParserErrorStyle =  re->sy;
  if (ParserErrorStyle == TermQuiet) {
      /* just fail */
      return YAP_PARSING_FINISHED;
    } else {
      Term terr = Yap_syntax_error(tokstart, inp_stream);
      if (ParserErrorStyle ==TermError) {
          LOCAL_ErrorMessage =  "SYNTAX ERROR";
          Yap_Error(SYNTAX_ERROR,terr,LOCAL_ErrorMessage);
          return YAP_PARSING_FINISHED;
        } else {
          Yap_PrintWarning(terr);
          if (ParserErrorStyle ==TermDec10);
          return YAP_SCANNING;
        }
    }
  return YAP_PARSING_FINISHED;
}

static parser_state_t
parse(REnv *re, FEnv  *fe, int inp_stream)
{
  CACHE_REGS
      TokEntry * tokstart =
      LOCAL_tokptr;
  fe->t = Yap_Parse(re->prio);
  if (fe->t == 0 || LOCAL_ErrorMessage)
    return YAP_SCANNING_ERROR;
  TR = (tr_fr_ptr)LOCAL_ScannerStack;
  if (fe->reading_clause)
    complete_clause_processing(  fe, tokstart, fe->t );
  else
    complete_processing(  fe, tokstart );
#if EMACS
  first_char = tokstart->TokPos;
#endif /* EMACS */
  Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable);
  LOCAL_tokptr = NULL;
  return YAP_PARSING_FINISHED;
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
 * Implemenfayubluses a state machine: default is init, scan, parse, complete.
 *
 *
 */
Term  Yap_read_term(int inp_stream, Term opts, int nargs)
{
  CACHE_REGS
  
  FEnv  fe;
  REnv re;
  tr_fr_ptr tr0 = TR;
#if EMACS
  int emacs_cares = FALSE;
#endif


  parser_state_t state = YAP_START_PARSING;
  while (state != YAP_PARSING_FINISHED)
    {
      switch( state )
        {
        case YAP_START_PARSING:
          state = initParser(opts, &fe, &re, inp_stream, nargs);
          break;
        case YAP_SCANNING:
          state = scan(&re, &fe, inp_stream);
          break;
        case YAP_SCANNING_ERROR:
          state = scanError(&re, &fe, inp_stream);
          break;
        case YAP_PARSING:
          state = parse(&re, &fe, inp_stream);
          break;
        case YAP_PARSING_ERROR:
          state = parseError(&re, &fe, inp_stream);
          break;
        case YAP_PARSING_FINISHED:
          return fe.t;
        }
    }
  TR = tr0;
  return fe.t;
}


static Int
read_term2( USES_REGS1 )
{				/* '$read'(+Flag,?Term,?Module,?Vars,-Pos,-Err)    */
  Term rc;
  yhandle_t h = Yap_InitSlot(ARG1);
  if ((rc =Yap_read_term(LOCAL_c_input_stream, ARG2, 2)) == 0)
    return FALSE;
  return Yap_unify( Yap_GetFromSlot( h ), rc );
}

static Int
read_term ( USES_REGS1 )
{				/* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
  int inp_stream;
  Int out;

  /* needs to change LOCAL_output_stream for write */
  yhandle_t h = Yap_InitSlot(ARG2);
  inp_stream = Yap_CheckStream (ARG1, Input_Stream_f, "read/3");
  if (inp_stream == -1) {
      return(FALSE);
    }
  out = Yap_read_term(inp_stream, ARG3, 3 );
  UNLOCK(GLOBAL_Stream[inp_stream].streamlock);
  return out != 0L && Yap_unify( Yap_GetFromSlot( h ), out );
}

#define    READ_CLAUSE_DEFS()							\
  PAR( "comments", filler, READ_CLAUSE_COMMENTS),				\
  PAR( "process_comments", boolean, READ_CLAUSE_PROCESS_COMMENTS),				\
  PAR( "module", isatom, READ_CLAUSE_MODULE ),				\
  PAR( "variable_names", filler, READ_CLAUSE_VARIABLE_NAMES ),		\
  PAR( "term_position", filler, READ_CLAUSE_TERM_POSITION  ),		\
  PAR( "syntax_errors", isatom, READ_CLAUSE_SYNTAX_ERRORS ),			\
  PAR( NULL, ok, READ_CLAUSE_END )


#define PAR(x,y,z) z

typedef enum read_clause_enum_choices
{
  READ_CLAUSE_DEFS()
} read_clause_choices_t;

#undef PAR

#define PAR(x,y,z) { x , y, z }


static const param_t read_clause_defs[] =
{
  READ_CLAUSE_DEFS()
};
#undef PAR

static xarg * setClauseReadEnv( Term opts, FEnv  *fe, struct renv *re, int inp_stream)
{
  CACHE_REGS
      re->cm = CurrentModule;
  xarg *args = Yap_ArgListToVector ( opts, read_clause_defs, READ_END  );
  if (args == NULL) {
      return NULL;
    }
  re->cm = CurrentModule;
  re->bq = getBackQuotesFlag();
  CurrentModule = LOCAL_SourceModule;
  fe->qq = 0;
  if (args[READ_CLAUSE_TERM_POSITION].used) {
      fe->tp = args[READ_CLAUSE_TERM_POSITION].tvalue;
    } else {
      fe->tp = 0;
    }
  if (trueLocalPrologFlag( SINGLE_VAR_WARNINGS_FLAG )) {
      fe->sp = MkVarTerm();
    } else {
      fe->sp = 0;
    }
  if (args[READ_CLAUSE_SYNTAX_ERRORS].used) {
      re->sy = args[READ_CLAUSE_SYNTAX_ERRORS].tvalue;
    } else {
      re->sy = TermDec10;
    }
  fe->vp = 0;
  if (args[READ_VARIABLE_NAMES].used) {
      fe->np = args[READ_VARIABLE_NAMES].tvalue;
    } else {
      fe->np = 0;
    }
  fe->ce =  Yap_CharacterEscapes( CurrentModule ) ;
  re->seekable = (GLOBAL_Stream[inp_stream].status & Seekable_Stream_f) != 0;
  if (re->seekable) {
      if (GLOBAL_Stream[inp_stream].stream_getc == PlUnGetc) {
          re->had_ungetc = TRUE;
          re->ungetc_oldc = GLOBAL_Stream[inp_stream].och;
        }
#if HAVE_FGETPOS
      fgetpos(GLOBAL_Stream[inp_stream].file, &re->rpos);
#else
      re->cpos = GLOBAL_Stream[inp_stream].charcount;
#endif
    }
  re->prio = LOCAL_default_priority;
  return args;
}

static bool
complete_clause_processing( FEnv  *fe, TokEntry *tokstart,  Term t )
{
  CACHE_REGS
      Term v;
  if (fe->np) {
      while (TRUE) {
          fe->old_H = HR;

          if (setjmp(LOCAL_IOBotch) == 0) {
              v = Yap_VarNames(LOCAL_VarTable, TermNil);
              break;
            } else {
              reset_regs( tokstart,  fe);
            }
        }
      if (!Yap_unify(v, fe->np))
        return false;
    }
  if (fe->sp) {
      Term singls[3];
      while (TRUE) {
          fe->old_H = HR;

          if (setjmp(LOCAL_IOBotch) == 0) {
              v = Yap_Singletons(LOCAL_VarTable, TermNil);
              if (v == TermNil)
                break;
              singls[0] = v;
              singls[1] = fe->tpos;
              singls[2] = t;
              jmp_deb(1);
               if (Yap_PrintWarning(Yap_MkApplTerm(Yap_MkFunctor(AtomSingleton,3),3,singls)))
                 break;
            } else {
	    reset_regs( tokstart,  fe);
            }
        }
    }
  if (fe->tp) {
      while (TRUE) {
          fe->old_H = HR;

          if (setjmp(LOCAL_IOBotch) == 0) {
              v = MkIntegerTerm(Yap_FirstLineInParse());
              break;
            } else {
              reset_regs( tokstart,  fe);
            }
        }
      if (!Yap_unify(v, fe->tp))
        return false;
    }

  return true;
}


/**
* @pred read_clause( +_Stream_, -_Clause_, ?_Opts) is det
*
* Same as read_clause/3, but from the standard input stream.
*
*/
static Int
read_clause2( USES_REGS1 )
{
  Term rc;
  yhandle_t h = Yap_InitSlot(ARG1);
  rc = Yap_read_term(LOCAL_c_input_stream, Deref(ARG2), -2 );
  return rc && Yap_unify( Yap_GetFromSlot( h ), rc );
}

/**
* @pred read_clause( +_Stream_, -_Clause_, ?_Opts) is det
*
* This predicate receives a set of options _OPts_ based on read_term/3, but specific
* to readin clauses. The following options are considered:
*
*   + The `comments` option unifies its argument with the comments in the term,
*     represented as strings
*   + The `process_comments` option calls a hook, it is current ignored by YAP.
*   + The `term_position` unifies its argument with a term describing the
*     position of the term.
*   + The `syntax_errors` flag controls response to syntactic errors, the default is `dec10`.
*
* The next two options are called implicitely:
*
*   + The `module` option is initialised to the current source module, by default.
*   + The `tons` option is set from the single var flag
*/
static Int
read_clause ( USES_REGS1 )
{				/* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
  int inp_stream;
  Int out;
  Term t3 = Deref(ARG3);
    yhandle_t h = Yap_InitSlot(ARG2);
  /* needs to change LOCAL_output_stream for write */
  inp_stream = Yap_CheckStream (ARG1, Input_Stream_f, "read/3");
  out = Yap_read_term(inp_stream, t3, -3 );
  UNLOCK(GLOBAL_Stream[inp_stream].streamlock);

  return out && Yap_unify( Yap_GetFromSlot( h ), out );
}

/**
* @pred source_location( - _File_ , _Line_ )
*
* unify  _File_ and  _Line_ wuth the position of the last term read, if the term
* comes from a stream created by opening a file-system path with open/3 and friends.>position
* It ignores user_input or
* sockets.
*
* @param - _File_
* @param - _Line_
*
* @note SWI-Prolog built-in.
*/
static Int
source_location ( USES_REGS1 )
{
  return Yap_unify( ARG1, MkAtomTerm(LOCAL_SourceFileName)) &&
      Yap_unify( ARG2, MkIntegerTerm(LOCAL_SourceFileLineno));
}


/**
* @pred read(+ _Stream_, - _Term_ ) is iso
*
* Reads term  _T_ from the stream  _S_ instead of from the current input
* stream.
*
* @param - _Stream_
* @param - _Term_
*
*/
static Int
read2 ( USES_REGS1 )
{				/* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
  int inp_stream;
  Int out;

  /* needs to change LOCAL_output_stream for write */
  inp_stream = Yap_CheckStream (ARG1, Input_Stream_f, "read/3");
  if (inp_stream == -1) {
      return(FALSE);
    }
  out = Yap_read_term(inp_stream, TermNil, 1);
  UNLOCK(GLOBAL_Stream[inp_stream].streamlock);
  return out && Yap_unify(ARG2, out);
}

/** @pred  read(- _T_) is iso

Reads the next term from the current input stream, and unifies it with
_T_. The term must be followed by a dot (`.`) and any blank-character
as previously defined. The syntax of the term must match the current
declarations for operators (see op). If the end-of-stream is reached,
_T_ is unified with the atom `end_of_file`. Further reads from of
the same stream may cause an error failure (see open/3).

*/
static Int
read1 ( USES_REGS1 )
{				/* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
  Term out = Yap_read_term(LOCAL_c_input_stream, TermNil, 1);
  return out && Yap_unify(ARG1, out);
}

/**  @pred fileerrors

Switches on the file_errors flag so that in certain error conditions
Input/Output predicates will produce an appropriated message and abort.

*/
static Int
fileerrors ( USES_REGS1 )
{
  return  setYapFlag( MkAtomTerm(AtomFileerrors), TermTrue );
}

/**
@pred  nofileerrors

Switches off the `file_errors` flag, so that the predicates see/1,
tell/1, open/3 and close/1 just fail, instead of producing
an error message and aborting whenever the specified file cannot be
opened or closed.

*/
static Int nofileerrors( USES_REGS1 )
{				/* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
  return  setYapFlag( MkAtomTerm(AtomFileerrors), TermFalse );
}


static Int style_checker( USES_REGS1 )
{
  Term t = Deref( ARG1 );

  if (IsVarTerm(t)) {
      Term t = TermNil;
      if ( getYapFlag( MkAtomTerm(AtomSingleVarWarnings)) == TermTrue) {
          t = MkPairTerm( MkAtomTerm(AtomSingleVarWarnings), t );
        }
      if (getYapFlag( MkAtomTerm(AtomDiscontiguousWarnings)) == TermTrue) {
          t = MkPairTerm( MkAtomTerm(AtomDiscontiguousWarnings), t );
        }
      if ( getYapFlag( MkAtomTerm(AtomRedefineWarnings)) == TermTrue ) {
          t = MkPairTerm( MkAtomTerm(AtomRedefineWarnings), t );
        }
    } else {
      while (IsPairTerm(t)) {
          Term h = HeadOfTerm( t );
          t = TailOfTerm( t );

          if (IsVarTerm(h)) {
              Yap_Error(INSTANTIATION_ERROR, t, "style_check/1");
              return (FALSE);
            } else if (IsAtomTerm(h)) {
              Atom at = AtomOfTerm( h );
              if (at == AtomSingleVarWarnings)
                setYapFlag( MkAtomTerm(AtomSingleVarWarnings), TermTrue);
              else if (at == AtomDiscontiguousWarnings)
                setYapFlag( MkAtomTerm(AtomDiscontiguousWarnings), TermTrue);
              else if (at == AtomRedefineWarnings)
                setYapFlag( MkAtomTerm(AtomRedefineWarnings), TermTrue);
            } else {
              Atom at = AtomOfTerm( ArgOfTerm( 1, h ) );
              if (at == AtomSingleVarWarnings)
                setYapFlag( MkAtomTerm(AtomSingleVarWarnings), TermFalse);
              else if (at == AtomDiscontiguousWarnings)
                setYapFlag( MkAtomTerm(AtomDiscontiguousWarnings), TermFalse);
              else if (at == AtomRedefineWarnings)
                setYapFlag( MkAtomTerm(AtomRedefineWarnings), TermFalse);
            }
        }
    }
  return TRUE;
}


Term
Yap_StringToTerm(const char *s, size_t len, encoding_t enc, int prio, Term *bindings)
{
  CACHE_REGS
      Term bvar = MkVarTerm(), ctl;
  yhandle_t sl;

  if (bindings) {
      ctl = Yap_MkApplTerm( Yap_MkFunctor(AtomVariableNames,1),1,&bvar);
      sl = Yap_InitSlot( bvar );
    } else {
      ctl = TermNil;
      sl = 0;
    }

  Term rval;
  int stream = Yap_open_buf_read_stream(s, len, enc, MEM_BUF_USER);

  rval = Yap_read_term(stream, ctl, 3);
  Yap_CloseStream(stream);
  UNLOCK(GLOBAL_Stream[stream].streamlock);
  if (rval && bindings) {
      *bindings = Yap_GetFromSlot( sl );
      Yap_RecoverSlots( sl, 1 PASS_REGS);
    }
  return rval;
}

Term
Yap_ReadFromAtom(Atom a, Term opts)
{
  Term rval;
  int sno;
  if (IsWideAtom( a )) {
      wchar_t *ws = a->WStrOfAE;
      size_t len = wcslen(ws);
      sno = Yap_open_buf_read_stream((char *)ws, len, ENC_ISO_ANSI, MEM_BUF_USER);
    } else {
      char *s = a->StrOfAE;
      size_t len = strlen(s);
      sno = Yap_open_buf_read_stream((char *)s, len, ENC_ISO_LATIN1, MEM_BUF_USER);
    }

  rval = Yap_read_term(sno, opts, 3);
  Yap_CloseStream(sno);
  return rval;
}

static Term
readFromBuffer(const char *s, Term opts)
{
  Term rval;
  int sno;
  sno = Yap_open_buf_read_stream((char *)s, utf8_strlen1(s), ENC_ISO_UTF8, MEM_BUF_USER);

  rval = Yap_read_term(sno, opts, 3);
  Yap_CloseStream(sno);
  return rval;
}

/**
* @pred read_term_from_atom( +_Atom_ , - _T_ , + _VarNames_
*
* read a term _T_ stored in constant _Atom_ and report their names
*
* @param _Atom_ the source _Atom_
* @param _T_ the output term _T_, may be any term
* @param _VarNames_ list of _Var_ = _Name_ tuples.
*
* @notes Originally from SWI-Prolog, in YAP only works with  atoms.
*/
static Int
atom_to_term( USES_REGS1 )
{
  Term t1 = Deref(ARG1), ctl, rc;
  Atom at;
  if (IsVarTerm(t1)) {
      Yap_Error(INSTANTIATION_ERROR, t1, "style_check/1");
      return (FALSE);
    } else if (!IsAtomTerm(t1)) {
      Yap_Error(TYPE_ERROR_ATOM, t1, "style_check/1");
      return (FALSE);
    } else {
      at = AtomOfTerm( t1 );
    }
  Term bvar = MkVarTerm();
  Yap_unify( ARG3, bvar );
  ctl = Yap_MkApplTerm( Yap_MkFunctor(AtomVariableNames,1),1,&bvar);
  if ((rc = Yap_ReadFromAtom( at, ctl)) == 0L)
    return false;
  return Yap_unify( rc, ARG2 );
}

/**
* @pred read_term_from_atom( +_Atom_ , - _T_ , + _Options_
*
* read a term _T_ stored in constant _Atom_ according to  _Options_
*
* @param _Atom_ the source _Atom_
* @param _T_ the output term _T_, may be any term
* @param _Options_ read_term/3 options.
*
* @notes Originally from SWI-Prolog, in YAP only works with internalised atoms
* Check  read_term_from_atomic/3 for the general version. Also, the built-in is supposed to
* use YAP's internal encoding, so please avoid the encoding/1 option.
*/  static Int
read_term_from_atom( USES_REGS1 )
{
  Term t1 = Deref(ARG1), rc;
  Atom at;
  if (IsVarTerm(t1)) {
      Yap_Error(INSTANTIATION_ERROR, t1, "style_check/1");
      return (FALSE);
    } else if (!IsAtomTerm(t1)) {
      Yap_Error(TYPE_ERROR_ATOM, t1, "style_check/1");
      return (FALSE);
    } else {
      at = AtomOfTerm( t1 );
    }
  if ((rc = Yap_ReadFromAtom( at, Deref(ARG3))) == 0L)
    return false;
  return Yap_unify( rc, ARG2 );
}

/**
* @pred read_term_from_string( +_String_ , - _T_ , + _Options_
*
* read a term _T_ stored in constant _String_ according to  _Options_
*
* @param _String_ the source _String_
* @param _T_ the output term _T_, may be any term
* @param _Options_ read_term/3 options.
*
* @notes Idea from SWI-Prolog, in YAP only works with strings
* Check  read_term_from_atomic/3 for the general version.
*/
static Int
read_term_from_string( USES_REGS1 )
{
  Term t1 = Deref(ARG1), rc;
  const char *s;
  size_t len;
  if (IsVarTerm(t1)) {
      Yap_Error(INSTANTIATION_ERROR, t1, "read_term_from_string/3");
      return (FALSE);
    } else if (!IsStringTerm(t1)) {
      Yap_Error(TYPE_ERROR_STRING, t1, "read_term_from_string/3");
      return (FALSE);
    } else {
      s = StringOfTerm( t1 );
      len =     utf8_strlen1( s );

    }
  int sno = Yap_open_buf_read_stream(s, len, ENC_ISO_UTF8, MEM_BUF_USER);
  rc = readFromBuffer( s, Deref(ARG3) );
  Yap_CloseStream(sno);
  if (!rc)
    return false;
  return Yap_unify( rc, ARG2 );
}

/**
*  @pred read_term_from_atomic( +_Atomic_ , - _T_ , + _Options_ )
*
* read a term _T_ stored in text _Atomic_ according to  _Options_
*
* @param _Atomic_ the source may be an atom, string, list of codes, or list of chars.
* @param _T_ the output term _T_, may be any term
* @param _Options_ read_term/3 options.
*
* @notes Idea originally from SWI-Prolog, but in YAP we separate atomic and atom.
* Encoding is fixed in atoms and strings.
*/  static Int
read_term_from_atomic( USES_REGS1 )
{
  Term t1 = Deref(ARG1),  rc;
  const char *s;
  size_t len;
  if (IsVarTerm(t1)) {
      Yap_Error(INSTANTIATION_ERROR, t1, "read_term_from_atomic/3");
      return (FALSE);
    } else if (!IsAtomicTerm(t1)) {
      Yap_Error(TYPE_ERROR_ATOMIC, t1, "read_term_from_atomic/3");
      return (FALSE);
    } else {
      Term t = Yap_AtomicToString(t1 PASS_REGS);
      s = StringOfTerm( t );
      len =     utf8_strlen1( s );

    }
  int sno = Yap_open_buf_read_stream(s, len, ENC_ISO_UTF8, MEM_BUF_USER);
  rc = readFromBuffer( s, Deref(ARG3) );
  Yap_CloseStream(sno);
  if (!rc)
    return false;   return Yap_unify( rc, ARG2 );
}

void
Yap_InitReadTPreds(void)
{
  Yap_InitCPred ("read", 1, read1, SyncPredFlag);
  Yap_InitCPred ("read", 2, read2, SyncPredFlag);
  Yap_InitCPred ("read_term", 2, read_term2, SyncPredFlag);
  Yap_InitCPred ("read_term", 3, read_term, SyncPredFlag);
  Yap_InitCPred ("read_clause", 2, read_clause2, SyncPredFlag);
  Yap_InitCPred ("read_clause", 3, read_clause, SyncPredFlag);

  Yap_InitCPred ("atom_to_term", 3, atom_to_term, 0);
  Yap_InitCPred ("read_term_from_atom", 3, read_term_from_atom, 0);
  Yap_InitCPred ("read_term_from_atomic", 3, read_term_from_atomic, 0);
  Yap_InitCPred ("read_term_from_string", 3, read_term_from_string, 0);

  Yap_InitCPred ("fileerrors", 0, fileerrors, SyncPredFlag);
  Yap_InitCPred ("nofileeleerrors", 0, nofileerrors, SyncPredFlag);
  Yap_InitCPred ("source_location", 2, source_location, SyncPredFlag);
  Yap_InitCPred ("$style_checker", 1, style_checker, SyncPredFlag|HiddenPredFlag);

}

