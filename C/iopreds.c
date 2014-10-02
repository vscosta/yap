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
 * for yap refering to: Files and Streams, Simple Input/Output, 
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "eval.h"
/* stuff we want to use in standard YAP code */
#include "pl-shared.h"
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
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR)==_S_IFDIR)
#endif
#endif
#include "iopreds.h"
#include "pl-read.h"

static Int p_set_read_error_handler( USES_REGS1 );
static Int p_get_read_error_handler( USES_REGS1 );
static Int p_startline( USES_REGS1 );
static Int p_change_type_of_char( USES_REGS1 );
static Int p_type_of_char( USES_REGS1 );

extern Atom Yap_FileName(IOSTREAM *s);

static Term
StreamName(IOSTREAM *s)
{
  return MkAtomTerm(Yap_FileName(s));
}


void
Yap_InitStdStreams (void)
{
}

void
Yap_InitPlIO (void)
{
}

/*
 * Used by the prompts to check if they are after a newline, and then a
 * prompt should be output, or if we are in the middle of a line.
 */
static int newline = TRUE;

#ifdef DEBUG

static       int   eolflg = 1;



static char     my_line[200] = {0};
static char    *lp = my_line;

static YP_File     curfile;

#ifdef MACC

static void 
InTTYLine(char *line)
{
	char           *p = line;
	char            ch;
	while ((ch = InKey()) != '\n' && ch != '\r')
		if (ch == 8) {
			if (line < p)
				BackupTTY(*--p);
		} else
			TTYChar(*p++ = ch);
	TTYChar('\n');
	*p = 0;
}

#endif

void 
Yap_DebugSetIFile(char *fname)
{
  if (curfile)
    YP_fclose(curfile);
  curfile = YP_fopen(fname, "r");
  if (curfile == NULL) {
    curfile = stdin;
    fprintf(stderr,"%% YAP Warning: can not open %s for input\n", fname);
  }
}

void 
Yap_DebugEndline()
{
	*lp = 0;

}

int 
Yap_DebugGetc()
{
  int             ch;
  if (eolflg) {
    if (curfile != NULL) {
      if (YP_fgets(my_line, 200, curfile) == 0)
	curfile = NULL;
    }
    if (curfile == NULL)
      if (YP_fgets(my_line, 200, stdin) == NULL) {
	return EOF;
      }
    eolflg = 0;
    lp = my_line;
  }
  if ((ch = *lp++) == 0)
    ch = '\n', eolflg = 1;
  if (GLOBAL_Option['l' - 96])
    putc(ch, GLOBAL_logfile);
  return (ch);
}

int 
Yap_DebugPutc(int sno, wchar_t ch)
{
  if (GLOBAL_Option['l' - 96])
    (void) putc(ch, GLOBAL_logfile);
  return (Sputc(ch, GLOBAL_stderr));
}

static int 
Yap_DebugPuts(int sno, const char * s)
{
  if (GLOBAL_Option['l' - 96])
    (void) fputs(s, GLOBAL_logfile);
  return (Sfputs(s, GLOBAL_stderr));
}

void
Yap_DebugPlWrite(Term t)
{
  Yap_plwrite(t, NULL, 15, 0, 1200);
}

void 
Yap_DebugErrorPutc(int c)
{
  CACHE_REGS
   Yap_DebugPutc (LOCAL_c_error_stream, c);
}

void 
Yap_DebugErrorPuts(const char *s)
{
  CACHE_REGS
   Yap_DebugPuts (LOCAL_c_error_stream, s);
}

#endif




int
Yap_GetCharForSIGINT(void)
{
  int ch;
  /* ask for a new line */
  Sfprintf(Serror, "\nAction (h for help): ");
  ch = Sgetchar();
  /* first process up to end of line */
  while ((Sfgetc(Sinput)) != '\n');
  newline = TRUE;
  return ch;
}



typedef struct stream_ref
{ struct io_stream *read;
  struct io_stream *write;
} stream_ref;

#ifdef BEAM
int beam_write (void)
{
  Yap_StartSlots();
  Yap_plwrite (ARG1, NULL, 0, 0, 1200);
  LOCAL_CurSlot = CurSlot;
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = 0L;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}
#endif

static void
clean_vars(VarEntry *p)
{
  if (p == NULL) return;
  p->VarAdr = TermNil;
  clean_vars(p->VarLeft);
  clean_vars(p->VarRight);
}

static Term
syntax_error (TokEntry * tokptr, IOSTREAM *st, Term *outp)
{
  CACHE_REGS
  Term info;
  int count = 0, out = 0;
  Int start, err = 0, end;
  Term tf[7];
  Term *error = tf+3;
  CELL *Hi = HR;
  int has_qq = FALSE;

  /* make sure to globalise variable */
  start = tokptr->TokPos;
  clean_vars(LOCAL_VarTable);
  clean_vars(LOCAL_AnonVarTable);
  while (1) {
    Term ts[2];
    if (HR > ASP-1024) {
      tf[3] = TermNil;
      err = 0;
      end = 0;
      /* for some reason moving this earlier confuses gcc on solaris */
      HR = Hi;
      break;
    }
    if (tokptr == LOCAL_toktide) {
      err = tokptr->TokPos;
      out = count;
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
    case QuasiQuotes_tok:
    case WQuasiQuotes_tok:
      {
	if (has_qq) {
	  Term t0[1];
	  t0[0] = MkAtomTerm(Yap_LookupAtom("{|"));
	  ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomAtom,1),1,t0);
	  has_qq = FALSE;
	} else {
	  Term t0[1];
	  t0[0] = MkAtomTerm(Yap_LookupAtom("|| ... |}"));
	  ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomAtom,1),1,t0);
	  has_qq = TRUE;
	}
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
	t[1] = Yap_CharsToListOfCodes((const char *)varinfo->VarRep PASS_REGS);
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
	Term t0 = Yap_CharsToListOfCodes((const char *)info PASS_REGS);
	ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomString,1),1,&t0);
      }
      break;
    case WString_tok:
      {
	Term t0 = Yap_WCharsToListOfCodes((const wchar_t *)info PASS_REGS);
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
	s[0] = (char)info;
	if (s[0] == 'l') 
	  s[0] = '(';
	ts[0] = MkAtomTerm(Yap_LookupAtom(s));
      }
    }
    if (tokptr->Tok == Ord (eot_tok)) {
      *error = TermNil;
      end = tokptr->TokPos;
      break;
    } else if (tokptr->Tok != Ord (Error_tok)) {
      ts[1] = MkIntegerTerm(tokptr->TokPos);
      *error =
	MkPairTerm(Yap_MkApplTerm(FunctorMinus,2,ts),TermNil);
      error = RepPair(*error)+1;
      count++;
    }
    tokptr = tokptr->TokNext;
  }
  /* now we can throw away tokens, so we can unify and possibly overwrite TR */
  Yap_unify(*outp, MkVarTerm());
  if (IsVarTerm(*outp) && (VarOfTerm(*outp) > HR || VarOfTerm(*outp) < H0)) {
    tf[0] = Yap_MkNewApplTerm(Yap_MkFunctor(AtomRead,1),1);
  } else {
    tf[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomRead,1),1,outp);
  }
  {
    Term t[3];

    t[0] = MkIntegerTerm(start);
    t[1] = MkIntegerTerm(err);
    t[2] = MkIntegerTerm(end);
    tf[1] = Yap_MkApplTerm(Yap_MkFunctor(AtomBetween,3),3,t);
  }
  tf[2] = MkAtomTerm(AtomHERE);
  tf[4] = MkIntegerTerm(out);
  tf[5] = MkIntegerTerm(err);
  tf[6] = StreamName(st);
  return(Yap_MkApplTerm(FunctorSyntaxError,7,tf));
}

static void
GenerateSyntaxError(Term *tp, TokEntry *tokstart, IOSTREAM *sno, Term msg USES_REGS)
{
  if (tp) {
    Term et[2];
    Term t = MkVarTerm();
    et[1] = MkPairTerm(syntax_error(tokstart, sno, &t), msg);
    t = MkAtomTerm(AtomSyntaxError);
    et[0] = Yap_MkApplTerm(FunctorShortSyntaxError,1,&t);
    *tp = Yap_MkApplTerm(FunctorError, 2, et);
  }
}

Int
Yap_FirstLineInParse (void)
{
  CACHE_REGS
  return LOCAL_StartLine;
}

static Int
p_startline ( USES_REGS1 )
{
  return (Yap_unify_constant (ARG1, MkIntegerTerm (LOCAL_StartLine)));
}

/* control the parser error handler */
static Int
p_set_read_error_handler( USES_REGS1 )
{
  Term t = Deref(ARG1);
  char *s;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"set_read_error_handler");
    return(FALSE);
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM,t,"bad syntax_error handler");
    return(FALSE);
  }
  s = RepAtom(AtomOfTerm(t))->StrOfAE;
  if (!strcmp(s, "fail")) {
    ParserErrorStyle = FAIL_ON_PARSER_ERROR;
  } else if (!strcmp(s, "error")) {
    ParserErrorStyle = EXCEPTION_ON_PARSER_ERROR;
  } else if (!strcmp(s, "quiet")) {
    ParserErrorStyle = QUIET_ON_PARSER_ERROR;
  } else if (!strcmp(s, "dec10")) {
    ParserErrorStyle = CONTINUE_ON_PARSER_ERROR;
  } else {
    Yap_Error(DOMAIN_ERROR_SYNTAX_ERROR_HANDLER,t,"bad syntax_error handler");
    return(FALSE);
  }
  return(TRUE);
}

/* return the status for the parser error handler */
static Int
p_get_read_error_handler( USES_REGS1 )
{
  Term t;

  switch (ParserErrorStyle) {
  case FAIL_ON_PARSER_ERROR:
    t = MkAtomTerm(AtomFail);
    break;
  case EXCEPTION_ON_PARSER_ERROR:
    t = MkAtomTerm(AtomError);
    break;
  case QUIET_ON_PARSER_ERROR:
    t = MkAtomTerm(AtomQuiet);
    break;
  case CONTINUE_ON_PARSER_ERROR:
    t = MkAtomTerm(AtomDec10);
    break;
  default:
    Yap_Error(SYSTEM_ERROR,TermNil,"corrupted syntax_error handler");
    return(FALSE);
  }
  return (Yap_unify_constant (ARG1, t));
}



int
Yap_read_term(term_t t0, IOSTREAM *inp_stream, struct read_data_t *rd)
{
  CACHE_REGS
  TokEntry *tokstart;
  Term t;
  Term OCurrentModule = CurrentModule, tmod, tpos;
  int store_comments = rd->comments;

  if (inp_stream == NULL) {
    return FALSE;
  }
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  while (TRUE) {
    CELL *old_H;
    int64_t cpos = 0;
    int seekable = inp_stream->functions->seek != NULL;

    /* two cases where we can seek: memory and console */
    if (seekable) {
      cpos = inp_stream->posbuf.byteno;
    }
    /* Scans the term using stack space */
    while (TRUE) {
      old_H = HR;
      LOCAL_Comments = TermNil;
      LOCAL_CommentsNextChar = LOCAL_CommentsTail = NULL;
      tokstart = LOCAL_tokptr = LOCAL_toktide = Yap_tokenizer(inp_stream, store_comments, &tpos, rd);
      if (LOCAL_Error_TYPE != YAP_NO_ERROR && seekable) {
	HR = old_H;
	Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable, LOCAL_Comments);
	if (seekable) {
	  Sseek64(inp_stream, cpos, SIO_SEEK_SET);
	}
	if (LOCAL_Error_TYPE == OUT_OF_TRAIL_ERROR) {
	  LOCAL_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_growtrail (sizeof(CELL) * K16, FALSE)) {
	    return FALSE;
	  }
	} else if (LOCAL_Error_TYPE == OUT_OF_AUXSPACE_ERROR) {
	  LOCAL_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	    return FALSE;
	  }
	} else if (LOCAL_Error_TYPE == OUT_OF_HEAP_ERROR) {
	  LOCAL_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_growheap(FALSE, 0, NULL)) {
	    return FALSE;
	  }
	} else if (LOCAL_Error_TYPE == OUT_OF_STACK_ERROR) {
	  LOCAL_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_dogc( 0, NULL PASS_REGS )) {
	    return FALSE;
	  }
	}
      } else {
	/* done with this */
	break;
      }
    }
    LOCAL_Error_TYPE = YAP_NO_ERROR;
    /* preserve value of H after scanning: otherwise we may lose strings
       and floats */
    old_H = HR;
    if (tokstart != NULL && tokstart->Tok == Ord (eot_tok)) {
      /* did we get the end of file from an abort? */
      if (LOCAL_ErrorMessage &&
	  !strcmp(LOCAL_ErrorMessage,"Abort")) {
	Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable, LOCAL_Comments);
	return FALSE;
      } else {
	Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable, LOCAL_Comments);
	rd->varnames = 0;
	rd->singles = 0;
	return Yap_unify_constant( Yap_GetFromSlot( t0 PASS_REGS), MkAtomTerm (AtomEof));
      }
    }
  repeat_cycle:
    CurrentModule = tmod = MkAtomTerm(rd->module->AtomOfME);
    if (LOCAL_ErrorMessage || (t = Yap_Parse(rd)) == 0) {
      CurrentModule = OCurrentModule;
      if (LOCAL_ErrorMessage) {
	int res;

	if (!strcmp(LOCAL_ErrorMessage,"Stack Overflow") ||
	    !strcmp(LOCAL_ErrorMessage,"Trail Overflow") ||
	    !strcmp(LOCAL_ErrorMessage,"Heap Overflow")) {
	  /* ignore term we just built */
	  tr_fr_ptr old_TR = TR;


	  HR = old_H;
	  TR = (tr_fr_ptr)LOCAL_ScannerStack;
	  
	  if (!strcmp(LOCAL_ErrorMessage,"Stack Overflow"))
	    res = Yap_growstack_in_parser(&old_TR, &tokstart, &LOCAL_VarTable);
	  else if (!strcmp(LOCAL_ErrorMessage,"Heap Overflow"))
	    res = Yap_growheap_in_parser(&old_TR, &tokstart, &LOCAL_VarTable);
	  else
	    res = Yap_growtrail_in_parser(&old_TR, &tokstart, &LOCAL_VarTable);
	  if (res) {
	    LOCAL_ScannerStack = (char *)TR;
	    TR = old_TR;
	    old_H = HR;
	    LOCAL_tokptr = LOCAL_toktide = tokstart;
	    LOCAL_ErrorMessage = NULL;
	    goto repeat_cycle;
	  }
	  LOCAL_ScannerStack = (char *)TR;
	  TR = old_TR;
	}
      }
      {
	Term terror;
	if (LOCAL_ErrorMessage == NULL)
	  LOCAL_ErrorMessage = "SYNTAX ERROR";
	GenerateSyntaxError(&terror, tokstart, inp_stream, MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage)) PASS_REGS);
	
	Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable, LOCAL_Comments);
	rd->has_exception = TRUE;
	rd->exception = Yap_InitSlot(terror PASS_REGS);
	CurrentModule = OCurrentModule;
	return FALSE;
      }
    } else {
      CurrentModule = OCurrentModule;
      /* parsing succeeded */
      break;
    }
  }
  if (!Yap_unify(t, Yap_GetFromSlot( t0 PASS_REGS)))
    return FALSE;
  if (store_comments && !Yap_unify(LOCAL_Comments, Yap_GetFromSlot( rd->comments PASS_REGS)))
    return FALSE;
  if (rd->varnames) {
      Term v;
    while (TRUE) {
      CELL *old_H = HR;

      if (setjmp(LOCAL_IOBotch) == 0) {
	v = Yap_VarNames(LOCAL_VarTable, TermNil);
	break;
      } else {
	tr_fr_ptr old_TR;
	restore_machine_regs();

	old_TR = TR;
	/* restart global */
	HR = old_H;
	TR = (tr_fr_ptr)LOCAL_ScannerStack;
	Yap_growstack_in_parser(&old_TR, &tokstart, &LOCAL_VarTable);
	LOCAL_ScannerStack = (char *)TR;
	TR = old_TR;
      }
    }
    if (!Yap_unify(v, Yap_GetFromSlot( rd->varnames PASS_REGS))) {
      CurrentModule = OCurrentModule;
      return FALSE;
    }
  }

  if (rd->variables) {
      Term v;
    while (TRUE) {
      CELL *old_H = HR;

      if (setjmp(LOCAL_IOBotch) == 0) {
	v = Yap_Variables(LOCAL_VarTable, TermNil);
	break;
      } else {
	tr_fr_ptr old_TR;
	restore_machine_regs();

	old_TR = TR;
	/* restart global */
	HR = old_H;
	TR = (tr_fr_ptr)LOCAL_ScannerStack;
	Yap_growstack_in_parser(&old_TR, &tokstart, &LOCAL_VarTable);
	LOCAL_ScannerStack = (char *)TR;
	TR = old_TR;
      }
    }
    if (!Yap_unify(v, Yap_GetFromSlot( rd->variables PASS_REGS))) {
      CurrentModule = OCurrentModule;
      return FALSE;
    }
  }
  if (rd->singles) {
    Term v;
    while (TRUE) {
      CELL *old_H = HR;

      if (setjmp(LOCAL_IOBotch) == 0) {
	v = Yap_Singletons(LOCAL_VarTable, TermNil);
	break;
      } else {
	tr_fr_ptr old_TR;
	restore_machine_regs();

	old_TR = TR;
	/* restart global */
	HR = old_H;
	TR = (tr_fr_ptr)LOCAL_ScannerStack;
	Yap_growstack_in_parser(&old_TR, &tokstart, &LOCAL_VarTable);
	LOCAL_ScannerStack = (char *)TR;
	TR = old_TR;
      }
    }
    if (rd->singles == 1) {
      if (IsPairTerm(v))
	rd->singles = Yap_InitSlot( v PASS_REGS);
      else
	rd->singles = FALSE;
    } else if (rd->singles) {
      if (!Yap_unify( v, Yap_GetFromSlot( rd->singles PASS_REGS ))) {
	CurrentModule = OCurrentModule;
	return FALSE;
      }
    }
  }
  Yap_clean_tokenizer(tokstart, LOCAL_VarTable, LOCAL_AnonVarTable, LOCAL_Comments);
  CurrentModule = OCurrentModule;
  return TRUE;
}

static Int
p_change_type_of_char ( USES_REGS1 )
{				/* change_type_of_char(+char,+type)      */
  Term t1 = Deref (ARG1);
  Term t2 = Deref (ARG2);
  if (!IsVarTerm (t1) && !IsIntegerTerm (t1))
    return FALSE;
  if (!IsVarTerm(t2) && !IsIntegerTerm(t2))
    return FALSE;
  Yap_chtype[IntegerOfTerm(t1)] = IntegerOfTerm(t2);
  return TRUE;
}

static Int
p_type_of_char ( USES_REGS1 )
{				/* type_of_char(+char,-type)      */
  Term t;

  Term t1 = Deref (ARG1);
  if (!IsVarTerm (t1) && !IsIntegerTerm (t1))
    return FALSE;
  t = MkIntTerm(Yap_chtype[IntegerOfTerm (t1)]);
  return Yap_unify(t,ARG2);
}


static Int 
p_force_char_conversion( USES_REGS1 )
{
  /* don't actually enable it until someone tries to add a conversion */
  if (CharConversionTable2 == NULL)
    return(TRUE);
  CharConversionTable = CharConversionTable2;
  return(TRUE);
}

static Int 
p_disable_char_conversion( USES_REGS1 )
{
  CharConversionTable = NULL;
  return(TRUE);
}

static Int 
p_char_conversion( USES_REGS1 )
{
  Term t0 = Deref(ARG1), t1 = Deref(ARG2);
  char *s0, *s1;

  if (IsVarTerm(t0)) {
    Yap_Error(INSTANTIATION_ERROR, t0, "char_conversion/2");
    return (FALSE);    
  }
  if (!IsAtomTerm(t0)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t0, "char_conversion/2");
    return (FALSE);    
  }
  s0 = RepAtom(AtomOfTerm(t0))->StrOfAE;
  if (s0[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t0, "char_conversion/2");
    return (FALSE);    
  }
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "char_conversion/2");
    return (FALSE);    
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "char_conversion/2");
    return (FALSE);    
  }
  s1 = RepAtom(AtomOfTerm(t1))->StrOfAE;
  if (s1[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "char_conversion/2");
    return (FALSE);    
  }
  /* check if we do have a table for converting characters */
  if (CharConversionTable2 == NULL) {
     GET_LD
     int i;

    /* don't create a table if we don't need to */
    if (s0[0] == s1[0])
      return(TRUE);
    CharConversionTable2 = Yap_AllocCodeSpace(NUMBER_OF_CHARS*sizeof(char));
    while (CharConversionTable2 == NULL) {
      if (!Yap_growheap(FALSE, NUMBER_OF_CHARS*sizeof(char), NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	return(FALSE);
      }
    }
    if (truePrologFlag(PLFLAG_CHARCONVERSION)) {
      if (p_force_char_conversion( PASS_REGS1 ) == FALSE)
	return(FALSE);
    }
    for (i = 0; i < NUMBER_OF_CHARS; i++) 
      CharConversionTable2[i] = i;
  }
  /* just add the new entry */
  CharConversionTable2[(int)s0[0]] = s1[0];
  /* done */
  return(TRUE);
}

static Int 
p_current_char_conversion( USES_REGS1 )
{
  Term t0, t1;
  char *s0, *s1;

  if (CharConversionTable == NULL) {
    return(FALSE);
  }
  t0 = Deref(ARG1);
  if (IsVarTerm(t0)) {
    Yap_Error(INSTANTIATION_ERROR, t0, "current_char_conversion/2");
    return (FALSE);    
  }
  if (!IsAtomTerm(t0)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t0, "current_char_conversion/2");
    return (FALSE);    
  }
  s0 = RepAtom(AtomOfTerm(t0))->StrOfAE;
  if (s0[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t0, "current_char_conversion/2");
    return (FALSE);    
  }
  t1 = Deref(ARG2);
  if (IsVarTerm(t1)) {
    char out[2];
    if (CharConversionTable[(int)s0[0]] == '\0') return(FALSE);
    out[0] = CharConversionTable[(int)s0[0]];
    out[1] = '\0';
    return(Yap_unify(ARG2,MkAtomTerm(Yap_LookupAtom(out))));
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "current_char_conversion/2");
    return (FALSE);    
  }
  s1 = RepAtom(AtomOfTerm(t1))->StrOfAE;
  if (s1[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "current_char_conversion/2");
    return (FALSE);    
  } else {
    return (CharConversionTable[(int)s0[0]] == '\0' &&
	    CharConversionTable[(int)s0[0]] == s1[0] );
  }
}

static Int 
p_all_char_conversions( USES_REGS1 )
{
  Term out = TermNil;
  int i;

  if (CharConversionTable == NULL) {
    return(FALSE);
  }
  for (i = NUMBER_OF_CHARS; i > 0; ) {
    i--;
    if (CharConversionTable[i] != '\0') {
      Term t1, t2;
      char s[2];
      s[1] = '\0';
      s[0] = CharConversionTable[i];
      t1 = MkAtomTerm(Yap_LookupAtom(s));
      out = MkPairTerm(t1,out);
      s[0] = i;
      t2 = MkAtomTerm(Yap_LookupAtom(s));
      out = MkPairTerm(t2,out);
    }
  }
  return(Yap_unify(ARG1,out));
}

static Int
p_float_format( USES_REGS1 )
{
  Term in = Deref(ARG1);
  if (IsVarTerm(in))
    return Yap_unify(ARG1, MkAtomTerm(AtomFloatFormat));
  AtomFloatFormat = AtomOfTerm(in);
  return TRUE;
}


static Int 
p_style_checker( USES_REGS1 )
{
  Term t = Deref( ARG1 );
  LD_FROM_REGS

  if (IsVarTerm(t)) {
    Term t = TermNil;
    if ( debugstatus.styleCheck & LONGATOM_CHECK) {
      t = MkPairTerm( MkAtomTerm(AtomAtom), t );
    }
    if ( debugstatus.styleCheck & SINGLETON_CHECK) {
      t = MkPairTerm( MkAtomTerm(AtomSingleton), t );
    }
    if ( debugstatus.styleCheck & MULTITON_CHECK) {
      t = MkPairTerm( MkAtomTerm(AtomVarBranches), t );
    }
    if ( debugstatus.styleCheck & DISCONTIGUOUS_STYLE) {
      t = MkPairTerm( MkAtomTerm(AtomDiscontiguous), t );
    }
    if ( debugstatus.styleCheck & NOEFFECT_CHECK) {
      t = MkPairTerm( MkAtomTerm(AtomNoEffect), t );
    }
    if ( debugstatus.styleCheck & CHARSET_CHECK) {
      t = MkPairTerm( MkAtomTerm(AtomCharset), t );
    }
    if ( debugstatus.styleCheck & MULTIPLE_CHECK) {
      t = MkPairTerm( MkAtomTerm(AtomMultiple), t );
    }
  } else {
    while (IsPairTerm(t)) {
      Term h = HeadOfTerm( t );
      t = TailOfTerm( t );

      if (IsVarTerm(h)) {
	Yap_Error(INSTANTIATION_ERROR, t, "style_check/1");
	return (FALSE);    
      } if (IsAtomTerm(h)) {
	Atom at = AtomOfTerm( h );
	if (at == AtomAtom) debugstatus.styleCheck |= LONGATOM_CHECK;
	else if (at == AtomSingleton) debugstatus.styleCheck |= SINGLETON_CHECK;
	else if (at == AtomVarBranches) debugstatus.styleCheck |= MULTITON_CHECK;
	else if (at == AtomDiscontiguous) debugstatus.styleCheck |= DISCONTIGUOUS_STYLE;
	else if (at == AtomNoEffect) debugstatus.styleCheck |= NOEFFECT_CHECK;
	else if (at == AtomCharset) debugstatus.styleCheck |= CHARSET_CHECK;
	else if (at == AtomMultiple) debugstatus.styleCheck |= MULTIPLE_CHECK;
      } else {
	Atom at = AtomOfTerm( ArgOfTerm( 1, h ) );
	if (at == AtomAtom) debugstatus.styleCheck |= LONGATOM_CHECK;
	else if (at == AtomSingleton) debugstatus.styleCheck &= ~SINGLETON_CHECK;
	else if (at == AtomVarBranches) debugstatus.styleCheck &= ~MULTITON_CHECK;
	else if (at == AtomDiscontiguous) debugstatus.styleCheck &= ~DISCONTIGUOUS_STYLE;
	else if (at == AtomNoEffect) debugstatus.styleCheck &= ~NOEFFECT_CHECK;
	else if (at == AtomMultiple) debugstatus.styleCheck &= ~MULTIPLE_CHECK;
      }
    }
  }
  return TRUE;
}


void
Yap_InitBackIO (void)
{
}

#ifdef DEBUG
static Int
p_write_string( USES_REGS1 )
{
  Term in = Deref(ARG1);
  char *s;
  size_t length;
  int encoding;
  char buf[256];

  if ((s = Yap_TermToString( in, buf, 256, &length, &encoding, 0)))
    fprintf(stderr,"%s\n", s);
  return TRUE;
}
#endif

void
Yap_InitIOPreds(void)
{
  /* here the Input/Output predicates */
  Yap_InitCPred ("$set_read_error_handler", 1, p_set_read_error_handler, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$get_read_error_handler", 1, p_get_read_error_handler, SafePredFlag|SyncPredFlag);
#ifdef DEBUG
  Yap_InitCPred ("write_string", 2, p_write_string, SyncPredFlag|UserCPredFlag);
#endif
  Yap_InitCPred ("$start_line", 1, p_startline, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$change_type_of_char", 2, p_change_type_of_char, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$type_of_char", 2, p_type_of_char, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("char_conversion", 2, p_char_conversion, SyncPredFlag);
/** @pred char_conversion(+ _IN_,+ _OUT_) is iso 


While reading terms convert unquoted occurrences of the character
 _IN_ to the character  _OUT_. Both  _IN_ and  _OUT_ must be
bound to single characters atoms.

Character conversion only works if the flag `char_conversion` is
on. This is default in the `iso` and `sicstus` language
modes. As an example, character conversion can be used for instance to
convert characters from the ISO-LATIN-1 character set to ASCII.

If  _IN_ is the same character as  _OUT_, char_conversion/2
will remove this conversion from the table.

 
*/
  Yap_InitCPred ("$current_char_conversion", 2, p_current_char_conversion, SyncPredFlag);
  Yap_InitCPred ("$all_char_conversions", 1, p_all_char_conversions, SyncPredFlag);
  Yap_InitCPred ("$force_char_conversion", 0, p_force_char_conversion, SyncPredFlag);
  Yap_InitCPred ("$disable_char_conversion", 0, p_disable_char_conversion, SyncPredFlag);
#if HAVE_SELECT
    //  Yap_InitCPred ("stream_select", 3, p_stream_select, SafePredFlag|SyncPredFlag);
/** @pred  stream_select(+ _STREAMS_,+ _TIMEOUT_,- _READSTREAMS_) 


Given a list of open  _STREAMS_ opened in read mode and a  _TIMEOUT_
return a list of streams who are now available for reading. 

If the  _TIMEOUT_ is instantiated to `off`,
stream_select/3 will wait indefinitely for a stream to become
open. Otherwise the timeout must be of the form `SECS:USECS` where
`SECS` is an integer gives the number of seconds to wait for a timeout
and `USECS` adds the number of micro-seconds.

This built-in is only defined if the system call `select` is
available in the system.

 
*/
#endif
  Yap_InitCPred ("$float_format", 1, p_float_format, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$style_checker", 1, p_style_checker, SyncPredFlag);
}
