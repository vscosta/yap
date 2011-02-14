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

STATIC_PROTO (Int PlIOError, (yap_error_number, Term, char *));
STATIC_PROTO (int FilePutc, (int, int));
STATIC_PROTO (int console_post_process_read_char, (int, StreamDesc *));
STATIC_PROTO (int console_post_process_eof, (StreamDesc *));
STATIC_PROTO (int post_process_read_char, (int, StreamDesc *));
STATIC_PROTO (int post_process_eof, (StreamDesc *));
STATIC_PROTO (int ConsolePutc, (int, int));
STATIC_PROTO (int PlGetc, (int));
STATIC_PROTO (int DefaultGets, (int,UInt,char*));
STATIC_PROTO (int PlGets, (int,UInt,char*));
STATIC_PROTO (int ISOWGetc, (int));
STATIC_PROTO (int ConsoleGetc, (int));
#if HAVE_LIBREADLINE && HAVE_READLINE_READLINE_H
STATIC_PROTO (int ReadlineGetc, (int));
STATIC_PROTO (int ReadlinePutc, (int,int));
#endif
STATIC_PROTO (int PlUnGetc, (int));
STATIC_PROTO (Term MkStream, (int));
STATIC_PROTO (int CheckStream, (Term, int, char *));
STATIC_PROTO (Int p_close, (void));
STATIC_PROTO (Int p_write, (void));
STATIC_PROTO (Int p_write2, (void));
STATIC_PROTO (Int p_set_read_error_handler, (void));
STATIC_PROTO (Int p_get_read_error_handler, (void));
STATIC_PROTO (Int p_read, (void));
STATIC_PROTO (Int p_past_eof, (void));
STATIC_PROTO (Int p_skip, (void));
STATIC_PROTO (Int p_write_depth, (void));
STATIC_PROTO (Int p_user_file_name, (void));
STATIC_PROTO (Int p_show_stream_position, (void));
STATIC_PROTO (Int p_set_stream_position, (void));
STATIC_PROTO (Int p_format, (void));
STATIC_PROTO (Int p_startline, (void));
STATIC_PROTO (Int p_change_type_of_char, (void));
STATIC_PROTO (Int p_type_of_char, (void));
STATIC_PROTO (void CloseStream, (int));
STATIC_PROTO (int get_wchar, (int));
STATIC_PROTO (int put_wchar, (int,wchar_t));
STATIC_PROTO (Term StreamPosition, (int));

static encoding_t
DefaultEncoding(void)
{
  char *s = getenv("LANG");
  size_t sz;

  /* if we don't have a LANG then just use ISO_LATIN1 */
  if (s == NULL)
    s = getenv("LC_CTYPE");
  if (s == NULL)
    return ENC_ISO_LATIN1;
  sz = strlen(s);
  if (sz >= 5) {
    if (s[sz-5] == 'U' &&
	s[sz-4] == 'T' &&
	s[sz-3] == 'F' &&
	s[sz-2] == '-' &&
	s[sz-1] == '8') {
      return ENC_ISO_UTF8;
    }
  }
  return ENC_ISO_ANSI;
}

static int
GetFreeStreamD(void)
{
  int sno;

  for (sno = 0; sno < MaxStreams; ++sno) {
    LOCK(Stream[sno].streamlock);
    if (Stream[sno].status & Free_Stream_f) {
      break;
    }
    UNLOCK(Stream[sno].streamlock);
  }
  if (sno == MaxStreams) {
    return -1;
  }
  Stream[sno].encoding = DefaultEncoding();
  return sno;
}

int
Yap_GetFreeStreamD(void)
{
  return GetFreeStreamD();
}

/* used from C-interface */
int
Yap_GetFreeStreamDForReading(void)
{
  int sno = GetFreeStreamD();
  StreamDesc *s;

  if (sno < 0) return sno;
  s = Stream+sno;
  s->status |= User_Stream_f|Input_Stream_f;
  s->charcount = 0;
  s->linecount = 1;
  s->linepos = 0;
  s->stream_wgetc = get_wchar;
  s->encoding = DefaultEncoding();
  if (CharConversionTable != NULL)
    s->stream_wgetc_for_read = ISOWGetc;
  else
    s->stream_wgetc_for_read = s->stream_wgetc;
  UNLOCK(s->streamlock);
  return sno;
}


static void
unix_upd_stream_info (StreamDesc * s)
{
#if _MSC_VER  || defined(__MINGW32__)
  {
    if (
	_isatty(_fileno(s->u.file.file))
	) {
      s->status |= Tty_Stream_f|Reset_Eof_Stream_f|Promptable_Stream_f;
      /* make all console descriptors unbuffered */
      setvbuf(s->u.file.file, NULL, _IONBF, 0);
      return;
    }
#if _MSC_VER
    /* standard error stream should never be buffered */
    else if (StdErrStream == s-Stream) {
      setvbuf(s->u.file.file, NULL, _IONBF, 0);      
    }
#endif
    s->status |= Seekable_Stream_f;
    return;
  }
#else
#if HAVE_ISATTY
#if __simplescalar__
  /* isatty does not seem to work with simplescar. I'll assume the first
     three streams will probably be ttys (pipes are not thatg different) */
  if (s-Stream < 3) {
    s->u.file.name = AtomTty;
    s->status |= Tty_Stream_f|Reset_Eof_Stream_f|Promptable_Stream_f;
  }
#else 
  {
    int filedes; /* visualc */
    filedes = YP_fileno (s->u.file.file);
    if (isatty (filedes)) {
#if HAVE_TTYNAME      
	char *ttys = ttyname(filedes);
	if (ttys == NULL)
	  s->u.file.name = AtomTty;
	else
	  s->u.file.name = AtomTtys;
#else
	s->u.file.name = AtomTty;
#endif
	s->status |= Tty_Stream_f|Reset_Eof_Stream_f|Promptable_Stream_f;
	return;
    }
  }
#endif
#endif /* HAVE_ISATTY */
#endif /* _MSC_VER */
  s->status |= Seekable_Stream_f;
}

static Int
p_always_prompt_user(void)
{
  StreamDesc *s = Stream+StdInStream;

  s->status |= Promptable_Stream_f;
  s->stream_gets = DefaultGets;
#if HAVE_LIBREADLINE && HAVE_READLINE_READLINE_H
     if (s->status & Tty_Stream_f) {
       s->stream_getc = ReadlineGetc;
       if (Stream[0].status & Tty_Stream_f &&
	   s->u.file.name == Stream[0].u.file.name)
	 s->stream_putc = ReadlinePutc;
	 s->stream_wputc = put_wchar;
     } else
#endif
       {
	 /* else just PlGet plus checking for prompt */
	 s->stream_getc = ConsoleGetc;
       }
  return(TRUE);
}

static int
is_same_tty(YP_File f1, YP_File f2)
{
#if HAVE_TTYNAME
  return(ttyname(YP_fileno(f1)) == ttyname(YP_fileno(f2)));
#else
  return(TRUE);
#endif  
}

static GetsFunc
PlGetsFunc(void)
{
  if (CharConversionTable)
      return DefaultGets;
    else
      return PlGets;
}

static void
InitStdStreams (void)
{
  Yap_c_input_stream = StdInStream;
  Yap_c_output_stream = StdOutStream;
  Yap_c_error_stream = StdErrStream;
}

void
Yap_InitStdStreams (void)
{
  InitStdStreams();
}

static void
InitPlIO (void)
{
  Int i;

  for (i = 0; i < MaxStreams; ++i) {
    INIT_LOCK(Stream[i].streamlock);    
    Stream[i].status = Free_Stream_f;
  }
  InitStdStreams();
}

void
Yap_InitPlIO (void)
{
  InitPlIO ();
}

static Int
PlIOError (yap_error_number type, Term culprit, char *who)
{
  if (Yap_GetValue(AtomFileerrors) == MkIntTerm(1) ||
      type == RESOURCE_ERROR_MAX_STREAMS /* do not catch resource errors */) {
    Yap_Error(type, culprit, who);
    /* and fail */
    return FALSE;
  } else {
    return FALSE;
  }
}

/*
 * Used by the prompts to check if they are after a newline, and then a
 * prompt should be output, or if we are in the middle of a line.
 */
static int newline = TRUE;

static void
count_output_char(int ch, StreamDesc *s)
{
  if (ch == '\n')
    {
#if MPWSHELL
      if (mpwshell && (sno == StdOutStream || sno ==
		       StdErrStream))
	{
	  putc (MPWSEP, s->u.file.file);
	  fflush (stdout);
	}
#endif
      /* Inform that we have written a newline */
      ++s->charcount;
      ++s->linecount;
      s->linepos = 0;
    }
  else {
#if MAC
    if ((sno == StdOutStream || sno == StdErrStream)
	&& s->linepos > 200)
      sno->stream_putc(sno, '\n');
#endif
    ++s->charcount;
    ++s->linepos;
  }
}

static void
console_count_output_char(int ch, StreamDesc *s)
{
  if (ch == '\n')
    {
#if MPWSHELL
      if (mpwshell && (sno == StdOutStream || sno ==
		       StdErrStream))
	{
	  putc (MPWSEP, s->u.file.file);
	  fflush (stdout);
	}
#endif
      ++s->charcount;
      ++s->linecount;
      s->linepos = 0;
      newline = TRUE;
      /* Inform we are not at the start of a newline */
    }
  else {
    newline = FALSE;
#if MAC
    if ((sno == StdOutStream || sno == StdErrStream)
	&& s->linepos > 200)
      sno->stream_putc(sno, '\n');
#endif
    ++s->charcount;
    ++s->linepos;
  }
}

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
  if (Yap_Option['l' - 96])
    putc(ch, Yap_logfile);
  return (ch);
}

int 
Yap_DebugPutc(int sno, wchar_t ch)
{
  if (Yap_Option['l' - 96])
    (void) putc(ch, Yap_logfile);
  return (putc(ch, Yap_stderr));
}

void
Yap_DebugPlWrite(Term t)
{
  Yap_plwrite(t, Yap_DebugPutc, 0, 1200);
}

void
Yap_PlWriteToStream(Term t, int sno, int flags)
{
  Yap_plwrite(t, Stream[sno].stream_wputc, flags, 1200);
}

void 
Yap_DebugErrorPutc(int c)
{
  Yap_DebugPutc (Yap_c_error_stream, c);
}

#endif

/* static */
static int
FilePutc(int sno, int ch)
{
  StreamDesc *s = &Stream[sno];
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
  putc(ch, s->u.file.file);
#if MAC || _MSC_VER
  if (ch == 10)
    {
      fflush(s->u.file.file);
    }
#endif
  count_output_char(ch,s);
  return ((int) ch);
}

/* static */
static int
IOSWIPutc(int sno, int ch)
{
  int i;
  Yap_StartSlots();
  i = (SWIPutc)(ch, Stream[sno].u.swi_stream.swi_ptr);
  Yap_CloseSlots();
  YENV = ENV;
  return i;
}

/* static */
static int
IOSWIGetc(int sno)
{
  int ch;
  Yap_StartSlots();
  ch = (SWIGetc)(Stream[sno].u.swi_stream.swi_ptr);
  if (ch == EOF) {
    return post_process_eof(Stream+sno);    
  }
  return post_process_read_char(ch, Stream+sno);
  Yap_CloseSlots();
  YENV = ENV;
  return ch;
}

/* static */
static int
IOSWIWidePutc(int sno, int ch)
{
  int i;
  Yap_StartSlots();
  i = (SWIWidePutc)(ch, Stream[sno].u.swi_stream.swi_ptr);
  Yap_CloseSlots();
  YENV = ENV;
  return i;
}

/* static */
static int
IOSWIWideGetc(int sno)
{
  int ch;
  Yap_StartSlots();
  ch = (SWIWideGetc)(Stream[sno].u.swi_stream.swi_ptr);
  if (ch == EOF) {
    return post_process_eof(Stream+sno);    
  }
  return post_process_read_char(ch, Stream+sno);
  Yap_CloseSlots();
  YENV = ENV;
  return ch;
}


/* static */
static int
ConsolePutc (int sno, int ch)
{
  StreamDesc *s = &Stream[sno];
#if MAC || _MSC_VER || defined(__MINGW32__)
  if (ch == 10)
    {
      putc ('\n', s->u.file.file);
    }
  else
#endif
    putc (ch, s->u.file.file);
  console_count_output_char(ch,s);
  return ((int) ch);
}

static Int
p_is_same_tty (void)
{				/* 'prompt(Atom)                 */
  int sni = CheckStream (ARG1, Input_Stream_f, "put/2");
  int sno = CheckStream (ARG2, Output_Stream_f, "put/2");
  int out = (Stream[sni].status & Tty_Stream_f) &&
    (Stream[sno].status & Tty_Stream_f) &&
    is_same_tty(Stream[sno].u.file.file,Stream[sni].u.file.file);
  UNLOCK(Stream[sno].streamlock);
  UNLOCK(Stream[sni].streamlock);
  return out;
}

#if HAVE_LIBREADLINE && HAVE_READLINE_READLINE_H

#include <readline/readline.h>
#include <readline/history.h>

static char *ttyptr = NULL;

static char *myrl_line = (char *) NULL;

static int cur_out_sno = 2;

#define READLINE_OUT_BUF_MAX 256

static void
InitReadline(void) {
  ReadlineBuf = (char *)Yap_AllocAtomSpace(READLINE_OUT_BUF_MAX+1);
  ReadlinePos = ReadlineBuf;
#if _MSC_VER || defined(__MINGW32__)
  rl_instream = stdin;
  rl_outstream = stdout;
#endif
}

static int
ReadlinePutc (int sno, int ch)
{
  if (ReadlinePos != ReadlineBuf &&
      (ReadlinePos - ReadlineBuf == READLINE_OUT_BUF_MAX-1 /* overflow */ ||
#if MAC || _MSC_VER
       ch == 10 ||
#endif
       ch == '\n')) {
#if MAC || _MSC_VER
    if (ch == 10)
      {
	ch = '\n';
      }
#endif
    if (ch == '\n') {
      ReadlinePos[0] = '\n';
      ReadlinePos++;
    }
    ReadlinePos[0] = '\0';
    fputs( ReadlineBuf, Stream[sno].u.file.file);
    ReadlinePos = ReadlineBuf;
    if (ch == '\n') {
      console_count_output_char(ch,Stream+sno);
      return((int) '\n');
    }
  }
  *ReadlinePos++ = ch;
  console_count_output_char(ch,Stream+sno);
  return ((int) ch);
}

/*
  reading from the console is complicated because we need to
  know whether to prompt and so on... 
*/
static int
ReadlineGetc(int sno)
{
  register StreamDesc *s = &Stream[sno];
  register wchar_t ch;

  while (ttyptr == NULL) {
    /* Only sends a newline if we are at the start of a line */
    if (myrl_line) {
      free (myrl_line);
      myrl_line = NULL;
    }
    rl_instream = Stream[sno].u.file.file;
    rl_outstream = Stream[cur_out_sno].u.file.file;
    /* window of vulnerability opened */
    if (newline) {
      Yap_PrologMode |= ConsoleGetcMode;
      myrl_line = readline (NULL);
    } else {
      if (ReadlinePos != ReadlineBuf) {
	ReadlinePos[0] = '\0';
	ReadlinePos = ReadlineBuf;
	Yap_PrologMode |= ConsoleGetcMode;
	myrl_line = readline (ReadlineBuf);
      } else {
	Yap_PrologMode |= ConsoleGetcMode;
	myrl_line = readline (NULL);
      }
    }
    /* Do it the gnu way */
    if (Yap_PrologMode & InterruptMode) {
      Yap_PrologMode &= ~InterruptMode;
      Yap_ProcessSIGINT();
      Yap_PrologMode &= ~ConsoleGetcMode;
      if (Yap_PrologMode & AbortMode) {
	Yap_Error(PURE_ABORT, TermNil, "");
	Yap_ErrorMessage = "Abort";
	return console_post_process_eof(s);
      }
      continue;
    } else {
      Yap_PrologMode &= ~ConsoleGetcMode;
    }
    newline=FALSE;
    strncpy (Prompt, RepAtom (AtPrompt)->StrOfAE, MAX_PROMPT);
    /* window of vulnerability closed */
    if (myrl_line == NULL)
      return console_post_process_eof(s);
    if (myrl_line[0] != '\0' && myrl_line[1] != '\0')
      add_history (myrl_line);
    ttyptr = myrl_line;
  }
  if (*ttyptr == '\0') {
    ttyptr = NIL;
    ch = '\n';
  } else {
    ch = *((unsigned char *)ttyptr);
    ttyptr++;
  }
  return console_post_process_read_char(ch, s);
}

#endif /* HAVE_LIBREADLINE */

static Int
p_has_readline(void)
{
#if HAVE_LIBREADLINE && HAVE_READLINE_READLINE_H
  return TRUE;
#else
  return FALSE;
#endif
}


int
Yap_GetCharForSIGINT(void)
{
  int ch;
#if  HAVE_LIBREADLINE && HAVE_READLINE_READLINE_H
  if ((Yap_PrologMode & ConsoleGetcMode) && myrl_line != (char *) NULL) {
    ch = myrl_line[0];
    free(myrl_line);
    myrl_line = NULL;
  } else {
    myrl_line = readline ("Action (h for help): ");
    if (!myrl_line) {
      ch = EOF;
    } else {
      ch = myrl_line[0];
      free(myrl_line);
      myrl_line = NULL;
    }
  }
#else
  /* ask for a new line */
  fprintf(stderr, "Action (h for help): ");
  ch = getc(stdin);
  /* first process up to end of line */
  while ((fgetc(stdin)) != '\n');
#endif
  newline = TRUE;
  return ch;
}

/* handle reading from a stream after having found an EOF */
static int
EOFGetc(int sno)
{
  register StreamDesc *s = &Stream[sno];

  if (s->status & Push_Eof_Stream_f) {
    /* ok, we have pushed an EOF, send it away */
    s->status &= ~Push_Eof_Stream_f;
    return EOF;
  }	  
  if (s->status & Eof_Error_Stream_f) {
    Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,MkAtomTerm(s->u.file.name),
	  "GetC");
  } else if (s->status & Reset_Eof_Stream_f) {
    /* reset the eof indicator on file */
    if (YP_feof (s->u.file.file))
      YP_clearerr (s->u.file.file);
    /* reset our function for reading input */
    if (s->status & Promptable_Stream_f) {
      s->stream_putc = ConsolePutc;
      s->stream_wputc = put_wchar;
#if HAVE_LIBREADLINE && HAVE_READLINE_READLINE_H
      if (s->status & Tty_Stream_f) {
	s->stream_getc = ReadlineGetc;
	if (Stream[0].status & Tty_Stream_f &&
	    is_same_tty(s->u.file.file,Stream[0].u.file.file))
	 s->stream_putc = ReadlinePutc;
      } else
#endif
	{
	  s->stream_getc = ConsoleGetc;
	}
    } else {
      s->stream_getc = PlGetc;
      s->stream_gets = PlGetsFunc();
    }
    if (s->status & SWI_Stream_f)
      s->stream_wgetc = IOSWIWideGetc;
    else
      s->stream_wgetc = get_wchar;
    if (CharConversionTable != NULL)
      s->stream_wgetc_for_read = ISOWGetc;
    else
      s->stream_wgetc_for_read = s->stream_wgetc;
    /* next, reset our own error indicator */
    s->status &= ~Eof_Stream_f;
    /* try reading again */
    return(s->stream_getc(sno));
  } else {
    s->status |= Past_Eof_Stream_f;
  }
  return EOF;
}

/* check if we read a newline or an EOF */
static int
post_process_read_char(int ch, StreamDesc *s)
{
  ++s->charcount;
  ++s->linepos;
  if (ch == '\n') {
    ++s->linecount;
    s->linepos = 0;
    /* don't convert if the stream is binary */
    if (!(s->status & Binary_Stream_f))
      ch = 10;
  }
  return ch;
}

/* check if we read a newline or an EOF */
static int
post_process_eof(StreamDesc *s)
{
  s->status |= Eof_Stream_f;
  s->stream_getc = EOFGetc;
  s->stream_wgetc = get_wchar;
  if (CharConversionTable != NULL)
    s->stream_wgetc_for_read = ISOWGetc;
  else
    s->stream_wgetc_for_read = s->stream_wgetc;
  return EOFCHAR;
}

/* check if we read a newline or an EOF */
static int
console_post_process_read_char(int ch, StreamDesc *s)
{
  /* the character is also going to be output by the console handler */
  console_count_output_char(ch,Stream+StdErrStream);
  if (ch == '\n') {
    ++s->linecount;
    ++s->charcount;
    s->linepos = 0;
    newline = TRUE;
  } else {
    ++s->charcount;
    ++s->linepos;
    newline = FALSE;
  }
  return ch;
}

/* check if we read a newline or an EOF */
static int
console_post_process_eof(StreamDesc *s)
{
  s->status |= Eof_Stream_f;
  s->stream_getc = EOFGetc;
  s->stream_wgetc = get_wchar;
  if (CharConversionTable != NULL)
    s->stream_wgetc_for_read = ISOWGetc;
  else
    s->stream_wgetc_for_read = s->stream_wgetc;
  newline = FALSE;
  return EOFCHAR;
}


/* standard routine, it should read from anything pointed by a FILE *.
   It could be made more efficient by doing our own buffering and avoiding
   post_process_read_char, something to think about */
static int
PlGetc (int sno)
{
  StreamDesc *s = &Stream[sno];
  Int ch;

  ch = YP_getc (s->u.file.file);
  if (ch == EOF) {
    return post_process_eof(s);    
  }
  return post_process_read_char(ch, s);
}

/* standard routine, it should read from anything pointed by a FILE *.
   It could be made more efficient by doing our own buffering and avoiding
   post_process_read_char, something to think about */
static int
PlGets (int sno, UInt size, char *buf)
{
  register StreamDesc *s = &Stream[sno];
  UInt len;

  if (fgets (buf, size, s->u.file.file) == NULL) {
    return post_process_eof(s);    
  }
  len = strlen(buf);
  s->charcount += len-1;
  post_process_read_char(buf[len-2], s);
  return strlen(buf);
}

/* standard routine, it should read from anything pointed by a FILE *.
   It could be made more efficient by doing our own buffering and avoiding
   post_process_read_char, something to think about */
static int
DefaultGets (int sno, UInt size, char *buf)
{
  StreamDesc *s = &Stream[sno];
  char ch;
  char *pt = buf;


  if (!size)
    return 0;
  while((ch = *buf++ = s->stream_getc(sno)) != 
	-1 && ch != 10 && --size); 
  *buf++ = '\0';
  return (buf-pt)-1;
}

/* I dispise this code!!!!! */
static int
ISOWGetc (int sno)
{
  int ch = Stream[sno].stream_wgetc(sno);
  if (ch != EOF && CharConversionTable != NULL) {

    if (ch < NUMBER_OF_CHARS) {
      /* only do this in ASCII */
      return CharConversionTable[ch];
    }
  }
  return ch; 
}

/* send a prompt, and use the system for internal buffering. Speed is
   not of the essence here !!! */
static int
ConsoleGetc(int sno)
{
  register StreamDesc *s = &Stream[sno];
  int ch;

 restart:
  /* keep the prompt around, just in case, but don't actually
     show it in silent mode */
  if (newline) {
    if (!yap_flags[QUIET_MODE_FLAG]) {
      char *cptr = Prompt, ch;

      /* use the default routine */
      while ((ch = *cptr++) != '\0') {
	Stream[StdErrStream].stream_putc(StdErrStream, ch);
      }
    }
    strncpy (Prompt, RepAtom (AtPrompt)->StrOfAE, MAX_PROMPT);
    newline = FALSE;
  }
#if HAVE_SIGINTERRUPT
  siginterrupt(SIGINT, TRUE);
#endif
  Yap_PrologMode |= ConsoleGetcMode;
  ch = YP_fgetc(s->u.file.file);
#if HAVE_SIGINTERRUPT
  siginterrupt(SIGINT, FALSE);
#endif
  if (Yap_PrologMode & InterruptMode) {
    Yap_PrologMode &= ~InterruptMode;
    Yap_ProcessSIGINT();
    Yap_PrologMode &= ~ConsoleGetcMode;
    newline = TRUE;
    if (Yap_PrologMode & AbortMode) {
      Yap_Error(PURE_ABORT, TermNil, "");
      Yap_ErrorMessage = "Abort";
      return console_post_process_eof(s);
    }
    goto restart;
  } else {
    Yap_PrologMode &= ~ConsoleGetcMode;
  }
  if (ch == EOF)
    return console_post_process_eof(s);    
  return console_post_process_read_char(ch, s);
}

/* reads a character from a buffer and does the rest  */
static int
PlUnGetc (int sno)
{
  register StreamDesc *s = &Stream[sno];
  Int ch;

  if (s->stream_getc != PlUnGetc)
    return(s->stream_getc(sno));
  ch = s->och;
  if (s->status & Promptable_Stream_f) {
    s->stream_putc = ConsolePutc;
    s->stream_wputc = put_wchar;
#if HAVE_LIBREADLINE && HAVE_READLINE_READLINE_H
    if (s->status & Tty_Stream_f) {
      s->stream_getc = ReadlineGetc;
      if (Stream[0].status & Tty_Stream_f &&
	    is_same_tty(s->u.file.file,Stream[0].u.file.file))
	s->stream_putc = ReadlinePutc;
      s->stream_wputc = put_wchar;
    } else
#endif
      {
	s->stream_getc = ConsoleGetc;
      }
  } else {
    s->stream_getc = PlGetc;
    s->stream_gets = PlGetsFunc();
  }
  return(ch);
}

static int
utf8_nof(char ch)
{
  if (!(ch & 0x20))
    return 1;
  if (!(ch & 0x10))
    return 2;
  if (!(ch & 0x08))
    return 3;
  if (!(ch & 0x04))
    return 4;
  return 5;
}

static int
get_wchar(int sno)
{
  int ch;
  wchar_t wch;
  int how_many = 0;

  while (TRUE) {
    ch = Stream[sno].stream_getc(sno);
    if (ch == -1) {
      if (how_many) {
	/* error */
      }
      return EOF;
    }
    switch (Stream[sno].encoding) {
    case ENC_OCTET:
      return ch;
    case ENC_ISO_LATIN1:
      return ch;
    case ENC_ISO_ASCII:
      if (ch & 0x80) {
	/* error */
      }
      return ch;
    case ENC_ISO_ANSI:
      {
	char buf[1];
	int out;

	if (!how_many) {
	  memset((void *)&(Stream[sno].mbstate), 0, sizeof(mbstate_t));
	}
	buf[0] = ch;
	if ((out = mbrtowc(&wch, buf, 1, &(Stream[sno].mbstate))) == 1)
	  return wch;
	if (out == -1) {
	  /* error */
	}
	how_many++;
	break;
      }
    case ENC_ISO_UTF8:
      {
	if (!how_many) {
	  if (ch & 0x80) {
	    how_many = utf8_nof(ch);
	    /* 
	       keep a backup of the start character in case we meet an error,
	       useful if we are scanning ISO files.
	     */
	    Stream[sno].och = ch;
	    wch = (ch & ((1<<(6-how_many))-1))<<(6*how_many);
	  } else {
	    return ch;
	  }
	} else {
	  how_many--;
	  if ((ch & 0xc0) == 0x80) {
	    wch += (ch & ~0xc0) << (how_many*6);
	  } else {
	    /* error */
	    /* try to recover character, assume this is our first character */
	    wchar_t och = Stream[sno].och;
 
	    Stream[sno].och = ch;
	    Stream[sno].stream_getc = PlUnGetc;
	    Stream[sno].stream_wgetc = get_wchar;
	    Stream[sno].stream_gets = DefaultGets;
	    return och;
	  }
	  if (!how_many) {
	    return wch;
	  }
	}
      }
      break;
    case ENC_UNICODE_BE:
      if (how_many) {
	return wch+ch;
      }
      how_many=1;
      wch = ch << 8;
      break;
    case ENC_UNICODE_LE:
      if (how_many) {
	return wch+(ch<<8);
      }
      how_many=1;
      wch = ch;
      break;
    case ENC_ISO_UTF32_LE:
      if (!how_many) {
	how_many = 4;
	wch = 0;
      }
      how_many--;
      wch += ((unsigned char) (ch & 0xff)) << ((3-how_many)*8);
      if (how_many == 0)
	return wch;
      break;
    case ENC_ISO_UTF32_BE:
      if (!how_many) {
	how_many = 4;
	wch = 0;
      }
      how_many--;
      wch += ((unsigned char) (ch & 0xff)) << (how_many*8);
      if (how_many == 0)
	return wch;
      break;
    }
  }
  return EOF;
}

#ifndef MB_LEN_MAX
#define MB_LEN_MAX 6
#endif

static int
handle_write_encoding_error(int sno, wchar_t ch)
{
  if (Stream[sno].status & RepError_Xml_f) {
    /* use HTML/XML encoding in ASCII */
    int i = ch, digits = 1;
    Stream[sno].stream_putc(sno, '&');
    Stream[sno].stream_putc(sno, '#');
    while (digits < i)
      digits *= 10;
    if (digits > i)
      digits /= 10;
    while (i) {
      Stream[sno].stream_putc(sno, i/digits);
      i %= 10;
      digits /= 10;
    }
    Stream[sno].stream_putc(sno, ';');
    return ch;
  } else if (Stream[sno].status & RepError_Prolog_f) {
    /* write quoted */
    Stream[sno].stream_putc(sno, '\\');
    Stream[sno].stream_putc(sno, 'u');
    Stream[sno].stream_putc(sno, ch>>24);
    Stream[sno].stream_putc(sno, 256&(ch>>16));
    Stream[sno].stream_putc(sno, 256&(ch>>8));
    Stream[sno].stream_putc(sno, 256&ch);
    return ch;
  } else {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, MkIntegerTerm(ch),"charater %ld cannot be encoded in stream %d",(unsigned long int)ch,sno);
    return -1;
  }
}

static int
put_wchar(int sno, wchar_t ch)
{

  /* pass the bug if we can */
  switch (Stream[sno].encoding) {
  case ENC_OCTET:
    return Stream[sno].stream_putc(sno, ch);
  case ENC_ISO_LATIN1:
    if (ch >= 0xff) {
      return handle_write_encoding_error(sno,ch);
    }
    return Stream[sno].stream_putc(sno, ch);
  case ENC_ISO_ASCII:
    if (ch >= 0x80) {
      return handle_write_encoding_error(sno,ch);
    }
    return Stream[sno].stream_putc(sno, ch);
  case ENC_ISO_ANSI:
    {
      char buf[MB_LEN_MAX];
      int n;

      memset((void *)&(Stream[sno].mbstate), 0, sizeof(mbstate_t));
      if ( (n = wcrtomb(buf, ch, &(Stream[sno].mbstate))) < 0 ) {
	/* error */
	Stream[sno].stream_putc(sno, ch);
	return -1;
      } else {
	int i;

	for (i =0; i< n; i++) {
	  Stream[sno].stream_putc(sno, buf[i]);
	}
	return ch;
      }
    case ENC_ISO_UTF8:
      if (ch < 0x80) {
	return Stream[sno].stream_putc(sno, ch);
      } else if (ch < 0x800) {
	Stream[sno].stream_putc(sno, 0xC0 | ch>>6);
	return Stream[sno].stream_putc(sno, 0x80 | (ch & 0x3F));
      } 
      else if (ch < 0x10000) {
	Stream[sno].stream_putc(sno, 0xE0 | ch>>12);
	Stream[sno].stream_putc(sno, 0x80 | (ch>>6 & 0x3F));
	return Stream[sno].stream_putc(sno, 0x80 | (ch & 0x3F));
      } else if (ch < 0x200000) {
	Stream[sno].stream_putc(sno, 0xF0 | ch>>18);
	Stream[sno].stream_putc(sno, 0x80 | (ch>>12 & 0x3F));
	Stream[sno].stream_putc(sno, 0x80 | (ch>>6 & 0x3F));
	return Stream[sno].stream_putc(sno, 0x80 | (ch & 0x3F));
      } else {
	/* should never happen */
	return -1;
      }
      break;
    case ENC_UNICODE_BE:
      Stream[sno].stream_putc(sno, (ch>>8));
      return Stream[sno].stream_putc(sno, (ch&0xff));
    case ENC_UNICODE_LE:
      Stream[sno].stream_putc(sno, (ch&0xff));
      return Stream[sno].stream_putc(sno, (ch>>8));
    case ENC_ISO_UTF32_BE:
      Stream[sno].stream_putc(sno, (ch>>24) & 0xff);
      Stream[sno].stream_putc(sno, (ch>>16) &0xff);
      Stream[sno].stream_putc(sno, (ch>>8) & 0xff);
      return Stream[sno].stream_putc(sno, ch&0xff);
    case ENC_ISO_UTF32_LE:
      Stream[sno].stream_putc(sno, ch&0xff);
      Stream[sno].stream_putc(sno, (ch>>8) & 0xff);
      Stream[sno].stream_putc(sno, (ch>>16) &0xff);
      return Stream[sno].stream_putc(sno, (ch>>24) & 0xff);
    }
  }
  return -1;
}

/* used by user-code to read characters from the current input stream */
int
Yap_PlGetchar (void)
{
  return(Stream[Yap_c_input_stream].stream_getc(Yap_c_input_stream));
}

int
Yap_PlGetWchar (void)
{
  return get_wchar(Yap_c_input_stream);
}

/* avoid using a variable to call a function */
int
Yap_PlFGetchar (void)
{
  return(PlGetc(Yap_c_input_stream));
}


static Term
MkStream (int n)
{
  Term t[1];
  t[0] = MkIntTerm (n);
  return (Yap_MkApplTerm (FunctorStream, 1, t));
}

/* given a stream index, get the corresponding fd */
static Int
GetStreamFd(int sno)
{
  return(YP_fileno(Stream[sno].u.file.file));
}

Int
Yap_GetStreamFd(int sno)
{
  return GetStreamFd(sno);
}

/* given a socket file descriptor, get the corresponding stream descripor */
int
Yap_CheckIOStream(Term stream, char * error)
{
  int sno = CheckStream(stream, Input_Stream_f|Output_Stream_f, error);
  UNLOCK(Stream[sno].streamlock);
  return(sno);
}


#if _MSC_VER || defined(__MINGW32__) 
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif

static Int
p_access(void)
{
  Term tname = Deref(ARG1);
  char *file_name;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "access");
    return FALSE;
  } else if (!IsAtomTerm (tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "access");
    return FALSE;
  } else {
#if HAVE_STAT
    struct SYSTEM_STAT ss;

    file_name = RepAtom(AtomOfTerm(tname))->StrOfAE;
    if (SYSTEM_STAT(file_name, &ss) != 0) {
    /* ignore errors while checking a file */
      return FALSE;
    }
    return TRUE;
#else
    return FALSE;
#endif
  }
}

static Int
p_exists_directory(void)
{
  Term tname = Deref(ARG1);
  char *file_name;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "exists_directory/1");
    return FALSE;
  } else if (!IsAtomTerm (tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "exists_directory/1");
    return FALSE;
  } else {
#if HAVE_STAT
    struct SYSTEM_STAT ss;

    file_name = RepAtom(AtomOfTerm(tname))->StrOfAE;
    if (SYSTEM_STAT(file_name, &ss) != 0) {
    /* ignore errors while checking a file */
      return FALSE;
    }
    return (S_ISDIR(ss.st_mode));
#else
    return FALSE;
#endif
  }
}

static Int
p_file_expansion (void)
{				/* '$file_expansion'(+File,-Name)      */
  Term file_name = Deref(ARG1);

  /* we know file_name is bound */
  if (!IsAtomTerm (file_name)) {
    PlIOError(TYPE_ERROR_ATOM, file_name, "absolute_file_name/3");
    return(FALSE);
  }
  if (!Yap_TrueFileName (RepAtom (AtomOfTerm (file_name))->StrOfAE, Yap_FileNameBuf, FALSE))
    return (PlIOError (EXISTENCE_ERROR_SOURCE_SINK,file_name,"absolute_file_name/3"));
  return(Yap_unify(ARG2,MkAtomTerm(Yap_LookupAtom(Yap_FileNameBuf))));
}


Term
Yap_OpenStream(FILE *fd, char *name, Term file_name, int flags)
{
  Term t;
  StreamDesc *st;
  int sno;

  sno = GetFreeStreamD();
  if (sno  < 0)
    return (PlIOError (RESOURCE_ERROR_MAX_STREAMS,TermNil, "new stream not available for open_null_stream/1"));
  st = &Stream[sno];
  st->status = 0;
  if (flags & YAP_INPUT_STREAM)
    st->status |= Input_Stream_f;
  if (flags & YAP_OUTPUT_STREAM)
    st->status |= Output_Stream_f;
  if (flags & YAP_APPEND_STREAM)
    st->status |= Append_Stream_f;
  /*
    pipes assume an integer file descriptor, not a FILE *:
    if (flags & YAP_PIPE_STREAM)
    st->status |= Pipe_Stream_f;
  */
  if (flags & YAP_TTY_STREAM)
    st->status |= Tty_Stream_f;
  if (flags & YAP_POPEN_STREAM)
    st->status |= Popen_Stream_f;
  if (flags & YAP_BINARY_STREAM)
    st->status |= Binary_Stream_f;
  if (flags & YAP_SEEKABLE_STREAM)
    st->status |= Seekable_Stream_f;
  st->charcount = 0;
  st->linecount = 1;
  st->u.file.name = Yap_LookupAtom(name);
  st->u.file.user_name = file_name;
  st->u.file.file = fd;
  st->linepos = 0;
  st->stream_gets = PlGetsFunc();
  if (flags & YAP_TTY_STREAM) {
    st->stream_putc = ConsolePutc;
    st->stream_wputc = put_wchar;
    st->stream_getc = ConsoleGetc;
  } else {
    st->stream_putc = FilePutc;
    st->stream_wputc = put_wchar;
    st->stream_getc = PlGetc;
    unix_upd_stream_info (st);
  }
  st->stream_wgetc = get_wchar;
  if (CharConversionTable != NULL)
    st->stream_wgetc_for_read = ISOWGetc;
  else
    st->stream_wgetc_for_read = st->stream_wgetc; 
  UNLOCK(st->streamlock);
  t = MkStream (sno);
  return t;
}


static int
LookupSWIStream (struct io_stream *swi_s)
{
  int i = 0;

  while (i < MaxStreams) {
    LOCK(Stream[i].streamlock);
    if (Stream[i].status & SWI_Stream_f &&
	Stream[i].u.swi_stream.swi_ptr == swi_s
	) {
      UNLOCK(Stream[i].streamlock);
      return i;
    }
    UNLOCK(Stream[i].streamlock);
    i++;
  }
  i = GetFreeStreamD();
  if (i < 0)
    return i;
  Stream[i].u.swi_stream.swi_ptr = swi_s;
  Stream[i].status = SWI_Stream_f|Output_Stream_f|Input_Stream_f|Append_Stream_f|Tty_Stream_f|Promptable_Stream_f;
  Stream[i].linepos = 0;
  Stream[i].linecount = 1;
  Stream[i].charcount = 0;
  Stream[i].encoding = DefaultEncoding();
  Stream[i].stream_getc = IOSWIGetc;
  Stream[i].stream_putc = IOSWIPutc;
  Stream[i].stream_wputc = IOSWIWidePutc;
  Stream[i].stream_wgetc = IOSWIWideGetc;
  Stream[i].stream_gets = DefaultGets;
  if (CharConversionTable != NULL)
    Stream[i].stream_wgetc_for_read = ISOWGetc;
  else
    Stream[i].stream_wgetc_for_read = IOSWIWideGetc;
  UNLOCK(Stream[i].streamlock);
  return i;
}

int
Yap_LookupSWIStream (void *swi_s)
{
  return LookupSWIStream (swi_s);
}

typedef struct stream_ref
{ struct io_stream *read;
  struct io_stream *write;
} stream_ref;

extern stream_ref *PL_blob_data(Atom, void *, void *);

static int
CheckStream (Term arg, int kind, char *msg)
{
  int sno = -1;
  arg = Deref (arg);
  if (IsVarTerm (arg)) {
    Yap_Error(INSTANTIATION_ERROR, arg, msg);
    return -1;
  } else if (IsAtomTerm (arg)) {
    struct io_stream *swi_stream;
    Atom sname = AtomOfTerm(arg);

    if (IsBlob(sname)) {
      struct io_stream *s;
      stream_ref *ref;
	
      ref = PL_blob_data(sname, NULL, NULL);
      {
	if ( ref->read )
	  {
	    if ( ref->write && (kind&Output_Stream_f) )
	      s = ref->write;
	    else
	      s = ref->read;
	  } else
	  s = ref->write;
      }
      sno = LookupSWIStream(s);
      return sno;
    }

    if (Yap_get_stream_handle(arg, kind & Input_Stream_f, kind & Output_Stream_f, &swi_stream)) {
      sno = LookupSWIStream(swi_stream);
      return sno;
    }       
  }
  if (sno < 0)
    {
      Yap_Error(DOMAIN_ERROR_STREAM_OR_ALIAS, arg, msg);
      return (-1);
    }
  LOCK(Stream[sno].streamlock);
  if (Stream[sno].status & Free_Stream_f)
    {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(EXISTENCE_ERROR_STREAM, arg, msg);
      return (-1);
    }
  if ((Stream[sno].status & kind) == 0)
    {
      UNLOCK(Stream[sno].streamlock);
      if (kind & Input_Stream_f)
	Yap_Error(PERMISSION_ERROR_INPUT_STREAM, arg, msg);
      else
	Yap_Error(PERMISSION_ERROR_OUTPUT_STREAM, arg, msg);
      return (-1);
    }
  return (sno);
}

int
Yap_CheckStream (Term arg, int kind, char *msg)
{
  return CheckStream(arg, kind, msg);
}


#if  defined(YAPOR) || defined(THREADS)
void
Yap_LockStream (int sno)
{
  LOCK(Stream[sno].streamlock);
}

void
Yap_UnLockStream (int sno)
{
  UNLOCK(Stream[sno].streamlock);
}
#endif

static Term
StreamName(int i)
{
  if (i < 3) return(MkAtomTerm(AtomUser));
  return(Stream[i].u.file.user_name);
}


/*
 * Called when you want to close all open streams, except for stdin, stdout
 * and stderr 
 */
void
Yap_CloseStreams (int loud)
{
  int sno;
  for (sno = 3; sno < MaxStreams; ++sno) {
    if (Stream[sno].status & Free_Stream_f)
      continue;
    if ((Stream[sno].status & Popen_Stream_f))
      pclose (Stream[sno].u.file.file);
    else if (Stream[sno].status & (SWI_Stream_f)) {
      SWIClose(Stream[sno].u.swi_stream.swi_ptr);
    } else {
      YP_fclose (Stream[sno].u.file.file);
    }
    if (Yap_c_input_stream == sno) {
      Yap_c_input_stream = StdInStream;
    } else if (Yap_c_output_stream == sno) {
      Yap_c_output_stream = StdOutStream;
    }
    Stream[sno].status = Free_Stream_f;
  }
}


static void
CloseStream(int sno)
{
  if (!(Stream[sno].status & (SWI_Stream_f))) {
    YP_fclose (Stream[sno].u.file.file);
  } else if (Stream[sno].status & (SWI_Stream_f)) {
    SWIClose(Stream[sno].u.swi_stream.swi_ptr);
  }
  Stream[sno].status = Free_Stream_f;
  if (Yap_c_input_stream == sno)
    {
      Yap_c_input_stream = StdInStream;
    }
  else if (Yap_c_output_stream == sno)
    {
      Yap_c_output_stream = StdOutStream;
    }
}

void
Yap_CloseStream(int sno)
{
  CloseStream(sno);
}

static Int
p_close (void)
{				/* '$close'(+Stream) */
  Int sno = CheckStream (ARG1, (Input_Stream_f | Output_Stream_f), "close/2");
  if (sno < 0)
    return (FALSE);
  if (sno <= StdErrStream) {
    UNLOCK(Stream[sno].streamlock);
    return TRUE;
  }
  CloseStream(sno);
  UNLOCK(Stream[sno].streamlock);
  return (TRUE);
}

static Int
p_past_eof (void)
{				/* at_end_of_stream */
  /* the next character is a EOF */ 
  int sno = CheckStream (ARG1, Input_Stream_f, "past_eof/1");
  Int out;

  if (sno < 0)
    return (FALSE);
  if (Stream[sno].stream_getc == PlUnGetc) {
    UNLOCK(Stream[sno].streamlock);
    return FALSE;
  }
  out = Stream[sno].status & Eof_Stream_f;
  UNLOCK(Stream[sno].streamlock);
  return out;
}

static Int
p_has_bom (void)
{				/* '$set_output'(+Stream,-ErrorMessage)  */
  Int sno = CheckStream (ARG1, Input_Stream_f|Output_Stream_f, "has_bom/1");
  if (sno < 0)
    return (FALSE);
  UNLOCK(Stream[sno].streamlock);
  return ((Stream[sno].status & HAS_BOM_f));
}

static Int
p_representation_error (void)
{
  /* '$representation_error'(+Stream,-ErrorMessage)  */
  Term t;
  Int sno = CheckStream (ARG1, Input_Stream_f|Output_Stream_f, "representation_errors/1");
  if (sno < 0)
    return (FALSE);
  t = Deref(ARG2);

  if (IsVarTerm(t)) {
    UNLOCK(Stream[sno].streamlock);
    if (Stream[sno].status & RepError_Prolog_f) {
      return Yap_unify(ARG2, MkIntegerTerm(512));
    }
    if (Stream[sno].status & RepError_Xml_f) {
      return Yap_unify(ARG2, MkIntegerTerm(1024));
    }
    return Yap_unify(ARG2, MkIntegerTerm(0));    
  } else {
    Int i = IntegerOfTerm(t);
    switch (i) {
    case 512:
      Stream[sno].status &= ~RepError_Xml_f;
      Stream[sno].status |= RepError_Prolog_f;
      break;
    case 1024:
      Stream[sno].status &= ~RepError_Prolog_f;
      Stream[sno].status |= RepError_Xml_f;
    default:
      Stream[sno].status &= ~(RepError_Prolog_f|RepError_Xml_f);
    }
  }
  UNLOCK(Stream[sno].streamlock);
  return TRUE;
}

#ifdef BEAM
int beam_write (void)
{
  Yap_StartSlots();
  Yap_plwrite (ARG1, Stream[Yap_c_output_stream].stream_wputc, 0, 1200);
  Yap_CloseSlots();
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = 0L;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}
#endif

static Int
p_write (void)
{
    /* '$write'(+Flags,?Term) */
  int flags = (int) IntOfTerm (Deref (ARG1));
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  Yap_StartSlots();
  Yap_plwrite (ARG2, Stream[Yap_c_output_stream].stream_wputc, flags, 1200);
  Yap_CloseSlots();
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = NULL;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}

static Int
p_write_prio (void)
{
    /* '$write'(+Flags,?Term) */
  int flags = (int) IntOfTerm (Deref (ARG1));
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  Yap_StartSlots();
  Yap_plwrite (ARG3, Stream[Yap_c_output_stream].stream_wputc, flags, (int)IntOfTerm(Deref(ARG2)));
  Yap_CloseSlots();
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = NULL;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}

static Int
p_write2_prio (void)
{				/* '$write'(+Stream,+Flags,?Term) */
  int old_output_stream = Yap_c_output_stream;
  Int flags = IntegerOfTerm(Deref(ARG2));
  int stream_f;

  if (flags & Use_SWI_Stream_f) {
    stream_f = Output_Stream_f|SWI_Stream_f;
  } else {
    stream_f = Output_Stream_f;
  }
  Yap_c_output_stream = CheckStream (ARG1, stream_f, "write/2");
  if (Yap_c_output_stream == -1) {
    Yap_c_output_stream = old_output_stream;
    return(FALSE);
  }
  UNLOCK(Stream[Yap_c_output_stream].streamlock);
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  Yap_StartSlots();
  Yap_plwrite (ARG4, Stream[Yap_c_output_stream].stream_wputc, (int) flags, (int) IntOfTerm (Deref (ARG3)));
  Yap_CloseSlots();
  Yap_c_output_stream = old_output_stream;
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = NULL;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}

static Int
p_write2 (void)
{				/* '$write'(+Stream,+Flags,?Term) */
  int old_output_stream = Yap_c_output_stream;
  Yap_c_output_stream = CheckStream (ARG1, Output_Stream_f, "write/2");
  if (Yap_c_output_stream == -1) {
    Yap_c_output_stream = old_output_stream;
    return(FALSE);
  }
  UNLOCK(Stream[Yap_c_output_stream].streamlock);
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  Yap_StartSlots();
  Yap_plwrite (ARG3, Stream[Yap_c_output_stream].stream_wputc, (int) IntOfTerm (Deref (ARG2)), 1200);
  Yap_CloseSlots();
  Yap_c_output_stream = old_output_stream;
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = NULL;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}

static void
clean_vars(VarEntry *p)
{
  if (p == NULL) return;
  p->VarAdr = TermNil;
  clean_vars(p->VarLeft);
  clean_vars(p->VarRight);
}

static Term
syntax_error (TokEntry * tokptr, int sno, Term *outp)
{
  Term info;
  int count = 0, out = 0;
  Int start, err = 0, end;
  Term tf[7];
  Term *error = tf+3;
  CELL *Hi = H;

  /* make sure to globalise variable */
  Yap_unify(*outp, MkVarTerm());
  start = tokptr->TokPos;
  clean_vars(Yap_VarTable);
  clean_vars(Yap_AnonVarTable);
  while (1) {
    Term ts[2];

    if (H > ASP-1024) {
      tf[3] = TermNil;
      err = 0;
      end = 0;
      /* for some reason moving this earlier confuses gcc on solaris */
      H = Hi;
      break;
    }
    if (tokptr == Yap_toktide) {
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
    case Number_tok:
      ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomNumber,1),1,&(tokptr->TokInfo));
      break;
    case Var_tok:
      {
	Term t[3];
	VarEntry *varinfo = (VarEntry *)info;

	t[0] = MkIntTerm(0);
	t[1] = Yap_StringToList(varinfo->VarRep);
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
	Term t0 = Yap_StringToList((char *)info);
	ts[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomString,1),1,&t0);
      }
      break;
    case WString_tok:
      {
	Term t0 = Yap_WideStringToList((wchar_t *)info);
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
	if (Ord (info) == 'l') {
	  s[0] = '(';
	} else  {
	  s[0] = (char)info;
	}
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
  if (IsVarTerm(*outp) && (VarOfTerm(*outp) > H || VarOfTerm(*outp) < H0)) {
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
  tf[6] = StreamName(sno);
  return(Yap_MkApplTerm(FunctorSyntaxError,7,tf));
}

Int
Yap_FirstLineInParse (void)
{
  return StartLine;
}

static Int
p_startline (void)
{
  return (Yap_unify_constant (ARG1, MkIntegerTerm (StartLine)));
}

/* control the parser error handler */
static Int
p_set_read_error_handler(void)
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
p_get_read_error_handler(void)
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
Yap_readTerm(int sno, Term *tp, Term *varnames, Term *terror, Term *tpos)
{
  TokEntry *tokstart;
  Term pt;

  if (sno < 0) {
    return FALSE;
  }
  tokstart = Yap_tokptr = Yap_toktide = Yap_tokenizer(sno, tpos);
  if (Yap_ErrorMessage)
    {
      Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
      if (terror)
	*terror = MkAtomTerm(Yap_LookupAtom(Yap_ErrorMessage));
      Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
      return FALSE;
    }
  pt = Yap_Parse();
  if (Yap_ErrorMessage) {
    Term t0 = MkVarTerm();
    *terror = syntax_error(tokstart, sno, &t0);
      Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
      return FALSE;
  }
  if (varnames) {
    *varnames = Yap_VarNames(Yap_VarTable, TermNil);
    if (!*varnames) {
      Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
      return FALSE;
    }
  }
  *tp = pt;
  return TRUE;
}

/*
  Assumes
  Flag: ARG1
  Term: ARG2
  Module: ARG3
  Vars: ARG4
  Pos: ARG5
  Err: ARG6
 */
static Int
  do_read(int inp_stream, int nargs)
{
  Term t, v;
  TokEntry *tokstart;
#if EMACS
  int emacs_cares = FALSE;
#endif
  Term tmod = Deref(ARG3), OCurrentModule = CurrentModule, tpos;

  if (IsVarTerm(tmod)) {
    tmod = CurrentModule;
  } else if (!IsAtomTerm(tmod)) {
    Yap_Error(TYPE_ERROR_ATOM, tmod, "read_term/2");
    return FALSE;
  }
  if (Stream[inp_stream].status & Binary_Stream_f) {
    Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, MkAtomTerm(Stream[inp_stream].u.file.name), "read_term/2");
    return FALSE;
  }
  Yap_Error_TYPE = YAP_NO_ERROR;
  tpos = StreamPosition(inp_stream);
  if (!Yap_unify(tpos,ARG5)) {
    /* do this early so that we do not have to protect it in case of stack expansion */  
    return FALSE;
  }
  while (TRUE) {
    CELL *old_H;
    UInt cpos = 0;
    int seekable = Stream[inp_stream].status & Seekable_Stream_f;
#if HAVE_FGETPOS
    fpos_t rpos;
#endif
    int ungetc_oldc = 0;
    int had_ungetc = FALSE;

    /* two cases where we can seek: memory and console */
    if (seekable) {
      if (Stream[inp_stream].stream_getc == PlUnGetc) {
	had_ungetc = TRUE;
	ungetc_oldc = Stream[inp_stream].och;
      }
      cpos = Stream[inp_stream].charcount;
    }
    /* Scans the term using stack space */
    while (TRUE) {
      old_H = H;
      Yap_eot_before_eof = FALSE;
      tpos = StreamPosition(inp_stream);
      tokstart = Yap_tokptr = Yap_toktide = Yap_tokenizer(inp_stream, &tpos);
      if (Yap_Error_TYPE != YAP_NO_ERROR && seekable) {
	H = old_H;
	Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
	if (had_ungetc) {
	  Stream[inp_stream].stream_getc = PlUnGetc;
	  Stream[inp_stream].och = ungetc_oldc;
	}
	if (seekable) {
	  if (Stream[inp_stream].status) {
#if HAVE_FGETPOS
	      fsetpos(Stream[inp_stream].u.file.file, &rpos);
#else
	      fseek(Stream[inp_stream].u.file.file, cpos, 0L);
#endif
	  }
	}
	if (Yap_Error_TYPE == OUT_OF_TRAIL_ERROR) {
	  Yap_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_growtrail (sizeof(CELL) * K16, FALSE)) {
	    return FALSE;
	  }
	} else if (Yap_Error_TYPE == OUT_OF_AUXSPACE_ERROR) {
	  Yap_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	    return FALSE;
	  }
	} else if (Yap_Error_TYPE == OUT_OF_HEAP_ERROR) {
	  Yap_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_growheap(FALSE, 0, NULL)) {
	    return FALSE;
	  }
	} else if (Yap_Error_TYPE == OUT_OF_STACK_ERROR) {
	  Yap_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_gcl(Yap_Error_Size, nargs, ENV, CP)) {
	    return FALSE;
	  }
	}
      } else {
	/* done with this */
	break;
      }
    }
    Yap_Error_TYPE = YAP_NO_ERROR;
    /* preserve value of H after scanning: otherwise we may lose strings
       and floats */
    old_H = H;
    if (Stream[inp_stream].status & Eof_Stream_f) {
      if (Yap_eot_before_eof) {
	/* next read should give out an end of file */
	Stream[inp_stream].status |= Push_Eof_Stream_f;
      } else {
	if (tokstart != NULL && tokstart->Tok != Ord (eot_tok)) {
	  /* we got the end of file from an abort */
	  if (Yap_ErrorMessage &&
	      !strcmp(Yap_ErrorMessage,"Abort")) {
	    Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
	    return FALSE;
	  }
	  /* we need to force the next reading to also give end of file.*/
	  Stream[inp_stream].status |= Push_Eof_Stream_f;
	  Yap_ErrorMessage = "end of file found before end of term";
	} else {
	  Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
	
	  return Yap_unify_constant(ARG2, MkAtomTerm (AtomEof))
	    && Yap_unify_constant(ARG4, TermNil);
	}
      }
    }
  repeat_cycle:
    CurrentModule = tmod;
    if (Yap_ErrorMessage || (t = Yap_Parse()) == 0) {
      CurrentModule = OCurrentModule;
      if (Yap_ErrorMessage) {
	int res;

	if (!strcmp(Yap_ErrorMessage,"Stack Overflow") ||
	    !strcmp(Yap_ErrorMessage,"Trail Overflow") ||
	    !strcmp(Yap_ErrorMessage,"Heap Overflow")) {
	  /* ignore term we just built */
	  tr_fr_ptr old_TR = TR;


	  H = old_H;
	  TR = (tr_fr_ptr)ScannerStack;
	  
	  if (!strcmp(Yap_ErrorMessage,"Stack Overflow"))
	    res = Yap_growstack_in_parser(&old_TR, &tokstart, &Yap_VarTable);
	  else if (!strcmp(Yap_ErrorMessage,"Heap Overflow"))
	    res = Yap_growheap_in_parser(&old_TR, &tokstart, &Yap_VarTable);
	  else
	    res = Yap_growtrail_in_parser(&old_TR, &tokstart, &Yap_VarTable);
	  if (res) {
	    ScannerStack = (char *)TR;
	    TR = old_TR;
	    old_H = H;
	    Yap_tokptr = Yap_toktide = tokstart;
	    Yap_ErrorMessage = NULL;
	    goto repeat_cycle;
	  }
	  ScannerStack = (char *)TR;
	  TR = old_TR;
	}
      }
      if (ParserErrorStyle == QUIET_ON_PARSER_ERROR) {
	/* just fail */
	Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
	return FALSE;
      } else if (ParserErrorStyle == CONTINUE_ON_PARSER_ERROR) {
	Yap_ErrorMessage = NULL;
	/* try again */
	goto repeat_cycle;
      } else {
	Term terr = syntax_error(tokstart, inp_stream, &ARG2);
	if (Yap_ErrorMessage == NULL)
	  Yap_ErrorMessage = "SYNTAX ERROR";
	
	if (ParserErrorStyle == EXCEPTION_ON_PARSER_ERROR) {
	  Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
	  Yap_Error(SYNTAX_ERROR,terr,Yap_ErrorMessage);
	  return FALSE;
	} else /* FAIL ON PARSER ERROR */ {
	  Term t[2];
	  t[0] = terr;
	  t[1] = MkAtomTerm(Yap_LookupAtom(Yap_ErrorMessage));
	  Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
	  return Yap_unify(ARG6,Yap_MkApplTerm(Yap_MkFunctor(AtomError,2),2,t));
	}
      }
    } else {
      CurrentModule = OCurrentModule;
      /* parsing succeeded */
      break;
    }
  }
#if EMACS
  first_char = tokstart->TokPos;
#endif /* EMACS */
  if (!Yap_unify(t, ARG2))
    return FALSE;
  if (AtomOfTerm (Deref (ARG1)) == AtomTrue) {
    while (TRUE) {
      CELL *old_H = H;

      if (setjmp(Yap_IOBotch) == 0) {
	v = Yap_VarNames(Yap_VarTable, TermNil);
	break;
      } else {
	tr_fr_ptr old_TR;
	restore_machine_regs();

	old_TR = TR;
	/* restart global */
	H = old_H;
	TR = (tr_fr_ptr)ScannerStack;
	Yap_growstack_in_parser(&old_TR, &tokstart, &Yap_VarTable);
	ScannerStack = (char *)TR;
	TR = old_TR;
      }
    }
    Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
    return Yap_unify (v, ARG4);
  } else {
    Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
    return TRUE;
  }
}

static Int
p_read (void)
{				/* '$read'(+Flag,?Term,?Module,?Vars,-Pos,-Err)    */
  return do_read(Yap_c_input_stream, 6);
}

static Int
p_read2 (void)
{				/* '$read2'(+Flag,?Term,?Module,?Vars,-Pos,-Err,+Stream)  */
  int inp_stream;
  Int out;

  /* needs to change Yap_c_output_stream for write */
  inp_stream = CheckStream (ARG7, Input_Stream_f, "read/3");
  if (inp_stream == -1) {
    return(FALSE);
  }
  UNLOCK(Stream[inp_stream].streamlock);
  out = do_read(inp_stream, 7);
  return out;
}

static Int
p_user_file_name (void)
{
  Term tout;
  int sno = CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f,"user_file_name/2");
  if (sno < 0)
    return (FALSE);
  tout = Stream[sno].u.file.user_name;
  UNLOCK(Stream[sno].streamlock);
  return (Yap_unify_constant (ARG2, tout));
}

static Term
StreamPosition(int sno)
{
  Term sargs[5];
  Int cpos;
  cpos = Stream[sno].charcount;
  if (Stream[sno].status & SWI_Stream_f) {
    return Yap_get_stream_position(Stream[sno].u.swi_stream.swi_ptr);
  }
  if (Stream[sno].stream_getc == PlUnGetc) {
    cpos--;
  }
  sargs[0] = MkIntegerTerm (cpos);
  sargs[1] = MkIntegerTerm (StartLine = Stream[sno].linecount);
  sargs[2] = MkIntegerTerm (Stream[sno].linepos);
  sargs[3] = sargs[4] = MkIntTerm (0);
  return Yap_MkApplTerm (FunctorStreamPos, 5, sargs);
}


Term
Yap_StreamPosition(int sno)
{
  return StreamPosition(sno);
}

static Int
p_show_stream_position (void)
{				/* '$show_stream_position'(+Stream,Pos) */
  Term tout;
  int sno =
    CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f, "stream_position/2");
  if (sno < 0)
    return (FALSE);
  tout = StreamPosition(sno);
  UNLOCK(Stream[sno].streamlock);
  return Yap_unify (ARG2, tout);
}

static Int
p_set_stream_position (void)
{				/* '$set_stream_position'(+Stream,Pos) */
  Term tin, tp;
  Int char_pos;
  int sno = CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f, "set_stream_position/2");
  if (sno < 0) {
    return (FALSE);
  }
  tin = Deref (ARG2);
  if (IsVarTerm (tin)) {
    UNLOCK(Stream[sno].streamlock);
    Yap_Error(INSTANTIATION_ERROR, tin, "set_stream_position/2");
    return (FALSE);
  } else if (!(IsApplTerm (tin))) {
    UNLOCK(Stream[sno].streamlock);
    Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
    return (FALSE);
  }
  if (FunctorOfTerm (tin) == FunctorStreamPos) {
    if (IsVarTerm (tp = ArgOfTerm (1, tin))) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(INSTANTIATION_ERROR, tp, "set_stream_position/2");
      return (FALSE);    
    } else if (!IsIntTerm (tp)) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
      return (FALSE);
    }
    if (!(Stream[sno].status & Seekable_Stream_f) ) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(PERMISSION_ERROR_REPOSITION_STREAM, ARG1,"set_stream_position/2");
      return(FALSE);
    }
    char_pos = IntOfTerm (tp);
    if (IsVarTerm (tp = ArgOfTerm (2, tin))) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(INSTANTIATION_ERROR, tp, "set_stream_position/2");
      return (FALSE);    
    } else if (!IsIntTerm (tp)) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
      return (FALSE);
    }
    Stream[sno].charcount = char_pos;
    Stream[sno].linecount = IntOfTerm (tp);
    if (IsVarTerm (tp = ArgOfTerm (3, tin))) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(INSTANTIATION_ERROR, tp, "set_stream_position/2");
      return (FALSE);    
    } else if (!IsIntTerm (tp)) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
      return (FALSE);
    }
    Stream[sno].linepos = IntOfTerm (tp);
    if (YP_fseek (Stream[sno].u.file.file, (long) (char_pos), 0) == -1) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(SYSTEM_ERROR, tp, 
	    "fseek failed for set_stream_position/2");
      return(FALSE);
    }
    Stream[sno].stream_getc = PlGetc;
    Stream[sno].stream_gets = PlGetsFunc();
  } else if (FunctorOfTerm (tin) == FunctorStreamEOS) {
    if (IsVarTerm (tp = ArgOfTerm (1, tin))) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(INSTANTIATION_ERROR, tp, "set_stream_position/2");
      return (FALSE);    
    } else if (tp != MkAtomTerm(AtomAt)) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
      return (FALSE);
    }
    if (!(Stream[sno].status & Seekable_Stream_f) ) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(PERMISSION_ERROR_REPOSITION_STREAM, ARG1,"set_stream_position/2");
      return(FALSE);
    }
    if (YP_fseek (Stream[sno].u.file.file, 0L, SEEK_END) == -1) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(SYSTEM_ERROR, tp, 
	    "fseek failed for set_stream_position/2");
      return(FALSE);
    }
    Stream[sno].stream_getc = PlGetc;
    Stream[sno].stream_gets = PlGetsFunc();
    /* reset the counters */
    Stream[sno].linepos = 0;
    Stream[sno].linecount = 1;
    Stream[sno].charcount = 0;
  }
  UNLOCK(Stream[sno].streamlock);
  return (TRUE);
}

static Term
read_line(int sno) 
{
  Term tail;
  Int ch;

  if ((ch = Stream[sno].stream_wgetc(sno)) == 10) {
    return(TermNil);
  }
  tail = read_line(sno);
  return(MkPairTerm(MkIntTerm(ch),tail));
}

static Int
p_get0_line_codes (void)
{				/* '$get0'(Stream,-N)                    */
  int sno = CheckStream (ARG1, Input_Stream_f, "get0/2");
  Int status;
  Term out;
  Int ch = '\0';
  int rewind;

  if (sno < 0)
    return(FALSE);
  if (Stream[sno].stream_getc == PlUnGetc) {
    ch = PlUnGetc(sno);
    rewind = TRUE;
  } else {
    rewind = FALSE;
  }
  status = Stream[sno].status;
  if (status & Binary_Stream_f) {
    UNLOCK(Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, ARG1, "get0/2");
    return FALSE;
  }
  UNLOCK(Stream[sno].streamlock);
  out = read_line(sno);
  if (rewind) 
    return Yap_unify(MkPairTerm(MkIntegerTerm(ch),out), ARG2);
  else
    return Yap_unify(out,ARG2);
}

#define FORMAT_MAX_SIZE 256

typedef struct {
  Int pos;                   /* tab point */
  char pad;                  /* ok, it's not standard english */
} pads;

typedef struct format_status {
  int format_error;
  char *format_ptr, *format_base, *format_max;
  int   format_buf_size;
  pads pad_entries[16], *pad_max;
} format_info;

static int
format_putc(int sno, wchar_t ch) {
  if (FormatInfo->format_buf_size == -1)
    return EOF;
  if (ch == 10) {
    char *ptr = FormatInfo->format_base;
#if MAC || _MSC_VER
    ch = '\n';
#endif
    for (ptr = FormatInfo->format_base; ptr < FormatInfo->format_ptr; ptr++) {
      Stream[sno].stream_putc(sno, *ptr);
    }
    /* reset line */
    FormatInfo->format_ptr = FormatInfo->format_base;
    FormatInfo->pad_max = FormatInfo->pad_entries;
    Stream[sno].stream_putc(sno, '\n');
    return((int)10);
  } else {
    *FormatInfo->format_ptr++ = (char)ch;
    if (FormatInfo->format_ptr == FormatInfo->format_max) {
      /* oops, we have reached an overflow */
      Int new_max_size = FormatInfo->format_buf_size + FORMAT_MAX_SIZE;
      char *newbuf;

      if ((newbuf = Yap_AllocAtomSpace(new_max_size*sizeof(char))) == NULL) {
	FormatInfo->format_buf_size = -1;
	Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow heap for format/2");
	return(EOF);
      }
#if HAVE_MEMMOVE
      memmove((void *)newbuf, (void *)FormatInfo->format_base, (size_t)((FormatInfo->format_ptr-FormatInfo->format_base)*sizeof(char)));
#else
      {
	Int n = FormatInfo->format_ptr-FormatInfo->format_base;
	char *to = newbuf;
	char *from = FormatInfo->format_base;
	while (n-- >= 0) {
	  *to++ = *from++;
	}
      }
#endif
      Yap_FreeAtomSpace(FormatInfo->format_base);
      FormatInfo->format_ptr = newbuf+(FormatInfo->format_ptr-FormatInfo->format_base);
      FormatInfo->format_base = newbuf;
      FormatInfo->format_max = newbuf+new_max_size;
      FormatInfo->format_buf_size = new_max_size;
      if (ActiveSignals & YAP_CDOVF_SIGNAL) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap at format");
	}
      }
    }
  }
  return ((int) ch);
}

static void fill_pads(int nchars)
{
  int nfillers, fill_space, lfill_space;

  if (nchars <= 0) return; /* ignore */
  nfillers = FormatInfo->pad_max-FormatInfo->pad_entries;
  if (nfillers == 0) {
    /* OK, just pad with spaces */
    while (nchars--) {
      *FormatInfo->format_ptr++ = ' ';
    }    
    return;
  }
  fill_space = nchars/nfillers;
  lfill_space = nchars%nfillers;

  if (fill_space) {
    pads *padi = FormatInfo->pad_max;

    while (padi > FormatInfo->pad_entries) {
      char *start_pos;
      int n, i;
      padi--;
      start_pos = FormatInfo->format_base+padi->pos;
      n = FormatInfo->format_ptr-start_pos;

#if HAVE_MEMMOVE
      memmove((void *)(start_pos+fill_space), (void *)start_pos, (size_t)(n*sizeof(char)));
#else
      {
	char *to = start_pos+(fill_space+n);
	char *from = FormatInfo->format_ptr;

	while (n-- > 0) {
	  *--to = *--from;
	}
      }
#endif
      FormatInfo->format_ptr += fill_space;
      for (i = 0; i < fill_space; i++) {
	*start_pos++ = padi->pad;
      }
    }
  }
  while (lfill_space--) {
    *FormatInfo->format_ptr++ = FormatInfo->pad_max[-1].pad;
  }
}

static int
format_print_str (Int sno, Int size, Int has_size, Term args, int (* f_putc)(int, wchar_t))
{
  Term arghd;
  while (!has_size || size > 0) {
    if (IsVarTerm(args)) {
      Yap_Error(INSTANTIATION_ERROR, args, "format/2");
      return FALSE;
    } else if (args == TermNil) {
      return TRUE;
    }
    else if (!IsPairTerm (args)) {
      Yap_Error(TYPE_ERROR_LIST, args, "format/2");
      return FALSE;
    }
    arghd = HeadOfTerm (args);
    args = TailOfTerm (args);
    if (IsVarTerm(arghd)) {
      Yap_Error(INSTANTIATION_ERROR, arghd, "format/2");
      return FALSE;
    } else if (!IsIntTerm (arghd)) {
      Yap_Error(TYPE_ERROR_LIST, arghd, "format/2");
      return FALSE;
    }
    f_putc(sno, (int) IntOfTerm (arghd));
    size--;
  }
  return TRUE;
}

typedef enum {
  fst_ok,
  fst_error,
  fst_too_long
} format_cp_res;

static format_cp_res
copy_format_string(Term inp, char *out, int max)
{
  int i = 0;
  while (inp != TermNil) {
    Term hd;
    int ch;

    if (IsVarTerm(inp)) {
      Yap_Error(INSTANTIATION_ERROR,inp,"format/2");
      return fst_error;
    }
    if (!IsPairTerm(inp)) {
      Yap_Error(TYPE_ERROR_LIST,inp,"format/2");
      return fst_error;
    }
    hd = HeadOfTerm(inp);
    if (IsVarTerm(hd)) {
      Yap_Error(INSTANTIATION_ERROR,hd,"format/2");
      return fst_error;
    }
    if (!IsIntTerm(hd)) {
      Yap_Error(TYPE_ERROR_INTEGER,hd,"format/2");
      return fst_error;
    }
    ch = IntOfTerm(hd);
    if (ch < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,hd,"format/2");
      return fst_error;
    }
    if (i+1 == max) {
      return fst_too_long;
    }
    /* we've got a character */
    out[i++] = ch;
    /* done */
    inp = TailOfTerm(inp);
  }
  out[i] = '\0';
  return fst_ok;
}

#define FORMAT_COPY_ARGS_ERROR    -1
#define FORMAT_COPY_ARGS_OVERFLOW -2

static Int
format_copy_args(Term args, Term *targs, Int tsz)
{
  Int n = 0;
  while (args != TermNil) {
    if (IsVarTerm(args)) {
      Yap_Error(INSTANTIATION_ERROR,args,"format/2");
      return FORMAT_COPY_ARGS_ERROR;
    }
    if (!IsPairTerm(args)) {
      Yap_Error(TYPE_ERROR_LIST,args,"format/2");
      return FORMAT_COPY_ARGS_ERROR;
    }
    if (n == tsz)
      return FORMAT_COPY_ARGS_OVERFLOW;
    targs[n] = HeadOfTerm(args);
    args = TailOfTerm(args);
    n++;
  }
  return n;
  
}

static void
format_clean_up(char *format_base, char *fstr, Term *targs)
{
  if (format_base)
    Yap_FreeAtomSpace(format_base);
  if (fstr)
    Yap_FreeAtomSpace(fstr);
  if (targs)
    Yap_FreeAtomSpace((char *)targs);
}

static Int
fetch_index_from_args(Term t)
{
  Int i;

  if (IsVarTerm(t))
    return -1;
  if (!IsIntegerTerm(t))
    return -1;
  i = IntegerOfTerm(t);
  if (i < 0)
    return -1;
  return i;
}

static int
format_has_tabs(const char *seq)
{
  int ch;

  while ((ch = *seq++)) {
    if (ch == '~') {
      ch = *seq++;
      if (ch == 'p' || ch == '@') {
	return TRUE;
      }
      if (ch == '*') {
	ch = *seq++;
      } else {
	while (ch >= '0' && ch <= '9') ch = *seq++;
      }
      if (ch == 't' || ch == '|' || ch == '+') {
	return TRUE;
      }
      if (!ch)
	return FALSE;
    }
  }
  return FALSE;
}

static wchar_t
base_dig(Int dig, Int ch)
{
  if (dig < 10) 
    return dig+'0';
  else if (ch == 'r')
    return (dig-10)+'a';
  else /* ch == 'R' */
    return (dig-10)+'A';
}

#define TMP_STRING_SIZE 1024

static Int
format(volatile Term otail, volatile Term oargs, int sno)
{
  char tmp1[TMP_STRING_SIZE], *tmpbase;
  int ch;
  int column_boundary;
  Term mytargs[8], *targs;
  Int tnum, targ;
  char *fstr = NULL, *fptr;
  Term args;
  Term tail;
  int (* f_putc)(int, wchar_t);
  int has_tabs;
  volatile void *old_handler;
  format_info finfo;
  Term fmod = CurrentModule;


  FormatInfo = &finfo;
  finfo.pad_max = finfo.pad_entries;
  finfo.format_error = FALSE;
  old_handler = NULL;
  args = oargs;
  tail = otail;
  targ = 0;
  column_boundary = 0;
  if (IsVarTerm(tail)) {
    Yap_Error(INSTANTIATION_ERROR,tail,"format/2");
    return(FALSE);
  } else if (IsPairTerm (tail)) {
    int sz = 256;
    do {
      format_cp_res fr;

      fstr = fptr = Yap_AllocAtomSpace(sz*sizeof(char));
      if ((fr = copy_format_string(tail, fstr, sz)) == fst_ok)
	break;
      if (fr == fst_error) return FALSE;
      sz += 256;
      Yap_FreeCodeSpace(fstr);
    } while (TRUE);
  } else if (IsAtomTerm(tail)) {
    fstr = fptr = RepAtom(AtomOfTerm(tail))->StrOfAE;
  } else {
    Yap_Error(CONSISTENCY_ERROR, tail, "format/2");
    return FALSE;
  }
  if (IsVarTerm(args)) {
    Yap_Error(INSTANTIATION_ERROR, args, "format/2");
    return FALSE;
  } 
  while (IsApplTerm(args) && FunctorOfTerm(args) == FunctorModule) {
    fmod = ArgOfTerm(1,args);
    args = ArgOfTerm(2,args);
    if (IsVarTerm(fmod)) {
      Yap_Error(INSTANTIATION_ERROR, fmod, "format/2");
      return FALSE;
    }
    if (!IsAtomTerm(fmod)) {
      Yap_Error(TYPE_ERROR_ATOM, fmod, "format/2");
      return FALSE;
    }
    if (IsVarTerm(args)) {
      Yap_Error(INSTANTIATION_ERROR, args, "format/2");
      return FALSE;
    }
  } 
  if (IsPairTerm(args)) {
    Int tsz = 8;

    targs = mytargs;
    do {
      tnum = format_copy_args(args, targs, tsz);
      if (tnum == FORMAT_COPY_ARGS_ERROR)
	return FALSE;
      else if (tnum == FORMAT_COPY_ARGS_OVERFLOW) {
	if (mytargs != targs) {
	  Yap_FreeCodeSpace((char *)targs);
	}
	tsz += 16;
	targs = (Term *)Yap_AllocAtomSpace(tsz*sizeof(Term));
      } else {
	break;
      }
    } while (TRUE);
  } else if (args != TermNil) {
    tnum = 1;
    mytargs[0] = args;
    targs = mytargs;
  } else {
    tnum = 0;
    targs = mytargs;
  }
  finfo.format_error = FALSE;

  if ((has_tabs = format_has_tabs(fptr))) {
    finfo.format_base = finfo.format_ptr = Yap_AllocAtomSpace(FORMAT_MAX_SIZE*sizeof(char));
    finfo.format_max = finfo.format_base+FORMAT_MAX_SIZE;
    if (finfo.format_ptr == NULL) {
      Yap_Error(INSTANTIATION_ERROR,tail,"format/2");
      return(FALSE);
    }  
    finfo.format_buf_size = FORMAT_MAX_SIZE;
    f_putc = format_putc;
  } else {
    f_putc = Stream[sno].stream_wputc;
    finfo.format_base = NULL;
  }
  while ((ch = *fptr++)) {
    Term t = TermNil;
    int has_repeats = FALSE;
    int repeats = 0;

    if (ch == '~') {
      /* start command */
      ch = *fptr++;
      if (ch == '*') {
	ch = *fptr++;
	has_repeats = TRUE;
	if (targ > tnum-1) {
	  goto do_consistency_error;
	}
	repeats = fetch_index_from_args(targs[targ++]);
	if (repeats == -1)
	  goto do_consistency_error;
      } else if (ch == '`') {
	/* next character is kept as code */
	has_repeats = TRUE;
	repeats = *fptr++;
	ch = *fptr++;
      } else if (ch >= '0' && ch <= '9') {
	has_repeats = TRUE;
	repeats = 0;
	while (ch >= '0' && ch <= '9') {
	  repeats = repeats*10+(ch-'0');
	  ch = *fptr++;
	}
      }
      switch (ch) {
      case 'a':
	/* print an atom */
	if (has_repeats || targ > tnum-1)
	  goto do_consistency_error;
	t = targs[targ++];
	if (IsVarTerm(t))
	  goto do_instantiation_error;
	if (!IsAtomTerm(t))
	  goto do_type_atom_error;
	Yap_StartSlots();
	Yap_plwrite (t, f_putc, Handle_vars_f|To_heap_f, 1200);
	Yap_CloseSlots();
	FormatInfo = &finfo;
	break;
      case 'c':
	{
	  Int nch, i;

	  if (targ > tnum-1)
	    goto do_consistency_error;
	  t = targs[targ++];
	  if (IsVarTerm(t))
	    goto do_instantiation_error;
	  if (!IsIntegerTerm(t))
	    goto do_type_int_error;
	  nch = IntegerOfTerm(t);
	  if (nch < 0)
	    goto do_domain_not_less_zero_error;
	  if (!has_repeats)
	    repeats = 1;
	  for (i = 0; i < repeats; i++)
	    f_putc(sno, nch);
	  break;
	}
      case 'e':
      case 'E':
      case 'f':
      case 'g':
      case 'G':
	{
	  Float fl;
	  char *ptr;

	  if (targ > tnum-1)
	    goto do_consistency_error;
	  t = targs[targ++];
	  if (IsVarTerm(t))
	    goto do_instantiation_error;
	  if (!IsNumTerm(t))
	    goto do_type_number_error;
	  if (IsIntegerTerm(t)) {
	    fl = (Float)IntegerOfTerm(t);
#ifdef USE_GMP
	  } else if (IsBigIntTerm(t)) {
	    fl = Yap_gmp_to_float(t);
#endif
	  } else {
	    fl = FloatOfTerm(t);
	  }
	  if (!has_repeats)
	    repeats = 6;
	  tmp1[0] = '%';
	  tmp1[1] = '.';
	  ptr = tmp1+2;
#if HAVE_SNPRINTF
	  snprintf(ptr,256-5,"%d",repeats);
#else
	  sprintf(ptr,"%d",repeats);
#endif
	  while (*ptr) ptr++;
	  ptr[0] = ch;
	  ptr[1] = '\0';
	  {
	    char *tmp2;
	    if (!(tmp2 = Yap_AllocCodeSpace(repeats+10)))
	      goto do_type_int_error;
#if HAVE_SNPRINTF
	    snprintf (tmp2, repeats+10, tmp1, fl);
#else
	    sprintf (tmp2, tmp1, fl);
#endif
	    ptr = tmp2;
	    while ((ch = *ptr++) != 0)
	      f_putc(sno, ch);
	    Yap_FreeCodeSpace(tmp2);
	  }
	  break;
	case 'd':
	case 'D':
	  /* print a decimal, using weird . stuff */
	  if (targ > tnum-1)
	    goto do_consistency_error;
	  t = targs[targ++];
	  if (IsVarTerm(t))
	    goto do_instantiation_error;
	  if (!IsIntegerTerm(t)
#ifdef USE_GMP
	      && !IsBigIntTerm(t)
#endif

	      )
	    goto do_type_int_error;

	  {
	    Int siz = 0;
	    char *ptr = tmp1;
	    tmpbase = tmp1;

	    if (IsIntegerTerm(t)) {
	      Int il = IntegerOfTerm(t);
#if HAVE_SNPRINTF
	      snprintf(tmp1, 256, "%ld", (long int)il);
#else
	      sprintf(tmp1, "%ld", (long int)il);
#endif
	      siz = strlen(tmp1);
	      if (il < 0)  siz--;
#ifdef USE_GMP
	    } else if (IsBigIntTerm(t) && RepAppl(t)[1] == BIG_INT) {
	      char *res;

	      tmpbase = tmp1;

	      while (!(res = Yap_gmp_to_string(t, tmpbase, TMP_STRING_SIZE, 10))) {
		if (tmpbase == tmp1) {
		  tmpbase = NULL;
		} else {
		  tmpbase = res;
		  goto do_type_int_error;
		}
	      }
	      tmpbase = res;
	      ptr = tmpbase;
#endif
	      siz = strlen(tmpbase);
	    } else {
	      goto do_type_int_error;
	    }
	      
	    if (tmpbase[0] == '-') {
	      f_putc(sno, (int) '-');
	      ptr++;
	    }
	    if (ch == 'D') {
	      int first = TRUE;

	      while (siz > repeats) {
		if ((siz-repeats) % 3 == 0 &&
		    !first) {
		  f_putc(sno, (int) ',');
		}
		f_putc(sno, (int) (*ptr++));
		first = FALSE;
		siz--;
	      }
	    } else {
	      while (siz > repeats) {
		f_putc(sno, (int) (*ptr++));
		siz--;
	      }
	    }
	    if (repeats) {
	      if (ptr == tmpbase ||
		  ptr[-1] == '-') {
		f_putc(sno, (int) '0');
	      }
	      f_putc(sno, (int) '.');
	      while (repeats > siz) {
		f_putc(sno, (int) '0');
		repeats--;
	      }
	      while (repeats) {
		f_putc(sno, (int) (*ptr++));
		repeats--;
	      }
	    }
	    if (tmpbase != tmp1)
	      free(tmpbase);
	    break;
	  case 'r':
	  case 'R':
	    {
	      Int numb, radix;
	      UInt divfactor = 1, size = 1, i;
	      wchar_t och;

	      /* print a decimal, using weird . stuff */
	      if (targ > tnum-1)
		goto do_consistency_error;
	      t = targs[targ++];
	      if (IsVarTerm(t))
		goto do_instantiation_error;
	      if (!has_repeats)
		radix = 8;
	      else
		radix = repeats;
	      if (radix > 36 || radix < 2)
		goto do_domain_error_radix;
#ifdef USE_GMP
	      if (IsBigIntTerm(t) && RepAppl(t)[1] == BIG_INT) {
		char *pt, *res;

		tmpbase = tmp1;
		while (!(res = Yap_gmp_to_string(t, tmpbase, TMP_STRING_SIZE, radix))) {
		  if (tmpbase == tmp1) {
		    tmpbase = NULL;
		  } else {
		    tmpbase = res;
		    goto do_type_int_error;
		  }
		}
		tmpbase = res;
		pt = tmpbase;
		while ((ch = *pt++))
		  f_putc(sno, ch);
		if (tmpbase != tmp1)
		  free(tmpbase);
		break;
	      }
#endif
	      if (!IsIntegerTerm(t))
		goto do_type_int_error;
	      numb = IntegerOfTerm(t);
	      if (numb < 0) {
		numb = -numb;
		f_putc(sno, (int) '-');
	      }
	      while (numb/divfactor >= radix) {
		divfactor *= radix;
		size++;
	      }
	      for (i = 1; i < size; i++) {
		Int dig = numb/divfactor;
		och = base_dig(dig, ch);
		f_putc(sno, och);
		numb %= divfactor;
		divfactor /= radix;
	      }
	      och = base_dig(numb, ch);
	      f_putc(sno, och);
	      break;
	    }
	  case 's':
	    if (targ > tnum-1)
	      goto do_consistency_error;
	    t = targs[targ++];
	    if (!format_print_str (sno, repeats, has_repeats, t, f_putc)) {
	      goto do_default_error;
	    }
	    break;
	  case 'i':
	    if (targ > tnum-1 || has_repeats)
	      goto do_consistency_error;
	    targ++;
	    break;
	  case 'k':
	    if (targ > tnum-1 || has_repeats)
	      goto do_consistency_error;
	    t = targs[targ++];
	    Yap_StartSlots();
	    Yap_plwrite (t, f_putc, Quote_illegal_f|Ignore_ops_f|To_heap_f , 1200);
	    Yap_CloseSlots();
	    FormatInfo = &finfo;
	    break;
	  case '@':
	    t = targs[targ++];
	    Yap_StartSlots();
	    { 
	      Int sl = Yap_InitSlot(args);
	      Int sl2;
	      Int res;
	      Term ta[2];
	      Term ts;

	      ta[0] = fmod;
	      ta[1] = t;
	      ta[0] = Yap_MkApplTerm(FunctorModule, 2, ta);
	      ta[1] = MkVarTerm();
	      sl2 = Yap_InitSlot(ta[1]);
	      ts = Yap_MkApplTerm(FunctorGFormatAt, 2, ta);
	      res = Yap_execute_goal(ts, 0, CurrentModule);
	      FormatInfo = &finfo;
	      args = Yap_GetFromSlot(sl);
	      if (EX) goto ex_handler;
	      if (!res) return FALSE;
	      ts = Yap_GetFromSlot(sl2);
	      Yap_RecoverSlots(2);
	      if (!format_print_str (sno, repeats, has_repeats, ts, f_putc)) {
		goto do_default_error;
	      }
	    }
	    Yap_CloseSlots();
	    break;
	  case 'p':
	    if (targ > tnum-1 || has_repeats)
	      goto do_consistency_error;
	    t = targs[targ++];
	    Yap_StartSlots();
	    { 
	      Int sl = Yap_InitSlot(args);
	      Yap_plwrite(t, f_putc, Handle_vars_f|Use_portray_f|To_heap_f, 1200);
	      FormatInfo = &finfo;
	      args = Yap_GetFromSlot(sl);
	      Yap_RecoverSlots(1);
	    }
	    Yap_CloseSlots();
	    if (EX != 0L) {
	      Term ball;

	    ex_handler:
	      ball = Yap_PopTermFromDB(EX);
	      EX = NULL;
	      if (tnum <= 8)
		targs = NULL;
	      if (IsAtomTerm(tail)) {
		fstr = NULL;
	      }
	      format_clean_up(finfo.format_base, fstr, targs);
	      Yap_JumpToEnv(ball);
	      return FALSE;
	    }
	    break;
	  case 'q':
	    if (targ > tnum-1 || has_repeats)
	      goto do_consistency_error;
	    t = targs[targ++];
	    Yap_StartSlots();
	    Yap_plwrite (t, f_putc, Handle_vars_f|Quote_illegal_f|To_heap_f, 1200);
	    Yap_CloseSlots();
	    FormatInfo = &finfo;
	    break;
	  case 'w':
	    if (targ > tnum-1 || has_repeats)
	      goto do_consistency_error;
	    t = targs[targ++];
	    Yap_StartSlots();
	    Yap_plwrite (t, f_putc, Handle_vars_f|To_heap_f, 1200);
	    Yap_CloseSlots();
	    FormatInfo = &finfo;
	    break;
	  case '~':
	    if (has_repeats)
	      goto do_consistency_error;
	    f_putc(sno, (int) '~');
	    break;
	  case 'n':
	    if (!has_repeats)
	      repeats = 1;
	    while (repeats--) {
	      f_putc(sno, (int) '\n');
	    }
	    column_boundary = 0;
	    finfo.pad_max = finfo.pad_entries;
	    break;
	  case 'N':
	    if (!has_repeats)
	      has_repeats = 1;
	    if (Stream[sno].linepos != 0) {
	      f_putc(sno, (int) '\n');
	      column_boundary = 0;
	      finfo.pad_max = finfo.pad_entries;
	    }
	    if (repeats > 1) {
	      Int i;
	      for (i = 1; i < repeats; i++)
		f_putc(sno, (int) '\n');
	      column_boundary = 0;
	      finfo.pad_max = finfo.pad_entries;
	    }
	    break;
	    /* padding */
	  case '|':
	    if (has_repeats) {
	      fill_pads(repeats-(finfo.format_ptr-finfo.format_base));
	    }
	    finfo.pad_max = finfo.pad_entries;
	    if (repeats) 
	      column_boundary =  repeats;
	    else
	      column_boundary = finfo.format_ptr-finfo.format_base;
	    break;
	  case '+':
	    if (has_repeats) {
	      fill_pads((repeats+column_boundary)-(finfo.format_ptr-finfo.format_base));
	    } else {
	      repeats = 8;
	      fill_pads(8);
	    }
	    finfo.pad_max = finfo.pad_entries;
	    column_boundary = repeats+column_boundary;
	    break;
	  case 't':
	    if (!has_repeats)
	      finfo.pad_max->pad = ' ';
	    else
	      finfo.pad_max->pad = fptr[-2];
	    finfo.pad_max->pos = finfo.format_ptr-finfo.format_base;
	    finfo.pad_max++;
	    f_putc = format_putc;
	    break;
	  do_instantiation_error:
	    Yap_Error_TYPE = INSTANTIATION_ERROR;
	    goto do_default_error;
	  do_type_int_error:
	    Yap_Error_TYPE = TYPE_ERROR_INTEGER;
	    goto do_default_error;
	  do_type_number_error:
	    Yap_Error_TYPE = TYPE_ERROR_NUMBER;
	    goto do_default_error;
	  do_type_atom_error:
	    Yap_Error_TYPE = TYPE_ERROR_ATOM;
	    goto do_default_error;
	  do_domain_not_less_zero_error:
	    Yap_Error_TYPE = DOMAIN_ERROR_NOT_LESS_THAN_ZERO;
	    goto do_default_error;
	  do_domain_error_radix:
	    Yap_Error_TYPE = DOMAIN_ERROR_RADIX;
	    goto do_default_error;
	  do_consistency_error:
	  default:
	    Yap_Error_TYPE = CONSISTENCY_ERROR;
	  do_default_error:
	    if (tnum <= 8)
	      targs = NULL;
	    if (IsAtomTerm(tail)) {
	      fstr = NULL;
	    }
	    {
	      Term ta[2];
	      ta[0] = otail;
	      ta[1] = oargs;
	      Yap_Error(Yap_Error_TYPE, Yap_MkApplTerm(Yap_MkFunctor(AtomFormat,2),2,ta), "format/2");
	    }
	    format_clean_up(finfo.format_base, fstr, targs);
	    Yap_Error_TYPE = YAP_NO_ERROR;
	    return FALSE;
	  }
	}
      /* ok, now we should have a command */
      }
    } else {
      f_putc(sno, ch);
    }
  }
  if (has_tabs) {
    for (fptr = finfo.format_base; fptr < finfo.format_ptr; fptr++) {
      Stream[sno].stream_putc(sno, *fptr);
    }
  }
  if (IsAtomTerm(tail)) {
    fstr = NULL;
  }
  if (tnum <= 8)
    targs = NULL;
  format_clean_up(finfo.format_base, fstr, targs);
  return (TRUE);
}

static Int
p_format(void)
{				/* 'format'(Control,Args)               */
  Int res;
  res = format(Deref(ARG1),Deref(ARG2), Yap_c_output_stream);
  return res;
}

static Int
format2(UInt  stream_flag)
{
  int old_c_stream = Yap_c_output_stream;
  Int out;
  Term tin = Deref(ARG1);

  if (IsVarTerm(tin)) {
    Yap_Error(INSTANTIATION_ERROR,tin,"format/3");
    return FALSE;
  }
  /* needs to change Yap_c_output_stream for write */
  Yap_c_output_stream = CheckStream (ARG1, Output_Stream_f|stream_flag, "format/3");
  UNLOCK(Stream[Yap_c_output_stream].streamlock);
  if (Yap_c_output_stream == -1) {
    Yap_c_output_stream = old_c_stream;  
    return FALSE;
  }
  out = format(Deref(ARG2),Deref(ARG3),Yap_c_output_stream);
 {
    Yap_c_output_stream = old_c_stream;  
  }
  return out;
}

static Int
p_format2(void)
{				/* 'format'(Stream,Control,Args)          */
  return format2(0);
}

static Int
p_swi_format(void)
{				/* 'format'(Stream,Control,Args)          */
  return format2(SWI_Stream_f);
}


static Int
p_skip (void)
{				/* '$skip'(Stream,N)                     */
  int sno = CheckStream (ARG1, Input_Stream_f, "skip/2");
  Int n = IntOfTerm (Deref (ARG2));
  int ch;

  if (sno < 0)
    return (FALSE);
  if (n < 0 || n > 127) {
    UNLOCK(Stream[sno].streamlock);
    return (FALSE);
  }
  UNLOCK(Stream[sno].streamlock);
  while ((ch = Stream[sno].stream_wgetc(sno)) != n && ch != -1);
  return (TRUE);
}

void Yap_FlushStreams(void)
{
}

#if HAVE_SELECT
/* stream_select(+Streams,+TimeOut,-Result)      */
static Int
p_stream_select(void)
{
  Term t1 = Deref(ARG1), t2;
  fd_set readfds, writefds, exceptfds;
  struct timeval timeout, *ptime;

#if _MSC_VER
  u_int fdmax=0;
#else
  int fdmax=0;
#endif
  Term tout = TermNil, ti, Head;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR,t1,"stream_select/3");
    return FALSE;
  }
  if (!IsPairTerm(t1)) {
    Yap_Error(TYPE_ERROR_LIST,t1,"stream_select/3");
    return(FALSE);
  }
  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  FD_ZERO(&exceptfds);
  ti = t1;
  while (ti != TermNil) {
#if _MSC_VER
    u_int fd;
#else
    int fd;
#endif
    int sno;

    Head = HeadOfTerm(ti);
    sno  = CheckStream(Head, Input_Stream_f, "stream_select/3");
    if (sno < 0)
      return(FALSE);
    fd = GetStreamFd(sno);
    FD_SET(fd, &readfds);
    UNLOCK(Stream[sno].streamlock);
    if (fd > fdmax)
      fdmax = fd;
    ti = TailOfTerm(ti);
  }
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR,t2,"stream_select/3");
    return(FALSE);
  }
  if (IsAtomTerm(t2)) {
    if (t2 == MkAtomTerm(AtomOff)) {
      /* wait indefinitely */
      ptime = NULL;
    } else {
      Yap_Error(DOMAIN_ERROR_TIMEOUT_SPEC,t1,"stream_select/3");
      return(FALSE);
    }
  } else {
    Term t21, t22;

    if (!IsApplTerm(t2) || FunctorOfTerm(t2) != FunctorModule) {
      Yap_Error(DOMAIN_ERROR_TIMEOUT_SPEC,t2,"stream_select/3");
      return(FALSE);
    }
    t21 = ArgOfTerm(1, t2);
    if (IsVarTerm(t21)) {
      Yap_Error(INSTANTIATION_ERROR,t2,"stream_select/3");
      return(FALSE);
    }
    if (!IsIntegerTerm(t21)) {
      Yap_Error(DOMAIN_ERROR_TIMEOUT_SPEC,t2,"stream_select/3");
      return(FALSE);
    }
    timeout.tv_sec = IntegerOfTerm(t21);
    if (timeout.tv_sec < 0) {
      Yap_Error(DOMAIN_ERROR_TIMEOUT_SPEC,t2,"stream_select/3");
      return(FALSE);
    }
    t22 = ArgOfTerm(2, t2);
    if (IsVarTerm(t22)) {
      Yap_Error(INSTANTIATION_ERROR,t2,"stream_select/3");
      return(FALSE);
    }
    if (!IsIntegerTerm(t22)) {
      Yap_Error(DOMAIN_ERROR_TIMEOUT_SPEC,t2,"stream_select/3");
      return(FALSE);
    }
    timeout.tv_usec = IntegerOfTerm(t22);
    if (timeout.tv_usec < 0) {
      Yap_Error(DOMAIN_ERROR_TIMEOUT_SPEC,t2,"stream_select/3");
      return(FALSE);
    }
    ptime = &timeout;
  }
  /* do the real work */
  if (select(fdmax+1, &readfds, &writefds, &exceptfds, ptime) < 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR, TermNil, 
	    "stream_select/3 (select: %s)", strerror(errno));
#else
      Yap_Error(SYSTEM_ERROR, TermNil,
	    "stream_select/3 (select)");
#endif
  }
  while (t1 != TermNil) {
    int fd;
    int sno;

    Head = HeadOfTerm(t1);
    sno  = CheckStream(Head, Input_Stream_f, "stream_select/3");
    fd = GetStreamFd(sno);
    if (FD_ISSET(fd, &readfds))
      tout = MkPairTerm(Head,tout);
    else 
      tout = MkPairTerm(TermNil,tout);
    UNLOCK(Stream[sno].streamlock);
    t1 = TailOfTerm(t1);
  }
  /* we're done, just pass the info back */
  return(Yap_unify(ARG3,tout));

}
#endif

static Int
p_write_depth (void)
{				/* write_depth(Old,New)          */
  Term t1 = Deref (ARG1);
  Term t2 = Deref (ARG2);
  Term t3 = Deref (ARG3);

  if (!IsVarTerm (t1) && !IsIntegerTerm (t1)) {
    Yap_Error(TYPE_ERROR_INTEGER,t1,"write_depth/3");
    return FALSE;
  }
  if (!IsVarTerm (t2) && !IsIntegerTerm (t2)) {
    Yap_Error(TYPE_ERROR_INTEGER,t2,"write_depth/3");
    return FALSE;
  }
  if (!IsVarTerm (t3) && !IsIntegerTerm (t3)) {
    Yap_Error(TYPE_ERROR_INTEGER,t3,"write_depth/3");
    return FALSE;
  }
  if (IsVarTerm (t1))
    {
      Term t = MkIntegerTerm (max_depth);
      if (!Yap_unify_constant(t1, t))
	return FALSE;
    }
  else
    max_depth = IntegerOfTerm (t1);
  if (IsVarTerm (t2))
    {
      Term t = MkIntegerTerm (max_list);
      if (!Yap_unify_constant (t2, t))
	return FALSE;
    }
  else
    max_list = IntegerOfTerm (t2);
  if (IsVarTerm (t3))
    {
      Term t = MkIntegerTerm (max_write_args);
      if (!Yap_unify_constant (t3, t))
	return FALSE;
    }
  else
    max_write_args = IntegerOfTerm (t3);
  return TRUE;
}

static Int
p_change_type_of_char (void)
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
p_type_of_char (void)
{				/* type_of_char(+char,-type)      */
  Term t;

  Term t1 = Deref (ARG1);
  if (!IsVarTerm (t1) && !IsIntegerTerm (t1))
    return FALSE;
  t = MkIntTerm(Yap_chtype[IntegerOfTerm (t1)]);
  return Yap_unify(t,ARG2);
}


static Int 
p_force_char_conversion(void)
{
  int i;

  /* don't actually enable it until someone tries to add a conversion */
  if (CharConversionTable2 == NULL)
    return(TRUE);
  for (i = 0; i < MaxStreams; i++) {
    if (!(Stream[i].status & Free_Stream_f))
	Stream[i].stream_wgetc_for_read = ISOWGetc;
  }
  CharConversionTable = CharConversionTable2;
  return(TRUE);
}

static Int 
p_disable_char_conversion(void)
{
  int i;

  for (i = 0; i < MaxStreams; i++) {
    if (!(Stream[i].status & Free_Stream_f))
      Stream[i].stream_wgetc_for_read = Stream[i].stream_wgetc;
  }
  CharConversionTable = NULL;
  return(TRUE);
}

static Int 
p_char_conversion(void)
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
    int i;

    /* don't create a table if we don't need to */
    if (s0[0] == s1[0])
      return(TRUE);
    CharConversionTable2 = Yap_AllocCodeSpace(NUMBER_OF_CHARS*sizeof(char));
    while (CharConversionTable2 == NULL) {
      if (!Yap_growheap(FALSE, NUMBER_OF_CHARS*sizeof(char), NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      }
    }
    if (yap_flags[CHAR_CONVERSION_FLAG] != 0) {
      if (p_force_char_conversion() == FALSE)
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
p_current_char_conversion(void)
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
p_all_char_conversions(void)
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

Int
Yap_StreamToFileNo(Term t)
{
  int sno  =
    CheckStream(t, (Input_Stream_f|Output_Stream_f), "StreamToFileNo");
  {
    UNLOCK(Stream[sno].streamlock);
    return(YP_fileno(Stream[sno].u.file.file));
  }
}

static Int
p_stream(void)
{
  Term in = Deref(ARG1);
  if (IsVarTerm(in))
    return(FALSE);
  if (IsApplTerm(in))
    return(FunctorOfTerm(in) == FunctorStream);
  return(FALSE);
}

static Int
p_same_file(void) {
  char *f1 = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
  char *f2 = RepAtom(AtomOfTerm(Deref(ARG2)))->StrOfAE;

  if (strcmp(f1,f2) == 0)
    return TRUE;
#if HAVE_LSTAT 
  {
    int out;
    struct stat *b1, *b2;
    while ((char *)H+sizeof(struct stat)*2 > (char *)(ASP-1024)) {
      if (!Yap_gcl(2*sizeof(struct stat), 2, ENV, gc_P(P,CP))) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	return FALSE;
      }
    }
    b1 = (struct stat *)H;
    b2 = b1+1;
    if (strcmp(f1,"user_input") == 0) {
      if (fstat(fileno(Stream[0].u.file.file), b1) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return FALSE;
      }
    } else   if (strcmp(f1,"user_output") == 0) {
      if (fstat(fileno(Stream[1].u.file.file), b1) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return FALSE;    
      }
    } else   if (strcmp(f1,"user_error") == 0) {
      if (fstat(fileno(Stream[2].u.file.file), b1) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return FALSE;    
      }
    } else if (stat(f1, b1) == -1) {
      /* file does not exist, but was opened? Return -1 */
      return FALSE;
    }
    if (strcmp(f2,"user_input") == 0) {
      if (fstat(fileno(Stream[0].u.file.file), b2) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return FALSE;
      }
    } else   if (strcmp(f2,"user_output") == 0) {
      if (fstat(fileno(Stream[1].u.file.file), b2) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return FALSE;    
      }
    } else   if (strcmp(f2,"user_error") == 0) {
      if (fstat(fileno(Stream[2].u.file.file), b2) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return FALSE;    
      }
    } else if (stat(f2, b2) == -1) {
      /* file does not exist, but was opened? Return -1 */
      return FALSE;
    }
    out = (b1->st_ino == b2->st_ino
#ifdef __LCC__
	   && memcmp((const void *)&(b1->st_dev),(const void *)&(b2->st_dev),sizeof(buf1.st_dev)) == 0
#else
	   && b1->st_dev == b2->st_dev
#endif
		  );
    return out;
  }
#else
  return(FALSE);
#endif
}

static Int
p_float_format(void)
{
  Term in = Deref(ARG1);
  if (IsVarTerm(in))
    return Yap_unify(ARG1, MkAtomTerm(AtomFloatFormat));
  AtomFloatFormat = AtomOfTerm(in);
  return TRUE;
}

static Int
p_get_default_encoding(void)
{
  Term out = MkIntegerTerm(DefaultEncoding());
  return Yap_unify(ARG1, out);
}

static Int
p_toupper(void)
{
  Int out = IntegerOfTerm(Deref(ARG1)), uout;
  if (out < 0) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE, ARG1, "toupper");
    return FALSE;
  }
  if (out < 128)
    uout = toupper(out);
  else
    uout = towupper(out);
  return Yap_unify(ARG2, MkIntegerTerm(uout));
}

static Int
p_tolower(void)
{
  Int out = IntegerOfTerm(Deref(ARG1)), uout;
  if (out < 0) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE, ARG1, "tolower");
    return FALSE;
  }
  if (out < 128)
    uout = tolower(out);
  else
    uout = towlower(out);
  return Yap_unify(ARG2, MkIntegerTerm(uout));
}

static Int
p_encoding (void)
{				/* '$encoding'(Stream,N)                      */
  int sno = CheckStream (ARG1, Input_Stream_f|Output_Stream_f, "encoding/2");
  Term t = Deref(ARG2);
  if (sno < 0)
    return FALSE;
  if (IsVarTerm(t)) {
    UNLOCK(Stream[sno].streamlock);
    return Yap_unify(ARG2, MkIntegerTerm(Stream[sno].encoding));
  }
  Stream[sno].encoding = IntegerOfTerm(Deref(ARG2));
  UNLOCK(Stream[sno].streamlock);
  return TRUE;
}


FILE *
Yap_FileDescriptorFromStream(Term t)
{
  int sno = CheckStream (t, Input_Stream_f|Output_Stream_f, "FileDescriptorFromStream");
  if (sno < 0)
    return NULL;
  if (Stream[sno].status & (Free_Stream_f))
    return NULL;
  return Stream[sno].u.file.file;
}

void
Yap_InitBackIO (void)
{
}


void
Yap_InitIOPreds(void)
{
  Term cm = CurrentModule;

  Yap_stdin = stdin;
  Yap_stdout = stdout;
  Yap_stderr = stderr;
  if (!Stream)
    Stream = (StreamDesc *)Yap_AllocCodeSpace(sizeof(StreamDesc)*MaxStreams);
  /* here the Input/Output predicates */
  Yap_InitCPred ("$close", 1, p_close, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$get0_line_codes", 2, p_get0_line_codes, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$access", 1, p_access, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("exists_directory", 1, p_exists_directory, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$file_expansion", 2, p_file_expansion, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$set_read_error_handler", 1, p_set_read_error_handler, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$get_read_error_handler", 1, p_get_read_error_handler, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$read", 6, p_read, SyncPredFlag|HiddenPredFlag|UserCPredFlag);
  Yap_InitCPred ("$read", 7, p_read2, SyncPredFlag|HiddenPredFlag|UserCPredFlag);
  Yap_InitCPred ("$skip", 2, p_skip, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$write", 2, p_write, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$write", 3, p_write2, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$write_with_prio", 3, p_write_prio, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$write_with_prio", 4, p_write2_prio, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("format", 2, p_format, SyncPredFlag);
  Yap_InitCPred ("format", 3, p_format2, SyncPredFlag);
  Yap_InitCPred ("$start_line", 1, p_startline, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$show_stream_position", 2, p_show_stream_position, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$set_stream_position", 2, p_set_stream_position, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$user_file_name", 2, p_user_file_name, SafePredFlag|SyncPredFlag),
  Yap_InitCPred ("$past_eof", 1, p_past_eof, SafePredFlag|SyncPredFlag),
  Yap_InitCPred ("$has_bom", 1, p_has_bom, SafePredFlag);
  Yap_InitCPred ("$stream_representation_error", 2, p_representation_error, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$is_same_tty", 2, p_is_same_tty, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("always_prompt_user", 0, p_always_prompt_user, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("write_depth", 3, p_write_depth, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$change_type_of_char", 2, p_change_type_of_char, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$type_of_char", 2, p_type_of_char, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("char_conversion", 2, p_char_conversion, SyncPredFlag);
  Yap_InitCPred ("$current_char_conversion", 2, p_current_char_conversion, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$all_char_conversions", 1, p_all_char_conversions, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$force_char_conversion", 0, p_force_char_conversion, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$disable_char_conversion", 0, p_disable_char_conversion, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$stream", 1, p_stream, SafePredFlag|TestPredFlag);
  Yap_InitCPred ("$get_default_encoding", 1, p_get_default_encoding, SafePredFlag|TestPredFlag);
  Yap_InitCPred ("$encoding", 2, p_encoding, SafePredFlag|SyncPredFlag),
#if HAVE_SELECT
  Yap_InitCPred ("stream_select", 3, p_stream_select, SafePredFlag|SyncPredFlag);
#endif
  Yap_InitCPred ("$same_file", 2, p_same_file, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$float_format", 1, p_float_format, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$has_readline", 0, p_has_readline, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred ("$toupper", 2, p_toupper, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred ("$tolower", 2, p_tolower, SafePredFlag|HiddenPredFlag);

  CurrentModule = SYSTEM_MODULE;
  Yap_InitCPred ("swi_format", 3, p_swi_format, SyncPredFlag);
  CurrentModule = cm;

  Yap_InitReadUtil ();
  InitPlIO ();
#if HAVE_LIBREADLINE && HAVE_READLINE_READLINE_H
  InitReadline();
#endif
}
