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
#include "Heap.h"
#include "yapio.h"
#include <stdlib.h>
#if HAVE_STDARG_H
#include <stdarg.h>
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
#if HAVE_IO_H
/* Windows */
#include <io.h>
#endif
#if !HAVE_STRNCAT
#define strncat(X,Y,Z) strcat(X,Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X,Y,Z) strcpy(X,Y)
#endif
#if _MSC_VER || defined(__MINGW32__) 
#include <windows.h>
#endif
#if _MSC_VER || defined(__MINGW32__) 
#if USE_SOCKET
#include <winsock2.h>
#endif
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR)==_S_IFDIR)
#endif
#endif
#include "iopreds.h"

STATIC_PROTO (Int PlIOError, (yap_error_number, Term, char *));
STATIC_PROTO (int FilePutc, (int, int));
STATIC_PROTO (int MemPutc, (int, int));
STATIC_PROTO (int console_post_process_read_char, (int, StreamDesc *));
STATIC_PROTO (int post_process_read_char, (int, StreamDesc *));
#if USE_SOCKET
STATIC_PROTO (int SocketPutc, (int, int));
STATIC_PROTO (int ConsoleSocketPutc, (int, int));
#endif
STATIC_PROTO (int PipePutc, (int, int));
STATIC_PROTO (int ConsolePipePutc, (int, int));
STATIC_PROTO (int NullPutc, (int, int));
STATIC_PROTO (int ConsolePutc, (int, int));
STATIC_PROTO (Int p_setprompt, (void));
STATIC_PROTO (Int p_prompt, (void));
STATIC_PROTO (int PlGetc, (int));
STATIC_PROTO (int MemGetc, (int));
STATIC_PROTO (int ISOGetc, (int));
STATIC_PROTO (int ConsoleGetc, (int));
STATIC_PROTO (int PipeGetc, (int));
STATIC_PROTO (int ConsolePipeGetc, (int));
#if USE_SOCKET
STATIC_PROTO (int SocketGetc, (int));
STATIC_PROTO (int ConsoleSocketGetc, (int));
#endif
#if HAVE_LIBREADLINE
STATIC_PROTO (int ReadlineGetc, (int));
STATIC_PROTO (int ReadlinePutc, (int,int));
#endif
STATIC_PROTO (int PlUnGetc, (int));
STATIC_PROTO (Term MkStream, (int));
STATIC_PROTO (Int p_stream_flags, (void));
STATIC_PROTO (int find_csult_file, (char *, char *, StreamDesc *, char *));
STATIC_PROTO (Int p_open, (void));
STATIC_PROTO (int AddAlias, (Atom, int));
STATIC_PROTO (void SetAlias, (Atom, int));
STATIC_PROTO (void PurgeAlias, (int));
STATIC_PROTO (int CheckAlias, (Atom));
STATIC_PROTO (Atom FetchAlias, (int));
STATIC_PROTO (int FindAliasForStream, (int, Atom));
STATIC_PROTO (int CheckStream, (Term, int, char *));
STATIC_PROTO (Int p_check_stream, (void));
STATIC_PROTO (Int p_check_if_stream, (void));
STATIC_PROTO (Int init_cur_s, (void));
STATIC_PROTO (Int cont_cur_s, (void));
STATIC_PROTO (Int p_close, (void));
STATIC_PROTO (Int p_set_input, (void));
STATIC_PROTO (Int p_set_output, (void));
STATIC_PROTO (Int p_current_input, (void));
STATIC_PROTO (Int p_current_output, (void));
STATIC_PROTO (Int p_write, (void));
STATIC_PROTO (Int p_write2, (void));
STATIC_PROTO (Int p_set_read_error_handler, (void));
STATIC_PROTO (Int p_get_read_error_handler, (void));
STATIC_PROTO (Int p_read, (void));
STATIC_PROTO (Int p_cur_line_no, (void));
STATIC_PROTO (Int p_get, (void));
STATIC_PROTO (Int p_get0, (void));
STATIC_PROTO (Int p_get_byte, (void));
STATIC_PROTO (Int p_peek, (void));
STATIC_PROTO (Int p_past_eof, (void));
STATIC_PROTO (Int p_put, (void));
STATIC_PROTO (Int p_put_byte, (void));
STATIC_PROTO (Int p_skip, (void));
STATIC_PROTO (Int p_flush, (void));
STATIC_PROTO (Int p_flush_all_streams, (void));
STATIC_PROTO (Int p_write_depth, (void));
STATIC_PROTO (Int p_open_null_stream, (void));
STATIC_PROTO (Int p_user_file_name, (void));
STATIC_PROTO (Int p_line_position, (void));
STATIC_PROTO (Int p_character_count, (void));
STATIC_PROTO (Int p_show_stream_flags, (void));
STATIC_PROTO (Int p_show_stream_position, (void));
STATIC_PROTO (Int p_set_stream_position, (void));
STATIC_PROTO (Int p_add_alias_to_stream, (void));
STATIC_PROTO (Int p_change_alias_to_stream, (void));
STATIC_PROTO (Int p_check_if_valid_new_alias, (void));
STATIC_PROTO (Int p_fetch_stream_alias, (void));
STATIC_PROTO (Int GetArgSizeFromThirdArg, (char **, Term *));
STATIC_PROTO (Int GetArgSizeFromChars, (char **pptr, Int *, Term *));
STATIC_PROTO (void format_print_str, (Int, Int, Term));
STATIC_PROTO (Int p_format, (void));
STATIC_PROTO (Int p_startline, (void));
STATIC_PROTO (Int p_change_type_of_char, (void));
STATIC_PROTO (Int p_type_of_char, (void));
STATIC_PROTO (Int GetArgSizeFromChar, (Term *));
STATIC_PROTO (void CloseStream, (int));

static int
yap_fflush(int sno)
{
#if HAVE_LIBREADLINE
  if (Stream[sno].status & Tty_Stream_f &&
      Stream[sno].status & Output_Stream_f) {
    if (ReadlinePos != ReadlineBuf) {
      ReadlinePos[0] = '\0';
      fputs( ReadlineBuf, Stream[sno].u.file.file);
    }
    ReadlinePos = ReadlineBuf;
  }
#endif
  if ( (Stream[sno].status & Output_Stream_f) &&
       ! (Stream[sno].status & 
         (Null_Stream_f|
	  InMemory_Stream_f|
	  Socket_Stream_f|
	  Pipe_Stream_f|
	  Free_Stream_f)) ) {
    return(fflush(Stream[sno].u.file.file));
  } else
    return(0);
}

static void
unix_upd_stream_info (StreamDesc * s)
{
  if (s->status & InMemory_Stream_f) {
    s->status |= Seekable_Stream_f;
    return;
  }
#if USE_SOCKET
  if (Yap_sockets_io &&
      s->u.file.file == NULL)
    {
      s->status |= Socket_Stream_f;
      s->u.socket.domain = af_inet;
      s->u.socket.flags = client_socket;
      s->u.socket.fd = 0;
      return;
    }
#endif /* USE_SOCKET */
#if _MSC_VER || defined(__MINGW32__)
  {
    if (_isatty(_fileno(s->u.file.file))) {
      s->status |= Tty_Stream_f|Reset_Eof_Stream_f|Promptable_Stream_f;
      /* make all console descriptors unbuffered */
      setvbuf(s->u.file.file, NULL, _IONBF, 0);
    }
    /* standard error stream should never be buffered */
    if (StdErrStream == s-Stream) {
      setvbuf(s->u.file.file, NULL, _IONBF, 0);      
    }
    return;
  }
#else
#if HAVE_ISATTY
#if __simplescalar__
  /* isatty does not seem to work with simplescar. I'll assume the first
     three streams will probably be ttys (pipes are not thatg different) */
  if (s-Stream < 3) {
    s->u.file.name = Yap_LookupAtom("tty");
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
	  s->u.file.name = Yap_LookupAtom("tty");
	else
	  s->u.file.name = Yap_LookupAtom(ttys);
#else
	s->u.file.name = Yap_LookupAtom("tty");
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
#if USE_SOCKET
  if (s->status & Socket_Stream_f) {
    s->stream_getc = ConsoleSocketGetc;
  } else
#endif
#if HAVE_LIBREADLINE
     if (s->status & Tty_Stream_f) {
       s->stream_getc = ReadlineGetc;
       if (Stream[0].status & Tty_Stream_f &&
	   s->u.file.name == Stream[0].u.file.name)
	 s->stream_putc = ReadlinePutc;
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

static void
InitStdStream (int sno, SMALLUNSGN flags, YP_File file)
{
  StreamDesc *s = &Stream[sno];
  s->u.file.file = file;
  s->status = flags;
  s->linepos = 0;
  s->linecount = 1;
  s->charcount = 0;
  unix_upd_stream_info (s);
  /* Getting streams to prompt is a mess because we need for cooperation
     between readers and writers to the stream :-(
  */
#if USE_SOCKET
  if (s->status & Socket_Stream_f) {
    /* Console is a socket and socket will prompt */
    s->stream_putc = ConsoleSocketPutc;
    s->stream_getc = ConsoleSocketGetc;
  } else
#endif
  if (s->status & Pipe_Stream_f) {
    /* Console is a socket and socket will prompt */
    s->stream_putc = ConsolePipePutc;
    s->stream_getc = ConsolePipeGetc;
  } else if (s->status & InMemory_Stream_f) {
    s->stream_putc = MemPutc;
    s->stream_getc = MemGetc;    
  } else {
   /* check if our console is promptable: may be tty or pipe */
    if (s->status & (Promptable_Stream_f)) {
      /* the putc routine only has to check it is putting out a newline */
      s->stream_putc = ConsolePutc;
      /* if a tty have a special routine to call readline */
#if HAVE_LIBREADLINE
      if (s->status & Tty_Stream_f) {
	if (Stream[0].status & Tty_Stream_f &&
	    is_same_tty(s->u.file.file,Stream[0].u.file.file))
	  s->stream_putc = ReadlinePutc;
	s->stream_getc = ReadlineGetc;
      } else
#endif
	{
	  /* else just PlGet plus checking for prompt */
	  s->stream_getc = ConsoleGetc;
	}
    } else {
      /* we are reading from a file, no need to check for prompts */
      s->stream_putc = FilePutc;
      s->stream_getc = PlGetc;
    } 
    switch(sno) {
    case 0:
      s->u.file.name=Yap_LookupAtom("user_input");
      break;
    case 1:
      s->u.file.name=Yap_LookupAtom("user_output");
      break;
    default:
      s->u.file.name=Yap_LookupAtom("user_error");
      break;
    }
    s->u.file.user_name = MkAtomTerm (s->u.file.name);
  }
  if (CharConversionTable != NULL)
    s->stream_getc_for_read = ISOGetc;
  else
    s->stream_getc_for_read = s->stream_getc;
#if LIGHT
  s->status |= Tty_Stream_f|Promptable_Stream_f;
#endif
#if HAVE_SETBUF
  if (s->status & Tty_Stream_f)
    /* make sure input is unbuffered if it comes from stdin, this
       makes life simpler for interrupt handling */
    YP_setbuf (stdin, NULL); 
#endif /* HAVE_SETBUF */

}


static void
InitStdStreams (void)
{
  if (Yap_sockets_io) {
    InitStdStream (StdInStream, Input_Stream_f, NULL);
    InitStdStream (StdOutStream, Output_Stream_f, NULL);
    InitStdStream (StdErrStream, Output_Stream_f, NULL);
  } else {
    InitStdStream (StdInStream, Input_Stream_f, stdin);
    InitStdStream (StdOutStream, Output_Stream_f, stdout);
    InitStdStream (StdErrStream, Output_Stream_f, stderr);
  }
  Yap_c_input_stream = StdInStream;
  Yap_c_output_stream = StdOutStream;
  Yap_c_error_stream = StdErrStream;
  /* init standard aliases */
  FileAliases[0].name = AtomUsrIn;
  FileAliases[0].alias_stream = 0;
  FileAliases[1].name = AtomUsrOut;
  FileAliases[1].alias_stream = 1;
  FileAliases[2].name = AtomUsrErr;
  FileAliases[2].alias_stream = 2;
  NOfFileAliases = 3;
  SzOfFileAliases = ALIASES_BLOCK_SIZE;
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

  for (i = 0; i < MaxStreams; ++i)
    Stream[i].status = Free_Stream_f;
  /* alloca alias array */
  FileAliases = (AliasDesc)Yap_AllocCodeSpace(sizeof(struct AliasDescS)*ALIASES_BLOCK_SIZE);
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
  if (Yap_GetValue(Yap_LookupAtom("fileerrors")) == MkIntTerm(1)) {
    Yap_Error(type, culprit, who);
    /* and fail */
    return(FALSE);
  } else {
    return(FALSE);
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
		       StdErrStream) &&
	  !(s->status & Null_Stream_f))
	{
	  putc (MPWSEP, s->u.file.file);
	  if (!(Stream[Yap_c_output_stream].status & Null_Stream_f))
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
		       StdErrStream) &&
	  !(s->status & Null_Stream_f))
	{
	  putc (MPWSEP, s->u.file.file);
	  if (!(Stream[Yap_c_output_stream].status & Null_Stream_f))
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
    fprintf(stderr,"[ Warning: can not open %s for input]\n", fname);
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
      YP_fgets(my_line, 200, stdin);
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
Yap_DebugPutc(int sno, int ch)
{
  if (Yap_Option['l' - 96])
    (void) putc(ch, Yap_logfile);
  return (putc(ch, Yap_stderr));
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
MemPutc(int sno, int ch)
{
  StreamDesc *s = &Stream[sno];
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
  s->u.mem_string.buf[s->u.mem_string.pos++] = ch;
  if (s->u.mem_string.pos == s->u.mem_string.max_size) {
    extern int Yap_page_size;
    /* oops, we have reached an overflow */
    Int new_max_size = s->u.mem_string.max_size + Yap_page_size;
    char *newbuf;

    if ((newbuf = Yap_AllocAtomSpace(new_max_size*sizeof(char))) == NULL) {
      Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow heap for writing to string");
    }
#if HAVE_MEMMOVE
    memmove((void *)newbuf, (void *)s->u.mem_string.buf, (size_t)((s->u.mem_string.pos)*sizeof(char)));
#else
    {
      Int n = s->u.mem_string.pos;
      char *to = newbuf;
      char *from = s->u.mem_string.buf;
      while (n-- >= 0) {
	*to++ = *from++;
      }
    }
#endif
    Yap_FreeAtomSpace(s->u.mem_string.buf);
    s->u.mem_string.buf = newbuf;
    s->u.mem_string.max_size = new_max_size;
  }
  count_output_char(ch,s);
  return ((int) ch);
}

#if USE_SOCKET
/* static */
static int
ConsoleSocketPutc (int sno, int ch)
{
  StreamDesc *s = &Stream[sno];
  char c = ch;
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
#if _MSC_VER || defined(__MINGW32__)
  send(s->u.socket.fd,  &c, sizeof(c), 0);
#else
  if (write(s->u.socket.fd,  &c, sizeof(c)) < 0) {
#if HAVE_STRERROR
    Yap_Error(FATAL_ERROR, TermNil, "no access to console: %s", strerror(errno));
#else
    Yap_Error(FATAL_ERROR, TermNil, "no access to console");
#endif
  }
#endif
  count_output_char(ch,s);
  return ((int) ch);
}

static int
SocketPutc (int sno, int ch)
{
  StreamDesc *s = &Stream[sno];
  char c = ch;
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
#if _MSC_VER || defined(__MINGW32__)
  send(s->u.socket.fd,  &c, sizeof(c), 0);
#else
  write(s->u.socket.fd,  &c, sizeof(c));
#endif
  console_count_output_char(ch,s);
  return ((int) ch);
}

#endif

/* static */
static int
ConsolePipePutc (int sno, int ch)
{
  StreamDesc *s = &Stream[sno];
  char c = ch;
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
#if _MSC_VER || defined(__MINGW32__) 
  {
    DWORD written;
    if (WriteFile(s->u.pipe.hdl, &c, sizeof(c), &written, NULL) == FALSE) {
      PlIOError (SYSTEM_ERROR,TermNil, "write to pipe returned error");
      return(EOF);
    }
  }
#else
  write(s->u.pipe.fd,  &c, sizeof(c));
#endif
  count_output_char(ch,s);
  return ((int) ch);
}

static int
PipePutc (int sno, int ch)
{
  StreamDesc *s = &Stream[sno];
  char c = ch;
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
#if _MSC_VER || defined(__MINGW32__) 
  {
    DWORD written;
    if (WriteFile(s->u.pipe.hdl, &c, sizeof(c), &written, NULL) == FALSE) {
      PlIOError (SYSTEM_ERROR,TermNil, "write to pipe returned error");
      return(EOF);
    }
  }
#else
  write(s->u.pipe.fd,  &c, sizeof(c));
#endif
  console_count_output_char(ch,s);
  return ((int) ch);
}

static int
NullPutc (int sno, int ch)
{
  StreamDesc *s = &Stream[sno];
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
  count_output_char(ch,s);
  return ((int) ch);
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
p_setprompt (void)
{				/* 'prompt(Atom)                 */
  Term t = Deref(ARG1);
  if (IsVarTerm (t) || !IsAtomTerm (t))
    return (FALSE);
  *AtPrompt = AtomOfTerm (t);
  return (TRUE);
}

static Int
p_is_same_tty (void)
{				/* 'prompt(Atom)                 */
  int sni = CheckStream (ARG1, Input_Stream_f, "put/2");
  int sno = CheckStream (ARG2, Output_Stream_f, "put/2");
  return (
	  (Stream[sni].status & Tty_Stream_f) &&
	  (Stream[sno].status & Tty_Stream_f) &&
	  is_same_tty(Stream[sno].u.file.file,Stream[sni].u.file.file)
	  );
}

static Int
p_prompt (void)
{				/* prompt(Old,New)       */
  Term t = Deref (ARG2);
  Atom a;
  if (!Yap_unify_constant (ARG1, MkAtomTerm (*AtPrompt)))
    return (FALSE);
  if (IsVarTerm (t) || !IsAtomTerm (t))
    return (FALSE);
  a = AtomOfTerm (t);
  if (strlen (RepAtom (a)->StrOfAE) > MAX_PROMPT) {
    Yap_Error(SYSTEM_ERROR,t,"prompt %s is too long", RepAtom (a)->StrOfAE);
    return(FALSE);
  }
  strncpy(Prompt, RepAtom (a)->StrOfAE, MAX_PROMPT);
  *AtPrompt = a;
  return (TRUE);
}

#if HAVE_LIBREADLINE

#if HAVE_READLINE_READLINE_H
#include <readline/readline.h>
#endif

extern void add_history (const char *);

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
  register int ch;

  while (ttyptr == NULL) {
    /* Only sends a newline if we are at the start of a line */
    if (myrl_line != NULL && myrl_line != (char *) EOF)
      free (myrl_line);
    rl_instream = Stream[sno].u.file.file;
    rl_outstream = Stream[cur_out_sno].u.file.file;
    /* window of vulnerability opened */
    if (newline) {
      char *cptr = Prompt, ch;

      if ((Stream[FileAliases[2].alias_stream].status & Tty_Stream_f) &&
	  Stream[FileAliases[0].alias_stream].u.file.name == Stream[sno].u.file.name) {
	/* don't just output the prompt */
	while ((ch = *cptr++) != '\0') {
	  console_count_output_char(ch,Stream+StdErrStream);
	}
	Yap_PrologMode |= ConsoleGetcMode;
	myrl_line = readline (Prompt);
      } else {
	Yap_PrologMode |= ConsoleGetcMode;
	myrl_line = readline (NULL);
      }
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
	return(console_post_process_read_char(EOF, s));
      }
      continue;
    } else {
      Yap_PrologMode &= ~ConsoleGetcMode;
    }
    newline=FALSE;
    strncpy (Prompt, RepAtom (*AtPrompt)->StrOfAE, MAX_PROMPT);
    /* window of vulnerability closed */
    if (myrl_line == NULL || myrl_line == (char *) EOF)
      return(console_post_process_read_char(EOF, s));
    if (myrl_line[0] != '\0' && myrl_line[1] != '\0')
      add_history (myrl_line);
    ttyptr = myrl_line;
  }
  if (*ttyptr == '\0') {
    ttyptr = NIL;
    ch = '\n';
  } else {
    ch = *ttyptr++;
  }
  return(console_post_process_read_char(ch, s));
}

#endif /* HAVE_LIBREADLINE */


int
Yap_GetCharForSIGINT(void)
{
  int ch;
#if  HAVE_LIBREADLINE
  if ((Yap_PrologMode & ConsoleGetcMode) && myrl_line != (char *) NULL) {
    ch = myrl_line[0];
    free(myrl_line);
    myrl_line = NULL;
  } else {
    myrl_line = readline ("Action (h for help): ");
    if (myrl_line == (char *)NULL || myrl_line == (char *)EOF) {
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
    return(EOF);
  }	  
  if (s->status & Eof_Error_Stream_f) {
    Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,MkAtomTerm(s->u.file.name),
	  "GetC");
  } else if (s->status & Reset_Eof_Stream_f) {
    /* reset the eof indicator on file */
    if (YP_feof (s->u.file.file))
      YP_clearerr (s->u.file.file);
    /* reset our function for reading input */
#if USE_SOCKET
    if (s->status & Socket_Stream_f) {
      if (s->status & Promptable_Stream_f)
	s->stream_putc = ConsoleSocketPutc;
      else 
	s->stream_putc = SocketPutc;
    } else 
#endif
    if (s->status & Pipe_Stream_f) {
      if (s->status & Promptable_Stream_f)
	s->stream_putc = ConsolePipePutc;
      else 
	s->stream_putc = PipePutc;
    } else if (s->status & InMemory_Stream_f) {
      s->stream_getc = MemGetc;
      s->stream_putc = MemPutc;
    } else if (s->status & Promptable_Stream_f) {
      s->stream_putc = ConsolePutc;
#if HAVE_LIBREADLINE
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
    }
    if (CharConversionTable != NULL)
      s->stream_getc_for_read = ISOGetc;
    else
      s->stream_getc_for_read = s->stream_getc;
    if (CharConversionTable != NULL)
      s->stream_getc = ISOGetc;
    /* next, reset our own error indicator */
    s->status &= ~Eof_Stream_f;
    /* try reading again */
    return(s->stream_getc(sno));
  } else {
    s->status |= Past_Eof_Stream_f;
  }
  return (EOF);
}

/* check if we read a newline or an EOF */
static int
post_process_read_char(int ch, StreamDesc *s)
{
  if (ch == '\n') {
    ++s->linecount;
    ++s->charcount;
    s->linepos = 0;
    /* don't convert if the stream is binary */
    if (!(s->status & Binary_Stream_f))
      ch = 10;
  } else if (ch == EOF) {
    s->status |= Eof_Stream_f;
    s->stream_getc = EOFGetc;
    if (CharConversionTable != NULL)
      s->stream_getc_for_read = ISOGetc;
    else
      s->stream_getc_for_read = s->stream_getc;
    return (EOFCHAR);
  } else {
    ++s->charcount;
    ++s->linepos;
  }
  return(ch);
}

/* check if we read a newline or an EOF */
static int
console_post_process_read_char(int ch, StreamDesc *s)
{
  if (ch == '\n') {
    ++s->linecount;
    ++s->charcount;
    s->linepos = 0;
    newline = TRUE;
  } else if (ch == EOF) {
    s->status |= Eof_Stream_f;
    s->stream_getc = EOFGetc;
    if (CharConversionTable != NULL)
      s->stream_getc_for_read = ISOGetc;
    else
      s->stream_getc_for_read = s->stream_getc;
    newline = FALSE;
    return (EOFCHAR);
  } else {
    ++s->charcount;
    ++s->linepos;
    newline = FALSE;
  }
  return(ch);
}

#if USE_SOCKET
/* 
   sockets cannot use standard FILE *, we have to go through fds, and in the
   case of VC++, we have to use the receive routines...
*/
static int
SocketGetc(int sno)
{
  register StreamDesc *s = &Stream[sno];
  register int ch;
  char c;
  int count;
  /* should be able to use a buffer */
#if _MSC_VER || defined(__MINGW32__)
  count = recv(s->u.socket.fd, &c, sizeof(char), 0);
#else
  count = read(s->u.socket.fd, &c, sizeof(char));
#endif
  if (count == 0) {
    s->u.socket.flags = closed_socket;
    ch = EOF;
  } else if (count > 0) {
    ch = c;
  } else {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR, TermNil, 
	    "( socket_getc: %s)", strerror(errno));
#else
      Yap_Error(SYSTEM_ERROR, TermNil,
	    "(socket_getc)");
#endif
    return(EOF);
  }
  return(post_process_read_char(ch, s));
}

/*
  Basically, the same as console but also sends a prompt and takes care of
  finding out whether we are at the start of a newline.
*/
static int
ConsoleSocketGetc(int sno)
{
  register StreamDesc *s = &Stream[sno];
  register int ch;
  char c;
  int count;

  /* send the prompt away */
  if (newline) {
    char *cptr = Prompt, ch;
    /* use the default routine */
    while ((ch = *cptr++) != '\0') {
      Stream[StdErrStream].stream_putc(StdErrStream, ch);
    }
    strncpy(Prompt, RepAtom (*AtPrompt)->StrOfAE, MAX_PROMPT);
    newline = FALSE;
  }
  /* should be able to use a buffer */
  Yap_PrologMode |= ConsoleGetcMode;
#if _MSC_VER || defined(__MINGW32__)
  count = recv(s->u.socket.fd, &c, sizeof(char), 0);
#else
  count = read(s->u.socket.fd, &c, sizeof(char));
#endif
  Yap_PrologMode &= ~ConsoleGetcMode;
  if (count == 0) {
    ch = EOF;
  } else if (count > 0) {
    ch = c;
  } else {
    Yap_Error(SYSTEM_ERROR, TermNil, "read");
    return(EOF);
  }
  return(console_post_process_read_char(ch, s));
}
#endif

static int
PipeGetc(int sno)
{
  register StreamDesc *s = &Stream[sno];
  register int ch;
  char c;
	/* should be able to use a buffer */
#if _MSC_VER || defined(__MINGW32__) 
  DWORD count;
  if (WriteFile(s->u.pipe.hdl, &c, sizeof(c), &count, NULL) == FALSE) {
    PlIOError (SYSTEM_ERROR,TermNil, "read from pipe returned error");
    return(EOF);
  }
#else
  int count;
  count = read(s->u.pipe.fd, &c, sizeof(char));
#endif
  if (count == 0) {
    ch = EOF;
  } else if (count > 0) {
    ch = c;
  } else {
    Yap_Error(SYSTEM_ERROR, TermNil, "read");
    return(EOF);
  }
  return(post_process_read_char(ch, s));
}

/*
  Basically, the same as console but also sends a prompt and takes care of
  finding out whether we are at the start of a newline.
*/
static int
ConsolePipeGetc(int sno)
{
  register StreamDesc *s = &Stream[sno];
  register int ch;
  char c;
#if _MSC_VER || defined(__MINGW32__) 
  DWORD count;
#else
  int count;
#endif

  /* send the prompt away */
  if (newline) {
    char *cptr = Prompt, ch;
    /* use the default routine */
    while ((ch = *cptr++) != '\0') {
      Stream[StdErrStream].stream_putc(StdErrStream, ch);
    }
    strncpy(Prompt, RepAtom (*AtPrompt)->StrOfAE, MAX_PROMPT);
    newline = FALSE;
  }
#if _MSC_VER || defined(__MINGW32__) 
  if (WriteFile(s->u.pipe.hdl, &c, sizeof(c), &count, NULL) == FALSE) {
    Yap_PrologMode |= ConsoleGetcMode;
    PlIOError (SYSTEM_ERROR,TermNil, "read from pipe returned error");
    Yap_PrologMode &= ~ConsoleGetcMode;
    return(EOF);
  }
#else
  /* should be able to use a buffer */
  Yap_PrologMode |= ConsoleGetcMode;
  count = read(s->u.pipe.fd, &c, sizeof(char));
  Yap_PrologMode &= ~ConsoleGetcMode;
#endif
  if (count == 0) {
    ch = EOF;
  } else if (count > 0) {
    ch = c;
  } else {
    Yap_Error(SYSTEM_ERROR, TermNil, "read");
    return(EOF);
  }
  return(console_post_process_read_char(ch, s));
}

/* standard routine, it should read from anything pointed by a FILE *.
   It could be made more efficient by doing our own buffering and avoiding
   post_process_read_char, something to think about */
static int
PlGetc (int sno)
{
  register StreamDesc *s = &Stream[sno];
  register int ch;

  ch = YP_getc (s->u.file.file);
  return(post_process_read_char(ch, s));
}

/* read from memory */
static int
MemGetc (int sno)
{
  register StreamDesc *s = &Stream[sno];
  Int ch, spos;

  spos = s->u.mem_string.pos;
  if (spos == s->u.mem_string.max_size)
    ch = -1;
  else {
    ch = s->u.mem_string.buf[spos];
    s->u.mem_string.pos = ++spos;
  }
  return(post_process_read_char(ch, s));
}

/* I dispise this code!!!!! */
static int
ISOGetc (int sno)
{
  int ch = Stream[sno].stream_getc(sno);
  if (ch != EOF && CharConversionTable != NULL) {
    int nch;

    nch = CharConversionTable[ch];
    if (nch != '\0') {
      ch = nch;
    }
  }
  return(ch); 
}

/* send a prompt, and use the system for internal buffering. Speed is
   not of the essence here !!! */
static int
ConsoleGetc(int sno)
{
  register StreamDesc *s = &Stream[sno];
  char ch;

 restart:
  if (newline) {
    char *cptr = Prompt, ch;

    /* use the default routine */
    while ((ch = *cptr++) != '\0') {
      Stream[StdErrStream].stream_putc(StdErrStream, ch);
    }
    strncpy (Prompt, RepAtom (*AtPrompt)->StrOfAE, MAX_PROMPT);
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
      return(console_post_process_read_char(EOF, s));
    }
    goto restart;
  } else {
    Yap_PrologMode &= ~ConsoleGetcMode;
  }
  return(console_post_process_read_char(ch, s));
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
  if (s->status & InMemory_Stream_f) {
    s->stream_getc = MemGetc;
    s->stream_putc = MemPutc;
  } else if (s->status & Promptable_Stream_f) {
    s->stream_putc = ConsolePutc;
#if HAVE_LIBREADLINE
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
  }
  return(post_process_read_char(ch, s));
}


/* used by user-code to read characters from the current input stream */
int
Yap_PlGetchar (void)
{
  return(Stream[Yap_c_input_stream].stream_getc(Yap_c_input_stream));
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

static Int
p_stream_flags (void)
{				/* '$stream_flags'(+N,-Flags)            */
  Term trm;
  trm = Deref (ARG1);
  if (IsVarTerm (trm) || !IsIntTerm (trm))
    return (FALSE);
  return (Yap_unify_constant (ARG2, MkIntTerm (Stream[IntOfTerm (trm)].status)));
}

static int
find_csult_file (char *source, char *buf, StreamDesc * st, char *io_mode)
{

  char *cp = source, ch;
  while (*cp++);
  while ((ch = *--cp) != '.' && !Yap_dir_separator((int)ch) && cp != source);
  if (ch == '.')
    return (FALSE);
  strncpy (buf, source, YAP_FILENAME_MAX);
  strncat (buf, ".yap", YAP_FILENAME_MAX);
  if ((st->u.file.file = YP_fopen (buf, io_mode)) != YAP_ERROR)
    return (TRUE);
  strncpy (buf, source, YAP_FILENAME_MAX);
  strncat (buf, ".pl", YAP_FILENAME_MAX);
  if ((st->u.file.file = YP_fopen (buf, io_mode)) != YAP_ERROR)
    return (TRUE);
  return (FALSE);
}

/* given a stream index, get the corresponding fd */
static int
GetStreamFd(int sno)
{
#if USE_SOCKET
  if (Stream[sno].status & Socket_Stream_f) {
    return(Stream[sno].u.socket.fd);
  } else
#endif
  if (Stream[sno].status & Pipe_Stream_f) {
#if _MSC_VER || defined(__MINGW32__) 
    return((int)(Stream[sno].u.pipe.hdl));
#else
    return(Stream[sno].u.pipe.fd);
#endif
  } else if (Stream[sno].status & InMemory_Stream_f) {
    return(-1);
  }
  return(YP_fileno(Stream[sno].u.file.file));
}

int
Yap_GetStreamFd(int sno)
{
  return GetStreamFd(sno);
}

/* given a socket file descriptor, get the corresponding stream descripor */
int
Yap_CheckIOStream(Term stream, char * error)
{
  return(CheckStream(stream, Input_Stream_f|Output_Stream_f|Socket_Stream_f, error));
}

#if USE_SOCKET

Term
Yap_InitSocketStream(int fd, socket_info flags, socket_domain domain) {
  StreamDesc *st;
  int sno;

  for (sno = 0; sno < MaxStreams; ++sno)
    if (Stream[sno].status & Free_Stream_f)
      break;
  if (sno == MaxStreams) {
    PlIOError (SYSTEM_ERROR,TermNil, "new stream not available for socket/4");
    return(TermNil);
  }
  st = &Stream[sno];
  st->u.socket.domain = domain;
  st->u.socket.flags = flags;
  if (flags & (client_socket|server_session_socket)) {
    /* I can read and write from these sockets */
    st->status = (Socket_Stream_f|Input_Stream_f|Output_Stream_f);
  } else {
    /* oops, I cannot */
    st->status = Socket_Stream_f;
  }
  st->u.socket.fd = fd;
  st->charcount = 0;
  st->linecount = 1;
  st->linepos = 0;
  st->stream_putc = SocketPutc;
  st->stream_getc = SocketGetc;
  if (CharConversionTable != NULL)
    st->stream_getc_for_read = ISOGetc;
  else
    st->stream_getc_for_read = st->stream_getc;
  return(MkStream(sno));
}

/* given a socket file descriptor, get the corresponding stream descripor */
int
Yap_CheckSocketStream(Term stream, char * error)
{
  return(CheckStream(stream, Socket_Stream_f, error));
}

/* given a stream index, get the corresponding domain */
socket_domain
Yap_GetSocketDomain(int sno)
{
  return(Stream[sno].u.socket.domain);
}

/* given a stream index, get the corresponding status */
socket_info
Yap_GetSocketStatus(int sno)
{
  return(Stream[sno].u.socket.flags);
}

/* update info on a socket, eg, new->server or new->client */
void
Yap_UpdateSocketStream(int sno, socket_info flags, socket_domain domain) {
  StreamDesc *st;

  st = &Stream[sno];
  st->u.socket.domain = domain;
  st->u.socket.flags = flags;
  if (flags & (client_socket|server_session_socket)) {
    /* I can read and write from these sockets */
    st->status = (Socket_Stream_f|Input_Stream_f|Output_Stream_f);
  } else {
    /* oops, I cannot */
    st->status = Socket_Stream_f;
  }
}

#endif /* USE_SOCKET */

static int
binary_file(char *file_name)
{
#if HAVE_STAT
#if _MSC_VER || defined(__MINGW32__) 
  struct _stat ss;
  if (_stat(file_name, &ss) != 0) {
#else
  struct stat ss;
  if (stat(file_name, &ss) != 0) {
#endif
    /* ignore errors while checking a file */
    return(FALSE);
  }
  return (S_ISDIR(ss.st_mode));
#else
  return(FALSE);
#endif
}

static Int
p_open (void)
{				/* '$open'(+File,+Mode,?Stream,-ReturnCode)      */
  Term file_name, t, t2, topts;
  Atom open_mode;
  int sno;
  SMALLUNSGN s;
  char io_mode[8];
  StreamDesc *st;
  Int opts;

  file_name = Deref(ARG1);
  /* we know file_name is bound */
  if (!IsAtomTerm (file_name)) {
    Yap_Error(DOMAIN_ERROR_SOURCE_SINK,file_name, "open/3");
    return(FALSE);
  }
  t2 = Deref (ARG2);
  if (!IsAtomTerm (t2)) {
    Yap_Error(TYPE_ERROR_ATOM,t2, "open/3");
    return(FALSE);
  }
  open_mode = AtomOfTerm (t2);
  if (open_mode == AtomRead || open_mode == AtomCsult) {
    if (open_mode == AtomCsult && AtomOfTerm(file_name) == AtomUsrIn) {
      return(Yap_unify(MkStream(FileAliases[0].alias_stream), ARG3));
    }
    strncpy(io_mode,"rb", 8);
    s = Input_Stream_f;
  } else if (open_mode == AtomWrite) {
    strncpy(io_mode,"w",8);
	s = Output_Stream_f;
  } else if (open_mode == AtomAppend) {
    strncpy(io_mode,"a",8);
	s = Append_Stream_f | Output_Stream_f;
  } else {
    Yap_Error(DOMAIN_ERROR_IO_MODE, t2, "open/3");
    return(FALSE);   
  }
  if (!Yap_TrueFileName (RepAtom (AtomOfTerm (file_name))->StrOfAE, Yap_FileNameBuf, FALSE))
    return (PlIOError (EXISTENCE_ERROR_SOURCE_SINK,file_name,"open/3"));
  for (sno = 0; sno < MaxStreams; ++sno)
    if (Stream[sno].status & Free_Stream_f)
      break;
  if (sno == MaxStreams)
    return (PlIOError (SYSTEM_ERROR,TermNil, "new stream not available for open/3"));
  st = &Stream[sno];
  /* can never happen */
  topts = Deref(ARG4);
  if (IsVarTerm(topts) || !IsIntTerm(topts))
    return(FALSE);
  opts = IntOfTerm(topts);
#ifdef _WIN32
  if (st->status & Binary_Stream_f) {
    strncat(io_mode, "b", 8);
  } else {
    strncat(io_mode, "t", 8);
  }
#endif
  if ((st->u.file.file = YP_fopen (Yap_FileNameBuf, io_mode)) == YAP_ERROR ||
      (!(opts & 2 /* binary */) && binary_file(Yap_FileNameBuf)))
    {
      if (open_mode == AtomCsult)
	{
	  if (!find_csult_file (Yap_FileNameBuf, Yap_FileNameBuf2, st, io_mode))
	    return (PlIOError (EXISTENCE_ERROR_SOURCE_SINK, file_name, "open/3"));
	  strncpy (Yap_FileNameBuf, Yap_FileNameBuf2, YAP_FILENAME_MAX);
	}
      else {
	if (errno == ENOENT)
	  return (PlIOError(EXISTENCE_ERROR_SOURCE_SINK,file_name,"open/3"));
	else
	  return (PlIOError(PERMISSION_ERROR_OPEN_SOURCE_SINK,file_name,"open/3"));
      }
    }
#if MAC
  if (open_mode == AtomWrite)
    {
      Yap_SetTextFile (RepAtom (AtomOfTerm (file_name))->StrOfAE);
    }
#endif
  st->status = s;
  st->charcount = 0;
  st->linecount = 1;
  st->u.file.name = Yap_LookupAtom (Yap_FileNameBuf);
  st->u.file.user_name = file_name;
  st->linepos = 0;
  st->stream_putc = FilePutc;
  st->stream_getc = PlGetc;
  unix_upd_stream_info (st);
  if (opts != 0) {
    if (opts & 2)
      st->status |= Binary_Stream_f;
    if (opts & 4) {
      if (st->status & (Tty_Stream_f|Socket_Stream_f|InMemory_Stream_f)) {
	Term ta[1], t;
	
#if USE_SOCKET
	if (st->status & Socket_Stream_f) {
	  st->stream_putc = SocketPutc;
	  st->stream_getc = SocketGetc;
	} else
#endif
	if (st->status & Pipe_Stream_f) {
	  st->stream_putc = PipePutc;
	  st->stream_getc = PipeGetc;
	} else if (st->status & InMemory_Stream_f) {
	  st->stream_putc = MemPutc;
	  st->stream_getc = MemGetc;	  
	} else {
	  st->stream_putc = ConsolePutc;
	  st->stream_getc = PlGetc;
	}
	ta[1] = MkAtomTerm(AtomTrue);
	t = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("reposition"),1),1,ta);
	Yap_Error(PERMISSION_ERROR_OPEN_SOURCE_SINK,t,"open/4");
	return(FALSE);
      }
      /* useless crap */
      st->status |= Seekable_Stream_f;
    }
    if (opts & 8) {
      /* There may be one reason why one wouldn't want to seek in a
	 file, maybe .... */
      st->status &= ~Seekable_Stream_f;
    }
    if (opts & 16) {
      st->status &= ~(Reset_Eof_Stream_f);
      st->status |= Eof_Error_Stream_f;
    }
    if (opts & 32) {
      st->status &= ~(Eof_Error_Stream_f|Reset_Eof_Stream_f);
    }
    if (opts & 64) {
      st->status &= ~(Eof_Error_Stream_f);
      st->status |= Reset_Eof_Stream_f;
    }
  }
  if (CharConversionTable != NULL)
    st->stream_getc_for_read = ISOGetc;
  else
    st->stream_getc_for_read = st->stream_getc;
  t = MkStream (sno);
  return (Yap_unify (ARG3, t));
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


static Int p_add_alias_to_stream (void)
{
  Term tname = Deref(ARG1);
  Term tstream = Deref(ARG2);
  Atom at;
  Int sno;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "$add_alias_to_stream");
    return (FALSE);
  } else if (!IsAtomTerm (tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "$add_alias_to_stream");
    return (FALSE);
  }
  if (IsVarTerm(tstream)) {
    Yap_Error(INSTANTIATION_ERROR, tstream, "$add_alias_to_stream");
    return (FALSE);
  } else if (!IsApplTerm (tstream) || FunctorOfTerm (tstream) != FunctorStream ||
	     !IsIntTerm(ArgOfTerm(1,tstream))) {
    Yap_Error(DOMAIN_ERROR_STREAM_OR_ALIAS, tstream, "$add_alias_to_stream");
    return (FALSE);
  }
  at = AtomOfTerm(tname);
  sno = (int)IntOfTerm(ArgOfTerm(1,tstream));
  if (AddAlias(at, sno))
    return(TRUE);
  /* we could not create the alias, time to close the stream */
  CloseStream(sno);
  Yap_Error(PERMISSION_ERROR_NEW_ALIAS_FOR_STREAM, tname, "open/3");
  return (FALSE);
}

static Int p_change_alias_to_stream (void)
{
  Term tname = Deref(ARG1);
  Term tstream = Deref(ARG2);
  Atom at;
  Int sno;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "$change_alias_to_stream/2");
    return (FALSE);
  } else if (!IsAtomTerm (tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "$change_alias_to_stream/2");
    return (FALSE);
  }
  at = AtomOfTerm(tname);
  if ((sno = CheckStream (tstream, Input_Stream_f | Output_Stream_f | Append_Stream_f | Socket_Stream_f,  "change_stream_alias/2"))
	  == -1)
    return(FALSE);
  SetAlias(at, sno);
  return(TRUE);
}

static Int p_check_if_valid_new_alias (void)
{
  Term tname = Deref(ARG1);
  Atom at;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "$add_alias_to_stream");
    return (FALSE);
  } else if (!IsAtomTerm (tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "$add_alias_to_stream");
    return (FALSE);
  }
  at = AtomOfTerm(tname);
  return(CheckAlias(at) == -1);
}


static Int
p_fetch_stream_alias (void)
{				/* '$fetch_stream_alias'(Stream)                  */
  int sno;
  Term t2 = Deref(ARG2);

  if ((sno = CheckStream (ARG1, Input_Stream_f | Output_Stream_f,
			  "fetch_stream_alias/2"))  == -1)
    return(FALSE);
  if (IsVarTerm(t2)) {
    Atom at = FetchAlias(sno);
    if (at == AtomFoundVar)
      return(FALSE);
    else 
      return(Yap_unify_constant(t2, MkAtomTerm(at)));
  } else if (IsAtomTerm(t2)) {
    Atom at = AtomOfTerm(t2);
    return((Int)FindAliasForStream(sno,at));
  } else {
    Yap_Error(TYPE_ERROR_ATOM, t2, "fetch_stream_alias/2");
    return(FALSE);
  }
}

static Int
p_open_null_stream (void)
{
  Term t;
  StreamDesc *st;
  int sno;
  for (sno = 0; sno < MaxStreams; ++sno)
    if (Stream[sno].status & Free_Stream_f)
      break;
  if (sno == MaxStreams)
    return (PlIOError (SYSTEM_ERROR,TermNil, "new stream not available for open_null_stream/1"));
  st = &Stream[sno];
  st->status = Append_Stream_f | Output_Stream_f | Null_Stream_f;
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->stream_putc = NullPutc;
  st->stream_getc = PlGetc;
  st->stream_getc_for_read = PlGetc;
  st->u.file.user_name = MkAtomTerm (st->u.file.name = Yap_LookupAtom ("/dev/null"));
  t = MkStream (sno);
  return (Yap_unify (ARG1, t));
}

Term
Yap_OpenStream(FILE *fd, char *name, Term file_name, int flags)
{
  Term t;
  StreamDesc *st;
  int sno;

  for (sno = 0; sno < MaxStreams; ++sno)
    if (Stream[sno].status & Free_Stream_f)
      break;
  if (sno == MaxStreams)
    return (PlIOError (SYSTEM_ERROR,TermNil, "new stream not available for open_null_stream/1"));
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
  if (flags & YAP_PIPE_STREAM) {
    st->stream_putc = PipePutc;
    st->stream_getc = PipeGetc;
  } else if (flags & YAP_TTY_STREAM) {
    st->stream_putc = ConsolePutc;
    st->stream_getc = ConsoleGetc;
  } else {
    st->stream_putc = FilePutc;
    st->stream_getc = PlGetc;
    unix_upd_stream_info (st);
  }
  if (CharConversionTable != NULL)
    st->stream_getc_for_read = ISOGetc;
  else
    st->stream_getc_for_read = st->stream_getc; 
  t = MkStream (sno);
  return (t);
}

static Int
p_open_pipe_stream (void)
{
  Term t1, t2;
  StreamDesc *st;
  int sno;
#if  _MSC_VER || defined(__MINGW32__) 
  HANDLE ReadPipe, WritePipe;
  SECURITY_ATTRIBUTES satt;

  satt.nLength = sizeof(satt);
  satt.lpSecurityDescriptor = NULL;
  satt.bInheritHandle = TRUE;
  if (!CreatePipe(&ReadPipe, &WritePipe, &satt, 0))
    {
      return (PlIOError (SYSTEM_ERROR,TermNil, "open_pipe_stream/2 could not create pipe"));
    }
#else
  int filedes[2];

  if (pipe(filedes) != 0)
    {
      return (PlIOError (SYSTEM_ERROR,TermNil, "open_pipe_stream/2 could not create pipe"));
    }
#endif
  for (sno = 0; sno < MaxStreams; ++sno)
    if (Stream[sno].status & Free_Stream_f)
      break;
  if (sno == MaxStreams)
    return (PlIOError (SYSTEM_ERROR,TermNil, "new stream not available for open_pipe_stream/2"));
  st = &Stream[sno];
  st->status = Input_Stream_f | Pipe_Stream_f;
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->stream_putc = PipePutc;
  st->stream_getc = PipeGetc;
  if (CharConversionTable != NULL)
    st->stream_getc_for_read = ISOGetc;
  else
    st->stream_getc_for_read = PipeGetc;
#if  _MSC_VER || defined(__MINGW32__) 
  st->u.pipe.hdl = ReadPipe;
#else
  st->u.pipe.fd = filedes[0];
#endif
  t1 = MkStream (sno);
  for (; sno < MaxStreams; ++sno)
    if (Stream[sno].status & Free_Stream_f)
      break;
  if (sno == MaxStreams)
    return (PlIOError (SYSTEM_ERROR,TermNil, "new stream not available for open_pipe_stream/2"));
  st = &Stream[sno];
  st->status = Output_Stream_f | Pipe_Stream_f;
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->stream_putc = PipePutc;
  st->stream_getc = PipeGetc;
  if (CharConversionTable != NULL)
    st->stream_getc_for_read = ISOGetc;
  else
    st->stream_getc_for_read = st->stream_getc; 
#if  _MSC_VER || defined(__MINGW32__) 
  st->u.pipe.hdl = WritePipe;
#else
  st->u.pipe.fd = filedes[1];
#endif
  t2 = MkStream (sno);
  return (Yap_unify (ARG1, t1) && Yap_unify (ARG2, t2));
}

static Int
p_open_mem_read_stream (void)   /* $open_mem_read_stream(+List,-Stream) */
{
  Term t, ti;
  StreamDesc *st;
  int sno;
  Int sl = 0, nchars = 0;
  char *nbuf;

  ti = Deref(ARG1);
  while (ti != TermNil) {
    if (IsVarTerm(ti)) {
      Yap_Error(INSTANTIATION_ERROR, ti, "open_mem_read_stream");
      return (FALSE);
    } else if (!IsPairTerm(ti)) {
      Yap_Error(TYPE_ERROR_LIST, ti, "open_mem_read_stream");
      return (FALSE);
    } else {
      sl++;
      ti = TailOfTerm(ti);
    }
  }
  while ((nbuf = (char *)Yap_AllocAtomSpace((sl+1)*sizeof(char))) == NULL) {
    if (!Yap_growheap(FALSE, (sl+1)*sizeof(char), NULL)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
  }
  ti = Deref(ARG1);
  while (ti != TermNil) {
    Term ts = HeadOfTerm(ti);

    if (IsVarTerm(ts)) {
      Yap_Error(INSTANTIATION_ERROR, ARG1, "open_mem_read_stream");
      return (FALSE);
    } else if (!IsIntTerm(ts)) {
      Yap_Error(TYPE_ERROR_INTEGER, ARG1, "open_mem_read_stream");
      return (FALSE);
    }
    nbuf[nchars++] = IntOfTerm(ts);
    ti = TailOfTerm(ti);
  }
  nbuf[nchars] = '\0';
  for (sno = 0; sno < MaxStreams; ++sno)
    if (Stream[sno].status & Free_Stream_f)
      break;
  if (sno == MaxStreams)
    return (PlIOError (SYSTEM_ERROR,TermNil, "new stream not available for open_mem_read_stream/1"));
  st = &Stream[sno];
  /* currently these streams are not seekable */
  st->status = Input_Stream_f | InMemory_Stream_f;
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->stream_putc = MemPutc;
  st->stream_getc = MemGetc;
  if (CharConversionTable != NULL)
    st->stream_getc_for_read = ISOGetc;
  else
    st->stream_getc_for_read = MemGetc;
  st->u.mem_string.pos = 0;
  st->u.mem_string.buf = nbuf;
  st->u.mem_string.max_size = nchars;
  t = MkStream (sno);
  return (Yap_unify (ARG2, t));
}

static Int
p_open_mem_write_stream (void)   /* $open_mem_write_stream(-Stream) */
{
  Term t;
  StreamDesc *st;
  int sno;
  char *nbuf;
  extern int Yap_page_size;

  while ((nbuf = (char *)Yap_AllocAtomSpace(Yap_page_size*sizeof(char))) == NULL) {
    if (!Yap_growheap(FALSE, Yap_page_size*sizeof(char), NULL)) {
      Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
      return(FALSE);
    }
  }
  for (sno = 0; sno < MaxStreams; ++sno)
    if (Stream[sno].status & Free_Stream_f)
      break;
  if (sno == MaxStreams)
    return (PlIOError (SYSTEM_ERROR,TermNil, "new stream not available for open_mem_read_stream/1"));
  st = &Stream[sno];
  /* currently these streams are not seekable */
  st->status = Output_Stream_f | InMemory_Stream_f;
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->stream_putc = MemPutc;
  st->stream_getc = MemGetc;
  if (CharConversionTable != NULL)
    st->stream_getc_for_read = ISOGetc;
  else
    st->stream_getc_for_read = MemGetc;
  st->u.mem_string.pos = 0;
  st->u.mem_string.buf = nbuf;
  st->u.mem_string.max_size = Yap_page_size;
  t = MkStream (sno);
  return (Yap_unify (ARG1, t));
}

static void
ExtendAliasArray(void)
{
  AliasDesc new;
  UInt new_size = SzOfFileAliases+ALIASES_BLOCK_SIZE;

  new = (AliasDesc)Yap_AllocCodeSpace(sizeof(AliasDesc *)*new_size);
  memcpy((void *)new, (void *)FileAliases, sizeof(AliasDesc *)*SzOfFileAliases);
  Yap_FreeCodeSpace((ADDR)FileAliases);
  FileAliases = new;
  SzOfFileAliases = new_size;
}

/* create a new alias arg for stream sno */
static int
AddAlias (Atom arg, int sno)
{
  
  AliasDesc aliasp = FileAliases, aliasp_max = FileAliases+NOfFileAliases;

  while (aliasp < aliasp_max) {
    if (aliasp->name == arg) {
      if (aliasp->alias_stream != sno) {
	return(FALSE);
      }
      return(TRUE);
    }
    aliasp++;
  }
  /* we have not found an alias neither a hole */
  if (aliasp == FileAliases+SzOfFileAliases)
    ExtendAliasArray();
  NOfFileAliases++;
  aliasp->name = arg;
  aliasp->alias_stream = sno;
  return(TRUE);
}

/* create a new alias arg for stream sno */
static void
SetAlias (Atom arg, int sno)
{
  
  AliasDesc aliasp = FileAliases, aliasp_max = FileAliases+NOfFileAliases;

  while (aliasp < aliasp_max) {
    if (aliasp->name == arg) {
      Int alno = aliasp-FileAliases;
      aliasp->alias_stream = sno;
      if (!(Stream[sno].status &
	    (Null_Stream_f|InMemory_Stream_f|Socket_Stream_f))) {
	switch(alno) {
	case 0:
	  Yap_stdin = Stream[sno].u.file.file;
	  break;
	case 1:
	  Yap_stdout = Stream[sno].u.file.file;
	  break;
	case 2:
	  Yap_stderr = Stream[sno].u.file.file;
	  break;
	default:
	  break;
	}
#if HAVE_SETBUF
	YP_setbuf (Stream[sno].u.file.file, NULL); 
#endif /* HAVE_SETBUF */
      }
      return;
    }
    aliasp++;
  }
  /* we have not found an alias, create one */
  if (aliasp == FileAliases+SzOfFileAliases) 
    ExtendAliasArray();
  NOfFileAliases++;
  aliasp->name = arg;
  aliasp->alias_stream = sno;
}

/* purge all aliases for stream sno */
static void
PurgeAlias (int sno)
{
  AliasDesc aliasp = FileAliases, aliasp_max = FileAliases+NOfFileAliases, new_aliasp = aliasp;

  while (aliasp < aliasp_max) {
    if (aliasp->alias_stream == sno) {
      if (aliasp - FileAliases < 3) {
	/* get back to std streams, but keep alias around */
	Int alno = aliasp-FileAliases;
	new_aliasp->alias_stream = alno;
	switch(alno) {
	case 0:
	  Yap_stdin = stdin;
	  break;
	case 1:
	  Yap_stdout = stdout;
	  break;
	case 2:
	  Yap_stderr = stderr;
	  break;
	default:
	  break; /* just put something here */
	}
	new_aliasp++;
      } else {
	NOfFileAliases--;
      }
    } else {
      /* avoid holes in alias array */
      if (new_aliasp != aliasp) {
	new_aliasp->alias_stream = aliasp->alias_stream;
	new_aliasp->name = aliasp->name;
      }
      new_aliasp++;
    }
    aliasp++;
  }
}

/* check if name is an alias */
static int
CheckAlias (Atom arg)
{
  AliasDesc aliasp = FileAliases, aliasp_max = FileAliases+NOfFileAliases;

  while (aliasp < aliasp_max) {
    if (aliasp->name == arg) {
      return(aliasp->alias_stream);
    }
    aliasp++;
  }
  return(-1);
}

/* check if stream has an alias */
static Atom
FetchAlias (int sno)
{
  AliasDesc aliasp = FileAliases, aliasp_max = FileAliases+NOfFileAliases;

  while (aliasp < aliasp_max) {
    if (aliasp->alias_stream == sno) {
      return(aliasp->name);
    }
    aliasp++;
  }
  return(AtomFoundVar);
}

/* check if arg is an alias */
static int
FindAliasForStream (int sno, Atom al)
{
  AliasDesc aliasp = FileAliases, aliasp_max = FileAliases+NOfFileAliases;

  while (aliasp < aliasp_max) {
    if (aliasp->alias_stream == sno && aliasp->name == al) {
      return(TRUE);
    }
    aliasp++;
  }
  return(FALSE);
}

static int
CheckStream (Term arg, int kind, char *msg)
{
  int sno = -1;
  arg = Deref (arg);
  if (IsVarTerm (arg)) {
    Yap_Error(INSTANTIATION_ERROR, arg, msg);
    return (-1);
  }
  else if (IsAtomTerm (arg)) {
    Atom sname = AtomOfTerm (arg);

    if (sname == AtomUser) {
      if (kind & Input_Stream_f) {
	if (kind & (Output_Stream_f|Append_Stream_f)) {
	  Yap_Error(PERMISSION_ERROR_INPUT_STREAM, arg,
		"ambiguous use of 'user' as a stream");
	  return (-1);	    
	}
	sname = AtomUsrIn;
      } else {
	sname = AtomUsrOut;
      }
    }
    if ((sno = CheckAlias(sname)) == -1) {
      Yap_Error(EXISTENCE_ERROR_STREAM, arg, msg);
      return(-1);
    }
  } else if (IsApplTerm (arg) && FunctorOfTerm (arg) == FunctorStream) {
    arg = ArgOfTerm (1, arg);
    if (!IsVarTerm (arg) && IsIntTerm (arg))
      sno = IntOfTerm (arg);
  }
  if (sno < 0)
    {
      Yap_Error(DOMAIN_ERROR_STREAM_OR_ALIAS, arg, msg);
      return (-1);
    }
  if (Stream[sno].status & Free_Stream_f)
    {
      Yap_Error(EXISTENCE_ERROR_STREAM, arg, msg);
      return (-1);
    }
  if ((Stream[sno].status & kind) == 0)
    {
      if (kind & Input_Stream_f)
	Yap_Error(PERMISSION_ERROR_INPUT_STREAM, arg, msg);
      else
	Yap_Error(PERMISSION_ERROR_OUTPUT_STREAM, arg, msg);
      return (-1);
    }
  return (sno);
}

static Int
p_check_stream (void)
{				/* '$check_stream'(Stream,Mode)                  */
  Term mode = Deref (ARG2);
  return (CheckStream (ARG1,
   AtomOfTerm (mode) == AtomRead ? Input_Stream_f : Output_Stream_f,
   "check_stream/2") != -1);
}

static Int
p_check_if_stream (void)
{				/* '$check_stream'(Stream)                  */
  return (CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f | Socket_Stream_f,  "check_stream/1")
	  != -1);
}

static Term
StreamName(int i)
{
  if (i < 3) return(MkAtomTerm(AtomUser));
#if USE_SOCKET
  if (Stream[i].status & Socket_Stream_f)
    return(MkAtomTerm(Yap_LookupAtom("socket")));
  else
#endif
    if (Stream[i].status & Pipe_Stream_f)
      return(MkAtomTerm(Yap_LookupAtom("pipe")));
    if (Stream[i].status & InMemory_Stream_f)
      return(MkAtomTerm(Yap_LookupAtom("charsio")));
    else {
      if (yap_flags[LANGUAGE_MODE_FLAG] == ISO_CHARACTER_ESCAPES) {
	return(Stream[i].u.file.user_name);
      } else
	return(MkAtomTerm(Stream[i].u.file.name));
    }
}

static Int
init_cur_s (void)
{				/* Init current_stream */
  Term t3 = Deref(ARG3);
  if (!IsVarTerm(t3)) {
    
    Int i;
    Term t1, t2;

    i = CheckStream (t3, Input_Stream_f|Output_Stream_f, "current_stream/3");
    if (i < 0) {
      return(FALSE);
    }
    t1 = StreamName(i);
    t2 = (Stream[i].status & Input_Stream_f ?
	  MkAtomTerm (AtomRead) :
	  MkAtomTerm (AtomWrite));
    if (Yap_unify(ARG1,t1) && Yap_unify(ARG2,t2)) {
      cut_succeed();
    } else {
      cut_fail();
    }
  } else {
    EXTRA_CBACK_ARG (3, 1) = MkIntTerm (0);
    return (cont_cur_s ());
  }
}

static Int
cont_cur_s (void)
{				/* current_stream */
  Term t1, t2, t3;
  int i = IntOfTerm (EXTRA_CBACK_ARG (3, 1));
  while (i < MaxStreams)
    {
      if (Stream[i].status & Free_Stream_f)
	{
	  ++i;
	  continue;
	}
      t1 = StreamName(i);
      t2 = (Stream[i].status & Input_Stream_f ?
	    MkAtomTerm (AtomRead) :
	    MkAtomTerm (AtomWrite));
      t3 = MkStream (i++);
      EXTRA_CBACK_ARG (3, 1) = Unsigned (MkIntTerm (i));
      if (Yap_unify (ARG3, t3) && Yap_unify_constant (ARG1, t1) && Yap_unify_constant (ARG2, t2))
	{
	  return (TRUE);
	}
      else
	{
	  return(FALSE);
	}
    }
  cut_fail();
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
#if _MSC_VER || defined(__MINGW32__) 
    if (Stream[sno].status & Pipe_Stream_f)
      CloseHandle (Stream[sno].u.pipe.hdl);
#else
    if (Stream[sno].status & (Pipe_Stream_f|Socket_Stream_f))
      close (Stream[sno].u.pipe.fd);
#endif
#if USE_SOCKET
    else if (Stream[sno].status & (Socket_Stream_f)) {
      Yap_CloseSocket(Stream[sno].u.socket.fd,
		  Stream[sno].u.socket.flags,
		  Stream[sno].u.socket.domain);
    } 
#endif
    else if (Stream[sno].status & InMemory_Stream_f) {
      Yap_FreeAtomSpace(Stream[sno].u.mem_string.buf);
    } else if (!(Stream[sno].status & Null_Stream_f))
      YP_fclose (Stream[sno].u.file.file);
    else {
      if (loud)
	fprintf (Yap_stderr, "[ Error: while closing stream: %s ]\n", RepAtom (Stream[sno].u.file.name)->StrOfAE);
    }
    if (Yap_c_input_stream == sno)
      {
	Yap_c_input_stream = StdInStream;
      }
    else if (Yap_c_output_stream == sno)
      {
	Yap_c_output_stream = StdOutStream;
      }
  }
  Stream[sno].status = Free_Stream_f;
}


static void
CloseStream(int sno)
{
  if (!(Stream[sno].status & (Null_Stream_f|Socket_Stream_f|InMemory_Stream_f|Pipe_Stream_f)))
    YP_fclose (Stream[sno].u.file.file);
#if USE_SOCKET
  else if (Stream[sno].status & (Socket_Stream_f)) {
    Yap_CloseSocket(Stream[sno].u.socket.fd,
		Stream[sno].u.socket.flags,
		Stream[sno].u.socket.domain);
  }
#endif
  else if (Stream[sno].status & Pipe_Stream_f) {
#if _MSC_VER || defined(__MINGW32__) 
    CloseHandle (Stream[sno].u.pipe.hdl);
#else
    close(Stream[sno].u.pipe.fd);
#endif
  }
  else if (Stream[sno].status & (InMemory_Stream_f)) {
    Yap_FreeAtomSpace(Stream[sno].u.mem_string.buf);
  }
  Stream[sno].status = Free_Stream_f;
  PurgeAlias(sno);
  if (Yap_c_input_stream == sno)
    {
      Yap_c_input_stream = StdInStream;
    }
  else if (Yap_c_output_stream == sno)
    {
      Yap_c_output_stream = StdOutStream;
    }
  /*  if (st->status == Socket_Stream_f|Input_Stream_f|Output_Stream_f) {
    Yap_CloseSocket();
  }
  */
}

void
Yap_CloseStream(int sno)
{
  CloseStream(sno);
}

static Int
p_close (void)
{				/* '$close'(+Stream) */
  Int sno = CheckStream (ARG1, (Input_Stream_f | Output_Stream_f | Socket_Stream_f), "close/2");
  if (sno < 0)
    return (FALSE);
  if (sno <= StdErrStream)
    return (TRUE);
  CloseStream(sno);
  return (TRUE);
}

static Int
p_peek_mem_write_stream (void)
{				/* '$peek_mem_write_stream'(+Stream,?S0,?S) */
  Int sno = CheckStream (ARG1, (Output_Stream_f | InMemory_Stream_f), "close/2");
  Int i = Stream[sno].u.mem_string.pos;
  Term tf = ARG2;
  CELL *HI;

  if (sno < 0)
    return (FALSE);
 restart:
  HI = H;
  while (i > 0) {
    --i;
    tf = MkPairTerm(MkIntTerm(Stream[sno].u.mem_string.buf[i]),tf);
    if (H + 1024 >= ASP) {
      H = HI;
      if (!Yap_gc(3, ENV, P)) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      }
      i = 0;
      tf = ARG2;
      goto restart;
    }
  }
  return (Yap_unify(ARG3,tf));
}

static Int
p_past_eof (void)
{				/* at_end_of_stream */
  /* the next character is a EOF */ 
  int sno = CheckStream (ARG1, Input_Stream_f, "past_eof/1");

  if (sno < 0)
    return (FALSE);
  return(Stream[sno].status & Eof_Stream_f);
}

static Int
p_peek_byte (void)
{				/* at_end_of_stream */
  /* the next character is a EOF */ 
  int sno = CheckStream (ARG1, Input_Stream_f, "peek/2");
  StreamDesc *s;
  Int ocharcount, olinecount, olinepos;
  Int status;
  Int ch;

  if (sno < 0)
    return(FALSE);
  status = Stream[sno].status;
  if (!(status & Binary_Stream_f)) {
    Yap_Error(PERMISSION_ERROR_INPUT_TEXT_STREAM, ARG1, "peek/2");
    return(FALSE);
  }
  if (status & Eof_Stream_f) {
    Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, ARG1, "peek/2");
    return(FALSE);
  }
  s = Stream+sno;
  ocharcount = s->charcount;
  olinecount = s->linecount;
  olinepos = s->linepos;
  ch = Stream[sno].stream_getc(sno);
  s->charcount = ocharcount;
  s->linecount = olinecount;
  s->linepos = olinepos;
  /* buffer the character */
  s->och = ch;
  /* mark a special function to recover this character */
  s->stream_getc = PlUnGetc;
  if (CharConversionTable != NULL)
    s->stream_getc_for_read = ISOGetc;
  else
    s->stream_getc_for_read = s->stream_getc;
  return(Yap_unify_constant(ARG2,MkIntTerm(ch)));
}

static Int
p_peek (void)
{				/* at_end_of_stream */
  /* the next character is a EOF */ 
  int sno = CheckStream (ARG1, Input_Stream_f, "peek/2");
  StreamDesc *s;
  Int ocharcount, olinecount, olinepos;
  Int status;
  Int ch;

  if (sno < 0)
    return(FALSE);
  status = Stream[sno].status;
  if (status & (Binary_Stream_f|Eof_Stream_f)) {
    if (status & Binary_Stream_f) {
      Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, ARG1, "peek/2");
      return(FALSE);
    } else if (status & (Eof_Error_Stream_f)) {
      Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, ARG1, "peek/2");
      return(FALSE);
    }
  }
  s = Stream+sno;
  ocharcount = s->charcount;
  olinecount = s->linecount;
  olinepos = s->linepos;
  ch = Stream[sno].stream_getc(sno);
  s->charcount = ocharcount;
  s->linecount = olinecount;
  s->linepos = olinepos;
  /* buffer the character */
  s->och = ch;
  /* mark a special function to recover this character */
  s->stream_getc = PlUnGetc;
  if (CharConversionTable != NULL)
    s->stream_getc_for_read = ISOGetc;
  else
    s->stream_getc_for_read = s->stream_getc;
  return(Yap_unify_constant(ARG2,MkIntTerm(ch)));
}

static Int
p_set_input (void)
{				/* '$set_input'(+Stream,-ErrorMessage)   */
  Int sno = CheckStream (ARG1, Input_Stream_f, "set_input/1");
  if (sno < 0)
    return (FALSE);
  Yap_c_input_stream = sno;
  return (TRUE);
}

static Int
p_set_output (void)
{				/* '$set_output'(+Stream,-ErrorMessage)  */
  Int sno = CheckStream (ARG1, Output_Stream_f, "set_output/1");
  if (sno < 0)
    return (FALSE);
  Yap_c_output_stream = sno;
  return (TRUE);
}

static Int
p_current_input (void)
{				/* current_input(?Stream)                */
  Term t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    Term t = MkStream (Yap_c_input_stream);
    BIND(VarOfTerm(t1), t, bind_in_current_input);
#ifdef COROUTINING
    DO_TRAIL(CellPtr(t1), t);
    if (CellPtr(t1) < H0) Yap_WakeUp(VarOfTerm(t1));
  bind_in_current_input:
#endif
    return(TRUE);
  } else if (!IsApplTerm(t1) ||
	     FunctorOfTerm(t1) != FunctorStream ||
	     !IsIntTerm((t1=ArgOfTerm(1,t1)))) {
    Yap_Error(DOMAIN_ERROR_STREAM,t1,"current_input/1");
    return(FALSE);
  } else {
    return(Yap_c_input_stream == IntOfTerm(t1));
  }
}

static Int
p_current_output (void)
{				/* current_output(?Stream)               */
  Term t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    Term t = MkStream (Yap_c_output_stream);
    BIND((CELL *)t1, t, bind_in_current_output);
#ifdef COROUTINING
    DO_TRAIL(CellPtr(t1), t);
    if (CellPtr(t1) < H0) Yap_WakeUp(VarOfTerm(t1));
  bind_in_current_output:
#endif
    return(TRUE);
  } else if (!IsApplTerm(t1) ||
	     FunctorOfTerm(t1) != FunctorStream ||
	     !IsIntTerm((t1=ArgOfTerm(1,t1)))) {
    Yap_Error(DOMAIN_ERROR_STREAM,t1,"current_output/1");
    return(FALSE);
  } else {
    return(Yap_c_output_stream == IntOfTerm(t1));
  }
}

static Int
p_write (void)
{				/* '$write'(+Flags,?Term) */
  int flags = (int) IntOfTerm (Deref (ARG1));
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  *--ASP = MkIntTerm(0);
  Yap_plwrite (ARG2, Stream[Yap_c_output_stream].stream_putc, flags);
  if (EX != 0L) {
    Term ball = EX;
    EX = 0L;
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
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  *--ASP = MkIntTerm(0);
  Yap_plwrite (ARG3, Stream[Yap_c_output_stream].stream_putc, (int) IntOfTerm (Deref (ARG2)));
  Yap_c_output_stream = old_output_stream;
  if (EX != 0L) {
    Term ball = EX;
    EX = 0L;
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
syntax_error (TokEntry * tokptr)
{
  Term info;
  int count = 0, out = 0;
  Int start, err = 0, end;
  Term tf[6];
  Term *error = tf+3;
  CELL *Hi = H;

  start = tokptr->TokPos;
  clean_vars(Yap_VarTable);
  clean_vars(Yap_AnonVarTable);
  while (1) {
    Term ts[2];

    if (H > ASP-1024) {
      H = Hi;
      tf[3] = TermNil;
      err = 0;
      end = 0;
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
	Term t0 = MkAtomTerm((Atom)info);
	ts[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("atom"),1),1,&t0);
      }
      break;
    case Number_tok:
      ts[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("number"),1),1,&(tokptr->TokInfo));
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
	ts[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("var"),3),3,t);
      }
      break;
    case String_tok:
      {
	Term t0 = Yap_StringToList((char *)info);
	ts[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("string"),1),1,&t0);
      }
      break;
    case Error_tok:
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
	MkPairTerm(Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("-"),2),2,ts),TermNil);
      error = RepPair(*error)+1;
      count++;
    }
    tokptr = tokptr->TokNext;
  }
  tf[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("read"),1),1,&ARG2);
  {
    Term t[3];
    t[0] = MkIntegerTerm(start);
    t[1] = MkIntegerTerm(err);
    t[2] = MkIntegerTerm(end);
    tf[1] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("between"),3),3,t);
  }
  tf[2] = MkAtomTerm(Yap_LookupAtom("\n<==== HERE ====>\n"));
  tf[4] = MkIntegerTerm(out);
  tf[5] = MkIntegerTerm(err);
  return(Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("syntax_error"),6),6,tf));
}

Int
Yap_FirstLineInParse (void)
{
  return(StartLine);
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
    t = MkAtomTerm(Yap_LookupAtom("fail"));
    break;
  case EXCEPTION_ON_PARSER_ERROR:
    t = MkAtomTerm(Yap_LookupAtom("error"));
    break;
  case QUIET_ON_PARSER_ERROR:
    t = MkAtomTerm(Yap_LookupAtom("quiet"));
    break;
  case CONTINUE_ON_PARSER_ERROR:
    t = MkAtomTerm(Yap_LookupAtom("dec10"));
    break;
  default:
    Yap_Error(SYSTEM_ERROR,TermNil,"corrupted syntax_error handler");
    return(FALSE);
  }
  return (Yap_unify_constant (ARG1, t));
}

static Int
do_read(int inp_stream)
{
  Term t, v;
  TokEntry *tokstart;
#if EMACS
  int emacs_cares = FALSE;
#endif
  tr_fr_ptr old_TR, TR_before_parse;
    
  if (Stream[inp_stream].status & Binary_Stream_f) {
    Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, MkAtomTerm(Stream[inp_stream].u.file.name), "read_term/2");
    return(FALSE);
  }
  old_TR = TR;
  while (TRUE) {
    CELL *old_H;

    /* Scans the term using stack space */
    Yap_eot_before_eof = FALSE;
    tokstart = Yap_tokptr = Yap_toktide = Yap_tokenizer (inp_stream);
    /* preserve value of H after scanning: otherwise we may lose strings
       and floats */
    old_H = H;
    if (Stream[inp_stream].status & Eof_Stream_f) {
      if (Yap_eot_before_eof) {
	/* next read should give out an end of file */
	Stream[inp_stream].status |= Push_Eof_Stream_f;
      } else {
	if (tokstart != NIL && tokstart->Tok != Ord (eot_tok)) {
	  /* we got the end of file from an abort */
	  if (Yap_ErrorMessage == "Abort") {
	  TR = old_TR;
	  return(FALSE);
	}
	  /* we need to force the next reading to also give end of file.*/
	  Stream[inp_stream].status |= Push_Eof_Stream_f;
	  Yap_ErrorMessage = "end of file found before end of term";
	} else {
	  /* restore TR */
	  TR = old_TR;
	
	  return (Yap_unify(MkIntegerTerm(StartLine = Stream[inp_stream].linecount),ARG4) &&
		  Yap_unify_constant (ARG2, MkAtomTerm (AtomEof)));
	}
      }
    }
  repeat_cycle:
    TR_before_parse = TR;
    if (Yap_ErrorMessage || (t = Yap_Parse ()) == 0) {
      if (Yap_ErrorMessage && (strcmp(Yap_ErrorMessage,"Stack Overflow") == 0)) {
	/* ignore term we just built */
	TR = TR_before_parse;
	H = old_H;
	if (Yap_growstack_in_parser(&old_TR, &tokstart, &Yap_VarTable)) {
	  old_H = H;
	  Yap_tokptr = Yap_toktide = tokstart;
	  Yap_ErrorMessage = NULL;
	  goto repeat_cycle;
	}
      }
      TR = old_TR;
      if (ParserErrorStyle == QUIET_ON_PARSER_ERROR) {
	/* just fail */
	return(FALSE);
      } else if (ParserErrorStyle == CONTINUE_ON_PARSER_ERROR) {
	Yap_ErrorMessage = NULL;
	TR = TR_before_parse;
	/* try again */
	goto repeat_cycle;
      } else {
	Term terr = syntax_error(tokstart);
	if (Yap_ErrorMessage == NULL)
	  Yap_ErrorMessage = "SYNTAX ERROR";
	
	if (ParserErrorStyle == EXCEPTION_ON_PARSER_ERROR) {
	  Yap_Error(SYNTAX_ERROR,terr,Yap_ErrorMessage);
	  return(FALSE);
	} else /* FAIL ON PARSER ERROR */ {
	  Term t[2];
	  t[0] = terr;
	  t[1] = MkAtomTerm(Yap_LookupAtom(Yap_ErrorMessage));
	  return(Yap_unify(MkIntTerm(StartLine = tokstart->TokPos),ARG4) &&
		 Yap_unify(ARG5,Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("error"),2),2,t)));
	}
      }
    } else {
      /* parsing succeeded */
      break;
    }
  }
#if EMACS
  first_char = tokstart->TokPos;
#endif /* EMACS */
  if (AtomOfTerm (Deref (ARG1)) == AtomTrue) {
    while (TRUE) {
      CELL *old_H = H;

      if (setjmp(Yap_IOBotch) == 0) {
	v = Yap_VarNames(Yap_VarTable, TermNil);
	TR = old_TR;
	break;
      } else {
	/* don't need to recheck tokens */
	tokstart = NULL;
	/* restart global */
	H = old_H;
	Yap_growstack_in_parser(&old_TR, &tokstart, &Yap_VarTable);
	old_H = H;
      }
    }
    return(Yap_unify(t, ARG2) && Yap_unify (v, ARG3) &&
	   Yap_unify(MkIntTerm(StartLine = tokstart->TokPos),ARG4));
  } else {
    TR = old_TR;
    return(Yap_unify(t, ARG2) && Yap_unify(MkIntTerm(StartLine = tokstart->TokPos),ARG4));
  }
}

static Int
p_read (void)
{				/* '$read'(+Flag,?Term,?Vars,-Pos,-Err)    */
  return(do_read(Yap_c_input_stream));
}

static Int
p_read2 (void)
{				/* '$read2'(+Flag,?Term,?Vars,-Pos,-Err,+Stream)  */
  int inp_stream;

  /* needs to change Yap_c_output_stream for write */
  inp_stream = CheckStream (ARG6, Input_Stream_f, "read/3");
  if (inp_stream == -1) {
    return(FALSE);
  }
  return(do_read(inp_stream));
}

static Int
p_user_file_name (void)
{
  Term tout;
  int sno = CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f,"user_file_name/2");
  if (sno < 0)
    return (FALSE);
#if USE_SOCKET
  if (Stream[sno].status & Socket_Stream_f)
    tout = MkAtomTerm(Yap_LookupAtom("socket"));
  else
#endif
  if (Stream[sno].status & Pipe_Stream_f)
    tout = MkAtomTerm(Yap_LookupAtom("pipe"));
  else if (Stream[sno].status & InMemory_Stream_f)
    tout = MkAtomTerm(Yap_LookupAtom("charsio"));
  else
    tout = Stream[sno].u.file.user_name;
  return (Yap_unify_constant (ARG2, tout));
}

static Int
p_file_name (void)
{
  Term tout;
  int sno = CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f,"file_name/2");
  if (sno < 0)
    return (FALSE);
#if USE_SOCKET
  if (Stream[sno].status & Socket_Stream_f)
    tout = MkAtomTerm(Yap_LookupAtom("socket"));
  else
#endif
  if (Stream[sno].status & Pipe_Stream_f)
    tout = MkAtomTerm(Yap_LookupAtom("pipe"));
  else if (Stream[sno].status & InMemory_Stream_f)
    tout = MkAtomTerm(Yap_LookupAtom("charsio"));
  else
    tout = MkAtomTerm(Stream[sno].u.file.name);
  return (Yap_unify_constant (ARG2, tout));
}

static Int
p_cur_line_no (void)
{				/* '$current_line_number'(+Stream,-N) */
  Term tout;
  int sno =
  CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f,"current_line_number/2");
  if (sno < 0)
    return (FALSE);
  /* one has to be somewhat more careful because of terminals */
  if (Stream[sno].status & Tty_Stream_f)
    {
      Int no = 1;
      int i;
      Atom my_stream;
#if USE_SOCKET
      if (Stream[sno].status & Socket_Stream_f)
	my_stream = Yap_LookupAtom("socket");
      else
#endif
      if (Stream[sno].status & Pipe_Stream_f)
	my_stream = Yap_LookupAtom("pipe");
      else
	if (Stream[sno].status & InMemory_Stream_f)
	  my_stream = Yap_LookupAtom("charsio");
	else
	  my_stream = Stream[sno].u.file.name;
      for (i = 0; i < MaxStreams; i++)
	{
	  if (!(Stream[i].status & (Free_Stream_f|Socket_Stream_f|Pipe_Stream_f|InMemory_Stream_f)) &&
	      Stream[i].u.file.name == my_stream)
	    no += Stream[i].linecount - 1;
	}
      tout = MkIntTerm (no);
    }
  else
    tout = MkIntTerm (Stream[sno].linecount);
  return (Yap_unify_constant (ARG2, tout));
}

static Int
p_line_position (void)
{				/* '$line_position'(+Stream,-N) */
  Term tout;
  int sno =
  CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f, "line_position/2");
  if (sno < 0)
    return (FALSE);
  if (Stream[sno].status & Tty_Stream_f)
    {
      Int no = 0;
      int i;
      Atom my_stream = Stream[sno].u.file.name;
      for (i = 0; i < MaxStreams; i++)
	{
	  if (!(Stream[i].status & Free_Stream_f) &&
	      Stream[i].u.file.name == my_stream)
	    no += Stream[i].linepos;
	}
      tout = MkIntTerm (no);
    }
  else
    tout = MkIntTerm (Stream[sno].linepos);
  return (Yap_unify_constant (ARG2, tout));
}

static Int
p_character_count (void)
{				/* '$character_count'(+Stream,-N) */
  Term tout;
  int sno =
  CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f, "character_count/2");
  if (sno < 0)
    return (FALSE);
  if (Stream[sno].status & Tty_Stream_f)
    {
      Int no = 0;
      int i;
      Atom my_stream = Stream[sno].u.file.name;
      for (i = 0; i < MaxStreams; i++)
	{
	  if (!(Stream[i].status & Free_Stream_f) &&
	      Stream[i].u.file.name == my_stream)
	    no += Stream[i].charcount;
	}
      tout = MkIntTerm (no);
    }
  else if (Stream[sno].status & Null_Stream_f)
    tout = MkIntTerm (Stream[sno].charcount);
  else
    tout = MkIntTerm (YP_ftell (Stream[sno].u.file.file));
  return (Yap_unify_constant (ARG2, tout));
}

static Int
p_show_stream_flags(void)
{				/* '$show_stream_flags'(+Stream,Pos) */
  Term tout;
  int sno =
    CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f, "stream_property/2");
  if (sno < 0)
    return (FALSE);
  tout = MkIntTerm(Stream[sno].status);
  return (Yap_unify (ARG2, tout));
}

static Int
p_show_stream_position (void)
{				/* '$show_stream_position'(+Stream,Pos) */
  Term sargs[3], tout;
  int sno =
  CheckStream (ARG1, Input_Stream_f | Output_Stream_f | Append_Stream_f, "stream_position/2");
  if (sno < 0)
    return (FALSE);
  if (Stream[sno].status & (Tty_Stream_f|Socket_Stream_f|Pipe_Stream_f|InMemory_Stream_f))
    sargs[0] = MkIntTerm (Stream[sno].charcount);
  else if (Stream[sno].status & Null_Stream_f)
    sargs[0] = MkIntTerm (Stream[sno].charcount);
  else
    sargs[0] = MkIntTerm (YP_ftell (Stream[sno].u.file.file));
  sargs[1] = MkIntTerm (Stream[sno].linecount);
  sargs[2] = MkIntTerm (Stream[sno].linepos);
  tout = Yap_MkApplTerm (FunctorStreamPos, 3, sargs);
  return (Yap_unify (ARG2, tout));
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
    Yap_Error(INSTANTIATION_ERROR, tin, "set_stream_position/2");
    return (FALSE);
  } else if (!(IsApplTerm (tin))) {
    Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
    return (FALSE);
  }
  if (FunctorOfTerm (tin) == FunctorStreamPos) {
    if (IsVarTerm (tp = ArgOfTerm (1, tin))) {
      Yap_Error(INSTANTIATION_ERROR, tp, "set_stream_position/2");
      return (FALSE);    
    } else if (!IsIntTerm (tp)) {
      Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
      return (FALSE);
    }
    if (!(Stream[sno].status & Seekable_Stream_f) ) {
      Yap_Error(PERMISSION_ERROR_REPOSITION_STREAM, ARG1,"set_stream_position/2");
      return(FALSE);
    }
    char_pos = IntOfTerm (tp);
    if (IsVarTerm (tp = ArgOfTerm (2, tin))) {
      Yap_Error(INSTANTIATION_ERROR, tp, "set_stream_position/2");
      return (FALSE);    
    } else if (!IsIntTerm (tp)) {
      Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
      return (FALSE);
    }
    Stream[sno].charcount = char_pos;
    Stream[sno].linecount = IntOfTerm (tp);
    if (IsVarTerm (tp = ArgOfTerm (2, tin))) {
      Yap_Error(INSTANTIATION_ERROR, tp, "set_stream_position/2");
      return (FALSE);    
    } else if (!IsIntTerm (tp)) {
      Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
      return (FALSE);
    }
    Stream[sno].linepos = IntOfTerm (tp);
    if (YP_fseek (Stream[sno].u.file.file, (long) (char_pos), 0) == -1) {
      Yap_Error(SYSTEM_ERROR, tp, 
	    "fseek failed for set_stream_position/2");
      return(FALSE);
    }
  } else if (FunctorOfTerm (tin) == FunctorStreamEOS) {
    if (IsVarTerm (tp = ArgOfTerm (1, tin))) {
      Yap_Error(INSTANTIATION_ERROR, tp, "set_stream_position/2");
      return (FALSE);    
    } else if (tp != MkAtomTerm(Yap_LookupAtom("at"))) {
      Yap_Error(DOMAIN_ERROR_STREAM_POSITION, tin, "set_stream_position/2");
      return (FALSE);
    }
    if (!(Stream[sno].status & Seekable_Stream_f) ) {
      Yap_Error(PERMISSION_ERROR_REPOSITION_STREAM, ARG1,"set_stream_position/2");
      return(FALSE);
    }
    if (YP_fseek (Stream[sno].u.file.file, 0L, SEEK_END) == -1) {
      Yap_Error(SYSTEM_ERROR, tp, 
	    "fseek failed for set_stream_position/2");
      return(FALSE);
    }
    /* reset the counters */
    Stream[sno].linepos = 0;
    Stream[sno].linecount = 0;
    Stream[sno].charcount = 0;
  }
  return (TRUE);
}

static Int
p_get (void)
{				/* '$get'(Stream,-N)                     */
  int sno = CheckStream (ARG1, Input_Stream_f, "get/2");
  Int ch;
  Int status;

  if (sno < 0)
    return(FALSE);
  status = Stream[sno].status;
  if (status & (Binary_Stream_f|Eof_Stream_f)) {
    if (status & Binary_Stream_f) {
      Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, ARG1, "get/2");
      return(FALSE);
    } else if (status & Eof_Error_Stream_f) {
      Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, ARG1, "get/2");
      return(FALSE);
    }
  }
  while ((ch = Stream[sno].stream_getc(sno)) <= 32 && ch >= 0);
  return (Yap_unify_constant (ARG2, MkIntTerm (ch)));
}

static Int
p_get0 (void)
{				/* '$get0'(Stream,-N)                    */
  int sno = CheckStream (ARG1, Input_Stream_f, "get0/2");
  Int status;
  Int out;

  if (sno < 0)
    return(FALSE);
  status = Stream[sno].status;
  if (status & (Binary_Stream_f|Eof_Stream_f)) {
    if (status & Binary_Stream_f) {
      Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, ARG1, "get0/2");
      return(FALSE);
    } else if (status & (Eof_Error_Stream_f)) {
      Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, ARG1, "get0/2");
      return(FALSE);
    }
  }
  out = Stream[sno].stream_getc(sno);
  return (Yap_unify_constant (ARG2, MkIntTerm (out)) );
}

static Term
read_line(int sno) 
{
  Term tail;
  Int ch;

  if ((ch = Stream[sno].stream_getc(sno)) == 10) {
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

  if (sno < 0)
    return(FALSE);
  status = Stream[sno].status;
  if (status & (Binary_Stream_f|Eof_Stream_f)) {
    if (status & Binary_Stream_f) {
      Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, ARG1, "get0/2");
      return(FALSE);
    } else if (status & (Eof_Error_Stream_f)) {
      Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, ARG1, "get0/2");
      return(FALSE);
    }
  }
  out = read_line(sno);
  return(Yap_unify(out,ARG2));
}

static Int
p_get_byte (void)
{				/* '$get_byte'(Stream,-N)                    */
  int sno = CheckStream (ARG1, Input_Stream_f, "get_byte/2");
  Int status;

  if (sno < 0)
    return(FALSE);
  status = Stream[sno].status;
  if (!(status & Binary_Stream_f)) {
    Yap_Error(PERMISSION_ERROR_INPUT_TEXT_STREAM, ARG1, "get_byte/2");
    return(FALSE);
  }
  if ((status & (Eof_Stream_f|Eof_Error_Stream_f)) == (Eof_Stream_f|Eof_Error_Stream_f)) {
    Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, ARG1, "get_byte/2");
    return(FALSE);
  }
  return (Yap_unify_constant (ARG2, MkIntTerm (Stream[sno].stream_getc(sno))));
}

static Int
p_put (void)
{				/* '$put'(Stream,N)                      */
  int sno = CheckStream (ARG1, Output_Stream_f, "put/2");
  if (sno < 0)
    return (FALSE);
  if (Stream[sno].status & Binary_Stream_f) {
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "get0/2");
    return(FALSE);
  }
  Stream[sno].stream_putc (sno, (int) IntOfTerm (Deref (ARG2)));
  /*
   * if (!(Stream[sno].status & Null_Stream_f))
   * yap_fflush(Stream[sno].u.file.file); 
   */
  return (TRUE);
}

static Int
p_put_byte (void)
{				/* '$put_byte'(Stream,N)                 */
  int sno = CheckStream (ARG1, Output_Stream_f, "put/2");
  if (sno < 0)
    return (FALSE);
  if (!(Stream[sno].status & Binary_Stream_f)) {
    Yap_Error(PERMISSION_ERROR_OUTPUT_TEXT_STREAM, ARG1, "get0/2");
    return(FALSE);
  }
  Stream[sno].stream_putc(sno, (int) IntOfTerm (Deref (ARG2)));
  /*
   * if (!(Stream[sno].status & Null_Stream_f))
   * yap_fflush(Stream[sno].u.file.file); 
   */
  return (TRUE);
}

static int format_error = FALSE;

static Int
GetArgSizeFromThirdArg (char **pptr, Term * termptr)
{
  Term args = *termptr, arghd;
  char *ptr = *pptr;
  Int res;
  if (IsVarTerm(args)) {
    format_error = TRUE;
    Yap_Error(INSTANTIATION_ERROR, args, "format/2");
    return(0);
  } else if (!IsPairTerm (args)) {
    format_error = TRUE;
    Yap_Error(TYPE_ERROR_LIST, args, "format/2");
    return(0);
  }
  arghd = HeadOfTerm (args);
  args = TailOfTerm (args);
  if (IsVarTerm(arghd)) {
    format_error = TRUE;
    Yap_Error(INSTANTIATION_ERROR, arghd, "format/2");
    return(0);
  } else if (!IsIntTerm (arghd)) {
    format_error = TRUE;
    Yap_Error(TYPE_ERROR_LIST, arghd, "format/2");
    return(0);
  }
  res = IntOfTerm (arghd);
  if (res < 0) {
    format_error = TRUE;
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, arghd, "format/2");
    return (0);
  }
#if SHORT_INTS
  sprintf (ptr, "%ld", res);
#else
  sprintf (ptr, "%d", res);
#endif
  while (*ptr)
    ptr++;
  *pptr = ptr;
  *termptr = args;
  return (res);
}

static Int
GetArgSizeFromChar (Term * args)
{
  Term arghd, argtl = *args;
  Int val;
  if (IsVarTerm(argtl)) {
    format_error = TRUE;
    Yap_Error(INSTANTIATION_ERROR, argtl, "format/2");
    return(0);
  } else if (!IsPairTerm (argtl)) {
    format_error = TRUE;
    Yap_Error(TYPE_ERROR_LIST, argtl, "format/2");
    return(0);
  }
  arghd = HeadOfTerm (argtl);
  argtl = TailOfTerm (argtl);
  if (IsVarTerm(arghd)) {
    format_error = TRUE;
    Yap_Error(INSTANTIATION_ERROR, arghd, "format/2");
    return(0);
  } else if (!IsIntTerm (arghd)) {
    format_error = TRUE;
    Yap_Error(TYPE_ERROR_LIST, arghd, "format/2");
    return(0);
  }
  val = IntOfTerm (arghd);
  if (IsVarTerm(argtl)) {
    format_error = TRUE;
    Yap_Error(INSTANTIATION_ERROR, argtl, "format/2");
    return(0);
  } else if (!IsPairTerm (argtl)) {
    format_error = TRUE;
    Yap_Error(TYPE_ERROR_LIST, argtl, "format/2");
    return(0);
  }
  arghd = HeadOfTerm (argtl);
  argtl = TailOfTerm (argtl);
  if (IsVarTerm(arghd)) {
    format_error = TRUE;
    Yap_Error(INSTANTIATION_ERROR, arghd, "format/2");
    return(0);
  } else if (!IsIntTerm (arghd)) {
    format_error = TRUE;
    Yap_Error(TYPE_ERROR_LIST, arghd, "format/2");
    return(0);
  }
  if (IntOfTerm (arghd) != 't') {
    format_error = TRUE;
    Yap_Error(TYPE_ERROR_LIST, arghd, "format/2");
    return (0);
  }
  *args = argtl;
  return (val);
}

static Int
GetArgSizeFromChars (char **pptr, Int * intptr, Term * termptr)
{
  Term args = *termptr, arghd;
  char *ptr = *pptr;
  Int up_to_now = *intptr;
  int ch;
  if (IsVarTerm(args)) {
    format_error = TRUE;
    Yap_Error(INSTANTIATION_ERROR, args, "format/2");
    return(0);
  } else if (!IsPairTerm (args)) {
    format_error = TRUE;
    Yap_Error(TYPE_ERROR_LIST, args, "format/2");
    return(0);
  }
  arghd = HeadOfTerm (args);
  args = TailOfTerm (args);
  if (IsVarTerm(arghd)) {
    format_error = TRUE;
    Yap_Error(INSTANTIATION_ERROR, arghd, "format/2");
    return(0);
  } else if (!IsIntTerm (arghd)) {
    format_error = TRUE;
    Yap_Error(TYPE_ERROR_LIST, arghd, "format/2");
    return(0);
  }
  ch = IntOfTerm (arghd);
  while (ch >= '0' && ch <= '9')
    {
      *ptr++ = ch;
      up_to_now = up_to_now * 10 + ch - '0';
      if (IsVarTerm(args)) {
	format_error = TRUE;
	Yap_Error(INSTANTIATION_ERROR, args, "format/2");
	return(0);
      } else if (!IsPairTerm (args)) {
	format_error = TRUE;
	Yap_Error(TYPE_ERROR_LIST, args, "format/2");
	return(0);
      }
      arghd = HeadOfTerm (args);
      args = TailOfTerm (args);
      if (IsVarTerm(arghd)) {
	format_error = TRUE;
	Yap_Error(INSTANTIATION_ERROR, arghd, "format/2");
	return(0);
      } else if (!IsIntTerm (arghd)) {
	format_error = TRUE;
	Yap_Error(TYPE_ERROR_LIST, arghd, "format/2");
	return(0);
      }
      ch = (int) IntOfTerm (arghd);
    }
  *intptr = up_to_now;
  *termptr = args;
  *pptr = ptr;
  return (ch);
}


#define FORMAT_MAX_SIZE 256

static char *format_ptr, *format_base, *format_max;
static int   format_buf_size;

typedef struct {
  Int pos;                   /* tab point */
  char pad;                  /* ok, it's not standard english */
} pads;

static  pads pad_entries[16], *pad_max = pad_entries;

static int
format_putc(int sno, int ch) {
  if (format_buf_size == -1)
    return(EOF);
  if (ch == 10) {
    char *ptr = format_base;
#if MAC || _MSC_VER
    ch = '\n';
#endif
    for (ptr = format_base; ptr < format_ptr; ptr++) {
      Stream[sno].stream_putc(sno, *ptr);
    }
    /* reset line */
    format_ptr = format_base;
    pad_max = pad_entries;
    Stream[sno].stream_putc(sno, '\n');
    return((int)10);
  } else {
    *format_ptr++ = (char)ch;
    if (format_ptr == format_max) {
      /* oops, we have reached an overflow */
      Int new_max_size = format_buf_size + FORMAT_MAX_SIZE;
      char *newbuf;

      if ((newbuf = Yap_AllocAtomSpace(new_max_size*sizeof(char))) == NULL) {
	format_buf_size = -1;
	Yap_Error(SYSTEM_ERROR, TermNil, "YAP could not grow heap for format/2");
	return(EOF);
      }
#if HAVE_MEMMOVE
      memmove((void *)newbuf, (void *)format_base, (size_t)((format_ptr-format_base)*sizeof(char)));
#else
      {
	Int n = format_ptr-format_base;
	char *to = newbuf;
	char *from = format_base;
	while (n-- >= 0) {
	  *to++ = *from++;
	}
      }
#endif
      Yap_FreeAtomSpace(format_base);
      format_ptr = newbuf+(format_ptr-format_base);
      format_base = newbuf;
      format_max = newbuf+new_max_size;
      format_buf_size = new_max_size;
      if (ActiveSignals & YAP_CDOVF_SIGNAL) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(FATAL_ERROR, TermNil, "YAP failed to grow heap at format");
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
  nfillers = pad_max-pad_entries;
  if (nfillers == 0) {
    /* OK, just pad with spaces */
    while (nchars--) {
      *format_ptr++ = ' ';
    }    
    return;
  }
  fill_space = nchars/nfillers;
  lfill_space = nchars%nfillers;

  if (fill_space) {
    pads *padi = pad_max;

    while (padi > pad_entries) {
      char *start_pos;
      int n, i;
      padi--;
      start_pos = format_base+padi->pos;
      n = format_ptr-start_pos;

#if HAVE_MEMMOVE
      memmove((void *)(start_pos+fill_space), (void *)start_pos, (size_t)(n*sizeof(char)));
#else
      {
	char *to = start_pos+(fill_space+n);
	char *from = format_ptr;

	while (n-- > 0) {
	  *--to = *--from;
	}
      }
#endif
      format_ptr += fill_space;
      for (i = 0; i < fill_space; i++) {
	*start_pos++ = padi->pad;
      }
    }
  }
  while (lfill_space--) {
    *format_ptr++ = pad_max[-1].pad;
  }
}

static void
format_print_str (Int sno, Int size, Term args)
{
  Term arghd;
  Int always_flag = !size;
  while (always_flag || size > 0)
    {
      if (IsVarTerm(args)) {
	format_error = TRUE;
	Yap_Error(INSTANTIATION_ERROR, args, "format/2");
	return;
      } else if (args == TermNil)
	break;
      else if (!IsPairTerm (args)) {
	format_error = TRUE;
	Yap_Error(TYPE_ERROR_LIST, args, "format/2");
	return;
      }
      arghd = HeadOfTerm (args);
      args = TailOfTerm (args);
      if (IsVarTerm(arghd)) {
	format_error = TRUE;
	Yap_Error(INSTANTIATION_ERROR, arghd, "format/2");
	return;
      } else if (!IsIntTerm (arghd)) {
	format_error = TRUE;
	Yap_Error(TYPE_ERROR_LIST, arghd, "format/2");
	return;
      }
      format_putc(sno, (int) IntOfTerm (arghd));
      size--;
    }
}

static Int
format(Term tail, Term args, int sno)
{
  char tmp1[256], tmp2[256], *ptr;
  Term head, arghd;
  int ch;
  Int int2, i;
  int column_boundary = 0;
  Int size_args, arg_size;
  Float float_tmp;
  
  if (IsVarTerm(tail)) {
    Yap_Error(INSTANTIATION_ERROR,tail,"format/2");
    return(FALSE);
  } else if (!IsPairTerm (tail)) {
    Yap_Error(TYPE_ERROR_LIST,tail,"format/2");
    return(FALSE);
  }
  head = HeadOfTerm (tail);
  tail = TailOfTerm (tail);
  if (IsVarTerm (args) || !IsPairTerm (args))
    args = MkPairTerm (args, TermNil);
  format_base = format_ptr = Yap_AllocAtomSpace(FORMAT_MAX_SIZE*sizeof(char));
  format_max = format_base+FORMAT_MAX_SIZE;
  if (format_ptr == NULL) {
    Yap_Error(INSTANTIATION_ERROR,tail,"format/2");
    return(FALSE);
  }  
  format_buf_size = FORMAT_MAX_SIZE;
  format_error = FALSE;
  while (!IsVarTerm (head) && IsIntTerm (head))
    {
      if (format_buf_size == -1) {
	Yap_FreeAtomSpace(format_base);
	return(FALSE);
      }
      ch = IntOfTerm (head);
      if (ch == '~')
	{
	  size_args = FALSE;
	  arg_size = 0;
	  ptr = tmp1;
	  *ptr++ = '%';
	  if (IsVarTerm (tail = Deref (tail)) ) {
	    Yap_FreeAtomSpace(format_base);
	    Yap_Error(INSTANTIATION_ERROR,tail,"format/2");
	    return(FALSE);
	  } else if (!IsPairTerm (tail)) {
	    Yap_FreeAtomSpace(format_base);
	    Yap_Error(TYPE_ERROR_LIST,tail,"format/2");
	    return(FALSE);
	  }
	  head = HeadOfTerm (tail);
	  tail = TailOfTerm (tail);
	  if (IsVarTerm (head)) {
	    Yap_FreeAtomSpace(format_base);
	    Yap_Error(INSTANTIATION_ERROR,tail,"format/2");
	    return(FALSE);
	  } else  if ( !IsIntTerm (head)) {
	    Yap_FreeAtomSpace(format_base);
	    Yap_Error(TYPE_ERROR_INTEGER,tail,"format/2");
	    return(FALSE);
	  } else
	    ch = IntOfTerm (head);
	  if (ch == '*')
	    {
	      size_args = TRUE;
	      arg_size = GetArgSizeFromThirdArg (&ptr, &args);
	      if (format_error) {
		Yap_FreeAtomSpace(format_base);
		return(FALSE);
	      }
	      if (IsVarTerm (tail = Deref (tail)) ) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,tail,"format/2");
		return(FALSE);
	      } else if (!IsPairTerm (tail)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,tail,"format/2");
		return(FALSE);
	      }
	      head = HeadOfTerm (tail);
	      tail = TailOfTerm (tail);
	      if (IsVarTerm (head)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,tail,"format/2");
		return(FALSE);
	      } else  if ( !IsIntTerm (head)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_INTEGER,tail,"format/2");
		return(FALSE);
	      } else
		ch = IntOfTerm (head);
	    }
	  else if (ch >= '0' && ch <= '9')
	    {
	      arg_size = ch - '0';
	      size_args = TRUE;
	      ch = GetArgSizeFromChars (&ptr, &arg_size, &tail);
	      if (format_error) {
		Yap_FreeAtomSpace(format_base);
		return (FALSE);
	      }
	    }
	  else if (ch == '`')
	    {
	      size_args = TRUE;
	      arg_size = GetArgSizeFromChar(&tail);
	      if (format_error) {
		Yap_FreeAtomSpace(format_base);
		return(FALSE);
	      }
	      ch = 't';
	    }
	  switch (ch)
	    {
	    case 'a':
	      if (size_args) {
		Yap_FreeAtomSpace(format_base);
		return (FALSE);
	      }
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~a format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~a format/2");
		return(FALSE);
	      }
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      if (IsVarTerm (arghd)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,arghd,"~a in format/2");
		return(FALSE);		
	      } else if (!IsAtomTerm (arghd)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_ATOM,arghd,"~a in format/2");
		return(FALSE);
	      }
	      Yap_plwrite (arghd, format_putc, Handle_vars_f|To_heap_f);
	      break;
	    case 'c':
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~c in format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~c in format/2");
		return(FALSE);
	      }
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      if (IsVarTerm (arghd)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,arghd,"~c in format/2");
		return(FALSE);		
	      } else if (!IsIntTerm (arghd)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_ATOM,arghd,"~a in format/2");
		return(FALSE);
	      } else int2= IntOfTerm(arghd);
	      if (!size_args)
		arg_size = 1;
	      for (i = 0; i < arg_size; i++)
		format_putc(sno, int2);
	      break;
	    case 'e':
	    case 'E':
	    case 'f':
	    case 'g':
	    case 'G':
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~%d in format/2", ch);
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~%d in format/2", ch);
		return(FALSE);
	      }
	      if (arg_size == 0 || arg_size > 6)
		arg_size = 6;
	      *ptr++ = '.';
	      *ptr++ = '0' + arg_size;
	      *ptr++ = ch;
	      *ptr = 0;
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      if (IsVarTerm(arghd)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,arghd,"~%c in format/2", ch);
		return(FALSE);		
	      } else if (!IsNumTerm (arghd)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_FLOAT,arghd,"~%c in format/2", ch);
		return(FALSE);
	      }
	      if (IsIntegerTerm(arghd)) {
		  float_tmp = IntegerOfTerm(arghd);
#ifdef USE_GMP
	      } else if (IsBigIntTerm(arghd)) {
		  float_tmp = mpz_get_d(Yap_BigIntOfTerm(arghd));
#endif
              } else {
		float_tmp = FloatOfTerm (arghd);
	      }
	      sprintf (tmp2, tmp1, float_tmp);
	      ptr = tmp2;
	      while ((ch = *ptr++) != 0)
		format_putc(sno, ch);
	      break;
	    case 'd':
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~d format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~d format/2");
		return(FALSE);
	      }
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      if (IsVarTerm (arghd)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,arghd,"~d in format/2");
		return(FALSE);		
	      } else if (IsIntTerm (arghd)) {
		int2 = IntOfTerm (arghd);
	      } else if (IsLongIntTerm (arghd)) {
		int2 = LongIntOfTerm(arghd);
	      } else {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_INTEGER,arghd,"~d in format/2");
		return(FALSE);
	      }
	      if (!arg_size) {
		Yap_plwrite (arghd, format_putc, Handle_vars_f|To_heap_f);
	      } else {
		Int siz;
		/*
		 * The guys at Quintus have probably
		 * read too much Cobol! 
		 */
		if (int2 < 0) {
		  int2 = -int2;
		  format_putc(sno, (int) '-');
		}
#if SHORT_INTS
		sprintf (tmp2, "%ld", int2);
#else
		sprintf (tmp2, "%d", int2);
#endif
		siz = strlen (tmp2);
		{
		  char *ptr = tmp2;
		  if (siz <= arg_size)
		    format_putc(sno, (int) '0');
		  else
		    while (siz > arg_size)
		      format_putc(sno, (int) *ptr++), --siz;
		  format_putc(sno, (int) '.');
		  while (siz < arg_size)
		    format_putc(sno, (int) '0'), --arg_size;
		  while (*ptr)
		    format_putc(sno, (int) (*ptr++));
		}
	      }
	      break;
	    case 'D':
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~D in format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~D in format/2");
		return(FALSE);
	      }
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      if (IsVarTerm (arghd)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,arghd,"~D in format/2");
		return(FALSE);		
	      } else if (IsIntTerm (arghd)) {
		int2 = IntOfTerm (arghd);
	      } else if (IsLongIntTerm (arghd)) {
		int2 = LongIntOfTerm(arghd);
	      } else {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_INTEGER,arghd,"~D in format/2");
		return(FALSE);
	      }
	      {
		Int siz, lsiz;
		char *ptr = tmp2;

		if (int2 < 0)
		  {
		    int2 = -int2;
		    format_putc(sno, (int) '-');
		  }
		/*
		 * The guys at Quintus have probably
		 * read too much Cobol! 
		 */
#if SHORT_INTS
		sprintf (tmp2, "%ld", int2);
#else
		sprintf (tmp2, "%d", int2);
#endif
		siz = strlen (tmp2);
		if ((lsiz = siz - arg_size) <= 0)
		  format_putc(sno, (int) '0');
		else
		  {
		    Int imod = lsiz % 3;
		    int output_done = FALSE;

		    for (i = 0; i < lsiz; i++)
		      {
			if (imod-- == 0)
			  {
			    if (output_done)
			      format_putc(sno, (int) ',');
			    imod = 2;
			  }
			format_putc(sno, (int) (*ptr++));
			output_done = TRUE;
		      }
		  }
		if (arg_size > 0)
		  format_putc(sno, (int) '.');
		while (lsiz < 0)
		  format_putc(sno, (int) '0'), ++lsiz;
		while (*ptr)
		  format_putc(sno, (int) (*ptr++));
	      }
	      break;
	    case 'r':
	      {
		Int radix = 8;
		ptr = tmp2;
		if (size_args)
		  radix = arg_size;
		if (IsVarTerm (args)) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(INSTANTIATION_ERROR,args,"~r in format/2");
		  return(FALSE);		
		} else if (!IsPairTerm (args)) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(TYPE_ERROR_LIST,args,"~r in format/2");
		  return(FALSE);
		}
		if (radix > 36 || radix < 2) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(DOMAIN_ERROR_RADIX,MkIntTerm(radix),"~r in format/2");
		  return(FALSE);
		}
		arghd = HeadOfTerm (args);
		args = TailOfTerm (args);
		if (IsVarTerm (arghd)) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(INSTANTIATION_ERROR,arghd,"~r in format/2");
		  return(FALSE);		
		} else if (IsIntTerm (arghd)) {
		  int2 = IntOfTerm (arghd);
		} else if (IsLongIntTerm (arghd)) {
		int2 = LongIntOfTerm(arghd);
		} else {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(TYPE_ERROR_INTEGER,arghd,"~r in format/2");
		  return(FALSE);
		}
		if (int2 < 0)
		  {
		    int2 *= -1;
		    format_putc(sno, (int) '-');
		  }
		else if (int2 == 0)
		  format_putc(sno, (int) '0');
		while (int2 != 0)
		  {
		    Int numb = int2 % radix;
		    if (numb >= 10)
		      numb += 'a' - 10;
		    else
		      numb += '0';
		    *ptr++ = numb;
		    int2 = int2 / radix;
		  }
		while (--ptr >= tmp2)
		  format_putc(sno, (int) *ptr);
	      }
	      break;
	    case 'R':
	      {
		Int radix = 8;
		ptr = tmp2;
		if (size_args)
		  radix = arg_size;
		if (IsVarTerm (args)) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(INSTANTIATION_ERROR,args,"~R in format/2");
		  return(FALSE);		
		} else if (!IsPairTerm (args)) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(TYPE_ERROR_LIST,args,"~R in format/2");
		  return(FALSE);
		}
		if (radix > 36 || radix < 2) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(DOMAIN_ERROR_RADIX,MkIntTerm(radix),"~R in format/2");
		  return(FALSE);
		}
		arghd = HeadOfTerm (args);
		args = TailOfTerm (args);
		if (IsVarTerm (arghd)) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(INSTANTIATION_ERROR,arghd,"~R in format/2");
		  return(FALSE);		
		} else if (IsIntTerm (arghd)) {
		  int2 = IntOfTerm (arghd);
		} else if (IsLongIntTerm (arghd)) {
		  int2 = LongIntOfTerm(arghd);
		} else {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(TYPE_ERROR_INTEGER,arghd,"~R in format/2");
		  return(FALSE);
		}
		if (int2 < 0)
		  {
		    int2 *= -1;
		    format_putc(sno, (int) '-');
		  }
		else if (int2 == 0)
		  format_putc(sno, (int) '0');
		while (int2 != 0)
		  {
		    Int numb = int2 % radix;
		    if (numb >= 10)
		      numb += 'A' - 10;
		    else
		      numb += '0';
		    *ptr++ = numb;
		    int2 = int2 / radix;
		  }
		while (--ptr >= tmp2)
		  format_putc(sno, (int) *ptr);
	      }
	      break;
	    case 's':
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~s in format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~s in format/2");
		return(FALSE);
	      }
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      if (IsVarTerm (arghd)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,arghd,"~s in format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (arghd) && arghd != TermNil) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,arghd,"~s in format/2");
		return(FALSE);
	      }
	      format_print_str (sno, arg_size, arghd);
	      if (format_error) {
		Yap_FreeAtomSpace(format_base);
		return(FALSE);
	      }
	      break;
	    case 'i':
	      if (size_args) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(DOMAIN_ERROR_NOT_ZERO,MkIntTerm(size_args),
		      "~i in format/2");
		return(FALSE);
	      }
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~i in format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~i in format/2");
		return(FALSE);
	      }
	      args = TailOfTerm (args);
	      break;
	    case 'k':
	      if (size_args) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(DOMAIN_ERROR_NOT_ZERO,MkIntTerm(size_args),
		      "~k in format/2");
		return(FALSE);
	      }
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~k in format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~k in format/2");
		return(FALSE);
	      }
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      Yap_plwrite (arghd, format_putc, Quote_illegal_f|Ignore_ops_f|To_heap_f );
	      break;
	    case 'p':
	      if (size_args) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(DOMAIN_ERROR_NOT_ZERO,MkIntTerm(size_args),
		      "~p in format/2");
		return(FALSE);
	      }
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~p in format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~p in format/2");
		return(FALSE);
	      }
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      *--ASP = MkIntTerm(0);
	      { 
		long sl = Yap_InitSlot(args);
		Yap_plwrite(arghd, format_putc, Handle_vars_f|Use_portray_f|To_heap_f);
		args = Yap_GetFromSlot(sl);
		Yap_RecoverSlots(1);
	      }
	      if (EX != 0L) {
		Term ball = EX;
		EX = 0L;
		Yap_FreeAtomSpace(format_base);
		Yap_JumpToEnv(ball);
		return(FALSE);
	      }
	      break;
	    case 'q':
	      if (size_args) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(DOMAIN_ERROR_NOT_ZERO,MkIntTerm(size_args),
		      "~q in format/2");
		return(FALSE);
	      }
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~q in format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~q in format/2");
		return(FALSE);
	      }
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      Yap_plwrite (arghd, format_putc, Handle_vars_f|Quote_illegal_f|To_heap_f);
	      break;
	    case 'w':
	      if (size_args) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(DOMAIN_ERROR_NOT_ZERO,MkIntTerm(size_args),
		      "bad arguments for ~w in format/2");
		return(FALSE);
	      }
	      if (IsVarTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,args,"~w in format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (args)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,args,"~w in format/2");
		return(FALSE);
	      }
	      arghd = HeadOfTerm (args);
	      args = TailOfTerm (args);
	      Yap_plwrite (arghd, format_putc, Handle_vars_f|To_heap_f);
	      break;
	    case '~':
	      if (size_args) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(DOMAIN_ERROR_NOT_ZERO,MkIntTerm(size_args),
		      "~~ in format/2");
		return(FALSE);
	      }
	      format_putc(sno, (int) '~');
	      break;
	    case 'n':
	      if (!size_args)
		arg_size = 1;
	      for (i = 0; i < arg_size; i++)
		format_putc(sno, (int) '\n');
	      column_boundary = 0;
	      pad_max = pad_entries;
	      break;
	    case 'N':
	      if (!size_args) {
		arg_size = 1;
	      }
	      if (Stream[sno].linepos != 0)
		{
		  format_putc(sno, (int) '\n');
		  column_boundary = 0;
		  pad_max = pad_entries;
		}
	      if (arg_size > 1) {
		for (i = 1; i < arg_size; i++)
		  format_putc(sno, (int) '\n');
		column_boundary = 0;
		pad_max = pad_entries;
	      }
	      break;
	      /* padding */
	    case '|':
	      if (size_args) {
		fill_pads(arg_size-(format_ptr-format_base));
	      }
	      pad_max = pad_entries;
	      column_boundary = arg_size;
	      break;
	    case '+':
	      if (size_args) {
		fill_pads((arg_size+column_boundary)-(format_ptr-format_base));
	      } else {
		fill_pads(8);
	      }
	      pad_max = pad_entries;
	      column_boundary = arg_size+column_boundary;
	      break;
	    case 't':
	      if (!size_args)
		arg_size = ' ';
	      pad_max->pad = arg_size;
	      pad_max->pos = format_ptr-format_base;
	      pad_max++;
	      break;
#if DEBUG
	    case 'T':
	      {
		Int radix = 16;
		unsigned *uint_ptr;

		ptr = tmp2;
		if (size_args)
		  radix = arg_size;
		if (IsVarTerm (args)) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(INSTANTIATION_ERROR,args,"~T in format/2");
		  return(FALSE);		
		} else if (!IsPairTerm (args)) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(TYPE_ERROR_LIST,args,"~T in format/2");
		  return(FALSE);
		}
		if (radix > 36 || radix < 2) {
		  Yap_FreeAtomSpace(format_base);
		  Yap_Error(DOMAIN_ERROR_RADIX,MkIntTerm(radix),"~T in format/2");
		  return(FALSE);
		}
		arghd = HeadOfTerm (args);
		args = TailOfTerm (args);

		uint_ptr = (unsigned *)&arghd;
		for (i = 0; i < sizeof (arghd) / sizeof (unsigned); ++i) {
		  if (uint_ptr[i] == 0)
		    format_putc(sno, (int) '0');
		  while (uint_ptr[i] != 0) {
		    Int numb = uint_ptr[i] % radix;
		    if (numb >= 10)
		      numb += 'a' - 10;
		    else
		      numb += '0';
		    *ptr++ = numb;
		    uint_ptr[i] = uint_ptr[i] / radix;
		  }
		  while (--ptr >= tmp2)
		    format_putc(sno, (int) *ptr);
		}
	      }
	      break;
#endif /* DEBUG */
	    default:
	      Yap_FreeAtomSpace(format_base);
	      return (FALSE);
	    }
	}
      else if (ch == '\\')
	{
	  if (HeadOfTerm (tail) == MkIntTerm ('c'))
	    {
	      tail = TailOfTerm (tail);
	      if (IsVarTerm (tail)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(INSTANTIATION_ERROR,tail,"format/2");
		return(FALSE);		
	      } else if (!IsPairTerm (tail)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(TYPE_ERROR_LIST,tail,"format/2");
		return(FALSE);
	      }
	      head = HeadOfTerm (tail);
	      tail = TailOfTerm (tail);
	      if (head != MkIntTerm (10)) {
		Yap_FreeAtomSpace(format_base);
		Yap_Error(DOMAIN_ERROR_NOT_NL,head,
		      "format/2");
		return(FALSE);
	      }
	    }
	  else
	    format_putc(sno, (int) '\\');
	}
      else
	format_putc(sno, ch);
      if (IsVarTerm (tail) || !IsPairTerm (tail)) {
	for (ptr = format_base; ptr < format_ptr; ptr++) {
	  Stream[sno].stream_putc(sno, *ptr);
	}
	Yap_FreeAtomSpace(format_base);
	return(TRUE);
      }
      head = HeadOfTerm (tail);
      tail = TailOfTerm (tail);
    }
  if (format_buf_size == -1) {
    Yap_FreeAtomSpace(format_base);
    return(FALSE);
  }
  for (ptr = format_base; ptr < format_ptr; ptr++) {
    Stream[sno].stream_putc(sno, *ptr);
  }
  Yap_FreeAtomSpace(format_base);
  return (TRUE);
}

static Int
p_format(void)
{				/* '$format'(Control,Args)               */
  return(format(Deref(ARG1),Deref(ARG2), Yap_c_output_stream));
}


static Int
p_format2(void)
{				/* '$format'(Stream,Control,Args)          */
  int old_c_stream = Yap_c_output_stream;
  Int out;

  /* needs to change Yap_c_output_stream for write */
  Yap_c_output_stream = CheckStream (ARG1, Output_Stream_f, "format/3");
  if (Yap_c_output_stream == -1) {
    Yap_c_output_stream = old_c_stream;  
    return(FALSE);
  }
  out = format(Deref(ARG2),Deref(ARG3),Yap_c_output_stream);
  Yap_c_output_stream = old_c_stream;  
  return(out);
}


static Int
p_skip (void)
{				/* '$skip'(Stream,N)                     */
  int sno = CheckStream (ARG1, Input_Stream_f, "skip/2");
  Int n = IntOfTerm (Deref (ARG2)), ch;

  if (sno < 0)
    return (FALSE);
  if (n < 0 || n > 127)
    return (FALSE);
  while ((ch = Stream[sno].stream_getc(sno)) != n && ch != -1);
  return (TRUE);
}

static Int
p_flush (void)
{				/* flush_output(Stream)          */
  int sno = CheckStream (ARG1, Output_Stream_f, "flush_output/1");
  if (sno < 0)
    return (FALSE);
  yap_fflush (sno);
  return (TRUE);
}

static Int
p_flush_all_streams (void)
{				/* $flush_all_streams          */
#if BROKEN_FFLUSH_NULL
  int i;
  for (i = 0; i < MaxStreams; ++i)
    yap_fflush (i);
#else
  fflush (NULL);
#endif
  
  return (TRUE);
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
    return(FALSE);
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
    if (t2 == MkAtomTerm(Yap_LookupAtom("off"))) {
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
  if (!IsVarTerm (t1) && !IsIntTerm (t1))
    return (FALSE);
  if (!IsVarTerm (t2) && !IsIntTerm (t2))
    return (FALSE);
  if (IsVarTerm (t1))
    {
      Term t = MkIntTerm (max_depth);
      if (!Yap_unify_constant(ARG1, t))
	return (FALSE);
    }
  else
    max_depth = IntOfTerm (t1);
  if (IsVarTerm (ARG2))
    {
      Term t = MkIntTerm (max_list);
      if (!Yap_unify_constant (ARG2, t))
	return (FALSE);
    }
  else
    max_list = IntOfTerm (t2);
  return (TRUE);
}

static Int
p_change_type_of_char (void)
{				/* change_type_of_char(+char,+type)      */
  Term t1 = Deref (ARG1);
  Term t2 = Deref (ARG2);
  if (!IsVarTerm (t1) && !IsIntTerm (t1))
    return (FALSE);
  if (!IsVarTerm(t2) && !IsIntTerm(t2))
    return (FALSE);
  Yap_chtype[IntOfTerm(t1)] = IntOfTerm(t2);
  return (TRUE);
}

static Int
p_type_of_char (void)
{				/* type_of_char(+char,-type)      */
  Term t;

  Term t1 = Deref (ARG1);
  if (!IsVarTerm (t1) && !IsIntTerm (t1))
    return (FALSE);
  t = MkIntTerm(Yap_chtype[IntOfTerm (t1)]);
  return (Yap_unify(t,ARG2));
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
	Stream[i].stream_getc_for_read = ISOGetc;
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
      Stream[i].stream_getc_for_read = Stream[i].stream_getc;
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
	Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	return(FALSE);
      }
    }
    if (yap_flags[CHAR_CONVERSION_FLAG] != 0) {
      if (p_force_char_conversion() == FALSE)
	return(FALSE);
    }
    for (i = 0; i < NUMBER_OF_CHARS; i++) 
      CharConversionTable2[i] = '\0';
  }
  /* just add the new entry */
  if (s0[0] == s1[0])
    CharConversionTable2[(int)s0[0]] = '\0';
  else
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

int
Yap_StreamToFileNo(Term t)
{
  int sno  =
    CheckStream(t, (Input_Stream_f|Output_Stream_f), "StreamToFileNo");
  if (Stream[sno].status & Pipe_Stream_f) {
#if _MSC_VER || defined(__MINGW32__) 
    return((int)(Stream[sno].u.pipe.hdl));
#else
    return(Stream[sno].u.pipe.fd);
#endif
#if USE_SOCKET
  } else if (Stream[sno].status & Socket_Stream_f) {
    return(Stream[sno].u.socket.fd);
#endif
  } else if (Stream[sno].status & (Null_Stream_f|InMemory_Stream_f)) {
    return(-1);
  } else {
    return(YP_fileno(Stream[sno].u.file.file));
  }
}

static Int
p_stream(void)
{
  Term in = Deref(ARG1);
  if (IsVarTerm(in))
    return(FALSE);
  if (IsAtomTerm(in))
    return(CheckAlias(AtomOfTerm(in)) >= 0);
  if (IsApplTerm(in))
    return(FunctorOfTerm(in) == FunctorStream);
  return(FALSE);
}

static Int
p_same_file(void) {
  char *f1 = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
  char *f2 = RepAtom(AtomOfTerm(Deref(ARG2)))->StrOfAE;
  if (strcmp(f1,f2) == 0)
    return(TRUE);
#if HAVE_LSTAT 
  {
    struct stat buf1, buf2;
    if (strcmp(f1,"user_input") == 0) {
      if (fstat(fileno(Stream[0].u.file.file), &buf1) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return(FALSE);
      }
    } else   if (strcmp(f1,"user_output") == 0) {
      if (fstat(fileno(Stream[1].u.file.file), &buf1) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return(FALSE);    
      }
    } else   if (strcmp(f1,"user_error") == 0) {
      if (fstat(fileno(Stream[2].u.file.file), &buf1) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return(FALSE);    
      }
    } else if (stat(f1, &buf1) == -1) {
      /* file does not exist, but was opened? Return -1 */
      return(FALSE);
    }
    if (strcmp(f2,"user_input") == 0) {
      if (fstat(fileno(Stream[0].u.file.file), &buf2) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return(FALSE);
      }
    } else   if (strcmp(f2,"user_output") == 0) {
      if (fstat(fileno(Stream[1].u.file.file), &buf2) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return(FALSE);    
      }
    } else   if (strcmp(f2,"user_error") == 0) {
      if (fstat(fileno(Stream[2].u.file.file), &buf2) == -1) {
	/* file does not exist, but was opened? Return -1 */
	return(FALSE);    
      }
    } else if (stat(f2, &buf2) == -1) {
      /* file does not exist, but was opened? Return -1 */
      return(FALSE);
    }
    return(buf1.st_ino == buf2.st_ino
#ifdef __LCC__
	   && memcmp((const void *)&(buf1.st_dev),(const void *)&(buf2.st_dev),sizeof(buf1.st_dev)) == 0
#else
	   && buf1.st_dev == buf2.st_dev
#endif
		  );
  }
#else
  return(FALSE);
#endif
}

void
Yap_InitBackIO (void)
{
  Yap_InitCPredBack ("$current_stream", 3, 1, init_cur_s, cont_cur_s, SafePredFlag|SyncPredFlag);
}


void
Yap_InitIOPreds(void)
{

  Yap_stdin = stdin;
  Yap_stdout = stdout;
  Yap_stderr = stderr;
  Stream = (StreamDesc *)Yap_AllocCodeSpace(sizeof(StreamDesc)*MaxStreams);
  /* here the Input/Output predicates */
  Yap_InitCPred ("$check_stream", 2, p_check_stream, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$check_stream", 1, p_check_if_stream, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$stream_flags", 2, p_stream_flags, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$close", 1, p_close, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("peek_mem_write_stream", 3, p_peek_mem_write_stream, SyncPredFlag);
  Yap_InitCPred ("flush_output", 1, p_flush, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$flush_all_streams", 0, p_flush_all_streams, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$get", 2, p_get, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$get0", 2, p_get0, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$get0_line_codes", 2, p_get0_line_codes, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$get_byte", 2, p_get_byte, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$open", 4, p_open, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$file_expansion", 2, p_file_expansion, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$open_null_stream", 1, p_open_null_stream, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$open_pipe_stream", 2, p_open_pipe_stream, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("open_mem_read_stream", 2, p_open_mem_read_stream, SyncPredFlag);
  Yap_InitCPred ("open_mem_write_stream", 1, p_open_mem_write_stream, SyncPredFlag);
  Yap_InitCPred ("$put", 2, p_put, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$put_byte", 2, p_put_byte, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$set_read_error_handler", 1, p_set_read_error_handler, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$get_read_error_handler", 1, p_get_read_error_handler, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$read", 5, p_read, SyncPredFlag);
  Yap_InitCPred ("$read", 6, p_read2, SyncPredFlag);
  Yap_InitCPred ("$set_input", 1, p_set_input, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$set_output", 1, p_set_output, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$skip", 2, p_skip, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$write", 2, p_write, SyncPredFlag);
  Yap_InitCPred ("$write", 3, p_write2, SyncPredFlag);
  Yap_InitCPred ("$format", 2, p_format, SyncPredFlag);
  Yap_InitCPred ("$format", 3, p_format2, SyncPredFlag);
  Yap_InitCPred ("$current_line_number", 2, p_cur_line_no, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$line_position", 2, p_line_position, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$character_count", 2, p_character_count, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$start_line", 1, p_startline, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$show_stream_flags", 2, p_show_stream_flags, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$show_stream_position", 2, p_show_stream_position, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$set_stream_position", 2, p_set_stream_position, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$user_file_name", 2, p_user_file_name, SafePredFlag|SyncPredFlag),
  Yap_InitCPred ("$file_name", 2, p_file_name, SafePredFlag|SyncPredFlag),
  Yap_InitCPred ("$past_eof", 1, p_past_eof, SafePredFlag|SyncPredFlag),
  Yap_InitCPred ("$peek", 2, p_peek, SafePredFlag|SyncPredFlag),
  Yap_InitCPred ("$peek_byte", 2, p_peek_byte, SafePredFlag|SyncPredFlag),
  Yap_InitCPred ("current_input", 1, p_current_input, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("current_output", 1, p_current_output, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("prompt", 1, p_setprompt, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$is_same_tty", 2, p_is_same_tty, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("prompt", 2, p_prompt, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("always_prompt_user", 0, p_always_prompt_user, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("write_depth", 2, p_write_depth, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$change_type_of_char", 2, p_change_type_of_char, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$type_of_char", 2, p_type_of_char, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("char_conversion", 2, p_char_conversion, SyncPredFlag);
  Yap_InitCPred ("$current_char_conversion", 2, p_current_char_conversion, SyncPredFlag);
  Yap_InitCPred ("$all_char_conversions", 1, p_all_char_conversions, SyncPredFlag);
  Yap_InitCPred ("$force_char_conversion", 0, p_force_char_conversion, SyncPredFlag);
  Yap_InitCPred ("$disable_char_conversion", 0, p_disable_char_conversion, SyncPredFlag);
  Yap_InitCPred ("$add_alias_to_stream", 2, p_add_alias_to_stream, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$change_alias_to_stream", 2, p_change_alias_to_stream, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$check_if_valid_new_alias", 1, p_check_if_valid_new_alias, TestPredFlag|SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$fetch_stream_alias", 2, p_fetch_stream_alias, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$stream", 1, p_stream, SafePredFlag|TestPredFlag),
#if HAVE_SELECT
  Yap_InitCPred ("stream_select", 3, p_stream_select, SafePredFlag|SyncPredFlag);
#endif
  Yap_InitCPred ("$same_file", 2, p_same_file, SafePredFlag|SyncPredFlag);

#if USE_SOCKET
  Yap_InitSockets ();
#endif
  InitPlIO ();
#if HAVE_LIBREADLINE
  InitReadline();
#endif
}
