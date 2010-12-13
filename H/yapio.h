/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2003	 *
*									 *
**************************************************************************
*									 *
* File:		yapio.h							 *
* Last rev:	22/1/03							 *
* mods:									 *
* comments:	Input/Output information				 *
*									 *
*************************************************************************/


#ifdef SIMICS
#undef HAVE_LIBREADLINE
#endif

#include <stdio.h>
#include <wchar.h>

#ifndef YAP_STDIO

#define YP_printf	printf
#define YP_putchar	putchar
#define YP_getc		getc
#define YP_fgetc	fgetc
#define YP_getchar	getchar
#define YP_fgets	fgets
#define YP_clearerr     clearerr
#define YP_feof		feof
#define YP_ferror	ferror
#if defined(_MSC_VER) || defined(__MINGW32__)
#define YP_fileno	_fileno
#else
#define YP_fileno	fileno
#endif
#define YP_fopen	fopen
#define YP_fclose	fclose
#define YP_ftell	ftell
#define YP_fseek	fseek
#define YP_setbuf	setbuf
#define YP_fputs	fputs
#define YP_ungetc	ungetc
#define YP_fdopen	fdopen
#define init_yp_stdio()

#if defined(_MSC_VER) || defined(__MINGW32__)
#define open _open
#define close _close
#define popen _popen
#define pclose _pclose
#define read _read
#define write _write
#define isatty _isatty
#define putenv(S) _putenv(S)
#define chdir(P) _chdir(P)
#define getcwd(B,S) _getcwd(B,S)
#endif

#define YP_FILE		FILE
extern YP_FILE *Yap_stdin;
extern YP_FILE *Yap_stdout;
extern YP_FILE *Yap_stderr;

int     STD_PROTO(YP_putc,(int, int));

#else

#ifdef putc
#undef putc
#undef getc
#undef putchar
#undef getchar
#undef stdin
#undef stdout
#undef stderr
#endif

#define printf	ERR_printf
#define fprintf ERR_fprintf
#define putchar	ERR_putchar
#define putc	ERR_putc
#define getc	ERR_getc
#define fgetc	ERR_fgetc
#define getchar	ERR_getchar
#define fgets	ERR_fgets
#define clearerr ERR_clearerr
#define feof	ERR_feof
#define ferror	ERR_ferror
#define fileno	ERR_fileno
#define fopen	ERR_fopen
#define fclose	ERR_fclose
#define fflush	ERR_fflush



/* flags for files in IOSTREAM struct */
#define _YP_IO_WRITE	1
#define _YP_IO_READ	2

#define _YP_IO_ERR	0x04
#define _YP_IO_EOF	0x08

#define _YP_IO_FILE	0x10
#define _YP_IO_SOCK	0x20


typedef struct IOSTREAM {
  int   check;
  int	fd;			/* file descriptor 	*/
  int 	flags;
  int   cnt;
  int	buflen;
  char  buf[2];
  char  *ptr;
  char  *base;
  int   (*close)(int fd);	/* close file		*/
  int	(*read)(int fd, char *b, int n); /* read bytes	*/
  int	(*write)(int fd, char *b, int n);/* write bytes */
} YP_FILE;

#define YP_stdin    &yp_iob[0]
#define YP_stdout   &yp_iob[1]
#define YP_stderr   &yp_iob[2]



#define YP_getc(f)	(--(f)->cnt < 0 ? YP_fillbuf(f) :  *((unsigned char *) ((f)->ptr++)))
#define YP_fgetc(f)	YP_fgetc(f)
#define YP_putc(c,f)	(--(f)->cnt < 0 ? YP_flushbuf(c,f) : (unsigned char) (*(f)->ptr++ = (char) c))
#define YP_putchar(cc)	YP_putc(cc,YP_stdout)
#define YP_getchar()	YP_getc(YP_stdin)

int YP_fillbuf(YP_FILE *f);
int YP_flushbuf(int c, YP_FILE *f);

int YP_printf(char *, ...);
int YP_fprintf(YP_FILE *, char *, ...);
char* YP_fgets(char *, int, YP_FILE *);
char* YP_gets(char *);
YP_FILE *YP_fopen(char *, char *);
int YP_fclose(YP_FILE *);
int YP_fileno(YP_FILE *);
int YP_fflush(YP_FILE *);
int YP_feof(YP_FILE *);
int YP_ftell(YP_FILE *);
int YP_fseek(YP_FILE *, int, int);
int YP_clearerr(YP_FILE *);
void init_yp_stdio(void);
int YP_fputs(char *s, YP_FILE *f);
int YP_puts(char *s);
int YP_setbuf(YP_FILE *f, char *buf);


#define YP_MAX_FILES 40

extern YP_FILE yp_iob[YP_MAX_FILES];

#endif /* YAP_STDIO */

typedef YP_FILE *YP_File;

typedef enum TokenKinds {
  Name_tok,
  Number_tok,
  Var_tok,
  String_tok,
  WString_tok,
  Ponctuation_tok,
  Error_tok,
  eot_tok
} tkinds;

typedef	 struct	TOKEN {
  enum TokenKinds Tok;
  Term TokInfo;
  int	TokPos;
  struct TOKEN *TokNext;
} TokEntry;

#define	Ord(X) ((enum TokenKinds) (X))

#define	NextToken GNextToken()

typedef	struct VARSTRUCT {
  Term VarAdr;
  CELL hv;
  struct VARSTRUCT *VarLeft, *VarRight;
  char VarRep[1];
} VarEntry;

/* Character types for tokenizer and write.c */

#define UC      1       /* Upper case */
#define UL      2       /* Underline */
#define LC      3       /* Lower case */
#define NU      4       /* digit */
#define	QT	5	/* single quote */
#define	DC	6	/* double quote */
#define SY      7       /* Symbol character */
#define SL      8       /* Solo character */
#define BK      9       /* Brackets & friends */
#define BS      10      /* Blank */
#define EF	11	/* End of File marker */
#define CC	12	/* comment char %	*/

#define EOFCHAR EOF


#if USE_SOCKET
/****************** defines for sockets *********************************/

typedef enum{        /* in YAP, sockets may be in one of 4 possible status */
  new_socket,
    server_socket,
    client_socket,
    server_session_socket,
    closed_socket
} socket_info;

typedef enum{       /* we accept two domains for the moment, IPV6 may follow */
      af_inet,      /* IPV4 */
      af_unix       /* or AF_FILE */
} socket_domain;

Term  STD_PROTO(Yap_InitSocketStream,(int, socket_info, socket_domain));
int   STD_PROTO(Yap_CheckStream,(Term, int, char *));
int   STD_PROTO(Yap_CheckSocketStream,(Term, char *));
socket_domain   STD_PROTO(Yap_GetSocketDomain,(int));
socket_info   STD_PROTO(Yap_GetSocketStatus,(int));
void  STD_PROTO(Yap_UpdateSocketStream,(int, socket_info, socket_domain));

/* routines in ypsocks.c */
Int STD_PROTO(Yap_CloseSocket,(int, socket_info, socket_domain));

#endif /* USE_SOCKET */

/* info on aliases */
typedef struct AliasDescS {
    Atom name;
    int alias_stream;
} * AliasDesc;

/************ SWI compatible support for different encodings ************/

#ifndef SIO_NL_POSIX
typedef enum {
  ENC_OCTET      = 0,
  ENC_ISO_LATIN1 = 1,
  ENC_ISO_ASCII  = 2,
  ENC_ISO_ANSI   = 4,
  ENC_ISO_UTF8   = 8,
  ENC_UNICODE_BE = 16,
  ENC_UNICODE_LE = 32,
  ENC_ISO_UTF32_BE = 64,
  ENC_ISO_UTF32_LE = 128
} encoding_t;
#endif

#define MAX_ISO_LATIN1 255

/****************** character definition table **************************/

#define NUMBER_OF_CHARS 256
extern char *Yap_chtype;

EXTERN inline int STD_PROTO(chtype,(Int));
int STD_PROTO(Yap_wide_chtype,(Int));

EXTERN inline int
chtype(Int ch)
{
  if (ch < NUMBER_OF_CHARS)
    return Yap_chtype[ch];
  return Yap_wide_chtype(ch);
}


/* parser stack, used to be AuxSp, now is ASP */
#define ParserAuxSp ScannerStack

/* routines in parser.c */
VarEntry STD_PROTO(*Yap_LookupVar,(char *));
Term STD_PROTO(Yap_VarNames,(VarEntry *,Term));

/* routines in scanner.c */
TokEntry STD_PROTO(*Yap_tokenizer,(int, Term *));
void     STD_PROTO(Yap_clean_tokenizer,(TokEntry *, VarEntry *, VarEntry *));
Term     STD_PROTO(Yap_scan_num,(int (*)(int)));
char	 STD_PROTO(*Yap_AllocScannerMemory,(unsigned int));

/* routines in iopreds.c */
FILE  *STD_PROTO(Yap_FileDescriptorFromStream,(Term));
Int   STD_PROTO(Yap_FirstLineInParse,(void));
int   STD_PROTO(Yap_CheckIOStream,(Term, char *));
#if  defined(YAPOR) || defined(THREADS)
void  STD_PROTO(Yap_LockStream,(int));
void  STD_PROTO(Yap_UnLockStream,(int));
#else
#define Yap_LockStream(X)
#define Yap_UnLockStream(X)
#endif
Int   STD_PROTO(Yap_GetStreamFd,(int));
void  STD_PROTO(Yap_CloseStreams,(int));
void  STD_PROTO(Yap_FlushStreams,(void));
void  STD_PROTO(Yap_CloseStream,(int));
int   STD_PROTO(Yap_PlGetchar,(void));
int   STD_PROTO(Yap_PlGetWchar,(void));
int   STD_PROTO(Yap_PlFGetchar,(void));
int   STD_PROTO(Yap_GetCharForSIGINT,(void));
Int   STD_PROTO(Yap_StreamToFileNo,(Term));
Term  STD_PROTO(Yap_OpenStream,(FILE *,char *,Term,int));
Term  STD_PROTO(Yap_StringToTerm,(char *,Term *));
Term  STD_PROTO(Yap_TermToString,(Term,char *,unsigned int,int));
int   STD_PROTO(Yap_GetFreeStreamD,(void));
int   STD_PROTO(Yap_GetFreeStreamDForReading,(void));

Term	STD_PROTO(Yap_WStringToList,(wchar_t *));
Term	STD_PROTO(Yap_WStringToListOfAtoms,(wchar_t *));
Atom	STD_PROTO(Yap_LookupWideAtom,(wchar_t *));

#define YAP_INPUT_STREAM	0x01
#define YAP_OUTPUT_STREAM	0x02
#define YAP_APPEND_STREAM	0x04
#define YAP_PIPE_STREAM 	0x08
#define YAP_TTY_STREAM	 	0x10
#define YAP_POPEN_STREAM	0x20
#define YAP_BINARY_STREAM	0x40
#define YAP_SEEKABLE_STREAM	0x80


#define	Quote_illegal_f		0x01
#define	Ignore_ops_f		0x02
#define	Handle_vars_f		0x04
#define	Use_portray_f		0x08
#define	To_heap_f	        0x10
#define	Unfold_cyclics_f        0x20
#define	Use_SWI_Stream_f        0x40

/* write.c */
void	STD_PROTO(Yap_plwrite,(Term,int (*)(int, wchar_t), int, int));

/* grow.c */
int  STD_PROTO(Yap_growheap_in_parser,   (tr_fr_ptr *, TokEntry **, VarEntry **));
int  STD_PROTO(Yap_growstack_in_parser,  (tr_fr_ptr *, TokEntry **, VarEntry **));
int  STD_PROTO(Yap_growtrail_in_parser,  (tr_fr_ptr *, TokEntry **, VarEntry **));



#ifdef HAVE_ERRNO_H
#include <errno.h>
#else
extern int errno;
#endif

#ifdef DEBUG
#if COROUTINING
extern int  Yap_Portray_delays;
#endif
#endif

EXTERN inline UInt STD_PROTO(HashFunction, (unsigned char *));
EXTERN inline UInt STD_PROTO(WideHashFunction, (wchar_t *));

EXTERN inline UInt
HashFunction(unsigned char *CHP)
{
  /* djb2 */
  UInt hash = 5381;
  UInt c;

  while ((c = *CHP++) != '\0') {
    /* hash = ((hash << 5) + hash) + c; hash * 33 + c */
    hash = hash * 33 ^ c;
  }
  return hash;
  /*
  UInt OUT=0, i = 1;
  while(*CHP != '\0') { OUT += (UInt)(*CHP++); }
  return OUT;
  */
}

EXTERN inline UInt
WideHashFunction(wchar_t *CHP)
{
  UInt hash = 5381;
  UInt c;

  while ((c = *CHP++) != '\0') {
    hash = hash * 33 ^ c;
  }
  return hash;
}

#define FAIL_ON_PARSER_ERROR      0
#define QUIET_ON_PARSER_ERROR     1
#define CONTINUE_ON_PARSER_ERROR  2
#define EXCEPTION_ON_PARSER_ERROR 3

#ifdef THREADS
#define    Yap_IOBotch    Yap_thread_gl[worker_id].io_botch
#define    Yap_tokptr     Yap_thread_gl[worker_id].tokptr
#define    Yap_toktide    Yap_thread_gl[worker_id].toktide
#define    Yap_VarTable   Yap_thread_gl[worker_id].var_table
#define    Yap_AnonVarTable   Yap_thread_gl[worker_id].anon_var_table
#define    Yap_eot_before_eof   Yap_thread_gl[worker_id].eot_before_eof
#define    Yap_FileNameBuf   Yap_thread_gl[worker_id].file_name_buf
#define    Yap_FileNameBuf2   Yap_thread_gl[worker_id].file_name_buf2
#else
extern jmp_buf Yap_IOBotch;

/*************** variables concerned with parsing   *********************/
extern TokEntry	*Yap_tokptr, *Yap_toktide;
extern VarEntry	*Yap_VarTable, *Yap_AnonVarTable;
extern int Yap_eot_before_eof;

extern char Yap_FileNameBuf[YAP_FILENAME_MAX], Yap_FileNameBuf2[YAP_FILENAME_MAX];

#endif

#ifdef DEBUG
extern YP_FILE *Yap_logfile;
#endif

#if USE_SOCKET
extern int Yap_sockets_io;
#endif

