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
 * File:		mem.c *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Input/Output C implemented predicates			 *
 *									 *
 *************************************************************************/

/*
 *
 * @file fmem.c
 * @brief includes the definition of a socket related IO.
 *
 */
/**
 * @defgroup MemoryIO Memory-Based Streams
 * @ingroup InputOutput
 * @{
 * Support for reading and writing on memory buffers, and therefore, on text.
 *
 * This code is used by open/3 and similar routines.
 *
 */


#define _GNU_SOURCE


#include "format.h"
#include "sysbits.h"

#include "YapText.h"

 Term Yap_memStreamToTerm(int output_stream, Functor f, Term inp) {
  CACHE_REGS
    const char *s = Yap_MemExportStreamPtr(output_stream);

    encoding_t enc = GLOBAL_Stream[output_stream].encoding;
    if (f == FunctorAtom) {
        return MkAtomTerm(Yap_LookupAtom(s));
    } else if (f == FunctorCodes) {
        return Yap_CharsToDiffListOfCodes(s, ArgOfTerm(2, inp), enc PASS_REGS);
    } else if (f == FunctorCodes1) {
        return Yap_CharsToListOfCodes(s, enc PASS_REGS);
    } else if (f == FunctorChars) {
        return Yap_CharsToDiffListOfAtoms(s, ArgOfTerm(2, inp), enc PASS_REGS);
    } else if (f == FunctorChars1) {
        return Yap_CharsToListOfAtoms(s, enc PASS_REGS);
    } else if (f == FunctorString1) {
        return Yap_CharsToString(s, enc PASS_REGS);
    }
    Yap_ThrowError(DOMAIN_ERROR_FORMAT_OUTPUT, inp, NULL);
    return 0L;
}
 
char *Yap_StrPrefix( const char *buf, size_t n) {
    char *b = malloc(n);
    strncpy(b, buf, n - 1);
    if (strlen(buf) > n - 1)
        b[15] = '\0';
    return b;
}

int fill_pads(int sno, int sno0, int total, format_info *fg USES_REGS)
// uses directly the buffer in the memory stream.
{
  int  fill_space, nchars, len, extra_fill;
  int (*f_putc)(int, int);
   char *buf;
  f_putc = GLOBAL_Stream[sno0].stream_putc;
  fflush(GLOBAL_Stream[sno].file);
  buf = GLOBAL_Stream[sno].nbuf;
  len = strlen(buf);
  nchars =total-len;
  if (nchars <= 0) {
    int i;
    fill_space=0;
    extra_fill=0;
   for (i = 0;i<len; i++) {
   f_putc(sno0, buf[i]); 
  }
 } else{
    if (fg->gapi==0) {
      fg->gapi=1;
      fg->gap[0].log = 0;
      fg->gap[0].filler = ' ';
    }

  fill_space = nchars / fg->gapi;
  extra_fill=nchars % fg->gapi;
  // printf("%d %d %d %s\n", total, len,fg->gap->log,buf);
  int i = 0, n=0, k=0;

  for (i = 0;i<len; i++,k++) {
    if (n<fg->gapi && i==fg->gap[n].log) {
      int extra  = (extra_fill>0?1:0),j;
      if (extra) extra_fill--;
    for (j = 0; j < fill_space+extra; j++) {
      f_putc(sno0, fg->gap[n].filler);
    k++;
    }
    n++;
    }
    f_putc(sno0, buf[i]); 
}
  if (k <total) {
    for (; k < total; k++) {
      f_putc(sno0, fg->gap[n].filler);
    }
  }
  }
  Yap_CloseMemoryStream( sno);
  sno = Yap_open_buf_write_stream(-1, LOCAL_encoding);
  fg->lstart = 0;
  fg->phys_start = 0;
  fg->gapi = 0;
  memset(fg->gap,0,sizeof(*fg->gap)*fg->gapi);
  return sno;
}

bool Yap_set_stream_to_buf(StreamDesc *st, const char *buf,
                           size_t nchars USES_REGS) {
  FILE *f;

  // like any file stream.
  st->file = f = fmemopen((char *)buf, nchars, "r");
  st->status = Input_Stream_f | Seekable_Stream_f | InMemory_Stream_f;
  st->vfs = NULL;
  st->buf.on = false;
  st->encoding = LOCAL_encoding;
  st->u.mem_string.buf = ( char *)buf;
  Yap_DefaultStreamOps(st);
  st-> linecount = 1;
  st->linestart = st->charcount = 0;
  return true;
}


int Yap_open_buf_read_stream(void *spt, const char *buf, size_t nchars,
                                 encoding_t *encp, memBufSource src, Atom fname,
                                 Term uname) {
  StreamDesc *st = spt;
  int sno;
  if (!buf) {
    return -1;
  }
  if (st == NULL) {  
    sno = GetFreeStreamD();
    if (sno < 0)
      return (PlIOError(RESOURCE_ERROR_MAX_STREAMS, TermNil,
			"new stream not available for open_mem_read_stream/1"));
  st = GLOBAL_Stream + sno;
  } else {
    sno = st-GLOBAL_Stream;
  }
       FILE* f =st->file = fmemopen((void *)buf, nchars, "r");
  st->vfs = NULL;
  st->u.mem_string.buf = (char *)buf;
  UInt flags = Input_Stream_f | InMemory_Stream_f | Seekable_Stream_f;
  Yap_initStream(sno, f, fname, "r", uname, st->encoding,  flags, NULL);
  // like any file stream.
  Yap_DefaultStreamOps(st);
  return sno;
}

/**
 * @pred open_mem_read_stream(+Text, -Stream)
 *
 * open a term with text (atom, string, list of codes, list_of_atoms) as a text buffer  .
 */
static Int
open_mem_read_stream(USES_REGS1) /* $open_mem_read_stream(+List,-Stream) */
{
  Term t, ti;
  int sno;
  char *buf;

  ti = Deref(ARG1);
  int l = push_text_stack();
  buf = Yap_TextTermToText(ti PASS_REGS);
  buf = Realloc(buf, 4096);
  if (!buf) {
    pop_text_stack(l);
    return false;
  }
  buf = pop_output_text_stack(l, buf);
  sno = Yap_open_buf_read_stream(NULL,buf, strlen(buf) , &LOCAL_encoding,
                                 MEM_BUF_MALLOC, Yap_LookupAtom(Yap_StrPrefix((char *)buf,16)), TermNone);
  t = Yap_MkStream(sno);
  return Yap_unify(ARG2, t);
}

// open a buffer for writing, currently just ignores buf and nchars.

int Yap_open_buf_write_stream(int sno, encoding_t enc) {
  StreamDesc *st;

  if (sno < 0) {
    sno = GetFreeStreamD();
    if (sno < 0)
      return -1;
    }

    st = GLOBAL_Stream + sno;
    st->status = Output_Stream_f | InMemory_Stream_f ;
  st->linestart = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->encoding = enc;
  st->vfs = NULL;
  st->buf.on = true;
  st->nbuf = NULL;
  st->status |= (Seekable_Stream_f|CloseOnException_Stream_f);
  #if HAVE_OPEN_MEMSTREAM
  st->nsize=0;
  st->file = open_memstream(&st->nbuf, &st->nsize);
  #else
  if (st->nbuf == NULL) {
     st->nbuf = malloc(32*K);
     st->nsize=32*K-1;
  }
   st->file = fmemopen((void *)st->nbuf, st->nsize, "w+");
  #endif
  Yap_DefaultStreamOps(st);
  return sno;
}

/**
 * @pred open_mem_write_stream(-Stream)
 *
 * open a system-allocated buffer for writing.
 */
static Int
open_mem_write_stream(USES_REGS1) /* $open_mem_write_stream(-Stream) */
{
  Term t;
  int sno;

  sno =  Yap_open_buf_write_stream(-1, LOCAL_encoding);
  if (sno == -1)
    return (PlIOError(SYSTEM_ERROR_INTERNAL, TermNil,
                      "new stream not available for open_mem_read_stream/1"));
  t = Yap_MkStream(sno);
  GLOBAL_Stream[sno].status |= InMemory_Stream_f;
  return (Yap_unify(ARG1, t));
}

/**
 * Yap_PeekMemwriteStream() shows the current buffer for a memory stream.
 *
 * @param sno, the in-memory stream
 *
 * @return temporary buffer, discarded by close and may be moved away
 * by other writes..
 */
char *Yap_MemExportStreamPtr(int sno) {
  FILE *f = GLOBAL_Stream[sno].file;
  fflush(f);
#if HAVE_OPEN_MEMSTREAM
  size_t sz =  GLOBAL_Stream[sno].nsize+1;
  sz = sz < 32? 32:sz;
  char *buf = malloc( sz );
  strcpy(buf,  GLOBAL_Stream[sno].nbuf );
#else
  size_t len, sz;
  len = strlen(buf);
  sz = len < 32? 32:sz;
  char *buf = malloc( sz );
  strcpy(buf,  GLOBAL_Stream[sno].nbuf );
#endif
  
  return buf;
}

/**
 * @pred peek_mem_write_stream(+Stream, -InpText, -FinalText)
 *
 * convert the buffer used by the memory-based stream to a difference of list of codes.
 */
static Int peek_mem_write_stream(
    USES_REGS1) { /* '$peek_mem_write_stream'(+GLOBAL_Stream,?S0,?S) */
  Int sno =
      Yap_CheckStream(ARG1, (Output_Stream_f | InMemory_Stream_f), "close/2");
  Term tf = ARG2;
  CELL *HI;
  char *ptr;
  int ch;
  
  if (sno < 0)
    return (FALSE);
  char *p = ptr = Yap_MemExportStreamPtr(sno);
 restart:
  HI = HR;
  while ((ch = *p++)) {
    HR[0] = MkIntTerm(ch);
    HR[1] = AbsPair(HR+2);
    HR += 2;
    if (HR + 1024 >= ASP) {
      HR = HI;
      if (!Yap_dogc(PASS_REGS1)) {
        Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        return (FALSE);
      }
      goto restart;
    }
  }
  HR[-1] = tf;
  #if HAVE_OPEN_MEMSTREAM
  GLOBAL_Stream[sno].file = open_memstream(&GLOBAL_Stream[sno].nbuf, &GLOBAL_Stream[sno].nsize);
  #else
   if (GLOBAL_Stream[sno].nbuf == NULL)
     GLOBAL_Stream[sno].nbuf = malloc(32*K);
   GLOBAL_Stream[sno].file = fmemopen((void *)GLOBAL_Stream[sno].nbuf, GLOBAL_Stream[sno]. nsize, "w+");
  #endif

  return (Yap_unify(ARG3, AbsPair(HI)));
}

void Yap_MemOps(StreamDesc *st) {
  st->stream_putc = FilePutc;

  st->stream_getc = PlGetc;
}

int format_synch(int sno, int sno0, format_info *fg) {
  CACHE_REGS
  char *s;
 if (sno==sno0)
   return sno;
 s = Yap_MemExportStreamPtr(sno);
    if (GLOBAL_Stream[sno0].vfs) {
      int ch;
      int (*f)(int,int) = GLOBAL_Stream[sno0].vfs->put_char;
      while ((ch = *s++)) {
        f(sno0, ch);
      }
    } else {
 int ch;
 int (*f)(int,int) = GLOBAL_Stream[sno0].stream_putc;
      while ((ch = *s++)) {
        f(sno0, ch);
      }
    }
    Yap_CloseMemoryStream(sno);
    return  Yap_open_buf_write_stream(-1, LOCAL_encoding);
 }


bool Yap_CloseMemoryStream(int sno) {
                                                                          
  if (GLOBAL_Stream[sno].status & Free_Stream_f)
    return true;
  if ((GLOBAL_Stream[sno].status & Output_Stream_f) &&
     GLOBAL_Stream[sno].file) {
    if (IsApplTerm(GLOBAL_Stream[sno].user_name)) {
      Term inp = GLOBAL_Stream[sno].user_name,
	rc = Yap_memStreamToTerm(sno, FunctorOfTerm(inp), inp);
      GLOBAL_Stream[sno].user_name = TermNil;
      if (!rc || !Yap_unify(rc, ArgOfTerm(1, inp))) {
	  return false;
	}
    fclose(GLOBAL_Stream[sno].file);
    }
	if (GLOBAL_Stream[sno].status & FreeOnClose_Stream_f)
            free(GLOBAL_Stream[sno].nbuf);
  } else {
    if (GLOBAL_Stream[sno].file)
      fclose(GLOBAL_Stream[sno].file);
    if (GLOBAL_Stream[sno].status & FreeOnClose_Stream_f)
      free(GLOBAL_Stream[sno].nbuf);
  }
  Yap_ReleaseStream(sno);
  return true;
}

void Yap_InitMems(void) {
  Yap_InitCPred("open_mem_read_stream", 2, open_mem_read_stream, SyncPredFlag);
  Yap_InitCPred("open_mem_write_stream", 1, open_mem_write_stream,
                SyncPredFlag);
  Yap_InitCPred("peek_mem_write_stream", 3, peek_mem_write_stream,
                SyncPredFlag);
}

