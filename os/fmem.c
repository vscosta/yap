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
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes the definition of a socket related IO.
 *
 */

#define _GNU_SOURCE


#include "format.h"
#include "sysbits.h"

#include "YapText.h"

 char *Yap_StrPrefix( const char *buf, size_t n) {
   CACHE_REGS
    char *b = Malloc(n);
    strncpy(b, buf, n - 1);
    if (strlen(buf) > n - 1)
        b[15] = '\0';
    return b;
}

int fill_pads(int sno, int sno0, int total, format_info *fg USES_REGS)
// uses directly the buffer in the memory stream.
{
  int nfillers, fill_space, lfill_space, nchars, len, j = fg->phys_start;
  int (*f_putc)(int, int);
   char *buf;

  f_putc = GLOBAL_Stream[sno0].stream_putc;
  len = 
  fseek(GLOBAL_Stream[sno].file, 0, SEEK_END);
  fflush(GLOBAL_Stream[sno].file);
  len = GLOBAL_Stream[sno].charcount;
  buf = GLOBAL_Stream[sno].nbuf;
  buf[len] = '\0';
  nchars =total-len;
  if (nchars < 0||fg->gapi==0) {
    nchars = 0; /* ignore */
    fg->gapi = 0;
    nfillers = 0;
  }
  if (fg->gapi  == 0){
    fg->gap[0].log = -1;
  } else {
    nfillers = fg->gapi;
  fill_space = nchars / nfillers;
  lfill_space = nchars % nfillers;

  }
  // printf("%d %d %d %s\n", total, len,fg->gap->log,buf);
  int i = 0;
  gap_t *padi = fg->gap;
  while (i < len ) {
    if (fg->gapi >padi-fg->gap && i == padi->log) {
      int k;
      for (k = 0; k < fill_space; k++) {
        f_putc(sno0, padi->filler);
	padi->log = -1;
	j++;
	//++;

      }
      padi++;
    } else {
      f_putc(sno0, buf[i++]);
      j++;
    }
   }
  //  final gap
  if (fg->gapi >padi-fg->gap){
    int k;
    for (k = 0; k < fill_space + lfill_space; k++) {
      f_putc(sno0, padi->filler);
    j++;
    }
  };
  Yap_flush(sno0);
  Yap_CloseMemoryStream( sno);
    sno = Yap_OpenBufWriteStream(PASS_REGS1);
  fg->lstart = 0;
  fg->phys_start = j;
  fg->gapi = 0;
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
  CACHE_REGS
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
  UNLOCK(st->streamlock);
  return sno;
}

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
  sno = Yap_open_buf_read_stream(NULL,buf, strlen(buf) + 1, &LOCAL_encoding,
                                 MEM_BUF_MALLOC, Yap_LookupAtom(Yap_StrPrefix((char *)buf,16)), TermNone);
  t = Yap_MkStream(sno);
  return Yap_unify(ARG2, t);
}

// open a buffer for writing, currently just ignores buf and nchars.

int Yap_open_buf_write_stream(encoding_t enc, memBufSource src) {
  int sno;
  StreamDesc *st;

  sno = GetFreeStreamD();
  if (sno < 0)
    return -1;

  st = GLOBAL_Stream + sno;
  st->status = Output_Stream_f | InMemory_Stream_f;
  st->linestart = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->encoding = enc;
  st->vfs = NULL;
  st->buf.on = true;
  st->nbuf = NULL;
  st->status |= (Seekable_Stream_f|CloseOnException_Stream_f);
  #if HAVE_OPEN_MEMSTREAM
  st->file = open_memstream(&st->nbuf, &st->nsize);
  #else
   if (st->nbuf == NULL)
     st->nbuf = malloc(32*K);
jj   st->file = fmemopen((void *)st->nbuf, st->nsize, "w+");
  #endif
  Yap_DefaultStreamOps(st);
  UNLOCK(st->streamlock);
  return sno;
}

int Yap_OpenBufWriteStream(USES_REGS1) {

  return Yap_open_buf_write_stream(
      GLOBAL_Stream[LOCAL_c_output_stream].encoding, 0);
}

static Int
open_mem_write_stream(USES_REGS1) /* $open_mem_write_stream(-Stream) */
{
  Term t;
  int sno;

  sno = Yap_OpenBufWriteStream(PASS_REGS1);
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
    CACHE_REGS
FILE *f = GLOBAL_Stream[sno].file;
  if (fflush(f) < 0) {
    return NULL;
  }
  
  size_t sz =  GLOBAL_Stream[sno].nsize+2;
  GLOBAL_Stream[sno].nsize='\0';
  sz = sz < 32? 32:sz;
#if HAVE_OPEN_MEMSTREAM
  char *buf = Malloc( sz );
  strcpy(buf,  GLOBAL_Stream[sno].nbuf );


#else
  size_t len;
  len = fseek(GLOBAL_Stream[sno].file, 0, SEEK_END);
  buf = malloc(len+1);
  fread(buf, len, 1, GLOBAL_Stream[sno].file);
  buf[len] = '\0';
#endif
  
  return buf;
}

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
      UNLOCK(GLOBAL_Stream[sno].streamlock);
      HR = HI;
      if (!Yap_dogc(PASS_REGS1)) {
        UNLOCK(GLOBAL_Stream[sno].streamlock);
        Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        return (FALSE);
      }
      LOCK(GLOBAL_Stream[sno].streamlock);
      goto restart;
    }
  }
  HR[-1] = tf;
  UNLOCK(GLOBAL_Stream[sno].streamlock);

    fclose(GLOBAL_Stream[sno].file);
    free(GLOBAL_Stream[sno].nbuf);
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
  char *s;
 if (sno==sno0)
   return sno;
 s = Yap_MemExportStreamPtr(sno);
    if (GLOBAL_Stream[sno0].vfs) {
      int ch;
      int (*f)() = GLOBAL_Stream[sno0].vfs->put_char;
      while ((ch = *s++)) {
        f(sno0, ch);
      }
    } else {
 int ch;
      int (*f)() = GLOBAL_Stream[sno0].stream_putc;
      while ((ch = *s++)) {
        f(sno0, ch);
      }
    }
    fclose(GLOBAL_Stream[sno].file);
    free(GLOBAL_Stream[sno].nbuf);
  #if HAVE_OPEN_MEMSTREAM
  GLOBAL_Stream[sno].file = open_memstream(&GLOBAL_Stream[sno].nbuf, &GLOBAL_Stream[sno].nsize);
  #else
   if (GLOBAL_Stream[sno].nbuf == NULL)
     GLOBAL_Stream[sno].nbuf = malloc(32*K);
   GLOBAL_Stream[sno].file = fmemopen((void *)GLOBAL_Stream[sno].nbuf, GLOBAL_Stream[sno]. nsize, "w+");
  #endif
   fg->lstart = 0;
    fg->phys_start = 0;
    fg->gapi = 0;

    Yap_flush(sno0);
 return sno;
}


bool Yap_CloseMemoryStream(int sno) {
                                                                         
  if ((GLOBAL_Stream[sno].status & Output_Stream_f) &&
      GLOBAL_Stream[sno].file) {
    fflush(GLOBAL_Stream[sno].file);
    fclose(GLOBAL_Stream[sno].file);
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

