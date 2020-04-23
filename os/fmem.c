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
    char *b = Malloc(n);
    strncpy(b, buf, n - 1);
    if (strlen(buf) > n - 1)
        b[15] = '\0';
    return b;
}


#if HAVE_FMEMOPEN

int format_synch(int sno, int sno0, format_info *fg) {
  const char *s;
  int n;
  if (sno != sno0) {
    fflush(GLOBAL_Stream[sno].file);
    n = ftell(GLOBAL_Stream[sno].file);
    s = GLOBAL_Stream[sno].nbuf;
    if (GLOBAL_Stream[sno0].vfs) {
      int ch;
      int (*f)() = GLOBAL_Stream[sno0].vfs->put_char;
      while ((ch = *s++)) {
        f(sno0, ch);
      }
    } else {
      fwrite(s, n, 1, GLOBAL_Stream[sno0].file);
    }
    rewind(GLOBAL_Stream[sno].file);
    fg->lstart = 0;
    fg->phys_start = 0;
    fg->gapi = 0;
  }
  Yap_flush(sno0);
  return sno;
}

bool fill_pads(int sno, int sno0, int total, format_info *fg USES_REGS)
// uses directly the buffer in the memory stream.
{
  int nfillers, fill_space, lfill_space, nchars;
  int (*f_putc)(int, int);
  const char *buf;
  int phys_end;

  f_putc = GLOBAL_Stream[sno0].stream_putc;
  if (fflush(GLOBAL_Stream[sno].file) == 0) {
    buf = GLOBAL_Stream[sno].nbuf;
    phys_end = ftell(GLOBAL_Stream[sno].file);
  } else
    return false;
  if (fg->gapi == 0) {
    fg->gap[0].phys = phys_end;
    fg->gap[0].filler = ' ';
    fg->gapi = 1;
  }
  nchars = total - GLOBAL_Stream[sno].linepos;
  if (nchars < 0)
    nchars = 0; /* ignore */
  nfillers = fg->gapi;
  fill_space = nchars / nfillers;
  lfill_space = nchars % nfillers;

  int i = fg->phys_start;
  gap_t *padi = fg->gap;
  while (i < phys_end) {
    if (i == padi->phys) {
      int j;
      for (j = 0; j < fill_space; j++)
        f_putc(sno0, padi->filler);
      padi++;
      /* last gap??*/
      if (padi - fg->gap == fg->gapi) {
        for (j = 0; j < fill_space; j++)
          f_putc(sno0, (padi - 1)->filler);
      }
    }
    f_putc(sno0, buf[i++]);
  }
  // final gap
  if (i == padi->phys) {
    int j;
    for (j = 0; j < fill_space + lfill_space; j++)
      f_putc(sno0, padi->filler);
  };

  rewind(GLOBAL_Stream[sno].file);
  Yap_flush(sno0);
  GLOBAL_Stream[sno].linecount = 1;
  GLOBAL_Stream[sno].linepos += nchars;
  GLOBAL_Stream[sno].charcount = 0;
  GLOBAL_Stream[sno].buf.on = false;
  fg->phys_start = 0;
  fg->lstart = GLOBAL_Stream[sno].linepos;
  fg->gapi = 0;
  return true;
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
  Yap_DefaultStreamOps(st);
  st->linecount = 0;
  st->linepos = st->charcount = 0;
  return true;
}


int Yap_open_buf_read_stream(const char *buf, size_t nchars,
                                 encoding_t *encp, memBufSource src, Atom fname,
                                 Term uname) {
  CACHE_REGS
  int sno;
  StreamDesc *st;
  FILE *f;
  encoding_t encoding;
  stream_flags_t flags;

  sno = GetFreeStreamD();
  if (sno < 0)
    return (PlIOError(RESOURCE_ERROR_MAX_STREAMS, TermNil,
                      "new stream not available for open_mem_read_stream/1"));
  st = GLOBAL_Stream + sno;
  if (encp)
    encoding = *encp;
  else
    encoding = LOCAL_encoding;
  // like any file stream.
  f = st->file = fmemopen((void *)buf, nchars, "r");
  st->vfs = NULL;
  flags = Input_Stream_f | InMemory_Stream_f | Seekable_Stream_f;
  Yap_initStream(sno, f, fname, "r", uname, encoding, flags, NULL);
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
  const char *buf;

  ti = Deref(ARG1);
  int l = push_text_stack();
  buf = Yap_TextTermToText(ti);
  if (!buf) {
    pop_text_stack(l);
    return false;
  }
  buf = pop_output_text_stack(l, buf);
  sno = Yap_open_buf_read_stream(buf, strlen(buf) + 1, &LOCAL_encoding,
                                 MEM_BUF_MALLOC, Yap_LookupAtom(Yap_StrPrefix((char *)buf,16)), TermNone);
  t = Yap_MkStream(sno);
  return Yap_unify(ARG2, t);
}

// open a buffer for writing, currently just ignores buf and nchars.

int Yap_open_buf_write_stream(encoding_t enc, memBufSource src) {
  CACHE_REGS
  int sno;
  StreamDesc *st;

  sno = GetFreeStreamD();
  if (sno < 0)
    return -1;

  st = GLOBAL_Stream + sno;
  st->status = Output_Stream_f | InMemory_Stream_f;
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->encoding = enc;
  st->vfs = NULL;
  st->buf.on = true;
  st->nbuf = NULL;
  st->status |= Seekable_Stream_f;
#if HAVE_OPEN_MEMSTREAM
  st->file = open_memstream(&st->nbuf, &st->nsize);
  // setbuf(st->file, NULL);
#else
  st->file = fmemopen((void *)st->nbuf, st->nsize, "w+");
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
FILE *f = GLOBAL_Stream[sno].file;
  if (fflush(f) < 0) {
    return NULL;
  }
  if (fseek(f, 0, SEEK_END) < 0) {
    return NULL;
  }
  size_t len = ftell(f);
  char *buf = malloc(len+1);
#if HAVE_OPEN_MEMSTREAM
  char *s = GLOBAL_Stream[sno].nbuf;
  memcpy(buf, s, len);
  // s[fseek(GLOBAL_Stream[sno].file, 0, SEEK_END)] = '\0';
#else
  fread(buf, len, 1, GLOBAL_Stream[sno].file);
#endif
  buf[len] = '\0';
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
      if (!Yap_gcl((ASP - HI) * sizeof(CELL), 3, ENV, Yap_gcP())) {
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
  free(ptr);
  return (Yap_unify(ARG3, AbsPair(HI)));
}

void Yap_MemOps(StreamDesc *st) {
  st->stream_putc = FilePutc;

  st->stream_getc = PlGetc;
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
  GLOBAL_Stream[sno].status = Free_Stream_f;
  return true;
}

void Yap_InitMems(void) {
  CACHE_REGS
  Yap_InitCPred("open_mem_read_stream", 2, open_mem_read_stream, SyncPredFlag);
  Yap_InitCPred("open_mem_write_stream", 1, open_mem_write_stream,
                SyncPredFlag);
  Yap_InitCPred("peek_mem_write_stream", 3, peek_mem_write_stream,
                SyncPredFlag);
}

#endif
