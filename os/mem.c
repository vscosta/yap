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

#include "sysbits.h"

#if !HAVE_FMEMOPEN || !defined(HAVE_FMEMOPEN)

#include "YapStreams.h"

#include "format.h"

int format_synch(int sno, int sno0, format_info *fg) {
  int (*f_putc)(int, int);
  const char *s;
  int n;
  if (sno == sno0) {
    return sno;
  }
  f_putc = GLOBAL_Stream[sno0].stream_putc;
  s = GLOBAL_Stream[sno].u.mem_string.buf;
  n = GLOBAL_Stream[sno].u.mem_string.pos;
  while (n--) {
    f_putc(sno0, *s++);
  }
  GLOBAL_Stream[sno].u.mem_string.pos = 0;
  GLOBAL_Stream[sno].linecount = 1;
  GLOBAL_Stream[sno].linepos = 0;
  GLOBAL_Stream[sno].charcount = 0;
  GLOBAL_Stream[sno].vfs = NULL;
  fg->lstart = 0;
  fg->phys_start = 0;
  fg->gapi = 0;
  return sno;
}

// uses directly the buffer in the memory stream.
bool fill_pads(int sno, int sno0, int total, format_info *fg USES_REGS) {
  int nfillers, fill_space, lfill_space, nchars;
  int (*f_putc)(int, int);
  const char *buf;
  int phys_end;

  f_putc = GLOBAL_Stream[sno0].stream_putc;
  buf = GLOBAL_Stream[sno].u.mem_string.buf;
  phys_end = GLOBAL_Stream[sno].u.mem_string.pos;
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

  GLOBAL_Stream[sno].u.mem_string.pos = 0;
  GLOBAL_Stream[sno].linecount = 1;
  GLOBAL_Stream[sno].linepos += nchars;
  GLOBAL_Stream[sno].charcount = 0;
  GLOBAL_Stream[sno].vfs = NULL;
  GLOBAL_Stream[sno].file = NULL;
  fg->phys_start = 0;
  fg->lstart = GLOBAL_Stream[sno].linepos;
  fg->gapi = 0;
  return true;
}

/* read from memory */
static int MemGetc(int sno) {
  register StreamDesc *s = &GLOBAL_Stream[sno];
  Int ch;
  int spos;

  spos = s->u.mem_string.pos;
  if (spos == s->u.mem_string.max_size) {
    return -1;
  } else {
    ch = s->u.mem_string.buf[spos];
    s->u.mem_string.pos = ++spos;
  }
  return ch;
}

/* peek from memory */
int Yap_MemPeekc(int sno) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  Int ch;
  int spos;

  spos = s->u.mem_string.pos;
  if (spos == s->u.mem_string.max_size) {
    return -1;
  } else {
    ch = s->u.mem_string.buf[spos];
  }
  return ch;
}

static int MemPutc(int, int);

/* static */
static int MemPutc(int sno, int ch) {
  StreamDesc *s = &GLOBAL_Stream[sno];
#if MAC || _MSC_VER
  if (ch == 10) {
    ch = '\n';
  }
#endif
  s->u.mem_string.buf[s->u.mem_string.pos++] = ch;
  if (s->u.mem_string.pos >= s->u.mem_string.max_size - 8) {
    int new_src;

    /* oops, we have reached an overflow */
    Int new_max_size = s->u.mem_string.max_size + Yap_page_size;
    char *newbuf;
    if ((newbuf = (ADDR)realloc(s->u.mem_string.buf,
                                new_max_size * sizeof(char))) != NULL) {
      new_src = MEM_BUF_MALLOC;
    } else {
      if (GLOBAL_Stream[sno].u.mem_string.error_handler) {
        CACHE_REGS
        LOCAL_Error_Size = new_max_size * sizeof(char);
        save_machine_regs();
        longjmp(*(jmp_buf *)GLOBAL_Stream[sno].u.mem_string.error_handler, 1);
      } else {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil,
                  "YAP could not grow heap for writing to string");
      }
      return -1;
    }
    s->u.mem_string.buf = newbuf;
    s->u.mem_string.max_size = new_max_size;
    s->u.mem_string.src = new_src;
  }
  count_output_char(ch, s);
  return ((int)ch);
}

bool Yap_set_stream_to_buf(StreamDesc *st, const char *buf,
                           size_t nchars USES_REGS) {
  FILE *f;
  stream_flags_t flags;

  st->file = f = NULL;
  flags = Input_Stream_f | InMemory_Stream_f;
  st->vfs = NULL;
  Yap_initStream(st - GLOBAL_Stream, f, Yap_LookupAtom("buffer"), "r", TermNil, LOCAL_encoding, flags, NULL);
  // like any file stream.
  /* currently these streams are not seekable */
  st->status = Input_Stream_f | InMemory_Stream_f;
  st->u.mem_string.pos = 0;
  st->u.mem_string.buf = (char *)buf;
  st->u.mem_string.max_size = nchars;
  st->u.mem_string.error_handler = NULL;
  // st->u.mem_string.src = src; check new assets coode
  Yap_DefaultStreamOps(st);
  return true;
}

int Yap_open_buf_read_stream(const char *buf, size_t nchars, encoding_t *encp,  memBufSource src , Atom name, Term uname) {
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
  st->file = f = NULL;
  flags = Input_Stream_f | InMemory_Stream_f;
  st->vfs = NULL;
  st->name = name;
  Yap_initStream(sno, f, Yap_LookupAtom("Memory Stream"),"wa", TermNil, encoding, flags, NULL);
  // like any file stream.
  /* currently these streams are not seekable */
  st->status = Input_Stream_f | InMemory_Stream_f;
  st->u.mem_string.pos = 0;
  st->u.mem_string.buf = (char *)buf;
  st->u.mem_string.max_size = nchars;
  st->u.mem_string.error_handler = NULL;
  st->u.mem_string.src = src;
  Yap_DefaultStreamOps(st);
  UNLOCK(st->streamlock);
  return sno;
}

static Int
open_mem_read_stream(USES_REGS1) /* $open_mem_read_stream(+List,-Stream) */
{
  Term t, ti;
  int sno;
  int i = push_text_stack();
  const char *buf;

  ti = Deref(ARG1);
  buf = Yap_TextTermToText(ti PASS_REGS);
  if (!buf) {
    return false;
  }
  buf = pop_output_text_stack(i, buf);
  sno = Yap_open_buf_read_stream(buf, strlen(buf) + 1, &LOCAL_encoding,
				 MEM_BUF_MALLOC, AtomNil, TermNil);
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
  st->status = Output_Stream_f | InMemory_Stream_f | FreeOnClose_Stream_f;
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->encoding = enc;
  st->vfs = NULL;
  st->file = NULL;
  Yap_DefaultStreamOps(st);
  st->nbuf = st->u.mem_string.buf = malloc(PLGETC_BUF_SIZE);
  st->u.mem_string.src = MEM_BUF_MALLOC;
  st->u.mem_string.max_size = PLGETC_BUF_SIZE - 1;
  st->u.mem_string.pos = 0;
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

  GLOBAL_Stream[sno].u.mem_string.buf[GLOBAL_Stream[sno].u.mem_string.pos] =
      '\0';
  return GLOBAL_Stream[sno].u.mem_string.buf;
}

static Int peek_mem_write_stream(
    USES_REGS1) { /* '$peek_mem_write_stream'(+GLOBAL_Stream,?S0,?S) */
  Int sno =
      Yap_CheckStream(ARG1, (Output_Stream_f | InMemory_Stream_f), "close/2");
  Int i;
  Term tf = ARG2;
  CELL *HI;
  const char *ptr;

  if (sno < 0)
    return (FALSE);
restart:
  HI = HR;
  ptr = GLOBAL_Stream[sno].u.mem_string.buf;
  i = GLOBAL_Stream[sno].u.mem_string.pos;
  while (i > 0) {
    --i;
    tf = MkPairTerm(MkIntTerm(ptr[i]), tf);
    if (HR + 1024 >= ASP) {
      UNLOCK(GLOBAL_Stream[sno].streamlock);
      HR = HI;
      if (!Yap_gcl((ASP - HI) * sizeof(CELL), 3, ENV, Yap_gcP())) {
        UNLOCK(GLOBAL_Stream[sno].streamlock);
        Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        return (FALSE);
      }
      i = GLOBAL_Stream[sno].u.mem_string.pos;
      tf = ARG2;
      LOCK(GLOBAL_Stream[sno].streamlock);
      goto restart;
    }
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify(ARG3, tf));
}

void Yap_MemOps(StreamDesc *st) {
  st->stream_putc = MemPutc;

  st->stream_getc = MemGetc;
}

bool Yap_CloseMemoryStream(int sno) {
  if ((GLOBAL_Stream[sno].status & Output_Stream_f)) {
    if (GLOBAL_Stream[sno].u.mem_string.src == MEM_BUF_MALLOC) {
      free(GLOBAL_Stream[sno].u.mem_string.buf);
    }
  } else {
    if (GLOBAL_Stream[sno].u.mem_string.src == MEM_BUF_MALLOC) {
      free(GLOBAL_Stream[sno].u.mem_string.buf);
    }
  }
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
